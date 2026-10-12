/// The range of the time limits a host sets: the compilation time limit
/// (JYRO-REQ-1668, 100.9; JYRO-REQ-1907, `JyroCompiler.WithMaxCompilationTime`)
/// and the execution time limit (JYRO-REQ-2115, 100.1; JYRO-REQ-2114,
/// `JyroRunner.WithMaxExecutionTime` and `JyroRunner.WithExecutionOptions`).
/// No limit (`TimeSpan.MaxValue`) or more than zero and at most 24 hours is
/// accepted; every other duration is refused by `ArgumentOutOfRangeException`,
/// whose `ParamName` is the name of the parameter (read by reflection: 160.7.14
/// leaves the name to the implementation), and the earlier limit is kept.
/// Written from the book and the public host API alone (FLOORS 4.1.1).
///
/// The runs that hold a thread in Sleep are in the non-parallel collection
/// "process-wide" (FLOORS 4.1.13). Every run sets the limits it does not test
/// explicitly (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S100_10_TimeLimitRange

open System
open System.Reflection
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- The durations -----------------------------------------------------------------------------------

let private ticks (t: TimeSpan) : obj[] = [| box t.Ticks |]

/// Durations every member accepts besides its own examples.
let private acceptedCommon =
    [ TimeSpan.MaxValue; TimeSpan.FromTicks 1L; TimeSpan.FromMilliseconds 1.; TimeSpan.FromSeconds 1.
      TimeSpan.FromSeconds 5.; TimeSpan.FromHours 24. ]

/// Durations every member refuses: zero, negative (InfiniteTimeSpan is -1 ms),
/// and longer than 24 hours other than MaxValue.
let private refusedCommon =
    [ TimeSpan.Zero; Timeout.InfiniteTimeSpan; TimeSpan.FromSeconds -5.; TimeSpan.FromTicks -1L; TimeSpan.MinValue
      TimeSpan.FromHours 24. + TimeSpan.FromTicks 1L; TimeSpan.FromDays 30.; TimeSpan.FromDays 60.
      TimeSpan.MaxValue - TimeSpan.FromTicks 1L ]

let acceptedDurations : obj[] seq = acceptedCommon |> Seq.map ticks
let refusedDurations : obj[] seq = refusedCommon |> Seq.map ticks

/// The name of the one parameter of `t.name(TimeSpan or JyroExecutionOptions)`.
let private parameterName (t: Type) (name: string) (arg: Type) : string =
    let m = t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Instance, null, [| arg |], null)
    if isNull m then Values.fail (sprintf "%s.%s(%s) is not in this build" t.Name name arg.Name)
    m.GetParameters().[0].Name

/// Assert `f` throws ArgumentOutOfRangeException whose ParamName is `param`.
let private refusedWith (param: string) (what: string) (f: unit -> 'T) : unit =
    let outcome = try Choice1Of2(f ()) with e -> Choice2Of2 e
    match outcome with
    | Choice1Of2 _ -> Values.fail (sprintf "%s returned normally; expected ArgumentOutOfRangeException" what)
    | Choice2Of2(:? ArgumentOutOfRangeException as e) ->
        Assert.True((e.ParamName = param), sprintf "%s: ParamName is %A, the parameter is %A" what e.ParamName param)
    | Choice2Of2 e -> Values.fail (sprintf "%s threw %s, not ArgumentOutOfRangeException: %s" what (e.GetType().FullName) e.Message)

/// Call `f`, ignoring the ArgumentOutOfRangeException a refused duration throws.
let private refusedCall (f: unit -> 'T) : unit =
    try f () |> ignore with :? ArgumentOutOfRangeException -> ()

// -- The compiler ---------------------------------------------------------------------------------------

/// A source whose compile takes far longer than one tick.
let private longCompile = manyStatements 2_000

let private compiler (source: string) : JyroCompiler = JyroCompiler().WithStdlib().WithSource(source)

let private compileParam () = parameterName typeof<JyroCompiler> "WithMaxCompilationTime" typeof<TimeSpan>

/// Assert a compile failed with CompilationTimeLimitExceeded.
let private compileTimedOut (what: string) (r: JyroResult<CompiledProgram>) : unit =
    if r.IsSuccess || not (List.contains MessageCode.CompilationTimeLimitExceeded (Script.errorCodes r)) then
        Values.fail (sprintf "%s: expected CompilationTimeLimitExceeded. IsSuccess=%b. Diagnostics:\n%s" what r.IsSuccess (Script.describe r))

/// Assert a compile succeeded, and failed with no internal error for its limit.
let private compiled (what: string) (r: JyroResult<CompiledProgram>) : unit =
    Assert.DoesNotContain(MessageCode.UnknownCompilerError, Script.codes r)
    Assert.True(r.IsSuccess, sprintf "%s: expected the compile to succeed. Diagnostics:\n%s" what (Script.describe r))

// ================================================================================================
// JYRO-REQ-1907 WithMaxCompilationTime refuses a duration outside the range by throwing
// ================================================================================================

[<Theory>]
[<MemberData(nameof acceptedDurations)>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 WithMaxCompilationTime accepts no limit and every duration above zero up to 24 hours`` (t: int64) =
    compiler "Output.x = 1" |> fun c -> c.WithMaxCompilationTime(TimeSpan.FromTicks t) |> ignore

[<Theory>]
[<MemberData(nameof refusedDurations)>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 WithMaxCompilationTime throws ArgumentOutOfRangeException naming its parameter for a duration outside the range`` (t: int64) =
    let c = compiler "Output.x = 1"
    Assert.Equal("time", compileParam ())
    refusedWith "time" (sprintf "WithMaxCompilationTime(%O)" (TimeSpan.FromTicks t)) (fun () -> c.WithMaxCompilationTime(TimeSpan.FromTicks t))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 the book's examples are refused`` () =
    let param = compileParam ()
    for t in [ TimeSpan.Zero; TimeSpan.FromDays 30.; TimeSpan.FromHours 24. + TimeSpan.FromTicks 1L; TimeSpan.FromMilliseconds -1. ] do
        refusedWith param (sprintf "WithMaxCompilationTime(%O)" t) (fun () -> compiler("Output.x = 1").WithMaxCompilationTime(t))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 after a refused call the compiler keeps its earlier limit of one tick`` () =
    for refused in refusedCommon do
        let c = compiler(longCompile).WithMaxCompilationTime(TimeSpan.FromTicks 1L)
        refusedCall (fun () -> c.WithMaxCompilationTime(refused))
        compileTimedOut (sprintf "one tick, then a refused %O" refused) (c.Compile())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 under 24 hours and under no limit the compile of Output x = 1 succeeds`` () =
    compiled "WithMaxCompilationTime(24 hours)" (compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.FromHours 24.).Compile())
    compiled "WithMaxCompilationTime(MaxValue)" (compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.MaxValue).Compile())

[<Theory>]
[<MemberData(nameof acceptedDurations)>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 no accepted limit makes the compile fail with an internal error`` (t: int64) =
    let r = compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.FromTicks t).Compile()
    // A limit this small may end the compile; it must do so for the limit, not fail for its value.
    Assert.DoesNotContain(MessageCode.UnknownCompilerError, Script.codes r)
    if not r.IsSuccess then compileTimedOut (sprintf "WithMaxCompilationTime(%O)" (TimeSpan.FromTicks t)) r

// ================================================================================================
// JYRO-REQ-1668 A compilation time limit is no limit, or more than zero and at most 24 hours
// ================================================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1668")>]
let ``JYRO-REQ-1668 accepted limits bound the compilation`` () =
    compileTimedOut "one tick" (compiler(longCompile).WithMaxCompilationTime(TimeSpan.FromTicks 1L).Compile())
    compiled "5 seconds" (compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.FromSeconds 5.).Compile())
    compiled "24 hours" (compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.FromHours 24.).Compile())
    compiled "no limit" (compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.MaxValue).Compile())

[<Theory>]
[<MemberData(nameof refusedDurations)>]
[<Trait("requirement", "JYRO-REQ-1668")>]
let ``JYRO-REQ-1668 zero, a negative duration and more than 24 hours are refused through the member`` (t: int64) =
    let c = compiler "Output.x = 1"
    let threw = try c.WithMaxCompilationTime(TimeSpan.FromTicks t) |> ignore; false with :? ArgumentOutOfRangeException -> true
    Assert.True(threw, sprintf "WithMaxCompilationTime(%O) was not refused" (TimeSpan.FromTicks t))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1668")>]
let ``JYRO-REQ-1668 a refused value is not used, the earlier limit is`` () =
    // One tick, then a refused 30 days: the next compile runs under one tick.
    let c = compiler(longCompile).WithMaxCompilationTime(TimeSpan.FromTicks 1L)
    refusedCall (fun () -> c.WithMaxCompilationTime(TimeSpan.FromDays 30.))
    compileTimedOut "one tick, then a refused 30 days" (c.Compile())
    // No limit, then a refused zero: the next compile runs under no limit.
    let d = compiler("Output.x = 1").WithMaxCompilationTime(TimeSpan.MaxValue)
    refusedCall (fun () -> d.WithMaxCompilationTime(TimeSpan.Zero))
    compiled "no limit, then a refused zero" (d.Compile())

// -- The runner ---------------------------------------------------------------------------------------

let private program (source: string) : CompiledProgram =
    JyroCompiler().WithStdlib().WithSource(source).WithMaxCompilationTime(generousCompile).Compile() |> Script.valueOf

/// `JyroExecutionOptions(time, 10000, 1000, 64)` by the constructor of 160.7.13
/// (reached by reflection, as S160_07_RunnerCompilerResult does).
let private optionsOf (time: TimeSpan) : JyroExecutionOptions =
    let c = typeof<JyroExecutionOptions>.GetConstructor([| typeof<TimeSpan>; typeof<int>; typeof<int>; typeof<int> |])
    if isNull c then Values.fail "160.7.13: JyroExecutionOptions has no public constructor (TimeSpan, int, int, int)"
    c.Invoke([| box time; box 10000; box 1000; box 64 |]) :?> JyroExecutionOptions

/// An options value made with a valid time, then given `time` through the
/// settable MaxExecutionTime (160.7.13).
let private optionsSetTo (time: TimeSpan) : JyroExecutionOptions =
    let o = optionsOf (TimeSpan.FromSeconds 30.)
    let p = typeof<JyroExecutionOptions>.GetProperty("MaxExecutionTime")
    p.SetValue(o, box time)
    o

let private timeParam () = parameterName typeof<JyroRunner> "WithMaxExecutionTime" typeof<TimeSpan>
let private optionsParam () = parameterName typeof<JyroRunner> "WithExecutionOptions" typeof<JyroExecutionOptions>

/// Assert the run failed with ExecutionTimeLimitExceeded.
let private runTimedOut (what: string) (r: JyroResult<JyroValue>) : unit =
    if r.IsSuccess || not (List.contains MessageCode.ExecutionTimeLimitExceeded (Script.errorCodes r)) then
        Values.fail (sprintf "%s: expected ExecutionTimeLimitExceeded. IsSuccess=%b. Diagnostics:\n%s" what r.IsSuccess (Script.describe r))

/// Assert the run succeeded with no internal error.
let private ran (what: string) (r: JyroResult<JyroValue>) : unit =
    Assert.DoesNotContain(MessageCode.RuntimeError, Script.codes r)
    Assert.True(r.IsSuccess, sprintf "%s: expected the run to succeed. Diagnostics:\n%s" what (Script.describe r))

// ================================================================================================
// JYRO-REQ-2114 WithMaxExecutionTime and WithExecutionOptions refuse a time limit outside the range
// ================================================================================================

[<Theory>]
[<MemberData(nameof acceptedDurations)>]
[<Trait("requirement", "JYRO-REQ-2114")>]
let ``JYRO-REQ-2114 WithMaxExecutionTime and WithExecutionOptions accept no limit and every duration above zero up to 24 hours`` (t: int64) =
    let time = TimeSpan.FromTicks t
    JyroRunner(program "Output.x = 1").WithMaxExecutionTime(time) |> ignore
    JyroRunner(program "Output.x = 1").WithExecutionOptions(optionsOf time) |> ignore
    JyroRunner(program "Output.x = 1").WithExecutionOptions(optionsSetTo time) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2114")>]
let ``JYRO-REQ-2114 WithExecutionOptions accepts Default and Unlimited`` () =
    JyroRunner(program "Output.x = 1").WithExecutionOptions(JyroExecutionOptions.Default) |> ignore
    JyroRunner(program "Output.x = 1").WithExecutionOptions(JyroExecutionOptions.Unlimited) |> ignore

[<Theory>]
[<MemberData(nameof refusedDurations)>]
[<Trait("requirement", "JYRO-REQ-2114")>]
let ``JYRO-REQ-2114 WithMaxExecutionTime throws ArgumentOutOfRangeException naming its parameter for a duration outside the range`` (t: int64) =
    let time = TimeSpan.FromTicks t
    let r = JyroRunner(program "Output.x = 1")
    refusedWith (timeParam ()) (sprintf "WithMaxExecutionTime(%O)" time) (fun () -> r.WithMaxExecutionTime(time))

[<Theory>]
[<MemberData(nameof refusedDurations)>]
[<Trait("requirement", "JYRO-REQ-2114")>]
let ``JYRO-REQ-2114 WithExecutionOptions throws ArgumentOutOfRangeException naming its parameter for options whose MaxExecutionTime is outside the range`` (t: int64) =
    let time = TimeSpan.FromTicks t
    let r = JyroRunner(program "Output.x = 1")
    let made = optionsOf time
    refusedWith (optionsParam ()) (sprintf "WithExecutionOptions(new JyroExecutionOptions(%O, ...))" time) (fun () -> r.WithExecutionOptions(made))
    let set = optionsSetTo time
    refusedWith (optionsParam ()) (sprintf "WithExecutionOptions(options with MaxExecutionTime set to %O)" time) (fun () -> r.WithExecutionOptions(set))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2114")>]
let ``JYRO-REQ-2114 the book's examples are refused`` () =
    let r = JyroRunner(program "Output.x = 1")
    refusedWith (timeParam ()) "WithMaxExecutionTime(Zero)" (fun () -> r.WithMaxExecutionTime(TimeSpan.Zero))
    refusedWith (timeParam ()) "WithMaxExecutionTime(60 days)" (fun () -> r.WithMaxExecutionTime(TimeSpan.FromDays 60.))
    let minusFive = optionsOf (TimeSpan.FromSeconds -5.)
    refusedWith (optionsParam ()) "WithExecutionOptions(new JyroExecutionOptions(-5 s, 10000, 1000, 64))" (fun () -> r.WithExecutionOptions(minusFive))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2114")>]
let ``JYRO-REQ-2114 a refused WithExecutionOptions applies none of its four limits`` () =
    // A low statement limit, then refused options whose other three limits are high:
    // the statement limit of 5 still ends a run of 20 statements.
    let low = { allLimits JyroExecutionOptions.Default with MaxStatements = 5 }
    let refused =
        { JyroExecutionOptions.Default with
            MaxExecutionTime = TimeSpan.Zero
            MaxStatements = 1_000_000
            MaxLoopIterations = 1_000_000
            MaxCallDepth = 100 }
    let r = JyroRunner(program (statementScript 20)).WithExecutionOptions(low)
    refusedCall (fun () -> r.WithExecutionOptions(refused))
    fails MessageCode.StatementLimitExceeded (r.Execute())

[<Collection("process-wide")>]
type ``S100_10 Runs that sleep under a time limit``() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2114")>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    member _.``JYRO-REQ-2114 JYRO-REQ-2116 after a refused call of either method the runner keeps its limit of 1 second``() =
        for refused in refusedCommon do
            let viaTime = JyroRunner(program "Sleep(3000)\nOutput.done = true").WithMaxExecutionTime(TimeSpan.FromSeconds 1.)
            refusedCall (fun () -> viaTime.WithMaxExecutionTime(refused))
            let viaOptions = JyroRunner(program "Sleep(3000)\nOutput.done = true").WithMaxExecutionTime(TimeSpan.FromSeconds 1.)
            refusedCall (fun () -> viaOptions.WithExecutionOptions(optionsOf refused))
            // Both runs at once: each sleeps 3 s under a limit of 1 s.
            let a = Tasks.Task.Run(fun () -> viaTime.Execute())
            let b = Tasks.Task.Run(fun () -> viaOptions.Execute())
            runTimedOut (sprintf "1 s, then a refused WithMaxExecutionTime(%O)" refused) a.Result
            runTimedOut (sprintf "1 s, then a refused WithExecutionOptions(%O)" refused) b.Result

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2114")>]
    member _.``JYRO-REQ-2114 under 24 hours and under no limit the run of Output x = 1 succeeds``() =
        ran "WithMaxExecutionTime(24 hours)" (JyroRunner(program "Output.x = 1").WithMaxExecutionTime(TimeSpan.FromHours 24.).Execute())
        ran "WithMaxExecutionTime(MaxValue)" (JyroRunner(program "Output.x = 1").WithMaxExecutionTime(TimeSpan.MaxValue).Execute())
        ran "WithExecutionOptions(24 hours)" (JyroRunner(program "Output.x = 1").WithExecutionOptions(optionsOf (TimeSpan.FromHours 24.)).Execute())
        ran "WithExecutionOptions(MaxValue)" (JyroRunner(program "Output.x = 1").WithExecutionOptions(optionsOf TimeSpan.MaxValue).Execute())

    // ============================================================================================
    // JYRO-REQ-2115 An execution time limit is no limit, or more than zero and at most 24 hours
    // ============================================================================================

    [<Theory>]
    [<InlineData("WithMaxExecutionTime")>]
    [<InlineData("WithExecutionOptions")>]
    [<Trait("requirement", "JYRO-REQ-2115")>]
    member _.``JYRO-REQ-2115 accepted limits bound the run``(memberName: string) =
        let set (r: JyroRunner) (t: TimeSpan) =
            if memberName = "WithMaxExecutionTime" then r.WithMaxExecutionTime(t) else r.WithExecutionOptions(optionsOf t)
        runTimedOut "1 second" ((set (JyroRunner(program "Sleep(3000)\nOutput.done = true")) (TimeSpan.FromSeconds 1.)).Execute())
        ran "no limit, Sleep(1500)" ((set (JyroRunner(program "Sleep(1500)\nOutput.done = true")) TimeSpan.MaxValue).Execute())
        ran "24 hours" ((set (JyroRunner(program "Output.x = 1")) (TimeSpan.FromHours 24.)).Execute())
        set (JyroRunner(program "Output.x = 1")) (TimeSpan.FromTicks 1L) |> ignore

    [<Theory>]
    [<InlineData("WithMaxExecutionTime")>]
    [<InlineData("WithExecutionOptions")>]
    [<Trait("requirement", "JYRO-REQ-2115")>]
    member _.``JYRO-REQ-2115 zero, a negative duration and more than 24 hours are refused, and the earlier limit is used``(memberName: string) =
        let set (r: JyroRunner) (t: TimeSpan) =
            if memberName = "WithMaxExecutionTime" then r.WithMaxExecutionTime(t) else r.WithExecutionOptions(optionsOf t)
        for refused in refusedCommon do
            let threw = try set (JyroRunner(program "Output.x = 1")) refused |> ignore; false with :? ArgumentOutOfRangeException -> true
            Assert.True(threw, sprintf "%s(%O) was not refused" memberName refused)
        // 1 s, then a refused 60 days: Sleep(3000) still ends at the limit of 1 s.
        let a = set (JyroRunner(program "Sleep(3000)\nOutput.done = true")) (TimeSpan.FromSeconds 1.)
        refusedCall (fun () -> set a (TimeSpan.FromDays 60.))
        runTimedOut "1 s, then a refused 60 days" (a.Execute())
        // No limit, then a refused zero: Output.x = 1 runs.
        let b = set (JyroRunner(program "Output.x = 1")) TimeSpan.MaxValue
        refusedCall (fun () -> set b TimeSpan.Zero)
        ran "no limit, then a refused zero" (b.Execute())

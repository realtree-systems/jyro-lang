/// 160.7.13 (JyroRunner and JyroExecutionOptions: run a compiled program, give it
/// a cancel, set its time limit), 160.7.14 (JyroCompiler and CompiledProgram:
/// compile a source text) and 160.7.15 (JyroResult<T>, DiagnosticMessage and
/// MessageSeverity). From the book's Interface clauses alone (FLOORS 4.1.1).
///
/// Every compile sets the compilation time limit (100.9.2) and every run sets
/// its limits explicitly, unless the case under test is the default itself
/// (FLOORS 4.1.11). The tests that hold a thread in Sleep run in the
/// non-parallel collection "process-wide" (FLOORS 4.1.13). None of them asserts
/// an upper wall-clock bound: they assert outcomes (success or the code) and,
/// where a case is the default time limit, a lower bound only.
module RealTree.Jyro.Spec.Tests.S160_07_RunnerCompilerResult

open System
open System.Collections.Generic
open System.Reflection
open System.Threading
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- Helpers ------------------------------------------------------------------------------------------------

let private flags = BindingFlags.Public ||| BindingFlags.Instance

/// A new compiler with the compilation time limit set (100.9.2, FLOORS 4.1.11).
let private compiler () : JyroCompiler =
    // The member is WithMaxCompilationTime (JYRO-REQ-1907, JYRO-ADR-201 item 1).
    JyroCompiler().WithMaxCompilationTime(generousCompile)

/// `source` compiled with the standard library; fails the test when it does not compile.
let private compiled (source: string) : CompiledProgram =
    compiler().WithSource(source).WithStdlib().Compile() |> Script.valueOf

/// `JyroExecutionOptions(maxExecutionTime, 64, 64, 64)`, by the constructor of
/// 160.7.13 (reached by reflection: F# source cannot call it). 64 statements,
/// loop iterations and call depth are far above what the one- or two-statement
/// programs here need, in whatever order the three int parameters come.
let private optionsOf (time: TimeSpan) : JyroExecutionOptions =
    let c = typeof<JyroExecutionOptions>.GetConstructor([| typeof<TimeSpan>; typeof<int>; typeof<int>; typeof<int> |])
    if isNull c then Values.fail "160.7.13: JyroExecutionOptions has no public constructor (TimeSpan, int, int, int)"
    c.Invoke([| box time; box 64; box 64; box 64 |]) :?> JyroExecutionOptions

/// A runner of `source` with every limit set: `time`, and 64 of the others.
let private runnerWith (time: TimeSpan) (source: string) : JyroRunner =
    JyroRunner(compiled source).WithExecutionOptions(optionsOf time)

/// Run `work` on another thread (for two sleeping runs at once).
let private background (work: unit -> 'T) : Task<'T> = Task.Run(fun () -> work ())

/// Cancel `cts` `ms` milliseconds from now, on a timer thread.
let private cancelAfter (ms: int) (cts: CancellationTokenSource) : Timer =
    new Timer((fun _ -> cts.Cancel()), null, ms, Timeout.Infinite)

/// Assert the result failed with a diagnostic of `code` (the code first, FLOORS 4.1.11).
let private failedWith (code: MessageCode) (r: JyroResult<'T>) : unit =
    if not (Script.codes r |> List.contains code) then
        Values.fail (sprintf "Expected a diagnostic %A. IsSuccess=%b. Diagnostics:\n%s" code r.IsSuccess (Script.describe r))
    Assert.False(r.IsSuccess, "Expected IsSuccess false. " + Script.describe r)

/// Assert the run succeeded with no CanceledByHost, and Output.done is true.
let private ranToTheEnd (r: JyroResult<JyroValue>) : unit =
    Assert.DoesNotContain(MessageCode.CanceledByHost, Script.codes r)
    Assert.True(r.IsSuccess, "Expected success. " + Script.describe r)
    Assert.True(Values.asBool (resultProp "done" r), "Output.done is not true")

// ================================================================================================
// 160.7.13 JyroRunner and JyroExecutionOptions
// ================================================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2116")>]
let ``JYRO-REQ-2116 JyroRunner_IsAPublicClassWithTheDeclaredMembers`` () =
    let t = typeof<JyroRunner>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed && not t.IsAbstract, "JyroRunner is a public sealed class (JYRO-REQ-1928)")
    Assert.NotNull(t.GetConstructor([| typeof<CompiledProgram> |]))
    let returns (name: string) (args: Type[]) (ret: Type) =
        let m = t.GetMethod(name, flags, null, args, null)
        Assert.True(not (isNull m), sprintf "JyroRunner.%s is missing" name)
        Assert.Equal(ret, m.ReturnType)
    returns "Execute" Type.EmptyTypes typeof<JyroExecutionResult>
    returns "WithCancellationToken" [| typeof<CancellationToken> |] typeof<JyroRunner>
    returns "WithMaxExecutionTime" [| typeof<TimeSpan> |] typeof<JyroRunner>
    returns "WithExecutionOptions" [| typeof<JyroExecutionOptions> |] typeof<JyroRunner>

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2116")>]
let ``JYRO-REQ-2116 JyroExecutionOptions_IsAPublicSealedClassWithTheDeclaredMembers`` () =
    let t = typeof<JyroExecutionOptions>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed, "JyroExecutionOptions is a public sealed class")
    Assert.NotNull(t.GetConstructor([| typeof<TimeSpan>; typeof<int>; typeof<int>; typeof<int> |]))
    let d = t.GetProperty("Default", BindingFlags.Public ||| BindingFlags.Static)
    Assert.True(not (isNull d), "JyroExecutionOptions.Default is missing")
    Assert.Equal(t, d.PropertyType)
    Assert.NotNull(d.GetGetMethod())
    Assert.Null(d.GetSetMethod())
    let m = t.GetProperty("MaxExecutionTime", flags)
    Assert.True(not (isNull m), "JyroExecutionOptions.MaxExecutionTime is missing")
    Assert.Equal(typeof<TimeSpan>, m.PropertyType)
    Assert.NotNull(m.GetGetMethod())
    Assert.NotNull(m.GetSetMethod())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2116")>]
let ``JYRO-REQ-2116 Execute_RunsTheProgramGivenToTheConstructorOnce_AndReturnsItsResult`` () =
    let mark = Host.recorder "Mark"
    let program =
        compiler().WithSource("Mark()\nOutput.x = 1").WithStdlib().AddFunction(mark.AsFunction).Compile() |> Script.valueOf
    let r = JyroRunner(program).WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 30.)).Execute()
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1.0, Values.asFloat (resultProp "x" r))
    Assert.Equal(1, mark.CallCount)
    // Another program, another result: the one given to the constructor runs.
    let r2 = (runnerWith (TimeSpan.FromSeconds 30.) "Output.y = \"b\"").Execute()
    Assert.Equal("b", Values.asString (resultProp "y" r2))
    Assert.False(hasProp "x" (Script.valueOf r2))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2116")>]
let ``JYRO-REQ-2116 EachWithMethod_ReturnsTheRunnerItWasCalledOn`` () =
    let runner = JyroRunner(compiled "Output.x = 1")
    use cts = new CancellationTokenSource()
    Assert.Same(runner, runner.WithCancellationToken(cts.Token))
    Assert.Same(runner, runner.WithMaxExecutionTime(TimeSpan.FromSeconds 30.))
    Assert.Same(runner, runner.WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 30.)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2116")>]
let ``JYRO-REQ-2116 JyroExecutionOptions_TheFirstConstructorArgumentIsMaxExecutionTime`` () =
    for ms in [ 1.; 1000.; 12345. ] do
        Assert.Equal(TimeSpan.FromMilliseconds ms, (optionsOf (TimeSpan.FromMilliseconds ms)).MaxExecutionTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2116")>]
let ``JYRO-REQ-2116 JyroExecutionOptions_Default_HasTheMaxExecutionTimeOf5Seconds`` () =
    Assert.Equal(TimeSpan.FromSeconds 5., JyroExecutionOptions.Default.MaxExecutionTime)

[<Collection("process-wide")>]
type ``S160_07_13 Runs that sleep``() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<Trait("requirement", "JYRO-REQ-2041")>]
    member _.``JYRO-REQ-2116 WithCancellationToken_IsTheSignalOfEveryLaterExecute``() =
        use cts = new CancellationTokenSource()
        let runner = (runnerWith (TimeSpan.FromSeconds 30.) "Sleep(10000)\nOutput.done = true").WithCancellationToken(cts.Token)
        use _t = cancelAfter 100 cts
        failedWith MessageCode.CanceledByHost (runner.Execute())
        // A later Execute() of the same runner: the token, now canceled, is still its signal.
        failedWith MessageCode.CanceledByHost (runner.Execute())

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    member _.``JYRO-REQ-2116 WithCancellationToken_OfATokenNeverCanceled_DoesNotEndTheRun``() =
        use cts = new CancellationTokenSource()
        let runner = (runnerWith (TimeSpan.FromSeconds 30.) "Sleep(200)\nOutput.done = true").WithCancellationToken(cts.Token)
        ranToTheEnd (runner.Execute())

    /// The clause's example: WithCancellationToken(t1) then WithCancellationToken(t2),
    /// on the program `Sleep(1500)` then `Output.done = true`, time limit 10 s.
    [<Theory>]
    [<InlineData("earlier-canceled-during")>]
    [<InlineData("last-canceled-during")>]
    [<InlineData("earlier-canceled-before")>]
    [<InlineData("earlier-canceled-two-executes")>]
    [<InlineData("reversed-order-earlier-canceled")>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    member _.``JYRO-REQ-2116 WithCancellationToken_CalledTwice_TheOneCalledLastIsTheSignal``(case: string) =
        use first = new CancellationTokenSource()
        use last = new CancellationTokenSource()
        let runner =
            (runnerWith (TimeSpan.FromSeconds 10.) "Sleep(1500)\nOutput.done = true")
                .WithCancellationToken(first.Token)
                .WithCancellationToken(last.Token)
        match case with
        | "earlier-canceled-during" ->
            use _t = cancelAfter 200 first
            ranToTheEnd (runner.Execute())
        | "last-canceled-during" ->
            use _t = cancelAfter 200 last
            failedWith MessageCode.CanceledByHost (runner.Execute())
        | "earlier-canceled-before" ->
            first.Cancel()
            ranToTheEnd (runner.Execute())
        | "earlier-canceled-two-executes" ->
            first.Cancel()
            ranToTheEnd (runner.Execute())
            ranToTheEnd (runner.Execute())
        | _ ->
            // The rule is the order of the calls, not which token: here `last` is given first.
            use u2 = new CancellationTokenSource()
            use u1 = new CancellationTokenSource()
            let reversed =
                (runnerWith (TimeSpan.FromSeconds 10.) "Sleep(1500)\nOutput.done = true")
                    .WithCancellationToken(u2.Token)
                    .WithCancellationToken(u1.Token)
            use _t = cancelAfter 200 u2
            ranToTheEnd (reversed.Execute())

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<Trait("requirement", "JYRO-REQ-1424")>]
    member _.``JYRO-REQ-2116 WithMaxExecutionTime_IsTheLimitOfEveryLaterExecute``() =
        // A program of one statement: no statement, loop or depth limit can end it.
        let runner = JyroRunner(compiled "Sleep(3000)\nOutput.done = true").WithMaxExecutionTime(TimeSpan.FromSeconds 1.)
        failedWith MessageCode.ExecutionTimeLimitExceeded (runner.Execute())
        failedWith MessageCode.ExecutionTimeLimitExceeded (runner.Execute())
        let roomy = JyroRunner(compiled "Sleep(100)\nOutput.done = true").WithMaxExecutionTime(TimeSpan.FromSeconds 10.)
        ranToTheEnd (roomy.Execute())
        ranToTheEnd (roomy.Execute())

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<Trait("requirement", "JYRO-REQ-1424")>]
    member _.``JYRO-REQ-2116 WithExecutionOptions_TheMaxExecutionTimeOfTheOptionsIsTheLimit``() =
        let runner = JyroRunner(compiled "Sleep(3000)\nOutput.done = true").WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 1.))
        failedWith MessageCode.ExecutionTimeLimitExceeded (runner.Execute())
        failedWith MessageCode.ExecutionTimeLimitExceeded (runner.Execute())
        let roomy = JyroRunner(compiled "Sleep(100)\nOutput.done = true").WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 10.))
        ranToTheEnd (roomy.Execute())

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<Trait("requirement", "JYRO-REQ-1424")>]
    member _.``JYRO-REQ-2116 WithMaxExecutionTimeAndWithExecutionOptions_TheOneCalledLastSetsTheLimit``() =
        // The clause's example, with 50 ms in place of 1 s: 50 ms then Default (5 s) runs Sleep(100) to its end. Every limit of a run ending ok is ten times the longest measured Execute() of that step, 168.27 ms, under a whole run of this project (see Host_CanLowerAndRaiseTheTimeLimit): Default is 5,000 ms, 29 times it.
        let example =
            JyroRunner(compiled "Sleep(100)\nOutput.done = true")
                .WithMaxExecutionTime(TimeSpan.FromMilliseconds 50.)
                .WithExecutionOptions(JyroExecutionOptions.Default)
        ranToTheEnd (example.Execute())
        // Reversed: Default then 50 ms ends it.
        let reversed =
            JyroRunner(compiled "Sleep(100)\nOutput.done = true")
                .WithExecutionOptions(JyroExecutionOptions.Default)
                .WithMaxExecutionTime(TimeSpan.FromMilliseconds 50.)
        failedWith MessageCode.ExecutionTimeLimitExceeded (reversed.Execute())
        // Options of 10 s then 50 ms, and 50 ms then options of 10 s.
        let a = JyroRunner(compiled "Sleep(100)\nOutput.done = true").WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 10.)).WithMaxExecutionTime(TimeSpan.FromMilliseconds 50.)
        failedWith MessageCode.ExecutionTimeLimitExceeded (a.Execute())
        let b = JyroRunner(compiled "Sleep(100)\nOutput.done = true").WithMaxExecutionTime(TimeSpan.FromMilliseconds 50.).WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 10.))
        ranToTheEnd (b.Execute())

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<Trait("requirement", "JYRO-REQ-1424")>]
    member _.``JYRO-REQ-2116 ARunnerWithNeitherMethod_UsesTheLimitOf5Seconds``() =
        // The case is the default itself, so no limit is set. Both programs are
        // one statement, so only the time limit can end them. The two run at once.
        let within = background (fun () -> JyroRunner(compiled "Sleep(4000)\nOutput.done = true").Execute())
        let past = background (fun () -> timed (fun () -> JyroRunner(compiled "Sleep(6000)\nOutput.done = true").Execute()))
        ranToTheEnd (within.Result)
        let r, elapsed = past.Result
        failedWith MessageCode.ExecutionTimeLimitExceeded r
        // Lower bound only: the limit did not end the run before 5 s.
        Assert.True(elapsed >= TimeSpan.FromMilliseconds 4900., sprintf "ended after %O, before 5 s" elapsed)

    /// The one-call members of 160.7.1 run at the limits of 100.1.2: 5 s, 10,000
    /// statements, 1,000 loop iterations and call depth 64. One case per member (FLOORS 4.1.8).
    [<Theory>]
    [<InlineData("ExecuteScript")>]
    [<InlineData("ExecuteScriptWithJson")>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<Trait("requirement", "JYRO-REQ-1405")>]
    member _.``JYRO-REQ-2116 OneCallMember_RunsAtTheLimitsOf100_1_2``(path: string) =
        let run (source: string) : JyroResult<JyroValue> =
            match path with
            | "ExecuteScript" -> JyroScript.executeScript source (box (Dictionary<string, obj>()))
            | _ -> JyroScript.executeScriptWithJson source "{}"
        let loop n = sprintf "var i = 0\nwhile i < %d do i = i + 1 end\nOutput.done = true" n
        let within = background (fun () -> run "Sleep(4000)\nOutput.done = true")
        let past = background (fun () -> timed (fun () -> run "Sleep(6000)\nOutput.done = true"))
        Assert.True((run (statementScript 10_000)).IsSuccess, "10,000 statements did not run")
        failedWith MessageCode.StatementLimitExceeded (run (statementScript 10_001))
        ranToTheEnd (run (loop 1_000))
        failedWith MessageCode.LoopIterationLimitExceeded (run (loop 1_001))
        Assert.True((run (recursion 64)).IsSuccess, "64 nested calls did not run")
        failedWith MessageCode.CallDepthLimitExceeded (run (recursion 65))
        ranToTheEnd within.Result
        let r, elapsed = past.Result
        failedWith MessageCode.ExecutionTimeLimitExceeded r
        Assert.True(elapsed >= TimeSpan.FromMilliseconds 4900., sprintf "%s ended after %O, before 5 s" path elapsed)

// ================================================================================================
// 160.7.14 JyroCompiler and CompiledProgram
// ================================================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``JyroCompiler_IsAPublicClassWithTheDeclaredMembers_AndCompiledProgramAPublicSealedClass`` () =
    let t = typeof<JyroCompiler>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed && not t.IsAbstract, "JyroCompiler is a public sealed class (JYRO-REQ-1928)")
    Assert.NotNull(t.GetConstructor(Type.EmptyTypes))
    let returns (name: string) (args: Type[]) (ret: Type) =
        let m = t.GetMethod(name, flags, null, args, null)
        Assert.True(not (isNull m), sprintf "JyroCompiler.%s is missing" name)
        Assert.Equal(ret, m.ReturnType)
    returns "WithSource" [| typeof<string> |] typeof<JyroCompiler>
    returns "WithStdlib" Type.EmptyTypes typeof<JyroCompiler>
    returns "Compile" Type.EmptyTypes typeof<JyroResult<CompiledProgram>>
    let p = typeof<CompiledProgram>
    Assert.Equal("RealTree.Jyro", p.Namespace)
    Assert.True(p.IsClass && p.IsPublic && p.IsSealed, "CompiledProgram is a public sealed class")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``WithSourceAndWithStdlib_ReturnTheCompilerTheyWereCalledOn`` () =
    let c = compiler ()
    Assert.Same(c, c.WithSource("Output.x = 1"))
    Assert.Same(c, c.WithStdlib())

/// Compile `c`, assert success, run the program with every limit set, and give Output.
let private compileAndRun (c: JyroCompiler) : JyroValue =
    let r = c.Compile()
    Assert.True(r.IsSuccess, "Expected the compile to succeed. " + Script.describe r)
    JyroRunner(Script.valueOf r).WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 30.)).Execute() |> Script.output

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``WithSource_GivesTheSourceTextOfTheLaterCompile`` () =
    Assert.Equal(1.0, compileAndRun (compiler().WithSource("Output.x = 1").WithStdlib()) |> Values.prop "x" |> Values.asFloat)
    Assert.Equal(2.0, compileAndRun (compiler().WithSource("Output.x = 2").WithStdlib()) |> Values.prop "x" |> Values.asFloat)
    // WithStdlib before WithSource: the same source text.
    Assert.Equal(3.0, compileAndRun (compiler().WithStdlib().WithSource("Output.x = 3")) |> Values.prop "x" |> Values.asFloat)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``WithSource_CalledTwice_TheOneCalledLastGivesTheSourceText`` () =
    // The clause's example: `var s: string` then `Output.x = 2` compiles, as `Output.x = 2`.
    let example = compiler().WithSource("var s: string").WithSource("Output.x = 2").WithStdlib()
    Assert.Equal(2.0, compileAndRun example |> Values.prop "x" |> Values.asFloat)
    let twoGood = compiler().WithSource("Output.x = 1").WithSource("Output.x = 2").WithStdlib()
    let out = compileAndRun twoGood
    Assert.Equal(2.0, out |> Values.prop "x" |> Values.asFloat)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``WithSource_CalledTwice_ALastTextThatDoesNotCompile_FailsTheCompile`` () =
    // 160.7.15 gives the outcome of `var s: string`: TypedVariableNeedsInitializer, Error.
    let r = compiler().WithSource("Output.x = 2").WithSource("var s: string").WithStdlib().Compile()
    failedWith MessageCode.TypedVariableNeedsInitializer r

/// The inventory of 90.2, one row per function (Inventory.fs, written out from the book).
let inventoryNames : obj[] seq = Inventory.all |> Seq.map (fun l -> [| box l.Name |])

[<Theory>]
[<MemberData(nameof inventoryNames)>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``WithStdlib_MakesEachFunctionOfTheInventoryCallable`` (name: string) =
    let listed = Inventory.all |> List.find (fun l -> l.Name = name)
    let r = compiler().WithSource("Output.r = " + listed.FullCall).WithStdlib().Compile()
    Assert.DoesNotContain(MessageCode.UndefinedFunction, Script.codes r)
    Assert.True(r.IsSuccess, sprintf "%s did not compile with WithStdlib(). %s" listed.FullCall (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``Compile_WithNoErrorDiagnostic_ReturnsSuccessWithTheCompiledProgram`` () =
    // The clause's example.
    let r = compiler().WithSource("Output.x = ToUpper(\"a\")").WithStdlib().Compile()
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.DoesNotContain(MessageSeverity.Error, r.Messages |> Seq.map (fun m -> m.Severity))
    let found, program = r.TryGetValue()
    Assert.True(found)
    Assert.NotNull(program)
    let out = JyroRunner(program).WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 30.)).Execute() |> Script.output
    Assert.Equal("A", out |> Values.prop "x" |> Values.asString)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``Compile_WithAnErrorDiagnostic_ReturnsFailureWithTheDiagnosticsInMessages`` () =
    // Two undeclared names on two lines: two Error diagnostics of the compilation.
    let r = compiler().WithSource("Output.a = u1\nOutput.b = u2").WithStdlib().Compile()
    Assert.False(r.IsSuccess, Script.describe r)
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    Assert.True(errors.Length >= 2, sprintf "Expected both errors in Messages. %s" (Script.describe r))

let noSourcePaths : obj[] seq = [ [| box "plain" |]; [| box "stdlib" |] ]

let private noSource (path: string) : JyroResult<CompiledProgram> =
    if path = "plain" then compiler().Compile() else compiler().WithStdlib().Compile()

[<Theory>]
[<MemberData(nameof noSourcePaths)>]
[<Trait("requirement", "JYRO-REQ-2108")>]
let ``Compile_WithNoSource_IsRefusedWithExactlyOneNoSourceProvidedError`` (path: string) =
    let r = noSource path
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.False(r.HasValue, "HasValue")
    Assert.Equal(1, r.Messages.Count)
    let m = r.Messages.[0]
    Assert.Equal(MessageCode.NoSourceProvided, m.Code)
    Assert.Equal(MessageSeverity.Error, m.Severity)

[<Theory>]
[<MemberData(nameof noSourcePaths)>]
[<Trait("requirement", "JYRO-REQ-2108")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 Compile_WithNoSource_TheRefusalHasTheStageValidate`` (path: string) =
    let r = noSource path
    let m = r.Messages |> Seq.tryFind (fun m -> m.Code = MessageCode.NoSourceProvided)
    match m with
    | None -> Values.fail (sprintf "No NoSourceProvided. %s" (Script.describe r))
    | Some m -> Assert.Equal("Validate", Pending.stageOf m)

[<Theory>]
[<MemberData(nameof noSourcePaths)>]
[<Trait("requirement", "JYRO-REQ-2108")>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-2108 JYRO-REQ-1942 Compile_OfTheEmptyString_IsNotRefused_AndCompilesTheEmptyText`` (path: string) =
    let c = if path = "plain" then compiler().WithSource("") else compiler().WithSource("").WithStdlib()
    let r = c.Compile()
    Assert.DoesNotContain(MessageCode.NoSourceProvided, Script.codes r)
    // 20.1.1 admits a program of no statements, and no clause gives it an Error.
    Assert.True(r.IsSuccess, Script.describe r)
    let out = JyroRunner(Script.valueOf r).WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 30.)).Execute()
    Assert.True(out.IsSuccess, Script.describe out)

// ================================================================================================
// 160.7.15 JyroResult<T>, DiagnosticMessage and MessageSeverity
// ================================================================================================

let private getOnly (t: Type) (name: string) (propertyType: Type) : unit =
    let p = t.GetProperty(name, flags)
    Assert.True(not (isNull p), sprintf "%s.%s is missing" t.Name name)
    Assert.Equal(propertyType, p.PropertyType)
    Assert.NotNull(p.GetGetMethod())
    Assert.Null(p.GetSetMethod())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``JyroResult_IsAPublicClassWithTheDeclaredMembers`` () =
    // `public class JyroResult<T>` (JYRO-ADR-198 items 7 and 8): a class that a result type derives from,
    // so it is not sealed.
    let g = typedefof<JyroResult<_>>
    Assert.Equal("RealTree.Jyro", g.Namespace)
    Assert.True(g.IsClass && g.IsPublic && not g.IsSealed && g.IsGenericTypeDefinition && g.GetGenericArguments().Length = 1)
    for t in [ typeof<JyroResult<JyroValue>>; typeof<JyroResult<CompiledProgram>> ] do
        getOnly t "IsSuccess" typeof<bool>
        getOnly t "HasValue" typeof<bool>
        getOnly t "Messages" typeof<IReadOnlyList<DiagnosticMessage>>
        let arg = t.GetGenericArguments().[0]
        let m = t.GetMethod("TryGetValue", flags, null, [| arg.MakeByRefType() |], null)
        Assert.True(not (isNull m), sprintf "%s.TryGetValue(out T) is missing" t.Name)
        Assert.Equal(typeof<bool>, m.ReturnType)
        Assert.True(m.GetParameters().[0].IsOut, "the parameter of TryGetValue is out")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``DiagnosticMessage_IsAPublicSealedClassWithCodeSeverityAndMessage`` () =
    let t = typeof<DiagnosticMessage>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed)
    getOnly t "Code" typeof<MessageCode>
    getOnly t "Severity" typeof<MessageSeverity>
    getOnly t "Message" typeof<string>

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``MessageSeverity_HasExactlyInfo0Warning1Error2`` () =
    let t = typeof<MessageSeverity>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsEnum && t.IsPublic)
    let members =
        t.GetFields(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.map (fun f -> f.Name, Convert.ToInt64(f.GetRawConstantValue()))
        |> Array.sortBy snd
        |> List.ofArray
    Assert.Equal<(string * int64) list>([ "Info", 0L; "Warning", 1L; "Error", 2L ], members)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``IsSuccess_OfCompile_IsTheSuccessIndicator`` () =
    Assert.True((compiler().WithSource("Output.x = 1").WithStdlib().Compile()).IsSuccess)
    // The clause's example: `var s: string` fails.
    let r = compiler().WithSource("var s: string").WithStdlib().Compile()
    failedWith MessageCode.TypedVariableNeedsInitializer r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
[<Trait("requirement", "JYRO-REQ-1751")>]
let ``IsSuccess_OfExecute_IsTheSuccessIndicator`` () =
    Assert.True(((runnerWith (TimeSpan.FromSeconds 30.) "Output.x = 1").Execute()).IsSuccess)
    // 40.3.1 gives the failure: DivisionByZero.
    failedWith MessageCode.DivisionByZero ((runnerWith (TimeSpan.FromSeconds 30.) "Output.x = 1 / 0").Execute())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
[<Trait("requirement", "JYRO-REQ-2106")>]
let ``IsSuccess_OfJyroArtifactInfoRead_IsTheSuccessIndicator`` () =
    let bytes = (compiler().WithSource("Output.x = 1").WithStdlib().CompileToArtifact()) |> Script.valueOf
    Assert.True((JyroArtifactInfo.Read bytes).IsSuccess)
    // 160.7.9's example: "JYR" is refused with ArtifactInvalidMagic.
    failedWith MessageCode.ArtifactInvalidMagic (JyroArtifactInfo.Read "JYR"B)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``HasValueAndTryGetValue_OfAResultWithAValue_GiveThatValue`` () =
    // A compile: the program it gives is the value, and it runs.
    let c = compiler().WithSource("Output.x = 7").WithStdlib().Compile()
    Assert.True(c.HasValue)
    let found, program = c.TryGetValue()
    Assert.True(found)
    Assert.NotNull(program)
    // A run: its Output is the value.
    let r = JyroRunner(program).WithExecutionOptions(optionsOf (TimeSpan.FromSeconds 30.)).Execute()
    Assert.True(r.HasValue)
    let found2, v = r.TryGetValue()
    Assert.True(found2)
    Assert.Equal(7.0, v |> Values.prop "x" |> Values.asFloat)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``HasValueAndTryGetValue_OfAResultWithNoValue_AreFalse`` () =
    // Failed compiles. Whether a failed compile has a value is not this clause's;
    // this clause binds TryGetValue to HasValue either way.
    for r in [ compiler().WithSource("var s: string").WithStdlib().Compile(); compiler().Compile() ] do
        let found, _ = r.TryGetValue()
        Assert.Equal(r.HasValue, found)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
[<Trait("requirement", "JYRO-REQ-1486")>]
[<Trait("requirement", "JYRO-REQ-2107")>]
let ``Messages_HoldTheDiagnosticsInTheOrderOf130_5_1`` () =
    let r = compiler().WithSource("Output.a = 1\nOutput.b = u2\nOutput.c = 3\nOutput.d = u4\nOutput.e = u5 + u6").WithStdlib().Compile()
    let places = [ for m in r.Messages do if m.Location.HasValue then yield m.Location.Value.Line, m.Location.Value.Column ]
    Assert.True(places.Length >= 4, Script.describe r)
    Assert.Equal<(int * int) list>(List.sort places, places)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
let ``DiagnosticMembers_TheClausesExample_CodeSeverityAndMessage`` () =
    let r = compiler().WithSource("var s: string").WithStdlib().Compile()
    Assert.False(r.IsSuccess)
    let d = r.Messages |> Seq.tryFind (fun m -> m.Code = MessageCode.TypedVariableNeedsInitializer)
    match d with
    | None -> Values.fail (sprintf "No TypedVariableNeedsInitializer. %s" (Script.describe r))
    | Some d ->
        Assert.Equal(MessageSeverity.Error, d.Severity)
        Assert.False(String.IsNullOrEmpty d.Message, "Message is null or empty")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2109")>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``Message_IsNotMadeFromAHostTemplate`` () =
    // Message is the diagnostic's message with no host template, which a template
    // of the host never changes: the same before and after FormatLocalized is
    // called with a template for every code, and the message FormatLocalized
    // gives under a provider with no template (160.7.17).
    let everyCode = TemplateProvider("X{0}Y", None) :> IMessageTemplateProvider
    for source in [ "fail \"boom\""; "var s: string"; "Output.x = Sum([1, null, \"a\"])"; "fail" ] do
        let d = Script.runWith pinned source |> Script.onlyErrorMessage
        let before = d.Message
        Assert.False(String.IsNullOrEmpty before, sprintf "%s: Message is null or empty" source)
        formatLocalized everyCode d |> ignore
        Assert.Equal(before, d.Message)
        let again = Script.runWith pinned source |> Script.onlyErrorMessage
        Assert.Equal(before, again.Message)
        Assert.Equal(before, localizedMessage (NoTemplateProvider()) d)

// ================================================================================================
// JYRO-REQ-2140 (beside 160.7.14): a throw from a host function during compilation is reported
// through the result, not thrown. It replaces JYRO-REQ-1679 and takes Effect out of its members
// (JYRO-REQ-2136 gives a throw from Effect the code MaxEffectNotApplicable)
// ================================================================================================
//
// Each case runs once through Compile() and once through CompileToArtifact(), each call wrapped so
// that any exception escaping it fails the test. A faulty member throws only once its switch is
// armed, and the switch is armed after the compiler is configured, immediately before the
// call: that holds the case to a throw 'while Compile() runs'. The clause binds what happens
// when a member throws while the compile member runs; it does not require that Name,
// Signature or Effect be read then (they may have been read when the function was added). So
// for those three the result is asserted when the armed member did throw during the call, and
// only 'returns' otherwise. CheckCall is called only during Link (JYRO-REQ-2119), so its cases
// are asserted always. Not asserted (outside the clause): CompileOrThrow(), and the code of
// the diagnostic for a JyroRuntimeException thrown by Name, Signature or Effect.

/// An exception type of the host's own.
type HostFault(message: string) =
    inherit Exception(message)

/// A host function `Faulty` whose member `which` ("Name", "Signature" or "Effect") throws
/// `fault ()` once armed, counting the throws.
type private ArmedFault(which: string, fault: unit -> exn) =
    let mutable armed = false
    let mutable thrown = 0
    member _.Arm() = armed <- true
    member _.Thrown = thrown
    member private _.Trip(m: string) =
        if armed && m = which then
            thrown <- thrown + 1
            raise (fault ())
    interface IJyroFunction with
        member this.Name = this.Trip "Name"; "Faulty"
        member this.Signature = this.Trip "Signature"; Host.signature "Faulty" [ Parameter.Optional("a", ParameterType.Any) ] ParameterType.Any
        member this.Effect = this.Trip "Effect"; FunctionEffect.Pure
        member _.Execute(_, _) = JyroNull.Instance :> JyroValue

/// The two compile members, each as a call that returns the result as a result of obj.
let private compileMembers : (string * (JyroCompiler -> JyroResult<obj>)) list =
    [ "Compile", (fun c -> Tests.CallCheck.boxed (c.Compile()))
      "CompileToArtifact", (fun c -> Tests.CallCheck.boxed (Loader.compileToArtifactOf c)) ]

/// Run `call`, failing the test if anything escapes it (the clause: it MUST return, and
/// MUST NOT throw that exception or any other).
let private mustReturn (what: string) (call: unit -> JyroResult<obj>) : JyroResult<obj> =
    try call ()
    with e -> Values.fail (sprintf "%s threw %s: %s" what (e.GetType().FullName) e.Message)

/// The result failed, with at least one Error.
let private failedWithAnError (what: string) (r: JyroResult<obj>) =
    Assert.False(r.IsSuccess, sprintf "%s: IsSuccess. %s" what (Script.describe r))
    Assert.True(r.Messages |> Seq.exists (fun m -> m.Severity = MessageSeverity.Error), sprintf "%s: no Error. %s" what (Script.describe r))

/// The result failed, with an UnknownCompilerError (6000) of severity Error.
let private unknownCompilerErrorIn (what: string) (r: JyroResult<obj>) =
    failedWithAnError what r
    let ds = Tests.CallCheck.ofCode Tests.CallCheck.unknownCompilerError r
    Assert.True(ds |> List.exists (fun d -> d.Severity = MessageSeverity.Error),
                sprintf "%s: no UnknownCompilerError (6000) of severity Error. Diagnostics:\n%s" what (Script.describe r))

let private faultSource = "Output.x = Faulty(1)"

// The members of JYRO-REQ-2140: Effect is not among them. A throw from Effect is the condition of
// JYRO-REQ-2136 (MaxEffectNotApplicable, S120_Sandboxing).
let faultingMembers : obj[] seq =
    [ for m in [ "Name"; "Signature" ] do
        for path in [ "Compile"; "CompileToArtifact" ] -> [| box m; box path |] ]

[<Theory>]
[<MemberData(nameof faultingMembers)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a Name or Signature that throws while the compile member runs gives UnknownCompilerError, not a throw`` (memberName: string, path: string) =
    let f = ArmedFault(memberName, fun () -> InvalidOperationException("the host cannot say") :> exn)
    let c = (Script.compilerFor { pinned with Functions = [ f :> IJyroFunction ] }).WithSource(faultSource)
    let run = compileMembers |> List.find (fst >> (=) path) |> snd
    f.Arm()
    let r = mustReturn path (fun () -> run c)
    if f.Thrown > 0 then unknownCompilerErrorIn (sprintf "%s throwing during %s" memberName path) r

[<Theory>]
[<MemberData(nameof faultingMembers)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a member that throws an exception type of the host's own is reported, not thrown`` (memberName: string, path: string) =
    let f = ArmedFault(memberName, fun () -> HostFault("the host's own fault") :> exn)
    let c = (Script.compilerFor { pinned with Functions = [ f :> IJyroFunction ] }).WithSource(faultSource)
    let run = compileMembers |> List.find (fst >> (=) path) |> snd
    f.Arm()
    let r = mustReturn path (fun () -> run c)
    if f.Thrown > 0 then unknownCompilerErrorIn (sprintf "%s throwing HostFault during %s" memberName path) r

[<Theory>]
[<MemberData(nameof faultingMembers)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a member that throws a JyroRuntimeException while the compile member runs makes it return a failure`` (memberName: string, path: string) =
    // The code of the diagnostic is left to the implementation: only 'returns, IsSuccess
    // false, at least one Error' is asserted.
    let f = ArmedFault(memberName, fun () ->
        try raise (JyroError.Runtime(MessageCode.InvalidJson))
        with e -> e)
    let c = (Script.compilerFor { pinned with Functions = [ f :> IJyroFunction ] }).WithSource(faultSource)
    let run = compileMembers |> List.find (fst >> (=) path) |> snd
    f.Arm()
    let r = mustReturn path (fun () -> run c)
    if f.Thrown > 0 then failedWithAnError (sprintf "%s throwing a JyroRuntimeException during %s" memberName path) r

/// A checking function `Faulty` whose CheckCall runs `act` (JYRO-REQ-2119).
let private checkThat (act: Tests.CallCheck.CheckingFunction -> unit) : Tests.CallCheck.CheckingFunction =
    Tests.CallCheck.checking "Faulty" (fun fn _ -> act fn)

let compilePaths : obj[] seq = [ "Compile"; "CompileToArtifact" ] |> Seq.map (fun p -> [| box p |])

/// Compile `faultSource` with `f` on `path`, wrapped by `mustReturn`.
let private compileWithCheck (path: string) (f: Tests.CallCheck.CheckingFunction) : JyroResult<obj> =
    let c = (Script.compilerFor { pinned with Functions = [ f.AsFunction ] }).WithSource(faultSource)
    let run = compileMembers |> List.find (fst >> (=) path) |> snd
    mustReturn path (fun () -> run c)

[<Theory>]
[<MemberData(nameof compilePaths)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a CheckCall that throws InvalidOperationException gives UnknownCompilerError, not a throw`` (path: string) =
    let f = checkThat (fun _ -> raise (InvalidOperationException "the host's check failed"))
    let r = compileWithCheck path f
    Assert.Equal(1, f.CheckCount)
    unknownCompilerErrorIn ("CheckCall throwing InvalidOperationException, " + path) r

[<Theory>]
[<MemberData(nameof compilePaths)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a CheckCall that throws an exception type of the host's own gives UnknownCompilerError`` (path: string) =
    let f = checkThat (fun _ -> raise (HostFault "the host's own fault"))
    let r = compileWithCheck path f
    unknownCompilerErrorIn ("CheckCall throwing HostFault, " + path) r

let raiseRuntimeCases : obj[] seq =
    [ for code in [ "InvalidJson"; "DivisionByZero" ] do
        for path in [ "Compile"; "CompileToArtifact" ] -> [| box code; box path |] ]

[<Theory>]
[<MemberData(nameof raiseRuntimeCases)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a throw of JyroError.Runtime inside CheckCall gives a diagnostic of its code and arguments, not UnknownCompilerError`` (codeName: string, path: string) =
    let code = Enum.Parse(typeof<MessageCode>, codeName) :?> MessageCode
    let f = checkThat (fun _ -> raise (JyroError.Runtime(code, [| box "at"; box 7 |])) |> ignore)
    let r = compileWithCheck path f
    Assert.False(r.IsSuccess, Script.describe r)
    match Tests.CallCheck.ofCode code r with
    | [] -> Values.fail (sprintf "No %s. Diagnostics:\n%s" codeName (Script.describe r))
    | ds -> Assert.True(ds |> List.exists (fun d -> Tests.CallCheck.argsOf d = [ "at"; "7" ]),
                        sprintf "No %s whose arguments read [at; 7]: %A" codeName (ds |> List.map Tests.CallCheck.argsOf))
    Assert.Empty(Tests.CallCheck.ofCode Tests.CallCheck.unknownCompilerError r)

let raiseHostCases : obj[] seq =
    [ for m in [ "Host"; "HostFromBase" ] do
        for path in [ "Compile"; "CompileToArtifact" ] -> [| box m; box path |] ]

[<Theory>]
[<MemberData(nameof raiseHostCases)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 a throw of JyroError.Host inside CheckCall gives HostLinkError, not UnknownCompilerError`` (memberName: string, path: string) =
    let f =
        checkThat (fun fn ->
            if memberName = "HostFromBase" then fn.Refuse("Refused", Some [| box 1 |])
            else raise (JyroError.Host("Refused", [| box 1 |])) |> ignore)
    let r = compileWithCheck path f
    Assert.False(r.IsSuccess, Script.describe r)
    match Tests.CallCheck.ofCode Tests.CallCheck.hostLinkError r with
    | [ d ] -> Assert.Equal<string list>([ "Refused"; "1" ], Tests.CallCheck.argsOf d)
    | ds -> Values.fail (sprintf "expected one HostLinkError, got %d. Diagnostics:\n%s" ds.Length (Script.describe r))
    Assert.Empty(Tests.CallCheck.ofCode Tests.CallCheck.unknownCompilerError r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 the clause's first example, a CheckCall that throws InvalidOperationException, makes Compile return a failure`` () =
    let check = checkThat (fun _ -> raise (InvalidOperationException "boom"))
    let r = compileWithCheck "Compile" check
    unknownCompilerErrorIn "the clause's CheckCall example" r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 the clause's second example, a Name that throws, makes Compile return a failure and not throw`` () =
    let name = ArmedFault("Name", fun () -> InvalidOperationException("boom") :> exn)
    let c = (Script.compilerFor { pinned with Functions = [ name :> IJyroFunction ] }).WithSource(faultSource)
    name.Arm()
    let r2 = mustReturn "Compile" (fun () -> Tests.CallCheck.boxed (c.Compile()))
    if name.Thrown > 0 then unknownCompilerErrorIn "the clause's Name example" r2

[<Theory>]
[<MemberData(nameof compilePaths)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 an Effect that throws gives MaxEffectNotApplicable and no UnknownCompilerError`` (path: string) =
    // The condition is JYRO-REQ-2136's: the result is a failure that returns, and the code is not this clause's.
    for kind in Tests.CallCheck.effectFaultKinds do
        let f = Tests.CallCheck.EffectFaultFunction("Faulty", kind)
        let c = (Script.compilerFor { pinned with Functions = [ f :> IJyroFunction ] }).WithSource(faultSource)
        let run = compileMembers |> List.find (fst >> (=) path) |> snd
        let r = mustReturn path (fun () -> run c)
        let what = sprintf "Effect %s, %s" kind path
        failedWithAnError what r
        Assert.NotEmpty(Tests.CallCheck.ofCode Tests.CallCheck.effectMaxNotApplicable r)
        Assert.Empty(Tests.CallCheck.ofCode Tests.CallCheck.unknownCompilerError r)

// -- JYRO-DEF-553: a host's TimeoutException is not the compilation time limit ------------------
//
// A TimeoutException is not raised by RaiseRuntime, RaiseHost or RaiseError, so JYRO-REQ-2140
// gives it UnknownCompilerError like any other host exception; CompilationTimeLimitExceeded
// (100.9.1) is for a compilation that reached its limit. The 'none' cases set no compilation
// time limit on purpose, outside `pinned`: the defect reported the time limit even then.

let timeoutCases : obj[] seq =
    [ for limit in [ "pinned"; "none" ] do
        for path in [ "Compile"; "CompileToArtifact" ] -> [| box limit; box path |] ]

let private withLimit (limit: string) (o: RunOptions) : RunOptions =
    if limit = "none" then { o with CompilationTimeout = None } else o

/// No CompilationTimeLimitExceeded in `r`.
let private noTimeLimit (what: string) (r: JyroResult<obj>) =
    Assert.True((Tests.CallCheck.ofCode MessageCode.CompilationTimeLimitExceeded r).IsEmpty,
                sprintf "%s: CompilationTimeLimitExceeded for a TimeoutException the host threw. Diagnostics:\n%s" what (Script.describe r))

/// Every diagnostic of `code` in `r` has the stage `stage` (160.1.12), and there is one.
let private stageOfEach (code: MessageCode) (stage: string) (r: JyroResult<obj>) =
    match Tests.CallCheck.ofCode code r with
    | [] -> Values.fail (sprintf "No %A. Diagnostics:\n%s" code (Script.describe r))
    | ds -> for d in ds do Assert.Equal(stage, Pending.stageOf d)

[<Theory>]
[<MemberData(nameof timeoutCases)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2140 JYRO-DEF-553 a CheckCall that throws TimeoutException gives UnknownCompilerError at Link, not CompilationTimeLimitExceeded`` (limit: string, path: string) =
    let f = checkThat (fun _ -> raise (TimeoutException "the host's database timed out"))
    let c = (Script.compilerFor (withLimit limit { pinned with Functions = [ f.AsFunction ] })).WithSource(faultSource)
    let run = compileMembers |> List.find (fst >> (=) path) |> snd
    let r = mustReturn path (fun () -> run c)
    let what = sprintf "CheckCall throwing TimeoutException, limit %s, %s" limit path
    Assert.Equal(1, f.CheckCount)
    unknownCompilerErrorIn what r
    noTimeLimit what r
    // JYRO-REQ-2139 row 2: an internal error has the stage that was running, Link for a check.
    stageOfEach Tests.CallCheck.unknownCompilerError "Link" r

[<Theory>]
[<MemberData(nameof faultingMembers)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-2140 JYRO-DEF-553 a Name or Signature that throws TimeoutException gives UnknownCompilerError, not CompilationTimeLimitExceeded`` (memberName: string, path: string) =
    for limit in [ "pinned"; "none" ] do
        let f = ArmedFault(memberName, fun () -> TimeoutException("the host's registry timed out") :> exn)
        let c = (Script.compilerFor (withLimit limit { pinned with Functions = [ f :> IJyroFunction ] })).WithSource(faultSource)
        let run = compileMembers |> List.find (fst >> (=) path) |> snd
        f.Arm()
        let r = mustReturn path (fun () -> run c)
        if f.Thrown > 0 then
            let what = sprintf "%s throwing TimeoutException, limit %s, %s" memberName limit path
            unknownCompilerErrorIn what r
            noTimeLimit what r

[<Theory>]
[<MemberData(nameof compilePaths)>]
[<Trait("requirement", "JYRO-REQ-1449")>]
[<Trait("requirement", "JYRO-REQ-2140")>]
let ``JYRO-REQ-1449 JYRO-DEF-553 a compilation that reaches its time limit inside a slow CheckCall still gives CompilationTimeLimitExceeded`` (path: string) =
    // The check blocks well past the 200 ms limit, and is released once the call has returned,
    // so no worker is left waiting. The event is not disposed: the abandoned worker may still
    // be waking from it.
    let release = new ManualResetEventSlim(false)
    let f = checkThat (fun _ -> release.Wait(TimeSpan.FromSeconds 10.) |> ignore)
    let o = { pinned with Functions = [ f.AsFunction ]; CompilationTimeout = Some (TimeSpan.FromMilliseconds 200.) }
    let c = (Script.compilerFor o).WithSource(faultSource)
    let run = compileMembers |> List.find (fst >> (=) path) |> snd
    let r = try mustReturn path (fun () -> run c) finally release.Set()
    Assert.False(r.IsSuccess, Script.describe r)
    if (Tests.CallCheck.ofCode MessageCode.CompilationTimeLimitExceeded r).IsEmpty then
        Values.fail (sprintf "%s: a compilation past its limit gave no CompilationTimeLimitExceeded. Diagnostics:\n%s" path (Script.describe r))
    Assert.Empty(Tests.CallCheck.ofCode Tests.CallCheck.unknownCompilerError r)

// -- JYRO-DEF-554 (JYRO-ADR-131): only RaiseRuntime's exception keeps its code ------------------
//
// JYRO-REQ-2140: a JyroRuntimeException that escapes CheckCall keeps its code only when
// JyroError.RaiseRuntime raised it; one an engine member throws inside the check (InvalidCast
// from AsNumber of a string, UnpairedSurrogate from ToJson) is UnknownCompilerError. Each case
// records the engine's exception as it passes, so a check that failed for another reason does
// not pass for this one. The decision is by who raised it, not by its code: RaiseRuntime of
// InvalidCast keeps InvalidCast. On source, through Compile and CompileToArtifact, and on a load
// of an artifact written by a host whose Faulty has no check.

let private engineSource = "Output.x = Faulty(\"a\")"

/// Compile `source` with `f` on `path`: "Compile", "CompileToArtifact", or "Load", a Compile()
/// of the artifact of `source` written by a host whose `Faulty` has no check.
let private compileOn (path: string) (f: Tests.CallCheck.CheckingFunction) (source: string) : JyroResult<obj> =
    match path with
    | "Load" ->
        let plain = Host.func "Faulty" Tests.CallCheck.fiveOptional ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)
        let bytes = Loader.compileToArtifactWith { pinned with Functions = [ plain.AsFunction ] } source |> Script.valueOf
        mustReturn "a load" (fun () -> Tests.CallCheck.boxed (Loader.loadWith { pinned with Functions = [ f.AsFunction ] } bytes))
    | _ ->
        let c = (Script.compilerFor { pinned with Functions = [ f.AsFunction ] }).WithSource(source)
        let run = compileMembers |> List.find (fst >> (=) path) |> snd
        mustReturn path (fun () -> run c)

let private engineMembers : (string * (Tests.CallCheck.Site -> unit)) list =
    [ "AsNumber of the string literal", (fun s ->
        match s.LiteralAt 0 with
        | Tests.CallCheck.Returned (true, v) when not (isNull (box v)) -> v.AsNumber() |> ignore
        | other -> raise (InvalidOperationException (sprintf "TryGetLiteral(0) of \"a\" gave %A" other)))
      // As 1682's example builds it (S160_07_DiagnosticArgsAndFormatter): an F# "\uD800" literal
      // does not survive as a lone surrogate, so the char is made from its number.
      "ToJson of an unpaired surrogate", (fun _ -> (Values.obj [ "k", Values.str (string (char 0xD800)) ]).ToJson() |> ignore) ]

let engineCases : obj[] seq =
    [ for (name, _) in engineMembers do
        for path in [ "Compile"; "CompileToArtifact"; "Load" ] -> [| box name; box path |] ]

[<Theory>]
[<MemberData(nameof engineCases)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2140 JYRO-DEF-554 a JyroRuntimeException that an engine member throws inside CheckCall gives UnknownCompilerError at Link, not its own code`` (caseName: string, path: string) =
    let act = engineMembers |> List.find (fst >> (=) caseName) |> snd
    let seen = ResizeArray<MessageCode>()
    let f =
        Tests.CallCheck.checking "Faulty" (fun _ s ->
            try act s
            with :? JyroRuntimeException as e ->
                seen.Add e.Code
                reraise ())
    let r = compileOn path f engineSource
    let what = sprintf "%s in CheckCall, %s" caseName path
    let engineCode =
        match List.ofSeq seen with
        | [ c ] -> c
        | cs -> Values.fail (sprintf "%s: expected the engine member to throw one JyroRuntimeException, saw %A. Diagnostics:\n%s" what cs (Script.describe r))
    unknownCompilerErrorIn what r
    stageOfEach Tests.CallCheck.unknownCompilerError "Link" r
    Assert.True((Tests.CallCheck.ofCode engineCode r).IsEmpty,
                sprintf "%s: the engine's %A kept its code. Diagnostics:\n%s" what engineCode (Script.describe r))

let compileAndLoadPaths : obj[] seq = [ "Compile"; "CompileToArtifact"; "Load" ] |> Seq.map (fun p -> [| box p |])

[<Theory>]
[<MemberData(nameof compileAndLoadPaths)>]
[<Trait("requirement", "JYRO-REQ-2140")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2140 JYRO-DEF-554 RaiseRuntime of InvalidCast inside CheckCall keeps InvalidCast and its arguments at Link, on source and on a load`` (path: string) =
    let f = Tests.CallCheck.checking "Faulty" (fun _ _ -> raise (JyroError.Runtime(MessageCode.InvalidCast, [| box "string"; box "number" |])) |> ignore)
    let r = compileOn path f engineSource
    Assert.False(r.IsSuccess, Script.describe r)
    match Tests.CallCheck.ofCode MessageCode.InvalidCast r with
    | [ d ] ->
        Assert.Equal<string list>([ "string"; "number" ], Tests.CallCheck.argsOf d)
        Assert.Equal("Link", Pending.stageOf d)
    | ds -> Values.fail (sprintf "%s: expected one InvalidCast, got %d. Diagnostics:\n%s" path ds.Length (Script.describe r))
    Assert.Empty(Tests.CallCheck.ofCode Tests.CallCheck.unknownCompilerError r)

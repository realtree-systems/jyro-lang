/// Part 160.1 and 160.2: host capabilities, diagnostics' stages and host functions.
module RealTree.Jyro.Spec.Tests.S160_01_Host

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading
open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact

// -- 160.1.1 Source text or artifact --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2094")>]
let ``Host_CanGiveASourceTextOrACompiledArtifact`` () =
    Assert.Equal("{\"a\":1}", Values.toJson (Script.run "Output.a = 1" |> ok))
    let bytes = Script.compileBytesWith Script.defaults "Output.a = 1" |> Script.valueOf
    Assert.Equal("{\"a\":1}", Values.toJson (Script.runBytesWith Script.defaults bytes |> ok))
    Assert.Equal("{\"a\":1}", Values.toJson (runOnPath ExecuteScript "Output.a = 1" |> ok))

// -- 160.1.2 Input as a Jyro value, a host value or JSON text ---------------------------------------

type Poco() =
    member val a = 1 with get, set

let inputPaths : obj[] seq =
    [ "jyro-value"; "anonymous"; "dictionary"; "poco"; "json"; "json-stream"; "ExecuteScript"; "ExecuteScriptWithJson" ]
    |> Seq.map (fun p -> [| box p |])

[<Theory>]
[<MemberData(nameof inputPaths)>]
[<Trait("requirement", "JYRO-REQ-1577")>]
let ``Host_CanGiveInputAsAJyroValueAHostValueOrJson`` (path: string) =
    let src = "Output.v = Input.a"
    let dict () = let d = Dictionary<string, obj>() in d.["a"] <- box 1; d
    let r =
        match path with
        | "jyro-value" -> Script.runInput (Values.obj [ "a", Values.int 1 ]) src
        | "anonymous" -> Script.runWith { Script.defaults with Input = HostInput(box {| a = 1 |}) } src
        | "dictionary" -> Script.runWith { Script.defaults with Input = HostInput(box (dict ())) } src
        | "poco" -> Script.runWith { Script.defaults with Input = HostInput(box (Poco())) } src
        | "json" -> Script.runJson "{\"a\": 1}" src
        | "json-stream" ->
            let p = Script.compile src |> Script.valueOf
            JyroRunner(p).WithInputJson(new MemoryStream(Encoding.UTF8.GetBytes "{\"a\": 1}")).Execute()
        | "ExecuteScript" -> JyroScript.executeScript src (box (dict ()))
        | _ -> JyroScript.executeScriptWithJson src "{\"a\": 1}"
    Assert.Equal(1m, Values.asDecimal (Values.prop "v" (ok r)))

// -- 160.1.3 A first value for Output -----------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1578")>]
let ``Host_CanGiveAFirstValueForOutput`` () =
    let first = Values.obj [ "pre", Values.int 1 ]
    let v = Script.runWith { Script.defaults with Output = Some first } "Output.post = 2\nOutput.seen = Output.pre" |> ok
    Assert.Equal("{\"pre\":1,\"post\":2,\"seen\":1}", Values.toJson v)

// -- 160.1.4 Choosing the standard library functions ------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1579")>]
let ``Host_ChoosesWhichStandardFunctionsAScriptCanCall`` () =
    let chosen = stdlibAll () |> List.filter (fun f -> f.Name = "Length" || f.Name = "ToUpper")
    let o = { Script.defaults with Stdlib = false; Functions = chosen }
    let v = Script.runWith o "Output.l = Length([1, 2])\nOutput.u = ToUpper(\"a\")" |> ok
    Assert.Equal("{\"l\":2,\"u\":\"A\"}", Values.toJson v)
    Script.compileWith o "Output.x = ToLower(\"A\")" |> fails MessageCode.UndefinedFunction
    Script.compileWith { Script.defaults with Stdlib = false } "Output.x = Length([1])" |> fails MessageCode.UndefinedFunction

// -- 160.1.5 Host functions ---------------------------------------------------------------------------

type DoubleIt() =
    inherit JyroFunctionBase("DoubleIt", Host.signature "DoubleIt" [ Parameter.Required("x", ParameterType.Number) ] ParameterType.Number)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(args, _) = JyroNumber(args.GetDecimal(0) * 2m) :> JyroValue

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1580")>]
let ``Host_CanRegisterHostFunctions_OneByOneOrSeveral`` () =
    let triple = Host.func "Triple" [ Parameter.Required("x", ParameterType.Number) ] ParameterType.Number FunctionEffect.Pure (fun a -> Values.exact (Values.asDecimal a.Head * 3m))
    let hello = Host.func "Hello" [] ParameterType.String FunctionEffect.Pure (fun _ -> Values.str "hi")
    let p =
        JyroCompiler().WithStdlib().AddFunction(DoubleIt()).AddFunctions([ triple.AsFunction; hello.AsFunction ])
            .WithSource("Output.d = DoubleIt(2)\nOutput.t = Triple(2)\nOutput.h = Hello()").Compile()
        |> Script.valueOf
    let v = (Script.runnerFor Script.defaults p).Execute() |> ok
    Assert.Equal("{\"d\":4,\"t\":6,\"h\":\"hi\"}", Values.toJson v)

// -- 160.1.6 Changing the limits ------------------------------------------------------------------------

let private runWithRunner (src: string) (configure: JyroRunner -> JyroRunner) =
    let p = Script.compile src |> Script.valueOf
    (configure (JyroRunner(p).WithInputJson("{}"))).Execute()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1581")>]
let ``Host_CanLowerAndRaiseEachLimit_ThroughTheSettersAndTheOptions`` () =
    let stmts n = lines (repeatLines n "Output.s = {i}")
    let loop n = sprintf "var i = 0\nwhile i < %d do i = i + 1 end" n
    // statements
    runWithRunner (stmts 5) (fun r -> r.WithMaxStatements 5) |> ok |> ignore
    runWithRunner (stmts 6) (fun r -> r.WithMaxStatements 5) |> fails MessageCode.StatementLimitExceeded
    runWithRunner (statementScript 20_000) (fun r -> r.WithMaxStatements 20_000) |> ok |> ignore
    runWithRunner (statementScript 20_001) (fun r -> r.WithMaxStatements 20_000) |> fails MessageCode.StatementLimitExceeded
    // loops
    runWithRunner (loop 3) (fun r -> r.WithMaxLoopIterations 3) |> ok |> ignore
    runWithRunner (loop 4) (fun r -> r.WithMaxLoopIterations 3) |> fails MessageCode.LoopIterationLimitExceeded
    runWithRunner (loop 2000) (fun r -> r.WithMaxLoopIterations 2000) |> ok |> ignore
    runWithRunner (loop 2001) (fun r -> r.WithMaxLoopIterations 2000) |> fails MessageCode.LoopIterationLimitExceeded
    // call depth
    runWithRunner (recursion 3) (fun r -> r.WithMaxCallDepth 3) |> ok |> ignore
    runWithRunner (recursion 4) (fun r -> r.WithMaxCallDepth 3) |> fails MessageCode.CallDepthLimitExceeded
    runWithRunner (recursion 80) (fun r -> r.WithMaxCallDepth 80) |> ok |> ignore
    runWithRunner (recursion 81) (fun r -> r.WithMaxCallDepth 80) |> fails MessageCode.CallDepthLimitExceeded
    // through the options object
    let opts = { JyroExecutionOptions.Default with MaxStatements = 5; MaxLoopIterations = 3; MaxCallDepth = 3 }
    runWithRunner (stmts 6) (fun r -> r.WithExecutionOptions opts) |> fails MessageCode.StatementLimitExceeded
    runWithRunner (loop 4) (fun r -> r.WithExecutionOptions { opts with MaxStatements = 100 }) |> fails MessageCode.LoopIterationLimitExceeded
    runWithRunner (recursion 4) (fun r -> r.WithExecutionOptions { opts with MaxStatements = 100 }) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1581")>]
let ``Host_CanLowerAndRaiseTheTimeLimit`` () =
    let st () = stall ()
    let run (limit: TimeSpan) (ms: int) (viaOptions: bool) =
        let s = st ()
        let p = Script.compileWith { Script.defaults with Functions = [ s.AsFunction ] } (sprintf "Stall(%d)\nOutput.after = 1" ms) |> Script.valueOf
        let r = JyroRunner(p).WithInputJson("{}")
        let r = if viaOptions then r.WithExecutionOptions { JyroExecutionOptions.Default with MaxExecutionTime = limit } else r.WithMaxExecutionTime limit
        r.Execute()
    // Every limit that must be met by a run ending `ok` is ten times the longest
    // duration of that step measured under a whole run of this project
    // (floors-process 4.1.18, JYRO-DEF-455). Measurement (ZZ_MarginProbe, not
    // committed): `dotnet test` of this project at 4b7eadd, 2026-10-09, 341
    // samples of each shape (Stall(5500): one in twenty) taken while its other
    // tests ran. From the call of Execute() to its end, `Stall(0)\nOutput.after = 1`
    // took at most 3.94 ms; `Stall(5500)` took at most 5,514.16 ms (an overrun of
    // 14.16 ms over its 5,500 ms); `Sleep(100)\nOutput.done = true` took at most
    // 168.27 ms (used by S160_07_RunnerCompilerResult). The lowered limit is
    // `marginLimitMs` (3,000 ms, ten times the 254 ms measured for the same
    // shape in Support.fs), not under one second; the raised limit, 60 s, is
    // more than ten times 5,514.16 ms (55.1 s) and above the default of 5 s.
    let lowered = TimeSpan.FromMilliseconds(float marginLimitMs)
    for via in [ true; false ] do
        run lowered marginStallMs via |> fails MessageCode.ExecutionTimeLimitExceeded
        run lowered 0 via |> ok |> ignore
    run (TimeSpan.FromSeconds 60.) 5500 false |> ok |> ignore
    run (TimeSpan.FromSeconds 60.) 5500 true |> ok |> ignore

// -- 160.1.7 A cancellation signal ---------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1582")>]
let ``Host_CanGiveACancellationSignal`` () =
    use cts = new CancellationTokenSource()
    // Compiled first, and timed over Execute alone (floors-process 4.1.14). A
    // signal that were not taken would let the Sleep run to the 30 s time limit. The cancel
    // comes from a thread of its own, and the Sleep is far longer than any late cancel
    // (JYRO-DEF-745).
    let runner = JyroRunner(Script.compileWith pinned (sprintf "Sleep(%d)\nOutput.after = 1" RealTree.Jyro.Spec.HostCancel.unboundedSleepMs) |> Script.valueOf)
                     .WithInputJson("{}").WithExecutionOptions(Script.limits allLimits).WithCancellationToken(cts.Token)
    RealTree.Jyro.Spec.HostCancel.cancelFromThread cts (TimeSpan.FromMilliseconds 100.)
    let r, elapsed = timed (fun () -> runner.Execute())
    fails MessageCode.CanceledByHost r
    Assert.True(elapsed < TimeSpan.FromSeconds 25., sprintf "took %O: the Sleep was not canceled" elapsed)

// -- 160.1.8 The result, the statistics and the diagnostics ---------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1583")>]
let ``Host_ReceivesTheResultTheStatisticsAndTheDiagnostics`` () =
    let o, s = withStats Script.defaults
    let good = Script.runWith o "Output.a = 1"
    Assert.True(good.IsSuccess)
    Assert.Equal("{\"a\":1}", Values.toJson (Script.valueOf good))
    Assert.Equal(1L, s.StatementCount)
    Assert.True(s.TotalTime > TimeSpan.Zero)
    let o2, s2 = withStats Script.defaults
    let bad = Script.runWith o2 "Output.a = 1\nOutput.b = 1 / 0"
    Assert.False(bad.IsSuccess)
    Assert.Equal<MessageCode list>([ MessageCode.DivisionByZero ], Script.codes bad)
    Assert.True(s2.ExecuteTime > TimeSpan.Zero)
    let p = Script.compile "Output.b = 1 / 0" |> Script.valueOf
    let ex = Assert.ThrowsAny<JyroException>(fun () -> JyroRunner(p).ExecuteOrThrow() |> ignore)
    Assert.Equal(MessageCode.DivisionByZero, ex.Messages.[0].Code)

// -- 160.1.9 / 160.7.1 The exit message -------------------------------------------------------------------

let exitCases : obj[] seq =
    [ "exit \"bye\"", Some "bye"
      "exit \"\"", Some ""
      "exit 7", Some "7"
      "exit [1]", Some "[1]"
      "exit {a: 1}", Some "{\"a\":1}"
      "exit true", Some "true"
      "exit null", Some "null"
      "exit", None
      "Output.a = 1", None
      "fail \"x\"", None
      "fail", None
      "Output.a = 1 / 0", None
      "func F()\n  exit \"in-f\"\nend\nF()", Some "in-f" ]
    |> Seq.map (fun (s, e) -> [| box s; box (defaultArg e null) |])

// The case `exit (x => x)`, an error only by JYRO-REQ-253, is in that requirement's tests
// (S50_11_ExitFailDelete), as JYRO-ADR-113 item 2 places it; `Output.a = 1 / 0` shows the
// error case here with a code this work order's build raises.
[<Theory>]
[<MemberData(nameof exitCases)>]
[<Trait("requirement", "JYRO-REQ-1584")>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``ExitMessage_IsTheTextOfTheExitValue_AndNullOtherwise_OnEveryMember`` (source: string, expected: string) =
    let want = Option.ofObj expected
    for path in [ Runner; ExecuteScript; ExecuteScriptWithJson ] do
        let r = runOnPath path source
        Assert.True((Pending.exitMessage r = want), sprintf "%A: exit message %A, expected %A" path (Pending.exitMessage r) want)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``ExitMessage_IsAPublicGetOnlyStringPropertyOfJyroExecutionResult`` () =
    let p = typeof<JyroExecutionResult>.GetProperty("ExitMessage")
    Assert.NotNull(p)
    Assert.Equal(typeof<string>, p.PropertyType)
    Assert.True(p.CanRead)
    Assert.Null(p.GetSetMethod())
    Assert.True(typeof<JyroResult<JyroValue>>.IsAssignableFrom(typeof<JyroExecutionResult>))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``ExitMessage_OfAResultThatIsNotOfAnExecution_IsAbsent`` () =
    // Only the result of a run has an exit message: no JyroResult<T> has the member.
    for t in [ typeof<int>; typeof<byte[]>; typeof<CompiledProgram>; typeof<JyroLibrary>; typeof<JyroArtifactInfo>; typeof<JyroValue> ] do
        let closed = typedefof<JyroResult<_>>.MakeGenericType(t)
        Assert.Null(closed.GetProperty("ExitMessage"))
    Assert.Null(typedefof<JyroResult<_>>.GetProperty("ExitMessage"))
    // A compile failure or a compile result carries no exit message.
    Assert.Equal(None, Pending.exitMessage (Script.compile "exit \"x\""))
    Assert.Equal(None, Pending.exitMessage (Script.run "var = ("))
    Assert.Equal(None, Pending.exitMessage (Script.compileBytesWith Script.defaults "exit \"x\""))

// -- 160.1.10 / 160.7.2 / 160.7.3 The regex timeout ----------------------------------------------------------

/// A subject for "(a+)+$" whose backtracking takes 2 to 4 seconds on this
/// machine with .NET's ECMAScript regex engine (calibrated once).
let private slowSubject : Lazy<string> =
    lazy
        (let time n =
            let s = String('a', n) + "!"
            let sw = Diagnostics.Stopwatch.StartNew()
            Regex.IsMatch(s, "(a+)+$", RegexOptions.ECMAScript) |> ignore
            sw.Elapsed.TotalSeconds
         let mutable n = 18
         let mutable t = time n
         while t < 0.4 && n < 40 do
             n <- n + 1
             t <- time n
         // each extra 'a' doubles the time
         while t < 2.0 && n < 40 do
             n <- n + 1
             t <- t * 2.0
         String('a', n) + "!")

let private regexScript () = sprintf "Output.r = RegexTest(\"%s\", \"(a+)+$\")" slowSubject.Value

let private regexRunner () = JyroRunner(Script.compile (regexScript ()) |> Script.valueOf).WithInputJson("{}")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2095")>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``RegexTimeout_IsTheOneTheHostSetForTheExecution`` () =
    let r = Pending.withRegexTimeout (regexRunner ()) (TimeSpan.FromMilliseconds 50.)
    fails MessageCode.RegexTimeout (r.Execute())
    fails MessageCode.RegexTimeout (r.Execute())
    let generous = Pending.withRegexTimeout (regexRunner ()) (TimeSpan.FromSeconds 60.)
    let result, elapsed = timed (fun () -> generous.Execute())
    Assert.DoesNotContain(MessageCode.RegexTimeout, Script.codes result)
    Assert.True(elapsed < TimeSpan.FromSeconds 9.)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``RegexTimeout_WithoutTheCall_IsOneSecond`` () =
    // The runner is compiled before the timing (floors-process 4.1.14).
    let runner = regexRunner ()
    let result, elapsed = timed (fun () -> runner.Execute())
    fails MessageCode.RegexTimeout result
    Assert.True(elapsed >= TimeSpan.FromSeconds 0.9 && elapsed < TimeSpan.FromSeconds 2.5, sprintf "took %O" elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``WithMaxRegexTime_AcceptsTheRange_AndReturnsTheRunner`` () =
    let m = typeof<JyroRunner>.GetMethod("WithMaxRegexTime", [| typeof<TimeSpan> |])
    Assert.NotNull(m)
    Assert.Null(typeof<JyroRunner>.GetMethod("WithRegexTimeout", [| typeof<TimeSpan> |]))
    Assert.Equal(typeof<JyroRunner>, m.ReturnType)
    for t in [ TimeSpan.FromTicks 1L; TimeSpan.FromMilliseconds 1.; TimeSpan.FromSeconds 1.; TimeSpan.FromHours 24.; TimeSpan.FromHours(24.) - TimeSpan.FromTicks 1L ] do
        let r = regexRunner ()
        Assert.Same(r, Pending.withRegexTimeout r t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``SecondCall_ReplacesTheFirst`` () =
    let r = regexRunner ()
    Pending.withRegexTimeout r (TimeSpan.FromSeconds 60.) |> ignore
    Pending.withRegexTimeout r (TimeSpan.FromMilliseconds 50.) |> ignore
    fails MessageCode.RegexTimeout (r.Execute())

let refusedTimeouts : obj[] seq =
    [ TimeSpan.Zero; TimeSpan.FromTicks -1L; TimeSpan.FromSeconds -1.; Timeout.InfiniteTimeSpan; TimeSpan.MinValue
      TimeSpan.FromHours 24. + TimeSpan.FromTicks 1L; TimeSpan.FromHours 25.; TimeSpan.MaxValue ]
    |> Seq.map (fun t -> [| box t.Ticks |])

[<Theory>]
[<MemberData(nameof refusedTimeouts)>]
[<Trait("requirement", "JYRO-REQ-2095")>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``WithMaxRegexTime_OutsideTheRange_ThrowsArgumentOutOfRangeException_ForTime`` (ticks: int64) =
    let r = regexRunner ()
    let e = Assert.Throws<ArgumentOutOfRangeException>(fun () -> Pending.withRegexTimeout r (TimeSpan.FromTicks ticks) |> ignore)
    Assert.Equal("time", e.ParamName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2095")>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``AfterARefusedTimeout_TheRunnerKeepsItsEarlierTimeout`` () =
    let r = regexRunner ()
    Pending.withRegexTimeout r (TimeSpan.FromMilliseconds 50.) |> ignore
    try Pending.withRegexTimeout r (TimeSpan.FromHours 25.) |> ignore with :? ArgumentOutOfRangeException -> ()
    let result, elapsed = timed (fun () -> r.Execute())
    fails MessageCode.RegexTimeout result
    Assert.True(elapsed < TimeSpan.FromSeconds 0.8, sprintf "took %O: the 50 ms timeout was not kept" elapsed)
    let fresh = regexRunner ()
    try Pending.withRegexTimeout fresh TimeSpan.Zero |> ignore with :? ArgumentOutOfRangeException -> ()
    let result2, elapsed2 = timed (fun () -> fresh.Execute())
    fails MessageCode.RegexTimeout result2
    Assert.True(elapsed2 >= TimeSpan.FromSeconds 0.9, sprintf "took %O: not the 1 s default" elapsed2)

// -- 160.1.11 / 160.1.12 / 160.7.4 / 160.7.5 The stage of a diagnostic ---------------------------------------

let private sixStages = [ "Deserialize"; "Parse"; "Validate"; "Link"; "Compile"; "Execute" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1615")>]
let ``MessageStage_IsAPublicEnumWithTheSixMembers`` () =
    let t = Pending.messageStageType ()
    Assert.True(t.IsEnum && t.IsPublic)
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.Equal<string list>(sixStages, Enum.GetNames t |> List.ofArray |> List.sortBy (fun n -> Convert.ToInt32(Enum.Parse(t, n))))
    for i, name in List.indexed sixStages do
        Assert.Equal(i, Convert.ToInt32(Enum.Parse(t, name)))
    Assert.Equal(6, Enum.GetValues(t).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2105")>]
let ``DiagnosticMessage_HasAPublicGetOnlyStageOfTypeMessageStage`` () =
    let p = typeof<DiagnosticMessage>.GetProperty("Stage")
    if isNull p then Pending.notInBuild "160.7.5" "DiagnosticMessage.Stage"
    Assert.Equal(Pending.messageStageType (), p.PropertyType)
    Assert.Null(p.GetSetMethod())

/// A load only (JyroArtifactLoader(...).Load()) by a host with the options `o`.
let private loadWith (o: RunOptions) (bytes: byte[]) : JyroResult<CompiledProgram> =
    Script.loadBytesWith o (bytes)

/// A top-level declaration `func <name>(x) return 1 end`, hand-built for an artifact, so that
/// no source compilation has refused it before the load sees it.
let private declared (name: string) : Stmt = SFunc(name, [ "x", None, None ], [ SReturn(Some(num 1m), p0) ], p0)

/// `union S Circle(r) end` and `func Circle(x) return 1 end`, hand-built (50.10.9).
let private variantAndFunction : Stmt list = [ unionOf "S" [ "Circle", [ "r", None ] ]; declared "Circle" ]

/// The options `pinned` with one host function registered, named `name`.
let private withHost (name: string) : RunOptions = { pinned with Functions = [ (Host.recorder name).AsFunction ] }

/// The artifact a writer with `pinned` and no host function writes for `source`.
let private writtenWithout (source: string) : byte[] = Script.compileBytesWith pinned source |> Script.valueOf

/// A host function `Broken` whose `Effect` throws (JYRO-REQ-2136).
let private brokenEffect () : IJyroFunction =
    Tests.CallCheck.EffectFaultFunction("Broken", "InvalidOperationException") :> IJyroFunction

/// One result per row of the table of 160.1.12: the result, the code (None for a
/// parse error the book gives no code) and its stage.
let private stageRows () : (string * (unit -> JyroResult<obj>) * MessageCode option * string) list =
    let boxed (r: JyroResult<'T>) : JyroResult<obj> =
        if r.IsSuccess then JyroResult<obj>.Success(box (Script.valueOf r), r.Messages)
        else JyroResult<obj>.Failure(r.Messages)
    let valid = build [ setOut "x" (num 1m) ]
    let lib = "func L()\n  return 1\nend"
    [ "magic", (fun () -> boxed (Script.loadBytesWith pinned ([| 1uy |]))), Some MessageCode.ArtifactInvalidMagic, "Deserialize"
      "unreadable", (fun () -> boxed (Script.loadBytesWith pinned (valid.[0..50]))), Some MessageCode.ArtifactUnreadable, "Deserialize"
      "version", (fun () -> boxed (Script.loadBytesWith pinned (encode { artifactOf [] with Version = 10us }))), Some MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "undefined-on-load", (fun () -> boxed (Script.loadBytesWith pinned (encode { artifactOf [] with Functions = [ "Nope" ] }))), Some MessageCode.UndefinedFunction, "Link"
      "effect-on-load", (fun () ->
            let b = Script.compileBytesWith pinned "Sleep(0)" |> Script.valueOf
            boxed (Script.loadBytesWith { pinned with MaxEffect = Some FunctionEffect.Pure } (b))), Some MessageCode.ArtifactEffectExceedsMax, "Link"
      "artifact-info", (fun () -> boxed (JyroArtifactInfo.Read [| 1uy; 2uy |])), Some MessageCode.ArtifactInvalidMagic, "Deserialize"
      // Rows 1 to 7 of 150.2.9, and refusals of JyroArtifactInfo.Read: Deserialize (160.1.12, row 2).
      "too-large", (fun () -> boxed (Script.loadBytesWith pinned (Array.zeroCreate 10_485_761))), Some MessageCode.ArtifactTooLarge, "Deserialize"
      "version-0", (fun () -> boxed (Script.loadBytesWith pinned (encode { artifactOf [] with Version = 0us }))), Some MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "table-over-1000", (fun () -> boxed (Script.loadBytesWith pinned (encode { artifactOf [] with Functions = [ for i in 1 .. 1001 -> sprintf "F%d" i ] }))), Some MessageCode.ArtifactUnreadable, "Deserialize"
      "unassigned-tag", (fun () -> boxed (Script.loadBytesWith pinned (build [ SRaw [| 0x31uy |] ]))), Some MessageCode.ArtifactUnreadable, "Deserialize"
      "info-too-large", (fun () -> boxed (JyroArtifactInfo.Read(Array.zeroCreate 10_485_761))), Some MessageCode.ArtifactTooLarge, "Deserialize"
      "info-version-0", (fun () -> boxed (JyroArtifactInfo.Read(encode { artifactOf [] with Version = 0us }))), Some MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "info-truncated", (fun () -> boxed (JyroArtifactInfo.Read(valid.[0..5]))), Some MessageCode.ArtifactUnreadable, "Deserialize"
      // A link-time rejection of a variant call (50.10.7): Link.
      "variant-too-few", (fun () -> boxed (Script.compileWith pinned "union U V(a, b) end\nOutput.v = V(1)")), Some MessageCode.TooFewArguments, "Link"
      // A parse-time rejection with its own code: Parse ("unterminated" and "escape" below).
      // Any other rejection before the run: Validate.
      "typed-var", (fun () -> boxed (Script.compileWith pinned "var s: string")), Some MessageCode.TypedVariableNeedsInitializer, "Validate"
      "cannot-encode", (fun () -> boxed (Script.compileBytesWith pinned ("Output.x = " + arrayLiteral 10_001))), Some MessageCode.ArtifactCannotEncode, "Compile"
      "encode-libraries", (fun () -> boxed (Script.compileBytesWith { pinned with Libraries = [ lib ] } "Output.x = 1")), Some MessageCode.ArtifactCannotEncodeLibraries, "Compile"
      "unterminated", (fun () -> boxed (Script.compileWith pinned "Output.x = \"abc")), Some MessageCode.UnterminatedString, "Parse"
      "escape", (fun () -> boxed (Script.compileWith pinned "Output.x = \"\\q\"")), Some MessageCode.InvalidEscapeSequence, "Parse"
      // A parse error with no code the book gives it (20.1.1 rejects `var = 1`; 130.1.4
      // leaves UnknownParserError to the implementation, d-563): the stage alone.
      "parser", (fun () -> boxed (Script.compileWith pinned "var = 1")), None, "Parse"
      // The refusal NoSourceProvided of a Compile() given no source text (160.7.14): Validate.
      // Both paths of 160.7.14's refusal, without and with WithStdlib (FLOORS 4.1.8).
      "no-source", (fun () -> boxed ((JyroCompiler()).WithMaxCompilationTime(generousCompile).Compile())), Some MessageCode.NoSourceProvided, "Validate"
      "no-source-stdlib", (fun () -> boxed ((JyroCompiler()).WithStdlib().WithMaxCompilationTime(generousCompile).Compile())), Some MessageCode.NoSourceProvided, "Validate"
      "host-collision", (fun () -> boxed (Script.compileWith { pinned with Functions = [ (Host.recorder "Length").AsFunction ] } "Output.x = 1")), Some MessageCode.HostFunctionOverridesStdlib, "Link"
      // MaxEffectNotApplicable (120.5, JYRO-REQ-2136) is named in the Link row, on a source and on a
      // load: a call to a function whose Effect gives no effect, under each maximum.
      "effect-max-not-applicable", (fun () -> boxed (Script.compileWith { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ brokenEffect () ] } "Output.r = Broken()")), Some Tests.CallCheck.effectMaxNotApplicable, "Link"
      "effect-max-not-applicable-mutating", (fun () -> boxed (Script.compileWith { pinned with MaxEffect = Some FunctionEffect.Mutating; Functions = [ brokenEffect () ] } "Output.r = Broken()")), Some Tests.CallCheck.effectMaxNotApplicable, "Link"
      "effect-max-not-applicable-bytes", (fun () -> boxed (Script.compileBytesWith { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ brokenEffect () ] } "Output.r = Broken()")), Some Tests.CallCheck.effectMaxNotApplicable, "Link"
      "effect-max-not-applicable-on-load", (fun () ->
            let written = Script.compileBytesWith { pinned with Functions = [ (Host.recorder "Broken").AsFunction ] } "Output.r = Broken()" |> Script.valueOf
            boxed (Script.loadBytesWith { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ brokenEffect () ] } written)), Some Tests.CallCheck.effectMaxNotApplicable, "Link"
      "effect-max-not-applicable-in-library", (fun () -> boxed (Script.compileWith { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ brokenEffect () ]; Libraries = [ "func L()\n  return Broken()\nend" ] } "Output.x = 1")), Some Tests.CallCheck.effectMaxNotApplicable, "Link"
      "effect", (fun () -> boxed (Script.compileWith { pinned with MaxEffect = Some FunctionEffect.Pure } "Output.x = Now()")), Some MessageCode.FunctionEffectExceedsMax, "Link"
      "too-few", (fun () -> boxed (Script.compileWith pinned "func F(a)\n  return a\nend\nOutput.x = F()")), Some MessageCode.TooFewArguments, "Link"
      "too-many", (fun () -> boxed (Script.compileWith pinned "Output.x = Length([1], 2)")), Some MessageCode.TooManyArguments, "Link"
      "unknown-named", (fun () -> boxed (Script.compileWith pinned "Output.x = Round(value: 1, nope: 2)")), Some MessageCode.UnknownNamedArgument, "Link"
      "duplicate-named", (fun () -> boxed (Script.compileWith pinned "Output.x = Round(value: 1, value: 2)")), Some MessageCode.DuplicateNamedArgument, "Link"
      "missing-named", (fun () -> boxed (Script.compileWith pinned "func F(a, b)\n  return a\nend\nOutput.x = F(a: 1)")), Some MessageCode.MissingRequiredNamedArgument, "Link"
      "variant-shadows", (fun () -> boxed (Script.compileWith { pinned with Functions = [ (Host.recorder "Circle").AsFunction ] } "union S Circle(r) end")), Some MessageCode.VariantShadowsFunction, "Link"
      "function-shadows-variant", (fun () -> boxed (Script.compileWith pinned "union S Circle(r) end\nfunc Circle(x)\n  return x\nend")), Some MessageCode.FunctionShadowsVariant, "Link"
      "undeclared", (fun () -> boxed (Script.compileWith pinned "Output.x = zzz")), Some MessageCode.UndeclaredVariable, "Validate"
      "redeclared", (fun () -> boxed (Script.compileWith pinned "var a = 1\nvar a = 2")), Some MessageCode.VariableAlreadyDeclared, "Validate"
      "duplicate-hint", (fun () -> boxed (Script.compileWith pinned "var a = 1\na: number = 2")), Some MessageCode.DuplicateTypeHint, "Validate"
      "library-statement", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ "var x = 1" ] } "Output.x = 1")), Some MessageCode.LibraryStatementForbidden, "Validate"
      "shadows-library", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ lib ] } "func L()\n  return 2\nend")), Some MessageCode.FunctionShadowsLibrary, "Validate"
      "division", (fun () -> boxed (Script.runWith pinned "Output.x = 1 / 0")), Some MessageCode.DivisionByZero, "Execute"
      "not-iterable", (fun () -> boxed (Script.runWith pinned "var n = 5\nforeach x in n do end")), Some MessageCode.NotIterable, "Execute"
      "script-failure", (fun () -> boxed (Script.runWith pinned "fail \"x\"")), Some MessageCode.ScriptFailure, "Execute"
      "limit", (fun () -> boxed (Script.runWith { pinned with Limits = Some(Script.limits (fun o -> { allLimits o with MaxStatements = 1 })) } "Output.a = 1\nOutput.b = 2")), Some MessageCode.StatementLimitExceeded, "Execute"
      "library-parse", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ "func L()\n  return \"abc\nend" ] } "Output.x = 1")), Some MessageCode.UnterminatedString, "Parse"
      "library-validate", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ "func L()\n  return zzz\nend" ] } "Output.x = 1")), Some MessageCode.UndeclaredVariable, "Validate"
      "library-runtime", (fun () -> boxed (Script.runWith { pinned with Libraries = [ "func L()\n  return 1 / 0\nend" ] } "Output.x = L()")), Some MessageCode.DivisionByZero, "Execute"
      // JYRO-REQ-2139 (JYRO-ADR-112): UndefinedFunction for a call in a source text is Link,
      // as on a load; on both compile members, and in a library.
      "undefined-in-source", (fun () -> boxed (Script.compileWith pinned "Output.a = Nope()")), Some MessageCode.UndefinedFunction, "Link"
      "undefined-in-source-bytes", (fun () -> boxed (Script.compileBytesWith pinned "Output.a = Nope()")), Some MessageCode.UndefinedFunction, "Link"
      "undefined-in-library", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ "func L()\n  return Nope()\nend" ] } "Output.x = 1")), Some MessageCode.UndefinedFunction, "Link"
      // DuplicateFunction (80.3.1) and FunctionOverride (80.3.2) are named in the Validate row,
      // in a script and in a library alike. FunctionOverride does depend on what the host
      // registered: 80.3.2 rejects the name of a 90.2 inventory function and also the name of a
      // host function or further standard library function registered for the compilation, so
      // Validate is given the registered names (JYRO-DEF-513, JYRO-ADR-115 point 2, JYRO-ADR-116).
      "duplicate-function", (fun () -> boxed (Script.compileWith pinned "func f()\n  return 1\nend\nfunc f()\n  return 2\nend")), Some MessageCode.DuplicateFunction, "Validate"
      "duplicate-function-in-a-library", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ lib + "\nfunc L()\n  return 2\nend" ] } "Output.x = 1")), Some MessageCode.DuplicateFunction, "Validate"
      "duplicate-function-across-libraries", (fun () -> boxed (Script.compileWith { pinned with Libraries = [ lib; "func L()\n  return 2\nend" ] } "Output.x = 1")), Some MessageCode.DuplicateFunction, "Validate"
      "function-override", (fun () -> boxed (Script.compileWith pinned "func Length(x)\n  return 0\nend")), Some MessageCode.FunctionOverride, "Validate"
      "function-override-no-stdlib", (fun () -> boxed (Script.compileWith { pinned with Stdlib = false } "func Length(x)\n  return 0\nend")), Some MessageCode.FunctionOverride, "Validate"
      "function-override-host", (fun () -> boxed (Script.compileWith { pinned with Functions = [ (Host.recorder "Hosted").AsFunction ] } "func Hosted()\n  return 1\nend")), Some MessageCode.FunctionOverride, "Validate"
      "function-override-host-bytes", (fun () -> boxed (Script.compileBytesWith { pinned with Functions = [ (Host.recorder "Hosted").AsFunction ] } "func Hosted()\n  return 1\nend")), Some MessageCode.FunctionOverride, "Validate"
      "function-override-host-no-stdlib", (fun () -> boxed (Script.compileWith { pinned with Stdlib = false; Functions = [ (Host.recorder "Hosted").AsFunction ] } "func Hosted()\n  return 1\nend")), Some MessageCode.FunctionOverride, "Validate"
      // A compilation from an artifact makes the same declaration-name checks, with the stages
      // of the table (JYRO-ADR-114 point 1, JYRO-ADR-116): DuplicateFunction and FunctionOverride
      // at Validate, VariantShadowsFunction and FunctionShadowsVariant at Link. The loading
      // host's registered functions are the ones checked, not the writer's.
      "duplicate-function-on-load", (fun () -> boxed (loadWith pinned (build [ declared "f"; declared "f" ]))), Some MessageCode.DuplicateFunction, "Validate"
      "function-override-on-load", (fun () -> boxed (loadWith pinned (build [ declared "Length" ]))), Some MessageCode.FunctionOverride, "Validate"
      "function-override-on-load-no-stdlib", (fun () -> boxed (loadWith { pinned with Stdlib = false } (build [ declared "Length" ]))), Some MessageCode.FunctionOverride, "Validate"
      "function-override-host-on-load", (fun () -> boxed (loadWith (withHost "Hosted") (writtenWithout "func Hosted()\n  return 1\nend\nOutput.x = Hosted()"))), Some MessageCode.FunctionOverride, "Validate"
      "variant-shadows-on-load", (fun () -> boxed (loadWith (withHost "Circle") (writtenWithout "union S Circle(r) end\nOutput.x = 1"))), Some MessageCode.VariantShadowsFunction, "Link"
      "function-shadows-variant-on-load", (fun () -> boxed (loadWith pinned (build variantAndFunction))), Some MessageCode.FunctionShadowsVariant, "Link"
      // A library diagnostic has the stage the same diagnostic has in a script, on each
      // member that takes a library: JyroLibrary.Parse as well as the compiler's libraries.
      "jyrolibrary-parse", (fun () -> boxed (JyroLibrary.Parse "func L()\n  return \"abc\nend")), Some MessageCode.UnterminatedString, "Parse"
      "jyrolibrary-escape", (fun () -> boxed (JyroLibrary.Parse "func L()\n  return \"\\q\"\nend")), Some MessageCode.InvalidEscapeSequence, "Parse"
      "jyrolibrary-number-format", (fun () -> boxed (JyroLibrary.Parse "func L()\n  return 1e400\nend")), Some MessageCode.InvalidNumberFormat, "Parse"
      "jyrolibrary-parser", (fun () -> boxed (JyroLibrary.Parse "func L(\n")), None, "Parse"
      "jyrolibrary-validate", (fun () -> boxed (JyroLibrary.Parse "func L()\n  return zzz\nend")), Some MessageCode.UndeclaredVariable, "Validate" ]

// Every row raises its code with this work order's build alone (JYRO-ADR-113 item 2): the
// DuplicateFunction and FunctionOverride rows by JYRO-REQ-291 and 292, which are in this
// work order. A row whose code only a requirement of another work order raises is tested in
// that requirement's tests, which assert the stage this table gives it: UnpairedSurrogate at
// Parse (JYRO-REQ-38, S20_07_Strings), InvalidNumberFormat of 12abc at Parse (JYRO-REQ-34,
// S20_06_Numbers), a table name two times at Deserialize (JYRO-REQ-2070, S150_03_Layout),
// a byte after the program at Deserialize (JYRO-REQ-2073, S150_11), MatchSubjectNotVariant
// at Execute (JYRO-REQ-1971, S50_08_SwitchMatch) and InvalidPadCharacter at Execute
// (JYRO-REQ-1999 and 511, S91_12_13_Pad).

let stageRowNames : obj[] seq = stageRows () |> Seq.map (fun (n, _, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof stageRowNames)>]
[<Trait("requirement", "JYRO-REQ-1586")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
[<Trait("requirement", "JYRO-REQ-2105")>]
[<Trait("requirement", "JYRO-REQ-1496")>]
let ``JYRO-REQ-2139 EachDiagnostic_HasTheStageTheTableAssigns`` (row: string) =
    let _, make, code, stage = stageRows () |> List.find (fun (n, _, _, _) -> n = row)
    let r = make ()
    match code with
    | Some c ->
        // The code first (FLOORS 4.1.11), then the stages.
        if not (Script.codes r |> List.contains c) then
            Values.fail (sprintf "No diagnostic %A. Diagnostics:\n%s" c (Script.describe r))
        for m in r.Messages do Assert.Contains(Pending.stageOf m, sixStages)
        stageOfCode c stage r
    | None ->
        // No code is given: the result failed with an Error, and every Error has the stage.
        let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        Assert.False(r.IsSuccess, "Expected a failure. " + Script.describe r)
        Assert.NotEmpty(errors)
        for m in errors do Assert.Equal(stage, Pending.stageOf m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 CompilationTimeLimitExceeded_HasTheStageThatWasRunning`` () =
    // A 1-tick limit is reached while the first stage runs: Parse for a source,
    // Deserialize for an artifact (100.9.3, 160.1.12 row 1).
    let r = Script.compileWith { pinned with CompilationTimeout = Some(TimeSpan.FromTicks 1L) } (manyStatements 20_000)
    fails MessageCode.CompilationTimeLimitExceeded r
    stageOfCode MessageCode.CompilationTimeLimitExceeded "Parse" r
    let bytes = Script.compileBytesWith pinned (manyStatements 9_000) |> Script.valueOf
    let r2 = Script.loadBytesWith { pinned with CompilationTimeout = Some(TimeSpan.FromTicks 1L) } bytes
    fails MessageCode.CompilationTimeLimitExceeded r2
    stageOfCode MessageCode.CompilationTimeLimitExceeded "Deserialize" r2

// -- JYRO-REQ-2139 (160.1.12 per JYRO-ADR-112): internal errors and the defects of JYRO-WRK-16 ---------------

let private internalErrors = [ MessageCode.UnknownParserError; MessageCode.UnknownCompilerError; MessageCode.InternalStageFailure ]

/// A host function whose effect cannot be read: the host's own fault, met when a call to
/// it is resolved and its effect checked, which is the work of Link (150.1.5, 120.5.2).
type private EffectFault(name: string) =
    inherit JyroFunctionBase(name, Host.signature name [] ParameterType.Any)
    override _.Effect = raise (InvalidOperationException "the host cannot say the effect")
    override _.Execute(_, _) = Values.nul

/// A host function whose signature cannot be read, met when a call to it is resolved.
type private SignatureFault(name: string) =
    interface IJyroFunction with
        member _.Name = name
        member _.Signature = raise (InvalidOperationException "the host cannot give the signature")
        member _.Effect = FunctionEffect.Pure
        member _.Execute(_, _) = Values.nul

/// The table's second row: an internal error (150.2.5) raised before the script runs has the
/// stage that was running when it was raised. A fault met while a call is resolved is met
/// while Link runs, on the source path (Compile and CompileToArtifact) and on a load; every
/// Error of such a result, and an internal error among them in particular, has the stage
/// Link. Which code the implementation gives the fault is its own (JYRO-REQ-1681, which replaces JYRO-REQ-1672).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
[<Trait("requirement", "JYRO-REQ-2105")>]
let ``JYRO-REQ-2139 an internal error raised while Link runs has the stage Link`` () =
    let withFn (f: IJyroFunction) = { pinned with Functions = [ f ] }
    let src = "Output.x = Faulty()"
    let bytes = Script.compileBytesWith (withFn (Host.recorder "Faulty").AsFunction) src |> Script.valueOf
    let cases : (string * JyroResult<obj>) list =
        let boxed (r: JyroResult<'T>) = if r.IsSuccess then JyroResult<obj>.Success(box (Script.valueOf r), r.Messages) else JyroResult<obj>.Failure(r.Messages)
        [ "effect, Compile", boxed (Script.compileWith (withFn (EffectFault "Faulty")) src)
          "effect, CompileToArtifact", boxed (Script.compileBytesWith (withFn (EffectFault "Faulty")) src)
          "signature, Compile", boxed (Script.compileWith (withFn (SignatureFault "Faulty")) src)
          "signature, CompileToArtifact", boxed (Script.compileBytesWith (withFn (SignatureFault "Faulty")) src)
          "effect, load", boxed (Script.loadBytesWith (withFn (EffectFault "Faulty")) bytes) ]
    let mutable sawInternal = false
    for name, r in cases do
        if not r.IsSuccess then
            let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
            Assert.True(not errors.IsEmpty, sprintf "%s: a failure with no Error. %s" name (Script.describe r))
            for m in errors do
                if List.contains m.Code internalErrors then sawInternal <- true
                Assert.True((Pending.stageOf m = "Link"), sprintf "%s: %A has the stage %s, not Link. %s" name m.Code (Pending.stageOf m) (Script.describe r))
    // At least one of the faults must give an internal error, or this test shows nothing of the row.
    Assert.True(sawInternal, "No case gave an internal error (UnknownCompilerError, InternalStageFailure); the row is not exercised. "
                             + String.Join("; ", cases |> List.map (fun (n, r) -> n + ": " + Script.describe r)))

/// The table's second row for Parse: a parse error that the book gives no code of its own
/// (UnknownParserError, an internal error by 150.2.5) has the stage Parse on each member
/// that parses: the source path, a compiler's library and JyroLibrary.Parse.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 an internal error raised while Parse runs has the stage Parse`` () =
    let text = "func L(\n"
    let results =
        [ "source", (Script.compileWith pinned text).Messages
          "AddLibrarySource", (Script.compileWith { pinned with Libraries = [ text ] } "Output.x = 1").Messages
          "JyroLibrary.Parse", (JyroLibrary.Parse text).Messages ]
    for name, messages in results do
        let errors = messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        Assert.True(not errors.IsEmpty, sprintf "%s: no Error for %A" name text)
        for m in errors do
            Assert.True((Pending.stageOf m = "Parse"), sprintf "%s: %A has the stage %s, not Parse" name m.Code (Pending.stageOf m))

/// JYRO-DEF-500: JyroLibrary.Parse returned the parser's diagnostics without the stage Parse,
/// so one whose code lies outside the parse band took the stage of its code. A library
/// diagnostic has the stage the same diagnostic has in a script (160.1.12, JYRO-ADR-111):
/// for each text the parser rejects, every Error of JyroLibrary.Parse and of a compiler's
/// library has the stage Parse, as the same text has as a script. The text with a lone
/// surrogate is the case that shows the fault once the parser reports UnpairedSurrogate
/// (5918, JYRO-DEF-103); the stage is read whatever code the parser gives.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
[<Trait("requirement", "JYRO-REQ-2105")>]
let ``JYRO-REQ-2139 JYRO-DEF-500 a diagnostic made while JyroLibrary.Parse parses a library has the stage Parse whatever its code`` () =
    let texts =
        [ "func L()\n  return \"\\uD800\"\nend"
          "func L()\n  return \"\\uDFFF\"\nend"
          "func L()\n  return \"" + string (char 0xD800) + "\"\nend"
          "func L()\n  return \"abc\nend"
          "func L()\n  return 1e400\nend"
          "func L()\n  return 0xFFFFFFFFFFFFFFFFFF\nend"
          "func L(\n" ]
    for text in texts do
        let asScript = (Script.compileWith pinned text).Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        Assert.True(not asScript.IsEmpty, sprintf "The text %A is not rejected as a script" text)
        for m in asScript do Assert.Equal("Parse", Pending.stageOf m)
        for name, messages in [ "JyroLibrary.Parse", (JyroLibrary.Parse text).Messages
                                "AddLibrarySource", (Script.compileWith { pinned with Libraries = [ text ] } "Output.x = 1").Messages ] do
            let errors = messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
            Assert.True(not errors.IsEmpty, sprintf "%s: the text %A is not rejected" name text)
            for m in errors do
                Assert.True((Pending.stageOf m = "Parse"), sprintf "%s: %A for %A has the stage %s, not Parse" name m.Code text (Pending.stageOf m))
            Assert.Equal<MessageCode list>(asScript |> List.map (fun m -> m.Code), errors |> List.map (fun m -> m.Code))

/// JYRO-DEF-501: DuplicateFunction and FunctionOverride of declarations had the stage Link,
/// from the number band of their codes; the table names them in the Validate row
/// (160.1.12, JYRO-ADR-112), in a script and in a library alike.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
[<Trait("requirement", "JYRO-REQ-2105")>]
let ``JYRO-REQ-2139 JYRO-DEF-501 DuplicateFunction and FunctionOverride of declarations have the stage Validate`` () =
    let lib = "func L()\n  return 1\nend"
    stageOfCode MessageCode.FunctionOverride "Validate" (Script.compileWith pinned "func Length(x)\n  return 0\nend")
    stageOfCode MessageCode.FunctionOverride "Validate" (Script.compileBytesWith pinned "func Length(x)\n  return 0\nend")
    stageOfCode MessageCode.FunctionOverride "Validate"
        (Script.compileWith { pinned with Functions = [ (Host.recorder "Hosted").AsFunction ] } "func Hosted()\n  return 1\nend")
    stageOfCode MessageCode.DuplicateFunction "Validate"
        (Script.compileWith { pinned with Libraries = [ lib; "func L()\n  return 2\nend" ] } "Output.x = 1")
    // On CompileToArtifact, a script's own two declarations (80.3.1): CompileToArtifact refuses
    // any library with ArtifactCannotEncodeLibraries (the row "encode-libraries"), so a library case there
    // would show the order of two refusals, which no clause gives.
    stageOfCode MessageCode.DuplicateFunction "Validate"
        (Script.compileBytesWith pinned "func f()\n  return 1\nend\nfunc f()\n  return 2\nend")
    stageOfCode MessageCode.DuplicateFunction "Validate"
        (Script.compileWith pinned "func f()\n  return 1\nend\nfunc f()\n  return 2\nend")

/// JYRO-DEF-504: 160.1.12 placed a call in a source text to a function neither declared nor
/// registered at no stage. JYRO-REQ-2139 names UndefinedFunction in the Link row on source as
/// on a load, and gives its three examples: `Output.a = Nope()` is UndefinedFunction at Link,
/// a script that declares `func f()` twice is DuplicateFunction at Validate (the code is
/// JYRO-REQ-291's, which JYRO-SET-13 moved into this work order for this example), and
/// `func Length(x)` is FunctionOverride at Validate.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 JYRO-DEF-504 the three examples of 160.1.12 have the stages the table names`` () =
    stageOfCode MessageCode.UndefinedFunction "Link" (Script.compileWith pinned "Output.a = Nope()")
    stageOfCode MessageCode.UndefinedFunction "Link" (Script.compileBytesWith pinned "Output.a = Nope()")
    stageOfCode MessageCode.UndefinedFunction "Link" (Script.compileWith { pinned with Stdlib = false } "Output.a = Length([1])")
    stageOfCode MessageCode.FunctionOverride "Validate" (Script.compileWith pinned "func Length(x)\n  return 0\nend")
    let twice = Script.compileWith pinned "func f()\n  return 1\nend\nfunc f()\n  return 2\nend"
    Assert.False(twice.IsSuccess, Script.describe twice)
    stageOfCode MessageCode.DuplicateFunction "Validate" twice

/// Assert `r` failed and holds an Error of each code of `codes`, every one with the stage `stage`.
let private allReported (what: string) (codes: MessageCode list) (stage: string) (r: JyroResult<'T>) =
    Assert.False(r.IsSuccess, sprintf "%s: compiled. %s" what (Script.describe r))
    for c in codes do
        if not (List.contains c (Script.errorCodes r)) then
            Values.fail (sprintf "%s: no %A beside the other diagnostics (JYRO-ADR-115 point 1). Diagnostics:\n%s" what c (Script.describe r))
        stageOfCode c stage r

/// JYRO-DEF-507: a load made no FunctionOverride check, so a program declaring a function
/// named after a 90.2 inventory function, or after a function the loading host registered,
/// was accepted. A compilation from an artifact makes every declaration-name check that a
/// compilation from source makes (160.1.1, 80.3.1, 80.3.2, JYRO-ADR-114 point 1), with the
/// stage Validate (160.1.12, JYRO-ADR-116). The registered names are the loading host's: an
/// artifact written by a host without the function `Hosted` is refused by a host with it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-292")>]
[<Trait("requirement", "JYRO-REQ-291")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 JYRO-DEF-507 a load refuses a function named after an inventory or registered function, and a second declaration, at Validate`` () =
    // An inventory name, with and without the standard library on the loading host.
    stageOfCode MessageCode.FunctionOverride "Validate" (loadWith pinned (build [ declared "Length" ]))
    stageOfCode MessageCode.FunctionOverride "Validate" (loadWith { pinned with Stdlib = false } (build [ declared "Length" ]))
    stageOfCode MessageCode.FunctionOverride "Validate" (loadWith { pinned with Stdlib = false } (build [ declared "ToUpper" ]))
    // A registered host-function name: the writer had no host function `Hosted`, so it wrote the
    // artifact; the loading host registers `Hosted`, and refuses it. Also on the run path.
    let bytes = writtenWithout "func Hosted()\n  return 1\nend\nOutput.x = Hosted()"
    Assert.True((Script.runBytesWith pinned bytes).IsSuccess, "the writer's own host must load and run its artifact")
    stageOfCode MessageCode.FunctionOverride "Validate" (loadWith (withHost "Hosted") bytes)
    stageOfCode MessageCode.FunctionOverride "Validate" (loadWith { withHost "Hosted" with Stdlib = false } bytes)
    let ran = Script.runBytesWith (withHost "Hosted") bytes
    Assert.False(ran.IsSuccess, "the loading host registers Hosted, so the program must not run. " + Script.describe ran)
    stageOfCode MessageCode.FunctionOverride "Validate" ran
    // A second declaration of one name.
    stageOfCode MessageCode.DuplicateFunction "Validate" (loadWith pinned (build [ declared "f"; declared "f" ]))

/// JYRO-DEF-508: a Validate-row code raised by a pass that runs only after a clean validation
/// was lost whenever validation reported anything. A stage is complete before compilation
/// stops (JYRO-ADR-115 point 1), and validation reports every error (130.4, JYRO-REQ-1485): for
/// each Validate-row code of this work order, a case with a second Validate diagnostic of
/// another rule reports both, each with the stage Validate, on source and on load.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<Trait("requirement", "JYRO-REQ-291")>]
[<Trait("requirement", "JYRO-REQ-292")>]
let ``JYRO-REQ-2139 JYRO-DEF-508 DuplicateFunction and FunctionOverride are reported beside another Validate diagnostic`` () =
    let twice = "func f()\n  return 1\nend\nfunc f()\n  return 2\nend\n"
    let length = "func Length(x)\n  return 0\nend\n"
    let hosted = "func Hosted()\n  return 1\nend\n"
    let undeclared = "Output.x = zzz"
    let typed = "var s: string\n"
    let both = [ MessageCode.DuplicateFunction; MessageCode.FunctionOverride ]
    // DuplicateFunction with UndeclaredVariable, and with TypedVariableNeedsInitializer.
    allReported "DuplicateFunction + UndeclaredVariable" [ MessageCode.DuplicateFunction; MessageCode.UndeclaredVariable ] "Validate"
        (Script.compileWith pinned (twice + undeclared))
    allReported "DuplicateFunction + UndeclaredVariable, CompileToArtifact" [ MessageCode.DuplicateFunction; MessageCode.UndeclaredVariable ] "Validate"
        (Script.compileBytesWith pinned (twice + undeclared))
    allReported "DuplicateFunction + TypedVariableNeedsInitializer" [ MessageCode.DuplicateFunction; MessageCode.TypedVariableNeedsInitializer ] "Validate"
        (Script.compileWith pinned (typed + twice))
    // FunctionOverride of an inventory name, with and without the standard library.
    for o in [ pinned; { pinned with Stdlib = false } ] do
        allReported "FunctionOverride (inventory) + UndeclaredVariable" [ MessageCode.FunctionOverride; MessageCode.UndeclaredVariable ] "Validate"
            (Script.compileWith o (length + undeclared))
    allReported "FunctionOverride (inventory) + UndeclaredVariable, CompileToArtifact" [ MessageCode.FunctionOverride; MessageCode.UndeclaredVariable ] "Validate"
        (Script.compileBytesWith pinned (length + undeclared))
    // FunctionOverride of a registered host-function name.
    allReported "FunctionOverride (host) + UndeclaredVariable" [ MessageCode.FunctionOverride; MessageCode.UndeclaredVariable ] "Validate"
        (Script.compileWith (withHost "Hosted") (hosted + undeclared))
    allReported "FunctionOverride (host) + TypedVariableNeedsInitializer" [ MessageCode.FunctionOverride; MessageCode.TypedVariableNeedsInitializer ] "Validate"
        (Script.compileWith (withHost "Hosted") (typed + hosted))
    // The two codes of this work order together: `func Length` declared twice.
    allReported "func Length twice" both "Validate" (Script.compileWith pinned (length + length))
    allReported "func Hosted twice" both "Validate" (Script.compileWith (withHost "Hosted") (hosted + hosted))
    // On load: the same pairs in a hand-built artifact.
    let zzz = setOut "x" (ident "zzz")
    allReported "load: DuplicateFunction + UndeclaredVariable" [ MessageCode.DuplicateFunction; MessageCode.UndeclaredVariable ] "Validate"
        (loadWith pinned (build [ declared "f"; declared "f"; zzz ]))
    allReported "load: FunctionOverride (inventory) + UndeclaredVariable" [ MessageCode.FunctionOverride; MessageCode.UndeclaredVariable ] "Validate"
        (loadWith pinned (build [ declared "Length"; zzz ]))
    allReported "load: FunctionOverride (host) + UndeclaredVariable" [ MessageCode.FunctionOverride; MessageCode.UndeclaredVariable ] "Validate"
        (loadWith (withHost "Hosted") (build [ declared "Hosted"; zzz ]))
    allReported "load: func Length twice" both "Validate" (loadWith pinned (build [ declared "Length"; declared "Length" ]))

/// JYRO-DEF-509: a load made neither variant check of 50.10.9. A compilation from an artifact
/// makes them (JYRO-ADR-114 point 1), in Link, the stage 160.1.12 gives both codes
/// (JYRO-ADR-116 point 3). The host functions checked are the loading host's: an artifact
/// with a variant `Circle`, written by a host without a function `Circle`, is refused by a
/// host that registers one.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 JYRO-DEF-509 a load refuses a variant named after a registered function, and a function named after a variant, at Link`` () =
    let bytes = writtenWithout "union S Circle(r) end\nOutput.x = 1"
    Assert.True((Script.runBytesWith pinned bytes).IsSuccess, "the writer's own host must load and run its artifact")
    stageOfCode MessageCode.VariantShadowsFunction "Link" (loadWith (withHost "Circle") bytes)
    stageOfCode MessageCode.VariantShadowsFunction "Link" (loadWith { withHost "Circle" with Stdlib = false } bytes)
    let ran = Script.runBytesWith (withHost "Circle") bytes
    Assert.False(ran.IsSuccess, "the loading host registers Circle, so the program must not run. " + Script.describe ran)
    stageOfCode MessageCode.VariantShadowsFunction "Link" ran
    stageOfCode MessageCode.FunctionShadowsVariant "Link" (loadWith pinned (build variantAndFunction))
    stageOfCode MessageCode.FunctionShadowsVariant "Link" (loadWith pinned (build (List.rev variantAndFunction)))

// -- JYRO-REQ-2139 (160.1.12): the stage of what a host function's call check gives ----------------

/// A host function that refuses every run by throwing JyroError.Host (JYRO-REQ-1891).
type private ErrorRefuser() =
    inherit JyroFunctionBase("Refuse", Host.signature "Refuse" [] ParameterType.Any)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(_, _) = raise (JyroError.Host("Refused"))

/// A `Guard()` whose call check throws JyroError.Runtime(code).
let private raisingGuard (code: MessageCode) =
    Tests.CallCheck.checking "Guard" (fun _ _ -> raise (JyroError.Runtime(code)) |> ignore)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 HostLinkError has the stage Link, in a source, in a library script and on a load`` () =
    let hostLink = Tests.CallCheck.hostLinkError
    // Query (Tests.CallCheck) refuses a string literal that is not one of its tables.
    let withQ () = { pinned with Functions = [ (Tests.CallCheck.query ()).AsFunction ] }
    stageOfCode hostLink "Link" (Script.compileWith (withQ ()) "Output.r = Query(\"custmers\")")
    stageOfCode hostLink "Link" (Script.compileBytesWith (withQ ()) "Output.r = Query(\"custmers\")")
    stageOfCode hostLink "Link" (Script.compileWith { withQ () with Libraries = [ "func L()\n  return Query(\"custmers\")\nend" ] } "Output.r = 1")
    // An artifact written by a host whose Query has no check, loaded by one whose Query has it.
    let unchecked = Host.func "Query" Tests.CallCheck.queryParameters ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)
    let bytes = Script.compileBytesWith { pinned with Functions = [ unchecked.AsFunction ] } "Output.r = Query(\"custmers\")" |> Script.valueOf
    stageOfCode hostLink "Link" (loadWith (withQ ()) bytes)
    stageOfCode hostLink "Link" (Script.runBytesWith (withQ ()) bytes)

/// Codes whose own rows of the table give another stage (Execute), and the clause's InvalidJson:
/// thrown through JyroError.Runtime in a call check, each has the stage Link (the table's third row).
/// Only codes that JyroError.Runtime makes an exception for are used: it refuses ScriptFailure, the
/// limits and every code below 5000 (JYRO-REQ-1892), which are the codes of the other rows.
let raisedInACheck : obj[] seq =
    [ "InvalidJson"; "DivisionByZero"; "ModuloByZero"; "ArgumentTypeMismatch"; "IndexOutOfRange" ] |> Seq.map (fun c -> [| box c |])

[<Theory>]
[<MemberData(nameof raisedInACheck)>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 a diagnostic a call check throws through JyroError.Runtime has the stage Link, whatever its code`` (codeName: string) =
    let code = Enum.Parse(typeof<MessageCode>, codeName) :?> MessageCode
    let r = Script.compileWith { pinned with Functions = [ (raisingGuard code).AsFunction ] } "Output.g = Guard()"
    Assert.False(r.IsSuccess, Script.describe r)
    stageOfCode code "Link" r
    stageOfCode code "Link" (Script.compileBytesWith { pinned with Functions = [ (raisingGuard code).AsFunction ] } "Output.g = Guard()")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 the clause's example, a call check that raises InvalidJson, gives InvalidJson at Link, not Execute`` () =
    let f = raisingGuard MessageCode.InvalidJson
    let r = Script.runWith { pinned with Functions = [ f.AsFunction ] } "Output.g = Guard()"
    Assert.False(r.IsSuccess, Script.describe r)
    stageOfCode MessageCode.InvalidJson "Link" r
    Assert.Equal(0, f.RunCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 a call check that throws InvalidOperationException gives UnknownCompilerError at Link, the stage that was running`` () =
    let make () = Tests.CallCheck.checking "Guard" (fun _ _ -> raise (InvalidOperationException "the host's check failed"))
    stageOfCode Tests.CallCheck.unknownCompilerError "Link" (Script.compileWith { pinned with Functions = [ (make ()).AsFunction ] } "Output.g = Guard()")
    stageOfCode Tests.CallCheck.unknownCompilerError "Link" (Script.compileBytesWith { pinned with Functions = [ (make ()).AsFunction ] } "Output.g = Guard()")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 HostFunctionError, thrown by JyroError.Host during a run, has the stage Execute`` () =
    let byHost = Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host("Refused", [| box 1 |])))
    stageOfCode MessageCode.HostFunctionError "Execute" (Script.runWith { pinned with Functions = [ byHost.AsFunction ] } "Output.a = Refuse()")
    stageOfCode MessageCode.HostFunctionError "Execute" (Script.runWith { pinned with Functions = [ ErrorRefuser() :> IJyroFunction ] } "Output.a = Refuse()")

// -- JYRO-WRK-23: every diagnostic of a failed stage, from every source text (JYRO-ADR-115, 120) --

/// Assert `r` failed, and that every Error it holds has the stage `stage`: a compilation stops
/// after the first stage that reports an Error, and reports no diagnostic of a later stage
/// (JYRO-ADR-115 point 1; JYRO-ADR-120 rejects reporting two stages).
let private stoppedAt (what: string) (stage: string) (r: JyroResult<'T>) =
    Assert.False(r.IsSuccess, sprintf "%s: compiled. %s" what (Script.describe r))
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    for m in errors do
        if Pending.stageOf m <> stage then
            Values.fail (sprintf "%s: %A has the stage %s; the compilation should have stopped after %s. Diagnostics:\n%s"
                             what m.Code (Pending.stageOf m) stage (Script.describe r))

/// Assert `r` holds an Error of the stage Parse whose code is not `except` (the script's).
let private parseErrorsBesides (what: string) (except: MessageCode option) (r: JyroResult<'T>) =
    let ps =
        r.Messages
        |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error && Pending.stageOf m = "Parse" && Some m.Code <> except)
        |> List.ofSeq
    if ps.IsEmpty then
        Values.fail (sprintf "%s: the script's parse error is not reported. Diagnostics:\n%s" what (Script.describe r))

/// The host options `pinned` with the libraries `libs` given as source.
let private withLibs (libs: string list) : RunOptions = { pinned with Libraries = libs }

/// The host options `o` with two host functions both named `name` (80.3.1: DuplicateFunction,
/// with Validate per JYRO-ADR-120).
let private twoHosts (name: string) (o: RunOptions) : RunOptions =
    { o with Functions = [ (Host.recorder name).AsFunction; (Host.recorder name).AsFunction ] }

/// JYRO-DEF-510: FunctionShadowsLibrary, a library's FunctionOverride and DuplicateFunction, and
/// a variant repeated across libraries are Validate diagnostics (160.1.12, JYRO-REQ-2139) checked
/// across source texts; they were raised only by the linker, which runs after a clean
/// validation, so they were lost whenever validation reported anything. Validate is complete
/// before the compilation stops (JYRO-ADR-115 points 1 and 2), and validation reports every error
/// (130.4, JYRO-REQ-1485): each arrangement, beside a second Validate diagnostic of another rule
/// (in the script, or in the library), reports both, every Error with the stage Validate.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-293")>]
[<Trait("requirement", "JYRO-REQ-1610")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
[<Trait("requirement", "JYRO-REQ-1485")>]
let ``JYRO-REQ-2139 JYRO-DEF-510 the cross-source declaration checks are reported beside another Validate diagnostic`` () =
    let h = "func H()\n  return 1\nend"
    let scriptH = "func H()\n  return 0\nend\n"
    let typed = "var s: string\n"
    let undeclared = "Output.x = zzz"
    let check what codes r =
        allReported what codes "Validate" r
        stoppedAt what "Validate" r
    // FunctionShadowsLibrary: the script declares a function a library declares.
    check "FunctionShadowsLibrary + TypedVariableNeedsInitializer" [ MessageCode.FunctionShadowsLibrary; MessageCode.TypedVariableNeedsInitializer ]
        (Script.compileWith (withLibs [ h ]) (typed + scriptH))
    check "FunctionShadowsLibrary + UndeclaredVariable" [ MessageCode.FunctionShadowsLibrary; MessageCode.UndeclaredVariable ]
        (Script.compileWith (withLibs [ h ]) (scriptH + undeclared))
    check "FunctionShadowsLibrary + UndeclaredVariable in the library" [ MessageCode.FunctionShadowsLibrary; MessageCode.UndeclaredVariable ]
        (Script.compileWith (withLibs [ h + "\nfunc K()\n  return zzz\nend" ]) scriptH)
    // The same with the library given as a JyroLibrary (160.6), not as source.
    let parsed = JyroLibrary.Parse h |> Script.valueOf
    check "FunctionShadowsLibrary (JyroLibrary) + TypedVariableNeedsInitializer" [ MessageCode.FunctionShadowsLibrary; MessageCode.TypedVariableNeedsInitializer ]
        ((Script.compilerFor pinned).AddLibrary(parsed).WithSource(typed + scriptH).Compile())
    // A library's FunctionOverride: an inventory name, and a registered host-function name.
    check "library FunctionOverride (inventory) + TypedVariableNeedsInitializer" [ MessageCode.FunctionOverride; MessageCode.TypedVariableNeedsInitializer ]
        (Script.compileWith (withLibs [ "func Length(x)\n  return 0\nend" ]) typed)
    check "library FunctionOverride (inventory) + UndeclaredVariable in the library" [ MessageCode.FunctionOverride; MessageCode.UndeclaredVariable ]
        (Script.compileWith (withLibs [ "func Length(x)\n  return zzz\nend" ]) "Output.x = 1")
    check "library FunctionOverride (host) + TypedVariableNeedsInitializer" [ MessageCode.FunctionOverride; MessageCode.TypedVariableNeedsInitializer ]
        (Script.compileWith { withHost "Hosted" with Libraries = [ "func Hosted()\n  return 1\nend" ] } typed)
    // A library's DuplicateFunction: within one library, and across two.
    check "library DuplicateFunction (one library) + TypedVariableNeedsInitializer" [ MessageCode.DuplicateFunction; MessageCode.TypedVariableNeedsInitializer ]
        (Script.compileWith (withLibs [ "func L()\n  return 1\nend\nfunc L()\n  return 2\nend" ]) typed)
    check "library DuplicateFunction (two libraries) + TypedVariableNeedsInitializer" [ MessageCode.DuplicateFunction; MessageCode.TypedVariableNeedsInitializer ]
        (Script.compileWith (withLibs [ "func L()\n  return 1\nend"; "func L()\n  return 2\nend" ]) typed)
    check "library DuplicateFunction (two libraries) + UndeclaredVariable" [ MessageCode.DuplicateFunction; MessageCode.UndeclaredVariable ]
        (Script.compileWith (withLibs [ "func L()\n  return 1\nend"; "func L()\n  return 2\nend" ]) undeclared)
    // A variant name repeated across two libraries. Which code it takes is JYRO-DEF-525's
    // (JYRO-ADR-121, outside this work order): DuplicateFunction today, DuplicateVariantName if
    // ADR-121 is taken. Either is a Validate code, and either must be reported beside the other.
    let r = Script.compileWith (withLibs [ "union U A end\nfunc F()\n  return 1\nend"; "union V A end\nfunc G()\n  return 1\nend" ]) typed
    allReported "variant repeated across libraries + TypedVariableNeedsInitializer" [ MessageCode.TypedVariableNeedsInitializer ] "Validate" r
    stoppedAt "variant repeated across libraries + TypedVariableNeedsInitializer" "Validate" r
    let clash = Script.errorCodes r |> List.filter (fun c -> c = MessageCode.DuplicateFunction || c = MessageCode.DuplicateVariantName)
    if clash.IsEmpty then
        Values.fail (sprintf "variant repeated across libraries: the clash is not reported beside TypedVariableNeedsInitializer. Diagnostics:\n%s" (Script.describe r))

/// JYRO-DEF-511: a library with a statement other than func or union gave
/// LibraryStatementForbidden only, and the declarations it kept were not validated. A check
/// within a stage does not return on its first failure (JYRO-ADR-115 point 4): both are
/// Validate diagnostics (160.1.12), and both are reported (130.4, JYRO-REQ-1485, JYRO-REQ-1609),
/// for a library added as source and for JyroLibrary.Parse.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1609")>]
[<Trait("requirement", "JYRO-REQ-1485")>]
let ``JYRO-DEF-511 LibraryStatementForbidden is reported beside the other validation errors of the library`` () =
    let cases =
        [ "Output.a = 1\nfunc H()\n  var s: string\n  return 1\nend", MessageCode.TypedVariableNeedsInitializer
          "var x = 1\nfunc H()\n  return zzz\nend", MessageCode.UndeclaredVariable
          "func H()\n  return zzz\nend\nvar x = 1", MessageCode.UndeclaredVariable
          "func H()\n  break\n  return 1\nend\nOutput.b = 2", MessageCode.LoopStatementOutsideOfLoop ]
    for lib, other in cases do
        let what = sprintf "%A" lib
        let codes = [ MessageCode.LibraryStatementForbidden; other ]
        let viaCompiler = Script.compileWith (withLibs [ lib ]) "Output.x = 1"
        allReported (what + " (AddLibrarySource)") codes "Validate" viaCompiler
        stoppedAt (what + " (AddLibrarySource)") "Validate" viaCompiler
        let viaParse = JyroLibrary.Parse lib
        allReported (what + " (JyroLibrary.Parse)") codes "Validate" viaParse
        stoppedAt (what + " (JyroLibrary.Parse)") "Validate" viaParse

/// JYRO-DEF-512: any diagnostic of a library stopped Compile() before the script was parsed.
/// Each source text goes through each stage before any goes to the next, and a library's
/// diagnostic does not stop the script's own Parse or Validate (JYRO-ADR-115 point 3); the
/// compilation then stops after the first stage that reports an Error (point 1). Every parse
/// error reaches the host (JYRO-REQ-1483), every validation error should (JYRO-REQ-1485), and
/// one result holds the libraries' and the script's diagnostics, the libraries' first
/// (JYRO-REQ-1489). Registration diagnostics are reported with their own stage, after the
/// earlier stages of every source text have run clean (JYRO-ADR-115 point 5, JYRO-ADR-120).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1483")>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<Trait("requirement", "JYRO-REQ-1489")>]
let ``JYRO-DEF-512 a library's diagnostic does not stop the script's Parse or Validate`` () =
    let forbidden = "Output.a = 1\nfunc L()\n  return 1\nend"
    let unterminated = "func L()\n  return \"abc\nend"
    let parseError = "var = 1"
    // A library's Validate error and a script's parse error: Parse fails, so the compilation
    // stops after Parse with the script's parse error.
    let r1 = Script.compileWith (withLibs [ forbidden ]) parseError
    parseErrorsBesides "library LibraryStatementForbidden + script parse error" None r1
    stoppedAt "library LibraryStatementForbidden + script parse error" "Parse" r1
    // A library's parse error and a script's parse error: both, the library's first.
    let r2 = Script.compileWith (withLibs [ unterminated ]) parseError
    allReported "library parse error + script parse error" [ MessageCode.UnterminatedString ] "Parse" r2
    parseErrorsBesides "library parse error + script parse error" (Some MessageCode.UnterminatedString) r2
    stoppedAt "library parse error + script parse error" "Parse" r2
    let ms = List.ofSeq r2.Messages
    let lastLib = ms |> List.findIndexBack (fun m -> m.Code = MessageCode.UnterminatedString)
    let firstScript = ms |> List.findIndex (fun m -> Pending.stageOf m = "Parse" && m.Code <> MessageCode.UnterminatedString)
    Assert.True(lastLib < firstScript, "The script's parse error precedes the library's (JYRO-REQ-1489). " + Script.describe r2)
    // A library's parse error and a script's Validate error: the compilation stops after Parse.
    let r3 = Script.compileWith (withLibs [ unterminated ]) "Output.x = zzz"
    allReported "library parse error + script UndeclaredVariable" [ MessageCode.UnterminatedString ] "Parse" r3
    stoppedAt "library parse error + script UndeclaredVariable" "Parse" r3
    // A library's Validate error and a script's Validate error: both, the library's first.
    let r4 = Script.compileWith (withLibs [ forbidden ]) "Output.x = zzz"
    allReported "library LibraryStatementForbidden + script UndeclaredVariable" [ MessageCode.LibraryStatementForbidden; MessageCode.UndeclaredVariable ] "Validate" r4
    stoppedAt "library LibraryStatementForbidden + script UndeclaredVariable" "Validate" r4
    let ms4 = List.ofSeq r4.Messages
    Assert.True(
        (ms4 |> List.findIndexBack (fun m -> m.Code = MessageCode.LibraryStatementForbidden)) < (ms4 |> List.findIndex (fun m -> m.Code = MessageCode.UndeclaredVariable)),
        "The script's diagnostic precedes the library's (JYRO-REQ-1489). " + Script.describe r4)
    // A library's own validation error (not LibraryStatementForbidden) beside the script's.
    let r5 = Script.compileWith (withLibs [ "func L()\n  return q1\nend" ]) "Output.x = zzz"
    let undeclared = r5.Messages |> Seq.filter (fun m -> m.Code = MessageCode.UndeclaredVariable) |> Seq.length
    Assert.True((undeclared = 2), sprintf "expected the library's and the script's UndeclaredVariable. %s" (Script.describe r5))
    stoppedAt "library UndeclaredVariable + script UndeclaredVariable" "Validate" r5
    // HostFunctionOverridesStdlib (Link) does not hide the script's Parse or Validate errors.
    let lengthHost = withHost "Length"
    let r6 = Script.compileWith lengthHost parseError
    parseErrorsBesides "HostFunctionOverridesStdlib + script parse error" None r6
    stoppedAt "HostFunctionOverridesStdlib + script parse error" "Parse" r6
    let r7 = Script.compileWith lengthHost "Output.x = zzz"
    allReported "HostFunctionOverridesStdlib + script UndeclaredVariable" [ MessageCode.UndeclaredVariable ] "Validate" r7
    stoppedAt "HostFunctionOverridesStdlib + script UndeclaredVariable" "Validate" r7
    let r8 = Script.compileWith lengthHost "Output.x = 1"
    allReported "HostFunctionOverridesStdlib alone" [ MessageCode.HostFunctionOverridesStdlib ] "Link" r8
    // Two host functions of one name: DuplicateFunction with Validate (JYRO-ADR-120), so after a
    // script's parse error, and beside a script's Validate error.
    let r9 = Script.compileWith (twoHosts "Twin" pinned) parseError
    parseErrorsBesides "two host functions + script parse error" None r9
    stoppedAt "two host functions + script parse error" "Parse" r9
    let r10 = Script.compileWith (twoHosts "Twin" pinned) "Output.x = zzz"
    allReported "two host functions + script UndeclaredVariable" [ MessageCode.DuplicateFunction; MessageCode.UndeclaredVariable ] "Validate" r10
    stoppedAt "two host functions + script UndeclaredVariable" "Validate" r10

/// JYRO-DEF-524: CompileToArtifact with a library reported ArtifactCannotEncodeLibraries (stage
/// Compile) before the script was parsed, and a load reported host-function collisions before
/// Deserialize. Every diagnostic JyroCompiler raises from its configuration is reported with the
/// stage StageOf gives it, once every earlier stage has run clean on every source text
/// (JYRO-ADR-120).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1483")>]
[<Trait("requirement", "JYRO-REQ-2093")>]
let ``JYRO-DEF-524 a configuration refusal of a later stage does not hide an earlier stage's diagnostics`` () =
    let lib = "func F()\n  return 1\nend"
    let bytesWithLib = Script.compileBytesWith (withLibs [ lib ])
    // ArtifactCannotEncodeLibraries comes after Parse, Validate and Link of every source text.
    let b1 = bytesWithLib "var = 1"
    parseErrorsBesides "CompileToArtifact, library + script parse error" None b1
    stoppedAt "CompileToArtifact, library + script parse error" "Parse" b1
    let b2 = bytesWithLib "Output.x = zzz"
    allReported "CompileToArtifact, library + script UndeclaredVariable" [ MessageCode.UndeclaredVariable ] "Validate" b2
    stoppedAt "CompileToArtifact, library + script UndeclaredVariable" "Validate" b2
    let b3 = bytesWithLib "Output.x = Nope()"
    allReported "CompileToArtifact, library + script UndefinedFunction" [ MessageCode.UndefinedFunction ] "Link" b3
    stoppedAt "CompileToArtifact, library + script UndefinedFunction" "Link" b3
    let b4 = Script.compileBytesWith (withLibs [ "func F()\n  return \"abc\nend" ]) "Output.x = 1"
    allReported "CompileToArtifact, library parse error" [ MessageCode.UnterminatedString ] "Parse" b4
    stoppedAt "CompileToArtifact, library parse error" "Parse" b4
    let b5 = Script.compileBytesWith (withLibs [ lib ]) "Output.x = F()"
    allReported "CompileToArtifact, library + clean script" [ MessageCode.ArtifactCannotEncodeLibraries ] "Compile" b5
    // A cross-source Validate code beside a Validate error, on CompileToArtifact with a library.
    let b6 = Script.compileBytesWith (withLibs [ lib ]) "var s: string\nfunc F()\n  return 0\nend"
    allReported "CompileToArtifact, FunctionShadowsLibrary + TypedVariableNeedsInitializer"
        [ MessageCode.FunctionShadowsLibrary; MessageCode.TypedVariableNeedsInitializer ] "Validate" b6
    stoppedAt "CompileToArtifact, FunctionShadowsLibrary + TypedVariableNeedsInitializer" "Validate" b6
    // On a load, Deserialize is the first stage (150.1.5): undecodable bytes are refused there
    // whatever host functions collide.
    let badMagic = [| 1uy; 2uy; 3uy |]
    let l1 = loadWith (twoHosts "Twin" pinned) badMagic
    allReported "load bad magic, two host functions" [ MessageCode.ArtifactInvalidMagic ] "Deserialize" l1
    stoppedAt "load bad magic, two host functions" "Deserialize" l1
    let l2 = loadWith (withHost "Length") badMagic
    allReported "load bad magic, HostFunctionOverridesStdlib" [ MessageCode.ArtifactInvalidMagic ] "Deserialize" l2
    stoppedAt "load bad magic, HostFunctionOverridesStdlib" "Deserialize" l2
    // After Deserialize: two host functions (Validate) beside the program's Validate error, and
    // HostFunctionOverridesStdlib (Link) not before it.
    let zzz = build [ setOut "x" (ident "zzz") ]
    let l3 = loadWith (twoHosts "Twin" pinned) zzz
    allReported "load UndeclaredVariable, two host functions" [ MessageCode.DuplicateFunction; MessageCode.UndeclaredVariable ] "Validate" l3
    stoppedAt "load UndeclaredVariable, two host functions" "Validate" l3
    let l4 = loadWith (withHost "Length") zzz
    allReported "load UndeclaredVariable, HostFunctionOverridesStdlib" [ MessageCode.UndeclaredVariable ] "Validate" l4
    stoppedAt "load UndeclaredVariable, HostFunctionOverridesStdlib" "Validate" l4
    let l5 = loadWith (twoHosts "Twin" pinned) (writtenWithout "Output.x = 1")
    allReported "load clean, two host functions" [ MessageCode.DuplicateFunction ] "Validate" l5
    // (The former last row, a load with a library reporting ArtifactCannotUseLibraries first, is
    // dropped: JyroArtifactLoader has no member that adds a library script, JYRO-REQ-1896.)

// -- JYRO-WRK-23 at Verify: order and repeats on every path (130.5, JYRO-ADR-126, JYRO-ADR-127) --

/// The diagnostics of `r`, one line each with the stage, the place and the arguments.
let private placed (r: JyroResult<'T>) : string =
    r.Messages
    |> Seq.map (fun m ->
        let at = if m.Location.HasValue then sprintf "%d:%d+%d" m.Location.Value.Line m.Location.Value.Column m.Location.Value.Length else "-"
        sprintf "%A %s %s (%s)" m.Code (Pending.stageOf m) at (String.Join(", ", m.Args |> Array.map (fun a -> if isNull a then "<null>" else string a))))
    |> String.concat "\n"

/// What 130.5.2 compares: the code, the position and the arguments.
let private sameness (m: DiagnosticMessage) =
    let at = if m.Location.HasValue then Some(m.Location.Value.Line, m.Location.Value.Column, m.Location.Value.Length) else None
    m.Code, at, (m.Args |> Array.map (fun a -> if isNull a then "<null>" else string a) |> List.ofArray)

/// Assert `r` failed, holds an Error of `code`, and has no two diagnostics with the same code,
/// the same position and the same arguments (130.5.2, JYRO-REQ-1487).
let private reportedWithoutRepeats (what: string) (code: MessageCode) (r: JyroResult<'T>) =
    Assert.False(r.IsSuccess, sprintf "%s: compiled. %s" what (placed r))
    if not (List.contains code (Script.errorCodes r)) then
        Values.fail (sprintf "%s: no %A. Diagnostics:\n%s" what code (placed r))
    let repeated = r.Messages |> Seq.map sameness |> Seq.countBy id |> Seq.filter (fun (_, n) -> n > 1) |> List.ofSeq
    if not repeated.IsEmpty then
        Values.fail (sprintf "%s: two diagnostics with the same code, position and arguments (JYRO-REQ-1487): %A. Diagnostics:\n%s" what repeated (placed r))

/// A text of declarations only, so that it is a script and a library alike, checked on every
/// path a source text takes (160.1.1, 160.6): Compile(), CompileToArtifact(), AddLibrarySource and
/// JyroLibrary.Parse.
let private noRepeatsOnEveryPath (code: MessageCode) (text: string) =
    let what = sprintf "%A" text
    reportedWithoutRepeats (what + " (script, Compile)") code (Script.compileWith pinned text)
    reportedWithoutRepeats (what + " (script, CompileToArtifact)") code (Script.compileBytesWith pinned text)
    reportedWithoutRepeats (what + " (AddLibrarySource)") code (Script.compileWith (withLibs [ text ]) "Output.x = 1")
    reportedWithoutRepeats (what + " (JyroLibrary.Parse)") code (JyroLibrary.Parse text)

/// `func F(<names>) return 1 end`, hand-built for an artifact.
let private funcWith (names: string list) : Stmt = SFunc("F", [ for n in names -> n, None, None ], [ SReturn(Some(num 1m), p0) ], p0)

/// A union of the variants `vs` (each a name and its field names), hand-built for an artifact.
/// Each variant has a position of its own (150.8.5, JYRO-REQ-2121): the k-th (from 1) is on line
/// k + 1, column 3, so that no two variants of a load share a place (JYRO-REQ-2122).
let private unionOf (name: string) (vs: (string * string list) list) : Stmt =
    let posOf k = { Line = k + 1; Column = 3; Start = 10 * k; End = 10 * k + 1 }
    SUnion(name, [ for k, (v, fs) in List.indexed vs -> v, [ for f in fs -> f, None ], posOf (k + 1) ], p0)

/// JYRO-DEF-537: a variant named three or more times in one union of the script, or named in an
/// earlier union and twice more in a later one, gave two DuplicateVariantName diagnostics with
/// the same code, position (the union's) and arguments. A result has no two such diagnostics
/// (130.5.2), on every path a source text or an artifact takes, and the repeat is still refused
/// (50.10.4, JYRO-REQ-241).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1487")>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-DEF-537 a variant named three times or more is refused without two identical DuplicateVariantName diagnostics`` () =
    for text in [ "union U A A end"
                  "union U A A A end"
                  "union U A A A A end"
                  "union U\n  A\n  A\n  A\nend"
                  "union U A(x) B A(y) C A(z) end"
                  "union U A end\nunion V A A end"
                  "union U A end\nunion V A A A end"
                  "union U A end\nunion V B end\nunion W A A end" ] do
        noRepeatsOnEveryPath MessageCode.DuplicateVariantName text
    // The .jyrx load (160.1.1): the same unions in a hand-built artifact.
    for what, program in [ "load: A three times", [ unionOf "U" [ "A", []; "A", []; "A", [] ] ]
                           "load: A four times", [ unionOf "U" [ "A", []; "A", []; "A", []; "A", [] ] ]
                           "load: A in U, twice in V", [ unionOf "U" [ "A", [] ]; unionOf "V" [ "A", []; "A", [] ] ] ] do
        reportedWithoutRepeats what MessageCode.DuplicateVariantName (loadWith pinned (build program))

/// JYRO-DEF-540: the other repeat checks of validation fired once per extra occurrence at the
/// declaration's position, with arguments that do not tell the occurrences apart, so a name
/// given three times gave two identical diagnostics; and the .jyrx load and JyroLibrary.Parse
/// removed no repeat. No result has two diagnostics with the same code, position and arguments
/// (130.5.2): DuplicateParameterName, ReservedName for a union's variants, a lambda's parameters
/// and a match case's names, and ReservedFieldName, each with the name three times as well as
/// twice, on every path.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1487")>]
let ``JYRO-DEF-540 a name repeated in a parameter list, a variant list or a field list gives no two identical diagnostics`` () =
    let cases =
        [ MessageCode.DuplicateParameterName, "func F(a, a)\n  return a\nend"
          MessageCode.DuplicateParameterName, "func F(a, a, a)\n  return a\nend"
          MessageCode.DuplicateParameterName, "func F(a, b, a, b, a)\n  return a\nend"
          MessageCode.ReservedName, "union U Input Input end"
          MessageCode.ReservedName, "union U Input Input Input end"
          MessageCode.ReservedName, "func F()\n  return Map([1], (Input, Input) => 1)\nend"
          MessageCode.ReservedName, "func F()\n  return Map([1], (Input, Input, Input) => 1)\nend"
          MessageCode.ReservedName, "union U A(x, y) end\nfunc F(v)\n  return match v do case A(Input, Input) then 1 end\nend"
          MessageCode.ReservedName, "union U A(x, y, z) end\nfunc F(v)\n  return match v do case A(Input, Input, Input) then 1 end\nend"
          MessageCode.ReservedFieldName, "union U A(_variant, _variant) end"
          MessageCode.ReservedFieldName, "union U A(_variant, _variant, _variant) end"
          MessageCode.ReservedFieldName, "union U A(_variant) A(_variant) end"
          MessageCode.ReservedFieldName, "union U A(_variant) A(_variant) A(_variant) end" ]
    for code, text in cases do
        noRepeatsOnEveryPath code text
    // The .jyrx load (160.1.1): the same declarations in a hand-built artifact.
    let lambda names = setOut "x" (call "Map" [ arr [ num 1m ]; ELambda(names, num 1m, p0) ])
    let loads =
        [ MessageCode.DuplicateParameterName, "load: func F(a, a, a)", [ funcWith [ "a"; "a"; "a" ] ]
          MessageCode.ReservedName, "load: union U Input Input Input", [ unionOf "U" [ "Input", []; "Input", []; "Input", [] ] ]
          MessageCode.ReservedName, "load: (Input, Input, Input) => 1", [ lambda [ "Input"; "Input"; "Input" ] ]
          MessageCode.ReservedFieldName, "load: A(_variant, _variant, _variant)", [ unionOf "U" [ "A", [ "_variant"; "_variant"; "_variant" ] ] ]
          MessageCode.ReservedFieldName, "load: A(_variant) A(_variant) A(_variant)", [ unionOf "U" [ "A", [ "_variant" ]; "A", [ "_variant" ]; "A", [ "_variant" ] ] ] ]
    for code, what, program in loads do
        reportedWithoutRepeats what code (loadWith pinned (build program))

/// JYRO-DEF-26: 130.5.2 is a property of the whole result, not of one check. The two shapes the
/// analysis of JYRO-DEF-537 found (a variant in an earlier union and twice in a later one, and a
/// parameter named three times) together, beside an ordinary validation error, in the script, in
/// two libraries added as source, in a library and the script at once, in JyroLibrary.Parse
/// and on a load: every expected diagnostic is reported, and none twice. A position is read as
/// a place in one source text (the developer's reading in 73cb34c, accepted at inspection): two
/// source texts with the same text give diagnostics at the same line and column in different
/// groups (130.5.4), and those are not asserted to be merged.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1487")>]
let ``JYRO-DEF-26 no result holds two diagnostics with the same code, position and arguments when several checks repeat`` () =
    let text = "union U A end\nunion V A A A end\nfunc F(a, a, a)\n  return zzz\nend"
    let expected = [ MessageCode.DuplicateVariantName; MessageCode.DuplicateParameterName; MessageCode.UndeclaredVariable ]
    let check what (r: JyroResult<'T>) =
        for c in expected do reportedWithoutRepeats what c r
    check "script, Compile" (Script.compileWith pinned text)
    check "script, CompileToArtifact" (Script.compileBytesWith pinned text)
    check "JyroLibrary.Parse" (JyroLibrary.Parse text)
    // A library and a script each with the shapes, at different lines and with other names.
    let scriptText = "Output.y = 1\n\nunion W B end\nunion X B B B end\nfunc G(b, b, b)\n  return yyy\nend"
    check "one library and the script" (Script.compileWith (withLibs [ text ]) scriptText)
    // Two libraries with the same text: each library's group is free of repeats, and the two
    // groups are two source texts, so their diagnostics at 1:1 are not the same position.
    let two = Script.compileWith (withLibs [ text; text ]) "Output.x = 1"
    Assert.False(two.IsSuccess, placed two)
    for c in expected do
        if not (List.contains c (Script.errorCodes two)) then Values.fail (sprintf "two libraries: no %A. Diagnostics:\n%s" c (placed two))
    let program =
        [ unionOf "U" [ "A", [] ]
          unionOf "V" [ "A", []; "A", []; "A", [] ]
          SFunc("F", [ for _ in 1 .. 3 -> "a", None, None ], [ SReturn(Some(ident "zzz"), p0) ], p0) ]
    check "load" (loadWith pinned (build program))

/// Assert the positioned diagnostics of `r` are in line-then-column order, those with no position
/// after them (130.5.1), and that the diagnostics picked by `expected` (a code and a line each)
/// are all there, in the order given.
let private inPositionOrder (what: string) (expected: (MessageCode * int) list) (r: JyroResult<'T>) =
    Assert.False(r.IsSuccess, sprintf "%s: compiled. %s" what (placed r))
    let ms = List.ofSeq r.Messages
    let flags = ms |> List.map (fun m -> m.Location.HasValue)
    match List.tryFindIndex not flags with
    | Some i when flags |> List.skip i |> List.exists id ->
        Values.fail (sprintf "%s: a positioned diagnostic follows one with no position (JYRO-REQ-1486). Diagnostics:\n%s" what (placed r))
    | _ -> ()
    let pos = [ for m in ms do if m.Location.HasValue then yield m.Location.Value.Line, m.Location.Value.Column ]
    if List.sort pos <> pos then
        Values.fail (sprintf "%s: not in position order (JYRO-REQ-1486). Diagnostics:\n%s" what (placed r))
    let indexes =
        [ for code, line in expected ->
            match ms |> List.tryFindIndex (fun m -> m.Code = code && m.Location.HasValue && m.Location.Value.Line = line) with
            | Some i -> i
            | None -> Values.fail (sprintf "%s: no %A on line %d. Diagnostics:\n%s" what code line (placed r)) ]
    if List.sort indexes <> indexes then
        Values.fail (sprintf "%s: %A are not in that order. Diagnostics:\n%s" what expected (placed r))

/// JYRO-DEF-25: nothing ordered the diagnostic list, so the errors of the validator's union
/// pass came before every statement error, whatever their lines. The diagnostics of one source
/// text are in line-then-column order (130.5.1): a statement error on a line before a union
/// error comes first, for each union error (DuplicateUnionName, UnionInsideFunction,
/// UnionNotAtTopLevel, DuplicateVariantName, ReservedFieldName), on Compile() and
/// CompileToArtifact(), in a library, and for the Link codes of a union (VariantShadowsFunction,
/// FunctionShadowsVariant) after a call to an undefined function on line 1.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1486")>]
let ``JYRO-DEF-25 a statement error on a line before a union error is reported first`` () =
    let cases =
        [ "Output.a = zzz\nunion U A end\nunion U B end", [ MessageCode.UndeclaredVariable, 1; MessageCode.DuplicateUnionName, 3 ]
          "Output.a = zzz\nfunc F()\n  union U A end\n  return 1\nend", [ MessageCode.UndeclaredVariable, 1; MessageCode.UnionInsideFunction, 3 ]
          "Output.a = zzz\nif true then\n  union U A end\nend", [ MessageCode.UndeclaredVariable, 1; MessageCode.UnionNotAtTopLevel, 3 ]
          "Output.a = zzz\nunion U A end\nunion V A end", [ MessageCode.UndeclaredVariable, 1; MessageCode.DuplicateVariantName, 3 ]
          "Output.a = zzz\nunion U A A end", [ MessageCode.UndeclaredVariable, 1; MessageCode.DuplicateVariantName, 2 ]
          "Output.a = zzz\nunion U A(_variant) end", [ MessageCode.UndeclaredVariable, 1; MessageCode.ReservedFieldName, 2 ]
          "Output.a = zzz\nunion U A end\nOutput.b = yyy\nunion U B end", [ MessageCode.UndeclaredVariable, 1; MessageCode.UndeclaredVariable, 3; MessageCode.DuplicateUnionName, 4 ] ]
    for text, expected in cases do
        inPositionOrder (sprintf "%A (Compile)" text) expected (Script.compileWith pinned text)
        inPositionOrder (sprintf "%A (CompileToArtifact)" text) expected (Script.compileBytesWith pinned text)
    // In a library: a function body's error on a line before a union error.
    let lib = "func L()\n  return zzz\nend\nunion U A end\nunion U B end"
    inPositionOrder "library" [ MessageCode.UndeclaredVariable, 2; MessageCode.DuplicateUnionName, 5 ]
        (Script.compileWith (withLibs [ lib ]) "Output.x = 1")
    inPositionOrder "JyroLibrary.Parse" [ MessageCode.UndeclaredVariable, 2; MessageCode.DuplicateUnionName, 5 ] (JyroLibrary.Parse lib)
    // Link: a call to an undefined function on line 1, a union's Link error on a later line.
    let withBar = withHost "Bar"
    inPositionOrder "Link: UndefinedFunction, then VariantShadowsFunction" [ MessageCode.UndefinedFunction, 1; MessageCode.VariantShadowsFunction, 2 ]
        (Script.compileWith withBar "Output.a = Nope()\nunion U Bar end")
    inPositionOrder "Link: UndefinedFunction, then FunctionShadowsVariant" [ MessageCode.UndefinedFunction, 1; MessageCode.FunctionShadowsVariant, 3 ]
        (Script.compileWith pinned "Output.a = Nope()\nunion S Circle(r) end\nfunc Circle(x)\n  return x\nend")

/// The arguments of `m`, as text.
let private argTexts (m: DiagnosticMessage) : string list = m.Args |> Array.map (fun a -> if isNull a then "<null>" else string a) |> List.ofArray

/// The Link diagnostics of `r` that name one of `names`, as (code, name), in order.
let private linkNamed (names: string list) (r: JyroResult<'T>) : (MessageCode * string) list =
    [ for m in r.Messages do
        if Pending.stageOf m = "Link" then
            match argTexts m |> List.tryFind (fun a -> List.contains a names) with
            | Some n -> yield m.Code, n
            | None -> () ]

/// JYRO-DEF-542: the Link stage's diagnostics bypassed the one function that orders and
/// deduplicates a stage's diagnostics, and came in the linker's pass order: declaration
/// checks before body checks, a call's arguments before the call, and the libraries' pre-pass
/// codes of every library before any library's body codes. Within one source text they are in
/// position order (130.5.1); from library scripts they are grouped by source text, each library
/// in the order added, then the script (130.5.4); every Error is Link (the compilation stops
/// after the first failing stage, JYRO-ADR-115 point 1).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1486")>]
[<Trait("requirement", "JYRO-REQ-1489")>]
let ``JYRO-DEF-542 Link diagnostics are in position order within a source text and grouped by source text`` () =
    // (a) One source text: a declaration code on a later line than a body code.
    let withBar = withHost "Bar"
    let scriptA = "Output.a = Foo()\nunion U Bar end"
    let expectedA = [ MessageCode.UndefinedFunction, 1; MessageCode.VariantShadowsFunction, 2 ]
    inPositionOrder "script, Compile" expectedA (Script.compileWith withBar scriptA)
    stoppedAt "script, Compile" "Link" (Script.compileWith withBar scriptA)
    inPositionOrder "script, CompileToArtifact" expectedA (Script.compileBytesWith withBar scriptA)
    // Three body codes and a declaration code, interleaved by line.
    let scriptB = "Output.a = Foo1()\nunion U Bar end\nOutput.b = Length(1, 2)\nunion V Baz end\nOutput.c = Foo2()"
    inPositionOrder "script, interleaved" [ MessageCode.UndefinedFunction, 1; MessageCode.VariantShadowsFunction, 2; MessageCode.TooManyArguments, 3
                                            MessageCode.VariantShadowsFunction, 4; MessageCode.UndefinedFunction, 5 ]
        (Script.compileWith { pinned with Functions = [ (Host.recorder "Bar").AsFunction; (Host.recorder "Baz").AsFunction ] } scriptB)
    // (b) A call and its arguments: F at column 12 before G at column 14.
    let nested = Script.compileWith pinned "Output.a = F(G())"
    inPositionOrder "F(G())" [ MessageCode.UndefinedFunction, 1 ] nested
    Assert.Equal<(MessageCode * string) list>([ MessageCode.UndefinedFunction, "F"; MessageCode.UndefinedFunction, "G" ], linkNamed [ "F"; "G" ] nested)
    let arity = Script.compileWith pinned "func F(a)\n  return a\nend\nOutput.a = F(Nope(), 2)"
    inPositionOrder "F(Nope(), 2)" [ MessageCode.TooManyArguments, 4; MessageCode.UndefinedFunction, 4 ] arity
    // (c) Two libraries, each with a body code and a declaration code, in opposite line order,
    // and the script's own body code: library by library in the order added, then the script.
    let hosts = { pinned with Functions = [ (Host.recorder "Circle").AsFunction; (Host.recorder "Square").AsFunction ] }
    let lib1 = "func L1()\n  return Nope1()\nend\nunion U1 Circle end"
    let lib2 = "union U2 Square end\nfunc L2()\n  return Nope2()\nend"
    let script = "Output.a = Nope3()"
    let names = [ "Nope1"; "Circle"; "Square"; "Nope2"; "Nope3" ]
    let group1 = [ MessageCode.UndefinedFunction, "Nope1"; MessageCode.VariantShadowsFunction, "Circle" ]
    let group2 = [ MessageCode.VariantShadowsFunction, "Square"; MessageCode.UndefinedFunction, "Nope2" ]
    let scriptGroup = [ MessageCode.UndefinedFunction, "Nope3" ]
    let grouped what (expected: (MessageCode * string) list) (r: JyroResult<'T>) =
        stoppedAt what "Link" r
        let got = linkNamed names r
        if got <> expected then
            Values.fail (sprintf "%s: expected %A, got %A (JYRO-REQ-1489). Diagnostics:\n%s" what expected got (placed r))
    grouped "lib1, lib2, script" (group1 @ group2 @ scriptGroup) (Script.compileWith { hosts with Libraries = [ lib1; lib2 ] } script)
    grouped "lib2, lib1, script" (group2 @ group1 @ scriptGroup) (Script.compileWith { hosts with Libraries = [ lib2; lib1 ] } script)
    // The same libraries given as JyroLibrary (160.6): Link checks them in the compilation.
    let parsed1 = JyroLibrary.Parse lib1 |> Script.valueOf
    let parsed2 = JyroLibrary.Parse lib2 |> Script.valueOf
    grouped "JyroLibrary lib1, lib2, script" (group1 @ group2 @ scriptGroup)
        ((Script.compilerFor hosts).AddLibrary(parsed1).AddLibrary(parsed2).WithSource(script).Compile())
    grouped "JyroLibrary lib2, lib1, script" (group2 @ group1 @ scriptGroup)
        ((Script.compilerFor hosts).AddLibrary(parsed2).AddLibrary(parsed1).WithSource(script).Compile())
    // (d) The .jyrx load: a variant named after a registered function on line 2. The table names
    // no function, so the load passes rows 9 and 10 of 150.2.9 and reaches the checks after them;
    // a table name no host has would end the load at row 9 with UndefinedFunction alone
    // (JYRO-REQ-2066, JYRO-DEF-736). Positioned diagnostics first, in position order, the
    // unpositioned after them; no two the same (130.5.2).
    let line2 = { p0 with Line = 2; Column = 1 }
    let program = [ setOut "a" (num 1m); SUnion("U", [ "Bar", [], { line2 with Column = 9 } ], line2) ]
    let loaded = loadWith withBar (build program)
    inPositionOrder "load" [ MessageCode.VariantShadowsFunction, 2 ] loaded
    reportedWithoutRepeats "load" MessageCode.VariantShadowsFunction loaded

/// JYRO-DEF-529: a variant named twice within one union compiled with no diagnostic, in the
/// script and in a library, the second declaration dropped. Validation refuses a variant name
/// that another variant in any union already has, with DuplicateVariantName at Validate (50.10.4,
/// JYRO-REQ-241; 160.1.12), in a library added as source, in JyroLibrary.Parse (so no such
/// library can be added as a JyroLibrary), and in the script; and whatever the code, the
/// compilation fails (the regression from a wrong code to acceptance turns this red).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
[<Trait("requirement", "JYRO-REQ-2139")>]
let ``JYRO-REQ-2139 JYRO-DEF-529 a variant named twice within one union is refused in a library given as source and as a JyroLibrary`` () =
    let unions =
        [ "union U\n  A\n  A\nend"
          "union U A A end"
          "union U A A A end"
          "union U A(x) B A(y) end"
          "func F()\n  return 1\nend\nunion U B A(x) A end" ]
    for u in unions do
        let what = sprintf "%A" u
        let refused name (r: JyroResult<'T>) =
            Assert.False(r.IsSuccess, sprintf "%s (%s): compiled with no diagnostic. %s" what name (placed r))
            if not (List.contains MessageCode.DuplicateVariantName (Script.errorCodes r)) then
                Values.fail (sprintf "%s (%s): no DuplicateVariantName. Diagnostics:\n%s" what name (placed r))
            stageOfCode MessageCode.DuplicateVariantName "Validate" r
        refused "AddLibrarySource" (Script.compileWith (withLibs [ u ]) "Output.x = A()")
        refused "AddLibrarySource, CompileToArtifact" (Script.compileBytesWith (withLibs [ u ]) "Output.x = A()")
        refused "second of two libraries" (Script.compileWith (withLibs [ "func G()\n  return 1\nend"; u ]) "Output.x = A()")
        refused "JyroLibrary.Parse" (JyroLibrary.Parse u)
        refused "script" (Script.compileWith pinned (u + "\nOutput.x = A()"))

/// A host function whose signature takes `delayMs` to read, counting each read: a long
/// Link, which reads the signature of each call it resolves, and a way to see whether it
/// is still running.
type private SlowSignature(name: string, delayMs: int) =
    let mutable reads = 0
    member _.Reads = Volatile.Read(&reads)
    interface IJyroFunction with
        member _.Name = name
        member _.Signature =
            Interlocked.Increment(&reads) |> ignore
            Thread.Sleep delayMs
            Host.signature name [] ParameterType.Any
        member _.Effect = FunctionEffect.Pure
        member _.Execute(_, _) = Values.nul

/// JYRO-DEF-502: JYRO-ADR-100 gives the worker of a compilation a cancellation that each
/// stage checks at its boundaries and in its long loops, so that an abandoned compilation
/// stops instead of running on. A source of 200 calls to a function whose signature takes
/// 10 ms to read makes a Link of about 2 s; a limit of 300 ms is reached while it runs
/// (CompilationTimeLimitExceeded, stage Link, JYRO-REQ-2174). Once Compile() has returned,
/// Link must stop: in the next second it may finish the read in progress (a slack of two reads), not go on to
/// the remaining calls.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``JYRO-DEF-502 an abandoned compilation stops in Link once the time limit is reached`` () =
    let slow = SlowSignature("Slow", 10)
    let src = String.Join("\n", [ for i in 1 .. 200 -> sprintf "Output.x%d = Slow()" i ])
    let o = { pinned with Functions = [ slow :> IJyroFunction ]; CompilationTimeout = Some(TimeSpan.FromMilliseconds 300.) }
    let r = Script.compileWith o src
    let atReturn = slow.Reads
    fails MessageCode.CompilationTimeLimitExceeded r
    stageOfCode MessageCode.CompilationTimeLimitExceeded "Link" r
    Thread.Sleep 1000
    let after = slow.Reads
    Assert.True(after <= atReturn + 2,
                sprintf "The abandoned compilation ran on: %d signature reads when Compile() returned, %d one second later" atReturn after)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2105")>]
let ``StageOfDiagnosticsInThrownExceptions_IsTheAssignedStage`` () =
    // The exception is the one the host API throws for a failed pipeline: JyroException (JYRO-REQ-1885).
    let messagesOf (ex: JyroException) : DiagnosticMessage list = ex.Messages |> List.ofSeq
    let p = Script.compile "Output.x = 1 / 0" |> Script.valueOf
    let ex = Assert.ThrowsAny<JyroException>(fun () -> JyroRunner(p).ExecuteOrThrow() |> ignore)
    Assert.Equal("Execute", Pending.stageOf (messagesOf ex).Head)
    let ex2 = Assert.ThrowsAny<JyroException>(fun () -> (Script.compilerFor Script.defaults).WithSource("Output.x = zzz").CompileOrThrow() |> ignore)
    Assert.Equal("Validate", Pending.stageOf (messagesOf ex2).Head)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2105")>]
let ``StageOnEveryResultMember_IsTheAssignedStage`` () =
    for path in [ Runner; ExecuteScript; ExecuteScriptWithJson; CompiledBytes ] do
        stageOfCode MessageCode.DivisionByZero "Execute" (runOnPath path "Output.x = 1 / 0")
        stageOfCode MessageCode.UndeclaredVariable "Validate" (runOnPath path "Output.x = zzz")
    let parsed = JyroLibrary.Parse "var x = 1"
    stageOfCode MessageCode.LibraryStatementForbidden "Validate" parsed

// -- 160.7.6 MatchSubjectNotVariant is 5403 -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1617")>]
let ``MatchSubjectNotVariant_IsAMemberOfMessageCodeWithValue5403`` () =
    Pending.requireMatchSubjectNotVariant ()
    Assert.Equal("MatchSubjectNotVariant", Enum.GetName(typeof<MessageCode>, Pending.matchSubjectNotVariant))
    let sharing = Enum.GetNames(typeof<MessageCode>) |> Array.filter (fun n -> int (Enum.Parse<MessageCode> n) = 5403)
    Assert.Equal<string[]>([| "MatchSubjectNotVariant" |], sharing)

// That a match on a value of no variant raises 5403 (null among them) is JYRO-REQ-1971's, and
// is in its tests (S50_08_SwitchMatch), as JYRO-ADR-113 item 2 places it.

// -- 160.2.1 / 160.2.2 Host functions --------------------------------------------------------------------

type Scale() =
    inherit JyroFunctionBase(
        "Scale",
        Host.signature "Scale"
            [ Parameter.Required("value", ParameterType.Number); Parameter.Optional("factor", ParameterType.Number) ]
            ParameterType.Number)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(args, _) =
        let f = if args.Count > 1 then args.GetDecimal(1) else 10m
        JyroNumber(args.GetDecimal(0) * f) :> JyroValue

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1588")>]
let ``HostFunction_HasANameAParameterListAndAReturnType`` () =
    let f = Scale() :> IJyroFunction
    Assert.Equal("Scale", f.Name)
    let ps = f.Signature.Parameters |> List.ofSeq
    Assert.Equal<string list>([ "value"; "factor" ], ps |> List.map (fun p -> p.Name))
    Assert.Equal<ParameterType list>([ ParameterType.Number; ParameterType.Number ], ps |> List.map (fun p -> p.Type))
    Assert.Equal<bool list>([ false; true ], ps |> List.map (fun p -> p.IsOptional))
    Assert.Equal(ParameterType.Number, f.Signature.ReturnType)
    let v = Script.runWith { Script.defaults with Functions = [ f ] } "Output.a = Scale(2)\nOutput.b = Scale(2, 3)" |> ok
    Assert.Equal("{\"a\":20,\"b\":6}", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1588")>]
let ``HostFunctionBase_CannotBeMadeWithoutANameAndASignature`` () =
    let ctors = typeof<JyroFunctionBase>.GetConstructors(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Instance)
    for c in ctors |> Array.filter (fun c -> c.IsPublic || c.IsFamily) do
        let types = c.GetParameters() |> Array.map (fun p -> p.ParameterType)
        Assert.Contains(typeof<string>, types)
        Assert.Contains(typeof<JyroFunctionSignature>, types)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1589")>]
let ``HostFunctionCall_CountsAgainstTheCallDepth`` () =
    let h = Host.recorder "H"
    let o d = { withLimits (fun l -> { l with MaxCallDepth = d }) with Functions = [ h.AsFunction ] }
    Script.runWith (o 1) "H()" |> ok |> ignore
    Script.runWith (o 1) "func F()\n  return H()\nend\nOutput.r = F()" |> fails MessageCode.CallDepthLimitExceeded
    Script.runWith (o 2) "func F()\n  return H()\nend\nOutput.r = F()" |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1589")>]
let ``HostFunctionCall_ObeysTheTimeLimit`` () =
    let checkedStall = stallChecked ()
    Script.runWith { withLimits (fun l -> { l with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with Functions = [ checkedStall.AsFunction ] } "Output.r = StallChecked(300)"
    |> fails MessageCode.ExecutionTimeLimitExceeded
    let s = stall ()
    let m = marker ()
    Script.runWith { withLimits (fun l -> { l with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with Functions = [ s.AsFunction; m.AsFunction ] } "Stall(300)\nMark(1)"
    |> fails MessageCode.ExecutionTimeLimitExceeded
    Assert.Equal(0, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1589")>]
let ``HostFunctionCall_ObeysTheCancellationSignal`` () =
    use cts = new CancellationTokenSource()
    let mutable sawIt = false
    let h =
        Host.funcWithContext "Watch" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
            cts.Cancel()
            sawIt <- ctx.CancellationToken.IsCancellationRequested
            ctx.CheckExecutionLimits()
            Values.nul)
    Script.runWith { Script.defaults with Functions = [ h.AsFunction ]; Cancellation = Some cts.Token } "Output.r = Watch()"
    |> fails MessageCode.CanceledByHost
    Assert.True(sawIt, "The host function could not see the cancellation")

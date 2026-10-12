/// Part 110: the statistics a result gives the host.
module RealTree.Jyro.Spec.Tests.S110_Statistics

open System
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private sumOf (s: JyroPipelineStats) =
    s.ParseTime + s.ValidateTime + s.LinkTime + s.CompileTime + s.ExecuteTime + s.DeserializeTime

let private source = manyStatements 800

// -- 110.1.1 The time of each stage --------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1452")>]
let ``SourceRun_GivesParseValidateLinkCompileAndExecuteTimes`` () =
    let o, s = withStats Script.defaults
    Script.runWith o source |> ok |> ignore
    for name, t in stageTimes s |> List.filter (fun (n, _) -> n <> "Deserialize") do
        Assert.True(t > TimeSpan.Zero, sprintf "%s time is %O" name t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1452")>]
let ``ArtifactRun_GivesDeserializeAndExecuteTimes`` () =
    let bytes = Script.compileBytesWith Script.defaults source |> Script.valueOf
    let o, s = withStats Script.defaults
    Script.runBytesWith o bytes |> ok |> ignore
    Assert.True(s.DeserializeTime > TimeSpan.Zero, sprintf "Deserialize %O" s.DeserializeTime)
    Assert.True(s.ExecuteTime > TimeSpan.Zero, sprintf "Execute %O" s.ExecuteTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1452")>]
let ``Statistics_HaveAMemberForEachOfTheSixStages`` () =
    for name in [ "ParseTime"; "ValidateTime"; "LinkTime"; "CompileTime"; "ExecuteTime"; "DeserializeTime"; "TotalTime" ] do
        let p = typeof<JyroPipelineStats>.GetProperty(name)
        Assert.NotNull p
        Assert.Equal(typeof<TimeSpan>, p.PropertyType)

// -- 110.1.2 The total is the sum ------------------------------------------------------

let totalCases : obj[] seq =
    [ "source"; "artifact"; "parse-failure"; "validation-failure"; "link-failure"; "runtime-failure"; "limit" ]
    |> Seq.map (fun c -> [| box c |])

let private runCase (case: string) : JyroPipelineStats =
    let o, s = withStats Script.defaults
    match case with
    | "source" -> Script.runWith o source |> ignore
    | "artifact" ->
        let bytes = Script.compileBytesWith Script.defaults source |> Script.valueOf
        Script.runBytesWith o bytes |> ignore
    | "parse-failure" -> Script.runWith o "Output.a = 1\nvar = (" |> ignore
    | "validation-failure" -> Script.runWith o "Output.a = zzz" |> ignore
    | "link-failure" -> Script.runWith o "Output.a = NoSuchFunction(1)" |> ignore
    | "runtime-failure" -> Script.runWith o (source + "\nOutput.z = 1 / 0") |> ignore
    | "limit" ->
        Script.runWith { o with Limits = Some(Script.limits (fun l -> { l with MaxStatements = 10 })) } source |> ignore
    | other -> failwithf "unknown case %s" other
    s

[<Theory>]
[<MemberData(nameof totalCases)>]
[<Trait("requirement", "JYRO-REQ-1453")>]
let ``Total_IsTheSumOfTheStageTimes`` (case: string) =
    let s = runCase case
    Assert.Equal(sumOf s, s.TotalTime)

// -- 110.2.1 Statistics after a failure ---------------------------------------------------

/// d-223 (fixed under FLOORS 4.1.11): each row names the failure it expects by
/// its code, and the compilation and execution limits are set explicitly, so a
/// failure of another stage cannot pass the first assertion. JYRO-DEF-472: the
/// stage is not asserted; 110.2.1 does not require it, the code pins the
/// failure, and Execute > 0 shows that Execute ran.
[<Theory>]
[<InlineData("Output.a = 1\nfail \"x\"", "ScriptFailure")>]      // 140.4.3
[<InlineData("Output.a = 1\nOutput.b = 1 / 0", "DivisionByZero")>]  // 40.3.1
[<Trait("requirement", "JYRO-REQ-1454")>]
let ``FailedExecution_GivesTheStatistics`` (script: string, code: string) =
    let o, s = withStats pinned
    let r = Script.runWith o (source + "\n" + script)
    // The code by the name the book gives it, not by a number (d-558, 10.1.1).
    let expected = Enum.Parse<MessageCode> code
    fails expected r
    Assert.True(s.ParseTime > TimeSpan.Zero && s.ExecuteTime > TimeSpan.Zero, sprintf "Parse %O Execute %O" s.ParseTime s.ExecuteTime)
    Assert.Equal(sumOf s, s.TotalTime)
    Assert.True(s.StatementCount >= 801L, sprintf "StatementCount %d" s.StatementCount)

/// d-457 (fixed under FLOORS 4.1.11): each limit breach is asserted as its own
/// code (100.3.2 to 100.3.4) before the statistics, every other limit is set
/// explicitly, and the compilation limit is generous. JYRO-DEF-472: the stage
/// is not asserted; 110.2.1 does not require it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1454")>]
let ``EachLimitBreach_GivesTheStatistics`` () =
    let cases =
        [ (fun (l: JyroExecutionOptions) -> { allLimits l with MaxStatements = 10 }), manyStatements 50, MessageCode.StatementLimitExceeded, 10L, 0L
          (fun l -> { allLimits l with MaxLoopIterations = 5 }), "var i = 0\nwhile i < 50 do i = i + 1 end", MessageCode.LoopIterationLimitExceeded, 7L, 5L
          (fun l -> { allLimits l with MaxCallDepth = 3 }), recursion 10, MessageCode.CallDepthLimitExceeded, 1L, 0L ]
    for change, script, code, minStatements, loops in cases do
        let o, s = withStats { pinned with Limits = Some(Script.limits change) }
        let r = Script.runWith o script
        fails code r
        Assert.True(s.ExecuteTime > TimeSpan.Zero, "Execute time is zero after a breach")
        Assert.Equal(sumOf s, s.TotalTime)
        Assert.True(s.StatementCount >= minStatements, sprintf "StatementCount %d" s.StatementCount)
        Assert.True(s.LoopIterations >= loops, sprintf "LoopIterations %d" s.LoopIterations)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1454")>]
let ``TimeLimitAndCancellation_GiveTheStatistics`` () =
    let st = stall ()
    let o, s =
        withStats { withLimits (fun l -> { l with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with Functions = [ st.AsFunction ] }
    Script.runWith o "Output.a = 1\nStall(300)\nOutput.b = 2" |> fails MessageCode.ExecutionTimeLimitExceeded
    Assert.True(s.ExecuteTime >= TimeSpan.FromMilliseconds 100., sprintf "Execute %O" s.ExecuteTime)
    Assert.Equal(sumOf s, s.TotalTime)
    use cts = new CancellationTokenSource()
    let c = canceller cts
    let o2, s2 = withStats { Script.defaults with Functions = [ c.AsFunction ]; Cancellation = Some cts.Token }
    Script.runWith o2 "Output.a = 1\nCancelNow()\nOutput.b = 2" |> fails MessageCode.CanceledByHost
    Assert.True(s2.ExecuteTime > TimeSpan.Zero)
    Assert.True(s2.StatementCount >= 2L, sprintf "StatementCount %d" s2.StatementCount)

[<Theory>]
[<InlineData("Output.a = 1\nvar = (")>]
[<InlineData("Output.a = zzz")>]
[<InlineData("Output.a = NoSuchFunction(1)")>]
[<Trait("requirement", "JYRO-REQ-1454")>]
let ``FailureBeforeExecution_GivesTheStatistics`` (script: string) =
    let o, s = withStats Script.defaults
    let r = Script.runWith o (source + "\n" + script)
    Assert.False(r.IsSuccess)
    Assert.True(s.ParseTime > TimeSpan.Zero, "Parse time is zero")
    Assert.Equal(sumOf s, s.TotalTime)

// -- 110.2.2 A stage that did not run has zero time ----------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``ParseError_LaterStagesAreZero`` () =
    let o, s = withStats Script.defaults
    Script.runWith o (source + "\nvar = (") |> ignore
    for name, t in stageTimes s |> List.filter (fun (n, _) -> n <> "Parse") do
        Assert.True((t = TimeSpan.Zero), sprintf "%s is %O after a parse error" name t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``ValidationError_LinkCompileExecuteAndDeserializeAreZero`` () =
    let o, s = withStats Script.defaults
    let r = Script.runWith o (source + "\nOutput.z = zzz")
    Assert.Contains(MessageCode.UndeclaredVariable, Script.codes r)
    for name in [ "Link"; "Compile"; "Execute"; "Deserialize" ] do
        let t = stageTimes s |> List.find (fun (n, _) -> n = name) |> snd
        Assert.True((t = TimeSpan.Zero), sprintf "%s is %O after a validation error" name t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``LinkError_CompileExecuteAndDeserializeAreZero`` () =
    let o, s = withStats Script.defaults
    let r = Script.runWith o (source + "\nOutput.z = NoSuchFunction(1)")
    Assert.Contains(MessageCode.UndefinedFunction, Script.codes r)
    for name in [ "Compile"; "Execute"; "Deserialize" ] do
        let t = stageTimes s |> List.find (fun (n, _) -> n = name) |> snd
        Assert.True((t = TimeSpan.Zero), sprintf "%s is %O after a link error" name t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``SourceRun_DeserializeIsZero`` () =
    let o, s = withStats Script.defaults
    Script.runWith o source |> ok |> ignore
    Assert.Equal(TimeSpan.Zero, s.DeserializeTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``ArtifactRun_ParseIsZero`` () =
    let bytes = Script.compileBytesWith Script.defaults source |> Script.valueOf
    let o, s = withStats Script.defaults
    Script.runBytesWith o bytes |> ok |> ignore
    Assert.Equal(TimeSpan.Zero, s.ParseTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``CompileWithoutExecute_ExecuteIsZero`` () =
    let o, s = withStats Script.defaults
    let r = Script.compileWith o source
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(TimeSpan.Zero, s.ExecuteTime)
    Assert.Equal(TimeSpan.Zero, s.DeserializeTime)

/// Part 140: the pipeline, determinism, single-pass execution and the script result.
module RealTree.Jyro.Spec.Tests.S140_Model

open System
open System.Globalization
open System.Security.Cryptography
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 140.1.x Pipeline stages -------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2156")>]
[<Trait("requirement", "JYRO-REQ-1497")>]
let ``JYRO-REQ-2156 SourceRun_EachOfTheFiveStagesRecordsItsOwnTime`` () =
    let o, s = withStats Script.defaults
    Script.runWith o (manyStatements 800) |> ok |> ignore
    for name, t in [ "Parse", s.ParseTime; "Validate", s.ValidateTime; "Link", s.LinkTime; "Compile", s.CompileTime; "Execute", s.ExecuteTime ] do
        Assert.True(t > TimeSpan.Zero, sprintf "%s recorded no time" name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2156")>]
let ``JYRO-REQ-2156 FailureInAStage_StopsTheLaterStages`` () =
    let check (src: string) (zero: string list) =
        let o, s = withStats Script.defaults
        Script.runWith o src |> ignore
        for name in zero do
            let t = stageTimes s |> List.find (fun (n, _) -> n = name) |> snd
            Assert.True((t = TimeSpan.Zero), sprintf "%s ran (%O) for %s" name t src)
    check "Output.a = 1\nvar = (" [ "Validate"; "Link"; "Compile"; "Execute" ]
    check "Output.a = zzz" [ "Link"; "Compile"; "Execute" ]
    check "Output.a = Nope()" [ "Compile"; "Execute" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2156")>]
let ``JYRO-REQ-2156 CompiledProgram_HasNotRun_UntilExecute`` () =
    let side = Host.recorder "Side"
    let program = Script.compileWith { Script.defaults with Functions = [ side.AsFunction ] } "Side(1)" |> Script.valueOf
    Assert.Equal(0, side.CallCount)
    (Script.runnerFor Script.defaults program).Execute() |> ok |> ignore
    Assert.Equal(1, side.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1496")>]
let ``EachDiagnostic_CarriesTheStageThatProducedIt`` () =
    stageOfCode MessageCode.ArtifactInvalidMagic "Deserialize" (Script.loadBytesWith Script.defaults ([| 0uy; 1uy |]))
    stageOfCode MessageCode.UnterminatedString "Parse" (Script.compile "Output.a = \"x")
    stageOfCode MessageCode.UndeclaredVariable "Validate" (Script.compile "Output.a = zzz")
    stageOfCode MessageCode.HostFunctionOverridesStdlib "Link"
        (Script.compileWith { Script.defaults with Functions = [ (Host.recorder "Length").AsFunction ] } "Output.a = 1")
    stageOfCode MessageCode.ArtifactCannotEncodeLibraries "Compile"
        (Script.compileBytesWith { Script.defaults with Libraries = [ "func L()\n  return 1\nend" ] } "Output.a = 1")
    stageOfCode MessageCode.DivisionByZero "Execute" (Script.run "Output.a = 1 / 0")
    stageOfCode MessageCode.ScriptFailure "Execute" (Script.run "fail \"x\"")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1496")>]
let ``EveryDiagnosticOfAMultiDiagnosticResult_CarriesAStage`` () =
    let r = Script.compile "Output.a = u1\nOutput.b = u2\nbreak"
    Assert.True(r.Messages.Count >= 3)
    for m in r.Messages do Assert.Equal("Validate", Pending.stageOf m)

/// JYRO-DEF-457: no diagnostic carried a stage a host could read. A host reads it
/// as `DiagnosticMessage.Stage`, a value of `MessageStage` (160.1.11, 160.7.4,
/// 160.7.5); one diagnostic from each of the six stages, read by that property
/// directly so that a build without it fails here plainly, not as blocked.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1496")>]
let ``JYRO-DEF-457 a host reads the stage of a diagnostic of each of the six stages from DiagnosticMessage.Stage`` () =
    let p = typeof<DiagnosticMessage>.GetProperty("Stage", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)
    if isNull p then Values.fail "DiagnosticMessage.Stage is not in this build: no diagnostic carries a stage a host can read"
    Assert.Equal("RealTree.Jyro.MessageStage", p.PropertyType.FullName)
    let cases : (string * MessageCode * DiagnosticMessage seq) list =
        [ "Deserialize", MessageCode.ArtifactInvalidMagic, (Script.loadBytesWith Script.defaults ([| 0uy; 1uy |])).Messages
          "Parse", MessageCode.UnterminatedString, (Script.compile "Output.a = \"x").Messages
          "Validate", MessageCode.UndeclaredVariable, (Script.compile "Output.a = zzz").Messages
          "Link", MessageCode.UndefinedFunction, (Script.compile "Output.a = Nope()").Messages
          "Compile", MessageCode.ArtifactCannotEncodeLibraries,
              (Script.compileBytesWith { Script.defaults with Libraries = [ "func L()\n  return 1\nend" ] } "Output.a = 1").Messages
          "Execute", MessageCode.DivisionByZero, (Script.run "Output.a = 1 / 0").Messages ]
    for stage, code, messages in cases do
        let m =
            match messages |> Seq.tryFind (fun m -> m.Code = code) with
            | Some m -> m
            | None -> Values.fail (sprintf "No diagnostic %A for the %s case" code stage)
        Assert.Equal(stage, string (p.GetValue m))

// -- 140.2.x Determinism -------------------------------------------------------------------------

let private corpus = determinismCorpus

let private sha (b: byte[]) = Convert.ToHexString(SHA256.HashData b)

let private bytesOf (src: string) = Script.compileBytesWith Script.defaults src |> Script.valueOf

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2057")>]
let ``SameSourceAndFunctions_GiveProgramsThatBehaveTheSame`` () =
    for src in corpus do
        let a = Script.run src
        let b = Script.run src
        Assert.Equal(a.IsSuccess, b.IsSuccess)
        Assert.Equal(Values.toJson (Script.valueOf a), Values.toJson (Script.valueOf b))
        Assert.Equal(Script.describe a, Script.describe b)
        Assert.True(a.IsSuccess, Script.describe a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2057")>]
let ``SameSourceAndFunctions_GiveTheSameArtifact`` () =
    for src in corpus do
        Assert.Equal(sha (bytesOf src), sha (bytesOf src))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2157")>]
let ``JYRO-REQ-2157 Compilation_DoesNotDependOnTheTime`` () =
    let before = corpus |> List.map (bytesOf >> sha)
    Thread.Sleep 1100
    let after = corpus |> List.map (bytesOf >> sha)
    Assert.Equal<string list>(before, after)

/// Runs its tests one at a time: they change the process culture.
[<CollectionDefinition("process-wide", DisableParallelization = true)>]
type ProcessWideCollection() = class end

[<Collection("process-wide")>]
type CultureTests() =
    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2157")>]
    member _.``JYRO-REQ-2157 Compilation_DoesNotDependOnTheLocale``() =
        let saved = CultureInfo.CurrentCulture, CultureInfo.CurrentUICulture
        let baseline =
            CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
            CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture
            corpus |> List.map (fun s -> sha (bytesOf s), Values.toJson (Script.run s |> Script.valueOf))
        try
            for name in [ "tr-TR"; "de-DE"; "ar-SA"; "fr-FR"; "ja-JP" ] do
                let c = CultureInfo(name)
                CultureInfo.CurrentCulture <- c
                CultureInfo.CurrentUICulture <- c
                let now = corpus |> List.map (fun s -> sha (bytesOf s), Values.toJson (Script.run s |> Script.valueOf))
                Assert.Equal<(string * string) list>(baseline, now)
        finally
            CultureInfo.CurrentCulture <- fst saved
            CultureInfo.CurrentUICulture <- snd saved

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2157")>]
let ``JYRO-REQ-2157 Compilation_InAnotherProcess_GivesTheSameArtifact`` () =
    let o = Child.run "hashes"
    Child.expectClean o
    let here = String.Join(",", corpus |> List.map (bytesOf >> sha))
    Assert.Equal("ok=true codes= " + here, o.Result.Value)

/// The hashes after the culture's name in a `culturehashes` child's result.
let private hashesOf (o: Child.ChildOutcome) : string =
    Child.expectClean o
    if not o.Ok then Values.fail ("Expected success: " + o.Describe)
    let r = o.Result.Value
    r.Substring(r.IndexOf ';' + 1)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2157")>]
let ``JYRO-REQ-2157 Artifact_WrittenUnderDeDEAndEnUS_InSeparateProcessesAtDifferentTimes_IsByteForByteTheSame`` () =
    let tz = [ "TZ", "Pacific/Auckland" ]
    let de = Child.runWith 30_000 tz "culturehashes|de-DE" |> hashesOf
    Thread.Sleep 1100
    let us = Child.runWith 30_000 [ "TZ", "America/Los_Angeles" ] "culturehashes|en-US" |> hashesOf
    Assert.Equal(de, us)
    // The artifact of `Output.x = 1.5` alone, written here, is part of the same bytes.
    let here = determinismHashes () + ";" + Child.registeredFunctionsHash ()
    Assert.Equal(here, us)

[<Theory>]
[<InlineData("tr-TR")>]
[<InlineData("ar-SA")>]
[<InlineData("fr-FR")>]
[<InlineData("ja-JP")>]
[<Trait("requirement", "JYRO-REQ-2157")>]
let ``JYRO-REQ-2157 Artifact_WithRegisteredFunctions_UnderAnotherCulture_IsTheSameAsUnderInvariant`` (culture: string) =
    let invariant = Child.runWith 30_000 [] "culturehashes|" |> hashesOf
    let other = Child.runWith 30_000 [] ("culturehashes|" + culture) |> hashesOf
    Assert.Equal(invariant, other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2157")>]
let ``JYRO-REQ-2157 Artifact_OfTheSameSourceAndFunctions_IsTheSameWhenWrittenTwiceInOneProcess`` () =
    let fns = [ (Host.recorder "Alpha").AsFunction; (Host.recorder "Beta").AsFunction ]
    let source = "Output.x = 1.5\nOutput.a = Alpha(1)\nOutput.b = Beta(2.25)"
    let write () = Script.compileBytesWith { Script.defaults with Functions = fns } source |> Script.valueOf
    let a = write ()
    Thread.Sleep 50
    let b = write ()
    Assert.Equal<byte[]>(a, b)

// -- 140.3.x Execution is a single pass per call ----------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1500")>]
let ``EachExecute_RunsTheProgramOnce`` () =
    let tick = Host.recorder "Tick"
    let o = { Script.defaults with Functions = [ tick.AsFunction ] }
    let program = Script.compileWith o "Tick()" |> Script.valueOf
    let runner = Script.runnerFor o program
    for i in 1 .. 3 do
        runner.Execute() |> ok |> ignore
        Assert.Equal(i, tick.CallCount)
    (Script.runnerFor o program).Execute() |> ok |> ignore
    (Script.runnerFor o program).Execute() |> ok |> ignore
    Assert.Equal(5, tick.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1501")>]
let ``EachExecution_StartsWithTheVariablesInTheirFirstState`` () =
    let program = Script.compile "var n = 0\nn = n + 1\nOutput.n = n" |> Script.valueOf
    let runner = Script.runnerFor Script.defaults program
    for _ in 1 .. 2 do
        Assert.Equal(1m, Values.asDecimal (Values.prop "n" (runner.Execute() |> ok)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1501")>]
let ``EachExecution_StartsWithFreshCounters`` () =
    let loops = Script.compile "var i = 0\nwhile i < 600 do i = i + 1 end" |> Script.valueOf
    let r1 = Script.runnerFor Script.defaults loops
    r1.Execute() |> ok |> ignore
    r1.Execute() |> ok |> ignore
    let stmts = Script.compile (statementScript 6_000) |> Script.valueOf
    let r2 = Script.runnerFor Script.defaults stmts
    r2.Execute() |> ok |> ignore
    r2.Execute() |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1501")>]
let ``EachExecution_StartsWithNewStatistics`` () =
    let program = Script.compile "var i = 0\nwhile i < 5 do i = i + 1 end" |> Script.valueOf
    let shared = JyroPipelineStats()
    let runner = (Script.runnerFor Script.defaults program).WithStats(shared)
    runner.Execute() |> ok |> ignore
    let first = shared.StatementCount, shared.LoopIterations
    runner.Execute() |> ok |> ignore
    Assert.Equal(first, (shared.StatementCount, shared.LoopIterations))
    let fresh = JyroPipelineStats()
    (Script.runnerFor Script.defaults program).WithStats(fresh).Execute() |> ok |> ignore
    Assert.Equal(first, (fresh.StatementCount, fresh.LoopIterations))
    Assert.Equal(7L, fresh.StatementCount)

// -- 140.4.x The script result --------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1502")>]
let ``ScriptEndsNormally_TheValueIsOutput_AndItSucceeds`` () =
    let v = Script.run "Output.a = 1" |> ok
    Assert.Equal("{\"a\":1}", Values.toJson v)
    let e = Script.run "var x = 1" |> ok
    Assert.Equal("{}", Values.toJson e)
    let first = Values.obj [ "pre", Values.int 1 ]
    let h = Script.runWith { Script.defaults with Output = Some first } "Output.a = 2" |> ok
    Assert.Equal("{\"pre\":1,\"a\":2}", Values.toJson h)

[<Theory>]
[<InlineData("Output.a = 1\nexit\nOutput.b = 2")>]
[<InlineData("Output.a = 1\nexit \"done\"\nOutput.b = 2")>]
[<InlineData("Output.a = 1\nforeach x in [1, 2] do\n  exit\nend\nOutput.b = 2")>]
[<InlineData("func F()\n  exit \"f\"\nend\nOutput.a = 1\nF()\nOutput.b = 2")>]
[<InlineData("Output.a = 1\nswitch 1 do case 1 then exit end\nOutput.b = 2")>]
[<Trait("requirement", "JYRO-REQ-1503")>]
let ``ScriptEndsWithExit_TheValueIsOutput_AndItSucceeds`` (src: string) =
    let r = Script.run src
    let v = ok r
    Assert.True(hasProp "a" v)
    Assert.False(hasProp "b" v)
    Assert.Empty(r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error))

[<Theory>]
[<InlineData("Output.a = 1\nfail\nOutput.b = 2")>]
[<InlineData("Output.a = 1\nfail \"bad\"\nOutput.b = 2")>]
[<InlineData("Output.a = 1\nforeach x in [1, 2] do\n  fail \"loop\"\nend\nOutput.b = 2")>]
[<InlineData("func F()\n  fail \"f\"\nend\nOutput.a = 1\nF()\nOutput.b = 2")>]
[<Trait("requirement", "JYRO-REQ-1504")>]
let ``ScriptEndsWithFail_TheValueIsOutput_ItFails_WithScriptFailure`` (src: string) =
    let r = Script.run src
    Assert.False(r.IsSuccess)
    fails MessageCode.ScriptFailure r
    let v = Script.valueOf r
    Assert.True(hasProp "a" v)
    Assert.False(hasProp "b" v)

[<Theory>]
[<InlineData("exit 42")>]
[<InlineData("exit {x: 1}")>]
[<InlineData("fail {x: 1}")>]
[<InlineData("fail 42")>]
[<InlineData("exit [9]")>]
[<Trait("requirement", "JYRO-REQ-1505")>]
let ``ExitOrFailValue_DoesNotReplaceOutput_AndEveryEarlierChangeIsKept`` (last: string) =
    let r = Script.run ("Output.a = 1\nOutput.b = [1]\nOutput.c = {n: 1}\nOutput.c.n = 2\nOutput.a = 3\n" + last)
    let v = Script.valueOf r
    Assert.Equal("{\"a\":3,\"b\":[1],\"c\":{\"n\":2}}", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1505")>]
let ``ExitInAFunction_KeepsTheChangesMadeBeforeIt`` () =
    let v = Script.run "func F(o)\n  o.x = 5\n  exit 7\nend\nOutput.o = {}\nF(Output.o)" |> ok
    Assert.Equal("{\"o\":{\"x\":5}}", Values.toJson v)

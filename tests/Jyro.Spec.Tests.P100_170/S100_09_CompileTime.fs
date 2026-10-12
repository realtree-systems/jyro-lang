/// 100.9.3: a limit the host sets bounds every stage before Execute, on both paths.
/// (The default compilation time limit of 5 seconds, JYRO-REQ-1911, is tested in the C# host
/// tests; the former tests here asserted that there was no default limit and were dropped.)
module RealTree.Jyro.Spec.Tests.S100_09_CompileTime

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

// -- 100.9.3 The limit covers every stage before Execute, on both paths --------------------------------------

let private oneTick = Some(TimeSpan.FromTicks 1L)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``SourceCompile_LimitReachedDuringParse_FailsWithTheStageParse`` () =
    let mark = marker ()
    let o = { pinned with CompilationTimeout = oneTick; Functions = [ mark.AsFunction ] }
    let r = Script.runWith o ("Mark(1)\n" + manyStatements 20_000)
    fails MessageCode.CompilationTimeLimitExceeded r
    stageOfCode MessageCode.CompilationTimeLimitExceeded "Parse" r
    Assert.False(r.HasValue)
    Assert.Equal(0, mark.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``SourceCompileWithALibrary_LimitReachedDuringParse_FailsWithTheStageParse`` () =
    let lib = lines ([ "func L()" ] @ repeatLines 5_000 "  var a{i} = {i}" @ [ "  return 0"; "end" ])
    let r = Script.compileWith { pinned with CompilationTimeout = oneTick; Libraries = [ lib ] } "Output.x = L()"
    fails MessageCode.CompilationTimeLimitExceeded r
    stageOfCode MessageCode.CompilationTimeLimitExceeded "Parse" r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``ArtifactLoad_LimitReachedDuringDeserialize_FailsWithTheStageDeserialize`` () =
    // The book's example: an artifact whose Deserialize outlasts the limit.
    let bytes = Script.compileBytesWith pinned (bigSource 160) |> Script.valueOf
    let r = Script.loadBytesWith { pinned with CompilationTimeout = oneTick } (bytes)
    fails MessageCode.CompilationTimeLimitExceeded r
    stageOfCode MessageCode.CompilationTimeLimitExceeded "Deserialize" r
    Assert.False(r.HasValue)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``Execute_IsNotBoundedByTheCompilationLimit`` () =
    // A 2-second compile limit, then a run of about 3 seconds under a 30-second
    // execution limit: the limit bounds only the stages before Execute (100.9.3).
    let st = stall ()
    let o = { pinned with CompilationTimeout = Some(TimeSpan.FromSeconds 2.); Functions = [ st.AsFunction ] }
    let source = "Output.a = 1\nStall(3000)\nOutput.b = 2"
    let fromSource = Script.runWith o source
    Assert.DoesNotContain(MessageCode.CompilationTimeLimitExceeded, Script.codes fromSource)
    Assert.Equal("{\"a\":1,\"b\":2}", Values.toJson (ok fromSource))
    let bytes = Script.compileBytesWith { pinned with Functions = [ st.AsFunction ] } source |> Script.valueOf
    let fromArtifact = Script.runBytesWith o bytes
    Assert.DoesNotContain(MessageCode.CompilationTimeLimitExceeded, Script.codes fromArtifact)
    Assert.Equal("{\"a\":1,\"b\":2}", Values.toJson (ok fromArtifact))

// -- JYRO-DEF-458, JYRO-DEF-463: the stage that was running when the limit was reached ------------------
//
// JYRO-DEF-458: CompilationTimeLimitExceeded carried no stage, and nothing recorded
// which stage was running when the limit was reached; the tests above reach the
// limit only in the first stage (Parse, Deserialize). These reach it in a later
// stage, on CompileToArtifact, and check that a limit never names a stage that a
// path does not have. JYRO-DEF-463: 100.9.3's stage is read through
// DiagnosticMessage.Stage (JYRO-REQ-2105), now in this work order with it.
//
// The stage is read here by the property itself, not through Pending, so that a
// build without it fails these tests plainly rather than as blocked.

/// The name of `DiagnosticMessage.Stage` of `m`, failing the test when the
/// property is not in the build.
let private stageName (m: DiagnosticMessage) : string =
    let p = typeof<DiagnosticMessage>.GetProperty("Stage", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)
    if isNull p then Values.fail "DiagnosticMessage.Stage is not in this build, so the stage of CompilationTimeLimitExceeded cannot be read"
    match p.GetValue m with
    | null -> Values.fail "DiagnosticMessage.Stage returned null"
    | v -> v.ToString()

/// Assert `r` failed with exactly one error, CompilationTimeLimitExceeded, of
/// the stage `stage`, and has no value.
let private limitReachedIn (stage: string) (r: JyroResult<'T>) : unit =
    fails MessageCode.CompilationTimeLimitExceeded r
    Assert.False(r.HasValue)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.CompilationTimeLimitExceeded)
    Assert.Equal(stage, stageName m)

/// A source whose Compile stage takes far longer than its Parse, Validate and
/// Link together (about 1.2 s against about 0.15 s, measured at 0df2471 in a
/// Debug build): 3,001 functions, each calling the one before.
let private compileHeavySource : string =
    lines (
        [ "func F0()\n  return 0\nend" ]
        @ [ for i in 1 .. 3000 -> sprintf "func F%d()\n  return F%d() + 1\nend" i (i - 1) ]
        @ [ "Output.x = F3000()" ])

/// A limit that falls inside a stage: from three compiles under a generous
/// limit with statistics (110.1.1), the latest end of the stages before it
/// (`before`) and the earliest end of the stage itself (`through`); the limit
/// is the midpoint. Fails when the window is under 400 ms, which would make the
/// test depend on the machine rather than on the build.
let private limitInside (compile: RunOptions -> unit) (before: JyroPipelineStats -> TimeSpan) (through: JyroPipelineStats -> TimeSpan) : TimeSpan =
    compile pinned // warm up
    let samples =
        [ for _ in 1 .. 3 ->
            let o, s = withStats pinned
            compile o
            before s, through s ]
    let latestBefore = samples |> List.map fst |> List.max
    let earliestThrough = samples |> List.map snd |> List.min
    let window = earliestThrough - latestBefore
    if window < TimeSpan.FromMilliseconds 400. then
        Values.fail (sprintf "No room for a limit inside the stage: it starts by %O and ends from %O" latestBefore earliestThrough)
    latestBefore + TimeSpan.FromTicks(window.Ticks / 2L)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``JYRO-DEF-458 a limit reached while a source's Compile stage runs fails with the stage Compile`` () =
    let compile o = Script.compileWith o compileHeavySource |> Script.valueOf |> ignore
    let limit =
        limitInside compile (fun s -> s.ParseTime + s.ValidateTime + s.LinkTime) (fun s -> s.ParseTime + s.ValidateTime + s.LinkTime + s.CompileTime)
    limitReachedIn "Compile" (Script.compileWith { pinned with CompilationTimeout = Some limit } compileHeavySource)
    let o, _ = withStats { pinned with CompilationTimeout = Some limit }
    limitReachedIn "Compile" (Script.compileWith o compileHeavySource)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``JYRO-DEF-458 a limit reached while an artifact's Compile stage runs fails with the stage Compile`` () =
    let bytes = Script.compileBytesWith pinned compileHeavySource |> Script.valueOf
    let load o = Script.loadBytesWith o (bytes) |> Script.valueOf |> ignore
    let limit =
        limitInside load (fun s -> s.DeserializeTime + s.ValidateTime + s.LinkTime) (fun s -> s.DeserializeTime + s.ValidateTime + s.LinkTime + s.CompileTime)
    limitReachedIn "Compile" (Script.loadBytesWith { pinned with CompilationTimeout = Some limit } (bytes))
    let o, _ = withStats { pinned with CompilationTimeout = Some limit }
    limitReachedIn "Compile" (Script.loadBytesWith o (bytes))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``JYRO-DEF-458 CompileToArtifact with a 1-tick limit fails with the stage Parse`` () =
    limitReachedIn "Parse" (Script.compileBytesWith { pinned with CompilationTimeout = oneTick } (manyStatements 9_000))
    let o, _ = withStats { pinned with CompilationTimeout = oneTick }
    limitReachedIn "Parse" (Script.compileBytesWith o (manyStatements 9_000))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``JYRO-DEF-458 a limit reached at any point names a stage of the path that was compiling`` () =
    // Limits from 1 tick to 1 s: whichever stage each reaches, a source names one
    // of Parse, Validate, Link and Compile, and an artifact one of Deserialize,
    // Validate, Link and Compile (100.9.3); never Execute, nor the other path's first stage.
    let bytes = Script.compileBytesWith pinned compileHeavySource |> Script.valueOf
    let limits = [ TimeSpan.FromTicks 1L ] @ [ for ms in [ 1.; 5.; 20.; 50.; 100.; 200.; 400.; 700.; 1000. ] -> TimeSpan.FromMilliseconds ms ]
    let check (allowed: string list) (r: JyroResult<CompiledProgram>) =
        if not r.IsSuccess then
            fails MessageCode.CompilationTimeLimitExceeded r
            let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.CompilationTimeLimitExceeded)
            Assert.Contains(stageName m, allowed)
    for limit in limits do
        let o = { pinned with CompilationTimeout = Some limit }
        check [ "Parse"; "Validate"; "Link"; "Compile" ] (Script.compileWith o compileHeavySource)
        check [ "Deserialize"; "Validate"; "Link"; "Compile" ] (Script.loadBytesWith o (bytes))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2174")>]
let ``JYRO-DEF-463 the stage of CompilationTimeLimitExceeded is read through DiagnosticMessage.Stage on both paths`` () =
    // 100.9.3's own example (an artifact whose Deserialize outlasts the limit), and
    // the source path, read by the member of 160.7.5 as a value of MessageStage (160.7.4).
    let source = Script.compileWith { pinned with CompilationTimeout = oneTick } (manyStatements 20_000)
    limitReachedIn "Parse" source
    let bytes = Script.compileBytesWith pinned (bigSource 160) |> Script.valueOf
    let artifact = Script.loadBytesWith { pinned with CompilationTimeout = oneTick } (bytes)
    limitReachedIn "Deserialize" artifact
    let p = typeof<DiagnosticMessage>.GetProperty("Stage")
    Assert.Equal("RealTree.Jyro.MessageStage", p.PropertyType.FullName)

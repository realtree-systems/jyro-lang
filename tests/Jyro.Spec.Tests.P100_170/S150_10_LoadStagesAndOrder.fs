/// 150.1.5 (a load runs Deserialize, then Link, then Compile) and 150.2.9 (the
/// checks of a load, in order). Artifacts come from CompileToArtifact or from the
/// book's codec (Artifact). Every load sets a generous compilation limit and every
/// run the four execution limits (`pinned`, FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S150_10_LoadStagesAndOrder

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

let private maxSize = 10_485_760

/// Load `bytes` under `o` with fresh statistics.
let private loadWithStats (o: RunOptions) (bytes: byte[]) : JyroResult<CompiledProgram> * JyroPipelineStats =
    let o, s = withStats o
    Script.loadBytesWith o (bytes), s

/// Load `bytes` under `o` with fresh statistics, through `JyroArtifactLoader.Load()` (JYRO-REQ-2066).
let private loaderLoadWithStats (o: RunOptions) (bytes: byte[]) : JyroResult<CompiledProgram> * JyroPipelineStats =
    let o, s = withStats o
    Loader.loadWith o bytes, s

/// `JyroArtifactLoader.Load()` of `bytes` under the pinned options (JYRO-REQ-2066).
let private loaderLoadOnly (bytes: byte[]) : JyroResult<CompiledProgram> = Loader.loadWith pinned bytes

/// Host functions whose effect is above Pure, registered by the loading host.
let private heavy (name: string) = (Host.recorderWithEffect name FunctionEffect.Mutating).AsFunction

let private valid () = build [ setOut "x" (num 1m) ]

let private lib = "func L()\n  return 1\nend"

// -- 150.1.5 The stages of a load ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2061")>]
let ``Load_TheBookExample_UnregisteredNope`` () =
    let nope = Host.recorder "Nope"
    let bytes = Script.compileBytesWith { pinned with Functions = [ nope.AsFunction ] } "Nope(1)" |> Script.valueOf
    let r, s = loadWithStats pinned bytes
    fails MessageCode.UndefinedFunction r
    stageOfCode MessageCode.UndefinedFunction "Link" r
    Assert.True(s.DeserializeTime > TimeSpan.Zero, "Deserialize is zero")
    Assert.Equal(TimeSpan.Zero, s.ParseTime)
    Assert.Equal(TimeSpan.Zero, s.CompileTime)
    Assert.Equal(TimeSpan.Zero, s.ExecuteTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2061")>]
let ``Load_LinkTime_IsAboveZeroWhenTheClockShowsIt`` () =
    // 1,000 unregistered names make the Link work visible at the clock's resolution.
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ for i in 0 .. 999 -> sprintf "Nope%04d" i ] }
    let r, s = loadWithStats pinned bytes
    Assert.False(r.IsSuccess)
    stageOfCode MessageCode.UndefinedFunction "Link" r
    Assert.True(s.LinkTime > TimeSpan.Zero, "Link is zero")
    Assert.Equal(TimeSpan.Zero, s.ParseTime)
    Assert.Equal(TimeSpan.Zero, s.CompileTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2061")>]
let ``Load_Successful_ParseIsZero_AndTheProgramRunsAsFromSource`` () =
    let source = manyStatements 800 + "\nOutput.t = Length([1, 2])"
    let bytes = Script.compileBytesWith pinned source |> Script.valueOf
    let o, s = withStats pinned
    let fromArtifact = Script.runBytesWith o bytes
    Assert.True(s.DeserializeTime > TimeSpan.Zero, "Deserialize is zero")
    Assert.Equal(TimeSpan.Zero, s.ParseTime)
    Assert.True(s.ExecuteTime > TimeSpan.Zero, "Execute is zero")
    Assert.Equal(Values.toJson (ok (Script.runWith pinned source)), Values.toJson (ok fromArtifact))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2061")>]
let ``Load_FailingInDeserialize_LinkCompileAndExecuteAreZero`` () =
    let o, s = withStats pinned
    let r = Script.runBytesWith o (Array.append (valid ()) [| 0uy |])
    fails MessageCode.ArtifactUnreadable r
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r
    for name, t in [ "Parse", s.ParseTime; "Link", s.LinkTime; "Compile", s.CompileTime; "Execute", s.ExecuteTime ] do
        Assert.True((t = TimeSpan.Zero), sprintf "%s is %O after a Deserialize failure" name t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2061")>]
let ``Load_FailingInLink_CompileAndExecuteAreZero`` () =
    let cases =
        [ MessageCode.UndefinedFunction, pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Nope" ] }
          MessageCode.ArtifactEffectExceedsMax,
          { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ heavy "Heavy" ] },
          encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Heavy" ] } ]
    for code, o, bytes in cases do
        let o, s = withStats o
        let r = Script.runBytesWith o bytes
        fails code r
        stageOfCode code "Link" r
        Assert.Equal(TimeSpan.Zero, s.ParseTime)
        Assert.Equal(TimeSpan.Zero, s.CompileTime)
        Assert.Equal(TimeSpan.Zero, s.ExecuteTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2061")>]
let ``SourceCompile_DeserializeIsZero`` () =
    let o, s = withStats pinned
    Script.runWith o "Output.x = Length([1])" |> ok |> ignore
    Assert.Equal(TimeSpan.Zero, s.DeserializeTime)

// -- 150.2.9 The checks of a load, in order ------------------------------------------------------------------

/// One row of 150.2.9 failing alone: the options, the bytes, the code and the stage.
let private eachRow () : (string * RunOptions * byte[] * MessageCode * string) list =
    let v = valid ()
    // Row 1 cannot arise in the .NET host API: JyroArtifactLoader has no member that adds a library
    // script (150.2.9, JYRO-ADR-199 item 2), and the number 7006 of ArtifactCannotUseLibraries stays assigned.
    [ "2-too-large", pinned, Array.append v (Array.zeroCreate (maxSize + 1 - v.Length)), MessageCode.ArtifactTooLarge, "Deserialize"
      "3-magic", pinned, Array.append "JYRZ"B v.[4..], MessageCode.ArtifactInvalidMagic, "Deserialize"
      // Row 4: every version other than 1, which this reader alone reads (150.10.2, 150.10.3, 150.10.5; JYRO-REQ-1921).
      "4-version-0", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Version = 0us }, MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "4-version-2", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Version = 2us }, MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "4-version-9", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Version = 9us }, MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "4-version-10", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Version = 10us }, MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "4-version-65535", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Version = 65535us }, MessageCode.ArtifactVersionUnsupported, "Deserialize"
      "5-count-1001", pinned, encode { artifactOf [] with Functions = [ for i in 0 .. 1000 -> sprintf "F%04d" i ] }, MessageCode.ArtifactUnreadable, "Deserialize"
      "6-tag-0x31", pinned, build [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ], MessageCode.ArtifactUnreadable, "Deserialize"
      "6-duplicate", pinned, encode { artifactOf [] with Functions = [ "Trim"; "Trim" ] }, MessageCode.ArtifactUnreadable, "Deserialize"
      "7-byte-after", pinned, Array.append v [| 0uy |], MessageCode.ArtifactUnreadable, "Deserialize"
      "9-undefined", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Nope" ] }, MessageCode.UndefinedFunction, "Link"
      "10-effect", { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ heavy "Heavy" ] },
        encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Heavy" ] }, MessageCode.ArtifactEffectExceedsMax, "Link" ]

let eachRowNames : obj[] seq = eachRow () |> Seq.map (fun (n, _, _, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof eachRowNames)>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_EachRow_FailingAlone_GivesOneDiagnosticWithItsCodeAndStage`` (row: string) =
    let _, o, bytes, code, stage = eachRow () |> List.find (fun (n, _, _, _, _) -> n = row)
    // The load is JyroArtifactLoader.Load() (JYRO-REQ-1896): it returns its refusal, and does not throw.
    let r = Loader.loadWith o bytes
    fails code r
    stageOfCode code stage r

/// Two faults of different rows: the earlier row alone decides the outcome.
let private pairs () : (string * RunOptions * byte[] * MessageCode) list =
    let oversizedBadMagic = Array.append "JYRZ"B (Array.zeroCreate maxSize)
    let nopeAnd (program: Stmt list) = encode { artifactOf program with Functions = [ "Nope" ] }
    // No pair holds row 1: a load in the .NET host API is given no library script (150.2.9).
    [ "2+3", pinned, oversizedBadMagic, MessageCode.ArtifactTooLarge
      "3+4", pinned, Array.append "JYRZ"B (encode { artifactOf [] with Version = 10us }).[4..], MessageCode.ArtifactInvalidMagic
      "4+5", pinned, encode { artifactOf [] with Version = 7us; Functions = [ for i in 0 .. 1000 -> sprintf "F%04d" i ] }, MessageCode.ArtifactVersionUnsupported
      "4+6", pinned, encode { artifactOf [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ] with Version = 2us }, MessageCode.ArtifactVersionUnsupported
      "4(0)+6", pinned, encode { artifactOf [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ] with Version = 0us }, MessageCode.ArtifactVersionUnsupported
      // The book's example: Nope unregistered and first statement tag 0x31.
      "6+9", pinned, nopeAnd [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ], MessageCode.ArtifactUnreadable
      "7+9", pinned, Array.append (nopeAnd [ setOut "x" (num 1m) ]) [| 0uy |], MessageCode.ArtifactUnreadable
      "6(dup)+9", pinned, encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Nope"; "Nope" ] }, MessageCode.ArtifactUnreadable ]

let pairNames : obj[] seq = pairs () |> Seq.map (fun (n, _, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof pairNames)>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_TwoFaultsOfDifferentRows_TheEarlierRowAloneDecides`` (pair: string) =
    let _, o, bytes, code = pairs () |> List.find (fun (n, _, _, _) -> n = pair)
    let r = Loader.loadWith o bytes
    fails code r
    stageOfCode code "Deserialize" r
    Assert.DoesNotContain(MessageCode.UndefinedFunction, Script.codes r)

[<Theory>]
[<InlineData(4)>]
[<InlineData(5)>]
[<InlineData(6)>]
[<InlineData(8)>]
[<InlineData(40)>]
[<InlineData(43)>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_HeaderEndingInOrBeforeTheVersionOrCount_IsArtifactUnreadable`` (length: int) =
    // Rows 4 and 5 do not fail when their bytes are absent; row 6 refuses such bytes.
    // The book's examples are 4 bytes (JYRX) and 5 bytes (JYRX 08).
    let bytes = (valid ()).[0 .. length - 1]
    Assert.Equal<byte[]>("JYRX"B, bytes.[0..3])
    let r = loaderLoadOnly bytes
    fails MessageCode.ArtifactUnreadable r
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_TheBookExamples_JYRX_And_JYRX08`` () =
    for bytes in [ "JYRX"B; Array.append "JYRX"B [| 8uy |] ] do
        let r = loaderLoadOnly bytes
        fails MessageCode.ArtifactUnreadable r
        stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r
        // JyroArtifactInfo.Read gives the same code (150.2.8).
        fails MessageCode.ArtifactUnreadable (JyroArtifactInfo.Read bytes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_Row9_OneDiagnosticPerUnresolvedNameInTableOrder_ThenNoRow10`` () =
    // Zed, Nope and Alpha are unregistered; Heavy is registered with an effect above the maximum.
    let o = { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ heavy "Heavy" ] }
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Zed"; "Heavy"; "Nope"; "Alpha" ] }
    let r = Loader.loadWith o bytes
    Assert.False(r.IsSuccess)
    Assert.Equal<MessageCode list>(List.replicate 3 MessageCode.UndefinedFunction, Script.errorCodes r)
    stageOfCode MessageCode.UndefinedFunction "Link" r
    // The name is what changes between occurrences, so it is an argument (130.6.2); the
    // order of the diagnostics is the table's order (150.2.9 row 9).
    let named =
        [ for m in r.Messages -> m.Args |> Array.map string |> Array.tryFind (fun a -> List.contains a [ "Zed"; "Nope"; "Alpha" ]) ]
    Assert.Equal<string option list>([ Some "Zed"; Some "Nope"; Some "Alpha" ], named)
    Assert.DoesNotContain(MessageCode.ArtifactEffectExceedsMax, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_Row10_OneDiagnosticPerFunctionInTableOrder`` () =
    let o = { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ heavy "HeavyB"; heavy "HeavyA"; (Host.recorder "Light").AsFunction ] }
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "HeavyB"; "Light"; "HeavyA" ] }
    let r = Loader.loadWith o bytes
    Assert.False(r.IsSuccess)
    Assert.Equal<MessageCode list>(List.replicate 2 MessageCode.ArtifactEffectExceedsMax, Script.errorCodes r)
    stageOfCode MessageCode.ArtifactEffectExceedsMax "Link" r
    // The function's name is an argument (130.6.2); the order is the table's (150.2.9 row 10).
    let named =
        [ for m in r.Messages -> m.Args |> Array.map string |> Array.tryFind (fun a -> a = "HeavyA" || a = "HeavyB") ]
    Assert.Equal<string option list>([ Some "HeavyB"; Some "HeavyA" ], named)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_Row8_WhenTheImplementationValidatesOnLoad_GivesTheSourceDiagnosticsAndNoLink`` () =
    // 150.2.6 is a MAY. An implementation that validates on load gives the validation
    // diagnostics of the equivalent source and does not start Link; one that does not
    // goes from row 7 to row 9, which gives UndefinedFunction (150.2.9).
    let cases =
        [ [ SBreak p0 ], "break"
          [ SBreak p0; SContinue p0 ], "break\ncontinue" ]
    for program, source in cases do
        let bytes = encode { artifactOf program with Functions = [ "Nope" ] }
        let r, s = loaderLoadWithStats pinned bytes
        let codes = Script.errorCodes r
        Assert.False(r.IsSuccess)
        if codes = [ MessageCode.UndefinedFunction ] then
            stageOfCode MessageCode.UndefinedFunction "Link" r
            Assert.Equal(TimeSpan.Zero, s.ValidateTime)
        else
            let fromSource = Script.compileWith pinned source
            Assert.Equal<MessageCode list>(Script.errorCodes fromSource, codes)
            Assert.DoesNotContain(MessageCode.UndefinedFunction, Script.codes r)
            for m in r.Messages do Assert.Equal("Validate", Pending.stageOf m)
            Assert.Equal(TimeSpan.Zero, s.LinkTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2066")>]
let ``Load_LinkStartsOnlyAfterTheLastByte`` () =
    // Nope unregistered, and the last byte of the program missing: only ArtifactUnreadable.
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Nope" ] }
    let r = loaderLoadOnly bytes.[0 .. bytes.Length - 2]
    fails MessageCode.ArtifactUnreadable r
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r
    Assert.DoesNotContain(MessageCode.UndefinedFunction, Script.codes r)

/// 150.2: every refusal of a load is a diagnostic about the artifact, never an internal error
/// (JYRO-REQ-2137). Written from the book alone (FLOORS 4.1.1).
///
/// A byte sequence made from a valid artifact by changing one byte, or by cutting it short, is
/// given to `JyroArtifactLoader.Load()` (JYRO-REQ-1896, reached through Loader.fs, which fails a test
/// as FLOORS-BLOCKED on JYRO-REQ-1896 when the build has no loader). The call MUST return, and a
/// refusal MUST be an Error diagnostic whose code is not one of the four internal error codes of
/// 130.1.4. Every load sets its compilation time limit explicitly (100.9.2, FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S150_12_RefusalsOfALoad

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// The four internal error codes of 130.1.4.
let private internalCodes : MessageCode list =
    [ MessageCode.UnknownParserError; MessageCode.RuntimeError; MessageCode.UnknownCompilerError; MessageCode.InternalStageFailure ]

/// The host the artifact is written and loaded by: one registered function, beside the standard library.
let private options () : RunOptions = { pinned with Functions = [ (Host.recorder "Mark").AsFunction ] }

/// A valid artifact that holds a function table, a user function, several statement kinds and
/// several value kinds.
let private validSource : string =
    lines [ "var a = [1, 2.5, \"three\", true, null, {k: [1, 2]}]"
            "func F(x)"
            "  var t = x * 2"
            "  if t > 3 then"
            "    return t"
            "  end"
            "  return 0"
            "end"
            "Mark(1)"
            "var n = 0"
            "foreach v in a do"
            "  n++"
            "end"
            "for i in 0 to 3 do"
            "  n = n + i"
            "end"
            "switch n do case 9 then Output.k = \"nine\" end"
            "Output.r = F(Length(a))"
            "Output.s = ToJson(a)" ]

let private validBytes () : byte[] =
    let r = Script.compileBytesWith (options ()) validSource
    if not r.IsSuccess then Values.fail ("The valid source did not compile to an artifact. " + Script.describe r)
    Script.valueOf r

/// What loading `bytes` gave: null when it loaded or was refused as the clause requires, and
/// otherwise what went wrong.
let private problemWith (bytes: byte[]) : string option =
    let outcome =
        try Choice1Of2 (Loader.loadWith (options ()) (bytes))
        with e -> Choice2Of2 e
    match outcome with
    | Choice2Of2 e -> Some (sprintf "Compile() threw %s: %s" (e.GetType().FullName) e.Message)
    | Choice1Of2 r ->
        let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        let internals = errors |> List.filter (fun m -> List.contains m.Code internalCodes)
        if r.IsSuccess then None
        elif errors.IsEmpty then Some (sprintf "refused with no Error diagnostic. %s" (Script.describe r))
        elif not internals.IsEmpty then Some (sprintf "refused with an internal error. %s" (Script.describe r))
        else None

let private assertNoProblems (what: string) (problems: (string * string) list) =
    if not problems.IsEmpty then
        let shown = problems |> List.truncate 10 |> List.map (fun (where, p) -> sprintf "  %s: %s" where p)
        Values.fail (sprintf "%s: %d of the byte sequences were not loaded or refused as JYRO-REQ-2137 requires. The first:\n%s"
                         what problems.Length (String.Join("\n", shown)))

/// The values a byte is changed to: a spread, never the byte itself.
let private replacements (b: byte) : byte list =
    [ 0x00uy; 0x01uy; 0x02uy; 0x7Fuy; 0x80uy; 0xFFuy; b ^^^ 0x01uy; b ^^^ 0x80uy; b + 1uy; b - 1uy ]
    |> List.distinct
    |> List.filter (fun v -> v <> b)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 The valid artifact loads`` () =
    let r = Loader.loadWith (options ()) (validBytes ())
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 Every artifact cut short at any length loads or is refused with a diagnostic, never an internal error or a throw`` () =
    let bytes = validBytes ()
    let problems =
        bounded 300. (fun () ->
            [ for n in 0 .. bytes.Length - 1 do
                match problemWith bytes.[0 .. n - 1] with
                | Some p -> yield sprintf "cut at %d" n, p
                | None -> () ])
    assertNoProblems "Cutting the artifact short" problems

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 Every artifact with one byte changed to another value loads or is refused with a diagnostic, never an internal error or a throw`` () =
    let bytes = validBytes ()
    let problems =
        bounded 600. (fun () ->
            [ for i in 0 .. bytes.Length - 1 do
                for v in replacements bytes.[i] do
                    let changed = Array.copy bytes
                    changed.[i] <- v
                    match problemWith changed with
                    | Some p -> yield sprintf "byte %d to 0x%02X" i v, p
                    | None -> () ])
    assertNoProblems "Changing one byte of the artifact" problems

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 Every byte of the artifact changed to every other value loads or is refused with a diagnostic, in the header`` () =
    // The header is where a refusal is most likely to be reached by a path nobody wrote a diagnostic for:
    // every value of each of its first bytes, not a spread.
    let bytes = validBytes ()
    let problems =
        bounded 300. (fun () ->
            [ for i in 0 .. (min 24 bytes.Length) - 1 do
                for v in 0 .. 255 do
                    if byte v <> bytes.[i] then
                        let changed = Array.copy bytes
                        changed.[i] <- byte v
                        match problemWith changed with
                        | Some p -> yield sprintf "byte %d to 0x%02X" i v, p
                        | None -> () ])
    assertNoProblems "Changing a byte of the header" problems

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 A refusal of an artifact cut short is an Error diagnostic, and none is an internal error`` () =
    let bytes = validBytes ()
    let codes = ResizeArray<MessageCode>()
    for n in 0 .. bytes.Length - 1 do
        let r = Loader.loadWith (options ()) (bytes.[0 .. n - 1])
        for m in r.Messages do
            if m.Severity = MessageSeverity.Error then codes.Add m.Code
    Assert.NotEmpty codes
    for c in codes do
        Assert.DoesNotContain(c, internalCodes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 A load refused because a function has no usable effect has the code MaxEffectNotApplicable, not an internal error`` () =
    let written = Script.compileBytesWith { pinned with Functions = [ (Host.recorder "Broken").AsFunction ] } "Output.r = Broken()" |> Script.valueOf
    for kind in Tests.CallCheck.effectFaultKinds do
        let loader = { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", kind) :> IJyroFunction ] }
        let r =
            try Loader.loadWith loader written
            with e -> Values.fail (sprintf "Effect %s: Compile() threw %s: %s" kind (e.GetType().FullName) e.Message)
        Assert.False(r.IsSuccess, sprintf "Effect %s: %s" kind (Script.describe r))
        Assert.NotEmpty(Tests.CallCheck.ofCode Tests.CallCheck.effectMaxNotApplicable r)
        for c in internalCodes do
            Assert.Empty(Tests.CallCheck.ofCode c r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 A load that reaches the compilation time limit has the code CompilationTimeLimitExceeded, not an internal error`` () =
    let written = Script.compileBytesWith pinned (manyStatements 9_000) |> Script.valueOf
    let r =
        try Loader.loadWith { pinned with CompilationTimeout = Some(TimeSpan.FromTicks 1L) } written
        with e -> Values.fail (sprintf "Compile() threw %s: %s" (e.GetType().FullName) e.Message)
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.NotEmpty(Tests.CallCheck.ofCode MessageCode.CompilationTimeLimitExceeded r)
    for c in internalCodes do
        Assert.Empty(Tests.CallCheck.ofCode c r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 A load that a validation rule refuses has the code of the rule, not an internal error`` () =
    // The artifact of a script that calls a function the loading host has not registered: UndefinedFunction (150.2.3).
    let written = Script.compileBytesWith (options ()) "Mark(1)\nOutput.r = 1" |> Script.valueOf
    let r = Loader.loadWith pinned written
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.Contains(MessageCode.UndefinedFunction, Script.codes r)
    for c in internalCodes do
        Assert.Empty(Tests.CallCheck.ofCode c r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2137")>]
let ``JYRO-REQ-2137 A load refused because a function is above the maximum effect has the code ArtifactEffectExceedsMax, not an internal error`` () =
    // 150.2.9 row 10: a function of the table above the maximum effect of the loading host.
    let heavy = (Host.recorderWithEffect "Heavy" FunctionEffect.Mutating).AsFunction
    let written = Script.compileBytesWith { pinned with Functions = [ heavy ] } "Output.r = Heavy()" |> Script.valueOf
    let r = Loader.loadWith { pinned with MaxEffect = Some FunctionEffect.Pure; Functions = [ heavy ] } written
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.NotEmpty(Tests.CallCheck.ofCode MessageCode.ArtifactEffectExceedsMax r)
    for c in internalCodes do
        Assert.Empty(Tests.CallCheck.ofCode c r)

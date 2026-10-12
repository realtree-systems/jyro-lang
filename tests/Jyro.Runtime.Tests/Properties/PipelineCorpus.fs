namespace RealTree.Jyro.Tests.Runtime

open System.IO
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// End-to-end pipeline tests driven by the plaintext corpus under
/// [tests/plaintext/]. Each `.jyro` script is executed through the real
/// `JyroCompiler`/`JyroRunner` pipeline with an optional `D-<name>.json` as `Input`
/// and the resulting `Output` compared against `O-<name>.json`.
///
/// Error-path tests are marked with an `E-<name>.txt` (exit code, legacy
/// convention; any non-zero means "script should fail") and, when
/// tightened, a sibling `EC-<name>.txt` naming the specific `MessageCode`
/// the script is expected to emit. When the EC- file is present we
/// assert the exact code; when absent we fall back to "any failure".
///
/// Replaces the PowerShell-driven `Run-Tests.ps1` with in-process xunit
/// execution: ~100× faster than per-test CLI spawning and fully
/// integrated with CI reporting.
module PipelineCorpus =

    let private plaintextDir : string =
        let asmDir =
            Path.GetDirectoryName(
                System.Reflection.Assembly.GetExecutingAssembly().Location)
        let repoRoot =
            [ ".."; ".."; ".."; ".."; ".." ]
            |> List.fold (fun p seg -> Path.Combine(p, seg)) asmDir
            |> Path.GetFullPath
        Path.Combine(repoRoot, "tests", "plaintext")

    /// Categories intentionally skipped by this in-process runner. Empty
    /// now that every corpus category runs end-to-end here.
    let private excludedCategories : Set<string> = Set.empty

    /// Parse a `MessageCode` name (e.g. "ScriptFailure") via reflection.
    /// Kept forgiving: unknown names fail the specific assertion with a
    /// clear message rather than crashing the test framework.
    let private tryParseMessageCode (name: string) : MessageCode option =
        match System.Enum.TryParse<MessageCode>(name.Trim(), true) with
        | true, code -> Some code
        | _ -> None

    /// Discover every `.jyro` file with a matching `O-*.json`. Yields a
    /// tuple carrying the display name and the four sibling paths;
    /// missing D-/E-/EC- files are null.
    let pipelineTriples : obj[] seq =
        if not (Directory.Exists(plaintextDir)) then
            Seq.empty
        else
            seq {
                for script in Directory.EnumerateFiles(plaintextDir, "*.jyro", SearchOption.AllDirectories) do
                    let folder = Path.GetDirectoryName(script)
                    let category = Path.GetFileName(folder)
                    if not (excludedCategories.Contains(category)) then
                        let baseName = Path.GetFileNameWithoutExtension(script)
                        let expected = Path.Combine(folder, "O-" + baseName + ".json")
                        if File.Exists(expected) then
                            let dataFile = Path.Combine(folder, "D-" + baseName + ".json")
                            let exitFile = Path.Combine(folder, "E-" + baseName + ".txt")
                            let codeFile = Path.Combine(folder, "EC-" + baseName + ".txt")
                            let dataPath =
                                if File.Exists(dataFile) then box dataFile else null
                            let codePath =
                                if File.Exists(codeFile) then box codeFile else null
                            let expectedExit =
                                if File.Exists(exitFile) then
                                    match System.Int32.TryParse((File.ReadAllText(exitFile)).Trim()) with
                                    | true, n -> n
                                    | _ -> 0
                                else 0
                            let displayName =
                                Path.GetRelativePath(plaintextDir, script).Replace('\\', '/')
                            yield [| box displayName
                                     box script
                                     box dataPath
                                     box expected
                                     box expectedExit
                                     box codePath |]
            }

    // -- Structural JSON equality --------------------------------------------

    let private floatEq (a: float) (b: float) =
        if System.Double.IsNaN(a) && System.Double.IsNaN(b) then true
        elif System.Double.IsInfinity(a) || System.Double.IsInfinity(b) then a = b
        else
            let diff = abs (a - b)
            let scale = max 1.0 (max (abs a) (abs b))
            diff <= scale * 1e-12

    let rec private jsonEqual (a: JsonElement) (b: JsonElement) : bool =
        if a.ValueKind <> b.ValueKind then false
        else
            match a.ValueKind with
            | JsonValueKind.Object ->
                let aProps = a.EnumerateObject() |> Seq.map (fun p -> p.Name, p.Value) |> Map.ofSeq
                let bProps = b.EnumerateObject() |> Seq.map (fun p -> p.Name, p.Value) |> Map.ofSeq
                Map.count aProps = Map.count bProps
                && Map.forall (fun k v ->
                    match Map.tryFind k bProps with
                    | Some bv -> jsonEqual v bv
                    | None -> false) aProps
            | JsonValueKind.Array ->
                let aList = a.EnumerateArray() |> List.ofSeq
                let bList = b.EnumerateArray() |> List.ofSeq
                List.length aList = List.length bList
                && List.forall2 jsonEqual aList bList
            | JsonValueKind.Number -> floatEq (a.GetDouble()) (b.GetDouble())
            | JsonValueKind.String -> a.GetString() = b.GetString()
            | JsonValueKind.True
            | JsonValueKind.False -> a.GetBoolean() = b.GetBoolean()
            | JsonValueKind.Null -> true
            | _ -> false

    let private parseJson (s: string) = (JsonDocument.Parse(s)).RootElement

    // -- The test itself ------------------------------------------------------

    [<Theory>]
    [<MemberData(nameof pipelineTriples)>]
    let ``plaintext corpus runs end-to-end`` (displayName: string) (scriptPath: string) (dataPath: obj) (expectedPath: string) (expectedExit: int) (expectedCodePath: obj) =
        let source = File.ReadAllText(scriptPath)

        let input : JyroValue =
            match dataPath with
            | null -> JyroObject() :> JyroValue
            | :? string as p -> JyroValue.FromJson(File.ReadAllText(p))
            | _ -> JyroObject() :> JyroValue

        let result = Runner.run source input

        if expectedExit <> 0 then
            Assert.False(
                result.IsSuccess,
                sprintf
                    "%s: expected failure (exit code %d), but script succeeded"
                    displayName expectedExit)

            // An EC- file is REQUIRED whenever an E- file is present.
            // This is a ratchet: every error-path test names the exact
            // MessageCode it expects, so any diagnostic-code drift fails
            // loudly. Adding a new error script? Drop `EC-<name>.txt`
            // alongside `E-<name>.txt` with one line naming the
            // `MessageCode`.
            match expectedCodePath with
            | :? string as ecPath ->
                let expectedName = (File.ReadAllText(ecPath)).Trim()
                match tryParseMessageCode expectedName with
                | None ->
                    Assert.Fail(
                        sprintf
                            "%s: EC- file references unknown MessageCode %A"
                            displayName expectedName)
                | Some expectedCode ->
                    let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
                    Assert.True(
                        List.contains expectedCode codes,
                        sprintf
                            "%s: expected MessageCode %A; actual diagnostics: %A"
                            displayName expectedCode codes)
            | _ ->
                Assert.Fail(
                    sprintf
                        "%s: missing EC-<name>.txt. Every E- marker must be paired with an EC- file naming the expected MessageCode."
                        displayName)
        else
            Assert.True(
                result.IsSuccess,
                sprintf
                    "%s: expected success but got failure.\nDiagnostics: %A"
                    displayName
                    (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> (m.Code, m.Message))))

            let actualJson =
                match result.TryValue with
                | Some v -> v.ToJson()
                | None -> "null"
            let expectedJson = File.ReadAllText(expectedPath)
            let actual = parseJson actualJson
            let expected = parseJson expectedJson
            Assert.True(
                jsonEqual actual expected,
                sprintf
                    "%s: output mismatch.\nExpected: %s\nActual:   %s"
                    displayName expectedJson actualJson)

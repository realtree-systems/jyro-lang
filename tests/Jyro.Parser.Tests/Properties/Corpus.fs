namespace RealTree.Jyro.Tests.Properties

open System.IO
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests.ParseShape

/// Replay the plaintext integration corpus ([tests/plaintext/]) as parser
/// regression anchors. Every `.jyro` file in that tree must parse without
/// error - these are hand-curated programs that cover every language
/// feature, so a failure here signals a real regression.
///
/// Exception: the `parse-errors` category holds programs that are *designed*
/// to fail at the parse stage (e.g. unterminated string, out-of-range number
/// literal) to pin their specific diagnostic codes; the end-to-end
/// PipelineCorpus runner asserts those codes. They are skipped here.
module CorpusReplay =

    /// Categories whose `.jyro` files are expected NOT to parse cleanly.
    let private excludedCategories = Set.ofList [ "parse-errors" ]

    /// Walk up from the test-assembly's binary output to the repo's
    /// tests/plaintext directory. Runs in any CI or local config.
    let private plaintextDir : string =
        let asmDir =
            Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
        // bin/Debug/net8.0 -> bin/Debug -> bin -> <testproj> -> tests -> (root)
        let repoRoot =
            [ ".."; ".."; ".."; ".."; ".." ]
            |> List.fold (fun p seg -> Path.Combine(p, seg)) asmDir
            |> Path.GetFullPath
        Path.Combine(repoRoot, "tests", "plaintext")

    /// One xUnit test per discovered `.jyro` file.
    let plaintextCorpusFiles : obj[] seq =
        if Directory.Exists(plaintextDir) then
            Directory.EnumerateFiles(plaintextDir, "*.jyro", SearchOption.AllDirectories)
            |> Seq.filter (fun f ->
                let category = Path.GetFileName(Path.GetDirectoryName(f))
                not (excludedCategories.Contains(category)))
            |> Seq.map (fun f ->
                // Use a repo-relative display name so test output is readable.
                let rel = Path.GetRelativePath(plaintextDir, f).Replace('\\', '/')
                [| box rel; box f |])
        else
            Seq.empty

    [<Theory>]
    [<MemberData(nameof plaintextCorpusFiles)>]
    let ``plaintext corpus files parse without error`` (name: string) (path: string) =
        let src = File.ReadAllText(path)
        match Parser.parse src with
        | ParseSuccess _ -> ()
        | ParseFailure(msg, line, col) ->
            Assert.Fail(sprintf "%s failed to parse at %d:%d: %s" name line col msg)

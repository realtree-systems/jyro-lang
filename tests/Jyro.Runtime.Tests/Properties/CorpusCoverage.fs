namespace RealTree.Jyro.Tests.Runtime

open System.IO
open Xunit
open RealTree.Jyro.Tests

/// Visibility test: asserts every category in the plaintext corpus is
/// represented and produces a per-category breakdown (total / error
/// tests / tightened-with-EC). Fails if any E- marker exists without a
/// matching EC- file - matches the ratchet in `PipelineCorpus` but
/// reports across the whole corpus rather than per-file.
///
/// The breakdown is echoed via `ITestOutputHelper` so CI logs carry a
/// quick inventory of coverage without needing to open the repo.
module CorpusCoverage =

    type private CategoryStats =
        { Category: string
          Scripts: int
          ErrorScripts: int
          TightenedWithEC: int }

    let private plaintextDir : string =
        let asmDir =
            Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
        let repoRoot =
            [ ".."; ".."; ".."; ".."; ".." ]
            |> List.fold (fun p seg -> Path.Combine(p, seg)) asmDir
            |> Path.GetFullPath
        Path.Combine(repoRoot, "tests", "plaintext")

    let private scanCategory (dir: string) : CategoryStats =
        let category = Path.GetFileName(dir)
        let scripts = Directory.EnumerateFiles(dir, "*.jyro") |> List.ofSeq
        let errorScripts =
            scripts
            |> List.filter (fun s ->
                let baseName = Path.GetFileNameWithoutExtension(s)
                File.Exists(Path.Combine(dir, "E-" + baseName + ".txt")))
        let tightened =
            errorScripts
            |> List.filter (fun s ->
                let baseName = Path.GetFileNameWithoutExtension(s)
                File.Exists(Path.Combine(dir, "EC-" + baseName + ".txt")))
        { Category = category
          Scripts = List.length scripts
          ErrorScripts = List.length errorScripts
          TightenedWithEC = List.length tightened }

    type CorpusCoverageFixture(output: Xunit.Abstractions.ITestOutputHelper) =

        [<Fact>]
        member _.``every E- marker has a matching EC- file`` () =
            let untightened = ResizeArray<string>()
            for dir in Directory.EnumerateDirectories(plaintextDir) do
                let category = Path.GetFileName(dir)
                for eFile in Directory.EnumerateFiles(dir, "E-*.txt") do
                    let baseName = Path.GetFileNameWithoutExtension(eFile).Substring(2)
                    let ecFile = Path.Combine(dir, "EC-" + baseName + ".txt")
                    if not (File.Exists(ecFile)) then
                        untightened.Add(sprintf "%s/%s" category baseName)
            Assert.True(
                untightened.Count = 0,
                sprintf
                    "Untightened error-path scripts (need EC-<name>.txt):\n  %s"
                    (System.String.Join("\n  ", untightened)))

        [<Fact>]
        member _.``per-category inventory`` () =
            let stats =
                Directory.EnumerateDirectories(plaintextDir)
                |> Seq.map scanCategory
                |> Seq.sortBy (fun s -> s.Category)
                |> List.ofSeq
            output.WriteLine(
                sprintf
                    "%-24s %8s %8s %10s"
                    "Category" "Scripts" "Error" "Tightened")
            output.WriteLine(String.replicate 54 "-")
            for s in stats do
                output.WriteLine(
                    sprintf
                        "%-24s %8d %8d %10d"
                        s.Category s.Scripts s.ErrorScripts s.TightenedWithEC)
            let totalScripts = stats |> List.sumBy (fun s -> s.Scripts)
            let totalError = stats |> List.sumBy (fun s -> s.ErrorScripts)
            let totalTightened = stats |> List.sumBy (fun s -> s.TightenedWithEC)
            output.WriteLine(String.replicate 54 "-")
            output.WriteLine(
                sprintf "%-24s %8d %8d %10d" "TOTAL" totalScripts totalError totalTightened)
            // Sanity: every error script is tightened (ratchet is green).
            Assert.Equal(totalError, totalTightened)
            // Sanity: there's at least one category (catches a broken plaintextDir).
            Assert.True(totalScripts > 0, "plaintext corpus is empty")

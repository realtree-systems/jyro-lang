/// Part 100.6: the call depth ceiling. Every run that may recurse past the
/// ceiling is in a child process (Child.fs), so a stack overflow shows as a
/// failed test and never kills the test host.
module RealTree.Jyro.Spec.Tests.S100_06_CallDepthCeiling

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private m () = maxSafeCallDepth ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1427")>]
let ``ChildProcess_Harness_Runs`` () =
    Child.run "probe" |> Child.expectSuccess

[<Theory>]
[<InlineData("above")>]
[<InlineData("maxint")>]
[<InlineData("unlimited")>]
[<InlineData("options-m1")>]
[<InlineData("options-maxint")>]
[<Trait("requirement", "JYRO-REQ-1427")>]
let ``LimitAboveTheSafeMaximum_IsReducedToIt_DepthMPasses`` (config: string) =
    Child.run (sprintf "depth|%s|%d" config (m ())) |> Child.expectSuccess

[<Theory>]
[<InlineData("above")>]
[<InlineData("maxint")>]
[<InlineData("unlimited")>]
[<InlineData("options-m1")>]
[<InlineData("options-maxint")>]
[<Trait("requirement", "JYRO-REQ-1427")>]
let ``LimitAboveTheSafeMaximum_IsReducedToIt_DepthMPlus1Fails`` (config: string) =
    Child.run (sprintf "depth|%s|%d" config (m () + 1)) |> Child.expectCode MessageCode.CallDepthLimitExceeded

[<Theory>]
[<InlineData("unlimited")>]
[<InlineData("maxint")>]
[<Trait("requirement", "JYRO-REQ-1427")>]
[<Trait("requirement", "JYRO-REQ-1826")>]
let ``JYRO-REQ-1826 UnlimitedSetting_FarPastTheSafeMaximum_StopsCleanly`` (config: string) =
    Child.run (sprintf "depth|%s|%d" config (m () * 50)) |> Child.expectCode MessageCode.CallDepthLimitExceeded

[<Theory>]
[<InlineData("default")>]
[<InlineData("unlimited")>]
[<InlineData("maxint")>]
[<InlineData("ExecuteScript")>]
[<InlineData("ExecuteScriptWithJson")>]
[<InlineData("bytes-unlimited")>]
[<InlineData("map-unlimited")>]
[<InlineData("host-unlimited")>]
[<Trait("requirement", "JYRO-REQ-1428")>]
let ``EveryExecutionPath_RecursionPastTheSafeMaximum_StopsWithCallDepthLimitExceeded`` (config: string) =
    let depth = if config.EndsWith "unlimited" || config = "maxint" then m () * 20 else m () + 1
    Child.run (sprintf "depth|%s|%d" config depth) |> Child.expectCode MessageCode.CallDepthLimitExceeded

// ---------------------------------------------------------------------------------
// JYRO-DEF-497: merge 5490dd1 took main's side of the Jyro.Api README whole and
// dropped JYRO-ADR-108's paragraph on the 1 MB stack MaxSafeCallDepth assumes.
// The README keeps both: the 1 MB note in its 'Call depth' section, and main's
// 'Maximum nesting depth' section.
// ---------------------------------------------------------------------------------

/// The repository root: the nearest directory above the test assembly that
/// holds Jyro.slnx.
let private repoRoot () : string =
    let rec up (dir: System.IO.DirectoryInfo) =
        if isNull dir then failwith "Jyro.slnx not found above the test assembly"
        elif System.IO.File.Exists(System.IO.Path.Combine(dir.FullName, "Jyro.slnx")) then dir.FullName
        else up dir.Parent
    up (System.IO.DirectoryInfo(System.AppContext.BaseDirectory))

let private apiReadme () : string[] =
    System.IO.File.ReadAllLines(System.IO.Path.Combine(repoRoot (), "src", "Jyro.Api", "README.md"))

let private headingLevel (line: string) : int option =
    let t = line.TrimStart()
    if t.StartsWith "#" then
        let n = t |> Seq.takeWhile ((=) '#') |> Seq.length
        if n < t.Length && t.[n] = ' ' then Some n else None
    else None

/// The lines of the first section whose heading contains `title` (any case),
/// up to the next heading of the same or a higher level. Lines inside code
/// fences are never headings.
let private section (title: string) (doc: string[]) : string list =
    let isFence (l: string) = l.TrimStart().StartsWith "```"
    let marked =
        doc
        |> Array.toList
        |> List.mapFold
            (fun inFence (l: string) ->
                if isFence l then (l, None), not inFence
                elif inFence then (l, None), inFence
                else (l, headingLevel l), inFence)
            false
        |> fst
    match marked |> List.tryFindIndex (fun (l, h) -> h.IsSome && l.IndexOf(title, System.StringComparison.OrdinalIgnoreCase) >= 0) with
    | None -> []
    | Some i ->
        let level = (snd marked.[i]).Value
        marked
        |> List.skip (i + 1)
        |> List.takeWhile (fun (_, h) -> match h with Some n -> n > level | None -> true)
        |> List.map fst

[<Fact>]
let ``JYRO-DEF-497 the Jyro.Api README's Call depth section documents the 1 MB stack MaxSafeCallDepth assumes`` () =
    let body = String.concat "\n" (section "Call depth" (apiReadme ()))
    Assert.False(System.String.IsNullOrWhiteSpace body, "The Jyro.Api README has no 'Call depth' section")
    Assert.Contains("MaxSafeCallDepth", body)
    Assert.True(
        System.Text.RegularExpressions.Regex.IsMatch(body, @"\b1\s*(MB|MiB)\b"),
        "The Jyro.Api README's 'Call depth' section does not state the 1 MB stack MaxSafeCallDepth is measured against")

[<Fact>]
let ``JYRO-DEF-497 the Jyro.Api README keeps main's Maximum nesting depth section beside the 1 MB note`` () =
    let doc = apiReadme ()
    Assert.True(
        doc |> Array.exists (fun l -> (headingLevel l).IsSome && l.IndexOf("Maximum nesting depth", System.StringComparison.OrdinalIgnoreCase) >= 0),
        "The Jyro.Api README has no 'Maximum nesting depth' section")

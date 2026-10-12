/// 100.2.5 (JYRO-REQ-1411): a regex match is a standard library function that iterates over
/// data from the script, so the time limit and the cancellation signal are checked at bounded
/// intervals DURING the match, whatever the length of the pattern or of the text.
///
/// JYRO-DEF-771: steps of a match each run in one stretch whose length grows with the pattern or
/// the text, with the check only before them:
/// - clearing the capture registers at each start position, the reset of the groups of a
///   quantified atom at each iteration, and the copy of the registers when a match succeeds: all
///   three grow with the number of groups of the pattern, and a pattern of 2^20 empty groups inside
///   a starred atom, matched against a text with many matches (case regex-registers), runs each of
///   them for a long stretch at a time;
/// - the comparison of a whole capture of 2^28 code units by a backreference (case
///   regex-backref-compare).
/// The allocation of the registers is not measured (JYRO-DEF-785): it is one step, left to
/// JYRO-DEF-781, and the case keeps it short beside the bound by using 2^20 groups, not 2^22 (at
/// which one match on a slow runner reached the 1 s regex timeout, 90.10.4, and the case could not
/// calibrate). The first case cannot tell the register steps apart: its test passes only when
/// every one of them is checked inside. A single start position, iteration or match is too short
/// to be told from the rest of the run, and one match is limited to 1 s, so the case matches many
/// times.
///
/// A correct run takes seconds, so the limit or the cancel cannot be placed at a fixed time. The
/// child first runs the case to its end (D, Execute alone, after a warm-up), then the limit (mode
/// time) or the cancel (mode cancel) falls at points spread over the part of the run after the
/// pattern has been read. Wherever it falls, the run must stop with the limit's own code, and
/// close behind it: a step that is not checked inside runs on to its end, past the bound. Runs
/// are in a child process under a 3 GB heap, killed after 600 s.
module RealTree.Jyro.Spec.Tests.S100_02_RegexMatchSteps

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.ValueSize

let private childEnv = [ "DOTNET_GCHeapHardLimit", "C0000000" ]

/// The longest a stop may come after the limit or the cancel, in ms.
let private promptly = 25.0

/// How many of the stops may be later than that and the test still pass (JYRO-DEF-785): a
/// garbage collection or a descheduled thread on a shared runner delays one stop by tens of ms,
/// whereas an unchecked step is long enough to delay every stop that falls inside it, and the steps
/// together fill most of the run.
let private noise = 1

/// The case's size (the pairs of "a!" of regex-registers; unused by the backreference case), and
/// the fraction of the run at which the points start: after the pattern has been read.
let private shapeOf (name: string) : int * float =
    match name with
    | "regex-registers" -> 60, 0.4
    | _ -> 0, 0.05

/// Run the case, then stop it at fourteen points; each stop must come promptly.
let private stopsPromptly (mode: string) (name: string) : unit =
    let size, from = shapeOf name
    let o = Child.runWith 600_000 childEnv (sprintf "ic|scan|%s|%d|%s|14|%g" name size mode from)
    Child.expectClean o
    let field' key = o.Result |> Option.bind (field key)
    if field' "ok" <> Some "true" then Values.fail (sprintf "The full run of %s did not succeed: %s" name o.Describe)
    let stops =
        (field' "stops" |> Option.defaultValue "").Split('|', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s ->
            let i = s.IndexOf ':'
            int (s.Substring(0, i)), s.Substring(i + 1))
        |> List.ofArray
    let stopped = stops |> List.choose (fun (at, v) -> if v.StartsWith "x" then None else Some(at, float v))
    Assert.True(not stopped.IsEmpty, sprintf "%s: no %s stopped the run: %s" name mode o.Describe)
    let late = stopped |> List.filter (fun (_, over) -> over > promptly)
    Assert.True(late.Length <= noise,
                sprintf "%s: %d stops came more than %.0f ms after the %s (at most %d tolerated), as (placed at ms, late by ms): %A; all points %A; %s"
                    name late.Length promptly mode noise late stops o.Describe)

[<Collection("LargeValues")>]
type RegexMatchSteps() =

    [<Theory>]
    [<InlineData("regex-registers")>]        // RegexMatchAll(60 "a!", "(?:a|b" and 2^20 "()" ")*")
    [<InlineData("regex-backref-compare")>]  // RegexTest(a ..., "(a)(\1\1)(\2\2)...(\28\28)" and six (?=\29))
    [<Trait("requirement", "JYRO-REQ-1411")>]
    [<Trait("defect", "JYRO-DEF-771")>]
    member _.``JYRO-DEF-771 JYRO-REQ-1411 a regex match stops with ExecutionTimeLimitExceeded when the time limit ends during a step that grows with the pattern or the text``(case: string) =
        stopsPromptly "time" case

    [<Theory>]
    [<InlineData("regex-registers")>]
    [<InlineData("regex-backref-compare")>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    [<Trait("defect", "JYRO-DEF-771")>]
    member _.``JYRO-DEF-771 JYRO-REQ-1411 a regex match stops with CanceledByHost when the host cancels during a step that grows with the pattern or the text``(case: string) =
        stopsPromptly "cancel" case

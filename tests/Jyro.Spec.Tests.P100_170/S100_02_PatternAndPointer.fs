/// 100.2.5 (JYRO-REQ-1411): reading a pattern or a JSON Pointer that a script gives is
/// iteration over data from the script, so the time limit and the cancellation signal are
/// checked during it at bounded intervals.
///
/// JYRO-DEF-768: a pattern is compiled by the Regex functions and by the pattern keywords
/// of ValidateSchema. The pattern here is a host string with a digit run of 2^26 digits, in
/// a quantifier bound or in a backreference. Read whole (BigInteger.Parse, quadratic on
/// net8.0) it runs for hours; read in bounded steps it is a linear pass of some hundred
/// milliseconds.
///
/// JYRO-DEF-769: a JSON Pointer token of 2^28 digits in a $ref, against an array. These two
/// cases pass on the code at 2b8fbf2: Int32.TryParse over the token is too short to see at
/// this resolution (a cancel placed anywhere in the 1.4 s run is answered within 1 ms), so
/// they guard the requirement and do not show the defect, which is for inspection.
///
/// A correct run takes some hundreds of milliseconds, so the limit or the cancel cannot be
/// placed at a fixed time. The child first runs the case to its end (D, Execute alone, after
/// a warm-up), then the limit (mode time) or the cancel (mode cancel) falls at several
/// fractions of D. Wherever it falls, the run must stop with the limit's own code, and close
/// behind it: a step of the run that is not checked inside runs on to its end, past the
/// bound. A case whose full run is too short to place a limit in is reported skipped
/// (JYRO-ADR-137). Runs are in child processes under a 3 GB heap, killed after 60 s.
module RealTree.Jyro.Spec.Tests.S100_02_PatternAndPointer

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.ValueSize

let private childEnv = [ "DOTNET_GCHeapHardLimit", "C0000000" ]

let private runCase (name: string) (mode: string) (ms: int) : Child.ChildOutcome =
    Child.runWith 60_000 childEnv (sprintf "ic|case|%s|0|%s|%d" name mode ms)

let private number (key: string) (o: Child.ChildOutcome) : float =
    match o.Result |> Option.bind (field key) with
    | Some v -> float v
    | None -> Values.fail (sprintf "No %s in the child's result: %s" key o.Describe)

/// The longest a stop may come after the limit or the cancel, in ms.
let private promptly = 25.0

/// Run the case to its end, then stop it with the limit (mode "time") or the cancel (mode
/// "cancel") at each fraction of that time. A run that ended before its limit fell is not
/// counted; at least one must have been stopped, and every stop must be prompt.
let private stopsPromptly (code: MessageCode) (mode: string) (name: string) : unit =
    let full = runCase name "full" 0
    Child.expectClean full
    let d = number "ms" full
    Skip.If(d < 100.0, sprintf "%s: the full run took %.1f ms, too short to place a %s in it" name d mode)
    let stops =
        [ for f in [ 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8 ] ->
            let at = int (d * f)
            let o = runCase name mode at
            Child.expectClean o
            if o.Codes = [ string code ] then
                let over = if mode = "time" then number "ms" o - float at else number "fromCancel" o
                Some(at, over)
            else
                None ]
        |> List.choose id
    Assert.True(not stops.IsEmpty, sprintf "%s: no %s fell inside the full run of %.1f ms" name mode d)
    let late = stops |> List.filter (fun (_, over) -> over > promptly)
    Assert.True(late.IsEmpty,
                sprintf "%s: the stop came more than %.0f ms after the %s, as (placed at ms, late by ms): %A; all stops %A; full run %.1f ms"
                    name promptly mode late stops d)

[<Collection("LargeValues")>]
type PatternCompilation() =

    [<SkippableTheory>]
    [<InlineData("regex-bound")>]     // RegexTest(_, "a{999...9}")
    [<InlineData("regex-backref")>]   // RegexTest(_, "(a)\999...9")
    [<InlineData("schema-bound")>]    // ValidateSchema(_, {pattern: "a{999...9}"})
    [<InlineData("schema-backref")>]  // ValidateSchema(_, {pattern: "(a)\999...9"})
    [<Trait("requirement", "JYRO-REQ-1411")>]
    [<Trait("defect", "JYRO-DEF-768")>]
    member _.``JYRO-DEF-768 JYRO-REQ-1411 compiling a pattern with a digit run of 2^26 digits stops with ExecutionTimeLimitExceeded when the time limit ends during it``(case: string) =
        stopsPromptly MessageCode.ExecutionTimeLimitExceeded "time" case

    [<SkippableTheory>]
    [<InlineData("regex-bound")>]
    [<InlineData("regex-backref")>]
    [<InlineData("schema-bound")>]
    [<InlineData("schema-backref")>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    [<Trait("defect", "JYRO-DEF-768")>]
    member _.``JYRO-DEF-768 JYRO-REQ-1411 compiling a pattern with a digit run of 2^26 digits stops with CanceledByHost when the host cancels during it``(case: string) =
        stopsPromptly MessageCode.CanceledByHost "cancel" case

[<Collection("LargeValues")>]
type PointerToken() =

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-REQ-1411 a JSON Pointer array token of 2^28 digits stops with ExecutionTimeLimitExceeded when the time limit ends during it``() =
        stopsPromptly MessageCode.ExecutionTimeLimitExceeded "time" "schema-pointer"

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-REQ-1411 a JSON Pointer array token of 2^28 digits stops with CanceledByHost when the host cancels during it``() =
        stopsPromptly MessageCode.CanceledByHost "cancel" "schema-pointer"

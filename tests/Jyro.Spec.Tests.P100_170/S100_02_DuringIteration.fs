/// 100.2.5: a standard library function that iterates over data from the script
/// checks the time and the cancellation signal at bounded intervals DURING the
/// iteration, and 150.9.5: an artifact inside the nesting bound loads whatever
/// the host thread's stack. From the failing cases of the SANDBOX defects d-96,
/// d-487, d-488, d-489, d-493, d-494, d-500, d-501 and d-503, whose inputs no
/// test ran.
///
/// Earlier tests stopped a call before its first unit, which a single check on
/// entry passes. Here the limit or the cancel falls inside the call:
/// - Calibrated cases: the child first runs the call to its end (D, Execute
///   alone, after a warm-up), doubling the workload where the case allows
///   until D is at least twice the 50 ms floor; a case that cannot reach it is
///   reported skipped, never failed (JYRO-ADR-137). The limit or the cancel then falls at D/5, in
///   three attempts; each must stop with the code, and the fastest must stop
///   before 0.75 D. A call that checks only on entry, or every so many units of
///   unbounded cost, runs to about D on every attempt.
/// - Fixed cases, whose unchecked run is known to take far longer than the
///   bound: the stop must come within a stated bound of the limit.
/// Every run sets all four limits (4.1.11) and times Execute alone (4.1.14).
/// The runs are in child processes (IterationCases.fs) under a 3 GB heap, one at
/// a time (collection LargeValues), killed if they do not end.
module RealTree.Jyro.Spec.Tests.S100_02_DuringIteration

open System
open System.IO
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.ValueSize
open RealTree.Jyro.Spec.Tests.Artifact

/// Two host strings of 2^28 code units are 1 GB; the child's heap limit is 3 GB.
let private childEnv = [ "DOTNET_GCHeapHardLimit", "C0000000" ]

let private runCase (name: string) (p: int) (mode: string) (ms: int) (timeoutMs: int) : Child.ChildOutcome =
    Child.runWith timeoutMs childEnv (sprintf "ic|case|%s|%d|%s|%d" name p mode ms)

let private number (key: string) (o: Child.ChildOutcome) : float =
    match o.Result |> Option.bind (field key) with
    | Some v -> float v
    | None -> Values.fail (sprintf "No %s in the child's result: %s" key o.Describe)

/// The shortest full call in which a limit or a cancel at D/5 can be placed.
let private floorMs = 50.0

/// JYRO-ADR-137 as amended by JYRO-ADR-171: the child sizes the case's workload
/// so that the full call, run under a 30 s ceiling, takes at least twice the
/// floor and less than the ceiling, halving it when it is too long and growing
/// it when it is too short. Returns the scale p and the time D of the full call
/// at that scale. A case with no such size within its bounds is reported as
/// skipped with the times it measured: the precondition of the test does not
/// hold, which says nothing of the requirement.
let private calibrated (name: string) : int * float =
    let o = Child.runWith 600_000 childEnv (sprintf "ic|calibrate|%s|%.0f" name (2.0 * floorMs))
    Child.expectClean o
    let p = int (number "p" o)
    let d = number "ms" o
    let times = o.Result |> Option.bind (field "times") |> Option.defaultValue "?"
    let tooLong = o.Result |> Option.bind (field "tooLong") = Some "true"
    Skip.If(tooLong,
            sprintf "%s at its smallest workload (scale %d) reached the %.0f s measuring ceiling (times by scale, ms: %s): too long to calibrate on this machine"
                name p IterationCases.measuringCeiling.TotalSeconds times)
    Skip.If(d < 2.0 * floorMs,
            sprintf "%s at its largest workload (scale %d) ran to its end in %.1f ms, under twice the %.0f ms floor (times by scale, ms: %s): too short to place a limit inside it on this machine"
                name p d floorMs times)
    p, d

/// A calibrated case: the limit (mode "time") or the cancel (mode "cancel")
/// falls at a fifth of the full call; see the module's comment.
let private stopsDuring (code: MessageCode) (mode: string) (name: string) : unit =
    let p, d = calibrated name
    let at = max 5 (int (d / 5.0))
    let stops =
        [ for _ in 1 .. 3 ->
            let o = runCase name p mode at 60_000
            Child.expectCode code o
            number "ms" o ]
    let fastest = List.min stops
    Assert.True(fastest < 0.75 * d,
                sprintf "%s: with the %s at %d ms of a %.1f ms call, the fastest stop came at %.1f ms (stops %A): not during the iteration"
                    name (if mode = "time" then "limit" else "cancel") at d fastest stops)

// -- Calibrated: the time limit falls inside the call -----------------------------------------------

[<Collection("LargeValues")>]
type TimeLimitDuringTheCall() =

    [<SkippableTheory>]
    [<InlineData("contains-array")>]   // d-96: Contains over 2^25 nulls, for a value it does not hold
    [<InlineData("reverse")>]          // d-493: Reverse of M elements
    [<InlineData("tojson-tree")>]      // d-489: ToJson of a = [a, a] 30 times
    [<InlineData("tostring-tree")>]    // d-489
    [<InlineData("flatten-tree")>]     // d-489
    [<InlineData("schema-tree")>]      // d-494: ValidateSchema over 20 doublings of ["a"], a pattern at each leaf
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``Call_WhenTheTimeLimitEndsDuringIt_StopsWithExecutionTimeLimitExceeded``(case: string) =
        stopsDuring MessageCode.ExecutionTimeLimitExceeded "time" case

// -- Calibrated: the host cancels inside the call ---------------------------------------------------

[<Collection("LargeValues")>]
type CancelDuringTheCall() =

    [<SkippableTheory>]
    [<InlineData("contains-array")>]   // d-96
    [<InlineData("append")>]           // d-493: M - 1 elements
    [<InlineData("prepend")>]
    [<InlineData("insert")>]
    [<InlineData("removeat")>]         // d-493: M + 1 elements
    [<InlineData("removefirst")>]
    [<InlineData("removelast")>]
    [<InlineData("coalesce")>]         // d-493: 2^25 nulls
    [<InlineData("validaterequired")>] // d-493: 2^22 fields, none present
    [<InlineData("tojson-tree")>]      // d-489
    [<InlineData("tostring-tree")>]
    [<InlineData("flatten-tree")>]
    [<InlineData("fromjson-host")>]    // d-503: a host string of 2^28 code units
    [<InlineData("startswith-host")>]  // d-503: two equal host strings of 2^28 code units
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``Call_WhenTheHostCancelsDuringIt_StopsWithCanceledByHost``(case: string) =
        stopsDuring MessageCode.CanceledByHost "cancel" case

// -- Calibrated: FormatDate reads a long format (JYRO-DEF-474) --------------------------------------

/// FormatDate reads its format left to right (94.4.8), so a long format is
/// iteration over data from the script, bound by 100.2.5. The format is M - 2
/// copies of "-", each copied to the result unchanged (94.4.7): a call that
/// takes the format in one unchecked step runs to its end on every attempt.
[<Collection("LargeValues")>]
type FormatDateDuringTheCall() =

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-REQ-1411 FormatDate over a format of M - 2 code units stops with ExecutionTimeLimitExceeded when the time limit ends during it``() =
        stopsDuring MessageCode.ExecutionTimeLimitExceeded "time" "formatdate-long"

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-REQ-1411 FormatDate over a format of M - 2 code units stops with CanceledByHost when the host cancels during it``() =
        stopsDuring MessageCode.CanceledByHost "cancel" "formatdate-long"

// -- Fixed: work that unchecked runs for minutes or more --------------------------------------------

/// Assert the child's run stopped with ExecutionTimeLimitExceeded within `bound`
/// ms of the start of Execute.
let private stopsWithin (bound: float) (o: Child.ChildOutcome) : unit =
    Child.expectCode MessageCode.ExecutionTimeLimitExceeded o
    let ms = number "ms" o
    Assert.True(ms < bound, sprintf "stopped after %.1f ms; the bound is %.0f ms. %s" ms bound o.Describe)

[<Collection("LargeValues")>]
type FixedLimits() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``ValidateSchema_OfTheAcyclicAllOfDoublingOf30Levels_StopsNearA500msLimit``() =
        // d-488: unchecked, 2^30 subschema applications run for hours (20 levels take about 0.6 s). 3 levels
        // take milliseconds, so the stop must come within 1.5 s of the 500 ms limit.
        runCase "schema-allof" 0 "time" 500 30_000 |> stopsWithin 2_000.0

    [<Theory>]
    [<InlineData("search-contains")>]
    [<InlineData("search-indexof")>]
    [<InlineData("search-replace")>]
    [<InlineData("search-split")>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``LongNeedleSearch_StopsWithin500msOfA1sLimit``(case: string) =
        // d-500: s = 2^24 "a", n = 2^22 "a" & "b" & 2^22 "a"; every position is a
        // candidate compared over about 4M code units, so unchecked it runs minutes.
        runCase case 0 "time" 1_000 30_000 |> stopsWithin 1_500.0

    /// The smallest n whose one-match case (`one`) takes 300 to 600 ms, below
    /// the 1 s regex timeout (90.10.4).
    static member private SlowN (one: string) : int * float =
        let rec find n =
            if n > 30 then Values.fail (sprintf "No n from 16 to 30 gives %s a match of 300 to 600 ms" one)
            else
                let o = runCase one n "full" 0 60_000
                Child.expectClean o
                let ms = number "ms" o
                if o.Codes <> [] || ms > 600.0 then Values.fail (sprintf "%s at n = %d: %.1f ms, codes %A: no n gives 300 to 600 ms" one n ms o.Codes)
                elif ms >= 300.0 then n, ms
                else find (n + 1)
        find 16

    [<Theory>]
    [<InlineData("regex-one", "regex-matchall")>]    // RegexMatchAll: ten matches of (a+)+b|! each ~0.3-0.6 s
    [<InlineData("screen-one", "screen-series")>]    // ValidateSchema's pattern screen over ten such strings
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``SeriesOfSlowMatches_StopsWithinOneMatchOfA1sLimit``(one: string, series: string) =
        // d-501: each match takes 300 to 600 ms, so ten take at least 3 s. A check
        // before every match stops within one match of the 1 s limit: before 2 s.
        let n, single = FixedLimits.SlowN one
        let o = runCase series n "time" 1_000 60_000
        Child.expectCode MessageCode.ExecutionTimeLimitExceeded o
        let ms = number "ms" o
        Assert.True(ms < 2_000.0, sprintf "n = %d (one match %.1f ms): stopped after %.1f ms, not within a match of the 1 s limit" n single ms)

// -- ValidateSchema: work inside one step that grows with the schema or the data --------------------
//
// JYRO-DEF-432, 596, 597 and 598 (JYRO-ADR-149): each input below makes a validator that does
// work growing with a key's length, a $ref chain's length, an array's length or an error path's
// length between two checks run for tens of seconds or minutes in one unchecked step. 100.2.5
// asks for a check at bounded intervals, whatever the shape of the schema or the data: so the
// run either completes, or stops with the limit's own code, and either way it ends within a
// second of the 1 s limit (or of the 500 ms cancel). The child is killed after 30 s, and a run
// that is killed, or that ends the process, fails the test.

/// Assert the child finished normally and Execute ended before `bound` ms, with
/// success or with exactly one error whose code is one of `codes`.
let private endsWithin (bound: float) (codes: MessageCode list) (o: Child.ChildOutcome) : unit =
    Child.expectClean o
    let allowed = codes |> List.map string
    Assert.True(o.Ok || (match o.Codes with [ c ] -> List.contains c allowed | _ -> false),
                sprintf "expected success or one of %A: %s" codes o.Describe)
    let ms = number "ms" o
    Assert.True(ms < bound, sprintf "Execute ended after %.1f ms; the bound is %.0f ms. %s" ms bound o.Describe)

[<Collection("LargeValues")>]
type SchemaWorkUnderTheLimit() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-596 ValidateSchema reading a schema whose one name of 2^22 code units holds 4096 members ends within 1 s of a 1 s time limit``() =
        runCase "schema-longkey" 0 "time" 1_000 30_000 |> endsWithin 2_000.0 [ MessageCode.ExecutionTimeLimitExceeded ]

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-596 ValidateSchema reading a schema whose one name of 2^22 code units holds 4096 members ends within 1 s of a host cancel``() =
        runCase "schema-longkey" 0 "cancel" 500 30_000 |> endsWithin 1_500.0 [ MessageCode.CanceledByHost ]

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-432 ValidateSchema following a chain of 2^20 $ref-only $defs entries ends within 1 s of a 1 s time limit``() =
        runCase "schema-refchain" 0 "time" 1_000 30_000 |> endsWithin 2_000.0 [ MessageCode.ExecutionTimeLimitExceeded ]

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-432 ValidateSchema following a chain of 2^20 $ref-only $defs entries ends within 1 s of a host cancel``() =
        runCase "schema-refchain" 0 "cancel" 500 30_000 |> endsWithin 1_500.0 [ MessageCode.CanceledByHost ]

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-432 ValidateSchema checking uniqueItems over many distinct objects stops with ExecutionTimeLimitExceeded when the time limit ends during it``() =
        stopsDuring MessageCode.ExecutionTimeLimitExceeded "time" "schema-unique"

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-432 ValidateSchema checking uniqueItems over many distinct objects stops with CanceledByHost when the host cancels during it``() =
        stopsDuring MessageCode.CanceledByHost "cancel" "schema-unique"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-598 ValidateSchema applying a subschema to 2^17 members under a key of 2^22 code units ends within 1 s of a 1 s time limit``() =
        runCase "schema-longpath" 0 "time" 1_000 30_000 |> endsWithin 2_000.0 [ MessageCode.ExecutionTimeLimitExceeded ]

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-598 ValidateSchema applying a subschema to 2^17 members under a key of 2^22 code units ends within 1 s of a host cancel``() =
        runCase "schema-longpath" 0 "cancel" 500 30_000 |> endsWithin 1_500.0 [ MessageCode.CanceledByHost ]

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-598 ValidateSchema reading and checking a large host array stops with ExecutionTimeLimitExceeded when the time limit ends during it``() =
        stopsDuring MessageCode.ExecutionTimeLimitExceeded "time" "schema-data"

    [<SkippableFact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-598 ValidateSchema reading and checking a large host array stops with CanceledByHost when the host cancels during it``() =
        stopsDuring MessageCode.CanceledByHost "cancel" "schema-data"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-DEF-597 ValidateSchema reporting 2^14 errors whose paths hold a key of 2^20 code units stops within 1 s of a 1 s time limit``() =
        // The result would hold about 2^35 code units, above the maximum value size M
        // (100.8.7), so it cannot complete: it stops with the time limit's code or the
        // size code, within the bound, and without exhausting the child's 3 GB heap.
        let o = runCase "schema-errorpaths" 0 "time" 1_000 30_000
        Child.expectClean o
        Assert.False(o.Ok, "the run succeeded, with a result above the maximum value size: " + o.Describe)
        let allowed = [ string MessageCode.ExecutionTimeLimitExceeded; valueSizeName ]
        Assert.True((match o.Codes with [ c ] -> List.contains c allowed | _ -> false),
                    sprintf "expected one of %A: %s" allowed o.Describe)
        let ms = number "ms" o
        Assert.True(ms < 2_000.0, sprintf "Execute ended after %.1f ms; the bound is 2000 ms. %s" ms o.Describe)

[<Collection("LargeValues")>]
type SchemaDuplicateFailures() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1254")>]
    member _.``JYRO-DEF-719 ValidateSchema with 2^14 identical failures under a key of 2^20 code units returns the one element within a 10 s limit``() =
        // 96.2.29 reports the 2^14 failures (/K, type) once, and that element's path
        // and message (about 2^21 code units) are far below M (100.8.7), so the call
        // completes. Writing the path of every failure first holds 2^34 code units,
        // which the child's 3 GB heap cannot, and takes far longer than 10 s.
        let o = Child.runWith 60_000 childEnv "ic|case|schema-duplicates|0|time|10000"
        Child.expectSuccess o
        Assert.Equal(Some "1", o.Result |> Option.bind (field "n"))
        let ms = number "ms" o
        Assert.True(ms < 10_000.0, sprintf "Execute took %.1f ms. %s" ms o.Describe)

// -- 150.9.5 (d-487): an artifact of nesting 256 loads on a host thread with a small stack ----------

let rec private unaries (k: int) (e: Expr) : Expr = if k = 0 then e else EUn(Op.negate, unaries (k - 1) e, p0)

/// Write `bytes` to a temporary file and load it in a child on a thread of `kb` KB.
let private loadOnSmallStack (bytes: byte[]) (kb: int) : Child.ChildOutcome =
    let path = Path.Combine(Path.GetTempPath(), sprintf "jyro-d487-%s.jyrx" (Guid.NewGuid().ToString("N")))
    File.WriteAllBytes(path, bytes)
    try Child.runWith 60_000 [] (sprintf "ic|load|%s|%d" path kb)
    finally File.Delete path

[<Theory>]
[<InlineData(256)>]
[<InlineData(384)>]
[<InlineData(1024)>]
[<Trait("requirement", "JYRO-REQ-2135")>]
let ``JYRO-REQ-2135 ArtifactOfNesting256_LoadsOnAHostThreadWithASmallStack_AndRuns`` (kb: int) =
    // Output.x = -(-(...(1))) with 254 negations: the Assignment has depth 1,
    // the outer Unary 2 and the Literal 256 (150.9.9). It must load, not crash
    // the process; 254 negations of 1 give 1.
    let o = loadOnSmallStack (build [ setOut "x" (unaries 254 (num 1m)) ]) kb
    Child.expectClean o
    Assert.True(o.Ok, "The artifact of depth 256 did not load: " + o.Describe)
    Assert.Equal(Some "1", field "x" o.Result.Value)

[<Theory>]
[<InlineData(256)>]
[<InlineData(384)>]
[<InlineData(1024)>]
[<Trait("requirement", "JYRO-REQ-2135")>]
let ``JYRO-REQ-2135 ArtifactOfNesting257_IsRejectedAsUnreadable_OnAHostThreadWithASmallStack`` (kb: int) =
    let o = loadOnSmallStack (build [ setOut "x" (unaries 255 (num 1m)) ]) kb
    Child.expectCode MessageCode.ArtifactUnreadable o

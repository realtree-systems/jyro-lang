/// jyro-spec 92.13 Range, 92.14 Skip, 92.15 Insert, 92.16 RemoveAt.
module RealTree.Jyro.Spec.Tests.S92_13_16_RangeSkipInsertRemoveAt

open System
open System.Diagnostics
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P92Support

// =============================================================================
// 92.13 Range
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-714")>]
let ``Range_TakesNumbersAndOptionalStep_ReturnsArrayOfNumbers`` () =
    assertSignature "Range" [ "start", false; "finish", false; "step", true ]
    typesAre "array" [ "Range(0, 3)"; "Range(0, 3, 1)"; "Range(0, 1, 0.5)" ]
    let o = out "var r = Range(0, 1, 0.5)\nvar s = Range(0, 3)\nOutput.t = [TypeOf(r[0]), TypeOf(r[1]), TypeOf(s[0]), TypeOf(s[1]), TypeOf(s[2])]"
    assertJson "[\"number\", \"number\", \"number\", \"number\", \"number\"]" (Values.prop "t" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-715")>]
let ``Range_WithoutStep_UsesExactOne`` () =
    let r1 = eval "Range(0, 3)"
    assertJson "[0, 1, 2]" r1
    let r2 = eval "Range(0.5, 3)"
    assertJson "[0.5, 1.5, 2.5]" r2
    for v in Values.asList r1 @ Values.asList r2 do
        Assert.True(Values.isExact v, sprintf "%s is not exact" (show v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-716")>]
let ``Range_PositiveStep_ReturnsEachStartPlusKStepBelowFinish`` () =
    evalsTo ""
        [ "Range(0, 5)", "[0, 1, 2, 3, 4]"; "Range(0, 5, 2)", "[0, 2, 4]"; "Range(1, 2)", "[1]"
          "Range(0, 1, 0.25)", "[0, 0.25, 0.5, 0.75]"; "Range(-3, 0)", "[-3, -2, -1]"
          "Range(0, 2.5)", "[0, 1, 2]"; "Range(0, 6, 2)", "[0, 2, 4]"; "Range(0, 6.5, 2)", "[0, 2, 4, 6]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-717")>]
let ``Range_NegativeStep_ReturnsEachStartPlusKStepAboveFinish`` () =
    evalsTo ""
        [ "Range(5, 0, -1)", "[5, 4, 3, 2, 1]"; "Range(0, -3, -1.5)", "[0, -1.5]"
          "Range(1, 0, -0.25)", "[1, 0.75, 0.5, 0.25]"; "Range(3, 2, -1)", "[3]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-718")>]
let ``Range_PositiveStepAndFinishAtOrBelowStart_ReturnsEmptyArray`` () =
    evalsTo "" [ "Range(5, 5)", "[]"; "Range(5, 0)", "[]"; "Range(5, 4.9, 1)", "[]"; "Range(0, -1, 2)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-719")>]
let ``Range_NegativeStepAndFinishAtOrAboveStart_ReturnsEmptyArray`` () =
    evalsTo "" [ "Range(0, 0, -1)", "[]"; "Range(0, 5, -1)", "[]"; "Range(0, 0.1, -2)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-720")>]
let ``Range_ComparesWithFinishAsLessThanAndGreaterThanDo`` () =
    evalsTo "var one = Power(1, 0.5)"
        [ "Range(0, 0.30000000000000001, 0.1)", "[0, 0.1, 0.2, 0.3]"
          "Range(1, 0.69999999999999999, -0.1)", "[1, 0.9, 0.8, 0.7]"
          "Range(0, 0.3 * one, 0.1)", "[0, 0.1, 0.2]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-721")>]
let ``Range_ComputesStartPlusKTimesStepNotRepeatedAddition`` () =
    // Judged by value and representation through the host API, not by the
    // string form of the elements (40.4.6; FLOORS 4.1.15).
    let r = Values.asList (evalWith "var step = 0.1 * Power(1, 0.5)" "Range(0, 1, step)")
    Assert.Equal(10, r.Length)
    // Inexact step: k * step in double, not eight or nine additions of 0.1.
    Assert.Equal(8.0 * 0.1, Values.asFloat r.[8])
    Assert.Equal(9.0 * 0.1, Values.asFloat r.[9])
    // Exact start and step: 0 + 3 * 0.1 is exactly 0.3 (30.2.9).
    let x3 = (Values.asList (eval "Range(0, 1, 0.1)")).[3]
    Assert.True(Values.isExact x3, sprintf "x[3] = %s is not exact" (show x3))
    Assert.Equal(0.3M, Values.asDecimal x3)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-722")>]
let ``Range_ExactStartAndStep_GiveExactElements`` () =
    for expr in [ "Range(0, 1, 0.1)"; "Range(-2, 2)"; "Range(0.25, 1, 0.25)" ] do
        for v in Values.asList (eval expr) do
            Assert.True(Values.isExact v, sprintf "%s: element %s is not exact" expr (show v))
    // Control (30.2.10): an inexact start gives inexact elements.
    for v in Values.asList (eval "Range(0 * Power(1, 0.5), 3)") do
        Assert.False(Values.isExact v, sprintf "inexact start: element %s is exact" (show v))

// -- JYRO-DEF-706: operands of 28 and 29 significant digits ---------------------------------
// The exact representation holds m / 10^s, |m| <= 2^96 - 1 = 79228162514264337593543950335,
// 0 <= s <= 28 (30.2.4). Each element is start + k * step, each operation exact when its
// result is held without rounding (30.2.9) and in IEEE double, inexact, when it is not
// (30.2.11). Every inexact case below is a multiple of a power of two, so the double that the
// double operation gives and the double nearest the exact value are the same number.

/// The decimal of `text`, checked to read back as `text`, so the expected value is the
/// literal itself and not a rounding of it by the .NET parser.
let private exactOf (text: string) : decimal =
    let d = Decimal.Parse(text, Globalization.CultureInfo.InvariantCulture)
    Assert.Equal(text, d.ToString(Globalization.CultureInfo.InvariantCulture))
    d

/// Assert that `v` is exact and equal to the decimal `text`.
let private isExactly (what: string) (text: string) (v: JyroValue) : unit =
    Assert.True(Values.isExact v, sprintf "%s = %s is not exact; expected the exact %s" what (show v) text)
    Assert.Equal(exactOf text, Values.asDecimal v)

/// Assert that `v` is inexact and the double `expected`.
let private isInexactly (what: string) (expected: float) (v: JyroValue) : unit =
    Assert.False(Values.isExact v, sprintf "%s = %s is exact; expected the inexact %s" what (show v) (expected.ToString("R", Globalization.CultureInfo.InvariantCulture)))
    Assert.Equal(expected, Values.asFloat v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-722")>]
let ``JYRO-REQ-722 JYRO-DEF-706 Range_ElementsOf28And29Digits_ThatTheExactRepresentationHolds_StayExactAndUnrounded`` () =
    // The defect's own call: step = floor(2^96 / 10) / 10^27. Every k * m for k <= 3 is at
    // most 2^96 - 1, so each element is held at scale 27 with no rounding.
    let r = Values.asList (eval "Range(0, 30, 7.922816251426433759354395033)")
    Assert.Equal(4, r.Length)
    [ "0"; "7.922816251426433759354395033"; "15.845632502852867518708790066"; "23.768448754279301278063185099" ]
    |> List.iteri (fun k text -> isExactly (sprintf "Range(0, 30, 7.92...)[%d]" k) text r.[k])
    // step = 2^90 + 0.5 = 12379400392853802748991242245 / 10: k * 12379400392853802748991242245
    // is at most 2^96 - 1 for k <= 6, and a multiple of 10 at k = 2, 4, 6.
    let up = Values.asList (eval "Range(0, 9e27, 1237940039285380274899124224.5)")
    let down = Values.asList (eval "Range(0, -9e27, -1237940039285380274899124224.5)")
    Assert.Equal(8, up.Length)
    Assert.Equal(8, down.Length)
    [ "0"; "1237940039285380274899124224.5"; "2475880078570760549798248449"; "3713820117856140824697372673.5"
      "4951760157141521099596496898"; "6189700196426901374495621122.5"; "7427640235712281649394745347" ]
    |> List.iteri (fun k text ->
        isExactly (sprintf "Range(0, 9e27, 2^90 + 0.5)[%d]" k) text up.[k]
        isExactly (sprintf "Range(0, -9e27, -(2^90 + 0.5))[%d]" k) (if k = 0 then text else "-" + text) down.[k])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-721")>]
let ``JYRO-REQ-721 JYRO-DEF-706 Range_AProductTheExactRepresentationCannotHold_GivesAnInexactElement_TheNearestDouble`` () =
    // k = 7: 7 * (2^90 + 0.5) = 8665580274997661924293869571.5, whose m at scale 1,
    // 86655802749976619242938695715, is above 2^96 - 1 and is not a multiple of 10. So 7 * step
    // is done in double: 7 * 2^90 (the double of step is 2^90), which is also the double nearest
    // the exact product (its ulp is 2^40). 0 + that is the same double. The element at k = 8,
    // 9903520314283042199192993796, is not below 9e27, so k = 7 is the last.
    let sevenTimes2To90 = Math.ScaleB(7.0, 90)
    let up = Values.asList (eval "Range(0, 9e27, 1237940039285380274899124224.5)")
    Assert.Equal(8, up.Length)
    isInexactly "Range(0, 9e27, 2^90 + 0.5)[7]" sevenTimes2To90 up.[7]
    let down = Values.asList (eval "Range(0, -9e27, -1237940039285380274899124224.5)")
    Assert.Equal(8, down.Length)
    isInexactly "Range(0, -9e27, -(2^90 + 0.5))[7]" (-sevenTimes2To90) down.[7]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-721")>]
let ``JYRO-REQ-721 JYRO-DEF-706 Range_ASumTheExactRepresentationCannotHold_GivesAnInexactElement_TheNearestDouble`` () =
    // step = 2^90 (28 digits, exact). k = 1: 1 * step is exact; 0.25 + 2^90 =
    // 1237940039285380274899124224.25, whose m at scale 2 has 30 digits, above 2^96 - 1, and
    // ends in 5. So the sum is done in double: 0.25 + 2^90 rounds to 2^90, the double nearest the
    // exact sum (its ulp is 2^38). k = 2 gives 2^91 (about 2.48e27), not below 2e27.
    let r = Values.asList (eval "Range(0.25, 2e27, 1237940039285380274899124224)")
    Assert.Equal(2, r.Length)
    isExactly "Range(0.25, 2e27, 2^90)[0]" "0.25" r.[0]
    isInexactly "Range(0.25, 2e27, 2^90)[1]" (Math.ScaleB(1.0, 90)) r.[1]
    let down = Values.asList (eval "Range(-0.25, -2e27, -1237940039285380274899124224)")
    Assert.Equal(2, down.Length)
    isExactly "Range(-0.25, -2e27, -2^90)[0]" "-0.25" down.[0]
    isInexactly "Range(-0.25, -2e27, -2^90)[1]" (Math.ScaleB(-1.0, 90)) down.[1]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-723")>]
let ``Range_WhenStepIsZero_RaisesInvalidRange`` () =
    errorsWith MessageCode.InvalidRange ""
        [ "Range(0, 5, 0)"; "Range(0, 5, 0.0)"; "Range(5, 0, 0)"; "Range(0, 0, 0)"; "Range(0, 5, 0 * Power(1, 0.5))"; "Range(0, 5, -0)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-724")>]
let ``Range_AcceptsNonIntegerStartFinishAndStep`` () =
    evalsTo ""
        [ "Range(0.5, 2.5)", "[0.5, 1.5]"; "Range(0, 1, 0.25)", "[0, 0.25, 0.5, 0.75]"
          "Range(-1.5, 1.5, 1.5)", "[-1.5, 0]"; "Range(0.1, 0.35, 0.1)", "[0.1, 0.2, 0.3]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-725")>]
let ``Range_WhenStartFinishOrStepHasWrongType_RaisesArgumentTypeMismatch`` () =
    let bad = [ "\"0\""; "null"; "true"; "[0]"; "{}"; "x => x" ]
    errorsWith MessageCode.ArgumentTypeMismatch "" [ for v in bad -> sprintf "Range(%s, 3)" v ]
    errorsWith MessageCode.ArgumentTypeMismatch "" [ for v in bad -> sprintf "Range(0, %s)" v ]
    errorsWith MessageCode.ArgumentTypeMismatch "" [ for v in bad |> List.filter ((<>) "null") -> sprintf "Range(0, 3, %s)" v ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-726")>]
let ``Range_NullStep_IsStepNotGiven`` () =
    evalsTo "" [ "Range(0, 3, null)", "[0, 1, 2]"; "Range(3, 0, null)", "[]" ]
    allTrue "" [ "Range(0, 3, null) == Range(0, 3)"; "Range(3, 0, null) == Range(3, 0)" ]

/// Every execution limit of 100.1.1 set explicitly (FLOORS 4.1.11): the time
/// limit given, and statements, loop iterations and call depth far above what
/// a one-statement Range script needs.
let private fourLimits (time: TimeSpan) : JyroExecutionOptions =
    { JyroExecutionOptions.Default with
        MaxExecutionTime = time
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000
        MaxCallDepth = 64 }

/// Run options with the four limits set and a time limit of 30 s.
let private pinned : RunOptions = { Script.defaults with Limits = Some(fourLimits (TimeSpan.FromSeconds 30.)) }

/// True when this build has `DiagnosticMessage.Stage` (160.7.5). The stage is
/// asserted only then, so a test of Range fails for its own clause and not for
/// 160.7.5, which its own tests pin.
let private hasStage () : bool =
    not (isNull (typeof<DiagnosticMessage>.GetProperty("Stage", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)))

/// Assert exactly one error, of `code`, then its stage Execute (160.1.12).
let private failsWith (code: MessageCode) (r: JyroResult<JyroValue>) : unit =
    Script.expectError code r
    if hasStage () then Assert.Equal("Execute", Pending.stageOf (Script.onlyErrorMessage r))

/// The one-statement script whose work is all inside one Range call.
let private rangeScript = "Output.n = Length(Range(0, 999999))"

/// Compile `rangeScript` under `options`, then time `Execute` alone (the wall
/// time of the execution, not of the compile), starting `beforeRun` just before.
let private timedRange (options: RunOptions) (beforeRun: unit -> unit) : JyroResult<JyroValue> * TimeSpan =
    let program = Script.valueOf (Script.compileWith options rangeScript)
    let runner = Script.runnerFor options program
    beforeRun ()
    let sw = Stopwatch.StartNew()
    let r = runner.Execute()
    sw.Stop()
    r, sw.Elapsed

/// The shortest execution time of the Range call when nothing stops it: one
/// warm-up run (so JIT compilation is not counted), then the minimum of five.
/// Every real run lasts at least about this long, so a limit at a fifth of it
/// falls inside the call. Only success is asserted: the count is 92.13.3's.
let private fullRangeTime () : TimeSpan =
    let once () =
        let r, elapsed = timedRange pinned ignore
        ignore (Script.output r)
        elapsed
    once () |> ignore
    [ for _ in 1 .. 5 -> once () ] |> List.min

/// Three attempts, each stopped at a fifth of the shortest full run; each must
/// fail with `code`, and the fastest stop must come before three quarters of
/// the shortest full run. A Range that checks only before or after it builds
/// cannot stop before the full run on any attempt; one attempt slowed by a GC
/// pause does not fail a Range that checks while it builds.
let private stopsWhileBuilding (code: MessageCode) (attempt: TimeSpan -> JyroResult<JyroValue> * TimeSpan) : unit =
    let full = fullRangeTime ()
    let at = TimeSpan.FromTicks(max 1L (full.Ticks / 5L))
    let stops =
        [ for _ in 1 .. 3 ->
            let r, elapsed = attempt at
            failsWith code r
            elapsed ]
    let fastest = List.min stops
    Assert.True(fastest.TotalMilliseconds < 0.75 * full.TotalMilliseconds,
                sprintf "The fastest of three stops came after %.1f ms of a Range whose shortest full run is %.1f ms (stopped at %.1f ms): not while building. Stops: %A"
                    fastest.TotalMilliseconds full.TotalMilliseconds at.TotalMilliseconds
                    [ for s in stops -> s.TotalMilliseconds ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-727")>]
let ``Range_WhileBuilding_ObeysExecutionTimeLimit`` () =
    // Range is the only statement, so a time limit that falls inside its call is
    // noticed in time only if Range checks the time while it builds.
    stopsWhileBuilding MessageCode.ExecutionTimeLimitExceeded (fun at ->
        timedRange { pinned with Limits = Some(fourLimits at) } ignore)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-727")>]
let ``Range_WhileBuilding_ObeysHostCancellation`` () =
    stopsWhileBuilding MessageCode.CanceledByHost (fun at ->
        use cts = new CancellationTokenSource()
        // From a thread of its own, not a pool timer (JYRO-DEF-745). A Range call is of fixed
        // length (at most 1,000,000 elements), so it cannot be made unbounded; the three
        // attempts of stopsWhileBuilding absorb one that is slowed.
        timedRange { pinned with Cancellation = Some cts.Token } (fun () -> RealTree.Jyro.Spec.HostCancel.cancelFromThread cts at))

// -- 92.13.16 Range computes its elements in order; an overflow raises ------------------
// Every literal is inexact (30.2.9); MAX is 1.7976931348623157e308. The expected
// results follow the order of the clause, checked by simulation in IEEE double.

let private rangeOf (call: string) : JyroResult<JyroValue> =
    Script.runWith pinned (sprintf "Output.r = %s" call)

[<Theory>]
[<InlineData("Range(-1e308, 1.5e308, 1e308)")>]      // the product at k = 2
[<InlineData("Range(0, 1.5e308, 1e308)")>]           // the product at k = 2, before the comparison
[<InlineData("Range(-1.7e308, 1.7e308, 2e302)")>]    // the product at k = 898,847, before 1,000,000
[<Trait("requirement", "JYRO-REQ-729")>]
let ``Range_TheClausesExamples_RaiseNumberOverflow`` (call: string) =
    rangeOf call |> failsWith MessageCode.NumberOverflow

[<Theory>]
[<InlineData("Range(1e308, 1.7e308, 0.9e308)")>]     // k = 1: the product is finite, the sum 1.9e308 is not
[<InlineData("Range(1e308, -1.5e308, -1e308)")>]     // negative step: the product at k = 2
[<InlineData("Range(1e308, -1e308, -1e308)")>]       // negative step: k = 1 gives 0, above finish; the product at k = 2
[<Trait("requirement", "JYRO-REQ-729")>]
let ``Range_ASumOrProductThatIsAnInfinity_RaisesNumberOverflow`` (call: string) =
    rangeOf call |> failsWith MessageCode.NumberOverflow

[<Theory>]
[<InlineData("Range(0, 1e308, 1e308)", "[0]")>]      // k = 1 is not below finish; k = 2 is never computed
[<InlineData("Range(0, -1e308, -1e308)", "[0]")>]    // the same with a negative step
[<InlineData("Range(0, 3.5, 1)", "[0, 1, 2, 3]")>]
[<Trait("requirement", "JYRO-REQ-729")>]
let ``Range_StopsAtTheFirstElementNotBeforeFinish_AndComputesNoLaterPosition`` (call: string, expected: string) =
    assertJson expected (Script.output (rangeOf call) |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-729")>]
let ``Range_ZeroToAMillion_EndsBeforeTheLengthBound`` () =
    // At k = 1,000,000 the element is not below finish, so the third row is not met.
    let o = Script.output (Script.runWith pinned "var r = Range(0, 1000000)\nOutput.n = Length(r)\nOutput.last = r[999999]")
    Assert.Equal(1_000_000m, Values.asDecimal (Values.prop "n" o))
    Assert.Equal(999_999m, Values.asDecimal (Values.prop "last" o))

[<Theory>]
[<InlineData("Range(-1.7976931348623157e308, 1.7976931348623157e308, 1.7976940337093327e302)")>]
[<InlineData("Range(1.7976931348623157e308, -1.7976931348623157e308, -1.7976940337093327e302)")>]
[<Trait("requirement", "JYRO-REQ-729")>]
let ``Range_OverflowAtPosition1000000_ComesBeforeTheLengthBound`` (call: string) =
    // Every element before k = 1,000,000 is below (above) finish, and k * step
    // is first an infinity at k = 1,000,000.
    rangeOf call |> failsWith MessageCode.NumberOverflow

[<Fact>]
[<Trait("requirement", "JYRO-REQ-729")>]
let ``Range_ElementAtPosition1000000BelowFinish_RaisesGeneratedLengthExceeded_WithoutAllocatingTheResult`` () =
    // At k = 1,000,000 the element is finite and below finish; the product
    // first overflows at k = 1,000,001, which is never computed.
    let program = Script.valueOf (Script.compileWith pinned "Output.r = Range(-1.7976931348623157e308, 1.7976931348623157e308, 1.7976922360161978e302)")
    let runner = Script.runnerFor pinned program
    let before = GC.GetAllocatedBytesForCurrentThread()
    let r = runner.Execute()
    let spent = GC.GetAllocatedBytesForCurrentThread() - before
    failsWith MessageCode.GeneratedLengthExceeded r
    Assert.True(spent < (1L <<< 20), sprintf "Range allocated %d bytes before it raised GeneratedLengthExceeded" spent)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-728")>]
let ``Range_HasEffectPure`` () = assertPure "Range" "Range(0, 3)"

// =============================================================================
// 92.14 Skip
// =============================================================================

let private a4 = "var a = [1, 2, 3, 4]"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-730")>]
let ``Skip_TakesArrayOrNullAndCount_ReturnsArray`` () =
    assertSignature "Skip" [ "items", false; "count", false ]
    typesAre "array" [ "Skip([1, 2], 1)"; "Skip(null, 0)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-731")>]
let ``Skip_ReturnsElementsFromCountToEnd`` () =
    evalsTo a4 [ "Skip(a, 1)", "[2, 3, 4]"; "Skip(a, 2)", "[3, 4]"; "Skip(a, 3)", "[4]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-732")>]
let ``Skip_WhenCountAtOrPastLength_ReturnsEmptyArray`` () =
    evalsTo a4 [ "Skip(a, 4)", "[]"; "Skip(a, 5)", "[]"; "Skip(a, 2147483647)", "[]"; "Skip([], 1)", "[]" ]
    errorsWith MessageCode.IntegerOutOfRange a4 [ "Skip(a, 2147483648)"; "Skip(a, 1e20)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-733")>]
let ``Skip_WhenCountIsZero_ReturnsNewArrayWithEveryElement`` () =
    let o = out "var a = [1, 2, 3]\nvar r = Skip(a, 0)\nOutput.before = Clone(r)\nr[0] = 99\nOutput.a = a\nOutput.e = Skip([], 0)"
    assertJson "[1, 2, 3]" (Values.prop "before" o)
    assertJson "[1, 2, 3]" (Values.prop "a" o)
    assertJson "[]" (Values.prop "e" o)
    assertNewArray "[1, 2, 3]" (fun a -> sprintf "Skip(%s, 0)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-734")>]
let ``Skip_CountIsNonNegativeWholeQuantity`` () =
    let a3 = "var a = [1, 2, 3]\nvar one = Power(1, 0.5)"
    errorsWith MessageCode.IntegerRequired a3 [ "Skip(a, 1.5)"; "Skip(a, 0.5)"; "Skip(a, -1.5)"; "Skip(null, 1.5)" ]
    errorsWith MessageCode.NonNegativeIntegerRequired a3 [ "Skip(a, -1)"; "Skip(a, -2147483648)"; "Skip(null, -1)" ]
    errorsWith MessageCode.IntegerOutOfRange a3 [ "Skip(a, 2147483648)"; "Skip(a, -2147483649)"; "Skip(a, 1e20)" ]
    errorsWith MessageCode.ArgumentTypeMismatch a3 [ "Skip(5, -1.5)" ]
    evalsTo a3 [ "Skip(a, 2.0)", "[3]"; "Skip(a, 2 * one)", "[3]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-735")>]
let ``Skip_WhenCountIsNotNumber_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ "Skip([1, 2], \"1\")"; "Skip([1, 2], null)"; "Skip([1, 2], true)"; "Skip([1, 2], [1])"; "Skip([1, 2], {})"; "Skip([1, 2], x => x)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-736")>]
let ``Skip_OfNullItems_ReturnsEmptyArray`` () = evalsTo "" [ "Skip(null, 0)", "[]"; "Skip(null, 3)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-737")>]
let ``Skip_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Skip(%s, 1)" v) [ "Skip([1, 2], 1)"; "Skip(null, 1)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-738")>]
let ``Skip_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[0, {\"k\": 1}, 2, [3]]" (fun a -> sprintf "Skip(%s, 1)" a)
    assertNewArray "[0, {\"k\": 1}, 2, [3]]" (fun a -> sprintf "Skip(%s, 0)" a)
    assertNewArray "[]" (fun a -> sprintf "Skip(%s, 0)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-739")>]
let ``Skip_DoesNotChangeItems`` () =
    assertUnchanged "[0, {\"k\": 1}, 2, [3]]" (fun a -> sprintf "Skip(%s, 1)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-740")>]
let ``Skip_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Skip([0, %s], 1)" x) 0
    assertElementsShared (fun x -> sprintf "Skip([%s], 0)" x) 0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2021")>]
let ``Skip_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [0, {\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Skip(%s, 1)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-742")>]
let ``Skip_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [0, {\"k\": 1}, 2, [3]], \"e\": [1]}" (fun a -> sprintf "Skip(%s, 1)" a) (Some "Skip(Input.e, 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-743")>]
let ``Skip_HasEffectPure`` () = assertPure "Skip" "Skip([1, 2], 1)"

// =============================================================================
// 92.15 Insert
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-744")>]
let ``Insert_TakesArrayOrNullIndexAndAnyValue_ReturnsArray`` () =
    assertSignature "Insert" [ "items", false; "index", false; "value", false ]
    typesAre "array"
        [ "Insert([1], 0, 2)"; "Insert(null, 0, 1)"; "Insert([], 0, null)"; "Insert([], 0, true)"; "Insert([], 0, 1)"
          "Insert([], 0, \"s\")"; "Insert([], 0, [1])"; "Insert([], 0, {a: 1})"; "Insert([], 0, x => x)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-745")>]
let ``Insert_PutsValueAtIndexWithOtherElementsInOrder`` () =
    evalsTo ""
        [ "Insert([1, 2, 3], 1, \"x\")", "[1, \"x\", 2, 3]"; "Insert([1, 2, 3], 0, \"x\")", "[\"x\", 1, 2, 3]"
          "Insert([1, 2, 3], 2, \"x\")", "[1, 2, \"x\", 3]"; "Insert([1], 0, [2, 3])", "[[2, 3], 1]"
          "Insert([1], 0, null)", "[null, 1]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-746")>]
let ``Insert_AtIndexEqualToLength_PutsValueLast`` () =
    evalsTo "" [ "Insert([1, 2], 2, \"x\")", "[1, 2, \"x\"]"; "Insert([], 0, \"x\")", "[\"x\"]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-747")>]
let ``Insert_WhenIndexBelowZeroOrAboveLength_RaisesIndexOutOfRange`` () =
    let a2 = "var a = [1, 2]"
    errorsWith MessageCode.IndexOutOfRange a2
        [ "Insert(a, -1, 0)"; "Insert(a, 3, 0)"; "Insert(a, 2147483647, 0)"; "Insert(a, -2147483648, 0)"
          "Insert(null, 1, 0)"; "Insert(null, -1, 0)"; "Insert([], -1, 0)"; "Insert([], 1, 0)" ]
    evalsTo a2 [ "Insert(a, 0, 0)", "[0, 1, 2]"; "Insert(a, 2, 0)", "[1, 2, 0]" ]
    errorsWith MessageCode.IntegerRequired a2 [ "Insert(a, -1.5, 0)"; "Insert(a, 3.5, 0)" ]
    errorsWith MessageCode.IntegerOutOfRange a2 [ "Insert(a, 2147483648, 0)"; "Insert(a, -2147483649, 0)"; "Insert(a, 1e20, 0)" ]
    errorsWith MessageCode.ArgumentTypeMismatch a2 [ "Insert(5, -1, 0)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-748")>]
let ``Insert_IndexIsWholeQuantity`` () =
    let a2 = "var a = [1, 2]\nvar one = Power(1, 0.5)"
    errorsWith MessageCode.IntegerRequired a2
        [ "Insert(a, 0.5, 9)"; "Insert(a, 1.5, 9)"; "Insert(a, -1.5, 9)"; "Insert(a, 0.0000001, 9)"; "Insert(a, 99.5, 9)" ]
    errorsWith MessageCode.IntegerOutOfRange a2
        [ "Insert(a, 2147483648, 9)"; "Insert(a, -2147483649, 9)"; "Insert(a, 1e20, 9)"; "Insert(a, -1e20, 9)" ]
    evalsTo a2 [ "Insert(a, 1.0, 9)", "[1, 9, 2]"; "Insert(a, one, 9)", "[1, 9, 2]" ]
    errorsWith MessageCode.IndexOutOfRange a2 [ "Insert(a, -1, 9)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-749")>]
let ``Insert_WhenIndexIsNotNumber_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ "Insert([1], \"0\", 0)"; "Insert([1], null, 0)"; "Insert([1], true, 0)"; "Insert([1], [0], 0)"; "Insert([1], {}, 0)"; "Insert([1], x => x, 0)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-750")>]
let ``Insert_OfNullItems_GivesOneElementArray`` () =
    evalsTo "" [ "Insert(null, 0, 5)", "[5]"; "Insert(null, 0, null)", "[null]"; "Insert(null, 0, [1])", "[[1]]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-751")>]
let ``Insert_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Insert(%s, 1, 9)" v) [ "Insert([1, 2], 1, 9)"; "Insert(null, 0, 9)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-752")>]
let ``Insert_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Insert(%s, 1, 9)" a)
    assertNewArray "[]" (fun a -> sprintf "Insert(%s, 0, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-753")>]
let ``Insert_DoesNotChangeItemsAndItsLength`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Insert(%s, 1, 9)" a)
    assertUnchanged "[1, 2]" (fun a -> sprintf "Insert(%s, 2, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-754")>]
let ``Insert_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Insert([%s], 1, 9)" x) 0
    assertElementsShared (fun x -> sprintf "Insert([%s], 0, 9)" x) 1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2022")>]
let ``Insert_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Insert(%s, 1, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-756")>]
let ``Insert_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Insert(%s, 1, 9)" a) None

[<Fact>]
[<Trait("requirement", "JYRO-REQ-757")>]
let ``Insert_HasEffectPure`` () = assertPure "Insert" "Insert([1, 2], 1, 9)"

// =============================================================================
// 92.16 RemoveAt
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-758")>]
let ``RemoveAt_TakesArrayOrNullAndIndex_ReturnsArray`` () =
    assertSignature "RemoveAt" [ "items", false; "index", false ]
    typesAre "array" [ "RemoveAt([1, 2], 0)" ]
    Script.expectError MessageCode.IndexOutOfRange (Script.run "Output.r = RemoveAt(null, 0)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-759")>]
let ``RemoveAt_ReturnsEveryElementExceptThatAtIndex`` () =
    evalsTo ""
        [ "RemoveAt([1, 2, 3], 1)", "[1, 3]"; "RemoveAt([1, 2, 3], 0)", "[2, 3]"; "RemoveAt([1, 2, 3], 2)", "[1, 2]"
          "RemoveAt([5], 0)", "[]"; "RemoveAt([1, 1, 1], 1)", "[1, 1]"; "RemoveAt([[1], 2], 0)", "[2]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-760")>]
let ``RemoveAt_WhenIndexBelowZeroOrAboveLengthMinusOne_RaisesIndexOutOfRange`` () =
    let a3 = "var a = [1, 2, 3]"
    errorsWith MessageCode.IndexOutOfRange a3
        [ "RemoveAt(a, -1)"; "RemoveAt(a, 3)"; "RemoveAt(a, 4)"; "RemoveAt(a, 2147483647)"; "RemoveAt(a, -2147483648)"; "RemoveAt([], 0)" ]
    evalsTo a3 [ "RemoveAt(a, 0)", "[2, 3]"; "RemoveAt(a, 2)", "[1, 2]" ]
    errorsWith MessageCode.IntegerRequired a3 [ "RemoveAt(a, -0.5)"; "RemoveAt(a, 3.5)" ]
    errorsWith MessageCode.IntegerOutOfRange a3 [ "RemoveAt(a, 2147483648)"; "RemoveAt(a, -2147483649)" ]
    errorsWith MessageCode.ArgumentTypeMismatch a3 [ "RemoveAt(5, 9)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-761")>]
let ``RemoveAt_IndexIsWholeQuantity`` () =
    let a2 = "var a = [1, 2]\nvar one = Power(1, 0.5)"
    errorsWith MessageCode.IntegerRequired a2 [ "RemoveAt(a, -0.5)"; "RemoveAt(a, 0.5)"; "RemoveAt(a, 1.5)"; "RemoveAt(a, 99.5)" ]
    errorsWith MessageCode.IntegerOutOfRange a2 [ "RemoveAt(a, 2147483648)"; "RemoveAt(a, 1e20)"; "RemoveAt(a, -2147483649)" ]
    evalsTo a2 [ "RemoveAt(a, 1.0)", "[1]"; "RemoveAt(a, one)", "[1]" ]
    errorsWith MessageCode.IndexOutOfRange a2 [ "RemoveAt(a, -1)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-762")>]
let ``RemoveAt_WhenIndexIsNotNumber_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ "RemoveAt([1], \"0\")"; "RemoveAt([1], null)"; "RemoveAt([1], true)"; "RemoveAt([1], [0])"; "RemoveAt([1], {})"; "RemoveAt([1], x => x)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-763")>]
let ``RemoveAt_OfNullItems_IsEmptyArray_IndexOutOfRangeForEveryInt32`` () =
    errorsWith MessageCode.IndexOutOfRange ""
        [ "RemoveAt(null, 0)"; "RemoveAt(null, -1)"; "RemoveAt(null, 1)"; "RemoveAt(null, 2147483647)"; "RemoveAt(null, -2147483648)"
          "RemoveAt([], 0)" ]
    errorsWith MessageCode.IntegerOutOfRange "" [ "RemoveAt(null, 2147483648)"; "RemoveAt(null, -2147483649)"; "RemoveAt(null, 1e20)" ]
    errorsWith MessageCode.IntegerRequired "" [ "RemoveAt(null, 0.5)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-764")>]
let ``RemoveAt_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "RemoveAt(%s, 1)" v) [ "RemoveAt([1, 2], 1)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-765")>]
let ``RemoveAt_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "RemoveAt(%s, 1)" a)
    assertNewArray "[5]" (fun a -> sprintf "RemoveAt(%s, 0)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-766")>]
let ``RemoveAt_DoesNotChangeItemsAndItsLength`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "RemoveAt(%s, 1)" a)
    assertUnchanged "[1, 2]" (fun a -> sprintf "RemoveAt(%s, 0)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-767")>]
let ``RemoveAt_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "RemoveAt([0, %s], 0)" x) 0
    assertElementsShared (fun x -> sprintf "RemoveAt([%s, 0], 1)" x) 0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2023")>]
let ``RemoveAt_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "RemoveAt(%s, 1)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-769")>]
let ``RemoveAt_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3]], \"e\": [1]}" (fun a -> sprintf "RemoveAt(%s, 1)" a) (Some "RemoveAt(Input.e, 0)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-770")>]
let ``RemoveAt_HasEffectPure`` () = assertPure "RemoveAt" "RemoveAt([1, 2], 1)"

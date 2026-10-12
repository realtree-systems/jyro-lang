/// jyro-spec 93.15 RandomInt. The statistical tests use bounds whose chance of
/// failing against a conforming implementation is below one in a million.
module RealTree.Jyro.Spec.Tests.P93_94.S93_15_RandomInt

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

/// Draw `count` values of RandomInt(lo, hi) in one script and return
/// [bad, below, atOrAboveMid] where `bad` counts results that are not integers
/// or are outside [lo, hi), and `mid` splits the range for the band checks.
let private drawSummary (lo: string) (hi: string) (mid: string) (count: int) : float list =
    let src =
        sprintf """var bad = 0
var low = 0
var high = 0
for i in 1 to %d do
  var r = RandomInt(%s, %s)
  if Floor(r) != r or r < %s or r >= %s then
    bad += 1
  end
  if r < %s then
    low += 1
  else
    high += 1
  end
end
Output.r = [bad, low, high]""" count lo hi lo hi mid
    runBig [] src |> Script.output |> Values.prop "r" |> Values.asList |> List.map Values.asFloat

/// Counts of each value of RandomInt(0, n) over `count` draws.
let private drawCounts (n: int) (count: int) : float list =
    let zeros = String.concat ", " [ for _ in 1 .. n -> "0" ]
    let src =
        sprintf """var c = [%s]
var bad = 0
for i in 1 to %d do
  var r = RandomInt(0, %d)
  if Floor(r) != r or r < 0 or r >= %d then
    bad += 1
  else
    c[r] = c[r] + 1
  end
end
Output.r = Append(c, bad)""" zeros count n n
    runBig [] src |> Script.output |> Values.prop "r" |> Values.asList |> List.map Values.asFloat

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2024")>]
let ``JYRO-REQ-2024 RandomInt_Arity_IsTwoArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = RandomInt(1)"
    expectCompileError MessageCode.TooFewArguments "Output.r = RandomInt()"
    expectCompileError MessageCode.TooManyArguments "Output.r = RandomInt(1, 2, 3)"

/// The run succeeds and gives a number no lower than `lo` and no higher than `hi`.
/// (Whether `hi` itself can come is 93.15.4's; this checks only which bound is which.)
let private assertDrawWithin (lo: float) (hi: float) (expr: string) =
    let r = Script.run ("Output.r = " + expr)
    assertNoDiagnostics r
    let v = Values.asFloat (Script.output r |> Values.prop "r")
    if not (v >= lo && v <= hi) then
        Values.fail (sprintf "%s gave %.17g, outside %g to %g" expr v lo hi)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2024")>]
let ``JYRO-REQ-2024 RandomInt_ParametersAreMinAndMax`` () =
    // Bound by name in either order; were they swapped, 10 to 4 would be refused or fall outside.
    for _ in 1 .. 20 do
        assertDrawWithin 4.0 10.0 "RandomInt(min: 4, max: 10)"
        assertDrawWithin 4.0 10.0 "RandomInt(max: 10, min: 4)"
        assertDrawWithin -10.0 -4.0 "RandomInt(max: -4, min: -10)"
    // The pre-ADR-195 names are not parameters.
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = RandomInt(minimum: 0, max: 10)"
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = RandomInt(min: 0, maximum: 10)"
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = RandomInt(minimum: 0, maximum: 10)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2024")>]
let ``JYRO-REQ-2024 RandomInt_ReturnsANumber`` () =
    assertString "number" (eval "TypeOf(RandomInt(0, 10))")
    assertString "number" (eval "TypeOf(RandomInt(-2147483648, 2147483647))")
    assertString "number" (evalIn [ "h", hostInexact 3.0 ] "TypeOf(RandomInt(Input.h, 5))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2025")>]
[<InlineData("0.5, 10")>]
[<InlineData("0, 10.5")>]
[<InlineData("-0.5, 10")>]
[<InlineData("0, 0.0000000000000000000000000001 + 5")>]
[<InlineData("0.9999999999999999999999999999, 10")>]
[<InlineData("0, Power(2, 0.5)")>]
[<InlineData("0.5, 0.25")>]
let ``JYRO-REQ-2025 RandomInt_WithANonIntegerBound_RaisesIntegerRequired`` (args: string) =
    expectExprError MessageCode.IntegerRequired (sprintf "RandomInt(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2025")>]
let ``JYRO-REQ-2025 RandomInt_WithAHostNonIntegerBound_RaisesIntegerRequired`` () =
    expectExprErrorIn [ "h", hostInexact 1.5 ] MessageCode.IntegerRequired "RandomInt(Input.h, 10)"
    expectExprErrorIn [ "h", hostInexact 1.5 ] MessageCode.IntegerRequired "RandomInt(0, Input.h)"
    expectExprErrorIn [ "h", hostExact 2.5m ] MessageCode.IntegerRequired "RandomInt(0, Input.h)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2025")>]
let ``JYRO-REQ-2025 RandomInt_WithIntegralBoundsOfEveryRepresentation_IsAccepted`` () =
    // Only the absence of an error is pinned here; the range of the result is 93.15.4's.
    for (pairs, expr) in
        [ [ "h", hostInexact 0.0 ], "RandomInt(Input.h, 2)"
          [ "h", hostInexact 5.0 ], "RandomInt(0, Input.h)"
          [], "RandomInt(1.0, 2)"
          [], "RandomInt(0, 2.00)"
          [], "RandomInt(Power(1, 0.5), 3)" ] do
        let r = runIn pairs ("Output.r = " + expr)
        assertNoDiagnostics r

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2026")>]
[<InlineData("-2147483649, 0")>]
[<InlineData("0, 2147483648")>]
[<InlineData("0, 1e30")>]
[<InlineData("-1e30, 0")>]
[<InlineData("-2147483649, -2147483648")>]
[<InlineData("2147483647, 2147483648")>]
let ``JYRO-REQ-2026 RandomInt_WithABoundOutsideInt32_RaisesIntegerOutOfRange`` (args: string) =
    expectExprError MessageCode.IntegerOutOfRange (sprintf "RandomInt(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2026")>]
let ``JYRO-REQ-2026 RandomInt_WithAHostBoundOutsideInt32_RaisesIntegerOutOfRange`` () =
    expectExprErrorIn [ "h", hostInexact 2147483648.0 ] MessageCode.IntegerOutOfRange "RandomInt(0, Input.h)"
    expectExprErrorIn [ "h", hostInexact -2147483649.0 ] MessageCode.IntegerOutOfRange "RandomInt(Input.h, 0)"
    expectExprErrorIn [ "h", hostExact 2147483648m ] MessageCode.IntegerOutOfRange "RandomInt(0, Input.h)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2026")>]
let ``JYRO-REQ-2026 RandomInt_WithBoundsAtInt32Limits_IsAccepted`` () =
    // Only the absence of an error is pinned here; the range of the result is 93.15.4's.
    for expr in
        [ "RandomInt(-2147483648, 2147483647)"
          "RandomInt(2147483646, 2147483647)"
          "RandomInt(-2147483648, -2147483647)"
          "RandomInt(-2147483648.0, 2147483647.0)" ] do
        assertNoDiagnostics (Script.run ("Output.r = " + expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2027")>]
let ``JYRO-REQ-2027 RandomInt_OfAOneWideRange_AlwaysReturnsTheMinimum`` () =
    match drawSummary "0" "1" "1" 1000 with
    | [ bad; low; high ] ->
        Assert.Equal(0.0, bad)
        Assert.Equal(0.0, high)
        Assert.True(low >= 999.0, sprintf "only %g draws counted" low)
    | other -> Values.fail (sprintf "Unexpected summary %A" other)
    assertNumber 2147483646.0 (eval "RandomInt(2147483646, 2147483647)")
    assertNumber -2147483648.0 (eval "RandomInt(-2147483648, -2147483647)")
    assertNumber 4.0 (eval "RandomInt(4, 5)")
    assertNumber -1.0 (eval "RandomInt(-1, 0)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2027")>]
let ``JYRO-REQ-2027 RandomInt_OfNegativeRange_StaysInRange_AndNeverReturnsTheMaximum`` () =
    match drawSummary "-3" "-1" "-2" 1000 with
    | [ bad; low; high ] ->
        Assert.Equal(0.0, bad)
        // Both -3 and -2 appear: the chance that one is missing is 2 * 2^-1000.
        Assert.True(low > 0.0 && high > 0.0, sprintf "-3 seen %g times, -2 seen %g times" low high)
    | other -> Values.fail (sprintf "Unexpected summary %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2027")>]
let ``JYRO-REQ-2027 RandomInt_OfTheWidestRange_ReturnsIntegersInRange`` () =
    match drawSummary "-2147483648" "2147483647" "0" 10000 with
    | [ bad; _; _ ] -> Assert.Equal(0.0, bad)
    | other -> Values.fail (sprintf "Unexpected summary %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2027")>]
let ``JYRO-REQ-2027 RandomInt_OfASmallRange_NeverReturnsTheMaximum`` () =
    match drawCounts 6 6000 with
    | counts when counts.Length = 7 -> Assert.Equal(0.0, counts.[6])
    | other -> Values.fail (sprintf "Unexpected counts %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2027")>]
let ``JYRO-REQ-2027 RandomInt_OfARangeAtTheTopOfInt32_NeverReturnsTheMaximum`` () =
    match drawSummary "2147483645" "2147483647" "2147483646" 1000 with
    | [ bad; low; high ] ->
        Assert.Equal(0.0, bad)
        Assert.True(low > 0.0 && high > 0.0, sprintf "2147483645 seen %g times, 2147483646 seen %g times" low high)
    | other -> Values.fail (sprintf "Unexpected summary %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1000")>]
let ``JYRO-REQ-1000 RandomInt_OfSixValues_IsUniform_ByChiSquare`` () =
    let n = 60000
    let counts = drawCounts 6 n
    Assert.Equal(0.0, counts.[6])
    let expected = float n / 6.0
    let chi2 = counts |> List.take 6 |> List.sumBy (fun c -> (c - expected) ** 2.0 / expected)
    // Chi-square with 5 degrees of freedom: P(chi2 > 40) is about 1.5e-7.
    Assert.True(chi2 < 40.0, sprintf "chi-square %g over counts %A" chi2 counts)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1000")>]
let ``JYRO-REQ-1000 RandomInt_OfThreeValues_EachCountIsInItsBand`` () =
    let counts = drawCounts 3 30000
    Assert.Equal(0.0, counts.[3])
    // Each count ~ Binomial(30000, 1/3): sd 81.6, so +-600 is 7.3 sd (chance < 1e-12).
    for c in counts |> List.take 3 do
        Assert.True(c >= 9400.0 && c <= 10600.0, sprintf "counts %A" counts)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1000")>]
let ``JYRO-REQ-1000 RandomInt_OfTheWidestRange_FillsBothHalvesEqually`` () =
    match drawSummary "-2147483648" "2147483647" "0" 20000 with
    | [ bad; low; high ] ->
        Assert.Equal(0.0, bad)
        let share = high / (low + high)
        // sd of the share is 0.0035; 0.47..0.53 is 8.5 sd.
        Assert.True(share >= 0.47 && share <= 0.53, sprintf "upper-half share %g" share)
    | other -> Values.fail (sprintf "Unexpected summary %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1000")>]
let ``JYRO-REQ-1000 RandomInt_OfARangeOfThreeQuartersOfTwoTo32_HasNoModuloBias`` () =
    // Range size 3 * 2^30: a generator that reduces 32 random bits modulo the
    // size would give the lower third of the range twice the weight of the rest.
    // Split at a third: a fair draw puts 1/3 below it.
    match drawSummary "-2147483648" "1073741824" "-1073741824" 20000 with
    | [ bad; low; high ] ->
        Assert.Equal(0.0, bad)
        let share = low / (low + high)
        // sd of the share is 0.0033; 0.30..0.367 is over 10 sd from 1/3 and 1/2.
        Assert.True(share >= 0.30 && share <= 0.367, sprintf "lower-third share %g" share)
    | other -> Values.fail (sprintf "Unexpected summary %A" other)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2028")>]
[<InlineData("5, 5")>]
[<InlineData("6, 5")>]
[<InlineData("2147483647, 2147483647")>]
[<InlineData("0, -2147483648")>]
[<InlineData("-2147483648, -2147483648")>]
let ``JYRO-REQ-2028 RandomInt_WithMinimumNotBelowMaximum_RaisesInvalidRange`` (args: string) =
    expectExprError MessageCode.InvalidRange (sprintf "RandomInt(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2028")>]
let ``JYRO-REQ-2028 RandomInt_WithEqualBoundsOfMixedRepresentation_RaisesInvalidRange`` () =
    expectExprErrorIn [ "h", hostInexact 5.0 ] MessageCode.InvalidRange "RandomInt(5, Input.h)"
    expectExprErrorIn [ "h", hostInexact 5.0 ] MessageCode.InvalidRange "RandomInt(Input.h, 5.0)"
    expectExprErrorIn [ "h", hostInexact 6.0 ] MessageCode.InvalidRange "RandomInt(Input.h, 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2028")>]
let ``JYRO-REQ-2028 RandomInt_WithMinimumBelowMaximum_DoesNotRaiseInvalidRange`` () =
    // Controls: one below, and the widest range.
    for expr in [ "RandomInt(4, 5)"; "RandomInt(-1, 0)"; "RandomInt(-2147483648, 2147483647)"; "RandomInt(2147483646, 2147483647)" ] do
        assertNoDiagnostics (Script.run ("Output.r = " + expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1002")>]
let ``JYRO-REQ-1002 RandomInt_ReturnsAnExactNumber`` () =
    Assert.True(Values.isExact (eval "RandomInt(0, 10)"))
    Assert.True(Values.isExact (eval "RandomInt(-2147483648, 2147483647)"))
    Assert.True(Values.isExact (eval "RandomInt(4, 5)"))
    let r = evalIn [ "lo", hostInexact 0.0; "hi", hostInexact 10.0 ] "RandomInt(Input.lo, Input.hi)"
    Assert.True(Values.isExact r, "RandomInt with inexact bounds must still return an exact number")
    let s = evalScriptIn [] "Output.r = RandomInt(Power(1, 0.5), 3 * Power(1, 0.5))"
    Assert.True(Values.isExact s, "RandomInt with bounds made inexact in the script must still return an exact number")
    assertTrue (eval "RandomInt(1, 2) * 0.1 + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2029")>]
let ``RandomInt_WithNonNumberMinimum_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "RandomInt({}, 10)" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2029")>]
let ``RandomInt_WithNonNumberMaximum_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "RandomInt(0, {})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1004")>]
let ``RandomInt_IsRefusedUnderAPureCeiling`` () =
    let r = Script.compileWith pureOptions "Output.r = RandomInt(1, 6)"
    Script.expectError MessageCode.FunctionEffectExceedsMax r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1004")>]
let ``RandomInt_IsAcceptedUnderANonDeterministicCeiling`` () =
    for effect in [ FunctionEffect.NonDeterministic; FunctionEffect.Blocking; FunctionEffect.Mutating ] do
        let r = Script.runWith { Script.defaults with MaxEffect = Some effect } "Output.r = RandomInt(1, 6)"
        let v = Script.output r |> Values.prop "r" |> Values.asFloat
        Assert.True(v >= 1.0 && v <= 5.0 && v = floor v, sprintf "RandomInt(1, 6) gave %g under %A" v effect)

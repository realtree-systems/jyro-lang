/// jyro-spec 93.4 Round.
module RealTree.Jyro.Spec.Tests.P93_94.S93_04_Round

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private roundExact (args: string) (expected: string) =
    assertExact (dec expected) (eval (sprintf "Round(%s)" args))

/// The value of the result only, whatever its representation: an exact result
/// must equal `expected` exactly, an inexact one must be the double nearest it.
/// (Which representation Round gives is 93.4.12 to 93.4.14 and 93.4.19.)
let private assertValue (what: string) (expected: string) (v: JyroValue) =
    if Values.isExact v then
        let actual = Values.asDecimal v
        if actual <> dec expected then
            Values.fail (sprintf "%s: expected the value %s, got the exact number %M" what expected actual)
    else
        let actual = Values.asFloat v
        if actual <> dbl expected then
            Values.fail (sprintf "%s: expected the value %s, got the inexact number %.17g" what expected actual)

/// `Round(args)` has the value `expected`, whatever its representation.
let private roundValue (args: string) (expected: string) =
    let expr = sprintf "Round(%s)" args
    assertValue expr expected (eval expr)

/// `Round(Input.h, decimals[, mode])` of a host inexact `h` has the value `expected`.
let private roundHostValue (h: float) (rest: string) (expected: string) =
    let expr = sprintf "Round(Input.h, %s)" rest
    assertValue (sprintf "%s with h = %.17g" expr h) expected (evalIn [ "h", hostInexact h ] expr)

/// `Output.r = Round(args)` runs with no diagnostic and gives a number.
let private roundSucceeds (args: string) =
    let r = Script.run (sprintf "Output.r = Round(%s)" args)
    assertNoDiagnostics r
    Values.asFloat (Script.output r |> Values.prop "r") |> ignore

// ---------------------------------------------------------------- 93.4.1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-885")>]
let ``Round_WithOneArgument_IsRejectedWithTooFewArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Round(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-885")>]
let ``Round_WithFourArguments_IsRejectedWithTooManyArguments`` () =
    expectCompileError MessageCode.TooManyArguments "Output.r = Round(1, 2, \"halfUp\", 4)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-885")>]
let ``Round_ParametersAreValueDecimalsAndOptionalMode`` () =
    assertExact 3.14m (eval "Round(value: 3.14159, decimals: 2)")
    assertExact 3.14m (eval "Round(decimals: 2, value: 3.14159)")
    assertExact 2m (eval "Round(value: 2.5, decimals: 0, mode: \"halfEven\")")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Round(v: 1, decimals: 0)"
    expectCompiles "Output.r = Round(2.5, 0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-885")>]
let ``Round_ReturnsANumber`` () =
    assertString "number" (eval "TypeOf(Round(2.5, 0))")
    assertString "number" (eval "TypeOf(Round(2.5, 0, \"halfEven\"))")

// ---------------------------------------------------------------- 93.4.2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-886")>]
let ``JYRO-REQ-886 Round_Of3Point14159To2_Returns3Point14`` () = roundValue "3.14159, 2" "3.14"

/// Values that are not halfway between two multiples, so no mode decides them.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-886")>]
[<InlineData("3.144, 2", "3.14")>]
[<InlineData("3.1451, 2", "3.15")>]
[<InlineData("3.1449, 2", "3.14")>]
[<InlineData("1.0051, 2", "1.01")>]
[<InlineData("0.49, 0", "0")>]
[<InlineData("0.51, 0", "1")>]
[<InlineData("-3.14159, 3", "-3.142")>]
[<InlineData("-3.14149, 3", "-3.141")>]
[<InlineData("2, 5", "2")>]
[<InlineData("0.1, 100", "0.1")>]
[<InlineData("2.7, 0", "3")>]
[<InlineData("-2.7, 0", "-3")>]
[<InlineData("0.0000000000000000000000000006, 27", "0.000000000000000000000000001")>]
let ``JYRO-REQ-886 Round_ReturnsTheNearestMultiple`` (args: string, expected: string) = roundValue args expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-886")>]
let ``JYRO-REQ-886 Round_OfInexact_RoundsItsDoubleValue`` () =
    // The double nearest 2.675 is 2.67499999999999982236431605997495353221893310546875,
    // below 2.675, so the nearest multiple of 0.01 is 2.67.
    roundHostValue 2.675 "2" "2.67"
    // The double nearest 1.15 is 1.149999999999999911182158029987..., so 1.1.
    roundHostValue 1.15 "1" "1.1"
    // The double nearest 2.345 is 2.34500000000000019984014443252817727625370025634765625,
    // above 2.345: no value is halfway, so every mode gives 2.35.
    roundHostValue 2.345 "2" "2.35"
    roundHostValue 2.345 "2, \"halfEven\"" "2.35"
    roundHostValue 2.345 "2, \"halfDown\"" "2.35"

// ---------------------------------------------------------------- 93.4.3

[<Theory>]
[<Trait("requirement", "JYRO-REQ-887")>]
[<InlineData("0.5")>]
[<InlineData("-0.5")>]
[<InlineData("2.0000000000000000000000000001")>]
[<InlineData("1.9999999999999999999999999999")>]
let ``Round_WithNonIntegerDecimals_RaisesIntegerRequired`` (decimals: string) =
    expectExprError MessageCode.IntegerRequired (sprintf "Round(1.5, %s)" decimals)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-887")>]
let ``Round_WithHostInexactNonIntegerDecimals_RaisesIntegerRequired`` () =
    expectExprErrorIn [ "d", hostInexact 2.5 ] MessageCode.IntegerRequired "Round(1.5, Input.d)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-887")>]
let ``Round_WithIntegralDecimals_IsAccepted`` () =
    roundExact "1.25, 2" "1.25"
    roundExact "1.25, 2.0" "1.25"
    assertNumber 1.3 (evalIn [ "d", hostInexact 1.0 ] "Round(1.25, Input.d)")

// ---------------------------------------------------------------- 93.4.4

[<Theory>]
[<Trait("requirement", "JYRO-REQ-888")>]
[<InlineData("2147483648")>]
[<InlineData("-2147483649")>]
[<InlineData("1e30")>]
[<InlineData("-1e30")>]
[<InlineData("79228162514264337593543950335")>]
let ``JYRO-REQ-888 Round_WithDecimalsOutOfInt32Range_RaisesIntegerOutOfRange`` (decimals: string) =
    expectExprError MessageCode.IntegerOutOfRange (sprintf "Round(1.5, %s)" decimals)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-888")>]
let ``JYRO-REQ-888 Round_WithHostDecimalsOutOfInt32Range_RaisesIntegerOutOfRange`` () =
    for d in [ hostInexact 2147483648.0; hostInexact -2147483649.0; hostExact 2147483648m; hostExact -2147483649m ] do
        expectExprErrorIn [ "d", d ] MessageCode.IntegerOutOfRange "Round(1.5, Input.d)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-888")>]
let ``JYRO-REQ-888 Round_WithDecimalsAtInt32Bounds_IsAccepted`` () =
    // Only the absence of an error is pinned here; the values are 93.4.2's and 93.4.6's.
    roundSucceeds "1.5, 2147483647"
    roundSucceeds "1.5, -2147483648"
    roundSucceeds "-1.5, 2147483647.0"
    roundSucceeds "-1.5, -2147483648.0"
    let r = runIn [ "d", hostInexact -2147483648.0 ] "Output.r = Round(1.5, Input.d)"
    assertNoDiagnostics r

// ---------------------------------------------------------------- 93.4.5

[<Fact>]
[<Trait("requirement", "JYRO-REQ-889")>]
let ``JYRO-REQ-889 Round_Of123Point456ToMinus1_Returns120`` () = roundValue "123.456, -1" "120"

/// Values that are not halfway between two multiples, so no mode decides them.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-889")>]
[<InlineData("124, -1", "120")>]
[<InlineData("126, -1", "130")>]
[<InlineData("-126, -1", "-130")>]
[<InlineData("149, -2", "100")>]
[<InlineData("151, -2", "200")>]
[<InlineData("5551, -2", "5600")>]
[<InlineData("99999, -5", "100000")>]
[<InlineData("1234, -3, \"halfEven\"", "1000")>]
[<InlineData("1501, -3, \"halfDown\"", "2000")>]
[<InlineData("-0.6, -1", "0")>]
let ``JYRO-REQ-889 Round_WithNegativeDecimals_RoundsToAPowerOfTen`` (args: string, expected: string) = roundValue args expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-889")>]
let ``JYRO-REQ-889 Round_OfInexactWithNegativeDecimals_RoundsToAPowerOfTen`` () =
    roundHostValue 123.456 "-1" "120"
    roundHostValue 99999.0 "-5" "100000"
    roundHostValue -1234.0 "-2" "-1200"
    roundHostValue 1e300 "-299" "1e300"

// ---------------------------------------------------------------- 93.4.6

[<Fact>]
[<Trait("requirement", "JYRO-REQ-890")>]
let ``JYRO-REQ-890 Round_Of5ToMinus400_Returns0`` () =
    assertNumber 0.0 (eval "Round(5, -400)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-890")>]
[<InlineData("4, -1")>]
[<InlineData("-4, -1")>]
[<InlineData("0.4, 0")>]
[<InlineData("-0.4, 0")>]
[<InlineData("0.004, 2")>]
[<InlineData("0, 5")>]
[<InlineData("5, -2147483648")>]
[<InlineData("79228162514264337593543950335, -400")>]
[<InlineData("79228162514264337593543950335, -30")>]
[<InlineData("1e300, -400")>]
[<InlineData("4, -1, \"halfEven\"")>]
[<InlineData("4, -1, \"halfDown\"")>]
let ``JYRO-REQ-890 Round_WhenTheNearestMultipleIsZero_Returns0`` (args: string) =
    let r = Script.run (sprintf "Output.r = Round(%s)" args)
    assertNoDiagnostics r
    assertNumber 0.0 (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-890")>]
let ``JYRO-REQ-890 Round_OfInexactWhenTheNearestMultipleIsZero_Returns0`` () =
    for (h, d) in [ 5.0, -400; 1e300, -2147483648; 0.4, 0; 1e-300, 2 ] do
        let r = runIn [ "h", hostInexact h; "d", Values.int d ] "Output.r = Round(Input.h, Input.d)"
        assertNoDiagnostics r
        assertNumber 0.0 (Script.output r |> Values.prop "r")

// ---------------------------------------------------------------- 93.4.7 to 93.4.10

[<Fact>]
[<Trait("requirement", "JYRO-REQ-891")>]
let ``JYRO-REQ-891 Round_HalfUpOfMinus2Point5_ReturnsMinus3`` () = roundValue "-2.5, 0, \"halfUp\"" "-3"

/// Each value is halfway between two multiples.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-891")>]
[<InlineData("2.5, 0", "3")>]
[<InlineData("0.5, 0", "1")>]
[<InlineData("-0.5, 0", "-1")>]
[<InlineData("0.125, 2", "0.13")>]
[<InlineData("-0.125, 2", "-0.13")>]
[<InlineData("1.005, 2", "1.01")>]
[<InlineData("25, -1", "30")>]
[<InlineData("-25, -1", "-30")>]
[<InlineData("150, -2", "200")>]
let ``JYRO-REQ-891 Round_HalfUp_RoundsHalfwayAwayFromZero`` (args: string, expected: string) =
    roundValue (args + ", \"halfUp\"") expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-891")>]
let ``JYRO-REQ-891 Round_HalfUpOfAnInexactHalfway_RoundsAwayFromZero`` () =
    // 2.5, 0.125 and 0.375 are doubles exactly, so each is halfway.
    roundHostValue 2.5 "0, \"halfUp\"" "3"
    roundHostValue -2.5 "0, \"halfUp\"" "-3"
    roundHostValue -0.125 "2, \"halfUp\"" "-0.13"
    roundHostValue 0.375 "2, \"halfUp\"" "0.38"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-892")>]
let ``JYRO-REQ-892 Round_HalfEvenOf2Point5_Returns2`` () = roundValue "2.5, 0, \"halfEven\"" "2"

/// Each value is halfway between two multiples.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-892")>]
[<InlineData("3.5, 0", "4")>]
[<InlineData("-2.5, 0", "-2")>]
[<InlineData("-3.5, 0", "-4")>]
[<InlineData("0.5, 0", "0")>]
[<InlineData("1.5, 0", "2")>]
[<InlineData("0.125, 2", "0.12")>]
[<InlineData("0.135, 2", "0.14")>]
[<InlineData("25, -1", "20")>]
[<InlineData("35, -1", "40")>]
[<InlineData("250, -2", "200")>]
[<InlineData("-150, -2", "-200")>]
let ``JYRO-REQ-892 Round_HalfEven_RoundsHalfwayToEven`` (args: string, expected: string) =
    roundValue (args + ", \"halfEven\"") expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-892")>]
let ``JYRO-REQ-892 Round_HalfEvenOfAnInexactHalfway_RoundsToEven`` () =
    roundHostValue 2.5 "0, \"halfEven\"" "2"
    roundHostValue -3.5 "0, \"halfEven\"" "-4"
    roundHostValue 0.125 "2, \"halfEven\"" "0.12"
    roundHostValue 0.375 "2, \"halfEven\"" "0.38"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-893")>]
let ``JYRO-REQ-893 Round_HalfDownOf2Point7_Returns3`` () = roundValue "2.7, 0, \"halfDown\"" "3"

/// Each value is halfway between two multiples.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-893")>]
[<InlineData("2.5, 0", "2")>]
[<InlineData("-2.5, 0", "-2")>]
[<InlineData("3.5, 0", "3")>]
[<InlineData("0.125, 2", "0.12")>]
[<InlineData("-0.125, 2", "-0.12")>]
[<InlineData("25, -1", "20")>]
[<InlineData("-150, -2", "-100")>]
let ``JYRO-REQ-893 Round_HalfDown_RoundsHalfwayTowardZero`` (args: string, expected: string) =
    roundValue (args + ", \"halfDown\"") expected

/// Values that are not halfway: each goes to the nearest multiple, never toward zero by truncation.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-893")>]
[<InlineData("2.51, 0", "3")>]
[<InlineData("2.3, 0", "2")>]
[<InlineData("-2.7, 0", "-3")>]
[<InlineData("-2.51, 0", "-3")>]
[<InlineData("0.126, 2", "0.13")>]
[<InlineData("2.9999, 0", "3")>]
[<InlineData("26, -1", "30")>]
[<InlineData("-0.6, 0", "-1")>]
let ``JYRO-REQ-893 Round_HalfDown_RoundsOtherValuesToTheNearest`` (args: string, expected: string) =
    roundValue (args + ", \"halfDown\"") expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-893")>]
let ``JYRO-REQ-893 Round_HalfDownOfInexact_HalfwayTowardZero_OthersNearest`` () =
    roundHostValue 2.5 "0, \"halfDown\"" "2"
    roundHostValue -0.125 "2, \"halfDown\"" "-0.12"
    // The double nearest 2.7 is above 2.7: the nearest whole number is 3.
    roundHostValue 2.7 "0, \"halfDown\"" "3"
    roundHostValue -2.7 "0, \"halfDown\"" "-3"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-894")>]
let ``JYRO-REQ-894 Round_Of2Point5To0_Returns3`` () = roundValue "2.5, 0" "3"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-894")>]
let ``JYRO-REQ-894 Round_WithNoMode_UsesHalfUp`` () =
    roundValue "-2.5, 0" "-3"
    roundValue "0.5, 0" "1"
    roundValue "0.125, 2" "0.13"
    roundValue "-0.125, 2" "-0.13"
    roundValue "25, -1" "30"
    roundValue "-150, -2" "-200"
    roundHostValue 2.5 "0" "3"
    roundHostValue -0.375 "2" "-0.38"
    for (v, d) in [ "2.5", "0"; "-2.5", "0"; "0.125", "2"; "25", "-1"; "3.5", "0"; "2.7", "0" ] do
        assertTrue (eval (sprintf "Round(%s, %s) == Round(%s, %s, \"halfUp\")" v d v d))

// ---------------------------------------------------------------- 93.4.11

[<Theory>]
[<Trait("requirement", "JYRO-REQ-895")>]
[<InlineData("\"HalfUp\"")>]
[<InlineData("\"HALFEVEN\"")>]
[<InlineData("\"halfup\"")>]
[<InlineData("\"halfeven\"")>]
[<InlineData("\"HalfDown\"")>]
[<InlineData("\"halfdown\"")>]
[<InlineData("\" halfUp\"")>]
[<InlineData("\"halfUp \"")>]
[<InlineData("\"\"")>]
[<InlineData("\"up\"")>]
[<InlineData("\"halfOdd\"")>]
[<InlineData("1")>]
[<InlineData("true")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("[\"halfUp\"]")>]
[<InlineData("{mode: \"halfUp\"}")>]
[<InlineData("0")>]
[<InlineData("-1")>]
[<InlineData("2.5")>]
[<InlineData("false")>]
[<InlineData("x => x")>]
[<InlineData("() => \"halfUp\"")>]
let ``JYRO-REQ-895 Round_WithAnUnknownMode_RaisesInvalidRoundingMode`` (mode: string) =
    // One error only, of InvalidRoundingMode, so never ArgumentTypeMismatch for `mode`.
    expectExprError MessageCode.InvalidRoundingMode (sprintf "Round(2.5, 0, %s)" mode)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 Round_WithANonStringModeFromInput_RaisesInvalidRoundingModeNotArgumentTypeMismatch`` () =
    for v in [ Values.exact 1m; hostInexact 1.0; Values.bool true; Values.bool false; Values.arr []; Values.emptyObj () ] do
        expectExprErrorIn [ "m", v ] MessageCode.InvalidRoundingMode "Round(2.5, 0, Input.m)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 Round_WithANonStringModeInAVariable_RaisesInvalidRoundingMode`` () =
    expectRuntimeError MessageCode.InvalidRoundingMode "var m = (x) => x\nOutput.r = Round(2.5, 0, m)"
    expectRuntimeError MessageCode.InvalidRoundingMode "var m = [1, 2]\nOutput.r = Round(2.5, 0, m)"
    expectRuntimeError MessageCode.InvalidRoundingMode "Output.r = Round(value: 2.5, decimals: 0, mode: 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 Round_WithEachKnownMode_IsNotAnError`` () =
    roundValue "2.5, 0, \"halfUp\"" "3"
    roundValue "2.5, 0, \"halfEven\"" "2"
    roundValue "2.5, 0, \"halfDown\"" "2"
    for m in [ "halfUp"; "halfEven"; "halfDown" ] do
        let r = runIn [ "m", Values.str m ] "Output.r = Round(2.5, 0, Input.m)"
        assertNoDiagnostics r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 Round_WithAnUnknownModeFromInput_RaisesInvalidRoundingMode`` () =
    for s in [ "HALFUP"; "HalfUp"; "halfup"; "half_up"; "" ] do
        expectExprErrorIn [ "m", Values.str s ] MessageCode.InvalidRoundingMode "Round(2.5, 0, Input.m)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 Round_WithNullMode_UsesHalfUp`` () =
    roundValue "2.5, 0, null" "3"
    roundValue "-2.5, 0, null" "-3"
    roundValue "0.125, 2, null" "0.13"
    assertValue "Round(value: 2.5, decimals: 0, mode: null)" "3" (eval "Round(value: 2.5, decimals: 0, mode: null)")
    assertValue "Round(2.5, 0, Input.m) with m null" "3" (evalIn [ "m", Values.nul ] "Round(2.5, 0, Input.m)")
    assertValue "Round(2.5, 0, Input.missing)" "3" (eval "Round(2.5, 0, Input.missing)")

// ---------------------------------------------------------------- 93.4.12 to 93.4.14

[<Theory>]
[<Trait("requirement", "JYRO-REQ-896")>]
[<InlineData("3.14159, 2", "3.14")>]
[<InlineData("2.5, 0", "3")>]
[<InlineData("1, 0", "1")>]
[<InlineData("0.1, 28", "0.1")>]
[<InlineData("0.1, 100", "0.1")>]
[<InlineData("0.0000000000000000000000000001, 28", "0.0000000000000000000000000001")>]
[<InlineData("79228162514264337593543950335, 0", "79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950335, 0", "-79228162514264337593543950335")>]
let ``Round_OfExactWithNonNegativeDecimals_IsExact`` (args: string, expected: string) = roundExact args expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-897")>]
[<InlineData("1234567890123456789012345, -2", "1234567890123456789012300")>]
[<InlineData("79228162514264337593543950334, -1", "79228162514264337593543950330")>]
[<InlineData("79228162514264337593543950335, -1, \"halfDown\"", "79228162514264337593543950330")>]
[<InlineData("-79228162514264337593543950335, -1, \"halfDown\"", "-79228162514264337593543950330")>]
[<InlineData("10000000000000000000000000000, -28", "10000000000000000000000000000")>]
[<InlineData("70000000000000000000000000000, -28", "70000000000000000000000000000")>]
[<InlineData("123.456, -1", "120")>]
[<InlineData("-126, -1", "-130")>]
[<InlineData("149, -2", "100")>]
[<InlineData("99999, -5", "100000")>]
[<InlineData("0.5, -1", "0")>]
[<InlineData("25, -1", "30")>]
[<InlineData("25, -1, \"halfEven\"", "20")>]
let ``JYRO-REQ-897 Round_OfExactWithNegativeDecimals_WhenHeld_IsExact`` (args: string, expected: string) = roundExact args expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-897")>]
let ``JYRO-REQ-897 Round_OfExactWithNegativeDecimals_KeepsExactArithmetic`` () =
    assertTrue (eval "Round(1234567890123456789012345, -2) + 45 == 1234567890123456789012345")
    assertTrue (eval "Round(1234, -2) * 0.1 + 0.2 == 120.2")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-897")>]
let ``JYRO-REQ-897 Round_OfHostExactWithNegativeDecimals_IsExact`` () =
    assertExact 1200m (evalIn [ "v", hostExact 1234m ] "Round(Input.v, -2)")
    assertExact (dec "79228162514264337593543950330") (evalIn [ "v", hostExact (dec "79228162514264337593543950334") ] "Round(Input.v, -1)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-898")>]
[<InlineData(3.14159, 2)>]
[<InlineData(2.5, 0)>]
[<InlineData(3.14159, -2)>]
[<InlineData(3.14159, 20)>]
[<InlineData(2.0, 3)>]
let ``Round_OfHostInexact_IsInexact`` (value: float, decimals: int) =
    let r = evalIn [ "h", hostInexact value; "d", Values.int decimals ] "Round(Input.h, Input.d)"
    Assert.False(Values.isExact r, "Round of an inexact value must be inexact")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-898")>]
let ``Round_OfInexactLiteral_IsInexact`` () =
    assertInexact 1e300 (eval "Round(1e300, 2)")
    assertInexact 1e300 (eval "Round(1e300, -2)")

// ---------------------------------------------------------------- 93.4.15

[<Fact>]
[<Trait("requirement", "JYRO-REQ-899")>]
let ``JYRO-REQ-899 Round_OfPowerOf2Minus20To18_Returns0Point000000953674316406`` () =
    assertTrue (eval "Round(Power(2, -20), 18) == 0.000000953674316406")
    // The same with an inexact 2^-20 made in the script (an inexact exponent).
    assertTrue (eval "Round(Power(2, -20 * Power(1, 0.5)), 18) == 0.000000953674316406")

/// 2^-20 = 0.00000095367431640625 and 2^-30 = 0.000000000931322574615478515625, both doubles exactly.
/// Each expected value is the multiple nearest the double, worked out from its decimal digits,
/// and none is the double itself, so a Round that leaves the value alone fails.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-899")>]
[<InlineData(-20, 18, "0.000000953674316406")>]
[<InlineData(-20, 16, "0.0000009536743164")>]
[<InlineData(-30, 16, "0.0000000009313226")>]
[<InlineData(-30, 18, "0.000000000931322575")>]
[<InlineData(-30, 20, "0.00000000093132257462")>]
let ``JYRO-REQ-899 Round_OfInexactWithMoreThan15Decimals_StillRoundsToTheNearestMultiple`` (power: int, decimals: int, expected: string) =
    let h = hostInexact (Math.Pow(2.0, float power))
    assertNumber (dbl expected) (evalIn [ "h", h; "d", Values.int decimals ] "Round(Input.h, Input.d)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-899")>]
let ``JYRO-REQ-899 Round_OfInexactWhenTheMultipleIsTheValue_KeepsIt`` () =
    // 2^-20 has 20 decimal places: to 30 places it is its own nearest multiple.
    assertNumber (Math.Pow(2.0, -20.0)) (evalIn [ "h", hostInexact (Math.Pow(2.0, -20.0)) ] "Round(Input.h, 30)")
    // 0.1 as a double is 0.1000000000000000055511...; to 17 places, 0.10000000000000001, whose nearest double is that double.
    assertNumber (dbl "0.10000000000000001") (evalIn [ "h", hostInexact 0.1 ] "Round(Input.h, 17)")

// ---------------------------------------------------------------- 93.4.16

[<Theory>]
[<Trait("requirement", "JYRO-REQ-900")>]
[<InlineData("1.7e308, -308")>]
[<InlineData("-1.7e308, -308")>]
[<InlineData("1.6e308, -308")>]
[<InlineData("1.7e308, -308, \"halfDown\"")>]
let ``JYRO-REQ-900 Round_WhenTheResultIsNotFinite_RaisesNumberOverflow`` (args: string) =
    // The nearest multiple of 1e308 is 2e308 (or -2e308), beyond the largest double.
    expectExprError MessageCode.NumberOverflow (sprintf "Round(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-900")>]
let ``JYRO-REQ-900 Round_OfHostInexactWhenTheResultIsNotFinite_RaisesNumberOverflow`` () =
    expectExprErrorIn [ "h", hostInexact Double.MaxValue ] MessageCode.NumberOverflow "Round(Input.h, -308)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-900")>]
let ``JYRO-REQ-900 Round_WhenTheResultIsFinite_DoesNotOverflow`` () =
    assertNumber 1.7e308 (eval "Round(1.7e308, -307)")
    assertNumber 1e308 (eval "Round(1.4e308, -308)")
    assertNumber Double.MaxValue (evalIn [ "h", hostInexact Double.MaxValue ] "Round(Input.h, 0)")

// ---------------------------------------------------------------- 93.4.17

[<Fact>]
[<Trait("requirement", "JYRO-REQ-901")>]
let ``Round_WithNonNumberValue_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Round({}, 2)" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-901")>]
let ``Round_WithNonNumberDecimals_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Round(1.5, {})" badLiterals

// ---------------------------------------------------------------- 93.4.18

[<Fact>]
[<Trait("requirement", "JYRO-REQ-902")>]
let ``Round_IsPure`` () = assertPure "Round(2.5, 0)" 3.0

// ---------------------------------------------------------------- 93.4.19

[<Fact>]
[<Trait("requirement", "JYRO-REQ-903")>]
let ``JYRO-REQ-903 Round_OfMaxExactToMinus1_ReturnsInexact2To96`` () =
    // 79228162514264337593543950335 is halfway; halfUp gives 79228162514264337593543950340,
    // above the largest exact value, whose nearest double is 2^96.
    let r = Script.run "Output.r = Round(79228162514264337593543950335, -1)"
    assertNoDiagnostics r
    assertInexact (Math.Pow(2.0, 96.0)) (Script.output r |> Values.prop "r")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-903")>]
[<InlineData("-79228162514264337593543950335, -1", "-79228162514264337593543950336")>]
[<InlineData("79228162514264337593543950335, -1, \"halfEven\"", "79228162514264337593543950336")>]
[<InlineData("79228162514264337593543950335, -1, \"halfUp\"", "79228162514264337593543950336")>]
[<InlineData("79228162514264337593543950335, -29", "1e29")>]
[<InlineData("-79228162514264337593543950335, -29", "-1e29")>]
[<InlineData("79228162514264337593543950335, -28", "8e28")>]
[<InlineData("-79228162514264337593543950335, -28", "-8e28")>]
[<InlineData("60000000000000000000000000000, -29", "1e29")>]
let ``JYRO-REQ-903 Round_WhenTheMultipleIsNotHeldExactly_ReturnsTheNearestDouble`` (args: string, expected: string) =
    let r = Script.run (sprintf "Output.r = Round(%s)" args)
    assertNoDiagnostics r
    assertInexact (dbl expected) (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-903")>]
let ``JYRO-REQ-903 Round_OfHostExactWhenTheMultipleIsNotHeld_ReturnsTheNearestDouble`` () =
    let r = runIn [ "v", hostExact (dec "79228162514264337593543950335") ] "Output.r = Round(Input.v, -1)"
    assertNoDiagnostics r
    assertInexact (Math.Pow(2.0, 96.0)) (Script.output r |> Values.prop "r")

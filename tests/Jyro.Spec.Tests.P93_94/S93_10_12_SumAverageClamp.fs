/// jyro-spec 93.10 Sum, 93.11 Average, 93.12 Clamp.
module RealTree.Jyro.Spec.Tests.P93_94.S93_10_12_SumAverageClamp

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private nonArrayLiterals = [ "5"; "\"12\""; "\"\""; "true"; "{}"; "{a: 1}"; "f" ]

let private expectNonArrayMismatch (fn: string) =
    for lit in nonArrayLiterals do
        expectRuntimeError MessageCode.ArgumentTypeMismatch (withFn (sprintf "%s(%s)" fn lit))
    for v in [ Values.exact 5m; Values.str "12"; Values.bool true; Values.obj [ "a", Values.exact 1m ] ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch (sprintf "%s(Input.v)" fn)

/// The code only: the arguments of the diagnostic are 90.7.4's, verified with that requirement.
let private expectElementError (fn: string) (values: string) (_position: int) =
    let m = runtimeErrorIn [] (sprintf "Output.r = %s(%s)" fn values)
    Assert.Equal(MessageCode.NumericElementRequired, m.Code)

// ================================================================= 93.10 Sum

[<Fact>]
[<Trait("requirement", "JYRO-REQ-947")>]
let ``Sum_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Sum()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Sum([1], [2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-947")>]
let ``Sum_ParameterIsNamedValues_AndAlwaysReturnsANumber`` () =
    assertExact 3m (eval "Sum(values: [1, 2])")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Sum(items: [1])"
    assertString "number" (eval "TypeOf(Sum([1, 2]))")
    assertString "number" (eval "TypeOf(Sum([]))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-948")>]
[<InlineData("[1, 2, 3]", "6")>]
[<InlineData("[-1, 1]", "0")>]
[<InlineData("[]", "0")>]
[<InlineData("[null]", "0")>]
[<InlineData("[null, 5, null]", "5")>]
[<InlineData("[1.5, 2.25, -0.75]", "3")>]
let ``Sum_ReturnsTheTotalOfTheNumbers`` (values: string, expected: string) =
    assertNumber (float (dec expected)) (eval (sprintf "Sum(%s)" values))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-948")>]
[<InlineData("[1, \"2\"]", 1)>]
[<InlineData("[1, true]", 1)>]
[<InlineData("[1, [1]]", 1)>]
[<InlineData("[1, {}]", 1)>]
[<InlineData("[1, null, \"a\"]", 2)>]
let ``Sum_WithANonNumberElement_RaisesNumericElementRequired`` (values: string, position: int) =
    expectElementError "Sum" values position

[<Fact>]
[<Trait("requirement", "JYRO-REQ-949")>]
let ``Sum_OfNull_Returns0`` () =
    assertNumber 0.0 (eval "Sum(null)")
    assertNumber 0.0 (eval "Sum(values: null)")
    assertNumber 0.0 (evalIn [ "v", Values.nul ] "Sum(Input.v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-950")>]
let ``Sum_OfNonArray_RaisesArgumentTypeMismatch`` () = expectNonArrayMismatch "Sum"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-951")>]
let ``JYRO-REQ-951 Sum_OfOneTenthAndTwoTenths_ReturnsExact0Point3`` () = assertExact 0.3m (eval "Sum([0.1, 0.2])")

/// The total is held; some running totals are not, which does not matter.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-951")>]
[<InlineData("[79228162514264337593543950335, 1, -1]", "79228162514264337593543950335")>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950335, -79228162514264337593543950335]", "79228162514264337593543950335")>]
[<InlineData("[79228162514264337593543950335, -79228162514264337593543950335]", "0")>]
[<InlineData("[0.0000000000000000000000000001, 0.0000000000000000000000000001]", "0.0000000000000000000000000002")>]
[<InlineData("[0.1, null, 0.2, 0.3]", "0.6")>]
[<InlineData("[5, 0.0000000000000000000000000001]", "5.0000000000000000000000000001")>]
[<InlineData("[0.3333333333333333333333333333, 0.6666666666666666666666666667]", "1")>]
[<InlineData("[1.5, -1.5]", "0")>]
let ``JYRO-REQ-951 Sum_OfExactNumbersWithAHeldTotal_IsExact`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Sum(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-951")>]
let ``JYRO-REQ-951 Sum_OfAnExactInputArray_IsExact`` () =
    let a = Values.arr [ hostExact 0.1m; Values.nul; hostExact 0.2m ]
    assertExact 0.3m (evalIn [ "a", a ] "Sum(Input.a)")
    assertTrue (evalIn [ "a", a ] "Sum(Input.a) == 0.3")

/// The exact total needs more digits than the exact representation holds (30.2.4),
/// so the numbers are added as doubles.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-952")>]
[<InlineData("[79228162514264337593543950335, 1]", 7.922816251426434e28)>]
[<InlineData("[79228162514264337593543950335, 0.1]", 7.922816251426434e28)>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950335]", 1.5845632502852868e29)>]
[<InlineData("[10, 0.0000000000000000000000000001]", 10.0)>]
[<InlineData("[8, 0.0000000000000000000000000001]", 8.0)>]
let ``JYRO-REQ-952 Sum_OfExactNumbersWithATotalNotHeld_IsInexactDouble`` (values: string, expected: float) =
    assertInexact expected (eval (sprintf "Sum(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-953")>]
let ``Sum_WithAnInexactElement_AddsInDoublePrecisionInOrderOfPosition`` () =
    let big = hostInexact 1e16
    assertInexact 1e16 (evalIn [ "a", Values.arr [ big; hostExact 1m; hostExact 1m ] ] "Sum(Input.a)")
    assertInexact 10000000000000002.0 (evalIn [ "a", Values.arr [ hostExact 1m; hostExact 1m; hostInexact 1e16 ] ] "Sum(Input.a)")
    assertInexact 0.30000000000000004 (evalIn [ "h", hostInexact 0.2 ] "Sum([0.1, Input.h])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-954")>]
[<InlineData("[1e308, 1e308]")>]
[<InlineData("[-1e308, -1e308]")>]
[<InlineData("[1e308, 1e308, -1e308]")>]
let ``Sum_WhenAnInexactTotalIsAnInfinity_RaisesNumberOverflow`` (values: string) =
    expectExprError MessageCode.NumberOverflow (sprintf "Sum(%s)" values)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-954")>]
let ``Sum_WhenTheRunningTotalStaysFinite_DoesNotOverflow`` () =
    assertInexact 1e308 (eval "Sum([1e308, -1e308, 1e308])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-955")>]
let ``Sum_IsPure_AndDoesNotChangeItsArgument`` () =
    assertCompilesPure "Sum([3, null, 1])"
    assertTrue (evalScriptIn [] "var a = [3, null, 1]\nvar s = Sum(a)\nOutput.r = a == [3, null, 1] and Length(a) == 3")

// ================================================================= 93.11 Average

[<Fact>]
[<Trait("requirement", "JYRO-REQ-956")>]
let ``Average_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Average()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Average([1], [2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-956")>]
let ``Average_ParameterIsNamedValues_AndReturnsANumberOrNull`` () =
    assertExact 1.5m (eval "Average(values: [1, 2])")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Average(items: [1])"
    assertString "number" (eval "TypeOf(Average([1, 2]))")
    assertString "null" (eval "TypeOf(Average([]))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-957")>]
[<InlineData("[1, 2, 3]", "2")>]
[<InlineData("[2, null, 4]", "3")>]
[<InlineData("[5]", "5")>]
[<InlineData("[-1, 1]", "0")>]
[<InlineData("[null, 1, null, 2, null, 3, null]", "2")>]
let ``Average_ReturnsTheTotalDividedByTheCountOfNumbers`` (values: string, expected: string) =
    assertNumber (float (dec expected)) (eval (sprintf "Average(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-957")>]
let ``Average_OfNoNumbers_ReturnsNull`` () =
    assertNull (eval "Average([])")
    assertNull (eval "Average([null, null])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-957")>]
[<InlineData("[1, \"a\"]", 1)>]
[<InlineData("[1, false]", 1)>]
[<InlineData("[null, [], 1]", 1)>]
let ``Average_WithANonNumberElement_RaisesNumericElementRequired`` (values: string, position: int) =
    expectElementError "Average" values position

[<Fact>]
[<Trait("requirement", "JYRO-REQ-958")>]
let ``Average_OfNull_ReturnsNull`` () =
    assertNull (eval "Average(null)")
    assertNull (eval "Average(values: null)")
    assertNull (evalIn [ "v", Values.nul ] "Average(Input.v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-959")>]
let ``Average_OfNonArray_RaisesArgumentTypeMismatch`` () = expectNonArrayMismatch "Average"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-960")>]
let ``JYRO-REQ-960 Average_Of1And2_ReturnsExact1Point5`` () = assertExact 1.5m (eval "Average([1, 2])")

/// The mean is held; for some the total is not, which does not matter.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-960")>]
[<InlineData("[0.1, 0.2]", "0.15")>]
[<InlineData("[1, 2, 3, 4]", "2.5")>]
[<InlineData("[1, 2, 3]", "2")>]
[<InlineData("[1, null, 2]", "1.5")>]
[<InlineData("[0.0000000000000000000000000001, 0.0000000000000000000000000003]", "0.0000000000000000000000000002")>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950335]", "79228162514264337593543950335")>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950335, 79228162514264337593543950335]", "79228162514264337593543950335")>]
[<InlineData("[-79228162514264337593543950335, -79228162514264337593543950335]", "-79228162514264337593543950335")>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950333]", "79228162514264337593543950334")>]
let ``JYRO-REQ-960 Average_OfExactNumbersWithAHeldMean_IsExact`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Average(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-960")>]
let ``JYRO-REQ-960 Average_OfExactNumbers_KeepsExactArithmetic`` () =
    assertTrue (eval "Average([0.1, 0.3]) + 0.1 == 0.3")
    assertExact 0.15m (evalIn [ "a", Values.arr [ hostExact 0.1m; hostExact 0.2m ] ] "Average(Input.a)")

/// The exact mean needs more digits than the exact representation holds.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-961")>]
let ``JYRO-REQ-961 Average_Of1And1And2_IsInexact`` () =
    assertInexact (4.0 / 3.0) (eval "Average([1, 1, 2])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-961")>]
[<InlineData("[1, 2, 4]", 7.0, 3.0)>]
[<InlineData("[1, 0, 0]", 1.0, 3.0)>]
[<InlineData("[2, 0, 0, 0, 0, 0, 0]", 2.0, 7.0)>]
let ``JYRO-REQ-961 Average_WhenTheMeanIsARecurringFraction_IsInexactDouble`` (values: string, total: float, count: float) =
    assertInexact (total / count) (eval (sprintf "Average(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-961")>]
let ``JYRO-REQ-961 Average_WhenTheMeanNeeds29Places_IsInexact`` () =
    assertInexact 5e-29 (eval "Average([0, 0.0000000000000000000000000001])")
    assertInexact 0.5 (eval "Average([1, 0.0000000000000000000000000001])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-962")>]
let ``Average_WithAnInexactElement_IsInexactDouble`` () =
    assertInexact 1.5 (evalIn [ "h", hostInexact 2.0 ] "Average([1, Input.h])")
    assertInexact 0.15000000000000002 (evalIn [ "h", hostInexact 0.2 ] "Average([0.1, Input.h])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-963")>]
let ``JYRO-REQ-963 Average_Of1e308Twice_Returns1e308`` () =
    let r = Script.run "Output.r = Average([1e308, 1e308])"
    assertNoDiagnostics r
    assertNumber 1e308 (Script.output r |> Values.prop "r")

/// The value only, near the mean; its representation is 93.11.7's.
let private assertNear (expected: float) (v: JyroValue) =
    let actual = Values.asFloat v
    if abs (actual - expected) > abs expected * 1e-15 then
        Values.fail (sprintf "Expected a number near %.17g, got %.17g" expected actual)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-963")>]
let ``JYRO-REQ-963 Average_WhenTheTotalIsNotFinite_ReturnsTheFiniteMean`` () =
    for (values, mean) in
        [ "[-1e308, -1e308]", -1e308
          "[1.7e308, 1.7e308, 1.7e308]", 1.7e308
          "[1.7e308, 1.3e308]", 1.5e308
          "[1e308, 1e308, null, 1e308, 1e308]", 1e308
          "[1.7e308, 1.7e308, -1.7e308]", 1.7e308 / 3.0 ] do
        let r = Script.run (sprintf "Output.r = Average(%s)" values)
        assertNoDiagnostics r
        assertNear mean (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-963")>]
let ``JYRO-REQ-963 Average_OfHostDoublesWhoseTotalIsNotFinite_ReturnsTheFiniteMean`` () =
    let a = Values.arr [ hostInexact Double.MaxValue; hostInexact Double.MaxValue ]
    let r = runIn [ "a", a ] "Output.r = Average(Input.a)"
    assertNoDiagnostics r
    assertNear Double.MaxValue (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-964")>]
let ``Average_IsPure_AndDoesNotChangeItsArgument`` () =
    assertCompilesPure "Average([1, null, 2])"
    assertTrue (evalScriptIn [] "var a = [1, null, 2]\nvar s = Average(a)\nOutput.r = a == [1, null, 2] and Length(a) == 3")

// ================================================================= 93.12 Clamp

[<Fact>]
[<Trait("requirement", "JYRO-REQ-965")>]
let ``Clamp_Arity_IsThreeArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Clamp(1, 2)"
    expectCompileError MessageCode.TooManyArguments "Output.r = Clamp(1, 2, 3, 4)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-965")>]
let ``Clamp_ParametersAreValueMinMax_AndReturnsANumber`` () =
    assertExact 3m (eval "Clamp(value: 5, min: 1, max: 3)")
    assertExact 3m (eval "Clamp(max: 3, value: 5, min: 1)")
    assertExact 1m (eval "Clamp(min: 1, max: 3, value: 0)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Clamp(v: 5, min: 1, max: 3)"
    assertString "number" (eval "TypeOf(Clamp(5, 1, 3))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-966")>]
let ``Clamp_Of5Between1And3_Returns3`` () = assertExact 3m (eval "Clamp(5, 1, 3)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-966")>]
[<InlineData("0, 1, 3", "1")>]
[<InlineData("2, 1, 3", "2")>]
[<InlineData("1, 1, 3", "1")>]
[<InlineData("3, 1, 3", "3")>]
[<InlineData("-5, -3, -1", "-3")>]
[<InlineData("0, -3, -1", "-1")>]
[<InlineData("0.9999999999999999999999999999, 1, 3", "1")>]
[<InlineData("1.0000000000000000000000000001, 1, 3", "1.0000000000000000000000000001")>]
[<InlineData("2.9999999999999999999999999999, 1, 3", "2.9999999999999999999999999999")>]
[<InlineData("3.0000000000000000000000000001, 1, 3", "3")>]
let ``Clamp_ReturnsMinBelowMaxAboveAndValueOtherwise`` (args: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Clamp(%s)" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-967")>]
let ``JYRO-REQ-967 Clamp_OrdersExactArgumentsByExactValue`` () =
    // Each pair has one double, so only an exact comparison (30.11.1) tells them apart.
    assertExact (dec "0.1000000000000000000000000001") (eval "Clamp(0.1, 0.1000000000000000000000000001, 1)")
    assertExact (dec "0.1999999999999999999999999999") (eval "Clamp(0.2, 0, 0.1999999999999999999999999999)")
    assertExact (dec "0.1000000000000000000000000001") (eval "Clamp(0.1000000000000000000000000001, 0.1, 0.1000000000000000000000000002)")
    assertExact (dec "0.1000000000000000000000000002") (eval "Clamp(0.1000000000000000000000000003, 0.1, 0.1000000000000000000000000002)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-967")>]
let ``JYRO-REQ-967 Clamp_OrdersMixedArgumentsByDoubleValue`` () =
    // h is the double nearest 0.1, which is also the double nearest 0.1000000000000000000000000001.
    // By double value (30.11.2) neither is less than the other, so value is returned; an exact
    // comparison would put 0.1000000000000000000000000001 below h and return min.
    assertExact (dec "0.1000000000000000000000000001")
        (evalIn [ "h", hostInexact 0.1 ] "Clamp(0.1000000000000000000000000001, Input.h, 1)")
    assertExact (dec "0.1000000000000000000000000001")
        (evalIn [ "h", hostInexact 0.1 ] "Clamp(0.1000000000000000000000000001, Input.h, 5)")
    assertExact (dec "0.1000000000000000000000000001")
        (evalIn [ "h", hostInexact 0.1 ] "Clamp(0.1000000000000000000000000001, 0, Input.h)")
    // The same with a value just above the double: 0.10000000000000001 also has that double.
    assertExact (dec "0.10000000000000001")
        (evalIn [ "h", hostInexact 0.1 ] "Clamp(0.10000000000000001, 0, Input.h)")
    // Different doubles: the double value decides.
    assertExact 0.3m (evalIn [ "h", hostInexact 0.30000000000000004 ] "Clamp(0.3, 0, Input.h)")
    assertInexact 0.30000000000000004 (evalIn [ "h", hostInexact 0.30000000000000004 ] "Clamp(0.3, Input.h, 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-968")>]
let ``Clamp_WithMinAboveMax_RaisesInvalidRange`` () =
    expectExprError MessageCode.InvalidRange "Clamp(5, 10, 1)"
    expectExprError MessageCode.InvalidRange "Clamp(5, 1.0000000000000000000000000001, 1)"
    expectExprErrorIn [ "h", hostInexact 2.0 ] MessageCode.InvalidRange "Clamp(5, Input.h, 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-968")>]
let ``Clamp_WithEqualBounds_DoesNotRaise`` () =
    assertExact 3m (eval "Clamp(5, 3, 3)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-969")>]
let ``JYRO-REQ-969 Clamp_WithMinEqualToMax_SelectsAsInEveryOtherCase`` () =
    let h () = [ "h", hostInexact 1.0 ]
    assertExact 1m (evalIn (h ()) "Clamp(1, Input.h, Input.h)")
    assertInexact 1.0 (evalIn (h ()) "Clamp(5, 1, Input.h)")
    assertExact 1m (evalIn (h ()) "Clamp(0, 1, Input.h)")
    assertInexact 1.0 (evalIn (h ()) "Clamp(0, Input.h, 1)")
    assertExact 1m (evalIn (h ()) "Clamp(5, Input.h, 1)")
    assertInexact 1.0 (evalIn (h ()) "Clamp(Input.h, 1, 1)")
    assertExact 1m (eval "Clamp(1, 1, 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-969")>]
let ``JYRO-REQ-969 Clamp_WithMinEqualToMax_ExactProbesIdentifyTheResult`` () =
    let h = [ "h", hostInexact 1.0 ]
    assertTrue (evalIn h "Clamp(1, Input.h, Input.h) * 0.1 + 0.2 == 0.3")
    assertTrue (evalIn h "Clamp(0, 1, Input.h) * 0.1 + 0.2 == 0.3")
    // The book's h made in the script: Power(1, 0.5) is an inexact 1.
    assertExact 1m (evalScriptIn [] "var h = Power(1, 0.5)\nOutput.r = Clamp(1, h, h)")
    assertInexact 1.0 (evalScriptIn [] "var h = Power(1, 0.5)\nOutput.r = Clamp(5, 1, h)")
    assertExact 1m (evalScriptIn [] "var h = Power(1, 0.5)\nOutput.r = Clamp(0, 1, h)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-969")>]
[<InlineData("2, 2, 2", "2")>]
[<InlineData("-1, 0, 0", "0")>]
[<InlineData("1, 0, 0", "0")>]
[<InlineData("0.1000000000000000000000000001, 0.1, 0.1", "0.1")>]
[<InlineData("0.0999999999999999999999999999, 0.1, 0.1", "0.1")>]
let ``JYRO-REQ-969 Clamp_WithMinEqualToMax_NeverRaisesInvalidRange`` (args: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Clamp(%s)" args))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-970")>]
[<InlineData("5, 1, 3", "3")>]
[<InlineData("0, 1, 3", "1")>]
[<InlineData("2.5, 1, 3", "2.5")>]
let ``Clamp_OfExactArguments_IsExactInEachBranch`` (args: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Clamp(%s)" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-971")>]
let ``JYRO-REQ-971 Clamp_OfMixedArguments_ReturnsTheSelectedArgumentsOwnRepresentation`` () =
    assertTrue (eval "Clamp(0.1, 0, Power(2, 0.5)) + 0.2 == 0.3")
    assertExact 0.1m (eval "Clamp(0.1, 0, Power(2, 0.5))")
    assertInexact (sqrt 2.0) (eval "Clamp(5, 0, Power(2, 0.5))")
    assertExact 0.5m (eval "Clamp(0, 0.5, Power(2, 0.5))")
    assertExact 3m (evalIn [ "h", hostInexact 5.0 ] "Clamp(Input.h, 1, 3)")
    assertInexact 1.0 (evalIn [ "h", hostInexact 1.0 ] "Clamp(0, Input.h, 3)")
    assertExact 2m (evalIn [ "lo", hostInexact 1.0; "hi", hostInexact 3.0 ] "Clamp(2, Input.lo, Input.hi)")
    assertInexact 3.0 (evalIn [ "hi", hostInexact 3.0 ] "Clamp(5, 1, Input.hi)")
    assertInexact 2.0 (evalIn [ "h", hostInexact 2.0 ] "Clamp(Input.h, 1, 3)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-972")>]
let ``Clamp_WithNonNumberValue_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Clamp({}, 1, 3)" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-972")>]
let ``Clamp_WithNonNumberMin_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Clamp(2, {}, 3)" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-972")>]
let ``Clamp_WithNonNumberMax_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Clamp(2, 1, {})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-973")>]
let ``Clamp_IsPure`` () = assertPure "Clamp(5, 1, 3)" 3.0

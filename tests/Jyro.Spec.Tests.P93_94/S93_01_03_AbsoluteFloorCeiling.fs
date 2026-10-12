/// jyro-spec 93.1 Absolute, 93.2 Floor, 93.3 Ceiling.
module RealTree.Jyro.Spec.Tests.P93_94.S93_01_03_AbsoluteFloorCeiling

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

// ---------------------------------------------------------------- 93.1 Absolute

[<Fact>]
[<Trait("requirement", "JYRO-REQ-867")>]
let ``Absolute_WithNoArguments_IsRejectedWithTooFewArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Absolute()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-867")>]
let ``Absolute_WithTwoArguments_IsRejectedWithTooManyArguments`` () =
    expectCompileError MessageCode.TooManyArguments "Output.r = Absolute(1, 2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-867")>]
let ``Absolute_ParameterIsNamedValue`` () =
    assertExact 3m (eval "Absolute(value: -3)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Absolute(x: -3)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-867")>]
let ``Absolute_ReturnsANumber_ForExactAndInexactInput`` () =
    assertString "number" (eval "TypeOf(Absolute(-3))")
    Assert.Equal(JyroValueType.Number, (eval "Absolute(-3)").ValueType)
    Assert.Equal(JyroValueType.Number, (evalIn [ "h", hostInexact -2.5 ] "Absolute(Input.h)").ValueType)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-868")>]
[<InlineData("-2.5", "2.5")>]
[<InlineData("2.5", "2.5")>]
[<InlineData("0", "0")>]
[<InlineData("0 - 0", "0")>]
[<InlineData("-1", "1")>]
[<InlineData("1", "1")>]
[<InlineData("-79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-1e300", "1e300")>]
[<InlineData("1e300", "1e300")>]
let ``Absolute_ReturnsTheMagnitude`` (value: string, expected: string) =
    assertTrue (eval (sprintf "Absolute(%s) == %s" value expected))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-868")>]
let ``Absolute_OfMinus2Point5_Returns2Point5`` () =
    assertExact 2.5m (eval "Absolute(-2.5)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-868")>]
let ``Absolute_OfInexactNegative_ReturnsItsNegation`` () =
    assertInexact 0.1 (evalIn [ "h", hostInexact -0.1 ] "Absolute(Input.h)")
    assertInexact 1e300 (evalIn [ "h", hostInexact -1e300 ] "Absolute(Input.h)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-869")>]
[<InlineData("-2.5", "2.5")>]
[<InlineData("0", "0")>]
[<InlineData("0.1", "0.1")>]
[<InlineData("-79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-0.0000000000000000000000000001", "0.0000000000000000000000000001")>]
let ``Absolute_OfExact_IsExact`` (value: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Absolute(%s)" value))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-869")>]
let ``Absolute_OfExact_KeepsExactArithmetic`` () =
    assertTrue (eval "Absolute(-0.1) + 0.2 == 0.3")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-870")>]
[<InlineData(-2.5, 2.5)>]
[<InlineData(0.1, 0.1)>]
[<InlineData(-1e300, 1e300)>]
let ``JYRO-REQ-870 Absolute_OfHostInexact_IsInexact`` (value: float, expected: float) =
    assertInexact expected (evalIn [ "h", hostInexact value ] "Absolute(Input.h)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-870")>]
let ``JYRO-REQ-870 Absolute_OfInexactLiteral_IsInexact`` () =
    // 1e-30 is below the exact representation's smallest step, so the literal is inexact (30.2.6).
    assertInexact 1e-30 (eval "Absolute(1e-30)")
    assertInexact 1e-30 (eval "Absolute(-1e-30)")
    assertInexact 1e300 (eval "Absolute(-1e300)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-870")>]
let ``JYRO-REQ-870 Absolute_OfAnInexactResultInScript_IsInexact`` () =
    assertInexact (sqrt 2.0) (eval "Absolute(-Power(2, 0.5))")
    assertInexact 3.0 (eval "Absolute(-3 * Power(1, 0.5))")
    assertInexact 0.0 (eval "Absolute(0 * Power(1, 0.5))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-871")>]
let ``Absolute_OfNonNumber_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Absolute({})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-872")>]
let ``Absolute_IsPure`` () = assertPure "Absolute(-2)" 2.0

// ---------------------------------------------------------------- 93.2 Floor

[<Fact>]
[<Trait("requirement", "JYRO-REQ-873")>]
let ``Floor_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Floor()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Floor(1, 2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-873")>]
let ``Floor_ParameterIsNamedValue_AndReturnsANumber`` () =
    assertExact 2m (eval "Floor(value: 2.5)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Floor(v: 2.5)"
    assertString "number" (eval "TypeOf(Floor(2.5))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-874")>]
[<InlineData("-2.5", "-3")>]
[<InlineData("2.5", "2")>]
[<InlineData("2", "2")>]
[<InlineData("-2", "-2")>]
[<InlineData("0", "0")>]
[<InlineData("-0.0000000000000000000000000001", "-1")>]
[<InlineData("0.9999999999999999999999999999", "0")>]
[<InlineData("1.0000000000000000000000000001", "1")>]
[<InlineData("-1.9999999999999999999999999999", "-2")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950335", "-79228162514264337593543950335")>]
let ``Floor_ReturnsTheGreatestIntegerNotAbove`` (value: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Floor(%s)" value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-874")>]
[<InlineData(-2.5, -3.0)>]
[<InlineData(1e300, 1e300)>]
[<InlineData(-0.1, -1.0)>]
[<InlineData(2.9999999999999996, 2.0)>]
let ``Floor_OfInexact_ReturnsTheGreatestIntegerNotAbove`` (value: float, expected: float) =
    assertNumber expected (evalIn [ "h", hostInexact value ] "Floor(Input.h)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-875")>]
[<InlineData("2.5", "2")>]
[<InlineData("-2.5", "-3")>]
[<InlineData("7", "7")>]
[<InlineData("-0.0000000000000000000000000001", "-1")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
let ``Floor_OfExact_IsExact`` (value: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Floor(%s)" value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-876")>]
[<InlineData(2.5, 2.0)>]
[<InlineData(-2.5, -3.0)>]
let ``Floor_OfHostInexact_IsInexact`` (value: float, expected: float) =
    assertInexact expected (evalIn [ "h", hostInexact value ] "Floor(Input.h)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-876")>]
let ``Floor_OfInexactLiteral_IsInexact`` () =
    assertInexact 1e300 (eval "Floor(1e300)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-877")>]
let ``Floor_OfNonNumber_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Floor({})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-878")>]
let ``Floor_IsPure`` () = assertPure "Floor(2.7)" 2.0

// ---------------------------------------------------------------- 93.3 Ceiling

[<Fact>]
[<Trait("requirement", "JYRO-REQ-879")>]
let ``Ceiling_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Ceiling()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Ceiling(1, 2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-879")>]
let ``Ceiling_ParameterIsNamedValue_AndReturnsANumber`` () =
    assertExact 3m (eval "Ceiling(value: 2.5)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Ceiling(v: 2.5)"
    assertString "number" (eval "TypeOf(Ceiling(2.5))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-880")>]
[<InlineData("-2.5", "-2")>]
[<InlineData("2.5", "3")>]
[<InlineData("2", "2")>]
[<InlineData("-2", "-2")>]
[<InlineData("0", "0")>]
[<InlineData("0.0000000000000000000000000001", "1")>]
[<InlineData("-0.9999999999999999999999999999", "0")>]
[<InlineData("1.9999999999999999999999999999", "2")>]
[<InlineData("-1.0000000000000000000000000001", "-1")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950335", "-79228162514264337593543950335")>]
let ``Ceiling_ReturnsTheLeastIntegerNotBelow`` (value: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Ceiling(%s)" value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-880")>]
[<InlineData(2.5, 3.0)>]
[<InlineData(0.1, 1.0)>]
[<InlineData(-1e300, -1e300)>]
[<InlineData(2.0000000000000004, 3.0)>]
let ``Ceiling_OfInexact_ReturnsTheLeastIntegerNotBelow`` (value: float, expected: float) =
    assertNumber expected (evalIn [ "h", hostInexact value ] "Ceiling(Input.h)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-881")>]
[<InlineData("2.5", "3")>]
[<InlineData("-2.5", "-2")>]
[<InlineData("7", "7")>]
[<InlineData("0.0000000000000000000000000001", "1")>]
[<InlineData("-79228162514264337593543950335", "-79228162514264337593543950335")>]
let ``Ceiling_OfExact_IsExact`` (value: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Ceiling(%s)" value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-882")>]
[<InlineData(2.5, 3.0)>]
[<InlineData(-2.5, -2.0)>]
let ``Ceiling_OfHostInexact_IsInexact`` (value: float, expected: float) =
    assertInexact expected (evalIn [ "h", hostInexact value ] "Ceiling(Input.h)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-882")>]
let ``Ceiling_OfInexactLiteral_IsInexact`` () =
    assertInexact 1e300 (eval "Ceiling(1e300)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-883")>]
let ``Ceiling_OfNonNumber_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Ceiling({})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-884")>]
let ``Ceiling_IsPure`` () = assertPure "Ceiling(2.1)" 3.0

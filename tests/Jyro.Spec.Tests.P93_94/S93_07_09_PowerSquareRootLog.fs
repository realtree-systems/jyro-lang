/// jyro-spec 93.7 Power, 93.8 SquareRoot, 93.9 Log.
module RealTree.Jyro.Spec.Tests.P93_94.S93_07_09_PowerSquareRootLog

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

/// The number is within `relTol` (relative) of `expected`, whatever its representation.
let private assertNear (expected: float) (relTol: float) (v: JyroValue) =
    let actual = Values.asFloat v
    if abs (actual - expected) > abs expected * relTol then
        Values.fail (sprintf "Expected a number near %.17g (rel %g), got %.17g" expected relTol actual)

// ================================================================= 93.7 Power

[<Fact>]
[<Trait("requirement", "JYRO-REQ-920")>]
let ``JYRO-REQ-920 Power_Arity_IsTwoArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Power(2)"
    expectCompileError MessageCode.TooManyArguments "Output.r = Power(2, 3, 4)"
    expectCompileError MessageCode.TooFewArguments "Output.r = Power()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-920")>]
let ``JYRO-REQ-920 Power_ParametersAreValueAndExponent_BoundByName`` () =
    // The value only: whether it is exact is 93.7.4's.
    assertNumber 1024.0 (eval "Power(value: 2, exponent: 10)")
    assertNumber 1024.0 (eval "Power(exponent: 10, value: 2)")
    assertNumber 0.25 (eval "Power(exponent: -2, value: 2)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Power(base: 2, exponent: 10)"
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Power(value: 2, power: 10)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-920")>]
let ``JYRO-REQ-920 Power_ReturnsANumber`` () =
    assertString "number" (eval "TypeOf(Power(2, 10))")
    assertString "number" (eval "TypeOf(Power(2, 0.5))")
    assertString "number" (eval "TypeOf(Power(1.1, 2))")
    assertString "number" (evalIn [ "h", hostInexact 2.0 ] "TypeOf(Power(Input.h, Input.h))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-921")>]
let ``Power_Of2To10_Returns1024`` () = assertNumber 1024.0 (eval "Power(2, 10)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-921")>]
[<InlineData("2, -1", 0.5)>]
[<InlineData("-2, 3", -8.0)>]
[<InlineData("-2, 2", 4.0)>]
[<InlineData("0, 5", 0.0)>]
[<InlineData("1, 1000000", 1.0)>]
[<InlineData("0.5, 3", 0.125)>]
[<InlineData("4, 0.5", 2.0)>]
[<InlineData("2, 0.5", 1.4142135623730951)>]
let ``Power_ReturnsValueRaisedToExponent`` (args: string, expected: float) =
    assertNumber expected (eval (sprintf "Power(%s)" args))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-922")>]
[<InlineData("0")>]
[<InlineData("5")>]
[<InlineData("-5")>]
[<InlineData("0.1")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950335")>]
[<InlineData("1e300")>]
let ``Power_ToTheZero_Returns1`` (value: string) =
    assertNumber 1.0 (eval (sprintf "Power(%s, 0)" value))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-922")>]
let ``Power_WithInexactOperandsAndZeroExponent_Returns1`` () =
    assertNumber 1.0 (evalIn [ "h", hostInexact 3.3 ] "Power(Input.h, 0)")
    assertNumber 1.0 (evalIn [ "z", hostInexact 0.0 ] "Power(0, Input.z)")
    assertNumber 1.0 (evalIn [ "z", hostInexact 0.0 ] "Power(7, Input.z)")
    assertNumber 1.0 (evalIn [ "z", hostInexact 0.0; "h", hostInexact 0.0 ] "Power(Input.h, Input.z)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-923")>]
let ``JYRO-REQ-923 Power_Of1Point1Squared_ReturnsExact1Point21`` () = assertExact 1.21m (eval "Power(1.1, 2)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-923")>]
[<InlineData("10, 28", "10000000000000000000000000000")>]
[<InlineData("2, -20", "0.00000095367431640625")>]
[<InlineData("10, -28", "0.0000000000000000000000000001")>]
[<InlineData("0.1, 28", "0.0000000000000000000000000001")>]
[<InlineData("2, 95", "39614081257132168796771975168")>]
[<InlineData("-2, 3", "-8")>]
[<InlineData("-1.5, 2", "2.25")>]
[<InlineData("0.5, 3", "0.125")>]
[<InlineData("3, 2.0", "9")>]
[<InlineData("1.1, 0", "1")>]
[<InlineData("0, 3", "0")>]
[<InlineData("5, -1", "0.2")>]
[<InlineData("-0.2, -3", "-125")>]
[<InlineData("2, 10", "1024")>]
let ``JYRO-REQ-923 Power_OfExactToAnExactInteger_WhenHeld_IsExact`` (args: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Power(%s)" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-923")>]
let ``JYRO-REQ-923 Power_OfExact_KeepsExactArithmetic`` () =
    assertTrue (eval "Power(0.1, 2) + 0.2 == 0.21")
    assertTrue (eval "Power(1.1, 3) == 1.331")
    assertTrue (eval "Power(5, -1) + 0.1 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-923")>]
let ``JYRO-REQ-923 Power_OfHostExactToAnExactInteger_IsExact`` () =
    assertExact 1.21m (evalIn [ "v", hostExact 1.1m ] "Power(Input.v, 2)")
    assertExact 8m (evalIn [ "v", hostExact 2m; "e", hostExact 3m ] "Power(Input.v, Input.e)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-924")>]
[<InlineData("3, -1", 0.33333333333333331)>]
[<InlineData("2, 0.5", 1.4142135623730951)>]
[<InlineData("4, 0.5", 2.0)>]
let ``Power_OutsideTheExactCase_IsInexactDouble`` (args: string, expected: float) =
    assertInexact expected (eval (sprintf "Power(%s)" args))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-924")>]
[<InlineData("10, 29", 10.0, 29.0)>]
[<InlineData("10, -29", 10.0, -29.0)>]
[<InlineData("1.1, 30", 1.1, 30.0)>]
let ``Power_WhenTheResultIsNotHeld_IsInexactDouble`` (args: string, x: float, y: float) =
    assertInexactNear (Math.Pow(x, y)) 1e-15 (eval (sprintf "Power(%s)" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-924")>]
let ``Power_WithAnInexactOperand_IsInexact`` () =
    assertInexact 4.0 (evalIn [ "h", hostInexact 2.0 ] "Power(Input.h, 2)")
    assertInexact 4.0 (evalIn [ "h", hostInexact 2.0 ] "Power(2, Input.h)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1752")>]
let ``JYRO-REQ-1752 Power_OfMinus8ToOneHalf_RaisesNumberDomainError`` () =
    expectExprError MessageCode.NumberDomainError "Power(-8, 0.5)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1752")>]
[<InlineData("-1, 0.5")>]
[<InlineData("-8, 1 / 3")>]
[<InlineData("-0.0000000000000000000000000001, 0.5")>]
[<InlineData("-2, 2.5")>]
[<InlineData("-2, -0.5")>]
[<InlineData("-1e200, 0.5")>]
[<InlineData("-0.5, 1.5")>]
[<InlineData("-3, 1e-10")>]
let ``JYRO-REQ-1752 Power_OfNegativeToNonInteger_RaisesNumberDomainError`` (args: string) =
    expectExprError MessageCode.NumberDomainError (sprintf "Power(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1752")>]
let ``JYRO-REQ-1752 Power_OfHostInexactNegativeToNonInteger_RaisesNumberDomainError`` () =
    expectExprErrorIn [ "v", hostInexact -2.0; "e", hostInexact 0.5 ] MessageCode.NumberDomainError "Power(Input.v, Input.e)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1752")>]
let ``JYRO-REQ-1752 Power_OfNegativeToInteger_IsNotADomainError`` () =
    assertNumber 64.0 (eval "Power(-8, 2)")
    assertNumber -512.0 (eval "Power(-8, 3)")
    assertNumber 64.0 (eval "Power(-8, 2.0)")
    assertNumber -512.0 (eval "Power(-8, 3.0)")
    assertNumber -0.125 (eval "Power(-2, -3)")
    assertNumber 1.0 (eval "Power(-8, 0)")
    assertNumber 64.0 (evalIn [ "v", hostInexact -8.0; "e", hostInexact 2.0 ] "Power(Input.v, Input.e)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1752")>]
[<InlineData("8, 0.5")>]
[<InlineData("2, 0.5")>]
[<InlineData("0.5, 1.5")>]
[<InlineData("0, 0.5")>]
[<InlineData("1e200, 0.5")>]
let ``JYRO-REQ-1752 Power_OfNonNegativeToNonInteger_IsNotADomainError`` (args: string) =
    let r = Script.run (sprintf "Output.r = Power(%s)" args)
    assertNoDiagnostics r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1752")>]
let ``JYRO-REQ-1752 Power_OfZeroToMinus1_RaisesNumberOverflow_NotNumberDomainError`` () =
    expectExprError MessageCode.NumberOverflow "Power(0, -1)"
    expectExprError MessageCode.NumberOverflow "Power(0, -0.5)"
    expectExprError MessageCode.NumberOverflow "Power(0, -2)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1752")>]
[<InlineData("-8, 2.00000000000000001")>]
[<InlineData("-2, 3.0000000000000000001")>]
[<InlineData("-8, 1.99999999999999999")>]
[<InlineData("-2, 9007199254740992.5")>]
[<InlineData("-0.5, 9007199254740992.5")>]
let ``JYRO-REQ-1752 Power_OfNegativeToExactExponentBeyondDoubleResolution_RaisesNumberDomainError`` (args: string) =
    expectExprError MessageCode.NumberDomainError (sprintf "Power(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1752")>]
let ``JYRO-REQ-1752 Power_OfHostInexactNegativeToExactNonIntegerExponentBeyondDoubleResolution_RaisesNumberDomainError`` () =
    expectExprErrorIn [ "v", hostInexact -8.0 ] MessageCode.NumberDomainError "Power(Input.v, 2.00000000000000001)"
    expectExprErrorIn [ "v", hostInexact -8.0 ] MessageCode.NumberDomainError "Power(Input.v, 9007199254740992.5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1752")>]
let ``JYRO-REQ-1752 Power_OfNegativeToExactIntegerExponentNearTheBoundary_IsNotADomainError`` () =
    assertNumber 64.0 (eval "Power(-8, 2.0)")
    assertNumber 64.0 (eval "Power(-8, 2.00000000000000000)")
    expectExprError MessageCode.NumberOverflow "Power(-2, 9007199254740992)"
    expectExprError MessageCode.NumberOverflow "Power(-2, 9007199254740993)"
    let r = Script.run "Output.r = Power(-0.5, 9007199254740992)"
    assertNoDiagnostics r
    assertNumber 0.0 (Script.output r |> Values.prop "r")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-926")>]
[<InlineData("1e200, 2")>]
[<InlineData("0, -1")>]
[<InlineData("0, -0.5")>]
[<InlineData("10, 400")>]
[<InlineData("-1e200, 3")>]
let ``Power_WhenTheResultIsAnInfinity_RaisesNumberOverflow`` (args: string) =
    expectExprError MessageCode.NumberOverflow (sprintf "Power(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-926")>]
let ``Power_WhenTheResultIsFinite_DoesNotOverflow`` () =
    assertNumber (Math.Pow(1e154, 2.0)) (eval "Power(1e154, 2)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-927")>]
[<InlineData("10, -400")>]
[<InlineData("-10, -401")>]
[<InlineData("2, -1075")>]
let ``Power_WhenTheResultIsBelowTheSmallestDouble_Returns0`` (args: string) =
    let r = Script.run (sprintf "Output.r = Power(%s)" args)
    assertNoDiagnostics r
    assertNumber 0.0 (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-927")>]
let ``Power_OfHostInexactUnderflow_Returns0`` () =
    let r = runIn [ "h", hostInexact 1e-200 ] "Output.r = Power(Input.h, 2)"
    assertNoDiagnostics r
    assertNumber 0.0 (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-928")>]
let ``Power_WithNonNumberValue_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Power({}, 2)" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-928")>]
let ``Power_WithNonNumberExponent_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Power(2, {})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-929")>]
let ``Power_IsPure`` () = assertPure "Power(2, 3)" 8.0

// ================================================================= 93.8 SquareRoot

[<Fact>]
[<Trait("requirement", "JYRO-REQ-930")>]
let ``SquareRoot_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = SquareRoot()"
    expectCompileError MessageCode.TooManyArguments "Output.r = SquareRoot(4, 2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-930")>]
let ``SquareRoot_ParameterIsNamedValue_AndReturnsANumber`` () =
    assertNumber 2.0 (eval "SquareRoot(value: 4)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = SquareRoot(x: 4)"
    assertString "number" (eval "TypeOf(SquareRoot(4))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-931")>]
let ``SquareRoot_Of2Point25_Returns1Point5`` () = assertNumber 1.5 (eval "SquareRoot(2.25)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-931")>]
[<InlineData("0", 0.0)>]
[<InlineData("1", 1.0)>]
[<InlineData("4", 2.0)>]
[<InlineData("0.0001", 0.01)>]
[<InlineData("0.0000000000000000000000000001", 1e-14)>]
[<InlineData("10000000000000000000000000000", 1e14)>]
[<InlineData("2", 1.4142135623730951)>]
let ``SquareRoot_ReturnsTheNonNegativeRoot`` (value: string, expected: float) =
    assertNumber expected (eval (sprintf "SquareRoot(%s)" value))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-931")>]
let ``SquareRoot_OfHostInexact_ReturnsTheNonNegativeRoot`` () =
    assertNumber 3.0 (evalIn [ "h", hostInexact 9.0 ] "SquareRoot(Input.h)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-932")>]
[<InlineData("2.25", "1.5")>]
[<InlineData("0", "0")>]
[<InlineData("4", "2")>]
[<InlineData("0.0001", "0.01")>]
[<InlineData("0.0000000000000000000000000001", "0.00000000000001")>]
[<InlineData("10000000000000000000000000000", "100000000000000")>]
[<InlineData("0.25", "0.5")>]
[<InlineData("1.44", "1.2")>]
[<InlineData("0.01", "0.1")>]
[<InlineData("0.0000000000000000000000000004", "0.00000000000002")>]
[<InlineData("12345678987654321", "111111111")>]
[<InlineData("1", "1")>]
let ``JYRO-REQ-932 SquareRoot_OfExactWithAnExactHeldRoot_IsExact`` (value: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "SquareRoot(%s)" value))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-932")>]
let ``JYRO-REQ-932 SquareRoot_OfExact_KeepsExactArithmetic`` () =
    // 0.1 has no double that is exactly it: an exact root keeps 0.1 + 0.2 equal to 0.3.
    assertTrue (eval "SquareRoot(0.01) + 0.2 == 0.3")
    assertTrue (eval "SquareRoot(1.44) * 3 == 3.6")
    assertExact 1.5m (evalIn [ "v", hostExact 2.25m ] "SquareRoot(Input.v)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-933")>]
[<InlineData("2", 2.0)>]
[<InlineData("0.1", 0.1)>]
[<InlineData("3", 3.0)>]
let ``SquareRoot_OfExactWithoutAnExactRoot_IsInexactDouble`` (value: string, host: float) =
    assertInexact (Math.Sqrt host) (eval (sprintf "SquareRoot(%s)" value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-933")>]
[<InlineData(4.0)>]
[<InlineData(2.25)>]
[<InlineData(2.0)>]
let ``SquareRoot_OfInexact_IsInexactDouble`` (value: float) =
    assertInexact (Math.Sqrt value) (evalIn [ "h", hostInexact value ] "SquareRoot(Input.h)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-934")>]
[<InlineData("-1")>]
[<InlineData("-0.0000000000000000000000000001")>]
[<InlineData("-79228162514264337593543950335")>]
[<InlineData("-1e300")>]
let ``SquareRoot_OfNegative_RaisesNumberDomainError`` (value: string) =
    expectExprError MessageCode.NumberDomainError (sprintf "SquareRoot(%s)" value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-934")>]
let ``SquareRoot_OfHostInexactNegative_RaisesNumberDomainError_AndZeroDoesNot`` () =
    expectExprErrorIn [ "h", hostInexact -4.0 ] MessageCode.NumberDomainError "SquareRoot(Input.h)"
    assertNumber 0.0 (eval "SquareRoot(0)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-935")>]
let ``SquareRoot_OfNonNumber_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "SquareRoot({})" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-936")>]
let ``SquareRoot_IsPure`` () = assertPure "SquareRoot(9)" 3.0

// ================================================================= 93.9 Log

[<Fact>]
[<Trait("requirement", "JYRO-REQ-937")>]
let ``Log_Arity_IsOneOrTwoArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Log()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Log(1, 2, 3)"
    expectCompiles "Output.r = Log(8)"
    expectCompiles "Output.r = Log(8, 2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-937")>]
let ``Log_ParametersAreValueAndOptionalBase`` () =
    assertNumber 3.0 (eval "Log(value: 8, base: 2)")
    assertNumber 3.0 (eval "Log(base: 2, value: 8)")
    assertNumber (Math.Log 8.0) (eval "Log(value: 8)")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Log(x: 8)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-937")>]
let ``Log_ReturnsANumber`` () =
    assertString "number" (eval "TypeOf(Log(8, 2))")
    assertString "number" (eval "TypeOf(Log(8))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-938")>]
let ``Log_Of1_Returns0`` () = assertNumber 0.0 (eval "Log(1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-938")>]
let ``Log_WithNoBase_IsTheNaturalLogarithm`` () =
    assertNumber (Math.Log Math.E) (evalIn [ "e", hostInexact Math.E ] "Log(Input.e)")
    assertNumber (Math.Log 10.0) (eval "Log(10)")
    assertNumber (Math.Log 0.5) (eval "Log(0.5)")
    assertNumber (Math.Log 2.0) (eval "Log(2)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-939")>]
let ``JYRO-REQ-939 Log_Of8Base2_Returns3`` () = assertNumber 3.0 (eval "Log(8, 2)")

/// The logarithm's value, whatever its representation: each of these is an integer
/// or a half, which a double holds exactly.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-939")>]
[<InlineData("27, 3", 3.0)>]
[<InlineData("0.001, 10", -3.0)>]
[<InlineData("100, 0.1", -2.0)>]
[<InlineData("2, 4", 0.5)>]
[<InlineData("1024, 2", 10.0)>]
[<InlineData("0.5, 2", -1.0)>]
[<InlineData("9, 3", 2.0)>]
[<InlineData("1, 7", 0.0)>]
let ``JYRO-REQ-939 Log_WithABase_IsTheLogarithmToThatBase`` (args: string, expected: float) =
    assertNumber expected (eval (sprintf "Log(%s)" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-939")>]
let ``JYRO-REQ-939 Log_OfANonIntegerPower_IsTheLogarithmToThatBase`` () =
    assertNear (Math.Log(10.0, 3.0)) 1e-15 (eval "Log(10, 3)")
    assertNear (Math.Log 7.0 / Math.Log 2.0) 1e-15 (eval "Log(7, 2)")
    assertNear (Math.Log 5.0 / Math.Log 0.5) 1e-15 (eval "Log(5, 0.5)")
    assertNear (Math.Log 1000.0 / Math.Log 10.0) 1e-15 (evalIn [ "h", hostInexact 1000.0 ] "Log(Input.h, 10)")
    assertNear 2.0 1e-15 (eval "Log(value: 100, base: 10)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-940")>]
[<InlineData("0")>]
[<InlineData("-1")>]
[<InlineData("-0.0000000000000000000000000001")>]
[<InlineData("-1e300")>]
[<InlineData("0, 10")>]
[<InlineData("-8, 2")>]
[<InlineData("0, 0.5")>]
let ``Log_OfZeroOrNegative_RaisesNumberDomainError`` (args: string) =
    expectExprError MessageCode.NumberDomainError (sprintf "Log(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-940")>]
let ``Log_OfHostInexactNegative_RaisesNumberDomainError`` () =
    expectExprErrorIn [ "h", hostInexact -2.0 ] MessageCode.NumberDomainError "Log(Input.h)"
    expectExprErrorIn [ "h", hostInexact 0.0 ] MessageCode.NumberDomainError "Log(Input.h)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-940")>]
let ``Log_OfTheSmallestPositiveExact_IsNotADomainError`` () =
    assertInexactNear (Math.Log 1e-28) 1e-15 (eval "Log(0.0000000000000000000000000001)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-941")>]
[<InlineData("8, 0")>]
[<InlineData("8, -2")>]
[<InlineData("8, 1")>]
[<InlineData("8, 1.0")>]
[<InlineData("8, -0.0000000000000000000000000001")>]
let ``JYRO-REQ-941 Log_WithBaseZeroNegativeOrOne_RaisesNumberDomainError`` (args: string) =
    expectExprError MessageCode.NumberDomainError (sprintf "Log(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-941")>]
let ``JYRO-REQ-941 Log_WithHostBaseZeroNegativeOrOne_RaisesNumberDomainError`` () =
    for b in [ hostInexact 1.0; hostInexact 0.0; hostInexact -0.0; hostInexact -2.0; hostExact 1m; hostExact 0m ] do
        expectExprErrorIn [ "b", b ] MessageCode.NumberDomainError "Log(8, Input.b)"
    expectExprError MessageCode.NumberDomainError "Log(8, Power(1, 0.5))"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-941")>]
let ``JYRO-REQ-941 Log_WithBaseJustAboveOrBelowOne_IsNotADomainError`` () =
    // Only the absence of NumberDomainError is pinned here: these bases are not 1,
    // and 93.9.5 does not decide the value of the logarithm.
    for b in [ "1.0000000000000000000000000001"; "0.9999999999999999999999999999" ] do
        let r = Script.run (sprintf "Output.r = Log(8, %s)" b)
        Assert.DoesNotContain(MessageCode.NumberDomainError, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-941")>]
let ``JYRO-REQ-941 Log_WithBaseBetweenZeroAndOneOrAboveOne_IsNotADomainError`` () =
    for args in [ "8, 0.5"; "8, 0.0000000000000000000000000001"; "8, 2"; "8, 79228162514264337593543950335"; "8, 1e300" ] do
        let r = Script.run (sprintf "Output.r = Log(%s)" args)
        assertNoDiagnostics r

[<Theory>]
[<Trait("requirement", "JYRO-REQ-942")>]
[<InlineData("0, 1")>]
[<InlineData("-1, 0")>]
[<InlineData("0, -2")>]
let ``JYRO-REQ-942 Log_ExaminesValueBeforeBase`` (args: string) =
    let m = runtimeErrorIn [] (sprintf "Output.r = Log(%s)" args)
    Assert.Equal(MessageCode.NumberDomainError, m.Code)
    // 130.6.4: the parameter name, as the signature spells it, is one whole argument.
    Assert.Contains("value", argStrings m)
    Assert.DoesNotContain("base", argStrings m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-942")>]
let ``JYRO-REQ-942 Log_TypeOfValueIsCheckedBeforeBase`` () =
    expectExprError MessageCode.ArgumentTypeMismatch "Log(\"x\", 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-942")>]
let ``JYRO-REQ-942 Log_ValueDomainIsCheckedBeforeTheTypeOfBase`` () =
    let m = runtimeErrorIn [] "Output.r = Log(0, \"x\")"
    Assert.Equal(MessageCode.NumberDomainError, m.Code)
    Assert.Contains("value", argStrings m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-943")>]
let ``JYRO-REQ-943 Log_Of1000Base10_ReturnsExact3`` () = assertExact 3m (eval "Log(1000, 10)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-943")>]
[<InlineData("8, 2", "3")>]
[<InlineData("1024, 2", "10")>]
[<InlineData("0.001, 10", "-3")>]
[<InlineData("1, 10", "0")>]
[<InlineData("0.25, 2", "-2")>]
[<InlineData("4, 0.5", "-2")>]
[<InlineData("27, 3", "3")>]
[<InlineData("100, 0.1", "-2")>]
[<InlineData("10000000000000000000000000000, 10", "28")>]
[<InlineData("0.0000000000000000000000000001, 10", "-28")>]
[<InlineData("0.0625, 0.5", "4")>]
[<InlineData("1, 0.5", "0")>]
[<InlineData("2, 2", "1")>]
[<InlineData("39614081257132168796771975168, 2", "95")>]
[<InlineData("0.2, 5", "-1")>]
[<InlineData("125, 0.2", "-3")>]
let ``JYRO-REQ-943 Log_OfAnIntegerPowerOfAnExactBase_IsThatExactInteger`` (args: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Log(%s)" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-943")>]
let ``JYRO-REQ-943 Log_OfAnIntegerPowerOfAnExactBase_KeepsExactArithmetic`` () =
    assertTrue (eval "Log(1000, 10) * 0.1 + 0.2 == 0.5")
    assertTrue (eval "Log(100, 0.1) == -2")
    assertExact 3m (evalIn [ "v", hostExact 1000m; "b", hostExact 10m ] "Log(Input.v, Input.b)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-944")>]
let ``Log_WithNoBase_IsInexact`` () =
    assertInexact 0.0 (eval "Log(1)")
    assertInexact (Math.Log 10.0) (eval "Log(10)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-944")>]
let ``Log_OfANonIntegerPower_IsInexact`` () =
    assertInexact 0.5 (eval "Log(2, 4)")
    assertInexactNear (Math.Log(10.0, 3.0)) 1e-15 (eval "Log(10, 3)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-944")>]
let ``Log_WithAnInexactOperand_IsInexact`` () =
    assertInexactNear 3.0 1e-15 (evalIn [ "h", hostInexact 1000.0 ] "Log(Input.h, 10)")
    assertInexactNear 3.0 1e-15 (evalIn [ "h", hostInexact 10.0 ] "Log(1000, Input.h)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-945")>]
let ``JYRO-REQ-945 Log_WithNonNumberValue_RaisesArgumentTypeMismatch`` () =
    expectTypeMismatchForAll "Log({})" badLiterals
    expectTypeMismatchForAll "Log({}, 2)" badLiterals
    expectTypeMismatchForAll "Log({}, null)" badLiterals

[<Fact>]
[<Trait("requirement", "JYRO-REQ-945")>]
let ``JYRO-REQ-945 Log_WithNonNumberNonNullBase_RaisesArgumentTypeMismatch`` () =
    let nonNull = badLiterals |> List.filter (fun l -> l <> "null")
    for lit in nonNull do
        expectRuntimeError MessageCode.ArgumentTypeMismatch (withFn (sprintf "Log(8, %s)" lit))
    for (name, v) in badInputs do
        if name <> "null" then
            expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "Log(8, Input.v)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-945")>]
let ``JYRO-REQ-945 Log_WithNullBase_IsTheNaturalLogarithm`` () =
    assertTrue (eval "Log(8, null) == Log(8)")
    assertNumber (Math.Log 8.0) (eval "Log(8, null)")
    assertNumber (Math.Log 8.0) (eval "Log(value: 8, base: null)")
    assertNumber (Math.Log 8.0) (evalIn [ "b", Values.nul ] "Log(8, Input.b)")
    assertNumber (Math.Log 8.0) (eval "Log(8, Input.missing)")
    assertNumber 0.0 (eval "Log(1, null)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-946")>]
let ``Log_IsPure`` () = assertPure "Log(100, 10)" 2.0

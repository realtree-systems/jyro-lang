/// 20.6 - numeric literals.
module RealTree.Jyro.Spec.Tests.S20_06_Numbers

open System
open System.Globalization
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private numberOf (literal: string) : JyroValue = outV ("Output.v = " + literal)

let private invalidNumber (text: string) =
    rejectedWithCode MessageCode.InvalidNumberFormat ("Output.v = " + text)

// -- 20.6.1 ------------------------------------------------------------------

[<Theory>]
[<InlineData("0", 0.0)>]
[<InlineData("7", 7.0)>]
[<InlineData("10", 10.0)>]
[<InlineData("1.5", 1.5)>]
[<InlineData("0.5", 0.5)>]
[<InlineData("0.0", 0.0)>]
[<InlineData("1e5", 100000.0)>]
[<InlineData("1E5", 100000.0)>]
[<InlineData("1e+5", 100000.0)>]
[<InlineData("1e-5", 0.00001)>]
[<InlineData("1.5e10", 15000000000.0)>]
[<InlineData("0e0", 0.0)>]
[<InlineData("0.001", 0.001)>]
[<InlineData("10.25E-2", 0.1025)>]
[<Trait("requirement", "JYRO-REQ-29")>]
let ``JYRO-REQ-29 DecimalLiteral_JsonNumberFormWithoutSign_IsAcceptedWithItsValue`` (literal: string, expected: float) =
    Assert.Equal(expected, Values.asFloat (numberOf literal))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-29")>]
let ``JYRO-REQ-29 DecimalLiteral_ThirtyDigitInteger_IsAccepted`` () =
    let v = Values.asFloat (numberOf "123456789012345678901234567890")
    Assert.Equal(1.2345678901234568e29, v, 1e14)

[<Theory>]
[<InlineData("007")>]
[<InlineData("01")>]
[<InlineData("00")>]
[<InlineData("01.5")>]
[<InlineData("00.5")>]
[<InlineData("1.")>]
[<InlineData("0.")>]
[<InlineData("1.e5")>]
[<InlineData("1e")>]
[<InlineData("1e+")>]
[<InlineData("1E-")>]
[<InlineData("1.5e")>]
[<Trait("requirement", "JYRO-REQ-29")>]
let ``JYRO-REQ-29 DecimalLiteral_LeadingZeroEmptyFractionOrEmptyExponent_IsInvalidNumberFormat`` (text: string) =
    invalidNumber text

// -- 20.6.2 ------------------------------------------------------------------

[<Theory>]
[<InlineData("-5", -5.0)>]
[<InlineData("- 5", -5.0)>]
[<InlineData("- -5", 5.0)>]
[<InlineData("3 -5", -2.0)>]
[<InlineData("-0x10", -16.0)>]
[<InlineData("-0b11", -3.0)>]
[<InlineData("-1.5e2", -150.0)>]
[<InlineData("1e-5", 0.00001)>]
[<InlineData("1E+2", 100.0)>]
[<Trait("requirement", "JYRO-REQ-30")>]
let ``NegativeNumber_IsThePrefixMinusOperatorOnAnUnsignedLiteral`` (expr: string, expected: float) =
    Assert.Equal(expected, Values.asFloat (numberOf expr))

[<Theory>]
[<InlineData("-0x8000000000000000")>]
[<InlineData("-0b1000000000000000000000000000000000000000000000000000000000000000")>]
[<InlineData("-1e400")>]
[<Trait("requirement", "JYRO-REQ-30")>]
let ``NegativeNumber_LiteralOutOfRangeBeforeNegation_IsInvalidNumberFormat`` (expr: string) =
    invalidNumber expr

[<Theory>]
[<InlineData("+5")>]
[<InlineData("+0x10")>]
[<InlineData("+1.5")>]
[<Trait("requirement", "JYRO-REQ-30")>]
let ``NegativeNumber_PlusSignBeforeALiteral_IsRejected`` (expr: string) =
    rejected ("Output.v = " + expr) |> ignore

// -- 20.6.3 ------------------------------------------------------------------

[<Theory>]
[<InlineData("0xFF", "255")>]
[<InlineData("0XFF", "255")>]
[<InlineData("0xff", "255")>]
[<InlineData("0xAbC", "2748")>]
[<InlineData("0x0", "0")>]
[<InlineData("0x00000010", "16")>]
[<InlineData("0b101", "5")>]
[<InlineData("0B101", "5")>]
[<InlineData("0b0", "0")>]
[<InlineData("0b0000", "0")>]
[<InlineData("0b1", "1")>]
[<InlineData("0x7FFFFFFFFFFFFFFF", "9223372036854775807")>]
[<InlineData("0b11111111", "255")>]
[<InlineData("0B0", "0")>]
[<InlineData("0xaBcDeF", "11259375")>]
[<InlineData("0x20000000000001", "9007199254740993")>]
[<InlineData("0x7FFFFFFFFFFFFFFE", "9223372036854775806")>]
[<Trait("requirement", "JYRO-REQ-1819")>]
let ``JYRO-REQ-1819 HexAndBinaryLiteral_PrefixesGiveBase16AndBase2Values`` (literal: string, expected: string) =
    Assert.Equal(Decimal.Parse(expected, CultureInfo.InvariantCulture), Values.asDecimal (numberOf literal))

[<Theory>]
[<InlineData("0xFF == 255")>]
[<InlineData("0XFF == 255")>]
[<InlineData("0b101 == 5")>]
[<InlineData("0B101 == 5")>]
[<InlineData("0Xff == 0b11111111")>]
[<InlineData("0x7FFFFFFFFFFFFFFF == 9223372036854775807")>]
[<InlineData("0x7FFFFFFFFFFFFFFF - 1 == 9223372036854775806")>]
[<InlineData("0x20000000000001 - 0x20000000000000 == 1")>]
[<InlineData("0xFF + 0.1 == 255.1")>]
[<Trait("requirement", "JYRO-REQ-1819")>]
let ``JYRO-REQ-1819 HexAndBinaryLiteral_EqualTheirDecimalValue`` (expr: string) =
    outV ("Output.v = " + expr) |> assertJson "true"

[<Theory>]
[<InlineData("0xFF")>]
[<InlineData("0Xff")>]
[<InlineData("0b11111111")>]
[<InlineData("0x0")>]
[<InlineData("0B0")>]
[<InlineData("0x7FFFFFFFFFFFFFFF")>]
[<InlineData("0x20000000000001")>]
[<InlineData("0b11111111111111111111111111111111111111111111111111111111111111")>]
[<Trait("requirement", "JYRO-REQ-1819")>]
let ``JYRO-REQ-1819 HexAndBinaryLiteral_GiveAnExactNumber`` (literal: string) =
    Assert.True(Values.isExact (numberOf literal), literal)

[<Theory>]
[<InlineData("0xFF", "255")>]
[<InlineData("0x7FFFFFFFFFFFFFFF", "9223372036854775807")>]
[<InlineData("0x20000000000001", "9007199254740993")>]
[<InlineData("0b11111111", "255")>]
[<Trait("requirement", "JYRO-REQ-1819")>]
let ``JYRO-REQ-1819 HexAndBinaryLiteral_StringFormIsTheExactDigits`` (literal: string, expected: string) =
    Assert.Equal(expected, Values.asString (outV ("Output.v = " + literal + " & \"\"")))

// -- 20.6.4 ------------------------------------------------------------------

let private ones63 = String('1', 63)

let atTheBound : seq<obj[]> =
    rowsOf [ "0x7FFFFFFFFFFFFFFF"; "0X7fffffffffffffff"; "0x00007FFFFFFFFFFFFFFF"; "0b" + ones63; "0B0" + ones63 ]

[<Theory>]
[<MemberData(nameof atTheBound)>]
[<Trait("requirement", "JYRO-REQ-32")>]
let ``JYRO-REQ-32 HexBinaryLiteral_AtTheBound_IsAcceptedWithItsExactValue`` (literal: string) =
    Assert.Equal(9223372036854775807m, Values.asDecimal (numberOf literal))
    Script.outputOf (sprintf "Output.a = %s == 9223372036854775807\nOutput.b = %s == 9223372036854775806" literal literal)
    |> assertJson "{\"a\":true,\"b\":false}"

let aboveTheBound : seq<obj[]> =
    rowsOf
        [ "0x8000000000000000"
          "0X8000000000000000"
          "0b1" + String('0', 63)
          "0xFFFFFFFFFFFFFFFF"
          "0x10000000000000000"
          "0x" + String('F', 100)
          "0b1" + String('0', 200) ]

[<Theory>]
[<MemberData(nameof aboveTheBound)>]
[<Trait("requirement", "JYRO-REQ-32")>]
let ``JYRO-REQ-32 HexBinaryLiteral_AboveTheBound_IsInvalidNumberFormatAndNothingRuns`` (literal: string) =
    invalidNumber literal

[<Theory>]
[<MemberData(nameof aboveTheBound)>]
[<Trait("requirement", "JYRO-REQ-32")>]
let ``JYRO-REQ-32 HexBinaryLiteral_AboveTheBound_IsRejectedAtParseTime`` (literal: string) =
    rejected ("Output.v = " + literal) |> assertParseStage

// -- 20.6.5 ------------------------------------------------------------------

let finiteDecimals : seq<obj[]> =
    rowsOf
        [ "1e308"
          "1.7976931348623157e308"
          "1.7976931348623158e308"
          "1e-400"
          "4.9e-325"
          "123456789012345678901234567890" ]

[<Theory>]
[<MemberData(nameof finiteDecimals)>]
[<Trait("requirement", "JYRO-REQ-33")>]
let ``JYRO-REQ-33 DecimalLiteral_FiniteAsADouble_IsAccepted`` (literal: string) =
    let v = Values.asFloat (numberOf literal)
    Assert.True(Double.IsFinite v, sprintf "%s gave %f" literal v)
    Assert.Equal(Double.Parse(literal, CultureInfo.InvariantCulture), v)

let infiniteDecimals : seq<obj[]> =
    rowsOf
        [ "1.7976931348623159e308"
          "1.8e308"
          "2e308"
          "1e309"
          "1e400"
          "1E400"
          "10e308"
          "1" + String('0', 400)
          "1" + String('0', 399) + ".5" ]

[<Theory>]
[<MemberData(nameof infiniteDecimals)>]
[<Trait("requirement", "JYRO-REQ-33")>]
let ``JYRO-REQ-33 DecimalLiteral_NotFiniteAsADouble_IsInvalidNumberFormatAndNothingRuns`` (literal: string) =
    invalidNumber literal

[<Theory>]
[<MemberData(nameof infiniteDecimals)>]
[<Trait("requirement", "JYRO-REQ-33")>]
let ``JYRO-REQ-33 DecimalLiteral_NotFiniteAsADouble_IsRejectedAtParseTime`` (literal: string) =
    rejected ("Output.v = " + literal) |> assertParseStage

// -- 20.6.6 ------------------------------------------------------------------

let invalidTokens : seq<obj[]> =
    rowsOf
        [ "007"; "01"; "00"; "01.5"
          "1."; "0."; "1.e5"
          "1e"; "1e+"; "1E-"
          "1.5.3"; "1_000"; "12abc"; "1.foo"
          "0x"; "0xG"; "0b"; "0b2"
          "1..2"; "1ee5"; "1e5e5"; "12e3.5"; "0x1.5"; "0b1e+1"; "0X"; "0B"; "0x1G"
          "1" + cp 0x0661
          "3x"; "1e5x"; "0xE_1" ]

[<Theory>]
[<MemberData(nameof invalidTokens)>]
[<Trait("requirement", "JYRO-REQ-34")>]
let ``JYRO-REQ-34 NumberToken_LongestRunNotMatchingNumberLiteral_IsOneInvalidNumberFormat`` (text: string) =
    invalidNumber text

/// The InvalidNumberFormat of such a token is a rejection placed at parse time, so 160.1.12
/// gives that diagnostic the stage Parse. Moved here from JYRO-REQ-1670's stage table
/// (JYRO-ADR-113 item 2, JYRO-DEF-505): this requirement raises the code for 12abc, so it
/// depends on 1670.
[<Theory>]
[<InlineData("12abc")>]
[<InlineData("3x")>]
[<InlineData("1.foo")>]
[<Trait("requirement", "JYRO-REQ-34")>]
let ``JYRO-REQ-34 NumberToken_InvalidNumberFormat_HasTheStageParse`` (text: string) =
    let r = Script.compile ("Output.v = " + text)
    let ms = r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.InvalidNumberFormat) |> List.ofSeq
    Assert.True(not ms.IsEmpty, sprintf "No InvalidNumberFormat for %s. Diagnostics:\n%s" text (Script.describe r))
    for m in ms do Assert.Equal("Parse", Pending.stageOf m)

[<Theory>]
[<MemberData(nameof invalidTokens)>]
[<Trait("requirement", "JYRO-REQ-34")>]
let ``JYRO-REQ-34 NumberToken_NotMatchingNumberLiteral_IsRejectedAtParseTime`` (text: string) =
    rejected ("Output.v = " + text) |> assertParseStage

[<Theory>]
[<InlineData("0xE+1", 15.0)>]
[<InlineData("0xe-1", 13.0)>]
[<InlineData("0XE+1", 15.0)>]
[<InlineData("0b1+1", 2.0)>]
[<InlineData("1e+5+3", 100003.0)>]
[<InlineData("1e-5-1", -0.99999)>]
[<InlineData("1E2-1", 99.0)>]
[<InlineData("3-1", 2.0)>]
[<InlineData("2e3", 2000.0)>]
[<Trait("requirement", "JYRO-REQ-34")>]
let ``JYRO-REQ-34 NumberToken_SignOnlyJoinsAfterEInADecimalRun`` (expr: string, expected: float) =
    Assert.Equal(expected, Values.asFloat (numberOf expr), 12)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-34")>]
let ``JYRO-REQ-34 NumberToken_LeadingDot_IsAParseErrorNotInvalidNumberFormat`` () =
    let r = rejected "Output.v = .5"
    Assert.DoesNotContain(MessageCode.InvalidNumberFormat, Script.errorCodes r)
    assertParseStage r

/// InvalidNumberFormat is a lexical error in a number token, so 130.1.2 (JYRO-REQ-2122)
/// places it at that whole token: the longest run this requirement reads, not its
/// first digit nor the point where it stops matching number_literal. Carried here from
/// JYRO-REQ-1475's place rows (JYRO-ADR-122 item 4). (source, line, column, length).
let invalidNumberPlaces : seq<obj[]> =
    [ "Output.x = 12abc", 1, 12, 5
      "Output.a = 1\nOutput.x = 1e5e5", 2, 12, 5
      "Output.x = 0b2 + 1", 1, 12, 3
      "var y = 1_000", 1, 9, 5
      "Output.a = 1\r\n  Output.x = 1.5.3", 2, 14, 5
      "Output.x = 1e+", 1, 12, 3 ]
    |> Seq.map (fun (s, l, c, n) -> [| box s; box l; box c; box n |])

[<Theory>]
[<MemberData(nameof invalidNumberPlaces)>]
[<Trait("requirement", "JYRO-REQ-34")>]
let ``JYRO-REQ-34 an invalid number token is placed at the whole token`` (source: string, line: int, column: int, length: int) =
    Assert.Equal((line, column, length), placeOf MessageCode.InvalidNumberFormat source)

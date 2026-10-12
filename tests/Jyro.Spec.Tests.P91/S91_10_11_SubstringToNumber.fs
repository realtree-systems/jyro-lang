/// jyro-spec 91.10 Substring, 91.11 ToNumber.
module RealTree.Jyro.Spec.Tests.P91.S91_10_11_SubstringToNumber

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P91.Support

let badValues = badData

// ============================================================================
// 91.10 Substring
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-469")>]
let Substring_PositionalNamedAndOptionalLength_ReturnString () =
    Assert.Equal("el", str "Substring(\"hello\", 1, 2)")
    Assert.Equal("el", str "Substring(text: \"hello\", start: 1, length: 2)")
    Assert.Equal("el", str "Substring(length: 2, start: 1, text: \"hello\")")
    Assert.Equal("ello", str "Substring(\"hello\", 1)")
    Assert.Equal("ello", str "Substring(text: \"hello\", start: 1)")
    Assert.Equal("string", typeOf "Substring(\"hello\", 1, 2)")
    Assert.Equal("string", typeOf "Substring(\"hello\", 5)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-469")>]
let Substring_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "Substring(\"a\")"
    expectErr MessageCode.TooManyArguments "Substring(\"a\", 0, 1, 2)"
    expectErrAmong MessageCode.UnknownNamedArgument "Substring(text: \"a\", begin: 0)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1754")>]
[<InlineData(1, 3, "ell")>]
[<InlineData(0, 5, "hello")>]
[<InlineData(0, 1, "h")>]
[<InlineData(4, 1, "o")>]
[<InlineData(2, 2, "ll")>]
[<InlineData(2, 0, "")>]
[<InlineData(5, 0, "")>]
[<InlineData(0, 0, "")>]
[<InlineData(3, 2, "lo")>]
let ``JYRO-REQ-1754 Substring_ReturnsLengthCodeUnitsFromStart`` (start: int, length: int, expected: string) =
    // every case has start + length at most Length("hello"), the scope of the clause
    Assert.True(start + length <= 5)
    Assert.Equal(expected, str (sprintf "Substring(\"hello\", %d, %d)" start length))

/// Code units are counted, not code points: a surrogate pair is two.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1754")>]
let ``JYRO-REQ-1754 Substring_CountsCodeUnitsOfAnAstralPair`` () =
    let input = inputOf [ "t", "a" + pair + "b" ]
    Assert.Equal(pair, strIn input "Substring(Input.t, 1, 2)")
    Assert.Equal(pair + "b", strIn input "Substring(Input.t, 1, 3)")
    Assert.Equal("a" + pair, strIn input "Substring(Input.t, 0, 3)")
    Assert.Equal(hi, strIn input "Substring(Input.t, 1, 1)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-471")>]
[<InlineData(2, "llo")>]
[<InlineData(0, "hello")>]
[<InlineData(4, "o")>]
let Substring_NoLength_ReturnsToEnd (start: int, expected: string) =
    Assert.Equal(expected, str (sprintf "Substring(\"hello\", %d)" start))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-472")>]
[<InlineData(3, 10L, "lo")>]
[<InlineData(3, 3L, "lo")>]
[<InlineData(3, 2L, "lo")>]
[<InlineData(3, 2147483647L, "lo")>]
[<InlineData(0, 2147483647L, "hello")>]
[<InlineData(4, 2147483647L, "o")>]
let Substring_StartPlusLengthPastEnd_ReturnsToEnd (start: int, length: int64, expected: string) =
    Assert.Equal(expected, str (sprintf "Substring(\"hello\", %d, %d)" start length))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-473")>]
[<InlineData("Substring(\"hello\", 5)")>]
[<InlineData("Substring(\"hello\", 5, 0)")>]
[<InlineData("Substring(\"hello\", 5, 3)")>]
[<InlineData("Substring(\"\", 0)")>]
[<InlineData("Substring(\"\", 0, 5)")>]
let Substring_StartEqualToLength_ReturnsEmptyString (expr: string) =
    Assert.Equal("", str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-474")>]
[<InlineData("Substring(\"abc\", 4)")>]
[<InlineData("Substring(\"abc\", 4, 0)")>]
[<InlineData("Substring(\"abc\", 4, 1)")>]
[<InlineData("Substring(\"abc\", 2147483647)")>]
[<InlineData("Substring(\"\", 1)")>]
[<InlineData("Substring(null, 1)")>]
[<InlineData("Substring(\"abc\", 5, null)")>]
let ``JYRO-REQ-474 Substring_StartPastLength_FailsWithIndexOutOfRange`` (expr: string) =
    expectErr MessageCode.IndexOutOfRange expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-474")>]
let ``JYRO-REQ-474 Substring_StartAtOrBelowLength_NoIndexOutOfRange`` () =
    Assert.Equal("", str "Substring(\"abc\", 3)")
    Assert.Equal("c", str "Substring(\"abc\", 2)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-474")>]
[<InlineData("Substring(\"abc\", 5.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 3000000000)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 5, -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", 5, 1.5)", "IntegerRequired")>]
[<InlineData("Substring([], 5)", "ArgumentTypeMismatch")>]
let ``JYRO-REQ-474 Substring_StartPastLengthWithFailingArgumentCheck_RaisesThatCheckFirst`` (expr: string, code: string) =
    expectErr (Enum.Parse<MessageCode>(code)) expr

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1757")>]
[<InlineData("Substring(\"abc\", -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", -2147483649)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 0, -2147483648)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", 0, -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", -2147483648)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", 1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 0, 1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 0, 0.0000001)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", -1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 0, -1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 2147483648)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 0, 2147483648)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 0, 1e20)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", -3000000000)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 0, -2147483649)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", -1, 1.5)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", 1.5, -1)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", -1.5, -1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", -3000000000, -2147483649)", "IntegerOutOfRange")>]
let ``JYRO-REQ-1757 Substring_StartOrLengthNotNonNegativeInteger_FailsWith90_3Code`` (expr: string, code: string) =
    expectErr (Enum.Parse<MessageCode>(code)) expr

/// A `null` length is not checked, and a valid start and length raise none.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1757")>]
let ``JYRO-REQ-1757 Substring_NullLengthAndValidArguments_RaiseNoCheckError`` () =
    Assert.Equal("abc", str "Substring(\"abc\", 0, null)")
    Assert.Equal("bc", str "Substring(\"abc\", 1, null)")
    Assert.Equal("", str "Substring(\"abc\", 0, 0)")
    Assert.Equal("a", str "Substring(\"abc\", 0, 1)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1757")>]
[<InlineData("Substring(\"abc\", 0, 2147483647)", "abc")>]
[<InlineData("Substring(\"abc\", 1.0)", "bc")>]
[<InlineData("Substring(\"abc\", 0, 2.0)", "ab")>]
[<InlineData("Substring(\"abc\", Power(4, 0.5))", "c")>]
[<InlineData("Substring(\"abc\", 0, Power(4, 0.5))", "ab")>]
[<InlineData("Substring(\"abc\", -0)", "abc")>]
let ``JYRO-REQ-1757 Substring_IntegralStartOrLength_Accepted`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-476")>]
[<InlineData("Substring(\"abc\", 3, -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", 3, 1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 3, 2147483648)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 3, \"x\")", "ArgumentTypeMismatch")>]
[<InlineData("Substring(\"abc\", 5, -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"abc\", 5, 1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"abc\", 5, 3000000000)", "IntegerOutOfRange")>]
[<InlineData("Substring(\"abc\", 5, true)", "ArgumentTypeMismatch")>]
[<InlineData("Substring(\"ab\", 5, -1)", "NonNegativeIntegerRequired")>]
let ``JYRO-REQ-476 Substring_StartAtOrPastLength_LengthStillChecked`` (expr: string, code: string) =
    expectErr (Enum.Parse<MessageCode>(code)) expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-476")>]
let ``JYRO-REQ-476 Substring_StartAtLengthWithValidOrNullLength_ReturnsEmpty`` () =
    Assert.Equal("", str "Substring(\"abc\", 3, 5)")
    Assert.Equal("", str "Substring(\"abc\", 3, null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-477")>]
[<InlineData(0)>]
[<InlineData(2)>]
[<InlineData(5)>]
let Substring_LengthZero_ReturnsEmptyString (start: int) =
    Assert.Equal("", str (sprintf "Substring(\"hello\", %d, 0)" start))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-478")>]
let Substring_CountsUtf16CodeUnits_BoundInsidePairGivesLoneSurrogate () =
    let input = inputOf [ "t", "a" + pair + "b" ]
    Assert.Equal(4, Values.asDecimal (evalIn input "Length(Input.t)") |> int)
    Assert.Equal(hi, strIn input "Substring(Input.t, 1, 1)")
    Assert.Equal(lo + "b", strIn input "Substring(Input.t, 2)")
    Assert.Equal("a" + hi, strIn input "Substring(Input.t, 0, 2)")
    Assert.Equal("b", strIn input "Substring(Input.t, 3)")
    Assert.Equal(pair, strIn input "Substring(Input.t, 1, 2)")
    Assert.Equal(lo, strIn input "Substring(Input.t, 2, 1)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-479")>]
[<InlineData("null")>]
[<InlineData("\"1\"")>]
[<InlineData("true")>]
[<InlineData("[]")>]
[<InlineData("[1]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``JYRO-REQ-479 Substring_StartNotNumber_FailsWithArgumentTypeMismatch`` (start: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Substring(\"hello\", %s)" start)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Substring(\"hello\", %s, 1)" start)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-479")>]
[<InlineData("\"2\"")>]
[<InlineData("true")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``JYRO-REQ-479 Substring_LengthNotNumberNotNull_FailsWithArgumentTypeMismatch`` (length: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Substring(\"hello\", 1, %s)" length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-479")>]
let ``JYRO-REQ-479 Substring_NullLength_MeansNoLength`` () =
    Assert.Equal("ello", str "Substring(\"hello\", 1, null)")
    Assert.Equal("ello", str "Substring(\"hello\", 1, Input.missing)")
    Assert.Equal("ello", str "Substring(text: \"hello\", start: 1, length: null)")
    Assert.Equal("", str "Substring(\"hello\", 5, null)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-480")>]
let ``JYRO-REQ-480 Substring_NullText_ReadAsEmptyString`` () =
    Assert.Equal("", str "Substring(null, 0)")
    Assert.Equal("", str "Substring(null, 0, 3)")
    Assert.Equal("", str "Substring(Input.missing, 0)")
    expectErr MessageCode.IndexOutOfRange "Substring(null, 1)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1996")>]
[<InlineData("Substring(12345, 1, 2)", "23")>]
[<InlineData("Substring(1.50, 1)", ".5")>]
[<InlineData("Substring(true, 1)", "rue")>]
[<InlineData("Substring(false, 0, 1)", "f")>]
[<InlineData("Substring(ToNumber(\"1e30\"), 1)", "e+30")>]
let ``JYRO-REQ-1996 Substring_NumberOrBooleanText_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1996")>]
let ``JYRO-REQ-1996 Substring_ExactNumberText_StartPastCanonicalFormFails`` () =
    // 1.50 reads as "1.5" (Length 3), so start 4 is past its end
    expectErr MessageCode.IndexOutOfRange "Substring(1.50, 4)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-482")>]
[<MemberData(nameof badValues)>]
let Substring_ContainerOrFunctionText_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Substring(%s, 0)" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Substring(%s, 0, 1)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-483")>]
let Substring_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("b", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "Substring(\"ab\", 1)"))

// ============================================================================
// 91.11 ToNumber
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-484")>]
let ToNumber_PositionalAndNamed_ReturnNumberOrNull () =
    Assert.Equal(5m, Values.asDecimal (eval "ToNumber(\"5\")"))
    Assert.Equal(5m, Values.asDecimal (eval "ToNumber(text: \"5\")"))
    Assert.Equal("number", typeOf "ToNumber(\"5\")")
    assertNull "ToNumber(\"x\")"
    Assert.Equal("null", typeOf "ToNumber(\"x\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-484")>]
let ToNumber_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "ToNumber()"
    expectErr MessageCode.TooManyArguments "ToNumber(\"1\", \"2\")"
    expectErrAmong MessageCode.UnknownNamedArgument "ToNumber(value: \"1\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1769")>]
[<InlineData("1E+2", "100")>]
[<InlineData("0.5e-3", "0.0005")>]
[<InlineData(" \t\r\n42\n\r\t ", "42")>]
[<InlineData("0", "0")>]
[<InlineData("-0", "0")>]
[<InlineData("42", "42")>]
[<InlineData("-1.5", "-1.5")>]
[<InlineData("0.25", "0.25")>]
[<InlineData("1e3", "1000")>]
[<InlineData("1E+3", "1000")>]
[<InlineData("1e-3", "0.001")>]
[<InlineData("1.5e-3", "0.0015")>]
[<InlineData("0e5", "0")>]
[<InlineData(" 42 ", "42")>]
[<InlineData("\t7\n", "7")>]
[<InlineData("\r\n7\r\n", "7")>]
[<InlineData("123456789012345678901234567", "123456789012345678901234567")>]
let ``JYRO-REQ-1769 ToNumber_JsonNumberAfterTrim_Accepted`` (text: string, expected: string) =
    let v = eval ("ToNumber(" + lit text + ")")
    Assert.Equal(Decimal.Parse(expected, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture), Values.asDecimal v)

/// The "otherwise null" side: only U+0020, U+0009, U+000A and U+000D are trimmed
/// from each end, so any other white space, or a trimmed text that is not a JSON
/// number, gives `null` with no error.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1769")>]
[<InlineData(" 42")>]
[<InlineData("42 ")>]
[<InlineData("\u000B42")>]
[<InlineData("\u000C42")>]
[<InlineData("42\u000C")>]
[<InlineData(" 42")>]
[<InlineData("〰42")>]
[<InlineData("  42  ")>]
[<InlineData("1_000")>]
[<InlineData("1e")>]
[<InlineData("+1")>]
[<InlineData(".5")>]
[<InlineData("1.")>]
[<InlineData("0x10")>]
[<InlineData("NaN")>]
[<InlineData("Infinity")>]
[<InlineData("-")>]
[<InlineData("")>]
[<InlineData("1 2")>]
[<InlineData("--1")>]
[<InlineData("1e+")>]
let ``JYRO-REQ-1769 ToNumber_TextNotAJsonNumberAfterTheFourWhiteSpaces_ReturnsNullWithoutError`` (text: string) =
    let result = run ("ToNumber(" + lit text + ")")
    Assert.Empty(Script.errorCodes result)
    let v = resultOf result
    Assert.True(isNullValue v, sprintf "ToNumber(%s) gave %s, not null" (lit text) (Values.toJson v))

/// 91.11.5: a value that is not finite gives `null`.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1769")>]
[<InlineData("1e999")>]
[<InlineData("-1e999")>]
[<InlineData(" 1e400 ")>]
let ``JYRO-REQ-1769 ToNumber_NotFiniteValue_ReturnsNull`` (text: string) =
    assertNull ("ToNumber(" + lit text + ")")

/// `-0` and the statement's own example.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1769")>]
let ``JYRO-REQ-1769 ToNumber_StatementExamples`` () =
    Assert.Equal(42m, Values.asDecimal (eval "ToNumber(\" 42 \")"))
    Assert.Equal("number", typeOf "ToNumber(\"-0\")")
    Assert.Equal(0m, Values.asDecimal (eval "ToNumber(\"-0\")"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-486")>]
[<InlineData("1,234")>]
[<InlineData("$5")>]
[<InlineData("(5)")>]
[<InlineData("5-")>]
[<InlineData("+5")>]
[<InlineData(".5")>]
[<InlineData("5.")>]
[<InlineData("01")>]
[<InlineData("0x10")>]
[<InlineData("Infinity")>]
[<InlineData("NaN")>]
[<InlineData("")>]
let ``JYRO-REQ-486 ToNumber_ListedNonJsonText_ReturnsNull`` (text: string) =
    let result = run ("ToNumber(" + lit text + ")")
    Assert.Empty(Script.errorCodes result)
    let v = resultOf result
    Assert.True(isNullValue v, sprintf "ToNumber(%s) gave %s, not null" (lit text) (Values.toJson v))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-486")>]
[<InlineData("   ")>]
[<InlineData("1e")>]
[<InlineData("-")>]
[<InlineData("1 2")>]
[<InlineData("0b1")>]
[<InlineData("-Infinity")>]
[<InlineData("1_000")>]
let ``JYRO-REQ-486 ToNumber_OtherNonJsonText_ReturnsNull`` (text: string) =
    assertNull ("ToNumber(" + lit text + ")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-487")>]
[<InlineData("0.1")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("79228162514264337593543950336")>]
[<InlineData("0.0000000000000000000000000001")>]
[<InlineData("0.00000000000000000000000000001")>]
[<InlineData("1e28")>]
[<InlineData("1e29")>]
[<InlineData("1.7976931348623157e308")>]
[<InlineData("1e30")>]
[<InlineData("-2.5")>]
let ``JYRO-REQ-487 ToNumber_Representation_SameAsJson`` (text: string) =
    let fromScript = eval ("ToNumber(" + lit text + ")")
    let fromJson = Values.json text
    Assert.Equal(Values.isExact fromJson, Values.isExact fromScript)
    if Values.isExact fromJson then Assert.Equal(Values.asDecimal fromJson, Values.asDecimal fromScript)
    else Assert.Equal(Values.asFloat fromJson, Values.asFloat fromScript)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-487")>]
[<InlineData("0.1", true)>]
[<InlineData("79228162514264337593543950335", true)>]
[<InlineData("79228162514264337593543950336", false)>]
[<InlineData("0.0000000000000000000000000001", true)>]
[<InlineData("0.00000000000000000000000000001", false)>]
[<InlineData("1e28", true)>]
[<InlineData("1e29", false)>]
[<InlineData("1.7976931348623157e308", false)>]
let ``JYRO-REQ-487 ToNumber_ExactOnlyWhenNoRoundingNeeded`` (text: string, exact: bool) =
    Assert.Equal(exact, Values.isExact (eval ("ToNumber(" + lit text + ")")))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-487")>]
let ``JYRO-REQ-487 ToNumber_ExactResults_AddExactly`` () =
    Assert.True(boolOf "ToNumber(\"0.1\") + ToNumber(\"0.2\") == 0.3")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-488")>]
[<InlineData("1e400")>]
[<InlineData("-1e400")>]
[<InlineData("1e309")>]
[<InlineData(" 1e400 ")>]
let ToNumber_NotFiniteAsDouble_ReturnsNull (text: string) =
    let result = run ("ToNumber(" + lit text + ")")
    Assert.Empty(Script.errorCodes result)
    Assert.True(isNullValue (resultOf result))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-488")>]
let ToNumber_LargestFiniteAndUnderflow_AreNumbers () =
    Assert.Equal(Double.MaxValue, Values.asFloat (eval "ToNumber(\"1.7976931348623157e308\")"))
    Assert.Equal(0.0, Values.asFloat (eval "ToNumber(\"1e-400\")"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-489")>]
let ``JYRO-REQ-489 ToNumber_Null_FailsWithArgumentTypeMismatch`` () =
    expectErr MessageCode.ArgumentTypeMismatch "ToNumber(null)"
    expectErr MessageCode.ArgumentTypeMismatch "ToNumber(Input.missing)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-490")>]
[<InlineData("5")>]
[<InlineData("1.5")>]
[<InlineData("Power(2, 0.5)")>]
[<InlineData("true")>]
[<InlineData("false")>]
let ``JYRO-REQ-490 ToNumber_NumberOrBoolean_FailsWithArgumentTypeMismatch`` (expr: string) =
    expectErr MessageCode.ArgumentTypeMismatch ("ToNumber(" + expr + ")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-491")>]
[<InlineData("[\"1\"]")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("{a: \"1\"}")>]
[<InlineData("x => x")>]
let ToNumber_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch ("ToNumber(" + bad + ")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-492")>]
let ToNumber_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal(1m, Values.asDecimal (evalUnderMaxEffect FunctionEffect.Pure "ToNumber(\"1\")"))

/// jyro-spec 94.3 ParseDate. The text is given through `Input.s`, so any
/// character reaches the function unchanged by string-literal escaping.
module RealTree.Jyro.Spec.Tests.P93_94.S94_03_ParseDate

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private parse (s: string) : JyroValue = evalIn [ "s", Values.str s ] "ParseDate(Input.s)"

let private parsesTo (s: string) (expected: string) = assertString expected (parse s)

let private refuses (s: string) =
    let r = runIn [ "s", Values.str s ] "Output.r = ParseDate(Input.s)"
    assertNoDiagnostics r
    assertNull (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1010")>]
let ``ParseDate_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = ParseDate()"
    expectCompileError MessageCode.TooManyArguments "Output.r = ParseDate(\"a\", \"b\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1010")>]
let ``ParseDate_ParameterIsNamedText`` () =
    assertString "2024-01-15T00:00:00.000Z" (eval "ParseDate(text: \"2024-01-15\")")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = ParseDate(s: \"x\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1010")>]
let ``ParseDate_ReturnsAStringOrNull`` () =
    assertString "string" (eval "TypeOf(ParseDate(\"2024-01-15\"))")
    assertString "null" (eval "TypeOf(ParseDate(\"nope\"))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1011")>]
[<InlineData("2024-01-15T10:20:30Z", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-01-15T10:20:30.5Z", "2024-01-15T10:20:30.500Z")>]
[<InlineData("2024-01-15T10:20:30.123Z", "2024-01-15T10:20:30.123Z")>]
[<InlineData("0001-01-01T00:00:00Z", "0001-01-01T00:00:00.000Z")>]
[<InlineData("2024-01-15T10:20:30+02:00", "2024-01-15T08:20:30.000Z")>]
[<InlineData("2024-01-05T01:02:03Z", "2024-01-05T01:02:03.000Z")>]
[<InlineData("0999-01-01T00:00:00Z", "0999-01-01T00:00:00.000Z")>]
let ``ParseDate_ReturnsTheUtcInstantInTheCanonicalForm`` (text: string, expected: string) =
    parsesTo text expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1012")>]
[<InlineData("2024-01-15")>]
[<InlineData("2024-02-29")>]
[<InlineData("0001-01-01")>]
[<InlineData("9999-12-31")>]
let ``ParseDate_OfADateOnly_IsMidnightUtc`` (text: string) =
    parsesTo text (text + "T00:00:00.000Z")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1013")>]
[<InlineData("2024-01-15T10:20:30+02:00", "2024-01-15T08:20:30.000Z")>]
[<InlineData("2024-01-15T10:20:30-05:30", "2024-01-15T15:50:30.000Z")>]
[<InlineData("2024-01-01T01:00:00+02:00", "2023-12-31T23:00:00.000Z")>]
[<InlineData("2024-12-31T23:00:00-01:00", "2025-01-01T00:00:00.000Z")>]
[<InlineData("2024-01-15T10:20:30+00:00", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-01-15T10:20:30-00:00", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-02-28T23:30:00-00:45", "2024-02-29T00:15:00.000Z")>]
[<InlineData("2024-03-01T00:10:00+00:15", "2024-02-29T23:55:00.000Z")>]
[<InlineData("2024-01-15T10:20:30+23:59", "2024-01-14T10:21:30.000Z")>]
[<InlineData("2024-01-15T10:20:30-23:59", "2024-01-16T10:19:30.000Z")>]
[<InlineData("2024-02-29T23:59:59.999-00:01", "2024-03-01T00:00:59.999Z")>]
let ``JYRO-REQ-1013 ParseDate_SubtractsANumericOffset`` (text: string, expected: string) =
    parsesTo text expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1014")>]
[<InlineData("2024-01-02", "2024-01-02T00:00:00.000Z")>]
[<InlineData("2024-01-02T03:04:05Z", "2024-01-02T03:04:05.000Z")>]
[<InlineData("2024-01-02t03:04:05.123z", "2024-01-02T03:04:05.123Z")>]
[<InlineData("2024-01-02T03:04:05+05:30", "2024-01-01T21:34:05.000Z")>]
[<InlineData("2024-01-02T03:04:05-00:00", "2024-01-02T03:04:05.000Z")>]
[<InlineData("2024-01-02T03:04:05", "2024-01-02T03:04:05.000Z")>]
[<InlineData("2024-01-02T03:04:05.1234567Z", "2024-01-02T03:04:05.123Z")>]
[<InlineData("2016-12-31T23:59:60Z", "2016-12-31T23:59:59.000Z")>]
let ``JYRO-REQ-1014 ParseDate_AcceptsTheRfc3339Forms`` (text: string, expected: string) =
    parsesTo text expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1014")>]
[<InlineData("2024-01-02 03:04:05Z")>]
[<InlineData("2024-01-02T03:04Z")>]
[<InlineData("2024-01-02T03:04:05.Z")>]
[<InlineData("2024-1-02")>]
[<InlineData("2024-01-2")>]
[<InlineData("24-01-02")>]
[<InlineData("+2024-01-02")>]
[<InlineData("2024-01-02Z")>]
[<InlineData("2024-01-02T")>]
[<InlineData("2024-01-02T03:04:05+0530")>]
[<InlineData("2024-01-02T03:04:05+05")>]
[<InlineData("2024-01-02T03:04:05+5:30")>]
[<InlineData("2024-01-02T24:00:00Z")>]
[<InlineData("2024-01-02T03:60:00Z")>]
[<InlineData("2024-01-02T03:04:61Z")>]
[<InlineData("2024-01-02T03:04:05+24:00")>]
[<InlineData("2024-01-02T03:04:05+05:60")>]
[<InlineData(" 2024-01-02")>]
[<InlineData("2024-01-02 ")>]
[<InlineData("2024-01-02T03:04:05ZZ")>]
[<InlineData("2024-01-02T03:04:05 Z")>]
[<InlineData("2024-W01-1")>]
[<InlineData("2024-002")>]
[<InlineData("20240102")>]
[<InlineData("2024/01/02")>]
[<InlineData("２０２４-01-02")>]
[<InlineData("٢٠٢٤-01-02")>]
[<InlineData("2024-01-02T03:04:05UTC")>]
[<InlineData("")>]
[<InlineData("nope")>]
let ``JYRO-REQ-1014 ParseDate_RefusesEveryOtherString_WithNull`` (text: string) =
    refuses text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1014")>]
[<Trait("requirement", "JYRO-REQ-1022")>]
[<InlineData("2024-01-02 03:04:05Z")>]
[<InlineData("2024-01-02T03:04Z")>]
[<InlineData("2024-01-02T03:04:05+0530")>]
[<InlineData(" 2024-01-02")>]
[<InlineData("")>]
let ``JYRO-REQ-1014 JYRO-REQ-1022 FormatDate_RefusesTheSameStrings_WithDateFormatInvalid`` (text: string) =
    expectExprErrorIn [ "s", Values.str text ] MessageCode.DateFormatInvalid "FormatDate(Input.s, \"yyyy\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1014")>]
[<Trait("requirement", "JYRO-REQ-1022")>]
[<InlineData("2024-01-02t03:04:05.123z")>]
[<InlineData("2024-01-02T03:04:05+05:30")>]
[<InlineData("2024-01-02T03:04:05")>]
[<InlineData("2016-12-31T23:59:60Z")>]
let ``JYRO-REQ-1014 JYRO-REQ-1022 FormatDate_GivesTheSameInstantAsParseDate`` (text: string) =
    assertTrue (evalIn [ "s", Values.str text ] "FormatDate(Input.s, \"yyyy-MM-ddTHH:mm:ss.fffZ\") == ParseDate(Input.s)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1015")>]
[<InlineData("2024-01-15t10:20:30z", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-01-15T10:20:30z", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-01-15t10:20:30Z", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-01-15t10:20:30+01:00", "2024-01-15T09:20:30.000Z")>]
[<InlineData("2024-01-15t10:20:30", "2024-01-15T10:20:30.000Z")>]
let ``ParseDate_AcceptsTAndZInEitherCase`` (text: string, expected: string) =
    parsesTo text expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1016")>]
[<InlineData("2024-02-30")>]
[<InlineData("2023-02-29")>]
[<InlineData("2100-02-29")>]
[<InlineData("1900-02-29")>]
[<InlineData("2024-04-31")>]
[<InlineData("2024-06-31")>]
[<InlineData("2024-13-01")>]
[<InlineData("2024-00-10")>]
[<InlineData("2024-01-00")>]
[<InlineData("2024-01-32")>]
[<InlineData("2023-02-29T00:00:00Z")>]
let ``ParseDate_OfACalendarDateThatDoesNotExist_ReturnsNull`` (text: string) =
    refuses text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1016")>]
[<InlineData("2024-02-29")>]
[<InlineData("2000-02-29")>]
[<InlineData("2024-04-30")>]
[<InlineData("2024-01-31")>]
let ``ParseDate_OfAnExistingMonthEndOrLeapDay_ReadsIt`` (text: string) =
    parsesTo text (text + "T00:00:00.000Z")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1017")>]
[<InlineData("0001-01-01T00:00:00.000Z", "0001-01-01T00:00:00.000Z")>]
[<InlineData("9999-12-31T23:59:59.999Z", "9999-12-31T23:59:59.999Z")>]
[<InlineData("9999-12-31T23:59:59.9999Z", "9999-12-31T23:59:59.999Z")>]
[<InlineData("9999-12-31T23:59:60Z", "9999-12-31T23:59:59.000Z")>]
[<InlineData("9999-12-31T23:59:60.9995Z", "9999-12-31T23:59:59.999Z")>]
[<InlineData("9999-12-31T23:59:59.999+00:01", "9999-12-31T23:58:59.999Z")>]
[<InlineData("0001-01-01T01:00:00+01:00", "0001-01-01T00:00:00.000Z")>]
[<InlineData("0000-12-31T23:30:00-01:00", "0001-01-01T00:30:00.000Z")>]
let ``JYRO-REQ-1017 ParseDate_ChecksTheRangeOnTheAdjustedInstant_InRange`` (text: string, expected: string) =
    parsesTo text expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1017")>]
[<InlineData("9999-12-31T23:30:00-01:00")>]
[<InlineData("9999-12-31T23:59:59.999-00:01")>]
[<InlineData("0001-01-01T00:00:00+00:01")>]
[<InlineData("0000-12-31T23:59:59Z")>]
[<InlineData("0000-01-01")>]
let ``JYRO-REQ-1017 ParseDate_OfAnInstantOutsideTheRange_ReturnsNull`` (text: string) =
    refuses text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1017")>]
[<Trait("requirement", "JYRO-REQ-1023")>]
[<InlineData("9999-12-31T23:30:00-01:00")>]
[<InlineData("0000-12-31T23:59:59Z")>]
[<InlineData("0001-01-01T00:00:00+00:01")>]
let ``JYRO-REQ-1017 JYRO-REQ-1023 FormatDate_OfAnInstantOutsideTheRange_RaisesDateFormatInvalid`` (text: string) =
    expectExprErrorIn [ "s", Values.str text ] MessageCode.DateFormatInvalid "FormatDate(Input.s, \"yyyy\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1018")>]
[<InlineData("2024-01-15T10:20:60Z", "2024-01-15T10:20:59.000Z")>]
[<InlineData("2016-12-31T23:59:60.5Z", "2016-12-31T23:59:59.500Z")>]
[<InlineData("2024-01-15T10:20:60.999Z", "2024-01-15T10:20:59.999Z")>]
[<InlineData("2024-01-15T10:20:60+01:00", "2024-01-15T09:20:59.000Z")>]
[<InlineData("2024-01-15T23:59:60", "2024-01-15T23:59:59.000Z")>]
let ``JYRO-REQ-1018 ParseDate_ReadsSecond60As59_KeepingTheFraction`` (text: string, expected: string) =
    parsesTo text expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1018")>]
let ``JYRO-REQ-1018 ParseDate_DoesNotAcceptSecond61`` () = refuses "2024-01-15T10:20:61Z"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1019")>]
[<InlineData("2024-01-15T10:20:30.1239Z", "2024-01-15T10:20:30.123Z")>]
[<InlineData("2024-01-15T10:20:30.9999Z", "2024-01-15T10:20:30.999Z")>]
[<InlineData("2024-01-15T10:20:30.123456789Z", "2024-01-15T10:20:30.123Z")>]
[<InlineData("2024-01-15T10:20:30.1Z", "2024-01-15T10:20:30.100Z")>]
[<InlineData("2024-01-15T10:20:30.12Z", "2024-01-15T10:20:30.120Z")>]
[<InlineData("2024-01-15T10:20:30.0009Z", "2024-01-15T10:20:30.000Z")>]
[<InlineData("2024-12-31T23:59:59.99999999Z", "2024-12-31T23:59:59.999Z")>]
[<InlineData("2024-01-15T10:20:30.0005+01:00", "2024-01-15T09:20:30.000Z")>]
let ``JYRO-REQ-1019 ParseDate_DiscardsFractionDigitsAfterTheThird_NeverRounds`` (text: string, expected: string) =
    parsesTo text expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1020")>]
let ``JYRO-REQ-1020 ParseDate_OfNullNumberOrBoolean_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "5"; "0"; "true"; "false" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "ParseDate(%s)" lit)
    for v in [ Values.nul; Values.exact 5m; Values.bool true ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "ParseDate(Input.v)"

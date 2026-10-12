/// jyro-spec 94.4 FormatDate. The date and the format are given through
/// `Input.d` and `Input.f`, so they reach the function unchanged by escaping.
module RealTree.Jyro.Spec.Tests.P93_94.S94_04_FormatDate

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private fmt (d: string) (f: string) : JyroValue =
    evalIn [ "d", Values.str d; "f", Values.str f ] "FormatDate(Input.d, Input.f)"

let private formatsTo (d: string) (f: string) (expected: string) = assertString expected (fmt d f)

let private expectFormatError (code: MessageCode) (d: string) (f: string) =
    expectExprErrorIn [ "d", Values.str d; "f", Values.str f ] code "FormatDate(Input.d, Input.f)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1021")>]
let ``FormatDate_Arity_IsTwoArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = FormatDate(\"2024-01-01\")"
    expectCompileError MessageCode.TooManyArguments "Output.r = FormatDate(\"2024-01-01\", \"yyyy\", 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1021")>]
let ``FormatDate_ParametersAreDateAndFormat_AndReturnsAString`` () =
    assertString "2024" (eval "FormatDate(date: \"2024-01-01\", format: \"yyyy\")")
    assertString "2024" (eval "FormatDate(format: \"yyyy\", date: \"2024-01-01\")")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = FormatDate(d: \"2024-01-01\", format: \"yyyy\")"
    assertString "string" (eval "TypeOf(FormatDate(\"2024-01-01\", \"yyyy\"))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1022")>]
[<InlineData("2024-01-15")>]
[<InlineData("2024-01-15T10:20:30Z")>]
[<InlineData("2024-01-15T10:20:30+02:00")>]
[<InlineData("2024-01-15t10:20:30z")>]
[<InlineData("2024-01-15T10:20:30")>]
[<InlineData("2024-01-15T10:20:60.5Z")>]
[<InlineData("2024-01-15T10:20:30.1239Z")>]
[<InlineData("2024-01-01T01:00:00+02:00")>]
let ``JYRO-REQ-1022 FormatDate_ReadsDateAsParseDateReadsText`` (d: string) =
    assertTrue (evalIn [ "d", Values.str d ] "FormatDate(Input.d, \"yyyy-MM-ddTHH:mm:ss.fffZ\") == ParseDate(Input.d)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1022")>]
[<InlineData("2024-02-30")>]
[<InlineData("x")>]
[<InlineData("")>]
[<InlineData("2024-1-5")>]
[<InlineData("2024-01-02 03:04:05Z")>]
let ``JYRO-REQ-1022 FormatDate_RefusesWhatParseDateRefuses`` (d: string) =
    expectFormatError MessageCode.DateFormatInvalid d "yyyy"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1023")>]
[<InlineData("2024-02-30")>]
[<InlineData("not a date")>]
[<InlineData("2024-1-5")>]
[<InlineData("2024-01-15T25:00:00Z")>]
[<InlineData("0000-12-31")>]
[<InlineData("15/01/2024")>]
[<InlineData("")>]
let ``JYRO-REQ-1023 FormatDate_OfAnUnreadableDate_RaisesDateFormatInvalid`` (d: string) =
    expectFormatError MessageCode.DateFormatInvalid d "yyyy"

/// 90.3.8 step 1: `date` goes through all of its own checks, its type and then whether it can
/// be read (94.4.3, a check of `date` alone, row 5 of 90.3.9), before `format` is read at all.
/// So an unreadable `date` gives `DateFormatInvalid` whatever is wrong with `format`.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1023")>]
[<InlineData("5")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("[1]")>]
[<InlineData("{a: 1}")>]
[<InlineData("\"yyy\"")>]
[<InlineData("\"'abc\"")>]
let ``JYRO-REQ-1023 JYRO-DEF-648 FormatDate_WithAnUnreadableDate_RaisesDateFormatInvalid_BeforeAnyCheckOfFormat`` (format: string) =
    expectExprError MessageCode.DateFormatInvalid (sprintf "FormatDate(\"not a date\", %s)" format)
    expectExprError MessageCode.DateFormatInvalid (sprintf "FormatDate(format: %s, date: \"2024-02-30\")" format)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1024")>]
let ``JYRO-REQ-1024 FormatDate_WithNullNumberOrBooleanDate_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "FormatDate(%s, \"yyyy\")" lit)
    for v in [ Values.nul; Values.exact 5m; Values.bool false ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "FormatDate(Input.v, \"yyyy\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1024")>]
let ``JYRO-REQ-1024 FormatDate_WithNullNumberOrBooleanFormat_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "FormatDate(\"2024-01-15\", %s)" lit)
    for v in [ Values.nul; Values.exact 5m; Values.bool false ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "FormatDate(\"2024-01-15\", Input.v)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1025")>]
[<InlineData("yyyy", "2024")>]
[<InlineData("MM", "03")>]
[<InlineData("M", "3")>]
[<InlineData("dd", "05")>]
[<InlineData("d", "5")>]
[<InlineData("HH", "07")>]
[<InlineData("H", "7")>]
[<InlineData("mm", "08")>]
[<InlineData("m", "8")>]
[<InlineData("ss", "09")>]
[<InlineData("s", "9")>]
[<InlineData("fff", "012")>]
[<InlineData("M/d/yyyy", "3/5/2024")>]
let ``JYRO-REQ-1025 FormatDate_ReplacesEachTokenWithSingleDigitFields`` (f: string, expected: string) =
    formatsTo "2024-03-05T07:08:09.012Z" f expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1025")>]
[<InlineData("M", "11")>]
[<InlineData("MM", "11")>]
[<InlineData("d", "25")>]
[<InlineData("H", "13")>]
[<InlineData("m", "45")>]
[<InlineData("s", "56")>]
[<InlineData("fff", "789")>]
let ``JYRO-REQ-1025 FormatDate_ReplacesEachTokenWithTwoDigitFields`` (f: string, expected: string) =
    formatsTo "2024-11-25T13:45:56.789Z" f expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1025")>]
let ``JYRO-REQ-1025 FormatDate_TokensAtTheEndsOfTheirRanges`` () =
    formatsTo "2024-01-01T00:00:00.000Z" "H:m:s" "0:0:0"
    formatsTo "2024-01-01T00:00:00.000Z" "HH:mm:ss.fff" "00:00:00.000"
    formatsTo "0045-06-07" "yyyy" "0045"
    formatsTo "0001-01-01" "yyyy-MM-dd" "0001-01-01"
    formatsTo "9999-12-31T23:59:59.999Z" "yyyy-MM-dd HH:mm:ss.fff" "9999-12-31 23:59:59.999"
    formatsTo "9999-12-31T23:59:59.999Z" "M d H m s" "12 31 23 59 59"
    formatsTo "2024-01-01T00:00:00.000Z" "M d" "1 1"
    formatsTo "2024-10-10T10:10:10.100Z" "M d H m s fff" "10 10 10 10 10 100"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1025")>]
let ``JYRO-REQ-1025 FormatDate_UsesTheUtcInstant`` () =
    formatsTo "2024-01-01T01:00:00+02:00" "yyyy-MM-dd HH" "2023-12-31 23"
    // 23:30 at -01:00 is 00:30 UTC on the next day, month and year.
    formatsTo "2024-12-31T23:30:00-01:00" "yyyy-M-d H:mm" "2025-1-1 0:30"
    // The fraction is read to milliseconds (94.3.10) and the second 60 as 59 (94.3.9).
    formatsTo "2016-12-31T23:59:60.2509Z" "s.fff" "59.250"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1026")>]
[<InlineData("'at' HH", "at 07")>]
[<InlineData("'yyyy'", "yyyy")>]
[<InlineData("''", "'")>]
[<InlineData("HH''mm", "07'08")>]
[<InlineData("'it''s' HH", "it's 07")>]
[<InlineData("'Q1 dd'", "Q1 dd")>]
[<InlineData("'MMM' MM", "MMM 03")>]
[<InlineData("yyyy'y'", "2024y")>]
[<InlineData("'T'HH'h'", "T07h")>]
[<InlineData("'a''b''c'", "a'b'c")>]
let ``JYRO-REQ-1026 FormatDate_CopiesQuotedTextAndDoubledQuotes`` (f: string, expected: string) =
    formatsTo "2024-03-05T07:08:09Z" f expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1027")>]
[<InlineData("yyyy-MM-ddTHH:mm:ssZ", "2024-03-05T07:08:09Z")>]
[<InlineData("yyyy/MM/dd, HH.mm", "2024/03/05, 07.08")>]
[<InlineData("yyyy年MM月", "2024年03月")>]
[<InlineData("dd 123 !?", "05 123 !?")>]
[<InlineData("T", "T")>]
[<InlineData("Z", "Z")>]
[<InlineData("é HH", "é 07")>]
[<InlineData("HH\tmm\nss", "07\t08\n09")>]
[<InlineData("😀Z", "😀Z")>]
[<InlineData("\"yyyy\"", "\"2024\"")>]
let ``JYRO-REQ-1027 FormatDate_CopiesTZAndNonLettersUnchanged`` (f: string, expected: string) =
    formatsTo "2024-03-05T07:08:09Z" f expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1028")>]
[<InlineData("yyyyMMdd", "20240305")>]
[<InlineData("yyyy-M-d H:m:s.fff", "2024-3-5 7:8:9.012")>]
[<InlineData("TT", "TT")>]
[<InlineData("ZZ", "ZZ")>]
[<InlineData("yyyyTHH", "2024T07")>]
[<InlineData("MMdd", "0305")>]
[<InlineData("MMm", "038")>]
[<InlineData("'yyy'", "yyy")>]
[<InlineData("HHmmss", "070809")>]
[<InlineData("Hmmssfff", "70809012")>]
[<InlineData("TTTZZZ", "TTTZZZ")>]
[<InlineData("ZyyyyT", "Z2024T")>]
[<InlineData("dd'd'd", "05d5")>]
let ``JYRO-REQ-1028 FormatDate_ReadsMaximalRuns_AcceptingTokensAndTZRuns`` (f: string, expected: string) =
    formatsTo "2024-03-05T07:08:09.012Z" f expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1028")>]
[<InlineData("yyy")>]
[<InlineData("yyyyy")>]
[<InlineData("yy")>]
[<InlineData("y")>]
[<InlineData("MMM")>]
[<InlineData("ddd")>]
[<InlineData("HHH")>]
[<InlineData("mmm")>]
[<InlineData("sss")>]
[<InlineData("ff")>]
[<InlineData("f")>]
[<InlineData("ffff")>]
[<InlineData("t")>]
[<InlineData("z")>]
[<InlineData("a")>]
[<InlineData("Q")>]
[<InlineData("hh")>]
[<InlineData("tt")>]
[<InlineData("yyyy-MMM")>]
[<InlineData("TZt")>]
[<InlineData("YYYY")>]
[<InlineData("DD")>]
[<InlineData("SS")>]
[<InlineData("MMMM")>]
[<InlineData("yyyyMMM")>]
[<InlineData("'ok' mmm")>]
[<InlineData("HH:mm:ss.ffffff")>]
let ``JYRO-REQ-1028 FormatDate_WithARunThatIsNotAToken_RaisesDateFormatStringInvalid`` (f: string) =
    expectFormatError MessageCode.DateFormatStringInvalid "2024-03-05T07:08:09.012Z" f

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1028")>]
let ``JYRO-REQ-1028 JYRO-DEF-648 FormatDate_ChecksTheDateBeforeTheFormat`` () =
    // 90.3.8: `date`, the first parameter, through all of its checks before `format`.
    expectFormatError MessageCode.DateFormatInvalid "bad" "yyy"
    expectFormatError MessageCode.DateFormatInvalid "2024-02-30" "MMM"
    expectExprError MessageCode.ArgumentTypeMismatch "FormatDate(1, \"yyy\")"
    expectExprError MessageCode.ArgumentTypeMismatch "FormatDate(null, \"MMM\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1029")>]
[<InlineData("'abc")>]
[<InlineData("yyyy'")>]
[<InlineData("HH 'x")>]
[<InlineData("'")>]
[<InlineData("'''")>]
let ``FormatDate_WithAnUnclosedQuote_RaisesDateFormatStringInvalid`` (f: string) =
    expectFormatError MessageCode.DateFormatStringInvalid "2024-03-05" f

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1029")>]
let ``FormatDate_WithAClosedQuote_DoesNotRaise`` () =
    formatsTo "2024-03-05" "'x'" "x"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1030")>]
[<InlineData("2024-01-02")>]
[<InlineData("2024-01-02T03:04:05+05:30")>]
[<InlineData("0001-01-01")>]
[<InlineData("9999-12-31T23:59:60.9999Z")>]
let ``JYRO-REQ-1030 FormatDate_WithAnEmptyFormat_ReturnsTheEmptyString`` (d: string) =
    formatsTo d "" ""

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1030")>]
[<InlineData("not a date")>]
[<InlineData("2024-02-30")>]
[<InlineData("9999-12-31T23:30:00-01:00")>]
[<InlineData("")>]
let ``JYRO-REQ-1030 FormatDate_WithAnEmptyFormatAndAnUnreadableDate_RaisesDateFormatInvalid`` (d: string) =
    expectFormatError MessageCode.DateFormatInvalid d ""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1030")>]
let ``JYRO-REQ-1030 FormatDate_NotADateWithAnEmptyFormat_RaisesDateFormatInvalid`` () =
    // The clause's own example, as a literal.
    expectExprError MessageCode.DateFormatInvalid "FormatDate(\"not a date\", \"\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1030")>]
let ``JYRO-REQ-1030 FormatDate_WithAnEmptyFormatAndANonStringDate_RaisesArgumentTypeMismatch`` () =
    expectExprError MessageCode.ArgumentTypeMismatch "FormatDate(null, \"\")"
    expectExprError MessageCode.ArgumentTypeMismatch "FormatDate(5, \"\")"

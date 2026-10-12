/// jyro-spec 94.5 DateAdd, 94.6 DateDiff, 94.7 DatePart.
module RealTree.Jyro.Spec.Tests.P93_94.S94_05_07_DateAddDiffPart

open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private instantForm = Regex(@"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", RegexOptions.CultureInvariant)

let private add (d: string) (amount: int) (unit: string) : JyroValue =
    evalIn [ "d", Values.str d; "a", Values.int amount; "u", Values.str unit ] "DateAdd(Input.d, Input.a, Input.u)"

let private addsTo (d: string) (amount: int) (unit: string) (expected: string) =
    assertString expected (add d amount unit)

let private expectAddError (code: MessageCode) (d: string) (amount: int) (unit: string) =
    expectExprErrorIn [ "d", Values.str d; "a", Values.int amount; "u", Values.str unit ] code "DateAdd(Input.d, Input.a, Input.u)"

/// The call with `amount` given as a literal (so that a fraction or a large number is exact).
let private expectAddErrorAmount (code: MessageCode) (amountLiteral: string) (unit: string) =
    expectExprErrorIn [ "u", Values.str unit ] code (sprintf "DateAdd(\"2024-01-01\", %s, Input.u)" amountLiteral)

let private instantOf (text: string) : System.DateTimeOffset =
    System.DateTimeOffset.Parse(text, System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.AssumeUniversal)

let private diff (e: string) (s: string) (unit: string) : JyroValue =
    evalIn [ "e", Values.str e; "s", Values.str s; "u", Values.str unit ] "DateDiff(Input.s, Input.e, Input.u)"

let private expectDiffError (code: MessageCode) (e: string) (s: string) (unit: string) =
    expectExprErrorIn [ "e", Values.str e; "s", Values.str s; "u", Values.str unit ] code "DateDiff(Input.s, Input.e, Input.u)"

let private part (d: string) (p: string) : JyroValue =
    evalIn [ "d", Values.str d; "p", Values.str p ] "DatePart(Input.d, Input.p)"

let private expectPartError (code: MessageCode) (d: string) (p: string) =
    expectExprErrorIn [ "d", Values.str d; "p", Values.str p ] code "DatePart(Input.d, Input.p)"

let private readableForms =
    [ "2024-01-15"
      "2024-01-15T10:20:30Z"
      "2024-01-15T10:20:30+02:00"
      "2024-01-15t10:20:30z"
      "2024-01-15T10:20:30"
      "2024-01-15T10:20:60.5Z"
      "2024-01-15T10:20:30.1239Z" ]

let private allUnits =
    [ "second"; "seconds"; "minute"; "minutes"; "hour"; "hours"; "day"; "days"
      "week"; "weeks"; "month"; "months"; "year"; "years" ]

// ================================================================= 94.5 DateAdd

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1031")>]
let ``DateAdd_Arity_IsThreeArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = DateAdd(\"2024-01-01\", 1)"
    expectCompileError MessageCode.TooManyArguments "Output.r = DateAdd(\"2024-01-01\", 1, \"day\", 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1031")>]
let ``DateAdd_ParametersAreDateAmountUnit_AndReturnsAString`` () =
    let expected = "2024-01-02T00:00:00.000Z"
    assertString expected (eval "DateAdd(date: \"2024-01-01\", amount: 1, unit: \"day\")")
    assertString expected (eval "DateAdd(unit: \"day\", date: \"2024-01-01\", amount: 1)")
    assertString expected (eval "DateAdd(amount: 1, unit: \"day\", date: \"2024-01-01\")")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = DateAdd(d: \"2024-01-01\", amount: 1, unit: \"day\")"
    assertString "string" (eval "TypeOf(DateAdd(\"2024-01-01\", 1, \"day\"))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1032")>]
let ``JYRO-REQ-1032 DateAdd_ReadsDateAsParseDateReadsText`` () =
    for d in readableForms do
        assertTrue (evalIn [ "d", Values.str d ] "DateAdd(Input.d, 0, \"day\") == ParseDate(Input.d)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1032")>]
[<InlineData("2024-02-30")>]
[<InlineData("x")>]
[<InlineData("")>]
[<InlineData("2024-01-02 03:04:05Z")>]
let ``JYRO-REQ-1032 DateAdd_RefusesWhatParseDateRefuses`` (d: string) =
    expectAddError MessageCode.DateFormatInvalid d 0 "day"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1033")>]
[<InlineData("2024-01-01", 1, "day", "2024-01-02T00:00:00.000Z")>]
[<InlineData("2024-01-01T10:20:30.5+01:00", 1, "hour", "2024-01-01T10:20:30.500Z")>]
[<InlineData("0001-01-01", 0, "second", "0001-01-01T00:00:00.000Z")>]
[<InlineData("2024-01-01T00:00:00.007Z", 1, "minute", "2024-01-01T00:01:00.007Z")>]
let ``DateAdd_ReturnsTheCanonicalForm`` (d: string, amount: int, unit: string, expected: string) =
    let v = add d amount unit
    Assert.Matches(instantForm, Values.asString v)
    assertString expected v

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1034")>]
[<InlineData("second", 90, "2024-01-01T00:01:30.000Z")>]
[<InlineData("minute", 90, "2024-01-01T01:30:00.000Z")>]
[<InlineData("hour", 25, "2024-01-02T01:00:00.000Z")>]
[<InlineData("day", 1, "2024-01-02T00:00:00.000Z")>]
[<InlineData("day", 60, "2024-03-01T00:00:00.000Z")>]
[<InlineData("week", 2, "2024-01-15T00:00:00.000Z")>]
[<InlineData("month", 1, "2024-02-01T00:00:00.000Z")>]
[<InlineData("month", 14, "2025-03-01T00:00:00.000Z")>]
[<InlineData("year", 1, "2025-01-01T00:00:00.000Z")>]
let ``DateAdd_AddsAmountTimesTheUnit_SingularAndPlural`` (unit: string, amount: int, expected: string) =
    addsTo "2024-01-01T00:00:00.000Z" amount unit expected
    addsTo "2024-01-01T00:00:00.000Z" amount (unit + "s") expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2127")>]
[<InlineData("fortnight")>]
[<InlineData("")>]
[<InlineData("ms")>]
[<InlineData("millisecond")>]
[<InlineData("milliseconds")>]
[<InlineData("d")>]
[<InlineData("secs")>]
[<InlineData(" day")>]
[<InlineData("day ")>]
[<InlineData("dayofweek")>]
[<InlineData("dayss")>]
[<InlineData("quarter")>]
let ``JYRO-REQ-2127 DateAdd_WithAnUnknownUnit_RaisesDateUnitInvalid`` (unit: string) =
    expectAddError MessageCode.DateUnitInvalid "2024-01-01" 1 unit

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2127")>]
[<InlineData("Weeks!")>]
[<InlineData("WEEKS!")>]
[<InlineData("fortnight")>]
[<InlineData("Day ")>]
[<InlineData(" DAY")>]
[<InlineData("")>]
[<InlineData("MiLLiSeCoNd")>]
[<InlineData("weeK")>]
[<InlineData("WEEKs")>]
[<InlineData("ſecond")>]
[<InlineData("MİNUTE")>]
let ``JYRO-REQ-2127 DateAdd_DateUnitInvalid_HasTheUnitExactlyAsGivenAsItsOneArgument`` (unit: string) =
    let m = runtimeErrorIn [ "d", Values.str "2024-01-01"; "a", Values.int 1; "u", Values.str unit ] "Output.r = DateAdd(Input.d, Input.a, Input.u)"
    Assert.Equal(MessageCode.DateUnitInvalid, m.Code)
    Assert.False(isNull m.Args, "the diagnostic has no Args")
    Assert.Equal(1, m.Args.Length)
    Assert.Equal(unit, System.Convert.ToString(m.Args.[0], System.Globalization.CultureInfo.InvariantCulture))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2127")>]
let ``JYRO-REQ-2127 DateAdd_TheClausesExamples_WeeksBangAndKelvinSign`` () =
    for call in [ "DateAdd(\"2024-01-01\", 1, \"Weeks!\")"; "DateAdd(unit: \"Weeks!\", amount: 1, date: \"2024-01-01\")" ] do
        let m = runtimeErrorIn [] ("Output.r = " + call)
        Assert.Equal(MessageCode.DateUnitInvalid, m.Code)
        Assert.Equal<string list>([ "Weeks!" ], argStrings m)
    let kelvin = "wee" + string (char 0x212A)
    let m = runtimeErrorIn [ "u", Values.str kelvin ] "Output.r = DateAdd(\"2024-01-01\", 1, Input.u)"
    Assert.Equal(MessageCode.DateUnitInvalid, m.Code)
    Assert.Equal(1, m.Args.Length)
    let arg = System.Convert.ToString(m.Args.[0], System.Globalization.CultureInfo.InvariantCulture)
    Assert.Equal(4, arg.Length)
    Assert.Equal(0x212A, int arg.[3])
    Assert.NotEqual<string>("week", arg)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1747")>]
[<InlineData("2024-01-31", 1, "month", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2023-01-31", 1, "month", "2023-02-28T00:00:00.000Z")>]
[<InlineData("2024-03-31", -1, "months", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2024-03-31", -1, "month", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2024-05-31", 1, "month", "2024-06-30T00:00:00.000Z")>]
[<InlineData("2024-01-30", 1, "month", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2024-01-31", 13, "month", "2025-02-28T00:00:00.000Z")>]
[<InlineData("2024-01-31", -2, "months", "2023-11-30T00:00:00.000Z")>]
[<InlineData("2024-01-31", -11, "month", "2023-02-28T00:00:00.000Z")>]
[<InlineData("2024-01-31", -14, "months", "2022-11-30T00:00:00.000Z")>]
let ``JYRO-REQ-1747 DateAdd_Month_UsesTheLastDayWhenTheDayDoesNotExist`` (d: string, amount: int, unit: string, expected: string) =
    addsTo d amount unit expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1747")>]
[<InlineData("2024-01-15", 1, "month", "2024-02-15T00:00:00.000Z")>]
[<InlineData("2024-01-15", 1, "months", "2024-02-15T00:00:00.000Z")>]
[<InlineData("2024-01-31", 2, "month", "2024-03-31T00:00:00.000Z")>]
[<InlineData("2024-01-31", 2, "months", "2024-03-31T00:00:00.000Z")>]
[<InlineData("2024-03-15", -1, "month", "2024-02-15T00:00:00.000Z")>]
[<InlineData("2024-03-29", -1, "months", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2024-01-15", 13, "month", "2025-02-15T00:00:00.000Z")>]
[<InlineData("2024-01-15", -13, "months", "2022-12-15T00:00:00.000Z")>]
[<InlineData("2024-12-31", 1, "month", "2025-01-31T00:00:00.000Z")>]
[<InlineData("2024-01-01", -1, "month", "2023-12-01T00:00:00.000Z")>]
[<InlineData("2024-01-31", 12, "months", "2025-01-31T00:00:00.000Z")>]
let ``JYRO-REQ-1747 DateAdd_Month_KeepsTheDayWhereItExists`` (d: string, amount: int, unit: string, expected: string) =
    addsTo d amount unit expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1749")>]
[<InlineData(1, "2025-02-28T00:00:00.000Z")>]
[<InlineData(-1, "2023-02-28T00:00:00.000Z")>]
[<InlineData(4, "2028-02-29T00:00:00.000Z")>]
[<InlineData(-4, "2020-02-29T00:00:00.000Z")>]
[<InlineData(76, "2100-02-28T00:00:00.000Z")>]
[<InlineData(376, "2400-02-29T00:00:00.000Z")>]
[<InlineData(0, "2024-02-29T00:00:00.000Z")>]
let ``JYRO-REQ-1749 DateAdd_Year_Moves29FebruaryTo28InANonLeapYear`` (amount: int, expected: string) =
    addsTo "2024-02-29" amount "year" expected
    addsTo "2024-02-29" amount "years" expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1749")>]
[<InlineData("2023-03-15", -1, "year", "2022-03-15T00:00:00.000Z")>]
[<InlineData("2023-03-15", -1, "years", "2022-03-15T00:00:00.000Z")>]
[<InlineData("2024-12-31", 1, "year", "2025-12-31T00:00:00.000Z")>]
[<InlineData("2024-01-01", 3, "years", "2027-01-01T00:00:00.000Z")>]
[<InlineData("2024-02-28", 1, "year", "2025-02-28T00:00:00.000Z")>]
[<InlineData("2024-02-28", 4, "years", "2028-02-28T00:00:00.000Z")>]
[<InlineData("2023-02-28", 1, "year", "2024-02-28T00:00:00.000Z")>]
[<InlineData("2024-03-01", 1, "year", "2025-03-01T00:00:00.000Z")>]
[<InlineData("2000-02-29", 100, "years", "2100-02-28T00:00:00.000Z")>]
[<InlineData("2000-02-29", 400, "years", "2400-02-29T00:00:00.000Z")>]
[<InlineData("2000-02-29", 4, "years", "2004-02-29T00:00:00.000Z")>]
let ``JYRO-REQ-1749 DateAdd_Year_KeepsTheMonthAndDayOtherwise`` (d: string, amount: int, unit: string, expected: string) =
    addsTo d amount unit expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1039")>]
[<InlineData("2024-01-31T13:14:15.678Z", 1, "month", "2024-02-29T13:14:15.678Z")>]
[<InlineData("2024-02-29T23:59:59.999Z", 1, "year", "2025-02-28T23:59:59.999Z")>]
[<InlineData("2024-03-15T01:02:03.004Z", -2, "months", "2024-01-15T01:02:03.004Z")>]
[<InlineData("2024-06-30T12:00:00.500+02:00", 1, "years", "2025-06-30T10:00:00.500Z")>]
let ``DateAdd_MonthAndYear_KeepTheTimeOfDay`` (d: string, amount: int, unit: string, expected: string) =
    addsTo d amount unit expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1750")>]
[<InlineData("1.5")>]
[<InlineData("-0.5")>]
[<InlineData("1.0000000000000000000000000001")>]
[<InlineData("2147483647.5")>]
[<InlineData("-2147483648.5")>]
let ``JYRO-REQ-1750 DateAdd_WithANonIntegerAmount_RaisesIntegerRequired`` (amount: string) =
    expectExprError MessageCode.IntegerRequired (sprintf "DateAdd(\"2024-01-01\", %s, \"second\")" amount)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1750")>]
[<InlineData("day")>]
[<InlineData("month")>]
[<InlineData("years")>]
let ``JYRO-REQ-1750 DateAdd_WithANonIntegerAmount_RaisesIntegerRequired_ForEveryKindOfUnit`` (unit: string) =
    expectAddErrorAmount MessageCode.IntegerRequired "1.5" unit

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1750")>]
let ``JYRO-REQ-1750 DateAdd_WithAHostInexactNonIntegerAmount_RaisesIntegerRequired`` () =
    expectExprErrorIn [ "a", hostInexact 1.5 ] MessageCode.IntegerRequired "DateAdd(\"2024-01-01\", Input.a, \"second\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1750")>]
[<InlineData("2147483648")>]
[<InlineData("-2147483649")>]
[<InlineData("1e30")>]
[<InlineData("-1e30")>]
let ``JYRO-REQ-1750 DateAdd_WithAnAmountOutsideInt32_RaisesIntegerOutOfRange`` (amount: string) =
    expectExprError MessageCode.IntegerOutOfRange (sprintf "DateAdd(\"2024-01-01\", %s, \"second\")" amount)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1750")>]
[<InlineData("day")>]
[<InlineData("month")>]
[<InlineData("years")>]
let ``JYRO-REQ-1750 DateAdd_WithAnAmountOutsideInt32_RaisesIntegerOutOfRange_ForEveryKindOfUnit`` (unit: string) =
    expectAddErrorAmount MessageCode.IntegerOutOfRange "2147483648" unit
    expectAddErrorAmount MessageCode.IntegerOutOfRange "-2147483649" unit

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1750")>]
let ``JYRO-REQ-1750 DateAdd_WithIntegralAmounts_AtTheBoundAndInexact_IsAccepted`` () =
    assertString "2092-01-19T03:14:07.000Z" (eval "DateAdd(\"2024-01-01\", 2147483647, \"second\")")
    assertString "2024-01-01T00:00:02.000Z" (evalIn [ "a", hostInexact 2.0 ] "DateAdd(\"2024-01-01\", Input.a, \"second\")")
    assertString "2024-01-01T00:00:02.000Z" (eval "DateAdd(\"2024-01-01\", 2.0, \"second\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1750")>]
let ``JYRO-REQ-1750 DateAdd_WithTheLowestInt32Amount_IsNotRefusedForItsAmount`` () =
    // -2147483648 seconds from 2024-01-01 is 1955-12-13T20:45:52.000Z.
    assertString "1955-12-13T20:45:52.000Z" (eval "DateAdd(\"2024-01-01\", -2147483648, \"second\")")
    assertString "2024-01-01T00:00:00.000Z" (eval "DateAdd(\"2024-01-01\", 0.0, \"second\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1750")>]
let ``JYRO-REQ-1750 DateAdd_WithTheTwoDayAmount2Point0_ReturnsTheThirdOfJanuary`` () =
    assertString "2024-01-03T00:00:00.000Z" (eval "DateAdd(\"2024-01-01\", 2.0, \"day\")")
    assertString "2024-01-03T00:00:00.000Z" (evalIn [ "a", hostInexact 2.0 ] "DateAdd(\"2024-01-01\", Input.a, \"day\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1041")>]
[<InlineData("2024-03-01", -1, "day", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2024-01-01T00:00:30Z", -90, "seconds", "2023-12-31T23:59:00.000Z")>]
[<InlineData("2024-03-31", -1, "month", "2024-02-29T00:00:00.000Z")>]
[<InlineData("2024-01-01", -1, "week", "2023-12-25T00:00:00.000Z")>]
[<InlineData("2024-01-01", -1, "hour", "2023-12-31T23:00:00.000Z")>]
[<InlineData("2024-01-01", -1, "minute", "2023-12-31T23:59:00.000Z")>]
[<InlineData("2024-01-01", -2, "years", "2022-01-01T00:00:00.000Z")>]
let ``DateAdd_WithANegativeAmount_MovesBack`` (d: string, amount: int, unit: string, expected: string) =
    addsTo d amount unit expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1042")>]
let ``DateAdd_WithANonNumberAmount_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "\"1\""; "true"; "[]"; "{}" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateAdd(\"2024-01-01\", %s, \"day\")" lit)
    for v in [ Values.nul; Values.str "1"; Values.bool true ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DateAdd(\"2024-01-01\", Input.v, \"day\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1712")>]
[<InlineData("9999-12-31T23:59:59.999Z", 1, "second")>]
[<InlineData("9999-12-31T23:59:59.999Z", 1, "minute")>]
[<InlineData("9999-12-31", 1, "day")>]
[<InlineData("9999-01-01", 1, "year")>]
[<InlineData("9999-12-01", 1, "month")>]
[<InlineData("0001-01-01", -1, "day")>]
[<InlineData("0001-01-01T00:00:00.000Z", -1, "second")>]
[<InlineData("0001-01-31", -1, "month")>]
[<InlineData("2024-01-31", 2147483647, "month")>]
[<InlineData("2024-01-01", 2147483647, "week")>]
[<InlineData("2024-01-01", 2147483647, "day")>]
[<InlineData("2024-01-01", 2147483647, "hour")>]
[<InlineData("2024-01-01", -2147483648, "year")>]
[<InlineData("2024-01-01", -2147483648, "month")>]
let ``JYRO-REQ-1712 DateAdd_WhenTheResultIsOutOfRange_RaisesDateOutOfRange`` (d: string, amount: int, unit: string) =
    expectAddError MessageCode.DateOutOfRange d amount unit

/// The arguments of a `DateOutOfRange` from `DateAdd(Input.d, <amountLiteral>, Input.u)`, as strings.
let private outOfRangeArgs (d: string) (amountLiteral: string) (unit: string) : string list =
    let m = runtimeErrorIn [ "d", Values.str d; "u", Values.str unit ] ("Output.r = DateAdd(Input.d, " + amountLiteral + ", Input.u)")
    Assert.Equal(MessageCode.DateOutOfRange, m.Code)
    Assert.False(isNull m.Args, "the diagnostic has no Args")
    Assert.Equal(4, m.Args.Length)
    // The .NET type of an element is the implementation's (JYRO-REQ-2110): read each as its invariant string.
    argStrings m

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1712")>]
[<InlineData("9999-12-31", "1", "DAYS", "1")>]
[<InlineData("0001-01-01", "-1", "Month", "-1")>]
[<InlineData("9999-12-31", "1", "day", "1")>]
[<InlineData("9999-12-31T23:59:59.999Z", "1", "Second", "1")>]
[<InlineData("9999-12-31T23:59:59.999+00:00", "1", "MINUTE", "1")>]
[<InlineData("0001-01-01T00:00:00.000Z", "-1", "sEcOnDs", "-1")>]
[<InlineData("2024-01-01", "2147483647", "HOURS", "2147483647")>]
[<InlineData("2024-01-01", "-2147483648", "Years", "-2147483648")>]
[<InlineData("2024-01-01", "2147483647", "week", "2147483647")>]
[<InlineData("9999-12-31", "2.0", "Day", "2")>]
let ``JYRO-REQ-1712 DateAdd_DateOutOfRange_HasTheFourArgumentsNameAmountUnitAndDateAsGiven`` (d: string, amountLiteral: string, unit: string, amountText: string) =
    Assert.Equal<string list>([ "DateAdd()"; amountText; unit; d ], outOfRangeArgs d amountLiteral unit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1712")>]
let ``JYRO-REQ-1712 DateAdd_DateOutOfRange_TheClausesExamples_ByPositionAndByName`` () =
    for call in [ "DateAdd(\"9999-12-31\", 1, \"DAYS\")"; "DateAdd(unit: \"DAYS\", amount: 1, date: \"9999-12-31\")" ] do
        let m = runtimeErrorIn [] ("Output.r = " + call)
        Assert.Equal(MessageCode.DateOutOfRange, m.Code)
        Assert.Equal<string list>([ "DateAdd()"; "1"; "DAYS"; "9999-12-31" ], argStrings m)
    for call in [ "DateAdd(\"0001-01-01\", -1, \"Month\")"; "DateAdd(amount: -1, date: \"0001-01-01\", unit: \"Month\")" ] do
        let m = runtimeErrorIn [] ("Output.r = " + call)
        Assert.Equal(MessageCode.DateOutOfRange, m.Code)
        Assert.Equal<string list>([ "DateAdd()"; "-1"; "Month"; "0001-01-01" ], argStrings m)

// The converse of 94.5.13's condition: a result at a bound is not before or after it, so it
// raises nothing. No requirement states it apart from JYRO-REQ-1712's own condition.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1712")>]
[<InlineData("9999-12-31T23:59:58.999Z", 1, "second", "9999-12-31T23:59:59.999Z")>]
[<InlineData("0001-01-02", -1, "day", "0001-01-01T00:00:00.000Z")>]
[<InlineData("9998-12-31T23:59:59.999Z", 1, "year", "9999-12-31T23:59:59.999Z")>]
let ``JYRO-REQ-1712 DateAdd_WhenTheResultIsAtTheBound_ReturnsIt`` (d: string, amount: int, unit: string, expected: string) =
    addsTo d amount unit expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1044")>]
[<InlineData("2024-02-30")>]
[<InlineData("bad")>]
[<InlineData("")>]
[<InlineData("2024-1-5")>]
let ``JYRO-REQ-1044 DateAdd_WithAnUnreadableDate_RaisesDateFormatInvalid`` (d: string) =
    expectAddError MessageCode.DateFormatInvalid d 1 "day"

/// 90.3.8 step 1: `date` goes through its type check and then whether it can be read (94.5.14,
/// a check of `date` alone, row 5 of 90.3.9) before any check of `amount` (type,
/// `IntegerRequired`, `IntegerOutOfRange`) or of `unit` (type, `DateUnitInvalid`).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1044")>]
[<InlineData("1.5", "\"days\"")>]
[<InlineData("-0.5", "\"day\"")>]
[<InlineData("3000000000", "\"day\"")>]
[<InlineData("1e30", "\"day\"")>]
[<InlineData("\"1\"", "\"day\"")>]
[<InlineData("null", "\"day\"")>]
[<InlineData("true", "\"day\"")>]
[<InlineData("1", "5")>]
[<InlineData("1", "null")>]
[<InlineData("1", "\"fortnight\"")>]
[<InlineData("1.5", "5")>]
let ``JYRO-REQ-1044 JYRO-DEF-648 DateAdd_WithAnUnreadableDate_RaisesDateFormatInvalid_BeforeAnyCheckOfAmountOrUnit`` (amount: string, unit: string) =
    expectExprError MessageCode.DateFormatInvalid (sprintf "DateAdd(\"not a date\", %s, %s)" amount unit)
    expectExprError MessageCode.DateFormatInvalid (sprintf "DateAdd(unit: %s, amount: %s, date: \"2024-02-30\")" unit amount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1045")>]
let ``JYRO-REQ-1045 JYRO-DEF-648 DateAdd_WithADateOfTheWrongType_RaisesArgumentTypeMismatch_BeforeAnyCheckOfAmountOrUnit`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateAdd(%s, 1.5, \"fortnight\")" lit)
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateAdd(%s, \"1\", 5)" lit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1045")>]
let ``JYRO-REQ-1045 DateAdd_WithANullNumberOrBooleanDateOrUnit_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateAdd(%s, 1, \"day\")" lit)
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateAdd(\"2024-01-01\", 1, %s)" lit)
    for v in [ Values.nul; Values.exact 5m; Values.bool false ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DateAdd(Input.v, 1, \"day\")"
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DateAdd(\"2024-01-01\", 1, Input.v)"

// ================================================================= 94.6 DateDiff

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2030")>]
let ``DateDiff_Arity_IsThreeArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = DateDiff(\"2024-01-01\", \"2024-01-02\")"
    expectCompileError MessageCode.TooManyArguments "Output.r = DateDiff(\"2024-01-01\", \"2024-01-02\", \"day\", 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2030")>]
let ``DateDiff_ParametersAreStartDateEndDateUnit_AndReturnsANumber`` () =
    // The first positional argument is startDate and the second endDate (JYRO-REQ-1916).
    assertNumber 1.0 (eval "DateDiff(\"2024-01-01\", \"2024-01-02\", \"day\")")
    assertNumber -1.0 (eval "DateDiff(\"2024-01-02\", \"2024-01-01\", \"day\")")
    assertNumber 1.0 (eval "DateDiff(endDate: \"2024-01-02\", startDate: \"2024-01-01\", unit: \"day\")")
    assertNumber 1.0 (eval "DateDiff(unit: \"day\", startDate: \"2024-01-01\", endDate: \"2024-01-02\")")
    assertNumber -1.0 (eval "DateDiff(startDate: \"2024-01-02\", endDate: \"2024-01-01\", unit: \"day\")")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = DateDiff(end: \"2024-01-02\", startDate: \"2024-01-01\", unit: \"day\")"
    assertString "number" (eval "TypeOf(DateDiff(\"2024-01-01\", \"2024-01-02\", \"day\"))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2031")>]
let ``JYRO-REQ-2031 DateDiff_ReadsBothDatesAsParseDateReadsText`` () =
    for d in readableForms do
        assertTrue (evalIn [ "d", Values.str d ] "DateDiff(Input.d, ParseDate(Input.d), \"second\") == 0")
        assertTrue (evalIn [ "d", Values.str d ] "DateDiff(ParseDate(Input.d), Input.d, \"second\") == 0")
    assertNumber 0.0 (diff "2024-01-01T02:00:00+02:00" "2024-01-01" "hour")
    // 10:20:60.5 is 10:20:59.500 (94.3.9), so 0.5 s before 10:21:00.
    assertNumber 0.5 (diff "2024-01-15T10:21:00Z" "2024-01-15T10:20:60.5Z" "second")
    // An offset is subtracted (94.3.4) from both dates.
    assertNumber 1.0 (diff "2024-01-01T00:00:00-01:00" "2024-01-01T00:00:00+00:00" "hour")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2031")>]
[<InlineData("2024-01-02 03:04:05Z", "2024-01-01")>]
[<InlineData("2024-01-01", "2024-1-5")>]
[<InlineData("2024-01-01", "2024-01-02T03:04Z")>]
let ``JYRO-REQ-2031 DateDiff_RefusesWhatParseDateRefuses`` (e: string, s: string) =
    expectDiffError MessageCode.DateFormatInvalid e s "day"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2145")>]
let ``JYRO-REQ-2145 DateDiff_IsPositiveNegativeOrZero_ForEveryUnit`` () =
    for u in allUnits do
        Assert.True(Values.asFloat (diff "2026-03-01" "2024-01-01" u) > 0.0, sprintf "%s: later minus earlier must be positive" u)
        Assert.True(Values.asFloat (diff "2024-01-01" "2026-03-01" u) < 0.0, sprintf "%s: earlier minus later must be negative" u)
        assertNumber 0.0 (diff "2024-01-01" "2024-01-01" u)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2145")>]
let ``JYRO-REQ-2145 DateDiff_OfTheSameInstantInDifferentForms_IsZero_ForEveryUnit`` () =
    for u in allUnits do
        assertNumber 0.0 (diff "2024-01-01T02:00:00+02:00" "2024-01-01" u)
        assertNumber 0.0 (diff "2024-01-01" "2023-12-31T23:00:00-01:00" u)
        assertNumber 0.0 (diff "2024-01-01T00:00:00.000Z" "2024-01-01T00:00:00Z" u)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2145")>]
let ``JYRO-REQ-2145 DateDiff_IsNeverNegativeWhenEndIsLater_AndNeverPositiveWhenEndIsEarlier_ForEveryUnit`` () =
    // From one millisecond apart to many years apart.
    let starts = [ "2024-01-01T00:00:00.000Z"; "2024-02-29T12:00:00.000Z"; "2023-12-31T23:59:59.999Z" ]
    let offsets = [ "2024-01-01T00:00:00.001Z"; "2024-01-01T00:00:01Z"; "2024-01-01T01:00:00Z"; "2024-01-02T00:00:00Z"
                    "2024-01-08T00:00:00Z"; "2024-01-15"; "2024-02-01"; "2024-12-31"; "2025-01-01"; "2030-06-15T12:34:56Z" ]
    for u in allUnits do
        for s in starts do
            for e in offsets do
                if instantOf e > instantOf s then
                    Assert.True(Values.asFloat (diff e s u) >= 0.0, sprintf "%s: DateDiff(%s, %s) is negative" u e s)
                    Assert.True(Values.asFloat (diff s e u) <= 0.0, sprintf "%s: DateDiff(%s, %s) is positive" u s e)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2145")>]
[<InlineData("second")>]
[<InlineData("seconds")>]
[<InlineData("minute")>]
[<InlineData("minutes")>]
[<InlineData("hour")>]
[<InlineData("hours")>]
[<InlineData("day")>]
[<InlineData("days")>]
[<InlineData("week")>]
[<InlineData("weeks")>]
let ``JYRO-REQ-2145 DateDiff_ForFixedUnits_IsZeroOnlyForTheSameInstant`` (unit: string) =
    // One millisecond apart is not zero, in either direction.
    let later = "2024-01-01T00:00:00.001Z"
    let earlier = "2024-01-01T00:00:00.000Z"
    Assert.True(Values.asFloat (diff later earlier unit) > 0.0, sprintf "%s: one millisecond later gave a result that is not positive" unit)
    Assert.True(Values.asFloat (diff earlier later unit) < 0.0, sprintf "%s: one millisecond earlier gave a result that is not negative" unit)
    assertNumber 0.0 (diff earlier earlier unit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2145")>]
let ``JYRO-REQ-2145 DateDiff_TheClausesExamples`` () =
    assertNumber 1.0 (diff "2024-01-02" "2024-01-01" "day")
    assertNumber -1.0 (diff "2024-01-01" "2024-01-02" "days")
    assertNumber 0.0 (diff "2024-01-15" "2024-01-01" "month")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2145")>]
[<InlineData("2024-01-15", "2024-01-01", "month")>]
[<InlineData("2024-01-01", "2024-01-15", "months")>]
[<InlineData("2024-12-31", "2024-01-01", "year")>]
[<InlineData("2024-01-01", "2024-12-31", "years")>]
[<InlineData("2024-01-31T23:59:59.999Z", "2024-01-01", "month")>]
[<InlineData("2024-01-01", "2024-01-31T23:59:59.999Z", "month")>]
let ``JYRO-REQ-2145 DateDiff_OfDatesLessThanOneMonthOrYearApart_IsZeroNotSignedWrongly`` (e: string, s: string, unit: string) =
    let v = Values.asFloat (diff e s unit)
    Assert.True((v = 0.0), sprintf "%s: DateDiff(%s, %s) is %g, not 0" unit e s v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1049")>]
[<InlineData("2024-01-02", "day", 1.0)>]
[<InlineData("2024-01-02", "hour", 24.0)>]
[<InlineData("2024-01-02", "minute", 1440.0)>]
[<InlineData("2024-01-02", "second", 86400.0)>]
[<InlineData("2024-01-15", "week", 2.0)>]
[<InlineData("2024-01-01T12:00:00Z", "day", 0.5)>]
[<InlineData("2024-01-01T00:00:01.500Z", "second", 1.5)>]
[<InlineData("2024-01-01T00:00:00.001Z", "second", 0.001)>]
[<InlineData("2024-01-01T00:30:00Z", "hour", 0.5)>]
let ``DateDiff_ForFixedUnits_ReturnsElapsedTimeWithTheFractionKept`` (e: string, unit: string, expected: float) =
    assertNumber expected (diff e "2024-01-01" unit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1049")>]
let ``DateDiff_ForFixedUnits_KeepsTheFractionInBothDirections`` () =
    assertNumber (1.0 / 7.0) (diff "2024-01-02" "2024-01-01" "week")
    assertNumber -0.5 (diff "2024-01-01" "2024-01-01T12:00:00Z" "day")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1050")>]
[<InlineData("2024-03-31", "2024-01-31", "month", 2.0)>]
[<InlineData("2024-02-29", "2024-01-31", "month", 1.0)>]
[<InlineData("2024-02-28", "2024-01-31", "month", 0.0)>]
[<InlineData("2024-01-31", "2024-03-31", "month", -2.0)>]
[<InlineData("2024-02-29", "2024-03-31", "month", -1.0)>]
[<InlineData("2024-02-01T00:00:00Z", "2024-01-01T12:00:00Z", "month", 0.0)>]
[<InlineData("2024-02-01T12:00:00Z", "2024-01-01T12:00:00Z", "month", 1.0)>]
[<InlineData("2025-02-28", "2024-02-29", "year", 1.0)>]
[<InlineData("2025-02-27", "2024-02-29", "year", 0.0)>]
[<InlineData("2024-02-29", "2025-02-28", "year", 0.0)>]
[<InlineData("2034-01-01", "2024-01-01", "year", 10.0)>]
[<InlineData("2033-12-31T23:59:59.999Z", "2024-01-01", "year", 9.0)>]
[<InlineData("2014-01-01", "2024-01-01", "years", -10.0)>]
[<InlineData("2024-04-30", "2024-01-31", "months", 3.0)>]
[<InlineData("2024-04-29", "2024-01-31", "month", 2.0)>]
[<InlineData("2024-01-01", "2024-01-01", "month", 0.0)>]
[<InlineData("2025-01-01", "2024-01-01", "month", 12.0)>]
[<InlineData("2024-12-31T23:59:59.999Z", "2024-01-01", "month", 11.0)>]
[<InlineData("2023-12-31T23:59:59.999Z", "2024-01-01", "month", 0.0)>]
[<InlineData("2023-12-01", "2024-01-01", "month", -1.0)>]
[<InlineData("2024-02-29", "2023-02-28", "year", 1.0)>]
[<InlineData("2024-02-28", "2025-02-28", "year", -1.0)>]
let ``JYRO-REQ-1050 DateDiff_ForMonthAndYear_ReturnsTheWholeCountThatDoesNotPassEndDate`` (e: string, s: string, unit: string, expected: float) =
    assertNumber expected (diff e s unit)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2128")>]
[<InlineData("fortnight")>]
[<InlineData("")>]
[<InlineData("ms")>]
[<InlineData("millisecond")>]
[<InlineData("dayofyear")>]
[<InlineData(" day")>]
[<InlineData("day ")>]
let ``JYRO-REQ-2128 DateDiff_WithAnUnknownUnit_RaisesDateUnitInvalid`` (unit: string) =
    expectDiffError MessageCode.DateUnitInvalid "2024-01-02" "2024-01-01" unit

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2128")>]
[<InlineData("Weeks!")>]
[<InlineData("WEEKS!")>]
[<InlineData("fortnight")>]
[<InlineData("Day ")>]
[<InlineData(" DAY")>]
[<InlineData("")>]
[<InlineData("MiLLiSeCoNd")>]
[<InlineData("weeK")>]
[<InlineData("WEEKs")>]
[<InlineData("ſecond")>]
[<InlineData("MİNUTE")>]
let ``JYRO-REQ-2128 DateDiff_DateUnitInvalid_HasTheUnitExactlyAsGivenAsItsOneArgument`` (unit: string) =
    let m = runtimeErrorIn [ "e", Values.str "2024-01-02"; "s", Values.str "2024-01-01"; "u", Values.str unit ] "Output.r = DateDiff(Input.s, Input.e, Input.u)"
    Assert.Equal(MessageCode.DateUnitInvalid, m.Code)
    Assert.False(isNull m.Args, "the diagnostic has no Args")
    Assert.Equal(1, m.Args.Length)
    Assert.Equal(unit, System.Convert.ToString(m.Args.[0], System.Globalization.CultureInfo.InvariantCulture))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2128")>]
let ``JYRO-REQ-2128 DateDiff_TheClausesExamples_WeeksBangAndKelvinSign`` () =
    for call in [ "DateDiff(\"2024-01-01\", \"2024-01-02\", \"Weeks!\")"
                  "DateDiff(unit: \"Weeks!\", startDate: \"2024-01-01\", endDate: \"2024-01-02\")" ] do
        let m = runtimeErrorIn [] ("Output.r = " + call)
        Assert.Equal(MessageCode.DateUnitInvalid, m.Code)
        Assert.Equal<string list>([ "Weeks!" ], argStrings m)
    let kelvin = "wee" + string (char 0x212A)
    let m = runtimeErrorIn [ "u", Values.str kelvin ] "Output.r = DateDiff(\"2024-01-01\", \"2024-01-02\", Input.u)"
    Assert.Equal(MessageCode.DateUnitInvalid, m.Code)
    Assert.Equal(1, m.Args.Length)
    let arg = System.Convert.ToString(m.Args.[0], System.Globalization.CultureInfo.InvariantCulture)
    Assert.Equal(4, arg.Length)
    Assert.Equal(0x212A, int arg.[3])
    Assert.NotEqual<string>("week", arg)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1053")>]
[<InlineData("2024-01-02", "day", "1")>]
[<InlineData("2024-01-01T00:00:01.500Z", "second", "1.5")>]
[<InlineData("2024-01-01T00:00:01.500Z", "minute", "0.025")>]
[<InlineData("2024-01-01T00:00:00.001Z", "second", "0.001")>]
[<InlineData("2024-03-01", "month", "2")>]
[<InlineData("2034-01-01", "year", "10")>]
[<InlineData("2024-01-01T06:00:00Z", "day", "0.25")>]
[<InlineData("2023-12-31T23:59:59.999Z", "second", "-0.001")>]
[<InlineData("2024-01-01T00:00:00.000Z", "second", "0")>]
[<InlineData("2024-01-01T00:00:00.125Z", "second", "0.125")>]
[<InlineData("2024-01-01T00:00:00.375Z", "minute", "0.00625")>]
let ``JYRO-REQ-1053 DateDiff_WhenTheExactRepresentationHoldsTheResult_IsExact`` (e: string, unit: string, expected: string) =
    assertExact (dec expected) (diff e "2024-01-01" unit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1053")>]
let ``JYRO-REQ-1053 DateDiff_OverTheWholeRange_InSecondsAndMonths_IsExact`` () =
    // 0001-01-01 to 10000-01-01 is 3652059 days (9999 years, 2424 of them leap years:
    // 2499 multiples of 4, less 99 of 100, plus 24 of 400), so 315537897600 s; one millisecond
    // less is 315537897599.999 s, which m / 10^3 holds.
    assertExact (dec "315537897599.999") (diff "9999-12-31T23:59:59.999Z" "0001-01-01" "second")
    assertExact (dec "-315537897599.999") (diff "0001-01-01" "9999-12-31T23:59:59.999Z" "second")
    // 9998 years and 11 months, as whole months: 9998 * 12 + 11.
    assertExact 119987m (diff "9999-12-31T23:59:59.999Z" "0001-01-01" "month")
    assertExact 9998m (diff "9999-12-31T23:59:59.999Z" "0001-01-01" "year")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1053")>]
[<InlineData("2024-01-02", "week", 7.0)>]
[<InlineData("2024-01-01T08:00:00Z", "day", 3.0)>]
[<InlineData("2024-01-01T00:00:00.001Z", "hour", 3600000.0)>]
[<InlineData("2024-01-01T00:00:00.001Z", "day", 86400000.0)>]
[<InlineData("2024-01-01T00:00:00.001Z", "minute", 60000.0)>]
let ``JYRO-REQ-1053 DateDiff_WhenTheExactRepresentationCannotHoldTheResult_IsInexact`` (e: string, unit: string, divisor: float) =
    // 1/7, 1/3, 1/3600000, 1/86400000 and 1/60000 have a factor 3 or 7 in the denominator: no m / 10^s
    // holds them (30.2.4). The value is 94.6.4's elapsed time over the duration, to within
    // the rounding of a double.
    assertInexactNear (1.0 / divisor) 1e-15 (diff e "2024-01-01" unit)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1054")>]
[<InlineData("2024-01-01T00:00:00.0019Z", "2024-01-01T00:00:00.0001Z", 0.001)>]
[<InlineData("2024-01-01T00:00:00.9999Z", "2024-01-01T00:00:00Z", 0.999)>]
[<InlineData("2024-01-01T00:00:00.1239Z", "2024-01-01T00:00:00.123Z", 0.0)>]
[<InlineData("2024-01-01T00:00:00Z", "2024-01-01T00:00:00.0009Z", 0.0)>]
[<InlineData("2024-01-01T00:00:00.0019Z", "2024-01-01T00:00:00.0011Z", 0.0)>]
[<InlineData("2024-01-01T00:00:01.0009Z", "2024-01-01T00:00:00.9999Z", 0.001)>]
let ``JYRO-REQ-1054 DateDiff_UsesTheMillisecondValuesOfTheDates`` (e: string, s: string, expected: float) =
    assertNumber expected (diff e s "second")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2032")>]
[<InlineData("bad", "2024-01-01")>]
[<InlineData("2024-01-01", "2024-02-30")>]
[<InlineData("", "2024-01-01")>]
[<InlineData("2024-01-01", "")>]
let ``DateDiff_WithAnUnreadableDate_RaisesDateFormatInvalid`` (e: string, s: string) =
    expectDiffError MessageCode.DateFormatInvalid e s "day"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2055")>]
[<InlineData("bad1", "bad2", "day")>]
[<InlineData("2024-02-30", "", "month")>]
[<InlineData("2026-02-30", "2025-02-29", "day")>]
[<InlineData("x", "y", "day")>]
[<InlineData("", "", "year")>]
let ``JYRO-REQ-2055 DateDiff_WithBothDatesUnreadable_NamesStartDate`` (startDate: string, endDate: string, unit: string) =
    let m = runtimeErrorIn [ "s", Values.str startDate; "e", Values.str endDate; "u", Values.str unit ] "Output.r = DateDiff(Input.s, Input.e, Input.u)"
    Assert.Equal(MessageCode.DateFormatInvalid, m.Code)
    // 130.6.2: `startDate`, spelled and cased as the signature gives, is one whole argument.
    Assert.Contains("startDate", argStrings m)
    Assert.DoesNotContain("endDate", argStrings m)

/// 90.3.8 step 1: `startDate`, then `endDate`, each through its type check and then whether it
/// can be read (a check of that argument alone, row 5 of 90.3.9), before any check of a later
/// parameter. So an unreadable `startDate` gives `DateFormatInvalid` whatever `endDate` and
/// `unit` are (naming `startDate` when `endDate` cannot be read either, 94.6.11), and an
/// unreadable `endDate` gives it before any check of `unit`.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2032")>]
[<InlineData("\"2024-01-01\"", "5")>]
[<InlineData("\"2024-01-01\"", "\"fortnight\"")>]
[<InlineData("\"bad\"", "null")>]
[<InlineData("5", "\"day\"")>]
[<InlineData("null", "true")>]
[<InlineData("[1]", "\"day\"")>]
let ``JYRO-REQ-2032 JYRO-DEF-648 DateDiff_WithAnUnreadableStartDate_RaisesDateFormatInvalid_BeforeAnyCheckOfEndDateOrUnit`` (endDate: string, unit: string) =
    for call in [ sprintf "DateDiff(\"not a date\", %s, %s)" endDate unit
                  sprintf "DateDiff(unit: %s, endDate: %s, startDate: \"2024-02-30\")" unit endDate ] do
        let m = runtimeErrorIn [] ("Output.r = " + call)
        if m.Code <> MessageCode.DateFormatInvalid then
            Values.fail (sprintf "Expected DateFormatInvalid from %s, got %A: %s" call m.Code m.Message)
        if endDate = "\"bad\"" then
            Assert.Contains("startDate", argStrings m)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2032")>]
[<InlineData("5")>]
[<InlineData("null")>]
[<InlineData("[1]")>]
[<InlineData("\"fortnight\"")>]
let ``JYRO-REQ-2032 JYRO-DEF-648 DateDiff_WithAnUnreadableEndDate_RaisesDateFormatInvalid_BeforeAnyCheckOfUnit`` (unit: string) =
    expectExprError MessageCode.DateFormatInvalid (sprintf "DateDiff(\"2024-01-01\", \"not a date\", %s)" unit)
    expectExprError MessageCode.DateFormatInvalid (sprintf "DateDiff(unit: %s, endDate: \"2024-02-30\", startDate: \"2024-01-01\")" unit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2033")>]
let ``JYRO-REQ-2033 JYRO-DEF-648 DateDiff_ChecksTheTypeOfEachArgumentInParameterOrder`` () =
    // startDate's type before endDate's readability, endDate's type before unit's.
    expectExprError MessageCode.ArgumentTypeMismatch "DateDiff(5, \"not a date\", \"fortnight\")"
    expectExprError MessageCode.ArgumentTypeMismatch "DateDiff(\"2024-01-01\", null, 5)"
    expectExprError MessageCode.ArgumentTypeMismatch "DateDiff(\"2024-01-01\", true, \"fortnight\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2033")>]
let ``JYRO-REQ-2033 DateDiff_WithANullNumberOrBooleanArgument_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateDiff(%s, \"2024-01-01\", \"day\")" lit)
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateDiff(\"2024-01-02\", %s, \"day\")" lit)
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DateDiff(\"2024-01-02\", \"2024-01-01\", %s)" lit)
    for v in [ Values.nul; Values.exact 5m; Values.bool false ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DateDiff(Input.v, \"2024-01-01\", \"day\")"
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DateDiff(\"2024-01-02\", Input.v, \"day\")"
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DateDiff(\"2024-01-02\", \"2024-01-01\", Input.v)"

// ================================================================= 94.7 DatePart

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1058")>]
let ``DatePart_Arity_IsTwoArguments`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = DatePart(\"2024-01-01\")"
    expectCompileError MessageCode.TooManyArguments "Output.r = DatePart(\"2024-01-01\", \"year\", 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1058")>]
let ``DatePart_ParametersAreDateAndPart_AndReturnsANumber`` () =
    assertNumber 2024.0 (eval "DatePart(date: \"2024-01-01\", part: \"year\")")
    assertNumber 2024.0 (eval "DatePart(part: \"year\", date: \"2024-01-01\")")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = DatePart(d: \"2024-01-01\", part: \"year\")"
    assertString "number" (eval "TypeOf(DatePart(\"2024-01-01\", \"year\"))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1059")>]
[<InlineData("2024-01-01T01:00:00+02:00", "day", 31.0)>]
[<InlineData("2024-01-01T01:00:00+02:00", "year", 2023.0)>]
[<InlineData("2024-01-01T01:00:00+02:00", "month", 12.0)>]
[<InlineData("2024-01-15t10:20:30z", "hour", 10.0)>]
[<InlineData("2024-01-15T10:20:60Z", "second", 59.0)>]
[<InlineData("2024-01-15T10:20:60Z", "minute", 20.0)>]
[<InlineData("2024-01-15", "hour", 0.0)>]
[<InlineData("2024-01-15T10:20:30", "hour", 10.0)>]
[<InlineData("2024-12-31T23:30:00-01:00", "year", 2025.0)>]
[<InlineData("2024-12-31T23:30:00-01:00", "dayofyear", 1.0)>]
[<InlineData("2024-01-15T10:20:30.9999Z", "second", 30.0)>]
let ``JYRO-REQ-1059 DatePart_ReadsDateAsParseDateReadsText`` (d: string, p: string, expected: float) =
    assertNumber expected (part d p)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1059")>]
[<InlineData("2024-02-30")>]
[<InlineData("2024-01-02 03:04:05Z")>]
[<InlineData("2024-01-02T03:04Z")>]
[<InlineData("2024/01/02")>]
let ``JYRO-REQ-1059 DatePart_RefusesWhatParseDateRefuses`` (d: string) =
    expectPartError MessageCode.DateFormatInvalid d "year"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1060")>]
[<InlineData("2024-03-05T07:08:09.012Z", "year", 2024.0)>]
[<InlineData("2024-03-05T07:08:09.012Z", "month", 3.0)>]
[<InlineData("2024-03-05T07:08:09.012Z", "day", 5.0)>]
[<InlineData("2024-03-05T07:08:09.012Z", "hour", 7.0)>]
[<InlineData("2024-03-05T07:08:09.012Z", "minute", 8.0)>]
[<InlineData("2024-03-05T07:08:09.012Z", "second", 9.0)>]
[<InlineData("2024-03-05T07:08:09.012Z", "dayofyear", 65.0)>]
[<InlineData("0001-01-01", "year", 1.0)>]
[<InlineData("0001-01-01", "month", 1.0)>]
[<InlineData("0001-01-01", "day", 1.0)>]
[<InlineData("0001-01-01", "hour", 0.0)>]
[<InlineData("0001-01-01", "minute", 0.0)>]
[<InlineData("0001-01-01", "second", 0.0)>]
[<InlineData("0001-01-01", "dayofyear", 1.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "year", 9999.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "month", 12.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "day", 31.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "hour", 23.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "minute", 59.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "second", 59.0)>]
[<InlineData("9999-12-31T23:59:59.999Z", "dayofyear", 365.0)>]
[<InlineData("2024-12-31", "dayofyear", 366.0)>]
[<InlineData("2023-12-31", "dayofyear", 365.0)>]
let ``DatePart_ReturnsTheTabledValue`` (d: string, p: string, expected: float) =
    assertNumber expected (part d p)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1061")>]
[<InlineData("2024-01-07", 0.0)>]
[<InlineData("2024-01-08", 1.0)>]
[<InlineData("2024-01-09", 2.0)>]
[<InlineData("2024-01-10", 3.0)>]
[<InlineData("2024-01-11", 4.0)>]
[<InlineData("2024-01-12", 5.0)>]
[<InlineData("2024-01-13", 6.0)>]
[<InlineData("0001-01-01", 1.0)>]
[<InlineData("9999-12-31", 5.0)>]
[<InlineData("2024-01-08T01:00:00+02:00", 0.0)>]
let ``DatePart_DayOfWeek_IsZeroForSunday`` (d: string, expected: float) =
    assertNumber expected (part d "dayofweek")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2129")>]
[<InlineData("millisecond")>]
[<InlineData("week")>]
[<InlineData("quarter")>]
[<InlineData("")>]
[<InlineData("days")>]
[<InlineData("years")>]
[<InlineData("dow")>]
[<InlineData(" year")>]
[<InlineData("year ")>]
let ``JYRO-REQ-2129 DatePart_WithAnUnknownPart_RaisesDatePartInvalid`` (p: string) =
    expectPartError MessageCode.DatePartInvalid "2024-03-05" p

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2129")>]
[<InlineData("Year!")>]
[<InlineData("YEAR!")>]
[<InlineData("Weeks")>]
[<InlineData("DAYS")>]
[<InlineData(" Year")>]
[<InlineData("")>]
[<InlineData("MiLLiSeCoNd")>]
[<InlineData("dayofweeK")>]
[<InlineData("DAYOFWEEK")>]
[<InlineData("ſecond")>]
[<InlineData("MİNUTE")>]
let ``JYRO-REQ-2129 DatePart_DatePartInvalid_HasThePartExactlyAsGivenAsItsOneArgument`` (p: string) =
    let m = runtimeErrorIn [ "d", Values.str "2024-03-05"; "p", Values.str p ] "Output.r = DatePart(Input.d, Input.p)"
    Assert.Equal(MessageCode.DatePartInvalid, m.Code)
    Assert.False(isNull m.Args, "the diagnostic has no Args")
    Assert.Equal(1, m.Args.Length)
    Assert.Equal(p, System.Convert.ToString(m.Args.[0], System.Globalization.CultureInfo.InvariantCulture))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2129")>]
let ``JYRO-REQ-2129 DatePart_TheClausesExamples_YearBangAndKelvinSign`` () =
    for call in [ "DatePart(\"2024-01-01\", \"Year!\")"; "DatePart(part: \"Year!\", date: \"2024-01-01\")" ] do
        let m = runtimeErrorIn [] ("Output.r = " + call)
        Assert.Equal(MessageCode.DatePartInvalid, m.Code)
        Assert.Equal<string list>([ "Year!" ], argStrings m)
    let kelvin = "dayofwee" + string (char 0x212A)
    let m = runtimeErrorIn [ "p", Values.str kelvin ] "Output.r = DatePart(\"2024-01-01\", Input.p)"
    Assert.Equal(MessageCode.DatePartInvalid, m.Code)
    Assert.Equal(1, m.Args.Length)
    let arg = System.Convert.ToString(m.Args.[0], System.Globalization.CultureInfo.InvariantCulture)
    Assert.Equal(9, arg.Length)
    Assert.Equal(0x212A, int arg.[8])
    Assert.NotEqual<string>("dayofweek", arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1064")>]
let ``JYRO-REQ-1064 DatePart_ReturnsAnExactInteger`` () =
    let parts = [ "year"; "month"; "day"; "hour"; "minute"; "second"; "dayofweek"; "dayofyear" ]
    for d in [ "2024-03-05T07:08:09.012Z"; "0001-01-01"; "9999-12-31T23:59:60.999Z"; "2024-01-07" ] do
        for p in parts do
            let v = part d p
            Assert.True(Values.isExact v, sprintf "DatePart(%s, %s) must be exact" d p)
            Assert.True((v :?> JyroNumber).IsInteger, sprintf "DatePart(%s, %s) must be an integer" d p)
    // The exact 5 times the exact 0.1 is the exact 0.5 (30.2.9).
    assertExact 0.5m (eval "DatePart(\"2024-03-05\", \"day\") * 0.1")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1065")>]
[<InlineData("2024-02-30")>]
[<InlineData("bad")>]
[<InlineData("")>]
[<InlineData("2024-1-5")>]
[<InlineData("0000-12-31T23:59:59Z")>]
let ``JYRO-REQ-1065 DatePart_WithAnUnreadableDate_RaisesDateFormatInvalid`` (d: string) =
    expectPartError MessageCode.DateFormatInvalid d "year"

/// 90.3.8 step 1: `date` goes through its type check and then whether it can be read (94.7.8,
/// a check of `date` alone, row 5 of 90.3.9) before any check of `part`.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1065")>]
[<InlineData("5")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("[1]")>]
[<InlineData("\"week\"")>]
[<InlineData("\"\"")>]
let ``JYRO-REQ-1065 JYRO-DEF-648 DatePart_WithAnUnreadableDate_RaisesDateFormatInvalid_BeforeAnyCheckOfPart`` (part: string) =
    expectExprError MessageCode.DateFormatInvalid (sprintf "DatePart(\"not a date\", %s)" part)
    expectExprError MessageCode.DateFormatInvalid (sprintf "DatePart(part: %s, date: \"2024-02-30\")" part)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1066")>]
let ``JYRO-REQ-1066 JYRO-DEF-648 DatePart_WithADateOfTheWrongType_RaisesArgumentTypeMismatch_BeforeAnyCheckOfPart`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DatePart(%s, \"week\")" lit)
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DatePart(%s, 5)" lit)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1066")>]
let ``JYRO-REQ-1066 DatePart_WithANullNumberOrBooleanArgument_RaisesArgumentTypeMismatch`` () =
    for lit in [ "null"; "5"; "true" ] do
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DatePart(%s, \"year\")" lit)
        expectExprError MessageCode.ArgumentTypeMismatch (sprintf "DatePart(\"2024-01-01\", %s)" lit)
    for v in [ Values.nul; Values.exact 5m; Values.bool false ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DatePart(Input.v, \"year\")"
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "DatePart(\"2024-01-01\", Input.v)"

// ============ 94.5.5, 94.6.6, 94.7.5: a unit or part name is compared by 10.7.1

/// Every limit that could end a run set explicitly (FLOORS 4.1.11).
let private nameOptions : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits (fun o ->
            { o with
                MaxExecutionTime = System.TimeSpan.FromSeconds 30.
                MaxStatements = 1_000_000
                MaxLoopIterations = 1_000_000
                MaxCallDepth = 64 }))
        CompilationTimeout = Some (System.TimeSpan.FromMinutes 10.) }

/// The call of `fn` with the unit or part `name` (a Jyro expression), every
/// argument by position or every argument by name (4.1.8).
let private nameCall (fn: string) (path: string) (name: string) : string =
    match fn, path with
    | "DateAdd", "positional" -> sprintf "DateAdd(\"2024-01-01T00:00:00Z\", 1, %s)" name
    | "DateAdd", _ -> sprintf "DateAdd(date: \"2024-01-01T00:00:00Z\", amount: 1, unit: %s)" name
    | "DateDiff", "positional" -> sprintf "DateDiff(\"2024-01-01T00:00:00Z\", \"2024-01-02T00:00:00Z\", %s)" name
    | "DateDiff", _ -> sprintf "DateDiff(startDate: \"2024-01-01T00:00:00Z\", endDate: \"2024-01-02T00:00:00Z\", unit: %s)" name
    | "DatePart", "positional" -> sprintf "DatePart(\"2024-03-05T07:08:09Z\", %s)" name
    | _ -> sprintf "DatePart(date: \"2024-03-05T07:08:09Z\", part: %s)" name

/// Run `fn` on `path` with `name` reaching it as a Jyro literal or as a host
/// string in `Input.n`.
let private runName (fn: string) (path: string) (form: string) (name: string) : JyroResult<JyroValue> =
    if form = "literal" then
        Script.runWith nameOptions ("Output.r = " + nameCall fn path (AsciiCase.literal name))
    else
        Script.runWith { nameOptions with Input = ValueInput(Values.obj [ "n", Values.str name ]) } ("Output.r = " + nameCall fn path "Input.n")

let private nameResult (fn: string) (path: string) (form: string) (name: string) : string =
    runName fn path form name |> Script.output |> Values.prop "r" |> Values.toJson

let private namePaths = [ "positional"; "named" ]
let private nameForms = [ "literal"; "host" ]

/// Rows: every path, every form, every case label (an unpaired surrogate only
/// in the host form, since no literal can hold one).
let private nameRows (cases: string list) : obj[] seq =
    seq {
        for path in namePaths do
            for form in nameForms do
                for s in cases do
                    if form = "host" || not (AsciiCase.hasUnpairedSurrogate s) then
                        yield [| box path; box form; box (AsciiCase.label s) |]
    }

/// The ASCII lower case of an ASCII string.
let private asciiLower (s: string) : string =
    System.String(s.ToCharArray() |> Array.map (fun c -> if c >= 'A' && c <= 'Z' then char (int c + 32) else c))

/// Assert the name gives the same result as its lower-case spelling.
let private sameAsLowerCase (fn: string) (path: string) (form: string) (name: string) =
    let lower = asciiLower name
    Assert.True(lower <> name, "the case is not a case variant")
    Assert.Equal(nameResult fn path "literal" lower, nameResult fn path form name)

let private expectNameRefused (code: MessageCode) (fn: string) (path: string) (form: string) (name: string) =
    Script.expectError code (runName fn path form name)

// -- 94.5.5 DateAdd

let private addVariants = [ "Weeks"; "WEEK"; "DAY"; "sEcOnDs"; "MiNuTe"; "HOURS"; "Months"; "YEAR"; "Days" ]
let addVariantRows () : obj[] seq = nameRows addVariants

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1035")>]
[<MemberData(nameof addVariantRows)>]
let ``DateAdd_UnitNamesMatchWithNoCaseSensitivityForAsciiLetters`` (path: string, form: string, caseLabel: string) =
    sameAsLowerCase "DateAdd" path form (AsciiCase.byLabel addVariants caseLabel)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1035")>]
let ``DateAdd_Weeks_NamesTheUnitWeeks`` () =
    // The clause's example, against the value of 94.5.4 (1 week after 2024-01-01).
    let t = System.DateTimeOffset.Parse(Values.asString (Values.json (nameResult "DateAdd" "positional" "literal" "Weeks")), System.Globalization.CultureInfo.InvariantCulture)
    Assert.Equal(System.DateTimeOffset(2024, 1, 8, 0, 0, 0, System.TimeSpan.Zero), t)

let private unitLookAlikes =
    [ "wee" + AsciiCase.kelvin
      "wee" + AsciiCase.kelvin + "s"
      "WEE" + AsciiCase.kelvin
      AsciiCase.longS + "econds"
      AsciiCase.longS + "econd"
      "day" + AsciiCase.longS
      "m" + AsciiCase.dotlessI + "nute"
      "M" + AsciiCase.dottedI + "NUTE"
      "m" + AsciiCase.dottedI + "nutes"
      "M" + AsciiCase.dotlessI + "NUTES"
      "wee"
      "weekss"
      "day" + AsciiCase.loneLow
      AsciiCase.loneLow + "days" ]

let unitLookAlikeRows () : obj[] seq = nameRows unitLookAlikes

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1035")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<MemberData(nameof unitLookAlikeRows)>]
let ``JYRO-REQ-1035 JYRO-REQ-1941 DateAdd_NonAsciiLookAlikeOrOtherLength_NamesNoUnit`` (path: string, form: string, caseLabel: string) =
    expectNameRefused MessageCode.DateUnitInvalid "DateAdd" path form (AsciiCase.byLabel unitLookAlikes caseLabel)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1035")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<InlineData("tr-TR")>]
[<InlineData("")>]
let ``JYRO-REQ-1035 JYRO-REQ-1941 DateAdd_UnitNameUnderAnotherCulture_IsComparedTheSameWay`` (culture: string) =
    // 10.7.1 reads no case mapping: under tr-TR, I lower-cases to U+0131.
    AsciiCase.underCulture culture (fun () ->
        for form in nameForms do
            for name in [ "MINUTE"; "MINUTES"; "Minute" ] do
                sameAsLowerCase "DateAdd" "positional" form name)

// -- 94.6.6 DateDiff

let private diffVariants = [ "DAYS"; "Day"; "HOURS"; "Month"; "yEaRs"; "SECOND"; "WEEKS"; "MiNuTeS" ]
let diffVariantRows () : obj[] seq = nameRows diffVariants

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1051")>]
[<MemberData(nameof diffVariantRows)>]
let ``DateDiff_UnitNamesMatchWithNoCaseSensitivityForAsciiLetters`` (path: string, form: string, caseLabel: string) =
    sameAsLowerCase "DateDiff" path form (AsciiCase.byLabel diffVariants caseLabel)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1051")>]
let ``DateDiff_AcceptsEveryUnitName`` () =
    for path in namePaths do
        for u in allUnits do
            for name in [ u; u.ToUpperInvariant() ] do
                let r = runName "DateDiff" path "literal" name
                Assert.True(r.IsSuccess, sprintf "%s %s: %s" path name (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1051")>]
let ``DateDiff_DAYS_NamesTheUnitDays`` () =
    // The clause's example: one day apart gives the count of days (94.6.4).
    Assert.Equal(1.0, Values.asFloat (Values.json (nameResult "DateDiff" "positional" "literal" "DAYS")))

let unitLookAlikeDiffRows () : obj[] seq = nameRows unitLookAlikes

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1051")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<MemberData(nameof unitLookAlikeDiffRows)>]
let ``JYRO-REQ-1051 JYRO-REQ-1941 DateDiff_NonAsciiLookAlikeOrOtherLength_NamesNoUnit`` (path: string, form: string, caseLabel: string) =
    expectNameRefused MessageCode.DateUnitInvalid "DateDiff" path form (AsciiCase.byLabel unitLookAlikes caseLabel)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1051")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<InlineData("tr-TR")>]
[<InlineData("")>]
let ``JYRO-REQ-1051 JYRO-REQ-1941 DateDiff_UnitNameUnderAnotherCulture_IsComparedTheSameWay`` (culture: string) =
    AsciiCase.underCulture culture (fun () ->
        for form in nameForms do
            for name in [ "MINUTE"; "MINUTES"; "Minute" ] do
                sameAsLowerCase "DateDiff" "positional" form name)

// -- 94.7.5 DatePart

let private partVariants = [ "YEAR"; "Month"; "DayOfWeek"; "DAYOFYEAR"; "hOuR"; "Day"; "MINUTE"; "Second" ]
let partVariantRows () : obj[] seq = nameRows partVariants

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1062")>]
[<MemberData(nameof partVariantRows)>]
let ``DatePart_PartNamesMatchWithNoCaseSensitivityForAsciiLetters`` (path: string, form: string, caseLabel: string) =
    sameAsLowerCase "DatePart" path form (AsciiCase.byLabel partVariants caseLabel)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1062")>]
let ``DatePart_DayOfWeek_NamesThePartDayofweek`` () =
    // 2024-03-05 is a Tuesday: dayofweek 2 (94.7.4).
    Assert.Equal(2.0, Values.asFloat (Values.json (nameResult "DatePart" "positional" "literal" "DayOfWeek")))

let private partLookAlikes =
    [ "dayofwee" + AsciiCase.kelvin
      "DAYOFWEE" + AsciiCase.kelvin
      AsciiCase.longS + "econd"
      "m" + AsciiCase.dotlessI + "nute"
      "M" + AsciiCase.dottedI + "NUTE"
      "dayofwee"
      "seconds"
      "second" + AsciiCase.loneLow ]

let partLookAlikeRows () : obj[] seq = nameRows partLookAlikes

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1062")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<MemberData(nameof partLookAlikeRows)>]
let ``JYRO-REQ-1062 JYRO-REQ-1941 DatePart_NonAsciiLookAlikeOrOtherLength_NamesNoPart`` (path: string, form: string, caseLabel: string) =
    expectNameRefused MessageCode.DatePartInvalid "DatePart" path form (AsciiCase.byLabel partLookAlikes caseLabel)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1062")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<InlineData("tr-TR")>]
[<InlineData("")>]
let ``JYRO-REQ-1062 JYRO-REQ-1941 DatePart_PartNameUnderAnotherCulture_IsComparedTheSameWay`` (culture: string) =
    AsciiCase.underCulture culture (fun () ->
        for form in nameForms do
            for name in [ "MINUTE"; "DAYOFWEEK"; "DayOfYear" ] do
                sameAsLowerCase "DatePart" "positional" form name)

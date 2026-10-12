/// jyro-spec 91.4 Replace, 91.5 Contains, 91.6 StartsWith, 91.7 EndsWith,
/// 91.8 Split, 91.9 Join.
module RealTree.Jyro.Spec.Tests.P91.S91_04_09_SearchSplitJoin

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P91.Support

let badValues = badData

// ============================================================================
// 91.4 Replace
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1982")>]
let Replace_PositionalAndNamed_ReturnString () =
    Assert.Equal("axc", str "Replace(\"abc\", \"b\", \"x\")")
    Assert.Equal("axc", str "Replace(text: \"abc\", oldValue: \"b\", newValue: \"x\")")
    Assert.Equal("axc", str "Replace(newValue: \"x\", text: \"abc\", oldValue: \"b\")")
    Assert.Equal("string", typeOf "Replace(\"abc\", \"b\", \"x\")")
    Assert.Equal("string", typeOf "Replace(1, 1, 2)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1982")>]
let Replace_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "Replace(\"a\", \"b\")"
    expectErr MessageCode.TooManyArguments "Replace(\"a\", \"b\", \"c\", \"d\")"
    expectErrAmong MessageCode.UnknownNamedArgument "Replace(source: \"a\", oldValue: \"a\", newValue: \"b\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1983")>]
[<InlineData("a-b-c", "-", "+", "a+b+c")>]
[<InlineData("abcabc", "bc", "", "aa")>]
[<InlineData("x", "x", "yyy", "yyy")>]
[<InlineData("aXbX", "X", "", "ab")>]
[<InlineData("XaX", "X", "_", "_a_")>]
let Replace_EveryOccurrence_Replaced (source: string, oldValue: string, newValue: string, expected: string) =
    Assert.Equal(expected, str (sprintf "Replace(%s, %s, %s)" (lit source) (lit oldValue) (lit newValue)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-408")>]
[<InlineData("aaa", "aa", "b", "ba")>]
[<InlineData("aaaa", "aa", "b", "bb")>]
[<InlineData("aaaaa", "aa", "b", "bba")>]
[<InlineData("ababa", "aba", "x", "xba")>]
let Replace_OccurrencesLeftToRight_NoOverlap (source: string, oldValue: string, newValue: string, expected: string) =
    Assert.Equal(expected, str (sprintf "Replace(%s, %s, %s)" (lit source) (lit oldValue) (lit newValue)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-409")>]
[<InlineData("ABC", "b")>]
[<InlineData("ß", "ss")>]
[<InlineData("strasse", "ß")>]
[<InlineData("a­b", "ab")>]
[<InlineData("é", "é")>]
[<InlineData("é", "é")>]
[<InlineData("I", "i")>]
[<InlineData("K", "K")>]
let Replace_OrdinalComparison_NoMatchWithoutEqualCodeUnits (source: string, oldValue: string) =
    Assert.Equal(source, str (sprintf "Replace(%s, %s, \"x\")" (lit source) (lit oldValue)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-409")>]
let Replace_OrdinalComparison_MatchesLoneSurrogateCodeUnit () =
    let input = surrogates ()
    Assert.Equal("x" + lo, strIn input "Replace(Input.pair, Input.hi, \"x\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-410")>]
[<InlineData("a", "a", "aa", "aa")>]
[<InlineData("ab", "a", "ba", "bab")>]
[<InlineData("aXa", "a", "aa", "aaXaa")>]
[<InlineData("aa", "a", "aaa", "aaaaaa")>]
let Replace_InsertedText_NotSearchedAgain (source: string, oldValue: string, newValue: string, expected: string) =
    Assert.Equal(expected, str (sprintf "Replace(%s, %s, %s)" (lit source) (lit oldValue) (lit newValue)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-411")>]
[<InlineData("abc", "z")>]
[<InlineData("", "z")>]
[<InlineData("ab", "abc")>]
let Replace_NoOccurrence_ReturnsEqualString (source: string, oldValue: string) =
    Assert.Equal(source, str (sprintf "Replace(%s, %s, \"y\")" (lit source) (lit oldValue)))
    Assert.True(boolOf (sprintf "Replace(%s, %s, \"y\") == %s" (lit source) (lit oldValue) (lit source)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1736")>]
[<InlineData("Replace(\"abc\", \"\", \"x\")")>]
[<InlineData("Replace(\"\", \"\", \"x\")")>]
[<InlineData("Replace(\"abc\", null, \"x\")")>]
[<InlineData("Replace(null, null, null)")>]
[<InlineData("Replace(\"abc\", Input.missing, \"x\")")>]
let ``JYRO-REQ-1736 Replace_EmptyOrNullOldValue_RaisesRuntimeError`` (expr: string) =
    failsAtRuntime expr
    expectErr MessageCode.RuntimeError expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1736")>]
let ``JYRO-REQ-1736 Replace_EmptyOrNullOldValue_ReturnsNoString`` () =
    for expr in [ "Replace(\"abc\", \"\", \"x\")"; "Replace(\"abc\", null, \"x\")" ] do
        let result = run expr
        Assert.False(Script.ok result, expr)
        Assert.Equal(5002, int (Script.onlyError result))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1736")>]
let ``JYRO-REQ-1736 Replace_NonEmptyOldValue_DoesNotRaise`` () =
    Assert.Equal("axc", str "Replace(\"abc\", \"b\", \"x\")")
    Assert.Equal("abc", str "Replace(\"abc\", \"z\", \"x\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1984")>]
let Replace_NullSourceOrNewValue_ReadAsEmptyString () =
    Assert.Equal("", str "Replace(null, \"a\", \"b\")")
    Assert.Equal("ac", str "Replace(\"abc\", \"b\", null)")
    Assert.Equal("", str "Replace(Input.missing, \"a\", \"b\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1984")>]
let Replace_NullOldValue_ReadAsEmptyStringAndSoFails () =
    failsAtRuntime "Replace(\"null\", null, \"x\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1985")>]
[<InlineData("Replace(12321, 2, 9)", "19391")>]
[<InlineData("Replace(true, \"u\", \"U\")", "trUe")>]
[<InlineData("Replace(\"x1.5\", 1.50, false)", "xfalse")>]
[<InlineData("Replace(\"t\", \"t\", true)", "true")>]
[<InlineData("Replace(ToNumber(\"1e30\"), \"+\", \"\")", "1e30")>]
let ``JYRO-REQ-1985 Replace_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1986")>]
[<MemberData(nameof badValues)>]
let Replace_ContainerOrFunctionInAnyPosition_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Replace(%s, \"a\", \"b\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Replace(\"a\", %s, \"b\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Replace(\"a\", \"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-416")>]
let Replace_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("b", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "Replace(\"a\", \"a\", \"b\")"))

// ============================================================================
// 91.5 Contains
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2141")>]
let ``JYRO-REQ-2141 Contains_StringOrArrayText_ReturnsBoolean`` () =
    Assert.True(boolOf "Contains(\"abc\", \"b\")")
    // the order is (source, value): swapped, "b" does not hold "abc"
    Assert.False(boolOf "Contains(\"b\", \"abc\")")
    Assert.Equal("boolean", typeOf "Contains(\"b\", \"abc\")")
    Assert.Equal("boolean", typeOf "Contains([[1]], [1])")
    Assert.Equal("boolean", typeOf "Contains(\"abc\", \"\")")
    Assert.True(boolOf "Contains(source: \"abc\", value: \"b\")")
    Assert.True(boolOf "Contains(value: \"b\", source: \"abc\")")
    Assert.True(boolOf "Contains([1], 1)")
    Assert.Equal("boolean", typeOf "Contains(\"abc\", \"z\")")
    Assert.Equal("boolean", typeOf "Contains([1], 2)")
    Assert.Equal("boolean", typeOf "Contains(null, 2)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2141")>]
let ``JYRO-REQ-2141 Contains_WrongArityOrName_FailsWithArgumentCodes`` () =
    expectErr MessageCode.TooFewArguments "Contains(\"a\")"
    expectErr MessageCode.TooManyArguments "Contains(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "Contains(text: \"a\", search: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2142")>]
[<InlineData("hello", "ell", true)>]
[<InlineData("hello", "hello", true)>]
[<InlineData("hello", "lo", true)>]
[<InlineData("hello", "h", true)>]
[<InlineData("hello", "hlo", false)>]
[<InlineData("hello", "ELL", false)>]
[<InlineData("hello", "helloo", false)>]
[<InlineData("straße", "ss", false)>]
[<InlineData("a­b", "ab", false)>]
[<InlineData("é", "é", false)>]
[<InlineData("é", "e", false)>]
[<InlineData("I", "i", false)>]
[<InlineData("abc", "bc", true)>]
[<InlineData("abc", "BC", false)>]
[<InlineData("abc", "ac", false)>]
[<InlineData("abc", "", true)>]
[<InlineData("", "a", false)>]
[<InlineData("abc", "abcd", false)>]
let ``JYRO-REQ-2142 Contains_StringText_TrueExactlyForContiguousOrdinalRun`` (text: string, search: string, expected: bool) =
    Assert.Equal(expected, boolOf (sprintf "Contains(%s, %s)" (lit text) (lit search)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2142")>]
let ``JYRO-REQ-2142 Contains_StringText_FindsLoneSurrogateCodeUnitOfPair`` () =
    let input = surrogates ()
    Assert.True(boolIn input "Contains(Input.pair, Input.hi)")
    Assert.True(boolIn input "Contains(Input.pair, Input.lo)")
    Assert.False(boolIn input "Contains(Input.hi, Input.pair)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1987")>]
let Contains_EmptySearchInString_ReturnsTrue () =
    Assert.True(boolOf "Contains(\"abc\", \"\")")
    Assert.True(boolOf "Contains(\"\", \"\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2143")>]
[<InlineData("[1, 2, 3]", "2", true)>]
[<InlineData("[1, 2]", "3", false)>]
[<InlineData("[\"1\"]", "1", false)>]
[<InlineData("[1]", "\"1\"", false)>]
[<InlineData("[\"abc\"]", "\"b\"", false)>]
[<InlineData("[[1, 2]]", "[1, 2]", true)>]
[<InlineData("[[1, 2]]", "[2, 1]", false)>]
[<InlineData("[{a: 1, b: 2}]", "{b: 2, a: 1}", true)>]
[<InlineData("[true]", "1", false)>]
[<InlineData("[false]", "false", true)>]
[<InlineData("[]", "1", false)>]
[<InlineData("[0.1 + 0.2]", "0.3", true)>]
[<InlineData("[1.50]", "1.5", true)>]
[<InlineData("[Power(2, 0.5)]", "Power(2, 0.5)", true)>]
[<InlineData("[\"a\", \"b\"]", "\"B\"", false)>]
[<InlineData("[[1]]", "[1]", true)>]
[<InlineData("[2]", "2.0", true)>]
[<InlineData("[1, null]", "null", true)>]
[<InlineData("[1]", "null", false)>]
[<InlineData("[[1]]", "1", false)>]
[<InlineData("[{a: [1]}]", "{a: [1]}", true)>]
[<InlineData("[{a: [1]}]", "{a: [2]}", false)>]
[<InlineData("[null]", "0", false)>]
[<InlineData("[]", "null", false)>]
let ``JYRO-REQ-2143 Contains_ArrayText_TrueExactlyWhenAnElementIsEqual`` (items: string, search: string, expected: bool) =
    Assert.Equal(expected, boolOf (sprintf "Contains(%s, %s)" items search))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-421")>]
let Contains_ArrayTextNullSearch_TrueWhenAnElementIsNull () =
    Assert.True(boolOf "Contains([1, null], null)")
    Assert.True(boolOf "Contains([null], Input.missing)")
    Assert.False(boolOf "Contains([1, 2], null)")
    Assert.False(boolOf "Contains([\"null\", \"\"], null)")
    Assert.False(boolOf "Contains([], null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1988")>]
[<InlineData("Contains(null, \"a\")")>]
[<InlineData("Contains(null, \"\")")>]
[<InlineData("Contains(null, null)")>]
[<InlineData("Contains(null, [1])")>]
[<InlineData("Contains(null, {})")>]
[<InlineData("Contains(Input.missing, \"a\")")>]
let Contains_NullText_ReturnsFalse (expr: string) =
    Assert.False(boolOf expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1989")>]
[<InlineData("Contains(\"abc\", null)")>]
[<InlineData("Contains(\"\", null)")>]
[<InlineData("Contains(\"null\", null)")>]
[<InlineData("Contains(\"abc\", Input.missing)")>]
let Contains_StringTextNullSearch_ReturnsFalse (expr: string) =
    Assert.False(boolOf expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1990")>]
[<InlineData("Contains(\"a12b\", 12)", true)>]
[<InlineData("Contains(\"a1.5b\", 1.50)", true)>]
[<InlineData("Contains(\"a1.50b\", 1.50)", true)>]
[<InlineData("Contains(\"x1.0\", 1.0)", true)>]
[<InlineData("Contains(\"a.0\", 1.0)", false)>]
[<InlineData("Contains(\"abc\", 1)", false)>]
[<InlineData("Contains(\"istrue\", true)", true)>]
[<InlineData("Contains(\"f\", false)", false)>]
[<InlineData("Contains(\"1e+30\", ToNumber(\"1e30\"))", true)>]
let ``JYRO-REQ-1990 Contains_NumberOrBooleanSearch_ReadAsStringForm`` (expr: string, expected: bool) =
    Assert.Equal(expected, boolOf expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1990")>]
let ``JYRO-REQ-1990 Contains_ExactNumberSearch_UsesCanonicalStringForm`` () =
    // 1.50 has the string form "1.5": text holding "1.5" but not "1.50" still contains it
    Assert.True(boolOf "Contains(\"v1.5!\", 1.50)")
    // 1.0 has the string form "1": the text "x1" contains it
    Assert.True(boolOf "Contains(\"x1\", 1.0)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1991")>]
[<MemberData(nameof badValues)>]
let ``JYRO-REQ-1991 Contains_StringTextContainerOrFunctionSearch_FailsWithArgumentTypeMismatch`` (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Contains(\"abc\", %s)" bad)
    expectErrPre MessageCode.ArgumentTypeMismatch ("var v = " + bad) "Contains(\"abc\", v)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1991")>]
let ``JYRO-REQ-1991 Contains_ArrayTextContainerSearch_Accepted`` () =
    Assert.True(boolOf "Contains([[\"a\"]], [\"a\"])")
    Assert.False(boolOf "Contains([1], {})")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1992")>]
[<InlineData("Contains(5, \"5\")")>]
[<InlineData("Contains(true, \"t\")")>]
[<InlineData("Contains(false, null)")>]
[<InlineData("Contains({a: 1}, \"a\")")>]
[<InlineData("Contains({}, null)")>]
[<InlineData("Contains(x => x, 1)")>]
[<InlineData("Contains(0, null)")>]
let ``JYRO-REQ-1992 Contains_TextNotStringArrayOrNull_FailsWithStringOrArrayRequired`` (expr: string) =
    expectErr MessageCode.StringOrArrayRequired expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-427")>]
let Contains_UnderPureCeiling_CompilesAndRuns () =
    Assert.True(Values.asBool (evalUnderMaxEffect FunctionEffect.Pure "Contains(\"ab\", \"a\")"))

// ============================================================================
// 91.6 StartsWith
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-428")>]
let StartsWith_PositionalAndNamed_ReturnBoolean () =
    Assert.True(boolOf "StartsWith(\"abc\", \"a\")")
    Assert.True(boolOf "StartsWith(text: \"abc\", prefix: \"a\")")
    Assert.False(boolOf "StartsWith(prefix: \"abc\", text: \"a\")")
    Assert.Equal("boolean", typeOf "StartsWith(\"abc\", \"z\")")
    Assert.Equal("boolean", typeOf "StartsWith(null, null)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-428")>]
let StartsWith_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "StartsWith(\"a\")"
    expectErr MessageCode.TooManyArguments "StartsWith(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "StartsWith(text: \"a\", value: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-429")>]
[<InlineData("hello", "he", true)>]
[<InlineData("hello", "hello", true)>]
[<InlineData("hello", "h", true)>]
[<InlineData("hello", "el", false)>]
[<InlineData("Hello", "he", false)>]
[<InlineData("hello", "hellx", false)>]
[<InlineData("Åx", "Å", false)>]
[<InlineData("Åx", "Å", false)>]
[<InlineData("­abc", "abc", false)>]
[<InlineData("é", "e", true)>]
[<InlineData("æ", "ae", false)>]
let StartsWith_TrueExactlyWhenLeadingCodeUnitsEqualPrefix (text: string, prefix: string, expected: bool) =
    Assert.Equal(expected, boolOf (sprintf "StartsWith(%s, %s)" (lit text) (lit prefix)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-429")>]
let StartsWith_PairText_StartsWithItsLoneHighSurrogate () =
    let input = surrogates ()
    Assert.True(boolIn input "StartsWith(Input.pair, Input.hi)")
    Assert.False(boolIn input "StartsWith(Input.pair, Input.lo)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-430")>]
[<InlineData("ab", "abc")>]
[<InlineData("", "a")>]
[<InlineData("a", "a ")>]
let StartsWith_PrefixLongerThanText_ReturnsFalse (text: string, prefix: string) =
    Assert.False(boolOf (sprintf "StartsWith(%s, %s)" (lit text) (lit prefix)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-431")>]
let StartsWith_EmptyPrefix_ReturnsTrue () =
    Assert.True(boolOf "StartsWith(\"abc\", \"\")")
    Assert.True(boolOf "StartsWith(\"\", \"\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-433")>]
let StartsWith_NullTextOrPrefix_ReadAsEmptyString () =
    Assert.True(boolOf "StartsWith(null, \"\")")
    Assert.False(boolOf "StartsWith(null, \"a\")")
    Assert.False(boolOf "StartsWith(null, \"n\")")
    Assert.True(boolOf "StartsWith(\"abc\", null)")
    Assert.True(boolOf "StartsWith(null, null)")
    Assert.True(boolOf "StartsWith(\"xnull\", null)")
    Assert.True(boolOf "StartsWith(Input.missing, Input.missing)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1993")>]
[<InlineData("StartsWith(123, 12)", true)>]
[<InlineData("StartsWith(1.50, \"1.5\")", true)>]
[<InlineData("StartsWith(1.50, \"1.50\")", false)>]
[<InlineData("StartsWith(\"1.5x\", 1.50)", true)>]
[<InlineData("StartsWith(true, \"tr\")", true)>]
[<InlineData("StartsWith(\"falsey\", false)", true)>]
[<InlineData("StartsWith(ToNumber(\"1e30\"), \"1e+\")", true)>]
let ``JYRO-REQ-1993 StartsWith_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: bool) =
    Assert.Equal(expected, boolOf expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-435")>]
[<MemberData(nameof badValues)>]
let StartsWith_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "StartsWith(%s, \"a\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "StartsWith(\"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-436")>]
let StartsWith_UnderPureCeiling_CompilesAndRuns () =
    Assert.True(Values.asBool (evalUnderMaxEffect FunctionEffect.Pure "StartsWith(\"ab\", \"a\")"))

// ============================================================================
// 91.7 EndsWith
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-437")>]
let EndsWith_PositionalAndNamed_ReturnBoolean () =
    Assert.True(boolOf "EndsWith(\"abc\", \"c\")")
    Assert.True(boolOf "EndsWith(text: \"abc\", suffix: \"c\")")
    Assert.False(boolOf "EndsWith(suffix: \"abc\", text: \"c\")")
    Assert.Equal("boolean", typeOf "EndsWith(\"abc\", \"z\")")
    Assert.Equal("boolean", typeOf "EndsWith(null, null)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-437")>]
let EndsWith_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "EndsWith(\"a\")"
    expectErr MessageCode.TooManyArguments "EndsWith(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "EndsWith(text: \"a\", value: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-438")>]
[<InlineData("hello", "lo", true)>]
[<InlineData("hello", "hello", true)>]
[<InlineData("hello", "o", true)>]
[<InlineData("hello", "ll", false)>]
[<InlineData("hellO", "lo", false)>]
[<InlineData("hello", "xello", false)>]
[<InlineData("xÅ", "Å", false)>]
[<InlineData("abc­", "abc", false)>]
[<InlineData("é", "́", true)>]
[<InlineData("é", "é", false)>]
let EndsWith_TrueExactlyWhenTrailingCodeUnitsEqualSuffix (text: string, suffix: string, expected: bool) =
    Assert.Equal(expected, boolOf (sprintf "EndsWith(%s, %s)" (lit text) (lit suffix)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-438")>]
let EndsWith_PairText_EndsWithItsLoneLowSurrogate () =
    let input = surrogates ()
    Assert.True(boolIn input "EndsWith(Input.pair, Input.lo)")
    Assert.False(boolIn input "EndsWith(Input.pair, Input.hi)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-439")>]
[<InlineData("bc", "abc")>]
[<InlineData("", "a")>]
[<InlineData("a", " a")>]
let EndsWith_SuffixLongerThanText_ReturnsFalse (text: string, suffix: string) =
    Assert.False(boolOf (sprintf "EndsWith(%s, %s)" (lit text) (lit suffix)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-440")>]
let EndsWith_EmptySuffix_ReturnsTrue () =
    Assert.True(boolOf "EndsWith(\"abc\", \"\")")
    Assert.True(boolOf "EndsWith(\"\", \"\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-442")>]
let EndsWith_NullTextOrSuffix_ReadAsEmptyString () =
    Assert.True(boolOf "EndsWith(null, \"\")")
    Assert.False(boolOf "EndsWith(null, \"a\")")
    Assert.False(boolOf "EndsWith(null, \"l\")")
    Assert.True(boolOf "EndsWith(\"abc\", null)")
    Assert.True(boolOf "EndsWith(null, null)")
    Assert.True(boolOf "EndsWith(\"nullx\", null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1994")>]
[<InlineData("EndsWith(123, 23)", true)>]
[<InlineData("EndsWith(1.50, \".5\")", true)>]
[<InlineData("EndsWith(1.50, \"0\")", false)>]
[<InlineData("EndsWith(\"x1.5\", 1.50)", true)>]
[<InlineData("EndsWith(true, \"ue\")", true)>]
[<InlineData("EndsWith(\"isfalse\", false)", true)>]
[<InlineData("EndsWith(ToNumber(\"1e30\"), \"e+30\")", true)>]
let ``JYRO-REQ-1994 EndsWith_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: bool) =
    Assert.Equal(expected, boolOf expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-444")>]
[<MemberData(nameof badValues)>]
let EndsWith_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "EndsWith(%s, \"a\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "EndsWith(\"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-445")>]
let EndsWith_UnderPureCeiling_CompilesAndRuns () =
    Assert.True(Values.asBool (evalUnderMaxEffect FunctionEffect.Pure "EndsWith(\"ab\", \"b\")"))

// ============================================================================
// 91.8 Split
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-446")>]
let Split_PositionalAndNamed_ReturnArrayOfStrings () =
    Assert.Equal<string list>([ "a"; "b" ], strs "Split(\"a,b\", \",\")")
    Assert.Equal<string list>([ "a"; "b" ], strs "Split(text: \"a,b\", separator: \",\")")
    Assert.Equal<string list>([ "a"; "b" ], strs "Split(separator: \",\", text: \"a,b\")")
    Assert.Equal("array", typeOf "Split(\"a,b\", \",\")")
    // numeric-looking parts stay strings (strs fails on a non-string element)
    Assert.Equal<string list>([ "1"; "2" ], strs "Split(\"1,2\", \",\")")
    Assert.Equal<string list>([ "true"; "null" ], strs "Split(\"true,null\", \",\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-446")>]
let Split_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "Split(\"a\")"
    expectErr MessageCode.TooManyArguments "Split(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "Split(text: \"a\", value: \",\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-447")>]
[<InlineData("a,b,c", ",", "a|b|c")>]
[<InlineData("one--two--three", "--", "one|two|three")>]
[<InlineData("abc", ",", "abc")>]
[<InlineData("c,b,a", ",", "c|b|a")>]
let Split_PartsBetweenOccurrences_InTextOrder (text: string, separator: string, expected: string) =
    Assert.Equal<string list>(List.ofArray (expected.Split('|')), strs (sprintf "Split(%s, %s)" (lit text) (lit separator)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-448")>]
[<InlineData("aaa", "aa", "|a")>]
[<InlineData("aaaa", "aa", "||")>]
[<InlineData("aaaaa", "aa", "||a")>]
[<InlineData("ababa", "aba", "|ba")>]
[<InlineData("aBa", "b", "aBa")>]
[<InlineData("a­b", "ab", "a­b")>]
[<InlineData("xßy", "ss", "xßy")>]
[<InlineData("é", "é", "é")>]
let Split_OccurrencesLeftToRightOrdinal_NoOverlap (text: string, separator: string, expected: string) =
    Assert.Equal<string list>(List.ofArray (expected.Split('|')), strs (sprintf "Split(%s, %s)" (lit text) (lit separator)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-448")>]
let Split_OrdinalOnCodeUnits_SplitsPairAtLoneSurrogate () =
    let input = surrogates ()
    Assert.Equal<string list>([ "a"; lo + "b" ], strsIn input "Split(\"a\" & Input.pair & \"b\", Input.hi)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-449")>]
[<InlineData("a,,b", "a||b")>]
[<InlineData(",a", "|a")>]
[<InlineData("a,", "a|")>]
[<InlineData(",", "|")>]
[<InlineData(",,,", "|||")>]
let Split_AdjacentOrEdgeOccurrences_GiveEmptyElements (text: string, expected: string) =
    Assert.Equal<string list>(List.ofArray (expected.Split('|')), strs (sprintf "Split(%s, \",\")" (lit text)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-450")>]
[<InlineData("a,b,c", ",", 2)>]
[<InlineData("abc", ",", 0)>]
[<InlineData(",,,", ",", 3)>]
[<InlineData("aaa", "aa", 1)>]
[<InlineData("aaaa", "aa", 2)>]
[<InlineData("one--two--three", "--", 2)>]
[<InlineData("a,,b", ",", 2)>]
let Split_ResultLength_IsOccurrencesPlusOne (text: string, separator: string, occurrences: int) =
    Assert.Equal(occurrences + 1, (strs (sprintf "Split(%s, %s)" (lit text) (lit separator))).Length)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-450")>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(50)>]
let Split_GeneratedText_LengthIsSeparatorCountPlusOne (k: int) =
    let text = String.replicate k "x;" + "y"
    Assert.Equal(k + 1, (strs (sprintf "Split(%s, \";\")" (lit text))).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-451")>]
let Split_EmptyText_ReturnsOneEmptyString () =
    Assert.Equal<string list>([ "" ], strs "Split(\"\", \",\")")
    Assert.Equal<string list>([ "" ], strs "Split(\"\", \"ab\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-452")>]
[<InlineData("Split(\"abc\", \"\")")>]
[<InlineData("Split(\"\", \"\")")>]
[<InlineData("Split(\"abc\", null)")>]
[<InlineData("Split(null, null)")>]
[<InlineData("Split(\"abc\", Input.missing)")>]
let ``JYRO-REQ-452 Split_EmptyOrNullSeparator_FailsAtRuntime`` (expr: string) =
    failsAtRuntime expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-453")>]
let ``JYRO-REQ-453 Split_NullText_ReadAsEmptyString`` () =
    Assert.Equal<string list>([ "" ], strs "Split(null, \",\")")
    Assert.Equal<string list>([ "" ], strs "Split(Input.missing, \",\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-453")>]
let ``JYRO-REQ-453 Split_NullSeparator_ReadAsEmptyStringAndSoFails`` () =
    failsAtRuntime "Split(\"a,null,b\", null)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1995")>]
[<InlineData("Split(1.5, \".\")", "1|5")>]
[<InlineData("Split(1.50, \"0\")", "1.5")>]
[<InlineData("Split(12321, 2)", "1|3|1")>]
[<InlineData("Split(true, \"u\")", "tr|e")>]
[<InlineData("Split(\"atrueb\", true)", "a|b")>]
[<InlineData("Split(\"a1.5b\", 1.50)", "a|b")>]
[<InlineData("Split(ToNumber(\"1e30\"), \"e\")", "1|+30")>]
let ``JYRO-REQ-1995 Split_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal<string list>(List.ofArray (expected.Split('|')), strs expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-455")>]
[<MemberData(nameof badValues)>]
let Split_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Split(%s, \",\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Split(\"a,b\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-456")>]
let Split_UnderPureCeiling_CompilesAndRuns () =
    let v = evalUnderMaxEffect FunctionEffect.Pure "Split(\"a,b\", \",\")"
    Assert.Equal<string list>([ "a"; "b" ], Values.asList v |> List.map Values.asString)

// ============================================================================
// 91.9 Join
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-457")>]
let Join_PositionalAndNamed_ReturnString () =
    Assert.Equal("a,b", str "Join([\"a\", \"b\"], \",\")")
    Assert.Equal("a,b", str "Join(values: [\"a\", \"b\"], separator: \",\")")
    Assert.Equal("a,b", str "Join(separator: \",\", values: [\"a\", \"b\"])")
    Assert.Equal("string", typeOf "Join([\"a\", \"b\"], \",\")")
    Assert.Equal("string", typeOf "Join([], \",\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-457")>]
let Join_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "Join([\"a\"])"
    expectErr MessageCode.TooManyArguments "Join([\"a\"], \",\", \"x\")"
    expectErrAmong MessageCode.UnknownNamedArgument "Join(items: [\"a\"], separator: \",\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-458")>]
[<InlineData("Join([\"a\", \"b\", \"c\"], \", \")", "a, b, c")>]
[<InlineData("Join([\"x\"], \"-\")", "x")>]
[<InlineData("Join([\"\", \"\"], \"-\")", "-")>]
[<InlineData("Join([\"\", \"\", \"\"], \"-\")", "--")>]
[<InlineData("Join([\"c\", \"a\"], \"\")", "ca")>]
[<InlineData("Join([\"a\", \"b\"], \"--\")", "a--b")>]
let Join_ElementsInOrderWithSeparatorBetween (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-459")>]
let Join_EmptyArray_ReturnsEmptyString () =
    Assert.Equal("", str "Join([], \",\")")
    Assert.Equal("", str "Join([], \"\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-460")>]
let Join_NullBooleanAndStringElements_WrittenAsStringForms () =
    Assert.Equal("null|true|false|s", str "Join([null, true, false, \"s\"], \"|\")")
    Assert.Equal("null", str "Join([null], \",\")")
    Assert.Equal("null", str "Join([\"null\"], \",\")")
    Assert.Equal(",", str "Join([\"\", \"\"], \",\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-461")>]
let ``JYRO-REQ-461 Join_ExactNumbers_WrittenInExactStringForm`` () =
    Assert.Equal("0,0,1.5,-2.25,100,0.3", str "Join([0, -0, 1.50, -2.25, 100, 0.1 + 0.2], \",\")")
    Assert.Equal("1000000000000000000000", str "Join([1000000000000000000000], \",\")")
    Assert.Equal("0.000001", str "Join([0.000001], \",\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-461")>]
let ``JYRO-REQ-461 Join_InexactNumbers_WrittenAsEcmaScriptNumberToString`` () =
    Assert.Equal("1.4142135623730951,1e+30", str "Join([Power(2, 0.5), ToNumber(\"1e30\")], \",\")")
    Assert.Equal("1e-7", str "Join([ToNumber(\"1e-7\") * Power(1, 0.5)], \",\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-462")>]
[<InlineData("Join([1, [2]], \",\")")>]
[<InlineData("Join([{}], \",\")")>]
[<InlineData("Join([\"a\", x => x], \",\")")>]
[<InlineData("Join([[]], \",\")")>]
[<InlineData("Join([[\"a\"], \"b\"], \",\")")>]
[<InlineData("Join([{a: 1}], \"\")")>]
let ``JYRO-REQ-462 Join_ContainerOrFunctionElement_FailsWithConcatWithInvalidType`` (expr: string) =
    expectErr MessageCode.ConcatWithInvalidType expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-463")>]
let Join_NullValues_ReturnsEmptyString () =
    Assert.Equal("", str "Join(null, \",\")")
    Assert.Equal("", str "Join(Input.missing, \"x\")")
    Assert.Equal("", str "Join(null, null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-464")>]
[<InlineData("Join(\"abc\", \",\")")>]
[<InlineData("Join(\"\", \",\")")>]
[<InlineData("Join(5, \",\")")>]
[<InlineData("Join(true, \",\")")>]
[<InlineData("Join({a: 1}, \",\")")>]
[<InlineData("Join({}, \",\")")>]
[<InlineData("Join(x => x, \",\")")>]
let Join_ValuesNotArrayNotNull_FailsWithArgumentTypeMismatch (expr: string) =
    expectErr MessageCode.ArgumentTypeMismatch expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-465")>]
let Join_NullSeparator_ReadAsEmptyString () =
    Assert.Equal("ab", str "Join([\"a\", \"b\"], null)")
    Assert.Equal("ab", str "Join([\"a\", \"b\"], Input.missing)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-466")>]
[<InlineData("Join([\"a\", \"b\"], 0)", "a0b")>]
[<InlineData("Join([\"a\", \"b\"], 1.50)", "a1.5b")>]
[<InlineData("Join([\"a\", \"b\"], true)", "atrueb")>]
[<InlineData("Join([\"a\", \"b\"], false)", "afalseb")>]
let ``JYRO-REQ-466 Join_NumberOrBooleanSeparator_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-467")>]
[<MemberData(nameof badValues)>]
let Join_ContainerOrFunctionSeparator_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Join([\"a\", \"b\"], %s)" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Join([\"a\"], %s)" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Join([], %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-468")>]
let Join_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("a", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "Join([\"a\"], \",\")"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-468")>]
let Join_DoesNotChangeValues () =
    let v = evalPre "var v = [1, \"b\", null, true]\nvar s = Join(v, \",\")" "v"
    match Values.asList v with
    | [ a; b; c; d ] ->
        Assert.Equal(1m, Values.asDecimal a)
        Assert.Equal("b", Values.asString b)
        Assert.True(isNullValue c)
        Assert.True(Values.asBool d)
    | other -> Values.fail (sprintf "values changed length: %d" other.Length)

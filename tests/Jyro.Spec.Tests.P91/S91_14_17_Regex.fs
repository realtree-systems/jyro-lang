/// jyro-spec 91.14 RegexTest, 91.15 RegexMatch, 91.16 RegexMatchAll,
/// 91.17 RegexMatchDetail.
module RealTree.Jyro.Spec.Tests.P91.S91_14_17_Regex

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P91.Support

let badValues = badData

/// The four regular expression functions (rule 4.1.8: each path tested).
let regexFunctions : obj[] seq =
    [ "RegexTest"; "RegexMatch"; "RegexMatchAll"; "RegexMatchDetail" ] |> Seq.map (fun s -> [| box s |])

/// A boolean expression that is true when `fn(text, pattern)` found a match.
let found (fn: string) (text: string) (pattern: string) : string =
    let call = sprintf "%s(%s, %s)" fn text pattern
    match fn with
    | "RegexTest" -> call
    | "RegexMatchAll" -> "Length(" + call + ") > 0"
    | _ -> call + " != null"

let matches (fn: string) (text: string) (pattern: string) : bool =
    boolOf (found fn (lit text) (lit pattern))

// ============================================================================
// 91.14 RegexTest (and the rules of 91.14.3, .4 and .6 for all four functions)
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-520")>]
let RegexTest_PositionalAndNamed_ReturnBoolean () =
    Assert.True(boolOf "RegexTest(\"abc\", \"b\")")
    Assert.True(boolOf "RegexTest(text: \"abc\", pattern: \"b\")")
    Assert.False(boolOf "RegexTest(pattern: \"abc\", text: \"b\")")
    Assert.Equal("boolean", typeOf "RegexTest(\"abc\", \"b\")")
    Assert.Equal("boolean", typeOf "RegexTest(\"abc\", \"z\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-520")>]
let RegexTest_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "RegexTest(\"a\")"
    expectErr MessageCode.TooManyArguments "RegexTest(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "RegexTest(input: \"a\", pattern: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-521")>]
[<InlineData("xxabc", "abc", true)>]
[<InlineData("abcxx", "abc", true)>]
[<InlineData("aXbXc", "X", true)>]
[<InlineData("abc", "d", false)>]
[<InlineData("abc", "^b", false)>]
[<InlineData("abc", "c$", true)>]
let RegexTest_TrueWhenPatternMatchesAtSomePosition (text: string, pattern: string, expected: bool) =
    Assert.Equal(expected, boolOf (sprintf "RegexTest(%s, %s)" (lit text) (lit pattern)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-522")>]
[<MemberData(nameof regexFunctions)>]
let ``JYRO-REQ-522 Regex_BackslashD_MatchesOnlyAsciiDigits`` (fn: string) =
    Assert.True(matches fn "0" @"^\d$")
    Assert.True(matches fn "9" @"^\d$")
    for c in [ "٠"; "०"; "１"; "²"; "a" ] do
        Assert.False(matches fn c @"\d", sprintf "%s: \\d matched U+%04X" fn (int c.[0]))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-522")>]
[<MemberData(nameof regexFunctions)>]
let ``JYRO-REQ-522 Regex_BackslashW_MatchesOnlyAsciiWordCharacters`` (fn: string) =
    for c in [ "A"; "Z"; "a"; "z"; "0"; "9"; "_" ] do
        Assert.True(matches fn c @"^\w$", sprintf "%s: \\w did not match %s" fn c)
    for c in [ "é"; "ß"; "а"; "٠"; "K"; "-" ] do
        Assert.False(matches fn c @"\w", sprintf "%s: \\w matched U+%04X" fn (int c.[0]))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-522")>]
[<MemberData(nameof regexFunctions)>]
let ``JYRO-REQ-522 Regex_OtherEcmaScriptMeanings_Hold`` (fn: string) =
    // \b: a non-ASCII letter is not a word character
    Assert.False(matches fn "é" @"\b")
    Assert.True(matches fn "a" @"\b")
    // \s includes U+FEFF and U+00A0
    Assert.True(matches fn "﻿" @"^\s$")
    Assert.True(matches fn " " @"^\s$")
    // . matches no line terminator
    for c in [ "\n"; "\r"; " "; " " ] do
        Assert.False(matches fn c ".", sprintf "%s: . matched U+%04X" fn (int c.[0]))

let invalidPatterns : obj[] seq =
    [ for fn in [ "RegexTest"; "RegexMatch"; "RegexMatchAll"; "RegexMatchDetail" ] do
        for p in [ "(?i)abc"; "(?#note)a"; "("; "[a"; "a{2,1}"; "*a"; "(?<n>a)(?<n>b)"; "a)" ] do
            yield [| box fn; box p |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-523")>]
[<MemberData(nameof invalidPatterns)>]
let ``JYRO-REQ-523 Regex_PatternEcmaScriptRejects_FailsWithRegexInvalidPattern`` (fn: string, pattern: string) =
    expectErr MessageCode.RegexInvalidPattern (sprintf "%s(\"abc\", %s)" fn (lit pattern))
    expectErr MessageCode.RegexInvalidPattern (sprintf "%s(\"\", %s)" fn (lit pattern))
    expectErr MessageCode.RegexInvalidPattern (sprintf "%s(null, %s)" fn (lit pattern))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-524")>]
[<InlineData("ABC", "abc", false)>]
[<InlineData("abc", "ABC", false)>]
[<InlineData("abc", "^abc$", true)>]
[<InlineData("a\nb", "^b", false)>]
[<InlineData("a\nb", "a$", false)>]
[<InlineData("abc\n", "c$", false)>]
[<InlineData("a\r\nb", "a$", false)>]
[<InlineData("a\nb", "^a", true)>]
[<InlineData("a\nb", "b$", true)>]
let ``JYRO-REQ-524 RegexTest_CaseSensitive_AnchorsOnlyAtTextEnds`` (text: string, pattern: string, expected: bool) =
    Assert.Equal(expected, boolOf (sprintf "RegexTest(%s, %s)" (lit text) (lit pattern)))

// -- 91.14.6: the regex timeout ---------------------------------------------------

/// A catastrophic-backtracking call: `(a+)+$` against 40 `a` and then `b`
/// takes far longer than any timeout used here.
let slowCall (fn: string) = sprintf "%s(\"%s\", \"^(a+)+$\")" fn (String('a', 40) + "b")

let runSlow (fn: string) (configure: JyroRunner -> JyroRunner) : JyroResult<JyroValue> * float =
    timed (fun () -> Script.runWith { Script.defaults with ConfigureRunner = configure } (script "" (slowCall fn)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-525")>]
[<MemberData(nameof regexFunctions)>]
let ``JYRO-REQ-525 Regex_HostTimeoutShorterThanDefault_Applied`` (fn: string) =
    let result, seconds =
        runSlow fn (fun r -> Pending.withRegexTimeout r (TimeSpan.FromMilliseconds 50.))
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds < 0.8, sprintf "%s with a 50 ms regex timeout took %.2fs (the default is 1 s)" fn seconds)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-525")>]
[<MemberData(nameof regexFunctions)>]
let ``JYRO-REQ-525 Regex_HostTimeoutLongerThanDefault_Applied`` (fn: string) =
    let result, seconds =
        runSlow fn (fun r -> Pending.withRegexTimeout r (TimeSpan.FromSeconds 3.))
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 2.5, sprintf "%s with a 3 s regex timeout stopped after %.2fs" fn seconds)
    Assert.True(seconds < 9.0, sprintf "%s with a 3 s regex timeout took %.2fs" fn seconds)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-525")>]
[<MemberData(nameof regexFunctions)>]
let ``JYRO-REQ-525 Regex_NoHostTimeout_DefaultOneSecondApplied`` (fn: string) =
    let result, seconds = runSlow fn id
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 0.8 && seconds < 2.5, sprintf "%s with no regex timeout set stopped after %.2fs, not about 1 s" fn seconds)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-525")>]
let ``JYRO-REQ-525 Regex_ThroughJyroScriptWithNoSetting_DefaultOneSecondApplied`` () =
    let result, seconds =
        timed (fun () -> JyroScript.executeScriptWithJson (script "" (slowCall "RegexTest")) "{}")
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 0.8 && seconds < 2.5, sprintf "stopped after %.2fs, not about 1 s" seconds)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-525")>]
let ``JYRO-REQ-525 Regex_FastPatternUnderShortTimeout_NoRegexTimeout`` () =
    let result =
        Script.runWith
            { Script.defaults with ConfigureRunner = fun r -> Pending.withRegexTimeout r (TimeSpan.FromMilliseconds 50.) }
            (script "" "RegexTest(\"abc\", \"b\")")
    Assert.True(Values.asBool (resultOf result))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-526")>]
let RegexTest_EmptyTextEmptyPattern_ReturnsTrue () =
    Assert.True(boolOf "RegexTest(\"\", \"\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-527")>]
[<InlineData("RegexTest(null, \"^$\")", true)>]
[<InlineData("RegexTest(null, \"a\")", false)>]
[<InlineData("RegexTest(null, \"null\")", false)>]
[<InlineData("RegexTest(\"abc\", null)", true)>]
[<InlineData("RegexTest(null, null)", true)>]
[<InlineData("RegexTest(\"null\", null)", true)>]
[<InlineData("RegexTest(\"x\", Input.missing)", true)>]
let RegexTest_NullTextOrPattern_ReadAsEmptyString (expr: string, expected: bool) =
    Assert.Equal(expected, boolOf expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2007")>]
[<InlineData("RegexTest(123, \"^[0-9]+$\")", true)>]
[<InlineData("RegexTest(1.50, \"^1[.]5$\")", true)>]
[<InlineData("RegexTest(\"1x5\", 1.5)", true)>]
[<InlineData("RegexTest(\"1.50\", 1.50)", true)>]
// 1.50's string form is "1.5" (40.4.6): the pattern '1', any one code unit, '5'.
// "1x5" matches it, and would not match "1.50" if the scale were kept; "150"
// matches neither reading.
[<InlineData("RegexTest(\"1x5\", 1.50)", true)>]
[<InlineData("RegexTest(\"150\", 1.50)", false)>]
[<InlineData("RegexTest(true, \"^true$\")", true)>]
[<InlineData("RegexTest(\"xfalsex\", false)", true)>]
[<InlineData("RegexTest(ToNumber(\"1e30\"), \"e[+]30\")", true)>]
let ``JYRO-REQ-2007 RegexTest_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: bool) =
    Assert.Equal(expected, boolOf expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-529")>]
[<MemberData(nameof badValues)>]
let RegexTest_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexTest(%s, \"a\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexTest(\"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-530")>]
let RegexTest_UnderPureCeiling_CompilesAndRuns () =
    Assert.True(Values.asBool (evalUnderMaxEffect FunctionEffect.Pure "RegexTest(\"a\", \"a\")"))

// ============================================================================
// 91.15 RegexMatch
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-531")>]
let RegexMatch_PositionalAndNamed_ReturnStringOrNull () =
    Assert.Equal("b", str "RegexMatch(\"abc\", \"b\")")
    Assert.Equal("b", str "RegexMatch(text: \"abc\", pattern: \"b\")")
    Assert.Equal("string", typeOf "RegexMatch(\"abc\", \"b\")")
    assertNull "RegexMatch(\"abc\", \"z\")"
    Assert.Equal("null", typeOf "RegexMatch(\"abc\", \"z\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-531")>]
let RegexMatch_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "RegexMatch(\"a\")"
    expectErr MessageCode.TooManyArguments "RegexMatch(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "RegexMatch(input: \"a\", pattern: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-532")>]
[<InlineData("abc123def456", "[0-9]+", "123")>]
[<InlineData("xaab", "a|aa", "a")>]
[<InlineData("aaa", "a+?", "a")>]
[<InlineData("aaa", "a+", "aaa")>]
[<InlineData("ab12", "([a-z])([0-9])", "b1")>]
[<InlineData("bab", "a|b", "b")>]
let RegexMatch_ReturnsLeftmostMatchText (text: string, pattern: string, expected: string) =
    Assert.Equal(expected, str (sprintf "RegexMatch(%s, %s)" (lit text) (lit pattern)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-533")>]
[<InlineData("abc", "d")>]
[<InlineData("", "a")>]
[<InlineData("ABC", "abc")>]
let RegexMatch_NoMatch_ReturnsNull (text: string, pattern: string) =
    let result = run (sprintf "RegexMatch(%s, %s)" (lit text) (lit pattern))
    Assert.Empty(Script.errorCodes result)
    Assert.True(isNullValue (resultOf result))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-534")>]
[<InlineData("abc", "x*")>]
[<InlineData("", "")>]
[<InlineData("abc", "$")>]
[<InlineData("abc", "(?=b)")>]
let RegexMatch_EmptyLeftmostMatch_ReturnsEmptyStringNotNull (text: string, pattern: string) =
    let call = sprintf "RegexMatch(%s, %s)" (lit text) (lit pattern)
    Assert.Equal("", str call)
    Assert.False(boolOf (call + " == null"))
    Assert.Equal("string", typeOf call)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-535")>]
let RegexMatch_NullTextOrPattern_ReadAsEmptyString () =
    Assert.Equal("", str "RegexMatch(null, \"^$\")")
    assertNull "RegexMatch(null, \"a\")"
    assertNull "RegexMatch(null, \"null\")"
    Assert.Equal("", str "RegexMatch(\"abc\", null)")
    Assert.Equal("", str "RegexMatch(null, null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2008")>]
[<InlineData("RegexMatch(12345, \"3[0-9]\")", "34")>]
[<InlineData("RegexMatch(true, \"ru\")", "ru")>]
[<InlineData("RegexMatch(\"a1x5b\", 1.5)", "1x5")>]
[<InlineData("RegexMatch(1.50, \"[0-9.]+\")", "1.5")>]
[<InlineData("RegexMatch(\"afalse\", false)", "false")>]
let ``JYRO-REQ-2008 RegexMatch_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-537")>]
[<MemberData(nameof badValues)>]
let RegexMatch_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexMatch(%s, \"a\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexMatch(\"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-538")>]
let RegexMatch_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("a", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "RegexMatch(\"a\", \"a\")"))

// ============================================================================
// 91.16 RegexMatchAll
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-539")>]
let RegexMatchAll_PositionalAndNamed_ReturnArrayOfStrings () =
    Assert.Equal<string list>([ "1"; "2" ], strs "RegexMatchAll(\"a1b2\", \"[0-9]\")")
    Assert.Equal<string list>([ "1"; "2" ], strs "RegexMatchAll(text: \"a1b2\", pattern: \"[0-9]\")")
    Assert.Equal("array", typeOf "RegexMatchAll(\"a1b2\", \"[0-9]\")")
    Assert.Equal("array", typeOf "RegexMatchAll(\"ab\", \"[0-9]\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-539")>]
let RegexMatchAll_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "RegexMatchAll(\"a\")"
    expectErr MessageCode.TooManyArguments "RegexMatchAll(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "RegexMatchAll(input: \"a\", pattern: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-540")>]
[<InlineData("a1b22c333", "[0-9]+", "1|22|333")>]
[<InlineData("aaaa", "aa", "aa|aa")>]
[<InlineData("aaaaa", "aa", "aa|aa")>]
[<InlineData("ababa", "aba", "aba")>]
[<InlineData("xaab", "a|aa", "a|a")>]
let RegexMatchAll_EveryMatchLeftToRight_NoOverlap (text: string, pattern: string, expected: string) =
    Assert.Equal<string list>(List.ofArray (expected.Split('|')), strs (sprintf "RegexMatchAll(%s, %s)" (lit text) (lit pattern)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-541")>]
let RegexMatchAll_AfterEmptyMatch_AdvancesOneCodeUnit () =
    Assert.Equal<string list>([ "aaa"; "" ], strs "RegexMatchAll(\"aaa\", \"a*\")")
    Assert.Equal<string list>([ ""; ""; ""; "" ], strs "RegexMatchAll(\"abc\", \"\")")
    Assert.Equal<string list>([ ""; "b"; "" ], strs "RegexMatchAll(\"ab\", \"b*\")")
    Assert.Equal<string list>([ "" ], strs "RegexMatchAll(\"\", \"\")")
    Assert.Equal<string list>([ ""; ""; "" ], strsIn (surrogates ()) "RegexMatchAll(Input.pair, \"\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-542")>]
[<InlineData("abc", "d")>]
[<InlineData("", "a")>]
[<InlineData("ABC", "abc")>]
let RegexMatchAll_NoMatch_ReturnsEmptyArray (text: string, pattern: string) =
    let v = eval (sprintf "RegexMatchAll(%s, %s)" (lit text) (lit pattern))
    Assert.Empty(Values.asList v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-543")>]
let RegexMatchAll_Elements_AreWholeMatchTextWithoutGroups () =
    Assert.Equal<string list>([ "a1"; "b2" ], strs "RegexMatchAll(\"a1b2\", \"([a-z])([0-9])\")")
    Assert.Equal<string list>([ "a"; "b" ], strs "RegexMatchAll(\"ab\", \"(a)|(b)\")")
    Assert.Equal<string list>([ "2024"; "09" ], strs "RegexMatchAll(\"2024-09\", \"(?<y>[0-9]+)\")")
    Assert.Equal<string list>([ "ab" ], strs "RegexMatchAll(\"ab\", \"((a)(b))\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-544")>]
let RegexMatchAll_NullTextOrPattern_ReadAsEmptyString () =
    Assert.Empty(Values.asList (eval "RegexMatchAll(null, \"a\")"))
    Assert.Empty(Values.asList (eval "RegexMatchAll(null, \"null\")"))
    Assert.Equal<string list>([ "" ], strs "RegexMatchAll(null, \"\")")
    Assert.Equal<string list>([ ""; ""; "" ], strs "RegexMatchAll(\"ab\", null)")
    Assert.Equal<string list>([ "" ], strs "RegexMatchAll(null, null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2009")>]
[<InlineData("RegexMatchAll(1213, 1)", "1|1")>]
[<InlineData("RegexMatchAll(true, \"[a-z]\")", "t|r|u|e")>]
[<InlineData("RegexMatchAll(\"1x5 1y5\", 1.5)", "1x5|1y5")>]
[<InlineData("RegexMatchAll(1.50, \"[0-9]\")", "1|5")>]
let ``JYRO-REQ-2009 RegexMatchAll_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal<string list>(List.ofArray (expected.Split('|')), strs expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-546")>]
[<MemberData(nameof badValues)>]
let RegexMatchAll_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexMatchAll(%s, \"a\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexMatchAll(\"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-547")>]
let RegexMatchAll_UnderPureCeiling_CompilesAndRuns () =
    let v = evalUnderMaxEffect FunctionEffect.Pure "RegexMatchAll(\"a\", \"a\")"
    Assert.Equal<string list>([ "a" ], Values.asList v |> List.map Values.asString)

// ============================================================================
// 91.17 RegexMatchDetail
// ============================================================================

let detail (text: string) (pattern: string) : JyroValue =
    eval (sprintf "RegexMatchDetail(%s, %s)" (lit text) (lit pattern))

let groupsOf (v: JyroValue) : string option list =
    Values.asList (Values.prop "groups" v)
    |> List.map (fun g -> if isNullValue g then None else Some (Values.asString g))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-548")>]
let RegexMatchDetail_PositionalAndNamed_ReturnObjectOrNull () =
    Assert.Equal("object", typeOf "RegexMatchDetail(\"abc\", \"b\")")
    Assert.Equal("object", typeOf "RegexMatchDetail(text: \"abc\", pattern: \"b\")")
    Assert.Equal("b", Values.asString (Values.prop "match" (eval "RegexMatchDetail(text: \"abc\", pattern: \"b\")")))
    Assert.Equal("null", typeOf "RegexMatchDetail(\"abc\", \"z\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-548")>]
let RegexMatchDetail_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "RegexMatchDetail(\"a\")"
    expectErr MessageCode.TooManyArguments "RegexMatchDetail(\"a\", \"b\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "RegexMatchDetail(input: \"a\", pattern: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-549")>]
[<InlineData("abc", "d")>]
[<InlineData("", "a")>]
[<InlineData("ABC", "abc")>]
let RegexMatchDetail_NoMatch_ReturnsNull (text: string, pattern: string) =
    let result = run (sprintf "RegexMatchDetail(%s, %s)" (lit text) (lit pattern))
    Assert.Empty(Script.errorCodes result)
    Assert.True(isNullValue (resultOf result))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-550")>]
[<InlineData("xx12", "([0-9])([0-9])")>]
[<InlineData("abc", "b")>]
[<InlineData("abc", "x*")>]
let RegexMatchDetail_Match_HasExactlyMatchIndexGroupsInOrder (text: string, pattern: string) =
    let keys = strs (sprintf "Keys(RegexMatchDetail(%s, %s))" (lit text) (lit pattern))
    Assert.Equal<string list>([ "match"; "index"; "groups" ], keys)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-550")>]
let RegexMatchDetail_EachCall_ReturnsNewObject () =
    let pre =
        String.concat "\n"
            [ "var r1 = RegexMatchDetail(\"ab\", \"a\")"
              "r1.match = \"z\""
              "r1.index = 9"
              "r1.extra = 1"
              "var r2 = RegexMatchDetail(\"ab\", \"a\")" ]
    let r2 = evalPre pre "r2"
    Assert.Equal("a", Values.asString (Values.prop "match" r2))
    Assert.Equal(0m, Values.asDecimal (Values.prop "index" r2))
    Assert.Empty(Values.asList (Values.prop "groups" r2))
    Assert.Equal<string list>([ "match"; "index"; "groups" ], Values.asList (evalPre pre "Keys(r2)") |> List.map Values.asString)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1763")>]
[<InlineData("abc123def456", "[0-9]+", "123")>]
[<InlineData("a1b22c333", "[0-9]+", "1")>]
[<InlineData("xx12yy", "[0-9]+", "12")>]
[<InlineData("abab", "ab", "ab")>]
[<InlineData("zzbaab", "a+b|b", "b")>]
[<InlineData("xaab", "a|aa", "a")>]
[<InlineData("abc", "x*", "")>]
[<InlineData("ab12", "([a-z])([0-9])", "b1")>]
let ``JYRO-REQ-1763 RegexMatchDetail_MatchProperty_IsLeftmostMatchText`` (text: string, pattern: string, expected: string) =
    Assert.Equal(expected, Values.asString (Values.prop "match" (detail text pattern)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1764")>]
[<InlineData("abc", "a", 0)>]
[<InlineData("abcb", "b", 1)>]
[<InlineData("a1b22c333", "[0-9]+", 1)>]
[<InlineData("", "$", 0)>]
[<InlineData("😀😀", "$", 4)>]
[<InlineData("😀xy", "y", 3)>]
[<InlineData("xx12", "[0-9]+", 2)>]
[<InlineData("abc", "$", 3)>]
[<InlineData("éx", "x", 2)>]
[<InlineData("😀a", "a", 2)>]
[<InlineData("😀😀a", "a", 4)>]
let ``JYRO-REQ-1764 RegexMatchDetail_IndexProperty_IsUtf16Position`` (text: string, pattern: string, expected: int) =
    Assert.Equal(decimal expected, Values.asDecimal (Values.prop "index" (detail text pattern)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-553")>]
[<InlineData("abc", "a")>]
[<InlineData("xx12", "[0-9]+")>]
[<InlineData("abc", "$")>]
let ``JYRO-REQ-553 RegexMatchDetail_IndexProperty_IsExactNumber`` (text: string, pattern: string) =
    Assert.True(Values.isExact (Values.prop "index" (detail text pattern)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-553")>]
let ``JYRO-REQ-553 RegexMatchDetail_IndexProperty_IsExactInScript`` () =
    Assert.True(boolOf "RegexMatchDetail(\"xx12\", \"[0-9]+\").index == 2")
    Assert.Equal("2", str "ToJson(RegexMatchDetail(\"xx12\", \"[0-9]+\").index)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-554")>]
[<InlineData("2024-09", "([0-9]{4})-([0-9]{2})", "2024|09")>]
[<InlineData("2024-09", "(?<y>[0-9]{4})-([0-9]{2})", "2024|09")>]
[<InlineData("2024-09", "([0-9]{4})-(?<m>[0-9]{2})", "2024|09")>]
[<InlineData("2024-09", "(?<m>[0-9]{4})-(?<y>[0-9]{2})", "2024|09")>]
[<InlineData("ab", "((a)(b))", "ab|a|b")>]
[<InlineData("abc", "(?<x>a)(b)(?<y>c)", "a|b|c")>]
let ``JYRO-REQ-554 RegexMatchDetail_Groups_OnePerCaptureGroupInOpeningOrder`` (text: string, pattern: string, expected: string) =
    Assert.Equal<string option list>(expected.Split('|') |> Array.map Some |> List.ofArray, groupsOf (detail text pattern))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-555")>]
let RegexMatchDetail_Groups_NeverHoldWholeMatch () =
    Assert.Equal<string option list>([ Some "a" ], groupsOf (detail "ab" "(a)b"))
    Assert.Equal<string option list>([ Some "a"; Some "b" ], groupsOf (detail "xab" "(a)(b)"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1765")>]
[<InlineData("abc", "b")>]
[<InlineData("abc", "(?:b)")>]
[<InlineData("abc", "b(?=c)")>]
[<InlineData("abc", "x*")>]
[<InlineData("abc", "[a-c]+")>]
[<InlineData("abc", "(?:a|b)+")>]
[<InlineData("abc", "(?<=a)b")>]
let ``JYRO-REQ-1765 RegexMatchDetail_NoCaptureGroups_GroupsIsEmptyArray`` (text: string, pattern: string) =
    let groups = Values.prop "groups" (detail text pattern)
    Assert.Empty(Values.asList groups)
    Assert.Equal("array", typeOf (sprintf "RegexMatchDetail(%s, %s).groups" (lit text) (lit pattern)))
    Assert.Equal(0m, Values.asDecimal (eval (sprintf "Length(RegexMatchDetail(%s, %s).groups)" (lit text) (lit pattern))))

/// The contrast: with a capture group the array is not empty.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1765")>]
let ``JYRO-REQ-1765 RegexMatchDetail_WithACaptureGroup_GroupsIsNotEmpty`` () =
    Assert.Equal(1, Values.asList (Values.prop "groups" (detail "abc" "(b)")) |> List.length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-557")>]
let ``JYRO-REQ-557 RegexMatchDetail_GroupNotParticipating_IsNull`` () =
    Assert.Equal<string option list>([ None; Some "b" ], groupsOf (detail "b" "(a)|(b)"))
    Assert.Equal<string option list>([ None ], groupsOf (detail "ab" "(x)?ab"))
    Assert.Equal<string option list>([ None ], groupsOf (detail "b" "(a)*b"))
    Assert.Equal<string option list>([ None ], groupsOf (detail "ab" "(?:(a)|b)+"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-557")>]
let ``JYRO-REQ-557 RegexMatchDetail_GroupParticipatingEmpty_IsEmptyString`` () =
    Assert.Equal<string option list>([ Some "" ], groupsOf (detail "b" "(a*)b"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-558")>]
let RegexMatchDetail_NullTextOrPattern_ReadAsEmptyString () =
    for expr in [ "RegexMatchDetail(null, \"^$\")"; "RegexMatchDetail(\"abc\", null)"; "RegexMatchDetail(null, null)" ] do
        let v = eval expr
        Assert.Equal("", Values.asString (Values.prop "match" v))
        Assert.Equal(0m, Values.asDecimal (Values.prop "index" v))
        Assert.Empty(Values.asList (Values.prop "groups" v))
    assertNull "RegexMatchDetail(null, \"a\")"
    assertNull "RegexMatchDetail(null, \"null\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2010")>]
let RegexMatchDetail_NumberOrBoolean_ReadAsStringForm () =
    let a = eval "RegexMatchDetail(12345, \"3\")"
    Assert.Equal("3", Values.asString (Values.prop "match" a))
    Assert.Equal(2m, Values.asDecimal (Values.prop "index" a))
    Assert.Equal(2m, Values.asDecimal (Values.prop "index" (eval "RegexMatchDetail(true, \"u\")")))
    let c = eval "RegexMatchDetail(\"a1x5\", 1.5)"
    Assert.Equal("1x5", Values.asString (Values.prop "match" c))
    Assert.Equal(1m, Values.asDecimal (Values.prop "index" c))
    Assert.Equal(2m, Values.asDecimal (Values.prop "index" (eval "RegexMatchDetail(1.50, \"5\")")))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-560")>]
[<MemberData(nameof badValues)>]
let RegexMatchDetail_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexMatchDetail(%s, \"a\")" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RegexMatchDetail(\"a\", %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-561")>]
let RegexMatchDetail_UnderPureCeiling_CompilesAndRuns () =
    let v = evalUnderMaxEffect FunctionEffect.Pure "RegexMatchDetail(\"a\", \"a\")"
    Assert.Equal("a", Values.asString (Values.prop "match" v))

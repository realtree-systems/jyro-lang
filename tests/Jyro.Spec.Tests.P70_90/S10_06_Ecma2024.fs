/// jyro-spec 10.6.2: every reference to ECMA-262 is to its 15th edition
/// (ECMAScript 2024), Annex B excluded. From the book and the public host API
/// alone. Every run sets the execution limits explicitly, and every run that
/// evaluates a pattern the regex timeout too (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S10_06_Ecma2024

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private regexFunctions = [ "RegexTest"; "RegexMatch"; "RegexMatchAll"; "RegexMatchDetail" ]

/// Run `source` under `boundedRegex` with `Input` = { t: text, p: pattern }.
let private runTP (text: string) (pattern: string) (source: string) =
    Script.runWith
        { boundedRegex with Input = ValueInput(Values.obj [ "t", Values.str text; "p", Values.str pattern ]) }
        source

/// The call of `fn` on Input.t and Input.p.
let private call (fn: string) = sprintf "%s(Input.t, Input.p)" fn

/// `Output.r` as JSON text.
let private rJson (result: JyroResult<JyroValue>) = Values.toJson (outProp "r" result)

// ---------------------------------------------------------------------------
// Pattern syntax: what ECMAScript 2024 accepts
// ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 Regex_Lookbehind_IsAccepted_TheBookExample`` () =
    // 10.6.2: RegexTest("ab", "(?<=a)b") MUST return true.
    let r = Script.runWith boundedRegex "Output.r = RegexTest(\"ab\", \"(?<=a)b\")"
    Assert.True(Values.asBool (outProp "r" r))

/// (function, text, pattern, the JSON of the result).
let lookbehindRows () : obj[] seq =
    [ "RegexTest", "ab", "(?<=a)b", "true"
      "RegexTest", "b", "(?<=a)b", "false"
      "RegexTest", "cb", "(?<!a)b", "true"
      "RegexTest", "ab", "(?<!a)b", "false"
      "RegexMatch", "ab", "(?<=a)b", "\"b\""
      "RegexMatch", "cb", "(?<=a)b", "null"
      "RegexMatchAll", "abcbab", "(?<=a)b", "[\"b\",\"b\"]"
      "RegexMatchAll", "cb", "(?<=a)b", "[]" ]
    |> Seq.map (fun (f, t, p, e) -> [| box f; box t; box p; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof lookbehindRows)>]
let ``JYRO-REQ-10 Regex_Lookbehind_IsAccepted`` (fn: string, text: string, pattern: string, expected: string) =
    let r = runTP text pattern ("Output.r = " + call fn)
    Assert.Equal(Values.toJson (Values.json expected), rJson r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 Regex_Lookbehind_IsAccepted_ByRegexMatchDetail`` () =
    let r = runTP "ab" "(?<=a)b" "var d = RegexMatchDetail(Input.t, Input.p)\nOutput.r = [d.match, d.index]"
    Assert.Equal("[\"b\",1]", rJson r)

let namedGroupRows () : obj[] seq =
    [ "RegexTest", "a", "(?<n>a)", "true"
      "RegexTest", "aa", @"(?<n>a)\k<n>", "true"
      "RegexTest", "ab", @"^(?<n>a)\k<n>$", "false"
      "RegexMatch", "xa", "(?<n>a)", "\"a\""
      "RegexMatchAll", "aba", "(?<n>a)", "[\"a\",\"a\"]" ]
    |> Seq.map (fun (f, t, p, e) -> [| box f; box t; box p; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof namedGroupRows)>]
let ``JYRO-REQ-10 Regex_NamedGroup_IsAccepted`` (fn: string, text: string, pattern: string, expected: string) =
    let r = runTP text pattern ("Output.r = " + call fn)
    Assert.Equal(Values.toJson (Values.json expected), rJson r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 Regex_NamedGroup_IsAccepted_ByRegexMatchDetail`` () =
    let r = runTP "xa" "(?<n>a)" "Output.r = RegexMatchDetail(Input.t, Input.p).match"
    Assert.Equal("\"a\"", rJson r)

// ---------------------------------------------------------------------------
// Pattern syntax: what ECMAScript 2024 rejects (2025 and Annex B add it)
// ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 Regex_ModifierGroup_RaisesRegexInvalidPattern_TheBookExample`` () =
    // 10.6.2: RegexTest("a", "(?i:a)") MUST raise RegexInvalidPattern.
    let r = Script.runWith boundedRegex "Output.r = RegexTest(\"a\", \"(?i:a)\")\nOutput.after = 1"
    expectRuntimeError MessageCode.RegexInvalidPattern r

/// Each of the four functions with each pattern.
let private inEachFunction (patterns: string list) : obj[] seq =
    [ for fn in regexFunctions do for p in patterns -> [| box fn; box p |] ]

let modifierGroups () = inEachFunction [ "(?i:a)"; "(?-i:a)"; "(?i-s:a)"; "(?m:a)"; "(?s:a)" ]

let private assertRejected (fn: string) (pattern: string) =
    for text in [ "a"; "A"; "b" ] do
        let r = runTP text pattern (lines [ "Output.r = " + call fn; "Output.after = 1" ])
        expectRuntimeError MessageCode.RegexInvalidPattern r
        match Script.value r with
        | Some v when (v :? JyroObject) -> Assert.False(hasKey "after" v, "the script went on after the rejected pattern")
        | _ -> ()

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof modifierGroups)>]
let ``JYRO-REQ-10 Regex_ModifierGroup_RaisesRegexInvalidPattern`` (fn: string, pattern: string) =
    assertRejected fn pattern

let duplicateNames () = inEachFunction [ "(?<n>a)|(?<n>b)"; "(?:(?<n>a)|(?<n>b))c" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof duplicateNames)>]
let ``JYRO-REQ-10 Regex_DuplicateGroupNameInAlternatives_RaisesRegexInvalidPattern`` (fn: string, pattern: string) =
    assertRejected fn pattern

let annexB () = inEachFunction [ @"\Aa"; @"\q"; "a{" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof annexB)>]
let ``JYRO-REQ-10 Regex_AnnexBSyntax_RaisesRegexInvalidPattern`` (fn: string, pattern: string) =
    assertRejected fn pattern

// ---------------------------------------------------------------------------
// ValidateSchema reads its patterns by the same edition (96.2.19)
// ---------------------------------------------------------------------------

let private schemaRun (dataJson: string) (schemaJson: string) =
    Script.runWith
        { boundedRegex with Input = JsonInput(sprintf "{\"d\": %s, \"s\": %s}" dataJson schemaJson) }
        "Output.r = ValidateSchema(Input.d, Input.s)\nOutput.after = 1"

let private patternSchema (p: string) = sprintf "{\"pattern\": %s}" (Values.toJson (Values.str p))
let private patternPropertiesSchema (p: string) = sprintf "{\"patternProperties\": {%s: {}}}" (Values.toJson (Values.str p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<InlineData("(?i:a)")>]
[<InlineData("(?-i:a)")>]
[<InlineData("(?<n>a)|(?<n>b)")>]
let ``JYRO-REQ-10 SchemaPattern_ReadsTheSameEdition_RejectsWhat2024Rejects`` (pattern: string) =
    expectRuntimeError MessageCode.RegexInvalidPattern (schemaRun "\"a\"" (patternSchema pattern))
    expectRuntimeError MessageCode.RegexInvalidPattern (schemaRun "{\"a\": 1}" (patternPropertiesSchema pattern))

/// The number of errors ValidateSchema gave in `Output.r`, the run having
/// succeeded. The path, keyword and message of an error element are 96.2's rules
/// (JYRO-WRK-13's requirements, such as JYRO-REQ-1219), so a test of this work
/// order reads only whether the data failed: none, or one per failed keyword.
let private errorCount (r: JyroResult<JyroValue>) : int =
    if not (Script.ok r) then
        Values.fail (sprintf "ValidateSchema failed. Diagnostics:\n%s" (Script.describe r))
    List.length (Values.asList (outProp "r" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<InlineData("\"ab\"", "(?<=a)b", 0)>]
[<InlineData("\"cb\"", "(?<=a)b", 1)>]
[<InlineData("\"a\"", "(?<n>a)", 0)>]
[<InlineData("\"b\"", "(?<n>a)", 1)>]
let ``JYRO-REQ-10 SchemaPattern_ReadsTheSameEdition_AcceptsWhat2024Accepts`` (data: string, pattern: string, expected: int) =
    Assert.Equal(expected, errorCount (schemaRun data (patternSchema pattern)))

// ---------------------------------------------------------------------------
// The semantics of a pattern are ECMA-262's (22.2.2), not a host engine's
// (JYRO-ADR-142; JYRO-DEF-576, JYRO-DEF-582)
// ---------------------------------------------------------------------------

/// `RegexMatchDetail(Input.t, Input.p)` as [match, index, groups], as JSON.
let private detailJson (text: string) (pattern: string) : string =
    let r = runTP text pattern "var d = RegexMatchDetail(Input.t, Input.p)\nOutput.r = d == null ? null : [d.match, d.index, d.groups]"
    rJson r

/// (function, text, pattern, the JSON of the result). ECMA-262 22.2.2.3.1
/// RepeatMatcher sets every capture inside a quantified atom to undefined at the
/// start of each iteration, so a back reference to it after an iteration that did
/// not capture matches the empty string.
let repeatMatcherRows () : obj[] seq =
    [ "RegexMatch", "aba", @"(?:(a)|b)+\1", "\"ab\""
      "RegexTest", "aba", @"^(?:(a)|b)+\1$", "false"
      "RegexTest", "ab", @"^(?:(a)|b)+\1$", "true"
      "RegexMatchAll", "abaxaba", @"(?:(a)|b)+\1", "[\"ab\",\"ab\"]"
      "RegexMatch", "aabba", @"(?:(a)|b)+\1", "\"aabb\""
      "RegexMatch", "abab", @"(?:(a)|(b))+\1\2", "null" ]
    |> Seq.map (fun (f, t, p, e) -> [| box f; box t; box p; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof repeatMatcherRows)>]
let ``JYRO-REQ-10 Regex_RepeatMatcher_ClearsCapturesOfEachIteration`` (fn: string, text: string, pattern: string, expected: string) =
    let r = runTP text pattern ("Output.r = " + call fn)
    Assert.Equal(Values.toJson (Values.json expected), rJson r)

/// (text, pattern, the JSON of [match, index, groups]), with a group that did
/// not take part in the match null (91.17). The first two are ECMA-262's own
/// NOTEs to 22.2.2.3.1.
let repeatMatcherDetailRows () : obj[] seq =
    [ "zaacbbbcac", "(z)((a+)?(b+)?(c))*", "[\"zaacbbbcac\",0,[\"z\",\"ac\",\"a\",null,\"c\"]]"
      "b", "(a*)*", "[\"\",0,[null]]"
      "baaaac", @"(a*)b\1+", "[\"b\",0,[\"\"]]"
      "aba", @"(?:(a)|b)+", "[\"aba\",0,[\"a\"]]"
      "ab", @"(?:(a)|b)+", "[\"ab\",0,[null]]"
      "abc", @"(?:(a)|(b)|c)+", "[\"abc\",0,[null,null]]"
      "aab", @"(?:(a)(b)?)+", "[\"aab\",0,[\"a\",\"b\"]]"
      "aba", @"(?:(a)(b)?)+", "[\"aba\",0,[\"a\",null]]" ]
    |> Seq.map (fun (t, p, e) -> [| box t; box p; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof repeatMatcherDetailRows)>]
let ``JYRO-REQ-10 RegexMatchDetail_RepeatMatcher_ClearsCapturesOfEachIteration`` (text: string, pattern: string, expected: string) =
    Assert.Equal(Values.toJson (Values.json expected), detailJson text pattern)

/// (text, pattern, the result of RegexTest). A lookbehind is matched backward
/// as a whole (22.2.2.4, direction backward), so a class in it, negated or
/// holding a negated class escape, tests the character before the position.
let lookbehindClassRows () : obj[] seq =
    [ "1b", @"(?<=[^\D])b", true
      "bb", @"(?<=[^\D])b", false
      "xb", @"(?<=[^\D])b", false
      " b", @"(?<=[^\S])b", true
      "ab", @"(?<=[^\S])b", false
      "_b", @"(?<=[^\W])b", true
      "-b", @"(?<=[^\W])b", false
      "1b", @"(?<=[^a\D])b", true
      "ab", @"(?<=[^a\D])b", false
      "1b", @"(?<![^\D])b", false
      "xb", @"(?<![^\D])b", true
      "a1b", @"(?<=a[^\D])b", true
      "x1b", @"(?<=a[^\D])b", false
      "2b", @"(?<=[^\D1])b", true
      "1b", @"(?<=[^\D1])b", false
      "1b", @"(?<=[^\W_])b", true
      "_b", @"(?<=[^\W_])b", false
      " b", @"(?<=[^\S\n])b", true
      "\nb", @"(?<=[^\S\n])b", false
      "12b", @"(?<=[^\D][^\D])b", true
      "1xb", @"(?<=[^\D][^\D])b", false
      "x1b", @"(?<=[^\D][^\D])b", false
      "xb", @"(?<=[\Da])b", true
      "ab", @"(?<=[\Da])b", true
      "1b", @"(?<=[\Da])b", false
      "1b", @"(?<=[\d])b", true
      "1b", @"(?<=[^x])b", true
      "xb", @"(?<=[^x])b", false ]
    |> Seq.map (fun (t, p, e) -> [| box t; box p; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof lookbehindClassRows)>]
let ``JYRO-REQ-10 Regex_ClassInALookbehind_TestsTheCharacterBefore`` (text: string, pattern: string, expected: bool) =
    let r = runTP text pattern ("Output.r = " + call "RegexTest")
    Assert.Equal(expected, Values.asBool (outProp "r" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 Regex_CapturesInALookbehind_AreMatchedBackward`` () =
    // ECMA-262 22.2.2.4: in a lookbehind the right-hand atom is matched first.
    let expect (json: string) = Values.toJson (Values.json json)
    Assert.Equal(expect "[\"\",4,[\"1\",\"053\"]]", detailJson "1053" @"(?<=(\d+)(\d+))$")
    Assert.Equal(expect "[\"b\",3,[\"a\"]]", detailJson "aaab" @"(?<=\1(a))b")
    Assert.Equal(expect "null", detailJson "abb" @"(?<=\1(a))b")

// ---------------------------------------------------------------------------
// ValidateSchema reads a keyword only where the schema has one (JYRO-ADR-144;
// JYRO-DEF-579, JYRO-DEF-580)
// ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 SchemaPattern_APropertyNamedPatternProperties_IsASubschemaNotPatterns`` () =
    // The member of `properties` named patternProperties holds a subschema; its
    // keys are keywords, not patterns, so `type` keeps its meaning.
    let schema = """{"properties": {"patternProperties": {"type": "number"}}}"""
    Assert.Equal(1, errorCount (schemaRun """{"patternProperties": "x"}""" schema))
    Assert.Equal(0, errorCount (schemaRun """{"patternProperties": 1}""" schema))
    // The same under $defs, reached by $ref.
    let defs = """{"$defs": {"patternProperties": {"type": "number"}}, "properties": {"a": {"$ref": "#/$defs/patternProperties"}}}"""
    Assert.Equal(1, errorCount (schemaRun """{"a": "x"}""" defs))
    Assert.Equal(0, errorCount (schemaRun """{"a": 1}""" defs))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<InlineData("default")>]
[<InlineData("enum")>]
[<InlineData("const")>]
[<InlineData("examples")>]
let ``JYRO-REQ-10 SchemaPattern_UnderAPropertyNamedAfterADataKeyword_ReadsTheSameEdition`` (name: string) =
    // A member of `properties` named default, enum, const or examples holds a
    // subschema, and its pattern is an ECMAScript pattern like any other: \d is
    // [0-9] only, so U+0663 ARABIC-INDIC DIGIT THREE fails it.
    let schema = sprintf """{"properties": {"%s": {"type": "string", "pattern": "^\\d$"}}}""" name
    let arabicThree = "\\u0663" // JSON text for U+0663
    Assert.Equal(1, errorCount (schemaRun (sprintf """{"%s": "%s"}""" name arabicThree) schema))
    Assert.Equal(0, errorCount (schemaRun (sprintf """{"%s": "3"}""" name) schema))
    // A pattern there that ECMAScript 2024 refuses raises RegexInvalidPattern.
    for bad in [ @"\Aa"; "(?i:a)" ] do
        let refused = sprintf """{"properties": {"%s": {"pattern": %s}}}""" name (Values.toJson (Values.str bad))
        expectRuntimeError MessageCode.RegexInvalidPattern (schemaRun (sprintf """{"%s": "a"}""" name) refused)
    // And its patternProperties too.
    let pp = sprintf """{"properties": {"%s": {"patternProperties": {"^\\d$": {"type": "number"}}}}}""" name
    Assert.Equal(0, errorCount (schemaRun (sprintf """{"%s": {"%s": "x"}}""" name arabicThree) pp))
    Assert.Equal(1, errorCount (schemaRun (sprintf """{"%s": {"3": "x"}}""" name) pp))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 SchemaPattern_InsideTheValueOfADataKeyword_IsNotAPattern`` () =
    // The value of enum, const, default and examples is data (JYRO-ADR-144): an
    // object there with a member named pattern is not a pattern, so a string that
    // ECMAScript 2024 refuses is not refused.
    let value = """{"pattern": "(?i:a)"}"""
    for schema in [ sprintf """{"enum": [%s]}""" value
                    sprintf """{"const": %s}""" value
                    sprintf """{"default": %s}""" value
                    sprintf """{"examples": [%s]}""" value ] do
        let r = schemaRun value schema
        Assert.Equal(0, errorCount r)

// ---------------------------------------------------------------------------
// The string form of an inexact number is Number::toString (40.4.7)
// ---------------------------------------------------------------------------

/// Host doubles (made inexact on the host side) and their Number::toString form.
let hostNumbers () : obj[] seq =
    [ 1e21, "1e+21"
      -1e21, "-1e+21"
      1e-7, "1e-7"
      1.5e-7, "1.5e-7"
      1e20, "100000000000000000000"
      123456789012345680000.0, "123456789012345680000"
      1e-6, "0.000001"
      1.23e-18, "1.23e-18"
      5e-324, "5e-324"
      1.7976931348623157e308, "1.7976931348623157e+308"
      0.1 + 0.2, "0.30000000000000004"
      1.0 / 3.0, "0.3333333333333333"
      100.0, "100"
      1.5, "1.5" ]
    |> Seq.map (fun (d, s) -> [| box d; box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof hostNumbers)>]
let ``JYRO-REQ-10 StringForm_ReadsNumberToString_OfAHostInexactNumber`` (d: float, expected: string) =
    let r =
        Script.runWith
            { bounded with Input = ValueInput(Values.obj [ "n", Values.inexact d ]) }
            (lines [ "Output.n = Input.n"; "Output.s = \"\" & Input.n"; "Output.t = ToString(Input.n)" ])
    Assert.False(Values.isExact (outProp "n" r), sprintf "the host number %A arrived exact, so the row does not test 40.4.7" d)
    Assert.Equal(expected, Values.asString (outProp "s" r))
    Assert.Equal(expected, Values.asString (outProp "t" r))

/// Script numbers made inexact by multiplying by `Power(1, 0.5)`, an inexact 1
/// (93.7.5), so the product is the binary64 of the literal (30.2.10). (Adding
/// 1e-29 was the earlier way; whether such a sum is exact is 30.2.6's rule, of
/// another work order, JYRO-REQ-49, so it no longer decides a row here.)
[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<InlineData("1e21", "1e+21")>]
[<InlineData("-1e21", "-1e+21")>]
[<InlineData("1e-7", "1e-7")>]
[<InlineData("1e20", "100000000000000000000")>]
[<InlineData("0.000001", "0.000001")>]
[<InlineData("123456789012345678901", "123456789012345680000")>]
[<InlineData("1e300", "1e+300")>]
[<InlineData("1.5", "1.5")>]
let ``JYRO-REQ-10 StringForm_ReadsNumberToString_OfAScriptInexactNumber`` (literal: string, expected: string) =
    let r =
        Script.runWith bounded
            (lines [ sprintf "var n = %s * Power(1, 0.5)" literal
                     "Output.n = n"; "Output.s = \"\" & n"; "Output.t = ToString(n)" ])
    Assert.False(Values.isExact (outProp "n" r), sprintf "%s * Power(1, 0.5) is exact, so the row does not test 40.4.7" literal)
    Assert.Equal(expected, Values.asString (outProp "s" r))
    Assert.Equal(expected, Values.asString (outProp "t" r))

// ---------------------------------------------------------------------------
// ToJson writes strings as QuoteJSONString does (95.14.3)
// ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 ToJson_ReadsQuoteJSONString`` () =
    let s = "a b\u001Fc /\u007F\u0008"
    let r =
        Script.runWith
            { bounded with Input = ValueInput(Values.obj [ "s", Values.str s ]) }
            "Output.r = ToJson(Input.s)\nvar o = {}\no[Input.s] = 1\nOutput.k = ToJson(o)"
    let body = "a b\\u001fc /\u007F\\b"
    Assert.Equal("\"" + body + "\"", Values.asString (outProp "r" r))
    Assert.Equal("{\"" + body + "\":1}", Values.asString (outProp "k" r))

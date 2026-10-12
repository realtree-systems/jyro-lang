/// jyro-spec 90.10 (regular expressions), 90.11 (JSON), 90.12 (Sleep) and
/// 90.13 (optional parameters). From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S90_10_13_RegexJsonSleepOptional

open System
open System.Diagnostics
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let rows (xs: 'a list) : obj[] seq = xs |> Seq.map (fun x -> [| box x |])

// ===========================================================================
// 90.10 Regular expressions
// ===========================================================================

let regexFunctions = [ "RegexTest"; "RegexMatch"; "RegexMatchAll"; "RegexMatchDetail" ]

/// A Jyro expression that is true when `fn` found a match of Input.p in Input.t.
let matched (fn: string) =
    match fn with
    | "RegexTest" -> "RegexTest(Input.t, Input.p)"
    | "RegexMatch" -> "RegexMatch(Input.t, Input.p) != null"
    | "RegexMatchAll" -> "Length(RegexMatchAll(Input.t, Input.p)) > 0"
    | _ -> "RegexMatchDetail(Input.t, Input.p) != null"

let runRegex (options: RunOptions) (text: string) (pattern: string) (source: string) =
    Script.runWith { options with Input = ValueInput(Values.obj [ "t", Values.str text; "p", Values.str pattern ]) } source

let ecmaCases () : obj[] seq =
    [ for fn in regexFunctions do
          for (t, p, e) in
              [ "٣", @"\d", false
                "５", @"\d", false
                "3", @"^\d$", true
                "é", @"\w", false
                "ß", @"\w", false
                "a", @"^\w$", true
                "aé", @"a\b", true
                "A", "a", false
                "\n", ".", false
                "\u2028", ".", false
                "a\nb", "^b", false
                "a\n", "a$", false
                "\n", "[^]", true
                "﻿", @"^\s$", true
                "x", "(?<n>x)", true
                "ab", "(?<=a)b", true
                "ab", "^ab$", true ] -> [| box fn; box t; box p; box e |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-363")>]
[<MemberData(nameof ecmaCases)>]
let ``JYRO-REQ-363 RegexFunction_ReadsThePatternAsECMAScriptWithNoFlags`` (fn: string, text: string, pattern: string, expected: bool) =
    let r = runRegex boundedRegex text pattern ("Output.m = " + matched fn)
    Assert.Equal(expected, Values.asBool (outProp "m" r))

let nonEcmaPatterns () : obj[] seq =
    [ for fn in regexFunctions do for p in [ "(?i)a"; "(?#c)a"; "a++"; "(?>a)" ] -> [| box fn; box p |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-363")>]
[<MemberData(nameof nonEcmaPatterns)>]
let ``JYRO-REQ-363 RegexFunction_GivenSyntaxThatIsNotECMAScript_RaisesRegexInvalidPattern`` (fn: string, pattern: string) =
    expectRuntimeError MessageCode.RegexInvalidPattern (runRegex boundedRegex "a" pattern ("Output.m = " + matched fn))

/// 90.10.1 with 10.6.2: the 15th edition (ECMAScript 2024). Lookbehind and
/// named groups are in it; the modifier group and duplicate names in
/// alternatives (added by ECMAScript 2025) are not.
let edition2024Accepted () : obj[] seq =
    [ for fn in regexFunctions do
          for (t, p, e) in
              [ "ab", "(?<=a)b", true
                "cb", "(?<=a)b", false
                "cb", "(?<!a)b", true
                "a", "(?<n>a)", true
                "aa", @"(?<n>a)\k<n>", true ] -> [| box fn; box t; box p; box e |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-363")>]
[<MemberData(nameof edition2024Accepted)>]
let ``JYRO-REQ-363 RegexFunction_ReadsThe2024Edition_Accepts`` (fn: string, text: string, pattern: string, expected: bool) =
    let r = runRegex boundedRegex text pattern ("Output.m = " + matched fn)
    Assert.Equal(expected, Values.asBool (outProp "m" r))

let edition2024Rejected () : obj[] seq =
    [ for fn in regexFunctions do
          for p in [ "(?i:a)"; "(?-i:a)"; "(?i-s:a)"; "(?<n>a)|(?<n>b)" ] -> [| box fn; box p |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-363")>]
[<MemberData(nameof edition2024Rejected)>]
let ``JYRO-REQ-363 RegexFunction_ReadsThe2024Edition_Rejects`` (fn: string, pattern: string) =
    for text in [ "a"; "A" ] do
        expectRuntimeError MessageCode.RegexInvalidPattern (runRegex boundedRegex text pattern ("Output.m = " + matched fn))

/// No flag applies: not i, not m, not s, not u or v (90.10.1, 10.6.2).
let noFlagCases () : obj[] seq =
    [ for fn in regexFunctions do
          for (t, p, e) in
              [ "A", "a", false             // no i
                "a", "A", false             // no i
                "a\nb", "^b", false         // no m
                "a\nb", "a$", false         // no m
                "a\u2028b", "^b", false     // no m, other line terminator
                "\n", ".", false            // no s
                "\r", ".", false            // no s
                "\u2028", ".", false        // no s
                "\u2029", ".", false        // no s
                "٣", @"\d", false      // no u: \d is ASCII
                "٣", @"^\D$", true
                "é", @"\w", false      // \w is ASCII
                "K", @"\w", false      // KELVIN SIGN: no i and u together
                "a\nb", @"^a\nb$", true ] -> [| box fn; box t; box p; box e |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-363")>]
[<MemberData(nameof noFlagCases)>]
let ``JYRO-REQ-363 RegexFunction_NoFlags`` (fn: string, text: string, pattern: string, expected: bool) =
    let r = runRegex boundedRegex text pattern ("Output.m = " + matched fn)
    Assert.Equal(expected, Values.asBool (outProp "m" r))

/// `\s` reads ECMA-262 WhiteSpace and LineTerminator, and so Zs of Unicode
/// 15.0.0 (10.6.1): U+3000 is Zs; U+180E has been Cf since Unicode 6.3.
let whiteSpaceClassCases () : obj[] seq =
    [ for fn in regexFunctions do
          for (t, p, e) in
              [ "　", @"^\s$", true
                " ", @"^\s$", true
                " ", @"^\s$", true
                "᠎", @"\s", false
                "​", @"\s", false
                "᠎", @"^\S$", true
                "　", @"\S", false ] -> [| box fn; box t; box p; box e |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-363")>]
[<MemberData(nameof whiteSpaceClassCases)>]
let ``JYRO-REQ-363 RegexFunction_WhiteSpaceClass_ReadsZsOfUnicode15_0`` (fn: string, text: string, pattern: string, expected: bool) =
    let r = runRegex boundedRegex text pattern ("Output.m = " + matched fn)
    Assert.Equal(expected, Values.asBool (outProp "m" r))

// 90.10.5: syntax that only ECMA-262 Annex B.1.2 adds is refused.
let annexBPatterns () : obj[] seq =
    [ for fn in regexFunctions do
          for p in [ @"\Aa"; @"a\Z"; @"\q"; "a]"; "a{"; @"\8"; @"\01"; "a{1"; "}"; "]"; @"\c"; @"a\z" ] -> [| box fn; box p |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-367")>]
[<MemberData(nameof annexBPatterns)>]
let ``JYRO-REQ-367 RegexFunction_GivenAPatternValidOnlyUnderAnnexB_RaisesRegexInvalidPattern`` (fn: string, pattern: string) =
    for text in [ "Aa"; "a"; "q]" ] do
        Script.expectError MessageCode.RegexInvalidPattern (runRegex Script.defaults text pattern ("Output.m = " + matched fn))

let standardForms () : obj[] seq =
    [ for fn in regexFunctions do
          for (t, p, e) in
              [ "a", "^a", true
                "a", "a$", true
                "7", @"\d", true
                "a]", @"a\]", true
                "a{", @"a\{", true
                "aa", @"(a)\1", true
                "a", "a{1}", true
                "\u0000", @"\0", true ] -> [| box fn; box t; box p; box e |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-367")>]
[<MemberData(nameof standardForms)>]
let ``JYRO-REQ-367 RegexFunction_GivenTheStandardFormOfTheSameConstruct_AcceptsIt`` (fn: string, text: string, pattern: string, expected: bool) =
    let r = runRegex Script.defaults text pattern ("Output.m = " + matched fn)
    Assert.Equal(expected, Values.asBool (outProp "m" r))

let invalidPatterns () : obj[] seq =
    [ for fn in regexFunctions do
          for p in [ "("; ")"; "[a"; "*"; "a**"; "+a"; "a{2,1}"; "(?i)a"; "(?<n>a)(?<n>b)"; "\\" ] -> [| box fn; box p |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-364")>]
[<MemberData(nameof invalidPatterns)>]
let ``JYRO-REQ-364 RegexFunction_GivenAnInvalidPattern_RaisesRegexInvalidPattern`` (fn: string, pattern: string) =
    for text in [ "a"; ""; "(ab)" ] do
        Script.expectError MessageCode.RegexInvalidPattern (runRegex Script.defaults text pattern ("Output.m = " + matched fn))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-364")>]
[<InlineData("RegexTest")>]
[<InlineData("RegexMatch")>]
[<InlineData("RegexMatchAll")>]
[<InlineData("RegexMatchDetail")>]
let ``JYRO-REQ-364 RegexFunction_GivenAValidPattern_DoesNotRaiseRegexInvalidPattern`` (fn: string) =
    for p in [ "a"; "(a)"; "[a-z]+"; "a{1,2}"; "\\("; "" ] do
        let r = runRegex Script.defaults "a(b" p ("Output.m = " + matched fn)
        Assert.True(r.IsSuccess, sprintf "%s: %s" p (Script.describe r))

let private catastrophicText = String('a', 40) + "!"
let private catastrophicPattern = "^(a+)+$"

/// Run in the background with a 10 s guard, and give the result and the elapsed time.
let timed (work: unit -> JyroResult<JyroValue>) =
    let sw = Stopwatch.StartNew()
    let r = await (background work)
    sw.Stop()
    r, sw.Elapsed

let withTimeout (ms: float) =
    { Script.defaults with ConfigureRunner = fun runner -> Pending.withRegexTimeout runner (TimeSpan.FromMilliseconds ms) }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-365")>]
[<InlineData("RegexTest")>]
[<InlineData("RegexMatch")>]
[<InlineData("RegexMatchAll")>]
[<InlineData("RegexMatchDetail")>]
let ``JYRO-REQ-365 RegexFunction_WhenOneEvaluationExceedsTheTimeout_StopsAndRaisesRegexTimeout`` (fn: string) =
    let r, elapsed =
        timed (fun () ->
            runRegex (withTimeout 50.0) catastrophicText catastrophicPattern (lines [ "Output.m = " + matched fn; "Output.after = true" ]))
    Script.expectError MessageCode.RegexTimeout r
    match Script.value r with
    | Some v when (v :? JyroObject) -> Assert.False(hasKey "after" v)
    | _ -> ()
    Assert.True(elapsed < TimeSpan.FromSeconds 5.0, sprintf "took %O" elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-365")>]
let ``JYRO-REQ-365 RegexTimeout_AppliesPerEvaluationNotToTheirTotal`` () =
    let r, _ =
        timed (fun () ->
            Script.runWith
                { withTimeout 50.0 with
                    Input = ValueInput(Values.obj [ "t", Values.str (String('a', 2000)) ])
                    Limits = Some(Script.limits (fun o -> { o with MaxLoopIterations = 100000; MaxStatements = 100000 })) }
                (lines [ "var i = 0"; "var n = 0"
                         "while i < 200 do"
                         "  if RegexTest(Input.t, \"(a|b)*c?$\") then n = n + 1 end"
                         "  i = i + 1"
                         "end"
                         "Output.n = n" ]))
    Assert.Equal(200.0, Values.asFloat (outProp "n" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-366")>]
let ``RegexTimeout_WhenTheHostSetsNone_IsOneSecond`` () =
    let r, elapsed = timed (fun () -> runRegex Script.defaults catastrophicText catastrophicPattern "Output.m = RegexTest(Input.t, Input.p)")
    Script.expectError MessageCode.RegexTimeout r
    Assert.True(elapsed >= TimeSpan.FromSeconds 0.9, sprintf "timed out after only %O" elapsed)
    Assert.True(elapsed < TimeSpan.FromSeconds 3.0, sprintf "timed out only after %O" elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-366")>]
let ``RegexTimeout_ThroughJyroScriptWithNoTimeoutSet_IsOneSecond`` () =
    let r, elapsed =
        timed (fun () ->
            JyroScript.executeScriptWithJson "Output.m = RegexTest(Input.t, Input.p)"
                (sprintf "{\"t\":\"%s\",\"p\":\"%s\"}" catastrophicText catastrophicPattern))
    Script.expectError MessageCode.RegexTimeout r
    Assert.True(elapsed >= TimeSpan.FromSeconds 0.9 && elapsed < TimeSpan.FromSeconds 3.0, sprintf "%O" elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-366")>]
let ``RegexTimeout_WhenTheHostSetsNone_AFastEvaluationDoesNotTimeOut`` () =
    let r = runRegex Script.defaults (String('a', 100000)) "a+$" "Output.m = RegexTest(Input.t, Input.p)"
    Assert.True(Values.asBool (outProp "m" r))

// ===========================================================================
// 90.11 JSON
// ===========================================================================

let fromJsonCases () : obj[] seq =
    [ "null", Values.nul
      "true", Values.bool true
      "false", Values.bool false
      "0", Values.exact 0m
      "-0", Values.exact 0m
      "1.5e3", Values.exact 1500m
      "1E-2", Values.exact 0.01m
      "\"s\"", Values.str "s"
      "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0041\\ud83d\\ude00\"", Values.str "\"\\/\b\f\n\r\tA\U0001F600"
      "[]", Values.arr []
      "{}", Values.emptyObj ()
      " \t\n\r[ 1 ,\t2 ]\r\n", Values.arr [ Values.exact 1m; Values.exact 2m ]
      "{\"a\":[1,{\"b\":null}],\"c\":{}}",
      Values.obj [ "a", Values.arr [ Values.exact 1m; Values.obj [ "b", Values.nul ] ]; "c", Values.emptyObj () ]
      "123456789012345678901234567890", Values.inexact 123456789012345678901234567890.0 ]
    |> Seq.map (fun (t, v) -> [| box t; box v |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1829")>]
[<MemberData(nameof fromJsonCases)>]
let ``JYRO-REQ-1829 FromJson_ReadsRfc8259JsonAsTheValueItDescribes`` (text: string, expected: JyroValue) =
    let v = outProp "r" (Script.runInput (Values.obj [ "t", Values.str text ]) "Output.r = FromJson(Input.t)")
    Assert.True(v.EqualsValue expected, sprintf "FromJson(%A) gave %s" text (Values.toJson v))
    Assert.Equal(expected.ValueType, v.ValueType)
    match expected with
    | :? JyroNumber as n -> Assert.Equal(n.IsExact, Values.isExact v)
    | _ -> ()

/// The statement's own example: an object whose property `a` is `[1, true, null]`
/// and whose property `b` is `"x"`, in that order.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1829")>]
let ``JYRO-REQ-1829 FromJson_StatementExample_IsTheObjectWithAThenB`` () =
    let r =
        Script.run
            "var v = FromJson(\"{\\\"a\\\": [1, true, null], \\\"b\\\": \\\"x\\\"}\")\nOutput.v = v\nOutput.keys = Keys(v)"
    Assert.True(r.IsSuccess, Script.describe r)
    let expected = Values.obj [ "a", Values.arr [ Values.exact 1m; Values.bool true; Values.nul ]; "b", Values.str "x" ]
    Assert.True((outProp "v" r).EqualsValue expected, Values.toJson (outProp "v" r))
    Assert.Equal<string list>([ "a"; "b" ], Values.asList (outProp "keys" r) |> List.map Values.asString)

/// 95.13.2 to 95.13.7: the property order is kept for a longer object, a nested
/// array and a nested object.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1829")>]
let ``JYRO-REQ-1829 FromJson_KeepsElementAndPropertyOrder`` () =
    let r =
        Script.runInput (Values.obj [ "t", Values.str "{\"z\": [3, 1, 2], \"a\": {\"y\": 1, \"b\": 2}, \"m\": null}" ])
            "var v = FromJson(Input.t)\nOutput.k = Keys(v)\nOutput.n = Keys(v.a)\nOutput.e = v.z"
    Assert.Equal<string list>([ "z"; "a"; "m" ], Values.asList (outProp "k" r) |> List.map Values.asString)
    Assert.Equal<string list>([ "y"; "b" ], Values.asList (outProp "n" r) |> List.map Values.asString)
    Assert.Equal<string>("[3,1,2]", Values.toJson (outProp "e" r))

/// The texts that JYRO-REQ-1172, 1179, 1726 and 1658 refuse give `null`, with no
/// error, and that of JYRO-REQ-1659 raises.
let private fromJsonOf (text: string) = Script.runInput (Values.obj [ "t", Values.str text ]) "Output.r = FromJson(Input.t)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1829")>]
[<InlineData("1e999")>]
[<InlineData("-1e999")>]
[<InlineData("[1e400]")>]
[<InlineData("\"\\ud800\"")>]
[<InlineData("\"a\\udc00\"")>]
[<InlineData("\"\\ud800x\"")>]
[<InlineData("[1,")>]
[<InlineData("{\"a\": ")>]
[<InlineData("x")>]
[<InlineData("")>]
[<InlineData("\"unterminated")>]
let ``JYRO-REQ-1829 FromJson_OfATextThatTheOtherClausesRefuse_ReturnsNullWithoutError`` (text: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull, Values.toJson (outProp "r" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1829")>]
let ``JYRO-REQ-1829 FromJson_OfARawUnpairedSurrogate_ReturnsNull`` () =
    for text in [ "\"a" + string (char 0xD800) + "\""; string (char 0xDC00); "{\"" + string (char 0xD800) + "\": 1}" ] do
        let r = fromJsonOf text
        Assert.True(r.IsSuccess, Script.describe r)
        Assert.True((outProp "r" r).IsNull, Values.toJson (outProp "r" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1829")>]
let ``JYRO-REQ-1829 FromJson_OfTextNestedBeyondTheBound_RaisesValueNestingLimitExceeded`` () =
    Script.expectError MessageCode.ValueNestingLimitExceeded (fromJsonOf (String('[', 100000)))

/// A text that would be JSON but for a trailing comma or a comment is read as
/// the JSON text without them: equal to the value JYRO-REQ-1170 gives for it.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("[1, 2,]", "[1,2]")>]
[<InlineData("{\"a\": 1,}", "{\"a\":1}")>]
[<InlineData("[[1,],]", "[[1]]")>]
[<InlineData("{\"a\": [1,],}", "{\"a\":[1]}")>]
[<InlineData("[1 /* one */]", "[1]")>]
[<InlineData("[1 /* c */, 2]", "[1,2]")>]
[<InlineData("[1,/*x*/2]", "[1,2]")>]
[<InlineData("/* a */[1]/* b */", "[1]")>]
[<InlineData("{\"a\"/*c*/:/*d*/1}", "{\"a\":1}")>]
[<InlineData("{\"a\"/*c*/:1}", "{\"a\":1}")>]
[<InlineData("{\"a\":/*d*/1}", "{\"a\":1}")>]
[<InlineData("{/*c*/\"a\":1}", "{\"a\":1}")>]
[<InlineData("{\"a\":1/*c*/}", "{\"a\":1}")>]
[<InlineData("{\"a\":1/*c*/,\"b\":2}", "{\"a\":1,\"b\":2}")>]
[<InlineData("{\"a\":1,/*c*/\"b\":2}", "{\"a\":1,\"b\":2}")>]
[<InlineData("{\"a\"// c\n:1}", "{\"a\":1}")>]
[<InlineData("// c\n[1]", "[1]")>]
[<InlineData("// c\r[1]", "[1]")>]
[<InlineData("// c\r\n[1]", "[1]")>]
[<InlineData("[1] // at the end of text", "[1]")>]
[<InlineData("[1, 2, // x\n]", "[1,2]")>]
[<InlineData("[1, // x\r2]", "[1,2]")>]
[<InlineData("[\"//\", \"/*\", \"*/\"]", "[\"//\",\"/*\",\"*/\"]")>]
[<InlineData("{\"a,]\": \"},\"}", "{\"a,]\":\"},\"}")>]
[<InlineData("[1 /* a */ /* b */ ,]", "[1]")>]
[<InlineData("[1 /* // not a line comment */]", "[1]")>]
[<InlineData("[1] /* it ends here */", "[1]")>]
let ``JYRO-REQ-1869 FromJson_WithATrailingCommaOrComment_ReadsItAsTheJsonWithoutIt`` (text: string, meaning: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    let v = outProp "r" r
    Assert.True(v.EqualsValue(Values.json meaning), sprintf "FromJson(%A) gave %s" text (Values.toJson v))

/// A `//` comment ends at the next LF or CR, not before: the text after it is read.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1869")>]
let ``JYRO-REQ-1869 FromJson_LineComment_EndsAtLfOrCrAndNotAtAnyOtherCharacter`` () =
    let read (text: string) = outProp "r" (fromJsonOf text)
    Assert.True((read "[1 // a ] b\n]").EqualsValue(Values.json "[1]"))
    Assert.True((read "[1 // a ] b\r]").EqualsValue(Values.json "[1]"))
    // U+0085, U+2028 and U+2029 do not end a comment: the closing bracket is still in it
    // (U+2028 and U+2029 also make the text one that FromJson cannot interpret, 1869)
    for c in [ "\u0085"; "\u2028"; "\u2029" ] do
        Assert.True((read ("[1 // a " + c + "]")).IsNull, sprintf "U+%04X ended the comment" (int c.[0]))

/// A `/*` with no later `*/`, two commas in a row, and a comma in an otherwise
/// empty array or object make a text that FromJson cannot interpret (1658).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("[1,,]")>]
[<InlineData("[,]")>]
[<InlineData("{,}")>]
[<InlineData("{\"a\": 1,,}")>]
[<InlineData("[1,,2]")>]
[<InlineData("[1, 2,,]")>]
[<InlineData("[1 /* x]")>]
[<InlineData("/*")>]
[<InlineData("/* never closed")>]
[<InlineData("[1] /*")>]
[<InlineData("[1, /* a */ , 2]")>]
let ``JYRO-REQ-1869 FromJson_OfAnUninterpretableTextWithCommasOrComments_ReturnsNull`` (text: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull, sprintf "FromJson(%A) gave %s" text (Values.toJson (outProp "r" r)))

/// A comment only where RFC 8259 permits white space: not inside a literal, a
/// number or a string key/value token.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("tr/*c*/ue")>]
[<InlineData("nu/*c*/ll")>]
[<InlineData("-/*c*/1")>]
[<InlineData("1/*c*/2")>]
[<InlineData("[1 / * c * /]")>]
[<InlineData("[1 / /c\n]")>]
let ``JYRO-REQ-1869 FromJson_CommentWhereWhiteSpaceIsNotPermitted_ReturnsNull`` (text: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull, sprintf "FromJson(%A) gave %s" text (Values.toJson (outProp "r" r)))

/// A comment at every other place RFC 8259 permits white space: inside an empty
/// array or object, straight after the opening bracket or brace, after a
/// trailing comma, and between a key that ends in an escaped quote or a
/// backslash (or holds comment openers) and its colon.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("[/*c*/]", "[]")>]
[<InlineData("{/*c*/}", "{}")>]
[<InlineData("[// c\n]", "[]")>]
[<InlineData("{// c\r}", "{}")>]
[<InlineData("[ /* a */ /* b */ ]", "[]")>]
[<InlineData("[/*c*/1]", "[1]")>]
[<InlineData("[// c\n1, 2]", "[1,2]")>]
[<InlineData("{\"a\":1,/*c*/}", "{\"a\":1}")>]
[<InlineData("{\"a\":1,// c\n}", "{\"a\":1}")>]
[<InlineData("{\"a\":1, /* c */ // d\n}", "{\"a\":1}")>]
[<InlineData("[1,/*c*/]", "[1]")>]
[<InlineData("{\"a\\\"\"/*c*/:1}", "{\"a\\\"\":1}")>]
[<InlineData("{\"a\\\\\"/*c*/:1}", "{\"a\\\\\":1}")>]
[<InlineData("{\"a\\\\\"// c\n:1}", "{\"a\\\\\":1}")>]
[<InlineData("{\"a\\\\\\\"\"/*c*/:1}", "{\"a\\\\\\\"\":1}")>]
[<InlineData("{\"a/*\"/*c*/:1}", "{\"a/*\":1}")>]
[<InlineData("{\"//\"/*c*/:1}", "{\"//\":1}")>]
[<InlineData("{\"a\":1,\"b\"/*c*/:2}", "{\"a\":1,\"b\":2}")>]
let ``JYRO-REQ-1869 FromJson_CommentInAnyWhiteSpacePosition_ReadsItAsNothing`` (text: string, meaning: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    let v = outProp "r" r
    Assert.True(v.EqualsValue(Values.json meaning), sprintf "FromJson(%A) gave %s" text (Values.toJson v))

/// A `//` comment that holds U+2028 or U+2029 anywhere between its `//` and the LF, CR or end
/// of text that ends it makes the text one FromJson cannot interpret (JYRO-ADR-107, which
/// JYRO-REQ-1869 states): before a colon, whichever of LF and CR ends the comment.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("{\"a\"//c\u2028\n:1}")>]
[<InlineData("{\"a\"//c\u2029\n:1}")>]
[<InlineData("{\"a\"//c\u2028\r:1}")>]
[<InlineData("{\"a\"//c\u2029\r:1}")>]
let ``JYRO-REQ-1869 FromJson_LineCommentHoldingALineSeparatorBeforeAColon_ReturnsNull`` (text: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull, sprintf "FromJson(%A) gave %s" text (Values.toJson (outProp "r" r)))

/// The texts of JYRO-REQ-1869's refusal: U+2028 and then U+2029 inside `//c`, followed by each of
/// LF, CR and the end of text (nothing after it, so the comment runs on to the end), placed
/// (a) after an array value, (b) between a key and its colon and (c) after the top-level value.
/// Where the comment runs to the end of text the text is not closed either, so null follows
/// either way; the rows with an LF or CR are the ones that pin the refusal.
let lineSeparatorInLineCommentCases () : obj[] seq =
    [ for sep in [ "\u2028"; "\u2029" ] do
          for term in [ "\n"; "\r"; "" ] do
              yield "[1 //c" + sep + term + "]"
              yield "{\"a\" //c" + sep + term + ":1}"
              yield "1 //c" + sep + term ]
    |> rows

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<MemberData(nameof lineSeparatorInLineCommentCases)>]
let ``JYRO-REQ-1869 FromJson_LineCommentHoldingU2028OrU2029_ReturnsNull`` (text: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull, sprintf "FromJson(%A) gave %s" text (Values.toJson (outProp "r" r)))

/// The same texts without the separator are read: the control that shows the separator, and
/// nothing else, makes them null.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("[1 //c\n]", "[1]")>]
[<InlineData("[1 //c\r]", "[1]")>]
[<InlineData("{\"a\" //c\n:1}", "{\"a\":1}")>]
[<InlineData("{\"a\" //c\r:1}", "{\"a\":1}")>]
[<InlineData("1 //c\n", "1")>]
[<InlineData("1 //c\r", "1")>]
[<InlineData("1 //c", "1")>]
let ``JYRO-REQ-1869 FromJson_LineCommentWithoutALineSeparator_IsReadAsNothing`` (text: string, meaning: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    let v = outProp "r" r
    Assert.True(v.EqualsValue(Values.json meaning), sprintf "FromJson(%A) gave %s" text (Values.toJson v))

/// A `/* */` comment ends at its `*/` whatever it holds: U+2028 and U+2029 in one read as nothing.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("[1 /*c\u2028*/]", "[1]")>]
[<InlineData("[1 /*c\u2029*/]", "[1]")>]
[<InlineData("{\"a\" /*c\u2028*/:1}", "{\"a\":1}")>]
[<InlineData("{\"a\" /*c\u2029*/:1}", "{\"a\":1}")>]
[<InlineData("1 /*c\u2028*/", "1")>]
[<InlineData("1 /*c\u2029*/", "1")>]
[<InlineData("[1 /*c\u2028\n\u2029*/]", "[1]")>]
let ``JYRO-REQ-1869 FromJson_BlockCommentHoldingU2028OrU2029_IsReadAsNothing`` (text: string, meaning: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    let v = outProp "r" r
    Assert.True(v.EqualsValue(Values.json meaning), sprintf "FromJson(%A) gave %s" text (Values.toJson v))

/// A comment between a key and its colon does not make a text that is otherwise
/// malformed readable: an unterminated `/*` (1869 makes it uninterpretable), a
/// `//` comment that holds U+2028 or U+2029 (uninterpretable too, and with no LF or CR after
/// it the colon is inside it), and a comment where no colon is due.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1869")>]
[<InlineData("{\"a\"/*c:1}")>]
[<InlineData("{\"a\"/*c*:1}")>]
[<InlineData("{\"a\"/*:1}")>]
[<InlineData("{\"a\"/*c*/:1")>]
[<InlineData("{\"a\"//c\u2028:1}")>]
[<InlineData("{\"a\"//c\u2029:1}")>]
[<InlineData("{\"a\"//c\u2028:1}\n")>]
[<InlineData("{\"a\"//c:1}")>]
[<InlineData("{\"a\\\"/*c*/:1}")>]
[<InlineData("{\"a\"/*c*/ /*d*/}")>]
[<InlineData("{\"a\"/*c*/,\"b\":1}")>]
[<InlineData("[\"a\"/*c*/:1]")>]
[<InlineData("{\"a\":\"b\"/*c*/:1}")>]
let ``JYRO-REQ-1869 FromJson_OfAMalformedTextWithACommentBeforeAColon_ReturnsNull`` (text: string) =
    let r = fromJsonOf text
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull, sprintf "FromJson(%A) gave %s" text (Values.toJson (outProp "r" r)))

let toJsonCorpus () =
    rows
        [ "null"; "true"; "1.5"; "\"s\""; "[]"; "{}"
          "{a: [1, \"x\\\"y\\\\z\", null, true], b: {}, c: [], \"/*\": \"//\", \"k e\": \"\\u0001\\t\\n\"}"
          "[[[]], [{}], {\"\": [1, 2, 3]}]"
          "{\"a,]\": \"},\"}" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-370")>]
[<MemberData(nameof toJsonCorpus)>]
let ``ToJson_WritesStrictJsonWithNoTrailingCommasOrComments`` (value: string) =
    let s = Values.asString (outProp "r" (Script.run ("Output.r = ToJson(" + value + ")")))
    let options = Text.Json.JsonDocumentOptions(AllowTrailingCommas = false, CommentHandling = Text.Json.JsonCommentHandling.Disallow)
    use _ = Text.Json.JsonDocument.Parse(s, options)
    // Outside strings: no ",]" or ",}" and no comment start.
    let outside = Text.RegularExpressions.Regex.Replace(s, "\"(\\\\.|[^\"\\\\])*\"", "\"\"")
    for bad in [ ",]"; ",}"; "/*"; "//" ] do
        Assert.DoesNotContain(bad, outside)

let roundTripScript () =
    rows
        [ "null"; "true"; "false"; "0"; "0.1"; "1e21"; "79228162514264337593543950335"; "-0.5"; "Power(2, 0.5)"
          "1e300 * Power(1, 0.5)"; "5e-324 * Power(1, 0.5)"; "\"\""; "\"q\\\"b\\\\s\""; "[]"; "{}"
          "[1, [2, {a: [3]}]]"; "{\"\": 1, \"a.b\": 2, c: [null, false]}" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-371")>]
[<MemberData(nameof roundTripScript)>]
let ``JYRO-REQ-371 FromJsonOfToJson_EqualsTheValue`` (value: string) =
    let r = Script.run (sprintf "var v = %s\nOutput.eq = FromJson(ToJson(v)) == v" value)
    Assert.True(Values.asBool (outProp "eq" r))

let roundTripHost () : obj[] seq =
    let controls = String([| for c in 0 .. 31 -> char c |])
    [ Values.str controls
      Values.str "\u2028\u2029"
      Values.str "\U0001F600"
      Values.obj [ "\U0001F600", Values.int 1; "", Values.str controls ] ]
    |> Seq.map (fun v -> [| box v |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-371")>]
[<MemberData(nameof roundTripHost)>]
let ``JYRO-REQ-371 FromJsonOfToJson_EqualsTheValue_ForStringsAndKeysOfEveryCodeUnitClass`` (value: JyroValue) =
    let r = Script.runInput (Values.obj [ "v", value ]) "Output.eq = FromJson(ToJson(Input.v)) == Input.v"
    Assert.True(Values.asBool (outProp "eq" r))

let nestedTo (depth: int) =
    lines [ "var v = {}"; "var i = 1"; sprintf "while i < %d do" depth; "  v = {c: v}"; "  i = i + 1"; "end" ]

let private generous = Some(Script.limits (fun o -> { o with MaxLoopIterations = 1000000; MaxStatements = 1000000 }))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-371")>]
let ``JYRO-REQ-371 FromJsonOfToJson_EqualsTheValue_AtExactlyTheNestingBound`` () =
    let r = Script.runWith { Script.defaults with Limits = generous } (nestedTo (maxDepth ()) + "\nOutput.eq = FromJson(ToJson(v)) == v")
    Assert.True(Values.asBool (outProp "eq" r))

// The edges of 90.11.4's domain: the values for which ToJson has no result.
// 90.11.4 only names them; that ToJson raises for them is 95.14.4, 95.14.5 and
// 95.14.10 (JYRO-REQ-1187, JYRO-REQ-1188, JYRO-REQ-1193), so these tests are
// not named by 90.11.4's requirement and are not its evidence.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1193")>]
let ``ToJson_OneLevelDeeperThanTheBound_RaisesValueNestingLimitExceeded`` () =
    let r = Script.runWith { Script.defaults with Limits = generous } (nestedTo (maxDepth () + 1) + "\nOutput.r = ToJson(v)")
    Script.expectError MessageCode.ValueNestingLimitExceeded r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1193")>]
let ``ToJson_OfACyclicValue_RaisesValueNestingLimitExceeded`` () =
    Script.expectError MessageCode.ValueNestingLimitExceeded (Script.run "var o = {}\no.self = o\nOutput.r = ToJson(o)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1187")>]
[<Trait("requirement", "JYRO-REQ-1188")>]
let ``JYRO-REQ-1187 JYRO-REQ-1188 ToJson_OfAStringOrKeyWithAnUnpairedSurrogate_RaisesUnpairedSurrogate`` () =
    // An F# string literal does not keep a lone surrogate (it holds U+FFFD): build each
    // string from its code unit number and check it still holds the surrogate (JYRO-DEF-643).
    let high = string (char 0xD800)
    let low = string (char 0xDC00)
    let text = "a" + high + "b"
    Assert.Equal(0xD800, int text.[1])
    Assert.Equal(0xDC00, int low.[0])
    Script.expectError MessageCode.UnpairedSurrogate
        (Script.runInput (Values.obj [ "v", Values.str text ]) "Output.r = ToJson(Input.v)")
    Script.expectError MessageCode.UnpairedSurrogate
        (Script.runInput (Values.obj [ "v", Values.obj [ low, Values.int 1 ] ]) "Output.r = ToJson(Input.v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-372")>]
let ``JYRO-REQ-372 ToJson_OfAFunctionValue_RaisesSoThereIsNoRoundTrip`` () =
    assertFailedWithError (Script.run "var v = [x => x]\nOutput.eq = FromJson(ToJson(v)) == v")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-372")>]
[<InlineData("x => x")>]
[<InlineData("[1, x => x]")>]
[<InlineData("{a: {b: [x => x]}}")>]
[<InlineData("[[[[[[[[[[x => x]]]]]]]]]]")>]
[<InlineData("[1, 2, 3, 4, 5, 6, 7, 8, 9, \"ok\", {a: 1}, x => x]")>]
let ``JYRO-REQ-372 ToJson_OfOrContainingAFunctionValue_RaisesARuntimeError`` (value: string) =
    let r = Script.run (lines [ "Output.r = ToJson(" + value + ")"; "Output.after = true" ])
    assertFailedWithError r
    match Script.value r with
    | Some v when (v :? JyroObject) ->
        Assert.False(hasKey "r" v)
        Assert.False(hasKey "after" v)
    | _ -> ()

[<Theory>]
[<Trait("requirement", "JYRO-REQ-373")>]
[<InlineData("0", "0")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("100", "100")>]
[<InlineData("-2.5", "-2.5")>]
[<InlineData("1e21", "1000000000000000000000")>]
[<InlineData("0.0000001", "0.0000001")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("Power(2, 0.5)", "1.4142135623730951")>]
[<InlineData("1e21 * Power(1, 0.5)", "1e+21")>]
[<InlineData("0.0000001 * Power(1, 0.5)", "1e-7")>]
[<InlineData("123456789012345678901 * Power(1, 0.5)", "123456789012345680000")>]
[<InlineData("0.1 * Power(1, 0.5)", "0.1")>]
[<InlineData("-0.5 * Power(1, 0.5)", "-0.5")>]
let ``JYRO-REQ-373 ToJson_WritesEachNumberInItsStringFormAtEveryPosition`` (number: string, expected: string) =
    let r =
        Script.run (
            lines [ "var n = " + number
                    "Output.top = ToJson(n)"
                    "Output.arr = ToJson([n])"
                    "Output.obj = ToJson({a: n})"
                    "Output.str = \"\" & n" ])
    Assert.Equal(expected, Values.asString (outProp "top" r))
    Assert.Equal("[" + expected + "]", Values.asString (outProp "arr" r))
    Assert.Equal("{\"a\":" + expected + "}", Values.asString (outProp "obj" r))
    Assert.Equal(expected, Values.asString (outProp "str" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-374")>]
[<InlineData("var o = {b: 1, a: 2, c: 3}", "{\"b\":1,\"a\":2,\"c\":3}")>]
[<InlineData("var o = {}\no[\"2\"] = 1\no[\"1\"] = 2\no[\"10\"] = 3", "{\"2\":1,\"1\":2,\"10\":3}")>]
[<InlineData("var o = {a: 1, b: 2}\no.a = 3", "{\"a\":3,\"b\":2}")>]
[<InlineData("var o = {a: 1, b: 2}\ndelete o.a\no.a = 3", "{\"b\":2,\"a\":3}")>]
[<InlineData("var o = {z: {y: 1, x: 2}, a: [{q: 1, p: 2}]}", "{\"z\":{\"y\":1,\"x\":2},\"a\":[{\"q\":1,\"p\":2}]}")>]
[<InlineData("var o = FromJson(\"{\\\"b\\\":1,\\\"a\\\":{\\\"d\\\":1,\\\"c\\\":2}}\")", "{\"b\":1,\"a\":{\"d\":1,\"c\":2}}")>]
[<InlineData("var o = Merge([{b: 1}, {a: 2}])", "{\"b\":1,\"a\":2}")>]
[<InlineData("var o = Clone({b: 1, a: 2})", "{\"b\":1,\"a\":2}")>]
let ``JYRO-REQ-374 ToJson_WritesPropertiesInTheOrderTheyWereFirstAdded`` (build: string, expected: string) =
    let r = Script.run (build + "\nOutput.j = ToJson(o)\nOutput.k = Keys(o)")
    Assert.Equal(expected, Values.asString (outProp "j" r))

// ===========================================================================
// 90.12 Sleep
// ===========================================================================

/// Every limit of 100.1.1 set explicitly (floors-process 4.1.11): 30 s, far
/// above every Sleep here, and statements, loops and call depth far above need.
let private sleepLimits (time: TimeSpan) : JyroExecutionOptions =
    { JyroExecutionOptions.Default with
        MaxExecutionTime = time
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000
        MaxCallDepth = 64 }

/// `allEffects` with the four limits set.
let private sleepOptions : RunOptions = { allEffects with Limits = Some(sleepLimits (TimeSpan.FromSeconds 30.)) }

/// Compile under `options` first, then run; give the result and the time of
/// Execute alone, so no compilation or setup is in it (floors-process 4.1.14).
let private sleepRun (options: RunOptions) (source: string) =
    let runner = Script.runnerFor options (program options source)
    let sw = Stopwatch.StartNew()
    let r = runner.Execute()
    sw.Stop()
    r, sw.Elapsed

/// Compile first, then run with a host cancellation `afterMs` after Execute
/// starts. Give the result and the time from the cancel to the end of Execute:
/// the interval 90.12.2 bounds (floors-process 4.1.14). The cancel time is
/// taken on the timer thread just before `Cancel()`, so it is not after any
/// reaction to the cancel.
let private cancelRun (options: RunOptions) (afterMs: int) (source: string) =
    use cts = new CancellationTokenSource()
    let options = { options with Cancellation = Some cts.Token }
    let runner = Script.runnerFor options (program options source)
    let canceledAt = ref 0L
    let cancel () =
        canceledAt.Value <- Stopwatch.GetTimestamp()
        cts.Cancel()
    use timer = new Timer((fun _ -> cancel ()), null, afterMs, Timeout.Infinite)
    let r = runner.Execute()
    let ended = Stopwatch.GetTimestamp()
    timer.Dispose()
    if canceledAt.Value = 0L then Values.fail (sprintf "The run ended before its cancel at %d ms was made: %s" afterMs (Script.describe r))
    r, Stopwatch.GetElapsedTime(canceledAt.Value, ended)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2158")>]
[<InlineData(1)>]
[<InlineData(50)>]
[<InlineData(200)>]
[<InlineData(300)>]
let ``JYRO-REQ-2158 Sleep_PausesAtLeastMsAndAtMostFiveHundredMoreMilliseconds`` (ms: int) =
    let r, elapsed = sleepRun sleepOptions (sprintf "Sleep(%d)\nOutput.after = true" ms)
    Assert.True(Values.asBool (outProp "after" r))
    Assert.True(elapsed >= TimeSpan.FromMilliseconds(float ms), sprintf "Sleep(%d) took %O" ms elapsed)
    Assert.True(elapsed <= TimeSpan.FromMilliseconds(float ms + 500.0), sprintf "Sleep(%d) took %O" ms elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2158")>]
let ``JYRO-REQ-2158 Sleep_TheNextStatementRunsAfterThePause`` () =
    let stamps = Collections.Concurrent.ConcurrentQueue<DateTime>()
    let stamp = Host.func "Stamp" [] ParameterType.Any FunctionEffect.Pure (fun _ -> stamps.Enqueue DateTime.UtcNow; Values.nul)
    let r = Script.runWith { sleepOptions with Functions = [ stamp.AsFunction ] } "Stamp()\nSleep(100)\nStamp()"
    Assert.True(r.IsSuccess, Script.describe r)
    let ts = List.ofSeq stamps
    Assert.True(ts.[1] - ts.[0] >= TimeSpan.FromMilliseconds 99.0, sprintf "%O" (ts.[1] - ts.[0]))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2158")>]
let ``JYRO-REQ-2158 Sleep_OfZero_ReturnsWithoutPause`` () =
    let r, elapsed = sleepRun sleepOptions "Sleep(0)\nOutput.after = true"
    Assert.True(Values.asBool (outProp "after" r))
    Assert.True(elapsed < TimeSpan.FromMilliseconds 500.0)

/// JYRO-DEF-495: "pause for ms milliseconds" is at least ms (the verification
/// of 90.12.1), on a monotonic clock. A pause shorter than one OS timer tick
/// (15.6 ms on Windows, 1 to 4 ms on Linux) is the case a tick-rounded wait gets
/// wrong, and only when the pause starts just before a tick. Back-to-back Sleeps
/// start just after the tick that woke the last one, so before each Sleep(1) a
/// host call spins for a different offset (0 to 16 ms, in steps of 0.37 ms) to
/// spread the starts across the tick. Sleep(1) is measured 200 times in one
/// run; each pause is the Stopwatch interval between a host call just before
/// Sleep(1) and one just after it, which encloses the pause. (Measured on main
/// at b521ba5, Windows: 12 of 200 such pauses were under 1 ms, the shortest
/// 0.083 ms.)
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2158")>]
let ``JYRO-REQ-2158 JYRO-DEF-495 Sleep_OfOneMs_PausesAtLeastOneMsOnAStopwatch_EachOf200Times`` () =
    let calls = 200
    let stamps = Collections.Concurrent.ConcurrentQueue<int64>()
    let offsets = ref 0
    let stamp = Host.func "Stamp" [] ParameterType.Any FunctionEffect.Pure (fun _ -> stamps.Enqueue(Stopwatch.GetTimestamp()); Values.nul)
    let offset =
        Host.func "Offset" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
            offsets.Value <- offsets.Value + 1
            let spin = TimeSpan.FromMilliseconds(float ((offsets.Value * 37) % 1600) / 100.0)
            let sw = Stopwatch.StartNew()
            while sw.Elapsed < spin do Thread.SpinWait 20
            Values.nul)
    let source = sprintf "var i = 0\nwhile i < %d do\n  Offset()\n  Stamp()\n  Sleep(1)\n  Stamp()\n  i = i + 1\nend" calls
    let r = Script.runWith { sleepOptions with Functions = [ stamp.AsFunction; offset.AsFunction ] } source
    Assert.True(r.IsSuccess, Script.describe r)
    let ts = Array.ofSeq stamps
    Assert.Equal(2 * calls, ts.Length)
    let pauses = [ for k in 0 .. calls - 1 -> Stopwatch.GetElapsedTime(ts.[2 * k], ts.[2 * k + 1]) ]
    let short = pauses |> List.filter (fun p -> p < TimeSpan.FromMilliseconds 1.0)
    Assert.True(List.isEmpty short,
                sprintf "%d of %d Sleep(1) calls paused under 1 ms; the shortest %.3f ms" short.Length calls (List.min pauses).TotalMilliseconds)

/// The bound of 90.12.2 and 90.12.3: 500 milliseconds, the book's (rc.8).
///
/// The measurement it is judged against (floors-process 4.1.13): under a whole
/// run of this project (2026-09-28, build at cbbb81d, 3,308 tests), Execute()
/// returned 0.5 to 1.3 ms after the call of Cancel(), and 1,000.5 to 1,003.4 ms
/// after its own call for a 1,000 ms limit. Each test writes the interval it
/// measured to its output. A failure of the bound means Execute() returned more
/// than 500 ms after the event: the sleeping script kept the host waiting.
let private bound = TimeSpan.FromMilliseconds 500.0

/// A runner whose program is compiled first, outside every measured interval.
let private runnerOf (options: RunOptions) (source: string) : JyroRunner =
    Script.runnerFor options (program options source)

/// The process's thread pool held saturated (JYRO-ADR-168 point 4): its worker
/// count capped at what it has (no fewer than the processors), every worker
/// blocked by the test and work still queued behind them, so no thread-pool
/// work item or timer callback can run until it is released. A dedicated thread
/// (not a pool one) releases the workers after `release` at the latest, so a run
/// that waits for a pool worker comes back late instead of never. Dispose
/// restores the cap and releases the workers. Uncapped, the pool adds a worker
/// about every 500 ms while starved, which on main let a wait end at the bound
/// itself (581 ms for 600) instead of showing the fault.
type private SaturatedPool(release: TimeSpan) =
    let gate = new ManualResetEventSlim(false)
    let started = ref 0
    let maxWorkers, maxIo = ThreadPool.GetMaxThreads()
    let restore () =
        ThreadPool.SetMaxThreads(maxWorkers, maxIo) |> ignore
        gate.Set()
    do
        let minWorkers, _ = ThreadPool.GetMinThreads()
        let cap = List.max [ ThreadPool.ThreadCount; minWorkers; Environment.ProcessorCount ]
        if not (ThreadPool.SetMaxThreads(cap, maxIo)) then
            failwithf "The thread pool's workers could not be capped at %d" cap
        for _ in 1 .. cap + 256 do
            ThreadPool.QueueUserWorkItem(WaitCallback(fun _ ->
                Interlocked.Increment(&started.contents) |> ignore
                gate.Wait())) |> ignore
        Thread(ThreadStart(fun () -> if not (gate.Wait release) then gate.Set()), IsBackground = true).Start()
        // Saturated when no further blocker has started for 50 ms and work is
        // still queued: every worker the pool has is held.
        let sw = Stopwatch.StartNew()
        let mutable last = -1
        let mutable settled = false
        while not settled && sw.Elapsed < TimeSpan.FromSeconds 10.0 do
            Thread.Sleep 50
            let now = Volatile.Read(&started.contents)
            settled <- now > 0 && now = last && ThreadPool.PendingWorkItemCount > 0L
            last <- now
        if not settled then
            restore ()
            failwithf "The thread pool could not be saturated in 10 s (%d blockers started)" last
    /// True while every worker is still held and work is still queued.
    member _.IsSaturated = not gate.IsSet && ThreadPool.PendingWorkItemCount > 0L
    // The gate is never disposed: a blocker not yet started must still be able
    // to wait on it, and finds it set.
    interface IDisposable with
        member _.Dispose() = restore ()

/// As `cancelRun`, but Cancel() is called from a dedicated thread, not a
/// thread-pool timer, so the cancel itself is on time when the pool is
/// saturated: the interval measured is the implementation's alone.
let private cancelRunFromAThread (options: RunOptions) (afterMs: int) (source: string) =
    use cts = new CancellationTokenSource()
    let options = { options with Cancellation = Some cts.Token }
    let runner = Script.runnerFor options (program options source)
    let canceledAt = ref 0L
    let canceller =
        Thread(ThreadStart(fun () ->
            Thread.Sleep afterMs
            canceledAt.Value <- Stopwatch.GetTimestamp()
            cts.Cancel()), IsBackground = true)
    use pool = new SaturatedPool(TimeSpan.FromSeconds 5.0)
    canceller.Start()
    let r = runner.Execute()
    let ended = Stopwatch.GetTimestamp()
    let saturated = pool.IsSaturated
    canceller.Join()
    if canceledAt.Value > ended then Values.fail (sprintf "The run ended before its cancel at %d ms was made: %s" afterMs (Script.describe r))
    r, Stopwatch.GetElapsedTime(canceledAt.Value, ended), saturated

[<Collection("ProcessWide")>]
type ``S90_12 Sleep ends within 500 ms``(output: Xunit.Abstractions.ITestOutputHelper) =

    /// 90.12.2: the only path is JyroRunner.Execute() of a runner given the token
    /// through WithCancellationToken (160.7.13). Each program is compiled first; the
    /// limits are set explicitly, the time limit 30 s (FLOORS 4.1.11). Cancel() is
    /// called 100 ms after Execute() is called, while Sleep(10000) runs; the
    /// interval is from the call of Cancel() to the return of Execute() (4.1.14).
    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-1978")>]
    [<InlineData("Sleep(10000)\nOutput.after = true")>]
    [<InlineData("func f() Sleep(10000) end\nf()\nOutput.after = true")>]
    [<InlineData("var i = 0\nwhile i < 3 do\n  Sleep(10000)\n  i = i + 1\nend\nOutput.after = true")>]
    member _.``Sleep_WhenTheHostCancels_ExecuteReturnsWithin500msOfCancel_WithCanceledByHost``(source: string) =
        let r, fromCancel = cancelRun sleepOptions 100 source
        // The code first (4.1.11), then the bound.
        if not (Script.codes r |> List.contains MessageCode.CanceledByHost) then
            Values.fail (sprintf "Expected CanceledByHost. Diagnostics:\n%s" (Script.describe r))
        output.WriteLine(sprintf "90.12.2 measured: Execute() returned %.1f ms after Cancel()" fromCancel.TotalMilliseconds)
        Assert.True(fromCancel <= bound, sprintf "Execute() returned %O after Cancel() (bound %O)" fromCancel bound)

    /// 90.12.3: a runner whose limit L was set through WithMaxExecutionTime or
    /// WithExecutionOptions (160.7.13). On the WithMaxExecutionTime path the other
    /// limits are set first by WithExecutionOptions, and WithMaxExecutionTime,
    /// called last, sets the time limit (160.7.13, so the test is tagged with it).
    /// The interval is from the call of Execute() to its return (4.1.14): the
    /// program is compiled before. Only limits that end during Sleep are used.
    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-377")>]
    [<Trait("requirement", "JYRO-REQ-2116")>]
    [<InlineData("WithMaxExecutionTime", "Sleep(10000)\nOutput.after = true")>]
    [<InlineData("WithExecutionOptions", "Sleep(10000)\nOutput.after = true")>]
    [<InlineData("WithMaxExecutionTime", "func f() Sleep(10000) end\nf()\nOutput.after = true")>]
    [<InlineData("WithExecutionOptions", "func f() Sleep(10000) end\nf()\nOutput.after = true")>]
    [<InlineData("WithMaxExecutionTime", "Sleep(300)\nSleep(10000)\nOutput.after = true")>]
    [<InlineData("WithExecutionOptions", "Sleep(300)\nSleep(10000)\nOutput.after = true")>]
    member _.``JYRO-REQ-2116 Sleep_WhenTheTimeLimitEndsDuringIt_ExecuteReturnsWithinLPlus500ms_WithExecutionTimeLimitExceeded``(path: string, source: string) =
        let limit = TimeSpan.FromMilliseconds 1000.0
        let options =
            match path with
            | "WithExecutionOptions" -> { sleepOptions with Limits = Some(sleepLimits limit) }
            | _ -> { sleepOptions with ConfigureRunner = fun r -> r.WithMaxExecutionTime(limit) }
        let runner = runnerOf options source
        let sw = Stopwatch.StartNew()
        let r = runner.Execute()
        sw.Stop()
        if not (Script.codes r |> List.contains MessageCode.ExecutionTimeLimitExceeded) then
            Values.fail (sprintf "Expected ExecutionTimeLimitExceeded. Diagnostics:\n%s" (Script.describe r))
        output.WriteLine(sprintf "90.12.3 measured (%s): Execute() returned %.1f ms after it was called, L = 1000 ms" path sw.Elapsed.TotalMilliseconds)
        Assert.True(sw.Elapsed <= limit + bound, sprintf "Execute() returned %O after it was called (bound L + 500 ms = %O)" sw.Elapsed (limit + bound))

    /// 90.12.3 with the process's thread pool saturated (JYRO-ADR-168 points 1
    /// and 4; JYRO-DEF-698): the bound holds whatever else the host runs on its
    /// pool. Both paths of 160.7.13, L = 1,000 ms, for one Sleep past the limit
    /// and for a loop of short Sleeps. The program is compiled and the runner set
    /// up before the pool is saturated; the interval is Execute() alone (4.1.14).
    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-377")>]
    [<InlineData("WithMaxExecutionTime", "Sleep(10000)\nOutput.after = true")>]
    [<InlineData("WithExecutionOptions", "Sleep(10000)\nOutput.after = true")>]
    [<InlineData("WithMaxExecutionTime", "while true do\n  Sleep(10)\nend")>]
    [<InlineData("WithExecutionOptions", "while true do\n  Sleep(10)\nend")>]
    member _.``JYRO-REQ-377 JYRO-DEF-698 Sleep_WhenTheTimeLimitEndsDuringIt_WithTheThreadPoolSaturated_ExecuteReturnsWithinLPlus500ms``(path: string, source: string) =
        let limit = TimeSpan.FromMilliseconds 1000.0
        let options =
            match path with
            | "WithExecutionOptions" -> { sleepOptions with Limits = Some(sleepLimits limit) }
            | _ -> { sleepOptions with ConfigureRunner = fun r -> r.WithMaxExecutionTime(limit) }
        let runner = runnerOf options source
        use pool = new SaturatedPool(TimeSpan.FromSeconds 5.0)
        let sw = Stopwatch.StartNew()
        let r = runner.Execute()
        sw.Stop()
        let saturated = pool.IsSaturated
        output.WriteLine(sprintf "90.12.3 measured, pool saturated (%s): Execute() returned %.1f ms after it was called, L = 1000 ms; saturated throughout: %b" path sw.Elapsed.TotalMilliseconds saturated)
        if not (Script.codes r |> List.contains MessageCode.ExecutionTimeLimitExceeded) then
            Values.fail (sprintf "Expected ExecutionTimeLimitExceeded. Diagnostics:\n%s" (Script.describe r))
        Assert.True(sw.Elapsed <= limit + bound, sprintf "Execute() returned %O after it was called with the thread pool saturated (bound L + 500 ms = %O)" sw.Elapsed (limit + bound))
        Assert.True(saturated, "The thread pool was released before Execute() returned, so the run was not measured under saturation")

    /// 90.12.2 with the process's thread pool saturated (JYRO-ADR-168 points 2
    /// and 4): Cancel() is called from a dedicated thread 100 ms after Execute()
    /// is called, while Sleep(10000) runs; the interval is from Cancel() to the
    /// return of Execute() (4.1.14).
    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-1978")>]
    [<InlineData("Sleep(10000)\nOutput.after = true")>]
    [<InlineData("var i = 0\nwhile i < 3 do\n  Sleep(10000)\n  i = i + 1\nend\nOutput.after = true")>]
    member _.``JYRO-REQ-1978 Sleep_WhenTheHostCancels_WithTheThreadPoolSaturated_ExecuteReturnsWithin500msOfCancel``(source: string) =
        let r, fromCancel, saturated = cancelRunFromAThread sleepOptions 100 source
        output.WriteLine(sprintf "90.12.2 measured, pool saturated: Execute() returned %.1f ms after Cancel(); saturated throughout: %b" fromCancel.TotalMilliseconds saturated)
        if not (Script.codes r |> List.contains MessageCode.CanceledByHost) then
            Values.fail (sprintf "Expected CanceledByHost. Diagnostics:\n%s" (Script.describe r))
        Assert.True(fromCancel <= bound, sprintf "Execute() returned %O after Cancel() with the thread pool saturated (bound %O)" fromCancel bound)
        Assert.True(saturated, "The thread pool was released before Execute() returned, so the run was not measured under saturation")

    /// 90.12.3 while another run's token callback blocks (JYRO-ADR-170 point 6;
    /// JYRO-DEF-711): the bound holds whatever else the host process runs, and a
    /// callback another run registered on its ctx.CancellationToken is such code.
    /// Run A (limit 200 ms, on a thread of its own) calls a host function that
    /// registers a callback on ctx.CancellationToken which blocks until the test
    /// releases it, then waits on that token. Once A's time limit has fired and
    /// the callback is blocking, run B (L = 1,000 ms) is executed on the test's
    /// thread: B blocks on its own token (a host function waiting on
    /// ctx.CancellationToken.WaitHandle) or sleeps past its limit. B's Execute()
    /// must return ExecutionTimeLimitExceeded within L + 500 ms of its call while
    /// A's callback is still blocked. Each case runs on an idle pool and with the
    /// pool saturated (ADR-168 point 4). The callback is released in a finally,
    /// and every wait in the test has a timeout of 10 s at most, so a build that
    /// stalls fails late instead of hanging.
    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-377")>]
    [<InlineData(false, "Output.r = WaitOnToken()")>]
    [<InlineData(false, "Sleep(10000)\nOutput.after = true")>]
    [<InlineData(true, "Output.r = WaitOnToken()")>]
    [<InlineData(true, "Sleep(10000)\nOutput.after = true")>]
    member _.``JYRO-REQ-377 JYRO-DEF-711 WhileAnotherRunsTokenCallbackBlocks_ExecuteReturnsWithinLPlus500ms_WithExecutionTimeLimitExceeded``(saturate: bool, source: string) =
        let limitA = TimeSpan.FromMilliseconds 200.0
        let limit = TimeSpan.FromMilliseconds 1000.0
        let longest = TimeSpan.FromSeconds 10.0
        // Never disposed: the callback may still be waiting on them when the
        // test ends, if a build stalls.
        let entered = new ManualResetEventSlim(false)
        let release = new ManualResetEventSlim(false)
        let returned = ref 0
        let hold =
            Host.funcWithContext "HoldTheCancel" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
                ctx.CancellationToken.Register(Action(fun () ->
                    entered.Set()
                    release.Wait longest |> ignore
                    Volatile.Write(&returned.contents, 1))) |> ignore
                ctx.CancellationToken.WaitHandle.WaitOne longest |> ignore
                ctx.CheckExecutionLimits()
                Values.nul)
        let waitOnToken =
            Host.funcWithContext "WaitOnToken" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
                ctx.CancellationToken.WaitHandle.WaitOne longest |> ignore
                ctx.CheckExecutionLimits()
                Values.nul)
        let runnerA = runnerOf { sleepOptions with Limits = Some(sleepLimits limitA); Functions = [ hold.AsFunction ] } "Output.r = HoldTheCancel()"
        let runnerB = runnerOf { sleepOptions with Limits = Some(sleepLimits limit); Functions = [ waitOnToken.AsFunction ] } source
        let pool = if saturate then Some(new SaturatedPool(TimeSpan.FromSeconds 5.0)) else None
        let threadA = Thread(ThreadStart(fun () -> runnerA.Execute() |> ignore), IsBackground = true)
        try
            threadA.Start()
            if not (entered.Wait(TimeSpan.FromSeconds 5.0)) then
                Values.fail "Run A's token callback never ran: its time limit of 200 ms did not cancel its token within 5 s"
            let sw = Stopwatch.StartNew()
            let r = runnerB.Execute()
            sw.Stop()
            let stillBlocked = Volatile.Read(&returned.contents) = 0
            let saturated = pool |> Option.forall (fun p -> p.IsSaturated)
            let poolNote = if saturate then sprintf "pool saturated, throughout: %b" saturated else "pool idle"
            output.WriteLine(sprintf "90.12.3 measured beside a blocking token callback (%s): Execute() returned %.1f ms after it was called, L = 1000 ms; callback still blocked: %b" poolNote sw.Elapsed.TotalMilliseconds stillBlocked)
            if not (Script.codes r |> List.contains MessageCode.ExecutionTimeLimitExceeded) then
                Values.fail (sprintf "Expected ExecutionTimeLimitExceeded. Diagnostics:\n%s" (Script.describe r))
            Assert.True(sw.Elapsed <= limit + bound, sprintf "Execute() returned %O after it was called while another run's token callback blocked (bound L + 500 ms = %O)" sw.Elapsed (limit + bound))
            Assert.True(stillBlocked, "Run A's token callback returned before run B ended, so B was not measured beside a blocking callback")
            Assert.True(saturated, "The thread pool was released before Execute() returned, so the run was not measured under saturation")
        finally
            release.Set()
            pool |> Option.iter (fun p -> (p :> IDisposable).Dispose())
            threadA.Join longest |> ignore

// ===========================================================================
// 90.13.1 An explicit null for an optional parameter means "not given"
// ===========================================================================

let private sameValue (a: JyroValue) (b: JyroValue) =
    a.EqualsValue b
    && (match a, b with
        | (:? JyroNumber as x), (:? JyroNumber as y) -> x.IsExact = y.IsExact
        | _ -> true)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-379")>]
[<InlineData("Substring(\"abcdef\", 2, null)", "Substring(\"abcdef\", 2)")>]
[<InlineData("Substring(text: \"abcdef\", start: 2, length: null)", "Substring(text: \"abcdef\", start: 2)")>]
[<InlineData("Substring(\"abcdef\", 2, n)", "Substring(\"abcdef\", 2)")>]
[<InlineData("PadLeft(\"a\", 3, null)", "PadLeft(\"a\", 3)")>]
[<InlineData("PadLeft(text: \"a\", length: 3, padding: null)", "PadLeft(\"a\", 3)")>]
[<InlineData("PadRight(\"a\", 3, null)", "PadRight(\"a\", 3)")>]
[<InlineData("PadRight(\"a\", 3, n)", "PadRight(\"a\", 3)")>]
[<InlineData("Slice([1, 2, 3], 1, null)", "Slice([1, 2, 3], 1)")>]
[<InlineData("Slice(items: [1, 2, 3], start: 1, finish: null)", "Slice([1, 2, 3], 1)")>]
[<InlineData("Range(0, 5, null)", "Range(0, 5)")>]
[<InlineData("Range(start: 0, finish: 5, step: n)", "Range(0, 5)")>]
[<InlineData("Round(2.5, 0, null)", "Round(2.5, 0)")>]
[<InlineData("Round(-2.5, 0, n)", "Round(-2.5, 0)")>]
[<InlineData("Log(8, null)", "Log(8)")>]
[<InlineData("Log(value: 8, base: null)", "Log(8)")>]
let ``JYRO-REQ-379 OptionalParameter_GivenNull_GivesTheSameResultAsOmitted`` (withNull: string, omitted: string) =
    let r = Script.run (lines [ "var n = null"; "Output.a = " + withNull; "Output.b = " + omitted ])
    let a, b = outProp "a" r, outProp "b" r
    Assert.True(sameValue a b, sprintf "%s gave %s, %s gave %s" withNull (Values.toJson a) omitted (Values.toJson b))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-379")>]
[<InlineData("Substring(\"ab\", 5, null)", "Substring(\"ab\", 5)")>]
[<InlineData("Log(0, null)", "Log(0)")>]
[<InlineData("Slice(5, 1, null)", "Slice(5, 1)")>]
let ``JYRO-REQ-379 OptionalParameter_GivenNull_GivesTheSameErrorAsOmitted`` (withNull: string, omitted: string) =
    let a = Script.onlyError (Script.run ("Output.r = " + withNull))
    let b = Script.onlyError (Script.run ("Output.r = " + omitted))
    Assert.Equal(b, a)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-379")>]
[<InlineData("RandomString(20, null)")>]
[<InlineData("RandomString(length: 20, characterSet: null)")>]
let ``JYRO-REQ-379 RandomString_WithANullCharacterSet_UsesTheDefaultSet`` (call: string) =
    let s = Values.asString (outProp "r" (Script.runWith allEffects ("Output.r = " + call)))
    Assert.Equal(20, s.Length)
    Assert.Matches("^[A-Za-z0-9]{20}$", s)

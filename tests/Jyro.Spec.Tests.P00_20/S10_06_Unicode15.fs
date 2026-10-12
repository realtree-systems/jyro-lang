/// 10.6.1 - Unicode 15.0.0: every property, category and case mapping the book
/// reads is the one of Unicode 15.0.0, whatever tables the host platform carries.
/// Every run sets the four execution limits (and, for a regex, the regex
/// timeout) explicitly (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S10_06_Unicode15

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// Code points, by their Unicode 15.0.0 data:
//   U+1E030 MODIFIER LETTER CYRILLIC SMALL A   Lm, new in 15.0
//   U+11F04 KAWI LETTER A                      Lo, new in 15.0
//   U+11F50 KAWI DIGIT ZERO                    Nd, new in 15.0
//   U+2EBF0, U+2EE5D CJK Extension I           unassigned in 15.0 (Lo in 15.1)
//   U+1C89 CYRILLIC CAPITAL LETTER TJE         unassigned in 15.0 (Lu in 16.0)
//   U+0264 LATIN SMALL LETTER RAMS HORN        no uppercase in 15.0 (U+A7CB from 16.0)
//   U+2C5F GLAGOLITIC SMALL LETTER CAUDATE CHRI uppercase U+2C2F (14.0)
//   U+A7CB, U+A7DC                             unassigned in 15.0 (Lu in 16.0)
//   U+180E MONGOLIAN VOWEL SEPARATOR           Cf, not White_Space, not Zs
//   U+3000 IDEOGRAPHIC SPACE                   Zs, White_Space

let lettersNewIn15Rows : seq<obj[]> = rowsOf [ "1E030"; "11F04" ]
let lettersAfter15Rows : seq<obj[]> = rowsOf [ "2EBF0"; "2EE5D"; "1C89" ]

let private hex (s: string) : int = System.Convert.ToInt32(s, 16)

// -- Identifiers (20.1.1, 20.3.1 with the 15.0 categories) ---------------------

[<Theory>]
[<MemberData(nameof lettersNewIn15Rows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 Identifier_LetterAssignedIn15_0_StartsAnIdentifier`` (codePoint: string) =
    let c = cp (hex codePoint)
    outVL (sprintf "var %sx = 1\nOutput.v = %sx" c c) |> assertJson "1"
    outVL (sprintf "var %s = 2\nOutput.v = %s" c c) |> assertJson "2"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 Identifier_DigitAssignedIn15_0_ContinuesAnIdentifier`` () =
    let d = cp 0x11F50
    outVL (sprintf "var a%s = 1\nOutput.v = a%s" d d) |> assertJson "1"
    outVL (sprintf "var _%s%s = 3\nOutput.v = _%s%s" d d d d) |> assertJson "3"

[<Theory>]
[<MemberData(nameof lettersAfter15Rows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 Identifier_LetterAssignedAfter15_0_IsAParseError`` (codePoint: string) =
    let c = cp (hex codePoint)
    parseErrorL (sprintf "var %s = 1\nOutput.v = 1" c) |> ignore
    parseErrorL (sprintf "var %sx = 1\nOutput.v = 1" c) |> ignore
    parseErrorL (sprintf "var x%s = 1\nOutput.v = 1" c) |> ignore
    parseErrorL (sprintf "var a%sb = 1\nOutput.v = 1" c) |> ignore
    // Outside a declaration too: any place outside a string and a comment.
    parseErrorL (sprintf "Output.v = 1 %s" c) |> ignore

[<Theory>]
[<MemberData(nameof lettersAfter15Rows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 CodePointAssignedAfter15_0_InAStringOrComment_IsAccepted`` (codePoint: string) =
    let c = cp (hex codePoint)
    // In a string literal it is kept, code unit for code unit.
    let s = Values.asString (outVL (sprintf "Output.v = \"%s\"" c))
    Assert.Equal(c, s)
    let s2 = Values.asString (outVL (sprintf "Output.v = 'a%sb'" c))
    Assert.Equal("a" + c + "b", s2)
    // In a comment it changes nothing.
    outputL (sprintf "# %s\nOutput.v = 1 # %s" c c) |> assertJson "{\"v\":1}"

// -- White_Space and the lexer (20.1.1 whitespace) ----------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 Whitespace_ReadsWhiteSpaceOf15_0`` () =
    // U+3000 has White_Space: it separates tokens.
    let ws = cp 0x3000
    outVL (sprintf "var%sa%s=%s1\nOutput.v%s=%sa" ws ws ws ws ws) |> assertJson "1"
    // U+180E does not have White_Space in 15.0: outside the grammar.
    let mvs = cp 0x180E
    parseErrorL (sprintf "var%sa = 1\nOutput.v = 1" mvs) |> ignore
    parseErrorL (sprintf "var a = 1%s\nOutput.v = a" mvs) |> ignore

// -- Case mappings (91.1.2, 91.2.2) ---------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 ToUpper_ReadsUnicode15_0`` () =
    // U+0264 has no simple uppercase mapping in 15.0 (16.0 gives U+A7CB).
    Assert.Equal(cp 0x0264, Values.asString (outVL (sprintf "Output.v = ToUpper(\"%s\")" (cp 0x0264))))
    // U+2C5F maps to U+2C2F (assigned in 14.0).
    Assert.Equal(cp 0x2C2F, Values.asString (outVL (sprintf "Output.v = ToUpper(\"%s\")" (cp 0x2C5F))))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 ToLower_ReadsUnicode15_0`` () =
    // U+A7CB and U+A7DC are unassigned in 15.0, so have no lowercase mapping.
    Assert.Equal(cp 0xA7CB, Values.asString (outVL (sprintf "Output.v = ToLower(\"%s\")" (cp 0xA7CB))))
    Assert.Equal(cp 0xA7DC, Values.asString (outVL (sprintf "Output.v = ToLower(\"%s\")" (cp 0xA7DC))))
    // U+1C89 is unassigned in 15.0 (16.0 gives it the lowercase U+1C8A).
    Assert.Equal(cp 0x1C89, Values.asString (outVL (sprintf "Output.v = ToLower(\"%s\")" (cp 0x1C89))))
    // U+2C2F maps to U+2C5F (assigned in 14.0).
    Assert.Equal(cp 0x2C5F, Values.asString (outVL (sprintf "Output.v = ToLower(\"%s\")" (cp 0x2C2F))))

// -- White_Space in the library (91.3.2, 91.3.3, 96.1.6) -----------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 Trim_And_ValidateRequired_ReadWhiteSpaceOf15_0`` () =
    let mvs = cp 0x180E
    let ideo = cp 0x3000
    // Trim keeps U+180E and removes U+3000.
    Assert.Equal(mvs + "a" + mvs, Values.asString (outVL (sprintf "Output.v = Trim(\"%sa%s\")" mvs mvs)))
    Assert.Equal("a", Values.asString (outVL (sprintf "Output.v = Trim(\"%sa%s\")" ideo ideo)))
    Assert.Equal(mvs, Values.asString (outVL (sprintf "Output.v = Trim(\"%s%s%s\")" ideo mvs ideo)))
    // ValidateRequired gives no `empty` for U+180E, and gives it for U+3000. Judged by
    // `valid` and the number of errors only: the shape of an error element is another
    // work order's rule (JYRO-REQ-1219), not a reading of White_Space.
    let required (value: string) =
        outVL (sprintf "var r = ValidateRequired({a: \"%s\"}, [\"a\"])\nOutput.v = [r.valid, Length(r.errors)]" value)
    required mvs |> assertJson "[true,0]"
    required (ideo + ideo) |> assertJson "[false,1]"
    required (ideo + mvs + ideo) |> assertJson "[true,0]"

// -- \s and \S in a pattern (90.10.1, ECMA-262 WhiteSpace reads Zs) -----------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexWhiteSpaceClass_ReadsZsOf15_0`` () =
    let run (text: string) (pattern: string) =
        let r = Script.runWith limitedRegex (sprintf "Output.v = RegexTest(\"%s\", \"%s\")" text pattern)
        Values.asBool ((Script.output r).GetProperty "v")
    // In the Jyro source "\\s" is the two characters \s.
    Assert.True(run (cp 0x3000) "^\\\\s$", "U+3000 (Zs) matches \\s")
    Assert.False(run (cp 0x180E) "^\\\\s$", "U+180E (Cf) does not match \\s")
    Assert.False(run (cp 0x3000) "^\\\\S$", "U+3000 (Zs) does not match \\S")
    Assert.True(run (cp 0x180E) "^\\\\S$", "U+180E (Cf) matches \\S")

// -- Group names and identity escapes in a pattern (90.10.1, 96.2.19) ----------
//
// ECMA-262's RegExpIdentifierName reads UnicodeIDStart and UnicodeIDContinue,
// and its IdentityEscape (no u flag, no Annex B) is a SourceCharacter that is not
// UnicodeIDContinue. 10.6.1 reads both as ID_Start and ID_Continue of
// DerivedCoreProperties.txt of Unicode 15.0.0: Other_ID_Start and Other_ID_Continue
// included, Pattern_Syntax and Pattern_White_Space excluded. Code points, by the
// Unicode 15.0.0 data:
//   U+2118 SCRIPT CAPITAL P              Sm, Other_ID_Start
//   U+212E ESTIMATED SYMBOL              So, Other_ID_Start
//   U+309B, U+309C KATAKANA-HIRAGANA (SEMI-)VOICED SOUND MARK  Sk, Other_ID_Start
//   U+1885 MONGOLIAN LETTER ALI GALI BALUDA  Mn, Other_ID_Start
//   U+2160 ROMAN NUMERAL ONE             Nl, ID_Start
//   U+00B7 MIDDLE DOT                    Po, Other_ID_Continue (not ID_Start)
//   U+0387 GREEK ANO TELEIA              Po, Other_ID_Continue
//   U+1369 ETHIOPIC DIGIT ONE            No, Other_ID_Continue
//   U+19DA NEW TAI LUE THAM DIGIT ONE    No, Other_ID_Continue
//   U+0300 COMBINING GRAVE ACCENT        Mn, ID_Continue
//   U+203F UNDERTIE                      Pc, ID_Continue
//   U+2E2F VERTICAL TILDE                Lm, Pattern_Syntax: neither ID_Start nor ID_Continue
//   U+30FB KATAKANA MIDDLE DOT           Po, not ID_Continue in 15.0
//   U+200D ZERO WIDTH JOINER             Cf, not ID_Continue in 15.0
//   U+00A7 SECTION SIGN                  Po, not ID_Continue
//   U+1C89                               unassigned in 15.0 (Lu in 16.0)
//   U+2EBF0                              unassigned in 15.0 (Lo in 15.1)
//   U+1E030, U+11F04                     Lm, Lo, new in 15.0: ID_Start
//   U+11F50 KAWI DIGIT ZERO              Nd, new in 15.0: ID_Continue, not ID_Start

/// Run `source` under `limitedRegex` with `Input` = { t: text, p: pattern }, so
/// that a pattern reaches the function exactly as written here.
let private runTP (text: string) (pattern: string) (source: string) : JyroResult<JyroValue> =
    Script.runWith
        { limitedRegex with Input = ValueInput(Values.obj [ "t", Values.str text; "p", Values.str pattern ]) }
        source

/// `RegexTest(Input.t, Input.p)` of a successful run.
let private regexTest (text: string) (pattern: string) : bool =
    let r = runTP text pattern "Output.v = RegexTest(Input.t, Input.p)"
    if not (Script.ok r) then
        Values.fail (sprintf "RegexTest(%A, %A) failed. Diagnostics:\n%s" text pattern (Script.describe r))
    Values.asBool ((Script.output r).GetProperty "v")

/// Assert that each of the four regex functions raises RegexInvalidPattern
/// (90.10.2) for `pattern`, and that the script stops there.
let private assertInvalidPattern (pattern: string) =
    for fn in [ "RegexTest"; "RegexMatch"; "RegexMatchAll"; "RegexMatchDetail" ] do
        let r = runTP "a" pattern (sprintf "Output.v = %s(Input.t, Input.p)\nOutput.after = 1" fn)
        if Script.ok r || not (Script.errorCodes r |> List.contains MessageCode.RegexInvalidPattern) then
            Values.fail (sprintf "%s with the pattern %A: expected RegexInvalidPattern. Diagnostics:\n%s"
                            fn pattern (Script.describe r))
        Assert.Equal<MessageCode list>([ MessageCode.RegexInvalidPattern ], Script.errorCodes r)

let private hexRows (xs: string list) : seq<obj[]> = rowsOf xs

/// Code points that are ID_Start in Unicode 15.0.0.
let idStartRows () = hexRows [ "2118"; "212E"; "309B"; "309C"; "1885"; "2160"; "1E030"; "11F04"; "41"; "E9" ]

/// Code points that are ID_Continue but not ID_Start in Unicode 15.0.0.
let idContinueOnlyRows () = hexRows [ "B7"; "387"; "1369"; "19DA"; "300"; "203F"; "11F50" ]

/// Code points that are not ID_Continue (so not ID_Start) in Unicode 15.0.0.
let notIdContinueRows () = hexRows [ "2E2F"; "30FB"; "1C89"; "2EBF0"; "A7" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexGroupName_OtherIdStart_IsAccepted_TheBookExample`` () =
    // 10.6.1: RegexTest("a", P), where P is "(?<", U+2118, ">a)", MUST return true.
    Assert.True(regexTest "a" ("(?<" + cp 0x2118 + ">a)"))

[<Theory>]
[<MemberData(nameof idStartRows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexGroupName_StartingWithAnIdStartCodePoint_IsAccepted`` (codePoint: string) =
    let c = cp (hex codePoint)
    Assert.True(regexTest "a" ("(?<" + c + ">a)"))
    Assert.True(regexTest "aa" ("^(?<" + c + ">a)\\k<" + c + ">$"))
    Assert.False(regexTest "ab" ("^(?<" + c + ">a)\\k<" + c + ">$"))
    // Written as a \u escape in the name (RegExpIdentifierStart), for a BMP code point.
    if hex codePoint <= 0xFFFF then
        Assert.True(regexTest "a" (sprintf "(?<\\u%04X>a)" (hex codePoint)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexGroupName_U1C89_RaisesRegexInvalidPattern_TheBookExample`` () =
    // 10.6.1: a group name that is U+1C89, which Unicode 16.0.0 first assigns,
    // MUST raise RegexInvalidPattern.
    assertInvalidPattern ("(?<" + cp 0x1C89 + ">a)")

[<Theory>]
[<MemberData(nameof notIdContinueRows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexGroupName_StartingWithACodePointNotIdStart_RaisesRegexInvalidPattern`` (codePoint: string) =
    let c = cp (hex codePoint)
    assertInvalidPattern ("(?<" + c + ">a)")
    assertInvalidPattern ("(?<" + c + "x>a)")
    if hex codePoint <= 0xFFFF then
        assertInvalidPattern (sprintf "(?<\\u%04X>a)" (hex codePoint))

[<Theory>]
[<MemberData(nameof idContinueOnlyRows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexGroupName_IdContinueNotIdStart_ContinuesButDoesNotStartAName`` (codePoint: string) =
    let c = cp (hex codePoint)
    // After the first character it is part of the name.
    Assert.True(regexTest "a" ("(?<x" + c + ">a)"))
    Assert.True(regexTest "aa" ("^(?<x" + c + ">a)\\k<x" + c + ">$"))
    // As the first character it is not.
    assertInvalidPattern ("(?<" + c + ">a)")
    assertInvalidPattern ("(?<" + c + "x>a)")

[<Theory>]
[<MemberData(nameof notIdContinueRows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexGroupName_ContinuingWithACodePointNotIdContinue_RaisesRegexInvalidPattern`` (codePoint: string) =
    let c = cp (hex codePoint)
    assertInvalidPattern ("(?<x" + c + ">a)")
    assertInvalidPattern ("(?<x" + c + "y>a)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexIdentityEscape_U1C89_MatchesU1C89_TheBookExample`` () =
    // 10.6.1: a backslash before U+1C89, which is not ID_Continue in Unicode
    // 15.0.0, MUST be an identity escape that matches U+1C89.
    let c = cp 0x1C89
    Assert.True(regexTest c ("^\\" + c + "$"))
    Assert.True(regexTest ("a" + c + "b") ("a\\" + c + "b"))
    Assert.False(regexTest "a" ("^\\" + c + "$"))

/// BMP code points that are not ID_Continue in Unicode 15.0.0: after a backslash
/// each is an identity escape. (Without the u flag a pattern is read by code
/// unit, so a code point above U+FFFF is not used here.)
let identityEscapeRows () = hexRows [ "1C89"; "2E2F"; "30FB"; "FF65"; "200D"; "A7"; "2014" ]

[<Theory>]
[<MemberData(nameof identityEscapeRows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexIdentityEscape_CodePointNotIdContinue_MatchesItself`` (codePoint: string) =
    let c = cp (hex codePoint)
    Assert.True(regexTest c ("^\\" + c + "$"))
    Assert.True(regexTest (c + c) ("^\\" + c + "{2}$"))
    Assert.False(regexTest "x" ("^\\" + c + "$"))

/// BMP code points that are ID_Continue in Unicode 15.0.0, through a general
/// category or through Other_ID_Start or Other_ID_Continue: after a backslash
/// none is an identity escape, and no other escape starts with it.
let notIdentityEscapeRows () = hexRows [ "B7"; "387"; "1369"; "19DA"; "2118"; "212E"; "309B"; "1885"; "2160"; "300"; "203F"; "E9" ]

[<Theory>]
[<MemberData(nameof notIdentityEscapeRows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 RegexIdentityEscape_CodePointIdContinue_RaisesRegexInvalidPattern`` (codePoint: string) =
    let c = cp (hex codePoint)
    assertInvalidPattern ("\\" + c)
    assertInvalidPattern ("a\\" + c + "b")

/// `ValidateSchema(Input.d, Input.s)` with the schema `{"pattern": p}` (96.2.19).
let private schemaPattern (data: string) (pattern: string) : JyroResult<JyroValue> =
    Script.runWith
        { limitedRegex with Input = ValueInput(Values.obj [ "d", Values.str data; "s", Values.obj [ "pattern", Values.str pattern ] ]) }
        "Output.v = Length(ValidateSchema(Input.d, Input.s))\nOutput.after = 1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 SchemaPattern_GroupNamesAndIdentityEscapes_ReadIdStartAndIdContinueOf15_0`` () =
    let count (data: string) (pattern: string) =
        let r = schemaPattern data pattern
        if not (Script.ok r) then
            Values.fail (sprintf "ValidateSchema with the pattern %A failed. Diagnostics:\n%s" pattern (Script.describe r))
        Values.asDecimal ((Script.output r).GetProperty "v")
    let invalid (pattern: string) =
        let r = schemaPattern "a" pattern
        Assert.Equal<MessageCode list>([ MessageCode.RegexInvalidPattern ], Script.errorCodes r)
    // A group name of U+2118 (Other_ID_Start) is accepted: "a" matches, "b" does not.
    Assert.Equal(0m, count "a" ("^(?<" + cp 0x2118 + ">a)$"))
    Assert.Equal(1m, count "b" ("^(?<" + cp 0x2118 + ">a)$"))
    // A group name of U+1C89 or U+2E2F is refused.
    invalid ("(?<" + cp 0x1C89 + ">a)")
    invalid ("(?<" + cp 0x2E2F + ">a)")
    // A backslash before U+1C89 is an identity escape; before U+00B7 it is refused.
    Assert.Equal(0m, count (cp 0x1C89) ("^\\" + cp 0x1C89 + "$"))
    Assert.Equal(1m, count "a" ("^\\" + cp 0x1C89 + "$"))
    invalid ("\\" + cp 0x00B7)

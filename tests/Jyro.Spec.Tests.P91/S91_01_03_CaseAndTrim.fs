/// jyro-spec 91.1 ToUpper, 91.2 ToLower, 91.3 Trim.
module RealTree.Jyro.Spec.Tests.P91.S91_01_03_CaseAndTrim

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P91.Support

// The container and function values every text parameter must refuse.
let badValues : obj[] seq = badData

// ============================================================================
// 91.1 ToUpper
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-380")>]
let ToUpper_PositionalAndNamedText_ReturnString () =
    Assert.Equal("ABC", str "ToUpper(\"abc\")")
    Assert.Equal("ABC", str "ToUpper(text: \"abc\")")
    Assert.Equal("string", typeOf "ToUpper(\"abc\")")
    Assert.Equal("string", typeOf "ToUpper(null)")
    Assert.Equal("string", typeOf "ToUpper(5)")
    Assert.Equal("string", typeOf "ToUpper(true)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-380")>]
let ToUpper_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "ToUpper()"
    expectErr MessageCode.TooManyArguments "ToUpper(\"a\", \"b\")"
    expectErrAmong MessageCode.UnknownNamedArgument "ToUpper(value: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-381")>]
[<InlineData("abc", "ABC")>]
[<InlineData("hElLo 1!", "HELLO 1!")>]
[<InlineData("\u00E0", "\u00C0")>]
[<InlineData("\u03C3", "\u03A3")>]
[<InlineData("\u03C2", "\u03A3")>]
[<InlineData("\u0131", "I")>]
[<InlineData("\u01C6", "\u01C4")>]
[<InlineData("\u00DF", "\u00DF")>]
[<InlineData("\uFB01", "\uFB01")>]
[<InlineData("\u0149", "\u0149")>]
[<InlineData("0123456789.,;-_", "0123456789.,;-_")>]
let ``JYRO-REQ-381 ToUpper_EachCodePoint_MapsToSimpleUppercase`` (text: string, expected: string) =
    Assert.Equal(expected, strB ("ToUpper(" + lit text + ")"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-381")>]
let ``JYRO-REQ-381 ToUpper_ReturnsNewString_ArgumentUnchanged`` () =
    let v = evalPreB "var s = \"abc\"\nvar t = ToUpper(s)" "[s, t]"
    Assert.Equal<string list>([ "abc"; "ABC" ], Values.asList v |> List.map Values.asString)

/// 10.6.1: the simple uppercase mapping of UnicodeData.txt of Unicode 15.0.0,
/// whatever the platform carries. U+0264 first gains an uppercase (U+A7CB) in
/// 16.0, so in 15.0 it maps to itself; U+2C5F (added in 14.0) maps to U+2C2F.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-381")>]
[<InlineData(0x0264, 0x0264)>]
[<InlineData(0x2C5F, 0x2C2F)>]
[<InlineData(0xA7C1, 0xA7C0)>]
[<InlineData(0xA7D1, 0xA7D0)>]
[<InlineData(0xA7F6, 0xA7F5)>]
let ``JYRO-REQ-381 ToUpper_ReadsUnicode15_0`` (codePoint: int, expected: int) =
    let text = cp [ codePoint ]
    Assert.Equal(cp [ expected ], strB ("ToUpper(" + lit text + ")"))
    Assert.Equal("X" + cp [ expected ] + "Y", strB ("ToUpper(" + lit ("x" + text + "y") + ")"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-382")>]
[<InlineData("")>]
[<InlineData("abc")>]
[<InlineData("\u00DF")>]
[<InlineData("\uFB01")>]
[<InlineData("\u0149")>]
[<InlineData("\u0390")>]
[<InlineData("\u1E96x\u01F0")>]
let ``JYRO-REQ-382 ToUpper_Result_HasSameUtf16Length`` (text: string) =
    Assert.Equal(text.Length, (str ("ToUpper(" + lit text + ")")).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-382")>]
let ``JYRO-REQ-382 ToUpper_SupplementaryAndLoneSurrogate_KeepUtf16Length`` () =
    let deseret = cp [ 0x10428 ]
    let input = inputOf [ "d", deseret; "h", hi; "l", lo ]
    Assert.Equal(2, (strIn input "ToUpper(Input.d)").Length)
    Assert.Equal(1, (strIn input "ToUpper(Input.h)").Length)
    Assert.Equal(1, (strIn input "ToUpper(Input.l)").Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-382")>]
let ``JYRO-REQ-382 ToUpper_LongMixedString_KeepsUtf16Length`` () =
    // The piece is 11 code units: a, U+00DF, b, U+FB01, U+0149, U+0390, C, 1,
    // space, U+00E9, full stop. U+00DF, U+FB01, U+0149 and U+0390 have a full
    // uppercase mapping of two or three code units but no simple one (10.6.1),
    // so a full mapping would grow the text and the simple one keeps it at 1100.
    let text = String.replicate 100 "a\u00DFb\uFB01\u0149\u0390C1 \u00E9."
    Assert.Equal(1100, text.Length)
    Assert.Equal(1100, (str ("ToUpper(" + lit text + ")")).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-384")>]
let ToUpper_SupplementaryCodePoint_MappedAsOneCodePoint () =
    let input = inputOf [ "d", cp [ 0x10428 ]; "a", cp [ 0x1E922 ] ]
    Assert.Equal(cp [ 0x10400 ], strIn input "ToUpper(Input.d)")
    Assert.Equal(cp [ 0x1E900 ], strIn input "ToUpper(Input.a)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-384")>]
let ToUpper_UnpairedSurrogate_Unchanged_NeighborsMapped () =
    let input = surrogates ()
    Assert.Equal(hi, strIn input "ToUpper(Input.hi)")
    Assert.Equal(lo, strIn input "ToUpper(Input.lo)")
    Assert.Equal("A" + hi + "B", strIn input "ToUpper(\"a\" & Input.hi & \"b\")")
    Assert.Equal("A" + lo + "B", strIn input "ToUpper(\"a\" & Input.lo & \"b\")")
    // a low surrogate before a high one is two unpaired surrogates, not a pair
    Assert.Equal(lo + hi, strIn input "ToUpper(Input.lo & Input.hi)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-385")>]
let ToUpper_Null_ReturnsEmptyString () =
    Assert.Equal("", str "ToUpper(null)")
    Assert.Equal("", str "ToUpper(Input.missing)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1979")>]
[<InlineData("true", "TRUE")>]
[<InlineData("false", "FALSE")>]
[<InlineData("42", "42")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("-0.5", "-0.5")>]
[<InlineData("ToNumber(\"1e30\")", "1E+30")>]
[<InlineData("Power(2, 0.5)", "1.4142135623730951")>]
let ``JYRO-REQ-1979 ToUpper_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str ("ToUpper(" + expr + ")"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-387")>]
[<MemberData(nameof badValues)>]
let ToUpper_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch ("ToUpper(" + bad + ")")
    expectErrPre MessageCode.ArgumentTypeMismatch ("var v = " + bad) "ToUpper(v)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-388")>]
let ToUpper_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("A", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "ToUpper(\"a\")"))

// ============================================================================
// 91.2 ToLower
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-389")>]
let ToLower_PositionalAndNamedText_ReturnString () =
    Assert.Equal("abc", str "ToLower(\"ABC\")")
    Assert.Equal("abc", str "ToLower(text: \"ABC\")")
    Assert.Equal("string", typeOf "ToLower(\"A\")")
    Assert.Equal("string", typeOf "ToLower(null)")
    Assert.Equal("string", typeOf "ToLower(5)")
    Assert.Equal("string", typeOf "ToLower(true)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-389")>]
let ToLower_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "ToLower()"
    expectErr MessageCode.TooManyArguments "ToLower(\"a\", \"b\")"
    expectErrAmong MessageCode.UnknownNamedArgument "ToLower(value: \"a\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-390")>]
[<InlineData("ABC", "abc")>]
[<InlineData("HeLLo 1!", "hello 1!")>]
[<InlineData("\u00C0", "\u00E0")>]
[<InlineData("\u03A3", "\u03C3")>]
[<InlineData("\u039F\u0394\u039F\u03A3", "\u03BF\u03B4\u03BF\u03C3")>]
[<InlineData("\u0130", "i")>]
[<InlineData("\u212A", "k")>]
[<InlineData("\u2126", "\u03C9")>]
[<InlineData("\u01C4", "\u01C6")>]
[<InlineData("\u1E9E", "\u00DF")>]
[<InlineData("0123456789.,;-_", "0123456789.,;-_")>]
let ``JYRO-REQ-390 ToLower_EachCodePoint_MapsToSimpleLowercase`` (text: string, expected: string) =
    Assert.Equal(expected, strB ("ToLower(" + lit text + ")"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-390")>]
let ``JYRO-REQ-390 ToLower_ReturnsNewString_ArgumentUnchanged`` () =
    let v = evalPreB "var s = \"ABC\"\nvar t = ToLower(s)" "[s, t]"
    Assert.Equal<string list>([ "ABC"; "abc" ], Values.asList v |> List.map Values.asString)

/// 10.6.1: the simple lowercase mapping of UnicodeData.txt of Unicode 15.0.0,
/// whatever the platform carries. U+A7CB and U+A7DC are unassigned in 15.0
/// (16.0 assigns them, with lowercase U+0264 and U+019B), so they map to
/// themselves; U+2C2F (added in 14.0) maps to U+2C5F.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-390")>]
[<InlineData(0xA7CB, 0xA7CB)>]
[<InlineData(0xA7DC, 0xA7DC)>]
[<InlineData(0x2C2F, 0x2C5F)>]
[<InlineData(0xA7C0, 0xA7C1)>]
[<InlineData(0xA7D0, 0xA7D1)>]
[<InlineData(0xA7F5, 0xA7F6)>]
let ``JYRO-REQ-390 ToLower_ReadsUnicode15_0`` (codePoint: int, expected: int) =
    let text = cp [ codePoint ]
    Assert.Equal(cp [ expected ], strB ("ToLower(" + lit text + ")"))
    Assert.Equal("x" + cp [ expected ] + "y", strB ("ToLower(" + lit ("X" + text + "Y") + ")"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-391")>]
[<InlineData("")>]
[<InlineData("ABC")>]
[<InlineData("\u0130")>]
[<InlineData("\u212A")>]
[<InlineData("I\u0130\u0130I")>]
let ToLower_Result_HasSameUtf16Length (text: string) =
    Assert.Equal(text.Length, (str ("ToLower(" + lit text + ")")).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-391")>]
let ToLower_SupplementaryAndLoneSurrogate_KeepUtf16Length () =
    let input = inputOf [ "d", cp [ 0x10400 ]; "h", hi; "l", lo ]
    Assert.Equal(2, (strIn input "ToLower(Input.d)").Length)
    Assert.Equal(1, (strIn input "ToLower(Input.h)").Length)
    Assert.Equal(1, (strIn input "ToLower(Input.l)").Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-391")>]
let ToLower_LongMixedString_KeepsUtf16Length () =
    let text = String.replicate 100 "A\u0130b\u212AC1 \u00C9.x"
    Assert.Equal(1000, text.Length)
    Assert.Equal(1000, (str ("ToLower(" + lit text + ")")).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-393")>]
let ToLower_SupplementaryCodePoint_MappedAsOneCodePoint () =
    let input = inputOf [ "d", cp [ 0x10400 ]; "a", cp [ 0x1E900 ] ]
    Assert.Equal(cp [ 0x10428 ], strIn input "ToLower(Input.d)")
    Assert.Equal(cp [ 0x1E922 ], strIn input "ToLower(Input.a)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-393")>]
let ToLower_UnpairedSurrogate_Unchanged_NeighborsMapped () =
    let input = surrogates ()
    Assert.Equal(hi, strIn input "ToLower(Input.hi)")
    Assert.Equal(lo, strIn input "ToLower(Input.lo)")
    Assert.Equal("a" + hi + "b", strIn input "ToLower(\"A\" & Input.hi & \"B\")")
    Assert.Equal("a" + lo + "b", strIn input "ToLower(\"A\" & Input.lo & \"B\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-394")>]
let ToLower_Null_ReturnsEmptyString () =
    Assert.Equal("", str "ToLower(null)")
    Assert.Equal("", str "ToLower(Input.missing)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1980")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("42", "42")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("ToNumber(\"1e30\")", "1e+30")>]
[<InlineData("Power(2, 0.5)", "1.4142135623730951")>]
let ``JYRO-REQ-1980 ToLower_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str ("ToLower(" + expr + ")"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-396")>]
[<MemberData(nameof badValues)>]
let ToLower_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch ("ToLower(" + bad + ")")
    expectErrPre MessageCode.ArgumentTypeMismatch ("var v = " + bad) "ToLower(v)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-397")>]
let ToLower_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("a", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "ToLower(\"A\")"))

// ============================================================================
// 91.3 Trim
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-398")>]
let Trim_PositionalAndNamedText_ReturnString () =
    Assert.Equal("a", str "Trim(\" a \")")
    Assert.Equal("a", str "Trim(text: \" a \")")
    Assert.Equal("string", typeOf "Trim(\" a \")")
    Assert.Equal("string", typeOf "Trim(null)")
    Assert.Equal("string", typeOf "Trim(5)")
    Assert.Equal("string", typeOf "Trim(true)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-398")>]
let Trim_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "Trim()"
    expectErr MessageCode.TooManyArguments "Trim(\"a\", \"b\")"
    expectErrAmong MessageCode.UnknownNamedArgument "Trim(value: \"a\")"

/// Every code point with the Unicode White_Space property.
let whiteSpace : obj[] seq =
    [ 0x09; 0x0A; 0x0B; 0x0C; 0x0D; 0x20; 0x85; 0xA0; 0x1680
      0x2000; 0x2001; 0x2002; 0x2003; 0x2004; 0x2005; 0x2006; 0x2007; 0x2008; 0x2009; 0x200A
      0x2028; 0x2029; 0x202F; 0x205F; 0x3000 ]
    |> Seq.map (fun c -> [| box c |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1735")>]
[<MemberData(nameof whiteSpace)>]
let ``JYRO-REQ-1735 Trim_WhiteSpaceCodePointAtBothEnds_Removed`` (codePoint: int) =
    let w = cp [ codePoint ]
    Assert.Equal("a b", strB ("Trim(" + lit (w + "a b" + w) + ")"))
    Assert.Equal("a b", strB ("Trim(" + lit (w + w + w + "a b") + ")"))
    Assert.Equal("a b", strB ("Trim(" + lit ("a b" + w + w) + ")"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1735")>]
let ``JYRO-REQ-1735 Trim_MixedRunsAndAllWhiteSpace_Removed`` () =
    let mix = " \t\n\r\u000B\u000C\u0085\u00A0\u1680\u2003\u2028\u2029\u202F\u205F\u3000"
    Assert.Equal("a b", strB ("Trim(" + lit (mix + "a b" + mix) + ")"))
    Assert.Equal("", strB ("Trim(" + lit mix + ")"))
    Assert.Equal("", strB "Trim(\"\")")
    Assert.Equal("", strB "Trim(\"   \")")

/// 10.6.1: White_Space of Unicode 15.0.0. U+180E is not White_Space (since
/// 6.3), so it is kept; U+0085, U+2028 and U+3000 are, so they are removed.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1735")>]
let ``JYRO-REQ-1735 Trim_ReadsWhiteSpaceOfUnicode15_0`` () =
    let mvs = "\u180E"
    Assert.Equal(mvs + "a b" + mvs, strB ("Trim(" + lit (mvs + "a b" + mvs) + ")"))
    Assert.Equal(mvs, strB ("Trim(" + lit (" " + mvs + " ") + ")"))
    for w in [ "\u0085"; "\u2028"; "\u3000" ] do
        Assert.Equal("a b", strB ("Trim(" + lit (w + "a b" + w) + ")"))
    Assert.Equal(mvs, strB ("Trim(" + lit ("\u3000\u0085" + mvs + "\u2028") + ")"))

/// JYRO-REQ-1735: the statement's own examples.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1735")>]
let ``JYRO-REQ-1735 Trim_StatementExamples`` () =
    Assert.Equal("a b", strB ("Trim(" + lit ("\u00A0 a b\u3000") + ")"))
    Assert.Equal("\u200Ba", strB ("Trim(" + lit "\u200Ba" + ")"))
    Assert.Equal("\u180Ea", strB ("Trim(" + lit "\u180Ea" + ")"))
    Assert.Equal("", strB "Trim(\" \\t \")")

/// A code point is removed exactly when it and every code point between it and
/// the nearer end have White_Space: a non-White_Space code point between two
/// White_Space runs stops both runs, and all between the first and the last
/// such code point is kept.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1735")>]
let ``JYRO-REQ-1735 Trim_NonWhiteSpaceBetweenTwoRuns_KeepsFromFirstToLastNonWhiteSpace`` () =
    let z = "\u200B"
    Assert.Equal(z, strB ("Trim(" + lit ("  " + z + "  ") + ")"))
    Assert.Equal(z + " " + z, strB ("Trim(" + lit (" " + z + " " + z + " ") + ")"))
    Assert.Equal(z + " a", strB ("Trim(" + lit (" \u00A0" + z + " a") + ")"))
    // one-sided runs
    Assert.Equal("a b", strB ("Trim(" + lit ("\u3000 a b") + ")"))
    Assert.Equal("a b", strB ("Trim(" + lit ("a b\u3000 ") + ")"))
    Assert.Equal(z + "a", strB ("Trim(" + lit ("\u3000 " + z + "a") + ")"))
    Assert.Equal("a" + z, strB ("Trim(" + lit ("a" + z + "\u3000 ") + ")"))
    // inner code units unchanged
    Assert.Equal("a \u00A0\u3000 b", strB ("Trim(" + lit (" a \u00A0\u3000 b ") + ")"))

/// A lone surrogate is not White_Space: at the edge it is kept, and it stops the
/// run. It reaches the script through `Input`, as a literal cannot hold it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1735")>]
let ``JYRO-REQ-1735 Trim_LoneSurrogateAtTheEdge_IsKept`` () =
    let input = inputOf [ "t", " " + hi + " "; "u", " " + lo + "a" + hi + " " ]
    Assert.Equal(hi, strInB input "Trim(Input.t)")
    Assert.Equal(lo + "a" + hi, strInB input "Trim(Input.u)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-400")>]
[<InlineData(0x200B)>]
[<InlineData(0xFEFF)>]
[<InlineData(0x180E)>]
[<InlineData(0x001C)>]
[<InlineData(0x001D)>]
[<InlineData(0x001E)>]
[<InlineData(0x001F)>]
[<InlineData(0x200C)>]
[<InlineData(0x200D)>]
[<InlineData(0x2060)>]
[<InlineData(0x00AD)>]
let Trim_NonWhiteSpaceCodePoint_NotRemoved (codePoint: int) =
    let c = cp [ codePoint ]
    let text = c + "a" + c
    Assert.Equal(text, str ("Trim(" + lit text + ")"))
    // also when it sits outside white space that is removed
    Assert.Equal(text, str ("Trim(" + lit (" " + text + " ") + ")"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-401")>]
let Trim_InnerCodeUnits_Unchanged () =
    Assert.Equal("a \t b\n c", str ("Trim(" + lit "  a \t b\n c  " + ")"))
    Assert.Equal("a\u3000\u00A0  b", str ("Trim(" + lit " a\u3000\u00A0  b\u3000" + ")"))
    let input = surrogates ()
    Assert.Equal("a" + hi + " " + lo + "b", strIn input "Trim(\" a\" & Input.hi & \" \" & Input.lo & \"b \")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-402")>]
let Trim_Null_ReturnsEmptyString () =
    Assert.Equal("", str "Trim(null)")
    Assert.Equal("", str "Trim(Input.missing)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1981")>]
[<InlineData("42", "42")>]
[<InlineData("-1.50", "-1.5")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("ToNumber(\"1e30\")", "1e+30")>]
let ``JYRO-REQ-1981 Trim_NumberOrBoolean_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str ("Trim(" + expr + ")"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-404")>]
[<MemberData(nameof badValues)>]
let Trim_ContainerOrFunction_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch ("Trim(" + bad + ")")
    expectErrPre MessageCode.ArgumentTypeMismatch ("var v = " + bad) "Trim(v)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-405")>]
let Trim_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("a", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "Trim(\" a \")"))

// ============================================================================
// Culture (91.1.4, 91.2.4, 91.6.5, 91.7.5): not run in parallel
// ============================================================================

[<Collection("P91 culture")>]
type CultureTests() =

    let under (f: unit -> 'T) : 'T list = cultures |> List.map (fun c -> withCulture c f)

    let sameEverywhere (probe: string) (expected: JyroValue -> unit) =
        let results = under (fun () -> eval probe)
        for v in results do expected v
        let texts = results |> List.map Values.toJson |> List.distinct
        Assert.True(texts.Length = 1, sprintf "%s differs by culture: %A" probe texts)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-383")>]
    member _.``JYRO-REQ-383 ToUpper_UnderEveryCulture_GivesSameResult``() =
        sameEverywhere "ToUpper(\"i\")" (fun v -> Assert.Equal("I", Values.asString v))
        sameEverywhere "ToUpper(\"istanbul\")" (fun v -> Assert.Equal("ISTANBUL", Values.asString v))
        sameEverywhere "ToUpper(\"\\u0131\")" (fun v -> Assert.Equal("I", Values.asString v))
        sameEverywhere "ToUpper(\"\\u00E0i\\u0307\")" (fun v -> Assert.Equal("\u00C0I\u0307", Values.asString v))

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-392")>]
    member _.``JYRO-REQ-392 ToLower_UnderEveryCulture_GivesSameResult``() =
        sameEverywhere "ToLower(\"I\")" (fun v -> Assert.Equal("i", Values.asString v))
        sameEverywhere "ToLower(\"ISTANBUL\")" (fun v -> Assert.Equal("istanbul", Values.asString v))
        sameEverywhere "ToLower(\"\\u0130\")" (fun v -> Assert.Equal("i", Values.asString v))
        sameEverywhere "ToLower(\"\\u00CC\")" (fun v -> Assert.Equal("\u00EC", Values.asString v))

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-432")>]
    member _.StartsWith_UnderEveryCulture_GivesSameOrdinalResult() =
        let f v = Assert.False(Values.asBool v)
        sameEverywhere "StartsWith(\"\\u00ADabc\", \"abc\")" f
        sameEverywhere "StartsWith(\"Istanbul\", \"istanbul\")" f
        sameEverywhere "StartsWith(\"\\u00C5\", \"A\\u030A\")" f
        sameEverywhere "StartsWith(\"\\u00E6\", \"ae\")" f
        sameEverywhere "StartsWith(\"\\u0130x\", \"i\")" f

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-441")>]
    member _.EndsWith_UnderEveryCulture_GivesSameOrdinalResult() =
        let f v = Assert.False(Values.asBool v)
        sameEverywhere "EndsWith(\"abc\\u00AD\", \"abc\")" f
        sameEverywhere "EndsWith(\"ISTANBUL\", \"istanbul\")" f
        sameEverywhere "EndsWith(\"\\u00C5\", \"A\\u030A\")" f
        sameEverywhere "EndsWith(\"\\u00E6\", \"ae\")" f

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-409")>]
    member _.Replace_UnderTurkishCulture_NoCaseFolding() =
        withCulture "tr-TR" (fun () ->
            Assert.Equal("I", str "Replace(\"I\", \"i\", \"x\")")
            Assert.Equal("\u0130", str "Replace(\"\\u0130\", \"i\", \"x\")"))

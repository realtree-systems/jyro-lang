/// jyro-spec 95.10 NewGuid, 95.11 Base64Encode, 95.12 Base64Decode.
module RealTree.Jyro.Spec.Tests.S95_10_12

open System
open System.Text
open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

// =========================================================================
// 95.10 NewGuid
// =========================================================================

let private guids : Lazy<string list> =
    lazy (
        let src = "var a = []\nfor i in 0 to 1000 do a = Append(a, NewGuid()) end\nOutput.r = a"
        let out = Script.output (Script.runWith { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited } src)
        Values.asList (Values.prop "r" out) |> List.map Values.asString)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1141")>]
let ``NewGuid_TakesNoArgumentAndReturnsAString`` () =
    Assert.Equal("string", rStr "Output.r = TypeOf(NewGuid())")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1141")>]
[<InlineData("NewGuid(1)")>]
[<InlineData("NewGuid(null)")>]
let ``NewGuid_WithAnArgument_FailsToCompileWithTooManyArguments`` (call: string) =
    compileFails MessageCode.TooManyArguments (sprintf "Output.r = %s" call)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1142")>]
let ``NewGuid_Returns36CharactersIn8_4_4_4_12HexGroups`` () =
    let re = Regex("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
    for g in guids.Force() do
        Assert.Equal(36, g.Length)
        Assert.True(re.IsMatch g, sprintf "not 8-4-4-4-12 hex: [%s]" g)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1143")>]
let ``NewGuid_HexDigitsAreLowerCase`` () =
    let all = guids.Force()
    for g in all do
        Assert.True(g |> Seq.forall (fun c -> not (c >= 'A' && c <= 'F')), sprintf "upper-case digit in %s" g)
    Assert.True(all |> List.exists (fun g -> g |> Seq.exists (fun c -> c >= 'a' && c <= 'f')), "no letter digit seen in 1000 results")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1144")>]
let ``NewGuid_Position14Is4_AndPosition19Is8_9_aOrb`` () =
    for g in guids.Force() do
        Assert.Equal('4', g.[14])
        Assert.Contains(g.[19], "89ab")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1144")>]
let ``NewGuid_RandomBitsVary_AllResultsDistinct`` () =
    let all = guids.Force()
    Assert.Equal(all.Length, (List.distinct all).Length)
    // every hexadecimal position other than 14 (version) takes more than one value
    for pos in [ 0 .. 35 ] |> List.filter (fun p -> p <> 8 && p <> 13 && p <> 14 && p <> 18 && p <> 23) do
        let seen = all |> List.map (fun g -> g.[pos]) |> List.distinct
        Assert.True(seen.Length > 1, sprintf "position %d is constant" pos)

/// The supporting test of JYRO-REQ-1145's inspection (the strength of the
/// source is judged by the inspection group; a test can only refute it):
/// 10,000 results are distinct, and each of the 122 random bit positions takes
/// both values. Bits 48 to 51 (the version) and 64 and 65 (the variant) are fixed.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1145")>]
let ``JYRO-REQ-1145 NewGuid_10000Results_AreDistinct_AndEachRandomBitTakesBothValues`` () =
    let src = "var a = []\nfor i in 0 to 1000 do a = Append(a, NewGuid()) end\nOutput.r = a"
    let all =
        [ for _ in 1 .. 10 do
            let out = Script.output (Script.runWith { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited } src)
            yield! Values.asList (Values.prop "r" out) |> List.map Values.asString ]
    Assert.Equal(10000, all.Length)
    Assert.Equal(10000, (List.distinct all).Length)
    let bitsOf (g: string) =
        let hex = g.Replace("-", "")
        Array.init 128 (fun b -> (Convert.ToInt32(string hex.[b / 4], 16) >>> (3 - b % 4)) &&& 1)
    let seen = Array.init 128 (fun _ -> Array.zeroCreate<bool> 2)
    for g in all do
        bitsOf g |> Array.iteri (fun b v -> seen.[b].[v] <- true)
    let fixedBits = set [ 48; 49; 50; 51; 64; 65 ]
    let stuck = [ for b in 0 .. 127 do if not (fixedBits.Contains b) && not (seen.[b].[0] && seen.[b].[1]) then yield b ]
    Assert.True(stuck.IsEmpty, sprintf "these random bit positions never changed in 10,000 results: %A" stuck)
    Assert.Equal(122, 128 - fixedBits.Count)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1146")>]
let ``NewGuid_HasEffectNonDeterministic`` () =
    assertEffect "NewGuid" FunctionEffect.NonDeterministic "Output.r = NewGuid()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1146")>]
let ``NewGuid_UnderPureCeiling_IsRejectedWithFunctionEffectExceedsMax`` () =
    assertRejectedUnder FunctionEffect.Pure "Output.r = NewGuid()"

// =========================================================================
// 95.11 Base64Encode
// =========================================================================

let private b64 (s: string) = Convert.ToBase64String(Encoding.UTF8.GetBytes s)

let private enc (s: string) : JyroValue =
    rVal (Values.obj [ "s", Values.str s ]) "Output.r = Base64Encode(Input.s)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1147")>]
let ``Base64Encode_TakesAStringAndReturnsAString`` () =
    Assert.Equal(norm """["string","YQ==","YQ=="]""", rJson "Output.r = [TypeOf(Base64Encode('a')), Base64Encode('a'), Base64Encode(text: 'a')]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1147")>]
let ``Base64Encode_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Base64Encode()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1147")>]
let ``Base64Encode_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Base64Encode('a', 'b')"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1148")>]
[<InlineData("f", "Zg==")>]
[<InlineData("fo", "Zm8=")>]
[<InlineData("foo", "Zm9v")>]
[<InlineData("foob", "Zm9vYg==")>]
[<InlineData("fooba", "Zm9vYmE=")>]
[<InlineData("foobar", "Zm9vYmFy")>]
[<InlineData("é", "w6k=")>]
[<InlineData("€", "4oKs")>]
[<InlineData("\U0001F600", "8J+YgA==")>]
[<InlineData("??>", "Pz8+")>]
[<InlineData("???", "Pz8/")>]
[<InlineData("ûÿ", "w7vDvw==")>]
let ``Base64Encode_EncodesUtf8BytesWithTheStandardAlphabet`` (text: string, expected: string) =
    Assert.Equal(expected, b64 text) // the vector itself, checked on the host
    Assert.Equal(expected, Values.asString (enc text))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1148")>]
let ``Base64Encode_OfScriptLiterals_EncodesUtf8`` () =
    Assert.Equal(norm """["Zm9vYmFy","w6k=","8J+YgA==","Pz8/"]""", rJson "Output.r = [Base64Encode('foobar'), Base64Encode('\\u00E9'), Base64Encode('\\uD83D\\uDE00'), Base64Encode('???')]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1149")>]
[<InlineData(1, 2)>]
[<InlineData(2, 1)>]
[<InlineData(3, 0)>]
[<InlineData(4, 2)>]
[<InlineData(5, 1)>]
[<InlineData(6, 0)>]
[<InlineData(57, 0)>]
[<InlineData(58, 2)>]
[<InlineData(59, 1)>]
let ``Base64Encode_PadsWithEqualsToAMultipleOf4`` (bytes: int, pad: int) =
    let s = String('x', bytes)
    let got = Values.asString (enc s)
    Assert.Equal(0, got.Length % 4)
    Assert.Equal(pad, got.Length - got.TrimEnd('=').Length)
    Assert.Equal(b64 s, got)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1150")>]
[<InlineData(57)>]
[<InlineData(58)>]
[<InlineData(76)>]
[<InlineData(100)>]
[<InlineData(10000)>]
let ``Base64Encode_PutsNoLineBreakOrWhiteSpace`` (bytes: int) =
    let s = String('é', bytes / 2) + (if bytes % 2 = 1 then "a" else "")
    let got = Values.asString (enc s)
    Assert.False(got |> Seq.exists (fun c -> c = '\r' || c = '\n' || c = ' ' || c = '\t'), "white space in the result")
    Assert.Equal(b64 s, got)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1151")>]
[<InlineData("Base64Encode('')")>]
[<InlineData("Base64Encode(null)")>]
[<InlineData("Base64Encode(x)")>]
[<InlineData("Base64Encode(o.missing)")>]
let ``Base64Encode_OfEmptyOrNull_ReturnsEmptyString`` (call: string) =
    Assert.Equal("", rStr (sprintf "var x = null\nvar o = {}\nOutput.r = %s" call))

/// The host strings with an unpaired surrogate, by label. Each is built here,
/// in the test, from `hi` and `lo`: a test row that carried the string itself
/// would pass through xUnit's serialization, which writes a lone surrogate as
/// U+FFFD before the test runs (JYRO-DEF-152).
let private loneByLabel (label: string) : string =
    match label with
    | "hi" -> hi
    | "lo" -> lo
    | "a_hi_b" -> "a" + hi + "b"
    | "hi_hi" -> hi + hi
    | "lo_hi" -> lo + hi
    | "x_lo" -> "x" + lo
    | "pair_lo" -> emoji + lo
    | "hi_pair" -> hi + emoji
    | "long_hi_at_end" -> String('a', 5000) + hi
    | other -> Values.fail (sprintf "no lone-surrogate string labeled %s" other)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1152")>]
[<InlineData("hi")>]
[<InlineData("lo")>]
[<InlineData("a_hi_b")>]
[<InlineData("hi_hi")>]
[<InlineData("lo_hi")>]
[<InlineData("x_lo")>]
[<InlineData("pair_lo")>]
[<InlineData("hi_pair")>]
[<InlineData("long_hi_at_end")>]
let ``JYRO-REQ-1152 Base64Encode_OfHostStringWithUnpairedSurrogate_RaisesUnpairedSurrogate`` (label: string) =
    let s = loneByLabel label
    // The string the test hands over still holds the lone surrogate.
    Assert.True(s |> Seq.exists Char.IsSurrogate, sprintf "the %s string has no surrogate" label)
    Assert.False(s.Contains '�', sprintf "the %s string holds U+FFFD" label)
    expectRuntimeError MessageCode.UnpairedSurrogate
        (Script.runInput (Values.obj [ "s", Values.str s ]) "Output.r = Base64Encode(Input.s)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1152")>]
[<InlineData("hi")>]
[<InlineData("lo")>]
[<InlineData("'a' & hi & 'b'")>]
[<InlineData("hi & hi")>]
[<InlineData("lo & hi")>]
[<InlineData("pair & lo")>]
[<InlineData("hi & pair")>]
[<InlineData("Substring('\\uD83D\\uDE00x', 1, 2)")>]
let ``JYRO-REQ-1152 Base64Encode_OfScriptStringWithUnpairedSurrogate_RaisesUnpairedSurrogate`` (expr: string) =
    expectRuntimeError MessageCode.UnpairedSurrogate (Script.run (loneDecl + sprintf "Output.r = Base64Encode(%s)" expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1152")>]
let ``JYRO-REQ-1152 Base64Encode_OfAPairedSurrogate_RaisesNoError`` () =
    // The control: a pair is not unpaired, so only a lone one raises.
    Assert.Equal("8J+YgA==", Values.asString (rVal (Values.obj [ "s", Values.str emoji ]) "Output.r = Base64Encode(Input.s)"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1153")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("1.5")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("-1")>]
[<InlineData("SquareRoot(2)")>]
[<InlineData("1 == 1")>]
[<InlineData("Input.n")>]
[<InlineData("Input.i")>]
[<InlineData("Input.b")>]
[<InlineData("text: 5")>]
let ``JYRO-REQ-1153 Base64Encode_OfNumberOrBoolean_RaisesArgumentTypeMismatch`` (arg: string) =
    let input = Values.obj [ "n", Values.exact 5m; "i", Values.inexact 0.5; "b", Values.bool false ]
    expectRuntimeError MessageCode.ArgumentTypeMismatch (Script.runInput input (sprintf "Output.r = Base64Encode(%s)" arg))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1154")>]
[<InlineData("[]")>]
[<InlineData("['a']")>]
[<InlineData("{}")>]
[<InlineData("{a: 1}")>]
[<InlineData("f")>]
let ``Base64Encode_OfContainerOrFunction_RaisesArgumentTypeMismatch`` (arg: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Base64Encode(%s)" arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1155")>]
let ``Base64Encode_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Base64Encode" FunctionEffect.Pure "Output.r = Base64Encode('a')"

// =========================================================================
// 95.12 Base64Decode
// =========================================================================

let private dec (s: string) : JyroValue =
    rVal (Values.obj [ "s", Values.str s ]) "Output.r = Base64Decode(Input.s)"

let private decodesTo (expected: string) (s: string) =
    let v = dec s
    Assert.False(isNullValue v, sprintf "Base64Decode(%A) gave null, expected %A" s expected)
    let got = Values.asString v
    Assert.True((expected = got), sprintf "Base64Decode(%A): expected [%s] got [%s]" s (units expected) (units got))

let private decodesToNull (s: string) =
    let v = dec s
    Assert.True(isNullValue v, sprintf "Base64Decode(%A) should be null, got %s" s (if isNull (box v) then "<.NET null>" else v.ToJson()))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1156")>]
let ``Base64Decode_TakesAStringAndReturnsAStringOrNull`` () =
    Assert.Equal(norm """["string","null","a"]""", rJson "Output.r = [TypeOf(Base64Decode('YQ==')), TypeOf(Base64Decode('!!!!')), Base64Decode(text: 'YQ==')]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1156")>]
let ``Base64Decode_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Base64Decode()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1156")>]
let ``Base64Decode_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Base64Decode('a', 'b')"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1157")>]
[<InlineData("", "")>]
[<InlineData("Zg==", "f")>]
[<InlineData("Zm8=", "fo")>]
[<InlineData("Zm9v", "foo")>]
[<InlineData("Zm9vYg==", "foob")>]
[<InlineData("Zm9vYmE=", "fooba")>]
[<InlineData("Zm9vYmFy", "foobar")>]
[<InlineData("w6k=", "é")>]
[<InlineData("4oKs", "€")>]
[<InlineData("8J+YgA==", "\U0001F600")>]
[<InlineData("77u/YQ==", "﻿a")>]
[<InlineData("AA==", "\u0000")>]
[<InlineData("Pz8+", "??>")>]
[<InlineData("Pz8/", "???")>]
[<InlineData("w7vDvw==", "ûÿ")>]
[<InlineData("776/", "﾿")>]
[<InlineData("5Lit5paH", "中文")>]
let ``JYRO-REQ-1157 Base64Decode_DecodesStandardAlphabetToTheUtf8String`` (text: string, expected: string) =
    // the vector itself, checked on the host: RFC 4648 section 4, then strict UTF-8
    Assert.Equal(expected, UTF8Encoding(false, true).GetString(Convert.FromBase64String text))
    decodesTo expected text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1157")>]
let ``JYRO-REQ-1157 Base64Decode_OfEmojiGivesASurrogatePairOfLength2`` () =
    Assert.Equal(norm "[2,true]", rJson "var s = Base64Decode('8J+YgA==')\nOutput.r = [Length(s), s == '\\uD83D\\uDE00']")

/// Text whose standard Base64 uses every one of the 64 characters of the
/// alphabet: every code point below U+0800, some 3-byte characters and an astral one.
let private wideText =
    String(Array.init 0x800 char) + "€中�￿" + emoji

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1157")>]
let ``JYRO-REQ-1157 Base64Decode_ReadsEveryCharacterOfTheStandardAlphabet`` () =
    let text = Convert.ToBase64String(Encoding.UTF8.GetBytes wideText)
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    let missing = alphabet |> Seq.filter (fun c -> not (text.Contains c)) |> Seq.toList
    Assert.True(missing.IsEmpty, sprintf "the test text does not use %A" missing)
    let got = rValB (Values.obj [ "t", Values.str text ]) "Output.r = Base64Decode(Input.t)"
    Assert.False(isNullValue got, "Base64Decode gave null")
    Assert.True((wideText = Values.asString got), "Base64Decode did not give the UTF-8 string of the bytes")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1157")>]
[<InlineData("Pz8-", "??>")>]
[<InlineData("Pz8_", "???")>]
[<InlineData("8J-YgA==", "\U0001F600")>]
let ``JYRO-REQ-1157 Base64Decode_OfTheUrlSafeAlphabet_DoesNotGiveTheString`` (text: string, notExpected: string) =
    // RFC 4648 section 5 (- for +, _ for /) is not the alphabet of section 4.
    let v = dec text
    Assert.False((not (isNullValue v)) && Values.asString v = notExpected, sprintf "Base64Decode(%s) decoded it as standard Base64" text)

let roundTrips : obj[] seq =
    let ascii = String(Array.init 128 char)
    [ ""; "a"; "ab"; "abc"; ascii; "é"; "€"; emoji; "﻿start"; "￿"
      String.replicate 1000 ("aé€" + emoji + "\u0000߿ࠀ퟿")
      String(Array.init 0xD800 (fun i -> char i)) ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1158")>]
[<MemberData(nameof roundTrips)>]
let ``Base64Decode_OfBase64Encode_GivesBackTheString`` (s: string) =
    let v = rVal (Values.obj [ "s", Values.str s ]) "Output.r = Base64Decode(Base64Encode(Input.s))"
    Assert.False(isNullValue v, "the round trip gave null")
    Assert.True((s = Values.asString v), "the round trip changed the string")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1158")>]
let ``Base64Decode_OfBase64Encode_ScriptStrings_EqualInScript`` () =
    Assert.Equal(norm "[true,true,true]", rJson "Output.r = [Base64Decode(Base64Encode('x\\ty\\n\\0')) == 'x\\ty\\n\\0', Base64Decode(Base64Encode('\\uD83D\\uDE00')) == '\\uD83D\\uDE00', Base64Decode(Base64Encode('')) == '']")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1159")>]
[<InlineData("Y")>]
[<InlineData("YQ")>]
[<InlineData("YQ=")>]
[<InlineData("YQ===")>]
[<InlineData("YWJ")>]
[<InlineData("YWJjZ")>]
[<InlineData("Zm9vYmFy=")>]
[<InlineData("Y Q")>]
let ``Base64Decode_WhenAlphabetCountNotMultipleOf4_ReturnsNull`` (text: string) =
    decodesToNull text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1159")>]
let ``Base64Decode_WhiteSpaceIsNotCounted`` () =
    decodesTo "a" "YQ =="

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1862")>]
[<InlineData("-_-_")>]
[<InlineData("YQ-_")>]
[<InlineData("Y-Q==")>]
[<InlineData("Y_Q==")>]
[<InlineData("YQ==\u000B")>]
[<InlineData("YQ==\u000C")>]
[<InlineData("YQ== ")>]
[<InlineData("YQ ==")>]
[<InlineData("YQ ==")>]
[<InlineData("YQ ==")>]
[<InlineData("YQ​==")>]
[<InlineData("YQ﻿==")>]
[<InlineData("YQ\u001F==")>]
[<InlineData("YQ,==")>]
[<InlineData("YQ:==")>]
[<InlineData("Y@Q==")>]
[<InlineData("Y[Q==")>]
[<InlineData("Y`Q==")>]
[<InlineData("Y{Q==")>]
[<InlineData("Y\u007FQ==")>]
[<InlineData("YQ==.")>]
[<InlineData("YQ*=")>]
[<InlineData("YéQ=")>]
[<InlineData("YQ\u0000==")>]
[<InlineData("YQ\u000B==")>]
[<InlineData("YQ\u000C==")>]
[<InlineData("YQ　==")>]
[<InlineData("YQ\u0085==")>]
let ``JYRO-REQ-1862 Base64Decode_WithCharacterOutsideAlphabetEqualsAndTheFourIgnoredCharacters_ReturnsNull`` (text: string) =
    decodesToNull text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1862")>]
[<InlineData("YQ== ")>]
[<InlineData("YQ==\t")>]
[<InlineData("YQ==\r")>]
[<InlineData("YQ==\n")>]
[<InlineData("YQ==\r\n")>]
[<InlineData(" Y\tQ\r=\n=")>]
let ``JYRO-REQ-1862 Base64Decode_WithSpaceTabCrOrLf_StillDecodes`` (text: string) =
    decodesTo "a" text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1862")>]
let ``JYRO-REQ-1862 Base64Decode_AcceptsEveryCharacterOfTheStandardAlphabet`` () =
    // All 64 characters of RFC 4648 section 4, each as the last character of "QUJ?": the bytes are
    // 'A', 'B' and 0x40 + the value of the character, so the decoded text is "AB" and one ASCII character.
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    for i in 0 .. 63 do
        let text = "QUJ" + string alphabet.[i]
        let v = rValB (Values.obj [ "t", Values.str text ]) "Output.r = Base64Decode(Input.t)"
        Assert.False(v.IsNull, sprintf "Base64Decode(%s) was null" text)
        Assert.Equal("AB" + string (char (0x40 + i)), Values.asString v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1842")>]
[<InlineData("=YQ=")>]
[<InlineData("Y=Q=")>]
[<InlineData("YQ==YQ==")>]
[<InlineData("Y===")>]
[<InlineData("====")>]
[<InlineData("YWJj=A==")>]
[<InlineData("YW=j")>]
[<InlineData("YQ==AAAA")>]
[<InlineData("YQ==\nAAAA")>]
[<InlineData("YQ= =AAAA")>]
[<InlineData("YWJj\n=A==")>]
[<InlineData("\n=YQ=")>]
[<InlineData("= YQ=")>]
let ``JYRO-REQ-1842 Base64Decode_WithEqualsNotAtTheEnd_ReturnsNull`` (text: string) =
    decodesToNull text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1842")>]
[<InlineData("YQ= =")>]
[<InlineData("YQ==\n")>]
let ``JYRO-REQ-1842 Base64Decode_WithWhiteSpaceBetweenOrAfterFinalEquals_Decodes`` (text: string) =
    decodesTo "a" text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1842")>]
[<InlineData("YQ==\r\n\t ", "a")>]
[<InlineData("YWI=", "ab")>]
[<InlineData("YWI= \n", "ab")>]
[<InlineData("YWI =\n", "ab")>]
[<InlineData("YWJj", "abc")>]
[<InlineData("Y Q = =", "a")>]
[<InlineData("YWJj\nYQ==", "abca")>]
let ``JYRO-REQ-1842 Base64Decode_WithPaddingAtTheEndOfTheCountedCharacters_Decodes`` (text: string, expected: string) =
    decodesTo expected text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1162")>]
[<InlineData("Y Q = =")>]
[<InlineData("YQ\t==")>]
[<InlineData("Y\r\nQ==")>]
[<InlineData("\nYQ==\n")>]
let ``Base64Decode_IgnoresSpaceTabCrAndLf`` (text: string) =
    decodesTo "a" text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1162")>]
let ``Base64Decode_OfMimeWrappedText_IgnoresCrlfLineEnds`` () =
    let s = String.init 200 (fun i -> string (char (32 + i % 90)))
    let flat = b64 s
    let wrapped = flat |> Seq.chunkBySize 76 |> Seq.map String |> String.concat "\r\n"
    decodesTo s wrapped

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1162")>]
let ``Base64Decode_OfOnlyWhiteSpace_ReturnsEmptyString`` () =
    decodesTo "" "   "
    decodesTo "" " \t\r\n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1163")>]
let ``Base64Decode_OfEmptyString_ReturnsEmptyString`` () =
    Assert.Equal(norm """["string",0]""", rJson "var s = Base64Decode('')\nOutput.r = [TypeOf(s), Length(s)]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1164")>]
[<InlineData("gA==")>]
[<InlineData("/w==")>]
[<InlineData("ww==")>]
[<InlineData("wK8=")>]
[<InlineData("7aCA")>]
[<InlineData("9JCAgA==")>]
[<InlineData("YYA=")>]
[<InlineData("4oI=")>]
[<InlineData("+A==")>]
[<InlineData("+/8=")>]
[<InlineData("7b+/")>]
[<InlineData("4ICA")>]
[<InlineData("8ICAgA==")>]
[<InlineData("/g==")>]
[<InlineData("YcI=")>]
[<InlineData("wEE=")>]
[<InlineData("4oJB")>]
let ``JYRO-REQ-1164 Base64Decode_OfInvalidUtf8Bytes_ReturnsNull`` (text: string) =
    // The vector itself, checked on the host: the bytes are standard Base64 and not UTF-8.
    let bytes = Convert.FromBase64String text
    Assert.Throws<DecoderFallbackException>(fun () -> UTF8Encoding(false, true).GetString bytes |> ignore) |> ignore
    decodesToNull text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1164")>]
let ``JYRO-REQ-1164 Base64Decode_OfInvalidUtf8Bytes_ReturnsNullWithNoError_AndTheRunGoesOn`` () =
    let res = Script.run "var a = Base64Decode('/w==')\nvar b = Base64Decode('YYA=')\nOutput.r = [a == null, b == null]\nOutput.after = 1"
    Assert.True(Script.ok res, Script.describe res)
    let out = Script.output res
    Assert.Equal(norm "[true,true]", Values.toJson (Values.prop "r" out))
    Assert.Equal(1m, Values.asDecimal (Values.prop "after" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1164")>]
let ``JYRO-REQ-1164 Base64Decode_OfLongTextWithOneInvalidByteAtTheEnd_ReturnsNull`` () =
    let bytes = Array.append (Encoding.UTF8.GetBytes(String('a', 9000))) [| 0xFFuy |]
    let v = dec (Convert.ToBase64String bytes)
    Assert.True(isNullValue v, sprintf "Base64Decode of 9,000 'a' then FF should be null, got a %A of length %d" v.ValueType (if v :? JyroString then (Values.asString v).Length else -1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1164")>]
let ``JYRO-REQ-1164 Base64Decode_OfBomBytes_ReturnsU_FEFF`` () =
    decodesTo "﻿" "77u/"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1164")>]
[<InlineData("776/", "﾿")>]
[<InlineData("77+/", "￿")>]
[<InlineData("77+9", "�")>]
[<InlineData("7p+/", "")>]
[<InlineData("9I+/vw==", "\U0010FFFF")>]
let ``JYRO-REQ-1164 Base64Decode_OfValidUtf8AtTheEdges_IsNotNull`` (text: string, expected: string) =
    // The controls: bytes that are UTF-8, next to the invalid ones above.
    Assert.Equal(expected, UTF8Encoding(false, true).GetString(Convert.FromBase64String text))
    decodesTo expected text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1165")>]
[<InlineData("Base64Decode(null)")>]
[<InlineData("Base64Decode(x)")>]
[<InlineData("Base64Decode(o.missing)")>]
[<InlineData("Base64Decode(text: null)")>]
[<InlineData("Base64Decode(Input.z)")>]
[<InlineData("Base64Decode(Input.missing)")>]
let ``JYRO-REQ-1165 Base64Decode_OfNull_RaisesArgumentTypeMismatch`` (call: string) =
    expectRuntimeError MessageCode.ArgumentTypeMismatch
        (Script.runJson """{"z":null}""" (sprintf "var x = null\nvar o = {}\nOutput.r = %s" call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1166")>]
[<InlineData("0")>]
[<InlineData("1234")>]
[<InlineData("1.5")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("-4")>]
[<InlineData("SquareRoot(2)")>]
[<InlineData("1 == 1")>]
[<InlineData("Input.n")>]
[<InlineData("Input.i")>]
[<InlineData("Input.b")>]
[<InlineData("text: 1234")>]
let ``JYRO-REQ-1166 Base64Decode_OfNumberBooleanOrContainer_RaisesArgumentTypeMismatch`` (arg: string) =
    let input = Values.obj [ "n", Values.exact 1234m; "i", Values.inexact 0.5; "b", Values.bool true ]
    expectRuntimeError MessageCode.ArgumentTypeMismatch (Script.runInput input (sprintf "Output.r = Base64Decode(%s)" arg))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1167")>]
let ``Base64Decode_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Base64Decode" FunctionEffect.Pure "Output.r = Base64Decode('YQ==')"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1168")>]
let ``Base64Decode_IgnoresNonZeroPadBits_TheTwoExamples`` () =
    decodesTo "a" "YR=="
    decodesTo "ab" "YWJ="

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1168")>]
let ``Base64Decode_IgnoresPadBits_ForAll16SecondCharacters`` () =
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    // 'Q' is index 16; indices 16..31 share the high two bits that give 'a' (0x61).
    for i in 16 .. 31 do
        decodesTo "a" (sprintf "Y%c==" alphabet.[i])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1168")>]
[<InlineData("YWI=", "ab")>]
[<InlineData("YWK=", "ab")>]
[<InlineData("YWL=", "ab")>]
[<InlineData("Zm9vYR==", "fooa")>]
[<InlineData("Y R = =", "a")>]
let ``Base64Decode_IgnoresPadBits_OtherCases`` (text: string, expected: string) =
    decodesTo expected text

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1168")>]
[<InlineData("YR")>]
[<InlineData("YR=")>]
let ``Base64Decode_PadBitsCaseWithBadLength_StillReturnsNull`` (text: string) =
    decodesToNull text

// =========================================================================
// Chunk seams (d-508, d-509, d-510): characters and padding at every offset
// around the 4,096 and 8,192 boundaries where work over long text is divided.
// =========================================================================

/// The lengths around the two seams: 4,088 to 4,100 and 8,184 to 8,196.
let private seamLengths = [ 4088 .. 4100 ] @ [ 8184 .. 8196 ]

/// Texts that put a multi-unit or multi-byte character at every offset around
/// the seams, in UTF-16 code units and in UTF-8 bytes.
let private seamTexts : string list =
    [ for n in seamLengths -> String('a', n) + "\U0001F600" + "b" ]           // a surrogate pair across 4,096 / 8,192 code units
    @ [ for n in seamLengths -> String('a', n) + "\U0001F600" ]               // 4-byte UTF-8 split 3|1, 2|2, 1|3 at 4,096 bytes
    @ [ for m in 2040 .. 2056 -> String('é', m) + "\U0001F600" ]         // 2-byte characters around 4,096 bytes
    @ [ for m in 2040 .. 2056 -> String('é', m) ]
    @ [ for m in 1360 .. 1370 -> String('中', m) + "z" ]                  // 3-byte characters around 4,096 bytes

let private stringArray (xs: string list) : JyroValue = Values.arr (xs |> List.map Values.str)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1148")>]
let ``Base64Encode_OfTextWithAMultiByteCharacterAtEverySeamOffset_EncodesItsUtf8Bytes`` () =
    let got = rValB (Values.obj [ "xs", stringArray seamTexts ]) "Output.r = Map(Input.xs, x => Base64Encode(x))" |> Values.asList
    let bad =
        List.zip seamTexts got
        |> List.filter (fun (s, g) -> Values.asString g <> Convert.ToBase64String(Encoding.UTF8.GetBytes s))
        |> List.map (fun (s, _) -> s.Length)
    Assert.True(bad.IsEmpty, sprintf "Base64Encode differs from the Base64 of the UTF-8 bytes for the texts of these lengths: %A" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1158")>]
let ``Base64Decode_OfBase64Encode_IsTheString_AtEverySeamOffset`` () =
    let got = rValB (Values.obj [ "xs", stringArray seamTexts ]) "Output.r = Map(Input.xs, x => Base64Decode(Base64Encode(x)) == x)" |> Values.asList
    let bad = List.zip seamTexts got |> List.filter (fun (_, g) -> not (Values.asBool g)) |> List.map (fun (s, _) -> s.Length)
    Assert.True(bad.IsEmpty, sprintf "The round trip lost data for the texts of these lengths: %A" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1842")>]
let ``JYRO-REQ-1842 Base64Decode_PaddingAtTheEndOfAnInnerChunk_IsNull`` () =
    // d-510's case: "AA==" ends at character 4,096, and "AAAA" follows it.
    let text = String.replicate 1023 "AAAA" + "AA==" + "AAAA"
    let v = rValB (Values.obj [ "t", Values.str text ]) "Output.r = Base64Decode(Input.t)"
    Assert.True(v.IsNull, sprintf "Base64Decode gave a %A for padding in the middle" v.ValueType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1842")>]
let ``JYRO-REQ-1842 Base64Decode_EqualsSignAtEveryPositionAroundTheSeams_IsNull`` () =
    // A text of 8,204 'A' with "=" (and with "==") at each position from 4,080 to
    // 4,100 and 8,180 to 8,200: never in the last two places, so each is null.
    let positions = [ 4080 .. 4100 ] @ [ 8180 .. 8200 ]
    let place (pad: string) (p: int) = let c = String('A', 8204).ToCharArray() in pad |> Seq.iteri (fun i ch -> c.[p + i] <- ch); String(c)
    let texts = [ for p in positions -> place "=" p ] @ [ for p in positions -> place "==" p ]
    let got = rValB (Values.obj [ "ts", stringArray texts ]) "Output.r = Map(Input.ts, t => Base64Decode(t) == null)" |> Values.asList
    let bad = List.zip (positions @ positions) got |> List.filter (fun (_, g) -> not (Values.asBool g)) |> List.map fst
    Assert.True(bad.IsEmpty, sprintf "Padding at these positions was accepted: %A" bad)
    // Control: padding in the last two places of the same length decodes.
    let ok = rValB (Values.obj [ "t", Values.str (String('A', 8202) + "==") ]) "Output.r = Base64Decode(Input.t) == null"
    Assert.False(Values.asBool ok, "Padding in the last two places was refused")

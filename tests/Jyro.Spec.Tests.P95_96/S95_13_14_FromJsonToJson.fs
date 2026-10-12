/// jyro-spec 95.13 FromJson, 95.14 ToJson.
module RealTree.Jyro.Spec.Tests.S95_13_14

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

/// Run `src` with `Input.t` = `text` (a host string, so any code unit can be given).
let private withText (text: string) (src: string) : JyroValue =
    rVal (Values.obj [ "t", Values.str text ]) src

/// `FromJson(text)` as a value.
let private fromJson (text: string) : JyroValue = withText text "Output.r = FromJson(Input.t)"

let private fromJsonNull (text: string) =
    let v = fromJson text
    Assert.True(isNullValue v, sprintf "FromJson(%A) should be null, got %s" text (if isNull (box v) then "<.NET null>" else v.ToJson()))

// =========================================================================
// 95.13 FromJson
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1169")>]
[<InlineData("null", "null")>]
[<InlineData("true", "boolean")>]
[<InlineData("1", "number")>]
[<InlineData("\"s\"", "string")>]
[<InlineData("[1]", "array")>]
[<InlineData("{\"a\":1}", "object")>]
let ``FromJson_TakesAStringAndReturnsAnyType`` (text: string, typeName: string) =
    Assert.Equal(typeName, Values.asString (withText text "Output.r = TypeOf(FromJson(Input.t))"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1169")>]
let ``FromJson_WithNamedArgument_ReturnsTheValue`` () =
    Assert.Equal(norm "[1]", rJson "Output.r = FromJson(text: '[1]')")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1169")>]
let ``FromJson_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = FromJson()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1169")>]
let ``FromJson_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = FromJson('1', '2')"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1170")>]
[<InlineData("null", "null")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("0", "0")>]
[<InlineData("-1.5e3", "-1500")>]
[<InlineData("[3,1,2]", "[3,1,2]")>]
[<InlineData("[]", "[]")>]
[<InlineData("{}", "{}")>]
[<InlineData("{\"a\":{\"b\":[null,true]}}", "{\"a\":{\"b\":[null,true]}}")>]
let ``FromJson_MapsEachJsonValueToItsJyroValue`` (text: string, expected: string) =
    Assert.Equal(expected, Values.toJson (fromJson text))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1170")>]
let ``FromJson_OfNestedObject_HasTheSameKeysAndValues`` () =
    let v = withText """{"a":{"b":[null,true]}}""" "var v = FromJson(Input.t)\nOutput.r = [TypeOf(v.a), TypeOf(v.a.b), v.a.b[0] == null, v.a.b[1], Length(v.a.b)]"
    Assert.Equal(norm """["object","array",true,true,2]""", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1170")>]
let ``FromJson_DecodesEveryStringEscape`` () =
    let text = "\"q\\\"b\\\\s\\/b\\bf\\fn\\nr\\rt\\tu\\u00e9p\\ud83d\\ude00\""
    let expected = "q\"b\\s/b\bf\fn\nr\rt\tu\u00e9p" + emoji
    let got = Values.asString (fromJson text)
    Assert.True((expected = got), sprintf "expected [%s] got [%s]" (units expected) (units got))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1171")>]
[<InlineData("0.1")>]
[<InlineData("1.50")>]
[<InlineData("1234567890.123456789012345678")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("1e28")>]
[<InlineData("1e-28")>]
[<InlineData("-0")>]
[<InlineData("1E2")>]
[<InlineData("1.00000000000000000000000000000")>]
[<InlineData("-79228162514264337593543950335")>]
[<InlineData("7.9228162514264337593543950335")>]
[<InlineData("1.2345678901234567890123456789")>]
[<InlineData("0.0000000000000000000000000001")>]
[<InlineData("1e-28")>]
[<InlineData("100e-30")>]
[<InlineData("0.00000000000000000000000000010")>]
[<InlineData("7922816251426433759354395033.5")>]
[<InlineData("123.4500000000000000000000000000000")>]
let ``JYRO-REQ-1171 FromJson_OfNumberTheExactFormHolds_IsExact`` (text: string) =
    let v = fromJson text
    Assert.True(Values.isExact v, sprintf "FromJson(%s) is not exact" text)
    Assert.Equal(Decimal.Parse(text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture), Values.asDecimal v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1171")>]
[<InlineData("0.1234567890123456789012345678901")>]
[<InlineData("1e-30")>]
[<InlineData("1e30")>]
[<InlineData("79228162514264337593543950336")>]
[<InlineData("1e-29")>]
[<InlineData("0.12345678901234567890123456789")>]
[<InlineData("-79228162514264337593543950336")>]
[<InlineData("8.9228162514264337593543950335")>]
[<InlineData("1e29")>]
[<InlineData("0.00000000000000000000000000015")>]
[<InlineData("12345678901234567890123456789.5")>]
[<InlineData("1.5e-300")>]
[<InlineData("1.7976931348623157e308")>]
let ``JYRO-REQ-1171 FromJson_OfNumberTheExactFormCannotHold_IsInexact`` (text: string) =
    let v = fromJson text
    Assert.False(Values.isExact v, sprintf "FromJson(%s) is exact" text)
    Assert.Equal(Double.Parse(text, Globalization.CultureInfo.InvariantCulture), Values.asFloat v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1171")>]
[<InlineData("[0.1, 1e-29]")>]
[<InlineData("{\"a\":0.1,\"b\":1e-29}")>]
[<InlineData("{\"a\":[{\"b\":0.1}],\"c\":[1e-29]}")>]
let ``JYRO-REQ-1171 FromJson_DecidesExactnessForEachNestedNumber`` (text: string) =
    let v = withText text "var v = FromJson(Input.t)\nOutput.r = v"
    let rec numbers (x: JyroValue) : JyroValue list =
        match x.ValueType with
        | JyroValueType.Array -> Values.asList x |> List.collect numbers
        | JyroValueType.Object -> (x :?> JyroObject) |> Seq.toList |> List.collect (fun kv -> numbers kv.Value)
        | JyroValueType.Number -> [ x ]
        | _ -> []
    match numbers v with
    | [ a; b ] ->
        Assert.True(Values.isExact a, "0.1 is not exact")
        Assert.Equal(0.1m, Values.asDecimal a)
        Assert.False(Values.isExact b, "1e-29 is exact")
        Assert.Equal(1e-29, Values.asFloat b)
    | other -> Values.fail (sprintf "expected two numbers, got %d" other.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1171")>]
let ``JYRO-REQ-1171 FromJson_ExactnessShowsInArithmeticAndStringForm`` () =
    let src =
        "Output.r = [FromJson('0.1') + FromJson('0.2') == 0.3, ToString(FromJson('1.50')), "
        + "ToString(FromJson('0.1234567890123456789012345678901')), ToString(FromJson('0.1234567890123456789012345678901') + 0), "
        + "ToString(FromJson('79228162514264337593543950336')), ToString(FromJson('1e30'))]"
    Assert.Equal(norm """[true,"1.5","0.12345678901234568","0.12345678901234568","7.922816251426434e+28","1e+30"]""", rJson src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1172")>]
[<InlineData("1e309")>]
[<InlineData("-1e309")>]
[<InlineData("[1, 1e400]")>]
[<InlineData("{\"a\":1e309}")>]
[<InlineData("{\"a\":[{\"b\":-1e309}]}")>]
let ``FromJson_WhenNumberOutsideFiniteDoubleRange_ReturnsNull`` (text: string) =
    fromJsonNull text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1172")>]
let ``FromJson_OfLargestFiniteDoubleAndUnderflow_ReturnsNumbers`` () =
    Assert.Equal(Double.MaxValue, Values.asFloat (fromJson "1.7976931348623157e308"))
    Assert.Equal("number", Values.asString (withText "1e-400" "Output.r = TypeOf(FromJson(Input.t))"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1173")>]
[<InlineData("{\"a\":1,\"a\":2}", "{\"a\":2}")>]
[<InlineData("{\"a\":1,\"a\":2,\"a\":3}", "{\"a\":3}")>]
[<InlineData("{\"a\":1,\"a\":null}", "{\"a\":null}")>]
[<InlineData("{\"a\":{\"x\":1},\"a\":5}", "{\"a\":5}")>]
[<InlineData("{\"o\":{\"k\":1,\"k\":[2]}}", "{\"o\":{\"k\":[2]}}")>]
let ``FromJson_DuplicateKey_TakesTheLastValue`` (text: string, expected: string) =
    Assert.Equal(expected, Values.toJson (fromJson text))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1174")>]
let ``FromJson_DuplicateKey_KeepsThePositionOfItsFirstOccurrence`` () =
    let v = withText """{"a":1,"b":2,"a":3}""" "var v = FromJson(Input.t)\nOutput.r = [Keys(v), ToJson(v)]"
    Assert.Equal(norm """[["a","b"],"{\"a\":3,\"b\":2}"]""", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1174")>]
let ``FromJson_NestedDuplicateKey_KeepsThePositionOfItsFirstOccurrence`` () =
    let v = withText """{"n":{"x":1,"y":2,"x":3,"z":4}}""" "Output.r = Keys(FromJson(Input.t).n)"
    Assert.Equal(norm """["x","y","z"]""", Values.toJson v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1175")>]
[<InlineData("{\"z\":1,\"a\":2,\"m\":3}", "[\"z\",\"a\",\"m\"]")>]
[<InlineData("{\"B\":1,\"a\":2,\"_\":3}", "[\"B\",\"a\",\"_\"]")>]
[<InlineData("{\"10\":1,\"2\":2,\"1\":3}", "[\"10\",\"2\",\"1\"]")>]
let ``FromJson_AddsPropertiesInTextOrder`` (text: string, expected: string) =
    Assert.Equal(expected, Values.toJson (withText text "Output.r = Keys(FromJson(Input.t))"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1175")>]
let ``FromJson_Of200ShuffledKeysAndNested_KeepsTextOrder`` () =
    let rnd = Random(7)
    let keys = [ 1 .. 200 ] |> List.map (fun i -> sprintf "k%d" i) |> List.sortBy (fun _ -> rnd.Next())
    let text = "{" + (keys |> List.map (fun k -> sprintf "\"%s\":1" k) |> String.concat ",") + ",\"n\":{\"y\":1,\"b\":2}}"
    let v = withText text "var v = FromJson(Input.t)\nOutput.r = [Keys(v), Keys(v.n)]"
    match Values.asList v with
    | [ k; kn ] ->
        Assert.Equal<string list>(keys @ [ "n" ], Values.asList k |> List.map Values.asString)
        Assert.Equal<string list>([ "y"; "b" ], Values.asList kn |> List.map Values.asString)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1176")>]
[<InlineData("1", "1")>]
[<InlineData("\"s\"", "\"s\"")>]
[<InlineData("true", "true")>]
[<InlineData("null", "null")>]
[<InlineData("[]", "[]")>]
[<InlineData("{}", "{}")>]
let ``FromJson_AcceptsAnyValueAtTopLevel`` (text: string, expected: string) =
    let res = Script.runInput (Values.obj [ "t", Values.str text ]) "Output.r = FromJson(Input.t)"
    Assert.True(Script.ok res, Script.describe res)
    Assert.Equal(expected, Values.toJson (Script.output res |> Values.prop "r"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1864")>]
[<InlineData(" 1 ", "1")>]
[<InlineData("\n\t[1]\r\n", "[1]")>]
[<InlineData("  {\"a\":1}  ", "{\"a\":1}")>]
[<InlineData(" \t\n1\r\n ", "1")>]
[<InlineData("\r\n\r\n1", "1")>]
[<InlineData("1\t\t", "1")>]
let ``JYRO-REQ-1864 FromJson_AcceptsWhiteSpaceBeforeAndAfter`` (text: string, expected: string) =
    Assert.Equal(expected, Values.toJson (fromJson text))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1864")>]
[<InlineData("1 2")>]
[<InlineData("[1]]")>]
[<InlineData("{}x")>]
[<InlineData("{} x")>]
[<InlineData("null null")>]
[<InlineData("[1] x")>]
[<InlineData("[1]\u0000")>]
[<InlineData("1\u0000")>]
[<InlineData("1\u00A0")>]
[<InlineData("1\u000B")>]
[<InlineData("1\u000C")>]
[<InlineData("1\u2028")>]
[<InlineData("1\uFEFF")>]
[<InlineData("1 /")>]
[<InlineData("1 / 2")>]
[<InlineData("1 # c")>]
[<InlineData("1,")>]
let ``JYRO-REQ-1864 FromJson_WhenOtherTextFollowsTheValue_ReturnsNull`` (text: string) =
    fromJsonNull text

// A comment is not other text (90.11.2, JYRO-REQ-1869): it after the value reads as nothing.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1864")>]
[<InlineData("1 // c", "1")>]
[<InlineData("1 /* c */", "1")>]
[<InlineData("1 // c\n", "1")>]
[<InlineData("[1] // c\r\n", "[1]")>]
[<InlineData(" 1 /* a */ /* b */ ", "1")>]
[<InlineData("{\"a\":1} /* c */", "{\"a\":1}")>]
let ``JYRO-REQ-1864 FromJson_WhenOnlyAcceptedCommentsFollowTheValue_ReturnsTheValue`` (text: string, expected: string) =
    Assert.Equal(expected, Values.toJson (fromJson text))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1864")>]
[<InlineData("1 // c\n2")>]
[<InlineData("1 /* c */ 2")>]
[<InlineData("1 /* c")>]
[<InlineData("1 // c\n x")>]
let ``JYRO-REQ-1864 FromJson_WhenOtherTextFollowsAComment_ReturnsNull`` (text: string) =
    fromJsonNull text

// A // comment holding U+2028 or U+2029 is not one FromJson accepts (JYRO-REQ-1869, JYRO-ADR-107),
// so what follows the value is other text and the result is null, whatever ends the comment.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1864")>]
[<InlineData("1 //c ")>]
[<InlineData("1 //c ")>]
[<InlineData("1 //c \n")>]
[<InlineData("1 //c \r")>]
[<InlineData("1 //c \r")>]
[<InlineData("1 //c \n")>]
[<InlineData("[1] //c \n")>]
let ``JYRO-REQ-1864 FromJson_WhenALineCommentHoldingALineSeparatorFollowsTheValue_ReturnsNull`` (text: string) =
    fromJsonNull text

// A /* */ comment holding U+2028 or U+2029 reads as nothing, after the value too.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1864")>]
[<InlineData("1 /*c */", "1")>]
[<InlineData("1 /*c */", "1")>]
[<InlineData("[1] /*c */", "[1]")>]
let ``JYRO-REQ-1864 FromJson_WhenABlockCommentHoldingALineSeparatorFollowsTheValue_ReturnsTheValue`` (text: string, expected: string) =
    Assert.Equal(expected, Values.toJson (fromJson text))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1178")>]
[<InlineData("")>]
[<InlineData(" ")>]
[<InlineData("\t\r\n ")>]
[<InlineData("\u00A0\u00A0")>]
let ``FromJson_OfEmptyOrWhiteSpaceText_ReturnsNull`` (text: string) =
    let res = Script.runInput (Values.obj [ "t", Values.str text ]) "Output.r = FromJson(Input.t)"
    Assert.True(Script.ok res, Script.describe res)
    expectNull (Script.output res |> Values.prop "r")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1179")>]
[<InlineData("\"\\uD800\"")>]
[<InlineData("\"\\uDC00\"")>]
[<InlineData("\"\\uD800\\u0041\"")>]
[<InlineData("\"\\uD800\\uD800\"")>]
[<InlineData("\"\\uDC00\\uD800\"")>]
[<InlineData("\"ab\\uD83D\"")>]
[<InlineData("{\"\\uD800\":1}")>]
[<InlineData("[1, [\"\\uDC00\"]]")>]
[<InlineData("{\"a\":{\"k\\uDE00\":1}}")>]
let ``FromJson_WhenEscapedSurrogateIsNotPaired_ReturnsNull`` (text: string) =
    fromJsonNull text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1179")>]
let ``FromJson_OfPairedSurrogateEscapes_DecodesThePair`` () =
    Assert.Equal(emoji, Values.asString (fromJson "\"\\uD83D\\uDE00\""))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1179")>]
let ``FromJson_OfScriptBuiltLoneSurrogateEscape_ReturnsNull`` () =
    // The JSON text holds a backslash, u, D800, made with an escaped backslash in the script.
    Assert.Equal(norm "[true,true]", rJson "Output.r = [FromJson('\"\\\\uD800\"') == null, FromJson('\"\\\\uD83D\\\\uDE00\"') == '\\uD83D\\uDE00']")

/// Texts built by the script from `hi` and `lo`, the two halves of a pair that `Substring`
/// cuts (91.10), so that the text holds a surrogate code unit itself and not a `\u` escape.
/// Each holds one unpaired code unit (JYRO-REQ-1726).
let rawUnpairedTexts : obj[] seq =
    [ "'\"a' & hi & '\"'"                  // in a string value: the spec's first example
      "'{\"' & hi & '\": 1}'"              // in a key: the spec's second example
      "hi"                                  // alone: the spec's third example
      "lo"
      "'\"a' & lo & '\"'"
      "'{\"' & lo & '\": 1}'"
      "'\"' & hi & 'x\"'"                  // a high surrogate followed by a character that is not a low one
      "'\"x' & lo & '\"'"                  // a low surrogate preceded by a character that is not a high one
      "'\"' & lo & hi & '\"'"              // a low surrogate then a high one: neither is paired
      "'\"' & hi & hi & '\"'"              // two high surrogates
      "'[\"' & hi & '\"]'"
      "'[1, {\"k\": [\"v' & lo & '\"]}]'"
      "'{\"a\": {\"k' & hi & '\": 1}}'"
      "'[1,' & hi & ']'"                   // outside a string
      "hi & '1'"
      "'1' & lo" ]
    |> Seq.map (fun e -> [| box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1726")>]
[<MemberData(nameof rawUnpairedTexts)>]
let ``JYRO-REQ-1726 FromJson_OfTextWithARawUnpairedSurrogate_ReturnsNull`` (textExpr: string) =
    let v = rVal (Values.obj []) (loneDecl + sprintf "Output.r = FromJson(%s)" textExpr)
    Assert.True(isNullValue v, sprintf "FromJson(%s) should be null, got %s" textExpr (if isNull (box v) then "<.NET null>" else v.ToJson()))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1726")>]
[<MemberData(nameof rawUnpairedTexts)>]
let ``JYRO-REQ-1726 FromJson_OfTextWithARawUnpairedSurrogate_IsNotReplacedByAFallbackOrAnotherCharacter`` (textExpr: string) =
    // `?? "fallback"` shows null (the text cannot be interpreted); a U+FFFD substitution would
    // give a value, and the text would have been read as JSON.
    let v = rVal (Values.obj []) (loneDecl + sprintf "Output.r = FromJson(%s) ?? \"fallback\"" textExpr)
    Assert.Equal("fallback", Values.asString v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1726")>]
let ``JYRO-REQ-1726 FromJson_OfHostTextWithARawUnpairedSurrogate_ReturnsNull`` () =
    let high = string (char 0xD800)
    let low = string (char 0xDC00)
    for text in [ "\"a" + high + "\""; "{\"" + high + "\": 1}"; high; low; "[\"" + low + "x\"]"; "\"" + high + "x\""; "[1," + low + "]" ] do
        fromJsonNull text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1726")>]
let ``JYRO-REQ-1726 FromJson_GivesTheSameForTheRawAndTheEscapedUnpairedSurrogate`` () =
    // The escaped form: quotation mark, a, backslash, u, D, 8, 0, 0, quotation mark (JYRO-REQ-1179).
    let escaped = rVal (Values.obj []) "Output.r = FromJson('\"a\\\\uD800\"')"
    let raw = rVal (Values.obj []) (loneDecl + "Output.r = FromJson('\"a' & hi & '\"')")
    Assert.True(isNullValue escaped, "the escaped form should give null")
    Assert.True(isNullValue raw, "the raw form should give null")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1726")>]
let ``JYRO-REQ-1726 FromJson_OfTextWithAPairedSurrogate_StillReadsIt`` () =
    // A pair, however it comes to be in the text, is a character, not an unpaired code unit.
    Assert.Equal(emoji, Values.asString (fromJson ("\"" + emoji + "\"")))
    Assert.Equal(emoji, Values.asString (rVal (Values.obj []) (loneDecl + "Output.r = FromJson('\"' & hi & lo & '\"')")))
    Assert.Equal(norm (sprintf "{\"%s\": 1}" emoji), Values.toJson (rVal (Values.obj []) (loneDecl + "Output.r = FromJson('{\"' & hi & lo & '\": 1}')")))
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1726")>]
let ``JYRO-REQ-1726 FromJson_OfTextWithAPairBesideAnUnpairedSurrogate_ReturnsNull`` () =
    // A pair beside an unpaired code unit: the unpaired one decides.
    fromJsonNull ("\"" + emoji + string (char 0xD800) + "\"")
    fromJsonNull ("\"" + string (char 0xDC00) + emoji + "\"")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 FromJson_AtTheNestingBound_Succeeds`` () =
    let d = nestingBound.Force()
    let arrays = String.replicate d "[" + String.replicate d "]"
    Assert.Equal("array", Values.asString (withText arrays "Output.r = TypeOf(FromJson(Input.t))"))
    let objects = String.replicate d "{\"k\":" + "1" + String.replicate d "}"
    Assert.Equal("object", Values.asString (withText objects "Output.r = TypeOf(FromJson(Input.t))"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 FromJson_OneAboveTheNestingBound_RaisesValueNestingLimitExceeded`` () =
    let d = nestingBound.Force() + 1
    let arrays = String.replicate d "[" + String.replicate d "]"
    failsVal (Values.obj [ "t", Values.str arrays ]) MessageCode.ValueNestingLimitExceeded "Output.r = FromJson(Input.t)"
    let objects = String.replicate d "{\"k\":" + "1" + String.replicate d "}"
    failsVal (Values.obj [ "t", Values.str objects ]) MessageCode.ValueNestingLimitExceeded "Output.r = FromJson(Input.t)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 FromJson_Of100000OpenBrackets_RaisesValueNestingLimitExceeded`` () =
    failsVal (Values.obj [ "t", Values.str (String('[', 100000)) ]) MessageCode.ValueNestingLimitExceeded "Output.r = FromJson(Input.t)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 FromJson_OfWideShallowValue_Succeeds`` () =
    let text = "[" + String.Join(",", Array.create 10000 "[1]") + "]"
    Assert.Equal(10000m, Values.asDecimal (withText text "Output.r = Length(FromJson(Input.t))"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1181")>]
let ``FromJson_ResultIsWritableAtEveryLevel`` () =
    let res = Script.runJson """{"t":"{\"b\":[1,{\"c\":0}]}"}""" "var r = FromJson(Input.t)\nr.a = 1\nr.b[0] = 2\nr.b[1].c = 3\nOutput.r = r"
    Assert.Equal(norm """{"b":[2,{"c":3}],"a":1}""", Values.toJson (Script.output res |> Values.prop "r"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1181")>]
let ``FromJson_EachCallReturnsANewValue`` () =
    Assert.Equal(norm """[{"b":[1]},{"b":[9],"x":1}]""", rJson "var t = '{\"b\":[1]}'\nvar p = FromJson(t)\nvar q = FromJson(t)\nq.b[0] = 9\nq.x = 1\nOutput.r = [p, q]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1182")>]
[<InlineData("FromJson(null)")>]
[<InlineData("FromJson(x)")>]
[<InlineData("FromJson(o.missing)")>]
[<InlineData("FromJson(0)")>]
[<InlineData("FromJson(1)")>]
[<InlineData("FromJson(1.5)")>]
[<InlineData("FromJson(true)")>]
[<InlineData("FromJson(false)")>]
[<InlineData("FromJson(text: null)")>]
[<InlineData("FromJson(-1)")>]
[<InlineData("FromJson(SquareRoot(2))")>]
[<InlineData("FromJson(1 == 1)")>]
[<InlineData("FromJson(Input.z)")>]
[<InlineData("FromJson(Input.n)")>]
[<InlineData("FromJson(Input.i)")>]
[<InlineData("FromJson(Input.b)")>]
let ``JYRO-REQ-1182 FromJson_OfNullNumberOrBoolean_RaisesArgumentTypeMismatch`` (call: string) =
    let input = Values.obj [ "z", Values.nul; "n", Values.exact 5m; "i", Values.inexact 0.5; "b", Values.bool true ]
    expectRuntimeError MessageCode.ArgumentTypeMismatch
        (Script.runInput input (sprintf "var x = null\nvar o = {}\nOutput.r = %s" call))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1183")>]
let ``FromJson_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "FromJson" FunctionEffect.Pure "Output.r = FromJson('1')"

// =========================================================================
// 95.14 ToJson
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1184")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("1")>]
[<InlineData("'s'")>]
[<InlineData("[1]")>]
[<InlineData("{a: 1}")>]
let ``ToJson_ForEachNonFunctionType_ReturnsAString`` (expr: string) =
    Assert.Equal("string", rStr (sprintf "Output.r = TypeOf(ToJson(%s))" expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1184")>]
let ``ToJson_WithNamedArgument_ReturnsString`` () =
    Assert.Equal("1", rStr "Output.r = ToJson(value: 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1184")>]
let ``ToJson_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = ToJson()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1184")>]
let ``ToJson_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = ToJson(1, 2)"

/// True when `json` has white space outside its string literals.
let private whiteOutsideStrings (json: string) =
    let mutable inString = false
    let mutable escaped = false
    let mutable found = false
    for c in json do
        if inString then
            if escaped then escaped <- false
            elif c = '\\' then escaped <- true
            elif c = '"' then inString <- false
        elif c = '"' then inString <- true
        elif c = ' ' || c = '\t' || c = '\r' || c = '\n' then found <- true
    found

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1185")>]
let ``ToJson_WritesNoWhiteSpaceOutsideStrings`` () =
    Assert.Equal("{\"a\":[1,2,{\"b\":null}],\"c\":\"x y\"}", rStr "Output.r = ToJson({a: [1, 2, {b: null}], c: 'x y'})")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1185")>]
[<InlineData("[[], {}, [[]]]")>]
[<InlineData("{a: {b: {c: [1, 'p q', {d: true}]}}, e: ' '}")>]
let ``ToJson_OfNestedValues_HasNoWhiteSpaceOutsideStrings`` (expr: string) =
    let s = rStr (sprintf "Output.r = ToJson(%s)" expr)
    Assert.False(whiteOutsideStrings s, s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1185")>]
let ``ToJson_OfDepth20_HasNoWhiteSpaceOutsideStrings`` () =
    let s = rStr (nestedObjectSrc 20 + "Output.r = ToJson([v, v])")
    Assert.False(whiteOutsideStrings s, s)

/// QuoteJSONString of one code unit, built from the table of 95.14.3.
let private quoted (c: char) =
    match c with
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\b' -> "\\b"
    | '\012' -> "\\f"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | c when int c < 0x20 -> sprintf "\\u%04x" (int c)
    | c -> string c

let escapeUnits : obj[] seq =
    seq {
        for i in 0 .. 0x1F do yield [| box (string (char i)) |]
        for s in [ "\""; "\\"; "/"; "\u007F"; "\u0080"; "\u2028"; "\u2029"; "\uFEFF"; "<"; "&"; "'"; "\u00E9"; emoji ] do
            yield [| box s |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1186")>]
[<MemberData(nameof escapeUnits)>]
let ``JYRO-REQ-1186 ToJson_WritesStringsAsJsonStringifyDoes`` (s: string) =
    let expected = "\"" + (s |> Seq.map quoted |> String.concat "") + "\""
    let got = Values.asString (rValB (Values.obj [ "t", Values.str s ]) "Output.r = ToJson(Input.t)")
    Assert.True((expected = got), sprintf "for [%s]: expected [%s] got [%s]" (units s) expected got)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1186")>]
[<MemberData(nameof escapeUnits)>]
let ``JYRO-REQ-1186 ToJson_WritesKeysAsJsonStringifyDoes`` (s: string) =
    let expected = "{\"" + (s |> Seq.map quoted |> String.concat "") + "\":1}"
    let got = Values.asString (rValB (Values.obj [ "t", Values.str s ]) "var o = {}\no[Input.t] = 1\nOutput.r = ToJson(o)")
    Assert.True((expected = got), sprintf "for [%s]: expected [%s] got [%s]" (units s) expected got)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1186")>]
let ``JYRO-REQ-1186 ToJson_EscapesAreLowerCaseHex`` () =
    Assert.Equal("\"\\u001f\\u000b\\u001b\"", Values.asString (rValB (Values.obj [ "t", Values.str "\u001F\u000B\u001B" ]) "Output.r = ToJson(Input.t)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1186")>]
let ``JYRO-REQ-1186 ToJson_WritesEveryCodeUnitBelowU0080AsTheTableGives`` () =
    // One string of the 128 code units U+0000 to U+007F, in order, as value and as key.
    let s = String(Array.init 128 char)
    let body = s |> Seq.map quoted |> String.concat ""
    Assert.StartsWith("\\u0000\\u0001", body)
    Assert.Contains("\\u0007\\b\\t\\n\\u000b\\f\\r\\u000e", body)
    Assert.Contains("!\\\"#", body)
    Assert.Contains("[\\\\]", body)
    Assert.EndsWith("~\u007F", body)
    let got = Values.asList (rValB (Values.obj [ "t", Values.str s ]) "var o = {}\no[Input.t] = 1\nOutput.r = [ToJson(Input.t), ToJson(o)]")
    match got |> List.map Values.asString with
    | [ v; k ] ->
        Assert.True(("\"" + body + "\"" = v), sprintf "value: got [%s]" v)
        Assert.True(("{\"" + body + "\":1}" = k), sprintf "key: got [%s]" k)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1186")>]
let ``JYRO-REQ-1186 ToJson_WritesStringsAndKeysAtEveryDepthAsTheTableGives`` () =
    let s = "a\"b\\c\u0001\u001F/\u007F\u2028é<&'+" + emoji + "￿"
    let q = "\"" + (s |> Seq.map quoted |> String.concat "") + "\""
    let expected = sprintf "{%s:[%s,{%s:%s}],\"k\":%s}" q q q q q
    let src = "var o = {}\nvar inner = {}\ninner[Input.t] = Input.t\no[Input.t] = [Input.t, inner]\no.k = Input.t\nOutput.r = ToJson(o)"
    let got = Values.asString (rValB (Values.obj [ "t", Values.str s ]) src)
    Assert.True((expected = got), sprintf "expected [%s] got [%s]" expected got)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1186")>]
let ``JYRO-REQ-1186 ToJson_OfScriptStringLiterals_WritesThemAsTheTableGives`` () =
    let got = rStr "Output.r = ToJson({'q\"\\\\': 'tab\\there\\u0000\\u001b\\u00e9\\u2029'})"
    Assert.Equal("{\"q\\\"\\\\\":\"tab\\there\\u0000\\u001bé\u2029\"}", got)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1187")>]
[<InlineData("hi")>]
[<InlineData("lo")>]
[<InlineData("'a' & hi")>]
[<InlineData("[hi]")>]
[<InlineData("{k: lo}")>]
[<InlineData("lo & hi")>]
let ``ToJson_OfStringWithUnpairedSurrogate_RaisesUnpairedSurrogate`` (expr: string) =
    fails MessageCode.UnpairedSurrogate (loneDecl + sprintf "Output.r = ToJson(%s)" expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1187")>]
let ``ToJson_OfHostStringWithUnpairedSurrogate_RaisesUnpairedSurrogate`` () =
    failsVal (Values.obj [ "s", Values.str ("x" + hi) ]) MessageCode.UnpairedSurrogate "Output.r = ToJson(Input.s)"
    failsVal (Values.obj [ "s", Values.str lo ]) MessageCode.UnpairedSurrogate "Output.r = ToJson(Input)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1188")>]
[<InlineData("o")>]
[<InlineData("{a: o}")>]
[<InlineData("[o]")>]
[<InlineData("p")>]
let ``ToJson_OfKeyWithUnpairedSurrogate_RaisesUnpairedSurrogate`` (expr: string) =
    fails MessageCode.UnpairedSurrogate (loneDecl + sprintf "var o = {}\no[hi] = 1\nvar p = {}\np['k' & lo] = 1\nOutput.r = ToJson(%s)" expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1189")>]
[<InlineData("0", "0")>]
[<InlineData("0 * -1", "0")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("0.1 + 0.2", "0.3")>]
[<InlineData("100", "100")>]
[<InlineData("0.000001", "0.000001")>]
[<InlineData("1234567890.123456789012345678", "1234567890.123456789012345678")>]
[<InlineData("-2.5", "-2.5")>]
[<InlineData("1e21 * 1.0", "1000000000000000000000")>]
[<InlineData("SquareRoot(2)", "1.4142135623730951")>]
[<InlineData("1e300 * 10", "1e+301")>]
[<InlineData("5e-324", "5e-324")>]
[<InlineData("1e-30", "1e-30")>]
[<InlineData("1e-28", "0.0000000000000000000000000001")>]
[<InlineData("-0.0500", "-0.05")>]
[<InlineData("2.50 * 2", "5")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("SquareRoot(2) - SquareRoot(2)", "0")>]
[<InlineData("-(SquareRoot(2) - SquareRoot(2))", "0")>]
[<InlineData("1e29", "1e+29")>]
let ``JYRO-REQ-1189 ToJson_WritesEachNumberInItsStringForm`` (expr: string, expected: string) =
    let src = sprintf "var n = %s\nOutput.r = [ToJson(n), ToJson([n]), '' & n, ToJson({k: n})]" expr
    match Values.asList (r src) |> List.map Values.asString with
    | [ top; nested; form; inObj ] ->
        Assert.Equal(expected, top)
        Assert.Equal(form, top)
        Assert.Equal("[" + expected + "]", nested)
        Assert.Equal("{\"k\":" + expected + "}", inObj)
    | other -> Values.fail (sprintf "unexpected %A" other)

let hostInexact : obj[] seq =
    [ 1e21, "1e+21"; 1e-7, "1e-7"; 1.7976931348623157e308, "1.7976931348623157e+308"; 0.1, "0.1"; 0.30000000000000004, "0.30000000000000004"
      -0.0, "0"; 0.0, "0"; 1e20, "100000000000000000000"; 5e-324, "5e-324"; 1.5e-7, "1.5e-7"; 123.0, "123"; -2.5, "-2.5"
      0.000001, "0.000001"; 1.234e-6, "0.000001234"; 9007199254740993.0, "9007199254740992" ]
    |> Seq.map (fun (d, s) -> [| box d; box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1189")>]
[<MemberData(nameof hostInexact)>]
let ``JYRO-REQ-1189 ToJson_WritesHostInexactNumbersInEcmaScriptForm`` (d: float, expected: string) =
    let n = Values.inexact d
    Assert.False(Values.isExact n)
    let v = rVal (Values.obj [ "n", n ]) "Output.r = [ToJson(Input.n), ToJson([Input.n]), ToJson({k: Input.n})]"
    match Values.asList v |> List.map Values.asString with
    | [ top; nested; inObj ] ->
        Assert.Equal(expected, top)
        Assert.Equal("[" + expected + "]", nested)
        Assert.Equal("{\"k\":" + expected + "}", inObj)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1189")>]
let ``JYRO-REQ-1189 ToJson_WritesHostExactNumbersWithoutTheirScale`` () =
    let cases = [ 1.500m, "1.5"; 100.00m, "100"; 0.000m, "0"; Decimal.Negate 0.0m, "0"; -2.50m, "-2.5"; 1.0000000000000000000000000000m, "1" ]
    for (d, expected) in cases do
        let n = Values.exact d
        Assert.True(Values.isExact n)
        let got = Values.asString (rVal (Values.obj [ "n", n ]) "Output.r = ToJson([Input.n])")
        Assert.True(("[" + expected + "]" = got), sprintf "ToJson([%M]): expected [%s] got [%s]" d expected got)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1189")>]
let ``JYRO-REQ-1189 ToJson_OfFromJsonNumbers_WritesTheirStringForm`` () =
    let src = "Output.r = ToJson(FromJson('[1.50, 1E2, -0, 0.10e1, 1e-29, 1e30, 79228162514264337593543950336]'))"
    Assert.Equal("[1.5,100,0,1,1e-29,1e+30,7.922816251426434e+28]", rStr src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1190")>]
[<InlineData("f")>]
[<InlineData("(a, b) => a")>]
[<InlineData("[1, f]")>]
[<InlineData("{a: {b: [f]}}")>]
[<InlineData("{a: 1, z: f}")>]
let ``JYRO-REQ-1190 ToJson_OfValueContainingFunction_RaisesInvalidType`` (expr: string) =
    fails MessageCode.InvalidType (sprintf "var f = x => x\nOutput.r = ToJson(%s)" expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1191")>]
[<InlineData("null", "null")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("1", "1")>]
[<InlineData("'a'", "\"a\"")>]
[<InlineData("[]", "[]")>]
[<InlineData("{}", "{}")>]
let ``ToJson_AcceptsEveryNonFunctionTopLevelValue`` (expr: string, expected: string) =
    Assert.Equal(expected, rStr (sprintf "Output.r = ToJson(%s)" expr))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1192")>]
[<InlineData("[]", "[]")>]
[<InlineData("{}", "{}")>]
[<InlineData("[[], {}]", "[[],{}]")>]
[<InlineData("{a: [], b: {}}", "{\"a\":[],\"b\":{}}")>]
let ``ToJson_WritesEmptyArrayAndObject`` (expr: string, expected: string) =
    Assert.Equal(expected, rStr (sprintf "Output.r = ToJson(%s)" expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1192")>]
let ``ToJson_OfContainersEmptiedByDeletion_WritesEmptyForms`` () =
    Assert.Equal(norm """["{}","[]"]""", rJson "var o = {a: 1}\ndelete o.a\nvar a = RemoveAt([1], 0)\nOutput.r = [ToJson(o), ToJson(a)]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1193")>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<InlineData("ci")>]
let ``ToJson_OfCyclicValue_RaisesValueNestingLimitExceeded`` (expr: string) =
    fails MessageCode.ValueNestingLimitExceeded (cyclesDecl + sprintf "Output.r = ToJson(%s)" expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1193")>]
let ``ToJson_AtTheNestingBound_Succeeds`` () =
    let d = nestingBound.Force()
    Assert.Equal("string", rStr (nestedArraySrc d + "Output.r = TypeOf(ToJson(v))"))
    Assert.Equal("string", rStr (nestedObjectSrc d + "Output.r = TypeOf(ToJson(v))"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1193")>]
let ``ToJson_OneAboveTheNestingBound_RaisesValueNestingLimitExceeded`` () =
    let d = nestingBound.Force() + 1
    fails MessageCode.ValueNestingLimitExceeded (nestedArraySrc d + "Output.r = ToJson(v)")
    fails MessageCode.ValueNestingLimitExceeded (nestedObjectSrc d + "Output.r = ToJson(v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1193")>]
let ``ToJson_OfSharedButAcyclicValue_Succeeds`` () =
    Assert.Equal("""{"a":[1],"b":[1]}""", rStr "var s = [1]\nOutput.r = ToJson({a: s, b: s})")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1194")>]
let ``ToJson_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "ToJson" FunctionEffect.Pure "Output.r = ToJson(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1194")>]
let ``ToJson_DoesNotChangeItsArgument`` () =
    Assert.Equal(norm "[true,true]", rJson "var x = {a: [1, {b: 2}], c: 'x'}\nvar c = Clone(x)\nvar s1 = ToJson(x)\nvar s2 = ToJson(x)\nOutput.r = [s1 == s2, x == c]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1194")>]
let ``ToJson_OfReadOnlyInput_RaisesNoWriteError`` () =
    Assert.Equal("""{"a":[1]}""", Values.asString (rIn """{"a":[1]}""" "Output.r = ToJson(Input)"))

// -- Chunk seams (d-508): a surrogate pair at every offset around 4,096 and 8,192 --

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1170")>]
let ``FromJson_OfAStringWithAnAstralCharacterAtEverySeamOffset_GivesThatString`` () =
    let lengths = [ 4086 .. 4100 ] @ [ 8182 .. 8196 ]
    let strings = [ for n in lengths -> String('a', n) + "\U0001F600" + "b" ]
    let texts = strings |> List.map (fun s -> "\"" + s + "\"")
    let input = Values.obj [ "ts", Values.arr (texts |> List.map Values.str); "xs", Values.arr (strings |> List.map Values.str) ]
    let parsed = rValB input "Output.r = Map(Input.ts, t => FromJson(t))" |> Values.asList
    let bad = List.zip3 lengths strings parsed |> List.filter (fun (_, s, p) -> not (p :? JyroString) || Values.asString p <> s) |> List.map (fun (n, _, _) -> n)
    Assert.True(bad.IsEmpty, sprintf "FromJson did not give the string for these lengths of \"a\" before the emoji: %A" bad)
    let round = rValB input "Output.r = Map(Input.xs, x => FromJson(ToJson(x)) == x)" |> Values.asList
    let bad2 = List.zip lengths round |> List.filter (fun (_, g) -> not (Values.asBool g)) |> List.map fst
    Assert.True(bad2.IsEmpty, sprintf "FromJson(ToJson(x)) differs from x for these lengths: %A" bad2)

/// 20.7 - string literals: delimiters, escapes, surrogate escapes, termination.
module RealTree.Jyro.Spec.Tests.S20_07_Strings

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private stringOf (source: string) : string = Values.asString (outProp "s" source)

/// `Output.s = <q><body><q>` for the quote style `q` ("dq" or "sq").
let private literal (style: string) (body: string) =
    let q = if style = "dq" then "\"" else "'"
    "Output.s = " + q + body + q

// -- 20.7.1 ------------------------------------------------------------------

[<Theory>]
[<InlineData("Output.s = \"abc\"", "abc")>]
[<InlineData("Output.s = 'abc'", "abc")>]
[<InlineData("Output.s = \"\"", "")>]
[<InlineData("Output.s = ''", "")>]
[<InlineData("Output.s = \"it's\"", "it's")>]
[<InlineData("Output.s = 'say \"hi\"'", "say \"hi\"")>]
[<Trait("requirement", "JYRO-REQ-35")>]
let ``StringLiteral_DelimitedByTwoDoubleOrTwoSingleQuotes`` (source: string, expected: string) =
    Assert.Equal(expected, stringOf source)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-35")>]
let ``StringLiteral_BothDelimitersGiveTheSameString`` () =
    outV "Output.v = \"abc\" == 'abc'" |> assertJson "true"

[<Theory>]
[<InlineData("Output.s = \"abc'")>]
[<InlineData("Output.s = 'abc\"")>]
[<InlineData("Output.s = `abc`")>]
[<Trait("requirement", "JYRO-REQ-35")>]
let ``StringLiteral_MismatchedOrOtherDelimiters_AreRejected`` (source: string) =
    rejected source |> ignore

// -- 20.7.2 ------------------------------------------------------------------

[<Theory>]
[<InlineData("dq", "\\n", "\n")>]
[<InlineData("sq", "\\n", "\n")>]
[<InlineData("dq", "\\r", "\r")>]
[<InlineData("sq", "\\r", "\r")>]
[<InlineData("dq", "\\t", "\t")>]
[<InlineData("sq", "\\t", "\t")>]
[<InlineData("dq", "\\\\", "\\")>]
[<InlineData("sq", "\\\\", "\\")>]
[<InlineData("dq", "\\\"", "\"")>]
[<InlineData("sq", "\\\"", "\"")>]
[<InlineData("dq", "\\'", "'")>]
[<InlineData("sq", "\\'", "'")>]
[<InlineData("dq", "\\u0041", "A")>]
[<InlineData("sq", "\\u0041", "A")>]
[<InlineData("dq", "\\u00e9", "\u00e9")>]
[<InlineData("dq", "\\u00E9", "\u00e9")>]
[<InlineData("sq", "\\u00E9", "\u00e9")>]
[<InlineData("dq", "\\u00411", "A1")>]
[<InlineData("sq", "\\u0041F", "AF")>]
[<InlineData("dq", "a\\tb\\nc", "a\tb\nc")>]
[<InlineData("sq", "\\\\n", "\\n")>]
[<Trait("requirement", "JYRO-REQ-1825")>]
let ``JYRO-REQ-1825 Escape_EachPermittedEscape_GivesItsCharacter`` (style: string, body: string, expected: string) =
    let s = stringOf (literal style body)
    Assert.Equal<char[]>(expected.ToCharArray(), s.ToCharArray())

[<Theory>]
[<InlineData("dq")>]
[<InlineData("sq")>]
[<Trait("requirement", "JYRO-REQ-1825")>]
let ``JYRO-REQ-1825 Escape_NulEscape_GivesAOneCodeUnitStringHoldingU0000`` (style: string) =
    let s = stringOf (literal style "\\0")
    Assert.Equal(1, s.Length)
    Assert.Equal(0, int s.[0])

/// The table of 20.7.2 row by row: each escape is exactly one code unit, of this value,
/// in both quote styles, as read by Length and by indexing inside the script.
[<Theory>]
[<InlineData("dq", "\\n", 0x000A)>]
[<InlineData("sq", "\\n", 0x000A)>]
[<InlineData("dq", "\\r", 0x000D)>]
[<InlineData("sq", "\\r", 0x000D)>]
[<InlineData("dq", "\\t", 0x0009)>]
[<InlineData("sq", "\\t", 0x0009)>]
[<InlineData("dq", "\\\\", 0x005C)>]
[<InlineData("sq", "\\\\", 0x005C)>]
[<InlineData("dq", "\\\"", 0x0022)>]
[<InlineData("sq", "\\\"", 0x0022)>]
[<InlineData("dq", "\\'", 0x0027)>]
[<InlineData("sq", "\\'", 0x0027)>]
[<InlineData("dq", "\\0", 0x0000)>]
[<InlineData("sq", "\\0", 0x0000)>]
[<InlineData("dq", "\\u0041", 0x0041)>]
[<InlineData("sq", "\\u0041", 0x0041)>]
[<InlineData("dq", "\\u00e9", 0x00E9)>]
[<InlineData("dq", "\\u00E9", 0x00E9)>]
[<InlineData("dq", "\\uabcd", 0xABCD)>]
[<InlineData("dq", "\\uABCD", 0xABCD)>]
[<InlineData("dq", "\\uAbCd", 0xABCD)>]
[<InlineData("sq", "\\u20ac", 0x20AC)>]
[<InlineData("dq", "\\uD83D", 0xD83D)>]
[<Trait("requirement", "JYRO-REQ-1825")>]
let ``JYRO-REQ-1825 Escape_EachRowOfTheTable_GivesExactlyOneCodeUnitOfThatValue`` (style: string, body: string, unit: int) =
    // A lone surrogate is refused (20.7.4); every other row is read as a string with one code unit.
    if unit >= 0xD800 && unit <= 0xDFFF then
        rejectedWithCode MessageCode.UnpairedSurrogate (literal style body)
    else
        let q = if style = "dq" then "\"" else "'"
        let out = Script.outputOf (sprintf "var s = %s%s%s\nOutput.n = Length(s)\nOutput.s = s" q body q)
        Values.prop "n" out |> assertJson "1"
        let s = Values.asString (Values.prop "s" out)
        Assert.Equal(1, s.Length)
        Assert.Equal(unit, int s.[0])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1825")>]
let ``JYRO-REQ-1825 Escape_TheStatementExamples_Hold`` () =
    outV "Output.v = \"\\u0041\" == \"A\"" |> assertJson "true"
    outV "Output.v = 'a\\tb'" |> assertJson "\"a\\tb\""
    outV "Output.v = Length(\"a\\tb\")" |> assertJson "3"
    outV "Output.v = Length(\"\\u00412\")" |> assertJson "2"
    let s = stringOf "Output.s = \"a\\tb\""
    Assert.Equal(0x09, int s.[1])
    let two = stringOf "Output.s = \"\\u00412\""
    Assert.Equal(0x41, int two.[0])
    Assert.Equal(int '2', int two.[1])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1825")>]
let ``JYRO-REQ-1825 Escape_SurrogateEscapePair_GivesTwoCodeUnitsFormingOneCodePoint`` () =
    let s = stringOf "Output.s = \"\\uD83D\\uDE00\""
    Assert.Equal(2, s.Length)
    Assert.Equal(0x1F600, System.Char.ConvertToUtf32(s, 0))

[<Theory>]
[<InlineData("dq", "\\u0000", 0x0000)>]
[<InlineData("sq", "\\u0000", 0x0000)>]
[<InlineData("dq", "\\uFFFF", 0xFFFF)>]
[<InlineData("sq", "\\uffff", 0xFFFF)>]
[<Trait("requirement", "JYRO-REQ-1825")>]
let ``JYRO-REQ-1825 Escape_UEscapeAtTheEdges_GivesThatCodeUnit`` (style: string, body: string, unit: int) =
    let s = stringOf (literal style body)
    Assert.Equal(1, s.Length)
    Assert.Equal(unit, int s.[0])

// -- 20.7.3 ------------------------------------------------------------------

let invalidEscapes : seq<obj[]> =
    seq {
        for style in [ "dq"; "sq" ] do
            for esc in [ "\\a"; "\\b"; "\\f"; "\\v"; "\\e"; "\\q"; "\\x41"; "\\U00000041"; "\\u{41}"; "\\/"; "\\ "
                         "\\N"; "\\T"; "\\R"; "\\1"; "\\9"; "\\uGGGG"; "\\u00G1"; "\\u004"  + "g"; "\\#" ] do
                yield [| box style; box esc |]
    }

[<Theory>]
[<MemberData(nameof invalidEscapes)>]
[<Trait("requirement", "JYRO-REQ-1822")>]
let ``JYRO-REQ-1822 Escape_AnyOtherEscape_IsInvalidEscapeSequenceAtTheStageParseAndNothingRuns`` (style: string, body: string) =
    let r = rejected (literal style ("x" + body + "y"))
    assertAllErrors MessageCode.InvalidEscapeSequence r
    assertParseStage r

[<Theory>]
[<InlineData("dq")>]
[<InlineData("sq")>]
[<Trait("requirement", "JYRO-REQ-1822")>]
let ``JYRO-REQ-1822 Escape_UEscapeWithFewerThanFourDigitsBeforeTheQuote_IsInvalidEscapeSequence`` (style: string) =
    let r = rejected (literal style "\\u12")
    assertHasError MessageCode.InvalidEscapeSequence r
    assertParseStage r
    let r41 = rejected (literal style "\\u41")
    assertHasError MessageCode.InvalidEscapeSequence r41
    assertParseStage r41

[<Theory>]
[<InlineData("dq", "\n")>]
[<InlineData("sq", "\n")>]
[<InlineData("dq", "\r\n")>]
[<Trait("requirement", "JYRO-REQ-1822")>]
let ``JYRO-REQ-1822 Escape_BackslashBeforeALineEnd_IsInvalidEscapeSequence`` (style: string, lineEnd: string) =
    let r = rejected (literal style ("a\\" + lineEnd + "b"))
    assertHasError MessageCode.InvalidEscapeSequence r
    assertParseStage r

[<Theory>]
[<InlineData("dq")>]
[<InlineData("sq")>]
[<Trait("requirement", "JYRO-REQ-1822")>]
let ``JYRO-REQ-1822 Escape_EveryValidEscapeIsAccepted_WhereTheNextOneIsRefused`` (style: string) =
    for body in [ "\\n"; "\\r"; "\\t"; "\\\\"; "\\\""; "\\'"; "\\0"; "\\u0041"; "\\uabcd" ] do
        compiles (literal style body)
    for body in [ "\\q"; "\\x41"; "\\u41" ] do
        let r = rejected (literal style body)
        assertHasError MessageCode.InvalidEscapeSequence r
        assertParseStage r

// -- 20.7.4 ------------------------------------------------------------------

[<Theory>]
[<InlineData("dq", "\\uD83D\\uDE00", 0x1F600)>]
[<InlineData("sq", "\\uD83D\\uDE00", 0x1F600)>]
[<InlineData("dq", "\\uD800\\uDC00", 0x10000)>]
[<InlineData("dq", "\\uDBFF\\uDFFF", 0x10FFFF)>]
[<InlineData("dq", "\\ud83d\\ude00", 0x1F600)>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 SurrogateEscape_HighThenLow_GivesThePair`` (style: string, body: string, codePoint: int) =
    let s = stringOf (literal style body)
    Assert.Equal(cp codePoint, s)
    Assert.Equal(2, s.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 SurrogateEscape_PairInsideOtherText_GivesThePair`` () =
    Assert.Equal("a" + cp 0x1F600 + "b", stringOf "Output.s = \"a\\uD83D\\uDE00b\"")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 SurrogateEscape_Pair_HasLengthTwoInTheScript`` () =
    outV "Output.v = Length(\"\\uD83D\\uDE00\")" |> assertJson "2"

[<Theory>]
[<InlineData("\\uD7FF", 0xD7FF)>]
[<InlineData("\\uE000", 0xE000)>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 SurrogateEscape_CodeUnitsJustOutsideTheRange_AreOrdinary`` (body: string, unit: int) =
    Assert.Equal(cu unit, stringOf (literal "dq" body))

let unpairedEscapes : seq<obj[]> =
    seq {
        for style in [ "dq"; "sq" ] do
            for body in [ "\\uD83D"; "\\uDE00"; "\\uDE00\\uD83D"; "\\uD800\\uD800"; "\\uD83D\\u0041"
                          "\\uD83Dx"; "\\uD83D\\n\\uDE00"; "abc\\uD83D"; "\\uD800"; "\\uDBFF"; "\\uDC00"; "\\uDFFF"
                          "\\uD83D\\uDE00\\uDE00" ] do
                yield [| box style; box body |]
    }

[<Theory>]
[<MemberData(nameof unpairedEscapes)>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 SurrogateEscape_NotPartOfAPair_IsUnpairedSurrogate`` (style: string, body: string) =
    rejectedWithCode MessageCode.UnpairedSurrogate (literal style body)

/// UnpairedSurrogate is a rejection placed at parse time, so 160.1.12 gives it the stage
/// Parse, in a script and in a library alike: as a source text, as a compiler's library
/// source and through JyroLibrary.Parse (a library diagnostic has the stage the same
/// diagnostic has in a script). Moved here from JYRO-REQ-1670's stage table
/// (JYRO-ADR-113 item 2, JYRO-DEF-505): this requirement raises the code, so it depends on 1670.
[<Theory>]
[<InlineData("\\uD800")>]
[<InlineData("\\uDFFF")>]
[<InlineData("\\uDE00\\uD83D")>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 SurrogateEscape_NotPartOfAPair_IsUnpairedSurrogateAtTheStageParse`` (body: string) =
    let text = sprintf "func L()\n  return \"%s\"\nend" body
    let results =
        [ "source", (Script.compile (sprintf "Output.s = \"%s\"" body)).Messages
          "AddLibrarySource", (Script.compileWith { Script.defaults with Libraries = [ text ] } "Output.x = 1").Messages
          "JyroLibrary.Parse", (JyroLibrary.Parse text).Messages ]
    for name, messages in results do
        let unpaired = messages |> Seq.filter (fun m -> m.Code = MessageCode.UnpairedSurrogate) |> List.ofSeq
        Assert.True(not unpaired.IsEmpty, sprintf "%s: no UnpairedSurrogate for %A. Codes: %A" name body [ for m in messages -> m.Code ])
        for m in unpaired do
            Assert.True((Pending.stageOf m = "Parse"), sprintf "%s: UnpairedSurrogate has the stage %s, not Parse" name (Pending.stageOf m))

/// UnpairedSurrogate is a lexical error in a string token, so 130.1.2 (JYRO-REQ-2122)
/// places it at that whole token, quotes included. Carried here from JYRO-REQ-1475's
/// place rows (JYRO-ADR-122 item 4): the code is this requirement's. (source, line,
/// column, length), the source holding the escape as the backslash-u text.
let unpairedEscapePlaces : seq<obj[]> =
    [ "Output.x = \"\\uD800\"", 1, 12, 8
      "Output.x = 'a\\uDC00b'", 1, 12, 10
      "Output.a = 1\nOutput.x = \"\\uD83D\\u0041\"", 2, 12, 14
      "Output.a = 1\r\n  Output.x = \"\\uDE00\\uD83D\"", 2, 14, 14 ]
    |> Seq.map (fun (s, l, c, n) -> [| box s; box l; box c; box n |])

[<Theory>]
[<MemberData(nameof unpairedEscapePlaces)>]
[<Trait("requirement", "JYRO-REQ-38")>]
let ``JYRO-REQ-38 an unpaired surrogate escape is placed at its whole string token`` (source: string, line: int, column: int, length: int) =
    Assert.Equal((line, column, length), placeOf MessageCode.UnpairedSurrogate source)

// -- 20.7.5 ------------------------------------------------------------------

[<Theory>]
[<InlineData("Output.s = \"abc")>]
[<InlineData("Output.s = 'abc")>]
[<InlineData("Output.s = \"abc'")>]
[<InlineData("Output.s = 'abc\"")>]
[<InlineData("Output.s = \"abc\\\"")>]
[<InlineData("Output.s = 'abc\\'")>]
[<InlineData("Output.s = \"")>]
[<InlineData("Output.s = '")>]
[<InlineData("Output.a = 1\nOutput.s = \"x\nOutput.b = 2\nOutput.c = 3")>]
[<Trait("requirement", "JYRO-REQ-39")>]
let ``StringLiteral_WithNoClosingQuote_IsUnterminatedStringAndNothingRuns`` (source: string) =
    let r = rejected source
    assertOnly MessageCode.UnterminatedString r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-39")>]
let ``StringLiteral_Control_SpanningALineEnd_IsClosedOnTheNextLine`` () =
    Assert.Equal("a\nb", stringOf "Output.s = \"a\nb\"")

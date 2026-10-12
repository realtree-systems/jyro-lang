/// 20.2 - source encoding: host strings, line ends, positions, U+FEFF, surrogates.
module RealTree.Jyro.Spec.Tests.S20_02_SourceEncoding

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 20.2.1: source text is a host String of UTF-16 code units ---------------

/// U+1F600 (a surrogate pair) then U+65E5: three UTF-16 code units.
let private text = cp 0x1F600 + cp 0x65E5
let private script = "Output.s = \"" + text + "\""

let private assertText (r: JyroResult<JyroValue>) =
    let o = Script.output r
    let s = Values.asString (o.GetProperty "s")
    Assert.Equal(text, s)
    Assert.Equal(3, s.Length)
    Assert.Equal<char[]>(text.ToCharArray(), s.ToCharArray())

let private stringParam (t: Type) (name: string) (types: Type[]) =
    let m = t.GetMethod(name, types)
    Assert.True(not (isNull m), sprintf "%s.%s(%s) is not declared" t.Name name (types |> Array.map (fun x -> x.Name) |> String.concat ", "))

/// The public type `RealTree.Jyro.<name>` of the host API (by reflection, so
/// the test reads the .NET surface whatever language declares it).
let private apiType (name: string) : Type =
    let t = typeof<JyroCompiler>.Assembly.GetType("RealTree.Jyro." + name)
    if isNull t then Values.fail (sprintf "RealTree.Jyro.%s is not in the host API" name)
    t

let private invokeStatic (typeName: string) (name: string) (types: Type[]) (args: obj[]) : JyroResult<JyroValue> =
    let m = (apiType typeName).GetMethod(name, types)
    if isNull m then Values.fail (sprintf "%s.%s is not declared" typeName name)
    m.Invoke(null, args) :?> JyroResult<JyroValue>

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_EveryHostMemberTakingSource_TakesASystemString`` () =
    stringParam typeof<JyroCompiler> "WithSource" [| typeof<string> |]
    stringParam typeof<JyroCompiler> "AddLibrarySource" [| typeof<string> |]
    stringParam typeof<JyroLibrary> "Parse" [| typeof<string> |]
    stringParam (apiType "JyroScript") "ExecuteScript" [| typeof<string>; typeof<obj> |]
    stringParam (apiType "JyroScript") "ExecuteScriptWithJson" [| typeof<string>; typeof<string> |]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_WithSource_KeepsUtf16CodeUnitsOfAStringLiteral`` () =
    assertText (Script.run script)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_WithSource_LengthCountsUtf16CodeUnits`` () =
    outProp "n" (script + "\nOutput.n = Length(Output.s)") |> assertJson "3"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_AddLibrarySource_KeepsUtf16CodeUnits`` () =
    Script.runWith
        { Script.defaults with Libraries = [ "func S() return \"" + text + "\" end" ] }
        "Output.s = S()"
    |> assertText

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_JyroLibraryParse_KeepsUtf16CodeUnits`` () =
    let lib = JyroLibrary.Parse("func S() return \"" + text + "\" end")
    Assert.True(lib.IsSuccess, Script.describe lib)
    let library = Script.valueOf lib
    Script.runWith
        { Script.defaults with ConfigureCompiler = fun c -> c.AddLibrary(library) }
        "Output.s = S()"
    |> assertText

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_ExecuteScript_KeepsUtf16CodeUnits`` () =
    assertText (invokeStatic "JyroScript" "ExecuteScript" [| typeof<string>; typeof<obj> |] [| box script; box (JyroObject()) |])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-17")>]
let ``SourceText_ExecuteScriptWithJson_KeepsUtf16CodeUnits`` () =
    assertText (invokeStatic "JyroScript" "ExecuteScriptWithJson" [| typeof<string>; typeof<string> |] [| box script; box "{}" |])

// -- 20.2.2: LF and CRLF are both line ends ----------------------------------

let private program =
    [ "# header comment"
      "var total = 0"
      "func add(a, b)"
      "  return a + b"
      "end"
      "foreach n in [1, 2, 3] do"
      "  total = add(total, n)"
      "end"
      "if total > 5 then"
      "  Output.big = true"
      "else"
      "  Output.big = false"
      "end"
      "Output.total = total # trailing comment" ]

let private withEnds (ends: int -> string) (lines: string list) =
    lines |> List.mapi (fun i l -> if i = lines.Length - 1 then l else l + ends i) |> String.concat ""

let private lf = withEnds (fun _ -> "\n")
let private crlf = withEnds (fun _ -> "\r\n")
let private mixed = withEnds (fun i -> if i % 2 = 0 then "\r\n" else "\n")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_Crlf_GivesTheSameOutputAsLf``() =
    let expected = Script.outputOf (lf program)
    let actual = Script.outputOf (crlf program)
    assertJson "{\"big\":true,\"total\":6}" expected
    assertJson (Values.toJson expected) actual

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_MixedLfAndCrlf_GivesTheSameOutputAsLf``() =
    Script.outputOf (mixed program) |> assertJson "{\"big\":true,\"total\":6}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_TrailingCrlf_IsAccepted``() =
    Script.outputOf (crlf program + "\r\n") |> assertJson "{\"big\":true,\"total\":6}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_CrlfEndsAComment_AndTheNextStatementRuns``() =
    Script.outputOf "# comment\r\nOutput.a = 1 # another\r\nOutput.b = 2"
    |> assertJson "{\"a\":1,\"b\":2}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_CrlfAfterExit_EndsTheStatementWithNoValue`` () =
    Script.outputOf "Output.a = 1\r\nexit\r\nOutput.a = 2" |> assertJson "{\"a\":1}"
    Script.outputOf "Output.a = 1\nexit\nOutput.a = 2" |> assertJson "{\"a\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_CrlfAfterFail_EndsTheStatementWithNoValue``() =
    // Compared with the same script with LF line ends: CRLF must end the
    // statement exactly as LF does (the value rules themselves are 50.11.4's).
    let withLf = Script.run "Output.a = 1\nfail\nOutput.a = 2"
    let withCrlf = Script.run "Output.a = 1\r\nfail\r\nOutput.a = 2"
    assertOnly MessageCode.ScriptFailure withCrlf
    let a = Script.onlyErrorMessage withLf
    let b = Script.onlyErrorMessage withCrlf
    let argsOf (m: DiagnosticMessage) = if isNull m.Args then [] else m.Args |> Array.map string |> List.ofArray
    Assert.Equal<string list>(argsOf a, argsOf b)
    Assert.Equal(a.Message, b.Message)
    Script.valueOf withCrlf |> Values.prop "a" |> assertJson "1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_CrlfAfterReturn_EndsTheStatementWithNoValue`` () =
    outV "func f()\r\n  return\r\n  5\r\nend\r\nOutput.v = f()" |> assertJson "null"
    outV "func f()\n  return\n  5\nend\nOutput.v = f()" |> assertJson "null"

/// The characters that 20.2.2 says are not line ends: a U+000D not directly
/// followed by U+000A, U+0085, U+2028 and U+2029.
let notLineEnds () : obj[] seq =
    [ "\r", "lone CR"; "\u0085", "U+0085"; " ", "U+2028"; " ", "U+2029" ]
    |> Seq.map (fun (c, label) -> [| box c; box label |])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_LfAndCrlfAround_Exit_LeaveTheFirstAssignment`` () =
    for e in [ "\n"; "\r\n" ] do
        Script.outputOf ("Output.a = 1" + e + "exit" + e + "Output.a = 2") |> assertJson "{\"a\":1}"

[<Theory>]
[<MemberData(nameof notLineEnds)>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_ACharacterThatIsNotALineEnd_InsideAComment_DoesNotEndIt`` (c: string, label: string) =
    // The text after `#` is one comment, which leaves Output.a equal to 1.
    Script.outputOf ("Output.a = 1 # note" + c + "Output.a = 2") |> assertJson "{\"a\":1}"
    Script.outputOf ("Output.a = 1 # note" + c + "Output.a = 2\nOutput.b = 3") |> assertJson "{\"a\":1,\"b\":3}"
    ignore label

[<Theory>]
[<MemberData(nameof notLineEnds)>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_ACharacterThatIsNotALineEnd_IsWhitespaceAfterReturn`` (c: string, label: string) =
    // With a line end, `return` ends and returns null; as whitespace, the 5 follows it.
    outV ("func f()\n  return" + c + "5\nend\nOutput.v = f()") |> assertJson "5"
    ignore label


// -- 20.2.3: LF and CRLF each count as one line end in positions --------------

let private locationOf (r: JyroResult<'T>) : SourceLocation =
    let m = Script.onlyErrorMessage r
    if not m.Location.HasValue then Values.fail (sprintf "The diagnostic has no location:\n%s" (Script.describe r))
    m.Location.Value

[<Theory>]
[<MemberData(nameof notLineEnds)>]
[<Trait("requirement", "JYRO-REQ-1814")>]
let ``JYRO-REQ-1814 LineEnds_ACharacterThatIsNotALineEnd_IsNotCountedInThePositionOfADiagnostic`` (c: string, label: string) =
    // The invalid escape is on line 1 when the characters before it are not line ends,
    Assert.Equal(1, (locationOf (Script.compile ("Output.a = 1" + c + "Output.c = \"a\\qb\""))).Line)
    // and on line 2 when the line end is an LF or a CRLF.
    Assert.Equal(2, (locationOf (Script.compile "Output.a = 1\nOutput.c = \"a\\qb\"")).Line)
    Assert.Equal(2, (locationOf (Script.compile "Output.a = 1\r\nOutput.c = \"a\\qb\"")).Line)
    ignore label

let private escapeErrorLines = [ "Output.a = 1"; "Output.b = 2"; "Output.c = \"a\\qb\""; "Output.d = 4" ]
let private stringErrorLines = [ "Output.a = 1"; "Output.b = 2"; "Output.s = \"abc" ]

[<Theory>]
[<InlineData("escape")>]
[<InlineData("string")>]
[<Trait("requirement", "JYRO-REQ-19")>]
let ``Position_ErrorOnLine3_HasTheSameLineAndColumnWithLfCrlfAndMixed`` (kind: string) =
    let lines = if kind = "escape" then escapeErrorLines else stringErrorLines
    let a = locationOf (Script.compile (lf lines))
    let b = locationOf (Script.compile (crlf lines))
    let c = locationOf (Script.compile (mixed lines))
    Assert.Equal(a.Line, b.Line)
    Assert.Equal(a.Column, b.Column)
    Assert.Equal(a.Line, c.Line)
    Assert.Equal(a.Column, c.Column)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-19")>]
let ``Position_WithCrlf_ErrorOnLine5IsFourLinesAfterErrorOnLine1`` () =
    let line1 = locationOf (Script.compile (crlf [ "Output.c = \"a\\qb\"" ]))
    let line5 = locationOf (Script.compile (crlf [ "Output.a = 1"; "Output.b = 2"; "Output.x = 3"; "Output.y = 4"; "Output.c = \"a\\qb\"" ]))
    Assert.Equal(4, line5.Line - line1.Line)
    Assert.Equal(line1.Column, line5.Column)

// -- 20.2.4: U+FEFF ----------------------------------------------------------

let private bom = cu 0xFEFF

[<Theory>]
[<InlineData("Output.a = 1")>]
[<InlineData("# a comment\nOutput.a = 1")>]
[<InlineData("  \nOutput.a = 1")>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_AtTheFirstPosition_IsIgnored`` (rest: string) =
    Script.outputOf (bom + rest) |> assertJson "{\"a\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_InsideAStringLiteral_IsKeptAsACharacter`` () =
    let s = Values.asString (outProp "s" ("Output.s = \"a" + bom + "b\""))
    Assert.Equal("a" + bom + "b", s)
    Assert.Equal(3, s.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_InsideASingleQuotedStringLiteral_IsKeptAsACharacter`` () =
    let s = Values.asString (outProp "s" ("Output.s = 'a" + bom + "b'"))
    Assert.Equal("a" + bom + "b", s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_InsideAComment_CausesNoError`` () =
    Script.outputOf ("Output.a = 1 # x" + bom + "y\nOutput.b = 2") |> assertJson "{\"a\":1,\"b\":2}"

let bomRejected : seq<obj[]> =
    [ "second_after_first", bom + bom + "Output.a = 1"
      "between_statements", "Output.a = 1\n" + bom + "Output.b = 2"
      "between_identifier_and_equals", "Output.a" + bom + " = 1"
      "last_character", "Output.a = 1" + bom
      "after_leading_white_space", " " + bom + "Output.a = 1"
      "after_leading_line_end", "\n" + bom + "Output.a = 1" ]
    |> Seq.map (fun (l, s) -> [| box l; box s |])

[<Theory>]
[<MemberData(nameof bomRejected)>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_AtAnyOtherPositionOutsideStringsAndComments_IsParseError`` (label: string, source: string) =
    ignore label
    parseErrorRaw source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_BetweenStatements_NothingRuns`` () =
    rejected ("Output.a = 1\n" + bom + "Output.b = 2") |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_AtFirstPositionOfALibrarySource_IsIgnored`` () =
    let r = Script.runWith { Script.defaults with Libraries = [ bom + "func L() return 1 end" ] } "Output.v = L()"
    Script.output r |> Values.prop "v" |> assertJson "1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_AtFirstPositionOfJyroLibraryParse_IsIgnored`` () =
    let r = JyroLibrary.Parse(bom + "func L() return 1 end")
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_InsideALibrarySourceAfterTheFirstPosition_IsParseError`` () =
    let r =
        Script.compileWith
            { Script.defaults with Libraries = [ "func L() return 1 end\n" + bom + "func M() return 2 end" ] }
            "Output.v = 1"
    assertFailed r
    assertParseStage r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 Bom_InsideJyroLibraryParseAfterTheFirstPosition_IsRejected`` () =
    let r = JyroLibrary.Parse("func L() return 1 end\n" + bom + "func M() return 2 end")
    assertFailed r
    assertParseStage r

/// A leading U+FEFF is ignored by the grammar but stays in the source text, so it
/// is counted in every position (150.4.4): here a union variant's (150.8.5,
/// JYRO-REQ-2121). Placed in 20.2.4's tests, not 1691's, by JYRO-ADR-122, since
/// only 20.2.4 makes a source with a leading mark compile (JYRO-DEF-575).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 a leading byte order mark is counted in a union variant's position`` () =
    // bom + "union U A B(x) end": the mark is index 0 and column 1.
    let _, a = Artifact.writtenBy limited (bom + "union U A B(x) end")
    match a.Program |> List.tryPick (function Artifact.SUnion(_, vs, _) -> Some vs | _ -> None) with
    | Some [ "A", [], pa; "B", [ "x", None ], pb ] ->
        Assert.Equal({ Artifact.Line = 1; Artifact.Column = 10; Artifact.Start = 9; Artifact.End = 10 }, pa)
        Assert.Equal({ Artifact.Line = 1; Artifact.Column = 12; Artifact.Start = 11; Artifact.End = 15 }, pb)
    | other -> Values.fail (sprintf "expected the variants A and B(x), got %A" other)

/// A leading U+FEFF is counted in the column of a diagnostic (130.1.2, JYRO-REQ-2122:
/// "a leading U+FEFF counted"), on its line only. Carried here from JYRO-REQ-1475's
/// place rows (JYRO-ADR-122 item 4), since only 20.2.4 makes such a source compile.
/// (code, source after the mark, line, column, length).
let bomPlaces : seq<obj[]> =
    [ "TypedVariableNeedsInitializer", "var s: string", 1, 2, 13
      "TypedVariableNeedsInitializer", "Output.a = 1\nvar s: string", 2, 1, 13
      "UndefinedFunction", "Output.x = Nope()", 1, 13, 6
      "InvalidNumberFormat", "Output.x = 12abc", 1, 13, 5 ]
    |> Seq.map (fun (c, s, l, col, n) -> [| box c; box s; box l; box col; box n |])

[<Theory>]
[<MemberData(nameof bomPlaces)>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 a leading byte order mark is counted in a diagnostic's column`` (code: string, rest: string, line: int, column: int, length: int) =
    Assert.Equal((line, column, length), placeOf (Enum.Parse<MessageCode> code) (bom + rest))

/// After a leading U+FEFF the first token has column 2 and start index 1 (150.4.4,
/// JYRO-REQ-1532, whose U+FEFF case is tested here under JYRO-ADR-122 item 4): the
/// source `var x = 1⏎Output.y = x` of 150.4.4's worked example, one code unit on.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 after a leading byte order mark the first token has column 2 and start index 1`` () =
    let _, a = Artifact.writtenBy limited (bom + "var x = 1\nOutput.y = x")
    match a.Program with
    | [ Artifact.SVar("x", _, Some(Artifact.ELit(_, pLit)), pVar);
        Artifact.SAssign(Artifact.EProp(Artifact.EId("Output", pOut), "y", pProp), _, Artifact.EId("x", pX), _, pAssign) ] ->
        let at (line, column, start, stop) : Artifact.Pos =
            { Artifact.Line = line; Artifact.Column = column; Artifact.Start = start; Artifact.End = stop }
        Assert.Equal(at (1, 2, 1, 10), pVar)
        Assert.Equal(at (1, 10, 9, 10), pLit)
        Assert.Equal(at (2, 1, 11, 23), pAssign)
        Assert.Equal(at (2, 1, 11, 19), pProp)
        Assert.Equal(at (2, 1, 11, 17), pOut)
        Assert.Equal(at (2, 12, 22, 23), pX)
    | other -> Values.fail (sprintf "unexpected program %A" other)

/// The source hash hashes a leading U+FEFF as EF BB BF (150.3.8, JYRO-REQ-1527, whose
/// U+FEFF case is tested here under JYRO-ADR-122 item 4): `Output.x = 1` after the mark
/// hashes 15 bytes that start EF BB BF, though 20.2.4 ignores the mark.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-20")>]
let ``JYRO-REQ-20 a leading byte order mark is hashed as EF BB BF`` () =
    let expected = Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] (Text.Encoding.ASCII.GetBytes "Output.x = 1")
    Assert.Equal(15, expected.Length)
    let bytes, _ = Artifact.writtenBy limited (bom + "Output.x = 1")
    Assert.Equal<byte[]>(Security.Cryptography.SHA256.HashData expected, bytes.[8..39])
    // The same source without the mark has another hash: the mark is not dropped.
    let plain, _ = Artifact.writtenBy limited "Output.x = 1"
    Assert.NotEqual<byte[]>(plain.[8..39], bytes.[8..39])

// -- 20.2.5: unpaired surrogate code units -----------------------------------

// A theory row passes only its label: the test runner serializes a row's
// strings, and a lone surrogate does not survive that (it arrives as U+FFFD), so
// each test builds its text from the label.
let private unpairedLiterals : Map<string, string> =
    Map.ofList
        [ "dq_lone_high", "\"a" + cu 0xD800 + "b\""
          "dq_lone_low", "\"a" + cu 0xDC00 + "b\""
          "dq_low_then_high", "\"a" + cu 0xDC00 + cu 0xD800 + "b\""
          "dq_high_at_end", "\"ab" + cu 0xD800 + "\""
          "dq_high_then_high", "\"" + cu 0xD800 + cu 0xD800 + "\""
          "sq_lone_high", "'a" + cu 0xDBFF + "b'"
          "sq_lone_low", "'a" + cu 0xDFFF + "b'"
          "sq_high_at_end", "'ab" + cu 0xD83D + "'" ]

let unpairedInLiteral : seq<obj[]> = unpairedLiterals |> Map.toSeq |> Seq.map (fun (l, _) -> [| box l |])

/// The literal of `label`, checked to hold a lone surrogate code unit.
let private unpairedLiteral (label: string) : string =
    let s = unpairedLiterals.[label]
    Assert.True(s |> Seq.exists Char.IsSurrogate, "the row lost its surrogate")
    s

[<Theory>]
[<MemberData(nameof unpairedInLiteral)>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_InAStringLiteral_IsRejectedWithUnpairedSurrogate`` (label: string) =
    rejectedWithCode MessageCode.UnpairedSurrogate ("Output.s = " + unpairedLiteral label)

[<Theory>]
[<MemberData(nameof unpairedInLiteral)>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_InAStringLiteral_IsRejectedAtParseTime`` (label: string) =
    rejected ("Output.s = " + unpairedLiteral label) |> assertParseStage

[<Fact>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_InALibraryStringLiteral_IsRejectedWithUnpairedSurrogate`` () =
    let r =
        Script.compileWith
            { Script.defaults with Libraries = [ "func L() return \"" + cu 0xD800 + "\" end" ] }
            "Output.v = L()"
    assertFailed r
    assertOnly MessageCode.UnpairedSurrogate r

let private unpairedSources : Map<string, string> =
    Map.ofList
        [ "between_statements_high", "Output.a = 1\n" + cu 0xD800 + "\nOutput.b = 2"
          "between_statements_low", "Output.a = 1\n" + cu 0xDC00 + "\nOutput.b = 2"
          "inside_identifier", "var x" + cu 0xD800 + "y = 1"
          "at_end_of_source", "Output.a = 1" + cu 0xD800 ]

let unpairedOutside : seq<obj[]> = unpairedSources |> Map.toSeq |> Seq.map (fun (l, _) -> [| box l |])

[<Theory>]
[<MemberData(nameof unpairedOutside)>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_OutsideStringsAndComments_IsParseError`` (label: string) =
    let source = unpairedSources.[label]
    Assert.True(source |> Seq.exists Char.IsSurrogate, "the row lost its surrogate")
    parseError source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_InTheMiddleOfAComment_CausesNoError`` () =
    Script.outputOf ("Output.a = 1 # x" + cu 0xD800 + "y\nOutput.b = 2 # " + cu 0xDC00 + cu 0xD800)
    |> assertJson "{\"a\":1,\"b\":2}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_AsTheLastCharacterOfAComment_CausesNoError`` () =
    Script.outputOf ("Output.a = 1 # x" + cu 0xDC00) |> assertJson "{\"a\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_InALibraryComment_CausesNoError`` () =
    Script.runWith
        { Script.defaults with Libraries = [ "# " + cu 0xD800 + "\nfunc L() return 1 end" ] }
        "Output.v = L()"
    |> Script.output
    |> Values.prop "v"
    |> assertJson "1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-21")>]
let ``JYRO-REQ-21 UnpairedSurrogate_Control_AValidPairInAStringAndAComment_IsAccepted`` () =
    let s = Values.asString (outProp "s" ("Output.s = \"" + cp 0x1F600 + "\" # " + cp 0x1F600))
    Assert.Equal(cp 0x1F600, s)

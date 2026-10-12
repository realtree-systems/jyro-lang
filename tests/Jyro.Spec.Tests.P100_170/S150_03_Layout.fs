/// Part 150.3 to 150.8: the layout of an artifact, written (CompileToArtifact,
/// decoded by the book's codec) and read (hand-built by the codec, then loaded).
module RealTree.Jyro.Spec.Tests.S150_03_Layout

open System
open System.Security.Cryptography
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

/// Run a hand-built program and return Output.
let private runs (program: Stmt list) : JyroValue = load (build program) |> ok

let private outJson (program: Stmt list) : string = Values.toJson (runs program)

/// The value of the first top-level `Output.x = <value>` in a decoded program.
let private assignedValue (program: Stmt list) : Expr =
    program
    |> List.pick (function
        | SAssign(EProp(EId("Output", _), _, _), _, v, _, _) -> Some v
        | _ -> None)

let private valueOf (source: string) : Expr = programOf source |> assignedValue

// -- 150.3.1 Header, table, program; little-endian; UTF-8 ------------------------------------------

let corpusRows : obj[] seq = behaviorCorpus

[<Theory>]
[<MemberData(nameof corpusRows)>]
[<Trait("requirement", "JYRO-REQ-2067")>]
let ``WrittenArtifact_IsHeaderThenTableThenProgram_WithNothingLeftOver`` (source: string) =
    let bytes, a = writtenBy Script.defaults source
    Assert.True(bytes.Length >= 44)
    Assert.Equal(uint32 a.Functions.Length, BitConverter.ToUInt32(bytes, 40))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2067")>]
let ``MultiByteNumbers_AreLittleEndian`` () =
    // A statement on line 258 (0x0102): its position's line field reads 258 little-endian.
    let src = String.replicate 257 "\n" + "Output.x = 1"
    let _, a = writtenBy Script.defaults src
    match a.Program with
    | [ SAssign(_, _, _, _, p) ] -> Assert.Equal(258, p.Line)
    | other -> Values.fail (sprintf "unexpected program %A" other)
    // A string of 300 bytes (0x012C).
    let s = String('q', 300)
    match valueOf (sprintf "Output.x = \"%s\"" s) with
    | ELit(LStr v, _) -> Assert.Equal(s, v)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2067")>]
let ``MultiByteNumbers_ATableOf258Names_IsCountedLittleEndian`` () =
    let fns = [ for i in 0 .. 257 -> Host.recorder (sprintf "H%03d" i) ]
    let src = lines [ for i in 0 .. 257 -> sprintf "H%03d()" i ]
    let bytes, a = writtenBy { Script.defaults with Functions = fns |> List.map (fun f -> f.AsFunction) } src
    Assert.Equal(258, a.Functions.Length)
    Assert.Equal<byte[]>([| 0x02uy; 0x01uy; 0uy; 0uy |], bytes.[40..43])

[<Theory>]
[<InlineData("é")>]
[<InlineData("日本")>]
[<InlineData("😀")>]
[<InlineData("aé日😀z")>]
[<Trait("requirement", "JYRO-REQ-2067")>]
let ``Strings_AreUtf8`` (text: string) =
    match valueOf (sprintf "Output.x = \"%s\"" text) with
    | ELit(LStr v, _) -> Assert.Equal(text, v)
    | other -> Values.fail (sprintf "unexpected %A" other)
    Assert.Equal(text, Values.asString (Values.prop "x" (runs [ setOut "x" (str text) ])))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2067")>]
let ``Reader_ReadsVersionLittleEndian`` () =
    let a = artifactOf [ setOut "x" (num 1m) ]
    // 0x0100 is 1 read big-endian; little-endian it is 256, a version above 1.
    let bytes = encode { a with Version = 0x0100us }
    Assert.Equal<byte[]>([| 0uy; 1uy |], bytes.[4..5])
    refused MessageCode.ArtifactVersionUnsupported bytes
    // The same program at 1 written little-endian (01 00) is not refused for its version.
    Assert.DoesNotContain(MessageCode.ArtifactVersionUnsupported, Script.codes (loadOnly (encode a)))

// -- 150.3.2 The header ------------------------------------------------------------------------------

[<Theory>]
[<InlineData("Output.x = 1")>]
[<InlineData("Output.x = \"é日\"\r\nOutput.y = Length([1])")>]
[<InlineData("Output.x = Trim(\" a \")\nOutput.y = ToUpper(\"b\")")>]
[<Trait("requirement", "JYRO-REQ-2068")>]
let ``Header_IsMagicVersionReservedHashAndTableCount`` (source: string) =
    // Version 1 (150.10.4, JYRO-REQ-1920), reserved 00 00 (150.3.7), the hash of 150.3.8: for
    // these sources (no unpaired surrogate) the UTF-8 of the text as given; the count of the
    // table (150.3.3). A leading U+FEFF case belongs in 20.2.4's tests (JYRO-ADR-122 item 4).
    let bytes, a = writtenBy pinned source
    Assert.Equal<byte[]>("JYRX"B, bytes.[0..3])
    Assert.Equal<byte[]>([| 1uy; 0uy |], bytes.[4..5])
    Assert.Equal<byte[]>([| 0uy; 0uy |], bytes.[6..7])
    Assert.Equal<byte[]>(SHA256.HashData(Encoding.UTF8.GetBytes source), bytes.[8..39])
    Assert.Equal(uint32 a.Functions.Length, BitConverter.ToUInt32(bytes, 40))
    let info = JyroArtifactInfo.Read bytes |> Script.valueOf
    Assert.Equal(1, info.FormatVersion)
    Assert.Equal<byte[]>(SHA256.HashData(Encoding.UTF8.GetBytes source), info.SourceHash.ToArray())
    Assert.Equal(a.Functions.Length, info.RequiredFunctions.Count)

// -- 150.3.3 The function table ------------------------------------------------------------------------

let private exampleSource =
    lines [ "func helper()"
            "  return ToUpper(\"a\")"
            "end"
            "Output.a = Trim(\" x \")"
            "Output.b = Trim(\" y \")"
            "Output.c = Length([1])"
            "Output.d = helper()"
            "Output.e = Append([1], 2)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2069")>]
let ``Table_OfTheBooksExample_IsAppendLengthToUpperTrim`` () =
    let bytes, a = writtenBy Script.defaults exampleSource
    Assert.Equal<string list>([ "Append"; "Length"; "ToUpper"; "Trim" ], a.Functions)
    Assert.Equal(4u, BitConverter.ToUInt32(bytes, 40))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2069")>]
let ``Table_HoldsACallInALambdaBody`` () =
    let _, a = writtenBy Script.defaults "Output.a = Map([\" x \"], x => Trim(x))"
    Assert.Equal<string list>([ "Map"; "Trim" ], a.Functions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2069")>]
let ``Table_IsInOrdinalOrder_UppercaseBeforeLowercase`` () =
    let fns = [ Host.recorder "Zeta"; Host.recorder "alpha"; Host.recorder "Beta" ] |> List.map (fun f -> f.AsFunction)
    let _, a = writtenBy { Script.defaults with Functions = fns } "alpha()\nZeta()\nLength([1])\nBeta()\nalpha()"
    Assert.Equal<string list>([ "Beta"; "Length"; "Zeta"; "alpha" ], a.Functions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2069")>]
let ``Table_HoldsNoDeclaredFunctionAndNoVariant`` () =
    let src =
        lines ([ "union Shape Circle(r) Square(s) end"; "func F(x)"; "  return x"; "end"; "Output.c = Circle(1)" ]
               @ repeatLines 100 "Output.f{i} = F({i})")
    let _, a = writtenBy Script.defaults src
    Assert.Empty(a.Functions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2069")>]
let ``Table_HoldsACallInABranchThatNeverRuns`` () =
    let _, a = writtenBy Script.defaults "if false then Output.x = Trim(\"a\") end"
    Assert.Equal<string list>([ "Trim" ], a.Functions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2069")>]
let ``Table_OfAProgramWithNoCalls_IsEmpty`` () =
    let bytes, a = writtenBy Script.defaults "Output.x = 1 + 2"
    Assert.Empty(a.Functions)
    Assert.Equal(0u, BitConverter.ToUInt32(bytes, 40))

// -- 150.3.4 The program ----------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1523")>]
let ``Program_IsTheTopLevelStatementsInSourceOrder`` () =
    let prog = programOf "var a = 1\nfunc f()\n  return 1\nend\nOutput.x = a\nif true then Output.y = 2 end"
    match prog with
    | [ SVar("a", _, _, _); SFunc("f", _, _, _); SAssign _; SIf _ ] -> ()
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1523")>]
let ``Reader_RunsTheStatementsInListOrder`` () =
    let append n = assign (out "log") (call "Append" [ out "log"; num n ])
    let v = runs [ setOut "log" (arr []); append 1m; append 2m; append 3m ]
    Assert.Equal("[1,2,3]", Values.toJson (Values.prop "log" v))

// -- 150.3.5 / 150.3.6 Table contents on read ----------------------------------------------------------

let private exampleArtifact () = writtenBy Script.defaults exampleSource |> snd

let private withTable (names: string list) = encode { exampleArtifact () with Functions = names }

[<Theory>]
[<InlineData("Append,Append,Length,ToUpper,Trim")>]
[<InlineData("Append,Length,Append,ToUpper,Trim")>]
[<InlineData("Append,Length,ToUpper,Trim,Trim")>]
[<Trait("requirement", "JYRO-REQ-2070")>]
let ``TableWithANameTwice_IsRejectedAsUnreadable`` (names: string) =
    refused MessageCode.ArtifactUnreadable (withTable (List.ofArray (names.Split ',')))

/// The refusal of a table name two times, as 160.1.12 places it and 160.7.9 shapes a refusal
/// of JyroArtifactInfo.Read: on a load and on Read, ArtifactUnreadable with the stage
/// Deserialize; on Read, a failed result with no value and exactly that one Error, with no
/// source location. Moved here from JYRO-REQ-1670's stage table and JYRO-REQ-2106's checks
/// of Read (JYRO-ADR-113 item 2, JYRO-DEF-505, JYRO-DEF-506): only this requirement makes
/// the check, so this requirement depends on 1670 and 1620.
[<Theory>]
[<InlineData("Trim,Trim")>]
[<InlineData("A,A")>]
[<InlineData("Append,Length,Append,ToUpper,Trim")>]
[<Trait("requirement", "JYRO-REQ-2070")>]
let ``TableWithANameTwice_IsRefusedAtDeserialize_OnALoadAndByRead`` (names: string) =
    let bytes = withTable (List.ofArray (names.Split ','))
    let loaded = Script.loadBytesWith pinned (bytes)
    fails MessageCode.ArtifactUnreadable loaded
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" loaded
    let r = JyroArtifactInfo.Read bytes
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.False(r.HasValue)
    let found, _ = r.TryGetValue()
    Assert.False(found)
    Assert.Equal(1, r.Messages.Count)
    let m = r.Messages.[0]
    Assert.Equal(MessageSeverity.Error, m.Severity)
    Assert.Equal(MessageCode.ArtifactUnreadable, m.Code)
    Assert.Equal("Deserialize", Pending.stageOf m)
    Assert.False(m.Location.HasValue, "a refusal of Read has no source location")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2070")>]
let ``TableWithoutDuplicates_Loads`` () =
    load (withTable [ "Append"; "Length"; "ToUpper"; "Trim" ]) |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2071")>]
let ``TableInAnotherOrder_Loads_AndRunsTheSame`` () =
    let expected = Values.toJson (Script.run exampleSource |> ok)
    for names in [ [ "Trim"; "ToUpper"; "Length"; "Append" ]; [ "Length"; "Trim"; "Append"; "ToUpper" ] ] do
        Assert.Equal(expected, Values.toJson (load (withTable names) |> ok))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2071")>]
let ``TableWithAnExtraNameThatResolves_Loads`` () =
    load (withTable [ "Append"; "Length"; "ToLower"; "ToUpper"; "Trim" ]) |> ok |> ignore
    let h = Host.recorder "Extra"
    Script.runBytesWith { Script.defaults with Functions = [ h.AsFunction ] } (withTable [ "Append"; "Extra"; "Length"; "ToUpper"; "Trim" ])
    |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2071")>]
let ``TableWithAnExtraNameThatDoesNotResolve_IsRejectedWithUndefinedFunction`` () =
    refused MessageCode.UndefinedFunction (withTable [ "Append"; "Length"; "NoSuchFunction"; "ToUpper"; "Trim" ])

// -- 150.4.x Primitive encodings -------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1529")>]
let ``WrittenString_IsAByteCountThenUtf8`` () =
    for s in [ ""; "a"; String.replicate 32767 "é" + "a" ] do
        let src = sprintf "Output.x = \"%s\"" s
        let bytes, _ = writtenBy Script.defaults src
        let utf8 = Encoding.UTF8.GetBytes s
        let needle = Array.concat [ [| 0x04uy |]; BitConverter.GetBytes(uint16 utf8.Length); utf8 ]
        let found = Seq.windowed needle.Length bytes |> Seq.exists (fun w -> w = needle)
        Assert.True(found, sprintf "No literal string of %d bytes encoded as count then UTF-8" utf8.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1529")>]
let ``StringOf65536Bytes_CannotBeWritten`` () =
    let r = Script.compileBytesWith Script.defaults (sprintf "Output.x = \"%s\"" (String('a', 65536)))
    fails MessageCode.ArtifactCannotEncode r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1529")>]
let ``Reader_ReadsStringsOf0And65535Bytes`` () =
    Assert.Equal("{\"x\":\"\"}", outJson [ setOut "x" (str "") ])
    let big = String.replicate 32767 "é" + "a"
    let v = runs [ setOut "x" (str big) ]
    Assert.Equal(big, Values.asString (Values.prop "x" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1529")>]
let ``Reader_StringCountPastTheEndOfTheFile_IsRejected`` () =
    let good = build [ setOut "x" (str "abc") ]
    let bad = good.[0 .. good.Length - 1 - 32 - 3]
    Assert.False((loadOnly bad).IsSuccess)
    refused MessageCode.ArtifactUnreadable bad

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1530")>]
let ``WrittenList_IsACountThenItems`` () =
    for n in [ 0; 1; 10_000 ] do
        match valueOf ("Output.x = " + arrayLiteral n) with
        | EArr(items, _) -> Assert.Equal(n, items.Length)
        | other -> Values.fail (sprintf "unexpected %A" other)
    Script.compileBytesWith Script.defaults ("Output.x = " + arrayLiteral 10_001) |> fails MessageCode.ArtifactCannotEncode

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1530")>]
let ``Reader_ReadsListsOf0And10000Items`` () =
    Assert.Equal("{\"x\":[]}", outJson [ setOut "x" (arr []) ])
    let v = runs [ setOut "n" (call "Length" [ arr [ for i in 0 .. 9_999 -> num (decimal i) ] ]) ]
    Assert.Equal(10_000m, Values.asDecimal (Values.prop "n" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1531")>]
let ``WrittenOption_IsAbsentByteOrPresentByteThenValue`` () =
    match programOf "var a\nvar b: number = 1" with
    | [ SVar("a", None, None, _); SVar("b", Some 1uy, Some(ELit(LExact 1m, _)), _) ] -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    match programOf "func F()\n  if true then\n    return\n  end\n  return 1\nend" with
    | [ SFunc(_, _, [ SIf(_, [ SReturn(None, _) ], _, _, _); SReturn(Some _, _) ], _) ] -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Theory>]
[<InlineData(0x01uy)>]
[<InlineData(0x02uy)>]
[<InlineData(0x7Fuy)>]
[<InlineData(0xFFuy)>]
[<Trait("requirement", "JYRO-REQ-1531")>]
let ``Reader_AnyNonZeroPresenceByte_MeansPresent`` (present: byte) =
    let prog = [ SVar("b", Some Hint.number, Some(num 5m), p0); SVar("a", None, None, p0); setOut "a" (ident "a"); setOut "b" (ident "b") ]
    let v = Script.runBytesWith Script.defaults (encodeWith present (artifactOf prog)) |> ok
    Assert.Equal("{\"a\":null,\"b\":5}", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``WrittenPosition_IsLineColumnStartEnd`` () =
    let prog = (writtenBy Script.defaults "Output.a = 1\n\n  Output.b = 2" |> snd).Program
    match prog with
    | [ _; SAssign(_, _, _, _, p) ] ->
        Assert.Equal(3, p.Line)
        Assert.Equal(3, p.Column)
        Assert.True(p.End >= p.Start && p.Start >= 0, sprintf "%A" p)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``Reader_ReportsTheLineAndColumnOfThePosition`` () =
    // A runtime error at a node of a loaded program reports the line and column as
    // written, and the length end - start (150.4.7); the place is the operator's node (130.1.2).
    let p = { Line = 7; Column = 9; Start = 100; End = 105 }
    let prog = [ SAssign(EProp(EId("Output", p), "x", p), Op.assign, EBin(ELit(LExact 1m, p), Op.div, ELit(LExact 0m, p), p), None, p) ]
    let r = load (build prog)
    // Judged by its code alone: 150.4.7 is not about the stage (JYRO-ADR-105 item 4).
    fails MessageCode.DivisionByZero r
    let d = Script.onlyErrorMessage r
    Assert.True(d.Location.HasValue, "no Location")
    Assert.Equal(7, d.Location.Value.Line)
    Assert.Equal(9, d.Location.Value.Column)
    Assert.Equal(5, d.Location.Value.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``Reader_AcceptsNegativeAndLargePositions`` () =
    for p in [ { Line = -1; Column = -5; Start = -10; End = -2 }; { Line = Int32.MaxValue; Column = Int32.MaxValue; Start = Int32.MaxValue; End = Int32.MaxValue } ] do
        let prog = [ SAssign(EProp(EId("Output", p), "x", p), Op.assign, ELit(LExact 1m, p), None, p) ]
        Assert.Equal("{\"x\":1}", outJson prog)

// -- 150.5.x Enumeration tags -----------------------------------------------------------------------------

let hints : obj[] seq =
    [ "any", 0uy, "1"; "number", 1uy, "1"; "string", 2uy, "\"s\""; "boolean", 3uy, "true"; "object", 4uy, "{}"
      "array", 5uy, "[]"; "null", 6uy, "null"; "function", 7uy, "x => x" ]
    |> Seq.map (fun (n, b, v) -> [| box n; box b; box v |])

[<Theory>]
[<MemberData(nameof hints)>]
[<Trait("requirement", "JYRO-REQ-1536")>]
let ``WrittenTypeHint_IsItsByte`` (name: string, tag: byte, value: string) =
    match programOf (sprintf "var a: %s = %s" name value) with
    | [ SVar("a", Some t, _, _) ] -> Assert.Equal(tag, t)
    | other -> Values.fail (sprintf "unexpected %A" other)
    match valueOf (sprintf "Output.x = 1 is %s" name) with
    | ETypeCheck(_, t, 0uy, _) -> Assert.Equal(tag, t)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1536")>]
let ``Reader_TypeHintBytes_MeanTheirTypes`` () =
    // 1 is a number; "s" a string; each hint byte tested against a sample of its type and a number.
    let samples = [ 0uy, num 1m; 1uy, num 1m; 2uy, str "s"; 3uy, tru; 4uy, EObj([], p0); 5uy, arr []; 6uy, nul; 7uy, ELambda([ "v" ], ident "v", p0) ]
    let prog =
        [ for tag, sample in samples do
            yield setOut (sprintf "own%d" tag) (ETypeCheck(sample, tag, 0uy, p0))
            yield setOut (sprintf "num%d" tag) (ETypeCheck(num 1m, tag, 0uy, p0)) ]
    let v = runs prog
    for tag, _ in samples do
        Assert.True(Values.asBool (Values.prop (sprintf "own%d" tag) v), sprintf "hint %d on its own type" tag)
        Assert.Equal((tag = 0uy || tag = 1uy), Values.asBool (Values.prop (sprintf "num%d" tag) v))

let binaryOps : obj[] seq =
    [ "+", 0x00uy; "-", 0x01uy; "*", 0x02uy; "/", 0x03uy; "%", 0x04uy; "==", 0x05uy; "!=", 0x06uy; "<", 0x07uy
      "<=", 0x08uy; ">", 0x09uy; ">=", 0x0Auy; "and", 0x0Buy; "or", 0x0Cuy; "??", 0x0Duy; "&", 0x0Euy ]
    |> Seq.map (fun (o, b) -> [| box o; box b |])

[<Theory>]
[<MemberData(nameof binaryOps)>]
[<Trait("requirement", "JYRO-REQ-1537")>]
let ``WrittenBinaryOperator_IsItsByte`` (op: string, tag: byte) =
    match valueOf (sprintf "var p = 7\nvar q = 2\nOutput.x = p %s q" op) with
    | EBin(_, t, _, _) -> Assert.Equal(tag, t)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1537")>]
let ``Reader_BinaryOperatorBytes_MeanTheirOperators`` () =
    let seven, two = num 7m, num 2m
    let prog =
        [ for t in 0x00uy .. 0x0Auy do
            yield setOut (sprintf "o%d" t) (bin seven t two)
          yield setOut "and" (bin tru Op.andOp fls)
          yield setOut "or" (bin fls Op.orOp tru)
          yield setOut "coalesce" (bin nul Op.coalesce two)
          yield setOut "concat" (bin (str "a") Op.concat (str "b")) ]
    Assert.Equal(
        "{\"o0\":9,\"o1\":5,\"o2\":14,\"o3\":3.5,\"o4\":1,\"o5\":false,\"o6\":true,\"o7\":false,\"o8\":false,\"o9\":true,\"o10\":true,"
        + "\"and\":false,\"or\":true,\"coalesce\":2,\"concat\":\"ab\"}",
        outJson prog)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1538")>]
let ``UnaryOperators_AreNegate0AndNot1`` () =
    match valueOf "var p = 5\nOutput.x = -p" with
    | EUn(0uy, _, _) -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    match valueOf "var p = true\nOutput.x = not p" with
    | EUn(1uy, _, _) -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    Assert.Equal("{\"n\":-5,\"b\":false}", outJson [ setOut "n" (EUn(0uy, num 5m, p0)); setOut "b" (EUn(1uy, tru, p0)) ])

let assignOps : obj[] seq =
    [ "=", 0uy; "+=", 1uy; "-=", 2uy; "*=", 3uy; "/=", 4uy; "%=", 5uy; "&=", 6uy ]
    |> Seq.map (fun (o, b) -> [| box o; box b |])

[<Theory>]
[<MemberData(nameof assignOps)>]
[<Trait("requirement", "JYRO-REQ-1539")>]
let ``WrittenAssignmentOperator_IsItsByte`` (op: string, tag: byte) =
    match programOf (sprintf "var x = 10\nx %s 3" op) with
    | [ _; SAssign(_, t, _, _, _) ] -> Assert.Equal(tag, t)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1539")>]
let ``Reader_AssignmentOperatorBytes_MeanTheirOperators`` () =
    let prog =
        [ for t in 0uy .. 5uy do
            yield var (sprintf "v%d" t) (num 10m)
            yield SAssign(ident (sprintf "v%d" t), t, num 4m, None, p0)
            yield setOut (sprintf "v%d" t) (ident (sprintf "v%d" t))
          yield var "s" (str "a")
          yield SAssign(ident "s", 6uy, str "b", None, p0)
          yield setOut "s" (ident "s") ]
    Assert.Equal("{\"v0\":4,\"v1\":14,\"v2\":6,\"v3\":40,\"v4\":2.5,\"v5\":2,\"s\":\"ab\"}", outJson prog)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1540")>]
let ``ForDirection_To0_Downto1`` () =
    match programOf "for i in 1 to 3 do Output.x = i end\nfor j in 3 downto 1 do Output.y = j end" with
    | [ SFor(_, _, _, _, 0uy, _, _); SFor(_, _, _, _, 1uy, _, _) ] -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    let loop dir start stop = SFor("i", num start, num stop, None, dir, [ assign (out "log") (call "Append" [ out "log"; ident "i" ]) ], p0)
    let v = runs [ setOut "log" (arr []); loop 0uy 1m 4m; loop 1uy 3m 0m ]
    Assert.Equal("[1,2,3,3,2,1]", Values.toJson (Values.prop "log" v))

// -- 150.6.x Literals ------------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2077")>]
let ``WrittenLiterals_AreTagThenPayload`` () =
    let lits = programOf "Output.a = null\nOutput.b = true\nOutput.c = false\nOutput.d = 1\nOutput.e = \"hi\"" |> List.map (function SAssign(_, _, v, _, _) -> v | _ -> ERaw [||])
    match lits with
    | [ ELit(LNull, _); ELit(LBool t, _); ELit(LBool 0uy, _); ELit(LExact 1m, _); ELit(LStr "hi", _) ] when t <> 0uy -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2077")>]
let ``Reader_Literals_AnyNonZeroBooleanByteIsTrue`` () =
    let prog =
        [ setOut "n" nul; setOut "f" (ELit(LBool 0uy, p0)); setOut "t1" (ELit(LBool 1uy, p0)); setOut "t2" (ELit(LBool 2uy, p0))
          setOut "t3" (ELit(LBool 0xFFuy, p0)); setOut "d" (num 1.5m); setOut "s" (str "hi") ]
    Assert.Equal("{\"n\":null,\"f\":false,\"t1\":true,\"t2\":true,\"t3\":true,\"d\":1.5,\"s\":\"hi\"}", outJson prog)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2078")>]
let ``WrittenNumber_IsRepresentationByteThenValue`` () =
    match valueOf "Output.x = 0.1" with
    | ELit(LExact 0.1m, _) -> ()
    | other -> Values.fail (sprintf "0.1: unexpected %A" other)
    match valueOf "Output.x = 1e300" with
    | ELit(LInexact f, _) -> Assert.Equal(1e300, f)
    | other -> Values.fail (sprintf "1e300: unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2078")>]
let ``Reader_Representation0IsInexact_1IsExact`` () =
    let v = runs [ setOut "a" (inexact 0.25); setOut "b" (inexact -1.5e300); setOut "c" (num 0.25m) ]
    Assert.False(Values.isExact (Values.prop "a" v))
    Assert.Equal(0.25, Values.asFloat (Values.prop "a" v))
    Assert.Equal(-1.5e300, Values.asFloat (Values.prop "b" v))
    Assert.True(Values.isExact (Values.prop "c" v))

let private exactBytes (d: decimal) : byte[] =
    let w = Writer(1uy)
    w.Exact d
    w.Bytes()

[<Theory>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("1.5")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("0.0000000000000000000000000001")>]
[<Trait("requirement", "JYRO-REQ-1543")>]
let ``WrittenExactNumber_IsSignificandZeroZeroScaleSign`` (literal: string) =
    // The book's decoder reads the 16 bytes (reserved bytes zero, scale at most
    // 28, sign 0x00 or 0x80) and gives significand / 10^scale.
    let d = Decimal.Parse(literal, Globalization.CultureInfo.InvariantCulture)
    match valueOf ("Output.x = " + literal) with
    | ELit(LExact v, _) -> Assert.Equal(d, v)
    | other -> Values.fail (sprintf "unexpected %A" other)
    // Each value has one encoding: the smallest scale (150.6.7), which these
    // literals are already written with, so the bytes are exactly these.
    let bytes, _ = writtenBy Script.defaults ("Output.x = " + literal)
    let expected = Array.append [| 0x03uy; 0x01uy |] (exactBytes d)
    let found = Seq.windowed expected.Length bytes |> Seq.exists (fun w -> w = expected)
    Assert.True(found, sprintf "%s is not written as its 16 exact bytes" literal)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1543")>]
let ``Reader_ExactBytes_AreSignificandOverTenToTheScale_WithSign`` () =
    let raw (sig96: byte[]) (scale: byte) (sign: byte) = ELit(LExactBytes(Array.concat [ sig96; [| 0uy; 0uy; scale; sign |] ]), p0)
    let le12 (n: bigint) = Array.init 12 (fun i -> byte ((n >>> (8 * i)) &&& 255I))
    let prog =
        [ setOut "a" (raw (le12 225I) 2uy 0x80uy)
          setOut "b" (raw (le12 15I) 1uy 0uy)
          setOut "c" (raw (Array.create 12 0xFFuy) 0uy 0uy)
          setOut "d" (raw (le12 1I) 28uy 0uy) ]
    let v = runs prog
    Assert.Equal(-2.25m, Values.asDecimal (Values.prop "a" v))
    Assert.Equal(1.5m, Values.asDecimal (Values.prop "b" v))
    Assert.Equal(Decimal.MaxValue, Values.asDecimal (Values.prop "c" v))
    Assert.Equal(0.0000000000000000000000000001m, Values.asDecimal (Values.prop "d" v))
    for k in [ "a"; "b"; "c"; "d" ] do Assert.True(Values.isExact (Values.prop k v))

[<Theory>]
[<InlineData("0.1")>]
[<InlineData("1e28")>]
[<InlineData("1e29")>]
[<InlineData("1e300")>]
[<InlineData("3.14159265358979323846")>]
[<InlineData("12345678901234567890123456789")>]
[<InlineData("0.30000000000000000000000000001")>]
[<Trait("requirement", "JYRO-REQ-2079")>]
let ``RoundTrip_KeepsTheValueAndTheRepresentation`` (literal: string) =
    let src = "Output.x = " + literal
    let fromSource = Script.run src |> ok |> Values.prop "x"
    let bytes, a = writtenBy Script.defaults src
    let fromArtifact = load bytes |> ok |> Values.prop "x"
    Assert.Equal(Values.toJson fromSource, Values.toJson fromArtifact)
    Assert.Equal(Values.isExact fromSource, Values.isExact fromArtifact)
    match assignedValue a.Program with
    | ELit(LExact _, _) -> Assert.True(Values.isExact fromSource, "an inexact value was written exact")
    | ELit(LInexact _, _) -> Assert.False(Values.isExact fromSource, "an exact value was written inexact")
    | other -> Values.fail (sprintf "unexpected %A" other)

let invalidLiterals : obj[] seq =
    let exact (reserved12: byte) (reserved13: byte) (scale: byte) (sign: byte) =
        ELit(LExactBytes(Array.concat [ Array.create 12 1uy; [| reserved12; reserved13; scale; sign |] ]), p0)
    [ "tag-0", ELit(LRaw(0x00uy, [||]), p0); "tag-5", ELit(LRaw(0x05uy, [| 0uy |]), p0); "tag-ff", ELit(LRaw(0xFFuy, [||]), p0)
      "rep-2", ELit(LNumRaw(0x02uy, Array.zeroCreate 16), p0); "rep-ff", ELit(LNumRaw(0xFFuy, Array.zeroCreate 16), p0)
      "reserved-12", exact 1uy 0uy 0uy 0uy; "reserved-13", exact 0uy 1uy 0uy 0uy
      "scale-29", exact 0uy 0uy 29uy 0uy; "scale-ff", exact 0uy 0uy 0xFFuy 0uy
      "sign-01", exact 0uy 0uy 0uy 0x01uy; "sign-7f", exact 0uy 0uy 0uy 0x7Fuy; "sign-81", exact 0uy 0uy 0uy 0x81uy
      "sign-ff", exact 0uy 0uy 0uy 0xFFuy ]
    |> Seq.map (fun (n, e) -> [| box n; box (build [ setOut "x" e ]) |])

[<Theory>]
[<MemberData(nameof invalidLiterals)>]
[<Trait("requirement", "JYRO-REQ-2080")>]
let ``Reader_RejectsAnInvalidLiteral`` (name: string, bytes: byte[]) =
    ignore name
    refused MessageCode.ArtifactUnreadable bytes

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2080")>]
let ``Reader_AcceptsTheBoundaryExactNumbers`` () =
    let exact (scale: byte) (sign: byte) = ELit(LExactBytes(Array.concat [ Array.create 12 1uy; [| 0uy; 0uy; scale; sign |] ]), p0)
    let v = runs [ setOut "a" (exact 28uy 0uy); setOut "b" (exact 0uy 0x80uy); setOut "c" (exact 0uy 0uy) ]
    Assert.True(Values.asDecimal (Values.prop "b" v) < 0m)

let nonFinite : obj[] seq =
    [ "quiet-nan", BitConverter.GetBytes(Double.NaN)
      "signaling-nan", BitConverter.GetBytes(0x7FF0000000000001L)
      "negative-nan", BitConverter.GetBytes(0xFFF8000000000000UL)
      "payload-nan", BitConverter.GetBytes(0x7FF8DEADBEEF0001L)
      "positive-infinity", BitConverter.GetBytes(Double.PositiveInfinity)
      "negative-infinity", BitConverter.GetBytes(Double.NegativeInfinity) ]
    |> Seq.map (fun (n, b) -> [| box n; box (build [ setOut "x" (ELit(LNumRaw(0x00uy, b), p0)) ]) |])

[<Theory>]
[<MemberData(nameof nonFinite)>]
[<Trait("requirement", "JYRO-REQ-2081")>]
let ``Reader_RejectsANonFiniteInexactNumber_AsUnreadable`` (name: string, bytes: byte[]) =
    ignore name
    refused MessageCode.ArtifactUnreadable bytes

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2081")>]
let ``Reader_AcceptsFiniteInexactNumbers`` () =
    let v = runs [ setOut "a" (inexact 2.0); setOut "b" (inexact Double.MaxValue); setOut "c" (inexact -0.0) ]
    Assert.Equal(2.0, Values.asFloat (Values.prop "a" v))
    Assert.Equal(Double.MaxValue, Values.asFloat (Values.prop "b" v))

// -- 150.7.x Expressions ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2082")>]
let ``WrittenExpressions_HaveTheTagsAndFieldsOfTheTable`` () =
    let src =
        lines [ "union U A(x) B end"
                "var p = 1"
                "var o = {k: [1]}"
                "Output.lit = 1"
                "Output.id = p"
                "Output.bin = p + 2"
                "Output.un = -p"
                "Output.tern = p > 0 ? 1 : 2"
                "Output.call = Length([p])"
                "Output.prop = o.k"
                "Output.idx = o[\"k\"]"
                "Output.obj = {a: 1, b: 2}"
                "Output.arr = [1, 2]"
                "Output.lam = Map([1], v => v)"
                "Output.tc = p is not string"
                "Output.inc = p++"
                "Output.pre = --p"
                "Output.m = match A(1) do case A(x) then x case B then 0 end" ]
    let values = programOf src |> List.choose (function SAssign(EProp(EId("Output", _), n, _), _, v, _, _) -> Some(n, v) | _ -> None) |> Map.ofList
    let check name (ok: Expr -> bool) = Assert.True(ok values.[name], sprintf "%s: %A" name values.[name])
    check "lit" (function ELit(LExact 1m, _) -> true | _ -> false)
    check "id" (function EId("p", _) -> true | _ -> false)
    check "bin" (function EBin(EId("p", _), 0uy, ELit(LExact 2m, _), _) -> true | _ -> false)
    check "un" (function EUn(0uy, EId("p", _), _) -> true | _ -> false)
    check "tern" (function ETern(EBin(_, 9uy, _, _), ELit(LExact 1m, _), ELit(LExact 2m, _), _) -> true | _ -> false)
    check "call" (function ECall("Length", Positional [ EArr([ EId("p", _) ], _) ], _) -> true | _ -> false)
    check "prop" (function EProp(EId("o", _), "k", _) -> true | _ -> false)
    check "idx" (function EIdx(EId("o", _), ELit(LStr "k", _), _) -> true | _ -> false)
    check "obj" (function EObj([ "a", ELit(LExact 1m, _); "b", ELit(LExact 2m, _) ], _) -> true | _ -> false)
    check "arr" (function EArr([ ELit(LExact 1m, _); ELit(LExact 2m, _) ], _) -> true | _ -> false)
    check "lam" (function ECall("Map", Positional [ _; ELambda([ "v" ], EId("v", _), _) ], _) -> true | _ -> false)
    check "tc" (function ETypeCheck(EId("p", _), 2uy, n, _) -> n <> 0uy | _ -> false)
    check "inc" (function EIncDec(EId("p", _), i, 0uy, _) -> i <> 0uy | _ -> false)
    check "pre" (function EIncDec(EId("p", _), 0uy, pr, _) -> pr <> 0uy | _ -> false)
    check "m" (function EMatch(ECall("A", _, _), [ "A", [ "x" ], EId("x", _), _; "B", [], ELit(LExact 0m, _), _ ], _) -> true | _ -> false)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2082")>]
let ``Reader_ExpressionNodes_HaveTheirMeaning`` () =
    let prog =
        [ unionOf "U" [ "A", [ "x", None ]; "B", [] ]
          var "o" (EObj([ "k", arr [ num 10m; num 20m ] ], p0))
          var "i" (num 5m)
          setOut "tern" (ETern(fls, num 1m, num 2m, p0))
          setOut "prop" (EIdx(prop (ident "o") "k", num 1m, p0))
          setOut "obj" (EObj([ "a", num 1m; "b", str "x" ], p0))
          setOut "lam" (call "Map" [ arr [ num 1m; num 2m ]; ELambda([ "v" ], bin (ident "v") Op.mul (num 3m), p0) ])
          setOut "is" (ETypeCheck(num 1m, Hint.number, 0uy, p0))
          setOut "isnot" (ETypeCheck(num 1m, Hint.number, 0x42uy, p0))
          setOut "postinc" (EIncDec(ident "i", 7uy, 0uy, p0))
          setOut "after1" (ident "i")
          setOut "predec" (EIncDec(ident "i", 0uy, 9uy, p0))
          setOut "after2" (ident "i")
          setOut "m" (EMatch(call "A" [ num 4m ], [ "A", [ "y" ], bin (ident "y") Op.add (num 1m), p0; "B", [], num 0m, p0 ], p0)) ]
    Assert.Equal(
        "{\"tern\":2,\"prop\":20,\"obj\":{\"a\":1,\"b\":\"x\"},\"lam\":[3,6],\"is\":true,\"isnot\":false,"
        + "\"postinc\":5,\"after1\":6,\"predec\":5,\"after2\":5,\"m\":5}",
        outJson prog)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2083")>]
let ``WrittenCallArguments_PositionalAndNamed`` () =
    match valueOf "Output.x = Length([1, 2])" with
    | ECall("Length", Positional [ EArr(_, _) ], _) -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    match valueOf "Output.x = Round(value: 1.25, decimals: 1)" with
    | ECall("Round", Named(form, [ "value", _; "decimals", _ ]), _) -> Assert.NotEqual(0uy, form)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2083")>]
let ``Reader_CallArguments_PositionalAndAnyNonZeroFormIsNamed`` () =
    let named form = ECall("Round", Named(form, [ "decimals", num 1m; "value", num 1.25m ]), p0)
    let prog =
        [ setOut "p" (call "Round" [ num 1.25m; num 1m ])
          setOut "n1" (named 0x01uy)
          setOut "n7" (named 0x7Fuy) ]
    let v = runs prog
    let p = Values.asDecimal (Values.prop "p" v)
    Assert.Equal(p, Values.asDecimal (Values.prop "n1" v))
    Assert.Equal(p, Values.asDecimal (Values.prop "n7" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2084")>]
let ``MatchExpressionCase_IsVariantBindingsBodyPosition`` () =
    let src = "union U Z O(a) T(a, b) end\nOutput.x = match T(1, 2) do case Z then 0 case O(a) then a case T(a, b) then a + b end"
    match valueOf src with
    | EMatch(_, [ "Z", [], _, _; "O", [ "a" ], _, _; "T", [ "a"; "b" ], _, _ ], _) -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    let prog =
        [ unionOf "U" [ "Z", []; "O", [ "a", None ]; "T", [ "a", None; "b", None ] ]
          setOut "x" (EMatch(call "T" [ num 1m; num 2m ], [ "Z", [], num 0m, p0; "T", [ "p"; "q" ], bin (ident "p") Op.sub (ident "q"), p0; "O", [ "r" ], ident "r", p0 ], p0)) ]
    Assert.Equal("{\"x\":-1}", outJson prog)

// -- 150.8.x Statements ----------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1551")>]
let ``WrittenStatements_HaveTheTagsAndFieldsOfTheTable`` () =
    let src =
        lines [ "var a"
                "var b: number = 1"
                "b += 2"
                "Output.h: string = 5"
                "if b > 0 then Output.i = 1 elseif b < 0 then Output.i = 2 elseif false then Output.i = 3 else Output.i = 4 end"
                "if false then Output.j = 1 end"
                "while false do Output.w = 1 end"
                "foreach e in [1] do Output.e = e end"
                "for i in 0 to 2 do Output.f = i end"
                "for k in 4 downto 0 by 2 do Output.g = k end"
                "switch 1 do case 1, 2 then Output.s = 1 default then Output.s = 0 end"
                "switch 1 do case 1 then Output.t = 1 end"
                "func F(x: number, y = 2)"
                "  while true do"
                "    break"
                "  end"
                "  foreach z in [1] do"
                "    continue"
                "  end"
                "  return x"
                "end"
                "func G()"
                "  return"
                "end"
                "Mark(1)"
                "union U A(x: number, y) B end"
                "match A(1, 2) do case A(x, y) then Output.m = x case B then Output.m = 0 end"
                "Output.d = {q: 1}"
                "delete Output.d.q"
                "if false then fail \"x\" end"
                "if false then fail end"
                "if false then exit 1 end"
                "exit" ]
    let m = marker ()
    let prog = (writtenBy { Script.defaults with Functions = [ m.AsFunction ] } src |> snd).Program |> List.map stripStmt
    let tagOf (s: Stmt) =
        match s with
        | SVar _ -> 0x20 | SAssign _ -> 0x21 | SIf _ -> 0x22 | SWhile _ -> 0x23 | SForEach _ -> 0x24 | SFor _ -> 0x25
        | SSwitch _ -> 0x26 | SReturn _ -> 0x27 | SFail _ -> 0x28 | SBreak _ -> 0x29 | SContinue _ -> 0x2A | SExpr _ -> 0x2B
        | SFunc _ -> 0x2C | SExit _ -> 0x2D | SUnion _ -> 0x2E | SMatch _ -> 0x2F | SDelete _ -> 0x30 | SRaw _ -> 0
    Assert.Equal<int list>(
        [ 0x20; 0x20; 0x21; 0x21; 0x22; 0x22; 0x23; 0x24; 0x25; 0x25; 0x26; 0x26; 0x2C; 0x2C; 0x2B; 0x2E; 0x2F; 0x21; 0x30; 0x22; 0x22; 0x22; 0x2D ],
        prog |> List.map tagOf)
    let at i = prog.[i]
    Assert.Equal(SVar("a", None, None, p0), at 0)
    Assert.Equal(SVar("b", Some 1uy, Some(ELit(LExact 1m, p0)), p0), at 1)
    match at 2 with SAssign(EId("b", _), 1uy, ELit(LExact 2m, _), None, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 3 with SAssign(_, 0uy, _, Some 2uy, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 4 with SIf(_, [ _ ], [ _; _ ], Some [ _ ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 5 with SIf(_, [ _ ], [], None, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 8 with SFor("i", _, _, None, 0uy, [ _ ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 9 with SFor("k", _, _, Some(ELit(LExact 2m, _)), 1uy, [ _ ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 10 with SSwitch(_, [ [ _; _ ], [ _ ] ], Some [ _ ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 11 with SSwitch(_, [ [ _ ], [ _ ] ], None, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 12 with
    | SFunc("F", [ "x", Some 1uy, None; "y", None, Some(ELit(LExact 2m, _)) ], [ SWhile(_, [ SBreak _ ], _); SForEach("z", _, [ SContinue _ ], _); SReturn(Some _, _) ], _) -> ()
    | s -> Values.fail (sprintf "%A" s)
    match at 13 with SFunc("G", [], [ SReturn(None, _) ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 15 with SUnion("U", [ "A", [ "x", Some 1uy; "y", None ], _; "B", [], _ ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 16 with SMatch(_, [ "A", [ "x"; "y" ], [ _ ], _; "B", [], [ _ ], _ ], _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 19 with SIf(_, [ SFail(Some _, _) ], _, _, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 20 with SIf(_, [ SFail(None, _) ], _, _, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 21 with SIf(_, [ SExit(Some _, _) ], _, _, _) -> () | s -> Values.fail (sprintf "%A" s)
    match at 22 with SExit(None, _) -> () | s -> Values.fail (sprintf "%A" s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1551")>]
let ``Reader_StatementNodes_HaveTheirMeaning`` () =
    let app v = assign (out "log") (call "Append" [ out "log"; v ])
    let prog =
        [ unionOf "U" [ "A", [ "x", Some Hint.number ]; "B", [] ]
          SFunc("F", [ "x", None, None; "y", None, Some(num 10m) ], [ SReturn(Some(bin (ident "x") Op.add (ident "y")), p0) ], p0)
          setOut "log" (arr [])
          SVar("n", Some Hint.number, Some(num 1m), p0)
          SAssign(ident "n", Op.addAssign, num 2m, None, p0)
          app (ident "n")
          SIf(fls, [ app (num 100m) ], [ fls, [ app (num 101m) ]; tru, [ app (num 102m) ] ], Some [ app (num 103m) ], p0)
          SWhile(bin (ident "n") Op.lt (num 5m), [ SAssign(ident "n", Op.addAssign, num 1m, None, p0) ], p0)
          app (ident "n")
          SForEach("e", arr [ num 7m; num 8m ], [ app (ident "e") ], p0)
          SFor("i", num 0m, num 6m, Some(num 3m), 0uy, [ app (ident "i") ], p0)
          SSwitch(num 2m, [ [ num 1m ], [ app (num 201m) ]; [ num 3m; num 2m ], [ app (num 202m) ] ], Some [ app (num 203m) ], p0)
          SSwitch(num 9m, [ [ num 1m ], [ app (num 301m) ] ], Some [ app (num 302m) ], p0)
          app (call "F" [ num 1m ])
          var "cnt" (num 0m)
          SExpr(EIncDec(ident "cnt", 1uy, 0uy, p0), p0)
          app (ident "cnt")
          SForEach("k", arr [ num 1m; num 2m; num 3m ], [ SIf(bin (ident "k") Op.eq (num 2m), [ SContinue p0 ], [], None, p0); SIf(bin (ident "k") Op.eq (num 3m), [ SBreak p0 ], [], None, p0); app (ident "k") ], p0)
          SMatch(call "A" [ num 4m ], [ "B", [], [ app (num 0m) ], p0; "A", [ "z" ], [ app (ident "z") ], p0 ], p0)
          setOut "d" (EObj([ "q", num 1m; "r", num 2m ], p0))
          SDelete(prop (out "d") "q", p0)
          SExit(Some(str "bye"), p0)
          app (num 404m) ]
    let v = runs prog
    Assert.Equal("[3,102,5,7,8,0,3,202,302,11,1,1,4]", Values.toJson (Values.prop "log" v))
    Assert.Equal("{\"r\":2}", Values.toJson (Values.prop "d" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1551")>]
let ``Reader_FailAndReturnWithAndWithoutValues`` () =
    let r = load (build [ setOut "a" (num 1m); SFail(Some(str "boom"), p0) ])
    fails MessageCode.ScriptFailure r
    Assert.Equal("boom", (Script.onlyErrorMessage r).Message)
    let r2 = load (build [ SFail(None, p0) ])
    fails MessageCode.ScriptFailure r2
    let v = runs [ SFunc("G", [], [ SReturn(None, p0) ], p0); setOut "g" (call "G" []); SExit(None, p0) ]
    Assert.Equal("{\"g\":null}", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1552")>]
let ``Reader_RejectsEveryStatementTagOutside0x20To0x30`` () =
    let rejectedWrongly = ResizeArray<string>()
    for t in [ 0x00 .. 0x1F ] @ [ 0x31 .. 0xFF ] do
        let bytes = build [ SRaw(Array.append [| byte t |] (posBytes p0)) ]
        let r = loadOnly bytes
        if r.IsSuccess || Script.errorCodes r <> [ MessageCode.ArtifactUnreadable ] then
            rejectedWrongly.Add(sprintf "0x%02X: %s" t (Script.describe r))
    Assert.True(rejectedWrongly.Count = 0, String.Join("\n", rejectedWrongly))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1553")>]
let ``SwitchCase_IsValuesThenBody`` () =
    match programOf "switch 3 do case 1 then Output.a = 1 case 2, 3 then Output.a = 2\n  Output.b = 3 case 4 then end" with
    | [ SSwitch(_, [ [ ELit(LExact 1m, _) ], [ _ ]; [ ELit(LExact 2m, _); ELit(LExact 3m, _) ], [ _; _ ]; [ _ ], [] ], None, _) ] -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    let v = runs [ SSwitch(num 3m, [ [ num 1m ], [ setOut "a" (num 1m) ]; [ num 2m; num 3m ], [ setOut "a" (num 2m); setOut "b" (num 3m) ] ], None, p0) ]
    Assert.Equal("{\"a\":2,\"b\":3}", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1554")>]
let ``MatchCase_IsVariantBindingsBodyPosition`` () =
    match programOf "union U Z O(a) T(a, b) end\nmatch T(1, 2) do case Z then Output.z = 1 case O(a) then Output.o = a case T(a, b) then Output.t = a + b end" with
    | [ _; SMatch(_, [ "Z", [], [ _ ], _; "O", [ "a" ], [ _ ], _; "T", [ "a"; "b" ], [ _ ], _ ], _) ] -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    let v =
        runs [ unionOf "U" [ "Z", []; "O", [ "a", None ]; "T", [ "a", None; "b", None ] ]
               SMatch(call "T" [ num 5m; num 2m ], [ "T", [ "p"; "q" ], [ setOut "t" (bin (ident "p") Op.sub (ident "q")) ], p0; "Z", [], [], p0; "O", [ "r" ], [], p0 ], p0) ]
    Assert.Equal("{\"t\":3}", Values.toJson v)

// -- 150.8.5 A union variant: name, fields, then its position (JYRO-REQ-2121, JYRO-ADR-138) ----------

/// The variants of the one union statement `source` compiles to, positions as written.
let private variantsOf (source: string) : Variant list =
    let _, a = writtenBy pinned source
    match a.Program |> List.tryPick (function SUnion(_, vs, _) -> Some vs | _ -> None) with
    | Some vs -> vs
    | None -> Values.fail (sprintf "no union in %A" a.Program)

let private at (line, col, start, stop) : Pos = { Line = line; Column = col; Start = start; End = stop }

/// Assert the variant `name` of `vs` has the position `expected`.
let private variantAt (vs: Variant list) (name: string) (expected: Pos) : unit =
    match vs |> List.filter (fun (n, _, _) -> n = name) with
    | [ _, _, p ] ->
        if p <> expected then
            Values.fail (sprintf "variant %s: expected the position %A, got %A (all: %A)" name expected p vs)
    | other -> Values.fail (sprintf "expected one variant %s, got %A" name other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a union variant is name, field count, fields with optional hints, then its position`` () =
    match programOf "union U Z O(a) T(a: number, b: string) end" with
    | [ SUnion("U", [ "Z", [], _; "O", [ "a", None ], _; "T", [ "a", Some 1uy; "b", Some 2uy ], _ ], _) ] -> ()
    | other -> Values.fail (sprintf "unexpected %A" other)
    // Read back, the variant behaves as the same union compiled from source.
    let v =
        runs [ unionOf "U" [ "Z", []; "T", [ "a", Some Hint.number; "b", Some Hint.string ] ]
               setOut "t" (call "T" [ str "7"; num 8m ])
               setOut "z" (call "Z" []) ]
    let fromSource = Script.run "union U Z T(a: number, b: string) end\nOutput.t = T(\"7\", 8)\nOutput.z = Z()" |> ok
    Assert.Equal(Values.toJson fromSource, Values.toJson v)
    Assert.Equal("{\"_variant\":\"Z\"}", Values.toJson (Values.prop "z" v))

/// (1) The book example.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 the book example, union U A B(x, y number), gives A and B their positions`` () =
    let vs = variantsOf "union U A B(x, y: number) end"
    variantAt vs "A" (at (1, 9, 8, 9))
    variantAt vs "B" (at (1, 11, 10, 25))

/// (2) The range: a variant without fields ends at its name, with fields at the `)` of its
/// fields; no whitespace or comment before or after it is in it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a variant's range is its name to the end of its fields, without whitespace or comments`` () =
    // One per line, indented: "union U\n  A\n  B(x)\nend".
    let vs = variantsOf "union U\n  A\n  B(x)\nend"
    variantAt vs "A" (at (2, 3, 10, 11))
    variantAt vs "B" (at (3, 3, 14, 18))
    // Spaces and a comment between them, and after the last.
    let vs = variantsOf "union U A   # one\n  B(x)   # two\nend"
    variantAt vs "A" (at (1, 9, 8, 9))
    variantAt vs "B" (at (2, 3, 20, 24))
    // A comment line between two variants.
    let vs = variantsOf "union U\n  A\n# between\n  B\nend"
    variantAt vs "A" (at (2, 3, 10, 11))
    variantAt vs "B" (at (4, 3, 24, 25))
    // Spaces inside the field list are inside the range, which ends at its `)`.
    let vs = variantsOf "union U A( x ,  y ) end"
    variantAt vs "A" (at (1, 9, 8, 19))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a variant's position counts CRLF as one line end and both of its code units in the index`` () =
    // "union U\r\n  A\r\n  B(x)\r\nend": A at index 11, B at 16.
    let vs = variantsOf "union U\r\n  A\r\n  B(x)\r\nend"
    variantAt vs "A" (at (2, 3, 11, 12))
    variantAt vs "B" (at (3, 3, 16, 20))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a variant whose field list spans two lines ends at the closing parenthesis`` () =
    // "union U A(x,\n  y) B end": A from index 8 to the `)` at 16; B on line 2.
    let source = "union U A(x,\n  y) B end"
    let vs = variantsOf source
    variantAt vs "A" (at (1, 9, 8, 17))
    variantAt vs "B" (at (2, 6, 18, 19))
    Assert.Equal("A(x,\n  y)", source.Substring(8, 17 - 8))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 every variant's range is the text of its name and fields`` () =
    for source in [ "union U A B(x, y: number) end"
                    "union Shape\n  Circle(r: number)\n  Rect(w: number, h: number)\n  Dot\nend"
                    "union U Z O(a) T(a: number, b: string) end\nunion V Q( a ,b ) end" ] do
        let _, a = writtenBy pinned source
        for s in a.Program do
            match s with
            | SUnion(_, vs, _) ->
                for (n, fs, p) in vs do
                    let text = source.Substring(p.Start, p.End - p.Start)
                    Assert.StartsWith(n, text)
                    if List.isEmpty fs then Assert.Equal(n, text) else Assert.EndsWith(")", text)
                    let line, col = (let before = source.Substring(0, p.Start) in before.Split('\n').Length, p.Start - before.LastIndexOf('\n'))
                    Assert.Equal(line, p.Line)
                    Assert.Equal(col, p.Column)
            | _ -> ()

/// (3) The layout: the bytes written are those the codec encodes, the variant's position last in
/// the variant, before the next variant and the union's own position.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 the bytes of a written union are name, count, fields, position for each variant, then the union's position`` () =
    let source = "union U A B(x, y: number) end"
    let bytes, a = writtenBy pinned source
    match a.Program with
    | [ SUnion("U", [ "A", [], pa; "B", [ "x", None; "y", Some 1uy ], pb ], pu) as u ] ->
        Assert.Equal(at (1, 9, 8, 9), pa)
        Assert.Equal(at (1, 11, 10, 25), pb)
        // Re-encoded from the decoded statement, the bytes are those written.
        Assert.Equal<byte[]>(bytes, encode { a with Program = [ u ] })
        // The statement by hand: tag, name, 2 variants; A: name, 0 fields, position; B: name,
        // 2 fields (x, no hint; y, number), position; then the union's position.
        let w = Writer(0x01uy)
        w.U8 0x2Euy; w.Str "U"; w.U16 2
        w.Str "A"; w.U16 0; w.Pos(at (1, 9, 8, 9))
        w.Str "B"; w.U16 2; w.Str "x"; w.U8 0uy; w.Str "y"; w.U8 1uy; w.U8 Hint.number; w.Pos(at (1, 11, 10, 25))
        w.Pos pu
        let stmt = w.Bytes()
        // The program is a list of one statement: its count, then the statement, to the end.
        Assert.Equal<byte[]>(stmt, bytes.[bytes.Length - stmt.Length ..])
        Assert.Equal<byte[]>([| 1uy; 0uy |], bytes.[bytes.Length - stmt.Length - 2 .. bytes.Length - stmt.Length - 1])
    | other -> Values.fail (sprintf "unexpected program %A" other)

/// (4) Read back: a hand-built union whose variants carry positions loads and runs as the same
/// union compiled from source.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a union whose variants carry positions is read back and runs as from source`` () =
    let union = SUnion("U", [ "A", [], at (1, 9, 8, 9); "B", [ "x", None; "y", Some Hint.number ], at (1, 11, 10, 25) ], at (1, 1, 0, 29))
    let prog =
        [ union
          setOut "a" (call "A" [])
          setOut "b" (call "B" [ num 1m; num 2m ])
          setOut "m" (EMatch(call "B" [ num 3m; num 4m ], [ "A", [], num 0m, p0; "B", [ "p"; "q" ], bin (ident "p") Op.add (ident "q"), p0 ], p0))
          SMatch(call "A" [], [ "A", [], [ setOut "s" (str "a") ], p0; "B", [ "p"; "q" ], [ setOut "s" (str "b") ], p0 ], p0) ]
    let fromSource =
        Script.runWith pinned
            "union U A B(x, y: number) end\nOutput.a = A()\nOutput.b = B(1, 2)\nOutput.m = match B(3, 4) do case A then 0 case B(p, q) then p + q end\nmatch A() do case A then Output.s = \"a\" case B(p, q) then Output.s = \"b\" end"
        |> ok
    Assert.Equal(Values.toJson fromSource, Values.toJson (runs prog))
    Assert.Equal("7", Values.toJson (Values.prop "m" fromSource))
    Assert.Equal("\"a\"", Values.toJson (Values.prop "s" fromSource))

/// (5) A variant cut before its position is complete is unreadable.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a variant that ends before its position is complete is ArtifactUnreadable at Deserialize`` () =
    let full = build [ SUnion("U", [ "A", [ "x", Some Hint.number ], at (1, 9, 8, 12) ], at (1, 1, 0, 16)) ]
    // The statement's last 32 bytes are the variant's position (16) and the union's (16).
    for cut in [ 32; 31; 24; 17 ] do
        let bytes = full.[0 .. full.Length - 1 - cut]
        refused MessageCode.ArtifactUnreadable bytes
        stageOfCode MessageCode.ArtifactUnreadable "Deserialize" (loadOnly bytes)
    // The variant without its position, then the union's position: the union's 16 bytes are
    // read as the variant's, and the statement then ends early.
    let w = Writer(0x01uy)
    w.U8 0x2Euy; w.Str "U"; w.U16 1; w.Str "A"; w.U16 1; w.Str "x"; w.U8 1uy; w.U8 Hint.number; w.Pos(at (1, 1, 0, 16))
    let bytes = build [ SRaw(w.Bytes()) ]
    refused MessageCode.ArtifactUnreadable bytes
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" (loadOnly bytes)

/// (6) Not a node: a union variant adds nothing to the nesting depth (150.9.5). A union at
/// depth 256, inside 255 Ifs, is not refused by the reader; were its variants nodes, they would
/// be at 257.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2121")>]
let ``JYRO-REQ-2121 a union variant is not a node and adds nothing to the nesting depth`` () =
    let rec wrap (m: int) (s: Stmt) : Stmt = if m = 0 then s else SIf(tru, [ wrap (m - 1) s ], [], None, p0)
    let union = SUnion("U", [ "A", [ "x", None ], at (1, 9, 8, 12) ], p0)
    let r = loadOnly (build [ wrap 255 union ])
    Assert.DoesNotContain(MessageCode.ArtifactUnreadable, Script.errorCodes r)
    // At 257 the union itself is too deep.
    refused MessageCode.ArtifactUnreadable (build [ wrap 256 union ])

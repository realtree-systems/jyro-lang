/// 150.3.7 (reserved header bytes), 150.3.8 (the source hash), 150.3.9 (nothing
/// follows the program), 150.4.5 (a writer writes 0x01 for a non-zero flag),
/// 150.4.6 (a string that is not well-formed UTF-8 is refused) and 150.4.7 (a
/// reader takes a position as written). Loads set a generous compilation limit and
/// runs the four execution limits (`pinned`, FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S150_11_HeaderStringsFlagsPositions

open System
open System.Security.Cryptography
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

let private write (source: string) : byte[] = Script.compileBytesWith pinned source |> Script.valueOf

let private read (bytes: byte[]) : JyroResult<JyroArtifactInfo> =
    try JyroArtifactInfo.Read bytes
    with e -> Values.fail (sprintf "JyroArtifactInfo.Read threw %s: %s" (e.GetType().FullName) e.Message)

/// `bytes` with the reserved header bytes (offsets 6 and 7, 150.3.2) set to `a b`.
let private withReserved (a: byte) (b: byte) (bytes: byte[]) : byte[] =
    let c = Array.copy bytes
    c.[6] <- a
    c.[7] <- b
    c

let private diag (r: JyroResult<JyroValue>) =
    [ for m in r.Messages -> m.Code, String.Join("|", m.Args |> Array.map string), (if m.Location.HasValue then string m.Location.Value else "") ]

// -- 150.3.7 The reserved header bytes -------------------------------------------------------------------

let private reservedCorpus =
    [ "Output.x = 1"; "Output.x = Trim(\" a \")\nOutput.y = [1, 2]"; "func F(a)\n  return a * 2\nend\nOutput.r = F(3)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2072")>]
let ``Writer_ReservedHeaderBytes_AreZero`` () =
    for source in reservedCorpus do
        Assert.Equal<byte[]>([| 0uy; 0uy |], (write source).[6..7])

[<Theory>]
[<InlineData(0x01, 0xFF)>]
[<InlineData(0xFF, 0xFF)>]
[<InlineData(0x00, 0x01)>]
[<InlineData(0x80, 0x00)>]
[<Trait("requirement", "JYRO-REQ-2072")>]
let ``Load_ReservedBytesNotZero_LoadsAndRunsTheSameProgram`` (a: int, b: int) =
    // The book's example is `Output.x = 1` with 0x01 0xFF: it loads, runs and sets Output.x to 1.
    let bytes = write "Output.x = 1"
    Assert.Equal("{\"x\":1}", Values.toJson (load (withReserved (byte a) (byte b) bytes) |> ok))
    for source in reservedCorpus do
        let original = load (write source)
        let changed = load (withReserved (byte a) (byte b) (write source))
        Assert.Equal(Values.toJson (ok original), Values.toJson (ok changed))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2072")>]
let ``Load_ReservedBytesNotZero_SameDiagnosticsAsUnchanged`` () =
    let bytes = write "Output.a = 1\nOutput.b = ToUpper([1])"
    let original = load bytes
    fails MessageCode.ArgumentTypeMismatch original
    let changed = load (withReserved 0x01uy 0xFFuy bytes)
    fails MessageCode.ArgumentTypeMismatch changed
    Assert.Equal<(MessageCode * string * string) list>(diag original, diag changed)
    Assert.Equal(Script.value original |> Option.map Values.toJson, Script.value changed |> Option.map Values.toJson)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2072")>]
let ``Read_ReservedBytesNotZero_Succeeds`` () =
    let bytes = write "Output.x = Trim(\" a \")"
    let before = read bytes |> Script.valueOf
    for a, b in [ 0x01uy, 0xFFuy; 0xFFuy, 0xFFuy; 0x00uy, 0x01uy ] do
        let r = read (withReserved a b bytes)
        Assert.True(r.IsSuccess, Script.describe r)
        let after = Script.valueOf r
        Assert.Equal(before.FormatVersion, after.FormatVersion)
        Assert.Equal<byte[]>(before.SourceHash.ToArray(), after.SourceHash.ToArray())
        Assert.Equal<string list>(List.ofSeq before.RequiredFunctions, List.ofSeq after.RequiredFunctions)

// -- 150.3.8 The source hash ------------------------------------------------------------------------------

/// The bytes 150.3.8 makes from `source`: each code point that is not a surrogate,
/// or a high-low pair, as UTF-8 (a leading U+FEFF included); an unpaired
/// surrogate as EF BF BD. No BOM added, no line end changed.
let private hashedBytes (source: string) : byte[] =
    let out = Collections.Generic.List<byte>()
    let mutable i = 0
    while i < source.Length do
        let c = source.[i]
        if Char.IsHighSurrogate c && i + 1 < source.Length && Char.IsLowSurrogate source.[i + 1] then
            out.AddRange(Encoding.UTF8.GetBytes(source.Substring(i, 2)))
            i <- i + 2
        elif Char.IsSurrogate c then
            out.AddRange([| 0xEFuy; 0xBFuy; 0xBDuy |])
            i <- i + 1
        else
            out.AddRange(Encoding.UTF8.GetBytes(string c))
            i <- i + 1
    out.ToArray()

let private headerHash (bytes: byte[]) : byte[] = bytes.[8..39]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1527")>]
let ``Hash_OfAsciiSource_IsOfItsBytes`` () =
    let source = "Output.x = 1"
    Assert.Equal(12, (hashedBytes source).Length)
    Assert.Equal<byte[]>(SHA256.HashData(Encoding.ASCII.GetBytes source), headerHash (write source))

// The leading U+FEFF case of 150.3.8 (15 bytes that start EF BB BF) is JYRO-REQ-20's test, in
// S20_02_SourceEncoding.fs, under JYRO-ADR-122 item 4 (JYRO-DEF-532): it is not tested here.

/// The sources of the hash theory, by label. A row passes only its label: the test
/// runner serializes a row's strings, and a lone surrogate does not survive that (it
/// arrives as U+FFFD, which would hash EF BF BD whatever the writer does with a lone
/// surrogate), so the test builds its text from the label.
let private hashSources : Map<string, string> =
    Map.ofList
        [ "unpaired-low-in-comment", "# a" + string (char 0xDC00)
          "unpaired-high-mid-comment", "# a" + string (char 0xD800) + "b\nOutput.x = 1"
          "high-high-low-in-comment", "# " + string (char 0xD800) + "\U00010000"
          "low-high-in-comment", "# " + string (char 0xDC00) + string (char 0xD800) + "z"
          "pair-in-string-and-comment", "Output.x = \"\U0001F600\" # \U0001F600"
          "crlf-kept", "Output.x = 1\r\nOutput.y = 2\n"
          "feff-not-first-in-string", "Output.x = \"a﻿b\""
          "non-ascii", "Output.x = \"é日\"" ]

let hashCases : obj[] seq = hashSources |> Map.toSeq |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof hashCases)>]
[<Trait("requirement", "JYRO-REQ-1527")>]
let ``JYRO-REQ-1527 the hash is the SHA-256 of the bytes the table gives`` (name: string) =
    let source = hashSources.[name]
    Assert.Equal<byte[]>(SHA256.HashData(hashedBytes source), headerHash (write source))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1527")>]
let ``JYRO-REQ-1527 an unpaired surrogate is hashed as EF BF BD, not dropped and not as its own bytes`` () =
    // A lone high and a lone low surrogate in a comment each give EF BF BD, so the two
    // sources hash alike, and unlike the source without the surrogate.
    let high = write ("# a" + string (char 0xD800))
    let low = write ("# a" + string (char 0xDC00))
    let none = write "# a"
    Assert.Equal<byte[]>(SHA256.HashData [| 0x23uy; 0x20uy; 0x61uy; 0xEFuy; 0xBFuy; 0xBDuy |], headerHash high)
    Assert.Equal<byte[]>(headerHash high, headerHash low)
    Assert.NotEqual<byte[]>(headerHash none, headerHash high)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1527")>]
let ``Hash_KeepsLineEnds_AndAddsNoByteOrderMark`` () =
    let lf = write "Output.x = 1\nOutput.y = 2"
    let crlf = write "Output.x = 1\r\nOutput.y = 2"
    Assert.Equal<byte[]>(SHA256.HashData(Encoding.ASCII.GetBytes "Output.x = 1\r\nOutput.y = 2"), headerHash crlf)
    Assert.NotEqual<byte[]>(headerHash lf, headerHash crlf)

// -- 150.3.9 Nothing follows the program -------------------------------------------------------------------

let private corpus = [ "Output.x = 1"; "var a = [1, 2]\nOutput.b = Map(a, v => v * 2)"; "union U A(x) end\nmatch A(1) do case A(x) then Output.x = x end" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2073")>]
let ``Writer_EndsTheArtifactWithTheLastByteOfTheProgram`` () =
    for source in corpus do
        let bytes = write source
        // The book's codec decodes header, table and program, and fails on any byte left over.
        let a =
            try decode bytes
            with DecodeError m -> Values.fail (sprintf "%s: %s" source m)
        Assert.NotEmpty a.Program

let trailing : obj[] seq =
    [ "one-00", [| 0uy |]
      "one-FF", [| 0xFFuy |]
      "thousand", Array.create 1000 0x20uy
      "a-whole-statement", stmtBytes (setOut "y" (num 2m)) ]
    |> Seq.map (fun (n, b) -> [| box n; box b |])

[<Theory>]
[<MemberData(nameof trailing)>]
[<Trait("requirement", "JYRO-REQ-2073")>]
let ``Load_BytesAfterTheProgram_AreArtifactUnreadable`` (name: string, extra: byte[]) =
    ignore name
    let mark = marker ()
    let bytes = Script.compileBytesWith { pinned with Functions = [ mark.AsFunction ] } "Mark(1)\nOutput.x = 1" |> Script.valueOf
    let r = Script.runBytesWith { pinned with Functions = [ mark.AsFunction ] } (Array.append bytes extra)
    fails MessageCode.ArtifactUnreadable r
    Assert.Equal(0, mark.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2073")>]
let ``Load_TheBookExample_Output_x_1_With00Appended_IsArtifactUnreadable`` () =
    let bytes = Array.append (write "Output.x = 1") [| 0uy |]
    refused MessageCode.ArtifactUnreadable bytes

[<Theory>]
[<MemberData(nameof trailing)>]
[<Trait("requirement", "JYRO-REQ-2073")>]
let ``Read_BytesAfterTheProgram_Succeeds`` (name: string, extra: byte[]) =
    ignore name
    let bytes = write "Output.x = Trim(\" a \")"
    let before = read bytes |> Script.valueOf
    let r = read (Array.append bytes extra)
    Assert.True(r.IsSuccess, Script.describe r)
    let after = Script.valueOf r
    Assert.Equal(before.FormatVersion, after.FormatVersion)
    Assert.Equal<byte[]>(before.SourceHash.ToArray(), after.SourceHash.ToArray())
    Assert.Equal<string list>(List.ofSeq before.RequiredFunctions, List.ofSeq after.RequiredFunctions)

// -- 150.4.5 A writer writes 0x01 for every non-zero flag ------------------------------------------------------

let private written (source: string) : Stmt list = (writtenBy pinned source |> snd).Program

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2074")>]
let ``Writer_BooleanLiterals_Are0201And0200`` () =
    match written "Output.t = true\nOutput.f = false" with
    | [ SAssign(_, _, ELit(LBool t, _), _, _); SAssign(_, _, ELit(LBool f, _), _, _) ] ->
        Assert.Equal(0x01uy, t)
        Assert.Equal(0x00uy, f)
    | other -> Values.fail (sprintf "unexpected program %A" other)
    // The literal `true` is the bytes 02 01 after the Literal tag 0x01.
    let bytes = write "Output.t = true"
    let lit = Array.concat [ [| 0x01uy; 0x02uy; 0x01uy |] ]
    Assert.True(Seq.windowed 3 bytes |> Seq.exists (fun w -> w = lit), "02 01 not found after a Literal tag")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2074")>]
let ``Writer_IncrementDecrement_WritesItsTwoFlagsAs0x01Or0x00`` () =
    let cases = [ "++x", (1uy, 1uy); "x++", (1uy, 0uy); "--x", (0uy, 1uy); "x--", (0uy, 0uy) ]
    for text, (inc, pre) in cases do
        match written ("var x = 1\nOutput.v = " + text) with
        | [ _; SAssign(_, _, EIncDec(EId("x", _), i, p, _), _, _) ] ->
            Assert.True((i = inc && p = pre), sprintf "%s written as %02X %02X" text i p)
        | other -> Values.fail (sprintf "unexpected program for %s: %A" text other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2074")>]
let ``Writer_TypeCheck_IsNot_Is0x01_And_Is_Is0x00`` () =
    match written "Output.a = 1 is not number\nOutput.b = 1 is number" with
    | [ SAssign(_, _, ETypeCheck(_, _, notByte, _), _, _); SAssign(_, _, ETypeCheck(_, _, isByte, _), _, _) ] ->
        Assert.Equal(0x01uy, notByte)
        Assert.Equal(0x00uy, isByte)
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2074")>]
let ``Writer_CallArgForm_Named0x01_Positional0x00`` () =
    match written "Output.a = PadLeft(text: \"a\", length: 3)\nOutput.b = PadLeft(\"a\", 3)" with
    | [ SAssign(_, _, ECall(_, Named(form, _), _), _, _); SAssign(_, _, ECall(_, Positional _, _), _, _) ] ->
        Assert.Equal(0x01uy, form)
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2074")>]
let ``Writer_EveryPresentOptionalValue_Is0x01`` () =
    // Re-encoding the decoded artifact with presence byte 0x01 gives the same bytes only
    // when every present optional value was written as 0x01 (the other flags are kept
    // as decoded).
    let sources =
        [ "var a = 1\nvar b: number = 2\nvar c\nOutput.r = [a, b, c]"
          "func F(x = 1)\n  return x\nend\nfunc G()\n  return\nend\nOutput.r = F()"
          "for i in 0 to 4 by 2 do Output.i = i end\nif true then Output.a = 1 else Output.a = 2 end\nswitch 1 do case 1 then Output.s = 1 default then Output.s = 0 end"
          "Output.h: number = \"7\"\nexit 1" ]
    for source in sources do
        let bytes, a = writtenBy pinned source
        Assert.Equal<byte[]>(bytes, encodeWith 0x01uy a)

let nonZero : obj[] seq = [ 0x02uy; 0x7Fuy; 0x80uy; 0xFFuy ] |> Seq.map (fun b -> [| box b |])

[<Theory>]
[<MemberData(nameof nonZero)>]
[<Trait("requirement", "JYRO-REQ-2074")>]
let ``Reader_ReadsEveryNonZeroFlagByteAs0x01`` (b: byte) =
    let runs (program: Stmt list) = load (build program) |> ok |> Values.toJson
    let run1 (program: byte -> Stmt list) = Assert.Equal(runs (program 0x01uy), runs (program b))
    // A boolean literal.
    run1 (fun f -> [ setOut "v" (ELit(LBool f, p0)) ])
    // The `is` byte of a TypeCheck.
    run1 (fun f -> [ setOut "v" (ETypeCheck(num 1m, Hint.number, f, p0)) ])
    // The first and the second byte of an IncrementDecrement.
    run1 (fun f -> [ var "x" (num 1m); setOut "v" (EIncDec(ident "x", f, 0x01uy, p0)); setOut "x" (ident "x") ])
    run1 (fun f -> [ var "x" (num 1m); setOut "v" (EIncDec(ident "x", 0x01uy, f, p0)); setOut "x" (ident "x") ])
    // The arg-form byte of a Call.
    run1 (fun f -> [ setOut "v" (ECall("PadLeft", Named(f, [ "text", str "a"; "length", num 3m ]), p0)) ])
    // An optional value.
    Assert.Equal(runs [ SVar("a", None, Some(num 5m), p0); setOut "a" (ident "a") ],
                 load (encodeWith b (artifactOf [ SVar("a", None, Some(num 5m), p0); setOut "a" (ident "a") ])) |> ok |> Values.toJson)

// -- 150.4.6 A string that is not well-formed UTF-8 is refused ---------------------------------------------------

/// `bytes` with the one string "QQQ" (03 00 51 51 51) replaced by `raw` with its byte count.
let private patchQQQ (raw: byte[]) (bytes: byte[]) : byte[] =
    let pattern = [| 3uy; 0uy; 0x51uy; 0x51uy; 0x51uy |]
    let at = [ for i in 0 .. bytes.Length - 5 do if bytes.[i .. i + 4] = pattern then yield i ]
    match at with
    | [ i ] -> Array.concat [ bytes.[0 .. i - 1]; [| byte raw.Length; 0uy |]; raw; bytes.[i + 5 ..] ]
    | other -> Values.fail (sprintf "expected one QQQ, found %d" other.Length)

let private illFormed =
    [ "80", [| 0x80uy |]; "FF", [| 0xFFuy |]; "FE", [| 0xFEuy |]; "C0-AF", [| 0xC0uy; 0xAFuy |]; "C1-BF", [| 0xC1uy; 0xBFuy |]
      "C0-80", [| 0xC0uy; 0x80uy |]; "E0-80-AF", [| 0xE0uy; 0x80uy; 0xAFuy |]; "F0-80-80-AF", [| 0xF0uy; 0x80uy; 0x80uy; 0xAFuy |]
      "ED-A0-80", [| 0xEDuy; 0xA0uy; 0x80uy |]; "ED-BF-BF", [| 0xEDuy; 0xBFuy; 0xBFuy |]; "F4-90-80-80", [| 0xF4uy; 0x90uy; 0x80uy; 0x80uy |]
      "F5-80-80-80", [| 0xF5uy; 0x80uy; 0x80uy; 0x80uy |]; "E2-82", [| 0xE2uy; 0x82uy |]; "a-C3", [| 0x61uy; 0xC3uy |] ]

/// Programs holding the one string "QQQ" in each place a string can be.
let private places () : (string * byte[]) list =
    [ "table-name", encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "QQQ" ] }
      "string-literal", build [ setOut "x" (str "QQQ") ]
      "variable-name", build [ var "QQQ" (num 1m) ]
      "identifier", build [ var "a" (num 1m); setOut "x" (ident "QQQ") ]
      "property-name", build [ setOut "QQQ" (num 1m) ]
      "object-key", build [ setOut "x" (EObj([ "QQQ", num 1m ], p0)) ]
      "function-parameter", build [ SFunc("F", [ "QQQ", None, None ], [ SReturn(None, p0) ], p0) ]
      "lambda-parameter", build [ setOut "x" (ELambda([ "QQQ" ], num 1m, p0)) ]
      "variant-name", build [ unionOf "U" [ "QQQ", [] ] ]
      "match-binding", build [ unionOf "U" [ "A", [ "f", None ] ]; SMatch(call "A" [ num 1m ], [ "A", [ "QQQ" ], [], p0 ], p0) ]
      "call-name", encode { artifactOf [ setOut "x" (ECall("QQQ", Positional [], p0)) ] with Functions = [] } ]

let illFormedEverywhere : obj[] seq =
    [ for place, _ in places () do
        for form, _ in illFormed -> [| box place; box form |] ]

[<Theory>]
[<MemberData(nameof illFormedEverywhere)>]
[<Trait("requirement", "JYRO-REQ-2075")>]
let ``Load_IllFormedUtf8_AnywhereInTheArtifact_IsArtifactUnreadable`` (place: string, form: string) =
    let bytes = places () |> List.find (fst >> (=) place) |> snd |> patchQQQ (illFormed |> List.find (fst >> (=) form) |> snd)
    let r = loadOnly bytes
    fails MessageCode.ArtifactUnreadable r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2075")>]
let ``Load_And_Read_TheBookExample_TableNameC080`` () =
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "QQQ" ] } |> patchQQQ [| 0xC0uy; 0x80uy |]
    refused MessageCode.ArtifactUnreadable bytes
    let r = read bytes
    Assert.False(r.IsSuccess)
    Script.expectError MessageCode.ArtifactUnreadable r
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r

/// Read of a table name C0 80 refuses in the form of 160.7.9: a failed result with no value,
/// exactly one Error, ArtifactUnreadable, with the stage Deserialize and no source location.
/// Moved here from JYRO-REQ-2106's checks of Read (JYRO-ADR-113 item 2, JYRO-DEF-506): only
/// this requirement makes the check, so this requirement depends on 1620.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2075")>]
let ``Read_ATableNameC080_IsRefusedThroughItsResult`` () =
    let bytes = encode { artifactOf [] with Functions = [ "QQQ" ] } |> patchQQQ [| 0xC0uy; 0x80uy |]
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
[<Trait("requirement", "JYRO-REQ-2075")>]
let ``Load_TheBookExample_StringLiteralEDA080`` () =
    let bytes = build [ setOut "x" (str "QQQ") ] |> patchQQQ [| 0xEDuy; 0xA0uy; 0x80uy |]
    refused MessageCode.ArtifactUnreadable bytes

[<Theory>]
[<MemberData(nameof illFormedEverywhere)>]
[<Trait("requirement", "JYRO-REQ-2075")>]
let ``Read_IllFormedUtf8_InTheTable_IsRefused_InTheProgram_IsRead`` (place: string, form: string) =
    let bytes = places () |> List.find (fst >> (=) place) |> snd |> patchQQQ (illFormed |> List.find (fst >> (=) form) |> snd)
    let r = read bytes
    if place = "table-name" then
        Script.expectError MessageCode.ArtifactUnreadable r
        stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r
    else
        // Read does not decode the program (150.2.7, 150.2.8).
        Assert.True(r.IsSuccess, Script.describe r)

[<Theory>]
[<InlineData("F0-9F-98-80", "\U0001F600")>]
[<InlineData("EF-BF-BF", "￿")>]
[<InlineData("00", "\u0000")>]
[<InlineData("DF-BF", "߿")>]
[<InlineData("F4-8F-BF-BF", "\U0010FFFF")>]
[<Trait("requirement", "JYRO-REQ-2075")>]
let ``Load_WellFormedUtf8_IsAccepted`` (hex: string, text: string) =
    let raw = hex.Split('-') |> Array.map (fun h -> Convert.ToByte(h, 16))
    let bytes = build [ setOut "x" (str "QQQ") ] |> patchQQQ raw
    let out = load bytes |> ok
    Assert.Equal(text, Values.asString (Values.prop "x" out))

// -- 150.4.7 A reader takes a position as written --------------------------------------------------------------

let rec private mapExpr (f: Pos -> Pos) (e: Expr) : Expr =
    let m = mapExpr f
    match e with
    | ELit(l, p) -> ELit(l, f p)
    | EId(n, p) -> EId(n, f p)
    | EBin(l, op, r, p) -> EBin(m l, op, m r, f p)
    | EUn(op, x, p) -> EUn(op, m x, f p)
    | ETern(a, b, c, p) -> ETern(m a, m b, m c, f p)
    | ECall(n, Positional xs, p) -> ECall(n, Positional(List.map m xs), f p)
    | ECall(n, Named(k, ps), p) -> ECall(n, Named(k, ps |> List.map (fun (a, v) -> a, m v)), f p)
    | EProp(t, n, p) -> EProp(m t, n, f p)
    | EIdx(t, i, p) -> EIdx(m t, m i, f p)
    | EObj(ps, p) -> EObj(ps |> List.map (fun (k, v) -> k, m v), f p)
    | EArr(xs, p) -> EArr(List.map m xs, f p)
    | ELambda(ps, b, p) -> ELambda(ps, m b, f p)
    | ETypeCheck(x, h, n, p) -> ETypeCheck(m x, h, n, f p)
    | EIncDec(x, i, pr, p) -> EIncDec(m x, i, pr, f p)
    | EMatch(x, cases, p) -> EMatch(m x, cases |> List.map (fun (v, bs, b, cp) -> v, bs, m b, f cp), f p)
    | ERaw b -> ERaw b

and private mapStmt (f: Pos -> Pos) (s: Stmt) : Stmt =
    let e = mapExpr f
    let block = List.map (mapStmt f)
    match s with
    | SVar(n, h, i, p) -> SVar(n, h, Option.map e i, f p)
    | SAssign(t, op, v, h, p) -> SAssign(e t, op, e v, h, f p)
    | SIf(c, t, eis, el, p) -> SIf(e c, block t, eis |> List.map (fun (c, b) -> e c, block b), Option.map block el, f p)
    | SWhile(c, b, p) -> SWhile(e c, block b, f p)
    | SForEach(v, c, b, p) -> SForEach(v, e c, block b, f p)
    | SFor(v, a, b, st, d, body, p) -> SFor(v, e a, e b, Option.map e st, d, block body, f p)
    | SSwitch(x, cases, d, p) -> SSwitch(e x, cases |> List.map (fun (vs, b) -> List.map e vs, block b), Option.map block d, f p)
    | SReturn(v, p) -> SReturn(Option.map e v, f p)
    | SFail(v, p) -> SFail(Option.map e v, f p)
    | SBreak p -> SBreak(f p)
    | SContinue p -> SContinue(f p)
    | SExpr(x, p) -> SExpr(e x, f p)
    | SFunc(n, ps, b, p) -> SFunc(n, ps |> List.map (fun (a, h, d) -> a, h, Option.map e d), block b, f p)
    | SExit(v, p) -> SExit(Option.map e v, f p)
    | SUnion(n, vs, p) -> SUnion(n, vs |> List.map (fun (vn, fs, vp) -> vn, fs, f vp), f p)
    | SMatch(x, cases, p) -> SMatch(e x, cases |> List.map (fun (v, bs, b, cp) -> v, bs, block b, f cp), f p)
    | SDelete(t, p) -> SDelete(e t, f p)
    | SRaw b -> SRaw b

/// The artifact of "Output.x = 1\nOutput.y = ToUpper([1])" with the ToUpper Call's position set to `p`.
let private toUpperAt (p: Pos) : byte[] =
    let _, a = writtenBy pinned "Output.x = 1\nOutput.y = ToUpper([1])"
    let program =
        match a.Program with
        | [ s1; SAssign(t, op, ECall(n, args, _), h, sp) ] -> [ s1; SAssign(t, op, ECall(n, args, p), h, sp) ]
        | other -> Values.fail (sprintf "unexpected program %A" other)
    encode { a with Program = program }

/// Load and run it; assert ArgumentTypeMismatch, judged by its code alone (150.4.7 is not
/// about the stage, JYRO-ADR-105 item 4); return line, column, length.
let private reported (bytes: byte[]) : int * int * int =
    let r = load bytes
    fails MessageCode.ArgumentTypeMismatch r
    let d = Script.onlyErrorMessage r
    Assert.True(d.Location.HasValue, "the diagnostic has no Location")
    let l = d.Location.Value
    l.Line, l.Column, l.Length

let private extremes = [ Int32.MinValue; -1; 0; Int32.MaxValue ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``Load_AcceptsEveryPositionValue`` () =
    let _, a = writtenBy pinned "var o = {k: [1, 2]}\nOutput.x = o.k[1] + 1\nOutput.y = Map([1], v => v * 2)"
    let positions =
        [ for v in extremes -> { Line = v; Column = v; Start = v; End = v } ]
        @ [ { Line = 1; Column = 1; Start = 50; End = 40 }; { Line = -3; Column = 0; Start = 1_000_000; End = 2_000_000 } ]
    for p in positions do
        let bytes = encode { a with Program = a.Program |> List.map (mapStmt (fun _ -> p)) }
        let r = load bytes
        Assert.True(r.IsSuccess, sprintf "positions %A: %s" p (Script.describe r))
        Assert.Equal("{\"x\":3,\"y\":[2]}", Values.toJson (ok r))

let bookRows : obj[] seq =
    [ 2, 12, 21, 22, 2, 12, 1
      -3, 0, 50, 40, -3, 0, 0
      1, 1, 0, Int32.MaxValue, 1, 1, Int32.MaxValue
      1, 1, Int32.MinValue, Int32.MaxValue, 1, 1, 0 ]
    |> Seq.map (fun (a, b, c, d, e, f, g) -> [| box a; box b; box c; box d; box e; box f; box g |])

[<Theory>]
[<MemberData(nameof bookRows)>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``LoadedProgram_TheBookTable_ReportsLineColumnAndLength`` (line: int, column: int, start: int, stop: int, eLine: int, eColumn: int, eLength: int) =
    let got = reported (toUpperAt { Line = line; Column = column; Start = start; End = stop })
    Assert.Equal((eLine, eColumn, eLength), got)

[<Theory>]
[<InlineData(7, 7, 0)>]                          // end = start: 0
[<InlineData(-1, 2147483646, 2147483647)>]       // the largest difference that is reported
[<InlineData(-1, 2147483647, 0)>]                // one above: 0, not a wrap and not a saturation
[<InlineData(2147483647, -2147483648, 0)>]       // end far below start: 0
[<InlineData(-2147483648, -2147483648, 0)>]
[<InlineData(-2147483648, -1, 2147483647)>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``LoadedProgram_LengthAtTheBounds`` (start: int, stop: int, length: int) =
    let _, _, got = reported (toUpperAt { Line = 4; Column = 5; Start = start; End = stop })
    Assert.Equal(length, got)

// -- 150.11.4 / 150.9.7 The arguments of ArtifactCannotEncode ------------------------------------------------

/// The first arguments the table of 150.9.7 gives, and whether the row has a second argument.
let private cannotEncodeRows : (string * bool) list =
    [ "File", false; "String", true; "List", true; "FunctionTable", true; "NestingDepth", false
      "ObjectLiteral", true; "If", true; "FuncDef", true; "Call", true; "UnionVariant", true; "Literal", true ]

/// The type names TypeOf gives (130.6.5), the second argument of the Literal row.
let private typeOfNames = [ "null"; "boolean"; "number"; "string"; "array"; "object"; "function" ]

/// Write `source` under `options`; assert the write is refused with no bytes, and return
/// the arguments of every ArtifactCannotEncode diagnostic, each read as 160.7.16 gives.
let private cannotEncodeArgs (options: RunOptions) (source: string) : string list list =
    let r = bounded 30. (fun () -> Script.compileBytesWith options source)
    Assert.False(r.IsSuccess, "the write succeeded")
    Assert.False(r.HasValue, "bytes were given for a refused program")
    let found =
        [ for m in r.Messages do
            if m.Code = MessageCode.ArtifactCannotEncode then
                Assert.NotNull(m.Args)
                yield m.Args |> Array.map argText |> List.ofArray ]
    if found.IsEmpty then
        // A compile-time limit (Part 100) that refuses before the writer runs makes the case
        // unreachable, not passed: the diagnostics say which.
        Values.fail (sprintf "no ArtifactCannotEncode. Diagnostics:\n%s" (Script.describe r))
    // Every ArtifactCannotEncode of the write matches a row of the table: a first argument
    // the table names, then a number alone (or a TypeOf name for Literal), or nothing.
    for args in found do
        match args with
        | [ first ] ->
            Assert.True(cannotEncodeRows |> List.exists (fun (n, second) -> n = first && not second),
                        sprintf "%A matches no row of the table" args)
        | [ "Literal"; t ] -> Assert.True(List.contains t typeOfNames, sprintf "%A: %s is not a TypeOf name" args t)
        | [ first; n ] ->
            Assert.True(cannotEncodeRows |> List.exists (fun (k, second) -> k = first && second && k <> "Literal"),
                        sprintf "%A matches no row of the table" args)
            Assert.True(n.Length > 0 && n |> Seq.forall Char.IsDigit, sprintf "%A: the second argument %A is not a number alone" args n)
        | _ -> Values.fail (sprintf "%A matches no row of the table" args)
    found

/// Assert that the write of `source` is refused with an ArtifactCannotEncode whose
/// arguments are exactly `expected`, and that it gives no ArtifactCannotEncode with others.
let private refusedWith (options: RunOptions) (source: string) (expected: string list) : unit =
    let found = cannotEncodeArgs options source
    Assert.True(List.contains expected found, sprintf "expected the arguments %A; ArtifactCannotEncode gave %A" expected found)
    Assert.True(found |> List.forall ((=) expected), sprintf "expected only %A; ArtifactCannotEncode gave %A" expected found)

let private paramList (n: int) = String.Join(", ", [ for i in 1 .. n -> sprintf "p%d" i ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 the book example, a string literal of 70,000 ASCII characters, gives String and 70000`` () =
    refusedWith pinned (sprintf "Output.x = \"%s\"" (String('a', 70_000))) [ "String"; "70000" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 the book example, an array literal of 10,001 elements, gives List and 10001`` () =
    refusedWith pinned ("Output.x = " + arrayLiteral 10_001) [ "List"; "10001" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 the book example, an object literal of 10,001 properties, gives ObjectLiteral and 10001`` () =
    let source = "Output.x = {" + String.Join(", ", [ for i in 1 .. 10_001 -> sprintf "k%d: 1" i ]) + "}"
    refusedWith pinned source [ "ObjectLiteral"; "10001" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 a string of 40,000 two-byte characters gives String and its length in bytes, 80000`` () =
    refusedWith pinned (sprintf "Output.x = \"%s\"" (String('é', 40_000))) [ "String"; "80000" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 a list of statements above the bound gives List and its number of items`` () =
    // 150.9.3 bounds every list alike, so the program's statement list is a List too.
    refusedWith pinned (lines (repeatLines 10_001 "Output.s = {i}")) [ "List"; "10001" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 an If of 10,001 elseif branches gives If and 10001`` () =
    let source = "if false then Output.x = 0 " + String.replicate 10_001 "elseif false then Output.x = 1 " + "end"
    refusedWith pinned source [ "If"; "10001" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 a function of 10,001 parameters gives FuncDef and 10001`` () =
    refusedWith pinned (sprintf "func F(%s)\n  return 1\nend" (paramList 10_001)) [ "FuncDef"; "10001" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 a union variant of 10,001 fields gives UnionVariant and 10001`` () =
    refusedWith pinned (sprintf "union U A(%s) end" (paramList 10_001)) [ "UnionVariant"; "10001" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 a call of 10,001 named arguments gives Call and 10001`` () =
    // A host function, so that no FuncDef of 10,001 parameters is written; other
    // ArtifactCannotEncode diagnostics of the write are allowed if they match the table.
    let ps = [ for i in 1 .. 10_001 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]
    let wide = Host.func "Wide" ps ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)
    let o = { pinned with Functions = [ wide.AsFunction ] }
    let found = cannotEncodeArgs o (sprintf "Wide(%s)" (String.Join(", ", [ for i in 1 .. 10_001 -> sprintf "a%d: 1" i ])))
    Assert.True(List.contains [ "Call"; "10001" ] found, sprintf "expected [Call; 10001]; ArtifactCannotEncode gave %A" found)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 a program calling 1,001 functions gives FunctionTable and 1001`` () =
    let o = { pinned with Functions = [ for i in 0 .. 1000 -> (Host.recorder (sprintf "F%04d" i)).AsFunction ] }
    refusedWith o (lines [ for i in 0 .. 1000 -> sprintf "F%04d()" i ]) [ "FunctionTable"; "1001" ]

// The NestingDepth row of the table (a depth of 257) cannot be reached from a source: a source above
// 256 is refused at Parse (100.10.4) and the depth of a source is never less than the depth of its
// artifact (100.10.2), so the writer never meets one (JYRO-REQ-2117 plan, JYRO-REQ-2130). The test
// that wrote a source of depth 201 is withdrawn with the bound of 200 (JYRO-REQ-1560), and the row
// is recorded as unreachable for Inspect.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2117")>]
let ``JYRO-REQ-2117 an artifact larger than 10,485,760 bytes gives File alone`` () =
    // 200 strings of 60,000 bytes, each under the string bound: about 12 MB.
    let source = lines [ for i in 0 .. 199 -> sprintf "var v%d = \"%s\"" i (String('s', 60_000)) ]
    refusedWith pinned source [ "File" ]

/// Part 150.9 to 150.11: reader safety limits, the format version and the
/// artifact diagnostics.
module RealTree.Jyro.Spec.Tests.S150_09_ReaderLimits

open System
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

let private maxSize = 10_485_760

/// Assert the reader did not reject the artifact as unreadable or too large
/// (a later stage may still refuse the program for its own reason).
let private notRejectedByTheReader (bytes: byte[]) : unit =
    let r =
        bounded 9. (fun () ->
            Script.loadBytesWith { Script.defaults with CompilationTimeout = Some(TimeSpan.FromSeconds 1.) } bytes)
    let codes = Script.errorCodes r
    Assert.DoesNotContain(MessageCode.ArtifactUnreadable, codes)
    Assert.DoesNotContain(MessageCode.ArtifactTooLarge, codes)

/// Assert the reader rejected the artifact as unreadable, on both load paths.
let private unreadable (bytes: byte[]) : unit =
    bounded 9. (fun () -> refused MessageCode.ArtifactUnreadable bytes)

/// A program whose artifact is exactly `size` bytes: variable declarations of
/// string literals, the last one sized to fit. Every string stays within the
/// 65,535 bytes of 150.9.2, and the bytes are checked to decode back to the
/// program meant, with nothing after it (JYRO-DEF-735: an earlier version left
/// the last string above 65,535 bytes, so its length field disagreed with it).
let private artifactOfSize (size: int) : byte[] =
    let decl (i: int) (n: int) = SVar(sprintf "v%04d" i, None, Some(ELit(LStr(String('s', n)), p0)), p0)
    let overhead = (stmtBytes (decl 0 0)).Length
    let full = 60_000
    let baseSize = (build []).Length
    let room = size - baseSize
    let k0 = room / (overhead + full)
    let rem = room - k0 * (overhead + full)
    // The remainder holds a last declaration when it covers its overhead;
    // otherwise one full declaration is given back to the last one.
    let k, last = if rem >= overhead then k0, rem - overhead else k0 - 1, rem + full
    Assert.InRange(last, 0, 65_535)
    let program = [ for i in 0 .. k - 1 -> decl i full ] @ [ decl k last ]
    let bytes = build program
    Assert.Equal(size, bytes.Length)
    Assert.True((decode bytes = artifactOf program), "the hand-built artifact does not decode back to its program")
    bytes

// -- 150.9.1 File size ----------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2085")>]
[<Trait("requirement", "JYRO-REQ-2090")>]
[<Trait("requirement", "JYRO-REQ-1570")>]
let ``ArtifactOfExactlyTheMaximumSize_IsNotRejectedAsTooLarge`` () =
    let bytes = artifactOfSize maxSize
    notRejectedByTheReader bytes
    let info = JyroArtifactInfo.Read bytes
    Assert.DoesNotContain(MessageCode.ArtifactTooLarge, Script.codes info)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2085")>]
[<Trait("requirement", "JYRO-REQ-2090")>]
[<Trait("requirement", "JYRO-REQ-1570")>]
let ``ArtifactOneByteOverTheMaximum_IsRejectedAsTooLarge`` () =
    let bytes = artifactOfSize (maxSize + 1)
    refused MessageCode.ArtifactTooLarge bytes
    fails MessageCode.ArtifactTooLarge (JyroArtifactInfo.Read bytes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2085")>]
[<Trait("requirement", "JYRO-REQ-1570")>]
let ``Artifactof64MB_IsRejectedAsTooLarge`` () =
    let bytes = Array.zeroCreate<byte> (64 * 1024 * 1024)
    Array.blit (build [ setOut "x" (num 1m) ]) 0 bytes 0 44
    refused MessageCode.ArtifactTooLarge bytes
    fails MessageCode.ArtifactTooLarge (JyroArtifactInfo.Read bytes)

// -- 150.9.2 Strings ----------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2162")>]
let ``JYRO-REQ-2162 StringOf65535Bytes_Loads_AndALengthPastTheData_IsRejected`` () =
    let s = String('z', 65_535)
    let v = load (build [ setOut "x" (str s) ]) |> ok
    Assert.Equal(65_535, (Values.asString (Values.prop "x" v)).Length)
    let full = build [ setOut "x" (str s) ]
    unreadable full.[0 .. full.Length - 1 - 32 - 100]
    Script.compileBytesWith Script.defaults (sprintf "Output.x = \"%s\"" (String('z', 65_536))) |> fails MessageCode.ArtifactCannotEncode

// -- 150.9.3 Lists -----------------------------------------------------------------------------------

let private listCases : (string * (int -> Stmt list)) list =
    [ "array", fun n -> [ setOut "x" (arr [ for _ in 1 .. n -> num 1m ]) ]
      "program", fun n -> [ for _ in 1 .. n -> SExpr(num 1m, p0) ]
      "block", fun n -> [ SWhile(fls, [ for _ in 1 .. n -> SExpr(num 1m, p0) ], p0) ]
      "lambda-params", fun n -> [ setOut "x" (ELambda([ for i in 1 .. n -> sprintf "p%d" i ], num 1m, p0)) ]
      "switch-cases", fun n -> [ SSwitch(num 0m, [ for i in 1 .. n -> [ num (decimal i) ], [] ], None, p0) ]
      "switch-values", fun n -> [ SSwitch(num 0m, [ [ for i in 1 .. n -> num (decimal i) ], [] ], None, p0) ]
      "union-variants", fun n -> [ unionOf "U" [ for i in 1 .. n -> sprintf "V%d" i, [] ] ]
      "match-bindings", fun n -> [ unionOf "U" [ "A", [] ]; SMatch(call "A" [], [ "A", [ for i in 1 .. n -> sprintf "b%d" i ], [], p0 ], p0) ]
      "match-cases", fun n ->
        [ unionOf "U" [ for i in 1 .. n -> sprintf "V%d" i, [] ]
          SMatch(call "V1" [], [ for i in 1 .. n -> sprintf "V%d" i, [], [], p0 ], p0) ]
      "match-expression-cases", fun n ->
        [ unionOf "U" [ for i in 1 .. n -> sprintf "V%d" i, [] ]
          setOut "x" (EMatch(call "V1" [], [ for i in 1 .. n -> sprintf "V%d" i, [], num 1m, p0 ], p0)) ] ]

let listRows : obj[] seq = listCases |> Seq.map (fun (n, _) -> [| box n |])

/// The program and block lists of 10,000 statements are left out: the build
/// takes longer than a test may to compile them (recorded as a gap of 150.9.3).
let acceptedListRows : obj[] seq =
    listCases |> Seq.filter (fun (n, _) -> n <> "program" && n <> "block") |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof acceptedListRows)>]
[<Trait("requirement", "JYRO-REQ-2086")>]
let ``ListOf10000Items_IsNotRejectedByTheReader`` (case: string) =
    let make = listCases |> List.find (fst >> (=) case) |> snd
    notRejectedByTheReader (build (make 10_000))

[<Theory>]
[<MemberData(nameof listRows)>]
[<Trait("requirement", "JYRO-REQ-2086")>]
[<Trait("requirement", "JYRO-REQ-2090")>]
let ``ListOf10001Items_IsRejectedAsUnreadable`` (case: string) =
    let make = listCases |> List.find (fst >> (=) case) |> snd
    unreadable (build (make 10_001))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2086")>]
let ``ListCountClaimedAboveTheBound_WithNoItems_IsRejectedAsUnreadable`` () =
    let head = build []
    for count in [ 10_001; 65_535 ] do
        let bytes = Array.append head.[0 .. head.Length - 3] (BitConverter.GetBytes(uint16 count))
        unreadable bytes

// -- 150.9.4 Function table entries -----------------------------------------------------------------

let private hostSet (n: int) = [ for i in 0 .. n - 1 -> (Host.recorder (sprintf "F%04d" i)).AsFunction ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2087")>]
let ``TableOf1000Entries_IsAccepted`` () =
    let names = [ for i in 0 .. 999 -> sprintf "F%04d" i ]
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = names }
    Script.runBytesWith { Script.defaults with Functions = hostSet 1000 } bytes |> ok |> ignore
    Assert.True((JyroArtifactInfo.Read bytes).IsSuccess)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2087")>]
[<Trait("requirement", "JYRO-REQ-2090")>]
let ``TableOf1001Entries_IsRejected`` () =
    let names = [ for i in 0 .. 1000 -> sprintf "F%04d" i ]
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = names }
    let r = Script.runBytesWith { Script.defaults with Functions = hostSet 1001 } bytes
    fails MessageCode.ArtifactUnreadable r
    Assert.False((JyroArtifactInfo.Read bytes).IsSuccess)
    let huge = encode { artifactOf [] with FunctionCount = Some 0xFFFFFFFFu }
    unreadable huge
    Assert.False((JyroArtifactInfo.Read huge).IsSuccess)

// -- 150.9.5 / 150.9.9 Nesting depth ---------------------------------------------------------------

let rec private wrapIfs (m: int) (s: Stmt) : Stmt = if m = 0 then s else SIf(tru, [ wrapIfs (m - 1) s ], [], None, p0)

let rec private unaries (k: int) (e: Expr) : Expr = if k = 0 then e else EUn(Op.negate, unaries (k - 1) e, p0)

/// A statement and the greatest depth of a node in it when it stands at depth 1.
let private depthCases : (string * Stmt * int) list =
    [ "example", setOut "x" (unaries 2 (num 1m)), 4
      "unary", setOut "x" (unaries 5 (num 1m)), 7
      "if-condition", SIf(unaries 3 tru, [], [], None, p0), 5
      "elseif-block", SIf(fls, [], [ tru, [ setOut "x" (num 1m) ] ], None, p0), 4
      "else-block", SIf(fls, [], [], Some [ setOut "x" (num 1m) ], p0), 4
      "switch-case", SSwitch(num 1m, [ [ num 1m ], [ setOut "x" (num 1m) ] ], None, p0), 4
      "switch-default", SSwitch(num 1m, [], Some [ setOut "x" (num 1m) ], p0), 4
      "match-case", SMatch(call "A" [], [ "A", [], [ setOut "x" (num 1m) ], p0 ], p0), 4
      "match-expression-case", setOut "x" (EMatch(call "A" [], [ "A", [], unaries 2 (num 1m), p0 ], p0)), 5
      "named-argument", setOut "x" (ECall("Round", Named(1uy, [ "value", unaries 1 (num 1m); "decimals", num 0m ]), p0)), 4
      "object-entry", setOut "x" (EObj([ "k", unaries 1 (num 1m) ], p0)), 4
      "lambda-body", setOut "x" (call "Map" [ arr [ num 1m ]; ELambda([ "v" ], unaries 1 (ident "v"), p0) ]), 5 ]

let private union = unionOf "U" [ "A", [] ]

/// The program for a case at total depth `depth`.
let private atDepth (case: string) (depth: int) : Stmt list =
    let _, s, local = depthCases |> List.find (fun (n, _, _) -> n = case)
    [ union; wrapIfs (depth - local) s ]

let depthRows : obj[] seq = depthCases |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof depthRows)>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
let ``JYRO-REQ-2169 NestingDepth256_IsAccepted`` (case: string) =
    notRejectedByTheReader (build (atDepth case 256))

[<Theory>]
[<MemberData(nameof depthRows)>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
[<Trait("requirement", "JYRO-REQ-2090")>]
let ``JYRO-REQ-2169 NestingDepth257_IsRejectedAsUnreadable`` (case: string) =
    unreadable (build (atDepth case 257))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
let ``JYRO-REQ-2169 FunctionBody_IsOneDeeperThanItsDeclaration`` () =
    // FuncDef 1, m Ifs 2..m+1, Return m+2, Unary m+3, Literal m+4.
    let func m = [ SFunc("F", [], [ wrapIfs m (SReturn(Some(unaries 1 (num 1m)), p0)) ], p0) ]
    notRejectedByTheReader (build (func 252))
    unreadable (build (func 253))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
let ``JYRO-REQ-2169 TheBooksExample_HasDepth4`` () =
    // Output.x = -(-(1)) inside 252 Ifs reaches 256; inside 253 Ifs, 257.
    notRejectedByTheReader (build [ wrapIfs 252 (setOut "x" (unaries 2 (num 1m))) ])
    unreadable (build [ wrapIfs 253 (setOut "x" (unaries 2 (num 1m))) ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
let ``JYRO-REQ-2169 TheBooksExample_EachNodeHasTheDepthTheClauseGives`` () =
    // Output.x = -(-(1)): the Assignment 1, the PropertyAccess and the outer Unary 2, the Identifier
    // `Output` and the inner Unary 3, the Literal 4. The greatest depth is the Literal's.
    notRejectedByTheReader (build [ wrapIfs 252 (setOut "x" (unaries 2 (num 1m))) ])
    unreadable (build [ wrapIfs 253 (setOut "x" (unaries 2 (num 1m))) ])
    // Output.x = 1: the Assignment 1, the PropertyAccess 2, the Identifier `Output` 3, the Literal 2;
    // the greatest depth is the Identifier's, 3.
    notRejectedByTheReader (build [ wrapIfs 253 (setOut "x" (num 1m)) ])
    unreadable (build [ wrapIfs 254 (setOut "x" (num 1m)) ])
    // Each further negation adds one.
    notRejectedByTheReader (build [ wrapIfs 251 (setOut "x" (unaries 3 (num 1m))) ])
    unreadable (build [ wrapIfs 252 (setOut "x" (unaries 3 (num 1m))) ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
let ``JYRO-REQ-2169 TheBodyOfAMatchExpressionCase_IsAChildOfItsMatchExpr`` () =
    // MatchExpr at depth d, the body of its case at d + 1 (the case adds nothing): in the Assignment
    // at depth 1 the MatchExpr is 2 and a body of two negations reaches 5 (see the depth table).
    for m, accepted in [ 251, true; 252, false ] do
        let program = [ union; wrapIfs m (setOut "x" (EMatch(call "A" [], [ "A", [], unaries 2 (num 1m), p0 ], p0))) ]
        if accepted then notRejectedByTheReader (build program) else unreadable (build program)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
[<Trait("requirement", "JYRO-REQ-2169")>]
let ``JYRO-REQ-2169 StructuresThatAreNotNodes_AddNothingToTheDepth`` () =
    // A parameter, an elseif pair, a named-argument pair and an object entry are not nodes: the
    // expression inside each stands one deeper than the node that holds the structure, not two.
    let cases : (string * Stmt * int) list =
        [ "parameter-default", SFunc("F", [ "p", None, Some(unaries 1 (num 1m)) ], [], p0), 3
          "elseif-pair", SIf(fls, [], [ unaries 1 tru, [] ], None, p0), 3
          "named-argument", setOut "x" (ECall("Round", Named(1uy, [ "value", unaries 1 (num 1m); "decimals", num 0m ]), p0)), 4
          "object-entry", setOut "x" (EObj([ "k", unaries 1 (num 1m) ], p0)), 4 ]
    for _, s, local in cases do
        let at (depth: int) = [ union; wrapIfs (depth - local) s ]
        notRejectedByTheReader (build (at 256))
        unreadable (build (at 257))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
let ``JYRO-REQ-2135 an artifact of 254 negations is not rejected for its depth, and one of 255 is unreadable`` () =
    // Output.x = -(-(...(1))): the Assignment 1, the Unary nodes 2 to N + 1, the Literal N + 2 (150.9.9).
    notRejectedByTheReader (build [ setOut "x" (unaries 254 (num 1m)) ])
    unreadable (build [ setOut "x" (unaries 255 (num 1m)) ])
    // The bound is not the old bound of 200: 199 negations (depth 201) and 200 (depth 202) are read.
    notRejectedByTheReader (build [ setOut "x" (unaries 199 (num 1m)) ])
    notRejectedByTheReader (build [ setOut "x" (unaries 200 (num 1m)) ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2135")>]
let ``JYRO-REQ-2135 an artifact of 254 negations loads, runs to 1, and one of 255 is refused, on both load paths`` () =
    let ok254 = build [ setOut "x" (unaries 254 (num 1m)) ]
    let r = bounded 20. (fun () -> Script.runBytesWith { Script.defaults with CompilationTimeout = Some generousCompile } ok254)
    Assert.Equal("1", Values.toJson (Values.prop "x" (ok r)))
    unreadable (build [ setOut "x" (unaries 255 (num 1m)) ])
// -- 150.9.6 Other counts ---------------------------------------------------------------------------

let private countCases : (string * (int -> Stmt list)) list =
    [ "object-properties", fun n -> [ setOut "x" (EObj([ for i in 1 .. n -> sprintf "k%d" i, num 1m ], p0)) ]
      "elseifs", fun n -> [ SIf(fls, [], [ for _ in 1 .. n -> fls, [] ], None, p0) ]
      "parameters", fun n -> [ SFunc("F", [ for i in 1 .. n -> sprintf "p%d" i, None, None ], [ SReturn(None, p0) ], p0) ]
      "named-arguments", fun n -> [ setOut "x" (ECall("Round", Named(1uy, [ for i in 1 .. n -> sprintf "a%d" i, num 1m ]), p0)) ]
      "variant-fields", fun n -> [ unionOf "U" [ "A", [ for i in 1 .. n -> sprintf "f%d" i, None ] ] ] ]

let countRows : obj[] seq = countCases |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof countRows)>]
[<Trait("requirement", "JYRO-REQ-2088")>]
let ``CountOf10000_IsNotRejectedByTheReader`` (case: string) =
    let make = countCases |> List.find (fst >> (=) case) |> snd
    notRejectedByTheReader (build (make 10_000))

[<Theory>]
[<MemberData(nameof countRows)>]
[<Trait("requirement", "JYRO-REQ-2088")>]
[<Trait("requirement", "JYRO-REQ-2090")>]
let ``CountOf10001_IsRejectedAsUnreadable`` (case: string) =
    let make = countCases |> List.find (fst >> (=) case) |> snd
    unreadable (build (make 10_001))

[<Theory>]
[<MemberData(nameof countRows)>]
[<Trait("requirement", "JYRO-REQ-2088")>]
let ``CountOf65535WithNoData_IsRejectedAsUnreadable`` (case: string) =
    let make = countCases |> List.find (fst >> (=) case) |> snd
    // Build with one item, then patch the count to 65535 and cut the data after it.
    let one = build (make 1)
    let zero = build (make 0)
    // The first byte where the one-item and zero-item encodings differ is the count.
    let i = Seq.zip one zero |> Seq.findIndex (fun (a, b) -> a <> b)
    let bytes = Array.concat [ one.[0 .. i - 1]; [| 0xFFuy; 0xFFuy |] ]
    unreadable bytes

// -- 150.9.7 The writer refuses what a reader would reject -----------------------------------------

let private cannotEncode (options: RunOptions) (source: string) =
    let r = bounded 9. (fun () -> Script.compileBytesWith options source)
    fails MessageCode.ArtifactCannotEncode r
    Assert.False(r.HasValue, "Bytes were given for a refused program")

let private encodes (options: RunOptions) (source: string) =
    let r = bounded 9. (fun () -> Script.compileBytesWith options source)
    Assert.True(r.IsSuccess, Script.describe r)
    notRejectedByTheReader (Script.valueOf r)

let private manyParams n = String.Join(", ", [ for i in 1 .. n -> sprintf "p%d" i ])

let writerCases : obj[] seq =
    [ "array", ("Output.x = " + arrayLiteral 10_001), ("Output.x = " + arrayLiteral 10_000)
      "object", ("Output.x = {" + String.Join(", ", [ for i in 1 .. 10_001 -> sprintf "k%d: 1" i ]) + "}"),
                ("Output.x = {" + String.Join(", ", [ for i in 1 .. 10_000 -> sprintf "k%d: 1" i ]) + "}")
      "parameters", sprintf "func F(%s)\n  return 1\nend" (manyParams 10_001), sprintf "func F(%s)\n  return 1\nend" (manyParams 10_000)
      "variant-fields", sprintf "union U A(%s) end" (manyParams 10_001), sprintf "union U A(%s) end" (manyParams 10_000)
      "string-ascii", sprintf "Output.x = \"%s\"" (String('a', 65_536)), sprintf "Output.x = \"%s\"" (String('a', 65_535))
      "string-3-byte", sprintf "Output.x = \"%s\"" (String('日', 21_846)), sprintf "Output.x = \"%s\"" (String('日', 21_845))
      // The depth row is gone: a source above 256 is refused at Parse (100.10.4), and the depth of
      // a source is never less than the depth of its artifact (100.10.2), so the writer meets no
      // program that the reader would reject for its depth (JYRO-REQ-2130).
      "statements", lines (repeatLines 10_001 "Output.s = {i}"), lines (repeatLines 100 "Output.s = {i}")
      "elseifs", "if false then Output.x = 0 " + String.replicate 10_001 "elseif false then Output.x = 1 " + "end",
                 "if false then Output.x = 0 " + String.replicate 100 "elseif false then Output.x = 1 " + "end" ]
    |> Seq.map (fun (n, bad, good) -> [| box n; box bad; box good |])

[<Theory>]
[<MemberData(nameof writerCases)>]
[<Trait("requirement", "JYRO-REQ-2089")>]
[<Trait("requirement", "JYRO-REQ-2164")>]
let ``Writer_RefusesAProgramAReaderWouldReject_AndWritesItsBound`` (case: string, bad: string, good: string) =
    ignore case
    cannotEncode Script.defaults bad
    encodes Script.defaults good

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2089")>]
[<Trait("requirement", "JYRO-REQ-2164")>]
let ``Writer_RefusesACallWith10001NamedArguments`` () =
    let ps = [ for i in 1 .. 10_001 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]
    let h = Host.func "Wide" ps ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)
    let args n = String.Join(", ", [ for i in 1 .. n -> sprintf "a%d: 1" i ])
    let o = { Script.defaults with Functions = [ h.AsFunction ] }
    cannotEncode o (sprintf "Wide(%s)" (args 10_001))
    encodes o (sprintf "Wide(%s)" (args 10_000))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2089")>]
[<Trait("requirement", "JYRO-REQ-2164")>]
let ``Writer_RefusesATableOf1001Names`` () =
    let o n = { Script.defaults with Functions = hostSet n }
    cannotEncode (o 1001) (lines [ for i in 0 .. 1000 -> sprintf "F%04d()" i ])
    encodes (o 1000) (lines [ for i in 0 .. 999 -> sprintf "F%04d()" i ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2089")>]
[<Trait("requirement", "JYRO-REQ-2164")>]
let ``Writer_RefusesAnArtifactOverTheMaximumSize`` () =
    let src n = lines [ for i in 0 .. n - 1 -> sprintf "var v%d = \"%s\"" i (String('s', 60_000)) ]
    cannotEncode Script.defaults (src 200)
    encodes Script.defaults (src 150)

// -- 150.10.x Format version -------------------------------------------------------------------------

/// Assert that every path refuses `bytes` with ArtifactVersionUnsupported: a load through
/// Compile (exactly one error, stage Deserialize) and CompileOrThrow, and JyroArtifactInfo.Read.
let private versionRefused (bytes: byte[]) : unit =
    refused MessageCode.ArtifactVersionUnsupported bytes
    stageOfCode MessageCode.ArtifactVersionUnsupported "Deserialize" (loadOnly bytes)
    fails MessageCode.ArtifactVersionUnsupported (JyroArtifactInfo.Read bytes)

/// The book's 44-byte header of 150.10.3 and 150.10.5: JYRX, `version`, reserved 0, a hash
/// and 0 table entries, followed by an empty program (00 00).
let private bookHeader (version: uint16) : byte[] =
    let bytes = encode { artifactOf [] with Version = version }
    Assert.Equal(46, bytes.Length)
    Assert.Equal<byte[]>([| byte (version &&& 0xFFus); byte (version >>> 8) |], bytes.[4..5])
    Assert.Equal<byte[]>([| 0uy; 0uy |], bytes.[44..45])
    bytes

[<Theory>]
[<InlineData(2)>]
[<InlineData(3)>]
[<InlineData(9)>]
[<InlineData(10)>]
[<InlineData(0x0100)>]
[<InlineData(0x0200)>]
[<InlineData(0x0900)>]
[<InlineData(0xFFFF)>]
[<Trait("requirement", "JYRO-REQ-2091")>]
let ``JYRO-REQ-2091 a version above the reader is rejected with ArtifactVersionUnsupported`` (version: int) =
    // 0x0100 and 0x0200 are 256 and 512, the bytes 00 01 and 00 02: versions 1 and 2 written
    // big-endian. A reader of this specification reads 1 only (150.10.5), so 2 and above are above it.
    versionRefused (encode { artifactOf [ setOut "x" (num 1m) ] with Version = uint16 version })

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2091")>]
[<Trait("requirement", "JYRO-REQ-1921")>]
let ``JYRO-REQ-1921 version 1 loads, runs and is read`` () =
    // The version the reader reads is not refused: the artifact loads, runs, and Read gives 1.
    let bytes = build [ setOut "x" (num 1m) ]
    Assert.Equal<byte[]>([| 1uy; 0uy |], bytes.[4..5])
    Assert.Equal("{\"x\":1}", Values.toJson (load bytes |> ok))
    let viaThrow = Script.loadBytesOrThrow pinned (bytes)
    Assert.NotNull viaThrow
    Assert.Equal(1, (JyroArtifactInfo.Read bytes |> Script.valueOf).FormatVersion)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2123")>]
let ``JYRO-REQ-2123 version 0, which no encoding defines, is rejected`` () =
    // A real program with version 0: ArtifactVersionUnsupported at Deserialize (150.10.3, 160.1.12).
    versionRefused (encode { artifactOf [ setOut "x" (num 1m) ] with Version = 0us })

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2123")>]
let ``JYRO-REQ-2123 the book example, the version 0 header, is ArtifactVersionUnsupported on a load and on Read`` () =
    // The 44-byte header JYRX, version 0, reserved 0, a hash, 0 entries, then an empty program (00 00);
    // JyroArtifactInfo.Read gives the same code (150.10.3, 150.2.8 check 3).
    versionRefused (bookHeader 0us)

[<Theory>]
[<InlineData(0)>]
[<InlineData(2)>]
[<InlineData(9)>]
[<InlineData(65535)>]
[<Trait("requirement", "JYRO-REQ-2123")>]
let ``JYRO-REQ-2123 a version the reader cannot read, older or newer, is rejected with ArtifactVersionUnsupported`` (version: int) =
    // A reader of this specification reads 1 only (150.10.5): 9, the version of the development
    // builds before 1.0, is one it cannot read, as are 0 and the versions above 1. The 44-byte
    // header of the book example, with an empty program, is refused as the real program is.
    versionRefused (encode { artifactOf [ setOut "x" (num 1m) ] with Version = uint16 version })
    versionRefused (bookHeader (uint16 version))

[<Theory>]
[<InlineData(0)>]
[<InlineData(2)>]
[<InlineData(3)>]
[<InlineData(6)>]
[<InlineData(7)>]
[<InlineData(8)>]
[<InlineData(9)>]
[<InlineData(10)>]
[<InlineData(255)>]
[<InlineData(0x0100)>]
[<InlineData(0x0900)>]
[<InlineData(32767)>]
[<InlineData(32768)>]
[<InlineData(0xFFFF)>]
[<Trait("requirement", "JYRO-REQ-1921")>]
let ``JYRO-REQ-1921 any version other than 1 is rejected with ArtifactVersionUnsupported on every path`` (version: int) =
    versionRefused (encode { artifactOf [ setOut "x" (num 1m) ] with Version = uint16 version })

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1921")>]
let ``JYRO-REQ-1921 every version from 0 to 65535 except 1 is rejected by a load and by Read`` () =
    let program = [ setOut "x" (num 1m) ]
    let wrong = ResizeArray<string>()
    for v in 0 .. 0xFFFF do
        if v <> 1 then
            let bytes = encode { artifactOf program with Version = uint16 v }
            let load = loadOnly bytes
            let info = JyroArtifactInfo.Read bytes
            if Script.errorCodes load <> [ MessageCode.ArtifactVersionUnsupported ] then
                wrong.Add(sprintf "load of version %d: %A" v (Script.errorCodes load))
            if info.IsSuccess || Script.errorCodes info <> [ MessageCode.ArtifactVersionUnsupported ] then
                wrong.Add(sprintf "Read of version %d: %A" v (Script.errorCodes info))
            if wrong.Count > 20 then Values.fail (String.Join("\n", wrong))
    if wrong.Count > 0 then Values.fail (String.Join("\n", wrong))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1921")>]
let ``JYRO-REQ-1921 the book example, the version 9, 2 and 0 headers, are refused, and version 1 is not`` () =
    // The 44-byte header JYRX, version 9, reserved 0, a hash and 0 table entries, followed by an empty program.
    versionRefused (bookHeader 9us)
    versionRefused (bookHeader 2us)
    versionRefused (bookHeader 0us)
    // The same bytes with version 1 are not refused by the version check.
    let one = bookHeader 1us
    Assert.DoesNotContain(MessageCode.ArtifactVersionUnsupported, Script.codes (loadOnly one))
    let info = JyroArtifactInfo.Read one
    Assert.True(info.IsSuccess, Script.describe info)
    Assert.Equal(1, (Script.valueOf info).FormatVersion)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1921")>]
let ``JYRO-REQ-1921 a version 8 artifact of a union in the version 8 layout is ArtifactVersionUnsupported, not ArtifactUnreadable`` () =
    // The version-8 layout of a union variant: name, field count, fields, and no position.
    let v8Union =
        Array.concat
            [ [| 0x2Euy |]
              (let w = Writer(0x01uy) in w.Str "U"; w.U16 2; w.Str "A"; w.U16 0; w.Str "B"; w.U16 1; w.Str "x"; w.U8 0uy; w.Bytes())
              posBytes p0 ]
    let program = [ SRaw v8Union; setOut "x" (num 1m) ]
    let bytes = encode { artifactOf program with Version = 8us; Functions = [] }
    versionRefused bytes
    // The same bytes as version 1 are not of the layout of version 1 (a variant has a position, 150.8.5):
    // refused, but not for the version.
    let asOne = encode { artifactOf program with Version = 1us; Functions = [] }
    Assert.DoesNotContain(MessageCode.ArtifactVersionUnsupported, Script.codes (loadOnly asOne))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1921")>]
let ``JYRO-REQ-1921 a version other than 1 is refused before the table and the program are read`` () =
    // Row 4 precedes rows 5 and 6 (150.2.9); check 3 precedes checks 4 and 5 (150.2.8).
    for v in [ 8us; 2us ] do
        let count1001 = encode { artifactOf [] with Version = v; Functions = [ for i in 0 .. 1000 -> sprintf "F%04d" i ] }
        versionRefused count1001
    for v in [ 8us; 2us ] do
        let tag31 = encode { artifactOf [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ] with Version = v }
        refused MessageCode.ArtifactVersionUnsupported tag31
        stageOfCode MessageCode.ArtifactVersionUnsupported "Deserialize" (loadOnly tag31)

/// Sources whose written artifacts the 150.10.4 tests check; the last declares a union with
/// fields, so that the version 1 encoding of a variant is written.
let writtenSources : obj[] seq =
    [ "empty", ""
      "assignment", "Output.x = 1"
      "trim", "Output.x = Trim(\" a \")"
      "several-functions", "Output.a = Length([1, 2])\nOutput.b = ToUpper(\"x\")\nOutput.c = Trim(\" y \")"
      "union", "union U A B(x, y: number) end\nOutput.x = match B(1, 2) do case A then 0 case B(x, y) then y end" ]
    |> Seq.map (fun (n, src) -> [| box n; box src |])

[<Theory>]
[<MemberData(nameof writtenSources)>]
[<Trait("requirement", "JYRO-REQ-1920")>]
let ``JYRO-REQ-1920 the format version written is 1, as the little-endian bytes 01 00`` (name: string, source: string) =
    ignore name
    let bytes = Script.compileBytesWith pinned source |> Script.valueOf
    // Bytes 4 and 5, counted from 0: 01 00 (not 00 01, not 09 00).
    Assert.Equal<byte[]>([| 1uy; 0uy |], bytes.[4..5])
    Assert.Equal(1, (JyroArtifactInfo.Read bytes |> Script.valueOf).FormatVersion)
    // The writer's version is one the reader of this specification reads (150.10.5).
    Assert.DoesNotContain(MessageCode.ArtifactVersionUnsupported, Script.codes (loadOnly bytes))

// -- 150.11.x Artifact diagnostics ---------------------------------------------------------------------

let badMagic : obj[] seq =
    let valid = build [ setOut "x" (num 1m) ]
    [ "empty", [||]
      "J", "J"B
      "JY", "JY"B
      "JYR", "JYR"B
      "JYRY", Array.append "JYRY"B valid.[4..]
      "lowercase", Array.append "jyrx"B valid.[4..]
      "first-byte", Array.append [| 0x4Buy |] valid.[1..]
      "random", Array.init 100 (fun i -> byte ((i * 37 + 11) % 251))
      "bom", Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] valid ]
    |> Seq.map (fun (n, b) -> [| box n; box b |])

[<Theory>]
[<MemberData(nameof badMagic)>]
[<Trait("requirement", "JYRO-REQ-1571")>]
let ``FileNotStartingWithJYRX_IsReportedWithArtifactInvalidMagic`` (name: string, bytes: byte[]) =
    ignore name
    refused MessageCode.ArtifactInvalidMagic bytes
    fails MessageCode.ArtifactInvalidMagic (JyroArtifactInfo.Read bytes)

let undecodable : obj[] seq =
    let valid = build [ setOut "x" (call "Length" [ arr [ num 1m ] ]) ]
    let table = encode { artifactOf [ setOut "x" (num 1m) ] with FunctionCount = Some 3u }
    let badUtf8 = build [ setOut "x" (ELit(LRaw(0x04uy, [| 2uy; 0uy; 0xC3uy; 0x28uy |]), p0)) ]
    let listPastEnd = Array.append (build []).[0 .. 43] [| 5uy; 0uy; 0x2Buy |]
    let exactWith (i: int) (b: byte) =
        let e = Array.zeroCreate<byte> 16
        e.[0] <- 1uy
        e.[i] <- b
        build [ setOut "x" (ELit(LExactBytes e, p0)) ]
    let rec unaries n (e: Expr) = if n = 0 then e else unaries (n - 1) (EUn(Op.negate, e, p0))
    [ "header-43", valid.[0..42]
      "table-count-past-the-strings", table
      "program-truncated", valid.[0 .. valid.Length - 10]
      "list-past-the-end", listPastEnd
      // One row for each row of the table of 150.11.3.
      "150.2.2-hint-0x08", build [ SVar("a", Some 0x08uy, Some(num 1m), p0) ]
      "150.2.2-binary-0x0F", build [ setOut "a" (bin (num 1m) 0x0Fuy (num 2m)) ]
      "150.2.2-unary-0x02", build [ setOut "a" (EUn(0x02uy, num 1m, p0)) ]
      "150.2.2-assign-0x07", build [ var "a" (num 1m); SAssign(ident "a", 0x07uy, num 2m, None, p0) ]
      "150.2.2-for-direction-0x02", build [ SFor("i", num 0m, num 2m, None, 0x02uy, [], p0) ]
      "150.2.2-expression-0x00", build [ setOut "a" (ERaw(Array.append [| 0x00uy |] (posBytes p0))) ]
      "150.2.2-expression-0x0F", build [ setOut "a" (ERaw(Array.append [| 0x0Fuy |] (posBytes p0))) ]
      "150.3.5-duplicate-name", encode { artifactOf [] with Functions = [ "Trim"; "Trim" ] }
      "150.3.9-byte-after-program", Array.append valid [| 0uy |]
      "150.4.6-string-not-utf8", badUtf8
      "150.4.6-string-ED-A0-80", build [ setOut "x" (ELit(LRaw(0x04uy, [| 3uy; 0uy; 0xEDuy; 0xA0uy; 0x80uy |]), p0)) ]
      "150.6.5-literal-tag-0x05", build [ setOut "x" (ELit(LRaw(0x05uy, [| 0uy |]), p0)) ]
      "150.6.5-representation-0x02", build [ setOut "x" (ELit(LNumRaw(0x02uy, Array.zeroCreate 16), p0)) ]
      "150.6.5-exact-reserved-not-zero", exactWith 12 1uy
      "150.6.5-exact-scale-29", exactWith 14 29uy
      "150.6.5-exact-sign-0x01", exactWith 15 1uy
      "150.6.6-nan", build [ setOut "x" (inexact Double.NaN) ]
      "150.6.6-positive-infinity", build [ setOut "x" (inexact Double.PositiveInfinity) ]
      "150.6.6-negative-infinity", build [ setOut "x" (inexact Double.NegativeInfinity) ]
      "150.8.2-statement-0x1F", build [ SRaw(Array.append [| 0x1Fuy |] (posBytes p0)) ]
      "150.8.2-statement-0x31", build [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ]
      "150.9.3-list-10001", build [ setOut "x" (arr (List.replicate 10_001 (num 1m))) ]
      "150.9.4-table-1001", encode { artifactOf [] with Functions = [ for i in 0 .. 1000 -> sprintf "F%04d" i ] }
      "150.9.5-depth-257", build [ setOut "x" (unaries 255 (num 1m)) ]
      "150.9.6-object-count-10001", build [ setOut "x" (EObj([ for i in 0 .. 10_000 -> sprintf "k%d" i, num 1m ], p0)) ] ]
    |> Seq.map (fun (n, b) -> [| box n; box b |])

[<Theory>]
[<MemberData(nameof undecodable)>]
[<Trait("requirement", "JYRO-REQ-2092")>]
let ``ArtifactThatCannotBeDecoded_IsReportedWithArtifactUnreadable`` (name: string, bytes: byte[]) =
    ignore name
    // Judged by code alone: 150.11.3 is not about the stage (JYRO-ADR-105 item 4).
    refused MessageCode.ArtifactUnreadable bytes

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2092")>]
let ``Truncated_AtEveryLengthFrom4Up_IsArtifactUnreadable`` () =
    // Bytes that end before the structure ends are ArtifactUnreadable (150.11.3); a cut to
    // 4 bytes (JYRX) or 5 bytes (JYRX 08) too, since rows 4 and 5 do not fail on absent
    // bytes and row 6 refuses them (150.2.9).
    let valid = Script.compileBytesWith pinned "var s = \"ab\"\nOutput.x = Length([1, s])\nOutput.y = s & 1.5" |> Script.valueOf
    for n in 4 .. valid.Length - 1 do
        let r = loadOnly valid.[0 .. n - 1]
        if Script.errorCodes r <> [ MessageCode.ArtifactUnreadable ] || r.IsSuccess then
            Values.fail (sprintf "Cut to %d of %d bytes:\n%s" n valid.Length (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2092")>]
let ``TheBookExamples`` () =
    // First statement tag 0x31.
    refused MessageCode.ArtifactUnreadable (build [ SRaw(Array.append [| 0x31uy |] (posBytes p0)) ])
    // Last byte the tag 0x02 of a boolean literal, with no value byte after it.
    let stmt = Array.concat [ [| 0x21uy |]; exprBytes (out "x"); [| Op.assign; 0x01uy; 0x02uy |] ]
    let bytes = Array.concat [ (build []).[0 .. 43]; [| 1uy; 0uy |]; stmt ]
    Assert.Equal(0x02uy, bytes.[bytes.Length - 1])
    refused MessageCode.ArtifactUnreadable bytes

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2164")>]
let ``JYRO-REQ-2164 ProgramThatCannotBeEncoded_IsReportedWithArtifactCannotEncode`` () =
    cannotEncode Script.defaults (sprintf "Output.x = \"%s\"" (String('a', 70_000)))
    cannotEncode Script.defaults (sprintf "Output.x = \"%s\"" (String('a', 65_536)))
    cannotEncode Script.defaults ("Output.x = " + arrayLiteral 10_001)
    cannotEncode Script.defaults (sprintf "func %s()\n  return 1\nend" (String('f', 65_536)))

let private lib = "func Twice(x)\n  return x * 2\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2093")>]
let ``WritingAnArtifactWithLibraries_IsReportedWithArtifactCannotEncodeLibraries`` () =
    let parsed = JyroLibrary.Parse lib |> Script.valueOf
    let cases : (unit -> JyroResult<byte[]>) list =
        [ fun () -> JyroCompiler().WithStdlib().AddLibrarySource(lib).WithSource("Output.x = Twice(1)").CompileToArtifact()
          fun () -> JyroCompiler().WithStdlib().WithSource("Output.x = Twice(1)").AddLibrarySource(lib).CompileToArtifact()
          fun () -> JyroCompiler().WithStdlib().AddLibrary(parsed).WithSource("Output.x = Twice(1)").CompileToArtifact()
          fun () -> JyroCompiler().WithStdlib().WithSource("Output.x = Twice(1)").AddLibrary(parsed).CompileToArtifact() ]
    for c in cases do
        let r = c ()
        fails MessageCode.ArtifactCannotEncodeLibraries r
        Assert.False(r.HasValue)

// The former test of this place (loading an artifact into a compiler that has libraries is
// reported with ArtifactCannotUseLibraries) is dropped: JyroArtifactLoader has no member that
// adds a library script (JYRO-REQ-1896), so the combination cannot be written.

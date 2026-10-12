/// Part 150.1 and 150.2: the compiled artifact form and the defense against a
/// modified artifact. Hand-built artifacts come from the book's codec (Artifact).
module RealTree.Jyro.Spec.Tests.S150_01_Artifact

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact

/// Load an artifact and run it with stdlib on and every limit set explicitly (`pinned`, FLOORS 4.1.11).
let load (bytes: byte[]) : JyroResult<JyroValue> = Script.runBytesWith pinned bytes

/// Load only (JyroArtifactLoader(...).Load()), with a generous compilation limit.
let loadOnly (bytes: byte[]) : JyroResult<CompiledProgram> =
    Script.loadBytesWith pinned (bytes)

/// Assert that a load of `bytes` is refused with `code` on the Compile and CompileOrThrow paths.
let refused (code: MessageCode) (bytes: byte[]) : unit =
    fails code (loadOnly bytes)
    throwsWithCode code (fun () -> Script.loadBytesOrThrow pinned (bytes))

/// A source whose artifact is about `n` x 60 KB: `n` assignments of a 60,000-character string.
let bigSource (n: int) : string =
    let text = String('x', 60_000)
    lines [ for i in 0 .. n - 1 -> sprintf "Output.s%d = \"%s\"" i text ]

/// The four codes of an internal error that 150.2.5 names (from 130.1.4), each
/// with the number 150.2.5 gives it (160.7.12).
let internalErrors : (MessageCode * int) list =
    [ MessageCode.UnknownParserError, 2000
      MessageCode.RuntimeError, 5002
      MessageCode.UnknownCompilerError, 6000
      MessageCode.InternalStageFailure, 6002 ]

// -- 150.1.x -------------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2159")>]
let ``JYRO-REQ-2159 ArtifactForm_RoundTripsAndRuns`` () =
    let bytes = Script.compileBytesWith Script.defaults "Output.a = 1" |> Script.valueOf
    Assert.NotEmpty bytes
    Assert.Equal("{\"a\":1}", Values.toJson (load bytes |> ok))

/// Sources covering every expression node and statement node, and runtime errors.
let behaviorCorpus : obj[] seq =
    [ "Output.a = null\nOutput.b = true\nOutput.c = false\nOutput.d = 1.25\nOutput.e = 1e300\nOutput.f = \"s\""
      "var x = 3\nOutput.r = [x + 2, x - 2, x * 2, x / 2, x % 2, x == 3, x != 3, x < 4, x <= 3, x > 4, x >= 3]"
      "Output.r = [true and false, true or false, null ?? 5, \"a\" & 1, -3, not true]"
      "Output.r = 1 > 0 ? \"y\" : \"n\"\nOutput.s = Length([1, 2])\nOutput.t = Round(value: 1.25, decimals: 1)"
      "var o = {a: {b: [10, 20]}}\nOutput.r = o.a.b[1]\nOutput.s = o[\"a\"]"
      "Output.r = Map([1, 2], x => x * 10)\nOutput.s = Reduce([1, 2, 3], (a, b) => a + b, 0)"
      "Output.r = [1 is number, \"x\" is not string, null is null, [] is array, {} is object, true is boolean, (x => x) is function, 1 is any]"
      "var i = 5\nOutput.a = i++\nOutput.b = ++i\nOutput.c = i--\nOutput.d = --i"
      "union Shape Circle(r) Square(s: number) end\nOutput.r = match Circle(2) do case Circle(r) then r * 2 case Square(s) then s end"
      "var a\nvar b: number = 1\nvar c = \"x\"\nOutput.r = [a, b, c]"
      "var x = 1\nx += 2\nx -= 1\nx *= 6\nx /= 3\nx %= 3\nvar s = \"a\"\ns &= \"b\"\nOutput.r = [x, s]\nOutput.h: number = \"7\""
      "var r = []\nif false then r[0] = 1 elseif true then r[0] = 2 else r[0] = 3 end\nif false then r[1] = 9 end\nOutput.r = r"
      "var t = 0\nwhile t < 3 do t = t + 1 end\nforeach v in [1, 2] do t = t + v end\nOutput.t = t"
      "var u = []\nfor i in 0 to 3 do u[Length(u)] = i end\nfor j in 6 downto 0 by 2 do u[Length(u)] = j end\nOutput.u = u"
      "switch 2 do case 1 then Output.k = 1 case 2, 3 then Output.k = 2 default then Output.k = 0 end\nswitch 9 do case 1 then Output.m = 1 default then Output.m = 0 end"
      "func F(a: number, b = 2)\n  if a > 10 then\n    return\n  end\n  return a * b\nend\nOutput.r = [F(1), F(2, 3), F(20)]"
      "var n = 0\nwhile true do\n  n = n + 1\n  if n < 3 then continue end\n  break\nend\nOutput.n = n"
      "union U A(x) B end\nmatch A(5) do case A(x) then Output.x = x case B then Output.x = 0 end"
      "Output.o = {a: 1, b: 2}\ndelete Output.o.a\nOutput.e = 1\nexit \"bye\""
      "Output.a = 1\nfail \"bad\""
      "Output.a = 1\nOutput.b = 1 / 0"
      "Output.a = [1][5]"
      "Output.a = Length(true)" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<MemberData(nameof behaviorCorpus)>]
[<Trait("requirement", "JYRO-REQ-2058")>]
let ``UnchangedArtifact_BehavesAsTheProgramCompiledFromItsSource`` (source: string) =
    let fromSource = Script.run source
    let bytes = Script.compileBytesWith Script.defaults source
    Assert.True(bytes.IsSuccess, Script.describe bytes)
    let fromArtifact = load (Script.valueOf bytes)
    Assert.Equal(fromSource.IsSuccess, fromArtifact.IsSuccess)
    Assert.Equal(Script.value fromSource |> Option.map Values.toJson, Script.value fromArtifact |> Option.map Values.toJson)
    let diag (r: JyroResult<JyroValue>) =
        [ for m in r.Messages -> m.Code, String.Join("|", m.Args |> Array.map string), (if m.Location.HasValue then string m.Location.Value else "") ]
    Assert.Equal<(MessageCode * string * string) list>(diag fromSource, diag fromArtifact)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2059")>]
let ``LoadingAnArtifact_IsRecordedAsTheDeserializeStage`` () =
    let bytes = Script.compileBytesWith pinned (manyStatements 800) |> Script.valueOf
    let o, s = withStats pinned
    Script.runBytesWith o bytes |> ok |> ignore
    Assert.True(s.DeserializeTime > TimeSpan.Zero, "Deserialize is zero")
    Assert.Equal(TimeSpan.Zero, s.ParseTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2059")>]
let ``SourceCompile_DeserializeIsZero`` () =
    let o, s = withStats pinned
    Script.runWith o (manyStatements 800) |> ok |> ignore
    Assert.Equal(TimeSpan.Zero, s.DeserializeTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2059")>]
let ``LoadFailingInDeserialize_RecordsDeserializeAndNoLink`` () =
    // Rows 1 to 7 of 150.2.9 are Deserialize; a byte after the program (row 7) ends the
    // load there, so Link did not run and records 0 (150.1.3, 110.2.2).
    let bytes = Script.compileBytesWith pinned (bigSource 160) |> Script.valueOf
    Assert.True(bytes.Length > 9_000_000 && bytes.Length < 10_485_760, sprintf "artifact of %d bytes" bytes.Length)
    let o, s = withStats pinned
    let r = Script.loadBytesWith o (Array.append bytes [| 0uy |])
    fails MessageCode.ArtifactUnreadable r
    stageOfCode MessageCode.ArtifactUnreadable "Deserialize" r
    Assert.True(s.DeserializeTime > TimeSpan.Zero, "Deserialize is zero")
    Assert.Equal(TimeSpan.Zero, s.LinkTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2059")>]
let ``LoadFailingInLink_RecordsLinkTime`` () =
    // Rows 9 and 10 of 150.2.9 are Link: 1,000 names that no registered function has.
    let names = [ for i in 0 .. 999 -> sprintf "Nope%04d" i ]
    let bytes = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = names }
    let o, s = withStats pinned
    let r = Script.loadBytesWith o (bytes)
    Assert.False(r.IsSuccess)
    Assert.Equal<MessageCode list>(List.replicate 1000 MessageCode.UndefinedFunction, Script.errorCodes r)
    stageOfCode MessageCode.UndefinedFunction "Link" r
    Assert.True(s.DeserializeTime > TimeSpan.Zero, "Deserialize is zero")
    Assert.True(s.LinkTime > TimeSpan.Zero, "Link is zero for 1,000 names")

// -- 150.2.1 Reader limits before allocation -------------------------------------------------------

let private header (count: uint32) : byte[] =
    let a = artifactOf []
    let w = Writer(1uy)
    w.Raw a.Magic
    w.U16 1
    w.U16 0
    w.Raw a.Hash
    w.U32 count
    w.Bytes()

let hugeClaims : obj[] seq =
    [ "table-count", Array.append (header 0xFFFFFFFFu) [||]
      "table-count-with-a-name", Array.concat [ header 0xFFFFFFFFu; [| 3uy; 0uy |]; "abc"B ]
      "string-length", Array.concat [ header 1u; [| 0xFFuy; 0xFFuy |]; "abc"B ]
      "program-list", Array.concat [ header 0u; [| 0xFFuy; 0xFFuy |] ]
      "nested-list", Array.concat [ header 0u; [| 1uy; 0uy; 0x2Buy; 0x0Auy; 0xFFuy; 0xFFuy |] ]
      "nested-string", Array.concat [ header 0u; [| 1uy; 0uy; 0x2Buy; 0x01uy; 0x04uy; 0xFFuy; 0xFFuy; 0x41uy |] ] ]
    |> Seq.map (fun (n, b) -> [| box n; box b |])

[<Theory>]
[<MemberData(nameof hugeClaims)>]
[<Trait("requirement", "JYRO-REQ-2161")>]
let ``JYRO-REQ-2161 ShortArtifactClaimingAHugeSize_IsRejectedWithoutAllocatingInProportion`` (name: string, bytes: byte[]) =
    ignore name
    let baseline = build [ setOut "a" (num 1m) ]
    let c0 = Script.loaderFor Script.defaults baseline
    let _, baseBytes = allocating (fun () -> c0.Load())
    let c = Script.loaderFor Script.defaults bytes
    let r, allocated = allocating (fun () -> c.Load())
    Assert.False(r.IsSuccess)
    Assert.True(allocated < baseBytes + oneMB, sprintf "The load allocated %d bytes (a valid load: %d)" allocated baseBytes)
    // JyroArtifactInfo.Read is not a load (150.2.7); its bound is verified under 150.2.8.
    throwsWithCode (Script.onlyError r) (fun () -> Script.loadBytesOrThrow Script.defaults (bytes))

/// The one number `c` of JYRO-REQ-2161, fixed here for every artifact below: a load may
/// allocate no more than `c` x (`F` + 1) bytes. It is generous (64 KiB for each byte), so that
/// the fixed cost of a load of a few bytes is inside it; a load that allocates in proportion to
/// a claimed count of 4,294,967,295 or 10,000 entries is far outside it.
let private allocationFactor = 65_536L

/// Artifacts of the same length that differ only in a count that they claim. Each is padded to
/// `length` bytes with bytes that no claim reads as an item.
let private sameLengthClaims : (string * byte[]) list =
    let pad (n: int) (b: byte[]) = Array.append b (Array.create (n - b.Length) 0xFFuy)
    let length = 64
    [ for claim in [ 1u; 2u; 4_294_967_295u ] ->
        sprintf "function-table-claiming-%d" claim, pad length (header claim)
      for claim in [ 1; 10_000 ] ->
        sprintf "program-list-claiming-%d" claim,
        pad length (Array.concat [ header 0u; BitConverter.GetBytes(uint16 claim) ]) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2161")>]
let ``JYRO-REQ-2161 SameLengthArtifacts_DifferingOnlyInAClaimedCount_AreEachRefusedWithinTheSameBound`` () =
    let load (bytes: byte[]) = Script.loaderFor Script.defaults bytes
    // Warm up, so that a first use of the reader is not counted.
    for _, bytes in sameLengthClaims do (load bytes).Load() |> ignore
    for name, bytes in sameLengthClaims do
        let c = load bytes
        let r, allocated = allocating (fun () -> c.Load())
        let bound = allocationFactor * (int64 bytes.Length + 1L)
        Assert.False(r.IsSuccess, sprintf "%s: the load succeeded" name)
        Assert.True(allocated <= bound, sprintf "%s: a load of %d bytes allocated %d bytes, over %d x %d" name bytes.Length allocated allocationFactor (bytes.Length + 1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2161")>]
let ``JYRO-REQ-2161 SameLengthArtifacts_DifferingOnlyInAClaimedCount_AllocateAboutTheSame`` () =
    let load (bytes: byte[]) = Script.loaderFor Script.defaults bytes
    for _, bytes in sameLengthClaims do (load bytes).Load() |> ignore
    let spent =
        [ for name, bytes in sameLengthClaims ->
            let c = load bytes
            name, snd (allocating (fun () -> c.Load())) ]
    let least = spent |> List.map snd |> List.min
    for name, allocated in spent do
        Assert.True(allocated <= least + oneMB, sprintf "%s allocated %d bytes, and the least of its family %d" name allocated least)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2161")>]
let ``JYRO-REQ-2161 ValidArtifact_Load_AllocatesNoMoreThanTheBound`` () =
    let bytes = build [ setOut "a" (num 1m); setOut "b" (str "text") ]
    let c = Script.loaderFor Script.defaults bytes
    c.Load() |> ignore
    let r, allocated = allocating (fun () -> c.Load())
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(allocated <= allocationFactor * (int64 bytes.Length + 1L), sprintf "a load of %d bytes allocated %d bytes" bytes.Length allocated)

// -- 150.2.2 Unassigned tags are rejected ---------------------------------------------------------------

let private withVar (s: Stmt) = [ var "a" (num 1m); s ]

let unassignedTags : obj[] seq =
    [ for h in [ 0x08uy; 0x10uy; 0x7Fuy; 0xFFuy ] do
        yield "hint", [ SVar("a", Some h, Some(num 1m), p0) ]
        yield "typecheck-hint", [ setOut "a" (ETypeCheck(num 1m, h, 0uy, p0)) ]
      for op in [ 0x0Fuy; 0x10uy; 0x80uy; 0xFFuy ] do
        yield "binary", [ setOut "a" (bin (num 1m) op (num 2m)) ]
      for op in [ 0x02uy; 0x7Fuy; 0xFFuy ] do
        yield "unary", [ setOut "a" (EUn(op, num 1m, p0)) ]
      for op in [ 0x07uy; 0x20uy; 0xFFuy ] do
        yield "assign", withVar (SAssign(ident "a", op, num 2m, None, p0))
      for d in [ 0x02uy; 0x7Fuy; 0xFFuy ] do
        yield "for-direction", [ SFor("i", num 0m, num 2m, None, d, [], p0) ]
      for t in [ 0x00uy; 0x05uy; 0x7Fuy; 0xFFuy ] do
        yield "literal", [ setOut "a" (ELit(LRaw(t, [| 0uy |]), p0)) ]
      for r in [ 0x02uy; 0x7Fuy; 0xFFuy ] do
        yield "representation", [ setOut "a" (ELit(LNumRaw(r, Array.zeroCreate 16), p0)) ]
      for t in [ 0x00uy; 0x0Fuy; 0x10uy; 0x20uy; 0xFFuy ] do
        yield "expression", [ setOut "a" (ERaw(Array.append [| t |] (posBytes p0))) ]
      for t in [ 0x00uy; 0x01uy; 0x1Fuy; 0x31uy; 0x80uy; 0xFFuy ] do
        yield "statement", [ SRaw(Array.append [| t |] (posBytes p0)) ] ]
    |> Seq.map (fun (k, prog) -> [| box k; box (build prog) |])

[<Theory>]
[<MemberData(nameof unassignedTags)>]
[<Trait("requirement", "JYRO-REQ-2062")>]
let ``UnassignedTag_IsRejected_AsUnreadable_AndNeverRuns`` (kind: string, bytes: byte[]) =
    ignore kind
    refused MessageCode.ArtifactUnreadable bytes

// -- 150.2.3 Every table name resolves ----------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2063")>]
let ``TableNameThatTheLoadingHostDidNotRegister_IsRejectedWithUndefinedFunction`` () =
    let h = Host.recorder "H"
    let bytes = Script.compileBytesWith { Script.defaults with Functions = [ h.AsFunction ] } "H(1)" |> Script.valueOf
    Script.runBytesWith Script.defaults bytes |> fails MessageCode.UndefinedFunction
    let lower = Host.recorder "h"
    Script.runBytesWith { Script.defaults with Functions = [ lower.AsFunction ] } bytes |> fails MessageCode.UndefinedFunction
    throwsWithCode MessageCode.UndefinedFunction (fun () -> Script.loadBytesOrThrow Script.defaults (bytes))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2063")>]
let ``StandardFunctionInTheTable_WithoutTheStandardLibrary_IsRejected`` () =
    let bytes = Script.compileBytesWith Script.defaults "Output.a = Length([1])" |> Script.valueOf
    Script.runBytesWith { Script.defaults with Stdlib = false } bytes |> fails MessageCode.UndefinedFunction

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2063")>]
let ``TableNameNoCallUses_ThatDoesNotResolve_IsRejected`` () =
    let a = artifactOf [ setOut "a" (num 1m) ]
    let bytes = encode { a with Functions = [ "Nope" ] }
    refused MessageCode.UndefinedFunction bytes

// -- 150.2.5 / 150.2.6 A construct validation rejects gets its own diagnostic ------------------------------

let private fn (name: string) (ps: (string * byte option * Expr option) list) (body: Stmt list) = SFunc(name, ps, body, p0)

/// Constructs validation rejects: the program, the code, and the equivalent source ("" when none).
let invalidConstructs : (string * Stmt list * MessageCode * string) list =
    [ "break", [ SBreak p0 ], MessageCode.LoopStatementOutsideOfLoop, "break"
      "continue", [ SContinue p0 ], MessageCode.LoopStatementOutsideOfLoop, "continue"
      "return", [ SReturn(Some(num 1m), p0) ], MessageCode.ReturnOutsideFunction, "return 1"
      "nested-func", [ fn "F" [] [ fn "G" [] [ SReturn(None, p0) ]; SReturn(None, p0) ] ], MessageCode.NestedFunctionDeclaration, "func F()\n  func G()\n    return\n  end\n  return\nend"
      "func-in-block", [ SIf(tru, [ fn "G" [] [ SReturn(None, p0) ] ], [], None, p0) ], MessageCode.FunctionNotAtTopLevel, "if true then\n  func G()\n    return\n  end\nend"
      "union-in-func", [ fn "F" [] [ unionOf "U" [ "A", [] ]; SReturn(None, p0) ] ], MessageCode.UnionInsideFunction, "func F()\n  union U A end\n  return\nend"
      "union-in-block", [ SIf(tru, [ unionOf "U" [ "A", [] ] ], [], None, p0) ], MessageCode.UnionNotAtTopLevel, "if true then\n  union U A end\nend"
      "assign-input", [ assign (ident "Input") (num 1m) ], MessageCode.InputIsReadOnly, "Input = 1"
      "literal-target", [ assign (num 1m) (num 2m) ], MessageCode.InvalidAssignmentTarget, "1 = 2"
      "delete-variable", [ var "x" (num 1m); SDelete(ident "x", p0) ], MessageCode.InvalidDeleteTarget, "var x = 1\ndelete x"
      "increment-literal", [ SExpr(EIncDec(num 1m, 1uy, 1uy, p0), p0) ], MessageCode.InvalidIncrementTarget, "++1"
      "output-in-func", [ fn "F" [] [ setOut "a" (num 1m); SReturn(None, p0) ] ], MessageCode.OutputAccessInFunction, "func F()\n  Output.a = 1\n  return\nend"
      "hint-on-compound", [ var "a" (num 1m); SAssign(ident "a", Op.addAssign, num 1m, Some Hint.number, p0) ], MessageCode.TypeHintOnCompoundAssignment, "var a = 1\na: number += 1"
      "typed-no-init", [ SVar("a", Some Hint.number, None, p0) ], MessageCode.TypedVariableNeedsInitializer, "var a: number"
      "default-not-literal", [ fn "F" [ "x", None, Some(bin (num 1m) Op.add (num 2m)) ] [ SReturn(None, p0) ] ], MessageCode.DefaultValueNotLiteral, "func F(x = 1 + 2)\n  return\nend"
      "duplicate-param", [ fn "F" [ "x", None, None; "x", None, None ] [ SReturn(None, p0) ] ], MessageCode.DuplicateParameterName, "func F(x, x)\n  return\nend"
      "required-after-optional", [ fn "F" [ "x", None, Some(num 1m); "y", None, None ] [ SReturn(None, p0) ] ], MessageCode.RequiredParamAfterOptional, "func F(x = 1, y)\n  return\nend"
      "reserved-lambda-param", [ setOut "a" (call "Map" [ arr [ num 1m ]; ELambda([ "Input" ], num 1m, p0) ]) ], MessageCode.ReservedName, "Output.a = Map([1], Input => 1)"
      "undeclared", [ setOut "a" (ident "zzz") ], MessageCode.UndeclaredVariable, "Output.a = zzz"
      "redeclared", [ var "a" (num 1m); var "a" (num 2m) ], MessageCode.VariableAlreadyDeclared, "var a = 1\nvar a = 2"
      "foreach-null", [ SForEach("x", nul, [], p0) ], MessageCode.NotIterableLiteral, "foreach x in null do end"
      "match-default", [ unionOf "U" [ "A", []; "B", [] ]; SMatch(call "A" [], [ "A", [], [], p0 ], p0) ], MessageCode.MatchNotExhaustive, "union U A B end\nmatch A() do case A then end" ]

let invalidConstructRows : obj[] seq = invalidConstructs |> Seq.map (fun (n, _, _, _) -> [| box n |])

let private findConstruct n = invalidConstructs |> List.find (fun (k, _, _, _) -> k = n)

[<Theory>]
[<MemberData(nameof invalidConstructRows)>]
[<Trait("requirement", "JYRO-REQ-2064")>]
let ``LoadedProgramWithAConstructValidationRejects_ReportsThatRule_AndNoInternalError`` (name: string) =
    // Loaded with JyroArtifactLoader and, when that succeeds, run: the later stage
    // that refuses it may be Link, Compile or Execute, so both results are judged.
    let _, program, code, _ = findConstruct name
    let r = load (build program)
    Assert.False(r.IsSuccess, sprintf "%s ran" name)
    // The rule's code first (FLOORS 4.1.11).
    if not (List.contains code (Script.errorCodes r)) then
        Values.fail (sprintf "%s: expected the error %A (%d). Diagnostics:\n%s" name code (int code) (Script.describe r))
    // Then no diagnostic of any severity of the four internal codes, by name or by number.
    for d in r.Messages do
        for (c, n) in internalErrors do
            Assert.False(d.Code = c || int d.Code = n, sprintf "%s: an internal error %A (%d). Diagnostics:\n%s" name d.Code (int d.Code) (Script.describe r))

[<Theory>]
[<MemberData(nameof invalidConstructRows)>]
[<Trait("requirement", "JYRO-REQ-1516")>]
let ``ValidationOnLoad_GivesTheSameDiagnosticAsTheEquivalentSource`` (name: string) =
    let _, program, code, source = findConstruct name
    let fromArtifact = load (build program)
    let fromSource = Script.compile source
    let argsOf (r: JyroResult<'T>) =
        r.Messages |> Seq.filter (fun m -> m.Code = code) |> Seq.map (fun m -> String.Join("|", m.Args |> Array.map string)) |> List.ofSeq
    Assert.Contains(code, Script.errorCodes fromSource)
    // This implementation runs Validate on load (JYRO-ADR-114, 115 and 116 decide the
    // declaration-name checks "on source and on load"), so the load must raise the code
    // itself: a load that raises nothing is no parity (JYRO-DEF-507's escape).
    if not (List.contains code (Script.errorCodes fromArtifact)) then
        Values.fail (sprintf "%s: the load did not raise %A, which the equivalent source raises. Diagnostics of the load:\n%s" name code (Script.describe fromArtifact))
    Assert.Equal<string list>(argsOf fromSource, argsOf fromArtifact)

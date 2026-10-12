/// 150.4.4 (a position locates a node's own source text) and 150.6.3 / 150.6.7
/// (an exact number is written with the smallest scale that holds its value),
/// read from CompileToArtifact() with the book's codec.
module RealTree.Jyro.Spec.Tests.S150_04_PositionsAndScale

open System
open System.Numerics
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

// -- Walking a decoded program ---------------------------------------------------------------

/// A node of a decoded program: a label, its position, and the nodes it holds.
type Node = { Label: string; Pos: Pos; Children: Node list }

let rec private exprNode (e: Expr) : Node option =
    let mk label p kids = Some { Label = label; Pos = p; Children = List.choose id kids }
    match e with
    | ELit(l, p) ->
        let label = match l with LExact _ -> "Literal:exact" | LStr _ -> "Literal:string" | _ -> "Literal"
        mk label p []
    | EId(n, p) -> mk ("Identifier:" + n) p []
    | EBin(l, _, r, p) -> mk "Binary" p [ exprNode l; exprNode r ]
    | EUn(_, x, p) -> mk "Unary" p [ exprNode x ]
    | ETern(a, b, c, p) -> mk "Ternary" p [ exprNode a; exprNode b; exprNode c ]
    | ECall(n, Positional xs, p) -> mk ("Call:" + n) p (List.map exprNode xs)
    | ECall(n, Named(_, ps), p) -> mk ("Call:" + n) p (ps |> List.map (snd >> exprNode))
    | EProp(t, n, p) -> mk ("Property:" + n) p [ exprNode t ]
    | EIdx(t, i, p) -> mk "Index" p [ exprNode t; exprNode i ]
    | EObj(ps, p) -> mk "Object" p (ps |> List.map (snd >> exprNode))
    | EArr(xs, p) -> mk "Array" p (List.map exprNode xs)
    | ELambda(_, b, p) -> mk "Lambda" p [ exprNode b ]
    | ETypeCheck(x, _, _, p) -> mk "TypeCheck" p [ exprNode x ]
    | EIncDec(x, _, _, p) -> mk "IncDec" p [ exprNode x ]
    | EMatch(x, cases, p) ->
        mk "MatchExpr" p (exprNode x :: [ for (_, _, b, cp) in cases -> Some { Label = "MatchExprCase"; Pos = cp; Children = List.choose id [ exprNode b ] } ])
    | ERaw _ -> None

and private stmtNode (s: Stmt) : Node option =
    let mk label p kids = Some { Label = label; Pos = p; Children = List.choose id kids }
    let block b = List.map stmtNode b
    let opt f o = match o with Some x -> [ f x ] | None -> []
    match s with
    | SVar(n, _, i, p) -> mk ("VarDecl:" + n) p (opt exprNode i)
    | SAssign(t, _, v, _, p) -> mk "Assignment" p [ exprNode t; exprNode v ]
    | SIf(c, t, eis, e, p) ->
        mk "If" p ([ exprNode c ] @ block t @ (eis |> List.collect (fun (ec, eb) -> exprNode ec :: block eb)) @ (match e with Some b -> block b | None -> []))
    | SWhile(c, b, p) -> mk "While" p (exprNode c :: block b)
    | SForEach(_, c, b, p) -> mk "ForEach" p (exprNode c :: block b)
    | SFor(_, a, b, st, _, body, p) -> mk "For" p ([ exprNode a; exprNode b ] @ opt exprNode st @ block body)
    | SSwitch(x, cases, d, p) ->
        mk "Switch" p (exprNode x :: (cases |> List.collect (fun (vs, b) -> List.map exprNode vs @ block b)) @ (match d with Some b -> block b | None -> []))
    | SReturn(v, p) -> mk "Return" p (opt exprNode v)
    | SFail(v, p) -> mk "Fail" p (opt exprNode v)
    | SExit(v, p) -> mk "Exit" p (opt exprNode v)
    | SBreak p -> mk "Break" p []
    | SContinue p -> mk "Continue" p []
    | SExpr(e, p) -> mk "ExprStmt" p [ exprNode e ]
    | SFunc(_, ps, b, p) -> mk "Func" p ((ps |> List.collect (fun (_, _, d) -> opt exprNode d)) @ block b)
    | SUnion(_, _, p) -> mk "Union" p []
    | SMatch(x, cases, p) ->
        mk "MatchStmt" p (exprNode x :: [ for (_, _, b, cp) in cases -> Some { Label = "MatchCase"; Pos = cp; Children = List.choose id (block b) } ])
    | SDelete(t, p) -> mk "Delete" p [ exprNode t ]
    | SRaw _ -> None

let rec private flatten (n: Node) : Node list = n :: List.collect flatten n.Children

/// Every node of the program `source` compiles to, top-level statements first.
let private nodesOf (source: string) : Node list * Node list =
    let _, a = writtenBy Script.defaults source
    let tops = a.Program |> List.choose stmtNode
    tops, List.collect flatten tops

/// Line and column (both from 1) of the UTF-16 offset `index`, counting LF and
/// CRLF as one line end each (20.2.2).
let private lineColumnOf (source: string) (index: int) : int * int =
    let mutable line = 1
    let mutable lineStart = 0
    let mutable i = 0
    while i < index do
        if source.[i] = '\n' then
            line <- line + 1
            lineStart <- i + 1
        i <- i + 1
    line, index - lineStart + 1

let private show (n: Node) = sprintf "%s %A" n.Label n.Pos

// -- 150.4.4 The worked example ------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_TheBookWorkedExample_IsWrittenExactly`` () =
    let source = "var x = 1\nOutput.y = x"
    let _, a = writtenBy Script.defaults source
    let expect (line, col, start, stop) (p: Pos) (what: string) =
        Assert.True(
            (p = { Line = line; Column = col; Start = start; End = stop }),
            sprintf "%s: expected %d/%d/%d/%d, got %A" what line col start stop p)
    match a.Program with
    | [ SVar("x", _, Some(ELit(_, pLit)), pVar); SAssign(EProp(EId("Output", pOut), "y", pProp), _, EId("x", pX), _, pAssign) ] ->
        expect (1, 1, 0, 9) pVar "VarDecl"
        expect (1, 9, 8, 9) pLit "Literal"
        expect (2, 1, 10, 22) pAssign "Assignment"
        expect (2, 1, 10, 18) pProp "PropertyAccess"
        expect (2, 12, 21, 22) pX "Identifier x"
        expect (2, 1, 10, 16) pOut "Identifier Output"
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_WithCrlf_LineCountsTheCrlfOnce_AndTheIndexCountsBothCodeUnits`` () =
    let _, a = writtenBy Script.defaults "var x = 1\r\nOutput.y = x"
    match a.Program with
    | [ _; SAssign(EProp(_, _, pProp), _, EId("x", pX), _, pAssign) ] ->
        Assert.Equal({ Line = 2; Column = 1; Start = 11; End = 23 }, pAssign)
        Assert.Equal({ Line = 2; Column = 1; Start = 11; End = 19 }, pProp)
        Assert.Equal({ Line = 2; Column = 12; Start = 22; End = 23 }, pX)
    | other -> Values.fail (sprintf "unexpected program %A" other)

// The leading U+FEFF case of 150.4.4 (column 2, start index 1) is JYRO-REQ-20's test, in
// S20_02_SourceEncoding.fs, under JYRO-ADR-122 item 4 (JYRO-DEF-532): it is not tested here.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_ExcludesWhitespaceAndCommentsAroundTheNode`` () =
    let _, a = writtenBy Script.defaults "  var x = 1 # c\n# a line\n\tOutput.y = x   # t"
    match a.Program with
    | [ SVar(_, _, _, pVar); SAssign(_, _, _, _, pAssign) ] ->
        Assert.Equal({ Line = 1; Column = 3; Start = 2; End = 11 }, pVar)
        Assert.Equal({ Line = 3; Column = 2; Start = 26; End = 38 }, pAssign)
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_OfANodeCoversTheNodesItHolds`` () =
    let _, a = writtenBy Script.defaults "Output.y = 1 + 22"
    match a.Program with
    | [ SAssign(_, _, EBin(ELit(_, pl), _, ELit(_, pr), pb), _, pa) ] ->
        Assert.Equal({ Line = 1; Column = 1; Start = 0; End = 17 }, pa)
        Assert.Equal({ Line = 1; Column = 12; Start = 11; End = 17 }, pb)
        Assert.Equal({ Line = 1; Column = 12; Start = 11; End = 12 }, pl)
        Assert.Equal({ Line = 1; Column = 16; Start = 15; End = 17 }, pr)
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_CountsASurrogatePairAsTwoCodeUnits`` () =
    // "Output.a = [" is 12 code units; the string literal is `"`, a pair, `"`.
    let source = "Output.a = [\"\U0001F600\", 7]"
    let _, a = writtenBy Script.defaults source
    match a.Program with
    | [ SAssign(_, _, EArr([ ELit(_, ps); ELit(_, p7) ], pArr), _, pa) ] ->
        Assert.Equal({ Line = 1; Column = 13; Start = 12; End = 16 }, ps)
        Assert.Equal({ Line = 1; Column = 19; Start = 18; End = 19 }, p7)
        Assert.Equal({ Line = 1; Column = 12; Start = 11; End = 20 }, pArr)
        Assert.Equal({ Line = 1; Column = 1; Start = 0; End = 20 }, pa)
    | other -> Values.fail (sprintf "unexpected program %A" other)

// -- 150.4.4 Grouped expressions and the parentheses of other nodes ------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_TheBookSecondExample_GroupedExpression`` () =
    let _, a = writtenBy pinned "Output.y = (1 + 22) * 3"
    match a.Program with
    | [ SAssign(_, _, EBin(EBin(ELit(_, p1), _, ELit(_, p22), pPlus), _, ELit(_, p3), pTimes), _, pa) ] ->
        Assert.Equal({ Line = 1; Column = 1; Start = 0; End = 23 }, pa)
        Assert.Equal({ Line = 1; Column = 12; Start = 11; End = 23 }, pTimes)
        Assert.Equal({ Line = 1; Column = 13; Start = 12; End = 18 }, pPlus)
        Assert.Equal({ Line = 1; Column = 13; Start = 12; End = 13 }, p1)
        Assert.Equal({ Line = 1; Column = 17; Start = 16; End = 18 }, p22)
        Assert.Equal({ Line = 1; Column = 23; Start = 22; End = 23 }, p3)
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_NestedGroupedExpression_ExcludesEveryParenthesis`` () =
    // "Output.y = ((1))": the literal is the `1` at index 13 alone; the Assignment,
    // which holds the grouped expression, ends after the last `)` (150.4.4).
    let _, a = writtenBy pinned "Output.y = ((1))"
    match a.Program with
    | [ SAssign(_, _, ELit(_, pLit), _, pa) ] ->
        Assert.Equal({ Line = 1; Column = 14; Start = 13; End = 14 }, pLit)
        Assert.Equal({ Line = 1; Column = 1; Start = 0; End = 16 }, pa)
    | other -> Values.fail (sprintf "unexpected program %A" other)
    let _, b = writtenBy pinned "var x = 1\nOutput.y = (x)"
    match b.Program with
    | [ _; SAssign(_, _, EId("x", px), _, pa) ] ->
        Assert.Equal({ Line = 2; Column = 13; Start = 22; End = 23 }, px)
        Assert.Equal({ Line = 2; Column = 1; Start = 10; End = 24 }, pa)
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``Position_ParenthesesOfCallLambdaFuncDefAndCase_BelongToThatNode`` () =
    // Call: "Length([1])" at 11 to 22.
    let _, a = writtenBy pinned "Output.y = Length([1])"
    match a.Program with
    | [ SAssign(_, _, ECall("Length", _, pc), _, _) ] -> Assert.Equal({ Line = 1; Column = 12; Start = 11; End = 22 }, pc)
    | other -> Values.fail (sprintf "unexpected program %A" other)
    // Lambda: "(a) => a" at 20 to 28, inside the Call "Map([1], (a) => a)" at 11 to 29.
    let _, b = writtenBy pinned "Output.y = Map([1], (a) => a)"
    match b.Program with
    | [ SAssign(_, _, ECall("Map", Positional [ _; ELambda(_, _, pl) ], pc), _, _) ] ->
        Assert.Equal({ Line = 1; Column = 21; Start = 20; End = 28 }, pl)
        Assert.Equal({ Line = 1; Column = 12; Start = 11; End = 29 }, pc)
    | other -> Values.fail (sprintf "unexpected program %A" other)
    // FuncDef: "func F(a)\n  return a\nend" is one node from `func` to `end`.
    let src = "func F(a)\n  return a\nend"
    let _, c = writtenBy pinned src
    match c.Program with
    | [ SFunc(_, _, _, pf) ] -> Assert.Equal({ Line = 1; Column = 1; Start = 0; End = src.Length }, pf)
    | other -> Values.fail (sprintf "unexpected program %A" other)
    // Match case: its bindings' parentheses are tokens of the case, which starts at `case`.
    let msrc = "union U A(x) end\nvar u = A(1)\nmatch u do case A(x) then Output.v = x end"
    let _, d = writtenBy pinned msrc
    match d.Program |> List.last with
    | SMatch(_, [ (_, _, _, pcase) ], _) ->
        let text = msrc.Substring(pcase.Start, pcase.End - pcase.Start)
        Assert.StartsWith("case A(x)", text)
    | other -> Values.fail (sprintf "unexpected statement %A" other)

// -- 150.4.4 Every node of a corpus ---------------------------------------------------------------

let positionCorpus : obj[] seq =
    let extra =
        [ "  var x = 1 # c\n# a line\n\tOutput.y = x   # t"
          "var x = 1\r\nvar y = [x,\r\n  x + 1]\r\nOutput.y = y"
          "Output.a = \"\U0001F600\" & 1\nOutput.b = {k: [1, 2], j: \"\u00E9\"}"
          "func F(a, b = 2)\n  if a > 1 then\n    return a * b\n  elseif a < 0 then\n    return -a\n  end\n  return F(a: a + 1, b: b)\nend\nOutput.r = F(0)"
          "var i = 0\nwhile i < 3 do i++ end\nOutput.i = --i\nOutput.t = i is number ? Map([i], v => v * 2) : null" ]
    Seq.append (behaviorCorpus |> Seq.map (fun r -> r.[0] :?> string)) (Seq.ofList (determinismCorpus @ extra))
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<MemberData(nameof positionCorpus)>]
[<Trait("requirement", "JYRO-REQ-1532")>]
let ``EveryNode_ItsPositionIsItsOwnTextByLineColumnAndIndexes`` (source: string) =
    let _, nodes = nodesOf source
    for n in nodes do
        let p = n.Pos
        Assert.True(0 <= p.Start && p.Start < p.End && p.End <= source.Length, sprintf "%s is outside the source (length %d)" (show n) source.Length)
        let text = source.Substring(p.Start, p.End - p.Start)
        Assert.True(text.Trim().Length = text.Length && not (text.StartsWith "#"), sprintf "%s has the text %A, with whitespace or a comment at an end" (show n) text)
        Assert.Equal((p.Line, p.Column), lineColumnOf source p.Start)
        for c in n.Children do
            Assert.True(c.Pos.Start >= p.Start && c.Pos.End <= p.End, sprintf "%s is not inside %s" (show c) (show n))

// -- 150.6.3 / 150.6.7 One encoding for each exact value -----------------------------------------------

/// The significand, scale and sign of a decimal as the codec read it.
let private parts (d: decimal) : BigInteger * int * bool =
    let bits = Decimal.GetBits d
    let lo = BigInteger(uint32 bits.[0])
    let mid = BigInteger(uint32 bits.[1])
    let hi = BigInteger(uint32 bits.[2])
    lo + (mid <<< 32) + (hi <<< 64), (bits.[3] >>> 16) &&& 0xFF, bits.[3] < 0

let private exactLiteralOf (source: string) : decimal =
    let _, a = writtenBy Script.defaults source
    match a.Program with
    | [ SAssign(_, _, ELit(LExact v, _), _, _) ] -> v
    | other -> Values.fail (sprintf "unexpected program %A" other)

[<Theory>]
[<InlineData("1.5", "15", 1)>]
[<InlineData("1.50", "15", 1)>]
[<InlineData("100", "100", 0)>]
[<InlineData("100.00", "100", 0)>]
[<InlineData("2.500", "25", 1)>]
[<InlineData("10", "10", 0)>]
[<InlineData("1.10", "11", 1)>]
[<InlineData("0.10", "1", 1)>]
[<InlineData("0.0000000000000000000000000001", "1", 28)>]
[<InlineData("0.00000000000000000000000000010", "1", 28)>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335", 0)>]
[<InlineData("123.4500", "12345", 2)>]
[<Trait("requirement", "JYRO-REQ-1547")>]
let ``ExactLiteral_IsWrittenWithTheSmallestScaleThatHoldsIt`` (literal: string, significand: string, scale: int) =
    let s, sc, negative = parts (exactLiteralOf ("Output.x = " + literal))
    Assert.Equal(BigInteger.Parse significand, s)
    Assert.Equal(scale, sc)
    Assert.False(negative)

[<Theory>]
[<InlineData("0")>]
[<InlineData("0.0")>]
[<InlineData("0.000")>]
[<InlineData("0.0000000000000000000000000000")>]
[<Trait("requirement", "JYRO-REQ-1547")>]
let ``Zero_IsWrittenWithSignificand0Scale0AndSign00`` (literal: string) =
    let s, sc, negative = parts (exactLiteralOf ("Output.x = " + literal))
    Assert.Equal(BigInteger.Zero, s)
    Assert.Equal(0, sc)
    Assert.False(negative, "sign byte 0x80")

[<Theory>]
[<MemberData(nameof positionCorpus)>]
[<Trait("requirement", "JYRO-REQ-1547")>]
let ``EveryExactLiteralWritten_HasASignificandThatIsAMultipleOf10OnlyAtScale0`` (source: string) =
    let _, a = writtenBy Script.defaults source
    let rec lits (e: Expr) : decimal list =
        match e with
        | ELit(LExact d, _) -> [ d ]
        | ELit _ | EId _ | ERaw _ -> []
        | EBin(l, _, r, _) -> lits l @ lits r
        | EUn(_, x, _) | ETypeCheck(x, _, _, _) | EIncDec(x, _, _, _) | EProp(x, _, _) | ELambda(_, x, _) -> lits x
        | ETern(a, b, c, _) -> lits a @ lits b @ lits c
        | ECall(_, Positional xs, _) | EArr(xs, _) -> List.collect lits xs
        | ECall(_, Named(_, ps), _) | EObj(ps, _) -> ps |> List.collect (snd >> lits)
        | EIdx(t, i, _) -> lits t @ lits i
        | EMatch(x, cases, _) -> lits x @ (cases |> List.collect (fun (_, _, b, _) -> lits b))
    let rec slits (s: Stmt) : decimal list =
        let o = function Some e -> lits e | None -> []
        let b = List.collect slits
        match s with
        | SVar(_, _, i, _) -> o i
        | SAssign(t, _, v, _, _) -> lits t @ lits v
        | SIf(c, t, eis, e, _) -> lits c @ b t @ (eis |> List.collect (fun (ec, eb) -> lits ec @ b eb)) @ (match e with Some x -> b x | None -> [])
        | SWhile(c, x, _) | SForEach(_, c, x, _) -> lits c @ b x
        | SFor(_, x, y, st, _, body, _) -> lits x @ lits y @ o st @ b body
        | SSwitch(x, cases, d, _) -> lits x @ (cases |> List.collect (fun (vs, bb) -> List.collect lits vs @ b bb)) @ (match d with Some y -> b y | None -> [])
        | SReturn(v, _) | SFail(v, _) | SExit(v, _) -> o v
        | SExpr(e, _) | SDelete(e, _) -> lits e
        | SFunc(_, ps, body, _) -> (ps |> List.collect (fun (_, _, d) -> o d)) @ b body
        | SMatch(x, cases, _) -> lits x @ (cases |> List.collect (fun (_, _, bb, _) -> b bb))
        | SBreak _ | SContinue _ | SUnion _ | SRaw _ -> []
    for d in a.Program |> List.collect slits do
        let s, sc, negative = parts d
        if sc > 0 then Assert.True(s % BigInteger(10) <> BigInteger.Zero, sprintf "%M is written with scale %d" d sc)
        if s.IsZero then
            Assert.Equal(0, sc)
            Assert.False(negative)

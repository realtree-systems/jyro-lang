namespace RealTree.Jyro.Parsing

open RealTree.Jyro
open RealTree.Jyro.Ast

open RealTree.Jyro.Parsing.Lexer
open RealTree.Jyro.Parsing.StatementParser

module internal Parser =
    open FParsec

    let private pProgram : Parser<Program, unit> =
        initParser >>. parseStmts .>> eof
        |>> fun stmts -> { Statements = stmts }

    /// The furthest position among an error and the errors of the branches
    /// the parser tried and abandoned before it: the point at which the
    /// parser could read no further (spec 130.1.2). FParsec reports the
    /// position of the last alternative it tried, which after a backtrack is
    /// the start of the statement, not where the text went wrong.
    let rec private furthest (pos: FParsec.Position) (msgs: ErrorMessageList) : FParsec.Position =
        let mutable best = pos
        let mutable list = msgs
        while not (isNull list) do
            let candidate =
                match list.Head with
                | :? ErrorMessage.NestedError as nested -> Some (furthest nested.Position nested.Messages)
                | :? ErrorMessage.CompoundError as compound when not (isNull compound.NestedErrorPosition) ->
                    Some (furthest compound.NestedErrorPosition compound.NestedErrorMessages)
                | _ -> None
            match candidate with
            | Some p when p.Index > best.Index -> best <- p
            | _ -> ()
            list <- list.Tail
        best

    /// The diagnostic at the range from `start` to `stop` of `source`.
    let private diagnostic (source: string) (code: MessageCode) (args: obj[]) (start: int) (stop: int) =
        let line, column = SourceScan.lineColumn source start
        let location = SourceLocation(line, column, max 0 (stop - start))
        // Every diagnostic the parser makes has the stage Parse, given here
        // and not by each caller (JYRO-ADR-111): a lexical error such as an
        // unpaired surrogate has a code of the runtime band, whose own
        // stage is Execute, and is still a parse-time rejection.
        DiagnosticMessage.Of(code, args = args, location = System.Nullable location).WithStage(MessageStage.Parse)

    /// The diagnostic of a parse that failed with `msg` at `err`: a syntax
    /// error at the token where the parser could read no further, or, for a
    /// lexical error the scan did not find, the code the lexer tagged it
    /// with. Its arguments are values, not the parser's own text (spec
    /// 130.6.2): the token it stopped at, or none, with the code
    /// `UnexpectedEndOfSource`, where the source ended first.
    ///
    /// `source` is the script as given, never the repaired copy the parser
    /// ran on: the copy has a closing quote appended after an unterminated
    /// string and each token with a lexical error blanked, so its text at
    /// those places is not the script's. `err` is in the copy's coordinates,
    /// which equal the script's everywhere inside it; an index past the end
    /// of the script (the appended quote) is the end of the script (spec
    /// 130.1.2).
    let private syntaxDiagnostic (source: string) (msg: string) (err: ParserError) =
        let at = furthest err.Position err.Messages
        let index = min (int at.Index) source.Length
        let length = SourceScan.tokenLength source index
        let token = if length = 0 then null else source.Substring(index, length)
        // The token, the one argument, or none where the source ended before
        // the parser could read further: nothing stands for the end of the
        // source (spec 130.6.2).
        let tokenArgs : obj[] = if isNull token then [||] else [| box token |]
        let code, args =
            // Recognize the lexical errors the lexer tags via marker
            // substrings. `SourceScan` finds every one of them before the
            // parse, so these arms are reached only for a token it reads
            // differently from the lexer.
            if msg.Contains(UnterminatedStringMarker) then
                MessageCode.UnterminatedString, ([||]: obj[])
            elif msg.Contains(NumberOutOfRangeMarker) || msg.Contains(NumberNotFiniteMarker)
                 || msg.Contains(NumberMalformedMarker) then
                MessageCode.InvalidNumberFormat, tokenArgs
            elif msg.Contains(UnpairedSurrogateMarker) then
                MessageCode.UnpairedSurrogate, ([| box 0 |]: obj[])
            elif msg.Contains(UnknownEscapeMarker) then
                MessageCode.InvalidEscapeSequence, tokenArgs
            elif msg.Contains(CallTargetNotNameMarker) then
                // A `(` after an expression that is not a bare name
                // (JYRO-REQ-1798): the place is the `(`, with no argument.
                MessageCode.CallTargetNotName, ([||]: obj[])
            elif isNull token then
                // The place is the empty range after the last code unit: the
                // source ended first (spec 130.1.2, JYRO-REQ-1674).
                MessageCode.UnexpectedEndOfSource, ([||]: obj[])
            else
                MessageCode.UnknownParserError, tokenArgs
        diagnostic source code args index (index + length)

    /// The first token, by its index, of the first node or parenthesized
    /// expression of a parsed source text whose nesting depth (spec
    /// 100.10.2) is above the maximum (100.10.1), in the order of the
    /// source text and, of two that begin at one token, the outer
    /// (JYRO-REQ-1718); or -1 when there is none.
    ///
    /// The parser's own count is a lower bound (`SourceNesting`), short of
    /// the depth inside a chain of operators; this count is exact. It visits
    /// each node before what it holds, and what it holds in the order of the
    /// text, so the first node it finds above the maximum is the first in
    /// the text and the outermost at its token. It never goes deeper than
    /// one level past the maximum, so its own recursion is bounded however
    /// deep the tree is.
    module private SourceDepth =
        let private maximum = Ast.MaxNestingDepth

        let private firstSome (items: 'a list) (f: 'a -> int) : int =
            let mutable rest = items
            let mutable found = -1
            while found < 0 && not rest.IsEmpty do
                found <- f rest.Head
                rest <- rest.Tail
            found

        let private orElse (next: unit -> int) (found: int) = if found >= 0 then found else next ()

        /// `e`, held in a place at depth `depth`: its parentheses, from the
        /// outermost in, each one deeper, then the node itself.
        let rec expr (depth: int) (e: Expr) : int =
            let parens = SourceNesting.ParensOf e
            let count = if isNull parens then 0 else parens.Count
            let mutable found = -1
            let mutable i = 0
            // The outermost parenthesis is the last recorded.
            while found < 0 && i < count do
                if depth + i > maximum then found <- parens.[count - 1 - i].StartIndex
                i <- i + 1
            if found >= 0 then found
            else
                let d = depth + count
                if d > maximum then e.Position.StartIndex
                else
                    let child = expr (d + 1)
                    match e with
                    | Literal _
                    | Identifier _ -> -1
                    | Binary(l, _, r, _) -> child l |> orElse (fun () -> child r)
                    | Unary(_, operand, _) -> child operand
                    | Ternary(c, t, f, _) -> child c |> orElse (fun () -> child t) |> orElse (fun () -> child f)
                    | Call(_, PositionalArgs args, _) -> firstSome args child
                    | Call(_, NamedArgs args, _) -> firstSome args (snd >> child)
                    | PropertyAccess(target, _, _) -> child target
                    | IndexAccess(target, index, _) -> child target |> orElse (fun () -> child index)
                    | ObjectLiteral(props, _) -> firstSome props (snd >> child)
                    | ArrayLiteral(elements, _) -> firstSome elements child
                    | Lambda(_, body, _) -> child body
                    | TypeCheck(operand, _, _, _) -> child operand
                    | IncrementDecrement(operand, _, _, _) -> child operand
                    | MatchExpr(subject, cases, _) ->
                        child subject |> orElse (fun () -> firstSome cases (fun c -> child c.Body))

        /// `s`, at depth `depth`.
        let rec stmt (depth: int) (s: Stmt) : int =
            if depth > maximum then s.Position.StartIndex
            else
                let e = expr (depth + 1)
                let opt = function Some x -> e x | None -> -1
                let block (body: Stmt list) = firstSome body (stmt (depth + 1))
                match s with
                | VarDecl(_, _, init, _) -> opt init
                | Assignment(target, _, value, _, _) -> e target |> orElse (fun () -> e value)
                | If(cond, thenBlock, elseIfs, elseBlock, _) ->
                    e cond
                    |> orElse (fun () -> block thenBlock)
                    |> orElse (fun () -> firstSome elseIfs (fun (c, b) -> e c |> orElse (fun () -> block b)))
                    |> orElse (fun () -> match elseBlock with Some b -> block b | None -> -1)
                | While(cond, body, _) -> e cond |> orElse (fun () -> block body)
                | ForEach(_, collection, body, _) -> e collection |> orElse (fun () -> block body)
                | For(_, start, finish, step, _, body, _) ->
                    e start |> orElse (fun () -> e finish) |> orElse (fun () -> opt step) |> orElse (fun () -> block body)
                | Switch(subject, cases, defaultCase, _) ->
                    e subject
                    |> orElse (fun () -> firstSome cases (fun c -> firstSome c.Values e |> orElse (fun () -> block c.Body)))
                    |> orElse (fun () -> match defaultCase with Some b -> block b | None -> -1)
                | Return(value, _)
                | Fail(value, _)
                | Exit(value, _) -> opt value
                | FuncDef(_, parameters, body, _) ->
                    firstSome parameters (fun p -> opt p.DefaultValue) |> orElse (fun () -> block body)
                | Break _
                | Continue _
                | UnionDef _ -> -1
                | ExprStmt(value, _) -> e value
                | Match(subject, cases, _) -> e subject |> orElse (fun () -> firstSome cases (fun c -> block c.Body))
                | Delete(target, _) -> e target

        let program (p: Program) : int = firstSome p.Statements (stmt 1)

    /// Runs `parser` on `source`, giving every parse error (spec 130.3.2):
    /// each lexical error of the source, which a scan finds token by token,
    /// and the syntax error at which the parser stops, which it finds in the
    /// source with each token that has a lexical error replaced by one of the
    /// same extent that has none. The diagnostics are in the order of their
    /// positions (spec 130.5.1).
    ///
    /// A source text nested deeper than the maximum (spec 100.10) fails with
    /// one `SourceNestingLimitExceeded`, whose one argument is the maximum,
    /// in place of the syntax error (JYRO-REQ-1718, JYRO-ADR-178). Its place
    /// is the first token of the first node or parenthesized expression whose
    /// reading count is above the maximum, which is where the parser stops
    /// (`Lexer.nestedAs`, `SourceNesting`); when no reading count is above
    /// it, the parser reads the whole text, and the place is the first node
    /// above the maximum by its exact depth (`SourceDepth`). A lexical error
    /// is still reported beside it.
    let private run (parser: Parser<'T, unit>) (depthOf: 'T -> int) (name: string) (source: string) : JyroResult<'T> =
        LastToken.Reset()
        SourceNesting.Reset()
        let lexical = SourceScan.lexicalErrors source
        let text = SourceScan.repair source lexical
        SourceLines.Set text
        let parsed =
            try runParserOnString parser () name text
            finally SourceLines.Clear()
        LastToken.Reset()
        let tooDeep =
            // The parser stopped at the first node whose reading count is
            // above the maximum: that node is the place.
            if SourceNesting.Exceeded >= 0L then int SourceNesting.Exceeded
            else
                match parsed with
                | ParserResult.Success(value, _, _) -> depthOf value
                | ParserResult.Failure _ -> -1
        SourceNesting.Clear()
        let lexicalDiagnostics =
            lexical |> List.map (fun e -> e.Start, diagnostic source e.Code e.Args e.Start e.End)
        let inOrder (first: int * DiagnosticMessage) =
            first :: lexicalDiagnostics
            |> List.sortBy fst
            |> List.map snd
        match parsed with
        | _ when tooDeep >= 0 ->
            let index = min tooDeep source.Length
            let nesting =
                diagnostic source MessageCode.SourceNestingLimitExceeded [| box Ast.MaxNestingDepth |]
                    index (index + SourceScan.tokenLength source index)
            JyroResult<'T>.Failure(inOrder (index, nesting))
        | ParserResult.Success(value, _, _) when lexical.IsEmpty ->
            JyroResult<'T>.Success(value)
        | ParserResult.Success _ ->
            JyroResult<'T>.Failure(lexicalDiagnostics |> List.map snd)
        | ParserResult.Failure(msg, err, _) ->
            let syntax = syntaxDiagnostic source msg err
            let syntaxIndex = min (int (furthest err.Position err.Messages).Index) source.Length
            let all =
                (syntaxIndex, syntax) :: lexicalDiagnostics
                |> List.sortBy fst
                |> List.map snd
            JyroResult<'T>.Failure(all)

    /// Parses a Jyro script.
    ///
    /// This module used to expose two entry points for one operation: `parse`,
    /// returning a bespoke `JyroParseResult` union carrying a raw string and no
    /// `MessageCode`, and `parseToResult`, returning the `JyroResult` every
    /// other stage in the pipeline returns. Production only ever called the
    /// second; the first existed for test convenience, and its union type
    /// shipped in the public API as one of four different ways this library
    /// reported failure. The convenience shape now lives in the test projects
    /// as an active pattern, which is where scaffolding belongs.
    let parse (source: string) : JyroResult<Program> =
        run pProgram SourceDepth.program "script" source

    /// Parses a single expression. Returns the same shape as `parse` and every
    /// other stage; it previously returned an F# `Result<Expr, string>`, which
    /// was the fourth distinct failure shape in the public API and the only one
    /// that discarded the code and the position entirely.
    let parseExpression (source: string) : JyroResult<Expr> =
        run (initParser >>. ExpressionParser.parseExpr .>> eof) (SourceDepth.expr 1) "expression" source

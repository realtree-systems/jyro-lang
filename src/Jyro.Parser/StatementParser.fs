namespace RealTree.Jyro.Parsing

open RealTree.Jyro
open RealTree.Jyro.Ast

open FParsec.Primitives
open FParsec.CharParsers
open RealTree.Jyro.Parsing.Lexer
open RealTree.Jyro.Parsing.PositionTracking
open RealTree.Jyro.Parsing.ExpressionParser

module internal StatementParser =
    // Forward reference for recursive statement parsing
    let pStmt, pStmtRef = createParserForwardedToRef<Stmt, unit>()

    let pBlock = many pStmt

    /// White space that stays on the line: any White_Space code point (20.1,
    /// JYRO-REQ-13) that does not begin a line end, an `LF` or a `CRLF`
    /// (20.1.2, 20.2.2). A `CR` that no `LF` follows is whitespace
    /// (JYRO-REQ-1814). Read code unit by code unit, since FParsec's own
    /// char parsers read a `CR` alone as a line end.
    let private hws : Parser<unit, unit> =
        fun stream ->
            let mutable go = true
            while go && not stream.IsEndOfStream do
                let c = stream.Peek()
                if c = '\n' || (c = '\r' && stream.Peek2().Char1 = '\n') then go <- false
                elif Unicode15.isWhiteSpace (int c) then stream.Skip()
                else go <- false
            FParsec.Reply(())

    /// Succeeds, consuming nothing, when a line end, an `LF` or a `CRLF`,
    /// begins at the stream's position (20.2.2).
    let private lineEndAhead : Parser<unit, unit> =
        fun stream ->
            let c = stream.Peek()
            if not stream.IsEndOfStream && (c = '\n' || (c = '\r' && stream.Peek2().Char1 = '\n')) then FParsec.Reply(())
            else FParsec.Reply(FParsec.ReplyStatus.Error, FParsec.ErrorMessageList(FParsec.ErrorMessage.Expected("a line end")))

    /// Parse a keyword without consuming newline-crossing whitespace
    let private keywordRaw kw : Parser<unit, unit> =
        attempt (rawToken (pstring kw .>> notFollowedBy identifierCharAhead)) >>% ()

    /// A keyword that closes or divides the enclosing block. After `return`,
    /// `fail` or `exit` it means "no value", so `if a then return end` works
    /// on one line. Only looked at, never consumed: the block parser reads it.
    let private blockBoundary : Parser<unit, unit> =
        let peek kw =
            attempt (pstring kw .>> notFollowedBy identifierCharAhead) >>% ()
        choice (List.map peek [ "end"; "elseif"; "else"; "case"; "default" ])

    /// Parse an optional expression that must start on the same line as the preceding keyword
    let private optSameLineExpr : Parser<Expr option, unit> =
        afterToken hws >>. (
            (followedBy (lineEndAhead <|> (pchar '#' >>% ()) <|> eof) >>. afterToken ws >>% None)
            <|>
            (followedBy blockBoundary >>% None)
            <|>
            (parseExpr |>> Some)
        )

    // Variable declaration: var name [: type] [= expr]
    let private pVarDecl =
        withPos (
            keyword "var" >>. identifier .>>.
            opt (colon >>. typeKeyword) .>>.
            opt (assignEq >>. parseExpr)
        )
        |>> fun (((name, typeHint), init), pos) -> VarDecl(name, typeHint, init, pos)

    // Assignment statement: target [: type] op= value
    //
    // The parser does not verify that `target` is a valid assignment
    // target (identifier / property access / index access) - that check
    // lives in the compiler layer, consistent with how other semantic
    // rules (nested func/union, delete target shape, type hint on
    // compound op) are enforced.
    let private pAssignment =
        withPos (
            // Commit once the target, optional type hint, and assignment
            // operator are seen. The right-hand side is then OUTSIDE the
            // backtracking region, so a committing failure within it (e.g. an
            // unterminated string) propagates with its specific diagnostic
            // instead of being unwound back to an expression statement.
            attempt (parseExpr .>>. opt (colon >>. typeKeyword) .>>. assignOp) .>>. parseExpr
        )
        |>> fun ((((target, typeHint), op), value), pos) ->
            Assignment(target, op, value, typeHint, pos)

    let private pElseIf =
        keyword "elseif" >>. parseExpr .>> keyword "then" .>>. pBlock

    let private pElse =
        keyword "else" >>. pBlock

    let private pIf =
        withPos (
            keyword "if" >>. parseExpr .>> keyword "then" .>>.
            pBlock .>>.
            many pElseIf .>>.
            opt pElse .>>
            keyword "end"
        )
        |>> fun ((((cond, thenBlock), elseIfs), elseBlock), pos) ->
            If(cond, thenBlock, elseIfs, elseBlock, pos)

    let private pWhile =
        withPos (
            keyword "while" >>. parseExpr .>> keyword "do" .>>.
            pBlock .>>
            keyword "end"
        )
        |>> fun ((cond, body), pos) -> While(cond, body, pos)

    // Range-based for loop: for varName in startExpr to|downto endExpr [by stepExpr] do ... end
    let private pFor =
        withPos (
            keyword "for" >>. identifier .>> keyword "in" .>>. parseExpr .>>.
            ((keyword "downto" >>% Descending) <|> (keyword "to" >>% Ascending)) .>>.
            parseExpr .>>.
            opt (keyword "by" >>. parseExpr) .>>
            keyword "do" .>>.
            pBlock .>>
            keyword "end"
        )
        |>> fun ((((((varName, startExpr), direction), endExpr), stepExpr), body), pos) ->
            For(varName, startExpr, endExpr, stepExpr, direction, body, pos)

    let private pForEach =
        withPos (
            keyword "foreach" >>. identifier .>> keyword "in" .>>. parseExpr .>> keyword "do" .>>.
            pBlock .>>
            keyword "end"
        )
        |>> fun (((varName, collection), body), pos) -> ForEach(varName, collection, body, pos)

    // Switch statement
    let private pCaseValues = sepBy1 parseExpr comma

    let private pCase =
        keyword "case" >>. pCaseValues .>> keyword "then" .>>. pBlock
        |>> fun (values, body) -> { Values = values; Body = body }

    let private pDefaultCase =
        keyword "default" >>. keyword "then" >>. pBlock

    let private pSwitch =
        withPos (
            keyword "switch" >>. parseExpr .>> keyword "do" .>>.
            many pCase .>>.
            opt pDefaultCase .>>
            keyword "end"
        )
        |>> fun (((expr, cases), defaultCase), pos) -> Switch(expr, cases, defaultCase, pos)

    // Function definition: func Name(param1, param2: type, param3 = default) ... end
    //
    // A default is any expression (JYRO-ADR-2), and validation refuses one
    // that is not a literal or `-` before a number literal (spec 80.4.5).
    // A default in parentheses is read with the range of its text, which
    // then starts or ends outside its node's (spec 150.4.4): the node is
    // marked, so validation can refuse `(1)` though its node is a literal.
    let private pDefaultValue : Parser<Expr, unit> =
        pExprSpanned
        |>> fun (e, textPos) ->
            let nodePos = e.Position
            if textPos.StartIndex < nodePos.StartIndex || textPos.EndIndex > nodePos.EndIndex then
                Ast.markGroupedDefault e
            e

    let private pFuncParam : Parser<FuncParam, unit> =
        identifier .>>. opt (colon >>. typeKeyword) .>>. opt (assignEq >>. pDefaultValue)
        |>> fun ((name, typeHint), defaultValue) ->
            { Name = name; TypeHint = typeHint; DefaultValue = defaultValue }

    let private pFuncParams =
        between lparen rparen (sepBy pFuncParam comma)

    let private pFuncDef =
        withPos (
            keyword "func" >>. identifier .>>. pFuncParams .>>.
            pBlock .>>
            keyword "end"
        )
        |>> fun (((name, parameters), body), pos) ->
            FuncDef(name, parameters, body, pos)

    // Union declaration: union Name Variant1(field1: type, ...) Variant2(...) end
    let private pVariantField =
        identifier .>>. opt (colon >>. typeKeyword)

    let private pVariantFields =
        between lparen rparen (sepBy pVariantField comma)

    let private pVariant =
        withPos (identifier .>>. (opt (attempt pVariantFields) |>> Option.defaultValue []))
        |>> fun ((name, fields), pos) -> { Name = name; Fields = fields; Pos = pos }

    let private pUnionDef =
        withPos (
            keyword "union" >>. identifier .>>.
            many1 (attempt pVariant) .>>
            keyword "end"
        )
        |>> fun ((name, variants), pos) -> UnionDef(name, variants, pos)

    // Match statement: match expr do case Variant(bindings) then block ... end
    let private pMatchBindings =
        between lparen rparen (sepBy identifier comma)

    /// `match_case` of 20.1.1: a case names an `identifier`, which `default`
    /// is not, so `case default` and `default then` in a match statement are
    /// parse errors (50.9.2).
    let private pMatchCase =
        withPos (
            keyword "case" >>. identifier .>>.
            (opt (attempt pMatchBindings) |>> Option.defaultValue []) .>>
            keyword "then" .>>.
            pBlock
        )
        |>> fun (((variantName, bindings), body), pos) ->
            { VariantName = variantName; Bindings = bindings; Body = body; Pos = pos }

    let private pMatch =
        withPos (
            keyword "match" >>. parseExpr .>> keyword "do" .>>.
            many1 pMatchCase .>>
            keyword "end"
        )
        |>> fun ((expr, cases), pos) -> Match(expr, cases, pos)

    let private pReturn =
        withPos (keywordRaw "return" >>. optSameLineExpr)
        |>> fun (value, pos) -> Return(value, pos)

    let private pExit =
        withPos (keywordRaw "exit" >>. optSameLineExpr)
        |>> fun (value, pos) -> Exit(value, pos)

    let private pFail =
        withPos (keywordRaw "fail" >>. optSameLineExpr)
        |>> fun (message, pos) -> Fail(message, pos)

    let private pBreak =
        withPos (keyword "break")
        |>> fun (_, pos) -> Break(pos)

    let private pContinue =
        withPos (keyword "continue")
        |>> fun (_, pos) -> Continue(pos)

    // Delete statement (removes a property from an object)
    let private pDelete =
        withPos (keyword "delete" >>. parseExpr)
        |>> fun (target, pos) -> Delete(target, pos)

    // Expression statement (expression evaluated for side effects)
    let private pExprStmt =
        withPos parseExpr
        |>> fun (expr, pos) -> ExprStmt(expr, pos)

    /// Stops an abandoned compilation before each statement, nested ones
    /// included (JYRO-ADR-100). Consumes nothing and always succeeds.
    let private checkCanceled : Parser<unit, unit> =
        fun _ ->
            CompilationStage.CheckAmbient()
            FParsec.Reply(())

    // A statement is one level deeper than the statement whose body holds
    // it, and a top-level statement has depth 1 (spec 100.10.2).
    do pStmtRef.Value <-
        nestedAs true (checkCanceled >>.
        choice [
            attempt pFuncDef
            attempt pUnionDef
            attempt pVarDecl
            attempt pIf
            attempt pWhile
            attempt pFor
            attempt pForEach
            attempt pSwitch
            // Not `attempt`: a statement that starts with `match` is a match
            // statement and never an expression statement (20.1.4), so once
            // the keyword is read the statement is `match_stmt` or an error.
            pMatch
            attempt pReturn
            attempt pExit
            attempt pFail
            attempt pBreak
            attempt pContinue
            attempt pDelete
            pAssignment
            attempt pExprStmt
        ])

    let parseStmt = pStmt

    let parseStmts = pBlock

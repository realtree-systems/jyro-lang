namespace RealTree.Jyro.Parsing

open RealTree.Jyro
open RealTree.Jyro.Ast

open FParsec.Primitives
open FParsec.CharParsers
open RealTree.Jyro.Parsing.Lexer
open RealTree.Jyro.Parsing.PositionTracking

/// Expression parser for the Jyro language using manual precedence climbing
module internal ExpressionParser =
    // Forward reference for recursive expression parsing. The parsers of
    // this module return an expression with the range of the text it was
    // read from, which for a grouped expression includes its parentheses: the
    // node inside them does not (spec 150.4.4), but a node that holds a
    // grouped expression starts or ends at them.
    let pExprAtDepth, pExprRef = createParserForwardedToRef<Expr * Position, unit>()

    /// An expression one level deeper than what holds it (spec 100.10.2):
    /// every place that reads a whole expression - an operand of a ternary,
    /// an argument, an element, a property value, an index, the body of a
    /// lambda or a match case, the inside of parentheses, and each child
    /// expression of a statement - is a child of the node or parenthesized
    /// expression around it.
    let pExprSpanned : Parser<Expr * Position, unit> = nested pExprAtDepth

    let pExpr : Parser<Expr, unit> = pExprSpanned |>> fst

    let private getPos' : Parser<Position, unit> = getPos

    // Primary expressions (literals, identifiers, grouped expressions)
    let private pNullLiteral : Parser<Expr, unit> =
        withPos nullLiteral |>> fun (_, pos) -> Literal(JyroNull.Instance, pos)

    let private pBoolLiteral : Parser<Expr, unit> =
        withPos booleanLiteral |>> fun (b, pos) -> Literal(JyroBoolean.FromBoolean(b), pos)

    let private pNumberLiteral : Parser<Expr, unit> =
        withPos numberLiteral |>> fun (n, pos) -> Literal(n :> JyroValue, pos)

    let private pStringLiteral : Parser<Expr, unit> =
        withPos anyStringLiteral |>> fun (s, pos) -> Literal(JyroString(s), pos)

    let private pIdentifier : Parser<Expr, unit> =
        withPos identifier |>> fun (name, pos) -> Identifier(name, pos)

    /// A parenthesized expression. It has no node (spec 150.4.4), but it is
    /// one level of nesting, and the expression inside it one more (spec
    /// 100.10.2): it takes the depth of the place it is read at, so the
    /// expression inside is read one deeper, and the parentheses are
    /// recorded by the node inside them for the exact count.
    let private pGroupedExpr : Parser<Expr, unit> =
        getPos .>>. between lparen rparen pExpr
        |>> fun (pos, e) ->
            SourceNesting.AddParens(e, pos)
            e

    let private pArrayLiteral : Parser<Expr, unit> =
        withPos (between lbracket rbracket (sepBy pExpr comma))
        |>> fun (elements, pos) -> ArrayLiteral(elements, pos)

    let private pObjectProperty : Parser<string * Expr, unit> =
        (anyIdentifier <|> anyStringLiteral) .>> colon .>>. pExpr

    let private pObjectLiteral : Parser<Expr, unit> =
        withPos (between lbrace rbrace (sepBy pObjectProperty comma))
        |>> fun (props, pos) -> ObjectLiteral(props, pos)

    let private pLambdaParams : Parser<string list, unit> =
        (between lparen rparen (sepBy identifier comma)) <|>
        (identifier |>> fun p -> [p])

    /// A lambda. Only its parameters and `=>` decide that a lambda begins,
    /// so only they are tried and backtracked over; the body is read
    /// committed, so an error in it, a `CallTargetNotName` among them
    /// (JYRO-DEF-786), is the error of the parse and is not unwound into a
    /// reading of the parameter as a name.
    let private pLambda : Parser<Expr, unit> =
        withPos (attempt (pLambdaParams .>> arrow) .>>. pExpr)
        |>> fun ((params', body), pos) -> Lambda(params', body, pos)

    // Match expression: match <expr> do case <Variant>[(bindings)] then <expr> ... end
    /// `match_expr_case` of 20.1.1. A case names any word, so `case default`
    /// parses, as a case the validator refuses with `MatchWithDefault`
    /// (50.9.2); `default then` has no `case` and is a parse error.
    let private pMatchExprCase : Parser<MatchExprCase, unit> =
        withPos (
            keyword "case" >>. anyIdentifier .>>.
            (opt (attempt (between lparen rparen (sepBy identifier comma))) |>> Option.defaultValue []) .>>
            keyword "then" .>>.
            pExpr
        )
        |>> fun (((variantName, bindings), body), pos) ->
            { VariantName = variantName; Bindings = bindings; Body = body; Pos = pos }

    let private pMatchExpr : Parser<Expr, unit> =
        withPos (
            keyword "match" >>. pExpr .>> keyword "do" .>>.
            many1 pMatchExprCase .>>
            keyword "end"
        )
        |>> fun ((expr, cases), pos) -> MatchExpr(expr, cases, pos)

    let private pPrimary : Parser<Expr, unit> =
        choice [
            pLambda
            pNullLiteral
            pBoolLiteral
            attempt pNumberLiteral
            pStringLiteral
            pArrayLiteral
            pObjectLiteral
            pMatchExpr
            pGroupedExpr
            pIdentifier
        ]

    // Postfix expressions (calls, property access, indexing, increment/decrement)
    type private PostfixOp =
        | CallOp of CallArgs * Position
        | DotOp of string * Position
        | IndexOp of Expr * Position
        | PostIncOp of Position
        | PostDecOp of Position

    /// Parse a single named argument: identifier: expr
    /// Uses anyIdentifier since parameter names can be keywords (e.g., "array", "number")
    let private pNamedArg : Parser<string * Expr, unit> =
        attempt (anyIdentifier .>> colon) .>>. pExpr

    /// The arguments of a call. A first argument that begins `name :` makes
    /// them named, and only that is looked ahead at: no positional argument
    /// begins so. The named arguments are then read committed, so an error in
    /// a value, a `CallTargetNotName` among them (JYRO-DEF-787), is the error
    /// of the parse and is not unwound into a positional reading.
    let private pCallArgs : Parser<CallArgs, unit> =
        between lparen rparen (
            (followedBy (attempt (anyIdentifier .>> colon)) >>. sepBy1 pNamedArg comma |>> NamedArgs)
            <|>
            (sepBy pExpr comma |>> PositionalArgs)
        )

    /// A postfix operator. A `(`, `[`, `++` or `--` after a line end does not
    /// continue the expression before it: it starts the next expression
    /// (20.1.2), so each of them is read here only on the line where the
    /// expression ends. A `.` may follow a line end.
    let private pPostfixOp : Parser<PostfixOp, unit> =
        choice [
            sameLine >>. withPos pCallArgs |>> fun (args, pos) -> CallOp(args, pos)
            withPos (dot >>. anyIdentifier) |>> fun (name, pos) -> DotOp(name, pos)
            sameLine >>. withPos (between lbracket rbracket pExpr) |>> fun (idx, pos) -> IndexOp(idx, pos)
            sameLine >>. withPos incrementOp |>> fun (_, pos) -> PostIncOp(pos)
            sameLine >>. withPos decrementOp |>> fun (_, pos) -> PostDecOp(pos)
        ]

    /// Apply one postfix operator to the current accumulator expression,
    /// whose text has the range `basePos`. The node's range runs from the
    /// start of that text to the end of the operator (spec 150.4.4): a call
    /// from its name to its `)`, an access from its target to its name or
    /// its `]`.
    /// A call is valid only when the accumulator is a single identifier,
    /// not one in parentheses: on one line, a `(` after any other expression
    /// is a parse error (20.1.3), so `(g)(1)`, `o.f(1)` and `x++(1)` are
    /// refused. It is reported as a parse failure via the `fail` combinator
    /// rather than a raw .NET exception so callers can observe a
    /// `JyroResult.Failure` in every error case.
    let private applyPostfix (baseExpr: Expr, basePos: Position) (op: PostfixOp) : Parser<Expr * Position, unit> =
        let node (opPos: Position) (expr: Position -> Expr) : Parser<Expr * Position, unit> =
            let pos = span basePos opPos
            preturn (expr pos, pos)
        match op with
        | CallOp(args, pos) ->
            match baseExpr with
            | Identifier(name, namePos) when namePos.StartIndex = basePos.StartIndex ->
                node pos (fun p -> Call(name, args, p))
            | _ -> fun _ -> callTargetNotName ()
        | DotOp(prop, pos) -> node pos (fun p -> PropertyAccess(baseExpr, prop, p))
        | IndexOp(idx, pos) -> node pos (fun p -> IndexAccess(baseExpr, idx, p))
        | PostIncOp(pos) -> node pos (fun p -> IncrementDecrement(baseExpr, true, false, p))
        | PostDecOp(pos) -> node pos (fun p -> IncrementDecrement(baseExpr, false, false, p))

    /// A primary expression and its postfix operators. A `(` on the line
    /// where an expression that is not a single identifier ends is a parse
    /// error there (20.1.3), at the `(`, rather than the start of another
    /// expression.
    let private pPostfixExpr : Parser<Expr * Position, unit> =
        fun stream ->
            let first = (withPos pPrimary) stream
            if first.Status <> FParsec.ReplyStatus.Ok then
                FParsec.Reply(first.Status, first.Error)
            else
                let mutable acc = first.Result
                let mutable isName =
                    match acc with
                    | Identifier(_, namePos), pos -> namePos.StartIndex = pos.StartIndex
                    | _ -> false
                let mutable result = None
                while result.IsNone do
                    if not isName && openParenOnSameLine stream then
                        result <- Some (callTargetNotName ())
                    else
                        let tag = stream.StateTag
                        let op = pPostfixOp stream
                        if op.Status = FParsec.ReplyStatus.Ok then
                            let applied = applyPostfix acc op.Result stream
                            if applied.Status = FParsec.ReplyStatus.Ok then
                                acc <- applied.Result
                                isName <- false
                            else
                                result <- Some (FParsec.Reply(applied.Status, applied.Error))
                        elif op.Status = FParsec.ReplyStatus.Error && stream.StateTag = tag then
                            result <- Some (FParsec.Reply(acc))
                        else
                            result <- Some (FParsec.Reply(op.Status, op.Error))
                result.Value

    // Prefix/unary expressions (only numeric negate at this level). A
    // prefix node runs from its operator to the end of its operand.
    let private pNegateOp : Parser<UnaryOp * Position, unit> =
        withPos (symbol "-") |>> fun (_, pos) -> (Negate, pos)

    let private pUnaryExpr : Parser<Expr * Position, unit> =
        let prefix (opPos: Position) (operandPos: Position) (make: Position -> Expr) =
            let pos = span opPos operandPos
            (make pos, pos)
        // The operand of a prefix operator is one level deeper than the
        // operator (spec 100.10.2).
        let rec impl stream =
            (choice [
                attempt (withPos incrementOp .>>. nested pPostfixExpr
                         |>> fun ((_, opPos), (e, ePos)) -> prefix opPos ePos (fun p -> IncrementDecrement(e, true, true, p)))
                attempt (withPos decrementOp .>>. nested pPostfixExpr
                         |>> fun ((_, opPos), (e, ePos)) -> prefix opPos ePos (fun p -> IncrementDecrement(e, false, true, p)))
                attempt (pNegateOp .>>. nested impl
                         |>> fun ((op, opPos), (e, ePos)) -> prefix opPos ePos (fun p -> Unary(op, e, p)))
                pPostfixExpr
            ]) stream
        impl

    /// A binary node runs from the start of its left operand's text to the
    /// end of its right operand's.
    let private binary (op: BinaryOp) (l: Expr, lPos: Position) (r: Expr, rPos: Position) =
        let pos = span lPos rPos
        (Binary(l, op, r, pos), pos)

    // Binary operators with precedence (using manual precedence climbing)
    // Helper: match binary operator symbol NOT followed by '=' (to avoid consuming *= /= %= += -=)
    let private binOpSym s : Parser<string, unit> = attempt (lexeme (pstring s .>> notFollowedBy (pchar '=')))

    let private pMulOp : Parser<BinaryOp, unit> =
        (binOpSym "*" >>% Multiply) <|> (binOpSym "/" >>% Divide) <|> (binOpSym "%" >>% Modulo)

    let private pMultiplicativeExpr : Parser<Expr * Position, unit> =
        chainl1 pUnaryExpr (pMulOp |>> binary)

    /// A binary `+` or `-`, never the first character of `++` or `--`: after
    /// a line end those start a prefix increment or decrement of the next
    /// expression (20.1.2), and on one line they are postfix operators.
    let private pAddOp : Parser<BinaryOp, unit> =
        let sym s (twice: char) = attempt (lexeme (pstring s .>> notFollowedBy (anyOf [ '='; twice ])))
        (sym "+" '+' >>% Add) <|> (sym "-" '-' >>% Subtract)

    let private pAdditiveExpr : Parser<Expr * Position, unit> =
        chainl1 pMultiplicativeExpr (pAddOp |>> binary)

    let private pConcatOp : Parser<BinaryOp, unit> = ampersandOp

    let private pConcatenationExpr : Parser<Expr * Position, unit> =
        chainl1 pAdditiveExpr (pConcatOp |>> binary)

    let private pRelOp : Parser<BinaryOp, unit> =
        choice [
            attempt (symbol "<=" >>% LessThanOrEqual)
            attempt (symbol ">=" >>% GreaterThanOrEqual)
            (symbol "<" >>% LessThan)
            (symbol ">" >>% GreaterThan)
        ]

    let private pRelationalExpr : Parser<Expr * Position, unit> =
        chainl1 pConcatenationExpr (pRelOp |>> binary)

    let private pEqOp : Parser<BinaryOp, unit> =
        (symbol "==" >>% Equal) <|> (symbol "!=" >>% NotEqual)

    let private pEqualityExpr : Parser<Expr * Position, unit> =
        chainl1 pRelationalExpr (pEqOp |>> binary)

    // Type check expression: expr is type or expr is not type
    // Placed between equality and logical AND so that "x is number and y > 0" works
    //
    // A type check ends with its type keyword, outside the postfix loop that
    // refuses a `(` on the same line after an expression that is not a single
    // name (20.1.3, JYRO-REQ-15), so the same refusal is made here.
    let private pTypeCheck : Parser<Expr * Position, unit> =
        let parse =
            pEqualityExpr .>>. opt (withPos (keyword "is" >>. opt (keyword "not") .>>. typeKeyword))
            |>> fun ((e, ePos), typeOpt) ->
                match typeOpt with
                | Some ((neg, t), tPos) ->
                    let pos = span ePos tPos
                    (TypeCheck(e, t, neg.IsSome, pos), true, pos)
                | None -> (e, false, ePos)
        fun stream ->
            let reply = parse stream
            if reply.Status <> FParsec.ReplyStatus.Ok then
                FParsec.Reply(reply.Status, reply.Error)
            else
                let (e, isTypeCheck, pos) = reply.Result
                if isTypeCheck && openParenOnSameLine stream then
                    callTargetNotName ()
                else
                    FParsec.Reply((e, pos))

    // Logical NOT: not (binds between type check and AND, like Python)
    let private pNotExpr : Parser<Expr * Position, unit> =
        let rec impl stream =
            (choice [
                attempt (withPos notOp .>>. nested impl
                         |>> fun ((_, opPos), (e, ePos)) ->
                             let pos = span opPos ePos
                             (Unary(Not, e, pos), pos))
                pTypeCheck
            ]) stream
        impl

    let private pAndOp : Parser<BinaryOp, unit> =
        keyword "and" >>% And

    let private pAndExpr : Parser<Expr * Position, unit> =
        chainl1 pNotExpr (pAndOp |>> binary)

    let private pOrOp : Parser<BinaryOp, unit> =
        keyword "or" >>% Or

    let private pOrExpr : Parser<Expr * Position, unit> =
        chainl1 pAndExpr (pOrOp |>> binary)

    let private pCoalesceExpr : Parser<Expr * Position, unit> =
        chainr1 pOrExpr (symbol "??" >>% binary Coalesce)

    let private pTernary : Parser<Expr * Position, unit> =
        pCoalesceExpr .>>. opt (questionMark >>. pExprSpanned .>> colon .>>. pExprSpanned)
        |>> fun ((cond, condPos), ternaryOpt) ->
            match ternaryOpt with
            | Some ((thenExpr, _), (elseExpr, elsePos)) ->
                let pos = span condPos elsePos
                (Ternary(cond, thenExpr, elseExpr, pos), pos)
            | None -> (cond, condPos)

    do pExprRef.Value <- pTernary

    let parseExpr : Parser<Expr, unit> = pExpr

    let parseExprList : Parser<Expr list, unit> = sepBy pExpr comma

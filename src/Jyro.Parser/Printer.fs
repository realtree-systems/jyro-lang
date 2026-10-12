namespace RealTree.Jyro.Parsing

open RealTree.Jyro
open RealTree.Jyro.Ast

open System
open System.Globalization
open System.Text

/// Pretty-printer: emits parseable Jyro source from an AST.
/// Precedence rules follow ExpressionParser so that parse(print(ast)) = ast
/// (structurally, ignoring Position). Intended uses beyond tests: diagnostic
/// rendering of AST fragments, `jyro fmt`-style formatting, refactor tooling.
module internal Printer =

    /// Precedence levels (higher = binds tighter). Mirrors the 14-level
    /// precedence table in the language reference.
    module Prec =
        let lambda = 0   // lambdas must be parenthesized in any sub-expr position
        let ternary = 1
        let coalesce = 2
        let logicalOr = 3
        let logicalAnd = 4
        let logicalNot = 5
        let typeCheck = 6
        let equality = 7
        let relational = 8
        let concat = 9
        let additive = 10
        let multiplicative = 11
        let unary = 12
        let postfix = 13
        let primary = 14

    let private binaryPrec =
        function
        | Coalesce -> Prec.coalesce
        | Or -> Prec.logicalOr
        | And -> Prec.logicalAnd
        | Equal
        | NotEqual -> Prec.equality
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual -> Prec.relational
        | Concat -> Prec.concat
        | Add
        | Subtract -> Prec.additive
        | Multiply
        | Divide
        | Modulo -> Prec.multiplicative

    let private isRightAssoc =
        function
        | Coalesce -> true
        | _ -> false

    let private exprPrec =
        function
        | Lambda _ -> Prec.lambda
        | Ternary _ -> Prec.ternary
        | Binary(_, op, _, _) -> binaryPrec op
        | Unary(Not, _, _) -> Prec.logicalNot
        | TypeCheck _ -> Prec.typeCheck
        | Unary(Negate, _, _) -> Prec.unary
        | IncrementDecrement(_, _, true, _) -> Prec.unary
        | IncrementDecrement(_, _, false, _) -> Prec.postfix
        | Call _
        | PropertyAccess _
        | IndexAccess _ -> Prec.postfix
        | Literal _
        | Identifier _
        | ArrayLiteral _
        | ObjectLiteral _
        | MatchExpr _ -> Prec.primary

    let private typeKeywordText =
        function
        | AnyType -> "any"
        | NumberType -> "number"
        | StringType -> "string"
        | BooleanType -> "boolean"
        | ObjectType -> "object"
        | ArrayType -> "array"
        | NullType -> "null"
        | FunctionType -> "function"

    let private escapeString (s: string) : string =
        let sb = StringBuilder(s.Length + 2)
        sb.Append('"') |> ignore
        for c in s do
            match c with
            | '\\' -> sb.Append("\\\\") |> ignore
            | '"' -> sb.Append("\\\"") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | '\000' -> sb.Append("\\0") |> ignore
            | c when Char.IsControl(c) ->
                sb.AppendFormat(CultureInfo.InvariantCulture, "\\u{0:X4}", int c) |> ignore
            | c -> sb.Append(c) |> ignore
        sb.Append('"') |> ignore
        sb.ToString()

    /// Format a number literal so the lexer can recover the same value.
    /// Exact decimals print in their natural (round-trip-safe) form; inexact
    /// doubles become `42` when whole and small, else "G17".
    let private formatNumber (n: JyroNumber) : string =
        if n.IsExact then
            n.DecimalValue.ToString(CultureInfo.InvariantCulture)
        else
            let f = n.Value
            if Double.IsNaN(f) || Double.IsInfinity(f) then
                invalidArg "n" "cannot print NaN or Infinity as a Jyro number literal"
            elif f = Math.Floor(f) && Math.Abs(f) < 1e16 then
                (int64 f).ToString(CultureInfo.InvariantCulture)
            else
                f.ToString("G17", CultureInfo.InvariantCulture)

    let private printLiteral (v: JyroValue) : string =
        match v with
        | :? JyroNull -> "null"
        | :? JyroBoolean as b -> if b.Value then "true" else "false"
        | :? JyroNumber as n -> formatNumber n
        | :? JyroString as s -> escapeString s.Value
        | _ -> invalidArg "v" (sprintf "unsupported literal type %A" (v.GetType()))

    let private isValidIdentifier (s: string) =
        if String.IsNullOrEmpty(s) then
            false
        elif Lexer.reservedKeywords.Contains(s) then
            false
        else
            // A word of 20.1.1, read by code point.
            let mutable ok = true
            let mutable i = 0
            while ok && i < s.Length do
                let struct (cp, width) = Unicode15.codePointAt s i
                ok <- if i = 0 then Unicode15.isIdentifierStart cp else Unicode15.isIdentifierChar cp
                i <- i + width
            ok

    let private printObjectKey (k: string) : string =
        if isValidIdentifier k then k else escapeString k

    /// Print an expression with a minimum-precedence context.
    /// The caller says "I need an expression at prec >= N here"; if the
    /// expression's own precedence is lower, we wrap it in parentheses.
    let rec private printExprAt (minPrec: int) (e: Expr) : string =
        let p = exprPrec e
        let raw = printRaw e
        if p < minPrec then "(" + raw + ")" else raw

    and private printRaw (e: Expr) : string =
        match e with
        | Literal(v, _) -> printLiteral v

        | Identifier(name, _) -> name

        | Binary(l, op, r, _) ->
            let opText = Operators.binaryOpToString op
            let prec = binaryPrec op
            let leftMin = if isRightAssoc op then prec + 1 else prec
            let rightMin = if isRightAssoc op then prec else prec + 1
            sprintf "%s %s %s" (printExprAt leftMin l) opText (printExprAt rightMin r)

        | Unary(Not, e', _) -> sprintf "not %s" (printExprAt Prec.typeCheck e')

        | Unary(Negate, e', _) ->
            // Protect against `- -x` collapsing into `--` decrement, or
            // `-++x` collapsing across tokens.
            let operandNeedsParens =
                match e' with
                | Unary(Negate, _, _) -> true
                | IncrementDecrement(_, _, true, _) -> true
                | _ -> false
            if operandNeedsParens then
                sprintf "-(%s)" (printRaw e')
            else
                sprintf "-%s" (printExprAt Prec.unary e')

        | Ternary(c, t, el, _) ->
            sprintf
                "%s ? %s : %s"
                (printExprAt Prec.coalesce c)
                (printExprAt Prec.ternary t)
                (printExprAt Prec.ternary el)

        | Call(name, args, _) -> sprintf "%s(%s)" name (printArgs args)

        | PropertyAccess(target, prop, _) -> sprintf "%s.%s" (printExprAt Prec.postfix target) prop

        | IndexAccess(target, idx, _) ->
            sprintf "%s[%s]" (printExprAt Prec.postfix target) (printExprAt Prec.ternary idx)

        | ObjectLiteral(props, _) ->
            let body =
                props
                |> List.map (fun (k, v) ->
                    sprintf "%s: %s" (printObjectKey k) (printExprAt Prec.ternary v))
                |> String.concat ", "
            sprintf "{%s}" body

        | ArrayLiteral(elems, _) ->
            let body =
                elems |> List.map (printExprAt Prec.ternary) |> String.concat ", "
            sprintf "[%s]" body

        | Lambda(parameters, body, _) ->
            let paramText =
                match parameters with
                | [ one ] -> one
                | many -> "(" + (String.concat ", " many) + ")"
            sprintf "%s => %s" paramText (printExprAt Prec.ternary body)

        | TypeCheck(e', t, isNeg, _) ->
            let kw = if isNeg then "is not" else "is"
            sprintf "%s %s %s" (printExprAt Prec.equality e') kw (typeKeywordText t)

        | IncrementDecrement(e', isInc, isPrefix, _) ->
            let op = if isInc then "++" else "--"
            if isPrefix then
                sprintf "%s%s" op (printExprAt Prec.postfix e')
            else
                sprintf "%s%s" (printExprAt Prec.postfix e') op

        | MatchExpr(scrutinee, cases, _) ->
            let caseLines =
                cases
                |> List.map (fun c ->
                    let bindText =
                        match c.Bindings with
                        | [] -> ""
                        | bs -> "(" + (String.concat ", " bs) + ")"
                    sprintf
                        "case %s%s then %s"
                        c.VariantName
                        bindText
                        (printExprAt Prec.ternary c.Body))
                |> String.concat " "
            sprintf "match %s do %s end" (printExprAt Prec.ternary scrutinee) caseLines

    and private printArgs =
        function
        | PositionalArgs xs ->
            xs |> List.map (printExprAt Prec.ternary) |> String.concat ", "
        | NamedArgs pairs ->
            pairs
            |> List.map (fun (n, v) -> sprintf "%s: %s" n (printExprAt Prec.ternary v))
            |> String.concat ", "

    let printExpr (e: Expr) : string = printExprAt Prec.ternary e

    // -- Statement printing ---------------------------------------------------

    let private indentStr (level: int) : string = String.replicate level "  "

    let private printFuncParam (p: FuncParam) : string =
        let sb = StringBuilder(p.Name)
        match p.TypeHint with
        | Some t -> sb.Append(": ").Append(typeKeywordText t) |> ignore
        | None -> ()
        match p.DefaultValue with
        | Some e -> sb.Append(" = ").Append(printExpr e) |> ignore
        | None -> ()
        sb.ToString()

    let private printUnionVariant (v: UnionVariant) : string =
        match v.Fields with
        | [] -> v.Name
        | fields ->
            let fieldText =
                fields
                |> List.map (fun (name, typeHint) ->
                    match typeHint with
                    | Some t -> sprintf "%s: %s" name (typeKeywordText t)
                    | None -> name)
                |> String.concat ", "
            sprintf "%s(%s)" v.Name fieldText

    let rec private printStmt (indent: int) (sb: StringBuilder) (s: Stmt) : unit =
        let pad = indentStr indent
        let emitLine (text: string) =
            sb.Append(pad).Append(text).Append('\n') |> ignore
        match s with
        | VarDecl(name, typeHint, init, _) ->
            let core =
                match typeHint with
                | Some t -> sprintf "var %s: %s" name (typeKeywordText t)
                | None -> sprintf "var %s" name
            match init with
            | Some e -> emitLine (sprintf "%s = %s" core (printExpr e))
            | None -> emitLine core

        | Assignment(target, op, value, typeHint, _) ->
            let targetText = printExprAt Prec.postfix target
            let typeText =
                match typeHint with
                | Some t -> ": " + typeKeywordText t
                | None -> ""
            let opText = Operators.assignOpToString op
            emitLine (sprintf "%s%s %s %s" targetText typeText opText (printExpr value))

        | If(cond, thenBlock, elseIfs, elseBlock, _) ->
            emitLine (sprintf "if %s then" (printExpr cond))
            printBlock (indent + 1) sb thenBlock
            for (econd, ebody) in elseIfs do
                emitLine (sprintf "elseif %s then" (printExpr econd))
                printBlock (indent + 1) sb ebody
            match elseBlock with
            | Some body ->
                emitLine "else"
                printBlock (indent + 1) sb body
            | None -> ()
            emitLine "end"

        | While(cond, body, _) ->
            emitLine (sprintf "while %s do" (printExpr cond))
            printBlock (indent + 1) sb body
            emitLine "end"

        | ForEach(varName, collection, body, _) ->
            emitLine (sprintf "foreach %s in %s do" varName (printExpr collection))
            printBlock (indent + 1) sb body
            emitLine "end"

        | For(varName, startE, endE, stepE, direction, body, _) ->
            let dir =
                match direction with
                | Ascending -> "to"
                | Descending -> "downto"
            let stepText =
                match stepE with
                | Some e -> sprintf " by %s" (printExpr e)
                | None -> ""
            emitLine (
                sprintf
                    "for %s in %s %s %s%s do"
                    varName
                    (printExpr startE)
                    dir
                    (printExpr endE)
                    stepText)
            printBlock (indent + 1) sb body
            emitLine "end"

        | Switch(scrutinee, cases, defaultCase, _) ->
            emitLine (sprintf "switch %s do" (printExpr scrutinee))
            for c in cases do
                let values = c.Values |> List.map printExpr |> String.concat ", "
                sb.Append(indentStr (indent + 1)).Append(sprintf "case %s then\n" values)
                |> ignore
                printBlock (indent + 2) sb c.Body
            match defaultCase with
            | Some body ->
                sb.Append(indentStr (indent + 1)).Append("default then\n") |> ignore
                printBlock (indent + 2) sb body
            | None -> ()
            emitLine "end"

        | Match(scrutinee, cases, _) ->
            emitLine (sprintf "match %s do" (printExpr scrutinee))
            for c in cases do
                let bindText =
                    match c.Bindings with
                    | [] -> ""
                    | bs -> "(" + (String.concat ", " bs) + ")"
                sb
                    .Append(indentStr (indent + 1))
                    .Append(sprintf "case %s%s then\n" c.VariantName bindText)
                |> ignore
                printBlock (indent + 2) sb c.Body
            emitLine "end"

        | Return(value, _) ->
            match value with
            | Some e -> emitLine (sprintf "return %s" (printExpr e))
            | None -> emitLine "return"

        | Exit(value, _) ->
            match value with
            | Some e -> emitLine (sprintf "exit %s" (printExpr e))
            | None -> emitLine "exit"

        | Fail(msg, _) ->
            match msg with
            | Some e -> emitLine (sprintf "fail %s" (printExpr e))
            | None -> emitLine "fail"

        | Break _ -> emitLine "break"
        | Continue _ -> emitLine "continue"

        | Delete(target, _) -> emitLine (sprintf "delete %s" (printExprAt Prec.postfix target))

        | ExprStmt(e, _) -> emitLine (printExpr e)

        | FuncDef(name, parameters, body, _) ->
            let paramText = parameters |> List.map printFuncParam |> String.concat ", "
            emitLine (sprintf "func %s(%s)" name paramText)
            printBlock (indent + 1) sb body
            emitLine "end"

        | UnionDef(name, variants, _) ->
            let variantText = variants |> List.map printUnionVariant |> String.concat " "
            emitLine (sprintf "union %s %s end" name variantText)

    and private printBlock (indent: int) (sb: StringBuilder) (stmts: Stmt list) : unit =
        for s in stmts do
            printStmt indent sb s

    let printProgram (p: Program) : string =
        let sb = StringBuilder()
        printBlock 0 sb p.Statements
        sb.ToString()

    let printStatement (s: Stmt) : string =
        let sb = StringBuilder()
        printStmt 0 sb s
        sb.ToString()

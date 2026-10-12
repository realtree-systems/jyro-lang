namespace RealTree.Jyro.Tests

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing

/// Structural shrinkers for AST types. Shrinking reduces a failing
/// counterexample to a minimal form by yielding simpler candidates. FsCheck
/// repeatedly tries each candidate; the smallest one that still falsifies
/// the property is what the user sees.
///
/// Strategy per type:
/// - Expr: yield each sub-expression (as a drop-the-wrapper shrink), then
///   yield structural shrinks that swap one sub-expression for one of its
///   simpler shrinks.
/// - Stmt: yield statement forms with parts removed (e.g. an If without
///   its elseif chain or else block) and the bodies of compound statements
///   as standalone replacements.
/// - Program / block: drop-one-statement shrinks first, then per-statement.
module internal Shrinkers =

    let private pos = Position.None

    // -- Expr -----------------------------------------------------------------

    let rec shrinkExpr (e: Expr) : seq<Expr> =
        seq {
            // Drop-the-wrapper shrinks: yield sub-expressions directly.
            match e with
            | Literal _
            | Identifier _ -> ()
            | Binary(l, _, r, _) ->
                yield l
                yield r
            | Unary(_, x, _) -> yield x
            | Ternary(c, t, f, _) ->
                yield c
                yield t
                yield f
            | Call(_, PositionalArgs args, _) -> yield! args
            | Call(_, NamedArgs pairs, _) -> yield! pairs |> List.map snd
            | PropertyAccess(t, _, _) -> yield t
            | IndexAccess(t, i, _) ->
                yield t
                yield i
            | ObjectLiteral(props, _) -> yield! props |> List.map snd
            | ArrayLiteral(es, _) -> yield! es
            | Lambda(_, body, _) -> yield body
            | TypeCheck(e', _, _, _) -> yield e'
            | IncrementDecrement(e', _, _, _) -> yield e'
            | MatchExpr(e', cases, _) ->
                yield e'
                yield! cases |> List.map (fun c -> c.Body)

            // Structural shrinks: replace one sub-expression with a smaller variant.
            match e with
            | Literal _
            | Identifier _ -> ()
            | Binary(l, op, r, p) ->
                for l' in shrinkExpr l -> Binary(l', op, r, p)
                for r' in shrinkExpr r -> Binary(l, op, r', p)
            | Unary(op, x, p) ->
                for x' in shrinkExpr x -> Unary(op, x', p)
            | Ternary(c, t, f, p) ->
                for c' in shrinkExpr c -> Ternary(c', t, f, p)
                for t' in shrinkExpr t -> Ternary(c, t', f, p)
                for f' in shrinkExpr f -> Ternary(c, t, f', p)
            | Call(n, PositionalArgs args, p) ->
                for i in 0 .. List.length args - 1 do
                    let dropped = args |> List.mapi (fun j a -> (j, a))
                                       |> List.filter (fun (j, _) -> j <> i)
                                       |> List.map snd
                    yield Call(n, PositionalArgs dropped, p)
            | Call(n, NamedArgs pairs, p) ->
                for i in 0 .. List.length pairs - 1 do
                    let dropped = pairs |> List.mapi (fun j a -> (j, a))
                                        |> List.filter (fun (j, _) -> j <> i)
                                        |> List.map snd
                    yield Call(n, NamedArgs dropped, p)
            | PropertyAccess(t, prop, p) ->
                for t' in shrinkExpr t -> PropertyAccess(t', prop, p)
            | IndexAccess(t, i, p) ->
                for t' in shrinkExpr t -> IndexAccess(t', i, p)
                for i' in shrinkExpr i -> IndexAccess(t, i', p)
            | ObjectLiteral(props, p) ->
                for i in 0 .. List.length props - 1 do
                    let dropped = props |> List.mapi (fun j a -> (j, a))
                                        |> List.filter (fun (j, _) -> j <> i)
                                        |> List.map snd
                    yield ObjectLiteral(dropped, p)
            | ArrayLiteral(es, p) ->
                for i in 0 .. List.length es - 1 do
                    let dropped = es |> List.mapi (fun j a -> (j, a))
                                     |> List.filter (fun (j, _) -> j <> i)
                                     |> List.map snd
                    yield ArrayLiteral(dropped, p)
            | Lambda(ps, body, p) ->
                for body' in shrinkExpr body -> Lambda(ps, body', p)
            | TypeCheck(e', t, neg, p) ->
                for e'' in shrinkExpr e' -> TypeCheck(e'', t, neg, p)
            | IncrementDecrement(e', inc, pre, p) ->
                for e'' in shrinkExpr e' -> IncrementDecrement(e'', inc, pre, p)
            | MatchExpr(scrut, cases, p) ->
                // Drop one case (keep at least one).
                if List.length cases > 1 then
                    for i in 0 .. List.length cases - 1 do
                        let dropped = cases |> List.mapi (fun j c -> (j, c))
                                            |> List.filter (fun (j, _) -> j <> i)
                                            |> List.map snd
                        yield MatchExpr(scrut, dropped, p)
        }

    // -- Block / Stmt ---------------------------------------------------------

    /// Drop any single statement from the block (a classic list shrinker).
    let private dropOne (xs: 'a list) : seq<'a list> =
        seq {
            for i in 0 .. List.length xs - 1 do
                yield xs |> List.mapi (fun j x -> (j, x))
                         |> List.filter (fun (j, _) -> j <> i)
                         |> List.map snd
        }

    let rec shrinkStmt (s: Stmt) : seq<Stmt> =
        seq {
            match s with
            | VarDecl(_, _, init, p) ->
                match init with
                | Some _ ->
                    yield VarDecl("x", None, None, p)
                | None -> ()
            | Assignment(target, _, value, _, p) ->
                yield ExprStmt(target, p)
                yield ExprStmt(value, p)
            | If(_, thenB, elseIfs, elseB, p) ->
                match elseB with
                | Some _ -> yield If(Literal(JyroBoolean.FromBoolean(true), p), thenB, elseIfs, None, p)
                | None -> ()
                for ei in dropOne elseIfs do
                    yield If(Literal(JyroBoolean.FromBoolean(true), p), thenB, ei, elseB, p)
                // Replace with the then-block as a single NOP-like stmt set.
                yield! thenB
                match elseB with
                | Some b -> yield! b
                | None -> ()
            | While(_, body, p) ->
                yield While(Literal(JyroBoolean.FromBoolean(false), p), [], p)
                yield! body
            | ForEach(_, _, body, p) ->
                yield ForEach("x", Literal(JyroNull.Instance, p), [], p)
                yield! body
            | For(_, _, _, _, dir, body, p) ->
                yield For("x", Literal(JyroNumber(0.0), p), Literal(JyroNumber(0.0), p), None, dir, [], p)
                yield! body
            | Switch(_, cases, defaultCase, p) ->
                yield Switch(Literal(JyroNull.Instance, p), [], None, p)
                for c in cases do yield! c.Body
                match defaultCase with
                | Some b -> yield! b
                | None -> ()
            | Match(_, cases, p) ->
                if List.length cases > 1 then
                    for dropped in dropOne cases do
                        yield Match(Literal(JyroNull.Instance, p), dropped, p)
                for c in cases do yield! c.Body
            | FuncDef(_, parameters, body, p) ->
                yield FuncDef("f", [], [], p)
                if not (List.isEmpty parameters) then
                    yield FuncDef("f", [], body, p)
                yield! body
            | UnionDef(_, variants, p) ->
                if List.length variants > 1 then
                    for dropped in dropOne variants do
                        yield UnionDef("U", dropped, p)
            | Return(Some _, p) -> yield Return(None, p)
            | Exit(Some _, p) -> yield Exit(None, p)
            | Fail(Some _, p) -> yield Fail(None, p)
            | Delete _
            | Return(None, _)
            | Exit(None, _)
            | Fail(None, _)
            | Break _
            | Continue _
            | ExprStmt _ -> ()

            // Structural shrinks: replace an inner expression with a shrunk variant.
            match s with
            | ExprStmt(e, p) ->
                for e' in shrinkExpr e -> ExprStmt(e', p)
            | VarDecl(n, t, Some init, p) ->
                for init' in shrinkExpr init -> VarDecl(n, t, Some init', p)
            | Assignment(target, op, value, th, p) ->
                for t' in shrinkExpr target -> Assignment(t', op, value, th, p)
                for v' in shrinkExpr value -> Assignment(target, op, v', th, p)
            | Return(Some v, p) ->
                for v' in shrinkExpr v -> Return(Some v', p)
            | Exit(Some v, p) ->
                for v' in shrinkExpr v -> Exit(Some v', p)
            | Fail(Some v, p) ->
                for v' in shrinkExpr v -> Fail(Some v', p)
            | Delete(target, p) ->
                for t' in shrinkExpr target -> Delete(t', p)
            | _ -> ()
        }

    let shrinkBlock (stmts: Stmt list) : seq<Stmt list> =
        seq {
            // Drop-one-statement shrinks first - often the most effective.
            yield! dropOne stmts
            // Then shrink each statement individually.
            for i in 0 .. List.length stmts - 1 do
                let s = stmts.[i]
                for s' in shrinkStmt s do
                    let replaced =
                        stmts
                        |> List.mapi (fun j x -> if j = i then s' else x)
                    yield replaced
        }

    let shrinkProgram (p: Program) : seq<Program> =
        shrinkBlock p.Statements
        |> Seq.map (fun stmts -> { Statements = stmts })

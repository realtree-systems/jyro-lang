namespace RealTree.Jyro.Tests

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing

/// Structural AST equality that ignores Position values.
/// Round-trip properties need this because `parse (print ast)` produces an AST
/// with real source positions, while generated ASTs carry Position.None.
module internal AstEquality =

    let private eqFloat (a: float) (b: float) =
        System.Double.IsNaN(a) && System.Double.IsNaN(b) || a = b

    let private eqJyroValue (a: JyroValue) (b: JyroValue) =
        match a, b with
        | (:? JyroNull), (:? JyroNull) -> true
        | (:? JyroBoolean as x), (:? JyroBoolean as y) -> x.Value = y.Value
        | (:? JyroNumber as x), (:? JyroNumber as y) ->
            x.IsExact = y.IsExact
            && (if x.IsExact then x.DecimalValue = y.DecimalValue else eqFloat x.Value y.Value)
        | (:? JyroString as x), (:? JyroString as y) -> x.Value = y.Value
        | _ -> false

    let rec eqExpr (a: Expr) (b: Expr) : bool =
        match a, b with
        | Literal(va, _), Literal(vb, _) -> eqJyroValue va vb
        | Identifier(na, _), Identifier(nb, _) -> na = nb
        | Binary(la, oa, ra, _), Binary(lb, ob, rb, _) ->
            oa = ob && eqExpr la lb && eqExpr ra rb
        | Unary(oa, ea, _), Unary(ob, eb, _) -> oa = ob && eqExpr ea eb
        | Ternary(ca, ta, ea, _), Ternary(cb, tb, eb, _) ->
            eqExpr ca cb && eqExpr ta tb && eqExpr ea eb
        | Call(na, aa, _), Call(nb, ab, _) -> na = nb && eqCallArgs aa ab
        | PropertyAccess(ta, pa, _), PropertyAccess(tb, pb, _) ->
            pa = pb && eqExpr ta tb
        | IndexAccess(ta, ia, _), IndexAccess(tb, ib, _) ->
            eqExpr ta tb && eqExpr ia ib
        | ObjectLiteral(pa, _), ObjectLiteral(pb, _) ->
            List.length pa = List.length pb &&
            List.forall2 (fun (ka, va) (kb, vb) -> ka = kb && eqExpr va vb) pa pb
        | ArrayLiteral(ea, _), ArrayLiteral(eb, _) ->
            List.length ea = List.length eb && List.forall2 eqExpr ea eb
        | Lambda(pa, ba, _), Lambda(pb, bb, _) -> pa = pb && eqExpr ba bb
        | TypeCheck(ea, ta, na, _), TypeCheck(eb, tb, nb, _) ->
            ta = tb && na = nb && eqExpr ea eb
        | IncrementDecrement(ea, ia, pa, _), IncrementDecrement(eb, ib, pb, _) ->
            ia = ib && pa = pb && eqExpr ea eb
        | MatchExpr(ea, ca, _), MatchExpr(eb, cb, _) ->
            eqExpr ea eb &&
            List.length ca = List.length cb &&
            List.forall2 (fun (x: MatchExprCase) (y: MatchExprCase) ->
                x.VariantName = y.VariantName
                && x.Bindings = y.Bindings
                && eqExpr x.Body y.Body) ca cb
        | _ -> false

    and eqCallArgs (a: CallArgs) (b: CallArgs) : bool =
        match a, b with
        | PositionalArgs xs, PositionalArgs ys ->
            List.length xs = List.length ys && List.forall2 eqExpr xs ys
        | NamedArgs xs, NamedArgs ys ->
            List.length xs = List.length ys &&
            List.forall2 (fun (na, va) (nb, vb) -> na = nb && eqExpr va vb) xs ys
        | _ -> false

    let rec eqStmt (a: Stmt) (b: Stmt) : bool =
        match a, b with
        | VarDecl(na, ta, ia, _), VarDecl(nb, tb, ib, _) ->
            na = nb && ta = tb && eqExprOption ia ib
        | Assignment(ta, oa, va, ha, _), Assignment(tb, ob, vb, hb, _) ->
            oa = ob && ha = hb && eqExpr ta tb && eqExpr va vb
        | If(ca, ta, ea, elsa, _), If(cb, tb, eb, elsb, _) ->
            eqExpr ca cb && eqBlock ta tb &&
            List.length ea = List.length eb &&
            List.forall2 (fun (x, xb) (y, yb) -> eqExpr x y && eqBlock xb yb) ea eb &&
            eqBlockOption elsa elsb
        | While(ca, ba, _), While(cb, bb, _) -> eqExpr ca cb && eqBlock ba bb
        | ForEach(na, ca, ba, _), ForEach(nb, cb, bb, _) ->
            na = nb && eqExpr ca cb && eqBlock ba bb
        | For(na, sa, ea, stpa, da, ba, _), For(nb, sb, eb, stpb, db, bb, _) ->
            na = nb && da = db
            && eqExpr sa sb && eqExpr ea eb
            && eqExprOption stpa stpb
            && eqBlock ba bb
        | Switch(ea, ca, da, _), Switch(eb, cb, db, _) ->
            eqExpr ea eb
            && List.length ca = List.length cb
            && List.forall2 (fun (x: SwitchCase) (y: SwitchCase) ->
                List.length x.Values = List.length y.Values
                && List.forall2 eqExpr x.Values y.Values
                && eqBlock x.Body y.Body) ca cb
            && eqBlockOption da db
        | Return(va, _), Return(vb, _) -> eqExprOption va vb
        | Fail(ma, _), Fail(mb, _) -> eqExprOption ma mb
        | Exit(va, _), Exit(vb, _) -> eqExprOption va vb
        | FuncDef(na, pa, ba, _), FuncDef(nb, pb, bb, _) ->
            na = nb
            && List.length pa = List.length pb
            && List.forall2 eqFuncParam pa pb
            && eqBlock ba bb
        | Break _, Break _ -> true
        | Continue _, Continue _ -> true
        | ExprStmt(ea, _), ExprStmt(eb, _) -> eqExpr ea eb
        | UnionDef(na, va, _), UnionDef(nb, vb, _) ->
            na = nb
            && List.length va = List.length vb
            && List.forall2 (fun (x: UnionVariant) (y: UnionVariant) ->
                x.Name = y.Name && x.Fields = y.Fields) va vb
        | Match(ea, ca, _), Match(eb, cb, _) ->
            eqExpr ea eb
            && List.length ca = List.length cb
            && List.forall2 (fun (x: MatchCase) (y: MatchCase) ->
                x.VariantName = y.VariantName
                && x.Bindings = y.Bindings
                && eqBlock x.Body y.Body) ca cb
        | Delete(ta, _), Delete(tb, _) -> eqExpr ta tb
        | _ -> false

    and eqBlock (a: Stmt list) (b: Stmt list) : bool =
        List.length a = List.length b && List.forall2 eqStmt a b

    and eqBlockOption (a: Stmt list option) (b: Stmt list option) : bool =
        match a, b with
        | None, None -> true
        | Some xs, Some ys -> eqBlock xs ys
        | _ -> false

    and eqExprOption (a: Expr option) (b: Expr option) : bool =
        match a, b with
        | None, None -> true
        | Some x, Some y -> eqExpr x y
        | _ -> false

    and eqFuncParam (a: FuncParam) (b: FuncParam) : bool =
        a.Name = b.Name
        && a.TypeHint = b.TypeHint
        && eqExprOption a.DefaultValue b.DefaultValue

    let eqProgram (a: Program) (b: Program) : bool =
        eqBlock a.Statements b.Statements

namespace RealTree.Jyro.Ast

open RealTree.Jyro

[<Struct>]
type internal Position =
    { Line: int
      Column: int
      StartIndex: int
      EndIndex: int }

    static member None = { Line = 0; Column = 0; StartIndex = 0; EndIndex = 0 }

    static member Create(line: int, column: int, ?startIndex: int, ?endIndex: int) =
        { Line = line
          Column = column
          StartIndex = defaultArg startIndex 0
          EndIndex = defaultArg endIndex 0 }

    // The length of the node's text in UTF-16 code units: the end index
    // minus the start index, computed in 64 bits so it cannot wrap, and 0
    // when that difference is negative or above Int32.MaxValue. A parsed
    // node always has a difference in range; a node read from an artifact
    // has the indexes its writer recorded.
    member this.Length : int =
        let d = int64 this.EndIndex - int64 this.StartIndex
        if d >= 0L && d <= int64 System.Int32.MaxValue then int d else 0

    // Where a diagnostic of this node is placed (spec 130.1.2): its line
    // and column, and the length of its text.
    member this.Location : SourceLocation =
        SourceLocation(this.Line, this.Column, this.Length)

type internal JyroType =
    | AnyType
    | NumberType
    | StringType
    | BooleanType
    | ObjectType
    | ArrayType
    | NullType
    | FunctionType

// A variant of a union declaration. It is a structure, not a node (spec
// 150.9.5), but it carries its own position (spec 150.8.5, JYRO-ADR-138):
// the range from the first code unit of its name to the end of its fields,
// where a diagnostic about the variant, `DuplicateVariantName`, is placed.
type internal UnionVariant =
    { Name: string
      Fields: (string * JyroType option) list
      Pos: Position }

type internal Expr =
    // A literal value (number, string, boolean, null)
    | Literal of value: JyroValue * pos: Position
    | Identifier of name: string * pos: Position
    // A binary operation (e.g., a + b)
    | Binary of left: Expr * op: BinaryOp * right: Expr * pos: Position
    // A unary operation (e.g., -x, !y)
    | Unary of op: UnaryOp * operand: Expr * pos: Position
    // A ternary/conditional expression (e.g., a ? b : c)
    | Ternary of condition: Expr * thenExpr: Expr * elseExpr: Expr * pos: Position
    // A function call (e.g., Func(a, b))
    | Call of name: string * args: CallArgs * pos: Position
    // Property access using dot notation (e.g., obj.prop)
    | PropertyAccess of target: Expr * property: string * pos: Position
    // Index access using bracket notation (e.g., arr[0])
    | IndexAccess of target: Expr * index: Expr * pos: Position
    // Object literal (e.g., { a: 1, b: 2 })
    | ObjectLiteral of properties: (string * Expr) list * pos: Position
    // Array literal (e.g., [1, 2, 3])
    | ArrayLiteral of elements: Expr list * pos: Position
    // Lambda expression (e.g., x => x + 1)
    | Lambda of parameters: string list * body: Expr * pos: Position
    // Type check expression (e.g., x is string)
    | TypeCheck of expr: Expr * typeName: JyroType * isNegated: bool * pos: Position
    // Pre/post increment/decrement (e.g., ++x, x--)
    | IncrementDecrement of expr: Expr * isIncrement: bool * isPrefix: bool * pos: Position
    // Match expression (each arm is a single expression that produces a value)
    | MatchExpr of expr: Expr * cases: MatchExprCase list * pos: Position

    member this.Position =
        match this with
        | Literal(_, pos) -> pos
        | Identifier(_, pos) -> pos
        | Binary(_, _, _, pos) -> pos
        | Unary(_, _, pos) -> pos
        | Ternary(_, _, _, pos) -> pos
        | Call(_, _, pos) -> pos
        | PropertyAccess(_, _, pos) -> pos
        | IndexAccess(_, _, pos) -> pos
        | ObjectLiteral(_, pos) -> pos
        | ArrayLiteral(_, pos) -> pos
        | Lambda(_, _, pos) -> pos
        | TypeCheck(_, _, _, pos) -> pos
        | IncrementDecrement(_, _, _, pos) -> pos
        | MatchExpr(_, _, pos) -> pos

// A case in a match expression (body is a single expression)
and internal MatchExprCase =
    { VariantName: string
      Bindings: string list
      Body: Expr
      Pos: Position }

// Arguments to a function call - either all positional or all named
and internal CallArgs =
    | PositionalArgs of Expr list
    | NamedArgs of (string * Expr) list

and internal FuncParam =
    { Name: string
      TypeHint: JyroType option
      DefaultValue: Expr option }

type internal ForDirection =
    | Ascending
    | Descending

type internal Stmt =
    // Variable declaration (e.g., var x = 5)
    | VarDecl of name: string * typeHint: JyroType option * init: Expr option * pos: Position
    // Assignment (e.g., x = 5, x += 3, Data.x: number = 42)
    | Assignment of target: Expr * op: AssignOp * value: Expr * typeHint: JyroType option * pos: Position
    | If of condition: Expr * thenBlock: Stmt list * elseIfs: (Expr * Stmt list) list * elseBlock: Stmt list option * pos: Position
    | While of condition: Expr * body: Stmt list * pos: Position
    | ForEach of varName: string * collection: Expr * body: Stmt list * pos: Position
    // Range-based for loop (e.g., for i in 0 to 10 by 2 do ... end)
    | For of varName: string * startExpr: Expr * endExpr: Expr * stepExpr: Expr option * direction: ForDirection * body: Stmt list * pos: Position
    | Switch of expr: Expr * cases: SwitchCase list * defaultCase: Stmt list option * pos: Position
    | Return of value: Expr option * pos: Position
    // Fail statement (business logic failure)
    | Fail of message: Expr option * pos: Position
    // Exit statement (clean script termination)
    | Exit of value: Expr option * pos: Position
    | FuncDef of name: string * parameters: FuncParam list * body: Stmt list * pos: Position
    | Break of pos: Position
    | Continue of pos: Position
    // Expression statement (expression evaluated for side effects)
    | ExprStmt of expr: Expr * pos: Position
    // Union type declaration (e.g., union Shape Circle(radius: number) ... end)
    | UnionDef of name: string * variants: UnionVariant list * pos: Position
    // Match statement for union pattern matching
    | Match of expr: Expr * cases: MatchCase list * pos: Position
    | Delete of target: Expr * pos: Position

    member this.Position =
        match this with
        | VarDecl(_, _, _, pos) -> pos
        | Assignment(_, _, _, _, pos) -> pos
        | If(_, _, _, _, pos) -> pos
        | While(_, _, pos) -> pos
        | ForEach(_, _, _, pos) -> pos
        | For(_, _, _, _, _, _, pos) -> pos
        | Switch(_, _, _, pos) -> pos
        | Return(_, pos) -> pos
        | Fail(_, pos) -> pos
        | Exit(_, pos) -> pos
        | FuncDef(_, _, _, pos) -> pos
        | Break pos -> pos
        | Continue pos -> pos
        | ExprStmt(_, pos) -> pos
        | UnionDef(_, _, pos) -> pos
        | Match(_, _, pos) -> pos
        | Delete(_, pos) -> pos

and internal SwitchCase =
    { Values: Expr list
      Body: Stmt list }

and internal MatchCase =
    { VariantName: string
      Bindings: string list
      Body: Stmt list
      Pos: Position }

type internal Program =
    { Statements: Stmt list }

// Predicates and case extractors on the `Stmt` and `Expr` DUs. Centralized
// so every consumer enumerates all cases through a single point.
module internal Ast =
    // The one maximum nesting depth of spec 100.10.1 (JYRO-ADR-177): the
    // bound of a source text, counted as 100.10.2 gives (the parser refuses
    // a deeper one with `SourceNestingLimitExceeded`), and of an artifact,
    // counted as 150.9.9 gives (its reader refuses a deeper one, its writer
    // will not write one). Fixed: a host cannot set it (100.1.3).
    [<Literal>]
    let MaxNestingDepth = 256

    let isAssignmentTarget = function
        | Identifier _
        | PropertyAccess _
        | IndexAccess _ -> true
        | Literal _
        | Binary _
        | Unary _
        | Ternary _
        | Call _
        | ObjectLiteral _
        | ArrayLiteral _
        | Lambda _
        | TypeCheck _
        | IncrementDecrement _
        | MatchExpr _ -> false

    let isDeleteTarget = function
        | PropertyAccess _
        | IndexAccess _ -> true
        | Identifier _
        | Literal _
        | Binary _
        | Unary _
        | Ternary _
        | Call _
        | ObjectLiteral _
        | ArrayLiteral _
        | Lambda _
        | TypeCheck _
        | IncrementDecrement _
        | MatchExpr _ -> false

    // The identifier an assignment or delete target is ultimately rooted at:
    // `Input.a.b[0].c` roots at `Input`. `None` for a target that bottoms out
    // in something other than an identifier.
    //
    // Callers enforcing a rule about a named value (Input being read-only)
    // MUST root the target rather than matching its outermost shape. Matching
    // only `Input.x` and `Input[i]` refuses the first level and admits every
    // level below it, so `Input.a.b = 1` would write straight into the host's
    // own object graph.
    let rec targetRoot expr =
        match expr with
        | Identifier(name, _) -> Some name
        | PropertyAccess(target, _, _) -> targetRoot target
        | IndexAccess(target, _, _) -> targetRoot target
        | Literal _
        | Binary _
        | Unary _
        | Ternary _
        | Call _
        | ObjectLiteral _
        | ArrayLiteral _
        | Lambda _
        | TypeCheck _
        | IncrementDecrement _
        | MatchExpr _ -> None

    // The parameter defaults the parser read in parentheses, by node
    // identity. A grouped expression has no node of its own (spec 150.4.4),
    // so `a = (1)` and `a = 1` give the same `Literal`; validation refuses
    // the first with `DefaultValueNotLiteral` and accepts the second (spec
    // 80.4.5, JYRO-ADR-2). The table holds its keys weakly, so a program
    // that is no longer referenced takes its entries with it. A node read
    // from an artifact is never marked: a grouped default never validates,
    // so it is never written to one.
    let private groupedDefaults =
        System.Runtime.CompilerServices.ConditionalWeakTable<Expr, obj>()

    let private grouped = obj ()

    // Record that the parameter default `e` was written in parentheses.
    let markGroupedDefault (e: Expr) : unit =
        groupedDefaults.AddOrUpdate(e, grouped)

    // Whether the parameter default `e` was written in parentheses.
    let isGroupedDefault (e: Expr) : bool =
        let mutable found = null
        groupedDefaults.TryGetValue(e, &found)

    let isTerminator = function
        | Return _
        | Fail _
        | Exit _
        | Break _
        | Continue _ -> true
        | VarDecl _
        | Assignment _
        | If _
        | While _
        | ForEach _
        | For _
        | Switch _
        | FuncDef _
        | UnionDef _
        | ExprStmt _
        | Match _
        | Delete _ -> false

    // Extract a function declaration if the statement is one. Used by every
    // stage that filters a program's top-level statements for declarations.
    let tryAsFuncDef = function
        | FuncDef(name, parameters, body, pos) -> Some (name, parameters, body, pos)
        | VarDecl _
        | Assignment _
        | If _
        | While _
        | ForEach _
        | For _
        | Switch _
        | UnionDef _
        | Return _
        | Fail _
        | Exit _
        | Break _
        | Continue _
        | ExprStmt _
        | Match _
        | Delete _ -> None

    let tryAsUnionDef = function
        | UnionDef(name, variants, pos) -> Some (name, variants, pos)
        | VarDecl _
        | Assignment _
        | If _
        | While _
        | ForEach _
        | For _
        | Switch _
        | FuncDef _
        | Return _
        | Fail _
        | Exit _
        | Break _
        | Continue _
        | ExprStmt _
        | Match _
        | Delete _ -> None

    let rec getIdentifiers expr =
        match expr with
        | Identifier(name, _) -> [ name ]
        | Binary(l, _, r, _) -> getIdentifiers l @ getIdentifiers r
        | Unary(_, e, _) -> getIdentifiers e
        | Ternary(c, t, e, _) -> getIdentifiers c @ getIdentifiers t @ getIdentifiers e
        | Call(_, args, _) ->
            match args with
            | PositionalArgs exprs -> exprs |> List.collect getIdentifiers
            | NamedArgs pairs -> pairs |> List.collect (fun (_, expr) -> getIdentifiers expr)
        | PropertyAccess(t, _, _) -> getIdentifiers t
        | IndexAccess(t, i, _) -> getIdentifiers t @ getIdentifiers i
        | ObjectLiteral(props, _) -> props |> List.collect (snd >> getIdentifiers)
        | ArrayLiteral(elems, _) -> elems |> List.collect getIdentifiers
        | Lambda(params', body, _) ->
            let bodyIds = getIdentifiers body
            bodyIds |> List.filter (fun id -> not (List.contains id params'))
        | TypeCheck(e, _, _, _) -> getIdentifiers e
        | IncrementDecrement(e, _, _, _) -> getIdentifiers e
        | MatchExpr(e, cases, _) ->
            let exprIds = getIdentifiers e
            let caseIds = cases |> List.collect (fun c ->
                let bodyIds = getIdentifiers c.Body
                bodyIds |> List.filter (fun id -> not (List.contains id c.Bindings)))
            exprIds @ caseIds
        | Literal _ -> []

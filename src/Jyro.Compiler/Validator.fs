namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast


/// The names registered for a compilation, which the Validate pass checks a
/// script's function declarations against (spec 80.3.2, JYRO-ADR-115,
/// JYRO-ADR-116). Both compilation paths build it: from source with the
/// compiling host's functions and libraries, on a load with the loading
/// host's functions, so the rule is made by the one validator on both.
type internal RegisteredNames =
    { /// Every function name a script function may not take: the inventory of
      /// 90.2 and every name registered for the compilation (host functions,
      /// further standard library functions).
      Reserved: Set<string>
      /// The function names the compilation's libraries declare. A script
      /// function of one of these names is `FunctionShadowsLibrary` (80.3.3),
      /// not `FunctionOverride`.
      LibraryFunctions: Set<string> }

    /// No registered names: a library's own validation, which knows no
    /// compilation. Its declarations are checked against the registered names
    /// and the other libraries by `Validator.checkLibraryDeclarations`, in the
    /// Validate stage of each compilation the library is added to
    /// (JYRO-ADR-115).
    static member None = { Reserved = Set.empty; LibraryFunctions = Set.empty }

/// Semantic validation for Jyro programs
module internal Validator =
    /// The names of the inventory of 90.2: every function the standard
    /// library defines. A script function may not take one of these names
    /// whether or not the host added the standard library (spec 80.3.2,
    /// JYRO-ADR-14).
    let inventoryNames : Set<string> =
        StdlibFunctions.getAll () |> Seq.map (fun f -> f.Name) |> Set.ofSeq

    /// The names a script's functions are checked against in Validate, for a
    /// compilation registering `functions` and declaring
    /// `libraryFunctionNames` in its libraries: the 90.2 inventory and every
    /// registered function name.
    let registeredNames (functions: IJyroFunction seq) (libraryFunctionNames: Set<string>) : RegisteredNames =
        { Reserved = functions |> Seq.map (fun f -> f.Name) |> Set.ofSeq |> Set.union inventoryNames
          LibraryFunctions = libraryFunctionNames }

    /// The source of `Scope.Id`: every scope a validation opens has its own.
    let private lastScopeId = ref 0L
    let private nextScopeId () = System.Threading.Interlocked.Increment(&lastScopeId.contents)

    type Scope =
        { Variables: Set<string>
          /// The names of this scope whose declaration has the type hint
          /// `string` or a string literal as its initial value (spec 40.2.3).
          /// Held per scope, so a name is read at the declaration that 60.3.1
          /// resolves it to, not at any declaration of the same name.
          StringTyped: Set<string>
          Parent: Scope option
          InLoop: bool
          /// This scope's identity: a name and the scope that declares it
          /// identify one declaration, since a scope declares a name once.
          Id: int64 }

        static member Empty = { Variables = Set.empty; StringTyped = Set.empty; Parent = None; InLoop = false; Id = nextScopeId () }

        member this.WithVariable(name: string) =
            { this with Variables = this.Variables.Add(name); StringTyped = this.StringTyped.Remove(name) }

        /// `WithVariable`, recording whether the declaration is string-typed
        /// as 40.2.3 reads it.
        member this.WithVariable(name: string, isStringTyped: bool) =
            if isStringTyped then
                { this with Variables = this.Variables.Add(name); StringTyped = this.StringTyped.Add(name) }
            else this.WithVariable(name)

        member this.Append() =
            { Variables = Set.empty; StringTyped = Set.empty; Parent = Some this; InLoop = this.InLoop; Id = nextScopeId () }

        member this.EnterLoop() =
            { this with InLoop = true }

        member this.IsDeclared(name: string) : bool =
            if this.Variables.Contains(name) then true
            elif this.Parent.IsSome then this.Parent.Value.IsDeclared(name)
            else false

        /// The scope whose declaration `name` resolves to (spec 60.3.1),
        /// parameters included (60.4.3), or none when no declaration in
        /// scope declares it.
        member this.DeclaringScope(name: string) : int64 option =
            if this.Variables.Contains(name) then Some this.Id
            elif this.Parent.IsSome then this.Parent.Value.DeclaringScope(name)
            else None

        /// Whether the declaration `name` resolves to (spec 60.3.1), the
        /// nearest enclosing one, is string-typed (spec 40.2.3). The
        /// declaration alone is read: no later assignment changes it
        /// (JYRO-ADR-7).
        member this.IsStringTyped(name: string) : bool =
            if this.Variables.Contains(name) then this.StringTyped.Contains(name)
            elif this.Parent.IsSome then this.Parent.Value.IsStringTyped(name)
            else false

    type ValidationContext =
        { Scope: Scope
          Messages: DiagnosticMessage list
          DefinedFunctions: Set<string>
          DefinedUnions: Map<string, UnionVariant list>
          VariantToUnion: Map<string, string>
          AssignedNames: Set<string>
          InFunction: bool
          InLambda: bool
          NestingDepth: int
          Registered: RegisteredNames
          /// A library parsed ahead of any compilation (`JyroLibrary.Parse`):
          /// a `match` with a case naming a variant this text does not
          /// declare is resolved in each compilation instead (spec 160.6,
          /// JYRO-ADR-139).
          DeferForeignMatches: bool
          /// Whether a `match` was left to each compilation under
          /// `DeferForeignMatches`: a compilation validates again only a
          /// parsed library that holds one (JYRO-ADR-140 point 1).
          HoldsDeferredMatch: bool }

        static member Empty =
            { Registered = RegisteredNames.None
              DeferForeignMatches = false
              HoldsDeferredMatch = false
              Scope = Scope.Empty
              Messages = []
              DefinedFunctions = Set.ofList [ "Input"; "Output" ]
              DefinedUnions = Map.empty
              VariantToUnion = Map.empty
              AssignedNames = Set.empty
              InFunction = false
              InLambda = false
              NestingDepth = 0 }

        member this.AddError(code: MessageCode, args: obj[], location: SourceLocation) =
            let msg = DiagnosticMessage.Of(code, args = args, location = location)
            { this with Messages = msg :: this.Messages }

        // There is no AddWarning. There was one, with no caller: scaffolding for
        // a validator that would warn about something - unreachable code after a
        // `return`, most likely, which is what `Ast.isTerminator` was written
        // for and which was never built.
        //
        // Deleted rather than left, because an unused emitter reads as "the
        // validator has warnings and none happen to fire here" when the truth is
        // that it has none at all. The severity model itself is not affected and
        // is not dead: `JyroResult.IsSuccess` is derived as "no message of Error
        // severity", so a warning genuinely does not spoil a success, a host can
        // build one through the documented `DiagnosticMessage.Warning`, and both
        // are covered by tests. What the engine does not do is author one.

    /// `Input` and `Output` are supplied by the host, so a script cannot bind
    /// either name - at any of the seven places a script binds one.
    ///
    /// Only named-function parameters used to hold this rule. Everywhere else
    /// the binding was accepted, allocated and assigned, and then never read:
    /// `compileIdentifier` special-cases both names before it consults the
    /// variable table, so the declaration compiled to a dead store and every
    /// later mention still meant the host's value. `var Input = 99` followed by
    /// `Output.d = Input` wrote the host's input object.
    ///
    /// The half-real part is what makes it worth a rule rather than a note: the
    /// validator registered the name in scope, so a *second* `var Input`
    /// reported VariableAlreadyDeclared - the declaration was real enough to
    /// collide with itself and not real enough to read back.
    ///
    /// Written once and called from each site, rather than inlined at each,
    /// because a rule that holds at seven places and is written at seven places
    /// is a rule that will hold at six after the next site is added.
    ///
    /// The names share one position, so a reserved name bound twice there is
    /// reported once, not once per occurrence (JYRO-ADR-126 point 2).
    let private checkBoundNames (ctx: ValidationContext) (names: string seq) (pos: Position) =
        let loc = pos.Location
        names
        |> Seq.filter (fun n -> n = "Input" || n = "Output")
        |> Seq.distinct
        |> Seq.fold
            (fun (c: ValidationContext) n -> c.AddError(MessageCode.ReservedName, [| box n |], loc))
            ctx

    /// The names that occur more than once in `names`, each once, in the order
    /// their second occurrence is reached. Names are compared exactly, as
    /// identifiers are: `x` and `X` differ, and `_` is an ordinary name
    /// (JYRO-ADR-136).
    let private repeatedNames (names: string seq) : string list =
        let seen = System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        let reported = System.Collections.Generic.HashSet<string>(System.StringComparer.Ordinal)
        [ for n in names do
            if not (seen.Add(n)) && reported.Add(n) then yield n ]

    /// A match case binds its names by position to the variant's fields, so a
    /// name given at two positions would hold whichever field was bound last,
    /// an order no clause gives. Each repeated name is reported once, at the
    /// case's position (JYRO-REQ-1684, JYRO-ADR-136, JYRO-ADR-126 point 2).
    let private checkCaseBindings (ctx: ValidationContext) (variantName: string) (bindings: string list) (pos: Position) =
        let loc = pos.Location
        repeatedNames bindings
        |> List.fold
            (fun (c: ValidationContext) n -> c.AddError(MessageCode.MatchDuplicateBinding, [| box n; box variantName |], loc))
            ctx

    /// The repeated-name check for a case that repeats an earlier case's
    /// variant. MatchDuplicateCase does not guard the case's other Validate
    /// checks (JYRO-ADR-115 point 4): the variant and its field count are
    /// known, so MatchDuplicateBinding is reported when the count is right,
    /// as JYRO-ADR-136 orders it (JYRO-REQ-1684, JYRO-DEF-564).
    let private checkRepeatedCaseBindings (ctx: ValidationContext) (variantName: string) (bindings: string list) (pos: Position) =
        let variantDefOpt =
            ctx.VariantToUnion.TryFind(variantName)
            |> Option.bind (fun u -> ctx.DefinedUnions.TryFind(u))
            |> Option.bind (List.tryFind (fun v -> v.Name = variantName))
        match variantDefOpt with
        | Some variantDef when bindings.Length = variantDef.Fields.Length ->
            checkCaseBindings ctx variantName bindings pos
        | _ -> ctx

    /// The checks of a match case's head, in the statement and expression
    /// forms alike: `MatchWithDefault`, `MatchDuplicateCase`,
    /// `MatchUnknownVariant` or `MatchBindingCountMismatch`, and
    /// `MatchDuplicateBinding` when the variant and its field count are
    /// known and right (JYRO-ADR-136); and `ReservedName` for its names,
    /// whatever else it reports. `seenCases` gains the case's variant. A
    /// rejection of the case does not guard its other checks, and the caller
    /// validates its body whatever this reports (JYRO-ADR-115 point 4,
    /// JYRO-DEF-566).
    let private checkCaseHead
        (ctx: ValidationContext)
        (matchUnion: string option)
        (seenCases: System.Collections.Generic.HashSet<string>)
        (variantName: string)
        (bindings: string list)
        (pos: Position)
        : ValidationContext =
        let caseLoc = pos.Location
        if variantName = "default" then
            let c = ctx.AddError(MessageCode.MatchWithDefault, Array.empty, caseLoc)
            checkBoundNames c bindings pos
        elif not (seenCases.Add(variantName)) then
            let c = ctx.AddError(MessageCode.MatchDuplicateCase, [| box variantName |], caseLoc)
            let c = checkBoundNames c bindings pos
            checkRepeatedCaseBindings c variantName bindings pos
        else
            // Defensive tryFind: if any future bug lets VariantToUnion point
            // at a union whose variant list doesn't contain the variant,
            // surface it as a diagnostic instead of throwing. A variant of a
            // union other than the match's is unknown to the match too
            // (spec 50.9.3, JYRO-REQ-1690).
            let variantDefOpt =
                ctx.VariantToUnion.TryFind(variantName)
                |> Option.filter (fun u -> matchUnion.IsNone || matchUnion = Some u)
                |> Option.bind (fun u -> ctx.DefinedUnions.TryFind(u))
                |> Option.bind (List.tryFind (fun v -> v.Name = variantName))
            match variantDefOpt with
            | None ->
                let c = ctx.AddError(MessageCode.MatchUnknownVariant, [| box variantName |], caseLoc)
                checkBoundNames c bindings pos
            | Some variantDef when bindings.Length <> variantDef.Fields.Length ->
                let c =
                    ctx.AddError(MessageCode.MatchBindingCountMismatch,
                        [| box variantName; box bindings.Length; box variantDef.Fields.Length |], caseLoc)
                checkBoundNames c bindings pos
            | Some _ ->
                let c = checkBoundNames ctx bindings pos
                checkCaseBindings c variantName bindings pos

    /// The union of a match: that of its first case naming a declared
    /// variant, a variant declared in any source text of the compilation
    /// (spec 50.9.3, JYRO-REQ-1690, JYRO-ADR-121). `default` names none.
    let private unionOfMatch (ctx: ValidationContext) (variantNames: string list) : string option =
        variantNames
        |> List.tryPick (fun n -> if n = "default" then None else ctx.VariantToUnion.TryFind(n))

    /// The arguments of a `MatchNotExhaustive`: the union's name, then each
    /// variant that has no case, each its own argument and in the order of
    /// their names, never joined into one (spec 130.6.2).
    let private matchNotExhaustiveArgs (unionName: string) (missing: Set<string>) : obj[] =
        Array.append [| box unionName |] (missing |> Set.toArray |> Array.map box)

    /// The codes of a `match` that read the union the match resolves to,
    /// which a library parsed ahead leaves to each compilation when a case
    /// names a variant it does not declare (spec 160.6, JYRO-REQ-1698,
    /// JYRO-ADR-139 point 2).
    let matchResolutionCodes : Set<MessageCode> =
        Set.ofList
            [ MessageCode.MatchUnknownVariant
              MessageCode.MatchBindingCountMismatch
              MessageCode.MatchDuplicateBinding
              MessageCode.MatchNotExhaustive ]

    /// Whether the checks of a `match` with these case names wait for the
    /// compilation: in a library parsed ahead, a case names a variant the
    /// library does not declare (JYRO-ADR-139 point 2).
    let private isDeferredMatch (ctx: ValidationContext) (variantNames: string list) : bool =
        ctx.DeferForeignMatches
        && variantNames |> List.exists (fun n -> n <> "default" && not (ctx.VariantToUnion.ContainsKey(n)))

    /// `after`, with the diagnostics added since `before` that resolve a
    /// deferred match's union removed; the others, such as
    /// `MatchDuplicateCase` or `ReservedName`, read the library alone and
    /// stay (JYRO-ADR-139 point 1).
    let private withoutResolution (before: ValidationContext) (after: ValidationContext) : ValidationContext =
        let added = after.Messages |> List.take (after.Messages.Length - before.Messages.Length)
        let kept = added |> List.filter (fun m -> not (matchResolutionCodes.Contains(m.Code)))
        { after with Messages = kept @ before.Messages }

    /// Whether `e` is an operand that 40.2.3 rejects in a `+`: a string
    /// literal, or an identifier whose declaration, as 60.3.1 resolves it,
    /// has the type hint `string` or a string literal as its initial value.
    /// A parameter's default value is not an initial value (40.2.7).
    let private isStringOperand (ctx: ValidationContext) (e: Expr) : bool =
        match e with
        | Literal(v, _) -> v.ValueType = JyroValueType.String
        | Identifier(name, _) -> ctx.Scope.IsStringTyped(name)
        | Binary _
        | Unary _
        | Ternary _
        | Call _
        | PropertyAccess _
        | IndexAccess _
        | ObjectLiteral _
        | ArrayLiteral _
        | Lambda _
        | TypeCheck _
        | IncrementDecrement _
        | MatchExpr _ -> false

    /// Whether a declaration with this hint and initial value is
    /// string-typed for 40.2.3.
    let private declaresString (typeHint: JyroType option) (init: Expr option) : bool =
        match typeHint, init with
        | Some StringType, _ -> true
        | _, Some (Literal(v, _)) -> v.ValueType = JyroValueType.String
        | _ -> false

    /// Whether a parsed node was written in parentheses inside the node
    /// that holds it. The parser gives a grouped expression the range of
    /// its text without the parentheses, and the node that holds it the
    /// range with them (spec 150.4.4), so the held node starts after the
    /// holder (`atStart`) or ends before it. A node with no range (one a
    /// host built) is never grouped.
    let private isGroupedIn (holder: Position) (held: Position) (atStart: bool) : bool =
        let hasRange (p: Position) = p.EndIndex > p.StartIndex
        hasRange holder && hasRange held
        && (if atStart then held.StartIndex > holder.StartIndex else held.EndIndex < holder.EndIndex)

    let rec validateExpr (ctx: ValidationContext) (expr: Expr) : ValidationContext =
        match expr with
        | Identifier(name, pos) when name = "Output" && (ctx.InFunction || ctx.InLambda) ->
            let loc = pos.Location
            ctx.AddError(MessageCode.OutputAccessInFunction,
                Array.empty, loc)
        | Identifier(name, pos) when name <> "Input" && name <> "Output" && not (ctx.Scope.IsDeclared(name)) ->
            let loc = pos.Location
            ctx.AddError(MessageCode.UndeclaredVariable, [| box name |], loc)
        | Binary(left, Add, right, pos) ->
            let ctx' = ctx |> validateExpr <| left |> validateExpr <| right
            if isStringOperand ctx left || isStringOperand ctx right then
                let loc = pos.Location
                ctx'.AddError(MessageCode.StringOperandInAddition,
                    Array.empty, loc)
            else ctx'
        | Binary(left, _, right, _) ->
            ctx |> validateExpr <| left |> validateExpr <| right
        | Unary(_, operand, _) ->
            ctx |> validateExpr <| operand
        | Ternary(cond, thenExpr, elseExpr, _) ->
            ctx |> validateExpr <| cond |> validateExpr <| thenExpr |> validateExpr <| elseExpr
        | Call(_, args, _) ->
            match args with
            | PositionalArgs exprs -> exprs |> List.fold validateExpr ctx
            | NamedArgs pairs -> pairs |> List.fold (fun c (_, expr) -> validateExpr c expr) ctx
        | PropertyAccess(target, _, _) ->
            ctx |> validateExpr <| target
        | IndexAccess(target, index, _) ->
            ctx |> validateExpr <| target |> validateExpr <| index
        | ObjectLiteral(props, _) ->
            props |> List.fold (fun c (_, e) -> validateExpr c e) ctx
        | ArrayLiteral(elems, _) ->
            elems |> List.fold validateExpr ctx
        | Lambda(params', body, pos) ->
            let ctx = checkBoundNames ctx params' pos
            // A second parameter of a name would hide the first, so one
            // argument could never be read (spec 40.10, JYRO-REQ-177). Each
            // repeated name is reported once, at the lambda's position; a
            // lambda has no name, so the diagnostic has no argument for one
            // (spec 130.6.2).
            let ctx =
                repeatedNames params'
                |> List.fold
                    (fun (c: ValidationContext) n -> c.AddError(MessageCode.DuplicateParameterName, [| box n |], pos.Location))
                    ctx
            // InLambda fires the Output-access guard at the top of validateExpr
            // even when the surrounding context is the top-level scope. Without
            // this a top-level lambda could capture Output and, on being passed
            // into a function, give the function indirect access - bypassing
            // 50.8 / 80.x's explicit-parameter rule.
            let innerCtx =
                { ctx with
                    Scope = params' |> List.fold (fun s p -> s.WithVariable(p)) (ctx.Scope.Append())
                    InLambda = true }
            let validated = validateExpr innerCtx body
            { ctx with Messages = validated.Messages; HoldsDeferredMatch = validated.HoldsDeferredMatch }
        | TypeCheck(e, _, _, _) ->
            ctx |> validateExpr <| e
        | IncrementDecrement(e, _, isPrefix, pos) ->
            // `x++` writes to `x`, so it answers to the same two rules as an
            // assignment: the target must be assignable, and it must not be
            // rooted at Input. Only recursing into the operand let `5++` and
            // `Input.a++` through to the compiler, which had no diagnostic for
            // either and reported them as internal compiler errors.
            let ctx' = validateExpr ctx e
            let loc = pos.Location
            // `s++` performs `s = s + 1` and `s--` performs `s = s - 1`
            // (40.8.4), so a string-declared `s` is refused as 40.2.3 refuses
            // the `+`, for `--` too (spec 40.2.9, JYRO-ADR-47).
            let ctx' =
                match e with
                | Identifier _ when isStringOperand ctx e ->
                    ctx'.AddError(MessageCode.StringOperandInAddition, Array.empty, loc)
                | _ -> ctx'
            // Parentheses around the whole target make it a grouped
            // expression, which 40.7.1 does not permit: `(x)++` (40.8.2).
            if Ast.targetRoot e = Some "Input" then
                ctx'.AddError(MessageCode.InputIsReadOnly, Array.empty, loc)
            elif not (Ast.isAssignmentTarget e) || isGroupedIn pos e.Position (not isPrefix) then
                ctx'.AddError(MessageCode.InvalidIncrementTarget, Array.empty, loc)
            else
                ctx'
        | MatchExpr(expr, cases, pos) ->
            let ctx' = validateExpr ctx expr
            let loc = pos.Location
            let nonDefaultCases = cases |> List.filter (fun mc -> mc.VariantName <> "default")
            let unionNameOpt = unionOfMatch ctx' (cases |> List.map (fun mc -> mc.VariantName))
            let deferred = isDeferredMatch ctx' (cases |> List.map (fun mc -> mc.VariantName))
            let ctx' = if deferred then { ctx' with HoldsDeferredMatch = true } else ctx'
            // Validate each case
            let seenCases = System.Collections.Generic.HashSet<string>()
            let ctx' =
                cases |> List.fold (fun (c: ValidationContext) (mc: MatchExprCase) ->
                    let checkedHead = checkCaseHead c unionNameOpt seenCases mc.VariantName mc.Bindings mc.Pos
                    let c = if deferred then withoutResolution c checkedHead else checkedHead
                    // A rejected case's body is validated as an accepted
                    // one's is, with its names bound as written, which needs
                    // no variant (JYRO-ADR-115 point 4, JYRO-DEF-566).
                    let bodyScope = mc.Bindings |> List.fold (fun (s: Scope) b -> s.WithVariable(b)) (c.Scope.Append())
                    let bodyCtx = validateExpr { c with Scope = bodyScope } mc.Body
                    { bodyCtx with Scope = c.Scope }
                ) ctx'
            // Exhaustiveness check (skip if the union can't be resolved - a
            // stale VariantToUnion entry would otherwise crash on
            // DefinedUnions.[unionName]). A `default` case names no variant,
            // so it covers none: a match with one and a missing variant is
            // both MatchWithDefault and MatchNotExhaustive (spec 50.9.4,
            // JYRO-REQ-229). A deferred match is checked in each compilation
            // instead (JYRO-ADR-139).
            if deferred then ctx' else
            match unionNameOpt |> Option.bind (fun u -> ctx'.DefinedUnions.TryFind(u) |> Option.map (fun vs -> u, vs)) with
            | Some (unionName, variants) ->
                let allVariants = variants |> List.map (fun v -> v.Name) |> Set.ofList
                let coveredVariants = nonDefaultCases |> List.map (fun c -> c.VariantName) |> Set.ofList
                let missing = Set.difference allVariants coveredVariants
                if not missing.IsEmpty then
                    ctx'.AddError(MessageCode.MatchNotExhaustive, matchNotExhaustiveArgs unionName missing, loc)
                else ctx'
            | None -> ctx'
        // Identifier and Literal: the only Expr cases validateExpr does not
        // recurse into. The Identifier branches above are guarded and only
        // fire on Output-in-function or undeclared-variable; this case
        // catches every other Identifier (Input, Output-outside-function,
        // declared variable) plus all literals.
        | Identifier _
        | Literal _ -> ctx

    let rec validateStmt (ctx: ValidationContext) (stmt: Stmt) : ValidationContext =
        // Each statement, nested ones included, is where an abandoned
        // compilation stops (JYRO-ADR-100).
        CompilationStage.CheckAmbient()
        match stmt with
        | VarDecl(name, typeHint, init, pos) ->
            let ctx' =
                match init with
                | Some expr -> validateExpr ctx expr
                | None -> ctx
            let ctx' = checkBoundNames ctx' [ name ] pos
            let ctx' =
                match typeHint, init with
                | Some hint, None when hint <> AnyType ->
                    // The place is the VarDecl (spec 130.1.2), so the length
                    // is the declaration's, as 160.7.10 reads it from
                    // `SourceLocation.Length`.
                    let loc = pos.Location
                    ctx'.AddError(MessageCode.TypedVariableNeedsInitializer, [| box name |], loc)
                | _ -> ctx'
            if ctx'.Scope.Variables.Contains(name) then
                let loc = pos.Location
                ctx'.AddError(MessageCode.VariableAlreadyDeclared, [| box name |], loc)
            else
                { ctx' with Scope = ctx'.Scope.WithVariable(name, declaresString typeHint init) }
        | Assignment(target, op, value, typeHint, pos) ->
            let ctx' = validateExpr ctx target |> validateExpr <| value
            // `t += v` is tested as the `t + v` it performs (spec 40.2.8,
            // JYRO-ADR-44).
            let ctx' =
                if op = AddAssign && (isStringOperand ctx target || isStringOperand ctx value) then
                    ctx'.AddError(MessageCode.StringOperandInAddition, Array.empty, pos.Location)
                else ctx'
            let ctx' =
                match typeHint with
                | Some _ when op <> Assign ->
                    let loc = pos.Location
                    ctx'.AddError(MessageCode.TypeHintOnCompoundAssignment,
                        Array.empty, loc)
                | _ -> ctx'
            // A target in parentheses is a grouped expression, not a name,
            // so it names no hinted variable or property.
            let grouped = isGroupedIn pos target.Position true
            // DuplicateTypeHint (spec 50.2.1, JYRO-ADR-8, JYRO-ADR-36). The
            // property `name` of the variable `x`, for `x.name` and
            // `x["name"]` with a string literal: the variable is the
            // declaration 60.3.1 resolves `x` to, or `Output`, which no
            // declaration declares. The key names the variable by its
            // declaring scope, so a variable of the same name that another
            // declaration declares has other keys.
            let propertyKey (variable: string) (prop: string) : string option =
                let owner =
                    match ctx.Scope.DeclaringScope(variable) with
                    | Some scopeId -> Some (string scopeId + ":" + variable)
                    | None when variable = "Output" || variable = "Input" -> Some variable
                    | None -> None
                owner |> Option.map (fun o -> o + "." + prop)
            let hintedName, propKey =
                match target with
                | _ when grouped -> None, None
                | Identifier(name, _) -> Some name, None
                | PropertyAccess(Identifier(obj, _), prop, _) -> None, propertyKey obj prop
                | IndexAccess(Identifier(obj, _), Literal(key, _), _) when key.ValueType = JyroValueType.String ->
                    None, propertyKey obj ((key :?> JyroString).Value)
                // A deeper target, an index that is not a string literal,
                // and every other Expr shape are never DuplicateTypeHint.
                | PropertyAccess _
                | IndexAccess _
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
                | MatchExpr _ -> None, None
            // Checked for every operator: a hint on a compound assignment to
            // a target that already has a value is DuplicateTypeHint (spec
            // 50.2.1) as well as TypeHintOnCompoundAssignment (30.12.10);
            // both rules reject it and no decision makes one give way.
            let ctx' =
                match typeHint, hintedName, propKey with
                // A declared identifier already has its value, and its type
                // hint belongs on its declaration. An undeclared one is
                // UndeclaredVariable, from the target's own validation.
                | Some _, Some name, _ when ctx.Scope.IsDeclared(name) ->
                    ctx'.AddError(MessageCode.DuplicateTypeHint, [| box name |], pos.Location)
                // An earlier assignment to the same property of the same
                // variable, in the order of the source text, whether or not
                // it runs.
                | Some _, _, Some key when ctx'.AssignedNames.Contains(key) ->
                    let shown =
                        match target with
                        | PropertyAccess(Identifier(obj, _), prop, _) -> obj + "." + prop
                        | IndexAccess(Identifier(obj, _), Literal(k, _), _) -> obj + "." + (k :?> JyroString).Value
                        | _ -> key
                    ctx'.AddError(MessageCode.DuplicateTypeHint, [| box shown |], pos.Location)
                | _ -> ctx'
            let ctx' =
                match propKey with
                | Some key -> { ctx' with AssignedNames = ctx'.AssignedNames.Add(key) }
                | None -> ctx'
            // Root the target rather than matching its outermost shape: a write
            // at ANY depth below Input mutates the host's own graph, and the
            // host is entitled to hand the same value to another execution.
            if Ast.targetRoot target = Some "Input" then
                let loc = pos.Location
                ctx'.AddError(MessageCode.InputIsReadOnly,
                    Array.empty, loc)
            // Parentheses around the whole target make it a grouped
            // expression, not one of the three forms 40.7.1 permits:
            // `(a) = 1` is refused, `(a).x = 1` is not (JYRO-ADR-2).
            elif not (Ast.isAssignmentTarget target) || grouped then
                let loc = pos.Location
                ctx'.AddError(MessageCode.InvalidAssignmentTarget, Array.empty, loc)
            else
                ctx'
        | If(cond, thenBlock, elseIfs, elseBlock, _) ->
            let ctx' = validateExpr ctx cond
            let nested = { ctx' with NestingDepth = ctx'.NestingDepth + 1 }
            let thenCtx = thenBlock |> List.fold validateStmt { nested with Scope = ctx'.Scope.Append() }
            let ctx'' = { thenCtx with Scope = ctx'.Scope; NestingDepth = ctx'.NestingDepth }
            let ctx''' = elseIfs |> List.fold (fun c (e, stmts) ->
                let c' = validateExpr c e
                let nestedC = { c' with NestingDepth = c'.NestingDepth + 1 }
                let bodyCtx = stmts |> List.fold validateStmt { nestedC with Scope = c'.Scope.Append() }
                { bodyCtx with Scope = c'.Scope; NestingDepth = c'.NestingDepth }) ctx''
            match elseBlock with
            | Some stmts ->
                let nestedElse = { ctx''' with NestingDepth = ctx'''.NestingDepth + 1 }
                let bodyCtx = stmts |> List.fold validateStmt { nestedElse with Scope = ctx'''.Scope.Append() }
                { bodyCtx with Scope = ctx'''.Scope; NestingDepth = ctx'''.NestingDepth }
            | None -> ctx'''
        | While(cond, body, _) ->
            let ctx' = validateExpr ctx cond
            let loopCtx = { ctx' with Scope = ctx'.Scope.Append().EnterLoop(); NestingDepth = ctx'.NestingDepth + 1 }
            let bodyCtx = body |> List.fold validateStmt loopCtx
            { bodyCtx with Scope = ctx'.Scope; NestingDepth = ctx'.NestingDepth }
        | ForEach(varName, collection, body, pos) ->
            let ctx = checkBoundNames ctx [ varName ] pos
            let ctx' = validateExpr ctx collection
            let ctx' =
                match collection with
                | Literal(v, pos) when v.ValueType = JyroValueType.Null
                                    || v.ValueType = JyroValueType.Number
                                    || v.ValueType = JyroValueType.Boolean ->
                    let loc = pos.Location
                    ctx'.AddError(MessageCode.NotIterableLiteral, [| box (TypeNames.typeOfName v.ValueType) |], loc)
                | _ -> ctx'
            let loopScope = ctx'.Scope.Append().EnterLoop().WithVariable(varName)
            let loopCtx = { ctx' with Scope = loopScope; NestingDepth = ctx'.NestingDepth + 1 }
            let bodyCtx = body |> List.fold validateStmt loopCtx
            { bodyCtx with Scope = ctx'.Scope; NestingDepth = ctx'.NestingDepth }
        | For(varName, startExpr, endExpr, stepExpr, _, body, pos) ->
            let ctx = checkBoundNames ctx [ varName ] pos
            let ctx' = validateExpr ctx startExpr
            let ctx' = validateExpr ctx' endExpr
            let ctx' = match stepExpr with Some e -> validateExpr ctx' e | None -> ctx'
            let loopScope = ctx'.Scope.Append().EnterLoop().WithVariable(varName)
            let bodyCtx = body |> List.fold validateStmt { ctx' with Scope = loopScope; NestingDepth = ctx'.NestingDepth + 1 }
            { bodyCtx with Scope = ctx'.Scope; NestingDepth = ctx'.NestingDepth }
        | Switch(expr, cases, defaultCase, _) ->
            let ctx' = validateExpr ctx expr
            let ctx'' = cases |> List.fold (fun c case ->
                let c' = case.Values |> List.fold validateExpr c
                let nestedC = { c' with NestingDepth = c'.NestingDepth + 1 }
                let bodyCtx = case.Body |> List.fold validateStmt { nestedC with Scope = c'.Scope.Append() }
                { bodyCtx with Scope = c'.Scope; NestingDepth = c'.NestingDepth }) ctx'
            match defaultCase with
            | Some stmts ->
                let nestedDef = { ctx'' with NestingDepth = ctx''.NestingDepth + 1 }
                let bodyCtx = stmts |> List.fold validateStmt { nestedDef with Scope = ctx''.Scope.Append() }
                { bodyCtx with Scope = ctx''.Scope; NestingDepth = ctx''.NestingDepth }
            | None -> ctx''
        | FuncDef(name, parameters, body, pos) ->
            let loc = pos.Location
            // A function in a function body, at any depth of blocks in it, is
            // `NestedFunctionDeclaration` only (spec 80.1.3); one in a block
            // outside every function is `FunctionNotAtTopLevel` (80.1.2). One
            // declaration gets one diagnostic (JYRO-ADR-10).
            let ctx' =
                if ctx.InFunction then
                    ctx.AddError(MessageCode.NestedFunctionDeclaration,
                        Array.empty, loc)
                elif ctx.NestingDepth > 0 then
                    ctx.AddError(MessageCode.FunctionNotAtTopLevel,
                        [| box name |], loc)
                else ctx
            // Check for duplicate parameter names: once per repeated name,
            // since every repeat is at the function's position with the same
            // arguments (JYRO-ADR-126 point 2).
            let ctx' =
                parameters |> List.fold (fun (c: ValidationContext, seen: Set<string>, reported: Set<string>) (p: FuncParam) ->
                    if seen.Contains(p.Name) then
                        if reported.Contains(p.Name) then (c, seen, reported)
                        else
                            let c' = c.AddError(MessageCode.DuplicateParameterName,
                                [| box p.Name; box name |], loc)
                            (c', seen, reported.Add(p.Name))
                    elif p.Name = "Input" || p.Name = "Output" then
                        let c' = checkBoundNames c [ p.Name ] pos
                        (c', seen.Add(p.Name), reported)
                    else
                        (c, seen.Add(p.Name), reported)
                ) (ctx', Set.empty, Set.empty) |> fun (c, _, _) -> c
            // Check that required params come before optional params
            let ctx' =
                parameters |> List.fold (fun (c: ValidationContext, lastOptional: string option) (p: FuncParam) ->
                    match p.DefaultValue, lastOptional with
                    | None, Some optName ->
                        let c' = c.AddError(MessageCode.RequiredParamAfterOptional,
                            [| box p.Name; box optName; box name |], loc)
                        (c', lastOptional)
                    | Some _, _ -> (c, Some p.Name)
                    | None, None -> (c, None)
                ) (ctx', None) |> fst
            // Check that default values are literals only (spec 80.4.5,
            // JYRO-ADR-2). A negative number counts: the parser reads `-1` as
            // Negate applied to the literal 1, of any radix. A default in
            // parentheses is an expression, not a literal: `(1)`, `(-1)`, and
            // `-(1)`, whose literal ends before the negation does.
            let ctx' =
                parameters |> List.fold (fun (c: ValidationContext) (p: FuncParam) ->
                    match p.DefaultValue with
                    | Some d when Ast.isGroupedDefault d ->
                        c.AddError(MessageCode.DefaultValueNotLiteral,
                            [| box p.Name; box name |], loc)
                    | Some (Literal _) -> c
                    | Some (Unary(Negate, (Literal(v, litPos)), negPos))
                        when v.ValueType = JyroValueType.Number && not (isGroupedIn negPos litPos false) -> c
                    | Some _ ->
                        c.AddError(MessageCode.DefaultValueNotLiteral,
                            [| box p.Name; box name |], loc)
                    | None -> c
                ) ctx'
            // A function named after a function of a library script is
            // `FunctionShadowsLibrary` (spec 80.3.3); one named after a 90.2
            // inventory function or a function registered for the compilation
            // is `FunctionOverride` (80.3.2). Both are Validate codes made
            // here, on source and on load, so another validation error does
            // not hide them (JYRO-ADR-115, JYRO-ADR-116).
            let ctx' =
                if ctx'.Registered.LibraryFunctions.Contains(name) then
                    ctx'.AddError(MessageCode.FunctionShadowsLibrary, [| box name |], loc)
                elif ctx'.Registered.Reserved.Contains(name) then
                    ctx'.AddError(MessageCode.FunctionOverride, [| box name |], loc)
                else ctx'
            // Track function name for duplicate detection across the program.
            // A second declaration of a function is `DuplicateFunction` (spec
            // 80.3.1), in a script and within one library alike. `Input` and
            // `Output` seed the set without being functions: a `func` named
            // after either binds a reserved name, `ReservedName` (spec
            // 60.4, JYRO-REQ-264). A declaration named after a registered
            // function and declared twice is both.
            let ctx' =
                if name = "Input" || name = "Output" then
                    checkBoundNames ctx' [ name ] pos
                elif ctx'.DefinedFunctions.Contains(name) then
                    ctx'.AddError(MessageCode.DuplicateFunction, [| box name |], loc)
                else
                    { ctx' with DefinedFunctions = ctx'.DefinedFunctions.Add(name) }
            // Validate function body in a new scope with parameters declared,
            // InFunction = true. A parameter is string-typed by its hint
            // alone: its default value is not an initial value (spec 40.2.7,
            // JYRO-ADR-44).
            let funcScope =
                parameters
                |> List.fold
                    (fun (s: Scope) (p: FuncParam) -> s.WithVariable(p.Name, (p.TypeHint = Some StringType)))
                    Scope.Empty
            let funcCtx =
                { ctx' with
                    Scope = funcScope
                    InFunction = true
                    NestingDepth = 0 }
            let bodyCtx = body |> List.fold validateStmt funcCtx
            // Restore outer context with accumulated messages, and whether a
            // match was left to the compilation (JYRO-ADR-140 point 1)
            { ctx' with Messages = bodyCtx.Messages; HoldsDeferredMatch = bodyCtx.HoldsDeferredMatch }
        | Return(valueOpt, pos) ->
            if not ctx.InFunction then
                let loc = pos.Location
                let ctx' = ctx.AddError(MessageCode.ReturnOutsideFunction,
                    Array.empty, loc)
                match valueOpt with
                | Some expr -> validateExpr ctx' expr
                | None -> ctx'
            else
                match valueOpt with
                | Some expr -> validateExpr ctx expr
                | None -> ctx
        | Exit(valueOpt, _) ->
            match valueOpt with
            | Some expr -> validateExpr ctx expr
            | None -> ctx
        | Fail(msgOpt, _) ->
            match msgOpt with
            | Some expr -> validateExpr ctx expr
            | None -> ctx
        | Break(pos) ->
            if not ctx.Scope.InLoop then
                let loc = pos.Location
                ctx.AddError(MessageCode.LoopStatementOutsideOfLoop, [| box "break" |], loc)
            else
                ctx
        | Continue(pos) ->
            if not ctx.Scope.InLoop then
                let loc = pos.Location
                ctx.AddError(MessageCode.LoopStatementOutsideOfLoop, [| box "continue" |], loc)
            else
                ctx
        | Delete(target, pos) ->
            let ctx' = validateExpr ctx target
            // A delete is a mutation, so it is refused on the same terms as an
            // assignment: rooted at Input, at any depth.
            if Ast.targetRoot target = Some "Input" then
                let loc = pos.Location
                ctx'.AddError(MessageCode.InputIsReadOnly,
                    Array.empty, loc)
            // Parentheses around the whole target make it a grouped
            // expression, whose outermost form is neither a property access
            // nor an index access: `delete (o.a)` (spec 50.12.1,
            // JYRO-REQ-254). The statement starts at its keyword, so the
            // grouping is seen at the target's end.
            elif not (Ast.isDeleteTarget target) || isGroupedIn pos target.Position false then
                let loc = pos.Location
                ctx'.AddError(MessageCode.InvalidDeleteTarget, Array.empty, loc)
            else
                ctx'
        | ExprStmt(expr, _) ->
            validateExpr ctx expr
        | UnionDef(name, _, pos) ->
            let loc = pos.Location
            // A union in a function body, at any depth of blocks in it, is
            // `UnionInsideFunction` only; one in a block outside every
            // function is `UnionNotAtTopLevel` (spec 50.10.2, JYRO-REQ-239).
            if ctx.InFunction then
                ctx.AddError(MessageCode.UnionInsideFunction,
                    Array.empty, loc)
            elif ctx.NestingDepth > 0 then
                ctx.AddError(MessageCode.UnionNotAtTopLevel,
                    [| box name |], loc)
            else ctx
        | Match(expr, cases, pos) ->
            let ctx' = validateExpr ctx expr
            let loc = pos.Location
            // Non-default cases drive the union-lookup and exhaustiveness checks.
            let nonDefaultCases = cases |> List.filter (fun mc -> mc.VariantName <> "default")
            let unionNameOpt = unionOfMatch ctx' (cases |> List.map (fun mc -> mc.VariantName))
            let deferred = isDeferredMatch ctx' (cases |> List.map (fun mc -> mc.VariantName))
            let ctx' = if deferred then { ctx' with HoldsDeferredMatch = true } else ctx'
            // Validate each case
            let seenCases = System.Collections.Generic.HashSet<string>()
            let ctx' =
                cases |> List.fold (fun (c: ValidationContext) (mc: MatchCase) ->
                    let checkedHead = checkCaseHead c unionNameOpt seenCases mc.VariantName mc.Bindings mc.Pos
                    let c = if deferred then withoutResolution c checkedHead else checkedHead
                    // A rejected case's body is validated too (JYRO-ADR-115
                    // point 4, JYRO-DEF-566); see MatchExpr above.
                    let bodyScope = mc.Bindings |> List.fold (fun (s: Scope) b -> s.WithVariable(b)) (c.Scope.Append())
                    let bodyCtx = mc.Body |> List.fold validateStmt { c with Scope = bodyScope; NestingDepth = c.NestingDepth + 1 }
                    { bodyCtx with Scope = c.Scope; NestingDepth = c.NestingDepth }
                ) ctx'
            // Exhaustiveness check (skip if the union can't be resolved - same
            // defensive pattern as MatchExpr above). A `default` case names no
            // variant, so it covers none (spec 50.9.4).
            // A deferred match is checked in each compilation instead.
            if deferred then ctx' else
            match unionNameOpt |> Option.bind (fun u -> ctx'.DefinedUnions.TryFind(u) |> Option.map (fun vs -> u, vs)) with
            | Some (unionName, variants) ->
                let allVariants = variants |> List.map (fun v -> v.Name) |> Set.ofList
                let coveredVariants = nonDefaultCases |> List.map (fun c -> c.VariantName) |> Set.ofList
                let missing = Set.difference allVariants coveredVariants
                if not missing.IsEmpty then
                    ctx'.AddError(MessageCode.MatchNotExhaustive, matchNotExhaustiveArgs unionName missing, loc)
                else ctx'
            | None -> ctx'

    /// The unions of a compilation, each name to its variants, and each
    /// variant name to its union: the first declaration of a name is kept.
    type UnionTable =
        { Unions: Map<string, UnionVariant list>
          VariantToUnion: Map<string, string> }

        static member Empty = { Unions = Map.empty; VariantToUnion = Map.empty }

    /// The unions one source text's validation sees. A union declared in any
    /// source text of a compilation is in scope in all of them (spec 160.6,
    /// JYRO-ADR-121): `Earlier` holds the unions of the source texts before
    /// this one, in the order of 130.5.4 (each library script in the order
    /// the host added it, then the script), against which a repeated union
    /// or variant name is checked (50.10.3, 50.10.4); `All` holds the unions
    /// of every source text, which a `match` case may name (50.9.3).
    type UnionScope =
        { Earlier: UnionTable
          All: UnionTable }

        /// One source text alone: a library's own validation, or a load.
        static member None = { Earlier = UnionTable.Empty; All = UnionTable.Empty }

    /// The union pre-pass of one source text: the checks of each top-level
    /// union declaration, and its registration in `ctx`'s unions, which hold
    /// those of the earlier source texts of the compilation.
    let private declareUnions (ctx: ValidationContext) (statements: Stmt list) : ValidationContext =
            statements |> List.fold (fun (c: ValidationContext) stmt ->
                CompilationStage.CheckAmbient()
                match stmt with
                | UnionDef(name, variants, pos) ->
                    let loc = pos.Location
                    // A variant builds one property per field, and a case
                    // reads each field by its name, so a second field of a
                    // name already declared would be lost. A type hint does
                    // not change the name: A(x, x: number) repeats x
                    // (JYRO-REQ-1683, JYRO-ADR-136). Each (field, variant)
                    // pair is reported once at the union's position, so a
                    // repeated variant does not repeat the diagnostic
                    // (JYRO-ADR-126 point 2). The check reads only the
                    // variants' own fields, so it runs whatever the union is
                    // called: a repeated union name skips only the
                    // registration below, not this check (JYRO-ADR-115
                    // point 4, JYRO-DEF-563).
                    let c =
                        variants
                        |> List.collect (fun v ->
                            repeatedNames (v.Fields |> List.map fst) |> List.map (fun f -> f, v.Name))
                        |> List.distinct
                        |> List.fold (fun (c2: ValidationContext) (fieldName, variantName) ->
                            c2.AddError(MessageCode.DuplicateFieldName, [| box fieldName; box variantName |], loc)
                        ) c
                    let repeatedUnion = c.DefinedUnions.ContainsKey(name)
                    let c =
                        if repeatedUnion then
                            c.AddError(MessageCode.DuplicateUnionName,
                                [| box name |], loc)
                        else c
                    // The checks below read only this union's variants and
                    // the unions registered before it, so they run whatever
                    // the union is called: DuplicateUnionName does not guard
                    // them (JYRO-ADR-115 point 4, JYRO-DEF-565).
                    let c =
                        // A variant constructor is a callable the script
                        // introduces, so it answers to the reserved-name rule
                        // like the other six binding sites. It was the one that
                        // got missed: `union U Input(x) end` compiled, and then
                        // `Input(1)` constructed the variant while `Input.marker`
                        // in the same script still read the host's value - one
                        // name meaning two things in one scope, with nothing
                        // reported. `func Input()` was already refused, so the
                        // two halves of one namespace disagreed.
                        let c' = checkBoundNames c (variants |> List.map (fun v -> v.Name)) pos
                        // A constructed variant is an object whose `_variant`
                        // property is its tag. A field of that name overwrote
                        // the tag, so `C("zzz")` built a value no `match` could
                        // recognize as a `C`.
                        // Every occurrence is at the union's position, so a
                        // variant that has the field more than once, or is
                        // itself repeated, is reported once (JYRO-ADR-126
                        // point 2).
                        let c' =
                            variants
                            |> List.filter (fun v -> v.Fields |> List.exists (fun (fieldName, _) -> fieldName = "_variant"))
                            |> List.map (fun v -> v.Name)
                            |> List.distinct
                            |> List.fold (fun (c2: ValidationContext) variantName ->
                                c2.AddError(MessageCode.ReservedFieldName, [| box variantName |], loc)
                            ) c'
                        // A variant name is tested against the unions of the
                        // earlier source texts and of this one before it, and
                        // against the variants already seen in this union: a
                        // repeat anywhere in the compilation is
                        // DuplicateVariantName, never DuplicateFunction or
                        // VariantShadowsFunction (spec 50.10.4, JYRO-REQ-1689,
                        // JYRO-ADR-121, JYRO-DEF-525). Each declaration of the
                        // name after the first is reported, at the variant's
                        // own position (spec 150.8.5, 130.1.2, JYRO-ADR-138),
                        // which a variant read from an artifact carries as
                        // written.
                        let c', _ =
                            variants |> List.fold (fun (c2: ValidationContext, seenHere: Set<string>) (v: UnionVariant) ->
                                let vloc = v.Pos.Location
                                match c2.VariantToUnion.TryFind(v.Name) with
                                | Some existingUnion ->
                                    c2.AddError(MessageCode.DuplicateVariantName,
                                        [| box v.Name; box existingUnion |], vloc), seenHere
                                | None when seenHere.Contains(v.Name) ->
                                    c2.AddError(MessageCode.DuplicateVariantName,
                                        [| box v.Name; box name |], vloc), seenHere
                                | None -> c2, seenHere.Add(v.Name)
                            ) (c', Set.empty)
                        c'
                    if repeatedUnion then
                        // Only the registration is skipped for a repeated
                        // union name: the original declaration is kept. Do
                        // NOT overwrite DefinedUnions or add new entries to
                        // VariantToUnion - doing so previously left
                        // VariantToUnion pointing at a union whose variant
                        // list no longer contained the variant, causing a
                        // KeyNotFoundException during match validation.
                        c
                    else
                        // A variant name already registered keeps the union
                        // it was first declared in, so a match in the earlier
                        // source text still resolves it there.
                        { c with
                            DefinedUnions = c.DefinedUnions.Add(name, variants)
                            VariantToUnion =
                                variants |> List.fold (fun m v ->
                                    if Map.containsKey v.Name m then m else m.Add(v.Name, name)) c.VariantToUnion }
                // Pre-pass only inspects UnionDef declarations; every other
                // statement kind is walked later in validateStmt.
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
                | Delete _ -> c
            ) ctx

    let private seeded (registered: RegisteredNames) (earlier: UnionTable) =
        { ValidationContext.Empty with
            Registered = registered
            DefinedUnions = earlier.Unions
            VariantToUnion = earlier.VariantToUnion }

    /// `earlier` with the unions `statements` declare registered after it,
    /// as the union pre-pass registers them: the table the next source text
    /// of the compilation is checked against.
    let unionTable (earlier: UnionTable) (statements: Stmt list) : UnionTable =
        let c = declareUnions (seeded RegisteredNames.None earlier) statements
        { Unions = c.DefinedUnions; VariantToUnion = c.VariantToUnion }


    /// Validate `program`, one source text of a compilation, checking its
    /// function declarations against the names `registered` for the
    /// compilation and its unions against `unions` (JYRO-ADR-121).
    let rec validateIn (registered: RegisteredNames) (unions: UnionScope) (program: Program) : JyroResult<Program> =
        fst (validateText registered unions false program)

    /// `validateIn`, and with `deferForeignMatches` a library parsed ahead of
    /// any compilation: a `match` naming a variant the text does not declare
    /// is left to each compilation (JYRO-ADR-139). Returns the result and
    /// whether the text holds such a match (JYRO-ADR-140 point 1).
    and validateText (registered: RegisteredNames) (unions: UnionScope) (deferForeignMatches: bool) (program: Program) : JyroResult<Program> * bool =
        let declared =
            declareUnions { seeded registered unions.Earlier with DeferForeignMatches = deferForeignMatches } program.Statements
        // A match case may name a variant of any source text's union, a later
        // one's too (spec 50.9.3, JYRO-REQ-1690): the unions of the whole
        // compilation are added to this text's, which keep their own entries.
        let initialCtx =
            { declared with
                DefinedUnions =
                    unions.All.Unions |> Map.fold (fun m k v -> if Map.containsKey k m then m else Map.add k v m) declared.DefinedUnions
                VariantToUnion =
                    unions.All.VariantToUnion |> Map.fold (fun m k v -> if Map.containsKey k m then m else Map.add k v m) declared.VariantToUnion }
        let ctx = program.Statements |> List.fold validateStmt initialCtx
        let messages = ctx.Messages |> List.rev
        let result =
            if messages |> List.exists (fun m -> m.Severity = MessageSeverity.Error) then
                JyroResult<Program>.Failure(messages)
            else
                JyroResult<Program>.Success(program, messages)
        result, ctx.HoldsDeferredMatch

    /// Validate `program`, checking its function declarations against the
    /// names `registered` for the compilation, as one source text alone. The
    /// load path calls this: an artifact carries no libraries (JYRO-ADR-114).
    let validateWith (registered: RegisteredNames) (program: Program) : JyroResult<Program> =
        validateIn registered UnionScope.None program

    /// Validate `program` with no registered names, as a library is validated
    /// on its own.
    let validate (program: Program) : JyroResult<Program> =
        validateWith RegisteredNames.None program

    /// The union pre-pass's checks of a library parsed ahead
    /// (`JyroLibrary.Parse`) that holds no `match` left to the compilation:
    /// its union and variant names against `earlier`, the unions of the
    /// earlier source texts (50.10.3, 50.10.4, JYRO-ADR-121). Its matches
    /// were checked in full when it was parsed, and their result cannot
    /// change in a compilation in which no earlier source text declares one
    /// of its union or variant names (JYRO-ADR-140 point 2, JYRO-ADR-141).
    /// When this gives any diagnostic, an earlier text repeats one of those
    /// names and the library's own matches may resolve against that text's
    /// union, so the compilation discards these diagnostics and checks the
    /// library with `checkParsedLibrary` instead.
    let checkUnionDeclarations (earlier: UnionTable) (statements: Stmt list) : DiagnosticMessage list =
        (declareUnions (seeded RegisteredNames.None earlier) statements).Messages |> List.rev

    /// The Validate checks of a library parsed ahead (`JyroLibrary.Parse`)
    /// that holds a `match` left to the compilation, made in `unions`, that
    /// compilation's scope: its union and variant names against the earlier
    /// source texts (50.10.3, 50.10.4), and the checks of each `match` that
    /// read the union it resolves to (50.9.3), against the unions of every
    /// source text (spec 160.6, JYRO-REQ-1698, JYRO-ADR-140 point 3). The
    /// library is validated as it would be from source, and only these codes
    /// are kept: every other check reads the library alone and was made,
    /// clean, when it was parsed. So a parsed library gives the diagnostics
    /// of the same text added from source, with the same places. A library
    /// that holds no such match has `checkUnionDeclarations` alone, unless an
    /// earlier source text declares one of its union or variant names, when
    /// it has this check too (JYRO-ADR-141). Its
    /// function names are `checkLibraryDeclarations`'s.
    let checkParsedLibrary (unions: UnionScope) (statements: Stmt list) : DiagnosticMessage list =
        let kept =
            matchResolutionCodes
            |> Set.add MessageCode.DuplicateUnionName
            |> Set.add MessageCode.DuplicateVariantName
        (validateIn RegisteredNames.None unions ({ Statements = statements } : Program)).Messages
        |> List.ofSeq
        |> List.filter (fun m -> kept.Contains(m.Code))

    /// The function names the library scripts of one compilation have
    /// declared so far. Their union and variant names are checked by the
    /// union pre-pass against a `UnionTable` (JYRO-ADR-121).
    type LibraryNames =
        { Functions: Set<string> }

        static member Empty = { Functions = Set.empty }

    /// The Validate checks of one library script's declarations that need the
    /// compilation: `FunctionOverride` for a function named after a 90.2
    /// inventory function or a function registered for the compilation (spec
    /// 80.3.2), and `DuplicateFunction` for a function that an earlier
    /// library script already declares (80.3.1).
    /// A library's own validation knows no compilation, so these run here, in
    /// the Validate stage of the compilation, beside every other Validate
    /// check of every source text (JYRO-ADR-115 point 2, JYRO-ADR-116). They
    /// used to run in `Linker.link`, after a clean validation, so any
    /// validation error hid them (JYRO-DEF-510).
    ///
    /// `earlier` holds the names of the library scripts before this one, in
    /// the order the host added them. `ownValidated` says this library's own
    /// validation runs in the same compilation and reports a name repeated
    /// within it (`DuplicateFunction`), so only the
    /// earlier libraries are checked; a library parsed up front was validated
    /// clean, so a repeat within its declarations is checked here too.
    /// Returns the diagnostics and the names with this library's added.
    let checkLibraryDeclarations
        (registered: RegisteredNames)
        (earlier: LibraryNames)
        (ownValidated: bool)
        (statements: Stmt list)
        : DiagnosticMessage list * LibraryNames =
        let folded, ownNames =
            statements
            |> List.fold (fun (messages: DiagnosticMessage list, seen: LibraryNames) stmt ->
                CompilationStage.CheckAmbient()
                let against = if ownValidated then earlier else seen
                match stmt with
                | FuncDef(name, _, _, pos) ->
                    // The place is the declaration's range, its length with
                    // it (spec 130.1.2).
                    let loc = pos.Location
                    // The two checks are independent, as they are for the
                    // script's own declarations: a declaration named after a
                    // registered function and declared in an earlier library
                    // is both. Every declaration's name is recorded, so a
                    // later one of a registered name is a duplicate too
                    // (JYRO-ADR-115 point 4, JYRO-DEF-567).
                    let messages =
                        if registered.Reserved.Contains(name) then
                            DiagnosticMessage.Of(MessageCode.FunctionOverride, [| box name |], loc) :: messages
                        else messages
                    let messages =
                        if against.Functions.Contains(name) then
                            DiagnosticMessage.Of(MessageCode.DuplicateFunction, [| box name |], loc) :: messages
                        else messages
                    messages, { seen with Functions = seen.Functions.Add(name) }
                // A union's and its variants' names are the union pre-pass's,
                // checked against every earlier source text with the codes of
                // 50.10.3 and 50.10.4 (`validateIn`; for a library parsed
                // ahead, `checkUnionDeclarations` or `checkParsedLibrary`,
                // JYRO-ADR-121, JYRO-ADR-140); a variant is not a function
                // here.
                // A library keeps only its declarations; every other statement
                // is its own validation's `LibraryStatementForbidden`.
                | UnionDef _
                | VarDecl _
                | Assignment _
                | If _
                | While _
                | ForEach _
                | For _
                | Switch _
                | Return _
                | Fail _
                | Exit _
                | Break _
                | Continue _
                | ExprStmt _
                | Match _
                | Delete _ -> messages, seen
            ) ([], earlier)
        List.rev folded, ownNames

    /// The diagnostics of several source texts of one stage, as the result
    /// reports them (spec 130.5). This is the one function every result path
    /// assembles a stage's diagnostics through (JYRO-ADR-126 point 1): compile
    /// from source, compile to bytes, a load from .jyrx and
    /// `JyroLibrary.Parse`. No caller orders or deduplicates a part itself.
    ///
    /// Order: grouped by source text in the order the groups are given (the
    /// library scripts in the order the host added them, then the script),
    /// each group in position order, by line and then by column (REQ-1486,
    /// REQ-1489); the diagnostics with no position after every group, in the
    /// order given. The sort is stable, so diagnostics at one position keep
    /// the order they were raised in.
    ///
    /// Repeats: within a group, a diagnostic with the same code, the same
    /// position and the same arguments as one before it is removed, the first
    /// kept (REQ-1487). Two checks of one stage can reach one declaration (a
    /// function declared twice in a library that an earlier library also
    /// declares is `DuplicateFunction` by the library's own validation and by
    /// `checkLibraryDeclarations` alike), and a check may fire more than once
    /// at one position. The comparison stays within a group because a
    /// position carries no source text: the same code at line 3, column 5 of
    /// a library and of the script are two diagnostics, not one.
    let inSourceOrder (groups: DiagnosticMessage list list) : DiagnosticMessage list =
        let hasPosition (m: DiagnosticMessage) = m.Location.HasValue && m.Location.Value.Line > 0
        let distinct (messages: DiagnosticMessage list) =
            messages
            |> List.distinctBy (fun m ->
                m.Code,
                m.Location,
                m.Args |> Array.map (fun a -> if isNull a then "" else a.ToString()) |> String.concat "\u0000")
        let groups = groups |> List.map distinct
        let positioned =
            groups
            |> List.collect (fun group ->
                group
                |> List.filter hasPosition
                |> List.sortBy (fun m -> m.Location.Value.Line, m.Location.Value.Column))
        positioned @ (groups |> List.collect (List.filter (hasPosition >> not)))

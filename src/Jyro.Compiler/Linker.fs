namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast


/// Linker for resolving function references
module internal Linker =
    type LinkContext =
        { Functions: Map<string, IJyroFunction>
          HostFunctions: Set<string>
          ReferencedFunctions: Set<string>
          Messages: DiagnosticMessage list
          // Effect-tracking fields. `MaxEffect` is the strongest observable
          // effect tolerated by the current compilation context (library
          // bodies cap at NonDeterministic; user scripts cap at Mutating).
          // `ContextLabel` is "library" or "script", used to disambiguate
          // diagnostics.
          MaxEffect: FunctionEffect
          ContextLabel: string
          CurrentFunctionName: string option }

        static member Empty =
            { Functions = Map.empty
              HostFunctions = Set.empty
              ReferencedFunctions = Set.empty
              Messages = []
              MaxEffect = FunctionEffect.Mutating
              ContextLabel = "script"
              CurrentFunctionName = None }

        /// `func` as the callable its name stands for, replacing any callable
        /// of that name already in `Functions`.
        member this.AddFunction(func: IJyroFunction) =
            { this with Functions = this.Functions.Add(func.Name, func) }

        /// `FunctionShadowsVariant` for `name` at `location`. The variant is
        /// the callable the name stands for (JYRO-ADR-162): the declaring
        /// passes keep or put its constructor in `Functions`, never the
        /// function, so a call of the name is checked as a call to the variant.
        member this.AddShadowsVariant(name: string, location: SourceLocation) =
            let msg = DiagnosticMessage.Of(MessageCode.FunctionShadowsVariant, args = [| box name |], location = location)
            { this with Messages = msg :: this.Messages }

        member this.MarkReferenced(name: string) =
            { this with ReferencedFunctions = this.ReferencedFunctions.Add(name) }

        member this.AddError(code: MessageCode, args: (obj | null)[], location: SourceLocation) =
            let msg = DiagnosticMessage.Of(code, args = args, location = location)
            { this with Messages = msg :: this.Messages }

    /// A `JyroRuntimeException` that an engine member threw inside a call
    /// check, not one the host made with `JyroError.Runtime`
    /// (JYRO-ADR-131). Deliberately not a `JyroRuntimeException`, so the
    /// compilation's handler reports it as `UnknownCompilerError` with stage
    /// Link, as it does any other exception the check throws. The engine's
    /// exception is kept as the inner exception for a debugger; no diagnostic
    /// reads it.
    type CallCheckCrash(inner: JyroRuntimeException) =
        inherit System.Exception("A call check threw an exception that JyroError.Runtime did not make.", inner)

    /// The call site a call check is given (JYRO-ADR-130): the name, the
    /// names of the named arguments in order, and for each argument written
    /// its value when it is a literal (spec 20.1.1), read-only, or null when
    /// it is any other expression.
    let private callSiteOf (name: string) (callArgs: CallArgs) : JyroCallSite =
        let literalOf (e: Expr) : JyroValue =
            match e with
            | Literal(value, _) -> value.Freeze()
            | _ -> Unchecked.defaultof<JyroValue>
        match callArgs with
        | PositionalArgs exprs ->
            JyroCallSite(name, System.Array.AsReadOnly(Array.empty<string>), exprs |> List.map literalOf |> Array.ofList)
        | NamedArgs pairs ->
            JyroCallSite(
                name,
                System.Array.AsReadOnly(pairs |> List.map fst |> Array.ofList),
                pairs |> List.map (snd >> literalOf) |> Array.ofList)

    /// Runs the call check of `func` on one call, when `func` has one
    /// (`IJyroCallCheck`, JYRO-ADR-130), and returns the refusal, if it
    /// refused: `HostLinkError`, whose arguments are the reason and the values
    /// given to `JyroError.Host`, placed at the call.
    ///
    /// Only the refusal is caught. Any other exception the check throws
    /// escapes to the compilation's own handler, with the stage that was
    /// running, Link. A `JyroRuntimeException` the host made with
    /// `JyroError.Runtime` keeps its code there; one an engine member
    /// threw inside the check, such as `InvalidCast` from `AsNumber`, is a
    /// crash of the check, not a diagnostic of the script, so it escapes as
    /// `CallCheckCrash` and the handler reports `UnknownCompilerError`
    /// (JYRO-ADR-131). The three are told apart by the origin the members of
    /// `JyroError` record, never by the exception's code.
    let checkCall (func: IJyroFunction) (name: string) (callArgs: CallArgs) (location: SourceLocation) : DiagnosticMessage option =
        match func with
        | :? IJyroCallCheck as check ->
            let site = callSiteOf name callArgs
            try
                CallCheckScope.Run(fun () -> check.CheckCall(site))
                None
            with
            | :? JyroRuntimeException as ex when ex.Origin = RuntimeExceptionOrigin.Host ->
                Some (DiagnosticMessage.Of(MessageCode.HostLinkError, args = ex.Args, location = location))
            | :? JyroRuntimeException as ex when ex.Origin = RuntimeExceptionOrigin.Engine ->
                raise (CallCheckCrash(ex))
        | _ -> None

    /// The effect of `func`, or None when the maximum effect cannot be
    /// applied to it (spec 120.5, JYRO-REQ-1723): reading `Effect`, which is
    /// a host's code, threw an exception of any type, or gave none of the four
    /// effects of 120.4.1. The caller reports `MaxEffectNotApplicable`; the
    /// compilation never continues with a weaker maximum, nor treats the
    /// throw as an internal error.
    let tryReadEffect (func: IJyroFunction) : FunctionEffect option =
        let effect =
            try Some func.Effect
            with _ -> None
        match effect with
        | Some e when FunctionEffect.isOneOfFour e -> Some e
        | _ -> None

    /// Check function calls in an expression
    let rec private linkExpr (ctx: LinkContext) (expr: Expr) : LinkContext =
        match expr with
        | Call(name, callArgs, pos) ->
            let ctx' =
                match callArgs with
                | PositionalArgs exprs -> exprs |> List.fold linkExpr ctx
                | NamedArgs pairs -> pairs |> List.fold (fun c (_, expr) -> linkExpr c expr) ctx
            // A name declared as a function and as a variant resolves to the
            // variant's constructor (JYRO-ADR-162), so its call is checked
            // against the variant's fields like any other variant call.
            match ctx'.Functions.TryFind(name) with
            | Some func ->
                // Each call resolved is where an abandoned compilation stops
                // (JYRO-ADR-100): resolving one reads the callee's signature
                // and effect, which are a host's code and may be slow.
                CompilationStage.CheckAmbient()
                let loc = pos.Location
                let ctx'' =
                    match callArgs with
                    | PositionalArgs exprs ->
                        if exprs.Length < func.Signature.MinArgs || exprs.Length > func.Signature.MaxArgs then
                            if exprs.Length < func.Signature.MinArgs then
                                let msgArgs = [| box name; box func.Signature.MinArgs; box exprs.Length |]
                                ctx'.AddError(MessageCode.TooFewArguments, msgArgs, loc)
                            else
                                let msgArgs = [| box name; box func.Signature.MaxArgs; box exprs.Length |]
                                ctx'.AddError(MessageCode.TooManyArguments, msgArgs, loc)
                        else
                            ctx'
                    | NamedArgs pairs ->
                        let paramNames = func.Signature.Parameters |> Seq.map (fun p -> p.Name) |> Set.ofSeq
                        let requiredParams = func.Signature.Parameters |> Seq.filter (fun p -> not p.IsOptional) |> Seq.map (fun p -> p.Name) |> Set.ofSeq
                        let providedNames = pairs |> List.map fst
                        let providedSet = providedNames |> Set.ofList
                        let c1 =
                            providedNames
                            |> List.groupBy id
                            |> List.filter (fun (_, group) -> group.Length > 1)
                            |> List.fold (fun (c: LinkContext) (dupName, _) ->
                                c.AddError(MessageCode.DuplicateNamedArgument, [| box dupName; box name |], loc)
                            ) ctx'
                        let c2 =
                            Set.difference providedSet paramNames
                            |> Set.fold (fun (c: LinkContext) unknownName ->
                                // The name written and the function called; the
                                // parameters it has are not joined into one
                                // argument, which would be a list, not a name
                                // (spec 130.6.2).
                                c.AddError(MessageCode.UnknownNamedArgument, [| box unknownName; box name |], loc)
                            ) c1
                        Set.difference requiredParams providedSet
                        |> Set.fold (fun (c: LinkContext) missingName ->
                            c.AddError(MessageCode.MissingRequiredNamedArgument, [| box missingName; box name |], loc)
                        ) c2
                let ctx''' =
                    match tryReadEffect func with
                    | None ->
                        // The function, then the maximum in force, whatever
                        // it is, `Mutating` included (JYRO-REQ-1723).
                        ctx''.AddError(
                            MessageCode.MaxEffectNotApplicable,
                            [| box name; box (FunctionEffect.name ctx''.MaxEffect) |],
                            loc)
                    | Some calleeEffect when FunctionEffect.exceeds calleeEffect ctx''.MaxEffect ->
                        let effectName = FunctionEffect.name calleeEffect
                        let maxName = FunctionEffect.name ctx''.MaxEffect
                        // The function called, its effect and the maximum in
                        // force, names the specification gives (Part 120).
                        // Not the enclosing function nor a word for the
                        // context, `script` or `library`, which the
                        // implementation chose (spec 130.6.2): the place of
                        // the diagnostic is the call.
                        ctx''.AddError(
                            MessageCode.FunctionEffectExceedsMax,
                            [| box name; box effectName; box maxName |],
                            loc)
                    | Some _ ->
                        ctx''
                // A call that passed every check above is given to its
                // function's call check, if it has one; a call that failed
                // one is not (JYRO-ADR-130). AddError conses, so the list is
                // the same one exactly when this call added no diagnostic.
                let ctx'''' =
                    if obj.ReferenceEquals(ctx'''.Messages, ctx'.Messages) then
                        match checkCall func name callArgs loc with
                        | Some refusal -> { ctx''' with Messages = refusal :: ctx'''.Messages }
                        | None -> ctx'''
                    else
                        ctx'''
                ctx''''.MarkReferenced(name)
            | None ->
                let loc = pos.Location
                ctx'.AddError(MessageCode.UndefinedFunction, [| box name |], loc)
        | Binary(left, _, right, _) ->
            ctx |> linkExpr <| left |> linkExpr <| right
        | Unary(_, operand, _) ->
            ctx |> linkExpr <| operand
        | Ternary(cond, thenExpr, elseExpr, _) ->
            ctx |> linkExpr <| cond |> linkExpr <| thenExpr |> linkExpr <| elseExpr
        | PropertyAccess(target, _, _) ->
            ctx |> linkExpr <| target
        | IndexAccess(target, index, _) ->
            ctx |> linkExpr <| target |> linkExpr <| index
        | ObjectLiteral(props, _) ->
            props |> List.fold (fun c (_, e) -> linkExpr c e) ctx
        | ArrayLiteral(elems, _) ->
            elems |> List.fold linkExpr ctx
        | Lambda(_, body, _) ->
            ctx |> linkExpr <| body
        | TypeCheck(e, _, _, _) ->
            ctx |> linkExpr <| e
        | IncrementDecrement(e, _, _, _) ->
            ctx |> linkExpr <| e
        | MatchExpr(expr, cases, _) ->
            let ctx' = linkExpr ctx expr
            cases |> List.fold (fun c (mc: MatchExprCase) -> linkExpr c mc.Body) ctx'
        | Literal _
        | Identifier _ -> ctx

    /// Check function calls in a statement
    let rec private linkStmt (ctx: LinkContext) (stmt: Stmt) : LinkContext =
        CompilationStage.CheckAmbient()
        match stmt with
        | VarDecl(_, _, init, _) ->
            match init with
            | Some expr -> linkExpr ctx expr
            | None -> ctx
        | Assignment(target, _, value, _, _) ->
            ctx |> linkExpr <| target |> linkExpr <| value
        | If(cond, thenBlock, elseIfs, elseBlock, _) ->
            let ctx' = linkExpr ctx cond
            let ctx'' = thenBlock |> List.fold linkStmt ctx'
            let ctx''' = elseIfs |> List.fold (fun c (e, stmts) ->
                let c' = linkExpr c e
                stmts |> List.fold linkStmt c') ctx''
            match elseBlock with
            | Some stmts -> stmts |> List.fold linkStmt ctx'''
            | None -> ctx'''
        | While(cond, body, _) ->
            let ctx' = linkExpr ctx cond
            body |> List.fold linkStmt ctx'
        | ForEach(_, collection, body, _) ->
            let ctx' = linkExpr ctx collection
            body |> List.fold linkStmt ctx'
        | For(_, startExpr, endExpr, stepExpr, _, body, _) ->
            let ctx' = linkExpr ctx startExpr
            let ctx' = linkExpr ctx' endExpr
            let ctx' = match stepExpr with Some e -> linkExpr ctx' e | None -> ctx'
            body |> List.fold linkStmt ctx'
        | Switch(expr, cases, defaultCase, _) ->
            let ctx' = linkExpr ctx expr
            let ctx'' = cases |> List.fold (fun c case ->
                let c' = case.Values |> List.fold linkExpr c
                case.Body |> List.fold linkStmt c') ctx'
            match defaultCase with
            | Some stmts -> stmts |> List.fold linkStmt ctx''
            | None -> ctx''
        | Return(valueOpt, _) ->
            match valueOpt with
            | Some expr -> linkExpr ctx expr
            | None -> ctx
        | Exit(valueOpt, _) ->
            match valueOpt with
            | Some expr -> linkExpr ctx expr
            | None -> ctx
        | Fail(msgOpt, _) ->
            match msgOpt with
            | Some expr -> linkExpr ctx expr
            | None -> ctx
        | FuncDef(name, _, body, _) ->
            // Track the current function name so library-mode diagnostics
            // can attribute forbidden calls to the offending function.
            let bodyCtx = { ctx with CurrentFunctionName = Some name }
            let afterBody = body |> List.fold linkStmt bodyCtx
            { afterBody with CurrentFunctionName = ctx.CurrentFunctionName }
        | UnionDef _ -> ctx
        | Match(expr, cases, _) ->
            let ctx' = linkExpr ctx expr
            cases |> List.fold (fun c mc -> mc.Body |> List.fold linkStmt c) ctx'
        | Delete(target, _) -> linkExpr ctx target
        | Break _ | Continue _ -> ctx
        | ExprStmt(expr, _) ->
            linkExpr ctx expr

    /// Every call in an expression, as (name, arguments, position), an
    /// argument's calls before the call it is an argument of.
    let rec private callsInExpr (expr: Expr) : (string * CallArgs * Position) list =
        match expr with
        | Call(name, callArgs, pos) ->
            let inner =
                match callArgs with
                | PositionalArgs exprs -> exprs |> List.collect callsInExpr
                | NamedArgs pairs -> pairs |> List.collect (snd >> callsInExpr)
            inner @ [ (name, callArgs, pos) ]
        | Binary(left, _, right, _) -> callsInExpr left @ callsInExpr right
        | Unary(_, operand, _) -> callsInExpr operand
        | Ternary(cond, thenExpr, elseExpr, _) -> callsInExpr cond @ callsInExpr thenExpr @ callsInExpr elseExpr
        | PropertyAccess(target, _, _) -> callsInExpr target
        | IndexAccess(target, index, _) -> callsInExpr target @ callsInExpr index
        | ObjectLiteral(props, _) -> props |> List.collect (snd >> callsInExpr)
        | ArrayLiteral(elems, _) -> elems |> List.collect callsInExpr
        | Lambda(_, body, _) -> callsInExpr body
        | TypeCheck(e, _, _, _) -> callsInExpr e
        | IncrementDecrement(e, _, _, _) -> callsInExpr e
        | MatchExpr(e, cases, _) -> callsInExpr e @ (cases |> List.collect (fun (mc: MatchExprCase) -> callsInExpr mc.Body))
        | Literal _
        | Identifier _ -> []

    /// Every call in a statement and the statements it holds.
    let rec private callsInStmt (stmt: Stmt) : (string * CallArgs * Position) list =
        CompilationStage.CheckAmbient()
        let ofOpt = function Some e -> callsInExpr e | None -> []
        let ofBlock (stmts: Stmt list) = stmts |> List.collect callsInStmt
        match stmt with
        | VarDecl(_, _, init, _) -> ofOpt init
        | Assignment(target, _, value, _, _) -> callsInExpr target @ callsInExpr value
        | If(cond, thenBlock, elseIfs, elseBlock, _) ->
            callsInExpr cond
            @ ofBlock thenBlock
            @ (elseIfs |> List.collect (fun (e, stmts) -> callsInExpr e @ ofBlock stmts))
            @ (match elseBlock with Some stmts -> ofBlock stmts | None -> [])
        | While(cond, body, _) -> callsInExpr cond @ ofBlock body
        | ForEach(_, collection, body, _) -> callsInExpr collection @ ofBlock body
        | For(_, startExpr, endExpr, stepExpr, _, body, _) ->
            callsInExpr startExpr @ callsInExpr endExpr @ ofOpt stepExpr @ ofBlock body
        | Switch(expr, cases, defaultCase, _) ->
            callsInExpr expr
            @ (cases |> List.collect (fun case -> (case.Values |> List.collect callsInExpr) @ ofBlock case.Body))
            @ (match defaultCase with Some stmts -> ofBlock stmts | None -> [])
        | Return(valueOpt, _) -> ofOpt valueOpt
        | Exit(valueOpt, _) -> ofOpt valueOpt
        | Fail(msgOpt, _) -> ofOpt msgOpt
        | FuncDef(_, _, body, _) -> ofBlock body
        | UnionDef _ -> []
        | Match(expr, cases, _) -> callsInExpr expr @ (cases |> List.collect (fun mc -> ofBlock mc.Body))
        | Delete(target, _) -> callsInExpr target
        | Break _ | Continue _ -> []
        | ExprStmt(expr, _) -> callsInExpr expr

    /// The call checks of a load (JYRO-ADR-130, spec 150.1.5): each call in
    /// `statements` of a function in `functions`, the host functions whose
    /// names resolved and whose effect is within the maximum, is given to
    /// that function's call check, as a compilation from source gives it.
    /// Returns the refusals, one per refused call, in source order.
    let checkCallsOnLoad (functions: Map<string, IJyroFunction>) (statements: Stmt list) : DiagnosticMessage list =
        statements
        |> List.collect callsInStmt
        |> List.choose (fun (name, callArgs, pos) ->
            match functions.TryFind(name) with
            | Some func -> checkCall func name callArgs pos.Location
            | None -> None)

    /// Walk an expression and return the maximum effect found at any call
    /// site. Used by effect inference to compute a user/library function's
    /// effect as the max of all callees in its body. Unresolved calls are
    /// treated as Pure (the validation pass emits UndefinedFunction for them).
    let rec private maxEffectInExpr (functions: Map<string, IJyroFunction>) (expr: Expr) : FunctionEffect =
        match expr with
        | Call(name, callArgs, _) ->
            CompilationStage.CheckAmbient()
            let calleeEffect =
                match functions.TryFind(name) with
                // A function whose effect cannot be read adds nothing here:
                // its call is reported as `MaxEffectNotApplicable` where it
                // stands, so the compilation fails (JYRO-REQ-1723).
                | Some f -> tryReadEffect f |> Option.defaultValue FunctionEffect.Pure
                | None -> FunctionEffect.Pure
            let argsEffect =
                match callArgs with
                | PositionalArgs exprs ->
                    exprs
                    |> List.fold (fun acc e -> FunctionEffect.max acc (maxEffectInExpr functions e))
                        FunctionEffect.Pure
                | NamedArgs pairs ->
                    pairs
                    |> List.fold (fun acc (_, e) -> FunctionEffect.max acc (maxEffectInExpr functions e))
                        FunctionEffect.Pure
            FunctionEffect.max calleeEffect argsEffect
        | Binary(left, _, right, _) ->
            FunctionEffect.max (maxEffectInExpr functions left) (maxEffectInExpr functions right)
        | Unary(_, operand, _) ->
            maxEffectInExpr functions operand
        | Ternary(cond, thenExpr, elseExpr, _) ->
            FunctionEffect.max
                (maxEffectInExpr functions cond)
                (FunctionEffect.max (maxEffectInExpr functions thenExpr) (maxEffectInExpr functions elseExpr))
        | PropertyAccess(target, _, _) ->
            maxEffectInExpr functions target
        | IndexAccess(target, index, _) ->
            FunctionEffect.max (maxEffectInExpr functions target) (maxEffectInExpr functions index)
        | ObjectLiteral(props, _) ->
            props |> List.fold (fun acc (_, e) -> FunctionEffect.max acc (maxEffectInExpr functions e)) FunctionEffect.Pure
        | ArrayLiteral(elems, _) ->
            elems |> List.fold (fun acc e -> FunctionEffect.max acc (maxEffectInExpr functions e)) FunctionEffect.Pure
        | Lambda(_, body, _) ->
            maxEffectInExpr functions body
        | TypeCheck(e, _, _, _) ->
            maxEffectInExpr functions e
        | IncrementDecrement(e, _, _, _) ->
            maxEffectInExpr functions e
        | MatchExpr(expr, cases, _) ->
            let initial = maxEffectInExpr functions expr
            cases
            |> List.fold (fun acc (mc: MatchExprCase) -> FunctionEffect.max acc (maxEffectInExpr functions mc.Body)) initial
        | Literal _
        | Identifier _ -> FunctionEffect.Pure

    /// Walk a statement and return the maximum effect found across all
    /// nested expressions and substatements.
    and private maxEffectInStmt (functions: Map<string, IJyroFunction>) (stmt: Stmt) : FunctionEffect =
        CompilationStage.CheckAmbient()
        match stmt with
        | VarDecl(_, _, init, _) ->
            match init with
            | Some expr -> maxEffectInExpr functions expr
            | None -> FunctionEffect.Pure
        | Assignment(target, _, value, _, _) ->
            FunctionEffect.max (maxEffectInExpr functions target) (maxEffectInExpr functions value)
        | If(cond, thenBlock, elseIfs, elseBlock, _) ->
            let condEff = maxEffectInExpr functions cond
            let thenEff = thenBlock |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
            let elseIfEff =
                elseIfs
                |> List.fold (fun acc (e, stmts) ->
                    let eEff = maxEffectInExpr functions e
                    let stmtsEff = stmts |> List.fold (fun a s -> FunctionEffect.max a (maxEffectInStmt functions s)) FunctionEffect.Pure
                    FunctionEffect.max acc (FunctionEffect.max eEff stmtsEff)) FunctionEffect.Pure
            let elseEff =
                match elseBlock with
                | Some stmts -> stmts |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
                | None -> FunctionEffect.Pure
            FunctionEffect.max condEff (FunctionEffect.max thenEff (FunctionEffect.max elseIfEff elseEff))
        | While(cond, body, _) ->
            let condEff = maxEffectInExpr functions cond
            let bodyEff = body |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
            FunctionEffect.max condEff bodyEff
        | ForEach(_, collection, body, _) ->
            let collEff = maxEffectInExpr functions collection
            let bodyEff = body |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
            FunctionEffect.max collEff bodyEff
        | For(_, startExpr, endExpr, stepExpr, _, body, _) ->
            let sEff = maxEffectInExpr functions startExpr
            let eEff = maxEffectInExpr functions endExpr
            let stEff = match stepExpr with Some e -> maxEffectInExpr functions e | None -> FunctionEffect.Pure
            let bodyEff = body |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
            FunctionEffect.max sEff (FunctionEffect.max eEff (FunctionEffect.max stEff bodyEff))
        | Switch(expr, cases, defaultCase, _) ->
            let exprEff = maxEffectInExpr functions expr
            let casesEff =
                cases
                |> List.fold (fun acc (case: SwitchCase) ->
                    let valsEff = case.Values |> List.fold (fun a e -> FunctionEffect.max a (maxEffectInExpr functions e)) FunctionEffect.Pure
                    let bodyEff = case.Body |> List.fold (fun a s -> FunctionEffect.max a (maxEffectInStmt functions s)) FunctionEffect.Pure
                    FunctionEffect.max acc (FunctionEffect.max valsEff bodyEff)) FunctionEffect.Pure
            let defaultEff =
                match defaultCase with
                | Some stmts -> stmts |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
                | None -> FunctionEffect.Pure
            FunctionEffect.max exprEff (FunctionEffect.max casesEff defaultEff)
        | Return(valueOpt, _)
        | Exit(valueOpt, _) ->
            match valueOpt with
            | Some expr -> maxEffectInExpr functions expr
            | None -> FunctionEffect.Pure
        | Fail(msgOpt, _) ->
            match msgOpt with
            | Some expr -> maxEffectInExpr functions expr
            | None -> FunctionEffect.Pure
        | FuncDef(_, _, body, _) ->
            body |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s)) FunctionEffect.Pure
        | UnionDef _ -> FunctionEffect.Pure
        | Match(expr, cases, _) ->
            let exprEff = maxEffectInExpr functions expr
            let casesEff =
                cases
                |> List.fold (fun acc (mc: MatchCase) ->
                    let bodyEff = mc.Body |> List.fold (fun a s -> FunctionEffect.max a (maxEffectInStmt functions s)) FunctionEffect.Pure
                    FunctionEffect.max acc bodyEff) FunctionEffect.Pure
            FunctionEffect.max exprEff casesEff
        | Delete(target, _) -> maxEffectInExpr functions target
        | Break _ | Continue _ -> FunctionEffect.Pure
        | ExprStmt(expr, _) -> maxEffectInExpr functions expr

    /// Run effect inference: for each (name, body) pair, compute the body's
    /// max effect (reading other shells' effects from `functions`), and
    /// write back via SetEffect. Iterate until stable. Bounded by
    /// `4 * shells.Count + 1` since the lattice has height 4.
    let private inferEffectsForBodies
        (shells: System.Collections.Generic.Dictionary<string, JyroUserFunction>)
        (bodies: (string * Stmt list) list)
        (functions: Map<string, IJyroFunction>)
        : unit =
        let maxIterations = 4 * shells.Count + 1
        let mutable changed = true
        let mutable iter = 0
        while changed && iter < maxIterations do
            changed <- false
            iter <- iter + 1
            for (name, body) in bodies do
                CompilationStage.CheckAmbient()
                let newEffect =
                    body
                    |> List.fold (fun acc s -> FunctionEffect.max acc (maxEffectInStmt functions s))
                        FunctionEffect.Pure
                match shells.TryGetValue(name) with
                | true, shell ->
                    let current = (shell :> IJyroFunction).Effect
                    if FunctionEffect.rank newEffect > FunctionEffect.rank current then
                        shell.SetEffect(newEffect)
                        changed <- true
                | _ -> ()

    /// The Link-stage checks of a program's top-level declarations, and the
    /// callables they introduce: a user-function shell for each `func` and a
    /// constructor for each variant. Called by `linkScripts` and by the load path
    /// alike (JYRO-ADR-114, JYRO-ADR-116), so a load makes every Link check of
    /// a declaration's name that a compilation from source makes; neither
    /// keeps a copy.
    ///
    /// `ctx.HostFunctions` holds the names a variant may not take: the
    /// registered functions (the loading host's, on a load), and on the
    /// source path also the libraries' functions and variant constructors.
    /// A variant of one of these names is `VariantShadowsFunction`; a
    /// function and a variant of one name are `FunctionShadowsVariant`
    /// (spec 50.10.9). The Validate codes of a declaration
    /// (`DuplicateFunction`, `FunctionOverride`, `FunctionShadowsLibrary`)
    /// are the validator's, which runs first on both paths and stops the
    /// compilation before Link when it reports one (JYRO-ADR-115,
    /// JYRO-ADR-116), so a declaration they reject is not declared here.
    ///
    /// `libraryFunctions` and `libraryVariants` are the library pre-pass's
    /// declarations, empty on a load (an artifact carries no libraries).
    let declare
        (ctx: LinkContext)
        (libraryFunctions: System.Collections.Generic.Dictionary<string, JyroUserFunction>)
        (libraryVariants: System.Collections.Generic.Dictionary<string, JyroVariantConstructor>)
        (statements: Stmt list)
        : LinkContext
          * System.Collections.Generic.Dictionary<string, JyroUserFunction>
          * System.Collections.Generic.Dictionary<string, JyroVariantConstructor> =
        let userFunctions = System.Collections.Generic.Dictionary<string, JyroUserFunction>()
        let variantConstructors = System.Collections.Generic.Dictionary<string, JyroVariantConstructor>()
        let declared =
            statements |> List.fold (fun (c: LinkContext) stmt ->
                CompilationStage.CheckAmbient()
                match stmt with
                | FuncDef(name, parameters, _, pos) ->
                    let loc = pos.Location
                    // A library function's name is the validator's
                    // FunctionShadowsLibrary, and a second declaration its
                    // DuplicateFunction; the first one declared is kept.
                    if libraryFunctions.ContainsKey(name) || userFunctions.ContainsKey(name) then
                        c
                    elif variantConstructors.ContainsKey(name) || libraryVariants.ContainsKey(name) then
                        c.AddShadowsVariant(name, loc)
                    else
                        let shell = JyroUserFunction.createShell name parameters
                        userFunctions.[name] <- shell
                        c.AddFunction(shell)
                | UnionDef(_, variants, pos) ->
                    let loc = pos.Location
                    // FunctionShadowsVariant for a variant whose name a
                    // function already has, and the variant still declared,
                    // replacing the function in `Functions`, so a call of the
                    // name binds to it (JYRO-ADR-162).
                    let shadowing (c': LinkContext) (variant: UnionVariant) =
                        let c'' = c'.AddShadowsVariant(variant.Name, loc)
                        if variantConstructors.ContainsKey(variant.Name) then
                            c''
                        else
                            let ctor = JyroVariantConstructor.create variant
                            variantConstructors.[variant.Name] <- ctor
                            c''.AddFunction(ctor)
                    variants |> List.fold (fun (c': LinkContext) variant ->
                        // A variant a library declares is the validator's
                        // DuplicateVariantName (spec 50.10.4, JYRO-ADR-121),
                        // which stops the compilation before Link; the first
                        // is kept. Tested before `HostFunctions`, which holds
                        // the library variant names too.
                        if libraryVariants.ContainsKey(variant.Name) then
                            c'
                        // A library function is a script function, not a
                        // host function: a variant of its name is
                        // FunctionShadowsVariant (spec 50.10.9). Tested before
                        // `HostFunctions`, into which the source path folds
                        // the library function names.
                        //
                        // The variant of a clashing name is still declared,
                        // replacing the function in `Functions`, so a call of
                        // the name binds to it (JYRO-ADR-162).
                        elif libraryFunctions.ContainsKey(variant.Name) then
                            shadowing c' variant
                        elif c'.HostFunctions.Contains(variant.Name) then
                            c'.AddError(MessageCode.VariantShadowsFunction,
                                [| box variant.Name |], loc)
                        elif userFunctions.ContainsKey(variant.Name) then
                            shadowing c' variant
                        // A variant repeated in one source text is the
                        // validator's DuplicateVariantName; the first is kept.
                        elif variantConstructors.ContainsKey(variant.Name) then
                            c'
                        else
                            let ctor = JyroVariantConstructor.create variant
                            variantConstructors.[variant.Name] <- ctor
                            c'.AddFunction(ctor)
                    ) c
                // Only top-level declarations are extracted; every other
                // statement kind is walked later, if at all.
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
                | Delete _ -> c
            ) ctx
        declared, userFunctions, variantConstructors

    type LinkedProgram =
        { Program: Program
          LibraryStatements: Stmt list
          Functions: Map<string, IJyroFunction>
          // The standard library and host functions the program calls, each
          // once, in the order of the function table of its artifact
          // (spec 150.3.3, 150.3.6): the order of the names on a compilation
          // from source, and the artifact's own table on a load.
          RequiredFunctions: string list
          UserFunctions: Map<string, JyroUserFunction>
          LibraryFunctions: Map<string, JyroUserFunction>
          VariantConstructors: Map<string, JyroVariantConstructor>
          LibraryVariantConstructors: Map<string, JyroVariantConstructor> }

    /// Link a program with the available functions.
    ///
    /// `libraryScripts` are the pre-parsed declarations of the library
    /// scripts, one list per source text in the order the host added them
    /// (only `FuncDef` and `UnionDef` are meaningful; other statements are
    /// expected to have been filtered by `LibraryValidator.validateLibrary`).
    /// `userMaxEffect` / `libraryMaxEffect` bound the strongest observable
    /// effect tolerated by user-script and library-body contexts respectively.
    ///
    /// On failure the diagnostics come back as one group per library script,
    /// in the order given, then one for the script; each diagnostic is in the
    /// group of the source text that holds its position (JYRO-ADR-127). The
    /// linker promises no order within a group: the caller assembles the
    /// groups through `Validator.inSourceOrder`, whose order it is.
    let linkScripts
        (program: Program)
        (functions: IJyroFunction seq)
        (libraryScripts: Stmt list list)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        : Result<LinkedProgram, DiagnosticMessage list list> =
        let hostFunctionNames = functions |> Seq.map (fun f -> f.Name) |> Set.ofSeq
        let libraryStatements = List.concat libraryScripts

        // Runs `step` over each library script in turn, carrying the context
        // from one to the next, and returns the diagnostics each script's
        // statements raised as a group of their own.
        let perLibrary (step: LinkContext -> Stmt -> LinkContext) (start: LinkContext) =
            libraryScripts
            |> List.mapFold (fun (c: LinkContext) statements ->
                let after = statements |> List.fold step { c with Messages = [] }
                List.rev after.Messages, { after with Messages = [] }) { start with Messages = [] }

        // === Pre-pass: extract library function/union shells ===
        let libraryFunctions = System.Collections.Generic.Dictionary<string, JyroUserFunction>()
        let libraryVariantConstructors = System.Collections.Generic.Dictionary<string, JyroVariantConstructor>()
        let preCtx =
            functions
            |> Seq.fold (fun (c: LinkContext) (f: IJyroFunction) -> c.AddFunction(f))
                { LinkContext.Empty with HostFunctions = hostFunctionNames }
        let preGroups, preCtx =
            preCtx |> perLibrary (fun (c: LinkContext) stmt ->
                CompilationStage.CheckAmbient()
                match stmt with
                | FuncDef(name, parameters, _, pos) ->
                    let loc = pos.Location
                    // A registered function's name is the validator's
                    // FunctionOverride, and a function another library
                    // declares its DuplicateFunction
                    // (`Validator.checkLibraryDeclarations`), both raised in
                    // Validate, which stops the compilation before Link
                    // (JYRO-ADR-115). The first declaration is kept.
                    if c.HostFunctions.Contains(name) || libraryFunctions.ContainsKey(name) then
                        c
                    elif libraryVariantConstructors.ContainsKey(name) then
                        c.AddShadowsVariant(name, loc)
                    else
                        let shell = JyroUserFunction.createShell name parameters
                        libraryFunctions.[name] <- shell
                        c.AddFunction(shell)
                | UnionDef(_, variants, pos) ->
                    let loc = pos.Location
                    variants |> List.fold (fun (c': LinkContext) variant ->
                        if c'.HostFunctions.Contains(variant.Name) then
                            c'.AddError(MessageCode.VariantShadowsFunction,
                                [| box variant.Name |], loc)
                        // The variant of a function's name is still declared,
                        // replacing the function in `Functions`, so a call of
                        // the name binds to it (JYRO-ADR-162).
                        elif libraryFunctions.ContainsKey(variant.Name) then
                            let c'' = c'.AddShadowsVariant(variant.Name, loc)
                            if libraryVariantConstructors.ContainsKey(variant.Name) then
                                c''
                            else
                                let ctor = JyroVariantConstructor.create variant
                                libraryVariantConstructors.[variant.Name] <- ctor
                                c''.AddFunction(ctor)
                        // A variant another library declares is the
                        // validator's DuplicateVariantName (the union
                        // pre-pass, JYRO-ADR-121); the first is kept.
                        elif libraryVariantConstructors.ContainsKey(variant.Name) then
                            c'
                        else
                            let ctor = JyroVariantConstructor.create variant
                            libraryVariantConstructors.[variant.Name] <- ctor
                            c'.AddFunction(ctor)
                    ) c
                // Library scripts only carry FuncDef/UnionDef (LibraryValidator
                // rejects everything else); the remaining cases are unreachable.
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
                | Delete _ -> c
            )

        // === Pass 1 (user): build initial context with host functions, library functions, library variant constructors ===
        // Library names are treated as host-level for shadowing checks - a
        // user `FuncDef` that names a library function is rejected with
        // `FunctionShadowsLibrary`.
        // The variant constructors are added after the functions, so a name
        // the libraries declare as both stands for the variant in the script
        // too (JYRO-ADR-162).
        let userBaseCtx =
            functions
            |> Seq.fold (fun (c: LinkContext) (f: IJyroFunction) -> c.AddFunction(f))
                { LinkContext.Empty with HostFunctions = hostFunctionNames }
        let userBaseCtx =
            libraryFunctions
            |> Seq.fold (fun (c: LinkContext) kv ->
                let withFn = c.AddFunction(kv.Value)
                { withFn with HostFunctions = withFn.HostFunctions.Add(kv.Key) })
                userBaseCtx
        let userBaseCtx =
            libraryVariantConstructors
            |> Seq.fold (fun (c: LinkContext) kv ->
                let withFn = c.AddFunction(kv.Value)
                { withFn with HostFunctions = withFn.HostFunctions.Add(kv.Key) })
                userBaseCtx

        let userPass1, userFunctions, variantConstructors =
            declare userBaseCtx libraryFunctions libraryVariantConstructors program.Statements

        // A union declared in the script is in scope in every library, as a
        // library's is in the script (spec 160.6, JYRO-REQ-1687,
        // JYRO-ADR-121 point 2): a library body may call the script's variant
        // constructors, though not its functions.
        // A name the script declares as a variant and a library as a
        // function stands for the variant in the library bodies too
        // (JYRO-ADR-162), so the variant replaces the library function there.
        let preCtx =
            variantConstructors
            |> Seq.fold (fun (c: LinkContext) kv ->
                if c.Functions.ContainsKey(kv.Key) && not (libraryFunctions.ContainsKey(kv.Key)) then c
                else c.AddFunction(kv.Value))
                preCtx

        // === Effect inference ===
        // Library functions are inferred first (against host functions only,
        // since library bodies cannot call user functions). User functions
        // are inferred next, seeing both host effects and the now-settled
        // library effects. Each shell's effect is updated in place via
        // SetEffect; the validation passes below read effects through
        // IJyroFunction.Effect.
        let libraryBodies =
            libraryStatements
            |> List.choose (fun stmt ->
                match stmt with
                | FuncDef(name, _, body, _) when libraryFunctions.ContainsKey(name) -> Some (name, body)
                | _ -> None)
        inferEffectsForBodies libraryFunctions libraryBodies preCtx.Functions

        let userBodies =
            program.Statements
            |> List.choose (fun stmt ->
                match stmt with
                | FuncDef(name, _, body, _) when userFunctions.ContainsKey(name) -> Some (name, body)
                | _ -> None)
        inferEffectsForBodies userFunctions userBodies userPass1.Functions

        // === Pass 2a: walk library function bodies under the library ceiling ===
        // The library context sees ALL host functions (and library shells +
        // variant constructors). Calls whose callee effect exceeds
        // `libraryMaxEffect` are reported as `FunctionEffectExceedsMax`.
        let libraryBodyCtx =
            { preCtx with
                MaxEffect = libraryMaxEffect
                ContextLabel = "library"
                ReferencedFunctions = Set.empty }
        let bodyGroups, libraryBodyResult =
            libraryBodyCtx |> perLibrary (fun (c: LinkContext) stmt ->
                match stmt with
                | FuncDef(_, _, _, _) -> linkStmt c stmt
                // UnionDef has no body to walk; everything else cannot appear
                // in a validated library.
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
                | Delete _ -> c
            )

        // === Pass 2b: walk user statements (including user function bodies) under the user ceiling ===
        let fullCtx =
            { userPass1 with
                MaxEffect = userMaxEffect
                ContextLabel = "script"
                ReferencedFunctions = Set.empty }
        let userBodyResult = program.Statements |> List.fold linkStmt fullCtx

        // One group per library script, its declarations' diagnostics then
        // its bodies', and one for the script: `userBodyResult` is built from
        // `userPass1`, so it already carries the script's declaration
        // diagnostics. The order within a group is the caller's
        // (`Validator.inSourceOrder`, JYRO-ADR-127).
        let groups =
            List.map2 (@) preGroups bodyGroups @ [ List.rev userBodyResult.Messages ]

        if groups |> List.exists (List.exists (fun m -> m.Severity = MessageSeverity.Error)) then
            Error groups
        else
            let allReferenced =
                Set.union libraryBodyResult.ReferencedFunctions userBodyResult.ReferencedFunctions
            let resolveFrom = userBodyResult.Functions
            let referencedFunctionMap =
                allReferenced
                |> Set.toSeq
                |> Seq.choose (fun name -> resolveFrom.TryFind(name) |> Option.map (fun f -> name, f))
                |> Map.ofSeq
            let userFuncMap =
                userFunctions |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
            let libraryFuncMap =
                libraryFunctions |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
            let variantCtorMap =
                variantConstructors |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
            let libraryVariantCtorMap =
                libraryVariantConstructors |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
            let requiredFunctions =
                referencedFunctionMap
                |> Map.toList
                |> List.choose (fun (name, _) ->
                    if userFuncMap.ContainsKey(name)
                       || variantCtorMap.ContainsKey(name)
                       || libraryFuncMap.ContainsKey(name)
                       || libraryVariantCtorMap.ContainsKey(name)
                    then None
                    else Some name)
            let linkedProgram =
                { Program = program
                  LibraryStatements = libraryStatements
                  Functions = referencedFunctionMap
                  RequiredFunctions = requiredFunctions
                  UserFunctions = userFuncMap
                  LibraryFunctions = libraryFuncMap
                  VariantConstructors = variantCtorMap
                  LibraryVariantConstructors = libraryVariantCtorMap }
            Ok linkedProgram

    /// `linkScripts` with the declarations of every library script in one
    /// list, as one source text, and the result assembled through
    /// `Validator.inSourceOrder` (JYRO-ADR-126, JYRO-ADR-127). The
    /// compilation paths call `linkScripts`, which keeps each library's
    /// diagnostics in a group of its own.
    let link
        (program: Program)
        (functions: IJyroFunction seq)
        (libraryStatements: Stmt list)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        : JyroResult<LinkedProgram> =
        match linkScripts program functions [ libraryStatements ] userMaxEffect libraryMaxEffect with
        | Ok linkedProgram -> JyroResult<LinkedProgram>.Success(linkedProgram)
        | Error groups -> JyroResult<LinkedProgram>.Failure(Validator.inSourceOrder groups)

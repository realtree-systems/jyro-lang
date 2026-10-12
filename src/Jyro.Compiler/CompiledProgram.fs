// CompiledProgram, the public type this module makes, is declared in
// Jyro.Core (JYRO-ADR-202 item 1).
namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing

open System
open System.Linq.Expressions
open System.Security.Cryptography
open System.Text
open RealTree.Jyro.Compilation.ExpressionCompiler
open RealTree.Jyro.Compilation.StatementCompiler
open RealTree.Jyro.Compilation.Linker
open RealTree.Jyro.Compilation.BinaryFormat

/// <summary>Module for compiling, executing, and building Jyro programs.</summary>
module internal Compiler =
    /// <summary>Compiles a user function body into a delegate and sets it on the shell.</summary>
    let private compileFuncBody
        (allFunctions: Map<string, IJyroFunction>)
        (unionDefs: Map<string, UnionVariant list>)
        (variantMap: Map<string, string>)
        (funcDef: Stmt)
        (shell: JyroUserFunction) =
        match funcDef with
        | FuncDef(_, parameters, body, _) ->
            let argsParam = Expression.Parameter(typeof<System.Collections.Generic.IReadOnlyList<JyroValue>>, "args")
            let ctxParam = Expression.Parameter(typeof<JyroExecutionContext>, "ctx")

            let returnLabel = Expression.Label(typeof<JyroValue>, "funcReturn")

            // Build inner context: function parameters only, InFunction = true.
            // Input is accessed via ctx.Input (not a lambda param).
            //
            // These two sentinels are never bound by any lambda, so an emitted
            // tree that references one fails to compile. They are unreachable
            // because `compileIdentifier` refuses `Output` under InFunction
            // and routes `Input` through the context - not because the
            // validator got there first, which is not true on the artifact
            // path. Naming them makes an accidental reference obvious.
            let dummyOutput = Expression.Parameter(typeof<JyroValue>, "__noOutput")
            let dummyInput = Expression.Parameter(typeof<JyroValue>, "__noInput")
            let innerCtx =
                { Variables = Map.empty
                  VariableTypes = Map.empty
                  Functions = allFunctions
                  UnionDefinitions = unionDefs
                  VariantToUnion = variantMap
                  InputParam = dummyInput
                  OutputParam = dummyOutput
                  ContextParam = ctxParam
                  BreakLabel = None
                  ContinueLabel = None
                  ReturnLabel = Some returnLabel
                  InFunction = true
                  InLambda = false }

            let itemGetter = EngineMembers.listItem
            let coerceMethod = EngineMembers.coerceToType

            let paramVars = ResizeArray<ParameterExpression>()
            let paramInits = ResizeArray<Expression>()
            let mutable funcCtx = innerCtx

            let isNullProp = EngineMembers.isNull
            let nullConst = Expression.Constant(JyroNull.Instance, typeof<JyroValue>)
            let countProp = EngineMembers.collectionCount
            let argsCount = Expression.Property(Expression.Convert(argsParam, typeof<System.Collections.Generic.IReadOnlyCollection<JyroValue>>), countProp)

            parameters |> List.iteri (fun i (p: FuncParam) ->
                let paramVar = Expression.Variable(typeof<JyroValue>, p.Name)
                paramVars.Add(paramVar)

                let idxConst = Expression.Constant(i)
                let rawGetItem = Expression.Call(argsParam, itemGetter, idxConst)

                // For optional params, bounds-check so callers don't need to pad to MaxArgs
                let getItem =
                    if p.DefaultValue.IsSome then
                        let inBounds = Expression.LessThan(idxConst, argsCount)
                        Expression.Condition(inBounds, rawGetItem, nullConst, typeof<JyroValue>) :> Expression
                    else
                        rawGetItem :> Expression

                let valueExpr =
                    match p.DefaultValue with
                    | Some defaultExpr ->
                        let compiledDefault = compileExpr funcCtx defaultExpr
                        let isNull = Expression.Property(getItem, isNullProp)
                        Expression.Condition(isNull, compiledDefault, getItem, typeof<JyroValue>) :> Expression
                    | None -> getItem

                let initExpr =
                    match p.TypeHint with
                    | Some hint when hint <> AnyType ->
                        let targetType = ExpressionCompiler.jyroTypeToValueType hint
                        Expression.Call(coerceMethod,
                            valueExpr,
                            Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                    | _ -> valueExpr

                paramInits.Add(Expression.Assign(paramVar, initExpr) :> Expression)
                funcCtx <- funcCtx.WithVariable(p.Name, paramVar)
                match p.TypeHint with
                | Some hint -> funcCtx <- funcCtx.WithVariableType(p.Name, hint)
                | None -> ())

            let bodyExpr = compileBlock funcCtx body

            let allExprs = ResizeArray<Expression>()
            allExprs.AddRange(paramInits)
            allExprs.Add(bodyExpr)
            // Return label defaults to null (implicit return)
            allExprs.Add(Expression.Label(returnLabel, Expression.Constant(JyroNull.Instance, typeof<JyroValue>)) :> Expression)

            let funcBody = Expression.Block(typeof<JyroValue>, paramVars, allExprs)
            let funcLambda = Expression.Lambda<Func<System.Collections.Generic.IReadOnlyList<JyroValue>, JyroExecutionContext, JyroValue>>(
                funcBody, argsParam, ctxParam)
            let compiled = funcLambda.Compile()
            shell.SetCompiled(compiled)
        // Callers (see `collectFuncDefs` below) only ever supply FuncDef;
        // everything else is an internal invariant violation.
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
        | Delete _ -> failwith "compileFuncBody requires a FuncDef statement"

    /// <summary>Compiles a linked program into an executable delegate.</summary>
    /// <param name="linkedProgram">The linked program containing resolved function references and a validated AST.</param>
    /// <returns>A <see cref="JyroResult{T}"/> containing the compiled program on success, or diagnostic messages on failure.</returns>
    let compileStaged (stage: CompilationStage) (linkedProgram: LinkedProgram) : JyroResult<CompiledProgram> =
        EngineBinding.ensureLoaded ()
        try
            // Build the combined function map: host/stdlib + user function shells + library function shells + variant constructors
            let allFunctions =
                linkedProgram.UserFunctions
                |> Map.fold (fun acc name shell -> Map.add name (shell :> IJyroFunction) acc) linkedProgram.Functions
            let allFunctions =
                linkedProgram.LibraryFunctions
                |> Map.fold (fun acc name shell -> Map.add name (shell :> IJyroFunction) acc) allFunctions
            let allFunctions =
                linkedProgram.VariantConstructors
                |> Map.fold (fun acc name ctor -> Map.add name (ctor :> IJyroFunction) acc) allFunctions
            let allFunctions =
                linkedProgram.LibraryVariantConstructors
                |> Map.fold (fun acc name ctor -> Map.add name (ctor :> IJyroFunction) acc) allFunctions

            let collectUnions stmts =
                stmts
                |> List.choose (Ast.tryAsUnionDef >> Option.map (fun (name, variants, _) -> name, variants))
            let unionDefs =
                (collectUnions linkedProgram.LibraryStatements @ collectUnions linkedProgram.Program.Statements)
                |> Map.ofList
            let variantMap =
                unionDefs
                |> Map.toSeq
                |> Seq.collect (fun (unionName, variants) ->
                    variants |> List.map (fun v -> v.Name, unionName))
                |> Map.ofSeq

            // Compile library function bodies first so user scripts can invoke them.
            let collectFuncDefs stmts =
                stmts
                |> List.choose (fun stmt ->
                    Ast.tryAsFuncDef stmt |> Option.map (fun (name, _, _, _) -> name, stmt))

            // Each function body is compiled on its own, and a program can
            // hold thousands, so this loop and the next are where an abandoned
            // compilation stops (JYRO-ADR-100).
            for (name, funcDef) in collectFuncDefs linkedProgram.LibraryStatements do
                stage.Check()
                match linkedProgram.LibraryFunctions.TryFind(name) with
                | Some shell -> compileFuncBody allFunctions unionDefs variantMap funcDef shell
                | None -> ()

            // Then user function bodies (before main script body)
            for (name, funcDef) in collectFuncDefs linkedProgram.Program.Statements do
                stage.Check()
                match linkedProgram.UserFunctions.TryFind(name) with
                | Some shell -> compileFuncBody allFunctions unionDefs variantMap funcDef shell
                | None -> ()

            // Create the return label first so statements can reference it
            let returnLabel = Expression.Label(typeof<JyroValue>, "return")
            let baseCtx = CompilationContext.Create(allFunctions, unionDefs = unionDefs, variantMap = variantMap)
            let ctx = baseCtx.WithReturnLabel(returnLabel)
            stage.Check()
            let bodyExpr = compileBlock ctx linkedProgram.Program.Statements

            // Wrap the body to return the Output parameter if no explicit return
            let finalBody = Expression.Block(
                typeof<JyroValue>,
                [| bodyExpr
                   Expression.Label(returnLabel, ctx.OutputParam) |])

            let lambda = Expression.Lambda<Func<JyroValue, JyroValue, JyroExecutionContext, JyroValue>>(
                            finalBody, ctx.InputParam, ctx.OutputParam, ctx.ContextParam)
            let compiled = lambda.Compile()

            let program = CompiledProgram(compiled, Array.ofList linkedProgram.RequiredFunctions)

            JyroResult<CompiledProgram>.Success(program)
        with
        | :? JyroRuntimeException as ex ->
            // A compiler stage refused the AST it was handed. On the source
            // path the validator has already rejected everything that gets
            // here, but the compiler is also reachable without it - a .jyrx
            // artifact skips parsing and validation, and `AddLibraryAst` takes
            // a host-built AST directly. Those inputs are malformed, not
            // symptoms of a compiler bug, so the diagnostic keeps the code the
            // input violated instead of becoming an internal error that invites
            // a bug report.
            JyroResult<CompiledProgram>.Failure(DiagnosticMessage.Of(ex.Code, ex.Args, ex.Location))
        // An abandoned compilation stopping at a check: nobody reads its
        // result, and turning the signal into a diagnostic would hide it.
        | :? OperationCanceledException -> reraise ()
        | _ ->
            // An internal error has the stage that was running (spec
            // 160.1.12, JYRO-ADR-112), which here is always Compile. It has
            // no argument: the exception's type and message are the host
            // platform's, not names this specification gives (spec 130.6.2).
            let msg = DiagnosticMessage.Of(MessageCode.UnknownCompilerError)
            JyroResult<CompiledProgram>.Failure(msg.WithStage MessageStage.Compile)

    /// `compileStaged` for a compilation no one abandons.
    let compile (linkedProgram: LinkedProgram) : JyroResult<CompiledProgram> =
        compileStaged (CompilationStage()) linkedProgram

    /// The arguments of a `RuntimeError` raised now on `ctx` (spec 130.6.2):
    /// the name of the function whose call is the innermost node running,
    /// written with `()` as 90.2 writes a function name, or none.
    let private runtimeErrorArgs (ctx: JyroExecutionContext) : obj[] =
        match ctx.CurrentFunction with
        | null -> Array.empty
        | name -> [| box (name + "()") |]

    /// The copy of the value a runner was given for `Output` with which a run
    /// starts (JYRO-ADR-201 item 7, JYRO-ADR-205): a new array or object for
    /// each writable container of the seed, made once, so a container the
    /// seed reaches by several paths becomes one copy reached by the same
    /// paths, and the copy costs no more than the seed holds; the scalars as
    /// they are. A read-only container is kept, not copied: no run can change
    /// it, and it stays read-only as 70.4.2 requires. The copy is the first
    /// value of `Output`, a value the host supplies, so the maximum value size
    /// does not apply to it (spec 100.8, JYRO-REQ-1444); the nesting bound
    /// does, and a cyclic or too deep seed raises `ValueNestingLimitExceeded`.
    let private copyOutputSeed (seed: JyroValue) : JyroValue =
        // A container's copy is recorded once it is complete, so a container
        // reached again along a later path is the same copy, while one reached
        // again from inside itself (a cycle) is walked again until the
        // nesting bound stops it.
        let copies = System.Collections.Generic.Dictionary<JyroValue, JyroValue>(HashIdentity.Reference)
        let rec copy (value: JyroValue) : JyroValue =
            match value with
            | :? JyroArray as arr when not arr.IsFrozen ->
                match copies.TryGetValue value with
                | true, existing -> existing
                | _ ->
                    ValueRecursionGuard.Enter()
                    try
                        let result = JyroArray(arr.Count)
                        for item in arr.Items do result.Add(copy item)
                        copies.[value] <- result
                        result :> JyroValue
                    finally
                        ValueRecursionGuard.Exit()
            | :? JyroObject as obj when not obj.IsFrozen ->
                match copies.TryGetValue value with
                | true, existing -> existing
                | _ ->
                    ValueRecursionGuard.Enter()
                    try
                        let result = JyroObject(obj.Count)
                        for kvp in obj.Properties do result.SetProperty(kvp.Key, copy kvp.Value)
                        copies.[value] <- result
                        result :> JyroValue
                    finally
                        ValueRecursionGuard.Exit()
            | other -> other
        copy seed

    /// <summary>Executes a compiled program with the given input data and execution context.</summary>
    /// <param name="program">The compiled program to execute.</param>
    /// <param name="input">The input data accessible as Input in the script. This value is read-only from the script's perspective.</param>
    /// <param name="ctx">The execution context providing resource limits, options, and runtime state.</param>
    /// <returns>
    /// A <see cref="JyroResult{T}"/> containing the Output value. Output is always returned even on failure,
    /// since the script may have partially populated it before the error.
    /// </returns>
    let execute (program: CompiledProgram) (input: JyroValue) (ctx: JyroExecutionContext) : JyroExecutionResult =
        // A host may seed Output before the run, so the script edits a structure
        // it was given rather than rebuilding one. Only an unset slot gets the
        // empty object, and the seed is NOT frozen: Output is the script's to
        // write, which is the whole point of handing it over.
        // A seed of `null` is a first value like any other (spec 70.3.1,
        // JYRO-REQ-273): what decides is whether the host set one.
        let output =
            if ctx.OutputSupplied then ctx.Output
            else JyroObject() :> JyroValue
        ctx.Input <- input
        ctx.Output <- output
        // The exit message is this run's: a context handed to a second run
        // must not report the `exit` of the first (spec 160.1.9).
        ctx.ReturnMessage <- null
        // No statement has run yet. A context can be handed to more than one
        // run, and a position left by the last would locate an error this run
        // raises before its first statement.
        ctx.MarkLocation(null)
        // Where a diagnostic is located (spec 130.1.2): the node whose
        // evaluation raised it - the call of a function or a variant, the
        // operator or the access, else the statement - which the compiled
        // code keeps on the context in place of a try/catch around every
        // statement (JYRO-ADR-108). The exception's own location only when no
        // node with a position had started.
        let locate (ex: JyroRuntimeException) =
            match ctx.CurrentLocation with
            | Some location -> Nullable location
            | None -> ex.Location
        // An exception made by `JyroError.Host` is a deliberate failure of a
        // host function, `HostFunctionError`, wherever it was made
        // (JYRO-ADR-192 item 3).
        let codeOf (ex: JyroRuntimeException) =
            if ex.Origin = RuntimeExceptionOrigin.Host then MessageCode.HostFunctionError else ex.Code
        // The operators and the walks of a value are handed no context, and
        // they check the time limit and the cancellation signal as they
        // iterate (spec 100.2.5), so the run installs its check for this
        // thread and hands back the one it replaced when it ends.
        let previousCheck = ExecutionCheck.Install(Action(fun () -> ctx.CheckExecutionLimits()))
        // A run started from inside a host's call check is not itself a call
        // check: a `JyroError.Host` thrown in it is HostFunctionError, not HostLinkError
        // (JYRO-REQ-1677, JYRO-REQ-1671). The scope it replaced comes back
        // when the run ends.
        let previousCallCheck = CallCheckScope.Clear()
        // Every diagnostic of a run has the stage Execute (spec 160.1.12),
        // whatever its code: a code that names a rule of an earlier stage and
        // is raised only now was still raised by the run.
        let failed (msg: DiagnosticMessage) =
            JyroExecutionResult.Of(JyroResult<JyroValue>.Failure(ctx.Output, msg.WithStage MessageStage.Execute))
        // The exit message is set by an `exit` with a value and by nothing
        // else that ends a run successfully: `fail` sets the same slot but
        // never ends in success, and an `exit` with no value leaves it null.
        let succeeded (value: JyroValue) =
            JyroExecutionResult.Of(JyroResult<JyroValue>.Success(value), ctx.ReturnMessage)
        try
            try
                // Input belongs to the host, so the script gets it read-only.
                //
                // The validator already refuses a write whose target is rooted at
                // `Input`, but that is a syntactic check and values are referenced
                // rather than copied: `var t = Input.a` hands the script the host's
                // own object with nothing left to root. Freezing is what closes
                // that, and it has to be the value's own doing.
                //
                // Inside the try because a graph deeper than the value-nesting
                // bound cannot be frozen; that must reach the host as a diagnostic,
                // not as an exception thrown out of Execute.
                JyroValue.FreezeAsInput(input)

                // A runner starts each run from a copy of the value it was
                // given for `Output`, so no run changes it (JYRO-ADR-201 item
                // 7). Made after `Input` is read-only, so a container of the
                // seed that is read-only - reached from this run's `Input` or
                // an earlier one's - is kept, not copied, and stays read-only
                // (spec 70.4.2).
                let output =
                    if ctx.CopyOutputSeed then
                        let copy = copyOutputSeed output
                        ctx.Output <- copy
                        copy
                    else output

                if ctx.HasLimiter then ctx.Limiter.Start()

                let result = program.CompiledFunction.Invoke(input, output, ctx)

                if ctx.HasLimiter then ctx.Limiter.Stop()

                succeeded result
            with
            | :? JyroScriptExit ->
                // `exit` from within a function, which throws to escape the whole
                // call stack. Clean termination, not a failure.
                if ctx.HasLimiter then ctx.Limiter.Stop()
                succeeded ctx.Output
            | :? JyroRuntimeException as ex ->
                if ctx.HasLimiter then ctx.Limiter.Stop()
                failed (DiagnosticMessage.Of(codeOf ex, ex.Args, locate ex))
            // Any other exception raised while a node was running - a host
            // function's own, an `OperationCanceledException` among them - is a
            // RuntimeError located at that node. Its one argument is the name
            // of the function whose call raised it, with `()`, or none where
            // no call did: never the exception's type or message, which are
            // the host platform's, not names this specification gives (spec
            // 130.6.2).
            | _ when ctx.CurrentLocation.IsSome ->
                if ctx.HasLimiter then ctx.Limiter.Stop()
                failed (DiagnosticMessage.Of(
                            MessageCode.RuntimeError,
                            runtimeErrorArgs ctx,
                            Option.toNullable ctx.CurrentLocation))
            | :? OperationCanceledException ->
                if ctx.HasLimiter then ctx.Limiter.Stop()
                failed (DiagnosticMessage.Of(MessageCode.CanceledByHost))
            | _ ->
                if ctx.HasLimiter then ctx.Limiter.Stop()
                failed (DiagnosticMessage.Of(MessageCode.RuntimeError, runtimeErrorArgs ctx))
        finally
            CallCheckScope.Restore(previousCallCheck)
            ExecutionCheck.Restore(previousCheck)

    /// <summary>
    /// Executes a compiled program using a default execution context, with no
    /// execution-time, statement or loop-iteration limit. Call depth is still
    /// bounded at <c>JyroExecutionOptions.MaxSafeCallDepth</c>, because a
    /// context with no limiter opts out of resource policy, not stack safety.
    /// Hosts running untrusted scripts want <c>JyroRunner.Execute</c>, which
    /// applies the full limit set.
    /// </summary>
    /// <param name="program">The compiled program to execute.</param>
    /// <param name="input">The input data accessible as Input in the script.</param>
    /// <returns>
    /// A <see cref="JyroResult{T}"/> containing the Output value.
    /// </returns>
    let executeSimple (program: CompiledProgram) (input: JyroValue) : JyroExecutionResult =
        execute program input (JyroExecutionContext())

    /// Runs `work` and records its time with `setter` when there is a
    /// collector; runs it untimed when there is none.
    let private timed
        (stats: JyroPipelineStats option)
        (setter: JyroPipelineStats -> TimeSpan -> unit)
        (work: unit -> 'T)
        : 'T =
        match stats with
        | Some s -> JyroPipelineStats.TimeStage((fun t -> setter s t), (fun () -> work ()))
        | None -> work ()

    /// The internal error of a stage that returned neither a failure nor a
    /// result, with the stage that was running, `running` (spec 160.1.12,
    /// JYRO-ADR-112). It has no argument: the stage is the diagnostic's
    /// `Stage`, and an argument naming it would be the same in every
    /// occurrence of that stage (spec 130.6.2).
    let private internalStageFailure (running: MessageStage) : DiagnosticMessage =
        DiagnosticMessage.Of(MessageCode.InternalStageFailure).WithStage(running)

    /// A library script of a compilation from source, in the order the host
    /// added it.
    type LibraryInput =
        /// A library given as source text (`AddLibrarySource`): parsed and
        /// validated in the Parse and Validate stages of the compilation,
        /// beside the script.
        | LibrarySource of string
        /// A library's declarations, parsed and validated on their own before
        /// the compilation (`JyroLibrary.Parse`), and whether they hold a
        /// `match` that validation left to the compilation (JYRO-ADR-140).
        | LibraryDeclarations of Stmt list * holdsDeferredMatch: bool

    /// A library after the Parse stage: its program, whether its own
    /// validation is still to run in this compilation, and, for a library
    /// parsed up front, whether it holds a `match` left to the compilation.
    type private ParsedLibrary =
        { Parsed: Program
          OwnValidation: bool
          HoldsDeferredMatch: bool }

    /// The configuration diagnostics of `stage`: those of `config` whose
    /// code the stage table gives that stage (spec 160.1.12, JYRO-ADR-120).
    let private configOf (config: DiagnosticMessage list) (stage: MessageStage) =
        config |> List.filter (fun m -> m.Stage = stage)

    let private hasError (messages: DiagnosticMessage list) =
        messages |> List.exists (fun m -> m.Severity = MessageSeverity.Error)

    /// Parse, Validate and Link of a compilation from source: every library
    /// script and the script go through each stage before any of them goes
    /// to the next, and the compilation stops after the first stage that
    /// reports an error, with every diagnostic of that stage from every
    /// source text (JYRO-ADR-115). A library's diagnostic does not stop the
    /// script's own Parse or Validate (JYRO-DEF-512).
    ///
    /// `config` holds the diagnostics the host's configuration raised rather
    /// than a source text - colliding host functions, a library on a path
    /// that cannot use one. Each is reported with the stage its code has,
    /// and only once every earlier stage has run clean on every source text
    /// (JYRO-ADR-120); those of the Compile stage are the caller's.
    ///
    /// Each stage is published to `stage` before it starts (JYRO-ADR-100),
    /// so that a compilation time limit names the stage that was running
    /// (spec 100.9.3, 160.1.12). The parser gives its own diagnostics the
    /// stage Parse (JYRO-ADR-111); the diagnostics of the later stages have
    /// the stage their code gives (`DiagnosticMessage.StageOf`).
    let private linkSources
        (libraries: LibraryInput list)
        (source: string)
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        (config: DiagnosticMessage list)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<LinkedProgram> =
        // The engine member table first, so a stale name fails every
        // compilation, whatever the script holds (JYRO-ADR-206).
        EngineBinding.ensureLoaded ()
        // === Parse: every library script given as source, then the script ===
        stage.Enter MessageStage.Parse
        let parsedLibraries, parsedScript =
            timed stats (fun s t -> s.ParseTime <- t) (fun () ->
                let libs =
                    libraries
                    |> List.map (fun library ->
                        match library with
                        | LibrarySource text ->
                            (match Parser.parse text with
                             | Produced program -> Ok { Parsed = program; OwnValidation = true; HoldsDeferredMatch = false }
                             | parsed -> Error (List.ofSeq parsed.Messages))
                        | LibraryDeclarations(statements, holdsDeferredMatch) ->
                            Ok { Parsed = ({ Statements = statements } : Program)
                                 OwnValidation = false
                                 HoldsDeferredMatch = holdsDeferredMatch })
                libs, Parser.parse source)
        let parseGroups =
            (parsedLibraries |> List.map (function Error msgs -> msgs | Ok _ -> []))
            @ [ (match parsedScript with Produced _ -> [] | failed -> List.ofSeq failed.Messages) ]
        let parseMessages = Validator.inSourceOrder (parseGroups @ [ configOf config MessageStage.Parse ])
        match parsedScript with
        | Produced program when not (hasError parseMessages) ->
            let libraryPrograms = parsedLibraries |> List.choose (function Ok p -> Some p | Error _ -> None)

            // === Validate: each library's own checks and its declarations
            // against the compilation, then the script, then the host's ===
            stage.Enter MessageStage.Validate
            let validateMessages, libraryStatements =
                timed stats (fun s t -> s.ValidateTime <- t) (fun () ->
                    let libraryDeclarations =
                        libraryPrograms |> List.map (fun p -> LibraryValidator.declarations p.Parsed)
                    let libraryFunctionNames =
                        libraryDeclarations
                        |> List.collect (List.choose (Ast.tryAsFuncDef >> Option.map (fun (name, _, _, _) -> name)))
                        |> Set.ofList
                    let registered = Validator.registeredNames functions libraryFunctionNames
                    // A union of any source text is in scope in all of them
                    // (spec 160.6, JYRO-ADR-121): each source text's union
                    // and variant names are checked against the unions of
                    // the texts before it, in the order of 130.5.4, and its
                    // match cases may name a union of any of them.
                    let unionsBefore =
                        libraryDeclarations
                        |> List.scan Validator.unionTable Validator.UnionTable.Empty
                    let allUnions = Validator.unionTable (List.last unionsBefore) program.Statements
                    let scopeAfter (earlier: Validator.UnionTable) : Validator.UnionScope =
                        { Earlier = earlier; All = allUnions }
                    let libraryGroups, _ =
                        List.zip3 libraryPrograms libraryDeclarations (List.take libraryPrograms.Length unionsBefore)
                        |> List.mapFold (fun earlier (library, declarations, unionsEarlier) ->
                            let own =
                                if library.OwnValidation then
                                    List.ofSeq (LibraryValidator.validateLibraryIn (scopeAfter unionsEarlier) library.Parsed).Messages
                                elif library.HoldsDeferredMatch then
                                    // A library parsed up front that left a
                                    // match to the compilation: its union and
                                    // variant names and its matches are
                                    // checked here, in this compilation's
                                    // scope (JYRO-ADR-140 point 3).
                                    Validator.checkParsedLibrary (scopeAfter unionsEarlier) declarations
                                else
                                    // One that left none: its union and
                                    // variant names against the earlier source
                                    // texts' (JYRO-ADR-140 point 2). When an
                                    // earlier text declares one of them, its
                                    // own matches resolve against that text's
                                    // union, as from source, so it is checked
                                    // as one that left a match, and only that
                                    // check's diagnostics are kept
                                    // (JYRO-ADR-141).
                                    match Validator.checkUnionDeclarations unionsEarlier declarations with
                                    | [] -> []
                                    | _ -> Validator.checkParsedLibrary (scopeAfter unionsEarlier) declarations
                            let crossSource, names =
                                Validator.checkLibraryDeclarations registered earlier library.OwnValidation declarations
                            own @ crossSource, names) Validator.LibraryNames.Empty
                    let scriptMessages =
                        List.ofSeq (Validator.validateIn registered (scopeAfter (List.last unionsBefore)) program).Messages
                    // One call orders and deduplicates every group, the
                    // host's configuration diagnostics last (JYRO-ADR-126).
                    Validator.inSourceOrder (libraryGroups @ [ scriptMessages; configOf config MessageStage.Validate ]),
                    libraryDeclarations)
            if hasError validateMessages then
                JyroResult<LinkedProgram>.Failure(validateMessages)
            else
                // === Link: the linker's checks, one group per source text,
                // then the host's, ordered as Parse and Validate are
                // (JYRO-ADR-127) ===
                stage.Enter MessageStage.Link
                let linkConfig = configOf config MessageStage.Link
                match timed stats (fun s t -> s.LinkTime <- t) (fun () -> linkScripts program functions libraryStatements userMaxEffect libraryMaxEffect) with
                | Error groups ->
                    JyroResult<LinkedProgram>.Failure(Validator.inSourceOrder (groups @ [ linkConfig ]))
                | Ok linkedProgram when linkConfig.IsEmpty ->
                    JyroResult<LinkedProgram>.Success(linkedProgram)
                | Ok _ ->
                    JyroResult<LinkedProgram>.Failure(Validator.inSourceOrder [ linkConfig ])
        | Produced _
        | Failed _ ->
            JyroResult<LinkedProgram>.Failure(parseMessages)
        | _ -> JyroResult<LinkedProgram>.Failure(internalStageFailure MessageStage.Parse)

    /// A compilation from source of `source` with the library scripts
    /// `libraries`, to a compiled program: Parse, Validate and Link of every
    /// source text (`linkSources`), then Compile. `config` holds the
    /// diagnostics of the host's configuration, each reported in the stage
    /// its code has (JYRO-ADR-120).
    let compileFromSourcesStaged
        (libraries: LibraryInput list)
        (source: string)
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        (config: DiagnosticMessage list)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<CompiledProgram> =
        match linkSources libraries source functions userMaxEffect libraryMaxEffect config stats stage with
        | Failed msgs ->
            JyroResult<CompiledProgram>.Failure(msgs)
        | Produced linkedProgram ->
            stage.Enter MessageStage.Compile
            match configOf config MessageStage.Compile with
            | [] -> timed stats (fun s t -> s.CompileTime <- t) (fun () -> compileStaged stage linkedProgram)
            | refusals -> JyroResult<CompiledProgram>.Failure(Validator.inSourceOrder [ refusals ])
        | _ -> JyroResult<CompiledProgram>.Failure(internalStageFailure MessageStage.Link)

    /// The source path with the declarations of already validated libraries,
    /// `libraryStatements`, and no configuration diagnostics.
    let compileFromSourceStaged
        (source: string)
        (functions: IJyroFunction seq)
        (libraryStatements: Stmt list)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<CompiledProgram> =
        // Whether these declarations hold a match left to the compilation is
        // not known here, so their matches are checked as for one that does
        // (JYRO-ADR-140 point 3).
        let libraries = if libraryStatements.IsEmpty then [] else [ LibraryDeclarations(libraryStatements, true) ]
        compileFromSourcesStaged libraries source functions userMaxEffect libraryMaxEffect [] stats stage

    /// <summary>
    /// Runs the full compilation pipeline from source code to a compiled program.
    /// Performs parsing, validation, linking, and compilation in sequence.
    /// </summary>
    /// <param name="source">The Jyro source code to compile.</param>
    /// <param name="functions">The available functions (stdlib and custom) to link against.</param>
    /// <param name="libraryStatements">Pre-parsed AST nodes from library scripts. Library function declarations are merged into the link scope so user scripts can call them transparently.</param>
    /// <param name="userMaxEffect">Strongest observable effect tolerated by user-script call sites.</param>
    /// <param name="libraryMaxEffect">Strongest observable effect tolerated by library-body call sites.</param>
    /// <returns>A <see cref="JyroResult{T}"/> containing the compiled program on success, or diagnostic messages from the first failing stage.</returns>
    let compileFromSource
        (source: string)
        (functions: IJyroFunction seq)
        (libraryStatements: Stmt list)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        : JyroResult<CompiledProgram> =
        compileFromSourceStaged source functions libraryStatements userMaxEffect libraryMaxEffect None (CompilationStage())

    /// Shared body of `compileToJyrx` and `compileToJyrxTimed`. One function
    /// with an optional collector rather than the copy-and-add-timing pattern
    /// used by the source and load paths: the artifact writer is the one stage
    /// whose timing was silently dropped, and a second copy is where the next
    /// such gap would open. `None` takes no timings and allocates nothing.
    ///
    /// Parse, Validate, Link and Compile are recorded. Writing the artifact is
    /// the Compile stage, so its time is the `Compile` time (JYRO-REQ-1452,
    /// 1455: zero is only for a stage that did not run); `Execute` stays zero.
    ///
    /// Each stage is published to `stage` before it starts, as on the source
    /// path. Writing the artifact is the Compile stage (spec 160.1.12): its
    /// refusals have the stage Compile.
    ///
    /// `libraries` are the compilation's library scripts. An artifact cannot
    /// encode them, and the caller says so with `ArtifactCannotEncodeLibraries`
    /// in `config`, a Compile refusal reported only once Parse, Validate and
    /// Link of the libraries and the script have run clean (JYRO-ADR-120), so
    /// the script is linked with them in scope.
    let compileToJyrxFromSourcesStaged
        (libraries: LibraryInput list)
        (source: string)
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        (config: DiagnosticMessage list)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<byte[]> =
        match linkSources libraries source functions userMaxEffect libraryMaxEffect config stats stage with
        | Failed msgs ->
            JyroResult<byte[]>.Failure(msgs)
        | Produced linkedProgram ->
            stage.Enter MessageStage.Compile
            match configOf config MessageStage.Compile with
            | _ :: _ as refusals -> JyroResult<byte[]>.Failure(Validator.inSourceOrder [ refusals ])
            | [] ->
                timed stats (fun s t -> s.CompileTime <- t) (fun () ->
                    let sourceHash = SHA256.HashData(Encoding.UTF8.GetBytes(source))
                    serialize linkedProgram.Program linkedProgram.RequiredFunctions sourceHash)
        | _ -> JyroResult<byte[]>.Failure(internalStageFailure MessageStage.Link)

    /// The artifact writer with no libraries and no configuration diagnostics.
    let compileToJyrxStaged
        (source: string)
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<byte[]> =
        compileToJyrxFromSourcesStaged [] source functions userMaxEffect FunctionEffect.NonDeterministic [] stats stage

    /// <summary>
    /// Compiles source code to .jyrx binary format. Performs the full pipeline (parse, validate, link)
    /// then serializes the validated AST and function dependency table. The .jyrx format does not
    /// currently encode library context, so this path links with an empty library set - which is also
    /// why it takes no library effect ceiling: with no library bodies there are no library call sites
    /// for one to constrain.
    /// </summary>
    /// <param name="source">The Jyro source code to compile.</param>
    /// <param name="functions">The available functions (stdlib and custom) to link against.</param>
    /// <param name="userMaxEffect">Strongest observable effect tolerated by user-script call sites.</param>
    /// <returns>A <see cref="JyroResult{T}"/> containing the .jyrx byte array on success.</returns>
    let compileToJyrx (source: string) (functions: IJyroFunction seq) (userMaxEffect: FunctionEffect) : JyroResult<byte[]> =
        compileToJyrxStaged source functions userMaxEffect None (CompilationStage())

    /// <summary>Compiles source code to .jyrx binary format, recording the timing of each stage it runs.</summary>
    let compileToJyrxTimed (source: string) (functions: IJyroFunction seq) (userMaxEffect: FunctionEffect) (stats: JyroPipelineStats) : JyroResult<byte[]> =
        compileToJyrxStaged source functions userMaxEffect (Some stats) (CompilationStage())

    /// Rebuilds a `LinkedProgram` from a deserialized artifact: validates the
    /// deserialized AST, resolves the required functions against the host's
    /// registry, and re-applies the effect ceiling. Shared by the timed and
    /// untimed load paths so each of those is enforced in exactly one place.
    ///
    /// The ceiling is checked here rather than trusted from build time because
    /// an artifact records no ceiling: the host loading it may be a different
    /// host with a stricter policy than whoever compiled it. Without this,
    /// `.jyrx` is a hole straight through `WithMaxEffect`, since the load path
    /// skips the linker entirely.
    ///
    /// Validation is here for the same reason, and it was missing for longer.
    /// An artifact is a file: the bytes that arrive are not necessarily the
    /// bytes some earlier run of this compiler wrote, and the header carries a
    /// hash of the *source*, not a signature over the AST. Skipping the
    /// validator therefore made every rule it holds - Input is read-only, a
    /// name is declared once in a scope, a match over a union covers it -
    /// conditional on the loading host having compiled the artifact itself.
    /// Worse, the expression compiler downstream assumes a validated AST, so a
    /// tampered one did not fail cleanly: a duplicate declaration reached an
    /// `InvalidOperationException` and was reported as an internal compiler
    /// error inviting the host to open an issue about a file it had been sent.
    ///
    /// Both checks sit inside this function rather than in its two callers
    /// precisely because there are two callers - `compileFromJyrx` and
    /// `compileFromJyrxTimed`, the second reached by `WithStats`. A guard
    /// added to one of them is a guard the other does not have, which is how
    /// the effect ceiling came to be missing here in the first place.
    ///
    /// Checking the required-function set is equivalent to the linker's
    /// per-call-site walk on this path: `RequiredFunctions` is exactly the set
    /// of host and stdlib functions the program references, and an artifact
    /// carries no library statements, so every call site is a user call site
    /// governed by the one ceiling.
    ///
    /// The program's declarations go through `Linker.declare`, the function
    /// `Linker.linkScripts` calls for its own, so a load makes every Link check of a
    /// declaration's name a compilation from source makes -
    /// `VariantShadowsFunction` against the loading host's functions, and
    /// `FunctionShadowsVariant` - and builds the user-function shells and
    /// variant constructors the same way (JYRO-ADR-114, JYRO-ADR-116). The
    /// per-call argument checks of 80.7.2 to 80.7.6 stay off a load
    /// (JYRO-ADR-62, JYRO-ADR-114 point 4).
    let private relinkArtifactCore
        (deserialized: DeserializedProgram)
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        : JyroResult<LinkedProgram> =
        let funcMap = functions |> Seq.map (fun f -> f.Name, f) |> Map.ofSeq
        let declaredCtx, userFunctions, variantConstructors =
            Linker.declare
                { LinkContext.Empty with HostFunctions = funcMap |> Map.keys |> Set.ofSeq }
                (System.Collections.Generic.Dictionary<string, JyroUserFunction>())
                (System.Collections.Generic.Dictionary<string, JyroVariantConstructor>())
                deserialized.Program.Statements
        // Rows 9 and 10 of spec 150.2.9, in that order, each one complete
        // before the load ends: every table name that no registered function
        // has, in table order, and the load ends there, without row 10; then
        // every table function whose effect is above the loading host's
        // maximum (120.6.2), in table order, and the load ends there. They
        // were one list, so a table that failed both rows gave both codes.
        let undefinedErrors =
            deserialized.RequiredFunctions
            |> List.choose (fun name ->
                // Each name resolved is where an abandoned load stops
                // (JYRO-ADR-100): a table holds up to 1,000 names.
                CompilationStage.CheckAmbient()
                match funcMap.TryFind(name) with
                | None ->
                    Some (DiagnosticMessage.Of(MessageCode.UndefinedFunction, args = [| box name |]))
                | Some _ -> None)
        let effectErrors () =
            deserialized.RequiredFunctions
            |> List.choose (fun name ->
                // Each reads a host function's effect (JYRO-ADR-100).
                CompilationStage.CheckAmbient()
                let func = funcMap.[name]
                match Linker.tryReadEffect func with
                | None ->
                    // The maximum cannot be applied to it (JYRO-REQ-1723):
                    // the function, then the loading host's maximum, with
                    // no place, in table order with the row's other refusals.
                    Some (DiagnosticMessage.Of(
                            MessageCode.MaxEffectNotApplicable,
                            args = [| box name
                                      box (FunctionEffect.name userMaxEffect) |]))
                | Some effect when FunctionEffect.exceeds effect userMaxEffect ->
                    Some (DiagnosticMessage.Of(
                            MessageCode.ArtifactEffectExceedsMax,
                            args = [| box name
                                      box (FunctionEffect.name effect)
                                      box (FunctionEffect.name userMaxEffect) |]))
                | Some _ -> None)
        let otherErrors () =
            let declarationErrors = declaredCtx.Messages |> List.rev
            // The call checks of the host functions (JYRO-ADR-130), as on
            // the source path: reached only once every name has resolved
            // within the maximum effect.
            let checkedFunctions =
                deserialized.RequiredFunctions
                |> List.map (fun name -> name, funcMap.[name])
                |> Map.ofList
            declarationErrors @ Linker.checkCallsOnLoad checkedFunctions deserialized.Program.Statements
        let errors =
            if not undefinedErrors.IsEmpty then undefinedErrors
            else
                match effectErrors () with
                | [] -> otherErrors ()
                | effects -> effects
        if not errors.IsEmpty then
            JyroResult<LinkedProgram>.Failure(errors)
        else
            let resolvedFunctions =
                deserialized.RequiredFunctions
                |> List.map (fun name -> name, funcMap.[name])
                |> Map.ofList
            JyroResult<LinkedProgram>.Success(
                { Program = deserialized.Program
                  LibraryStatements = []
                  Functions = resolvedFunctions
                  RequiredFunctions = deserialized.RequiredFunctions
                  UserFunctions = userFunctions |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
                  LibraryFunctions = Map.empty
                  VariantConstructors = variantConstructors |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
                  LibraryVariantConstructors = Map.empty })

    /// `relinkArtifactCore` with the validator in front of it. The stage is
    /// timed into the collector when one is supplied, so a `WithStats` load
    /// reports a Validate time rather than reporting zero and implying the
    /// stage was skipped - which, until this was added, it was.
    ///
    /// `config` holds the diagnostics of the loading host's configuration
    /// (colliding host functions), each reported with the stage its code has,
    /// after Deserialize and every earlier stage have run clean
    /// (JYRO-ADR-120, JYRO-DEF-524).
    let private relinkArtifact
        (deserialized: DeserializedProgram)
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (config: DiagnosticMessage list)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<LinkedProgram> =
        stage.Enter MessageStage.Validate
        // The loading host's registered names, not the writer's, decide
        // FunctionOverride on a load (JYRO-ADR-114, JYRO-ADR-116); an
        // artifact carries no libraries.
        let registered = Validator.registeredNames functions Set.empty
        let validated =
            timed stats (fun s t -> s.ValidateTime <- t) (fun () -> Validator.validateWith registered deserialized.Program)
        let validateMessages =
            Validator.inSourceOrder [ List.ofSeq validated.Messages; configOf config MessageStage.Validate ]
        match validated with
        | Produced validatedProgram when not (hasError validateMessages) ->
            // Resolving the table and applying the loading host's maximum
            // effect are the Link stage of a load (spec 150.1.5, 150.2.9 rows
            // 9 and 10), and are published as such, so a time limit reached
            // here names Link and not the stage before it.
            stage.Enter MessageStage.Link
            let linkConfig = configOf config MessageStage.Link
            // Timed as the Link stage (spec 150.1.5, 110.1.1): a load that
            // resolved its table recorded no Link time at all.
            let relinked =
                timed stats (fun s t -> s.LinkTime <- t) (fun () ->
                    relinkArtifactCore { deserialized with Program = validatedProgram } functions userMaxEffect)
            match relinked with
            // An artifact is one source text: its Link diagnostics are one
            // group, the host's after it (JYRO-ADR-127).
            | Failed msgs -> JyroResult<LinkedProgram>.Failure(Validator.inSourceOrder [ List.ofSeq msgs; linkConfig ])
            | Produced linkedProgram when linkConfig.IsEmpty -> JyroResult<LinkedProgram>.Success(linkedProgram)
            | Produced _ -> JyroResult<LinkedProgram>.Failure(Validator.inSourceOrder [ linkConfig ])
            | _ -> JyroResult<LinkedProgram>.Failure(internalStageFailure MessageStage.Link)
        | Produced _
        | Failed _ -> JyroResult<LinkedProgram>.Failure(validateMessages)
        | _ ->
            JyroResult<LinkedProgram>.Failure(internalStageFailure MessageStage.Validate)

    /// <summary>
    /// Loads and compiles a program from .jyrx binary format. Deserializes the AST, validates it,
    /// resolves functions by name against the provided registry, and compiles the Expression Tree.
    /// Skips parsing only - the artifact carries the AST the parser would have produced, but not
    /// the guarantee that it is one the validator accepts, so validation runs here as it does on
    /// the source paths.
    /// </summary>
    /// <param name="data">The .jyrx binary data.</param>
    /// <param name="functions">The available functions (stdlib and custom) to resolve against.</param>
    /// <param name="userMaxEffect">Strongest observable effect tolerated by user-script call sites.</param>
    /// <returns>A <see cref="JyroResult{T}"/> containing the compiled program on success.</returns>
    ///
    /// Each stage is published to `stage` before it starts (JYRO-ADR-100):
    /// Deserialize, Validate, Link and Compile (spec 150.1.5). A refusal met
    /// while the bytes are read has the stage Deserialize whatever its code
    /// (spec 160.1.12).
    ///
    /// `config` holds the diagnostics of the loading host's configuration,
    /// reported with the stage each code has once every earlier stage has
    /// run clean (JYRO-ADR-120): never before Deserialize, which is a load's
    /// first stage (150.1.5).
    let compileFromJyrxConfiguredStaged
        (data: byte[])
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (config: DiagnosticMessage list)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<CompiledProgram> =
        // The engine member table first, as on the source path (JYRO-ADR-206).
        EngineBinding.ensureLoaded ()
        stage.Enter MessageStage.Deserialize
        match timed stats (fun s t -> s.DeserializeTime <- t) (fun () -> deserialize data) with
        | Failed msgs ->
            JyroResult<CompiledProgram>.Failure(stage.Stamp msgs)
        | Produced deserialized ->
            // Validate is timed inside `relinkArtifact` and reported under its
            // own stage; what remains there - resolving names and re-applying
            // the ceiling - is the Link stage.
            match relinkArtifact deserialized functions userMaxEffect config stats stage with
            | Failed msgs ->
                JyroResult<CompiledProgram>.Failure(msgs)
            | Produced linkedProgram ->
                stage.Enter MessageStage.Compile
                match configOf config MessageStage.Compile with
                | [] -> timed stats (fun s t -> s.CompileTime <- t) (fun () -> compileStaged stage linkedProgram)
                | refusals -> JyroResult<CompiledProgram>.Failure(Validator.inSourceOrder [ refusals ])
            | _ -> JyroResult<CompiledProgram>.Failure(internalStageFailure stage.Current)
        | _ ->
            JyroResult<CompiledProgram>.Failure(
                // No argument: nothing of the artifact names what went wrong
                // (spec 130.6.2).
                DiagnosticMessage.Of(MessageCode.ArtifactUnreadable))

    /// The load path with no configuration diagnostics.
    let compileFromJyrxStaged
        (data: byte[])
        (functions: IJyroFunction seq)
        (userMaxEffect: FunctionEffect)
        (stats: JyroPipelineStats option)
        (stage: CompilationStage)
        : JyroResult<CompiledProgram> =
        compileFromJyrxConfiguredStaged data functions userMaxEffect [] stats stage

    let compileFromJyrx (data: byte[]) (functions: IJyroFunction seq) (userMaxEffect: FunctionEffect) : JyroResult<CompiledProgram> =
        compileFromJyrxStaged data functions userMaxEffect None (CompilationStage())

    /// <summary>Compiles from source with per-stage timing recorded into the stats collector.</summary>
    let compileFromSourceTimed
        (source: string)
        (functions: IJyroFunction seq)
        (libraryStatements: Stmt list)
        (userMaxEffect: FunctionEffect)
        (libraryMaxEffect: FunctionEffect)
        (stats: JyroPipelineStats)
        : JyroResult<CompiledProgram> =
        compileFromSourceStaged source functions libraryStatements userMaxEffect libraryMaxEffect (Some stats) (CompilationStage())

    /// <summary>Loads and compiles from .jyrx binary format with per-stage timing.</summary>
    let compileFromJyrxTimed (data: byte[]) (functions: IJyroFunction seq) (userMaxEffect: FunctionEffect) (stats: JyroPipelineStats) : JyroResult<CompiledProgram> =
        compileFromJyrxStaged data functions userMaxEffect (Some stats) (CompilationStage())

    /// <summary>Executes a compiled program with timing for the execute stage.</summary>
    let executeTimed (program: CompiledProgram) (input: JyroValue) (ctx: JyroExecutionContext) (stats: JyroPipelineStats) : JyroExecutionResult =
        JyroPipelineStats.TimeStage((fun t -> stats.ExecuteTime <- t), fun () -> execute program input ctx)

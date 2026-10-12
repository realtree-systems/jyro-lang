namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast

open System
open System.Linq.Expressions
open System.Reflection
open System.Collections.Generic

/// Compiles Jyro expressions to LINQ Expression Trees
module internal ExpressionCompiler =
    /// Compilation context for tracking variables, functions, and execution context
    type CompilationContext =
        { Variables: Map<string, ParameterExpression>
          VariableTypes: Map<string, JyroType>
          Functions: Map<string, IJyroFunction>
          UnionDefinitions: Map<string, UnionVariant list>
          VariantToUnion: Map<string, string>
          InputParam: ParameterExpression
          OutputParam: ParameterExpression
          ContextParam: ParameterExpression
          BreakLabel: LabelTarget option
          ContinueLabel: LabelTarget option
          ReturnLabel: LabelTarget option
          InFunction: bool
          // Tracked separately from InFunction because a lambda body sees the
          // enclosing scope's parameters but is subject to the same
          // Output-access rule as a function body (clause 40.11). The
          // validator carries the same pair.
          InLambda: bool }

        static member Create(functions: Map<string, IJyroFunction>, ?unionDefs: Map<string, UnionVariant list>, ?variantMap: Map<string, string>) =
            let inputParam = Expression.Parameter(typeof<JyroValue>, "Input")
            let outputParam = Expression.Parameter(typeof<JyroValue>, "Output")
            let contextParam = Expression.Parameter(typeof<JyroExecutionContext>, "ctx")
            { Variables = Map.empty
              VariableTypes = Map.empty
              Functions = functions
              UnionDefinitions = defaultArg unionDefs Map.empty
              VariantToUnion = defaultArg variantMap Map.empty
              InputParam = inputParam
              OutputParam = outputParam
              ContextParam = contextParam
              BreakLabel = None
              ContinueLabel = None
              ReturnLabel = None
              InFunction = false
              InLambda = false }

        /// A new binding of `name`. It has no type hint of its own until
        /// `WithVariableType` gives it one: a hint belongs to the declaration,
        /// not the name, so a binding that shadows a hinted outer variable
        /// does not take the outer hint (50.1, JYRO-REQ-193, JYRO-REQ-194).
        member this.WithVariable(name: string, param: ParameterExpression) =
            { this with
                Variables = this.Variables.Add(name, param)
                VariableTypes = this.VariableTypes.Remove(name) }

        /// The type hint of the binding of `name` just made by `WithVariable`.
        member this.WithVariableType(name: string, jyroType: JyroType) =
            { this with VariableTypes = this.VariableTypes.Add(name, jyroType) }

        member this.WithLoopLabels(breakLabel: LabelTarget, continueLabel: LabelTarget) =
            { this with BreakLabel = Some breakLabel; ContinueLabel = Some continueLabel }

        member this.WithReturnLabel(returnLabel: LabelTarget) =
            { this with ReturnLabel = Some returnLabel }

    // Every member compiled code calls comes from the engine member table
    // (EngineMembers, JYRO-ADR-206), resolved once when the compiler loads.
    let internal contextOutputSetter = EngineMembers.contextOutputSetter
    let private checkAndEnterCallMethod = EngineMembers.checkAndEnterCall
    let private exitCallMethod = EngineMembers.exitCall
    let private onFunctionResultMethod = EngineMembers.onFunctionResult

    // The place of the running node that the compiled code keeps on the
    // context, in place of a try/catch around every statement (JYRO-ADR-108).
    let internal markLocationMethod = EngineMembers.markLocation
    let private runningLocationProp = EngineMembers.runningLocation
    let private runningCallWithinMethod = EngineMembers.runningCallWithin

    /// The store of a node's place (spec 130.1.2) on the context, boxed once
    /// here so that the compiled code stores one reference; nothing for a
    /// node with no position (`Position.None`, a host-built AST). A node read
    /// from an artifact has the position its writer recorded, whatever its
    /// values (spec 150.4.7).
    let internal markNode (ctx: CompilationContext) (pos: Position) : Expression option =
        if pos = Position.None then None
        else
            Some (Expression.Call(
                    ctx.ContextParam,
                    markLocationMethod,
                    Expression.Constant(box pos.Location, typeof<obj>)) :> Expression)

    /// `work` run as the node at `pos`: its operands are evaluated first, into
    /// the locals `operands` names, then the node's place is stored, so that
    /// an error the work raises is located at this node and not at the
    /// statement or an operand (spec 130.1.2), and the place that was stored
    /// before is given back when the work returns, so that what the enclosing
    /// node does next is located at itself. Nothing gives it back when the
    /// work throws: the place left is the innermost node running.
    let internal asNode
        (ctx: CompilationContext)
        (pos: Position)
        (operands: (ParameterExpression * Expression) list)
        (work: Expression)
        : Expression =
        let assigns = operands |> List.map (fun (v, e) -> Expression.Assign(v, e) :> Expression)
        let vars = operands |> List.map fst
        match markNode ctx pos with
        | None ->
            Expression.Block(typeof<JyroValue>, vars, assigns @ [ work ]) :> Expression
        | Some mark ->
            let saved = Expression.Variable(typeof<obj>, "enclosingLocation")
            let result = Expression.Variable(typeof<JyroValue>, "nodeResult")
            Expression.Block(
                typeof<JyroValue>,
                vars @ [ saved; result ],
                assigns
                @ [ Expression.Assign(saved, Expression.Property(ctx.ContextParam, runningLocationProp)) :> Expression
                    mark
                    Expression.Assign(result, work) :> Expression
                    Expression.Call(ctx.ContextParam, markLocationMethod, saved) :> Expression
                    result :> Expression ]) :> Expression

    // The engine's bounded members a compiled script calls: the string form
    // of a value (spec 100.8.7) and a write that can grow an array or an
    // object (spec 100.8.7, 100.8.8).
    let internal scriptStringMethod = EngineMembers.scriptString
    let internal scriptSetPropertyMethod = EngineMembers.scriptSetProperty
    let internal scriptSetIndexMethod = EngineMembers.scriptSetIndex

    // Type hint coercion.
    let internal coerceToTypeMethod = EngineMembers.coerceToType

    // Increment and decrement.
    let internal evaluateBinaryMethod = EngineMembers.evaluateBinary
    let private getPropertyMethod = EngineMembers.getProperty
    // A script's write to a property or an index goes through the bounded
    // setters, which refuse to grow an array or an object past the maximum
    // value size (spec 100.8.7, 100.8.8); a host's `SetProperty` does not.
    let private setPropertyMethod = scriptSetPropertyMethod
    let internal getIndexMethod = EngineMembers.getIndex
    let private setIndexMethod = scriptSetIndexMethod
    // The exact 1 (spec 30.2.5), so `t++` and `t--` leave the value and the
    // representation of `t + 1` and `t - 1` (JYRO-REQ-1727).
    let private oneConst = Expression.Constant(JyroNumber(1M) :> JyroValue, typeof<JyroValue>)

    /// Compile a literal value to an expression
    let private compileLiteral (value: JyroValue) : Expression =
        Expression.Constant(value, typeof<JyroValue>) :> Expression

    /// Compile an identifier reference
    let private compileIdentifier (ctx: CompilationContext) (name: string) : Expression =
        if name = "Input" then
            if ctx.InFunction then
                Expression.Property(ctx.ContextParam, EngineMembers.contextInput) :> Expression
            else
                ctx.InputParam :> Expression
        elif name = "Output" then
            // Clause 40.11: no function or lambda body may reference Output.
            // The validator refuses this, but the compiler also runs on ASTs
            // it never saw - a .jyrx artifact, or a host-built library AST -
            // and this is the rule that most needs restating here. In a
            // function body the emitted tree referenced an unbound sentinel
            // parameter and surfaced as an internal compiler error; in a
            // top-level lambda body OutputParam was genuinely in scope, so the
            // reference compiled and ran, which is exactly the back door
            // clause 40.11 exists to close.
            if ctx.InFunction || ctx.InLambda then
                JyroError.raiseRuntime MessageCode.OutputAccessInFunction [||]
            else
                ctx.OutputParam :> Expression
        else
            match ctx.Variables.TryFind(name) with
            | Some param -> param :> Expression
            | None -> JyroError.raiseRuntime MessageCode.UndeclaredVariable [| box name |]

    /// Compile a binary operation
    let rec private compileBinary (ctx: CompilationContext) (left: Expr) (op: BinaryOp) (right: Expr) (pos: Position) : Expression =
        match op with
        | And ->
            // Short-circuit: if left is falsy return left, else return right
            let leftExpr = compileExpr ctx left
            let leftVar = Expression.Variable(typeof<JyroValue>, "andLeft")
            let truthinessMethod = EngineMembers.isTruthy
            let testExpr = Expression.Call(leftVar, truthinessMethod)
            let rightExpr = compileExpr ctx right
            Expression.Block(
                [| leftVar |],
                [| Expression.Assign(leftVar, leftExpr) :> Expression
                   Expression.Condition(testExpr, rightExpr, leftVar :> Expression, typeof<JyroValue>) :> Expression |]) :> Expression
        | Or ->
            // Short-circuit: if left is truthy return left, else return right
            let leftExpr = compileExpr ctx left
            let leftVar = Expression.Variable(typeof<JyroValue>, "orLeft")
            let truthinessMethod = EngineMembers.isTruthy
            let testExpr = Expression.Call(leftVar, truthinessMethod)
            let rightExpr = compileExpr ctx right
            Expression.Block(
                [| leftVar |],
                [| Expression.Assign(leftVar, leftExpr) :> Expression
                   Expression.Condition(testExpr, leftVar :> Expression, rightExpr, typeof<JyroValue>) :> Expression |]) :> Expression
        | Coalesce ->
            // Short-circuit: if left is null return right, else return left
            // (without evaluating right).
            let leftExpr = compileExpr ctx left
            let leftVar = Expression.Variable(typeof<JyroValue>, "coalesceLeft")
            let isNullProp = EngineMembers.isNull
            let testExpr = Expression.Property(leftVar, isNullProp)
            let rightExpr = compileExpr ctx right
            Expression.Block(
                [| leftVar |],
                [| Expression.Assign(leftVar, leftExpr) :> Expression
                   Expression.Condition(testExpr, rightExpr, leftVar :> Expression, typeof<JyroValue>) :> Expression |]) :> Expression
        // Every other binary op compiles to a single EvaluateBinary call -
        // no short-circuit semantics needed.
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | Concat
        | Equal
        | NotEqual
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual ->
            // The operator is the node an error of the operation is located
            // at (spec 130.1.2), so both operands are evaluated before its
            // place is stored.
            let leftVar = Expression.Variable(typeof<JyroValue>, "binaryLeft")
            let rightVar = Expression.Variable(typeof<JyroValue>, "binaryRight")
            let opValue = Expression.Constant(op, typeof<BinaryOp>)
            asNode ctx pos
                [ leftVar, compileExpr ctx left
                  rightVar, compileExpr ctx right ]
                (Expression.Call(leftVar, EngineMembers.evaluateBinary, opValue, rightVar))

    /// Compile a unary operation
    and private compileUnary (ctx: CompilationContext) (op: UnaryOp) (operand: Expr) (pos: Position) : Expression =
        let operandVar = Expression.Variable(typeof<JyroValue>, "unaryOperand")
        let opValue = Expression.Constant(op, typeof<UnaryOp>)
        asNode ctx pos
            [ operandVar, compileExpr ctx operand ]
            (Expression.Call(operandVar, EngineMembers.evaluateUnary, opValue))

    /// Compile a ternary/conditional expression
    and private compileTernary (ctx: CompilationContext) (cond: Expr) (thenExpr: Expr) (elseExpr: Expr) : Expression =
        let condExpr = compileExpr ctx cond
        let truthinessMethod = EngineMembers.isTruthy
        let testExpr = Expression.Call(condExpr, truthinessMethod)
        let thenCompiled = compileExpr ctx thenExpr
        let elseCompiled = compileExpr ctx elseExpr
        Expression.Condition(testExpr, thenCompiled, elseCompiled, typeof<JyroValue>) :> Expression

    /// Compile a function call with call depth tracking
    and private compileCall (ctx: CompilationContext) (name: string) (callArgs: CallArgs) (pos: Position) : Expression =
        match ctx.Functions.TryFind(name) with
        | Some func ->
            let compiledArgs =
                match callArgs with
                | PositionalArgs exprs ->
                    exprs |> List.map (compileExpr ctx) |> Array.ofList
                | NamedArgs pairs ->
                    // Reorder named args into parameter-declaration order, trim trailing omitted
                    let argMap = pairs |> Map.ofList
                    let reordered =
                        func.Signature.Parameters
                        |> Seq.map (fun param ->
                            match argMap.TryFind(param.Name) with
                            | Some expr -> Some (compileExpr ctx expr)
                            | None -> None)
                        |> List.ofSeq
                    // Find last provided arg index; trim trailing omitted so args.Count is correct
                    let lastIdx =
                        reordered
                        |> List.mapi (fun i opt -> (i, opt))
                        |> List.filter (fun (_, opt) -> opt.IsSome)
                        |> List.tryLast
                        |> Option.map fst
                        |> Option.defaultValue -1
                    reordered
                    |> List.take (lastIdx + 1)
                    |> List.map (Option.defaultValue (Expression.Constant(JyroNull.Instance, typeof<JyroValue>) :> Expression))
                    |> Array.ofList
            // The arguments, positional or named, are evaluated into a local
            // before the call adds to the call depth (spec 100.2.4, ADR 0044,
            // ADR 0047): a call in an argument starts and returns at the depth
            // of the expression that holds the outer call, so `F(G())` runs at
            // a call depth limit of 1. Building the array inline in the call
            // expression would evaluate `G()` after `CheckAndEnterCall`.
            let argsVar = Expression.Variable(typeof<JyroValue[]>, "callArgs")
            let evalArgs =
                Expression.Assign(argsVar, Expression.NewArrayInit(typeof<JyroValue>, compiledArgs)) :> Expression
            let argsArray = argsVar :> Expression

            // For user functions we read `CompiledDelegate` off the instance
            // and invoke it directly - skipping the `IJyroFunction.Execute`
            // interface dispatch + `JyroUserFunction.Execute` trampoline (a
            // two-frame-per-call saving). Stdlib and host functions still go
            // through the interface because they don't share the delegate
            // shape.
            let actualCall : Expression =
                match func with
                | :? JyroUserFunction as uf ->
                    let funcConst = Expression.Constant(uf, typeof<JyroUserFunction>)
                    // `JyroUserFunction` is internal; expression trees compile
                    // with visibility checks relaxed, so the emitted call is fine.
                    let delegateExpr = Expression.Property(funcConst, EngineMembers.userFunctionDelegate)
                    Expression.Invoke(delegateExpr, argsArray, ctx.ContextParam) :> Expression
                | _ ->
                    let funcConst = Expression.Constant(func, typeof<IJyroFunction>)
                    let executeMethod = EngineMembers.functionExecute
                    // The arguments go with the function they are passed to,
                    // so that a reader of `JyroArguments` names the function
                    // and the parameter of its signature (JYRO-ADR-192 item 2).
                    let callArgumentsCtor = EngineMembers.newCallArguments
                    let callArguments =
                        Expression.Convert(
                            Expression.New(callArgumentsCtor, funcConst, Expression.Convert(argsArray, typeof<IReadOnlyList<JyroValue>>)),
                            typeof<IReadOnlyList<JyroValue>>)
                    Expression.Call(funcConst, executeMethod, callArguments, ctx.ContextParam) :> Expression

            // Track call depth without a try/finally. If the call throws
            // (either Jyro `fail` or a runtime exception), execution unwinds
            // all the way out - the stale `callDepth` never affects
            // subsequent calls because there aren't any. Dropping
            // `Expression.TryFinally` removes the exception-unwinder frame
            // state from every call site.
            let resultVar = Expression.Variable(typeof<JyroValue>, "callResult")
            // Host/stdlib functions get a post-call result observation so a host
            // can detect error envelopes. User functions are skipped - they don't
            // return host envelopes and the extra call would add per-call overhead
            // to the hottest path.
            let isHostFunction = match func with :? JyroUserFunction -> false | _ -> true
            // The call is the node an error of the function is located at
            // (spec 130.1.2): a standard library or host function's error,
            // the call depth limit, and a variant's. Its place is stored once
            // its arguments are evaluated. The callee's statements (a user
            // function's body, or a lambda or function a host function calls
            // back) move the place on, so an error raised in them is located
            // where it was raised. The place before the call is read first
            // and given back after, so an error the enclosing node raises once
            // the call has returned is located at that node. Nothing restores
            // it when the call throws: the place left is the innermost node
            // running, which is where the error is reported (JYRO-ADR-108).
            let savedPosition = Expression.Variable(typeof<obj>, "callerLocation")
            // The place stored is the call's with the callee's name, so that
            // an exception that is not a diagnostic, escaping the callee,
            // becomes a RuntimeError naming the function (spec 130.6.2). A
            // call with no position (a host-built AST) is placed at the node
            // that encloses it, as before, with its name added.
            let markCall : Expression =
                if pos = Position.None then
                    Expression.Call(
                        ctx.ContextParam,
                        markLocationMethod,
                        Expression.Call(runningCallWithinMethod, savedPosition, Expression.Constant(name, typeof<string>))) :> Expression
                else
                    Expression.Call(
                        ctx.ContextParam,
                        markLocationMethod,
                        Expression.Constant(RunningCall(box pos.Location, name), typeof<obj>)) :> Expression
            let stmts =
                [ evalArgs
                  Expression.Assign(savedPosition, Expression.Property(ctx.ContextParam, runningLocationProp)) :> Expression
                  markCall ]
                @ [ Expression.Call(ctx.ContextParam, checkAndEnterCallMethod) :> Expression
                    Expression.Assign(resultVar, actualCall) :> Expression
                    Expression.Call(ctx.ContextParam, exitCallMethod) :> Expression
                    Expression.Call(ctx.ContextParam, markLocationMethod, savedPosition) :> Expression ]
                @ (if isHostFunction then
                     [ Expression.Call(ctx.ContextParam, onFunctionResultMethod,
                                       Expression.Constant(name, typeof<string>), resultVar) :> Expression ]
                   else [])
                @ [ resultVar :> Expression ]
            Expression.Block([| argsVar; resultVar; savedPosition |], stmts) :> Expression
        | None -> JyroError.raiseRuntime MessageCode.UndefinedFunction [| box name |]

    /// Compile a property access
    and private compilePropertyAccess (ctx: CompilationContext) (target: Expr) (prop: string) (pos: Position) : Expression =
        let targetVar = Expression.Variable(typeof<JyroValue>, "accessTarget")
        let propConst = Expression.Constant(prop, typeof<string>)
        asNode ctx pos
            [ targetVar, compileExpr ctx target ]
            (Expression.Call(targetVar, EngineMembers.getProperty, propConst))

    /// Compile an index access
    and private compileIndexAccess (ctx: CompilationContext) (target: Expr) (index: Expr) (pos: Position) : Expression =
        let targetVar = Expression.Variable(typeof<JyroValue>, "accessTarget")
        let indexVar = Expression.Variable(typeof<JyroValue>, "accessIndex")
        asNode ctx pos
            [ targetVar, compileExpr ctx target
              indexVar, compileExpr ctx index ]
            (Expression.Call(targetVar, EngineMembers.getIndex, indexVar))

    /// Compile an object literal
    and private compileObjectLiteral (ctx: CompilationContext) (props: (string * Expr) list) : Expression =
        let objVar = Expression.Variable(typeof<JyroObject>, "obj")
        let newObj = Expression.New(EngineMembers.newObject)
        let assigns = props |> List.map (fun (key, value) ->
            let keyConst = Expression.Constant(key, typeof<string>)
            let valueExpr = compileExpr ctx value
            Expression.Call(objVar, EngineMembers.objectSetProperty, keyConst, valueExpr) :> Expression)
        let body = Expression.Block(
            [| objVar |],
            [ yield Expression.Assign(objVar, newObj) :> Expression
              yield! assigns
              yield objVar :> Expression ] |> Array.ofList)
        body :> Expression

    /// Compile an array literal
    and private compileArrayLiteral (ctx: CompilationContext) (elements: Expr list) : Expression =
        let arrVar = Expression.Variable(typeof<JyroArray>, "arr")
        let newArr = Expression.New(EngineMembers.newArray)
        let adds = elements |> List.map (fun elem ->
            let elemExpr = compileExpr ctx elem
            Expression.Call(arrVar, EngineMembers.arrayAdd, elemExpr) :> Expression)
        let body = Expression.Block(
            [| arrVar |],
            [ yield Expression.Assign(arrVar, newArr) :> Expression
              yield! adds
              yield arrVar :> Expression ] |> Array.ofList)
        body :> Expression

    /// Map JyroType (AST) to JyroValueType (runtime)
    and jyroTypeToValueType (jyroType: JyroType) : JyroValueType =
        match jyroType with
        | NumberType -> JyroValueType.Number
        | StringType -> JyroValueType.String
        | BooleanType -> JyroValueType.Boolean
        | ObjectType -> JyroValueType.Object
        | ArrayType -> JyroValueType.Array
        | NullType -> JyroValueType.Null
        | FunctionType -> JyroValueType.Function
        | AnyType -> JyroValueType.Null // Any always matches, handled specially

    /// Compile a lambda expression to a JyroFunction value
    and private compileLambda (ctx: CompilationContext) (params': string list) (body: Expr) : Expression =
        let argsParam = Expression.Parameter(typeof<IReadOnlyList<JyroValue>>, "lambdaArgs")

        let itemGetter = EngineMembers.listItem
        let paramVars = params' |> List.mapi (fun i name ->
            let var = Expression.Variable(typeof<JyroValue>, name)
            let getItem = Expression.Call(argsParam, itemGetter, Expression.Constant(i)) :> Expression
            let init = Expression.Assign(var, getItem) :> Expression
            (name, var, init))

        // Build inner context: lambda params + captured outer variables
        // ContextParam stays the same (captured via closure), break/continue/return cleared
        let innerCtx =
            paramVars |> List.fold (fun (c: CompilationContext) (name, var, _) -> c.WithVariable(name, var))
                { ctx with
                    BreakLabel = None
                    ContinueLabel = None
                    ReturnLabel = None
                    InLambda = true }

        let bodyExpr = compileExpr innerCtx body

        let vars = paramVars |> List.map (fun (_, var, _) -> var) |> Array.ofList
        let inits = paramVars |> List.map (fun (_, _, init) -> init)
        let allExprs = [ yield! inits; yield bodyExpr ] |> Array.ofList
        let blockBody = Expression.Block(typeof<JyroValue>, vars, allExprs)

        // Create the LINQ lambda expression (execution context captured via outer scope closure)
        let lambdaExpr = Expression.Lambda<Func<IReadOnlyList<JyroValue>, JyroValue>>(
            blockBody, argsParam)

        // Pre-compile the inner lambda at tree-build time and embed the
        // delegate as a constant. This avoids nested LambdaExpression
        // compilation issues in Mono WASM's LightCompiler.
        //
        // The function VALUE is made each time the lambda expression is
        // evaluated, not once here: two evaluations are two function values,
        // and are not equal (spec 30.10.9). Embedding one `JyroFunction` as
        // the constant made every evaluation the same value, so two calls of
        // `mk()` returning `x => x` compared equal.
        let createMethod = EngineMembers.createFunction
        try
            let compiled = lambdaExpr.Compile()
            Expression.Call(createMethod,
                            Expression.Constant(compiled, typeof<Func<IReadOnlyList<JyroValue>, JyroValue>>),
                            Expression.Constant(params'.Length)) :> Expression
        with :? InvalidOperationException ->
            // Lambda captures outer scope variables - must embed in tree for runtime compilation
            Expression.Call(createMethod, lambdaExpr, Expression.Constant(params'.Length)) :> Expression

    /// Compile an increment/decrement expression (++x, x++, --x, x--)
    and private compileIncrementDecrement (ctx: CompilationContext) (expr: Expr) (isIncrement: bool) (isPrefix: bool) : Expression =
        let binaryOp =
            if isIncrement then Expression.Constant(Add, typeof<BinaryOp>)
            else Expression.Constant(Subtract, typeof<BinaryOp>)

        match expr with
        | Identifier(name, _) when name <> "Input" && name <> "Output" ->
            match ctx.Variables.TryFind(name) with
            | Some varExpr ->
                let newVal = Expression.Variable(typeof<JyroValue>, "incNew")
                let computeNew = Expression.Assign(newVal, Expression.Call(varExpr, evaluateBinaryMethod, binaryOp, oneConst)) :> Expression
                let coercedNew =
                    match ctx.VariableTypes.TryFind(name) with
                    | Some hint when hint <> AnyType ->
                        let targetType = jyroTypeToValueType hint
                        Expression.Call(coerceToTypeMethod,
                            newVal :> Expression,
                            Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                    | _ -> newVal :> Expression
                if isPrefix then
                    Expression.Block(
                        [| newVal |],
                        [| computeNew
                           Expression.Assign(varExpr, coercedNew) :> Expression
                           varExpr :> Expression |]) :> Expression
                else
                    let oldVal = Expression.Variable(typeof<JyroValue>, "incOld")
                    Expression.Block(
                        [| oldVal; newVal |],
                        [| Expression.Assign(oldVal, varExpr :> Expression) :> Expression
                           computeNew
                           Expression.Assign(varExpr, coercedNew) :> Expression
                           oldVal :> Expression |]) :> Expression
            | None -> JyroError.raiseRuntime MessageCode.UndeclaredVariable [| box name |]

        | Identifier("Output", _) ->
            // Same reasoning as compound assignment to Output (see
            // `StatementCompiler.compileAssignment`): `Output` reads like any
            // other identifier, so `Output++` is sugar for `Output = Output +
            // 1` and rebinds it, which means the context needs the same sync.
            let setOutputMethod = contextOutputSetter
            let oldVal = Expression.Variable(typeof<JyroValue>, "incOld")
            Expression.Block(
                [| oldVal |],
                [| Expression.Assign(oldVal, ctx.OutputParam :> Expression) :> Expression
                   Expression.Assign(ctx.OutputParam, Expression.Call(ctx.OutputParam, evaluateBinaryMethod, binaryOp, oneConst)) :> Expression
                   Expression.Call(ctx.ContextParam, setOutputMethod, ctx.OutputParam) :> Expression
                   (if isPrefix then ctx.OutputParam :> Expression else oldVal :> Expression) |]) :> Expression

        | PropertyAccess(obj, prop, _) ->
            let objExpr = compileExpr ctx obj
            let propConst = Expression.Constant(prop, typeof<string>)
            let objVar = Expression.Variable(typeof<JyroValue>, "incObj")
            let oldVal = Expression.Variable(typeof<JyroValue>, "incOld")
            let newVal = Expression.Variable(typeof<JyroValue>, "incNew")
            Expression.Block(
                [| objVar; oldVal; newVal |],
                [| Expression.Assign(objVar, objExpr) :> Expression
                   Expression.Assign(oldVal, Expression.Call(objVar, getPropertyMethod, propConst)) :> Expression
                   Expression.Assign(newVal, Expression.Call(oldVal, evaluateBinaryMethod, binaryOp, oneConst)) :> Expression
                   Expression.Call(setPropertyMethod, objVar, propConst, newVal) :> Expression
                   (if isPrefix then newVal else oldVal) :> Expression |]) :> Expression

        | IndexAccess(obj, index, _) ->
            let objExpr = compileExpr ctx obj
            let indexExpr = compileExpr ctx index
            let objVar = Expression.Variable(typeof<JyroValue>, "incObj")
            let idxVar = Expression.Variable(typeof<JyroValue>, "incIdx")
            let oldVal = Expression.Variable(typeof<JyroValue>, "incOld")
            let newVal = Expression.Variable(typeof<JyroValue>, "incNew")
            Expression.Block(
                [| objVar; idxVar; oldVal; newVal |],
                [| Expression.Assign(objVar, objExpr) :> Expression
                   Expression.Assign(idxVar, indexExpr) :> Expression
                   Expression.Assign(oldVal, Expression.Call(objVar, getIndexMethod, idxVar)) :> Expression
                   Expression.Assign(newVal, Expression.Call(oldVal, evaluateBinaryMethod, binaryOp, oneConst)) :> Expression
                   Expression.Call(setIndexMethod, objVar, idxVar, newVal) :> Expression
                   (if isPrefix then newVal else oldVal) :> Expression |]) :> Expression

        // Everything the validator rejects as an inc/dec target, plus
        // `Identifier("Input", _)`, which the guarded branches above leave
        // unmatched. Reachable only via an AST the validator never saw - a
        // .jyrx artifact or a host-built library AST.
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
        | MatchExpr _ -> JyroError.raiseRuntime MessageCode.InvalidIncrementTarget [||]

    /// Compile a type check expression (x is type / x is not type)
    and private compileTypeCheck (ctx: CompilationContext) (expr: Expr) (jyroType: JyroType) (isNegated: bool) : Expression =
        let exprCompiled = compileExpr ctx expr
        let valueTypeProp = EngineMembers.valueType
        let valueTypeExpr = Expression.Property(exprCompiled, valueTypeProp)
        let expectedType = jyroTypeToValueType jyroType
        let expectedTypeConst = Expression.Constant(expectedType, typeof<JyroValueType>)
        let comparison =
            if jyroType = AnyType then
                // Every value is of type `any`, so the answer is constant - but
                // the operand is still evaluated for its effects. Folding the
                // whole expression to `true` and dropping `exprCompiled` from
                // the tree made `is any` the only operator in the language that
                // deletes its operand: `(1 / 0) is any` yielded true instead of
                // raising, and `n++ is any` left `n` unchanged.
                Expression.Block(exprCompiled, Expression.Constant(true)) :> Expression
            else
                Expression.Equal(valueTypeExpr, expectedTypeConst) :> Expression
        let result =
            if isNegated then
                Expression.Not(comparison) :> Expression
            else
                comparison
        let fromBoolMethod = EngineMembers.fromBoolean
        Expression.Call(fromBoolMethod, result) :> Expression

    /// Compile a match expression as a chain of conditionals returning JyroValue
    and private compileMatchExpr (ctx: CompilationContext) (expr: Expr) (cases: MatchExprCase list) : Expression =
        let matchExpr = compileExpr ctx expr
        let matchVar = Expression.Variable(typeof<JyroValue>, "matchVal")
        let assignMatch = Expression.Assign(matchVar, matchExpr) :> Expression

        let getPropertyMethod = EngineMembers.getProperty
        // The variant the subject is a value of: its `_variant` when it is an
        // object and that property is a string, null otherwise (spec 50.9.9).
        let variantTagExpr = Expression.Call(EngineMembers.variantOf, matchVar)
        let variantTagVar = Expression.Variable(typeof<string>, "variantTag")
        let assignTag = Expression.Assign(variantTagVar, variantTagExpr) :> Expression

        let rec buildCases (remaining: MatchExprCase list) : Expression =
            match remaining with
            // No case names the subject's variant, or it is no variant's
            // value: `MatchSubjectNotVariant`, not null (spec 50.9.9,
            // JYRO-REQ-236).
            | [] -> Expression.Call(EngineMembers.subjectNotVariant) :> Expression
            | case :: rest ->
                let testExpr =
                    Expression.Equal(
                        variantTagVar,
                        Expression.Constant(case.VariantName, typeof<string>)) :> Expression

                let unionName = ctx.VariantToUnion.[case.VariantName]
                let variants = ctx.UnionDefinitions.[unionName]
                let variant = variants |> List.find (fun v -> v.Name = case.VariantName)
                let fieldNames = variant.Fields |> List.map fst

                let bindingVars = ResizeArray<ParameterExpression>()
                let bindingInits = ResizeArray<Expression>()
                let mutable caseCtx = ctx
                for (bindingName, fieldName) in List.zip case.Bindings fieldNames do
                    let bindVar = Expression.Variable(typeof<JyroValue>, bindingName)
                    bindingVars.Add(bindVar)
                    let getField = Expression.Call(matchVar, getPropertyMethod, Expression.Constant(fieldName, typeof<string>))
                    bindingInits.Add(Expression.Assign(bindVar, getField) :> Expression)
                    caseCtx <- caseCtx.WithVariable(bindingName, bindVar)

                let bodyExpr = compileExpr caseCtx case.Body
                let caseExpr =
                    if bindingVars.Count > 0 then
                        let allExprs = ResizeArray<Expression>()
                        allExprs.AddRange(bindingInits)
                        allExprs.Add(bodyExpr)
                        Expression.Block(typeof<JyroValue>, bindingVars, allExprs) :> Expression
                    else
                        bodyExpr

                let elseExpr = buildCases rest
                Expression.Condition(testExpr, caseExpr, elseExpr, typeof<JyroValue>) :> Expression

        let chain = buildCases cases
        Expression.Block(typeof<JyroValue>, [| matchVar; variantTagVar |], [| assignMatch; assignTag; chain |]) :> Expression

    /// Compile an expression to an Expression Tree
    and compileExpr (ctx: CompilationContext) (expr: Expr) : Expression =
        match expr with
        | Literal(value, _) -> compileLiteral value
        | Identifier(name, _) -> compileIdentifier ctx name
        | Binary(left, op, right, pos) -> compileBinary ctx left op right pos
        | Unary(op, operand, pos) -> compileUnary ctx op operand pos
        | Ternary(cond, thenExpr, elseExpr, _) -> compileTernary ctx cond thenExpr elseExpr
        | Call(name, callArgs, pos) -> compileCall ctx name callArgs pos
        | PropertyAccess(target, prop, pos) -> compilePropertyAccess ctx target prop pos
        | IndexAccess(target, index, pos) -> compileIndexAccess ctx target index pos
        | ObjectLiteral(props, _) -> compileObjectLiteral ctx props
        | ArrayLiteral(elements, _) -> compileArrayLiteral ctx elements
        | Lambda(params', body, _) -> compileLambda ctx params' body
        | TypeCheck(expr, jyroType, isNegated, _) -> compileTypeCheck ctx expr jyroType isNegated
        | IncrementDecrement(expr, isInc, isPre, pos) ->
            // The operator is the node its error is located at (spec 130.1.2).
            asNode ctx pos [] (compileIncrementDecrement ctx expr isInc isPre)
        | MatchExpr(expr, cases, _) -> compileMatchExpr ctx expr cases

namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast

open System
open System.Linq.Expressions
open RealTree.Jyro.Compilation.ExpressionCompiler

/// Compiles Jyro statements to LINQ Expression Trees
module internal StatementCompiler =
    // Cached MethodInfo for execution context resource checks
    let private checkStatementMethod = EngineMembers.checkAndCountStatement
    let private checkLoopMethod = EngineMembers.checkAndEnterLoop
    let private setReturnMessageMethod = EngineMembers.setReturnMessage
    // JyroRuntimeException(code, args), which is internal (JYRO-ADR-129): a
    // host raises one through JyroError, and the exception makes its own
    // message from the code and the arguments.
    let private runtimeExceptionCtor = EngineMembers.newRuntimeException

    let private withStatementCheck (ctx: CompilationContext) (expr: Expression) : Expression =
        Expression.Block(
            Expression.Call(ctx.ContextParam, checkStatementMethod),
            expr) :> Expression

    /// The store of a statement's position on the context, or nothing for a
    /// statement with no position (a host-built AST). When an exception
    /// reaches `execute` without a location of its own, the diagnostic takes
    /// it from there (JYRO-ADR-108).
    ///
    /// This used to be a try/catch around every statement, whose handlers
    /// threw a located copy of the exception, rethrew one already located,
    /// wrapped any other exception, and rethrew `exit`. Throwing from a catch
    /// handler starts a nested exception dispatch on top of the frames not
    /// yet unwound (Windows, .NET 8), so every enclosing statement added to
    /// what an error cost the stack on its way out: the call-depth limit
    /// tripping, a runtime error, `fail` or `exit` deep in nested blocks
    /// overflowed a 1 MB thread and killed the host (JYRO-DEF-487,
    /// JYRO-DEF-488). Compiled code MUST NOT throw or rethrow from inside a
    /// catch handler; with no handler at all, an exception, `exit` included,
    /// unwinds in one dispatch whatever the depth.
    let private markPosition (ctx: CompilationContext) (pos: Position) : Expression list =
        Option.toList (markNode ctx pos)

    /// Prefixes a statement with the store of its position.
    let private withLocationTracking (ctx: CompilationContext) (body: Expression) (pos: Position) : Expression =
        match markPosition ctx pos with
        | [] -> body
        | mark -> Expression.Block(mark @ [ body ]) :> Expression

    /// A loop's own work after its body (the condition, the step, the next
    /// element, the iteration check) belongs to the loop statement, which the
    /// try/catch used to say by catching it there. The body's statements have
    /// moved the position on, so the loop stores its own again before that
    /// work. The list is empty for a statement with no position.
    let private remark (ctx: CompilationContext) (pos: Position) : Expression list =
        markPosition ctx pos

    /// One loop iteration, preceded by the loop's position stored again.
    let private withRemark (ctx: CompilationContext) (pos: Position) (iteration: Expression) : Expression =
        match remark ctx pos with
        | [] -> iteration
        | mark -> Expression.Block(mark @ [ iteration ]) :> Expression

    let private forValuesCheckMethod = EngineMembers.checkForValues
    let private forValueMethod = EngineMembers.forValue
    let private variantOfMethod = EngineMembers.variantOf
    let private subjectNotVariantMethod = EngineMembers.subjectNotVariant
    let private deleteAtIndexMethod = EngineMembers.deleteAtIndex
    // `delete o.key`, from the engine member table, so a name the engine no
    // longer has fails when the compiler loads rather than when a script
    // first uses `delete` (JYRO-ADR-206).
    let private deletePropertyMethod = EngineMembers.deleteProperty

    let private compileVarDecl (ctx: CompilationContext) (name: string) (typeHint: JyroType option) (init: Expr option) : Expression * CompilationContext =
        let varExpr = Expression.Variable(typeof<JyroValue>, name)
        let ctx' = ctx.WithVariable(name, varExpr)
        let ctx' =
            match typeHint with
            | Some hint -> ctx'.WithVariableType(name, hint)
            | None -> ctx'
        // The declaration's scope begins after its initial value (60.1.5), so
        // the initial value is compiled in the enclosing context: in
        // `var x = x + 1` the right-hand `x`, and any lambda in the initial
        // value, refers to the outer variable, not to the one being declared.
        let initExpr =
            match init with
            | Some expr -> compileExpr ctx expr
            | None -> Expression.Constant(JyroNull.Instance, typeof<JyroValue>) :> Expression
        let finalInit =
            match typeHint with
            | Some hint when hint <> AnyType ->
                let targetType = jyroTypeToValueType hint
                Expression.Call(coerceToTypeMethod,
                    initExpr,
                    Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
            | _ -> initExpr
        let assignExpr = Expression.Assign(varExpr, finalInit) :> Expression
        (assignExpr, ctx')

    let private compileAssignment (ctx: CompilationContext) (target: Expr) (op: AssignOp) (value: Expr) (typeHint: JyroType option) : Expression =
        let valueExpr = compileExpr ctx value
        match target with
        | Identifier("Output", _) ->
            // Assignment to Output (e.g., Output = someArray) - sync to context
            // for exit safety.
            //
            // Compound forms read the current binding first, exactly as the
            // local-variable branch below does. `Output` is an ordinary
            // readable identifier, so `Output = Output & "x"` already worked;
            // refusing `Output &= "x"` would have been a hole in the sugar
            // rather than a rule about Output. The context sync is needed for
            // both, because a compound assignment rebinds Output to a fresh
            // value rather than mutating the one it holds.
            let setOutputMethod = contextOutputSetter
            let finalValue =
                match Operators.binaryOpForAssign op with
                | None -> valueExpr
                | Some binOp ->
                    let binaryOp = Expression.Constant(binOp, typeof<BinaryOp>)
                    Expression.Call(ctx.OutputParam, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
            let checkedValue =
                match typeHint with
                | Some hint when hint <> AnyType ->
                    let targetType = jyroTypeToValueType hint
                    Expression.Call(coerceToTypeMethod,
                        finalValue,
                        Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                | _ -> finalValue
            Expression.Block(
                Expression.Assign(ctx.OutputParam, checkedValue),
                Expression.Call(ctx.ContextParam, setOutputMethod, ctx.OutputParam)
            ) :> Expression
        | Identifier(name, _) when name <> "Input" && name <> "Output" ->
            match ctx.Variables.TryFind(name) with
            | Some varExpr ->
                let finalValue =
                    match op with
                    | Assign -> valueExpr
                    | AddAssign ->
                        let binaryOp = Expression.Constant(Add, typeof<BinaryOp>)
                        Expression.Call(varExpr, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
                    | SubtractAssign ->
                        let binaryOp = Expression.Constant(Subtract, typeof<BinaryOp>)
                        Expression.Call(varExpr, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
                    | MultiplyAssign ->
                        let binaryOp = Expression.Constant(Multiply, typeof<BinaryOp>)
                        Expression.Call(varExpr, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
                    | DivideAssign ->
                        let binaryOp = Expression.Constant(Divide, typeof<BinaryOp>)
                        Expression.Call(varExpr, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
                    | ModuloAssign ->
                        let binaryOp = Expression.Constant(Modulo, typeof<BinaryOp>)
                        Expression.Call(varExpr, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
                    | ConcatAssign ->
                        let binaryOp = Expression.Constant(Concat, typeof<BinaryOp>)
                        Expression.Call(varExpr, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
                let hintedValue =
                    match typeHint with
                    | Some hint when hint <> AnyType ->
                        let targetType = jyroTypeToValueType hint
                        Expression.Call(coerceToTypeMethod,
                            finalValue,
                            Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                    | _ -> finalValue
                let checkedValue =
                    match ctx.VariableTypes.TryFind(name) with
                    | Some hint when hint <> AnyType ->
                        let targetType = jyroTypeToValueType hint
                        Expression.Call(coerceToTypeMethod,
                            hintedValue,
                            Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                    | _ -> hintedValue
                Expression.Assign(varExpr, checkedValue) :> Expression
            | None -> JyroError.raiseRuntime MessageCode.UndeclaredVariable [| box name |]
        // The target's object (and index) are evaluated once, into block
        // variables, as `compileIncrementDecrement` does. A compound form reads
        // and then writes the target, and it used to compile the target
        // expression into both halves: `a[i++] += 5` stepped `i` twice and
        // `G(o).k += 1` called `G` twice.
        | PropertyAccess(obj, prop, _) ->
            let objVar = Expression.Variable(typeof<JyroValue>, "assignObj")
            let objExpr = objVar :> Expression
            let propConst = Expression.Constant(prop, typeof<string>)
            let finalValue =
                match Operators.binaryOpForAssign op with
                | None -> valueExpr
                | Some binOp ->
                    let currentValue = Expression.Call(objExpr, EngineMembers.getProperty, propConst) :> Expression
                    let binaryOp = Expression.Constant(binOp, typeof<BinaryOp>)
                    Expression.Call(currentValue, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
            let checkedValue =
                match typeHint with
                | Some hint when hint <> AnyType ->
                    let targetType = jyroTypeToValueType hint
                    Expression.Call(coerceToTypeMethod,
                        finalValue,
                        Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                | _ -> finalValue
            Expression.Block(
                [| objVar |],
                [| Expression.Assign(objVar, compileExpr ctx obj) :> Expression
                   Expression.Call(scriptSetPropertyMethod, objExpr, propConst, checkedValue) :> Expression |]) :> Expression
        | IndexAccess(obj, index, _) ->
            let objVar = Expression.Variable(typeof<JyroValue>, "assignObj")
            let idxVar = Expression.Variable(typeof<JyroValue>, "assignIdx")
            let objExpr = objVar :> Expression
            let indexExpr = idxVar :> Expression
            let finalValue =
                match Operators.binaryOpForAssign op with
                | None -> valueExpr
                | Some binOp ->
                    let currentValue = Expression.Call(objExpr, EngineMembers.getIndex, indexExpr) :> Expression
                    let binaryOp = Expression.Constant(binOp, typeof<BinaryOp>)
                    Expression.Call(currentValue, EngineMembers.evaluateBinary, binaryOp, valueExpr) :> Expression
            let checkedValue =
                match typeHint with
                | Some hint when hint <> AnyType ->
                    let targetType = jyroTypeToValueType hint
                    Expression.Call(coerceToTypeMethod,
                        finalValue,
                        Expression.Constant(targetType, typeof<JyroValueType>)) :> Expression
                | _ -> finalValue
            Expression.Block(
                [| objVar; idxVar |],
                [| Expression.Assign(objVar, compileExpr ctx obj) :> Expression
                   Expression.Assign(idxVar, compileExpr ctx index) :> Expression
                   Expression.Call(scriptSetIndexMethod, objExpr, indexExpr, checkedValue) :> Expression |]) :> Expression
        // Everything the validator rejects as an assignment target (see
        // `Ast.isAssignmentTarget`), plus `Identifier("Input", _)`, which the
        // guarded branches above leave unmatched. Not dead: the compiler also
        // runs on ASTs the validator never saw - a .jyrx artifact or a
        // host-built library AST - so this reports rather than crashes.
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
        | MatchExpr _ -> JyroError.raiseRuntime MessageCode.InvalidAssignmentTarget [||]

    let rec private compileIf (ctx: CompilationContext) (cond: Expr) (thenBlock: Stmt list) (elseIfs: (Expr * Stmt list) list) (elseBlock: Stmt list option) : Expression =
        let condExpr = compileExpr ctx cond
        let testExpr = Expression.Call(condExpr, EngineMembers.isTruthy)
        let thenExpr = compileBlock ctx thenBlock
        let elseExpr =
            match elseIfs, elseBlock with
            | [], None -> Expression.Empty() :> Expression
            | [], Some stmts -> compileBlock ctx stmts
            | (c, b) :: rest, _ -> compileIf ctx c b rest elseBlock
        Expression.IfThenElse(testExpr, thenExpr, elseExpr) :> Expression

    /// Compile a switch statement as a chain of if/else-if with equality checks
    and private compileSwitch (ctx: CompilationContext) (expr: Expr) (cases: RealTree.Jyro.Ast.SwitchCase list) (defaultCase: Stmt list option) : Expression =
        let switchExpr = compileExpr ctx expr
        let switchVar = Expression.Variable(typeof<JyroValue>, "switchVal")
        let assignSwitch = Expression.Assign(switchVar, switchExpr) :> Expression
        let equalsMethod = EngineMembers.equalsValue
        let rec buildCases (remaining: RealTree.Jyro.Ast.SwitchCase list) : Expression =
            match remaining with
            | [] ->
                match defaultCase with
                | Some stmts -> compileBlock ctx stmts
                | None -> Expression.Empty() :> Expression
            | case :: rest ->
                let testExpr =
                    case.Values
                    |> List.map (fun v -> Expression.Call(switchVar, equalsMethod, compileExpr ctx v) :> Expression)
                    |> List.reduce (fun a b -> Expression.OrElse(a, b) :> Expression)
                let bodyExpr = compileBlock ctx case.Body
                let elseExpr = buildCases rest
                Expression.IfThenElse(testExpr, bodyExpr, elseExpr) :> Expression
        let chain = buildCases cases
        Expression.Block(typeof<System.Void>, [| switchVar |], [| assignSwitch; chain |]) :> Expression

    /// Compile a while loop with iteration counting
    and private compileWhile (ctx: CompilationContext) (cond: Expr) (body: Stmt list) (pos: Position) : Expression =
        let breakLabel = Expression.Label("break")
        let continueLabel = Expression.Label("continue")
        let ctx' = ctx.WithLoopLabels(breakLabel, continueLabel)
        let condExpr = compileExpr ctx' cond
        let testExpr = Expression.Call(condExpr, EngineMembers.isTruthy)
        let bodyExpr = compileBlock ctx' body

        // Loop body with iteration check at the top of each iteration
        let checkedBody =
            Expression.IfThenElse(
                testExpr,
                Expression.Block(
                    Expression.Call(ctx.ContextParam, checkLoopMethod),
                    bodyExpr),
                Expression.Break(breakLabel))

        // The condition is evaluated again after the body, so the loop's
        // position is stored again first (see `remark`).
        Expression.Loop(withRemark ctx pos checkedBody, breakLabel, continueLabel) :> Expression

    /// Compile a range-based for loop with iteration counting
    and private compileFor (ctx: CompilationContext) (varName: string) (startExpr: Expr) (endExpr: Expr) (stepExpr: Expr option) (direction: ForDirection) (body: Stmt list) (pos: Position) : Expression * CompilationContext =
        let breakLabel = Expression.Label("break")
        let continueLabel = Expression.Label("continue")

        // The counter holds the loop's values; each iteration has a new loop
        // variable that takes the counter's value (spec 60.1.4, JYRO-ADR-12),
        // so a lambda built in one iteration keeps that iteration's value,
        // and an assignment to the loop variable in the body does not change
        // the next value.
        let counterVar = Expression.Variable(typeof<JyroValue>, "__counter")
        let startVar = Expression.Variable(typeof<JyroValue>, "__start")
        let kVar = Expression.Variable(typeof<int64>, "__k")
        let loopVar = Expression.Variable(typeof<JyroValue>, varName)
        let endVar = Expression.Variable(typeof<JyroValue>, "__end")
        let stepVar = Expression.Variable(typeof<JyroValue>, "__step")

        // Evaluate start/end/step once at loop entry (using outer context)
        let startCompiled = compileExpr ctx startExpr
        let endCompiled = compileExpr ctx endExpr
        let stepCompiled =
            match stepExpr with
            | Some expr -> compileExpr ctx expr
            // With no `by step` the step is the exact number 1 (spec 50.5.3):
            // an inexact 1 made every value of the loop inexact, so a large
            // integer start never advanced.
            | None -> Expression.Constant(JyroNumber(1) :> JyroValue, typeof<JyroValue>) :> Expression

        let initStart = Expression.Assign(startVar, startCompiled) :> Expression
        let initEnd = Expression.Assign(endVar, endCompiled) :> Expression
        let initStep = Expression.Assign(stepVar, stepCompiled) :> Expression

        // start, then end, then step, after all three are evaluated and
        // whether or not the loop would iterate (spec 50.5.9, JYRO-ADR-19).
        let ascending = (direction = Ascending)
        let stepCheck =
            Expression.Call(
                forValuesCheckMethod,
                startVar :> Expression,
                endVar :> Expression,
                stepVar :> Expression,
                Expression.Constant(ascending)) :> Expression

        // Set up context with loop variable and labels
        let ctx' = ctx.WithVariable(varName, loopVar).WithLoopLabels(breakLabel, continueLabel)

        // Condition: loopVar < endVar (ascending) or loopVar > endVar (descending)
        let comparisonOp =
            match direction with
            | Ascending -> Expression.Constant(LessThan, typeof<BinaryOp>)
            | Descending -> Expression.Constant(GreaterThan, typeof<BinaryOp>)
        let condResult = Expression.Call(counterVar, EngineMembers.evaluateBinary, comparisonOp, endVar)
        let testExpr = Expression.Call(condResult, EngineMembers.isTruthy)

        // Compile loop body, in a block that declares this iteration's loop
        // variable. A block's variables are new each time it is entered, a
        // captured one included.
        let bodyExpr =
            Expression.Block(
                [| loopVar |],
                [| Expression.Assign(loopVar, counterVar) :> Expression
                   compileBlock ctx' body |]) :> Expression

        // The k-th value is start + k * step (ascending) or start - k * step
        // (descending), made from start each time rather than by stepping the
        // previous value (spec 50.5.4, 50.5.5): with an inexact start,
        // repeated addition rounds at every step and can drift or stall.
        let valueAtK =
            Expression.Assign(
                counterVar,
                Expression.Call(
                    forValueMethod,
                    startVar :> Expression,
                    stepVar :> Expression,
                    kVar :> Expression,
                    Expression.Constant(ascending))) :> Expression
        let initCounter =
            Expression.Block(
                Expression.Assign(kVar, Expression.Constant(0L)),
                valueAtK) :> Expression
        let updateExpr =
            Expression.Block(
                Expression.Assign(kVar, Expression.Increment(kVar)),
                valueAtK) :> Expression

        // Loop body: if(!cond) break; checkLoop(); body; continueLabel; update
        // Continue label is placed before update so 'continue' still advances the counter.
        // The update and the next condition belong to the loop, so its
        // position is stored again after the body (see `remark`).
        let loopBody =
            Expression.IfThenElse(
                testExpr,
                Expression.Block(
                    [ Expression.Call(ctx.ContextParam, checkLoopMethod) :> Expression
                      bodyExpr
                      Expression.Label(continueLabel) :> Expression ]
                    @ remark ctx pos
                    @ [ updateExpr ]),
                Expression.Break(breakLabel))

        let loopExpr = Expression.Block(
            [| startVar; kVar; counterVar; endVar; stepVar |],
            [| initStart; initEnd; initStep; stepCheck; initCounter
               Expression.Loop(loopBody, breakLabel) :> Expression |]) :> Expression

        // Return the original context - loop variable is scoped to the for block
        (loopExpr, ctx)

    /// Compile a foreach loop with iteration counting
    and private compileForEach (ctx: CompilationContext) (varName: string) (collection: Expr) (body: Stmt list) (pos: Position) : Expression =
        let breakLabel = Expression.Label("break")
        let continueLabel = Expression.Label("continue")
        let varParam = Expression.Variable(typeof<JyroValue>, varName)
        let ctx' = ctx.WithVariable(varName, varParam).WithLoopLabels(breakLabel, continueLabel)

        let collectionExpr = compileExpr ctx collection
        let iterableExpr = Expression.Call(collectionExpr, EngineMembers.toIterable)
        let enumeratorVar = Expression.Variable(typeof<System.Collections.Generic.IEnumerator<JyroValue>>, "enumerator")
        let getEnumeratorExpr = Expression.Call(iterableExpr, EngineMembers.getEnumerator)

        let moveNextExpr = Expression.Call(enumeratorVar, EngineMembers.moveNext)
        let currentProp = EngineMembers.enumeratorCurrent
        let currentExpr = Expression.Property(enumeratorVar, currentProp)
        let assignCurrent = Expression.Assign(varParam, currentExpr)
        let bodyExpr = compileBlock ctx' body

        // Loop body with iteration check after MoveNext succeeds. The block
        // declares the loop variable, so each iteration has a new one (spec
        // 60.1.4, JYRO-ADR-12) and a lambda built in it keeps its element.
        let checkedBody =
            Expression.IfThenElse(
                moveNextExpr,
                Expression.Block(
                    [| varParam |],
                    [| Expression.Call(ctx.ContextParam, checkLoopMethod) :> Expression
                       assignCurrent :> Expression
                       bodyExpr |]),
                Expression.Break(breakLabel))

        Expression.Block(
            [| enumeratorVar |],
            [| Expression.Assign(enumeratorVar, getEnumeratorExpr) :> Expression
               // The next element and the iteration check belong to the loop,
               // so its position is stored again before them (see `remark`).
               Expression.Loop(withRemark ctx pos checkedBody, breakLabel, continueLabel) :> Expression |]) :> Expression

    /// Compile a match statement as a chain of if/else-if on _variant tag
    and private compileMatch (ctx: CompilationContext) (expr: Expr) (cases: MatchCase list) : Expression =
        let matchExpr = compileExpr ctx expr
        let matchVar = Expression.Variable(typeof<JyroValue>, "matchVal")
        let assignMatch = Expression.Assign(matchVar, matchExpr) :> Expression

        // The variant the subject is a value of: its `_variant` when it is an
        // object and that property is a string, null otherwise (spec 50.9.9).
        let getPropertyMethod = EngineMembers.getProperty
        let variantTagExpr = Expression.Call(variantOfMethod, matchVar)
        let variantTagVar = Expression.Variable(typeof<string>, "variantTag")
        let assignTag = Expression.Assign(variantTagVar, variantTagExpr) :> Expression

        let rec buildCases (remaining: MatchCase list) : Expression =
            match remaining with
            // No case names the subject's variant, or it is no variant's
            // value: `MatchSubjectNotVariant` (spec 50.9.9, JYRO-REQ-236).
            | [] -> Expression.Call(subjectNotVariantMethod) :> Expression
            | case :: rest ->
                // Compare variantTag == case.VariantName
                let testExpr =
                    Expression.Equal(
                        variantTagVar,
                        Expression.Constant(case.VariantName, typeof<string>)) :> Expression

                // Destructure fields: look up variant's field names from ctx.UnionDefinitions
                let unionName = ctx.VariantToUnion.[case.VariantName]
                let variants = ctx.UnionDefinitions.[unionName]
                let variant = variants |> List.find (fun v -> v.Name = case.VariantName)
                let fieldNames = variant.Fields |> List.map fst

                // Create binding variables and initialize from matchVar properties
                let bindingVars = ResizeArray<ParameterExpression>()
                let bindingInits = ResizeArray<Expression>()
                let mutable caseCtx = ctx
                for (bindingName, fieldName) in List.zip case.Bindings fieldNames do
                    let bindVar = Expression.Variable(typeof<JyroValue>, bindingName)
                    bindingVars.Add(bindVar)
                    let getField = Expression.Call(matchVar, getPropertyMethod, Expression.Constant(fieldName, typeof<string>))
                    bindingInits.Add(Expression.Assign(bindVar, getField) :> Expression)
                    caseCtx <- caseCtx.WithVariable(bindingName, bindVar)

                // Compile case body with bindings in scope
                let bodyExpr = compileBlock caseCtx case.Body
                let caseBlock =
                    if bindingVars.Count > 0 then
                        let allExprs = ResizeArray<Expression>()
                        allExprs.AddRange(bindingInits)
                        allExprs.Add(bodyExpr)
                        Expression.Block(bindingVars, allExprs) :> Expression
                    else
                        bodyExpr

                let elseExpr = buildCases rest
                Expression.IfThenElse(testExpr, caseBlock, elseExpr) :> Expression

        let chain = buildCases cases
        Expression.Block(typeof<System.Void>, [| matchVar; variantTagVar |], [| assignMatch; assignTag; chain |]) :> Expression

    and compileBlock (ctx: CompilationContext) (stmts: Stmt list) : Expression =
        let variables = ResizeArray<ParameterExpression>()
        let addedVars = System.Collections.Generic.HashSet<string>()
        let expressions = ResizeArray<Expression>()
        let mutable ctx' = ctx

        for stmt in stmts do
            let (expr, newCtx) = compileStmt ctx' stmt
            // Collect new variables (including shadows of outer-scope variables)
            for kvp in newCtx.Variables do
                let isNewName = not (ctx.Variables.ContainsKey(kvp.Key))
                let isShadow =
                    match ctx.Variables.TryFind(kvp.Key) with
                    | Some existing -> not (Object.ReferenceEquals(existing, kvp.Value))
                    | None -> false
                if (isNewName || isShadow) && addedVars.Add(kvp.Key) then
                    variables.Add(kvp.Value)
            ctx' <- newCtx
            expressions.Add(expr)

        if expressions.Count = 0 then
            Expression.Empty() :> Expression
        else
            Expression.Block(variables, expressions) :> Expression

    /// Compile a delete statement (remove a property from an object)
    and private compileDelete (ctx: CompilationContext) (target: Expr) : Expression =
        match target with
        | PropertyAccess(obj, prop, _) ->
            let objExpr = compileExpr ctx obj
            let propConst = Expression.Constant(prop, typeof<string>) :> Expression
            Expression.Call(objExpr, deletePropertyMethod, propConst) :> Expression
        | IndexAccess(obj, index, _) ->
            // The value decides before the key: an object takes only a
            // string key, and a value that is not an object takes none
            // (spec 50.12.4, 50.12.5, JYRO-ADR-19).
            let objExpr = compileExpr ctx obj
            let indexExpr = compileExpr ctx index
            Expression.Call(deleteAtIndexMethod, objExpr, indexExpr) :> Expression
        // Everything the validator rejects as a delete target (see
        // `Ast.isDeleteTarget`). Reachable only via an AST the validator never
        // saw - a .jyrx artifact or a host-built library AST.
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
        | MatchExpr _ -> JyroError.raiseRuntime MessageCode.InvalidDeleteTarget [||]

    /// Compile a single statement with resource checks and location tracking
    and compileStmt (ctx: CompilationContext) (stmt: Stmt) : Expression * CompilationContext =
        let pos = stmt.Position
        let (compiled, ctx') =
            match stmt with
            | VarDecl(name, typeHint, init, _) ->
                let (expr, ctx') = compileVarDecl ctx name typeHint init
                (withStatementCheck ctx expr, ctx')
            | Assignment(target, op, value, typeHint, _) ->
                (withStatementCheck ctx (compileAssignment ctx target op value typeHint), ctx)
            | If(cond, thenBlock, elseIfs, elseBlock, _) ->
                (withStatementCheck ctx (compileIf ctx cond thenBlock elseIfs elseBlock), ctx)
            | While(cond, body, _) ->
                (withStatementCheck ctx (compileWhile ctx cond body pos), ctx)
            | ForEach(varName, collection, body, _) ->
                (withStatementCheck ctx (compileForEach ctx varName collection body pos), ctx)
            | For(varName, startExpr, endExpr, stepExpr, direction, body, _) ->
                let (expr, ctx') = compileFor ctx varName startExpr endExpr stepExpr direction body pos
                (withStatementCheck ctx expr, ctx')
            | Switch(expr, cases, defaultCase, _) ->
                (withStatementCheck ctx (compileSwitch ctx expr cases defaultCase), ctx)
            | Return(valueOpt, _) ->
                // Return from a function - returns the expression value (or null if bare return)
                let returnValue =
                    match valueOpt with
                    | Some expr -> compileExpr ctx expr
                    | None -> Expression.Constant(JyroNull.Instance, typeof<JyroValue>) :> Expression
                match ctx.ReturnLabel with
                | Some label ->
                    (withStatementCheck ctx (Expression.Return(label, returnValue) :> Expression), ctx)
                | None -> JyroError.raiseRuntime MessageCode.ReturnOutsideFunction [||]
            | Exit(messageOpt, _) ->
                // Exit - clean script termination
                let setMessageExpr =
                    match messageOpt with
                    | Some expr ->
                        let compiled = compileExpr ctx expr
                        let msgStr = Expression.Call(scriptStringMethod, compiled) :> Expression
                        Some (Expression.Call(ctx.ContextParam, setReturnMessageMethod, msgStr) :> Expression)
                    | None -> None
                if ctx.InFunction then
                    // Inside a function, exit must escape the entire call stack,
                    // so it is thrown rather than jumped. JyroScriptExit is a
                    // control-flow signal, not a diagnostic: execute() catches
                    // it as clean termination. No statement catches it on the
                    // way, so it unwinds in one dispatch (JYRO-ADR-108).
                    //
                    // The message is evaluated once and recorded on the
                    // context, which is where the result's ExitMessage comes
                    // from; the signal carries none (JYRO-DEF-552). It used to
                    // be compiled twice, so `exit Next()` inside a function
                    // called `Next` twice.
                    let exitSignal = Expression.New(EngineMembers.newScriptExit)
                    let block =
                        match messageOpt with
                        | Some expr ->
                            let compiled = compileExpr ctx expr
                            Expression.Block(
                                [| Expression.Call(ctx.ContextParam, setReturnMessageMethod,
                                                   Expression.Call(scriptStringMethod, compiled)) :> Expression
                                   Expression.Throw(exitSignal, typeof<System.Void>) :> Expression |]) :> Expression
                        | None ->
                            Expression.Throw(exitSignal, typeof<System.Void>) :> Expression
                    (withStatementCheck ctx block, ctx)
                else
                    // At top level, jump to the script return label
                    match ctx.ReturnLabel with
                    | Some label ->
                        let returnExpr = Expression.Return(label, ctx.OutputParam :> Expression) :> Expression
                        let block =
                            match setMessageExpr with
                            | Some setMsg -> Expression.Block(setMsg, returnExpr) :> Expression
                            | None -> returnExpr
                        (withStatementCheck ctx block, ctx)
                    | None ->
                        let block =
                            match setMessageExpr with
                            | Some setMsg -> Expression.Block(setMsg, ctx.OutputParam :> Expression) :> Expression
                            | None -> ctx.OutputParam :> Expression
                        (withStatementCheck ctx block, ctx)
            | FuncDef _ ->
                // No-op at script level - function bodies are compiled separately
                (Expression.Empty() :> Expression, ctx)
            | UnionDef _ ->
                // No-op at script level - union definitions are processed by the linker
                (Expression.Empty() :> Expression, ctx)
            | Match(expr, cases, _) ->
                (withStatementCheck ctx (compileMatch ctx expr cases), ctx)
            | Fail(messageOpt, _) ->
                let messageExpr =
                    match messageOpt with
                    | Some expr ->
                        let compiled = compileExpr ctx expr
                        Expression.Call(scriptStringMethod, compiled) :> Expression
                    | None ->
                        Expression.Constant(MessageTemplates.ScriptFailureWithoutValue, typeof<string>) :> Expression

                // Bound once. The message expression is read three times below, and
                // evaluating a call or a string concatenation once per read charges the
                // statement budget for work the script asked for once.
                let messageVar = Expression.Variable(typeof<string>, "failMessage")
                let bindMessage = Expression.Assign(messageVar, messageExpr) :> Expression

                let setMsgExpr = Expression.Call(ctx.ContextParam, setReturnMessageMethod, messageVar) :> Expression

                // The text of a value is the one argument (spec 50.11.4), and
                // `DiagnosticMessage.Message` renders it through the template `{0}`
                // unchanged. A `fail` with no value has no argument (spec 50.11.4,
                // 160.7.16); `DiagnosticMessage.Message` gives it the fixed text.
                let argsExpr =
                    match messageOpt with
                    | Some _ ->
                        Expression.NewArrayInit(
                            typeof<obj>,
                            [| Expression.Convert(messageVar, typeof<obj>) :> Expression |]) :> Expression
                    | None ->
                        Expression.Constant(Array.empty<obj>, typeof<obj[]>) :> Expression

                let rteNew = Expression.New(
                    runtimeExceptionCtor,
                    Expression.Constant(MessageCode.ScriptFailure, typeof<MessageCode>),
                    argsExpr)
                let throwExpr = Expression.Throw(rteNew, typeof<System.Void>) :> Expression

                let block =
                    Expression.Block([| messageVar |], [| bindMessage; setMsgExpr; throwExpr |]) :> Expression
                (withStatementCheck ctx block, ctx)
            | Break _ ->
                match ctx.BreakLabel with
                // `break` and `continue` are statements that run, so each adds
                // one to the statement count at its start (spec 100.2.2, 100.2.10).
                | Some label -> (withStatementCheck ctx (Expression.Break(label) :> Expression), ctx)
                | None -> JyroError.raiseRuntime MessageCode.LoopStatementOutsideOfLoop [| box "break" |]
            | Continue _ ->
                match ctx.ContinueLabel with
                | Some label -> (withStatementCheck ctx (Expression.Continue(label) :> Expression), ctx)
                | None -> JyroError.raiseRuntime MessageCode.LoopStatementOutsideOfLoop [| box "continue" |]
            | Delete(target, _) ->
                (withStatementCheck ctx (compileDelete ctx target), ctx)
            | ExprStmt(expr, _) ->
                (withStatementCheck ctx (compileExpr ctx expr), ctx)
        (withLocationTracking ctx compiled pos, ctx')

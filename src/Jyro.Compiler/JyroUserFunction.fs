namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast

open System
open System.Collections.Generic

/// A user-defined function declared in a Jyro script.
/// Created as a shell before function bodies are compiled, enabling mutual recursion.
/// The compiled delegate is set via SetCompiled before any script code executes.
///
/// The compiled delegate is held in a plain field (not wrapped in `option`)
/// so that the expression-tree compiler can read it as a property and emit
/// `Expression.Invoke` directly - skipping the `IJyroFunction.Execute`
/// interface dispatch and one extra CLR frame per call.
type internal JyroUserFunction(name: string, signature: JyroFunctionSignature) =
    // Default delegate throws `UndefinedFunctionRuntime` if invoked before
    // the function body is compiled. In practice Linker + Compiler always
    // call SetCompiled before any script code runs.
    let mutable compiled: Func<IReadOnlyList<JyroValue>, JyroExecutionContext, JyroValue> =
        Func<_, _, _>(fun _ _ ->
            JyroError.raiseRuntime MessageCode.UndefinedFunctionRuntime [| box name |])

    /// Effect inferred by the Linker from the function body (fixed-point
    /// over all callees). Defaults to Pure before inference runs.
    let mutable effect: FunctionEffect = FunctionEffect.Pure

    /// Sets the compiled delegate for this function. Called after the function body is compiled.
    member _.SetCompiled(fn: Func<IReadOnlyList<JyroValue>, JyroExecutionContext, JyroValue>) =
        compiled <- fn

    /// Sets the inferred effect. Called by the Linker during effect inference.
    member _.SetEffect(e: FunctionEffect) =
        effect <- e

    /// The compiled delegate for this function. Read at runtime from the
    /// expression tree via `Expression.Property` so call sites can skip
    /// the `IJyroFunction.Execute` dispatch.
    member _.CompiledDelegate = compiled

    interface IJyroFunction with
        member _.Name = name
        member _.Signature = signature
        member _.Effect = effect
        member _.Execute(args, ctx) = compiled.Invoke(args, ctx)

module internal JyroUserFunction =
    let paramTypeFromJyroType (t: JyroType option) : ParameterType =
        match t with
        | None -> ParameterType.Any
        | Some AnyType -> ParameterType.Any
        | Some NumberType -> ParameterType.Number
        | Some StringType -> ParameterType.String
        | Some BooleanType -> ParameterType.Boolean
        | Some ObjectType -> ParameterType.Object
        | Some ArrayType -> ParameterType.Array
        | Some NullType -> ParameterType.Null
        | Some FunctionType -> ParameterType.Function

    let buildSignature (parameters: FuncParam list) : JyroFunctionSignature =
        let paramDefs =
            parameters |> List.map (fun p ->
                let pType = paramTypeFromJyroType p.TypeHint
                if p.DefaultValue.IsSome then
                    Parameter.Optional(p.Name, pType)
                else
                    Parameter.Required(p.Name, pType))
        JyroFunctionSignature.Create(paramDefs, ParameterType.Any)

    /// Create a JyroUserFunction shell from a FuncDef AST node's name and parameters
    let createShell (name: string) (parameters: FuncParam list) : JyroUserFunction =
        let signature = buildSignature parameters
        JyroUserFunction(name, signature)

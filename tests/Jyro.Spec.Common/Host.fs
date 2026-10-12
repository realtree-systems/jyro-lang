namespace RealTree.Jyro.Spec

open System.Collections.Generic
open RealTree.Jyro

/// A host function built on the public `JyroFunctionBase`, which records every
/// call it receives (the arguments, in order) and answers with `reply`.
/// Recording is thread-safe, so concurrent executions may share one.
type RecordingFunction
    (name: string,
     signature: JyroFunctionSignature,
     effect: FunctionEffect,
     reply: JyroValue list -> JyroExecutionContext -> JyroValue) =
    inherit JyroFunctionBase(name, signature)

    let calls = ResizeArray<JyroValue list>()
    let gate = obj ()

    override _.Effect = effect

    override _.Execute(args: IReadOnlyList<JyroValue>, context: JyroExecutionContext) =
        let received = List.ofSeq args
        lock gate (fun () -> calls.Add received)
        reply received context

    /// Every call so far, each as the list of arguments it was given.
    member _.Calls : JyroValue list list = lock gate (fun () -> List.ofSeq calls)

    /// The number of calls so far.
    member _.CallCount : int = lock gate (fun () -> calls.Count)

    /// This function as the interface the compiler takes (`AddFunction`).
    member this.AsFunction : IJyroFunction = this :> IJyroFunction

/// Making host functions.
module Host =

    /// A signature from its parts. A signature states each fact once and has no name (JYRO-ADR-192):
    /// `_name` is kept so callers read as before, and is not used.
    let signature (_name: string) (parameters: Parameter list) (returns: ParameterType) : JyroFunctionSignature =
        JyroFunctionSignature.Create(parameters, returns)

    /// A recording host function with the given parameters, return type and
    /// effect, whose result is `reply args`.
    let func
        (name: string)
        (parameters: Parameter list)
        (returns: ParameterType)
        (effect: FunctionEffect)
        (reply: JyroValue list -> JyroValue)
        : RecordingFunction =
        RecordingFunction(name, signature name parameters returns, effect, (fun args _ -> reply args))

    /// As `func`, with the execution context passed to `reply` as well.
    let funcWithContext
        (name: string)
        (parameters: Parameter list)
        (returns: ParameterType)
        (effect: FunctionEffect)
        (reply: JyroValue list -> JyroExecutionContext -> JyroValue)
        : RecordingFunction =
        RecordingFunction(name, signature name parameters returns, effect, reply)

    /// A `Pure` recording host function taking 0 to 8 arguments of any type,
    /// returning `null`.
    let recorder (name: string) : RecordingFunction =
        let ps = [ for i in 0 .. 7 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]
        func name ps ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)

    /// A recording host function with the given effect taking 0 to 8 arguments
    /// of any type, returning `null`.
    let recorderWithEffect (name: string) (effect: FunctionEffect) : RecordingFunction =
        let ps = [ for i in 0 .. 7 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]
        func name ps ParameterType.Any effect (fun _ -> Values.nul)

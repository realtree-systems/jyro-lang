namespace RealTree.Jyro

/// <summary>
/// The one-call members, for hosts that compile and run in the same breath: the only members of the host API that compile and run a
/// script in one call. Anything that reuses a program across runs should hold
/// a <c>CompiledProgram</c> from <c>JyroCompiler</c> and execute it through a
/// <c>JyroRunner</c> instead - these recompile on every call. No extension method
/// compiles or runs a script.
/// </summary>
[<AutoOpen>]
module JyroScript =

    // Compile with the stdlib available and run once, reporting a compile-stage
    // failure through the same result type as a run-stage one.
    let internal compileThenRun (source: string) (input: JyroValue) : JyroExecutionResult =
        match JyroCompiler().WithStdlib().WithSource(source).Compile() with
        | Failed msgs ->
            JyroExecutionResult.Of(JyroResult<JyroValue>.Failure(msgs))
        | Produced program ->
            JyroRunner(program).WithInput(input).Execute()
        | _ ->
            JyroExecutionResult.Of(
                JyroResult<JyroValue>.Failure(DiagnosticMessage.Of(MessageCode.InternalStageFailure)))

    // Converts the input and runs, returning a failed result, not raising,
    // when the input is nested deeper than the maximum nesting depth or is a
    // cyclic host graph (spec 100.7.6), is text that is not JSON (spec
    // 160.1, JYRO-REQ-1657), holds a number that is not finite (spec
    // 30.2.18), or is or holds a delegate, reflection metadata or a task
    // (`HostValueNotConvertible`, JYRO-ADR-200 item 2): the failure channel
    // of these members is the result they return.
    let internal convertThenRun (source: string) (convert: unit -> JyroValue) : JyroExecutionResult =
        let converted =
            try Choice1Of2 (convert ())
            with :? JyroRuntimeException as ex when JyroRunner.IsInputDataFault ex.Code ->
                Choice2Of2 (JyroRunner.HostInputFault ex)
        match converted with
        | Choice1Of2 input -> compileThenRun source input
        | Choice2Of2 message -> JyroExecutionResult.Of(JyroResult<JyroValue>.Failure(message))

    // The input of `executeScript`: `convertThenRun`'s conversion of a host
    // object, written out rather than through a closure, whose code the
    // trimmer would not count as this function's own.
    [<System.Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    let private objectInput (data: obj | null) : Choice<JyroValue, DiagnosticMessage> =
        try Choice1Of2 (JyroValue.FromObject(data))
        with :? JyroRuntimeException as ex when JyroRunner.IsInputDataFault ex.Code ->
            Choice2Of2 (JyroRunner.HostInputFault ex)

    /// <summary>
    /// Compiles <c>source</c> with the standard library and runs it once with
    /// <c>data</c>, a host object converted by <c>JyroValue.FromObject</c>, as <c>Input</c>.
    ///
    /// It reads an object by the public properties and fields of its type,
    /// which trimming may remove, so it warns a host that trims, as
    /// <c>JyroValue.FromObject</c> does.
    /// </summary>
    [<CompiledName("ExecuteScript")>]
    [<System.Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    let executeScript (source: string) (data: obj | null) : JyroExecutionResult =
        if obj.ReferenceEquals(source, null) then raise (System.ArgumentNullException(nameof source))
        match objectInput data with
        | Choice1Of2 input -> compileThenRun source input
        | Choice2Of2 message -> JyroExecutionResult.Of(JyroResult<JyroValue>.Failure(message))

    /// <summary>
    /// Compiles <c>source</c> with the standard library and runs it once with the
    /// value of the JSON text <c>json</c> as <c>Input</c>.
    /// </summary>
    [<CompiledName("ExecuteScriptWithJson")>]
    let executeScriptWithJson (source: string) (json: string) : JyroExecutionResult =
        if obj.ReferenceEquals(source, null) then raise (System.ArgumentNullException(nameof source))
        if obj.ReferenceEquals(json, null) then raise (System.ArgumentNullException(nameof json))
        convertThenRun source (fun () -> JyroValue.FromJson(json))

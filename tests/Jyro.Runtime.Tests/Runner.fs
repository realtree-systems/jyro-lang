namespace RealTree.Jyro.Tests

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Thin wrapper around the compile-then-run pipeline, tailored for property
/// tests. All runtime tests go through `run` / `runOk` / `runFail` so we have a
/// single place to tune execution options (timeouts, resource limits).
module Runner =

    /// Compile, then execute the resulting program, reporting a compile-stage
    /// failure through the same result type as a run-stage one.
    ///
    /// Compilation and execution are separate types (`JyroCompiler` then
    /// `JyroRunner`) precisely so that limits cannot be mistaken for something
    /// the program carries. Tests assert on compile diagnostics as often as
    /// runtime ones, so this returns a result rather than using
    /// `CompileOrThrow`.
    let compileThenRun (compiler: JyroCompiler) (prepare: JyroRunner -> JyroRunner) : JyroResult<JyroValue> =
        match compiler.Compile() with
        | Failed msgs ->
            JyroResult<JyroValue>.Failure(msgs)
        | Produced program ->
            (prepare (JyroRunner(program))).Execute()
        | _ ->
            JyroResult<JyroValue>.Failure(
                DiagnosticMessage.Error(MessageCode.InternalStageFailure, MessageStage.Compile, [| box "compile" |]))

    /// Load an artifact, then execute the resulting program, reporting a load
    /// failure through the same result type as a run-stage one.
    let loadThenRun (loader: JyroArtifactLoader) (prepare: JyroRunner -> JyroRunner) : JyroResult<JyroValue> =
        match loader.Load() with
        | Failed msgs ->
            JyroResult<JyroValue>.Failure(msgs)
        | Produced program ->
            (prepare (JyroRunner(program))).Execute()
        | _ ->
            JyroResult<JyroValue>.Failure(
                DiagnosticMessage.Error(MessageCode.InternalStageFailure, MessageStage.Deserialize, [| box "load" |]))

    /// Execute a Jyro source string with the given input. Returns the raw
    /// `JyroResult<JyroValue>` so callers can inspect both success and
    /// failure outcomes. Uses the runtime's default execution options -
    /// post-refactor (see CallDepthMeasurement) the default MaxCallDepth
    /// of 64 is well within .NET's stack budget.
    let run (source: string) (input: JyroValue) : JyroResult<JyroValue> =
        compileThenRun
            (JyroCompiler().WithStdlib().WithSource(source))
            (fun r -> r.WithInput(input))

    /// Execute with an empty `JyroObject` input.
    let runEmpty (source: string) : JyroResult<JyroValue> =
        run source (JyroObject() :> JyroValue)

    /// Assert success and return the output value.
    let expectSuccess (source: string) (input: JyroValue) : JyroValue =
        let r = run source input
        if not r.IsSuccess then
            failwithf
                "Expected success but got failure.\nSource:\n%s\nDiagnostics: %A"
                source (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> (m.Code, m.Message)))
        match r.TryValue with
        | Some v -> v
        | None -> failwith "IsSuccess=true but Value was None"

    /// Assert failure and return the diagnostic codes.
    let expectFailure (source: string) (input: JyroValue) : MessageCode list =
        let r = run source input
        if r.IsSuccess then
            failwithf "Expected failure but got success.\nSource:\n%s" source
        List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)

    // -- Helpers for building inputs -----------------------------------------

    let objOf (pairs: (string * JyroValue) list) : JyroValue =
        let o = JyroObject()
        for (k, v) in pairs do
            o.SetProperty(k, v)
        o :> JyroValue

    let arrOf (values: JyroValue seq) : JyroValue =
        let a = JyroArray()
        for v in values do
            a.Add(v)
        a :> JyroValue

    let num (n: float) : JyroValue = JyroNumber(n) :> JyroValue
    let str (s: string) : JyroValue = JyroString(s) :> JyroValue
    let bool (b: bool) : JyroValue = JyroBoolean.FromBoolean(b) :> JyroValue
    let nullv : JyroValue = JyroNull.Instance :> JyroValue

    /// Read a property off a runtime result. Used for extracting Output.foo
    /// after script execution.
    let prop (name: string) (v: JyroValue) : JyroValue = v.GetProperty(name)

    let asNumber (v: JyroValue) : float =
        match v with
        | :? JyroNumber as n -> n.Value
        | _ -> failwithf "expected JyroNumber, got %O" (v.GetType())

    let asBool (v: JyroValue) : bool =
        match v with
        | :? JyroBoolean as b -> b.Value
        | _ -> failwithf "expected JyroBoolean, got %O" (v.GetType())

    let asString (v: JyroValue) : string =
        match v with
        | :? JyroString as s -> s.Value
        | _ -> failwithf "expected JyroString, got %O" (v.GetType())

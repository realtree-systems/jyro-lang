namespace RealTree.Jyro.Spec

open System
open System.Threading
open RealTree.Jyro

/// How `Input` is given to an execution (160.1.2).
type Input =
    /// Do not call any `WithInput`/`WithInputJson`: the runner's own default.
    | NoInput
    /// `JyroRunner.WithInputJson(text)`.
    | JsonInput of string
    /// `JyroRunner.WithInput(JyroValue)`.
    | ValueInput of JyroValue
    /// `JyroRunner.WithInput(Object)` - a host (CLR) value.
    | HostInput of obj

/// Everything a host can choose for one compile-and-run. Start from
/// `Script.defaults` and copy-and-update: `{ Script.defaults with Stdlib = false }`.
type RunOptions =
    { /// `JyroCompiler.WithStdlib()` when true. Default: true.
      Stdlib: bool
      /// Default: `JsonInput "{}"` (an empty object, made fresh for each run).
      Input: Input
      /// `JyroRunner.WithOutput` when set (a first value for `Output`, 160.1.3).
      Output: JyroValue option
      /// `JyroRunner.WithExecutionOptions` when set (resource limits).
      Limits: JyroExecutionOptions option
      /// `JyroCompiler.WithMaxEffect` when set.
      MaxEffect: FunctionEffect option
      /// `JyroCompiler.WithLibraryMaxEffect` when set.
      LibraryMaxEffect: FunctionEffect option
      /// Host functions, added with `JyroCompiler.AddFunction` in order.
      Functions: IJyroFunction list
      /// Library script sources, added with `JyroCompiler.AddLibrarySource` in order.
      Libraries: string list
      /// `JyroRunner.WithCancellationToken` when set.
      Cancellation: CancellationToken option
      /// `JyroCompiler.WithMaxCompilationTime` when set.
      CompilationTimeout: TimeSpan option
      /// `WithStats` on both the compiler and the runner when set.
      Stats: JyroPipelineStats option
      /// Last step on the compiler, for anything not listed above.
      ConfigureCompiler: JyroCompiler -> JyroCompiler
      /// Last step on the runner, for anything not listed above
      /// (for example `fun r -> r.WithMaxRegexTime(TimeSpan.FromMilliseconds 50.)`).
      ConfigureRunner: JyroRunner -> JyroRunner }

/// Compiling and running scripts through the public host API
/// (`JyroCompiler` then `JyroRunner`), and reading the result.
module Script =

    /// Stdlib on, `{}` as `Input` (JSON text, so no instance is shared between
    /// runs), nothing else set.
    let defaults : RunOptions =
        { Stdlib = true
          Input = JsonInput "{}"
          Output = None
          Limits = None
          MaxEffect = None
          LibraryMaxEffect = None
          Functions = []
          Libraries = []
          Cancellation = None
          CompilationTimeout = None
          Stats = None
          ConfigureCompiler = id
          ConfigureRunner = id }

    /// The default `JyroExecutionOptions`, changed by `change` (a copy-and-update).
    /// Use as `Limits = Some (Script.limits (fun o -> { o with MaxStatements = 10 }))`.
    let limits (change: JyroExecutionOptions -> JyroExecutionOptions) : JyroExecutionOptions =
        change JyroExecutionOptions.Default

    /// The compiler for `options`, without a source or compiled bytes.
    let compilerFor (options: RunOptions) : JyroCompiler =
        let mutable c = JyroCompiler()
        if options.Stdlib then c <- c.WithStdlib()
        options.MaxEffect |> Option.iter (fun e -> c <- c.WithMaxEffect(e))
        options.LibraryMaxEffect |> Option.iter (fun e -> c <- c.WithLibraryMaxEffect(e))
        for f in options.Functions do c <- c.AddFunction(f)
        for l in options.Libraries do c <- c.AddLibrarySource(l)
        options.CompilationTimeout |> Option.iter (fun t -> c <- c.WithMaxCompilationTime(t))
        options.Stats |> Option.iter (fun s -> c <- c.WithStats(s))
        options.ConfigureCompiler c

    /// The runner for a compiled program under `options`.
    let runnerFor (options: RunOptions) (program: CompiledProgram) : JyroRunner =
        let mutable r = JyroRunner(program)
        match options.Input with
        | NoInput -> ()
        | JsonInput text -> r <- r.WithInputJson(text)
        | ValueInput v -> r <- r.WithInput(v)
        | HostInput o -> r <- r.WithInput(o)
        options.Output |> Option.iter (fun o -> r <- r.WithOutput(o))
        options.Limits |> Option.iter (fun l -> r <- r.WithExecutionOptions(l))
        options.Cancellation |> Option.iter (fun t -> r <- r.WithCancellationToken(t))
        options.Stats |> Option.iter (fun s -> r <- r.WithStats(s))
        options.ConfigureRunner r

    /// Compile only (`JyroCompiler.Compile()`): for validation and link errors.
    let compileWith (options: RunOptions) (source: string) : JyroResult<CompiledProgram> =
        (compilerFor options).WithSource(source).Compile()

    /// Compile only, with the defaults.
    let compile (source: string) : JyroResult<CompiledProgram> = compileWith defaults source

    /// Compile to an artifact (`JyroCompiler.CompileToArtifact()`).
    let compileBytesWith (options: RunOptions) (source: string) : JyroResult<byte[]> =
        (compilerFor options).WithSource(source).CompileToArtifact()

    /// The artifact loader for `bytes`, set up as `options` say. The options a loader
    /// has no member for are not applied: libraries (the loader has no member that adds
    /// one) and `ConfigureCompiler`.
    let loaderFor (options: RunOptions) (bytes: byte[]) : JyroArtifactLoader =
        let mutable l = JyroArtifactLoader(bytes)
        if options.Stdlib then l <- l.WithStdlib()
        options.MaxEffect |> Option.iter (fun e -> l <- l.WithMaxEffect(e))
        for f in options.Functions do l <- l.AddFunction(f)
        options.CompilationTimeout |> Option.iter (fun t -> l <- l.WithMaxCompilationTime(t))
        options.Stats |> Option.iter (fun s -> l <- l.WithStats(s))
        l

    /// Load only: `JyroArtifactLoader(bytes)` set up as `options` say, then `Load()`.
    let loadBytesWith (options: RunOptions) (bytes: byte[]) : JyroResult<CompiledProgram> =
        (loaderFor options bytes).Load()

    /// Load only, throwing: `Load()` then `GetValueOrThrow()` (a `JyroException` when the load failed).
    let loadBytesOrThrow (options: RunOptions) (bytes: byte[]) : CompiledProgram =
        (loaderFor options bytes).Load().GetValueOrThrow()

    let private execute (options: RunOptions) (compiled: JyroResult<CompiledProgram>) : JyroResult<JyroValue> =
        match compiled.TryGetValue() with
        | true, program when compiled.IsSuccess -> (runnerFor options program).Execute()
        | _ -> JyroResult<JyroValue>.Failure(compiled.Messages)

    /// Compile then run. A compile failure comes back as a failed
    /// `JyroResult<JyroValue>` carrying the compiler's diagnostics (the same
    /// `DiagnosticMessage` objects), with no value.
    let runWith (options: RunOptions) (source: string) : JyroResult<JyroValue> =
        execute options (compileWith options source)

    /// Run a compiled artifact (`JyroArtifactLoader(bytes)` ... `Load()`) under `options`.
    let runBytesWith (options: RunOptions) (bytes: byte[]) : JyroResult<JyroValue> =
        execute options (loadBytesWith options bytes)

    /// Run with the defaults (stdlib on, empty object `Input`).
    let run (source: string) : JyroResult<JyroValue> = runWith defaults source

    /// Run with `Input` given as JSON text (`WithInputJson`).
    let runJson (inputJson: string) (source: string) : JyroResult<JyroValue> =
        runWith { defaults with Input = JsonInput inputJson } source

    /// Run with `Input` given as a Jyro value (`WithInput(JyroValue)`).
    let runInput (input: JyroValue) (source: string) : JyroResult<JyroValue> =
        runWith { defaults with Input = ValueInput input } source

    // -- Reading a result -------------------------------------------------

    /// One line per diagnostic: severity, code, number, message. For failure text.
    let describe (result: JyroResult<'T>) : string =
        result.Messages
        |> Seq.map (fun m -> sprintf "%A %A (%d): %s" m.Severity m.Code (int m.Code) m.Message)
        |> String.concat "\n"

    /// `IsSuccess`.
    let ok (result: JyroResult<'T>) : bool = result.IsSuccess

    /// The result's value, if it has one (a failed `fail` still carries Output, 140.4.3).
    let value (result: JyroResult<'T>) : 'T option =
        match result.TryGetValue() with
        | true, v -> Some v
        | _ -> None

    /// The result's value; fails the test when there is none.
    let valueOf (result: JyroResult<'T>) : 'T =
        match value result with
        | Some v -> v
        | None -> Values.fail (sprintf "The result has no value. Diagnostics:\n%s" (describe result))

    /// The `Output` value of a successful execution; fails the test (listing the
    /// diagnostics) when the execution did not succeed.
    let output (result: JyroResult<JyroValue>) : JyroValue =
        if not result.IsSuccess then
            Values.fail (sprintf "Expected success but the execution failed. Diagnostics:\n%s" (describe result))
        valueOf result

    /// Run `source` with the defaults and return its `Output` (asserting success).
    let outputOf (source: string) : JyroValue = output (run source)

    /// Every diagnostic code, in order, of every severity.
    let codes (result: JyroResult<'T>) : MessageCode list =
        result.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq

    /// The codes of the diagnostics of severity `Error`, in order.
    let errorCodes (result: JyroResult<'T>) : MessageCode list =
        result.Messages
        |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error)
        |> Seq.map (fun m -> m.Code)
        |> List.ofSeq

    /// The only diagnostic of severity `Error`. Fails the test when the result
    /// succeeded or has zero or several errors.
    let onlyErrorMessage (result: JyroResult<'T>) : DiagnosticMessage =
        let errors = result.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        match errors with
        | [ m ] when not result.IsSuccess -> m
        | [ _ ] -> Values.fail (sprintf "An error diagnostic, but IsSuccess is true:\n%s" (describe result))
        | [] -> Values.fail (sprintf "Expected exactly one error, got none. Diagnostics:\n%s" (describe result))
        | _ -> Values.fail (sprintf "Expected exactly one error, got %d:\n%s" errors.Length (describe result))

    /// The code of the only error (see `onlyErrorMessage`).
    let onlyError (result: JyroResult<'T>) : MessageCode = (onlyErrorMessage result).Code

    /// Assert that `result` failed with exactly one error, of code `code`.
    let expectError (code: MessageCode) (result: JyroResult<'T>) : unit =
        let actual = onlyError result
        if actual <> code then
            Values.fail (sprintf "Expected the one error %A (%d), got %A (%d). Diagnostics:\n%s"
                            code (int code) actual (int actual) (describe result))

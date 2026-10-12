namespace RealTree.Jyro

open RealTree.Jyro.Compilation

open System
open System.IO
open System.Threading

// Where the `Input` of the next run comes from: a value, a fault in the
// data the host gave, which the run reports, or a stream not read yet.
type internal RunnerInput =
    | InputValue of JyroValue
    | InputFault of DiagnosticMessage
    | InputStream of Stream

/// <summary>
/// Fluent builder for executing an already-compiled program. Holds the input
/// and the policy for its runs: resource limits, the regex timeout,
/// cancellation, the first value of <c>Output</c>, the host state, and observers.
///
/// The program is a constructor argument rather than a <c>With*</c> member, so a
/// runner with nothing to run cannot be built and the type needs no
/// "no source provided" diagnostic. <c>CompiledProgram</c> is immutable and safe to
/// share across threads; a runner is not.
///
/// Each <c>With</c> member changes the runner it is called on and returns that
/// runner. A runner may be executed more than once, and
/// each <c>Execute()</c> starts from what was set on it, not from what an earlier
/// run left: <c>Output</c> starts as a copy of the value given to <c>WithOutput</c>, and
/// a stream given to <c>WithInputJson</c> is read once, at the first <c>Execute()</c>
/// after the call, its value the <c>Input</c> of every later run.
///
/// Limits belong to the run, not to the program. Neither a
/// <c>CompiledProgram</c> nor the <c>.jyrx</c> artifact it may have come from records a
/// resource limit, so whatever this runner is configured with is what applies -
/// and a runner configured with nothing runs at <c>JyroExecutionOptions.Default</c>,
/// not at whatever the compiling side was told.
///
/// A null argument for a parameter of a reference type throws
/// <c>ArgumentNullException</c> at the call and leaves the runner as it was, except where null is a value the member gives a
/// meaning: <c>WithInput</c>, where it is the Jyro <c>null</c>, and <c>WithHostState</c>,
/// where it is no state.
/// </summary>
[<Sealed>]
type JyroRunner(program: CompiledProgram) =
    // Guarded because C# can pass null and the failure would otherwise surface
    // as a NullReferenceException from inside `Execute`, far from the call that
    // caused it.
    do if obj.ReferenceEquals(program, null) then
        raise (ArgumentNullException(nameof program))

    let mutable input: RunnerInput = InputValue JyroNull.Instance
    let mutable seededOutput: JyroValue option = None
    let mutable executionOptions: JyroExecutionOptions option = None
    let mutable cancellationToken: CancellationToken option = None
    let mutable statsCollector: JyroPipelineStats option = None
    let mutable functionResultObserver: Action<string, JyroValue> | null = null
    let mutable regexTimeout = JyroExecutionOptions.DefaultRegexTime
    let mutable hostState: obj | null = null

    // The options of the runner now, or the default ones: a copy, which the
    // caller may change.
    let currentOptions () =
        match executionOptions with
        | Some options -> options.Copy()
        | None -> JyroExecutionOptions.Default

    // Throws `ArgumentOutOfRangeException` naming `paramName` when `count`,
    // a count limit, is negative (JYRO-ADR-201 item 3). 0 is a limit not
    // set, and takes its default (JYRO-ADR-181).
    static let requireCount (count: int) (paramName: string) =
        if count < 0 then
            raise (ArgumentOutOfRangeException(paramName, box count, "A count limit is 0 (not set) or more."))

    // Throws `ArgumentNullException` naming `paramName` when `value` is null.
    static let requireNotNull (value: obj | null) (paramName: string) =
        if isNull value then raise (ArgumentNullException(paramName))

    // Whether a fault in converting the input is data the host handed over,
    // reported by `Execute` rather than raised by the member that took it:
    // text that is not JSON, and a value nested deeper than the maximum
    // nesting depth or cyclic in a host graph (spec 100.7.6). Their channel is
    // the result, as it is for every other fault in the data of a run.
    //
    // A number that is not finite is one too (spec 30.2.18, JYRO-ADR-5): a
    // host value holding NaN or an infinity, `HostNumberNaN` or
    // `HostNumberInfinite` as the first such number decides (spec 30.2.19,
    // JYRO-ADR-153 (2)), and JSON text holding a number beyond the range of
    // a double, `NumberOverflow`. So is a host value that is or holds a
    // delegate, reflection metadata or a task, `HostValueNotConvertible`
    // (JYRO-ADR-200 item 2), which the first such value or number met decides.
    static member internal IsInputDataFault(code: MessageCode) =
        code = MessageCode.InvalidJson || code = MessageCode.ValueNestingLimitExceeded
        || code = MessageCode.HostNumberNaN || code = MessageCode.HostNumberInfinite
        || code = MessageCode.HostValueNotConvertible
        || code = MessageCode.NumberOverflow

    // The diagnostic for a fault in converting `Input`: the exception's code
    // and arguments. `HostNumberNaN` and `HostNumberInfinite` have no
    // argument here, as where a host member raises them (spec 30.2.18,
    // JYRO-REQ-1676, JYRO-ADR-153 (2)); a number of JSON text beyond the
    // range of a double has the arguments `Input` and the number as the text
    // writes it.
    static member internal HostInputFault(ex: JyroRuntimeException) : DiagnosticMessage =
        DiagnosticMessage.Of(ex.Code, args = ex.Args)

    // Converts the input with `convert`, a fault in the data becoming the
    // diagnostic the next run reports.
    static member private Convert(convert: unit -> JyroValue) : RunnerInput =
        try InputValue (convert ())
        with :? JyroRuntimeException as ex when JyroRunner.IsInputDataFault ex.Code ->
            InputFault (JyroRunner.HostInputFault ex)

    /// <summary>
    /// Sets the <c>Input</c> of every later <c>Execute()</c> to a Jyro value. Changes
    /// this runner and returns it.
    ///
    /// <c>inputData</c> may be null, and a null is the Jyro <c>null</c>, as
    /// <c>JyroNull.Instance</c> is: it does not throw. A C# host
    /// writing <c>WithInput(null)</c> binds here rather than to the <c>object</c>
    /// overload, this one being more specific, and gets the same Jyro <c>null</c>
    /// the <c>object</c> overload gives through <c>JyroValue.FromObject</c>.
    /// </summary>
    member this.WithInput(inputData: JyroValue | null) =
        input <-
            match inputData with
            | null -> InputValue JyroNull.Instance
            | value -> InputValue value
        this

    /// <summary>
    /// Sets the <c>Input</c> of every later <c>Execute()</c> to a host object, converted
    /// by <c>JyroValue.FromObject</c>. Changes this runner and returns it.
    ///
    /// A host graph deeper than the maximum nesting depth, or cyclic, is
    /// reported by <c>Execute</c> with <c>ValueNestingLimitExceeded</c>, rather than
    /// raised here; so is a value holding a number that is not finite, with
    /// <c>HostNumberNaN</c> or <c>HostNumberInfinite</c>, and a value
    /// that is or holds a delegate, reflection metadata or a task, with
    /// <c>HostValueNotConvertible</c>. A null <c>inputData</c> is the Jyro <c>null</c> and
    /// does not throw.
    ///
    /// It reads an object by the public properties and fields of its type,
    /// which trimming may remove, so it warns a host that trims, as
    /// <c>JyroValue.FromObject</c> does.
    /// </summary>
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    member this.WithInput(inputData: obj | null) =
        input <- JyroRunner.ConvertObject(inputData)
        this

    // `Convert` of a host object, written out rather than through a closure,
    // whose code the trimmer would not count as this member's own.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private ConvertObject(inputData: obj | null) : RunnerInput =
        try InputValue (JyroValue.FromObject(inputData))
        with :? JyroRuntimeException as ex when JyroRunner.IsInputDataFault ex.Code ->
            InputFault (JyroRunner.HostInputFault ex)

    /// <summary>
    /// Sets the <c>Input</c> of every later <c>Execute()</c> to the value of JSON text.
    /// Changes this runner and returns it.
    ///
    /// Text that is not JSON (<c>InvalidJson</c>), or that is nested deeper than
    /// the maximum nesting depth, or that holds a number beyond the range of
    /// a double (<c>NumberOverflow</c>, with the arguments <c>Input</c> and the number
    /// as the text writes it), is reported by <c>Execute</c>, rather
    /// than raised here: it is data, not a mistake in the host's code.
    /// </summary>
    member this.WithInputJson(json: string) =
        requireNotNull (box json) (nameof json)
        input <- JyroRunner.Convert(fun () -> JyroValue.FromJson(json))
        this

    /// <summary>
    /// Sets the <c>Input</c> of every later <c>Execute()</c> to the value of the UTF-8
    /// JSON a stream holds. Changes this runner and returns it.
    ///
    /// The stream is read once, to its end, at the first <c>Execute()</c> after
    /// this call, and the value read is the <c>Input</c> of that run and every
    /// later one until another input is set. Text that is not JSON is
    /// reported by that <c>Execute()</c>, as for <c>WithInputJson(string)</c>.
    /// </summary>
    member this.WithInputJson(utf8Json: Stream) =
        requireNotNull (box utf8Json) (nameof utf8Json)
        input <- InputStream utf8Json
        this

    /// <summary>
    /// Sets the first value of <c>Output</c> of every later <c>Execute()</c>, which the
    /// script edits in place, instead of the empty object it otherwise starts
    /// from. Changes this runner and returns it.
    ///
    /// For a host that hands the script a structure to modify: <c>Input</c> is
    /// read-only, so a script cannot transform anything reached through it.
    /// Each run starts from a copy of <c>value</c>, taken when <c>Execute()</c> is
    /// called, so no run changes <c>value</c> and a second run does not continue
    /// from what the first left; the host reads the script's edits from the
    /// result. Each writable container of <c>value</c> is copied once: a container
    /// that <c>value</c> reaches by more than one path becomes one copy reached by
    /// the same paths. A container of <c>value</c> that is read-only is kept as it
    /// is, and stays read-only. The copy is the first value of <c>Output</c>, so the
    /// maximum value size does not apply to it.
    /// </summary>
    member this.WithOutput(value: JyroValue) =
        requireNotNull (box value) (nameof value)
        seededOutput <- Some value
        this

    /// <summary>
    /// Sets the resource limits and the regex timeout of every later
    /// <c>Execute()</c>. Changes this runner and returns it.
    ///
    /// Its <c>MaxExecutionTime</c> is <c>TimeSpan.MaxValue</c>, which is no limit, or
    /// more than zero and at most 24 hours. Its
    /// <c>MaxStatements</c>, <c>MaxLoopIterations</c> and <c>MaxCallDepth</c> are 0, a limit
    /// not set that takes its default, or more. Its
    /// <c>MaxRegexTime</c> is zero, which leaves the regex timeout as it was, or
    /// more than zero and at most 24 hours. Any other value throws
    /// <c>ArgumentOutOfRangeException</c> naming <c>options</c>, and the runner keeps
    /// every limit it had: none of the options is applied.
    ///
    /// A copy is kept, not the host's instance, so a value set on it after
    /// this call does not reach <c>Execute</c> unchecked.
    /// </summary>
    member this.WithExecutionOptions(options: JyroExecutionOptions) =
        requireNotNull (box options) (nameof options)
        let copy = options.Copy()
        if copy.MaxStatements < 0 || copy.MaxLoopIterations < 0 || copy.MaxCallDepth < 0 then
            raise (ArgumentOutOfRangeException(
                    nameof options, box (copy.ToString()),
                    "MaxStatements, MaxLoopIterations and MaxCallDepth are 0 (not set) or more."))
        JyroExecutionOptions.RequireTimeLimit(
            copy.MaxExecutionTime, nameof options,
            "MaxExecutionTime is TimeSpan.MaxValue (no limit), or more than zero and at most 24 hours.")
        if copy.MaxRegexTime <> TimeSpan.Zero && not (JyroExecutionOptions.IsAcceptedRegexTime copy.MaxRegexTime) then
            raise (ArgumentOutOfRangeException(
                    nameof options, box copy.MaxRegexTime,
                    "MaxRegexTime is zero (not set), or more than zero and at most 24 hours."))
        // A count limit left at zero is one the host did not set, and takes
        // its 100.1 default (JYRO-REQ-1405, JYRO-ADR-181).
        executionOptions <- Some (JyroExecutionOptions.WithDefaultsForUnset copy)
        if copy.MaxRegexTime <> TimeSpan.Zero then regexTimeout <- copy.MaxRegexTime
        this

    /// <summary>
    /// Sets the execution time limit of every later <c>Execute()</c>. Changes this
    /// runner and returns it.
    ///
    /// <c>time</c> is <c>TimeSpan.MaxValue</c>, which removes the limit, or more than
    /// zero and at most 24 hours. Any other value - zero,
    /// a negative value, <c>Timeout.InfiniteTimeSpan</c> included, or more than 24
    /// hours - throws <c>ArgumentOutOfRangeException</c> and leaves the limit the
    /// runner had.
    /// </summary>
    member this.WithMaxExecutionTime(time: TimeSpan) =
        JyroExecutionOptions.RequireTimeLimit(
            time, nameof time,
            "An execution time limit is TimeSpan.MaxValue (no limit), or more than zero and at most 24 hours.")
        let opts = currentOptions ()
        let opts = { opts with MaxExecutionTime = time }
        executionOptions <- Some opts
        this

    /// <summary>
    /// Sets the statement limit of every later <c>Execute()</c>. Changes this
    /// runner and returns it. 0 is a limit not set, which takes its default;
    /// a negative count throws <c>ArgumentOutOfRangeException</c> and leaves the
    /// runner as it was.
    /// </summary>
    member this.WithMaxStatements(count: int) =
        requireCount count (nameof count)
        let opts = currentOptions ()
        let opts = { opts with MaxStatements = count }
        executionOptions <- Some opts
        this

    /// <summary>
    /// Sets the loop iteration limit of every later <c>Execute()</c>. Changes this
    /// runner and returns it. 0 is a limit not set, which takes its default;
    /// a negative count throws <c>ArgumentOutOfRangeException</c> and leaves the
    /// runner as it was.
    /// </summary>
    member this.WithMaxLoopIterations(count: int) =
        requireCount count (nameof count)
        let opts = currentOptions ()
        let opts = { opts with MaxLoopIterations = count }
        executionOptions <- Some opts
        this

    /// <summary>
    /// Sets the call depth limit of every later <c>Execute()</c>. Changes this
    /// runner and returns it. 0 is a limit not set, which takes its default;
    /// a negative depth throws <c>ArgumentOutOfRangeException</c> and leaves the
    /// runner as it was. A depth above <c>JyroExecutionOptions.MaxSafeCallDepth</c>
    /// is reduced to it when the run starts - the CLR stack cannot safely
    /// accommodate more frames.
    /// </summary>
    member this.WithMaxCallDepth(depth: int) =
        requireCount depth (nameof depth)
        let opts = currentOptions ()
        let opts = { opts with MaxCallDepth = depth }
        executionOptions <- Some opts
        this

    /// <summary>
    /// Sets the regex timeout of every later <c>Execute()</c>: how long one
    /// evaluation of a regular expression function may take before it stops
    /// with <c>RegexTimeout</c>. Changes this runner and
    /// returns it. A runner on which neither this nor <c>WithExecutionOptions</c>
    /// with a <c>MaxRegexTime</c> other than zero is called uses 1 second; of the two, the one called last sets it.
    ///
    /// <c>time</c> is more than zero and at most 24 hours. Any other value - zero,
    /// a negative value, <c>Timeout.InfiniteTimeSpan</c> included, or more than 24
    /// hours - throws <c>ArgumentOutOfRangeException</c> naming <c>time</c>, and the
    /// runner keeps the regex timeout it had. There is no "no limit": a
    /// regular expression a script supplies is one of the few things in a run
    /// that can take unbounded time without reaching a checkpoint.
    /// </summary>
    member this.WithMaxRegexTime(time: TimeSpan) =
        if not (JyroExecutionOptions.IsAcceptedRegexTime time) then
            raise (ArgumentOutOfRangeException(
                    nameof time, box time,
                    "A regex timeout is more than zero and at most 24 hours."))
        regexTimeout <- time
        this

    /// <summary>
    /// Sets a cancellation token for cooperative cancellation of every later
    /// <c>Execute()</c>. Changes this runner and returns it. When combined with
    /// execution options, the token is linked with the limiter's time-based
    /// cancellation. Custom functions can observe ctx.CancellationToken to
    /// cancel blocking operations such as web requests.
    ///
    /// Execution only. Compilation is bounded by
    /// <c>JyroCompiler.WithMaxCompilationTime</c> instead, because the parser has
    /// no cooperative checkpoint at which a token could be observed.
    /// </summary>
    member this.WithCancellationToken(token: CancellationToken) =
        cancellationToken <- Some token
        this

    /// <summary>
    /// Sets a pipeline stats collector, to record the Execute stage and the
    /// resource counters of every later run. Changes this runner and returns
    /// it. Compile-stage timings come from the collector given to
    /// <c>JyroCompiler.WithStats</c> or <c>JyroArtifactLoader.WithStats</c>; the same
    /// instance can be passed to both to get the whole pipeline in one place.
    /// </summary>
    member this.WithStats(stats: JyroPipelineStats) =
        requireNotNull (box stats) (nameof stats)
        statsCollector <- Some stats
        this

    /// <summary>
    /// Sets an observer invoked after every host-function call returns,
    /// carrying the function name and result. Changes this runner and returns
    /// it.
    /// </summary>
    member this.WithFunctionResultObserver(observer: Action<string, JyroValue>) =
        requireNotNull (box observer) (nameof observer)
        functionResultObserver <- observer
        this

    /// <summary>
    /// Sets the host state of every later <c>Execute()</c>: the object that
    /// <c>JyroExecutionContext.HostState</c> gives every host function of the run,
    /// the same object by reference. Changes this runner
    /// and returns it. The one called last gives it; null, or no call, is no
    /// state. The engine never reads, changes or keeps <c>state</c> beyond this
    /// runner, so two runners of one program can carry two tenants on two
    /// threads at once.
    /// </summary>
    member this.WithHostState(state: obj | null) =
        hostState <- state
        this

    // The execution context of one run, with its own limiter.
    member private _.CreateExecutionContext() : JyroExecutionContext =
        // Always install a resource limiter. When the host hasn't supplied
        // explicit options, fall back to `JyroExecutionOptions.Default` so
        // scripts run with sensible bounds out of the box (MaxCallDepth=64
        // etc.). Hosts that truly want no limits must set
        // `JyroExecutionOptions.Unlimited` themselves.
        let opts = currentOptions ()
        let limiter =
            match cancellationToken with
            | Some token -> JyroResourceLimiter(opts, token)
            | None -> JyroResourceLimiter(opts)
        let ctx = JyroExecutionContext(limiter)
        ctx.FunctionResultObserver <- functionResultObserver
        ctx.RegexTimeout <- regexTimeout
        ctx.HostState <- hostState
        match seededOutput with
        | Some value ->
            ctx.Output <- value
            ctx.CopyOutputSeed <- true
        | None -> ()
        ctx

    // The input of this run: a stream given since the last run is read now,
    // once, and its value kept for the runs after it.
    member private _.ResolveInput() : RunnerInput =
        match input with
        | InputStream stream ->
            let resolved = JyroRunner.Convert(fun () -> JyroValue.FromJson(stream))
            input <- resolved
            resolved
        | other -> other

    /// <summary>
    /// Runs the program with what was set on this runner, and returns its
    /// result: <c>Output</c> as the value, the diagnostics, and the exit message.
    /// </summary>
    member this.Execute() : JyroExecutionResult =
        match this.ResolveInput() with
        // A fault in the data comes first: running a script against input
        // that could not be read would report whatever the script made of
        // `null`, which is a second wrong answer on top of the first.
        | InputFault fault -> JyroExecutionResult.Of(JyroResult<JyroValue>.Failure(fault))
        | InputStream _ ->
            JyroExecutionResult.Of(JyroResult<JyroValue>.Failure(DiagnosticMessage.Of(MessageCode.InternalStageFailure)))
        | InputValue value ->
            let ctx = this.CreateExecutionContext()
            let result =
                match statsCollector with
                | Some s -> Compiler.executeTimed program value ctx s
                | None -> Compiler.execute program value ctx
            // Surface limiter counters into stats so hosts that opted in via
            // WithStats() can read relevant telemetry post-execution. The
            // limiter is always installed (see CreateExecutionContext), so the
            // values are well-defined even when the script aborted on a limit.
            match statsCollector with
            | Some s when ctx.HasLimiter ->
                let lim = ctx.Limiter
                s.StatementCount <- lim.StatementCount
                s.LoopIterations <- lim.LoopIterations
                // Call depth is tracked on the context (kept off the limiter's
                // hot path); read its high-water mark, since the live counter
                // has unwound to 0 by the time execution returns.
                s.PeakCallDepth <- ctx.PeakCallDepth
                s.LimiterElapsedTime <- lim.ElapsedTime
            | _ -> ()
            result

    /// <summary>
    /// Runs the program and returns <c>Output</c>, throwing a <c>JyroException</c>
    /// that holds the diagnostics when the run fails, so a host can tell a
    /// resource-limit breach from a runtime fault without parsing English.
    /// </summary>
    member this.ExecuteOrThrow() : JyroValue =
        this.Execute().GetValueOrThrow()

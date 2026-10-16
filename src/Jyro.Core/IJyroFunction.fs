namespace RealTree.Jyro

open System.Collections.Generic
open System.Threading

/// <summary>
/// The type a parameter or a return value declares.
///
/// Declared, not enforced. Nothing reads these: the linker checks arity
/// (<c>MinArgs</c>/<c>MaxArgs</c>) and parameter names for named-argument calls, and
/// argument types are checked at run time by the readers of <c>JyroArguments</c>,
/// which ignore the declared type. A host that declares <c>ParameterType.Number</c>
/// and skips its own validation has not been type-checked.
///
/// The values are accurate, and they are the machine-readable record of what
/// a function accepts and returns, for a host or a tool that lists functions:
/// <c>JyroValueType</c> describes runtime values rather than declarations.
///
/// An enumeration whose members have the names and the
/// values of <c>JyroValueType</c>, so a host can cast between the two, and <c>Any</c>
/// after them, which <c>JyroValueType</c> has no use for. Being an enumeration, it
/// is equal by value, with no structural interface, and its
/// <c>ToString</c> is the member's name.
/// </summary>
type ParameterType =
    /// <summary>
    /// The value <c>null</c>.
    /// </summary>
    | Null = 0
    /// <summary>
    /// A boolean.
    /// </summary>
    | Boolean = 1
    /// <summary>
    /// A number.
    /// </summary>
    | Number = 2
    /// <summary>
    /// A string.
    /// </summary>
    | String = 3
    /// <summary>
    /// An array.
    /// </summary>
    | Array = 4
    /// <summary>
    /// An object.
    /// </summary>
    | Object = 5
    /// <summary>
    /// A function value.
    /// </summary>
    | Function = 6
    /// <summary>
    /// A value of any type.
    /// </summary>
    | Any = 7

module internal ParameterTypeCheck =
    // Refuses a parameter type that is none of the eight members, naming
    // `paramName` (JYRO-REQ-2176): an enumeration takes any integer a host
    // casts to it, and a signature never declares a type no host or tool can
    // name.
    let require (value: ParameterType) (paramName: string) =
        if value < ParameterType.Null || value > ParameterType.Any then
            raise (System.ArgumentOutOfRangeException(
                    paramName, box value,
                    "A parameter type is one of the eight members of ParameterType."))

/// <summary>
/// One parameter of a function signature: its name, its declared type and
/// whether a call may leave it out. Immutable, and made only by <c>Required</c> and
/// <c>Optional</c>.
/// </summary>
[<Sealed>]
type Parameter private (name: string, parameterType: ParameterType, isOptional: bool) =
    /// <summary>
    /// Creates a required parameter with the given name and type. A type that
    /// is none of the members of <c>ParameterType</c> throws
    /// <c>ArgumentOutOfRangeException</c> naming <c>type</c>.
    /// </summary>
    static member Required(name: string, ``type``: ParameterType) : Parameter =
        ParameterTypeCheck.require ``type`` "type"
        Parameter(name, ``type``, false)

    /// <summary>
    /// Creates an optional parameter with the given name and type. A type that
    /// is none of the members of <c>ParameterType</c> throws
    /// <c>ArgumentOutOfRangeException</c> naming <c>type</c>.
    /// </summary>
    static member Optional(name: string, ``type``: ParameterType) : Parameter =
        ParameterTypeCheck.require ``type`` "type"
        Parameter(name, ``type``, true)

    /// <summary>
    /// The parameter's name, against which the linker resolves the named
    /// arguments of a call.
    /// </summary>
    member _.Name : string = name

    /// <summary>
    /// The parameter's declared type; see <c>ParameterType</c> - declared, not
    /// enforced.
    /// </summary>
    member _.Type : ParameterType = parameterType

    /// <summary>
    /// Whether this parameter may be omitted from a call.
    /// </summary>
    member _.IsOptional : bool = isOptional

    /// <summary>
    /// Whether <c>other</c> is a <c>Parameter</c> with the same name (ordinal), type
    /// and optionality. A null <c>other</c> is not equal.
    /// </summary>
    override this.Equals(other: obj | null) : bool =
        match other with
        | :? Parameter as p ->
            System.String.Equals(this.Name, p.Name, System.StringComparison.Ordinal)
            && this.Type = p.Type
            && this.IsOptional = p.IsOptional
        | _ -> false

    override this.GetHashCode() : int =
        // `Name` is null when a host passed null to `Required` or `Optional`.
        let name = if isNull (box this.Name) then 0 else System.StringComparer.Ordinal.GetHashCode(this.Name)
        System.HashCode.Combine(name, int this.Type, this.IsOptional)

    /// <summary>
    /// One line: the name, with a backslash written as two and each control
    /// character, U+2028 and U+2029 written as <c>\u</c> and the four lowercase
    /// hexadecimal digits of its code point; then the type, and
    /// <c>optional</c> when it is.
    /// </summary>
    override this.ToString() : string =
        OneLine.escape this.Name + " " + this.Type.ToString() + (if this.IsOptional then " optional" else "")

/// <summary>
/// A function's declared shape: its parameters, its return type, and how
/// many arguments it accepts.
///
/// Arity is enforced - the linker rejects a call outside <c>MinArgs</c>..<c>MaxArgs</c>,
/// and resolves named arguments against <c>Parameter.Name</c>. The types are
/// declared and not enforced; see <c>ParameterType</c>.
///
/// Immutable, and made only by <c>Create</c>. Each fact is
/// stated once: the arity is derived from the parameters, so the two cannot
/// disagree, and the name belongs to the function, not to its signature. The
/// parameters are copied when the signature is made, so changing the array or
/// the sequence given to <c>Create</c> changes no signature.
///
/// Equal when the parameters, position by position, and the return type are
/// equal, with a one-line <c>ToString</c>.
/// </summary>
[<Sealed>]
type JyroFunctionSignature private (parameters: Parameter[], returnType: ParameterType) =
    do
        for p in parameters do
            if isNull (box p) then nullArg "parameters"
    let view = System.Array.AsReadOnly parameters
    let minArgs = parameters |> Array.filter (fun p -> not p.IsOptional) |> Array.length

    /// <summary>
    /// A signature with the return type and the parameters given, in order.
    /// A return type that is none of the members of <c>ParameterType</c> throws
    /// <c>ArgumentOutOfRangeException</c> naming <c>returnType</c>.
    /// </summary>
    static member Create(returnType: ParameterType, [<System.ParamArray>] parameters: Parameter[]) : JyroFunctionSignature =
        ParameterTypeCheck.require returnType (nameof returnType)
        if isNull (box parameters) then nullArg "parameters"
        JyroFunctionSignature(Array.copy parameters, returnType)

    /// <summary>
    /// A signature with the parameters given, in order, and the return type.
    /// A return type that is none of the members of <c>ParameterType</c> throws
    /// <c>ArgumentOutOfRangeException</c> naming <c>returnType</c>.
    /// </summary>
    static member Create(parameters: IEnumerable<Parameter>, returnType: ParameterType) : JyroFunctionSignature =
        ParameterTypeCheck.require returnType (nameof returnType)
        if isNull (box parameters) then nullArg "parameters"
        JyroFunctionSignature(Seq.toArray parameters, returnType)

    /// <summary>
    /// The parameters in declaration order. Read-only, and unchanged after
    /// <c>Create</c> returns.
    /// </summary>
    member _.Parameters : IReadOnlyList<Parameter> = view

    /// <summary>
    /// The function's declared return type; see <c>ParameterType</c>.
    /// </summary>
    member _.ReturnType : ParameterType = returnType

    /// <summary>
    /// The fewest arguments a call to this function may supply: the number of
    /// parameters that are not optional. Enforced by the linker.
    /// </summary>
    member _.MinArgs : int = minArgs

    /// <summary>
    /// The most arguments a call to this function may supply: the number of
    /// parameters. Enforced by the linker.
    /// </summary>
    member _.MaxArgs : int = parameters.Length

    /// <summary>
    /// Whether <c>other</c> is a <c>JyroFunctionSignature</c> with the same parameters,
    /// position by position, and the same return type; the arity follows
    /// from the parameters. A null <c>other</c> is not equal.
    /// </summary>
    override this.Equals(other: obj | null) : bool =
        match other with
        | :? JyroFunctionSignature as s ->
            let mine = this.Parameters
            let theirs = s.Parameters
            this.ReturnType = s.ReturnType
            && mine.Count = theirs.Count
            && Seq.forall2 (fun (a: Parameter) (b: Parameter) -> a.Equals(b)) mine theirs
        | _ -> false

    override this.GetHashCode() : int =
        System.HashCode.Combine(int this.ReturnType, this.Parameters.Count, this.MinArgs)

    /// <summary>
    /// One line: the parameters in parentheses, the return type and the
    /// arity.
    /// </summary>
    override this.ToString() : string =
        let ps = this.Parameters |> Seq.map (fun p -> p.ToString())
        System.String.Format(
            System.Globalization.CultureInfo.InvariantCulture,
            "({0}) {1}, {2} to {3} arguments",
            System.String.Join(", ", ps), this.ReturnType.ToString(), this.MinArgs, this.MaxArgs)

// The place of a call now running, with the name of the function it calls,
// stored on the context in place of a bare `SourceLocation` (JYRO-ADR-108).
// An exception that is not a Jyro diagnostic and escapes while a call is the
// innermost node running becomes a `RuntimeError` whose one argument is that
// name with `()` (spec 130.6.2); the name is all it carries, never the
// exception's type or message.
[<Sealed; AllowNullLiteral>]
type internal RunningCall(location: obj | null, name: string) =
    // The call's place, a boxed `SourceLocation`, or null when it has none.
    member _.Location : obj | null = location
    // The name of the function called, as the call writes it.
    member _.Name : string = name
    // A call with no position of its own, at the place of the node that
    // encloses it (`enclosing`, as `RunningLocation` holds it).
    static member Within(enclosing: obj | null, name: string) : obj =
        let location =
            match enclosing with
            | :? RunningCall as call -> call.Location
            | other -> other
        nonNull (box (RunningCall(location, name)))

/// <summary>
/// Interface for Jyro functions (stdlib and host functions).
///
/// Thread safety: a single function instance is shared across executions - the
/// compiler bakes it into the compiled program and reuses it for every call -
/// and a compiled program may be executed concurrently from multiple threads
/// (the recommended per-request server pattern). Execution within a single run
/// is single-threaded, and every per-invocation value is carried on the
/// JyroExecutionContext passed to Execute, which is created fresh per run.
/// Implementations must therefore hold no mutable instance state, or guard any
/// they hold with their own synchronization; keep per-call state on the context,
/// never on the instance. Most functions are naturally stateless; only a
/// Mutating function (see FunctionEffect - one that "reads or writes shared
/// state") needs internal synchronization.
///
/// A function with side effects weakens the sandbox in proportion to what it
/// does: a script reaches nothing outside its input and output
/// except through the functions a host gives it, so a function that reads a
/// file, calls a service or changes shared state hands that power to every
/// script that can call it. Give it the narrowest inputs it can work with,
/// declare its true <c>Effect</c>, and cap it with <c>JyroCompiler.WithMaxEffect</c>.
/// </summary>
type IJyroFunction =
    /// <summary>
    /// The name scripts use to call this function.
    /// </summary>
    abstract member Name: string
    /// <summary>
    /// The function's declared signature: parameters, arity, and return
    /// type.
    /// </summary>
    abstract member Signature: JyroFunctionSignature
    /// <summary>
    /// The function's observable effect; see <c>FunctionEffect</c>.
    /// </summary>
    abstract member Effect: FunctionEffect
    /// <summary>
    /// Invokes the function with the given arguments and execution context,
    /// returning its result.
    /// </summary>
    abstract member Execute: args: IReadOnlyList<JyroValue> * ctx: JyroExecutionContext -> JyroValue

/// <summary>
/// The context a host function is given: the run it is called in, and only
/// what a function may use of it.
///
/// Its public members are the parameterless constructor, which makes a
/// context outside any run so that a host can call its own function, through
/// <c>IJyroFunction.Execute</c>, in a unit test; the cancellation signal and whether the host requested it; the check
/// of the time limit and the cancellation; <c>Input</c> and <c>Output</c>, read-only;
/// the call depth and its peak; and the run's <c>HostState</c>. Every hook that
/// compiled code calls is internal, bound by the compiler with the non-public
/// binding flags, so a host function cannot end the call depth or the time
/// limit of the run it is called in.
/// </summary>
and [<Sealed>] JyroExecutionContext internal (limiter: JyroResourceLimiter, cancellationToken: CancellationToken) =
    let limiterInstance = limiter
    let externalCt = cancellationToken
    let mutable returnMessage: string | null = null
    // The run's input (spec 70.2), set by the run before its first statement.
    let mutable input: JyroValue = JyroNull.Instance
    // The value the runner was given by `WithHostState`, handed to every host
    // function of the run and never read here (JYRO-ADR-201 item 6).
    let mutable hostState: obj | null = null
    let mutable functionResultObserver: System.Action<string, JyroValue> | null = null
    // `Output`, and whether it has been given a value: before a run, a value
    // the host supplied, which the run starts from (spec 70.3.1).
    let mutable output: JyroValue = JyroNull.Instance
    let mutable outputSupplied = false

    // Call-depth tracking is held directly on the context (not delegated to
    // the limiter) so that `CheckAndEnterCall` - emitted on every function
    // invocation by the compiler - is as short as possible. Every CLR
    // frame we save per Jyro call buys room under the .NET stack ceiling.
    // The call-depth bound is NOT optional, so a context built without a
    // limiter still gets one. Every other limit is recoverable - exceeding it
    // raises a `JyroRuntimeException` the host catches as a diagnostic - but
    // exceeding the CLR stack is a `StackOverflowException`, which .NET makes
    // uncatchable and which kills the host process. A limiter-free context is
    // therefore a context that opts out of resource *policy*, never one that
    // opts out of stack safety. `JyroExecutionOptions.Unlimited` holds the
    // same line for the same reason.
    let mutable callDepth = 0
    let mutable peakCallDepth = 0

    // The place of the node now running (spec 130.1.2), as a boxed
    // `SourceLocation`, or null before the first statement (JYRO-ADR-108).
    // Compiled code stores it at every statement, and at every call,
    // operator and access before it does its own work, and `execute` reads
    // it when an exception reaches the top, to locate the diagnostic at the
    // node whose evaluation raised it. A box is one reference store, as the
    // packed line and column it replaces were one 64-bit store, and it holds
    // the length as well. It replaces a try/catch around every statement
    // whose handlers threw or rethrew: on Windows each of those started a
    // nested exception dispatch above the frames not yet unwound, so an
    // error raised deep in nested blocks overflowed a 1 MB stack.
    let mutable runningLocation : obj | null = null
    // The regex timeout of this run (spec 90.10.3), which a host sets with
    // `JyroRunner.WithMaxRegexTime` (160.1.10); 1 second when it sets none
    // (90.10.4). On the context because it belongs to one execution, and the
    // regular expression functions are handed the context and nothing else.
    let mutable regexTimeout = System.TimeSpan.FromSeconds(1.0)
    let maxCallDepth =
        if isNull (box limiter) then JyroExecutionOptions.MaxSafeCallDepth
        else limiter.Options.MaxCallDepth

    /// <summary>
    /// A context outside any run, for a host that calls its own function
    /// through <c>IJyroFunction.Execute</c> in a unit test: no time, statement or loop limit, a call
    /// depth bounded at <c>JyroExecutionOptions.MaxSafeCallDepth</c>, a call depth
    /// of 0, <c>Input</c> and <c>HostState</c> null.
    /// </summary>
    new() = JyroExecutionContext(Unchecked.defaultof<JyroResourceLimiter>, CancellationToken.None)
    internal new(limiter: JyroResourceLimiter) = JyroExecutionContext(limiter, CancellationToken.None)

    // The resource limiter governing this execution, or null when the context
    // was built without one.
    member internal _.Limiter : JyroResourceLimiter = limiterInstance
    // Whether this context was constructed with a resource limiter. When
    // false, `Limiter` is null and the resource-limit checks below are
    // no-ops.
    member internal _.HasLimiter = not (isNull (box limiterInstance))
    /// <summary>
    /// The current call depth: how many Jyro function calls are presently
    /// nested. It goes up by one as a call starts and down by one as it
    /// returns; see <c>PeakCallDepth</c> for the run's high-water mark.
    /// </summary>
    member _.CallDepth = callDepth
    /// <summary>
    /// High-water mark of call depth reached during this execution. Unlike
    /// CallDepth - which returns to 0 as calls unwind - this retains the peak
    /// so post-execution telemetry can report the deepest recursion reached.
    /// </summary>
    member _.PeakCallDepth = peakCallDepth

    /// <summary>
    /// Cancellation token for cooperative cancellation of blocking operations.
    /// When a resource limiter is configured, this token cancels itself when
    /// the run reaches MaxExecutionTime, from the engine's own threads rather
    /// than a thread-pool timer, so it fires on time even when the host's
    /// thread pool is busy. Callbacks registered on it run, at
    /// the limit, on an engine canceller thread of their own: one that blocks
    /// holds that thread until it returns but delays no other run's limit. A host can also supply a
    /// token of its own with <c>JyroRunner.WithCancellationToken</c>; canceling it cancels this token on
    /// the canceling thread.
    /// </summary>
    member _.CancellationToken =
        if isNull (box limiterInstance) then externalCt else limiterInstance.CancellationToken

    /// <summary>
    /// Whether the host has canceled this run, as distinct from the run
    /// having exhausted its time budget. <c>CancellationToken</c> above cannot
    /// answer that: the limiter links the host's token with its own deadline
    /// at MaxExecutionTime, so it fires for either cause. A
    /// function that blocks on the token and catches the resulting
    /// <c>OperationCanceledException</c> reads this to decide which of the two it
    /// is reporting.
    /// </summary>
    member _.HostCancellationRequested =
        externalCt.IsCancellationRequested
        || (not (isNull (box limiterInstance)) && limiterInstance.HostCancellationRequested)

    /// <summary>
    /// The run's input value, bound to the script's <c>Input</c> identifier, and
    /// read-only. Set by the run before its first statement.
    /// </summary>
    member _.Input
        with get () : JyroValue = input
        and internal set (value: JyroValue) =
            input <- (if obj.ReferenceEquals(value, null) then JyroNull.Instance :> JyroValue else value)

    /// <summary>
    /// The run's output value, bound to the script's <c>Output</c> identifier.
    /// Starts as an empty object (or the value the runner was given) and is
    /// read by the host from the result. Only the run sets it: a value set
    /// before the run is the first value of <c>Output</c>, <c>null</c> included; a CLR null is read as <c>null</c>.
    /// </summary>
    member _.Output
        with get () : JyroValue = output
        and internal set (value: JyroValue) =
            output <- (if obj.ReferenceEquals(value, null) then JyroNull.Instance :> JyroValue else value)
            outputSupplied <- true

    /// <summary>
    /// The value the runner of this run was given by
    /// <c>JyroRunner.WithHostState</c>, the same object by reference, or <c>null</c>
    /// when it was given none. A host puts per-run state
    /// here - a tenant, a service provider - for its functions to read; the
    /// engine never reads, changes or keeps it.
    /// </summary>
    member _.HostState
        with get () : obj | null = hostState
        and internal set (value: obj | null) = hostState <- value

    // Whether `Output` has been given a value, so a run starts from it and
    // not from the empty object: a host-supplied `null` is a first value
    // like any other (spec 70.3.1, JYRO-REQ-273).
    member internal _.OutputSupplied = outputSupplied

    // Whether the run starts `Output` from a copy of the value it was given,
    // so that no run changes the value a runner holds (JYRO-ADR-201 item 7).
    member val internal CopyOutputSeed = false with get, set

    // `AddMessage` and `Messages` used to sit here, over a `ResizeArray` this
    // type allocated per run. Nothing ever read them: `Compiler.execute` builds
    // its result from the return value or the caught exception and never
    // consults the context, so a host function calling `AddMessage` had its
    // diagnostic silently discarded - the same defect as the registries on the
    // late `JyroHostContext`. A host that wants to add a warning to a run has
    // no route today; giving it one means merging the list into the result in
    // `execute`, which is a feature to add deliberately rather than a member to
    // leave lying around looking like it works.

    // Message set by an exit or fail statement, or null when neither ran;
    // the run's result reads it as its `ExitMessage`.
    member internal _.ReturnMessage
        with get() : string | null = returnMessage
        and set(v: string | null) = returnMessage <- v

    // Called from compiled expression trees to store the exit/fail message
    member internal _.SetReturnMessage(msg: string) = returnMessage <- msg

    // -- Resource-limit hooks (called from compiled expression trees) --------

    // Called from compiled expression trees at each statement boundary.
    // Counts against the run's statement-count budget, raising
    // `StatementLimitExceeded` if exceeded, then applies the same checks as
    // `CheckExecutionLimits`. No-op when the context has no limiter.
    member internal _.CheckAndCountStatement() =
        if not (isNull (box limiterInstance)) then limiterInstance.CheckAndCountStatement()

    // Called from compiled expression trees on each loop iteration. Counts
    // against the run's cumulative loop-iteration budget (shared across all
    // loops), raising `LoopIterationLimitExceeded` if exceeded, then applies
    // the same checks as `CheckExecutionLimits`. No-op when the context has
    // no limiter.
    member internal _.CheckAndEnterLoop() =
        if not (isNull (box limiterInstance)) then limiterInstance.CheckAndEnterLoop()

    /// <summary>
    /// Checks the wall-clock execution-time limit and host cancellation
    /// without counting against the statement or loop budgets. A function
    /// that iterates over script-controlled data calls this periodically, so
    /// that the execution-time limit and host cancellation are honored during
    /// a long operation, which contains no statement boundary of its own.
    /// </summary>
    member _.CheckExecutionLimits() =
        if not (isNull (box limiterInstance)) then limiterInstance.CheckExecutionLimits()
        if externalCt.IsCancellationRequested then
            JyroError.raiseRuntime MessageCode.CanceledByHost [||]

    // Called immediately before a Jyro function call. Increments the
    // context-local depth counter and raises `CallDepthLimitExceeded`
    // if the configured limit is tripped. Deliberately does NOT call
    // `CheckExecutionTime` - time is checked at every statement via
    // `CheckAndCountStatement`, which bounds how far we can drift.
    member internal _.CheckAndEnterCall() =
        callDepth <- callDepth + 1
        if callDepth > peakCallDepth then peakCallDepth <- callDepth
        if callDepth > maxCallDepth then
            JyroError.raiseRuntime
                MessageCode.CallDepthLimitExceeded
                [| box maxCallDepth |]

    // Called immediately after a Jyro function call returns. Decrements the
    // call-depth counter that `CheckAndEnterCall` incremented.
    member internal _.ExitCall() = callDepth <- callDepth - 1

    // -- Statement position (called from compiled expression trees) ----------

    // The place of the node now running, a boxed `SourceLocation`, or null
    // when none has run. A call, an operator or an access reads it before it
    // marks its own and gives it back with `MarkLocation` after, so the
    // expression that goes on after it is located at itself, not at the
    // node that ran last.
    member internal _.RunningLocation : obj | null = runningLocation

    // Records the place of the node about to run. No catch handler is
    // involved: an exception unwinds in one dispatch, whatever the depth,
    // and `execute` takes the location from here (JYRO-ADR-108).
    member internal _.MarkLocation(location: obj | null) = runningLocation <- location

    // The regex timeout of this run (spec 90.10.3, 90.10.4).
    member internal _.RegexTimeout
        with get() = regexTimeout
        and set(v: System.TimeSpan) = regexTimeout <- v

    // The place of the node now running, or none when no statement with a
    // position has run.
    member internal _.CurrentLocation : SourceLocation option =
        match runningLocation with
        | :? SourceLocation as location -> Some location
        | :? RunningCall as call ->
            match call.Location with
            | :? SourceLocation as location -> Some location
            | _ -> None
        | _ -> None

    // The name of the function whose call is the innermost node running,
    // or null when that node is not a call (spec 130.6.2: the argument of
    // a `RuntimeError`).
    member internal _.CurrentFunction : string | null =
        match runningLocation with
        | :? RunningCall as call -> call.Name
        | _ -> null

    // Optional host-supplied observer invoked from compiled expression trees
    // immediately after a host-function call returns, carrying the function
    // name and its returned value. Lets a host watch results (e.g. detect
    // error envelopes) without modifying individual functions. No-op when
    // unset. Set per-execution from the builder via CreateExecutionContext.
    member internal _.FunctionResultObserver
        with get () : System.Action<string, JyroValue> | null = functionResultObserver
        and set (value: System.Action<string, JyroValue> | null) = functionResultObserver <- value

    // Called from compiled expression trees after a host-function call returns.
    member internal _.OnFunctionResult(name: string, result: JyroValue) =
        match functionResultObserver with
        | null -> ()
        | obs -> obs.Invoke(name, result)

namespace RealTree.Jyro

open System

// The message of a `JyroRuntimeException`: the message of a diagnostic of
// the same code and arguments, made by the default templates (spec 130.6.1,
// JYRO-ADR-129). One definition, so the text a host catches cannot drift
// from the text the diagnostic reports.
module internal RuntimeExceptionText =
    let argsOf (args: (obj | null)[]) : (obj | null)[] =
        if obj.ReferenceEquals(args, null) then Array.empty else args

    let messageOf (code: MessageCode) (args: (obj | null)[]) : string =
        (DiagnosticMessage.Of(code, argsOf args)).Message

    // The one diagnostic a `JyroRuntimeException` holds: its code, its
    // arguments and its place (JYRO-REQ-1886).
    let diagnosticOf (code: MessageCode) (args: (obj | null)[]) (location: Nullable<SourceLocation>) : DiagnosticMessage[] =
        [| DiagnosticMessage.Of(code, argsOf args, location) |]

// Who made a `JyroRuntimeException`, which decides what it gives when a
// host function or a call check throws it (JYRO-ADR-192, JYRO-ADR-131).
type internal RuntimeExceptionOrigin =
    // The engine raised it. Thrown inside a call check, it is a crash of
    // the check, not a refusal of the call.
    | Engine = 0
    // `JyroError.Runtime` made it: it keeps its code wherever it is thrown.
    | HostRuntime = 1
    // `JyroError.Host` made it: `HostFunctionError` from a host function,
    // `HostLinkError` from a call check.
    | Host = 2

/// <summary>
/// Exception thrown during Jyro script execution with a diagnostic code and
/// structured arguments: a <c>JyroException</c> with exactly one diagnostic, of
/// its <c>Code</c> and <c>Args</c>, placed at its <c>Location</c>.
///
/// A host does not construct it: it makes one with
/// <c>JyroError.Host</c> or <c>JyroError.Runtime</c> and throws it, and catches this
/// type or <c>JyroException</c>. Its <c>Message</c> is made from <c>Code</c> and <c>Args</c>,
/// never given, so it is always the text of a diagnostic of that code and
/// those arguments.
/// </summary>
[<Sealed>]
type JyroRuntimeException internal (code: MessageCode, args: (obj | null)[], location: Nullable<SourceLocation>) =
    inherit JyroException(
        RuntimeExceptionText.messageOf code args,
        Array.AsReadOnly(RuntimeExceptionText.diagnosticOf code args location))
    let args = RuntimeExceptionText.argsOf args
    let mutable origin = RuntimeExceptionOrigin.Engine

    // Creates the exception with structured arguments but no source location.
    internal new(code: MessageCode, args: (obj | null)[]) = JyroRuntimeException(code, args, Nullable())

    // Who made this exception (JYRO-ADR-131, JYRO-ADR-192). Only the
    // members of `JyroError` set it; the engine's curried
    // `JyroError.raiseRuntime` and every direct construction leave it
    // `Engine`. The linker reads it to tell a call check's deliberate
    // `JyroError.Runtime`, which keeps its code, from an exception an engine
    // member threw inside the check, which is a crash of the check and gives
    // `UnknownCompilerError`. An engine site that switched to the public
    // member would therefore change behavior: the engine MUST keep raising
    // through `raiseRuntime`.
    member internal _.Origin
        with get () = origin
        and set (value: RuntimeExceptionOrigin) = origin <- value

    /// <summary>
    /// The diagnostic code identifying the kind of error.
    /// </summary>
    member _.Code : MessageCode = code

    /// <summary>
    /// The structured arguments substituted into the message template.
    /// </summary>
    member _.Args : (obj | null)[] = args

// Control-flow signal raised by `exit` from inside a function, where the
// return has to escape the whole call stack rather than fall to a label.
//
// It is deliberately *not* a `JyroRuntimeException`, and not public
// (JYRO-ADR-198): it is the engine's signal, never one a host throws or
// catches. Reaching the top of `execute` means the script ended cleanly. Any
// statement wrapper that catches broadly MUST let this through untouched -
// the general handler turns an escaping exception into `RuntimeError`, which
// would report a successful `exit` as a crash.
type internal JyroScriptExit() =
    inherit Exception()

// Raised when a member of the engine that compiled code calls, or that
// converts host numbers, is missing or ambiguous (JYRO-ADR-206): its message
// names the declaring type and the member. It is not a diagnostic: no
// compilation can succeed in a build that raises it, so it passes out of
// every compilation as it is.
type internal EngineBindingException(message: string, inner: exn | null) =
    inherit Exception(message, inner)
    new(message: string) = EngineBindingException(message, null)

    // The EngineBindingException at the bottom of `ex`'s chain of inner
    // exceptions, when there is one: a type initializer that fails wraps the
    // exception it raised in a TypeInitializationException, whose own message
    // names only the class being initialized.
    static member Within(ex: exn) : EngineBindingException option =
        let rec find (e: exn | null) =
            match e with
            | null -> None
            | :? EngineBindingException as binding -> Some binding
            | e -> find e.InnerException
        find ex

// Whether the code running on this thread is a host function's call check
// (`IJyroCallCheck.CheckCall`), which the linker calls at link time
// (JYRO-ADR-130). An exception made by `JyroError.Host` and thrown there
// refuses the call with `HostLinkError`, not `HostFunctionError`: one door
// serves both stages, and the stage follows from the code.
//
// A run that a check starts on this thread is not a call check: a
// `JyroError.Host` thrown in it refuses nothing the linker is checking, so it
// gives `HostFunctionError` (spec 160.1.12). Each run therefore clears the
// scope at entry and restores it at exit, as it does `ExecutionCheck`. A
// compilation that a check starts is a call check of its own, and runs its
// checks under `Run` again.
type internal CallCheckScope private () =
    [<ThreadStatic; DefaultValue>]
    static val mutable private depth: int

    // Whether a call check is running on this thread.
    static member Active = CallCheckScope.depth > 0

    // Runs `check` as a call check.
    static member Run(check: unit -> unit) =
        CallCheckScope.depth <- CallCheckScope.depth + 1
        try check ()
        finally CallCheckScope.depth <- CallCheckScope.depth - 1

    // Leaves any call check on this thread for a run, and returns the depth
    // it replaced, which the caller MUST hand to `Restore` in a `finally`.
    static member Clear() : int =
        let previous = CallCheckScope.depth
        CallCheckScope.depth <- 0
        previous

    // Puts back the depth that `Clear` replaced.
    static member Restore(previous: int) : unit =
        CallCheckScope.depth <- previous

// The curried forms the five assemblies call, behind `JyroError` below.
//
// A host does not see this module: it is internal, and F# gives it the
// compiled name `JyroErrorModule` because the public type has its name. F#
// code resolves `JyroError.raiseRuntime` here and `JyroError.Runtime` on the
// type, so the in-tree callers keep their curried form.
module internal JyroError =
    // Raise a JyroRuntimeException using the default English template for the given code.
    let raiseRuntime (code: MessageCode) (args: (obj | null)[]) : 'T =
        // The exception makes its message from the code and the arguments,
        // through `render`, which strips placeholders it has no arguments for
        // rather than throwing `FormatException` out of the raise itself.
        raise (JyroRuntimeException(code, args))

    // The code of an exception made by `JyroError.Host` when it is thrown
    // where it is made: `HostLinkError` in a call check, `HostFunctionError`
    // otherwise (JYRO-ADR-130). Where it is thrown decides in the end: the
    // linker and the run read its origin.
    let hostCode () : MessageCode =
        if CallCheckScope.Active then MessageCode.HostLinkError else MessageCode.HostFunctionError

    // The exception of a deliberate failure of a host function, as a
    // reason and the values that change between occurrences (JYRO-ADR-124):
    // its arguments are `reason` followed by `values`, in order. A null
    // `values` is the empty array.
    let makeHost (reason: string) (values: (obj | null)[] | null) : JyroRuntimeException =
        if isNull reason then raise (ArgumentNullException(nameof reason))
        let values : (obj | null)[] = match values with | null -> Array.empty | values -> values
        let args = Array.append [| box reason |] values
        let ex = JyroRuntimeException(hostCode (), args)
        ex.Origin <- RuntimeExceptionOrigin.Host
        ex

    // The exception `ex` gives where it is caught as `code`, `HostLinkError`
    // or `HostFunctionError`: itself when it has that code already, else a
    // copy of its arguments under `code`.
    let asHostCode (code: MessageCode) (ex: JyroRuntimeException) : JyroRuntimeException =
        if ex.Code = code then ex
        else
            let copy = JyroRuntimeException(code, ex.Args)
            copy.Origin <- RuntimeExceptionOrigin.Host
            copy

/// <summary>
/// Makes the exceptions a host function throws, the only way a host makes a
/// <c>JyroRuntimeException</c>: the exception has no public constructor.
///
/// Each member returns the exception and does not throw it, so a C# host
/// writes <c>throw JyroError.Host(...)</c> in any method, a <c>void</c> one included.
/// </summary>
[<AbstractClass; Sealed>]
type JyroError =
    // The codes that only the engine raises, which `Runtime` refuses
    // (JYRO-ADR-192 item 4, JYRO-REQ-1892): every code below 5000 or from
    // 6000 up, and these of the 5000 band.
    static member private EngineOnly(code: MessageCode) : bool =
        let n = int code
        n < 5000 || n >= 6000
        || code = MessageCode.RuntimeError
        || code = MessageCode.CanceledByHost
        || code = MessageCode.ScriptFailure
        || code = MessageCode.HostFunctionError
        || code = MessageCode.StatementLimitExceeded
        || code = MessageCode.LoopIterationLimitExceeded
        || code = MessageCode.CallDepthLimitExceeded
        || code = MessageCode.ExecutionTimeLimitExceeded
        || code = MessageCode.ValueNestingLimitExceeded
        || code = MessageCode.ValueSizeLimitExceeded

    /// <summary>
    /// A deliberate failure of a host function, named by <c>reason</c>, with the
    /// values that change between occurrences, such as the amount refused.
    ///
    /// Thrown from a host function's <c>Execute</c>, it ends the run with one
    /// <c>HostFunctionError</c> whose arguments are <c>reason</c> followed by <c>values</c>,
    /// in order; thrown from a call check (<c>IJyroCallCheck.CheckCall</c>), it
    /// refuses the call being checked with one <c>HostLinkError</c> of those
    /// arguments, placed at the call. It keeps a
    /// condition the function chose to reject distinguishable from one that
    /// crashed it, which the runtime reports as <c>RuntimeError</c>.
    ///
    /// <c>reason</c> is a name for the condition, such as <c>AmountNotPositive</c>, not
    /// a sentence: a host that wants a sentence makes it in its
    /// own <c>IMessageTemplateProvider</c>. A null array is the empty one.
    /// </summary>
    static member Host(reason: string, [<ParamArray>] values: (obj | null)[] | null) : JyroRuntimeException =
        JyroError.makeHost reason values

    /// <summary>
    /// An exception of <c>code</c> with <c>args</c> substituted into its template, for
    /// the conditions of the argument and conversion codes the standard
    /// library raises. Thrown from a host function or a call check, it gives a
    /// diagnostic of <c>code</c> and <c>args</c>. A null array is the empty one.
    ///
    /// A code that only the engine raises is refused with
    /// <c>ArgumentOutOfRangeException</c> naming <c>code</c>, so host code cannot make
    /// a diagnostic that looks like the engine's: a value that is not a
    /// member of <c>MessageCode</c>, every code below 5000 or from 6000 up,
    /// <c>RuntimeError</c>, <c>CanceledByHost</c>, <c>ScriptFailure</c>, <c>HostFunctionError</c>
    /// (a host raises that through <c>Host</c>), the four resource limits of 5900
    /// to 5903, <c>ValueNestingLimitExceeded</c> and <c>ValueSizeLimitExceeded</c>.
    /// </summary>
    static member Runtime(code: MessageCode, [<ParamArray>] args: (obj | null)[] | null) : JyroRuntimeException =
        if not (Enum.IsDefined(typeof<MessageCode>, code)) || JyroError.EngineOnly code then
            raise (ArgumentOutOfRangeException(
                    nameof code, box code,
                    "A host makes an exception of a code the engine does not keep to itself."))
        let args : (obj | null)[] = match args with | null -> Array.empty | args -> args
        let ex = JyroRuntimeException(code, args)
        ex.Origin <- RuntimeExceptionOrigin.HostRuntime
        ex

namespace RealTree.Jyro

open RealTree.Jyro.Compilation

open System
open System.Collections.Generic

// How every compilation of the host API runs - from source, from an
// artifact, and a library parsed ahead: on a thread with a large stack,
// under the compilation time limit, with whatever ends it other than a
// result turned into a diagnostic.
module internal CompilationRun =

    // The compilation time limit when the host sets none (JYRO-ADR-194): 5
    // seconds, the default execution time limit of spec 100.1.2.
    let DefaultTimeLimit = TimeSpan.FromSeconds(5.0)

    // The message of the `ArgumentOutOfRangeException` of a compilation
    // time limit outside the range.
    [<Literal>]
    let TimeLimitRange =
        "A compilation time limit is TimeSpan.MaxValue (no limit), or more than zero and at most 24 hours."

    // Runs the given work on a worker thread with a 16 MiB stack (vs .NET's
    // default 1 MiB on x64) and enforces a wall-clock deadline.
    //
    // Why the larger stack: the parser and every later stage recurse once
    // per level of nesting, and the recursive FParsec combinators of the
    // parser take a lot of stack for each. The bound is the maximum nesting
    // depth of 256 (spec 100.10, JYRO-ADR-177): the parser counts the depth
    // before it recurses and refuses a deeper source with
    // `SourceNestingLimitExceeded`, and the artifact reader refuses a deeper
    // artifact, so no stage recurses much past it.
    //
    // If the worker exceeds `timeout`, the thread is abandoned (left to
    // finish as a background thread) and the result is `None`. Before that,
    // the abandoned compilation is told to stop through `cancel`, which its
    // stages check (JYRO-ADR-100). An exception the worker threw is raised
    // again in the caller.
    //
    // The deadline is a result, not an exception: a `TimeoutException` is a
    // platform type that host code running in the worker (a call check, or a
    // function's `Name`, `Signature` or `Effect`) can throw too, and that one
    // is an internal compiler error (JYRO-REQ-1679), not the time limit.
    //
    // On .NET on single-threaded browser WebAssembly the work runs on the
    // caller's thread instead (JYRO-DEF-775, JYRO-ADR-183, JYRO-ADR-189).
    // That runtime is single-threaded: starting a thread throws
    // `PlatformNotSupportedException`, which would fail every compilation
    // with `UnknownCompilerError`. There is no larger stack there, and no
    // deadline: with no second thread, nothing can keep time while the work
    // runs, so neither the default limit nor one the host sets is enforced
    // there, and the result is always `Some`.
    let private runOnLargerStack (work: unit -> 'T) (timeout: TimeSpan) (cancel: Threading.CancellationTokenSource) : 'T option =
        if OperatingSystem.IsBrowser() then
            Some (work ())
        else
            let stackSize = 16 * 1024 * 1024
            let mutable result : 'T = Unchecked.defaultof<_>
            let mutable caught : exn | null = null
            let thread =
                Threading.Thread(
                    (fun () ->
                        try result <- work ()
                        with ex -> caught <- ex),
                    stackSize)
            thread.IsBackground <- true
            thread.Start()
            let joined =
                if timeout = TimeSpan.MaxValue then
                    thread.Join(); true
                else
                    thread.Join(timeout)
            if not joined then
                cancel.Cancel()
                None
            else
                match caught with
                | null -> Some result
                | ex -> raise ex

    // Runs a compilation on the larger stack under the time limit, and turns
    // whatever ends it other than a result into a diagnostic: a method whose
    // signature promises a `JyroResult` must not also throw.
    //
    // The compilation publishes each stage it enters to a cell made here
    // (JYRO-ADR-100), so every diagnostic made here has the stage that was
    // running (spec 160.1.12): `CompilationTimeLimitExceeded` the stage the
    // limit ended (100.9.3), and an exception that escaped a stage the stage
    // it escaped. A `JyroRuntimeException` keeps its own code; anything else
    // is an internal compiler error, which is what it is. An engine
    // `JyroRuntimeException` thrown inside a call check reaches here as the
    // linker's `CallCheckCrash`, so it is one of the latter (JYRO-ADR-131).
    let runStaged (first: MessageStage) (work: CompilationStage -> JyroResult<'T>) (timeout: TimeSpan) : JyroResult<'T> =
        // Not disposed: an abandoned worker goes on reading its token after
        // this returns, and a source with no timer and no wait handle holds
        // nothing that disposing would release.
        let cancel = new Threading.CancellationTokenSource()
        let stage = CompilationStage(first, cancel.Token)
        // The cell is also installed as the worker thread's own, so every
        // stage's long loops check it, not only the stages handed it.
        try
            match runOnLargerStack (fun () -> CompilationStage.RunAmbient(stage, fun () -> work stage)) timeout cancel with
            | Some result -> result
            | None ->
                JyroResult<'T>.Failure(
                    DiagnosticMessage.Of(
                        MessageCode.CompilationTimeLimitExceeded,
                        args = [| box (int timeout.TotalMilliseconds) |]).WithStage(stage.Current))
        with
        | :? JyroRuntimeException as ex ->
            JyroResult<'T>.Failure(
                DiagnosticMessage.Of(ex.Code, ex.Args, ex.Location).WithStage(stage.Current))
        // A member of the engine that compiled code calls is missing or
        // ambiguous: a fault of the build, not of the script, which fails every
        // compilation, so it passes out naming the type and the member
        // (JYRO-ADR-206) rather than hiding as UnknownCompilerError.
        | :? EngineBindingException -> reraise ()
        // The same fault met first in a type initializer, which wraps it: the
        // EngineBindingException within passes out in its place.
        | :? TypeInitializationException as ex when (EngineBindingException.Within ex).IsSome ->
            raise (EngineBindingException((EngineBindingException.Within ex).Value.Message, ex))
        | _ ->
            // No argument: the exception's type and message are the host
            // platform's, not names this specification gives (spec 130.6.2).
            JyroResult<'T>.Failure(
                DiagnosticMessage.Of(MessageCode.UnknownCompilerError).WithStage(stage.Current))

// The functions and the maximum effect a compilation is given, shared by
// `JyroCompiler` and `JyroArtifactLoader`, whose members of the same names
// register them with the same outcomes (JYRO-ADR-199 item 2).
[<Sealed>]
type internal FunctionRegistration() =
    // See `IsStdlib`.
    static let stdlibTypes =
        HashSet<Type>(StdlibRegistry.getAll () |> Seq.map (fun f -> f.GetType()))
    let functions = ResizeArray<IJyroFunction>()
    let mutable includeStdlib = false

    // The maximum effect of a script's calls (spec 120.5.1): `Mutating`, no
    // restriction, unless the host sets another.
    member val MaxEffect = FunctionEffect.Mutating with get, set

    // Refuses an effect maximum that is none of the four effects, naming
    // `paramName`, before anything is changed (JYRO-REQ-2176). An enumeration
    // takes any integer a host casts to it, and an unchecked `(FunctionEffect)99`
    // would be a ceiling above `Mutating` that lets every function through.
    static member RequireEffect(maxEffect: FunctionEffect, paramName: string) =
        if maxEffect < FunctionEffect.Pure || maxEffect > FunctionEffect.Mutating then
            raise (ArgumentOutOfRangeException(
                    paramName, box maxEffect,
                    "An effect maximum is one of Pure, NonDeterministic, Blocking and Mutating."))

    // Adds one host function; a null is refused naming `paramName`.
    member _.Add(func: IJyroFunction, paramName: string) =
        if obj.ReferenceEquals(func, null) then raise (ArgumentNullException(paramName))
        functions.Add(func)

    // Adds host functions, enumerating `funcs` exactly once (JYRO-REQ-1935):
    // a null sequence, or one holding a null, is refused naming `paramName`
    // and adds none.
    member _.AddRange(funcs: IEnumerable<IJyroFunction>, paramName: string) =
        if obj.ReferenceEquals(funcs, null) then raise (ArgumentNullException(paramName))
        let added = Seq.toArray funcs
        if added |> Array.exists (fun f -> obj.ReferenceEquals(f, null)) then
            raise (ArgumentNullException(paramName, "The sequence holds a null function."))
        functions.AddRange(added)

    // Includes the standard library.
    member _.IncludeStdlib() = includeStdlib <- true

    // The types of the standard library's functions. A function is a
    // standard library function by its origin (spec 160.2.3): an instance of
    // one of these types, however the host added it - with `WithStdlib()`,
    // or by passing what `StdlibRegistry.GetAll()` returned to `AddFunction`
    // or `AddFunctions`. `GetAll` makes new instances on each call, so the
    // type, not the instance, is what identifies one.
    static member private IsStdlib(func: IJyroFunction) =
        stdlibTypes.Contains(func.GetType())

    // Every function of the compilation: the standard library when it is
    // included, then the host's, in the order added.
    member _.All() : IJyroFunction list =
        let stdlib = if includeStdlib then List.ofSeq (StdlibRegistry.getAll ()) else []
        List.concat [ stdlib; List.ofSeq functions ]

    // Detect host-function registration collisions. Each is reported with
    // the stage its code has - `DuplicateFunction` with Validate,
    // `HostFunctionOverridesStdlib` with Link - once every earlier stage has
    // run clean on every source text, never ahead of the source's own
    // diagnostics (JYRO-ADR-120, JYRO-DEF-512, JYRO-DEF-524). Without
    // this the linker collapses colliding names into a single `Map` entry
    // (last-write-wins), so a host function silently shadows a stdlib
    // function of the same name, or a second host function silently replaces
    // the first, with no diagnostic. Both are rejected as hard errors:
    //   * a host function whose name is the name of a standard library
    //     function the host also added, by `WithStdlib()` or by adding it
    //     itself -> `HostFunctionOverridesStdlib` (spec 160.2.3; there is no
    //     silent override; rename it);
    //   * two host functions registered under the same name ->
    //     `DuplicateFunction`.
    // A name that collides with stdlib is reported once as the stdlib
    // collision (the more actionable message) and not also as a duplicate.
    // A standard library function added twice - by `WithStdlib()` and again
    // by `AddFunctions(StdlibRegistry.GetAll())` - is the same function both
    // times, and is not a collision.
    member _.DetectCollisions() : DiagnosticMessage list =
        let added = List.ofSeq functions
        let stdlibNames =
            let addedStdlib = added |> List.filter FunctionRegistration.IsStdlib |> List.map (fun f -> f.Name)
            let included =
                if includeStdlib then StdlibRegistry.getAll () |> Seq.map (fun f -> f.Name) |> List.ofSeq
                else []
            Set.ofList (included @ addedStdlib)
        let seen = HashSet<string>()
        let reported = HashSet<string>()
        [ for func in added do
            if not (FunctionRegistration.IsStdlib func) then
                let name = func.Name
                if stdlibNames.Contains(name) then
                    if reported.Add(name) then
                        yield DiagnosticMessage.Of(
                            MessageCode.HostFunctionOverridesStdlib,
                            [| box name |])
                elif not (seen.Add(name)) then
                    if reported.Add(name) then
                        yield DiagnosticMessage.Of(
                            MessageCode.DuplicateFunction,
                            [| box name |]) ]

/// Helpers of the chapter 70-90 suite that Jyro.Spec.Common does not have.
/// Written from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.Support

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// The lines of a script, joined with line feeds.
let lines (ls: string list) : string = String.concat "\n" ls

/// The safety bound on every wait in this suite (a test must end, 4.1.x).
let safety = TimeSpan.FromSeconds 10.0

/// Options that allow every effect, for scripts that call `Sleep` or other
/// `Blocking` functions (120.5.1): the host sets the maximum explicitly.
let allEffects : RunOptions =
    { Script.defaults with MaxEffect = Some FunctionEffect.Mutating }

/// A host function `Hold()` (rule 4.1.4) that blocks the execution calling it
/// until the test releases it, so that one execution is held open while another
/// runs. Every wait has a 10 s safety bound.
type Gate(name: string) =
    let entered = new SemaphoreSlim(0)
    let release = new SemaphoreSlim(0)
    let fn =
        Host.func name
            [ for i in 0 .. 3 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]
            ParameterType.Any FunctionEffect.Pure
            (fun _ ->
                entered.Release() |> ignore
                if not (release.Wait safety) then
                    raise (TimeoutException(sprintf "%s() was never released" name))
                Values.nul)
    new() = Gate("Hold")
    member _.Function : RecordingFunction = fn
    member _.AsFunction : IJyroFunction = fn.AsFunction
    /// Wait until an execution is inside `Hold()`.
    member _.WaitEntered() =
        if not (entered.Wait safety) then Values.fail (sprintf "No execution entered %s() within 10 s" name)
    /// Let one held execution go on.
    member _.Release() = release.Release() |> ignore

/// Run `work` on another thread.
let background (work: unit -> 'T) : Task<'T> = Task.Run(fun () -> work ())

/// The result of a background task, failing the test if it does not end in 10 s.
let await (task: Task<'T>) : 'T =
    if not (task.Wait safety) then Values.fail "A background execution did not end within 10 s"
    task.Result

/// Compile under `options` and fail the test if compilation fails.
let program (options: RunOptions) (source: string) : CompiledProgram =
    let compiled = Script.compileWith options source
    if not compiled.IsSuccess then
        Values.fail (sprintf "Expected the script to compile. Diagnostics:\n%s" (Script.describe compiled))
    Script.valueOf compiled

/// Execute a compiled program with a fresh runner under `options`.
let execute (options: RunOptions) (prog: CompiledProgram) : JyroResult<JyroValue> =
    (Script.runnerFor options prog).Execute()

/// A host function `name(v)` that answers `IsFrozen` of its argument.
let probe (name: string) : RecordingFunction =
    Host.func name [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Boolean FunctionEffect.Pure
        (fun args -> Values.bool (args.Head.IsFrozen))

/// A host function `name(v)` that answers whether its argument is the very
/// instance `expected` (reference identity).
let sameAs (name: string) (expected: JyroValue) : RecordingFunction =
    Host.func name [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Boolean FunctionEffect.Pure
        (fun args -> Values.bool (Object.ReferenceEquals(args.Head, expected)))

/// Assert that the result failed, with at least one diagnostic of severity Error.
let assertFailedWithError (result: JyroResult<'T>) =
    Assert.False(result.IsSuccess, "Expected failure. Diagnostics:\n" + Script.describe result)
    Assert.True(
        result.Messages |> Seq.exists (fun m -> m.Severity = MessageSeverity.Error),
        "Expected an Error diagnostic. Diagnostics:\n" + Script.describe result)

/// Assert that the result has no Error diagnostic whose code is `code`.
let assertNoErrorCode (code: MessageCode) (result: JyroResult<'T>) =
    let errs = Script.errorCodes result
    Assert.False(List.contains code errs, sprintf "Did not expect %A. Diagnostics:\n%s" code (Script.describe result))

/// Assert that compilation succeeded (no Error diagnostic).
let assertCompiles (result: JyroResult<'T>) =
    Assert.True(result.IsSuccess, "Expected the script to compile. Diagnostics:\n" + Script.describe result)
    Assert.Empty(Script.errorCodes result)

/// The property `name` of the Output of a successful execution.
let outProp (name: string) (result: JyroResult<JyroValue>) : JyroValue =
    Values.prop name (Script.output result)

/// True when the object value has the property (by key, not by null value).
let hasKey (name: string) (v: JyroValue) : bool =
    match v with
    | :? JyroObject as o -> (o :> Collections.Generic.IReadOnlyDictionary<string, JyroValue>).ContainsKey name
    | _ -> false

/// Nest `depth` objects `{ c: { c: ... {} } }`, the root counted as one (100.7.3).
/// Returns the root and every container on the path, root first.
let nestedObjects (depth: int) : JyroObject * JyroObject list =
    let all = [ for _ in 1 .. depth -> JyroObject() ]
    all |> List.pairwise |> List.iter (fun (outer, inner) -> outer.SetProperty("c", inner))
    List.head all, all

/// A JSON text of `depth` nested objects `{"c":{"c":...{}}}`.
let nestedJson (depth: int) : string =
    let sb = Text.StringBuilder()
    for _ in 1 .. depth - 1 do sb.Append("{\"c\":") |> ignore
    sb.Append("{}") |> ignore
    for _ in 1 .. depth - 1 do sb.Append("}") |> ignore
    sb.ToString()

/// The maximum nesting depth of 100.7 (the documented bound).
/// The value nesting maximum of the reference implementation (JYRO-REQ-2042 states it: 128).
let maxDepth () : int = 128

/// Every array and object reachable from `v`, `v` included (no cycle expected).
let rec containers (v: JyroValue) : JyroValue list =
    match v with
    | :? JyroObject as o -> v :: ((o :> Collections.Generic.IReadOnlyDictionary<string, JyroValue>).Values |> Seq.collect containers |> List.ofSeq)
    | :? JyroArray as a -> v :: (a |> Seq.collect containers |> List.ofSeq)
    | _ -> []

/// A compact type-and-exactness-aware rendering of a value, for comparing a
/// value's content before and after an execution (ToJson plus exactness).
let rec snapshot (v: JyroValue) : string =
    match v with
    | _ when isNull (box v) -> "<clr-null>"
    | :? JyroNumber as n -> sprintf "%s%s" (n.ToJson()) (if n.IsExact then "e" else "i")
    | :? JyroObject as o ->
        o |> Seq.map (fun kv -> sprintf "%s:%s" (Text.Json.JsonSerializer.Serialize kv.Key) (snapshot kv.Value))
        |> String.concat "," |> sprintf "{%s}"
    | :? JyroArray as a -> a |> Seq.map snapshot |> String.concat "," |> sprintf "[%s]"
    | _ -> v.ToJson()

/// `StdlibRegistry.TryGetFunction(name)`, as an option.
let stdlibLookup (name: string) : IJyroFunction option =
    let mutable f = Unchecked.defaultof<IJyroFunction>
    if StdlibRegistry.tryGetFunction (name, &f) then Some f else None

// -- FLOORS 4.1.11: every run sets its limits, and a regex run its timeout -----

/// The execution limits every run of the 4.1.11 tests sets explicitly: none of
/// them can end a run before the case under test is reached.
let specLimits : JyroExecutionOptions =
    Script.limits (fun o ->
        { o with
            MaxExecutionTime = TimeSpan.FromSeconds 30.
            MaxStatements = 1_000_000
            MaxLoopIterations = 1_000_000
            MaxCallDepth = min 64 JyroExecutionOptions.MaxSafeCallDepth })

/// The defaults with the limits of `specLimits`.
let bounded : RunOptions = { Script.defaults with Limits = Some specLimits }

/// `bounded`, and a regex timeout of 5 s (160.1.10, 160.7.2, reached through
/// `Pending`), for every run that evaluates a regular expression or a schema
/// pattern.
let boundedRegex : RunOptions =
    { bounded with ConfigureRunner = fun r -> Pending.withRegexTimeout r (TimeSpan.FromSeconds 5.) }

/// Exactly one error, of `code`, at the stage Execute (160.1.12: a runtime error).
let expectRuntimeError (code: MessageCode) (result: JyroResult<'T>) : unit =
    Script.expectError code result
    Assert.Equal("Execute", Pending.stageOf (Script.onlyErrorMessage result))

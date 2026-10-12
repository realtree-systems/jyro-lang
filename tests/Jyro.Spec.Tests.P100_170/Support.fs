/// Helpers local to the chapter 100-170 suite, written from the book and the
/// public host API alone (FLOORS 4.1.1). The shared helpers are in
/// Jyro.Spec.Common; what is here is only what those lack.
module RealTree.Jyro.Spec.Tests.Support

open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// Join source lines with "\n".
let lines (xs: string list) : string = String.concat "\n" xs

/// `n` copies of a line, each with `{i}` replaced by its index.
let repeatLines (n: int) (template: string) : string list =
    [ for i in 0 .. n - 1 -> template.Replace("{i}", string i) ]

/// Run options with the default limits changed by `f`.
let withLimits (f: JyroExecutionOptions -> JyroExecutionOptions) : RunOptions =
    { Script.defaults with Limits = Some (Script.limits f) }

/// Options that lift every limit far above what a test needs, so that the
/// limit under test is the only one that can fire.
let roomy (o: JyroExecutionOptions) : JyroExecutionOptions =
    { o with
        MaxExecutionTime = TimeSpan.FromSeconds 30.
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000 }

/// Every execution limit of 100.1.1 set explicitly and far above what a test
/// needs (FLOORS 4.1.11): no test depends on an implementation default.
let allLimits (o: JyroExecutionOptions) : JyroExecutionOptions =
    { o with
        MaxExecutionTime = TimeSpan.FromSeconds 30.
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000
        MaxCallDepth = 64 }

/// A generous compilation time limit, set explicitly (100.9.1, FLOORS 4.1.11),
/// so that no compile or load in a test can be ended by a limit it did not choose.
let generousCompile : TimeSpan = TimeSpan.FromMinutes 10.

/// `Script.defaults` with every limit that could end a run set explicitly:
/// the four execution limits (`allLimits`) and a generous compilation limit.
let pinned : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits allLimits)
        CompilationTimeout = Some generousCompile }

/// The time limit of a test that needs a statement BEFORE a host stall to run
/// before the limit takes effect (floors-process 4.1.18; d-581, d-585). The book
/// bounds nothing about when the first check comes after Execute() is called
/// (100.2.1, 100.3.1), so the pre-stall step is given ten times its longest
/// duration measured under a whole verification run.
/// Measurement (ZZ_MarginProbe, not committed): `dotnet test Jyro.slnx` at
/// 7d78d5a on 2026-09-28, 20:36-20:40, all 14 test projects, 22,786 samples of
/// each shape. From the call of JyroRunner.Execute() on the compiled program to
/// the end of the pre-stall statement - `Output.a = 1`, `Mark(0)`, and the
/// first iteration's `Mark(x)` of a foreach - the longest was 254.42 ms
/// (foreach; Output.a = 1: 138.29 ms; Mark(0): 0.58 ms; p99.9 under 0.05 ms).
/// Ten times 254.42 ms is 2,544 ms: the limit is 3,000 ms. (Counting the
/// compile too, from the call of Script.runWith, the longest was 691 ms in that
/// run and 918 ms in a run overlapping a second whole run; the limit's clock
/// does not cover the compile.) At 3,000 ms the limit is not under one second,
/// so these tests need no non-parallel collection.
let marginLimitMs : int = 3000

/// The stall after that statement: longer than `marginLimitMs`, so the check
/// at the next statement always finds the limit exceeded.
let marginStallMs : int = 3500

/// `pinned` limits with the execution time limit `marginLimitMs` (100.1.1),
/// and `fns` as host functions.
let marginOpts (fns: RecordingFunction list) : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits (fun o -> { allLimits o with MaxExecutionTime = TimeSpan.FromMilliseconds(float marginLimitMs) }))
        CompilationTimeout = Some generousCompile
        Functions = fns |> List.map (fun f -> f.AsFunction) }

/// True when `v` is an object that has the property `name`.
let hasProp (name: string) (v: JyroValue) : bool =
    match v with
    | :? JyroObject as o -> fst (o.TryGetValue name)
    | _ -> false

/// The value of the result (asserting there is one), then its property `name`.
let resultProp (name: string) (r: JyroResult<JyroValue>) : JyroValue =
    Script.valueOf r |> Values.prop name

/// Assert the execution succeeded, and return Output.
let ok (r: JyroResult<JyroValue>) : JyroValue = Script.output r

/// Assert exactly one error, of `code`.
let fails (code: MessageCode) (r: JyroResult<'T>) : unit = Script.expectError code r

/// Assert the error codes contain `code` (the result may also carry others).
let hasError (code: MessageCode) (r: JyroResult<'T>) : unit =
    Assert.False(r.IsSuccess, "Expected a failure. " + Script.describe r)
    if not (List.contains code (Script.errorCodes r)) then
        Values.fail (sprintf "Expected an error %A, got:\n%s" code (Script.describe r))

/// A number argument of a host function as an int.
let argInt (args: JyroValue list) (i: int) : int = int (Values.asFloat args.[i])

// -- Host functions used by many tests ----------------------------------------

let private anyArgs n = [ for i in 0 .. n - 1 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]

/// `Stall(ms[, value])`: blocks the thread for `ms` milliseconds without any
/// limit check, then returns `value` (or null).
let stall () : RecordingFunction =
    Host.func "Stall" (anyArgs 2) ParameterType.Any FunctionEffect.Pure (fun args ->
        Thread.Sleep(argInt args 0)
        if args.Length > 1 then args.[1] else Values.nul)

/// `StallChecked(ms[, value])`: blocks for `ms` milliseconds, then calls
/// `JyroExecutionContext.CheckExecutionLimits()`, then returns `value`.
let stallChecked () : RecordingFunction =
    Host.funcWithContext "StallChecked" (anyArgs 2) ParameterType.Any FunctionEffect.Pure (fun args ctx ->
        Thread.Sleep(argInt args 0)
        ctx.CheckExecutionLimits()
        if args.Length > 1 then args.[1] else Values.nul)

/// `CancelNow([value])`: cancels `cts` and returns `value` (or null).
let canceller (cts: CancellationTokenSource) : RecordingFunction =
    Host.func "CancelNow" (anyArgs 1) ParameterType.Any FunctionEffect.Pure (fun args ->
        cts.Cancel()
        if args.Length > 0 then args.[0] else Values.nul)

/// `Spin(n)`: a host function that loops `n` times, calling
/// `CheckExecutionLimits()` every 1,000 iterations, sleeping 1 ms each time.
let spinChecked () : RecordingFunction =
    Host.funcWithContext "Spin" (anyArgs 1) ParameterType.Any FunctionEffect.Pure (fun args ctx ->
        let n = argInt args 0
        for i in 0 .. n - 1 do
            if i % 1000 = 0 then
                Thread.Sleep 1
                ctx.CheckExecutionLimits()
        Values.nul)

/// `Mark(x)`: a recording function (Pure, any arguments, returns null).
let marker () : RecordingFunction = Host.recorder "Mark"

// -- Measuring -----------------------------------------------------------------

/// Run `f`, returning its result and the wall time it took.
let timed (f: unit -> 'T) : 'T * TimeSpan =
    let sw = Stopwatch.StartNew()
    let r = f ()
    sw.Stop()
    r, sw.Elapsed

/// Run `f`, returning its result and the bytes the current thread allocated.
let allocating (f: unit -> 'T) : 'T * int64 =
    let before = GC.GetAllocatedBytesForCurrentThread()
    let r = f ()
    let after = GC.GetAllocatedBytesForCurrentThread()
    r, after - before

/// 1 MB, the allocation ceiling the plan uses for "does not allocate the result".
let oneMB = 1L <<< 20

// -- Other entry paths ---------------------------------------------------------

/// The public entry paths that execute a script with no host limit
/// configuration. `Runner` is `new JyroRunner(program).Execute()` with no
/// `With*` call beyond the input.
type EntryPath =
    | Runner
    | ExecuteScript
    | ExecuteScriptWithJson
    | CompiledBytes

let allPaths = [ Runner; ExecuteScript; ExecuteScriptWithJson; CompiledBytes ]

let pathNamed (name: string) : EntryPath =
    allPaths |> List.find (fun p -> (sprintf "%A" p) = name)

let pathNames : obj[] seq = allPaths |> Seq.map (fun p -> [| box (sprintf "%A" p) |])

/// Run `source` with an empty-object input, on `path`, with the defaults.
let runOnPath (path: EntryPath) (source: string) : JyroResult<JyroValue> =
    match path with
    | Runner -> Script.runWith Script.defaults source
    | ExecuteScript -> JyroScript.executeScript source (box (Dictionary<string, obj>()))
    | ExecuteScriptWithJson -> JyroScript.executeScriptWithJson source "{}"
    | CompiledBytes ->
        let bytes = Script.compileBytesWith Script.defaults source
        if not bytes.IsSuccess then JyroResult<JyroValue>.Failure(bytes.Messages)
        else Script.runBytesWith Script.defaults (Script.valueOf bytes)

// -- Exceptions carrying codes -------------------------------------------------

/// The Jyro codes an exception carries: `JyroRuntimeException.Code`, or every
/// code of `JyroException.Messages`, looking through wrappers.
let rec codesOfExn (e: exn) : MessageCode list =
    match e with
    | :? JyroRuntimeException as r -> [ r.Code ]
    | :? JyroException as x -> [ for m in x.Messages -> m.Code ]
    | :? TargetInvocationException as t when not (isNull t.InnerException) -> codesOfExn t.InnerException
    | :? AggregateException as a -> a.InnerExceptions |> Seq.collect codesOfExn |> List.ofSeq
    | _ -> []

/// Assert that `f` fails through an exception carrying `code`.
let throwsWithCode (code: MessageCode) (f: unit -> 'T) : unit =
    let outcome =
        try Choice1Of2 (f ())
        with e -> Choice2Of2 e
    match outcome with
    | Choice1Of2 _ -> Values.fail (sprintf "Expected a failure carrying %A; the call returned normally" code)
    | Choice2Of2 e ->
        let codes = codesOfExn e
        if not (List.contains code codes) then
            Values.fail (sprintf "Expected %A; the call threw %s: %s (codes %A)" code (e.GetType().FullName) e.Message codes)

// -- Type names in diagnostics (130.6.5) ----------------------------------------

/// The names of the public types of the host API's assemblies (130.6.5 forbids them as arguments).
let apiTypeNames : Set<string> =
    [ typeof<JyroValue>.Assembly; typeof<DiagnosticMessage>.Assembly; typeof<JyroRunner>.Assembly; typeof<JyroCompiler>.Assembly ]
    |> List.distinct
    |> List.collect (fun a -> a.GetExportedTypes() |> Array.map (fun t -> t.Name) |> List.ofArray)
    |> Set.ofList

/// 130.6.5: the diagnostic's arguments hold `expected` and then, at a higher
/// index, `actual`, each spelled exactly as TypeOf gives it (95.1.2), and no
/// argument is the name of a public type of the API.
let assertTypeNameArgs (expected: string) (actual: string) (d: DiagnosticMessage) : unit =
    let args = d.Args |> Array.map (fun a -> if isNull a then "<null>" else string a)
    let ie = Array.IndexOf(args, expected)
    if ie < 0 then Values.fail (sprintf "130.6.5: no argument is exactly %A; the arguments are %A" expected args)
    let ia = if ie + 1 < args.Length then Array.IndexOf(args, actual, ie + 1) else -1
    if ia < 0 then Values.fail (sprintf "130.6.5: no argument %A after the expected type %A; the arguments are %A" actual expected args)
    for a in args do
        if apiTypeNames.Contains a then Values.fail (sprintf "130.6.5: the argument %A is a class name of the API; the arguments are %A" a args)

// -- Stages (160.1.11) ---------------------------------------------------------

/// The stage name of every diagnostic, in order.
let stages (r: JyroResult<'T>) : string list = [ for m in r.Messages -> Pending.stageOf m ]

/// Assert every diagnostic of `r` of `code` has the stage `stage`, and there is one.
let stageOfCode (code: MessageCode) (stage: string) (r: JyroResult<'T>) : unit =
    let ms = r.Messages |> Seq.filter (fun m -> m.Code = code) |> List.ofSeq
    if ms.IsEmpty then Values.fail (sprintf "No diagnostic %A. Diagnostics:\n%s" code (Script.describe r))
    for m in ms do Assert.Equal(stage, Pending.stageOf m)

// -- Nested values -------------------------------------------------------------

/// JSON text of nested arrays `depth` deep (the root counted as one), with the
/// scalar 1 at the bottom.
let nestedArrayJson (depth: int) : string = String.replicate depth "[" + "1" + String.replicate depth "]"

/// JSON text of nested objects `depth` deep, each holding the next as "a".
let nestedObjectJson (depth: int) : string =
    String.replicate depth "{\"a\":" + "1" + String.replicate depth "}"

/// A host-built Jyro array value `depth` deep, built without any walk.
let nestedArrayValue (depth: int) : JyroValue =
    let mutable v : JyroValue = Values.int 1
    for _ in 1 .. depth do
        let a = JyroArray()
        a.Add v
        v <- a
    v

/// A host (CLR) value of nested List<obj>, `depth` deep.
let nestedHostList (depth: int) : obj =
    let mutable v : obj = box 1
    for _ in 1 .. depth do
        let l = List<obj>()
        l.Add v
        v <- box l
    v

/// Jyro source lines that bind `name` to nested arrays `depth` deep, with 1 at
/// the bottom, built by a loop (so no literal or host walk is involved).
let buildNested (name: string) (depth: int) : string =
    sprintf "var %s = 1\nfor i_%s in 0 to %d do %s = [%s] end" name name depth name name

/// Same as `buildNested`, with objects `{a: ...}`.
let buildNestedObj (name: string) (depth: int) : string =
    sprintf "var %s = 1\nfor i_%s in 0 to %d do %s = {a: %s} end" name name depth name name

/// The documented maximum nesting depth (100.7.1).
/// The value nesting maximum of the reference implementation (JYRO-REQ-2042 states it: 128). The
/// guard that holds it is not public, so the tests state it.
let maxDepth () : int = 128

/// The documented safe maximum of the call depth (100.6.1).
let maxSafeCallDepth () : int = JyroExecutionOptions.MaxSafeCallDepth

/// A script function `R(n)` that recurses `n` more times, and a statement that
/// starts a chain of exactly `depth` nested calls of it.
let recursion (depth: int) : string =
    lines [ "func R(n)"
            "  if n > 0 then"
            "    return R(n - 1)"
            "  end"
            "  return 0"
            "end"
            sprintf "Output.r = R(%d)" (depth - 1) ]

// -- The standard library registry (StdlibRegistry, reached by its host names) ----

let private registryType () : Type =
    // JYRO-REQ-1922: StdlibRegistry is declared in Jyro.Api.
    Assembly.Load("Jyro.Api").GetType("RealTree.Jyro.StdlibRegistry", true)

/// `StdlibRegistry.GetAll()`.
let stdlibAll () : IJyroFunction list =
    let m = registryType().GetMethod("GetAll", Type.EmptyTypes)
    (m.Invoke(null, [||]) :?> IReadOnlyList<IJyroFunction>) |> List.ofSeq

/// `StdlibRegistry.TryGetFunction(name, out f)`.
let stdlibTryGet (name: string) : IJyroFunction option =
    let m = registryType().GetMethod("TryGetFunction")
    let args : obj[] = [| box name; null |]
    let found = m.Invoke(null, args) :?> bool
    if found then Some(args.[1] :?> IJyroFunction) else None

/// `StdlibRegistry.GetAll()` without the functions named.
let stdlibWithout (names: string list) : IJyroFunction list =
    stdlibAll () |> List.filter (fun f -> not (List.contains f.Name names))

// -- Bounding a test's run time ----------------------------------------------------

/// Run `f` on a background thread and wait at most `seconds` for it. A run that
/// does not finish fails the test (the book's limits should have stopped it);
/// the thread is left to finish on its own, as a background thread.
let bounded (seconds: float) (f: unit -> 'T) : 'T =
    let mutable result = Unchecked.defaultof<'T>
    let mutable error : exn = null
    let t =
        Thread(
            (fun () ->
                try result <- f ()
                with e -> error <- e),
            16 * 1024 * 1024)
    t.IsBackground <- true
    t.Start()
    if not (t.Join(TimeSpan.FromSeconds seconds)) then
        Values.fail (sprintf "The run did not finish within %.0f seconds" seconds)
    if not (isNull error) then
        System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(error).Throw()
    result

/// Jyro source of an array literal of the numbers 0 to n - 1 (no library call).
let arrayLiteral (n: int) : string = "[" + String.Join(", ", [ for i in 0 .. n - 1 -> string i ]) + "]"

/// A script that runs exactly `n` statements, most of them in the body of a
/// function called many times, so that the source stays short.
let statementScript (n: int) : string =
    let calls = n / 101
    let rest = n - calls * 101
    let body = [ "func F()" ] @ repeatLines 99 "  var a{i} = {i}" @ [ "  return 0"; "end" ]
    if calls = 0 then lines (repeatLines n "Output.s = {i}")
    else lines (body @ repeatLines calls "Output.c{i} = F()" @ repeatLines rest "Output.s{i} = {i}")

/// A script of `n` short top-level statements (for stage timings and artifacts).
let manyStatements (n: int) : string = lines (repeatLines n "Output.s{i} = ({i} + 1) * 2")

/// Stats attached to both the compiler and the runner.
let withStats (o: RunOptions) : RunOptions * JyroPipelineStats =
    let s = JyroPipelineStats()
    { o with Stats = Some s }, s

/// The six stage times of `s`, in the order Parse, Validate, Link, Compile, Execute, Deserialize.
let stageTimes (s: JyroPipelineStats) : (string * TimeSpan) list =
    [ "Parse", s.ParseTime; "Validate", s.ValidateTime; "Link", s.LinkTime; "Compile", s.CompileTime
      "Execute", s.ExecuteTime; "Deserialize", s.DeserializeTime ]

/// Sources covering most statement and expression kinds, for determinism checks (140.2).
let determinismCorpus : string list =
    [ "Output.a = 1 + 2 * 3\nOutput.b = \"x\" & 1.5\nOutput.c = [1, 2, 3]\nOutput.d = {k: true, j: null}"
      "func F(x, y = 2)\n  return x * y\nend\nOutput.r = F(3)\nOutput.s = F(x: 1, y: 5)"
      "union Shape Circle(r) Square(s: number) end\nvar c = Circle(2)\nmatch c do case Circle(r) then Output.r = r case Square(s) then Output.s = s end"
      "var t = 0\nfor i in 0 to 10 by 2 do t += i end\nforeach x in [1, 2] do t -= x end\nOutput.t = t"
      "Output.m = Map([1, 2, 3], x => x * 2)\nOutput.w = Where([1, 2, 3], x => x > 1)\nOutput.e = 0.1 + 0.2\nOutput.i = 1e300"
      "switch 2 do case 1 then Output.k = 1 case 2, 3 then Output.k = 2 default then Output.k = 0 end\nOutput.n = 5 is number\nOutput.q = null ?? 4"
      "var x = 1\nx += 1\nOutput.x = x\nOutput.o = {a: 1}\ndelete Output.o.a\nif x > 0 then Output.p = 1 elseif x < 0 then Output.p = 2 else Output.p = 3 end" ]


/// SHA-256 of each corpus source's artifact, joined with commas.
let determinismHashes () : string =
    let sha (b: byte[]) = Convert.ToHexString(System.Security.Cryptography.SHA256.HashData b)
    String.Join(",", determinismCorpus |> List.map (fun s -> Script.compileBytesWith Script.defaults s |> Script.valueOf |> sha))

// -- The formatter of 160.7.17, by the name the book declares -------------------

/// `DiagnosticFormatter.FormatLocalized(IMessageTemplateProvider, DiagnosticMessage)`
/// of 160.7.17, found by the C# name and the parameter types that clause
/// declares (the suite is F#, where a module function may be seen by another
/// name). Fails the test, for 160.7.17, when the member is not in the build.
let formatLocalizedMethod () : MethodInfo =
    let found =
        [ typeof<DiagnosticMessage>.Assembly; typeof<IMessageTemplateProvider>.Assembly; typeof<JyroRunner>.Assembly ]
        |> List.distinct
        |> List.tryPick (fun a -> Option.ofObj (a.GetType("RealTree.Jyro.DiagnosticFormatter")))
        |> Option.bind (fun t ->
            Option.ofObj (
                t.GetMethod(
                    "FormatLocalized",
                    BindingFlags.Public ||| BindingFlags.Static,
                    null,
                    [| typeof<IMessageTemplateProvider>; typeof<DiagnosticMessage> |],
                    null)))
    match found with
    | Some m -> m
    | None -> Pending.notInBuild "160.7.17" "DiagnosticFormatter.FormatLocalized(IMessageTemplateProvider, DiagnosticMessage)"

/// `DiagnosticFormatter.FormatLocalized(provider, msg)` (160.7.17). What it
/// throws reaches the caller as itself, not wrapped.
let formatLocalized (provider: IMessageTemplateProvider) (msg: DiagnosticMessage) : string =
    let m = formatLocalizedMethod ()
    try
        m.Invoke(null, [| box provider; box msg |]) :?> string
    with :? TargetInvocationException as e when not (isNull e.InnerException) ->
        System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e.InnerException).Throw()
        Unchecked.defaultof<_>

/// The prefix that 160.7.17 gives `FormatLocalized` for `msg`: `[JM`, the
/// code's number in decimal with zeros before it to 4 digits, `]`, then
/// ` Ln L, Col C: ` when `msg.Location` has a value whose `Line` is greater
/// than 0, and `: ` otherwise.
let formatPrefix (msg: DiagnosticMessage) : string =
    let code = "[JM" + (int msg.Code).ToString("D4", Globalization.CultureInfo.InvariantCulture) + "]"
    if msg.Location.HasValue && msg.Location.Value.Line > 0 then
        sprintf "%s Ln %d, Col %d: " code msg.Location.Value.Line msg.Location.Value.Column
    else code + ": "

/// The message of `msg` under `provider`: the text `FormatLocalized` returns
/// after its prefix (130.6.3, 160.7.17). Fails the test when the line does not
/// begin with the prefix.
let localizedMessage (provider: IMessageTemplateProvider) (msg: DiagnosticMessage) : string =
    let line = formatLocalized provider msg
    let prefix = formatPrefix msg
    if isNull line || not (line.StartsWith(prefix, StringComparison.Ordinal)) then
        Values.fail (sprintf "FormatLocalized returned %A, which does not begin with the prefix %A of 160.7.17" line prefix)
    line.Substring(prefix.Length)

/// A provider whose `TryGetTemplate` returns false for every code.
type NoTemplateProvider() =
    interface IMessageTemplateProvider with
        member _.TryGetTemplate(_code: MessageCode, [<Runtime.InteropServices.Out>] template: byref<string>) =
            template <- null
            false

/// A provider whose `TryGetTemplate` returns true with `null` for every code.
type NullTemplateProvider() =
    interface IMessageTemplateProvider with
        member _.TryGetTemplate(_code: MessageCode, [<Runtime.InteropServices.Out>] template: byref<string>) =
            template <- null
            true

/// A provider that gives `text` as the template of `code` when `code` is
/// `Some c` and the code is `c`, or of every code when it is `None`; for any
/// other code it returns false.
type TemplateProvider(text: string, code: MessageCode option) =
    interface IMessageTemplateProvider with
        member _.TryGetTemplate(c: MessageCode, [<Runtime.InteropServices.Out>] template: byref<string>) =
            match code with
            | Some only when only <> c ->
                template <- null
                false
            | _ ->
                template <- text
                true

/// An argument of a diagnostic read as 160.7.16 reads it.
let argText (a: obj) : string = Convert.ToString(a, Globalization.CultureInfo.InvariantCulture)

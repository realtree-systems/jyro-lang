/// Helpers for the maximum value size of 100.8.6 to 100.8.11 and the host
/// conversions of 160.8, written from the book and the public host API alone
/// (FLOORS 4.1.1): M, the code ValueSizeLimitExceeded reached by name
/// (160.7.11, not in every build), builders of large values, explicit limits
/// (FLOORS 4.1.11), the LargeValues collection, and the scenarios that run in a
/// child process (Child.fs), where a non-conforming build could walk 2^30 paths
/// or exhaust memory.
module RealTree.Jyro.Spec.Tests.ValueSize

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// The maximum value size M of 100.8.7: 16,777,216 (2^24).
let M = 16_777_216

// -- The code ValueSizeLimitExceeded (160.7.11), reached by name -------------------------------

let notInBuild = "160.7.11 member MessageCode.ValueSizeLimitExceeded is not in this build"

/// Fails the test because the code member is missing, after what was observed
/// has been checked against everything but the code. `ownFailure` is Some
/// description when the observation already breaks the test's own clause
/// whatever the code (no error at all, the wrong exception type): then the
/// failure is the test's own, with the missing member only noted. Otherwise
/// the only thing the test cannot check is the code, and the failure is
/// reported as blocked by 160.7.11 (floors-process 4.2.5, Pending.notInBuild).
let missingSizeCode (ownFailure: string option) (observed: string) : 'T =
    match ownFailure with
    | Some own -> Values.fail (sprintf "%s (and %s). %s" own notInBuild observed)
    | None ->
        Values.fail (sprintf "%s\nThe code cannot be compared with ValueSizeLimitExceeded. %s"
                        (Pending.notInBuildMessage "160.7.11" "MessageCode.ValueSizeLimitExceeded") observed)

/// `MessageCode.ValueSizeLimitExceeded`, found by its name; None when the
/// build does not declare it.
let tryValueSizeCode () : MessageCode option =
    match Enum.TryParse<MessageCode>("ValueSizeLimitExceeded", false) with
    | true, c when Enum.IsDefined(typeof<MessageCode>, c) -> Some c
    | _ -> None

/// The name of the size code, as 100.8.7, 100.8.9 and 160.8.1 give it.
let valueSizeName = "ValueSizeLimitExceeded"

/// True when `code` is the member named ValueSizeLimitExceeded. By the name the
/// clauses give, not by the number of 160.7.11 (d-575): a test of 100.8.9 or
/// 160.8.x that says "not the size code" depends on no clause but its own.
let isSizeCode (code: MessageCode) : bool = Enum.GetName(typeof<MessageCode>, code) = valueSizeName

/// The member, or a failed test that says it is not in this build.
let valueSizeCode () : MessageCode =
    match tryValueSizeCode () with
    | Some c -> c
    | None -> Pending.notInBuild "160.7.11" "MessageCode.ValueSizeLimitExceeded"

/// What a result gave, for a failure message.
let outcomeOf (r: JyroResult<'T>) : string =
    if r.IsSuccess && r.Messages.Count = 0 then "the run succeeded with no diagnostic"
    else sprintf "IsSuccess %b; diagnostics:\n%s" r.IsSuccess (Script.describe r)

/// True when this build has `DiagnosticMessage.Stage` (160.7.5).
let hasStage () : bool =
    not (isNull (typeof<DiagnosticMessage>.GetProperty("Stage", Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)))

/// Assert the stage of a runtime error is Execute (160.1.12). The member
/// `DiagnosticMessage.Stage` is pinned by the tests of 160.7.5; where a build
/// lacks it, the stage is not asserted here, so that a test of Part 100 fails
/// for its own clause and not for 160.7.5.
let assertExecuteStage (d: DiagnosticMessage) : unit =
    if hasStage () then Assert.Equal("Execute", Pending.stageOf d)

/// Assert that `r` failed with exactly one error, `ValueSizeLimitExceeded`,
/// of stage Execute (160.1.12), code first (FLOORS 4.1.11).
let failsForSize (r: JyroResult<'T>) : unit =
    match tryValueSizeCode () with
    | None ->
        let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> Seq.length
        let own =
            if r.IsSuccess || errors <> 1 then Some "Expected exactly one error, ValueSizeLimitExceeded, and a failed result"
            else None
        missingSizeCode own (sprintf "The run gave: %s" (outcomeOf r))
    | Some code ->
        let d = Script.onlyErrorMessage r
        if d.Code <> code then
            Values.fail (sprintf "Expected the one error ValueSizeLimitExceeded, got %A (%d). %s" d.Code (int d.Code) (outcomeOf r))
        assertExecuteStage d

/// Assert that `r` failed with exactly one error, `code`, stage Execute.
let failsWith (code: MessageCode) (r: JyroResult<'T>) : unit =
    let d = Script.onlyErrorMessage r
    if d.Code <> code then
        Values.fail (sprintf "Expected the one error %A (%d), got %A (%d). %s" code (int code) d.Code (int d.Code) (outcomeOf r))
    assertExecuteStage d

/// Assert that `f` throws a `JyroRuntimeException` whose `Code` is
/// `ValueSizeLimitExceeded` (160.8.1, 160.7.11).
let throwsForSize (what: string) (f: unit -> 'T) : unit =
    let outcome = try Choice1Of2(f ()) with e -> Choice2Of2 e
    match outcome, tryValueSizeCode () with
    | Choice1Of2 _, None ->
        missingSizeCode (Some (sprintf "%s: expected JyroRuntimeException ValueSizeLimitExceeded" what)) "The call returned normally."
    | Choice1Of2 _, Some _ -> Values.fail (sprintf "%s: expected JyroRuntimeException ValueSizeLimitExceeded; the call returned normally" what)
    | Choice2Of2 e, None ->
        let own =
            match e with
            | :? JyroRuntimeException -> None
            | _ -> Some (sprintf "%s: expected JyroRuntimeException ValueSizeLimitExceeded" what)
        missingSizeCode own (sprintf "%s: the call threw %s: %s" what (e.GetType().FullName) e.Message)
    | Choice2Of2 e, Some code ->
        match e with
        | :? JyroRuntimeException as j when j.Code = code -> ()
        | _ ->
            Values.fail (sprintf "%s: expected JyroRuntimeException with Code ValueSizeLimitExceeded; threw %s (codes %A): %s"
                            what (e.GetType().FullName) (codesOfExn e) e.Message)

/// Assert that `f` fails with `ValueNestingLimitExceeded` and not with the size
/// code (100.8.9), through an exception (100.7.6, 160.8.1).
let throwsForDepth (what: string) (f: unit -> 'T) : unit =
    let outcome = try Choice1Of2(f ()) with e -> Choice2Of2 e
    match outcome with
    | Choice1Of2 _ -> Values.fail (sprintf "%s: expected ValueNestingLimitExceeded; the call returned normally" what)
    | Choice2Of2 e ->
        let codes = codesOfExn e
        if List.exists isSizeCode codes || not (List.contains MessageCode.ValueNestingLimitExceeded codes) then
            Values.fail (sprintf "%s: expected ValueNestingLimitExceeded and not ValueSizeLimitExceeded; threw %s (codes %A): %s"
                            what (e.GetType().FullName) codes e.Message)

/// Assert that `f` does not fail with the size code (it may return, or fail otherwise).
let neverSizeError (what: string) (f: unit -> 'T) : unit =
    try f () |> ignore
    with e ->
        if List.exists isSizeCode (codesOfExn e) then
            Values.fail (sprintf "%s threw ValueSizeLimitExceeded for a value of size 1" what)

// -- Limits, set explicitly (FLOORS 4.1.11) ----------------------------------------------------

/// The four execution limits: `time`, statements and loop iterations at
/// Int32.MaxValue (so iteration inside an operation is never cut short), call
/// depth 64.
let bigLimits (time: TimeSpan) : JyroExecutionOptions =
    { JyroExecutionOptions.Default with
        MaxExecutionTime = time
        MaxStatements = Int32.MaxValue
        MaxLoopIterations = Int32.MaxValue
        MaxCallDepth = 64 }

/// Run options for large values: every limit explicit, 3 minutes of execution
/// time, and a generous compilation limit.
let large : RunOptions =
    { Script.defaults with
        Limits = Some(bigLimits (TimeSpan.FromMinutes 3.))
        CompilationTimeout = Some generousCompile }

/// `large` with `Input` given as a Jyro value.
let largeInput (input: JyroValue) : RunOptions = { large with Input = ValueInput input }

// -- Builders ---------------------------------------------------------------------------------

/// A host array of `n` copies of `v`.
let filled (n: int) (v: JyroValue) : JyroArray =
    let a = JyroArray()
    for _ in 1 .. n do a.Add v
    a

/// A host array of `n` nulls (size n + 1 by 100.8.6).
let nulls (n: int) : JyroArray = filled n JyroNull.Instance

/// A host string of `n` copies of `c` (size n + 1).
let chars (n: int) (c: char) : JyroString = JyroString(String(c, n))

/// A host object from pairs.
let objOf (pairs: (string * JyroValue) list) : JyroObject =
    let o = JyroObject()
    for (k, v) in pairs do o.SetProperty(k, v)
    o

/// The book's shared tree, built by the host: `a = [1]`, then `a = [a, a]`
/// `k` times; k + 1 arrays, size 3 x 2^k - 1 (100.8.6).
let hostTree (k: int) : JyroArray =
    let mutable a = JyroArray()
    a.Add(JyroNumber(1))
    for _ in 1 .. k do
        let b = JyroArray()
        b.Add a
        b.Add a
        a <- b
    a

/// The size 3 x 2^k - 1 of the shared tree after k doublings (100.8.6).
let treeSize (k: int) : int64 = 3L * (1L <<< k) - 1L

/// A host array that holds itself (30.8).
let hostCycle () : JyroArray =
    let a = JyroArray()
    a.Add(JyroNumber(1))
    a.Add a
    a

/// Source: `var s = "x"`, then `s = s & s` k times (length 2^k).
let stringDoubling (k: int) : string list = "var s = \"x\"" :: List.replicate k "s = s & s"

/// Source: `var a = [1]`, then `a = Concatenate(a, a)` k times (2^k elements).
let arrayDoubling (k: int) : string list = "var a = [1]" :: List.replicate k "a = Concatenate(a, a)"

/// Source: `var <n> = [1]`, then `<n> = [<n>, <n>]` k times (the book's shared tree).
let treeDoubling (name: string) (k: int) : string list =
    sprintf "var %s = [1]" name :: List.replicate k (sprintf "%s = [%s, %s]" name name name)

/// Source: `var <n> = {k: 1}`, then `<n> = {a: <n>, b: <n>}` k times.
let objectTreeDoubling (name: string) (k: int) : string list =
    sprintf "var %s = {k: 1}" name :: List.replicate k (sprintf "%s = {a: %s, b: %s}" name name name)

// -- Host functions ---------------------------------------------------------------------------

let private noArgs = Host.signature "Mark" [] ParameterType.Any

/// `Mark()`: records `GC.GetTotalAllocatedBytes(true)` (process-wide, for the
/// LargeValues collection, which runs alone) or the executing thread's
/// allocated bytes, and returns null. NonDeterministic so that no compiler may
/// fold or drop the call.
type AllocationMark(perThread: bool) =
    inherit JyroFunctionBase("Mark", noArgs)
    let marks = ResizeArray<int64>()
    override _.Effect = FunctionEffect.NonDeterministic
    override _.Execute(_args, _ctx) =
        let now = if perThread then GC.GetAllocatedBytesForCurrentThread() else GC.GetTotalAllocatedBytes(true)
        lock marks (fun () -> marks.Add now)
        JyroNull.Instance :> JyroValue
    member _.Marks : int64 list = lock marks (fun () -> List.ofSeq marks)
    member this.AsFunction : IJyroFunction = this :> IJyroFunction

/// `Count(x)`: adds one to a counter and returns `x`. Holds no arguments, so
/// M + 1 calls cost nothing.
type CountingFunction() =
    inherit JyroFunctionBase(
        "Count",
        Host.signature "Count" [ Parameter.Required("x", ParameterType.Any) ] ParameterType.Any)
    let mutable count = 0L
    override _.Effect = FunctionEffect.NonDeterministic
    override _.Execute(args, _ctx) =
        Interlocked.Increment(&count) |> ignore
        args.[0]
    member _.Count : int64 = Interlocked.Read(&count)
    member this.AsFunction : IJyroFunction = this :> IJyroFunction

/// `<name>()`: returns the host value `v` (a host function's result, exempt from 100.8.7).
type ReturnsHostValue(name: string, v: JyroValue) =
    inherit JyroFunctionBase(name, Host.signature name [] ParameterType.Any)
    override _.Effect = FunctionEffect.NonDeterministic
    override _.Execute(_args, _ctx) = v
    member this.AsFunction : IJyroFunction = this :> IJyroFunction

// -- The six members of 160.8.1 -------------------------------------------------------------------

/// The members of 160.8.1 that every class deriving from JyroValue has.
let valueMembers = [ "ToObjectValue"; "ToJson"; "WriteJson"; "ToStringValue"; "ToString" ]

/// Call the member `name` of 160.8.1 on `v`. `ToDictionary` needs an object.
/// `WriteJson` writes to a writer on Stream.Null and flushes it.
let convert (name: string) (v: JyroValue) : obj =
    match name with
    | "ToObjectValue" -> v.ToObjectValue()
    | "ToJson" -> box (v.ToJson())
    | "WriteJson" ->
        use w = new Utf8JsonWriter(Stream.Null)
        v.WriteJson(w)
        w.Flush()
        box w.BytesCommitted
    | "ToStringValue" -> box (v.ToStringValue())
    | "ToString" -> box (v.ToString())
    | "ToDictionary" ->
        match v with
        | :? JyroObject as o -> box (o.ToDictionary())
        | _ -> failwithf "ToDictionary needs an object, got %A" v.ValueType
    | other -> failwithf "unknown member %s" other

// -- The LargeValues collection -------------------------------------------------------------------

/// The heap limit the test host runs under (System.GC.HeapHardLimit in the
/// project file), so that a non-conforming build fails with an exception
/// instead of exhausting the machine.
let heapLimit = 4L * 1024L * 1024L * 1024L

/// Fails every test of the collection when the test host has no heap limit.
type HeapLimitGuard() =
    do
        let available = GC.GetGCMemoryInfo().TotalAvailableMemoryBytes
        if available > heapLimit then
            raise (
                Xunit.Sdk.XunitException(
                    sprintf "The test host has no GC heap limit (%d bytes available); LargeValues needs System.GC.HeapHardLimit = %d"
                        available heapLimit))

/// Collects and compacts the whole heap, the large object heap included, so that
/// what an earlier test built no longer counts against the heap limit.
let reclaimHeap () : unit =
    System.Runtime.GCSettings.LargeObjectHeapCompactionMode <- System.Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
    GC.Collect(2, GCCollectionMode.Forced, true, true)
    GC.WaitForPendingFinalizers()
    GC.Collect(2, GCCollectionMode.Forced, true, true)

/// The one mechanism of the LargeValues collection (JYRO-DEF-634, 635): xUnit runs
/// the attribute of a collection definition around every test of every class in the
/// collection, so each test starts with, and leaves, a reclaimed heap.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method)>]
type ReclaimHeapAttribute() =
    inherit Xunit.Sdk.BeforeAfterTestAttribute()
    override _.Before(_methodUnderTest: System.Reflection.MethodInfo) = reclaimHeap ()
    override _.After(_methodUnderTest: System.Reflection.MethodInfo) = reclaimHeap ()

/// Tests that hold values of size about M. They run one at a time, after every
/// other collection, under the test host's heap limit. Every test of the collection
/// has its heap reclaimed before and after it (ReclaimHeapAttribute).
[<CollectionDefinition("LargeValues", DisableParallelization = true); ReclaimHeap>]
type LargeValuesCollection() =
    interface ICollectionFixture<HeapLimitGuard>

// -- Scenarios that run in a child process (Child.fs) -----------------------------------------------

let private describeExn (e: exn) : string =
    let codes = codesOfExn e |> List.map string
    sprintf "ok=false codes=%s exn=%s" (String.Join(",", codes)) (e.GetType().Name)

let private resultLine (r: JyroResult<JyroValue>) : string =
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    let stage =
        match errors with
        | d :: _ -> (try Pending.stageOf d with _ -> "unknown")
        | [] -> "none"
    sprintf "ok=%b codes=%s stage=%s msgs=%d hasValue=%b"
        r.IsSuccess (String.Join(",", errors |> List.map (fun m -> string m.Code))) stage r.Messages.Count r.HasValue

let private compileLarge (options: RunOptions) (source: string) : CompiledProgram =
    let c = Script.compileWith options source
    match c.TryGetValue() with
    | true, p when c.IsSuccess -> p
    | _ -> failwithf "compile failed: %s" (Script.describe c)

/// Operator walks of 100.2.5 and 100.4.3. `kind` is the operation, `mode` is
/// "time" (a 1 s limit, no token), "timetoken" (a 1 s limit and a token that is
/// never canceled) or "cancel" (a 10 min limit, canceled after 500 ms).
let private operatorScenario (kind: string) (mode: string) : string =
    let walk op = lines (treeDoubling "a" 30 @ treeDoubling "b" 30 @ [ sprintf "Output.r = a %s b" op ])
    let walkObj op = lines (objectTreeDoubling "o" 30 @ objectTreeDoubling "p" 30 @ [ sprintf "Output.r = o %s p" op ])
    let loop expr = lines [ "var x = null"; "while true do"; sprintf "  x = %s" expr; "end" ]
    let half = M / 2
    let source, input =
        match kind with
        | "eq-array" -> walk "==", Values.emptyObj ()
        | "ne-array" -> walk "!=", Values.emptyObj ()
        | "eq-object" -> walkObj "==", Values.emptyObj ()
        | "ne-object" -> walkObj "!=", Values.emptyObj ()
        | "concat" -> loop "Input.s & Input.t", objOf [ "s", chars half 'x'; "t", chars half 'y' ] :> JyroValue
        | "lt" | "le" | "gt" | "ge" ->
            let op = match kind with "lt" -> "<" | "le" -> "<=" | "gt" -> ">" | _ -> ">="
            // Two distinct string instances with equal content: every code unit is compared.
            loop (sprintf "Input.s %s Input.t" op), objOf [ "s", chars M 'x'; "t", chars M 'x' ] :> JyroValue
        | other -> failwithf "unknown operator kind %s" other
    let time = if mode = "cancel" then TimeSpan.FromMinutes 10. else TimeSpan.FromSeconds 1.
    let options = { Script.defaults with Limits = Some(bigLimits time); CompilationTimeout = Some generousCompile }
    let program = compileLarge options source
    use cts = new CancellationTokenSource()
    let mutable runner = JyroRunner(program).WithInput(input).WithExecutionOptions(bigLimits time)
    if mode <> "time" then runner <- runner.WithCancellationToken(cts.Token)
    let sw = Stopwatch.StartNew()
    // From a thread of its own, not a pool timer (JYRO-DEF-745); the walk does not end by itself.
    if mode = "cancel" then RealTree.Jyro.Spec.HostCancel.cancelFromThread cts (TimeSpan.FromMilliseconds 500.)
    let r = runner.Execute()
    sw.Stop()
    sprintf "%s ms=%d" (resultLine r) sw.ElapsedMilliseconds

/// Source of the book's example of 100.8.9: a cyclic array whose first two
/// elements are strings of 8,388,608 code units.
let bookCyclic : string list =
    stringDoubling 23 @ [ "var a = [s, s]"; "a[Length(a)] = a" ]

/// A host array [s, s, x] with s of 8,388,608 code units.
let hostTwoStringsAnd (x: JyroValue) : JyroArray =
    let s = chars (M / 2) 'x'
    let a = JyroArray()
    a.Add s
    a.Add(chars (M / 2) 'y')
    a.Add x
    a

/// A value for a conversion scenario.
let private conversionValue (value: string) : JyroValue * string =
    match value with
    | "out30" ->
        // 160.8.1: the result's value of the book's script of 30 doublings.
        let r = Script.runWith large (lines (treeDoubling "a" 30 @ [ "Output.x = a" ]))
        Script.valueOf r, resultLine r
    | "outbook" ->
        // 100.8.9: the result's value of a run that builds the book's cyclic example.
        let r = Script.runWith large (lines (bookCyclic @ [ "Output.a = a" ]))
        Script.valueOf r, resultLine r
    | "tree30" -> hostTree 30 :> JyroValue, "host"
    | "cyclic" -> hostCycle () :> JyroValue, "host"
    | "sscyclic" -> hostTwoStringsAnd (hostCycle ()) :> JyroValue, "host"
    | "surrcyclic" ->
        // 160.8.4: a cyclic array whose first element is a string of one unpaired surrogate.
        let a = JyroArray()
        a.Add(JyroString(string (char 0xD800)))
        a.Add a
        a :> JyroValue, "host"
    | other -> failwithf "unknown value %s" other

/// Call one member of 160.8.1 on a value and report: the codes and the
/// exception, the bytes the calling thread allocated during the call, the
/// time, and for WriteJson whether the writer and its stream are unchanged
/// (160.8.2). `ToDictionary` is called on the value when it is an object, and
/// otherwise on an object `{k: value}`.
let private conversionScenario (value: string) (memberName: string) : string =
    let v, run = conversionValue value
    let target = if memberName = "ToDictionary" && not (v :? JyroObject) then objOf [ "k", v ] :> JyroValue else v
    use ms = new MemoryStream()
    use w = new Utf8JsonWriter(ms)
    w.WriteStartArray()
    w.WriteNumberValue(1)
    w.Flush()
    w.WriteNumberValue(2) // pending, not flushed: "[1" committed, ",2" pending
    let pending0, committed0, length0 = w.BytesPending, w.BytesCommitted, ms.Length
    let sw = Stopwatch.StartNew()
    let before = GC.GetAllocatedBytesForCurrentThread()
    let outcome =
        try
            match memberName with
            | "WriteJson" -> target.WriteJson(w)
            | m -> convert m target |> ignore
            "ok=true codes= exn=none"
        with e -> describeExn e
    let allocated = GC.GetAllocatedBytesForCurrentThread() - before
    sw.Stop()
    let unchanged = w.BytesPending = pending0 && w.BytesCommitted = committed0 && ms.Length = length0
    sprintf "%s alloc=%d ms=%d unchanged=%b run=[%s]" outcome allocated sw.ElapsedMilliseconds unchanged run

/// 160.8.1 (4): a host function that receives the shared tree of 30 doublings
/// during an execution and calls one member on it, catching what it throws.
type ConvertInside(memberName: string) =
    inherit JyroFunctionBase(
        "Convert",
        Host.signature "Convert" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any)
    member val Outcome = "not called" with get, set
    override _.Effect = FunctionEffect.NonDeterministic
    override this.Execute(args, _ctx) =
        this.Outcome <-
            try
                convert memberName args.[0] |> ignore
                "ok=true codes= exn=none"
            with e -> describeExn e
        JyroNull.Instance :> JyroValue

let private duringScenario (memberName: string) : string =
    let f = ConvertInside(memberName)
    let arg = if memberName = "ToDictionary" then "{k: a}" else "a"
    let r =
        Script.runWith
            { large with Functions = [ f :> IJyroFunction ] }
            (lines (treeDoubling "a" 30 @ [ sprintf "Convert(%s)" arg ]))
    sprintf "%s run=[%s]" f.Outcome (resultLine r)

/// 100.8.9 script walks of a cyclic value whose walk passes M first.
let private nestingScenario (case: string) : string =
    match case with
    | "clone" -> resultLine (Script.runWith large (lines (bookCyclic @ [ "Output.r = Length(Clone(a))" ])))
    | "tojson" -> resultLine (Script.runWith large (lines (bookCyclic @ [ "Output.r = Length(ToJson(a))" ])))
    | "flatten" ->
        let input = objOf [ "b", nulls (M / 2 + 1) ]
        resultLine (
            Script.runWith (largeInput input)
                (lines [ "var c = [1]"; "c[1] = c"; "Output.r = Length(Flatten([Input.b, Input.b, c]))" ]))
    | other -> failwithf "unknown nesting case %s" other

/// 160.8.3: the script of 160.8.1 run on each public path.
let private runPathScenario (path: string) : string =
    let source = lines (treeDoubling "a" 30 @ [ "Output.x = a" ])
    match path with
    | "Execute" -> resultLine (Script.runWith large source)
    | "ExecuteOrThrow" ->
        let program = compileLarge large source
        try
            let v = JyroRunner(program).WithInputJson("{}").WithExecutionOptions(bigLimits (TimeSpan.FromMinutes 3.)).ExecuteOrThrow()
            sprintf "ok=true codes= stage=none msgs=0 hasValue=%b" (not (isNull (box v)))
        with e -> describeExn e
    | "ExecuteScript" -> resultLine (JyroScript.executeScript source (box (Dictionary<string, obj>())))
    | other -> failwithf "unknown path %s" other

/// JYRO-REQ-1656 and 90.4.2: FromJson of a well-formed text below M, in a child
/// whose heap is too small to hold the value it makes. The run may fail or, if the
/// value fits, succeed, but FromJson may not report the text as not JSON.
let private fromJsonShortOfHeapScenario () : string =
    let text = "[" + String.Join(",", Seq.replicate 8_000_000 "{}") + "]"
    let r =
        Script.runWith
            (largeInput (objOf [ "t", JyroString(text) ]))
            (lines [ "var v = FromJson(Input.t)"; "Output.isNull = v == null"; "v = null" ])
    let isNullField =
        if r.IsSuccess then string (Values.asBool (Values.prop "isNull" (Script.valueOf r))) else "n/a"
    sprintf "%s isNull=%s" (resultLine r) (isNullField.ToLowerInvariant())

/// The child's entry for the scenarios of this module ("vs|...").
let runScenario (parts: string[]) : string =
    match parts with
    | [| "fromjson"; "short-of-heap" |] -> fromJsonShortOfHeapScenario ()
    | [| "op"; kind; mode |] -> operatorScenario kind mode
    | [| "conv"; value; memberName |] -> conversionScenario value memberName
    | [| "during"; memberName |] -> duringScenario memberName
    | [| "nest"; case |] -> nestingScenario case
    | [| "path"; path |] -> runPathScenario path
    | _ -> failwithf "unknown value-size scenario %A" parts

/// A `key=value` field of a child's RESULT line.
let field (key: string) (result: string) : string option =
    let marker = key + "="
    let i = result.IndexOf(" " + marker)
    let i = if result.StartsWith marker then 0 else if i >= 0 then i + 1 else -1
    if i < 0 then None
    else
        let rest = result.Substring(i + marker.Length)
        let stop = rest.IndexOf ' '
        Some(if stop >= 0 then rest.Substring(0, stop) else rest)

/// The part `run=[...]` of a child's RESULT line: what the run that made the value gave.
let runOf (result: string) : string option =
    let i = result.IndexOf "run=["
    if i < 0 then None
    else
        let rest = result.Substring(i + 5)
        let j = rest.IndexOf ']'
        Some(if j >= 0 then rest.Substring(0, j) else rest)

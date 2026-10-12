namespace RealTree.Jyro.Tests.Runtime

open System
open System.Diagnostics
open System.Threading
open Xunit
open FsCheck
open FsCheck.FSharp
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.RuntimeGenerators

/// Adversarial sweep: a hostile script under tightened limits must ALWAYS
/// terminate via a JyroResult failure carrying the expected limit code -
/// never a CLR exception, never a hang. Generalizes the hand-picked cases
/// in `NoCrash` and the depth ceiling in `CallDepthMeasurement` over
/// FsCheck-generated inputs.
module SandboxSecurity =

    // -- helpers ------------------------------------------------------------

    /// Build options with all four limits tightened. `Int32.MaxValue` for any
    /// axis disables that axis so the test can isolate the one it cares about.
    let private tightOpts
        (maxStatements: int)
        (maxLoopIterations: int)
        (maxCallDepth: int)
        (maxExecutionMs: int) : JyroExecutionOptions =
        { MaxStatements = maxStatements
          MaxLoopIterations = maxLoopIterations
          MaxCallDepth = maxCallDepth
          MaxExecutionTime = TimeSpan.FromMilliseconds(float maxExecutionMs) }

    /// Run with explicit options and return both the result and wall-clock
    /// elapsed. Wraps the same pipeline `Runner.run` uses, with the limits set
    /// on the runner - which is the only place they ever applied.
    ///
    /// The program is compiled and the runner configured first; the time is
    /// that of `Execute()` alone, from its call to its return, the interval the
    /// time limit counts (90.12.3, JYRO-REQ-377: compilation is not counted).
    /// Timing the compile too made a bound depend on the load of the runner.
    /// A compilation that fails is returned with no time.
    let private runMeasured
        (opts: JyroExecutionOptions)
        (source: string)
        (input: JyroValue) : JyroResult<JyroValue> * TimeSpan =
        match JyroCompiler().WithStdlib().WithSource(source).Compile() with
        | Produced program ->
            let runner = JyroRunner(program).WithExecutionOptions(opts).WithInput(input)
            let sw = Stopwatch.StartNew()
            let r = runner.Execute()
            sw.Stop()
            r, sw.Elapsed
        | Failed msgs ->
            JyroResult<JyroValue>.Failure(msgs), TimeSpan.Zero
        | _ ->
            JyroResult<JyroValue>.Failure(
                DiagnosticMessage.Error(MessageCode.InternalStageFailure, MessageStage.Compile, [| box "compile" |])), TimeSpan.Zero

    let private codes (r: JyroResult<JyroValue>) : MessageCode list =
        List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)

    let private expectCode (expected: MessageCode) (r: JyroResult<JyroValue>) =
        Assert.False(r.IsSuccess, sprintf "Expected failure %A, got success" expected)
        Assert.Contains(codes r, fun c -> c = expected)

    let private expectAnyCode (expected: MessageCode list) (r: JyroResult<JyroValue>) =
        Assert.False(r.IsSuccess, sprintf "Expected failure %A, got success" expected)
        let got = codes r
        Assert.True(
            expected |> List.exists (fun e -> List.contains e got),
            sprintf "Expected any of %A, got %A" expected got)

    let private wallBound (limitMs: int) (elapsed: TimeSpan) =
        Assert.True(
            elapsed.TotalMilliseconds <= float limitMs,
            sprintf "Wall clock %.0fms exceeded %dms budget" elapsed.TotalMilliseconds limitMs)

    /// Straight-line "var x1 = 1\nvar x2 = 2\n..." bomb of `n` statements.
    let private bombOfSize (n: int) : string =
        let sb = System.Text.StringBuilder(n * 16)
        for i in 1 .. n do
            sb.AppendFormat("var x{0} = {0}\n", i) |> ignore
        sb.ToString()

    let private emptyInput () : JyroValue = JyroObject() :> JyroValue

    // -- local adversarial generators --------------------------------------

    /// ReDoS templates paired with subject strings chosen to maximize
    /// backtracking. Padding is bounded so we don't blow the 1s regex budget
    /// on every run - we just need to RELIABLY trip the timeout or be caught
    /// by the surrounding execution-time cap.
    let private genEvilRegex : Gen<string * string> =
        gen {
            let! pad = Gen.choose(20, 30)
            let aStr = String.replicate pad "a"
            let! template =
                Gen.elements [
                    "^(a+)+$",     aStr + "!"
                    "^(a|a)*$",    aStr + "b"
                    "^(a*)*b$",    aStr + "c"
                    "^(.*a){25}$", aStr + "b"
                ]
            return template
        }

    let private genMutualRecursionDepth : Gen<int> = Gen.choose(16, 96)
    let private genLimitN : Gen<int> = Gen.choose(8, 256)
    let private genStatementBombSize : Gen<int> = Gen.choose(50, 500)

    /// JSON literal with `{ "a": { "a": ... } }` nesting of a given depth.
    /// Bounded at 400 so we pressure the parser without blowing out assembly
    /// limits on the host.
    let private genDeeplyNestedJson : Gen<string> =
        gen {
            let! depth = Gen.choose(5, 400)
            let sb = System.Text.StringBuilder(depth * 8)
            for _ in 1 .. depth do sb.Append("{\"a\":") |> ignore
            sb.Append("1") |> ignore
            for _ in 1 .. depth do sb.Append("}") |> ignore
            return sb.ToString()
        }

    // -- A. Resource-limit invariants --------------------------------------

    [<Fact>]
    let ``tight while-loop trips loop-iteration or statement limit`` () =
        // Default statements=10_000 is far above 500 loop iterations * 1-body
        // statement - so LoopIterationLimit wins. Keep both in the expected
        // set since the ordering is an internal detail.
        let opts = tightOpts Int32.MaxValue 500 64 5_000
        let src = "while true do\n  var x = 1\nend"
        let r, elapsed = runMeasured opts src (emptyInput ())
        expectAnyCode
            [ MessageCode.LoopIterationLimitExceeded
              MessageCode.StatementLimitExceeded ]
            r
        wallBound 2_000 elapsed

    [<FsCheck.Xunit.Property>]
    let ``straight-line N-statement bomb trips StatementLimitExceeded`` () =
        Prop.forAll (Arb.fromGen genStatementBombSize) (fun size ->
            // Cap below the generated bomb so the trip point is inside the bomb.
            let cap = size / 2
            let opts = tightOpts cap Int32.MaxValue 64 5_000
            let src = bombOfSize size
            let r, elapsed = runMeasured opts src (emptyInput ())
            expectCode MessageCode.StatementLimitExceeded r
            wallBound 2_000 elapsed
            true)

    [<FsCheck.Xunit.Property>]
    let ``mutual recursion at generated depth trips CallDepthLimitExceeded`` () =
        Prop.forAll (Arb.fromGen genMutualRecursionDepth) (fun depth ->
            // MaxCallDepth is the trip axis - keep other caps generous.
            let opts = tightOpts Int32.MaxValue Int32.MaxValue depth 5_000
            let src =
                """
func a(n)
  return b(n + 1)
end
func b(n)
  return a(n + 1)
end
a(0)"""
            let r, elapsed = runMeasured opts src (emptyInput ())
            expectCode MessageCode.CallDepthLimitExceeded r
            wallBound 2_000 elapsed
            true)

    [<Fact>]
    let ``Sleep with MaxExecutionTime cancels inside wall-clock bound`` () =
        let opts = tightOpts Int32.MaxValue Int32.MaxValue 64 200
        // Sleep catches OperationCanceledException and re-raises as
        // ExecutionTimeLimitExceeded so the diagnostic carries the real
        // cause, not a generic "A task was canceled" RuntimeError.
        let src = "Sleep(10000000)"
        let r, elapsed = runMeasured opts src (emptyInput ())
        expectCode MessageCode.ExecutionTimeLimitExceeded r
        wallBound 1_500 elapsed

    [<FsCheck.Xunit.Property>]
    let ``catastrophic regex ends inside wall-clock bound`` () =
        Prop.forAll (Arb.fromGen genEvilRegex) (fun (pattern, subject) ->
            // Generous MaxExecutionTime so we race against the regex's own 1s
            // timeout. Either code is acceptable - they both mean "sandbox
            // caught a ReDoS attempt".
            let opts = tightOpts Int32.MaxValue Int32.MaxValue 64 5_000
            let input = JyroObject()
            input.SetProperty("p", JyroString(pattern))
            input.SetProperty("s", JyroString(subject))
            // `RegexTest(text, pattern)` - text first. This passed the evil
            // pattern as the *text* and the padded subject as the *pattern*,
            // so the pattern actually compiled was a literal `"aaaa...!"` and
            // no catastrophic backtracking was ever built. Measured: as
            // written, 12/12 generated cases returned success in 0-1 ms with
            // no diagnostic; in the documented order, 7/12 report RegexTimeout
            // at ~1000 ms. The whole ReDoS guard - `regexTimeout` and its four
            // handlers in StringFunctions - could be deleted and this property
            // stayed green.
            let src = "Output.result = RegexTest(Input.s, Input.p)"
            let r, elapsed = runMeasured opts src (input :> JyroValue)
            // On fast paths the match might succeed; only assert wall clock
            // when it does NOT succeed. (Success just means the runtime
            // resolved the pattern within budget - still sandbox-safe.)
            if not r.IsSuccess then
                expectAnyCode
                    [ MessageCode.RegexTimeout
                      MessageCode.ExecutionTimeLimitExceeded ]
                    r
            wallBound 3_500 elapsed
            true)

    // -- B. Memory / allocation pressure -----------------------------------

    [<Fact>]
    let ``repeated Append is stopped by the loop budget`` () =
        // This script did not compile. `set` is not a Jyro keyword - nothing in
        // StatementParser defines it, and this was its only occurrence in any
        // script in the suite - so both assignments failed validation with
        // `UndeclaredVariable` and the loop never ran. Not one `Append` was
        // ever called, `MaxLoopIterations` was never consulted, and the
        // wall-clock bound timed a 31 ms compile.
        //
        // The old assertion could not tell the cases apart: `IsSuccess ||
        // Messages.Count > 0` is satisfied by two compile errors, by the
        // correct result, and by an unrelated garbage script alike. It is
        // replaced by the specific outcome, which is what the sibling budget
        // tests in this file assert.
        let opts = tightOpts Int32.MaxValue 1_000 64 5_000
        let src =
            """
var xs = []
var i = 0
while i < 5000 do
  xs = Append(xs, i)
  i = i + 1
end
Output.count = Length(xs)"""
        let r, elapsed = runMeasured opts src (emptyInput ())
        Assert.False(r.IsSuccess, "5000 iterations under a 1000-iteration budget must be stopped")
        expectAnyCode [ MessageCode.LoopIterationLimitExceeded ] r
        wallBound 5_000 elapsed

    [<Fact>]
    let ``the Append script compiles`` () =
        // Pinned separately, because a script that does not compile satisfies
        // almost any assertion about the run that follows it. Compiling is the
        // precondition of the test above, so it is stated rather than assumed.
        let compiled = JyroCompiler().WithStdlib().WithSource("""
var xs = []
var i = 0
while i < 3 do
  xs = Append(xs, i)
  i = i + 1
end
Output.count = Length(xs)""").Compile()
        Assert.True(compiled.IsSuccess,
                    sprintf "%A" (List.ofSeq compiled.Messages |> List.map (fun m -> m.Message)))

    [<FsCheck.Xunit.Property>]
    let ``FromJson on deeply-nested literal never throws`` () =
        Prop.forAll (Arb.fromGen genDeeplyNestedJson) (fun json ->
            let input = JyroObject()
            input.SetProperty("j", JyroString(json))
            let src = "Output.v = FromJson(Input.j)"
            try
                let _r = Runner.run src (input :> JyroValue)
                true
            with _ -> false)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``FromJson on hostile literals never throws`` () =
        Prop.forAll (Arb.fromGen genHostileJsonLiteral) (fun json ->
            let input = JyroObject()
            input.SetProperty("j", JyroString(json))
            let src = "Output.v = FromJson(Input.j)"
            try
                let _r = Runner.run src (input :> JyroValue)
                true
            with _ -> false)

    // -- C. No-crash under adversarial input -------------------------------

    let private riskyHarnesses = [
        "Output.r = FromJson(ToJson(Input.v))"
        "Output.r = Base64Decode(ToString(Input.v))"
        "Output.r = Substring(ToString(Input.v), -5, 99)"
        "Output.r = RegexTest(ToString(Input.v), ToString(Input.v))"
        "Output.r = Split(ToString(Input.v), \"\")"
    ]

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``risky stdlib calls over generated primitives never throw`` () =
        Prop.forAll (Arb.fromGen RuntimeGenerators.genPrimitive) (fun v ->
            let input = JyroObject()
            input.SetProperty("v", v)
            riskyHarnesses
            |> List.forall (fun src ->
                try
                    let _r = Runner.run src (input :> JyroValue)
                    true
                with _ -> false))

    // -- D. Escape-surface ratchet -----------------------------------------

    [<Fact>]
    let ``stdlib exposes no filesystem or process or reflection surface`` () =
        // Case-sensitive, position-0 prefix match. `ToJson` / `FromJson`-like
        // legitimate names are safe because we only match a handful of
        // known-bad prefixes that no real stdlib helper starts with.
        let banned =
            [ "File"; "Process"; "Exec"; "Shell"; "Eval"
              "Reflect"; "Load"; "Environment"; "GetType"
              "Assembly"; "Spawn"; "System"; "Import"; "Require" ]
        let names =
            StdlibRegistry.getAll () |> List.ofSeq
            |> List.map (fun f -> f.Name)
        let violations =
            names
            |> List.filter (fun n ->
                banned |> List.exists (fun b -> n.StartsWith(b, StringComparison.Ordinal)))
        Assert.True(
            List.isEmpty violations,
            sprintf
                "Stdlib exposes escape-surface names: %A\nThis test is a ratchet: if you registered one of these deliberately, update SandboxSecurity.fs."
                violations)

    // -- E. Limit honesty (FsCheck sweep) ----------------------------------

    [<FsCheck.Xunit.Property>]
    let ``MaxStatements N on a bomb trips at the statement cap`` () =
        Prop.forAll (Arb.fromGen genLimitN) (fun n ->
            let opts = tightOpts n Int32.MaxValue 64 5_000
            // Bomb is generously larger than N so the cap is always inside.
            let src = bombOfSize (n + 50)
            let r, elapsed = runMeasured opts src (emptyInput ())
            expectCode MessageCode.StatementLimitExceeded r
            wallBound 2_000 elapsed
            true)

    [<FsCheck.Xunit.Property>]
    let ``MaxLoopIterations N on an infinite loop trips at the loop cap`` () =
        Prop.forAll (Arb.fromGen genLimitN) (fun n ->
            // Statements budget must be generous enough to not win the race.
            let opts = tightOpts Int32.MaxValue n 64 5_000
            let src = "while true do\n  var x = 1\nend"
            let r, elapsed = runMeasured opts src (emptyInput ())
            expectAnyCode
                [ MessageCode.LoopIterationLimitExceeded
                  MessageCode.StatementLimitExceeded ]
                r
            wallBound 2_000 elapsed
            true)

    [<FsCheck.Xunit.Property>]
    let ``MaxCallDepth N on unbounded recursion trips at the depth cap`` () =
        // Use `genMutualRecursionDepth` (caps at 96) rather than `genLimitN`
        // (caps at 256). The CLR stack ceiling sits just above 128 per
        // CallDepthMeasurement - sweeping above that crashes the test host.
        Prop.forAll (Arb.fromGen genMutualRecursionDepth) (fun n ->
            let opts = tightOpts Int32.MaxValue Int32.MaxValue n 5_000
            let src =
                """
func recurse(x)
  return recurse(x + 1)
end
recurse(0)"""
            let r, elapsed = runMeasured opts src (emptyInput ())
            expectCode MessageCode.CallDepthLimitExceeded r
            wallBound 2_000 elapsed
            true)

    // -- F. Cancellation propagation ---------------------------------------

    [<Fact>]
    let ``while-true with Sleep cancels under 100ms budget within wall bound`` () =
        let opts = tightOpts Int32.MaxValue Int32.MaxValue 64 100
        let src =
            """
while true do
  Sleep(10)
end"""
        let r, elapsed = runMeasured opts src (emptyInput ())
        expectCode MessageCode.ExecutionTimeLimitExceeded r
        // 90.12.3 (JYRO-REQ-377): when the limit ends during Sleep, Execute()
        // returns no later than L + 500 ms after its call; L = 100 ms here.
        wallBound (100 + 500) elapsed

    // -- F'. The same bound with the thread pool saturated -------------------
    // JYRO-DEF-698: the case above passes on an idle pool and failed on a busy
    // runner (4,114 ms for L = 100 ms). JYRO-ADR-168 point 4: a time bound of
    // 90.12 is also tested while the process's thread pool is saturated, its
    // workers blocked by the test itself, so the load is not left to chance.

    /// The process's thread pool held saturated: its worker count capped at
    /// what it has (no fewer than the processors), every worker blocked by this
    /// test and work still queued behind them, so no thread-pool work item or
    /// timer callback can run until it is released. A dedicated thread (not a
    /// pool one) releases the workers after `release` at the latest, so a run
    /// that waits for a pool worker comes back late instead of never. Dispose
    /// restores the cap and releases the workers. Uncapped, the pool adds a
    /// worker about every 500 ms while starved, which on main let this case end
    /// at the bound itself (581 ms for 600) instead of showing the fault.
    type internal SaturatedPool(release: TimeSpan) =
        let gate = new ManualResetEventSlim(false)
        let started = ref 0
        let maxWorkers, maxIo = ThreadPool.GetMaxThreads()
        let restore () =
            ThreadPool.SetMaxThreads(maxWorkers, maxIo) |> ignore
            gate.Set()
        do
            let minWorkers, _ = ThreadPool.GetMinThreads()
            let cap = List.max [ ThreadPool.ThreadCount; minWorkers; Environment.ProcessorCount ]
            if not (ThreadPool.SetMaxThreads(cap, maxIo)) then
                failwithf "The thread pool's workers could not be capped at %d" cap
            for _ in 1 .. cap + 256 do
                ThreadPool.QueueUserWorkItem(WaitCallback(fun _ ->
                    Interlocked.Increment(&started.contents) |> ignore
                    gate.Wait())) |> ignore
            Thread(ThreadStart(fun () -> if not (gate.Wait release) then gate.Set()), IsBackground = true).Start()
            // Saturated when no further blocker has started for 50 ms and work is
            // still queued: every worker the pool has is held.
            let sw = Stopwatch.StartNew()
            let mutable last = -1
            let mutable settled = false
            while not settled && sw.Elapsed < TimeSpan.FromSeconds 10.0 do
                Thread.Sleep 50
                let now = Volatile.Read(&started.contents)
                settled <- now > 0 && now = last && ThreadPool.PendingWorkItemCount > 0L
                last <- now
            if not settled then
                restore ()
                failwithf "The thread pool could not be saturated in 10 s (%d blockers started)" last
        /// True while every worker is still held and work is still queued.
        member _.IsSaturated = not gate.IsSet && ThreadPool.PendingWorkItemCount > 0L
        // The gate is never disposed: a blocker not yet started must still be
        // able to wait on it, and finds it set.
        interface IDisposable with
            member _.Dispose() = restore ()

    /// Saturating the pool would slow every test running beside it, so these
    /// run alone.
    [<CollectionDefinition("SaturatedPool", DisableParallelization = true)>]
    type SaturatedPoolCollection() = class end

    [<Collection("SaturatedPool")>]
    type ``Sleep under a time limit with the thread pool saturated``(output: Xunit.Abstractions.ITestOutputHelper) =

        /// 90.12.3 (JYRO-REQ-377): Execute() returns no later than L + 500 ms
        /// after its call; L = 100 ms. The program is compiled and the runner set
        /// up before the pool is saturated; the time is Execute() alone.
        [<Fact>]
        member _.``JYRO-DEF-698 while-true with Sleep under a 100ms budget returns within L + 500 ms with the thread pool saturated`` () =
            let opts = tightOpts Int32.MaxValue Int32.MaxValue 64 100
            let program =
                match JyroCompiler().WithStdlib().WithSource("while true do\n  Sleep(10)\nend").Compile() with
                | Produced p -> p
                | Failed msgs -> failwithf "The script did not compile: %A" (List.ofSeq msgs |> List.map (fun m -> m.Message))
                | _ -> failwith "The script did not compile"
            let runner = JyroRunner(program).WithExecutionOptions(opts).WithInput(emptyInput ())
            use pool = new SaturatedPool(TimeSpan.FromSeconds 5.0)
            let sw = Stopwatch.StartNew()
            let r = runner.Execute()
            sw.Stop()
            let saturated = pool.IsSaturated
            output.WriteLine(sprintf "Execute() returned %.1f ms after its call, L = 100 ms, pool saturated throughout: %b" sw.Elapsed.TotalMilliseconds saturated)
            expectCode MessageCode.ExecutionTimeLimitExceeded r
            wallBound (100 + 500) sw.Elapsed
            Assert.True(saturated, "The thread pool was released before Execute() returned, so the run was not measured under saturation")

    // -- G. Input belongs to the host --------------------------------------
    // `Input` is read-only, and the check must root the assignment target
    // rather than match its outermost shape. Matching only `Input.x` and
    // `Input[i]` refused the first level and admitted every level below it,
    // so `Input.a.b = 1` wrote straight into the host's own object graph -
    // which the host is entitled to hand to another execution concurrently.

    /// Host-owned graph: `{ a: { k: "host-owned" }, list: [1, 2, 3] }`,
    /// returning the inner object so a test can assert the host's copy was
    /// left alone.
    let private hostGraph () =
        let root = JyroObject()
        let inner = JyroObject()
        inner.SetProperty("k", JyroString("host-owned"))
        root.SetProperty("a", inner)
        let list = JyroArray()
        for n in [ 1.0; 2.0; 3.0 ] do list.Add(JyroNumber(n))
        root.SetProperty("list", list)
        root, inner

    [<Theory>]
    [<InlineData("Input = 1")>]
    [<InlineData("Input.k = 'MUTATED'")>]
    [<InlineData("Input.a.k = 'MUTATED'")>]
    [<InlineData("Input.a.b.c.d = 'MUTATED'")>]
    [<InlineData("Input['a']['k'] = 'MUTATED'")>]
    [<InlineData("Input.a['k'] = 'MUTATED'")>]
    [<InlineData("Input['a'].k = 'MUTATED'")>]
    [<InlineData("Input.a.k += 'MUTATED'")>]
    [<InlineData("delete Input.a")>]
    [<InlineData("delete Input.a.k")>]
    [<InlineData("delete Input['a']['k']")>]
    let ``a write rooted at Input is refused at any depth`` (src: string) =
        let root, inner = hostGraph ()
        let opts = JyroExecutionOptions.Default
        let r, _ = runMeasured opts src (root :> JyroValue)
        expectCode MessageCode.InputIsReadOnly r
        Assert.Equal("host-owned", (inner.GetProperty("k")).ToStringValue())

    [<Theory>]
    [<InlineData("var t = Input.a\nt.k = 'MUTATED'")>]
    [<InlineData("var t = Input\nt.a.k = 'MUTATED'")>]
    [<InlineData("var t = Input.a\ndelete t.k")>]
    [<InlineData("var t = Input.a\nt['k'] = 'MUTATED'")>]
    [<InlineData("func f(o)\n  o.k = 'MUTATED'\nend\nf(Input.a)")>]
    [<InlineData("var t = Input.list\nt[0] = 99")>]
    [<InlineData("var t = Input.list\nEach(t, i => 1)\nt[1] = 99")>]
    let ``a write reached through an alias of Input is refused at runtime`` (src: string) =
        // The validator refuses a write whose TARGET is rooted at Input, but it
        // is a syntactic check and values are referenced rather than copied:
        // `var t = Input.a` hands the script the host's own object with nothing
        // left to root, and passing it to a function launders it further. Only
        // the value itself can refuse these, which is why the runtime freezes
        // Input before the script runs.
        let root, inner = hostGraph ()
        let r, _ = runMeasured JyroExecutionOptions.Default src (root :> JyroValue)
        expectCode MessageCode.InputValueIsReadOnly r
        Assert.Equal("host-owned", (inner.GetProperty("k")).ToStringValue())

    [<Fact>]
    let ``the Input write error names Input rather than a bare read-only value`` () =
        // The message states what failed and stops. The remedy and the
        // reference-semantics explanation belong in the documentation.
        let root, _ = hostGraph ()
        let r, _ = runMeasured JyroExecutionOptions.Default
                        "var t = Input.a\nt.k = 'MUTATED'" (root :> JyroValue)
        Assert.Equal("Cannot write to Input.", (List.ofSeq r.Messages |> List.ofSeq |> List.head).Message)

    [<Fact>]
    let ``Clone gives a script a modifiable copy of Input`` () =
        // The way through has to actually work, and the copy must be deep
        // enough to modify nested values.
        let root, inner = hostGraph ()
        let src = "var t = Clone(Input.a)\nt.k = 'MINE'\nOutput.v = t.k"
        let r, _ = runMeasured JyroExecutionOptions.Default src (root :> JyroValue)
        Assert.True(r.IsSuccess, sprintf "codes: %A" (codes r))
        Assert.Equal("host-owned", (inner.GetProperty("k")).ToStringValue())

    [<Fact>]
    let ``a host holding the input graph sees it frozen after execution`` () =
        // Freezing Input mutates the host's own value, so this is a real part
        // of the contract rather than an implementation detail: WithInput hands
        // the graph over, and the host cannot modify it afterwards either.
        let root, inner = hostGraph ()
        let r, _ = runMeasured JyroExecutionOptions.Default "Output.v = Input.a.k" (root :> JyroValue)
        Assert.True(r.IsSuccess)
        Assert.True(root.IsFrozen)
        Assert.True(inner.IsFrozen)
        Assert.Throws<JyroRuntimeException>(fun () -> inner.SetProperty("k", JyroString("host"))) |> ignore

    [<Fact>]
    let ``a scalar Input is accepted rather than treated as unfreezable`` () =
        let r, _ = runMeasured JyroExecutionOptions.Default "Output.v = Input" (JyroNumber(42.0) :> JyroValue)
        Assert.True(r.IsSuccess, sprintf "codes: %A" (codes r))

    [<Fact>]
    let ``Output remains writable`` () =
        let root, _ = hostGraph ()
        let r, _ = runMeasured JyroExecutionOptions.Default "Output.x = Input.a.k" (root :> JyroValue)
        Assert.True(r.IsSuccess, sprintf "codes: %A" (codes r))

    // -- An index write cannot allocate without bound ----------------------
    // The resource limits of Part 100 bound how many operations a script
    // performs - statements, loop iterations, call depth. None of them bounds
    // how much a single operation allocates, and the array index setter used
    // to pad with nulls up to any index, so `arr[50000000] = 1` allocated
    // fifty million slots from one statement. Measured before the fix: six
    // statements retained 1.5 GB, inside every default limit, while the same
    // array built by appending tripped the 1,000-iteration budget in 57 ms.

    let private runDefault (source: string) =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource(source))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))

    [<Theory>]
    [<InlineData("Output.a = []\nOutput.a[1000000] = 1")>]
    [<InlineData("Output.a = []\nOutput.a[50000000] = 1")>]
    [<InlineData("Output.a = [1,2,3]\nOutput.a[100] = 9")>]
    [<InlineData("Output.a = []\nOutput.a[1] = 1")>]
    let ``an index write beyond the append point is refused`` (source: string) =
        let r = runDefault source
        Assert.False(r.IsSuccess, "a sparse index write must not allocate")
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.IndexOutOfRange)

    [<Fact>]
    let ``a negative index write is refused rather than discarded`` () =
        // Previously a silent no-op, so the write was lost with no diagnostic.
        let r = runDefault "Output.a = [1,2,3]\nOutput.a[-1] = 9"
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.IndexOutOfRange)

    [<Theory>]
    [<InlineData("Output.a = []\nOutput.a[0] = 7", "[7]")>]
    [<InlineData("Output.a = [1,2,3]\nOutput.a[3] = 4", "[1,2,3,4]")>]
    [<InlineData("Output.a = [1,2,3]\nOutput.a[1] = 99", "[1,99,3]")>]
    [<InlineData("Output.a = [1,2,3]\nOutput.a[2] = 99", "[1,2,99]")>]
    let ``writing in range or exactly one past the end still works`` (source: string) (expected: string) =
        // Appending at `Length` is the growth idiom scripts rely on; bounding
        // the setter must not cost them that.
        let r = runDefault source
        Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
        Assert.Equal(expected, (Runner.prop "a" r.TryValue.Value).ToJson())

    [<Fact>]
    let ``building an array by index is bounded by the loop budget`` () =
        // The property the fix restores: growing an array costs one operation
        // per element, and operations are what the limits already bound.
        let r = runDefault "Output.a = []\nvar i = 0\nwhile i < 100000 do\n  Output.a[i] = 1\n  i = i + 1\nend"
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.LoopIterationLimitExceeded)

    [<Fact>]
    let ``reading out of range still yields null`` () =
        // Deliberately unchanged. The read side is a separate finding: an
        // out-of-range read is indistinguishable from a stored null.
        let r = runDefault "Output.a = [1,2,3]\nOutput.v = Output.a[100] is null"
        Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
        Assert.True(Runner.prop "v" r.TryValue.Value |> Runner.asBool)

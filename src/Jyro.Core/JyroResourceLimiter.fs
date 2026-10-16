namespace RealTree.Jyro

open System.Collections.Generic
open System.Diagnostics
open System.Threading

// One run's execution deadline, held by `ExecutionDeadlines` until it comes
// due or the run ends. `Deadline` is a `Stopwatch.GetTimestamp()` value;
// `Id` orders two deadlines that fall on the same tick. `State` and
// `Deadline` are read and written only under the scheduler's lock;
// `Deadline` is the schedule's sort key, so it changes only while the
// deadline is off the schedule (a hand-off that failed, put back to retry).
[<Sealed; AllowNullLiteral>]
type internal ExecutionDeadline(deadline: int64, id: int64, source: CancellationTokenSource) =
    member val Deadline = deadline with get, set
    member _.Id = id
    member _.Source = source
    // 0 waiting, 1 taken off the schedule and handed to a canceller, 2 the
    // run ended while it was being canceled (the canceller disposes the
    // source), 3 canceled.
    member val State = 0 with get, set

// An engine canceller thread's mailbox: the due deadline handed to it, and
// the kernel event that wakes it. `Item` is read and written only under
// `ExecutionDeadlines`'s canceller lock.
[<Sealed; AllowNullLiteral>]
type internal DeadlineCanceller() =
    member val Item : ExecutionDeadline = null with get, set
    member val Signal = new AutoResetEvent(false)

// Cancels each run's token source when its execution time limit comes due
// (JYRO-ADR-168 point 3, as amended by JYRO-ADR-170).
//
// This replaces `CancellationTokenSource.CancelAfter`, whose timer callback
// .NET runs as a work item on the shared thread pool: when the host's pool
// was busy, the callback - and so the end of every wait on the limiter's
// token, `Sleep` and host functions alike - came seconds after the limit
// (JYRO-DEF-698).
//
// Two kinds of engine thread do the work, neither of them a pool thread,
// each a background thread started with `UnsafeStart` so it carries no
// caller's ExecutionContext and does not keep the process alive:
//
// - One deadline thread per process, started on first use and never
//   stopped. It waits with `Monitor.Wait` and a timeout, a kernel timed
//   wait, reads the high-resolution `Stopwatch` clock, and only takes due
//   deadlines off the schedule. It never calls `Cancel()`, so it runs no
//   callback registered on any run's token (ADR-170 point 1).
// - Canceller threads. The deadline thread hands each due source to an idle
//   canceller, or starts a new one when none is idle (each is inside a
//   `Cancel()` whose callbacks have not returned); when no thread can be
//   started, the deadline goes back on the schedule and the hand-off is
//   retried after a short backoff. The canceller calls
//   `Cancel()`, which runs the token's callbacks on it. A callback that
//   blocks holds its own canceller until it returns and delays no other
//   run's limit; a callback that throws is ignored. An idle canceller waits
//   on its own event; idle cancellers beyond one exit after
//   `idleTimeoutMs` (ADR-170 point 2).
[<AbstractClass; Sealed>]
type internal ExecutionDeadlines private () =
    static let gate = obj ()
    // The canceller threads' lock and their idle list, apart from `gate`,
    // the schedule's lock.
    static let cancelGate = obj ()
    static let idle = ResizeArray<DeadlineCanceller>()
    static let idleTimeoutMs = 30000
    static let pending =
        SortedSet<ExecutionDeadline>(
            { new IComparer<ExecutionDeadline> with
                member _.Compare(a, b) =
                    let c = compare a.Deadline b.Deadline
                    if c <> 0 then c else compare a.Id b.Id })
    static let mutable nextId = 0L
    static let mutable thread : Thread | null = null

    // The milliseconds from `now` until `deadline`, rounded up and at least
    // 1, so the wait never ends before the deadline by a rounding and never
    // spins on a timeout of 0.
    static let millisecondsUntil (deadline: int64) (now: int64) : int =
        let ms = System.Math.Ceiling(float (deadline - now) * 1000.0 / float Stopwatch.Frequency)
        if ms < 1.0 then 1
        elif ms >= float System.Int32.MaxValue then System.Int32.MaxValue - 1
        else int ms

    // Cancels one due source on the calling canceller thread, then settles
    // who disposes it: this thread when its run ended meanwhile (State 2),
    // the run's `Stop` otherwise.
    static let cancel (d: ExecutionDeadline) =
        try d.Source.Cancel() with _ -> ()
        let dispose =
            lock gate (fun () ->
                if d.State = 2 then true
                else
                    d.State <- 3
                    false)
        if dispose then
            try d.Source.Dispose() with _ -> ()

    // A canceller thread's loop: cancel the source it was started or woken
    // with, then wait idle on its own event for the next. Only the timeout
    // of that wait lets it exit, and only while another canceller is idle,
    // so one idle canceller always remains once any has run.
    static let cancellerLoop (self: DeadlineCanceller) (first: ExecutionDeadline) () =
        let item = ref first
        while not (isNull item.Value) do
            try cancel item.Value with _ -> ()
            item.Value <- null
            lock cancelGate (fun () -> idle.Add self)
            let waiting = ref true
            while waiting.Value do
                let signaled = self.Signal.WaitOne(idleTimeoutMs)
                lock cancelGate (fun () ->
                    if not (isNull self.Item) then
                        // Handed a source: the deadline thread took this
                        // canceller off the idle list when it handed it.
                        item.Value <- self.Item
                        self.Item <- null
                        waiting.Value <- false
                    elif not signaled && idle.Count > 1 then
                        idle.Remove self |> ignore
                        waiting.Value <- false)
        self.Signal.Dispose()

    // Hands a due source to an idle canceller, or starts a new canceller
    // with it when none is idle. Called by the deadline thread outside
    // `gate`; it never waits for a callback. Throws when no thread can be
    // started, having released the canceller it built.
    static let hand (d: ExecutionDeadline) =
        lock cancelGate (fun () ->
            if idle.Count > 0 then
                let c = idle.[idle.Count - 1]
                idle.RemoveAt(idle.Count - 1)
                c.Item <- d
                c.Signal.Set() |> ignore
            else
                let c = DeadlineCanceller()
                try
                    let t = Thread(ThreadStart(cancellerLoop c d), IsBackground = true, Name = "Jyro execution canceller")
                    t.UnsafeStart()
                with _ ->
                    c.Signal.Dispose()
                    reraise ())

    // The first and longest delay before a failed hand-off is tried again,
    // in Stopwatch ticks: 1 ms, doubling on each failure up to about 50 ms.
    static let firstRetryTicks = max 1L (Stopwatch.Frequency / 1000L)
    static let maxRetryTicks = max 1L (Stopwatch.Frequency / 20L)

    static let run () =
        let due = ResizeArray<ExecutionDeadline>()
        let retryTicks = ref firstRetryTicks
        while true do
            due.Clear()
            lock gate (fun () ->
                while due.Count = 0 do
                    if pending.Count = 0 then
                        Monitor.Wait(gate) |> ignore
                    else
                        let now = Stopwatch.GetTimestamp()
                        let first = pending.Min
                        if first.Deadline <= now then
                            while pending.Count > 0 && pending.Min.Deadline <= now do
                                let d = pending.Min
                                pending.Remove(d) |> ignore
                                d.State <- 1
                                due.Add d
                        else
                            Monitor.Wait(gate, millisecondsUntil first.Deadline now) |> ignore)
            // Handed on, never canceled here (JYRO-ADR-170 point 1), not
            // even when no canceller thread can be started (the process is
            // out of threads or memory): the deadline is then put back on the
            // schedule, due again after a delay that doubles from 1 ms to
            // about 50 ms, and handed again on that round. Meanwhile the run
            // itself still ends at its limit through the Stopwatch checks of
            // `CheckExecutionLimits` and Sleep's bounded wait; only a wait on
            // the token is late, for as long as no thread can be started.
            for d in due do
                let handed =
                    try
                        hand d
                        true
                    with _ -> false
                if handed then
                    retryTicks.Value <- firstRetryTicks
                else
                    let disposeNow =
                        lock gate (fun () ->
                            if d.State = 2 then
                                // The run ended meanwhile: nothing is left to
                                // cancel, and Dispose runs no token callbacks.
                                true
                            else
                                // Off the schedule since it came due, so its
                                // sort key may change before it is re-added.
                                d.State <- 0
                                d.Deadline <- Stopwatch.GetTimestamp() + retryTicks.Value
                                pending.Add(d) |> ignore
                                false)
                    retryTicks.Value <- min maxRetryTicks (retryTicks.Value * 2L)
                    if disposeNow then
                        try d.Source.Dispose() with _ -> ()

    // Cancels `source` once `Stopwatch.GetTimestamp()` reaches `deadline`.
    static member Register(deadline: int64, source: CancellationTokenSource) : ExecutionDeadline =
        lock gate (fun () ->
            if isNull thread then
                let t = Thread(ThreadStart(run), IsBackground = true, Name = "Jyro execution deadlines")
                // Unsafe: the thread outlives the run that starts it, so it
                // must not carry that caller's ExecutionContext (AsyncLocal
                // values, culture flow) for the life of the process.
                t.UnsafeStart()
                thread <- t
            nextId <- nextId + 1L
            let d = ExecutionDeadline(deadline, nextId, source)
            pending.Add(d) |> ignore
            Monitor.Pulse(gate)
            d)

    // Withdraws a deadline whose run has ended. Returns whether the caller
    // may dispose the source now; false when it has been taken off the
    // schedule to be canceled and that has not finished, in which case the
    // canceller thread disposes it once `Cancel()` returns.
    static member Unregister(d: ExecutionDeadline) : bool =
        lock gate (fun () ->
            match d.State with
            | 0 ->
                pending.Remove(d) |> ignore
                true
            | 1 ->
                d.State <- 2
                false
            | _ -> true)

// Enforces runtime resource quotas and execution limits to protect against
// runaway scripts and ensure system stability during Jyro script execution.
// `AllowNullLiteral` because `JyroExecutionContext.Limiter` reports "no
// limiter" as null rather than as an F# option - see the note there.
//
// Internal (JYRO-ADR-191 item 3): a host that could stop the limiter of a
// run would end its time limit. A host reads the counters of a run from
// `JyroPipelineStats`.
[<Sealed; AllowNullLiteral>]
type internal JyroResourceLimiter(rawOptions: JyroExecutionOptions, externalToken: CancellationToken) =
    // Clamp MaxCallDepth at the CLR-safe ceiling. A host that configures
    // above `MaxSafeCallDepth` (or passes `Unlimited` pre-clamp) would
    // otherwise let a recursive script stack-overflow the process before
    // the Jyro guard fires - a sandbox escape. Silent clamp keeps existing
    // hosts working; the documented contract is "sandbox always holds".
    // Options are a resource the host supplies, not a value, so a null is
    // refused here rather than dereferenced on the next line.
    do if obj.ReferenceEquals(rawOptions, null) then
        raise (System.ArgumentNullException(nameof rawOptions))

    // A count limit left at zero is one the host did not set (options bound
    // from configuration or built with an object initializer), and takes its
    // 100.1 default (JYRO-REQ-1405, JYRO-ADR-181). MaxExecutionTime is taken
    // as given. `WithDefaultsForUnset` makes a copy, which the host's later
    // changes to its own options do not reach.
    let options =
        let copy = JyroExecutionOptions.WithDefaultsForUnset rawOptions
        if copy.MaxCallDepth > JyroExecutionOptions.MaxSafeCallDepth then
            { copy with MaxCallDepth = JyroExecutionOptions.MaxSafeCallDepth }
        else
            copy
    let stopwatch = Stopwatch()
    let externalCt = externalToken
    let mutable cts: CancellationTokenSource option = None
    let mutable deadline : ExecutionDeadline = null
    // 64-bit, and the counters and the comparisons both are.
    //
    // They were `int`, incremented with F#'s default unchecked arithmetic, so
    // past Int32.MaxValue they wrapped to Int32.MinValue: the `> MaxStatements`
    // test stopped firing, the run continued unbounded, and the host-visible
    // counters surfaced into `JyroPipelineStats` read as large negative
    // numbers. Measured at 2,200,000,000 statements: tripped = false, count =
    // -2094967296, no diagnostic.
    //
    // Only reachable once a limit has been raised or removed - but
    // `JyroExecutionOptions.Unlimited` sets both to Int32.MaxValue, which is
    // exactly the configuration that reaches it, and with an `int` counter the
    // test against that limit could never fire at all. A 64-bit counter cannot
    // wrap inside any process lifetime: at a billion statements a second it
    // would take close to three hundred years.
    let mutable statementCount = 0L
    let mutable loopIterations = 0L

    // Overloaded rather than written with an optional `externalToken`, which
    // F# emits as a required `FSharpOption<CancellationToken>`.
    new(rawOptions: JyroExecutionOptions) =
        JyroResourceLimiter(rawOptions, CancellationToken.None)

    // The effective execution options this limiter enforces, after clamping MaxCallDepth to MaxSafeCallDepth.
    member _.Options = options
    // Number of statements counted so far via CheckAndCountStatement.
    member _.StatementCount = statementCount
    // Cumulative loop iterations counted so far via CheckAndEnterLoop.
    member _.LoopIterations = loopIterations
    // Wall-clock time elapsed since Start() was called.
    member _.ElapsedTime = stopwatch.Elapsed

    // Cancellation token that fires when MaxExecutionTime is reached or the
    // host cancels.
    //
    // Neither needs a thread-pool thread (JYRO-ADR-168, JYRO-ADR-170): the
    // time limit is fired by the engine's deadline and canceller threads
    // (`ExecutionDeadlines`), the callbacks of one run never delaying
    // another's, and the
    // host's cancellation reaches this token inline, on the thread that
    // cancels the host's token. A function that blocks on this token, or on
    // its `WaitHandle`, is therefore woken on time even when the host's
    // thread pool is saturated.
    member _.CancellationToken =
        match cts with
        | Some c -> c.Token
        | None -> externalCt

    // Start the execution timer and create the cancellation token source,
    // due to cancel when the run has spent `MaxExecutionTime`.
    member _.Start() =
        stopwatch.Start()
        let newCts =
            if externalCt.CanBeCanceled then
                CancellationTokenSource.CreateLinkedTokenSource(externalCt)
            else
                new CancellationTokenSource()
        if options.MaxExecutionTime < System.TimeSpan.MaxValue then
            if System.OperatingSystem.IsBrowser() then
                // A single-threaded WebAssembly runtime cannot start the
                // deadline thread. Its timer callbacks cannot run while a
                // script holds the only thread either, so this token never
                // stops a run there; the Stopwatch checks of
                // `CheckExecutionLimits` do.
                newCts.CancelAfter(options.MaxExecutionTime)
            else
                // The rest of the budget on this limiter's own clock, from
                // now: a limiter started again resumes its Stopwatch.
                let left = options.MaxExecutionTime - stopwatch.Elapsed
                let leftTicks =
                    if left <= System.TimeSpan.Zero then 0L
                    else int64 (System.Math.Ceiling(left.TotalSeconds * float Stopwatch.Frequency))
                // Register starts the deadline thread on the process's first
                // run, which can fail (out of threads or memory). The new
                // source, linked to the host's token, is then disposed before
                // the failure goes on, so nothing stays registered on the
                // host's token (JYRO-DEF-716); the next Register tries the
                // start again.
                try
                    deadline <- ExecutionDeadlines.Register(Stopwatch.GetTimestamp() + leftTicks, newCts)
                with _ ->
                    newCts.Dispose()
                    reraise ()
        cts <- Some newCts

    // Stop the execution timer, withdraw the deadline and dispose the
    // cancellation token source.
    member _.Stop() =
        stopwatch.Stop()
        let mayDispose =
            if isNull deadline then true
            else
                let d = deadline
                deadline <- null
                ExecutionDeadlines.Unregister d
        match cts with
        | Some c ->
            if mayDispose then c.Dispose()
            cts <- None
        | None -> ()

    // Whether the *host* has canceled this run.
    //
    // Deliberately the token the host handed us, not `CancellationToken` above
    // - that one also fires at `MaxExecutionTime`, so it fires for both
    // causes and cannot tell them apart. Exhausting a time budget and
    // being canceled are different things to report, and the second is the
    // one a host cannot work out for itself from the result.
    member _.HostCancellationRequested = externalCt.IsCancellationRequested

    // Check execution time limit
    member _.CheckExecutionTime() =
        if stopwatch.Elapsed > options.MaxExecutionTime then
            JyroError.raiseRuntime MessageCode.ExecutionTimeLimitExceeded [| box (int options.MaxExecutionTime.TotalMilliseconds) |]

    // The whole of what is checked at an evaluation point that does not count
    // against a budget: the time limit, then host cancellation.
    //
    // Time comes first so that a run which reached its budget is reported as
    // having reached its budget. The limiter's token cancels itself at
    // `MaxExecutionTime`, so the two conditions are true together at the end
    // of a timed-out run, and testing cancellation first would relabel every
    // timeout as a host cancellation.
    member this.CheckExecutionLimits() =
        this.CheckExecutionTime()
        if externalCt.IsCancellationRequested then
            JyroError.raiseRuntime MessageCode.CanceledByHost [||]

    // Check and count a statement execution
    member this.CheckAndCountStatement() =
        statementCount <- statementCount + 1L
        if statementCount > int64 options.MaxStatements then
            JyroError.raiseRuntime MessageCode.StatementLimitExceeded [| box options.MaxStatements |]
        this.CheckExecutionLimits()

    // Check and count a loop iteration (cumulative across all loops)
    member this.CheckAndEnterLoop() =
        loopIterations <- loopIterations + 1L
        if loopIterations > int64 options.MaxLoopIterations then
            JyroError.raiseRuntime MessageCode.LoopIterationLimitExceeded [| box options.MaxLoopIterations |]
        this.CheckExecutionLimits()

    // NOTE: call-depth tracking deliberately does NOT live here. It is held on
    // `JyroExecutionContext` (`CheckAndEnterCall` / `ExitCall` / `CallDepth` /
    // `PeakCallDepth`), which is what the compiler binds into the emitted
    // expression trees and what enforces `options.MaxCallDepth`. A duplicate
    // counter on the limiter would never be incremented, so a host reading it
    // would always see 0 - do not reinstate one here.

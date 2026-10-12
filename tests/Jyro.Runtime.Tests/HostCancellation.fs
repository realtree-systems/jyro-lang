module RealTree.Jyro.Tests.HostCancellation

open System
open System.Diagnostics
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Tests

// A host-supplied cancellation token was observed only by the stdlib.
//
// `WithCancellationToken` is documented as the way a host stops a run in
// flight, and the engine honored it from inside iterating stdlib bodies -
// `Map` over a large array, `Sum`, `GroupBy` - because each of those calls
// `ctx.CheckExecutionLimits()` per element. Compiled script code called
// neither that member nor anything that read the token: the statement and loop
// hooks the emitted expression trees do call, `CheckAndCountStatement` and
// `CheckAndEnterLoop`, both ended at `CheckExecutionTime`, which compared the
// stopwatch against `MaxExecutionTime` and nothing else.
//
// So a script whose hot loop was pure Jyro ran to completion and reported
// success, however long ago the host had canceled. Whether cancellation
// worked depended on whether the script happened to call a library function
// that iterates - which is not something a host can know in advance about a
// script it did not write.
//
// The token check now sits in `JyroResourceLimiter.CheckExecutionLimits`,
// which both hooks call, so the two evaluation points the specification names
// are the two the engine checks.
//
// **Time is tested before cancellation** at every one of these sites, and the
// token consulted is the host's own rather than the limiter's linked one. The
// limiter links the host token with a `CancelAfter(MaxExecutionTime)`, so
// reading the linked token would report a run that exhausted its time budget
// as one the host had canceled - the wrong cause, and the one a host is least
// able to check for itself.

/// A loop long enough that finishing it is unmistakably distinct from stopping
/// early, with the statement and iteration budgets lifted so that the only
/// thing which can stop it is time or the token.
let private longLoop = "var i = 0\nwhile i < 30000000 do\n  i = i + 1\nend\nOutput.i = i"

let private unlimitedBut (time: TimeSpan) =
    { JyroExecutionOptions.Unlimited with MaxExecutionTime = time }

let private runWith (opts: JyroExecutionOptions) (token: CancellationToken) (source: string) =
    Runner.compileThenRun
        (JyroCompiler().WithStdlib().WithSource(source))
        (fun r ->
            r.WithInput(JyroObject() :> JyroValue)
             .WithExecutionOptions(opts)
             .WithCancellationToken(token))

let private codes (r: JyroResult<JyroValue>) =
    r.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq

// -- The statement and loop hooks -------------------------------------------

[<Fact>]
let ``an already-canceled token stops a pure Jyro loop`` () =
    // Canceled before the run starts, so no timing assumption is involved:
    // the first statement checkpoint must already refuse to proceed.
    use cts = new CancellationTokenSource()
    cts.Cancel()
    let r = runWith (unlimitedBut (TimeSpan.FromMinutes 2.0)) cts.Token longLoop
    Assert.False(r.IsSuccess, "A canceled run reported success")
    Assert.Equal<MessageCode list>([ MessageCode.CanceledByHost ], codes r)

[<Fact>]
let ``a token canceled mid-run stops a pure Jyro loop`` () =
    // The case the host actually writes: cancel while the script is running.
    // The assertion is on the outcome, not on a duration - a loaded build
    // agent can make any wall-clock bound flaky - but the time limit is set
    // far enough above the loop's own runtime that reaching it would report a
    // different code, so a pass here cannot be the timeout in disguise.
    //
    // The loop never ends by itself and the host cancels from a thread of its
    // own, so a cancel that arrives late (a loaded thread pool, a slow
    // compile) only stops the run later and cannot let it finish and report
    // success (JYRO-DEF-743). The five-minute limit is the backstop.
    use cts = new CancellationTokenSource()
    let canceller =
        Thread((fun () ->
                    Thread.Sleep 100
                    cts.Cancel()),
               IsBackground = true)
    canceller.Start()
    // The watch starts after compilation, when the runner is prepared
    // (floors-process 4.1.14).
    let watch = Stopwatch()
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("var i = 0\nwhile true do\n  i = i + 1\nend"))
            (fun runner ->
                watch.Start()
                runner.WithInput(JyroObject() :> JyroValue)
                      .WithExecutionOptions(unlimitedBut (TimeSpan.FromMinutes 5.0))
                      .WithCancellationToken(cts.Token))
    watch.Stop()
    Assert.False(r.IsSuccess, "A canceled run reported success")
    Assert.Equal<MessageCode list>([ MessageCode.CanceledByHost ], codes r)
    Assert.True(
        watch.Elapsed < TimeSpan.FromMinutes 5.0,
        sprintf "Stopped only at the time limit, after %O" watch.Elapsed)

[<Fact>]
let ``cancellation is observed inside a script-declared function`` () =
    // Call depth deliberately does not check time or the token, so a loop
    // inside a function body is only covered by the statement and loop hooks
    // reaching there too.
    let source =
        String.concat "\n"
            [ "func spin()"
              "  var i = 0"
              "  while i < 30000000 do"
              "    i = i + 1"
              "  end"
              "  return i"
              "end"
              "Output.i = spin()" ]
    use cts = new CancellationTokenSource()
    cts.Cancel()
    let r = runWith (unlimitedBut (TimeSpan.FromMinutes 2.0)) cts.Token source
    Assert.False(r.IsSuccess, "A canceled run reported success")
    Assert.Equal<MessageCode list>([ MessageCode.CanceledByHost ], codes r)

[<Fact>]
let ``an uncanceled token leaves an ordinary run alone`` () =
    use cts = new CancellationTokenSource()
    let r = runWith JyroExecutionOptions.Default cts.Token "Output.v = 1 + 1"
    Assert.True(r.IsSuccess, sprintf "%A" (codes r))

// -- Time and cancellation are different answers ----------------------------

[<Fact>]
let ``exhausting the time budget reports the time limit, not cancellation`` () =
    // The limiter links the host's token with a CancelAfter(MaxExecutionTime),
    // so the linked token fires for both causes. Reading it instead of the
    // host's own would report every timed-out run as host-canceled.
    use cts = new CancellationTokenSource()
    let r = runWith (unlimitedBut (TimeSpan.FromMilliseconds 50.0)) cts.Token longLoop
    Assert.False(r.IsSuccess, "A run past its time budget reported success")
    Assert.Equal<MessageCode list>([ MessageCode.ExecutionTimeLimitExceeded ], codes r)

[<Fact>]
let ``the time limit is reported even when no token was supplied`` () =
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource(longLoop))
            (fun r ->
                r.WithInput(JyroObject() :> JyroValue)
                 .WithExecutionOptions(unlimitedBut (TimeSpan.FromMilliseconds 50.0)))
    Assert.False(r.IsSuccess)
    Assert.Equal<MessageCode list>([ MessageCode.ExecutionTimeLimitExceeded ], codes r)

// -- Sleep, the one site that already observed the token --------------------

[<Fact>]
let ``Sleep reports host cancellation as cancellation`` () =
    // `Sleep` passes the token to `Task.Delay` and catches the
    // OperationCanceledException, which is right - but it then reported every
    // one of them as ExecutionTimeLimitExceeded, quoting a budget the run had
    // not come close to spending. Under a five-minute limit, a run canceled
    // after 100ms was told it had "exceeded maximum time limit of 300000ms".
    use cts = new CancellationTokenSource()
    //
    // The cancel comes from a thread of its own and the Sleep is far longer than
    // any late cancel (JYRO-DEF-745); the five-minute limit is the backstop.
    Thread((fun () ->
                Thread.Sleep 100
                cts.Cancel()),
           IsBackground = true).Start()
    let r = runWith (unlimitedBut (TimeSpan.FromMinutes 5.0)) cts.Token "Sleep(600000)"
    Assert.False(r.IsSuccess, "A canceled Sleep reported success")
    Assert.Equal<MessageCode list>([ MessageCode.CanceledByHost ], codes r)

[<Fact>]
let ``Sleep still reports the time limit when the budget elapses`` () =
    // The control for the test above: same handler, other cause.
    use cts = new CancellationTokenSource()
    let r = runWith (unlimitedBut (TimeSpan.FromMilliseconds 100.0)) cts.Token "Sleep(30000)"
    Assert.False(r.IsSuccess)
    Assert.Equal<MessageCode list>([ MessageCode.ExecutionTimeLimitExceeded ], codes r)
    Assert.Contains("100", (Seq.head r.Messages).Message)

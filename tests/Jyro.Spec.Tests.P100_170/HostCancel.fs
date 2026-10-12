/// How a test delivers a host's cancellation (JYRO-DEF-745, after JYRO-DEF-743).
///
/// `CancellationTokenSource.CancelAfter` runs its callback as a thread-pool work item, so a busy
/// pool delivers the cancel late; a workload of fixed length can then end first, or overrun a
/// bound, on a product that observes the token correctly. A test that asserts an outcome of a host
/// cancel MUST cancel from a thread of its own, against a workload that cannot end before the
/// cancel, with a backstop (a time limit) that is far beyond the cancel.
module RealTree.Jyro.Spec.HostCancel

open System
open System.Threading

/// A workload length (Spin iterations, Sleep milliseconds) that no run reaches before a cancel of
/// a few hundred milliseconds, however late the cancel comes, while the backstop time limit of the
/// test ends the run if the token is not observed at all.
[<Literal>]
let unboundedSpin = 2_000_000_000

/// Sleep(n) milliseconds long enough to outlast any late cancel; the test's time limit is the
/// backstop.
[<Literal>]
let unboundedSleepMs = 600_000

/// Start a background thread of its own that waits `delay` and cancels `cts`. Call it immediately
/// before the run it is to cancel (after the compile). The thread does not depend on the
/// thread pool; a cancel made after `cts` is disposed is ignored.
let cancelFromThread (cts: CancellationTokenSource) (delay: TimeSpan) : unit =
    let t =
        Thread(
            (fun () ->
                Thread.Sleep delay
                try cts.Cancel() with :? ObjectDisposedException -> ()),
            IsBackground = true)
    t.Start()

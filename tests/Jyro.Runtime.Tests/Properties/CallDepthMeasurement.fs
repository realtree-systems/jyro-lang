namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// Measurement tests for how deep a Jyro call chain can go before
/// .NET's own stack runs out. A well-behaved runtime should trip
/// `CallDepthLimitExceeded` BEFORE a CLR stack overflow.
///
/// If any test here aborts the test run with "Test host process crashed:
/// Stack overflow", the depth in the test name is over the current
/// ceiling - lower the default `MaxCallDepth` in `JyroExecutionOptions`
/// or reduce per-call CLR frame cost further.
module CallDepthMeasurement =

    let private recurseAt (depth: int) =
        let src =
            """
func recurse(n)
  return recurse(n + 1)
end
recurse(0)"""
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource(src))
            (fun r -> r.WithMaxCallDepth(depth).WithInput(JyroObject() :> JyroValue))

    let private expectGuardTrips (depth: int) =
        let r = recurseAt depth
        Assert.False(r.IsSuccess, sprintf "Expected limit to trip at depth %d" depth)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.CallDepthLimitExceeded)

    // These two depths anchor the measured ceiling. Pre-refactor, even 64
    // blew the CLR stack because each Jyro call cost ~5-6 CLR frames. After
    // inlining the depth check, skipping the interface hop for user
    // functions, and dropping the try/finally, the safe ceiling sits just
    // above 128. `MaxSafeCallDepth` is pinned at 96 in `JyroExecutionOptions`
    // - the limiter silently clamps higher values, so 96 is the deepest
    // configurable depth. Any regression below 96 means the per-call frame
    // cost has crept back up.
    [<Fact>]
    let ``depth 64 trips cleanly (default MaxCallDepth)`` () = expectGuardTrips 64

    [<Fact>]
    let ``depth 96 trips cleanly (MaxSafeCallDepth)`` () = expectGuardTrips 96

    [<Fact>]
    let ``MaxCallDepth above MaxSafeCallDepth is clamped`` () =
        // Configuring 10_000 would stack-overflow the CLR if honored.
        // The limiter clamps to MaxSafeCallDepth (96) - the guard still
        // trips cleanly instead of crashing the host.
        let r = recurseAt 10_000
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.CallDepthLimitExceeded)

    // -- A context with no limiter is still stack-safe ---------------------
    // `JyroExecutionContext()` used to fall back to `Int32.MaxValue` for
    // MaxCallDepth, so any limiter-free execution path - `Compiler.executeSimple`,
    // or a host building its own context - could be driven into an uncatchable
    // CLR StackOverflowException that killed the process. Measured before the
    // fix: "Stack overflow." on stderr, exit code 127. Every other limit is
    // recoverable, so opting out of them is a policy choice; this one is not.

    let private compiledRecursion () =
        let src =
            """
func recurse(n)
  return recurse(n + 1)
end
recurse(0)"""
        match Compiler.compileFromSource src (StdlibRegistry.getAll ()) []
                  FunctionEffect.Mutating FunctionEffect.NonDeterministic with
        | Produced program -> program
        | r -> failwithf "probe source failed to compile: %A" r.Messages

    [<Fact>]
    let ``executeSimple bounds call depth despite having no limiter`` () =
        let r = Compiler.executeSimple (compiledRecursion ()) (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.CallDepthLimitExceeded)

    [<Fact>]
    let ``a host-built context with no limiter bounds call depth`` () =
        let ctx = JyroExecutionContext()
        let r = Compiler.execute (compiledRecursion ()) (JyroObject() :> JyroValue) ctx
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.CallDepthLimitExceeded)
        // The bound applied is the safe ceiling, not some larger number that
        // merely happened to survive this particular recursion.
        Assert.Equal(JyroExecutionOptions.MaxSafeCallDepth + 1, ctx.PeakCallDepth)

    // -- Call depth is tracked on the context, not the limiter ------------
    // `JyroResourceLimiter` once carried its own `callDepth` field plus
    // `CheckAndEnterCall` / `ExitCall`; the context stopped delegating to them
    // when depth tracking moved onto the hot path, leaving a counter that was
    // never incremented and a `CallDepth` property that always read 0. Those
    // members are gone. These tests pin the surviving contract so a future
    // refactor cannot quietly route telemetry back through a dead counter.

    [<Fact>]
    let ``stats report the peak call depth reached`` () =
        let stats = JyroPipelineStats()
        let src =
            """
func down(n)
  if n <= 0 then
    return 0
  end
  return down(n - 1)
end
Output.r = down(10)"""
        let r =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().WithSource(src))
                (fun r -> r.WithStats(stats).WithInput(JyroObject() :> JyroValue))
        Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
        // 11 nested `down` calls; the peak must be recorded, not the unwound
        // live counter (which is back at 0 by the time Execute returns).
        Assert.Equal(11, stats.PeakCallDepth)
        Assert.True(stats.StatementCount > 0, "statement counter should be surfaced too")

    /// A host function that records the live and the peak call depth the context it
    /// receives reads (JYRO-REQ-1873).
    type private DepthProbe(seen: System.Collections.Generic.List<struct (int * int)>) =
        interface IJyroFunction with
            member _.Name = "Probe"
            member _.Effect = FunctionEffect.Pure
            member _.Signature =
                JyroFunctionSignature.Create(ParameterType.Number)
            member _.Execute(_, ctx) =
                seen.Add(struct (ctx.CallDepth, ctx.PeakCallDepth))
                JyroNumber(1.0) :> JyroValue

    // The former test of this place drove a context built with a limiter through
    // CheckAndEnterCall and ExitCall. Those members are not public any more (JYRO-REQ-1873),
    // so the same behavior, live depth unwinding while the peak is kept, is read through a run.
    [<Fact>]
    let ``context tracks live and peak call depth independently`` () =
        let seen = System.Collections.Generic.List<struct (int * int)>()
        let src =
            """
func inner()
  return Probe()
end
func outer()
  return inner()
end
var a = outer()
var b = Probe()
Output.r = 1"""
        let r =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().AddFunction(DepthProbe(seen)).WithSource(src))
                (fun r -> r.WithInput(JyroObject() :> JyroValue))
        Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))
        Assert.Equal(2, seen.Count)
        let struct (deepLive, deepPeak) = seen.[0]
        let struct (shallowLive, shallowPeak) = seen.[1]
        // In the nested call the live depth is the peak so far; after the calls unwind it is
        // lower, and the high-water mark is retained.
        Assert.Equal(deepLive, deepPeak)
        Assert.True(shallowLive < shallowPeak, sprintf "live %d, peak %d" shallowLive shallowPeak)
        Assert.True(deepLive > shallowLive)
        Assert.Equal(deepPeak, shallowPeak)

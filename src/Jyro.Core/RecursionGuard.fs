namespace RealTree.Jyro

open System

// Bounds native recursion over the value graph - structural equality,
// cloning, freezing, JSON/string/host serialization, flatten, and the
// inbound host converters (`FromDictionary`, `FromEnumerable`,
// `FromComplexObject`, `FromJsonElement`) - against unbounded depth (spec
// 100.7, JYRO-REQ-1433, 1435). JSON text is not walked here: `ReadDocument`
// reads it iteratively and counts its depth itself.
//
// A script can trivially build a cyclic reference (`a.self = a`, since
// assignment shares references) or a pathologically deep structure. A naive
// recursive walk of such a value overflows the CLR stack with an
// UNCATCHABLE StackOverflowException, which crashes the host process and
// defeats the sandbox - the very thing MaxCallDepth exists to prevent, but
// MaxCallDepth only covers Jyro-level calls, not these in-engine walks. This
// guard trips a catchable JyroRuntimeException at a fixed nesting depth
// instead: it is the value-graph analog of MaxCallDepth.
//
// Depth is tracked per-thread. Execution within a single run is
// single-threaded and every guarded traversal is synchronous, so a plain
// thread-static counter is correct; concurrent runs on other threads keep
// their own counters. `Enter()` checks BEFORE it increments, so a tripped
// guard leaves the counter unchanged - callers pair every successful
// `Enter()` with an `Exit()` in a `finally`, and the frame that trips never
// entered its `try`, so the counter always unwinds back to its start value.
//
// Internal (JYRO-ADR-191 item 5): a host cannot enter or leave a level of a
// walk, which would poison every later walk on its thread. The maximum it
// applies is documented with `ValueNestingLimitExceeded` and in the package
// README, not read from here.
[<AbstractClass; Sealed>]
type internal ValueRecursionGuard =

    [<ThreadStatic; DefaultValue>]
    static val mutable private depth : int

    // Maximum value-graph nesting depth.
    //
    // A guarded walk runs on the SAME caller thread as script execution
    // (only compilation gets JyroCompiler's 16 MiB worker), so its frames sit
    // ON TOP of whatever Jyro call-depth frames are already on the stack - a
    // value walk can be triggered from the bottom of a near-`MaxSafeCallDepth`
    // (96) Jyro recursion. The two budgets are therefore additive against one
    // physical stack, and this cap is sized to leave room for the call stack
    // beneath it: a full 96-deep Jyro call chain (~5-6 CLR frames each) plus
    // 128 value-walk frames stays clear of the ~128-Jyro-call CLR ceiling that
    // `CallDepthMeasurement` pins on the smallest supported (WASM) stack.
    //
    // It is the one maximum nesting depth of spec 100.7.1, applied to every
    // structural walk of a value, JSON text read into a value included: the
    // engine's JSON readers carry no depth of their own, so text deeper than
    // this raises `ValueNestingLimitExceeded`, not `InvalidJson`. A host
    // cannot raise, lower or remove it (spec 100.7.2), and it is documented
    // in the package README. `CyclicValueProtection` measures both the standalone and the
    // combined-with-call-depth ceilings; if either test aborts the run with a
    // stack overflow, this value is too high for the combined budget and must
    // be lowered (the same contract `CallDepthMeasurement` uses).
    static member MaxDepth = 128

    // Enters one level of value-graph recursion. Raises
    // `ValueNestingLimitExceeded` (catchable) once the depth cap is reached.
    // MUST be paired with `Exit()` in a `finally` on the success path.
    static member Enter() : unit =
        if ValueRecursionGuard.depth >= ValueRecursionGuard.MaxDepth then
            JyroError.raiseRuntime MessageCode.ValueNestingLimitExceeded
                [| box ValueRecursionGuard.MaxDepth |]
        ValueRecursionGuard.depth <- ValueRecursionGuard.depth + 1

    // Leaves one level of value-graph recursion.
    static member Exit() : unit =
        ValueRecursionGuard.depth <- ValueRecursionGuard.depth - 1

// Detects a back-reference while reflecting over a host object graph.
//
// `JyroObject.FromComplexObject` walks a host type's properties, and a host
// object that points at itself - directly, or around a longer loop - has no
// representation as a tree. `ValueRecursionGuard` alone does not save that
// walk: it would recurse all the way to the depth cap, and a reflection frame
// is far heavier than a value-walk frame, so the CLR stack runs out first.
// Measured, when `FromObject` was first routed here: an uncatchable
// StackOverflowException that aborted the process rather than a diagnostic.
//
// Detecting the back-reference stops the walk at the point it would repeat,
// before the heavy reflection frames pile up: `FromObject` raises
// `ValueNestingLimitExceeded` there (spec 100.7.5, 100.7.6), as it would for
// a cyclic dictionary or list, because a walk that never ends exceeds the
// maximum nesting depth. The repeated object is neither expanded nor rendered
// as its text form. Tracked per-thread for the same reason the depth counter
// is.
type internal HostGraphCycleGuard =

    [<ThreadStatic; DefaultValue>]
    static val mutable private visiting : System.Collections.Generic.HashSet<obj>

    // True when `value` is already being reflected further up this walk.
    static member IsVisiting(value: obj) : bool =
        not (isNull (box HostGraphCycleGuard.visiting))
        && not (obj.ReferenceEquals(value, null))
        && HostGraphCycleGuard.visiting.Contains(value)

    // MUST be paired with `Exit` in a `finally`.
    static member Enter(value: obj) : unit =
        if isNull (box HostGraphCycleGuard.visiting) then
            HostGraphCycleGuard.visiting <-
                System.Collections.Generic.HashSet<obj>(HashIdentity.Reference)
        HostGraphCycleGuard.visiting.Add(value) |> ignore

    // Removes `value` from the visiting set - the counterpart to `Enter`, called from the same `finally`.
    static member Exit(value: obj) : unit =
        if not (isNull (box HostGraphCycleGuard.visiting)) then
            HostGraphCycleGuard.visiting.Remove(value) |> ignore

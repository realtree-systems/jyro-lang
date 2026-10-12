namespace RealTree.Jyro.Tests.Runtime

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// A script can build a cyclic reference (`a.self = a`, since assignment
/// shares references) or a pathologically deep value. The engine's native
/// walks over the value graph - structural equality (`==`/`!=`, `switch`,
/// Distinct, IndexOf, Diff), Clone, Flatten, Freeze, and JSON/string/host
/// serialization - would then recurse until the CLR stack overflows with an
/// UNCATCHABLE StackOverflowException, crashing the host and defeating the
/// sandbox.
///
/// `ValueRecursionGuard` bounds those walks: they must ALWAYS terminate via a
/// catchable `ValueNestingLimitExceeded` failure, never a CLR crash, never a
/// hang. If any test here aborts the run with "Test host process crashed:
/// Stack overflow", the guard regressed (or `ValueRecursionGuard.MaxDepth` is
/// above the host stack ceiling - possibly only once STACKED on a deep Jyro
/// call chain, see the combined test) - the analog of `CallDepthMeasurement`.
module CyclicValueProtection =

    // -- helpers ------------------------------------------------------------

    /// The value nesting maximum of the reference implementation (JYRO-REQ-2042 states it: 128).
    /// The guard that holds it is not public.
    let private valueNestingMaximum = 128

    let private codes(r: JyroResult<JyroValue>) : MessageCode list =
        List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)

    /// Runs a script (default limits) and asserts it fails with the value
    /// nesting guard - and, by not crashing the host, that the guard fired.
    let private expectGuardTrips (source: string) =
        let sw = Stopwatch.StartNew()
        let r = Runner.runEmpty source
        sw.Stop()
        Assert.False(r.IsSuccess, sprintf "Expected failure, got success.\nSource:\n%s" source)
        Assert.Contains(codes r, fun c -> c = MessageCode.ValueNestingLimitExceeded)
        // A guarded walk aborts in O(MaxDepth); it must never spin.
        Assert.True(sw.Elapsed.TotalMilliseconds < 2_000.0,
            sprintf "Guarded abort took %.0fms" sw.Elapsed.TotalMilliseconds)

    /// Script that assembles an acyclic object nested `depth` levels deep in
    /// variable `a`, then runs `useExpr`.
    let private deepChain (depth: int) (useExpr: string) : string =
        sprintf
            "var a = {}\nvar b = {}\nvar i = 0\nwhile i < %d do\n  b = {}\n  b.child = a\n  a = b\n  i = i + 1\nend\n%s"
            depth useExpr

    /// Builds a deep acyclic JyroObject directly (host side), `depth` nested.
    let private buildDeepObject (depth: int) : JyroObject =
        let mutable current = JyroObject()
        for _ in 1 .. depth do
            let parent = JyroObject()
            parent.SetProperty("child", current)
            current <- parent
        current

    // -- A. Cyclic references trip cleanly on every native walk -------------

    [<Fact>]
    let ``reflexive == on a cyclic object does not crash`` () =
        expectGuardTrips "Output.self = Output\nOutput.eq = Output == Output"

    [<Fact>]
    let ``== between two distinct cyclic objects does not crash`` () =
        // Distinct references, so a reference-equality short-circuit would not
        // save us - only the depth guard does.
        expectGuardTrips
            "var a = {}\na.self = a\nvar b = {}\nb.self = b\nOutput.eq = a == b"

    [<Fact>]
    let ``transitive (indirect) cycle a-b-a does not crash`` () =
        // The cycle closes through a second node, so no single-node self-check
        // catches it - the depth guard must.
        expectGuardTrips
            "var a = {}\nvar b = {}\na.next = b\nb.next = a\nOutput.c = Clone(a)"

    [<Fact>]
    let ``Clone of a cyclic object does not crash`` () =
        expectGuardTrips "var a = {}\na.self = a\nOutput.c = Clone(a)"

    [<Fact>]
    let ``Distinct over cyclic elements does not crash`` () =
        expectGuardTrips "var a = {}\na.self = a\nOutput.d = Distinct([a, a])"

    [<Fact>]
    let ``IndexOf over a cyclic element does not crash`` () =
        expectGuardTrips "var a = {}\na.self = a\nOutput.i = IndexOf([a], a)"

    [<Fact>]
    let ``ToJson of a cyclic object does not crash`` () =
        expectGuardTrips "var a = {}\na.self = a\nOutput.j = ToJson(a)"

    [<Fact>]
    let ``ToString of a cyclic object does not crash`` () =
        expectGuardTrips "var a = {}\na.self = a\nOutput.s = ToString(a)"

    [<Fact>]
    let ``Diff over structurally-matching cyclic field values does not crash`` () =
        // Diff compares field values with EqualsValue. Both fields must have the
        // same shape (a self-cyclic object) so the comparison descends into the
        // cycle rather than short-circuiting on a key/count mismatch.
        expectGuardTrips
            ("var a = {}\na.self = a\nvar b = {}\nb.self = b\n"
             + "var x = {}\nx.k = a\nvar y = {}\ny.k = b\nOutput.d = Diff(x, y)")

    // -- A'. Same, but through the ARRAY code path (distinct from objects) ---

    [<Fact>]
    let ``Flatten of a cyclic array does not crash`` () =
        expectGuardTrips "var a = []\na[0] = a\nOutput.f = Flatten(a)"

    [<Fact>]
    let ``ToJson of a cyclic array does not crash`` () =
        expectGuardTrips "var a = []\na[0] = a\nOutput.j = ToJson(a)"

    [<Fact>]
    let ``ToString of a cyclic array does not crash`` () =
        expectGuardTrips "var a = []\na[0] = a\nOutput.s = ToString(a)"

    [<Fact>]
    let ``== between two distinct cyclic arrays does not crash`` () =
        expectGuardTrips
            "var a = []\na[0] = a\nvar b = []\nb[0] = b\nOutput.eq = a == b"

    // -- B. Deep-but-acyclic values: bounded, not crashed ------------------

    [<Fact>]
    let ``deep acyclic Clone below the cap succeeds`` () =
        // 64 levels is well under MaxDepth (128): the guard must NOT reject
        // legitimate nesting.
        let r = Runner.runEmpty (deepChain 64 "Output.r = Clone(a)")
        Assert.True(r.IsSuccess, sprintf "Expected success, got %A" (codes r))

    [<Fact>]
    let ``deep acyclic Clone above the cap trips cleanly`` () =
        // 400 nested levels (> MaxDepth) under default limits: the value guard
        // (not the loop/statement limit) must fire, and the host must survive
        // recursing MaxDepth levels before it does.
        expectGuardTrips (deepChain 400 "Output.r = Clone(a)")

    [<Fact>]
    let ``deep acyclic == of two independent chains above the cap trips cleanly`` () =
        // Build two SEPARATE deep chains and compare them: distinct references
        // and equal structure, so the comparison genuinely recurses (a future
        // reference-equality fast path could not mask this) and trips the guard.
        let src =
            "var a = {}\nvar ta = {}\nvar b = {}\nvar tb = {}\nvar i = 0\n"
            + "while i < 400 do\n  ta = {}\n  ta.child = a\n  a = ta\n"
            + "  tb = {}\n  tb.child = b\n  b = tb\n  i = i + 1\nend\nOutput.eq = a == b"
        expectGuardTrips src

    [<Fact>]
    let ``deep nesting is bounded even with loop and statement limits disabled`` () =
        // The sandbox guarantee must hold under JyroExecutionOptions.Unlimited:
        // with the loop/statement/time caps off, only the value guard stands
        // between a 5_000-level build+Clone and a native stack overflow.
        let opts =
            { JyroExecutionOptions.Unlimited with
                MaxExecutionTime = TimeSpan.FromSeconds(5.0) }
        let source = deepChain 5_000 "Output.r = Clone(a)"
        let sw = Stopwatch.StartNew()
        let r =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().WithSource(source))
                (fun r -> r.WithExecutionOptions(opts).WithInput(JyroObject() :> JyroValue))
        sw.Stop()
        Assert.False(r.IsSuccess)
        Assert.Contains(codes r, fun c -> c = MessageCode.ValueNestingLimitExceeded)
        Assert.True(sw.Elapsed.TotalMilliseconds < 3_000.0,
            sprintf "Bounded abort took %.0fms" sw.Elapsed.TotalMilliseconds)

    [<Fact>]
    let ``value walk stacked on a near-max Jyro call chain does not overflow`` () =
        // The value-guard budget and the Jyro call-depth budget are additive on
        // the SAME caller thread: a deep value walk triggered from the bottom of
        // a ~MaxCallDepth-deep recursion stacks both. MaxDepth is sized so this
        // combination trips the value guard cleanly instead of overflowing. If
        // this test aborts with "Stack overflow", MaxDepth is too high for the
        // combined budget (cf. CallDepthMeasurement).
        let src =
            "func deep(n)\n"
            + "  if n <= 0 then\n"
            + "    var a = {}\n    var b = {}\n    var i = 0\n"
            + "    while i < 1000 do\n      b = {}\n      b.child = a\n      a = b\n      i = i + 1\n    end\n"
            + "    return Clone(a)\n"
            + "  end\n"
            + "  return deep(n - 1)\n"
            + "end\n"
            + "Output.r = deep(90)"
        let opts =
            { JyroExecutionOptions.Default with
                MaxCallDepth = 96
                MaxLoopIterations = Int32.MaxValue
                MaxStatements = Int32.MaxValue }
        let r =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().WithSource(src))
                (fun r -> r.WithExecutionOptions(opts).WithInput(JyroObject() :> JyroValue))
        Assert.False(r.IsSuccess)
        Assert.Contains(codes r, fun c -> c = MessageCode.ValueNestingLimitExceeded)

    // -- C. Direct-API walks (host extraction / freezing) ------------------

    [<Fact>]
    let ``ToObjectValue on a cyclic object raises a catchable error`` () =
        // The host-extraction walk runs OUTSIDE CompiledProgram's try/with, so
        // it must throw a catchable JyroRuntimeException directly rather than
        // an uncatchable StackOverflowException.
        let o = JyroObject()
        o.SetProperty("self", o)
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> (o :> JyroValue).ToObjectValue() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

    [<Fact>]
    let ``ToObjectValue on a cyclic array raises a catchable error`` () =
        let a = JyroArray()
        a.Add(a)
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> (a :> JyroValue).ToObjectValue() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

    [<Fact>]
    let ``Freeze of a deep acyclic object raises rather than overflowing`` () =
        let deep = buildDeepObject 400
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> deep.Freeze() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

    [<Fact>]
    let ``ToDictionary on a cyclic object raises a catchable error`` () =
        // ToDictionary is the typed host-extraction entry point; it must give
        // the same catchable failure as ToObjectValue on a cycle.
        let o = JyroObject()
        o.SetProperty("self", o)
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> o.ToDictionary() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

    [<Fact>]
    let ``ToDictionary and ToObjectValue share one depth budget`` () =
        // ToDictionary used to restate ToObjectValue's walk without entering
        // ValueRecursionGuard, so it tolerated exactly one level more than
        // every other guarded entry point. Both must now trip at the same
        // depth: the two members are one conversion, not two.
        let justUnder = buildDeepObject (valueNestingMaximum - 1)
        (justUnder :> JyroValue).ToObjectValue() |> ignore
        justUnder.ToDictionary() |> ignore

        let atLimit = buildDeepObject valueNestingMaximum
        let viaObjectValue =
            Assert.Throws<JyroRuntimeException>(fun () -> (atLimit :> JyroValue).ToObjectValue() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, viaObjectValue.Code)
        let viaDictionary =
            Assert.Throws<JyroRuntimeException>(fun () -> atLimit.ToDictionary() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, viaDictionary.Code)

    [<Fact>]
    let ``Freeze of a cyclic object terminates via the in-progress flag`` () =
        // A revisited node is mid-freeze, so it returns immediately and the
        // cycle terminates without the guard tripping: Freeze must SUCCEED
        // here, not raise, and the node must end up frozen.
        let o = JyroObject()
        o.SetProperty("self", o)
        let frozen = o.Freeze()
        Assert.True(frozen.IsFrozen)

    [<Fact>]
    let ``Freeze of a two-node cycle freezes both`` () =
        let a = JyroObject()
        let b = JyroObject()
        a.SetProperty("b", b)
        b.SetProperty("a", a)
        a.Freeze() |> ignore
        Assert.True(a.IsFrozen)
        Assert.True(b.IsFrozen)

    // -- IsFrozen must never claim more than it delivers -------------------
    // Freeze marked a node frozen BEFORE descending. If the guard tripped
    // partway, the node stayed marked while its children were still writable,
    // and a retry short-circuited on that flag and returned success without
    // descending - so the graph claimed read-only, was not, and could not be
    // fixed. `frozen` is now set only after the descent completes.

    [<Fact>]
    let ``a failed deep Freeze does not mark the root frozen`` () =
        let deep = buildDeepObject 400
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> deep.Freeze() |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)
        // The whole point: IsFrozen must not lie about a subtree that is still
        // writable, and the write must still be allowed since nothing froze.
        Assert.False(deep.IsFrozen)
        deep.SetProperty("stillWritable", JyroNumber(1.0))

    [<Fact>]
    let ``retrying a failed deep Freeze raises again rather than reporting success`` () =
        let deep = buildDeepObject 400
        for _ in 1 .. 3 do
            let ex = Assert.Throws<JyroRuntimeException>(fun () -> deep.Freeze() |> ignore)
            Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)
            Assert.False(deep.IsFrozen)

    [<Fact>]
    let ``a failed deep Freeze does not mark a deep array root frozen`` () =
        let root = JyroArray()
        let mutable current = root
        for _ in 1 .. 400 do
            let next = JyroArray()
            current.Add(next)
            current <- next
        Assert.Throws<JyroRuntimeException>(fun () -> root.Freeze() |> ignore) |> ignore
        Assert.False(root.IsFrozen)

    [<Fact>]
    let ``IsFrozen implies the whole subtree is frozen`` () =
        let root = buildDeepObject (valueNestingMaximum - 2)
        root.Freeze() |> ignore
        Assert.True(root.IsFrozen)
        // Walk to the leaf and confirm every node along the way is frozen.
        let mutable current = root
        let mutable depth = 0
        while (current :> IReadOnlyDictionary<string, JyroValue>).ContainsKey("child") do
            Assert.True(current.IsFrozen, sprintf "node at depth %d not frozen" depth)
            current <- current.GetProperty("child") :?> JyroObject
            depth <- depth + 1
        Assert.True(current.IsFrozen, "leaf not frozen")
        Assert.Equal(valueNestingMaximum - 2, depth)

    [<Fact>]
    let ``a failed Freeze marks nothing - not even a completed sibling`` () =
        // Freeze is all-or-nothing. The collection pass walks the whole graph
        // and can raise; only then does the marking pass run, and it cannot
        // fail. So a graph too deep to freeze is left exactly as it was.
        //
        // This previously asserted the opposite - that a completed sibling
        // stayed frozen - which was the residual of the mark-as-you-descend
        // design. There is no unfreeze, so that residual was permanent.
        let root = JyroObject()
        let shallow = JyroObject()
        shallow.SetProperty("k", JyroString("v"))
        root.SetProperty("a_shallow", shallow)
        root.SetProperty("b_deep", buildDeepObject 400)
        Assert.Throws<JyroRuntimeException>(fun () -> root.Freeze() |> ignore) |> ignore
        Assert.False(root.IsFrozen)
        Assert.False(shallow.IsFrozen)
        // ...and the sibling is still writable, as an unfrozen value must be.
        shallow.SetProperty("k", JyroString("w"))

    [<Fact>]
    let ``IsFrozen implies the reachable graph is frozen even across a back-edge`` () =
        // The case the two-flag design could not hold. `b` completes its own
        // descent (its only child is the in-progress root) and used to mark
        // itself; a later sibling of root then aborted, leaving root unmarked
        // and writable while b claimed to be frozen.
        let root = JyroObject()
        let b = JyroObject()
        root.SetProperty("a_b", b)
        b.SetProperty("back", root)
        root.SetProperty("z_deep", buildDeepObject 400)
        Assert.Throws<JyroRuntimeException>(fun () -> root.Freeze() |> ignore) |> ignore
        Assert.False(root.IsFrozen)
        Assert.False(b.IsFrozen)

    [<Fact>]
    let ``a wrongly marked node cannot poison a later freeze of a new root`` () =
        // The compounding failure: because the old traversal short-circuited on
        // the frozen flag, a node marked by an earlier partial freeze stopped
        // the walk, and a fresh root containing it reported success while
        // unmarked containers stayed reachable below.
        let parent = JyroObject()
        let kid = JyroObject()
        let grandkid = JyroObject()
        kid.SetProperty("g", grandkid)
        parent.SetProperty("k", kid)
        kid.Freeze() |> ignore
        Assert.True(kid.IsFrozen)
        // A sibling added to the parent after the child was already frozen.
        let late = JyroObject()
        parent.SetProperty("late", late)
        parent.Freeze() |> ignore
        // The already-frozen child must not have terminated the traversal.
        Assert.True(parent.IsFrozen)
        Assert.True(grandkid.IsFrozen)
        Assert.True(late.IsFrozen)
        Assert.Throws<JyroRuntimeException>(fun () -> late.SetProperty("x", JyroNumber(1.0))) |> ignore

    // -- C2. FromObject of a plain object: the reflection-based walk --------
    // `FromObject` routes plain data objects to a reflection walk rather than
    // stringifying them, so it CAN recurse into itself, and the depth guard is
    // load-bearing rather than precautionary.
    //
    // The depth guard alone turned out not to be enough. Measured when the
    // routing was first added: a self-referencing POCO overflowed the CLR
    // stack and aborted the test host, because a reflection frame is far
    // heavier than a value-walk frame and 128 of them exhaust the stack before
    // the counter trips. `HostGraphCycleGuard` closes that by stopping the
    // walk where it would repeat instead of where it would run out.

    // These POCOs must be public: the walk reflects with the default
    // binding flags, which see only public instance properties, so a `private`
    // type would present no properties at all and the walk would be a no-op.

    /// A POCO whose only property is a host dictionary, so the reflection
    /// walk descends into the guarded dictionary path.
    type DictWrapper(payload: IDictionary<string, obj>) =
        member _.Payload = payload

    /// A POCO that points back at itself.
    type SelfReferencing() =
        member this.Self: SelfReferencing = this
        member _.Name = "node"

    let private buildDeepDictionary (depth: int) : IDictionary<string, obj> =
        let mutable current: IDictionary<string, obj> = Dictionary<string, obj>()
        for _ in 1 .. depth do
            let parent = Dictionary<string, obj>()
            parent.["child"] <- box current
            current <- parent
        current

    [<Fact>]
    let ``FromObject charges a nesting level for the POCO wrapper`` () =
        // The wrapper object IS a level of nesting and must cost one, exactly
        // as a dictionary or a sequence charges for itself.
        // Without the guard the wrapper was free, so a POCO-wrapped payload
        // tolerated one level more than the same payload passed directly.
        let payload = buildDeepDictionary (valueNestingMaximum - 1)
        JyroValue.FromObject(payload) |> ignore
        let ex =
            Assert.Throws<JyroRuntimeException>(fun () ->
                JyroValue.FromObject(DictWrapper(payload)) |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

    [<Fact>]
    let ``FromObject unwinds the guard after a trip`` () =
        let payload = buildDeepDictionary 400
        for _ in 1 .. 20 do
            Assert.Throws<JyroRuntimeException>(fun () ->
                JyroValue.FromObject(DictWrapper(payload)) |> ignore)
            |> ignore
        // A trivial conversion must still succeed - a per-trip leak in the
        // thread-static counter would eventually make legitimate walks raise.
        let shallow = Dictionary<string, obj>()
        shallow.["x"] <- box 1
        Assert.Equal(1, (JyroValue.FromObject(shallow) :?> JyroObject).Count)

    [<Fact>]
    let ``FromObject terminates on a self-referencing POCO`` () =
        // Conversion from a host value is a walk the maximum nesting depth
        // binds (JYRO-REQ-2047), and a cycle exceeds it (JYRO-REQ-2045), so a
        // self-referencing POCO is refused with ValueNestingLimitExceeded,
        // thrown, since no execution is in progress (JYRO-REQ-2046). It must
        // be refused cleanly: a stack overflow here would abort the run.
        let ex =
            Assert.Throws<JyroRuntimeException>(fun () ->
                JyroValue.FromObject(SelfReferencing()) |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

    // -- D. Guard bookkeeping ----------------------------------------------

    [<Fact>]
    let ``guard counter unwinds after a trip so later shallow walks still work`` () =
        // A tripped guard must leave the thread-static depth counter balanced;
        // otherwise a per-trip leak would eventually make legitimate shallow
        // walks falsely raise. Trip it repeatedly on one thread, then a trivial
        // walk must still succeed.
        for _ in 1 .. 50 do
            let r = Runner.runEmpty "var a = {}\na.self = a\nOutput.c = Clone(a)"
            Assert.False(r.IsSuccess)
        let ok = Runner.runEmpty "Output.r = Clone({ \"k\": [1, 2, 3] })"
        Assert.True(ok.IsSuccess, sprintf "Shallow walk failed after trips: %A" (codes ok))

    // -- E. Limit checks on the newly-guarded iteration functions ----------

    [<Fact>]
    let ``iteration functions honor cancellation over their input`` () =
        // Locks in the ctx.CheckExecutionLimits() additions to Merge / GroupBy /
        // Select / Sum: with an already-canceled token, each must abort with
        // CanceledByHost on the first element rather than run to completion.
        use cts = new CancellationTokenSource()
        cts.Cancel()
        let runCanceled (src: string) =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().WithSource(src))
                (fun r ->
                    r.WithInput(JyroObject() :> JyroValue)
                     .WithCancellationToken(cts.Token))
        for src in
            [ "Output.r = Merge([{\"a\": 1}, {\"b\": 2}])"
              "Output.r = GroupBy([{\"k\": 1}, {\"k\": 2}], \"k\")"
              "Output.r = Select([{\"k\": 1}, {\"k\": 2}], \"k\")"
              "Output.r = Sum([1, 2, 3])" ] do
            let r = runCanceled src
            Assert.False(r.IsSuccess, sprintf "Expected cancellation for: %s" src)
            Assert.Contains(codes r, fun c -> c = MessageCode.CanceledByHost)

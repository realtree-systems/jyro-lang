module RealTree.Jyro.Tests.CompileRunSplit

open System
open Xunit
open RealTree.Jyro

// The contract of the `JyroCompiler` / `JyroRunner` split.
//
// These exist because the question they answer used to have no answer in the
// code: when a program was handed from one `JyroBuilder` to another, whose
// limits applied? Measured then: the executing builder's, always - but nothing
// said so, and the members that had no effect on a given path were silently
// ignored rather than refused. The split makes most of that unrepresentable;
// what remains is pinned here.

let private script = "var i = 0\nwhile i < 100 do\n  i = i + 1\nend\nOutput.i = i"

let private codes (r: JyroResult<'a>) =
    List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)

// -- Limits belong to the run, not to the program --------------------------

[<Fact>]
let ``a compiled program carries no resource limit into the runner`` () =
    // The compile side has no resource-limit member to leak - that is enforced
    // by the type, not by this test. What is left to check is the consequence:
    // one program, two runners, two outcomes. The 100-iteration loop is inside
    // the default 1,000 and outside the tightened 5.
    let program = JyroCompiler().WithStdlib().WithSource(script).CompileOrThrow()

    let unrestricted = JyroRunner(program).WithInput(JyroObject()).Execute()
    Assert.True(unrestricted.IsSuccess, sprintf "%A" (codes unrestricted))

    // The same program, run by a stricter runner, trips. Same bytes, different
    // policy - which is the property the split exists to make obvious.
    let restricted =
        JyroRunner(program).WithInput(JyroObject()).WithMaxLoopIterations(5).Execute()
    Assert.False(restricted.IsSuccess)
    Assert.Contains(MessageCode.LoopIterationLimitExceeded, codes restricted)

[<Fact>]
let ``an artifact carries no resource limit either`` () =
    let bytes =
        match JyroCompiler().WithStdlib().WithSource(script).CompileToArtifact() with
        | Produced b -> b
        | r -> failwithf "compile to bytes failed: %A" (codes r)

    let loaded = JyroArtifactLoader(bytes).WithStdlib().Load().GetValueOrThrow()
    Assert.True((JyroRunner(loaded).WithInput(JyroObject()).Execute()).IsSuccess)

    let restricted =
        JyroRunner(loaded).WithInput(JyroObject()).WithMaxLoopIterations(5).Execute()
    Assert.Contains(MessageCode.LoopIterationLimitExceeded, codes restricted)

[<Fact>]
let ``a runner given no options runs at the documented defaults`` () =
    // The trap the old shape set: a host that hardened the compile step and
    // left the run step unconfigured got `JyroExecutionOptions.Default`, not
    // the stricter values it had asked for. That is still true - it is the
    // *only* correct answer once limits do not travel - so it is pinned rather
    // than left to be rediscovered.
    let program = JyroCompiler().WithStdlib().WithSource(script).CompileOrThrow()
    let stats = JyroPipelineStats()
    let r = JyroRunner(program).WithInput(JyroObject()).WithStats(stats).Execute()
    Assert.True(r.IsSuccess, sprintf "%A" (codes r))
    Assert.Equal(100L, stats.LoopIterations)

// -- A library cannot apply to an artifact: the loader has no member for one --------

let private validLibrary = "func Helper(x)\n    return x\nend"

// The former test of this section (a library configured for an artifact load is refused with
// ArtifactCannotUseLibraries) is dropped: JyroArtifactLoader has no member that adds a library
// script (JYRO-REQ-1896), so the combination cannot be written.

[<Fact>]
let ``a valid library configured for an artifact write is refused`` () =
    let r =
        JyroCompiler()
            .WithStdlib()
            .WithSource("Output.v = 1")
            .AddLibrarySource(validLibrary)
            .CompileToArtifact()
    Assert.False(r.IsSuccess)
    Assert.Contains(MessageCode.ArtifactCannotEncodeLibraries, codes r)

[<Fact>]
let ``a faulty library configured for an artifact write gets its own errors first`` () =
    // Unlike the load refusal above, the write refusal is a Compile diagnostic
    // (160.1.12), reported only once every earlier stage has run clean on every
    // source text (JYRO-ADR-120). A library that fails Validate stops the
    // compilation there, with its own diagnostic and not the refusal.
    let r =
        JyroCompiler()
            .WithStdlib()
            .WithSource("Output.v = 1")
            .AddLibrarySource("var notAFunction = 1")
            .CompileToArtifact()
    Assert.False(r.IsSuccess)
    let cs = codes r
    Assert.Contains(MessageCode.LibraryStatementForbidden, cs)
    Assert.DoesNotContain(MessageCode.ArtifactCannotEncodeLibraries, cs)
    let forbidden = List.ofSeq r.Messages |> List.filter (fun m -> m.Code = MessageCode.LibraryStatementForbidden)
    Assert.All(forbidden, fun m -> Assert.Equal("Validate", string m.Stage))

[<Fact>]
let ``a library parsed up front is refused on the same terms as a source one`` () =
    // `AddLibrary` takes an already-parsed `JyroLibrary`, so it can never
    // contribute a parse error - the flag has to be set by both entry points or
    // this path stays silent.
    let library =
        match JyroLibrary.Parse(validLibrary) with
        | Produced lib -> lib
        | r -> failwithf "library failed to parse: %A" (codes r)
    let r =
        JyroCompiler().WithStdlib().WithSource("Output.v = 1").AddLibrary(library).CompileToArtifact()
    Assert.False(r.IsSuccess)
    Assert.Contains(MessageCode.ArtifactCannotEncodeLibraries, codes r)

[<Fact>]
let ``a library still works on the path that can use one`` () =
    // The refusals above must not have made libraries unusable.
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output.v = Helper(41) + 1").AddLibrarySource(validLibrary))
            (fun r -> r.WithInput(JyroObject()))
    Assert.True(r.IsSuccess, sprintf "%A" (codes r))
    Assert.Equal(42.0, (r.TryValue.Value.GetProperty("v")).ToDouble())

// -- The effect ceiling is applied where it can be ------------------------

type private Blocker() =
    interface IJyroFunction with
        member _.Name = "Block"
        member _.Effect = FunctionEffect.Blocking
        member _.Signature =
            JyroFunctionSignature.Create(ParameterType.Number)
        member _.Execute(_, _) = JyroNumber(1.0) :> JyroValue

[<Fact>]
let ``the loading host's effect ceiling is applied to an artifact`` () =
    // An artifact records no ceiling, so the host loading it re-applies its
    // own. This is the one policy that genuinely crosses the artifact boundary
    // - by being re-decided rather than carried.
    let blocker = Blocker() :> IJyroFunction
    let bytes =
        match JyroCompiler().AddFunction(blocker).WithSource("Output.v = Block()").CompileToArtifact() with
        | Produced b -> b
        | r -> failwithf "compile to bytes failed: %A" (codes r)

    let strict =
        JyroArtifactLoader(bytes)
            .AddFunction(blocker)
            .WithMaxEffect(FunctionEffect.Pure)
            .Load()
    Assert.False(strict.IsSuccess)
    Assert.Contains(MessageCode.ArtifactEffectExceedsMax, codes strict)

[<Fact>]
let ``CompileToArtifact records the stages it runs`` () =
    // `WithStats` was inert on this path: `compileToJyrx` had no timed variant,
    // so a host collecting stats around an artifact build got zeros and no
    // indication why.
    let stats = JyroPipelineStats()
    let r =
        JyroCompiler().WithStdlib().WithSource("Output.v = 1").WithStats(stats).CompileToArtifact()
    Assert.True(r.IsSuccess, sprintf "%A" (codes r))
    Assert.True(stats.ParseTime > TimeSpan.Zero, "parse must be recorded")
    Assert.True(stats.ValidateTime > TimeSpan.Zero, "validate must be recorded")
    Assert.True(stats.LinkTime > TimeSpan.Zero, "link must be recorded")
    // The Compile stage runs on this path (JYRO-DEF-756), so its time is
    // recorded. Nothing runs, so Execute stays zero rather than being filled
    // with something plausible.
    Assert.True(stats.CompileTime > TimeSpan.Zero, "compile must be recorded")
    Assert.Equal(TimeSpan.Zero, stats.ExecuteTime)

[<Fact>]
let ``one collector spans compile and run`` () =
    let stats = JyroPipelineStats()
    let program =
        JyroCompiler().WithStdlib().WithSource("Output.v = 1").WithStats(stats).CompileOrThrow()
    let r = JyroRunner(program).WithInput(JyroObject()).WithStats(stats).Execute()
    Assert.True(r.IsSuccess, sprintf "%A" (codes r))
    Assert.True(stats.ParseTime > TimeSpan.Zero, "the compile half must be recorded")
    Assert.True(stats.ExecuteTime > TimeSpan.Zero, "the run half must be recorded")
    Assert.True(stats.TotalTime >= stats.ParseTime + stats.ExecuteTime)

[<Fact>]
let ``a runner refuses a null program at construction`` () =
    // C# can pass null. Without the guard it surfaces as a
    // NullReferenceException from inside Execute, far from the call that
    // caused it.
    Assert.Throws<ArgumentNullException>(fun () ->
        JyroRunner(Unchecked.defaultof<CompiledProgram>) |> ignore)
    |> ignore

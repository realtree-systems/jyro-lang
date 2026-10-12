/// What JyroCompiler and JyroArtifactLoader do after JYRO-ADR-199 and 201: the loader and what it shares with
/// the compiler (JYRO-REQ-1896), WithMaxCompilationTime and its refusals (1907), JyroLibrary.Parse bounded by a
/// compilation time limit (1912) and registering functions and looking them up without needless work (1935).
/// Written from the specification alone (FLOORS 4.1.1). The 5-second default is in S160_09_DefaultCompileLimit.fs.
module RealTree.Jyro.Spec.Tests.S160_09_CompilerAndLoader

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.S160_09_Reflect

let private asResult (r: JyroExecutionResult) : JyroResult<JyroValue> = upcast r

let private artifactOf (source: string) : byte[] =
    Script.valueOf (JyroCompiler().WithSource(source).WithStdlib().CompileToArtifact())

let private trimSource = "Output.x = Trim(\" a \")"

let private run (program: CompiledProgram) : JyroValue =
    Script.output (asResult (JyroRunner(program).WithInput(Values.emptyObj ()).Execute()))

// -- JYRO-REQ-1896: the loader --------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 a load with the standard library succeeds and its program sets Output.x to a`` () =
    let loaded = JyroArtifactLoader(artifactOf trimSource).WithStdlib().Load()
    Assert.True(loaded.IsSuccess, Script.describe loaded)
    Assert.Equal("{\"x\":\"a\"}", Values.toJson (run (Script.valueOf loaded)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 the same load without WithStdlib fails with UndefinedFunction naming Trim`` () =
    let loaded = JyroArtifactLoader(artifactOf trimSource).Load()
    Assert.False(loaded.IsSuccess)
    let d = loaded.Messages |> Seq.find (fun m -> m.Code = MessageCode.UndefinedFunction)
    Assert.Contains("Trim", [ for a in d.Args -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture) ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 CompileToArtifact of Output.x = 1 gives bytes that Load loads and runs`` () =
    let bytes = Script.valueOf (JyroCompiler().WithSource("Output.x = 1").CompileToArtifact())
    Assert.Equal("{\"x\":1}", Values.toJson (run (Script.valueOf (JyroArtifactLoader(bytes).Load()))))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 CompileToArtifact of a source that does not compile gives the diagnostics and no bytes`` () =
    let written = JyroCompiler().WithSource("var s: string").CompileToArtifact()
    Assert.False(written.IsSuccess)
    Assert.False(written.HasValue)
    Assert.Contains(written.Messages, fun m -> m.Code = MessageCode.TypedVariableNeedsInitializer)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 CompileToArtifact of a compilation that has a library gives ArtifactCannotEncodeLibraries`` () =
    let written =
        JyroCompiler().WithStdlib().AddLibrarySource("func L() return 1 end").WithSource("Output.x = L()").CompileToArtifact()
    Assert.False(written.IsSuccess)
    Assert.Contains(written.Messages, fun m -> m.Code = MessageCode.ArtifactCannotEncodeLibraries)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 each With and Add method of the loader returns the loader it was called on`` () =
    let loader = JyroArtifactLoader(artifactOf "Output.x = 1")
    let f = (Host.recorder "F").AsFunction
    Assert.Same(loader, loader.WithStdlib())
    Assert.Same(loader, loader.AddFunction(f))
    Assert.Same(loader, loader.AddFunctions([ f ]))
    Assert.Same(loader, loader.WithMaxEffect(FunctionEffect.Mutating))
    Assert.Same(loader, loader.WithMaxCompilationTime(TimeSpan.FromSeconds 30.))
    Assert.Same(loader, loader.WithStats(JyroPipelineStats()))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 AddFunction and AddFunctions register host functions for a load as they do on the compiler`` () =
    let make () = Host.func "Twice" [ Parameter.Required("x", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure (fun args -> args.Head)
    let bytes = Script.valueOf (JyroCompiler().WithSource("Output.x = Twice(1)").AddFunction((make ()).AsFunction).CompileToArtifact())
    let one = make ()
    let viaOne = Script.valueOf (JyroArtifactLoader(bytes).AddFunction(one.AsFunction).Load())
    Assert.Equal("{\"x\":1}", Values.toJson (run viaOne))
    Assert.Equal(1, one.CallCount)
    let many = make ()
    let viaMany = Script.valueOf (JyroArtifactLoader(bytes).AddFunctions([ many.AsFunction ]).Load())
    Assert.Equal("{\"x\":1}", Values.toJson (run viaMany))
    Assert.Equal(1, many.CallCount)
    // Without the function the load is refused as the compiler refuses an undefined function.
    let missing = JyroArtifactLoader(bytes).Load()
    Assert.False(missing.IsSuccess)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 WithMaxEffect on the loader refuses a function above the maximum as the load of an artifact does`` () =
    let bytes = Script.valueOf (JyroCompiler().WithStdlib().WithMaxEffect(FunctionEffect.Mutating).WithSource("Sleep(0)\nOutput.r = 1").CompileToArtifact())
    let refused = JyroArtifactLoader(bytes).WithStdlib().WithMaxEffect(FunctionEffect.Pure).Load()
    Assert.False(refused.IsSuccess)
    Assert.Contains(refused.Messages, fun m -> m.Code = MessageCode.ArtifactEffectExceedsMax)
    let accepted = JyroArtifactLoader(bytes).WithStdlib().WithMaxEffect(FunctionEffect.Mutating).Load()
    Assert.True(accepted.IsSuccess, Script.describe accepted)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 a load of bytes that are not an artifact fails through its result and does not throw`` () =
    let loaded = JyroArtifactLoader([| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy |]).Load()
    Assert.False(loaded.IsSuccess)
    Assert.False(loaded.HasValue)
    Assert.Contains(loaded.Messages, fun m -> m.Code = MessageCode.ArtifactInvalidMagic)

// -- JYRO-REQ-1907: WithMaxCompilationTime refuses a duration outside the range ---------------------------------

/// The same operations on the compiler and on the loader: set the limit, and run the stage that it bounds.
type private Builder =
    { Set: TimeSpan -> obj
      Build: unit -> JyroResult<CompiledProgram>
      Self: obj }

let private builderOf (source: string) (kind: string) : Builder =
    if kind = "compiler" then
        let c = JyroCompiler().WithSource(source)
        { Set = (fun t -> box (c.WithMaxCompilationTime t)); Build = (fun () -> c.Compile()); Self = box c }
    else
        let l = JyroArtifactLoader(artifactOf source)
        { Set = (fun t -> box (l.WithMaxCompilationTime t)); Build = (fun () -> l.Load()); Self = box l }

let private builder (kind: string) : Builder = builderOf "Output.x = 1" kind

/// A source whose compile and load take far longer than one tick, so that a limit of one tick ends them
/// whatever the speed of the machine (S100_10_TimeLimitRange does the same for the compiler).
let private longSource = Tests.Support.manyStatements 2_000

[<Theory>]
[<InlineData("compiler", 9_223_372_036_854_775_807L)>] // TimeSpan.MaxValue: no limit
[<InlineData("compiler", 1L)>]
[<InlineData("compiler", 10_000L)>]
[<InlineData("compiler", 50_000_000L)>]
[<InlineData("compiler", 864_000_000_000L)>]
[<InlineData("loader", 9_223_372_036_854_775_807L)>]
[<InlineData("loader", 1L)>]
[<InlineData("loader", 10_000L)>]
[<InlineData("loader", 50_000_000L)>]
[<InlineData("loader", 864_000_000_000L)>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 WithMaxCompilationTime accepts TimeSpan.MaxValue and every duration from one tick to 24 hours and returns the builder`` (kind: string, n: int64) =
    let b = builder kind
    Assert.Same(b.Self, b.Set(TimeSpan.FromTicks n))

[<Theory>]
[<InlineData("compiler", 0L)>]
[<InlineData("compiler", -1L)>]
[<InlineData("compiler", -10_000L)>] // Timeout.InfiniteTimeSpan, and a negative millisecond
[<InlineData("compiler", -50_000_000L)>]
[<InlineData("compiler", 864_000_000_001L)>]
[<InlineData("compiler", 25_920_000_000_000L)>] // 30 days
[<InlineData("compiler", 9_223_372_036_854_775_806L)>] // TimeSpan.MaxValue less a tick
[<InlineData("compiler", -9_223_372_036_854_775_808L)>] // TimeSpan.MinValue
[<InlineData("loader", 0L)>]
[<InlineData("loader", -1L)>]
[<InlineData("loader", -10_000L)>]
[<InlineData("loader", -50_000_000L)>]
[<InlineData("loader", 864_000_000_001L)>]
[<InlineData("loader", 25_920_000_000_000L)>]
[<InlineData("loader", 9_223_372_036_854_775_806L)>]
[<InlineData("loader", -9_223_372_036_854_775_808L)>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 WithMaxCompilationTime throws ArgumentOutOfRangeException named time for any other duration`` (kind: string, n: int64) =
    let b = builder kind
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> b.Set(TimeSpan.FromTicks n) |> ignore)
    Assert.Equal("time", thrown.ParamName)

[<Theory>]
[<InlineData("compiler")>]
[<InlineData("loader")>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 the infinite time span is refused by name`` (kind: string) =
    let b = builder kind
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> b.Set(Timeout.InfiniteTimeSpan) |> ignore)
    Assert.Equal("time", thrown.ParamName)

[<Theory>]
[<InlineData("compiler")>]
[<InlineData("loader")>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 after a refused call the builder keeps the limit it had and the build fails with CompilationTimeLimitExceeded`` (kind: string) =
    let b = builderOf longSource kind
    b.Set(TimeSpan.FromTicks 1L) |> ignore
    for refused in [ TimeSpan.Zero; TimeSpan.FromMilliseconds -1.; TimeSpan.FromDays 30. ] do
        Assert.Throws<ArgumentOutOfRangeException>(fun () -> b.Set refused |> ignore) |> ignore
    let result = b.Build()
    Assert.False(result.IsSuccess)
    Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.CompilationTimeLimitExceeded)

[<Theory>]
[<InlineData("compiler")>]
[<InlineData("loader")>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 an accepted limit does not fail a compilation because of its value`` (kind: string) =
    for limit in [ TimeSpan.MaxValue; TimeSpan.FromHours 24.; TimeSpan.FromSeconds 30. ] do
        let b = builder kind
        b.Set limit |> ignore
        let result = b.Build()
        Assert.True(result.IsSuccess, sprintf "with a limit of %O: %s" limit (Script.describe result))
        Assert.DoesNotContain(MessageCode.CompilationTimeLimitExceeded, Script.codes result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1907")>]
let ``JYRO-REQ-1907 the limit set on the compiler bounds CompileToArtifact too`` () =
    let compiler = JyroCompiler().WithSource(Tests.Support.manyStatements 20_000).WithMaxCompilationTime(TimeSpan.FromTicks 1L)
    let written = compiler.CompileToArtifact()
    Assert.False(written.IsSuccess)
    Assert.Contains(written.Messages, fun m -> m.Code = MessageCode.CompilationTimeLimitExceeded)

// -- JYRO-REQ-1912: JyroLibrary.Parse bounded by a compilation time limit -------------------------------------------

let private simpleLibrary = "func f() return 1 end"

let private slowLibrary : string =
    Tests.Support.lines ([ "func L()" ] @ Tests.Support.repeatLines 20_000 "  var a{i} = {i}" @ [ "  return 0"; "end" ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1912")>]
let ``JYRO-REQ-1912 JyroLibrary has Parse of a string and a time span beside Parse of a string`` () =
    let one = staticOf typeof<JyroLibrary> "Parse" [ typeof<string> ]
    let two = staticOf typeof<JyroLibrary> "Parse" [ typeof<string>; typeof<TimeSpan> ]
    Assert.Equal(typeof<JyroResult<JyroLibrary>>, one.ReturnType)
    Assert.Equal(typeof<JyroResult<JyroLibrary>>, two.ReturnType)
    Assert.Equal("maxCompilationTime", two.GetParameters().[1].Name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1912")>]
let ``JYRO-REQ-1912 Parse of a library that outlasts the limit returns one CompilationTimeLimitExceeded at Parse with no value and does not throw`` () =
    let result = JyroLibrary.Parse(slowLibrary, TimeSpan.FromTicks 1L)
    Assert.False(result.IsSuccess)
    Assert.False(result.HasValue)
    let only = Assert.Single(result.Messages)
    Assert.Equal(MessageCode.CompilationTimeLimitExceeded, only.Code)
    Assert.Equal(MessageStage.Parse, only.Stage)

[<Theory>]
[<InlineData(50_000_000L)>]
[<InlineData(864_000_000_000L)>]
[<InlineData(9_223_372_036_854_775_807L)>]
[<Trait("requirement", "JYRO-REQ-1912")>]
let ``JYRO-REQ-1912 Parse accepts a limit from one tick to 24 hours and TimeSpan.MaxValue, and a library within it succeeds`` (n: int64) =
    let result = JyroLibrary.Parse(simpleLibrary, TimeSpan.FromTicks n)
    Assert.True(result.IsSuccess, Script.describe result)
    Assert.True(result.HasValue)

[<Theory>]
[<InlineData(0L)>]
[<InlineData(-1L)>]
[<InlineData(-10_000L)>]
[<InlineData(864_000_000_001L)>]
[<InlineData(25_920_000_000_000L)>]
[<InlineData(9_223_372_036_854_775_806L)>]
[<Trait("requirement", "JYRO-REQ-1912")>]
let ``JYRO-REQ-1912 Parse throws ArgumentOutOfRangeException named maxCompilationTime for any other limit`` (n: int64) =
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> JyroLibrary.Parse(simpleLibrary, TimeSpan.FromTicks n) |> ignore)
    Assert.Equal("maxCompilationTime", thrown.ParamName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1912")>]
let ``JYRO-REQ-1912 Parse of func f return 1 end with five seconds succeeds`` () =
    let result = JyroLibrary.Parse("func f() return 1 end", TimeSpan.FromSeconds 5.)
    Assert.True(result.IsSuccess, Script.describe result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1912")>]
let ``JYRO-REQ-1912 Parse of a string alone still parses a library that is within the default limit`` () =
    let result = JyroLibrary.Parse(simpleLibrary)
    Assert.True(result.IsSuccess, Script.describe result)

// -- JYRO-REQ-1935: registering and looking up functions does no needless work ---------------------------------------

/// A sequence that can be enumerated once: a second GetEnumerator call throws InvalidOperationException.
type private OnceSequence(items: IJyroFunction list) =
    let mutable enumerations = 0
    member _.Enumerations = enumerations
    interface IEnumerable<IJyroFunction> with
        member _.GetEnumerator() : IEnumerator<IJyroFunction> =
            enumerations <- enumerations + 1
            if enumerations > 1 then raise (InvalidOperationException "the sequence was enumerated a second time")
            (items :> IEnumerable<IJyroFunction>).GetEnumerator()
    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            ((this :> IEnumerable<IJyroFunction>).GetEnumerator()) :> IEnumerator

let private twoFunctions () : RecordingFunction * RecordingFunction =
    let make name = Host.func name [ Parameter.Required("x", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure (fun args -> args.Head)
    make "FirstOne", make "SecondOne"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1935")>]
let ``JYRO-REQ-1935 JyroCompiler AddFunctions enumerates its argument exactly once and registers every function`` () =
    let a, b = twoFunctions ()
    let sequence = OnceSequence([ a.AsFunction; b.AsFunction ])
    let compiler = JyroCompiler().AddFunctions(sequence).WithSource("Output.a = FirstOne(1)\nOutput.b = SecondOne(2)")
    let program = Script.valueOf (compiler.Compile())
    Assert.Equal(1, sequence.Enumerations)
    Assert.Equal("{\"a\":1,\"b\":2}", Values.toJson (run program))
    Assert.Equal(1, a.CallCount)
    Assert.Equal(1, b.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1935")>]
let ``JYRO-REQ-1935 JyroArtifactLoader AddFunctions enumerates its argument exactly once and registers every function`` () =
    let a, b = twoFunctions ()
    let bytes =
        Script.valueOf (
            JyroCompiler().AddFunction(a.AsFunction).AddFunction(b.AsFunction)
                .WithSource("Output.a = FirstOne(1)\nOutput.b = SecondOne(2)").CompileToArtifact())
    let sequence = OnceSequence([ a.AsFunction; b.AsFunction ])
    let loader = JyroArtifactLoader(bytes).AddFunctions(sequence)
    let program = Script.valueOf (loader.Load())
    Assert.Equal(1, sequence.Enumerations)
    Assert.Equal("{\"a\":1,\"b\":2}", Values.toJson (run program))

/// The names of the standard library, from GetAll (97 at the time of writing).
let private libraryNames : string list =
    Tests.Support.stdlibAll () |> List.map (fun f -> f.Name) |> List.distinct

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1935")>]
let ``JYRO-REQ-1935 GetAll names the functions of the standard library`` () =
    Assert.True(libraryNames.Length >= 90, sprintf "GetAll holds %d names" libraryNames.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1935")>]
let ``JYRO-REQ-1935 TryGetFunction gives the same object for a name on every call, whatever other names were looked up in between`` () =
    let get name = Tests.Support.stdlibTryGet name
    let first = libraryNames |> List.map (fun n -> n, get n)
    for name, found in first do
        Assert.True(found.IsSome, sprintf "TryGetFunction did not know %s" name)
    // Other names in between, in a different order, several times over.
    for _ in 1..3 do
        for name in List.rev libraryNames do
            let other = libraryNames.[(List.findIndex ((=) name) libraryNames + 1) % libraryNames.Length]
            Assert.True((get other).IsSome)
            let again = get name
            Assert.True(again.IsSome)
            let before = first |> List.find (fun (n, _) -> n = name) |> snd
            Assert.True(Object.ReferenceEquals(before.Value, again.Value), sprintf "TryGetFunction gave a new object for %s" name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1935")>]
let ``JYRO-REQ-1935 TryGetFunction of an unknown name returns false`` () =
    Assert.True((Tests.Support.stdlibTryGet "NoSuchFunctionInTheLibrary").IsNone)

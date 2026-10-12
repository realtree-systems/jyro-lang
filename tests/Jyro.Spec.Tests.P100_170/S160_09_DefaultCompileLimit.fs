/// JYRO-REQ-1911: compilation has a time limit of 5 seconds unless the host sets another. A source that takes
/// 6 seconds to compile is made by a host function whose call check sleeps 6 seconds: the compilation calls
/// CheckCall during Link for each call of the function (JYRO-REQ-2119), on a compilation from source and on a
/// load of an artifact alike. The tests that wait for it are in this module so that they run beside the others.
/// Written from the specification alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.S160_09_DefaultCompileLimit

open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// `Slow()`: a host function whose call check takes `delayMs` milliseconds to run.
type private SlowToCheck(name: string, delayMs: int) =
    inherit JyroFunctionBase(name, Host.signature name [] ParameterType.Any)
    override _.Effect = FunctionEffect.Pure
    override _.Execute(_: IReadOnlyList<JyroValue>, _: JyroExecutionContext) = Values.nul
    override _.CheckCall(_: JyroCallSite) = Thread.Sleep delayMs

/// `Slow()` with no call check: the function that writes the artifact.
let private plain () : IJyroFunction =
    (Host.func "Slow" [] ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)).AsFunction

let private slow () : IJyroFunction = SlowToCheck("Slow", 6000) :> IJyroFunction

let private slowSource = "Output.x = Slow()"

let private limitExceeded (result: JyroResult<'T>) : unit =
    Assert.False(result.IsSuccess, "the compilation succeeded")
    Assert.False(result.HasValue)
    Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.CompilationTimeLimitExceeded)

let private stageOfLimit (result: JyroResult<'T>) : MessageStage =
    (result.Messages |> Seq.find (fun m -> m.Code = MessageCode.CompilationTimeLimitExceeded)).Stage

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 a compiler on which WithMaxCompilationTime has not been called fails for a source that takes 6 seconds, with CompilationTimeLimitExceeded at the stage that was running`` () =
    let result = JyroCompiler().WithSource(slowSource).AddFunction(slow ()).Compile()
    limitExceeded result
    // The 6 seconds are spent in the call check, which runs during Link.
    Assert.Equal(MessageStage.Link, stageOfLimit result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 the same compiler compiles Output.x = 1`` () =
    let result = JyroCompiler().WithSource("Output.x = 1").AddFunction(slow ()).Compile()
    Assert.True(result.IsSuccess, Script.describe result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 CompileToArtifact has the same default limit`` () =
    let result = JyroCompiler().WithSource(slowSource).AddFunction(slow ()).CompileToArtifact()
    limitExceeded result

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 the same compiler with WithMaxCompilationTime of TimeSpan.MaxValue compiles the source that takes 6 seconds`` () =
    let watch = Stopwatch.StartNew()
    let result = JyroCompiler().WithSource(slowSource).AddFunction(slow ()).WithMaxCompilationTime(TimeSpan.MaxValue).Compile()
    Assert.True(result.IsSuccess, Script.describe result)
    Assert.True(watch.Elapsed.TotalSeconds >= 5.5, "the source did not take about 6 seconds to compile")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 a host that sets a limit replaces the default`` () =
    let result = JyroCompiler().WithSource(slowSource).AddFunction(slow ()).WithMaxCompilationTime(TimeSpan.FromSeconds 60.).Compile()
    Assert.True(result.IsSuccess, Script.describe result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 a host that sets a shorter limit than the default has it`` () =
    let watch = Stopwatch.StartNew()
    let result = JyroCompiler().WithSource(slowSource).AddFunction(slow ()).WithMaxCompilationTime(TimeSpan.FromSeconds 1.).Compile()
    limitExceeded result
    Assert.True(watch.Elapsed.TotalSeconds < 5.5, "a limit of 1 second did not end the compilation before the default would")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 a JyroArtifactLoader on which WithMaxCompilationTime has not been called has the same default`` () =
    let bytes = Script.valueOf (JyroCompiler().WithSource(slowSource).AddFunction(plain ()).CompileToArtifact())
    let result = JyroArtifactLoader(bytes).AddFunction(slow ()).Load()
    limitExceeded result
    Assert.Equal(MessageStage.Link, stageOfLimit result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1911")>]
let ``JYRO-REQ-1911 a JyroArtifactLoader with WithMaxCompilationTime of TimeSpan.MaxValue loads the artifact that takes 6 seconds`` () =
    let bytes = Script.valueOf (JyroCompiler().WithSource(slowSource).AddFunction(plain ()).CompileToArtifact())
    let result = JyroArtifactLoader(bytes).AddFunction(slow ()).WithMaxCompilationTime(TimeSpan.MaxValue).Load()
    Assert.True(result.IsSuccess, Script.describe result)

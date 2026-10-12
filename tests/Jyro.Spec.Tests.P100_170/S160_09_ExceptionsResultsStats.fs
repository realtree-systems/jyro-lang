/// What JyroException, JyroResult<T>, JyroExecutionResult, JyroExecutionContext and JyroPipelineStats do, after
/// JYRO-ADR-191, 198 and 201: JYRO-REQ-1873 (what a host function sees of the run), 1878 (the one-call members),
/// 1885 and 1886 (JyroException and JyroRuntimeException), 1887 (GetValueOrThrow and ExitMessage), 1897 (what
/// each path records in the statistics) and 1927 (the statistics are read-only telemetry). Written from the
/// specification alone (FLOORS 4.1.1). The shape of the same types is in S160_09_ApiShape.fs.
module RealTree.Jyro.Spec.Tests.S160_09_ExceptionsResultsStats

open System
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.S160_09_Reflect

let private compile (source: string) : CompiledProgram =
    Script.valueOf (JyroCompiler().WithSource(source).WithStdlib().Compile())

let private run (source: string) : JyroExecutionResult =
    JyroRunner(compile source).WithInput(Values.emptyObj ()).Execute()

let private asResult (r: JyroExecutionResult) : JyroResult<JyroValue> = upcast r

let private firstError (messages: IReadOnlyList<DiagnosticMessage>) : DiagnosticMessage =
    messages |> Seq.find (fun m -> m.Severity = MessageSeverity.Error)

// -- JYRO-REQ-1873: a host function sees the run through a restricted context ---------------------------------

/// What a host function `Probe()` read from its context.
type private Seen =
    { Input: string
      Output: string
      CallDepth: int
      PeakCallDepth: int
      Token: CancellationToken
      HostCancellationRequested: bool }

let private probing (cancelInside: CancellationTokenSource option) : RecordingFunction * (unit -> Seen) =
    let seen = ref Unchecked.defaultof<Seen>
    let f =
        Host.funcWithContext "Probe" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
            cancelInside |> Option.iter (fun c -> c.Cancel())
            seen.Value <-
                { Input = ctx.Input.ToJson()
                  Output = ctx.Output.ToJson()
                  CallDepth = ctx.CallDepth
                  PeakCallDepth = ctx.PeakCallDepth
                  Token = ctx.CancellationToken
                  HostCancellationRequested = ctx.HostCancellationRequested }
            Values.nul)
    f, (fun () -> seen.Value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 a host function reads the Input, the Output so far and the call depths of the run through the context`` () =
    let probe, seen = probing None
    let source = "func inner()\n  Probe()\n  return 1\nend\nOutput.a = 1\nOutput.b = inner()"
    let program = Script.valueOf (JyroCompiler().WithSource(source).AddFunction(probe.AsFunction).Compile())
    JyroRunner(program).WithInputJson("{\"k\":7}").Execute() |> asResult |> Script.output |> ignore
    let s = seen ()
    Assert.Equal("{\"k\":7}", s.Input)
    Assert.Equal("{\"a\":1}", s.Output)
    Assert.True(s.CallDepth >= 1, sprintf "the call depth inside a script function was %d" s.CallDepth)
    Assert.True(s.PeakCallDepth >= s.CallDepth, "the peak call depth is below the call depth")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 a host function sees the cancellation token the host gave the runner and a host cancellation`` () =
    use cts = new CancellationTokenSource()
    let probe, seen = probing (Some cts)
    let program = Script.valueOf (JyroCompiler().WithSource("Probe()").AddFunction(probe.AsFunction).Compile())
    // The host cancels from inside the function; the function read the context at the moment of the call.
    JyroRunner(program).WithInput(Values.emptyObj ()).WithCancellationToken(cts.Token).Execute() |> ignore
    let s = seen ()
    // The token the function sees may be linked to the host's token and to the time limit (it is not
    // required to be the host's very token): it can be canceled, and it is canceled when the host cancels.
    Assert.True(s.Token.CanBeCanceled, "the token of the context cannot be canceled")
    Assert.True(s.Token.IsCancellationRequested, "the token of the context was not canceled by the host's cancel")
    Assert.True(s.HostCancellationRequested, "HostCancellationRequested was false after the host canceled")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 HostCancellationRequested is false when the host has not canceled`` () =
    use cts = new CancellationTokenSource()
    let probe, seen = probing None
    let program = Script.valueOf (JyroCompiler().WithSource("Probe()").AddFunction(probe.AsFunction).Compile())
    JyroRunner(program).WithInput(Values.emptyObj ()).WithCancellationToken(cts.Token).Execute() |> ignore
    Assert.False((seen ()).HostCancellationRequested)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 CheckExecutionLimits from a host function ends a run past its time limit`` () =
    let spin = Tests.Support.stallChecked ()
    let program = Script.valueOf (JyroCompiler().WithSource("StallChecked(400)\nOutput.after = 1").AddFunction(spin.AsFunction).Compile())
    let options = { JyroExecutionOptions.Default with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }
    let result = JyroRunner(program).WithInput(Values.emptyObj ()).WithExecutionOptions(options).Execute()
    Script.expectError MessageCode.ExecutionTimeLimitExceeded (asResult result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 a context made by a host for a unit test of its own function has a null HostState and no cancellation`` () =
    let ctx = JyroExecutionContext()
    Assert.Null(ctx.HostState)
    Assert.False(ctx.HostCancellationRequested)
    Assert.Equal(0, ctx.CallDepth)
    Assert.Equal(0, ctx.PeakCallDepth)
    ctx.CheckExecutionLimits()

// -- JYRO-REQ-1878: the one-call members -----------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1878")>]
let ``JYRO-REQ-1878 ExecuteScript and ExecuteScriptWithJson compile and run a script and return a JyroExecutionResult`` () =
    let fromData = JyroScript.executeScript "Output.x = Input.k + 1" (box (Dictionary<string, obj>(dict [ "k", box 4 ])))
    Assert.Equal("{\"x\":5}", Values.toJson (Script.output (asResult fromData)))
    let fromJson = JyroScript.executeScriptWithJson "Output.x = Input.k + 1" "{\"k\":4}"
    Assert.Equal("{\"x\":5}", Values.toJson (Script.output (asResult fromJson)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1878")>]
let ``JYRO-REQ-1878 the one-call members take the source first and the data second`` () =
    // The older extension took its arguments the other way round, so that "Output.x = 1".ExecuteJyro(json)
    // compiled the JSON as the script. The members that remain read the first argument as the script.
    let result = JyroScript.executeScriptWithJson "Output.x = 1" "{}"
    Assert.True(result.IsSuccess)
    let swapped = JyroScript.executeScriptWithJson "{}" "Output.x = 1"
    Assert.False(swapped.IsSuccess)

// -- JYRO-REQ-1885: one exception, JyroException, reports a failed pipeline ----------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 CompileOrThrow of var s string throws a JyroException at Validate, at line 1 column 1, holding TypedVariableNeedsInitializer`` () =
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroCompiler().WithSource("var s: string").CompileOrThrow() |> ignore)
    Assert.Equal(MessageStage.Validate, thrown.Stage)
    Assert.True(thrown.Location.HasValue, "the exception has no location")
    Assert.Equal(1, thrown.Location.Value.Line)
    Assert.Equal(1, thrown.Location.Value.Column)
    Assert.Contains(thrown.Messages, fun m -> m.Code = MessageCode.TypedVariableNeedsInitializer)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 Stage and Location are those of the first error in Messages, and Messages hold at least one error`` () =
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroCompiler().WithSource("Output.a = Nope(1)\nOutput.b = Other(2)").CompileOrThrow() |> ignore)
    Assert.True(thrown.Messages.Count >= 1)
    let first = firstError thrown.Messages
    Assert.Equal(first.Stage, thrown.Stage)
    Assert.Equal(first.Location, thrown.Location)
    Assert.Contains(thrown.Messages, fun m -> m.Severity = MessageSeverity.Error)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 the Messages of the exception are the Messages of the failed result`` () =
    let source = "Output.a = Nope(1)\nOutput.b = Other(2)"
    let failed = JyroCompiler().WithSource(source).Compile()
    Assert.False(failed.IsSuccess)
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroCompiler().WithSource(source).CompileOrThrow() |> ignore)
    Assert.Equal<MessageCode list>([ for m in failed.Messages -> m.Code ], [ for m in thrown.Messages -> m.Code ])
    let viaResult = Assert.ThrowsAny<JyroException>(fun () -> failed.GetValueOrThrow() |> ignore)
    Assert.Equal<MessageCode list>([ for m in failed.Messages -> m.Code ], [ for m in viaResult.Messages -> m.Code ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 catch of JyroException catches a failure of every stage`` () =
    let stageOf (f: unit -> unit) : MessageStage =
        (Assert.ThrowsAny<JyroException>(fun () -> f ())).Stage
    Assert.Equal(MessageStage.Parse, stageOf (fun () -> JyroCompiler().WithSource("var = (").CompileOrThrow() |> ignore))
    Assert.Equal(MessageStage.Validate, stageOf (fun () -> JyroCompiler().WithSource("var s: string").CompileOrThrow() |> ignore))
    Assert.Equal(MessageStage.Link, stageOf (fun () -> JyroCompiler().WithSource("Output.x = Nope(1)").CompileOrThrow() |> ignore))
    Assert.Equal(
        MessageStage.Execute,
        stageOf (fun () -> JyroRunner(compile "Output.x = 1 / 0").WithInput(Values.emptyObj ()).ExecuteOrThrow() |> ignore))
    Assert.Equal(
        MessageStage.Deserialize,
        stageOf (fun () -> JyroArtifactLoader([| 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy |]).Load().GetValueOrThrow() |> ignore))

// -- JYRO-REQ-1886: JyroRuntimeException is a JyroException with one diagnostic -------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1886")>]
let ``JYRO-REQ-1886 the exception that FromJson of an unclosed array throws is caught as a JyroException with one InvalidJson diagnostic and no location`` () =
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroValue.FromJson("[1,") |> ignore)
    Assert.IsType<JyroRuntimeException>(thrown) |> ignore
    Assert.Equal(1, thrown.Messages.Count)
    Assert.Equal(MessageCode.InvalidJson, thrown.Messages.[0].Code)
    Assert.False(thrown.Location.HasValue, "FromJson gave a location")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1886")>]
let ``JYRO-REQ-1886 for a JyroRuntimeException Messages hold exactly one diagnostic with the Code and Args of the exception`` () =
    let cases : (unit -> exn) list =
        [ fun () -> JyroError.Runtime(MessageCode.InvalidJson, "at", 7) :> exn
          fun () -> JyroError.Runtime(MessageCode.ArgumentTypeMismatch, "string", "number") :> exn
          fun () -> JyroError.Host("AmountNotPositive", -5) :> exn
          fun () -> JyroError.Host("Only a reason") :> exn
          fun () -> Assert.ThrowsAny<JyroRuntimeException>(fun () -> JyroValue.FromJson("{\"a\":1") |> ignore) :> exn ]
    for make in cases do
        let e = make () :?> JyroRuntimeException
        Assert.Equal(1, e.Messages.Count)
        let d = e.Messages.[0]
        Assert.Equal(e.Code, d.Code)
        Assert.Equal<string list>(
            [ for a in e.Args -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture) ],
            [ for a in d.Args -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture) ])
        Assert.Equal(e.Location, d.Location)
        Assert.Equal(d.Location, (e :> JyroException).Location)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1886")>]
let ``JYRO-REQ-1886 the exception of a run that a host function ended is a JyroException that holds the diagnostic of the run`` () =
    let failing = Host.func "Boom" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host("Reason", 3)))
    let program = Script.valueOf (JyroCompiler().WithSource("Output.x = Boom()").AddFunction(failing.AsFunction).Compile())
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroRunner(program).WithInput(Values.emptyObj ()).ExecuteOrThrow() |> ignore)
    Assert.Contains(thrown.Messages, fun m -> m.Code = MessageCode.HostFunctionError)
    Assert.Equal(MessageStage.Execute, thrown.Stage)

// -- JYRO-REQ-1887: GetValueOrThrow and ExitMessage -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 GetValueOrThrow returns the value of a successful result`` () =
    let compiled = JyroCompiler().WithSource("Output.x = 1").Compile()
    Assert.NotNull(compiled.GetValueOrThrow())
    let library = JyroLibrary.Parse "func f() return 1 end"
    Assert.NotNull(library.GetValueOrThrow())
    let artifact = JyroCompiler().WithSource("Output.x = 1").CompileToArtifact()
    Assert.NotEmpty(artifact.GetValueOrThrow())
    Assert.Equal(1, (JyroArtifactInfo.Read(artifact.GetValueOrThrow())).GetValueOrThrow().FormatVersion)
    let executed = run "Output.x = 1"
    Assert.Equal("{\"x\":1}", executed.GetValueOrThrow().ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 GetValueOrThrow of JyroArtifactInfo Read of three bytes throws a JyroException holding ArtifactInvalidMagic`` () =
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroArtifactInfo.Read([| 0x4Auy; 0x59uy; 0x52uy |]).GetValueOrThrow() |> ignore)
    Assert.Contains(thrown.Messages, fun m -> m.Code = MessageCode.ArtifactInvalidMagic)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 GetValueOrThrow of JyroLibrary Parse of a statement throws a JyroException holding LibraryStatementForbidden`` () =
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroLibrary.Parse("Output.x = 1").GetValueOrThrow() |> ignore)
    Assert.Contains(thrown.Messages, fun m -> m.Code = MessageCode.LibraryStatementForbidden)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 GetValueOrThrow of a failed Compile, Execute and CompileToArtifact throws the JyroException of the first error`` () =
    let compiled = JyroCompiler().WithSource("var s: string").Compile()
    let a = Assert.ThrowsAny<JyroException>(fun () -> compiled.GetValueOrThrow() |> ignore)
    Assert.Equal((firstError compiled.Messages).Stage, a.Stage)
    let executed = run "Output.x = 1 / 0"
    let b = Assert.ThrowsAny<JyroException>(fun () -> executed.GetValueOrThrow() |> ignore)
    Assert.Equal(MessageStage.Execute, b.Stage)
    Assert.Contains(b.Messages, fun m -> m.Code = MessageCode.DivisionByZero)
    let artifact = JyroCompiler().WithSource("var s: string").CompileToArtifact()
    let c = Assert.ThrowsAny<JyroException>(fun () -> artifact.GetValueOrThrow() |> ignore)
    Assert.Contains(c.Messages, fun m -> m.Code = MessageCode.TypedVariableNeedsInitializer)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 the ExitMessage of Execute after exit with an empty text is the empty string and after fail with a text is null`` () =
    Assert.Equal("", (run "exit \"\"").ExitMessage)
    Assert.Null((run "fail \"x\"").ExitMessage)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 the ExitMessage is the text of exit with a value and null in every other case`` () =
    Assert.Equal("done", (run "exit \"done\"").ExitMessage)
    Assert.Null((run "Output.x = 1").ExitMessage)
    Assert.Null((run "exit").ExitMessage)
    Assert.Null((run "Output.x = 1 / 0").ExitMessage)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 the one-call members give the ExitMessage too`` () =
    Assert.Equal("m", (JyroScript.executeScriptWithJson "exit \"m\"" "{}").ExitMessage)
    Assert.Equal("m", (JyroScript.executeScript "exit \"m\"" (box (Dictionary<string, obj>()))).ExitMessage)

// -- JYRO-REQ-1927: JyroPipelineStats is read-only telemetry ----------------------------------------------

let private depthProgram : string =
    "func f(n)\n  if n > 0 then\n    return f(n - 1)\n  end\n  return 0\nend\nOutput.x = f(3)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 PeakCallDepth is 4 after a run of f(3) with the statistics given by WithStats`` () =
    let stats = JyroPipelineStats()
    JyroRunner(compile depthProgram).WithInput(Values.emptyObj ()).WithStats(stats).Execute() |> asResult |> Script.output |> ignore
    Assert.Equal(4, stats.PeakCallDepth)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 PeakCallDepth equals what the context reads at the end of the run`` () =
    let atEnd = ref (-1)
    let probe =
        Host.funcWithContext "AtEnd" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
            atEnd.Value <- ctx.PeakCallDepth
            Values.nul)
    let program = Script.valueOf (JyroCompiler().WithSource(depthProgram + "\nAtEnd()").AddFunction(probe.AsFunction).Compile())
    let stats = JyroPipelineStats()
    JyroRunner(program).WithInput(Values.emptyObj ()).WithStats(stats).Execute() |> asResult |> Script.output |> ignore
    Assert.Equal(atEnd.Value, stats.PeakCallDepth)
    Assert.Equal(4, stats.PeakCallDepth)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 TotalTime is the sum of the six stage times`` () =
    let stats = JyroPipelineStats()
    let program = Script.valueOf (JyroCompiler().WithSource(depthProgram).WithStdlib().WithStats(stats).Compile())
    JyroRunner(program).WithInput(Values.emptyObj ()).WithStats(stats).Execute() |> asResult |> Script.output |> ignore
    let sum = stats.ParseTime + stats.ValidateTime + stats.LinkTime + stats.CompileTime + stats.DeserializeTime + stats.ExecuteTime
    Assert.Equal(sum, stats.TotalTime)
    Assert.True(stats.ExecuteTime > TimeSpan.Zero, "ExecuteTime is zero after a run")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 StatementCount and LoopIterations are counts of the run, and LimiterElapsedTime is a time`` () =
    let stats = JyroPipelineStats()
    let source = "var t = 0\nforeach i in [1, 2, 3] do t += i end\nOutput.t = t"
    JyroRunner(compile source).WithInput(Values.emptyObj ()).WithStats(stats).Execute() |> asResult |> Script.output |> ignore
    Assert.True(stats.StatementCount > 0L, "StatementCount is zero after a run")
    Assert.True(stats.LoopIterations >= 3L, sprintf "LoopIterations is %d after three iterations" stats.LoopIterations)
    Assert.True(stats.LimiterElapsedTime >= TimeSpan.Zero)

// -- JYRO-REQ-1897: the stages each path records, and the documents that say so --------------------------------

let private forSource (source: string) : JyroPipelineStats * JyroResult<CompiledProgram> =
    let stats = JyroPipelineStats()
    stats, JyroCompiler().WithSource(source).WithStats(stats).Compile()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1897")>]
let ``JYRO-REQ-1897 a compilation that ends in Compile records Parse, Validate, Link and Compile and not Deserialize`` () =
    let stats, result = forSource "Output.x = 1"
    Assert.True(result.IsSuccess, Script.describe result)
    Assert.True(stats.ParseTime > TimeSpan.Zero, "ParseTime")
    Assert.True(stats.ValidateTime > TimeSpan.Zero, "ValidateTime")
    Assert.True(stats.LinkTime > TimeSpan.Zero, "LinkTime")
    Assert.True(stats.CompileTime > TimeSpan.Zero, "CompileTime")
    Assert.Equal(TimeSpan.Zero, stats.DeserializeTime)
    Assert.Equal(TimeSpan.Zero, stats.ExecuteTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1897")>]
let ``JYRO-REQ-1897 CompileToArtifact records Parse, Validate, Link and Compile and not Deserialize`` () =
    let stats = JyroPipelineStats()
    let written = JyroCompiler().WithSource("Output.x = 1").WithStats(stats).CompileToArtifact()
    Assert.True(written.IsSuccess, Script.describe written)
    Assert.True(stats.CompileTime > TimeSpan.Zero, "CompileToArtifact recorded no Compile time")
    Assert.True(stats.ParseTime > TimeSpan.Zero, "ParseTime")
    Assert.True(stats.ValidateTime > TimeSpan.Zero, "ValidateTime")
    Assert.True(stats.LinkTime > TimeSpan.Zero, "LinkTime")
    Assert.Equal(TimeSpan.Zero, stats.DeserializeTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1897")>]
let ``JYRO-REQ-1897 a load records Deserialize, Validate, Link and Compile and not Parse`` () =
    let bytes = Script.valueOf (JyroCompiler().WithSource("Output.x = 1").CompileToArtifact())
    let stats = JyroPipelineStats()
    let loaded = JyroArtifactLoader(bytes).WithStats(stats).Load()
    Assert.True(loaded.IsSuccess, Script.describe loaded)
    Assert.True(stats.DeserializeTime > TimeSpan.Zero, "DeserializeTime")
    Assert.True(stats.ValidateTime > TimeSpan.Zero, "a load recorded no Validate time")
    Assert.True(stats.LinkTime > TimeSpan.Zero, "LinkTime")
    Assert.True(stats.CompileTime > TimeSpan.Zero, "CompileTime")
    Assert.Equal(TimeSpan.Zero, stats.ParseTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1897")>]
let ``JYRO-REQ-1897 with a fresh statistics object each time CompileToArtifact gives a CompileTime and no DeserializeTime and a Load gives a ValidateTime and no ParseTime`` () =
    let a = JyroPipelineStats()
    let bytes = Script.valueOf (JyroCompiler().WithSource("Output.x = 1").WithStats(a).CompileToArtifact())
    Assert.True(a.CompileTime > TimeSpan.Zero)
    Assert.Equal(TimeSpan.Zero, a.DeserializeTime)
    let b = JyroPipelineStats()
    JyroArtifactLoader(bytes).WithStats(b).Load() |> ignore
    Assert.True(b.ValidateTime > TimeSpan.Zero)
    Assert.Equal(TimeSpan.Zero, b.ParseTime)

let private forbiddenInDocs : Regex list =
    [ // "a load skips validation" in any phrasing that puts the load and the skipped validation in one sentence
      Regex(@"\b(load|loads|loading|artifact|artifacts)\b[^.\n]{0,100}\b(skip|skips|skipped|skipping|bypass|bypasses|without|does not run|doesn't run|do not run)\b[^.\n]{0,40}\bvalidat", RegexOptions.IgnoreCase)
      // "CompileToArtifact records no Compile"
      Regex(@"\bCompileToArtifact\b[^.\n]{0,100}\b(no|not|never|without)\b[^.\n]{0,30}\bCompile\b", RegexOptions.IgnoreCase) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1897")>]
let ``JYRO-REQ-1897 the Jyro.Api README states the stages of each path and does not say a load skips validation or that CompileToArtifact records no Compile`` () =
    let readme = apiReadme ()
    Assert.Contains("Deserialize", readme)
    Assert.Contains("JyroArtifactLoader", readme)
    Assert.Contains("CompileToArtifact", readme)
    for pattern in forbiddenInDocs do
        let m = pattern.Match readme
        Assert.False(m.Success, sprintf "The README says: %s" m.Value)

[<Theory>]
[<InlineData("JyroCompiler")>]
[<InlineData("JyroArtifactLoader")>]
[<InlineData("JyroPipelineStats")>]
[<Trait("requirement", "JYRO-REQ-1897")>]
let ``JYRO-REQ-1897 the XML documentation of the type states the stages and does not say a load skips validation or that CompileToArtifact records no Compile`` (name: string) =
    let docs = docsOfType (exported name) |> List.map snd |> String.concat " "
    Assert.False(String.IsNullOrWhiteSpace docs, sprintf "%s has no XML documentation" name)
    for pattern in forbiddenInDocs do
        let m = pattern.Match docs
        Assert.False(m.Success, sprintf "The documentation of %s says: %s" name m.Value)
    match name with
    | "JyroPipelineStats" ->
        for stage in [ "Parse"; "Validate"; "Link"; "Compile"; "Deserialize" ] do
            Assert.Contains(stage, docs)
    | "JyroArtifactLoader" ->
        Assert.Contains("Deserialize", docs)
        Assert.Contains("Validate", docs)
    | _ -> Assert.Contains("CompileToArtifact", docs)

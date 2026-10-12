module RealTree.Jyro.Tests.HostEntryContracts

open System
open System.IO
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

// The builder chain threw where it promised a result.
//
// `Compile()` and `Execute()` return `JyroResult`, and the package documents
// checking `IsSuccess` as the way to handle failure - but six entry points on
// the way there raised a BCL exception instead, so a host writing the
// documented shape still had to wrap the chain in a `try`:
//
//   WithInputJson("{ not json")        -> JsonReaderException
//   JyroArtifactLoader(null).Load()    -> NullReferenceException
//   JyroLibrary.Parse(null)            -> NullReferenceException
//   AddLibrarySource(null)             -> NullReferenceException
//   FromJson("{oops")                  -> JsonReaderException
//
// Two rules settle them, and neither is new. **A null argument is a mistake in
// the host's own code**, so it is refused at the call with
// ArgumentNullException - which is what `JyroRunner`'s constructor already does
// for `program`, and what `JyroString(null)` does. **Malformed content is
// data**, so it is reported as a diagnostic through the result - which is what
// `JyroCompiler` already does for a faulty library script.

let private program () =
    JyroCompiler().WithStdlib().WithSource("Output.v = Input").CompileOrThrow()

let private streamOf (text: string) = new MemoryStream(Encoding.UTF8.GetBytes text)

// -- Malformed content: a diagnostic, not an exception ----------------------

[<Fact>]
let ``malformed JSON input is reported by Execute, not raised by WithInputJson`` () =
    // The whole point: this is the shape the README teaches, and it used to
    // throw before `Execute` was ever reached.
    let result = JyroRunner(program ()).WithInputJson("{ not json").Execute()
    Assert.False(result.IsSuccess)
    Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.InvalidJson)

[<Fact>]
let ``malformed JSON from a stream is reported the same way`` () =
    use s = streamOf "{ not json"
    let result = JyroRunner(program ()).WithInputJson(s).Execute()
    Assert.False(result.IsSuccess)
    Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.InvalidJson)

[<Fact>]
let ``the script does not run when its input could not be read`` () =
    // Running anyway would report whatever the script made of a null input -
    // a second wrong answer stacked on the first. `Output.v` must be absent
    // because nothing executed.
    let result = JyroRunner(program ()).WithInputJson("{ not json").Execute()
    Assert.False(result.IsSuccess)
    Assert.Equal(1, Seq.length result.Messages)

[<Fact>]
let ``well-formed JSON is unaffected`` () =
    let result = JyroRunner(program ()).WithInputJson("{\"a\": 1}").Execute()
    Assert.True(result.IsSuccess, sprintf "%A" (List.ofSeq result.Messages |> List.map (fun m -> m.Message)))
    match result.TryGetValue() with
    | true, v -> Assert.Equal(1.0, (v.GetProperty("v").GetProperty("a")).ToDouble())
    | _ -> Assert.Fail("expected a value")

[<Theory>]
[<InlineData("{\"a\": 1, /* note */ }")>]
[<InlineData("{\"a\": 1, }")>]
[<InlineData("{\"a\": 1 /* note */ }")>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 WithInputJson refuses a trailing comma or a comment with InvalidJson`` (json: string) =
    // These were accepted by design as a lenient dialect. JYRO-REQ-2113 makes
    // the host's doors read JSON (RFC 8259), which has neither, so the run
    // fails through its result with InvalidJson and the script does not run.
    let result = JyroRunner(program ()).WithInputJson(json).Execute()
    Assert.False(result.IsSuccess)
    Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.InvalidJson)

// -- Direct conversion: a Jyro exception, not a BCL one ---------------------

[<Fact>]
let ``FromJson reports InvalidJson rather than leaking JsonException`` () =
    // This member returns a JyroValue and so has nowhere to put a diagnostic;
    // raising is the only way it can report. What matters is that it raises a
    // type the documented catch sees.
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromJson("{oops") |> ignore)
    Assert.Equal(MessageCode.InvalidJson, ex.Code)

[<Fact>]
let ``FromJson of a stream reports InvalidJson too`` () =
    use s = streamOf "{oops"
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromJson(s) |> ignore)
    Assert.Equal(MessageCode.InvalidJson, ex.Code)

[<Fact>]
let ``the script-facing FromJson still answers null, per the parse rule`` () =
    // Clause 90.5: a script has no exception handling, so the standard
    // library's parse functions answer null rather than raising. Changing the
    // exception type underneath must not have disturbed that.
    let v = Runner.expectSuccess "Output.v = FromJson('{oops')" (JyroObject() :> JyroValue)
    Assert.Equal(JyroValueType.Null, (v.GetProperty "v").ValueType)

// -- Null arguments: refused at the call ------------------------------------

[<Fact>]
let ``a null argument is refused where it is passed, not where it is used`` () =
    // Each of these stored the null and surfaced a NullReferenceException from
    // deep inside Compile() or Parse(), far from the member that accepted it.
    Assert.Throws<ArgumentNullException>(fun () -> JyroCompiler().WithSource(null) |> ignore) |> ignore
    Assert.Throws<ArgumentNullException>(fun () -> JyroArtifactLoader(null) |> ignore) |> ignore
    Assert.Throws<ArgumentNullException>(fun () -> JyroCompiler().AddLibrarySource(null) |> ignore) |> ignore
    Assert.Throws<ArgumentNullException>(fun () -> JyroLibrary.Parse(null) |> ignore) |> ignore
    Assert.Throws<ArgumentNullException>(fun () -> JyroRunner(program ()).WithInputJson(null: string) |> ignore) |> ignore

[<Fact>]
let ``the null refusal names the parameter`` () =
    // What a NullReferenceException could not do.
    let ex = Assert.Throws<ArgumentNullException>(fun () -> JyroArtifactLoader(null) |> ignore)
    Assert.Equal("artifact", ex.ParamName)

[<Fact>]
let ``a null argument is not turned into a diagnostic`` () =
    // The distinction the two rules draw. A faulty *library script* is data and
    // is deferred to Compile(); a null is the host's own bug and is refused.
    // Collapsing them would tell a host its script was bad when its code was.
    let compiler = JyroCompiler().WithStdlib().WithSource("Output.v = 1")
    Assert.Throws<ArgumentNullException>(fun () -> compiler.AddLibrarySource(null) |> ignore) |> ignore

[<Fact>]
let ``a faulty library script is still deferred to Compile`` () =
    // The other side of that line, pinned so the guard above cannot creep.
    let result =
        JyroCompiler().WithStdlib().WithSource("Output.v = 1")
            .AddLibrarySource("this is not a library")
            .Compile()
    Assert.False(result.IsSuccess)

// -- What a host actually writes --------------------------------------------

[<Fact>]
let ``the documented shape needs no try around it`` () =
    // Compile, run, check IsSuccess - with hostile input at every door and not
    // a single catch. This is the contract the package advertises, and before
    // this change it did not hold.
    let attempt (json: string) =
        let compiled = JyroCompiler().WithStdlib().WithSource("Output.v = Input").Compile()
        match compiled.TryGetValue() with
        | true, prog ->
            let r = JyroRunner(prog).WithInputJson(json).Execute()
            r.IsSuccess
        | _ -> false
    Assert.True(attempt "{\"a\": 1}")
    Assert.False(attempt "{ not json")
    Assert.False(attempt "")
    Assert.False(attempt "[1, 2")

// -- A failure that has nothing to report is not a failure ------------------

// `JyroResult.Success` derives its indicator from the messages - "true if and
// only if the list carries no message of Error severity", which the type's own
// comment says it does "rather than taken on trust, because the specification
// defines it that way". The four `Failure` factories hard-coded `false`, so
// that was true of one half of the type.
//
// `JyroResult<JyroValue>.Failure(emptySequence)` was therefore a publicly
// constructible result with IsSuccess = false and no Error message to justify
// it: the "if and only if" broken in exactly the direction the derivation was
// added to close. No pipeline path built one, but these are public factories,
// and a host wrapping Jyro in its own result type reaches them.
//
// Refused rather than derived-to-true: returning IsSuccess = true from a
// method called `Failure` would be the worse answer. A failure with nothing to
// say is a mistake in the caller's own code, so it is turned away at the call,
// which is the rule `JyroString(null)` already follows.

[<Fact>]
let ``a failure with no messages is refused`` () =
    Assert.Throws<ArgumentException>(fun () ->
        JyroResult<JyroValue>.Failure(Seq.empty) |> ignore) |> ignore

[<Fact>]
let ``a failure with no messages is refused on the value-carrying overload too`` () =
    // The overload `Compiler.execute` uses to report a partial Output with a
    // diagnostic. Both doors, one rule.
    Assert.Throws<ArgumentException>(fun () ->
        JyroResult<JyroValue>.Failure(JyroNull.Instance :> JyroValue, Seq.empty) |> ignore) |> ignore

[<Fact>]
let ``a failure carrying only warnings is refused`` () =
    // The subtler half: a non-empty list still cannot justify a failure unless
    // something in it is an error, because that is what IsSuccess is derived
    // from.
    let warning = DiagnosticMessage.Warning(MessageCode.RuntimeError, MessageStage.Execute)
    Assert.Throws<ArgumentException>(fun () ->
        JyroResult<JyroValue>.Failure([| warning |]) |> ignore) |> ignore

[<Fact>]
let ``a null message sequence is refused rather than dereferenced`` () =
    Assert.Throws<ArgumentNullException>(fun () ->
        JyroResult<JyroValue>.Failure(null: seq<DiagnosticMessage>) |> ignore) |> ignore

[<Fact>]
let ``a failure carrying an error is built as before`` () =
    // The control. Everything the pipeline actually constructs still works, and
    // IsSuccess is now derived rather than asserted.
    let error = DiagnosticMessage.Error(MessageCode.RuntimeError, MessageStage.Execute)
    let r = JyroResult<JyroValue>.Failure([| error |])
    Assert.False(r.IsSuccess)
    Assert.Single(r.Messages) |> ignore
    let withValue = JyroResult<JyroValue>.Failure(JyroNull.Instance :> JyroValue, [| error |])
    Assert.False(withValue.IsSuccess)
    Assert.True(withValue.HasValue)

// -- The resource counters do not roll over ---------------------------------

[<Fact>]
let ``the resource counters are 64-bit`` () =
    // They were `int`, incremented unchecked, so past Int32.MaxValue they
    // wrapped to Int32.MinValue: the `> MaxStatements` test stopped firing, the
    // run continued unbounded, and the counters a host reads out of
    // `JyroPipelineStats` went large and negative. Measured at 2.2 billion
    // statements under `JyroExecutionOptions.Unlimited`: tripped = false, count
    // = -2094967296, no diagnostic.
    //
    // The repair is the width, and the width is what this pins - reaching the
    // wrap itself costs about a minute of CPU, which is not worth spending on
    // every run of the suite to observe a bound that 64 bits puts three hundred
    // years away.
    let stats = JyroPipelineStats()
    Assert.Equal(typeof<int64>, stats.StatementCount.GetType())
    Assert.Equal(typeof<int64>, stats.LoopIterations.GetType())

[<Fact>]
let ``the counters still count, and the limits still fire`` () =
    // The comparison moved to int64 alongside them, so this is the part that
    // could have broken: an ordinary limit must still trip at the same place.
    // The limiter is not public (JYRO-REQ-1874): the limit is set and read through a run.
    let limited (source: string) =
        let stats = JyroPipelineStats()
        let result =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().WithSource(source))
                (fun r ->
                    r.WithExecutionOptions({ JyroExecutionOptions.Default with MaxStatements = 3 })
                     .WithStats(stats)
                     .WithInput(JyroObject() :> JyroValue))
        result, stats
    let ok, okStats = limited "var a = 1\nvar b = 2\nvar c = 3"
    Assert.True(ok.IsSuccess)
    Assert.True(okStats.StatementCount >= 3L, sprintf "StatementCount is %d" okStats.StatementCount)
    let over, _ = limited "var a = 1\nvar b = 2\nvar c = 3\nvar d = 4"
    Assert.False(over.IsSuccess)
    Assert.Equal<MessageCode list>([ MessageCode.StatementLimitExceeded ], over.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq)

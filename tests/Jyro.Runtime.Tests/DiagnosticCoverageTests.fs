module RealTree.Jyro.Tests.DiagnosticCoverage

open System
open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// A diagnostic travels as a code plus arguments and becomes a string only in
/// the formatter, so a host translating a code substitutes the same arguments
/// into its own template. These tests pin the properties that has to hold:
/// every code has a template, every template's placeholders are contiguous
/// from {0}, and a host template renders without leaking a placeholder.

/// Every declared code, with no exclusions.
///
/// There used to be one: `ScriptReturn`, a control-flow token thrown to
/// implement `exit` from a function body and caught by `Compiler.execute` as a
/// successful termination. It could never reach a host as a message, could not
/// be given a template that would not have lied about it being user-facing, and
/// so had to be carved out by name from every test that walks this enum. It is
/// now an exception type of its own, and `MessageCode` holds
/// diagnostics only. The list is exhaustive again, which is the point: a
/// name-based exclusion is where the next such thing would have hidden.
let private allCodes =
    Enum.GetValues(typeof<MessageCode>) :?> MessageCode[]
    |> Array.toList

let private placeholders (template: string) =
    Regex.Matches(template, @"\{(\d+)\}")
    |> Seq.map (fun m -> int m.Groups.[1].Value)
    |> Seq.distinct
    |> Seq.sort
    |> Seq.toList

[<Fact>]
let ``every MessageCode has a default template`` () =
    let hasTemplate (c: MessageCode) =
        let mutable template = Unchecked.defaultof<string>
        MessageTemplates.tryGet (c, &template)
    let missing = allCodes |> List.filter (hasTemplate >> not)
    Assert.True(missing.IsEmpty, sprintf "codes with no template: %A" missing)

[<Fact>]
let ``no template is left as a bare passthrough that cannot be translated`` () =
    // A template of "{0}" carries no translatable text - the English is the
    // argument. That is right for a message the engine did not author (a
    // script's own `fail` text, a host function's error, an escaped
    // exception), and wrong for anything the engine writes, because a host
    // translating that code cannot reach the words.
    let allowed =
        set [ MessageCode.ScriptFailure        // the script author's `fail` message
              MessageCode.HostFunctionError    // authored by the host function
              MessageCode.RuntimeError         // an escaped exception's message
              MessageCode.UnknownParserError ] // the parser's own failure detail
    let offenders =
        allCodes
        |> List.filter (fun c -> MessageTemplates.get c = "{0}")
        |> List.filter (fun c -> not (allowed.Contains c))
    Assert.True(offenders.IsEmpty, sprintf "engine-authored text hidden behind a passthrough template: %A" offenders)

[<Fact>]
let ``template placeholders are contiguous from zero`` () =
    // {0} then {2} would silently drop an argument at format time.
    let bad =
        allCodes
        |> List.map (fun c -> c, placeholders (MessageTemplates.get c))
        |> List.filter (fun (_, ps) -> ps <> [ 0 .. ps.Length - 1 ])
    Assert.True(bad.IsEmpty, sprintf "non-contiguous placeholders: %A" bad)

[<Fact>]
let ``a rendered message never contains an unfilled placeholder`` () =
    // The failure this replaces: a host supplied a French template for a code
    // whose diagnostic carried no arguments, and the formatter returned the
    // template verbatim - so the reader got a literal "{0}" on screen.
    for code in allCodes do
        let template = MessageTemplates.get code
        let arity = (placeholders template).Length
        // Rendered with the arguments it expects...
        let args = Array.init arity (fun i -> box (sprintf "arg%d" i))
        let rendered = MessageTemplates.render template args
        Assert.DoesNotContain("{", rendered)
        // ...and rendered with none, which is what a miswired call site does.
        Assert.DoesNotContain("{", MessageTemplates.render template [||])

[<Fact>]
let ``a host template is filled from the same arguments as the default`` () =
    // The end-to-end property: same code, same args, two languages.
    let msg =
        DiagnosticMessage.Error(
            MessageCode.TooFewArguments,
            MessageStage.Validate,
            Nullable(SourceLocation(1, 1, 1)),
            [| box "Substring"; box 2; box 1 |])
    Assert.Equal(
        "Function 'Substring' requires at least 2 arguments, but 1 were provided",
        msg.Message)

    let french =
        { new IMessageTemplateProvider with
            member _.TryGetTemplate(code, template) =
                if code = MessageCode.TooFewArguments then
                    template <- "La fonction '{0}' exige au moins {1} arguments, mais {2} ont ete fournis"
                    true
                else false }
    Assert.Contains(
        "La fonction 'Substring' exige au moins 2 arguments, mais 1 ont ete fournis",
        DiagnosticFormatter.formatLocalized french msg)

[<Fact>]
let ``a host template for a code with no arguments still renders`` () =
    let msg = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute)
    let french =
        { new IMessageTemplateProvider with
            member _.TryGetTemplate(code, template) =
                if code = MessageCode.DivisionByZero then
                    template <- "Division par zero"
                    true
                else false }
    Assert.Contains("Division par zero", DiagnosticFormatter.formatLocalized french msg)

// -- The result envelope enforces its own contract ------------------------
// Specification 130.2 defines the success indicator as true if and only if the
// message list carries no Error, and requires the value to be PRESENT whenever
// execution began - including on a failed run, since a script that populates
// Output incrementally and then breaches a limit leaves partial results in it.
//
// The type could express neither. `IsSuccess` was a stored field taken on the
// caller's word, with a second predicate `HasErrors` that could disagree with
// it; and `Failure` hard-coded `Value = None`, so the three sites in
// `Compiler.execute` that must report a value with a failure built the record
// by hand.

[<Fact>]
let ``a success carrying an error message is not a success`` () =
    // JYRO-REQ-1888: a success cannot be made holding an error.
    let thrown =
        Assert.Throws<ArgumentException>(fun () ->
            JyroResult<int>.Success(1, [ DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute) ]) |> ignore)
    Assert.Equal("messages", thrown.ParamName)

[<Fact>]
let ``warnings do not spoil a success`` () =
    let withWarning =
        JyroResult<int>.Success(1, [ DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute) ])
    Assert.True(withWarning.IsSuccess)
    Assert.Equal(Some 1, withWarning.TryValue)

// -- A message the engine did not author still has to arrive ---------------
// The tests above pin the templates. These pin the other half: a code whose
// template is "{0}" carries its whole text in Args, so a raise site that omits
// them renders an empty string. Nothing above can catch that - the template is
// present and correct, and the code is on the allowed passthrough list.

let private executeExpectingFailure (source: string) =
    let result =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource(source))
            (fun r -> r.WithInput(JyroObject()))
    Assert.False(result.IsSuccess)
    List.ofSeq result.Messages |> List.ofSeq |> List.find (fun m -> m.Severity = MessageSeverity.Error)

[<Fact>]
let ``a script's fail message reaches the diagnostic`` () =
    let msg = executeExpectingFailure "fail \"order is already shipped\""

    Assert.Equal(MessageCode.ScriptFailure, msg.Code)
    Assert.Equal("order is already shipped", msg.Message)

    // In Args, not only pre-formatted on the exception: a host with its own
    // templates renders from Args, and Message itself is derived from them.
    Assert.Equal<obj[]>([| box "order is already shipped" |], msg.Args)

[<Fact>]
let ``a bare fail still says something`` () =
    let msg = executeExpectingFailure "fail"
    Assert.Equal(MessageCode.ScriptFailure, msg.Code)
    Assert.Equal("Script execution failed", msg.Message)

[<Fact>]
let ``a fail message built from an expression reaches the diagnostic`` () =
    let msg = executeExpectingFailure "var item = \"widget\"\nfail \"no stock of \" & item"
    Assert.Equal("no stock of widget", msg.Message)

[<Fact>]
let ``a fail inside a function carries its message out`` () =
    let source =
        "func Reject()\n    fail \"not allowed\"\nend\nReject()"
    let msg = executeExpectingFailure source
    Assert.Equal(MessageCode.ScriptFailure, msg.Code)
    Assert.Equal("not allowed", msg.Message)

[<Fact>]
let ``an exception escaping a host function is a RuntimeError that does not carry its message`` () =
    // Every statement wraps an escaping exception as RuntimeError. A host function
    // that throws something other than JyroError.Host reaches the host through this path.
    // The exception's message is a sentence of the platform or the host, which
    // 130.6.2 (JYRO-REQ-2054) keeps out of the arguments, and so out of the message
    // made from them; a deliberate refusal is a throw of JyroError.Host (JYRO-ADR-124).
    let thrower =
        { new IJyroFunction with
            member _.Name = "Explode"
            member _.Signature = JyroFunctionSignature.Create(ParameterType.Null)
            member _.Effect = FunctionEffect.Pure
            member _.Execute(_, _) = failwith "the widget store is closed" }

    let result =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().AddFunction(thrower).WithSource("Explode()"))
            (fun r -> r.WithInput(JyroObject()))

    Assert.False(result.IsSuccess)
    let msg = List.ofSeq result.Messages |> List.ofSeq |> List.find (fun m -> m.Severity = MessageSeverity.Error)
    Assert.Equal(MessageCode.RuntimeError, msg.Code)
    Assert.DoesNotContain("widget store", msg.Message)
    for a in msg.Args do
        Assert.DoesNotContain("widget store", Convert.ToString(a, Globalization.CultureInfo.InvariantCulture))

// -- A seeded Output -------------------------------------------------------
// Input is frozen, so a script cannot transform anything reached through it.
// A host that wants a structure edited rather than rebuilt hands it over as
// Output instead.

[<Fact>]
let ``a seeded Output is what the script edits, and the host sees the edits`` () =
    let row = JyroObject()
    row.["item"] <- JyroString("widget") :> JyroValue
    let rows = JyroArray()
    rows.Add(row)

    let result =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output[0].stamped = \"yes\""))
            (fun r -> r.WithInput(JyroObject()).WithOutput(rows))

    Assert.True(result.IsSuccess)

    // A run starts its Output from a copy of the seed (JYRO-REQ-1929): the edit is in the result, and the
    // host's own object is as it was.
    let edited = Option.get result.TryValue :?> JyroArray
    Assert.Equal("yes", (edited.[0].GetProperty("stamped")).ToStringValue())
    Assert.Equal("widget", (edited.[0].GetProperty("item")).ToStringValue())
    Assert.Equal("{\"item\":\"widget\"}", row.ToJson())

[<Fact>]
let ``an unseeded Output is still the empty object`` () =
    let result =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output.n = 1"))
            (fun r -> r.WithInput(JyroObject()))

    Assert.True(result.IsSuccess)
    let output = Assert.IsType<JyroObject>(Option.get result.TryValue)
    Assert.Equal("1", (output.GetProperty("n")).ToStringValue())

[<Fact>]
let ``a seeded Output is writable even though it came from the host`` () =
    // Freezing applies to Input alone. A host that seeds Output with a graph it
    // also passed as Input would find it frozen; these are separate objects.
    let seed = JyroObject()
    seed.["count"] <- JyroNumber(1.0) :> JyroValue

    let result =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output.count = Output.count + 1"))
            (fun r -> r.WithInput(JyroObject()).WithOutput(seed))

    Assert.True(result.IsSuccess)
    // The script wrote to the run's copy of the seed (JYRO-REQ-1929); the host's seed is as it was.
    Assert.Equal("2", (Option.get result.TryValue).GetProperty("count").ToStringValue())
    Assert.Equal("1", (seed.GetProperty("count")).ToStringValue())

[<Fact>]
let ``a failure can carry the value execution produced`` () =
    let r = JyroResult<string>.Failure("partial", DiagnosticMessage.Error(MessageCode.StatementLimitExceeded, MessageStage.Execute))
    Assert.False(r.IsSuccess)
    Assert.Equal(Some "partial", r.TryValue)
    Assert.Single(r.Messages) |> ignore

[<Fact>]
let ``a failed run keeps the output built so far`` () =
    // The end-to-end property 130.2 exists for. The limit trips after Output
    // has been populated, and what was written must survive the failure.
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib()
                .WithSource("Output.before = 'kept'\nvar i = 0\nwhile i < 100 do\n  i = i + 1\nend"))
            (fun r ->
                r.WithExecutionOptions({ JyroExecutionOptions.Default with MaxLoopIterations = 5 })
                 .WithInput(JyroObject() :> JyroValue))
    Assert.False(r.IsSuccess)
    Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.LoopIterationLimitExceeded)
    Assert.True(r.TryValue.IsSome, "a run that began must report its output")
    Assert.Equal("kept", (r.TryValue.Value.GetProperty("before")).ToStringValue())

[<Fact>]
let ``a compile failure reports no value`` () =
    // The other half of 130.2: absent only where execution never began.
    let r = Runner.compileThenRun (JyroCompiler().WithStdlib().WithSource("Output.v = (((")) id
    Assert.False(r.IsSuccess)
    Assert.True(r.TryValue.IsNone)

// -- The throwing entry point loses nothing -------------------------------

[<Fact>]
let ``CompileOrThrow carries the diagnostics, not a joined string`` () =
    // It used to raise InvalidOperationException with the message texts joined
    // by "; ", discarding every code and location - so the throwing form
    // reported strictly less than the JyroResult form.
    //
    // An undeclared variable is caught by the validator, so this is the
    // compile-side throwing entry point. Its run-side twin is below: the two
    // used to be one method on one type, and only one of them had a throwing
    // form at all.
    let ex =
        Assert.ThrowsAny<JyroException>(fun () ->
            JyroCompiler().WithStdlib().WithSource("Output.v = zzz").CompileOrThrow() |> ignore)
    Assert.Contains(ex.Messages, fun m -> m.Code = MessageCode.UndeclaredVariable)
    Assert.True(ex.Messages.[0].Location.HasValue, "the location must survive")

[<Fact>]
let ``ExecuteOrThrow carries the diagnostics of a run-stage failure`` () =
    let program = JyroCompiler().WithStdlib().WithSource("Output.v = 1 / 0").CompileOrThrow()
    let ex =
        Assert.ThrowsAny<JyroException>(fun () ->
            JyroRunner(program).WithInput(JyroObject()).ExecuteOrThrow() |> ignore)
    Assert.Contains(ex.Messages, fun m -> m.Code = MessageCode.DivisionByZero)

// -- StructuredDiagnostic distinguishes "no location" from line 0 ---------

[<Fact>]
let ``toStructured keeps location as an option`` () =
    // Previously flattened to Line/Column/Length ints with 0/0/0 for absent,
    // which a consumer could not tell from a real position.
    let located =
        DiagnosticFormatter.toStructured(
            DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, Nullable(SourceLocation(3, 7, 1))))
    Assert.True(located.Location.HasValue)
    Assert.Equal(3, located.Location.Value.Line)

    let unlocated = DiagnosticFormatter.toStructured(DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute))
    Assert.True(unlocated.Location.HasValue |> not)

// ---------------------------------------------------------------------------
// Subsystem mapping
// ---------------------------------------------------------------------------

/// `toStructured` reports the pipeline stage a diagnostic came from, derived
/// from its numeric band. The mapping is a `match` with a catch-all returning
/// `"unknown"`, so a band added to the enum without a matching arm answers
/// rather than fails - and that is exactly what happened: the whole 7xxx
/// artifact band, six codes, reported `"unknown"` to any host filtering by
/// subsystem. These walk the enum so the next band cannot slip through.

let private structuredFor (code: MessageCode) =
    DiagnosticFormatter.toStructured (DiagnosticMessage.Error(code, MessageStage.Execute, [||]))

[<Fact>]
let ``every declared code maps to a named subsystem`` () =
    let unmapped =
        allCodes
        |> List.filter (fun c -> (structuredFor c).Subsystem = "unknown")
    Assert.True(unmapped.IsEmpty, sprintf "codes with no subsystem arm: %A" unmapped)

[<Fact>]
let ``the artifact band reports itself as the artifact stage`` () =
    // The band this test was written for. Loading a .jyrx is its own pipeline
    // stage and had been given its own code range, but not its own arm.
    Assert.Equal("artifact", (structuredFor MessageCode.ArtifactTooLarge).Subsystem)
    Assert.Equal("artifact", (structuredFor MessageCode.ArtifactEffectExceedsMax).Subsystem)

[<Fact>]
let ``a compile-phase timeout is reported against the compiler, not the runtime`` () =
    // It used to borrow ExecutionTimeLimitExceeded, a 59xx code, so a timeout
    // during compilation arrived carrying Subsystem = "runtime" on a message
    // about compilation. Nothing had run yet.
    Assert.Equal("compiler", (structuredFor MessageCode.CompilationTimeLimitExceeded).Subsystem)
    Assert.Equal("runtime", (structuredFor MessageCode.ExecutionTimeLimitExceeded).Subsystem)

[<Fact>]
let ``each subsystem name is one a host can switch on`` () =
    // Guards against a typo introducing a second spelling of a stage.
    let known = set [ "lexer"; "parser"; "validator"; "linker"; "runtime"; "compiler"; "artifact" ]
    let strays =
        allCodes |> List.map (fun c -> (structuredFor c).Subsystem) |> List.distinct
        |> List.filter (fun s -> not (known.Contains s))
    Assert.True(strays.IsEmpty, sprintf "unexpected subsystem names: %A" strays)

// ---------------------------------------------------------------------------
// exit is control flow, not a diagnostic
// ---------------------------------------------------------------------------

[<Fact>]
let ``exit from inside a function terminates the script cleanly`` () =
    // The behavior that used to depend on ScriptReturn being a
    // JyroRuntimeException: withLocationTracking wraps every statement in a
    // catch-all that rewrites any escaping exception into RuntimeError, so the
    // signal has to be rethrown ahead of it or a successful exit is reported
    // as a crash.
    // `Output` is unreachable inside a function body, so the marker is written
    // either side of the call rather than within it.
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib()
                .WithSource("func Stop()\n  exit\nend\nOutput.before = true\nStop()\nOutput.after = true"))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    let out = Option.get r.TryValue
    Assert.True(Runner.asBool (Runner.prop "before" out))
    // The statement after the call never runs: exit escapes the whole stack.
    Assert.True((Runner.prop "after" out).IsNull)

[<Fact>]
let ``exit deep inside nested calls still escapes the whole stack`` () =
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib()
                .WithSource("func Inner()\n  exit\nend\nfunc Outer()\n  Inner()\nend\nOuter()\nOutput.unreachable = true"))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    Assert.True((Runner.prop "unreachable" (Option.get r.TryValue)).IsNull)

[<Fact>]
let ``a genuine failure inside a function is still a failure`` () =
    // The other half: narrowing what escapes must not make real errors escape
    // too. A crash inside a function still arrives as a diagnostic.
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("func Boom()\n  var x = 1 / 0\nend\nBoom()"))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))
    Assert.False(r.IsSuccess)
    Assert.Contains(MessageCode.DivisionByZero, List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code))

// ---------------------------------------------------------------------------
// The deliberate host-failure channel
// ---------------------------------------------------------------------------
//
// Throwing the exception that `JyroError.Host` makes is the documented way for a host function to
// report a condition it chose to reject, and it is the only emitter of
// `HostFunctionError` (5006). Nothing called it and no test asserted the code,
// so the one path that keeps a deliberate refusal distinguishable from a crash
// was never exercised - and that distinction is the whole reason the code
// exists beside `RuntimeError`.

/// Rejects its argument on purpose, the way a host function reports a business
/// rule rather than a fault.
type private RefusingFunction() =
    inherit JyroFunctionBase("MustBePositive",
        JyroFunctionSignature.Create(ParameterType.Number, Parameter.Required("value", ParameterType.Number)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _) =
        let n = args.GetNumber(0)
        // A reason the host names, and the value that changes (JYRO-ADR-124).
        if n <= 0.0 then raise (JyroError.Host("ValueNotPositive", box n))
        else JyroNumber(n) :> JyroValue

/// Crashes, so the two can be told apart.
type private CrashingFunction() =
    inherit JyroFunctionBase("Crash", JyroFunctionSignature.Create(ParameterType.Number, Parameter.Required("value", ParameterType.Number)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(_, _) = failwith "something went wrong inside the host"

let private runWith (fn: IJyroFunction) (src: string) =
    Runner.compileThenRun
        (JyroCompiler().WithStdlib().AddFunction(fn).WithSource(src))
        (fun r -> r.WithInput(JyroObject() :> JyroValue))

[<Fact>]
let ``throwing JyroError.Host reports HostFunctionError, carrying the host reason and values`` () =
    // JYRO-ADR-124 item 2: the arguments are the reason, then the values, in order.
    let r = runWith (RefusingFunction()) "Output.v = MustBePositive(0 - 5)"
    Assert.False(r.IsSuccess)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.HostFunctionError)
    Assert.Equal(2, m.Args.Length)
    Assert.Equal("ValueNotPositive", string m.Args.[0])
    Assert.Equal(-5.0, Convert.ToDouble(m.Args.[1], Globalization.CultureInfo.InvariantCulture))
    Assert.Contains("ValueNotPositive", m.Message)

/// Refuses with a reason and no values, through the overload JYRO-ADR-117 requires.
type private RefusingWithoutValues() =
    inherit JyroFunctionBase("Unavailable",
        JyroFunctionSignature.Create(ParameterType.Null))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(_, _) = raise (JyroError.Host("ResourceUnavailable"))

[<Fact>]
let ``throwing JyroError.Host with a reason alone reports HostFunctionError whose one argument is the reason`` () =
    let r = runWith (RefusingWithoutValues()) "Unavailable()"
    Assert.False(r.IsSuccess)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.HostFunctionError)
    Assert.Equal<string list>([ "ResourceUnavailable" ], m.Args |> Array.map string |> List.ofArray)
    Assert.Contains("ResourceUnavailable", m.Message)

[<Fact>]
let ``JyroError.Host reports HostFunctionError, carrying the reason and the values`` () =
    let fn =
        { new IJyroFunction with
            member _.Name = "Refuse"
            member _.Signature = JyroFunctionSignature.Create(ParameterType.Null)
            member _.Effect = FunctionEffect.Pure
            member _.Execute(_, _) = raise (JyroError.Host("QuotaExceeded", box 3, box "orders")) }
    let r = runWith fn "Refuse()"
    Assert.False(r.IsSuccess)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.HostFunctionError)
    Assert.Equal<string list>([ "QuotaExceeded"; "3"; "orders" ], m.Args |> Array.map (fun a -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture)) |> List.ofArray)

[<Fact>]
let ``a deliberate refusal is distinguishable from a crash`` () =
    // The distinction the code exists for. Both fail the run; only one of them
    // is a bug in the host function.
    let refused = runWith (RefusingFunction()) "Output.v = MustBePositive(0 - 5)"
    Assert.Contains(refused.Messages, fun m -> m.Code = MessageCode.HostFunctionError)
    Assert.DoesNotContain(refused.Messages, fun m -> m.Code = MessageCode.RuntimeError)

    let crashed = runWith (CrashingFunction()) "Output.v = Crash(1)"
    Assert.Contains(crashed.Messages, fun m -> m.Code = MessageCode.RuntimeError)
    Assert.DoesNotContain(crashed.Messages, fun m -> m.Code = MessageCode.HostFunctionError)

[<Fact>]
let ``a host function that refuses does not stop one that does not`` () =
    // The accepting path has to keep working, or the test above would pass over
    // a function that always refused.
    let r = runWith (RefusingFunction()) "Output.v = MustBePositive(7)"
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))

// ---------------------------------------------------------------------------
// The severity model is real, and only Error is engine-authored
// ---------------------------------------------------------------------------

[<Fact>]
let ``all three severities can be built and keep their severity`` () =
    // `Info` had a public factory and no user anywhere - not even a test - so
    // the one severity nothing produces was also the one nothing checked.
    Assert.Equal(MessageSeverity.Error, (DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute)).Severity)
    Assert.Equal(MessageSeverity.Warning, (DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute)).Severity)
    Assert.Equal(MessageSeverity.Info, (DiagnosticMessage.Info(MessageCode.DivisionByZero, MessageStage.Execute)).Severity)

[<Fact>]
let ``only an Error decides success`` () =
    // Why the severity model is load-bearing even though the engine authors
    // only errors: IsSuccess is derived from the messages, so a host attaching
    // a warning or a note to a result does not turn it into a failure.
    let info = JyroResult<int>.Success(1, [ DiagnosticMessage.Info(MessageCode.DivisionByZero, MessageStage.Execute) ])
    Assert.True(info.IsSuccess)
    let warned = JyroResult<int>.Success(1, [ DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute) ])
    Assert.True(warned.IsSuccess)
    let failed = JyroResult<int>.Failure(DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute))
    Assert.False(failed.IsSuccess)

[<Fact>]
let ``the engine itself authors only Error-severity diagnostics`` () =
    // Stated as a fact about the engine rather than left implicit. The
    // validator's unused AddWarning is gone; if a real warning is ever added,
    // this test is where the change gets noticed and argued.
    let sources =
        [ "Output.v = 1 / 0"; "Output.v = Nope()"; "Output.v = ("
          "var a = 1\nvar a = 2"; "Input.x = 1"; "Output.v = Log(0)" ]
    for src in sources do
        let r = Runner.run src (JyroObject() :> JyroValue)
        for m in r.Messages do
            Assert.Equal(MessageSeverity.Error, m.Severity)

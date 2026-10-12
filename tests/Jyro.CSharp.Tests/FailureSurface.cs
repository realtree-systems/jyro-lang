using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.Json;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// What the shipped surface DOES when you give it something it cannot use, as a
// checked-in inventory.
//
// This is the deliberate twin of PublicApiSurface.cs, and it exists because
// that file cannot do this job. PublicApi.approved.txt is rendered from CLR
// reflection metadata, and metadata records no throws - there is no position in
// that rendering where an exception type could ever appear. DiagnosticCoverage-
// Tests walks the whole MessageCode enum and checks that every code has a
// well-formed, translatable template, but those are properties OF the
// catalog: ArtifactUnreadable satisfied every one of them while wired to the
// opposite half of the pipeline.
//
// So the package had two ratchets and neither could see which condition
// produces which failure. Three separate tests were then found asserting the
// defective answer on exactly that axis - a read code on a write failure, and a
// raw System.Text.Json.JsonException escaping a member on the documented
// catch(JyroRuntimeException) path, asserted twice including once in THIS
// project, which exists specifically to see what an external host sees.
//
// Why a table rather than a rule. Each probe below states one condition and one
// answer. A test that needs an observable in order to see its own subject is
// how the wrong answers got written down in the first place: the author needed
// something to assert, sampled whatever the code already emitted, and the value
// then got no review because review follows the test's stated subject. Here the
// value IS the subject, for every line, and a change to any of them shows up as
// a diff rather than as a green test.
//
// To accept a deliberate change:
//
//     JYRO_APPROVE_FAILURES=1 dotnet test tests/Jyro.CSharp.Tests
//
// then read the diff to FailureSurface.approved.txt before committing it. A
// line that moves from JyroRuntimeException to a BCL type, or from one
// MessageCode to another, is the thing this file exists to make visible.
public class FailureSurface
{
    // Each probe: a name, and an action that provokes one failure condition
    // through a public entry point. Ordered by area, then by name, so the
    // approved file is stable and reviewable.
    // `HostContract` marks a raise that is NOT Jyro reporting a failure: either
    // an explicit BCL interface implementation behaving as its own interface
    // requires, or a host's own exception passing through code that deliberately
    // does not swallow it. The leak check below is about what Jyro raises, so it
    // skips these - and the reason lives on the probe rather than in a list of
    // names somewhere else, because a list of names is where the next thing
    // hides.
    private static (string Name, Action Run, bool HostContract) Jyro(string name, Action run) =>
        (name, run, false);

    /// A raise that is NOT Jyro reporting a failure - see the note above.
    private static (string Name, Action Run, bool HostContract) Host(string name, Action run) =>
        (name, run, true);

    private static IEnumerable<(string Name, Action Run, bool HostContract)> Probes()
    {
        var program = new JyroCompiler().WithStdlib().WithSource("Output.v = Input").CompileOrThrow();
        var empty = new JyroObject();

        // -- Values a host constructs directly --------------------------------
        yield return Jyro("JyroNumber(NaN)", () => { _ = new JyroNumber(double.NaN); });
        yield return Jyro("JyroNumber(Infinity)", () => { _ = new JyroNumber(double.PositiveInfinity); });
        yield return Jyro("JyroNumber(1e300).ToInt32", () => { _ = new JyroNumber(1e300).ToInt32(); });
        yield return Jyro("JyroNumber(1e300).ToInt64", () => { _ = new JyroNumber(1e300).ToInt64(); });
        yield return Jyro("JyroNumber(1e300).DecimalValue", () => { _ = new JyroNumber(1e300).DecimalValue; });
        yield return Jyro("JyroString(null)", () => { _ = new JyroString(null!); });
        yield return Jyro("JyroString.ToInt32", () => { _ = new JyroString("x").ToInt32(); });
        yield return Jyro("JyroString.AsArray", () => { _ = new JyroString("x").AsArray(); });

        // -- Container doors ---------------------------------------------------
        yield return Jyro("JyroArray.Insert past the end", () => Two().Insert(9, new JyroNumber(1)));
        yield return Jyro("JyroArray.Insert negative", () => Two().Insert(-1, new JyroNumber(1)));
        yield return Jyro("JyroArray.RemoveAt past the last", () => Two().RemoveAt(9));
        yield return Jyro("JyroArray.RemoveAt on empty", () => new JyroArray().RemoveAt(0));
        yield return Jyro("JyroArray index write past the end", () => { Two()[9] = new JyroNumber(1); });
        yield return Jyro("JyroArray mutate when frozen", () =>
        {
            var a = Two();
            a.Freeze();
            a.Add(new JyroNumber(1));
        });

        // -- Host conversion ---------------------------------------------------
        yield return Jyro("JyroValue.FromJson malformed", () => { _ = JyroValue.FromJson("{oops"); });
        yield return Jyro("JyroValue.FromJson null", () => { _ = JyroValue.FromJson((string)null!); });
        yield return Jyro("JyroValue.FromJson non-finite magnitude", () =>
            { _ = JyroValue.FromJson("{\"v\": 1e400}"); });
        yield return Jyro("JyroValue.FromJson stream malformed", () =>
        {
            using var s = new MemoryStream(Encoding.UTF8.GetBytes("{oops"));
            _ = JyroValue.FromJson(s);
        });
        yield return Jyro("JyroValue.FromJson stream null", () => { _ = JyroValue.FromJson((Stream)null!); });
        yield return Jyro("JyroValue.FromObject NaN", () => { _ = JyroValue.FromObject(double.NaN); });
        // JYRO-REQ-1903: a delegate, reflection metadata or a task is refused, with HostValueNotConvertible.
        yield return Jyro("JyroValue.FromObject delegate", () => { _ = JyroValue.FromObject((Action)(() => { })); });
        yield return Jyro("JyroValue.FromObject Type", () => { _ = JyroValue.FromObject(typeof(string)); });
        yield return Jyro("JyroValue.FromObject Task",
            () => { _ = JyroValue.FromObject(System.Threading.Tasks.Task.FromResult(1)); });
        // JYRO-REQ-1901: Add refuses a key the object has; ArgumentException carries no name in this rendering.
        yield return Jyro("JyroObject.Add duplicate key", () =>
            { var o = new JyroObject(); o.Add("a", new JyroNumber(1)); o.Add("a", new JyroNumber(2)); });

        // -- The null rule on members added after it was settled ---------------
        yield return Jyro("JyroArray.WriteJson null writer", () => new JyroArray().WriteJson(null!));
        yield return Jyro("JyroObject.WriteJson null writer", () => new JyroObject().WriteJson(null!));
        yield return Jyro("JyroObject.GetProperty null key", () => { _ = new JyroObject().GetProperty(null!); });
        yield return Jyro("JyroObject.SetProperty null key",
            () => new JyroObject().SetProperty(null!, new JyroNumber(1)));
        yield return Jyro("JyroObject.TryGetValue null key",
            () => { _ = new JyroObject().TryGetValue(null!, out _); });
        yield return Jyro("JyroObject.Remove null key", () => { _ = new JyroObject().Remove(null!); });
        yield return Jyro("JyroObject indexer null key", () => { _ = new JyroObject()[null!]; });
        yield return Jyro("JyroValue.CompareForSort null left",
            () => { _ = JyroValue.CompareForSort(null!, new JyroNumber(1)); });
        yield return Jyro("JyroValue.CompareForSort null right",
            () => { _ = JyroValue.CompareForSort(new JyroNumber(1), null!); });
        yield return Jyro("JyroArray.Add null value", () => new JyroArray().Add(null!));
        yield return Jyro("JyroObject.SetProperty null value",
            () => new JyroObject().SetProperty("k", null!));

        // -- Diagnostics: the formatting surface a host drives ------------------
        yield return Jyro("DiagnosticMessage.Error null args", () =>
            { _ = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, (object[])null!).Message; });
        yield return Jyro("DiagnosticFormatter.FormatMessage null", () =>
            { _ = DiagnosticFormatter.FormatMessage(null!); });
        yield return Jyro("DiagnosticFormatter.ToStructured null", () =>
            { _ = DiagnosticFormatter.ToStructured(null!); });
        yield return Jyro("DiagnosticFormatter.FormatLocalized null provider", () =>
            { _ = DiagnosticFormatter.FormatLocalized(null!, DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute)); });
        // Host contract: the exception is the host's own, thrown from its IMessageTemplateProvider. Swallowing it would hide the host's bug rather than report ours - the line already drawn for a POCO property getter that throws in FromObject.
        yield return Host("DiagnosticFormatter.FormatLocalized throwing provider", () =>
            { _ = DiagnosticFormatter.FormatLocalized(new ThrowingTemplates(),
                     DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute)); });
        yield return Jyro("MessageTemplates.Format null args", () =>
            { _ = MessageTemplates.Format(MessageCode.DivisionByZero, null!); });
        // JYRO-REQ-1891 and JYRO-REQ-1892: the factories return the exception and do not throw it, except
        // that Runtime refuses a code that only the engine raises, or one that is no member of MessageCode,
        // by throwing ArgumentOutOfRangeException whose ParamName is "code".
        yield return Jyro("JyroError.Host returns the exception", () =>
            { _ = JyroError.Host("AmountNotPositive", -5); });
        yield return Jyro("JyroError.Runtime returns the exception", () =>
            { _ = JyroError.Runtime(MessageCode.InvalidJson); });
        yield return Jyro("JyroError.Runtime too few args returns the exception", () =>
            { _ = JyroError.Runtime(MessageCode.IndexOutOfRange, new object[] { "Array" }); });
        yield return Jyro("JyroError.Runtime engine-only code", () =>
            { _ = JyroError.Runtime(MessageCode.StatementLimitExceeded); });
        yield return Jyro("JyroError.Runtime code below 5000", () =>
            { _ = JyroError.Runtime(MessageCode.UndefinedFunction); });
        yield return Jyro("JyroError.Runtime canceled by host", () =>
            { _ = JyroError.Runtime(MessageCode.CanceledByHost); });
        yield return Jyro("JyroError.Runtime not a member", () =>
            { _ = JyroError.Runtime((MessageCode)9999); });
        yield return Jyro("JyroResult.Failure null sequence", () =>
            { _ = JyroResult<int>.Failure((IEnumerable<DiagnosticMessage>)null!); });
        // A failure has to carry something that makes it one. Both of these
        // used to build a result with IsSuccess = false and no error to justify
        // it, which is the "if and only if" broken from the other side.
        yield return Jyro("JyroResult.Failure no messages", () =>
            { _ = JyroResult<int>.Failure(Array.Empty<DiagnosticMessage>()); });
        yield return Jyro("JyroResult.Failure warnings only", () =>
            { _ = JyroResult<int>.Failure(
                new[] { DiagnosticMessage.Warning(MessageCode.RuntimeError, MessageStage.Execute) }); });

        // -- Argument helpers with an index the host computed wrongly -----------
        yield return Jyro("Get negative index",
            () => Report(RunWithFunction(new BadIndexFunction(), "Output.v = BadIndex(1)", empty)));

        // -- Limits and configuration ------------------------------------------
        yield return Jyro("JyroRunner.WithExecutionOptions null then Execute",
            () => Report(new JyroRunner(program).WithExecutionOptions(null!).Execute()));
        yield return Jyro("JyroCompiler.AddFunction null then Compile",
            () => Report(new JyroCompiler().WithStdlib().AddFunction(null!)
                             .WithSource("Output.v = 1").Compile()));
        yield return Jyro("JyroCompiler.AddFunctions null", () => new JyroCompiler().AddFunctions(null!));
        yield return Jyro("JyroCompiler.AddLibrary null", () => new JyroCompiler().AddLibrary(null!));

        // -- One-shot helpers ---------------------------------------------------
        yield return Jyro("JyroScript.ExecuteScriptWithJson malformed",
            () => Report(JyroScript.ExecuteScriptWithJson("Output.v = 1", "{ not json")));

        // -- The interface views, whose answers differ from the class doors -----
        // These were marked as the BCL's contract rather than ours, and that
        // reading was overruled: every door onto a container now gives one
        // answer, so a miss is null whichever reference you hold. Both
        // array doors and all three object doors are probed, because the point
        // is that they agree - and because the class indexer and a cast have the
        // *same static type*, so a disagreement between them was invisible in a
        // signature.
        yield return Jyro("array door: class indexer out of range", () =>
            { _ = Two()[9]; });
        yield return Jyro("array door: IReadOnlyList out of range", () =>
            { _ = ((IReadOnlyList<JyroValue>)Two())[9]; });
        yield return Jyro("object door: class indexer missing key", () =>
            { _ = new JyroObject()["nope"]; });
        yield return Jyro("object door: GetProperty missing key", () =>
            { _ = new JyroObject().GetProperty("nope"); });
        yield return Jyro("object door: IReadOnlyDictionary missing key", () =>
            { _ = ((IReadOnlyDictionary<string, JyroValue>)new JyroObject())["nope"]; });

        // -- Encoding a string the target cannot carry -------------------------
        yield return Jyro("ToJson with an unpaired surrogate", () =>
            { var o = new JyroObject();
              o.SetProperty("s", new JyroString(new string((char)0xD800, 1)));
              _ = o.ToJson(); });
        yield return Jyro("Base64Encode an unpaired surrogate",
            () => Report(RunScript("Output.v = Base64Encode(Input.s)",
                                   MakeObject("s", new string((char)0xD800, 1)))));

        // -- Compile-side configuration ---------------------------------------
        yield return Jyro("JyroCompiler.WithSource null", () => new JyroCompiler().WithSource(null!));
        yield return Jyro("JyroArtifactLoader ctor null artifact",
            () => { _ = new JyroArtifactLoader(null!); });
        yield return Jyro("JyroCompiler.AddLibrarySource null",
            () => new JyroCompiler().AddLibrarySource(null!));
        yield return Jyro("JyroLibrary.Parse null", () => { _ = JyroLibrary.Parse(null!); });

        // -- Run-side configuration -------------------------------------------
        yield return Jyro("JyroRunner ctor null program", () => { _ = new JyroRunner(null!); });
        yield return Jyro("JyroRunner.WithInputJson string null", () => new JyroRunner(program!).WithInputJson((string)null!));
        yield return Jyro("JyroRunner.WithInputJson stream null", () => new JyroRunner(program!).WithInputJson((Stream)null!));

        // -- Terminal methods: these REPORT rather than raise -------------------
        yield return Jyro("JyroCompiler.Compile with no source",
            () => Report(new JyroCompiler().Compile()));
        yield return Jyro("JyroCompiler.Compile syntax error",
            () => Report(new JyroCompiler().WithStdlib().WithSource("Output.v = (").Compile()));
        yield return Jyro("JyroCompiler.Compile unknown function",
            () => Report(new JyroCompiler().WithStdlib().WithSource("Output.v = Nope()").Compile()));
        yield return Jyro("JyroArtifactLoader garbage then Load",
            () => Report(new JyroArtifactLoader(new byte[] { 1, 2, 3, 4 }).WithStdlib().Load()));
        yield return Jyro("JyroRunner.WithInputJson string malformed then Execute",
            () => Report(new JyroRunner(program!).WithInputJson("{ not json").Execute()));
        yield return Jyro("JyroRunner.WithInputJson stream malformed then Execute", () =>
        {
            using var s = new MemoryStream(Encoding.UTF8.GetBytes("{ not json"));
            Report(new JyroRunner(program!).WithInputJson(s).Execute());
        });

        // -- Script faults, reported through Execute ---------------------------
        foreach (var (name, script) in new[]
        {
            ("divide by zero", "Output.v = 1 / 0"),
            ("modulo by zero", "Output.v = 1 % 0"),
            ("numeric overflow", "Output.v = Power(1e300, 3)"),
            ("numeric domain", "Output.v = Log(0)"),
            ("reversed range", "Output.v = Clamp(5, 10, 1)"),
            ("date out of range", "Output.v = DateAdd('2024-01-15', 20000, 'years')"),
            ("integer out of range", "Output.v = Skip([1,2,3], 1e20)"),
            ("fractional integer", "Output.v = Skip([1,2,3], 1.5)"),
            ("negative quantity", "Output.v = PadLeft('a', -5)"),
            ("index out of range", "Output.v = RemoveAt([1,2,3], 99)"),
            ("non-numeric element", "Output.v = Sum([1, 'a'])"),
            ("incomparable types", "Output.v = true < false"),
            ("script fail", "fail 'stop here'"),
            ("write to Input", "Input.x = 1"),
        })
        {
            var s = script;
            yield return Jyro($"script: {name}", () =>
                Report(RunScript(s, empty)));
        }

        // A host function that refuses on purpose. HostFunctionError (5006) was
        // absent from this inventory because nothing in the repository called
        // throw JyroError.Host(...) - the one emitter of the code that keeps a deliberate
        // refusal distinguishable from a crash.
        yield return Jyro("host function refuses via JyroError.Host", () =>
            Report(RunWithFunction(new RefusingHostFunction(), "Output.v = MustBePositive(0 - 1)", empty)));
        yield return Jyro("host function crashes", () =>
            Report(RunWithFunction(new CrashingHostFunction(), "Output.v = Crash(1)", empty)));

        static JyroArray Two()
        {
            var a = new JyroArray();
            a.Add(new JyroNumber(1));
            a.Add(new JyroNumber(2));
            return a;
        }
    }

    // A reported failure is signaled by throwing this, so one probe shape
    // covers both "raises" and "returns a failing result". The distinction is
    // preserved in the rendered line, which is the point.
    private sealed class ReportedFailure : Exception
    {
        public ReportedFailure(string codes) : base(codes) { }
    }

    private static void Report<T>(JyroResult<T> result) where T : notnull
    {
        if (result.IsSuccess) return;
        var codes = string.Join("+", result.Messages.Select(m => m.Code.ToString()).Distinct().OrderBy(c => c, StringComparer.Ordinal));
        throw new ReportedFailure(codes);
    }

    private static JyroResult<JyroValue> RunScript(string source, JyroValue input)
    {
        var compiled = new JyroCompiler().WithStdlib().WithSource(source).Compile();
        if (!compiled.IsSuccess)
        {
            var codes = string.Join("+", compiled.Messages.Select(m => m.Code.ToString()).Distinct().OrderBy(c => c, StringComparer.Ordinal));
            throw new ReportedFailure(codes);
        }
        compiled.TryGetValue(out var program);
        return new JyroRunner(program!).WithInput(input).Execute();
    }

    private static List<string> DescribeFailures()
    {
        var lines = new List<string>();
        foreach (var (name, run, _) in Probes())
        {
            string outcome;
            try
            {
                run();
                outcome = "no failure";
            }
            catch (ReportedFailure r)
            {
                outcome = "reports " + r.Message;
            }
            catch (JyroRuntimeException ex)
            {
                outcome = "raises JyroRuntimeException | " + ex.Code;
            }
            catch (JyroException)
            {
                outcome = "raises JyroException";
            }
            catch (ArgumentOutOfRangeException ex)
            {
                // JYRO-REQ-1892: the name of the parameter is part of the contract here too.
                outcome = "raises ArgumentOutOfRangeException | " + ex.ParamName;
            }
            catch (ArgumentNullException ex)
            {
                // The parameter name is part of the contract: a
                // NullReferenceException cannot carry one, which is how the
                // four null doors were found.
                outcome = "raises ArgumentNullException | " + ex.ParamName;
            }
            catch (Exception ex)
            {
                outcome = "raises " + ex.GetType().FullName;
            }
            lines.Add($"{name,-52} -> {outcome}");
        }
        lines.Sort(StringComparer.Ordinal);
        return lines;
    }

    [Fact]
    public void TheFailureSurfaceMatchesTheApprovedInventory()
    {
        var actual = DescribeFailures();
        var path = ApprovedFilePath();

        if (Environment.GetEnvironmentVariable("JYRO_APPROVE_FAILURES") == "1")
        {
            File.WriteAllText(path, string.Join(Environment.NewLine, actual) + Environment.NewLine);
            return;
        }

        Assert.True(File.Exists(path), $"The approved failure inventory is missing: {path}");

        var approved = File.ReadAllLines(path).Where(l => l.Length > 0).ToList();
        var added = actual.Except(approved, StringComparer.Ordinal).ToList();
        var removed = approved.Except(actual, StringComparer.Ordinal).ToList();

        var message = new StringBuilder("The failure surface no longer matches FailureSurface.approved.txt.");
        if (removed.Count > 0)
        {
            message.AppendLine().AppendLine().AppendLine("No longer produced:");
            foreach (var line in removed) message.AppendLine("  - " + line);
        }
        if (added.Count > 0)
        {
            message.AppendLine().AppendLine("Now produced:");
            foreach (var line in added) message.AppendLine("  + " + line);
        }
        message.AppendLine().AppendLine("If the change is intended, re-approve it and review the diff:")
               .AppendLine("  JYRO_APPROVE_FAILURES=1 dotnet test tests/Jyro.CSharp.Tests");

        Assert.True(added.Count == 0 && removed.Count == 0, message.ToString());
    }

    [Fact]
    public void NoProbeLeaksAnExceptionTypeAHostIsNotToldToCatch()
    {
        // Measured directly rather than trusted from the inventory, so it still
        // holds if the inventory is re-approved carelessly - the same
        // belt-and-braces shape as PublicApiSurface's F#-type check.
        //
        // The line, stated once so it is not re-drawn per probe:
        //
        //   Argument validation uses the BCL Argument* family. The caller
        //   passed something its own code should not have passed - a null
        //   source, a NaN to a constructor whose type is finite - and .NET
        //   hosts expect ArgumentNullException / ArgumentException there. Both
        //   name the offending parameter, which is the whole reason the four
        //   NullReferenceException doors were worth fixing.
        //
        //   Everything else - a data fault, a script fault, a limit, a
        //   condition the engine recognized - is a JyroRuntimeException
        //   carrying a MessageCode, or a reported diagnostic.
        //
        // The first run of this check flagged JyroNumber(NaN), which raises
        // ArgumentException by deliberate design (see the constructor's own
        // comment: a named code there would make a MISSED route look like a
        // handled one). That is argument validation, so it belongs on the
        // admitted side - and the rule above is what says so, rather than an
        // exemption list, which is where the next such thing would hide.
        //
        // ArgumentNullException derives from ArgumentException, so one clause
        // covers both; the rendered line still distinguishes them.
        // Built from the probes directly rather than from the rendered lines, so
        // the HostContract reason travels with the probe instead of being
        // re-derived from a name here.
        var hostContract = new HashSet<string>(
            Probes().Where(p => p.HostContract).Select(p => p.Name), StringComparer.Ordinal);

        var offenders = new List<string>();
        foreach (var line in DescribeFailures())
        {
            if (hostContract.Any(n => line.StartsWith(n, StringComparison.Ordinal))) continue;
            if (line.Contains("-> raises ") &&
                !line.Contains("raises JyroRuntimeException") &&
                !line.Contains("raises JyroException") &&
                !line.Contains("raises ArgumentNullException") &&
                !line.Contains("raises ArgumentOutOfRangeException") &&
                !line.Contains("raises System.ArgumentException"))
            {
                offenders.Add(line);
            }
        }

        Assert.True(offenders.Count == 0,
            "These entry points raise a type a host is not told to catch:"
            + Environment.NewLine
            + string.Join(Environment.NewLine, offenders));
    }

    /// A host template provider that throws, which a host can certainly write.
    private sealed class ThrowingTemplates : IMessageTemplateProvider
    {
        public bool TryGetTemplate(MessageCode code, out string template) =>
            throw new KeyNotFoundException("no template for " + code);
    }

    /// A host function that indexes its arguments with an offset it computed
    /// wrongly - args.Count - 2 on a one-argument call.
    private sealed class BadIndexFunction : JyroFunctionBase
    {
        public BadIndexFunction()
            : base("BadIndex",
                   JyroFunctionSignature.Create(ParameterType.Number,
                                                Parameter.Required("value", ParameterType.Number)))
        { }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx) =>
            args.Get<JyroNumber>(args.Count - 2);
    }

    private sealed class RefusingHostFunction : JyroFunctionBase
    {
        public RefusingHostFunction()
            : base("MustBePositive",
                   JyroFunctionSignature.Create(ParameterType.Number,
                                                Parameter.Required("value", ParameterType.Number)))
        { }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
        {
            var n = args.GetNumber(0);
            // A reason the host names, and the value that changes (JYRO-ADR-124).
            if (n <= 0) throw JyroError.Host("ValueNotPositive", n);
            return new JyroNumber(n);
        }
    }

    private sealed class CrashingHostFunction : JyroFunctionBase
    {
        public CrashingHostFunction()
            : base("Crash",
                   JyroFunctionSignature.Create(ParameterType.Number,
                                                Parameter.Required("value", ParameterType.Number)))
        { }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx) =>
            throw new InvalidOperationException("something went wrong inside the host");
    }

    private static JyroValue MakeObject(string key, string value)
    {
        var o = new JyroObject();
        o.SetProperty(key, new JyroString(value));
        return o;
    }

    private static JyroResult<JyroValue> RunWithFunction(IJyroFunction fn, string source, JyroValue input)
    {
        var compiled = new JyroCompiler().WithStdlib().AddFunction(fn).WithSource(source).Compile();
        if (!compiled.IsSuccess)
        {
            var codes = string.Join("+", compiled.Messages.Select(m => m.Code.ToString()).Distinct().OrderBy(c => c, StringComparer.Ordinal));
            throw new ReportedFailure(codes);
        }
        compiled.TryGetValue(out var program);
        return new JyroRunner(program!).WithInput(input).Execute();
    }

    private static string ApprovedFilePath([CallerFilePath] string thisFile = "") =>
        Path.Combine(Path.GetDirectoryName(thisFile)!, "FailureSurface.approved.txt");
}

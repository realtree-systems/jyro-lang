using System;
using System.Collections.Generic;
using System.Threading;
using RealTree.Jyro;
using Xunit;

// Every C# snippet in src/Jyro.Api/README.md, compiled.
//
// The shipped README's history is exactly "snippets that do not compile":
// the audit reproduced five broken examples against the built assemblies, and
// the cause was that nothing ever compiled the documented surface. This file
// closes that loop. Each region below carries a README snippet verbatim,
// adapted only with the scaffolding a snippet elides (an enclosing method, a
// variable the prose introduced). When the README changes, change this file
// in the same commit; when a snippet here stops compiling, the README is
// wrong again.
//
// The rule from HostSurfaceTests.cs applies here too: no using
// Microsoft.FSharp.* - a snippet that needs one has no place in the README.

namespace JyroHostSurface;

public class ReadmeSnippets
{
    // -- The repository root README's quick start ----------------------------

    [Fact]
    public void RootReadmeQuickStart()
    {
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.greeting = Input.name & ' is ' & Input.age & ' years old'")
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson("""{ "name": "Alice", "age": 30 }""")
            .Execute();

        if (result.TryGetValue(out var output))
            Console.WriteLine(output.ToJson()); // {"greeting":"Alice is 30 years old"}

        Assert.True(result.IsSuccess);
        Assert.Equal("""{"greeting":"Alice is 30 years old"}""", output!.ToJson());
    }

    // -- Build and run scripts (the opening example) -------------------------

    [Fact]
    public void MinimalExample()
    {
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.name = Input.name")
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson("""{ "name": "Alice" }""")
            .Execute();

        if (result.TryGetValue(out var output))
            Console.WriteLine(output.ToJson()); // {"name":"Alice"}

        Assert.True(result.IsSuccess);
        Assert.Equal("""{"name":"Alice"}""", output!.ToJson());
    }

    // -- Use the results ------------------------------------------------------

    [Fact]
    public void ResultsExample()
    {
        var runner = new JyroRunner(
                new JyroCompiler()
                    .WithStdlib()
                    .WithSource("Output.first = 1\nOutput.second = 1 / 0")
                    .CompileOrThrow())
            .WithInput(new JyroObject());

        var result = runner.Execute();

        if (!result.IsSuccess)
        {
            foreach (var msg in result.Messages)
                Console.Error.WriteLine(msg.Message);
        }

        // Partial output survives a failed run.
        if (result.TryGetValue(out var partial))
            Console.WriteLine(partial.ToJson());

        Assert.False(result.IsSuccess);
        Assert.Equal(1d, partial!.GetProperty("first").ToDouble());
    }

    [Fact]
    public void ResultFactories()
    {
        JyroValue myValue = new JyroString("ok");

        var success = JyroResult<JyroValue>.Success(myValue);

        var failure = JyroResult<JyroValue>.Failure(
            DiagnosticMessage.Error(MessageCode.RuntimeError, MessageStage.Execute, new object[] { "LoadOrder()" }));

        Assert.True(success.IsSuccess);
        Assert.False(failure.IsSuccess);
        Assert.Equal(MessageCode.RuntimeError, failure.Messages[0].Code);
        Assert.Equal(new object[] { "LoadOrder()" }, failure.Messages[0].Args);
        Assert.Contains("LoadOrder()", failure.Messages[0].Message);
    }

    // -- Construct Jyro values ------------------------------------------------

    [Fact]
    public void ConstructValues()
    {
        // Primitives
        var str = new JyroString("hello");
        var count = new JyroNumber(42);      // int: exact
        var price = new JyroNumber(19.99m);  // decimal: exact
        var ratio = new JyroNumber(0.5);     // double: inexact
        var flag = JyroBoolean.True;         // or JyroBoolean.False
        var nil = JyroNull.Instance;

        // Objects
        var obj = new JyroObject();
        obj.SetProperty("name", new JyroString("Alice"));
        obj.SetProperty("age", new JyroNumber(30));
        obj.SetProperty("active", JyroBoolean.True);

        // Arrays
        var arr = new JyroArray();
        arr.Add(new JyroString("one"));
        arr.Add(new JyroNumber(2));
        arr.Add(obj);

        Assert.True(count.IsExact);
        Assert.False(ratio.IsExact);
        Assert.Equal(3, arr.Count);
        _ = (str, price, flag, nil);

        // The members the prose beneath the snippet claims.
        obj["key"] = new JyroString("v");
        Assert.True(obj.TryGetValue("key", out _));
        Assert.True(obj.Remove("key"));
        obj.Clear();

        arr[0] = new JyroString("replaced");
        Assert.True(arr.TryGetItem(0, out _));
        arr.Insert(0, new JyroNumber(0));
        arr.RemoveAt(0);
        arr.Clear();
    }

    // -- Diagnostics ----------------------------------------------------------

    [Fact]
    public void FormatDiagnostics()
    {
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.x = 1 / 0")
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInput(new JyroObject())
            .Execute();

        foreach (var msg in result.Messages)
        {
            Console.Error.WriteLine(DiagnosticFormatter.FormatMessage(msg));
        }

        Assert.StartsWith("[JM5200]", DiagnosticFormatter.FormatMessage(result.Messages[0]));
    }

    [Fact]
    public void StructuredDiagnostics()
    {
        var msg = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute);

        var structured = DiagnosticFormatter.ToStructured(msg);

        Assert.Equal("JM5200", structured.Code);
        Assert.Equal(5200, structured.NumericCode);
        Assert.Equal("runtime", structured.Subsystem);
        Assert.Equal("Division by zero", structured.Message);
        Assert.False(structured.Location.HasValue);
    }

    // The README's localization example, verbatim.
    public sealed class FrenchTemplates : IMessageTemplateProvider
    {
        public bool TryGetTemplate(MessageCode code, out string template)
        {
            switch (code)
            {
                case MessageCode.DivisionByZero:
                    template = "Division par zéro";
                    return true;
                case MessageCode.InvalidType:
                    template = "Impossible d'assigner {0} à la variable '{1}' de type {2}";
                    return true;
                default:
                    template = null!;
                    return false;
            }
        }
    }

    [Fact]
    public void Localization()
    {
        var msg = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute);

        var formatted = DiagnosticFormatter.FormatLocalized(new FrenchTemplates(), msg);

        Assert.Contains("Division par zéro", formatted);

        Assert.True(MessageTemplates.TryGet(MessageCode.DivisionByZero, out var template));
        Assert.Equal("Division by zero", template);
    }

    [Fact]
    public void ConstructDiagnostics()
    {
        // An error. The args fill the code's message template.
        var error = DiagnosticMessage.Error(
            MessageCode.RuntimeError, MessageStage.Execute, new object[] { "LoadOrder()" });

        // An error with a source location: line 10, column 5, 12 code units long.
        var located = DiagnosticMessage.Error(
            MessageCode.UndefinedFunction,
            MessageStage.Link,
            new SourceLocation(10, 5, 12),
            new object[] { "LoadOrder" });

        // Warning and Info have the same overloads.
        var warning = DiagnosticMessage.Warning(
            MessageCode.UndefinedFunction, MessageStage.Link, new object[] { "LegacyTotal" });

        Assert.Equal(MessageCode.RuntimeError, error.Code);
        Assert.Contains("LoadOrder()", error.Message);
        Assert.Equal(MessageCode.UndefinedFunction, located.Code);
        Assert.Equal((10, 5, 12), (located.Location!.Value.Line, located.Location!.Value.Column, located.Location!.Value.Length));
        Assert.Contains("LoadOrder", located.Message);
        Assert.Equal(MessageSeverity.Warning, warning.Severity);
        Assert.Equal(MessageCode.UndefinedFunction, warning.Code);
        Assert.Contains("LegacyTotal", warning.Message);
    }

    // -- Resource limits ------------------------------------------------------

    [Fact]
    public void ConvenienceLimits()
    {
        var script = "Output.x = 1";
        var json = "{}";

        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource(script)
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson(json)
            .WithMaxExecutionTime(TimeSpan.FromSeconds(2))
            .WithMaxLoopIterations(500)
            .Execute();

        Assert.True(result.IsSuccess);
    }

    [Fact]
    public void DirectOptionsConstruction()
    {
        var script = "Output.x = 1";
        var json = "{}";

        var options = new JyroExecutionOptions
        {
            MaxExecutionTime = TimeSpan.FromSeconds(10),
            MaxStatements = 20_000,
            MaxLoopIterations = 2_000,
            MaxCallDepth = 128,
        };

        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource(script)
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson(json)
            .WithExecutionOptions(options)
            .Execute();

        Assert.True(result.IsSuccess);
        Assert.Equal(96, JyroExecutionOptions.MaxSafeCallDepth);
        _ = JyroExecutionOptions.Default;
        _ = JyroExecutionOptions.Unlimited;
    }

    [Fact]
    public void Cancellation()
    {
        var script = "Output.x = 1";
        var json = "{}";

        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource(script)
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson(json)
            .WithMaxExecutionTime(TimeSpan.FromSeconds(5))
            .WithCancellationToken(cts.Token)
            .Execute();

        Assert.True(result.IsSuccess);
    }

    // -- Pipeline statistics --------------------------------------------------

    [Fact]
    public void PipelineStatistics()
    {
        var script = "Output.x = 1";
        var json = "{}";

        // One collector, handed to both halves: the compiler records Parse,
        // Validate, Link and Compile; the runner records Execute and the
        // resource counters. Give it to only one and you get only that half.
        var stats = new JyroPipelineStats();
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource(script)
            .WithStats(stats)
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson(json)
            .WithStats(stats)
            .Execute();

        Console.WriteLine($"Parse: {stats.ParseTime.TotalMilliseconds}ms");
        Console.WriteLine($"Execute: {stats.ExecuteTime.TotalMilliseconds}ms");
        Console.WriteLine($"Statements: {stats.StatementCount}");
        Console.WriteLine($"Total: {stats.TotalTime.TotalMilliseconds}ms");

        Assert.True(result.IsSuccess);
        Assert.True(stats.StatementCount > 0);
    }

    // -- Function signatures --------------------------------------------------

    [Fact]
    public void SignatureFactories()
    {
        // One required parameter: (returnType, parameters)
        var unary = JyroFunctionSignature.Create(
            ParameterType.String,
            Parameter.Required("text", ParameterType.String));

        // Two required parameters
        var binary = JyroFunctionSignature.Create(
            ParameterType.Number,
            Parameter.Required("left", ParameterType.Number),
            Parameter.Required("right", ParameterType.Number));

        var custom = JyroFunctionSignature.Create(
            new[]
            {
                Parameter.Required("haystack", ParameterType.String),
                Parameter.Required("needle", ParameterType.String),
                Parameter.Optional("caseSensitive", ParameterType.Boolean),
            },
            ParameterType.Number);

        Assert.Equal(1, unary.MaxArgs);
        Assert.Equal(2, binary.MinArgs);
        Assert.Equal(2, custom.MinArgs);
        Assert.Equal(3, custom.MaxArgs);
    }

    // The README's worked example (JYRO-ADR-192): one Execute override, arguments read through JyroArguments.
    public sealed class ReadmeReverseStringFunction : JyroFunctionBase
    {
        public ReadmeReverseStringFunction()
            : base("ReverseString",
                   JyroFunctionSignature.Create(
                       ParameterType.String,
                       Parameter.Required("text", ParameterType.String)))
        {
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(
            IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
        {
            var text = args.GetString(0);
            var chars = text.ToCharArray();
            Array.Reverse(chars);
            return new JyroString(new string(chars));
        }
    }

    [Fact]
    public void RegisterAndCall()
    {
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.result = ReverseString(Input.text)")
            .AddFunction(new ReadmeReverseStringFunction())
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson("""{ "text": "hello" }""")
            .Execute();

        Assert.True(result.IsSuccess);
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal("olleh", output.GetProperty("result").ToStringValue());
    }

    [Fact]
    public void UnitTestAFunctionWithABareContext()
    {
        var ctx = new JyroExecutionContext();
        var value = new ReadmeReverseStringFunction()
            .Execute(new JyroValue[] { new JyroString("abc") }, ctx);
        // value.ToStringValue() == "cba"

        Assert.Equal("cba", value.ToStringValue());
    }

    // -- Report a failure your function chose ---------------------------------
    //
    // Both forms of throw JyroError.Host (JYRO-REQ-1891, JYRO-ADR-124, JYRO-DEF-541): a
    // reason and values, and a reason alone. JyroError.Host returns the exception, so
    // C# writes throw before it, in a method of any return type.

    public sealed class ReadmeChargeFunction : JyroFunctionBase
    {
        public ReadmeChargeFunction()
            : base("Charge",
                   JyroFunctionSignature.Create(
                       ParameterType.Number,
                       Parameter.Required("amount", ParameterType.Number)))
        {
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(
            IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
        {
            var amount = args.GetDecimal(0);
            if (amount <= 0) throw JyroError.Host("AmountNotPositive", amount);
            return new JyroNumber(amount);
        }
    }

    public sealed class ReadmeUnavailableFunction : JyroFunctionBase
    {
        public ReadmeUnavailableFunction()
            : base("Reserve",
                   JyroFunctionSignature.Create(
                       ParameterType.String,
                       Parameter.Required("item", ParameterType.String)))
        {
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(
            IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
        {
            throw JyroError.Host("ResourceUnavailable");
        }
    }

    [Fact]
    public void ReportAFailureWithAReasonAndValues()
    {
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.r = Charge(Input.amount)")
            .AddFunction(new ReadmeChargeFunction())
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson("""{ "amount": -5 }""")
            .Execute();

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.HostFunctionError);
        Assert.Equal(2, d.Args.Length);
        Assert.Equal("AmountNotPositive", Convert.ToString(d.Args[0], System.Globalization.CultureInfo.InvariantCulture));
        Assert.Equal(-5d, Convert.ToDouble(d.Args[1], System.Globalization.CultureInfo.InvariantCulture));
    }

    [Fact]
    public void ReportAFailureWithAReasonAlone()
    {
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.r = Reserve(Input.item)")
            .AddFunction(new ReadmeUnavailableFunction())
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson("""{ "item": "room" }""")
            .Execute();

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.HostFunctionError);
        var reason = Assert.Single(d.Args);
        Assert.Equal("ResourceUnavailable", Convert.ToString(reason, System.Globalization.CultureInfo.InvariantCulture));
    }

    // -- Check a call before the script runs ----------------------------------
    //
    // The README's CheckCall override, verbatim (JYRO-REQ-2119), in a host function
    // whose _tables holds the tables the host knows.

    public sealed class ReadmeQueryFunction : JyroFunctionBase
    {
        private readonly HashSet<string> _tables = new() { "customers" };

        public ReadmeQueryFunction()
            : base("Query",
                   JyroFunctionSignature.Create(
                       ParameterType.Any,
                       Parameter.Required("table", ParameterType.String)))
        {
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(
            IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
            => JyroNull.Instance;

        public override void CheckCall(JyroCallSite call)
        {
            if (call.TryGetLiteral(0, out var table)
                && table is JyroString name
                && !_tables.Contains(name.Value))
            {
                throw JyroError.Host("TableUnknown", name.Value);
            }
        }
    }

    [Fact]
    public void CheckACallBeforeTheScriptRuns()
    {
        var refused = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.r = Query(\"custmers\")")
            .AddFunction(new ReadmeQueryFunction())
            .Compile();

        Assert.False(refused.IsSuccess);
        var d = Assert.Single(refused.Messages, m => m.Code == MessageCode.HostLinkError);
        Assert.Equal(new[] { "TableUnknown", "custmers" },
            Array.ConvertAll(d.Args, a => Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture)));

        var accepted = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.r = Query(\"customers\")")
            .AddFunction(new ReadmeQueryFunction())
            .Compile();

        Assert.True(accepted.IsSuccess);
    }

    // -- Script libraries -----------------------------------------------------

    [Fact]
    public void ScriptLibraries()
    {
        var librarySource = "func Triple(n: number)\n    return n * 3\nend";

        var parsed = JyroLibrary.Parse(librarySource);
        if (!parsed.TryGetValue(out var library))
            return; // read parsed.Messages

        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource("Output.r = Triple(Input.n)")
            .AddLibrary(library)
            .CompileOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson("""{ "n": 2 }""")
            .Execute();

        Assert.True(result.IsSuccess);
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal(6d, output.GetProperty("r").ToDouble());
    }

    // -- Precompilation -------------------------------------------------------

    [Fact]
    public void InMemoryCaching()
    {
        var script = "Output.doubled = Input.n * 2";
        var json = """{ "n": 21 }""";

        // Compile one time.
        CompiledProgram program = new JyroCompiler()
            .WithStdlib()
            .WithSource(script)
            .CompileOrThrow();

        // Execute many times with different data, and with per-run limits.
        var result = new JyroRunner(program)
            .WithInput(JyroValue.FromJson(json))
            .WithMaxExecutionTime(TimeSpan.FromSeconds(2))
            .Execute();

        Assert.True(result.IsSuccess);
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal(42d, output.GetProperty("doubled").ToDouble());
    }

    [Fact]
    public void BinaryPrecompilation()
    {
        var script = "Output.x = ToUpper(Input.s)";
        var json = """{ "s": "a" }""";

        // Compile one time.
        var bytesResult = new JyroCompiler()
            .WithStdlib()
            .WithSource(script)
            .CompileToArtifact();

        if (!bytesResult.TryGetValue(out var jyrxBytes))
            return; // read bytesResult.Messages

        // Load, then execute many times.
        CompiledProgram program = new JyroArtifactLoader(jyrxBytes)
            .WithStdlib()
            .Load()
            .GetValueOrThrow();

        var result = new JyroRunner(program)
            .WithInputJson(json)
            .Execute();

        Assert.True(result.IsSuccess);

        var infoResult = JyroArtifactInfo.Read(jyrxBytes);
        if (infoResult.TryGetValue(out var info))
        {
            // info.FormatVersion      - the .jyrx container version
            // info.SourceHash         - SHA-256 of the source the artifact was built from
            // info.RequiredFunctions  - the functions the host must supply at load time
        }

        Assert.NotNull(info);
        Assert.Equal(32, info!.SourceHash.Length);
        Assert.Contains("ToUpper", info.RequiredFunctions);
    }

    // -- The one-call members of JyroScript -----------------------------------

    [Fact]
    public void OneCallMembers()
    {
        var result = JyroScript.ExecuteScriptWithJson("Output.result = Input.x * 2", """{ "x": 10 }""");

        Assert.True(result.IsSuccess);
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal(20d, output.GetProperty("result").ToDouble());
    }
}

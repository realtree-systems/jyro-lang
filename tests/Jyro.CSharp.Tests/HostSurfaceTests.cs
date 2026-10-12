using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using RealTree.Jyro;
using Xunit;

// This file is the C# host surface, compiled.
//
// Every public type in this package ships to C# hosts and the README documents
// that surface in C#, but no .csproj existed anywhere in the repository, so
// nothing had ever compiled a line of it. This project exists to fail when the
// C# view breaks. It is not a behavioral suite - the F# tests own that - it is
// a compilation check with assertions attached, written the way a host writes.
//
// It is also the enforcement point for a rule: a C# host should not have to
// know this package is written in F#. There is deliberately no `using
// Microsoft.FSharp.Core` or `Microsoft.FSharp.Collections` below, and there
// must never be one. If a change to the library makes this file need an
// FSharpOption, an FSharpList or a ListModule call to compile, that change has
// put an F# type back on the host surface and the fix belongs in the library,
// not here.

// Deliberately NOT under a `RealTree.*` namespace: C# resolves `RealTree` to the
// nearest enclosing namespace root, so a host whose own namespace starts with
// `RealTree` can shadow the package's `RealTree.Jyro` and stop its types
// resolving. Worth knowing, and worth not tripping over inside our own check.
namespace JyroHostSurface;

/// <summary>
/// The pipeline as a C# host drives it.
/// </summary>
public class PipelineTests
{
    [Fact]
    public void ExecuteReturnsAValueTheHostCanRead()
    {
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.total = 2 + 3")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        Assert.True(result.IsSuccess);
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal("{\"total\":5}", output.ToJson());
    }

    [Fact]
    public void AFailedRunCarriesTypedDiagnostics()
    {
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.x = 1 / 0")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        Assert.False(result.IsSuccess);

        IReadOnlyList<DiagnosticMessage> messages = result.Messages;
        DiagnosticMessage first = messages[0];
        Assert.Equal(MessageCode.DivisionByZero, first.Code);
        Assert.Equal(MessageSeverity.Error, first.Severity);
        Assert.False(string.IsNullOrWhiteSpace(first.Message));
    }

    [Fact]
    public void AFailedRunStillCarriesWhateverOutputWasBuilt()
    {
        // Specification 130.2: a script that populates Output and then fails
        // leaves partial results, and those matter most when the run did not
        // succeed. TryGetValue is how a C# host reaches them.
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.first = 1\nOutput.second = 1 / 0")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        Assert.False(result.IsSuccess);
        Assert.True(result.TryGetValue(out var partial));
        Assert.Equal(1d, partial.GetProperty("first").ToDouble());
    }

    [Fact]
    public void ExecuteOrThrowCarriesTheDiagnosticsNotAJoinedString()
    {
        var runner = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.x = 1 / 0")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject());

        // Documented as InvalidOperationException in the shipped README, which
        // is no longer true: a host catching that type would miss the failure.
        var ex = Assert.Throws<JyroException>(() => runner.ExecuteOrThrow());
        Assert.Contains(ex.Messages, m => m.Code == MessageCode.DivisionByZero);
    }

    [Fact]
    public void CompileOnceAndExecuteManyTimes()
    {
        CompiledProgram program = new JyroCompiler()
            .WithSource("Output.doubled = Input.n * 2")
            .WithStdlib()
            .CompileOrThrow();

        foreach (var n in new[] { 1, 2, 3 })
        {
            var result = new JyroRunner(program)
                .WithInput(JyroValue.FromJson($"{{\"n\":{n}}}"))
                .Execute();

            Assert.True(result.IsSuccess);
            Assert.True(result.TryGetValue(out var output));
            Assert.Equal($"{{\"doubled\":{n * 2}}}", output.ToJson());
        }
    }

    [Fact]
    public void CompileToArtifactAndBack()
    {
        var bytesResult = new JyroCompiler()
            .WithSource("Output.greeting = ToUpper(Input.name)")
            .WithStdlib()
            .CompileToArtifact();

        Assert.True(bytesResult.IsSuccess);
        Assert.True(bytesResult.TryGetValue(out var jyrx));

        var result = new JyroRunner(
                new JyroArtifactLoader(jyrx)
                    .WithStdlib()
                    .Load()
                    .GetValueOrThrow())
            .WithInput(JyroValue.FromJson("{\"name\":\"jyro\"}"))
            .Execute();

        Assert.True(result.IsSuccess);
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal("{\"greeting\":\"JYRO\"}", output.ToJson());
    }

    [Fact]
    public void AnArtifactCanBeInspectedWithoutLoadingIt()
    {
        // The public replacement for BinaryFormat.readHeader, which was
        // reachable only because the whole binary-format module was public.
        var bytesResult = new JyroCompiler()
            .WithSource("Output.x = ToUpper('a')")
            .WithStdlib()
            .CompileToArtifact();
        Assert.True(bytesResult.TryGetValue(out var jyrx));

        var infoResult = JyroArtifactInfo.Read(jyrx);
        Assert.True(infoResult.IsSuccess);
        Assert.True(infoResult.TryGetValue(out var info));

        Assert.Equal(32, info.SourceHash.Length);
        Assert.Contains("ToUpper", info.RequiredFunctions);
        Assert.True(info.FormatVersion > 0);
    }

    [Fact]
    public void AnUnreadableArtifactFailsRatherThanThrowing()
    {
        var result = JyroArtifactInfo.Read(new byte[] { 1, 2, 3, 4, 5, 6, 7, 8 });

        Assert.False(result.IsSuccess);
        Assert.Contains(result.Messages, m => m.Code == MessageCode.ArtifactInvalidMagic);
    }

    [Fact]
    public void ALibraryIsParsedOnceAndSharedAcrossEngines()
    {
        // Replaces AddLibraryAst(Stmt seq), which offered the same reuse only
        // by making the compiler's AST public API.
        var libraryResult = JyroLibrary.Parse("func Triple(n: number)\n    return n * 3\nend");
        Assert.True(libraryResult.IsSuccess, string.Join("; ", libraryResult.Messages.Select(m => m.Message)));
        Assert.True(libraryResult.TryGetValue(out var library));
        Assert.Equal(1, library.DeclarationCount);

        foreach (var n in new[] { 1, 2 })
        {
            var result = new JyroRunner(
                    new JyroCompiler()
                        .WithSource("Output.r = Triple(Input.n)")
                        .WithStdlib()
                        .AddLibrary(library)
                        .CompileOrThrow())
                .WithInput(JyroValue.FromJson($"{{\"n\":{n}}}"))
                .Execute();

            Assert.True(result.IsSuccess, string.Join("; ", result.Messages.Select(m => m.Message)));
            Assert.True(result.TryGetValue(out var output));
            Assert.Equal(n * 3d, output.GetProperty("r").ToDouble());
        }
    }

    [Fact]
    public void ALibraryThatBreaksTheLibraryRulesFailsAtParse()
    {
        // A library may hold only func and union declarations. Parsing up front
        // is the point of the type, so the rule is enforced here rather than
        // deferred to Compile().
        var result = JyroLibrary.Parse("var x = 1");

        Assert.False(result.IsSuccess);
        Assert.NotEmpty(result.Messages);
    }
}

/// <summary>
/// A custom function, written the way the README tells a host to write one.
/// </summary>
public sealed class ReverseStringFunction : JyroFunctionBase
{
    public ReverseStringFunction()
        : base("ReverseString",
               JyroFunctionSignature.Create(
                   ParameterType.String,
                   Parameter.Required("text", ParameterType.String)))
    {
    }

    // Abstract with no default, so a host MUST override it, as it MUST override Execute (JYRO-REQ-1889).
    public override FunctionEffect Effect => FunctionEffect.Pure;

    public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext context)
    {
        var text = args.GetString(0);
        var chars = text.ToCharArray();
        Array.Reverse(chars);
        return new JyroString(new string(chars));
    }
}

public class CustomFunctionTests
{
    [Fact]
    public void AHostFunctionIsCallableFromScript()
    {
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.r = ReverseString('abc')")
                    .WithStdlib()
                    .AddFunction(new ReverseStringFunction())
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        Assert.True(result.IsSuccess, string.Join("; ", result.Messages.Select(m => m.Message)));
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal("{\"r\":\"cba\"}", output.ToJson());
    }

    [Fact]
    public void ASignatureIsReadableAsAPlainList()
    {
        IReadOnlyList<Parameter> parameters = new ReverseStringFunction().Signature.Parameters;

        Assert.Single(parameters);
        Assert.Equal("text", parameters[0].Name);
        Assert.False(parameters[0].IsOptional);
    }

    [Fact]
    public void AFunctionCanBeInvokedDirectlyWithABareContext()
    {
        // `new JyroExecutionContext()` is what a host writes to unit-test its
        // own function without standing up the pipeline. It did not exist for
        // C# while the constructor took two optional parameters.
        var ctx = new JyroExecutionContext();
        var result = new ReverseStringFunction()
            .Execute(new JyroValue[] { new JyroString("abc") }, ctx);

        Assert.Equal("cba", result.ToStringValue());
        Assert.Null(ctx.HostState);
    }

    [Fact]
    public void TheStandardLibraryIsEnumerable()
    {
        IReadOnlyList<IJyroFunction> all = StdlibRegistry.GetAll();
        Assert.NotEmpty(all);

        Assert.True(StdlibRegistry.TryGetFunction("ToUpper", out var toUpper));
        Assert.Equal("ToUpper", toUpper.Name);
        Assert.False(StdlibRegistry.TryGetFunction("NoSuchFunction", out _));
    }

    [Fact]
    public void TheExactNumericAccessorsAreReachableFromCSharp()
    {
        // The guarantee a host most needs and is most likely to lose: see
        // Host Integration 160.3.
        var value = JyroValue.FromJson("{\"id\":9007199254740993}");
        var number = (JyroNumber)value.GetProperty("id");

        Assert.True(number.IsExact);
        Assert.Equal(9007199254740993m, number.DecimalValue);

        // The double view has lost it. Note this cannot be written as
        // Assert.NotEqual(9007199254740993d, number.Value): the C# literal is
        // itself rounded to ...992 at compile time, so both sides would be the
        // same value and the assertion would fail. The comparison has to be
        // made in the type that can still hold the difference - which is the
        // whole reason GetDecimal exists.
        Assert.NotEqual(number.DecimalValue, (decimal)number.Value);
    }
}

/// <summary>
/// A host function that reads its one argument at index -1 through the helper
/// it is named after: a host's mistake, not a script's (JYRO-ADR-118).
/// </summary>
public sealed class NegativeIndexReader : JyroFunctionBase
{
    private readonly string _read;

    public NegativeIndexReader(string read)
        : base("NegIndex",
               JyroFunctionSignature.Create(
                   ParameterType.Any,
                   Parameter.Required("value", ParameterType.Any)))
    {
        _read = read;
    }

    public override FunctionEffect Effect => FunctionEffect.Pure;

    public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext context)
    {
        const int i = -1;
        switch (_read)
        {
            case "Get<JyroNumber>": return args.Get<JyroNumber>(i);
            case "Get<JyroValue>": return args.Get<JyroValue>(i);
            case "GetOptional<JyroNumber>": return args.GetOptional(i, new JyroNumber(7));
            case "GetOptional<JyroValue>": return args.GetOptional<JyroValue>(i, new JyroString("default"));
            case "GetString": return new JyroString(args.GetString(i));
            case "GetNumber": return new JyroNumber(args.GetNumber(i));
            case "GetDecimal": return new JyroNumber(args.GetDecimal(i));
            case "GetBoolean": return JyroBoolean.FromBoolean(args.GetBoolean(i));
            case "GetArray": return args.GetArray(i);
            case "GetObject": return args.GetObject(i);
            case "GetInt32": return new JyroNumber(args.GetInt32(i));
            case "GetNonNegativeInt32": return new JyroNumber(args.GetNonNegativeInt32(i));
            case "GetInt64": return new JyroNumber(args.GetInt64(i));
            default: throw new ArgumentException("unknown read " + _read);
        }
    }
}

public class NegativeArgumentIndexTests
{
    public static IEnumerable<object[]> Reads() => new[]
    {
        "Get<JyroNumber>", "Get<JyroValue>",
        "GetOptional<JyroNumber>", "GetOptional<JyroValue>",
        "GetString", "GetNumber", "GetDecimal", "GetBoolean",
        "GetArray", "GetObject",
        "GetInt32", "GetNonNegativeInt32", "GetInt64",
    }.Select(r => new object[] { r });

    private static JyroResult<JyroValue> RunReading(string read) =>
        new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.v = NegIndex(1)")
                    .WithStdlib()
                    .AddFunction(new NegativeIndexReader(read))
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

    // JYRO-ADR-118 (JYRO-DEF-519): only i >= args.Count is a missing argument
    // (160.3.11). A negative index is a host's mistake and every argument read
    // fails on it the same way, as FailureSurface.approved.txt approves
    // (RuntimeError): none returns null or a default, and none reports it in
    // the script's vocabulary as ArgumentTypeMismatch or ArgumentNotProvided.
    [Theory(DisplayName = "JYRO-REQ-1936 a negative argument index fails with RuntimeError and is never read as a missing argument")]
    [Trait("requirement", "JYRO-REQ-1936")]
    [MemberData(nameof(Reads))]
    public void ANegativeIndexFailsTheSameWayForEveryRead(string read)
    {
        var result = RunReading(read);

        var codes = result.Messages.Select(m => m.Code).ToList();
        Assert.False(result.IsSuccess,
            $"{read}(args, -1) must fail, not read the index as a missing argument");
        Assert.Contains(MessageCode.RuntimeError, codes);
        Assert.DoesNotContain(MessageCode.ArgumentTypeMismatch, codes);
        Assert.DoesNotContain(MessageCode.ArgumentNotProvided, codes);
    }

    [Fact(DisplayName = "JYRO-REQ-1890 every argument read reports a negative index with the same codes")]
    [Trait("requirement", "JYRO-REQ-1890")]
    public void EveryReadReportsANegativeIndexAlike()
    {
        var reports = Reads()
            .Select(r => (string)r[0])
            .ToDictionary(r => r, r => string.Join(",", RunReading(r).Messages.Select(m => m.Code)));

        var distinct = reports.Values.Distinct().ToList();
        Assert.True(distinct.Count == 1,
            "the reads answer one mistake more than one way: "
            + string.Join("; ", reports.Select(kv => $"{kv.Key} -> [{kv.Value}]")));
    }
}

public class ValueSurfaceTests
{
    [Fact]
    public void BuildingAValueGraphByHand()
    {
        var root = new JyroObject();
        root.SetProperty("name", new JyroString("widget"));
        root.SetProperty("price", new JyroNumber(19.99m));
        // Unsuffixed, which used to be CS0121 - ambiguous between the double
        // and decimal constructors.
        root.SetProperty("quantity", new JyroNumber(3));

        var tags = new JyroArray();
        tags.Add(new JyroString("new"));
        root.SetProperty("tags", tags);

        Assert.Equal(4, root.Count);
        Assert.Equal(1, tags.Count);
        Assert.True(root.TryGetValue("name", out var name));
        Assert.Equal("widget", name.ToStringValue());
    }

    [Fact]
    public void AnIntegerIsExactAndSerializesAsOne()
    {
        // int and int64 route to the exact decimal representation, which is
        // what an integer is - and what keeps an id past 2^53 intact.
        Assert.True(new JyroNumber(3).IsExact);
        Assert.True(new JyroNumber(9007199254740993L).IsExact);
        Assert.Equal(9007199254740993m, new JyroNumber(9007199254740993L).DecimalValue);
    }

    [Fact]
    public void FreezeIsReachableOnTheBaseType()
    {
        // Every handle the API gives a host is typed JyroValue, so Freeze has
        // to be callable there - it used to be declared only on the two
        // container subclasses, making the README's thread-safety advice a
        // compile error without an undocumented downcast.
        JyroValue value = JyroValue.FromJson("{\"a\":[1,2]}");
        value.Freeze();

        Assert.True(value.IsFrozen);
        Assert.Throws<JyroRuntimeException>(() =>
            value.AsObject().SetProperty("b", new JyroNumber(1)));
    }

    [Fact]
    public void EqualityAndHashingWorkForDictionaryKeys()
    {
        var set = new HashSet<JyroValue>
        {
            new JyroString("a"),
            new JyroString("a"),
            new JyroNumber(1),
        };

        Assert.Equal(2, set.Count);
    }

    [Fact]
    public void ConversionHelpersRoundTripHostData()
    {
        var value = JyroValue.FromObject(new Dictionary<string, object>
        {
            ["n"] = 42,
            ["s"] = "text",
            ["b"] = true,
        });

        var obj = value.AsObject();
        Assert.Equal(42d, obj.GetProperty("n").ToDouble());
        Assert.Equal("text", obj.GetProperty("s").ToStringValue());
        Assert.True(obj.GetProperty("b").IsTruthy());
    }

    [Fact]
    public void ValueTypeIsAnEnumAHostCanSwitchOn()
    {
        // The README now documents this and shows a switch, so the switch has to
        // compile - which is the whole reason JyroValueType is an enum rather
        // than a third discriminated union. An F# DU's cases cannot appear in a
        // C# case label.
        static string Describe(JyroValue v) => v.ValueType switch
        {
            JyroValueType.Null => "nothing",
            JyroValueType.Number => "a number",
            JyroValueType.String => "a string",
            JyroValueType.Boolean => "a boolean",
            JyroValueType.Array => "an array",
            JyroValueType.Object => "an object",
            JyroValueType.Function => "a lambda",
            _ => "unknown",
        };

        Assert.Equal("nothing", Describe(JyroNull.Instance));
        Assert.Equal("a number", Describe(new JyroNumber(1)));
        Assert.Equal("a string", Describe(new JyroString("s")));
        Assert.Equal("a boolean", Describe(JyroBoolean.True));
        Assert.Equal("an array", Describe(new JyroArray()));
        Assert.Equal("an object", Describe(new JyroObject()));

        // And it round-trips through Enum.Parse, which a DU could not do.
        Assert.Equal(JyroValueType.Object, Enum.Parse<JyroValueType>("Object"));
    }

    [Fact(DisplayName = "JYRO-REQ-1893 a signature is built by Create and reads its parameters, types and argument counts")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void ASignatureIsBuiltByTheFactory()
    {
        var signature = JyroFunctionSignature.Create(
            ParameterType.Number,
            Parameter.Required("haystack", ParameterType.String),
            Parameter.Required("needle", ParameterType.String),
            Parameter.Optional("start", ParameterType.Number));

        Assert.Equal(3, signature.Parameters.Count);
        Assert.Equal(2, signature.MinArgs);
        Assert.Equal(3, signature.MaxArgs);

        // The declared types are readable metadata, which is what they are for.
        Assert.Equal(ParameterType.Number, signature.ReturnType);
        Assert.Equal(ParameterType.String, signature.Parameters[0].Type);
        Assert.True(signature.Parameters[2].IsOptional);
    }

    [Fact(DisplayName = "JYRO-REQ-2113 JyroValue.FromJson refuses a trailing comma with InvalidJson")]
    [Trait("requirement", "JYRO-REQ-2113")]
    public void FromJsonRefusesATrailingCommaWithInvalidJson()
    {
        // FromJson reads JSON (RFC 8259): a trailing comma is not JSON, so it
        // fails with a JyroRuntimeException carrying InvalidJson, the type a
        // host is told to catch, and not the reader's own JsonException. The
        // overload that took JsonDocumentOptions is removed (JYRO-ADR-106).
        var ex = Assert.Throws<JyroRuntimeException>(() => JyroValue.FromJson("{\"a\":1,}"));
        Assert.Equal(MessageCode.InvalidJson, ex.Code);
    }
}

public class DiagnosticSurfaceTests
{
    [Fact]
    public void StructuredDiagnosticsCarryLocationAndSubsystem()
    {
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.x = 1 / 0")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        var structured = DiagnosticFormatter.ToStructured(result.Messages[0]);

        Assert.Equal("JM5200", structured.Code);
        Assert.Equal(5200, structured.NumericCode);
        Assert.Equal("runtime", structured.Subsystem);

        // Nullable<SourceLocation>, so a C# host uses the language's own null
        // handling rather than FSharpOption.
        Assert.True(structured.Location.HasValue);
        Assert.True(structured.Location.Value.Line > 0);
    }

    [Fact]
    public void ADiagnosticWithNoPositionSaysSoRatherThanReportingLineZero()
    {
        var msg = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute);
        Assert.False(msg.Location.HasValue);

        var located = DiagnosticMessage.Error(
            MessageCode.DivisionByZero,
            MessageStage.Execute,
            new SourceLocation(4, 9, 2));
        Assert.True(located.Location.HasValue);
        Assert.Equal(4, located.Location.Value.Line);
        Assert.Equal(2, located.Location.Value.Length);
    }

    // JYRO-ADR-117 (b) x (c): a host holding a SourceLocation? and no template
    // values passes the location as it is, with no empty args array and no
    // unwrapping (JYRO-DEF-516). These lines are the compile check: each calls
    // the (MessageCode, Nullable<SourceLocation>) overload of one severity.
    [Fact(DisplayName = "JYRO-REQ-2103 DiagnosticMessage.Error takes a code and a nullable location without args")]
    [Trait("requirement", "JYRO-REQ-2103")]
    public void ErrorTakesACodeAndANullableLocation()
    {
        SourceLocation? at = new SourceLocation(4, 9, 2);
        SourceLocation? none = null;

        var located = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, at);
        Assert.Equal(MessageSeverity.Error, located.Severity);
        Assert.Equal(MessageCode.DivisionByZero, located.Code);
        Assert.True(located.Location.HasValue);
        Assert.Equal(4, located.Location.Value.Line);

        var unlocated = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, none);
        Assert.Equal(MessageSeverity.Error, unlocated.Severity);
        Assert.False(unlocated.Location.HasValue);
    }

    [Fact(DisplayName = "JYRO-REQ-2103 DiagnosticMessage.Warning takes a code and a nullable location without args")]
    [Trait("requirement", "JYRO-REQ-2103")]
    public void WarningTakesACodeAndANullableLocation()
    {
        SourceLocation? at = new SourceLocation(4, 9, 2);
        SourceLocation? none = null;

        var located = DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute, at);
        Assert.Equal(MessageSeverity.Warning, located.Severity);
        Assert.Equal(MessageCode.DivisionByZero, located.Code);
        Assert.True(located.Location.HasValue);
        Assert.Equal(4, located.Location.Value.Line);

        var unlocated = DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute, none);
        Assert.Equal(MessageSeverity.Warning, unlocated.Severity);
        Assert.False(unlocated.Location.HasValue);
    }

    [Fact(DisplayName = "JYRO-REQ-2103 DiagnosticMessage.Info takes a code and a nullable location without args")]
    [Trait("requirement", "JYRO-REQ-2103")]
    public void InfoTakesACodeAndANullableLocation()
    {
        SourceLocation? at = new SourceLocation(4, 9, 2);
        SourceLocation? none = null;

        var located = DiagnosticMessage.Info(MessageCode.DivisionByZero, MessageStage.Execute, at);
        Assert.Equal(MessageSeverity.Info, located.Severity);
        Assert.Equal(MessageCode.DivisionByZero, located.Code);
        Assert.True(located.Location.HasValue);
        Assert.Equal(4, located.Location.Value.Line);

        var unlocated = DiagnosticMessage.Info(MessageCode.DivisionByZero, MessageStage.Execute, none);
        Assert.Equal(MessageSeverity.Info, unlocated.Severity);
        Assert.False(unlocated.Location.HasValue);
    }

    [Fact]
    public void AHostCanSupplyItsOwnTemplates()
    {
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("Output.x = 1 / 0")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        var text = DiagnosticFormatter.FormatLocalized(new FrenchTemplates(), result.Messages[0]);

        Assert.Contains("Division par zero", text);
    }

    [Fact]
    public void TheDefaultTemplatesAreQueryable()
    {
        Assert.True(MessageTemplates.TryGet(MessageCode.DivisionByZero, out var template));
        Assert.Equal("Division by zero", template);
    }

    // A host implements this interface, which is why it must not have an F#
    // type in its signature. TryGetTemplate is the shape C# already uses for
    // "maybe a value" and needs no F# reference to implement.
    private sealed class FrenchTemplates : IMessageTemplateProvider
    {
        public bool TryGetTemplate(MessageCode code, out string template)
        {
            if (code == MessageCode.DivisionByZero)
            {
                template = "Division par zero";
                return true;
            }

            template = null!;
            return false;
        }
    }
}

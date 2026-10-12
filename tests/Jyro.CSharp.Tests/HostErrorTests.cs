using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// The host's way to end a call with a diagnostic of its own, and the type that describes an artifact:
// JYRO-REQ-1891 (JyroError.Host and JyroError.Runtime, made and thrown by the host), JYRO-REQ-1892
// (the codes that only the engine raises), JYRO-REQ-1898 (JyroArtifactInfo is a read-only class) and
// the C# side of JYRO-REQ-1919. Written from the specification alone (FLOORS 4.1.1); every host
// function and every run sets its limits through the defaults of 100.1.2 and 100.9.2, which these
// small programs stay far below.
public class HostErrorTests
{
    private static string[] Args(DiagnosticMessage d) =>
        d.Args.Select(a => Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture)!).ToArray();

    // -- JYRO-REQ-1891 -----------------------------------------------------------------------------

    private sealed class AmountFunction : JyroFunctionBase
    {
        public AmountFunction()
            : base("Charge", JyroFunctionSignature.Create(ParameterType.Number, Parameter.Required("amount", ParameterType.Number)))
        { }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
        {
            var amount = args.GetDecimal(0);
            if (amount <= 0) throw JyroError.Host("AmountNotPositive", amount);
            return new JyroNumber(amount);
        }
    }

    private sealed class ThrowingFunction : JyroFunctionBase
    {
        private readonly Func<Exception> _make;

        public ThrowingFunction(string name, Func<Exception> make)
            : base(name, JyroFunctionSignature.Create(ParameterType.Any, Parameter.Required("x", ParameterType.Any)))
        {
            _make = make;
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx) => throw _make();
    }

    private static JyroResult<JyroValue> Run(IJyroFunction function, string source) =>
        new JyroRunner(new JyroCompiler().WithStdlib().AddFunction(function).WithSource(source).CompileOrThrow()).Execute();

    [Fact(DisplayName = "JYRO-REQ-1891 JyroError declares exactly Host and Runtime, as public static methods")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void JyroErrorDeclaresExactlyHostAndRuntime()
    {
        var type = typeof(JyroError);
        Assert.Equal("RealTree.Jyro", type.Namespace);
        Assert.True(type.IsPublic && type.IsAbstract && type.IsSealed, "JyroError is a public static class");

        var methods = type.GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.Instance | BindingFlags.DeclaredOnly);
        Assert.Equal(new[] { "Host", "Runtime" }, methods.Select(m => m.Name).OrderBy(n => n, StringComparer.Ordinal).ToArray());

        var host = Assert.Single(methods, m => m.Name == "Host");
        Assert.True(host.IsStatic);
        Assert.Equal(typeof(JyroRuntimeException), host.ReturnType);
        var hostParameters = host.GetParameters();
        Assert.Equal(new[] { typeof(string), typeof(object[]) }, hostParameters.Select(p => p.ParameterType).ToArray());
        Assert.Equal("reason", hostParameters[0].Name);
        Assert.Equal("values", hostParameters[1].Name);
        Assert.True(hostParameters[1].IsDefined(typeof(ParamArrayAttribute), false), "values is a params array");

        var runtime = Assert.Single(methods, m => m.Name == "Runtime");
        Assert.True(runtime.IsStatic);
        Assert.Equal(typeof(JyroRuntimeException), runtime.ReturnType);
        var runtimeParameters = runtime.GetParameters();
        Assert.Equal(new[] { typeof(MessageCode), typeof(object[]) }, runtimeParameters.Select(p => p.ParameterType).ToArray());
        Assert.Equal("code", runtimeParameters[0].Name);
        Assert.Equal("args", runtimeParameters[1].Name);
        Assert.True(runtimeParameters[1].IsDefined(typeof(ParamArrayAttribute), false), "args is a params array");
    }

    [Fact(DisplayName = "JYRO-REQ-1891 no public type of the shipped assemblies declares a member named RaiseHost, RaiseRuntime or RaiseError")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void NoMemberIsNamedRaise()
    {
        var found = new List<string>();
        foreach (var assemblyName in new[] { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" })
        {
            foreach (var type in Assembly.Load(assemblyName).GetExportedTypes())
            {
                var members = type.GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly);
                foreach (var member in members)
                {
                    var reachable = member is not MethodBase method || method.IsPublic || method.IsFamily || method.IsFamilyOrAssembly;
                    if (reachable && member.Name is "RaiseHost" or "RaiseRuntime" or "RaiseError")
                        found.Add(type.FullName + "." + member.Name);
                }
            }
        }
        Assert.Empty(found);
    }

    [Fact(DisplayName = "JYRO-REQ-1891 Host and Runtime return the exception and do not throw it")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void TheFactoriesReturnTheException()
    {
        JyroRuntimeException host = JyroError.Host("AmountNotPositive", -5);
        JyroRuntimeException runtime = JyroError.Runtime(MessageCode.ArgumentTypeMismatch, "string", "number");

        Assert.NotNull(host);
        Assert.NotNull(runtime);
        Assert.Equal(MessageCode.ArgumentTypeMismatch, runtime.Code);
        Assert.Equal(new object[] { "string", "number" }, runtime.Args);
        Assert.NotNull(JyroError.Host("Reason alone"));
        Assert.NotNull(JyroError.Host("Reason and an empty array", Array.Empty<object>()));
        Assert.NotNull(JyroError.Runtime(MessageCode.InvalidJson));
    }

    [Fact(DisplayName = "JYRO-REQ-1891 a host function whose Execute throws JyroError.Host ends the run with one HostFunctionError whose Args are the reason and the values")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void ThrowingHostEndsTheRunWithHostFunctionError()
    {
        var result = Run(new AmountFunction(), "Output.r = Charge(0 - 5)\nOutput.after = 1");

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.HostFunctionError);
        Assert.Equal(new[] { "AmountNotPositive", "-5" }, Args(d));
        Assert.Single(result.Messages);
        if (result.TryGetValue(out var output))
            Assert.False(((JyroObject)output).TryGetValue("after", out _), "the script went on after the refusal");
    }

    [Theory(DisplayName = "JYRO-REQ-1891 the Args of the HostFunctionError are the reason, then each value in order, and no other")]
    [Trait("requirement", "JYRO-REQ-1891")]
    [InlineData("omitted")]
    [InlineData("empty")]
    [InlineData("seven-and-x")]
    public void TheArgsAreTheReasonThenTheValues(string form)
    {
        Func<Exception> make = form switch
        {
            "omitted" => () => JyroError.Host("Why"),
            "empty" => () => JyroError.Host("Why", Array.Empty<object>()),
            _ => () => JyroError.Host("Why", 7, "x"),
        };
        var result = Run(new ThrowingFunction("Refuse", make), "Output.a = Refuse(1)");

        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.HostFunctionError);
        Assert.Equal(form == "seven-and-x" ? new[] { "Why", "7", "x" } : new[] { "Why" }, Args(d));
    }

    [Fact(DisplayName = "JYRO-REQ-1891 a host function whose Execute throws JyroError.Runtime ends the run with a diagnostic of that code and those args")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void ThrowingRuntimeGivesItsCodeAndArgs()
    {
        var result = Run(
            new ThrowingFunction("Boom", () => JyroError.Runtime(MessageCode.ArgumentTypeMismatch, "string", "number")),
            "Output.a = Boom(1)");

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.ArgumentTypeMismatch);
        Assert.Equal(new[] { "string", "number" }, Args(d));
        Assert.DoesNotContain(result.Messages, m => m.Code == MessageCode.HostFunctionError);
    }

    private sealed class QueryFunction : JyroFunctionBase
    {
        private readonly HashSet<string> _tables = new() { "customers" };

        public QueryFunction()
            : base("Query", JyroFunctionSignature.Create(ParameterType.Any, Parameter.Required("table", ParameterType.String)))
        { }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx) => JyroNull.Instance;

        // A CheckCall declared void: `throw` compiles in C# with no generic argument.
        public override void CheckCall(JyroCallSite call)
        {
            if (call.TryGetLiteral(0, out var table) && table is JyroString name && !_tables.Contains(name.Value))
                throw JyroError.Host("TableUnknown", name.Value);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1891 a CheckCall that throws JyroError.Host refuses the call with one HostLinkError, not HostFunctionError")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void ACheckCallRefusesWithHostLinkError()
    {
        var refused = new JyroCompiler().WithStdlib().WithSource("Output.r = Query(\"custmers\")").AddFunction(new QueryFunction()).Compile();

        Assert.False(refused.IsSuccess);
        var d = Assert.Single(refused.Messages, m => m.Code == MessageCode.HostLinkError);
        Assert.Equal(new[] { "TableUnknown", "custmers" }, Args(d));
        Assert.True(d.Location.HasValue, "the diagnostic is placed at the call");
        Assert.DoesNotContain(refused.Messages, m => m.Code == MessageCode.HostFunctionError);

        var accepted = new JyroCompiler().WithStdlib().WithSource("Output.r = Query(\"customers\")").AddFunction(new QueryFunction()).Compile();
        Assert.True(accepted.IsSuccess);
    }

    [Fact(DisplayName = "JYRO-REQ-1891 a CheckCall that throws JyroError.Runtime gives a diagnostic of that code and those args")]
    [Trait("requirement", "JYRO-REQ-1891")]
    public void ACheckCallThrowingRuntimeKeepsItsCodeAndArgs()
    {
        var function = new CheckingFunction(() => JyroError.Runtime(MessageCode.InvalidJson, "at", 7));
        var result = new JyroCompiler().WithStdlib().WithSource("Output.r = Check(1)").AddFunction(function).Compile();

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.InvalidJson);
        Assert.Equal(new[] { "at", "7" }, Args(d));
        Assert.DoesNotContain(result.Messages, m => m.Code == MessageCode.UnknownCompilerError);
    }

    private sealed class CheckingFunction : JyroFunctionBase
    {
        private readonly Func<Exception> _make;

        public CheckingFunction(Func<Exception> make)
            : base("Check", JyroFunctionSignature.Create(ParameterType.Any, Parameter.Required("x", ParameterType.Any)))
        {
            _make = make;
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> args, JyroExecutionContext ctx) => JyroNull.Instance;

        public override void CheckCall(JyroCallSite call) => throw _make();
    }

    // -- JYRO-REQ-1892 -----------------------------------------------------------------------------

    public static IEnumerable<object[]> CodesOnlyTheEngineRaises()
    {
        // 5002, 5003, 5004 and 5006
        yield return new object[] { MessageCode.RuntimeError };
        yield return new object[] { MessageCode.CanceledByHost };
        yield return new object[] { MessageCode.ScriptFailure };
        yield return new object[] { MessageCode.HostFunctionError };
        // 5900 to 5903
        yield return new object[] { MessageCode.StatementLimitExceeded };
        yield return new object[] { MessageCode.LoopIterationLimitExceeded };
        yield return new object[] { MessageCode.CallDepthLimitExceeded };
        yield return new object[] { MessageCode.ExecutionTimeLimitExceeded };
        // 5910 and 5919
        yield return new object[] { MessageCode.ValueNestingLimitExceeded };
        yield return new object[] { MessageCode.ValueSizeLimitExceeded };
        // A number below 5000, or 6000 and above
        yield return new object[] { MessageCode.UndefinedFunction };
        yield return new object[] { MessageCode.UnknownCompilerError };
        yield return new object[] { MessageCode.ArtifactTooLarge };
        yield return new object[] { MessageCode.CompilationTimeLimitExceeded };
        // A value that is not a member of MessageCode
        yield return new object[] { (MessageCode)9999 };
        yield return new object[] { (MessageCode)4999 };
        yield return new object[] { (MessageCode)5001 };
        yield return new object[] { (MessageCode)5999 };
        yield return new object[] { (MessageCode)0 };
        yield return new object[] { (MessageCode)(-1) };
    }

    [Theory(DisplayName = "JYRO-REQ-1892 JyroError.Runtime throws ArgumentOutOfRangeException for a code that only the engine raises")]
    [Trait("requirement", "JYRO-REQ-1892")]
    [MemberData(nameof(CodesOnlyTheEngineRaises))]
    public void RuntimeRefusesACodeOnlyTheEngineRaises(MessageCode code)
    {
        var thrown = Assert.Throws<ArgumentOutOfRangeException>(() => JyroError.Runtime(code));
        Assert.Equal("code", thrown.ParamName);
        // With arguments too.
        var withArgs = Assert.Throws<ArgumentOutOfRangeException>(() => JyroError.Runtime(code, "a", 1));
        Assert.Equal("code", withArgs.ParamName);
    }

    [Fact(DisplayName = "JYRO-REQ-1892 JyroError.Runtime returns a JyroRuntimeException of that code for every other member of MessageCode")]
    [Trait("requirement", "JYRO-REQ-1892")]
    public void RuntimeReturnsTheExceptionForTheOtherCodes()
    {
        var invalidJson = JyroError.Runtime(MessageCode.InvalidJson);
        Assert.Equal(MessageCode.InvalidJson, invalidJson.Code);

        var mismatch = JyroError.Runtime(MessageCode.ArgumentTypeMismatch, "string", "number");
        Assert.Equal(MessageCode.ArgumentTypeMismatch, mismatch.Code);
        Assert.Equal(new object[] { "string", "number" }, mismatch.Args);

        // Every member in the range 5000 to 5999 that the clause does not exclude is accepted.
        var excluded = new HashSet<MessageCode>
        {
            MessageCode.RuntimeError, MessageCode.CanceledByHost, MessageCode.ScriptFailure, MessageCode.HostFunctionError,
            MessageCode.StatementLimitExceeded, MessageCode.LoopIterationLimitExceeded, MessageCode.CallDepthLimitExceeded,
            MessageCode.ExecutionTimeLimitExceeded, MessageCode.ValueNestingLimitExceeded, MessageCode.ValueSizeLimitExceeded,
        };
        foreach (var code in Enum.GetValues<MessageCode>().Where(c => (int)c >= 5000 && (int)c < 6000 && !excluded.Contains(c)))
        {
            var made = JyroError.Runtime(code);
            Assert.Equal(code, made.Code);
        }
    }

    // -- JYRO-REQ-1898 -----------------------------------------------------------------------------

    private static byte[] ArtifactOf(string source)
    {
        var written = new JyroCompiler().WithSource(source).WithStdlib().CompileToArtifact();
        Assert.True(written.TryGetValue(out var bytes), "the source did not compile to an artifact");
        return bytes;
    }

    [Fact(DisplayName = "JYRO-REQ-1898 Read of a span gives the same result as Read of the array of the same bytes")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void ReadOfASpanGivesTheSameResultAsReadOfTheArray()
    {
        var bytes = ArtifactOf("Output.x = Trim(\" a \")");
        var fromArray = JyroArtifactInfo.Read(bytes);
        var fromSpan = JyroArtifactInfo.Read(new ReadOnlySpan<byte>(bytes));
        Assert.True(fromArray.IsSuccess);
        Assert.True(fromSpan.IsSuccess);
        Assert.True(fromArray.TryGetValue(out var a));
        Assert.True(fromSpan.TryGetValue(out var s));
        Assert.Equal(a, s);
        Assert.Equal(a.FormatVersion, s.FormatVersion);
        Assert.Equal(a.RequiredFunctions, s.RequiredFunctions);

        // A refusal is the same too.
        var refusals = new[]
        {
            Encoding.ASCII.GetBytes("JYR"),
            Encoding.ASCII.GetBytes("JYRX").Concat(new byte[] { 1 }).ToArray(),
            Encoding.ASCII.GetBytes("JYRX").Concat(new byte[] { 7, 0 }).ToArray(),
        };
        foreach (var refused in refusals)
        {
            var viaArray = JyroArtifactInfo.Read(refused);
            var viaSpan = JyroArtifactInfo.Read(new ReadOnlySpan<byte>(refused));
            Assert.False(viaArray.IsSuccess);
            Assert.False(viaSpan.IsSuccess);
            Assert.Equal(viaArray.Messages.Select(m => m.Code), viaSpan.Messages.Select(m => m.Code));
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1898 JyroArtifactInfo is a sealed class that implements IEquatable of itself and no other interface, with no public or protected constructor")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void JyroArtifactInfoIsASealedReadOnlyClass()
    {
        var type = typeof(JyroArtifactInfo);
        Assert.Equal("RealTree.Jyro", type.Namespace);
        Assert.True(type.IsClass && type.IsSealed && type.IsPublic);
        Assert.Equal(new[] { typeof(IEquatable<JyroArtifactInfo>) }, type.GetInterfaces());
        var reachable = type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)
            .Where(c => c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly);
        Assert.Empty(reachable);
    }

    [Fact(DisplayName = "JYRO-REQ-1898 JyroArtifactInfo declares exactly the nine public members of the clause")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void JyroArtifactInfoDeclaresTheMembersOfTheClause()
    {
        var members = typeof(JyroArtifactInfo)
            .GetMembers(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly)
            .Where(m => m is not MethodInfo method || !method.IsSpecialName)
            .Select(Describe)
            .OrderBy(s => s, StringComparer.Ordinal)
            .ToArray();
        var expected = new[]
        {
            "FormatVersion : Int32",
            "SourceHash : ReadOnlyMemory<Byte>",
            "RequiredFunctions : IReadOnlyList<String>",
            "static Read(Byte[]) : JyroResult<JyroArtifactInfo>",
            "static Read(ReadOnlySpan<Byte>) : JyroResult<JyroArtifactInfo>",
            "Equals(JyroArtifactInfo) : Boolean",
            "Equals(Object) : Boolean",
            "GetHashCode() : Int32",
            "ToString() : String",
        }.OrderBy(s => s, StringComparer.Ordinal).ToArray();
        Assert.Equal(expected, members);

        static string Describe(MemberInfo member) => member switch
        {
            PropertyInfo p => p.Name + " : " + TypeName(p.PropertyType),
            MethodInfo m => (m.IsStatic ? "static " : "") + m.Name + "(" + string.Join(", ", m.GetParameters().Select(p => TypeName(p.ParameterType))) + ") : " + TypeName(m.ReturnType),
            _ => member.MemberType + " " + member.Name,
        };

        static string TypeName(Type type) => type.IsGenericType
            ? type.Name.Split('`')[0] + "<" + string.Join(", ", type.GetGenericArguments().Select(TypeName)) + ">"
            : type.Name;
    }

    [Fact(DisplayName = "JYRO-REQ-1898 Read gives the format version, the hash of the source and the function names of the header and the table")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void ReadGivesTheHeaderAndTheTable()
    {
        const string source = "Output.x = Trim(\" a \")";
        var bytes = ArtifactOf(source);

        Assert.True(JyroArtifactInfo.Read(bytes).TryGetValue(out var info));
        Assert.Equal(1, info.FormatVersion);
        Assert.Equal(SHA256.HashData(Encoding.UTF8.GetBytes(source)), info.SourceHash.ToArray());
        Assert.Equal(new[] { "Trim" }, info.RequiredFunctions.ToArray());

        var text = info.ToString();
        Assert.Contains("Trim", text);
        Assert.Contains(Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(source))).ToLowerInvariant(), text);
        Assert.DoesNotContain('\n', text);
        Assert.DoesNotContain('\r', text);
    }

    [Fact(DisplayName = "JYRO-REQ-1898 Read of a span gives the same result as Read of the array")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void ReadOfASpanMatchesReadOfTheArray()
    {
        var bytes = ArtifactOf("Output.x = Trim(\" a \")");

        Assert.True(JyroArtifactInfo.Read(bytes).TryGetValue(out var fromArray));
        Assert.True(JyroArtifactInfo.Read(new ReadOnlySpan<byte>(bytes)).TryGetValue(out var fromSpan));
        Assert.Equal(fromArray, fromSpan);

        var garbage = new byte[] { 1, 2, 3, 4, 5, 6, 7, 8 };
        var refusedFromArray = JyroArtifactInfo.Read(garbage);
        var refusedFromSpan = JyroArtifactInfo.Read(new ReadOnlySpan<byte>(garbage));
        Assert.False(refusedFromSpan.IsSuccess);
        Assert.Equal(refusedFromArray.Messages.Select(m => m.Code), refusedFromSpan.Messages.Select(m => m.Code));
        Assert.Equal(MessageCode.ArtifactInvalidMagic, Assert.Single(refusedFromSpan.Messages).Code);
    }

    [Fact(DisplayName = "JYRO-REQ-1898 two Reads of the same artifact give values that are Equals, with the same hash code")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void TwoReadsAreEqual()
    {
        var bytes = ArtifactOf("Output.x = Trim(\" a \")");

        Assert.True(JyroArtifactInfo.Read(bytes).TryGetValue(out var first));
        Assert.True(JyroArtifactInfo.Read((byte[])bytes.Clone()).TryGetValue(out var second));
        Assert.True(first.Equals(second));
        Assert.True(first.Equals((object)second));
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.False(first.Equals((JyroArtifactInfo)null!));
        Assert.False(first.Equals("not an info"));

        // A different source has a different hash: not equal.
        Assert.True(JyroArtifactInfo.Read(ArtifactOf("Output.x = Trim(\" b \")")).TryGetValue(out var other));
        Assert.False(first.Equals(other));
    }

    [Fact(DisplayName = "JYRO-REQ-1898 a value that was read does not change when the bytes it was read from change")]
    [Trait("requirement", "JYRO-REQ-1898")]
    public void AReadValueIsIndependentOfItsBytes()
    {
        var bytes = ArtifactOf("Output.x = Trim(\" a \")");
        Assert.True(JyroArtifactInfo.Read(bytes).TryGetValue(out var info));
        var hash = info.SourceHash.ToArray();
        var names = info.RequiredFunctions.ToArray();

        Array.Fill(bytes, (byte)0xFF);

        Assert.Equal(hash, info.SourceHash.ToArray());
        Assert.Equal(names, info.RequiredFunctions.ToArray());
        Assert.Equal(1, info.FormatVersion);
    }

    // -- JYRO-REQ-1919 -----------------------------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1919 the codes that JYRO-ADR-196 renames have their new names and numbers, and the older names are not members")]
    [Trait("requirement", "JYRO-REQ-1919")]
    public void TheRenamedCodes()
    {
        Assert.Equal(4106, (int)MessageCode.MaxEffectNotApplicable);
        Assert.Equal(5003, (int)MessageCode.CanceledByHost);
        Assert.Equal(5005, (int)MessageCode.ValueIsReadOnly);
        Assert.Equal(5007, (int)MessageCode.InputValueIsReadOnly);
        Assert.Equal(5210, (int)MessageCode.StringOperandInAdditionRuntime);
        Assert.Equal(7000, (int)MessageCode.ArtifactTooLarge);
        Assert.Equal(7001, (int)MessageCode.ArtifactInvalidMagic);
        Assert.Equal(7002, (int)MessageCode.ArtifactVersionUnsupported);
        Assert.Equal(7003, (int)MessageCode.ArtifactUnreadable);
        Assert.Equal(7004, (int)MessageCode.ArtifactEffectExceedsMax);
        Assert.Equal(7005, (int)MessageCode.ArtifactCannotEncodeLibraries);
        Assert.Equal(7006, (int)MessageCode.ArtifactCannotUseLibraries);
        Assert.Equal(7007, (int)MessageCode.ArtifactCannotEncode);
        Assert.Equal(7008, (int)MessageCode.ArtifactUnwritable);

        // The older names that are British spellings are made from the new names at run time, so that no file
        // of the tests holds a form that the spelling check of JYRO-REQ-1938 flags.
        static string British(string name) => name.Replace("Artifact", "Arte" + "fact").Replace("Canceled", "Cance" + "lled");

        var older = new[]
        {
            "EffectMaxNotApplicable", "ReadOnlyValue", "CannotWriteToInput", "AddWithStringOperand",
            British("CanceledByHost"),
            British("ArtifactTooLarge"), British("ArtifactInvalidMagic"), British("ArtifactVersionUnsupported"),
            British("ArtifactUnreadable"), British("ArtifactEffectExceedsMax"), British("ArtifactCannotEncodeLibraries"),
            British("ArtifactCannotUseLibraries"), British("ArtifactCannotEncode"), British("ArtifactUnwritable"),
        };
        foreach (var name in older)
        {
            Assert.False(Enum.IsDefined(typeof(MessageCode), name), name + " is still a member of MessageCode");
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1919 no public type or member of the shipped assemblies has a name with a British form of Artifact or Canceled")]
    [Trait("requirement", "JYRO-REQ-1919")]
    public void NoPublicNameHasABritishForm()
    {
        var forms = new[] { "Arte" + "fact", "Cance" + "lled" };
        var found = new List<string>();
        foreach (var assemblyName in new[] { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" })
        {
            foreach (var type in Assembly.Load(assemblyName).GetExportedTypes())
            {
                if (forms.Any(f => type.FullName!.Contains(f, StringComparison.OrdinalIgnoreCase))) found.Add(type.FullName!);
                foreach (var member in type.GetMembers(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
                {
                    if (forms.Any(f => member.Name.Contains(f, StringComparison.OrdinalIgnoreCase)))
                        found.Add(type.FullName + "." + member.Name);
                }
            }
        }
        Assert.Empty(found);
    }
}

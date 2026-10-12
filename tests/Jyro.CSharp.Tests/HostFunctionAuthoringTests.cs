using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// How a host writes a function (JYRO-ADR-192): one Execute to override (JYRO-REQ-1889), arguments read through
// JyroArguments (JYRO-REQ-1890), an immutable signature made by a factory (JYRO-REQ-1893), and
// FunctionEffect and ParameterType as enumerations (JYRO-REQ-1894). Written from the specification alone.
public class HostFunctionAuthoringTests
{
    private const BindingFlags Declared =
        BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;

    private static readonly string[] ShippedAssemblies = { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static string TypeName(Type type)
    {
        if (type.IsByRef) return TypeName(type.GetElementType()!);
        if (type.IsArray) return TypeName(type.GetElementType()!) + "[]";
        if (type.IsGenericType)
            return type.Name.Split('`')[0] + "<" + string.Join(", ", type.GetGenericArguments().Select(TypeName)) + ">";
        return type.Name;
    }

    private static string Describe(MemberInfo member) => member switch
    {
        ConstructorInfo c => ".ctor(" + string.Join(", ", c.GetParameters().Select(p => TypeName(p.ParameterType) + " " + p.Name)) + ")",
        MethodInfo m => (m.IsStatic ? "static " : "") + TypeName(m.ReturnType) + " " + m.Name
            + (m.IsGenericMethodDefinition ? "<" + string.Join(", ", m.GetGenericArguments().Select(a => a.Name)) + ">" : "")
            + "(" + string.Join(", ", m.GetParameters().Select(p => TypeName(p.ParameterType) + " " + p.Name)) + ")",
        PropertyInfo p => (p.GetGetMethod()!.IsStatic ? "static " : "") + TypeName(p.PropertyType) + " " + p.Name
            + " { " + (p.GetGetMethod() is not null ? "get; " : "") + (p.GetSetMethod() is not null ? "set; " : "") + "}",
        _ => member.MemberType + " " + member.Name,
    };

    // Equals(object), GetHashCode() and ToString() are left out when the clause names them as allowed.
    private static bool IsObjectOverride(MemberInfo m) => m is MethodInfo method && !method.IsStatic
        && ((method.Name == "Equals" && method.GetParameters().Select(p => p.ParameterType).SequenceEqual(new[] { typeof(object) }))
            || (method.Name == "GetHashCode" && method.GetParameters().Length == 0)
            || (method.Name == "ToString" && method.GetParameters().Length == 0));

    private static string[] PublicMembers(Type type, bool allowObjectOverrides = false) =>
        type.GetMembers(Declared)
            .Where(m => m is not MethodInfo method || !method.IsSpecialName)
            .Where(m => !(allowObjectOverrides && IsObjectOverride(m)))
            .Select(Describe)
            .OrderBy(s => s, StringComparer.Ordinal)
            .ToArray();

    private static string[] Sorted(params string[] lines) => lines.OrderBy(s => s, StringComparer.Ordinal).ToArray();

    private static string[] ArgStrings(DiagnosticMessage d) =>
        d.Args.Select(a => Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture)!).ToArray();

    // -- JYRO-REQ-1889 ---------------------------------------------------------------------------

    /// <summary>A function written the way the requirement says: it overrides only Effect and Execute.</summary>
    private sealed class MinimalFunction : JyroFunctionBase
    {
        public MinimalFunction()
            : base("Minimal", JyroFunctionSignature.Create(ParameterType.String, Parameter.Required("text", ParameterType.String)))
        { }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> arguments, JyroExecutionContext context) =>
            new JyroString(arguments.GetString(0) + "!");
    }

    [Fact(DisplayName = "JYRO-REQ-1889 JyroFunctionBase declares exactly the six public members of the clause")]
    [Trait("requirement", "JYRO-REQ-1889")]
    public void JyroFunctionBaseDeclaresExactlyTheMembersOfTheClause()
    {
        var type = typeof(JyroFunctionBase);
        Assert.Equal("RealTree.Jyro", type.Namespace);
        Assert.True(type.IsPublic && type.IsAbstract && !type.IsSealed, "JyroFunctionBase is a public abstract class");
        Assert.Contains(typeof(IJyroFunction), type.GetInterfaces());
        Assert.Contains(typeof(IJyroCallCheck), type.GetInterfaces());

        Assert.Equal(
            Sorted(
                ".ctor(String name, JyroFunctionSignature signature)",
                "String Name { get; }",
                "JyroFunctionSignature Signature { get; }",
                "FunctionEffect Effect { get; }",
                "JyroValue Execute(IReadOnlyList<JyroValue> arguments, JyroExecutionContext context)",
                "Void CheckCall(JyroCallSite call)"),
            PublicMembers(type));
    }

    [Fact(DisplayName = "JYRO-REQ-1889 Effect and Execute are abstract and CheckCall is virtual, with a parameter named call")]
    [Trait("requirement", "JYRO-REQ-1889")]
    public void EffectAndExecuteAreAbstractAndCheckCallIsVirtual()
    {
        var type = typeof(JyroFunctionBase);
        Assert.True(type.GetProperty("Effect")!.GetGetMethod()!.IsAbstract, "Effect is abstract");
        var execute = type.GetMethod("Execute", Declared)!;
        Assert.True(execute.IsAbstract, "Execute is abstract");
        Assert.Equal(new[] { typeof(IReadOnlyList<JyroValue>), typeof(JyroExecutionContext) }, execute.GetParameters().Select(p => p.ParameterType).ToArray());
        Assert.Equal(typeof(JyroValue), execute.ReturnType);

        var check = type.GetMethod("CheckCall", Declared)!;
        Assert.True(check.IsVirtual && !check.IsAbstract, "CheckCall is virtual and has a default");
        Assert.Equal("call", Assert.Single(check.GetParameters()).Name);
        Assert.Equal(typeof(void), check.ReturnType);
    }

    [Fact(DisplayName = "JYRO-REQ-1889 JyroFunctionBase has no ExecuteImpl, no RaiseError and no member that starts with Get and ends with Argument")]
    [Trait("requirement", "JYRO-REQ-1889")]
    public void NoOldMemberIsReachable()
    {
        var reachable = typeof(JyroFunctionBase)
            .GetMembers(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static)
            .Where(m => m is not MethodBase method || method.IsPublic || method.IsFamily || method.IsFamilyOrAssembly)
            .Select(m => m.Name)
            .Where(n => n == "ExecuteImpl" || n == "RaiseError" || (n.StartsWith("Get", StringComparison.Ordinal) && n.EndsWith("Argument", StringComparison.Ordinal)))
            .ToList();
        Assert.Empty(reachable);
    }

    [Fact(DisplayName = "JYRO-REQ-1889 a class that overrides only Effect and Execute can be registered with AddFunction and its override runs")]
    [Trait("requirement", "JYRO-REQ-1889")]
    public void AClassThatOverridesOnlyEffectAndExecuteRuns()
    {
        var result = new JyroRunner(
                new JyroCompiler().WithStdlib().AddFunction(new MinimalFunction()).WithSource("Output.r = Minimal('hey')").CompileOrThrow())
            .Execute();

        Assert.True(result.IsSuccess, string.Join("; ", result.Messages.Select(m => m.Message)));
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal("hey!", output.GetProperty("r").ToStringValue());
    }

    [Fact(DisplayName = "JYRO-REQ-1889 a call of IJyroFunction.Execute on an instance runs the override of Execute")]
    [Trait("requirement", "JYRO-REQ-1889")]
    public void TheInterfaceCallRunsTheOverride()
    {
        IJyroFunction function = new MinimalFunction();
        Assert.Equal("Minimal", function.Name);
        Assert.Equal(FunctionEffect.Pure, function.Effect);

        var value = function.Execute(new JyroValue[] { new JyroString("abc") }, new JyroExecutionContext());

        Assert.Equal("abc!", value.ToStringValue());
    }

    // -- JYRO-REQ-1890 ---------------------------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1890 JyroArguments is a static class that declares exactly the eleven readers of the clause, each an extension method")]
    [Trait("requirement", "JYRO-REQ-1890")]
    public void JyroArgumentsDeclaresExactlyTheReadersOfTheClause()
    {
        var type = typeof(JyroArguments);
        Assert.Equal("RealTree.Jyro", type.Namespace);
        Assert.True(type.IsPublic && type.IsAbstract && type.IsSealed, "JyroArguments is a public static class");

        Assert.Equal(
            Sorted(
                "static String GetString(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static Double GetNumber(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static Decimal GetDecimal(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static Boolean GetBoolean(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static JyroArray GetArray(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static JyroObject GetObject(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static Int32 GetInt32(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static Int32 GetNonNegativeInt32(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static Int64 GetInt64(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static T Get<T>(IReadOnlyList<JyroValue> arguments, Int32 index)",
                "static T GetOptional<T>(IReadOnlyList<JyroValue> arguments, Int32 index, T defaultValue)"),
            PublicMembers(type));

        foreach (var method in type.GetMethods(Declared))
        {
            Assert.True(method.IsDefined(typeof(ExtensionAttribute), false), method.Name + " is an extension method");
            Assert.Equal(typeof(IReadOnlyList<JyroValue>), method.GetParameters()[0].ParameterType);
        }

        foreach (var name in new[] { "Get", "GetOptional" })
        {
            var generic = type.GetMethod(name, Declared)!;
            Assert.True(generic.IsGenericMethodDefinition);
            var constraint = Assert.Single(generic.GetGenericArguments()[0].GetGenericParameterConstraints());
            Assert.Equal(typeof(JyroValue), constraint);
        }
    }

    /// <summary>A function Pad(text, width) whose body is a reader given by the test.</summary>
    private sealed class PadFunction : JyroFunctionBase
    {
        private readonly Func<IReadOnlyList<JyroValue>, JyroValue> _body;

        public PadFunction(Func<IReadOnlyList<JyroValue>, JyroValue> body, bool bothOptional = false)
            : base("Pad",
                   JyroFunctionSignature.Create(
                       ParameterType.Any,
                       bothOptional ? Parameter.Optional("text", ParameterType.String) : Parameter.Required("text", ParameterType.String),
                       Parameter.Optional("width", ParameterType.Number)))
        {
            _body = body;
        }

        public override FunctionEffect Effect => FunctionEffect.Pure;

        public override JyroValue Execute(IReadOnlyList<JyroValue> arguments, JyroExecutionContext context) => _body(arguments);
    }

    private static JyroResult<JyroValue> RunPad(Func<IReadOnlyList<JyroValue>, JyroValue> body, string call, bool bothOptional = false) =>
        new JyroRunner(
                new JyroCompiler().WithStdlib().AddFunction(new PadFunction(body, bothOptional)).WithSource("Output.r = " + call).CompileOrThrow())
            .Execute();

    [Fact(DisplayName = "JYRO-REQ-1890 GetString of an index beyond the arguments returns the empty string")]
    [Trait("requirement", "JYRO-REQ-1890")]
    public void GetStringOfAMissingArgumentIsEmpty()
    {
        var result = RunPad(a => new JyroString(a.GetString(1)), "Pad('a')");

        Assert.True(result.IsSuccess, string.Join("; ", result.Messages.Select(m => m.Message)));
        Assert.True(result.TryGetValue(out var output));
        Assert.Equal("", output.GetProperty("r").ToStringValue());
    }

    [Fact(DisplayName = "JYRO-REQ-1890 GetNumber of an index beyond the arguments raises ArgumentTypeMismatch naming number and null, and not ArgumentNotProvided")]
    [Trait("requirement", "JYRO-REQ-1890")]
    public void GetNumberOfAMissingArgumentRaisesArgumentTypeMismatch()
    {
        var result = RunPad(a => new JyroNumber(a.GetNumber(1)), "Pad('a')");

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.ArgumentTypeMismatch);
        var args = ArgStrings(d);
        Assert.True(Array.IndexOf(args, "number") >= 0 && Array.IndexOf(args, "null") > Array.IndexOf(args, "number"),
            "the arguments name number, then null: " + string.Join(", ", args));
        Assert.DoesNotContain(result.Messages, m => m.Code == MessageCode.ArgumentNotProvided);
    }

    [Fact(DisplayName = "JYRO-REQ-1890 GetInt32 of 2.5 raises IntegerRequired whose arguments name the function and the parameter at that index of its signature")]
    [Trait("requirement", "JYRO-REQ-1890")]
    public void GetInt32NamesTheFunctionAndTheParameterOfTheSignature()
    {
        var first = RunPad(a => new JyroNumber(a.GetInt32(0)), "Pad(2.5)");
        var firstMessage = Assert.Single(first.Messages, m => m.Code == MessageCode.IntegerRequired);
        var firstArgs = ArgStrings(firstMessage);
        Assert.Contains(firstArgs, a => a == "Pad" || a == "Pad()");
        Assert.Contains("text", firstArgs);
        Assert.DoesNotContain("width", firstArgs);

        // The second parameter of the signature is named for index 1.
        var second = RunPad(a => new JyroNumber(a.GetInt32(1)), "Pad('a', 2.5)");
        var secondMessage = Assert.Single(second.Messages, m => m.Code == MessageCode.IntegerRequired);
        var secondArgs = ArgStrings(secondMessage);
        Assert.Contains(secondArgs, a => a == "Pad" || a == "Pad()");
        Assert.Contains("width", secondArgs);
        Assert.DoesNotContain("text", secondArgs);
    }

    public static IEnumerable<object[]> ReadersOfAMissingArgument() => new[]
    {
        // reader, the code of the error it raises ("" when it returns), and what it returns as JSON
        new object[] { "GetString", "", "\"\"" },
        new object[] { "GetArray", "", "[]" },
        new object[] { "GetObject", "", "{}" },
        new object[] { "GetNumber", "ArgumentTypeMismatch", "" },
        new object[] { "GetDecimal", "ArgumentTypeMismatch", "" },
        new object[] { "GetBoolean", "ArgumentTypeMismatch", "" },
        new object[] { "GetInt32", "ArgumentTypeMismatch", "" },
        new object[] { "GetNonNegativeInt32", "ArgumentTypeMismatch", "" },
        new object[] { "GetInt64", "ArgumentTypeMismatch", "" },
        new object[] { "Get<JyroString>", "ArgumentTypeMismatch", "" },
        new object[] { "Get<JyroValue>", "", "null" },
        new object[] { "GetOptional<JyroString>", "", "\"dflt\"" },
    };

    private static JyroValue ReadAt(IReadOnlyList<JyroValue> a, string reader, int i) => reader switch
    {
        "GetString" => new JyroString(a.GetString(i)),
        "GetArray" => a.GetArray(i),
        "GetObject" => a.GetObject(i),
        "GetNumber" => new JyroNumber(a.GetNumber(i)),
        "GetDecimal" => new JyroNumber(a.GetDecimal(i)),
        "GetBoolean" => JyroBoolean.FromBoolean(a.GetBoolean(i)),
        "GetInt32" => new JyroNumber(a.GetInt32(i)),
        "GetNonNegativeInt32" => new JyroNumber(a.GetNonNegativeInt32(i)),
        "GetInt64" => new JyroNumber(a.GetInt64(i)),
        "Get<JyroString>" => a.Get<JyroString>(i),
        "Get<JyroValue>" => a.Get<JyroValue>(i),
        "GetOptional<JyroString>" => a.GetOptional(i, new JyroString("dflt")),
        _ => throw new ArgumentException("unknown reader " + reader),
    };

    [Theory(DisplayName = "JYRO-REQ-1890 every reader reads an index beyond the arguments as null, and none raises ArgumentNotProvided")]
    [Trait("requirement", "JYRO-REQ-1890")]
    [MemberData(nameof(ReadersOfAMissingArgument))]
    public void EveryReaderReadsAMissingArgumentAsNull(string reader, string code, string json)
    {
        foreach (var index in new[] { 0, 1, 5 })
        {
            var result = RunPad(a => ReadAt(a, reader, index), "Pad()", bothOptional: true);

            Assert.DoesNotContain(result.Messages, m => m.Code == MessageCode.ArgumentNotProvided);
            if (code == "")
            {
                Assert.True(result.IsSuccess, reader + " at " + index + ": " + string.Join("; ", result.Messages.Select(m => m.Message)));
                Assert.True(result.TryGetValue(out var output));
                Assert.Equal(json, output.GetProperty("r").ToJson());
            }
            else
            {
                Assert.False(result.IsSuccess, reader + " at " + index + " must fail");
                Assert.Contains(result.Messages, m => m.Code == Enum.Parse<MessageCode>(code));
            }
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1890 a reader of a given null and a reader of a missing argument give the same diagnostic")]
    [Trait("requirement", "JYRO-REQ-1890")]
    public void AMissingArgumentAndANullGiveTheSameDiagnostic()
    {
        foreach (var reader in new[] { "GetNumber", "GetDecimal", "GetBoolean", "GetInt32", "GetInt64", "Get<JyroString>" })
        {
            var missing = RunPad(a => ReadAt(a, reader, 1), "Pad('a')");
            var given = RunPad(a => ReadAt(a, reader, 1), "Pad('a', null)");

            var m = Assert.Single(missing.Messages);
            var g = Assert.Single(given.Messages);
            Assert.Equal(g.Code, m.Code);
            Assert.Equal(ArgStrings(g), ArgStrings(m));
        }
    }

    // -- JYRO-REQ-1893 ---------------------------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1893 JyroFunctionSignature is a sealed class with the members of the clause and no constructor or setter that a host can reach")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void JyroFunctionSignatureHasTheMembersOfTheClause()
    {
        var type = typeof(JyroFunctionSignature);
        Assert.Equal("RealTree.Jyro", type.Namespace);
        Assert.True(type.IsClass && type.IsSealed && type.IsPublic && !type.IsAbstract);
        AssertNoReachableConstructor(type);
        AssertNoPublicSetter(type);

        Assert.Equal(
            Sorted(
                "static JyroFunctionSignature Create(ParameterType returnType, Parameter[] parameters)",
                "static JyroFunctionSignature Create(IEnumerable<Parameter> parameters, ParameterType returnType)",
                "IReadOnlyList<Parameter> Parameters { get; }",
                "ParameterType ReturnType { get; }",
                "Int32 MinArgs { get; }",
                "Int32 MaxArgs { get; }"),
            PublicMembers(type, allowObjectOverrides: true));

        Assert.Empty(type.GetMember("Name", Declared));
        Assert.True(type.GetMethod("Create", new[] { typeof(ParameterType), typeof(Parameter[]) })!
            .GetParameters()[1].IsDefined(typeof(ParamArrayAttribute), false), "the parameters are a params array");
    }

    [Fact(DisplayName = "JYRO-REQ-1893 Parameter is a sealed class with the members of the clause and no constructor or setter that a host can reach")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void ParameterHasTheMembersOfTheClause()
    {
        var type = typeof(Parameter);
        Assert.Equal("RealTree.Jyro", type.Namespace);
        Assert.True(type.IsClass && type.IsSealed && type.IsPublic && !type.IsAbstract);
        AssertNoReachableConstructor(type);
        AssertNoPublicSetter(type);

        Assert.Equal(
            Sorted(
                "static Parameter Required(String name, ParameterType type)",
                "static Parameter Optional(String name, ParameterType type)",
                "String Name { get; }",
                "ParameterType Type { get; }",
                "Boolean IsOptional { get; }"),
            PublicMembers(type, allowObjectOverrides: true));
    }

    private static void AssertNoReachableConstructor(Type type)
    {
        var reachable = type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)
            .Where(c => c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly);
        Assert.Empty(reachable);
    }

    private static void AssertNoPublicSetter(Type type)
    {
        var withSetter = type.GetProperties(Declared).Where(p => p.GetSetMethod() is not null).Select(p => p.Name);
        Assert.Empty(withSetter);
    }

    [Fact(DisplayName = "JYRO-REQ-1893 Create with a required and an optional parameter has MinArgs 1 and MaxArgs 2")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void MinArgsCountsTheRequiredAndMaxArgsAllOfThem()
    {
        var signature = JyroFunctionSignature.Create(
            ParameterType.String,
            Parameter.Required("text", ParameterType.String),
            Parameter.Optional("width", ParameterType.Number));

        Assert.Equal(1, signature.MinArgs);
        Assert.Equal(2, signature.MaxArgs);
        Assert.Equal(ParameterType.String, signature.ReturnType);
        Assert.Equal(new[] { "text", "width" }, signature.Parameters.Select(p => p.Name).ToArray());
        Assert.Equal(new[] { ParameterType.String, ParameterType.Number }, signature.Parameters.Select(p => p.Type).ToArray());
        Assert.Equal(new[] { false, true }, signature.Parameters.Select(p => p.IsOptional).ToArray());

        var none = JyroFunctionSignature.Create(ParameterType.Null);
        Assert.Empty(none.Parameters);
        Assert.Equal(0, none.MinArgs);
        Assert.Equal(0, none.MaxArgs);

        // MinArgs is the number of parameters that are not optional, wherever they stand.
        var mixed = JyroFunctionSignature.Create(
            new[]
            {
                Parameter.Optional("a", ParameterType.Any),
                Parameter.Required("b", ParameterType.Any),
                Parameter.Required("c", ParameterType.Any),
            },
            ParameterType.Any);
        Assert.Equal(2, mixed.MinArgs);
        Assert.Equal(3, mixed.MaxArgs);
        Assert.Equal(new[] { "a", "b", "c" }, mixed.Parameters.Select(p => p.Name).ToArray());
    }

    [Fact(DisplayName = "JYRO-REQ-1893 Parameters does not change when the host changes the array it passed")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void ParametersDoNotChangeWhenTheArrayChanges()
    {
        var given = new[] { Parameter.Required("a", ParameterType.String), Parameter.Optional("b", ParameterType.Number) };
        var signature = JyroFunctionSignature.Create(ParameterType.Any, given);

        given[0] = Parameter.Required("changed", ParameterType.Boolean);
        given[1] = Parameter.Required("alsoChanged", ParameterType.Boolean);

        Assert.Equal(new[] { "a", "b" }, signature.Parameters.Select(p => p.Name).ToArray());
        Assert.Equal(1, signature.MinArgs);
        Assert.Equal(2, signature.MaxArgs);
    }

    [Fact(DisplayName = "JYRO-REQ-1893 Parameters does not change when the host changes the sequence it passed")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void ParametersDoNotChangeWhenTheSequenceChanges()
    {
        var given = new List<Parameter> { Parameter.Required("a", ParameterType.String) };
        var signature = JyroFunctionSignature.Create(given, ParameterType.Any);

        given.Add(Parameter.Required("added", ParameterType.Number));
        given[0] = Parameter.Optional("changed", ParameterType.Boolean);

        Assert.Single(signature.Parameters);
        Assert.Equal("a", signature.Parameters[0].Name);
        Assert.False(signature.Parameters[0].IsOptional);
        Assert.Equal(1, signature.MinArgs);
        Assert.Equal(1, signature.MaxArgs);
    }

    [Fact(DisplayName = "JYRO-REQ-1893 Required and Optional make a parameter with the name, the type and IsOptional given")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void TheFactoriesOfParameter()
    {
        var required = Parameter.Required("n", ParameterType.Number);
        var optional = Parameter.Optional("o", ParameterType.Array);

        Assert.Equal("n", required.Name);
        Assert.Equal(ParameterType.Number, required.Type);
        Assert.False(required.IsOptional);
        Assert.Equal("o", optional.Name);
        Assert.Equal(ParameterType.Array, optional.Type);
        Assert.True(optional.IsOptional);
    }

    [Fact(DisplayName = "JYRO-REQ-1893 no public type of the shipped assemblies is named FunctionSignatures")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void FunctionSignaturesIsGone()
    {
        var found = ShippedAssemblies
            .SelectMany(name => Assembly.Load(name).GetExportedTypes())
            .Where(t => t.Name == "FunctionSignatures")
            .Select(t => t.FullName)
            .ToList();
        Assert.Empty(found);
    }

    [Fact(DisplayName = "JYRO-REQ-1893 a registered function's signature is read-only and the function runs under the arity of the signature")]
    [Trait("requirement", "JYRO-REQ-1893")]
    public void ARegisteredSignatureCannotBeChangedAndSetsTheArity()
    {
        var function = new PadFunction(a => new JyroString(a.GetString(0)));

        Assert.Equal(1, function.Signature.MinArgs);
        Assert.Equal(2, function.Signature.MaxArgs);

        var tooFew = new JyroCompiler().WithStdlib().AddFunction(function).WithSource("Output.r = Pad()").Compile();
        Assert.False(tooFew.IsSuccess);

        var tooMany = new JyroCompiler().WithStdlib().AddFunction(function).WithSource("Output.r = Pad('a', 1, 2)").Compile();
        Assert.False(tooMany.IsSuccess);

        var exactly = new JyroCompiler().WithStdlib().AddFunction(function).WithSource("Output.r = Pad('a', 1)").Compile();
        Assert.True(exactly.IsSuccess, string.Join("; ", exactly.Messages.Select(m => m.Message)));
    }

    // -- JYRO-REQ-1894 ---------------------------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1894 FunctionEffect is an enumeration with Pure 0, NonDeterministic 1, Blocking 2 and Mutating 3")]
    [Trait("requirement", "JYRO-REQ-1894")]
    public void FunctionEffectIsAnEnumeration()
    {
        Assert.True(typeof(FunctionEffect).IsEnum);
        Assert.Equal("RealTree.Jyro", typeof(FunctionEffect).Namespace);
        Assert.Equal(
            new[] { ("Pure", 0), ("NonDeterministic", 1), ("Blocking", 2), ("Mutating", 3) },
            Enum.GetNames<FunctionEffect>().Select(n => (n, (int)Enum.Parse<FunctionEffect>(n))).OrderBy(p => p.Item2).ToArray());
        Assert.Equal(0, (int)FunctionEffect.Pure);
        Assert.Equal(1, (int)FunctionEffect.NonDeterministic);
        Assert.Equal(2, (int)FunctionEffect.Blocking);
        Assert.Equal(3, (int)FunctionEffect.Mutating);
    }

    [Fact(DisplayName = "JYRO-REQ-1894 effects compare in the order Pure, NonDeterministic, Blocking, Mutating and a switch on an effect compiles")]
    [Trait("requirement", "JYRO-REQ-1894")]
    public void EffectsCompareAndSwitch()
    {
        Assert.True(FunctionEffect.Blocking < FunctionEffect.Mutating);
        Assert.True(FunctionEffect.Pure < FunctionEffect.NonDeterministic);
        Assert.True(FunctionEffect.NonDeterministic < FunctionEffect.Blocking);
        Assert.False(FunctionEffect.Mutating < FunctionEffect.Blocking);

        static string Describe(FunctionEffect effect)
        {
            switch (effect)
            {
                case FunctionEffect.Pure: return "pure";
                case FunctionEffect.NonDeterministic: return "random";
                case FunctionEffect.Blocking: return "waits";
                case FunctionEffect.Mutating: return "changes";
                default: return "unknown";
            }
        }

        Assert.Equal("pure", Describe(FunctionEffect.Pure));
        Assert.Equal("random", Describe(FunctionEffect.NonDeterministic));
        Assert.Equal("waits", Describe(FunctionEffect.Blocking));
        Assert.Equal("changes", Describe(FunctionEffect.Mutating));
    }

    [Fact(DisplayName = "JYRO-REQ-1894 ParameterType is an enumeration with the members and values of the clause")]
    [Trait("requirement", "JYRO-REQ-1894")]
    public void ParameterTypeIsAnEnumeration()
    {
        Assert.True(typeof(ParameterType).IsEnum);
        Assert.Equal("RealTree.Jyro", typeof(ParameterType).Namespace);
        Assert.Equal(
            new[] { ("Null", 0), ("Boolean", 1), ("Number", 2), ("String", 3), ("Array", 4), ("Object", 5), ("Function", 6), ("Any", 7) },
            Enum.GetNames<ParameterType>().Select(n => (n, (int)Enum.Parse<ParameterType>(n))).OrderBy(p => p.Item2).ToArray());
    }

    [Fact(DisplayName = "JYRO-REQ-1894 each ParameterType member other than Any has the value of the JyroValueType member of its name")]
    [Trait("requirement", "JYRO-REQ-1894")]
    public void ParameterTypeMirrorsJyroValueType()
    {
        Assert.Equal((int)JyroValueType.Number, (int)ParameterType.Number);
        foreach (var name in Enum.GetNames<JyroValueType>())
        {
            Assert.True(Enum.IsDefined(typeof(ParameterType), name), name + " is a member of ParameterType");
            Assert.Equal((int)Enum.Parse<JyroValueType>(name), (int)Enum.Parse<ParameterType>(name));
        }
    }
}

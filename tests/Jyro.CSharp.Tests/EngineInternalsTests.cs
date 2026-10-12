using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// JYRO-ADR-191: the engine's internals are internal, and compiled code binds them. What a C# host can
// reach is what these tests look at: reflection over the five shipped assemblies, the C# compiler
// run over host code, and an F# script that opens the namespace.
public class EngineInternalsTests
{
    private static readonly string[] ShippedAssemblies =
        { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static IEnumerable<Assembly> Shipped() => ShippedAssemblies.Select(Assembly.Load);

    private static IEnumerable<Type> ExportedTypes() => Shipped().SelectMany(a => a.GetExportedTypes());

    private const BindingFlags AllInstance =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance;

    private static bool Reachable(MethodBase m) => m.IsPublic || m.IsFamily || m.IsFamilyOrAssembly;

    // -- Reflection helpers ---------------------------------------------------

    // Every type a signature mentions: the type itself, its element type, and its generic arguments.
    private static IEnumerable<Type> Mentioned(Type t)
    {
        yield return t;
        if (t.HasElementType)
            foreach (var inner in Mentioned(t.GetElementType()!)) yield return inner;
        if (t.IsGenericType)
            foreach (var arg in t.GetGenericArguments())
                foreach (var inner in Mentioned(arg)) yield return inner;
    }

    // The types named by the public and protected members of a type: parameters, returns, properties,
    // fields, events and constructors.
    private static IEnumerable<Type> SignatureTypes(Type type)
    {
        const BindingFlags flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance
            | BindingFlags.Static | BindingFlags.DeclaredOnly;
        foreach (var c in type.GetConstructors(flags).Where(Reachable))
            foreach (var p in c.GetParameters())
                foreach (var m in Mentioned(p.ParameterType)) yield return m;
        foreach (var method in type.GetMethods(flags).Where(Reachable))
        {
            foreach (var m in Mentioned(method.ReturnType)) yield return m;
            foreach (var p in method.GetParameters())
                foreach (var m in Mentioned(p.ParameterType)) yield return m;
        }
        foreach (var f in type.GetFields(flags).Where(f => f.IsPublic || f.IsFamily || f.IsFamilyOrAssembly))
            foreach (var m in Mentioned(f.FieldType)) yield return m;
        // Property and event accessors are methods, so the loop over methods has them.
    }

    private static bool ExposesType(string fullName) =>
        ExportedTypes().Any(t => SignatureTypes(t).Any(s => s.FullName == fullName));

    private static void AssertNotPublic(string fullName)
    {
        // The spec's own probe, and the same search over every shipped assembly.
        var viaCore = typeof(JyroValue).Assembly.GetType(fullName);
        Assert.True(viaCore is null || (!viaCore.IsPublic && !viaCore.IsNestedPublic),
            fullName + " is a public type of Jyro.Core");
        foreach (var a in Shipped())
        {
            var t = a.GetType(fullName);
            Assert.True(t is null || (!t.IsPublic && !t.IsNestedPublic),
                fullName + " is a public type of " + a.GetName().Name);
        }
        var simple = fullName.Substring(fullName.LastIndexOf('.') + 1);
        Assert.DoesNotContain(ExportedTypes(), t => t.Name == simple);
    }

    // -- JYRO-REQ-1874 --------------------------------------------------------

    [Theory(DisplayName = "JYRO-REQ-1874 the limiter and the two recursion guards are not public types")]
    [Trait("requirement", "JYRO-REQ-1874")]
    [InlineData("RealTree.Jyro.JyroResourceLimiter")]
    [InlineData("RealTree.Jyro.ValueRecursionGuard")]
    [InlineData("RealTree.Jyro.HostGraphCycleGuard")]
    public void TheLimiterAndTheGuardsAreNotPublic(string fullName) => AssertNotPublic(fullName);

    [Fact(DisplayName = "JYRO-REQ-1874 no public or protected member of the shipped assemblies takes or returns the limiter or a guard")]
    [Trait("requirement", "JYRO-REQ-1874")]
    public void NoMemberSignatureNamesTheLimiterOrAGuard()
    {
        // The walker must see a type it is told about, or "no member names it" proves nothing.
        Assert.True(ExposesType("RealTree.Jyro.JyroValue"), "the signature walker does not see JyroValue");
        Assert.True(ExposesType("RealTree.Jyro.JyroExecutionOptions"), "the signature walker does not see JyroExecutionOptions");

        Assert.False(ExposesType("RealTree.Jyro.JyroResourceLimiter"), "a member names JyroResourceLimiter");
        Assert.False(ExposesType("RealTree.Jyro.ValueRecursionGuard"), "a member names ValueRecursionGuard");
        Assert.False(ExposesType("RealTree.Jyro.HostGraphCycleGuard"), "a member names HostGraphCycleGuard");
    }

    [Fact(DisplayName = "JYRO-REQ-1874 FromJson then ToJson of [[1]] gives [[1]] on a fresh thread, whatever host code ran there before")]
    [Trait("requirement", "JYRO-REQ-1874")]
    public void AWalkIsNotPoisonedByHostCodeOnTheThread()
    {
        string? fresh = null, afterRun = null, afterRefusals = null, afterDeepWalk = null;
        Exception? failure = null;
        var t = new Thread(() =>
        {
            try
            {
                fresh = JyroValue.FromJson("[[1]]").ToJson();

                // Host code that drives the engine on this thread: a whole execution.
                var program = new JyroCompiler().WithStdlib().WithSource("Output.v = Input.a").CompileOrThrow();
                var input = JyroValue.FromJson("{\"a\":[[1]]}");
                var result = new JyroRunner(program).WithInput(input).Execute();
                Assert.True(result.IsSuccess);
                afterRun = JyroValue.FromJson("[[1]]").ToJson();

                // Walks that fail on the nesting maximum, many more times than the maximum itself.
                var tooDeep = string.Concat(Enumerable.Repeat("[", 300)) + string.Concat(Enumerable.Repeat("]", 300));
                for (var i = 0; i < 400; i++)
                    Assert.ThrowsAny<Exception>(() => JyroValue.FromJson(tooDeep));
                afterRefusals = JyroValue.FromJson("[[1]]").ToJson();

                // A walk at a depth the guard allows, made repeatedly.
                var deep = string.Concat(Enumerable.Repeat("[", 100)) + string.Concat(Enumerable.Repeat("]", 100));
                for (var i = 0; i < 400; i++)
                    _ = JyroValue.FromJson(deep).ToJson();
                afterDeepWalk = JyroValue.FromJson("[[1]]").ToJson();
            }
            catch (Exception e)
            {
                failure = e;
            }
        });
        t.Start();
        Assert.True(t.Join(TimeSpan.FromMinutes(2)), "the thread did not finish");
        Assert.Null(failure);
        Assert.Equal("[[1]]", fresh);
        Assert.Equal("[[1]]", afterRun);
        Assert.Equal("[[1]]", afterRefusals);
        Assert.Equal("[[1]]", afterDeepWalk);
    }

    // -- JYRO-REQ-1875 --------------------------------------------------------

    private static readonly Type CompiledProgramType = typeof(CompiledProgram);

    [Fact(DisplayName = "JYRO-REQ-1875 CompiledProgram is a sealed class with no public or protected constructor")]
    [Trait("requirement", "JYRO-REQ-1875")]
    public void CompiledProgramIsSealedWithNoReachableConstructor()
    {
        Assert.Equal("RealTree.Jyro", CompiledProgramType.Namespace);
        Assert.True(CompiledProgramType.IsClass && CompiledProgramType.IsSealed);
        var reachable = CompiledProgramType.GetConstructors(AllInstance).Where(Reachable).ToList();
        Assert.True(reachable.Count == 0,
            "CompiledProgram has constructors a host can call: " + string.Join("; ", reachable));
    }

    [Fact(DisplayName = "JYRO-REQ-1875 CompiledProgram has no member named CompiledFunction and none of delegate type or taking a JyroExecutionContext")]
    [Trait("requirement", "JYRO-REQ-1875")]
    public void CompiledProgramHasNoDelegateAndNoContext()
    {
        // The detector must find what it looks for: JyroFunction takes a Func in its constructor.
        Assert.Contains(typeof(JyroFunction), ExportedTypes().Where(t => SignatureTypes(t)
            .Any(s => typeof(Delegate).IsAssignableFrom(s))));

        const BindingFlags all = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance
            | BindingFlags.Static | BindingFlags.FlattenHierarchy;
        Assert.DoesNotContain(CompiledProgramType.GetMembers(all).Where(m =>
                (m is MethodBase mb && Reachable(mb)) || (m is PropertyInfo p
                    && ((p.GetMethod is { } getter && Reachable(getter)) || (p.SetMethod is { } setter && Reachable(setter))))
                || m is FieldInfo { IsPublic: true }),
            m => m.Name == "CompiledFunction");

        var offenders = SignatureTypes(CompiledProgramType)
            .Where(s => typeof(Delegate).IsAssignableFrom(s) || s == typeof(JyroExecutionContext))
            .Select(s => s.FullName).Distinct().ToList();
        Assert.True(offenders.Count == 0, "CompiledProgram names: " + string.Join(", ", offenders));
    }

    [Fact(DisplayName = "JYRO-REQ-1875 CompiledProgram implements neither IEquatable of itself nor IStructuralEquatable and has only Equals(object)")]
    [Trait("requirement", "JYRO-REQ-1875")]
    public void CompiledProgramHasReferenceEqualityOnly()
    {
        var interfaces = CompiledProgramType.GetInterfaces();
        Assert.DoesNotContain(typeof(IEquatable<CompiledProgram>), interfaces);
        Assert.DoesNotContain(typeof(System.Collections.IStructuralEquatable), interfaces);
        var equalsMethods = CompiledProgramType.GetMethods(BindingFlags.Public | BindingFlags.Instance)
            .Where(m => m.Name == "Equals").ToList();
        Assert.All(equalsMethods, m =>
            Assert.Equal(new[] { typeof(object) }, m.GetParameters().Select(p => p.ParameterType).ToArray()));
    }

    [Fact(DisplayName = "JYRO-REQ-1875 two programs compiled from the same source are not Equal, and a program is Equal to itself")]
    [Trait("requirement", "JYRO-REQ-1875")]
    public void ProgramsAreEqualOnlyToThemselves()
    {
        CompiledProgram Compile() =>
            new JyroCompiler().WithStdlib().WithSource("Output.v = 1").CompileOrThrow();
        var a = Compile();
        var b = Compile();
        Assert.True(a.Equals(a));
        Assert.True(a.Equals((object)a));
        Assert.False(a.Equals(b));
        Assert.False(a.Equals((object)b));
        Assert.False(b.Equals(a));
        Assert.False(a.Equals((object?)null));
        Assert.False(a.Equals("Output.v = 1"));
    }

    [Fact(DisplayName = "JYRO-REQ-1875 a C# host cannot write new CompiledProgram(...)")]
    [Trait("requirement", "JYRO-REQ-1875")]
    public void NewCompiledProgramDoesNotCompile()
    {
        Assert.Empty(HostCompile("class Control { object M() => new JyroNumber(1); }"));

        foreach (var expression in new[]
        {
            "new CompiledProgram(null!)",
            "new CompiledProgram((Func<JyroValue, JyroValue, JyroExecutionContext, JyroValue>)null!)",
            "new CompiledProgram()",
        })
        {
            var errors = HostCompile("using System; class Forged { object M() => " + expression + "; }");
            Assert.True(errors.Count > 0, expression + " compiled");
        }
    }

    // -- JYRO-REQ-1876 --------------------------------------------------------

    private static readonly string[] SevenKinds =
        { "JyroNull", "JyroBoolean", "JyroNumber", "JyroString", "JyroArray", "JyroObject", "JyroFunction" };

    [Fact(DisplayName = "JYRO-REQ-1876 JyroValue has no public or protected constructor")]
    [Trait("requirement", "JYRO-REQ-1876")]
    public void JyroValueHasNoReachableConstructor()
    {
        var reachable = typeof(JyroValue).GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)
            .Where(c => c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly).ToList();
        Assert.True(reachable.Count == 0, "JyroValue constructors: " + string.Join("; ", reachable));
    }

    [Fact(DisplayName = "JYRO-REQ-1876 the public types deriving from JyroValue are exactly the seven kinds, each sealed")]
    [Trait("requirement", "JYRO-REQ-1876")]
    public void TheHierarchyIsClosedToSevenSealedKinds()
    {
        var derived = ExportedTypes()
            .Where(t => t != typeof(JyroValue) && typeof(JyroValue).IsAssignableFrom(t))
            .ToList();
        Assert.Equal(SevenKinds.OrderBy(n => n, StringComparer.Ordinal),
            derived.Select(t => t.Name).OrderBy(n => n, StringComparer.Ordinal));
        Assert.All(derived, t => Assert.True(t.IsSealed, t.Name + " is not sealed"));
        Assert.All(derived, t => Assert.Equal("RealTree.Jyro", t.Namespace));
    }

    [Fact(DisplayName = "JYRO-REQ-1876 no public member of JyroValue or the seven kinds is an evaluation hook")]
    [Trait("requirement", "JYRO-REQ-1876")]
    public void TheEvaluationHooksAreNotPublic()
    {
        var hooks = new[] { "EvaluateBinary", "EvaluateUnary", "CoerceToType", "GetIndex", "SetIndex", "ToIterable" };
        var types = new[] { typeof(JyroValue) }
            .Concat(ExportedTypes().Where(t => SevenKinds.Contains(t.Name) && typeof(JyroValue).IsAssignableFrom(t)))
            .ToList();
        Assert.Equal(8, types.Count);

        // The check must see public members at all, or "none is named so" proves nothing.
        var toJson = typeof(JyroValue).GetMember("ToJson", BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy);
        Assert.NotEmpty(toJson);

        const BindingFlags flags = BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.FlattenHierarchy;
        var found = types.SelectMany(t => t.GetMembers(flags).Select(m => t.Name + "." + m.Name))
            .Where(n => hooks.Contains(n.Substring(n.IndexOf('.') + 1))).ToList();
        Assert.True(found.Count == 0, "public hooks: " + string.Join(", ", found));
    }

    [Theory(DisplayName = "JYRO-REQ-1876 the operator unions are not public types")]
    [Trait("requirement", "JYRO-REQ-1876")]
    [InlineData("RealTree.Jyro.BinaryOp")]
    [InlineData("RealTree.Jyro.UnaryOp")]
    [InlineData("RealTree.Jyro.AssignOp")]
    public void TheOperatorUnionsAreNotPublic(string fullName) => AssertNotPublic(fullName);

    [Fact(DisplayName = "JYRO-REQ-1876 a C# class derived from JyroValue does not compile, and neither does one derived from a kind")]
    [Trait("requirement", "JYRO-REQ-1876")]
    public void AHostCannotDeriveFromJyroValue()
    {
        // The control: host code that uses the kinds compiles, so the errors below are about the subclass.
        Assert.Empty(HostCompile("class Control { JyroValue M() => new JyroString(\"x\"); }"));

        var direct = HostCompile("class MyValue : JyroValue { }");
        Assert.True(direct.Count > 0, "class MyValue : JyroValue compiled");
        // Unimplemented abstract members (CS0534) would fail it whatever the constructor is; the
        // constructor itself must be what refuses, so an error other than CS0534 must be there.
        Assert.True(direct.Any(d => d.Id != "CS0534"),
            "class MyValue : JyroValue fails only on its abstract members: " + string.Join("; ", direct.Select(d => d.Id)));

        var withMembers = HostCompile(
            "class MyValue : JyroValue { public override JyroValueType ValueType => JyroValueType.String; }");
        Assert.True(withMembers.Count > 0, "a JyroValue subclass that reports ValueType String compiled");

        var fromKind = HostCompile("class MyString : JyroString { public MyString() : base(\"x\") { } }");
        Assert.True(fromKind.Count > 0, "class MyString : JyroString compiled");
    }

    // -- JYRO-REQ-1877 --------------------------------------------------------

    private static bool IsFSharpSumType(Type t) =>
        t.GetCustomAttributesData().Any(a =>
            a.AttributeType.FullName == "Microsoft.FSharp.Core.CompilationMappingAttribute"
            && a.ConstructorArguments.Count > 0
            && Convert.ToInt32(a.ConstructorArguments[0].Value) == 1); // SourceConstructFlags.SumType

    private static bool RequiresQualifiedAccess(Type t) =>
        t.GetCustomAttributesData().Any(a =>
            a.AttributeType.FullName == "Microsoft.FSharp.Core.RequireQualifiedAccessAttribute");

    [Fact(DisplayName = "JYRO-REQ-1877 every public F# union type of the shipped assemblies carries RequireQualifiedAccess")]
    [Trait("requirement", "JYRO-REQ-1877")]
    public void EveryPublicUnionRequiresQualifiedAccess()
    {
        // The detector must recognize a union, and tell one with the attribute from one without.
        var fsharpCore = Assembly.Load("FSharp.Core");
        var option = fsharpCore.GetType("Microsoft.FSharp.Core.FSharpOption`1")!;
        var list = fsharpCore.GetType("Microsoft.FSharp.Collections.FSharpList`1")!;
        var choice = fsharpCore.GetType("Microsoft.FSharp.Core.FSharpChoice`2")!;
        Assert.True(IsFSharpSumType(option), "FSharpOption is not seen as a union");
        Assert.True(IsFSharpSumType(list), "FSharpList is not seen as a union");
        Assert.True(IsFSharpSumType(choice), "FSharpChoice is not seen as a union");
        Assert.False(IsFSharpSumType(typeof(string)));
        Assert.False(IsFSharpSumType(typeof(JyroNumber)));
        // FunctionEffect and ParameterType are enumerations (JYRO-REQ-1894), so no shipped type is left that
        // is both a union and an example of the attribute; FSharp.Core's ListModule carries it
        // (FSharp.Core's own FSharpResult does not, as Ok and Error are in scope after open FSharp.Core).
        var withAttribute = fsharpCore.GetType("Microsoft.FSharp.Collections.ListModule")!;
        Assert.True(RequiresQualifiedAccess(withAttribute), "ListModule requires qualified access");
        Assert.False(RequiresQualifiedAccess(option), "FSharpOption does not require qualified access");
        Assert.True(ExportedTypes().Single(t => t.Name == "FunctionEffect").IsEnum, "FunctionEffect is an enumeration");

        var offenders = ExportedTypes()
            .Where(IsFSharpSumType)
            .Where(t => !RequiresQualifiedAccess(t))
            .Select(t => t.FullName)
            .ToList();
        Assert.True(offenders.Count == 0,
            "These public unions bring their cases into scope on open RealTree.Jyro: " + string.Join(", ", offenders));
    }

    private static string DotnetFsi(string script, out string output)
    {
        var dir = Path.Combine(Path.GetTempPath(), "jyro-req-1877-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);
        try
        {
            var path = Path.Combine(dir, "probe.fsx");
            var refs = string.Join("\n", ShippedAssemblies.Select(n =>
                "#r @\"" + Assembly.Load(n).Location + "\""));
            File.WriteAllText(path, refs + "\n" + script);
            var psi = new ProcessStartInfo("dotnet", "fsi --quiet --exec \"" + path + "\"")
            {
                WorkingDirectory = dir,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
            };
            using var process = Process.Start(psi)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            if (!process.WaitForExit((int)TimeSpan.FromMinutes(4).TotalMilliseconds))
            {
                process.Kill(true);
                throw new TimeoutException("dotnet fsi did not finish");
            }
            output = stdout.Result + "\n" + stderr.Result;
            return process.ExitCode.ToString();
        }
        finally
        {
            try { Directory.Delete(dir, true); } catch (IOException) { }
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1877 after open RealTree.Jyro in an F# script, Add Not Equal And Or Concat Assign and Any are undefined")]
    [Trait("requirement", "JYRO-REQ-1877")]
    public void NoUnionCaseIsInScopeUnqualified()
    {
        var names = new[] { "Add", "Not", "Equal", "And", "Or", "Concat", "Assign", "Any" };
        // One script per name: fsi reports only the first undefined name of a script.
        foreach (var n in names)
        {
            var script = "open RealTree.Jyro\n"
                + "let control = MessageCode.DivisionByZero\n"
                + "let probe = " + n;
            var exit = DotnetFsi(script, out var output);

            // The control line must resolve, so that an undefined name below is the script's own doing.
            Assert.DoesNotContain("'MessageCode' is not defined", output);
            Assert.DoesNotContain("not defined: MessageCode", output);
            Assert.NotEqual("0", exit);
            Assert.True(output.Contains("'" + n + "' is not defined"),
                n + " is in scope after open RealTree.Jyro. fsi said:\n" + output);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1877 in an F# script that opens RealTree.Jyro, let Add x y = x + y then Add 1 2 gives 3")]
    [Trait("requirement", "JYRO-REQ-1877")]
    public void AHostCanDefineItsOwnAdd()
    {
        var exit = DotnetFsi(
            "open RealTree.Jyro\nlet Add x y = x + y\nprintfn \"result=%d\" (Add 1 2)\n", out var output);
        Assert.Equal("0", exit);
        Assert.Contains("result=3", output);
    }

    // -- Compiling host code --------------------------------------------------

    // Compiles C# written the way a host writes it, against the shipped assemblies, and gives the errors.
    private static List<Diagnostic> HostCompile(string code)
    {
        var trusted = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!).Split(Path.PathSeparator);
        // The shipped assemblies are added once below, from where the tests loaded them.
        var references = trusted
            .Where(p => !Path.GetFileName(p).StartsWith("Jyro.", StringComparison.Ordinal))
            .Select(p => MetadataReference.CreateFromFile(p))
            .ToList();
        references.AddRange(ShippedAssemblies.Select(n => MetadataReference.CreateFromFile(Assembly.Load(n).Location)));
        var tree = CSharpSyntaxTree.ParseText("using RealTree.Jyro;\n" + code);
        var compilation = CSharpCompilation.Create(
            "HostCode" + Guid.NewGuid().ToString("N"),
            new[] { tree },
            references,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, nullableContextOptions: NullableContextOptions.Enable));
        return compilation.GetDiagnostics()
            .Where(d => d.Severity == DiagnosticSeverity.Error)
            .ToList();
    }
}

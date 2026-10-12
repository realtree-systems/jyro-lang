using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// JYRO-ADR-202: where the public types live, trimming metadata, strong names and friend assemblies.
public class PackagingLayoutTests
{
    private static readonly string[] ShippedAssemblies =
        { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static IEnumerable<Assembly> Shipped() => ShippedAssemblies.Select(Assembly.Load);

    // -- JYRO-REQ-1922 --------------------------------------------------------

    [Theory(DisplayName = "JYRO-REQ-1922 the assembly exports no public type")]
    [Trait("requirement", "JYRO-REQ-1922")]
    [InlineData("Jyro.Parser")]
    [InlineData("Jyro.Compiler")]
    [InlineData("Jyro.Stdlib")]
    public void TheInnerAssembliesExportNoType(string name)
    {
        // The assembly is loaded, so an empty answer is about this assembly and not about a missing one.
        Assert.Equal(name, Assembly.Load(name).GetName().Name);
        Assert.Empty(Assembly.Load(name).GetExportedTypes());
    }

    [Fact(DisplayName = "JYRO-REQ-1922 every exported type of the five assemblies is declared in Jyro.Core or Jyro.Api")]
    [Trait("requirement", "JYRO-REQ-1922")]
    public void EveryPublicTypeIsInCoreOrApi()
    {
        var exported = Shipped().SelectMany(a => a.GetExportedTypes()).ToList();
        Assert.True(exported.Count >= 40, "the walk saw only " + exported.Count + " exported types");
        var offenders = exported
            .Where(t => t.Assembly.GetName().Name is not ("Jyro.Core" or "Jyro.Api"))
            .Select(t => t.FullName + " in " + t.Assembly.GetName().Name)
            .ToList();
        Assert.True(offenders.Count == 0, string.Join(", ", offenders));
    }

    [Fact(DisplayName = "JYRO-REQ-1922 CompiledProgram and StdlibRegistry are declared in Jyro.Core or Jyro.Api")]
    [Trait("requirement", "JYRO-REQ-1922")]
    public void CompiledProgramAndStdlibRegistryAreInCoreOrApi()
    {
        Assert.Contains(typeof(CompiledProgram).Assembly.GetName().Name, new[] { "Jyro.Core", "Jyro.Api" });
        Assert.Contains(typeof(StdlibRegistry).Assembly.GetName().Name, new[] { "Jyro.Core", "Jyro.Api" });
    }

    [Fact(DisplayName = "JYRO-REQ-1922 CompiledProgram is declared in Jyro.Core and StdlibRegistry in Jyro.Api (JYRO-ADR-202 item 1)")]
    [Trait("requirement", "JYRO-REQ-1922")]
    public void TheTwoTypesAreWhereTheDecisionPutsThem()
    {
        Assert.Equal("Jyro.Core", typeof(CompiledProgram).Assembly.GetName().Name);
        Assert.Equal("Jyro.Api", typeof(StdlibRegistry).Assembly.GetName().Name);
    }

    // -- JYRO-REQ-1924 (the metadata clause; the trimmed publish is a demonstration) ----

    [Theory(DisplayName = "JYRO-REQ-1924 the assembly declares AssemblyMetadata IsTrimmable True")]
    [Trait("requirement", "JYRO-REQ-1924")]
    [InlineData("Jyro.Core")]
    [InlineData("Jyro.Parser")]
    [InlineData("Jyro.Compiler")]
    [InlineData("Jyro.Stdlib")]
    [InlineData("Jyro.Api")]
    public void EveryAssemblyIsTrimmable(string name)
    {
        var values = Assembly.Load(name).GetCustomAttributes<AssemblyMetadataAttribute>()
            .Where(a => a.Key == "IsTrimmable")
            .Select(a => a.Value)
            .ToList();
        Assert.Equal(new string?[] { "True" }, values);
    }

    // -- JYRO-REQ-1925 --------------------------------------------------------

    [Theory(DisplayName = "JYRO-REQ-1925 the assembly is strong-named: its public key is not empty")]
    [Trait("requirement", "JYRO-REQ-1925")]
    [InlineData("Jyro.Core")]
    [InlineData("Jyro.Parser")]
    [InlineData("Jyro.Compiler")]
    [InlineData("Jyro.Stdlib")]
    [InlineData("Jyro.Api")]
    public void EveryAssemblyIsSigned(string name)
    {
        var key = Assembly.Load(name).GetName().GetPublicKey();
        Assert.NotNull(key);
        Assert.NotEmpty(key!);
    }

    private static List<string> FriendDeclarations() =>
        Shipped()
            .SelectMany(a => a.GetCustomAttributes<InternalsVisibleToAttribute>()
                .Select(v => a.GetName().Name + " -> " + v.AssemblyName))
            .ToList();

    [Fact(DisplayName = "JYRO-REQ-1925 every InternalsVisibleTo of the five assemblies names its friend with PublicKey=")]
    [Trait("requirement", "JYRO-REQ-1925")]
    public void EveryFriendIsNamedWithAPublicKey()
    {
        var declarations = FriendDeclarations();
        var offenders = declarations.Where(d => !d.Contains("PublicKey=", StringComparison.Ordinal)).ToList();
        Assert.True(offenders.Count == 0, "Friend names without a public key: " + string.Join("; ", offenders));
    }

    private static string AnInternalTypeName()
    {
        var name = Assembly.Load("Jyro.Core").GetTypes()
            .Where(t => t.IsNotPublic && !t.IsNested && t.Namespace == "RealTree.Jyro" && !t.IsGenericType
                        && t.Name.All(char.IsLetterOrDigit))
            .Select(t => t.Name)
            .OrderBy(n => n, StringComparer.Ordinal)
            .FirstOrDefault();
        Assert.NotNull(name);
        return name!;
    }

    private static List<Diagnostic> CompileAs(string assemblyName, string code)
    {
        var trusted = ((string)AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES")!).Split(Path.PathSeparator);
        var references = trusted
            .Where(p => !Path.GetFileName(p).StartsWith("Jyro.", StringComparison.Ordinal))
            .Select(p => (MetadataReference)MetadataReference.CreateFromFile(p))
            .ToList();
        references.AddRange(ShippedAssemblies.Select(n => MetadataReference.CreateFromFile(Assembly.Load(n).Location)));
        var compilation = CSharpCompilation.Create(
            assemblyName,
            new[] { CSharpSyntaxTree.ParseText(code) },
            references,
            new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary, nullableContextOptions: NullableContextOptions.Enable));
        return compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToList();
    }

    public static IEnumerable<object[]> FriendNames()
    {
        var declared = Assembly.Load("Jyro.Core").GetCustomAttributes<InternalsVisibleToAttribute>()
            .Concat(Assembly.Load("Jyro.Api").GetCustomAttributes<InternalsVisibleToAttribute>())
            .Concat(Assembly.Load("Jyro.Compiler").GetCustomAttributes<InternalsVisibleToAttribute>())
            .Concat(Assembly.Load("Jyro.Stdlib").GetCustomAttributes<InternalsVisibleToAttribute>())
            .Concat(Assembly.Load("Jyro.Parser").GetCustomAttributes<InternalsVisibleToAttribute>())
            .Select(a => a.AssemblyName.Split(',')[0].Trim());
        var names = new[] { "Jyro.Tests", "Jyro.CSharp.Tests", "Jyro.Benchmarks", "Jyro.Fuzz" }
            .Concat(declared)
            .Distinct(StringComparer.Ordinal);
        foreach (var n in names) yield return new object[] { n };
    }

    [Theory(DisplayName = "JYRO-REQ-1925 an unsigned assembly with a friend name cannot use an internal type of the package")]
    [Trait("requirement", "JYRO-REQ-1925")]
    [MemberData(nameof(FriendNames))]
    public void AnUnsignedAssemblyWithAFriendNameSeesNoInternals(string friendName)
    {
        // The control: with the same name, host code that stays on the public surface compiles.
        var control = CompileAs(friendName, "using RealTree.Jyro; class Control { object M() => new JyroNumber(1); }");
        Assert.Empty(control);

        var internalType = AnInternalTypeName();
        var errors = CompileAs(friendName,
            "class Probe { System.Type M() => typeof(RealTree.Jyro." + internalType + "); }");
        Assert.NotEmpty(errors);
        Assert.All(errors, e => Assert.Contains(e.Id, new[] { "CS0122", "CS0281" }));
    }
}

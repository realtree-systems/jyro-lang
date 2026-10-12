using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml.Linq;
using Xunit;

// The two READMEs name the package and the public namespace the project file
// gives: PackageId and RootNamespace in src/Jyro.Api/Jyro.Api.fsproj.
//
// Commit 2fa4d3c renamed both to RealTree.Jyro by a rename that moved
// declarations and imports and skipped prose, so the READMEs still told a host
// to install and import "Jyro" (JYRO-DEF-793, JYRO-DEF-794). This pins the
// prose to the project file, so the next rename of either fails here.
//
// The names are read from the project file at run time, not written into this
// file, so the test follows a rename and does not need editing for one.

namespace JyroHostSurface;

public class ReadmeIdentityTests
{
    private static string RepositoryRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir is not null && !File.Exists(Path.Combine(dir.FullName, "Jyro.slnx")))
            dir = dir.Parent;
        Assert.True(dir is not null, "Jyro.slnx was not found above " + AppContext.BaseDirectory);
        return dir!.FullName;
    }

    private static string ProjectProperty(string name)
    {
        var path = Path.Combine(RepositoryRoot(), "src", "Jyro.Api", "Jyro.Api.fsproj");
        var value = XDocument.Load(path).Descendants(name).FirstOrDefault()?.Value;
        Assert.False(string.IsNullOrWhiteSpace(value), name + " is not set in " + path);
        return value!.Trim();
    }

    private static string Readme(string relativePath) =>
        File.ReadAllText(Path.Combine(RepositoryRoot(), relativePath));

    // Each pattern finds the bare name Jyro (not RealTree.Jyro, not Jyro.Api)
    // where a README uses it as the package or as the namespace.
    private static readonly (string Use, Regex Pattern)[] BareNameUses =
    {
        ("a NuGet package link to packages/Jyro", new Regex(@"nuget\.org/packages/Jyro(?![\w.])", RegexOptions.IgnoreCase)),
        ("a link text [Jyro]", new Regex(@"\[Jyro\]")),
        ("a backticked `Jyro`", new Regex(@"`Jyro`")),
        ("\"Jyro NuGet package\"", new Regex(@"(?<![\w.])Jyro (NuGet )?package", RegexOptions.IgnoreCase)),
        ("\"Jyro namespace\"", new Regex(@"(?<![\w.])Jyro namespace", RegexOptions.IgnoreCase)),
        ("\"namespace Jyro\"", new Regex(@"namespace `?Jyro(?![\w.])", RegexOptions.IgnoreCase)),
        ("a using directive for Jyro", new Regex(@"using Jyro\s*;")),
        ("a PackageReference to Jyro", new Regex(@"Include=""Jyro""")),
        ("dotnet add package Jyro", new Regex(@"add package Jyro(?![\w.])")),
        ("Install-Package Jyro", new Regex(@"Install-Package Jyro(?![\w.])")),
    };

    [Theory(DisplayName = "JYRO-DEF-793 JYRO-DEF-794 a README does not name the package or namespace as bare Jyro")]
    [InlineData("README.md")]
    [InlineData("src/Jyro.Api/README.md")]
    public void JYRO_DEF_793_and_794_ReadmeDoesNotNameThePackageOrNamespaceAsBareJyro(string readme)
    {
        var text = Readme(readme);
        var packageId = ProjectProperty("PackageId");
        var rootNamespace = ProjectProperty("RootNamespace");
        // Guard: the patterns above are only right while the project file's
        // names differ from the bare name.
        Assert.NotEqual("Jyro", packageId);
        Assert.NotEqual("Jyro", rootNamespace);

        foreach (var (use, pattern) in BareNameUses)
        {
            var m = pattern.Match(text);
            var line = m.Success ? text.Take(m.Index).Count(c => c == '\n') + 1 : 0;
            Assert.False(m.Success,
                $"{readme} line {line} has {use}; the project file gives PackageId {packageId} and RootNamespace {rootNamespace}.");
        }
    }

    [Theory(DisplayName = "JYRO-DEF-793 JYRO-DEF-794 a README names the PackageId and the RootNamespace")]
    [InlineData("README.md")]
    [InlineData("src/Jyro.Api/README.md")]
    public void JYRO_DEF_793_and_794_ReadmeNamesThePackageIdAndTheRootNamespace(string readme)
    {
        var text = Readme(readme);
        Assert.Contains(ProjectProperty("PackageId"), text);
        Assert.Contains(ProjectProperty("RootNamespace"), text);
    }
}

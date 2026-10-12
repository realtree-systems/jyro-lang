using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml.Linq;
using RealTree.Jyro;
using Xunit;

// Every standard library function is documented for script authors in the
// Jyro.Stdlib.xml that the RealTree.Jyro package ships.
//
// The file is read from the packed .nupkg, not from the build output, because
// the package is what a consumer (the VS Code extension) receives. Each
// function StdlibRegistry gives must have a member whose id is "T:" plus the
// full name of the class of which the function is an instance, with each '+'
// read as '.'.
//
// Only what a scan can decide is tested here. The prose rules (a first
// sentence that stands alone, script-author voice, when a null is returned,
// what an omitted optional argument does, lambda shapes, accepted strings,
// stated effects) are checked by inspection of the XML diff.

namespace JyroHostSurface;

public class StdlibDocumentationTests
{
    private static readonly Lazy<XDocument> Packed = new(PackAndReadStdlibXml);

    private static string RepositoryRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir is not null && !File.Exists(Path.Combine(dir.FullName, "Jyro.slnx")))
            dir = dir.Parent;
        Assert.True(dir is not null, "Jyro.slnx was not found above " + AppContext.BaseDirectory);
        return dir!.FullName;
    }

    private static XDocument PackAndReadStdlibXml()
    {
        var root = RepositoryRoot();
        var output = Path.Combine(Path.GetTempPath(), "jyro-req-2179-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(output);

        var info = new ProcessStartInfo("dotnet")
        {
            WorkingDirectory = root,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        info.ArgumentList.Add("pack");
        info.ArgumentList.Add(Path.Combine(root, "src", "Jyro.Api", "Jyro.Api.fsproj"));
        info.ArgumentList.Add("--output");
        info.ArgumentList.Add(output);
        info.ArgumentList.Add("--nologo");
        // Build servers and reusable nodes outlive the command and hold its
        // output pipes open, so reading the output to its end would never return.
        info.ArgumentList.Add("-nodeReuse:false");
        info.ArgumentList.Add("--disable-build-servers");

        using var process = Process.Start(info)!;
        var stdout = new System.Text.StringBuilder();
        process.OutputDataReceived += (_, e) => { lock (stdout) stdout.AppendLine(e.Data); };
        process.ErrorDataReceived += (_, e) => { lock (stdout) stdout.AppendLine(e.Data); };
        process.BeginOutputReadLine();
        process.BeginErrorReadLine();
        Assert.True(process.WaitForExit(900_000), "dotnet pack did not finish in fifteen minutes");
        string log;
        lock (stdout) log = stdout.ToString();
        Assert.True(process.ExitCode == 0, "dotnet pack failed:\n" + log);

        var package = Directory.GetFiles(output, "*.nupkg").FirstOrDefault(p => !p.EndsWith(".symbols.nupkg", StringComparison.Ordinal));
        Assert.True(package is not null, "dotnet pack produced no .nupkg in " + output);

        using var archive = ZipFile.OpenRead(package!);
        var entry = archive.Entries.FirstOrDefault(e => e.Name == "Jyro.Stdlib.xml");
        Assert.True(entry is not null, "the packed RealTree.Jyro .nupkg holds no Jyro.Stdlib.xml");
        using var stream = entry!.Open();
        return XDocument.Load(stream);
    }

    private static string DocumentationId(IJyroFunction function) =>
        "T:" + function.GetType().FullName!.Replace('+', '.');

    private static IReadOnlyDictionary<string, XElement> Members()
    {
        var members = new Dictionary<string, XElement>(StringComparer.Ordinal);
        foreach (var member in Packed.Value.Descendants("member"))
        {
            var name = (string?)member.Attribute("name");
            if (name is not null) members[name] = member;
        }
        return members;
    }

    public static IEnumerable<object[]> Functions() =>
        StdlibRegistry.GetAll().Select(f => new object[] { f.Name });

    private static IJyroFunction Function(string name)
    {
        Assert.True(StdlibRegistry.TryGetFunction(name, out var function), name + " is not in StdlibRegistry");
        return function;
    }

    private static string Text(XElement? element) =>
        element is null ? "" : element.Value.Trim();

    [Theory(DisplayName = "JYRO-REQ-2179 a stdlib function has a member keyed by its class full name")]
    [Trait("requirement", "JYRO-REQ-2179")]
    [MemberData(nameof(Functions))]
    public void EveryFunctionHasAMember(string name)
    {
        var id = DocumentationId(Function(name));
        Assert.True(Members().ContainsKey(id), $"Jyro.Stdlib.xml holds no member {id} for {name}");
    }

    [Theory(DisplayName = "JYRO-REQ-2179 a stdlib function has a non-empty summary and a non-empty returns")]
    [Trait("requirement", "JYRO-REQ-2179")]
    [MemberData(nameof(Functions))]
    public void EveryFunctionHasSummaryAndReturns(string name)
    {
        var id = DocumentationId(Function(name));
        Assert.True(Members().TryGetValue(id, out var member), $"no member {id}");
        Assert.False(string.IsNullOrWhiteSpace(Text(member!.Element("summary"))), $"{id} has no summary text");
        Assert.False(string.IsNullOrWhiteSpace(Text(member.Element("returns"))), $"{id} has no returns text");
    }

    [Theory(DisplayName = "JYRO-REQ-2179 the arg names of a stdlib function equal its signature parameter names in order")]
    [Trait("requirement", "JYRO-REQ-2179")]
    [MemberData(nameof(Functions))]
    public void ArgNamesEqualSignatureParameterNames(string name)
    {
        var function = Function(name);
        var id = DocumentationId(function);
        Assert.True(Members().TryGetValue(id, out var member), $"no member {id}");

        var expected = function.Signature.Parameters.Select(p => p.Name).ToArray();
        var actual = member!.Elements("arg").Select(a => (string?)a.Attribute("name")).ToArray();

        Assert.Equal(expected, actual);
        Assert.Empty(member.Elements("param"));
    }

    private static readonly Regex[] Forbidden =
    {
        new(@"\b[A-Z][A-Z0-9]*-(REQ|ADR|DEF|NED|WRK|MIL)-\d+", RegexOptions.None),
        new(@"\b(REQ|ADR|DEF)-\d+", RegexOptions.None),
        new(@"\bclauses?\s+\d", RegexOptions.IgnoreCase),
        new(@"§\s*\d"),
    };

    [Theory(DisplayName = "JYRO-REQ-2179 the text of a stdlib function names no requirement, decision, defect or clause")]
    [Trait("requirement", "JYRO-REQ-2179")]
    [MemberData(nameof(Functions))]
    public void TextNamesNoSpecificationIdentifier(string name)
    {
        var id = DocumentationId(Function(name));
        Assert.True(Members().TryGetValue(id, out var member), $"no member {id}");

        foreach (var element in member!.Elements().Where(e => e.Name.LocalName is "summary" or "returns" or "arg" or "remarks" or "example"))
        {
            var text = element.Value;
            foreach (var pattern in Forbidden)
            {
                var match = pattern.Match(text);
                Assert.False(match.Success, $"{id} <{element.Name.LocalName}> names '{match.Value}'");
            }
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2179 Replace is documented under StringFunctions.ReplaceFunction with args text, oldValue, newValue")]
    [Trait("requirement", "JYRO-REQ-2179")]
    public void ReplaceIsDocumentedAsTheWorkedExample()
    {
        Assert.Equal("RealTree.Jyro.StringFunctions+ReplaceFunction", Function("Replace").GetType().FullName);

        Assert.True(Members().TryGetValue("T:RealTree.Jyro.StringFunctions.ReplaceFunction", out var member),
            "no member T:RealTree.Jyro.StringFunctions.ReplaceFunction");
        var args = member!.Elements("arg").Select(a => (string?)a.Attribute("name")).ToArray();
        Assert.Equal(new[] { "text", "oldValue", "newValue" }, args);
        Assert.Empty(member.Elements("param"));
    }
}

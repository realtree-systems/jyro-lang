using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.Json.Nodes;
using System.Xml.Linq;
using RealTree.Jyro;
using Xunit;

namespace JyroHostSurface;

// The packed RealTree.Jyro package, made once by `dotnet pack -c Release` as the pack workflow makes it, and read
// as a host's NuGet client would.
internal sealed class PackedPackage
{
    public static readonly string[] Assemblies = { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    public static readonly Lazy<PackedPackage> Instance = new(() => Make(null), isThreadSafe: true);

    // The same pack with --artifacts-path outside the repository, so that the obj folder is outside it too.
    public static readonly Lazy<PackedPackage> OutsideRepository = new(() => Make(
        Path.Combine(Path.GetTempPath(), "jyro-artifacts-" + Guid.NewGuid().ToString("N"))), isThreadSafe: true);

    public string RepositoryRoot { get; private set; } = "";
    public string? ArtifactsPath { get; private set; }
    public string OutputDirectory { get; private set; } = "";
    public string Nupkg { get; private set; } = "";
    public string? Snupkg { get; private set; }
    public string Commit { get; private set; } = "";

    private static string RootFrom([CallerFilePath] string thisFile = "") =>
        Path.GetFullPath(Path.Combine(Path.GetDirectoryName(thisFile)!, "..", ".."));

    private static string Run(string fileName, string arguments, string workingDirectory, TimeSpan limit)
    {
        var psi = new ProcessStartInfo(fileName, arguments)
        {
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        if (!process.WaitForExit((int)limit.TotalMilliseconds))
        {
            process.Kill(true);
            throw new TimeoutException(fileName + " " + arguments + " did not finish");
        }
        var output = stdout.Result + "\n" + stderr.Result;
        if (process.ExitCode != 0)
            throw new InvalidOperationException(fileName + " " + arguments + " exited " + process.ExitCode + ":\n" + output);
        return output;
    }

    private static PackedPackage Make(string? artifactsPath)
    {
        var root = RootFrom();
        var output = Path.Combine(Path.GetTempPath(), "jyro-packed-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(output);
        var commit = Run("git", "rev-parse HEAD", root, TimeSpan.FromMinutes(1)).Split('\n')[0].Trim();
        var artifacts = artifactsPath is null ? "" : " --artifacts-path \"" + artifactsPath + "\"";
        Run("dotnet", "pack \"" + Path.Combine(root, "src", "Jyro.Api", "Jyro.Api.fsproj") + "\" -c Release -o \"" + output + "\"" + artifacts,
            root, TimeSpan.FromMinutes(9));
        var nupkg = Directory.GetFiles(output, "RealTree.Jyro.*.nupkg").Single();
        var snupkgs = Directory.GetFiles(output, "RealTree.Jyro.*.snupkg");
        return new PackedPackage
        {
            RepositoryRoot = root,
            ArtifactsPath = artifactsPath,
            OutputDirectory = output,
            Nupkg = nupkg,
            Snupkg = snupkgs.Length == 1 ? snupkgs[0] : null,
            Commit = commit,
        };
    }

    public static ZipArchive Open(string path) => ZipFile.OpenRead(path);

    public static byte[] ReadAll(ZipArchiveEntry entry)
    {
        using var s = entry.Open();
        using var m = new MemoryStream();
        s.CopyTo(m);
        return m.ToArray();
    }

    public XDocument Nuspec()
    {
        using var zip = Open(Nupkg);
        var entry = zip.Entries.Single(e => e.FullName.EndsWith(".nuspec", StringComparison.Ordinal) && !e.FullName.Contains('/'));
        using var s = entry.Open();
        return XDocument.Load(s);
    }
}

public class PackedPackageTests
{
    private static PackedPackage Package => PackedPackage.Instance.Value;

    private static IEnumerable<ZipArchiveEntry> LibEntries(ZipArchive zip, string extension) =>
        zip.Entries.Where(e => e.FullName.StartsWith("lib/", StringComparison.Ordinal)
                               && e.FullName.EndsWith(extension, StringComparison.Ordinal));

    // -- JYRO-REQ-2182 (checked here with the SDK that packs for the tests; packing with the other SDK is an inspection) --------

    [Fact(DisplayName = "JYRO-REQ-2182 the packed nuspec declares FSharp.Core with the minimum version 10.1.302 in every dependency group")]
    [Trait("requirement", "JYRO-REQ-2182")]
    public void TheNuspecStatesTheFSharpCoreFloor()
    {
        var nuspec = Package.Nuspec();
        var groups = nuspec.Descendants().Where(e => e.Name.LocalName == "group" && e.Parent?.Name.LocalName == "dependencies").ToList();
        Assert.NotEmpty(groups);
        foreach (var group in groups)
        {
            var dependency = group.Elements().Single(e => e.Name.LocalName == "dependency" && (string?)e.Attribute("id") == "FSharp.Core");
            Assert.Equal("10.1.302", (string?)dependency.Attribute("version"));
        }
    }

    // -- JYRO-REQ-1926 ---------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1926 dotnet pack produces the .snupkg beside the .nupkg")]
    [Trait("requirement", "JYRO-REQ-1926")]
    public void ThePackProducesASymbolPackage()
    {
        Assert.NotNull(Package.Snupkg);
        Assert.Equal(Path.ChangeExtension(Package.Nupkg, null), Path.ChangeExtension(Package.Snupkg!, null));
    }

    [Fact(DisplayName = "JYRO-REQ-1926 the .nupkg ships the five assemblies and the .snupkg holds a portable PDB for each")]
    [Trait("requirement", "JYRO-REQ-1926")]
    public void TheSymbolPackageHoldsAPortablePdbForEachAssembly()
    {
        using (var nupkg = PackedPackage.Open(Package.Nupkg))
        {
            var dlls = LibEntries(nupkg, ".dll").Select(e => Path.GetFileNameWithoutExtension(e.FullName)).OrderBy(n => n).ToList();
            Assert.Equal(PackedPackage.Assemblies.OrderBy(n => n), dlls);
        }

        Assert.NotNull(Package.Snupkg);
        using var snupkg = PackedPackage.Open(Package.Snupkg!);
        var pdbs = LibEntries(snupkg, ".pdb").ToList();
        Assert.Equal(PackedPackage.Assemblies.OrderBy(n => n),
            pdbs.Select(e => Path.GetFileNameWithoutExtension(e.FullName)).OrderBy(n => n));
        foreach (var pdb in pdbs)
            Assert.Equal("BSJB", Encoding.ASCII.GetString(PackedPackage.ReadAll(pdb), 0, 4));
    }

    [Fact(DisplayName = "JYRO-REQ-1926 the nuspec names an icon file and the .nupkg holds a file at that path")]
    [Trait("requirement", "JYRO-REQ-1926")]
    public void TheNuspecNamesAnIconThePackageHolds()
    {
        var icon = Package.Nuspec().Descendants().SingleOrDefault(e => e.Name.LocalName == "icon")?.Value.Trim();
        Assert.False(string.IsNullOrEmpty(icon), "the nuspec has no icon element");
        using var zip = PackedPackage.Open(Package.Nupkg);
        var entry = zip.Entries.SingleOrDefault(e => e.FullName == icon!.Replace('\\', '/'));
        Assert.NotNull(entry);
        Assert.True(entry!.Length > 0, "the icon file is empty");
    }

    private static readonly Guid SourceLinkKind = new("CC110556-A091-4D38-9FEC-25AB9A351A6A");

    private static readonly Guid EmbeddedSourceKind = new("0E8A571B-6926-466E-B4AD-8AB04611F5FE");

    private static bool IsLocalPath(PackedPackage pkg, string p) =>
        (p.Length >= 2 && char.IsLetter(p[0]) && p[1] == ':')
        || p.StartsWith("\\\\", StringComparison.Ordinal)
        || p.Contains(pkg.RepositoryRoot, StringComparison.OrdinalIgnoreCase)
        || (pkg.ArtifactsPath is not null && p.Contains(pkg.ArtifactsPath, StringComparison.OrdinalIgnoreCase))
        || p.Contains(Path.GetTempPath().TrimEnd('\\', '/'), StringComparison.OrdinalIgnoreCase)
        || p.StartsWith("/home/", StringComparison.Ordinal) || p.StartsWith("/Users/", StringComparison.Ordinal);

    // The files the private repository holds at the packed commit, fetched with the repository's credentials (gh).
    private static readonly Lazy<HashSet<string>> RepositoryFilesLazy = new(() =>
    {
        var psi = new ProcessStartInfo("gh", "api \"repos/realtree-systems/jyro-lang/git/trees/" + Package.Commit + "?recursive=1\" --jq \".tree[].path\"")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        process.WaitForExit();
        if (process.ExitCode != 0)
            throw new InvalidOperationException("gh api could not list the repository at " + Package.Commit + " (is the commit pushed and gh signed in?): " + stderr.Result);
        return stdout.Result.Split('\n', StringSplitOptions.RemoveEmptyEntries).Select(s => s.Trim()).ToHashSet(StringComparer.Ordinal);
    });

    private static HashSet<string> RepositoryFiles => RepositoryFilesLazy.Value;

    [Fact(DisplayName = "JYRO-REQ-1926 every source document of every PDB is embedded or maps to a URL of realtree-systems/jyro-lang at the packed commit that the repository holds, and none is a local path")]
    [Trait("requirement", "JYRO-REQ-1926")]
    public void EverySourceDocumentIsMapped() => CheckEverySourceDocument(Package);

    [Fact(DisplayName = "JYRO-REQ-1926 with the artifacts path outside the repository, every source document of every PDB is embedded or maps to a URL of realtree-systems/jyro-lang at the packed commit that the repository holds, and none is a local path")]
    [Trait("requirement", "JYRO-REQ-1926")]
    public void EverySourceDocumentIsMappedWithArtifactsOutsideTheRepository()
    {
        Assert.NotNull(PackedPackage.OutsideRepository.Value.ArtifactsPath);
        CheckEverySourceDocument(PackedPackage.OutsideRepository.Value);
    }

    // Each path and URL is compared exactly: no trimming and no separator normalizing, so a "//" fails.
    private static void CheckEverySourceDocument(PackedPackage pkg)
    {
        const string Root = "/_/";
        var commitSegment = "/" + pkg.Commit + "/";
        Assert.NotNull(pkg.Snupkg);
        using var snupkg = PackedPackage.Open(pkg.Snupkg!);
        var pdbs = LibEntries(snupkg, ".pdb").ToList();
        Assert.Equal(5, pdbs.Count);
        foreach (var pdb in pdbs)
        {
            var bytes = PackedPackage.ReadAll(pdb);
            {
                {
                    using var provider = MetadataReaderProvider.FromPortablePdbStream(new MemoryStream(bytes));
                    var reader = provider.GetMetadataReader();

                    byte[]? json = null;
                    foreach (var handle in reader.GetCustomDebugInformation(EntityHandle.ModuleDefinition))
                    {
                        var info = reader.GetCustomDebugInformation(handle);
                        if (reader.GetGuid(info.Kind) == SourceLinkKind)
                            json = reader.GetBlobBytes(info.Value);
                    }
                    Assert.True(json is not null, pdb.FullName + " carries no SourceLink information");

                    var map = JsonNode.Parse(Encoding.UTF8.GetString(json!))!["documents"]!.AsObject()
                        .ToDictionary(kv => kv.Key, kv => kv.Value!.GetValue<string>());
                    foreach (var kv in map)
                    {
                        Assert.Contains("realtree-systems/jyro-lang", kv.Value, StringComparison.OrdinalIgnoreCase);
                        Assert.Contains(commitSegment, kv.Value, StringComparison.Ordinal);
                        Assert.False(IsLocalPath(pkg, kv.Key), pdb.FullName + " maps a local build path: " + kv.Key);
                    }

                    var unmapped = new List<string>();
                    var notFound = new List<string>();
                    foreach (var h in reader.Documents)
                    {
                        var name = reader.GetString(reader.GetDocument(h).Name);
                        Assert.False(IsLocalPath(pkg, name), pdb.FullName + " carries a local build path: " + name);
                        var embedded = reader.GetCustomDebugInformation(h)
                            .Select(reader.GetCustomDebugInformation)
                            .Any(i => reader.GetGuid(i.Kind) == EmbeddedSourceKind);
                        if (embedded) continue;

                        // A document that is not embedded sits exactly under /_/src/ (one separator, no empty segment).
                        if (!name.StartsWith(Root + "src/", StringComparison.Ordinal) || name.Length == (Root + "src/").Length || name[(Root + "src/").Length] == '/')
                        { unmapped.Add(name); continue; }

                        var pattern = map.Keys.FirstOrDefault(p => p.EndsWith("*", StringComparison.Ordinal)
                            ? name.StartsWith(p.Substring(0, p.Length - 1), StringComparison.Ordinal)
                            : string.Equals(name, p, StringComparison.Ordinal));
                        if (pattern is null) { unmapped.Add(name); continue; }

                        var url = map[pattern];
                        if (pattern.EndsWith("*", StringComparison.Ordinal))
                            url = url.Replace("*", name.Substring(pattern.Length - 1));
                        var at = url.IndexOf(commitSegment, StringComparison.Ordinal);
                        if (at < 0) { notFound.Add(url); continue; }
                        var path = Uri.UnescapeDataString(url.Substring(at + commitSegment.Length));
                        if (!path.StartsWith("src/", StringComparison.Ordinal) || !RepositoryFiles.Contains(path)) notFound.Add(url);
                    }
                    Assert.True(unmapped.Count == 0, pdb.FullName + " has documents neither embedded nor mapped: " + string.Join(", ", unmapped.Take(10)));
                    Assert.True(notFound.Count == 0, pdb.FullName + " maps files the repository does not hold at the packed commit: " + string.Join(", ", notFound.Take(10)));
                }
            }
        }
    }

    // -- JYRO-REQ-2180 ---------------------------------------------------------

    private static XmlDocScanner Scanner() =>
        new(PackedPackage.Assemblies.Select(Assembly.Load));

    // JYRO-ADR-208: the classes of which a StdlibRegistry function is an instance.
    private static HashSet<string> StdlibFunctionClassIds() =>
        StdlibRegistry.GetAll().Select(f => "T:" + f.GetType().FullName!.Replace('+', '.')).ToHashSet(StringComparer.Ordinal);

    [Fact(DisplayName = "JYRO-REQ-2180 no XML documentation file is shipped for an assembly that exports no public type, other than Jyro.Stdlib.xml")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void NoXmlForAnAssemblyWithoutPublicTypes()
    {
        using var zip = PackedPackage.Open(Package.Nupkg);
        var xmls = LibEntries(zip, ".xml").ToList();
        Assert.Contains(xmls, e => e.Name == "Jyro.Core.xml");
        Assert.Contains(xmls, e => e.Name == "Jyro.Api.xml");
        Assert.DoesNotContain(xmls, e => e.Name == "Jyro.Parser.xml");
        Assert.DoesNotContain(xmls, e => e.Name == "Jyro.Compiler.xml");
        foreach (var xml in xmls)
        {
            var name = Path.GetFileNameWithoutExtension(xml.Name);
            // JYRO-ADR-208 and JYRO-REQ-2179: the standard library function-class blocks ship in Jyro.Stdlib.xml.
            if (name == "Jyro.Stdlib") continue;
            var assembly = Assembly.Load(name);
            Assert.True(assembly.GetExportedTypes().Length > 0, xml.FullName + " documents " + assembly.GetName().Name + ", which exports no public type");
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2180 a scan of every XML file of the package finds no identifier, clause number, backtick, capital word, non-public member, name the package lacks or former name")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void TheShippedDocumentationIsClean()
    {
        using var zip = PackedPackage.Open(Package.Nupkg);
        var scanner = Scanner();
        var exempt = StdlibFunctionClassIds();
        Assert.True(exempt.Count >= 90, "the standard library function classes were not found: " + exempt.Count);

        var problems = new List<string>();
        var members = 0;
        foreach (var xml in LibEntries(zip, ".xml"))
        {
            using var s = xml.Open();
            var doc = XDocument.Load(s);
            members += doc.Descendants("member").Count();
            problems.AddRange(scanner.Scan(doc, exempt).Select(p => xml.Name + ": " + p));
        }
        Assert.True(members > 200, "the scan saw only " + members + " documented members");
        Assert.True(problems.Count == 0,
            problems.Count + " problems, for example:\n" + string.Join("\n", problems.Distinct().Take(120)));
    }

    [Fact(DisplayName = "JYRO-REQ-2180 the documentation of JyroValue.Freeze does not name JyroResult<JyroValue>.Value")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void FreezeDoesNotNameTheResultValue()
    {
        using var zip = PackedPackage.Open(Package.Nupkg);
        var core = zip.Entries.Single(e => e.FullName.StartsWith("lib/", StringComparison.Ordinal) && e.Name == "Jyro.Core.xml");
        using var s = core.Open();
        var doc = XDocument.Load(s);
        var freeze = doc.Descendants("member")
            .Where(m => ((string?)m.Attribute("name") ?? "").StartsWith("M:RealTree.Jyro.JyroValue.Freeze", StringComparison.Ordinal))
            .ToList();
        Assert.NotEmpty(freeze);
        foreach (var m in freeze)
        {
            Assert.DoesNotContain("JyroResult<JyroValue>.Value", m.Value);
            Assert.DoesNotContain("JyroResult`1.Value", m.ToString());
        }
    }

    // The scanner itself: each kind of problem is found in a document written to hold it, and a clean document is clean.

    private static XDocument Doc(string members) => XDocument.Parse("<doc><assembly><name>X</name></assembly><members>" + members + "</members></doc>");

    private static List<string> Scan(string members, params string[] exempt) =>
        Scanner().Scan(Doc(members), exempt.ToHashSet(StringComparer.Ordinal));

    [Fact(DisplayName = "JYRO-REQ-2180 the scan finds an identifier, a clause number, a backtick, a capital word and a former name")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void TheScanFindsTextProblems()
    {
        var problems = Scan(
            "<member name=\"T:RealTree.Jyro.JyroValue\"><summary>See JYRO-REQ-1930 and 160.7.12. It MUST use `x`. Replaces JyroBuilder; HasErrors.</summary></member>");
        Assert.Contains("identifier: T:RealTree.Jyro.JyroValue: JYRO-REQ-1930", problems);
        Assert.Contains("clause: T:RealTree.Jyro.JyroValue: 160.7.12", problems);
        Assert.Contains("backtick: T:RealTree.Jyro.JyroValue", problems);
        Assert.Contains("capitals: T:RealTree.Jyro.JyroValue: MUST", problems);
        Assert.Contains("former: T:RealTree.Jyro.JyroValue: JyroBuilder", problems);
        Assert.Contains("former: T:RealTree.Jyro.JyroValue: HasErrors", problems);
    }

    [Fact(DisplayName = "JYRO-REQ-2180 the scan finds a history narrative, an internal name and a member the package does not have")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void TheScanFindsNarrativesAndInternalNames()
    {
        var problems = Scan(
            "<member name=\"T:RealTree.Jyro.JyroValue\"><summary>This used to split on a dot. The array had no counterpart to it. "
            + "Kept rather than deleted. It was also documentation-only until named-argument calls. See GetPropertyLiteral and ExactNumber.toDouble.</summary></member>");
        Assert.Contains(problems, p => p.StartsWith("history: T:RealTree.Jyro.JyroValue: This used to", StringComparison.Ordinal));
        Assert.Contains(problems, p => p.StartsWith("history: T:RealTree.Jyro.JyroValue: had no counterpart", StringComparison.Ordinal));
        Assert.Contains(problems, p => p.StartsWith("history: T:RealTree.Jyro.JyroValue: Kept rather than", StringComparison.Ordinal));
        Assert.Contains(problems, p => p.StartsWith("history: T:RealTree.Jyro.JyroValue: was also documentation-only", StringComparison.Ordinal));
        Assert.Contains("unknown-name: T:RealTree.Jyro.JyroValue: GetPropertyLiteral", problems);
        Assert.Contains("internal-name: T:RealTree.Jyro.JyroValue: ExactNumber.toDouble", problems);
    }

    [Fact(DisplayName = "JYRO-REQ-2180 the scan accepts a passive use of used to and a public member written as Type.Member")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void TheScanAcceptsCurrentProse()
    {
        Assert.Empty(Scan(
            "<member name=\"T:RealTree.Jyro.JyroValue\"><summary>Used to compare values; it can be used to read a property, as JyroCompiler.Compile does.</summary></member>"));
    }

    [Fact(DisplayName = "JYRO-REQ-2180 the scan accepts an acronym, a cref and a documented public type")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void TheScanAcceptsCleanText()
    {
        Assert.Empty(Scan(
            "<member name=\"T:RealTree.Jyro.JyroValue\"><summary>Reads JSON as UTF text; see <see cref=\"T:RealTree.Jyro.JyroArray\"/> and <c>x</c>.</summary></member>"));
    }

    [Fact(DisplayName = "JYRO-REQ-2180 the scan finds a documented internal type, a missing member and a cref to nothing, and exempts only the named type id")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public void TheScanFindsNonPublicAndMissing()
    {
        var internalType = Assembly.Load("Jyro.Core").GetTypes()
            .Where(t => t.IsNotPublic && !t.IsNested && t.Namespace == "RealTree.Jyro" && !t.IsGenericType && t.Name.All(char.IsLetterOrDigit))
            .OrderBy(t => t.Name, StringComparer.Ordinal).First();
        var id = "T:" + internalType.FullName;

        Assert.Contains("not-public: " + id, Scan("<member name=\"" + id + "\"><summary>x</summary></member>"));
        Assert.Empty(Scan("<member name=\"" + id + "\"><summary>x</summary></member>", id));

        Assert.Contains(Scan("<member name=\"M:RealTree.Jyro.JyroValue.NoSuchMember\"><summary>x</summary></member>"),
            p => p.StartsWith("missing: M:RealTree.Jyro.JyroValue.NoSuchMember", StringComparison.Ordinal));
        Assert.Contains(Scan("<member name=\"T:RealTree.Jyro.JyroValue\"><summary><see cref=\"T:RealTree.Jyro.NoSuchType\"/></summary></member>"),
            p => p.StartsWith("cref: T:RealTree.Jyro.JyroValue: T:RealTree.Jyro.NoSuchType", StringComparison.Ordinal));
    }
}

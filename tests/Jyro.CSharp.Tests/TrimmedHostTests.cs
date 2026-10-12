using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using RealTree.Jyro;
using Xunit;

namespace JyroHostSurface;

// JYRO-REQ-1924 and JYRO-DEF-836: reflection over the type of a host's own object cannot be kept by the trimmer from
// inside Jyro, so the entry points that do it carry RequiresUnreferencedCode and the warning is the host's, at the
// host's call site. A suppression inside Jyro would hide it and a trimmed host would silently get a different Input.
public class TrimmedHostTests
{
    private static readonly string[] Five = { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static bool Requires(MethodBase m) =>
        m.GetCustomAttributes(false).Any(a => a.GetType().FullName == "System.Diagnostics.CodeAnalysis.RequiresUnreferencedCodeAttribute");

    [Fact(DisplayName = "JYRO-REQ-1924 the entry points that convert an arbitrary host object by reflection carry RequiresUnreferencedCode")]
    [Trait("requirement", "JYRO-REQ-1924")]
    public void ObjectEntryPointsRequireUnreferencedCode()
    {
        var fromObject = typeof(JyroValue).GetMethod("FromObject", new[] { typeof(object) })!;
        var withInput = typeof(JyroRunner).GetMethod("WithInput", new[] { typeof(object) })!;
        var executeScript = typeof(JyroScript).GetMethod("ExecuteScript", new[] { typeof(string), typeof(object) })!;
        Assert.True(Requires(fromObject), "JyroValue.FromObject(object) has no RequiresUnreferencedCode");
        Assert.True(Requires(withInput), "JyroRunner.WithInput(object) has no RequiresUnreferencedCode");
        Assert.True(Requires(executeScript), "JyroScript.ExecuteScript(string, object) has no RequiresUnreferencedCode");
    }

    [Fact(DisplayName = "JYRO-REQ-1924 the entry points that take a JyroValue or JSON carry no RequiresUnreferencedCode, so a trimmed host runs every script of those inputs without a warning")]
    [Trait("requirement", "JYRO-REQ-1924")]
    public void JsonAndValueEntryPointsAreFree()
    {
        var withValue = typeof(JyroRunner).GetMethod("WithInput", new[] { typeof(JyroValue) })!;
        var withJson = typeof(JyroRunner).GetMethod("WithInputJson", new[] { typeof(string) })!;
        var script = typeof(JyroScript).GetMethod("ExecuteScriptWithJson", new[] { typeof(string), typeof(string) })!;
        var compile = typeof(JyroCompiler).GetMethod("Compile", Type.EmptyTypes)!;
        var execute = typeof(JyroRunner).GetMethod("Execute", Type.EmptyTypes)!;
        foreach (var m in new MethodBase[] { withValue, withJson, script, compile, execute })
            Assert.False(Requires(m), m.DeclaringType!.Name + "." + m.Name + " requires unreferenced code");
    }

    private static string Run(string fileName, string arguments, string workingDirectory)
    {
        var psi = new ProcessStartInfo(fileName, arguments)
        {
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        psi.Environment.Remove("UseArtifactsOutput");
        psi.Environment.Remove("ArtifactsPath");
        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        if (!process.WaitForExit((int)TimeSpan.FromMinutes(5).TotalMilliseconds))
        {
            process.Kill(true);
            throw new TimeoutException(fileName + " " + arguments + " did not finish");
        }
        return stdout.Result + "\n" + stderr.Result;
    }

    // Builds a host with the trim analyzer on, which reports at build the warnings a trimmed publish reports for the
    // host's own code, and returns the build output.
    private static string BuildWithTrimAnalyzer(string program)
    {
        var root = Path.Combine(Path.GetTempPath(), "jyro-trim-host-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        try
        {
            var references = string.Join("\n", Five.Append("FSharp.Core").Select(n =>
                "<Reference Include=\"" + n + "\"><HintPath>" + Assembly.Load(n).Location + "</HintPath></Reference>"));
            File.WriteAllText(Path.Combine(root, "Host.csproj"),
                "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework>"
                + "<Nullable>disable</Nullable><ImplicitUsings>enable</ImplicitUsings><EnableTrimAnalyzer>true</EnableTrimAnalyzer>"
                + "<TreatWarningsAsErrors>false</TreatWarningsAsErrors></PropertyGroup><ItemGroup>" + references + "</ItemGroup></Project>");
            File.WriteAllText(Path.Combine(root, "Program.cs"), program);
            return Run("dotnet", "build \"" + Path.Combine(root, "Host.csproj") + "\" -c Release -o \"" + Path.Combine(root, "out") + "\" --nologo",
                root);
        }
        finally { try { Directory.Delete(root, true); } catch (IOException) { } }
    }

    private static readonly Regex Il2026 = new(@"Program\.cs\((\d+),\d+\): warning IL2026: [^\r\n]*", RegexOptions.Compiled);

    [Fact(DisplayName = "JYRO-REQ-1924 a host that converts its own object with FromObject, WithInput(object) or ExecuteScript(script, object) gets the trim warning IL2026 at its own call site")]
    [Trait("requirement", "JYRO-REQ-1924")]
    public void ReflectionOverHostTypesWarnsAtTheHostsCallSite()
    {
        var output = BuildWithTrimAnalyzer("""
            using RealTree.Jyro;
            var host = new { Name = "x", Count = 2 };
            var v = JyroValue.FromObject(host);
            var compiled = new JyroCompiler().WithSource("Output.x = Input.Name").Compile().GetValueOrThrow();
            var r = new JyroRunner(compiled).WithInput((object)host).Execute();
            var s = JyroScript.ExecuteScript("Output.x = 1", host);
            """);
        var lines = Il2026.Matches(output).Select(m => m.Value).ToList();
        Assert.True(lines.Any(l => l.Contains("FromObject", StringComparison.Ordinal)), "No IL2026 for FromObject at Program.cs:\n" + output);
        Assert.True(lines.Any(l => l.Contains("WithInput", StringComparison.Ordinal)), "No IL2026 for WithInput at Program.cs:\n" + output);
        Assert.True(lines.Any(l => l.Contains("ExecuteScript", StringComparison.Ordinal)), "No IL2026 for ExecuteScript at Program.cs:\n" + output);
    }

    [Fact(DisplayName = "JYRO-REQ-1924 a host that compiles and runs scripts over a JyroValue or JSON input gets no trim warning at all")]
    [Trait("requirement", "JYRO-REQ-1924")]
    public void JsonAndValueHostsGetNoTrimWarning()
    {
        var output = BuildWithTrimAnalyzer("""
            using RealTree.Jyro;
            var compiled = new JyroCompiler().WithSource("Output.r = Map([1, 2], (x) => x * 2)").WithStdlib().Compile().GetValueOrThrow();
            var a = new JyroRunner(compiled).WithInputJson("{\"n\":1}").Execute();
            var b = new JyroRunner(compiled).WithInput(new JyroObject()).Execute();
            var c = JyroScript.ExecuteScriptWithJson("Output.x = 1", "{}");
            """);
        Assert.Contains("Build succeeded", output);
        Assert.DoesNotMatch(new Regex(@"warning IL\d{4}"), output);
    }

    [Fact(DisplayName = "JYRO-REQ-1924 no trim warning of a build of the host has a Jyro assembly as its origin")]
    [Trait("requirement", "JYRO-REQ-1924")]
    public void NoTrimWarningOriginatesInAJyroAssembly()
    {
        var output = BuildWithTrimAnalyzer("""
            using RealTree.Jyro;
            var v = JyroValue.FromObject(new { A = 1 });
            """);
        Assert.Contains("Build succeeded", output);
        Assert.DoesNotMatch(new Regex(@"Jyro\.\w+\.(dll|fs)[^\r\n]*warning IL\d{4}"), output);
    }
}

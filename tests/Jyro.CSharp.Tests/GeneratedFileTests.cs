using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using Xunit;

namespace JyroHostSurface;

// JYRO-DEF-837: a file committed as generated is reproduced by its generator. The generator is run in a copy of the
// directory, with the committed output removed, so the repository is not touched, and what it writes is compared with
// the committed file byte for byte (line endings aside).
public class GeneratedFileTests
{
    private static string RootFrom([CallerFilePath] string thisFile = "") =>
        Path.GetFullPath(Path.Combine(Path.GetDirectoryName(thisFile)!, "..", ".."));

    private static string Normalize(string text) => text.Replace("\r\n", "\n");

    [Fact(DisplayName = "JYRO-REQ-2180 running Unicode15.generate.fsx reproduces the committed Unicode15.fs")]
    [Trait("requirement", "JYRO-REQ-2180")]
    public async System.Threading.Tasks.Task TheGeneratorReproducesTheCommittedFile()
    {
        var source = Path.Combine(RootFrom(), "src", "Jyro.Core");
        var committed = Path.Combine(source, "Unicode15.fs");
        var script = Path.Combine(source, "Unicode15.generate.fsx");
        Assert.True(File.Exists(committed) && File.Exists(script), "Unicode15.fs or Unicode15.generate.fsx is missing");

        var work = Path.Combine(Path.GetTempPath(), "jyro-generate-" + Guid.NewGuid().ToString("N"));
        var dir = Path.Combine(work, "src", "Jyro.Core");
        Directory.CreateDirectory(dir);
        try
        {
            // The script, and the pin files and data it reads from beside itself; never the file it writes.
            foreach (var file in Directory.GetFiles(source, "Unicode15.*").Where(f => Path.GetFileName(f) != "Unicode15.fs"))
                File.Copy(file, Path.Combine(dir, Path.GetFileName(file)));
            var psi = new ProcessStartInfo("dotnet", "fsi \"" + Path.Combine(dir, "Unicode15.generate.fsx") + "\"")
            {
                WorkingDirectory = dir,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
            };
            using var process = Process.Start(psi)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            Assert.True(process.WaitForExit((int)TimeSpan.FromMinutes(8).TotalMilliseconds), "the generator did not finish");
            var log = await stdout + "\n" + await stderr;
            Assert.True(process.ExitCode == 0, "the generator exited " + process.ExitCode + ":\n" + log);

            var written = Directory.GetFiles(work, "Unicode15.fs", SearchOption.AllDirectories);
            Assert.True(written.Length == 1,
                "the generator wrote " + written.Length + " files named Unicode15.fs beside its copy; it must write there, next to the script:\n" + log);
            var expected = Normalize(File.ReadAllText(committed));
            var actual = Normalize(File.ReadAllText(written[0]));
            if (expected != actual)
            {
                var a = expected.Split('\n');
                var b = actual.Split('\n');
                var at = Enumerable.Range(0, Math.Min(a.Length, b.Length)).FirstOrDefault(i => a[i] != b[i], Math.Min(a.Length, b.Length));
                Assert.Fail("the generated file differs from the committed one at line " + (at + 1) + ":\ncommitted: "
                            + (at < a.Length ? a[at] : "(end)") + "\ngenerated: " + (at < b.Length ? b[at] : "(end)"));
            }
        }
        finally { try { Directory.Delete(work, true); } catch (IOException) { } }
    }
}

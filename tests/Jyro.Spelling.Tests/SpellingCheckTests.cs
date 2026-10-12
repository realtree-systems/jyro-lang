using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Jyro.Spelling;
using Xunit;

// JYRO-REQ-2175: the US English spelling check of JYRO-REQ-1938 (JYRO-ADR-196 item 8) also runs
// in `dotnet test` of the solution, on a machine with no Python.
//
// What these tests need from the implementation, in the namespace Jyro.Spelling (the folder
// src/Jyro.Spelling), and nothing else:
//
//   public sealed class SpellingFinding
//   {
//       public string Path { get; }     // relative to the root, with '/' separators, as git lists it
//       public int Line { get; }        // from 1
//       public int Column { get; }      // from 1: the first character of the flagged word
//       public string Word { get; }     // the flagged word as written
//   }
//
//   public static class SpellingCheck
//   {
//       // The entries of an allow-list file: one per line, lower case, with blank lines and
//       // lines that begin with '#' left out.
//       public static IReadOnlyList<string> ReadAllow(string allowFile);
//
//       // Each flagged word of one line of text, with no allow-list applied.
//       public static IReadOnlyList<string> FlaggedWords(string line);
//
//       // Each flagged form in the checked files that git tracks under `root` (git ls-files in
//       // that directory), in the order of path and then line, with the allow-list applied.
//       public static IReadOnlyList<SpellingFinding> Scan(string root, IReadOnlyList<string> allow);
//   }
//
// The rules are those of the spelling check (JYRO-ADR-207: one implementation, Jyro.Spelling). The
// cases of the former Python self-test (its tree, its flagged and its passing forms) are held in
// this file, and nothing is read from a self-test folder. Only allow.txt is read.
//
// This file is itself read by the check, so it never spells a flagged form in one piece: the
// British forms below carry a tilde inside the word, and B(...) removes it when the test runs.

namespace JyroSpellingTests;

internal static class Repo
{
    /// The root of the repository: the nearest folder above the tests that holds the allow-list.
    public static string Root { get; } = FindRoot();

    public static string AllowFile => Path.Combine(Root, ".github", "spelling", "allow.txt");

    /// A text with the tildes taken out: how this file writes a form that the check flags.
    public static string B(string text) => text.Replace("~", "");

    private static string FindRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir != null)
        {
            if (File.Exists(Path.Combine(dir.FullName, ".github", "spelling", "allow.txt")))
                return dir.FullName;
            dir = dir.Parent;
        }
        throw new InvalidOperationException(".github/spelling/allow.txt was not found above " + AppContext.BaseDirectory);
    }

    public static string Git(string workingDirectory, params string[] arguments) =>
        GitWithInput(workingDirectory, null, arguments);

    /// Runs git, writing the given bytes to its standard input, and returns its standard output.
    public static string GitWithInput(string workingDirectory, byte[]? input, params string[] arguments)
    {
        var info = new ProcessStartInfo("git")
        {
            WorkingDirectory = workingDirectory,
            RedirectStandardInput = input != null,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        foreach (var a in arguments) info.ArgumentList.Add(a);
        using var process = Process.Start(info)!;
        if (input != null)
        {
            process.StandardInput.BaseStream.Write(input, 0, input.Length);
            process.StandardInput.Close();
        }
        var output = process.StandardOutput.ReadToEnd();
        var error = process.StandardError.ReadToEnd();
        process.WaitForExit();
        Assert.True(process.ExitCode == 0, "git " + string.Join(" ", arguments) + " failed: " + output + error);
        return output;
    }

    /// Builds a git repository holding the given files, tracked, and scans it with the allow-list.
    /// `afterAdd` runs on the repository folder once the files are tracked, before the scan.
    public static IReadOnlyList<SpellingFinding> ScanTree(
        IEnumerable<(string Path, string Text)> files, IReadOnlyList<string> allow, Action<string>? afterAdd = null)
    {
        var tmp = Directory.CreateTempSubdirectory("jyro-spelling-").FullName;
        try
        {
            Git(tmp, "init", "-q");
            foreach (var (path, text) in files)
            {
                var full = System.IO.Path.Combine(tmp, B(path));
                Directory.CreateDirectory(System.IO.Path.GetDirectoryName(full)!);
                File.WriteAllBytes(full, new UTF8Encoding(false).GetBytes(B(text)));
            }
            Git(tmp, "add", "-A");
            afterAdd?.Invoke(tmp);
            return SpellingCheck.Scan(tmp, allow);
        }
        finally
        {
            try { ForceDelete(tmp); } catch (IOException) { } catch (UnauthorizedAccessException) { }
        }
    }

    private static void ForceDelete(string folder)
    {
        foreach (var file in Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories))
            File.SetAttributes(file, FileAttributes.Normal);
        Directory.Delete(folder, recursive: true);
    }

    public static string Describe(IEnumerable<SpellingFinding> found) =>
        string.Join(Environment.NewLine, found.Select(f => $"{f.Path}:{f.Line}:{f.Column}: '{f.Word}' is not US English (JYRO-ADR-196)"));
}

public class SpellingOfTheRepositoryTests
{
    [Fact(DisplayName = "JYRO-REQ-2175 no tracked file of the repository holds a British or Australian form, and a failure names the file, the line and the form")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void TheTrackedFilesHoldNoFlaggedForm()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        var found = SpellingCheck.Scan(Repo.Root, allow);
        Assert.True(found.Count == 0,
            found.Count + " British or Australian form(s) found; respell each in US English, or name an external name in "
            + ".github/spelling/allow.txt:" + Environment.NewLine + Repo.Describe(found));
    }

    [Fact(DisplayName = "JYRO-REQ-2175 the allow-list the check reads is .github/spelling/allow.txt, with its external names")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void TheSharedAllowList()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.Contains("floors defects analyse", allow);
        Assert.Contains("floors-next:analyse", allow);
        Assert.DoesNotContain(allow, e => e.StartsWith('#') || e.Length == 0 || e != e.ToLowerInvariant());
    }
}

public class SpellingSelfTestTests
{
    // The flagged cases of the former Python self-test, now held here so that nothing is read from
    // a self-test folder. Each carries a tilde that B(...) removes.
    private static readonly string[] Flagged =
    {
        "arte~fact", "ARTE~FACTs", "JyroArte~factInfo", "is_cance~lled", "Cance~lledByHost",
        "cen~tring", "cen~trer", "colo~urise", "analy~ser", "serialis~able", "finalis~er",
        "normalis~er", "labe~ller", "colo~urless", "behavio~ural", "Behavio~urs", "initiali~se",
        "me~tres", "defe~nce", "catalo~gue", "gr~ey", "judge~ment", "unauthori~sed",
        "unrecogni~sed", "uninitiali~sed", "unlabe~lled", "denormali~sed", "reorgani~se",
        "misbehavio~ur", "colo~urfully", "gr~eyscale", "neighbo~urhood",
    };

    // The passing cases of the former Python self-test.
    private static readonly string[] Passing =
    {
        "artifact", "ARTIFACTs", "JyroArtifactInfo", "is_canceled", "CanceledByHost",
        "CancellationToken", "cancellation", "DeadlineCanceller", "centering", "analyzes",
        "analyses", "labeled", "behavior", "initialize", "meters", "parameter", "defense",
        "license", "totally", "gray", "judgment", "XMLReader", "unauthorized", "uninitialized",
        "unlabeled", "denormalized", "reorganize", "misbehavior", "grayscale", "neighborhood",
        "underline", "cooperate", "uncover",
    };

    public static IEnumerable<object[]> FlaggedLines() => Flagged.Select(l => new object[] { l });

    public static IEnumerable<object[]> PassingLines() => Passing.Select(l => new object[] { l });

    [Theory(DisplayName = "JYRO-REQ-2175 a British or Australian form of the self-test is flagged")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [MemberData(nameof(FlaggedLines))]
    public void AFlaggedFormIsFlagged(string line)
    {
        Assert.NotEmpty(SpellingCheck.FlaggedWords(Repo.B(line)));
    }

    [Theory(DisplayName = "JYRO-REQ-2175 a US form or a word that only looks British of the self-test is not flagged")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [MemberData(nameof(PassingLines))]
    public void APassingFormIsNotFlagged(string line)
    {
        Assert.Empty(SpellingCheck.FlaggedWords(line));
    }

    [Fact(DisplayName = "JYRO-REQ-2175 an identifier is split at its word boundaries and the flagged word is reported as written")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void IdentifiersAreSplit()
    {
        Assert.Equal(new[] { Repo.B("Arte~fact") }, SpellingCheck.FlaggedWords(Repo.B("let Arte~factTooLarge = 7000")));
        Assert.Equal(new[] { Repo.B("cance~lled") }, SpellingCheck.FlaggedWords(Repo.B("is_cance~lled")));
        Assert.Equal(new[] { Repo.B("ARTE~FACTs") }, SpellingCheck.FlaggedWords(Repo.B("ARTE~FACTs")));
        Assert.Empty(SpellingCheck.FlaggedWords("XMLReader and CancellationToken"));
        Assert.Equal(new[] { Repo.B("unauthori~sed") }, SpellingCheck.FlaggedWords(Repo.B("an unauthori~sed call")));
        Assert.Equal(new[] { Repo.B("COLO~UR") }, SpellingCheck.FlaggedWords(Repo.B("COLO~UR")));
    }

    [Fact(DisplayName = "JYRO-REQ-2175 the entries of an allow-list file are lower case, with blank lines and comments left out")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void ReadAllowParsesAnEntryPerLine()
    {
        var file = Path.GetTempFileName();
        try
        {
            File.WriteAllText(file, Repo.B("# a comment\n\n  Vendor Colo~ur Picker  \n#another\nfloors defects analyse\n"));
            Assert.Equal(new[] { Repo.B("vendor colo~ur picker"), "floors defects analyse" }, SpellingCheck.ReadAllow(file));
        }
        finally
        {
            File.Delete(file);
        }
    }
}

public class SpellingOfAScratchTreeTests
{
    /// The tree of this project's own case list (formerly the Python script's TREE): each file, its text and the flagged
    /// words the scan must report for it (none where it must pass).
    public static IEnumerable<object[]> TheTree() => new[]
    {
        new object[] { "src/A.fs", "let Arte~factTooLarge = 7000", new[] { "Arte~fact" } },
        new object[] { "src/B.fs", "// the colo~ur of the value", new[] { "colo~ur" } },
        new object[] { "tests/C.fs", "cance~lled lice~nce initiali~se labe~lled", new[] { "cance~lled", "lice~nce", "initiali~se", "labe~lled" } },
        new object[] { "README.md", "The behavio~ur of a run", new[] { "behavio~ur" } },
        new object[] { "docs/Guide.md", "the cen~tre of the page", new[] { "cen~tre" } },
        new object[] { "src/D.fs", "// run floors defects analyse to classify it", Array.Empty<string>() },
        new object[] { "src/E.fs", "analy~se // spelling:ignore", new[] { "analy~se" } },
        new object[] { "src/F.fs", "color center license behavior canceled", Array.Empty<string>() },
        new object[] { "AUDIT.md", "an arte~fact found in the audit", Array.Empty<string>() },
        new object[] { "review notes.md", "the arte~fact the review named", Array.Empty<string>() },
        new object[] { "other/G.txt", "colo~ur outside src, tests and Markdown", Array.Empty<string>() },
    };

    private static IReadOnlyList<SpellingFinding> ScanTheTree()
    {
        var files = TheTree().Select(row => ((string)row[0], (string)row[1] + "\n"));
        return Repo.ScanTree(files, SpellingCheck.ReadAllow(Repo.AllowFile));
    }

    [Theory(DisplayName = "JYRO-REQ-2175 a scan of the tree of the Python self-test reports for each file exactly the words that self-test expects")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [MemberData(nameof(TheTree))]
    public void EachFileGivesTheExpectedWords(string path, string text, string[] expected)
    {
        Assert.NotEmpty(text);
        var want = expected.Select(Repo.B).ToHashSet();
        var got = ScanTheTree().Where(f => f.Path == Repo.B(path)).Select(f => f.Word).ToHashSet();
        Assert.True(got.SetEquals(want),
            path + " gave [" + string.Join(", ", got.OrderBy(w => w, StringComparer.Ordinal)) + "], expected [" + string.Join(", ", want.OrderBy(w => w, StringComparer.Ordinal)) + "]");
    }

    [Fact(DisplayName = "JYRO-REQ-2175 a scan names the file, the line, the column and the form of each flagged form")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void AFindingNamesFileLineColumnAndForm()
    {
        var found = Repo.ScanTree(
            new[] { ("src/Deep/A.fs", "let ok = 1\nlet Arte~factTooLarge = 7000\n") },
            SpellingCheck.ReadAllow(Repo.AllowFile));
        var f = Assert.Single(found);
        Assert.Equal("src/Deep/A.fs", f.Path);
        Assert.Equal(2, f.Line);
        Assert.Equal(5, f.Column);
        Assert.Equal(Repo.B("Arte~fact"), f.Word);
    }

    [Fact(DisplayName = "JYRO-REQ-2175 the lines of a file with Windows line ends are counted once each")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void WindowsLineEnds()
    {
        var found = Repo.ScanTree(
            new[] { ("README.md", "first line\r\nsecond line\r\nthe colo~ur\r\n") },
            SpellingCheck.ReadAllow(Repo.AllowFile));
        var f = Assert.Single(found);
        Assert.Equal(3, f.Line);
        Assert.Equal(5, f.Column);
        Assert.Equal(Repo.B("colo~ur"), f.Word);
    }

    [Fact(DisplayName = "JYRO-REQ-2175 the examples of the requirement: a British form in a comment under tests, in an identifier under src and in a README fails, and the US form of each passes")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void TheBookExamples()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);

        var comment = Repo.ScanTree(new[] { ("tests/Jyro.CSharp.Tests/A.cs", "// The detector must recogni~se a union\n") }, allow);
        Assert.Equal(Repo.B("recogni~se"), Assert.Single(comment).Word);
        Assert.Empty(Repo.ScanTree(new[] { ("tests/Jyro.CSharp.Tests/A.cs", "// The detector must recognize a union\n") }, allow));

        var identifier = Repo.ScanTree(new[] { ("src/Jyro.Core/B.fs", "type Arte~factTooLarge = int\n") }, allow);
        Assert.Equal(Repo.B("Arte~fact"), Assert.Single(identifier).Word);
        Assert.Empty(Repo.ScanTree(new[] { ("src/Jyro.Core/B.fs", "type ArtifactTooLarge = int\n") }, allow));

        var readme = Repo.ScanTree(new[] { ("README.md", "The value is initiali~sed first.\n") }, allow);
        Assert.Equal(Repo.B("initiali~sed"), Assert.Single(readme).Word);
        Assert.Empty(Repo.ScanTree(new[] { ("README.md", "The value is initialized first.\n") }, allow));
    }

    [Fact(DisplayName = "JYRO-REQ-2175 a British form in AUDIT.md or review notes.md does not fail, and the same form in any other Markdown file does")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void TheHistoricalRecordsAreLeftOut()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.Empty(Repo.ScanTree(new[] { ("AUDIT.md", "the behavio~ur was initiali~sed\n"), ("review notes.md", "the colo~ur was recogni~sed\n") }, allow));

        var elsewhere = Repo.ScanTree(
            new[] { ("docs/AUDIT.md", "the behavio~ur\n"), ("Notes.MD", "the colo~ur\n"), ("src/review notes.md", "the cen~tre\n") }, allow);
        Assert.Equal(new[] { "Notes.MD", "docs/AUDIT.md", "src/review notes.md" }, elsewhere.Select(f => f.Path).OrderBy(p => p, StringComparer.Ordinal).ToArray());
    }

    [Fact(DisplayName = "JYRO-REQ-2175 only src, tests and Markdown are checked, and a file that holds a NUL byte is not read")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void WhichFilesAreChecked()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        var found = Repo.ScanTree(
            new[]
            {
                ("srcs/A.fs", "the colo~ur\n"),
                ("docs/page.txt", "the colo~ur\n"),
                ("build/notes.json", "{\"colo~ur\": 1}\n"),
                ("src/blob.bin", "the colo~ur\0 more\n"),
                ("tests/inner/Deep.fs", "the colo~ur\n"),
                ("src/inner/Deep.fs", "the colo~ur\n"),
            }, allow);
        Assert.Equal(new[] { "src/inner/Deep.fs", "tests/inner/Deep.fs" }, found.Select(f => f.Path).OrderBy(p => p, StringComparer.Ordinal).ToArray());
    }

    [Fact(DisplayName = "JYRO-REQ-2175 an allow-list entry accepts a flagged word inside an occurrence of the entry on its line and nowhere else, in any case")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void TheAllowListAcceptsWithinAnEntry()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);

        // The name of the FLOORS command passes, in any case; a second flagged word on the line does not.
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", "// run floors defects analyse to classify it\n") }, allow));
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", "// run FLOORS Defects Analyse to classify it\n") }, allow));
        var second = Repo.ScanTree(new[] { ("src/A.fs", "floors defects analyse the colo~ur\n") }, allow);
        Assert.Equal(Repo.B("colo~ur"), Assert.Single(second).Word);
        var apart = Repo.ScanTree(new[] { ("src/A.fs", "analy~se and then floors defects analyse\n") }, allow);
        var only = Assert.Single(apart);
        Assert.Equal(1, only.Column);
        Assert.Equal(Repo.B("analy~se"), only.Word);
    }

    [Fact(DisplayName = "JYRO-REQ-2175 a word added to the allow-list file is accepted by the check without being added anywhere else")]
    [Trait("requirement", "JYRO-REQ-2175")]
    public void AnAddedEntryIsAccepted()
    {
        var file = Path.GetTempFileName();
        try
        {
            var line = "the Vendor Colo~ur Picker widget\n";
            var files = new[] { ("src/A.fs", line) };
            var without = Repo.ScanTree(files, SpellingCheck.ReadAllow(Repo.AllowFile));
            Assert.Equal(Repo.B("Colo~ur"), Assert.Single(without).Word);

            File.WriteAllText(file, File.ReadAllText(Repo.AllowFile) + Repo.B("\n# added by a test\nvendor colo~ur picker\n"));
            Assert.Empty(Repo.ScanTree(files, SpellingCheck.ReadAllow(file)));
        }
        finally
        {
            File.Delete(file);
        }
    }
}

// JYRO-DEF-825, 826, 827: the .NET check must give the verdict the former Python check gave for
// any tree, so case folding and trimming are ASCII only, allow.txt is read strictly as UTF-8, and
// a tracked path that cannot be read, or whose name is not valid UTF-8, makes the scan fail.
public class SpellingStrictnessTests
{
    private static string WriteTemp(byte[] bytes)
    {
        var file = Path.GetTempFileName();
        File.WriteAllBytes(file, bytes);
        return file;
    }

    [Fact(DisplayName = "JYRO-DEF-825 JYRO-REQ-2175 a line holding U+0130 before an allow-listed phrase gives the same verdict as without it")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-825")]
    public void CaseFoldingIsAsciiOnly()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", "// run floors defects analyse to classify it\n") }, allow));
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", "// İ run floors defects analyse to classify it\n") }, allow));
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", "İİİ FLOORS Defects Analyse\n") }, allow));

        var entry = new[] { Repo.B("analy~se x") };
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", Repo.B("I analy~se x\n")) }, entry));
        Assert.Empty(Repo.ScanTree(new[] { ("src/A.fs", Repo.B("İ I analy~se x\n")) }, entry));

        // A word that is not allowed is still flagged after U+0130, at its column counted in characters.
        var found = Repo.ScanTree(new[] { ("src/A.fs", "İ colo~ur\n") }, allow);
        var f = Assert.Single(found);
        Assert.Equal(Repo.B("colo~ur"), f.Word);
        Assert.Equal(3, f.Column);
    }

    [Fact(DisplayName = "JYRO-DEF-825 JYRO-REQ-2175 an allow-list entry is trimmed of ASCII white space only and lowered for A to Z only")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-825")]
    public void EntriesAreTrimmedAndLoweredAsAscii()
    {
        var bytes = new UTF8Encoding(false).GetBytes(
            " \tAscii Entry\t \n Nbsp Entry \n Em Space \nÉcole Name\n");
        var file = WriteTemp(bytes);
        try
        {
            var allow = SpellingCheck.ReadAllow(file);
            Assert.Equal(
                new[] { "ascii entry", " nbsp entry ", " em space ", "École name" },
                allow);
        }
        finally
        {
            File.Delete(file);
        }
    }

    [Fact(DisplayName = "JYRO-DEF-825 JYRO-REQ-2175 allow.txt is read as UTF-8 with a leading byte order mark kept as part of the first line")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-825")]
    public void ABomIsKept()
    {
        var bytes = new byte[] { 0xEF, 0xBB, 0xBF }.Concat(Encoding.ASCII.GetBytes("vendor name\nsecond entry\n")).ToArray();
        var file = WriteTemp(bytes);
        try
        {
            Assert.Equal(new[] { "﻿vendor name", "second entry" }, SpellingCheck.ReadAllow(file));
        }
        finally
        {
            File.Delete(file);
        }
    }

    [Fact(DisplayName = "JYRO-DEF-825 JYRO-REQ-2175 allow.txt that holds invalid UTF-8 is refused, not read with replacement")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-825")]
    public void InvalidUtf8IsRefused()
    {
        var bytes = Encoding.ASCII.GetBytes("good entry\nbad ").Concat(new byte[] { 0xFF, 0xFE, 0x41 }).Concat(Encoding.ASCII.GetBytes("\n")).ToArray();
        var file = WriteTemp(bytes);
        try
        {
            Assert.ThrowsAny<Exception>(() => SpellingCheck.ReadAllow(file));
        }
        finally
        {
            File.Delete(file);
        }
    }

    [Fact(DisplayName = "JYRO-DEF-826 JYRO-REQ-2175 a tracked path under src that is deleted from the working tree makes the scan fail")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-826")]
    public void ADeletedTrackedFileFails()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.ThrowsAny<Exception>(() => Repo.ScanTree(
            new[] { ("src/A.fs", "let a = 1\n"), ("src/B.fs", "let b = 2\n") },
            allow,
            tmp => File.Delete(Path.Combine(tmp, "src", "A.fs"))));
    }

    [Fact(DisplayName = "JYRO-DEF-826 JYRO-REQ-2175 a tracked Markdown path that is deleted from the working tree makes the scan fail")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-826")]
    public void ADeletedTrackedMarkdownFails()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.ThrowsAny<Exception>(() => Repo.ScanTree(
            new[] { ("README.md", "text\n"), ("src/B.fs", "let b = 2\n") },
            allow,
            tmp => File.Delete(Path.Combine(tmp, "README.md"))));
    }

    [Fact(DisplayName = "JYRO-DEF-826 JYRO-REQ-2175 a tracked path under src that is a directory (a submodule) makes the scan fail")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-826")]
    public void ATrackedDirectoryFails()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.ThrowsAny<Exception>(() => Repo.ScanTree(
            new[] { ("src/B.fs", "let b = 2\n") },
            allow,
            tmp =>
            {
                Directory.CreateDirectory(Path.Combine(tmp, "src", "sub"));
                Repo.Git(tmp, "update-index", "--add", "--cacheinfo", "160000,e69de29bb2d1d6434b8b29ae775ad8c2e48c5391,src/sub");
            }));
    }

    [Fact(DisplayName = "JYRO-DEF-827 JYRO-REQ-2175 a tracked path name under src that is not valid UTF-8 makes the scan fail")]
    [Trait("requirement", "JYRO-REQ-2175")]
    [Trait("defect", "JYRO-DEF-827")]
    public void ANonUtf8PathNameFails()
    {
        var allow = SpellingCheck.ReadAllow(Repo.AllowFile);
        Assert.ThrowsAny<Exception>(() => Repo.ScanTree(
            new[] { ("src/B.fs", "let b = 2\n") },
            allow,
            tmp =>
            {
                var blob = Repo.GitWithInput(tmp, Encoding.ASCII.GetBytes("x\n"), "hash-object", "-w", "--stdin").Trim();
                var entry = Encoding.ASCII.GetBytes("100644 " + blob + "\tsrc/bad")
                    .Concat(new byte[] { 0xFF, 0xFE })
                    .Concat(Encoding.ASCII.GetBytes(".fs\n"))
                    .ToArray();
                Repo.GitWithInput(tmp, entry, "update-index", "--add", "--index-info");
            }));
    }
}

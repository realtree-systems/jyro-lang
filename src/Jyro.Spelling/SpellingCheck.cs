// The US English spelling check of the repository (JYRO-ADR-196 item 8), in
// .NET (JYRO-REQ-2175). It is the one implementation of the check (JYRO-ADR-207):
// CI runs it through its tests. Its rules:
//
//   * the files read are the tracked files under src and tests, and every
//     tracked Markdown document, except the historical records AUDIT.md and
//     review notes.md;
//   * a word is flagged when, in lower case, it is one of the forms of the
//     decision with their inflections, after an optional prefix, in prose or
//     inside an identifier split at its word boundaries;
//   * a flagged word is accepted only inside an occurrence, on its line, of a
//     phrase of the allow-list, .github/spelling/allow.txt; no marker on a
//     line accepts one.
//
// Case is folded in ASCII only, A to Z to a to z, which keeps the length of a
// line, so a span found in the folded line is the same span of the line; a
// flagged word is ASCII letters, so nothing is lost. An entry of the allow-list
// is trimmed of ASCII white space only. allow.txt and the list of tracked paths
// are decoded as strict UTF-8: a byte order mark is kept as U+FEFF, and invalid
// UTF-8 throws. A tracked checked path that is not a readable file throws.
//
// This file is itself one of the files the check reads, so it holds no form
// the check flags: each stem below is written in two pieces, neither of which
// is a flagged word, and joined when the type is first used.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace Jyro.Spelling;

/// <summary>A flagged form found in a checked file: the path relative to the
/// root, with forward slashes, the 1-based line, the 1-based column in code
/// points, and the word as the file writes it.</summary>
public sealed record SpellingFinding(string Path, int Line, int Column, string Word)
{
    /// <summary>The line CI prints for the same finding.</summary>
    public override string ToString() =>
        $"{Path}:{Line}:{Column}: '{Word}' is not US English (JYRO-ADR-196)";
}

/// <summary>The check: its word rule, its allow-list, and the scan of a
/// repository.</summary>
public static class SpellingCheck
{
    // The endings a stem takes, as the CI check names them.
    private const string Our = "(s|ed|ing|ful|fully|less|able|ably|al|ally|ite|ites|er|ers|hood|hoods)?";
    private const string Ise = "(e|es|ed|ing|er|ers|able|ation|ations)";
    private const string Prefix = "(un|re|de|mis|pre|non|dis|sub|over|under|co)?";
    private const string Lled = "(ed|ing|er|ers)";

    // The forms of JYRO-ADR-196 item 8, in the order of the CI check. Each
    // stem is split in two so that this file holds no flagged word.
    private static readonly string[] Flagged =
    {
        "arte" + "facts?",
        "behavi" + "our" + Our,
        "col" + "our" + Our,
        "fav" + "our" + Our,
        "hon" + "our" + Our,
        "lab" + "our" + Our,
        "neighb" + "our" + Our,
        "cancell(ed|ing)",
        "initialis" + Ise,
        "serialis" + Ise,
        "normalis" + Ise,
        "analys(e|ed|ing|er|ers)",
        "optimis" + Ise,
        "recognis" + Ise,
        "organis" + Ise,
        "finalis" + Ise,
        "minimis" + Ise,
        "maximis" + Ise,
        "summaris" + Ise,
        "utilis" + Ise,
        "customis" + Ise,
        "specialis" + Ise,
        "authoris" + Ise,
        "prioritis" + Ise,
        "synchronis" + Ise,
        "catalogu(e|es|ed|ing)",
        "centr(e|es|ed|ing|er|ers)",
        "col" + "ouris" + Ise,
        "(kilo|centi|milli)?met" + "res?",
        "lic" + "ences?",
        "def" + "ences?",
        "off" + "ences?",
        "label" + "l" + Lled,
        "signal" + "l" + Lled,
        "model" + "l" + Lled,
        "travel" + "l" + Lled,
        "total" + "l" + Lled,
        "fulf" + "il|" + "fulf" + "ils|" + "fulf" + "ilment|" + "fulf" + "ilments",
        "judge" + "ments?",
        "acknowledge" + "ments?",
        "gr" + "ey(s|ed|ing|ish|ness|scale)?",
        "programm" + "es?",
    };

    private static readonly Regex FlaggedRe = new(
        "^" + Prefix + "(?:" + string.Join("|", Flagged.Select(p => "(?:" + p + ")")) + ")$",
        RegexOptions.CultureInvariant);

    // A run of letters, then the words of an identifier inside it: an
    // upper-case run before a capitalized word, a word with an optional
    // capital, or an upper-case run.
    private static readonly Regex LettersRe = new("[A-Za-z]+", RegexOptions.CultureInvariant);
    private static readonly Regex WordRe = new("[A-Z]+(?=[A-Z][a-z])|[A-Z]?[a-z]+|[A-Z]+", RegexOptions.CultureInvariant);

    // An upper-case run followed by lower-case letters, read as one word as
    // well: an upper-case word with a lower-case plural ending.
    private static readonly Regex CapsEndingRe = new("[A-Z]{2,}[a-z]+", RegexOptions.CultureInvariant);

    // The historical records, which keep their text.
    private static readonly HashSet<string> Excluded = new(StringComparer.Ordinal) { "AUDIT.md", "review notes.md" };

    /// <summary>Whether the check reads the tracked file <paramref name="path"/>,
    /// a path relative to the root with forward slashes.</summary>
    public static bool IsChecked(string path)
    {
        ArgumentNullException.ThrowIfNull(path);
        if (Excluded.Contains(path)) return false;
        return path.StartsWith("src/", StringComparison.Ordinal)
            || path.StartsWith("tests/", StringComparison.Ordinal)
            || path.EndsWith(".md", StringComparison.OrdinalIgnoreCase);
    }

    /// <summary>The entries of an allow-list file, folded to lower case in
    /// ASCII: each line with its ASCII white space trimmed, but the empty lines
    /// and the comments, lines that start with <c>#</c>. The file is read as
    /// strict UTF-8: a byte order mark is kept as U+FEFF, part of the first
    /// line, and invalid UTF-8 throws.</summary>
    public static IReadOnlyList<string> ReadAllow(string file)
    {
        ArgumentNullException.ThrowIfNull(file);
        var allow = new List<string>();
        foreach (var raw in SplitLines(StrictUtf8.GetString(File.ReadAllBytes(file))))
        {
            var entry = raw.Trim(AsciiWhiteSpace);
            if (entry.Length > 0 && !entry.StartsWith('#'))
                allow.Add(FoldAscii(entry));
        }
        return allow;
    }

    /// <summary>Whether <paramref name="word"/> is a flagged form, in any
    /// ASCII case.</summary>
    public static bool IsFlagged(string word)
    {
        ArgumentNullException.ThrowIfNull(word);
        return FlaggedRe.IsMatch(FoldAscii(word));
    }

    // UTF-8 with no byte order mark written and an exception on invalid bytes.
    // GetString does not detect or strip a byte order mark.
    private static readonly UTF8Encoding StrictUtf8 = new(false, true);

    // The ASCII white space: space, tab, line feed, vertical tab, form feed and
    // carriage return.
    private static readonly char[] AsciiWhiteSpace = { ' ', '\t', '\n', '\v', '\f', '\r' };

    // `text` with each of A to Z as a to z and every other character as it
    // is, so the length is kept.
    internal static string FoldAscii(string text) =>
        string.Create(text.Length, text, static (span, source) =>
        {
            for (var i = 0; i < source.Length; i++)
            {
                var c = source[i];
                span[i] = c is >= 'A' and <= 'Z' ? (char)(c + ('a' - 'A')) : c;
            }
        });

    /// <summary>Each flagged word of <paramref name="line"/>, as the line
    /// writes it, in the order the CI check finds them.</summary>
    public static IReadOnlyList<string> FlaggedWords(string line)
    {
        ArgumentNullException.ThrowIfNull(line);
        return FlaggedSpans(line).Select(f => f.Word).ToList();
    }

    /// <summary>Each flagged form in the checked files tracked under
    /// <paramref name="root"/>, a git working tree, that no entry of
    /// <paramref name="allow"/> covers: in the order of the paths, compared
    /// ordinally, then of the lines and the words.</summary>
    public static IReadOnlyList<SpellingFinding> Scan(string root, IEnumerable<string> allow)
    {
        ArgumentNullException.ThrowIfNull(root);
        ArgumentNullException.ThrowIfNull(allow);
        var phrases = allow.Select(FoldAscii).ToArray();
        var utf8 = new UTF8Encoding(false, false);
        var found = new List<SpellingFinding>();
        var paths = TrackedFiles(root).Where(p => p.Length > 0 && IsChecked(p)).ToList();
        paths.Sort(StringComparer.Ordinal);
        foreach (var path in paths)
        {
            // A tracked path that is not a readable file, such as one deleted
            // from the working tree or a submodule, throws here.
            var full = System.IO.Path.Combine(root, path);
            var data = File.ReadAllBytes(full);
            if (Array.IndexOf(data, (byte)0) >= 0) continue;
            var number = 0;
            foreach (var line in SplitLines(utf8.GetString(data)))
            {
                number++;
                List<(int Start, int End)>? spans = null;
                foreach (var (start, end, word) in FlaggedSpans(line))
                {
                    spans ??= AllowedSpans(line, phrases);
                    if (spans.Any(s => s.Start <= start && end <= s.End)) continue;
                    found.Add(new SpellingFinding(path, number, CodePoints(line, start) + 1, word));
                }
            }
        }
        return found;
    }

    // Each flagged word of `line`, as UTF-16 indexes.
    private static IEnumerable<(int Start, int End, string Word)> FlaggedSpans(string line)
    {
        foreach (Match run in LettersRe.Matches(line))
        {
            var seen = new HashSet<int>();
            var candidates = WordRe.Matches(run.Value).Select(m => (m.Index, m.Value))
                .Concat(CapsEndingRe.Matches(run.Value).Select(m => (m.Index, m.Value)));
            foreach (var (offset, word) in candidates)
            {
                if (!IsFlagged(word)) continue;
                var start = run.Index + offset;
                if (seen.Add(start))
                    yield return (start, start + word.Length, word);
            }
        }
    }

    // The spans of `line` that an entry of the allow-list covers: every
    // occurrence of every entry, folded in ASCII, which keeps the length.
    private static List<(int Start, int End)> AllowedSpans(string line, string[] phrases)
    {
        var lower = FoldAscii(line);
        var spans = new List<(int, int)>();
        foreach (var phrase in phrases)
        {
            if (phrase.Length == 0)
            {
                for (var i = 0; i <= lower.Length; i++) spans.Add((i, i));
                continue;
            }
            var start = lower.IndexOf(phrase, StringComparison.Ordinal);
            while (start >= 0)
            {
                spans.Add((start, start + phrase.Length));
                start = start + 1 <= lower.Length ? lower.IndexOf(phrase, start + 1, StringComparison.Ordinal) : -1;
            }
        }
        return spans;
    }

    // The number of code points of `text` before the UTF-16 index `index`.
    private static int CodePoints(string text, int index)
    {
        var count = 0;
        for (var i = 0; i < index; i++)
        {
            if (char.IsLowSurrogate(text[i]) && i > 0 && char.IsHighSurrogate(text[i - 1])) continue;
            count++;
        }
        return count;
    }

    // The lines of `text` as Python's `str.splitlines` gives them: split at
    // LF, CR, CR LF, VT, FF, FS, GS, RS, NEL, U+2028 and U+2029, with no
    // empty line after a final line end.
    private static bool IsLineBreak(char c) =>
        c is (char)0x0A or (char)0x0D or (char)0x0B or (char)0x0C or (char)0x1C or (char)0x1D or (char)0x1E
            or (char)0x85 or (char)0x2028 or (char)0x2029;

    internal static IEnumerable<string> SplitLines(string text)
    {
        var start = 0;
        var i = 0;
        while (i < text.Length)
        {
            var c = text[i];
            if (IsLineBreak(c))
            {
                yield return text.Substring(start, i - start);
                i += c == '\r' && i + 1 < text.Length && text[i + 1] == '\n' ? 2 : 1;
                start = i;
            }
            else i++;
        }
        if (start < text.Length) yield return text.Substring(start);
    }

    // The tracked files of the working tree at `root`, as `git ls-files -z`
    // lists them, decoded as strict UTF-8: a path name that is not valid
    // UTF-8 throws.
    private static IEnumerable<string> TrackedFiles(string root)
    {
        var info = new ProcessStartInfo("git")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        info.ArgumentList.Add("-C");
        info.ArgumentList.Add(root);
        info.ArgumentList.Add("ls-files");
        info.ArgumentList.Add("-z");
        using var git = Process.Start(info) ?? throw new InvalidOperationException("git could not be started.");
        var errors = git.StandardError.ReadToEndAsync();
        using var bytes = new MemoryStream();
        git.StandardOutput.BaseStream.CopyTo(bytes);
        git.WaitForExit();
        if (git.ExitCode != 0)
            throw new InvalidOperationException($"git ls-files failed in {root}: {errors.Result}");
        return StrictUtf8.GetString(bytes.GetBuffer(), 0, (int)bytes.Length).Split('\0');
    }
}

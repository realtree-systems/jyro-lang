using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// A message-code number, once retired, is not available again.
//
// The public API ratchet pins what the code numbers are today. It cannot pin
// what they used to be, and that is the half that bites: a number removed in
// one release looks free in the next, so the next code to need a slot in that
// band lands on it. The number is by then in logs, in alert rules, in support
// tickets and in the published error-code reference, all of which now say
// something they did not say when they were written.
//
// This is not hypothetical here. It happened twice in one sitting: `3102` was
// `ReservedIdentifier` and `5301` was `NegativeIndex`, both shipped under the
// v0.9.x tags and both removed; both were then chosen for new codes precisely
// because the numbers looked unused, in the same change that retired `3503` and
// wrote "a retired number stays retired" into the source beside it. Nothing was
// enforcing it, so writing it down was not enough.
//
// The list is recovered from the history of Diagnostics.fs and checked in.
public class RetiredMessageCodes
{
    [Fact]
    public void NoLiveCodeReusesARetiredNumber()
    {
        var retired = ReadRetired();
        var live = Enum.GetValues<MessageCode>()
            .Select(c => (Number: (int)c, Name: c.ToString()))
            .ToList();

        var collisions = live
            .Where(c => retired.ContainsKey(c.Number))
            .Select(c => $"  {c.Number} is now {c.Name}, and was {retired[c.Number]}")
            .ToList();

        Assert.True(collisions.Count == 0,
            "These live message codes reuse a number that was retired:"
            + Environment.NewLine
            + string.Join(Environment.NewLine, collisions)
            + Environment.NewLine + Environment.NewLine
            + "A code number reaches logs, alert rules and the published error-code "
            + "reference, and it outlives the release that emitted it. Give the new "
            + "code the next free number instead - RetiredMessageCodes.txt lists what "
            + "is spent.");
    }

    [Fact]
    public void EveryRetiredNumberIsGenuinelyAbsent()
    {
        // The list has to stay honest in the other direction too, or it grows
        // entries that block numbers nothing ever used. Anything named here
        // must be gone from the enum - which is what the test above assumes.
        var retired = ReadRetired();
        var live = Enum.GetValues<MessageCode>().Select(c => (int)c).ToHashSet();
        var stillPresent = retired.Keys.Where(live.Contains).ToList();

        Assert.True(stillPresent.Count == 0,
            "RetiredMessageCodes.txt lists numbers that are still live: "
            + string.Join(", ", stillPresent));
    }

    private static Dictionary<int, string> ReadRetired()
    {
        var path = Path.Combine(Path.GetDirectoryName(ThisFile())!, "RetiredMessageCodes.txt");
        Assert.True(File.Exists(path), $"The retired-code list is missing: {path}");

        var retired = new Dictionary<int, string>();
        foreach (var raw in File.ReadAllLines(path))
        {
            var line = raw.Trim();
            if (line.Length == 0 || line.StartsWith("#", StringComparison.Ordinal)) continue;

            var split = line.IndexOf(' ');
            Assert.True(split > 0, $"Malformed line in the retired-code list: {raw}");
            var number = int.Parse(line[..split], CultureInfo.InvariantCulture);
            retired[number] = line[(split + 1)..].Trim();
        }
        return retired;
    }

    private static string ThisFile([CallerFilePath] string path = "") => path;
}

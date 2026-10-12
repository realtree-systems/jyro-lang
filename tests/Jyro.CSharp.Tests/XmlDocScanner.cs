using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Xml.Linq;

namespace JyroHostSurface;

// The scan of JYRO-REQ-2180 over one shipped XML documentation file. It reports problems as
// "kind: member: detail" lines, so a test can name the kind it expects.
internal sealed class XmlDocScanner
{
    private static readonly Regex Identifier = new(@"JYRO-(ADR|REQ|DEF)-\d+", RegexOptions.Compiled);
    private static readonly Regex ClauseNumber = new(@"\b\d{2,3}\.\d{1,2}(\.\d{1,2})+\b", RegexOptions.Compiled);
    private static readonly Regex CapitalWord = new(@"\b[A-Z]{3,}\b", RegexOptions.Compiled);
    private static readonly Regex Denylisted = new(
        @"\b(JyroBuilder|JyroHostContext|ToBooleanTruthiness|HasErrors|Rt[A-Z]\w*|formerly)\b|\bused to (be|have|return|throw|take|accept|give)\b|\breplaces Jyro",
        RegexOptions.Compiled);

    // JYRO-DEF-834: prose that tells a history instead of the current behavior. Each pattern is a phrase of the
    // narratives found in the shipped files; passive uses such as "is used to compare" are not matched.
    private static readonly Regex History = new(
        @"\b(This|It|They|These|Those|That|Which|Both|There|Each) used to\b"
        + @"|\b[Hh]ad no (counterpart|source|script|precompiled)\b"
        + @"|\b[Kk]ept rather than\b|\bmeasured as\b|\bwrite-only\b"
        + @"|\b(was|were) (also )?(documentation-only|the exception|added|removed|renamed|replaced|changed|moved|introduced)\b"
        + @"|\buntil (named|\w+-argument)\b"
        + @"|\bthis completes\b|\bsmuggled\b|\b(is|are|was|were) no longer\b|\bbefore this change\b"
        + @"|\bthis file\b|\bof the book\b|\bthe builder\b|\bthe old\b|\bformer(ly)?\b",
        RegexOptions.Compiled);

    // A name written as a code word: two or more humps (GetPropertyLiteral) or a leading lower-case hump (toDouble).
    private static readonly Regex CodeWord = new(
        @"\b[A-Z][a-z0-9]+(?:[A-Z][a-z0-9]*)+\b", RegexOptions.Compiled);
    private static readonly Regex QualifiedName = new(@"\b([A-Z][A-Za-z0-9_]*)\.([A-Za-z_][A-Za-z0-9_]*)\b", RegexOptions.Compiled);

    // Words of ordinary prose or of other products that are written with humps.
    private static readonly HashSet<string> NotCode = new(StringComparer.Ordinal)
    {
        "JavaScript", "TypeScript", "NuGet", "GitHub", "PowerShell", "NaN", "FSharp", "McCabe", "IntelliSense", "WebAssembly", "McDonald",
        "ParamArray", "AmountNotPositive", "ToString", "GetHashCode", "Equals", "GetType", "CompareTo", "IEquatable", "IComparable", "DateTime", "TimeSpan",
    };

    public static readonly HashSet<string> Acronyms = new(StringComparer.Ordinal)
    {
        "JSON", "UTF", "ASCII", "API", "URL", "URI", "XML", "UTC", "IEEE", "ISO", "RFC", "BOM", "DLL", "CLR", "SDK", "GUID", "UUID", "BCL",
    };

    private const BindingFlags AllMembers =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;

    private readonly Dictionary<string, Type> types = new(StringComparer.Ordinal);

    // Simple type name (no arity) -> the package's types of that name; and every name a reader of the shipped
    // documentation may use: public types and public or protected members of the package, plus the BCL's public types.
    private readonly Dictionary<string, List<Type>> bySimpleName = new(StringComparer.Ordinal);
    private readonly HashSet<string> knownNames = new(StringComparer.Ordinal);

    private static string Simple(string name)
    {
        var tick = name.IndexOf('`');
        return tick >= 0 ? name.Substring(0, tick) : name;
    }

    public XmlDocScanner(IEnumerable<Assembly> assemblies)
    {
        foreach (var assembly in assemblies)
            foreach (var type in assembly.GetTypes())
                if (type.FullName is not null)
                {
                    types[type.FullName.Replace('+', '.')] = type;
                    var simple = Simple(type.Name);
                    if (!bySimpleName.TryGetValue(simple, out var list)) bySimpleName[simple] = list = new List<Type>();
                    list.Add(type);
                    if (Visible(type))
                    {
                        knownNames.Add(simple);
                        foreach (var m in type.GetMembers(AllMembers))
                            if (Reachable(m)) knownNames.Add(Simple(m.Name));
                    }
                }
        // The BCL and the other libraries the package builds on, as loaded.
        foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies().Where(a => !a.IsDynamic))
        {
            Type[] exported;
            try { exported = assembly.GetExportedTypes(); }
            catch (Exception) { continue; }
            foreach (var type in exported)
            {
                knownNames.Add(Simple(type.Name));
                if (assembly == typeof(object).Assembly)
                    foreach (var m in type.GetMembers(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static))
                        knownNames.Add(Simple(m.Name));
            }
        }
        // Script function names (Where, DateAdd) are the standard library's, written as code in the messages.
        foreach (var f in RealTree.Jyro.StdlibRegistry.GetAll()) knownNames.Add(f.Name);
    }

    // Names written as code in a block that the package does not have, or has only as internals (JYRO-DEF-834).
    private IEnumerable<string> UnknownNames(string id, string text)
    {
        foreach (Match m in QualifiedName.Matches(text))
        {
            if (!bySimpleName.TryGetValue(m.Groups[1].Value, out var owners)) continue;
            var member = m.Groups[2].Value;
            var reachable = owners.Any(o => Visible(o)
                && (o.GetMembers(AllMembers).Any(x => x.Name == member && Reachable(x))
                    || o.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic).Any(n => n.Name == member && Visible(n))));
            if (!reachable) yield return $"internal-name: {id}: {m.Value}";
        }
        foreach (Match m in CodeWord.Matches(text))
            if (!knownNames.Contains(m.Value) && !NotCode.Contains(m.Value))
                yield return $"unknown-name: {id}: {m.Value}";
    }

    private static bool Visible(Type type)
    {
        for (var t = type; t is not null; t = t.DeclaringType)
        {
            var ok = t.IsNested ? t.IsNestedPublic || t.IsNestedFamily || t.IsNestedFamORAssem : t.IsPublic;
            if (!ok) return false;
        }
        return true;
    }

    private static bool Reachable(MemberInfo m) => m switch
    {
        MethodBase b => b.IsPublic || b.IsFamily || b.IsFamilyOrAssembly,
        FieldInfo f => f.IsPublic || f.IsFamily || f.IsFamilyOrAssembly,
        PropertyInfo p => (p.GetMethod?.IsPublic ?? false) || (p.GetMethod?.IsFamily ?? false) || (p.GetMethod?.IsFamilyOrAssembly ?? false)
                          || (p.SetMethod?.IsPublic ?? false) || (p.SetMethod?.IsFamily ?? false) || (p.SetMethod?.IsFamilyOrAssembly ?? false),
        EventInfo e => (e.AddMethod?.IsPublic ?? false) || (e.AddMethod?.IsFamily ?? false) || (e.AddMethod?.IsFamilyOrAssembly ?? false),
        Type t => Visible(t),
        _ => false,
    };

    private enum Resolution { Missing, NotReachable, Reachable }

    // Resolves a documentation id (T:, M:, P:, F:, E:) against the assemblies' metadata.
    private Resolution Resolve(string id)
    {
        if (id.Length < 3 || id[1] != ':') return Resolution.Missing;
        var kind = id[0];
        var body = id.Substring(2);
        var paren = body.IndexOf('(');
        if (paren >= 0) body = body.Substring(0, paren);

        if (kind == 'T')
            return types.TryGetValue(body, out var type)
                ? (Visible(type) ? Resolution.Reachable : Resolution.NotReachable)
                : Resolution.Missing;

        var dot = body.LastIndexOf('.');
        if (dot < 0) return Resolution.Missing;
        var typeName = body.Substring(0, dot);
        var memberName = Regex.Replace(body.Substring(dot + 1), @"``\d+$", "");
        if (!types.TryGetValue(typeName, out var owner)) return Resolution.Missing;

        // A union case or a nested type documented with a member id.
        if (types.TryGetValue(body, out var nested))
            return Visible(nested) ? Resolution.Reachable : Resolution.NotReachable;

        IEnumerable<MemberInfo> candidates = memberName switch
        {
            "#ctor" => owner.GetConstructors(AllMembers),
            "#cctor" => owner.GetConstructors(AllMembers),
            _ => owner.GetMember(memberName, AllMembers),
        };
        var found = candidates.ToList();
        if (found.Count == 0) return Resolution.Missing;
        if (!Visible(owner)) return Resolution.NotReachable;
        return found.Any(Reachable) ? Resolution.Reachable : Resolution.NotReachable;
    }

    // JYRO-REQ-2180: the exception covers the standard library function classes and their members.
    private static bool IsExempt(string id, ISet<string> exemptTypeIds)
    {
        if (exemptTypeIds.Contains(id)) return true;
        if (id.Length < 3) return false;
        var body = id.Substring(2);
        var paren = body.IndexOf('(');
        if (paren >= 0) body = body.Substring(0, paren);
        return exemptTypeIds.Any(t => body.StartsWith(t.Substring(2) + ".", StringComparison.Ordinal));
    }

    // The problems of one documentation file. exemptTypeIds are the "T:" ids that JYRO-ADR-208 lets a file document
    // although the type is not public; no other id is exempt.
    public List<string> Scan(XDocument doc, ISet<string> exemptTypeIds)
    {
        var problems = new List<string>();
        foreach (var member in doc.Descendants("member"))
        {
            var id = (string?)member.Attribute("name") ?? "";
            var raw = member.ToString();
            var text = string.Concat(member.DescendantNodes().OfType<XText>().Select(t => t.Value));

            foreach (Match m in Identifier.Matches(raw)) problems.Add($"identifier: {id}: {m.Value}");
            foreach (Match m in ClauseNumber.Matches(text)) problems.Add($"clause: {id}: {m.Value}");
            if (text.Contains('`')) problems.Add($"backtick: {id}");
            foreach (Match m in CapitalWord.Matches(text))
                if (!Acronyms.Contains(m.Value)) problems.Add($"capitals: {id}: {m.Value}");
            foreach (Match m in Denylisted.Matches(text)) problems.Add($"former: {id}: {m.Value}");
            foreach (Match m in History.Matches(text)) problems.Add($"history: {id}: {m.Value}");
            problems.AddRange(UnknownNames(id, text));

            var resolution = Resolve(id);
            if (resolution == Resolution.Missing)
                problems.Add($"missing: {id}: the package has no such type or member");
            else if (resolution == Resolution.NotReachable && !IsExempt(id, exemptTypeIds))
                problems.Add($"not-public: {id}");

            foreach (var cref in member.Descendants().Select(e => (string?)e.Attribute("cref")).Where(c => c is not null))
            {
                if (cref!.StartsWith("!:", StringComparison.Ordinal))
                    problems.Add($"cref: {id}: {cref} does not resolve");
                else if (cref.Length > 2 && cref[1] == ':' && cref.Substring(2).StartsWith("RealTree.Jyro", StringComparison.Ordinal)
                         && Resolve(cref) == Resolution.Missing)
                    problems.Add($"cref: {id}: {cref} names nothing the package has");
            }
        }
        return problems;
    }
}

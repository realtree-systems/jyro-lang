using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using Xunit;

namespace JyroHostSurface;

// The public surface of all five shipped assemblies, as a checked-in inventory.
//
// No compiler can tell you a public member is unused - its consumers are
// outside the compilation by definition - so the enforceable half of "keep the
// surface intentional" is the other direction: nothing joins or leaves it by
// accident. This is the F#-native stand-in for Microsoft.CodeAnalysis
// .PublicApiAnalyzers, which only runs on C# projects.
//
// Adding a public member, renaming one, changing a parameter type, or
// renumbering a MessageCode all fail this test until PublicApi.approved.txt is
// updated in the same commit. That makes the surface diff visible in review,
// which is where a compatibility commitment should be decided rather than
// noticed later.
//
// To accept a deliberate change:
//
//     JYRO_APPROVE_API=1 dotnet test tests/Jyro.CSharp.Tests
//
// then read the diff to PublicApi.approved.txt before committing it. The
// rewrite is the easy part; the diff is the point.
public class PublicApiSurface
{
    // All five, in the order the package layers them. Loaded by name rather
    // than through typeof(...) because Jyro.Parser deliberately exports no
    // public type at all - there is no type to name - and that emptiness is
    // itself part of what this test pins.
    private static readonly string[] ShippedAssemblies =
        { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private const BindingFlags Declared =
        BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;

    [Fact]
    public void ThePublicSurfaceMatchesTheApprovedInventory()
    {
        var actual = DescribeSurface();
        var path = ApprovedFilePath();

        if (Environment.GetEnvironmentVariable("JYRO_APPROVE_API") == "1")
        {
            File.WriteAllText(path, string.Join(Environment.NewLine, actual) + Environment.NewLine);
            return;
        }

        Assert.True(File.Exists(path), $"The approved inventory is missing: {path}");

        var approved = File.ReadAllLines(path).Where(l => l.Length > 0).ToList();
        var added = OwnedLines(actual).Except(OwnedLines(approved), StringComparer.Ordinal).ToList();
        var removed = OwnedLines(approved).Except(OwnedLines(actual), StringComparer.Ordinal).ToList();

        Assert.True(added.Count == 0 && removed.Count == 0, Explain(added, removed));
    }

    [Fact]
    public void ThePublicSurfaceNamesNoFSharpType()
    {
        // Measured directly rather than trusted from the inventory, so this
        // still holds if the inventory is regenerated carelessly. An
        // FSharpOption or FSharpList on a member a C# host must call is the
        // defect this guards: it was true of JyroResult.Value until that type
        // became a sealed class, and it is the kind of thing an F# author adds
        // without noticing, because from F# it reads perfectly well.
        var offenders = DescribeSurface()
            .Where(line => line.Contains("FSharp", StringComparison.Ordinal))
            .ToList();

        Assert.True(offenders.Count == 0,
            "These public members name an F# type, which a C# host cannot use without "
            + "referencing FSharp.Core:" + Environment.NewLine
            + string.Join(Environment.NewLine, offenders));
    }

    // -- Rendering -----------------------------------------------------------

    private static List<string> DescribeSurface()
    {
        var lines = new List<string>();
        foreach (var name in ShippedAssemblies)
        {
            var assembly = Assembly.Load(name);
            var types = assembly.GetExportedTypes()
                .OrderBy(t => t.FullName, StringComparer.Ordinal)
                .ToList();

            lines.Add($"## {name}  ({types.Count} public types)");

            foreach (var type in types)
            {
                lines.Add(DescribeType(type));
                foreach (var member in DescribeMembers(type).OrderBy(m => m, StringComparer.Ordinal))
                    lines.Add("    " + member);
            }
        }
        return lines;
    }

    private static string DescribeType(Type type)
    {
        var kind = type.IsEnum ? "enum"
            : type.IsInterface ? "interface"
            : type.IsValueType ? "struct"
            : "class";

        var modifiers = new List<string>();
        if (!type.IsEnum && !type.IsInterface && !type.IsValueType)
        {
            if (type.IsAbstract && type.IsSealed) modifiers.Add("static");
            else
            {
                if (type.IsAbstract) modifiers.Add("abstract");
                if (type.IsSealed) modifiers.Add("sealed");
            }
        }

        var declaration = $"{string.Join(" ", modifiers.Append(kind))} {TypeName(type)}";

        // The base type and interfaces are surface too: a host can hold a
        // JyroString as a JyroValue, or pass its own IJyroFunction.
        var bases = new List<string>();
        if (type.BaseType is not null && type.BaseType != typeof(object)
            && type.BaseType != typeof(ValueType) && !type.IsEnum)
            bases.Add(TypeName(type.BaseType));
        bases.AddRange(type.GetInterfaces()
            .Where(i => i.IsPublic)
            .Select(TypeName)
            .OrderBy(n => n, StringComparer.Ordinal));

        return bases.Count == 0 ? declaration : declaration + " : " + string.Join(", ", bases);
    }

    private static IEnumerable<string> DescribeMembers(Type type)
    {
        foreach (var c in type.GetConstructors(Declared))
            yield return $".ctor({Parameters(c)})";

        foreach (var m in type.GetMethods(Declared))
        {
            // JYRO-REQ-1893 declares the members of JyroFunctionSignature and Parameter "besides Equals(object),
            // GetHashCode() and ToString()": a host may be given these overrides or not, and neither is surface to approve.
            if (type.FullName is "RealTree.Jyro.JyroFunctionSignature" or "RealTree.Jyro.Parameter"
                && !m.IsStatic
                && ((m.Name == "Equals" && m.GetParameters().Length == 1 && m.GetParameters()[0].ParameterType == typeof(object))
                    || (m.Name is "GetHashCode" or "ToString" && m.GetParameters().Length == 0)))
                continue;

            // Property and event accessors are reported through the property
            // or event itself; listing both would double every line.
            if (m.IsSpecialName && (m.Name.StartsWith("get_", StringComparison.Ordinal)
                                    || m.Name.StartsWith("set_", StringComparison.Ordinal)
                                    || m.Name.StartsWith("add_", StringComparison.Ordinal)
                                    || m.Name.StartsWith("remove_", StringComparison.Ordinal)))
                continue;
            var modifier = m.IsStatic ? "static " : "";
            yield return $"{modifier}{m.Name}({Parameters(m)}) : {TypeName(m.ReturnType)}";
        }

        foreach (var p in type.GetProperties(Declared))
        {
            // Only a public accessor is surface: a property with an internal setter is read-only to a host
            // (JYRO-REQ-1873, JYRO-REQ-1927), and CanWrite is true for it.
            var canRead = p.GetGetMethod() is not null;
            var canWrite = p.GetSetMethod() is not null;
            var accessors = (canRead ? "get" : "") + (canRead && canWrite ? " " : "") + (canWrite ? "set" : "");
            var modifier = (p.GetGetMethod() ?? p.GetSetMethod())!.IsStatic ? "static " : "";
            var indexer = p.GetIndexParameters().Length == 0 ? "" : $"[{Parameters(p.GetIndexParameters())}]";
            yield return $"{modifier}{p.Name}{indexer} : {TypeName(p.PropertyType)} {{ {accessors} }}";
        }

        foreach (var f in type.GetFields(Declared))
        {
            // An enum also exposes the instance field holding its backing
            // value, which has no constant to read.
            if (type.IsEnum && !f.IsLiteral) continue;

            // An enum's values carry their numbers: hosts switch on a
            // MessageCode and the numeric form appears in JMXXXX, so
            // renumbering one is a breaking change and should read as one here.
            if (type.IsEnum)
                yield return $"{f.Name} = {Convert.ToInt64(f.GetRawConstantValue(), System.Globalization.CultureInfo.InvariantCulture)}";
            else
                yield return $"{(f.IsStatic ? "static " : "")}{f.Name} : {TypeName(f.FieldType)}";
        }

        foreach (var e in type.GetEvents(Declared))
            yield return $"event {e.Name} : {TypeName(e.EventHandlerType!)}";
    }

    private static string Parameters(MethodBase method) => Parameters(method.GetParameters());

    private static string Parameters(ParameterInfo[] parameters) =>
        string.Join(", ", parameters.Select(p =>
        {
            var direction = p.IsOut ? "out " : p.ParameterType.IsByRef ? "ref " : "";
            return $"{direction}{TypeName(p.ParameterType)}";
        }));

    private static string TypeName(Type type)
    {
        if (type.IsByRef) return TypeName(type.GetElementType()!);
        if (type.IsArray) return TypeName(type.GetElementType()!) + "[]";
        if (type.IsGenericType)
        {
            var stem = type.Name.Split('`')[0];
            var args = string.Join(", ", type.GetGenericArguments().Select(TypeName));
            return $"{Qualifier(type)}{stem}<{args}>";
        }
        return Qualifier(type) + type.Name;
    }

    // Namespaces are dropped for System and RealTree.Jyro - the two that would appear
    // on nearly every line - and kept for everything else, so a type arriving from
    // an unexpected assembly stands out. FSharp.Core in particular.
    private static string Qualifier(Type type) =>
        type.Namespace is null or "System" or "RealTree.Jyro" ? "" : type.Namespace + ".";

    // -- Failure reporting ---------------------------------------------------

    // Each member line prefixed with the type that declares it, for the
    // comparison only - the file keeps its indented, readable form.
    //
    // The comparison used to run on the raw lines, and `Except` is a set
    // operation over them. A member line carries no owner, so two identical
    // signatures on different types were one element: adding
    // `CheckExecutionLimits() : Void` to `JyroResourceLimiter` changed nothing
    // the diff could see, because `JyroExecutionContext` already had a member
    // that rendered the same way. Three additions were reported as one, and
    // one of them was reported not at all. The approved file was written
    // correctly either way - what failed was the report that is meant to send
    // a reviewer to look at it.
    private static List<string> OwnedLines(IEnumerable<string> lines)
    {
        var owned = new List<string>();
        var owner = "";
        foreach (var line in lines)
        {
            if (line.StartsWith("    ", StringComparison.Ordinal))
                owned.Add(owner + " . " + line.Trim());
            else
            {
                owner = line.Trim();
                owned.Add(owner);
            }
        }
        return owned;
    }

    private static string Explain(List<string> added, List<string> removed)
    {
        var report = new StringBuilder();
        report.AppendLine("The public surface no longer matches PublicApi.approved.txt.");
        report.AppendLine();

        if (added.Count > 0)
        {
            report.AppendLine($"Added ({added.Count}) - new public surface is a new compatibility commitment:");
            foreach (var line in added) report.AppendLine("  + " + line.Trim());
            report.AppendLine();
        }

        if (removed.Count > 0)
        {
            report.AppendLine($"Removed ({removed.Count}) - this breaks any host using it:");
            foreach (var line in removed) report.AppendLine("  - " + line.Trim());
            report.AppendLine();
        }

        report.AppendLine("If the change is intended, re-approve it and review the diff:");
        report.AppendLine("  JYRO_APPROVE_API=1 dotnet test tests/Jyro.CSharp.Tests");
        return report.ToString();
    }

    // The source path at compile time. The inventory is a reviewed artifact, so
    // it lives beside this file rather than being copied to the output, where an
    // approval run would rewrite a build artifact and lose the diff.
    private static string ApprovedFilePath([CallerFilePath] string thisFile = "") =>
        Path.Combine(Path.GetDirectoryName(thisFile)!, "PublicApi.approved.txt");
}

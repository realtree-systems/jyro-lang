using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.Json.Nodes;
using RealTree.Jyro;
using Xunit;

namespace JyroHostSurface;

// JYRO-ADR-206: every engine member that compiled code binds is resolved when the compiler loads, a stale
// name fails loudly, and the ordinary test run notices.
public class EngineBindingTests
{
    private const BindingFlags AnyStatic =
        BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static | BindingFlags.DeclaredOnly;

    // -- The binding table -----------------------------------------------------------------------------------
    //
    // The table of JYRO-ADR-206 item 1 is found by its shape, not its name: the static fields and properties of
    // Jyro.Compiler (any type, any visibility) that hold a MemberInfo, or a collection, array, tuple or pair of them.
    // Reading each forces the compiler's module initialization, which is the load of JYRO-REQ-2177.

    private static IEnumerable<Type> CompilerTypes()
    {
        try { return Assembly.Load("Jyro.Compiler").GetTypes(); }
        catch (ReflectionTypeLoadException e) { return e.Types.Where(t => t is not null)!; }
    }

    private static bool MayHoldMembers(Type t) =>
        typeof(MemberInfo).IsAssignableFrom(t)
        || (t != typeof(string) && (typeof(IEnumerable).IsAssignableFrom(t) || typeof(System.Runtime.CompilerServices.ITuple).IsAssignableFrom(t)
                                    || (t.IsGenericType && t.GetGenericTypeDefinition() == typeof(KeyValuePair<,>))));

    private static void Collect(object? value, string owner, List<MemberInfo> found, List<string> faults, int depth = 0)
    {
        if (depth > 4) return;
        switch (value)
        {
            case null:
                return;
            case Type:
                // A type is held beside the members (the type of an expression, a generic argument); it is not a
                // method, property, field or constructor that compiled code calls (JYRO-REQ-2177).
                return;
            case MemberInfo m:
                found.Add(m);
                return;
            case string:
                return;
            case System.Runtime.CompilerServices.ITuple tuple:
                for (var i = 0; i < tuple.Length; i++) Collect(tuple[i], owner, found, faults, depth + 1);
                return;
            case IEnumerable seq:
                foreach (var item in seq) Collect(item, owner, found, faults, depth + 1);
                return;
            default:
                var type = value.GetType();
                if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(KeyValuePair<,>))
                {
                    Collect(type.GetProperty("Key")!.GetValue(value), owner, found, faults, depth + 1);
                    Collect(type.GetProperty("Value")!.GetValue(value), owner, found, faults, depth + 1);
                }
                return;
        }
    }

    private static (List<MemberInfo> Members, List<string> Faults) ReadTable()
    {
        var found = new List<MemberInfo>();
        var faults = new List<string>();
        foreach (var type in CompilerTypes())
        {
            foreach (var f in type.GetFields(AnyStatic).Where(f => MayHoldMembers(f.FieldType)))
            {
                try
                {
                    var value = f.GetValue(null);
                    if (value is null && typeof(MemberInfo).IsAssignableFrom(f.FieldType))
                        faults.Add(type.FullName + "." + f.Name + " is null");
                    Collect(value, type.FullName + "." + f.Name, found, faults);
                }
                catch (Exception e) { faults.Add(type.FullName + "." + f.Name + ": " + (e.InnerException ?? e).Message); }
            }
            foreach (var p in type.GetProperties(AnyStatic).Where(p => p.GetIndexParameters().Length == 0 && p.GetMethod is not null && MayHoldMembers(p.PropertyType)))
            {
                try
                {
                    var value = p.GetValue(null);
                    if (value is null && typeof(MemberInfo).IsAssignableFrom(p.PropertyType))
                        faults.Add(type.FullName + "." + p.Name + " is null");
                    Collect(value, type.FullName + "." + p.Name, found, faults);
                }
                catch (Exception e) { faults.Add(type.FullName + "." + p.Name + ": " + (e.InnerException ?? e).Message); }
            }
        }
        return (found.Distinct().ToList(), faults);
    }

    private static string Describe(MemberInfo m) => m.DeclaringType?.FullName + "." + m.Name;

    [Fact(DisplayName = "JYRO-REQ-2178 every engine member that compiled code calls resolves: none is missing, ambiguous or null")]
    [Trait("requirement", "JYRO-REQ-2178")]
    [Trait("requirement", "JYRO-REQ-2177")]
    public void EveryEngineBindingResolves()
    {
        var (members, faults) = ReadTable();

        Assert.True(faults.Count == 0, "A binding failed to resolve (type.member): " + string.Join("; ", faults));
        // JYRO-ADR-206 counts about forty lookups; a table this test cannot find, or finds nearly empty, proves nothing.
        Assert.True(members.Count >= 30, "Only " + members.Count + " engine members were found in static fields of Jyro.Compiler");
        Assert.All(members, m => Assert.NotNull(m.DeclaringType));

        var own = members.Where(m => m.DeclaringType!.Assembly.GetName().Name?.StartsWith("Jyro.", StringComparison.Ordinal) == true).ToList();
        Assert.True(own.Count >= 20, "Only " + own.Count + " of the members are Jyro's own");
        var internals = own.Where(IsNotPublic).ToList();
        Assert.True(internals.Count >= 10, "Only " + internals.Count + " of the members are internal (JYRO-ADR-191 binds internal members)");
        Assert.Contains(own, m => m.DeclaringType!.Assembly == typeof(JyroValue).Assembly);
    }

    private static bool IsNotPublic(MemberInfo m) => m switch
    {
        MethodBase b => !b.IsPublic || !b.DeclaringType!.IsVisible,
        FieldInfo f => !f.IsPublic || !f.DeclaringType!.IsVisible,
        PropertyInfo p => !(p.GetMethod?.IsPublic ?? p.SetMethod?.IsPublic ?? false) || !p.DeclaringType!.IsVisible,
        _ => false,
    };

    // -- A stale name fails when the compiler loads, whatever the script ---------------------------------------

    private static readonly string[] Five = { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static string Run(string fileName, string arguments, string workingDirectory)
    {
        var psi = new ProcessStartInfo(fileName, arguments)
        {
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        // The build of the test run may redirect its outputs; the host built here goes where it is told.
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
        return stdout.Result + "\n" + stderr.Result + "\nexit " + process.ExitCode;
    }

    // A console host that compiles Output.x = 1 against the five shipped assemblies, built once.
    private static readonly Lazy<string> HostDirectory = new(() => BuildHost("""
            using System.Reflection;
            using System.Runtime.CompilerServices;
            using RealTree.Jyro;
            // Load the compiler: run the static initialization of every type of Jyro.Compiler.
            foreach (var type in Assembly.Load("Jyro.Compiler").GetTypes())
            {
                try { RuntimeHelpers.RunClassConstructor(type.TypeHandle); }
                catch (Exception e) { Console.WriteLine("LOADFAILED " + type.FullName + ": " + e); }
            }
            try
            {
                var r = new JyroCompiler().WithSource("Output.x = 1").WithStdlib().Compile();
                if (r.IsSuccess) Console.WriteLine("COMPILED");
                else foreach (var m in r.Messages) Console.WriteLine("DIAGNOSTIC " + m.Message);
            }
            catch (Exception e) { Console.WriteLine("EXCEPTION " + e); }
            """));

    // JYRO-DEF-833: a host that only compiles, as every host does. Nothing forces the load of the compiler first, so
    // what a stale name does is what the host sees from Compile(): the exception type and message as the host reads
    // them (not the inner exceptions), and the code and message of each diagnostic.
    private static readonly Lazy<string> CompileOnlyHostDirectory = new(() => BuildHost("""
            using RealTree.Jyro;
            try
            {
                var r = new JyroCompiler().WithSource("Output.x = 1").WithStdlib().Compile();
                if (r.IsSuccess) Console.WriteLine("COMPILED");
                else foreach (var m in r.Messages) Console.WriteLine("DIAGNOSTIC " + m.Code + " " + m.Message);
            }
            catch (Exception e) { Console.WriteLine("THROWN " + e.GetType().Name + " " + e.Message); }
            """));

    private static string BuildHost(string program)
    {
        var root = Path.Combine(Path.GetTempPath(), "jyro-binding-host-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var references = string.Join("\n", Five.Append("FSharp.Core").Select(n =>
            "<Reference Include=\"" + n + "\"><HintPath>" + Assembly.Load(n).Location + "</HintPath></Reference>"));
        File.WriteAllText(Path.Combine(root, "Host.csproj"),
            "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework>"
            + "<Nullable>disable</Nullable><ImplicitUsings>enable</ImplicitUsings></PropertyGroup><ItemGroup>" + references + "</ItemGroup></Project>");
        File.WriteAllText(Path.Combine(root, "Program.cs"), program);
        var built = Run("dotnet", "build \"" + Path.Combine(root, "Host.csproj") + "\" -c Release -o \"" + Path.Combine(root, "out") + "\" --nologo -v q", root);
        Assert.True(File.Exists(Path.Combine(root, "out", "Host.dll")), "The host did not build:\n" + built);
        return Path.Combine(root, "out");
    }

    // Runs the host over a copy of the shipped assemblies, after the callback has changed the copy.
    private static string RunHost(Func<string, bool> changeCopy, Lazy<string>? host = null)
    {
        var hostDirectory = (host ?? HostDirectory).Value;
        var copy = Path.Combine(Path.GetTempPath(), "jyro-binding-run-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(copy);
        try
        {
            foreach (var file in Directory.GetFiles(hostDirectory))
                File.Copy(file, Path.Combine(copy, Path.GetFileName(file)));
            if (!changeCopy(copy)) return "NOT CHANGED";
            return Run("dotnet", "\"" + Path.Combine(copy, "Host.dll") + "\"", copy);
        }
        finally { try { Directory.Delete(copy, true); } catch (IOException) { } }
    }

    // Renames the member in the metadata of its declaring assembly: the last character of its name, in the string
    // heap, becomes '~'. False when the name has no entry of its own in the heap.
    private static bool Rename(string dll, string name)
    {
        var bytes = File.ReadAllBytes(dll);
        var needle = Encoding.UTF8.GetBytes(name);
        for (var i = 1; i + needle.Length < bytes.Length; i++)
        {
            if (bytes[i - 1] != 0 || bytes[i + needle.Length] != 0) continue;
            if (!bytes.AsSpan(i, needle.Length).SequenceEqual(needle)) continue;
            bytes[i + needle.Length - 1] = (byte)'~';
            File.WriteAllBytes(dll, bytes);
            return true;
        }
        return false;
    }

    [Fact(DisplayName = "JYRO-REQ-2177 with every member present the compiler loads and compiles a script")]
    [Trait("requirement", "JYRO-REQ-2177")]
    public void TheControlCompiles()
    {
        var output = RunHost(_ => true);
        Assert.Contains("COMPILED", output);
    }

    [Fact(DisplayName = "JYRO-REQ-2177 a stale name of a bound member fails the compilation of a script that does not use it, naming the declaring type and the member")]
    [Trait("requirement", "JYRO-REQ-2177")]
    [Trait("requirement", "JYRO-REQ-2178")]
    public void AStaleNameFailsLoudly()
    {
        var (members, _) = ReadTable();
        var core = typeof(JyroValue).Assembly;
        var names = members.GroupBy(m => m.Name).ToDictionary(g => g.Key, g => g.Count());
        // Members of Jyro.Core that compiled code calls by name, each name once in the table, spread over the table.
        var candidates = members
            .Where(m => m.DeclaringType!.Assembly == core && names[m.Name] == 1 && m.Name.Length > 4 && !m.Name.StartsWith(".", StringComparison.Ordinal))
            .Where(m => m is MethodInfo or PropertyInfo or FieldInfo)
            .ToList();
        Assert.True(candidates.Count >= 3, "Only " + candidates.Count + " table members of Jyro.Core have a name of their own");

        var tried = 0;
        var failures = new List<string>();
        for (var i = 0; i < candidates.Count && tried < 3; i += Math.Max(1, candidates.Count / 3))
        {
            var member = candidates[i];
            var output = RunHost(dir => Rename(Path.Combine(dir, "Jyro.Core.dll"), member.Name));
            if (output == "NOT CHANGED") continue;
            tried++;
            if (output.Contains("COMPILED", StringComparison.Ordinal)
                || !output.Contains(member.Name, StringComparison.Ordinal)
                || !output.Contains(member.DeclaringType!.Name, StringComparison.Ordinal))
                failures.Add(Describe(member) + " renamed gave:\n" + output);
        }
        Assert.True(tried >= 2, "Fewer than two members could be renamed in the metadata");
        Assert.True(failures.Count == 0, string.Join("\n---\n", failures));
    }

    [Fact(DisplayName = "JYRO-REQ-2177 a host that only compiles sees a stale engine member as a failure naming the declaring type and the member, not UnknownCompilerError")]
    [Trait("requirement", "JYRO-REQ-2177")]
    public void AStaleNameSurfacesNamingTypeAndMemberToACompileOnlyHost()
    {
        Assert.Contains("COMPILED", RunHost(_ => true, CompileOnlyHostDirectory));

        var (members, _) = ReadTable();
        var core = typeof(JyroValue).Assembly;
        var names = members.GroupBy(m => m.Name).ToDictionary(g => g.Key, g => g.Count());
        var candidates = members
            .Where(m => m.DeclaringType!.Assembly == core && names[m.Name] == 1 && m.Name.Length > 4 && !m.Name.StartsWith(".", StringComparison.Ordinal))
            .Where(m => m is MethodInfo or PropertyInfo or FieldInfo)
            .ToList();
        Assert.True(candidates.Count >= 3);

        var tried = 0;
        var failures = new List<string>();
        for (var i = 0; i < candidates.Count && tried < 3; i += Math.Max(1, candidates.Count / 3))
        {
            var member = candidates[i];
            var output = RunHost(dir => Rename(Path.Combine(dir, "Jyro.Core.dll"), member.Name), CompileOnlyHostDirectory);
            if (output == "NOT CHANGED") continue;
            tried++;
            if (output.Contains("COMPILED", StringComparison.Ordinal)
                || output.Contains("UnknownCompilerError", StringComparison.Ordinal)
                || output.Contains("TypeInitializationException", StringComparison.Ordinal)
                || !output.Contains(member.Name, StringComparison.Ordinal)
                || !output.Contains(member.DeclaringType!.Name, StringComparison.Ordinal))
                failures.Add(Describe(member) + " renamed gave:\n" + output);
        }
        Assert.True(tried >= 2, "Fewer than two members could be renamed in the metadata");
        Assert.True(failures.Count == 0, string.Join("\n---\n", failures));
    }

    // -- JYRO-REQ-2178 (2): one script with every statement form and every expression form --------------------

    private const string EveryForm = """
        union Shape
            Circle(r)
            Rect(w, h)
            Dot
        end

        func Fact(n: number, acc = 1)
            if n <= 1 then
                return acc
            end
            return Fact(n - 1, acc * n)
        end

        func Area(s)
            return match s do
                case Circle(r) then r * 2
                case Rect(w, h) then w * h
                case Dot then 0
            end
        end

        # variables, type hints, every assignment operator
        var a = 10
        a += 5
        a -= 3
        a *= 2
        a /= 4
        a %= 4
        Output.assign = a
        var s = "a"
        s &= "b"
        Output.concat = s & "c"
        var hinted: number = "5"
        Output.hint = hinted
        var unset = null
        Output.unset = unset == null

        # increments and decrements, prefix and postfix
        var i = 0
        i++
        ++i
        i--
        --i
        i++
        i++
        Output.inc = i
        var arr = [10, 20]
        arr[1] += 1
        var obj = {x: 1, "y z": 2, k: 3}
        obj.x += 1
        Output.idx = arr
        Output.objx = obj.x
        Output.brk = obj["k"]

        # delete: a property by dot and by index
        delete obj["y z"]
        var del = {a: 1, b: 2, c: 3}
        delete del.a
        delete del["b"]
        var d = [1, 2, 3, 4]
        Output.deleted = [Length(Keys(obj)), HasProperty(obj, "y z"), del, d]

        # loops: for ascending, descending, with step; foreach over an array and over keys; while with break and continue
        var total = 0
        for q in 0 to 5 do
            total += q
        end
        var desc = ""
        for j in 3 downto 0 do
            desc = desc & j
        end
        var stepped = 0
        for k in 0 to 10 by 3 do
            stepped += k
        end
        var each = 0
        foreach item in d do
            each += item
        end
        var keyCount = 0
        foreach key in Keys(obj) do
            keyCount += 1
        end
        var wi = 0
        var wsum = 0
        while true do
            wi += 1
            if wi > 6 then
                break
            end
            if wi % 2 == 0 then
                continue
            end
            wsum += wi
        end
        Output.loops = [total, desc, stepped, each, keyCount, wsum]

        # if, elseif, else; switch with a multi-value case and a default
        var grade = ""
        var score = 85
        if score >= 90 then
            grade = "A"
        elseif score >= 80 then
            grade = "B"
        else
            grade = "F"
        end
        var sw = ""
        switch "b" do
            case "a", "b" then
                sw = "ab"
            case "c" then
                sw = "c"
            default then
                sw = "other"
        end
        Output.branches = [grade, sw]

        # match as a statement and as an expression, a function with a match, named arguments and a default
        var shape = Rect(3, 4)
        var ms = 0
        match shape do
            case Circle(rad) then
                ms = rad
            case Rect(wd, ht) then
                ms = wd * ht
            case Dot then
                ms = 0
        end
        var me = match shape do
            case Circle(rad) then 1
            case Rect(wd, ht) then wd + ht
            case Dot then 0
        end
        Output.matches = [ms, me, Area(Circle(5)), Area(Dot()), Fact(n: 5), Fact(4, 1)]

        # expressions: literals, operators, ternary, ??, is, and, or, not, lambdas, indexing
        Output.lit = [0xFF, 0b101, 'single', "dq", true, false, null]
        Output.arith = [7 + 3, 7 - 3, 7 * 3, 7 / 2, 7 % 4, -7]
        Output.cmp = [1 < 2, 2 <= 2, 3 > 2, 3 >= 4, 1 == 1, 1 != 1]
        Output.logic = [true and not false, false or 1 < 2, not true, 5 is number, "x" is not number, 5 is string]
        Output.tern = 3 > 2 ? "yes" : "no"
        Output.coal = [obj.nothing ?? 7, 0 ?? 9]
        Output.text = "n=" & 5
        Output.lambdas = [Map([1, 2, 3], x => x * 2), Reduce([1, 2, 3], (acc, n) => acc + n, 0), Where([1, 2, 3], x => x > 1)]
        Output.deep = {}
        Output.deep.inner = {}
        Output.deep.inner.v = [1, 2][1] + {p: 4}.p

        # fail in a branch that is not taken, then a clean exit
        if score < 0 then
            fail "never"
        end
        exit
        """;

    private const string EveryFormOutput = """
        {
          "assign": 2,
          "concat": "abc",
          "hint": 5,
          "unset": true,
          "inc": 2,
          "idx": [10, 21],
          "objx": 2,
          "brk": 3,
          "deleted": [2, false, { "c": 3 }, [1, 2, 3, 4]],
          "loops": [10, "321", 18, 10, 2, 9],
          "branches": ["B", "ab"],
          "matches": [12, 7, 10, 0, 120, 24],
          "lit": [255, 5, "single", "dq", true, false, null],
          "arith": [10, 4, 21, 3.5, 3, -7],
          "cmp": [true, true, true, false, true, false],
          "logic": [true, true, false, true, true, false],
          "tern": "yes",
          "coal": [7, 0],
          "text": "n=5",
          "lambdas": [[2, 4, 6], 6, [2, 3]],
          "deep": { "inner": { "v": 6 } }
        }
        """;

    [Fact(DisplayName = "JYRO-REQ-2178 one script that uses every statement form and every expression form compiles with no diagnostic and gives its Output")]
    [Trait("requirement", "JYRO-REQ-2178")]
    public void EveryFormCompilesAndRuns()
    {
        var compiled = new JyroCompiler().WithSource(EveryForm).WithStdlib().Compile();
        Assert.True(compiled.IsSuccess, "Diagnostics: " + string.Join("; ", compiled.Messages.Select(m => m.Message)));
        Assert.Empty(compiled.Messages);

        var result = new JyroRunner(compiled.GetValueOrThrow()).WithInput(new JyroObject()).Execute();
        Assert.True(result.IsSuccess, "Run messages: " + string.Join("; ", result.Messages.Select(m => m.Code + " " + m.Message)));
        Assert.True(result.TryGetValue(out var output));
        var actual = JsonNode.Parse(output.ToJson())!;
        var expected = JsonNode.Parse(EveryFormOutput)!;
        Assert.True(JsonNode.DeepEquals(expected, actual), "Output was " + actual.ToJsonString());
    }
}

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using RealTree.Jyro;
using Xunit;

// DiagnosticMessage as a sealed class that holds its stage (JYRO-ADR-198): the factories, equality,
// ToString, SourceLocation, StructuredDiagnostic, the end of F# record semantics on public types,
// and the success result that cannot hold an error. Written the way a C# host writes them.

namespace JyroHostSurface;

public class DiagnosticMessageFactoryTests
{
    private static readonly string[] Severities = { "Error", "Warning", "Info" };

    private static MethodInfo[] PublicStatics() =>
        typeof(DiagnosticMessage).GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly)
            .Where(m => !m.IsSpecialName)
            .ToArray();

    private static string Signature(MethodInfo m) =>
        m.Name + "(" + string.Join(", ", m.GetParameters().Select(p => p.ParameterType.Name + (p.GetCustomAttribute<ParamArrayAttribute>() != null ? " params" : ""))) + ")";

    [Fact(DisplayName = "JYRO-REQ-1881 DiagnosticMessage has exactly the six factories Error, Warning and Info and no other public static method")]
    [Trait("requirement", "JYRO-REQ-1881")]
    public void ExactlySixFactories()
    {
        var actual = PublicStatics().Select(Signature).OrderBy(s => s, StringComparer.Ordinal).ToArray();
        var expected = new[]
        {
            "Error(MessageCode, MessageStage, Nullable`1, Object[] params)",
            "Error(MessageCode, MessageStage, Object[] params)",
            "Info(MessageCode, MessageStage, Nullable`1, Object[] params)",
            "Info(MessageCode, MessageStage, Object[] params)",
            "Warning(MessageCode, MessageStage, Nullable`1, Object[] params)",
            "Warning(MessageCode, MessageStage, Object[] params)",
        };
        Assert.Equal(expected.OrderBy(s => s, StringComparer.Ordinal).ToArray(), actual);
    }

    [Fact(DisplayName = "JYRO-REQ-1881 no overload takes a SourceLocation that is not nullable, and each factory returns a DiagnosticMessage")]
    [Trait("requirement", "JYRO-REQ-1881")]
    public void NoOverloadTakesANonNullableLocation()
    {
        foreach (var m in PublicStatics())
        {
            Assert.Equal(typeof(DiagnosticMessage), m.ReturnType);
            Assert.DoesNotContain(m.GetParameters(), p => p.ParameterType == typeof(SourceLocation));
            Assert.Contains(Severities, s => s == m.Name);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1881 Warning with a stage and a location has that severity, stage, empty Args and location")]
    [Trait("requirement", "JYRO-REQ-1881")]
    public void TheBookExample()
    {
        var d = DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Validate, new SourceLocation(3, 5, 1));
        Assert.Equal(MessageSeverity.Warning, d.Severity);
        Assert.Equal(MessageStage.Validate, d.Stage);
        Assert.Equal(MessageCode.DivisionByZero, d.Code);
        Assert.Empty(d.Args);
        Assert.True(d.Location.HasValue);
        Assert.Equal(3, d.Location!.Value.Line);
    }

    public static IEnumerable<object[]> ThreeSeverities() => new[]
    {
        new object[] { "Error", MessageSeverity.Error },
        new object[] { "Warning", MessageSeverity.Warning },
        new object[] { "Info", MessageSeverity.Info },
    };

    private static DiagnosticMessage Make(string name, MessageCode code, MessageStage stage, params object[] args) => name switch
    {
        "Error" => DiagnosticMessage.Error(code, stage, args),
        "Warning" => DiagnosticMessage.Warning(code, stage, args),
        _ => DiagnosticMessage.Info(code, stage, args),
    };

    private static DiagnosticMessage MakeAt(string name, MessageCode code, MessageStage stage, SourceLocation? at, params object[] args) => name switch
    {
        "Error" => DiagnosticMessage.Error(code, stage, at, args),
        "Warning" => DiagnosticMessage.Warning(code, stage, at, args),
        _ => DiagnosticMessage.Info(code, stage, at, args),
    };

    [Theory(DisplayName = "JYRO-REQ-1881 a factory without a location gives its severity, code, stage, args in order and no location")]
    [Trait("requirement", "JYRO-REQ-1881")]
    [MemberData(nameof(ThreeSeverities))]
    public void WithoutALocation(string name, MessageSeverity severity)
    {
        var d = Make(name, MessageCode.UndefinedFunction, MessageStage.Link, "A", 2, "C");
        Assert.Equal(severity, d.Severity);
        Assert.Equal(MessageCode.UndefinedFunction, d.Code);
        Assert.Equal(MessageStage.Link, d.Stage);
        Assert.Equal(new object[] { "A", 2, "C" }, d.Args);
        Assert.False(d.Location.HasValue);
    }

    [Theory(DisplayName = "JYRO-REQ-1881 a factory with a location gives its severity, code, stage, args in order and that location")]
    [Trait("requirement", "JYRO-REQ-1881")]
    [MemberData(nameof(ThreeSeverities))]
    public void WithALocation(string name, MessageSeverity severity)
    {
        var d = MakeAt(name, MessageCode.UndefinedFunction, MessageStage.Compile, new SourceLocation(7, 2, 4), "A", "B");
        Assert.Equal(severity, d.Severity);
        Assert.Equal(MessageStage.Compile, d.Stage);
        Assert.Equal(new object[] { "A", "B" }, d.Args);
        Assert.Equal(new SourceLocation(7, 2, 4), d.Location);
    }

    [Theory(DisplayName = "JYRO-REQ-1881 a null location gives no location, and none or null args give the empty array")]
    [Trait("requirement", "JYRO-REQ-1881")]
    [MemberData(nameof(ThreeSeverities))]
    public void NullLocationAndNullArgs(string name, MessageSeverity severity)
    {
        var none = MakeAt(name, MessageCode.DivisionByZero, MessageStage.Execute, null);
        Assert.Equal(severity, none.Severity);
        Assert.False(none.Location.HasValue);
        Assert.NotNull(none.Args);
        Assert.Empty(none.Args);

        var nullArgs = name switch
        {
            "Error" => DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, (object[])null!),
            "Warning" => DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute, (object[])null!),
            _ => DiagnosticMessage.Info(MessageCode.DivisionByZero, MessageStage.Execute, (object[])null!),
        };
        Assert.NotNull(nullArgs.Args);
        Assert.Empty(nullArgs.Args);

        var nullBoth = name switch
        {
            "Error" => DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, null, (object[])null!),
            "Warning" => DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute, null, (object[])null!),
            _ => DiagnosticMessage.Info(MessageCode.DivisionByZero, MessageStage.Execute, null, (object[])null!),
        };
        Assert.NotNull(nullBoth.Args);
        Assert.Empty(nullBoth.Args);
    }

    [Fact(DisplayName = "JYRO-REQ-1881 the stage given to a factory is the stage of the diagnostic, for each of the six stages")]
    [Trait("requirement", "JYRO-REQ-1881")]
    public void EachStageIsKept()
    {
        foreach (var stage in new[] { MessageStage.Parse, MessageStage.Validate, MessageStage.Link, MessageStage.Compile, MessageStage.Execute, MessageStage.Deserialize })
        {
            Assert.Equal(stage, DiagnosticMessage.Error(MessageCode.DivisionByZero, stage).Stage);
        }
    }
}

public class DiagnosticMessageShapeTests
{
    [Fact(DisplayName = "JYRO-REQ-1879 DiagnosticMessage is a sealed class in RealTree.Jyro with no public or protected constructor")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void SealedWithNoPublicConstructor()
    {
        var t = typeof(DiagnosticMessage);
        Assert.Equal("RealTree.Jyro", t.Namespace);
        Assert.True(t.IsClass);
        Assert.True(t.IsSealed);
        var ctors = t.GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Where(c => c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly)
            .ToList();
        Assert.Empty(ctors);
    }

    [Fact(DisplayName = "JYRO-REQ-1879 the public properties Code, Severity, Stage, Args, Location and Message have no setter")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void PropertiesHaveNoSetter()
    {
        foreach (var name in new[] { "Code", "Severity", "Stage", "Args", "Location", "Message" })
        {
            var p = typeof(DiagnosticMessage).GetProperty(name, BindingFlags.Public | BindingFlags.Instance);
            Assert.NotNull(p);
            Assert.NotNull(p!.GetGetMethod());
            Assert.Null(p.GetSetMethod(nonPublic: true));
        }
        Assert.DoesNotContain(
            typeof(DiagnosticMessage).GetProperties(BindingFlags.Public | BindingFlags.Instance),
            p => p.GetSetMethod() != null);
    }

    [Fact(DisplayName = "JYRO-REQ-1879 DiagnosticMessage implements IEquatable of itself and not IStructuralEquatable")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void Interfaces()
    {
        Assert.True(typeof(IEquatable<DiagnosticMessage>).IsAssignableFrom(typeof(DiagnosticMessage)));
        Assert.False(typeof(IStructuralEquatable).IsAssignableFrom(typeof(DiagnosticMessage)));
    }
}

public class DiagnosticMessageEqualityTests
{
    [Fact(DisplayName = "JYRO-REQ-1879 two diagnostics that differ only in stage are not Equals")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void StageDecidesEquality()
    {
        var parse = DiagnosticMessage.Error(MessageCode.UnpairedSurrogate, MessageStage.Parse);
        var execute = DiagnosticMessage.Error(MessageCode.UnpairedSurrogate, MessageStage.Execute);
        Assert.False(parse.Equals(execute));
        Assert.False(execute.Equals(parse));
        Assert.False(parse.Equals((object)execute));
        Assert.True(parse.Equals(DiagnosticMessage.Error(MessageCode.UnpairedSurrogate, MessageStage.Parse)));
    }

    [Fact(DisplayName = "JYRO-REQ-1879 the UnpairedSurrogate diagnostic of a compilation has the stage Parse, and so has each diagnostic Equals to it")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void StageOfACompilationDiagnosticSurvives()
    {
        var result = new JyroCompiler().WithSource("Output.x = \"a\uD800b\"").Compile();
        Assert.False(result.IsSuccess);
        var found = result.Messages.Where(m => m.Code == MessageCode.UnpairedSurrogate).ToList();
        Assert.NotEmpty(found);
        foreach (var d in found)
        {
            Assert.Equal(MessageStage.Parse, d.Stage);
            // Every copy a host can make through the public members keeps the stage.
            var copy = DiagnosticMessage.Error(d.Code, d.Stage, d.Location, d.Args);
            Assert.True(copy.Equals(d));
            Assert.Equal(MessageStage.Parse, copy.Stage);
            Assert.Equal(MessageStage.Parse, DiagnosticFormatter.ToStructured(d).Stage);
            var other = DiagnosticMessage.Error(d.Code, MessageStage.Execute, d.Location, d.Args);
            Assert.False(other.Equals(d));
        }
        // The stage is the same whichever reference to the diagnostic is read.
        var first = result.Messages.First(m => m.Code == MessageCode.UnpairedSurrogate);
        var again = result.Messages.First(m => m.Code == MessageCode.UnpairedSurrogate);
        Assert.Equal(first.Stage, again.Stage);
        Assert.Equal(MessageStage.Parse, again.Stage);
    }

    [Fact(DisplayName = "JYRO-REQ-1879 diagnostics are Equals when code, severity, stage and location are equal and the args have the same invariant strings")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void FieldsDecideEquality()
    {
        var at = new SourceLocation(1, 2, 3);
        var a = DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, at, "f");
        var b = DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, at, "f");
        Assert.True(a.Equals(b));
        Assert.True(a.Equals((object)b));
        Assert.Equal(a.GetHashCode(), b.GetHashCode());

        Assert.False(a.Equals(DiagnosticMessage.Error(MessageCode.UndeclaredVariable, MessageStage.Link, at, "f")));
        Assert.False(a.Equals(DiagnosticMessage.Warning(MessageCode.UndefinedFunction, MessageStage.Link, at, "f")));
        Assert.False(a.Equals(DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, new SourceLocation(1, 2, 4), "f")));
        Assert.False(a.Equals(DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, null, "f")));
        Assert.False(a.Equals(DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, at, "g")));
        Assert.False(a.Equals(DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, at, "f", "g")));
        Assert.False(a.Equals(DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, at)));
        Assert.False(a.Equals((object?)null));
    }

    [Fact(DisplayName = "JYRO-REQ-1879 args are compared position by position by their invariant string form, whatever the current culture")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void ArgsCompareByInvariantString()
    {
        var saved = CultureInfo.CurrentCulture;
        try
        {
            CultureInfo.CurrentCulture = new CultureInfo("de-DE");
            var number = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, 1.5, 2);
            var text = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, "1.5", "2");
            Assert.True(number.Equals(text));
            Assert.True(text.Equals(number));
            Assert.Equal(number.GetHashCode(), text.GetHashCode());

            var swapped = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, 2, 1.5);
            Assert.False(number.Equals(swapped));

            var withNull = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, new object?[] { null }!);
            var withEmpty = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute, "");
            Assert.True(withNull.Equals(withEmpty));
            Assert.Equal(withNull.GetHashCode(), withEmpty.GetHashCode());
        }
        finally
        {
            CultureInfo.CurrentCulture = saved;
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1879 equal diagnostics have the same hash code across many codes and stages")]
    [Trait("requirement", "JYRO-REQ-1879")]
    public void EqualMeansSameHash()
    {
        foreach (var code in Enum.GetValues<MessageCode>())
        {
            foreach (var stage in Enum.GetValues<MessageStage>())
            {
                var a = DiagnosticMessage.Error(code, stage, 1, "x");
                var b = DiagnosticMessage.Error(code, stage, "1", "x");
                Assert.True(a.Equals(b), $"{code} {stage}");
                Assert.Equal(a.GetHashCode(), b.GetHashCode());
            }
        }
    }
}

public class DiagnosticMessageToStringTests
{
    [Fact(DisplayName = "JYRO-REQ-1880 ToString of the TypedVariableNeedsInitializer diagnostic of var s: string is [JM3302] Ln 1, Col 1: and its Message")]
    [Trait("requirement", "JYRO-REQ-1880")]
    public void TheBookExampleWithALocation()
    {
        var result = new JyroCompiler().WithSource("var s: string").Compile();
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.TypedVariableNeedsInitializer);
        Assert.Equal(1, d.Location!.Value.Line);
        Assert.Equal(1, d.Location!.Value.Column);
        Assert.Equal("[JM3302] Ln 1, Col 1: " + d.Message, d.ToString());
        Assert.Equal(DiagnosticFormatter.FormatMessage(d), d.ToString());
    }

    [Fact(DisplayName = "JYRO-REQ-1880 ToString of a diagnostic made with no location is [JM5004]: boom")]
    [Trait("requirement", "JYRO-REQ-1880")]
    public void TheBookExampleWithoutALocation()
    {
        var d = DiagnosticMessage.Error(MessageCode.ScriptFailure, MessageStage.Execute, "boom");
        Assert.Equal("[JM5004]: boom", d.ToString());
        Assert.Equal(DiagnosticFormatter.FormatMessage(d), d.ToString());
    }

    [Fact(DisplayName = "JYRO-REQ-1880 a location whose Line is 0 is left out of ToString")]
    [Trait("requirement", "JYRO-REQ-1880")]
    public void LineZeroIsNoLocation()
    {
        var d = DiagnosticMessage.Error(MessageCode.ScriptFailure, MessageStage.Execute, new SourceLocation(0, 0, 0), "boom");
        Assert.Equal("[JM5004]: boom", d.ToString());
    }

    [Fact(DisplayName = "JYRO-REQ-1880 ToString gives the line and the column of a located diagnostic in decimal")]
    [Trait("requirement", "JYRO-REQ-1880")]
    public void LocatedDiagnostic()
    {
        var d = DiagnosticMessage.Error(MessageCode.ScriptFailure, MessageStage.Execute, new SourceLocation(12, 345, 6), "boom");
        Assert.Equal("[JM5004] Ln 12, Col 345: boom", d.ToString());
    }

    [Fact(DisplayName = "JYRO-REQ-1880 ToString equals FormatMessage (nothing to escape) and holds no line break for every code")]
    [Trait("requirement", "JYRO-REQ-1880")]
    public void EveryCode()
    {
        foreach (var code in Enum.GetValues<MessageCode>())
        {
            var d = DiagnosticMessage.Error(code, MessageStage.Execute, new SourceLocation(2, 3, 1), "a", "b", "c", "d", "e", "f");
            var text = d.ToString();
            Assert.Equal(DiagnosticFormatter.FormatMessage(d), text);
            Assert.StartsWith("[JM" + ((int)code).ToString("D4", CultureInfo.InvariantCulture) + "] Ln 2, Col 3: ", text);
            Assert.DoesNotContain('\n', text);
            Assert.DoesNotContain('\r', text);
        }
    }

    [Fact(DisplayName = "JYRO-DEF-805 JYRO-REQ-1880 ToString of a diagnostic whose Message holds a line feed is the 18 characters [JM5004]: a\\u000ab and holds no line break")]
    [Trait("requirement", "JYRO-REQ-1880")]
    [Trait("defect", "JYRO-DEF-805")]
    public void AMessageWithALineFeed()
    {
        var d = DiagnosticMessage.Error(MessageCode.ScriptFailure, MessageStage.Execute, "a\nb");
        Assert.Equal("a\nb", d.Message);
        var text = d.ToString();
        Assert.Equal(18, text.Length);
        Assert.Equal("[JM5004]: a\\u000ab", text);
        Assert.DoesNotContain('\n', text);
        Assert.DoesNotContain('\r', text);
        // FormatMessage writes the message as it is.
        Assert.Equal("[JM5004]: a\nb", DiagnosticFormatter.FormatMessage(d));
    }

    [Fact(DisplayName = "JYRO-DEF-805 JYRO-REQ-1880 ToString escapes a carriage return, a backslash, U+0000, U+007F, U+0085, U+2028 and U+2029 and leaves other characters as they are")]
    [Trait("requirement", "JYRO-REQ-1880")]
    [Trait("defect", "JYRO-DEF-805")]
    public void EscapedCharacters()
    {
        string Print(string message) =>
            DiagnosticMessage.Error(MessageCode.ScriptFailure, MessageStage.Execute, message).ToString();

        Assert.Equal("[JM5004]: a\\u000db", Print("a\rb"));
        Assert.Equal("[JM5004]: a\\u000d\\u000ab", Print("a\r\nb"));
        Assert.Equal("[JM5004]: a\\\\b", Print("a\\b"));
        Assert.Equal("[JM5004]: a\\\\u000ab", Print("a\\u000ab"));
        Assert.Equal("[JM5004]: \\u0000\\u007f\\u0085", Print("\0\u007f\u0085"));
        Assert.Equal("[JM5004]: " + @"\u2028\u2029", Print("\u2028" + "\u2029"));
        Assert.Equal("[JM5004]: a\\u0009b", Print("a\tb"));
        Assert.Equal("[JM5004]: café   中 \"q\" 'r' /", Print("café   中 \"q\" 'r' /"));
    }

    [Fact(DisplayName = "JYRO-DEF-805 JYRO-REQ-1880 ToString escapes the message after the location, with the code and location part unchanged")]
    [Trait("requirement", "JYRO-REQ-1880")]
    [Trait("defect", "JYRO-DEF-805")]
    public void LocatedMessageWithALineFeed()
    {
        var d = DiagnosticMessage.Error(MessageCode.ScriptFailure, MessageStage.Execute, new SourceLocation(4, 7, 1), "x\ny");
        Assert.Equal("[JM5004] Ln 4, Col 7: x\\u000ay", d.ToString());
    }

    [Fact(DisplayName = "JYRO-DEF-805 JYRO-REQ-1880 a script that fails with a message holding a line feed prints its ScriptFailure diagnostic as [JM5004] Ln 1, Col 1: a\\u000ab, and FormatMessage gives the Message with the line feed")]
    [Trait("requirement", "JYRO-REQ-1880")]
    [Trait("defect", "JYRO-DEF-805")]
    public void FailWithALineFeed()
    {
        var result = new JyroRunner(
                new JyroCompiler()
                    .WithSource("fail \"a\\nb\"")
                    .WithStdlib()
                    .CompileOrThrow())
            .WithInput(new JyroObject())
            .Execute();

        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.ScriptFailure);
        Assert.Equal("a\nb", d.Message);
        Assert.Equal(1, d.Location!.Value.Line);
        Assert.Equal(1, d.Location!.Value.Column);
        Assert.Equal("[JM5004] Ln 1, Col 1: a\\u000ab", d.ToString());
        Assert.Equal("[JM5004] Ln 1, Col 1: a\nb", DiagnosticFormatter.FormatMessage(d));
    }
}

public class SourceLocationShapeTests
{
    [Fact(DisplayName = "JYRO-REQ-1882 SourceLocation declares only the constructor, Line, Column and Length besides Equals, GetHashCode and ToString")]
    [Trait("requirement", "JYRO-REQ-1882")]
    public void DeclaredMembers()
    {
        var t = typeof(SourceLocation);
        Assert.True(t.IsValueType);
        Assert.True(t.IsPublic);

        var ctors = t.GetConstructors(BindingFlags.Public | BindingFlags.Instance);
        var ctor = Assert.Single(ctors, c => c.GetParameters().Length > 0);
        Assert.Equal(new[] { typeof(int), typeof(int), typeof(int) }, ctor.GetParameters().Select(p => p.ParameterType).ToArray());
        Assert.Equal(new[] { "line", "column", "length" }, ctor.GetParameters().Select(p => p.Name).ToArray());

        var members = t.GetMembers(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly)
            .Where(m => !(m is MethodBase mb && (mb.IsSpecialName || mb is ConstructorInfo)))
            .Select(m => m.MemberType + " " + m.Name)
            .Distinct()
            .OrderBy(s => s, StringComparer.Ordinal)
            .ToArray();
        var expected = new[]
        {
            "Method Equals", "Method GetHashCode", "Method ToString",
            "Property Column", "Property Length", "Property Line",
        };
        Assert.Equal(expected, members);

        foreach (var p in t.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static))
        {
            Assert.Equal(typeof(int), p.PropertyType);
            Assert.NotNull(p.GetGetMethod());
            Assert.Null(p.GetSetMethod(nonPublic: true));
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1882 SourceLocation has no member named None or Create and is not comparable or structural")]
    [Trait("requirement", "JYRO-REQ-1882")]
    public void NoNoneNoCreateNoStructure()
    {
        var all = typeof(SourceLocation).GetMembers(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
        Assert.DoesNotContain(all, m => m.Name == "None" || m.Name == "Create");
        Assert.False(typeof(IComparable).IsAssignableFrom(typeof(SourceLocation)));
        Assert.False(typeof(IStructuralComparable).IsAssignableFrom(typeof(SourceLocation)));
        Assert.False(typeof(IStructuralEquatable).IsAssignableFrom(typeof(SourceLocation)));
    }

    [Fact(DisplayName = "JYRO-REQ-1882 SourceLocation(3, 5, 2).ToString() is Ln 3, Col 5")]
    [Trait("requirement", "JYRO-REQ-1882")]
    public void ToStringExample()
    {
        Assert.Equal("Ln 3, Col 5", new SourceLocation(3, 5, 2).ToString());
        Assert.Equal("Ln 12, Col 345", new SourceLocation(12, 345, 0).ToString());
        var saved = CultureInfo.CurrentCulture;
        try
        {
            CultureInfo.CurrentCulture = new CultureInfo("ar-SA");
            Assert.Equal("Ln 3, Col 5", new SourceLocation(3, 5, 2).ToString());
        }
        finally
        {
            CultureInfo.CurrentCulture = saved;
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1882 the properties of a SourceLocation are the three arguments of the constructor, and equal locations are Equals")]
    [Trait("requirement", "JYRO-REQ-1882")]
    public void ConstructorArguments()
    {
        var a = new SourceLocation(3, 5, 2);
        Assert.Equal(3, a.Line);
        Assert.Equal(5, a.Column);
        Assert.Equal(2, a.Length);
        Assert.True(a.Equals(new SourceLocation(3, 5, 2)));
        Assert.Equal(a.GetHashCode(), new SourceLocation(3, 5, 2).GetHashCode());
        Assert.False(a.Equals(new SourceLocation(3, 5, 3)));
    }

    [Fact(DisplayName = "JYRO-REQ-1882 the Location of the diagnostic that JyroArtifactInfo.Read gives for the 3 bytes JYR has no value")]
    [Trait("requirement", "JYRO-REQ-1882")]
    public void NoPlaceIsANullLocation()
    {
        var result = JyroArtifactInfo.Read(new byte[] { (byte)'J', (byte)'Y', (byte)'R' });
        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages);
        Assert.False(d.Location.HasValue);
        Assert.Null(d.Location);
    }
}

public class StructuredDiagnosticTests
{
    [Fact(DisplayName = "JYRO-REQ-1883 StructuredDiagnostic is a sealed class with no public or protected constructor and get-only typed properties")]
    [Trait("requirement", "JYRO-REQ-1883")]
    public void Shape()
    {
        var t = typeof(StructuredDiagnostic);
        Assert.True(t.IsClass);
        Assert.True(t.IsSealed);
        Assert.DoesNotContain(t.GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic),
            c => c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly);

        var expected = new (string Name, Type Type)[]
        {
            ("Code", typeof(string)),
            ("NumericCode", typeof(int)),
            ("Severity", typeof(MessageSeverity)),
            ("Stage", typeof(MessageStage)),
            ("Subsystem", typeof(string)),
            ("Message", typeof(string)),
            ("Args", typeof(object[])),
            ("Location", typeof(SourceLocation?)),
        };
        foreach (var (name, type) in expected)
        {
            var p = t.GetProperty(name, BindingFlags.Public | BindingFlags.Instance);
            Assert.True(p != null, name + " is missing");
            Assert.Equal(type, p!.PropertyType);
            Assert.NotNull(p.GetGetMethod());
            Assert.Null(p.GetSetMethod(nonPublic: true));
        }
        Assert.DoesNotContain(t.GetProperties(BindingFlags.Public | BindingFlags.Instance), p => p.GetSetMethod() != null);
    }

    [Fact(DisplayName = "JYRO-REQ-1883 ToStructured of the TypedVariableNeedsInitializer diagnostic of var s: string is JM3302, Error, Validate and validator")]
    [Trait("requirement", "JYRO-REQ-1883")]
    public void TheBookExample()
    {
        var result = new JyroCompiler().WithSource("var s: string").Compile();
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.TypedVariableNeedsInitializer);
        var s = DiagnosticFormatter.ToStructured(d);
        Assert.Equal("JM3302", s.Code);
        Assert.Equal(3302, s.NumericCode);
        Assert.Equal(MessageSeverity.Error, s.Severity);
        Assert.Equal(MessageStage.Validate, s.Stage);
        Assert.Equal("validator", s.Subsystem);
        Assert.Equal(d.Message, s.Message);
        Assert.Equal(d.Location, s.Location);
    }

    private static string Band(int value) => value switch
    {
        >= 1000 and <= 1999 => "lexer",
        >= 2000 and <= 2999 => "parser",
        >= 3000 and <= 3999 => "validator",
        >= 4000 and <= 4999 => "linker",
        >= 5000 and <= 5999 => "runtime",
        >= 6000 and <= 6999 => "compiler",
        >= 7000 and <= 7999 => "artifact",
        _ => "no band: " + value,
    };

    [Fact(DisplayName = "JYRO-REQ-1883 ToStructured gives the code text, the number, the fields of the diagnostic and the subsystem of the band for every MessageCode")]
    [Trait("requirement", "JYRO-REQ-1883")]
    public void EveryCode()
    {
        var stages = Enum.GetValues<MessageStage>();
        var severities = Enum.GetValues<MessageSeverity>();
        var i = 0;
        foreach (var code in Enum.GetValues<MessageCode>())
        {
            var stage = stages[i % stages.Length];
            var severity = severities[i % severities.Length];
            i++;
            var at = (i % 2 == 0) ? new SourceLocation(i, i + 1, 1) : (SourceLocation?)null;
            var d = severity switch
            {
                MessageSeverity.Error => DiagnosticMessage.Error(code, stage, at, "x", 2),
                MessageSeverity.Warning => DiagnosticMessage.Warning(code, stage, at, "x", 2),
                _ => DiagnosticMessage.Info(code, stage, at, "x", 2),
            };
            var s = DiagnosticFormatter.ToStructured(d);
            var value = (int)code;
            Assert.Equal("JM" + value.ToString("D4", CultureInfo.InvariantCulture), s.Code);
            Assert.Equal(value, s.NumericCode);
            Assert.Equal(d.Severity, s.Severity);
            Assert.Equal(d.Stage, s.Stage);
            Assert.Equal(d.Message, s.Message);
            Assert.Equal(d.Args, s.Args);
            Assert.Equal(d.Location, s.Location);
            Assert.NotEqual("unknown", s.Subsystem);
            Assert.True(Band(value) == s.Subsystem, $"{code} ({value}): band {Band(value)}, Subsystem {s.Subsystem}");
        }
    }
}

public class NoRecordSemanticsTests
{
    private static readonly string[] ShippedAssemblies =
        { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static IEnumerable<Type> ExportedTypes() =>
        ShippedAssemblies.SelectMany(name => Assembly.Load(name).GetExportedTypes());

    [Fact(DisplayName = "JYRO-REQ-1884 no exported type of the five assemblies implements IStructuralEquatable or IStructuralComparable")]
    [Trait("requirement", "JYRO-REQ-1884")]
    public void NoStructuralInterfaces()
    {
        var offenders = ExportedTypes()
            .Where(t => t.GetInterfaces().Any(i => i == typeof(IStructuralEquatable) || i == typeof(IStructuralComparable)))
            .Select(t => t.FullName)
            .OrderBy(n => n, StringComparer.Ordinal)
            .ToList();
        Assert.True(offenders.Count == 0, "These types show F# record semantics: " + string.Join(", ", offenders));
    }

    [Fact(DisplayName = "JYRO-REQ-1884 JyroExecutionOptions and Parameter implement neither IComparable nor IComparable of themselves")]
    [Trait("requirement", "JYRO-REQ-1884")]
    public void NotComparable()
    {
        foreach (var t in new[] { typeof(JyroExecutionOptions), typeof(Parameter) })
        {
            Assert.False(typeof(IComparable).IsAssignableFrom(t), t.Name);
            Assert.DoesNotContain(t.GetInterfaces(), i => i.IsGenericType && i.GetGenericTypeDefinition() == typeof(IComparable<>));
        }
    }

    private static void AssertOneLine(string name, string text)
    {
        Assert.False(string.IsNullOrEmpty(text), name + " gave no text");
        Assert.DoesNotContain('\n', text);
        Assert.DoesNotContain('\r', text);
        Assert.DoesNotContain("{ ", text);
    }

    [Fact(DisplayName = "JYRO-REQ-1884 JyroExecutionOptions.Default.ToString() holds no line break and no record listing")]
    [Trait("requirement", "JYRO-REQ-1884")]
    public void OptionsToString()
    {
        AssertOneLine("JyroExecutionOptions", JyroExecutionOptions.Default.ToString());
        AssertOneLine("JyroExecutionOptions.Unlimited", JyroExecutionOptions.Unlimited.ToString());
    }

    [Fact(DisplayName = "JYRO-REQ-1884 the ToString of SourceLocation, StructuredDiagnostic, DiagnosticMessage, Parameter, JyroFunctionSignature and JyroArtifactInfo holds no line break and no record listing")]
    [Trait("requirement", "JYRO-REQ-1884")]
    public void OtherToStrings()
    {
        AssertOneLine("SourceLocation", new SourceLocation(3, 5, 2).ToString());

        var d = DiagnosticMessage.Error(MessageCode.UndefinedFunction, MessageStage.Link, new SourceLocation(2, 4, 3), "Nope");
        AssertOneLine("DiagnosticMessage", d.ToString());
        AssertOneLine("StructuredDiagnostic", DiagnosticFormatter.ToStructured(d).ToString()!);

        var parameter = Parameter.Required("value", ParameterType.String);
        AssertOneLine("Parameter", parameter.ToString());
        AssertOneLine("JyroFunctionSignature",
            JyroFunctionSignature.Create(ParameterType.Any, parameter).ToString());

        var bytes = new JyroCompiler().WithSource("Output.x = 1").WithStdlib().CompileToArtifact();
        Assert.True(bytes.TryGetValue(out var jyrx));
        var info = JyroArtifactInfo.Read(jyrx).GetValueOrThrow();
        AssertOneLine("JyroArtifactInfo", info.ToString());
    }
}

public class SuccessResultTests
{
    [Fact(DisplayName = "JYRO-REQ-1888 Success with a message of severity Error throws ArgumentException whose ParamName is messages")]
    [Trait("requirement", "JYRO-REQ-1888")]
    public void SuccessRefusesAnError()
    {
        var ex = Assert.Throws<ArgumentException>(() =>
            JyroResult<int>.Success(1, new[] { DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute) }));
        Assert.Equal("messages", ex.ParamName);

        var mixed = Assert.Throws<ArgumentException>(() =>
            JyroResult<int>.Success(1, new[]
            {
                DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute),
                DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute),
            }));
        Assert.Equal("messages", mixed.ParamName);
    }

    [Fact(DisplayName = "JYRO-REQ-1888 Success with a warning returns a success holding the value and that warning")]
    [Trait("requirement", "JYRO-REQ-1888")]
    public void SuccessKeepsAWarning()
    {
        var warning = DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute);
        var r = JyroResult<int>.Success(1, new[] { warning });
        Assert.True(r.IsSuccess);
        Assert.True(r.TryGetValue(out var value));
        Assert.Equal(1, value);
        var only = Assert.Single(r.Messages);
        Assert.True(only.Equals(warning));
    }

    [Fact(DisplayName = "JYRO-REQ-1888 Success keeps the messages in order, and a result is a success exactly when it holds no Error")]
    [Trait("requirement", "JYRO-REQ-1888")]
    public void OrderAndIndicator()
    {
        var warn = DiagnosticMessage.Warning(MessageCode.DivisionByZero, MessageStage.Execute);
        var info = DiagnosticMessage.Info(MessageCode.UndefinedFunction, MessageStage.Link, "f");
        var error = DiagnosticMessage.Error(MessageCode.DivisionByZero, MessageStage.Execute);

        var ok = JyroResult<int>.Success(7, new[] { info, warn, info });
        Assert.True(ok.IsSuccess);
        Assert.Equal(new[] { info, warn, info }, ok.Messages.ToArray());
        Assert.Empty(JyroResult<int>.Success(7, Array.Empty<DiagnosticMessage>()).Messages);

        var failed = JyroResult<int>.Failure(7, new[] { warn, error });
        Assert.False(failed.IsSuccess);
        Assert.Contains(failed.Messages, m => m.Severity == MessageSeverity.Error);

        foreach (var r in new[] { ok, failed, JyroResult<int>.Failure(error), JyroResult<int>.Success(1) })
        {
            Assert.Equal(!r.Messages.Any(m => m.Severity == MessageSeverity.Error), r.IsSuccess);
        }
    }
}

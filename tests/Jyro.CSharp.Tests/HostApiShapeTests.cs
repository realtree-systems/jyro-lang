using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Xml.Linq;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// JYRO-ADR-193 and JYRO-ADR-200: number equality, IsTruthy, the one converter from .NET values, the
// refusal of delegates, reflection metadata and tasks, the collection shapes of JyroArray and
// JyroObject, and the smaller shapes. A C# host is the reader: collection initializers, named
// arguments and member lookups are written the way a host writes them, and reflection looks at the
// five shipped assemblies.
public class HostApiShapeTests
{
    private static readonly string[] ShippedAssemblies =
        { "Jyro.Core", "Jyro.Parser", "Jyro.Compiler", "Jyro.Stdlib", "Jyro.Api" };

    private static IEnumerable<Type> ExportedTypes() =>
        ShippedAssemblies.Select(Assembly.Load).SelectMany(a => a.GetExportedTypes());

    private static IEnumerable<Type> JyroValueTypes() =>
        ExportedTypes().Where(t => typeof(JyroValue).IsAssignableFrom(t));

    private const BindingFlags AllPublic =
        BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.FlattenHierarchy;

    private const BindingFlags DeclaredPublic =
        BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;

    private static string[] PublicMembersNamed(Type type, string name) =>
        type.GetMember(name, AllPublic).Select(m => type.Name + "." + m.Name + " (" + m.MemberType + ")").ToArray();

    private static string DocOf(Type type, string idPrefix)
    {
        var path = Path.ChangeExtension(type.Assembly.Location, ".xml");
        Assert.True(File.Exists(path), "The XML documentation file is not beside the assembly: " + path);
        var entry = XDocument.Load(path).Descendants("member")
            .FirstOrDefault(m => ((string?)m.Attribute("name") ?? "").StartsWith(idPrefix, StringComparison.Ordinal));
        Assert.True(entry is not null, "The XML documentation has no entry for " + idPrefix);
        return entry!.Value;
    }

    // -- JYRO-REQ-1895 --------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1895 no parameter of a public constructor, method or indexer of a public type is empty, begins with an underscore or ends in arg and digits")]
    [Trait("requirement", "JYRO-REQ-1895")]
    public void NoPublicParameterHasAnUnchosenName()
    {
        var made = new Regex("arg[0-9]+$", RegexOptions.CultureInvariant);
        var offenders = new List<string>();
        foreach (var type in ExportedTypes())
        {
            var methods = type.GetConstructors(DeclaredPublic).Cast<MethodBase>()
                .Concat(type.GetMethods(DeclaredPublic));
            foreach (var method in methods)
                foreach (var p in method.GetParameters())
                {
                    var name = p.Name;
                    if (string.IsNullOrEmpty(name) || name.StartsWith("_", StringComparison.Ordinal) || made.IsMatch(name))
                        offenders.Add(type.FullName + "." + method.Name + " parameter '" + name + "'");
                }
        }

        Assert.True(offenders.Count == 0,
            "These public parameters have no chosen name:" + Environment.NewLine + string.Join(Environment.NewLine, offenders));
    }

    [Fact(DisplayName = "JYRO-REQ-1895 JyroValue.GetProperty has the parameter key, SetProperty the parameters key and value in that order, and CheckCall the parameter call")]
    [Trait("requirement", "JYRO-REQ-1895")]
    public void TheNamedMembersHaveTheirChosenParameterNames()
    {
        foreach (var type in new[] { typeof(JyroValue), typeof(JyroObject), typeof(JyroArray) })
        {
            var gets = type.GetMethods(DeclaredPublic).Where(m => m.Name == "GetProperty").ToList();
            foreach (var m in gets)
                Assert.Equal(new[] { "key" }, m.GetParameters().Select(p => p.Name).ToArray());
            var sets = type.GetMethods(DeclaredPublic).Where(m => m.Name == "SetProperty").ToList();
            foreach (var m in sets)
                Assert.Equal(new[] { "key", "value" }, m.GetParameters().Select(p => p.Name).ToArray());
        }

        var onValue = typeof(JyroValue).GetMethods(DeclaredPublic).Where(m => m.Name == "GetProperty");
        Assert.Single(onValue);
        var checks = typeof(JyroFunctionBase).GetMethods(DeclaredPublic).Where(m => m.Name == "CheckCall").ToList();
        Assert.NotEmpty(checks);
        foreach (var m in checks)
            Assert.Equal(new[] { "call" }, m.GetParameters().Select(p => p.Name).ToArray());
    }

    [Fact(DisplayName = "JYRO-REQ-1895 a C# host can call GetProperty, SetProperty and CheckCall with named arguments")]
    [Trait("requirement", "JYRO-REQ-1895")]
    public void NamedArgumentsCompileAndWork()
    {
        var o = new JyroObject();
        o.SetProperty(key: "a", value: new JyroNumber(1));
        Assert.Equal(1, o.GetProperty(key: "a").ToInt32());

        JyroValue v = o;
        v.SetProperty(key: "b", value: new JyroNumber(2));
        Assert.Equal(2, v.GetProperty(key: "b").ToInt32());

        // Compiled, not run: the named argument is the point.
        Action<JyroFunctionBase, JyroCallSite> check = (function, site) => function.CheckCall(call: site);
        Assert.NotNull(check);
    }

    // -- JYRO-REQ-1899 --------------------------------------------------------

    // The mathematical value of a number as a fraction, worked out here and not by the library under test.
    private static (BigInteger Num, BigInteger Den) Fraction(JyroNumber n)
    {
        if (n.IsExact)
        {
            var bits = decimal.GetBits(n.DecimalValue);
            var mantissa = ((BigInteger)(uint)bits[2] << 64) | ((BigInteger)(uint)bits[1] << 32) | (uint)bits[0];
            var scale = (bits[3] >> 16) & 0xFF;
            if (bits[3] < 0) mantissa = -mantissa;
            return (mantissa, BigInteger.Pow(10, scale));
        }

        var raw = BitConverter.DoubleToInt64Bits(n.Value);
        var negative = raw < 0;
        var exponent = (int)((raw >> 52) & 0x7FF);
        var fraction = raw & 0xFFFFFFFFFFFFFL;
        BigInteger significand;
        int power;
        if (exponent == 0) { significand = fraction; power = -1074; }
        else { significand = fraction | (1L << 52); power = exponent - 1075; }
        if (negative) significand = -significand;
        return power >= 0
            ? (significand * BigInteger.Pow(2, power), BigInteger.One)
            : (significand, BigInteger.Pow(2, -power));
    }

    private static bool SameValue(JyroNumber a, JyroNumber b)
    {
        var (an, ad) = Fraction(a);
        var (bn, bd) = Fraction(b);
        return an * bd == bn * ad;
    }

    private static List<JyroNumber> NumberPool() => new()
    {
        new JyroNumber(0), new JyroNumber(0.0), new JyroNumber(-0.0), new JyroNumber(0m),
        new JyroNumber(1), new JyroNumber(1.0), new JyroNumber(1m), new JyroNumber(1.00m), new JyroNumber(1L),
        new JyroNumber(-1), new JyroNumber(-1.0), new JyroNumber(-1m),
        new JyroNumber(0.1), new JyroNumber(0.1m), new JyroNumber(0.5), new JyroNumber(0.5m),
        new JyroNumber(2.5), new JyroNumber(2.5m), new JyroNumber(-2.5), new JyroNumber(-2.5m),
        new JyroNumber(9007199254740993m), new JyroNumber(9007199254740992.0), new JyroNumber(9007199254740992m),
        new JyroNumber(9007199254740992L), new JyroNumber(9007199254740993L),
        new JyroNumber(1e22), new JyroNumber(10000000000000000000000m),
        new JyroNumber(1e23), new JyroNumber(100000000000000000000000m),
        new JyroNumber(Math.Pow(2, -20)), new JyroNumber(0.00000095367431640625m),
        new JyroNumber(double.MaxValue), new JyroNumber(double.Epsilon), new JyroNumber(79228162514264337593543950335m),
    };

    [Fact(DisplayName = "JYRO-REQ-1899 the clause's examples: 9007199254740993, 9007199254740992.0 and 9007199254740992 are not transitively equal")]
    [Trait("requirement", "JYRO-REQ-1899")]
    public void TheExamplesOfTheClauseHold()
    {
        var a = new JyroNumber(9007199254740993m);
        var b = new JyroNumber(9007199254740992.0);
        var c = new JyroNumber(9007199254740992m);

        Assert.True(b.Equals(c));
        Assert.True(c.Equals(b));
        Assert.False(a.Equals(b));
        Assert.False(b.Equals(a));
        Assert.False(a.Equals(c));
        Assert.False(c.Equals(a));
        Assert.Equal(b.GetHashCode(), c.GetHashCode());

        Assert.False(new JyroNumber(0.1).Equals(new JyroNumber(0.1m)));
        Assert.False(new JyroNumber(0.1m).Equals(new JyroNumber(0.1)));
        Assert.True(new JyroNumber(1.0).Equals(new JyroNumber(1)));
        Assert.True(new JyroNumber(1).Equals(new JyroNumber(1.0)));
    }

    [Fact(DisplayName = "JYRO-REQ-1899 Equals is true exactly when the mathematical values are equal, for every pair of a pool of exact and inexact numbers")]
    [Trait("requirement", "JYRO-REQ-1899")]
    public void EqualsFollowsTheMathematicalValue()
    {
        var pool = NumberPool();
        var wrong = new List<string>();
        foreach (var x in pool)
            foreach (var y in pool)
                if (x.Equals(y) != SameValue(x, y))
                    wrong.Add(x.ToJson() + (x.IsExact ? " exact" : " inexact") + " vs " + y.ToJson() + (y.IsExact ? " exact" : " inexact")
                              + ": Equals gave " + x.Equals(y));

        Assert.True(wrong.Count == 0, string.Join(Environment.NewLine, wrong));
    }

    [Fact(DisplayName = "JYRO-REQ-1899 two numbers that are Equals have the same GetHashCode, and each number is Equals to itself")]
    [Trait("requirement", "JYRO-REQ-1899")]
    public void EqualNumbersHaveEqualHashCodes()
    {
        var pool = NumberPool();
        var wrong = new List<string>();
        foreach (var x in pool)
        {
            Assert.True(x.Equals(x));
            foreach (var y in pool)
                if (SameValue(x, y) && x.GetHashCode() != y.GetHashCode())
                    wrong.Add(x.ToJson() + (x.IsExact ? " exact" : " inexact") + " and " + y.ToJson() + (y.IsExact ? " exact" : " inexact") + " differ in hash");
        }

        Assert.True(wrong.Count == 0, string.Join(Environment.NewLine, wrong));
    }

    [Fact(DisplayName = "JYRO-REQ-1899 the double nearest a decimal is not that decimal, and a double equals the decimal that is its exact value")]
    [Trait("requirement", "JYRO-REQ-1899")]
    public void TheExactValueOfADoubleDecides()
    {
        Assert.False(new JyroNumber(0.1).Equals(new JyroNumber(0.1m)));
        Assert.True(new JyroNumber(0.5).Equals(new JyroNumber(0.5m)));
        Assert.True(new JyroNumber(Math.Pow(2, -20)).Equals(new JyroNumber(0.00000095367431640625m)));
        Assert.True(new JyroNumber(1e22).Equals(new JyroNumber(10000000000000000000000m)));
        Assert.False(new JyroNumber(1e23).Equals(new JyroNumber(100000000000000000000000m)));
        Assert.True(new JyroNumber(-0.0).Equals(new JyroNumber(0)));
        Assert.Equal(new JyroNumber(-0.0).GetHashCode(), new JyroNumber(0).GetHashCode());
        Assert.True(new JyroNumber(1.00m).Equals(new JyroNumber(1m)));
        Assert.Equal(new JyroNumber(1.00m).GetHashCode(), new JyroNumber(1m).GetHashCode());
    }

    [Fact(DisplayName = "JYRO-REQ-1899 Equals is false for a value that is not a JyroNumber")]
    [Trait("requirement", "JYRO-REQ-1899")]
    public void EqualsIsFalseForOtherTypes()
    {
        var one = new JyroNumber(1);
        Assert.False(one.Equals(null));
        Assert.False(one.Equals(1));
        Assert.False(one.Equals(1.0));
        Assert.False(one.Equals("1"));
        Assert.False(one.Equals(new JyroString("1")));
        Assert.False(one.Equals(JyroBoolean.True));
        Assert.False(one.Equals(JyroNull.Instance));
    }

    [Fact(DisplayName = "JYRO-REQ-1899 a HashSet and a Dictionary merge numbers of the same value, exact with inexact, and keep numbers of different value apart")]
    [Trait("requirement", "JYRO-REQ-1899")]
    public void NumbersWorkAsKeysOfHashedCollections()
    {
        var same = new HashSet<JyroNumber> { new(1), new(1.0), new(1m), new(1.00m), new(1L) };
        Assert.Single(same);

        var apart = new HashSet<JyroNumber> { new(9007199254740993m), new(9007199254740992.0), new(9007199254740992m) };
        Assert.Equal(2, apart.Count);

        var tenths = new HashSet<JyroNumber> { new(0.1), new(0.1m) };
        Assert.Equal(2, tenths.Count);

        var map = new Dictionary<JyroNumber, string> { [new JyroNumber(2.5)] = "two and a half" };
        Assert.Equal("two and a half", map[new JyroNumber(2.5m)]);
        Assert.False(map.ContainsKey(new JyroNumber(2.6m)));
    }

    // -- JYRO-REQ-1900 --------------------------------------------------------

    public static IEnumerable<object[]> TruthinessTable()
    {
        yield return new object[] { "null", JyroNull.Instance, false };
        yield return new object[] { "false", JyroBoolean.False, false };
        yield return new object[] { "true", JyroBoolean.True, true };
        yield return new object[] { "exact zero", new JyroNumber(0m), false };
        yield return new object[] { "inexact zero", new JyroNumber(0.0), false };
        yield return new object[] { "negative inexact zero", new JyroNumber(-0.0), false };
        yield return new object[] { "one", new JyroNumber(1), true };
        yield return new object[] { "minus one", new JyroNumber(-1.5), true };
        yield return new object[] { "tiny", new JyroNumber(double.Epsilon), true };
        yield return new object[] { "empty string", new JyroString(""), false };
        yield return new object[] { "text", new JyroString("x"), true };
        yield return new object[] { "the text false", new JyroString("false"), true };
        yield return new object[] { "empty array", new JyroArray(), true };
        yield return new object[] { "empty object", new JyroObject(), true };
        yield return new object[] { "array of null", new JyroArray { JyroNull.Instance }, true };
        yield return new object[] { "function", new JyroFunction(_ => JyroNull.Instance, 0), true };
    }

    [Theory(DisplayName = "JYRO-REQ-1900 IsTruthy gives the truthiness of every value and does not throw")]
    [Trait("requirement", "JYRO-REQ-1900")]
    [MemberData(nameof(TruthinessTable))]
    public void IsTruthyGivesTheTruthiness(string label, JyroValue value, bool expected)
    {
        Assert.True(expected == value.IsTruthy(), label + " should be " + (expected ? "truthy" : "falsy"));
    }

    [Fact(DisplayName = "JYRO-REQ-1900 the examples: null and the empty string are falsy, an empty array is truthy")]
    [Trait("requirement", "JYRO-REQ-1900")]
    public void TheTruthinessExamplesHold()
    {
        Assert.False(JyroNull.Instance.IsTruthy());
        Assert.False(new JyroString("").IsTruthy());
        Assert.True(new JyroArray().IsTruthy());
    }

    [Fact(DisplayName = "JYRO-REQ-1900 IsTruthy is a public bool method of JyroValue with no parameter, and no JyroValue type has a member named ToBoolean")]
    [Trait("requirement", "JYRO-REQ-1900")]
    public void IsTruthyReplacesToBoolean()
    {
        var m = typeof(JyroValue).GetMethod("IsTruthy", BindingFlags.Public | BindingFlags.Instance, null, Type.EmptyTypes, null);
        Assert.NotNull(m);
        Assert.Equal(typeof(bool), m!.ReturnType);
        Assert.Equal(typeof(JyroValue), m.DeclaringType);

        var found = JyroValueTypes().SelectMany(t => PublicMembersNamed(t, "ToBoolean")).Distinct().ToList();
        Assert.True(found.Count == 0, "Members named ToBoolean remain: " + string.Join(", ", found));
    }

    [Fact(DisplayName = "JYRO-REQ-1900 JyroNumber.Negate returns the opposite number in the same representation, and there is no member named Negated")]
    [Trait("requirement", "JYRO-REQ-1900")]
    public void NegateKeepsTheRepresentation()
    {
        var m = typeof(JyroNumber).GetMethod("Negate", BindingFlags.Public | BindingFlags.Instance, null, Type.EmptyTypes, null);
        Assert.NotNull(m);
        Assert.Equal(typeof(JyroNumber), m!.ReturnType);

        var exact = new JyroNumber(2.5m).Negate();
        Assert.True(exact.IsExact);
        Assert.Equal(-2.5m, exact.DecimalValue);

        var inexact = new JyroNumber(2.5).Negate();
        Assert.False(inexact.IsExact);
        Assert.Equal(-2.5, inexact.Value);

        var back = exact.Negate();
        Assert.True(back.IsExact);
        Assert.Equal(2.5m, back.DecimalValue);
        Assert.True(new JyroNumber(-3).Negate().IsExact);
        Assert.Equal(3m, new JyroNumber(-3).Negate().DecimalValue);

        Assert.Empty(PublicMembersNamed(typeof(JyroNumber), "Negated"));
    }

    [Fact(DisplayName = "JYRO-REQ-1900 JyroNumber.Value is a double, the double nearest the number, and DecimalValue is exact")]
    [Trait("requirement", "JYRO-REQ-1900")]
    public void ValueIsTheLossyDouble()
    {
        Assert.Equal(typeof(double), typeof(JyroNumber).GetProperty("Value")!.PropertyType);
        Assert.Equal(typeof(decimal), typeof(JyroNumber).GetProperty("DecimalValue")!.PropertyType);

        var big = new JyroNumber(9007199254740993m);
        Assert.Equal(9007199254740992.0, big.Value);
        Assert.Equal(9007199254740993m, big.DecimalValue);
        Assert.True(big.IsExact);

        Assert.Equal(0.1, new JyroNumber(0.1m).Value);
        Assert.Equal(2.5, new JyroNumber(2.5).Value);
    }

    [Fact(DisplayName = "JYRO-REQ-1900 the XML documentation of JyroNumber.Value says that it loses precision for an exact number and that DecimalValue gives the exact value")]
    [Trait("requirement", "JYRO-REQ-1900")]
    public void ValueIsDocumentedAsLossy()
    {
        var doc = DocOf(typeof(JyroNumber), "P:RealTree.Jyro.JyroNumber.Value");
        Assert.Contains("precision", doc, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("exact", doc, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("DecimalValue", doc, StringComparison.Ordinal);
    }

    // -- JYRO-REQ-1901 --------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1901 JyroArray has a public Count and no public member named Length or Items")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void JyroArrayHasOneSizeProperty()
    {
        var count = typeof(JyroArray).GetProperty("Count", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(count);
        Assert.Equal(typeof(int), count!.PropertyType);
        Assert.Empty(PublicMembersNamed(typeof(JyroArray), "Length"));
        Assert.Empty(PublicMembersNamed(typeof(JyroArray), "Items"));

        var array = new JyroArray { new JyroNumber(1) };
        int size = array.Count;
        Assert.Equal(1, size);
    }

    [Fact(DisplayName = "JYRO-REQ-1901 JyroObject has public ContainsKey, Keys, Values and Add, and no member named Properties or RemoveProperty")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void JyroObjectHasTheCollectionMembersPublicly()
    {
        var instance = BindingFlags.Public | BindingFlags.Instance;

        var contains = typeof(JyroObject).GetMethod("ContainsKey", instance, null, new[] { typeof(string) }, null);
        Assert.NotNull(contains);
        Assert.Equal(typeof(bool), contains!.ReturnType);
        Assert.Equal(new[] { "key" }, contains.GetParameters().Select(p => p.Name).ToArray());

        var keys = typeof(JyroObject).GetProperty("Keys", instance);
        Assert.NotNull(keys);
        Assert.Equal(typeof(IEnumerable<string>), keys!.PropertyType);
        Assert.NotNull(keys.GetGetMethod());

        var values = typeof(JyroObject).GetProperty("Values", instance);
        Assert.NotNull(values);
        Assert.Equal(typeof(IEnumerable<JyroValue>), values!.PropertyType);
        Assert.NotNull(values.GetGetMethod());

        var add = typeof(JyroObject).GetMethod("Add", instance, null, new[] { typeof(string), typeof(JyroValue) }, null);
        Assert.NotNull(add);
        Assert.Equal(typeof(void), add!.ReturnType);
        Assert.Equal(new[] { "key", "value" }, add.GetParameters().Select(p => p.Name).ToArray());

        Assert.Empty(PublicMembersNamed(typeof(JyroObject), "Properties"));
        Assert.Empty(PublicMembersNamed(typeof(JyroObject), "RemoveProperty"));
    }

    [Fact(DisplayName = "JYRO-REQ-1901 neither JyroValue nor any subclass has a public member named RemoveProperty")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void NoJyroValueTypeHasRemoveProperty()
    {
        var found = JyroValueTypes().SelectMany(t => PublicMembersNamed(t, "RemoveProperty")).Distinct().ToList();
        Assert.True(found.Count == 0, "Members named RemoveProperty remain: " + string.Join(", ", found));
    }

    [Fact(DisplayName = "JYRO-REQ-1901 a collection initializer builds a JyroObject whose Keys are in order, and ContainsKey, Keys and Values are callable without a cast")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void ACollectionInitializerBuildsAnObject()
    {
        var o = new JyroObject { { "a", new JyroNumber(1) }, { "b", JyroNull.Instance } };

        IEnumerable<string> keys = o.Keys;
        IEnumerable<JyroValue> values = o.Values;
        Assert.Equal(new[] { "a", "b" }, keys.ToArray());
        Assert.Equal(new[] { "1", "null" }, values.Select(v => v.ToJson()).ToArray());
        Assert.True(o.ContainsKey("a"));
        Assert.True(o.ContainsKey("b"));
        Assert.False(o.ContainsKey("c"));
        Assert.Equal(2, o.Count);
    }

    [Fact(DisplayName = "JYRO-REQ-1901 Add puts a new property at the end, also after a removal")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void AddAppendsAtTheEnd()
    {
        var o = new JyroObject();
        o.Add("b", new JyroNumber(1));
        o.Add("a", new JyroNumber(2));
        o.Add("c", new JyroNumber(3));
        Assert.Equal(new[] { "b", "a", "c" }, o.Keys.ToArray());

        Assert.True(o.Remove("b"));
        o.Add("b", new JyroNumber(4));
        Assert.Equal(new[] { "a", "c", "b" }, o.Keys.ToArray());
        Assert.Equal(4, o.GetProperty("b").ToInt32());
    }

    [Fact(DisplayName = "JYRO-REQ-1901 Add of an existing key throws ArgumentException whose ParamName is key and leaves the object unchanged")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void AddOfAnExistingKeyThrows()
    {
        var o = new JyroObject { { "a", new JyroNumber(1) }, { "b", new JyroNumber(2) } };

        var ex = Assert.Throws<ArgumentException>(() => o.Add("a", JyroNull.Instance));
        Assert.Equal("key", ex.ParamName);

        Assert.Equal(new[] { "a", "b" }, o.Keys.ToArray());
        Assert.Equal(1, o.GetProperty("a").ToInt32());
        Assert.Equal(2, o.GetProperty("b").ToInt32());
        Assert.Equal(2, o.Count);
    }

    [Fact(DisplayName = "JYRO-REQ-1901 a JyroArray collection initializer fills the array")]
    [Trait("requirement", "JYRO-REQ-1901")]
    public void AnArrayCollectionInitializerFillsTheArray()
    {
        var array = new JyroArray { new JyroNumber(1), new JyroString("two") };
        int size = array.Count;
        Assert.Equal(2, size);
        Assert.Equal("two", array[1].ToStringValue());
    }

    // -- JYRO-REQ-1902 --------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1902 no shipped type has a public member named FromDictionary, FromComplexObject or FromEnumerable")]
    [Trait("requirement", "JYRO-REQ-1902")]
    public void TheOldConvertersAreGone()
    {
        var names = new[] { "FromDictionary", "FromComplexObject", "FromEnumerable" };
        var found = ExportedTypes()
            .SelectMany(t => names.SelectMany(n => PublicMembersNamed(t, n)))
            .Distinct()
            .ToList();
        Assert.True(found.Count == 0, "These converters remain public: " + string.Join(", ", found));
    }

    [Fact(DisplayName = "JYRO-REQ-1902 JyroObject and JyroArray declare no public static method whose name starts with From")]
    [Trait("requirement", "JYRO-REQ-1902")]
    public void ContainersDeclareNoFromMethods()
    {
        foreach (var type in new[] { typeof(JyroObject), typeof(JyroArray) })
        {
            var from = type.GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly)
                .Where(m => m.Name.StartsWith("From", StringComparison.Ordinal))
                .Select(m => type.Name + "." + m.Name)
                .ToList();
            Assert.True(from.Count == 0, "Public From methods remain: " + string.Join(", ", from));
        }
    }

    [Fact(DisplayName = "JYRO-REQ-1902 FromObject is the only public static method of a JyroValue type that takes an object, an IEnumerable of object or an IDictionary of string and object")]
    [Trait("requirement", "JYRO-REQ-1902")]
    public void FromObjectIsTheOneConverter()
    {
        var wanted = new HashSet<Type>
        {
            typeof(object), typeof(IEnumerable<object>), typeof(IDictionary<string, object>),
        };
        var takers = JyroValueTypes()
            .SelectMany(t => t.GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly)
                .Where(m => m.GetParameters().Any(p => wanted.Contains(p.ParameterType)))
                .Select(m => t.Name + "." + m.Name))
            .OrderBy(n => n, StringComparer.Ordinal)
            .ToList();

        Assert.Equal(new[] { "JyroValue.FromObject" }, takers);
    }

    [Fact(DisplayName = "JYRO-REQ-1902 FromObject of a Dictionary of string and object holding a = 1 gives the object {\"a\": 1}, not the object of its properties")]
    [Trait("requirement", "JYRO-REQ-1902")]
    public void ADictionaryBecomesItsEntries()
    {
        var value = JyroValue.FromObject(new Dictionary<string, object> { ["a"] = 1 });

        var o = Assert.IsType<JyroObject>(value);
        Assert.Equal(new[] { "a" }, o.Keys.ToArray());
        Assert.Equal("{\"a\":1}", o.ToJson());
        Assert.False(o.ContainsKey("Comparer"));
        Assert.False(o.ContainsKey("Count"));
        Assert.False(o.ContainsKey("Keys"));
        Assert.False(o.ContainsKey("Values"));
    }

    [Fact(DisplayName = "JYRO-REQ-1902 FromObject takes a dictionary, a sequence and a plain object whatever their static type")]
    [Trait("requirement", "JYRO-REQ-1902")]
    public void FromObjectTakesEachShape()
    {
        IDictionary<string, object> dictionary = new Dictionary<string, object> { ["k"] = "v" };
        IEnumerable<object> sequence = new List<object> { 1, "two" };
        object plain = new PlainPoint { X = 3, Y = 4 };

        Assert.Equal("{\"k\":\"v\"}", JyroValue.FromObject(dictionary).ToJson());
        Assert.Equal("[1,\"two\"]", JyroValue.FromObject(sequence).ToJson());
        Assert.Equal("{\"X\":3,\"Y\":4}", JyroValue.FromObject(plain).ToJson());
        Assert.Equal("{\"Length\":3}", JyroValue.FromObject(new Dictionary<string, object> { ["Length"] = 3 }).ToJson());
    }

    public sealed class PlainPoint
    {
        public int X { get; set; }
        public int Y { get; set; }
    }

    // -- JYRO-REQ-1903 --------------------------------------------------------

    public sealed class ProbeTask : Task
    {
        public ProbeTask() : base(() => { }) { }

        public int Reads;

        public int Probe
        {
            get { Reads++; return 1; }
        }
    }

    public sealed class ThrowingGetter
    {
        public int Boom => throw new InvalidOperationException("a getter that throws");
        public int Fine => 1;
    }

    public sealed class Holder
    {
        public object? Inner { get; set; }
    }

    private static readonly (string Name, Func<object> Make)[] RefusedKinds =
    {
        ("Action", () => (Action)(() => { })),
        ("Func of int", () => (Func<int>)(() => 1)),
        ("multicast delegate", () => (Action)(() => { }) + (Action)(() => { })),
        ("typeof string", () => typeof(string)),
        ("typeof closed generic", () => typeof(List<int>)),
        ("MethodInfo", () => typeof(string).GetMethod("Trim", Type.EmptyTypes)!),
        ("ConstructorInfo", () => typeof(object).GetConstructor(Type.EmptyTypes)!),
        ("PropertyInfo", () => typeof(string).GetProperty("Length")!),
        ("FieldInfo", () => typeof(string).GetField("Empty")!),
        ("Assembly", () => typeof(string).Assembly),
        ("Module", () => typeof(string).Module),
        ("ParameterInfo", () => typeof(string).GetMethod("Contains", new[] { typeof(string) })!.GetParameters()[0]),
        ("completed Task", () => Task.CompletedTask),
        ("completed Task of int", () => Task.FromResult(1)),
        ("pending Task of int", () => new TaskCompletionSource<int>().Task),
        ("default ValueTask", () => new ValueTask()),
        ("completed ValueTask of int", () => new ValueTask<int>(1)),
        ("pending ValueTask of int", () => new ValueTask<int>(new TaskCompletionSource<int>().Task)),
    };

    private static readonly string[] Wrappers =
        { "bare", "object[]", "List of object", "Dictionary of string and object", "anonymous object", "three deep", "plain object property" };

    public static IEnumerable<object[]> RefusedNames() => RefusedKinds.Select(k => new object[] { k.Name });

    public static IEnumerable<object[]> RefusedInWrappers() =>
        from k in RefusedKinds
        from w in Wrappers
        select new object[] { k.Name, w };

    private static object MakeRefused(string name) => RefusedKinds.First(k => k.Name == name).Make();

    private static object Wrap(string wrapper, object value) => wrapper switch
    {
        "bare" => value,
        "object[]" => new object[] { 1, value },
        "List of object" => new List<object> { "x", value },
        "Dictionary of string and object" => new Dictionary<string, object> { ["a"] = 1, ["b"] = value },
        "anonymous object" => new { t = value },
        "three deep" => new object[] { new Dictionary<string, object> { ["k"] = new List<object> { 2, value } } },
        "plain object property" => new Holder { Inner = value },
        _ => throw new ArgumentException(wrapper),
    };

    // FromObject runs on a pool thread and must return within the time given: a pending Task read for
    // its Result would block for good.
    private static JyroRuntimeException AssertRefused(object value) =>
        AssertRefused(value, MessageCode.HostValueNotConvertible);

    private static JyroRuntimeException AssertRefused(object value, MessageCode expected)
    {
        var worker = Task.Run(() => Record.Exception(() => JyroValue.FromObject(value)));
        Assert.True(worker.Wait(TimeSpan.FromSeconds(30)), "FromObject did not return: it blocked on the value");
        var thrown = worker.Result;
        Assert.NotNull(thrown);
        var ex = Assert.IsType<JyroRuntimeException>(thrown);
        Assert.Equal(expected, ex.Code);
        Assert.Empty(ex.Args);
        return ex;
    }

    [Theory(DisplayName = "JYRO-REQ-1903 FromObject of a delegate, reflection metadata or a task throws HostValueNotConvertible with no arguments, without blocking")]
    [Trait("requirement", "JYRO-REQ-1903")]
    [MemberData(nameof(RefusedNames))]
    public void ARefusedValueIsRefused(string kind)
    {
        AssertRefused(MakeRefused(kind));
    }

    [Theory(DisplayName = "JYRO-REQ-1903 FromObject refuses such a value held at any depth of the walk")]
    [Trait("requirement", "JYRO-REQ-1903")]
    [MemberData(nameof(RefusedInWrappers))]
    public void ARefusedValueIsRefusedAtDepth(string kind, string wrapper)
    {
        AssertRefused(Wrap(wrapper, MakeRefused(kind)));
    }

    [Fact(DisplayName = "JYRO-REQ-1903 the examples: an array holding an Action, typeof(string) and a pending Task each throw HostValueNotConvertible")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void TheExamplesOfTheRefusalHold()
    {
        AssertRefused(new object[] { 1, (Action)(() => { }) });
        AssertRefused(typeof(string));
        AssertRefused(new TaskCompletionSource<int>().Task);
    }

    [Fact(DisplayName = "JYRO-REQ-1903 no property of the refused instance is read, bare or held")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void NoPropertyOfARefusedInstanceIsRead()
    {
        var bare = new ProbeTask();
        AssertRefused(bare);
        Assert.Equal(0, bare.Reads);

        var inArray = new ProbeTask();
        AssertRefused(new object[] { 1, inArray });
        Assert.Equal(0, inArray.Reads);

        var inObject = new ProbeTask();
        AssertRefused(new Holder { Inner = inObject });
        Assert.Equal(0, inObject.Reads);

        var inDictionary = new ProbeTask();
        AssertRefused(new Dictionary<string, object> { ["a"] = new List<object> { inDictionary } });
        Assert.Equal(0, inDictionary.Reads);
    }

    [Fact(DisplayName = "JYRO-REQ-1903 a graph with a throwing getter before a task is still refused with HostValueNotConvertible")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void AThrowingGetterBesideATaskDoesNotChangeTheCode()
    {
        AssertRefused(new object[] { new ThrowingGetter(), Task.CompletedTask });
        AssertRefused(new object[] { new ThrowingGetter(), new Holder { Inner = new TaskCompletionSource<int>().Task } });
    }

    [Fact(DisplayName = "JYRO-REQ-1903 a thrown exception holds its TargetSite, a MethodBase, and is refused")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void AThrownExceptionIsRefusedForItsTargetSite()
    {
        Exception thrown;
        try { throw new InvalidOperationException("boom"); }
        catch (Exception e) { thrown = e; }

        Assert.NotNull(thrown.TargetSite);
        AssertRefused(thrown);
    }

    [Fact(DisplayName = "JYRO-REQ-1903 when the walk meets a refused instance and a number that is not finite, the one met first decides the code")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void TheFirstMetDecidesTheCode()
    {
        Action noop = () => { };

        AssertRefused(new object[] { double.NaN, noop }, MessageCode.HostNumberNaN);
        AssertRefused(new object[] { noop, double.NaN }, MessageCode.HostValueNotConvertible);
        AssertRefused(new object[] { double.PositiveInfinity, typeof(string) }, MessageCode.HostNumberInfinite);
        AssertRefused(new object[] { typeof(string), double.PositiveInfinity }, MessageCode.HostValueNotConvertible);

        var nanFirst = new Dictionary<string, object> { ["a"] = double.NaN, ["b"] = noop };
        AssertRefused(nanFirst, MessageCode.HostNumberNaN);
        var refusedFirst = new Dictionary<string, object> { ["a"] = noop, ["b"] = double.NaN };
        AssertRefused(refusedFirst, MessageCode.HostValueNotConvertible);

        AssertRefused(new object[] { new object[] { 1, double.NegativeInfinity }, Task.CompletedTask }, MessageCode.HostNumberInfinite);
        AssertRefused(new object[] { new object[] { 1, Task.CompletedTask }, double.NegativeInfinity }, MessageCode.HostValueNotConvertible);
    }

    private static void AssertRunRefused(object input)
    {
        var program = new JyroCompiler().WithStdlib().WithSource("fail \"ran\"").CompileOrThrow();

        // The member that takes the input does not throw.
        var runner = new JyroRunner(program).WithInput(input);
        var result = runner.Execute();

        Assert.False(result.IsSuccess);
        var message = Assert.Single(result.Messages);
        Assert.Equal(MessageCode.HostValueNotConvertible, message.Code);
        Assert.Empty(message.Args);
    }

    [Fact(DisplayName = "JYRO-REQ-1903 a run given new { t = Task.FromResult(1) } by WithInput(object) fails with one HostValueNotConvertible diagnostic and does not start")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void ARunGivenATaskInAnObjectFails()
    {
        var program = new JyroCompiler().WithStdlib().WithSource("fail \"ran\"").CompileOrThrow();

        var runner = new JyroRunner(program).WithInput(new { t = Task.FromResult(1) });
        var result = runner.Execute();

        Assert.False(result.IsSuccess);
        var message = Assert.Single(result.Messages);
        Assert.Equal(MessageCode.HostValueNotConvertible, message.Code);
        Assert.Empty(message.Args);
    }

    [Theory(DisplayName = "JYRO-REQ-1903 a run given a refused value as Input, bare or held, fails before its first statement with one HostValueNotConvertible diagnostic")]
    [Trait("requirement", "JYRO-REQ-1903")]
    [MemberData(nameof(RefusedInWrappers))]
    public void ARunGivenARefusedValueFails(string kind, string wrapper)
    {
        AssertRunRefused(Wrap(wrapper, MakeRefused(kind)));
    }

    [Fact(DisplayName = "JYRO-REQ-1903 HostValueNotConvertible is numbered 5923 and no other MessageCode member has that value")]
    [Trait("requirement", "JYRO-REQ-1903")]
    public void TheCodeIs5923()
    {
        Assert.Equal(5923, (int)MessageCode.HostValueNotConvertible);

        var withTheValue = Enum.GetNames(typeof(MessageCode))
            .Where(n => (int)Enum.Parse<MessageCode>(n) == 5923)
            .ToArray();
        Assert.Equal(new[] { "HostValueNotConvertible" }, withTheValue);
    }

    // -- JYRO-REQ-2181 --------------------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-2181 JyroString has no public indexer")]
    [Trait("requirement", "JYRO-REQ-2181")]
    public void JyroStringHasNoIndexer()
    {
        var indexers = typeof(JyroString).GetProperties()
            .Where(p => p.GetIndexParameters().Length > 0)
            .Select(p => p.Name)
            .ToList();
        Assert.True(indexers.Count == 0, "Indexers remain: " + string.Join(", ", indexers));
        Assert.Equal("abc", new JyroString("abc").Value);
    }

    [Fact(DisplayName = "JYRO-REQ-2181 JyroFunction has no public static Create, the property ParameterCount and no ParamCount")]
    [Trait("requirement", "JYRO-REQ-2181")]
    public void JyroFunctionIsMadeByItsConstructorOnly()
    {
        var creates = typeof(JyroFunction).GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.FlattenHierarchy)
            .Where(m => m.Name == "Create")
            .ToList();
        Assert.Empty(creates);

        var count = typeof(JyroFunction).GetProperty("ParameterCount", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(count);
        Assert.Equal(typeof(int), count!.PropertyType);
        Assert.Empty(PublicMembersNamed(typeof(JyroFunction), "ParamCount"));

        Func<IReadOnlyList<JyroValue>, JyroValue> body = _ => JyroNull.Instance;
        Assert.Equal(2, new JyroFunction(body, 2).ParameterCount);
        Assert.Equal(0, new JyroFunction(body, 0).ParameterCount);
        Assert.Equal(3, new JyroFunction(body, 3).ParameterCount);
    }

    [Fact(DisplayName = "JYRO-REQ-2181 a change to the JyroExecutionOptions.Default a host read is not seen by a later read")]
    [Trait("requirement", "JYRO-REQ-2181")]
    public void DefaultIsFreshOnEachRead()
    {
        var first = JyroExecutionOptions.Default;
        first.MaxStatements = 1;
        first.MaxLoopIterations = 2;
        first.MaxCallDepth = 3;
        first.MaxExecutionTime = TimeSpan.FromMilliseconds(4);
        first.MaxRegexTime = TimeSpan.FromMilliseconds(5);

        var later = JyroExecutionOptions.Default;
        Assert.NotSame(first, later);
        Assert.Equal(10000, later.MaxStatements);
        Assert.NotEqual(2, later.MaxLoopIterations);
        Assert.NotEqual(3, later.MaxCallDepth);
        Assert.NotEqual(TimeSpan.FromMilliseconds(4), later.MaxExecutionTime);
        Assert.NotEqual(TimeSpan.FromMilliseconds(5), later.MaxRegexTime);
        Assert.Equal(JyroExecutionOptions.Default, JyroExecutionOptions.Default);
        Assert.NotSame(JyroExecutionOptions.Default, JyroExecutionOptions.Default);
    }

    [Fact(DisplayName = "JYRO-REQ-2181 a change to the JyroExecutionOptions.Unlimited a host read is not seen by a later read")]
    [Trait("requirement", "JYRO-REQ-2181")]
    public void UnlimitedIsFreshOnEachRead()
    {
        var before = JyroExecutionOptions.Unlimited;
        var statements = before.MaxStatements;
        var loops = before.MaxLoopIterations;
        var depth = before.MaxCallDepth;
        var time = before.MaxExecutionTime;
        var regex = before.MaxRegexTime;

        var changed = JyroExecutionOptions.Unlimited;
        changed.MaxStatements = statements == 7 ? 8 : 7;
        changed.MaxLoopIterations = loops == 7 ? 8 : 7;
        changed.MaxCallDepth = depth == 7 ? 8 : 7;
        changed.MaxExecutionTime = TimeSpan.FromMilliseconds(7);
        changed.MaxRegexTime = TimeSpan.FromMilliseconds(7);

        var later = JyroExecutionOptions.Unlimited;
        Assert.NotSame(changed, later);
        Assert.Equal(statements, later.MaxStatements);
        Assert.Equal(loops, later.MaxLoopIterations);
        Assert.Equal(depth, later.MaxCallDepth);
        Assert.Equal(time, later.MaxExecutionTime);
        Assert.Equal(regex, later.MaxRegexTime);
        Assert.NotSame(JyroExecutionOptions.Unlimited, JyroExecutionOptions.Unlimited);
    }

    [Fact(DisplayName = "JYRO-REQ-2181 the XML documentation of Unlimited says that it keeps the call depth at the safe maximum, in plain words with no clause number")]
    [Trait("requirement", "JYRO-REQ-2181")]
    public void UnlimitedIsDocumentedAsKeepingTheCallDepth()
    {
        var doc = DocOf(typeof(JyroExecutionOptions), "P:RealTree.Jyro.JyroExecutionOptions.Unlimited");
        Assert.Contains("call depth", doc, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("safe maximum", doc, StringComparison.OrdinalIgnoreCase);
        Assert.DoesNotMatch(@"\d+\.\d+", doc);
    }
}

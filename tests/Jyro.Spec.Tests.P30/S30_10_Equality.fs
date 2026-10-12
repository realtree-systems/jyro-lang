/// 30.10 Equality and 30.11 Relational comparison.
module RealTree.Jyro.Spec.Tests.P30.S30_10_Equality

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

/// `Output.r = [a == b, a != b, b == a, b != a]` for the operands, with
/// `i1` an inexact 1 and `f`, `g` function values available.
let private compare (a: string) (b: string) : bool list =
    let out =
        outOf (
            "var i1 = " + I1 + "\n"
            + "var a = " + a + "\nvar b = " + b + "\n"
            + "Output.r = [a == b, a != b, b == a, b != a]")
    Values.asList (Values.prop "r" out) |> List.map Values.asBool

let private assertEquality (expected: bool) (a: string) (b: string) =
    match compare a b with
    | [ eq; ne; eq2; ne2 ] ->
        Assert.True((eq = expected), sprintf "%s == %s should be %b" a b expected)
        Assert.True((eq2 = expected), sprintf "%s == %s should be %b" b a expected)
        Assert.True((ne = not eq), sprintf "%s != %s should be the opposite of ==" a b)
        Assert.True((ne2 = not eq2), sprintf "%s != %s should be the opposite of ==" b a)
    | other -> Values.fail (sprintf "unexpected result %A" other)

// ---------------------------------------------------------------- 30.10.1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-105")>]
[<InlineData("0", "false")>]
[<InlineData("1", "true")>]
[<InlineData("0", "null")>]
[<InlineData("\"\"", "null")>]
[<InlineData("\"\"", "false")>]
[<InlineData("0", "\"0\"")>]
[<InlineData("1", "\"1\"")>]
[<InlineData("\"true\"", "true")>]
[<InlineData("[]", "{}")>]
[<InlineData("[]", "null")>]
[<InlineData("{}", "null")>]
[<InlineData("[]", "\"\"")>]
[<InlineData("x => x", "null")>]
[<InlineData("x => x", "\"x => x\"")>]
[<InlineData("[1]", "1")>]
[<InlineData("[]", "0")>]
[<InlineData("\"null\"", "null")>]
let ``== gives false when the operands have different types`` (a: string, b: string) =
    assertEquality false a b

// ---------------------------------------------------------------- 30.10.2

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1772")>]
[<InlineData("true", "true", true)>]
[<InlineData("false", "false", true)>]
[<InlineData("true", "false", false)>]
[<InlineData("false", "true", false)>]
[<InlineData("(1 < 2)", "true", true)>]
[<InlineData("(not 1)", "false", true)>]
[<InlineData("(1 > 2)", "true", false)>]
[<InlineData("(1 > 2)", "(2 > 1)", false)>]
[<InlineData("(not true)", "(not true)", true)>]
[<InlineData("(not true)", "(not false)", false)>]
let ``JYRO-REQ-1772 Two booleans are equal exactly when they have the same value`` (a: string, b: string, expected: bool) =
    assertEquality expected a b

// ---------------------------------------------------------------- 30.10.3

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1773")>]
[<InlineData("1", "1", true)>]
[<InlineData("1", "2", false)>]
[<InlineData("-1", "1", false)>]
[<InlineData("1", "1.0000000000000000000000000001", false)>]
[<InlineData("9007199254740993", "9007199254740994", false)>]
[<InlineData("1", "1.0", true)>]
[<InlineData("0.10", "0.1", true)>]
[<InlineData("1e2", "100", true)>]
[<InlineData("0", "-0", true)>]
[<InlineData("0.1 + 0.2", "0.3", true)>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950334", false)>]
[<InlineData("9007199254740993", "9007199254740992", false)>]
[<InlineData("1e-28", "0", false)>]
[<InlineData("0.1", "0.2", false)>]
[<InlineData("0.1000000000000000000000000001", "0.1", false)>]
let ``JYRO-REQ-1773 Two exact numbers are equal exactly when their exact values are equal`` (a: string, b: string, expected: bool) =
    assertEquality expected a b

// ---------------------------------------------------------------- 30.10.4

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1774")>]
[<InlineData("1.0000000000000002 * i1", "i1", false)>]
[<InlineData("1.0000000000000002 * i1", "1", false)>]
[<InlineData("1", "1.0000000000000002 * i1", false)>]
[<InlineData("1.0000000000000002 * i1", "1.0000000000000004 * i1", false)>]
[<InlineData("0.1 * i1", "0.2", false)>]
[<InlineData("-0.0 * i1", "0", true)>]
[<InlineData("-0.0 * i1", "0.0 * i1", true)>]
[<InlineData("9007199254740993", "9007199254740992 * i1", true)>]
[<InlineData("0.1", "0.1 * i1", true)>]
[<InlineData("0.3", "0.1 * i1 + 0.2", false)>]
[<InlineData("Power(2, 0.5)", "1.4142135623730951", true)>]
[<InlineData("0", "0 * -Power(2, 0.5)", true)>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950334 * i1", true)>]
[<InlineData("Power(2, 0.5)", "Power(2, 0.5)", true)>]
[<InlineData("1", "i1", true)>]
[<InlineData("0.1000000000000000000000000001", "0.1 * i1", true)>]
[<InlineData("1e29", "1e29 + 1", true)>]
let ``JYRO-REQ-1774 Numbers of which one or both are inexact are equal exactly when their doubles are equal`` (a: string, b: string, expected: bool) =
    assertEquality expected a b

// ---------------------------------------------------------------- 30.10.5

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2148")>]
[<InlineData("\"abc\"", "\"abd\"", false)>]
[<InlineData("\"abc\"", "\"bbc\"", false)>]
[<InlineData("\"abc\"", "\"ab\"", false)>]
[<InlineData("\"\"", "\" \"", false)>]
[<InlineData("\"\\uD83D\\uDE00\"", "\"\\uD83D\\uDE00\"", true)>]
[<InlineData("\"\\uD83D\\uDE00\"", "\"\\uD83D\\uDE01\"", false)>]
[<InlineData("\"\\uD83D\\uDE00\"[0]", "\"\\uD83D\\uDE00\"[1]", false)>]
[<InlineData("\"abc\"", "\"abc\"", true)>]
[<InlineData("\"\"", "\"\"", true)>]
[<InlineData("\"a\"", "\"A\"", false)>]
[<InlineData("\"abc\"", "\"abc \"", false)>]
[<InlineData("\"\\u00E9\"", "\"e\\u0301\"", false)>]
[<InlineData("\"\\u00E9\"", "\"\\u00E9\"", true)>]
[<InlineData("Substring(\"xabc\", 1)", "\"ab\" & \"c\"", true)>]
[<InlineData("\"\\uD83D\\uDE00\"[0]", "\"\\uD83D\\uDE01\"[0]", true)>]
[<InlineData("\"\\uD83D\\uDE00\"[1]", "\"\\uD83D\\uDE01\"[1]", false)>]
[<InlineData("\"\\u0130\"", "\"I\"", false)>]
let ``JYRO-REQ-2148 Two strings are equal exactly when they are the same sequence of code units`` (a: string, b: string, expected: bool) =
    assertEquality expected a b

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2148")>]
let ``JYRO-REQ-2148 A string from JSON Input equals a literal with the same code units`` () =
    let r = Script.runJson "{\"s\":\"abc\",\"e\":\"\\u00e9\"}" "Output.r = [Input.s == \"abc\", Input.e == \"\\u00E9\", Input.e == \"e\\u0301\"]"
    Assert.Equal("[true,true,false]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.10.6

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1776")>]
[<InlineData("[1, 2, 3]", "[9, 2, 3]", false)>]
[<InlineData("[1, 2, 3]", "[1, 9, 3]", false)>]
[<InlineData("[1, 2, 3]", "[1, 2, 9]", false)>]
[<InlineData("[1, 2, 3]", "[1, 2]", false)>]
[<InlineData("[]", "[[]]", false)>]
[<InlineData("[[]]", "[[]]", true)>]
[<InlineData("[1, 2, 3]", "[1, 2, 3]", true)>]
[<InlineData("[]", "[]", true)>]
[<InlineData("[1, 2]", "[1, 2]", true)>]
[<InlineData("[1, 2]", "[2, 1]", false)>]
[<InlineData("[1]", "[1, 1]", false)>]
[<InlineData("[[1], [2]]", "[[1], [2]]", true)>]
[<InlineData("[[1]]", "[[2]]", false)>]
[<InlineData("[0.1]", "[0.1 * i1]", true)>]
[<InlineData("[null]", "[]", false)>]
[<InlineData("[{a: 1}]", "[{a: 1}]", true)>]
[<InlineData("[1, \"1\"]", "[1, 1]", false)>]
[<InlineData("[1, [2, [3]]]", "[1, [2, [3]]]", true)>]
[<InlineData("[1, [2, [3]]]", "[1, [2, [4]]]", false)>]
let ``JYRO-REQ-1776 Two arrays are equal exactly when they have the same length and equal elements`` (a: string, b: string, expected: bool) =
    assertEquality expected a b

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1776")>]
let ``JYRO-REQ-1776 Separately built equal arrays are equal, also from JSON Input`` () =
    let r =
        Script.runJson "{\"a\":[1,[2],{\"k\":3}]}"
            "var x = []\nx[0] = 1\nx[1] = [2]\nx[2] = {k: 3}\nOutput.r = [x == [1, [2], {k: 3}], Input.a == x, Input.a == [1, [2], {k: 4}]]"
    Assert.Equal("[true,true,false]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.10.7

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1777")>]
[<InlineData("{a: 1, b: 2}", "{a: 1, b: 3}", false)>]
[<InlineData("{a: 1, b: 2}", "{a: 1}", false)>]
[<InlineData("{a: 1, b: 2}", "{b: 2, c: 1}", false)>]
[<InlineData("{a: 1, b: 2, c: 3}", "{c: 3, a: 1, b: 2}", true)>]
[<InlineData("{a: {}}", "{a: []}", false)>]
[<InlineData("{}", "{}", true)>]
[<InlineData("{a: 1, b: 2}", "{b: 2, a: 1}", true)>]
[<InlineData("{a: 1}", "{a: 1, b: 2}", false)>]
[<InlineData("{a: null}", "{}", false)>]
[<InlineData("{a: 1}", "{A: 1}", false)>]
[<InlineData("{a: {b: [1]}}", "{a: {b: [1]}}", true)>]
[<InlineData("{a: {b: [1]}}", "{a: {b: [2]}}", false)>]
[<InlineData("{a: 1}", "{a: \"1\"}", false)>]
[<InlineData("{a: 0.1}", "{a: 0.1 * i1}", true)>]
[<InlineData("{a: 1, b: null}", "{a: 1, c: null}", false)>]
let ``JYRO-REQ-1777 Two objects are equal exactly when they have the same keys and equal values`` (a: string, b: string, expected: bool) =
    assertEquality expected a b

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1777")>]
let ``JYRO-REQ-1777 An object from JSON Input equals a literal with the same keys and values`` () =
    let r = Script.runJson "{\"o\":{\"b\":2,\"a\":1}}" "Output.r = [Input.o == {a: 1, b: 2}, Input.o == {a: 1}]"
    Assert.Equal("[true,false]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.10.8

[<Fact>]
[<Trait("requirement", "JYRO-REQ-112")>]
let ``null equals null from every source`` () =
    let out = outOf "var n\nOutput.r = [null == null, {}.x == null, Input.missing == null, n == null, null != null, [][0] == {}.y]"
    Assert.Equal("[true,true,true,true,false,true]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.10.9

[<Fact>]
[<Trait("requirement", "JYRO-REQ-113")>]
let ``JYRO-REQ-113 A function value is equal to itself by every path`` () =
    let out =
        outOf (
            "func same(p)\n return p\nend\n"
            + "var f = x => x\nvar g = f\n"
            + "Output.r = [f == g, f == f, [f] == [f], {k: f} == {k: f}, Clone(f) == f, same(f) == f, f != g]")
    Assert.Equal("[true,true,true,true,true,true,false]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-113")>]
let ``JYRO-REQ-113 Two different function values are not equal, whatever their source text`` () =
    let out =
        outOf (
            "func mk()\n return x => x\nend\n"
            + "var f = x => x\nvar h = x => x\n"
            + "Output.r = [(x => x) == (x => x), f == h, mk() == mk(), [f] == [h], {k: f} == {k: h}, f != h]")
    Assert.Equal("[false,false,false,false,false,true]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.10.10

[<Theory>]
[<Trait("requirement", "JYRO-REQ-114")>]
[<InlineData("0", "false")>]
[<InlineData("[]", "{}")>]
[<InlineData("true", "true")>]
[<InlineData("1", "1.0")>]
[<InlineData("9007199254740993", "9007199254740992")>]
[<InlineData("0.3", "0.1 * i1 + 0.2")>]
[<InlineData("0.1", "0.1 * i1")>]
[<InlineData("\"a\"", "\"A\"")>]
[<InlineData("[1, [2]]", "[1, [2]]")>]
[<InlineData("{a: 1}", "{a: 2}")>]
[<InlineData("null", "null")>]
[<InlineData("x => x", "x => x")>]
[<InlineData("null", "x => x")>]
let ``a != b gives the opposite of a == b`` (a: string, b: string) =
    match compare a b with
    | [ eq; ne; eq2; ne2 ] ->
        Assert.Equal(not eq, ne)
        Assert.Equal(not eq2, ne2)
    | other -> Values.fail (sprintf "unexpected result %A" other)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-114")>]
[<InlineData("var a = [1]\na[1] = a\nvar b = [1]\nb[1] = b\n")>]
[<InlineData("var a = {}\na.s = a\nvar b = {}\nb.s = b\n")>]
let ``!= raises what == raises`` (make: string) =
    let eq = Script.run (make + "Output.r = a == b")
    let ne = Script.run (make + "Output.r = a != b")
    Script.expectError MessageCode.ValueNestingLimitExceeded eq
    Script.expectError MessageCode.ValueNestingLimitExceeded ne

// ---------------------------------------------------------------- 30.10.11

/// Two separate but equal nestings of depth `d` (the root counted as one),
/// of arrays (`[x]`) or objects (`{k: x}`), compared with == and !=.
let private nestedCompare (wrap: string) (d: int) =
    let source =
        "var a = " + (if wrap = "array" then "[]" else "{}") + "\n"
        + "var b = " + (if wrap = "array" then "[]" else "{}") + "\n"
        + "var i = 1\nwhile i < " + string d + " do\n"
        + (if wrap = "array" then " a = [a]\n b = [b]\n" else " a = {k: a}\n b = {k: b}\n")
        + " i = i + 1\nend\n"
    Script.runWith bigLimits (source + "Output.eq = a == b"), Script.runWith bigLimits (source + "Output.ne = a != b")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1952")>]
[<InlineData("array", -1)>]
[<InlineData("array", 0)>]
[<InlineData("object", -1)>]
[<InlineData("object", 0)>]
let ``A comparison within the nesting bound succeeds`` (wrap: string, offset: int) =
    let eq, ne = nestedCompare wrap (maxDepth () + offset)
    Assert.True(Values.asBool (Values.prop "eq" (Script.output eq)))
    Assert.False(Values.asBool (Values.prop "ne" (Script.output ne)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1952")>]
[<InlineData("array", 1)>]
[<InlineData("array", 50)>]
[<InlineData("object", 1)>]
[<InlineData("object", 50)>]
let ``A comparison deeper than the nesting bound raises ValueNestingLimitExceeded`` (wrap: string, offset: int) =
    let eq, ne = nestedCompare wrap (maxDepth () + offset)
    Script.expectError MessageCode.ValueNestingLimitExceeded eq
    Script.expectError MessageCode.ValueNestingLimitExceeded ne

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1952")>]
let ``The width of a value does not count toward the bound`` () =
    let r = Script.runWith bigLimits "Output.r = [Range(0, 10000), [Range(0, 10000)]] == [Range(0, 10000), [Range(0, 10000)]]"
    Assert.True(Values.asBool (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.10.12

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1953")>]
[<InlineData("var a = []\na[0] = a\nOutput.r = a == a")>]
[<InlineData("var a = []\na[0] = a\nvar b = a\nOutput.r = a == b")>]
[<InlineData("var a = []\na[0] = a\nOutput.r = a != a")>]
[<InlineData("var a = []\na[0] = a\nOutput.r = [a] == [a]")>]
[<InlineData("var o = {}\no.s = o\nOutput.r = o == o")>]
[<InlineData("var o = {}\no.s = o\nOutput.r = {k: o} == {k: o}")>]
let ``Comparing a cyclic value with itself walks it and raises`` (source: string) =
    Script.expectError MessageCode.ValueNestingLimitExceeded (Script.run source)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1953")>]
let ``Comparing an over-deep value with itself walks it and raises`` () =
    let d = maxDepth () + 5
    let r = Script.runWith bigLimits ("var d = []\nvar i = 1\nwhile i < " + string d + " do\n d = [d]\n i = i + 1\nend\nOutput.r = d == d")
    Script.expectError MessageCode.ValueNestingLimitExceeded r

// ---------------------------------------------------------------- 30.11

/// `[a < b, a <= b, a > b, a >= b]`.
let private relations (a: string) (b: string) : string =
    let out =
        outOf ("var i1 = " + I1 + "\nvar a = " + a + "\nvar b = " + b + "\nOutput.r = [a < b, a <= b, a > b, a >= b]")
    Values.toJson (Values.prop "r" out)

let private order lt le gt ge = sprintf "[%b,%b,%b,%b]" lt le gt ge
let private less = order true true false false
let private equal = order false true false true
let private greater = order false false true true

[<Theory>]
[<Trait("requirement", "JYRO-REQ-117")>]
[<InlineData("0.1", "0.2", "less")>]
[<InlineData("0.2", "0.1", "greater")>]
[<InlineData("0.1", "0.10", "equal")>]
[<InlineData("79228162514264337593543950334", "79228162514264337593543950335", "less")>]
[<InlineData("9007199254740993", "9007199254740992", "greater")>]
[<InlineData("0", "1e-28", "less")>]
[<InlineData("-1e-28", "0", "less")>]
[<InlineData("0", "-0", "equal")>]
[<InlineData("-5", "-4", "less")>]
[<InlineData("0.1000000000000000000000000001", "0.1", "greater")>]
let ``Relational operators order two exact numbers by their exact values`` (a: string, b: string, expected: string) =
    let exp = match expected with "less" -> less | "greater" -> greater | _ -> equal
    Assert.Equal(exp, relations a b)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-118")>]
[<InlineData("9007199254740993", "9007199254740992 * i1", "equal")>]
[<InlineData("0.1 * i1 + 0.2", "0.3", "greater")>]
[<InlineData("0.3", "0.1 * i1 + 0.2", "less")>]
[<InlineData("Power(2, 0.5)", "1.5", "less")>]
[<InlineData("0 * -Power(2, 0.5)", "0", "equal")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950334 * i1", "equal")>]
[<InlineData("0.1000000000000000000000000001", "0.1 * i1", "equal")>]
[<InlineData("1e29", "1e29 + 1", "equal")>]
let ``Relational operators order numbers with an inexact operand by their doubles`` (a: string, b: string, expected: string) =
    let exp = match expected with "less" -> less | "greater" -> greater | _ -> equal
    Assert.Equal(exp, relations a b)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-119")>]
[<InlineData("\"a\"", "\"b\"", "less")>]
[<InlineData("\"B\"", "\"a\"", "less")>]
[<InlineData("\"a\"", "\"ab\"", "less")>]
[<InlineData("\"\"", "\"a\"", "less")>]
[<InlineData("\"abc\"", "\"abc\"", "equal")>]
[<InlineData("\"Z\"", "\"a\"", "less")>]
[<InlineData("\"\\u00E9\"", "\"f\"", "greater")>]
[<InlineData("\"\\uD83D\\uDE00\"", "\"\\uFF5E\"", "less")>]
[<InlineData("\"10\"", "\"9\"", "less")>]
[<InlineData("\"a\"", "\"\\u00E4\"", "less")>]
[<InlineData("\"e\\u0301\"", "\"\\u00E9\"", "less")>]
let ``Relational operators order two strings by their code units`` (a: string, b: string, expected: string) =
    let exp = match expected with "less" -> less | "greater" -> greater | _ -> equal
    Assert.Equal(exp, relations a b)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-120")>]
[<InlineData("1", "\"1\"")>]
[<InlineData("\"a\"", "1")>]
[<InlineData("null", "1")>]
[<InlineData("1", "null")>]
[<InlineData("null", "null")>]
[<InlineData("true", "false")>]
[<InlineData("true", "1")>]
[<InlineData("[]", "[]")>]
[<InlineData("[1]", "[2]")>]
[<InlineData("{}", "{}")>]
[<InlineData("x => x", "x => x")>]
[<InlineData("\"a\"", "[\"a\"]")>]
let ``Relational operators raise UnsupportedBinaryOperation unless both are numbers or both strings`` (a: string, b: string) =
    for op in [ "<"; "<="; ">"; ">=" ] do
        let r = Script.run ("var a = " + a + "\nvar b = " + b + "\nOutput.r = a " + op + " b")
        try Script.expectError MessageCode.UnsupportedBinaryOperation r
        with e -> Values.fail (sprintf "%s %s %s: %s" a op b e.Message)

/// jyro-spec 95.1 TypeOf, 95.2 Coalesce, 95.3 ToString, 95.4 ToBoolean.
module RealTree.Jyro.Spec.Tests.S95_01_04

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

let private noLimits = Some JyroExecutionOptions.Unlimited

let private rUnlimited (src: string) =
    Script.output (Script.runWith { Script.defaults with Limits = noLimits } src) |> Values.prop "r"

let private typeNames = set [ "null"; "boolean"; "number"; "string"; "array"; "object"; "function" ]

// =========================================================================
// 95.1 TypeOf
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1067")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("1")>]
[<InlineData("1.5")>]
[<InlineData("'s'")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``TypeOf_ForEachType_ReturnsAString`` (expr: string) =
    let src = sprintf "var v = %s\nOutput.r = TypeOf(TypeOf(v))" expr
    Assert.Equal("string", rStr src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1067")>]
let ``TypeOf_WithNamedArgumentValue_ReturnsString`` () =
    Assert.Equal("number", rStr "Output.r = TypeOf(value: 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1067")>]
let ``TypeOf_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = TypeOf()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1067")>]
let ``TypeOf_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = TypeOf(1, 2)"

/// Seven type-name checks against `is`, for 95.1.2's agreement line.
let private agreement =
    "Output.agree = ((TypeOf(v) == 'null') == (v is null)) and ((TypeOf(v) == 'boolean') == (v is boolean)) "
    + "and ((TypeOf(v) == 'number') == (v is number)) and ((TypeOf(v) == 'string') == (v is string)) "
    + "and ((TypeOf(v) == 'array') == (v is array)) and ((TypeOf(v) == 'object') == (v is object)) "
    + "and ((TypeOf(v) == 'function') == (v is function))"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1068")>]
[<InlineData("null", "null")>]
[<InlineData("true", "boolean")>]
[<InlineData("false", "boolean")>]
[<InlineData("0", "number")>]
[<InlineData("1", "number")>]
[<InlineData("-2.5", "number")>]
[<InlineData("0.1 + 0.2", "number")>]
[<InlineData("5e-324", "number")>]
[<InlineData("1e308", "number")>]
[<InlineData("SquareRoot(2)", "number")>]
[<InlineData("''", "string")>]
[<InlineData("'x'", "string")>]
[<InlineData("[]", "array")>]
[<InlineData("[1]", "array")>]
[<InlineData("{}", "object")>]
[<InlineData("{a: 1}", "object")>]
[<InlineData("x => x", "function")>]
[<InlineData("(a, b) => a", "function")>]
[<InlineData("Input", "object")>]
[<InlineData("Input.arr", "array")>]
[<InlineData("Input.n", "number")>]
[<InlineData("Input.s", "string")>]
[<InlineData("Input.z", "null")>]
[<InlineData("Input.missing", "null")>]
let ``TypeOf_ReturnsTheTableNameForTheType`` (expr: string, expected: string) =
    let src = sprintf "var v = %s\nOutput.r = TypeOf(v)\n%s" expr agreement
    let out = Script.output (Script.runJson """{"arr":[1,2],"n":0.5,"s":"t","z":null}""" src)
    Assert.Equal(expected, Values.asString (Values.prop "r" out))
    Assert.True(Values.asBool (Values.prop "agree" out), "TypeOf(v) == T disagrees with v is T")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1068")>]
let ``TypeOf_OfHostInexactNumber_ReturnsNumber`` () =
    let n = Values.inexact 0.1
    Assert.False(Values.isExact n)
    Assert.Equal("number", Values.asString (rVal (Values.obj [ "n", n ]) "Output.r = TypeOf(Input.n)"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1069")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("0")>]
[<InlineData("1e-30")>]
[<InlineData("''")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
[<InlineData("Input")>]
[<InlineData("Input.a")>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<InlineData("ci")>]
let ``TypeOf_NeverReturnsAnyOrANameOutsideTheTable`` (expr: string) =
    let src = cyclesDecl + sprintf "Output.r = TypeOf(%s)" expr
    let name = Values.asString (rIn """{"a":[{}]}""" src)
    Assert.NotEqual<string>("any", name)
    Assert.True(typeNames.Contains name, sprintf "TypeOf returned %s" name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1070")>]
let ``TypeOf_OfCyclicArray_ReturnsArrayWithNoError`` () =
    Assert.Equal("array", rStr (cyclesDecl + "Output.r = TypeOf(ca)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1070")>]
let ``TypeOf_OfCyclicObject_ReturnsObjectWithNoError`` () =
    Assert.Equal("object", rStr (cyclesDecl + "Output.r = TypeOf(co)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1070")>]
let ``TypeOf_OfMutualAndIndirectCycles_ReturnsObjectWithNoError`` () =
    let src = cyclesDecl + "var o = {}\nvar p = {}\no.b = p\np.o = o\nOutput.r = [TypeOf(o), TypeOf(ci), TypeOf(ci.p)]"
    Assert.Equal(norm """["object","object","array"]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1070")>]
let ``TypeOf_OfValueDeeperThanNestingBound_DoesNotRaise`` () =
    let d = nestingBound.Force() + 10
    Assert.Equal("array", rStr (nestedArraySrc d + "Output.r = TypeOf(v)"))
    Assert.Equal("object", rStr (nestedObjectSrc d + "Output.r = TypeOf(v)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1071")>]
let ``TypeOf_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "TypeOf" FunctionEffect.Pure "Output.r = TypeOf(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1071")>]
let ``TypeOf_DoesNotChangeItsArgument`` () =
    let src = "var v = {a: [1, {b: 2}], c: 'x'}\nvar s = ToJson(v)\nvar t = TypeOf(v)\nvar u = TypeOf(v.a)\nOutput.r = ToJson(v) == s"
    Assert.True(rBool src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1071")>]
let ``TypeOf_OfReadOnlyInput_RaisesNoWriteError`` () =
    Assert.Equal(norm """["object","object"]""",
                 Values.toJson (rIn """{"nested":{"x":[1]}}""" "Output.r = [TypeOf(Input), TypeOf(Input.nested)]"))

// =========================================================================
// 95.2 Coalesce
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1072")>]
let ``Coalesce_TakesOneArrayAndReturnsAnElementOfAnyType`` () =
    let src =
        "var f = x => x\nOutput.r = [TypeOf(Coalesce([null, 1])), TypeOf(Coalesce(['s'])), TypeOf(Coalesce([[1]])), "
        + "TypeOf(Coalesce([{}])), TypeOf(Coalesce([f])), TypeOf(Coalesce([true])), Coalesce(values: [2])]"
    Assert.Equal(norm """["number","string","array","object","function","boolean",2]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1072")>]
let ``Coalesce_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Coalesce()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1072")>]
let ``Coalesce_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Coalesce([1], [2])"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1073")>]
[<InlineData("[1, 2]", "1")>]
[<InlineData("[null, 2, 3]", "2")>]
[<InlineData("[null, null, 'x']", "\"x\"")>]
[<InlineData("[null, [], 5]", "[]")>]
[<InlineData("[o.missing, 4]", "4")>]
[<InlineData("[o.missing, o.a, 6]", "{\"b\":1}")>]
let ``Coalesce_ReturnsTheLowestPositionElementThatIsNotNull`` (arr: string, expected: string) =
    expectJson expected (sprintf "var o = {a: {b: 1}}\nOutput.r = Coalesce(%s)" arr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1073")>]
let ``Coalesce_With1000NullsThenSeven_ReturnsSeven`` () =
    let src = "var a = []\nfor i in 0 to 1000 do a = Append(a, null) end\na = Append(a, 7)\nOutput.r = Coalesce(a)"
    Assert.Equal(7m, Values.asDecimal (rUnlimited src))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1074")>]
[<InlineData("[false, 1]", "false")>]
[<InlineData("[null, false, 1]", "false")>]
[<InlineData("[0, 1]", "0")>]
[<InlineData("[null, 0, 1]", "0")>]
[<InlineData("['', 'a']", "\"\"")>]
[<InlineData("[null, '', 'a']", "\"\"")>]
[<InlineData("[[], 1]", "[]")>]
[<InlineData("[{}, 1]", "{}")>]
let ``Coalesce_TreatsFalseZeroAndEmptyStringAsNotNull`` (arr: string, expected: string) =
    expectJson expected (sprintf "Output.r = Coalesce(%s)" arr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1074")>]
let ``Coalesce_ReturnsInexactZeroAndNegativeZero_NotSkipped`` () =
    let src =
        "var z = SquareRoot(2) - SquareRoot(2)\nvar nz = -z\nvar a = Coalesce([null, z, 1])\nvar b = Coalesce([nz, 1])\n"
        + "Output.r = [TypeOf(a), a == 0, TypeOf(b), b == 0]"
    Assert.Equal(norm """["number",true,"number",true]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1074")>]
let ``Coalesce_ReturnsHostInexactZero_NotSkipped`` () =
    let input = Values.obj [ "z", Values.inexact 0.0; "nz", Values.inexact -0.0 ]
    Assert.Equal(norm "[0,0]", Values.toJson (rVal input "Output.r = [Coalesce([null, Input.z, 1]), Coalesce([Input.nz, 1])]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1075")>]
let ``Coalesce_ReturnsTheArrayElementItself_NotACopy`` () =
    Assert.Equal(9m, rNum "var inner = [1]\nvar r = Coalesce([null, inner])\nr[0] = 9\nOutput.r = inner[0]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1075")>]
let ``Coalesce_ReturnsTheObjectElementItself_NotACopy`` () =
    Assert.Equal(1m, rNum "var inner = {}\nvar r = Coalesce([null, inner])\nr.x = 1\nOutput.r = inner.x")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1075")>]
let ``Coalesce_ReturnsReadOnlyElementStillReadOnly`` () =
    failsIn """{"a":{"x":1}}""" MessageCode.InputValueIsReadOnly "var r = Coalesce([null, Input.a])\nr.x = 2"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1076")>]
[<InlineData("[]")>]
[<InlineData("[null]")>]
[<InlineData("[null, null, null]")>]
let ``Coalesce_WhenEmptyOrAllNull_ReturnsNull`` (arr: string) =
    expectNull (r (sprintf "Output.r = Coalesce(%s)" arr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1076")>]
let ``Coalesce_With10000Nulls_ReturnsNull`` () =
    let src = "var a = []\nfor i in 0 to 100 do a = Append(a, null) end\nvar b = []\nfor j in 0 to 100 do b = Concatenate(b, a) end\nOutput.n = Length(b)\nOutput.r = Coalesce(b)"
    let out = Script.output (Script.runWith { Script.defaults with Limits = noLimits } src)
    Assert.Equal(10000m, Values.asDecimal (Values.prop "n" out))
    expectNull (Values.prop "r" out)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1077")>]
[<InlineData("Coalesce(null)")>]
[<InlineData("Coalesce(x)")>]
[<InlineData("Coalesce(o.missing)")>]
[<InlineData("Coalesce(values: null)")>]
let ``Coalesce_OfNull_ReturnsNull`` (call: string) =
    let res = Script.run (sprintf "var x = null\nvar o = {}\nOutput.r = %s" call)
    Assert.Empty(res.Messages)
    expectNull (Script.output res |> Values.prop "r")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1078")>]
[<InlineData("1")>]
[<InlineData("0")>]
[<InlineData("'a'")>]
[<InlineData("''")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("{}")>]
[<InlineData("{a: 1}")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``Coalesce_WhenArgumentNotArrayOrNull_RaisesArgumentTypeMismatch`` (arg: string) =
    failsIn """{"a":1}""" MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Coalesce(%s)" arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1079")>]
let ``Coalesce_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Coalesce" FunctionEffect.Pure "Output.r = Coalesce([1])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1079")>]
let ``Coalesce_DoesNotChangeItsArgument`` () =
    let src = "var a = [null, {b: [1]}, [2], null]\nvar s = ToJson(a)\nvar c = Coalesce(a)\nOutput.r = ToJson(a) == s and Length(a) == 4"
    Assert.True(rBool src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1079")>]
let ``Coalesce_OfReadOnlyInputArray_RaisesNoWriteError`` () =
    Assert.Equal(norm "1", Values.toJson (rIn """{"a":[null,1]}""" "Output.r = Coalesce(Input.a)"))

// =========================================================================
// 95.3 ToString
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1080")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("1")>]
[<InlineData("1.5")>]
[<InlineData("'s'")>]
[<InlineData("[1]")>]
[<InlineData("{a: 1}")>]
let ``ToString_ForEachType_ReturnsAString`` (expr: string) =
    Assert.Equal("string", rStr (sprintf "Output.r = TypeOf(ToString(%s))" expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1080")>]
let ``ToString_WithNamedArgument_ReturnsString`` () =
    Assert.Equal("2", rStr "Output.r = ToString(value: 2)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1080")>]
let ``ToString_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = ToString()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1080")>]
let ``ToString_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = ToString(1, 2)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1081")>]
[<InlineData("null", "null")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("''", "")>]
[<InlineData("'abc'", "abc")>]
[<InlineData("'null'", "null")>]
[<InlineData("'a\\tb\"c\\\\d'", "a\tb\"c\\d")>]
[<InlineData("'\\uD83D\\uDE00'", "\U0001F600")>]
let ``ToString_OfNullBooleanOrString_ReturnsItsStringForm`` (expr: string, expected: string) =
    let got = rStr (sprintf "Output.r = ToString(%s)" expr)
    Assert.True((expected = got), sprintf "expected [%s] got [%s]" (units expected) (units got))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1082")>]
[<InlineData("0", "0")>]
[<InlineData("0 * -1", "0")>]
[<InlineData("1", "1")>]
[<InlineData("-1", "-1")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("1.0", "1")>]
[<InlineData("100", "100")>]
[<InlineData("0.1 + 0.2", "0.3")>]
[<InlineData("1 / 4", "0.25")>]
[<InlineData("123456789012345678901234567", "123456789012345678901234567")>]
[<InlineData("1e3", "1000")>]
[<InlineData("0.000001", "0.000001")>]
[<InlineData("-2.5", "-2.5")>]
[<InlineData("1e21 * 1.0", "1000000000000000000000")>]
[<InlineData("2.50 * 2", "5")>]
[<InlineData("1e-28", "0.0000000000000000000000000001")>]
[<InlineData("-0.0500", "-0.05")>]
[<InlineData("1.5 * 2", "3")>]
[<InlineData("0.0", "0")>]
[<InlineData("-1.0 + 1", "0")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-7.9228162514264337593543950335", "-7.9228162514264337593543950335")>]
let ``JYRO-REQ-1082 ToString_OfExactNumber_ReturnsDecimalFormWithNoExponentOrTrailingZeros`` (expr: string, expected: string) =
    Assert.Equal(expected, rStr (sprintf "Output.r = ToString(%s)" expr))

/// Exact numbers given by the host (JyroNumber(Decimal)) with a decimal scale
/// that 40.4.6 drops: the string form depends on the value only.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1082")>]
let ``JYRO-REQ-1082 ToString_OfHostExactNumberWithScale_ReturnsTheValueOnly`` () =
    let cases =
        [ 1.500m, "1.5"
          100.00m, "100"
          0.000m, "0"
          Decimal.Negate 0.0m, "0"
          -2.50m, "-2.5"
          0.0000000000000000000000000001m, "0.0000000000000000000000000001"
          1.0000000000000000000000000000m, "1"
          79228162514264337593543950335m, "79228162514264337593543950335" ]
    for (d, expected) in cases do
        let n = Values.exact d
        Assert.True(Values.isExact n, sprintf "the host decimal %M was not held exactly" d)
        let got = Values.asString (rVal (Values.obj [ "n", n ]) "Output.r = ToString(Input.n)")
        Assert.True((expected = got), sprintf "ToString(%M): expected [%s] got [%s]" d expected got)

/// Inexact numbers given by the host (JyroNumber(Double)) with the ECMAScript
/// Number::toString text of each double.
let inexactCases : obj[] seq =
    [ 1.4142135623730951, "1.4142135623730951"
      1e301, "1e+301"
      5e-324, "5e-324"
      1e21, "1e+21"
      1e-7, "1e-7"
      1.23e-18, "1.23e-18"
      0.1, "0.1"
      123456.789, "123456.789"
      1e20, "100000000000000000000"
      1.7976931348623157e308, "1.7976931348623157e+308"
      0.000001, "0.000001"
      -2.5e-7, "-2.5e-7"
      0.30000000000000004, "0.30000000000000004"
      -1.0, "-1"
      0.0, "0"
      -0.0, "0"
      0.5, "0.5"
      100.0, "100"
      4.35, "4.35"
      1.5e-7, "1.5e-7"
      5e-7, "5e-7"
      1.234e-6, "0.000001234"
      1.23e20, "123000000000000000000"
      999999999999999900000.0, "999999999999999900000"
      1.234e21, "1.234e+21"
      -1e21, "-1e+21"
      9007199254740992.0, "9007199254740992"
      2.2250738585072014e-308, "2.2250738585072014e-308"
      1.5e300, "1.5e+300" ]
    |> Seq.map (fun (d, s) -> [| box d; box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1083")>]
[<MemberData(nameof inexactCases)>]
let ``JYRO-REQ-1083 ToString_OfInexactNumber_ReturnsEcmaScriptNumberToString`` (d: float, expected: string) =
    let n = Values.inexact d
    Assert.False(Values.isExact n, "the host double was held exactly; the test needs an inexact number")
    Assert.Equal(expected, Values.asString (rVal (Values.obj [ "n", n ]) "Output.r = ToString(Input.n)"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1083")>]
[<InlineData("SquareRoot(2)", "1.4142135623730951")>]
[<InlineData("1e300 * 10", "1e+301")>]
[<InlineData("5e-324", "5e-324")>]
[<InlineData("1e-30", "1e-30")>]
[<InlineData("1e29", "1e+29")>]
[<InlineData("SquareRoot(2) - SquareRoot(2)", "0")>]
[<InlineData("-(SquareRoot(2) - SquareRoot(2))", "0")>]
let ``JYRO-REQ-1083 ToString_OfInexactScriptNumber_ReturnsEcmaScriptNumberToString`` (expr: string, expected: string) =
    Assert.Equal(expected, rStr (sprintf "Output.r = ToString(%s)" expr))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1084")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("0")>]
[<InlineData("1.50")>]
[<InlineData("-2.5")>]
[<InlineData("0.1 + 0.2")>]
[<InlineData("1e21 * 1.0")>]
[<InlineData("SquareRoot(2)")>]
[<InlineData("1e300 * 10")>]
[<InlineData("5e-324")>]
[<InlineData("1e-30")>]
[<InlineData("''")>]
[<InlineData("'abc'")>]
[<InlineData("'\\uD83D\\uDE00'")>]
[<InlineData("Input.n")>]
[<InlineData("Input.m")>]
let ``ToString_EqualsEmptyStringConcatenation`` (expr: string) =
    let input = Values.obj [ "n", Values.inexact 1e21; "m", Values.inexact 1e-7 ]
    let src = sprintf "var v = %s\nOutput.r = [ToString(v), '' & v, v & '']" expr
    match Values.asList (rVal input src) |> List.map Values.asString with
    | [ a; b; c ] ->
        Assert.Equal(b, a)
        Assert.Equal(c, a)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1085")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("[1, 'a', null, true, [2], {b: 1.5}]")>]
[<InlineData("['a\\tb', '\\u0001', 'q\"']")>]
[<InlineData("{k: 'x\\ny'}")>]
let ``ToString_OfArrayOrObject_EqualsToJson`` (expr: string) =
    let src = sprintf "var v = %s\nOutput.r = [ToString(v), ToJson(v)]" expr
    match Values.asList (r src) |> List.map Values.asString with
    | [ a; b ] -> Assert.Equal(b, a)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1085")>]
let ``ToString_OfObjectWithReorderedAndDeletedKeys_EqualsToJson`` () =
    let src = "var o = {}\no.z = 1\no.a = 2\no.m = 3\ndelete o.a\no.b = [1, {c: 2}]\nOutput.r = [ToString(o), ToJson(o)]"
    match Values.asList (r src) |> List.map Values.asString with
    | [ a; b ] -> Assert.Equal(b, a)
    | other -> Values.fail (sprintf "unexpected %A" other)

/// The key order this object's ToJson must have is 90.11.7's, in the order of
/// 30.5.9, not 95.3.6's (FLOORS 4.1.15): tagged with both. The text is parsed
/// by System.Text.Json, so only the key order is judged, not the spacing or
/// the number text.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-374")>]
[<Trait("requirement", "JYRO-REQ-87")>]
let ``JYRO-REQ-374 ToJson_OfObjectWithPropertyAddedAfterDelete_WritesKeysInOrderAdded`` () =
    let src = "var o = {}\no.z = 1\no.a = 2\no.m = 3\ndelete o.a\no.b = [1, {c: 2}]\nOutput.r = ToJson(o)"
    let text = Values.asString (r src)
    use doc = System.Text.Json.JsonDocument.Parse(text)
    let keys = doc.RootElement.EnumerateObject() |> Seq.map (fun p -> p.Name) |> List.ofSeq
    Assert.Equal<string list>([ "z"; "m"; "b" ], keys)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1085")>]
let ``ToString_OfNestedDepth10_EqualsToJson`` () =
    let src = nestedObjectSrc 10 + "Output.r = ToString(v) == ToJson(v)"
    Assert.True(rBool src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1086")>]
[<InlineData("f")>]
[<InlineData("(a, b) => a")>]
[<InlineData("[1, f]")>]
[<InlineData("{a: {b: [f]}}")>]
[<InlineData("{a: 1, z: f}")>]
let ``JYRO-REQ-1086 ToString_OfValueContainingFunction_RaisesInvalidTypeAsToJsonDoes`` (expr: string) =
    let toJson = Script.run (sprintf "var f = x => x\nOutput.r = ToJson(%s)" expr)
    let toStr = Script.run (sprintf "var f = x => x\nOutput.r = ToString(%s)" expr)
    Script.expectError MessageCode.InvalidType toStr
    Assert.Equal(Script.onlyError toJson, Script.onlyError toStr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1087")>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<InlineData("ci")>]
[<InlineData("{w: [ci]}")>]
let ``ToString_OfCyclicValue_RaisesValueNestingLimitExceeded`` (expr: string) =
    fails MessageCode.ValueNestingLimitExceeded (cyclesDecl + sprintf "Output.r = ToString(%s)" expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1088")>]
let ``ToString_OfHostStringWithLoneSurrogates_ReturnsItUnchanged`` () =
    for s in [ hi; lo; "a" + hi + "b"; lo + hi; hi + hi ] do
        let got = Values.asString (rVal (Values.obj [ "s", Values.str s ]) "Output.r = ToString(Input.s)")
        Assert.True((s = got), sprintf "expected [%s] got [%s]" (units s) (units got))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1088")>]
let ``ToString_OfScriptStringWithLoneSurrogates_ReturnsItUnchanged`` () =
    let src =
        loneDecl
        + "var s = 'a' & hi & 'b'\nvar t = ToString(s)\nvar u = ToString(lo)\n"
        + "Output.r = [t == s, Length(t) == 3, Substring(t, 1, 1) == hi, u == lo, Length(u) == 1]"
    Assert.Equal(norm "[true,true,true,true,true]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1089")>]
let ``ToString_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "ToString" FunctionEffect.Pure "Output.r = ToString(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1089")>]
let ``ToString_DoesNotChangeItsArgument`` () =
    let src = "var v = {a: [1, {b: 2}], c: 'x'}\nvar s = ToJson(v)\nvar t = ToString(v)\nOutput.r = ToJson(v) == s"
    Assert.True(rBool src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1089")>]
let ``ToString_OfReadOnlyInput_RaisesNoWriteError`` () =
    Assert.Equal("{\"a\":[1]}", Values.asString (rIn """{"a":[1]}""" "Output.r = ToString(Input)"))

// =========================================================================
// 95.4 ToBoolean
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1090")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("1")>]
[<InlineData("'s'")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``ToBoolean_ForEachType_ReturnsABoolean`` (expr: string) =
    Assert.Equal("boolean", rStr (sprintf "var v = %s\nOutput.r = TypeOf(ToBoolean(v))" expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1090")>]
let ``ToBoolean_WithNamedArgument_ReturnsBoolean`` () =
    Assert.True(rBool "Output.r = ToBoolean(value: 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1090")>]
let ``ToBoolean_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = ToBoolean()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1090")>]
let ``ToBoolean_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = ToBoolean(1, 2)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1091")>]
[<InlineData("null", false)>]
[<InlineData("false", false)>]
[<InlineData("true", true)>]
[<InlineData("0", false)>]
[<InlineData("0 * -1", false)>]
[<InlineData("SquareRoot(2) - SquareRoot(2)", false)>]
[<InlineData("-(SquareRoot(2) - SquareRoot(2))", false)>]
[<InlineData("Input.z", false)>]
[<InlineData("Input.nz", false)>]
[<InlineData("1", true)>]
[<InlineData("-1", true)>]
[<InlineData("0.5", true)>]
[<InlineData("''", false)>]
[<InlineData("' '", true)>]
[<InlineData("'a'", true)>]
[<InlineData("[]", true)>]
[<InlineData("[0]", true)>]
[<InlineData("{}", true)>]
[<InlineData("{a: null}", true)>]
[<InlineData("x => x", true)>]
let ``ToBoolean_GivesTheTruthinessConversion_AndAgreesWithIf`` (expr: string, expected: bool) =
    let input = Values.obj [ "z", Values.inexact 0.0; "nz", Values.inexact -0.0 ]
    let src = sprintf "var v = %s\nvar c = false\nif v then c = true end\nOutput.r = [ToBoolean(v), c]" expr
    match Values.asList (rVal input src) |> List.map Values.asBool with
    | [ b; c ] ->
        Assert.Equal(expected, b)
        Assert.Equal(c, b)
    | other -> Values.fail (sprintf "unexpected %A" other)

let tinyInexact : obj[] seq =
    [ 5e-324; -5e-324; 1e-323; 2.2250738585072009e-308; 2.2250738585072014e-308; 1e-300; 1e308; -1e308; 1e-10
      Double.MaxValue; -Double.MaxValue ]
    |> Seq.map (fun d -> [| box d |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1092")>]
[<MemberData(nameof tinyInexact)>]
let ``JYRO-REQ-1092 ToBoolean_OfNonZeroInexactNumber_ReturnsTrue`` (d: float) =
    let n = Values.inexact d
    Assert.False(Values.isExact n)
    Assert.True(Values.asBool (rVal (Values.obj [ "n", n ]) "Output.r = ToBoolean(Input.n)"))

/// The smallest subnormal built on the host from its bits, so that no
/// serialization of a test row can round it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1092")>]
let ``JYRO-REQ-1092 ToBoolean_OfTheSmallestSubnormalFromItsBits_ReturnsTrue`` () =
    let tiny = BitConverter.Int64BitsToDouble 1L
    Assert.Equal(Double.Epsilon, tiny)
    for d in [ tiny; -tiny ] do
        let n = Values.inexact d
        Assert.False(Values.isExact n)
        Assert.True(Values.asBool (rVal (Values.obj [ "n", n ]) "Output.r = ToBoolean(Input.n)"), sprintf "ToBoolean(%g) is false" d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1092")>]
let ``JYRO-REQ-1092 ToBoolean_OfScriptSubnormal_ReturnsTrue_AndUnderflowedZeroFalse`` () =
    // 5e-324 / 2 is 2^-1075, half way between 0 and 2^-1074: ties-to-even gives 0.
    Assert.Equal(norm "[true,true,false]", rJson "Output.r = [ToBoolean(5e-324), ToBoolean(-5e-324), ToBoolean(5e-324 / 2)]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1092")>]
let ``JYRO-REQ-1092 ToBoolean_OfInexactSubnormalMadeByArithmetic_ReturnsTrue`` () =
    // SquareRoot(2) is inexact; scaled down twice it is a subnormal near 1.4e-310, not zero.
    Assert.True(rBool "var t = SquareRoot(2) * 1e-160 * 1e-150\nOutput.r = ToBoolean(t) and t != 0")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1093")>]
[<InlineData("'false'", true)>]
[<InlineData("'0'", true)>]
[<InlineData("' '", true)>]
[<InlineData("'null'", true)>]
[<InlineData("'FALSE'", true)>]
[<InlineData("'\\t'", true)>]
[<InlineData("''", false)>]
let ``ToBoolean_OfNonEmptyString_ReturnsTrue`` (expr: string, expected: bool) =
    Assert.Equal(expected, rBool (sprintf "Output.r = ToBoolean(%s)" expr))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1094")>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<InlineData("ci")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``ToBoolean_OfCyclicFunctionOrReadOnlyValue_ReturnsTrueWithNoError`` (expr: string) =
    Assert.True(Values.asBool (rIn """{"a":1}""" (cyclesDecl + sprintf "var f = x => x\nOutput.r = ToBoolean(%s)" expr)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1094")>]
let ``ToBoolean_OfValueDeeperThanNestingBound_ReturnsTrueWithNoError`` () =
    let d = nestingBound.Force() + 10
    Assert.True(rBool (nestedArraySrc d + "Output.r = ToBoolean(v)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1095")>]
let ``ToBoolean_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "ToBoolean" FunctionEffect.Pure "Output.r = ToBoolean(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1095")>]
let ``ToBoolean_DoesNotChangeItsArgument`` () =
    Assert.True(rBool "var v = {a: [1, {b: 2}]}\nvar s = ToJson(v)\nvar t = ToBoolean(v)\nOutput.r = ToJson(v) == s")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1095")>]
let ``ToBoolean_OfReadOnlyInput_RaisesNoWriteError`` () =
    Assert.True(Values.asBool (rIn """{"a":[1]}""" "Output.r = ToBoolean(Input.a)"))

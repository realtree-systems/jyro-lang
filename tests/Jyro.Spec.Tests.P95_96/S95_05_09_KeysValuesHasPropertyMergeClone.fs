/// jyro-spec 95.5 Keys, 95.6 Values, 95.7 HasProperty, 95.8 Merge, 95.9 Clone.
module RealTree.Jyro.Spec.Tests.S95_05_09

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

let private unlimited (src: string) =
    Script.output (Script.runWith { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited } src)

// =========================================================================
// 95.5 Keys
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1096")>]
let ``Keys_TakesAnObjectAndReturnsAnArray`` () =
    Assert.Equal(norm """["array","array",["b"]]""", rJson "Output.r = [TypeOf(Keys({a: 1})), TypeOf(Keys(source: {b: 2})), Keys(source: {b: 2})]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1096")>]
let ``Keys_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Keys()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1096")>]
let ``Keys_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Keys({}, {})"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1830")>]
[<InlineData("{}", "[]")>]
[<InlineData("{a: 1, b: null, c: [1]}", "[\"a\",\"b\",\"c\"]")>]
[<InlineData("{a: 1, b: null}", "[\"a\",\"b\"]")>]
[<InlineData("{'': 1, 'a.b': 2, A: 3, a: 4, '1': 5, '\\uD83D\\uDE00': 6}", "[\"\",\"a.b\",\"A\",\"a\",\"1\",\"\\uD83D\\uDE00\"]")>]
let ``JYRO-REQ-1830 Keys_HoldsEachKeyOnceAsAString`` (obj: string, expected: string) =
    expectJson expected (sprintf "Output.r = Keys(%s)" obj)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1830")>]
[<InlineData("{}", 0)>]
[<InlineData("{a: 1}", 1)>]
[<InlineData("{a: 1, b: null}", 2)>]
[<InlineData("{a: [1, 2], b: {c: 3}, d: null, e: 'x'}", 4)>]
[<InlineData("{'': 0, ' ': 1, '1': 2}", 3)>]
let ``JYRO-REQ-1830 Keys_HasLengthEqualToTheNumberOfProperties_AndNoOtherElement`` (obj: string, count: int) =
    // Nested properties and property values are not keys of the source: the length is the property count of source.
    let src = sprintf "var o = %s\nvar k = Keys(o)\nOutput.r = [Length(k), Length(Distinct(k)), Length(Values(o))]" obj
    Assert.Equal(sprintf "[%d,%d,%d]" count count count, rJson src)
    // Every element of the result is a key of source.
    let allKeys = sprintf "var o = %s\nvar ok = true\nforeach key in Keys(o) do if HasProperty(o, key) == false then ok = false end end\nOutput.r = ok" obj
    Assert.Equal("true", rJson allKeys)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1830")>]
let ``JYRO-REQ-1830 Keys_TheClausesExamples`` () =
    Assert.Equal(norm """["a","b"]""", rJson "Output.r = Keys({a: 1, b: null})")
    Assert.Equal(norm "2", rJson "Output.r = Length(Keys({a: 1, b: null}))")
    Assert.Equal(norm "[]", rJson "Output.r = Keys({})")
    Assert.Equal(norm "0", rJson "Output.r = Length(Keys({}))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1830")>]
let ``JYRO-REQ-1830 Keys_OfHostInput_HasExactlyTheKeysOfTheInput`` () =
    Assert.Equal(norm """[["x","a.b","y"],3]""", Values.toJson (rIn """{"o":{"x":1,"a.b":{"c":2},"y":null}}""" "Output.r = [Keys(Input.o), Length(Keys(Input.o))]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1830")>]
let ``JYRO-REQ-1830 Keys_ElementsAreStrings`` () =
    Assert.Equal(norm """["string","string"]""", rJson "var k = Keys({'1': 1, 'true': 2})\nOutput.r = [TypeOf(k[0]), TypeOf(k[1])]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1830")>]
let ``JYRO-REQ-1830 Keys_ReturnsANewArrayEachCall`` () =
    let src = "var o = {a: 1, b: 2}\nvar k1 = Keys(o)\nvar k2 = Keys(o)\nk1[0] = 'z'\nOutput.r = [k2, Keys(o), o]"
    Assert.Equal(norm """[["a","b"],["a","b"],{"a":1,"b":2}]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1830")>]
let ``JYRO-REQ-1830 Keys_Of1000KeyObject_HoldsEachKeyOnce`` () =
    let src = "var o = {}\nfor i in 1 to 1001 do o['k' & i] = i end\nvar k = Keys(o)\nOutput.n = Length(k)\nOutput.d = Length(Distinct(k))\nOutput.f = k[0]\nOutput.l = k[999]"
    let out = unlimited src
    Assert.Equal(1000m, Values.asDecimal (Values.prop "n" out))
    Assert.Equal(1000m, Values.asDecimal (Values.prop "d" out))
    Assert.Equal("k1", Values.asString (Values.prop "f" out))
    Assert.Equal("k1000", Values.asString (Values.prop "l" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1830")>]
let ``JYRO-REQ-1830 Keys_Of1000KeyObject_HasNoElementThatIsNotAKey`` () =
    // Set equality with the expected key set, not only count and distinctness.
    let src = "var o = {}\nfor i in 1 to 1001 do o['k' & i] = i end\nvar k = Keys(o)\nvar bad = 0\nforeach key in k do if HasProperty(o, key) == false then bad = bad + 1 end end\nvar missing = 0\nfor i in 1 to 1001 do if Contains(k, 'k' & i) == false then missing = missing + 1 end end\nOutput.bad = bad\nOutput.missing = missing\nOutput.n = Length(k)"
    let out = unlimited src
    Assert.Equal(0m, Values.asDecimal (Values.prop "bad" out))
    Assert.Equal(0m, Values.asDecimal (Values.prop "missing" out))
    Assert.Equal(1000m, Values.asDecimal (Values.prop "n" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_FollowsFirstAddedOrder_NotSorted`` () =
    Assert.Equal(norm """["z","a","m"]""", rJson "var o = {}\no.z = 1\no.a = 2\no.m = 3\nOutput.r = Keys(o)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_AfterAssigningExistingKey_KeepsItsPosition`` () =
    Assert.Equal(norm """["z","a","m"]""", rJson "var o = {}\no.z = 1\no.a = 2\no.m = 3\no.z = 9\nOutput.r = Keys(o)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1098")>]
[<InlineData("delete o.a", "[\"z\",\"m\"]")>]
[<InlineData("delete o.z", "[\"a\",\"m\"]")>]
[<InlineData("delete o.m", "[\"z\",\"a\"]")>]
[<InlineData("delete o.z\no.z = 5", "[\"a\",\"m\",\"z\"]")>]
[<InlineData("delete o.a\no.a = 5", "[\"z\",\"m\",\"a\"]")>]
[<InlineData("delete o.a\no.b = 4", "[\"z\",\"m\",\"b\"]")>]
[<InlineData("delete o.z\ndelete o.a\no.a = 5\no.z = 6", "[\"m\",\"a\",\"z\"]")>]
[<InlineData("delete o.z\ndelete o.a\ndelete o.m\no.m = 1\no.z = 2", "[\"m\",\"z\"]")>]
[<InlineData("delete o.a\no.n = 4\no.z = 7\no.p = 8", "[\"z\",\"m\",\"n\",\"p\"]")>]
let ``JYRO-REQ-1098 Keys_AfterDelete_KeepsTheRemainingOrder`` (change: string, expected: string) =
    expectJson expected (sprintf "var o = {}\no.z = 1\no.a = 2\no.m = 3\n%s\nOutput.r = Keys(o)" change)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_TheBookExampleOf30_5_9`` () =
    Assert.Equal(norm """["b","a"]""", rJson "var o = {a: 1, b: 2}\ndelete o.a\no.a = 3\nOutput.r = Keys(o)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_AfterDeleteOnAnObjectFromFromJson_KeepsTheRemainingOrder`` () =
    let src = "var o = FromJson('{\"c\":1,\"b\":2,\"a\":3}')\ndelete o.c\no.d = 4\no.c = 5\nOutput.r = Keys(o)"
    Assert.Equal(norm """["b","a","d","c"]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_OfFromJsonAndHostInput_FollowTheTextOrder`` () =
    Assert.Equal(norm """[["b","a"],["y","x","w"]]""",
                 Values.toJson (rIn """{"y":1,"x":2,"w":3}""" "Output.r = [Keys(FromJson('{\"b\":1,\"a\":2}')), Keys(Input)]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_Of1000KeysWithEveryOtherDeletedAndReAdded_PutsTheReAddedLast`` () =
    let src =
        "var o = {}\nfor i in 0 to 1000 do o['k' & i] = i end\n"
        + "for i in 0 to 500 do delete o['k' & (i * 2)] end\n"
        + "for i in 0 to 500 do o['k' & (i * 2)] = i end\n"
        + "var k = Keys(o)\nOutput.r = [Length(k), k[0], k[1], k[499], k[500], k[501], k[999]]"
    Assert.Equal(norm """[1000,"k1","k3","k999","k0","k2","k998"]""", Values.toJson (Values.prop "r" (unlimited src)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1098")>]
let ``JYRO-REQ-1098 Keys_Of1000KeysAddedInReverseOrder_KeepsAddedOrder`` () =
    let src = "var o = {}\nfor i in 1000 downto 0 do o['k' & PadLeft('' & i, 4, '0')] = i end\nvar k = Keys(o)\nOutput.r = [k[0], k[1], k[999]]"
    Assert.Equal(norm """["k1000","k0999","k0001"]""", Values.toJson (Values.prop "r" (unlimited src)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1099")>]
let ``Keys_OfReadOnlyInput_ReturnsAWritableArray`` () =
    let src = "var k = Keys(Input)\nk[0] = 'x'\nk = Insert(k, 0, 'y')\nk[1] = 'w'\nvar kb = Keys(Input.b)\nkb[0] = 'q'\nOutput.r = [k, kb, Input]"
    Assert.Equal(norm """[["y","w","b"],["q"],{"a":1,"b":{"c":2}}]""", Values.toJson (rIn """{"a":1,"b":{"c":2}}""" src))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1100")>]
[<InlineData("Keys(null)")>]
[<InlineData("Keys(x)")>]
[<InlineData("Keys(o.missing)")>]
let ``Keys_OfNull_ReturnsAnEmptyWritableArray`` (call: string) =
    let src = sprintf "var x = null\nvar o = {}\nvar k = %s\nOutput.r = [TypeOf(k), Length(k)]\nk = Append(k, 1)\nOutput.w = k" call
    let out = Script.output (Script.run src)
    Assert.Equal(norm """["array",0]""", Values.toJson (Values.prop "r" out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1101")>]
[<InlineData("[]")>]
[<InlineData("[1, 2]")>]
[<InlineData("'abc'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``Keys_WhenArgumentNotObjectOrNull_RaisesArgumentTypeMismatch`` (arg: string) =
    failsIn "[1,2]" MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Keys(%s)" arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1102")>]
let ``Keys_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Keys" FunctionEffect.Pure "Output.r = Keys({})"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1102")>]
let ``Keys_DoesNotChangeSource`` () =
    Assert.True(rBool "var o = {z: 1, a: [2], m: {b: 3}}\nvar s = ToJson(o)\nvar k = Keys(o)\nOutput.r = ToJson(o) == s")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1102")>]
let ``Keys_OfReadOnlySource_RaisesNoWriteError`` () =
    Assert.Equal(norm """["a"]""", Values.toJson (rIn """{"a":1}""" "Output.r = Keys(Input)"))

// =========================================================================
// 95.6 Values
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1103")>]
let ``Values_TakesAnObjectAndReturnsAnArray`` () =
    Assert.Equal(norm """["array",[2]]""", rJson "Output.r = [TypeOf(Values({a: 1})), Values(source: {b: 2})]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1103")>]
let ``Values_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Values()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1103")>]
let ``Values_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Values({}, {})"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1833")>]
[<InlineData("{}", "[]")>]
[<InlineData("{a: 1, b: null, c: [1], d: 1}", "[1,null,[1],1]")>]
[<InlineData("{a: 1, b: 1, c: null}", "[1,1,null]")>]
[<InlineData("{a: {b: 1}, c: [2, 3]}", "[{\"b\":1},[2,3]]")>]
let ``JYRO-REQ-1833 Values_HoldsOneValuePerProperty`` (obj: string, expected: string) =
    expectJson expected (sprintf "Output.r = Values(%s)" obj)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1833")>]
[<InlineData("{}", 0)>]
[<InlineData("{a: 1}", 1)>]
[<InlineData("{a: 1, b: 1, c: null}", 3)>]
[<InlineData("{a: [1, 2, 3], b: {c: 4, d: 5}}", 2)>]
[<InlineData("{'': null, ' ': null}", 2)>]
let ``JYRO-REQ-1833 Values_HasLengthEqualToTheNumberOfProperties_AndNoOtherElement`` (obj: string, count: int) =
    // Equal lengths with Keys, so that the two pair up by position.
    let src = sprintf "var o = %s\nOutput.r = [Length(Values(o)), Length(Keys(o))]" obj
    Assert.Equal(sprintf "[%d,%d]" count count, rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1833")>]
let ``JYRO-REQ-1833 Values_TheClausesExamples`` () =
    Assert.Equal(norm "[1,1,null]", rJson "Output.r = Values({a: 1, b: 1, c: null})")
    Assert.Equal(norm "3", rJson "Output.r = Length(Values({a: 1, b: 1, c: null}))")
    Assert.Equal(norm "[]", rJson "Output.r = Values({})")
    Assert.Equal(norm "0", rJson "Output.r = Length(Values({}))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1833")>]
let ``JYRO-REQ-1833 Values_OfHostInput_HasExactlyOneValuePerProperty`` () =
    Assert.Equal(norm """[[1,{"c":2},null],3]""", Values.toJson (rIn """{"o":{"x":1,"a.b":{"c":2},"y":null}}""" "Output.r = [Values(Input.o), Length(Values(Input.o))]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1833")>]
let ``JYRO-REQ-1833 Values_ReturnsANewArrayEachCall`` () =
    let src = "var o = {a: 1, b: 2}\nvar v1 = Values(o)\nvar v2 = Values(o)\nv1[0] = 9\nOutput.r = [v2, o]"
    Assert.Equal(norm """[[1,2],{"a":1,"b":2}]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1833")>]
let ``JYRO-REQ-1833 Values_Of1000KeyObject_HoldsEachValueOnce`` () =
    let src = "var o = {}\nfor i in 1 to 1001 do o['k' & i] = i end\nvar v = Values(o)\nOutput.r = [Length(v), Sum(v), v[0], v[999]]"
    Assert.Equal(norm "[1000,500500,1,1000]", Values.toJson (Values.prop "r" (unlimited src)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1105")>]
[<InlineData("var o = {}\no.z = 1\no.a = 2\no.m = 3")>]
[<InlineData("var o = {}\no.z = 1\no.a = 2\no.z = 7")>]
[<InlineData("var o = {z: 1, a: [2], m: null}\ndelete o.a\no.a = 'x'")>]
[<InlineData("var o = FromJson('{\"b\":1,\"a\":{\"c\":2},\"b2\":null}')")>]
[<InlineData("var o = Input")>]
[<InlineData("var o = {}\nfor i in 200 downto 1 do o['k' & i] = i * 2 end")>]
let ``Values_PositionIHoldsTheValueOfKeysPositionI`` (decl: string) =
    let src =
        decl
        + "\nvar k = Keys(o)\nvar v = Values(o)\nvar ok = Length(k) == Length(v)\n"
        + "for i in 0 to Length(k) do if v[i] != o[k[i]] then ok = false end end\nOutput.r = ok"
    Assert.True(Values.asBool (Values.prop "r" (Script.output (Script.runWith { Script.defaults with Input = JsonInput """{"y":[1],"x":{"a":null},"w":3}"""; Limits = Some JyroExecutionOptions.Unlimited } src))))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1106")>]
let ``Values_ReturnsThePropertyValuesThemselves`` () =
    Assert.Equal(norm """{"a":[9],"b":{"x":7}}""", rJson "var o = {a: [1], b: {x: 1}}\nvar v = Values(o)\nv[0][0] = 9\nv[1].x = 7\nOutput.r = o")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1106")>]
let ``Values_OfReadOnlyInput_ElementsStayReadOnly`` () =
    failsIn """{"a":{"x":1}}""" MessageCode.InputValueIsReadOnly "var v = Values(Input)\nv[0].x = 2"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1107")>]
[<InlineData("Values(null)")>]
[<InlineData("Values(x)")>]
[<InlineData("Values(o.missing)")>]
let ``Values_OfNull_ReturnsAnEmptyArray`` (call: string) =
    Assert.Equal(norm """["array",0]""", rJson (sprintf "var x = null\nvar o = {}\nvar v = %s\nOutput.r = [TypeOf(v), Length(v)]" call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1108")>]
[<InlineData("[]")>]
[<InlineData("[1]")>]
[<InlineData("'a'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``Values_WhenArgumentNotObjectOrNull_RaisesArgumentTypeMismatch`` (arg: string) =
    failsIn "[1]" MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Values(%s)" arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1109")>]
let ``Values_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Values" FunctionEffect.Pure "Output.r = Values({})"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1109")>]
let ``Values_DoesNotChangeSource`` () =
    Assert.True(rBool "var o = {z: 1, a: [2], m: {b: 3}}\nvar s = ToJson(o)\nvar v = Values(o)\nOutput.r = ToJson(o) == s")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1109")>]
let ``Values_OfReadOnlySource_RaisesNoWriteError`` () =
    Assert.Equal(norm "[1]", Values.toJson (rIn """{"a":1}""" "Output.r = Values(Input)"))

// =========================================================================
// 95.7 HasProperty
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1110")>]
let ``HasProperty_TakesObjectAndStringAndReturnsBoolean`` () =
    Assert.Equal(norm """["boolean",true,true,true]""",
                 rJson "Output.r = [TypeOf(HasProperty({a: 1}, 'a')), HasProperty({a: 1}, 'a'), HasProperty(source: {a: 1}, name: 'a'), HasProperty(name: 'a', source: {a: 1})]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1110")>]
let ``HasProperty_WithOneArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = HasProperty({})"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1110")>]
let ``HasProperty_WithThreeArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = HasProperty({}, 'a', 1)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1111")>]
[<InlineData("'a'", true)>]
[<InlineData("'A'", true)>]
[<InlineData("'a.b'", true)>]
[<InlineData("''", true)>]
[<InlineData("'\\u00E9'", true)>]
[<InlineData("'\\uD83D\\uDE00'", true)>]
[<InlineData("'b'", false)>]
[<InlineData("'e\\u0301'", false)>]
[<InlineData("'a '", false)>]
[<InlineData("'gone'", false)>]
[<InlineData("'\\uD83D'", false)>]
let ``HasProperty_IsTrueExactlyForTheSameCodeUnits`` (name: string, expected: bool) =
    let src =
        sprintf "var o = {a: 1, A: 2, 'a.b': 3, '': 4, '\\u00E9': 5, '\\uD83D\\uDE00': 6, gone: 7}\ndelete o.gone\nvar n = %s\nOutput.r = HasProperty(o, n)" name
    // '\uD83D' alone is not a valid literal (20.7.4); make the lone surrogate with Substring instead.
    let src = src.Replace("var n = '\\uD83D'", "var n = Substring('\\uD83D\\uDE00', 0, 1)")
    Assert.Equal(expected, rBool src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1111")>]
let ``HasProperty_DottedNameIsOneKey_NotAPath`` () =
    Assert.Equal(norm "[false,true]", rJson "Output.r = [HasProperty({a: {b: 1}}, 'a.b'), HasProperty({'a.b': 1}, 'a.b')]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1112")>]
[<InlineData("HasProperty({a: null}, 'a')")>]
[<InlineData("HasProperty(o, 'b')")>]
[<InlineData("HasProperty(FromJson('{\"c\":null}'), 'c')")>]
[<InlineData("HasProperty(Input, 'd')")>]
let ``HasProperty_ForKeyWithNullValue_ReturnsTrue`` (call: string) =
    Assert.True(Values.asBool (rIn """{"d":null}""" (sprintf "var o = {}\no.b = null\nOutput.r = %s" call)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1113")>]
[<InlineData("HasProperty(null, 'a')")>]
[<InlineData("HasProperty(null, '')")>]
[<InlineData("HasProperty(null, null)")>]
[<InlineData("HasProperty(x, 'a')")>]
let ``HasProperty_OfNullSource_ReturnsFalse`` (call: string) =
    Assert.False(rBool (sprintf "var x = null\nOutput.r = %s" call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1114")>]
[<InlineData("HasProperty([], 'a')")>]
[<InlineData("HasProperty([1], '0')")>]
[<InlineData("HasProperty([1], null)")>]
[<InlineData("HasProperty([1], 0)")>]
[<InlineData("HasProperty([1], [1])")>]
[<InlineData("HasProperty(Input, 'length')")>]
let ``HasProperty_WhenSourceIsArray_RaisesArgumentTypeMismatch`` (call: string) =
    failsIn "[1]" MessageCode.ArgumentTypeMismatch (sprintf "Output.r = %s" call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1865")>]
[<InlineData("HasProperty(5, \"a\")")>]
[<InlineData("HasProperty(1.5, \"a\")")>]
[<InlineData("HasProperty(\"abc\", \"length\")")>]
[<InlineData("HasProperty(\"\", \"\")")>]
[<InlineData("HasProperty(true, \"a\")")>]
[<InlineData("HasProperty(false, null)")>]
[<InlineData("HasProperty(0, \"a\")")>]
[<InlineData("HasProperty(-3, \"a\")")>]
[<InlineData("HasProperty(5, 5)")>]
[<InlineData("HasProperty(\"abc\", 0)")>]
[<InlineData("HasProperty(true, true)")>]
[<InlineData("HasProperty(5, null)")>]
[<InlineData("HasProperty(5, \"\")")>]
[<InlineData("HasProperty(\"a\", \"a\")")>]
[<InlineData("HasProperty(source: 5, name: \"a\")")>]
let ``JYRO-REQ-1865 HasProperty_WhenSourceIsNumberStringOrBoolean_RaisesArgumentTypeMismatch`` (call: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "Output.r = %s" call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1865")>]
[<InlineData("5")>]
[<InlineData("\"abc\"")>]
[<InlineData("true")>]
[<InlineData("1.5")>]
let ``JYRO-REQ-1865 HasProperty_ScalarSourceFromInput_RaisesArgumentTypeMismatchAndReturnsNoBoolean`` (json: string) =
    let res = Script.runJson (sprintf "{\"s\":%s}" json) "Output.r = HasProperty(Input.s, \"a\")\nOutput.after = 1"
    Script.expectError MessageCode.ArgumentTypeMismatch res
    match Script.value res with
    | Some v ->
        match v with
        | :? JyroObject as o -> Assert.False(fst (o.TryGetValue "r"), "Output.r was set")
        | _ -> ()
    | None -> ()

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1115")>]
[<InlineData("var o = {}\no[''] = 1", "null", true)>]
[<InlineData("var o = {a: 1}", "null", false)>]
[<InlineData("var o = {a: 1}", "x", false)>]
[<InlineData("var o = {'null': 1}", "null", false)>]
[<InlineData("var o = {'null': 1, '': 2}", "x", true)>]
let ``HasProperty_ReadsNullNameAsEmptyString`` (decl: string, name: string, expected: bool) =
    Assert.Equal(expected, rBool (sprintf "var x = null\n%s\nOutput.r = HasProperty(o, %s)" decl name))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1116")>]
[<InlineData("1")>]
[<InlineData("0")>]
[<InlineData("1.5")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("-1")>]
[<InlineData("1.50")>]
[<InlineData("1 == 1")>]
[<InlineData("SquareRoot(2)")>]
[<InlineData("Input.n")>]
[<InlineData("Input.i")>]
[<InlineData("Input.b")>]
[<InlineData("name: 1")>]
let ``JYRO-REQ-1116 HasProperty_WhenNameIsNumberOrBoolean_RaisesArgumentTypeMismatch`` (name: string) =
    let input = Values.obj [ "n", Values.exact 1m; "i", Values.inexact 1.5; "b", Values.bool true ]
    let o = "var o = {'1': true, 'true': 1, '0': 1, '1.5': 1, 'false': 1, '-1': 1, '1.50': 1, '1.4142135623730951': 1}"
    let call = if name.StartsWith "name:" then sprintf "HasProperty(source: o, %s)" name else sprintf "HasProperty(o, %s)" name
    expectRuntimeError MessageCode.ArgumentTypeMismatch (Script.runInput input (sprintf "%s\nOutput.r = %s" o call))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1116")>]
let ``JYRO-REQ-1116 HasProperty_OfEmptyObjectWithNumberName_RaisesArgumentTypeMismatch`` () =
    fails MessageCode.ArgumentTypeMismatch "Output.r = HasProperty({}, 1)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1117")>]
[<InlineData("['a']")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("{a: 1}")>]
[<InlineData("f")>]
let ``HasProperty_WhenNameIsContainerOrFunction_RaisesArgumentTypeMismatch`` (name: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = HasProperty({a: 1}, %s)" name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1118")>]
let ``HasProperty_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "HasProperty" FunctionEffect.Pure "Output.r = HasProperty({}, 'a')"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1118")>]
let ``HasProperty_ForMissingKey_DoesNotAddIt`` () =
    Assert.Equal(norm """[false,["a"],"{\"a\":1}"]""", rJson "var o = {a: 1}\nvar h = HasProperty(o, 'b')\nOutput.r = [h, Keys(o), ToJson(o)]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1118")>]
let ``HasProperty_OfReadOnlySource_RaisesNoWriteError`` () =
    Assert.Equal(norm "[false,true]", Values.toJson (rIn """{"a":1}""" "Output.r = [HasProperty(Input, 'x'), HasProperty(Input, 'a')]"))

// =========================================================================
// 95.8 Merge
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1119")>]
let ``Merge_TakesOneArrayAndReturnsAnObject`` () =
    Assert.Equal(norm """["object",{"a":1,"b":2}]""", rJson "Output.r = [TypeOf(Merge([{a: 1}])), Merge(values: [{a: 1}, {b: 2}])]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1119")>]
let ``Merge_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Merge()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1119")>]
let ``Merge_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Merge([{}], [{}])"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1836")>]
[<InlineData("[{a: 1}, {b: 2}, {c: 3}]", "[\"a\",\"b\",\"c\"]")>]
[<InlineData("[{b: 2}, {a: 1}]", "[\"b\",\"a\"]")>]
[<InlineData("[{a: 1}, null, {b: 2}, {a: 3}]", "[\"a\",\"b\"]")>]
[<InlineData("[{}, {}]", "[]")>]
[<InlineData("[]", "[]")>]
[<InlineData("[null, {a: 1}, null]", "[\"a\"]")>]
[<InlineData("[{a: 1, b: 2}, {b: 3, c: 4}, {c: 5, d: 6}]", "[\"a\",\"b\",\"c\",\"d\"]")>]
let ``JYRO-REQ-1836 Merge_HoldsEachPropertyInElementOrder`` (arr: string, keys: string) =
    expectJson keys (sprintf "Output.r = Keys(Merge(%s))" arr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1836")>]
let ``JYRO-REQ-1836 Merge_TheClausesExamples_HaveExactlyTheUnionOfTheKeys`` () =
    Assert.Equal(norm """[["a","b"],2]""", rJson "var m = Merge([{a: 1}, null, {b: 2}, {a: 3}])\nOutput.r = [Keys(m), Length(Keys(m))]")
    Assert.Equal(norm """[[],0,"object"]""", rJson "var m = Merge([{}, {}])\nOutput.r = [Keys(m), Length(Keys(m)), TypeOf(m)]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1836")>]
[<InlineData("[{a: 1}, {b: 2}]")>]
[<InlineData("[{a: 1}, {a: 2}, {a: 3}]")>]
[<InlineData("[null, {x: 1}, null, {y: null}, {x: 2}]")>]
[<InlineData("[{a: {b: 1}}, {a: {c: 2}}]")>]
let ``JYRO-REQ-1836 Merge_HasNoKeyOutsideTheUnionOfTheElementKeys_AndEachKeyOnce`` (arr: string) =
    // Every key of the result is a key of an element, every element key is in the result, and none repeats.
    let src =
        sprintf "var xs = %s\nvar m = Merge(xs)\nvar seen = []\nforeach e in xs do if TypeOf(e) == 'object' then foreach k in Keys(e) do if Contains(seen, k) == false then seen = Append(seen, k) end end end end\nvar bad = 0\nforeach k in Keys(m) do if Contains(seen, k) == false then bad = bad + 1 end end\nOutput.r = [Length(Keys(m)), Length(seen), bad, Length(Distinct(Keys(m)))]" arr
    let out = Script.outputOf src |> Values.prop "r" |> Values.asList |> List.map Values.asDecimal
    Assert.Equal(out.[1], out.[0])
    Assert.Equal(0m, out.[2])
    Assert.Equal(out.[0], out.[3])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1836")>]
let ``JYRO-REQ-1836 Merge_OfOneElement_ReturnsANewObject`` () =
    Assert.Equal(norm """[{"a":1},{"a":1,"x":1}]""", rJson "var o = {a: 1}\nvar r = Merge([o])\nr.x = 1\nOutput.r = [o, r]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1836")>]
let ``JYRO-REQ-1836 Merge_Of100Objects_HoldsEveryProperty`` () =
    let src = "var a = []\nfor i in 1 to 101 do var e = {}\ne['k' & i] = i\na = Append(a, e) end\nvar m = Merge(a)\nOutput.r = [Length(Keys(m)), Keys(m)[0], Keys(m)[99], m.k50]"
    Assert.Equal(norm """[100,"k1","k100",50]""", Values.toJson (Values.prop "r" (unlimited src)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1121")>]
[<InlineData("[{a: 1}, {a: 2}]", "{\"a\":2}")>]
[<InlineData("[{a: 1}, {a: 2}, {a: 3}]", "{\"a\":3}")>]
[<InlineData("[{a: 1}, {a: null}]", "{\"a\":null}")>]
[<InlineData("[{a: 1}, {}, null]", "{\"a\":1}")>]
[<InlineData("[{a: 1}, {b: 1}, {a: 3}]", "{\"a\":3,\"b\":1}")>]
let ``Merge_ForKeyInSeveralElements_TakesTheHighestPosition`` (arr: string, expected: string) =
    expectJson expected (sprintf "Output.r = Merge(%s)" arr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1122")>]
[<InlineData("[{a: 1, b: 2}, {b: 3, c: 4, a: 5}]", "{\"a\":5,\"b\":3,\"c\":4}")>]
[<InlineData("[{x: 1}, {y: 1}, {x: 2}]", "{\"x\":2,\"y\":1}")>]
let ``Merge_KeyKeepsPositionOfFirstAddition`` (arr: string, expected: string) =
    let src = sprintf "var m = Merge(%s)\nOutput.r = m\nOutput.k = Keys(m)" arr
    let out = Script.outputOf src
    Assert.Equal(expected, Values.toJson (Values.prop "r" out))
    let keys = Values.asList (Values.prop "k" out) |> List.map Values.asString
    let expectedKeys = Values.json expected :?> JyroObject |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    Assert.Equal<string list>(expectedKeys, keys)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1123")>]
let ``Merge_CopiesPropertyValuesByReference`` () =
    let src = "var inner = {x: 1}\nvar arr = [1]\nvar r = Merge([{n: inner}, {a: arr}])\nr.n.x = 9\nr.a[0] = 5\nOutput.r = [inner.x, arr[0]]"
    Assert.Equal(norm "[9,5]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1123")>]
let ``Merge_DoesNotMergeNestedObjects`` () =
    let src = "var second = {x: 2}\nvar r = Merge([{n: {x: 1, y: 1}}, {n: second}])\nr.n.z = 3\nOutput.r = [HasProperty(r.n, 'y'), second.z, r.n.x]"
    Assert.Equal(norm "[false,3,2]", rJson src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1124")>]
[<InlineData("[null]", "{}")>]
[<InlineData("[{a: 1}, null, {b: 2}]", "{\"a\":1,\"b\":2}")>]
[<InlineData("[null, {a: 1}, null]", "{\"a\":1}")>]
[<InlineData("[o.missing, {a: 1}]", "{\"a\":1}")>]
let ``Merge_IgnoresNullElements`` (arr: string, expected: string) =
    expectJson expected (sprintf "var o = {}\nOutput.r = Merge(%s)" arr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1125")>]
[<InlineData("[{a: 1}, 1]")>]
[<InlineData("[0]")>]
[<InlineData("['']")>]
[<InlineData("['a']")>]
[<InlineData("[true]")>]
[<InlineData("[false]")>]
[<InlineData("[[]]")>]
[<InlineData("[[{a: 1}]]")>]
[<InlineData("[f]")>]
[<InlineData("[1, {a: 1}, {b: 2}]")>]
[<InlineData("[{a: 1}, 'x', {b: 2}]")>]
[<InlineData("[{a: 1}, {b: 2}, []]")>]
[<InlineData("[{a: 1}, null, 2]")>]
[<InlineData("[null, false]")>]
[<InlineData("[SquareRoot(2)]")>]
[<InlineData("[x => x, {a: 1}]")>]
[<InlineData("[Input.arr]")>]
[<InlineData("[Input.o, Input.s]")>]
[<InlineData("Input.mixed")>]
[<InlineData("values: [{a: 1}, 'x']")>]
let ``JYRO-REQ-1125 Merge_WhenElementNotObjectOrNull_RaisesArgumentTypeMismatch`` (arr: string) =
    let input = """{"arr":[{"a":1}],"o":{"a":1},"s":"t","mixed":[{"a":1},3]}"""
    expectRuntimeError MessageCode.ArgumentTypeMismatch
        (Script.runJson input (sprintf "var f = x => x\nOutput.r = Merge(%s)" arr))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1126")>]
[<InlineData("Merge([])")>]
[<InlineData("Merge(null)")>]
[<InlineData("Merge(x)")>]
let ``Merge_OfEmptyOrNull_ReturnsAnEmptyWritableObject`` (call: string) =
    Assert.Equal(norm """[0,{"w":1}]""", rJson (sprintf "var x = null\nvar m = %s\nvar n = Length(Keys(m))\nm.w = 1\nOutput.r = [n, m]" call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1127")>]
[<InlineData("{a: 1}")>]
[<InlineData("{}")>]
[<InlineData("1")>]
[<InlineData("0")>]
[<InlineData("'a'")>]
[<InlineData("true")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``Merge_WhenArgumentNotArrayOrNull_RaisesArgumentTypeMismatch`` (arg: string) =
    failsIn """{"a":1}""" MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Merge(%s)" arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1128")>]
let ``Merge_OfReadOnlyElements_ReturnsAWritableObject`` () =
    let src = "var r = Merge([Input.a, {z: 1}])\nr.w = 2\nr.z = 3\nr.x = 4\nvar r2 = Merge([Input])\nr2.new = 1\nOutput.r = [r, r2.new]"
    Assert.Equal(norm """[{"x":4,"z":3,"w":2},1]""", Values.toJson (rIn """{"a":{"x":{"y":1}},"b":[1]}""" src))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1128")>]
let ``Merge_PropertyValueFromReadOnlyObjectElement_StaysReadOnly`` () =
    failsIn """{"a":{"x":{"y":1}},"b":[1]}""" MessageCode.InputValueIsReadOnly "var r = Merge([Input.a, {z: 1}])\nr.x.y = 5"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1128")>]
let ``Merge_PropertyValueFromReadOnlyArray_StaysReadOnly`` () =
    failsIn """{"a":{"x":{"y":1}},"b":[1]}""" MessageCode.InputValueIsReadOnly "var r2 = Merge([Input])\nr2.b[0] = 2"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1129")>]
let ``Merge_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Merge" FunctionEffect.Pure "Output.r = Merge([])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1129")>]
let ``Merge_DoesNotChangeTheArrayOrAnyElement`` () =
    let src = "var e0 = {a: 1, b: [1]}\nvar e1 = {a: 2, c: {d: 1}}\nvar arr = [e0, null, e1]\nvar s = ToJson(arr)\nvar m = Merge(arr)\nOutput.r = [ToJson(arr) == s, e0.a, e1.a, Length(arr)]"
    Assert.Equal(norm "[true,1,2,3]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1129")>]
let ``Merge_OfReadOnlyElements_RaisesNoWriteError`` () =
    Assert.Equal(norm """{"a":2}""", Values.toJson (rIn """{"x":{"a":1},"y":{"a":2}}""" "Output.r = Merge([Input.x, Input.y])"))

// =========================================================================
// 95.9 Clone
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1130")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("1")>]
[<InlineData("'s'")>]
[<InlineData("[1]")>]
[<InlineData("{a: 1}")>]
[<InlineData("x => x")>]
let ``Clone_ReturnsAValueOfTheSameType`` (expr: string) =
    Assert.True(rBool (sprintf "var v = %s\nOutput.r = TypeOf(Clone(v)) == TypeOf(v)" expr))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1130")>]
let ``Clone_WithNamedArgument_ReturnsTheValue`` () =
    Assert.Equal(1m, rNum "Output.r = Clone(value: 1)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1130")>]
let ``Clone_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Clone()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1130")>]
let ``Clone_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Clone(1, 2)"

let private cloneX = "var x = {a: [1, {b: [2, {c: 3}]}], d: {}}\nvar c = Clone(x)\n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1131")>]
let ``Clone_IsEqualToTheArgument`` () =
    Assert.True(rBool (cloneX + "Output.r = c == x"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1131")>]
[<InlineData("c.z = 1")>]
[<InlineData("c.a[0] = 9")>]
[<InlineData("c.a[1].q = 9")>]
[<InlineData("c.a[1].b[0] = 9")>]
[<InlineData("c.a[1].b[1].c = 9")>]
[<InlineData("c.d.e = 9")>]
let ``Clone_MakesANewContainerAtEveryLevel`` (write: string) =
    Assert.Equal(norm """{"a":[1,{"b":[2,{"c":3}]}],"d":{}}""", rJson (cloneX + write + "\nOutput.r = x"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1131")>]
let ``Clone_OfEmptyContainers_ReturnsNewContainers`` () =
    Assert.Equal(norm "[[],{}]", rJson "var a = []\nvar o = {}\nvar ca = Clone(a)\nvar co = Clone(o)\nca = Append(ca, 1)\nco.k = 1\nOutput.r = [a, o]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1132")>]
let ``Clone_ChangeToCopyNotVisibleThroughArgument`` () =
    Assert.Equal(norm """{"a":[1,{"b":[2,{"c":3}]}],"d":{}}""", rJson (cloneX + "c.a[0] = 9\nc.new = 1\ndelete c.d\nOutput.r = x"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1132")>]
let ``Clone_ChangeToArgumentNotVisibleThroughCopy`` () =
    Assert.Equal(norm """{"a":[1,{"b":[2,{"c":3}]}],"d":{}}""", rJson (cloneX + "x.a[0] = 7\nx.a[1].b = null\nx.extra = 1\nOutput.r = c"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1132")>]
let ``Clone_OfArrayRoot_IsIndependentBothWays`` () =
    Assert.Equal(norm "[[1,[2]],[9,[3]]]", rJson "var x = [1, [2]]\nvar c = Clone(x)\nc[0] = 9\nx[1][0] = 2\nc[1][0] = 3\nOutput.r = [x, c]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1133")>]
let ``Clone_KeepsKeyOrderAtEveryLevel`` () =
    let src =
        "var o = {}\no.z = 1\no.a = 2\no.m = 3\no.z = 4\no.q = 5\ndelete o.a\no.n = {}\no.n.y = 1\no.n.b = 2\n"
        + "var c = Clone(o)\nOutput.r = [Keys(c) == Keys(o), Keys(c.n) == Keys(o.n), ToJson(c) == ToJson(o)]"
    Assert.Equal(norm """[true,true,true]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1133")>]
let ``Clone_Of500KeysInReverseOrder_KeepsOrder`` () =
    let src = "var o = {}\nfor i in 500 downto 1 do o['k' & i] = i end\nvar c = Clone(o)\nOutput.r = Keys(c) == Keys(o) and Keys(c)[0] == 'k500'"
    Assert.True(Values.asBool (Values.prop "r" (unlimited src)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1134")>]
let ``Clone_OfSharedArray_MakesSeparateCopies`` () =
    let src = "var s = [1]\nvar x = {a: s, b: s, c: [s, s]}\nvar c = Clone(x)\nc.a[0] = 9\nOutput.r = [c.b[0], c.c[0][0], c.c[1][0], s[0]]"
    Assert.Equal(norm "[1,1,1,1]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1134")>]
let ``Clone_OfSharedObject_MakesSeparateCopies`` () =
    let src = "var o = {v: 1}\nvar x = {p: o, q: o}\nvar c = Clone(x)\nc.p.v = 9\nOutput.r = [c.q.v, o.v]"
    Assert.Equal(norm "[1,1]", rJson src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1135")>]
[<InlineData("0.1")>]
[<InlineData("0.1 + 0.2")>]
[<InlineData("1.50")>]
[<InlineData("1234567890.123456789012345678")>]
[<InlineData("0 * -1")>]
[<InlineData("SquareRoot(2)")>]
[<InlineData("5e-324")>]
[<InlineData("1e308")>]
[<InlineData("1e-30")>]
let ``Clone_OfNumber_ReturnsAnEqualNumberWithTheSameStringForm`` (expr: string) =
    let src = sprintf "var n = %s\nvar c = Clone(n)\nvar w = Clone([n])[0]\nOutput.r = [c == n, ToString(c) == ToString(n), w == n, ToString(w) == ToString(n)]" expr
    Assert.Equal(norm "[true,true,true,true]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1135")>]
let ``Clone_OfExactNumber_StaysExact`` () =
    Assert.Equal(norm """[true,"0.3",true]""", rJson "var s = Clone(0.1) + Clone(0.2)\nOutput.r = [s == 0.3, ToString(s), Clone({n: 0.1}).n + 0.2 == 0.3]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1135")>]
let ``Clone_OfHostNumbers_KeepsRepresentation`` () =
    let input = Values.obj [ "e", Values.exact 0.1m; "i", Values.inexact 0.1 ]
    let v = rVal input "Output.r = [Clone(Input.e), Clone(Input.i)]"
    match Values.asList v with
    | [ e; i ] ->
        Assert.True(Values.isExact e, "exact number became inexact")
        Assert.Equal(0.1m, Values.asDecimal e)
        Assert.Equal(0.1, Values.asFloat i)
    | other -> Values.fail (sprintf "unexpected %A" other)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1136")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("''")>]
[<InlineData("'abc'")>]
[<InlineData("'\\uD83D\\uDE00'")>]
[<InlineData("hi")>]
[<InlineData("lo")>]
let ``Clone_OfNullBooleanOrString_ReturnsAnEqualValue`` (expr: string) =
    let src = loneDecl + sprintf "var v = %s\nvar c = Clone(v)\nOutput.r = [c == v, TypeOf(c) == TypeOf(v)]" expr
    Assert.Equal(norm "[true,true]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1136")>]
let ``Clone_OfHostLoneSurrogateString_ReturnsSameCodeUnits`` () =
    let s = "a" + hi
    Assert.Equal(s, Values.asString (rVal (Values.obj [ "s", Values.str s ]) "Output.r = Clone(Input.s)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1137")>]
let ``Clone_OfFunctionValue_ReturnsTheSameFunctionValue`` () =
    let src = "var g = x => x * 2\nvar h = x => x * 2\nOutput.r = [Clone(g) == g, Clone(g) == h, Clone([g])[0] == g, Clone({f: g}).f == g, Map([2], Clone(g))[0]]"
    Assert.Equal(norm "[true,false,true,true,4]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1138")>]
let ``Clone_OfReadOnlyInput_IsWritableAtEveryLevel`` () =
    let src = "var c = Clone(Input)\nc.x = 1\nc.a.b[0] = 5\nc.a.b[1].c = 3\nvar d = Clone(Input.a.b)\nd[0] = 7\nd[1].c = 8\nvar e = Clone(Input)\ndelete e.a\nOutput.r = [c, d]\nOutput.i = Input"
    let out = Script.output (Script.runJson """{"a":{"b":[1,{"c":2}]}}""" src)
    Assert.Equal(norm """[{"a":{"b":[5,{"c":3}]},"x":1},[7,{"c":8}]]""", Values.toJson (Values.prop "r" out))
    Assert.Equal(norm """{"a":{"b":[1,{"c":2}]}}""", Values.toJson (Values.prop "i" out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1139")>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<InlineData("ci")>]
let ``Clone_OfCyclicValue_RaisesValueNestingLimitExceeded`` (expr: string) =
    fails MessageCode.ValueNestingLimitExceeded (cyclesDecl + sprintf "Output.r = Clone(%s)" expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1139")>]
let ``Clone_AtTheNestingBound_Succeeds`` () =
    let d = nestingBound.Force()
    Assert.True(rBool (nestedArraySrc d + "var c = Clone(v)\nOutput.r = TypeOf(c) == 'array'"))
    Assert.True(rBool (nestedObjectSrc d + "var c = Clone(v)\nOutput.r = TypeOf(c) == 'object'"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1139")>]
let ``Clone_OneAboveTheNestingBound_RaisesValueNestingLimitExceeded`` () =
    let d = nestingBound.Force() + 1
    fails MessageCode.ValueNestingLimitExceeded (nestedArraySrc d + "var c = Clone(v)")
    fails MessageCode.ValueNestingLimitExceeded (nestedObjectSrc d + "var c = Clone(v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1140")>]
let ``Clone_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Clone" FunctionEffect.Pure "Output.r = Clone(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1140")>]
let ``Clone_DoesNotChangeItsArgument`` () =
    Assert.True(rBool "var v = {a: [1, {b: 2}], c: 'x'}\nvar s = ToJson(v)\nvar t = Clone(v)\nOutput.r = ToJson(v) == s")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1140")>]
let ``Clone_OfReadOnlyInput_RaisesNoWriteError`` () =
    Assert.Equal(norm """{"a":[1]}""", Values.toJson (rIn """{"a":[1]}""" "Output.r = Clone(Input)"))

/// jyro-spec 92.6 IndexOf, 92.7 Reverse, 92.8 Slice.
module RealTree.Jyro.Spec.Tests.S92_06_08_IndexOfReverseSlice

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P92Support

// =============================================================================
// 92.6 IndexOf
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-620")>]
let ``IndexOf_TakesStringArrayOrNullAndAnyValue_ReturnsNumber`` () =
    assertSignature "IndexOf" [ "source", false; "value", false ]
    typesAre "number"
        [ "IndexOf(\"abc\", \"b\")"; "IndexOf([1, 2], 2)"; "IndexOf(null, 1)"
          "IndexOf([null], null)"; "IndexOf([true], true)"; "IndexOf([1], 1)"; "IndexOf([\"s\"], \"s\")"
          "IndexOf([[1]], [1])"; "IndexOf([{a: 1}], {a: 1})" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-621")>]
let ``IndexOf_InArray_ReturnsLowestPositionEqualUnderEquality`` () =
    evalsTo "var f = x => x\nvar one = Power(1, 0.5)"
        [ "IndexOf([1, 2, 3, 2], 2)", "1"
          "IndexOf([[1, 2]], [1, 2])", "0"
          "IndexOf([{a: 1, b: 2}], {b: 2, a: 1})", "0"
          "IndexOf([\"1\"], 1)", "-1"
          "IndexOf([true], 1)", "-1"
          "IndexOf([null, 0], null)", "0"
          "IndexOf([one], 1)", "0"
          "IndexOf([5, one], one)", "1"
          "IndexOf([f], f)", "0"
          "IndexOf([f], x => x)", "-1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-622")>]
let ``IndexOf_InArrayWithNoEqualElement_ReturnsMinusOne`` () =
    evalsTo ""
        [ "IndexOf([1, 2, 3], 4)", "-1"; "IndexOf([], 1)", "-1"; "IndexOf([[1]], [2])", "-1"
          "IndexOf([\"a\"], \"A\")", "-1"; "IndexOf([{a: 1}], {a: 1, b: 2})", "-1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-623")>]
let ``IndexOf_InString_ReturnsFirstOrdinalCodeUnitPosition`` () =
    evalsTo ""
        [ "IndexOf(\"hello\", \"l\")", "2"
          "IndexOf(\"abcabc\", \"c\")", "2"
          "IndexOf(\"ABC\", \"b\")", "-1"
          "IndexOf(\"Strasse\", \"ss\")", "4"
          "IndexOf(\"Stra\\u00DFe\", \"ss\")", "-1" ]
    let o =
        outOnInput "{\"s\": \"\\ud83d\\ude00x\", \"pre\": \"\\u00e9\", \"dec\": \"e\\u0301\"}"
            "Output.x = IndexOf(Input.s, \"x\")\nOutput.a = IndexOf(Input.dec, Input.pre)\nOutput.b = IndexOf(Input.pre, Input.dec)"
    assertJson "2" (Values.prop "x" o)
    assertJson "-1" (Values.prop "a" o)
    assertJson "-1" (Values.prop "b" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-624")>]
let ``IndexOf_InStringWhenValueDoesNotOccur_ReturnsMinusOne`` () =
    evalsTo "" [ "IndexOf(\"abc\", \"d\")", "-1"; "IndexOf(\"\", \"a\")", "-1"; "IndexOf(\"ab\", \"abc\")", "-1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-625")>]
let ``IndexOf_InStringOfEmptyValue_ReturnsZero`` () =
    evalsTo "" [ "IndexOf(\"abc\", \"\")", "0"; "IndexOf(\"\", \"\")", "0" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-626")>]
let ``IndexOf_WhenSourceNullOrStringWithNullValue_ReturnsMinusOneWithoutError`` () =
    evalsTo ""
        [ "IndexOf(null, 1)", "-1"; "IndexOf(null, \"a\")", "-1"; "IndexOf(null, null)", "-1"
          "IndexOf(null, [1])", "-1"; "IndexOf(null, {})", "-1"; "IndexOf(null, true)", "-1"
          "IndexOf(\"abc\", null)", "-1"; "IndexOf(\"\", null)", "-1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-627")>]
let ``IndexOf_InStringOfNonStringNonNullValue_RaisesStringOrArrayRequired`` () =
    errorsWith MessageCode.StringOrArrayRequired ""
        [ "IndexOf(\"abc\", 1)"; "IndexOf(\"1\", 1)"; "IndexOf(\"true\", true)"
          "IndexOf(\"abc\", [1])"; "IndexOf(\"abc\", {})"; "IndexOf(\"abc\", x => x)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-628")>]
let ``JYRO-REQ-628 IndexOf_WhenSourceIsBooleanNumberObjectOrFunction_RaisesStringOrArrayRequiredWhateverValue`` () =
    errorsWith MessageCode.StringOrArrayRequired ""
        [ "IndexOf(true, 1)"; "IndexOf(false, \"a\")"; "IndexOf(5, 5)"; "IndexOf(1.5, \"1\")"
          "IndexOf({a: 1}, 1)"; "IndexOf({a: 1}, \"a\")"; "IndexOf(x => x, 1)"
          "IndexOf(5, null)"; "IndexOf(true, null)"; "IndexOf(false, null)"; "IndexOf({a: 1}, null)"
          "IndexOf(x => x, null)"; "IndexOf({}, null)"; "IndexOf(0, null)"
          "IndexOf(5, [1])"; "IndexOf({a: 1}, {a: 1})"; "IndexOf(true, x => x)" ]
    // the same through a variable and through Input, whose missing property is null
    errorsWith MessageCode.StringOrArrayRequired "var n = 5\nvar o = {a: 1}" [ "IndexOf(n, Input.missing)"; "IndexOf(o, Input.missing)" ]
    // controls: a null, string or array source with a null value raises no error
    allSucceed "" [ "IndexOf(null, null)"; "IndexOf(\"a\", null)"; "IndexOf([1], null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-629")>]
let ``JYRO-REQ-629 IndexOf_ReturnsExactNumber`` () =
    allExact ""
        [ "IndexOf([5, 6], 6)"; "IndexOf([5], 7)"; "IndexOf([], 7)"; "IndexOf(\"abc\", \"c\")"; "IndexOf(\"abc\", \"z\")"
          "IndexOf(\"abc\", \"\")"; "IndexOf(\"abc\", null)"; "IndexOf(null, 1)" ]
    let o = outOnInput "{\"a\": [3, 4], \"s\": \"xyz\"}" "Output.a = IndexOf(Input.a, 4)\nOutput.s = IndexOf(Input.s, \"z\")"
    Assert.True(Values.isExact (Values.prop "a" o), "IndexOf in an Input array is not exact")
    Assert.True(Values.isExact (Values.prop "s" o), "IndexOf in an Input string is not exact")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-630")>]
let ``IndexOf_DoesNotChangeSourceOrValue`` () =
    let o =
        out "var a = [3, {k: 1}, [2]]\nvar v = {k: 1}\nvar s1 = Clone(a)\nvar s2 = Clone(v)\nvar n = Length(a)\nvar r1 = IndexOf(a, v)\nvar r2 = IndexOf(\"abc\", \"b\")\nOutput.a = a == s1\nOutput.v = v == s2\nOutput.n = Length(a) == n\nOutput.r1 = r1"
    assertJson "true" (Values.prop "a" o)
    assertJson "true" (Values.prop "v" o)
    assertJson "true" (Values.prop "n" o)
    assertJson "1" (Values.prop "r1" o)
    let o2 = outOnInput "{\"a\": [3, {\"k\": 1}], \"v\": {\"k\": 1}}" "Output.r = IndexOf(Input.a, Input.v)"
    assertJson "1" (Values.prop "r" o2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-631")>]
let ``IndexOf_HasEffectPure`` () = assertPure "IndexOf" "IndexOf([1, 2], 2)"

// =============================================================================
// 92.7 Reverse
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-632")>]
let ``Reverse_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "Reverse" [ "items", false ]
    typesAre "array" [ "Reverse([1, 2])"; "Reverse(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-633")>]
let ``Reverse_PutsElementAtIAtLengthMinusOneMinusI`` () =
    evalsTo ""
        [ "Reverse([1, 2, 3])", "[3, 2, 1]"; "Reverse([1, 2, 3, 4])", "[4, 3, 2, 1]"
          "Reverse([1])", "[1]"; "Reverse([])", "[]"
          "Reverse([1, [2, 3]])", "[[2, 3], 1]"; "Reverse([null, 1])", "[1, null]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-634")>]
let ``Reverse_OfNull_ReturnsEmptyArray`` () = evalsTo "" [ "Reverse(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-635")>]
let ``Reverse_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Reverse(%s)" v) [ "Reverse([1, 2])"; "Reverse(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-636")>]
let ``Reverse_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Reverse(%s)" a)
    assertNewArray "[7]" (fun a -> sprintf "Reverse(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "Reverse(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-637")>]
let ``Reverse_DoesNotChangeOrderOfItems`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Reverse(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-638")>]
let ``Reverse_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Reverse([%s, 0])" x) 1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-639")>]
let ``Reverse_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Reverse(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-640")>]
let ``Reverse_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3]], \"e\": []}" (fun a -> sprintf "Reverse(%s)" a) (Some "Reverse(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-641")>]
let ``Reverse_HasEffectPure`` () = assertPure "Reverse" "Reverse([1, 2])"

// =============================================================================
// 92.8 Slice
// =============================================================================

let private a5 = "var a = [10, 11, 12, 13, 14]"
let private a3 = "var a = [10, 11, 12]"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-642")>]
let ``Slice_TakesArrayOrNullStartAndOptionalFinish_ReturnsArray`` () =
    assertSignature "Slice" [ "items", false; "start", false; "finish", true ]
    typesAre "array" [ "Slice([1, 2, 3], 1)"; "Slice([1, 2, 3], 0, 2)"; "Slice(null, 0)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2011")>]
let ``Slice_ReturnsElementsFromStartUpToButNotIncludingFinish`` () =
    evalsTo a5
        [ "Slice(a, 1, 3)", "[11, 12]"; "Slice(a, 2, 3)", "[12]"; "Slice(a, 0, 5)", "[10, 11, 12, 13, 14]"; "Slice(a, 4, 5)", "[14]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2012")>]
let ``Slice_WithoutFinish_UsesLength`` () =
    evalsTo a5
        [ "Slice(a, 2)", "[12, 13, 14]"; "Slice(a, 0)", "[10, 11, 12, 13, 14]"; "Slice(a, 4)", "[14]"; "Slice(a, 5)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2013")>]
let ``Slice_WhenFinishAboveLength_UsesLength`` () =
    evalsTo a5
        [ "Slice(a, 1, 5)", "[11, 12, 13, 14]"; "Slice(a, 1, 6)", "[11, 12, 13, 14]"
          "Slice(a, 1, 1000)", "[11, 12, 13, 14]"; "Slice(a, 1, 2147483647)", "[11, 12, 13, 14]" ]
    errorsWith MessageCode.IntegerOutOfRange a5 [ "Slice(a, 1, 2147483648)"; "Slice(a, 1, 1e20)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-646")>]
let ``Slice_WhenFinishAtOrBelowStart_ReturnsEmptyArray`` () =
    evalsTo a5 [ "Slice(a, 3, 3)", "[]"; "Slice(a, 3, 2)", "[]"; "Slice(a, 3, 0)", "[]"; "Slice(a, 0, 0)", "[]" ]
    errorsWith MessageCode.NonNegativeIntegerRequired a5 [ "Slice(a, 3, -1)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2014")>]
let ``Slice_WhenStartAboveLength_ReturnsEmptyArray`` () =
    evalsTo a5
        [ "Slice(a, 5)", "[]"; "Slice(a, 6)", "[]"; "Slice(a, 6, 10)", "[]"; "Slice(a, 2147483647)", "[]"; "Slice([], 1)", "[]" ]
    errorsWith MessageCode.IntegerOutOfRange a5 [ "Slice(a, 2147483648)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-648")>]
let ``JYRO-REQ-648 Slice_StartAndFinishAreNonNegativeWholeQuantities`` () =
    // 90.3.1: not an integer
    errorsWith MessageCode.IntegerRequired a3
        [ "Slice(a, 1.5)"; "Slice(a, 0.5)"; "Slice(a, -1.5)"; "Slice(a, 0, 2.5)"; "Slice(a, 0, -1.5)"
          "Slice(a, 1.5, -1)"; "Slice(null, 1.5)"; "Slice(a, Power(2, 0.5))" ]
    // 90.3.3: negative (start is checked through all of its checks before finish, 90.3.8)
    errorsWith MessageCode.NonNegativeIntegerRequired a3
        [ "Slice(a, -1)"; "Slice(a, 0, -1)"; "Slice(a, -2147483648)"; "Slice(a, -1, 1.5)"; "Slice(a, 3, -1)" ]
    // 90.3.2: outside the 32-bit range, checked before the sign
    errorsWith MessageCode.IntegerOutOfRange a3
        [ "Slice(a, 2147483648)"; "Slice(a, -2147483649)"; "Slice(a, 1e20)"; "Slice(a, 0, 2147483648)"; "Slice(a, 0, -2147483649)" ]
    // the type of arr comes first (90.3.8)
    errorsWith MessageCode.ArgumentTypeMismatch a3 [ "Slice(5, 1.5)" ]
    // an integer value is accepted whatever its representation
    evalsTo (a3 + "\nvar one = Power(1, 0.5)")
        [ "Slice(a, 1.0)", "[11, 12]"; "Slice(a, one)", "[11, 12]"; "Slice(a, 0, 2 * one)", "[10, 11]"; "Slice(a, 0, 2.00)", "[10, 11]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-649")>]
let ``JYRO-REQ-649 Slice_WhenStartNotNumberOrFinishNeitherNumberNorNull_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch a3
        [ "Slice(a, \"1\")"; "Slice(a, null)"; "Slice(a, true)"; "Slice(a, [1])"; "Slice(a, {})"; "Slice(a, x => x)"
          "Slice(a, 0, \"2\")"; "Slice(a, 0, true)"; "Slice(a, 0, [1])"; "Slice(a, 0, {})"; "Slice(a, 0, x => x)"
          "Slice(a, \"1\", 2)"; "Slice(a, Input.missing)" ]
    // a finish of null is not refused (its meaning is 92.8.9's)
    allSucceed a3 [ "Slice(a, 0, null)"; "Slice(a, 1, Input.missing)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-650")>]
let ``JYRO-REQ-650 Slice_NullFinish_IsFinishNotGiven`` () =
    evalsTo a3
        [ "Slice(a, 1, null)", "[11, 12]"; "Slice(a, 0, null)", "[10, 11, 12]"; "Slice(a, 3, null)", "[]"; "Slice(a, 4, null)", "[]"
          "Slice(a, 1, Input.missing)", "[11, 12]"; "Slice(items: a, start: 2, finish: null)", "[12]"
          "Slice(null, 0, null)", "[]" ]
    allTrue a3 [ "Slice(a, 1, null) == Slice(a, 1)"; "Slice(a, 0, null) == Slice(a, 0)"; "Slice(a, 3, null) == Slice(a, 3)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2015")>]
let ``Slice_OfNullArr_ReturnsEmptyArray`` () =
    evalsTo "" [ "Slice(null, 0)", "[]"; "Slice(null, 0, 5)", "[]"; "Slice(null, 3)", "[]" ]
    errorsWith MessageCode.IntegerRequired "" [ "Slice(null, 1.5)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2016")>]
let ``Slice_WhenArrIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Slice(%s, 0, 3)" v) [ "Slice([1, 2], 0, 3)"; "Slice(null, 0, 3)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2017")>]
let ``Slice_ReturnsNewArray_WritesToResultDoNotChangeArr`` () =
    assertNewArray "[{\"k\": 1}, 2, [3], 4]" (fun a -> sprintf "Slice(%s, 0, 3)" a)
    assertNewArray "[{\"k\": 1}, 2, [3], 4]" (fun a -> sprintf "Slice(%s, 0)" a)
    assertNewArray "[]" (fun a -> sprintf "Slice(%s, 0)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2018")>]
let ``Slice_DoesNotChangeArr`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3], 4]" (fun a -> sprintf "Slice(%s, 0, 3)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2019")>]
let ``Slice_ResultElementsAreTheElementsOfArrNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Slice([%s, 1], 0)" x) 0
    assertElementsShared (fun x -> sprintf "Slice([0, %s, 1], 1, 2)" x) 0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-656")>]
let ``Slice_OnReadOnlyArr_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3], 4]}" (fun a -> sprintf "Slice(%s, 0, 3)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2020")>]
let ``Slice_OnReadOnlyArr_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3], 4], \"e\": []}" (fun a -> sprintf "Slice(%s, 0, 3)" a) (Some "Slice(Input.e, 0)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-658")>]
let ``Slice_HasEffectPure`` () = assertPure "Slice" "Slice([1, 2], 0, 3)"

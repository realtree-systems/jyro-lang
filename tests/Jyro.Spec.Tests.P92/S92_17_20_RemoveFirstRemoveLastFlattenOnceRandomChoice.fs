/// jyro-spec 92.17 RemoveFirst, 92.18 RemoveLast, 92.19 FlattenOnce, 92.20 RandomChoice.
module RealTree.Jyro.Spec.Tests.S92_17_20_RemoveFirstRemoveLastFlattenOnceRandomChoice

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P92Support

// =============================================================================
// 92.17 RemoveFirst
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-771")>]
let ``RemoveFirst_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "RemoveFirst" [ "items", false ]
    typesAre "array" [ "RemoveFirst([1, 2])"; "RemoveFirst(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-772")>]
let ``RemoveFirst_ReturnsElementsFromPositionOne`` () =
    evalsTo ""
        [ "RemoveFirst([1, 2, 3])", "[2, 3]"; "RemoveFirst([1])", "[]"; "RemoveFirst([[1], 2])", "[2]"
          "RemoveFirst([null, null])", "[null]"; "RemoveFirst([1, 1, 2])", "[1, 2]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-773")>]
let ``RemoveFirst_OfEmptyOrNull_ReturnsEmptyArrayWithoutError`` () =
    evalsTo "" [ "RemoveFirst([])", "[]"; "RemoveFirst(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-774")>]
let ``RemoveFirst_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "RemoveFirst(%s)" v) [ "RemoveFirst([1, 2])"; "RemoveFirst(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-775")>]
let ``RemoveFirst_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[0, {\"k\": 1}, 2, [3]]" (fun a -> sprintf "RemoveFirst(%s)" a)
    assertNewArray "[5]" (fun a -> sprintf "RemoveFirst(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "RemoveFirst(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-776")>]
let ``RemoveFirst_DoesNotChangeItemsAndItsLength`` () =
    assertUnchanged "[0, {\"k\": 1}, 2, [3]]" (fun a -> sprintf "RemoveFirst(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-777")>]
let ``RemoveFirst_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "RemoveFirst([0, %s])" x) 0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-778")>]
let ``RemoveFirst_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [0, {\"k\": 1}, 2, [3]]}" (fun a -> sprintf "RemoveFirst(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-779")>]
let ``RemoveFirst_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [0, {\"k\": 1}, 2, [3]], \"e\": []}" (fun a -> sprintf "RemoveFirst(%s)" a) (Some "RemoveFirst(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-780")>]
let ``RemoveFirst_HasEffectPure`` () = assertPure "RemoveFirst" "RemoveFirst([1, 2])"

// =============================================================================
// 92.18 RemoveLast
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-781")>]
let ``RemoveLast_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "RemoveLast" [ "items", false ]
    typesAre "array" [ "RemoveLast([1, 2])"; "RemoveLast(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-782")>]
let ``RemoveLast_ReturnsElementsFromZeroToLengthMinusTwo`` () =
    evalsTo ""
        [ "RemoveLast([1, 2, 3])", "[1, 2]"; "RemoveLast([1])", "[]"; "RemoveLast([1, [2]])", "[1]"
          "RemoveLast([null, null])", "[null]"; "RemoveLast([2, 1, 1])", "[2, 1]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-783")>]
let ``RemoveLast_OfEmptyOrNull_ReturnsEmptyArrayWithoutError`` () =
    evalsTo "" [ "RemoveLast([])", "[]"; "RemoveLast(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-784")>]
let ``RemoveLast_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "RemoveLast(%s)" v) [ "RemoveLast([1, 2])"; "RemoveLast(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-785")>]
let ``RemoveLast_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, [3], 0]" (fun a -> sprintf "RemoveLast(%s)" a)
    assertNewArray "[5]" (fun a -> sprintf "RemoveLast(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "RemoveLast(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-786")>]
let ``RemoveLast_DoesNotChangeItemsAndItsLength`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3], 0]" (fun a -> sprintf "RemoveLast(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-787")>]
let ``RemoveLast_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "RemoveLast([%s, 0])" x) 0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-788")>]
let ``RemoveLast_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3], 0]}" (fun a -> sprintf "RemoveLast(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-789")>]
let ``RemoveLast_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3], 0], \"e\": []}" (fun a -> sprintf "RemoveLast(%s)" a) (Some "RemoveLast(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-790")>]
let ``RemoveLast_HasEffectPure`` () = assertPure "RemoveLast" "RemoveLast([1, 2])"

// =============================================================================
// 92.19 FlattenOnce
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-791")>]
let ``FlattenOnce_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "FlattenOnce" [ "items", false ]
    typesAre "array" [ "FlattenOnce([[1]])"; "FlattenOnce(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-792")>]
let ``FlattenOnce_GivesElementsOfEachArrayElementElseTheElement`` () =
    evalsTo ""
        [ "FlattenOnce([[1, 2], 3, [4]])", "[1, 2, 3, 4]"; "FlattenOnce([[], [1]])", "[1]"
          "FlattenOnce([1, 2])", "[1, 2]"; "FlattenOnce([])", "[]"; "FlattenOnce([\"ab\", [\"c\"]])", "[\"ab\", \"c\"]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-793")>]
let ``FlattenOnce_KeepsArraysTwoOrMoreLevelsDeepWhole`` () =
    evalsTo "" [ "FlattenOnce([[1, [2, 3]], [[4]]])", "[1, [2, 3], [4]]"; "FlattenOnce([[[[5]]]])", "[[[5]]]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-794")>]
let ``JYRO-REQ-794 FlattenOnce_KeepsNullElements`` () =
    evalsTo ""
        [ "FlattenOnce([null, 1, null])", "[null, 1, null]"; "FlattenOnce([null, [2]])", "[null, 2]"; "FlattenOnce([[null]])", "[null]"
          "FlattenOnce([null, [null]])", "[null, null]"; "FlattenOnce([null])", "[null]"; "FlattenOnce([[1], null, [null, 2]])", "[1, null, null, 2]" ]
    let o = outOnInput "{\"a\": [null, [3], null]}" "Output.r = FlattenOnce(Input.a)\nOutput.n = Length(Output.r)"
    assertJson "[null, 3, null]" (Values.prop "r" o)
    assertJson "3" (Values.prop "n" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-795")>]
let ``FlattenOnce_KeepsObjectElementsWhole`` () =
    evalsTo "" [ "FlattenOnce([{a: [1]}, 2])", "[{\"a\": [1]}, 2]"; "FlattenOnce([[{b: [3]}]])", "[{\"b\": [3]}]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1770")>]
let ``JYRO-REQ-1770 FlattenOnce_OfCyclicElement_DoesNotRaiseValueNestingLimitExceeded`` () =
    let o = out "var c = [1, 0]\nc[1] = c\nvar r = FlattenOnce([c])\nOutput.n = Length(r)\nOutput.e0 = r[0]\nr[1][0] = 7\nOutput.same = c[0]"
    assertJson "2" (Values.prop "n" o)
    assertJson "1" (Values.prop "e0" o)
    assertJson "7" (Values.prop "same" o)

/// Element 1 of the result is `c` itself: a write through it is seen through `c`,
/// and a write through `c` is seen through it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1770")>]
let ``JYRO-REQ-1770 FlattenOnce_OfCyclicElement_ElementOneIsCItself`` () =
    let o = out "var c = [1, 0]\nc[1] = c\nvar r = FlattenOnce([c])\nc[0] = 9\nOutput.viaR = r[1][0]\nr[1][0] = 5\nOutput.viaC = c[0]\nOutput.isArray = TypeOf(r[1]) == \"array\""
    assertJson "9" (Values.prop "viaR" o)
    assertJson "5" (Values.prop "viaC" o)
    assertJson "true" (Values.prop "isArray" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1770")>]
let ``JYRO-REQ-1770 FlattenOnce_OfElementDeeperThanNestingBound_DoesNotRaise`` () =
    let n = maxDepth ()
    let r = runLoops (n + 20) (sprintf "%s\nOutput.n = Length(FlattenOnce([d]))" (deepArrayScript "d" (n + 5)))
    assertJson "1" (Values.prop "n" (Script.output r))

/// With `a` an array of N + 1 arrays nested one inside the next, FlattenOnce([a])
/// is an array whose one element is a[0] itself.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1770")>]
let ``JYRO-REQ-1770 FlattenOnce_OfAnArrayNestedNPlusOneDeep_GivesAZeroItself`` () =
    let n = maxDepth ()
    let script =
        sprintf "%s\nvar r = FlattenOnce([d])\nOutput.n = Length(r)\nr[0][0] = 7\nOutput.seen = d[0][0]\nOutput.isArray = TypeOf(r[0]) == \"array\""
            (deepArrayScript "d" (n + 1))
    let o = Script.output (runLoops (n + 20) script)
    assertJson "1" (Values.prop "n" o)
    assertJson "7" (Values.prop "seen" o)
    assertJson "true" (Values.prop "isArray" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-797")>]
let ``FlattenOnce_OfNull_ReturnsEmptyArray`` () = evalsTo "" [ "FlattenOnce(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-798")>]
let ``FlattenOnce_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "FlattenOnce(%s)" v) [ "FlattenOnce([1, 2])"; "FlattenOnce(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-799")>]
let ``FlattenOnce_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[[{\"k\": 1}, [2]], 3, [4]]" (fun a -> sprintf "FlattenOnce(%s)" a)
    assertNewArray "[1, 2]" (fun a -> sprintf "FlattenOnce(%s)" a)
    assertNewArray "[7]" (fun a -> sprintf "FlattenOnce(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "FlattenOnce(%s)" a)
    // Nor is the result the one array element whose elements it gives.
    let o = out "var inner = [1, 2]\nvar r = FlattenOnce([inner])\nr[0] = 99\nOutput.inner = inner"
    assertJson "[1, 2]" (Values.prop "inner" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-800")>]
let ``FlattenOnce_DoesNotChangeItems`` () =
    assertUnchanged "[[{\"k\": 1}, [2]], 3, [4]]" (fun a -> sprintf "FlattenOnce(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-801")>]
let ``FlattenOnce_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    let o =
        out "var o = {k: 1}\nvar f = x => x\nvar r = FlattenOnce([[o], f])\nr[0].k = 2\nOutput.k = o.k\nOutput.f = r[1] == f\nvar n = [1]\nvar r2 = FlattenOnce([[n]])\nr2[0][0] = 7\nOutput.n0 = n[0]"
    assertJson "2" (Values.prop "k" o)
    assertJson "true" (Values.prop "f" o)
    assertJson "7" (Values.prop "n0" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-802")>]
let ``FlattenOnce_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [[{\"k\": 1}, [2]], 3, [4]]}" (fun a -> sprintf "FlattenOnce(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-803")>]
let ``FlattenOnce_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [[{\"k\": 1}, [2]], 3, [4]], \"e\": [[]]}" (fun a -> sprintf "FlattenOnce(%s)" a) (Some "FlattenOnce(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-804")>]
let ``FlattenOnce_HasEffectPure`` () = assertPure "FlattenOnce" "FlattenOnce([1, [2]])"

// =============================================================================
// 92.20 RandomChoice
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-805")>]
let ``RandomChoice_TakesArrayOrNull_ReturnsValueOfAnyType`` () =
    assertSignature "RandomChoice" [ "items", false ]
    [ "[1]", "number"; "[{a: 1}]", "object"; "[[1]]", "array"; "[\"s\"]", "string"; "[true]", "boolean"; "[x => x]", "function"; "null", "null" ]
    |> List.map (fun (arr, t) -> arr, (fun () -> assertJson (sprintf "\"%s\"" t) (eval (sprintf "TypeOf(RandomChoice(%s))" arr))))
    |> checkAll

[<Fact>]
[<Trait("requirement", "JYRO-REQ-806")>]
let ``RandomChoice_ReturnsAnElementWithEachPositionEquallyProbable`` () =
    let draws = 12000
    let script =
        sprintf "var c = [0, 0, 0, 0]\nvar bad = 0\nvar i = 0\nwhile i < %d do\n  var x = RandomChoice([0, 1, 2, 3])\n  if x == 0 or x == 1 or x == 2 or x == 3 then\n    c[x] = c[x] + 1\n  else\n    bad = bad + 1\n  end\n  i = i + 1\nend\nOutput.c = c\nOutput.bad = bad\nvar sevens = 0\nvar j = 0\nwhile j < 100 do\n  if RandomChoice([7]) == 7 then sevens = sevens + 1 end\n  j = j + 1\nend\nOutput.sevens = sevens\nvar a = [{i: 0}, {i: 1}]\nvar hits = 0\nvar k = 0\nwhile k < 200 do\n  var r = RandomChoice(a)\n  if r == a[0] or r == a[1] then hits = hits + 1 end\n  k = k + 1\nend\nOutput.hits = hits"
            draws
    let o = Script.output (runLoops (draws + 400) script)
    assertJson "0" (Values.prop "bad" o)
    assertJson "100" (Values.prop "sevens" o)
    assertJson "200" (Values.prop "hits" o)
    let counts = Values.asList (Values.prop "c" o) |> List.map Values.asFloat
    Assert.Equal(float draws, List.sum counts)
    Assert.All(counts, fun c -> Assert.True(c > 0.0, sprintf "a position was never chosen: %A" counts))
    let expected = float draws / 4.0
    let chi = counts |> List.sumBy (fun c -> (c - expected) ** 2.0 / expected)
    // df 3, p = 0.000001: a uniform choice fails about once in a million runs.
    Assert.True(chi < 27.9, sprintf "chi-square %f rejects uniformity; counts %A" chi counts)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-807")>]
let ``RandomChoice_OfEmptyOrNull_ReturnsNullWithoutError`` () =
    evalsTo "" [ "RandomChoice([])", "null"; "RandomChoice(null)", "null" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-808")>]
let ``RandomChoice_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "RandomChoice(%s)" v) [ "RandomChoice([1, 2])"; "RandomChoice(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-809")>]
let ``RandomChoice_ReturnsTheElementItselfNotACopy`` () =
    let o =
        out "var o = {k: 1}\nvar inner = [1]\nvar a = [o]\nvar r = RandomChoice(a)\nr.k = 2\nOutput.ok = o.k\nvar b = [inner]\nvar s = RandomChoice(b)\ns[0] = 5\nOutput.inner = inner[0]"
    assertJson "2" (Values.prop "ok" o)
    assertJson "5" (Values.prop "inner" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-810")>]
let ``RandomChoice_DoesNotChangeItems`` () =
    let script =
        "var a = [1, {k: 1}, [2], 4]\nvar s = Clone(a)\nvar i = 0\nwhile i < 500 do\n  var r = RandomChoice(a)\n  i = i + 1\nend\nOutput.same = a == s\nOutput.n = Length(a)"
    let o = Script.output (runLoops 600 script)
    assertJson "true" (Values.prop "same" o)
    assertJson "4" (Values.prop "n" o)
    let o2 = outOnInput "{\"a\": [1, 2, 3]}" "var r = RandomChoice(Input.a)\nOutput.ok = r == 1 or r == 2 or r == 3"
    assertJson "true" (Values.prop "ok" o2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-811")>]
let ``RandomChoice_HasEffectNonDeterministic`` () =
    let f = stdlib "RandomChoice"
    Assert.True(f.Effect = FunctionEffect.NonDeterministic, sprintf "RandomChoice declares %A" f.Effect)
    let pure' = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } "Output.r = RandomChoice([1])"
    Assert.False(Script.ok pure')
    Assert.Contains(MessageCode.FunctionEffectExceedsMax, Script.errorCodes pure')
    let nd = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.NonDeterministic } "Output.r = RandomChoice([1])"
    Assert.True(Script.ok nd, Script.describe nd)

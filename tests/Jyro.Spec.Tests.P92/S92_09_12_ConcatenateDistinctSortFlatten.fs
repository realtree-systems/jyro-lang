/// jyro-spec 92.9 Concatenate, 92.10 Distinct, 92.11 Sort, 92.12 Flatten.
module RealTree.Jyro.Spec.Tests.S92_09_12_ConcatenateDistinctSortFlatten

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P92Support

// =============================================================================
// 92.9 Concatenate
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-659")>]
let ``Concatenate_TakesArraysOrNull_ReturnsArray`` () =
    assertSignature "Concatenate" [ "first", false; "second", false ]
    typesAre "array"
        [ "Concatenate([1], [2])"; "Concatenate(null, [2])"; "Concatenate([1], null)"; "Concatenate(null, null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-660")>]
let ``Concatenate_ReturnsFirstElementsThenSecondElements`` () =
    evalsTo ""
        [ "Concatenate([1, 2], [3])", "[1, 2, 3]"; "Concatenate([], [1])", "[1]"; "Concatenate([1], [])", "[1]"
          "Concatenate([[1]], [[2]])", "[[1], [2]]"; "Concatenate([1, 2], [2, 1])", "[1, 2, 2, 1]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-661")>]
let ``Concatenate_NullFirstOrSecond_IsEmptyArray`` () =
    evalsTo "" [ "Concatenate(null, [1])", "[1]"; "Concatenate([1], null)", "[1]"; "Concatenate(null, null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-662")>]
let ``Concatenate_WhenFirstIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Concatenate(%s, [1])" v)
        [ "Concatenate([1], [2])"; "Concatenate(null, [2])"; "Concatenate([1], null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-662")>]
let ``Concatenate_WhenSecondIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Concatenate([1], %s)" v)
        [ "Concatenate([1], [2])"; "Concatenate(null, [2])"; "Concatenate([1], null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-663")>]
let ``Concatenate_ReturnsNewArrayEvenWhenOtherArgumentIsEmptyOrNull`` () =
    for callOf in
        [ (fun a -> sprintf "Concatenate(%s, [])" a); (fun a -> sprintf "Concatenate([], %s)" a)
          (fun a -> sprintf "Concatenate(%s, null)" a); (fun a -> sprintf "Concatenate(null, %s)" a)
          (fun a -> sprintf "Concatenate(%s, [5])" a) ] do
        assertNewArray "[1, 2]" callOf

[<Fact>]
[<Trait("requirement", "JYRO-REQ-664")>]
let ``Concatenate_DoesNotChangeFirstOrSecond`` () =
    let o =
        out "var a = [1, {k: 1}]\nvar b = [[2]]\nvar sa = Clone(a)\nvar sb = Clone(b)\nvar r = Concatenate(a, b)\nOutput.a = a == sa\nOutput.b = b == sb\nOutput.la = Length(a)\nOutput.lb = Length(b)"
    assertJson "true" (Values.prop "a" o)
    assertJson "true" (Values.prop "b" o)
    assertJson "2" (Values.prop "la" o)
    assertJson "1" (Values.prop "lb" o)
    let o2 = outOnInput "{\"a\": [1], \"b\": [2]}" "Output.r = Concatenate(Input.a, Input.b)"
    assertJson "[1, 2]" (Values.prop "r" o2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-665")>]
let ``Concatenate_ResultElementsAreTheElementsOfFirstAndSecondNotCopies`` () =
    let o = out "var o1 = {k: 1}\nvar o2 = {k: 2}\nvar r = Concatenate([o1], [o2])\nr[0].k = 10\nr[1].k = 20\nOutput.a = o1.k\nOutput.b = o2.k"
    assertJson "10" (Values.prop "a" o)
    assertJson "20" (Values.prop "b" o)
    assertElementsShared (fun x -> sprintf "Concatenate([%s], [1])" x) 0
    assertElementsShared (fun x -> sprintf "Concatenate([1], [%s])" x) 1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-666")>]
let ``Concatenate_OnReadOnlyFirstOrSecond_GivesResultWithoutCannotWriteToInput`` () =
    let o =
        outOnInput "{\"a\": [{\"k\": 1}, 2], \"b\": [5]}"
            "Output.r1 = Concatenate(Input.a, [1])\nOutput.r2 = Concatenate([1], Input.a)\nOutput.r3 = Concatenate(Input.a, Input.b)"
    assertJson "[{\"k\": 1}, 2, 1]" (Values.prop "r1" o)
    assertJson "[1, {\"k\": 1}, 2]" (Values.prop "r2" o)
    assertJson "[{\"k\": 1}, 2, 5]" (Values.prop "r3" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-667")>]
let ``Concatenate_OnReadOnlyArguments_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2], \"b\": [5], \"e\": []}"
        (fun a -> sprintf "Concatenate(%s, Input.b)" a) (Some "Concatenate(Input.e, Input.e)")
    assertResultWritable "{\"a\": [{\"k\": 1}, 2]}" (fun a -> sprintf "Concatenate(%s, [])" a) None
    assertResultWritable "{\"a\": [{\"k\": 1}, 2]}" (fun a -> sprintf "Concatenate(null, %s)" a) None

[<Fact>]
[<Trait("requirement", "JYRO-REQ-668")>]
let ``Concatenate_HasEffectPure`` () = assertPure "Concatenate" "Concatenate([1, 2], [3])"

// =============================================================================
// 92.10 Distinct
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-669")>]
let ``Distinct_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "Distinct" [ "items", false ]
    typesAre "array" [ "Distinct([1, 1])"; "Distinct(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-670")>]
let ``Distinct_ReturnsFirstOccurrencesInOrder`` () =
    evalsTo ""
        [ "Distinct([3, 1, 3, 2, 1])", "[3, 1, 2]"; "Distinct([])", "[]"; "Distinct([1, 2, 3])", "[1, 2, 3]"
          "Distinct([\"a\", \"A\", \"a\"])", "[\"a\", \"A\"]"; "Distinct([null, null, 1])", "[null, 1]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-671")>]
let ``Distinct_DecidesEqualityAsEqualsOperator`` () =
    evalsTo "var f = x => x\nvar one = Power(1, 0.5)"
        [ "Length(Distinct([1, \"1\"]))", "2"
          "Length(Distinct([true, 1]))", "2"
          "Length(Distinct([[1, 2], [1, 2]]))", "1"
          "Length(Distinct([{a: 1, b: 2}, {b: 2, a: 1}]))", "1"
          "Length(Distinct([{a: 1}, {a: 1, b: null}]))", "2"
          "Length(Distinct([1, one]))", "1"
          "Length(Distinct([f, f]))", "1"
          "Length(Distinct([x => x, x => x]))", "2"
          "Length(Distinct([0.1 + 0.2, 0.3]))", "1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-672")>]
let ``Distinct_KeepsElementAtLowerPosition`` () =
    let o =
        out "var o1 = {k: 1}\nvar o2 = {k: 1}\nvar r = Distinct([o1, o2])\nr[0].m = 1\nOutput.m1 = o1.m\nOutput.m2 = o2.m\nOutput.n = Length(r)\nvar one = Power(1, 0.5)\nOutput.x = Distinct([one, 1])\nOutput.y = Distinct([1, one])"
    assertJson "1" (Values.prop "m1" o)
    assertJson "null" (Values.prop "m2" o)
    assertJson "1" (Values.prop "n" o)
    let x = Values.asList (Values.prop "x" o)
    let y = Values.asList (Values.prop "y" o)
    Assert.Equal(1, x.Length)
    Assert.Equal(1, y.Length)
    Assert.False(Values.isExact x.Head, "Distinct([inexact 1, exact 1]) kept the exact (later) one")
    Assert.True(Values.isExact y.Head, "Distinct([exact 1, inexact 1]) kept the inexact (later) one")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-673")>]
let ``Distinct_WhenComparisonDeeperThanNestingBound_RaisesValueNestingLimitExceeded`` () =
    let n = maxDepth ()
    let script depth =
        sprintf "%s\n%s\nOutput.n = Length(Distinct([d1, d2]))" (deepArrayScript "d1" depth) (deepArrayScript "d2" depth)
    let atBound = runLoops (2 * n + 10) (script n)
    assertJson "1" (Values.prop "n" (Script.output atBound))
    let above = runLoops (2 * n + 10) (script (n + 1))
    Script.expectError MessageCode.ValueNestingLimitExceeded above

[<Fact>]
[<Trait("requirement", "JYRO-REQ-673")>]
let ``Distinct_OfCyclicElementComparedWithItself_RaisesValueNestingLimitExceeded`` () =
    let r = Script.run "var c = [1]\nc[1] = c\nOutput.n = Length(Distinct([c, c]))"
    Script.expectError MessageCode.ValueNestingLimitExceeded r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-674")>]
let ``Distinct_OfNull_ReturnsEmptyArray`` () = evalsTo "" [ "Distinct(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-675")>]
let ``Distinct_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Distinct(%s)" v) [ "Distinct([1, 2])"; "Distinct(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-676")>]
let ``Distinct_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, 2, [3]]" (fun a -> sprintf "Distinct(%s)" a)
    assertNewArray "[1, 2]" (fun a -> sprintf "Distinct(%s)" a)
    assertNewArray "[7]" (fun a -> sprintf "Distinct(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "Distinct(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-677")>]
let ``Distinct_DoesNotChangeItems`` () =
    assertUnchanged "[{\"k\": 1}, 2, 2, [3]]" (fun a -> sprintf "Distinct(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-678")>]
let ``Distinct_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Distinct([%s])" x) 0
    assertElementsShared (fun x -> sprintf "Distinct([1, 1, %s])" x) 1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-679")>]
let ``Distinct_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, 2, [3]]}" (fun a -> sprintf "Distinct(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-680")>]
let ``Distinct_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, 2, [3]], \"e\": []}" (fun a -> sprintf "Distinct(%s)" a) (Some "Distinct(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-681")>]
let ``Distinct_HasEffectPure`` () = assertPure "Distinct" "Distinct([1, 2])"

// -- 92.10: the pairs Distinct compares (JYRO-REQ-1708, JYRO-ADR-174) -----------
//
// N is the maximum nesting depth. D is N + 1 arrays nested one inside the next
// and E is N - 1 arrays so, around 1: [1, E] is N deep, and == on two values
// that hold D at the same place walks past the bound. A cyclic array c holds
// itself at index 1.

/// The script that declares D, E, a second copy D2 of D, and c.
let private deepDecls () =
    let n = maxDepth ()
    String.concat "\n"
        [ deepArrayScript "D" (n + 1); deepArrayScript "D2" (n + 1); deepArrayScript "E" (n - 1)
          "var c = [1]"; "c[1] = c"; "var c2 = [1]"; "c2[1] = c2"
          "var co = {}"; "co.self = co"; "var co2 = {}"; "co2.self = co2" ]

/// Run `Output.r = <expr>` after the declarations, with loop limits roomy for them.
let private runDeep (expr: string) : JyroResult<JyroValue> =
    runLoops (2 * maxDepth () + 50) (sprintf "%s\nOutput.r = %s" (deepDecls ()) expr)

/// `Length(Distinct(<items>))`, asserting the run succeeds.
let private distinctLength (items: string) : string =
    let r = runDeep (sprintf "Length(Distinct(%s))" items)
    if not (Script.ok r) then Values.fail (sprintf "Distinct(%s) did not succeed:\n%s" items (Script.describe r))
    show (Values.prop "r" (Script.output r))

/// Pairs that are not alike above the bound, where == on the pair walks past the
/// bound: Distinct compares neither pair, so it returns both and raises nothing.
let notAlikePairs : obj[] seq =
    [ "{d: D, b: 1}", "{d: D, b: 2}"         // the example of the clause: b differs
      "{z: 1, a: D}", "{z: 2, a: D}"         // the first property differs
      "[1, D]", "[2, D]"                     // the first element differs
      "[D]", "[D, 1]"                        // lengths differ
      "{a: D}", "{b: D}"                     // keys differ
      "D", "{}"                              // types differ
      "[D]", "[\"x\"]"                       // element types differ
      "[D, 1]", "[D2, \"1\"]"                // the second element's type differs
      "c", "[1, 2]"                          // a cyclic array beside one not alike
      "c", "{}"
      "c", "5"
      "co", "{other: 1}" ]                   // a cyclic object beside one not alike
    |> Seq.collect (fun (x, y) -> [ [| box x; box y |]; [| box y; box x |] ])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1708")>]
[<MemberData(nameof notAlikePairs)>]
let ``JYRO-REQ-1708 Distinct_OfTwoElementsNotAlike_ComparesNeitherAndRaisesNothing`` (x: string, y: string) =
    Assert.Equal("2", distinctLength (sprintf "[%s, %s]" x y))

/// The three cases of JYRO-DEF-733.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1708")>]
let ``JYRO-REQ-1708 Distinct_TheCasesOfTheInspector_ReturnBothWithoutRaising`` () =
    checkAll
        [ // alike (the numbers share a double), compared, == false at the first element
          "[[0.1, D], [0.1000000000000000000000000001, D]]", (fun () ->
              Assert.Equal("2", distinctLength "[[0.1, D], [0.1000000000000000000000000001, D]]"))
          // not alike (z differs): not compared
          "[{z: 1, a: D}, {z: 2, a: D}]", (fun () -> Assert.Equal("2", distinctLength "[{z: 1, a: D}, {z: 2, a: D}]"))
          // not alike (b differs): not compared, whether or not == on them would raise
          "[{d: D, b: 1}, {d: D, b: 2}]", (fun () -> Assert.Equal("2", distinctLength "[{d: D, b: 1}, {d: D, b: 2}]")) ]

/// REQ-1705's chains (100.7.8) through Distinct: alike at depth N + 1 whatever
/// their leaves, and decided by == at depth N.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1708")>]
let ``JYRO-REQ-1708 Distinct_OfTheChainsOf100_7_8_RaisesAtNPlus1AndNotAtN`` () =
    let n = maxDepth ()
    let chains (depth: int) (leafB: int) =
        sprintf "var a = %d\nvar ia = 0\nwhile ia < %d do\n  a = [a]\n  ia = ia + 1\nend\nvar b = %d\nvar ib = 0\nwhile ib < %d do\n  b = [b]\n  ib = ib + 1\nend\nOutput.r = Length(Distinct([a, b]))"
            1 depth leafB depth
    let run depth leafB = runLoops (2 * n + 50) (chains depth leafB)
    checkAll
        [ "depth N, equal leaves", (fun () -> assertJson "1" (Values.prop "r" (Script.output (run n 1))))
          "depth N, leaves differ", (fun () -> assertJson "2" (Values.prop "r" (Script.output (run n 2))))
          "depth N + 1, equal leaves", (fun () -> Script.expectError MessageCode.ValueNestingLimitExceeded (run (n + 1) 1))
          "depth N + 1, leaves differ", (fun () -> Script.expectError MessageCode.ValueNestingLimitExceeded (run (n + 1) 2)) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1708")>]
let ``JYRO-REQ-1708 Distinct_OfCyclicElementsAlike_RaisesValueNestingLimitExceeded`` () =
    checkAll
        [ for items in [ "[c, c]"; "[c, c2]"; "[co, co]"; "[co, co2]"; "[1, c, 2, c]" ] ->
            items, (fun () -> Script.expectError MessageCode.ValueNestingLimitExceeded (runDeep (sprintf "Length(Distinct(%s))" items))) ]

/// Alike pairs: Distinct([x, y]) raises exactly when x == y raises, and
/// otherwise keeps one element when x == y and both when not.
let alikePairs : obj[] seq =
    [ "[1, E]", "[1, E]"                                           // equal within depth N
      "[0.1, E]", "[0.1000000000000000000000000001, E]"            // alike, unequal within depth N
      "{a: 1, e: E}", "{e: E, a: 1}"                               // equal, keys in another order
      "[1, D]", "[1, D2]"                                          // == walks into D: raises
      "D", "D2"
      "{a: 1, b: D}", "{a: 1, b: D2}"
      "[0.1, D]", "[0.1000000000000000000000000001, D2]"           // == stops at the first element
      "{a: 0.1, b: D}", "{a: 0.1000000000000000000000000001, b: D2}"
      "[[1, 2], D]", "[[1, 2], D2]"
      "\"s\"", "\"s\""
      "null", "null"
      "0", "0 * -1" ]
    |> Seq.map (fun (x, y) -> [| box x; box y |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1708")>]
[<MemberData(nameof alikePairs)>]
let ``JYRO-REQ-1708 Distinct_OfTwoAlikeElements_RaisesExactlyWhenEqualsRaises`` (x: string, y: string) =
    let eq = runDeep (sprintf "%s == %s" x y)
    let d = runDeep (sprintf "Length(Distinct([%s, %s]))" x y)
    if Script.ok eq then
        Assert.True(Script.ok d, sprintf "%s == %s does not raise, but Distinct did:\n%s" x y (Script.describe d))
        let equal = show (Values.prop "r" (Script.output eq)) = "true"
        assertJson (if equal then "1" else "2") (Values.prop "r" (Script.output d))
    else
        Assert.Equal(Script.onlyError eq, MessageCode.ValueNestingLimitExceeded)
        Script.expectError MessageCode.ValueNestingLimitExceeded d

/// Only compared alike pairs can raise: the alike pair (0, 1) is compared and
/// raises; in the second array the first element is alike with neither other,
/// and the other two are equal within depth N.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1708")>]
let ``JYRO-REQ-1708 Distinct_ComparesNoOtherPair`` () =
    checkAll
        [ "[[1, D], [1, D2], [2, D]]", (fun () ->
              Script.expectError MessageCode.ValueNestingLimitExceeded (runDeep "Length(Distinct([[1, D], [1, D2], [2, D]]))"))
          "[[2, D], [1, E], [1, E]]", (fun () -> Assert.Equal("2", distinctLength "[[2, D], [1, E], [1, E]]"))
          "[[2, D], [3, D2], [1, E]]", (fun () -> Assert.Equal("3", distinctLength "[[2, D], [3, D2], [1, E]]")) ]

/// x an exact 0.1, y an inexact 0.1 from the host, z an exact
/// 0.1000000000000000000000000001: y == x and y == z (one double), x != z
/// (exact values differ). An element equal to an omitted lower element is
/// omitted (JYRO-REQ-670, JYRO-DEF-732), whether bare or inside a container.
let exactInexactCases : obj[] seq =
    [ "[x, y, z]", "x"            // the example of the clause: z equals the omitted y
      "[x, z, y]", "x,z"          // y equals x
      "[z, y, x]", "z"            // y equals z; x equals the omitted y
      "[y, x, z]", "y"            // x and z each equal y
      "[[x], [y], [z]]", "[x]"
      "[{k: x}, {k: y}, {k: z}]", "{x}"
      "[[x, 1], [y, 1], [z, 1]]", "[x]" ]
    |> Seq.map (fun (items, kept) -> [| box items; box kept |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1708")>]
[<MemberData(nameof exactInexactCases)>]
let ``JYRO-REQ-1708 Distinct_OfExactAndInexactNumbersSharingADouble_OmitsAnElementEqualToAnOmittedOne`` (items: string, kept: string) =
    let input = Values.obj [ "y", Values.inexact 0.1 ]
    let src = sprintf "var x = 0.1\nvar y = Input.y\nvar z = 0.1000000000000000000000000001\nOutput.r = Distinct(%s)" items
    let r = Values.asList (Values.prop "r" (Script.output (Script.runWith { Script.defaults with Input = ValueInput input } src)))
    /// Which of x, y, z a number is.
    let which (v: JyroValue) =
        if not (Values.isExact v) then "y"
        elif Values.asDecimal v = 0.1M then "x"
        elif Values.asDecimal v = 0.1000000000000000000000000001M then "z"
        else sprintf "?%s" (show v)
    let label (v: JyroValue) =
        match v.ValueType with
        | JyroValueType.Number -> which v
        | JyroValueType.Array -> sprintf "[%s]" (which (Values.asList v).Head)
        | JyroValueType.Object -> sprintf "{%s}" (which (Values.prop "k" v))
        | _ -> show v
    Assert.Equal(kept, r |> List.map label |> String.concat ",")

// =============================================================================
// 92.11 Sort
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-682")>]
let ``Sort_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "Sort" [ "items", false ]
    typesAre "array" [ "Sort([2, 1])"; "Sort(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-683")>]
let ``Sort_ReturnsElementsInAscendingOrder`` () =
    evalsTo ""
        [ "Sort([3, 1, 2])", "[1, 2, 3]"; "Sort([1, 2, 3])", "[1, 2, 3]"; "Sort([3, 2, 1])", "[1, 2, 3]"
          "Sort([])", "[]"; "Sort([5])", "[5]"; "Sort([10, 9, -1, 0.5])", "[-1, 0.5, 9, 10]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-684")>]
let ``Sort_NullComesBeforeEveryOtherValue`` () =
    evalsTo ""
        [ "Sort([1, null, \"a\", null, true])", "[null, null, true, 1, \"a\"]"
          "Sort([\"z\", null])", "[null, \"z\"]"
          "Sort([false, null])", "[null, false]"
          "Sort([[1], {}, null])", "[null, [1], {}]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-685")>]
let ``Sort_OrdersDifferentTypesNullBooleanNumberStringArrayObjectFunction`` () =
    let o =
        out "var f = x => x\nvar r = Sort([f, {}, [1], \"a\", 1, true, null])\nOutput.t = [TypeOf(r[0]), TypeOf(r[1]), TypeOf(r[2]), TypeOf(r[3]), TypeOf(r[4]), TypeOf(r[5]), TypeOf(r[6])]"
    assertJson "[\"null\", \"boolean\", \"number\", \"string\", \"array\", \"object\", \"function\"]" (Values.prop "t" o)
    evalsTo ""
        [ "Sort([\"1\", 2])", "[2, \"1\"]"; "Sort([[0], \"z\"])", "[\"z\", [0]]"
          "Sort([{}, [9]])", "[[9], {}]"; "Sort([true, 0])", "[true, 0]"
          "Sort([{a: 1}, \"b\", 3, false, [2], null])", "[null, false, 3, \"b\", [2], {\"a\": 1}]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-686")>]
let ``Sort_FalseComesBeforeTrue`` () =
    evalsTo "" [ "Sort([true, false, true])", "[false, true, true]"; "Sort([false, true])", "[false, true]" ]

/// a = exact 1, b = exact 1.0000000000000000000000000001, c = inexact 1.
let private abc = "var a = 1\nvar b = 1.0000000000000000000000000001\nvar c = Power(1, 0.5)"

/// Which of a, b, c a sorted element is: by representation and exact value.
let private which (v: JyroValue) : string =
    if not (Values.isExact v) then (if Values.asFloat v = 1.0 then "c" else sprintf "inexact %s" (show v))
    elif Values.asDecimal v = 1M then "a"
    elif Values.asDecimal v = 1.0000000000000000000000000001M then "b"
    else sprintf "exact %s" (show v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-687")>]
let ``JYRO-REQ-687 Sort_NumbersOfEqualDoubleValue_ExactByExactValueThenExactBeforeInexact`` () =
    [ "[b, c, a]"; "[a, b, c]"; "[a, c, b]"; "[b, a, c]"; "[c, a, b]"; "[c, b, a]" ]
    |> List.map (fun perm -> perm, (fun () ->
        let r = evalWith abc (sprintf "Sort(%s)" perm)
        let got = Values.asList r |> List.map which
        Assert.Equal<string list>([ "a"; "b"; "c" ], got)))
    |> checkAll
    [ "[c, a]", [ "a"; "c" ]; "[c, b]", [ "b"; "c" ]; "[b, a]", [ "a"; "b" ]; "[a, c]", [ "a"; "c" ] ]
    |> List.map (fun (arr, want) -> arr, (fun () ->
        let got = Values.asList (evalWith abc (sprintf "Sort(%s)" arr)) |> List.map which
        Assert.Equal<string list>(want, got)))
    |> checkAll

[<Fact>]
[<Trait("requirement", "JYRO-REQ-687")>]
let ``JYRO-REQ-687 Sort_NumbersOfDifferentDoubleValue_SmallerFirst`` () =
    let r = Values.asList (eval "Sort([2, Power(2, 0.5)])")
    Assert.False(Values.isExact r.[0])
    Assert.Equal(sqrt 2.0, Values.asFloat r.[0])
    Assert.True(Values.isExact r.[1])
    assertJson "2" r.[1]
    let r2 = Values.asList (evalWith abc "Sort([-1, c, 0])")
    assertJson "-1" r2.[0]
    assertJson "0" r2.[1]
    Assert.False(Values.isExact r2.[2])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-687")>]
let ``JYRO-REQ-687 Sort_TwoInexactNumbersOfEqualDoubleValue_AreTiedAndKeepInputOrder`` () =
    // Two inexact numbers of equal double value are indistinguishable as bare
    // elements, so their tie is observed through the sort order that SortBy
    // shares with Sort (90.6.3): the keys are tied, so the ids keep input order.
    let o =
        out "var c = Power(1, 0.5)\nvar d = Power(1, 0.25)\nvar r = SortBy([{k: c, id: 1}, {k: d, id: 2}], x => x.k)\nvar s = SortBy([{k: d, id: 2}, {k: c, id: 1}], x => x.k)\nOutput.r = [r[0].id, r[1].id]\nOutput.s = [s[0].id, s[1].id]"
    assertJson "[1, 2]" (Values.prop "r" o)
    assertJson "[2, 1]" (Values.prop "s" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-688")>]
let ``Sort_StringsByOrdinalUtf16CodeUnits`` () =
    evalsTo ""
        [ "Sort([\"b\", \"a\", \"B\", \"A\"])", "[\"A\", \"B\", \"a\", \"b\"]"
          "Sort([\"aa\", \"a\", \"\"])", "[\"\", \"a\", \"aa\"]"
          "Sort([\"\\u00E9\", \"f\"])", "[\"f\", \"\\u00e9\"]" ]
    let o = outOnInput "{\"wide\": \"\\uff5e\", \"astral\": \"\\ud83d\\ude00\"}" "var r = Sort([Input.wide, Input.astral])\nOutput.first = r[0] == Input.astral"
    assertJson "true" (Values.prop "first" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-689")>]
let ``Sort_TwoArraysTwoObjectsTwoFunctions_AreTied`` () =
    evalsTo ""
        [ "Sort([[2], [1]])", "[[2], [1]]"; "Sort([[1], [2]])", "[[1], [2]]"
          "Sort([{b: 1}, {a: 0}])", "[{\"b\": 1}, {\"a\": 0}]"; "Sort([{a: 0}, {b: 1}])", "[{\"a\": 0}, {\"b\": 1}]" ]
    let o =
        out "var a1 = [2]\nvar a2 = [1]\nvar ra = Sort([a1, a2])\nra[0][0] = 7\nOutput.a1 = a1[0]\nvar f1 = x => 1\nvar f2 = x => 0\nvar rf = Sort([f1, f2])\nOutput.f = rf[0] == f1 and rf[1] == f2\nvar rg = Sort([f2, f1])\nOutput.g = rg[0] == f2 and rg[1] == f1"
    assertJson "7" (Values.prop "a1" o)
    assertJson "true" (Values.prop "f" o)
    assertJson "true" (Values.prop "g" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-690")>]
let ``Sort_IsStable_TiedElementsKeepRelativeOrder`` () =
    // 64 objects {id: i}, 64 arrays [i] and 64 numbers interleaved: the objects
    // and the arrays are tied within their type, so each keeps input order.
    let script =
        "var items = []\nvar i = 0\nwhile i < 64 do\n  items[Length(items)] = {id: i}\n  items[Length(items)] = [i]\n  items[Length(items)] = 63 - i\n  i = i + 1\nend\nvar r = Sort(items)\nOutput.r = r"
    let o = Script.output (runLoops 100 script)
    let r = Values.asList (Values.prop "r" o)
    Assert.Equal(192, r.Length)
    // Compared by value (assertJson), not by the text of the numbers (4.1.15).
    assertJson (sprintf "[%s]" (System.String.Join(", ", [ for i in 0 .. 63 -> string i ]))) (Values.arr (List.take 64 r))
    assertJson (sprintf "[%s]" (System.String.Join(", ", [ for i in 0 .. 63 -> sprintf "[%d]" i ]))) (Values.arr (r |> List.skip 64 |> List.take 64))
    assertJson (sprintf "[%s]" (System.String.Join(", ", [ for i in 0 .. 63 -> string i ]))) (Values.arr (r |> List.skip 128 |> List.map (Values.prop "id")))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-691")>]
let ``Sort_OfNull_ReturnsEmptyArray`` () = evalsTo "" [ "Sort(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-692")>]
let ``Sort_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Sort(%s)" v) [ "Sort([1, 2])"; "Sort(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-693")>]
let ``Sort_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[3, {\"k\": 1}, 1, [3], 2]" (fun a -> sprintf "Sort(%s)" a)
    assertNewArray "[1, 2]" (fun a -> sprintf "Sort(%s)" a)
    assertNewArray "[7]" (fun a -> sprintf "Sort(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "Sort(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-694")>]
let ``Sort_DoesNotChangeOrderOfItems`` () =
    assertUnchanged "[3, {\"k\": 1}, 1, [3], 2]" (fun a -> sprintf "Sort(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-695")>]
let ``Sort_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Sort([%s])" x) 0
    assertElementsShared (fun x -> sprintf "Sort([%s, 2, 1])" x) 2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-696")>]
let ``Sort_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [3, {\"k\": 1}, 1, [3], 2]}" (fun a -> sprintf "Sort(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-697")>]
let ``Sort_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [3, {\"k\": 1}, 1, [3], 2], \"e\": []}" (fun a -> sprintf "Sort(%s)" a) (Some "Sort(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-698")>]
let ``Sort_HasEffectPure`` () = assertPure "Sort" "Sort([1, 2])"

// =============================================================================
// 92.12 Flatten
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-699")>]
let ``Flatten_TakesArrayOrNull_ReturnsArray`` () =
    assertSignature "Flatten" [ "items", false ]
    typesAre "array" [ "Flatten([[1]])"; "Flatten(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-700")>]
let ``Flatten_ReturnsNonArrayValuesAtEveryDepthDepthFirst`` () =
    evalsTo ""
        [ "Flatten([1, [2, [3, [4]]], 5])", "[1, 2, 3, 4, 5]"; "Flatten([[1, 2], [3]])", "[1, 2, 3]"
          "Flatten([1, 2])", "[1, 2]"; "Flatten([[[\"a\"]], \"b\"])", "[\"a\", \"b\"]"
          "Flatten([[1, [2]], [[3], 4]])", "[1, 2, 3, 4]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-701")>]
let ``Flatten_KeepsEveryNullAtEveryDepth`` () =
    evalsTo "" [ "Flatten([null, [null, [null]]])", "[null, null, null]"; "Flatten([[null]])", "[null]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1811")>]
let ``JYRO-REQ-1811 Flatten_KeepsEachObjectWholeAndDoesNotLookIntoIt`` () =
    evalsTo "" [ "Flatten([{a: [1, 2]}, [{b: [3]}]])", "[{\"a\": [1, 2]}, {\"b\": [3]}]" ]
    let o = out "var o = {}\no.self = o\nvar r = Flatten([o])\nOutput.n = Length(r)\nr[0].mark = 1\nOutput.m = o.mark"
    assertJson "1" (Values.prop "n" o)
    assertJson "1" (Values.prop "m" o)

/// An object at any depth of nested arrays is one element of the result, and an
/// array inside an object stays inside it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1811")>]
let ``JYRO-REQ-1811 Flatten_ObjectAtAnyDepth_IsOneElementAndItsArraysStayInside`` () =
    evalsTo ""
        [ "Flatten([[[{a: [1, [2]]}]]])", "[{\"a\": [1, [2]]}]"
          "Flatten([{a: [1, 2]}, [{b: [3]}, [{c: [[4]]}]]])", "[{\"a\": [1, 2]}, {\"b\": [3]}, {\"c\": [[4]]}]"
          "Flatten([1, [{}], [[{}]]])", "[1, {}, {}]"
          "Flatten([{a: []}])", "[{\"a\": []}]" ]
    allTrue "" [ "Length(Flatten([{a: [1, 2, 3]}])) == 1"; "TypeOf(Flatten([[{a: [1]}]])[0]) == \"object\"" ]

/// What an object holds: a cycle, and a nesting deeper than the bound, do not
/// make Flatten raise, and the element is the object itself.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1811")>]
let ``JYRO-REQ-1811 Flatten_OfObjectHoldingACycleOrAnArrayDeeperThanTheBound_DoesNotRaise`` () =
    let n = maxDepth ()
    // a cycle through an array held by the object
    let o1 = out "var o = {a: []}\no.a[0] = o\nvar r = Flatten([[o]])\nOutput.n = Length(r)\nr[0].mark = 1\nOutput.m = o.mark"
    assertJson "1" (Values.prop "n" o1)
    assertJson "1" (Values.prop "m" o1)
    // an array held by the object nested deeper than the bound
    let deep = sprintf "%s\nvar o = {a: d}\nvar r = Flatten([[o]])\nOutput.n = Length(r)\nr[0].mark = 1\nOutput.m = o.mark" (deepArrayScript "d" (n + 5))
    let o2 = Script.output (runLoops (n + 20) deep)
    assertJson "1" (Values.prop "n" o2)
    assertJson "1" (Values.prop "m" o2)
    // an object nested deeper than the bound inside the object
    let nest =
        sprintf "var o = {}\nvar p = o\nvar i = 0\nwhile i < %d do\n  p.k = {}\n  p = p.k\n  i = i + 1\nend\nvar r = Flatten([o])\nOutput.n = Length(r)\nr[0].mark = 1\nOutput.m = o.mark" (n + 5)
    let o3 = Script.output (runLoops (n + 20) nest)
    assertJson "1" (Values.prop "n" o3)
    assertJson "1" (Values.prop "m" o3)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-703")>]
let ``Flatten_EmptyNestedArraysAddNothing`` () =
    evalsTo "" [ "Flatten([1, [], [[]], 2])", "[1, 2]"; "Flatten([[], [[], []]])", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-704")>]
let ``Flatten_WhenArrayNestingDeeperThanBound_RaisesValueNestingLimitExceeded`` () =
    let n = maxDepth ()
    let script depth = sprintf "%s\nOutput.r = Flatten(d)" (deepArrayScript "d" depth)
    assertJson "[1]" (Values.prop "r" (Script.output (runLoops (n + 10) (script n))))
    Script.expectError MessageCode.ValueNestingLimitExceeded (runLoops (n + 10) (script (n + 1)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-704")>]
let ``Flatten_WhenExtraDepthIsInsideAnObject_Succeeds`` () =
    let n = maxDepth ()
    // An array n deep whose innermost element is an object holding an array:
    // the object is not walked, so the array nesting stays within the bound.
    let script =
        sprintf "var d = [{x: [1]}]\nvar i = 1\nwhile i < %d do\n  d = [d]\n  i = i + 1\nend\nOutput.n = Length(Flatten(d))" n
    assertJson "1" (Values.prop "n" (Script.output (runLoops (n + 10) script)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-705")>]
let ``Flatten_OfCyclicArray_RaisesValueNestingLimitExceeded`` () =
    checkAll
        [ "Flatten(c)", (fun () -> Script.expectError MessageCode.ValueNestingLimitExceeded (Script.run "var c = [1]\nc[1] = c\nOutput.r = Length(Flatten(c))"))
          "Flatten([0, [c]])", (fun () -> Script.expectError MessageCode.ValueNestingLimitExceeded (Script.run "var c = [1]\nc[1] = c\nOutput.r = Length(Flatten([0, [c]]))")) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-706")>]
let ``Flatten_OfNull_ReturnsEmptyArray`` () = evalsTo "" [ "Flatten(null)", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-707")>]
let ``Flatten_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Flatten(%s)" v) [ "Flatten([1, 2])"; "Flatten(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-708")>]
let ``Flatten_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, [2, [3]], 4]" (fun a -> sprintf "Flatten(%s)" a)
    assertNewArray "[1, 2]" (fun a -> sprintf "Flatten(%s)" a)
    assertNewArray "[7]" (fun a -> sprintf "Flatten(%s)" a)
    assertNewArray "[]" (fun a -> sprintf "Flatten(%s)" a)
    // Writes to the result do not reach a nested array of items either.
    let o = out "var inner = [2, 3]\nvar a = [1, inner]\nvar r = Flatten(a)\nr[1] = 99\nOutput.inner = inner"
    assertJson "[2, 3]" (Values.prop "inner" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-709")>]
let ``Flatten_DoesNotChangeItems`` () =
    assertUnchanged "[{\"k\": 1}, [2, [3]], 4]" (fun a -> sprintf "Flatten(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-710")>]
let ``Flatten_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    let o = out "var o = {k: 1}\nvar f = x => x\nvar r = Flatten([[o], f])\nr[0].k = 2\nOutput.k = o.k\nOutput.f = r[1] == f"
    assertJson "2" (Values.prop "k" o)
    assertJson "true" (Values.prop "f" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-711")>]
let ``Flatten_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, [2, [3]], 4]}" (fun a -> sprintf "Flatten(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-712")>]
let ``Flatten_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, [2, [3]], 4], \"e\": [[]]}" (fun a -> sprintf "Flatten(%s)" a) (Some "Flatten(Input.e)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-713")>]
let ``Flatten_HasEffectPure`` () = assertPure "Flatten" "Flatten([1, [2]])"

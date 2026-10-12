/// jyro-spec 92.1 Length, 92.2 Append, 92.3 Prepend, 92.4 First, 92.5 Last.
module RealTree.Jyro.Spec.Tests.S92_01_05_LengthAppendPrependFirstLast

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P92Support

// =============================================================================
// 92.1 Length
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-577")>]
let ``Length_AcceptsStringArrayObjectAndNull_ReturnsNumber`` () =
    assertSignature "Length" [ "source", false ]
    typesAre "number" [ "Length(\"abc\")"; "Length([1, 2])"; "Length({a: 1, b: 2})"; "Length(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-578")>]
let ``Length_OfString_CountsUtf16CodeUnits`` () =
    evalsTo "" [ "Length(\"\")", "0"; "Length(\"abc\")", "3" ]
    let o =
        outOnInput "{\"astral\": \"\\ud83d\\ude00\", \"pre\": \"\\u00e9\", \"dec\": \"e\\u0301\"}"
            "Output.a = Length(Input.astral)\nOutput.p = Length(Input.pre)\nOutput.d = Length(Input.dec)"
    assertJson "2" (Values.prop "a" o)
    assertJson "1" (Values.prop "p" o)
    assertJson "2" (Values.prop "d" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-579")>]
let ``Length_OfArray_CountsElementsNotNestedOnes`` () =
    evalsTo "" [ "Length([])", "0"; "Length([null])", "1"; "Length([1, [2, 3]])", "2"; "Length([[], []])", "2" ]
    let big = Values.arr [ for i in 1 .. 1000 -> Values.int i ]
    let r = Script.runInput (Values.obj [ "a", big ]) "Output.r = Length(Input.a)"
    assertJson "1000" (Values.prop "r" (Script.output r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-580")>]
let ``Length_OfObject_CountsPropertiesIncludingNullValued`` () =
    evalsTo "" [ "Length({})", "0"; "Length({a: 1, b: null})", "2"; "Length({a: {b: 1, c: 2}})", "1" ]
    let o = out "var o = {a: 1}\no.b = 2\nOutput.x = Length(o)\no.a = 5\nOutput.y = Length(o)"
    assertJson "2" (Values.prop "x" o)
    assertJson "2" (Values.prop "y" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-581")>]
let ``Length_OfNull_ReturnsZero`` () =
    evalsTo "" [ "Length(null)", "0"; "Length(Input.missing)", "0" ]

/// The one error of `r` has the MessageCode member named `codeName` (reached by name, so that
/// this project compiles on a build without it) and exactly the arguments `args`, read as strings.
let private failsNamedWithArgs (codeName: string) (args: string list) (r: JyroResult<JyroValue>) : unit =
    let m = Script.onlyErrorMessage r
    let name = System.Enum.GetName(typeof<MessageCode>, m.Code)
    if name <> codeName then
        Values.fail (sprintf "Expected the one error %s, got %A (%d). Diagnostics:\n%s" codeName m.Code (int m.Code) (Script.describe r))
    let got =
        if isNull m.Args then Values.fail (sprintf "The %s diagnostic's Args is null" codeName)
        else
            m.Args
            |> Array.map (fun a ->
                match a with
                | :? string as s -> s
                | :? JyroString as s -> s.Value
                | o -> sprintf "<not a string: %A>" o)
            |> List.ofArray
    if got <> args then
        Values.fail (sprintf "Expected the arguments %A, got %A. Diagnostics:\n%s" args got (Script.describe r))

/// 92.1.6: a boolean, number (exact or inexact) or function source raises StringArrayOrObjectRequired,
/// never ArgumentTypeMismatch, with exactly the arguments Length() and the TypeOf name of source.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1701")>]
let ``JYRO-REQ-1701 Length_OfBooleanNumberOrFunction_RaisesStringArrayOrObjectRequiredWithLengthAndActualType`` () =
    let code = "StringArrayOrObjectRequired"
    [ "true", "boolean"; "false", "boolean"
      "5", "number"; "0", "number"; "1", "number"; "1.5", "number"; "-2", "number"; "Power(2, 0.5)", "number"
      "x => x", "function" ]
    |> List.map (fun (arg, t) ->
        sprintf "Length(%s)" arg, (fun () ->
            failsNamedWithArgs code [ "Length()"; t ] (Script.run (sprintf "Output.r = Length(%s)" arg))))
    |> checkAll
    // the same through variables, and through Input
    [ "f", "function"; "n", "number"; "b", "boolean" ]
    |> List.map (fun (v, t) ->
        sprintf "Length(%s)" v, (fun () ->
            failsNamedWithArgs code [ "Length()"; t ]
                (Script.run (sprintf "var f = x => x\nvar n = 5\nvar b = true\nOutput.r = Length(%s)" v))))
    |> checkAll
    [ "Input.n", "number"; "Input.b", "boolean" ]
    |> List.map (fun (e, t) ->
        sprintf "Length(%s)" e, (fun () ->
            failsNamedWithArgs code [ "Length()"; t ]
                (runOnInput "{\"n\": 3, \"b\": false}" (sprintf "Output.r = Length(%s)" e))))
    |> checkAll

[<Fact>]
[<Trait("requirement", "JYRO-REQ-583")>]
let ``JYRO-REQ-583 Length_ReturnsExactNumber`` () =
    allExact "" [ "Length(\"abc\")"; "Length(\"\")"; "Length([1, 2, 3])"; "Length([])"; "Length({a: 1})"; "Length({})"; "Length(null)" ]
    let o = outOnInput "{\"a\": [1, 2], \"s\": \"xyz\"}" "Output.a = Length(Input.a)\nOutput.s = Length(Input.s)"
    Assert.True(Values.isExact (Values.prop "a" o), "Length of an Input array is not exact")
    Assert.True(Values.isExact (Values.prop "s" o), "Length of an Input string is not exact")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-584")>]
let ``Length_DoesNotChangeSource`` () =
    let o =
        out "var a = [1, [2], {k: 1}]\nvar o = {x: 1, y: [2]}\nvar sa = Clone(a)\nvar so = Clone(o)\nvar r1 = Length(a)\nvar r2 = Length(o)\nvar r3 = Length(\"abc\")\nOutput.a = a == sa\nOutput.o = o == so"
    assertJson "true" (Values.prop "a" o)
    assertJson "true" (Values.prop "o" o)
    let o2 = outOnInput "{\"a\": [1, 2], \"o\": {\"k\": 1}}" "Output.x = Length(Input.a)\nOutput.y = Length(Input.o)"
    assertJson "2" (Values.prop "x" o2)
    assertJson "1" (Values.prop "y" o2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-585")>]
let ``Length_HasEffectPure`` () = assertPure "Length" "Length([1, 2])"

// =============================================================================
// 92.2 Append
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-586")>]
let ``Append_TakesArrayOrNullAndAnyValue_ReturnsArray`` () =
    assertSignature "Append" [ "items", false; "value", false ]
    typesAre "array"
        [ "Append([1], 2)"; "Append(null, 1)"; "Append([], null)"; "Append([], true)"; "Append([], 1)"
          "Append([], \"s\")"; "Append([], [1])"; "Append([], {a: 1})"; "Append([], x => x)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-587")>]
let ``Append_ReturnsItemsThenValueAsLastElement`` () =
    evalsTo ""
        [ "Append([1, 2, 3], 4)", "[1, 2, 3, 4]"
          "Append([], \"x\")", "[\"x\"]"
          "Append([1], [2, 3])", "[1, [2, 3]]"
          "Append([1], null)", "[1, null]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1740")>]
let ``JYRO-REQ-1740 Append_WhenItemsIsNull_GivesOneElementArray`` () =
    evalsTo ""
        [ "Append(null, 1)", "[1]"; "Append(null, null)", "[null]"
          "Append(null, 5)", "[5]"; "Append(null, [1, 2])", "[[1, 2]]"
          "Append(null, [1])", "[[1]]"; "Append(null, {a: 1})", "[{\"a\": 1}]"
          "Append(null, \"s\")", "[\"s\"]"; "Append(null, true)", "[true]"; "Append(null, [])", "[[]]" ]

/// Each result has `Length` 1 and its one element is `value`, for a value of every
/// type, and no error is raised.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1740")>]
let ``JYRO-REQ-1740 Append_WhenItemsIsNull_HasLengthOneForEveryTypeOfValue`` () =
    allTrue ""
        [ for v in [ "5"; "null"; "[1, 2]"; "{a: 1}"; "\"s\""; "true"; "[]"; "{}"; "x => x"; "1.5" ] ->
              sprintf "Length(Append(null, %s)) == 1 and TypeOf(Append(null, %s)) == \"array\"" v v ]
    allTrue "" [ "Append(null, [1, 2])[0] == [1, 2]"; "Append(null, 5)[0] == 5"; "Append(null, null)[0] == null" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-589")>]
let ``Append_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Append(%s, 9)" v) [ "Append([1, 2], 9)"; "Append(null, 9)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-590")>]
let ``Append_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Append(%s, 9)" a)
    assertNewArray "[]" (fun a -> sprintf "Append(%s, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-591")>]
let ``Append_DoesNotChangeItems`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Append(%s, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-592")>]
let ``Append_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Append([%s], 9)" x) 0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-593")>]
let ``Append_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Append(%s, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-594")>]
let ``Append_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Append(%s, 9)" a) None

[<Fact>]
[<Trait("requirement", "JYRO-REQ-595")>]
let ``Append_HasEffectPure`` () = assertPure "Append" "Append([1, 2], 9)"

// =============================================================================
// 92.3 Prepend
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-596")>]
let ``Prepend_TakesArrayOrNullAndAnyValue_ReturnsArray`` () =
    assertSignature "Prepend" [ "items", false; "value", false ]
    typesAre "array"
        [ "Prepend([1], 2)"; "Prepend(null, 1)"; "Prepend([], null)"; "Prepend([], true)"; "Prepend([], 1)"
          "Prepend([], \"s\")"; "Prepend([], [1])"; "Prepend([], {a: 1})"; "Prepend([], x => x)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-597")>]
let ``Prepend_ReturnsValueThenItems`` () =
    evalsTo ""
        [ "Prepend([1, 2, 3], 0)", "[0, 1, 2, 3]"
          "Prepend([], \"x\")", "[\"x\"]"
          "Prepend([1], [2, 3])", "[[2, 3], 1]"
          "Prepend([1], null)", "[null, 1]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1741")>]
let ``JYRO-REQ-1741 Prepend_WhenItemsIsNull_GivesOneElementArray`` () =
    evalsTo ""
        [ "Prepend(null, 1)", "[1]"; "Prepend(null, null)", "[null]"
          "Prepend(null, 5)", "[5]"; "Prepend(null, [1, 2])", "[[1, 2]]"
          "Prepend(null, [1])", "[[1]]"; "Prepend(null, {a: 1})", "[{\"a\": 1}]"
          "Prepend(null, \"s\")", "[\"s\"]"; "Prepend(null, true)", "[true]"; "Prepend(null, [])", "[[]]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1741")>]
let ``JYRO-REQ-1741 Prepend_WhenItemsIsNull_HasLengthOneForEveryTypeOfValue`` () =
    allTrue ""
        [ for v in [ "5"; "null"; "[1, 2]"; "{a: 1}"; "\"s\""; "true"; "[]"; "{}"; "x => x"; "1.5" ] ->
              sprintf "Length(Prepend(null, %s)) == 1 and TypeOf(Prepend(null, %s)) == \"array\"" v v ]
    allTrue "" [ "Prepend(null, [1, 2])[0] == [1, 2]"; "Prepend(null, 5)[0] == 5"; "Prepend(null, null)[0] == null" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-599")>]
let ``Prepend_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Prepend(%s, 9)" v) [ "Prepend([1, 2], 9)"; "Prepend(null, 9)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-600")>]
let ``Prepend_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Prepend(%s, 9)" a)
    assertNewArray "[]" (fun a -> sprintf "Prepend(%s, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-601")>]
let ``Prepend_DoesNotChangeItems`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "Prepend(%s, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-602")>]
let ``Prepend_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    assertElementsShared (fun x -> sprintf "Prepend([%s], 9)" x) 1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-603")>]
let ``Prepend_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Prepend(%s, 9)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-604")>]
let ``Prepend_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"k\": 1}, 2, [3]]}" (fun a -> sprintf "Prepend(%s, 9)" a) None

[<Fact>]
[<Trait("requirement", "JYRO-REQ-605")>]
let ``Prepend_HasEffectPure`` () = assertPure "Prepend" "Prepend([1, 2], 9)"

// =============================================================================
// 92.4 First and 92.5 Last
// =============================================================================

let private anyTypeElements =
    [ "[1]", "number"; "[{a: 1}]", "object"; "[[1]]", "array"; "[\"s\"]", "string"
      "[true]", "boolean"; "[x => x]", "function" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-606")>]
let ``First_TakesArrayOrNull_ReturnsValueOfAnyType`` () =
    assertSignature "First" [ "items", false ]
    anyTypeElements
    |> List.map (fun (arr, t) -> arr, (fun () -> assertJson (sprintf "\"%s\"" t) (eval (sprintf "TypeOf(First(%s))" arr))))
    |> checkAll
    assertJson "\"null\"" (eval "TypeOf(First(null))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-607")>]
let ``First_ReturnsElementAtPositionZero`` () =
    evalsTo "" [ "First([7, 8, 9])", "7"; "First([null, 1])", "null"; "First([[1], 2])", "[1]"; "First([5])", "5" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-608")>]
let ``First_OfEmptyOrNull_ReturnsNullWithoutError`` () =
    evalsTo "" [ "First([])", "null"; "First(null)", "null" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-609")>]
let ``First_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "First(%s)" v) [ "First([1, 2])"; "First(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-610")>]
let ``First_ReturnsTheElementItself_WritesAreVisibleThroughItems`` () =
    let o =
        out "var o = {k: 1}\nvar inner = [1]\nvar a = [o, 1]\nvar r = First(a)\nr.k = 2\nOutput.ok = o.k\nvar b = [inner, 1]\nvar s = First(b)\ns[0] = 5\nOutput.inner = inner[0]\nOutput.viaItems = b[0][0]"
    assertJson "2" (Values.prop "ok" o)
    assertJson "5" (Values.prop "inner" o)
    assertJson "5" (Values.prop "viaItems" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-611")>]
let ``First_DoesNotChangeItems`` () =
    assertUnchanged "[{\"k\": 1}, 2, [3]]" (fun a -> sprintf "First(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-612")>]
let ``First_HasEffectPure`` () = assertPure "First" "First([1, 2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-613")>]
let ``Last_TakesArrayOrNull_ReturnsValueOfAnyType`` () =
    assertSignature "Last" [ "items", false ]
    anyTypeElements
    |> List.map (fun (arr, t) -> arr, (fun () -> assertJson (sprintf "\"%s\"" t) (eval (sprintf "TypeOf(Last(%s))" arr))))
    |> checkAll
    assertJson "\"null\"" (eval "TypeOf(Last(null))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-614")>]
let ``Last_ReturnsElementAtLengthMinusOne`` () =
    evalsTo "" [ "Last([7, 8, 9])", "9"; "Last([1, null])", "null"; "Last([1, [2]])", "[2]"; "Last([5])", "5" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-615")>]
let ``Last_OfEmptyOrNull_ReturnsNullWithoutError`` () =
    evalsTo "" [ "Last([])", "null"; "Last(null)", "null" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-616")>]
let ``Last_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "Last(%s)" v) [ "Last([1, 2])"; "Last(null)" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-617")>]
let ``Last_ReturnsTheElementItself_WritesAreVisibleThroughItems`` () =
    let o =
        out "var o = {k: 1}\nvar inner = [1]\nvar a = [1, o]\nvar r = Last(a)\nr.k = 2\nOutput.ok = o.k\nvar b = [1, inner]\nvar s = Last(b)\ns[0] = 5\nOutput.inner = inner[0]\nOutput.viaItems = b[1][0]"
    assertJson "2" (Values.prop "ok" o)
    assertJson "5" (Values.prop "inner" o)
    assertJson "5" (Values.prop "viaItems" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-618")>]
let ``Last_DoesNotChangeItems`` () =
    assertUnchanged "[2, [3], {\"k\": 1}]" (fun a -> sprintf "Last(%s)" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-619")>]
let ``Last_HasEffectPure`` () = assertPure "Last" "Last([1, 2])"

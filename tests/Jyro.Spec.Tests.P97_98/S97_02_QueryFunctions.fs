/// 97.1 to 97.8: what each query function returns, its order of examination,
/// and its effect.
module RealTree.Jyro.Spec.Tests.S97_02_QueryFunctions

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private fieldCall (fn: string) (items: string) (op: string) (value: string) =
    sprintf "%s(%s, \"a\", \"%s\", %s)" fn items op value

/// Every type of value: null, boolean, number, string, array, object, function.
let private allValues = [ "null"; "true"; "1"; "1.5"; "\"1\""; "[1]"; "{a: 1}"; "(x => x)" ]

/// The call returns a value whose TypeOf is one of `types`, for every operator
/// and every type of `value`.
let private assertReturnsType (fn: string) (types: string list) =
    let mutable cases = []
    for op in [ "=="; "!=" ] do
        for v in allValues do
            cases <- ("[{a: 1}, {a: 2}]", op, v) :: cases
    for op in relOps do
        for v in allValues do
            cases <- ("[{b: 1}]", op, v) :: cases // absent property: null, so no comparison error
        for v in [ "1"; "1.5"; "2" ] do
            cases <- ("[{a: 1}, {a: 2}]", op, v) :: cases
    for (items, op, v) in cases do
        let call = fieldCall fn items op v
        let t = expr (sprintf "TypeOf(%s)" call) |> Values.asString
        check (List.contains t types) (sprintf "%s gave a %s, expected one of %A" call t types)

/// `items` and its elements are unchanged: for a script array and for Input.
let assertItemsUnchanged (callOf: string -> string) =
    let source =
        sprintf
            "var xs = [{a: 1, n: {m: [1, 2]}}, {a: 2}, 3, null]\nvar before = ToJson(xs)\nvar len = Length(xs)\nvar res = %s\nOutput.r = [ToJson(xs) == before, Length(xs) == len]"
            (callOf "xs")
    assertJson "[true, true]" (evalR source) source
    let inputSource =
        sprintf "var before = ToJson(Input.items)\nvar res = %s\nOutput.r = ToJson(Input.items) == before" (callOf "Input.items")
    let options =
        { Script.defaults with Input = JsonInput "{\"items\":[{\"a\":1,\"n\":{\"m\":[1,2]}},{\"a\":2},3,null]}" }
    check (runR options inputSource |> Values.asBool) (sprintf "Input.items changed:\n%s" inputSource)

let private assertFieldFnPure (fn: string) =
    assertPureEffect fn (sprintf "Output.r = %s([{a: 1}], \"a\", \"==\", 1)" fn)
    assertItemsUnchanged (fun items -> sprintf "%s(%s, \"a\", \">=\", 1)" fn items)
    assertItemsUnchanged (fun items -> sprintf "%s(%s, \"a\", \"==\", null)" fn items)

// == 97.1 AnyByField ======================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1267")>]
let ``AnyByField_ReturnsBoolean_ForEveryOperatorAndEveryTypeOfValue`` () =
    assertReturnsType "AnyByField" [ "boolean" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1267")>]
let ``AnyByField_Signature_IsItemsFieldNameOperatorValue_NoneOptional`` () =
    assertSignature "AnyByField" [ "items"; "fieldName"; "operator"; "value" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1283")>]
let ``AnyByField_WhenSomeElementPasses_IsTrue_Otherwise_False`` () =
    assertExpr "true" "AnyByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"==\", 3)"
    assertExpr "true" "AnyByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"==\", 1)"
    assertExpr "true" "AnyByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"==\", 2)"
    assertExpr "false" "AnyByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"==\", 9)"
    assertExpr "true" "AnyByField([null, 5, {a: 3}], \"a\", \"==\", 3)"
    assertExpr "false" "AnyByField([null, 5, \"a\"], \"a\", \"==\", null)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1284")>]
let ``AnyByField_WhenItemsEmpty_IsFalse_ForEveryOperator`` () =
    for op in ops do
        assertExpr "false" (fieldCall "AnyByField" "[]" op "1")
    assertExpr "false" (fieldCall "AnyByField" "[]" "==" "null")
    assertExpr "false" (fieldCall "AnyByField" "[]" "!=" "null")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1285")>]
let ``AnyByField_ExaminesInOrder_AndStopsAtTheFirstPassingElement`` () =
    // The second element would raise IncomparableTypes if examined.
    assertExpr "true" "AnyByField([{a: 1}, {a: \"x\"}], \"a\", \"<\", 5)"
    assertExpr "true" "AnyByField([{a: 9}, {a: 1}, {a: \"x\"}], \"a\", \"<\", 5)"
    // The first element is examined first.
    expectErr MessageCode.IncomparableTypes "Output.r = AnyByField([{a: \"x\"}, {a: 1}], \"a\", \"<\", 5)"
    // A failing element does not stop the examination.
    expectErr MessageCode.IncomparableTypes "Output.r = AnyByField([{a: 9}, {a: \"x\"}], \"a\", \"<\", 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1286")>]
let ``AnyByField_IsPure_AndDoesNotChangeItems`` () = assertFieldFnPure "AnyByField"

// == 97.2 AllByField ======================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1288")>]
let ``AllByField_ReturnsBoolean_AndTakesTheArgumentsOfAnyByField`` () =
    assertReturnsType "AllByField" [ "boolean" ]
    assertSignature "AllByField" [ "items"; "fieldName"; "operator"; "value" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1290")>]
let ``AllByField_IsTrueWhenEveryElementPasses_FalseWhenAnyFails`` () =
    let xs = "[{a: 1}, {a: 2}, {a: 3}]"
    assertExpr "true" (fieldCall "AllByField" xs ">" "0")
    assertExpr "false" (fieldCall "AllByField" xs ">" "1") // first fails
    assertExpr "false" (fieldCall "AllByField" xs "<" "3") // last fails
    assertExpr "false" (fieldCall "AllByField" xs "!=" "2") // middle fails
    assertExpr "true" (fieldCall "AllByField" xs "!=" "4")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1291")>]
let ``AllByField_WhenItemsEmpty_IsTrue_ForEveryOperator`` () =
    for op in ops do
        assertExpr "true" (fieldCall "AllByField" "[]" op "1")
    assertExpr "true" (fieldCall "AllByField" "[]" "==" "\"no element could pass\"")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1292")>]
let ``AllByField_WhenAnElementIsNotAnObject_IsFalse`` () =
    assertExpr "false" "AllByField([{a: null}, null], \"a\", \"==\", null)"
    assertExpr "false" "AllByField([{a: 1}, 1], \"a\", \"!=\", 5)"
    assertExpr "false" "AllByField([[1]], \"a\", \"==\", null)"
    assertExpr "false" "AllByField([\"a\"], \"a\", \"!=\", \"b\")"
    assertExpr "false" "AllByField([{a: 1}, (x => x)], \"a\", \"<\", 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1293")>]
let ``AllByField_ExaminesInOrder_AndStopsAtTheFirstFailingElement`` () =
    assertExpr "false" "AllByField([{a: 9}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = AllByField([{a: 1}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = AllByField([{a: \"x\"}, {a: 9}], \"a\", \"<\", 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1294")>]
let ``AllByField_IsPure_AndDoesNotChangeItems`` () = assertFieldFnPure "AllByField"

// == 97.3 FindByField =====================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1295")>]
let ``FindByField_ReturnsAnObjectOrNull_AndTakesTheArgumentsOfAnyByField`` () =
    assertReturnsType "FindByField" [ "object"; "null" ]
    assertExpr "\"object\"" "TypeOf(FindByField([{a: 1}], \"a\", \"==\", 1))"
    assertExpr "\"null\"" "TypeOf(FindByField([{a: 1}], \"a\", \"==\", 2))"
    assertSignature "FindByField" [ "items"; "fieldName"; "operator"; "value" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1297")>]
let ``FindByField_ReturnsTheFirstPassingElementInOrderOfPosition`` () =
    assertExpr "{\"a\": 2, \"id\": 2}" "FindByField([{a: 1, id: 1}, {a: 2, id: 2}, {a: 2, id: 3}], \"a\", \"==\", 2)"
    assertExpr "{\"a\": 3, \"id\": 3}" "FindByField([{a: 1, id: 1}, {a: 2, id: 2}, {a: 3, id: 3}], \"a\", \"==\", 3)"
    assertExpr "{\"a\": 2}" "FindByField([null, 7, \"s\", [2], {a: 2}], \"a\", \"==\", 2)"
    assertExpr "{\"a\": 5, \"id\": 1}" "FindByField([{a: 5, id: 1}, {a: 6, id: 2}], \"a\", \">\", 4)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1298")>]
let ``FindByField_ReturnsTheElementItself_NotACopy`` () =
    let source =
        "var xs = [{a: 1}, {a: 2}]\nvar r = FindByField(xs, \"a\", \"==\", 2)\nr.b = 9\nxs[1].d = 4\nOutput.r = [xs[1].b, r.d]"
    assertJson "[9, 4]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1299")>]
let ``FindByField_WhenNoElementPassesOrItemsEmpty_ReturnsNull`` () =
    assertExpr "null" "FindByField([{a: 1}], \"a\", \"==\", 5)"
    assertExpr "null" "FindByField([null, 1], \"a\", \"==\", null)"
    for op in ops do
        assertExpr "null" (fieldCall "FindByField" "[]" op "1")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1300")>]
let ``FindByField_DoesNotExamineAnElementAfterTheFirstPassing`` () =
    assertExpr "{\"a\": 1}" "FindByField([{a: 1}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = FindByField([{a: 9}, {a: \"x\"}], \"a\", \"<\", 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1301")>]
let ``FindByField_IsPure_AndDoesNotChangeItems`` () = assertFieldFnPure "FindByField"

// == 97.4 WhereByField ====================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1302")>]
let ``WhereByField_ReturnsAnArray_AndTakesTheArgumentsOfAnyByField`` () =
    assertReturnsType "WhereByField" [ "array" ]
    assertSignature "WhereByField" [ "items"; "fieldName"; "operator"; "value" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1779")>]
let ``JYRO-REQ-1779 WhereByField_ReturnsThePassingElementsInOrderOfPosition`` () =
    let xs = "[{a: 3, id: 1}, {a: 1, id: 2}, {a: 3, id: 3}, 5, {a: 3, id: 4}]"
    assertExpr "[1, 3, 4]" (sprintf "Select(WhereByField(%s, \"a\", \"==\", 3), \"id\")" xs)
    assertExpr "[1, 2, 3, 4]" (sprintf "Select(WhereByField(%s, \"a\", \">=\", 1), \"id\")" xs)
    assertExpr "[{\"a\": 3, \"id\": 1}, {\"a\": 3, \"id\": 3}, {\"a\": 3, \"id\": 4}]" (sprintf "WhereByField(%s, \"a\", \"==\", 3)" xs)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1779")>]
let ``JYRO-REQ-1779 WhereByField_TheClausesExample_ReturnsExactlyThePassingElements`` () =
    assertExpr "[{\"a\": 1}, {\"a\": 1, \"b\": 3}]" "WhereByField([{a: 1}, {a: 2}, {a: 1, b: 3}], \"a\", \"==\", 1)"
    assertExpr "2" "Length(WhereByField([{a: 1}, {a: 2}, {a: 1, b: 3}], \"a\", \"==\", 1))"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1779")>]
let ``JYRO-REQ-1779 WhereByField_LeavesOutEveryElementThatFails`` () =
    // Every element fails: the result holds none of them.
    assertExpr "[]" "WhereByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"==\", 9)"
    assertExpr "[]" "WhereByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \">\", 3)"
    assertExpr "0" "Length(WhereByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"<\", 1))"
    // Some fail: the failing ones are absent, not merely listed after.
    assertExpr "[{\"a\": 2}]" "WhereByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"==\", 2)"
    assertExpr "[{\"a\": 1}, {\"a\": 3}]" "WhereByField([{a: 1}, {a: 2}, {a: 3}], \"a\", \"!=\", 2)"
    // An element that is not an object, or lacks the field, is absent unless it passes.
    assertExpr "[]" "WhereByField([1, \"a\", [1], true, {b: 1}], \"a\", \"==\", 1)"
    assertExpr "[{\"b\": 1}]" "WhereByField([1, \"a\", [1], true, {b: 1}], \"a\", \"==\", null)"
    assertExpr "[{\"a\": 1}]" "WhereByField([null, 5, {b: 2}, {a: 1}], \"a\", \"==\", 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1779")>]
let ``JYRO-REQ-1779 WhereByField_KeepsTheOrderOfItems_SoASortedSourceGivesASortedResult`` () =
    assertExpr "[1, 2, 3, 5, 6, 7]" "Select(WhereByField([{a: 1}, {a: 2}, {a: 3}, {a: 4}, {a: 5}, {a: 6}, {a: 7}], \"a\", \"!=\", 4), \"a\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1305")>]
let ``WhereByField_ResultElementsAreTheElementsThemselves`` () =
    let source =
        "var xs = [{a: 1}, {a: 2}]\nvar r = WhereByField(xs, \"a\", \">\", 0)\nr[0].b = 7\nxs[1].c = 8\nOutput.r = [xs[0].b, r[1].c]"
    assertJson "[7, 8]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1306")>]
let ``WhereByField_ResultIsANewArray_AlsoWhenEveryElementPasses`` () =
    let all =
        "var xs = [{a: 1}, {a: 2}]\nvar r = WhereByField(xs, \"a\", \">\", 0)\nr[0] = 99\nOutput.r = [xs[0], Length(xs), Length(r)]"
    assertJson "[{\"a\": 1}, 2, 2]" (evalR all) all
    let some =
        "var xs = [{a: 1}, {a: 2}]\nvar r = WhereByField(xs, \"a\", \">\", 1)\nr[0] = 99\nOutput.r = [xs, Length(r)]"
    assertJson "[[{\"a\": 1}, {\"a\": 2}], 1]" (evalR some) some

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1307")>]
let ``WhereByField_WhenNoElementPassesOrItemsEmpty_ReturnsEmptyArray`` () =
    assertExpr "[]" "WhereByField([{a: 1}], \"a\", \"==\", 9)"
    assertExpr "[]" "WhereByField([1, null], \"a\", \"==\", null)"
    for op in ops do
        assertExpr "[]" (fieldCall "WhereByField" "[]" op "1")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1308")>]
let ``WhereByField_TestsEveryElement_PassingOrNot`` () =
    expectErr MessageCode.IncomparableTypes "Output.r = WhereByField([{a: 1}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = WhereByField([{a: 9}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = WhereByField([{a: \"x\"}, {a: 1}], \"a\", \"<\", 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1309")>]
let ``WhereByField_IsPure_AndDoesNotChangeItems`` () = assertFieldFnPure "WhereByField"

// == 97.5 Select ==========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1310")>]
let ``Select_ReturnsAnArray_AndTakesItemsAndFieldName`` () =
    assertExpr "\"array\"" "TypeOf(Select([{a: 1}], \"a\"))"
    assertExpr "\"array\"" "TypeOf(Select([], \"a\"))"
    assertExpr "\"array\"" "TypeOf(Select(null, \"a\"))"
    assertSignature "Select" [ "items"; "fieldName" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1311")>]
let ``Select_ReturnsANewArrayWithOneElementPerItem_InOrder`` () =
    assertExpr "[1, null, null, null, 3]" "Select([{a: 1}, {b: 2}, null, 5, {a: 3}], \"a\")"
    let source = "var xs = [{a: 1}, {a: 2}]\nvar r = Select(xs, \"a\")\nr[0] = 9\nOutput.r = [xs, Length(r)]"
    assertJson "[[{\"a\": 1}, {\"a\": 2}], 2]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1785")>]
let ``JYRO-REQ-1785 Select_ForAnObjectElement_GivesThePropertyValueItself`` () =
    let source =
        "var xs = [{a: {n: 1}}, {a: [1]}]\nvar r = Select(xs, \"a\")\nr[0].n = 5\nr[1][0] = 7\nOutput.r = [xs[0].a.n, xs[1].a[0]]"
    assertJson "[5, 7]" (evalR source) source
    assertExpr "[\"s\", true, 1.5]" "Select([{a: \"s\"}, {a: true}, {a: 1.5}], \"a\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1785")>]
let ``JYRO-REQ-1785 Select_ThePropertyValueIsTheSameValueNotACopy_ForEveryContainerAndAWriteInTheSourceShowsInTheResult`` () =
    // After r = Select([{a: [1]}], "a"), r[0] is the same array as items[0].a: a write through either shows in the other.
    let throughResult = "var items = [{a: [1]}]\nvar r = Select(items, \"a\")\nr[0][0] = 9\nOutput.r = items[0].a[0]"
    assertJson "9" (evalR throughResult) throughResult
    let throughSource = "var items = [{a: [1]}]\nvar r = Select(items, \"a\")\nitems[0].a[0] = 8\nOutput.r = r[0][0]"
    assertJson "8" (evalR throughSource) throughSource
    let sameArray = "var items = [{a: [1]}]\nvar r = Select(items, \"a\")\nOutput.r = r[0] == items[0].a"
    assertJson "true" (evalR sameArray) sameArray
    // An element that is the value of an object property: a write adds to the one object.
    let throughObject = "var items = [{a: {n: 1}}, {a: {n: 2}}]\nvar r = Select(items, \"a\")\nr[1].added = true\nOutput.r = [HasProperty(items[1].a, \"added\"), HasProperty(items[0].a, \"added\")]"
    assertJson "[true, false]" (evalR throughObject) throughObject
    // The value of the Input property is the Input value itself too.
    let options = { Script.defaults with Input = JsonInput "{\"items\":[{\"a\":{\"n\":1}}]}" }
    let src = "var r = Select(Input.items, \"a\")\nOutput.r = r[0] == Input.items[0].a"
    check (runR options src |> Values.asBool) src

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1787")>]
let ``JYRO-REQ-1787 Select_ForAnObjectWithoutTheProperty_GivesNull`` () =
    assertExpr "[null, null, 1]" "Select([{b: 1}, {a: null}, {a: 1}], \"a\")"
    assertExpr "[null]" "Select([{b: 1}], \"a\")"
    assertExpr "[null]" "Select([{}], \"a\")"
    assertExpr "[null]" "Select([{a: 1}], \"A\")"
    assertExpr "[null]" "Select([{a: 1}], \"a \")"
    assertExpr "[\"null\", \"null\", \"number\"]" "Map(Select([{b: 1}, {a: null}, {a: 1}], \"a\"), v => TypeOf(v))"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1788")>]
let ``JYRO-REQ-1788 Select_ForAnElementThatIsNotAnObject_GivesNull`` () =
    assertExpr "[null, null, null, null, null, null]" "Select([null, 1, \"a\", true, [1], (x => x)], \"a\")"
    assertExpr "[null, null]" "Select([\"abc\", [7]], \"length\")"
    assertExpr "[null, null]" "Select([\"abc\", [7]], \"0\")"
    assertExpr "[null, null]" "Select([\"abc\", [7]], 0)"
    assertExpr "[null, null, 2]" "Select([1, null, {a: 2}], \"a\")"
    assertExpr "[null, null, null, null, 2]" "Select([\"a\", [\"a\"], false, 0, {a: 2}], \"a\")"
    assertExpr "[6, 6]" "[Length(Select([1, null, {a: 2}, 4, 5, 6], \"a\")), Length(Select([\"a\", [\"a\"], false, 0, {a: 2}, null], \"a\"))]"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1315")>]
let ``JYRO-REQ-1315 Select_FieldNameIsOneKey_AndNullNumberBooleanAreReadAsStrings`` () =
    assertExpr "[1]" "Select([{\"a.b\": 1, a: {b: 2}}], \"a.b\")"
    assertExpr "[null]" "Select([{a: {b: 2}}], \"a.b\")"
    assertExpr "[3, null]" "Select([{\"\": 3}, {\"null\": 4}], null)"
    assertExpr "[\"x\"]" "Select([{\"1\": \"x\"}], 1)"
    assertExpr "[\"y\"]" "Select([{\"1.5\": \"y\"}], 1.5)"
    assertExpr "[1, null]" "Select([{\"1\": 1}, {\"1.0\": 2}], 1.0)"
    assertExpr "[7]" "Select([{\"true\": 7}], true)"
    assertExpr "[8]" "Select([{\"false\": 8}], false)"
    assertExpr "[null]" "Select([{\"True\": 7}], true)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1316")>]
let ``Select_WhenItemsEmptyOrNull_ReturnsEmptyArray`` () =
    assertExpr "[]" "Select([], \"a\")"
    assertExpr "[]" "Select(null, \"a\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1317")>]
let ``Select_IsPure_AndDoesNotChangeItems`` () =
    assertPureEffect "Select" "Output.r = Select([{a: 1}], \"a\")"
    assertItemsUnchanged (fun items -> sprintf "Select(%s, \"a\")" items)
    assertItemsUnchanged (fun items -> sprintf "Select(%s, \"n\")" items)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1318")>]
let ``Select_ContainerOrFunctionFieldName_RaisesArgumentTypeMismatch_AlsoForEmptyOrNullItems`` () =
    for items in [ "[{a: 1}]"; "[]"; "null" ] do
        for f in [ "[1]"; "{}"; "(x => x)"; "[\"a\"]" ] do
            expectErr MessageCode.ArgumentTypeMismatch (sprintf "Output.r = Select(%s, %s)" items f)
    expectErr MessageCode.ArgumentTypeMismatch "Output.r = Select(5, [1])"

// == 97.6 Project =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1319")>]
let ``Project_ReturnsAnArrayOfObjects_AndTakesItemsAndFields`` () =
    assertExpr "[\"array\", \"object\"]" "[TypeOf(Project([{a: 1}], [\"a\"])), TypeOf(Project([{a: 1}], [\"a\"])[0])]"
    assertExpr "\"array\"" "TypeOf(Project([], [\"a\"]))"
    assertExpr "[\"array\", \"object\"]" "[TypeOf(Project([{a: 1}], [])), TypeOf(Project([{a: 1}], [])[0])]"
    assertSignature "Project" [ "items"; "fields" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1781")>]
let ``JYRO-REQ-1781 Project_GivesOnePropertyPerField_InTheOrderOfFields`` () =
    assertExpr "[{\"b\": 2, \"a\": 1}]" "Project([{c: 3, a: 1, b: 2}], [\"b\", \"a\"])"
    assertExpr "[\"b\", \"a\"]" "Keys(Project([{c: 3, a: 1, b: 2}], [\"b\", \"a\"])[0])"
    assertExpr "[{}]" "Project([{c: 3, a: 1, b: 2}], [])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1781")>]
let ``JYRO-REQ-1781 Project_GivesNoPropertyThatIsNotNamedInFields`` () =
    assertExpr "[{\"c\": 3, \"a\": 1}]" "Project([{a: 1, b: 2, c: 3}], [\"c\", \"a\"])"
    assertExpr "[[\"c\", \"a\"], 2]" "[Keys(Project([{a: 1, b: 2, c: 3}], [\"c\", \"a\"])[0]), Length(Keys(Project([{a: 1, b: 2, c: 3}], [\"c\", \"a\"])[0]))]"
    // The properties of the element that fields does not name are absent, for each element.
    assertExpr "[{\"a\": 1}, {\"a\": 4}]" "Project([{a: 1, x: 2, y: 3}, {z: 0, a: 4, w: 5}], [\"a\"])"
    assertExpr "[false, false]" "[HasProperty(Project([{a: 1, b: 2}], [\"a\"])[0], \"b\"), HasProperty(Project([{a: 1, b: 2}], [\"b\"])[0], \"a\")]"
    // A name that the element lacks and that fields gives is present with null: the keys are exactly those of fields.
    assertExpr "[{\"z\": null, \"a\": 1}]" "Project([{a: 1, q: 5}], [\"z\", \"a\"])"
    assertExpr "[[\"z\", \"a\"]]" "Map(Project([{a: 1, q: 5}], [\"z\", \"a\"]), o => Keys(o))"
    // Keys are compared whole: a dotted name is one key, and case counts.
    assertExpr "[{\"A\": null}]" "Project([{a: 1}], [\"A\"])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1781")>]
let ``JYRO-REQ-1781 Project_GivesNewObjects_AWriteToAResultObjectDoesNotChangeItems`` () =
    let source =
        "var xs = [{a: 1, b: 2, c: 3}]\nvar r = Project(xs, [\"c\", \"a\"])\nr[0].a = 9\nr[0].extra = 1\nOutput.r = [xs, Keys(r[0])]"
    assertJson "[[{\"a\": 1, \"b\": 2, \"c\": 3}], [\"c\", \"a\", \"extra\"]]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1781")>]
let ``JYRO-REQ-1781 Project_WithFieldsThatAreNotStrings_GivesNoPropertyForThem`` () =
    assertExpr "[{\"a\": 1}]" "Project([{a: 1, b: 2}], [1, null, \"a\", true])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1321")>]
let ``JYRO-REQ-1321 Project_KeyIsTheFieldStringAsWritten_DotIncluded`` () =
    assertExpr "[{\"a.b\": 1}]" "Project([{\"a.b\": 1, a: {b: 2}}], [\"a.b\"])"
    assertExpr "[\"a.b\"]" "Keys(Project([{\"a.b\": 1, a: {b: 2}}], [\"a.b\"])[0])"
    assertExpr "[{\"a.b\": null}]" "Project([{a: {b: 2}}], [\"a.b\"])"
    assertExpr "[{\"A\": null}]" "Project([{a: 1}], [\"A\"])"
    // two dotted fields with the same last segment stay two keys
    assertExpr "[{\"x.b\": 1, \"y.b\": 2}]" "Project([{\"x.b\": 1, \"y.b\": 2}], [\"x.b\", \"y.b\"])"
    assertExpr "[\"x.b\", \"y.b\", \"b\"]" "Keys(Project([{\"x.b\": 1, \"y.b\": 2, b: 3}], [\"x.b\", \"y.b\", \"b\"])[0])"
    // keys that are not names: written as given
    assertExpr "[\".\", \"a.\", \" a\", \"\"]" "Keys(Project([{}], [\".\", \"a.\", \" a\", \"\"])[0])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1322")>]
let ``Project_PropertyValueIsTheElementsValueItself`` () =
    let source = "var xs = [{a: {n: 1}, b: [1]}]\nvar r = Project(xs, [\"a\", \"b\"])\nr[0].a.n = 5\nr[0].b[0] = 6\nOutput.r = [xs[0].a.n, xs[0].b[0]]"
    assertJson "[5, 6]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1323")>]
let ``Project_WhenElementLacksAField_GivesThatPropertyWithNull`` () =
    assertExpr "[{\"a\": 1, \"z\": null}]" "Project([{a: 1}], [\"a\", \"z\"])"
    assertExpr "[true, [\"a\", \"z\"]]" "[HasProperty(Project([{a: 1}], [\"a\", \"z\"])[0], \"z\"), Keys(Project([{a: 1}], [\"a\", \"z\"])[0])]"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1789")>]
let ``JYRO-REQ-1789 Project_RepeatedField_GivesOnePropertyAtItsFirstPosition`` () =
    assertExpr "[\"a\", \"b\"]" "Keys(Project([{a: 1, b: 2}], [\"a\", \"b\", \"a\"])[0])"
    assertExpr "[\"b\", \"a\"]" "Keys(Project([{a: 1, b: 2}], [\"b\", \"a\", \"b\", \"b\"])[0])"
    assertExpr "[{\"b\": 2, \"a\": 1}]" "Project([{a: 1, b: 2}], [\"b\", \"a\", \"b\"])"
    assertExpr "[\"b\", \"a\"]" "Keys(Project([{a: 1, b: 2}], [\"b\", \"a\", \"b\"])[0])"
    assertExpr "[2, 1]" "Values(Project([{a: 1, b: 2}], [\"b\", \"a\", \"b\"])[0])"
    assertExpr "[\"a\"]" "Keys(Project([{a: 1}], [\"a\", \"a\", \"a\"])[0])"
    // The repetition is of a string, also for an absent property, and in every object of the result.
    assertExpr "[[\"z\", \"a\"], [\"z\", \"a\"]]" "Map(Project([{a: 1}, {a: 2, z: 3}], [\"z\", \"a\", \"z\"]), o => Keys(o))"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1325")>]
let ``Project_IgnoresFieldsThatAreNotStrings`` () =
    assertExpr "[{\"a\": 1}]" "Project([{a: 1, \"1\": 2, \"true\": 3, \"\": 4, \"null\": 5}], [1, true, null, [\"a\"], {}, \"a\"])"
    assertExpr "[{}]" "Project([{a: 1, \"1\": 2}], [1, (x => x)])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1326")>]
let ``Project_NullFieldsIsEmpty_OtherNonArrayFieldsRaiseArgumentTypeMismatch`` () =
    assertExpr "[{}]" "Project([{a: 1}], null)"
    for items in [ "[{a: 1}]"; "[]" ] do
        for f in [ "\"a\""; "1"; "{a: 1}"; "true"; "(x => x)" ] do
            expectErr MessageCode.ArgumentTypeMismatch (sprintf "Output.r = Project(%s, %s)" items f)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1327")>]
let ``Project_GivesNoResultForANonObjectElement_AndKeepsOrder`` () =
    assertExpr "[{\"id\": 1}, {\"id\": 2}]" "Project([null, {a: 1, id: 1}, 5, \"s\", [1], (x => x), {a: 2, id: 2}], [\"id\"])"
    assertExpr "[]" "Project([1, 2], [\"a\"])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1328")>]
let ``Project_IsPure_DoesNotChangeItems_AndReturnsNewObjects`` () =
    assertPureEffect "Project" "Output.r = Project([{a: 1}], [\"a\"])"
    let source =
        "var xs = [{a: 1, b: 2}]\nvar r = Project(xs, [\"a\", \"b\"])\nvar same = r[0] == xs[0]\nr[0].a = 9\nr[0].c = 1\nr[0] = null\nOutput.r = [xs, same]"
    assertJson "[[{\"a\": 1, \"b\": 2}], true]" (evalR source) source
    assertItemsUnchanged (fun items -> sprintf "Project(%s, [\"a\", \"n\"])" items)

// == 97.7 Omit ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1329")>]
let ``Omit_ReturnsAnArrayOfObjects_AndTakesItemsAndFields`` () =
    assertExpr "[\"array\", \"object\"]" "[TypeOf(Omit([{a: 1}], [\"a\"])), TypeOf(Omit([{a: 1}], [\"a\"])[0])]"
    assertExpr "\"array\"" "TypeOf(Omit([], [\"a\"]))"
    assertSignature "Omit" [ "items"; "fields" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1782")>]
let ``JYRO-REQ-1782 Omit_KeepsEveryPropertyNotNamedInFields`` () =
    assertExpr "[{\"a\": 1, \"c\": 3}]" "Omit([{a: 1, b: 2, c: 3}], [\"b\"])"
    assertExpr "[{\"b\": 2}]" "Omit([{a: 1, b: 2, c: 3}], [\"a\", \"c\"])"
    assertExpr "[{\"a\": 1, \"b\": 2, \"c\": 3}]" "Omit([{a: 1, b: 2, c: 3}], [])"
    assertExpr "[{}]" "Omit([{a: 1, b: 2}], [\"a\", \"b\"])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1782")>]
let ``JYRO-REQ-1782 Omit_GivesNoPropertyWhoseKeyIsNamedInFields`` () =
    assertExpr "[false, true, true]" "[HasProperty(Omit([{a: 1, b: 2, c: 3}], [\"b\"])[0], \"b\"), HasProperty(Omit([{a: 1, b: 2, c: 3}], [\"b\"])[0], \"a\"), HasProperty(Omit([{a: 1, b: 2, c: 3}], [\"b\"])[0], \"c\")]"
    assertExpr "[[\"c\"], [\"a\", \"c\"], []]" "[Keys(Omit([{a: 1, b: 2, c: 3}], [\"a\", \"b\"])[0]), Keys(Omit([{a: 1, b: 2, c: 3}], [\"b\"])[0]), Keys(Omit([{a: 1, b: 2, c: 3}], [\"c\", \"b\", \"a\"])[0])]"
    // Each element of items is treated alone, and a null or falsy value that is kept is kept.
    assertExpr "[{\"b\": null}, {\"b\": 0, \"c\": false}]" "Omit([{a: 1, b: null}, {a: 2, b: 0, c: false}], [\"a\"])"
    // The result object is a new object: a write to it does not change the element.
    let source = "var xs = [{a: 1, b: 2, c: 3}]\nvar r = Omit(xs, [\"b\"])\nr[0].a = 9\nr[0].b = 7\nOutput.r = [xs, r]"
    assertJson "[[{\"a\": 1, \"b\": 2, \"c\": 3}], [{\"a\": 9, \"c\": 3, \"b\": 7}]]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1331")>]
let ``Omit_KeepsThePropertyOrderOfTheElement`` () =
    let source =
        "var e = {z: 1, a: 2, m: 3}\ne.y = 4\ne.z = 9\nvar r = Omit([e], [\"a\"])\nOutput.r = [Keys(r[0]), r[0]]"
    assertJson "[[\"z\", \"m\", \"y\"], {\"z\": 9, \"m\": 3, \"y\": 4}]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1332")>]
let ``Omit_KeptValueIsTheElementsValueItself`` () =
    let source = "var xs = [{a: {n: 1}, b: 1}]\nvar r = Omit(xs, [\"b\"])\nr[0].a.n = 5\nOutput.r = xs[0].a.n"
    assertJson "5" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1333")>]
let ``Omit_ComparesWholeKeys_DotNamesOneKey_NeverANestedProperty`` () =
    assertExpr "[{\"a\": {\"b\": 2, \"c\": 3}}]" "Omit([{\"a.b\": 1, a: {b: 2, c: 3}}], [\"a.b\"])"
    assertExpr "[{\"a\": {\"b\": 2}}]" "Omit([{a: {b: 2}}], [\"a.b\"])"
    assertExpr "[{\"ab\": 1}]" "Omit([{ab: 1, a: 2}], [\"a\"])"
    assertExpr "[{\"a\": 1}]" "Omit([{a: 1}], [\"A\"])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1790")>]
let ``JYRO-REQ-1790 Omit_FieldNamingNoProperty_HasNoEffect`` () =
    assertExpr "[{\"a\": 1}]" "Omit([{a: 1}], [\"zz\"])"
    assertExpr "[{}, {\"b\": 2}]" "Omit([{a: 1}, {b: 2}], [\"a\"])"
    assertExpr "[{\"a\": 1}]" "Omit([{a: 1}], [\"z\"])"
    // Present and absent names together: only the present one has an effect.
    assertExpr "[{\"b\": 2}]" "Omit([{a: 1, b: 2}], [\"z\", \"a\", \"y\"])"
    // Names that differ only by case, by a space or by a dot name no property, and there is no error.
    assertExpr "[{\"a\": 1, \"b\": {\"c\": 2}}]" "Omit([{a: 1, b: {c: 2}}], [\"A\", \"a \", \"b.c\"])"
    // An empty fields leaves every element as it is, and gives no error.
    assertExpr "[{\"a\": 1}, {\"b\": 2}]" "Omit([{a: 1}, {b: 2}], [])"
    let source = "var xs = [{a: 1}]\nvar r = Omit(xs, [\"z\"])\nOutput.r = [r, Length(r), xs]"
    assertJson "[[{\"a\": 1}], 1, [{\"a\": 1}]]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1335")>]
let ``Omit_IgnoresFieldsThatAreNotStrings`` () =
    assertExpr "[{\"a\": 1, \"1\": 2, \"true\": 3, \"\": 4, \"null\": 5}]" "Omit([{a: 1, \"1\": 2, \"true\": 3, \"\": 4, \"null\": 5}], [1, true, null, [\"a\"], {}, (x => x)])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1336")>]
let ``Omit_NullFieldsIsEmpty_OtherNonArrayFieldsRaiseArgumentTypeMismatch`` () =
    assertExpr "[{\"a\": 1}]" "Omit([{a: 1}], null)"
    for items in [ "[{a: 1}]"; "[]" ] do
        for f in [ "\"a\""; "1"; "{a: 1}"; "true"; "(x => x)" ] do
            expectErr MessageCode.ArgumentTypeMismatch (sprintf "Output.r = Omit(%s, %s)" items f)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1337")>]
let ``Omit_GivesNoResultForANonObjectElement_AndKeepsOrder`` () =
    assertExpr "[{\"id\": 1}, {\"id\": 2}]" "Omit([null, {id: 1, x: 0}, 5, \"s\", [1], (x => x), {id: 2, x: 0}], [\"x\"])"
    assertExpr "[]" "Omit([1, 2], [])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1338")>]
let ``Omit_IsPure_DoesNotChangeItems_AndReturnsNewObjects_AlsoWhenNothingIsRemoved`` () =
    assertPureEffect "Omit" "Output.r = Omit([{a: 1}], [\"a\"])"
    let source =
        "var xs = [{a: 1, b: 2}]\nvar r = Omit(xs, [])\nr[0].a = 9\nr[0].c = 1\nr[0] = null\nOutput.r = xs"
    assertJson "[{\"a\": 1, \"b\": 2}]" (evalR source) source
    assertItemsUnchanged (fun items -> sprintf "Omit(%s, [\"a\"])" items)

// == 97.8 CountIf =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1339")>]
let ``CountIf_ReturnsANumber_AndTakesTheArgumentsOfAnyByField`` () =
    assertReturnsType "CountIf" [ "number" ]
    assertSignature "CountIf" [ "items"; "fieldName"; "operator"; "value" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1341")>]
let ``CountIf_ReturnsTheNumberOfPassingElements`` () =
    let xs = "[{a: 1}, {a: 2}, {a: 2}, null, {b: 2}]"
    for (op, v, n) in [ "==", "2", "2"; ">=", "1", "3"; "==", "null", "1"; "!=", "2", "2"; "<", "0", "0" ] do
        assertExpr n (fieldCall "CountIf" xs op v)
        let cross = sprintf "%s == Length(%s)" (fieldCall "CountIf" xs op v) (fieldCall "WhereByField" xs op v)
        assertExpr "true" cross

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1342")>]
let ``CountIf_WhenItemsEmpty_IsZero`` () =
    for op in ops do
        assertExpr "0" (fieldCall "CountIf" "[]" op "1")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1343")>]
let ``JYRO-REQ-1343 CountIf_ResultIsAnExactNumber`` () =
    // Only the representation is judged here; the count itself is 97.8.3's.
    for items in [ "[]"; "null"; "[{a: 1}]"; "[{a: 1}, {a: 1}, {a: 1}, {a: 2}]"; "[1, {a: 2}]" ] do
        for op in ops do
            let call = fieldCall "CountIf" items op "1"
            let v = expr call
            check (Values.isExact v) (sprintf "%s gave an inexact number %s" call (Values.toJson v))
    let input = Values.obj [ "xs", Values.json "[{\"a\": 1}, {\"a\": 1.5}]" ]
    let v = runR { Script.defaults with Input = ValueInput input } "Output.r = CountIf(Input.xs, \"a\", \">\", 0)"
    check (Values.isExact v) (sprintf "CountIf over Input gave an inexact number %s" (Values.toJson v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1344")>]
let ``CountIf_TestsEveryElement_InOrder`` () =
    expectErr MessageCode.IncomparableTypes "Output.r = CountIf([{a: 1}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = CountIf([{a: 9}, {a: \"x\"}], \"a\", \"<\", 5)"
    expectErr MessageCode.IncomparableTypes "Output.r = CountIf([{a: \"x\"}, {a: 1}], \"a\", \"<\", 5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1345")>]
let ``CountIf_IsPure_AndDoesNotChangeItems`` () = assertFieldFnPure "CountIf"

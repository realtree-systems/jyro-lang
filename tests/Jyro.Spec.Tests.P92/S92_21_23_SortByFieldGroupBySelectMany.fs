/// jyro-spec 92.21 SortByField, 92.22 GroupBy, 92.23 SelectMany.
module RealTree.Jyro.Spec.Tests.S92_21_23_SortByFieldGroupBySelectMany

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P92Support

/// Script text that sets `Output.ids` to the `id` of each element of `r`.
let private idsOfR = "var ids = []\nforeach e in r do\n  ids[Length(ids)] = e.id\nend\nOutput.ids = ids"

/// Run `prelude`, then `var r = <expr>`, and return the ids of `r` as JSON.
let private sortedIds (prelude: string) (expr: string) : JyroValue =
    outProp "ids" (sprintf "%s\nvar r = %s\n%s" prelude expr idsOfR)

// =============================================================================
// 92.21 SortByField
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-812")>]
let ``SortByField_TakesArrayOrNullFieldNameAndDirection_ReturnsArray`` () =
    assertSignature "SortByField" [ "items", false; "fieldName", false; "direction", false ]
    typesAre "array" [ "SortByField([{k: 2}, {k: 1}], \"k\", \"asc\")"; "SortByField(null, \"k\", \"desc\")" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-813")>]
let ``JYRO-REQ-813 SortByField_OrdersByFieldValueUnderTheSortOrder`` () =
    evalsTo ""
        [ "SortByField([{v: 3}, {v: 1}, {v: 2}], \"v\", \"asc\")", "[{\"v\": 1}, {\"v\": 2}, {\"v\": 3}]"
          "SortByField([{v: 10}, {v: 9}], \"v\", \"asc\")", "[{\"v\": 9}, {\"v\": 10}]" ]
    let items =
        "[{v: \"b\", id: 0}, {v: 2, id: 1}, {v: [1], id: 2}, {v: null, id: 3}, {v: x => x, id: 4}, {v: true, id: 5}, {v: {x: 1}, id: 6}, {v: 1, id: 7}, {v: false, id: 8}, {v: \"a\", id: 9}]"
    assertJson "[3, 8, 5, 7, 1, 9, 0, 2, 6, 4]" (sortedIds "" (sprintf "SortByField(%s, \"v\", \"asc\")" items))
    let abc = "var a = 1\nvar b = 1.0000000000000000000000000001\nvar c = Power(1, 0.5)"
    assertJson "[\"a\", \"b\", \"c\"]"
        (sortedIds abc "SortByField([{v: c, id: \"c\"}, {v: b, id: \"b\"}, {v: a, id: \"a\"}], \"v\", \"asc\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-814")>]
let ``SortByField_ElementWithoutField_SortsAsNull`` () =
    let items = "[{v: 2, id: \"two\"}, {id: \"m\"}, {v: null, id: \"n\"}, {v: 1, id: \"one\"}]"
    assertJson "[\"m\", \"n\", \"one\", \"two\"]" (sortedIds "" (sprintf "SortByField(%s, \"v\", \"asc\")" items))
    assertJson "[\"two\", \"one\", \"m\", \"n\"]" (sortedIds "" (sprintf "SortByField(%s, \"v\", \"desc\")" items))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-815")>]
let ``JYRO-REQ-815 SortByField_NonObjectElement_SortsAsNull`` () =
    evalsTo ""
        [ "SortByField([{v: 2}, 7, \"s\", null, [1], {v: 1}], \"v\", \"asc\")", "[7, \"s\", null, [1], {\"v\": 1}, {\"v\": 2}]"
          "SortByField([{v: 2}, 7, {v: null}, \"s\"], \"v\", \"asc\")", "[7, {\"v\": null}, \"s\", {\"v\": 2}]"
          // a non-object element sorts as null: before false, a number or a string
          "SortByField([{v: false}, 3, {v: \"a\"}, {v: 0}], \"v\", \"asc\")", "[3, {\"v\": false}, {\"v\": 0}, {\"v\": \"a\"}]"
          // in reverse for desc; tied elements keep their order (92.21.8)
          "SortByField([{v: 2}, 7, {v: 1}, \"s\"], \"v\", \"desc\")", "[{\"v\": 2}, {\"v\": 1}, 7, \"s\"]" ]
    let f = out "var f = x => x\nvar r = SortByField([{v: 1, id: 0}, f, {v: 0, id: 1}], \"v\", \"asc\")\nOutput.first = r[0] == f\nOutput.ids = [r[1].id, r[2].id]"
    assertJson "true" (Values.prop "first" f)
    assertJson "[1, 0]" (Values.prop "ids" f)

let private mixed = "[{v: 3, id: 3}, {v: null, id: \"null\"}, {v: 1, id: 1}, {v: \"a\", id: \"a\"}, {v: 2, id: 2}]"

// -- 92.21.5 to 92.21.7 and JYRO-REQ-1868: the direction, on every path (4.1.8) -------

/// Every limit that could end a run set explicitly (FLOORS 4.1.11).
let private sbfOptions : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits (fun o ->
            { o with
                MaxExecutionTime = System.TimeSpan.FromSeconds 30.
                MaxStatements = 1_000_000
                MaxLoopIterations = 1_000_000
                MaxCallDepth = 64 }))
        CompilationTimeout = Some (System.TimeSpan.FromMinutes 10.) }

/// The ways a script can call SortByField: every argument by position, every
/// argument by name, and by position inside a function body.
let private sbfPaths = [ "positional"; "named"; "in a function" ]

/// Script text that sets `r` to SortByField(items, "v", direction) on `path`.
let private sbfCall (path: string) (items: string) (direction: string) : string =
    match path with
    | "positional" -> sprintf "var r = SortByField(%s, \"v\", %s)\n" items direction
    | "named" -> sprintf "var r = SortByField(items: %s, fieldName: \"v\", direction: %s)\n" items direction
    | _ -> sprintf "func SortIt(d)\n  return SortByField(%s, \"v\", d)\nend\nvar r = SortIt(%s)\n" items direction

/// Run SortByField on `path` with `direction` reaching it as a Jyro expression
/// (`Literal text`) or as the host value `Input.d` (`Host value`).
type private Direction =
    | Literal of string
    | Host of JyroValue

let private runSbf (path: string) (items: string) (direction: Direction) (after: string) : JyroResult<JyroValue> =
    match direction with
    | Literal text -> Script.runWith sbfOptions (sbfCall path items text + after)
    | Host v ->
        Script.runWith { sbfOptions with Input = ValueInput(Values.obj [ "d", v ]) } (sbfCall path items "Input.d" + after)

let private forms = [ "literal"; "host" ]

let private directionOf (form: string) (s: string) : Direction =
    if form = "literal" then Literal(AsciiCase.literal s) else Host(Values.str s)

let private sortedOn (path: string) (direction: Direction) : JyroValue =
    Script.output (runSbf path mixed direction idsOfR) |> Values.prop "ids"

let private sortOrderIds = "[\"null\", 1, 2, 3, \"a\"]"
let private reverseIds = "[\"a\", 3, 2, 1, \"null\"]"

/// Theory rows: every path, every form, every case (by label; a case with an
/// unpaired surrogate only in the host form, since no literal can hold one).
let private directionRows (cases: string list) : obj[] seq =
    seq {
        for path in sbfPaths do
            for form in forms do
                for s in cases do
                    if form = "host" || not (AsciiCase.hasUnpairedSurrogate s) then
                        yield [| box path; box form; box (AsciiCase.label s) |]
    }

let private descCases = [ "desc"; "DESC"; "DeSc"; "dEsC"; "Desc" ]
let descRows () : obj[] seq = directionRows descCases

[<Theory>]
[<Trait("requirement", "JYRO-REQ-816")>]
[<MemberData(nameof descRows)>]
let ``SortByField_DescWithNoCaseSensitivityForAsciiLetters_ReversesTheSortOrder`` (path: string, form: string, caseLabel: string) =
    let d = AsciiCase.byLabel descCases caseLabel
    let got = sortedOn path (directionOf form d)
    assertJson reverseIds got
    // The reverse of what "asc" gives on the same items and path.
    let asc = sortedOn path (Literal "\"asc\"") |> Values.asList |> List.rev
    Assert.Equal<string list>(asc |> List.map Values.toJson, Values.asList got |> List.map Values.toJson)

let private ascCases = [ "asc"; "ASC"; "aSc"; "Asc"; "asC" ]
let ascRows () : obj[] seq = directionRows ascCases

[<Theory>]
[<Trait("requirement", "JYRO-REQ-817")>]
[<MemberData(nameof ascRows)>]
let ``SortByField_AscWithNoCaseSensitivityForAsciiLetters_IsTheSortOrder`` (path: string, form: string, caseLabel: string) =
    assertJson sortOrderIds (sortedOn path (directionOf form (AsciiCase.byLabel ascCases caseLabel)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-816")>]
[<Trait("requirement", "JYRO-REQ-817")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<InlineData("tr-TR")>]
[<InlineData("")>]
let ``JYRO-REQ-816 JYRO-REQ-817 JYRO-REQ-1941 SortByField_DirectionUnderAnotherCulture_IsComparedTheSameWay`` (culture: string) =
    // 10.7.1 reads no case mapping, so the current culture changes nothing.
    AsciiCase.underCulture culture (fun () ->
        for form in forms do
            for d in [ "DESC"; "DeSc" ] do assertJson reverseIds (sortedOn "positional" (directionOf form d))
            for d in [ "ASC"; "aSc" ] do assertJson sortOrderIds (sortedOn "positional" (directionOf form d)))

/// The items with which every direction is checked: the row-5 check is made
/// also when items is empty or null (92.21.7).
let private refusalItems = [ "[{v: 1}]"; "[]"; "null" ]

let private refusalRows (cases: string list) : obj[] seq =
    seq {
        for row in directionRows cases do
            for items in refusalItems do
                yield Array.append row [| box items |]
    }

let private expectInvalidSortDirection (r: JyroResult<JyroValue>) =
    Script.expectError MessageCode.InvalidSortDirection r

/// The result fails with the one error InvalidSortDirection (92.21.7), whose
/// diagnostic has exactly one argument (130.6.2), a string equal, code unit for
/// code unit, to `expected`.
let private expectInvalidSortDirectionNaming (expected: string) (r: JyroResult<JyroValue>) =
    Script.expectError MessageCode.InvalidSortDirection r
    let m = Script.onlyErrorMessage r
    Assert.NotNull(m.Args)
    Assert.Equal(1, m.Args.Length)
    match m.Args.[0] with
    | :? string as s -> Assert.Equal(expected, s)
    | other -> Values.fail (sprintf "The argument is a %s, not the string %A" (if isNull other then "null" else other.GetType().Name) expected)

/// Strings that are neither "asc" nor "desc" in any spelling.
let private otherDirections =
    [ "decs"; "ascending"; "descending"; ""; " asc"; "asc "; "\tdesc"; "up"; "down"; "as"; "de"; "des"; "descs"; "ascc" ]

let otherDirectionRows () : obj[] seq = refusalRows otherDirections

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<MemberData(nameof otherDirectionRows)>]
let ``JYRO-REQ-2170 SortByField_WhenDirectionIsNotAscOrDesc_RaisesInvalidSortDirectionNamingItAsGiven`` (path: string, form: string, caseLabel: string, items: string) =
    let d = AsciiCase.byLabel otherDirections caseLabel
    expectInvalidSortDirectionNaming d (runSbf path items (directionOf form d) "")

/// Strings that differ from "asc" or "desc" only where 10.7.1 decides: U+017F
/// (the long s) or U+212A (KELVIN SIGN) in place of an ASCII letter, or an
/// unpaired surrogate (host form only).
let private lookAlikeDirections =
    [ "a" + AsciiCase.longS + "c"
      "de" + AsciiCase.longS + "c"
      "A" + AsciiCase.longS + "C"
      "DE" + AsciiCase.longS + "C"
      "de" + AsciiCase.loneLow + "c"
      "desc" + AsciiCase.loneLow
      AsciiCase.loneLow + "asc" ]

let lookAlikeDirectionRows () : obj[] seq = refusalRows lookAlikeDirections

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<MemberData(nameof lookAlikeDirectionRows)>]
let ``JYRO-REQ-2170 SortByField_WhenDirectionIsAscOrDescOnlyByANonAsciiLookAlike_RaisesInvalidSortDirectionNamingItAsGiven`` (path: string, form: string, caseLabel: string, items: string) =
    let d = AsciiCase.byLabel lookAlikeDirections caseLabel
    expectInvalidSortDirectionNaming d (runSbf path items (directionOf form d) "")

/// 92.21.7's examples: the diagnostic names the direction exactly as the script gave it.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"decs\")", "decs")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"DECS\")", "DECS")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"a\\u017Fc\")", "aſc")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"de\\u017Fc\")", "deſc")>]
[<InlineData("SortByField(null, \"v\", \"up\")", "up")>]
[<InlineData("SortByField([], \"v\", \"up\")", "up")>]
[<InlineData("SortByField([], \"v\", true)", "true")>]
[<InlineData("SortByField([{v: 1}], \"v\", null)", "null")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"\")", "")>]
[<InlineData("SortByField([{v: 1}], \"v\", \" asc\")", " asc")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"desc \")", "desc ")>]
[<InlineData("SortByField([{v: 1}], \"v\", \"Ascending\")", "Ascending")>]
let ``JYRO-REQ-2170 SortByField_TheClausesExamples_NameTheDirectionAsGiven`` (call: string, expected: string) =
    expectInvalidSortDirectionNaming expected (Script.runWith sbfOptions ("var r = " + call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<InlineData("0", "0")>]
[<InlineData("1", "1")>]
[<InlineData("-1", "-1")>]
[<InlineData("1.5", "1.5")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("null", "null")>]
let ``JYRO-REQ-2170 SortByField_NullNumberOrBooleanDirection_NamesItsStringForm`` (text: string, expected: string) =
    for items in refusalItems do
        for path in sbfPaths do
            expectInvalidSortDirectionNaming expected (runSbf path items (Literal text) "")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2170")>]
let ``JYRO-REQ-2170 SortByField_ValidDirectionsInAnyAsciiCase_AreNotRefused`` () =
    for items in refusalItems do
        for d in [ "ASC"; "Asc"; "DESC"; "dEsC" ] do
            let r = runSbf "positional" items (Literal(AsciiCase.literal d)) ""
            Assert.True(r.IsSuccess, sprintf "%s %s: %s" items d (Script.describe r))

/// A wrong items or fieldName is reported before a bad direction (92.21.7, 90.3.8).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<InlineData("SortByField(5, \"v\", \"decs\")")>]
[<InlineData("SortByField(\"x\", \"v\", \"up\")")>]
[<InlineData("SortByField([{v: 1}], [], \"up\")")>]
[<InlineData("SortByField(null, {}, \"decs\")")>]
let ``JYRO-REQ-2170 SortByField_WrongItemsOrFieldName_IsReportedBeforeADirectionString`` (call: string) =
    Script.expectError MessageCode.ArgumentTypeMismatch (Script.runWith sbfOptions ("var r = " + call))

/// 92.21.7 and JYRO-REQ-1868: the direction examples of JYRO-REQ-1868.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1868")>]
[<InlineData("SortByField([{v: 1}], \"v\", null)")>]
[<InlineData("SortByField([{v: 1}], \"v\", false)")>]
[<InlineData("SortByField([{v: 1}], \"v\", 0)")>]
[<InlineData("SortByField([{v: 1}], \"v\", 1.5)")>]
[<InlineData("SortByField([], \"v\", true)")>]
[<InlineData("SortByField(null, \"v\", -1)")>]
let ``JYRO-REQ-1868 SortByField_TheClausesExamples_RaiseInvalidSortDirectionAndNoOtherError`` (call: string) =
    let r = Script.runWith sbfOptions ("var r = " + call)
    expectInvalidSortDirection r
    Assert.DoesNotContain(MessageCode.ArgumentTypeMismatch, Script.codes r)
    Assert.Equal<MessageCode list>([ MessageCode.InvalidSortDirection ], Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1868")>]
let ``JYRO-REQ-1868 SortByField_WrongItemsWithNullDirection_RaisesArgumentTypeMismatchForItems`` () =
    let r = Script.runWith sbfOptions "var r = SortByField(5, \"v\", null)"
    Script.expectError MessageCode.ArgumentTypeMismatch r

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<Trait("requirement", "JYRO-REQ-822")>]
[<Trait("requirement", "JYRO-REQ-825")>]
[<InlineData("SortByField(5, \"v\", null)")>]
[<InlineData("SortByField(\"x\", \"v\", true)")>]
[<InlineData("SortByField(5, \"v\", \"decs\")")>]
[<InlineData("SortByField([{v: 1}], [], 1)")>]
[<InlineData("SortByField([{v: 1}], {}, \"up\")")>]
[<InlineData("SortByField(null, x => x, \"up\")")>]
let ``SortByField_WrongItemsOrFieldName_IsReportedBeforeADirectionError`` (call: string) =
    // A wrong items (92.21.14) or fieldName (92.21.11) is reported first (92.21.7, 90.3.8).
    Script.expectError MessageCode.ArgumentTypeMismatch (Script.runWith sbfOptions ("var r = " + call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-816")>]
[<Trait("requirement", "JYRO-REQ-817")>]
[<Trait("requirement", "JYRO-REQ-2170")>]
[<InlineData("positional")>]
[<InlineData("named")>]
[<InlineData("in a function")>]
let ``SortByField_AscAndDesc_AreAcceptedWithEveryItems`` (path: string) =
    // Controls for the refusals above: the same items and paths, a good direction.
    for items in refusalItems do
        for d in [ "\"asc\""; "\"desc\""; "\"ASC\""; "\"Desc\"" ] do
            let r = runSbf path items (Literal d) ""
            Assert.True(r.IsSuccess, sprintf "%s %s %s: %s" path items d (Script.describe r))

/// The directions of JYRO-REQ-1868: null, numbers and booleans, each written as a
/// Jyro literal and as a host value.
let private scalarDirections : (string * JyroValue) list =
    [ "null", Values.nul
      "0", Values.int 0
      "1", Values.int 1
      "-1", Values.int (-1)
      "1.5", Values.exact 1.5M
      "1e21", Values.exact 1000000000000000000000M
      "true", Values.bool true
      "false", Values.bool false ]

let scalarDirectionRows () : obj[] seq =
    seq {
        for path in sbfPaths do
            for form in forms do
                for (text, _) in scalarDirections do
                    for items in refusalItems do
                        yield [| box path; box form; box text; box items |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1868")>]
[<MemberData(nameof scalarDirectionRows)>]
let ``JYRO-REQ-1868 SortByField_NullNumberOrBooleanDirection_IsRefusedWithInvalidSortDirection`` (path: string, form: string, text: string, items: string) =
    // None of "null", "0", "1", "-1", "1.5", "1000000000000000000000", "true" or
    // "false" is asc or desc, so 92.21.7 refuses each. A null direction is not
    // the default "asc", and none raises ArgumentTypeMismatch.
    let direction = if form = "literal" then Literal text else Host(scalarDirections |> List.find (fst >> (=) text) |> snd)
    let r = runSbf path items direction ""
    expectInvalidSortDirection r
    Assert.DoesNotContain(MessageCode.ArgumentTypeMismatch, Script.codes r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1868")>]
[<Trait("requirement", "JYRO-REQ-823")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``SortByField_ContainerOrFunctionDirection_IsNotReadAsAString`` (direction: string) =
    // 92.21.12 names the only directions that fail the type check (JYRO-REQ-1868).
    for items in refusalItems do
        Script.expectError MessageCode.ArgumentTypeMismatch (runSbf "positional" items (Literal direction) "")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-819")>]
let ``SortByField_TiedElementsKeepInputOrderInBothDirections`` () =
    let prelude =
        "var items = [{v: 1, id: \"a\"}, {v: 0, id: \"b\"}, {v: 1, id: \"c\"}, {v: 0, id: \"d\"}]\nvar i = 0\nwhile i < 60 do\n  items[Length(items)] = {v: 5, id: i}\n  i = i + 1\nend"
    // The ids compared by value (assertJson), not by the text of the numbers (4.1.15).
    let extra = [ for i in 0 .. 59 -> string i ]
    let run expr =
        let o = Script.output (runLoops 200 (sprintf "%s\nvar r = %s\n%s" prelude expr idsOfR))
        Values.prop "ids" o
    let list (xs: string list) = sprintf "[%s]" (System.String.Join(", ", xs))
    assertJson (list ([ "\"b\""; "\"d\""; "\"a\""; "\"c\"" ] @ extra)) (run "SortByField(items, \"v\", \"asc\")")
    assertJson (list (extra @ [ "\"a\""; "\"c\""; "\"b\""; "\"d\"" ])) (run "SortByField(items, \"v\", \"desc\")")
    // Two inexact numbers of equal double value, and two arrays, are tied too.
    let tied = "var one = Power(1, 0.5)\nvar two = Power(1, 0.25)"
    let items = "[{v: one, id: 1}, {v: two, id: 2}, {v: [2], id: 3}, {v: [1], id: 4}]"
    assertJson "[1, 2, 3, 4]" (sortedIds tied (sprintf "SortByField(%s, \"v\", \"asc\")" items))
    assertJson "[3, 4, 1, 2]" (sortedIds tied (sprintf "SortByField(%s, \"v\", \"desc\")" items))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-820")>]
let ``SortByField_FieldNameWithDotIsOneKey`` () =
    assertJson "[3, 1, 2]"
        (sortedIds "" "SortByField([{\"a.b\": 2, id: 2}, {\"a.b\": 1, id: 1}, {a: {b: 0}, id: 3}], \"a.b\", \"asc\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-821")>]
let ``SortByField_NullNumberOrBooleanFieldName_ReadsItsStringForm`` () =
    checkAll
        [ "null", (fun () -> assertJson "[1, 2]" (sortedIds "" "SortByField([{\"\": 2, id: 2}, {\"\": 1, id: 1}], null, \"asc\")"))
          "1", (fun () -> assertJson "[1, 2]" (sortedIds "" "SortByField([{\"1\": 2, id: 2}, {\"1\": 1, id: 1}], 1, \"asc\")"))
          "true", (fun () -> assertJson "[1, 2]" (sortedIds "" "SortByField([{\"true\": 2, id: 2}, {\"true\": 1, id: 1}], true, \"asc\")"))
          "1.5", (fun () -> assertJson "[1, 2]" (sortedIds "" "SortByField([{\"1.5\": 2, id: 2}, {\"1.5\": 1, id: 1}], 1.5, \"asc\")")) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-822")>]
let ``SortByField_WhenFieldNameIsArrayObjectOrFunction_RaisesArgumentTypeMismatch`` () =
    let bad = [ "[]"; "[\"k\"]"; "{}"; "x => x" ]
    errorsWith MessageCode.ArgumentTypeMismatch "" [ for f in bad -> sprintf "SortByField([{k: 1}], %s, \"asc\")" f ]
    errorsWith MessageCode.ArgumentTypeMismatch "" [ for f in bad -> sprintf "SortByField(null, %s, \"asc\")" f ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-823")>]
let ``SortByField_WhenDirectionIsArrayObjectOrFunction_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ for d in [ "[]"; "[\"asc\"]"; "{}"; "x => x" ] -> sprintf "SortByField([{k: 1}], \"k\", %s)" d ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-824")>]
let ``SortByField_OfNullItems_ReturnsEmptyArray`` () =
    evalsTo "" [ "SortByField(null, \"k\", \"asc\")", "[]"; "SortByField(null, \"k\", \"desc\")", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-825")>]
let ``SortByField_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "SortByField(%s, \"k\", \"asc\")" v)
        [ "SortByField([{k: 1}], \"k\", \"asc\")"; "SortByField(null, \"k\", \"asc\")" ]

let private sbfItems = "[{\"k\": 3}, {\"k\": 1}, {\"k\": 2, \"t\": [1]}]"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-826")>]
let ``SortByField_ReturnsNewArray_WritesToResultDoNotChangeItems`` () =
    assertNewArray sbfItems (fun a -> sprintf "SortByField(%s, \"k\", \"asc\")" a)
    assertNewArray "[{\"k\": 1}, {\"k\": 2}]" (fun a -> sprintf "SortByField(%s, \"k\", \"asc\")" a)
    assertNewArray "[{\"k\": 1}]" (fun a -> sprintf "SortByField(%s, \"k\", \"desc\")" a)
    assertNewArray "[]" (fun a -> sprintf "SortByField(%s, \"k\", \"asc\")" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-827")>]
let ``SortByField_DoesNotChangeOrderOfItems`` () =
    assertUnchanged sbfItems (fun a -> sprintf "SortByField(%s, \"k\", \"asc\")" a)
    assertUnchanged sbfItems (fun a -> sprintf "SortByField(%s, \"k\", \"desc\")" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-828")>]
let ``SortByField_ResultElementsAreTheElementsOfItemsNotCopies`` () =
    // o sorts after {k: 0}; the array n is the only element, so its place is fixed.
    let o =
        out "var o = {k: 1}\nvar r = SortByField([o, {k: 0}], \"k\", \"asc\")\nr[1].m = 2\nOutput.m = o.m\nvar n = [1]\nvar r2 = SortByField([n], \"k\", \"asc\")\nr2[0][0] = 5\nOutput.n0 = n[0]"
    assertJson "2" (Values.prop "m" o)
    assertJson "5" (Values.prop "n0" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-829")>]
let ``SortByField_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument (sprintf "{\"a\": %s}" sbfItems) (fun a -> sprintf "SortByField(%s, \"k\", \"asc\")" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-830")>]
let ``SortByField_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable (sprintf "{\"a\": %s, \"e\": []}" sbfItems)
        (fun a -> sprintf "SortByField(%s, \"k\", \"asc\")" a) (Some "SortByField(Input.e, \"k\", \"asc\")")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-831")>]
let ``SortByField_HasEffectPure`` () = assertPure "SortByField" "SortByField([1, 2], \"k\", \"asc\")"

// =============================================================================
// 92.22 GroupBy
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-833")>]
let ``GroupBy_TakesArrayOrNullAndFieldName_ReturnsObject`` () =
    assertSignature "GroupBy" [ "items", false; "fieldName", false ]
    typesAre "object" [ "GroupBy([{k: \"a\"}], \"k\")"; "GroupBy(null, \"k\")" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-834")>]
let ``GroupBy_ReturnsNewObjectWithOnePropertyPerDistinctKey`` () =
    let o =
        out "var items = [{t: \"a\"}, {t: \"b\"}, {t: \"a\"}]\nvar g = GroupBy(items, \"t\")\nOutput.keys = Keys(g)\nvar h = GroupBy(items, \"t\")\ng.extra = 1\nOutput.h = Keys(h)"
    assertJson "[\"a\", \"b\"]" (Values.prop "keys" o)
    assertJson "[\"a\", \"b\"]" (Values.prop "h" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-835")>]
let ``JYRO-REQ-835 GroupBy_GroupKeyIsStringFormOfFieldValue`` () =
    let o =
        out "var one = Power(1, 0.5)\nvar g = GroupBy([{v: 1}, {v: 1.5}, {v: true}, {v: false}, {v: \"x\"}, {v: -2}, {v: 0.1 * one + 0.2 * one}, {v: one}, {v: 2.50}], \"v\")\nOutput.keys = Keys(g)\nOutput.one = Length(g[\"1\"])"
    assertJson "[\"1\", \"1.5\", \"true\", \"false\", \"x\", \"-2\", \"0.30000000000000004\", \"2.5\"]" (Values.prop "keys" o)
    assertJson "2" (Values.prop "one" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-836")>]
let ``GroupBy_NullOrAbsentFieldValue_GroupsUnderNull`` () =
    let o = out "var g = GroupBy([{v: null}, {}, {v: 1}], \"v\")\nOutput.n = Length(g[\"null\"])\nOutput.keys = Keys(g)"
    assertJson "2" (Values.prop "n" o)
    assertJson "[\"null\", \"1\"]" (Values.prop "keys" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-837")>]
let ``JYRO-REQ-837 GroupBy_ValuesWithSameStringFormShareOneGroup`` () =
    evalsTo ""
        [ "Length(GroupBy([{v: 1}, {v: \"1\"}], \"v\")[\"1\"])", "2"
          "Length(GroupBy([{v: null}, {v: \"null\"}, {}], \"v\")[\"null\"])", "3"
          "Length(GroupBy([{v: true}, {v: \"true\"}], \"v\")[\"true\"])", "2"
          "Length(Keys(GroupBy([{v: 1}, {v: \"1\"}, {v: 1.0}], \"v\")))", "1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-838")>]
let ``JYRO-REQ-838 GroupBy_WhenFieldValueIsArrayObjectOrFunction_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ "GroupBy([{v: [1]}], \"v\")"; "GroupBy([{v: []}], \"v\")"; "GroupBy([{v: {}}], \"v\")"; "GroupBy([{v: {a: 1}}], \"v\")"
          "GroupBy([{v: x => x}], \"v\")"; "GroupBy([{v: 1}, {v: [1]}], \"v\")"; "GroupBy([{v: [1, 2]}, {v: \"[1, 2]\"}], \"v\")" ]
    let r = runOnInput "{\"a\": [{\"v\": \"x\"}, {\"v\": {\"k\": 1}}]}" "Output.r = GroupBy(Input.a, \"v\")"
    Script.expectError MessageCode.ArgumentTypeMismatch r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-839")>]
let ``GroupBy_EachGroupIsNewArrayOfItsElementsInItemsOrder`` () =
    let o =
        out "var items = [{t: \"a\", i: 1}, {t: \"b\", i: 2}, {t: \"a\", i: 3}, {t: \"a\", i: 4}]\nvar g = GroupBy(items, \"t\")\nOutput.a = [g.a[0].i, g.a[1].i, g.a[2].i]\nOutput.b = [g.b[0].i]\nOutput.la = Length(g.a)\nOutput.lb = Length(g.b)\ng.a[0] = 0\nOutput.items0 = items[0].i\nOutput.n = Length(items)"
    assertJson "[1, 3, 4]" (Values.prop "a" o)
    assertJson "[2]" (Values.prop "b" o)
    assertJson "3" (Values.prop "la" o)
    assertJson "1" (Values.prop "lb" o)
    assertJson "1" (Values.prop "items0" o)
    assertJson "4" (Values.prop "n" o)
    let items = Values.json "[{\"t\": \"a\"}]"
    let res = Script.runWith { Script.defaults with Output = Some (Values.obj [ "items", items ]) } "Output.g = GroupBy(Output.items, \"t\")"
    let g = Values.prop "a" (Values.prop "g" (Script.output res))
    Assert.False(obj.ReferenceEquals(g, items), "a group is the items array itself")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-840")>]
let ``GroupBy_PropertiesFollowFirstElementOfEachGroup`` () =
    evalsTo ""
        [ "Keys(GroupBy([{t: \"b\"}, {t: \"a\"}, {t: \"b\"}, {t: \"c\"}], \"t\"))", "[\"b\", \"a\", \"c\"]"
          "Keys(GroupBy([{t: 2}, {t: 1}, {t: 10}], \"t\"))", "[\"2\", \"1\", \"10\"]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-841")>]
let ``GroupBy_OmitsNonObjectElements`` () =
    evalsTo ""
        [ "GroupBy([1, \"s\", null, [{t: \"a\"}], {t: \"a\"}, true], \"t\")", "{\"a\": [{\"t\": \"a\"}]}"
          "GroupBy([1, 2], \"t\")", "{}" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-842")>]
let ``GroupBy_OfEmptyOrNull_ReturnsEmptyObject`` () =
    evalsTo "" [ "GroupBy([], \"t\")", "{}"; "GroupBy(null, \"t\")", "{}" ]
    typesAre "object" [ "GroupBy([], \"t\")"; "GroupBy(null, \"t\")" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-843")>]
let ``GroupBy_FieldNameWithDotIsOneKey`` () =
    evalsTo "" [ "GroupBy([{\"a.b\": 1}, {a: {b: 1}}], \"a.b\")", "{\"1\": [{\"a.b\": 1}], \"null\": [{\"a\": {\"b\": 1}}]}" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-844")>]
let ``GroupBy_NullNumberOrBooleanFieldName_ReadsItsStringForm`` () =
    evalsTo ""
        [ "GroupBy([{\"\": \"x\"}], null)", "{\"x\": [{\"\": \"x\"}]}"
          "GroupBy([{\"1\": \"x\"}], 1)", "{\"x\": [{\"1\": \"x\"}]}"
          "GroupBy([{\"true\": \"x\"}], true)", "{\"x\": [{\"true\": \"x\"}]}"
          "GroupBy([{\"1.5\": \"x\"}], 1.5)", "{\"x\": [{\"1.5\": \"x\"}]}" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-845")>]
let ``GroupBy_WhenFieldNameIsArrayObjectOrFunction_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ for f in [ "[]"; "[\"k\"]"; "{}"; "x => x" ] -> sprintf "GroupBy([{k: 1}], %s)" f ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-846")>]
let ``GroupBy_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "GroupBy(%s, \"k\")" v) [ "GroupBy([{k: 1}], \"k\")"; "GroupBy(null, \"k\")" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-847")>]
let ``GroupBy_DoesNotChangeItems`` () =
    let o =
        out "var a = [{k: \"a\"}, {k: \"b\"}]\nvar s = Clone(a)\nvar g = GroupBy(a, \"k\")\nOutput.after = a == s\ng.a[0] = 1\ng.a[1] = 2\nOutput.same = a == s\nOutput.n = Length(a)"
    assertJson "true" (Values.prop "after" o)
    assertJson "true" (Values.prop "same" o)
    assertJson "2" (Values.prop "n" o)
    let o2 = outOnInput "{\"a\": [{\"k\": \"a\"}]}" "Output.g = GroupBy(Input.a, \"k\")"
    assertJson "{\"a\": [{\"k\": \"a\"}]}" (Values.prop "g" o2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-848")>]
let ``GroupBy_OnReadOnlyItems_ResultObjectAndGroupArraysAreWritable`` () =
    let o =
        outOnInput "{\"a\": [{\"k\": \"a\"}, {\"k\": \"b\"}]}"
            "var g = GroupBy(Input.a, \"k\")\ng.c = 1\ng.a[1] = 2\ng.b[0] = 3\nOutput.g = g"
    let g = Values.prop "g" o
    Assert.False(frozen g, "the result object is frozen")
    Assert.False(frozen (Values.prop "a" g), "group a is frozen")
    Assert.False(frozen (Values.prop "b" g), "group b is frozen")
    assertJson "{\"a\": [{\"k\": \"a\"}, 2], \"b\": [3], \"c\": 1}" g

[<Fact>]
[<Trait("requirement", "JYRO-REQ-849")>]
let ``GroupBy_GroupElementsAreTheElementsOfItemsNotCopies`` () =
    let o = out "var o = {k: \"a\", n: 1}\nvar r = GroupBy([o, {k: \"b\"}], \"k\")\nr.a[0].n = 2\nOutput.n = o.n"
    assertJson "2" (Values.prop "n" o)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-850")>]
let ``GroupBy_HasEffectPure`` () = assertPure "GroupBy" "GroupBy([{k: \"a\"}, 2], \"k\")"

// =============================================================================
// 92.23 SelectMany
// =============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-851")>]
let ``SelectMany_TakesArrayOrNullAndFieldName_ReturnsArray`` () =
    assertSignature "SelectMany" [ "items", false; "fieldName", false ]
    typesAre "array" [ "SelectMany([{t: [1]}], \"t\")"; "SelectMany(null, \"t\")" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-852")>]
let ``SelectMany_GivesElementsOfEachArrayPropertyInOrder`` () =
    evalsTo ""
        [ "SelectMany([{t: [1, 2]}, {t: [3]}], \"t\")", "[1, 2, 3]"
          "SelectMany([{t: []}, {t: [4]}], \"t\")", "[4]"
          "SelectMany([{t: [3]}, {t: [1, 2]}], \"t\")", "[3, 1, 2]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-853")>]
let ``SelectMany_AbsentOrNonArrayPropertyAddsNothing`` () =
    evalsTo "" [ "SelectMany([{t: 5}, {t: \"ab\"}, {t: {a: 1}}, {t: null}, {}, {t: true}, {t: [9]}], \"t\")", "[9]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-854")>]
let ``SelectMany_NonObjectElementAddsNothing`` () =
    evalsTo "" [ "SelectMany([1, \"s\", null, [[1]], true, {t: [2]}], \"t\")", "[2]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-855")>]
let ``SelectMany_KeepsArrayInsidePropertyWhole`` () =
    evalsTo "" [ "SelectMany([{t: [[1, 2], 3]}, {t: [[]]}], \"t\")", "[[1, 2], 3, []]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-856")>]
let ``SelectMany_FieldNameWithDotIsOneKey`` () =
    evalsTo "" [ "SelectMany([{\"a.b\": [1]}, {a: {b: [2]}}], \"a.b\")", "[1]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-857")>]
let ``SelectMany_NullNumberOrBooleanFieldName_ReadsItsStringForm`` () =
    evalsTo ""
        [ "SelectMany([{\"\": [1]}], null)", "[1]"; "SelectMany([{\"1\": [2]}], 1)", "[2]"
          "SelectMany([{\"true\": [3]}], true)", "[3]"; "SelectMany([{\"1.5\": [4]}], 1.5)", "[4]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-858")>]
let ``SelectMany_WhenFieldNameIsArrayObjectOrFunction_RaisesArgumentTypeMismatch`` () =
    errorsWith MessageCode.ArgumentTypeMismatch ""
        [ for f in [ "[]"; "[\"t\"]"; "{}"; "x => x" ] -> sprintf "SelectMany([{t: [1]}], %s)" f ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-859")>]
let ``SelectMany_OfNullItems_ReturnsEmptyArray`` () = evalsTo "" [ "SelectMany(null, \"t\")", "[]" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-860")>]
let ``SelectMany_WhenItemsIsNotArrayOrNull_RaisesArgumentTypeMismatch`` () =
    typeMismatchFor (fun v -> sprintf "SelectMany(%s, \"t\")" v) [ "SelectMany([{t: [1]}], \"t\")"; "SelectMany(null, \"t\")" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-861")>]
let ``SelectMany_DoesNotChangeItemsOrPropertyArrays`` () =
    let o =
        out "var inner = [1, 2]\nvar a = [{t: inner}, {t: [3]}]\nvar s = Clone(a)\nvar r = SelectMany(a, \"t\")\nOutput.after = a == s\nr[0] = 99\nOutput.same = a == s\nOutput.inner = inner\nOutput.n = Length(inner)"
    assertJson "true" (Values.prop "after" o)
    assertJson "true" (Values.prop "same" o)
    assertJson "[1, 2]" (Values.prop "inner" o)
    assertJson "2" (Values.prop "n" o)
    let o2 = outOnInput "{\"a\": [{\"t\": [1]}, {\"t\": [2]}]}" "Output.r = SelectMany(Input.a, \"t\")"
    assertJson "[1, 2]" (Values.prop "r" o2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-862")>]
let ``SelectMany_ResultElementsAreTheElementsOfThePropertyArraysNotCopies`` () =
    let o =
        out "var inner = [{x: 1}, [2]]\nvar items = [{t: inner}]\nvar r = SelectMany(items, \"t\")\nr[0].x = 5\nOutput.x = inner[0].x\nr[1][0] = 7\nOutput.y = inner[1][0]\nvar o1 = {k: 1}\nvar o2 = {k: 2}\nvar s = SelectMany([{t: [o1]}, {t: [o2]}], \"t\")\ns[0].k = 10\ns[1].k = 20\nOutput.k = [o1.k, o2.k]"
    assertJson "5" (Values.prop "x" o)
    assertJson "7" (Values.prop "y" o)
    assertJson "[10, 20]" (Values.prop "k" o)
    // Through the host: the result's elements are the Input's own element instances.
    let input = Values.json "{\"a\": [{\"t\": [{\"x\": 1}, [2]]}]}"
    let res = Script.runInput input "Output.r = SelectMany(Input.a, \"t\")"
    let r = Values.asList (Values.prop "r" (Script.output res))
    let source = Values.asList (Values.prop "t" (Values.asList (Values.prop "a" input)).Head)
    Assert.True(obj.ReferenceEquals(r.[0], source.[0]), "element 0 is a copy")
    Assert.True(obj.ReferenceEquals(r.[1], source.[1]), "element 1 is a copy")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-863")>]
let ``SelectMany_OnReadOnlyItems_GivesResultWithoutCannotWriteToInput`` () =
    assertReadOnlyArgument "{\"a\": [{\"t\": [{\"k\": 1}, 2]}, {\"t\": [[3]]}, 5]}" (fun a -> sprintf "SelectMany(%s, \"t\")" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-864")>]
let ``SelectMany_OnReadOnlyItems_ResultIsWritable`` () =
    assertResultWritable "{\"a\": [{\"t\": [{\"k\": 1}, 2]}, {\"t\": [[3]]}, 5], \"e\": []}"
        (fun a -> sprintf "SelectMany(%s, \"t\")" a) (Some "SelectMany(Input.e, \"t\")")
    // Also when exactly one element contributes its (read-only) array.
    assertResultWritable "{\"a\": [{\"t\": [1, 2]}]}" (fun a -> sprintf "SelectMany(%s, \"t\")" a) None

[<Fact>]
[<Trait("requirement", "JYRO-REQ-865")>]
let ``SelectMany_HasEffectPure`` () = assertPure "SelectMany" "SelectMany([1, 2], \"t\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-866")>]
let ``SelectMany_ReturnsNewArrayEvenWhenOneElementContributes`` () =
    let o =
        out "var inner = [1, 2]\nvar items = [{t: inner}]\nvar r = SelectMany(items, \"t\")\nr[0] = 9\nr[1] = 8\nOutput.one = inner\nvar inner2 = [1, 2]\nvar r2 = SelectMany([{t: []}, {t: inner2}, {u: 1}], \"t\")\nr2[0] = 9\nOutput.two = inner2\nvar p = [1]\nvar q = [2]\nvar r3 = SelectMany([{t: p}, {t: q}], \"t\")\nr3[0] = 0\nr3[1] = 0\nOutput.pq = [p, q]\nvar e1 = SelectMany([], \"t\")\ne1[0] = 1\nvar e2 = SelectMany(null, \"t\")\ne2[0] = 1\nOutput.e = [e1, e2]\nvar objs = [{t: [5]}]\nvar r4 = SelectMany(objs, \"t\")\nr4[0] = 1\nr4[1] = 2\nOutput.objs = objs"
    assertJson "[1, 2]" (Values.prop "one" o)
    assertJson "[1, 2]" (Values.prop "two" o)
    assertJson "[[1], [2]]" (Values.prop "pq" o)
    assertJson "[[1], [1]]" (Values.prop "e" o)
    assertJson "[{\"t\": [5]}]" (Values.prop "objs" o)
    // Through the host: the result is neither items nor the property array.
    let inner = Values.json "[1, 2]"
    let items = Values.arr [ Values.obj [ "t", inner ] ]
    let res = Script.runWith { Script.defaults with Output = Some (Values.obj [ "items", items ]) } "Output.r = SelectMany(Output.items, \"t\")"
    let r = Values.prop "r" (Script.output res)
    Assert.False(obj.ReferenceEquals(r, inner), "the result is the one contributing property array")
    Assert.False(obj.ReferenceEquals(r, items), "the result is items")

/// 130.6.5: a diagnostic names a Jyro type by an argument that TypeOf would give,
/// the expected type before the actual one, and never by a class name.
module RealTree.Jyro.Spec.Tests.S130_06_TypeNames

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.S160_03_TypedReads
open RealTree.Jyro.Spec.Tests.S160_03_Helpers

let private run (fn: IJyroFunction) (call: string) : JyroResult<JyroValue> =
    Script.runWith { pinned with Functions = [ fn ] } (sprintf "Output.r = %s" call)

/// Assert ArgumentTypeMismatch, then the type-name arguments of 130.6.5. The stage is
/// not asserted: 1494 is judged by its code and its arguments (JYRO-ADR-105 item 4).
let private names (expected: string) (actual: string) (r: JyroResult<JyroValue>) : unit =
    fails MessageCode.ArgumentTypeMismatch r
    assertTypeNameArgs expected actual (Script.onlyErrorMessage r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 GetOfJyroString_ForANumber_ArgsNameStringThenNumber`` () =
    run (TypedRead("string", 0, false)) "Read(1.5)" |> names "string" "number"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 GetOfJyroString_ForNull_ArgsNameStringThenNull`` () =
    run (TypedRead("string", 0, false)) "Read(null)" |> names "string" "null"

/// Each T of Get<T> (by the Jyro type it reads) and a value of each other type.
let private values = [ "null", "null"; "boolean", "true"; "number", "1"; "string", "\"s\""; "array", "[1]"; "object", "{a: 1}"; "function", "(x => x)" ]

let everyTypeAgainstEveryOther : obj[] seq =
    [ for kind in [ "string"; "number"; "boolean"; "array"; "object" ] do
        for actual, literal in values do
            if actual <> kind then yield [| box kind; box actual; box literal |] ]

[<Theory>]
[<MemberData(nameof everyTypeAgainstEveryOther)>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 Get_EveryTypeAgainstEveryOtherValue_ArgsAreTypeOfNames`` (kind: string, actual: string, literal: string) =
    run (TypedRead(kind, 0, false)) (sprintf "Read(%s)" literal) |> names kind actual

[<Theory>]
[<InlineData("boolean", "true")>]
[<InlineData("number", "1")>]
[<InlineData("string", "\"s\"")>]
[<InlineData("array", "[1]")>]
[<InlineData("object", "{a: 1}")>]
[<InlineData("function", "(x => x)")>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 GetOfTheNullType_ForAnotherValue_ArgsNameNullThenItsType`` (actual: string, literal: string) =
    run (TypedRead("null", 0, false)) (sprintf "Read(%s)" literal) |> names "null" actual

[<Theory>]
[<InlineData("string", "[1]", "string", "array")>]
[<InlineData("number", "\"a\"", "number", "string")>]
[<InlineData("number", "null", "number", "null")>]
[<InlineData("boolean", "1", "boolean", "number")>]
[<InlineData("array", "\"s\"", "array", "string")>]
[<InlineData("object", "[1]", "object", "array")>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 Helpers_ArgsNameExpectedThenActual`` (mode: string, literal: string, expected: string, actual: string) =
    run (Probe(mode)) (sprintf "Probe(%s)" literal) |> names expected actual

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 Get_OfAMissingArgument_ArgsNameStringThenNull`` () =
    run (TypedRead("string", 1, false)) "Read(1)" |> names "string" "null"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2056")>]
let ``JYRO-REQ-2056 NoArgument_IsAClassNameOfTheApi`` () =
    // Every row above already checks this; here the class names that 130.6.5 names are
    // confirmed to be in the set the check uses, so the check can fail.
    Assert.Contains("JyroString", apiTypeNames)
    Assert.Contains("JyroNumber", apiTypeNames)
    for kind, call in [ "number", "Read(\"x\")"; "array", "Read(1)"; "object", "Read(true)" ] do
        let d = run (TypedRead(kind, 0, false)) call |> Script.onlyErrorMessage
        for a in d.Args do
            Assert.False(apiTypeNames.Contains(string a), sprintf "argument %A is a class name" a)

// -- JYRO-REQ-1703 and JYRO-REQ-1704 (JYRO-ADR-164 items 1 and 2) ---------------------------------
// The expected type of an ArgumentTypeMismatch is the one TypeOf name the value is read as: never a
// list of names, a phrase with "or", or null for a parameter that also accepts null; a value read as
// its string form names string.

/// The seven names TypeOf gives (95.1.2).
let private typeOfNames = set [ "null"; "boolean"; "number"; "string"; "array"; "object"; "function" ]

let private typeWord =
    Text.RegularExpressions.Regex(@"\b(null|boolean|number|string|array|object|function)\b")

/// Exactly one error, ArgumentTypeMismatch, whose arguments hold exactly two TypeOf names, equal to
/// `expected` and then `actual`; no other argument names a type (so no list or "or" phrase sits
/// beside them), and no argument is a class name of the API. The stage is not asserted
/// (JYRO-ADR-105 item 4).
let private oneExpected (expected: string) (actual: string) (what: string) (r: JyroResult<JyroValue>) : unit =
    let d = Script.onlyErrorMessage r
    if d.Code <> MessageCode.ArgumentTypeMismatch then
        Values.fail (sprintf "%s: expected ArgumentTypeMismatch, got %A: %s" what d.Code d.Message)
    Assert.NotNull(d.Args)
    let args = d.Args |> Array.map (fun a -> if isNull a then "<null>" else Convert.ToString(a, Globalization.CultureInfo.InvariantCulture)) |> List.ofArray
    let names = args |> List.filter typeOfNames.Contains
    if names <> [ expected; actual ] then
        Values.fail (sprintf "%s: the TypeOf-name arguments are %A, not [%s; %s]; all arguments are %A" what names expected actual args)
    for a in args do
        if not (typeOfNames.Contains a) && typeWord.IsMatch a then
            Values.fail (sprintf "%s: the argument %A names a type beside the expected and actual ones; all arguments are %A" what a args)
    assertTypeNameArgs expected actual d

let private script (source: string) : JyroResult<JyroValue> = Script.runWith pinned source

let private onInput (json: string) (source: string) : JyroResult<JyroValue> =
    Script.runWith { pinned with Input = ValueInput(Values.obj [ "v", Values.json json ]) } source

/// The call with the value written in place, held in a variable, and (when JSON can carry it) read
/// from Input, each giving `expected` then `actual`.
let private everyWay (callOf: string -> string) (literal: string) (json: string option) (expected: string) (actual: string) : unit =
    let direct = sprintf "Output.r = %s" (callOf literal)
    script direct |> oneExpected expected actual direct
    let viaVar = sprintf "var v = %s\nOutput.r = %s" literal (callOf "v")
    script viaVar |> oneExpected expected actual viaVar
    match json with
    | Some j ->
        let viaInput = sprintf "Output.r = %s" (callOf "Input.v")
        onInput j viaInput |> oneExpected expected actual (sprintf "%s with Input.v = %s" viaInput j)
    | None -> ()

// -- JYRO-REQ-1703 ----------------------------------------------------------------------------------

[<Theory>]
[<InlineData("First(5)", "number")>]
[<InlineData("First(true)", "boolean")>]
[<Trait("requirement", "JYRO-REQ-1703")>]
let ``JYRO-REQ-1703 First_TheBookExamples_NameArrayThenTheActualType`` (call: string, actual: string) =
    let source = "Output.r = " + call
    script source |> oneExpected "array" actual source

[<Theory>]
[<InlineData("5", "5", "number")>]
[<InlineData("true", "true", "boolean")>]
[<InlineData("\"s\"", "\"s\"", "string")>]
[<InlineData("{a: 1}", "{\"a\": 1}", "object")>]
[<InlineData("(x => x)", null, "function")>]
[<Trait("requirement", "JYRO-REQ-1703")>]
let ``JYRO-REQ-1703 First_ItemsOfEachOtherType_NamesArrayNotNull_LiteralVariableAndInput`` (literal: string, json: string, actual: string) =
    everyWay (sprintf "First(%s)") literal (Option.ofObj json) "array" actual

/// Each "not an array and not null" items clause, with every other argument valid (90.3.8).
let notArrayNorNullCalls : obj[] seq =
    let calls =
        [ "Last", sprintf "Last(%s)"
          "Append", sprintf "Append(%s, 9)"
          "SortByField", sprintf "SortByField(%s, \"k\", \"asc\")"
          "GroupBy", sprintf "GroupBy(%s, \"k\")"
          "AnyByField", sprintf "AnyByField(%s, \"a\", \"==\", 1)"
          "AllByField", sprintf "AllByField(%s, \"a\", \"==\", 1)"
          "FindByField", sprintf "FindByField(%s, \"a\", \"==\", 1)"
          "WhereByField", sprintf "WhereByField(%s, \"a\", \"==\", 1)"
          "CountIf", sprintf "CountIf(%s, \"a\", \"==\", 1)"
          "Select", sprintf "Select(%s, \"a\")"
          "Project", sprintf "Project(%s, [\"a\"])"
          "Omit", sprintf "Omit(%s, [\"a\"])" ]
    [ for name, callOf in calls do
        for literal, actual in [ "5", "number"; "true", "boolean" ] do
            yield [| box name; box (callOf literal); box actual |] ]

[<Theory>]
[<MemberData(nameof notArrayNorNullCalls)>]
[<Trait("requirement", "JYRO-REQ-1703")>]
let ``JYRO-REQ-1703 ItemsNotArrayNorNull_InEveryClause_NamesArrayThenTheActualType`` (fn: string, call: string, actual: string) =
    let source = "Output.r = " + call
    script source |> oneExpected "array" actual (fn + ": " + source)

[<Theory>]
[<InlineData("PadLeft(\"a\", null)", "null")>]
[<InlineData("PadLeft(\"a\", \"3\")", "string")>]
[<Trait("requirement", "JYRO-REQ-1703")>]
let ``JYRO-REQ-1703 PadLeftLength_ReadAsANumber_NamesNumberThenTheActualType`` (call: string, actual: string) =
    let source = "Output.r = " + call
    script source |> oneExpected "number" actual source

// -- JYRO-REQ-1704 ----------------------------------------------------------------------------------

[<Theory>]
[<InlineData("ToUpper([1])", "array")>]
[<InlineData("GroupBy([{k: [1]}], \"k\")", "array")>]
[<InlineData("GroupBy([{k: {a: 1}}], \"k\")", "object")>]
[<InlineData("GroupBy([{k: x => x}], \"k\")", "function")>]
[<Trait("requirement", "JYRO-REQ-2126")>]
let ``JYRO-REQ-2126 TheBookExamples_NameStringThenTheActualType`` (call: string, actual: string) =
    let source = "Output.r = " + call
    script source |> oneExpected "string" actual source

[<Theory>]
[<InlineData("[1]", "[1]", "array")>]
[<InlineData("{a: 1}", "{\"a\": 1}", "object")>]
[<InlineData("(x => x)", null, "function")>]
[<Trait("requirement", "JYRO-REQ-2126")>]
let ``JYRO-REQ-2126 ToUpper_TextWithNoStringForm_NamesString_LiteralVariableAndInput`` (literal: string, json: string, actual: string) =
    everyWay (sprintf "ToUpper(%s)") literal (Option.ofObj json) "string" actual

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2126")>]
let ``JYRO-REQ-2126 GroupBy_FieldValueAfterNumberBooleanAndNullFieldValues_NamesStringThenArray`` () =
    let source = "Output.r = GroupBy([{k: 1}, {k: true}, {k: null}, {k: [1]}], \"k\")"
    script source |> oneExpected "string" "array" source
    let viaInput = "Output.r = GroupBy(Input.v, \"k\")"
    onInput "[{\"k\": 1}, {\"k\": true}, {\"k\": null}, {\"k\": {\"a\": 1}}]" viaInput
    |> oneExpected "string" "object" viaInput

[<Theory>]
[<InlineData("ToLower([1])", "array")>]
[<InlineData("ToLower({a: 1})", "object")>]
[<InlineData("ToLower(x => x)", "function")>]
[<InlineData("Trim([1])", "array")>]
[<InlineData("Trim({a: 1})", "object")>]
[<InlineData("PadLeft(\"a\", 3, [\"x\"])", "array")>]
[<InlineData("PadRight(\"a\", 3, [\"x\"])", "array")>]
[<InlineData("GroupBy([{k: 1}], [1])", "array")>]
[<InlineData("GroupBy([{k: 1}], {a: 1})", "object")>]
[<InlineData("GroupBy([{k: 1}], x => x)", "function")>]
[<Trait("requirement", "JYRO-REQ-2126")>]
let ``JYRO-REQ-2126 OtherStringFormParameters_NameStringThenTheActualType`` (call: string, actual: string) =
    let source = "Output.r = " + call
    script source |> oneExpected "string" actual source

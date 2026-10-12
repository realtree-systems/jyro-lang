/// 97.1: the field test and the argument rules shared by AnyByField, AllByField,
/// FindByField, WhereByField and CountIf. Each rule is a check over one function;
/// the 97.1.x theories run it over all five (rule 4.1.8), and the 97.2.2, 97.3.2,
/// 97.4.2 and 97.8.2 theories run every rule over their own function.
module RealTree.Jyro.Spec.Tests.S97_01_FieldTest

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- Observing the field test -------------------------------------------------

/// Whether a one-element result says the element passed.
let private passed (fn: string) (v: JyroValue) (context: string) : bool =
    match fn with
    | "AnyByField"
    | "AllByField" -> Values.asBool v
    | "FindByField" -> v.ValueType <> JyroValueType.Null
    | "WhereByField" -> (Values.asList v).Length = 1
    | "CountIf" -> Values.asFloat v = 1.0
    | _ -> fail (sprintf "not a field-test function: %s (%s)" fn context)

/// Run `fn([e], field, op, value)` with `e` the element `elem` (Jyro source),
/// assert success, and say whether `e` passed the field test.
let passesWith (options: RunOptions) (fn: string) (prelude: string) (elem: string) (field: string) (op: string) (value: string) : bool =
    let source =
        sprintf "%s\nvar e = %s\nOutput.r = %s([e], %s, %s, %s)" prelude elem fn field op value
    passed fn (runR options source) source

let passes fn prelude elem field op value = passesWith Script.defaults fn prelude elem field op value

let private assertPasses (expected: bool) fn prelude elem field op value =
    let got = passes fn prelude elem field op value
    check (got = expected)
        (sprintf "%s over [%s] with fieldName %s, operator %s, value %s: expected the element to %s"
            fn elem field op value (if expected then "pass" else "fail"))

let private call (fn: string) (items: string) (field: string) (op: string) (value: string) =
    sprintf "Output.r = %s(%s, %s, %s, %s)" fn items field op value

/// The result of a query function for an empty `items`.
let emptyResult (fn: string) : string =
    match fn with
    | "AnyByField" -> "false"
    | "AllByField" -> "true"
    | "FindByField" -> "null"
    | "CountIf" -> "0"
    | _ -> "[]"

/// A call of a query function with `items` and otherwise valid arguments.
let queryCall (fn: string) (items: string) : string =
    match fn with
    | "Select" -> sprintf "Select(%s, \"a\")" items
    | "Project"
    | "Omit" -> sprintf "%s(%s, [\"a\"])" fn items
    | _ -> sprintf "%s(%s, \"a\", \"==\", 1)" fn items

// -- The rules, each over one function ----------------------------------------------

let private nearMisses =
    [ "="; "==="; "<>"; "=>"; "=<"; " =="; "== "; "eq"; "EQ"; ""; "!"; "lt"; "!=="; "<<"
      "\\uFF1D\\uFF1D" // full-width equals signs
      "\u2264" ] // a single less-than-or-equal sign

/// 97.1.3: exactly the six operator strings, compared by code unit.
let rule_1_3 (fn: string) =
    for op in ops do
        let source = call fn "[{a: 1}, {a: 2}]" (q "a") (q op) "1"
        let result = Script.run source
        check result.IsSuccess (sprintf "%s rejected the operator %s:\n%s" fn op (Script.describe result))
    for bad in nearMisses do
        expectErr MessageCode.UnsupportedComparisonOperator (call fn "[{a: 1}, {a: 2}]" (q "a") (q bad) "1")

/// 97.1.4: another string raises UnsupportedComparisonOperator, before the first element.
let rule_1_4 (fn: string) =
    for bad in [ "~"; "="; "==="; "<>"; "=<"; " =="; ""; "eq" ] do
        for items in [ "[{a: 1}]"; "[]"; "null" ] do
            expectErr MessageCode.UnsupportedComparisonOperator (call fn items (q "a") (q bad) "1")
    // Elements whose comparison would itself raise under a relational operator.
    for items in [ "[{a: (x => x)}]"; "[{a: \"s\"}]"; "[{a: [1]}, {a: true}]" ] do
        expectErr MessageCode.UnsupportedComparisonOperator (call fn items (q "a") (q "~") "1")

/// 97.1.5: == and != agree with the script's == (30.10), property on the left.
let rule_1_5 (fn: string) =
    let prelude = "var f = x => x\nvar g = x => x"
    let props = [ "1"; "Power(1, 0.5)"; "\"1\""; "true"; "null"; "[1, 2]"; "{x: 1}"; "f" ]
    let values = [ "1"; "\"1\""; "[1, 2]"; "{x: 1}"; "null"; "true"; "f"; "g"; "Power(1, 0.5)" ]
    for p in props do
        for v in values do
            for op in [ "=="; "!=" ] do
                let elem = sprintf "{a: %s}" p
                let oracle =
                    evalR (sprintf "%s\nvar e = %s\nOutput.r = e.a %s %s" prelude elem op v) |> Values.asBool
                assertPasses oracle fn prelude elem (q "a") (q op) v
    // Anchors that do not depend on the script's own ==.
    assertPasses false fn prelude "{a: 1}" (q "a") (q "==") "\"1\""
    assertPasses true fn prelude "{a: 1}" (q "a") (q "!=") "\"1\""
    assertPasses true fn prelude "{a: [1, 2]}" (q "a") (q "==") "[1, 2]"
    assertPasses true fn prelude "{a: {x: 1}}" (q "a") (q "==") "{x: 1}"
    assertPasses true fn prelude "{a: f}" (q "a") (q "==") "f"
    assertPasses false fn prelude "{a: f}" (q "a") (q "==") "g"
    assertPasses true fn prelude "{a: f}" (q "a") (q "!=") "g"
    assertPasses true fn prelude "{a: null}" (q "a") (q "==") "null"
    assertPasses true fn prelude "{a: 1}" (q "a") (q "==") "Power(1, 0.5)"

let private relHolds (op: string) (sign: int) =
    match op with
    | "<" -> sign < 0
    | "<=" -> sign <= 0
    | ">" -> sign > 0
    | ">=" -> sign >= 0
    | _ -> failwith op

/// 97.1.6: < <= > >= give what the operator gives (30.11), property on the left.
let rule_1_6 (fn: string) =
    // (property, value, sign of property compared with value)
    let cases =
        [ "1", "2", -1
          "2", "2", 0
          "3", "2", 1
          "\"B\"", "\"a\"", -1
          "\"a\"", "\"ab\"", -1
          "\"ab\"", "\"a\"", 1
          "\"b\"", "\"b\"", 0
          "\"\\uD83D\\uDE00\"", "\"\\uFFFF\"", -1 // UTF-16 code units, not code points
          "Power(1, 0.5)", "1", 0 // inexact 1 against exact 1: by double value
          "1.0000000000000000000000000001", "1", 1 // two exact numbers: by exact value
          "1.0000000000000000000000000001", "Power(1, 0.5)", 0 ] // one inexact: by double value
    for (p, v, sign) in cases do
        for op in relOps do
            let elem = sprintf "{a: %s}" p
            let expected = relHolds op sign
            assertPasses expected fn "" elem (q "a") (q op) v

/// 97.1.7: a relational test of operands that are not two numbers or two strings raises IncomparableTypes.
let rule_1_7 (fn: string) =
    let prelude = "var f = x => x"
    let pairs =
        [ "1", "\"1\""; "\"1\"", "1"; "true", "false"; "[1]", "[1]"; "{x: 1}", "{x: 1}"
          "1", "null"; "\"a\"", "true"; "f", "f"; "[1]", "1" ]
    for (p, v) in pairs do
        for op in relOps do
            let source = sprintf "%s\n%s" prelude (call fn (sprintf "[{a: %s}]" p) (q "a") (q op) v)
            expectErr MessageCode.IncomparableTypes source
    expectErr MessageCode.IncomparableTypes (call fn "[{active: true}]" (q "active") (q ">") "false")

/// 97.1.8: a null property value fails a relational test, without an error.
let rule_1_8 (fn: string) =
    for elem in [ "{a: null}"; "{b: 1}" ] do
        for v in [ "1"; "\"x\""; "null"; "true"; "[1]"; "{}" ] do
            for op in relOps do
                assertPasses false fn "" elem (q "a") (q op) v

/// 97.1.9: fieldName is one key, never a path.
let rule_1_9 (fn: string) =
    assertPasses true fn "" "{\"a.b\": 1, a: {b: 2}}" (q "a.b") (q "==") "1"
    assertPasses false fn "" "{\"a.b\": 1, a: {b: 2}}" (q "a.b") (q "==") "2"
    assertPasses false fn "" "{a: {b: 2}}" (q "a.b") (q "==") "2"
    assertPasses true fn "" "{a: {b: 2}}" (q "a.b") (q "==") "null"
    assertPasses false fn "" "{A: 1}" (q "a") (q "==") "1"

/// 97.1.10: an absent property is read as null.
let rule_1_10 (fn: string) =
    assertPasses true fn "" "{b: 1}" (q "a") (q "==") "null"
    assertPasses false fn "" "{b: 1}" (q "a") (q "!=") "null"
    assertPasses false fn "" "{b: 1}" (q "a") (q "==") "1"
    assertPasses true fn "" "{b: 1}" (q "a") (q "!=") "1"
    assertPasses true fn "" "{}" (q "a") (q "==") "null"

/// 97.1.11: an element that is not an object fails, under every operator and value.
let rule_1_11 (fn: string) =
    for elem in [ "null"; "1"; "\"a\""; "true"; "[1]"; "(x => x)"; "\"\"" ] do
        for op in ops do
            for v in [ "null"; "1"; "\"a\"" ] do
                assertPasses false fn "" elem (q "a") (q op) v
    // A null element must not pass "== null" while the object after it does.
    let expected =
        match fn with
        | "AnyByField" -> "true"
        | "AllByField" -> "false"
        | "FindByField" -> "{\"k\": 1}"
        | "WhereByField" -> "[{\"k\": 1}]"
        | _ -> "1"
    let e = sprintf "%s([null, 5, {k: 1}], \"a\", \"==\", null)" fn
    assertExpr expected e

/// 97.1.12: a null fieldName is the empty string.
let rule_1_12 (fn: string) =
    assertPasses true fn "" "{\"\": 5}" "null" (q "==") "5"
    assertPasses false fn "" "{a: 5}" "null" (q "==") "5"
    assertPasses false fn "" "{\"null\": 5}" "null" (q "==") "5"

/// 97.1.13: a number or boolean fieldName is its string form.
let rule_1_13 (fn: string) =
    let cases =
        [ "{\"1\": 7}", "1", true
          "{\"1.5\": 7}", "1.5", true
          "{\"10\": 7}", "10.0", true
          "{\"10.0\": 7}", "10.0", false
          "{\"-2\": 7}", "-2", true
          "{\"0\": 7}", "0", true
          "{\"true\": 7}", "true", true
          "{\"false\": 7}", "false", true
          "{\"True\": 7}", "true", false ]
    for (elem, field, expected) in cases do
        assertPasses expected fn "" elem field (q "==") "7"
    // An inexact number: the ECMAScript form (40.4.7).
    let input = Values.obj [ "k", Values.inexact (0.1 + 0.2) ]
    let options = { Script.defaults with Input = ValueInput input }
    let got = passesWith options fn "" "{\"0.30000000000000004\": 7}" "Input.k" (q "==") "7"
    check got (sprintf "%s: an inexact fieldName 0.1+0.2 did not read the key \"0.30000000000000004\"" fn)

/// 97.1.14: a container or function fieldName or operator raises ArgumentTypeMismatch.
let rule_1_14 (fn: string) =
    let bad = [ "[1]"; "{}"; "(x => x)"; "[\"a\"]" ]
    for items in [ "[{a: 1}]"; "[]"; "null" ] do
        for b in bad do
            expectErr MessageCode.ArgumentTypeMismatch (call fn items b (q "==") "1")
        for b in [ "[\"==\"]"; "{}"; "(x => x)" ] do
            expectErr MessageCode.ArgumentTypeMismatch (call fn items (q "a") b "1")
    // fieldName is checked before operator (90.3.8).
    expectErr MessageCode.ArgumentTypeMismatch (call fn "[]" "[1]" (q "~") "1")
    expectErr MessageCode.ArgumentTypeMismatch (call fn "[{a: 1}]" "{}" "null" "1")

/// 97.1.15: a null operator is the empty string, so UnsupportedComparisonOperator.
let rule_1_15 (fn: string) =
    for items in [ "[{a: 1}]"; "[]"; "null" ] do
        expectErr MessageCode.UnsupportedComparisonOperator (call fn items (q "a") "null" "1")

/// 97.1.16: a null items is an empty array (for all eight query functions).
let rule_1_16 (fn: string) =
    let fromNull = expr (queryCall fn "null")
    let fromEmpty = expr (queryCall fn "[]")
    assertJson (emptyResult fn) fromNull (queryCall fn "null")
    assertJson (Values.toJson fromEmpty) fromNull (sprintf "%s: null items differs from []" fn)

/// 97.1.17: items that is not an array and not null raises ArgumentTypeMismatch.
let rule_1_17 (fn: string) =
    for items in [ "{a: 1}"; "{}"; "1"; "\"abc\""; "true"; "(x => x)"; "0"; "false"; "\"\"" ] do
        expectErr MessageCode.ArgumentTypeMismatch ("Output.r = " + queryCall fn items)

/// 97.1.22: a number or boolean operator is its string form, so UnsupportedComparisonOperator.
let rule_1_22 (fn: string) =
    for op in [ "5"; "0"; "-1"; "1.5"; "1e21"; "true"; "false" ] do
        for items in [ "[{a: 1}]"; "[]"; "null" ] do
            expectErr MessageCode.UnsupportedComparisonOperator (call fn items (q "a") op "1")

let checkRule (rule: string) (fn: string) =
    match rule with
    | "97.1.3" -> rule_1_3 fn
    | "97.1.4" -> rule_1_4 fn
    | "97.1.5" -> rule_1_5 fn
    | "97.1.6" -> rule_1_6 fn
    | "97.1.7" -> rule_1_7 fn
    | "97.1.8" -> rule_1_8 fn
    | "97.1.9" -> rule_1_9 fn
    | "97.1.10" -> rule_1_10 fn
    | "97.1.11" -> rule_1_11 fn
    | "97.1.12" -> rule_1_12 fn
    | "97.1.13" -> rule_1_13 fn
    | "97.1.14" -> rule_1_14 fn
    | "97.1.15" -> rule_1_15 fn
    | "97.1.16" -> rule_1_16 fn
    | "97.1.17" -> rule_1_17 fn
    | _ -> failwithf "no rule %s" rule

// -- 97.1.x over all five functions ------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1268")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_AcceptsExactlyTheSixOperators_AndRejectsNearMisses`` (fn: string) = rule_1_3 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1269")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``JYRO-REQ-1269 FieldTest_WhenOperatorUnknown_RaisesUnsupportedComparisonOperatorBeforeAnyElement_AlsoForEmptyAndNullItems`` (fn: string) =
    rule_1_4 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1270")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_EqualityOperators_GiveTheResultOfScriptEquality_PropertyOnTheLeft`` (fn: string) = rule_1_5 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1271")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_RelationalOperators_GiveTheResultOfTheOperator_PropertyOnTheLeft`` (fn: string) = rule_1_6 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1272")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_RelationalOnIncomparableOperands_RaisesIncomparableTypes`` (fn: string) = rule_1_7 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1273")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_RelationalOnNullProperty_FailsWithoutError`` (fn: string) = rule_1_8 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1274")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_FieldNameWithDot_NamesOneKeyNotAPath`` (fn: string) = rule_1_9 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1275")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_WhenPropertyAbsent_UsesNull`` (fn: string) = rule_1_10 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1276")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_NonObjectElement_FailsUnderEveryOperatorAndValue`` (fn: string) = rule_1_11 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1277")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_NullFieldName_ReadsTheEmptyStringKey`` (fn: string) = rule_1_12 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1278")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``JYRO-REQ-1278 FieldTest_NumberOrBooleanFieldName_ReadsItsStringForm`` (fn: string) = rule_1_13 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1279")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``FieldTest_ContainerOrFunctionFieldNameOrOperator_RaisesArgumentTypeMismatch`` (fn: string) = rule_1_14 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1280")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``JYRO-REQ-1280 FieldTest_NullOperator_RaisesUnsupportedComparisonOperator`` (fn: string) = rule_1_15 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1281")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
[<InlineData("Select")>] [<InlineData("Project")>] [<InlineData("Omit")>]
let ``Query_NullItems_IsTreatedAsEmptyArray`` (fn: string) = rule_1_16 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1282")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
[<InlineData("Select")>] [<InlineData("Project")>] [<InlineData("Omit")>]
let ``Query_ItemsNotArrayNorNull_RaisesArgumentTypeMismatch`` (fn: string) = rule_1_17 fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1287")>]
[<InlineData("AnyByField")>] [<InlineData("AllByField")>] [<InlineData("FindByField")>] [<InlineData("WhereByField")>] [<InlineData("CountIf")>]
let ``JYRO-REQ-1287 FieldTest_NumberOrBooleanOperator_RaisesUnsupportedComparisonOperator_NotArgumentTypeMismatch`` (fn: string) =
    rule_1_22 fn

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1287")>]
let ``JYRO-REQ-1287 AnyByField_BookExamplesOfNumberAndBooleanOperator_RaiseUnsupportedComparisonOperator`` () =
    expectErr MessageCode.UnsupportedComparisonOperator "Output.r = AnyByField([{a: 1}], \"a\", 5, 1)"
    expectErr MessageCode.UnsupportedComparisonOperator "Output.r = AnyByField([], \"a\", true, 1)"
    expectErr MessageCode.UnsupportedComparisonOperator "Output.r = CountIf(null, \"a\", 5, 1)"

// -- 97.2.2, 97.3.2, 97.4.2, 97.8.2: every rule of 97.1.3 to 97.1.17 over one function ---

let private rules =
    [ "97.1.3"; "97.1.4"; "97.1.5"; "97.1.6"; "97.1.7"; "97.1.8"; "97.1.9"; "97.1.10"
      "97.1.11"; "97.1.12"; "97.1.13"; "97.1.14"; "97.1.15"; "97.1.16"; "97.1.17" ]

let ruleData : obj[] seq = rules |> Seq.map (fun r -> [| box r |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1289")>]
[<MemberData(nameof ruleData)>]
let ``JYRO-REQ-1289 AllByField_AppliesTheFieldTestAndArgumentRule`` (rule: string) = checkRule rule "AllByField"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1296")>]
[<MemberData(nameof ruleData)>]
let ``JYRO-REQ-1296 FindByField_AppliesTheFieldTestAndArgumentRule`` (rule: string) = checkRule rule "FindByField"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1303")>]
[<MemberData(nameof ruleData)>]
let ``JYRO-REQ-1303 WhereByField_AppliesTheFieldTestAndArgumentRule`` (rule: string) = checkRule rule "WhereByField"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1340")>]
[<MemberData(nameof ruleData)>]
let ``JYRO-REQ-1340 CountIf_AppliesTheFieldTestAndArgumentRule`` (rule: string) = checkRule rule "CountIf"

/// 90.3.8 for these functions: items first, then fieldName, then operator.
let private checkArgumentOrder (fn: string) =
    // items wrong and operator unknown: items' error, not the operator's.
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Output.r = %s(5, \"a\", \"~\", 1)" fn)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Output.r = %s(\"s\", \"a\", null, 1)" fn)
    // fieldName wrong and operator unknown: fieldName's error.
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "Output.r = %s([{a: 1}], [1], \"~\", 1)" fn)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1289")>]
let ``JYRO-REQ-1289 AllByField_ChecksItemsThenFieldNameThenOperator`` () = checkArgumentOrder "AllByField"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1296")>]
let ``JYRO-REQ-1296 FindByField_ChecksItemsThenFieldNameThenOperator`` () = checkArgumentOrder "FindByField"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1303")>]
let ``JYRO-REQ-1303 WhereByField_ChecksItemsThenFieldNameThenOperator`` () = checkArgumentOrder "WhereByField"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1340")>]
let ``JYRO-REQ-1340 CountIf_ChecksItemsThenFieldNameThenOperator`` () = checkArgumentOrder "CountIf"

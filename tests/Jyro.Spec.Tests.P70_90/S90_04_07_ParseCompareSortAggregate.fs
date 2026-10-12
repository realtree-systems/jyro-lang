/// jyro-spec 90.4 (a failed parse), 90.5 (relational comparison in query
/// functions), 90.6 (sort order) and 90.7 (aggregation).
/// From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S90_04_07_ParseCompareSortAggregate

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let rows (xs: 'a list) : obj[] seq = xs |> Seq.map (fun x -> [| box x |])

/// Call `fn(Input.s)` with the host string `s`; assert the script goes on, the
/// result is null and there is no Error diagnostic.
let assertNullNoError (fn: string) (s: string) =
    let r = Script.runInput (Values.obj [ "s", Values.str s ]) (sprintf "Output.r = %s(Input.s)\nOutput.after = true" fn)
    Assert.True(r.IsSuccess, sprintf "%s(%A) raised:\n%s" fn s (Script.describe r))
    Assert.Empty(Script.errorCodes r)
    Assert.True((outProp "r" r).IsNull, sprintf "%s(%A) gave %s" fn s (Values.toJson (outProp "r" r)))
    Assert.True(Values.asBool (outProp "after" r))

// ===========================================================================
// 90.4 Reporting a failed parse
// ===========================================================================

/// Texts that no reading of a number interprets, and Infinity and NaN, which
/// name no Jyro number. The texts that only the JSON number grammar of 91.11
/// refuses ("", "1,234", "+5", ".5", "5.", "01", "0x10", "(5)", "5-") are
/// JYRO-REQ-486's and are tested under it: 90.4.1 says what ToNumber does with
/// text it cannot interpret, not which text that is.
let badNumbers () =
    rows [ "abc"; "$5"; "Infinity"; "NaN"; "1 2"; "--1"; "1e"; "e5"; "1.2.3"; "five" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-340")>]
[<MemberData(nameof badNumbers)>]
let ``JYRO-REQ-340 ToNumber_OfAStringItCannotInterpret_ReturnsNullWithoutError`` (s: string) = assertNullNoError "ToNumber" s

let badJson () =
    rows [ ""; "   "; "{"; "[1"; "nul"; "{a:1}"; "'x'"; "01"; "NaN"; "1 2"; "[1] x"; "\"unterminated"; "\"\\uD800\""; "1e400" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1658")>]
[<MemberData(nameof badJson)>]
let ``JYRO-REQ-1658 FromJson_OfAStringItCannotInterpret_ReturnsNullWithoutError`` (s: string) = assertNullNoError "FromJson" s

/// Texts that are not Base64. "/w==" decodes to the byte FF, which is not
/// UTF-8: that it gives null is JYRO-REQ-1164's rule, tested under it.
let badBase64 () = rows [ "abc"; "@@@@"; "ab-_"; "a=bc"; "=abc"; "YQ"; "YQ===" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-342")>]
[<MemberData(nameof badBase64)>]
let ``JYRO-REQ-342 Base64Decode_OfAStringItCannotInterpret_ReturnsNullWithoutError`` (s: string) = assertNullNoError "Base64Decode" s

let badDates () =
    rows
        [ ""; "not a date"; "2024-13-01"; "2024-02-30"; "2024-1-1"; "24-01-01"; "2024/01/01"; "2024-01-01T25:00:00Z"
          "2024-01-01T10:00:00+25:00"; "10000-01-01"; "2024-01-01Z" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-343")>]
[<MemberData(nameof badDates)>]
let ``JYRO-REQ-343 ParseDate_OfAStringItCannotInterpret_ReturnsNullWithoutError`` (s: string) = assertNullNoError "ParseDate" s

let nonStrings () : obj[] seq =
    [ for fn in [ "ToNumber"; "FromJson"; "Base64Decode"; "ParseDate" ] do
          for v in [ "null"; "0"; "1.5"; "true"; "false"; "[]"; "[\"1\"]"; "{}"; "{a: 1}"; "x => x" ] -> [| box fn; box v |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-344")>]
[<MemberData(nameof nonStrings)>]
let ``JYRO-REQ-344 ParseFunction_GivenANonString_RaisesArgumentTypeMismatch`` (fn: string, value: string) =
    Script.expectError MessageCode.ArgumentTypeMismatch (Script.run (sprintf "Output.r = %s(%s)" fn value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-344")>]
[<InlineData("ToNumber")>]
[<InlineData("FromJson")>]
[<InlineData("Base64Decode")>]
[<InlineData("ParseDate")>]
let ``JYRO-REQ-344 ParseFunction_GivenAString_DoesNotRaiseArgumentTypeMismatch`` (fn: string) =
    for s in [ "\"\""; "\"x\""; "\"1\""; "\"2024-01-02\""; "\"YQ==\"" ] do
        assertNoErrorCode MessageCode.ArgumentTypeMismatch (Script.run (sprintf "Output.r = %s(%s)" fn s))

// ===========================================================================
// 90.5 Relational comparison in query functions
// ===========================================================================

let private relationalOps = [ "<"; "<="; ">"; ">=" ]

let comparablePairs () : obj[] seq =
    [ for (l, r) in
          [ "1", "2"; "2", "2"; "2", "1"; "-1", "0"; "0.1", "0.10"; "1", "Power(1, 0.5)"; "Power(1, 0.5)", "1"
            "Power(2, 0.5)", "Power(3, 0.5)"; "1.0000000000000000000000000001", "1"; "1", "1.0000000000000000000000000001"
            "\"a\"", "\"b\""; "\"a\"", "\"a\""; "\"B\"", "\"a\""; "\"\"", "\"a\""
            "\"\U0001F600\"", "\"\uE000\""; "\"\uE000\"", "\"\U0001F600\"" ] do
          for op in relationalOps -> [| box l; box op; box r |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-345")>]
[<MemberData(nameof comparablePairs)>]
let ``QueryFunctions_WithARelationalOperator_GiveTheResultTheOperatorGives`` (left: string, op: string, right: string) =
    let items = "[{v: L}]"
    let source =
        lines [ sprintf "var L = %s" left
                sprintf "var R = %s" right
                sprintf "Output.e = L %s R" op
                sprintf "Output.any = AnyByField(%s, \"v\", \"%s\", R)" items op
                sprintf "Output.all = AllByField(%s, \"v\", \"%s\", R)" items op
                sprintf "Output.count = CountIf(%s, \"v\", \"%s\", R)" items op
                sprintf "Output.where = Length(WhereByField(%s, \"v\", \"%s\", R))" items op
                sprintf "Output.find = FindByField(%s, \"v\", \"%s\", R) != null" items op ]
    let r = Script.run source
    let e = Values.asBool (outProp "e" r)
    Assert.Equal(e, Values.asBool (outProp "any" r))
    Assert.Equal(e, Values.asBool (outProp "all" r))
    Assert.Equal((if e then 1.0 else 0.0), Values.asFloat (outProp "count" r))
    Assert.Equal((if e then 1.0 else 0.0), Values.asFloat (outProp "where" r))
    Assert.Equal(e, Values.asBool (outProp "find" r))

let private queryCalls (items: string) (op: string) (value: string) =
    [ sprintf "AnyByField(%s, \"v\", \"%s\", %s)" items op value
      sprintf "AllByField(%s, \"v\", \"%s\", %s)" items op value
      sprintf "FindByField(%s, \"v\", \"%s\", %s)" items op value
      sprintf "WhereByField(%s, \"v\", \"%s\", %s)" items op value
      sprintf "CountIf(%s, \"v\", \"%s\", %s)" items op value ]

let incomparablePairs () : obj[] seq =
    [ for (l, r) in
          [ "1", "\"a\""; "\"a\"", "1"; "true", "false"; "true", "1"; "[]", "[]"; "{}", "1"; "1", "null"
            "x => x", "x => x" ] do
          for op in relationalOps -> [| box l; box op; box r |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-346")>]
[<MemberData(nameof incomparablePairs)>]
let ``QueryFunctions_WhenTheOperatorWouldRaise_RaiseIncomparableTypes`` (left: string, op: string, right: string) =
    for call in queryCalls (sprintf "[{v: %s}]" left) op right do
        Script.expectError MessageCode.IncomparableTypes (Script.run ("Output.r = " + call))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-346")>]
[<InlineData("[{v: null}]", "1")>]
[<InlineData("[{v: null}]", "\"a\"")>]
[<InlineData("[{v: null}]", "null")>]
[<InlineData("[{}]", "1")>]
[<InlineData("[{}]", "\"a\"")>]
[<InlineData("[{}]", "null")>]
let ``QueryFunctions_WhenThePropertyValueIsNull_GiveFalseAndDoNotRaise`` (items: string, value: string) =
    for op in relationalOps do
        let source =
            lines [ sprintf "Output.any = AnyByField(%s, \"v\", \"%s\", %s)" items op value
                    sprintf "Output.all = AllByField(%s, \"v\", \"%s\", %s)" items op value
                    sprintf "Output.find = FindByField(%s, \"v\", \"%s\", %s)" items op value
                    sprintf "Output.where = Length(WhereByField(%s, \"v\", \"%s\", %s))" items op value
                    sprintf "Output.count = CountIf(%s, \"v\", \"%s\", %s)" items op value ]
        let r = Script.run source
        Assert.False(Values.asBool (outProp "any" r))
        Assert.False(Values.asBool (outProp "all" r))
        Assert.True((outProp "find" r).IsNull)
        Assert.Equal(0.0, Values.asFloat (outProp "where" r))
        Assert.Equal(0.0, Values.asFloat (outProp "count" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-346")>]
[<InlineData("1", "\"a\"")>]
[<InlineData("true", "1")>]
[<InlineData("[]", "{}")>]
let ``QueryFunctions_WithEqualityOperatorsOnIncomparableTypes_DoNotRaise`` (left: string, right: string) =
    for op in [ "=="; "!=" ] do
        for call in queryCalls (sprintf "[{v: %s}]" left) op right do
            let r = Script.run ("Output.r = " + call)
            Assert.True(r.IsSuccess, sprintf "%s: %s" call (Script.describe r))

// ===========================================================================
// 90.6 Sort order
// ===========================================================================

let private generousLimits =
    Some(Script.limits (fun o -> { o with MaxStatements = 10000000; MaxLoopIterations = 10000000 }))

/// The rank of a value's type in the sort order: null, boolean, number,
/// string, array, object, function value.
let rank (v: JyroValue) =
    match v.ValueType with
    | JyroValueType.Null -> 0
    | JyroValueType.Boolean -> 1
    | JyroValueType.Number -> 2
    | JyroValueType.String -> 3
    | JyroValueType.Array -> 4
    | JyroValueType.Object -> 5
    | _ -> 6

/// Two values are the same item: same type, and for numbers the same
/// representation and value, else equal (identity for function values, 30.10.9).
let sameItem (a: JyroValue) (b: JyroValue) =
    a.ValueType = b.ValueType
    && (match a, b with
        | (:? JyroNumber as x), (:? JyroNumber as y) ->
            x.IsExact = y.IsExact && (if x.IsExact then x.DecimalValue = y.DecimalValue else x.Value = y.Value)
        | _ -> a.EqualsValue b)

let private scrambled = "[{a: 1}, \"b\", fn, 3, [1], null, true, {}, 1.5, [], \"a\", false]"
let private sortedOrder = [ 5; 11; 6; 8; 3; 10; 1; 4; 9; 0; 7; 2 ] // indices into `scrambled`

[<Fact>]
[<Trait("requirement", "JYRO-REQ-347")>]
let ``JYRO-REQ-347 Sort_OfValuesOfEveryType_SucceedsAndOrdersByType`` () =
    let r = Script.run (lines [ "var fn = x => x"; "var items = " + scrambled; "Output.in = items"; "Output.s = Sort(items)" ])
    let input = Values.asList (outProp "in" r)
    let sorted = Values.asList (outProp "s" r)
    Assert.Equal(input.Length, sorted.Length)
    List.iteri (fun i idx -> Assert.True(sameItem input.[idx] sorted.[i], sprintf "position %d" i)) sortedOrder

[<Fact>]
[<Trait("requirement", "JYRO-REQ-347")>]
let ``JYRO-REQ-347 SortBy_OfValuesOfEveryType_SucceedsAndOrdersByType`` () =
    let r = Script.run (lines [ "var fn = x => x"; "var items = " + scrambled; "Output.in = items"; "Output.s = SortBy(items, x => x)" ])
    let input = Values.asList (outProp "in" r)
    let sorted = Values.asList (outProp "s" r)
    List.iteri (fun i idx -> Assert.True(sameItem input.[idx] sorted.[i], sprintf "position %d" i)) sortedOrder

[<Theory>]
[<Trait("requirement", "JYRO-REQ-347")>]
[<InlineData("asc")>]
[<InlineData("desc")>]
let ``JYRO-REQ-347 SortByField_OverFieldValuesOfEveryType_SucceedsAndOrdersByType`` (direction: string) =
    let r =
        Script.run (
            lines [ "var fn = x => x"
                    // Every element is an object with the field: what SortByField
                    // reads for an element without it, or for a non-object, is
                    // 92.21's (JYRO-REQ-815), not the sort order's.
                    "var items = Map(" + scrambled + ", v => {k: v})"
                    sprintf "Output.s = SortByField(items, \"k\", \"%s\")" direction ])
    let sorted = Values.asList (outProp "s" r)
    Assert.Equal(12, sorted.Length)
    let key (e: JyroValue) = rank (Values.prop "k" e)
    let ranks = sorted |> List.map key
    let expected = if direction = "asc" then List.sort ranks else List.sortDescending ranks
    Assert.Equal<int list>(expected, ranks)

let typeSamples = [ "null"; "true"; "1"; "\"s\""; "[1]"; "{a: 1}"; "fn" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-347")>]
let ``JYRO-REQ-347 SortFunctions_OnEveryPairOfDifferentTypes_DoNotRaiseAndOrderByType`` () =
    let pairs = [ for i in 0 .. 6 do for j in 0 .. 6 do if i <> j then yield i, j ]
    let source =
        lines [ yield "var fn = x => x"
                yield "Output.p = []"
                for (i, j) in pairs do
                    let a, b = typeSamples.[i], typeSamples.[j]
                    yield sprintf "Output.p = Append(Output.p, [Sort([%s, %s]), SortBy([%s, %s], x => x), SortByField([{k: %s}, {k: %s}], \"k\", \"asc\")])" a b a b a b ]
    let r = Script.run source
    let results = Values.asList (outProp "p" r)
    for ((i, j), triple) in List.zip pairs results do
        let lo, hi = min i j, max i j
        match Values.asList triple with
        | [ s; sb; sf ] ->
            Assert.Equal<int list>([ lo; hi ], Values.asList s |> List.map rank)
            Assert.Equal<int list>([ lo; hi ], Values.asList sb |> List.map rank)
            Assert.Equal<int list>([ lo; hi ], Values.asList sf |> List.map (fun e -> rank (Values.prop "k" e)))
        | _ -> Values.fail "unexpected shape"

/// Items `{k: key, id: i}` as a Jyro value, with keys from `keyOf`.
let keyed (n: int) (keyOf: int -> int) : JyroValue =
    Values.arr [ for i in 0 .. n - 1 -> Values.obj [ "k", Values.int (keyOf i); "id", Values.int i ] ]

let private patterns (n: int) : (string * (int -> int)) list =
    let rng = Random(1234)
    let random = Array.init n (fun _ -> rng.Next 3)
    [ "sorted", (fun i -> i * 3 / n); "reversed", (fun i -> 2 - i * 3 / n); "random", (fun i -> random.[i]) ]

let stabilityCases () : obj[] seq =
    [ for n in [ 2; 3; 17; 100; 10000 ] do
          for (p, _) in patterns n do
              for fn in [ "SortByField asc"; "SortByField desc"; "SortBy" ] -> [| box n; box p; box fn |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-348")>]
[<MemberData(nameof stabilityCases)>]
let ``SortByFieldAndSortBy_KeepTiedElementsInTheirInputOrder`` (n: int, pattern: string, fn: string) =
    let keyOf = patterns n |> List.pick (fun (p, k) -> if p = pattern then Some k else None)
    let call =
        match fn with
        | "SortByField asc" -> "SortByField(Input.items, \"k\", \"asc\")"
        | "SortByField desc" -> "SortByField(Input.items, \"k\", \"desc\")"
        | _ -> "SortBy(Input.items, x => x.k)"
    let r =
        Script.runWith { Script.defaults with Input = ValueInput(Values.obj [ "items", keyed n keyOf ]); Limits = generousLimits }
            ("Output.s = " + call)
    let sorted = Values.asList (outProp "s" r) |> List.map (fun e -> int (Values.asFloat (Values.prop "k" e)), int (Values.asFloat (Values.prop "id" e)))
    Assert.Equal(n, sorted.Length)
    let desc = fn = "SortByField desc"
    for ((k1, id1), (k2, id2)) in List.pairwise sorted do
        if k1 = k2 then Assert.True(id1 < id2, sprintf "tied elements %d and %d out of input order" id1 id2)
        elif desc then Assert.True(k1 > k2)
        else Assert.True(k1 < k2)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-348")>]
[<InlineData(2)>]
[<InlineData(3)>]
[<InlineData(17)>]
[<InlineData(100)>]
[<InlineData(10000)>]
let ``Sort_KeepsTiedArraysAndTiedObjectsInTheirInputOrder`` (n: int) =
    let items =
        Values.arr [ for i in 0 .. n - 1 ->
                         match i % 3 with
                         | 0 -> Values.obj [ "id", Values.int i ]
                         | 1 -> Values.arr [ Values.int i ]
                         | _ -> Values.int (i % 5) ]
    let r =
        Script.runWith { Script.defaults with Input = ValueInput(Values.obj [ "items", items ]); Limits = generousLimits }
            "Output.s = Sort(Input.items)"
    let sorted = Values.asList (outProp "s" r)
    Assert.Equal(n, sorted.Length)
    let objIds = sorted |> List.choose (fun e -> match e with :? JyroObject as o -> Some(Values.asFloat (o.GetProperty "id")) | _ -> None)
    let arrIds = sorted |> List.choose (fun e -> match e with :? JyroArray as a -> Some(Values.asFloat a.[0]) | _ -> None)
    Assert.Equal<float list>(List.sort objIds, objIds)
    Assert.Equal<float list>(List.sort arrIds, arrIds)

/// Cases for 90.6.3: the values (Jyro expressions) in input order, and the
/// input positions in the order the sort must give.
let orderCases () : obj[] seq =
    [ "{} | [1] | \"a\" | 1 | true | false | null | fn", [ 6; 5; 4; 3; 2; 1; 0; 7 ]
      "2 | -1 | 0.5", [ 1; 2; 0 ]
      "b | c | a", [ 2; 0; 1 ]
      "c | b | a", [ 2; 1; 0 ]
      "c | a", [ 1; 0 ]
      "\"b\" | \"a\" | \"B\"", [ 2; 1; 0 ]
      "\"\u00E9\" | \"z\"", [ 1; 0 ]
      "\"\uFFFF\" | \"\U0001F600\"", [ 1; 0 ]
      "[2] | [1]", [ 0; 1 ]
      "{b: 1} | {a: 1}", [ 0; 1 ]
      "h | g", [ 0; 1 ] ]
    |> Seq.map (fun (vs, order) -> [| box vs; box (order |> List.map string |> String.concat ",") |])

let private prelude =
    [ "var fn = x => x"; "var g = x => 1"; "var h = x => 2"
      "var a = 1"; "var b = 1.0000000000000000000000000001"; "var c = Power(1, 0.5)" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-349")>]
[<MemberData(nameof orderCases)>]
let ``JYRO-REQ-349 SortFunctions_UseTheTotalOrderOfTypesThenValues`` (values: string, order: string) =
    let vs = values.Split(" | ") |> List.ofArray
    let expected = order.Split(',') |> Array.map int |> List.ofArray
    let items = vs |> String.concat ", " |> sprintf "[%s]"
    let keyedItems = vs |> List.mapi (fun i v -> sprintf "{k: %s, id: %d}" v i) |> String.concat ", " |> sprintf "[%s]"
    let r =
        Script.run (
            lines (prelude @ [ "Output.in = " + items
                               "Output.s = Sort(" + items + ")"
                               "Output.f = SortByField(" + keyedItems + ", \"k\", \"asc\")"
                               "Output.b = SortBy(" + keyedItems + ", x => x.k)" ]))
    let input = Values.asList (outProp "in" r)
    let sorted = Values.asList (outProp "s" r)
    List.iteri (fun i idx -> Assert.True(sameItem input.[idx] sorted.[i], sprintf "Sort: position %d" i)) expected
    for name in [ "f"; "b" ] do
        let ids = Values.asList (outProp name r) |> List.map (fun e -> int (Values.asFloat (Values.prop "id" e)))
        Assert.Equal<int list>(expected, ids)

// ===========================================================================
// 90.7 Aggregation
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1823")>]
[<InlineData("[]")>]
[<InlineData("[null]")>]
[<InlineData("[null, null]")>]
[<InlineData("[null, null, null]")>]
let ``JYRO-REQ-1823 Sum_OfASequenceWithNoNumbers_IsExactZero`` (values: string) =
    let r = Script.run (sprintf "Output.r = Sum(%s)\nOutput.t = TypeOf(Output.r)" values)
    let v = outProp "r" r
    Assert.Equal("number", Values.asString (outProp "t" r))
    Assert.Equal(0m, Values.asDecimal v)
    Assert.True(Values.isExact v)

/// The exact 0 is the one that leaves 0.1 + 0.2 equal to 0.3 (30.2.4): an
/// inexact 0 would give false.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1823")>]
[<InlineData("[]")>]
[<InlineData("[null]")>]
[<InlineData("[null, null]")>]
let ``JYRO-REQ-1823 Sum_OfNoNumbers_PlusTenthsEqualsThreeTenths`` (values: string) =
    let r = Script.run (sprintf "Output.r = Sum(%s) + 0.1 + 0.2 == 0.3" values)
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(Values.asBool (outProp "r" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1823")>]
let ``JYRO-REQ-1823 Sum_OfOneNumber_IsThatNumber`` () =
    Assert.Equal(1.0, Values.asFloat (outProp "r" (Script.run "Output.r = Sum([1])")))

let aggregatesNoNumbers () : obj[] seq =
    [ for fn in [ "Min"; "Max"; "Average"; "Median"; "Mode" ] do
          for v in [ "[]"; "[null]"; "[null, null]" ] -> [| box fn; box v |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-351")>]
[<MemberData(nameof aggregatesNoNumbers)>]
let ``Aggregate_OfASequenceWithNoNumbers_IsNullWithoutError`` (fn: string, values: string) =
    let r = Script.run (sprintf "Output.r = %s(%s)" fn values)
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((outProp "r" r).IsNull)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-351")>]
[<InlineData("Min")>]
[<InlineData("Max")>]
[<InlineData("Average")>]
[<InlineData("Median")>]
[<InlineData("Mode")>]
let ``Aggregate_OfOneNumber_IsThatNumber`` (fn: string) =
    Assert.Equal(5.0, Values.asFloat (outProp "r" (Script.run (sprintf "Output.r = %s([5])" fn))))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1824")>]
[<InlineData("Sum([1, null, 2])", "3")>]
[<InlineData("Sum([null, 1, 2, null])", "3")>]
[<InlineData("Min([null, 5, 3])", "3")>]
[<InlineData("Min([5, 3, null])", "3")>]
[<InlineData("Max([4, null])", "4")>]
[<InlineData("Max([null, 4, 2])", "4")>]
[<InlineData("Average([1, null, 3])", "2")>]
[<InlineData("Average([null, 4])", "4")>]
[<InlineData("Average([null, 2, null, 4, null])", "3")>]
[<InlineData("Median([1, null, 3])", "2")>]
[<InlineData("Median([1, null, 2, 3])", "2")>]
[<InlineData("Median([null, 1, 2, 3, 4, null])", "2.5")>]
[<InlineData("Mode([null, null, 1])", "1")>]
[<InlineData("Mode([null, null, null, 2, 2, 1])", "2")>]
let ``JYRO-REQ-1824 Aggregate_IgnoresNullElementsInTheComputationAndTheCount`` (call: string, expected: string) =
    let v = outProp "r" (Script.run ("Output.r = " + call))
    Assert.True(v.EqualsValue(Values.json expected), sprintf "%s gave %s" call (Values.toJson v))

/// The statement itself: for each of the six functions, a sequence with `null`
/// elements gives what the same sequence with every `null` removed gives, with
/// the nulls at the start, in the middle, at the end and several together.
let nullSequences () : obj[] seq =
    [ for fn in [ "Sum"; "Average"; "Min"; "Max"; "Median"; "Mode" ] do
          for (withNulls, without) in
              [ "[null, 4, 1, 3]", "[4, 1, 3]"
                "[4, null, 1, 3]", "[4, 1, 3]"
                "[4, 1, 3, null]", "[4, 1, 3]"
                "[null, 4, null, 1, null, 3, null]", "[4, 1, 3]"
                "[null, 7, 7, 2, null]", "[7, 7, 2]"
                "[2.5, null, 1.5]", "[2.5, 1.5]"
                "[null, null, 7]", "[7]"
                "[null, 1, 2, 3, 4]", "[1, 2, 3, 4]" ] ->
              [| box fn; box withNulls; box without |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1824")>]
[<MemberData(nameof nullSequences)>]
let ``JYRO-REQ-1824 Aggregate_OfASequenceWithNulls_EqualsTheResultWithoutThem`` (fn: string, withNulls: string, without: string) =
    let r = Script.run (sprintf "Output.a = %s(%s)\nOutput.b = %s(%s)" fn withNulls fn without)
    Assert.True(r.IsSuccess, Script.describe r)
    let a = outProp "a" r
    let b = outProp "b" r
    Assert.True(a.EqualsValue(b), sprintf "%s(%s) gave %s, %s(%s) gave %s" fn withNulls (Values.toJson a) fn without (Values.toJson b))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1824")>]
[<InlineData("Average([1, null, 3])", "2")>]
[<InlineData("Median([4, null, 1, null, 3])", "3")>]
[<InlineData("Mode([null, null, 7])", "7")>]
let ``JYRO-REQ-1824 StatementExamples`` (call: string, expected: string) =
    let v = outProp "r" (Script.run ("Output.r = " + call))
    Assert.True(v.EqualsValue(Values.json expected), sprintf "%s gave %s" call (Values.toJson v))

let private aggregates = [ "Sum"; "Average"; "Min"; "Max"; "Median"; "Mode" ]

let wrongElements () : obj[] seq =
    [ for fn in aggregates do
          for bad in [ "\"a\""; "\"1\""; "true"; "false"; "[]"; "[1]"; "{}"; "x => x" ] do
              for (values, pos) in [ sprintf "[%s, 1, 2]" bad, 0; sprintf "[1, %s, 2]" bad, 1; sprintf "[1, 2, %s]" bad, 2 ] ->
                  [| box fn; box values; box pos |] ]

let private argAsInt (o: obj) : int64 option =
    match o with
    | :? JyroNumber as n when n.IsInteger -> Some(int64 n.Value)
    | :? int | :? int64 | :? int16 | :? byte | :? uint32 | :? uint64 -> Some(Convert.ToInt64 o)
    | :? decimal as d when Decimal.Truncate d = d -> Some(int64 d)
    | :? float as f when Math.Truncate f = f -> Some(int64 f)
    | _ -> None

let private argAsString (o: obj) : string option =
    match o with
    | :? string as s -> Some s
    | :? JyroString as s -> Some s.Value
    | _ -> None

let assertNumericElement (fn: string) (pos: int) (r: JyroResult<JyroValue>) =
    let m = Script.onlyErrorMessage r
    Assert.Equal(MessageCode.NumericElementRequired, m.Code)
    Assert.Equal(2, m.Args.Length)
    Assert.Equal(Some fn, argAsString m.Args.[0])
    Assert.Equal(Some(int64 pos), argAsInt m.Args.[1])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-353")>]
[<MemberData(nameof wrongElements)>]
let ``Aggregate_WithANonNumericElement_RaisesNumericElementRequiredNamingFunctionAndPosition`` (fn: string, values: string, pos: int) =
    assertNumericElement fn pos (Script.run (sprintf "Output.r = %s(%s)" fn values))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-353")>]
[<InlineData("Sum")>]
[<InlineData("Average")>]
[<InlineData("Min")>]
[<InlineData("Max")>]
[<InlineData("Median")>]
[<InlineData("Mode")>]
let ``Aggregate_WithSeveralWrongElementsAfterNulls_ReportsTheLowestPosition`` (fn: string) =
    assertNumericElement fn 2 (Script.run (sprintf "Output.r = %s([1, null, \"a\"])" fn))
    assertNumericElement fn 1 (Script.run (sprintf "Output.r = %s([1, \"a\", true])" fn))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-353")>]
let ``Sum_WithAWrongElementAndAnOverflow_RaisesNumericElementRequired`` () =
    assertNumericElement "Sum" 2 (Script.run "Output.r = Sum([1e308, 1e308, \"a\"])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-353")>]
[<InlineData("Sum")>]
[<InlineData("Average")>]
[<InlineData("Min")>]
[<InlineData("Max")>]
[<InlineData("Median")>]
[<InlineData("Mode")>]
let ``Aggregate_OfNumbersAndNulls_RaisesNothing`` (fn: string) =
    let r = Script.run (sprintf "Output.r = %s([1, null, 2.5, null])" fn)
    Assert.True(r.IsSuccess, Script.describe r)

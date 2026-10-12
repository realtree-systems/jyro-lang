/// Part 100.7: one maximum nesting depth for every structural walk of a value.
module RealTree.Jyro.Spec.Tests.S100_07_Nesting

open System
open System.Collections.Generic
open System.IO
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private n () = maxDepth ()

/// Script walks whose root is exactly the value built to depth k.
let private exactWalk (walk: string) (k: int) : string =
    match walk with
    | "equal" -> lines [ buildNested "a" k; buildNested "b" k; "Output.r = a == b" ]
    | "notequal" -> lines [ buildNested "a" k; buildNested "b" k; "Output.r = a != b" ]
    | "equal-object" -> lines [ buildNestedObj "a" k; buildNestedObj "b" k; "Output.r = a == b" ]
    | "tostring" -> lines [ buildNested "a" k; "Output.r = Length(ToString(a))" ]
    | "tojson" -> lines [ buildNested "a" k; "Output.r = Length(ToJson(a))" ]
    | "tojson-object" -> lines [ buildNestedObj "a" k; "Output.r = Length(ToJson(a))" ]
    | "fromjson" -> "Output.r = TypeOf(FromJson(Input.t))"
    | "clone" -> lines [ buildNested "a" k; "Output.r = TypeOf(Clone(a))" ]
    | "flatten" -> lines [ buildNested "a" k; "Output.r = Length(Flatten(a))" ]
    | "switch" -> lines [ buildNested "a" k; buildNested "b" k; "switch a do case b then Output.r = 1 end" ]
    | "validateschema" -> lines [ buildNested "a" k; "Output.r = TypeOf(ValidateSchema(a, {}))" ]
    | other -> failwithf "unknown walk %s" other

let private inputFor (walk: string) (k: int) : Input =
    if walk = "fromjson" then JsonInput(sprintf "{\"t\": \"%s\"}" (nestedArrayJson k)) else JsonInput "{}"

let exactWalks : obj[] seq =
    [ "equal"; "notequal"; "equal-object"; "tostring"; "tojson"; "tojson-object"; "fromjson"; "clone"
      "flatten"; "switch"; "validateschema" ]
    |> Seq.map (fun w -> [| box w |])

let private roomyRun (input: Input) (source: string) =
    Script.runWith { withLimits roomy with Input = input } source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2042")>]
let ``Maximum_IsAtLeast64`` () =
    Assert.True(n () >= 64, sprintf "the value nesting maximum is %d" (n ()))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2042")>]
let ``Maximum_IsDocumented`` () =
    // The public documentation shipped with the package (the XML documentation
    // file beside Jyro.Core) states the maximum on the public code that reports a breach.
    let asm = typeof<JyroValue>.Assembly.Location
    let xml = Path.ChangeExtension(asm, ".xml")
    Assert.True(File.Exists xml, "No XML documentation beside Jyro.Core")
    let doc = Xml.Linq.XDocument.Load xml
    let member' =
        doc.Descendants("member")
        |> Seq.find (fun m -> m.Attribute("name").Value = "F:RealTree.Jyro.MessageCode.ValueNestingLimitExceeded")
    Assert.Contains(string (n ()), member'.Value)

[<Theory>]
[<MemberData(nameof exactWalks)>]
[<Trait("requirement", "JYRO-REQ-2042")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``Walk_AtTheMaximum_Succeeds`` (walk: string) =
    roomyRun (inputFor walk (n ())) (exactWalk walk (n ())) |> ok |> ignore

[<Theory>]
[<MemberData(nameof exactWalks)>]
[<Trait("requirement", "JYRO-REQ-2042")>]
[<Trait("requirement", "JYRO-REQ-2045")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``Walk_OnePastTheMaximum_RaisesValueNestingLimitExceeded`` (walk: string) =
    roomyRun (inputFor walk (n () + 1)) (exactWalk walk (n () + 1)) |> fails MessageCode.ValueNestingLimitExceeded

// -- Diff walks from each compared value, not from its argument (JYRO-DEF-485) ---------------------
//
// Diff compares the values of the keys its two arguments share (JYRO-REQ-1205). By JYRO-REQ-2043's
// last sentence and JYRO-REQ-1705 the walk of that comparison starts at the two compared values,
// each depth 1, and the argument that holds them adds none. So the compared values are built k
// objects deep under the one shared key "k": at k = N each argument is N + 1 deep counted from the
// argument, yet the comparison is within the maximum; at k = N + 1 it is past it. `differ` puts
// another leaf at the bottom of b, so the comparison must reach the bottom either way.

/// Diff({k: a}, {k: b}) with a and b each `k` objects `{a: ...}` deep.
let private diffOfComparedValues (k: int) (differ: bool) : string =
    let build (name: string) (leaf: string) =
        sprintf "var %s = %s\nfor i_%s in 0 to %d do %s = {a: %s} end" name leaf name k name name
    lines [ build "a" "1"; build "b" (if differ then "2" else "1"); "Output.r = TypeOf(Diff({k: a}, {k: b}))" ]

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
[<Trait("requirement", "JYRO-REQ-2042")>]
[<Trait("requirement", "JYRO-REQ-2043")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
[<Trait("requirement", "JYRO-REQ-1705")>]
let ``JYRO-REQ-2043 JYRO-REQ-1705 Diff of compared values at the maximum succeeds although its arguments are one deeper`` (differ: bool) =
    roomyRun (JsonInput "{}") (diffOfComparedValues (n ()) differ) |> ok |> ignore

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
[<Trait("requirement", "JYRO-REQ-2042")>]
[<Trait("requirement", "JYRO-REQ-2043")>]
[<Trait("requirement", "JYRO-REQ-2045")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
[<Trait("requirement", "JYRO-REQ-1705")>]
let ``JYRO-REQ-2043 JYRO-REQ-2045 JYRO-REQ-1705 Diff of compared values one past the maximum raises ValueNestingLimitExceeded`` (differ: bool) =
    roomyRun (JsonInput "{}") (diffOfComparedValues (n () + 1) differ) |> fails MessageCode.ValueNestingLimitExceeded

/// Walks over a value held inside an argument, where the root of the walk may
/// be the argument or the element: the element is built at depth `e`. At
/// e = N - 1 both readings are within the maximum; at e = N + 1 both are past it.
let private innerWalk (walk: string) (e: int) : string =
    match walk with
    | "distinct" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = Length(Distinct([a, b]))" ]
    | "indexof" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = IndexOf([a], b)" ]
    | "anybyfield" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = AnyByField([{f: a}], \"f\", \"==\", b)" ]
    | "wherebyfield" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = Length(WhereByField([{f: a}], \"f\", \"==\", b))" ]
    | "countif" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = CountIf([{f: a}], \"f\", \"==\", b)" ]
    | "findbyfield" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = TypeOf(FindByField([{f: a}], \"f\", \"==\", b))" ]
    | "allbyfield" -> lines [ buildNested "a" e; buildNested "b" e; "Output.r = AllByField([{f: a}], \"f\", \"==\", b)" ]
    | other -> failwithf "unknown walk %s" other

let innerWalks : obj[] seq =
    // No GroupBy: it refuses an array field value (92.22.6), so it walks no nested value (JYRO-DEF-655).
    [ "distinct"; "indexof"; "anybyfield"; "wherebyfield"; "countif"; "findbyfield"; "allbyfield" ]
    |> Seq.map (fun w -> [| box w |])

[<Theory>]
[<MemberData(nameof innerWalks)>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``LibraryWalkOfAnArgument_WithinTheMaximum_Succeeds`` (walk: string) =
    roomyRun (JsonInput "{}") (innerWalk walk (n () - 1)) |> ok |> ignore

[<Theory>]
[<MemberData(nameof innerWalks)>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``LibraryWalkOfAnArgument_PastTheMaximum_RaisesValueNestingLimitExceeded`` (walk: string) =
    roomyRun (JsonInput "{}") (innerWalk walk (n () + 1)) |> fails MessageCode.ValueNestingLimitExceeded

// -- ValidateSchema walks its schema argument too (JYRO-ADR-103, JYRO-DEF-460) -------------------

/// A schema `{items: {items: ... {type: "string"}}}` nested `k` objects deep, and data of `k - 1`
/// nested arrays with `leaf` at the bottom, so the innermost schema applies to the leaf.
let private deepSchema (k: int) (leaf: string) : string =
    lines [ "var s = {type: \"string\"}"
            sprintf "for i_s in 0 to %d do s = {items: s} end" (k - 1)
            sprintf "var d = %s" leaf
            sprintf "for i_d in 0 to %d do d = [d] end" (k - 1)
            "Output.r = Length(ValidateSchema(d, s)) == 0" ]

[<Theory>]
[<InlineData("\"x\"", true)>]
[<InlineData("1", false)>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``JYRO-REQ-2047 ValidateSchema reads and applies a schema argument nested to the maximum`` (leaf: string, valid: bool) =
    let out = roomyRun (JsonInput "{}") (deepSchema (n ()) leaf) |> ok
    Assert.Equal(valid, Values.asBool (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``JYRO-REQ-2047 ValidateSchema of a schema argument one past the maximum raises ValueNestingLimitExceeded`` () =
    roomyRun (JsonInput "{}") (deepSchema (n () + 1) "\"x\"") |> fails MessageCode.ValueNestingLimitExceeded

// -- 100.7.3 / 100.7.8 A comparison in a library function walks from the compared values --------

/// Source lines binding `name` to `depth` nested arrays with `leaf` at the bottom.
let private buildNestedLeaf (name: string) (depth: int) (leaf: string) : string =
    sprintf "var %s = %s\nfor i_%s in 0 to %d do %s = [%s] end" name leaf name depth name name

/// Library comparisons of a and b, each built `depth` arrays deep; `differ`
/// puts another leaf at the bottom of b, so the comparison must reach it.
let private comparison (walk: string) (depth: int) (differ: bool) : string =
    let setup = lines [ buildNestedLeaf "a" depth "1"; buildNestedLeaf "b" depth (if differ then "2" else "1") ]
    let call =
        match walk with
        | "equal" -> "Output.r = a == b"
        | "distinct" -> "Output.r = Length(Distinct([a, b]))"
        | "indexof" -> "Output.r = IndexOf([a], b)"
        | "contains" -> "Output.r = Contains([a], b)"
        | "anybyfield" -> "Output.r = AnyByField([{k: a}], \"k\", \"==\", b)"
        | "allbyfield" -> "Output.r = AllByField([{k: a}], \"k\", \"==\", b)"
        | "findbyfield" -> "Output.r = TypeOf(FindByField([{k: a}], \"k\", \"==\", b))"
        | "wherebyfield" -> "Output.r = Length(WhereByField([{k: a}], \"k\", \"==\", b))"
        | "countif" -> "Output.r = CountIf([{k: a}], \"k\", \"==\", b)"
        | other -> failwithf "unknown comparison %s" other
    lines [ setup; call ]

// No GroupBy: it compares no nested value, refusing an array field value (92.22.6, JYRO-DEF-653).
let comparisons : obj[] seq =
    [ for w in [ "equal"; "distinct"; "indexof"; "contains"; "anybyfield"; "allbyfield"; "findbyfield"; "wherebyfield"; "countif" ] do
        for differ in [ false; true ] -> [| box w; box differ |] ]

[<Theory>]
[<MemberData(nameof comparisons)>]
[<Trait("requirement", "JYRO-REQ-1705")>]
let ``JYRO-REQ-1705 a library comparison of two values of depth N does not raise, as == does not`` (walk: string, differ: bool) =
    roomyRun (JsonInput "{}") (comparison walk (n ()) differ) |> ok |> ignore

[<Theory>]
[<MemberData(nameof comparisons)>]
[<Trait("requirement", "JYRO-REQ-1705")>]
let ``JYRO-REQ-1705 a library comparison of two values of depth N + 1 raises ValueNestingLimitExceeded, as == does`` (walk: string, differ: bool) =
    roomyRun (JsonInput "{}") (comparison walk (n () + 1) differ) |> fails MessageCode.ValueNestingLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1705")>]
let ``JYRO-REQ-1705 the book examples at depth N`` () =
    let src =
        lines [ buildNestedLeaf "a" (n ()) "1"
                buildNestedLeaf "b" (n ()) "1"
                "Output.d = Length(Distinct([a, b]))"
                "Output.i = IndexOf([a], b)"
                "Output.f = AnyByField([{k: a}], \"k\", \"==\", b)"
                "Output.e = a == b" ]
    let out = roomyRun (JsonInput "{}") src |> ok
    Assert.Equal(1.0, Values.asFloat (Values.prop "d" out))
    Assert.Equal(0.0, Values.asFloat (Values.prop "i" out))
    Assert.True(Values.asBool (Values.prop "f" out))
    Assert.True(Values.asBool (Values.prop "e" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2043")>]
let ``RootOfALibraryComparison_IsEachComparedValue`` () =
    // The array that holds a and b adds no depth (full cases under 100.7.8).
    roomyRun (JsonInput "{}") (comparison "distinct" (n ()) false) |> ok |> ignore
    roomyRun (JsonInput "{}") (comparison "distinct" (n () + 1) false) |> fails MessageCode.ValueNestingLimitExceeded

// -- 100.7.9 Merge does not walk the merged values ------------------------------------------------

let mergeCases : obj[] seq =
    [ "array", "Merge([{k: a}])"
      "array-two-elements", "Merge([{k: a}, {j: 1}])"
      "array-later-element", "Merge([{j: 1}, {k: a}])"
      "object", "Merge([{k: a}])" ]
    |> Seq.map (fun (l, c) -> [| box l; box c |])

[<Theory>]
[<MemberData(nameof mergeCases)>]
[<Trait("requirement", "JYRO-REQ-1437")>]
let ``Merge_OfAValuePastTheMaximum_DoesNotWalkIt_AndGivesTheValueItself`` (label: string, call: string) =
    let build = if label = "object" then buildNestedObj "a" (n () + 1) else buildNested "a" (n () + 1)
    let write = if label = "object" then "m.k.a = 7\nOutput.r = a.a" else "m.k[0] = 7\nOutput.r = a[0]"
    let src =
        lines [ build
                sprintf "var m = %s" call
                sprintf "Output.t = m.k is %s" (if label = "object" then "object" else "array")
                write ]
    let out = roomyRun (JsonInput "{}") src |> ok
    Assert.True(Values.asBool (Values.prop "t" out))
    // A write through m.k is seen through a: m.k is a itself.
    Assert.Equal(7.0, Values.asFloat (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1437")>]
let ``Merge_ThenAWalkOfTheResult_AppliesTheMaximumThen`` () =
    let src = lines [ buildNested "a" (n () + 1); "var m = Merge([{k: a}])"; "Output.r = Length(ToJson(m))" ]
    roomyRun (JsonInput "{}") src |> fails MessageCode.ValueNestingLimitExceeded

// -- 100.7.2 The host cannot move the maximum ------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1430")>]
let ``NoPublicMember_SetsTheNestingMaximum`` () =
    let assemblies =
        [ typeof<JyroValue>.Assembly; typeof<CompiledProgram>.Assembly
          Reflection.Assembly.Load "Jyro.Stdlib"; typeof<JyroCompiler>.Assembly ]
    let setters =
        [ for a in assemblies do
            for t in a.GetExportedTypes() do
                for pr in t.GetProperties() do
                    if pr.CanWrite && not (isNull (pr.GetSetMethod())) && (pr.Name.Contains "Nest" || (pr.Name.Contains "Depth" && not (pr.Name.Contains "Call"))) then
                        yield t.Name + "." + pr.Name
                for m in t.GetMethods() do
                    if (m.Name.StartsWith "With" || m.Name.StartsWith "Set") && (m.Name.Contains "Nest" || (m.Name.Contains "Depth" && not (m.Name.Contains "Call"))) then
                        yield t.Name + "." + m.Name ]
    Assert.True(setters.IsEmpty, sprintf "Members that could set the nesting maximum: %A" setters)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1430")>]
let ``Threshold_IsTheSameUnderEveryOptionSetting`` () =
    let settings =
        [ JyroExecutionOptions.Unlimited
          roomy JyroExecutionOptions.Default
          { roomy JyroExecutionOptions.Default with MaxCallDepth = 1 }
          { roomy JyroExecutionOptions.Default with MaxCallDepth = JyroExecutionOptions.MaxSafeCallDepth } ]
    for s in settings do
        let o = { Script.defaults with Limits = Some s }
        Script.runWith o (exactWalk "equal" (n ())) |> ok |> ignore
        Script.runWith o (exactWalk "equal" (n () + 1)) |> fails MessageCode.ValueNestingLimitExceeded

// -- 100.7.3 Depth counts containers on the path, root = 1; width adds nothing --------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2043")>]
let ``InputOfNestedArrays_DepthNWithScalarLeaf_Passes_NPlus1Fails`` () =
    Script.runWith { Script.defaults with Input = JsonInput(nestedArrayJson (n ())) } "Output.r = Length(ToJson(Input))" |> ok |> ignore
    let r = Script.runWith { Script.defaults with Input = JsonInput(nestedArrayJson (n () + 1)) } "Output.r = Length(ToJson(Input))"
    hasError MessageCode.ValueNestingLimitExceeded r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2043")>]
let ``AlternatingArraysAndObjects_CountEachContainer`` () =
    let build (k: int) =
        lines [ "var a = 1"
                sprintf "for i in 0 to %d do" k
                "  if i % 2 == 0 then a = [a] else a = {x: a} end"
                "end"
                "Output.r = Length(ToJson(a))" ]
    roomyRun (JsonInput "{}") (build (n ())) |> ok |> ignore
    roomyRun (JsonInput "{}") (build (n () + 1)) |> fails MessageCode.ValueNestingLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2043")>]
let ``EmptyContainerAtTheBottom_CountsAsALevel`` () =
    // N - 1 wraps around an empty array: depth N. N wraps: depth N + 1.
    let build (wraps: int) = sprintf "var a = []\nfor i in 0 to %d do a = [a] end\nOutput.r = Length(ToJson(a))" wraps
    roomyRun (JsonInput "{}") (build (n () - 1)) |> ok |> ignore
    roomyRun (JsonInput "{}") (build (n ())) |> fails MessageCode.ValueNestingLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2043")>]
let ``Width_DoesNotAddDepth`` () =
    let wideObject = "{" + String.Join(",", [ for i in 0 .. 99_999 -> sprintf "\"k%d\":%d" i i ]) + "}"
    let r = Script.runWith { withLimits roomy with Input = JsonInput wideObject } "Output.r = Length(ToJson(Input)) > 0"
    Assert.True(Values.asBool (Values.prop "r" (ok r)))
    let wideArray = "[" + String.Join(",", [ for i in 0 .. 99_999 -> string i ]) + "]"
    Script.runWith { withLimits roomy with Input = JsonInput wideArray } "Output.r = Length(ToJson(Input)) > 0" |> ok |> ignore
    // About 1,000 siblings at each of N levels, built in the script.
    let src =
        lines [ "var w = Range(0, 999)"
                "var a = 1"
                sprintf "for i in 0 to %d do a = Concatenate([a], w) end" (n ())
                "Output.r = Length(ToJson(a)) > 0"
                "Output.e = Clone(a) == a" ]
    Script.runWith (withLimits roomy) src |> ok |> ignore

// -- 100.7.4 A walk of two values counts each pair as one level ------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2044")>]
let ``EqualityOfTwoValuesAtDepthN_DoesNotCountDouble`` () =
    let r = roomyRun (JsonInput "{}") (exactWalk "equal" (n ()))
    Assert.True(Values.asBool (resultProp "r" r), Script.describe r)
    ok r |> ignore
    let r2 = roomyRun (JsonInput "{}") (exactWalk "notequal" (n ()))
    Assert.False(Values.asBool (resultProp "r" r2), Script.describe r2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2044")>]
let ``EqualityOfObjectsAtDepthN_DoesNotCountDouble`` () =
    let r = roomyRun (JsonInput "{}") (exactWalk "equal-object" (n ()))
    Assert.True(Values.asBool (resultProp "r" r), Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2044")>]
let ``PairWalkInALibraryFunction_DoesNotCountDouble`` () =
    // IndexOf compares b with the array's element: a pair walk of depth N - 1 at most.
    let r = roomyRun (JsonInput "{}") (innerWalk "indexof" (n () - 1))
    Assert.Equal(0m, Values.asDecimal (resultProp "r" r))
    Script.runWith (withLimits roomy) (exactWalk "switch" (n ())) |> ok |> ignore

// -- 100.7.5 The error comes before the walk goes deeper; cycles --------------------------------

[<Theory>]
[<InlineData("equal-array")>]
[<InlineData("notequal-array")>]
[<InlineData("equal-object")>]
[<InlineData("tostring-array")>]
[<InlineData("tostring-object")>]
[<InlineData("tojson-array")>]
[<InlineData("tojson-object")>]
[<InlineData("clone")>]
[<InlineData("flatten")>]
[<InlineData("distinct")>]
[<InlineData("indexof")>]
[<InlineData("diff")>]
[<InlineData("switch")>]
[<Trait("requirement", "JYRO-REQ-2045")>]
let ``CyclicValue_EveryWalk_RaisesValueNestingLimitExceeded_WithoutCrashOrHang`` (walk: string) =
    Child.run ("cycle|" + walk) |> Child.expectCode MessageCode.ValueNestingLimitExceeded

[<Theory>]
[<InlineData("ToJson")>]
[<InlineData("ToStringValue")>]
[<InlineData("ToObjectValue")>]
[<InlineData("WriteJson")>]
[<InlineData("ToDictionary")>]
[<InlineData("WithInputDictionary")>]
[<InlineData("WithInputList")>]
[<Trait("requirement", "JYRO-REQ-2045")>]
[<Trait("requirement", "JYRO-REQ-2046")>]
let ``HostBuiltCyclicValue_EveryHostWalk_FailsWithValueNestingLimitExceeded`` (op: string) =
    let o = Child.run ("hostcycle|" + op)
    Child.expectClean o
    if o.Ok || not (List.contains (string MessageCode.ValueNestingLimitExceeded) o.Codes) then
        Values.fail ("Expected a failure carrying ValueNestingLimitExceeded: " + o.Describe)

// -- 100.7.6 Host API calls with no execution fail through their own channel ---------------

/// Assert that `f` fails through its own channel with ValueNestingLimitExceeded:
/// an exception carrying the code, or a failed result carrying it.
let private hostFails (f: unit -> obj) : unit =
    let code = MessageCode.ValueNestingLimitExceeded
    let outcome = try Choice1Of2(f ()) with e -> Choice2Of2 e
    match outcome with
    | Choice2Of2 e ->
        if not (List.contains code (codesOfExn e)) then
            Values.fail (sprintf "Threw %s without %A: %s" (e.GetType().FullName) code e.Message)
    | Choice1Of2(:? JyroResult<JyroValue> as r) -> hasError code r
    | Choice1Of2 _ -> Values.fail (sprintf "The call returned normally; expected %A" code)

let private hostOk (f: unit -> obj) : unit =
    match f () with
    | :? JyroResult<JyroValue> as r -> Assert.True(r.IsSuccess, Script.describe r)
    | _ -> ()

let private nestedDict (depth: int) : IDictionary<string, obj> =
    let mutable d = Dictionary<string, obj>() :> IDictionary<string, obj>
    d.["a"] <- box 1
    for _ in 2 .. depth do
        let outer = Dictionary<string, obj>()
        outer.["a"] <- box d
        d <- outer
    d

let private nestedObjectValue (depth: int) : JyroValue =
    let mutable v : JyroValue = Values.int 1
    for _ in 1 .. depth do
        let o = JyroObject()
        o.SetProperty("a", v)
        v <- o
    v

let private compiled () = Script.compile "Output.r = 1" |> Script.valueOf

let hostCalls : obj[] seq =
    [ "FromJson"; "FromJsonStream"; "FromJsonAsync"; "FromObject"; "FromObjectDictionary"
      "FromObjectSequence"; "ToJson"; "WriteJson"; "ToObjectValue"; "ToDictionary"
      "ToStringValue"; "WithInputJson"; "WithInputJsonStream"; "WithInputObject"; "WithInputValue"; "ExecuteScriptWithJson"
      "ExecuteScript" ]
    |> Seq.map (fun c -> [| box c |])

let private hostCall (call: string) (k: int) : unit -> obj =
    let json = nestedArrayJson k
    match call with
    | "FromJson" -> fun () -> box (JyroValue.FromJson json)
    | "FromJsonStream" -> fun () -> box (JyroValue.FromJson(new MemoryStream(Encoding.UTF8.GetBytes json) :> Stream))
    | "FromJsonAsync" ->
        fun () ->
            box (JyroValue.FromJsonAsync(new MemoryStream(Encoding.UTF8.GetBytes json), System.Threading.CancellationToken.None).Result)
    | "FromObject" -> fun () -> box (JyroValue.FromObject(nestedHostList k))
    | "FromObjectDictionary" -> fun () -> box (JyroValue.FromObject(nestedDict k))
    | "FromObjectSequence" -> fun () -> box (JyroValue.FromObject(nestedHostList k :?> IEnumerable<obj>))
    | "ToJson" -> fun () -> box ((nestedArrayValue k).ToJson())
    | "WriteJson" ->
        fun () ->
            use ms = new MemoryStream()
            use w = new Text.Json.Utf8JsonWriter(ms)
            (nestedArrayValue k).WriteJson(w)
            box ms.Length
    | "ToObjectValue" -> fun () -> (nestedArrayValue k).ToObjectValue()
    | "ToDictionary" -> fun () -> box ((nestedObjectValue k :?> JyroObject).ToDictionary())
    | "ToStringValue" -> fun () -> box ((nestedArrayValue k).ToStringValue())
    | "WithInputJson" -> fun () -> box (JyroRunner(compiled ()).WithInputJson(json).Execute())
    | "WithInputJsonStream" ->
        fun () -> box (JyroRunner(compiled ()).WithInputJson(new MemoryStream(Encoding.UTF8.GetBytes json)).Execute())
    | "WithInputObject" -> fun () -> box (JyroRunner(compiled ()).WithInput(nestedHostList k).Execute())
    | "WithInputValue" -> fun () -> box (JyroRunner(compiled ()).WithInput(nestedArrayValue k).Execute())
    | "ExecuteScriptWithJson" -> fun () -> box (JyroScript.executeScriptWithJson "Output.r = 1" json)
    | "ExecuteScript" -> fun () -> box (JyroScript.executeScript "Output.r = 1" (nestedHostList k))
    | other -> failwithf "unknown call %s" other

[<Theory>]
[<MemberData(nameof hostCalls)>]
[<Trait("requirement", "JYRO-REQ-2046")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``HostApiCall_AtTheMaximum_Succeeds`` (call: string) =
    hostOk (hostCall call (n ()))

[<Theory>]
[<MemberData(nameof hostCalls)>]
[<Trait("requirement", "JYRO-REQ-2046")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``HostApiCall_PastTheMaximum_FailsThroughItsOwnChannelWithValueNestingLimitExceeded`` (call: string) =
    hostFails (hostCall call (n () + 1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``ResultValueConversion_ToHostValue_AppliesTheMaximum`` () =
    // An Output holding a value of depth N + 1 (Output is depth 1) converted to a host value.
    let r = roomyRun (JsonInput "{}") (lines [ buildNested "a" (n () - 1); "Output.a = a" ])
    let v = ok r
    v.ToObjectValue() |> ignore
    let r2 = roomyRun (JsonInput "{}") (lines [ buildNested "a" (n ()); "Output.a = a" ])
    match r2.TryGetValue() with
    | true, v2 when r2.IsSuccess -> throwsWithCode MessageCode.ValueNestingLimitExceeded (fun () -> v2.ToObjectValue())
    | _ -> hasError MessageCode.ValueNestingLimitExceeded r2

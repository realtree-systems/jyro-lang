/// 30.5 Objects and property keys.
module RealTree.Jyro.Spec.Tests.P30.S30_05_Objects

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Numerics
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Xml
open System.Xml.Linq
open Newtonsoft.Json.Linq
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support
open RealTree.Jyro.Spec.Tests.P30.HostNumbers

let private keysOf (o: JyroValue) : string list =
    match o with
    | :? JyroObject as obj -> obj |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    | _ -> Values.fail (sprintf "expected an object, got %s" (o.ToJson()))

let private strings (v: JyroValue) = Values.asList v |> List.map Values.asString

// ---------------------------------------------------------------- 30.5.1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-79")>]
let ``An object is changed in place through every reference`` () =
    let out =
        outOf (
            "func addD(q)\n q.d = 5\nend\n"
            + "var o = {a: 1}\nvar p = o\n"
            + "o.b = 2\no.a = 3\ndelete o.b\no[\"c\"] = 4\naddD(o)\n"
            + "Output.p = p\nOutput.o = o")
    Assert.Equal("{\"a\":3,\"c\":4,\"d\":5}", Values.toJson (Values.prop "p" out))
    Assert.Same(Values.prop "o" out, Values.prop "p" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 A script write to an object of the seed of Output lands in the result and leaves the host's own object as it was`` () =
    let o = JyroObject()
    o.SetProperty("a", Values.int 1)
    let r = Script.runWith { Script.defaults with Output = Some (Values.obj [ "o", o :> JyroValue ]) } "Output.o.b = 2\nOutput.o.a = 3"
    Assert.True(Script.ok r, Script.describe r)
    Assert.Equal("{\"a\":3,\"b\":2}", (Script.output r |> Values.prop "o").ToJson())
    Assert.Equal("{\"a\":1}", o.ToJson())

// ---------------------------------------------------------------- 30.5.2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1778")>]
let ``JYRO-REQ-1778 Keys that differ in any code unit are different keys, and the same code units are one key`` () =
    let out =
        outOf (
            "var o = {}\n"
            + "o[\"a\"] = 1\no[\"A\"] = 2\no.name = 3\no.Name = 4\n"
            + "o[\"\\u00E9\"] = 5\no[\"e\\u0301\"] = 6\no[\"\"] = 7\no[\"\\uD83D\\uDE00\"] = 8\n"
            + "o[\"a\"] = 10\n"
            + "Output.k = Keys(o)\nOutput.n = Length(Keys(o))\n"
            + "Output.v = [o.a, o.A, o.name, o.Name, o[\"\\u00E9\"], o[\"e\\u0301\"], o[\"\"], o[\"\\uD83D\\uDE00\"]]")
    assertNum 8M (Values.prop "n" out)
    Assert.Equal("[10,2,3,4,5,6,7,8]", Values.toJson (Values.prop "v" out))
    let keys = strings (Values.prop "k" out)
    Assert.Equal<string list>([ "a"; "A"; "name"; "Name"; "\u00E9"; "e\u0301"; ""; "\uD83D\uDE00" ], keys)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1778")>]
let ``JYRO-REQ-1778 HasProperty is case sensitive`` () =
    let out = outOf "var o = {a: 1}\nOutput.r = [HasProperty(o, \"a\"), HasProperty(o, \"A\")]"
    Assert.Equal("[true,false]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1778")>]
let ``JYRO-REQ-1778 Every key of an object is a string, however it was written`` () =
    let out =
        outOf (
            "var o = {a: 1, \"b c\": 2}\no[\"d\"] = 3\nvar k = Keys(o)\nvar t = []\n"
            + "foreach key in k do\n t = Append(t, TypeOf(key))\nend\n"
            + "Output.t = t\nOutput.s = [k[0] is string, k[1] is string, k[2] is string]")
    Assert.Equal("[\"string\",\"string\",\"string\"]", Values.toJson (Values.prop "t" out))
    Assert.Equal("[true,true,true]", Values.toJson (Values.prop "s" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1778")>]
let ``JYRO-REQ-1778 A key written with the same code units reaches the same property, a compatibility or normalized form does not`` () =
    let out =
        outOf (
            "var o = {}\no[\"k\"] = 1\no[\"\\u212A\"] = 2\no[\"\\uAC00\"] = 3\no[\"\\u1100\\u1161\"] = 4\n"
            + "o[\"k\"] = 5\n"
            + "Output.n = Length(Keys(o))\nOutput.v = [o[\"k\"], o[\"\\u212A\"], o[\"\\uAC00\"], o[\"\\u1100\\u1161\"]]")
    assertNum 4M (Values.prop "n" out)
    Assert.Equal("[5,2,3,4]", Values.toJson (Values.prop "v" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1778")>]
let ``JYRO-REQ-1778 JSON keys that differ only in case are different keys`` () =
    let r = Script.runJson "{\"a\":1,\"A\":2}" "Output.r = [Input.a, Input.A, Length(Keys(Input))]"
    Assert.Equal("[1,2,2]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.5.3

[<Theory>]
[<Trait("requirement", "JYRO-REQ-81")>]
[<InlineData("o.b")>]
[<InlineData("o[\"b\"]")>]
[<InlineData("o.A")>]
[<InlineData("{}.x")>]
[<InlineData("{x: {}}.x.y")>]
[<InlineData("Input.missing")>]
let ``Reading a property that does not exist gives null`` (expr: string) =
    let out = outOf ("var o = {a: 1}\nOutput.r = [" + expr + "]")
    Assert.Equal("[null]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-81")>]
let ``Reading a deleted property gives null`` () =
    let out = outOf "var o = {a: 1}\ndelete o.a\nOutput.r = [o.a, o[\"a\"]]"
    Assert.Equal("[null,null]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.5.4

[<Theory>]
[<Trait("requirement", "JYRO-REQ-82")>]
[<InlineData("1")>]
[<InlineData("1.0")>]
[<InlineData("true")>]
[<InlineData("null")>]
[<InlineData("[\"1\"]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``o[k] gives null when k is not a string`` (key: string) =
    let out = outOf ("var o = {\"1\": \"one\", \"true\": \"t\", \"null\": \"n\"}\nvar k = " + key + "\nOutput.r = [o[k]]")
    Assert.Equal("[null]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.5.5

[<Theory>]
[<Trait("requirement", "JYRO-REQ-83")>]
[<InlineData("1")>]
[<InlineData("1.5")>]
[<InlineData("true")>]
[<InlineData("null")>]
[<InlineData("[\"a\"]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``o[k] = v raises InvalidIndexType when k is not a string`` (key: string) =
    let target = JyroObject()
    let r = Script.runWith { Script.defaults with Output = Some (target :> JyroValue) } ("var k = " + key + "\nOutput[k] = 9")
    Script.expectError MessageCode.InvalidIndexType r
    Assert.Equal(0, target.Count)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-83")>]
let ``o[k] = v on a script object raises InvalidIndexType for a number key`` () =
    expectRun MessageCode.InvalidIndexType "var o = {}\no[0] = 1"

// ---------------------------------------------------------------- 30.5.6

[<Fact>]
[<Trait("requirement", "JYRO-REQ-84")>]
let ``A key with a dot is one key, not a path`` () =
    let out =
        outOf (
            "var o = {a: {b: 1}}\n"
            + "Output.before = [o[\"a.b\"]]\n"
            + "o[\"a.b\"] = 2\n"
            + "Output.after = o[\"a.b\"]\nOutput.nested = o.a.b\nOutput.keys = Keys(o)\n"
            + "var l = {\"a.b\": 3}\nOutput.lit = [l[\"a.b\"], l.a]\n"
            + "var d = {}\nd[\".\"] = 4\nd[\"a..b\"] = 5\nd[\".a\"] = 6\n"
            + "Output.dots = [d[\".\"], d[\"a..b\"], d[\".a\"], Length(Keys(d))]")
    Assert.Equal("[null]", Values.toJson (Values.prop "before" out))
    assertNum 2M (Values.prop "after" out)
    assertNum 1M (Values.prop "nested" out)
    Assert.Equal<string list>([ "a"; "a.b" ], strings (Values.prop "keys" out))
    Assert.Equal("[3,null]", Values.toJson (Values.prop "lit" out))
    Assert.Equal("[4,5,6,3]", Values.toJson (Values.prop "dots" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-84")>]
let ``A dotted key from JSON Input is one key`` () =
    let r = Script.runJson "{\"a.b\": 5, \"a\": {\"b\": 6}}" "Output.r = [Input[\"a.b\"], Input.a.b]"
    Assert.Equal("[5,6]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.5.7

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1949")>]
[<InlineData("k")>]
[<InlineData("if")>]
[<InlineData("end")>]
[<InlineData("null")>]
[<InlineData("Input")>]
[<InlineData("Output")>]
[<InlineData("any")>]
[<InlineData("caf\u00E9")>]
[<InlineData("_x1")>]
let ``Dot and bracket access resolve the same key to the same property`` (key: string) =
    let out =
        outOf (
            "var o = {}\n"
            + "o." + key + " = 1\n"
            + "Output.a = o[\"" + key + "\"]\n"
            + "o[\"" + key + "\"] = 2\n"
            + "Output.b = o." + key + "\n"
            + "Output.n = Length(Keys(o))")
    assertNum 1M (Values.prop "a" out)
    assertNum 2M (Values.prop "b" out)
    assertNum 1M (Values.prop "n" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1949")>]
let ``Dot access of a key that differs in case gives null`` () =
    let out = outOf "var o = {}\no[\"Key\"] = 1\nOutput.r = [o.key, o.Key]"
    Assert.Equal("[null,1]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.5.8

let private hostLookups : (string * (JyroObject -> string -> JyroValue option)) list =
    [ "JyroObject.GetProperty", (fun o k -> let v = o.GetProperty k in if v.IsNull then None else Some v)
      "JyroValue.GetProperty", (fun o k -> let v = (o :> JyroValue).GetProperty k in if v.IsNull then None else Some v)
      "Item",
      (fun o k ->
          try
              let v = o.[k]
              if isNull (box v) || v.IsNull then None else Some v
          with :? KeyNotFoundException -> None)
      "TryGetValue", (fun o k -> match o.TryGetValue k with | true, v -> Some v | _ -> None)
      "ContainsKey", (fun o k -> if (o :> IReadOnlyDictionary<string, JyroValue>).ContainsKey k then Some ((o :> IReadOnlyDictionary<string, JyroValue>).[k]) else None)
      "ToDictionary", (fun o k -> match o.ToDictionary().TryGetValue k with | true, v -> Some (JyroValue.FromObject v) | _ -> None)
      "ToObjectValue",
      (fun o k ->
          match o.ToObjectValue() with
          | :? IDictionary<string, obj> as d -> (match d.TryGetValue k with | true, v -> Some (JyroValue.FromObject v) | _ -> None)
          | other -> Values.fail (sprintf "ToObjectValue gave %s" (other.GetType().FullName))) ]

let hostLookupNames () : obj[] seq = hostLookups |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-86")>]
[<MemberData(nameof hostLookupNames)>]
let ``Every host lookup resolves a key to the property script access resolves it to`` (lookup: string) =
    let out =
        outOf (
            "Output.a = 1\nOutput.A = 2\nOutput[\"n.b\"] = 3\nOutput[\"\"] = 4\n"
            + "Output[\"\\u00E9\"] = 5\nOutput[\"e\\u0301\"] = 6\nOutput.n = {b: 7}")
    let o = out :?> JyroObject
    let find = hostLookups |> List.find (fun (n, _) -> n = lookup) |> snd
    let expect (key: string) (json: string) =
        match find o key with
        | Some v -> Assert.Equal(json, Values.toJson v)
        | None -> Values.fail (sprintf "%s did not find the key %A" lookup key)
    expect "a" "1"
    expect "A" "2"
    expect "n.b" "3"
    expect "" "4"
    expect "\u00E9" "5"
    expect "e\u0301" "6"
    expect "n" "{\"b\":7}"
    for absent in [ "b"; "N"; "a.b"; "E\u0301"; "n.B" ] do
        Assert.True((find o absent).IsNone, sprintf "%s found the absent key %A" lookup absent)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-86")>]
let ``Keys the host sets are read by script access to the same property`` () =
    let o = JyroObject()
    o.SetProperty("a.b", Values.int 8)
    o.SetProperty("A", Values.int 9)
    o.SetProperty("a", Values.obj [ "b", Values.int 10 ])
    let r = Script.runInput o "Output.r = [Input[\"a.b\"], Input.A, Input.a.b, Input[\"a\"][\"b\"]]"
    Assert.Equal("[8,9,10,10]", Values.toJson (Values.prop "r" (Script.output r)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-86")>]
let ``Keys of a host dictionary are read by script access to the same property`` () =
    let d = Dictionary<string, obj>()
    d.["a.b"] <- box 8
    d.["A"] <- box 9
    d.["a"] <- box 11
    let o = JyroValue.FromObject(d)
    let r = Script.runInput o "Output.r = [Input[\"a.b\"], Input.A, Input.a]"
    Assert.Equal("[8,9,11]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.5.9

/// Reads the order of the object `o` of the script by every path:
/// foreach (values that equal their keys), Keys, Values, ToJson, host enumeration.
let private orderPaths (build: string) =
    let out =
        outOf (
            build + "\n"
            + "var f = []\nvar i = 0\nforeach v in o do\n f[i] = v\n i = i + 1\nend\n"
            + "Output.foreach = f\nOutput.keys = Keys(o)\nOutput.values = Values(o)\nOutput.json = ToJson(o)\nOutput.o = o")
    out

[<Theory>]
[<Trait("requirement", "JYRO-REQ-87")>]
[<InlineData("var o = {}\no.b = \"b\"\no.a = \"a\"\no.c = \"c\"", "b,a,c")>]
[<InlineData("var o = {b: \"b\", a: \"a\", c: \"c\"}", "b,a,c")>]
[<InlineData("var o = {}\no[\"2\"] = \"2\"\no[\"1\"] = \"1\"\no[\"10\"] = \"10\"", "2,1,10")>]
[<InlineData("var o = {b: \"b\", a: \"a\"}\no.b = \"b\"\no.a = \"a\"", "b,a")>]
[<InlineData("var o = {a: \"a\", b: \"b\", c: \"c\", d: \"d\", e: \"e\"}\ndelete o.c\no.c = \"c\"", "a,b,d,e,c")>]
[<InlineData("var o = {a: \"a\", b: \"b\", c: \"c\"}\ndelete o.a\no.a = \"a\"\ndelete o.b\no[\"b\"] = \"b\"", "c,a,b")>]
[<InlineData("var o = {}\no.x = \"x\"\no[\"y\"] = \"y\"\no.z = \"z\"\no[\"x\"] = \"x\"", "x,y,z")>]
let ``JYRO-REQ-87 Every iteration path follows the order properties were added`` (build: string, order: string) =
    let expected = order.Split(',') |> List.ofArray
    let out = orderPaths build
    Assert.Equal<string list>(expected, strings (Values.prop "foreach" out))
    Assert.Equal<string list>(expected, strings (Values.prop "keys" out))
    Assert.Equal<string list>(expected, strings (Values.prop "values" out))
    let json = "{" + String.Join(",", expected |> List.map (fun k -> sprintf "\"%s\":\"%s\"" k k)) + "}"
    Assert.Equal(json, Values.asString (Values.prop "json" out))
    Assert.Equal<string list>(expected, keysOf (Values.prop "o" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-87")>]
let ``JYRO-REQ-87 A property removed and added again goes after every property the object has`` () =
    let out = outOf "var o = {a: 1, b: 2}\ndelete o.a\no.a = 3\nOutput.k = Keys(o)\nOutput.j = ToJson(o)"
    Assert.Equal<string list>([ "b"; "a" ], strings (Values.prop "k" out))
    Assert.Equal("{\"b\":2,\"a\":3}", Values.asString (Values.prop "j" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-87")>]
let ``JYRO-REQ-87 A thousand keys keep the order they were added in`` () =
    let rng = Random(30509)
    let keys = [ for i in 0 .. 999 -> sprintf "k%04d" i ] |> List.sortBy (fun _ -> rng.Next())
    let input = Values.obj [ "ks", Values.arr (keys |> List.map Values.str) ]
    let r =
        Script.runWith { bigLimits with Input = ValueInput input }
            "var o = {}\nforeach k in Input.ks do\n o[k] = k\nend\nOutput.k = Keys(o)\nOutput.o = o"
    let out = Script.output r
    Assert.Equal<string list>(keys, strings (Values.prop "k" out))
    Assert.Equal<string list>(keys, keysOf (Values.prop "o" out))

// ---------------------------------------------------------------- 30.5.10

/// A script with no standard library that lists the values of Input in its order.
let private listValues =
    "var r = []\nvar i = 0\nforeach v in Input do\n r[i] = v\n i = i + 1\nend\nOutput.r = r"

/// A dictionary whose values equal their keys, and its own enumeration order.
let private selfDict (build: Dictionary<string, obj> -> unit) =
    let d = Dictionary<string, obj>()
    build d
    let order = d |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    d, order

let private dictionaries () : (string * IDictionary<string, obj> * string list) list =
    let plain, plainOrder = selfDict (fun d -> d.["z"] <- box "z"; d.["a"] <- box "a"; d.["m"] <- box "m")
    let removed, removedOrder =
        selfDict (fun d ->
            d.["z"] <- box "z"; d.["a"] <- box "a"; d.["m"] <- box "m"
            d.Remove("a") |> ignore
            d.["q"] <- box "q")
    let sorted = SortedDictionary<string, obj>()
    sorted.["z"] <- box "z"
    sorted.["a"] <- box "a"
    sorted.["m"] <- box "m"
    let sortedOrder = sorted |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    [ "Dictionary", plain :> IDictionary<string, obj>, plainOrder
      "Dictionary after a removal", removed :> IDictionary<string, obj>, removedOrder
      "SortedDictionary", sorted :> IDictionary<string, obj>, sortedOrder ]

let dictionaryNames () : obj[] seq = dictionaries () |> Seq.map (fun (n, _, _) -> [| box n |])

let private hostDictPaths : (string * (IDictionary<string, obj> -> JyroValue)) list =
    let runOn (input: obj) =
        let program = Script.valueOf (Script.compileWith { Script.defaults with Stdlib = false } listValues)
        JyroRunner(program).WithInput(input).Execute() |> Script.output |> Values.prop "r"
    [ "JyroValue.FromObject", (fun d -> (JyroValue.FromObject(box d) :?> JyroObject) |> Seq.map (fun kv -> kv.Value) |> Values.arr)
      "JyroRunner.WithInput(Object)", (fun d -> runOn (box d))
      "JyroScript.ExecuteScript", (fun d -> executeScript(listValues, box d) |> Script.output |> Values.prop "r")
      "nested in a List", (fun d -> JyroValue.FromObject(box (List<obj>([ box d ]))) |> Values.asList |> List.head :?> JyroObject |> Seq.map (fun kv -> kv.Value) |> Values.arr) ]

let dictionaryPathData () : obj[] seq =
    seq {
        for (d, _, _) in dictionaries () do
            for (p, _) in hostDictPaths do
                yield [| box d; box p |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof dictionaryPathData)>]
let ``JYRO-REQ-88 An object made from a host dictionary has the dictionary's enumeration order`` (dictName: string, path: string) =
    let (_, d, order) = dictionaries () |> List.find (fun (n, _, _) -> n = dictName)
    let make = hostDictPaths |> List.find (fun (n, _) -> n = path) |> snd
    Assert.Equal<string list>(order, strings (make d))

/// A host object with public properties declared Z, A, M.
type Zam() =
    member _.Z = "Z"
    member _.A = "A"
    member _.M = "M"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 An object made from any other host object has the order of its public properties`` () =
    let expected = typeof<Zam>.GetProperties() |> Array.map (fun p -> p.Name) |> List.ofArray
    let viaFromObject = JyroValue.FromObject(box (Zam())) :?> JyroObject |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    Assert.Equal<string list>(expected, viaFromObject)
    let program = Script.valueOf (Script.compileWith { Script.defaults with Stdlib = false } listValues)
    let viaInput = JyroRunner(program).WithInput(box (Zam())).Execute() |> Script.output |> Values.prop "r"
    Assert.Equal<string list>(expected, strings viaInput)
    let viaScript = executeScript(listValues, box (Zam())) |> Script.output |> Values.prop "r"
    Assert.Equal<string list>(expected, strings viaScript)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 An object made from an anonymous host object has the order of its public properties`` () =
    let anon = {| Zed = 1; Alpha = 2; Mid = 3 |}
    let expected = anon.GetType().GetProperties() |> Array.map (fun p -> p.Name) |> List.ofArray
    let keys = JyroValue.FromObject(box anon) :?> JyroObject |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    Assert.Equal<string list>(expected, keys)

// JYRO-DEF-607 under JYRO-ADR-152: a host dictionary is any IDictionary<string, T>
// or IReadOnlyDictionary<string, T>, or a non-generic IDictionary whose keys are all
// strings; any other host object is an object of its public properties (and a
// ValueTuple's fields Item1, Item2, ...), whether or not its type has a ToString
// of its own. Each is an object, never text and never an array of entries.

/// Host values that are objects, with their keys in the host's order and the
/// object's JSON. Order and value are both REQ-88's: the object has the entries.
let private hostObjects () : (string * obj * string list * string) list =
    let typedInt = Dictionary<string, int>()
    typedInt.["z"] <- 1
    typedInt.["a"] <- 2
    typedInt.["m"] <- 3
    let typedDouble = Dictionary<string, float>()
    typedDouble.["b"] <- 1.5
    typedDouble.["a"] <- 2.5
    let hashtable = Collections.Hashtable()
    hashtable.["z"] <- box 1
    hashtable.["a"] <- box "x"
    hashtable.["m"] <- box true
    let hashOrder = [ for k in hashtable.Keys -> string k ]
    let hashJson =
        "{" + String.Join(",", hashOrder |> List.map (fun k ->
            match k with
            | "z" -> "\"z\":1"
            | "a" -> "\"a\":\"x\""
            | _ -> "\"m\":true")) + "}"
    [ "Dictionary<string, int>", box typedInt, [ "z"; "a"; "m" ], "{\"z\":1,\"a\":2,\"m\":3}"
      "Dictionary<string, double>", box typedDouble, [ "b"; "a" ], "{\"b\":1.5,\"a\":2.5}"
      "IReadOnlyDictionary<string, int>", box (ReadOnlyOnly<int>([ "z", 1; "a", 2 ])), [ "z"; "a" ], "{\"z\":1,\"a\":2}"
      "Hashtable with string keys", box hashtable, hashOrder, hashJson
      "record-like (ToString of its own)", box (RecordPair<int>(1, 2)), [ "A"; "B" ], "{\"A\":1,\"B\":2}"
      "ValueTuple (1, \"s\")", box (struct (1, "s")), [ "Item1"; "Item2" ], "{\"Item1\":1,\"Item2\":\"s\"}"
      "Dictionary<string, int> in a record-like", box (RecordLike<Dictionary<string, int>>(typedInt)), [ "X" ], "{\"X\":{\"z\":1,\"a\":2,\"m\":3}}" ]

/// A plain host object (no ToString of its own) with one property, X.
type Wrap(x: obj) =
    member _.X = x

let private hostObjectPaths : (string * (obj -> JyroValue)) list =
    let echo = "Output.v = Input"
    [ "JyroValue.FromObject", (fun o -> JyroValue.FromObject o)
      "JyroRunner.WithInput(Object)",
      (fun o -> JyroRunner(Script.valueOf (Script.compile echo)).WithInput(o).Execute() |> Script.output |> Values.prop "v")
      "JyroScript.ExecuteScript", (fun o -> executeScript(echo, o) |> Script.output |> Values.prop "v")
      "nested in a List", (fun o -> JyroValue.FromObject(box (List<obj>([ o ]))) |> Values.asList |> List.head)
      "JyroValue.FromObject dictionary value", (fun o -> JyroValue.FromObject(Dictionary<string, obj>(dict [ "k", o ])) |> Values.prop "k")
      "JyroValue.FromObject property", (fun o -> JyroValue.FromObject(Wrap(o)) |> Values.prop "X") ]

let hostObjectData () : obj[] seq =
    seq {
        for (name, _, _, _) in hostObjects () do
            for (p, _) in hostObjectPaths do
                yield [| box name; box p |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof hostObjectData)>]
let ``JYRO-REQ-88 a typed or non-generic dictionary, a record-like object and a tuple become objects of their entries in the host's order`` (name: string, path: string) =
    let (_, value, order, json) = hostObjects () |> List.find (fun (n, _, _, _) -> n = name)
    let make = hostObjectPaths |> List.find (fun (n, _) -> n = path) |> snd
    let v = make value
    if v.ValueType <> JyroValueType.Object then
        Values.fail (sprintf "%s through %s became a %A, %s, not an object" name path v.ValueType (Values.toJson v))
    Assert.Equal<string list>(order, keysOf v)
    Assert.Equal(json, Values.toJson v)

// JYRO-ADR-154 (amending ADR-152 (1)): whether a host value is a dictionary is
// decided by its type. A type that implements IDictionary<K, T> or
// IReadOnlyDictionary<K, T> only for a K that is not string is an array of its
// entries, each {Key, Value}, even when empty; a non-generic IDictionary that
// implements no generic one is an object when every key is a string (empty
// included), else an array of entries. JYRO-ADR-156 (amending ADR-152 (3), (4)):
// every .NET integer and binary floating-point type is a number; a JsonNode is
// its JSON; StringBuilder and CultureInfo are their text (reflection metadata and
// delegates are refused, JYRO-REQ-1903); any other object is its public readable instance properties then
// its public instance fields; a property whose getter throws is left out; an
// object with no public property or field is its text.

/// What a host value must become.
type private Becomes =
    /// A value whose ToJson is exactly this text.
    | Json of string
    /// A string holding exactly this text.
    | Text of string
    /// An object that passes this check.
    | ObjectWhere of (JyroValue -> unit)

let private notThrown () : exn = InvalidOperationException("boom") :> exn

let private hostConversions () : (string * (unit -> obj) * Becomes) list =
    let intDict (pairs: (int * int) list) =
        let d = Dictionary<int, int>()
        for (k, v) in pairs do d.[k] <- v
        box d
    let objectKeyed () =
        let d = Dictionary<obj, int>()
        d.[box "a"] <- 1
        box d
    let sorted () =
        let d = SortedDictionary<int, string>()
        d.[3] <- "c"
        d.[1] <- "a"
        box d
    let intKeyHashtable () =
        let h = Collections.Hashtable()
        h.[box 1] <- box "x"
        box h
    let nested () = box (Dictionary<string, obj>(dict [ "d", intDict [ 1, 2 ] ]))
    let hasKey k (v: JyroValue) = Assert.Contains(k, keysOf v)
    let lacksKey k (v: JyroValue) = Assert.DoesNotContain(k, keysOf v)
    let parse (t: string) = JsonNode.Parse t
    let xmlDoc (t: string) =
        let d = XmlDocument()
        d.LoadXml t
        d
    [ // JYRO-ADR-154: dictionaries by key type (JYRO-DEF-611, JYRO-DEF-612).
      "Dictionary<int, int> {2: 20, 1: 10}", (fun () -> intDict [ 2, 20; 1, 10 ]), Json "[{\"Key\":2,\"Value\":20},{\"Key\":1,\"Value\":10}]"
      "Dictionary<int, int>, empty", (fun () -> intDict []), Json "[]"
      "Dictionary<object, int> whose keys are all strings", objectKeyed, Json "[{\"Key\":\"a\",\"Value\":1}]"
      "Dictionary<object, int>, empty", (fun () -> box (Dictionary<obj, int>())), Json "[]"
      "SortedDictionary<int, string>", sorted, Json "[{\"Key\":1,\"Value\":\"a\"},{\"Key\":3,\"Value\":\"c\"}]"
      "Hashtable with an int key", intKeyHashtable, Json "[{\"Key\":1,\"Value\":\"x\"}]"
      "Hashtable, empty", (fun () -> box (Collections.Hashtable())), Json "{}"
      "Dictionary<string, int>, empty", (fun () -> box (Dictionary<string, int>())), Json "{}"
      "Dictionary<string, object>, empty", (fun () -> box (Dictionary<string, obj>())), Json "{}"
      "IReadOnlyDictionary<string, int> alone, empty", (fun () -> box (ReadOnlyOnly<int>([]))), Json "{}"
      "Dictionary<int, int> inside a Dictionary<string, object>", nested, Json "{\"d\":[{\"Key\":1,\"Value\":2}]}"
      // JYRO-ADR-156 (b): a JsonNode is its JSON (JYRO-DEF-618).
      "JsonNode.Parse of an object", (fun () -> box (parse "{\"z\":1,\"a\":[1,\"s\"],\"n\":null,\"t\":true}")), Json "{\"z\":1,\"a\":[1,\"s\"],\"n\":null,\"t\":true}"
      "JsonNode.Parse of an array", (fun () -> box (parse "[1,\"s\",{\"k\":false}]")), Json "[1,\"s\",{\"k\":false}]"
      "JsonNode.Parse of a number", (fun () -> box (parse "5")), Json "5"
      "JsonNode.Parse of a string", (fun () -> box (parse "\"x\"")), Text "x"
      "a JsonArray child of a parsed object", (fun () -> box ((parse "{\"a\":[3,4]}").["a"])), Json "[3,4]"
      "a JsonObject child of a parsed object", (fun () -> box ((parse "{\"o\":{\"k\":1}}").["o"])), Json "{\"k\":1}"
      "a JsonValue child of a parsed array", (fun () -> box ((parse "[7]").[0])), Json "7"
      "JsonObject, empty", (fun () -> box (JsonObject())), Json "{}"
      "JsonArray, empty", (fun () -> box (JsonArray())), Json "[]"
      "JsonValue.Create(true)", (fun () -> box (JsonValue.Create(true))), Json "true"
      // JYRO-ADR-156 (c), (f): text (JYRO-DEF-614, JYRO-DEF-619).
      "StringBuilder", (fun () -> box (StringBuilder("abc"))), Text "abc"
      // (A Type, MemberInfo, Assembly, Module, ParameterInfo or delegate is no longer text: it is refused,
      // JYRO-REQ-1903, JYRO-ADR-200 item 2.)
      "CultureInfo fr-FR", (fun () -> box (CultureInfo("fr-FR"))), Text "fr-FR"
      "CultureInfo.InvariantCulture", (fun () -> box CultureInfo.InvariantCulture), Text ""
      "no public property or field", (fun () -> box (NoMembers())), Text "no members"
      // JYRO-ADR-156 (d): properties then fields, for every type (JYRO-DEF-617).
      "struct with a public field", (fun () -> box (fieldStruct 5)), Json "{\"X\":5}"
      "class with a property and a field", (fun () -> box (propAndField 1 2)), Json "{\"P\":1,\"F\":2}"
      "static, indexer and internal members left out", (fun () -> box (Members())), Json "{\"Visible\":2}"
      "Vector2(1, 2)", (fun () -> box (Vector2(1.0f, 2.0f))),
      ObjectWhere (fun v ->
          Assert.Equal<string list>(memberKeys typeof<Vector2>, keysOf v)
          Assert.Equal(1.0, Values.asFloat (Values.prop "X" v))
          Assert.Equal(2.0, Values.asFloat (Values.prop "Y" v)))
      // An exception that was not thrown has no TargetSite, which would be a MethodBase (JYRO-REQ-1903).
      "an Exception that was not thrown", (fun () -> box (notThrown ())),
      ObjectWhere (fun v ->
          Assert.Equal("boom", Values.asString (Values.prop "Message" v))
          Assert.Equal(JyroValueType.Null, (Values.prop "TargetSite" v).ValueType))
      // JYRO-ADR-156 (e): a getter that throws is left out; the value stays an object (JYRO-DEF-615).
      "a getter that does not throw", (fun () -> box (Guarded(true))), Json "{\"Ok\":true,\"Value\":5}"
      "a getter that throws", (fun () -> box (Guarded(false))), Json "{\"Ok\":false}"
      "F# ValueSome 5", (fun () -> box (ValueSome 5)),
      ObjectWhere (fun v -> Assert.Equal(5.0, Values.asFloat (Values.prop "Value" v)))
      "F# ValueNone", (fun () -> box (ValueNone: int voption)), ObjectWhere (lacksKey "Value")
      "MemoryStream", (fun () -> box (new MemoryStream())),
      ObjectWhere (fun v ->
          hasKey "CanRead" v
          hasKey "Length" v
          lacksKey "ReadTimeout" v
          lacksKey "WriteTimeout" v)
      // JYRO-ADR-156 (g): the walk calls the getters it reads.
      "an unforced Lazy<int>", (fun () -> box (Lazy<int>(fun () -> 5))),
      ObjectWhere (fun v -> Assert.Equal(5.0, Values.asFloat (Values.prop "Value" v)))
      // JYRO-ADR-157 (b1): a JsonValue made from a .NET value is that value, in the
      // walk's shape, not JsonSerializer's (JYRO-DEF-620).
      "JsonValue of a class with a property and a field", (fun () -> jsonValueOf (propAndField 1 2)), Json "{\"P\":1,\"F\":2}"
      "JsonValue of a getter that throws", (fun () -> jsonValueOf (Guarded(false))), Json "{\"Ok\":false}"
      "JsonValue of Dictionary<int, int>", (fun () -> jsonValueOf (intDict [ 2, 20; 1, 10 ] :?> Dictionary<int, int>)),
      Json "[{\"Key\":2,\"Value\":20},{\"Key\":1,\"Value\":10}]"
      "JsonValue of an object[]", (fun () -> jsonValueOf [| box 1; box "s"; box true; null |]), Json "[1,\"s\",true,null]"
      "JsonValue of a struct with a public field", (fun () -> jsonValueOf (fieldStruct 5)), Json "{\"X\":5}"
      "JsonValue of StringBuilder", (fun () -> jsonValueOf (StringBuilder("abc"))), Text "abc"
      // (JsonValue.Create(JsonElement) takes only a JSON primitive.)
      "JsonValue of a JsonElement string", (fun () -> box (JsonValue.Create(JsonDocument.Parse("\"x\"").RootElement))), Text "x"
      // JYRO-ADR-157 (b2): a Newtonsoft JToken is its JSON (JYRO-DEF-623).
      "JObject.Parse of an object", (fun () -> box (JObject.Parse "{\"z\":1,\"a\":[1,\"s\"],\"n\":null,\"t\":true}")),
      Json "{\"z\":1,\"a\":[1,\"s\"],\"n\":null,\"t\":true}"
      "JArray.Parse of an array", (fun () -> box (JArray.Parse "[1,\"s\",{\"k\":false},[]]")), Json "[1,\"s\",{\"k\":false},[]]"
      "JArray of numbers", (fun () -> box (JArray(1, 2, 3))), Json "[1,2,3]"
      "JToken.Parse of a number", (fun () -> box (JToken.Parse "5")), Json "5"
      "a JObject child of a parsed JObject", (fun () -> box ((JObject.Parse "{\"o\":{\"k\":1}}").["o"])), Json "{\"k\":1}"
      "a JArray child of a parsed JObject", (fun () -> box ((JObject.Parse "{\"a\":[3,4]}").["a"])), Json "[3,4]"
      "a JValue child of a parsed JArray", (fun () -> box ((JArray.Parse "[7]").[0])), Json "7"
      "JValue(\"s\")", (fun () -> box (JValue("s"))), Text "s"
      "JValue(true)", (fun () -> box (JValue(true))), Json "true"
      "JValue null", (fun () -> box (JValue.CreateNull())), Json "null"
      "JValue undefined", (fun () -> box (JValue.CreateUndefined())), Json "null"
      "JObject, empty", (fun () -> box (JObject())), Json "{}"
      "JArray, empty", (fun () -> box (JArray())), Json "[]"
      "JRaw", (fun () -> box (JRaw("[1, 2]"))), Text "[1, 2]"
      // JYRO-ADR-157 (b3): a LINQ to XML or System.Xml node is its XML text (JYRO-DEF-624).
      "XElement with a child", (fun () -> box (XElement.Parse "<a><b>1</b></a>")), Text "<a><b>1</b></a>"
      "XElement with an attribute and an empty child", (fun () -> box (XElement(XName.Get "a", XAttribute(XName.Get "k", "v"), XElement(XName.Get "b")))),
      Text (XElement(XName.Get "a", XAttribute(XName.Get "k", "v"), XElement(XName.Get "b")).ToString(SaveOptions.DisableFormatting))
      "a child XElement of a parsed tree", (fun () -> box ((XElement.Parse "<a><b>1</b><c/></a>").Element(XName.Get "b"))), Text "<b>1</b>"
      "XDocument", (fun () -> box (XDocument.Parse "<a><b>1</b></a>")), Text "<a><b>1</b></a>"
      "XText of a parsed tree", (fun () -> box ((XElement.Parse "<a><b>1</b></a>").Element(XName.Get "b").FirstNode)), Text "1"
      "XmlDocument's element", (fun () -> box (xmlDoc "<a><b>1</b></a>").DocumentElement), Text "<a><b>1</b></a>"
      "XmlDocument", (fun () -> box (xmlDoc "<a><b>1</b></a>")), Text "<a><b>1</b></a>"
      "XmlText", (fun () -> box (xmlDoc "<a><b>1</b></a>").DocumentElement.FirstChild.FirstChild), Text "1"
      "XmlAttribute", (fun () -> box (xmlDoc "<a k=\"v\"/>").DocumentElement.Attributes.["k"]), Text "k=\"v\"" ]

let hostConversionData () : obj[] seq =
    seq {
        for (name, _, _) in hostConversions () do
            for (p, _) in hostObjectPaths do
                yield [| box name; box p |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof hostConversionData)>]
let ``JYRO-REQ-88 a host value becomes the object, array or text its type gives, on every host path`` (name: string, path: string) =
    let (_, make, becomes) = hostConversions () |> List.find (fun (n, _, _) -> n = name)
    let convert = hostObjectPaths |> List.find (fun (n, _) -> n = path) |> snd
    let v = convert (make ())
    match becomes with
    | Json json ->
        Assert.Equal(json, Values.toJson v)
    | Text text ->
        if v.ValueType <> JyroValueType.String then
            Values.fail (sprintf "%s through %s became a %A, %s, not a string" name path v.ValueType (Values.toJson v))
        Assert.Equal(text, Values.asString v)
    | ObjectWhere check ->
        if v.ValueType <> JyroValueType.Object then
            Values.fail (sprintf "%s through %s became a %A, %s, not an object" name path v.ValueType (Values.toJson v))
        check v

[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 FromObject of a plain host object reads properties then fields and leaves out a getter that throws`` () =
    let keysFrom (o: obj) = JyroValue.FromObject(o) :?> JyroObject |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    Assert.Equal<string list>([ "P"; "F" ], keysFrom (propAndField 1 2))
    Assert.Equal<string list>([ "X" ], keysFrom (fieldStruct 5))
    Assert.Equal<string list>([ "Ok"; "Value" ], keysFrom (Guarded(true)))
    Assert.Equal<string list>([ "Ok" ], keysFrom (Guarded(false)))
    Assert.Equal<string list>([ "Visible" ], keysFrom (Members()))

/// A host sequence whose enumeration fails after its first element.
let private brokenSequence () : obj =
    box (seq {
        yield 1
        raise (InvalidOperationException "the host's sequence is broken") })

[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 an exception from enumerating a host sequence passes out of FromObject unchanged`` () =
    // JYRO-ADR-156 (e): the host's container is broken, not a member absent.
    let ex = Assert.ThrowsAny<exn>(fun () -> JyroValue.FromObject(brokenSequence ()) |> ignore)
    Assert.IsType<InvalidOperationException>(ex) |> ignore
    Assert.Equal("the host's sequence is broken", ex.Message)

/// Host numbers of every .NET number type, each with what it must become
/// (JYRO-ADR-156 (a)): an integer within decimal's range exact, beyond it the
/// nearest double; a binary floating-point number widened to double; a
/// JsonNode number read as the JSON doors read it (30.2.7).
let private hostNumbers () : (string * (unit -> obj) * (JyroValue -> unit)) list =
    let two (n: float) = Math.Pow(2.0, n)
    [ "BigInteger 5", (fun () -> box (BigInteger(5))), assertExact 5m
      "BigInteger 2^96 - 1", (fun () -> box (BigInteger.Parse "79228162514264337593543950335")), assertExact 79228162514264337593543950335m
      "BigInteger 10^30", (fun () -> box (BigInteger.Pow(BigInteger(10), 30))), assertInexact (dbl "1e30")
      "BigInteger -10^30", (fun () -> box (BigInteger.Negate(BigInteger.Pow(BigInteger(10), 30)))), assertInexact (dbl "-1e30")
      "Int128 5", (fun () -> box (Int128(0UL, 5UL))), assertExact 5m
      "Int128.MaxValue", (fun () -> box Int128.MaxValue), assertInexact (two 127.0)
      "UInt128.MaxValue", (fun () -> box UInt128.MaxValue), assertInexact (two 128.0)
      "nint -7", (fun () -> box (nativeint -7)), assertExact -7m
      "nuint 8", (fun () -> box (unativeint 8)), assertExact 8m
      "byte 200", (fun () -> box 200uy), assertExact 200m
      "Half 1.5", (fun () -> box (BitConverter.UInt16BitsToHalf 0x3E00us)), assertInexact 1.5
      "NFloat 2.5", (fun () -> box (Runtime.InteropServices.NFloat(2.5))), assertInexact 2.5
      "decimal 0.1", (fun () -> box 0.1m), assertExact 0.1m
      "JsonNode.Parse 0.1", (fun () -> box (JsonNode.Parse "0.1")), assertExact 0.1m
      "JsonNode.Parse 1e-29", (fun () -> box (JsonNode.Parse "1e-29")), assertInexact (dbl "1e-29")
      // JYRO-ADR-157 (a1): integers are read through their two's complement and rounded
      // to the nearest double, not truncated: 2^100 + 2^47 + 1 is past the midpoint
      // 2^100 + 2^47, so it rounds up to 2^100 + 2^48.
      "BigInteger 2^100 + 2^47 + 1", (fun () -> box (BigInteger.Pow(BigInteger(2), 100) + BigInteger.Pow(BigInteger(2), 47) + BigInteger.One)),
      assertInexact (two 100.0 + two 48.0)
      "BigInteger -(2^100 + 2^47 + 1)", (fun () -> box (BigInteger.Negate(BigInteger.Pow(BigInteger(2), 100) + BigInteger.Pow(BigInteger(2), 47) + BigInteger.One))),
      assertInexact (-(two 100.0 + two 48.0))
      // JYRO-ADR-157 (a1): types from outside the BCL, read only through the members their
      // interfaces require (JYRO-DEF-621, JYRO-DEF-622). ThirdPartyInteger's "D" text is
      // hexadecimal and its conversions refuse; ThirdPartyFloat's conversions refuse.
      "ThirdPartyInteger 5", (fun () -> box (ThirdPartyInteger(BigInteger(5)))), assertExact 5m
      "ThirdPartyInteger -7", (fun () -> box (ThirdPartyInteger(BigInteger(-7)))), assertExact -7m
      "ThirdPartyInteger 0", (fun () -> box (ThirdPartyInteger(BigInteger.Zero))), assertExact 0m
      "ThirdPartyInteger 2^96 - 1", (fun () -> box (ThirdPartyInteger(BigInteger.Parse "79228162514264337593543950335"))), assertExact 79228162514264337593543950335m
      "ThirdPartyInteger 2^100 + 2^47 + 1", (fun () -> box (ThirdPartyInteger(BigInteger.Pow(BigInteger(2), 100) + BigInteger.Pow(BigInteger(2), 47) + BigInteger.One))),
      assertInexact (two 100.0 + two 48.0)
      "ThirdPartyFloat 2.5", (fun () -> box (ThirdPartyFloat(2.5))), assertInexact 2.5
      "ThirdPartyFloat 0.1", (fun () -> box (ThirdPartyFloat(0.1))), assertInexact 0.1
      "ThirdPartyFloat -0.375", (fun () -> box (ThirdPartyFloat(-0.375))), assertInexact -0.375
      "ThirdPartyFloat smallest subnormal", (fun () -> box (ThirdPartyFloat(Double.Epsilon))), assertInexact Double.Epsilon
      "ThirdPartyFloat largest double", (fun () -> box (ThirdPartyFloat(Double.MaxValue))), assertInexact Double.MaxValue
      // JYRO-ADR-158 (a2): a subclass of an unsealed number class is a number, read as
      // its base (JYRO-DEF-625); a class that implements the interface for itself and
      // for its base is a number, read as itself (JYRO-DEF-627).
      "DerivedInteger 5", (fun () -> box (DerivedInteger(BigInteger(5)))), assertExact 5m
      "DerivedInteger -7", (fun () -> box (DerivedInteger(BigInteger(-7)))), assertExact -7m
      "DerivedInteger 2^100 + 2^47 + 1", (fun () -> box (DerivedInteger(BigInteger.Pow(BigInteger(2), 100) + BigInteger.Pow(BigInteger(2), 47) + BigInteger.One))),
      assertInexact (two 100.0 + two 48.0)
      "BaseInteger 5", (fun () -> box (BaseInteger(BigInteger(5)))), assertExact 5m
      "SelfAndBaseInteger 7", (fun () -> box (SelfAndBaseInteger(BigInteger(7)))), assertExact 7m
      "SelfAndBaseInteger 2^96 - 1", (fun () -> box (SelfAndBaseInteger(BigInteger.Parse "79228162514264337593543950335"))), assertExact 79228162514264337593543950335m
      "DerivedFloat 2.5", (fun () -> box (DerivedFloat(2.5))), assertInexact 2.5
      "DerivedFloat 0.1", (fun () -> box (DerivedFloat(0.1))), assertInexact 0.1
      "DerivedFloat smallest subnormal", (fun () -> box (DerivedFloat(Double.Epsilon))), assertInexact Double.Epsilon
      "BaseFloat -0.375", (fun () -> box (BaseFloat(-0.375))), assertInexact -0.375
      // JYRO-ADR-157 (b1): a JsonValue made from a .NET number is that number, as
      // FromObject converts it passed directly; one holding a JsonElement is that JSON (JYRO-DEF-620).
      "JsonValue.Create(0.1), a double", (fun () -> box (JsonValue.Create(0.1))), assertInexact 0.1
      "JsonValue.Create(0.1f), a float", (fun () -> box (JsonValue.Create(0.1f))), assertInexact (float 0.1f)
      "JsonValue.Create(0.1m), a decimal", (fun () -> box (JsonValue.Create(0.1m))), assertExact 0.1m
      "JsonValue.Create(5), an int", (fun () -> box (JsonValue.Create(5))), assertExact 5m
      "JsonValue of BigInteger 5", (fun () -> jsonValueOf (BigInteger(5))), assertExact 5m
      "JsonValue of ThirdPartyFloat 2.5", (fun () -> jsonValueOf (ThirdPartyFloat(2.5))), assertInexact 2.5
      "JsonValue of a JsonElement 0.1", (fun () -> box (JsonValue.Create(JsonDocument.Parse("0.1").RootElement))), assertExact 0.1m
      // JYRO-ADR-157 (b2): a JValue is its Value converted by the walk (JYRO-DEF-623).
      "JValue 5, a long", (fun () -> box (JValue(5L))), assertExact 5m
      "JValue 0.1, a double", (fun () -> box (JValue(0.1))), assertInexact 0.1
      "JValue 0.1m, a decimal", (fun () -> box (JValue(0.1m))), assertExact 0.1m
      "JToken.Parse of 5", (fun () -> box (JToken.Parse "5")), assertExact 5m
      "JValue of BigInteger 10^30", (fun () -> box (JValue(box (BigInteger.Pow(BigInteger(10), 30))))), assertInexact (dbl "1e30") ]

let hostNumberData () : obj[] seq =
    seq {
        for (name, _, _) in hostNumbers () do
            for (p, _) in hostObjectPaths do
                yield [| box name; box p |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof hostNumberData)>]
let ``JYRO-REQ-88 every dotnet integer and binary floating-point type is a number, not an object or text`` (name: string, path: string) =
    let (_, make, check) = hostNumbers () |> List.find (fun (n, _, _) -> n = name)
    let convert = hostObjectPaths |> List.find (fun (n, _) -> n = path) |> snd
    let v = convert (make ())
    if v.ValueType <> JyroValueType.Number then
        Values.fail (sprintf "%s through %s became a %A, %s, not a number" name path v.ValueType (Values.toJson v))
    check v

/// Floats from outside the BCL whose writers do not follow .NET's own conventions
/// (JYRO-ADR-158 (a3), (a4)). Each must be its value: a binary one as the nearest
/// double, a decimal-radix one as an exact number when System.Decimal holds it and
/// otherwise its nearest double.
let private conventionFloats () : (string * (unit -> obj) * (JyroValue -> unit)) list =
    let ieee (d: float) = fun () -> box (IeeeConventionFloat(d))
    let dec (negative: bool) (c: string) (q: int) = fun () -> box (DecimalRadixFloat.Of(negative, BigInteger.Parse c, q))
    let two (n: float) = Math.Pow(2.0, n)
    // IEEE's least exponent for a subnormal, and 52 for the significand length (JYRO-DEF-626, JYRO-DEF-629).
    [ "IEEE writers, the smallest subnormal", ieee Double.Epsilon, assertInexact Double.Epsilon
      "IEEE writers, 3 times the smallest subnormal", ieee (3.0 * Double.Epsilon), assertInexact (3.0 * Double.Epsilon)
      "IEEE writers, the largest subnormal", ieee (BitConverter.Int64BitsToDouble 0x000F_FFFF_FFFF_FFFFL), assertInexact (BitConverter.Int64BitsToDouble 0x000F_FFFF_FFFF_FFFFL)
      "IEEE writers, the smallest normal", ieee (two -1022.0), assertInexact (two -1022.0)
      "IEEE writers, 2.5", ieee 2.5, assertInexact 2.5
      "IEEE writers, 0.1", ieee 0.1, assertInexact 0.1
      "IEEE writers, -0.375", ieee -0.375, assertInexact -0.375
      "IEEE writers, 1", ieee 1.0, assertInexact 1.0
      "IEEE writers, the largest double", ieee Double.MaxValue, assertInexact Double.MaxValue
      // Radix 10: c * 10^q, read as c * 10^(ILogB - (digits(c) - 1)) (JYRO-DEF-628).
      "radix 10, 15e-1", dec false "15" -1, assertExact 1.5m
      "radix 10, 1500e-3", dec false "1500" -3, assertExact 1.5m
      "radix 10, -25e-2", dec true "25" -2, assertExact -0.25m
      "radix 10, 7e3", dec false "7" 3, assertExact 7000m
      "radix 10, 1e-28", dec false "1" -28, assertExact 0.0000000000000000000000000001m
      "radix 10, 1e-40, not a decimal", dec false "1" -40, assertInexact (dbl "1e-40")
      "radix 10, 123456789012345678901234567890 (past 2^96 - 1)", dec false "123456789012345678901234567890" 0,
      assertInexact (dbl "123456789012345678901234567890")
      "radix 10, -9999999999999999e292", dec true "9999999999999999" 292, assertInexact (dbl "-9999999999999999e292") ]

let conventionFloatData () : obj[] seq =
    seq {
        for (name, _, _) in conventionFloats () do
            for (p, _) in hostObjectPaths do
                yield [| box name; box p |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-48")>]
[<MemberData(nameof conventionFloatData)>]
let ``JYRO-REQ-48 a host float outside the BCL is its value, whatever its exponent convention, significand length or radix`` (name: string, path: string) =
    let (_, make, check) = conventionFloats () |> List.find (fun (n, _, _) -> n = name)
    let convert = hostObjectPaths |> List.find (fun (n, _) -> n = path) |> snd
    let v = convert (make ())
    if v.ValueType <> JyroValueType.Number then
        Values.fail (sprintf "%s through %s became a %A, %s, not a number" name path v.ValueType (Values.toJson v))
    check v

/// The fixtures write what JYRO-ADR-158's cases need, so the rows above test the
/// conventions they name and not .NET's.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-48")>]
let ``JYRO-REQ-48 the host float fixtures write IEEE's conventions and radix 10`` () =
    let e = IeeeConventionFloat(Double.Epsilon)
    let buffer = Array.zeroCreate<byte> 8
    Assert.Equal(52, e.GetSignificandBitLength())
    let ok1, _ = e.TryWriteExponentLittleEndian(Span<byte>(buffer))
    Assert.True(ok1)
    Assert.Equal(-1022s, BitConverter.ToInt16(buffer, 0))
    let ok2, _ = e.TryWriteSignificandLittleEndian(Span<byte>(buffer))
    Assert.True(ok2)
    Assert.Equal(1UL, BitConverter.ToUInt64(buffer, 0))
    Assert.Equal(-1074, Double.ILogB Double.Epsilon)
    // .NET's own double writes -1023 and 53 here (BaseFloat writes as double does).
    let b = BaseFloat(Double.Epsilon)
    let ok3, _ = b.TryWriteExponentLittleEndian(Span<byte>(buffer))
    Assert.True(ok3)
    Assert.Equal(-1023s, BitConverter.ToInt16(buffer, 0))
    Assert.Equal(53, b.GetSignificandBitLength())
    let one = IeeeConventionFloat(1.0)
    let ok4, _ = one.TryWriteSignificandLittleEndian(Span<byte>(buffer))
    Assert.True(ok4)
    Assert.Equal(1UL <<< 52, BitConverter.ToUInt64(buffer, 0))
    let x = DecimalRadixFloat.Of(false, BigInteger(15), -1)
    Assert.Equal(0, DecimalRadixFloat.ILogB x)
    Assert.Equal(10, DecimalRadixFloat.Radix)

// JYRO-ADR-157 (b1): a JsonValue that holds a .NET value other than a JsonElement
// is that value, converted by the walk exactly as FromObject converts it passed
// directly: its shape, its numbers' representation, and a NaN, an infinity, a
// depth or a cycle in it give what the value gives alone (JYRO-DEF-620).

/// What one conversion gave, as text that tells exact from inexact numbers:
/// a value, a JyroRuntimeException's code, a run's codes, or another exception.
let rec private shapeOf (v: JyroValue) : string =
    match v with
    | :? JyroNumber -> (if Values.isExact v then "exact " else "inexact ") + v.ToJson()
    | :? JyroArray as a -> "[" + String.Join(",", a |> Seq.map shapeOf) + "]"
    | :? JyroObject as o -> "{" + String.Join(",", o |> Seq.map (fun kv -> kv.Key + ":" + shapeOf kv.Value)) + "}"
    | _ -> v.ToJson()

let private outcome (convert: unit -> JyroValue) : string =
    try "value " + shapeOf (convert ())
    with
    | :? JyroRuntimeException as j -> "code " + string j.Code
    | e -> "exception " + e.GetType().FullName + ": " + e.Message

let private runOutcome (r: JyroResult<JyroValue>) : string =
    if r.IsSuccess then "value " + shapeOf (Script.output r |> Values.prop "v")
    else "codes " + String.Join(",", Script.codes r |> List.map string)

/// Every host path of 30.2.17 and 30.2.18, each giving what one host value became.
let private wrapPaths : (string * (obj -> string)) list =
    let echo = "Output.v = Input"
    let tryRun (run: unit -> JyroResult<JyroValue>) =
        try runOutcome (run ()) with e -> "exception " + e.GetType().FullName + ": " + e.Message
    [ "JyroValue.FromObject", (fun o -> outcome (fun () -> JyroValue.FromObject o))
      "nested in a List", (fun o -> outcome (fun () -> JyroValue.FromObject(box (List<obj>([ o ])))))
      "nested in an array", (fun o -> outcome (fun () -> JyroValue.FromObject(box [| o |])))
      "JyroValue.FromObject dictionary value", (fun o -> outcome (fun () -> JyroValue.FromObject(Dictionary<string, obj>(dict [ "k", o ]))))
      "JyroValue.FromObject property", (fun o -> outcome (fun () -> JyroValue.FromObject(Wrap(o))))
      "JyroRunner.WithInput(Object)", (fun o -> tryRun (fun () -> JyroRunner(Script.valueOf (Script.compile echo)).WithInput(o).Execute()))
      "JyroScript.ExecuteScript", (fun o -> tryRun (fun () -> executeScript(echo, o))) ]

/// The depths of the nesting cases: past System.Text.Json's default maximum of 64
/// and within the engine's (100.7), and past the engine's.
let private withinDepth = 100
let private beyondDepth = 200

/// .NET values, each with the same value made into a JsonValue by JsonValue.Create<T>.
let private wrappedValues () : (string * (unit -> obj * obj)) list =
    let both (x: 'T) = box x, jsonValueOf x
    let intDict () =
        let d = Dictionary<int, int>()
        d.[2] <- 20
        d.[1] <- 10
        d
    [ "a class with a property and a field", (fun () -> both (propAndField 1 2))
      "a getter that throws", (fun () -> both (Guarded(false)))
      "an enum", (fun () -> both DayOfWeek.Monday)
      "BigInteger 5", (fun () -> both (BigInteger(5)))
      "Dictionary<int, int>", (fun () -> both (intDict ()))
      "DateTime", (fun () -> both (DateTime(2026, 10, 7, 8, 30, 0, DateTimeKind.Utc)))
      "a record-like object", (fun () -> both (RecordPair<int>(1, 2)))
      "a ValueTuple", (fun () -> both (struct (1, "s")))
      "double 0.1", (fun () -> both 0.1)
      "decimal 0.1", (fun () -> both 0.1m)
      "a List<double>", (fun () -> both (List<float>([ 0.1; 2.0 ])))
      "ThirdPartyInteger 5", (fun () -> both (ThirdPartyInteger(BigInteger(5))))
      "arrays nested 100 deep", (fun () -> both (nestedArrays withinDepth))
      "arrays nested 200 deep", (fun () -> both (nestedArrays beyondDepth))
      "a cyclic POCO", (fun () -> both (cyclicNode ()))
      "double[] {NaN}", (fun () -> both [| Double.NaN |])
      "a POCO with +Infinity", (fun () -> both (Wrap(box Double.PositiveInfinity)))
      "NFloat.NaN", (fun () -> both (Runtime.InteropServices.NFloat(Double.NaN))) ]

let wrappedValueData () : obj[] seq =
    seq {
        for (name, _) in wrappedValues () do
            for (p, _) in wrapPaths do
                yield [| box name; box p |]
    }

[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 the nesting cases lie past the JSON serializer's default depth and either side of the engine's`` () =
    Assert.True(64 < withinDepth && withinDepth + 3 < maxDepth () && maxDepth () < beyondDepth,
                sprintf "the cases need 64 < %d < %d < %d" withinDepth (maxDepth ()) beyondDepth)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof wrappedValueData)>]
let ``JYRO-REQ-88 a JsonValue made from a dotnet value converts exactly as that value passed directly`` (name: string, path: string) =
    let (_, make) = wrappedValues () |> List.find (fun (n, _) -> n = name)
    let convert = wrapPaths |> List.find (fun (n, _) -> n = path) |> snd
    let direct, wrapped = make ()
    let expected = convert direct
    let actual = convert wrapped
    if actual.StartsWith "exception " then
        Values.fail (sprintf "%s in a JsonValue through %s threw a foreign exception: %s" name path actual)
    Assert.Equal(expected, actual)

/// Values whose outcome the comparison above leaves to the value passed directly,
/// pinned here so the comparison cannot pass by both sides failing alike.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 a JsonValue's value keeps the walk's outcome for depth, cycles and numbers`` () =
    let fromWrapped (x: 'T) = outcome (fun () -> JyroValue.FromObject(jsonValueOf x))
    Assert.StartsWith("value ", fromWrapped (nestedArrays withinDepth))
    Assert.Equal("code " + string MessageCode.ValueNestingLimitExceeded, fromWrapped (nestedArrays beyondDepth))
    Assert.Equal("code " + string MessageCode.ValueNestingLimitExceeded, fromWrapped (cyclicNode ()))
    Assert.Equal("value inexact 0.1", fromWrapped 0.1)
    Assert.Equal("value exact 0.1", fromWrapped 0.1m)
    Assert.Equal("value exact 5", fromWrapped (BigInteger(5)))
    Assert.Equal("value {P:exact 1,F:exact 2}", fromWrapped (propAndField 1 2))
    Assert.Equal("value {Ok:false}", fromWrapped (Guarded(false)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-88")>]
let ``JYRO-REQ-88 an exception from a required member of a host number type passes out of FromObject unchanged`` () =
    // JYRO-ADR-157 (a1): as JYRO-ADR-156 (e) says of a broken host container.
    let broken = ThirdPartyInteger.Broken(BigInteger(5))
    for convert in [ (fun () -> JyroValue.FromObject(box broken))
                     (fun () -> JyroValue.FromObject(box [| box 1; box broken |]))
                     (fun () -> JyroValue.FromObject(jsonValueOf broken)) ] do
        let ex = Assert.ThrowsAny<exn>(fun () -> convert () |> ignore)
        Assert.IsType<BrokenNumberException>(ex) |> ignore
        Assert.Equal("the host's integer type is broken", ex.Message)

let private stream (text: string) = new MemoryStream(Encoding.UTF8.GetBytes text) :> Stream

let private jsonObjectPaths : (string * (string -> JyroValue)) list =
    let viaRunner (read: JyroRunner -> JyroRunner) =
        let program = Script.valueOf (Script.compile "Output.v = Input")
        (read (JyroRunner(program))).Execute() |> Script.output |> Values.prop "v"
    [ "WithInputJson(String)", (fun t -> viaRunner (fun r -> r.WithInputJson(t: string)))
      "WithInputJson(Stream)", (fun t -> viaRunner (fun r -> r.WithInputJson(stream t)))
      "ExecuteScriptWithJson", (fun t -> executeScriptWithJson("Output.v = Input", t) |> Script.output |> Values.prop "v")
      "JyroValue.FromJson", (fun t -> JyroValue.FromJson t)
      "JyroValue.FromJson(Stream)", (fun t -> JyroValue.FromJson(stream t))
      "JyroValue.FromJsonAsync", (fun t -> JyroValue.FromJsonAsync(stream t, System.Threading.CancellationToken.None).Result)
      "script FromJson",
      (fun t -> Script.runInput (Values.obj [ "t", Values.str t ]) "Output.v = FromJson(Input.t)" |> Script.output |> Values.prop "v") ]

let jsonObjectPathNames () : obj[] seq = jsonObjectPaths |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof jsonObjectPathNames)>]
let ``JYRO-REQ-88 An object made from JSON text has the order of the keys in the text`` (path: string) =
    let read = jsonObjectPaths |> List.find (fun (n, _) -> n = path) |> snd
    Assert.Equal<string list>([ "z"; "a"; "m" ], keysOf (read "{\"z\":1,\"a\":2,\"m\":3}"))
    let nested = read "{\"o\":{\"z\":1,\"a\":2,\"m\":3}}" |> Values.prop "o"
    Assert.Equal<string list>([ "z"; "a"; "m" ], keysOf nested)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-88")>]
[<MemberData(nameof jsonObjectPathNames)>]
let ``JYRO-REQ-88 A repeated JSON key has its first position and its last value`` (path: string) =
    let read = jsonObjectPaths |> List.find (fun (n, _) -> n = path) |> snd
    let o = read "{\"a\":1,\"b\":2,\"a\":3}"
    Assert.Equal<string list>([ "a"; "b" ], keysOf o)
    Assert.Equal("{\"a\":3,\"b\":2}", Values.toJson o)

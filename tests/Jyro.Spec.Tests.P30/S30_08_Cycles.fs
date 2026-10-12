/// 30.8 Cycles and bounded traversal.
module RealTree.Jyro.Spec.Tests.P30.S30_08_Cycles

open System
open System.IO
open System.Text
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

/// `WriteJson` of a value to a memory stream, as the host would.
let private writeJson (v: JyroValue) : obj =
    use ms = new MemoryStream()
    do
        use w = new Utf8JsonWriter(ms)
        v.WriteJson(w)
        w.Flush()
    box ms.Length

// ---------------------------------------------------------------- 30.8.1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-98")>]
[<InlineData("var a = []\na[0] = a\nOutput.t = TypeOf(a[0])")>]
[<InlineData("var a = [1]\na[Length(a)] = a\nOutput.t = TypeOf(a[1])")>]
[<InlineData("var a = [1]\na[0] = a\nOutput.t = TypeOf(a[0])")>]
[<InlineData("var o = {}\no.self = o\nOutput.t = TypeOf(o.self)")>]
[<InlineData("var o = {}\no[\"k\"] = o\nOutput.t = TypeOf(o.k)")>]
[<InlineData("var x = []\nvar y = [x]\nx[0] = y\nOutput.t = TypeOf(x[0][0])")>]
[<InlineData("Output.c = {}\nOutput.c.me = Output.c\nOutput.t = TypeOf(Output.c.me)")>]
[<InlineData("var o = {a: {}}\no.a.up = o\nOutput.t = TypeOf(o.a.up.a)")>]
let ``The assignment that closes a cycle is not rejected`` (source: string) =
    let r = Script.run (source + "\nOutput.ok = true")
    let out = Script.value r
    Assert.True(Script.ok r, Script.describe r)
    match out with
    | Some o ->
        Assert.True(Values.asBool (Values.prop "ok" o))
        Assert.Contains(Values.asString (Values.prop "t" o), [ "array"; "object" ])
    | None -> Values.fail "no Output"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-98")>]
let ``A host-supplied Output may be made to contain itself`` () =
    let output = JyroObject()
    let r = Script.runWith { Script.defaults with Output = Some (output :> JyroValue) } "Output.self = Output\nOutput.ok = true"
    Assert.True(Script.ok r, Script.describe r)
    // The run's Output is a copy of the seed (JYRO-REQ-1929): the cycle is in the result, not in the host's object.
    let result = Script.output r
    Assert.Same(result, result.GetProperty("self"))

// ---------------------------------------------------------------- 30.8.2

let private cyclic = "var a = [1]\na[1] = a\nvar o = {n: 1}\no.self = o\n"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1806")>]
[<InlineData("func id(p)\n return p\nend\nOutput.r = TypeOf(id(a)) & TypeOf(id(o))")>]
[<InlineData("var b = a\nvar arr = [a, o]\nvar h = {a: a, o: o}\nOutput.r = TypeOf(arr[0]) & TypeOf(h.o)")>]
[<InlineData("Output.r = [TypeOf(a), TypeOf(o)]")>]
[<InlineData("Output.r = [ToBoolean(a), ToBoolean(o)]")>]
[<InlineData("if a then Output.r = 1 end\nwhile o do Output.r = 2\n break\nend")>]
[<InlineData("Output.r = [a and 1, o or 1, not a, a ? 1 : 2]")>]
[<InlineData("Output.r = [a is array, o is object, a is not null]")>]
[<InlineData("Output.r = [Length(a), Length(o)]")>]
[<InlineData("Output.r = TypeOf(First(a)) & TypeOf(Last(a))")>]
[<InlineData("var n = 0\nforeach e in a do n = n + 1 end\nforeach v in o do n = n + 1 end\nOutput.r = n")>]
[<InlineData("Output.r = [Keys(o), HasProperty(o, \"self\")]")>]
[<InlineData("Output.r = Length(FlattenOnce([a]))")>]
[<InlineData("Output.r = Map(a, x => 1)")>]
[<InlineData("Output.r = Length(Values(o))")>]
let ``JYRO-REQ-1806 Using a cyclic value where no walk is needed is not rejected`` (usage: string) =
    let r = Script.run (cyclic + usage)
    Assert.True(Script.ok r, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1806")>]
let ``JYRO-REQ-1806 An execution whose Output is cyclic succeeds`` () =
    let r = Script.run (cyclic + "Output.a = a\nOutput.o = o")
    Assert.True(Script.ok r, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1806")>]
let ``JYRO-REQ-1806 An execution succeeds when Output itself is assigned a cyclic value`` () =
    let r = Script.run "var a = [1]\na[1] = a\nOutput = a"
    Assert.True(Script.ok r, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1806")>]
let ``JYRO-REQ-1806 Each operation that does not walk gives the same result for a cyclic value as for a plain one`` () =
    let operations =
        "func id(p)\n return p\nend\n"
        + "var store = {}\nstore.k = v\nvar arr = []\narr[0] = v\n"
        + "var back = id(v)\n"
        + "Output.r = [TypeOf(v), v is array, v is object, v is not null, TypeOf(store.k), TypeOf(arr[0]), TypeOf(back), Length(arr), TypeOf(v[0]), TypeOf(v[1])]"
    let cyclicArray = "var v = [[], 1]\nv[0] = v\n"
    let plainArray = "var v = [[], 1]\nv[0] = [2]\n"
    let withoutWalk = operations
    let rc = Script.run (cyclicArray + withoutWalk)
    let rp = Script.run (plainArray + withoutWalk)
    Assert.True(Script.ok rc, Script.describe rc)
    Assert.True(Script.ok rp, Script.describe rp)
    Assert.Equal(Values.toJson (Values.prop "r" (Script.output rp)), Values.toJson (Values.prop "r" (Script.output rc)))
    let ro =
        Script.run "var o = {n: 1}\no.self = o\nvar e = o.self.self.n\nvar f = o[\"self\"]\nOutput.r = [e, TypeOf(f), f.n, o.self.self.self is object]"
    Assert.True(Script.ok ro, Script.describe ro)
    Assert.Equal("[1,\"object\",1,true]", Values.toJson (Values.prop "r" (Script.output ro)))

// ---------------------------------------------------------------- 30.8.3 and 30.8.4

/// Scripts that make each kind of value; the value is `v`, and `w` is an equal
/// but separate value where one can be made.
let private makers =
    [ "cyclic array", "var v = [1]\nv[1] = v\nvar w = [1]\nw[1] = w\n"
      "cyclic object", "var v = {n: 1}\nv.self = v\nvar w = {n: 1}\nw.self = w\n"
      "two-step cycle", "var x = []\nvar y = [x]\nx[0] = y\nvar v = x\nvar w = y\n"
      "cycle behind a prefix", "var c = [1]\nc[1] = c\nvar v = [1, {x: c}]\nvar w = [1, {x: c}]\n" ]

let private deepMaker =
    "var v = []\nvar w = []\nvar i = 0\nwhile i < 100000 do\n v = [v]\n w = [w]\n i = i + 1\nend\n"

/// Each structural walk a script can do, as a statement over `v` and `w`.
let private walks =
    [ "==", "Output.r = v == w"
      "!=", "Output.r = v != w"
      "== itself", "Output.r = v == v"
      "ToString", "Output.r = ToString(v)"
      "exit", "exit v"
      "fail", "fail v"
      "ToJson", "Output.r = ToJson(v)"
      "Clone", "Output.r = TypeOf(Clone(v))"
      "Flatten", "Output.r = Flatten([v])"
      "Distinct", "Output.r = Length(Distinct([v, w]))"
      "IndexOf", "Output.r = IndexOf([1, v], w)"
      "Contains", "Output.r = Contains([1, v], w)"
      "switch", "switch v do\ncase w then Output.r = 1\ndefault then Output.r = 2\nend"
      "ValidateSchema", "Output.r = ValidateSchema(v, {type: \"array\"})"
      "Diff", "Output.r = TypeOf(Diff({k: v}, {k: w}))" ]

let walkData () : obj[] seq =
    seq {
        for (m, _) in makers do
            for (w, _) in walks do
                yield [| box m; box w |]
    }

let deepWalkData () : obj[] seq = walks |> Seq.map (fun (w, _) -> [| box w |])

let private walk name = walks |> List.find (fun (n, _) -> n = name) |> snd
let private maker name = makers |> List.find (fun (n, _) -> n = name) |> snd

[<Theory>]
[<Trait("requirement", "JYRO-REQ-100")>]
[<MemberData(nameof walkData)>]
let ``Every walk of a cyclic value stops`` (makerName: string, walkName: string) =
    match within 10.0 (fun () -> Script.runWith bigLimits (maker makerName + walk walkName)) with
    | Ok _ -> ()
    | Error e -> Values.fail (sprintf "the walk threw to the host: %s" e.Message)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-100")>]
[<MemberData(nameof deepWalkData)>]
let ``Every walk of a value nested 100000 deep stops with a result or ValueNestingLimitExceeded`` (walkName: string) =
    match within 10.0 (fun () -> Script.runWith bigLimits (deepMaker + walk walkName)) with
    | Ok r ->
        if not (Script.ok r) then
            Assert.Contains(MessageCode.ValueNestingLimitExceeded, Script.codes r)
    | Error e -> Values.fail (sprintf "the walk threw to the host: %s" e.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-100")>]
let ``FromJson of 100000 nested brackets stops`` () =
    let text = String('[', 100000) + String(']', 100000)
    let input = Values.obj [ "t", Values.str text ]
    match within 10.0 (fun () -> Script.runWith { bigLimits with Input = ValueInput input } "Output.r = TypeOf(FromJson(Input.t))") with
    | Ok r -> if not (Script.ok r) then Assert.Contains(MessageCode.ValueNestingLimitExceeded, Script.codes r)
    | Error e -> Values.fail (sprintf "FromJson threw to the host: %s" e.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-100")>]
let ``The host's walks of a value nested 100000 deep stop`` () =
    let r = Script.runWith bigLimits (deepMaker + "Output.v = v")
    let out = match Script.value r with Some o -> o | None -> Values.fail (Script.describe r)
    let hostWalks : (string * (unit -> obj)) list =
        [ "ToJson", (fun () -> box (out.ToJson()))
          "WriteJson", (fun () -> writeJson out)
          "ToObjectValue", (fun () -> out.ToObjectValue())
          "ToDictionary", (fun () -> box ((out :?> JyroObject).ToDictionary())) ]
    for (name, run) in hostWalks do
        match within 10.0 run with
        | Ok _ -> ()
        | Error e ->
            try assertCarriesCode MessageCode.ValueNestingLimitExceeded e
            with x -> Values.fail (name + ": " + x.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-100")>]
let ``The host's walks of a cyclic Output stop`` () =
    let r = Script.run "var a = [1]\na[1] = a\nOutput.a = a"
    let out = match Script.value r with Some o -> o | None -> Values.fail (Script.describe r)
    let hostWalks : (unit -> obj) list =
        [ (fun () -> box (out.ToJson()))
          (fun () -> writeJson out)
          (fun () -> out.ToObjectValue())
          (fun () -> box ((out :?> JyroObject).ToDictionary())) ]
    for run in hostWalks do
        match within 10.0 run with
        | Ok _ -> ()
        | Error _ -> ()

/// The walks of 30.8.4 that reach the cycle. `Flatten` descends arrays only
/// (92.12.2), so it reaches no cycle that runs through an object.
let cyclicWalkData () : obj[] seq =
    seq {
        for (m, _) in makers do
            for (w, _) in walks do
                if not (w = "Flatten" && (m = "cyclic object" || m = "cycle behind a prefix")) then
                    yield [| box m; box w |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1950")>]
[<MemberData(nameof cyclicWalkData)>]
let ``A walk of a cyclic value raises ValueNestingLimitExceeded`` (makerName: string, walkName: string) =
    match within 10.0 (fun () -> Script.runWith bigLimits (maker makerName + walk walkName)) with
    | Ok r -> Script.expectError MessageCode.ValueNestingLimitExceeded r
    | Error e -> Values.fail (sprintf "the walk threw to the host: %s" e.Message)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1950")>]
[<InlineData("ToJson")>]
[<InlineData("WriteJson")>]
let ``A host walk of a cyclic Output fails with ValueNestingLimitExceeded`` (which: string) =
    let r = Script.run "var a = [1]\na[1] = a\nOutput.a = a"
    let out = match Script.value r with Some o -> o | None -> Values.fail (Script.describe r)
    let run () =
        match which with
        | "ToJson" -> box (out.ToJson())
        | _ -> writeJson out
    match within 10.0 run with
    | Ok v -> Values.fail (sprintf "%s of a cyclic value returned %A" which v)
    | Error e -> assertCarriesCode MessageCode.ValueNestingLimitExceeded e

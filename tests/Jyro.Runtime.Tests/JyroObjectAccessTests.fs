module RealTree.Jyro.Tests.JyroObjectAccessTests

open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// `JyroObject` carried two byte-identical lookup members, `TryGet` and
// `TryGetValue`; only `TryGetValue` was documented (README "JyroObject also
// supports ... TryGetValue"), and neither had a test. `TryGet` has been
// removed. These pin the survivor's contract - in particular the miss case,
// which yields `JyroNull.Instance` rather than `null`, so a C# host that
// ignores the bool still gets a usable value instead of an NRE.

[<Fact>]
let ``TryGetValue returns true and the value for a present key`` () =
    let o = JyroObject()
    o.["a"] <- JyroNumber(1.0)
    match o.TryGetValue("a") with
    | true, v -> Assert.Equal(1.0, (v :?> JyroNumber).Value)
    | false, _ -> Assert.Fail("expected 'a' to be found")

[<Fact>]
let ``TryGetValue returns false and JyroNull for a missing key`` () =
    let o = JyroObject()
    o.["a"] <- JyroNumber(1.0)
    match o.TryGetValue("nope") with
    | true, _ -> Assert.Fail("did not expect 'nope' to be found")
    | false, v ->
        Assert.NotNull(v)
        Assert.Same(JyroNull.Instance, v)

[<Fact>]
let ``TryGetValue on an empty object is a miss, not a throw`` () =
    let o = JyroObject()
    let found, v = o.TryGetValue("anything")
    Assert.False(found)
    Assert.Same(JyroNull.Instance, v)

[<Fact>]
let ``TryGetValue reflects Remove and Clear`` () =
    let o = JyroObject()
    o.["a"] <- JyroNumber(1.0)
    o.["b"] <- JyroNumber(2.0)
    Assert.True(o.Remove("a"))
    Assert.False(fst (o.TryGetValue("a")))
    Assert.True(fst (o.TryGetValue("b")))
    o.Clear()
    Assert.False(fst (o.TryGetValue("b")))

[<Fact>]
let ``TryGetValue sees a null assignment as JyroNull`` () =
    // SetProperty normalizes a null reference to JyroNull.Instance, so the
    // hit case can never hand a C# host a null JyroValue either.
    let o = JyroObject()
    o.SetProperty("a", Unchecked.defaultof<JyroValue>)
    match o.TryGetValue("a") with
    | true, v -> Assert.Same(JyroNull.Instance, v)
    | false, _ -> Assert.Fail("expected 'a' to be present")

// -- ToDictionary -----------------------------------------------------------
// `ToDictionary` used to restate `ToObjectValue`'s walk verbatim, minus the
// ValueRecursionGuard. It now delegates. It is documented host API (the
// "ToDictionary Helper" section of the C# and F# host-integration chapters),
// so the typed return is the contract - these tests pin both the equivalence
// and the signature. The depth-budget half of the contract lives in
// Properties/CyclicValueProtection.fs.

/// One key per leaf kind the conversion has to handle.
let private sampleObject () =
    let o = JyroObject()
    o.["num"] <- JyroNumber(1.5)
    o.["str"] <- JyroString("hi")
    o.["bool"] <- JyroBoolean.True
    o.["null"] <- JyroNull.Instance
    let arr = JyroArray()
    arr.Add(JyroNumber(1.0))
    arr.Add(JyroNull.Instance)
    o.["arr"] <- arr
    let nested = JyroObject()
    nested.["k"] <- JyroString("v")
    o.["obj"] <- nested
    o.["emptyObj"] <- JyroObject()
    o

[<Fact>]
let ``ToDictionary agrees with ToObjectValue on keys, order and value types`` () =
    let viaDict = (sampleObject ()).ToDictionary()
    let viaObject = (sampleObject () :> JyroValue).ToObjectValue() :?> Dictionary<string, obj>
    Assert.Equal(viaObject.Count, viaDict.Count)
    Assert.Equal<string list>(List.ofSeq viaObject.Keys, List.ofSeq viaDict.Keys)
    for k in viaObject.Keys do
        let a, b = viaDict.[k], viaObject.[k]
        if isNull a || isNull b then
            Assert.True(isNull a && isNull b, sprintf "null mismatch on key '%s'" k)
        else
            Assert.Equal(b.GetType(), a.GetType())

[<Fact>]
let ``ToDictionary returns the concrete dictionary type without a cast`` () =
    // Pins the signature. The documented C# usage is
    // `Dictionary<string, object> dict = obj.ToDictionary();` - loosening the
    // return type to obj or IDictionary would break that, and this line.
    let d: Dictionary<string, obj> = (sampleObject ()).ToDictionary()
    Assert.Equal(7, d.Count)

[<Fact>]
let ``ToDictionary returns a fresh dictionary that does not alias the object`` () =
    let o = sampleObject ()
    let first = o.ToDictionary()
    let second = o.ToDictionary()
    Assert.False(obj.ReferenceEquals(first, second))
    first.["injected"] <- box 99
    Assert.False((o :> IReadOnlyDictionary<string, JyroValue>).ContainsKey("injected"))
    Assert.False((o.ToDictionary()).ContainsKey("injected"))

[<Fact>]
let ``ToDictionary works on empty and frozen objects`` () =
    Assert.Empty((JyroObject()).ToDictionary())
    let frozen = sampleObject ()
    frozen.Freeze() |> ignore
    let d = frozen.ToDictionary()
    Assert.Equal(7, d.Count)
    // The source being read-only must not make the extracted copy read-only.
    d.["added"] <- box 1
    Assert.Equal(8, d.Count)

// -- All five lookup doors agree, and a key is just a key -----------------
// `GetProperty` used to split on `.` and walk the result as a path, alone
// among the doors. A script never saw it - `o['a.b']` compiles to `GetIndex`,
// which matched literally - and it made `SetProperty` not the inverse of
// `GetProperty`. Specification clause 30.8 was amended to match: a key
// containing a dot is a key.

[<Fact>]
let ``every lookup door matches the key literally`` () =
    let o = JyroObject()
    let inner = JyroObject()
    inner.SetProperty("b", JyroNumber(7.0) :> JyroValue)
    o.SetProperty("a", inner :> JyroValue)
    // No door invents a traversal.
    Assert.True((o.GetProperty("a.b")).IsNull)
    Assert.True((o.["a.b"]).IsNull)
    let mutable found = JyroNull.Instance :> JyroValue
    Assert.False(o.TryGetValue("a.b", &found))
    // The nested value is still reachable the ordinary way.
    Assert.Equal(7.0, (o.GetProperty("a").GetProperty("b")).ToDouble())

[<Fact>]
let ``SetProperty is the inverse of GetProperty for a dotted key`` () =
    // Previously: SetProperty stored the literal key and GetProperty walked
    // past it, so the value written could not be read back.
    let o = JyroObject()
    o.SetProperty("x.y", JyroNumber(1.0) :> JyroValue)
    Assert.Equal(1.0, (o.GetProperty("x.y")).ToDouble())

[<Fact>]
let ``a dotted key round-trips through a script`` () =
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("var o = {}\no['a.b'] = 9\nOutput.v = o['a.b']"))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    Assert.Equal(9.0, (r.TryValue.Value.GetProperty("v")).ToDouble())

[<Fact>]
let ``a JyroArray is an IReadOnlyList`` () =
    let a = JyroArray()
    for n in [ 1.0; 2.0; 3.0 ] do a.Add(JyroNumber(n) :> JyroValue)
    let view = a :> System.Collections.Generic.IReadOnlyList<JyroValue>
    Assert.Equal(3, view.Count)
    Assert.Equal(2.0, (view.[1]).ToDouble())
    Assert.Equal(3, Seq.length view)

[<Fact>]
let ``a JyroObject is an IReadOnlyDictionary`` () =
    let o = JyroObject()
    o.SetProperty("a", JyroNumber(1.0) :> JyroValue)
    o.SetProperty("b", JyroNumber(2.0) :> JyroValue)
    let view = o :> System.Collections.Generic.IReadOnlyDictionary<string, JyroValue>
    Assert.Equal(2, view.Count)
    Assert.True(view.ContainsKey("a"))
    Assert.Equal<string list>([ "a"; "b" ], view.Keys |> List.ofSeq)
    Assert.Equal(2.0, (view.["b"]).ToDouble())

// -- The array gains the lookups the object already had -------------------

[<Fact>]
let ``TryGetItem distinguishes a miss from a stored null`` () =
    // The indexer yields JyroNull for both, which is the finding this pairs
    // with - it is left as it is, and this is the door that answers instead.
    let a = JyroArray()
    a.Add(JyroNull.Instance)
    let mutable value = JyroNull.Instance :> JyroValue
    Assert.True(a.TryGetItem(0, &value))
    Assert.True(value.IsNull)
    Assert.False(a.TryGetItem(1, &value))
    Assert.False(a.TryGetItem(-1, &value))

[<Fact>]
let ``IndexOf and Contains use the script's equality`` () =
    let a = JyroArray()
    a.Add(JyroString("x") :> JyroValue)
    a.Add(JyroNumber(2.0) :> JyroValue)
    // A distinct but equal value matches, as `==` would.
    Assert.Equal(0, a.IndexOf(JyroString("x") :> JyroValue))
    Assert.True(a.Contains(JyroNumber(2.0) :> JyroValue))
    Assert.Equal(-1, a.IndexOf(JyroString("absent") :> JyroValue))
    Assert.False(a.Contains(JyroBoolean.True :> JyroValue))

[<Fact>]
let ``AsFunction completes the As family`` () =
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output.f = x => x + 1"))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    let f = (r.TryValue.Value.GetProperty("f")).AsFunction()
    Assert.Equal(JyroValueType.Function, (f :> JyroValue).ValueType)
    Assert.Throws<JyroRuntimeException>(fun () -> (JyroNumber(1.0) :> JyroValue).AsFunction() |> ignore) |> ignore

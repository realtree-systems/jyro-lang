module RealTree.Jyro.Tests.HostConversionTests

open System
open System.Collections.Generic
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// The contract of JyroValue.FromObject, the one converter from host values (JYRO-REQ-1902),
// and the rule that separates it from the entry points that reject a null argument.

type Customer() =
    member _.Name = "Alice"
    member _.Age = 30

let private sampleDictionary () =
    let d = Dictionary<string, obj>()
    d.["a"] <- box 1
    d.["b"] <- box "two"
    d :> IDictionary<string, obj>

// -- The reject half of the contract ----------------------------------------

[<Fact>]
let ``FromJson of a stream rejects null with ArgumentNullException`` () =
    let ex = Assert.Throws<ArgumentNullException>(fun () -> JyroValue.FromJson(null: IO.Stream) |> ignore)
    Assert.Equal("utf8Json", ex.ParamName)

[<Fact>]
let ``FromJsonAsync rejects null at the call site, not as a faulted task`` () =
    // The check must sit outside the task { } block. Raising inside it would
    // produce a faulted Task that only throws at the await, so this very
    // call - with no await - would return normally and a host's try/catch
    // around the call site would catch nothing.
    let ex = Assert.Throws<ArgumentNullException>(fun () -> JyroValue.FromJsonAsync(null, Threading.CancellationToken.None) |> ignore)
    Assert.Equal("utf8Json", ex.ParamName)

[<Fact>]
let ``FromJson of a stream and FromJsonAsync agree on a real stream`` () =
    let json = """{"a":1,"b":[2,3]}"""
    let bytes = Text.Encoding.UTF8.GetBytes(json)
    use sync = new IO.MemoryStream(bytes)
    use async' = new IO.MemoryStream(bytes)
    let fromSync = JyroValue.FromJson(sync)
    let fromAsync = (JyroValue.FromJsonAsync(async', Threading.CancellationToken.None)).GetAwaiter().GetResult()
    Assert.Equal(fromSync.ToJson(), fromAsync.ToJson())
    Assert.True(fromSync.EqualsValue(fromAsync))

// -- The normalize half, which must NOT change ------------------------------

[<Fact>]
let ``entry points returning JyroValue normalize null to JyroNull`` () =
    // These return or store a JyroValue, so JyroNull is expressible and null
    // is normalized rather than rejected. This is the other side of the rule
    // the converter follows; if it ever changes, the rule needs revisiting.
    Assert.Same(JyroNull.Instance, JyroValue.FromObject(null))
    let arr = JyroArray()
    arr.Add(null)
    Assert.Same(JyroNull.Instance, arr.[0])
    let o = JyroObject()
    o.SetProperty("k", null)
    Assert.Same(JyroNull.Instance, o.GetProperty("k"))

// -- Rejection must not disturb the shared depth counter --------------------

/// The value nesting maximum of the reference implementation (JYRO-REQ-2042 states it: 128). The
/// guard that holds it is not public.
let private valueNestingMaximum = 128

/// A walk at exactly one below the cap, which succeeds only if the
/// thread-static guard counter is at zero when it starts.
let private walkAtDepthLimit () =
    let mutable current: IDictionary<string, obj> = Dictionary<string, obj>()
    for _ in 1 .. (valueNestingMaximum - 1) do
        let parent = Dictionary<string, obj>()
        parent.["child"] <- box current
        current <- parent
    JyroValue.FromObject(current) |> ignore

[<Fact>]
let ``a refused conversion leaves the depth counter untouched`` () =
    // A number that is not finite throws from inside the walk, after the guard
    // has charged its levels. A leak here would make legitimate deep walks
    // start failing after enough refused calls.
    walkAtDepthLimit ()
    for _ in 1 .. 50 do
        Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(box [| box 1; box Double.NaN |]) |> ignore) |> ignore
        Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(Dictionary<string, obj>(dict [ "k", box [| box Double.PositiveInfinity |] ])) |> ignore) |> ignore
    walkAtDepthLimit ()

[<Fact>]
let ``an enumerable that throws mid-iteration leaves the depth counter untouched`` () =
    // FromObject accepts a lazy sequence, so the source can fail after the
    // guard is entered. The finally must still unwind it.
    let exploding () : IEnumerable<obj> =
        seq {
            yield box 1
            failwith "boom"
        }
    for _ in 1 .. 50 do
        Assert.ThrowsAny<exn>(fun () -> JyroValue.FromObject(exploding ()) |> ignore) |> ignore
    walkAtDepthLimit ()

[<Fact>]
let ``null is normalized the same way regardless of ambient walk depth`` () =
    // A call made while the counter already sits near the cap must still give
    // JyroNull for a null argument, not ValueNestingLimitExceeded.
    let mutable observed = "<not reached>"
    let rec nest (remaining: int) : IEnumerable<obj> =
        seq {
            if remaining = 0 then
                observed <-
                    try
                        if obj.ReferenceEquals(JyroValue.FromObject(null), JyroNull.Instance) then "JyroNull" else "another value"
                    with e -> e.GetType().Name
                yield box 1
            else
                yield box (JyroValue.FromObject(nest (remaining - 1)))
        }
    // Shallow, then as deep as the guard allows.
    for depth in [ 1; valueNestingMaximum - 2 ] do
        observed <- "<not reached>"
        JyroValue.FromObject(nest depth) |> ignore
        Assert.Equal("JyroNull", observed)

// -- Happy paths, so the guards above are not masking a broken conversion ---

[<Fact>]
let ``FromObject produces the expected values for a dictionary, a plain object and a sequence`` () =
    let fromDict = JyroValue.FromObject(sampleDictionary ()) :?> JyroObject
    Assert.Equal<string list>([ "a"; "b" ], List.ofSeq (fromDict :> IReadOnlyDictionary<string, JyroValue>).Keys)

    let fromPoco = JyroValue.FromObject(Customer()) :?> JyroObject
    Assert.Equal<string list>([ "Name"; "Age" ], List.ofSeq (fromPoco :> IReadOnlyDictionary<string, JyroValue>).Keys)
    Assert.Equal("Alice", (fromPoco.GetProperty("Name") :?> JyroString).Value)

    let fromSeq = JyroValue.FromObject(List<obj>([ box 1; box "two"; null ])) :?> JyroArray
    Assert.Equal(3, fromSeq.Count)
    Assert.Same(JyroNull.Instance, fromSeq.[2])

// -- FromObject's text arm does not fail on a null text ----------------------

/// A host type whose ToString() returns null. Legal in .NET, and it used to
/// make FromObject throw ArgumentNullException out of JyroString's constructor.
type NullToString() =
    override _.ToString() : string = null

[<Fact>]
let ``FromObject normalizes a host type whose ToString returns null`` () =
    // FromObject refuses only what the book refuses (a non-finite number,
    // 30.2.17; a value nested past the maximum depth); a type with no public
    // property or field becomes its text (JYRO-ADR-156 (f)), and JyroString
    // rejects null, so a null text must become the empty string.
    let v = JyroValue.FromObject(NullToString())
    let s = Assert.IsType<JyroString>(v)
    Assert.Equal("", s.Value)

// -- The JSON walkers charge depth like the other converters ----------------

let private deepJson (depth: int) =
    String.replicate depth "[" + "1" + String.replicate depth "]"

[<Fact>]
let ``a JsonElement deeper than the guard is rejected, not silently built`` () =
    // A host can parse with a raised MaxDepth and hand the element to
    // FromObject. Before the walkers were guarded this produced a value deeper
    // than MaxDepth, which then failed on every later walk over it - ToJson,
    // Freeze, Clone - far from the entry point that created it.
    let opts = JsonDocumentOptions(MaxDepth = 5000)
    use doc = JsonDocument.Parse(deepJson 400, opts)
    let ex =
        Assert.Throws<JyroRuntimeException>(fun () ->
            JyroValue.FromObject(box doc.RootElement) |> ignore)
    Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

[<Fact>]
let ``a JsonElement within the guard converts and stays walkable`` () =
    let opts = JsonDocumentOptions(MaxDepth = 5000)
    use doc = JsonDocument.Parse(deepJson 50, opts)
    let v = JyroValue.FromObject(box doc.RootElement)
    // The point of the cap: whatever we build must survive the guarded walks.
    v.ToJson() |> ignore
    v.ToObjectValue() |> ignore
    (v :?> JyroArray).Freeze() |> ignore

[<Fact>]
let ``the JSON walkers leave the depth counter balanced`` () =
    let opts = JsonDocumentOptions(MaxDepth = 5000)
    use deep = JsonDocument.Parse(deepJson 400, opts)
    for _ in 1 .. 20 do
        Assert.Throws<JyroRuntimeException>(fun () ->
            JyroValue.FromObject(box deep.RootElement) |> ignore) |> ignore
    walkAtDepthLimit ()

// -- FromObject tolerates indexed properties --------------------------------

/// A POCO with an indexer. `prop.CanRead` is true for `Item`, but reading it
/// needs an argument, so an unfiltered GetValue raises
/// TargetParameterCountException.
type WithIndexer() =
    member _.Name = "indexed"
    member _.Item with get (i: int) = i * 2

[<Fact>]
let ``FromObject skips indexed properties instead of crashing`` () =
    let o = JyroValue.FromObject(WithIndexer()) :?> JyroObject
    let keys = o :> IReadOnlyDictionary<string, JyroValue>
    Assert.True(keys.ContainsKey("Name"))
    Assert.False(keys.ContainsKey("Item"))

[<Fact>]
let ``FromObject handles BCL types that carry indexers`` () =
    // string, List<T> and JyroObject all expose an indexer; each used to throw.
    JyroValue.FromObject("a string") |> ignore
    JyroValue.FromObject(List<int>([ 1; 2 ])) |> ignore
    JyroValue.FromObject(JyroObject()) |> ignore

// -- WithInput normalizes null ----------------------------------------------

[<Fact>]
let ``WithInput(null) normalizes rather than storing null`` () =
    // JyroValue is AllowNullLiteral, so a C# host writing WithInput(null)
    // binds to the JyroValue overload, not the obj one. Storing the null
    // surfaced as an NRE later, during execution.
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output.seen = Input == null"))
            (fun r -> r.WithInput(Unchecked.defaultof<JyroValue>))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))

[<Fact>]
let ``FromObject accepts an empty dictionary and an empty sequence`` () =
    Assert.Equal(0, (JyroValue.FromObject(Dictionary<string, obj>()) :?> JyroObject).Count)
    Assert.Equal(0, (JyroValue.FromObject(List<obj>()) :?> JyroArray).Count)

// -- The null rule on the members added after it was settled ----------------
//
// Two doors still dereferenced a null and raised NullReferenceException, which
// names nothing and is not a type a host is told to catch. They are the two
// halves of the rule, so they get opposite answers.

[<Fact>]
let ``WriteJson refuses a null writer, naming it`` () =
    // A writer is a resource, so it is refused - the same side of the rule as
    // FromJson of a stream.
    for v in [ JyroArray() :> JyroValue; JyroObject() :> JyroValue
               JyroString("s") :> JyroValue; JyroNumber(1.0) :> JyroValue
               JyroNull.Instance :> JyroValue; JyroBoolean.True :> JyroValue ] do
        let ex = Assert.Throws<System.ArgumentNullException>(fun () -> v.WriteJson(null))
        Assert.Equal("writer", ex.ParamName)

[<Fact>]
let ``WriteJson still writes through a real writer`` () =
    // The guard must not have changed what a successful write produces.
    let o = JyroObject()
    o.SetProperty("a", JyroNumber(1.0))
    Assert.Equal("""{"a":1}""", o.ToJson())

[<Fact>]
let ``CompareForSort treats a null reference as the null value`` () =
    // A value, so it normalizes - the other side of the rule. CompareForSort is
    // documented total: a sort has to place every pair somewhere, which is why
    // it exists separately from the relational operators, which refuse.
    let one = JyroNumber(1.0) :> JyroValue
    Assert.Equal(0, JyroValue.CompareForSort(null, null))
    Assert.Equal(0, JyroValue.CompareForSort(null, JyroNull.Instance))
    // Null ranks first, exactly where a stored null ranks.
    Assert.True(JyroValue.CompareForSort(null, one) < 0)
    Assert.True(JyroValue.CompareForSort(one, null) > 0)
    Assert.Equal(JyroValue.CompareForSort(JyroNull.Instance, one),
                 JyroValue.CompareForSort(null, one))

[<Fact>]
let ``a null reference sorts where a stored null sorts`` () =
    // The property that makes normalizing the right answer rather than merely a
    // permitted one: the two are indistinguishable to a sort.
    let values : JyroValue list =
        [ JyroNumber(2.0); null; JyroString("a"); JyroNull.Instance; JyroNumber(1.0) ]
    let sorted = values |> List.sortWith (fun a b -> JyroValue.CompareForSort(a, b))
    // `CompareForSort` normalizes for the *comparison*; the element itself is
    // still the null reference the caller put in the list, so read its kind
    // through the same normalization rather than dereferencing it.
    let kindOf (v: JyroValue) =
        if obj.ReferenceEquals(v, null) then JyroValueType.Null else v.ValueType
    Assert.Equal<JyroValueType list>(
        [ JyroValueType.Null; JyroValueType.Null; JyroValueType.Number
          JyroValueType.Number; JyroValueType.String ],
        sorted |> List.map kindOf)

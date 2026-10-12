module RealTree.Jyro.Tests.ValueIdentity

open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// `EqualsValue` is the script's `==`; `Object.Equals` is what .NET collections
/// use. They are deliberately different operations, and the split is by
/// mutability: the four immutable scalars carry value equality, the two mutable
/// containers and `JyroFunction` keep reference identity.
///
/// Before this, no subclass overrode `Equals`, `GetHashCode` or `ToString`, so
/// `JyroString("a").Equals(JyroString("a"))` was false, every `Dictionary` and
/// `Distinct` fell back to reference identity, and logging a value printed
/// `RealTree.Jyro.JyroString`.

let private v (x: #JyroValue) = x :> JyroValue

// -- Scalars compare and hash by value ------------------------------------

[<Fact>]
let ``equal strings are equal and hash alike`` () =
    Assert.True((JyroString("a") :> obj).Equals(JyroString("a")))
    Assert.Equal(JyroString("a").GetHashCode(), JyroString("a").GetHashCode())
    Assert.False((JyroString("a") :> obj).Equals(JyroString("b")))

[<Fact>]
let ``equal booleans and nulls are equal`` () =
    Assert.True((JyroBoolean.True :> obj).Equals(JyroBoolean.FromBoolean(true)))
    Assert.False((JyroBoolean.True :> obj).Equals(JyroBoolean.False))
    Assert.True((JyroNull.Instance :> obj).Equals(JyroNull.Instance))

[<Fact>]
let ``a number equal across representations hashes alike`` () =
    // The mixed exact/inexact case compares through the double, so the hash
    // has to be taken from the double too or equal values would hash apart.
    Assert.True((JyroNumber(1m) :> obj).Equals(JyroNumber(1.0)))
    Assert.Equal(JyroNumber(1m).GetHashCode(), JyroNumber(1.0).GetHashCode())

[<Fact>]
let ``a scalar works as a dictionary key`` () =
    let d = Dictionary<JyroValue, string>()
    d.[v (JyroString("k"))] <- "first"
    d.[v (JyroString("k"))] <- "second"
    Assert.Equal(1, d.Count)
    Assert.Equal("second", d.[v (JyroString("k"))])

[<Fact>]
let ``Distinct collapses equal scalars`` () =
    let distinct = Seq.distinct [ v (JyroString("a")); v (JyroString("a")); v (JyroString("b")) ]
    Assert.Equal(2, Seq.length distinct)

[<Fact>]
let ``a value of another type is never equal`` () =
    Assert.False((JyroString("1") :> obj).Equals(JyroNumber(1.0)))
    Assert.False((JyroNull.Instance :> obj).Equals(JyroBoolean.False))

// -- Containers keep reference identity, deliberately ---------------------

[<Fact>]
let ``two structurally equal arrays are not Equals`` () =
    // Identity, because a structural hash on a mutable container strands every
    // dictionary entry keyed by it the moment anything writes to the container.
    let a, b = JyroArray(), JyroArray()
    a.Add(v (JyroNumber(1.0)))
    b.Add(v (JyroNumber(1.0)))
    Assert.False((a :> obj).Equals(b))
    Assert.True((a :> obj).Equals(a))
    // The script-level answer is unchanged and still structural.
    Assert.True((a :> JyroValue).EqualsValue(b))

[<Fact>]
let ``a container's hash is stable across mutation`` () =
    let a = JyroArray()
    let d = Dictionary<JyroValue, string>()
    d.[v a] <- "kept"
    a.Add(v (JyroNumber(1.0)))
    Assert.True(d.ContainsKey(v a), "mutating a key must not strand its entry")

[<Fact>]
let ``Equals does not throw on a cyclic value`` () =
    // `EqualsValue` raises here, and the specification requires it to. That is
    // exactly why `Equals` must not delegate: a throwing `Equals` would fault
    // every BCL collection operation touching the value.
    let a = JyroArray()
    a.Add(v a)
    let b = JyroArray()
    b.Add(v b)
    Assert.False((a :> obj).Equals(b))
    Assert.Throws<JyroRuntimeException>(fun () -> (a :> JyroValue).EqualsValue(b) |> ignore) |> ignore

// -- ToString is useful and total -----------------------------------------

[<Theory>]
[<InlineData("hello", "hello")>]
[<InlineData("", "")>]
let ``a string prints its contents`` (input: string) (expected: string) =
    Assert.Equal(expected, (JyroString(input) :> obj).ToString())

[<Fact>]
let ``the other scalars print their script form`` () =
    Assert.Equal("1.5", (JyroNumber(1.5m) :> obj).ToString())
    Assert.Equal("true", (JyroBoolean.True :> obj).ToString())
    Assert.Equal("null", (JyroNull.Instance :> obj).ToString())

[<Fact>]
let ``a container prints its ToStringValue text, and raises where that does`` () =
    // 160.8.1 (JYRO-REQ-1630): ToString() on every JyroValue class returns the
    // same text as ToStringValue(), and is bound by the same limits, so it
    // raises where ToStringValue does. It once printed the shape instead.
    let a = JyroArray()
    a.Add(v (JyroNumber(1.0)))
    Assert.Equal(a.ToStringValue(), (a :> obj).ToString())
    Assert.Equal("[1]", (a :> obj).ToString())
    let o = JyroObject()
    Assert.Equal(o.ToStringValue(), (o :> obj).ToString())
    Assert.Equal("{}", (o :> obj).ToString())
    // A cycle has no text: ToStringValue raises, and so does ToString, with
    // the same code, a JyroRuntimeException rather than a stack overflow.
    let cyclic = JyroArray()
    cyclic.Add(v cyclic)
    let viaValue = Assert.Throws<JyroRuntimeException>(fun () -> cyclic.ToStringValue() |> ignore)
    let viaToString = Assert.Throws<JyroRuntimeException>(fun () -> (cyclic :> obj).ToString() |> ignore)
    Assert.Equal(viaValue.Code, viaToString.Code)
    Assert.Contains(viaToString.Code, [ MessageCode.ValueNestingLimitExceeded; MessageCode.ValueSizeLimitExceeded ])

// -- Freeze is reachable from the type hosts are actually handed ----------

[<Fact>]
let ``Freeze and IsFrozen are callable on a JyroValue`` () =
    // The README tells a host to "call Freeze() on the shared graph first".
    // Every handle the API gives out is typed JyroValue, on which that used to
    // be a compile error.
    let shared : JyroValue = JyroValue.FromJson("""{"a":[1,2]}""")
    Assert.False(shared.IsFrozen)
    let returned = shared.Freeze()
    Assert.True(shared.IsFrozen)
    Assert.Same(shared, returned)

[<Fact>]
let ``freezing through the base type is still deep`` () =
    let shared : JyroValue = JyroValue.FromJson("""{"a":{"b":[1]}}""")
    shared.Freeze() |> ignore
    Assert.True((shared.GetProperty("a")).IsFrozen)
    Assert.True((shared.GetProperty("a").GetProperty("b")).IsFrozen)

[<Fact>]
let ``a scalar is frozen already and freezing it is a no-op`` () =
    for scalar in [ v (JyroString("x")); v (JyroNumber(1.0)); v JyroBoolean.True; v JyroNull.Instance ] do
        Assert.True(scalar.IsFrozen)
        Assert.Same(scalar, scalar.Freeze())

// -- The two JSON doors parse the same dialect ----------------------------

[<Theory>]
[<InlineData("{ \"a\": 1, }")>]
[<InlineData("{ \"a\": 1 } // trailing comment")>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 FromJson of text and FromJson of a stream both refuse a trailing comma or comment with InvalidJson`` (json: string) =
    // The two doors once disagreed on these, and were then made to agree on
    // a lenient dialect. JYRO-REQ-2113 makes both read JSON (RFC 8259), which
    // has neither a trailing comma nor a comment, so both refuse alike.
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromJson(json) |> ignore)
    Assert.Equal(MessageCode.InvalidJson, ex.Code)
    use ms = new System.IO.MemoryStream(System.Text.Encoding.UTF8.GetBytes(json))
    let ex2 = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromJson(ms) |> ignore)
    Assert.Equal(MessageCode.InvalidJson, ex2.Code)

// -- IsTruthy is the language's rule, and the only one -------------------
// This was two members that disagreed. `ToBoolean` (now `IsTruthy`) was abstract with per-type
// overrides and `ToBooleanTruthiness` was a match on the base; they differed
// in exactly two places - an empty container was false through one and true
// through the other, and null raised through one and was false through the
// other. The compiler emitted the truthiness form, so the overrides were a
// second answer only a host could reach, and the shipped README named that one.

let private truthinessInScript (value: JyroValue) : bool =
    let input = JyroObject()
    input.SetProperty("x", value)
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource("Output.v = false\nif Input.x then\n  Output.v = true\nend"))
            (fun r -> r.WithInput(input :> JyroValue))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    (r.TryValue.Value.GetProperty("v")).IsTruthy()

[<Fact>]
let ``IsTruthy agrees with the language for every value shape`` () =
    let cases : (string * JyroValue) list =
        [ "empty array",  JyroArray() :> JyroValue
          "empty object", JyroObject() :> JyroValue
          "null",         JyroNull.Instance :> JyroValue
          "empty string", JyroString("") :> JyroValue
          "non-empty",    JyroString("x") :> JyroValue
          "zero",         JyroNumber(0.0) :> JyroValue
          "one",          JyroNumber(1.0) :> JyroValue
          "false",        JyroBoolean.False :> JyroValue
          "true",         JyroBoolean.True :> JyroValue ]
    for (label, value) in cases do
        Assert.True(
            value.IsTruthy() = truthinessInScript value,
            sprintf "%s: host says %b, `if` says %b" label (value.IsTruthy()) (truthinessInScript value))

[<Fact>]
let ``an empty container is truthy and null is falsy, without raising`` () =
    // The two disagreements, pinned directly.
    Assert.True((JyroArray() :> JyroValue).IsTruthy())
    Assert.True((JyroObject() :> JyroValue).IsTruthy())
    Assert.False((JyroNull.Instance :> JyroValue).IsTruthy())

[<Fact>]
let ``AsBoolean is the strict question and still raises`` () =
    // A host that wants "this must actually be a boolean" has a member for it.
    Assert.Equal(true, ((JyroBoolean.True :> JyroValue).AsBoolean()).Value)
    Assert.Throws<JyroRuntimeException>(fun () -> (JyroString("x") :> JyroValue).AsBoolean() |> ignore) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> (JyroNull.Instance :> JyroValue).AsBoolean() |> ignore) |> ignore

// -- FromObject: data bags reflect, canonical forms stringify -------------
// The fallback arm stringified everything it did not recognize, so a host
// passing a POCO got a `JyroString` and found out several layers later - the
// README's own F# example returned "Unsupported binary operation + for Null".
// Reflecting everything instead is worse, and measurably so: it turns a `Guid`
// into `{}` (no readable properties at all), a `DateTime` into a
// fifteen-property bag, and raises out of `System.Type` and `MemoryStream`.

type Poco() =
    member val Name = "Ada" with get, set
    member val Age = 36 with get, set

type Customer = { Name: string; Age: int }

[<Fact>]
let ``a POCO becomes an object`` () =
    Assert.Equal("""{"Name":"Ada","Age":36}""", (JyroValue.FromObject(Poco())).ToJson())

[<Fact>]
let ``an F# record becomes an object`` () =
    // Records get a compiler-generated ToString, which is a debugging
    // convenience rather than a canonical form - hence the explicit test.
    Assert.Equal("""{"Name":"Ada","Age":36}""", (JyroValue.FromObject({ Name = "Ada"; Age = 36 })).ToJson())

[<Fact>]
let ``an anonymous record becomes an object`` () =
    Assert.Equal("""{"x":10}""", (JyroValue.FromObject({| x = 10 |})).ToJson())

[<Theory>]
[<InlineData("00000000-0000-0000-0000-000000000000")>]
let ``a Guid keeps its text form`` (expected: string) =
    // Reflection yields `{}` for a Guid - the value would simply be lost.
    Assert.Equal(expected, (JyroValue.FromObject(System.Guid.Empty)).ToStringValue())

[<Fact>]
let ``types that declare a canonical text form keep it`` () =
    Assert.Equal("https://example.com/", (JyroValue.FromObject(System.Uri("https://example.com"))).ToStringValue())
    Assert.Equal("01:30:00", (JyroValue.FromObject(System.TimeSpan.FromMinutes(90.0))).ToStringValue())

[<Fact>]
let ``a property whose getter throws is left out and the object stays an object`` () =
    // JYRO-ADR-156 (e): MemoryStream's ReadTimeout and WriteTimeout throw; they
    // are left out, and the stream is an object of its other readable
    // properties, so every value of one type has one Jyro type.
    use ms = new System.IO.MemoryStream()
    let v = JyroValue.FromObject(ms)
    let o = Assert.IsType<JyroObject>(v)
    let keys = o |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
    Assert.Contains("CanRead", keys)
    Assert.Contains("Length", keys)
    Assert.DoesNotContain("ReadTimeout", keys)
    Assert.DoesNotContain("WriteTimeout", keys)

[<Fact>]
let ``the documented F# helper example works`` () =
    // README.md:472 - previously [JM5206] Unsupported binary operation + for Null.
    let r = JyroScript.executeScript "Output.result = Input.x + 1" {| x = 10 |}
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    Assert.Equal("""{"result":11}""", r.TryValue.Value.ToJson())

type SelfRef() =
    member this.Self : SelfRef = this
    member _.Name = "node"

[<AllowNullLiteral>]
type Node(name: string) =
    member val Name = name with get, set
    member val Next : Node = null with get, set

[<Fact>]
let ``a self-referencing host object terminates`` () =
    // Conversion from a host value is a walk the maximum nesting depth binds
    // (JYRO-REQ-2047), and a cycle never ends, so it exceeds that maximum
    // (JYRO-REQ-2045). With no execution in progress, FromObject fails through
    // its own channel, a thrown JyroRuntimeException, with that code
    // (JYRO-REQ-2046). It must fail cleanly: an unbounded reflection walk
    // would overflow the CLR stack and abort the test host instead.
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(SelfRef()) |> ignore)
    Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

[<Fact>]
let ``a longer reference loop terminates`` () =
    // A -> B -> A, which a direct self-reference check would miss. The loop
    // closes through a second object, and is refused the same way.
    let a = Node("a")
    let b = Node("b")
    a.Next <- b
    b.Next <- a
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(a) |> ignore)
    Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)

[<Fact>]
let ``an acyclic chain is still expanded in full`` () =
    // The cycle guard must not truncate legitimate nesting.
    let a, b, c = Node("a"), Node("b"), Node("c")
    a.Next <- b
    b.Next <- c
    let v = JyroValue.FromObject(a)
    Assert.Equal("c", (v.GetProperty("Next").GetProperty("Next").GetProperty("Name")).ToStringValue())

[<Fact>]
let ``the cycle guard unwinds between conversions`` () =
    // Each refused conversion must leave no state behind: every one of them
    // fails with the same code, and a later conversion of an acyclic graph
    // is still expanded in full rather than refused or cut short.
    let node = SelfRef()
    for _ in 1 .. 5 do
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(node) |> ignore)
        Assert.Equal(MessageCode.ValueNestingLimitExceeded, ex.Code)
    let a, b, c = Node("a"), Node("b"), Node("c")
    a.Next <- b
    b.Next <- c
    let v = JyroValue.FromObject(a)
    Assert.IsType<JyroObject>(v) |> ignore
    Assert.Equal("c", (v.GetProperty("Next").GetProperty("Next").GetProperty("Name")).ToStringValue())

module RealTree.Jyro.Tests.FreezeTests

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

[<Fact>]
let ``Freeze blocks every object mutator`` () =
    let o = JyroObject()
    o.["a"] <- JyroNumber(1.0)
    Assert.False(o.IsFrozen)
    o.Freeze() |> ignore
    Assert.True(o.IsFrozen)
    Assert.Throws<JyroRuntimeException>(fun () -> o.SetProperty("a", JyroNumber(2.0))) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> o.["a"] <- JyroNumber(2.0)) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> o.Remove("a") |> ignore) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> o.Clear()) |> ignore

[<Fact>]
let ``Freeze blocks every array mutator`` () =
    let a = JyroArray()
    a.Add(JyroNumber(1.0))
    a.Freeze() |> ignore
    Assert.True(a.IsFrozen)
    Assert.Throws<JyroRuntimeException>(fun () -> a.Add(JyroNumber(2.0))) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> a.Insert(0, JyroNumber(9.0))) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> a.[0] <- JyroNumber(2.0)) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> a.RemoveAt(0)) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> a.Clear()) |> ignore

[<Fact>]
let ``Freeze is deep over nested objects and arrays`` () =
    let inner = JyroObject()
    inner.["x"] <- JyroNumber(1.0)
    let arr = JyroArray()
    arr.Add(JyroString("v"))
    let outer = JyroObject()
    outer.["inner"] <- inner
    outer.["arr"] <- arr
    outer.Freeze() |> ignore
    Assert.True(inner.IsFrozen)
    Assert.True(arr.IsFrozen)
    Assert.Throws<JyroRuntimeException>(fun () -> inner.SetProperty("x", JyroNumber(2.0))) |> ignore
    Assert.Throws<JyroRuntimeException>(fun () -> arr.Add(JyroString("z"))) |> ignore

[<Fact>]
let ``Reads are unaffected by freeze`` () =
    let o = JyroObject()
    o.["a"] <- JyroNumber(42.0)
    let arr = JyroArray()
    arr.Add(JyroString("hi"))
    o.["list"] <- arr
    o.Freeze() |> ignore
    Assert.Equal(42.0, o.["a"].ToDouble())
    Assert.Equal(2, o.Count)
    Assert.Equal("hi", (o.["list"].AsArray().[0]).ToStringValue())

[<Fact>]
let ``Unfrozen values still mutate, and Freeze is idempotent`` () =
    let o = JyroObject()
    o.SetProperty("a", JyroNumber(1.0))
    o.SetProperty("a", JyroNumber(2.0))
    Assert.Equal(2.0, o.["a"].ToDouble())
    o.Freeze() |> ignore
    o.Freeze() |> ignore
    Assert.True(o.IsFrozen)

// -- The backing store must not be reachable in a writable form ------------
// A container is its own read-only collection view (IReadOnlyList, IReadOnlyDictionary):
// a caller who casts a frozen container to a collection type must not reach the live
// List or Dictionary behind it, and so must not write through it around Freeze.

[<Fact>]
let ``a frozen object cannot be cast to a writable Dictionary`` () =
    let o = JyroObject()
    o.SetProperty("k", JyroNumber(1.0))
    o.Freeze() |> ignore
    Assert.False((box o) :? System.Collections.Generic.Dictionary<string, JyroValue>)
    Assert.False((box o) :? System.Collections.Generic.IDictionary<string, JyroValue>)
    Assert.Equal(1.0, o.["k"].ToDouble())

[<Fact>]
let ``a frozen array cannot be cast to a writable List`` () =
    let a = JyroArray()
    a.Add(JyroNumber(1.0))
    a.Freeze() |> ignore
    Assert.False((box a) :? System.Collections.Generic.List<JyroValue>)
    Assert.False((box a) :? System.Collections.Generic.IList<JyroValue>)
    Assert.Equal(1, a.Count)

[<Fact>]
let ``the collection views track later writes to an unfrozen value`` () =
    let a = JyroArray()
    let view = a :> System.Collections.Generic.IReadOnlyList<JyroValue>
    a.Add(JyroNumber(1.0))
    Assert.Equal(1, view.Count)
    let o = JyroObject()
    let pview = o :> System.Collections.Generic.IReadOnlyDictionary<string, JyroValue>
    o.SetProperty("k", JyroNumber(1.0))
    Assert.Equal(1, pview.Count)

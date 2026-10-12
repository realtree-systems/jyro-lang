module RealTree.Jyro.Tests.ContainerIndexBounds

open System
open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

// Two separate defects with one cause: an index reached its container without
// anything checking that it named a position.
//
// (1) `JyroArray.Insert` and `RemoveAt` delegated straight to `List<T>`, which
//     raises `ArgumentOutOfRangeException`. The package documents
//     `catch (JyroRuntimeException)` as the way to handle a failure, so an
//     out-of-range call from a host left that handler silent while a BCL
//     exception traveled past it. Their stdlib twins already bound-checked,
//     so the same mistake was reported one way from a script and another way
//     from C#.
//
// (2) Every index door converted with `Convert.ToInt32` *before* testing its
//     own bounds, so `a[1e30]` raised `OverflowException` from the conversion
//     and never reached the rule the door already had. A CLR failure stood in
//     for an index that is merely outside the container.
//
// The rule now: converting an index is never the step that decides its fate.
// An index with no `int` form is out of range, and each door then applies the
// out-of-range rule it already had - null for a read, a report for a write.

let private arrayOf (n: int) : JyroArray =
    let a = JyroArray()
    for i in 1 .. n do a.Add(JyroNumber(float i))
    a

let private expectCode (code: MessageCode) (f: unit -> unit) =
    let ex = Assert.Throws<JyroRuntimeException>(f)
    Assert.Equal(code, ex.Code)
    ex

// -- Insert: the bound is Length, because that position appends -------------

[<Fact>]
let ``Insert past the end is reported, not raised as a BCL exception`` () =
    let a = arrayOf 2
    let ex = expectCode MessageCode.IndexOutOfRange (fun () -> a.Insert(5, JyroNumber(9.0)))
    // The whole point: a host catching JyroRuntimeException sees this. Before
    // the fix the escaping type was ArgumentOutOfRangeException, which is not
    // a JyroRuntimeException, so Assert.Throws above would have failed.
    Assert.IsNotType<ArgumentOutOfRangeException>(ex)
    Assert.Equal(2, a.Count)

[<Fact>]
let ``Insert at a negative index is reported`` () =
    let a = arrayOf 2
    expectCode MessageCode.IndexOutOfRange (fun () -> a.Insert(-1, JyroNumber(9.0))) |> ignore
    Assert.Equal(2, a.Count)

[<Fact>]
let ``Insert at Length appends and must keep working`` () =
    let a = arrayOf 2
    a.Insert(a.Count, JyroNumber(9.0))
    Assert.Equal(3, a.Count)
    Assert.Equal(9.0, (a.[2] :?> JyroNumber).Value)

[<Fact>]
let ``Insert at the front and in the middle keep working`` () =
    let a = arrayOf 2
    a.Insert(0, JyroNumber(0.0))
    a.Insert(2, JyroNumber(1.5))
    Assert.Equal(4, a.Count)
    Assert.Equal<float list>(
        [ 0.0; 1.0; 1.5; 2.0 ],
        [ for i in 0 .. a.Count - 1 -> (a.[i] :?> JyroNumber).Value ])

[<Fact>]
let ``Insert into an empty array at 0 keeps working`` () =
    let a = JyroArray()
    a.Insert(0, JyroNumber(7.0))
    Assert.Equal(1, a.Count)

// -- RemoveAt: the bound is Length - 1, because it must name an element ------

[<Fact>]
let ``RemoveAt past the last element is reported`` () =
    let a = arrayOf 2
    expectCode MessageCode.IndexOutOfRange (fun () -> a.RemoveAt(5)) |> ignore
    Assert.Equal(2, a.Count)

[<Fact>]
let ``RemoveAt at a negative index is reported`` () =
    let a = arrayOf 2
    expectCode MessageCode.IndexOutOfRange (fun () -> a.RemoveAt(-1)) |> ignore
    Assert.Equal(2, a.Count)

[<Fact>]
let ``RemoveAt on an empty array is reported rather than raised`` () =
    let a = JyroArray()
    expectCode MessageCode.IndexOutOfRange (fun () -> a.RemoveAt(0)) |> ignore

[<Fact>]
let ``RemoveAt at Length - 1 keeps working`` () =
    let a = arrayOf 3
    a.RemoveAt(a.Count - 1)
    Assert.Equal(2, a.Count)
    Assert.Equal(2.0, (a.[1] :?> JyroNumber).Value)

/// The asymmetry between the twins, stated as a test so it cannot be "tidied
/// up" into a shared bound. `Length` is the one index where they must disagree.
[<Fact>]
let ``Length is insertable but not removable`` () =
    let a = arrayOf 2
    let atLength = a.Count
    expectCode MessageCode.IndexOutOfRange (fun () -> a.RemoveAt(atLength)) |> ignore
    a.Insert(atLength, JyroNumber(9.0))
    Assert.Equal(3, a.Count)

/// A frozen empty array fails both tests at once. Read-only must win: the
/// caller's mistake is writing to a frozen value, and which index they named
/// is not the fact they need to hear about.
[<Fact>]
let ``a frozen array reports read-only, not out-of-range`` () =
    let a = JyroArray()
    a.Freeze() |> ignore
    expectCode MessageCode.ValueIsReadOnly (fun () -> a.RemoveAt(0)) |> ignore
    expectCode MessageCode.ValueIsReadOnly (fun () -> a.Insert(99, JyroNumber(1.0))) |> ignore

// -- An index with no int form is out of range, not an overflow -------------

[<Fact>]
let ``a read at an index too large for int yields null rather than overflowing`` () =
    // GetIndex is internal (JYRO-REQ-1876): the read is made from a script.
    let v =
        Runner.expectSuccess
            "Output.a = Input.items[1e30]\nOutput.b = Input.items[-1e30]\nOutput.c = Input.items[3000000000]"
            (Runner.objOf [ "items", Runner.arrOf [ Runner.num 1.0; Runner.num 2.0 ] ])
    for name in [ "a"; "b"; "c" ] do
        Assert.True((Runner.prop name v).IsNull, name)

/// A NaN or infinite index is no longer expressible: a Jyro number is finite,
/// so `TryToInt32`'s NaN and infinity arms are now unreachable through a
/// constructed value. They are kept because `TryToInt32` is a general
/// conversion and its contract should not depend on which callers exist today.
[<Fact>]
let ``a NaN or infinite index cannot be built in the first place`` () =
    // 30.2.17 (JYRO-REQ-60): the constructor raises the host-number code.
    expectCode MessageCode.HostNumberNaN (fun () -> JyroNumber(Double.NaN) |> ignore) |> ignore
    expectCode MessageCode.HostNumberInfinite (fun () -> JyroNumber(Double.PositiveInfinity) |> ignore) |> ignore
    expectCode MessageCode.HostNumberInfinite (fun () -> JyroNumber(Double.NegativeInfinity) |> ignore) |> ignore

[<Fact>]
let ``a string read at an index too large for int yields null`` () =
    let v =
        Runner.expectSuccess
            "Output.a = Input.s[1e30]\nOutput.b = Input.s[1]\nOutput.c = Input.s[99]"
            (Runner.objOf [ "s", Runner.str "abc" ])
    Assert.True((Runner.prop "a" v).IsNull)
    // The in-range cases this shares a code path with must be unaffected.
    Assert.Equal("b", Runner.asString (Runner.prop "b" v))
    Assert.True((Runner.prop "c" v).IsNull)

[<Fact>]
let ``a write at an index too large for int is reported, not overflowed`` () =
    // SetIndex is internal (JYRO-REQ-1876): the write is made from a script.
    let r = Runner.runEmpty "var a = [1, 2]\na[1e30] = 1"
    Assert.False(r.IsSuccess)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.IndexOutOfRange)
    // The index is named by the number the caller actually wrote. 1e30 has no
    // int form, so reporting it as one would print a number nobody supplied.
    // Its string form is ECMAScript Number::toString, `1e+30` (40.4.7, JYRO-REQ-151).
    Assert.Contains("1e+30", m.Message)

[<Fact>]
let ``a write at a negative index too large for int is reported`` () =
    let r = Runner.runEmpty "var a = [1, 2]\na[-1e30] = 1"
    Assert.False(r.IsSuccess)
    Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.IndexOutOfRange)

[<Fact>]
let ``a script reading past int range gets null, not a crash`` () =
    let v = Runner.expectSuccess "Output.hit = Input.items[1e30]" (Runner.objOf [ "items", Runner.arrOf [ Runner.num 1.0 ] ])
    Assert.Equal(JyroValueType.Null, (v.GetProperty("hit")).ValueType)

[<Fact>]
let ``a script writing past int range reports the index, not an internal error`` () =
    let codes =
        Runner.expectFailure
            "var a = [1, 2]\na[1e30] = 5"
            (JyroObject() :> JyroValue)
    // Before the fix the OverflowException escaped into the statement
    // wrapper's catch-all and arrived as RuntimeError - a generic "something
    // threw" for a condition the engine knows precisely.
    Assert.Contains(MessageCode.IndexOutOfRange, codes)
    Assert.DoesNotContain(MessageCode.RuntimeError, codes)

[<Fact>]
let ``the append idiom still works from a script`` () =
    let v = Runner.expectSuccess "var a = []\na[0] = 1\na[1] = 2\nOutput.n = Length(a)" (JyroObject() :> JyroValue)
    Assert.Equal(2.0, Runner.asNumber (v.GetProperty("n")))

// -- Characterisation: what deliberately did NOT change ---------------------

/// The language has since decided the question this characterisation left
/// open: 30.4.10 (JYRO-REQ-78) refuses a fractional index with
/// IntegerIndexRequired, whose one argument is the index (JYRO-ADR-153 (1)).
[<Fact>]
let ``a fractional index is refused with IntegerIndexRequired`` () =
    let r = Runner.runEmpty "var a = [1, 2, 3, 4]\nOutput.r = a[1.5]"
    Assert.False(r.IsSuccess)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.IntegerIndexRequired)
    Assert.Equal(1, m.Args.Length)

/// The getter's out-of-range answer is still null, and still indistinguishable
/// from a stored null. That is a known, separately-recorded decision - pinned
/// here so this change is not misread as having settled it.
[<Fact>]
let ``an ordinary out-of-range read still yields null`` () =
    let v = Runner.expectSuccess "Output.a = [1, 2][99]\nOutput.b = [1, 2][-1]" (JyroObject() :> JyroValue)
    Assert.True((Runner.prop "a" v).IsNull)
    Assert.True((Runner.prop "b" v).IsNull)

// -- Every door onto a container gives one answer ---------------------------
//
// There were three doors onto an array's elements and they gave two answers:
// `arr[5]` was null, while `arr.Items[5]` and `((IReadOnlyList<JyroValue>)arr)[5]`
// threw. The sharpest part is that `Items` and the cast have the *same static
// type*, so which behavior you got depended on how the reference had been
// obtained - a difference no signature could show. `Items` and `Properties` are
// gone (JYRO-REQ-1901); the doors that remain still agree.
//
// The BCL reading (a host casting to IReadOnlyList has asked for that
// interface's contract) was considered and overruled: the doors agree.

[<Fact>]
let ``both array doors answer an out-of-range read the same way`` () =
    let a = arrayOf 2
    Assert.Same(JyroNull.Instance, a.[5])
    Assert.Same(JyroNull.Instance, (a :> IReadOnlyList<JyroValue>).[5])

[<Fact>]
let ``all three object doors answer a missing key the same way`` () =
    let o = JyroObject()
    o.SetProperty("k", JyroNumber(1.0))
    Assert.Same(JyroNull.Instance, o.["zz"])
    Assert.Same(JyroNull.Instance, o.GetProperty("zz"))
    Assert.Same(JyroNull.Instance, (o :> IReadOnlyDictionary<string, JyroValue>).["zz"])

[<Fact>]
let ``the collection interfaces still read, count and enumerate normally`` () =
    let a = arrayOf 3
    let list = a :> IReadOnlyList<JyroValue>
    Assert.Equal(3, list.Count)
    Assert.Equal(2.0, (list.[1] :?> JyroNumber).Value)
    Assert.Equal<float list>([ 1.0; 2.0; 3.0 ],
                             [ for v in a -> (v :?> JyroNumber).Value ])
    let o = JyroObject()
    o.SetProperty("a", JyroNumber(1.0))
    let dict = o :> IReadOnlyDictionary<string, JyroValue>
    Assert.Equal(1, dict.Count)
    Assert.True(dict.ContainsKey("a"))
    Assert.Equal<string list>([ "a" ], List.ofSeq dict.Keys)

[<Fact>]
let ``the containers expose no writable collection interface`` () =
    // An IList or IDictionary reference could mutate a frozen container around Freeze.
    let a = arrayOf 2
    Assert.False((box a) :? IList<JyroValue>)
    Assert.False((box a) :? ResizeArray<JyroValue>)
    let o = JyroObject()
    Assert.False((box o) :? IDictionary<string, JyroValue>)
    Assert.False((box o) :? Dictionary<string, JyroValue>)

// -- An index of the wrong type ---------------------------------------------

// The third face of the same defect, and the one the two fixes above left
// behind. `JyroArray.SetIndex` handled a `JyroNumber` index and `JyroObject`
// a `JyroString`, and both ended `| _ -> ()`. So `o[1] = 5` on an object and
// `a['x'] = 9` on an array did nothing at all and reported nothing at all -
// the script proceeded as though the write had happened, which is the rule
// stated when the array's negative index was fixed: silently discarding a
// write is worse than either alternative.
//
// The numeric axis was fixed on both containers and the wrong-type axis was
// missed on both, so the two halves of one door disagreed: `a[-1] = 9` and
// `a[1e30] = 9` were reported while `a[true] = 9` was not.
//
// Reads are deliberately not symmetrical. A read answers every miss with null
// - that is the settled rule for this type, and an index of the wrong type is
// a miss like any other. A write has no such answer available: there is no
// position to write to, and pretending otherwise loses the value.

let private runFail (source: string) =
    let r = Runner.runEmpty source
    Assert.False(r.IsSuccess, sprintf "Expected a failure from: %s" source)
    r.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq

[<Theory>]
[<InlineData("var o = { 'a': 1 }\no[1] = 5")>]
[<InlineData("var o = { 'a': 1 }\no[true] = 5")>]
[<InlineData("var o = { 'a': 1 }\no[null] = 5")>]
[<InlineData("var o = { 'a': 1 }\no[[1]] = 5")>]
let ``writing to an object at a non-string index is reported`` (source: string) =
    Assert.Contains(MessageCode.InvalidIndexType, runFail source)

[<Theory>]
[<InlineData("var a = [1, 2]\na['x'] = 9")>]
[<InlineData("var a = [1, 2]\na[true] = 9")>]
[<InlineData("var a = [1, 2]\na[null] = 9")>]
[<InlineData("var a = [1, 2]\na[{ 'k': 1 }] = 9")>]
let ``writing to an array at a non-number index is reported`` (source: string) =
    Assert.Contains(MessageCode.InvalidIndexType, runFail source)

[<Fact>]
let ``the numeric out-of-range report is unchanged`` () =
    // The control for the boundary between the two codes: an index of the
    // right type that names no position is still IndexOutOfRange, not the new
    // one. Only the type axis moved.
    Assert.Contains(MessageCode.IndexOutOfRange, runFail "var a = [1, 2]\na[-1] = 9")
    Assert.Contains(MessageCode.IndexOutOfRange, runFail "var a = [1, 2]\na[5] = 9")
    Assert.Contains(MessageCode.IndexOutOfRange, runFail "var a = [1, 2]\na[1e30] = 9")

[<Fact>]
let ``a read at an index of the wrong type is still null`` () =
    let v = Runner.expectSuccess "Output.a = ({ 'k': 1 })[1]\nOutput.b = ([1, 2])['x']" (JyroObject() :> JyroValue)
    Assert.True((Runner.prop "a" v).IsNull)
    Assert.True((Runner.prop "b" v).IsNull)

[<Fact>]
let ``writes at a correct-type index keep working`` () =
    let v = Runner.expectSuccess "var o = { }\no['k'] = 1\nvar a = [0, 0]\na[1] = 7\nOutput.o = o\nOutput.a = a" (JyroObject() :> JyroValue)
    Assert.Equal(1.0, Runner.asNumber (Runner.prop "k" (Runner.prop "o" v)))
    Assert.Equal(7.0, Runner.asNumber ((Runner.prop "a" v :?> JyroArray).[1]))

// The host-side SetIndex door is internal (JYRO-REQ-1876); a host writes through SetProperty and the indexers.

// -- Immutability is reported before the index's type -----------------------

// Adding the wrong-type report created a collision the audit then found: two
// rules now cover `t['x'] = 9` on a frozen container, and nothing ordered them.
//
// The engine was three-ways inconsistent inside one type. `Insert` and the
// numeric arm of `SetIndex` both tested frozen first; the new wrong-type arm
// tested the index first, so a frozen array reported `InvalidIndexType` where
// its own sibling members reported `ValueIsReadOnly`. `JyroObject` reported the
// immutability in every case, so the two containers disagreed as well.
//
// `Insert` already stated the principle in place - "a frozen array's index
// validity is not the fact the caller needs to hear about" - and it is the
// right one: immutability is the fact the author has to act on, and it does not
// stop being true if they also fix the index.

[<Fact>]
let ``a write through an Input alias reports Input, whatever the index type`` () =
    // The script-level shape, and the one that matters: a host reading
    // `InvalidIndexType` here would go looking for a bug in the index
    // expression when the actual fault is that the value belongs to the host.
    let run (source: string) =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource(source))
            (fun r -> r.WithInputJson("{ \"o\": { \"k\": 1 }, \"a\": [1, 2] }"))
    for source in
        [ "var t = Input.a\nt['x'] = 9"        // array, wrong-typed index
          "var t = Input.a\nt[0] = 9"          // array, right-typed index
          "var t = Input.o\nt[1] = 9"          // object, wrong-typed index
          "var t = Input.o\nt.k = 9" ] do      // object, right-typed index
        let r = run source
        Assert.False(r.IsSuccess, source)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.InputValueIsReadOnly)

[<Fact>]
let ``an unfrozen container still reports the index type`` () =
    // The control: ordering the two checks must not disarm the newer one.
    Assert.Contains(MessageCode.InvalidIndexType, runFail "var a = [1, 2]\na['x'] = 9")
    Assert.Contains(MessageCode.InvalidIndexType, runFail "var o = { 'k': 1 }\no[1] = 9")

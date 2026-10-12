module RealTree.Jyro.Tests.CollectionIteration

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Tests
open System

// `foreach` iterated the live collection, so mutating it mid-loop gave four
// different answers depending on which container and which operation - none of
// them chosen, and none of them a script author could predict:
//
//                        array          object
//   add an element/key   aborts         aborts
//   set an existing one  ABORTS         COMPLETES
//   remove one           n/a            COMPLETES
//
// `a[0] = 99` and `o.a = 99` are the same act on the two containers, and they
// disagreed. The reason is entirely below the language: `List<T>` bumps its
// version counter on an indexer set and `Dictionary<K,V>` does not, and
// `Dictionary.Remove` stopped invalidating enumerators in .NET Core 3.0. So the
// script-visible behavior of a Jyro loop was decided by the BCL's enumerator
// rules, which have already changed once underneath this engine and would
// differ again on another runtime - in a language whose specification expects
// re-implementations.
//
// The abort was not reportable either. It surfaced as a bare `RuntimeError`
// carrying "Collection was modified; enumeration operation may not execute." -
// a .NET string, reached from untrusted script input, naming nothing about the
// script. That is the same defect class as the wrong-arity callback fixed
// earlier in this audit.
//
// **`foreach` now iterates a snapshot taken at loop entry.** The loop visits
// exactly the elements the collection held when it began, whatever the body
// does to it. That is one rule for both containers and every operation, it does
// not depend on the host runtime's enumerator semantics, and it removes the
// failure rather than dressing it up.
//
// No working script is affected. Every mutation shape that a snapshot changes
// the meaning of used to abort the run, so nothing could have depended on it;
// the two that used to complete - setting and removing an object key - are the
// two a snapshot leaves observable through the collection afterwards anyway.

let private outputOf (source: string) = Runner.expectSuccess source (JyroObject() :> JyroValue)

// -- The array and the object now answer alike ------------------------------

[<Fact>]
let ``setting an element during iteration no longer aborts the run`` () =
    // `a[0] = 99` used to abort; `o.a = 99` did not. One act, two containers,
    // two answers.
    let v = outputOf "var a = [1, 2, 3]\nforeach x in a do\n  a[0] = 99\nend\nOutput.a = a"
    Assert.Equal(99.0, Runner.asNumber ((Runner.prop "a" v :?> JyroArray).[0]))

[<Fact>]
let ``setting an object value during iteration keeps working`` () =
    let v = outputOf "var o = { 'a': 1, 'b': 2 }\nforeach x in o do\n  o.a = 99\nend\nOutput.o = o"
    Assert.Equal(99.0, Runner.asNumber (Runner.prop "a" (Runner.prop "o" v)))

// -- Growing the collection is bounded by the snapshot ----------------------

[<Fact>]
let ``appending during iteration visits only what was there at entry`` () =
    // The queue-drain shape. It used to abort, so no script can be relying on
    // the appended elements being visited - and with a snapshot they are not,
    // which is the answer `foreach xs` reads as.
    let v =
        outputOf
            (String.concat "\n"
            [ "var a = [1, 2]"
              "var seen = 0"
              "foreach x in a do"
              "  seen = seen + 1"
              "  a[Length(a)] = x"
              "end"
              "Output.seen = seen"
              "Output.length = Length(a)" ])
    Assert.Equal(2.0, Runner.asNumber (Runner.prop "seen" v))
    Assert.Equal(4.0, Runner.asNumber (Runner.prop "length" v))

[<Fact>]
let ``adding a key during iteration visits only what was there at entry`` () =
    let v =
        outputOf
            (String.concat "\n"
            [ "var o = { 'a': 1 }"
              "var seen = 0"
              "foreach x in o do"
              "  seen = seen + 1"
              "  o.b = 2"
              "end"
              "Output.seen = seen"
              "Output.keys = Length(Keys(o))" ])
    Assert.Equal(1.0, Runner.asNumber (Runner.prop "seen" v))
    Assert.Equal(2.0, Runner.asNumber (Runner.prop "keys" v))

[<Fact>]
let ``deleting a key during iteration still visits every original value`` () =
    // This one used to complete, but only because `Dictionary.Remove` happens
    // not to invalidate an enumerator on this runtime - and it visited one
    // value fewer as a result. A snapshot makes the count deliberate.
    let v =
        outputOf
            (String.concat "\n"
            [ "var o = { 'a': 1, 'b': 2 }"
              "var seen = 0"
              "foreach x in o do"
              "  seen = seen + 1"
              "  delete o.a"
              "end"
              "Output.seen = seen"
              "Output.keys = Length(Keys(o))" ])
    Assert.Equal(2.0, Runner.asNumber (Runner.prop "seen" v))
    Assert.Equal(1.0, Runner.asNumber (Runner.prop "keys" v))

// -- Nothing that worked before changed -------------------------------------

[<Fact>]
let ``an ordinary iteration is unchanged`` () =
    let v =
        outputOf
            (String.concat "\n"
            [ "var a = [1, 2, 3]"
              "var t = 0"
              "foreach x in a do"
              "  t = t + x"
              "end"
              "var o = { 'a': 10, 'b': 20 }"
              "var u = 0"
              "foreach x in o do"
              "  u = u + x"
              "end"
              "var s = ''"
              "foreach c in 'abc' do"
              "  s = s & c"
              "end"
              "Output.t = t"
              "Output.u = u"
              "Output.s = s" ])
    Assert.Equal(6.0, Runner.asNumber (Runner.prop "t" v))
    Assert.Equal(30.0, Runner.asNumber (Runner.prop "u" v))
    Assert.Equal("abc", Runner.asString (Runner.prop "s" v))

[<Fact>]
let ``break and continue still work over a snapshot`` () =
    let v =
        outputOf
            (String.concat "\n"
            [ "var a = [1, 2, 3, 4]"
              "var t = 0"
              "foreach x in a do"
              "  if x == 2 then"
              "    continue"
              "  end"
              "  if x == 4 then"
              "    break"
              "  end"
              "  t = t + x"
              "end"
              "Output.t = t" ])
    Assert.Equal(4.0, Runner.asNumber (Runner.prop "t" v))

[<Fact>]
let ``the loop still counts against the iteration budget`` () =
    // The snapshot must not move the limit check off the loop path: a script
    // that iterates a large collection is still bounded.
    let r =
        Runner.compileThenRun
            (JyroCompiler().WithStdlib().WithSource(
                "var a = Range(0, 100)\nforeach x in a do\n  Output.x = x\nend"))
            (fun runner ->
                runner.WithInput(JyroObject() :> JyroValue)
                      .WithExecutionOptions({ JyroExecutionOptions.Default with MaxLoopIterations = 10 }))
    Assert.False(r.IsSuccess, "the iteration budget did not fire")
    Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.LoopIterationLimitExceeded)

[<Fact>]
let ``a nested loop over the same collection is unaffected`` () =
    let v =
        outputOf
            (String.concat "\n"
            [ "var a = [1, 2]"
              "var t = 0"
              "foreach x in a do"
              "  foreach y in a do"
              "    t = t + 1"
              "  end"
              "end"
              "Output.t = t" ])
    Assert.Equal(4.0, Runner.asNumber (Runner.prop "t" v))

// The host-facing ToIterable door is internal (JYRO-REQ-1876); the snapshot rule is the script's.

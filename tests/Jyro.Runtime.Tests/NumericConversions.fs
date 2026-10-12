module RealTree.Jyro.Tests.NumericConversions

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// `JyroNumber.ToInt32`, `ToInt64` and `DecimalValue` let a raw
// `System.OverflowException` escape for a value too large to narrow.
//
// The defect is the inconsistency, not the failure. The `As*`/`To*` family
// reports a failed conversion as a `JyroRuntimeException` - every default on
// the base class raises `InvalidCast` - so these three members reported a
// wrong *type* one way and a wrong *magnitude* another, on a public surface
// where the package documents `catch (JyroRuntimeException)` as the way to
// handle a failure. `ToObjectValue` and `WriteJson` already fenced the
// identical conversion; these were the members that did not.
//
// Distinct from the finiteness rule: NaN and the infinities are unconstructible
// now, so every value reaching these members is finite. What remains is a
// finite magnitude with no `int`, `int64` or `decimal` to narrow to.

let private outOfRange (f: unit -> unit) =
    let ex = Assert.Throws<JyroRuntimeException>(f)
    Assert.Equal(MessageCode.IntegerOutOfRange, ex.Code)
    ex

// -- The contract: a Jyro exception, not a BCL one --------------------------

[<Fact>]
let ``ToInt32 past int range reports rather than raising OverflowException`` () =
    let ex = outOfRange (fun () -> JyroNumber(1e300).ToInt32() |> ignore)
    // The message must name the bound and the value; the BCL's own text
    // ("Value was either too large or too small for an Int32") named neither.
    // The value is written in its string form, which for an inexact number is
    // ECMAScript Number::toString, `1e+300` (40.4.7, JYRO-REQ-151), not .NET's.
    Assert.Contains("2147483647", ex.Message)
    Assert.Contains("1e+300", ex.Message)

[<Fact>]
let ``ToInt64 past int64 range reports`` () =
    let ex = outOfRange (fun () -> JyroNumber(1e300).ToInt64() |> ignore)
    Assert.Contains("9223372036854775807", ex.Message)

[<Fact>]
let ``DecimalValue past decimal range reports`` () =
    // decimal's range is far narrower than a double's - about 7.9e28 against
    // 1.8e308 - so this is reachable well below the values the other two need.
    let ex = outOfRange (fun () -> JyroNumber(1e300).DecimalValue |> ignore)
    Assert.Contains("79228162514264337593543950335", ex.Message)

[<Fact>]
let ``a value just past decimal range but well inside double range reports`` () =
    outOfRange (fun () -> JyroNumber(1e29).DecimalValue |> ignore) |> ignore

[<Fact>]
let ``the three agree with each other on a value only one of them can hold`` () =
    // 1e20 fits int64 and decimal, but not int. Each member answers for its
    // own range rather than deferring to a shared one.
    let n = JyroNumber(1e20)
    outOfRange (fun () -> n.ToInt32() |> ignore) |> ignore
    Assert.Equal(100000000000000000000m, n.DecimalValue)
    Assert.Equal(100000000000000000000.0, n.ToDouble())

// -- What must not have changed ---------------------------------------------

[<Fact>]
let ``ordinary narrowing is untouched`` () =
    Assert.Equal(42, JyroNumber(42).ToInt32())
    Assert.Equal(-7, JyroNumber(-7).ToInt32())
    Assert.Equal(0, JyroNumber(0).ToInt32())
    Assert.Equal(1000000000000000000L, JyroNumber(1e18).ToInt64())
    Assert.Equal(1.5m, JyroNumber(1.5).DecimalValue)

/// Caught rather than range-tested precisely so this stays put: `Convert`
/// rounds half-to-even on the way to an integer, and a hand-written range test
/// would have had to re-derive that to remain equivalent.
[<Fact>]
let ``Convert's own rounding is preserved`` () =
    Assert.Equal(3, JyroNumber(2.6).ToInt32())
    Assert.Equal(2, JyroNumber(2.5).ToInt32())   // half-to-even
    Assert.Equal(4, JyroNumber(3.5).ToInt32())   // half-to-even
    Assert.Equal(-3, JyroNumber(-2.6).ToInt32())

[<Fact>]
let ``the exact boundaries themselves still convert`` () =
    Assert.Equal(Int32.MaxValue, JyroNumber(Int32.MaxValue).ToInt32())
    Assert.Equal(Int32.MinValue, JyroNumber(Int32.MinValue).ToInt32())
    Assert.Equal(Int64.MaxValue, JyroNumber(Int64.MaxValue).ToInt64())
    Assert.Equal(Int64.MinValue, JyroNumber(Int64.MinValue).ToInt64())

[<Fact>]
let ``an exact number keeps its exact decimal`` () =
    // The Exact arm does not convert at all, so it cannot overflow and must
    // return the stored value untouched - including above 2^53.
    let n = JyroNumber(9007199254740993m)
    Assert.True(n.IsExact)
    Assert.Equal(9007199254740993m, n.DecimalValue)

[<Fact>]
let ``the wrong-type failure still reports InvalidCast`` () =
    // The other half of the family's contract: a wrong type and a wrong
    // magnitude are different failures and keep different codes.
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroString("x").ToInt32() |> ignore)
    Assert.Equal(MessageCode.InvalidCast, ex.Code)

[<Fact>]
let ``the argument helpers are unaffected`` () =
    // GetInt32 range-checks before converting, so it reports its own
    // diagnostic naming the function and parameter - which must keep winning
    // over the one added here.
    let codes =
        Runner.expectFailure "Output.v = Skip([1,2,3], 1e20)" (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.IntegerOutOfRange, codes)
    Assert.DoesNotContain(MessageCode.RuntimeError, codes)

// -- Every CLR numeric primitive becomes a number ---------------------------

// `FromObject` is the host-object door behind `WithInput(object)`, and its
// type-test chain had arms for `int`, `int64`, `float`, `decimal`, `bool` and
// `string`. Every other numeric primitive - `float32`, `byte`, `sbyte`,
// `int16`, `uint16`, `uint32`, `uint64` - fell through to the stringify
// fallback and arrived in the script as a **string**.
//
// `1.5f` became the JSON string `"1.5"`, so `Input.x + 1` in a script was
// string concatenation or a type error rather than arithmetic. `Single.NaN`
// became the literal string `"NaN"` - which is exactly the outcome the
// finiteness rule was introduced to prevent, arriving through the one door
// that had no finiteness check.
//
// A missed case in a type-test chain rather than a decision: every numeric
// primitive the chain did name converted, and nothing recorded a reason for
// the ones it did not.

let private numberOf (o: obj) : JyroNumber =
    match JyroValue.FromObject(o) with
    | :? JyroNumber as n -> n
    | other -> failwithf "Expected a JyroNumber for %s, got %O" (o.GetType().Name) (other.GetType().Name)

[<Fact>]
let ``an integral primitive converts exactly`` () =
    // Exact, like `int` and `int64` beside them: an integer read from a host
    // field is a whole number, and decimal is where this engine keeps those.
    let cases : (obj * decimal) list =
        [ box (7y: sbyte), 7m
          box (7uy: byte), 7m
          box (7s: int16), 7m
          box (7us: uint16), 7m
          box (7: int), 7m
          box (7u: uint32), 7m
          box (7L: int64), 7m
          box (7UL: uint64), 7m ]
    for (value, expected) in cases do
        let n = numberOf value
        Assert.True(n.IsExact, sprintf "%s arrived inexact" (value.GetType().Name))
        Assert.Equal(expected, n.DecimalValue)

[<Fact>]
let ``the extremes of the unsigned types survive`` () =
    Assert.Equal(decimal UInt64.MaxValue, (numberOf (box UInt64.MaxValue)).DecimalValue)
    Assert.Equal(decimal UInt32.MaxValue, (numberOf (box UInt32.MaxValue)).DecimalValue)
    Assert.Equal(decimal SByte.MinValue, (numberOf (box SByte.MinValue)).DecimalValue)

[<Fact>]
let ``a single-precision float converts inexactly, like a double`` () =
    // Widened to double rather than rounded to decimal. A float32 is a binary
    // float and stays one: converting to decimal would read 0.1f back as the
    // exact 0.1 the author wrote, which is precision the value does not carry.
    let n = numberOf (box 1.5f)
    Assert.False(n.IsExact, "a float32 arrived exact")
    Assert.Equal(1.5, n.Value)

[<Theory>]
[<InlineData(Single.NaN)>]
[<InlineData(Single.PositiveInfinity)>]
[<InlineData(Single.NegativeInfinity)>]
let ``a non-finite single is refused, as a double is`` (value: float32) =
    // 30.2.17 (JYRO-REQ-60): a float32 is widened to double through the double
    // constructor, so NaN and the infinities raise the host-number code of
    // 30.2.19 with no argument, not null and not the string "NaN".
    let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(box value) |> ignore)
    let expected = if Single.IsNaN value then MessageCode.HostNumberNaN else MessageCode.HostNumberInfinite
    Assert.Equal(expected, ex.Code)
    Assert.Empty(ex.Args)

[<Fact>]
let ``a numeric primitive reaches a script as a number`` () =
    // The end-to-end shape, which is where this mattered: a host field typed
    // `float` in C# arriving as a Jyro string made every arithmetic use of it
    // wrong.
    let input = JyroObject()
    input.SetProperty("f", JyroValue.FromObject(box 2.5f))
    input.SetProperty("b", JyroValue.FromObject(box (3uy: byte)))
    let v = Runner.expectSuccess "Output.sum = Input.f + Input.b" (input :> JyroValue)
    Assert.Equal(5.5, Runner.asNumber (Runner.prop "sum" v))

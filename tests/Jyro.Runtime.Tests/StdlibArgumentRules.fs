module RealTree.Jyro.Tests.StdlibArgumentRules

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Tests

// Library-wide rules for the arguments the standard library takes, as opposed
// to any one function's behavior.
//
// The audit filed two separate findings here - "integer arguments are
// validated by four functions and coerced by four others" and "RandomInt
// performs no bounds validation". Measuring showed they are one defect with
// three faces, and the largest face was in neither finding: *every* route from
// a Jyro number to a .NET `int` was unchecked at the top end. `IsInteger` is
// true for 1e20, so the four "strict" functions passed their integrality check
// and then overflowed inside `Convert.ToInt32`; the four "lax" ones used F#
// `int`, which wraps silently to `Int32.MinValue` and crashed further in. Nine
// functions surfaced a raw .NET sentence through the `RuntimeError`
// catch-all - "Value was either too large or too small for an Int32" - naming
// neither the function nor the parameter.

let private failsWith (code: MessageCode) (src: string) =
    let codes = Runner.expectFailure ("Output.r = " + src) (JyroObject() :> JyroValue)
    Assert.Contains(code, codes)
    // The point of the exercise: a rejected argument is a diagnostic Jyro
    // authored, never a .NET exception that escaped into the catch-all.
    Assert.DoesNotContain(MessageCode.RuntimeError, codes)

let private succeeds (src: string) =
    Runner.prop "r" (Runner.expectSuccess ("Output.r = " + src) (JyroObject() :> JyroValue))

// ---------------------------------------------------------------------------
// A fractional value is never an index, a count or a width
// ---------------------------------------------------------------------------

[<Theory>]
[<InlineData("Slice([1,2,3], 1.5)")>]
[<InlineData("Slice([1,2,3], 0, 1.5)")>]
[<InlineData("Insert([1,2,3], 1.5, 9)")>]
[<InlineData("RemoveAt([1,2,3], 1.5)")>]
[<InlineData("Round(1.234, 1.5)")>]
[<InlineData("Skip([1,2,3], 1.5)")>]
[<InlineData("PadLeft('a', 5.5)")>]
[<InlineData("PadRight('a', 5.5)")>]
[<InlineData("Substring('abcdef', 2.5)")>]
[<InlineData("Substring('abcdef', 0, 2.5)")>]
[<InlineData("Sleep(1.5)")>]
[<InlineData("RandomString(2.5)")>]
[<InlineData("RandomInt(1.5, 3)")>]
[<InlineData("DateAdd('2024-01-15', 1.5, 'days')")>]
let ``a fractional integer argument is refused`` (src: string) =
    failsWith MessageCode.IntegerRequired src

[<Fact>]
let ``the lax functions had no coherent rule to standardise on`` () =
    // Before the change these four all accepted a fractional argument, and
    // disagreed about what it meant. Skip truncated toward zero, while
    // Substring went through Convert.ToInt32 and rounded half-to-even, so
    // Substring(s, 1.5) and Substring(s, 2.5) both began at index 2. Whichever
    // way a lax rule had been written, half these sites would have changed
    // behavior anyway.
    failsWith MessageCode.IntegerRequired "Substring('abcdef', 1.5)"
    failsWith MessageCode.IntegerRequired "Substring('abcdef', 2.5)"
    failsWith MessageCode.IntegerRequired "Skip([1,2,3,4], 1.5)"

// ---------------------------------------------------------------------------
// Integral but unrepresentable is a different rejection
// ---------------------------------------------------------------------------

[<Theory>]
[<InlineData("Slice([1,2,3], 1e20)")>]
[<InlineData("Slice([1,2,3], 0, 1e20)")>]
[<InlineData("Insert([1,2,3], 1e20, 9)")>]
[<InlineData("RemoveAt([1,2,3], 1e20)")>]
[<InlineData("Round(1.234, 1e20)")>]
[<InlineData("Skip([1,2,3], 1e20)")>]
[<InlineData("PadLeft('a', 1e20)")>]
[<InlineData("PadRight('a', 1e20)")>]
[<InlineData("Substring('abcdef', 1e20)")>]
[<InlineData("Sleep(1e20)")>]
[<InlineData("RandomString(1e20)")>]
[<InlineData("RandomInt(1, 1e20)")>]
[<InlineData("DateAdd('2024-01-15', 1e20, 'days')")>]
let ``an integral value too large to convert is refused by name`` (src: string) =
    // IntegerRequired cannot express this: 1e20 *is* an integer.
    failsWith MessageCode.IntegerOutOfRange src

[<Fact>]
let ``the refusal names the function and the parameter`` () =
    // What the .NET message could not do. "Value was either too large or too
    // small for an Int32" told a script author nothing about which call failed.
    let r = Runner.run "Output.r = Skip([1,2,3], 1e20)" (JyroObject() :> JyroValue)
    let text = List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message) |> String.concat " "
    Assert.Contains("Skip()", text)
    Assert.Contains("count", text)

// ---------------------------------------------------------------------------
// Negative: a quantity and a position are refused differently
// ---------------------------------------------------------------------------

[<Theory>]
[<InlineData("Skip([1,2,3], -1)")>]
[<InlineData("PadLeft('a', -5)")>]
[<InlineData("PadRight('a', -5)")>]
[<InlineData("Substring('abcdef', -2)")>]
[<InlineData("Substring('abcdef', 0, -2)")>]
[<InlineData("Slice([1,2,3], -1)")>]
[<InlineData("Sleep(-1)")>]
[<InlineData("RandomString(-1)")>]
let ``a negative quantity is refused`` (src: string) =
    failsWith MessageCode.NonNegativeIntegerRequired src

[<Theory>]
[<InlineData("Insert([1,2,3], -1, 9)")>]
[<InlineData("Insert([1,2,3], 4, 9)")>]
[<InlineData("RemoveAt([1,2,3], -1)")>]
[<InlineData("RemoveAt([1,2,3], 3)")>]
[<InlineData("RemoveAt([1,2,3], 99)")>]
let ``a position outside the array is refused`` (src: string) =
    failsWith MessageCode.IndexOutOfRange src

[<Fact>]
let ``RemoveAt no longer reports success for an element it did not remove`` () =
    // It filtered on `i <> idx`, so an out-of-range index returned an unchanged
    // copy and the script proceeded believing a removal had happened. Its twin
    // Insert rejected the same index.
    failsWith MessageCode.IndexOutOfRange "RemoveAt([1,2,3], 99)"

[<Fact>]
let ``Insert still admits the append position but RemoveAt does not`` () =
    // The bounds genuinely differ by one: inserting at Length appends, while
    // removing at Length names no element.
    Assert.Equal(4.0, Runner.asNumber (succeeds "Length(Insert([1,2,3], 3, 9))"))
    failsWith MessageCode.IndexOutOfRange "RemoveAt([1,2,3], 3)"

// ---------------------------------------------------------------------------
// What must keep working
// ---------------------------------------------------------------------------

[<Fact>]
let ``clamping a range is still allowed where naming a position is not`` () =
    // Slice and Substring bound a range, so an end past the end means "to the
    // end" and stays legal. That is not the same as accepting a nonsense
    // position, which is why Insert and RemoveAt refuse one. Only the bounds
    // of 90.3.5 (JYRO-REQ-337) are reduced: Slice's finish and start, and
    // Substring's end. Substring's start names a position, so past the text
    // it is refused like Insert's and RemoveAt's (91.10.6).
    Assert.Equal(3.0, Runner.asNumber (succeeds "Length(Slice([1,2,3], 0, 1000))"))
    Assert.Equal(0.0, Runner.asNumber (succeeds "Length(Slice([1,2,3], 99))"))
    failsWith MessageCode.IndexOutOfRange "Substring('abc', 99)"
    Assert.Equal("abc", Runner.asString (succeeds "Substring('abc', 0, 1000)"))

[<Fact>]
let ``a negative digit count still rounds to tens and hundreds`` () =
    // Round's `decimals` is the one integer parameter that is legitimately
    // signed, so it uses the signed helper.
    Assert.Equal(1200.0, Runner.asNumber (succeeds "Round(1234.5, -2)"))

[<Fact>]
let ``DateAdd still accepts a negative amount`` () =
    Assert.Equal("2024-01-14T00:00:00.000Z", Runner.asString (succeeds "DateAdd('2024-01-15', -1, 'days')"))

// ---------------------------------------------------------------------------
// RandomInt
// ---------------------------------------------------------------------------

[<Fact>]
let ``RandomInt refuses a range given back to front`` () =
    // Was `InvalidRandomRange`, a code named for this one function. `Clamp`
    // reaches the same condition, so the general `InvalidRange` replaced it
    // rather than a second code being minted for one concept. The message
    // RandomInt produces is unchanged - the template now takes the function
    // and the two bound names instead of hardcoding them.
    failsWith MessageCode.InvalidRange "RandomInt(5, 3)"

[<Fact>]
let ``RandomInt spans the whole of int32`` () =
    // maxVal + 1 overflowed, so the natural way to ask for "any positive int"
    // raised ArgumentOutOfRangeException through the catch-all.
    succeeds "RandomInt(1, 2147483647)" |> ignore
    succeeds "RandomInt(-2147483648, 2147483647)" |> ignore
    succeeds "RandomInt(-2147483648, 0)" |> ignore

[<Fact>]
let ``RandomInt refuses equal bounds`` () =
    // The range is half-open (JYRO-REQ-2027), so [3, 3) holds no integer, and
    // a min not below the max is InvalidRange (JYRO-REQ-2028).
    failsWith MessageCode.InvalidRange "RandomInt(3, 3)"
    // The one-wide range [3, 4) holds only 3.
    Assert.Equal(3.0, Runner.asNumber (succeeds "RandomInt(3, 4)"))

[<Fact>]
let ``RandomInt stays within its half-open bounds`` () =
    // [10, 12) holds 10 and 11; the max 12 is never returned (JYRO-REQ-2027).
    let output = succeeds "Map(Range(1, 200), i => RandomInt(10, 12))"
    for v in (output :?> JyroArray) do
        let n = Runner.asNumber v
        Assert.InRange(n, 10.0, 11.0)

// ---------------------------------------------------------------------------
// Allocation bounded by an argument's value rather than its size
// ---------------------------------------------------------------------------

[<Fact>]
let ``RandomString cannot allocate an unbounded string from one statement`` () =
    // Same shape as the array index setter (Resource Limits 100.8): the cost is
    // set by the *value* of an argument, so no statement or loop budget can see
    // it coming. PadLeft and PadRight already carried this bound; RandomString
    // shared neither the bound nor the constant until they were unified.
    failsWith MessageCode.GeneratedLengthExceeded "RandomString(500000000)"
    failsWith MessageCode.GeneratedLengthExceeded "PadLeft('a', 500000000)"
    failsWith MessageCode.GeneratedLengthExceeded "PadRight('a', 500000000)"

[<Fact>]
let ``the generation bound is one shared constant`` () =
    // Drifting apart is the failure mode worth pinning: all three report the
    // same maximum.
    let maxima =
        [ "RandomString(500000000)"; "PadLeft('a', 500000000)"; "PadRight('a', 500000000)" ]
        |> List.map (fun src ->
            let r = Runner.run ("Output.r = " + src) (JyroObject() :> JyroValue)
            (List.ofSeq r.Messages |> List.ofSeq |> List.head).Message.Split(' ') |> Array.last)
    Assert.Single(List.distinct maxima) |> ignore

// ---------------------------------------------------------------------------
// The remaining raw-BCL escapes: Clamp, Round and DateAdd
// ---------------------------------------------------------------------------
//
// Three functions still reached a BCL exception on ordinary finite arguments
// and surfaced it through the RuntimeError catch-all, carrying a .NET sentence
// that named neither the function nor the parameter - the exact loss the rest
// of this file was written to remove. None of them needed exotic input.

[<Fact>]
let ``Clamp refuses bounds given back to front`` () =
    // Math.Clamp raises "'min' cannot be greater than 'max'" - three small
    // exact integers were enough. Its sibling RandomInt had the identical
    // defect and was fixed; Clamp was missed.
    failsWith MessageCode.InvalidRange "Clamp(5, 10, 1)"

[<Fact>]
let ``Clamp still clamps when the bounds are the right way round`` () =
    Assert.Equal(5.0, Runner.asNumber (succeeds "Clamp(5, 1, 10)"))
    Assert.Equal(1.0, Runner.asNumber (succeeds "Clamp(0, 1, 10)"))
    Assert.Equal(10.0, Runner.asNumber (succeeds "Clamp(99, 1, 10)"))
    // Equal bounds are a degenerate range, not a reversed one.
    Assert.Equal(3.0, Runner.asNumber (succeeds "Clamp(99, 3, 3)"))

[<Fact>]
let ``Round accepts more decimal places than a double can carry`` () =
    // Math.Round(double, digits, mode) takes at most 15 digits and raised above
    // that, so Round(1.5, 29) crashed. Rounding to more places than the value
    // holds cannot move it.
    Assert.Equal(1.5, Runner.asNumber (succeeds "Round(1.5, 29)"))
    Assert.Equal(1.5, Runner.asNumber (succeeds "Round(1.5, 16)"))
    Assert.Equal(0.1, Runner.asNumber (succeeds "Round(0.1, 100)"))

[<Fact>]
let ``Round to more places than a tiny value occupies still rounds it away`` () =
    // The half-open other side: a value below half the place being rounded to
    // goes to zero, so the answer is not simply the identity.
    Assert.Equal(0.0, Runner.asNumber (succeeds "Round(1e-30, 20)"))

[<Fact>]
let ``Round keeps an exact value exact past the double digit limit`` () =
    // The branch returns the argument rather than rebuilding it from the double
    // view, so exactness survives.
    let v = Runner.expectSuccess "Output.r = Round(Input.id, 29)" (Runner.objOf [ "id", Runner.num 1.0 ])
    Assert.Equal(1.0, Runner.asNumber (Runner.prop "r" v))

[<Fact>]
let ``ordinary rounding is untouched`` () =
    Assert.Equal(3.14, Runner.asNumber (succeeds "Round(3.14159, 2)"))
    Assert.Equal(2.0, Runner.asNumber (succeeds "Round(1.5, 0)"))
    Assert.Equal(100.0, Runner.asNumber (succeeds "Round(123, 0 - 2)"))

[<Fact>]
let ``DateAdd reports a result outside the range a date can hold`` () =
    // The amount is a valid integer and the unit is valid; it is the result
    // that has nowhere to go. Every arm can reach it, not just one.
    // The day and hour counts have to be genuinely large: a date runs to the
    // year 9999, so 1,000,000 days is only about 2,700 years and lands well
    // inside it.
    failsWith MessageCode.DateOutOfRange "DateAdd('2024-01-15', 20000, 'years')"
    failsWith MessageCode.DateOutOfRange "DateAdd('2024-01-15', 200000, 'months')"
    failsWith MessageCode.DateOutOfRange "DateAdd('2024-01-15', 4000000, 'days')"
    failsWith MessageCode.DateOutOfRange "DateAdd('2024-01-15', 500000, 'weeks')"
    failsWith MessageCode.DateOutOfRange "DateAdd('2024-01-15', 100000000, 'hours')"

[<Fact>]
let ``DateAdd still accepts a large amount that stays inside the range`` () =
    // The boundary matters in both directions: this must not become a blanket
    // refusal of anything big. A million days is about 2,700 years, so it
    // lands in the 4700s - asserted as the year rather than a full timestamp,
    // which is the fact the test is about.
    Assert.StartsWith("4761-", Runner.asString (succeeds "DateAdd('2024-01-15', 1000000, 'days')"))

[<Fact>]
let ``DateAdd distinguishes an unusable amount from an unusable result`` () =
    // 1e20 is not a representable int at all, which is a different correction
    // from an int that produces an impossible date, so the two keep separate
    // codes.
    failsWith MessageCode.IntegerOutOfRange "DateAdd('2024-01-15', 1e20, 'days')"
    failsWith MessageCode.DateOutOfRange "DateAdd('2024-01-15', 20000, 'years')"

[<Fact>]
let ``DateAdd still adds ordinary amounts`` () =
    Assert.Equal("2024-01-16T00:00:00.000Z", (succeeds "DateAdd('2024-01-15', 1, 'days')").ToStringValue())
    Assert.Equal("2025-01-15T00:00:00.000Z", (succeeds "DateAdd('2024-01-15', 1, 'years')").ToStringValue())

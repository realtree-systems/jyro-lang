module RealTree.Jyro.Tests.FiniteNumbers

open System
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

// A Jyro number is finite. NaN and the infinities are not Jyro numbers.
//
// The defect this closes: `Log(0)` returned -Infinity, the run reported
// IsSuccess = true, and `ToJson()` on that result then raised a BCL
// ArgumentException from Utf8JsonWriter - so a successful run handed the host
// a value the engine could not serialize, and the failure arrived through the
// documented way of getting the result out, as an exception type the
// documented `catch (JyroRuntimeException)` does not see.
//
// The rule is not new. The specification already recorded it as the rationale
// for DivisionByZero - "Producing `Infinity` or `NaN` silently is hazardous in
// script contexts; raising at the operator boundary makes the cause obvious to
// the host" - a sentence written about *producing* those values, under a guard
// that only ever tested the divisor. This is that rule reaching the rest of
// its own scope.
//
// Enforced as an invariant at the single double constructor, with every route
// reporting first in the vocabulary its own family uses: the computation
// family raises, the parse family returns null (clause 90.5), and the ingest
// doors refuse what they are handed.

let private codeOf (f: unit -> unit) =
    let ex = Assert.Throws<JyroRuntimeException>(f)
    ex.Code

let private runFail (src: string) = Runner.expectFailure src (JyroObject() :> JyroValue)
let private runOk (src: string) = Runner.expectSuccess src (JyroObject() :> JyroValue)

[<Fact>]
let ``the double constructor refuses every non-finite value`` () =
    // 30.2.17 and 30.2.19 (JYRO-REQ-60, JYRO-REQ-1700): a JyroRuntimeException
    // of HostNumberNaN or HostNumberInfinite, with no argument.
    for (v, code) in [ Double.NaN, MessageCode.HostNumberNaN
                       Double.PositiveInfinity, MessageCode.HostNumberInfinite
                       Double.NegativeInfinity, MessageCode.HostNumberInfinite ] do
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroNumber(v) |> ignore)
        Assert.Equal(code, ex.Code)
        Assert.Empty(ex.Args)

[<Fact>]
let ``finite values at the extremes are still numbers`` () =
    // The rule is finiteness, not magnitude. Double.MaxValue is a fine number.
    for v in [ Double.MaxValue; Double.MinValue; Double.Epsilon; 0.0; -0.0; 1e300 ] do
        Assert.Equal(JyroValueType.Number, JyroNumber(v).ValueType)

[<Fact>]
let ``the reported defect is gone: a run either fails or yields serializable output`` () =
    // Previously: IsSuccess = true, then ToJson() threw ArgumentException.
    Assert.Contains(MessageCode.NumberDomainError, runFail "Output.v = Log(0)")
    Assert.Contains(MessageCode.NumberOverflow, runFail "Output.v = Power(1e300, 3)")

[<Fact>]
let ``every successful run produces output ToJson can write`` () =
    // The property the invariant exists to guarantee, exercised over the
    // operations that used to break it.
    for src in [ "Output.v = Power(2, 10)"
                 "Output.v = Log(100)"
                 "Output.v = SquareRoot(16)"
                 "Output.v = 1e300"
                 "Output.v = 1 / 3"
                 "Output.v = Round(5, 0 - 400)" ] do
        let out = runOk src
        // Would have thrown ArgumentException for a non-finite result.
        out.ToJson() |> ignore

// -- Computation family: raise, and distinguish the two failures ------------

[<Fact>]
let ``a domain error names the function, the parameter and its value`` () =
    // Reported as a diagnostic on the result, not thrown to the caller - the
    // runtime converts it, which is the whole point of it being a Jyro error.
    // Its arguments name the function and the parameter that is out of its
    // domain, followed by the value as the script gave it (130.6.4,
    // JYRO-REQ-2055, JYRO-ADR-123), not an English phrase for the condition
    // (130.6.2, JYRO-REQ-2054, JYRO-ADR-125).
    let r = Runner.run "Output.v = Log(0)" (JyroObject() :> JyroValue)
    Assert.False(r.IsSuccess)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.NumberDomainError)
    let args =
        m.Args
        |> Array.map (fun a -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture))
        |> List.ofArray
    Assert.Contains("Log()", args)
    let at = List.findIndex ((=) "value") args
    Assert.True(at + 1 < args.Length, sprintf "no value follows 'value' in %A" args)
    Assert.Equal("0", args.[at + 1])
    Assert.DoesNotContain("base", args)

[<Fact>]
let ``domain and overflow are separate codes because they are corrected differently`` () =
    // Guard the input vs scale the computation - the same split as
    // IntegerRequired / IntegerOutOfRange.
    Assert.Contains(MessageCode.NumberDomainError, runFail "Output.v = SquareRoot(0 - 1)")
    Assert.Contains(MessageCode.NumberOverflow, runFail "Output.v = Power(1e300, 3)")

[<Fact>]
let ``Log rejects a base that has no logarithm`` () =
    Assert.Contains(MessageCode.NumberDomainError, runFail "Output.v = Log(10, 1)")
    Assert.Contains(MessageCode.NumberDomainError, runFail "Output.v = Log(10, 0)")

[<Fact>]
let ``the operators are covered, not just the math functions`` () =
    // The DivisionByZero guard tests only the divisor, so these reach a
    // non-finite result with entirely ordinary operands and a nonzero divisor.
    Assert.Contains(MessageCode.NumberOverflow, runFail "Output.v = 1e300 / 1e-28")
    Assert.Contains(MessageCode.NumberOverflow, runFail "Output.v = 1e300 * 1e300")

[<Fact>]
let ``division by zero still reports its own code, not the new one`` () =
    // The pre-existing, more specific diagnostic must keep winning.
    Assert.Contains(MessageCode.DivisionByZero, runFail "Output.v = 1 / 0")
    Assert.Contains(MessageCode.ModuloByZero, runFail "Output.v = 1 % 0")

[<Fact>]
let ``an aggregate whose total overflows reports rather than returning infinity`` () =
    // 1e308 is finite; the sum of two is not. It takes values this near the
    // top of the range to overflow an addition, which is why the aggregate
    // guards are backstops rather than commonly-hit paths.
    Assert.Contains(MessageCode.NumberOverflow, runFail "Output.v = Sum([1e308, 1e308])")
    // ...and a total that stays representable must still be produced.
    Assert.Equal(4e300, Runner.asNumber ((runOk "Output.v = Sum([1e300, 1e300, 1e300, 1e300])").GetProperty "v"))

// -- Parse family: null, per clause 90.5 ------------------------------------

[<Fact>]
let ``ToNumber returns null for text with no finite value`` () =
    // Not a report: the script has no other way to ask, and the coalesce
    // idiom the docs teach must fire. Double.TryParse returns *true* with
    // Infinity for these, so before the fix they became numbers.
    for text in [ "1e400"; "-1e400"; "Infinity"; "-Infinity"; "NaN" ] do
        let out = runOk (sprintf "Output.v = ToNumber('%s')" text)
        Assert.Equal(JyroValueType.Null, (out.GetProperty "v").ValueType)

[<Fact>]
let ``the coalesce idiom now actually fires`` () =
    // Before: ToNumber("1e400") was +Infinity, so `?? 0` never fired and the
    // run failed later at the JSON boundary instead.
    let out = runOk "Output.v = ToNumber('1e400') ?? 0"
    Assert.Equal(0.0, Runner.asNumber (out.GetProperty "v"))

[<Fact>]
let ``ToNumber still parses ordinary text`` () =
    Assert.Equal(42.5, Runner.asNumber ((runOk "Output.v = ToNumber('42.5')").GetProperty "v"))

// -- Ingest doors: refuse what they are handed ------------------------------

[<Fact>]
let ``a source literal with no finite value is refused where it is written`` () =
    // `1e400` used to compile to +Infinity, so the file ran and then failed at
    // the JSON boundary. It is a parse-stage refusal now.
    let r = Runner.run "Output.v = 1e400" (JyroObject() :> JyroValue)
    Assert.False(r.IsSuccess)

[<Fact>]
let ``JSON input carrying a non-finite magnitude is refused at the door`` () =
    // Well-formed JSON: the grammar bounds no exponent. TryGetDouble reads it
    // as +Infinity.
    Assert.Equal(MessageCode.NumberOverflow,
                 codeOf (fun () -> JyroValue.FromJson("{\"v\": 1e400}") |> ignore))

[<Fact>]
let ``ordinary JSON numbers are unaffected`` () =
    let v = JyroValue.FromJson("{\"a\": 1, \"b\": 2.5, \"c\": 1e300, \"d\": 9007199254740993}")
    Assert.Equal(1.0, (v.GetProperty "a").ToDouble())
    Assert.Equal(2.5, (v.GetProperty "b").ToDouble())
    Assert.Equal(JyroValueType.Number, (v.GetProperty "c").ValueType)
    // The exact-decimal path must not have been disturbed.
    Assert.Equal("9007199254740993", (v.GetProperty "d").ToStringValue())

/// 30.2.17 (JYRO-REQ-60) replaced the old rule that FromObject turned a
/// non-finite double into null: a host member that makes a Jyro value from a
/// host value refuses NaN and the infinities, with the code 30.2.19
/// (JYRO-REQ-1700) decides and no argument. Null would hide the host's fault.
[<Fact>]
let ``FromObject refuses a non-finite double with the host-number code`` () =
    for (v, code) in [ Double.NaN, MessageCode.HostNumberNaN
                       Double.PositiveInfinity, MessageCode.HostNumberInfinite
                       Double.NegativeInfinity, MessageCode.HostNumberInfinite ] do
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> JyroValue.FromObject(box v) |> ignore)
        Assert.Equal(code, ex.Code)
        Assert.Empty(ex.Args)
    // A finite double still converts.
    Assert.Equal(JyroValueType.Number, (JyroValue.FromObject(1.5)).ValueType)

[<Fact>]
let ``the constructor and FromObject refuse alike`` () =
    // Both doors are host members of 30.2.17: neither normalizes.
    Assert.Equal(MessageCode.HostNumberNaN, codeOf (fun () -> JyroNumber(Double.NaN) |> ignore))
    Assert.Equal(MessageCode.HostNumberNaN, codeOf (fun () -> JyroValue.FromObject(Double.NaN) |> ignore))

[<Fact>]
let ``WithInput reports a host value it cannot represent, before the first statement`` () =
    // 30.2.18 (JYRO-REQ-1948): the run fails with one diagnostic of the host
    // code, and WithInput itself does not raise.
    let compiled = JyroCompiler().WithStdlib().WithSource("Output.v = Input").CompileOrThrow()
    let r = JyroRunner(compiled).WithInput(box Double.NaN).Execute()
    Assert.False(r.IsSuccess, "a NaN Input ran")
    Assert.Equal<MessageCode list>([ MessageCode.HostNumberNaN ], r.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq)

[<Fact>]
let ``equality and ordering give one answer`` () =
    // EqualsValue used to test `abs(a - b) < Double.Epsilon`, a second answer
    // beside Compare. For finite values the two coincide - Double.Epsilon is
    // the smallest subnormal, so the test was exactly `a = b` - which is why
    // collapsing them changed nothing that can still be observed.
    let pairs = [ 1.0, 1.0; 1.0, 2.0; 0.0, -0.0; 1e300, 1e300; Double.MaxValue, Double.MaxValue ]
    for (a, b) in pairs do
        let x, y = JyroNumber(a), JyroNumber(b)
        Assert.Equal(JyroNumber.Compare(x, y) = 0, x.EqualsValue(y))

[<Fact>]
let ``every number is equal to itself`` () =
    // Was false for the infinities, which is wrong even by the IEEE contract
    // the old documentation claimed to follow. They are now unconstructible,
    // so this holds by two independent mechanisms.
    for v in [ 0.0; 1.0; -1.0; 1e300; Double.MaxValue; Double.Epsilon ] do
        let n = JyroNumber(v)
        Assert.True(n.EqualsValue(n))

// -- Round: the independent bug this pass turned up -------------------------

[<Fact>]
let ``rounding to an absurd number of places gives zero, not NaN`` () =
    // Math.Pow(10, 400) is +Infinity, after which value/Infinity is 0.0 and
    // 0.0 * Infinity is NaN - so Round fabricated a NaN from two finite
    // arguments. Rounding 5 to the nearest 10^400 has a correct answer.
    Assert.Equal(0.0, Runner.asNumber ((runOk "Output.v = Round(5, 0 - 400)").GetProperty "v"))
    Assert.Equal(0.0, Runner.asNumber ((runOk "Output.v = Round(123456, 0 - 320)").GetProperty "v"))

[<Fact>]
let ``ordinary rounding is unchanged`` () =
    Assert.Equal(3.14, Runner.asNumber ((runOk "Output.v = Round(3.14159, 2)").GetProperty "v"))
    Assert.Equal(100.0, Runner.asNumber ((runOk "Output.v = Round(123, 0 - 2)").GetProperty "v"))
    Assert.Equal(0.0, Runner.asNumber ((runOk "Output.v = Round(4, 0 - 1)").GetProperty "v"))

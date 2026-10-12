module RealTree.Jyro.Tests.ArgumentHelpers

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Tests

/// The `JyroFunctionBase` helper family is the host-facing surface: a custom
/// function is the one place a host touches a Jyro value directly, so whatever
/// these helpers do is what the engine's guarantees are worth in practice.
///
/// The audit filed them as "three different failure models". Two of the three
/// turned out to be principled - a container helper treats null as the empty
/// container, and a number helper refuses null because a number has no
/// identity to fall back on, which is the same distinction clause 90.7 draws
/// for aggregates. The outliers were the two that coerced anything, and a
/// fourth model the entry did not name: `GetOptional` silently
/// substituted its default for an argument of the wrong type.

// ---------------------------------------------------------------------------
// A harness that exposes one helper per function
// ---------------------------------------------------------------------------

type private DecimalProbe() =
    inherit JyroFunctionBase("ExactOf", JyroFunctionSignature.Create(ParameterType.String, Parameter.Required("value", ParameterType.Number)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _) =
        JyroString(args.GetDecimal(0).ToString(Globalization.CultureInfo.InvariantCulture)) :> JyroValue

type private DoubleProbe() =
    inherit JyroFunctionBase("DoubleOf", JyroFunctionSignature.Create(ParameterType.String, Parameter.Required("value", ParameterType.Number)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _) =
        JyroString(args.GetNumber(0).ToString("R", Globalization.CultureInfo.InvariantCulture)) :> JyroValue

type private Int64Probe() =
    inherit JyroFunctionBase("Int64Of", JyroFunctionSignature.Create(ParameterType.String, Parameter.Required("value", ParameterType.Number)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _) =
        JyroString(string (args.GetInt64(0))) :> JyroValue

type private BooleanProbe() =
    inherit JyroFunctionBase("BoolOf", JyroFunctionSignature.Create(ParameterType.Boolean, Parameter.Required("value", ParameterType.Any)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _) =
        JyroBoolean.FromBoolean(args.GetBoolean(0)) :> JyroValue

type private OptionalProbe() =
    inherit JyroFunctionBase("OptOf",
        JyroFunctionSignature.Create(ParameterType.String, Parameter.Optional("value", ParameterType.Any)))
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _) =
        JyroString((args.GetOptional(0, JyroString("fallback"))).Value) :> JyroValue

let private runWith (src: string) =
    Runner.compileThenRun
        (JyroCompiler()
            .WithStdlib()
            .AddFunctions([ DecimalProbe(); DoubleProbe(); Int64Probe(); BooleanProbe(); OptionalProbe() ])
            .WithSource("Output.r = " + src))
        (fun r -> r.WithInput(JyroValue.FromJson("""{"obj":{"a":1},"arr":[1,2]}""")))

let private value (src: string) =
    let r = runWith src
    Assert.True(r.IsSuccess, $"%A{List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)}")
    Runner.prop "r" r.TryValue.Value

let private codes (src: string) =
    let r = runWith src
    Assert.False(r.IsSuccess, $"expected failure from {src}")
    List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)

// ---------------------------------------------------------------------------
// Exactness at the host boundary
// ---------------------------------------------------------------------------

[<Theory>]
[<InlineData("9007199254740993")>]      // 2^53 + 1
[<InlineData("1234567890123456789")>]
[<InlineData("9223372036854775807")>]   // Int64.MaxValue
let ``GetDecimal keeps a large integer the double view loses`` (literal: string) =
    // The README promises "Large integer IDs round-trip losslessly ... no
    // silent corruption above 2^53". JyroNumber stores these exactly; before
    // GetDecimal existed, the documented way for a host to read one
    // returned a double and quietly changed it.
    Assert.Equal(literal, Runner.asString (value $"ExactOf({literal})"))
    Assert.NotEqual<string>(literal, Runner.asString (value $"DoubleOf({literal})"))

[<Theory>]
[<InlineData("19.99")>]
[<InlineData("0.1")>]
[<InlineData("1.005")>]
let ``GetDecimal keeps money exact`` (literal: string) =
    Assert.Equal(literal, Runner.asString (value $"ExactOf({literal})"))

[<Fact>]
let ``GetInt64 carries a value int cannot hold`` () =
    // The 64-bit sibling that did not exist. A host reading a timestamp wrote
    // `int64 (GetNumber …)`, which is lossy above 2^53 and, being an
    // unchecked conversion, silently produced Int64.MinValue out of range.
    Assert.Equal("9223372036854775807", Runner.asString (value "Int64Of(9223372036854775807)"))
    Assert.Equal("-9223372036854775808", Runner.asString (value "Int64Of(-9223372036854775808)"))

[<Fact>]
let ``GetInt64 applies both integer checks`` () =
    Assert.Contains(MessageCode.IntegerRequired, codes "Int64Of(1.5)")
    Assert.Contains(MessageCode.IntegerOutOfRange, codes "Int64Of(1e30)")
    Assert.DoesNotContain(MessageCode.RuntimeError, codes "Int64Of(1e30)")

[<Fact>]
let ``GetDecimal refuses a value decimal cannot hold`` () =
    // Rather than letting the conversion raise OverflowException into the
    // RuntimeError catch-all.
    Assert.Contains(MessageCode.IntegerOutOfRange, codes "ExactOf(1e30)")
    Assert.DoesNotContain(MessageCode.RuntimeError, codes "ExactOf(1e30)")

// ---------------------------------------------------------------------------
// One rule across the family: null is absent, a wrong type is an error
// ---------------------------------------------------------------------------

[<Theory>]
[<InlineData("ToUpper(123)", "123")>]
[<InlineData("Trim(true)", "true")>]
[<InlineData("Trim(null)", "")>]
let ``a string parameter takes the canonical text of a scalar`` (src: string, expected: string) =
    // Each scalar has one written form a script author would recognize and
    // intend, and null yields the identity - the same choice the container
    // helpers make with their empty container.
    Assert.Equal(expected, Runner.asString (value src))

[<Theory>]
[<InlineData("ToUpper(Input.obj)")>]
[<InlineData("ToUpper(Input.arr)")>]
[<InlineData("Replace(Input.obj, 'a', 'b')")>]
[<InlineData("StartsWith(Input.obj, '{')")>]
[<InlineData("PadLeft(Input.obj, 12, '.')")>]
let ``a string parameter refuses a container`` (src: string) =
    // These used to succeed by rendering the container to its debug form and
    // then string-processing it: ToUpper(obj) returned "{A: 1}" with the key
    // upper-cased, and Replace(obj, "a", "b") returned "{b: 1}", having renamed
    // a property by substitution and reported success.
    Assert.Contains(MessageCode.ArgumentTypeMismatch, codes src)

[<Fact>]
let ``ToString still renders anything, because that is its job`` () =
    // The narrowing is of the *helper*, not of the language's ability to
    // stringify. ToString declares AnyParam and reads the argument directly.
    // Of an array or an object it gives what ToJson gives (95.3.6, JYRO-REQ-1085).
    Assert.Equal("{\"a\":1}", Runner.asString (value "ToString(Input.obj)"))
    Assert.Equal("[1,2]", Runner.asString (value "ToString(Input.arr)"))

[<Theory>]
[<InlineData("BoolOf(true)", true)>]
[<InlineData("BoolOf(false)", false)>]
let ``a boolean parameter takes a boolean`` (src: string, expected: bool) =
    Assert.Equal(expected, Runner.asBool (value src))

[<Fact>]
let ``JYRO-REQ-2097 a boolean parameter refuses null with ArgumentTypeMismatch naming boolean then null`` () =
    // 160.3.2: a helper that reads a boolean has no boolean that means "no
    // boolean", so null is a type mismatch, not false (JYRO-DEF-520). The
    // diagnostic names the expected type before the type it got (130.6.5).
    let r = runWith "BoolOf(null)"
    Assert.False(r.IsSuccess, "BoolOf(null) must fail")
    let mismatch =
        r.Messages |> List.ofSeq |> List.tryFind (fun m -> m.Code = MessageCode.ArgumentTypeMismatch)
    Assert.True(mismatch.IsSome,
        $"expected ArgumentTypeMismatch, got %A{r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)}")
    let text = mismatch.Value.Message
    let expected = text.IndexOf("boolean", StringComparison.Ordinal)
    Assert.True(expected >= 0, $"the message must name boolean: {text}")
    let got = text.IndexOf("null", expected + "boolean".Length, StringComparison.Ordinal)
    Assert.True(got > expected, $"the message must name null after boolean: {text}")

[<Theory>]
[<InlineData("BoolOf(1)")>]
[<InlineData("BoolOf('yes')")>]
[<InlineData("BoolOf(Input.arr)")>]
let ``a boolean parameter refuses a value that merely has a truthiness`` (src: string) =
    // A helper named for a type should return that type's value. Truthiness is
    // a language rule, available directly to anyone who wants it.
    Assert.Contains(MessageCode.ArgumentTypeMismatch, codes src)

[<Fact>]
let ``an omitted optional argument takes the default and a wrong one does not`` () =
    // The fourth failure model the audit did not name. Substituting the default
    // for a wrong type conflates "the caller omitted this" with "the caller got
    // it wrong", and a default is only an answer to the first.
    Assert.Equal("fallback", Runner.asString (value "OptOf()"))
    Assert.Equal("fallback", Runner.asString (value "OptOf(null)"))
    Assert.Equal("given", Runner.asString (value "OptOf('given')"))
    Assert.Contains(MessageCode.ArgumentTypeMismatch, codes "OptOf(123)")

[<Fact>]
let ``the container helpers keep treating null as the empty container`` () =
    // Unchanged, and the precedent the rest of the family was aligned to.
    Assert.Equal(0.0, Runner.asNumber (value "Length(null)"))
    Assert.Equal(0.0, Runner.asNumber (value "Length(Sort(null))"))
    Assert.Equal(0.0, Runner.asNumber (value "Length(Keys(null))"))

[<Fact>]
let ``a number parameter still refuses null, because a number has no identity`` () =
    // Not an inconsistency with the container helpers but the same rule as
    // clause 90.7: an empty array is a fine array, whereas there is no number
    // that means "no number".
    Assert.Contains(MessageCode.ArgumentTypeMismatch, codes "Absolute(null)")

// ---------------------------------------------------------------------------
// Diagnostics speak the language's vocabulary
// ---------------------------------------------------------------------------

[<Fact>]
let ``a type mismatch names Jyro types, not CLR types`` () =
    // The generic helpers reported typeof<'T>.Name, so a script author who has
    // never heard of JyroNumber was told "Expected JyroNumber but got String" -
    // half the message in the implementation's vocabulary - while the
    // non-generic helper beside it already said "number".
    let text (src: string) =
        (runWith src).Messages |> List.ofSeq |> List.map (fun m -> m.Message) |> String.concat " "
    Assert.Contains("number", text "Floor('12')")
    Assert.DoesNotContain("JyroNumber", text "Floor('12')")
    Assert.DoesNotContain("JyroString", text "Sort('abc')")

module RealTree.Jyro.Tests.ExactNumberTests

open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// Regression tests for the exact (decimal) number representation: JSON
// integers/decimals within decimal range round-trip losslessly, money
// arithmetic is exact, and values beyond int64 range no longer crash
// serialization. See the audit finding on float-backed numbers.

let private run (script: string) (inputJson: string) : JyroResult<JyroValue> =
    Runner.compileThenRun
        (JyroCompiler().WithStdlib().WithSource(script))
        (fun r -> r.WithInputJson(inputJson))

/// Execute and return the raw JSON token text of Output.<prop>.
let private rawProp (script: string) (inputJson: string) (prop: string) : string =
    let r = run script inputJson
    Assert.True(r.IsSuccess, sprintf "script failed: %A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    match r.TryValue with
    | Some v ->
        use doc = JsonDocument.Parse(v.ToJson())
        doc.RootElement.GetProperty(prop).GetRawText()
    | None -> "null"

[<Fact>]
let ``large int64 id round-trips exactly through JSON`` () =
    // A Discord/Twitter-style snowflake ID, well above 2^53 - previously
    // silently corrupted to 175928847299117056.
    Assert.Equal("175928847299117063", rawProp "Output.id = Input.id" """{"id": 175928847299117063}""" "id")

[<Fact>]
let ``2^53 + 1 integer is preserved`` () =
    Assert.Equal("9007199254740993", rawProp "Output.n = Input.n" """{"n": 9007199254740993}""" "n")

[<Fact>]
let ``Int64.MaxValue round-trips without crashing serialization`` () =
    // Previously WriteJson threw OverflowException converting the 2^63 double.
    Assert.Equal("9223372036854775807", rawProp "Output.n = Input.n" """{"n": 9223372036854775807}""" "n")

[<Fact>]
let ``money value round-trips exactly`` () =
    Assert.Equal("19.99", rawProp "Output.p = Input.p" """{"p": 19.99}""" "p")

[<Fact>]
let ``money addition is exact`` () =
    Assert.Equal("0.3", rawProp "Output.total = Input.a + Input.b" """{"a": 0.1, "b": 0.2}""" "total")

[<Fact>]
let ``0.1 + 0.2 equals 0.3`` () =
    let r = run "Output.eq = (0.1 + 0.2) == 0.3" "{}"
    Assert.True(r.IsSuccess)
    match r.TryValue with
    | Some v ->
        use doc = JsonDocument.Parse(v.ToJson())
        Assert.True(doc.RootElement.GetProperty("eq").GetBoolean())
    | None -> Assert.Fail "no output"

[<Fact>]
let ``Sum of money array is exact`` () =
    Assert.Equal("0.6", rawProp "Output.total = Sum([0.1, 0.2, 0.3])" "{}" "total")

[<Fact>]
let ``Average of exact values is exact`` () =
    Assert.Equal("2.5", rawProp "Output.avg = Average([1, 2, 3, 4])" "{}" "avg")

[<Fact>]
let ``value beyond decimal range serializes without crashing`` () =
    // 1e30 exceeds decimal's ~7.9e28 range, so it is an inexact integer-valued
    // double >= 2^63 - the exact case that used to throw on serialization.
    let r = run "Output.x = Input.x" """{"x": 1000000000000000000000000000000}"""
    Assert.True(r.IsSuccess, sprintf "script failed: %A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    match r.TryValue with
    | Some v ->
        use doc = JsonDocument.Parse(v.ToJson())
        Assert.Equal(1e30, doc.RootElement.GetProperty("x").GetDouble())
    | None -> Assert.Fail "no output"

[<Fact>]
let ``exact literals survive a .jyrx round-trip`` () =
    let bytesResult =
        JyroCompiler()
            .WithStdlib()
            .WithSource("Output.id = 175928847299117063\nOutput.price = 19.99")
            .CompileToArtifact()
    Assert.True(bytesResult.IsSuccess, "compile-to-bytes failed")
    let bytes = Option.get bytesResult.TryValue

    let r =
        Runner.loadThenRun
            (JyroArtifactLoader(bytes).WithStdlib())
            (fun r -> r.WithInputJson("{}"))
    Assert.True(r.IsSuccess, sprintf "execute-from-bytes failed: %A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    match r.TryValue with
    | Some v ->
        use doc = JsonDocument.Parse(v.ToJson())
        Assert.Equal("175928847299117063", doc.RootElement.GetProperty("id").GetRawText())
        Assert.Equal("19.99", doc.RootElement.GetProperty("price").GetRawText())
    | None -> Assert.Fail "no output"

// ---------------------------------------------------------------------------
// Conformance to Type System 30.2 and 30.11
// ---------------------------------------------------------------------------

// The clauses above were rewritten to describe this behavior after the
// specification was found to require the opposite - double-only arithmetic,
// with exactness permitted only as a serialization hint that "MUST NOT
// influence the result of any arithmetic, comparison or coercion". These
// tests pin the claims the new text makes normative, since a specification
// that no test enforces is how the two drifted apart in the first place.

[<Fact>]
let ``the exact range covers at least 96 bits of integer significand`` () =
    // 30.2 states the floor as a capability, not as a named format. This is
    // 2^96 - 1: any signed or unsigned 64-bit integer has ten orders of
    // magnitude to spare inside it.
    Assert.Equal(
        "79228162514264337593543950335",
        rawProp "Output.n = 79228162514264337593543950335" "{}" "n")

[<Fact>]
let ``a literal beyond the exact range is held inexactly rather than refused`` () =
    // Demotion keeps the operation total. The alternatives 30.2 rejects are
    // refusing the value, and growing without bound - which clause 100.8
    // forbids, since the width would be set by the script.
    let raw = rawProp "Output.n = 79228162514264337593543950336" "{}" "n"
    Assert.Contains("E+28", raw)

[<Fact>]
let ``an exact operation whose result overflows falls back to double`` () =
    let raw = rawProp "Output.n = 79228162514264337593543950335 * 10" "{}" "n"
    Assert.Contains("E+", raw)

[<Fact>]
let ``one inexact operand demotes the whole expression`` () =
    // The consequence a script author has to know, stated in 30.2: the odd
    // unit is lost because 1e0 is inexact, not because the sum is too large.
    Assert.Equal("9007199254740994", rawProp "Output.n = 9007199254740993 + 1e0" "{}" "n")
    Assert.Equal("9007199254740993", rawProp "Output.n = 9007199254740992 + 1" "{}" "n")

[<Fact>]
let ``two exact values distinguishable only beyond double precision are unequal`` () =
    // The claim that separates this model from a double-backed one. Both
    // operands round to the same double, so the superseded 30.11 rule would
    // have made this true.
    Assert.Equal("false", rawProp "Output.b = 9007199254740993 == 9007199254740992" "{}" "b")
    Assert.Equal("true", rawProp "Output.b = 9007199254740993 != 9007199254740992" "{}" "b")

[<Fact>]
let ``relational comparison orders exact values exactly`` () =
    Assert.Equal("true", rawProp "Output.b = 9007199254740993 > 9007199254740992" "{}" "b")
    Assert.Equal("false", rawProp "Output.b = 9007199254740993 <= 9007199254740992" "{}" "b")

[<Fact>]
let ``equality is exact rather than approximate on both branches`` () =
    // 30.11 deliberately offers no tolerance. 0.1 + 0.2 == 0.3 is true here
    // because the arithmetic was exact, not because the comparison was lenient.
    Assert.Equal("true", rawProp "Output.b = 0.1 + 0.2 == 0.3" "{}" "b")
    Assert.Equal("false", rawProp "Output.b = 0.1 + 0.2 == 0.30000000001" "{}" "b")

[<Fact>]
let ``the representation is not observable from script`` () =
    // 30.2 requires one type at the language level: TypeOf reports "number"
    // whichever representation is in use, and no operation reveals which.
    Assert.Equal("\"number\"", rawProp "Output.t = TypeOf(9007199254740993)" "{}" "t")
    Assert.Equal("\"number\"", rawProp "Output.t = TypeOf(1e30)" "{}" "t")
    Assert.Equal("\"number\"", rawProp "Output.t = TypeOf(0.1)" "{}" "t")

[<Fact>]
let ``an inexact literal survives a .jyrx round-trip as inexact`` () =
    // The companion to the exact case above. Artifact Format 150 requires the
    // representation byte to round-trip, not merely the value: an earlier
    // revision specified eight bytes of double for every number, which would
    // have made a script mean one thing from source and another from .jyrx.
    let bytesResult =
        JyroCompiler().WithStdlib().WithSource("Output.big = 1e30\nOutput.id = 9007199254740993").CompileToArtifact()
    Assert.True(bytesResult.IsSuccess, "compile-to-bytes failed")
    let r =
        Runner.loadThenRun
            (JyroArtifactLoader(Option.get bytesResult.TryValue).WithStdlib())
            (fun r -> r.WithInputJson("{}"))
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    use doc = JsonDocument.Parse((Option.get r.TryValue).ToJson())
    Assert.Contains("E+30", doc.RootElement.GetProperty("big").GetRawText())
    Assert.Equal("9007199254740993", doc.RootElement.GetProperty("id").GetRawText())

// ---------------------------------------------------------------------------
// Clone preserves the representation, not just the value
// ---------------------------------------------------------------------------

[<Fact>]
let ``Clone keeps an exact number exact`` () =
    // `Clone` took the double view (`n.Value`), so the one function whose whole
    // job is to preserve a value was the one that rounded it. The copy came
    // back as 9007199254740992 while the original round-tripped correctly, and
    // it also flipped IsExact - so the clone serialized differently from then
    // on, silently, and only on the copy.
    let r = run "Output.original = Input.id\nOutput.copy = Clone(Input.id)" "{\"id\": 9007199254740993}"
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))
    use doc = JsonDocument.Parse((Option.get r.TryValue).ToJson())
    Assert.Equal("9007199254740993", doc.RootElement.GetProperty("original").GetRawText())
    Assert.Equal("9007199254740993", doc.RootElement.GetProperty("copy").GetRawText())

[<Fact>]
let ``Clone keeps exactness through a nested container`` () =
    // The scalar arm is reached recursively, so the deep case is the one a host
    // actually hits - cloning a document, not a bare number.
    let r =
        run "Output.copy = Clone(Input.doc)"
            "{\"doc\": {\"id\": 9007199254740993, \"amounts\": [0.1, 12345678901234567890]}}"
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))
    use doc = JsonDocument.Parse((Option.get r.TryValue).ToJson())
    let copy = doc.RootElement.GetProperty("copy")
    let amounts = copy.GetProperty("amounts")
    Assert.Equal("9007199254740993", copy.GetProperty("id").GetRawText())
    Assert.Equal("0.1", amounts.[0].GetRawText())
    Assert.Equal("12345678901234567890", amounts.[1].GetRawText())

[<Fact>]
let ``Clone still copies rather than aliases`` () =
    // The fix must not become "return the same instance": JyroNumber carries a
    // mutable ForceFloat flag, so aliasing would defeat the point of cloning.
    // Mutating the clone's container must not touch the original.
    let r =
        run "var c = Clone(Input.doc)\nc.id = 1\nOutput.originalId = Input.doc.id\nOutput.cloneId = c.id"
            "{\"doc\": {\"id\": 9007199254740993}}"
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))
    use doc = JsonDocument.Parse((Option.get r.TryValue).ToJson())
    Assert.Equal("9007199254740993", doc.RootElement.GetProperty("originalId").GetRawText())
    Assert.Equal("1", doc.RootElement.GetProperty("cloneId").GetRawText())

[<Fact>]
let ``Clone leaves an inexact number inexact`` () =
    // The other half: a value a decimal cannot hold must stay on the double
    // path rather than being forced exact.
    let r = run "Output.copy = Clone(Input.big)" "{\"big\": 1e30}"
    Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))
    use doc = JsonDocument.Parse((Option.get r.TryValue).ToJson())
    Assert.Contains("E+30", doc.RootElement.GetProperty("copy").GetRawText())

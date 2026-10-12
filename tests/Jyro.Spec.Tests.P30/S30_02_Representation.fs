/// 30.2.1 to 30.2.7: numbers, their two representations, literals and JSON.
module RealTree.Jyro.Spec.Tests.P30.S30_02_Representation

open System
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

// ---------------------------------------------------------------- 30.2.1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-44")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("-1")>]
[<InlineData("0.1")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("1e-28")>]
[<InlineData("Power(2, 0.5)")>]
[<InlineData("Power(1, 0.5)")>]
[<InlineData("0 * Power(2, 0.5)")>]
[<InlineData("-Power(2, 0.5)")>]
[<InlineData("1e29")>]
let ``TypeOf gives number for every script number`` (expr: string) =
    Assert.Equal("number", Values.asString (evalR ("TypeOf(" + expr + ")")))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-44")>]
[<InlineData("0.1")>]
[<InlineData("1e29")>]
[<InlineData("-0")>]
let ``TypeOf gives number for a number read from JSON Input`` (json: string) =
    let r = Script.runJson ("{\"v\":" + json + "}") "Output.r = TypeOf(Input.v)"
    Assert.Equal("number", Values.asString (Values.prop "r" (Script.output r)))

let hostNumbers () : obj[] seq =
    seq {
        yield [| box "Int32"; box (JyroNumber(42) :> JyroValue) |]
        yield [| box "Int64"; box (JyroNumber(9007199254740993L) :> JyroValue) |]
        yield [| box "Decimal"; box (JyroNumber(0.1M) :> JyroValue) |]
        yield [| box "Double"; box (JyroNumber(0.1) :> JyroValue) |]
        yield [| box "Double max"; box (JyroNumber(Double.MaxValue) :> JyroValue) |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-44")>]
[<MemberData(nameof hostNumbers)>]
let ``TypeOf gives number for every number the host gives as Input`` (_label: string, input: JyroValue) =
    let r = Script.runInput input "Output.r = TypeOf(Input)"
    Assert.Equal("number", Values.asString (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.2.2

/// The inventory of 90.2.1 to 90.2.8.
let inventory =
    set [
        // 90.2.1
        "ToUpper"; "ToLower"; "Trim"; "Replace"; "Contains"; "StartsWith"; "EndsWith"; "Split"; "Join"
        "Substring"; "ToNumber"; "PadLeft"; "PadRight"; "RegexTest"; "RegexMatch"; "RegexMatchAll"
        "RegexMatchDetail"; "RandomString"
        // 90.2.2
        "Length"; "Append"; "Prepend"; "First"; "Last"; "IndexOf"; "Reverse"; "Slice"; "Concatenate"
        "Distinct"; "Sort"; "Flatten"; "Range"; "Skip"; "Insert"; "RemoveAt"; "RemoveFirst"; "RemoveLast"
        "FlattenOnce"; "RandomChoice"; "SortByField"; "GroupBy"; "SelectMany"
        // 90.2.3
        "Absolute"; "Floor"; "Ceiling"; "Round"; "Min"; "Max"; "Power"; "SquareRoot"; "Log"; "Sum"
        "Average"; "Clamp"; "Median"; "Mode"; "RandomInt"
        // 90.2.4
        "Now"; "Today"; "ParseDate"; "FormatDate"; "DateAdd"; "DateDiff"; "DatePart"
        // 90.2.5
        "TypeOf"; "Coalesce"; "ToString"; "ToBoolean"; "Keys"; "Values"; "HasProperty"; "Merge"; "Clone"
        "NewGuid"; "Base64Encode"; "Base64Decode"; "FromJson"; "ToJson"; "Sleep"; "Diff"
        // 90.2.6
        "ValidateRequired"; "ValidateSchema"
        // 90.2.7
        "AnyByField"; "AllByField"; "FindByField"; "WhereByField"; "Select"; "Project"; "Omit"; "CountIf"
        // 90.2.8
        "Map"; "Where"; "Reduce"; "Each"; "Find"; "All"; "Any"; "SortBy"
    ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1786")>]
let ``JYRO-REQ-1786 The implementation adds no standard library function to the inventory, so none can tell an exact from an inexact number`` () =
    let extra =
        StdlibRegistry.getAll()
        |> Seq.map (fun f -> f.Name)
        |> Seq.filter (fun n -> not (inventory.Contains n))
        |> List.ofSeq
    Assert.True(List.isEmpty extra, sprintf "Functions beyond the inventory of 90.2: %A" extra)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1786")>]
[<InlineData("exact")>]
[<InlineData("decimal")>]
[<InlineData("double")>]
[<InlineData("integer")>]
[<InlineData("inexact")>]
[<InlineData("float")>]
let ``JYRO-REQ-1786 The implementation adds no type name that tells the representation of a number`` (typeName: string) =
    assertFails (Script.compile ("var x = 1\nOutput.r = x is " + typeName))
    assertFails (Script.compile ("var x = 1\nOutput.r = x is not " + typeName))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1786")>]
[<InlineData("1", "Power(1, 0.5)")>]
[<InlineData("0", "0 * Power(2, 0.5)")>]
[<InlineData("0.1 + 0.2", "0.1 * Power(1, 0.5) + 0.2")>]
[<InlineData("2", "Power(2, 0.5) * Power(2, 0.5)")>]
let ``JYRO-REQ-1786 TypeOf, is number and is not number agree for an exact and an inexact number`` (exactExpr: string, inexactExpr: string) =
    let source =
        "var e = " + exactExpr + "\n"
        + "var i = " + inexactExpr + "\n"
        + "Output.e = [TypeOf(e), e is number, e is not number, e is any, e is string]\n"
        + "Output.i = [TypeOf(i), i is number, i is not number, i is any, i is string]\n"
        + "Output.ev = e\nOutput.iv = i"
    let out = outOf source
    Assert.True(Values.isExact (Values.prop "ev" out), "the first operand should be exact")
    Assert.False(Values.isExact (Values.prop "iv" out), "the second operand should be inexact")
    Assert.Equal(Values.toJson (Values.prop "e" out), Values.toJson (Values.prop "i" out))
    Assert.Equal("[\"number\",true,false,true,false]", Values.toJson (Values.prop "e" out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1786")>]
[<InlineData("{\"v\":0.1}")>]
[<InlineData("{\"v\":1e29}")>]
let ``JYRO-REQ-1786 is number and TypeOf agree for exact and inexact numbers from JSON Input`` (json: string) =
    let out = Script.output (Script.runJson json "Output.r = [TypeOf(Input.v), Input.v is number, Input.v is not number]")
    Assert.Equal("[\"number\",true,false]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1786")>]
let ``JYRO-REQ-1786 is number and TypeOf agree for a host double given as Input`` () =
    let out = Script.output (Script.runInput (Values.inexact 0.5) "Output.r = [TypeOf(Input), Input is number, Input is not number]")
    Assert.Equal("[\"number\",true,false]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.2.3

[<Fact>]
[<Trait("requirement", "JYRO-REQ-46")>]
let ``Both representations exist and each behaves as its kind`` () =
    let out =
        outOf (
            "Output.e = 0.1\nOutput.i = Power(2, 0.5)\n"
            + "Output.exactSum = (0.1 + 0.2) == 0.3\n"
            + "Output.inexactSum = (0.1 * Power(1, 0.5) + 0.2) == 0.3\n"
            + "Output.inexactValue = 0.1 * Power(1, 0.5) + 0.2")
    Assert.True(Values.isExact (Values.prop "e" out))
    Assert.False(Values.isExact (Values.prop "i" out))
    Assert.True(Values.asBool (Values.prop "exactSum" out))
    Assert.False(Values.asBool (Values.prop "inexactSum" out))
    assertInexact 0.30000000000000004 (Values.prop "inexactValue" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-46")>]
let ``A compiled artifact keeps each number in one of the two representations`` () =
    let bytes = Script.valueOf (Script.compileBytesWith Script.defaults "Output.e = 0.1\nOutput.i = 1e29")
    let out = Script.output (Script.runBytesWith Script.defaults bytes)
    assertExact 0.1M (Values.prop "e" out)
    assertInexact 1e29 (Values.prop "i" out)

// ---------------------------------------------------------------- 30.2.4

/// Exact values: text as a literal (a leading '-' is the prefix operator) and as JSON.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1944")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950335", "-79228162514264337593543950335")>]
[<InlineData("1e28", "10000000000000000000000000000")>]
[<InlineData("1e-28", "0.0000000000000000000000000001")>]
[<InlineData("0.1234567890123456789012345678", "0.1234567890123456789012345678")>]
[<InlineData("7.9228162514264337593543950335", "7.9228162514264337593543950335")>]
[<InlineData("1.00000000000000000000000000000", "1")>]
[<InlineData("0.00000000000000000000000000010", "0.0000000000000000000000000001")>]
let ``JYRO-REQ-1944 A value m over 10 to the s within the capacity is held exactly`` (text: string, value: string) =
    let expected = dec value
    assertExact expected (evalR text)
    assertExact expected (Script.output (Script.runJson ("{\"v\":" + text + "}") "Output.r = Input.v") |> Values.prop "r")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1944")>]
[<InlineData("79228162514264337593543950336")>]
[<InlineData("-79228162514264337593543950336")>]
[<InlineData("99999999999999999999999999999")>]
[<InlineData("1e29")>]
[<InlineData("1e-29")>]
[<InlineData("0.12345678901234567890123456789")>]
let ``JYRO-REQ-1944 A value with no form within the capacity is not held exactly`` (text: string) =
    let lit = evalR text
    Assert.False(Values.isExact lit, "literal " + text)
    let fromJson = Script.output (Script.runJson ("{\"v\":" + text + "}") "Output.r = Input.v") |> Values.prop "r"
    Assert.False(Values.isExact fromJson, "JSON " + text)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1944")>]
let ``JYRO-REQ-1944 A value written with 29 fractional digits whose value is 1 is held exactly as 1`` () =
    // Exactness depends on the value, not on the digits written: the text has
    // 29 fractional digits, its value is 1 = 1 / 10^0. Judged by value and
    // exact arithmetic only; its string form is Part 40's (JYRO-REQ-150).
    let out =
        outOf (
            "var x = 1.00000000000000000000000000000\n"
            + "Output.x = x\nOutput.eq = x == 1\n"
            + "Output.y = x + 0.0000000000000000000000000001\n"
            + "Output.yeq = (x + 0.0000000000000000000000000001) == 1.0000000000000000000000000001")
    assertExact 1M (Values.prop "x" out)
    Assert.True(Values.asBool (Values.prop "eq" out))
    assertExact (dec "1.0000000000000000000000000001") (Values.prop "y" out)
    Assert.True(Values.asBool (Values.prop "yeq" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1944")>]
let ``JYRO-REQ-1944 Arithmetic reaching the largest significand stays exact and one more is not exact`` () =
    let out =
        outOf (
            "Output.a = 79228162514264337593543950334 + 1\n"
            + "Output.eq = (79228162514264337593543950334 + 1) == 79228162514264337593543950335\n"
            + "Output.b = 79228162514264337593543950335 + 1")
    assertExact 79228162514264337593543950335M (Values.prop "a" out)
    Assert.True(Values.asBool (Values.prop "eq" out))
    Assert.False(Values.isExact (Values.prop "b" out))

// ---------------------------------------------------------------- 30.2.5

[<Fact>]
[<Trait("requirement", "JYRO-REQ-48")>]
let ``JYRO-REQ-48 An inexact square root is the IEEE double of the square root`` () =
    assertInexact (Math.Sqrt 2.0) (evalR "Power(2, 0.5)")
    assertInexact 1.4142135623730951 (evalR "Power(2, 0.5)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-48")>]
let ``JYRO-REQ-48 Inexact addition rounds as binary64 does`` () =
    assertInexact (0.1 * 1.0 + 0.2) (evalR "0.1 * Power(1, 0.5) + 0.2")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-48")>]
let ``JYRO-REQ-48 The inexact representation has subnormals`` () =
    // Non-zero is read by the relational operator, not by ToBoolean (JYRO-REQ-1092, another work order).
    let out = outOf "var t = Power(2, -1074 * Power(1, 0.5))\nOutput.t = t\nOutput.nz = t > 0\nOutput.half = t / 2"
    assertInexact Double.Epsilon (Values.prop "t" out)
    Assert.True(Values.asBool (Values.prop "nz" out))
    assertInexact 0.0 (Values.prop "half" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-48")>]
let ``JYRO-REQ-48 The inexact representation reaches the largest double`` () =
    assertInexact Double.MaxValue (evalR ("1.7976931348623157e308 * " + I1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-48")>]
let ``JYRO-REQ-48 The inexact representation has a 53-bit significand`` () =
    let out = outOf ("Output.v = 9007199254740993 * " + I1 + "\nOutput.eq = (9007199254740993 * " + I1 + ") == 9007199254740992")
    assertInexact 9007199254740992.0 (Values.prop "v" out)
    Assert.True(Values.asBool (Values.prop "eq" out))

// ---------------------------------------------------------------- 30.2.6

[<Theory>]
[<Trait("requirement", "JYRO-REQ-49")>]
[<InlineData("0", "0")>]
[<InlineData("0.1", "0.1")>]
[<InlineData("19.99", "19.99")>]
[<InlineData("1e28", "1e28")>]
[<InlineData("1e-28", "1e-28")>]
[<InlineData("100e-30", "1e-28")>]
[<InlineData("0.1e-27", "1e-28")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("12345678901234567890123456789", "12345678901234567890123456789")>]
[<InlineData("1.00000000000000000000000000000", "1")>]
[<InlineData("0x7FFFFFFFFFFFFFFF", "9223372036854775807")>]
[<InlineData("0b101", "5")>]
let ``JYRO-REQ-49 A literal the exact representation can hold is exact`` (literal: string, value: string) =
    assertExact (dec value) (evalR literal)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-49")>]
[<InlineData("79228162514264337593543950336")>]
[<InlineData("1e29")>]
[<InlineData("1e-29")>]
[<InlineData("0.12345678901234567890123456789")>]
[<InlineData("99999999999999999999999999999")>]
[<InlineData("1e308")>]
[<InlineData("5e-324")>]
let ``JYRO-REQ-49 A literal the exact representation cannot hold is inexact with the nearest double`` (literal: string) =
    assertInexact (dbl literal) (evalR literal)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-49")>]
let ``JYRO-REQ-49 Exact literals add exactly and an inexact literal stays inexact`` () =
    let out = outOf "Output.a = 0.1 + 0.2 == 0.3\nOutput.b = 1e-29 * 1 == 1e-29\nOutput.c = 1e-29 * 1"
    Assert.True(Values.asBool (Values.prop "a" out))
    Assert.True(Values.asBool (Values.prop "b" out))
    Assert.False(Values.isExact (Values.prop "c" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-49")>]
let ``JYRO-REQ-49 Literal representations survive a compiled artifact`` () =
    let bytes = Script.valueOf (Script.compileBytesWith Script.defaults "Output.a = 19.99\nOutput.b = 1e-29")
    let out = Script.output (Script.runBytesWith Script.defaults bytes)
    assertExact 19.99M (Values.prop "a" out)
    assertInexact 1e-29 (Values.prop "b" out)

// ---------------------------------------------------------------- 30.2.7

let private viaRunner (read: JyroRunner -> JyroRunner) : JyroValue =
    let program = Script.valueOf (Script.compile "Output.v = Input")
    let r = (read (JyroRunner(program))).Execute()
    Script.output r |> Values.prop "v"

let private stream (text: string) = new MemoryStream(Encoding.UTF8.GetBytes text) :> Stream

/// Every JSON reading path of 30.2.7, each turning JSON text into a Jyro value.
let jsonPaths : (string * (string -> JyroValue)) list =
    [ "JyroRunner.WithInputJson(String)", (fun t -> viaRunner (fun r -> r.WithInputJson(t: string)))
      "JyroRunner.WithInputJson(Stream)", (fun t -> viaRunner (fun r -> r.WithInputJson(stream t)))
      "JyroScript.executeScriptWithJson",
      (fun t -> executeScriptWithJson("Output.v = Input", t) |> Script.output |> Values.prop "v")
      "JyroValue.FromJson(String)", (fun t -> JyroValue.FromJson t)
      "JyroValue.FromJson(Stream)", (fun t -> JyroValue.FromJson(stream t))
      "JyroValue.FromJsonAsync", (fun t -> JyroValue.FromJsonAsync(stream t, CancellationToken.None).Result)
      "script FromJson",
      (fun t ->
          Script.runInput (Values.obj [ "t", Values.str t ]) "Output.v = FromJson(Input.t)"
          |> Script.output |> Values.prop "v") ]

let jsonPathNames () : obj[] seq = jsonPaths |> Seq.map (fun (n, _) -> [| box n |])

let private readAll (path: string) (text: string) : JyroValue list =
    let read = jsonPaths |> List.find (fun (n, _) -> n = path) |> snd
    [ read text
      read ("[" + text + "]") |> Values.asList |> List.head
      read ("{\"v\":" + text + "}") |> Values.prop "v" ]

let private exactJson =
    [ "0.1", "0.1"; "19.99", "19.99"; "-0", "0"; "1E2", "100"
      "79228162514264337593543950335", "79228162514264337593543950335"; "1e28", "1e28"; "1e-28", "1e-28"
      "1.00000000000000000000000000000", "1"; "0.1234567890123456789012345678", "0.1234567890123456789012345678" ]

let private inexactJson =
    [ "79228162514264337593543950336"; "1e29"; "1e-29"; "0.12345678901234567890123456789"; "1e308" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-50")>]
[<MemberData(nameof jsonPathNames)>]
let ``JYRO-REQ-50 On every JSON path a number the exact representation holds is exact`` (path: string) =
    for (text, value) in exactJson do
        for v in readAll path text do
            try assertExact (dec value) v
            with e -> Values.fail (sprintf "%s, JSON %s: %s" path text e.Message)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-50")>]
[<MemberData(nameof jsonPathNames)>]
let ``JYRO-REQ-50 On every JSON path a number beyond the exact representation is the nearest double`` (path: string) =
    for text in inexactJson do
        for v in readAll path text do
            try assertInexact (dbl text) v
            with e -> Values.fail (sprintf "%s, JSON %s: %s" path text e.Message)

/// 1 + 2^-53, the midpoint between 1 and the next double, written exactly.
let private midpoint = "1.00000000000000011102230246251565404236316680908203125"

/// The double after 1.
let private afterOne = BitConverter.Int64BitsToDouble(BitConverter.DoubleToInt64Bits 1.0 + 1L)

/// Number tokens far longer than 768 significant digits and one chunk, and their
/// representation: a value that is m / 10^s (30.2.4) is exact whatever its length;
/// any other is the double nearest its whole value (JYRO-ADR-151 (1), (2), (5)(b)),
/// which a digit 100000 places after the point can decide.
let longNumbers : (string * string * Choice<decimal, float>) list =
    let zeros n = String('0', n)
    [ "the midpoint, 100000 zeros, then 1: rounds up", midpoint + zeros 100000 + "1", Choice2Of2 afterOne
      "the midpoint, then 100000 zeros: a tie, to even", midpoint + zeros 100000, Choice2Of2 1.0
      "the midpoint less 1e-100056: rounds down", "1.00000000000000011102230246251565404236316680908203124" + String('9', 100000), Choice2Of2 1.0
      "0. then 100000 zeros, 1: rounds to zero", "0." + zeros 100000 + "1", Choice2Of2 0.0
      "1 then 100000 zero digits, e-100000: exactly 1", "1" + zeros 100000 + "e-100000", Choice1Of2 1M
      "0. then 100000 zeros, 1e100001: exactly 1", "0." + zeros 100000 + "1e100001", Choice1Of2 1M
      "1. then 100000 zeros: exactly 1", "1." + zeros 100000, Choice1Of2 1M
      "0.1 then 100000 zeros, e1: exactly 1", "0.1" + zeros 100000 + "e1", Choice1Of2 1M ]

let longNumberData () : obj[] seq =
    seq {
        for (path, _) in jsonPaths do
            for (name, _, _) in longNumbers do
                yield [| box path; box name |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-50")>]
[<MemberData(nameof longNumberData)>]
let ``JYRO-REQ-50 On every JSON path a long number token is exact when it can be, else the nearest double of its whole value`` (path: string, name: string) =
    let (_, text, expected) = longNumbers |> List.find (fun (n, _, _) -> n = name)
    for v in readAll path text do
        match expected with
        | Choice1Of2 d -> assertExact d v
        | Choice2Of2 f -> assertInexact f v

/// The JSON paths of 30.2.7 that read text already in memory, on this thread:
/// what each allocates on this thread while it reads `text`, after reading it once
/// to warm it up. The script's FromJson is measured over Execute alone, its
/// program compiled and its Input made beforehand.
let private inMemoryReads : (string * (string -> int64)) list =
    let measure (read: unit -> unit) =
        read ()
        let before = GC.GetAllocatedBytesForCurrentThread()
        read ()
        GC.GetAllocatedBytesForCurrentThread() - before
    [ "JyroValue.FromJson(String)", (fun t -> measure (fun () -> JyroValue.FromJson t |> ignore))
      "script FromJson",
      (fun t ->
          let program = Script.valueOf (Script.compile "var v = FromJson(Input.t)\nOutput.ok = v != null")
          let input = Values.obj [ "t", Values.str t ]
          measure (fun () ->
              let r = JyroRunner(program).WithInput(input).Execute()
              if not r.IsSuccess then Values.fail (sprintf "the script's FromJson failed: %s" (describe r)))) ]

let inMemoryReadNames () : obj[] seq = inMemoryReads |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-50")>]
[<MemberData(nameof inMemoryReadNames)>]
let ``JYRO-REQ-50 a long number token is read where it lies, not first copied whole (JYRO-ADR-151 (5)(a))`` (path: string) =
    // JYRO-DEF-605: the token is not made a string (Encoding.GetString, ToArray) before
    // it is read. Two texts of one length are compared: a number of n digits, and the
    // number 0.1 followed by white space to the same length. Reading white space copies nothing, so what the
    // long token costs beyond the padded text is what reading its digits allocates. A
    // copy of the token as a .NET string costs 2n bytes, as UTF-8 bytes n; reading
    // the digits in place costs a bounded amount, whatever n is.
    let n = 4_000_000
    let measure = inMemoryReads |> List.find (fun (p, _) -> p = path) |> snd
    let longToken = "[0." + String('1', n) + "]"
    let padded = "[0.1" + String(' ', n - 1) + "]"
    Assert.Equal(longToken.Length, padded.Length)
    let extra = measure longToken - measure padded
    if extra >= int64 (n / 2) then
        Values.fail (sprintf "%s allocated %d bytes more for a number token of %d digits than for the same length of white space" path extra n)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-50")>]
let ``JYRO-REQ-50 A decimal from JSON Input adds exactly in the script`` () =
    let out = Script.output (Script.runJson "{\"v\":0.1}" "Output.r = Input.v + 0.2 == 0.3")
    Assert.True(Values.asBool (Values.prop "r" out))

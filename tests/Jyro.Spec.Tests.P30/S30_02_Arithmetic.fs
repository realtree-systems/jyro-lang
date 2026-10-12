/// 30.2.8 to 30.2.11, 30.2.15, 30.2.16: arithmetic by representation, and
/// numbers written to JSON and to a compiled artifact.
module RealTree.Jyro.Spec.Tests.P30.S30_02_Arithmetic

open System
open System.IO
open System.Text
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

// ---------------------------------------------------------------- 30.2.8

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1945")>]
let ``A number exact when the artifact was written is exact when it is read`` () =
    let source =
        "Output.a = 0.1\nOutput.b = 19.99\nOutput.c = 79228162514264337593543950335\n"
        + "Output.d = -0.000000000000000000000000001\nOutput.z = 0\nOutput.i = Power(2, 0.5)\n"
        + "Output.sum = 0.1 + 0.2 == 0.3"
    let bytes = Script.valueOf (Script.compileBytesWith Script.defaults source)
    let out = Script.output (Script.runBytesWith Script.defaults bytes)
    assertExact 0.1M (Values.prop "a" out)
    assertExact 19.99M (Values.prop "b" out)
    assertExact 79228162514264337593543950335M (Values.prop "c" out)
    assertExact -0.000000000000000000000000001M (Values.prop "d" out)
    assertExact 0M (Values.prop "z" out)
    Assert.False(Values.isExact (Values.prop "i" out))
    Assert.True(Values.asBool (Values.prop "sum" out))

// ---------------------------------------------------------------- 30.2.9

[<Theory>]
[<Trait("requirement", "JYRO-REQ-52")>]
[<InlineData("0.1 + 0.2", "0.3")>]
[<InlineData("1 - 0.9", "0.1")>]
[<InlineData("1.1 * 1.1", "1.21")>]
[<InlineData("1 / 8", "0.125")>]
[<InlineData("10 / 4", "2.5")>]
[<InlineData("5.5 % 2", "1.5")>]
[<InlineData("-7 % 2", "-1")>]
[<InlineData("79228162514264337593543950334 + 1", "79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950334 - 1", "-79228162514264337593543950335")>]
[<InlineData("79228162514264337593543950335 - 79228162514264337593543950335", "0")>]
[<InlineData("7922816251426433759354395033.5 * 10", "79228162514264337593543950335")>]
[<InlineData("0.00000000000001 * 0.00000000000001", "1e-28")>]
[<InlineData("1e-27 / 10", "1e-28")>]
[<InlineData("1e28 * 1", "1e28")>]
[<InlineData("1.0000000000000000000000000001 - 1", "1e-28")>]
[<InlineData("7.9228162514264337593543950335 + 0", "7.9228162514264337593543950335")>]
[<InlineData("0.3 / 0.1", "3")>]
[<InlineData("0.7 % 0.2", "0.1")>]
let ``Exact operands whose result the capacity holds give an exact result`` (expr: string, expected: string) =
    let out = outOf ("Output.r = " + expr + "\nOutput.eq = (" + expr + ") == " + expected)
    assertExact (dec expected) (Values.prop "r" out)
    Assert.True(Values.asBool (Values.prop "eq" out), expr + " == " + expected)

// ---------------------------------------------------------------- 30.2.10

[<Theory>]
[<Trait("requirement", "JYRO-REQ-53")>]
[<InlineData("0.1 * i1 + 0.2", 0.30000000000000004)>]
[<InlineData("0.2 + 0.1 * i1", 0.30000000000000004)>]
[<InlineData("0.3 - 0.1 * i1", 0.19999999999999998)>]
[<InlineData("0.1 * i1 * 3", 0.30000000000000004)>]
[<InlineData("1 / (3 * i1)", 0.3333333333333333)>]
[<InlineData("5.5 * i1 % 2", 1.5)>]
[<InlineData("2 % (3 * i1)", 2.0)>]
[<InlineData("9007199254740993 + 0 * i1", 9007199254740992.0)>]
[<InlineData("i1 - 1", 0.0)>]
[<InlineData("3 * i1", 3.0)>]
let ``An inexact operand makes the operation double precision and the result inexact`` (expr: string, expected: float) =
    let r = rOf ("var i1 = " + I1 + "\nOutput.r = " + expr)
    assertInexact expected r

// ---------------------------------------------------------------- 30.2.11

[<Theory>]
[<Trait("requirement", "JYRO-REQ-54")>]
[<InlineData("1 / 3", "1", "/", "3")>]
[<InlineData("2 / 3", "2", "/", "3")>]
[<InlineData("10 / 7", "10", "/", "7")>]
[<InlineData("79228162514264337593543950335 + 1", "79228162514264337593543950335", "+", "1")>]
[<InlineData("-79228162514264337593543950335 - 1", "-79228162514264337593543950335", "-", "1")>]
[<InlineData("79228162514264337593543950335 * 2", "79228162514264337593543950335", "*", "2")>]
[<InlineData("1e28 * 10", "1e28", "*", "10")>]
[<InlineData("0.00000000000001 * 0.000000000000001", "0.00000000000001", "*", "0.000000000000001")>]
[<InlineData("1e-28 / 10", "1e-28", "/", "10")>]
[<InlineData("1e-28 / 3", "1e-28", "/", "3")>]
[<InlineData("0.1234567890123456789012345678 * 0.1", "0.1234567890123456789012345678", "*", "0.1")>]
[<InlineData("1.5 * 79228162514264337593543950335", "1.5", "*", "79228162514264337593543950335")>]
let ``JYRO-REQ-54 Exact operands whose exact result the capacity cannot hold give the IEEE double`` (expr: string, left: string, op: string, right: string) =
    let a = dbl left
    let b = dbl right
    let expected =
        match op with
        | "+" -> a + b
        | "-" -> a - b
        | "*" -> a * b
        | "/" -> a / b
        | _ -> a % b
    assertInexact expected (evalR expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-54")>]
let ``JYRO-REQ-54 A third times three is done in double precision`` () =
    assertInexact ((1.0 / 3.0) * 3.0) (evalR "(1 / 3) * 3")

// ---------------------------------------------------------------- 30.2.15

let private exactCases =
    [ "0.1", "0.1"; "19.99", "19.99"; "0.1 + 0.2", "0.3"; "175928847299117063", "175928847299117063"
      "9007199254740993", "9007199254740993"; "79228162514264337593543950335", "79228162514264337593543950335"
      "0.1234567890123456789012345678", "0.1234567890123456789012345678"; "1e21", "1000000000000000000000" ]

let exactCaseData () : obj[] seq = exactCases |> Seq.map (fun (e, t) -> [| box e; box t |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1946")>]
[<MemberData(nameof exactCaseData)>]
let ``Script ToJson writes an exact number with its exact digits`` (expr: string, token: string) =
    Assert.Equal(token, Values.asString (evalR ("ToJson(" + expr + ")")))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1946")>]
[<MemberData(nameof exactCaseData)>]
let ``Host ToJson writes an exact Output number with its exact digits`` (expr: string, token: string) =
    Assert.Equal("{\"v\":" + token + "}", Values.toJson (outOf ("Output.v = " + expr)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1946")>]
[<MemberData(nameof exactCaseData)>]
let ``Host WriteJson writes an exact Output number with its exact digits`` (expr: string, token: string) =
    let out = outOf ("Output.v = " + expr)
    use ms = new MemoryStream()
    do
        use w = new Utf8JsonWriter(ms)
        out.WriteJson(w)
        w.Flush()
    Assert.Equal("{\"v\":" + token + "}", Encoding.UTF8.GetString(ms.ToArray()))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1946")>]
[<MemberData(nameof exactCaseData)>]
let ``The one-call host entry points return an exact number with its exact digits`` (expr: string, token: string) =
    let viaJson = executeScriptWithJson("Output.v = " + expr, "{}") |> Script.output
    Assert.Equal("{\"v\":" + token + "}", Values.toJson viaJson)
    let viaObj = executeScript("Output.v = " + expr, box (Collections.Generic.Dictionary<string, obj>())) |> Script.output
    Assert.Equal("{\"v\":" + token + "}", Values.toJson viaObj)

// ---------------------------------------------------------------- 30.2.16

/// Every exact number encoded in the artifact (tag 0x03, representation 0x01,
/// then 16 bytes as 150.6.3 gives), decoded.
let private exactNumbersIn (bytes: byte[]) : decimal list =
    [ for i in 0 .. bytes.Length - 18 do
        if bytes.[i] = 0x03uy && bytes.[i + 1] = 0x01uy then
            let p = i + 2
            let scale = int bytes.[p + 14]
            let sign = bytes.[p + 15]
            if bytes.[p + 12] = 0uy && bytes.[p + 13] = 0uy && scale <= 28 && (sign = 0uy || sign = 0x80uy) then
                let lo = BitConverter.ToInt32(bytes, p)
                let mid = BitConverter.ToInt32(bytes, p + 4)
                let hi = BitConverter.ToInt32(bytes, p + 8)
                yield Decimal(lo, mid, hi, (sign = 0x80uy), byte scale) ]

/// Every inexact number encoded in the artifact (0x03, 0x00, 8 bytes).
let private inexactNumbersIn (bytes: byte[]) : float list =
    [ for i in 0 .. bytes.Length - 10 do
        if bytes.[i] = 0x03uy && bytes.[i + 1] = 0x00uy then
            yield BitConverter.ToDouble(bytes, i + 2) ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-59")>]
[<InlineData("9007199254740993")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("0.1")>]
[<InlineData("0.1234567890123456789012345678")>]
[<InlineData("175928847299117063")>]
let ``A compiled artifact keeps an exact literal exact and never as a double`` (literal: string) =
    let expected = dec literal
    let bytes = Script.valueOf (Script.compileBytesWith { Script.defaults with Stdlib = false } ("Output.x = " + literal))
    Assert.Contains(expected, exactNumbersIn bytes)
    Assert.DoesNotContain(float expected, inexactNumbersIn bytes)
    let out = Script.output (Script.runBytesWith { Script.defaults with Stdlib = false } bytes)
    assertExact expected (Values.prop "x" out)

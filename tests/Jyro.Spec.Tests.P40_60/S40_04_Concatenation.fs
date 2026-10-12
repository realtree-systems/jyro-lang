/// 40.4 Concatenation and string forms.
module RealTree.Jyro.Spec.Tests.P40_60.S40_04_Concatenation

open System
open System.Globalization
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

/// A reference implementation of ECMA-262 Number::toString(x) with radix 10,
/// built on .NET's shortest round-trip digits ("R" on .NET Core 3.0 and later).
let rec ecmaNumberToString (x: float) : string =
    if Double.IsNaN x then "NaN"
    elif x = 0.0 then "0"
    elif x < 0.0 then "-" + ecmaNumberToString (-x)
    elif Double.IsPositiveInfinity x then "Infinity"
    else
        let r = x.ToString("R", CultureInfo.InvariantCulture)
        let mantissa, exp =
            match r.IndexOfAny([| 'E'; 'e' |]) with
            | -1 -> r, 0
            | i -> r.Substring(0, i), Int32.Parse(r.Substring(i + 1), CultureInfo.InvariantCulture)
        let intPart, fracPart =
            match mantissa.IndexOf('.') with
            | -1 -> mantissa, ""
            | i -> mantissa.Substring(0, i), mantissa.Substring(i + 1)
        let mutable digits = intPart + fracPart
        let mutable n = intPart.Length + exp
        while digits.Length > 1 && digits.[0] = '0' do
            digits <- digits.Substring 1
            n <- n - 1
        digits <- digits.TrimEnd('0')
        let k = digits.Length
        if k <= n && n <= 21 then digits + String('0', n - k)
        elif 0 < n && n <= 21 then digits.Substring(0, n) + "." + digits.Substring n
        elif -6 < n && n <= 0 then "0." + String('0', -n) + digits
        else
            let e = n - 1
            let es = (if e >= 0 then "+" else "-") + string (abs e)
            if k = 1 then digits + "e" + es
            else digits.Substring(0, 1) + "." + digits.Substring 1 + "e" + es

// ---- 40.4.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1956")>]
[<InlineData("1 & 2", "12")>]
[<InlineData("\"a\" & \"b\"", "ab")>]
[<InlineData("\"a\" & null", "anull")>]
[<InlineData("null & \"a\"", "nulla")>]
[<InlineData("null & null", "nullnull")>]
[<InlineData("true & false", "truefalse")>]
[<InlineData("\"\" & \"\"", "")>]
[<InlineData("\"x\" & 1 & \"y\"", "x1y")>]
[<InlineData("-1 & -2", "-1-2")>]
[<InlineData("\"b\" & \"a\"", "ba")>]
let ``JYRO-REQ-1956 Concat_GivesLeftStringFormThenRightStringForm`` (expr: string, expected: string) =
    assertRStr expected (sprintf "Output.r = %s" expr)

/// The string form of each operand is taken whatever it is (40.4.6 and 40.4.7 give
/// the text of a number, and are judged by their own tests): `a & b` is the text
/// of `a & ""` followed by the text of `"" & b`.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1956")>]
[<InlineData("1.50", "\"q\"")>]
[<InlineData("\"q\"", "1e300")>]
[<InlineData("2.5", "-0.125")>]
[<InlineData("1e300", "true")>]
[<InlineData("null", "0x1F")>]
let ``JYRO-REQ-1956 Concat_IsTheLeftStringFormFollowedByTheRightStringForm`` (a: string, b: string) =
    let out = Script.outputOf (sprintf "var a = %s\nvar b = %s\nOutput.l = a & \"\"\nOutput.r = \"\" & b\nOutput.c = a & b" a b)
    let text k = Values.asString (Values.prop k out)
    Assert.Equal(text "l" + text "r", text "c")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1956")>]
let ``JYRO-REQ-1956 Concat_CompoundAssignment_AppendsStringForm`` () =
    assertRStr "a1" "var s = \"a\"\ns &= 1\nOutput.r = s"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1956")>]
let ``JYRO-REQ-1956 Concat_ResultIsAString`` () =
    Assert.Equal("12", Values.asString (rOf "Output.r = 1 & 2"))
    Assert.Equal("nullnull", Values.asString (rOf "Output.r = null & null"))
    assertRBool true "Output.r = (1 & 2) is string"
    assertRBool true "Output.r = (null & null) is string"

// ---- 40.4.2 --------------------------------------------------------------

let invalidConcatValues : seq<obj[]> =
    seq {
        for v in [ "[]"; "[1]"; "{}"; "{a: 1}"; "x => x" ] do
            yield [| box v; box "left" |]
            yield [| box v; box "right" |]
            yield [| box v; box "compound" |]
            yield [| box v; box "compoundTarget" |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1957")>]
[<MemberData(nameof invalidConcatValues)>]
let ``JYRO-REQ-1957 Concat_ArrayObjectOrFunctionOperand_RaisesConcatWithInvalidType`` (value: string, place: string) =
    let source =
        match place with
        | "left" -> sprintf "var x = %s\nOutput.r = x & \"a\"" value
        | "right" -> sprintf "var x = %s\nOutput.r = \"a\" & x" value
        | "compound" -> sprintf "var x = %s\nvar s = \"a\"\ns &= x" value
        | _ -> sprintf "var x = %s\nx &= \"a\"" value
    runtimeError MessageCode.ConcatWithInvalidType source

// ---- 40.4.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-147")>]
[<InlineData("Output.r = null & \"\"")>]
[<InlineData("Output.r = \"\" & null")>]
[<InlineData("var n = null\nOutput.r = n & \"\"")>]
[<InlineData("var n\nOutput.r = n & \"\"")>]
[<InlineData("Output.r = Input.missing & \"\"")>]
let StringForm_OfNull_IsNull (source: string) =
    assertRStr "null" source

// ---- 40.4.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-148")>]
[<InlineData("Output.r = true & \"\"", "true")>]
[<InlineData("Output.r = false & \"\"", "false")>]
[<InlineData("Output.r = (1 == 1) & \"\"", "true")>]
[<InlineData("Output.r = (1 == 2) & \"\"", "false")>]
[<InlineData("Output.r = Input.b & \"\"", "false")>]
[<InlineData("Output.r = Input.t & \"\"", "true")>]
let StringForm_OfBoolean_IsLowerCaseName (source: string, expected: string) =
    Assert.Equal(expected, Values.asString (Values.prop "r" (Script.output (Script.runJson """{"b":false,"t":true}""" source))))

// ---- 40.4.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-149")>]
[<InlineData("\"abc\"", "abc")>]
[<InlineData("\"\"", "")>]
[<InlineData("\"a\\\"b\"", "a\"b")>]
[<InlineData("'a\\'b'", "a'b")>]
[<InlineData("\"a\\\\b\"", "a\\b")>]
[<InlineData("\"a\\nb\"", "a\nb")>]
[<InlineData("\"\\uD83D\\uDE00\"", "😀")>]
[<InlineData("\"null\"", "null")>]
let StringForm_OfString_IsTheStringItself (literal: string, expected: string) =
    let out = Script.outputOf (sprintf "var s = %s\nOutput.r = s & \"\"\nOutput.n = Length(s & \"\")\nOutput.m = Length(s)" literal)
    Assert.Equal(expected, Values.asString (Values.prop "r" out))
    Assert.Equal(Values.asFloat (Values.prop "m" out), Values.asFloat (Values.prop "n" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-149")>]
let StringForm_OfInputString_IsTheStringItself () =
    let out = Script.output (Script.runJson """{"s":"q\"u\\oé"}""" "Output.r = Input.s & \"\"")
    Assert.Equal("q\"u\\oé", Values.asString (Values.prop "r" out))

// ---- 40.4.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-150")>]
[<InlineData("0", "0")>]
[<InlineData("-0", "0")>]
[<InlineData("0.0", "0")>]
[<InlineData("-0.0", "0")>]
[<InlineData("0 * -1", "0")>]
[<InlineData("1.0", "1")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("100", "100")>]
[<InlineData("1e2", "100")>]
[<InlineData("1E2", "100")>]
[<InlineData("1e21", "1000000000000000000000")>]
[<InlineData("1e28", "10000000000000000000000000000")>]
[<InlineData("1e-5", "0.00001")>]
[<InlineData("1e-7", "0.0000001")>]
[<InlineData("0.0000000000000000000000000001", "0.0000000000000000000000000001")>]
[<InlineData("1e-28", "0.0000000000000000000000000001")>]
[<InlineData("-0.5", "-0.5")>]
[<InlineData("-12.340", "-12.34")>]
[<InlineData("79228162514264337593543950335", "79228162514264337593543950335")>]
[<InlineData("-79228162514264337593543950335", "-79228162514264337593543950335")>]
[<InlineData("10 / 4", "2.5")>]
[<InlineData("0.1 + 0.2", "0.3")>]
[<InlineData("2.50 * 2", "5")>]
[<InlineData("1.25 - 0.25", "1")>]
[<InlineData("0x1F", "31")>]
let ``JYRO-REQ-150 StringForm_OfExactNumber_IsPlainDecimal`` (expr: string, expected: string) =
    let out = Script.outputOf (sprintf "var v = %s\nOutput.v = v\nOutput.r = v & \"\"" expr)
    Assert.True(Values.isExact (Values.prop "v" out), "the value is exact")
    Assert.Equal(expected, Values.asString (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-150")>]
let ``JYRO-REQ-150 StringForm_OfExactNumberFromInput_DropsTrailingZeros`` () =
    let out = Script.output (Script.runJson """{"n":1.10,"z":0.000,"i":5.0}""" "Output.r = Input.n & \"\"\nOutput.z = Input.z & \"\"\nOutput.i = Input.i & \"\"")
    Assert.Equal("1.1", Values.asString (Values.prop "r" out))
    Assert.Equal("0", Values.asString (Values.prop "z" out))
    Assert.Equal("5", Values.asString (Values.prop "i" out))

// ---- 40.4.7 --------------------------------------------------------------

// Which script expressions give an inexact number is Part 30's rule, not this
// requirement's. So the table takes each inexact number from the host, as a
// double (JyroNumber(Double), 160.8), and the script only reads and joins it;
// the computed rows start from an operand that is inexact in any reading of
// Part 30 (a double from the host, or a literal beyond the exact range).

/// `limited`, with `Input` an object whose property `v` is the host double `d`.
let private withDouble (d: float) : RunOptions =
    { limited with Input = ValueInput(Values.obj [ "v", Values.inexact d; "one", Values.inexact 1.0 ]) }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-151")>]
[<InlineData(1e300, "1e+300")>]
[<InlineData(-1e300, "-1e+300")>]
[<InlineData(0.3333333333333333, "0.3333333333333333")>]
[<InlineData(0.6666666666666666, "0.6666666666666666")>]
[<InlineData(1e-29, "1e-29")>]
[<InlineData(7.922816251426434e28, "7.922816251426434e+28")>]
[<InlineData(0.5, "0.5")>]
[<InlineData(100.0, "100")>]
[<InlineData(1e21, "1e+21")>]
[<InlineData(1e20, "100000000000000000000")>]
[<InlineData(0.000001, "0.000001")>]
[<InlineData(1e-7, "1e-7")>]
[<InlineData(1.5e-7, "1.5e-7")>]
[<InlineData(123.456, "123.456")>]
[<InlineData(1.5e22, "1.5e+22")>]
[<InlineData(0.0, "0")>]
[<InlineData(0.30000000000000004, "0.30000000000000004")>]
[<InlineData(0.1, "0.1")>]
[<InlineData(999999999999999900000.0, "999999999999999900000")>]
[<InlineData(9.99999999999999e-7, "9.99999999999999e-7")>]
[<InlineData(-1e-7, "-1e-7")>]
[<InlineData(-1e21, "-1e+21")>]
[<InlineData(1.5e-300, "1.5e-300")>]
[<InlineData(5e-324, "5e-324")>]
[<InlineData(1.7976931348623157e308, "1.7976931348623157e+308")>]
let ``JYRO-REQ-151 StringForm_OfInexactNumber_IsEcmaScriptNumberToString`` (d: float, expected: string) =
    let out = Script.output (Script.runWith (withDouble d) "Output.v = Input.v\nOutput.r = Input.v & \"\"")
    Assert.False(Values.isExact (Values.prop "v" out), "the value is inexact")
    Assert.Equal(expected, ecmaNumberToString d)   // the table agrees with the reference
    Assert.Equal(expected, Values.asString (Values.prop "r" out))

/// An inexact number computed in the script from an inexact operand: a host
/// double (`one`, `v`) or a literal beyond the exact range (1e300).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-151")>]
[<InlineData("Input.one / 3", "0.3333333333333333")>]
[<InlineData("Input.one * 2 / 3", "0.6666666666666666")>]
[<InlineData("-2 * Input.one / 3", "-0.6666666666666666")>]
[<InlineData("Input.one / 7", "0.14285714285714285")>]
[<InlineData("Input.one * 0.1 + Input.one * 0.2", "0.30000000000000004")>]
[<InlineData("1e300 / 1e300 * 0.5", "0.5")>]
[<InlineData("1e300 / 1e300 * 1e21", "1e+21")>]
[<InlineData("1e300 / 1e300 * 1e20", "100000000000000000000")>]
[<InlineData("1e300 / 1e300 * 0.0000001", "1e-7")>]
[<InlineData("1e300 / 1e300 * 1e21 - 1e300 / 1e300 * 1e5", "999999999999999900000")>]
[<InlineData("1e300 / 1e300 * 0.000001 - 1e300 / 1e300 * 1e-21", "9.99999999999999e-7")>]
[<InlineData("-1e300 / 1e300 * 1e21", "-1e+21")>]
[<InlineData("1e300 - 1e300", "0")>]
[<InlineData("1e300 / 1e279", "")>]        // "" : the reference alone gives the text
[<InlineData("1e300 / 1e280", "")>]
[<InlineData("1e300 / 7e290", "")>]
[<InlineData("1e-300 * 1e294", "")>]
[<InlineData("1e-300 * 1e293", "")>]
[<InlineData("1e-300 * 1e295", "")>]
[<InlineData("Input.one * 1.7976931348623157e308", "1.7976931348623157e+308")>]
let ``JYRO-REQ-151 StringForm_OfComputedInexactNumber_IsEcmaScriptNumberToString`` (expr: string, expected: string) =
    let out = Script.output (Script.runWith (withDouble 0.0) (sprintf "var v = %s\nOutput.v = v\nOutput.r = v & \"\"" expr))
    let v = Values.prop "v" out
    Assert.False(Values.isExact v, "the value is inexact")
    Assert.Equal(ecmaNumberToString (Values.asFloat v), Values.asString (Values.prop "r" out))
    if expected <> "" then Assert.Equal(expected, Values.asString (Values.prop "r" out))

/// A host double read through `Input` keeps its string form when stored and read back.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-151")>]
let ``JYRO-REQ-151 StringForm_OfInexactInputNumber_IsEcmaScriptNumberToString`` () =
    let input = Values.obj [ "n", Values.inexact 1e300; "m", Values.inexact 1.5e-300 ]
    let out = Script.output (Script.runWith { limited with Input = ValueInput input } "var n = Input.n\nvar m = Input.m\nOutput.r = n & \"\"\nOutput.s = \"\" & m")
    Assert.Equal("1e+300", Values.asString (Values.prop "r" out))
    Assert.Equal("1.5e-300", Values.asString (Values.prop "s" out))

// ---- 40.4 read through a host function's string helper (160.3.7) ---------

/// A host function that reads its argument with the string helper and returns the text.
type private StringProbe() =
    inherit JyroFunctionBase("StringProbe", Host.signature "StringProbe" [ Parameter.Optional("v", ParameterType.Any) ] ParameterType.Any)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(args, _ctx) = JyroString(args.GetString(0)) :> JyroValue

let private helperText (input: JyroValue) : string =
    let o = { Script.defaults with Functions = [ StringProbe() :> IJyroFunction ]; Input = ValueInput(Values.obj [ "v", input ]) }
    Values.asString (Values.prop "r" (Script.output (Script.runWith o "Output.r = StringProbe(Input.v)")))

/// A host function's string helper gives an exact number its string form (160.3.7), which
/// drops trailing zeros. Moved here from JYRO-REQ-1597's tests (JYRO-ADR-113 item 2): the
/// string form of 1.50 is this requirement's, so it depends on 1597.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-150")>]
let ``JYRO-REQ-150 StringForm_OfExactNumber_ThroughTheHostStringHelper_DropsTrailingZeros`` () =
    let script = Script.output (Script.run "Output.v = 1.50")
    Assert.Equal("1.5", helperText (Values.prop "v" script))

/// A host function's string helper gives an inexact number its string form (160.3.7),
/// Number::toString. Moved here from JYRO-REQ-1597's tests (JYRO-ADR-113 item 2): these
/// string forms are this requirement's, so it depends on 1597.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-151")>]
[<InlineData(1e21, "1e+21")>]
[<InlineData(1e-7, "1e-7")>]
let ``JYRO-REQ-151 StringForm_OfInexactNumber_ThroughTheHostStringHelper_IsNumberToString`` (d: float, expected: string) =
    Assert.Equal(expected, helperText (Values.inexact d))

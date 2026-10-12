/// 40.2 Arithmetic, 40.3 Division and modulo.
module RealTree.Jyro.Spec.Tests.P40_60.S40_02_Arithmetic

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// ---- 40.2.1 --------------------------------------------------------------

// Values are compared with `==` (30.10) and the representation with IsExact,
// so that these tests do not depend on the string forms of 40.4.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-133")>]
[<InlineData("2 + 3", "5", true)>]
[<InlineData("2 - 3", "-1", true)>]
[<InlineData("4 * 2.5", "10", true)>]
[<InlineData("0.3 - 0.1", "0.2", true)>]
[<InlineData("1.1 * 1.1", "1.21", true)>]
[<InlineData("0.1 + 0.2", "0.3", true)>]
[<InlineData("-5 + 5", "0", true)>]
[<InlineData("0 * -3", "0", true)>]
[<InlineData("79228162514264337593543950334 + 1", "79228162514264337593543950335", true)>]
[<InlineData("79228162514264337593543950335 + 1", "79228162514264337593543950336", false)>]
[<InlineData("-79228162514264337593543950335 - 1", "-79228162514264337593543950336", false)>]
[<InlineData("1e300 + 1", "1e300", false)>]
[<InlineData("1e300 - 1", "1e300", false)>]
[<InlineData("1e300 * 2", "2e300", false)>]
[<InlineData("0.5 * 0.5", "0.25", true)>]
let Arithmetic_NumberOperands_GiveSumDifferenceProductAsPart30 (expr: string, expected: string, exact: bool) =
    let out = Script.outputOf (sprintf "var v = %s\nOutput.v = v\nOutput.eq = v == %s" expr expected)
    Assert.True(Values.asBool (Values.prop "eq" out), sprintf "%s == %s" expr expected)
    Assert.Equal(exact, Values.isExact (Values.prop "v" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-133")>]
let Arithmetic_ExactResultAtTheBound_HasFullPrecision () =
    Assert.Equal(79228162514264337593543950335m, Values.asDecimal (rOf "Output.r = 79228162514264337593543950334 + 1"))
    Assert.Equal(1.21m, Values.asDecimal (rOf "Output.r = 1.1 * 1.1"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-133")>]
let Arithmetic_ExactSum_PointOnePlusPointTwoEqualsPointThree () =
    assertRBool true "Output.r = 0.1 + 0.2 == 0.3"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-133")>]
let Arithmetic_ExactResults_AreHeldExactly () =
    let out = Script.outputOf "Output.a = 0.1 + 0.2\nOutput.b = 1.1 * 1.1\nOutput.c = 0.3 - 0.1"
    for k in [ "a"; "b"; "c" ] do Assert.True(Values.isExact (Values.prop k out), k)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-133")>]
let Arithmetic_ExactOverflowingTheExactRange_BecomesInexact () =
    let out = Script.outputOf "Output.a = 79228162514264337593543950335 + 1\nOutput.t = 79228162514264337593543950335 + 1 == 79228162514264337593543950336"
    Assert.False(Values.isExact (Values.prop "a" out))
    Assert.True(Values.asBool (Values.prop "t" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-133")>]
let Arithmetic_InexactOperand_GivesInexactResult () =
    let out = Script.outputOf "Output.a = 1e300 + 1"
    Assert.False(Values.isExact (Values.prop "a" out))
    Assert.Equal(1e300, Values.asFloat (Values.prop "a" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-133")>]
let Arithmetic_InexactProductOverflowing_RaisesNumberOverflow () =
    runtimeError MessageCode.NumberOverflow "var v = 1e300\nOutput.r = v * 1e10"

// ---- 40.2.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1955")>]
[<InlineData("Output.r = Input.s + 1")>]
[<InlineData("Output.r = 1 + Input.s")>]
[<InlineData("Output.r = Input.s + Input.s")>]
[<InlineData("Output.r = Input.s + null")>]
[<InlineData("Output.r = null + Input.s")>]
[<InlineData("Output.r = Input.s + []")>]
[<InlineData("Output.r = Input.s + {}")>]
[<InlineData("Output.r = Input.s + true")>]
[<InlineData("Output.r = Input.n + Input.s")>]
[<InlineData("Output.r = ToString(1) + 2")>]
[<InlineData("Output.r = ToString(\"\") + 0")>]
[<InlineData("var o = {s: \"x\"}\nOutput.r = o.s + 1")>]
[<InlineData("var o = {s: \"x\"}\no.s += 1")>]
[<InlineData("var n = 1\nn += Input.s")>]
let Add_StringOperandAtRunTime_RaisesStringOperandInAdditionRuntime (source: string) =
    runtimeErrorWith """{"s":"a","n":1}""" MessageCode.StringOperandInAdditionRuntime source

// "Either operand": a string kept out of validation's sight, on each side of
// `+`, against every value type on the other side (JYRO-DEF-630).
let private everyType =
    [ "null", "null"; "boolean", "true"; "number", "1"; "string", "Input.t"
      "array", "[1]"; "object", "{}"; "function", "f" ]

let private addCase (template: string) (operand: string) =
    "var f = x => x\n" + template.Replace("%s", operand)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1955")>]
[<InlineData("null")>]
[<InlineData("boolean")>]
[<InlineData("number")>]
[<InlineData("string")>]
[<InlineData("array")>]
[<InlineData("object")>]
[<InlineData("function")>]
let ``JYRO-REQ-1955 JYRO-DEF-630 Add_HiddenStringOnEitherSideOfEveryType_RaisesStringOperandInAdditionRuntime`` (kind: string) =
    let operand = everyType |> List.find (fun (k, _) -> k = kind) |> snd
    let input = """{"s":"a","t":"b","a":[1],"o":{}}"""
    for template in [ "Output.r = %s + Input.s"; "Output.r = Input.s + %s" ] do
        runtimeErrorWith input MessageCode.StringOperandInAdditionRuntime (addCase template operand)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1955")>]
[<InlineData("Output.r = Input.a + Input.s")>]
[<InlineData("Output.r = Input.o + Input.s")>]
[<InlineData("var a = [1]\na += Input.s")>]
[<InlineData("var o = {}\no += Input.s")>]
[<InlineData("var f = x => x\nf += Input.s")>]
[<InlineData("var o = {k: [1]}\no.k += Input.s")>]
let ``JYRO-REQ-1955 JYRO-DEF-630 Add_NonScalarLeftHiddenStringRight_RaisesStringOperandInAdditionRuntime`` (source: string) =
    runtimeErrorWith """{"s":"a","a":[1],"o":{}}""" MessageCode.StringOperandInAdditionRuntime source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-136")>]
[<InlineData("Output.r = [1] + 1")>]
[<InlineData("Output.r = [1] + [2]")>]
[<InlineData("Output.r = {} + 1")>]
[<InlineData("var f = x => x\nOutput.r = f + 1")>]
[<InlineData("Output.r = Input.a + null")>]
let ``JYRO-REQ-136 JYRO-DEF-630 Add_NonNumberWithoutString_StillRaisesUnsupportedBinaryOperation`` (source: string) =
    runtimeErrorWith """{"a":[1]}""" MessageCode.UnsupportedBinaryOperation source

// ---- 40.2.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-135")>]
[<InlineData("Output.r = \"a\" + 1")>]
[<InlineData("Output.r = 1 + \"a\"")>]
[<InlineData("Output.r = 'a' + 1")>]
[<InlineData("Output.r = \"a\" + \"b\"")>]
[<InlineData("Output.r = \"\" + 1")>]
[<InlineData("Output.r = 1 + (2 + \"a\")")>]
[<InlineData("var s: string = Input.x\nOutput.r = s + 1")>]
[<InlineData("var s: string = \"a\"\nOutput.r = 1 + s")>]
[<InlineData("var s = \"a\"\nOutput.r = 1 + s")>]
[<InlineData("var s = \"a\"\nOutput.r = s + 1")>]
[<InlineData("var s = \"a\"\ns = 1\nOutput.r = s + 1")>]
[<InlineData("var s = \"a\"\nfunc f() return 1 end\nOutput.r = s + f()")>]
[<InlineData("var s = \"a\"\nif true then Output.r = s + 1 end")>]
[<InlineData("var s: any = \"a\"\nOutput.r = s + 1")>]
[<InlineData("func f(s: string) return s + 1 end\nOutput.r = f(\"a\")")>]
[<InlineData("func f(s: string = \"a\") return s + 1 end")>]
[<InlineData("var s = \"a\"\ns += 1")>]                               // `+=` is tested as its `+` (40.2.8)
let ``JYRO-REQ-135 Add_StringLiteralOrStringDeclaredIdentifier_IsRejectedWithStringOperandInAddition`` (source: string) =
    rejects MessageCode.StringOperandInAddition source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-135")>]
[<InlineData("var s = \"a\" & \"b\"\nOutput.r = s + 1")>]              // initial value is not a string literal
[<InlineData("var s\ns = \"a\"\nOutput.r = s + 1")>]                   // no initial value
[<InlineData("var s = 1\ns = \"a\"\nOutput.r = s + 1")>]               // the declaration reads 1
[<InlineData("var s = \"a\"\nif true then\n  var s = 1\n  Output.r = s + 1\nend")>]  // inner declaration
[<InlineData("var s = 1\nif true then\n  var s = \"a\"\nend\nOutput.r = s + 1")>]    // outer declaration
[<InlineData("Output.r = Input.s + 1")>]
[<InlineData("func f(s = \"a\") return s + 1 end\nOutput.r = f(2)")>]  // a default is not an initial value (40.2.7)
let ``JYRO-REQ-135 Add_OperandNotAStringByItsDeclaration_IsNotRejectedWithStringOperandInAddition`` (source: string) =
    notRejectedWith MessageCode.StringOperandInAddition source

// ---- 40.2.7 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-139")>]
[<InlineData("func f(s = \"a\") return s + 1 end\nOutput.r = f(2)")>]
[<InlineData("func f(s = 'a') return s + 1 end\nOutput.r = f(2)")>]
[<InlineData("func f(s = \"a\") return 1 + s end\nOutput.r = f(2)")>]
[<InlineData("func f(s: any = \"a\") return s + 1 end\nOutput.r = f(2)")>]
[<InlineData("func f(n, s = \"a\") return n + s end\nOutput.r = f(1, 2)")>]
[<InlineData("var s = \"a\"\nfunc f(s = \"b\") return s + 1 end\nOutput.r = f(2)")>]  // resolves to the parameter
let ``JYRO-REQ-139 ParameterDefault_StringLiteral_IsNotAnInitialValue_SoPlusIsNotRejected`` (source: string) =
    notRejectedWith MessageCode.StringOperandInAddition source
    assertR "3" source

/// The requirement's own example, as written: a declaration with no call.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-139")>]
let ``JYRO-REQ-139 the requirement's example func f(s = "a") return s + 1 end is not rejected`` () =
    let source = "func f(s = \"a\") return s + 1 end"
    notRejectedWith MessageCode.StringOperandInAddition source
    compiles source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-139")>]
[<InlineData("func f(s: string) return s + 1 end")>]
[<InlineData("func f(s: string = \"a\") return s + 1 end")>]
[<InlineData("func f(s: string = \"a\") return 1 + s end")>]
[<InlineData("func f(n, s: string = 'a') return n + s end")>]
let ``JYRO-REQ-139 ParameterHintedString_WithOrWithoutDefault_IsRejectedWithStringOperandInAddition`` (source: string) =
    rejects MessageCode.StringOperandInAddition source

// ---- 40.2.8 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-140")>]
[<InlineData("var n = 1\nn += \"a\"")>]
[<InlineData("var n = 1\nn += 'a'")>]
[<InlineData("var s = \"a\"\ns += 1")>]
[<InlineData("var s = \"a\"\ns += Input.x")>]
[<InlineData("var o = {}\no.k += \"a\"")>]
[<InlineData("var a = [1]\na[0] += 'x'")>]
[<InlineData("var t: string = Input.x\nt += 1")>]
[<InlineData("var s = \"a\"\ns = 1\ns += 1")>]                       // the declaration alone (40.2.3)
[<InlineData("func f(s: string) s += 1 return s end")>]
[<InlineData("var s = \"a\"\nif true then s += 1 end")>]
[<InlineData("var n = 1\nn += \"\"")>]
let ``JYRO-REQ-140 CompoundAdd_RejectedWhenItsPlusWouldBe_WithStringOperandInAddition`` (source: string) =
    rejects MessageCode.StringOperandInAddition source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-140")>]
[<InlineData("var n = 1\nn += 2\nOutput.r = n", "3")>]
[<InlineData("func f(s = \"a\")\n  s += 1\n  return s\nend\nOutput.r = f(2)", "3")>]   // 40.2.7
[<InlineData("var s = 1\ns = \"a\"\ns = 1\ns += 2\nOutput.r = s", "3")>]  // declared with a number
let ``JYRO-REQ-140 CompoundAdd_NotRejectedWhenItsPlusWouldNotBe`` (source: string, expected: string) =
    notRejectedWith MessageCode.StringOperandInAddition source
    assertR expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-140")>]
[<InlineData("var s = 1\ns += Input.x")>]
[<InlineData("var s = 1\ns = \"a\"\ns += 0")>]
[<InlineData("var s = \"a\"\ns -= 1")>]                               // only `+=` performs a `+`
[<InlineData("var s = \"a\"\ns &= 1")>]
let ``JYRO-REQ-140 CompoundAdd_NotRejectedAtValidationWhenItsPlusWouldNotBe`` (source: string) =
    notRejectedWith MessageCode.StringOperandInAddition source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-140")>]
[<InlineData("var n = 1", "n", "\"a\"")>]
[<InlineData("var s = \"a\"", "s", "1")>]
[<InlineData("var o = {}", "o.k", "\"a\"")>]
[<InlineData("var t: string = Input.x", "t", "1")>]
[<InlineData("var n = 1", "n", "2")>]
[<InlineData("var n = 1", "n", "Input.x")>]
[<InlineData("func g() return 1 end\nvar n = 1", "n", "g()")>]
let ``JYRO-REQ-140 CompoundAdd_IsRejectedExactlyWhenTheMatchingPlusIs`` (setup: string, target: string, value: string) =
    let plus = Script.compile (sprintf "%s\nOutput.r = %s + %s" setup target value)
    let compound = Script.compile (sprintf "%s\n%s += %s" setup target value)
    let has (r: JyroResult<CompiledProgram>) = List.contains MessageCode.StringOperandInAddition (Script.codes r)
    Assert.Equal(has plus, has compound)

// ---- 40.2.9 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-141")>]
[<InlineData("var s = \"a\"\ns++")>]
[<InlineData("var s = \"a\"\ns--")>]
[<InlineData("var s = \"a\"\n++s")>]
[<InlineData("var s = \"a\"\n--s")>]
[<InlineData("var t: string = \"b\"\nt--")>]
[<InlineData("var t: string = \"b\"\nt++")>]
[<InlineData("var t: string = Input.x\nt++")>]
[<InlineData("var u = \"c\"\n++u")>]
[<InlineData("var u = \"c\"\n--u")>]
[<InlineData("var u = \"c\"\r\n++u")>]
[<InlineData("var s = 'a'\nOutput.r = [s++]")>]
[<InlineData("var s = \"a\"\nOutput.r = --s")>]
[<InlineData("var s = \"a\"\ns = 1\ns++")>]                          // the declaration alone
[<InlineData("func f(s: string)\n  s++\n  return s\nend")>]
[<InlineData("func f(s: string = \"a\")\n  --s\n  return s\nend")>]
[<InlineData("var s = \"a\"\nif true then s-- end")>]
let ``JYRO-REQ-141 IncrementOrDecrement_OfAStringDeclaredVariable_IsRejectedWithStringOperandInAddition`` (source: string) =
    rejects MessageCode.StringOperandInAddition source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-141")>]
[<InlineData("func f(s = \"a\")\n  s++\n  return s\nend\nOutput.r = f(1)", "2")>]   // 40.2.7
[<InlineData("func f(s = \"a\")\n  s--\n  return s\nend\nOutput.r = f(1)", "0")>]
[<InlineData("var n = 1\nn++\nOutput.r = n", "2")>]
[<InlineData("var n = 1\n--n\nOutput.r = n", "0")>]
let ``JYRO-REQ-141 IncrementOrDecrement_NotOfAStringDeclaredVariable_IsNotRejected`` (source: string, expected: string) =
    notRejectedWith MessageCode.StringOperandInAddition source
    assertR expected source

/// The requirement's own example, as written on one line, with no call.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-141")>]
let ``JYRO-REQ-141 the requirement's example func f(s = "a") s++ return s end is not rejected`` () =
    notRejectedWith MessageCode.StringOperandInAddition "func f(s = \"a\") s++ return s end"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-141")>]
[<InlineData("var o = {k: \"a\"}\no.k++")>]        // not an identifier: runtime only
[<InlineData("var s = Input.s\ns++")>]
[<InlineData("var s = 1\ns = \"a\"\ns++")>]           // declared with a number
[<InlineData("var s\ns = \"a\"\ns--")>]
let ``JYRO-REQ-141 IncrementOrDecrement_OperandNotAStringByItsDeclaration_IsNotRejectedAtValidation`` (source: string) =
    notRejectedWith MessageCode.StringOperandInAddition source

// ---- 40.2.4 --------------------------------------------------------------

let nonNumbers = [ "null"; "true"; "false"; "[]"; "[1]"; "{}"; "x => x" ]
let arithmeticOps = [ "+"; "-"; "*"; "/"; "%" ]

let nonNumberOperandCases : seq<obj[]> =
    seq {
        for op in arithmeticOps do
            for v in nonNumbers do
                yield [| box op; box v; box true |]
                yield [| box op; box v; box false |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-136")>]
[<MemberData(nameof nonNumberOperandCases)>]
let ``JYRO-REQ-136 Arithmetic_NonNumberOperand_RaisesUnsupportedBinaryOperation`` (op: string, value: string, onLeft: bool) =
    let source =
        if onLeft then sprintf "var a = %s\nvar b = 1\nOutput.r = a %s b" value op
        else sprintf "var a = 1\nvar b = %s\nOutput.r = a %s b" value op
    runtimeError MessageCode.UnsupportedBinaryOperation source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-136")>]
[<InlineData("Output.r = Input.s - 1")>]
[<InlineData("Output.r = 1 - Input.s")>]
[<InlineData("Output.r = 1 * Input.s")>]
[<InlineData("Output.r = Input.s / 2")>]
[<InlineData("Output.r = Input.s % 2")>]
[<InlineData("Output.r = Input.s - Input.s")>]
[<InlineData("var x = 1\nx -= Input.s")>]
let ``JYRO-REQ-136 Arithmetic_StringOperandOfOperatorOtherThanPlus_RaisesUnsupportedBinaryOperation`` (source: string) =
    runtimeErrorWith """{"s":"4"}""" MessageCode.UnsupportedBinaryOperation source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-136")>]
[<InlineData("var a = null\nvar b = true\nOutput.r = a + b")>]
[<InlineData("var a = []\nvar b = {}\nOutput.r = a * b")>]
[<InlineData("var a = null\nvar b = null\nOutput.r = a - b")>]
let ``JYRO-REQ-136 Arithmetic_BothOperandsNonNumbers_RaisesUnsupportedBinaryOperation`` (source: string) =
    runtimeError MessageCode.UnsupportedBinaryOperation source

/// The inputs that produced the array `+` removed by jyro-spec--d-499
/// (floors-process 4.2.4): two arrays, two empty arrays, and a compound
/// assignment. Each must raise UnsupportedBinaryOperation, and the array that
/// `+=` would have changed must stay as it was.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-136")>]
[<InlineData("Output.r = [1] + [2]")>]
[<InlineData("Output.r = [] + []")>]
[<InlineData("var a = [1]\nOutput.a = a\na += [2]")>]
let ``JYRO-REQ-136 Arithmetic_ArrayPlusArray_TheRemovedBehaviorOfD499_RaisesUnsupportedBinaryOperation`` (source: string) =
    runtimeError MessageCode.UnsupportedBinaryOperation source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-136")>]
let ``JYRO-REQ-136 Arithmetic_ArrayPlusAssign_LeavesTheArrayAsItWas`` () =
    // 130.2.2: the result's value is the final Output, also after the error.
    let r = Script.runWith Script.defaults "var a = [1]\nOutput.a = a\na += [2]"
    Script.expectError MessageCode.UnsupportedBinaryOperation r
    Assert.Equal("[1]", Values.toJson (Script.valueOf r |> Values.prop "a"))

// ---- 40.2.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-137")>]
[<InlineData("5", "0 - 5", true)>]
[<InlineData("-5", "5", true)>]
[<InlineData("0", "0", true)>]
[<InlineData("2.50", "0 - 2.5", true)>]
[<InlineData("79228162514264337593543950335", "0 - 79228162514264337593543950335", true)>]
[<InlineData("0.0000000000000000000000000001", "0 - 0.0000000000000000000000000001", true)>]
[<InlineData("1e300", "0 - 1e300", false)>]
let Negate_Number_GivesItsNegation (value: string, expected: string, exact: bool) =
    let out = Script.outputOf (sprintf "var x = %s\nvar v = -x\nOutput.v = v\nOutput.eq = v == (%s)" value expected)
    Assert.True(Values.asBool (Values.prop "eq" out), sprintf "-(%s) == %s" value expected)
    Assert.Equal(exact, Values.isExact (Values.prop "v" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-137")>]
let Negate_DoubleNegation_GivesTheValue () =
    assertRBool true "Output.r = -(-5) == 5"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-137")>]
let Negate_ExactOperand_GivesExactResult () =
    assertRBool true "var x = 0.1\nOutput.r = -x + 0.3 == 0.2"
    let out = Script.outputOf "var x = 0.1\nOutput.r = -x"
    Assert.True(Values.isExact (Values.prop "r" out))
    Assert.Equal(-0.1m, Values.asDecimal (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-137")>]
let Negate_ExactInputNumber_GivesExactResult () =
    let out = Script.output (Script.runJson """{"n":1.1}""" "Output.r = -Input.n")
    Assert.True(Values.isExact (Values.prop "r" out))
    Assert.Equal(-1.1m, Values.asDecimal (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-137")>]
let Negate_InexactOperand_GivesNegatedDouble () =
    let out = Script.outputOf "var x = 1e300\nOutput.r = -x"
    Assert.False(Values.isExact (Values.prop "r" out))
    Assert.Equal(-1e300, Values.asFloat (Values.prop "r" out))

// ---- 40.2.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-138")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("\"5\"")>]
[<InlineData("\"\"")>]
[<InlineData("[]")>]
[<InlineData("[1]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let Negate_NonNumber_RaisesUnsupportedUnaryOperation (value: string) =
    runtimeError MessageCode.UnsupportedUnaryOperation (sprintf "var v = %s\nOutput.r = -v" value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-138")>]
let Negate_NumericStringFromInput_RaisesUnsupportedUnaryOperation () =
    runtimeErrorWith """{"s":"1"}""" MessageCode.UnsupportedUnaryOperation "Output.r = -Input.s"

// ---- 40.3.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1751")>]
[<InlineData("Output.r = 0 / 0")>]
[<InlineData("Output.r = 1 / -0")>]
[<InlineData("Output.r = 1 / 0.0")>]
[<InlineData("var z = 0\nOutput.r = 1 / z")>]
[<InlineData("var z = 0.0\nOutput.r = 1 / z")>]
[<InlineData("var z = -0\nOutput.r = 1 / z")>]
[<InlineData("var z = 0\nOutput.r = -1 / z")>]
[<InlineData("var z = 0\nOutput.r = 0 / z")>]
[<InlineData("var z = 0\nOutput.r = 1.5 / z")>]
[<InlineData("var z = 0\nOutput.r = 1e300 / z")>]
[<InlineData("var z = 1e300 - 1e300\nOutput.r = 1 / z")>]
[<InlineData("var z = 1e300 - 1e300\nOutput.r = 1e300 / z")>]
[<InlineData("Output.r = 1 / 0")>]
[<InlineData("var x = 5\nx /= 0")>]
let ``JYRO-REQ-1751 Divide_NumberByZero_RaisesDivisionByZero`` (source: string) =
    runtimeError MessageCode.DivisionByZero source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1751")>]
[<InlineData("""{"z":0}""")>]
[<InlineData("""{"z":0.0}""")>]
[<InlineData("""{"z":-0}""")>]
[<InlineData("""{"z":-0.0}""")>]
let ``JYRO-REQ-1751 Divide_ByZeroFromInput_RaisesDivisionByZero`` (input: string) =
    runtimeErrorWith input MessageCode.DivisionByZero "Output.r = 1 / Input.z"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1751")>]
[<InlineData("Output.r = \"a\" / 0")>]
[<InlineData("Output.r = null / 0")>]
[<InlineData("var v = \"a\"\nOutput.r = v / 0")>]
[<InlineData("var v = null\nOutput.r = v / 0")>]
[<InlineData("var v = true\nOutput.r = v / 0")>]
[<InlineData("var v = []\nOutput.r = v / 0")>]
[<InlineData("var v = {}\nOutput.r = v / 0")>]
[<InlineData("var v = x => x\nOutput.r = v / 0")>]
[<InlineData("var v = \"0\"\nOutput.r = v / 0")>]
[<InlineData("var v = \"a\"\nOutput.r = v / 0.0")>]
[<InlineData("var v = \"a\"\nOutput.r = v / 1")>]
[<InlineData("var v = \"0\"\nOutput.r = 1 / v")>]
[<InlineData("var v = null\nOutput.r = 1 / v")>]
[<InlineData("var v = false\nOutput.r = 1 / v")>]
[<InlineData("var v = \"a\"\nOutput.r = v / \"b\"")>]
[<InlineData("var v = \"a\"\nv /= 0")>]
let ``JYRO-REQ-1751 Divide_NonNumberOperand_RaisesUnsupportedBinaryOperationWhateverTheRightOperand`` (source: string) =
    runtimeError MessageCode.UnsupportedBinaryOperation source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1751")>]
let ``JYRO-REQ-1751 Divide_NonNumberOperandFromInput_RaisesUnsupportedBinaryOperation`` () =
    runtimeErrorWith """{"s":"a"}""" MessageCode.UnsupportedBinaryOperation "Output.r = Input.s / 0"
    runtimeErrorWith """{"s":"0"}""" MessageCode.UnsupportedBinaryOperation "Output.r = 1 / Input.s"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1751")>]
let ``JYRO-REQ-1751 Divide_CompoundNumberByZero_RaisesDivisionByZero`` () =
    runtimeError MessageCode.DivisionByZero "var x = 5\nx /= 0"
    runtimeError MessageCode.DivisionByZero "var x = 5\nx /= 0.0"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1751")>]
let ``JYRO-REQ-1751 Divide_BySmallestNonZeroExact_Succeeds`` () =
    assertRBool true "Output.r = 1 / 0.0000000000000000000000000001 == 10000000000000000000000000000"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1751")>]
[<InlineData("Output.r = 1 / 0.0000001", "10000000")>]
[<InlineData("Output.r = 6 / 3", "2")>]
[<InlineData("Output.r = 0 / 5", "0")>]
[<InlineData("var z = 1e-300\nOutput.r = 1 / z > 0", "true")>]
let ``JYRO-REQ-1751 Divide_ByNonZeroNumber_DoesNotRaise`` (source: string, expected: string) =
    assertR expected source

// ---- 40.3.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1753")>]
[<InlineData("Output.r = 0 % 0")>]
[<InlineData("Output.r = 5 % -0")>]
[<InlineData("Output.r = 5 % 0.0")>]
[<InlineData("var x = 5\nx %= 0.0")>]
[<InlineData("var z = 0\nOutput.r = 5 % z")>]
[<InlineData("var z = 0.0\nOutput.r = 5 % z")>]
[<InlineData("var z = -0\nOutput.r = 5 % z")>]
[<InlineData("var z = 0\nOutput.r = 0 % z")>]
[<InlineData("var z = 0\nOutput.r = 1e300 % z")>]
[<InlineData("var z = 1e300 - 1e300\nOutput.r = 5 % z")>]
[<InlineData("Output.r = 5 % 0")>]
[<InlineData("var x = 5\nx %= 0")>]
let ``JYRO-REQ-1753 Modulo_NumberByZero_RaisesModuloByZero`` (source: string) =
    runtimeError MessageCode.ModuloByZero source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1753")>]
[<InlineData("""{"z":0}""")>]
[<InlineData("""{"z":0.0}""")>]
[<InlineData("""{"z":-0.0}""")>]
let ``JYRO-REQ-1753 Modulo_ByZeroFromInput_RaisesModuloByZero`` (input: string) =
    runtimeErrorWith input MessageCode.ModuloByZero "Output.r = 5 % Input.z"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1753")>]
[<InlineData("Output.r = \"a\" % 0")>]
[<InlineData("Output.r = null % 0")>]
[<InlineData("var v = \"a\"\nOutput.r = v % 0")>]
[<InlineData("var v = null\nOutput.r = v % 0")>]
[<InlineData("var v = true\nOutput.r = v % 0")>]
[<InlineData("var v = []\nOutput.r = v % 0")>]
[<InlineData("var v = {}\nOutput.r = v % 0")>]
[<InlineData("var v = x => x\nOutput.r = v % 0")>]
[<InlineData("var v = \"0\"\nOutput.r = v % 0")>]
[<InlineData("var v = \"a\"\nOutput.r = v % 0.0")>]
[<InlineData("var v = \"a\"\nOutput.r = v % 3")>]
[<InlineData("var v = \"0\"\nOutput.r = 5 % v")>]
[<InlineData("var v = null\nOutput.r = 5 % v")>]
[<InlineData("var v = false\nOutput.r = 5 % v")>]
[<InlineData("var v = \"a\"\nv %= 0")>]
let ``JYRO-REQ-1753 Modulo_NonNumberOperand_RaisesUnsupportedBinaryOperationWhateverTheRightOperand`` (source: string) =
    runtimeError MessageCode.UnsupportedBinaryOperation source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1753")>]
let ``JYRO-REQ-1753 Modulo_CompoundNumberByZero_RaisesModuloByZeroNotDivisionByZero`` () =
    runtimeError MessageCode.ModuloByZero "var x = 5\nx %= 0"
    runtimeError MessageCode.ModuloByZero "var x = 0\nx %= 0"
    runtimeError MessageCode.ModuloByZero "var x = 1e300\nx %= 0"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1753")>]
let ``JYRO-REQ-1753 Modulo_ByNonZeroFraction_Succeeds`` () =
    assertRBool true "Output.r = 5 % 0.5 == 0"
    assertRBool true "Output.r = 5 % 0.0000001 == 0"

// ---- 40.3.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1756")>]
[<InlineData("7", "3", "1")>]
[<InlineData("-7", "3", "-1")>]
[<InlineData("7", "-3", "1")>]
[<InlineData("-7", "-3", "-1")>]
[<InlineData("7.5", "2", "1.5")>]
[<InlineData("-7.5", "2", "-1.5")>]
[<InlineData("5", "5", "0")>]
[<InlineData("0", "3", "0")>]
[<InlineData("2", "5", "2")>]
[<InlineData("-2", "5", "-2")>]
[<InlineData("1", "0.3", "0.1")>]
[<InlineData("-0.5", "0.2", "-0.1")>]
[<InlineData("10", "4", "2")>]
[<InlineData("-6", "3", "0")>]
let ``JYRO-REQ-1756 Modulo_GivesTruncatedRemainderWithTheSignOfTheLeftOperand`` (a: string, b: string, expected: string) =
    assertRStr expected (sprintf "var a = %s\nvar b = %s\nOutput.r = (a %% b) & \"\"" a b)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1756")>]
let ``JYRO-REQ-1756 Modulo_SpecExamples_HoldAsWritten`` () =
    assertRBool true "Output.r = 7 % 3 == 1"
    assertRBool true "Output.r = -7 % 3 == -1"
    assertRBool true "Output.r = 7 % -3 == 1"
    assertRBool true "Output.r = 1 % 0.3 == 0.1"
    assertRBool true "Output.r = 5 % 0.5 == 0"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1756")>]
let ``JYRO-REQ-1756 Modulo_ExactOperands_GiveExactResult`` () =
    assertRBool true "Output.r = 1 % 0.3 == 0.1"
    Assert.True(Values.isExact (rOf "Output.r = 1 % 0.3"))
    Assert.True(Values.isExact (rOf "Output.r = 7 % 3"))
    Assert.True(Values.isExact (rOf "Output.r = 5 % 0.5"))
    Assert.True(Values.isExact (rOf "Output.r = -7 % 3"))

// The quotient is exact: where the quotient that `/` gives would be rounded,
// the remainder is still `a - b * t` for the true quotient's integer part.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1756")>]
let ``JYRO-REQ-1756 Modulo_ExactOperandsWhoseQuotientNeedsRounding_GiveTheExactRemainder`` () =
    assertRStr "1" "Output.r = (10000000000000000 % 3) & \"\""
    assertRBool true "Output.r = 10000000000000000 % 3 == 1"
    assertRBool true "Output.r = 79228162514264337593543950335 % 10 == 5"
    assertRBool true "Output.r = -79228162514264337593543950335 % 10 == -5"
    assertRBool true "Output.r = 79228162514264337593543950335 % 11 == 8"
    Assert.True(Values.isExact (rOf "Output.r = 79228162514264337593543950335 % 10"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1756")>]
let ``JYRO-REQ-1756 Modulo_ResultZero_IsZero`` () =
    assertRBool true "Output.r = -6 % 3 == 0"
    assertRBool true "Output.r = 6 % -3 == 0"
    assertRBool true "Output.r = 0 % 3 == 0"
    assertRBool true "Output.r = 5 % 0.5 == 0"

/// `Output.r` of `source` run with `Input` = { a: <inexact a>, b: <inexact b> }:
/// the host gives doubles (30.2.10), which a literal of the source cannot.
let private rOfInexactInput (a: float) (b: float) (source: string) : JyroValue =
    let input = Values.obj [ "a", Values.inexact a; "b", Values.inexact b ]
    Script.output (Script.runInput input source) |> Values.prop "r"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1756")>]
[<InlineData("Output.r = 5 % Input.b", 5.0, 2.0, 1.0)>]
[<InlineData("Output.r = Input.a % 2", 5.0, 2.0, 1.0)>]
[<InlineData("Output.r = -5 % Input.b", 5.0, 2.0, -1.0)>]
[<InlineData("Output.r = Input.a % Input.b", -5.0, 2.0, -1.0)>]
let ``JYRO-REQ-1756 Modulo_EitherOperandInexact_GivesInexactResult`` (source: string, a: float, b: float, expected: float) =
    let r = rOfInexactInput a b source
    Assert.False(Values.isExact r)
    Assert.Equal(expected, Values.asFloat r)

// Doubles given by the host; the remainder is that of the exact quotient, which
// is IEEE 754 fmod, here `%` on doubles. In each row the quotient that `/` gives
// is rounded to a double, so a - b * trunc(a / b) differs from it.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1756")>]
[<InlineData(1.0, 0.1)>]
[<InlineData(0.7, 0.1)>]
[<InlineData(0.3, 0.1)>]
[<InlineData(1e17, 3.0)>]
[<InlineData(-1e17, 3.0)>]
[<InlineData(1e17, -3.0)>]
[<InlineData(9007199254740991.0, 2.0)>]
[<InlineData(5.5, 0.1)>]
[<InlineData(123456789.123456789, 0.0001)>]
let ``JYRO-REQ-1756 Modulo_InexactOperandsWhoseQuotientNeedsRounding_GiveTheFmodOfTheExactQuotient`` (a: float, b: float) =
    let r = rOfInexactInput a b "Output.r = Input.a % Input.b"
    Assert.False(Values.isExact r)
    Assert.Equal(a % b, Values.asFloat r)
    if a % b <> 0.0 then Assert.Equal(sign a, sign (Values.asFloat r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1756")>]
[<InlineData(1e300, 7.0)>]
[<InlineData(-1e300, 7.0)>]
[<InlineData(1e300, -7.0)>]
[<InlineData(7.0, 1e300)>]
[<InlineData(1e300, 3e299)>]
let ``JYRO-REQ-1756 Modulo_InexactOperands_GiveDoubleTruncatedRemainder`` (a: float, b: float) =
    let expected = a % b   // IEEE fmod: a - b * t, t the exact quotient truncated toward zero, sign of a
    let src = sprintf "var a = %s\nvar b = %s\nOutput.r = a %% b" (a.ToString("R", System.Globalization.CultureInfo.InvariantCulture)) (b.ToString("R", System.Globalization.CultureInfo.InvariantCulture))
    let src = src.Replace("E+", "e").Replace("E-", "e-").Replace("var a = -", "var a = -").Replace("var b = -", "var b = -")
    let r = rOf src
    Assert.Equal(expected, Values.asFloat r)
    Assert.Equal(sign expected, sign (Values.asFloat r))

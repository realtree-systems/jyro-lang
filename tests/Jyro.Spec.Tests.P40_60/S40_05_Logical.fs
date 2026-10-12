/// 40.5 Logical and conditional operators, 40.6 Type check.
module RealTree.Jyro.Spec.Tests.P40_60.S40_05_Logical

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// ---- 40.5.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-152")>]
[<InlineData("0", "5", "0")>]
[<InlineData("\"\"", "5", "\"\"")>]
[<InlineData("null", "5", "null")>]
[<InlineData("false", "5", "false")>]
[<InlineData("1", "5", "5")>]
[<InlineData("-1", "\"b\"", "\"b\"")>]
[<InlineData("[]", "5", "5")>]
[<InlineData("{}", "\"x\"", "\"x\"")>]
[<InlineData("\"a\"", "[1]", "[1]")>]
[<InlineData("\"false\"", "7", "7")>]
[<InlineData("true", "null", "null")>]
[<InlineData("2", "0", "0")>]
[<InlineData("true", "false", "false")>]
let And_GivesLeftWhenFalsyElseRight (a: string, b: string, expected: string) =
    assertR expected (sprintf "Output.r = %s and %s" a b)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-152")>]
let And_TruthyLeft_GivesTheRightOperandItself () =
    assertR "[9]" "var a = [1]\nOutput.r = true and a\na[0] = 9"

// ---- 40.5.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-153")>]
[<InlineData("0", "5", "5")>]
[<InlineData("\"\"", "\"x\"", "\"x\"")>]
[<InlineData("null", "false", "false")>]
[<InlineData("false", "0", "0")>]
[<InlineData("1", "5", "1")>]
[<InlineData("[]", "5", "[]")>]
[<InlineData("{}", "5", "{}")>]
[<InlineData("\"a\"", "null", "\"a\"")>]
[<InlineData("-1", "5", "-1")>]
[<InlineData("true", "5", "true")>]
[<InlineData("\"0\"", "5", "\"0\"")>]
let Or_GivesLeftWhenTruthyElseRight (a: string, b: string, expected: string) =
    assertR expected (sprintf "Output.r = %s or %s" a b)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-153")>]
let Or_TruthyLeft_GivesTheLeftOperandItself () =
    assertR "[9]" "var a = [1]\nOutput.r = a or 5\na[0] = 9"

// ---- 40.5.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-154")>]
[<InlineData("null", "1", "1")>]
[<InlineData("0", "1", "0")>]
[<InlineData("false", "1", "false")>]
[<InlineData("\"\"", "1", "\"\"")>]
[<InlineData("[]", "1", "[]")>]
[<InlineData("{}", "1", "{}")>]
[<InlineData("null", "null", "null")>]
[<InlineData("\"a\"", "null", "\"a\"")>]
[<InlineData("null", "\"b\"", "\"b\"")>]
let Coalesce_GivesLeftUnlessNull (a: string, b: string, expected: string) =
    assertR expected (sprintf "Output.r = %s ?? %s" a b)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-154")>]
let Coalesce_MissingInputProperty_GivesRight () =
    assertR "7" "Output.r = Input.missing ?? 7"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-154")>]
let Coalesce_NonNullLeft_GivesTheLeftOperandItself () =
    assertR "[9]" "var a = [1]\nOutput.r = a ?? 5\na[0] = 9"

// ---- 40.5.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-155")>]
[<InlineData("false and P(Output, \"hit\", 1)", "false")>]
[<InlineData("0 and P(Output, \"hit\", 1)", "0")>]
[<InlineData("null and P(Output, \"hit\", 1)", "null")>]
[<InlineData("true or P(Output, \"hit\", 1)", "true")>]
[<InlineData("\"a\" or P(Output, \"hit\", 1)", "\"a\"")>]
[<InlineData("1 ?? P(Output, \"hit\", 1)", "1")>]
[<InlineData("false ?? P(Output, \"hit\", 1)", "false")>]
[<InlineData("0 and (1 / 0)", "0")>]
[<InlineData("1 or (1 / 0)", "1")>]
[<InlineData("false ?? (1 / 0)", "false")>]
let ShortCircuit_LeftDecides_RightNotEvaluated (expr: string, expected: string) =
    // No log property: the probe did not run; success: the failing operand did not run.
    assertOut (sprintf """{"r": %s}""" expected) (probe + sprintf "Output.r = %s" expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-155")>]
[<InlineData("true and P(Output, \"hit\", 1)")>]
[<InlineData("false or P(Output, \"hit\", 1)")>]
[<InlineData("null ?? P(Output, \"hit\", 1)")>]
let ShortCircuit_LeftDoesNotDecide_RightIsEvaluated (expr: string) =
    assertOut """{"log": ["hit"], "r": 1}""" (probe + sprintf "Output.r = %s" expr)

// ---- 40.5.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-156")>]
[<InlineData("null", true)>]
[<InlineData("false", true)>]
[<InlineData("0", true)>]
[<InlineData("0.0", true)>]
[<InlineData("-0", true)>]
[<InlineData("1e300 - 1e300", true)>]
[<InlineData("\"\"", true)>]
[<InlineData("true", false)>]
[<InlineData("1", false)>]
[<InlineData("-1", false)>]
[<InlineData("0.5", false)>]
[<InlineData("\"a\"", false)>]
[<InlineData("\"false\"", false)>]
[<InlineData("\"0\"", false)>]
[<InlineData("[]", false)>]
[<InlineData("{}", false)>]
[<InlineData("x => x", false)>]
let Not_GivesTrueExactlyForFalsyValues (value: string, expected: bool) =
    assertRBool expected (sprintf "var v = %s\nOutput.r = not v" value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-156")>]
let Not_Twice_GivesABoolean () =
    assertRBool false "Output.r = not not 0"
    assertRBool true "Output.r = not not \"a\""

// ---- 40.5.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-157")>]
[<InlineData("true", true)>]
[<InlineData("1", true)>]
[<InlineData("\"a\"", true)>]
[<InlineData("[]", true)>]
[<InlineData("{}", true)>]
[<InlineData("false", false)>]
[<InlineData("0", false)>]
[<InlineData("\"\"", false)>]
[<InlineData("null", false)>]
let Conditional_EvaluatesConditionThenOnlyTheChosenBranch (cond: string, truthy: bool) =
    let src = probe + sprintf "Output.r = P(Output, \"c\", %s) ? P(Output, \"x\", 1) : P(Output, \"y\", 2)" cond
    if truthy then assertOut """{"log": ["c", "x"], "r": 1}""" src
    else assertOut """{"log": ["c", "y"], "r": 2}""" src

[<Fact>]
[<Trait("requirement", "JYRO-REQ-157")>]
let Conditional_UnchosenBranchThatWouldFail_IsNotEvaluated () =
    assertR "1" "var c = 1\nOutput.r = c ? 1 : (1 / 0)"
    assertR "2" "var c = 0\nOutput.r = c ? (1 / 0) : 2"

// ---- 40.6.1 / 40.6.2 -----------------------------------------------------

let typeNames = [ "null"; "boolean"; "number"; "string"; "array"; "object"; "function" ]

/// (value expression, its type name)
let typedValues =
    [ "null", "null"
      "true", "boolean"
      "false", "boolean"
      "0", "number"
      "1.5", "number"
      "1e300", "number"
      "\"\"", "string"
      "\"a\"", "string"
      "[]", "array"
      "{}", "object"
      "x => x", "function"
      "V(1)", "object"
      "Input.o", "object"
      "Input.a", "array"
      "Input.n", "number"
      "Input.s", "string" ]

let typeCheckCases : seq<obj[]> =
    seq {
        for (v, t) in typedValues do
            for name in typeNames do
                yield [| box v; box name; box (t = name) |]
    }

let private typeCheckRun (source: string) =
    let full = "union U V(x) end\n" + source
    Values.asBool (Values.prop "r" (Script.output (Script.runJson """{"o":{},"a":[],"n":2,"s":"x"}""" full)))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-158")>]
[<MemberData(nameof typeCheckCases)>]
let Is_GivesTrueExactlyWhenValueHasTheType (value: string, typeName: string, expected: bool) =
    Assert.Equal(expected, typeCheckRun (sprintf "var v = %s\nOutput.r = v is %s" value typeName))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-158")>]
let Is_LambdaInVariable_IsFunction () =
    assertRBool true "var f = x => x\nOutput.r = f is function"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-159")>]
[<MemberData(nameof typeCheckCases)>]
let IsNot_GivesTheNegationOfIs (value: string, typeName: string, isResult: bool) =
    Assert.Equal(not isResult, typeCheckRun (sprintf "var v = %s\nOutput.r = v is not %s" value typeName))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-159")>]
[<InlineData("null")>]
[<InlineData("0")>]
[<InlineData("\"\"")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let IsNot_Any_IsFalse (value: string) =
    assertRBool false (sprintf "var v = %s\nOutput.r = v is not any" value)

// ---- 40.6.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-160")>]
[<InlineData("null")>]
[<InlineData("boolean")>]
[<InlineData("number")>]
[<InlineData("string")>]
[<InlineData("array")>]
[<InlineData("object")>]
[<InlineData("function")>]
[<InlineData("any")>]
let TypeName_OfTheEightIsAccepted (name: string) =
    compiles (sprintf "var x = 1\nOutput.a = x is %s\nOutput.b = x is not %s" name name)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-160")>]
[<InlineData("integer")>]
[<InlineData("int")>]
[<InlineData("Number")>]
[<InlineData("Any")>]
[<InlineData("NULL")>]
[<InlineData("foo")>]
[<InlineData("undefined")>]
[<InlineData("\"number\"")>]
[<InlineData("date")>]
[<InlineData("bool")>]
let TypeName_OtherWord_IsRejectedBeforeExecution (name: string) =
    compileFails (sprintf "var x = 1\nOutput.a = x is %s" name)
    compileFails (sprintf "var x = 1\nOutput.a = x is not %s" name)

// ---- 40.6.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-161")>]
[<InlineData("null")>]
[<InlineData("false")>]
[<InlineData("true")>]
[<InlineData("0")>]
[<InlineData("1e300")>]
[<InlineData("\"\"")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let IsAny_IsTrueForEveryValue (value: string) =
    assertRBool true (sprintf "var v = %s\nOutput.r = v is any" value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-161")>]
let IsAny_IsTrueForCyclicValue () =
    assertRBool true "var a = []\na[0] = a\nOutput.r = a is any"

// ---- 40.6.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-162")>]
[<InlineData("is any", true)>]
[<InlineData("is not any", false)>]
[<InlineData("is number", true)>]
[<InlineData("is not string", true)>]
let IsLeftOperand_IsAlwaysEvaluated_EffectOccurs (check: string, expected: bool) =
    assertOut (sprintf """{"log": ["hit"], "r": %s}""" (if expected then "true" else "false"))
        (probe + sprintf "Output.r = P(Output, \"hit\", 1) %s" check)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-162")>]
[<InlineData("is any")>]
[<InlineData("is not any")>]
[<InlineData("is number")>]
let IsLeftOperand_ErrorPropagates_DivisionByZero (check: string) =
    runtimeError MessageCode.DivisionByZero (sprintf "var z = 0\nOutput.r = (1 / z) %s" check)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-162")>]
[<InlineData("is any")>]
[<InlineData("is not any")>]
let IsLeftOperand_ErrorPropagates_ArgumentTypeMismatch (check: string) =
    runtimeError MessageCode.ArgumentTypeMismatch (sprintf "var n = 1\nOutput.r = Keys(n) %s" check)

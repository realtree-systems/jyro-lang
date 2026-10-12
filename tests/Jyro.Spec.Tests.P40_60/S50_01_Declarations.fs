/// 50.1 Variable declarations, 50.2 Assignment and expression statements.
module RealTree.Jyro.Spec.Tests.P40_60.S50_01_Declarations

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// ---- 50.1.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-188")>]
[<InlineData("var a")>]
[<InlineData("var b = 1")>]
[<InlineData("var c: any")>]
[<InlineData("var d: number = 2")>]
[<InlineData("var e: any = 1")>]
[<InlineData("var e: function = x => x")>]
[<InlineData("var e: number = 1")>]
[<InlineData("var e: string = \"s\"")>]
[<InlineData("var e: boolean = true")>]
[<InlineData("var e: object = {}")>]
[<InlineData("var e: array = []")>]
[<InlineData("var e: null = null")>]
let VarDeclaration_OfTheFourForms_IsAcceptedAndRuns (source: string) =
    Assert.True(Script.ok (Script.run (source + "\nOutput.done = true")), "the declaration compiles and runs")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-188")>]
[<InlineData("var")>]
[<InlineData("var = 1")>]
[<InlineData("var 1x = 1")>]
[<InlineData("var a: = 1")>]
[<InlineData("var a: foo = 1")>]
[<InlineData("var a: Number = 1")>]
[<InlineData("var a: int = 1")>]
[<InlineData("var a b")>]
[<InlineData("var a == 1")>]
[<InlineData("var a: number: string = 1")>]
[<InlineData("var if = 1")>]
[<InlineData("var a = ")>]
[<InlineData("var a: = ")>]
[<InlineData("var \"a\" = 1")>]
[<InlineData("var a.b = 1")>]
[<InlineData("var (a) = 1")>]
let VarDeclaration_OtherShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 50.1.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1962")>]
[<InlineData("var a\nOutput.t = a is null\nOutput.s = a & \"\"")>]
[<InlineData("var c: any\nOutput.t = c is null\nOutput.s = c & \"\"")>]
[<InlineData("if true then\n  var a\n  Output.t = a is null\n  Output.s = a & \"\"\nend")>]
[<InlineData("foreach q in [1, 2] do\n  var a\n  Output.t = a is null\n  Output.s = a & \"\"\n  a = 5\nend")>]
[<InlineData("func f()\n  var a\n  return a\nend\nOutput.t = f() is null\nOutput.s = f() & \"\"")>]
let VarDeclaration_WithoutInitialValue_HoldsNull (source: string) =
    let out = Script.outputOf source
    Assert.True(Values.asBool (Values.prop "t" out))
    Assert.Equal("null", Values.asString (Values.prop "s" out))

// ---- 50.1.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1963")>]
[<InlineData("number")>]
[<InlineData("string")>]
[<InlineData("boolean")>]
[<InlineData("array")>]
[<InlineData("object")>]
[<InlineData("function")>]
[<InlineData("null")>]
let ``JYRO-REQ-1963 TypedVariable_WithoutInitialValue_IsRejectedWithTypedVariableNeedsInitializer`` (hint: string) =
    for source in
        [ sprintf "var x: %s" hint
          sprintf "if true then\n  var x: %s\nend" hint
          sprintf "func f()\n  var x: %s\nend" hint
          sprintf "while false do\n  var x: %s\nend" hint
          sprintf "foreach q in [1] do\n  var x: %s\nend" hint
          sprintf "for i in 0 to 1 do\n  var x: %s\nend" hint ] do
        rejectedAt MessageCode.TypedVariableNeedsInitializer "Validate" source |> Script.expectError MessageCode.TypedVariableNeedsInitializer

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1963")>]
[<InlineData("var x: any\nOutput.r = x", "null")>]
[<InlineData("var x: string = \"a\"\nOutput.r = x", "\"a\"")>]
[<InlineData("var x: number = 0\nOutput.r = x", "0")>]
[<InlineData("var x: null = null\nOutput.r = x", "null")>]
[<InlineData("func f()\n  var x: any\n  return x\nend\nOutput.r = f()", "null")>]
[<InlineData("foreach q in [1] do\n  var x: any\n  Output.r = x\nend", "null")>]
let ``JYRO-REQ-1963 TypedVariable_AnyOrWithInitialValue_IsAccepted`` (source: string, expected: string) =
    assertRL expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1963")>]
[<InlineData("func f(s: string) return s end\nOutput.r = f(5)", "\"5\"")>]
[<InlineData("func f(s: string = \"a\") return s end\nOutput.r = [f(), f(5)]", "[\"a\", \"5\"]")>]
[<InlineData("func f(n: number) return n end\nOutput.r = f(\"7\")", "7")>]
[<InlineData("func f(s: string) return s end\nOutput.r = f(s: true)", "\"true\"")>]
let ``JYRO-REQ-1963 HintedParameter_WithoutDefault_IsNotRejected`` (source: string, expected: string) =
    assertRL expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1963")>]
[<InlineData("union U V(s: string) end")>]
[<InlineData("union U V(s: string) end\nOutput.r = V(5)")>]
[<InlineData("union U V(n: number, s: string) end\nOutput.r = V(\"2\", 3)")>]
let ``JYRO-REQ-1963 HintedVariantField_IsNotRejected`` (source: string) =
    // 50.1.3 is judged by the absence of its code and a run that succeeds; the
    // coerced field values are JYRO-REQ-245's (50.10.8), tested in S50_08_SwitchMatch.
    notRejectedWith MessageCode.TypedVariableNeedsInitializer source
    let r = Script.runWith limited source
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1963")>]
let ``JYRO-REQ-1963 HintedAssignment_IsNotRejectedWithThisCode`` () =
    assertOutL """{"a": "b"}""" "Output.a: string = \"b\""
    assertOutL """{"a": "5"}""" "Output.a: string = 5"
    // A hint on an already declared variable is governed by 50.2.1.
    let r = rejectedAt MessageCode.DuplicateTypeHint "Validate" "var x = 1\nx: number = 2"
    Assert.DoesNotContain(MessageCode.TypedVariableNeedsInitializer, Script.codes r)

// ---- 50.1.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1964")>]
[<InlineData("var x = 1\nvar x = 2")>]
[<InlineData("var x\nvar x: number = 1")>]
[<InlineData("var x: any\nvar x")>]
[<InlineData("if true then\n  var x = 1\n  var x = 2\nend")>]
[<InlineData("while false do\n  var x = 1\n  var x = 2\nend")>]
[<InlineData("func f()\n  var x = 1\n  var x = 2\nend")>]
[<InlineData("var x = 1\nOutput.a = x\nvar x = 2")>]
let VarDeclaration_SameNameInSameScope_IsRejectedWithVariableAlreadyDeclared (source: string) =
    rejects MessageCode.VariableAlreadyDeclared source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1964")>]
[<InlineData("var x = 1\nif true then\n  var x = 2\nend")>]
[<InlineData("if true then\n  var x = 1\nend\nif true then\n  var x = 2\nend")>]
[<InlineData("func f()\n  var x = 1\nend\nfunc g()\n  var x = 1\nend")>]
[<InlineData("var x = 1\nvar X = 2")>]
[<InlineData("var x = 1\nfunc f()\n  var x = 2\nend")>]
let VarDeclaration_SameNameInDifferentScopesOrCase_IsAccepted (source: string) =
    notRejectedWith MessageCode.VariableAlreadyDeclared source
    compiles source

// ---- 50.1.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-192")>]
[<InlineData("number", "\"42\"", "42")>]
[<InlineData("number", "\" 7 \"", "7")>]
[<InlineData("number", "\"0.1\"", "0.1")>]
[<InlineData("number", "true", "1")>]
[<InlineData("number", "false", "0")>]
[<InlineData("number", "5", "5")>]
[<InlineData("string", "5", "\"5\"")>]
[<InlineData("string", "true", "\"true\"")>]
[<InlineData("string", "false", "\"false\"")>]
[<InlineData("boolean", "0", "false")>]
[<InlineData("boolean", "2", "true")>]
[<InlineData("boolean", "\"TRUE\"", "true")>]
[<InlineData("boolean", "\"false\"", "false")>]
[<InlineData("boolean", "\"False\"", "false")>]
[<InlineData("null", "null", "null")>]
[<InlineData("any", "\"x\"", "\"x\"")>]
[<InlineData("array", "[1]", "[1]")>]
[<InlineData("object", "{a: 1}", "{\"a\": 1}")>]
let ``JYRO-REQ-192 TypedDeclaration_InitialValue_IsCoercedAsPart30`` (hint: string, value: string, expected: string) =
    assertR expected (sprintf "var v: %s = %s\nOutput.r = v" hint value)

/// A number coerced to `string` gives its string form as `&` gives it (30.12). Compared with
/// `&` rather than with a literal, so this tests that the declaration coerces, not the string
/// form of an exact number (40.4.6, JYRO-REQ-150).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-192")>]
[<InlineData("1.50")>]
[<InlineData("-0.10")>]
[<InlineData("1e21")>]
[<InlineData("Power(2, 0.5)")>]
let ``JYRO-REQ-192 TypedDeclaration_NumberUnderStringHint_IsItsStringForm`` (value: string) =
    let out = outputL (sprintf "var v: string = %s\nOutput.t = TypeOf(v)\nOutput.same = v == (%s & \"\")" value value)
    Assert.Equal("string", Values.asString (Values.prop "t" out))
    Assert.True(Values.asBool (Values.prop "same" out), sprintf "var v: string = %s is not the string form of %s" value value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-192")>]
let ``JYRO-REQ-192 TypedDeclaration_NumberFromString_IsExact`` () =
    let r = rOf "var v: number = \"0.1\"\nOutput.r = v"
    Assert.True(Values.isExact r)
    Assert.Equal(0.1m, Values.asDecimal r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-192")>]
[<InlineData("number", "\"abc\"")>]
[<InlineData("number", "\"\"")>]
[<InlineData("number", "null")>]
[<InlineData("number", "[]")>]
[<InlineData("number", "{}")>]
[<InlineData("string", "null")>]
[<InlineData("string", "[]")>]
[<InlineData("string", "{}")>]
[<InlineData("boolean", "\"yes\"")>]
[<InlineData("boolean", "null")>]
[<InlineData("boolean", "[]")>]
[<InlineData("array", "{}")>]
[<InlineData("array", "null")>]
[<InlineData("array", "\"[]\"")>]
[<InlineData("object", "[]")>]
[<InlineData("object", "null")>]
[<InlineData("function", "1")>]
[<InlineData("function", "null")>]
[<InlineData("null", "1")>]
[<InlineData("null", "\"null\"")>]
let ``JYRO-REQ-192 TypedDeclaration_InitialValueNotCoercible_RaisesInvalidType`` (hint: string, value: string) =
    runtimeError MessageCode.InvalidType (sprintf "var v: %s = %s\nOutput.r = 1" hint value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-192")>]
let ``JYRO-REQ-192 TypedDeclaration_ValueAlreadyOfTheType_IsTheSameValue`` () =
    assertR "[9]" "var src = [1]\nvar a: array = src\na[0] = 9\nOutput.r = src"

// ---- 50.1.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-193")>]
[<InlineData("var n: number = 1\nn = \"5\"\nOutput.r = n", "5")>]
[<InlineData("var n: number = 1\nn = true\nOutput.r = n", "1")>]
[<InlineData("var n: number = 1\nif true then n = \"7\" end\nOutput.r = n", "7")>]
[<InlineData("var s: string = 5\ns = 6\nOutput.r = s", "\"6\"")>]
[<InlineData("var s: string = \"a\"\ns = false\nOutput.r = s", "\"false\"")>]
[<InlineData("var b: boolean = true\nb = 0\nOutput.r = b", "false")>]
[<InlineData("var b: boolean = false\nb = \"TRUE\"\nOutput.r = b", "true")>]
[<InlineData("var n: number = 7\nn &= \"1\"\nOutput.r = n", "71")>]
[<InlineData("var s: string = \"a\"\ns &= 1\nOutput.r = s", "\"a1\"")>]
[<InlineData("var b: boolean = true\nb &= \"\"\nOutput.r = b", "true")>]
[<InlineData("var n: number = 1\nn++\nOutput.r = n", "2")>]
[<InlineData("var n: number = 1\nn--\nOutput.r = n", "0")>]
[<InlineData("var a: any = 1\na = \"x\"\nOutput.r = a", "\"x\"")>]
[<InlineData("var u = 1\nu = \"x\"\nOutput.r = u", "\"x\"")>]
let HintedVariable_LaterAssignment_IsCoerced (source: string, expected: string) =
    assertR expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let HintedVariable_CompoundConcatenation_IsCoercedBackToNumber () =
    assertRStr "number" "var n: number = 7\nn &= \"1\"\nOutput.r = TypeOf(n)"
    assertRStr "number" "var n: number = 1\nn++\nOutput.r = TypeOf(n)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-193")>]
[<InlineData("var n: number = 1\nn = \"x\"")>]
[<InlineData("var n: number = 1\nn = null")>]
[<InlineData("var n: number = 1\nn = []")>]
[<InlineData("var s: string = \"a\"\ns = null")>]
[<InlineData("var s: string = \"a\"\ns = [1]")>]
[<InlineData("var b: boolean = true\nb = \"no\"")>]
[<InlineData("var b: boolean = false\nb &= \"x\"")>]
[<InlineData("var a: array = []\na = {}")>]
[<InlineData("var o: object = {}\no = null")>]
[<InlineData("var n: number = 1\nif true then n = \"x\" end")>]
let HintedVariable_LaterAssignmentNotCoercible_RaisesInvalidType (source: string) =
    runtimeError MessageCode.InvalidType source

// ---- 50.2.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-194")>]
[<InlineData("var x = 1\nx: number = 2")>]
[<InlineData("var x = 1\nif true then x: number = 2 end")>]
[<InlineData("func f(a)\n  a: number = 1\n  return a\nend")>]
[<InlineData("var o = {}\no.a = 1\no.a: number = 2")>]
[<InlineData("var o = {}\no[\"a\"] = 1\no.a: number = 2")>]
[<InlineData("var o = {}\no.a = 1\no[\"a\"]: number = 2")>]
[<InlineData("var o = {}\nif false then o.a = 1 end\no.a: number = 2")>]
[<InlineData("var o = {}\no.a = 1\nif true then o.a: number = 2 end")>]
[<InlineData("Output.a = 0\nOutput.a: number = 1")>]
[<InlineData("Output[\"a\"] = 0\nOutput.a: number = 1")>]
[<InlineData("func g(o)\n  o.a = 1\n  o.a: number = 2\nend")>]
[<InlineData("var o = {}\no.a += 1\no.a: number = 2")>]
let ``JYRO-REQ-194 TypeHint_OnTargetThatAlreadyHasAValue_IsRejectedWithDuplicateTypeHint`` (source: string) =
    // Rejected by validation: before anything runs, every error of the stage Validate.
    rejectedHasAt MessageCode.DuplicateTypeHint "Validate" source |> ignore

/// 50.2.1 and 30.12.10 both apply to a hint on a compound assignment to a target that
/// already has a value: the two codes, and no other.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-194")>]
[<InlineData("var x = 1\nx: number += 1")>]
[<InlineData("var x = 1\nx: number -= 1")>]
[<InlineData("var s = \"\"\ns: string &= \"a\"")>]
[<InlineData("var o = {}\no.a = 1\no.a: number += 2")>]
[<InlineData("var o = {}\no[\"a\"] = 1\no.a: number *= 2")>]
[<InlineData("func f(a)\n  a: number += 1\n  return a\nend")>]
let ``JYRO-REQ-194 JYRO-DEF-639 TypeHint_OnCompoundAssignmentToTargetThatAlreadyHasAValue_GivesDuplicateTypeHintAndTypeHintOnCompoundAssignment`` (source: string) =
    let r = Script.compile source
    let expected = [ MessageCode.DuplicateTypeHint; MessageCode.TypeHintOnCompoundAssignment ] |> List.sort
    Assert.Equal<MessageCode list>(expected, Script.errorCodes r |> List.distinct |> List.sort)
    rejectedHasAt MessageCode.DuplicateTypeHint "Validate" source |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-194")>]
[<InlineData("var o = {}\no.a: number = \"1\"")>]
[<InlineData("Output.a: number = 1")>]
[<InlineData("var o = {}\no.a: number = 2\no.a = 1")>]
[<InlineData("var o = {}\no.a = 1\no.b: number = 2")>]
[<InlineData("var o = {}\no.a = 1\nif true then\n  var o = {}\n  o.a: number = 2\nend")>]
[<InlineData("func f()\n  var o = {}\n  o.a = 1\n  return o\nend\nvar o = {}\no.a: number = 2")>]
[<InlineData("func g(o)\n  o.a = 1\nend\nfunc h(o)\n  o.a: number = 2\nend")>]
[<InlineData("var x = {a: {}}\nx.a.b = 1\nx.a.b: number = 2")>]
[<InlineData("var x = [0]\nvar i = 0\nx[i] = 1\nx[i]: number = 2")>]
[<InlineData("var x = [0]\nx[0] = 1\nx[0]: number = 2")>]
[<InlineData("var p = {}\nvar q = {}\np.a = 1\nq.a: number = 2")>]
let ``JYRO-REQ-194 TypeHint_FirstOnItsTargetOrOtherForm_IsAccepted`` (source: string) =
    notRejectedWith MessageCode.DuplicateTypeHint source
    compiles source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-194")>]
[<InlineData("x: number = 1")>]
[<InlineData("o.a: number = 1")>]
[<InlineData("o[\"a\"]: number = 1")>]
let ``JYRO-REQ-194 TypeHint_OnUndeclaredIdentifierTarget_IsRejectedWithUndeclaredVariableNotDuplicateTypeHint`` (source: string) =
    rejects MessageCode.UndeclaredVariable source
    notRejectedWith MessageCode.DuplicateTypeHint source

// ---- 50.2.2 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-195")>]
let TypeHintOnProperty_AppliesOnlyToThatAssignment () =
    assertOut """{"v": "x", "t1": true}""" "Output.v: number = \"5\"\nOutput.t1 = Output.v is number\nOutput.v = \"x\""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-195")>]
let TypeHintOnProperty_CoercesTheAssignedValue () =
    assertOut """{"w": "5"}""" "Output.w: string = 5"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-195")>]
let TypeHintOnProperty_NotCoercible_RaisesInvalidType () =
    runtimeError MessageCode.InvalidType "Output.z: number = \"q\""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-195")>]
let TypeHintOnElement_AppliesOnlyToThatAssignment () =
    assertOut """{"t": true, "a": ["x"]}""" "var a = [0]\na[0]: number = \"5\"\nOutput.t = a[0] is number\na[0] = \"x\"\nOutput.a = a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-195")>]
let TypeHintOnVariableProperty_AppliesOnlyToThatAssignment () =
    assertOut """{"p": true, "q": 5}""" "var o = {}\no.p: boolean = 1\nOutput.p = o.p\no.p = 5\nOutput.q = o.p"

// ---- 50.2.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1748")>]
[<InlineData("1 + 2")>]
[<InlineData("\"a\" & \"b\"")>]
[<InlineData("Length([1])")>]
[<InlineData("x => x")>]
[<InlineData("Map([1], x => x)")>]
[<InlineData("Output")>]
[<InlineData("Input")>]
[<InlineData("null")>]
[<InlineData("[1, 2]")>]
[<InlineData("true ? 1 : 2")>]
let ExpressionStatement_IsAcceptedAndItsValueDiscarded (source: string) =
    assertOut "{}" source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1748")>]
let ExpressionStatement_CallKeepsItsEffectButNotItsValue () =
    assertOut """{"x": 1}""" "func f(o)\n  o.x = 1\n  return 5\nend\nf(Output)"

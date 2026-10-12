/// 40.10 Lambda expressions, 40.11 Lambda captures, 40.12 Lambdas may not reference Output.
module RealTree.Jyro.Spec.Tests.P40_60.S40_10_Lambdas

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// ---- 40.10.1 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-174")>]
[<InlineData("Output.r = Map([1, 2], x => x * 2)", "[2, 4]")>]
[<InlineData("Output.r = Map([1], (x) => x)", "[1]")>]
[<InlineData("Output.r = Reduce([1, 2], (a, b) => a + b, 0)", "3")>]
[<InlineData("var f = () => 1\nOutput.r = f is function", "true")>]
[<InlineData("Output.r = Map([1, 2], x => x > 1 ? \"big\" : \"small\")", "[\"small\", \"big\"]")>]
let Lambda_ParameterListArrowOneExpression_IsAccepted (source: string, expected: string) =
    assertR expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-174")>]
[<InlineData("Output.r = Map([1], x => do Output.a = 1 end)")>]
[<InlineData("Output.r = Map([1], x => { return x })")>]
[<InlineData("Output.r = Map([1], x => return x)")>]
[<InlineData("Output.r = Map([1], x => var y = 1)")>]
[<InlineData("Output.r = Map([1], x => if x then 1 end)")>]
[<InlineData("Output.r = Map([1], x => while x do end)")>]
[<InlineData("Output.r = Map([1], x =>)")>]
[<InlineData("var f = x =>")>]
[<InlineData("var f = x => x = 1")>]
let Lambda_StatementOrBlockBody_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 40.10.2 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-175")>]
[<InlineData("Output.r = Map([1], (x: number) => x)")>]
[<InlineData("Output.r = Map([1], x: number => x)")>]
[<InlineData("Output.r = Map([1], (x: any) => x)")>]
[<InlineData("Output.r = Map([1], (x: string) => x)")>]
[<InlineData("Output.r = Reduce([1], (a, b: number) => a, 0)")>]
[<InlineData("var f = (x: function) => 1")>]
let Lambda_ParameterWithTypeHint_IsRejectedBeforeExecution (source: string) =
    compileFails source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-175")>]
let Lambda_ParameterWithoutTypeHint_IsAccepted () =
    assertR "[1]" "Output.r = Map([1], (x) => x)"

// ---- 40.10.3 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-176")>]
[<InlineData("Output.r = Map([1], (x = 1) => x)")>]
[<InlineData("Output.r = Reduce([1], (a, b = 0) => a, 0)")>]
[<InlineData("var f = (x = null) => x")>]
[<InlineData("var f = (x: number = 1) => x")>]
let Lambda_ParameterWithDefault_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 40.10.4 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-177")>]
[<InlineData("var f = (a, a) => a")>]
[<InlineData("var f = (x, y, x) => x")>]
[<InlineData("var f = (x, x, x) => 1")>]
[<InlineData("var f = (a, b, b) => 1")>]
[<InlineData("var g = y => ((b, b) => b)")>]
[<InlineData("Output.r = [(a, a) => a]")>]
[<InlineData("func h()\n  var f = (p, p) => p\n  return 1\nend")>]
[<InlineData("if true then\n  var f = (q, q) => 1\nend")>]
let ``JYRO-REQ-177 Lambda_DuplicateParameterName_IsRejectedWithDuplicateParameterName`` (source: string) =
    rejects MessageCode.DuplicateParameterName source

// A lambda has no function name, so its DuplicateParameterName message must read
// without one: no empty quoted name and no "in function" left for it (130.6.2,
// JYRO-ADR-128 point 2; JYRO-DEF-631).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-177")>]
[<InlineData("var f = (x, x) => x", "x")>]
[<InlineData("var g = y => ((b, b) => b)", "b")>]
[<InlineData("func h()\n  var f = (p, p) => p\n  return 1\nend", "p")>]
let ``JYRO-REQ-177 JYRO-DEF-631 Lambda_DuplicateParameterName_MessageReadsWithoutAFunctionName`` (source: string, name: string) =
    let r = Script.compile source
    let ms = r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.DuplicateParameterName) |> List.ofSeq
    if List.isEmpty ms then
        Values.fail (sprintf "Expected DuplicateParameterName. Diagnostics:\n%s\nSource:\n%s" (Script.describe r) source)
    for m in ms do
        let text = m.Message.TrimEnd()
        Assert.Contains(name, text)
        Assert.DoesNotContain("''", text)
        Assert.DoesNotContain("\"\"", text)
        Assert.DoesNotContain("in function", text)
        Assert.False(text.EndsWith " in" || text.EndsWith "function", sprintf "message ends on a dangling word: %s" text)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-177")>]
[<InlineData("var f = (a, b) => a")>]
[<InlineData("var f = (a, A) => a")>]
[<InlineData("var f = (a, b, c) => a")>]
[<InlineData("var g = a => ((b, c) => a)")>]
let ``JYRO-REQ-177 Lambda_DistinctParameterNames_AreAccepted`` (source: string) =
    notRejectedWith MessageCode.DuplicateParameterName source
    compiles source

// ---- 40.10.5 -------------------------------------------------------------

let reservedKeywords =
    [ "and"; "array"; "boolean"; "break"; "by"; "case"; "continue"; "default"; "delete"; "do"; "downto"
      "else"; "elseif"; "end"; "exit"; "fail"; "false"; "for"; "foreach"; "func"; "if"; "in"; "is"
      "match"; "not"; "null"; "number"; "object"; "or"; "return"; "string"; "switch"; "then"; "to"
      "true"; "union"; "var"; "while" ]

let keywordParamCases : seq<obj[]> =
    seq {
        for kw in reservedKeywords do
            yield [| box kw; box "single" |]
            yield [| box kw; box "list" |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-178")>]
[<MemberData(nameof keywordParamCases)>]
let Lambda_ReservedKeywordParameter_IsRejected (keyword: string, form: string) =
    let src =
        if form = "single" then sprintf "var f = %s => 1" keyword
        else sprintf "var f = (a, %s) => 1" keyword
    compileFails src

[<Theory>]
[<Trait("requirement", "JYRO-REQ-178")>]
[<InlineData("Output.r = Map([2], any => any + 1)", "[3]")>]
[<InlineData("Output.r = Map([2], function => function * 2)", "[4]")>]
[<InlineData("Output.r = Reduce([2], (any, function) => any + function, 1)", "3")>]
let Lambda_ContextualKeywordParameter_IsAccepted (source: string, expected: string) =
    assertR expected source

// ---- 40.10.6 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-179")>]
[<InlineData("Output.r = Map([1], Input => 1)")>]
[<InlineData("Output.r = Map([1], Output => 1)")>]
[<InlineData("Output.r = Map([1], (x, Output) => x)")>]
[<InlineData("Output.r = Reduce([1], (Input, x) => x, 0)")>]
[<InlineData("var f = (Input) => 1")>]
[<InlineData("func g()\n  return Map([1], Input => 1)\nend")>]
[<InlineData("func g()\n  return Map([1], (Output) => 1)\nend")>]
[<InlineData("Output.r = Map([1], x => Map([1], Output => 1))")>]
[<InlineData("Output.r = Map([1], x => Map([1], Input => x))")>]
let Lambda_ParameterNamedInputOrOutput_IsRejectedWithReservedName (source: string) =
    rejects MessageCode.ReservedName source

// ---- 40.10.7 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1958")>]
[<InlineData("var v = x => x")>]
[<InlineData("var v = null\nv = x => x")>]
[<InlineData("var c = [x => x]\nvar v = c[0]")>]
[<InlineData("var d = {f: x => x}\nvar v = d.f")>]
[<InlineData("func mk()\n  return x => x\nend\nvar v = mk()")>]
[<InlineData("func id(p)\n  return p\nend\nvar v = id(x => x)")>]
[<InlineData("var v = true ? x => x : null")>]
[<InlineData("var v = false ? null : x => x")>]
[<InlineData("var v = null ?? x => x")>]
[<InlineData("var v = (x => x)")>]
[<InlineData("union U A end\nvar v = match A() do case A then x => x end")>]
[<InlineData("var v = First([x => x])")>]
let Lambda_InEveryExpressionPosition_GivesAFunctionValue (setup: string) =
    assertRStr "function" (setup + "\nOutput.r = TypeOf(v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1958")>]
let Lambda_PassedThroughMap_IsUsable () =
    assertR "[2]" "Output.r = Map([1], (Map([x => x + 1], f => f))[0])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1958")>]
let Lambda_AsExpressionStatement_IsAccepted () =
    assertOut "{}" "x => x"

// ---- 40.11.1 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1959")>]
[<InlineData("var k = 10\nOutput.r = Map([1, 2], x => x + k)", "[11, 12]")>]
[<InlineData("var k = 1\nif true then\n  var k = 100\n  Output.r = Map([1], x => x + k)\nend", "[101]")>]
[<InlineData("func f(p)\n  return Map([1], x => x + p)\nend\nOutput.r = f(5)", "[6]")>]
[<InlineData("func f(p)\n  var q = p * 2\n  return Map([1], x => x + q)\nend\nOutput.r = f(5)", "[11]")>]
[<InlineData("var k = 1\nvar m = 2\nOutput.r = Reduce([10], (a, b) => a + b + k + m, 0)", "13")>]
[<InlineData("var k = 3\nOutput.r = Map([1], x => Map([1], y => x + y + k))", "[[5]]")>]
let Lambda_CapturesNonParameterNamesFromConstructionScope (source: string, expected: string) =
    assertR expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1959")>]
let Lambda_BodyReferencingUndeclaredName_IsRejectedWithUndeclaredVariable () =
    rejects MessageCode.UndeclaredVariable "Output.r = Map([1], x => x + zz)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1959")>]
let Lambda_CaptureResolvesToInnermostBindingAtConstruction () =
    assertOut """{"a": [101], "b": [2]}"""
        "var k = 1\nif true then\n  var k = 100\n  Output.a = Map([1], x => x + k)\nend\nOutput.b = Map([1], x => x + k)"

// ---- 40.11.2 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-182")>]
[<InlineData("var k = 1\nvar f = x => x + k\nk = 5\nOutput.r = Map([1], f)", "[6]")>]
[<InlineData("var a = [1]\nvar f = x => a[0]\na[0] = 9\nOutput.r = Map([0], f)", "[9]")>]
[<InlineData("var a = [1]\nvar f = x => a[0]\na = [7]\nOutput.r = Map([0], f)", "[7]")>]
let Lambda_CapturesTheVariableNotACopy (source: string, expected: string) =
    assertR expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-182")>]
let Lambda_ChangeBetweenCalls_IsVisible () =
    assertOut """{"a": [1], "b": [2]}""" "var k = 1\nvar f = x => k\nOutput.a = Map([0], f)\nk = 2\nOutput.b = Map([0], f)"

// ---- 40.11.3 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1960")>]
[<InlineData("func apply(g)\n  var k = 100\n  return Map([1], g)\nend\nvar k = 2\nvar f = x => x + k\nOutput.r = apply(f)", "[3]")>]
[<InlineData("func make(n)\n  return x => x + n\nend\nOutput.r = Map([1], make(5))", "[6]")>]
[<InlineData("func make(n)\n  return x => x + n\nend\nvar n = 100\nvar g = make(5)\nOutput.r = Map([1], g)", "[6]")>]
[<InlineData("func use(o)\n  var n = 0\n  return Map([1], o.f)\nend\nvar n = 7\nvar h = {f: x => x + n}\nOutput.r = use(h)", "[8]")>]
[<InlineData("func use(a)\n  var n = 0\n  return Map([1], a[0])\nend\nvar n = 7\nOutput.r = use([x => x + n])", "[8]")>]
[<InlineData("func apply(g)\n  var n = 50\n  return Map([1], x => Reduce([x], g, n))\nend\nvar n = 3\nOutput.r = apply((a, b) => a + b + n)", "[54]")>]
let Lambda_PassedReturnedOrStored_KeepsItsConstructionCaptures (source: string, expected: string) =
    assertR expected source

// ---- 40.11.4 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1961")>]
[<InlineData("var k = 1\nfunc f()\n  return Map([1], x => x + k)\nend\nOutput.r = f()")>]
[<InlineData("func f()\n  return Map([1], x => x + k)\nend\nvar k = 1\nOutput.r = f()")>]
[<InlineData("func g()\n  var loc = 1\n  return h()\nend\nfunc h()\n  return Map([1], x => loc)\nend")>]
let Lambda_InFunctionBody_DoesNotSeeOuterNames (source: string) =
    rejects MessageCode.UndeclaredVariable source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1961")>]
let Lambda_InFunctionBody_SeesParametersLocalsAndInput () =
    let out = Script.output (Script.runJson """{"n": 100}""" "func f(p)\n  var q = 2\n  return Map([1], x => x + p + q + Input.n)\nend\nOutput.r = f(10)")
    Assert.Equal(norm "[113]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1961")>]
let Lambda_InFunctionBody_CallsDeclaredFunctionsAndVariantsByName () =
    // 60.4.1: a function body sees every function and variant as a call target.
    assertR """[[2, 5]]"""
        "func g() return 2 end\nunion U V(x) end\nfunc f()\n  return Map([5], x => [g(), match V(x) do case V(y) then y end])\nend\nOutput.r = f()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1961")>]
let Lambda_AtTopLevel_SeesTopLevelScope () =
    let out = Script.output (Script.runJson """{"n": 100}""" "var t = 5\nOutput.r = Map([1], x => x + t + Input.n)")
    Assert.Equal(norm "[106]", Values.toJson (Values.prop "r" out))

// ---- 40.11.5 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-185")>]
[<InlineData("var f = null\nif true then\n  var k = 3\n  f = x => x * k\nend\nOutput.r = Map([2], f)", "[6]")>]
[<InlineData("func make(n)\n  var m = n * 2\n  return x => x + m\nend\nvar g = make(5)\nOutput.r = Map([1], g)", "[11]")>]
[<InlineData("var f = null\nforeach v in [4] do\n  f = x => x + v\nend\nOutput.r = Map([1], f)", "[5]")>]
[<InlineData("var f = null\nfor i in 3 to 4 do\n  var w = i * 10\n  f = x => x + w\nend\nOutput.r = Map([1], f)", "[31]")>]
let Lambda_CapturedVariable_OutlivesItsScope (source: string, expected: string) =
    assertR expected source

// ---- 40.11.6 -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_WithNameOfVisibleVariable_IsAcceptedWithNoDiagnostic () =
    compilesClean "var o = 5\nOutput.r = Map([3], o => o + 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_WithNameOfVisibleVariable_RefersToTheParameter () =
    assertR "[4]" "var o = 5\nOutput.r = Map([3], o => o + 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_OuterVariableOfSameName_NotCapturedAndUnchanged () =
    assertOut """{"r": [3], "o": 5}""" "var o = 5\nOutput.r = Map([3], o => o)\nOutput.o = o"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_ScopeLiesInsideConstructingScope_OtherNamesResolve () =
    assertR "[11, 12]" "var k = 10\nvar o = 5\nOutput.r = Map([1, 2], o => o + k)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_WithNameOfFunctionParameter_RefersToTheLambdaParameter () =
    assertR "[2]" "func f(o)\n  return Map([1], o => o * 2)\nend\nOutput.r = f(9)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_SameNameInTwoLambdas_InOneScope () =
    assertOut """{"a": [2], "b": [2]}""" "var o = 1\nOutput.a = Map([1], o => o + 1)\nOutput.b = Where([1, 2], o => o > 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-186")>]
let LambdaParameter_WithNameOfVisibleVariable_NoVariableAlreadyDeclared () =
    notRejectedWith MessageCode.VariableAlreadyDeclared "var o = 5\nvar f = (k, o) => o\nOutput.r = Reduce([1], f, 0)"
    assertR "1" "var o = 5\nvar f = (k, o) => o\nOutput.r = Reduce([1], f, 0)"

// ---- 40.12.1 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-187")>]
[<InlineData("Output.r = Map([1], x => Output)")>]
[<InlineData("Output.r = Map([1], x => Output.x)")>]
[<InlineData("Output.r = Map([1], x => Output[\"x\"])")>]
[<InlineData("Output.r = Map([1], x => Map([1], y => Output.a))")>]
[<InlineData("func f()\n  return Map([1], x => Output.a)\nend")>]
[<InlineData("var g = x => Output")>]
[<InlineData("var g = x => Length(Output)")>]
[<InlineData("var g = x => x ?? Output.a")>]
let Lambda_ReferenceToOutput_IsRejectedWithOutputAccessInFunction (source: string) =
    rejects MessageCode.OutputAccessInFunction source

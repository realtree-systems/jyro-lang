/// 60 Scope: block scoping, reserved names, declaration before use, function bodies.
module RealTree.Jyro.Spec.Tests.P40_60.S60_Scope

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

/// Every block kind of 60.1.1 other than a function body, as a template in
/// which BODY runs exactly once. The union for the match case is declared by
/// `blockPrelude`.
let blockKinds =
    [ "if", "if true then\nBODY\nend"
      "elseif", "if false then\nelseif true then\nBODY\nend"
      "else", "if false then\nelse\nBODY\nend"
      "while", "var w = true\nwhile w do\nw = false\nBODY\nend"
      "for", "for q in 0 to 1 do\nBODY\nend"
      "foreach", "foreach q in [1] do\nBODY\nend"
      "case", "switch 1 do\ncase 1 then\nBODY\nend"
      "default", "switch 1 do\ndefault then\nBODY\nend"
      "match", "match A1() do\ncase A1 then\nBODY\nend" ]

let blockPrelude = "union U1 A1 end\n"

let blockKindCases : seq<obj[]> = blockKinds |> Seq.map (fun (k, _) -> [| box k |])

let private block (kind: string) (body: string) =
    let t = blockKinds |> List.find (fun (k, _) -> k = kind) |> snd
    t.Replace("BODY", body)

// ---- 60.1.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-259")>]
[<MemberData(nameof blockKindCases)>]
let Block_StartsANewScope_InnerDeclarationDoesNotClashWithOuter (kind: string) =
    let src = blockPrelude + "var x = 1\n" + block kind "var x = 2\nOutput.i = x" + "\nOutput.r = x"
    notRejectedWith MessageCode.VariableAlreadyDeclared src
    assertOut """{"i": 2, "r": 1}""" src

[<Theory>]
[<Trait("requirement", "JYRO-REQ-259")>]
[<MemberData(nameof blockKindCases)>]
let Block_StartsANewScope_DeclarationNotVisibleAfterIt (kind: string) =
    rejects MessageCode.UndeclaredVariable (blockPrelude + block kind "var y = 1" + "\nOutput.r = y")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-259")>]
[<InlineData("if false then\n  var z = 1\nelseif false then\n  var z = 2\nelse\n  var z = 3\n  Output.z = z\nend", "{\"z\": 3}")>]
[<InlineData("switch 2 do\n  case 1 then\n    var z = 1\n  case 2 then\n    var z = 2\n    Output.z = z\n  default then\n    var z = 3\nend", "{\"z\": 2}")>]
[<InlineData("union U2 B1 B2 end\nmatch B2() do\n  case B1 then\n    var z = 1\n  case B2 then\n    var z = 2\n    Output.z = z\nend", "{\"z\": 2}")>]
let Block_SiblingBlocks_AreSeparateScopes (source: string, expected: string) =
    notRejectedWith MessageCode.VariableAlreadyDeclared source
    assertOut expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-259")>]
let FunctionBody_StartsANewScope () =
    let src = "var x = 1\nfunc f()\n  var x = 2\n  return x\nend\nOutput.r = f()\nOutput.x = x"
    notRejectedWith MessageCode.VariableAlreadyDeclared src
    assertOut """{"r": 2, "x": 1}""" src

// ---- 60.1.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-260")>]
[<MemberData(nameof blockKindCases)>]
let Name_VisibleInItsScopeAndNestedScopes (kind: string) =
    assertOut """{"n": 7, "m": 7}"""
        (blockPrelude + block kind "var y = 7\nif true then\n  if true then Output.n = y end\nend\nOutput.m = y")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-260")>]
[<MemberData(nameof blockKindCases)>]
let Name_NotVisibleOutsideItsScope (kind: string) =
    rejects MessageCode.UndeclaredVariable (blockPrelude + block kind "if true then\n  var y = 1\nend\nOutput.r = y")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-260")>]
[<InlineData("if true then\n  var x = 1\nelse\n  Output.r = x\nend")>]
[<InlineData("switch 1 do\n  case 1 then\n    var x = 1\n  default then\n    Output.r = x\nend")>]
[<InlineData("func f()\n  var y = 1\n  return y\nend\nOutput.r = y")>]
[<InlineData("func f()\n  var y = 1\nend\nfunc g()\n  return y\nend")>]
let Name_NotVisibleInSiblingOrOutsideScope (source: string) =
    rejects MessageCode.UndeclaredVariable source

// ---- 60.1.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1828")>]
[<MemberData(nameof blockKindCases)>]
let ``JYRO-REQ-1828 InnerDeclaration_ShadowsOuter_InnerNameRefersToInnerVariable`` (kind: string) =
    assertOut """{"r": 2, "s": 5, "t": 1}"""
        (blockPrelude + "var x = 1\n" + block kind "var x = 2\nOutput.r = x\nx = 5\nOutput.s = x" + "\nOutput.t = x")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1828")>]
[<MemberData(nameof blockKindCases)>]
let ``JYRO-REQ-1828 InnerDeclaration_ReferenceBeforeItsEnd_RefersToTheOuterVariable`` (kind: string) =
    assertOut """{"a": 1, "b": 5, "t": 1}"""
        (blockPrelude + "var x = 1\n" + block kind "Output.a = x\nvar x = 5\nOutput.b = x" + "\nOutput.t = x")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_TheStatementExamples_Hold`` () =
    assertOut """{"r": 1}""" "var x = 1\nif true then\n  var x = 2\n  x = 3\nend\nOutput.r = x"
    assertOut """{"a": 1, "b": 5}""" "var x = 1\nif true then\n  Output.a = x\n  var x = 5\n  Output.b = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_TheInitializerIsBeforeItsEnd_SoItReadsTheOuterVariable`` () =
    assertOut """{"r": 11, "t": 10}""" "var x = 10\nif true then\n  var x = x + 1\n  Output.r = x\nend\nOutput.t = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_CompoundAssignmentAndIncrementUseTheInnerVariable`` () =
    assertOut """{"r": 7, "s": 3, "t": 1}"""
        "var x = 1\nif true then\n  var x = 2\n  x += 5\n  Output.r = x\n  x = 2\n  x++\n  Output.s = x\nend\nOutput.t = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_AssignmentInANestedScopeThroughTheInnerNameLeavesTheOuterVariable`` () =
    assertOut """{"r": 9, "t": 1}"""
        "var x = 1\nif true then\n  var x = 2\n  if true then\n    x = 9\n  end\n  Output.r = x\nend\nOutput.t = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_ThreeDeep`` () =
    assertOut """{"c": 3, "b": 2, "a": 1}"""
        "var x = 1\nif true then\n  var x = 2\n  if true then\n    var x = 3\n    Output.c = x\n  end\n  Output.b = x\nend\nOutput.a = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_LambdaCapturesTheInnerVariable`` () =
    assertOut """{"r": [2], "t": [1]}"""
        "var x = 1\nif true then\n  var x = 2\n  Output.r = Map([0], y => x)\nend\nOutput.t = Map([0], y => x)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_LambdaBeforeTheInnerDeclarationCapturesTheOuterVariable`` () =
    assertOut """{"r": [1], "s": [2]}"""
        "var x = 1\nif true then\n  var f = y => x\n  Output.r = Map([0], f)\n  var x = 2\n  Output.s = Map([0], y => x)\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_WithDifferentTypeHint`` () =
    assertOut """{"r": "a", "t": 1}""" "var x = 1\nif true then\n  var x: string = \"a\"\n  Output.r = x\nend\nOutput.t = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1828")>]
let ``JYRO-REQ-1828 InnerDeclaration_InFunctionBody_ShadowsTopLevelName`` () =
    assertOut """{"r": 5, "t": 1}""" "var x = 1\nfunc f()\n  var x = 2\n  x = 5\n  return x\nend\nOutput.r = f()\nOutput.t = x"

// ---- 60.1.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-262")>]
[<InlineData("for i in 0 to 2 do\n  var k = i\n  fs = Append(fs, x => x + k)\nend", "[0]", "[1]")>]
[<InlineData("for i in 0 to 2 do\n  fs = Append(fs, x => x + i)\nend", "[0]", "[1]")>]
[<InlineData("for i in 2 downto 0 do\n  fs = Append(fs, x => x + i)\nend", "[2]", "[1]")>]
[<InlineData("foreach i in [1, 2] do\n  fs = Append(fs, x => x + i)\nend", "[1]", "[2]")>]
[<InlineData("foreach i in [1, 2] do\n  var k = i * 10\n  fs = Append(fs, x => k)\nend", "[10]", "[20]")>]
[<InlineData("var n = 0\nwhile n < 2 do\n  var k = n\n  fs = Append(fs, x => k)\n  n += 1\nend", "[0]", "[1]")>]
[<InlineData("for i in 0 to 2 do\n  var k = i\n  fs = Append(fs, x => k)\n  k = k + 10\nend", "[10]", "[11]")>]
let ``JYRO-REQ-262 LoopIteration_NewScopePerIteration_LambdaCapturesThatIterationsVariables`` (loop: string, first: string, second: string) =
    let out = Script.outputOf ("var fs = []\n" + loop + "\nOutput.a = Map([0], fs[0])\nOutput.b = Map([0], fs[1])")
    Assert.Equal(norm first, Values.toJson (Values.prop "a" out))
    Assert.Equal(norm second, Values.toJson (Values.prop "b" out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-262")>]
[<InlineData("for i in 0 to 2 do\n  var k\n  if i == 0 then k = 5 end\n  l = Append(l, k)\nend")>]
[<InlineData("foreach i in [0, 1] do\n  var k\n  if i == 0 then k = 5 end\n  l = Append(l, k)\nend")>]
[<InlineData("var i = 0\nwhile i < 2 do\n  var k\n  if i == 0 then k = 5 end\n  l = Append(l, k)\n  i++\nend")>]
let ``JYRO-REQ-262 LoopIteration_DeclarationIsANewVariableEachIteration`` (loop: string) =
    assertR "[5, null]" ("var l = []\n" + loop + "\nOutput.r = l")

/// The requirement's own example, as written.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-262")>]
let ``JYRO-REQ-262 the requirement's example gives each lambda its own iteration's k`` () =
    let out =
        Script.outputOf
            "var fs = []\nfor i in 0 to 2 do var k = i  fs = Append(fs, x => x + k) end\nOutput.a = Map([0], fs[0])\nOutput.b = Map([0], fs[1])"
    Assert.Equal(norm "[0]", Values.toJson (Values.prop "a" out))
    Assert.Equal(norm "[1]", Values.toJson (Values.prop "b" out))

/// A change to one iteration's variable after its lambda is built is seen by
/// that lambda only: each iteration's lambda captures a different variable.
/// (The loop variable itself is not assigned here: what that does to the next
/// value is 50.5's rule, not this requirement's.)
[<Theory>]
[<Trait("requirement", "JYRO-REQ-262")>]
[<InlineData("for i in 0 to 2 do\n  var k = i\n  fs = Append(fs, x => k)\n  k = k + 100\nend")>]
[<InlineData("foreach i in [0, 1] do\n  var k = i\n  fs = Append(fs, x => k)\n  k = k + 100\nend")>]
[<InlineData("var n = 0\nwhile n < 2 do\n  var k = n\n  fs = Append(fs, x => k)\n  k = k + 100\n  n++\nend")>]
let ``JYRO-REQ-262 a lambda sees later changes to its own iteration's variable only`` (loop: string) =
    let out = Script.outputOf ("var fs = []\n" + loop + "\nOutput.a = Map([0], fs[0])\nOutput.b = Map([0], fs[1])")
    Assert.Equal(norm "[100]", Values.toJson (Values.prop "a" out))
    Assert.Equal(norm "[101]", Values.toJson (Values.prop "b" out))

// ---- 60.1.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-263")>]
[<MemberData(nameof blockKindCases)>]
let ``JYRO-REQ-263 DeclarationScope_BeginsAfterItsInitialValue`` (kind: string) =
    assertOut """{"a": 1, "r": 2}"""
        (blockPrelude + "var x = 1\n" + block kind "Output.a = x\nvar x = x + 1\nOutput.r = x")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-263")>]
let ``JYRO-REQ-263 DeclarationScope_RequirementExamples`` () =
    assertR "2" "var x = 1\nif true then\n  var x = x + 1\n  Output.r = x\nend"
    assertOut """{"a": 1, "b": 5}""" "var x = 1\nif true then\n  Output.a = x\n  var x = 5\n  Output.b = x\nend"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-263")>]
[<InlineData("var y = y")>]
[<InlineData("if true then\n  var y = y\nend")>]
[<InlineData("if true then\n  Output.a = y\n  var y = 1\nend")>]
[<InlineData("var y = [y]")>]
[<InlineData("func f()\n  var y = y + 1\nend")>]
let ``JYRO-REQ-263 DeclarationScope_NoOuterVariable_IsUndeclaredVariable`` (source: string) =
    rejects MessageCode.UndeclaredVariable source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-263")>]
let ``JYRO-REQ-263 DeclarationScope_InitialValueRefersToOuterParameter`` () =
    assertR "6" "func f(n)\n  if true then\n    var n = n * 2\n    return n\n  end\nend\nOutput.r = f(3)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-263")>]
let ``JYRO-REQ-263 DeclarationScope_LambdaInInitialValueRefersToOuterVariable`` () =
    assertR "[2]" "var x = 1\nif true then\n  var x = Map([1], v => v + x)\n  Output.r = x\nend"

/// JYRO-DEF-498 and JYRO-DEF-310 are one fault against 60.1.5: the initial
/// value of an inner `var x = x + 1` read the new, unassigned x instead of the
/// outer one. DEF-498 records the if, elseif and else blocks, DEF-310 the
/// loops, switch arms, match arms, a parameter and a lambda in the value.
let def498Kinds : seq<obj[]> = [ "if"; "elseif"; "else" ] |> Seq.map (fun k -> [| box k |])

let def310Kinds : seq<obj[]> =
    [ "while"; "for"; "foreach"; "case"; "default"; "match" ] |> Seq.map (fun k -> [| box k |])

let private redeclaresOuter (kind: string) =
    blockPrelude + "var x = 1\n" + block kind "var x = x + 1\nOutput.r = x" + "\nOutput.t = x"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-263")>]
[<MemberData(nameof def498Kinds)>]
let ``JYRO-REQ-263 JYRO-DEF-498 var x = x + 1 in an if, elseif or else block builds on the outer x`` (kind: string) =
    assertOut """{"r": 2, "t": 1}""" (redeclaresOuter kind)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-263")>]
let ``JYRO-REQ-263 JYRO-DEF-498 the requirement's own example gives Output.r the value 2`` () =
    assertR "2" "var x = 1\nif true then\n  var x = x + 1\n  Output.r = x\nend"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-263")>]
[<MemberData(nameof def310Kinds)>]
let ``JYRO-REQ-263 JYRO-DEF-310 var x = x + 1 in a loop, switch arm or match arm builds on the outer x`` (kind: string) =
    assertOut """{"r": 2, "t": 1}""" (redeclaresOuter kind)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-263")>]
let ``JYRO-REQ-263 JYRO-DEF-310 an inner var n = n * 2 in a function reads the outer parameter n`` () =
    assertR "6" "func f(n)\n  if true then\n    var n = n * 2\n    return n\n  end\nend\nOutput.r = f(3)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-263")>]
let ``JYRO-REQ-263 JYRO-DEF-310 a lambda in the initial value captures the outer x`` () =
    assertR "[2]" "var x = 1\nif true then\n  var x = Map([1], v => v + x)\n  Output.r = x\nend"

// ---- 60.2.1 --------------------------------------------------------------

let bindingPlaces =
    [ "var NAME = 1"
      "func NAME()\nend"
      "func f(NAME)\nend"
      "var g = NAME => 1"
      "var g = (a, NAME) => 1"
      "for NAME in 0 to 1 do end"
      "foreach NAME in [] do end"
      "union U A(x) end\nvar v = A(1)\nmatch v do\n  case A(NAME) then Output.r = 1\nend"
      "union U NAME end" ]

let reservedNameCases : seq<obj[]> =
    seq {
        for name in [ "Input"; "Output" ] do
            for place in bindingPlaces do
                yield [| box name; box place |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-264")>]
[<MemberData(nameof reservedNameCases)>]
let ``JYRO-REQ-264 Binding_InputOrOutput_IsRejectedWithReservedName`` (name: string, place: string) =
    rejects MessageCode.ReservedName (place.Replace("NAME", name))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-264")>]
[<InlineData("var input = 1\nvar output = 2\nOutput.r = input + output")>]
[<InlineData("foreach input in [1] do Output.r = input end")>]
let ``JYRO-REQ-264 Binding_LowerCaseInputOrOutput_IsAnOrdinaryName`` (source: string) =
    notRejectedWith MessageCode.ReservedName source
    compiles source

// ---- 60.3.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-265")>]
[<InlineData("Output.r = y")>]
[<InlineData("Output.r = x\nvar x = 1")>]
[<InlineData("z = 1")>]
[<InlineData("z += 1")>]
[<InlineData("z++")>]
[<InlineData("if true then\n  var q = 1\nend\nOutput.r = q")>]
[<InlineData("Output.r = q\nif true then\n  var q = 1\nend")>]
[<InlineData("func f()\n  return t\nend\nvar t = 1")>]
[<InlineData("Output.r = [1, y]")>]
[<InlineData("delete y.a")>]
let Reference_ToUndeclaredName_IsRejectedWithUndeclaredVariable (source: string) =
    rejects MessageCode.UndeclaredVariable source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-265")>]
[<InlineData("Output.r = Input")>]
[<InlineData("Output.r = Input.a\nOutput = {}")>]
[<InlineData("var a = 1\nif true then\n  Output.r = a\nend")>]
[<InlineData("var a = 1\nOutput.r = a")>]
let Reference_InputOutputOrDeclaredEarlier_IsAccepted (source: string) =
    notRejectedWith MessageCode.UndeclaredVariable source
    compiles source

// ---- 60.3.2 --------------------------------------------------------------

let private fnAndVariants = "func F()\n  return 1\nend\nunion U V(x) W end\n"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1832")>]
[<InlineData("var a = F")>]
[<InlineData("Output.r = F")>]
[<InlineData("Output.r = Map([1], F)")>]
[<InlineData("Output.r = V")>]
[<InlineData("Output.r = W")>]
[<InlineData("var b = [F]")>]
[<InlineData("var b = {f: F}")>]
[<InlineData("Output.r = F is function")>]
[<InlineData("Output.r = F ?? 1")>]
[<InlineData("var c = Length")>]
[<InlineData("Output.r = Map([1], Length)")>]
let ``JYRO-REQ-1832 FunctionOrVariantName_UsedOtherThanAsCallName_IsRejectedWithUndeclaredVariable`` (stmt: string) =
    rejects MessageCode.UndeclaredVariable (fnAndVariants + stmt)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1832")>]
let ``JYRO-REQ-1832 FunctionOrVariantName_AsCallName_IsAccepted`` () =
    assertOut """{"f": 1, "v": {"_variant": "V", "x": 1}, "w": {"_variant": "W"}}"""
        (fnAndVariants + "Output.f = F()\nOutput.v = V(1)\nOutput.w = W()")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1832")>]
let ``JYRO-REQ-1832 FunctionName_WithoutACall_IsRejectedWithUndeclaredVariable`` () =
    rejects MessageCode.UndeclaredVariable "func f() return 1 end\nOutput.x = f"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1832")>]
let ``JYRO-REQ-1832 VariantName_OfAMatchCase_AndAsCallName_AreNotRejected`` () =
    notRejectedWith MessageCode.UndeclaredVariable "union U A end\nvar u = A()\nOutput.m = match u do case A then 1 end"
    assertOut """{"m": 1}""" "union U A end\nvar u = A()\nOutput.m = match u do case A then 1 end"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1832")>]
let ``JYRO-REQ-1832 VariantName_OfAMatchStatementCase_IsNotRejected`` () =
    notRejectedWith MessageCode.UndeclaredVariable "union U A end\nvar u = A()\nmatch u do\ncase A then\nOutput.m = 1\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1832")>]
let ``JYRO-REQ-1832 NamedArgumentLabel_EqualToAFunctionName_IsNotRejected`` () =
    assertOut """{"r": 5}""" "func f(g) return g end\nfunc g() return 1 end\nOutput.r = f(g: 5)"
    assertOut """{"r": 5}""" "func f() return 1 end\nfunc h(f) return f end\nOutput.r = h(f: 5)"

// ---- 60.4.1 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-267")>]
let FunctionBody_SeesParametersAndInput () =
    assertOutWith """{"n": 3}""" """{"r": [1, 2, 3]}""" "func f(p, q)\n  return [p, q, Input.n]\nend\nOutput.r = f(1, 2)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-267")>]
[<InlineData("func a()\n  return b()\nend\nfunc b()\n  return 7\nend\nOutput.r = a()", "7")>]
[<InlineData("func fact(n)\n  if n <= 1 then return 1 end\n  return n * fact(n - 1)\nend\nOutput.r = fact(5)", "120")>]
[<InlineData("union U V(x) end\nfunc g()\n  return V(1)\nend\nOutput.r = g()", "{\"_variant\": \"V\", \"x\": 1}")>]
[<InlineData("func g()\n  return V(1)\nend\nunion U V(x) end\nOutput.r = g()", "{\"_variant\": \"V\", \"x\": 1}")>]
[<InlineData("func h()\n  return Length([1, 2])\nend\nOutput.r = h()", "2")>]
let FunctionBody_SeesFunctionsAndVariantsAsCallTargets (source: string, expected: string) =
    assertR expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-267")>]
let FunctionBody_SeesHostFunctionAsCallTarget () =
    let twice =
        Host.func "Twice" [ Parameter.Required("n", ParameterType.Number) ] ParameterType.Number FunctionEffect.Pure
            (fun args -> Values.int (2 * int (Values.asFloat args.Head)))
    let r = Script.runWith { Script.defaults with Functions = [ twice.AsFunction ] } "func k()\n  return Twice(21)\nend\nOutput.r = k()"
    Assert.Equal(42.0, Values.asFloat (Values.prop "r" (Script.output r)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-267")>]
let FunctionBody_SeesLibraryFunctionAsCallTarget () =
    let r = Script.runWith { Script.defaults with Libraries = [ "func Lib()\n  return 9\nend" ] } "func k()\n  return Lib()\nend\nOutput.r = k()"
    Assert.Equal(9.0, Values.asFloat (Values.prop "r" (Script.output r)))

// ---- 60.4.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1835")>]
[<InlineData("var t = 1\nfunc f()\n  return t\nend")>]
[<InlineData("func f()\n  return t\nend\nvar t = 1")>]
[<InlineData("func g()\n  var loc = 1\n  return h()\nend\nfunc h()\n  return loc\nend")>]
[<InlineData("func f()\n  return i\nend\nfor i in 0 to 2 do Output.r = f() end")>]
[<InlineData("var t = 1\nfunc f()\n  t = 2\nend")>]
[<InlineData("var t = [1]\nfunc f()\n  t[0] = 2\nend")>]
[<InlineData("if true then\n  var t = 1\nend\nfunc f()\n  return t\nend")>]
let ``JYRO-REQ-1835 FunctionBody_DoesNotSeeOuterVariables`` (source: string) =
    rejects MessageCode.UndeclaredVariable source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1835")>]
[<InlineData("var t = 1\nfunc f()\n  t = 2\n  return 0\nend")>]
[<InlineData("var t = 1\nfunc f()\n  t += 2\n  return 0\nend")>]
[<InlineData("var t = 1\nfunc f()\n  t++\n  return 0\nend")>]
[<InlineData("if true then\n  var t = 1\nend\nfunc f()\n  t = 2\n  return 0\nend")>]
[<InlineData("for i in 0 to 1 do\n  var t = i\nend\nfunc f()\n  return t\nend")>]
[<InlineData("var t = 1\nfunc f()\n  if true then\n    return t\n  end\n  return 0\nend")>]
[<InlineData("var t = 1\nfunc f()\n  return Map([1], x => t)\nend")>]
let ``JYRO-REQ-1835 FunctionBody_AssignmentOrNestedReadOfAnOuterVariable_IsUndeclaredVariable`` (source: string) =
    rejects MessageCode.UndeclaredVariable source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1835")>]
let ``JYRO-REQ-1835 FunctionBody_AParameterOfTheSameNameIsVisible`` () =
    assertOut """{"r": 2}""" "var t = 1\nfunc f(t) return t end\nOutput.r = f(2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1835")>]
let ``JYRO-REQ-1835 FunctionBody_ALocalDeclarationOfTheSameNameIsVisibleAfterIt`` () =
    assertOut """{"r": 5}""" "var t = 1\nfunc f()\n  var t = 5\n  return t\nend\nOutput.r = f()"

// ---- 60.4.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-269")>]
[<InlineData("func f(a)\n  var a = 1\n  return a\nend")>]
[<InlineData("func f(a, b)\n  var b = 1\n  return b\nend")>]
[<InlineData("func f(a, b)\n  var c = 1\n  var a = 2\nend")>]
let Parameter_RedeclaredInOutermostBodyBlock_IsRejectedWithVariableAlreadyDeclared (source: string) =
    rejects MessageCode.VariableAlreadyDeclared source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-269")>]
[<InlineData("func f(a)\n  if true then\n    var a = 1\n  end\n  return a\nend\nOutput.r = f(2)", "2")>]
[<InlineData("func f(a)\n  foreach x in [1] do\n    var a = 1\n  end\n  return a\nend\nOutput.r = f(3)", "3")>]
[<InlineData("func f(a)\n  var b = 1\n  return a + b\nend\nOutput.r = f(2)", "3")>]
[<InlineData("func f(a)\n  if true then\n    var a = 10\n    return a\n  end\nend\nOutput.r = f(2)", "10")>]
let Parameter_ShadowedInInnerBlockOnly (source: string, expected: string) =
    notRejectedWith MessageCode.VariableAlreadyDeclared source
    assertR expected source

// ---- 60.5.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-270")>]
[<InlineData("Output.x = 1")>]
[<InlineData("return Output")>]
[<InlineData("var o = Output")>]
[<InlineData("return Output[\"x\"]")>]
[<InlineData("Output = {}")>]
[<InlineData("delete Output.x")>]
[<InlineData("Output.n++")>]
[<InlineData("return Length(Output)")>]
[<InlineData("if true then\n    Output.x = 1\n  end")>]
[<InlineData("foreach q in [1] do\n    return Output.x\n  end")>]
let FunctionBody_ReferenceToOutput_IsRejectedWithOutputAccessInFunction (stmt: string) =
    rejects MessageCode.OutputAccessInFunction (sprintf "func f()\n  %s\nend" stmt)

// ---- 60.5.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-271")>]
[<InlineData("func f(o)\n  o.x = 1\nend\nf(Output)", "{\"x\": 1}")>]
[<InlineData("func g(a)\n  a[Length(a)] = 5\nend\nOutput.list = []\ng(Output.list)", "{\"list\": [5]}")>]
[<InlineData("func h(o)\n  delete o.y\nend\nOutput.y = 1\nOutput.z = 2\nh(Output)", "{\"z\": 2}")>]
[<InlineData("func k(o)\n  o = {}\n  o.x = 1\nend\nOutput.a = 1\nk(Output)", "{\"a\": 1}")>]
[<InlineData("func n(o)\n  o.c++\nend\nOutput.c = 1\nn(Output)", "{\"c\": 2}")>]
[<InlineData("func m(o)\n  o.inner.v = 3\nend\nOutput.inner = {}\nm(Output)", "{\"inner\": {\"v\": 3}}")>]
let Function_ChangesOutputReceivedAsArgument (source: string, expected: string) =
    assertOut expected source

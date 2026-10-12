/// 40.7 Assignment, 40.8 Increment and decrement, 40.9 Input is read-only.
module RealTree.Jyro.Spec.Tests.P40_60.S40_07_Assignment

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

/// Declarations that make every name the target forms use a declared one.
let private decls =
    "var x = 0\nvar b = 0\nvar c = true\nvar i = 0\nvar j = 0\nvar o = {a: {b: 0}}\nvar a = [{b: [0]}]\nfunc f()\n  return {x: 0}\nend\n"

// ---- 40.7.1 --------------------------------------------------------------

let acceptedTargets =
    [ "x"; "o.a"; "o[\"a\"]"; "a[0]"; "f().x"; "(o).a"; "a[i].b[j]"; "o.a.b"; "f()[0]"; "(a)[0]"; "o.end" ]

let acceptedTargetCases : seq<obj[]> =
    seq {
        for t in acceptedTargets do
            for op in [ "="; "+="; "-="; "*="; "/="; "%="; "&=" ] do
                yield [| box t; box op |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-163")>]
[<MemberData(nameof acceptedTargetCases)>]
let ``JYRO-REQ-163 AssignmentTarget_IdentifierPropertyOrIndexAccess_PassesValidation`` (target: string, op: string) =
    let src = decls + sprintf "%s %s 1" target op
    notRejectedWith MessageCode.InvalidAssignmentTarget src
    compiles src

[<Theory>]
[<Trait("requirement", "JYRO-REQ-163")>]
[<InlineData("1 = 2")>]
[<InlineData("f() = 1")>]
[<InlineData("(x + b) = 1")>]
[<InlineData("(x) = 1")>]
[<InlineData("x++ = 1")>]
[<InlineData("\"s\" = 1")>]
[<InlineData("[1] = 2")>]
[<InlineData("-x = 1")>]
[<InlineData("(c ? x : b) = 1")>]
[<InlineData("x ?? b = 1")>]
[<InlineData("f() += 1")>]
[<InlineData("(x) += 1")>]
[<InlineData("null = 1")>]
[<InlineData("true = 1")>]
[<InlineData("not x = 1")>]
[<InlineData("x is number = 1")>]
let ``JYRO-REQ-163 AssignmentTarget_OtherForm_IsRejectedWithInvalidAssignmentTarget`` (stmt: string) =
    rejectsNotAtParse "JYRO-REQ-163" MessageCode.InvalidAssignmentTarget (decls + stmt)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-163")>]
let ``JYRO-REQ-163 AssignmentTarget_ParenthesisedIdentifierWithTypeHint_IsRejectedWithInvalidAssignmentTarget`` () =
    rejectsNotAtParse "JYRO-REQ-163" MessageCode.InvalidAssignmentTarget (decls + "(x): number = 1")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-163")>]
let ``JYRO-REQ-163 AssignmentTarget_ObjectLiteral_IsRejectedWithInvalidAssignmentTarget`` () =
    rejectsNotAtParse "JYRO-REQ-163" MessageCode.InvalidAssignmentTarget (decls + "Output.q = 0\n{} = 1")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-163")>]
let ``JYRO-REQ-163 AssignmentTarget_AcceptedForms_RunAndWrite`` () =
    assertOut """{"o": {"a": 5}, "a": [{"b": [7]}], "p": 9}"""
        "var o = {a: 1}\nvar a = [{b: [0]}]\nvar i = 0\nvar j = 0\nif true then end\n(o).a = 5\na[i].b[j] = 7\nOutput.o = o\nOutput.a = a\nOutput.p = 0\nif true then end\n(Output).p = 9"

// ---- 40.7.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-164")>]
[<InlineData("=", "2")>]
[<InlineData("+=", "9")>]
[<InlineData("-=", "5")>]
[<InlineData("*=", "14")>]
[<InlineData("/=", "3.5")>]
[<InlineData("%=", "1")>]
[<InlineData("&=", "72")>]
let AssignmentOperator_EachOfTheSeven_HasItsMeaning (op: string, expected: string) =
    let src =
        sprintf "var x = 7\nx %s 2\nvar o = {p: 7}\no.p %s 2\nvar a = [7]\na[0] %s 2\nOutput.x = x & \"\"\nOutput.p = o.p & \"\"\nOutput.e = a[0] & \"\"" op op op
    let out = Script.outputOf src
    for k in [ "x"; "p"; "e" ] do Assert.Equal(expected, Values.asString (Values.prop k out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-164")>]
[<InlineData("**=")>]
[<InlineData("^=")>]
[<InlineData("??=")>]
[<InlineData("|=")>]
[<InlineData("&&=")>]
[<InlineData("||=")>]
[<InlineData(":=")>]
[<InlineData("<<=")>]
[<InlineData(">>=")>]
let AssignmentOperator_Other_IsRejectedBeforeExecution (op: string) =
    compileFails (sprintf "var x = 7\nx %s 2" op)

// ---- 40.7.3 --------------------------------------------------------------

let private outcome (src: string) =
    let r = Script.run src
    r.IsSuccess, Script.errorCodes r, (if r.IsSuccess then Values.toJson (Script.valueOf r) else "")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-165")>]
[<InlineData("+", "7", "2")>]
[<InlineData("-", "7", "2")>]
[<InlineData("*", "7", "2")>]
[<InlineData("/", "7", "2")>]
[<InlineData("%", "7", "2")>]
[<InlineData("&", "7", "2")>]
[<InlineData("&", "\"a\"", "null")>]
[<InlineData("+", "7", "Input.s")>]
[<InlineData("+", "7", "ToString(1)")>]
[<InlineData("/", "7", "0")>]
[<InlineData("%", "7", "0")>]
[<InlineData("-", "7", "null")>]
[<InlineData("*", "[]", "2")>]
[<InlineData("&", "7", "[]")>]
[<InlineData("+", "0.1", "0.2")>]
let CompoundAssignment_GivesTheSameResultAsExpandedForm (op: string, start: string, v: string) =
    let compound = sprintf "var x = %s\nx %s= %s\nOutput.x = x" start op v
    let expanded = sprintf "var x = %s\nx = x %s %s\nOutput.x = x" start op v
    let a = outcome compound
    let b = outcome expanded
    Assert.Equal(b, a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-165")>]
let CompoundAssignment_IndexEvaluatedOnce () =
    assertOut """{"n": 1, "a": [6]}"""
        "func idx(o)\n  o.n = (o.n ?? 0) + 1\n  return 0\nend\nvar a = [5]\na[idx(Output)] += 1\nOutput.a = a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-165")>]
let CompoundAssignment_ObjectEvaluatedOnce () =
    assertOut """{"n": 1, "t": {"k": 2}}"""
        "func obj(o, t)\n  o.n = (o.n ?? 0) + 1\n  return t\nend\nvar t = {k: 1}\nobj(Output, t).k += 1\nOutput.t = t"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-165")>]
let CompoundAssignment_ObjectAndStringKeyEvaluatedOnce () =
    assertOut """{"n": 2, "t": {"k": "ab"}}"""
        "func obj(o, t)\n  o.n = (o.n ?? 0) + 1\n  return t\nend\nfunc key(o)\n  o.n = (o.n ?? 0) + 1\n  return \"k\"\nend\nvar t = {k: \"a\"}\nobj(Output, t)[key(Output)] &= \"b\"\nOutput.t = t"

// ---- 40.8.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-166")>]
[<InlineData("x++")>]
[<InlineData("x--")>]
[<InlineData("++x")>]
[<InlineData("--x")>]
[<InlineData("o.a++")>]
[<InlineData("o[\"a\"]--")>]
[<InlineData("a[0]++")>]
[<InlineData("--a[0]")>]
[<InlineData("++o.a.b")>]
[<InlineData("f().x++")>]
[<InlineData("(o).a++")>]
let ``JYRO-REQ-166 IncrementTarget_IdentifierPropertyOrIndex_IsAccepted`` (stmt: string) =
    let src = decls + stmt
    notRejectedWith MessageCode.InvalidIncrementTarget src
    compiles src

[<Theory>]
[<Trait("requirement", "JYRO-REQ-166")>]
[<InlineData("5++")>]
[<InlineData("++5")>]
[<InlineData("\"s\"--")>]
[<InlineData("Length(a)++")>]
[<InlineData("f()++")>]
[<InlineData("--f()")>]
[<InlineData("(x + 1)++")>]
[<InlineData("++(x + 1)")>]
[<InlineData("(x)++")>]
[<InlineData("null++")>]
[<InlineData("true--")>]
[<InlineData("[1]++")>]
let ``JYRO-REQ-166 IncrementTarget_OtherForm_IsRejectedWithInvalidIncrementTarget`` (stmt: string) =
    rejects MessageCode.InvalidIncrementTarget (decls + stmt)

// ---- 40.8.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-167")>]
[<InlineData("var x = 5\nOutput.r = ++x\nOutput.t = x", "6", "6")>]
[<InlineData("var x = 5\nOutput.r = --x\nOutput.t = x", "4", "4")>]
[<InlineData("var o = {a: 1}\nOutput.r = ++o.a\nOutput.t = o.a", "2", "2")>]
[<InlineData("var o = {a: 1}\nOutput.r = --o[\"a\"]\nOutput.t = o.a", "0", "0")>]
[<InlineData("var a = [1]\nOutput.r = ++a[0]\nOutput.t = a[0]", "2", "2")>]
[<InlineData("var y = 0.5\nOutput.r = ++y\nOutput.t = y", "1.5", "1.5")>]
[<InlineData("var y = -0.5\nOutput.r = --y\nOutput.t = y", "-1.5", "-1.5")>]
let PrefixIncrement_GivesTheNewValue (source: string, r: string, t: string) =
    let out = Script.outputOf source
    Assert.Equal(norm r, Values.toJson (Values.prop "r" out))
    Assert.Equal(norm t, Values.toJson (Values.prop "t" out))

// ---- 40.8.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-168")>]
[<InlineData("var x = 5\nOutput.r = x++\nOutput.t = x", "5", "6")>]
[<InlineData("var x = 5\nOutput.r = x--\nOutput.t = x", "5", "4")>]
[<InlineData("var o = {a: 1}\nOutput.r = o.a++\nOutput.t = o.a", "1", "2")>]
[<InlineData("var o = {a: 1}\nOutput.r = o[\"a\"]--\nOutput.t = o.a", "1", "0")>]
[<InlineData("var a = [1]\nOutput.r = a[0]--\nOutput.t = a[0]", "1", "0")>]
[<InlineData("var y = 0.5\nOutput.r = y++\nOutput.t = y", "0.5", "1.5")>]
let PostfixIncrement_GivesTheValueBefore (source: string, r: string, t: string) =
    let out = Script.outputOf source
    Assert.Equal(norm r, Values.toJson (Values.prop "r" out))
    Assert.Equal(norm t, Values.toJson (Values.prop "t" out))

// ---- 40.8.4 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-169")>]
let Increment_WritesToTheTargetItRead () =
    assertOut """{"x": 1, "o": {"a": 2, "b": 5}, "a": [0, 9, 0]}"""
        "var x = 0\nx++\nvar o = {a: 1, b: 5}\no.a++\nvar a = [0, 10, 0]\na[1]--\nOutput.x = x\nOutput.o = o\nOutput.a = a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-169")>]
let Increment_IndexEvaluatedOnce () =
    assertOut """{"n": 1, "a": [0, 11, 20]}"""
        "func idx(o)\n  o.n = (o.n ?? 0) + 1\n  return 1\nend\nvar a = [0, 10, 20]\na[idx(Output)]++\nOutput.a = a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-169")>]
let Increment_ObjectEvaluatedOnce () =
    assertOut """{"n": 1, "t": {"k": 2}, "r": 2}"""
        "func obj(o, t)\n  o.n = (o.n ?? 0) + 1\n  return t\nend\nvar t = {k: 1}\nvar r = ++obj(Output, t).k\nOutput.t = t\nOutput.r = r"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-169")>]
let Decrement_PostfixIndexEvaluatedOnce () =
    assertOut """{"n": 1, "a": [0, 9, 20], "r": 10}"""
        "func idx(o)\n  o.n = (o.n ?? 0) + 1\n  return 1\nend\nvar a = [0, 10, 20]\nvar r = a[idx(Output)]--\nOutput.a = a\nOutput.r = r"

// ---- 40.8.2 to 40.8.4: the new value of a number target is t + 1 or t - 1 (JYRO-REQ-1727) ----------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1727")>]
[<InlineData("var x = 1e21\nx++\nOutput.r = ToString(x)\nOutput.e = x == 1000000000000000000001", "1000000000000000000001")>]
[<InlineData("var x = 1e21\n++x\nOutput.r = ToString(x)\nOutput.e = x == 1000000000000000000001", "1000000000000000000001")>]
[<InlineData("var y = 9007199254740993\ny++\nOutput.r = ToString(y)\nOutput.e = y == 9007199254740994", "9007199254740994")>]
[<InlineData("var y = 9007199254740993\n++y\nOutput.r = ToString(y)\nOutput.e = y == 9007199254740994", "9007199254740994")>]
[<InlineData("var z = 0.1\nz++\nOutput.r = ToString(z)\nOutput.e = z == 1.1", "1.1")>]
[<InlineData("var z = 0.1\n++z\nOutput.r = ToString(z)\nOutput.e = z == 1.1", "1.1")>]
[<InlineData("var x = 1e21\nx--\nOutput.r = ToString(x)\nOutput.e = x == 999999999999999999999", "999999999999999999999")>]
[<InlineData("var x = 1e21\n--x\nOutput.r = ToString(x)\nOutput.e = x == 999999999999999999999", "999999999999999999999")>]
[<InlineData("var y = 9007199254740993\ny--\nOutput.r = ToString(y)\nOutput.e = y == 9007199254740992", "9007199254740992")>]
[<InlineData("var z = 0.1\nz--\nOutput.r = ToString(z)\nOutput.e = z == -0.9", "-0.9")>]
[<InlineData("var z = 0.1\n--z\nOutput.r = ToString(z)\nOutput.e = z == -0.9", "-0.9")>]
let ``JYRO-REQ-1727 The new value of an exact number target is exact`` (source: string, text: string) =
    let out = Script.outputOf source
    Assert.Equal(text, Values.asString (Values.prop "r" out))
    Assert.True(Values.asBool (Values.prop "e" out), "the new value is not equal to the exact number the clause gives")

let incrementPlaces : seq<obj[]> =
    seq {
        for lit in [ "1e21"; "9007199254740993"; "0.1"; "123456789012345678901234567890"; "-0.5"; "0"; "79228162514264337593543950335"; "1e-20" ] do
            for op in [ "+"; "-" ] do
                for place in [ "variable postfix"; "variable prefix"; "property postfix"; "property prefix"; "element postfix"; "element prefix" ] do
                    yield [| box lit; box op; box place |]
    }

/// The script that increments (or decrements) a target holding `lit` by `place`, and records the
/// new value, the value of the expression, and the same new value made by `t = t + 1` (or `- 1`).
let private incrementScript (lit: string) (op: string) (place: string) : string =
    let sign = if op = "+" then "++" else "--"
    let decl, target =
        if place.StartsWith "variable" then sprintf "var t = %s" lit, "t"
        elif place.StartsWith "property" then sprintf "var o = {p: %s}" lit, "o.p"
        else sprintf "var o = [%s]" lit, "o[0]"
    let step = if place.EndsWith "prefix" then sign + target else target + sign
    let expectedOfExpression = if place.EndsWith "prefix" then "u" else "v"
    String.concat "\n"
        [ decl
          sprintf "var u = %s %s 1" lit op
          sprintf "var v = %s" lit
          sprintf "var r = %s" step
          sprintf "Output.eq = %s == u" target
          sprintf "Output.same = ToString(%s) == ToString(u)" target
          sprintf "Output.rEq = r == %s" expectedOfExpression
          sprintf "Output.rSame = ToString(r) == ToString(%s)" expectedOfExpression ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1727")>]
[<MemberData(nameof incrementPlaces)>]
let ``JYRO-REQ-1727 x++ and x-- leave the same value as x = x + 1 and x = x - 1 at every place`` (lit: string, op: string, place: string) =
    let out = Script.outputOf (incrementScript lit op place)
    Assert.True(Values.asBool (Values.prop "eq" out), "the new value is not equal to t + 1 or t - 1")
    Assert.True(Values.asBool (Values.prop "same" out), "the string form of the new value is not that of t + 1 or t - 1")
    Assert.True(Values.asBool (Values.prop "rEq" out), "the value of the expression is not the one that the form gives")
    Assert.True(Values.asBool (Values.prop "rSame" out), "the string form of the value of the expression is not the one that the form gives")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1727")>]
let ``JYRO-REQ-1727 x++ leaves the same value as x = x + 1 for the clause's three targets`` () =
    let out =
        Script.outputOf
            (String.concat "\n"
                [ "var x = 1e21"
                  "var x2 = 1e21"
                  "x++"
                  "x2 = x2 + 1"
                  "var y = 9007199254740993"
                  "var y2 = 9007199254740993"
                  "y++"
                  "y2 = y2 + 1"
                  "var z = 0.1"
                  "var z2 = 0.1"
                  "z++"
                  "z2 = z2 + 1"
                  "Output.x = x == x2 and ToString(x) == ToString(x2)"
                  "Output.y = y == y2 and ToString(y) == ToString(y2)"
                  "Output.z = z == z2 and ToString(z) == ToString(z2)"
                  "Output.zz = z == 1.1" ])
    for key in [ "x"; "y"; "z"; "zz" ] do
        Assert.True(Values.asBool (Values.prop key out), sprintf "Output.%s is not true" key)

// ---- 40.8.5 --------------------------------------------------------------

let incrementNonNumberCases : seq<obj[]> =
    seq {
        for v in [ "null"; "true"; "[]"; "{}"; "x => x"; "Input.s" ] do
            for place in [ "variable"; "property"; "element" ] do
                for op in [ "++"; "--" ] do
                    yield [| box v; box place; box op |]
    }

/// `Input.s` is a string that validation cannot see (40.2.3 reads only declarations).
let private sInput = """{"s": "a"}"""

let private compareOutcome (incrementSrc: string) (binarySrc: string) =
    let ri = Script.runJson sInput incrementSrc
    let rb = Script.runJson sInput binarySrc
    Assert.False(ri.IsSuccess, "the increment fails")
    Assert.Equal((Script.compile binarySrc).IsSuccess, (Script.compile incrementSrc).IsSuccess)   // same stage
    Assert.Equal<MessageCode list>(Script.errorCodes rb, Script.errorCodes ri)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-170")>]
[<MemberData(nameof incrementNonNumberCases)>]
let ``JYRO-REQ-170 Increment_NonNumberTarget_RaisesTheErrorOfPlusOrMinusOne`` (value: string, place: string, op: string) =
    let binary = if op = "++" then "+" else "-"
    let setup, target =
        match place with
        | "variable" -> sprintf "var t = %s\n" value, "t"
        | "property" -> sprintf "var o = {t: %s}\n" value, "o.t"
        | _ -> sprintf "var a = [%s]\n" value, "a[0]"
    compareOutcome (setup + target + op) (setup + sprintf "Output.r = %s %s 1" target binary)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-170")>]
let ``JYRO-REQ-170 Increment_StringFromRunTime_RaisesStringOperandInAdditionRuntime`` () =
    runtimeErrorWith sInput MessageCode.StringOperandInAdditionRuntime "var t = Input.s\nt++"
    runtimeErrorWith sInput MessageCode.UnsupportedBinaryOperation "var t = Input.s\nt--"
    runtimeErrorWith sInput MessageCode.StringOperandInAdditionRuntime "var o = {t: Input.s}\n++o.t"
    runtimeErrorWith sInput MessageCode.UnsupportedBinaryOperation "var a = [Input.s]\n--a[0]"
    runtimeError MessageCode.UnsupportedBinaryOperation "var t = null\nt++"
    runtimeError MessageCode.UnsupportedBinaryOperation "var t = true\nt--"

// On a string-declared variable, `s++` is refused at validation as `s + 1` is:
// that is 40.2.9's rule (JYRO-REQ-141), not 40.8.5's, whose statement is about
// the error raised, so the case is verified under 141. `s--` is refused by 40.2.9
// although `s - 1` is a runtime error, so only `++` is compared here.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-141")>]
let ``JYRO-REQ-141 Increment_StringLiteralDeclaredVariable_MatchesPlusOneAtValidation`` () =
    compareOutcome "var s = \"a\"\ns++" "var s = \"a\"\nOutput.r = s + 1"

// ---- 40.9.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2144")>]
[<InlineData("Input = 1")>]
[<InlineData("(Input).a = 1")>]
[<InlineData("((Input)).a = 1")>]
[<InlineData("(Input).a.b = 1")>]
[<InlineData("(Input)[\"a\"] = 1")>]
[<InlineData("(Input.a).b = 1")>]
[<InlineData("(Input).n += 1")>]
[<InlineData("Input.n += 1")>]
[<InlineData("Input[\"a\"][0] = 1")>]
[<InlineData("Input.a = 1")>]
[<InlineData("Input.a.b = 1")>]
[<InlineData("Input[0] = 1")>]
[<InlineData("Input.a[0].b = 1")>]
[<InlineData("Input[\"a\"] = 1")>]
[<InlineData("Input.a += 1")>]
[<InlineData("Input &= \"x\"")>]
[<InlineData("Input.a -= 1")>]
[<InlineData("Input[\"a\"] &= 1")>]
[<InlineData("if true then Input.a = 1 end")>]
[<InlineData("while false do Input.a = 1 end")>]
[<InlineData("func f()\n  Input.a = 1\nend")>]
[<InlineData("func f()\n  if true then Input[0] = 1 end\nend")>]
let ``JYRO-REQ-2144 InputRootedAssignment_IsRejectedWithInputIsReadOnly`` (source: string) =
    rejects MessageCode.InputIsReadOnly source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2144")>]
let ``JYRO-REQ-2144 AssignmentToVariableHoldingInput_IsNotRejectedByValidation`` () =
    notRejectedWith MessageCode.InputIsReadOnly "var i = Input\ni = 1"
    compiles "var i = Input\ni = 1"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2144")>]
[<InlineData("var t = Input.a\nt.x = 1")>]
[<InlineData("var t = Input\nt.x = 1")>]
[<InlineData("var t = (Input).a\nt.x = 1")>]
[<InlineData("func f(x)\n  return x\nend\nf(Input).x = 1")>]
[<InlineData("func f(x)\n  return x\nend\nvar t = f(Input)\nt.x = 1")>]
[<InlineData("Output.a = 1")>]
[<InlineData("var Inputs = {}\nInputs.a = 1")>]
let ``JYRO-REQ-2144 AssignmentWhoseTargetIsNotRootedAtInput_IsNotRejectedWithInputIsReadOnly`` (source: string) =
    notRejectedWith MessageCode.InputIsReadOnly source

// ---- 40.9.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1745")>]
[<InlineData("(Input).n--")>]
[<InlineData("++(Input).n")>]
[<InlineData("(Input)[\"a\"][0]++")>]
[<InlineData("--(Input.a).n")>]
[<InlineData("Input.n++")>]
[<InlineData("--Input[\"a\"][0]")>]
[<InlineData("Input++")>]
[<InlineData("--Input")>]
[<InlineData("Input.a++")>]
[<InlineData("++Input.a.b")>]
[<InlineData("Input[0]--")>]
[<InlineData("Input.a[0].b++")>]
[<InlineData("Input[\"a\"]--")>]
[<InlineData("func f()\n  Input.a++\nend")>]
[<InlineData("func f()\n  --Input[0]\nend")>]
[<InlineData("if true then Input.a-- end")>]
let ``JYRO-REQ-1745 InputRootedIncrement_IsRejectedWithInputIsReadOnly`` (source: string) =
    rejects MessageCode.InputIsReadOnly source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1745")>]
[<InlineData("var t = Input.a\nt.n++")>]
[<InlineData("var t = Input.a\n--t.n")>]
[<InlineData("var t = Input\nt.n++")>]
[<InlineData("var n = 0\nn++")>]
[<InlineData("Output.n = 0\nOutput.n++")>]
let ``JYRO-REQ-1745 IncrementWhoseTargetIsNotRootedAtInput_IsNotRejectedWithInputIsReadOnly`` (source: string) =
    notRejectedWith MessageCode.InputIsReadOnly source

// ---- 40.9.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1746")>]
[<InlineData("delete (Input).a")>]
[<InlineData("delete ((Input)).a")>]
[<InlineData("delete (Input)[\"a\"]")>]
[<InlineData("delete (Input.a).b")>]
[<InlineData("delete Input.a")>]
[<InlineData("delete Input[\"a\"]")>]
[<InlineData("delete Input.a.b")>]
[<InlineData("delete Input[0]")>]
[<InlineData("delete Input.a[0].b")>]
[<InlineData("func f()\n  delete Input.a\nend")>]
[<InlineData("func f()\n  if true then delete Input.a.b end\nend")>]
[<InlineData("foreach q in [1] do delete Input.a end")>]
let ``JYRO-REQ-1746 InputRootedDelete_IsRejectedWithInputIsReadOnly`` (source: string) =
    rejects MessageCode.InputIsReadOnly source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1746")>]
[<InlineData("var t = Input.a\ndelete t.x")>]
[<InlineData("var t = Input\ndelete t.x")>]
[<InlineData("var t = {x: 1}\ndelete t.x")>]
[<InlineData("Output.x = 1\ndelete Output.x")>]
let ``JYRO-REQ-1746 DeleteWhoseTargetIsNotRootedAtInput_IsNotRejectedWithInputIsReadOnly`` (source: string) =
    notRejectedWith MessageCode.InputIsReadOnly source

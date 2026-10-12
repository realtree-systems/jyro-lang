/// 40.1 Precedence and associativity.
module RealTree.Jyro.Spec.Tests.P40_60.S40_01_Precedence

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// Each case is chosen so that the grouping the table gives and the other
// grouping produce different outcomes.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1954")>]
[<InlineData("1 + 2 * 3", "7")>]                      // * above +
[<InlineData("10 - 4 - 3", "3")>]                     // - left
[<InlineData("24 / 4 / 2", "3")>]                     // / left
[<InlineData("2 * 7 % 4", "2")>]                      // * % left (right would give 6)
[<InlineData("20 % 7 * 2", "12")>]                    // % * left (right would give 6)
[<InlineData("\"a\" & 1 + 2", "\"a3\"")>]             // + above &
[<InlineData("1 + 2 & 3", "\"33\"")>]                 // + above & (right operand side)
[<InlineData("1 & 2 < \"2\"", "true")>]               // & above < ("12" < "2")
[<InlineData("1 < 2 == true", "true")>]               // relational above equality
[<InlineData("1 == 1 == true", "true")>]              // == left (right would give false)
[<InlineData("1 != 2 == true", "true")>]              // != == left
[<InlineData("1 == 1 is boolean", "true")>]           // == above is
[<InlineData("not 1 is string", "true")>]             // is above not
[<InlineData("not 1 == 2", "true")>]                  // == above not
[<InlineData("not true and false", "false")>]         // not above and
[<InlineData("true or true and false", "true")>]      // and above or
[<InlineData("false and true or true", "true")>]      // and above or (other side)
[<InlineData("0 ?? 1 or 2", "0")>]                    // or above ??
[<InlineData("1 ?? false ? 2 : 3", "2")>]             // ?? above ?:
[<InlineData("true or false ? 5 : 6", "5")>]          // or above ?:
[<InlineData("null ?? null ?? 3", "3")>]
[<InlineData("1 ?? 2 ?? 3", "1")>]
[<InlineData("true ? 1 : false ? 2 : 3", "1")>]       // ?: right (left would give 2)
[<InlineData("false ? 1 : false ? 2 : 3", "3")>]
[<InlineData("false ? 1 : true ? 2 : 3", "2")>]
[<InlineData("-2 * 3", "-6")>]
[<InlineData("- 2 + 3", "1")>]                        // prefix - above binary +
[<InlineData("2 - -3", "5")>]
let Precedence_ExpressionGroupsAsTheTableGives (expr: string, expected: string) =
    assertR expected (sprintf "Output.r = %s" expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1954")>]
let Precedence_PostfixPropertyAccessBindsAbovePrefixMinus () =
    assertR "-5" "var o = {a: 5}\nOutput.r = -o.a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1954")>]
let Precedence_PostfixIncrementBindsAboveMultiply () =
    assertOut """{"r": 10, "a": [2]}""" "var a = [1]\nOutput.r = a[0]++ * 10\nOutput.a = a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1954")>]
let Precedence_PrefixIncrementBindsAboveMultiply () =
    assertOut """{"r": 20, "a": [2]}""" "var a = [1]\nOutput.r = ++a[0] * 10\nOutput.a = a"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1954")>]
let Precedence_PostfixAccessChainsLeftToRight () =
    assertR "7" "var o = {a: [{b: 7}]}\nOutput.r = o.a[0].b"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1954")>]
let Precedence_RelationalIsLeftAssociative_ChainRaisesUnsupportedBinaryOperation () =
    // (1 < 2) < 3 compares a boolean with a number (30.11.4).
    runtimeError MessageCode.UnsupportedBinaryOperation "Output.r = 1 < 2 < 3"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1954")>]
[<InlineData("Output.r = 1 is number is boolean")>]
[<InlineData("Output.r = 1 is not number is boolean")>]
[<InlineData("Output.r = 1 is number is not boolean")>]
let Precedence_ChainedIsIsRejectedBeforeExecution (source: string) =
    compileFails source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-132")>]
let ``JYRO-REQ-132 Call_OnDeclaredFunctionIdentifier_ParsesAndRuns`` () =
    assertR "2" "Output.r = Length(\"ab\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-132")>]
let ``JYRO-REQ-132 Call_OnScriptFunctionIdentifier_ParsesAndRuns`` () =
    assertR "3" "func f(x)\n  return x + 1\nend\nOutput.r = f(2)"

// 40.1.2 as revised for jyro-spec--d-100: on one line, `(` after a property
// access, an index access, a call or a parenthesised expression is a parse
// error (20.1.3); after a line end it starts a new expression (20.1.2). Either
// way, the function value held at the callee position is never invoked.
let private notInvoked (source: string) =
    let recorder = Host.recorder "Rec"
    let _ = Script.runWith { Script.defaults with Functions = [ recorder.AsFunction ] } source
    Assert.Equal(0, recorder.CallCount)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-132")>]
[<InlineData("var o = {f: x => Rec(x)}\no.f(1)")>]
[<InlineData("var o = {f: x => Rec(x)}\nOutput.r = o.f(1)")>]
[<InlineData("var a = [x => Rec(x)]\na[0](1)")>]
[<InlineData("var a = [x => Rec(x)]\nOutput.r = a[0](1)")>]
[<InlineData("var g = x => Rec(x)\n(g)(1)")>]
[<InlineData("var g = x => Rec(x)\nOutput.r = (g)(1)")>]
[<InlineData("(x => Rec(x))(1)")>]
[<InlineData("func mk()\n  return x => Rec(x)\nend\nmk()(1)")>]
[<InlineData("var g = x => Rec(x)\ng(1)")>]
[<InlineData("var o = {f: x => Rec(x)}\nvar r = o.f\n(1)")>]
[<InlineData("var a = [x => Rec(x)]\nvar r = a[0]\n(1)")>]
[<InlineData("var g = x => Rec(x)\n(g)\n(1)")>]
[<InlineData("(x => Rec(x))\n(1)")>]
let ``JYRO-REQ-132 Call_OnPropertyIndexParenthesisedOrCallResult_NeverInvokesTheValue`` (source: string) =
    notInvoked source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-132")>]
[<InlineData("var g = x => x\nOutput.r = g(1)")>]      // g is a variable, not a declared function
[<InlineData("var g = x => x\ng(1)")>]
[<InlineData("func f() return 1 end\nOutput.r = f()()")>]
[<InlineData("Output.r = (Length)(\"a\")")>]         // a function name used as a value
let ``JYRO-REQ-132 Call_WhoseCalleeIsNotADeclaredFunctionIdentifier_IsRejectedBeforeExecution`` (source: string) =
    compileFails source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-132")>]
[<InlineData("var g = x => x\n(g)(1)")>]
[<InlineData("var o = {f: x => x}\no.f(1)")>]
[<InlineData("var a = [x => x]\na[0](1)")>]
[<InlineData("Length(\"a\")(1)")>]
[<InlineData("(x => x)(1)")>]
[<InlineData("var g = x => x\nOutput.r = (g)(1)")>]
[<InlineData("var o = {f: x => x}\nOutput.r = o.f(1)")>]
[<InlineData("Output.r = (Length)(\"a\")")>]
[<InlineData("func f() return 1 end\nOutput.r = f()()")>]
let ``JYRO-REQ-132 Call_OnOneLineAfterPropertyIndexCallOrParenthesisedExpression_IsAParseError`` (source: string) =
    parseError source

/// The five forms the requirement names, each written on one line, are parse
/// errors: rejected before anything runs, every error of the stage Parse, and
/// the function value at the callee position never invoked.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-132")>]
[<InlineData("var g = x => Rec(x)\n", "(g)(1)")>]
[<InlineData("var o = {f: x => Rec(x)}\n", "o.f(1)")>]
[<InlineData("var a = [x => Rec(x)]\n", "a[0](1)")>]
[<InlineData("", "Length(\"a\")(1)")>]
[<InlineData("", "(x => Rec(x))(1)")>]
let ``JYRO-REQ-132 the five forms of the requirement on one line are parse errors and call nothing`` (setup: string, call: string) =
    let recorder = Host.recorder "Rec"
    let r, mark = runMarked { limited with Functions = [ recorder.AsFunction ] } (setup + call)
    Assert.False(r.IsSuccess, sprintf "Expected a parse error. Source:\n%s%s" setup call)
    assertAllStages "Parse" r
    Assert.Equal(0, mark.CallCount)
    Assert.Equal(0, recorder.CallCount)

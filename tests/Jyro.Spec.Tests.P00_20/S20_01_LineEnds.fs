/// 20.1.2 - a line end before `(`, `[`, `++` or `--` ends the expression.
/// 20.1.3 - on one line, `(` after an expression other than a name is a parse error.
module RealTree.Jyro.Spec.Tests.S20_01_LineEnds

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// The two line ends of 20.2.2.
let private lineEnds = [ "LF", "\n"; "CRLF", "\r\n" ]

/// `source` with every `⏎` replaced by the line end `nl`.
let private withLineEnd (nl: string) (source: string) = source.Replace("⏎", nl)

/// Each row crossed with both line ends.
let private crossLineEnds (rows: (string * string * string) list) : seq<obj[]> =
    seq {
        for (label, source, expected) in rows do
            for (name, nl) in lineEnds ->
                [| box (label + "_" + name); box (withLineEnd nl source); box expected |]
    }

let private crossLineEnds2 (rows: (string * string) list) : seq<obj[]> =
    seq {
        for (label, source) in rows do
            for (name, nl) in lineEnds ->
                [| box (label + "_" + name); box (withLineEnd nl source) |]
    }

// -- 20.1.2: the line-initial token starts the next expression --------------

let lineInitialTokenStartsANewExpression : seq<obj[]> =
    crossLineEnds
        [ // `++` is the prefix increment of the next statement, not a postfix on `{k: 1}`'s variable.
          "prefix_increment_of_property", "var o = {k: 1}⏎++o.k⏎Output.r = o.k", "2"
          "prefix_decrement_of_element", "var a = [1]⏎--a[0]⏎Output.r = a", "[0]"
          "prefix_increment_after_identifier", "var i = 1⏎var j = i⏎++i⏎Output.r = [i, j]", "[2,1]"
          "prefix_decrement_after_identifier", "var i = 3⏎var j = i⏎--i⏎Output.r = [i, j]", "[2,3]"
          "prefix_increment_after_literal", "var i = 1⏎var j = 7⏎++i⏎Output.r = [i, j]", "[2,7]"
          // `[` is an array literal statement, not an index access of `a`.
          "bracket_after_identifier", "var a = [5]⏎var x = a⏎[1]⏎Output.r = x", "[5]"
          "bracket_after_property", "var o = {k: [5, 6]}⏎var x = o.k⏎[1]⏎Output.r = x", "[5,6]"
          "bracket_after_array_literal", "var x = [1]⏎[0]⏎Output.r = x", "[1]"
          // `(` is a grouped expression statement, not the arguments of a call of `x`.
          "paren_after_identifier", "var x = 1⏎var y = x⏎(2)⏎Output.r = y", "1"
          "paren_after_grouped", "var x = (1)⏎(2)⏎Output.r = x", "1"
          "paren_after_call", "func F() return 3 end⏎var x = F()⏎(2)⏎Output.r = x", "3"
          // A comment before the line end changes nothing.
          "comment_then_prefix_increment", "var b = 1⏎var x = 2 # note⏎++b⏎Output.r = [x, b]", "[2,2]"
          "comment_then_bracket", "var a = [5]⏎var x = a # note⏎[1]⏎Output.r = x", "[5]" ]

[<Theory>]
[<MemberData(nameof lineInitialTokenStartsANewExpression)>]
[<Trait("requirement", "JYRO-REQ-14")>]
let ``JYRO-REQ-14 LineEnd_BeforeParenBracketOrIncrement_EndsTheExpressionAndStartsTheNext`` (label: string, source: string, expected: string) =
    ignore label
    Script.outputOf source |> Values.prop "r" |> assertJson expected

[<Theory>]
[<InlineData("\n")>]
[<InlineData("\r\n")>]
[<Trait("requirement", "JYRO-REQ-14")>]
let ``JYRO-REQ-14 LineEnd_BeforePrefixIncrementOfAVariableHoldingInput_IsNotAPostfixOnInput`` (nl: string) =
    // The d-103 case: read as `Input.o++` it would be refused with InputIsReadOnly.
    let r = Script.compile (sprintf "var o = Input.o%s++o.k" nl)
    Assert.DoesNotContain(MessageCode.InputIsReadOnly, Script.codes r)
    Assert.True(Script.ok r, Script.describe r)

[<Theory>]
[<InlineData("\n")>]
[<InlineData("\r\n")>]
[<Trait("requirement", "JYRO-REQ-14")>]
let ``JYRO-REQ-14 LineEnd_BeforeParenAfterAFunctionName_IsNotACall_SoTheNameIsUndeclaredVariable`` (nl: string) =
    // `x = F` then `(1)`: F as a value is refused by 60.3.2.
    rejected (sprintf "func F() return 1 end%svar x = F%s(1)" nl nl) |> assertOnly MessageCode.UndeclaredVariable
    rejected (sprintf "func F(a) return a end%svar x = 0%sx = F%s(1)" nl nl nl) |> assertOnly MessageCode.UndeclaredVariable

[<Theory>]
[<InlineData("\n")>]
[<InlineData("\r\n")>]
[<Trait("requirement", "JYRO-REQ-14")>]
let ``JYRO-REQ-14 LineEnd_BeforeParenAfterAFunctionName_NeverCallsTheFunction`` (nl: string) =
    // With a variable F holding a lambda, `x = F` then `(1)` compiles and calls nothing.
    let recf = Host.recorder "Rec"
    let r =
        Script.runWith { Script.defaults with Functions = [ recf.AsFunction ] }
            (sprintf "var F = v => Rec(v)%svar x = 2%sx = F%s(1)%sOutput.r = x is function" nl nl nl nl)
    Script.output r |> Values.prop "r" |> assertJson "true"
    Assert.Equal(0, recf.CallCount)

let noNewExpressionPermitted : seq<obj[]> =
    crossLineEnds2
        [ "index_inside_argument_list", "func F(x) return x end⏎var a = [[1]]⏎Output.r = F(a⏎[0])"
          "paren_inside_argument_list", "func F(x) return x end⏎var a = 1⏎Output.r = F(a⏎(0))"
          "increment_inside_argument_list", "func F(x, y) return x end⏎var a = 1⏎var b = 1⏎Output.r = F(a⏎++b)"
          "index_inside_array_literal", "var a = [1]⏎Output.r = [a⏎[0]]"
          "index_inside_object_literal", "var a = [1]⏎Output.r = {k: a⏎[0]}"
          "index_inside_parentheses", "var a = [1]⏎Output.r = (a⏎[0])"
          "paren_inside_if_condition", "var a = true⏎var b = 1⏎if a⏎(b) then Output.r = 1 end"
          "index_inside_while_condition", "var a = [false]⏎while a⏎[0] do end" ]

[<Theory>]
[<MemberData(nameof noNewExpressionPermitted)>]
[<Trait("requirement", "JYRO-REQ-14")>]
let ``JYRO-REQ-14 LineEnd_BeforeTokenWhereNoNewExpressionIsPermitted_IsAParseError`` (label: string, source: string) =
    ignore label
    parseError source

let otherTokensContinue : seq<obj[]> =
    crossLineEnds
        [ "minus_continues", "var a = 5⏎var x = a⏎- 1⏎Output.r = x", "4"
          "plus_continues", "var a = 5⏎var x = a⏎+ 1⏎Output.r = x", "6"
          "dot_continues", "var o = {k: 3}⏎var x = o⏎.k⏎Output.r = x", "3"
          "concat_continues", "var x = \"a\"⏎& \"b\"⏎Output.r = x", "\"ab\""
          "argument_after_comma", "func F(a, b) return b end⏎Output.r = F(1,⏎2)", "2"
          "array_argument_after_comma", "func F(a, b) return b end⏎Output.r = F(1,⏎[2])", "[2]"
          "array_element_after_comma", "Output.r = [1,⏎[2]]", "[1,[2]]"
          "argument_after_open_paren", "func F(a) return a end⏎Output.r = F(⏎[2])", "[2]"
          "lambda_argument_on_next_line", "Output.r = Map([1, 2],⏎x => x * 2)", "[2,4]" ]

[<Theory>]
[<MemberData(nameof otherTokensContinue)>]
[<Trait("requirement", "JYRO-REQ-14")>]
let ``JYRO-REQ-14 LineEnd_BeforeAnyOtherToken_IsWhitespace`` (label: string, source: string, expected: string) =
    ignore label
    Script.outputOf source |> Values.prop "r" |> assertJson expected

// -- 20.1.3: on one line, `(` after an expression other than a name ---------

let oneLineParenAfterNonName : seq<obj[]> =
    [ "parenthesised_name", "var g = x => Mark(x)\n(g)(1)"
      "property_access", "var o = {f: x => Mark(x)}\no.f(1)"
      "property_access_with_space", "var o = {f: x => Mark(x)}\no.f (1)"
      "index_access", "var a = [x => Mark(x)]\na[0](1)"
      "call_result", "Length(\"a\")(1)"
      "script_call_result", "func F() return 1 end\nF()(1)"
      "parenthesised_lambda", "(x => Mark(x))(1)"
      "postfix_increment", "var x = 1\nx++(1)"
      "array_literal", "var f = x => Mark(x)\n[f](1)"
      "number_after_assignment", "Output.r = 1  (2)"
      "string_literal", "\"s\"(1)"
      "boolean_literal", "true (1)"
      "null_literal", "null(1)"
      "object_literal", "{}(1)"
      "assigned_parenthesised_name", "var g = x => Mark(x)\nOutput.r = (g)(1)"
      "parenthesised_function_name", "Output.r = (Length)(\"a\")"
      "assigned_index_access", "var a = [1]\nOutput.r = a[0] (1)"
      "tab_between", "var o = {f: 1}\nOutput.r = o.f\t(1)" ]
    |> Seq.map (fun (l, s) -> [| box l; box s |])

[<Theory>]
[<MemberData(nameof oneLineParenAfterNonName)>]
[<Trait("requirement", "JYRO-REQ-15")>]
let ``JYRO-REQ-15 OneLine_ParenAfterAnExpressionThatIsNotAName_IsAParseErrorAndNothingRuns`` (label: string, source: string) =
    ignore label
    // `rejected` puts `Mark()` first; a parse error means Mark is never called.
    parseError source

[<Theory>]
[<InlineData("func F(a) return a end\nOutput.r = F (1)", "1")>]
[<InlineData("func F(a) return a end\nOutput.r = F\t(1)", "1")>]
[<InlineData("func F(a) return a end\nOutput.r = F(1)", "1")>]
[<InlineData("Output.r = Length(\"ab\")", "2")>]
[<InlineData("Output.r = Length (\"ab\")", "2")>]
[<Trait("requirement", "JYRO-REQ-15")>]
let ``JYRO-REQ-15 OneLine_ParenAfterASingleIdentifier_IsACall`` (source: string, expected: string) =
    Script.outputOf source |> Values.prop "r" |> assertJson expected

[<Theory>]
[<InlineData("\n")>]
[<InlineData("\r\n")>]
[<Trait("requirement", "JYRO-REQ-15")>]
let ``JYRO-REQ-15 SplitAcrossALineEnd_ParenAfterAnExpression_IsANewStatementNotAParseError`` (nl: string) =
    let recf = Host.recorder "Rec"
    let r =
        Script.runWith { Script.defaults with Functions = [ recf.AsFunction ] }
            (sprintf "var g = x => Rec(x)%s(g)%s(1)%sOutput.r = 1%s(2)" nl nl nl nl)
    Script.output r |> Values.prop "r" |> assertJson "1"
    Assert.Equal(0, recf.CallCount)

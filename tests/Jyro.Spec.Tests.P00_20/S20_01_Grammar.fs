/// 20.1.1 - the grammar: accepted forms, precedence, widened productions, parse errors.
module RealTree.Jyro.Spec.Tests.S20_01_Grammar

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- Every statement form of the grammar is accepted and has its meaning ------

let statementForms : seq<obj[]> =
    [ "func_def", "func f(a, b) return a + b end\nOutput.v = f(1, 2)", "3"
      "union_def_and_match_stmt",
        "union Shape Circle(r) Square(s: number) end\nvar c = Circle(2)\nmatch c do case Circle(r) then Output.v = r case Square(s) then Output.v = 0 end",
        "2"
      "var_decl_all_four_forms", "var a\nvar b: number = 2\nvar c = 3\nvar d: any\nOutput.v = [a, b, c, d]", "[null,2,3,null]"
      "if_then_end", "if true then Output.v = 1 end", "1"
      "if_elseif_taken", "var x = 2\nif x == 1 then Output.v = \"one\" elseif x == 2 then Output.v = \"two\" else Output.v = \"other\" end", "\"two\""
      "if_else_taken", "var x = 5\nif x == 1 then Output.v = \"one\" elseif x == 2 then Output.v = \"two\" else Output.v = \"other\" end", "\"other\""
      "if_two_elseif", "var x = 3\nif x == 1 then Output.v = 1 elseif x == 2 then Output.v = 2 elseif x == 3 then Output.v = 3 end", "3"
      "while", "var i = 0\nwhile i < 3 do i = i + 1 end\nOutput.v = i", "3"
      "for_to", "var s = \"\"\nfor i in 0 to 4 do s = s & i end\nOutput.v = s", "\"0123\""
      "for_to_by", "var s = \"\"\nfor i in 0 to 10 by 3 do s = s & i end\nOutput.v = s", "\"0369\""
      "for_downto", "var s = \"\"\nfor i in 3 downto 0 do s = s & i end\nOutput.v = s", "\"321\""
      "for_downto_by", "var s = \"\"\nfor i in 9 downto 0 by 4 do s = s & i end\nOutput.v = s", "\"951\""
      "foreach", "var s = \"\"\nforeach c in [\"a\", \"b\"] do s = s & c end\nOutput.v = s", "\"ab\""
      "switch_case_of_several_values", "var x = 3\nswitch x do case 1, 2 then Output.v = \"low\" case 3, 4 then Output.v = \"mid\" default then Output.v = \"high\" end", "\"mid\""
      "switch_default", "var x = 9\nswitch x do case 1, 2 then Output.v = \"low\" case 3, 4 then Output.v = \"mid\" default then Output.v = \"high\" end", "\"high\""
      "switch_no_case", "switch 1 do end\nOutput.v = 1", "1"
      "switch_only_default", "switch 1 do default then Output.v = 2 end", "2"
      "return_with_and_without_value", "func f() return end\nfunc g() return 5 end\nOutput.v = [f(), g()]", "[null,5]"
      "exit_without_value", "Output.v = 1\nexit\nOutput.v = 2", "1"
      "exit_with_value", "Output.v = 1\nexit \"bye\"\nOutput.v = 2", "1"
      "break_and_continue", "var s = \"\"\nforeach x in [1, 2, 3, 4] do if x == 2 then continue end if x == 4 then break end s = s & x end\nOutput.v = s", "\"13\""
      "delete", "var o = {a: 1, b: 2}\ndelete o.a\nOutput.v = o", "{\"b\":2}"
      "assign_ops_arithmetic", "var x = 10\nx += 5\nx -= 3\nx *= 2\nx /= 4\nx %= 4\nOutput.v = x", "2"
      "assign_op_concat", "var s = \"a\"\ns &= \"b\"\nOutput.v = s", "\"ab\""
      "assignment_with_hint", "Output.v: number = 5", "5"
      "any_as_identifier", "var any = 1\nOutput.v = any", "1"
      "any_after_colon_and_is", "var a: any = 1\nOutput.v = a is any", "true"
      "keyword_after_dot_and_as_key", "var o = {end: 1, if: 2}\nOutput.v = o.end + o.if", "3"
      "named_arguments", "func f(a, b) return a - b end\nOutput.v = f(b: 1, a: 5)", "4"
      "lambda_one_bare_param", "Output.v = Map([1, 2], x => x * 2)", "[2,4]"
      "lambda_two_params", "Output.v = Reduce([1, 2, 3], (acc, x) => acc + x, 0)", "6"
      "lambda_no_params", "var f = () => 1\nOutput.v = f is function", "true"
      "match_expression", "union U A(v) B end\nvar u = A(7)\nOutput.v = match u do case A(v) then v case B then 0 end", "7"
      "both_quote_kinds", "Output.v = \"a\\tb\" == 'a\\tb'", "true"
      "grouped", "Output.v = (1 + 2) * 3", "9"
      "postfix_index_chain", "var a = [[1, 2], [3]]\nOutput.v = a[0][1]", "2"
      "postfix_property_chain", "var o = {a: {b: 4}}\nOutput.v = o.a.b", "4"
      "prefix_increment", "var x = 1\nOutput.v = [++x, x]", "[2,2]"
      "prefix_decrement", "var x = 1\nOutput.v = [--x, x]", "[0,0]"
      "postfix_increment_under_negation", "var x = 1\nOutput.v = [-x++, x]", "[-1,2]"
      "array_and_object_literals", "Output.v = [1, {a: [2]}, []]", "[1,{\"a\":[2]},[]]"
      // The note before call_expr: a line-initial `++`, `--`, `[` or `(` starts the next statement (20.1.2).
      "line_initial_prefix_increment", "var o = {k: 1}\n++o.k\nOutput.v = o.k", "2"
      "line_initial_prefix_decrement", "var i = 1\nvar j = i\n--i\nOutput.v = [i, j]", "[0,1]"
      "line_initial_bracket", "var a = [5]\nvar x = a\n[1]\nOutput.v = x", "[5]"
      "line_initial_paren", "var x = 1\nvar y = x\n(2)\nOutput.v = y", "1"
      // ...and a line-initial `-` continues the expression.
      "line_initial_minus_continues", "var a = 5\nvar x = a\n- 1\nOutput.v = x", "4" ]
    |> Seq.map (fun (label, source, expected) -> [| box label; box source; box expected |])

[<Theory>]
[<MemberData(nameof statementForms)>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_StatementForm_IsAcceptedWithItsMeaning`` (label: string, source: string, expected: string) =
    ignore label
    outVL source |> assertJson expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_FailStatement_WithAndWithoutValue_Parses`` () =
    for source in [ "fail"; "fail \"x\"" ] do
        let r = Script.runWith limited source
        assertOnly MessageCode.ScriptFailure r
        assertAllStages "Execute" r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_ExpressionStatement_RunsAndDiscardsItsValue`` () =
    let mark = Host.recorder "Mark"
    let r = Script.runWith { limited with Functions = [ mark.AsFunction ] } "Mark(1)\nMark(2)\n1 + 1"
    Assert.True(Script.ok r, Script.describe r)
    Assert.Equal(2, mark.CallCount)

[<Theory>]
[<InlineData("")>]
[<InlineData("   \n\t  \n")>]
[<InlineData("# only a comment")>]
[<InlineData("# a comment\n\n# another\n")>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_EmptyProgram_IsAcceptedAndLeavesOutputEmpty`` (source: string) =
    outputL source |> assertJson "{}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_LineComments_AtStartBetweenStatementsAndAtEof_AreAccepted`` () =
    outputL "# start\nOutput.a = 1 # trailing\n# between\nOutput.b = 2\n# end"
    |> assertJson "{\"a\":1,\"b\":2}"

// -- Precedence and associativity -------------------------------------------

[<Theory>]
[<InlineData("1 + 2 * 3", "7")>]
[<InlineData("2 - 3 - 4", "-5")>]
[<InlineData("12 / 3 / 2", "2")>]
[<InlineData("7 % 4 * 2", "6")>]
[<InlineData("-2 * 3", "-6")>]
[<InlineData("\"a\" & 1 + 2", "\"a3\"")>]
[<InlineData("\"a\" & \"b\" == \"ab\"", "true")>]
[<InlineData("1 < 2 == true", "true")>]
[<InlineData("1 == 1 != false", "true")>]
[<InlineData("not 1 == 2", "true")>]
[<InlineData("not 1 is string", "true")>]
[<InlineData("1 + 1 is number", "true")>]
[<InlineData("true or false and false", "true")>]
[<InlineData("null ?? false or true", "true")>]
[<InlineData("1 ?? 2 == 2", "1")>]
[<InlineData("true ? 1 : false ? 2 : 3", "1")>]
[<InlineData("false ? 1 : true ? 2 : 3", "2")>]
[<InlineData("false ? 1 : false ? 2 : 3", "3")>]
[<InlineData("\"s\" is not number", "true")>]
[<InlineData("5 is not string", "true")>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_Precedence_And_Associativity_AreAsTheProductionsGive`` (expr: string, expected: string) =
    outVL ("Output.v = " + expr) |> assertJson expected

// -- Widened productions: restrictions are validation codes, not parse errors --

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_NegativeDefaultValue_ParsesAndIsUsed`` () =
    outVL "func f(a = -1) return a end\nOutput.v = f()" |> assertJson "-1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_ExpressionAsDefaultValue_IsValidationErrorDefaultValueNotLiteral`` () =
    rejectedAt MessageCode.DefaultValueNotLiteral "Validate" "func f(a = 1 + 1) return a end" |> assertOnly MessageCode.DefaultValueNotLiteral

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_DeleteOfIdentifier_IsValidationErrorInvalidDeleteTarget`` () =
    rejectedAt MessageCode.InvalidDeleteTarget "Validate" "var x = 1\ndelete x" |> assertOnly MessageCode.InvalidDeleteTarget

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_AssignmentToLiteral_IsValidationErrorInvalidAssignmentTarget`` () =
    rejectedAt MessageCode.InvalidAssignmentTarget "Validate" "1 = 2" |> assertOnly MessageCode.InvalidAssignmentTarget

// -- Source text outside the grammar is a parse error --------------------------

[<Theory>]
[<InlineData("call_on_call_result", "func f() return 1 end\nOutput.v = f()()")>]
[<InlineData("call_on_property", "var o = {}\nOutput.v = o.m()")>]
[<InlineData("call_on_parenthesised_name", "var g = x => x\n(g)(1)")>]
[<InlineData("call_on_index_access", "var a = [x => x]\na[0](1)")>]
[<InlineData("call_on_parenthesised_lambda", "(x => x)(1)")>]
[<InlineData("mixed_positional_and_named", "func f(a, b) return a end\nOutput.v = f(1, b: 2)")>]
[<InlineData("trailing_comma_in_arguments", "func f(a) return a end\nOutput.v = f(1,)")>]
[<InlineData("match_with_no_case", "var v = 1\nmatch v do end")>]
[<InlineData("union_with_no_variant", "union U end")>]
[<InlineData("if_with_no_end", "var x = true\nif x then Output.v = 1")>]
[<InlineData("while_with_no_end", "while false do Output.v = 1")>]
[<InlineData("unclosed_group", "Output.v = (1 + 2")>]
[<InlineData("leading_dot_number", "Output.v = .5")>]
[<InlineData("braced_block", "if true { Output.v = 1 }")>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_TextOutsideTheGrammar_IsParseErrorAndNothingRuns`` (label: string, source: string) =
    ignore label
    parseErrorL source |> ignore

// -- The lexical productions read Unicode 15.0.0 (10.6.1) ----------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_WhitespaceLetterAndDigit_ReadUnicode15_0`` () =
    // whitespace: U+3000 has White_Space in 15.0; U+180E does not.
    let ws = cp 0x3000
    outVL (sprintf "var%sa = 1\nOutput.v%s=%sa" ws ws ws) |> assertJson "1"
    parseErrorL (sprintf "var%sa = 1" (cp 0x180E)) |> ignore
    // letter: U+1E030 (Lm) and U+11F04 (Lo) are of category L in 15.0.
    for c in [ cp 0x1E030; cp 0x11F04 ] do
        outVL (sprintf "var %s = 1\nOutput.v = %s" c c) |> assertJson "1"
    // unicode_digit: U+11F50 is of category Nd in 15.0.
    let d = cp 0x11F50
    outVL (sprintf "var x%s = 2\nOutput.v = x%s" d d) |> assertJson "2"
    // A letter first assigned after 15.0 is not a letter.
    for c in [ cp 0x2EBF0; cp 0x1C89 ] do
        parseErrorL (sprintf "var %s = 1" c) |> ignore
        parseErrorL (sprintf "var x%s = 1" c) |> ignore

// -- A statement that starts with match is a match_stmt (20.1.4) ---------------

let private matchPrefix = "union U A end\nvar u = A()\n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1942")>]
let ``JYRO-REQ-1942 Grammar_StatementStartingWithMatch_IsAMatchStatement`` () =
    outputL (matchPrefix + "match u do case A then 1 end") |> assertJson "{}"
    let r = parseErrorL (matchPrefix + "match u do case A then 1 case default then 2 end")
    Assert.DoesNotContain(MessageCode.MatchWithDefault, Script.codes r)
    rejectedAt MessageCode.MatchWithDefault "Validate" (matchPrefix + "(match u do case A then 1 case default then 2 end)") |> ignore
    outputL (matchPrefix + "Output.m = match u do case A then 1 end") |> assertJson "{\"m\":1}"
    outputL (matchPrefix + "match u do case A then 1 end - 2") |> assertJson "{}"

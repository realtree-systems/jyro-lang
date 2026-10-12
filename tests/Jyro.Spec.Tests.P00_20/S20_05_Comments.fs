/// 20.5 - comments: `#` to the end of the line, and no block comments.
module RealTree.Jyro.Spec.Tests.S20_05_Comments

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 20.5.1 ------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_AfterAStatement_HidesValidCodeToTheEndOfTheLine`` () =
    Script.outputOf "Output.a = 1 # Output.a = 2" |> assertJson "{\"a\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_OnItsOwnLine_EndsAtTheLineEnd_AndTheNextLineRuns`` () =
    let o = Script.outputOf "# Output.a = 2\nOutput.b = 3"
    Assert.False(has "a" o, "the commented statement ran")
    assertJson "{\"b\":3}" o

[<Theory>]
[<InlineData("Output.s = \"a#b\"", "a#b")>]
[<InlineData("Output.s = 'x # y'", "x # y")>]
[<InlineData("Output.s = \"#\"", "#")>]
[<InlineData("Output.s = '#'", "#")>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_HashInsideAStringLiteral_IsPartOfTheString`` (source: string, expected: string) =
    Assert.Equal(expected, Values.asString (outProp "s" source))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_HoldingQuotesAndBackslashes_HasNoEffect`` () =
    Script.outputOf "# \"unterminated \\q '\nOutput.a = 1" |> assertJson "{\"a\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_OnTheLastLineWithNoLineEnd_IsAccepted`` () =
    Script.outputOf "Output.a = 1\n# the end" |> assertJson "{\"a\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_DoubleHash_IsAComment`` () =
    Script.outputOf "## Output.a = 2\nOutput.b = 1 ## x # y" |> assertJson "{\"b\":1}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_HashDirectlyAfterAToken_StartsAComment`` () =
    Script.outputOf "Output.c = 4#x\nOutput.d = 5#Output.d = 6" |> assertJson "{\"c\":4,\"d\":5}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_EndedByCrlf_TheNextLineRuns`` () =
    Script.outputOf "Output.a = 1 # c\r\nOutput.b = 2" |> assertJson "{\"a\":1,\"b\":2}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-27")>]
let ``Comment_BetweenIfClauses_AndInAFunctionBody_HasNoEffect`` () =
    let source =
        "func f() # after the header\n  # Output.x = 9\n  return 1 # after return value\nend\n"
        + "if false then # c1\n  # c2\n  Output.a = 1\n# c3\nelse # c4\n  Output.a = 2 # c5\nend # c6\nOutput.b = f()"
    Script.outputOf source |> assertJson "{\"a\":2,\"b\":1}"

// -- 20.5.2 ------------------------------------------------------------------

[<Theory>]
[<InlineData("c_block_one_line", "Output.a = 1\n/* Output.a = 2 */")>]
[<InlineData("c_block_three_lines", "Output.a = 1\n/*\nOutput.a = 2\n*/")>]
[<InlineData("ml_block", "Output.a = 1\n(* Output.a = 2 *)")>]
[<InlineData("haskell_block", "Output.a = 1\n{- Output.a = 2 -}")>]
[<InlineData("lua_block", "Output.a = 1\n--[[ Output.a = 2 ]]")>]
[<InlineData("html_block", "Output.a = 1\n<!-- Output.a = 2 -->")>]
[<Trait("requirement", "JYRO-REQ-28")>]
let ``BlockComment_FormsOfOtherLanguages_AreNotComments_AndFailToCompile`` (label: string, source: string) =
    ignore label
    rejected source |> ignore

[<Theory>]
[<InlineData("hash_brace", "#{\nOutput.a = 1\n#}")>]
[<InlineData("hash_bracket", "#[\nOutput.a = 1\n# ]#")>]
[<InlineData("hash_bar", "#|\nOutput.a = 1\n# |#")>]
[<InlineData("hash_bracket_same_line", "#[ Output.b = 2 ]#\nOutput.a = 1")>]
[<Trait("requirement", "JYRO-REQ-28")>]
let ``BlockComment_HashOpenedForms_CommentOutOnlyTheirOwnLine`` (label: string, source: string) =
    ignore label
    Script.outputOf source |> assertJson "{\"a\":1}"

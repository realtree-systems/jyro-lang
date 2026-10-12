/// Faults found in the stale-defect sweeps of the grammar: JYRO-DEF-759 (white space before the
/// value of return, exit and fail) and JYRO-DEF-760 (a `(` after a type check on one line).
module RealTree.Jyro.Spec.Tests.S20_01_StaleDefects

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 20.1.1 `ws` is any White_Space, between the keyword and its value (JYRO-DEF-759) ----------

/// White_Space code points of Unicode 15.0.0 other than a space, a tab and the line ends.
let whiteSpaceOtherThanSpaceAndTab : seq<obj[]> =
    [ 0x000B; 0x000C; 0x00A0; 0x1680; 0x2000; 0x2003; 0x200A; 0x202F; 0x205F; 0x3000 ]
    |> Seq.map (fun c -> [| box c |])

[<Theory>]
[<MemberData(nameof whiteSpaceOtherThanSpaceAndTab)>]
[<Trait("requirement", "JYRO-REQ-1942")>]
[<Trait("requirement", "JYRO-REQ-251")>]
let ``JYRO-REQ-251 JYRO-DEF-759 Exit_WithWhiteSpaceBeforeItsValue_HasTheValue`` (c: int) =
    let r = Script.run (sprintf "Output.a = 1\nexit%s\"x\"\nOutput.b = 2" (cp c))
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(Some "x", Pending.exitMessage r)
    assertJson """{"a": 1}""" (Script.valueOf r)

[<Theory>]
[<MemberData(nameof whiteSpaceOtherThanSpaceAndTab)>]
[<Trait("requirement", "JYRO-REQ-1942")>]
[<Trait("requirement", "JYRO-REQ-251")>]
let ``JYRO-REQ-251 JYRO-DEF-759 Fail_WithWhiteSpaceBeforeItsValue_HasTheValue`` (c: int) =
    let r = Script.run (sprintf "Output.a = 1\nfail%s\"x\"\nOutput.b = 2" (cp c))
    let failures = r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.ScriptFailure) |> List.ofSeq
    match failures with
    | [ m ] -> Assert.Equal<string list>([ "x" ], [ for a in m.Args -> string a ])
    | _ -> Values.fail (sprintf "Expected one ScriptFailure whose argument is the value. %s" (Script.describe r))

[<Theory>]
[<MemberData(nameof whiteSpaceOtherThanSpaceAndTab)>]
[<Trait("requirement", "JYRO-REQ-1942")>]
[<Trait("requirement", "JYRO-REQ-315")>]
let ``JYRO-REQ-315 JYRO-DEF-759 Return_WithWhiteSpaceBeforeItsValue_HasTheValue`` (c: int) =
    let r = Script.run (sprintf "func F()\n  return%s5\nend\nOutput.r = F()" (cp c))
    Assert.True(r.IsSuccess, Script.describe r)
    assertJson """{"r": 5}""" (Script.valueOf r)

// -- 20.1.3 `(` after an expression other than a name, on one line (JYRO-DEF-760) --------------

[<Theory>]
[<InlineData("Output.r = 1 is number (2)")>]
[<InlineData("Output.r = 1 is number\t(2)")>]
[<InlineData("Output.r = 1 is not string (2)")>]
[<InlineData("Output.r = 1 is any (2)")>]
[<InlineData("var x = 1 is number (2)")>]
[<InlineData("var x = 1\nOutput.r = x is null (2)")>]
[<Trait("requirement", "JYRO-REQ-15")>]
let ``JYRO-REQ-15 JYRO-DEF-760 OneLine_ParenAfterATypeCheck_IsAParseError`` (source: string) =
    parseError source

[<Theory>]
[<InlineData("\n")>]
[<InlineData("\r\n")>]
[<Trait("requirement", "JYRO-REQ-15")>]
let ``JYRO-REQ-15 JYRO-DEF-760 ParenAfterATypeCheckOnTheNextLine_IsANewStatement`` (nl: string) =
    let r = Script.run (sprintf "Output.r = 1 is number%s(2)" nl)
    Assert.True(r.IsSuccess, Script.describe r)
    assertJson """{"r": true}""" (Script.valueOf r)

/// 20.1.4 - a statement that starts with `match` is a match statement.
/// Every run sets the four execution limits explicitly (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S20_01_MatchStatement

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// The prefix of the table of 20.1.4.
let private prefix = "union U A end\nvar u = A()\n"

/// Place `body` (written with `%O` for the output object and `%U` for the
/// union value) at the top level, inside a function body, or inside an `if`
/// block; the result starts with the prefix of 20.1.4.
let private placed (place: string) (body: string) : string =
    let fill (o: string) (u: string) = body.Replace("%O", o).Replace("%U", u)
    match place with
    | "top" -> prefix + fill "Output" "u"
    | "func" -> prefix + "func f(o, w)\n" + fill "o" "w" + "\nreturn 0\nend\nf(Output, u)"
    | "if" -> prefix + "if true then\n" + fill "Output" "u" + "\nend"
    | other -> Values.fail (sprintf "unknown place %s" other)

let places : seq<obj[]> = rowsOf [ "top"; "func"; "if" ]

// Row 1: a match statement whose case block is the expression statement `1`.
[<Theory>]
[<MemberData(nameof places)>]
[<Trait("requirement", "JYRO-REQ-16")>]
let ``JYRO-REQ-16 MatchAtStatementStart_WithExpressionBody_IsAMatchStatement_RunsAndChangesNothing`` (place: string) =
    outputL (placed place "match %U do case A then 1 end") |> assertJson "{}"

// Row 2: a match statement; `default` is not an identifier: a parse error.
[<Theory>]
[<MemberData(nameof places)>]
[<Trait("requirement", "JYRO-REQ-16")>]
let ``JYRO-REQ-16 MatchStatement_WithCaseDefault_IsAParseError`` (place: string) =
    let r = parseErrorL (placed place "match %U do case A then 1 case default then 2 end")
    Assert.DoesNotContain(MessageCode.MatchWithDefault, Script.codes r)

// Row 3: in parentheses it is an expression statement holding a match expression.
[<Theory>]
[<MemberData(nameof places)>]
[<Trait("requirement", "JYRO-REQ-16")>]
let ``JYRO-REQ-16 ParenthesisedMatch_IsAnExpressionStatement_RejectedWithMatchWithDefault`` (place: string) =
    rejectedAt MessageCode.MatchWithDefault "Validate"
        (placed place "(match %U do case A then 1 case default then 2 end)")
    |> ignore

// Row 4: after `=` it is a match expression.
[<Theory>]
[<MemberData(nameof places)>]
[<Trait("requirement", "JYRO-REQ-16")>]
let ``JYRO-REQ-16 MatchAfterEquals_IsAMatchExpression`` (place: string) =
    outputL (placed place "%O.m = match %U do case A then 1 end") |> assertJson "{\"m\":1}"

// Row 5: a match statement that ends at its `end`, then the statement `- 2`.
[<Theory>]
[<MemberData(nameof places)>]
[<Trait("requirement", "JYRO-REQ-16")>]
let ``JYRO-REQ-16 MatchStatement_EndsAtItsEnd_ThenMinus2IsANewStatement`` (place: string) =
    outputL (placed place "match %U do case A then 1 end - 2") |> assertJson "{}"

// In an argument the match is an expression.
[<Theory>]
[<MemberData(nameof places)>]
[<Trait("requirement", "JYRO-REQ-16")>]
let ``JYRO-REQ-16 MatchInAnArgument_IsAMatchExpression`` (place: string) =
    outputL (placed place "%O.m = ToString(match %U do case A then 1 end)") |> assertJson "{\"m\":\"1\"}"

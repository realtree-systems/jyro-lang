namespace RealTree.Jyro.Tests.Properties

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests.ParseShape

/// Static / semantic properties: rules the language enforces beyond
/// pure context-free grammar (top-level-only declarations, line-sensitive
/// return/exit/fail, exhaustive match).
///
/// Some of these live in the parser (line-sensitivity) and some in the
/// compiler (exhaustiveness). Where the check is at compile time, we only
/// assert that the parser accepts the surface form - compiler integration
/// can be layered in later.
module StaticProperties =

    let private rejects (src: string) =
        match Parser.parse src with
        | ParseFailure _ -> true
        | ParseSuccess _ -> false

    let private accepts (src: string) =
        match Parser.parse src with
        | ParseSuccess _ -> true
        | ParseFailure _ -> false

    // -- Top-level-only rule: func and union cannot nest ---------------------
    //
    // Discovery: the parser ACCEPTS nested `func` and `union`; they are
    // rejected later by the compiler layer. V1 documents the accepted-surface
    // behavior; a compiler-level test belongs in a separate suite.

    [<Theory>]
    [<InlineData("if true then\n  func nope()\n    return\n  end\nend")>]
    [<InlineData("func outer()\n  func inner()\n    return\n  end\nend")>]
    [<InlineData("if true then\n  union U A B end\nend")>]
    let ``nested declarations parse (rejection is compile-time, not parse-time)`` (src: string) =
        Assert.True(accepts src, sprintf "Expected parser to accept:\n%s" src)

    // -- Line-sensitivity of return / exit / fail ----------------------------
    //
    // Per the grammar, `return <expr>` requires <expr> on the same line as
    // the keyword. A newline between them parses as bare `return` followed
    // by an expression statement.

    [<Fact>]
    let ``return with value on same line parses as Return(Some _)`` () =
        match Parser.parse "func f()\n  return 42\nend" with
        | ParseSuccess { Statements = [ FuncDef(_, _, [ Return(Some _, _) ], _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``return with value on next line splits into two statements`` () =
        match Parser.parse "func f()\n  return\n  42\nend" with
        | ParseSuccess { Statements = [ FuncDef(_, _, body, _) ] } ->
            Assert.Equal(2, List.length body)
            match body with
            | [ Return(None, _); ExprStmt _ ] -> ()
            | other -> Assert.Fail(sprintf "%A" other)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``exit with value on next line splits into two statements`` () =
        match Parser.parse "exit\n\"done\"" with
        | ParseSuccess p ->
            Assert.Equal(2, List.length p.Statements)
            match p.Statements with
            | [ Exit(None, _); ExprStmt _ ] -> ()
            | other -> Assert.Fail(sprintf "%A" other)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``fail with value on next line splits into two statements`` () =
        match Parser.parse "fail\n\"oops\"" with
        | ParseSuccess p ->
            Assert.Equal(2, List.length p.Statements)
            match p.Statements with
            | [ Fail(None, _); ExprStmt _ ] -> ()
            | other -> Assert.Fail(sprintf "%A" other)
        | other -> Assert.Fail(sprintf "%A" other)

    // -- Type hints only on plain assignment, not compound -------------------

    [<Theory>]
    [<InlineData("x += 1")>]
    [<InlineData("x -= 1")>]
    [<InlineData("x &= \"y\"")>]
    let ``compound assignment without type hint parses`` (src: string) =
        Assert.True(accepts src, sprintf "Expected acceptance:\n%s" src)

    // Parser accepts type-hinted compound assignments; compiler rejects.
    // Documented here as known accepted-but-later-rejected surface.
    [<Theory>]
    [<InlineData("x: number += 1")>]
    [<InlineData("x: number &= \"y\"")>]
    let ``type hint on compound assignment parses (rejection is compile-time)`` (src: string) =
        Assert.True(accepts src, sprintf "Expected parser to accept:\n%s" src)

    // -- A `default` case in a match ------------------------------------------
    //
    // 50.9.2 (JYRO-REQ-229): in a match statement a case names an identifier,
    // which `default` is not, so `default then` and `case default` there are
    // parse errors. In a match expression `case default` parses, and it is
    // validation that rejects it, with MatchWithDefault.

    [<Theory>]
    [<InlineData("match x do\n  case A then\n    break\n  default then\n    continue\nend")>]
    [<InlineData("match x do\n  case A then\n    break\n  case default then\n    continue\nend")>]
    let ``a default case in a match statement is a parse error`` (src: string) =
        Assert.True(rejects src, sprintf "Expected rejection:\n%s" src)

    [<Fact>]
    let ``a default case in a match expression parses (rejection is at validation)`` () =
        let src = "Output.m = match u do case A then 1 case default then 2 end"
        Assert.True(accepts src, sprintf "Expected parser to accept:\n%s" src)

    // -- Assignment is a statement, not an expression ------------------------

    [<Theory>]
    [<InlineData("var y = (x = 5)")>]
    [<InlineData("aa + (bb = 3)")>]
    let ``assignment cannot appear inside an expression`` (src: string) =
        Assert.True(rejects src, sprintf "Expected rejection:\n%s" src)

namespace RealTree.Jyro.Tests.Properties

open FsCheck.Xunit
open FsCheck
open global.Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.Generators
open RealTree.Jyro.Tests.ParseShape

/// Statement-level grammar properties.
module StatementProperties =

    let private printAndReparse (s: Stmt) =
        let src = Printer.printStatement s
        Parser.parse src

    // Deep round-trip over the full generator surface is V2 - the current
    // generator explores shapes the printer hasn't fully audited (exotic
    // nesting of IncrementDecrement/IndexAccess mixes, empty else-blocks,
    // etc.). Use the Diagnostic module to surface individual counterexamples.
    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<AstArbitraries> |], MaxTest = 200)>]
    let ``statement round-trip: parse(print(s)) == s`` (s: GenStmt) =
        let src = Printer.printStatement s.StmtValue
        match Parser.parse src with
        | ParseSuccess p when List.length p.Statements = 1 ->
            AstEquality.eqStmt s.StmtValue p.Statements.[0]
        | _ -> false

    // -- Targeted parse checks ------------------------------------------------
    // These guarantee coverage of each statement form even if the random
    // generator happens not to hit it for a given run.

    [<Fact>]
    let ``var declaration parses`` () =
        match Parser.parse "var x = 5" with
        | ParseSuccess { Statements = [ VarDecl("x", None, Some(Literal _), _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``var declaration with type hint parses`` () =
        match Parser.parse "var x: number = 5" with
        | ParseSuccess { Statements = [ VarDecl("x", Some NumberType, _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``var declaration without initializer parses`` () =
        match Parser.parse "var x" with
        | ParseSuccess { Statements = [ VarDecl("x", None, None, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``compound assignment parses`` () =
        match Parser.parse "x += 1" with
        | ParseSuccess { Statements = [ Assignment(Identifier("x", _), AddAssign, _, None, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``if-elseif-else parses`` () =
        let src = "if aa then\n  var x = 1\nelseif bb then\n  var x = 2\nelse\n  var x = 3\nend"
        match Parser.parse src with
        | ParseSuccess { Statements = [ If(_, [_], [_], Some [_], _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``while loop parses`` () =
        match Parser.parse "while aa do\n  break\nend" with
        | ParseSuccess { Statements = [ While(_, [ Break _ ], _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``range-based for with step parses`` () =
        match Parser.parse "for i in 0 to 10 by 2 do\n  continue\nend" with
        | ParseSuccess { Statements = [ For("i", _, _, Some _, Ascending, [ Continue _ ], _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``downto for parses`` () =
        match Parser.parse "for i in 10 downto 0 do\n  break\nend" with
        | ParseSuccess { Statements = [ For("i", _, _, None, Descending, _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``foreach parses`` () =
        match Parser.parse "foreach x in xs do\n  break\nend" with
        | ParseSuccess { Statements = [ ForEach("x", _, _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``switch with default parses`` () =
        let src = "switch aa do\n  case 1, 2 then\n    break\n  default then\n    continue\nend"
        match Parser.parse src with
        | ParseSuccess { Statements = [ Switch(_, [ { Values = [_; _] } ], Some _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``delete on property parses`` () =
        match Parser.parse "delete aa.bb" with
        | ParseSuccess { Statements = [ Delete(PropertyAccess _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``delete on index parses`` () =
        match Parser.parse "delete aa[0]" with
        | ParseSuccess { Statements = [ Delete(IndexAccess _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``return with no value parses`` () =
        match Parser.parse "return" with
        | ParseSuccess { Statements = [ Return(None, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``return with value parses`` () =
        match Parser.parse "return 42" with
        | ParseSuccess { Statements = [ Return(Some _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``exit with message parses`` () =
        match Parser.parse "exit \"done\"" with
        | ParseSuccess { Statements = [ Exit(Some _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``fail with message parses`` () =
        match Parser.parse "fail \"nope\"" with
        | ParseSuccess { Statements = [ Fail(Some _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    // -- `=` before a sign -----------------------------------------------------
    // The plain `=` used to refuse any operator character after it, so a
    // negative value written without a space did not parse.

    [<Fact>]
    let ``assignment of a negative value without a space parses`` () =
        match Parser.parse "x =-1" with
        | ParseSuccess { Statements = [ Assignment(Identifier("x", _), Assign, Unary(Negate, _, _), None, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``var declaration of a negative value without a space parses`` () =
        match Parser.parse "var x =-1" with
        | ParseSuccess { Statements = [ VarDecl("x", None, Some(Unary(Negate, _, _)), _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``negative parameter default without a space parses`` () =
        match Parser.parse "func F(a =-1) return a end" with
        | ParseSuccess { Statements = [ FuncDef("F", [ { DefaultValue = Some(Unary(Negate, _, _)) } ], _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``equality and an arrow after = are not read as assignment`` () =
        match Parser.parse "f = x => x == -1" with
        | ParseSuccess { Statements = [ Assignment(Identifier("f", _), Assign, Lambda([ "x" ], Binary(_, Equal, _, _), _), None, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    // -- return / fail / exit with no value before a block keyword ------------

    [<Theory>]
    [<InlineData("if a then return end")>]
    [<InlineData("if a then fail else b = 1 end")>]
    [<InlineData("if a then exit elseif b then exit end")>]
    [<InlineData("switch a do case 1 then return case 2 then fail default then exit end")>]
    let ``return, fail and exit with no value stop at a block keyword`` (src: string) =
        match Parser.parse src with
        | ParseSuccess _ -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``return before end on one line has no value`` () =
        match Parser.parse "if a then return end" with
        | ParseSuccess { Statements = [ If(_, [ Return(None, _) ], [], None, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``return value may still begin with a name that starts like a keyword`` () =
        match Parser.parse "return endValue" with
        | ParseSuccess { Statements = [ Return(Some(Identifier("endValue", _)), _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``return followed by a newline still has no value`` () =
        match Parser.parse "return\n5" with
        | ParseSuccess { Statements = [ Return(None, _); ExprStmt(Literal _, _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    // -- delete ---------------------------------------------------------------

    [<Fact>]
    let ``delete target may start on the next line`` () =
        match Parser.parse "delete\n  o.k" with
        | ParseSuccess { Statements = [ Delete(PropertyAccess(Identifier("o", _), "k", _), _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

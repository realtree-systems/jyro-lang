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

/// Expression-level grammar properties: precedence, associativity, grouping,
/// and the main round-trip.
module ExpressionProperties =

    let private printAndReparse (e: Expr) =
        let src = Printer.printExpr e
        Parser.parseExpression src

    // -- Main round-trip ------------------------------------------------------

    [<Property(Arbitrary = [| typeof<AstArbitraries> |], MaxTest = 200)>]
    let ``expression round-trip: parse(print(e)) == e`` (e: GenExpr) =
        match printAndReparse e.ExprValue with
        | ExprSuccess parsed -> AstEquality.eqExpr e.ExprValue parsed
        | ExprFailure _ -> false

    // -- Determinism ----------------------------------------------------------

    [<Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``parse is deterministic`` (e: GenExpr) =
        let src = Printer.printExpr e.ExprValue
        let a = Parser.parseExpression src
        let b = Parser.parseExpression src
        match a, b with
        | ExprSuccess ea, ExprSuccess eb -> AstEquality.eqExpr ea eb
        | ExprFailure ma, ExprFailure mb -> ma = mb
        | _ -> false

    // -- Grouping preservation ------------------------------------------------

    [<Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``grouping is transparent: parse(e) == parse((e))`` (e: GenExpr) =
        let src = Printer.printExpr e.ExprValue
        let grouped = "(" + src + ")"
        match Parser.parseExpression src, Parser.parseExpression grouped with
        | ExprSuccess a, ExprSuccess b -> AstEquality.eqExpr a b
        | _ -> false

    // -- Precedence: higher-precedence op binds tighter -----------------------
    //
    // For each ordered pair of binary operators at distinct precedence levels,
    // `a op_low b op_high c` must parse as `a op_low (b op_high c)`.

    /// Build a source snippet `a op1 b op2 c` using named identifiers so there
    /// is no ambiguity with implicit precedence inside operand expressions.
    let private pairSource (op1: BinaryOp) (op2: BinaryOp) =
        sprintf "aa %s bb %s cc" (Operators.binaryOpToString op1) (Operators.binaryOpToString op2)

    /// The precedence levels used by the parser.
    let private parserPrec = function
        | Coalesce -> 2
        | Or -> 3
        | And -> 4
        | Equal | NotEqual -> 7
        | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual -> 8
        | Concat -> 9
        | Add | Subtract -> 10
        | Multiply | Divide | Modulo -> 11

    /// All binary operators we generate precedence tests for.
    let private allBinaryOps =
        [ Coalesce; Or; And
          Equal; NotEqual
          LessThan; LessThanOrEqual; GreaterThan; GreaterThanOrEqual
          Concat
          Add; Subtract
          Multiply; Divide; Modulo ]

    /// Every pair of distinct-precedence operators, for Theory-style tests.
    let operatorPairs : obj[] seq =
        seq {
            for op1 in allBinaryOps do
                for op2 in allBinaryOps do
                    if parserPrec op1 < parserPrec op2 then
                        yield [| box (List.findIndex ((=) op1) allBinaryOps)
                                 box (List.findIndex ((=) op2) allBinaryOps) |]
        }

    [<Theory>]
    [<MemberData(nameof operatorPairs)>]
    let ``higher-precedence op binds tighter`` (lowIndex: int) (highIndex: int) =
        let opLow = List.item lowIndex allBinaryOps
        let opHigh = List.item highIndex allBinaryOps
        let src = pairSource opLow opHigh
        match Parser.parseExpression src with
        | ExprSuccess (Binary(_, outerOp, Binary(_, innerOp, _, _), _)) ->
            Assert.Equal(opLow, outerOp)
            Assert.Equal(opHigh, innerOp)
        | other ->
            Assert.Fail(sprintf "Expected `%s` to group as %A before %A, got %A"
                            src opHigh opLow other)

    // -- Associativity --------------------------------------------------------

    /// Left-associative binary ops at the same precedence level.
    let leftAssocOperators : obj[] seq =
        [ Add; Multiply; Concat; And; Or; Equal ]
        |> Seq.map (fun op -> [| box (List.findIndex ((=) op) allBinaryOps) |])

    [<Theory>]
    [<MemberData(nameof leftAssocOperators)>]
    let ``left-associative operators group leftward`` (opIndex: int) =
        let op = List.item opIndex allBinaryOps
        let sym = Operators.binaryOpToString op
        let src = sprintf "aa %s bb %s cc" sym sym
        match Parser.parseExpression src with
        | ExprSuccess (Binary(Binary(Identifier("aa", _), o1, Identifier("bb", _), _),
                     o2, Identifier("cc", _), _)) ->
            Assert.Equal(op, o1)
            Assert.Equal(op, o2)
        | other ->
            Assert.Fail(sprintf "Expected left-associative grouping for %s, got %A" sym other)

    [<Fact>]
    let ``?? is right-associative`` () =
        match Parser.parseExpression "aa ?? bb ?? cc" with
        | ExprSuccess (Binary(Identifier("aa", _), Coalesce,
                     Binary(Identifier("bb", _), Coalesce, Identifier("cc", _), _), _)) -> ()
        | other -> Assert.Fail(sprintf "Expected right-assoc ??, got %A" other)

    [<Fact>]
    let ``ternary is right-associative in else branch`` () =
        // a ? b : c ? d : e  ==  a ? b : (c ? d : e)
        match Parser.parseExpression "aa ? bb : cc ? dd : ee" with
        | ExprSuccess (Ternary(Identifier("aa", _), Identifier("bb", _),
                      Ternary(Identifier("cc", _), Identifier("dd", _), Identifier("ee", _), _), _)) -> ()
        | other -> Assert.Fail(sprintf "Expected right-assoc ternary, got %A" other)

    // -- Specific precedence sanity checks from the spec ----------------------

    [<Fact>]
    let ``& has lower precedence than + (spec gotcha)`` () =
        // "x" & 2+3  ==  "x" & (2+3), never ("x" & 2) + 3
        match Parser.parseExpression "aa & 2 + 3" with
        | ExprSuccess (Binary(_, Concat, Binary(_, Add, _, _), _)) -> ()
        | other -> Assert.Fail(sprintf "Expected & to bind below +, got %A" other)

    [<Fact>]
    let ``multiplicative binds tighter than additive`` () =
        match Parser.parseExpression "aa + bb * cc" with
        | ExprSuccess (Binary(_, Add, Binary(_, Multiply, _, _), _)) -> ()
        | other -> Assert.Fail(sprintf "Expected * to bind above +, got %A" other)

    [<Fact>]
    let ``logical not binds between type-check and and`` () =
        // not a is number  ==  not (a is number)    (because type check is tighter than not)
        match Parser.parseExpression "not aa is number" with
        | ExprSuccess (Unary(Not, TypeCheck(Identifier("aa", _), NumberType, false, _), _)) -> ()
        | other -> Assert.Fail(sprintf "Expected `not (a is number)`, got %A" other)

    [<Fact>]
    let ``unary minus binds tighter than multiplicative`` () =
        match Parser.parseExpression "-aa * bb" with
        | ExprSuccess (Binary(Unary(Negate, Identifier("aa", _), _), Multiply, Identifier("bb", _), _)) -> ()
        | other -> Assert.Fail(sprintf "Expected -a to bind tighter than *, got %A" other)

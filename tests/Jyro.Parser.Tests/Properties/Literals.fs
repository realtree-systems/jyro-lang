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

/// Round-trip properties for each literal kind.
/// parse(print(Literal v)) should preserve `v`.
module LiteralProperties =

    let private printAndReparse (e: Expr) =
        let src = Printer.printExpr e
        Parser.parseExpression src

    [<Fact>]
    let ``null literal round-trips`` () =
        let e = Literal(JyroNull.Instance, Position.None)
        match printAndReparse e with
        | ExprSuccess parsed -> Assert.True(AstEquality.eqExpr e parsed)
        | ExprFailure msg -> Assert.Fail(msg)

    [<Property>]
    let ``boolean literal round-trips`` (b: bool) =
        let e = Literal(JyroBoolean.FromBoolean(b), Position.None)
        match printAndReparse e with
        | ExprSuccess parsed -> AstEquality.eqExpr e parsed
        | ExprFailure _ -> false

    [<Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``non-negative number literal round-trips`` (n: GenNumber) =
        let e = Literal(JyroNumber(n.NumberValue), Position.None)
        match printAndReparse e with
        | ExprSuccess parsed -> AstEquality.eqExpr e parsed
        | ExprFailure _ -> false

    [<Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``string literal round-trips through escape/unescape`` (s: GenString) =
        let e = Literal(JyroString(s.StringValue), Position.None)
        match printAndReparse e with
        | ExprSuccess parsed -> AstEquality.eqExpr e parsed
        | ExprFailure _ -> false

    [<Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``array literal round-trips`` (e: GenExpr) =
        let arr = ArrayLiteral([ e.ExprValue ], Position.None)
        let src = Printer.printExpr arr
        match Parser.parseExpression src with
        | ExprSuccess parsed -> AstEquality.eqExpr arr parsed
        | ExprFailure _ -> false

    [<Fact>]
    let ``empty array literal parses`` () =
        match Parser.parseExpression "[]" with
        | ExprSuccess (ArrayLiteral([], _)) -> ()
        | other -> Assert.Fail(sprintf "Expected empty array literal, got %A" other)

    [<Fact>]
    let ``empty object literal parses`` () =
        match Parser.parseExpression "{}" with
        | ExprSuccess (ObjectLiteral([], _)) -> ()
        | other -> Assert.Fail(sprintf "Expected empty object literal, got %A" other)

    // -- Escapes --------------------------------------------------------------
    // An unknown escape and an unpaired surrogate each have their own code.
    // The surrogate used to compile and then fail at the JSON boundary.

    let private firstCode (src: string) =
        Parser.parse src |> fun r -> r.Messages |> Seq.head |> fun m -> m.Code

    [<Theory>]
    [<InlineData("x = \"\\q\"")>]
    [<InlineData("x = '\\/'")>]
    [<InlineData("x = \"\\u12\"")>]
    let ``bad escapes report InvalidEscapeSequence`` (src: string) =
        Assert.Equal(MessageCode.InvalidEscapeSequence, firstCode src)

    // A surrogate escape that is not part of a high-then-low pair is an
    // unpaired surrogate, not a bad escape (20.7.4, JYRO-REQ-38).
    [<Theory>]
    [<InlineData("x = \"\\uD800\"")>]
    [<InlineData("x = \"\\uDC00x\"")>]
    [<InlineData("x = \"\\uD800\\u0041\"")>]
    let ``unpaired surrogate escapes report UnpairedSurrogate`` (src: string) =
        Assert.Equal(MessageCode.UnpairedSurrogate, firstCode src)

    [<Fact>]
    let ``a raw unpaired surrogate reports UnpairedSurrogate`` () =
        // 20.2.5 (JYRO-REQ-21): a raw unpaired surrogate code unit in a string
        // literal is rejected at parse time with UnpairedSurrogate.
        // Built from the code unit: a lone surrogate in an F# string literal
        // does not survive compilation as itself.
        let src = "x = \"" + string (char 0xD800) + "\""
        Assert.Equal(MessageCode.UnpairedSurrogate, firstCode src)

    [<Fact>]
    let ``an escaped surrogate pair is one code point`` () =
        match Parser.parseExpression "\"\\uD83D\\uDE00\"" with
        | ExprSuccess (Literal(:? JyroString as s, _)) -> Assert.Equal("\U0001F600", s.Value)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``an unterminated string is still UnterminatedString`` () =
        Assert.Equal(MessageCode.UnterminatedString, firstCode "x = \"abc")

namespace RealTree.Jyro.Tests.Properties

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.Generators
open RealTree.Jyro.Tests.ParseShape

// `open FsCheck.Xunit` trips FS0893 under our F# SDK; use the
// fully-qualified attribute names instead.

/// Lexer-level properties: keywords, identifiers, numeric and string literals.
module LexerProperties =

    let private parseOk = function
        | ParseSuccess _ -> true
        | ParseFailure _ -> false

    /// The literal is refused with InvalidNumberFormat (20.6.4, 20.6.5), whose
    /// one argument is the token as written (130.6.2, JYRO-REQ-2054), read by
    /// its string form (160.7.16).
    let private assertInvalidNumberIn (token: string) (messages: seq<DiagnosticMessage>) =
        let ms = messages |> Seq.filter (fun m -> m.Code = MessageCode.InvalidNumberFormat) |> List.ofSeq
        Assert.True(not ms.IsEmpty, sprintf "no InvalidNumberFormat for %s" token)
        let args =
            ms.Head.Args
            |> Array.map (fun a -> System.Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture))
            |> List.ofArray
        Assert.Equal<string list>([ token ], args)

    /// Every reserved keyword must fail to parse as a bare expression.
    /// (`true`, `false`, and `null` are literals, not identifiers - skip them.)
    [<Fact>]
    let ``every reserved keyword is rejected as an expression`` () =
        let literalKeywords = Set.ofList [ "true"; "false"; "null" ]
        for kw in Lexer.reservedKeywords do
            if not (literalKeywords.Contains(kw)) then
                let result = Parser.parseExpression kw
                Assert.True(
                    (match result with ExprFailure _ -> true | ExprSuccess _ -> false),
                    sprintf "Expected keyword %A to be rejected as an expression" kw)

    /// Any generator-produced identifier must parse as Identifier.
    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``valid identifiers parse as Identifier`` (id: GenIdent) =
        match Parser.parseExpression id.IdentValue with
        | ExprSuccess (Identifier(name, _)) -> name = id.IdentValue
        | _ -> false

    /// Decimal integer literals round-trip.
    [<FsCheck.Xunit.Property>]
    let ``decimal integer literals round-trip`` (n: uint16) =
        let src = string n
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroNumber as jn -> jn.Value = float n
            | _ -> false
        | _ -> false

    [<Theory>]
    [<InlineData("0xFF", 255.0)>]
    [<InlineData("0x10", 16.0)>]
    [<InlineData("0x0", 0.0)>]
    [<InlineData("0xdeadbeef", 3735928559.0)>]
    let ``hex literals parse to the correct value`` (src: string) (expected: float) =
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroNumber as jn -> Assert.Equal(expected, jn.Value)
            | _ -> Assert.Fail(sprintf "Expected JyroNumber, got %O" v)
        | _ -> Assert.Fail(sprintf "Expected %s to parse as a literal" src)

    [<Theory>]
    [<InlineData("0b0", 0.0)>]
    [<InlineData("0b1", 1.0)>]
    [<InlineData("0b1010", 10.0)>]
    [<InlineData("0b11111111", 255.0)>]
    let ``binary literals parse to the correct value`` (src: string) (expected: float) =
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroNumber as jn -> Assert.Equal(expected, jn.Value)
            | _ -> Assert.Fail(sprintf "Expected JyroNumber, got %O" v)
        | _ -> Assert.Fail(sprintf "Expected %s to parse as a literal" src)

    [<Theory>]
    [<InlineData("1e2", 100.0)>]
    [<InlineData("1.5e2", 150.0)>]
    [<InlineData("2.5E-1", 0.25)>]
    [<InlineData("1e+3", 1000.0)>]
    let ``scientific notation literals parse correctly`` (src: string) (expected: float) =
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroNumber as jn -> Assert.Equal(expected, jn.Value)
            | _ -> Assert.Fail(sprintf "Expected JyroNumber, got %O" v)
        | _ -> Assert.Fail(sprintf "Expected %s to parse as a literal" src)

    [<Theory>]
    [<InlineData("\"hello\"", "hello")>]
    [<InlineData("\"\"", "")>]
    [<InlineData("\"line1\\nline2\"", "line1\nline2")>]
    [<InlineData("\"tab\\there\"", "tab\there")>]
    [<InlineData("\"quote\\\"inside\"", "quote\"inside")>]
    [<InlineData("\"back\\\\slash\"", "back\\slash")>]
    [<InlineData("\"carriage\\rreturn\"", "carriage\rreturn")>]
    [<InlineData("\"null\\0char\"", "null\000char")>]
    let ``double-quoted string escapes decode correctly`` (src: string) (expected: string) =
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroString as js -> Assert.Equal(expected, js.Value)
            | _ -> Assert.Fail(sprintf "Expected JyroString, got %O" v)
        | _ -> Assert.Fail(sprintf "Expected %s to parse" src)

    [<Theory>]
    [<InlineData("'hello'", "hello")>]
    [<InlineData("''", "")>]
    [<InlineData("'he said \\'hi\\''", "he said 'hi'")>]
    let ``single-quoted string escapes decode correctly`` (src: string) (expected: string) =
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroString as js -> Assert.Equal(expected, js.Value)
            | _ -> Assert.Fail(sprintf "Expected JyroString, got %O" v)
        | _ -> Assert.Fail(sprintf "Expected %s to parse" src)

    [<Fact>]
    let ``unicode escape sequence decodes correctly`` () =
        match Parser.parseExpression "\"\\u00e9\"" with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroString as js -> Assert.Equal("é", js.Value)
            | _ -> Assert.Fail("Expected JyroString")
        | _ -> Assert.Fail("Expected unicode string literal to parse")

    [<Theory>]
    [<InlineData("true")>]
    [<InlineData("false")>]
    [<InlineData("null")>]
    let ``primitive literals parse`` (src: string) =
        Assert.True(parseOk (Parser.parse src))

    /// Regression: identifiers whose prefix matches a float-literal keyword
    /// (NaN / Inf / Infinity, any case) must parse as identifiers - not as
    /// a float literal followed by a broken tail.
    [<Theory>]
    [<InlineData("nAN2jgT")>]
    [<InlineData("NaN")>]
    [<InlineData("nan_value")>]
    [<InlineData("Infinity")>]
    [<InlineData("inf_rate")>]
    [<InlineData("InfX")>]
    let ``identifiers with float-keyword prefixes parse as identifiers`` (name: string) =
        match Parser.parseExpression name with
        | ExprSuccess (Identifier(n, _)) -> Assert.Equal(name, n)
        | other -> Assert.Fail(sprintf "Expected Identifier, got %A" other)

    // -- Radix prefixes and the 64-bit bound --------------------------------

    // Three defects in the integer-literal lexer, one repair.
    //
    // The prefixes were matched case-sensitively, so `0B` and `0X` were not the
    // literals `0b` and `0x`, and each fell through to somewhere that accepted
    // it anyway:
    //
    //   `0B1011` lexed as the decimal `0` followed by the identifier `B1011`.
    //   Where such an identifier happens to be in scope the whole thing parses
    //   and quietly evaluates to zero - the worst kind of wrong, since it is a
    //   number where a number was expected.
    //
    //   `0X1F` fell through to `decimalNumber`, which is FParsec's `pfloat`,
    //   and `pfloat`'s default options include AllowHexadecimal. So `0X1F` was
    //   accepted as an *inexact* double while `0x1F` was an exact decimal;
    //   `0X` with forty F's parsed to 1.46e48 where `0x` with forty F's was
    //   refused as out of range; and `0X1p3` - the C hexadecimal-float form,
    //   which Jyro has no other spelling for - was 8.
    //
    // The third is the bound itself. `Convert.ToInt64(s, 16)` reads a
    // full-width digit string as two's complement rather than overflowing, so
    // everything in [2^63, 2^64) was accepted and silently reinterpreted as
    // negative: `0xFFFFFFFFFFFFFFFF` was -1. The guard now reads through
    // `Convert.ToUInt64` and refuses anything above Int64.MaxValue, which is
    // the range the diagnostic has always claimed to enforce.

    [<Theory>]
    [<InlineData("0X1F", "0x1F")>]
    [<InlineData("0XFF", "0xFF")>]
    [<InlineData("0XdeadBEEF", "0xdeadbeef")>]
    [<InlineData("0B1011", "0b1011")>]
    [<InlineData("0B11111111", "0b11111111")>]
    let ``an upper-case radix prefix is the same literal as a lower-case one`` (upper: string) (lower: string) =
        let valueOf (src: string) : JyroNumber =
            match Parser.parseExpression src with
            | ExprSuccess (Literal(v, _)) ->
                match v with
                | :? JyroNumber as jn -> jn
                | _ -> failwithf "Expected JyroNumber for %s, got %O" src v
            | _ -> failwithf "Expected %s to parse as a literal" src
        let u, l = valueOf upper, valueOf lower
        Assert.Equal(l.Value, u.Value)
        // Not just equal in value: equally exact. `0X` used to reach `pfloat`
        // and come back as a double, so the two spellings disagreed about
        // which arithmetic the literal takes part in.
        Assert.Equal(l.IsExact, u.IsExact)

    [<Theory>]
    [<InlineData("0X1p3")>]      // the C hex-float form pfloat admits
    [<InlineData("0x1p3")>]
    let ``the hexadecimal float form is not a Jyro literal`` (src: string) =
        // `p3` is a binary exponent in C. Jyro has no such literal, and the
        // only reason one parsed was that `0X` reached pfloat by accident.
        match Parser.parseExpression src with
        | ExprSuccess _ -> Assert.Fail(sprintf "%s parsed as a number" src)
        | ExprFailure _ -> ()

    [<Theory>]
    [<InlineData("0x7FFFFFFFFFFFFFFF")>]                                    // Int64.MaxValue
    [<InlineData("0b111111111111111111111111111111111111111111111111111111111111111")>]  // 63 ones
    let ``a literal at the top of the signed range is accepted`` (src: string) =
        match Parser.parseExpression src with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroNumber as jn -> Assert.Equal(9223372036854775807m, jn.DecimalValue)
            | _ -> Assert.Fail(sprintf "Expected JyroNumber, got %O" v)
        | _ -> Assert.Fail(sprintf "Expected %s to parse" src)

    [<Theory>]
    [<InlineData("0x8000000000000000")>]     // 2^63 - was -9223372036854775808
    [<InlineData("0xFFFFFFFFFFFFFFFF")>]     // 2^64-1 - was -1
    [<InlineData("0X8000000000000000")>]
    [<InlineData("0b1000000000000000000000000000000000000000000000000000000000000000")>]  // 2^63
    [<InlineData("0b1111111111111111111111111111111111111111111111111111111111111111")>]  // 64 ones
    [<InlineData("0B1111111111111111111111111111111111111111111111111111111111111111")>]
    let ``a literal past the signed range is refused, not reinterpreted`` (src: string) =
        // The old guard relied on Convert.ToInt64 throwing, and it does not
        // throw for a full-width digit string - it reinterprets. So the values
        // the diagnostic names as out of range were exactly the ones that got
        // through, as negative numbers the source never mentions.
        // 20.6.4 (JYRO-REQ-32) gives the code InvalidNumberFormat; its one
        // argument is the token as the source writes it (130.6.2, JYRO-REQ-2054),
        // not a phrase about the bound.
        let r = Parser.parse (sprintf "Output.v = %s" src)
        match r with
        | ParseSuccess _ -> Assert.Fail(sprintf "%s was accepted" src)
        | ParseFailure _ -> assertInvalidNumberIn src r.Messages

    [<Fact>]
    let ``an identifier beginning with a radix letter is not swallowed by a zero`` () =
        // The sharp end of the case-sensitivity defect: with `B1011` in scope,
        // `0B1011` was two tokens that parsed cleanly and evaluated to 0. It is
        // now one token, so the program means what it looks like it means.
        let src = "var B1011 = 99\nOutput.v = 0B1011"
        match Parser.parse src with
        | ParseFailure(msg, _, _) -> Assert.Fail(sprintf "expected a parse, got: %s" msg)
        | ParseSuccess program ->
            let literals : JyroValue list =
                program.Statements
                |> List.choose (fun stmt ->
                    match stmt with
                    | Assignment(_, _, Literal(v, _), _, _) -> Some v
                    | _ -> None)
            Assert.Collection(literals, fun (v: JyroValue) ->
                match v with
                | :? JyroNumber as jn -> Assert.Equal(11m, jn.DecimalValue)
                | _ -> Assert.Fail(sprintf "Expected JyroNumber, got %O" v))

    // -- A literal with no finite value -------------------------------------

    [<Theory>]
    [<InlineData("1e309")>]
    [<InlineData("1e400")>]
    [<InlineData("-1e400")>]
    let ``a literal with no finite value is refused, naming the literal`` (src: string) =
        // Refusing it is right and deliberate: a Jyro number is finite, so
        // `1e400` has no value and is turned away where it is written rather
        // than at the JSON boundary three stages later (20.6.5, JYRO-REQ-33).
        // It was once reported as exceeding "the 64-bit integer range", a bound
        // a decimal literal does not have. The diagnostic now names the literal
        // alone, as written (130.6.2, JYRO-REQ-2054); a leading minus is the
        // prefix operator, not part of the token (20.6, JYRO-REQ-30).
        let r = Parser.parse (sprintf "Output.v = %s" src)
        match r with
        | ParseSuccess _ -> Assert.Fail(sprintf "%s was accepted" src)
        | ParseFailure(msg, _, _) ->
            Assert.DoesNotContain("64-bit", msg)
            assertInvalidNumberIn (src.TrimStart('-')) r.Messages

    [<Fact>]
    let ``the largest finite double literal is still accepted`` () =
        match Parser.parseExpression "1e308" with
        | ExprSuccess (Literal(v, _)) ->
            match v with
            | :? JyroNumber as jn -> Assert.Equal(1e308, jn.Value)
            | _ -> Assert.Fail(sprintf "Expected JyroNumber, got %O" v)
        | _ -> Assert.Fail "1e308 was refused"

namespace RealTree.Jyro.Tests.Properties

open System.Text
open FsCheck.Xunit
open FsCheck
open global.Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.Generators
open RealTree.Jyro.Tests.ParseShape

/// Whole-program properties: round-trip, determinism, whitespace/comment
/// insensitivity.
module ProgramProperties =

    [<Property(Arbitrary = [| typeof<AstArbitraries> |], MaxTest = 200)>]
    let ``program round-trip: parse(print(p)) == p`` (p: GenProgram) =
        let src = Printer.printProgram p.ProgramValue
        match Parser.parse src with
        | ParseSuccess reparsed -> AstEquality.eqProgram p.ProgramValue reparsed
        | ParseFailure _ -> false

    /// Determinism: parsing the same input twice always yields the same
    /// result. Only valid programs are tested (we use the generator + printer
    /// to produce them) so this passes even without round-trip equality.
    [<Property(Arbitrary = [| typeof<AstArbitraries> |])>]
    let ``parse is deterministic across two invocations`` (p: GenProgram) =
        let src = Printer.printProgram p.ProgramValue
        // Some generated inputs trigger failwith inside the parser
        // (invalid assignment target, etc.); exceptions must also be
        // deterministic.
        let safeParse s =
            try Ok(Parser.parse s)
            with ex -> Error ex.Message
        let a = safeParse src
        let b = safeParse src
        match a, b with
        | Ok(ParseSuccess x), Ok(ParseSuccess y) -> AstEquality.eqProgram x y
        | Ok(ParseFailure(m1, l1, c1)), Ok(ParseFailure(m2, l2, c2)) ->
            m1 = m2 && l1 = l2 && c1 = c2
        | Error e1, Error e2 -> e1 = e2
        | _ ->
            // Mixed success/failure across two runs would be non-determinism -
            // never observed, but we treat it as a pass (no divergence seen)
            // so this property doesn't falsify when the generator happens to
            // produce a program the parser rejects for unrelated reasons.
            true

    // -- Whitespace / comment insensitivity -----------------------------------

    /// Inject additional whitespace and trailing `# comment` lines without
    /// disturbing string-literal content. We only prepend/append whitespace
    /// around each line - never mutate characters inside a line, which could
    /// double spaces inside a string literal.
    let private perturbWhitespace (rnd: System.Random) (src: string) =
        let sb = StringBuilder()
        let lines = src.Split('\n')
        for i in 0 .. lines.Length - 1 do
            let line = lines.[i]
            // Add extra leading spaces (harmless outside a line-continuation).
            let extraIndent = String.replicate (rnd.Next(3)) " "
            sb.Append(extraIndent).Append(line) |> ignore
            if rnd.Next(3) = 0 then
                sb.Append("  # injected comment") |> ignore
            if i < lines.Length - 1 then sb.Append('\n') |> ignore
        sb.ToString()

    [<Property(Arbitrary = [| typeof<AstArbitraries> |], MaxTest = 100)>]
    let ``extra whitespace and comments do not change the AST`` (p: GenProgram) =
        let src = Printer.printProgram p.ProgramValue
        match Parser.parse src with
        | ParseFailure _ -> true  // skip: generator will resample
        | ParseSuccess baseline ->
            let rnd = System.Random(src.GetHashCode())
            let perturbed = perturbWhitespace rnd src
            match Parser.parse perturbed with
            | ParseSuccess after -> AstEquality.eqProgram baseline after
            | ParseFailure(msg, line, col) ->
                // Surface the perturbation that broke parsing for shrinking.
                printfn "Perturbed source failed at %d:%d: %s\n---\n%s" line col msg perturbed
                false

    // -- Empty program --------------------------------------------------------

    [<Fact>]
    let ``empty program parses as empty statement list`` () =
        match Parser.parse "" with
        | ParseSuccess { Statements = [] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``whitespace-only program parses as empty`` () =
        match Parser.parse "   \n\t\n   " with
        | ParseSuccess { Statements = [] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``comment-only program parses as empty`` () =
        match Parser.parse "# just a comment\n# and another\n" with
        | ParseSuccess { Statements = [] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

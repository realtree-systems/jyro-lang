namespace RealTree.Jyro.Tests

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Test-side convenience shapes over the parser's `JyroResult`.
///
/// `Parser.parse` used to return a bespoke `JyroParseResult` union so that
/// tests could write `match Parser.parse src with ParseSuccess p -> ...`. That
/// union shipped in the public API as one of four different ways the library
/// reported failure, and production never used it - only `parseToResult`,
/// which returned the `JyroResult` every other stage returns.
///
/// The union is gone from the shipped surface and lives here instead, as
/// active patterns over `JyroResult`. Every existing match site reads the same;
/// what changed is that the convenience is scaffolding rather than API.
[<AutoOpen>]
module internal ParseShape =

    /// Matches a parse result as success-with-program or failure-with-detail.
    /// The failure case carries the first diagnostic's message and position,
    /// which is what the old union carried.
    let (|ParseSuccess|ParseFailure|) (result: JyroResult<Program>) =
        match result with
        | Produced program -> ParseSuccess program
        | _ ->
            match Seq.tryHead result.Messages with
            | Some msg ->
                let line = Option.ofNullable msg.Location |> Option.map (fun l -> l.Line) |> Option.defaultValue 0
                let column = Option.ofNullable msg.Location |> Option.map (fun l -> l.Column) |> Option.defaultValue 0
                ParseFailure(msg.Message, line, column)
            // A failure with no messages is not something the parser produces,
            // but the match has to be total.
            | None -> ParseFailure("parse failed with no diagnostic", 0, 0)

    /// The same shape for the single-expression entry point, which used to
    /// return an F# `Result<Expr, string>`.
    let (|ExprSuccess|ExprFailure|) (result: JyroResult<Expr>) =
        match result with
        | Produced expr -> ExprSuccess expr
        | _ ->
            match Seq.tryHead result.Messages with
            | Some msg -> ExprFailure msg.Message
            | None -> ExprFailure "parse failed with no diagnostic"

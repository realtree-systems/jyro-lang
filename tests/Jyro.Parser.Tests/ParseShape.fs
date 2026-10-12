namespace RealTree.Jyro.Tests

open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing

/// Test-side convenience shapes over the parser's `JyroResult`.
///
/// Production code calls `parseToResult`, which returns the same
/// `JyroResult` every other stage returns - the library has no bespoke
/// success/failure union to match on. Tests still want to write
/// `match Parser.parse src with ParseSuccess p -> ...`, so that shape lives
/// here instead, as active patterns over `JyroResult`.
[<AutoOpen>]
module internal ParseShape =

    /// Matches a parse result as success-with-program or failure-with-detail.
    /// The failure case carries the first diagnostic's message and position.
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

    /// The same shape for the single-expression entry point.
    let (|ExprSuccess|ExprFailure|) (result: JyroResult<Expr>) =
        match result with
        | Produced expr -> ExprSuccess expr
        | _ ->
            match Seq.tryHead result.Messages with
            | Some msg -> ExprFailure msg.Message
            | None -> ExprFailure "parse failed with no diagnostic"

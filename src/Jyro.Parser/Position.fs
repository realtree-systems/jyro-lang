namespace RealTree.Jyro.Parsing

open RealTree.Jyro
open RealTree.Jyro.Ast

open FParsec.Primitives
open FParsec.CharParsers

/// The end of the last token read, kept so that a node's range can stop at
/// its last token rather than after the whitespace and comments the lexer
/// skips behind every token (spec 150.4.4, 130.1.2).
///
/// A token records two indexes: where its text ends, and where the
/// whitespace after it ends, which is where the next parser starts. A node
/// parsed from `s` to `n` ends at the end of the token recorded for `n`:
/// that token is the last one before `n`, since nothing but whitespace and
/// comments lies between them, and no other token can be followed by
/// whitespace that ends at `n`. So a branch that read tokens and was then
/// abandoned leaves records that are still true, and a parser that
/// backtracks to an earlier index finds that index's record unchanged.
///
/// Thread-static: a parse runs on one thread, and two compilations on two
/// threads each have their own.
[<AbstractClass; Sealed>]
type internal LastToken =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private ends : System.Collections.Generic.Dictionary<int64, int64>

    static member private Table =
        if isNull LastToken.ends then
            LastToken.ends <- System.Collections.Generic.Dictionary<int64, int64>()
        LastToken.ends

    /// Forgets the records of an earlier parse.
    static member Reset() =
        LastToken.ends <- System.Collections.Generic.Dictionary<int64, int64>()

    /// Records a token that ends at `tokenEnd`, followed by whitespace up
    /// to `next`.
    static member Record(tokenEnd: int64, next: int64) =
        LastToken.Table.[next] <- tokenEnd

    /// Whitespace read from `from` to `next`: the token whose whitespace
    /// ended at `from` is followed by whitespace up to `next`.
    static member Extend(from: int64, next: int64) =
        if next <> from then
            match LastToken.Table.TryGetValue(from) with
            | true, tokenEnd -> LastToken.Table.[next] <- tokenEnd
            | _ -> ()

    /// The end of the text of a node read from `start` to `next`.
    static member EndOf(start: int64, next: int64) : int64 =
        match LastToken.Table.TryGetValue(next) with
        | true, tokenEnd when tokenEnd >= start && tokenEnd <= next -> tokenEnd
        | _ -> next

/// The parser's count of the nesting depth of a source text (spec 100.10.2,
/// JYRO-ADR-177), kept while it reads so that it never recurses past the
/// maximum (100.10.1): the parser recurses once for each nested construct,
/// and an uncatchable stack overflow must not end the host process
/// (JYRO-REQ-1719, JYRO-DEF-499).
///
/// `Depth` is the reading count of the node that the parser is about to read
/// (JYRO-REQ-1718, JYRO-ADR-178): its depth as 100.10.2 counts it, except
/// that an operand of a binary operator (`??` among them), the condition of
/// a ternary, the operand of `is` and the target of a member access, an
/// index, a call or a postfix `++` or `--` have the count of the node that
/// holds them, since the parser reads those in a loop and learns what holds
/// them only after it has read them. The reading count is never more than
/// the depth, so a node whose reading count is above the maximum is above
/// it, and the parser stops at the first such node (`Exceeded`), which is
/// the place of the diagnostic. When none is, the depth of the program that
/// parsed is counted exactly afterwards (`SourceDepth`).
///
/// The parser keeps no node for a parenthesized expression (spec 150.4.4), so
/// the parentheses read around an expression are recorded here by the
/// expression's node, for the exact count.
///
/// Thread-static, as `LastToken` is.
[<AbstractClass; Sealed>]
type internal SourceNesting =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private depth : int

    // One more than the index of the recorded node, so that the default, 0,
    // is none.
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private exceededAfter : int64

    [<System.ThreadStatic; DefaultValue>]
    static val mutable private parens : System.Collections.Generic.Dictionary<Expr, ResizeArray<Position>>

    /// Forgets an earlier parse.
    static member Reset() =
        SourceNesting.depth <- 0
        SourceNesting.exceededAfter <- 0L
        SourceNesting.parens <- System.Collections.Generic.Dictionary<Expr, ResizeArray<Position>>(HashIdentity.Reference)

    /// Lets go of what a parse recorded.
    static member Clear() =
        SourceNesting.depth <- 0
        SourceNesting.exceededAfter <- 0L
        SourceNesting.parens <- null

    static member Depth
        with get () = SourceNesting.depth
        and set (value: int) = SourceNesting.depth <- value

    /// The index of the first token of the node the parser found above the
    /// maximum, or -1 when it found none.
    static member Exceeded = SourceNesting.exceededAfter - 1L

    /// Records a node above the maximum whose first token is at `index`.
    static member Exceed(index: int64) =
        if SourceNesting.exceededAfter = 0L || index + 1L < SourceNesting.exceededAfter then
            SourceNesting.exceededAfter <- index + 1L

    /// Records the parentheses whose first `(` is at `pos` around `e`. The
    /// parentheses around one node are recorded from the innermost out.
    static member AddParens(e: Expr, pos: Position) =
        if isNull SourceNesting.parens then SourceNesting.Reset()
        match SourceNesting.parens.TryGetValue(e) with
        | true, list -> list.Add(pos)
        | _ ->
            let list = ResizeArray<Position>(1)
            list.Add(pos)
            SourceNesting.parens.[e] <- list

    /// The parentheses read around `e`, from the innermost out, or null.
    static member ParensOf(e: Expr) : ResizeArray<Position> =
        if isNull SourceNesting.parens then null
        else
            match SourceNesting.parens.TryGetValue(e) with
            | true, list -> list
            | _ -> null

/// The lines of the source text being parsed, for the line and column of a
/// position (spec 150.4.4). Only an `LF`, alone or as the end of a `CRLF`,
/// ends a line (20.2.2, JYRO-REQ-1814); FParsec's own count also ends one at
/// a `CR` alone, so the line and column are taken from the index here.
///
/// Thread-static, as `LastToken` is.
[<AbstractClass; Sealed>]
type internal SourceLines =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private starts : int[]

    /// Records the lines of `text`.
    static member Set(text: string) =
        let list = ResizeArray<int>()
        list.Add(0)
        for i in 0 .. text.Length - 1 do
            if text.[i] = '\n' then list.Add(i + 1)
        SourceLines.starts <- list.ToArray()

    /// Lets go of the lines recorded.
    static member Clear() = SourceLines.starts <- null

    /// The 1-based line and column of the code unit at `index`, or the ones
    /// FParsec gave, `pos`, when no text is recorded.
    static member LineColumn(pos: FParsec.Position) : int * int =
        let starts = SourceLines.starts
        if isNull starts then (int pos.Line, int pos.Column)
        else
            let index = int pos.Index
            // The last line start at or before `index`.
            let mutable lo = 0
            let mutable hi = starts.Length - 1
            while lo < hi do
                let mid = (lo + hi + 1) / 2
                if starts.[mid] <= index then lo <- mid else hi <- mid - 1
            (lo + 1, index - starts.[lo] + 1)

module internal PositionTracking =
    let positionFromFParsec (pos: FParsec.Position) : Position =
        let line, column = SourceLines.LineColumn pos
        { Line = line
          Column = column
          StartIndex = int pos.Index
          EndIndex = int pos.Index }

    /// The range of a node that starts at `startPos` and was read up to
    /// `endPos`: from its first token to the end of its last token.
    let positionFromRange (startPos: FParsec.Position) (endPos: FParsec.Position) : Position =
        let line, column = SourceLines.LineColumn startPos
        { Line = line
          Column = column
          StartIndex = int startPos.Index
          EndIndex = int (LastToken.EndOf(startPos.Index, endPos.Index)) }

    /// The range from the start of `first` to the end of `last`.
    let span (first: Position) (last: Position) : Position =
        { Line = first.Line
          Column = first.Column
          StartIndex = first.StartIndex
          EndIndex = last.EndIndex }

    let getPos<'u> : Parser<Position, 'u> =
        getPosition |>> positionFromFParsec

    let withPos<'a, 'u> (p: Parser<'a, 'u>) : Parser<'a * Position, 'u> =
        pipe3 getPosition p getPosition (fun s v e -> (v, positionFromRange s e))

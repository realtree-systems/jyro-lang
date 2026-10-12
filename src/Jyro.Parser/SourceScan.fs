namespace RealTree.Jyro.Parsing

open System
open System.Globalization
open System.Text
open RealTree.Jyro

/// A lexical error in one token: its code and arguments, and the range of
/// the token, from its first code unit to the code unit after its last.
type internal LexicalError =
    { Code: MessageCode
      Args: obj[]
      Start: int
      End: int }

/// Reads a source text token by token without its grammar, to find every
/// lexical error in it (spec 130.3.2) and to measure the token at which a
/// parse stops (spec 130.1.2). The parser stops at the first error it
/// meets; this scan does not, so a result can hold every lexical error of
/// the source and a syntax error after them.
module internal SourceScan =

    /// The code unit that ends a line: an `LF`, alone or as the end of a
    /// `CRLF`. A `CR` that no `LF` follows is not a line end (20.2.2,
    /// JYRO-REQ-1814).
    let private isLineEnd (c: char) = c = '\n'
    let private isAsciiDigit (c: char) = c >= '0' && c <= '9'

    /// The end of the word (20.1.1) that starts at `start`, whose first
    /// code point is an `identifier_start`, read by code point.
    let private wordEnd (source: string) (start: int) : int =
        let struct (_, width) = Unicode15.codePointAt source start
        let mutable i = start + width
        let mutable go = true
        while go && i < source.Length do
            let struct (cp, w) = Unicode15.codePointAt source i
            if Unicode15.isIdentifierChar cp then i <- i + w else go <- false
        i

    /// A word starts at `index`.
    let private startsWord (source: string) (index: int) : bool =
        let struct (cp, _) = Unicode15.codePointAt source index
        Unicode15.isIdentifierStart cp

    /// The 1-based line and column of the code unit at `index`, counting a
    /// `CRLF` or an `LF` as one line end, a `CR` alone as none, and every
    /// code unit as one column, as the parser counts them (spec 150.4.4,
    /// 20.2.2).
    let lineColumn (source: string) (index: int) : int * int =
        let limit = min (max index 0) source.Length
        let mutable line = 1
        let mutable lineStart = 0
        let mutable i = 0
        while i < limit do
            let c = source.[i]
            if c = '\r' && i + 1 < source.Length && source.[i + 1] = '\n' then
                if i + 1 < limit then
                    line <- line + 1
                    lineStart <- i + 2
                i <- i + 2
            elif isLineEnd c then
                line <- line + 1
                lineStart <- i + 1
                i <- i + 1
            else
                i <- i + 1
        (line, limit - lineStart + 1)

    /// The end of the number token that starts at `start`: the longest run
    /// of identifier characters and `.`, with a `+` or `-` directly after an
    /// `e` or `E` when the run is not a hexadecimal or binary literal.
    let private numberEnd (source: string) (start: int) : int =
        let n = source.Length
        let radix =
            start + 1 < n && source.[start] = '0'
            && (let p = source.[start + 1] in p = 'x' || p = 'X' || p = 'b' || p = 'B')
        let mutable i = start
        let mutable go = true
        while go && i < n do
            let c = source.[i]
            if c = '.' then i <- i + 1
            elif (c = '+' || c = '-') && not radix && i > start
                 && (source.[i - 1] = 'e' || source.[i - 1] = 'E') then i <- i + 1
            else
                let struct (cp, w) = Unicode15.codePointAt source i
                if Unicode15.isIdentifierChar cp then i <- i + w else go <- false
        i

    /// The end of the string literal that starts with the quote at `start`:
    /// after its closing quote, or the end of the source when it has none.
    let private stringEnd (source: string) (start: int) : int * bool =
        let n = source.Length
        let quote = source.[start]
        let mutable i = start + 1
        let mutable closed = false
        while not closed && i < n do
            let c = source.[i]
            if c = quote then
                closed <- true
                i <- i + 1
            elif c = '\\' then i <- min n (i + 2)
            else i <- i + 1
        (i, closed)

    let private twoUnitOperators =
        [| "=="; "!="; "<="; ">="; "=>"; "+="; "-="; "*="; "/="; "%="; "&="; "++"; "--"; "??" |]

    /// The number of code units of the token that starts at `index`, or 0 at
    /// the end of the source.
    let tokenLength (source: string) (index: int) : int =
        let n = source.Length
        if index < 0 || index >= n then 0
        else
            let c = source.[index]
            if startsWord source index then
                wordEnd source index - index
            elif isAsciiDigit c then
                numberEnd source index - index
            elif c = '"' || c = '\'' then
                fst (stringEnd source index) - index
            elif index + 1 < n
                 && twoUnitOperators |> Array.contains (source.Substring(index, 2)) then
                2
            elif Char.IsHighSurrogate c && index + 1 < n && Char.IsLowSurrogate source.[index + 1] then
                2
            else
                1

    let private isHex (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    /// The lexical errors of a string literal from `start` to `stop`: each
    /// escape that is not one of the language's, each code unit of its value
    /// that is half of a surrogate pair standing alone, and a missing closing
    /// quote.
    let private stringErrors (source: string) (start: int) (stop: int) (closed: bool) : LexicalError list =
        let errors = ResizeArray<LexicalError>()
        let add code (args: obj[]) =
            errors.Add({ Code = code; Args = args; Start = start; End = stop })
        let value = StringBuilder()
        let contentEnd = if closed then stop - 1 else stop
        let mutable i = start + 1
        while i < contentEnd do
            let c = source.[i]
            if c = '\\' && i + 1 < contentEnd then
                let e = source.[i + 1]
                match e with
                | 'n' -> value.Append('\n') |> ignore; i <- i + 2
                | 'r' -> value.Append('\r') |> ignore; i <- i + 2
                | 't' -> value.Append('\t') |> ignore; i <- i + 2
                | '\\' | '"' | '\'' -> value.Append(e) |> ignore; i <- i + 2
                | '0' -> value.Append('\000') |> ignore; i <- i + 2
                | 'u' ->
                    let digits = source.Substring(i + 2, min 4 (max 0 (contentEnd - (i + 2))))
                    if digits.Length = 4 && digits |> Seq.forall isHex then
                        value.Append(char (Convert.ToInt32(digits, 16))) |> ignore
                        i <- i + 6
                    else
                        // The argument is the escape as written, up to the
                        // first code unit that is not a hexadecimal digit.
                        let shown = String(digits |> Seq.takeWhile isHex |> Seq.toArray)
                        add MessageCode.InvalidEscapeSequence [| box ("\\u" + shown) |]
                        i <- i + 2
                | _ ->
                    let width =
                        if Char.IsHighSurrogate e && i + 2 < contentEnd && Char.IsLowSurrogate source.[i + 2] then 2
                        else 1
                    add MessageCode.InvalidEscapeSequence [| box ("\\" + source.Substring(i + 1, width)) |]
                    i <- i + 1 + width
            elif c = '\\' then
                // A backslash that is the last code unit of an unterminated
                // literal escapes nothing; the missing quote is the error.
                i <- i + 1
            else
                value.Append(c) |> ignore
                i <- i + 1
        // A surrogate of the value that is not half of a high-then-low pair,
        // written raw or as a `\u` escape (20.2.5, 20.7.4): one
        // `UnpairedSurrogate` for the literal, at the whole string token,
        // whose argument is the index in the value of the first such unit.
        let s = value.ToString()
        let mutable j = 0
        let mutable found = false
        while not found && j < s.Length do
            if Char.IsHighSurrogate s.[j] && j + 1 < s.Length && Char.IsLowSurrogate s.[j + 1] then
                j <- j + 2
            elif Char.IsSurrogate s.[j] then
                add MessageCode.UnpairedSurrogate [| box j |]
                found <- true
            else
                j <- j + 1
        if not closed then add MessageCode.UnterminatedString [||]
        List.ofSeq errors

    /// The lexical error of the number token `text` at `start` (20.6.6):
    /// a token that does not match `number_literal` as a whole, a
    /// hexadecimal or binary literal above 9223372036854775807, or a decimal
    /// literal whose value is not finite.
    let private numberError (text: string) (start: int) : LexicalError option =
        let error () =
            Some { Code = MessageCode.InvalidNumberFormat; Args = [| box text |]
                   Start = start; End = start + text.Length }
        let radixError (digits: string) (radix: int) =
            try
                if Convert.ToUInt64(digits, radix) > uint64 Int64.MaxValue then error () else None
            with :? OverflowException -> error ()
        if Lexer.isHexLiteral text then radixError (text.Substring 2) 16
        elif Lexer.isBinaryLiteral text then radixError (text.Substring 2) 2
        elif Lexer.isDecimalLiteral text then
            match Decimal.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, _ -> None
            | _ ->
                match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                | true, d when Double.IsInfinity d || Double.IsNaN d -> error ()
                | false, _ -> error ()
                | _ -> None
        else error ()

    /// Every lexical error of `source`, in the order of their tokens.
    let lexicalErrors (source: string) : LexicalError list =
        let errors = ResizeArray<LexicalError>()
        let n = source.Length
        let mutable i = 0
        while i < n do
            let c = source.[i]
            if c = '#' then
                while i < n && not (isLineEnd source.[i]) do i <- i + 1
            elif c = '"' || c = '\'' then
                let stop, closed = stringEnd source i
                errors.AddRange(stringErrors source i stop closed)
                i <- stop
            elif startsWord source i then
                i <- wordEnd source i
            elif isAsciiDigit c then
                let stop = numberEnd source i
                match numberError (source.Substring(i, stop - i)) i with
                | Some e -> errors.Add e
                | None -> ()
                i <- stop
            else
                i <- i + 1
        List.ofSeq errors

    /// `source` with each token that has a lexical error replaced by one of
    /// the same extent that has none, every line end kept, so that the parser
    /// can read on past it to the syntax errors that follow, at the same
    /// lines and columns. A string literal keeps its quotes and has its other
    /// code units made spaces; one with no closing quote, which runs to the
    /// end of the source, gets one there; a number becomes `0` and spaces.
    let repair (source: string) (errors: LexicalError list) : string =
        if errors.IsEmpty then source
        else
            let chars = source.ToCharArray()
            let mutable closeAtEnd = None
            for e in errors |> List.distinctBy (fun e -> e.Start) do
                let first = chars.[e.Start]
                if first = '"' || first = '\'' then
                    for k in e.Start + 1 .. e.End - 1 do
                        if not (isLineEnd chars.[k]) then chars.[k] <- ' '
                    let closed = e.End - e.Start >= 2 && source.[e.End - 1] = first
                    if closed then chars.[e.End - 1] <- first
                    else closeAtEnd <- Some first
                else
                    chars.[e.Start] <- '0'
                    for k in e.Start + 1 .. e.End - 1 do chars.[k] <- ' '
            let repaired = String(chars)
            match closeAtEnd with
            | Some quote -> repaired + string quote
            | None -> repaired

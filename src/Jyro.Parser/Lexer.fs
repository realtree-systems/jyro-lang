namespace RealTree.Jyro.Parsing

open RealTree.Jyro
open RealTree.Jyro.Ast

open System
open System.Globalization
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

module internal Lexer =
    /// A comment (20.5.1): a `#` and every code unit up to the next `LF`,
    /// which it leaves. Only an `LF` or a `CRLF` ends a line (20.2.2,
    /// JYRO-REQ-1814), and the `CR` of a `CRLF` may be read as part of the
    /// comment. Read code unit by code unit, since FParsec's own line
    /// readers also stop at a `CR` alone.
    let private lineComment : Parser<unit, unit> =
        fun stream ->
            if stream.Peek() <> '#' || stream.IsEndOfStream then
                Reply(ReplyStatus.Error, ErrorMessageList(ErrorMessage.Expected("'#'")))
            else
                stream.Skip()
                while not stream.IsEndOfStream && stream.Peek() <> '\n' do
                    stream.Skip()
                Reply(())

    /// `whitespace` of 20.1.1: a code point with the property White_Space
    /// (10.6.1). Every such code point is in the Basic Multilingual Plane.
    let private whitespace : Parser<unit, unit> = skipSatisfy (fun c -> Unicode15.isWhiteSpace (int c))

    let ws : Parser<unit, unit> = skipMany (whitespace <|> lineComment)

    let ws1 : Parser<unit, unit> = skipMany1 (whitespace <|> lineComment)

    let private here : Parser<int64, unit> = fun stream -> Reply(stream.Index)

    /// The code point at the stream's position and its width in code units:
    /// a surrogate pair is one code point; an unpaired surrogate is its own
    /// code unit, which is neither a letter nor a digit (20.2.5).
    let private peekCodePoint (stream: CharStream<unit>) : struct (int * int) =
        let c = stream.Peek()
        if Char.IsHighSurrogate c then
            let two = stream.Peek2()
            if Char.IsLowSurrogate two.Char1 then struct (Char.ConvertToUtf32(c, two.Char1), 2)
            else struct (int c, 1)
        else struct (int c, 1)

    let private expectedError (label: string) =
        ErrorMessageList(ErrorMessage.Expected(label))

    let messageError (message: string) =
        ErrorMessageList(ErrorMessage.Message(message))

    /// Succeeds, consuming nothing, when the next code point is an
    /// `identifier_char` (20.1.1).
    let identifierCharAhead : Parser<unit, unit> =
        fun stream ->
            let struct (cp, _) = peekCodePoint stream
            if not stream.IsEndOfStream && Unicode15.isIdentifierChar cp then Reply(())
            else Reply(ReplyStatus.Error, expectedError "an identifier character")

    /// A `word` of 20.1.1: an `identifier_start` and any number of
    /// `identifier_char`, read by code point so that a letter outside the
    /// Basic Multilingual Plane is one character.
    let private word : Parser<string, unit> =
        fun stream ->
            let struct (cp, width) = peekCodePoint stream
            if stream.IsEndOfStream || not (Unicode15.isIdentifierStart cp) then
                Reply(ReplyStatus.Error, expectedError "identifier")
            else
                let start = stream.IndexToken
                stream.Skip(width)
                let mutable go = true
                while go do
                    let struct (cp, width) = peekCodePoint stream
                    if not stream.IsEndOfStream && Unicode15.isIdentifierChar cp then stream.Skip(width)
                    else go <- false
                Reply(stream.ReadFrom(start))

    /// A line end lies between the end of the last token read and the
    /// stream's position: the whitespace and comments after that token hold
    /// an `LF`, alone or as the end of a `CRLF` (20.1.2, 20.2.2); a `CR`
    /// that no `LF` follows is whitespace (JYRO-REQ-1814). False where no
    /// token was read.
    let private lineEndBefore (stream: CharStream<unit>) : bool =
        let next = stream.Index
        let tokenEnd = LastToken.EndOf(0L, next)
        if tokenEnd >= next then false
        else
            // Read back and return, restoring the state tag too: FParsec
            // takes a changed tag for consumed input.
            let token = stream.IndexToken
            let tag = stream.StateTag
            stream.Seek(tokenEnd)
            let gap = stream.Read(int (next - tokenEnd))
            stream.Seek(token)
            stream.StateTag <- tag
            gap.IndexOf('\n') >= 0

    /// Succeeds, consuming nothing, when no line end lies between the last
    /// token and the next one: a `(`, `[`, `++` or `--` after a line end
    /// starts a new expression rather than continuing the one before it
    /// (20.1.2).
    let sameLine : Parser<unit, unit> =
        fun stream ->
            if lineEndBefore stream then Reply(ReplyStatus.Error, expectedError "a token on the same line")
            else Reply(())

    /// A `(` follows on the line where the last token ended.
    let openParenOnSameLine (stream: CharStream<unit>) : bool =
        stream.Peek() = '(' && not (lineEndBefore stream)

    /// A token and the whitespace after it, recording where the token's own
    /// text ends (`LastToken`), so that a node ends at its last token.
    let lexeme p =
        p .>>. here .>> ws .>>. here
        |>> fun ((v, tokenEnd), next) ->
            LastToken.Record(tokenEnd, next)
            v

    /// A token read with no whitespace after it.
    let rawToken p =
        p .>>. here
        |>> fun (v, tokenEnd) ->
            LastToken.Record(tokenEnd, tokenEnd)
            v

    /// Whitespace `w` read after a token, which the token's record extends
    /// over.
    let afterToken (w: Parser<unit, unit>) : Parser<unit, unit> =
        here .>>. w .>>. here
        |>> fun ((from, ()), next) -> LastToken.Extend(from, next)

    let reservedKeywords : Set<string> = Set.ofList [
        "var"; "if"; "then"; "elseif"; "else"; "end"; "switch"; "do"; "case"; "default";
        "while"; "for"; "foreach"; "in"; "return"; "fail"; "break"; "continue";
        "and"; "or"; "not"; "is"; "true"; "false"; "null";
        "number"; "string"; "boolean"; "object"; "array";
        "to"; "downto"; "by"; "func"; "exit";
        "union"; "match"; "delete"
    ]

    let isKeyword s = reservedKeywords.Contains(s)

    let keyword kw = attempt (lexeme (pstring kw .>> notFollowedBy identifierCharAhead))

    /// An `identifier`: a word that is not a reserved keyword. A keyword is
    /// refused where it starts, consuming nothing, so that a parse error
    /// names the keyword's token.
    let identifier =
        lexeme (
            fun stream ->
                let start = stream.IndexToken
                let tag = stream.StateTag
                let reply = word stream
                if reply.Status = ReplyStatus.Ok && isKeyword reply.Result then
                    stream.Seek(start)
                    stream.StateTag <- tag
                    Reply(ReplyStatus.Error, messageError (sprintf "'%s' is a reserved keyword" reply.Result))
                else reply
        )

    let anyIdentifier =
        lexeme word

    // Literals
    /// Marker embedded in the out-of-range number error so the parser
    /// front-end can classify it as InvalidNumberFormat (JM2200). Matched as
    /// a substring of the rendered FParsec error, so it must be distinctive.
    let [<Literal>] NumberOutOfRangeMarker = "exceeds 64-bit integer range"

    /// The same idea for a decimal literal past the double range. It used to
    /// share the marker above, so `1e400` - which has no integer part, no
    /// radix prefix and nothing 64-bit about it - was reported as "numeric
    /// literal exceeds the 64-bit integer range", sending the author to look
    /// for a bound they had not crossed.
    let [<Literal>] NumberNotFiniteMarker = "has no finite value"

    /// The same for a number token that does not match `number_literal` as
    /// a whole (20.6.6): `007`, `1.`, `1e+`, `12abc`, `0x`, `0b2`.
    let [<Literal>] NumberMalformedMarker = "is not a number literal"

    let private isAsciiDigit (c: char) = c >= '0' && c <= '9'

    /// The number token of 20.6.6 that starts at the stream's position, which
    /// must be a digit: the longest run of `identifier_char` and `.`, with a
    /// `+` or `-` directly after an `e` or `E` when the run does not start
    /// with `0x`, `0X`, `0b` or `0B`. So `12abc` is one token, never `12`
    /// and then a name, and `0xE+1` is the token `0xE` and then `+`.
    let numberToken : Parser<string, unit> =
        fun stream ->
            if not (isAsciiDigit (stream.Peek())) then
                Reply(ReplyStatus.Error, expectedError "number")
            else
                let start = stream.IndexToken
                let first = stream.Peek()
                let second = stream.Peek2().Char1
                let radix = first = '0' && (second = 'x' || second = 'X' || second = 'b' || second = 'B')
                let mutable previous = '\000'
                let mutable go = true
                while go && not stream.IsEndOfStream do
                    let c = stream.Peek()
                    if c = '.' || ((c = '+' || c = '-') && not radix && (previous = 'e' || previous = 'E')) then
                        stream.Skip()
                        previous <- c
                    else
                        let struct (cp, width) = peekCodePoint stream
                        if Unicode15.isIdentifierChar cp then
                            stream.Skip(width)
                            previous <- if width = 1 then c else '\000'
                        else go <- false
                Reply(stream.ReadFrom(start))

    let private decimalForm =
        System.Text.RegularExpressions.Regex(@"^(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?$", System.Text.RegularExpressions.RegexOptions.CultureInvariant)
    let private hexForm = System.Text.RegularExpressions.Regex(@"^0[xX][0-9A-Fa-f]+$", System.Text.RegularExpressions.RegexOptions.CultureInvariant)
    let private binaryForm = System.Text.RegularExpressions.Regex(@"^0[bB][01]+$", System.Text.RegularExpressions.RegexOptions.CultureInvariant)

    /// The number token matches `decimal_literal` of 20.1.1 as a whole: the
    /// form of a JSON number without a sign (20.6.1).
    let isDecimalLiteral (text: string) = decimalForm.IsMatch text
    /// The number token matches `hex_literal` as a whole.
    let isHexLiteral (text: string) = hexForm.IsMatch text
    /// The number token matches `binary_literal` as a whole.
    let isBinaryLiteral (text: string) = binaryForm.IsMatch text

    /// Parse a number literal into a JyroNumber.
    /// The literal is exact when the exact representation holds its value
    /// with no rounding, and otherwise the double nearest to it (spec 30.2.6,
    /// JYRO-ADR-4). `Decimal.TryParse` is not asked: it rounds `1e-29` and a
    /// 29-digit fraction to 28 digits and reports success.
    let numberLiteral : Parser<JyroNumber, unit> =
        let decimalNumber (text: string) =
            match ExactNumber.ofText text with
            | ValueSome d -> preturn (JyroNumber(d))
            | ValueNone ->
                let f = Double.Parse(text, NumberStyles.Float, CultureInfo.InvariantCulture)
                // A literal past the double range has no finite value, and a
                // Jyro number is finite, so the literal is refused where it is
                // written, the same route the hex and binary overflows take.
                if Double.IsInfinity f || Double.IsNaN f then
                    fail (sprintf "Number literal '%s' %s" text NumberNotFiniteMarker)
                else preturn (JyroNumber(f))
        // A radix literal is bounded at Int64.MaxValue, and the bound is
        // enforced by reading the digits UNSIGNED and comparing. The obvious
        // spelling - `Convert.ToInt64(s, 16)` and catch OverflowException -
        // does not work, because those overloads read a full-width digit
        // string as two's complement instead of overflowing: every value in
        // [2^63, 2^64) was accepted and silently reinterpreted as negative, so
        // `0xFFFFFFFFFFFFFFFF` was -1 and `0b` followed by 64 ones was -1. The
        // guard fired one digit too late, and the comment here described the
        // rule it was aiming at (">16 hex digits, >63 binary digits") rather
        // than the one it had.
        //
        // The OverflowException catch stays for the wider digit strings
        // `Convert.ToUInt64` does refuse: without it a hostile source like
        // `0x` followed by 105 hex digits propagates an uncaught CLR exception
        // up through JyroCompiler and terminates the host process - a sandbox
        // escape. Route both refusals through FParsec's `fail` so they surface
        // as parse diagnostics.
        //
        // Hex/binary literals are integers, so they are exact decimals.
        let radixLiteral (text: string) (radix: int) (name: string) =
            let digits = text.Substring 2
            try
                let value = Convert.ToUInt64(digits, radix)
                if value > uint64 Int64.MaxValue then
                    fail (sprintf "%s literal '%s' %s" name text NumberOutOfRangeMarker)
                else
                    preturn (JyroNumber(decimal value))
            with :? OverflowException ->
                fail (sprintf "%s literal '%s' %s" name text NumberOutOfRangeMarker)
        // No sign here: a leading `-` is always taken by the unary negate
        // parser first, so a literal is unsigned and `-5` is `Negate(5)`.
        lexeme (
            numberToken >>= fun text ->
                if isHexLiteral text then radixLiteral text 16 "Hexadecimal"
                elif isBinaryLiteral text then radixLiteral text 2 "Binary"
                elif isDecimalLiteral text then decimalNumber text
                else fail (sprintf "Number '%s' %s" text NumberMalformedMarker))

    /// Marker embedded in the error for an escape the lexer does not know
    /// (`\q`, or `\u` with fewer than four hex digits) so the parser front-end
    /// can classify it as InvalidEscapeSequence (JM1201).
    let [<Literal>] UnknownEscapeMarker = "is not a known escape sequence"

    /// The same for a string literal that holds half of a UTF-16 surrogate
    /// pair. `\uD800` used to compile, and the script then failed at the JSON
    /// boundary, where an unpaired surrogate has no UTF-8 form.
    let [<Literal>] UnpairedSurrogateMarker = "holds an unpaired UTF-16 surrogate"

    let private escapedChar =
        pchar '\\' >>. (
            (pchar 'n' >>% '\n') <|>
            (pchar 'r' >>% '\r') <|>
            (pchar 't' >>% '\t') <|>
            (pchar '\\' >>% '\\') <|>
            (pchar '"' >>% '"') <|>
            (pchar '\'' >>% '\'') <|>
            (pchar '0' >>% '\000') <|>
            (pchar 'u' >>. (
                attempt (manyMinMaxSatisfy 4 4 isHex) |>> fun s -> char (Convert.ToInt32(s, 16))
                <|> fail (sprintf "'\\u' without four hex digits %s" UnknownEscapeMarker))) <|>
            (anyChar >>= fun c -> fail (sprintf "'\\%c' %s" c UnknownEscapeMarker))
        )

    /// Refuse a literal whose content holds a surrogate that is not half of
    /// a high-then-low pair. Checked on the finished content rather than per
    /// escape, so a raw surrogate in host-supplied source text is caught too.
    let private checkSurrogates (s: string) : Parser<string, unit> =
        let rec unpaired i =
            if i >= s.Length then false
            elif Char.IsHighSurrogate(s.[i]) then
                if i + 1 < s.Length && Char.IsLowSurrogate(s.[i + 1]) then unpaired (i + 2)
                else true
            elif Char.IsLowSurrogate(s.[i]) then true
            else unpaired (i + 1)
        if unpaired 0 then fail (sprintf "String literal %s" UnpairedSurrogateMarker)
        else preturn s

    /// Marker embedded in the missing-close-quote error so the parser
    /// front-end can classify it as UnterminatedString (JM1200). Matched as
    /// a substring of the rendered FParsec error, so it must be distinctive.
    let [<Literal>] UnterminatedStringMarker = "to terminate the string literal"

    /// Marker of the error for a `(` on the same line after an expression
    /// that is not a name written without parentheses, so the parser
    /// front-end can classify it as CallTargetNotName (2003, JYRO-REQ-1798).
    let [<Literal>] CallTargetNotNameMarker = "a call must be made on a name"

    /// The refusal of a `(` on the same line after an expression that is not
    /// a bare name (20.1.3, JYRO-REQ-1798). Fatal, so that no branch the
    /// parser could try after it reports another error in its place: no
    /// script reads a `(` there in any other way.
    let callTargetNotName<'a> () : Reply<'a> =
        Reply(ReplyStatus.FatalError, messageError CallTargetNotNameMarker)

    /// A raw U+FFFF in a literal's content (20.7.1). FParsec's char parsers,
    /// `noneOf` among them, never accept U+FFFF, which is also their
    /// end-of-stream char (CharStream.EOS), so the end is told by position.
    let private rawFFFF : Parser<char, unit> =
        fun stream ->
            if not stream.IsEndOfStream && stream.Peek() = '￿' then
                stream.Skip()
                Reply('￿')
            else Reply(ReplyStatus.Error, NoErrorMessages)

    /// One unescaped code unit of a literal's content: anything but the
    /// closing quote and the backslash.
    let private literalChar (stops: string) =
        noneOf stops <|> rawFFFF

    /// Parse a double-quoted string literal. Once the opening quote is
    /// consumed the closing quote is required; its absence is a committing
    /// failure (this parser is not `attempt`-wrapped in pPrimary) tagged for
    /// UnterminatedString classification.
    let stringLiteral =
        lexeme (
            pchar '"' >>. manyChars (escapedChar <|> literalChar "\"\\")
            .>> (pchar '"' <?> ("a closing double quote " + UnterminatedStringMarker))
            >>= checkSurrogates
        )

    /// Parse a single-quoted string literal (see stringLiteral).
    let stringLiteralSingle =
        lexeme (
            pchar '\'' >>. manyChars (escapedChar <|> literalChar "'\\")
            .>> (pchar '\'' <?> ("a closing single quote " + UnterminatedStringMarker))
            >>= checkSurrogates
        )

    let anyStringLiteral = stringLiteral <|> stringLiteralSingle

    let booleanLiteral = (keyword "true" >>% true) <|> (keyword "false" >>% false)

    let nullLiteral = keyword "null"

    // Operators and punctuation
    let private opChars = "+-*/%=!<>?:."

    let symbol s = lexeme (pstring s)

    let symbolOp s = lexeme (pstring s .>> notFollowedBy (anyOf opChars))

    // Common symbols
    let lparen = symbol "("
    let rparen = symbol ")"
    let lbracket = symbol "["
    let rbracket = symbol "]"
    let lbrace = symbol "{"
    let rbrace = symbol "}"
    let comma = symbol ","
    let colon = symbol ":"
    let dot = symbol "."
    let arrow = symbol "=>"
    let questionMark = symbol "?"

    /// A plain `=`: refused only when it begins `==` or `=>`. It used to be
    /// `symbolOp "="`, which refuses every operator character after it, so
    /// `x =-1`, `var x =-1` and a default `a =-1` did not parse.
    let assignEq = lexeme (pstring "=" .>> notFollowedBy (anyOf "=>"))

    // Assignment operators
    let assignOp =
        choice [
            symbol "+=" >>% AddAssign
            symbol "-=" >>% SubtractAssign
            symbol "*=" >>% MultiplyAssign
            symbol "/=" >>% DivideAssign
            symbol "%=" >>% ModuloAssign
            symbol "&=" >>% ConcatAssign
            assignEq >>% Assign
        ]

    // Comparison and logical operators
    let eqOp = symbol "==" >>% Equal
    let neqOp = symbol "!=" >>% NotEqual
    let leOp = symbol "<=" >>% LessThanOrEqual
    let geOp = symbol ">=" >>% GreaterThanOrEqual
    let ltOp = symbolOp "<" >>% LessThan
    let gtOp = symbolOp ">" >>% GreaterThan
    let andOp = keyword "and" >>% And
    let orOp = keyword "or" >>% Or
    let notOp = keyword "not"
    let coalesceOp = symbol "??" >>% Coalesce

    // Arithmetic operators
    let addOp = symbolOp "+" >>% Add
    let subOp = symbolOp "-" >>% Subtract
    let mulOp = symbolOp "*" >>% Multiply
    let divOp = symbolOp "/" >>% Divide
    let modOp = symbolOp "%" >>% Modulo

    // String concatenation operator
    let ampersandOp = attempt (lexeme (pchar '&' .>> notFollowedBy (pchar '&') .>> notFollowedBy (pchar '='))) >>% Concat

    // Increment/decrement
    let incrementOp = symbol "++"
    let decrementOp = symbol "--"

    // Type keywords. `any` and `function` are deliberately absent from
    // `reservedKeywords`: both are only ever read in a type position (after
    // `:` or `is`), so leaving them unreserved keeps them usable as ordinary
    // identifiers. `function` is spelled out in full precisely because the
    // declaration keyword is `func` - the two never collide.
    let typeKeyword =
        choice [
            keyword "any" >>% AnyType
            keyword "number" >>% NumberType
            keyword "string" >>% StringType
            keyword "boolean" >>% BooleanType
            keyword "object" >>% ObjectType
            keyword "array" >>% ArrayType
            keyword "null" >>% NullType
            keyword "function" >>% FunctionType
        ]

    /// Initialize the parser: a U+FEFF at the first position of the source
    /// is ignored (20.2.4), though it still counts as a column, and then the
    /// leading whitespace. A U+FEFF anywhere else is not whitespace and
    /// starts no token, so it is a parse error.
    let initParser : Parser<unit, unit> = optional (skipChar (char 0xFEFF)) >>. ws

    let private expressionKeywords = Set.ofList [ "true"; "false"; "null"; "not"; "match" ]

    let private statementKeywords =
        Set.ofList [
            "var"; "if"; "while"; "for"; "foreach"; "switch"; "match"; "return"; "exit"
            "fail"; "break"; "continue"; "delete"; "func"; "union" ]

    /// Whether an expression, or with `statement` a statement, begins at the
    /// stream's position, judged from its first token alone and consuming
    /// nothing: a name, a literal, a keyword that begins one, or a `(`, `[`,
    /// `{`, `-` or `++`. So `end`, `)`, `]` and the end of the source, where
    /// a list or a body ends, begin none.
    let private nodeAhead (statement: bool) (stream: CharStream<unit>) : bool =
        if stream.IsEndOfStream then false
        else
            let struct (cp, _) = peekCodePoint stream
            if Unicode15.isIdentifierStart cp then
                let start = stream.IndexToken
                let tag = stream.StateTag
                let reply = word stream
                stream.Seek(start)
                stream.StateTag <- tag
                let w = reply.Result
                not (isKeyword w) || expressionKeywords.Contains w || (statement && statementKeywords.Contains w)
            else
                let c = stream.Peek()
                (c >= '0' && c <= '9') || c = '"' || c = '\'' || c = '(' || c = '[' || c = '{' || c = '-'
                || (c = '+' && stream.Peek2().Char1 = '+')

    let private tooDeep () : Reply<'a> =
        Reply(ReplyStatus.FatalError, messageError "the source nests too deeply")

    /// `p`, read one level deeper than what holds it (spec 100.10.2): the
    /// one place where the parser goes deeper, checked before it recurses
    /// (JYRO-ADR-177 point 4). Where the reading count (`SourceNesting`,
    /// JYRO-ADR-178) would go above the maximum, `p` is not run: if
    /// a node begins there, its first token is recorded and the parse fails
    /// with a fatal error, so it never recurses past the maximum; if none
    /// does (an empty list or body), this fails as `p` would, consuming
    /// nothing. Once a node is recorded every later call fails at once, so a
    /// branch the parser tries after it does not read deeper again, and
    /// whatever the parse then gives, the source is refused for its depth
    /// (`Parser.run`). `statement` says whether `p` reads a statement or an
    /// expression.
    let nestedAs<'a> (statement: bool) (p: Parser<'a, unit>) : Parser<'a, unit> =
        fun stream ->
            let saved = SourceNesting.Depth
            let depth = saved + 1
            if SourceNesting.Exceeded >= 0L then tooDeep ()
            elif depth > Ast.MaxNestingDepth then
                if nodeAhead statement stream then
                    SourceNesting.Exceed(stream.Index)
                    tooDeep ()
                else
                    Reply(ReplyStatus.Error, expectedError (if statement then "statement" else "expression"))
            else
                SourceNesting.Depth <- depth
                let reply = p stream
                SourceNesting.Depth <- saved
                reply

    /// An expression, or part of one, read one level deeper (`nestedAs`).
    let nested<'a> (p: Parser<'a, unit>) : Parser<'a, unit> = nestedAs false p

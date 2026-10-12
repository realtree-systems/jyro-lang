namespace RealTree.Jyro

open System
open System.Globalization
open System.Text

// The string form of a double as ECMA-262, 15th edition (ECMAScript 2024),
// gives it: `Number::toString` with radix 10 (spec 10.6.2, 40.4.7).
module internal NumberText =

    // The shortest decimal digits that read back to `f`, which is finite and
    // above zero, and the exponent `n` of 10 such that `f` is the digits
    // read as `0.d1d2...` times `10^n`. .NET (Core 3.0 and later) gives the
    // shortest round-trip text of a double, nearest to it where two are
    // equally short, which is the choice Number::toString makes.
    let private shortestDigits (f: float) : string * int =
        let text = f.ToString("R", CultureInfo.InvariantCulture)
        let mantissa, exponent =
            match text.IndexOfAny([| 'E'; 'e' |]) with
            | -1 -> text, 0
            | i -> text.Substring(0, i), Int32.Parse(text.Substring(i + 1), NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)
        let point = match mantissa.IndexOf('.') with -1 -> mantissa.Length | i -> i
        let allDigits = mantissa.Replace(".", "")
        let leading = allDigits.Length - allDigits.TrimStart('0').Length
        let digits = allDigits.TrimStart('0').TrimEnd('0')
        (digits, point - leading + exponent)

    // `Number::toString(f, 10)`. Both zeros are `"0"`; NaN is `"NaN"` and an
    // infinity `"Infinity"` or `"-Infinity"`, though a Jyro number is finite.
    let ecmaScript (f: float) : string =
        if Double.IsNaN f then "NaN"
        elif f = 0.0 then "0"
        elif Double.IsPositiveInfinity f then "Infinity"
        elif Double.IsNegativeInfinity f then "-Infinity"
        else
            let sign = if f < 0.0 then "-" else ""
            let digits, n = shortestDigits (abs f)
            let k = digits.Length
            let body =
                if k <= n && n <= 21 then
                    digits + String('0', n - k)
                elif 0 < n && n <= 21 then
                    digits.Substring(0, n) + "." + digits.Substring(n)
                elif -6 < n && n <= 0 then
                    "0." + String('0', -n) + digits
                else
                    let e = n - 1
                    let exponent = (if e > 0 then "+" else "-") + string (abs e)
                    let b = StringBuilder()
                    b.Append(digits.[0]) |> ignore
                    if k > 1 then b.Append('.').Append(digits, 1, k - 1) |> ignore
                    b.Append('e').Append(exponent).ToString()
            sign + body

// The exact representation of a number (spec 30.2.4, JYRO-ADR-4): exactly
// the values `m / 10^s` with `m` an integer of magnitude at most `2^96 - 1`
// and `s` from 0 to 28. .NET's `decimal` holds every such value, and also
// rounds silently where it cannot hold one - `Decimal.Parse` rounds a
// 29-digit fraction to 28 digits, and `1m / 3m` to 28 - so whether a value
// is exact is decided here, from its digits, and never by asking `decimal`.
module internal ExactNumber =

    open System.Numerics

    // `2^96 - 1`, the largest significand of the exact representation.
    let private maxSignificand = BigInteger.Pow(BigInteger(2), 96) - BigInteger.One

    // `2^96 - 1` written out: 29 digits.
    let private maxDigits = "79228162514264337593543950335"

    // The largest scale of the exact representation.
    [<Literal>]
    let MaxScale = 28

    // `d` as its significand and its scale: `d = m / 10^s`.
    let parts (d: decimal) : struct (BigInteger * int) =
        let bits = Decimal.GetBits d
        let magnitude =
            (BigInteger(uint32 bits.[2]) <<< 64) ||| (BigInteger(uint32 bits.[1]) <<< 32) ||| BigInteger(uint32 bits.[0])
        let scale = (bits.[3] >>> 16) &&& 0xFF
        struct ((if bits.[3] < 0 then -magnitude else magnitude), scale)

    // The decimal `m / 10^s`, when `m` and `s` are already within the exact
    // representation.
    let private ofFitting (m: BigInteger) (s: int) : decimal =
        let negative = m.Sign < 0
        let magnitude = BigInteger.Abs m
        let mask = BigInteger(UInt32.MaxValue)
        let lo = int (uint32 (magnitude &&& mask))
        let mid = int (uint32 ((magnitude >>> 32) &&& mask))
        let hi = int (uint32 ((magnitude >>> 64) &&& mask))
        Decimal(lo, mid, hi, negative && not magnitude.IsZero, byte s)

    // The value `m / 10^s` (`s` at least 0) in the exact representation,
    // when it holds it with no rounding. A trailing zero of `m` is given up,
    // with one place of scale, only while the value does not fit as written,
    // so a value keeps the scale it was written with wherever it can, as
    // `decimal` keeps it.
    let ofParts (m: BigInteger) (s: int) : decimal voption =
        let mutable m = m
        let mutable s = s
        let ten = BigInteger(10)
        let fits () = s <= MaxScale && BigInteger.Abs m <= maxSignificand
        while not (fits ()) && s > 0 && (m % ten).IsZero do
            m <- m / ten
            s <- s - 1
        if fits () then ValueSome (ofFitting m s) else ValueNone

    // Whether the code units of `text` from `start` up to `finish` match the
    // JSON number grammar of RFC 8259 6 as a whole:
    // `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`. The time and
    // the cancellation signal are checked a chunk at a time (spec 100.2.5).
    let isJsonNumberIn (text: string) (start: int) (finish: int) : bool =
        let i = ref start
        let nextCheck = ref (start + ExecutionCheck.ChunkLength)
        let isDigit () = i.Value < finish && text.[i.Value] >= '0' && text.[i.Value] <= '9'
        let at (c: char) = i.Value < finish && text.[i.Value] = c
        let advance () =
            i.Value <- i.Value + 1
            if i.Value >= nextCheck.Value then
                ExecutionCheck.Check()
                nextCheck.Value <- i.Value + ExecutionCheck.ChunkLength
        // One or more digits.
        let digits () =
            if isDigit () then
                while isDigit () do advance ()
                true
            else false
        if at '-' then advance ()
        let integer =
            if at '0' then
                advance ()
                true
            else digits ()
        let fraction =
            if integer && at '.' then
                advance ()
                digits ()
            else integer
        let exponent =
            if fraction && (at 'e' || at 'E') then
                advance ()
                if at '+' || at '-' then advance ()
                digits ()
            else fraction
        exponent && i.Value = finish

    // Whether `text` as a whole matches the JSON number grammar.
    let isJsonNumber (text: string) : bool = isJsonNumberIn text 0 text.Length

    // The most significant digits of a number that the text handed to the
    // platform's parser keeps (JYRO-ADR-151 (5)(b)). No midpoint between two
    // adjacent doubles has more than 767 significant digits, so the first 768
    // digits, and a 1 after them when any digit dropped is not zero, have
    // the same nearest double as the whole number.
    [<Literal>]
    let BoundedDigits = 768

    // A number written to the JSON number grammar (a numeric literal of 20.6
    // is one without the sign), read a character at a time where its text
    // lies - a string, a span of bytes, the segments of a sequence - with
    // the time and the cancellation signal checked a chunk at a time (spec
    // 100.2.5). Nothing copies or parses the whole number in one call
    // (JYRO-ADR-151 (5)).
    //
    // It keeps two things as it reads: at most the 29 significant digits
    // that the exact representation can hold, from which `Exact` decides
    // whether the value is exact (JYRO-ADR-151 (1)); and the first
    // `BoundedDigits` significant digits with a sticky digit and the
    // exponent adjusted for the digits dropped, from which `Nearest` asks the
    // platform's correctly rounded parser for the nearest double over a text
    // of fixed bound (JYRO-ADR-151 (2), (5)(b)).
    [<Sealed>]
    type Reader() =
        let mutable negative = false
        let mutable inExponent = false
        let mutable expNegative = false
        // The exponent as written, saturated far beyond any scale or double
        // exponent that matters.
        let mutable exponent = 0L
        let mutable inFraction = false
        // The exact side: the significant digits, from the first that is not
        // zero, while there are at most 29; the count of digits written after
        // the point; and the zeros met after the last digit that is not zero,
        // not yet added.
        let significant = Text.StringBuilder(maxDigits.Length + 1)
        let mutable tooMany = false
        let mutable afterPoint = 0L
        let mutable zeros = 0L
        // The bounded side: the first `BoundedDigits` significant digits,
        // whether a digit after them is not zero, and where the point falls
        // among all the significant digits.
        let mutable bounded : Text.StringBuilder = null
        let mutable sticky = false
        let mutable seenNonZero = false
        let mutable integerSignificant = 0L
        let mutable leadingFractionZeros = 0L
        let mutable sinceCheck = 0

        // Reads the next character of the number, as its UTF-16 code unit or
        // its byte: JSON's number grammar is ASCII.
        member _.Read(code: int) : unit =
            sinceCheck <- sinceCheck + 1
            if sinceCheck >= ExecutionCheck.ChunkLength then
                ExecutionCheck.Check()
                sinceCheck <- 0
            if inExponent then
                if code = int '-' then expNegative <- true
                elif code >= int '0' && code <= int '9' then
                    if exponent < 1_000_000_000_000L then
                        exponent <- exponent * 10L + int64 (code - int '0')
            elif code >= int '0' && code <= int '9' then
                let c = char code
                if inFraction then afterPoint <- afterPoint + 1L
                if c = '0' then
                    if significant.Length > 0 then zeros <- zeros + 1L
                else
                    if not tooMany then
                        if int64 significant.Length + zeros + 1L > int64 maxDigits.Length then tooMany <- true
                        else significant.Append('0', int zeros).Append(c) |> ignore
                    zeros <- 0L
                if c = '0' && not seenNonZero then
                    if inFraction then leadingFractionZeros <- leadingFractionZeros + 1L
                else
                    seenNonZero <- true
                    if not inFraction then integerSignificant <- integerSignificant + 1L
                    if isNull bounded then bounded <- Text.StringBuilder(32)
                    if bounded.Length < BoundedDigits then bounded.Append(c) |> ignore
                    elif c <> '0' then sticky <- true
            elif code = int '.' then inFraction <- true
            elif code = int '-' then negative <- true
            elif code = int 'e' || code = int 'E' then inExponent <- true

        // The exponent as written, with its sign.
        member private _.Exponent = if expNegative then -exponent else exponent

        // The value in the exact representation, when it holds it with no
        // rounding (spec 30.2.4, 30.2.6, 30.2.7). Exactness depends on the
        // value and not on how many digits wrote it:
        // `1.00000000000000000000000000000` is exact, `1e-29` is not.
        member this.Exact : decimal voption =
            let exponent = this.Exponent
            if significant.Length = 0 then
                // Zero is exact, at the scale it was written with where the
                // representation has it, as `decimal` keeps `0.0`.
                let scale = max 0L (min (int64 MaxScale) (afterPoint - exponent))
                ValueSome (Decimal(0, 0, 0, false, byte scale))
            elif tooMany then ValueNone
            else
                // The value is `significant * 10^(zeros - afterPoint + exponent)`;
                // the scale it was written with is `afterPoint - exponent`.
                let written = afterPoint - exponent
                let power = zeros - afterPoint + exponent
                // The scale that the value needs at least.
                let least = max 0L (-power)
                if least > int64 MaxScale then ValueNone
                // A value above 10^29 has more than 29 digits as an integer.
                elif int64 significant.Length + power > int64 maxDigits.Length then ValueNone
                else
                    let scale = max least (min (max written 0L) (int64 MaxScale))
                    // The significand at `scale`: the significant digits, then
                    // `power + scale` zeros, which is at most 29 digits.
                    let trailing = int (power + scale)
                    let digits = significant.ToString() + String('0', trailing)
                    let mutable m = BigInteger.Parse(digits, Globalization.CultureInfo.InvariantCulture)
                    if negative then m <- -m
                    ofParts m (int scale)

        // The text handed to the platform's parser (JYRO-ADR-151 (5)(b)):
        // `0.` then the first `BoundedDigits` significant digits, a 1 when a
        // digit dropped is not zero, and the exponent that puts the point
        // where the number has it, saturated far outside a double's range.
        // At most about 800 characters, whatever the length of the number.
        member this.ParserText : string =
            if not seenNonZero then (if negative then "-0" else "0")
            else
                let point = integerSignificant - leadingFractionZeros + this.Exponent
                let point = max -1_000_000L (min 1_000_000L point)
                let b = Text.StringBuilder(bounded.Length + 16)
                if negative then b.Append('-') |> ignore
                b.Append("0.").Append(bounded) |> ignore
                if sticky then b.Append('1') |> ignore
                b.Append('e').Append(point.ToString(CultureInfo.InvariantCulture)).ToString()

        // The double nearest to the number (round half to even), from the
        // platform's correctly rounded parser over `ParserText`
        // (JYRO-ADR-151 (2)). An infinity when the number is beyond the
        // range of a double.
        member this.Nearest : float =
            Double.Parse(this.ParserText, NumberStyles.Float, CultureInfo.InvariantCulture)

    // The code units of `text` from `start` up to `finish`, which match the
    // JSON number grammar, read by a `Reader`.
    let readIn (text: string) (start: int) (finish: int) : Reader =
        let r = Reader()
        for i in start .. finish - 1 do r.Read(int text.[i])
        r

    // The bytes of `bytes`, a JSON number token, read by a `Reader`.
    let readBytes (bytes: ReadOnlySpan<byte>) : Reader =
        let r = Reader()
        for i in 0 .. bytes.Length - 1 do r.Read(int bytes.[i])
        r

    // The bytes of `sequence`, a JSON number token, read segment by segment
    // by a `Reader`.
    let readSequence (sequence: Buffers.ReadOnlySequence<byte>) : Reader =
        let r = Reader()
        for segment in sequence do
            let span = segment.Span
            for i in 0 .. span.Length - 1 do r.Read(int span.[i])
        r

    // The value of the code units of `text` from `start` up to `finish`,
    // which match the JSON number grammar, in the exact representation, when
    // it holds that value with no rounding; see `Reader.Exact`.
    let ofTextIn (text: string) (start: int) (finish: int) : decimal voption =
        (readIn text start finish).Exact

    // `ofTextIn` over the whole of `text`.
    let ofText (text: string) : decimal voption = ofTextIn text 0 text.Length

    // The ASCII text of `bytes`, a JSON number token, copied a chunk at a
    // time with a check between chunks (JYRO-ADR-151 (5)(c)).
    let asciiText (bytes: ReadOnlySpan<byte>) : string =
        let b = Text.StringBuilder(min bytes.Length ExecutionCheck.ChunkLength)
        let mutable at = 0
        while at < bytes.Length do
            if at > 0 then ExecutionCheck.Check()
            let n = min ExecutionCheck.ChunkLength (bytes.Length - at)
            b.Append(Text.Encoding.ASCII.GetString(bytes.Slice(at, n))) |> ignore
            at <- at + n
        BoundedText.ofBuilder b ExecutionCheck.Check

    // `asciiText` over the segments of `sequence`.
    let asciiTextOfSequence (sequence: Buffers.ReadOnlySequence<byte>) : string =
        let b = Text.StringBuilder(ExecutionCheck.ChunkLength)
        let mutable first = true
        for segment in sequence do
            let span = segment.Span
            let mutable at = 0
            while at < span.Length do
                if not first then ExecutionCheck.Check()
                first <- false
                let n = min ExecutionCheck.ChunkLength (span.Length - at)
                b.Append(Text.Encoding.ASCII.GetString(span.Slice(at, n))) |> ignore
                at <- at + n
        BoundedText.ofBuilder b ExecutionCheck.Check

    // 10^0 to 10^22, each of which a double holds exactly.
    let private exactPowersOfTen =
        [| 1e0; 1e1; 1e2; 1e3; 1e4; 1e5; 1e6; 1e7; 1e8; 1e9; 1e10; 1e11
           1e12; 1e13; 1e14; 1e15; 1e16; 1e17; 1e18; 1e19; 1e20; 1e21; 1e22 |]

    // The double nearest to `d` (round half to even), as IEEE 754 gives it.
    //
    // `decimal`'s own conversion divides a double significand by a double
    // power of ten and so can round twice. When the significand has at most
    // 53 bits and the scale is at most 22, both are exact doubles and the one
    // division is correctly rounded; otherwise the decimal's text, at most 31
    // characters, is read by the platform's parser, which is correctly
    // rounded.
    let toDouble (d: decimal) : float =
        let bits = Decimal.GetBits d
        let scale = (bits.[3] >>> 16) &&& 0xFF
        if bits.[2] = 0 && (uint32 bits.[1]) < (1u <<< 21) && scale <= 22 then
            let magnitude = (uint64 (uint32 bits.[1]) <<< 32) ||| uint64 (uint32 bits.[0])
            let value = float magnitude / exactPowersOfTen.[scale]
            if bits.[3] < 0 then -value else value
        else
            Double.Parse(d.ToString(Globalization.CultureInfo.InvariantCulture),
                         Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture)

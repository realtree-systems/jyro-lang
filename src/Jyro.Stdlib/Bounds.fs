namespace RealTree.Jyro

open System

// What the standard library needs to keep a call inside the limits of Part
// 100: a counter that checks the execution time and the cancellation signal
// at a fixed interval while a function iterates over script data (spec
// 100.2.5), and the maximum value size of what it makes (spec 100.8.7).
module internal Bounds =

    // Checks `ctx` on the first unit of work a call counts and then on every
    // `ExecutionCheck.Interval`-th, so a loop over a large argument never runs
    // more than one interval past a check, and a small one checks once.
    //
    // A unit counted by `Tick` MUST be a fixed amount of work - at most about
    // `ExecutionCheck.ChunkLength` elementary steps (a code unit copied or
    // compared, an element visited) - so the work between two checks has a
    // fixed bound. Work with no fixed bound per unit is checked with
    // `CheckNow` before each unit.
    //
    // The work between the last `Tick` of a call and its return is bound by
    // the same rule: no step after the loop may be one call over the whole
    // result (`String(char[])`, `String(char[], int, int)`, `new string`,
    // `StringBuilder.ToString`, `ToArray`). A string result is written in
    // place a chunk at a time (`create`, `ofBuilder`); only its allocation,
    // whose size has been checked against the maximum value size, is one
    // step with no check inside (JYRO-ADR-101).
    [<Sealed>]
    type Ticker(ctx: JyroExecutionContext) =
        let mutable count = 0
        member _.Tick() =
            if count = 0 && not (isNull (box ctx)) then ctx.CheckExecutionLimits()
            count <- count + 1
            if count >= ExecutionCheck.Interval then count <- 0
        // Checks now, for a unit of work that is not a fixed amount: a regex
        // match, bounded only by the regex timeout, is checked before each
        // one rather than counted.
        member _.CheckNow() =
            if not (isNull (box ctx)) then ctx.CheckExecutionLimits()

    // The position of the first occurrence of `search` in `text` at or after
    // `start`, by ordinal comparison, or -1. An empty `search` is found at
    // `start`, as `String.IndexOf` finds it.
    //
    // Each unit the ticker counts is a fixed amount of work - at most about
    // `ExecutionCheck.ChunkLength` code-unit comparisons - whatever the
    // lengths of `text` and `search` (spec 100.2.5): a short `search` is
    // looked for over a window of positions sized so that the window costs at
    // most that many comparisons, and a long one is compared at each
    // candidate a chunk at a time.
    let indexOfOrdinal (ticker: Ticker) (text: string) (search: string) (start: int) : int =
        if search.Length = 0 then (if start <= text.Length then start else -1)
        else
            let chunk = ExecutionCheck.ChunkLength
            let mutable found = -1
            let mutable position = start
            let lastStart = text.Length - search.Length
            if search.Length <= chunk then
                let windowStarts = max 1 (chunk / search.Length)
                while found < 0 && position <= lastStart do
                    ticker.Tick()
                    let starts = min windowStarts (lastStart - position + 1)
                    let at = text.IndexOf(search, position, starts + search.Length - 1, StringComparison.Ordinal)
                    if at >= 0 then found <- at
                    else position <- position + starts
            else
                while found < 0 && position <= lastStart do
                    ticker.Tick()
                    let starts = min chunk (lastStart - position + 1)
                    let at = text.IndexOf(search.[0], position, starts)
                    if at < 0 then position <- position + starts
                    else
                        let mutable i = 1
                        let mutable equal = true
                        while equal && i < search.Length do
                            ticker.Tick()
                            let n = min chunk (search.Length - i)
                            equal <- text.AsSpan(at + i, n).SequenceEqual(search.AsSpan(i, n))
                            i <- i + n
                        if equal then found <- at else position <- at + 1
            found

    // Appends `piece` to `text` a chunk of code units at a time, with a
    // count per chunk.
    let appendChunked (ticker: Ticker) (text: Text.StringBuilder) (piece: string) : unit =
        let mutable i = 0
        while i < piece.Length do
            ticker.Tick()
            let n = min ExecutionCheck.ChunkLength (piece.Length - i)
            text.Append(piece, i, n) |> ignore
            i <- i + n

    // A string of `length` code units written in place by `write` a chunk
    // at a time, with a count per chunk (JYRO-ADR-101). The caller has
    // checked `length` against the maximum value size.
    let create (ticker: Ticker) (length: int) (write: ChunkWriter) : string =
        if length > 0 then ticker.Tick()
        BoundedText.create length ticker.Tick write

    // The text `text` holds, copied out a chunk at a time with a count per
    // chunk, never by one `StringBuilder.ToString` over the whole text
    // (JYRO-ADR-101).
    let ofBuilder (ticker: Ticker) (text: Text.StringBuilder) : string =
        ticker.Tick()
        BoundedText.ofBuilder text ticker.Tick

    // `length` code units of `text` from `start`, copied a chunk at a time
    // with a count per chunk. The whole of `text` is `text` itself, which
    // the bound exempts (spec 100.8.7); any other result above the maximum
    // value size is refused before it is allocated.
    let slice (ticker: Ticker) (text: string) (start: int) (length: int) : string =
        if start = 0 && length = text.Length then text
        elif length = 0 then String.Empty
        else
            ValueSize.Check(int64 length)
            create ticker length (ChunkWriter(fun destination at ->
                let n = min ExecutionCheck.ChunkLength (length - at)
                text.AsSpan(start + at, n).CopyTo(destination.Slice(at, n))
                n))

    // Writes the chunk of `text` from `i` into `destination`, each code
    // point changed by the simple uppercase or lowercase mapping of
    // UnicodeData.txt of Unicode 15.0.0 (spec 10.6.1, 91.1.2, 91.2.2,
    // JYRO-ADR-143), and returns its length: at most
    // `ExecutionCheck.ChunkLength` code units, and never ending between the
    // two halves of a surrogate pair. A simple mapping keeps a code point in
    // its plane, so the chunk keeps its length; an unpaired surrogate maps to
    // itself.
    let private casedChunk (upper: bool) (text: string) (i: int) (destination: Span<char>) : int =
        let mutable n = min ExecutionCheck.ChunkLength (text.Length - i)
        if n > 1 && i + n < text.Length && Char.IsHighSurrogate(text.[i + n - 1]) then n <- n - 1
        let mutable k = 0
        while k < n do
            let struct (pair, pairWidth) = Unicode15.codePointAt text (i + k)
            // A pair is mapped as one code point only when both halves are in
            // the chunk, which the chunk's end above ensures but for a text
            // of one code unit.
            let cp, width = if pairWidth = 2 && k + 1 < n then pair, 2 else int text.[i + k], 1
            let changed = if upper then Unicode15.toUpper cp else Unicode15.toLower cp
            if width = 2 then
                let pair = Char.ConvertFromUtf32 changed
                destination.[k] <- pair.[0]
                destination.[k + 1] <- pair.[1]
            else destination.[k] <- char changed
            k <- k + width
        n

    // `text` with its case changed by the simple case mappings of Unicode
    // 15.0.0, a chunk at a time with a count per chunk. A chunk never ends
    // between the two halves of a surrogate pair, so a pair is cased as one
    // character.
    //
    // Casing keeps the length. Above the maximum value size the result is
    // allowed only when it equals `text` (spec 100.8.7), so a text that long
    // is scanned for a code unit the casing would change, and `text` itself is
    // returned when there is none.
    let changeCase (ticker: Ticker) (upper: bool) (text: string) : string =
        let length = text.Length
        if int64 length > int64 ValueSize.Maximum then
            let scratch = Array.zeroCreate<char> ExecutionCheck.ChunkLength
            let mutable i = 0
            while i < length do
                ticker.Tick()
                let n = casedChunk upper text i (Span<char>(scratch))
                if not (MemoryExtensions.SequenceEqual(ReadOnlySpan<char>(scratch, 0, n), text.AsSpan(i, n))) then
                    ValueSize.Raise()
                i <- i + n
            text
        else
            create ticker length (ChunkWriter(fun destination at -> casedChunk upper text at (destination.Slice(at))))

    // Raises `ValueSizeLimitExceeded` when a result of `count` elements,
    // properties or code units would be above the maximum value size. For a
    // result whose size is known before it is built and before any other
    // error can occur (spec 100.8.11).
    let requireSize (count: int64) : unit = ValueSize.Check count

    // A new array of `items`, in order, checking the limits as it copies.
    // The count is checked before anything is allocated, and the array is
    // created at that capacity, so it is never grown and copied again
    // (JYRO-ADR-101 (3), JYRO-ADR-133).
    let copyOf (ticker: Ticker) (count: int) (item: int -> JyroValue) : JyroArray =
        requireSize (int64 count)
        let result = JyroArray(count)
        for i = 0 to count - 1 do
            ticker.Tick()
            result.Add(item i)
        result

    // True when the optional argument at `index` was not given, or was given
    // as `null`: the two mean the same (spec 90.13.1), so an explicit `null`
    // takes the parameter's default and is not checked (90.3.8).
    let isAbsent (args: Collections.Generic.IReadOnlyList<JyroValue>) (index: int) : bool =
        index >= args.Count
        || (let value = args.[index] in obj.ReferenceEquals(value, null) || value.IsNull)

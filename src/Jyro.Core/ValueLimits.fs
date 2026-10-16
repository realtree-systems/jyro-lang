namespace RealTree.Jyro

open System
open System.Collections.Generic

// The run's check of the execution time limit and the cancellation signal,
// reachable from code that is handed no execution context.
//
// The operators and the structural walks of a value - `&`, `==` on two
// containers, the relational operators on two strings, the string form, the
// JSON writer - are members of `JyroValue`, and the compiler emits them as
// plain calls with no context argument. Spec 100.2.5 binds them to check the
// time and the cancellation signal while they iterate, as a library function
// does, so the run installs its check here for the thread it runs on and the
// walks call it.
//
// Per thread, for the reason `ValueRecursionGuard` is: a run executes on one
// thread, and concurrent runs on other threads keep their own check. A run
// restores the check it replaced, so a host function that starts a nested run
// gets its own and hands the outer one back.
//
// With no run in progress there is no check, and a walk runs without one: a
// host converting a result after the run is bounded by the size of the value
// (spec 160.8.1), not by a time limit.
type internal ExecutionCheck =

    [<ThreadStatic; DefaultValue>]
    static val mutable private current : Action | null

    [<ThreadStatic; DefaultValue>]
    static val mutable private ticks : int

    // How many units of work `Tick` lets pass between two checks. Fixed, so
    // the interval does not grow with the size of what is walked.
    static member Interval = 1024

    // The number of UTF-16 code units a string operation copies or compares
    // between two checks.
    static member ChunkLength = 4096

    // Installs `check` for this thread and returns the check it replaced,
    // which the caller MUST hand to `Restore` in a `finally`.
    static member Install(check: Action) : Action | null =
        let previous = ExecutionCheck.current
        ExecutionCheck.current <- check
        ExecutionCheck.ticks <- 0
        previous

    // Puts back the check that `Install` replaced.
    static member Restore(previous: Action | null) : unit =
        ExecutionCheck.current <- previous

    // Checks the time limit and the cancellation signal now. Raises
    // `ExecutionTimeLimitExceeded` or `CanceledByHost`; does nothing when
    // no run is in progress on this thread.
    static member Check() : unit =
        match ExecutionCheck.current with
        | null -> ()
        | c -> c.Invoke()

    // Counts one unit of work, and checks on every `Interval`-th.
    static member Tick() : unit =
        let t = ExecutionCheck.ticks + 1
        if t >= ExecutionCheck.Interval then
            ExecutionCheck.ticks <- 0
            ExecutionCheck.Check()
        else
            ExecutionCheck.ticks <- t

// The maximum value size of spec 100.8.7: no operator, assignment or standard
// library function makes a string longer than `Maximum` UTF-16 code units, an
// array of more than `Maximum` elements or an object of more than `Maximum`
// properties, and a host conversion refuses a value whose size (spec 100.8.6)
// is above it (spec 160.8.1).
//
// Fixed, and not an execution option: a host cannot raise or remove it
// (ADR 0072).
type internal ValueSize =

    // M, 16,777,216 (2^24).
    static member Maximum = 16_777_216

    // Raises `ValueSizeLimitExceeded`.
    static member Raise() : 'T =
        JyroError.raiseRuntime MessageCode.ValueSizeLimitExceeded [| box ValueSize.Maximum |]

    // Raises `ValueSizeLimitExceeded` when a string, array or object of
    // `count` code units, elements or properties would be above the maximum.
    static member Check(count: int64) : unit =
        if count > int64 ValueSize.Maximum then ValueSize.Raise()

// Writes one chunk of a string that is made in place (`BoundedText.create`):
// handed the whole result and the offset to write at, it writes at most about
// `ExecutionCheck.ChunkLength` code units there and returns how many it
// wrote, at least one.
type internal ChunkWriter = delegate of Span<char> * int -> int

// String operations over the code units of their operands that check the
// time limit and the cancellation signal at a fixed interval (spec 100.2.5).
//
// Each copies or compares `ExecutionCheck.ChunkLength` code units at a time
// and checks between two chunks, so one call over a long string - a copy, an
// ordinal comparison - never runs more than one chunk past a check. The first
// chunk runs without one: the statement that holds the operation has just
// checked, so a short operand pays nothing.
//
// A result is written in place, into the string itself, a chunk at a time
// (`create`, `ofBuilder`): never into a scratch buffer that one call then
// copies whole into a string (`String(char[])`, `String(char[], int, int)`,
// `StringBuilder.ToString`). The allocation of the result, whose length the
// caller has checked, is the one step with no check inside (JYRO-ADR-101).
module internal BoundedText =

    // A string of `length` code units written in place by `write`, a chunk
    // at a time, with `between` before every chunk after the first
    // (JYRO-ADR-101 (2)).
    let create (length: int) (between: unit -> unit) (write: ChunkWriter) : string =
        if length = 0 then String.Empty
        else
            String.Create(length, write, Buffers.SpanAction<char, ChunkWriter>(fun span w ->
                let mutable at = 0
                while at < span.Length do
                    if at > 0 then between ()
                    let n = w.Invoke(span, at)
                    if n <= 0 then invalidOp "A chunk writer wrote nothing."
                    at <- at + n))

    // The text `text` holds, copied out of the builder's own chunks into the
    // string a piece of at most `ExecutionCheck.ChunkLength` code units at a
    // time, with `between` once at least a chunk has been copied since the
    // last: never one `StringBuilder.ToString` over a text longer than one
    // chunk (JYRO-ADR-101 (2)).
    let ofBuilder (text: Text.StringBuilder) (between: unit -> unit) : string =
        if text.Length <= ExecutionCheck.ChunkLength then text.ToString()
        else
            String.Create(text.Length, text, Buffers.SpanAction<char, Text.StringBuilder>(fun span builder ->
                let chunk = ExecutionCheck.ChunkLength
                let mutable at = 0
                let mutable checkedAt = 0
                for piece in builder.GetChunks() do
                    let mutable i = 0
                    while i < piece.Length do
                        if at - checkedAt >= chunk then
                            between ()
                            checkedAt <- at
                        let n = min chunk (piece.Length - i)
                        piece.Span.Slice(i, n).CopyTo(span.Slice(at, n))
                        i <- i + n
                        at <- at + n))

    // `left` followed by `right`, as `&` gives it (spec 40.4.1). Raises
    // `ValueSizeLimitExceeded` before it allocates when the result would be
    // above the maximum (spec 100.8.7, 100.8.8).
    let concat (left: string) (right: string) : string =
        let total = int64 left.Length + int64 right.Length
        ValueSize.Check total
        if right.Length = 0 then left
        elif left.Length = 0 then right
        else
            create (int total) ExecutionCheck.Check (ChunkWriter(fun destination at ->
                let source, offset = if at < left.Length then left, at else right, at - left.Length
                let n = min ExecutionCheck.ChunkLength (source.Length - offset)
                source.AsSpan(offset, n).CopyTo(destination.Slice(at, n))
                n))

    // Ordinal comparison of the UTF-16 code units of two strings (spec
    // 30.11.3): negative, zero or positive.
    let compareOrdinal (a: string) (b: string) : int =
        let common = min a.Length b.Length
        let chunk = ExecutionCheck.ChunkLength
        let mutable i = 0
        let mutable result = 0
        while result = 0 && i < common do
            if i > 0 then ExecutionCheck.Check()
            let n = min chunk (common - i)
            result <- String.CompareOrdinal(a, i, b, i, n)
            i <- i + n
        if result <> 0 then sign result
        else compare a.Length b.Length

    // True when the two strings hold the same code units.
    let equalsOrdinal (a: string) (b: string) : bool =
        if obj.ReferenceEquals(a, b) then true
        elif a.Length <> b.Length then false
        else
            let chunk = ExecutionCheck.ChunkLength
            let mutable i = 0
            let mutable equal = true
            while equal && i < a.Length do
                if i > 0 then ExecutionCheck.Check()
                let n = min chunk (a.Length - i)
                equal <- a.AsSpan(i, n).SequenceEqual(b.AsSpan(i, n))
                i <- i + n
            equal

    // True when the `part.Length` code units of `text` from `offset` equal
    // `part` by ordinal comparison, compared a chunk at a time with a check
    // between chunks. The caller has checked that they fit in `text`.
    let regionEquals (text: string) (offset: int) (part: string) : bool =
        let chunk = ExecutionCheck.ChunkLength
        let mutable i = 0
        let mutable equal = true
        while equal && i < part.Length do
            if i > 0 then ExecutionCheck.Check()
            let n = min chunk (part.Length - i)
            equal <- text.AsSpan(offset + i, n).SequenceEqual(part.AsSpan(i, n))
            i <- i + n
        equal

    // The end of the chunk of `text` that starts at `start`: at most
    // `ExecutionCheck.ChunkLength` code units, and never between the two
    // halves of a surrogate pair.
    let chunkEnd (text: string) (start: int) : int =
        let mutable finish = min text.Length (start + ExecutionCheck.ChunkLength)
        if finish < text.Length && finish - start > 1 && Char.IsHighSurrogate(text.[finish - 1]) then
            finish <- finish - 1
        finish

    // The length of the UTF-8 encoding of `text`, as
    // `Encoding.UTF8.GetByteCount` gives it, counted a chunk at a time with a
    // check between chunks.
    let utf8ByteCount (text: string) : int64 =
        let counter = Text.UTF8Encoding(false, false).GetEncoder()
        let mutable count = 0L
        let mutable i = 0
        while i < text.Length do
            if i > 0 then ExecutionCheck.Check()
            // Never between the two halves of a surrogate pair:
            // Encoder.GetByteCount keeps no state from one call to the next,
            // so a pair split across two chunks was counted as two unpaired
            // surrogates, three bytes each, instead of four bytes.
            let finish = chunkEnd text i
            count <- count + int64 (counter.GetByteCount(text.AsSpan(i, finish - i), true))
            i <- finish
        count

    // The UTF-8 encoding of `text`, as `Encoding.UTF8.GetBytes` gives it (an
    // unpaired surrogate becomes U+FFFD), made a chunk at a time with a check
    // between chunks: the length is counted first, then the bytes written.
    let utf8Bytes (text: string) : byte[] =
        let bytes = Array.zeroCreate<byte> (int (utf8ByteCount text))
        let writer = Text.UTF8Encoding(false, false).GetEncoder()
        let mutable written = 0
        let mutable i = 0
        while i < text.Length do
            if i > 0 then ExecutionCheck.Check()
            // The same chunks as the count, so each is a whole number of
            // characters and the two agree.
            let finish = chunkEnd text i
            written <- written + writer.GetBytes(text.AsSpan(i, finish - i), Span<byte>(bytes, written, bytes.Length - written), true)
            i <- finish
        bytes

    // The JSON escape whose backslash is at `at` in `source`, read no further
    // than `finish`: its first code unit, its second or -1, and the bytes it
    // takes; or a struct whose bytes taken is -1 where the escape is one that
    // `Utf8JsonReader.GetString` refuses. A `\u` escape of a low surrogate,
    // or of a high surrogate not followed by a `\u` escape of a low one, is
    // refused (JYRO-REQ-1179), as are an escape letter JSON does not have and
    // a `\u` escape without four hexadecimal digits.
    //
    // The one judgment of an escape, shared by `jsonString`, which decodes
    // a string, and by the reading that sizes a string the reader has not
    // handed over (JYRO-DEF-742).
    let tryJsonEscape (source: byte[]) (at: int) (finish: int) : struct (int * int * int) =
        let refused = struct (0, -1, -1)
        let hex (from: int) : int =
            if from + 4 > finish then -1
            else
                let mutable value = 0
                let mutable k = from
                while value >= 0 && k < from + 4 do
                    let b = int source.[k]
                    let digit =
                        if b >= int '0' && b <= int '9' then b - int '0'
                        elif b >= int 'a' && b <= int 'f' then b - int 'a' + 10
                        elif b >= int 'A' && b <= int 'F' then b - int 'A' + 10
                        else -1
                    value <- if digit < 0 then -1 else value * 16 + digit
                    k <- k + 1
                value
        if at + 1 >= finish then refused
        else
        match char source.[at + 1] with
        | '"' -> struct (int '"', -1, 2)
        | '\\' -> struct (int '\\', -1, 2)
        | '/' -> struct (int '/', -1, 2)
        | 'b' -> struct (8, -1, 2)
        | 'f' -> struct (12, -1, 2)
        | 'n' -> struct (10, -1, 2)
        | 'r' -> struct (13, -1, 2)
        | 't' -> struct (9, -1, 2)
        | 'u' ->
            let unit = hex (at + 2)
            if unit < 0 then refused
            elif unit >= 0xDC00 && unit <= 0xDFFF then refused
            elif unit >= 0xD800 && unit <= 0xDBFF then
                if at + 12 > finish || source.[at + 6] <> byte '\\' || source.[at + 7] <> byte 'u' then refused
                else
                    let low = hex (at + 8)
                    if low < 0xDC00 || low > 0xDFFF then refused
                    else struct (unit, low, 12)
            else struct (unit, -1, 6)
        | _ -> refused

    // The value of a JSON string token, as `Utf8JsonReader.GetString` gives
    // it, from the token's text between its quotes: the `length` bytes of
    // `source` from `start`, which the reader has already found to be a
    // well-formed string. Null when the value is longer than `limit` code
    // units.
    //
    // Unescaped and decoded from UTF-8 a chunk at a time with a check between
    // chunks (spec 100.2.5, JYRO-ADR-101 (5)): the code units are counted
    // first, over the whole token, then written in place. As `GetString`
    // does, a `\u` escape of a low surrogate, or of a high surrogate not
    // followed by a `\u` escape of a low one, raises
    // `InvalidOperationException`, wherever it is in the token; invalid UTF-8
    // raises `DecoderFallbackException`.
    let jsonString (source: byte[]) (start: int) (length: int) (limit: int64) : string | null =
        let finish = start + length
        let chunk = ExecutionCheck.ChunkLength
        let utf8 = Text.UTF8Encoding(false, true)
        // The escape at `at` (a backslash): its first code unit, its second
        // or -1, and the bytes it takes; one `GetString` refuses raises
        // `InvalidOperationException`, as `GetString` does.
        let escape (at: int) : struct (int * int * int) =
            let struct (_, _, taken) as read = tryJsonEscape source at finish
            if taken < 0 then
                raise (InvalidOperationException("The string holds an escape that GetString refuses, such as a \\u escape of an unpaired surrogate."))
            read
        let continuation (k: int) = k < finish && (source.[k] &&& 0xC0uy) = 0x80uy
        // The end of the run of unescaped text from `at`, within the chunk
        // that ends at `chunkFinish`: before the next backslash, or else at
        // the chunk's end, moved so it falls after a whole character
        // (`Encoding` keeps no state between two calls): back to the start of
        // the character the chunk's end cuts, or, when that is `at` itself
        // (an escape ended just before it), forward past that character. A
        // backslash is never part of a multi-byte UTF-8 sequence.
        let runEnd (at: int) (chunkFinish: int) : int =
            let found = Array.IndexOf(source, byte '\\', at, chunkFinish - at)
            if found >= 0 then found
            else
                let mutable e = chunkFinish
                let mutable back = 0
                while back < 3 && e > at && continuation e do
                    e <- e - 1
                    back <- back + 1
                if e = at then
                    e <- chunkFinish
                    let mutable ahead = 0
                    while ahead < 3 && continuation e do
                        e <- e + 1
                        ahead <- ahead + 1
                e
        // Pass 1: the number of code units, counted over the whole token.
        let mutable count = 0L
        let mutable i = start
        while i < finish do
            if i > start then ExecutionCheck.Check()
            let mutable chunkFinish = min finish (i + chunk)
            while i < chunkFinish do
                if source.[i] = byte '\\' then
                    let struct (_, second, taken) = escape i
                    count <- count + (if second >= 0 then 2L else 1L)
                    i <- i + taken
                else
                    let e = runEnd i chunkFinish
                    count <- count + int64 (utf8.GetCharCount(source, i, e - i))
                    // A run moved back from the chunk's end ends the chunk:
                    // the next chunk starts at the character it cut.
                    if e < chunkFinish && source.[e] <> byte '\\' then chunkFinish <- e
                    i <- e
        if count > limit then null
        else
            // Pass 2: the same chunks, written in place.
            String.Create(int count, source, Buffers.SpanAction<char, byte[]>(fun span bytes ->
                let mutable at = 0
                let mutable i = start
                while i < finish do
                    if i > start then ExecutionCheck.Check()
                    let mutable chunkFinish = min finish (i + chunk)
                    while i < chunkFinish do
                        if bytes.[i] = byte '\\' then
                            let struct (first, second, taken) = escape i
                            span.[at] <- char first
                            at <- at + 1
                            if second >= 0 then
                                span.[at] <- char second
                                at <- at + 1
                            i <- i + taken
                        else
                            let e = runEnd i chunkFinish
                            at <- at + utf8.GetChars(ReadOnlySpan<byte>(bytes, i, e - i), span.Slice(at))
                            if e < chunkFinish && bytes.[e] <> byte '\\' then chunkFinish <- e
                            i <- e))

// A stable merge sort that calls `tick` once per comparison.
//
// `List<T>.Sort` is not stable, and it wraps any exception its comparer
// raises in `InvalidOperationException`, so a time limit or a cancellation
// reached inside the comparer reached the script as `RuntimeError` (spec
// 100.4.3). This sort raises what the comparer or `tick` raises, unwrapped.
module internal StableSort =

    let sortInPlace (items: 'T[]) (comparer: 'T -> 'T -> int) (tick: unit -> unit) : unit =
        let n = items.Length
        if n > 1 then
            let mutable source = items
            let mutable target = Array.zeroCreate<'T> n
            let mutable width = 1
            while width < n do
                let mutable lo = 0
                while lo < n do
                    let mid = min (lo + width) n
                    let hi = min (lo + 2 * width) n
                    let mutable i = lo
                    let mutable j = mid
                    let mutable k = lo
                    while k < hi do
                        if i < mid && (j >= hi || (tick (); comparer source.[i] source.[j] <= 0)) then
                            target.[k] <- source.[i]
                            i <- i + 1
                        else
                            target.[k] <- source.[j]
                            j <- j + 1
                        k <- k + 1
                    lo <- hi
                let swap = source
                source <- target
                target <- swap
                width <- width * 2
            if not (obj.ReferenceEquals(source, items)) then
                Array.blit source 0 items 0 n

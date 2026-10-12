namespace RealTree.Jyro

open System
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.Threading

// Regular expression patterns read and matched as ECMA-262, 15th edition
// (ECMAScript 2024), gives them, with no flags and without Annex B (spec
// 10.6.2, 90.10.1, 90.10.5, 91.14.3 to 91.14.5, 96.2.19), by Jyro's own
// matcher (JYRO-ADR-142).
//
// A pattern is parsed by the grammar of 22.2.1 with [~UnicodeMode,
// ~UnicodeSetsMode, +NamedCaptureGroups] and its early errors, and compiled
// into a program for a backtracking machine whose instructions are the
// Matchers and MatcherContinuations of 22.2.2: a Disjunction tries its
// alternatives from the left; an Alternative runs its terms from the left,
// or from the right in a lookbehind, where every character, class and back
// reference is matched backward; RepeatMatcher (22.2.2.3.1) clears the
// captures of a quantified atom at the start of each iteration and fails an
// iteration that matches the empty string once the minimum is met; a
// lookaround, once it has succeeded, is not backtracked into, and keeps the
// captures of a positive one; a back reference to a group that took part in
// no match matches the empty string. The text and the pattern are read as
// UTF-16 code units, as they are without the `u` flag.
//
// The Unicode data the grammar reads (ID_Start and ID_Continue for group
// names and identity escapes, general category Zs for `\s`) is that of
// Unicode 15.0.0, from `Unicode15` (JYRO-REQ-9, JYRO-ADR-143).
module internal EcmaRegex =

    // The pattern is not valid ECMAScript: the regular expression functions
    // report it as `RegexInvalidPattern` (90.10.2).
    type InvalidPatternException() =
        inherit Exception("The pattern is not a valid ECMAScript pattern.")

    // One evaluation ran longer than the regex timeout: the regular
    // expression functions report it as `RegexTimeout` (90.10.3).
    type MatchTimeoutException() =
        inherit Exception("The pattern took longer than the regex timeout to match.")

    let private invalid () : 'T = raise (InvalidPatternException())

    let private isDecimalDigit (c: char) = c >= '0' && c <= '9'

    let private isHexDigit (c: char) =
        isDecimalDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    let private isAsciiLetter (c: char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    let private hexValue (c: char) =
        if c <= '9' then int c - int '0'
        elif c <= 'F' then int c - int 'A' + 10
        else int c - int 'a' + 10

    // ---------------------------------------------------------------------
    // The bound on reading a pattern (spec 100.2.5): a pattern is data from
    // the script, so its parse and its compile check the time limit and the
    // cancellation signal at a fixed interval, and no step of them runs over
    // a part of the pattern whose length the script chooses in one call.

    // Counts the units of work of a parse or a compile - a code unit read, a
    // term joined, an instruction laid out - and calls `check` once every
    // `ExecutionCheck.ChunkLength` of them.
    [<Sealed>]
    type private Meter(check: unit -> unit) =
        let mutable count = 0
        member _.Step() =
            count <- count + 1
            if count >= ExecutionCheck.ChunkLength then
                count <- 0
                check ()
        // `n` units at once: a block of working storage allocated, `n` its
        // entries (JYRO-ADR-185). A whole block, `ExecutionCheck.ChunkLength`
        // entries, always checks.
        member _.Add(n: int) =
            count <- count + n
            if count >= ExecutionCheck.ChunkLength then
                count <- 0
                check ()
        // The check itself, for a step that checks on its own.
        member _.Check : unit -> unit = check
        member _.CheckNow() = check ()

    // Ordinal string equality for a dictionary keyed by text from a script
    // (a group name, a pattern): the hash reads the length and at most one
    // chunk at each end, and an equality test compares a chunk at a time
    // with `check` between two chunks, so neither runs over the whole of a
    // long key in one call.
    [<Sealed>]
    type internal OrdinalComparer(check: unit -> unit) =
        interface IEqualityComparer<string> with
            member _.Equals(a: string, b: string) =
                if obj.ReferenceEquals(a, b) then true
                elif isNull a || isNull b || a.Length <> b.Length then false
                else
                    let chunk = ExecutionCheck.ChunkLength
                    let mutable i = 0
                    let mutable equal = true
                    while equal && i < a.Length do
                        if i > 0 then check ()
                        let n = min chunk (a.Length - i)
                        equal <- a.AsSpan(i, n).SequenceEqual(b.AsSpan(i, n))
                        i <- i + n
                    equal
            member _.GetHashCode(s: string) =
                if isNull s then 0
                else
                    let n = min s.Length ExecutionCheck.ChunkLength
                    HashCode.Combine(s.Length, String.GetHashCode(s.AsSpan(0, n)),
                                     String.GetHashCode(s.AsSpan(s.Length - n, n)))

    // A run of decimal digits of a pattern, read a digit at a time: where it
    // starts and ends, and its value saturated at `cap`, with whether the
    // value written is above `cap`. Nothing parses the whole run in one call.
    [<Struct>]
    type private DigitRun =
        { Start: int
          End: int
          Value: int64
          Over: bool }

    let private readDigits (meter: Meter) (p: string) (start: int) (cap: int64) : DigitRun =
        let mutable i = start
        let mutable value = 0L
        let mutable over = false
        while i < p.Length && isDecimalDigit p.[i] do
            meter.Step()
            if not over then
                let d = int64 (int p.[i] - int '0')
                if value > (cap - d) / 10L then
                    over <- true
                    value <- cap
                else value <- value * 10L + d
            i <- i + 1
        { Start = start; End = i; Value = value; Over = over }

    // The order of the values of two digit runs: negative, zero or
    // positive. Two runs above the cap are compared by their digits, the
    // leading zeros skipped, a chunk at a time with a check between chunks.
    let private compareRuns (meter: Meter) (p: string) (a: DigitRun) (b: DigitRun) : int =
        match a.Over, b.Over with
        | false, false -> compare a.Value b.Value
        | true, false -> 1
        | false, true -> -1
        | true, true ->
            let skipZeros (run: DigitRun) =
                let mutable k = run.Start
                while k < run.End && p.[k] = '0' do
                    meter.Step()
                    k <- k + 1
                k
            let sa = skipZeros a
            let sb = skipZeros b
            let la = a.End - sa
            let lb = b.End - sb
            if la <> lb then compare la lb
            else
                let chunk = ExecutionCheck.ChunkLength
                let mutable k = 0
                let mutable result = 0
                while result = 0 && k < la do
                    if k > 0 then meter.CheckNow()
                    let n = min chunk (la - k)
                    result <- sign (String.CompareOrdinal(p, sa + k, p, sb + k, n))
                    k <- k + n
                result

    // ---------------------------------------------------------------------
    // Sets of code units, as flat arrays of sorted, disjoint, inclusive
    // ranges [lo0; hi0; lo1; hi1; ...] over U+0000 to U+FFFF.

    let private normalize (ranges: seq<int * int>) : int[] =
        let sorted = ranges |> Seq.sortBy fst |> Array.ofSeq
        let result = List<int>()
        for (lo, hi) in sorted do
            let n = result.Count
            if n > 0 && lo <= result.[n - 1] + 1 then
                if hi > result.[n - 1] then result.[n - 1] <- hi
            else
                result.Add(lo)
                result.Add(hi)
        result.ToArray()

    // `normalize` over the ranges of a class of the pattern, which are as
    // many as the class is long: a sort while they are at most a chunk, and
    // otherwise a count of the ranges that start and end at each code unit
    // followed by one pass over the 65,536 code units, with a step for each
    // range and each code unit, so no call sorts a list whose length the
    // script chooses.
    let private normalizeMetered (meter: Meter) (ranges: List<int * int>) : int[] =
        if ranges.Count <= ExecutionCheck.ChunkLength then normalize ranges
        else
            let cover = Array.zeroCreate<int> 0x10001
            for (lo, hi) in ranges do
                meter.Step()
                cover.[lo] <- cover.[lo] + 1
                cover.[hi + 1] <- cover.[hi + 1] - 1
            let result = List<int>()
            let mutable depth = 0
            let mutable openAt = -1
            for c in 0 .. 0xFFFF do
                meter.Step()
                depth <- depth + cover.[c]
                if depth > 0 && openAt < 0 then openAt <- c
                elif depth = 0 && openAt >= 0 then
                    result.Add(openAt)
                    result.Add(c - 1)
                    openAt <- -1
            if openAt >= 0 then
                result.Add(openAt)
                result.Add(0xFFFF)
            result.ToArray()

    let private complement (set: int[]) : int[] =
        let result = List<int>()
        let mutable next = 0
        for k in 0 .. 2 .. set.Length - 1 do
            if set.[k] > next then
                result.Add(next)
                result.Add(set.[k] - 1)
            next <- set.[k + 1] + 1
        if next <= 0xFFFF then
            result.Add(next)
            result.Add(0xFFFF)
        result.ToArray()

    let private pairs (set: int[]) = seq { for k in 0 .. 2 .. set.Length - 1 -> (set.[k], set.[k + 1]) }

    let private contains (set: int[]) (c: int) : bool =
        let mutable lo = 0
        let mutable hi = set.Length / 2 - 1
        let mutable found = false
        while not found && lo <= hi do
            let mid = (lo + hi) >>> 1
            if c < set.[2 * mid] then hi <- mid - 1
            elif c > set.[2 * mid + 1] then lo <- mid + 1
            else found <- true
        found

    // `\d`: U+0030 to U+0039.
    let private digitSet = normalize [ (0x30, 0x39) ]
    // `\w`: WordCharacters with no flags, [A-Za-z0-9_].
    let private wordSet = normalize [ (0x30, 0x39); (0x41, 0x5A); (0x5F, 0x5F); (0x61, 0x7A) ]
    // `\s`: WhiteSpace (TAB, VT, FF, ZWNBSP and general category Zs of
    // Unicode 15.0.0) and LineTerminator (LF, CR, LS, PS).
    let private spaceSet =
        normalize
            [ yield (0x09, 0x0D)
              yield (0xFEFF, 0xFEFF)
              yield (0x2028, 0x2029)
              for c in 0 .. 0xFFFF do
                  if Unicode15.isSpaceSeparator c then yield (c, c) ]
    // `.` with no `s` flag: every code unit but a LineTerminator.
    let private dotSet = complement (normalize [ (0x0A, 0x0A); (0x0D, 0x0D); (0x2028, 0x2029) ])

    let private isWordUnit (c: char) =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

    // ---------------------------------------------------------------------
    // Working storage in blocks (JYRO-ADR-185 (1)). The matcher's register
    // file, backtrack stack and capture log, and the compiled program, are
    // sized by the pattern or by the work done, not by M, so none of them is
    // one array: each is a `Blocks`, whose blocks hold at most
    // `ExecutionCheck.ChunkLength` (4,096) entries and are allocated one at a
    // time with the budget (or the compile's meter) charged between them.

    // A sequence of entries held in blocks of `ExecutionCheck.ChunkLength`
    // entries (JYRO-ADR-185 (1)).
    //
    // - Entry `i` is entry `i &&& mask` of block `i >>> shift`. An index
    //   below one block reads the first block directly: a store of at most
    //   one block (almost every pattern's registers and program, and most
    //   stacks and logs) is one array read with no shift and no mask.
    // - Only the last block may be shorter than a whole block. Growing a
    //   short last block doubles it, up to one block: one allocation and one
    //   copy of at most one block. Otherwise a new whole block is allocated.
    //   Either way `charge` is called first with the entries allocated, so a
    //   whole block always reaches a check.
    // - The directory of blocks holds one reference per block and grows by
    //   doubling: a copy of at most size / 4,096 references.
    // - `Truncate` and `Clear` only lower the count: the blocks are kept,
    //   whole, to be written again. Nothing is copied or cleared.
    [<Sealed>]
    type internal Blocks<'T>(charge: int -> unit, firstLength: int) =
        static let blockLength = ExecutionCheck.ChunkLength
        static let shift = Numerics.BitOperations.Log2(uint32 blockLength)
        static let mask = blockLength - 1
        static do
            if 1 <<< shift <> blockLength then
                invalidOp "ExecutionCheck.ChunkLength must be a power of two."

        // The blocks: one reference each, the directory grown by doubling.
        let mutable directory : 'T[][] = Array.zeroCreate 1
        // The number of blocks allocated: in use, or kept after a truncation.
        let mutable blocks = 0
        // Block 0, read directly for an index below one block.
        let mutable first : 'T[] = Array.Empty<'T>()
        // The entries the allocated blocks hold.
        let mutable capacity = 0
        // The entries in use.
        let mutable count = 0

        // Allocates one block of `size` entries at the end, growing the
        // directory by doubling if it is full. Charges nothing itself.
        let newBlock (size: int) : 'T[] =
            if blocks = directory.Length then
                let grown = Array.zeroCreate<'T[]> (2 * directory.Length)
                Array.Copy(directory, grown, blocks)
                directory <- grown
            let block = Array.zeroCreate<'T> size
            directory.[blocks] <- block
            if blocks = 0 then first <- block
            blocks <- blocks + 1
            capacity <- capacity + size
            block

        // Room for one more entry: the short last block doubled (up to one
        // block), or a new block, with `charge` called first.
        let grow () =
            let last = if blocks = 0 then null else directory.[blocks - 1]
            if not (isNull last) && last.Length < blockLength then
                let size = min blockLength (2 * last.Length)
                charge size
                let block = Array.zeroCreate<'T> size
                Array.Copy(last, block, last.Length)
                directory.[blocks - 1] <- block
                if blocks = 1 then first <- block
                capacity <- capacity + size - last.Length
            else
                let size = if blocks = 0 then max 1 (min blockLength firstLength) else blockLength
                charge size
                newBlock size |> ignore

        // A store whose first block starts at 16 entries.
        new(charge: int -> unit) = Blocks<'T>(charge, 16)

        // A store of `length` entries, each `value`, made a block at a time:
        // every block but the last is a whole block, and `charge 1` is called
        // for each entry written, so a check falls between two blocks.
        static member Filled(length: int, value: 'T, charge: int -> unit) : Blocks<'T> =
            let store = Blocks<'T>(charge)
            store.Fill(length, value)
            store

        member private _.Fill(length: int, value: 'T) =
            while count < length do
                let block = newBlock (min blockLength (length - count))
                for k = 0 to block.Length - 1 do
                    charge 1
                    block.[k] <- value
                count <- count + block.Length

        // The number of entries in use.
        member _.Count = count

        member _.Item
            with get (i: int) : 'T =
                if i < blockLength then first.[i] else directory.[i >>> shift].[i &&& mask]
            and set (i: int) (value: 'T) =
                if i < blockLength then first.[i] <- value
                else directory.[i >>> shift].[i &&& mask] <- value

        // Appends `value`, allocating at most one block.
        member this.Add(value: 'T) =
            if count = capacity then grow ()
            this.[count] <- value
            count <- count + 1

        // Drops the entries from `length` on, keeping every block.
        member _.Truncate(length: int) = if length < count then count <- length

        // Drops every entry, keeping every block.
        member _.Clear() = count <- 0

    // ---------------------------------------------------------------------
    // The program.

    // An instruction. Every jump is relative to the instruction that holds
    // it. `back` is the direction of a lookbehind (22.2.2.7, 22.2.2.9).
    type internal Op =
        // A code unit, or a set of code units, matched in the direction.
        | Units of set: int[] * back: bool
        // Try `first`; on failure, `second`.
        | Split of first: int * second: int
        | Jump of offset: int
        // `^` and `$` with no `m` flag: the two ends of the text.
        | TextStart
        | TextEnd
        // `\b` and `\B`.
        | WordBoundary of negated: bool
        // The position where a capture group starts, kept in `reg`.
        | GroupOpen of reg: int
        // The capture of `group`, from the position in `reg` to here.
        | GroupClose of group: int * reg: int * back: bool
        // BackreferenceMatcher (22.2.2.7.2).
        | BackReference of group: int * back: bool
        // RepeatMatcher (22.2.2.3.1), in four parts: the count reset; the
        // choice between another iteration and the continuation; the start
        // of an iteration, which clears the captures of the atom; and its
        // end, which refuses an empty iteration once the minimum is met.
        | RepeatInit of counter: int
        | RepeatLoop of counter: int * min: int64 * max: int64 * greedy: bool * exit: int
        | RepeatIter of start: int * firstGroup: int * groupCount: int
        | RepeatNext of counter: int * start: int * min: int64 * loop: int
        // RepeatMatcher over an atom of one code unit with no capture in it,
        // which can never match empty: the same choices, kept as one entry.
        | UnitLoop of set: int[] * min: int64 * max: int64 * greedy: bool * back: bool
        // A lookaround (22.2.2.4): its body follows, ending in `LookEnd`, and
        // the continuation is `skip` on.
        | Look of negated: bool * skip: int
        | LookEnd
        | Succeed

    // A piece of a program, kept as a tree so that wrapping a group in a
    // quantifier or an alternation never copies the code inside it.
    type private Frag =
        | Leaf of Op
        | Seq of Frag[] * length: int

    let private lengthOf frag =
        match frag with
        | Leaf _ -> 1
        | Seq(_, n) -> n

    let private sequence (parts: Frag seq) =
        let a = Array.ofSeq parts
        Seq(a, a |> Array.sumBy lengthOf)

    // The instructions of `frag`, in order, without recursion, laid out
    // into the program with a step for each piece. The program is held in
    // blocks (JYRO-ADR-185 (1)), its length about 2M for a pattern of M code
    // units: its first block is sized to the program, up to one block, so a
    // program of at most 4,096 instructions is one array; a longer one gains
    // a block at a time, each charged to the meter, so a check falls between
    // two blocks.
    let private flatten (meter: Meter) (frag: Frag) : Blocks<Op> =
        let result = Blocks<Op>(meter.Add, lengthOf frag)
        let stack = Stack<Frag>()
        stack.Push(frag)
        while stack.Count > 0 do
            meter.Step()
            match stack.Pop() with
            | Leaf op -> result.Add(op)
            | Seq(parts, _) ->
                for k = parts.Length - 1 downto 0 do
                    meter.Step()
                    stack.Push(parts.[k])
        result

    // A term of an alternative: its code, and what a quantifier needs to
    // know of its atom.
    type private Piece =
        { Code: Frag
          // The set of an atom of one code unit, or null.
          Unit: int[]
          // The number of capture groups to the left of the atom, and the
          // number inside it (22.2.2.3, parenIndex and parenCount).
          ParenIndex: int
          ParenCount: int }

    // An open group while the pattern is parsed.
    type private Frame =
        { // 0 for the pattern itself, the group number of a capture group,
          // -1 for `(?:`, and -2 for a lookaround.
          Group: int
          Negated: bool
          // The direction of the terms inside: backward in a lookbehind.
          Back: bool
          ParenIndex: int
          Alternatives: List<List<Piece>> }

    // Reads a group name, `<` RegExpIdentifierName `>`, from `p` at `start`
    // (just after `<`): the name and the index after `>`. The name's code
    // points are ID_Start, then ID_Continue, of Unicode 15.0.0, or `$` and
    // `_`, and ZWNJ and ZWJ after the first (22.2.1, IdentifierStartChar and
    // IdentifierPartChar).
    let private readGroupName (meter: Meter) (p: string) (start: int) : string * int =
        let name = Text.StringBuilder()
        let mutable i = start
        let hex4 (at: int) =
            if at + 4 <= p.Length && Seq.forall isHexDigit (p.Substring(at, 4)) then
                Some (Convert.ToInt32(p.Substring(at, 4), 16))
            else None
        // One code point of the name, written or as a `\u` escape.
        let next () : int =
            meter.Step()
            if i >= p.Length then invalid ()
            let c = p.[i]
            if c = '\\' then
                if i + 1 >= p.Length || p.[i + 1] <> 'u' then invalid ()
                if i + 2 < p.Length && p.[i + 2] = '{' then
                    // `\u{` CodePoint `}`: one or more hexadecimal digits
                    // whose value is at most 10FFFF, read a digit at a time
                    // and saturated once it is above.
                    let mutable k = i + 3
                    let mutable value = 0
                    while k < p.Length && isHexDigit p.[k] do
                        meter.Step()
                        if value <= 0x10FFFF then value <- value * 16 + hexValue p.[k]
                        k <- k + 1
                    if k = i + 3 || k >= p.Length || p.[k] <> '}' then invalid ()
                    if value > 0x10FFFF then invalid ()
                    i <- k + 1
                    value
                else
                    match hex4 (i + 2) with
                    | None -> invalid ()
                    | Some lead when lead >= 0xD800 && lead <= 0xDBFF
                                     && i + 7 < p.Length && p.[i + 6] = '\\' && p.[i + 7] = 'u'
                                     && (match hex4 (i + 8) with Some t -> t >= 0xDC00 && t <= 0xDFFF | None -> false) ->
                        let trail = (hex4 (i + 8)).Value
                        i <- i + 12
                        Char.ConvertToUtf32(char lead, char trail)
                    | Some v ->
                        i <- i + 6
                        v
            elif Char.IsHighSurrogate c && i + 1 < p.Length && Char.IsLowSurrogate p.[i + 1] then
                i <- i + 2
                Char.ConvertToUtf32(c, p.[i - 1])
            else
                i <- i + 1
                int c
        let append (cp: int) =
            if cp >= 0x10000 then name.Append(Char.ConvertFromUtf32 cp) |> ignore
            else name.Append(char cp) |> ignore
        let first = next ()
        if not (first = int '$' || first = int '_' || Unicode15.isIdStart first) then invalid ()
        append first
        let mutable go = true
        while go do
            if i >= p.Length then invalid ()
            if p.[i] = '>' then
                i <- i + 1
                go <- false
            else
                let cp = next ()
                if not (cp = int '$' || cp = 0x200C || cp = 0x200D || Unicode15.isIdContinue cp) then invalid ()
                append cp
        (BoundedText.ofBuilder name meter.Check, i)

    // The capture groups of `p`, from the left: how many, and the number of
    // each named one. A name used twice is an early error in this edition,
    // in whatever alternatives the two groups are.
    let private scanGroups (meter: Meter) (p: string) : int * Dictionary<string, int> =
        let names = Dictionary<string, int>(OrdinalComparer(meter.Check))
        let mutable count = 0
        let mutable i = 0
        let mutable inClass = false
        while i < p.Length do
            meter.Step()
            match p.[i] with
            | '\\' -> i <- i + 2
            | '[' when not inClass -> inClass <- true; i <- i + 1
            | ']' when inClass -> inClass <- false; i <- i + 1
            | '(' when not inClass ->
                if i + 1 < p.Length && p.[i + 1] = '?' then
                    if i + 2 < p.Length && p.[i + 2] = '<'
                       && not (i + 3 < p.Length && (p.[i + 3] = '=' || p.[i + 3] = '!')) then
                        count <- count + 1
                        let name, after = readGroupName meter p (i + 3)
                        if names.ContainsKey name then invalid ()
                        names.[name] <- count
                        i <- after
                    else i <- i + 1
                else
                    count <- count + 1
                    i <- i + 1
            | _ -> i <- i + 1
        (count, names)

    // The parse of a pattern into a program, without recursion, so that the
    // depth of the groups of a pattern is bounded by nothing but its length.
    type private Compiler(p: string, groupCount: int, names: Dictionary<string, int>, meter: Meter) =
        let mutable i = 0
        let mutable captures = 0
        // Registers: the captures of groups 0..groupCount, two each; then the
        // start of each capture group; then two for each quantifier.
        let mutable registers = 2 * (groupCount + 1) + groupCount + 1

        let peek () = if i < p.Length then p.[i] else char 0xFFFF
        let atEnd () = i >= p.Length

        let single (c: int) = [| c; c |]

        // The value of a CharacterEscape that starts at `i` (after `\`), one
        // code unit, for the escapes an atom and a class both read.
        member private _.CharacterEscape() : int =
            if atEnd () then invalid ()
            let c = p.[i]
            match c with
            | 'f' -> i <- i + 1; 0x0C
            | 'n' -> i <- i + 1; 0x0A
            | 'r' -> i <- i + 1; 0x0D
            | 't' -> i <- i + 1; 0x09
            | 'v' -> i <- i + 1; 0x0B
            | 'c' ->
                if i + 1 < p.Length && isAsciiLetter p.[i + 1] then
                    let v = int p.[i + 1] % 32
                    i <- i + 2
                    v
                else invalid ()
            | '0' ->
                if i + 1 < p.Length && isDecimalDigit p.[i + 1] then invalid ()
                i <- i + 1
                0
            | 'x' ->
                if i + 2 < p.Length && isHexDigit p.[i + 1] && isHexDigit p.[i + 2] then
                    let v = Convert.ToInt32(p.Substring(i + 1, 2), 16)
                    i <- i + 3
                    v
                else invalid ()
            | 'u' ->
                if i + 4 < p.Length && Seq.forall isHexDigit (p.Substring(i + 1, 4)) then
                    let v = Convert.ToInt32(p.Substring(i + 1, 4), 16)
                    i <- i + 5
                    v
                else invalid ()
            | _ ->
                // IdentityEscape [~UnicodeMode]: a source character, here one
                // code unit, that is not ID_Continue of Unicode 15.0.0.
                if Unicode15.isIdContinue (int c) then invalid ()
                i <- i + 1
                int c

        // A ClassAtom: one code unit, or the set of a class escape.
        member private this.ClassAtom() : Choice<int, int[]> =
            let c = p.[i]
            if c = '\\' then
                i <- i + 1
                if atEnd () then invalid ()
                match p.[i] with
                | 'b' -> i <- i + 1; Choice1Of2 0x08
                | 'd' -> i <- i + 1; Choice2Of2 digitSet
                | 'D' -> i <- i + 1; Choice2Of2 (complement digitSet)
                | 'w' -> i <- i + 1; Choice2Of2 wordSet
                | 'W' -> i <- i + 1; Choice2Of2 (complement wordSet)
                | 's' -> i <- i + 1; Choice2Of2 spaceSet
                | 'S' -> i <- i + 1; Choice2Of2 (complement spaceSet)
                | _ -> Choice1Of2(this.CharacterEscape())
            else
                i <- i + 1
                Choice1Of2(int c)

        // A CharacterClass, from `[`: its set of code units.
        member private this.CharacterClass() : int[] =
            i <- i + 1
            let negated = peek () = '^'
            if negated then i <- i + 1
            let ranges = List<int * int>()
            let add atom =
                match atom with
                | Choice1Of2 u -> ranges.Add((u, u))
                | Choice2Of2 set -> ranges.AddRange(pairs set)
            let mutable closed = false
            while not closed do
                meter.Step()
                if atEnd () then invalid ()
                if peek () = ']' then
                    i <- i + 1
                    closed <- true
                else
                    let first = this.ClassAtom()
                    if peek () = '-' && i + 1 < p.Length && p.[i + 1] <> ']' then
                        i <- i + 1
                        let last = this.ClassAtom()
                        // A range of a class escape is an early error without
                        // Annex B, as is a range whose ends are out of order.
                        match first, last with
                        | Choice1Of2 a, Choice1Of2 b when a <= b -> ranges.Add((a, b))
                        | _ -> invalid ()
                    else add first
            let set = normalizeMetered meter ranges
            if negated then complement set else set

        // A quantifier after an atom, if one follows: min, max and greedy.
        member private _.Quantifier() : (int64 * int64 * bool) option =
            let quantifier =
                match peek () with
                | '*' -> i <- i + 1; Some (0L, Int64.MaxValue)
                | '+' -> i <- i + 1; Some (1L, Int64.MaxValue)
                | '?' -> i <- i + 1; Some (0L, 1L)
                | '{' ->
                    // Only a whole `{n}`, `{n,}` or `{n,m}`; any other `{` is
                    // an error without Annex B. A bound is read a digit at a
                    // time and saturated at Int64.MaxValue, which no count
                    // of iterations reaches.
                    let readNumber () =
                        let run = readDigits meter p i Int64.MaxValue
                        i <- run.End
                        if run.End = run.Start then None else Some run
                    i <- i + 1
                    let low = match readNumber () with Some n -> n | None -> invalid ()
                    // None for `{n}`; Some None for `{n,}`; Some (Some m) for `{n,m}`.
                    let high =
                        if peek () = ',' then
                            i <- i + 1
                            Some (readNumber ())
                        else None
                    if peek () <> '}' then invalid ()
                    i <- i + 1
                    match high with
                    | None -> Some (low.Value, low.Value)
                    | Some None -> Some (low.Value, Int64.MaxValue)
                    | Some (Some h) ->
                        // Out of order is an early error, decided on the
                        // values as written, not as saturated.
                        if compareRuns meter p low h > 0 then invalid ()
                        Some (low.Value, h.Value)
                | _ -> None
            match quantifier with
            | None -> None
            | Some (min, max) ->
                let greedy = peek () <> '?'
                if not greedy then i <- i + 1
                Some (min, max, greedy)

        // The code of `piece` under its quantifier, if one follows.
        member private this.Quantified(piece: Piece, back: bool) : Frag =
            match this.Quantifier() with
            | None -> piece.Code
            | Some (_, 0L, _) -> Seq([||], 0)
            | Some (min, max, greedy) when not (isNull piece.Unit) ->
                Leaf(UnitLoop(piece.Unit, min, max, greedy, back))
            | Some (min, max, greedy) ->
                let counter = registers
                let start = registers + 1
                registers <- registers + 2
                let body = lengthOf piece.Code
                sequence
                    [ Leaf(RepeatInit counter)
                      Leaf(RepeatLoop(counter, min, max, greedy, body + 3))
                      Leaf(RepeatIter(start, piece.ParenIndex + 1, piece.ParenCount))
                      piece.Code
                      Leaf(RepeatNext(counter, start, min, -(body + 2))) ]

        // The code of an alternation of `alternatives`, each run in the
        // direction `back` gives.
        member private _.Disjunction(alternatives: List<List<Piece>>, back: bool) : Frag =
            // The terms in the direction, with a step for each.
            let alternative (terms: List<Piece>) =
                let codes = Array.zeroCreate<Frag> terms.Count
                let mutable length = 0
                for k = 0 to terms.Count - 1 do
                    meter.Step()
                    let code = terms.[k].Code
                    codes.[if back then terms.Count - 1 - k else k] <- code
                    length <- length + lengthOf code
                Seq(codes, length)
            let mutable result = alternative alternatives.[alternatives.Count - 1]
            for k = alternatives.Count - 2 downto 0 do
                meter.Step()
                let left = alternative alternatives.[k]
                let rest = result
                result <-
                    sequence
                        [ Leaf(Split(1, lengthOf left + 2))
                          left
                          Leaf(Jump(lengthOf rest + 1))
                          rest ]
            result

        // The atom at `i` that is not a group, as a piece.
        member private this.SimpleAtom(back: bool) : Piece =
            let piece code unit = { Code = Leaf code; Unit = unit; ParenIndex = captures; ParenCount = 0 }
            let units set = piece (Units(set, back)) set
            match peek () with
            | '.' ->
                i <- i + 1
                units dotSet
            | '[' -> units (this.CharacterClass())
            | '\\' ->
                i <- i + 1
                if atEnd () then invalid ()
                match p.[i] with
                | 'd' -> i <- i + 1; units digitSet
                | 'D' -> i <- i + 1; units (complement digitSet)
                | 'w' -> i <- i + 1; units wordSet
                | 'W' -> i <- i + 1; units (complement wordSet)
                | 's' -> i <- i + 1; units spaceSet
                | 'S' -> i <- i + 1; units (complement spaceSet)
                | 'k' ->
                    if i + 1 < p.Length && p.[i + 1] = '<' then
                        let name, after = readGroupName meter p (i + 2)
                        match names.TryGetValue name with
                        | true, n ->
                            i <- after
                            piece (BackReference(n, back)) null
                        | _ -> invalid ()
                    else invalid ()
                | d when d >= '1' && d <= '9' ->
                    // The whole run of digits is the group number, read a
                    // digit at a time and saturated above any group count.
                    let run = readDigits meter p i (int64 Int32.MaxValue + 1L)
                    i <- run.End
                    if run.Over || run.Value > int64 groupCount then invalid ()
                    piece (BackReference(int run.Value, back)) null
                | _ -> units (single (this.CharacterEscape()))
            | '*' | '+' | '?' | '{' | '}' | ']' -> invalid ()
            | c ->
                i <- i + 1
                units (single (int c))

        // The program of the whole pattern, and the number of registers it
        // uses.
        member this.Compile() : Blocks<Op> * int =
            let frames = Stack<Frame>()
            let open' group negated back parenIndex =
                let alternatives = List<List<Piece>>()
                alternatives.Add(List<Piece>())
                frames.Push({ Group = group; Negated = negated; Back = back; ParenIndex = parenIndex; Alternatives = alternatives })
            open' 0 false false 0
            let current () = frames.Peek()
            let addTerm (piece: Piece) =
                let alternatives = current().Alternatives
                alternatives.[alternatives.Count - 1].Add(piece)
            let assertion op = addTerm { Code = Leaf op; Unit = null; ParenIndex = captures; ParenCount = 0 }
            while not (atEnd ()) do
                meter.Step()
                let back = current().Back
                match peek () with
                | '|' ->
                    i <- i + 1
                    current().Alternatives.Add(List<Piece>())
                | ')' ->
                    if frames.Count = 1 then invalid ()
                    i <- i + 1
                    let frame = frames.Pop()
                    let body = this.Disjunction(frame.Alternatives, frame.Back)
                    let outer = current().Back
                    if frame.Group = -2 then
                        // A lookaround is an Assertion, which no quantifier
                        // may follow without Annex B: a quantifier after it
                        // is read as an atom, and refused.
                        addTerm
                            { Code = sequence [ Leaf(Look(frame.Negated, lengthOf body + 2)); body; Leaf LookEnd ]
                              Unit = null
                              ParenIndex = captures
                              ParenCount = 0 }
                    else
                        let code =
                            if frame.Group > 0 then
                                let reg = 2 * (groupCount + 1) + frame.Group
                                sequence [ Leaf(GroupOpen reg); body; Leaf(GroupClose(frame.Group, reg, outer)) ]
                            else body
                        let piece = { Code = code; Unit = null; ParenIndex = frame.ParenIndex; ParenCount = captures - frame.ParenIndex }
                        addTerm { piece with Code = this.Quantified(piece, outer) }
                | '(' ->
                    if i + 1 < p.Length && p.[i + 1] = '?' then
                        if i + 2 >= p.Length then invalid ()
                        match p.[i + 2] with
                        | ':' ->
                            i <- i + 3
                            open' -1 false back captures
                        | '=' | '!' ->
                            let negated = p.[i + 2] = '!'
                            i <- i + 3
                            open' -2 negated false captures
                        | '<' when i + 3 < p.Length && (p.[i + 3] = '=' || p.[i + 3] = '!') ->
                            let negated = p.[i + 3] = '!'
                            i <- i + 4
                            open' -2 negated true captures
                        | '<' ->
                            let _, after = readGroupName meter p (i + 3)
                            i <- after
                            let before = captures
                            captures <- captures + 1
                            open' captures false back before
                        | _ -> invalid ()
                    else
                        i <- i + 1
                        let before = captures
                        captures <- captures + 1
                        open' captures false back before
                | '^' ->
                    i <- i + 1
                    assertion TextStart
                | '$' ->
                    i <- i + 1
                    assertion TextEnd
                | '\\' when i + 1 < p.Length && (p.[i + 1] = 'b' || p.[i + 1] = 'B') ->
                    let negated = p.[i + 1] = 'B'
                    i <- i + 2
                    assertion (WordBoundary negated)
                | _ ->
                    let piece = this.SimpleAtom(back)
                    addTerm { piece with Code = this.Quantified(piece, back) }
            if frames.Count <> 1 then invalid ()
            let root = frames.Pop()
            let program = sequence [ this.Disjunction(root.Alternatives, false); Leaf Succeed ]
            (flatten meter program, registers)

    // ---------------------------------------------------------------------
    // The machine.

    // The bound on one evaluation: the regex timeout (90.10.3, JYRO-ADR-15)
    // and the run's own limits and cancellation, checked by `check`, both
    // read once every so many steps.
    [<Sealed>]
    type private Budget(timeout: TimeSpan, check: unit -> unit) =
        let deadline =
            if timeout = Timeout.InfiniteTimeSpan || timeout <= TimeSpan.Zero then Int64.MaxValue
            else
                let ticks = timeout.TotalSeconds * float Stopwatch.Frequency
                let now = Stopwatch.GetTimestamp()
                if ticks >= float (Int64.MaxValue - now) then Int64.MaxValue else now + int64 ticks
        let mutable steps = 0L
        member _.Check() =
            if Stopwatch.GetTimestamp() > deadline then raise (MatchTimeoutException())
            check ()
        member this.Add(n: int64) =
            let before = steps
            steps <- steps + n
            if (before >>> 12) <> (steps >>> 12) then this.Check()

    [<Struct>]
    type private Entry =
        { // 0: a choice; 1: a lookaround; 2: a greedy UnitLoop that can give
          // back a unit; 3: a lazy UnitLoop that can take another.
          Kind: int
          Pc: int
          Pos: int
          Log: int
          // For a greedy UnitLoop, the furthest it may give back to; for a
          // lazy one, its count.
          Aux: int64 }

    // A compiled pattern.
    [<Sealed>]
    type Pattern internal (ops: Blocks<Op>, registers: int, groupCount: int) =

        // The number of capture groups.
        member _.GroupCount = groupCount

        // The first match of the pattern in `text` at `from` or after, as
        // RegExpBuiltinExec finds it with no flags (22.2.7.2): the start and
        // end of group 0, then of each capture group, -1 for one that took
        // part in no match; or null. Raises `MatchTimeoutException` when the
        // search runs longer than `timeout`, and whatever `check` raises.
        member _.Exec(text: string, from: int, timeout: TimeSpan, check: unit -> unit) : int[] =
            let budget = Budget(timeout, check)
            budget.Check()
            let n = text.Length
            // Every step whose length the pattern or the text chooses - a
            // fill, a copy, a compare - charges the budget one unit at a time
            // (or a chunk of at most ExecutionCheck.ChunkLength units), inside
            // the step, so no step runs more than one chunk past a check
            // (spec 100.2.5).
            //
            // The working storage is held in blocks of 4,096 entries
            // (JYRO-ADR-185 (1)), each block charged to the budget, so no
            // allocation or copy in the matcher is more than one block:
            // - the register file, about 5/3 M registers for a pattern of M
            //   code units, made a block at a time and filled with the budget
            //   charged per register;
            // - the backtrack stack and the capture log, whose size follows the
            //   work done, grown a block at a time with the budget charged a
            //   block's worth before each block, and truncated by count alone.
            let charge (entries: int) = budget.Add(int64 entries)
            let regs = Blocks<int64>.Filled(registers, -1L, charge)
            let stack = Blocks<Entry>(charge)
            let log = Blocks<struct (int * int64)>(charge)
            // At most one entry per lookaround of the pattern: bounded by M
            // (JYRO-ADR-185 (2)).
            let looks = Stack<int>()
            let captureSlots = 2 * (groupCount + 1)
            // A register is written through `set`, which keeps its old value
            // for as long as a choice could return to a state before it.
            let set (slot: int) (value: int64) =
                if regs.[slot] <> value then
                    if stack.Count > 0 then log.Add(struct (slot, regs.[slot]))
                    regs.[slot] <- value
            let undo (length: int) =
                for k = log.Count - 1 downto length do
                    budget.Add 1L
                    let struct (slot, old) = log.[k]
                    regs.[slot] <- old
                log.Truncate length
            let push kind pc pos aux = stack.Add({ Kind = kind; Pc = pc; Pos = pos; Log = log.Count; Aux = aux })
            let isWordAt (k: int) = k >= 0 && k < n && isWordUnit text.[k]
            // Whether the `length` code units at `a` and at `b` are equal,
            // compared a chunk at a time with the budget charged before each.
            let sameUnits (a: int) (b: int) (length: int) =
                let mutable i = 0
                let mutable same = true
                while same && i < length do
                    let k = min ExecutionCheck.ChunkLength (length - i)
                    budget.Add(int64 k)
                    same <- String.CompareOrdinal(text, a + i, text, b + i, k) = 0
                    i <- i + k
                same
            let mutable result : int[] = null
            let mutable start = from
            while isNull result && start <= n do
                budget.Add 1L
                for k = 0 to captureSlots - 1 do
                    budget.Add 1L
                    regs.[k] <- -1L
                stack.Clear()
                log.Clear()
                looks.Clear()
                let mutable pc = 0
                let mutable pos = start
                let mutable running = true
                while running do
                    budget.Add 1L
                    let mutable fail = false
                    match ops.[pc] with
                    | Units(units, back) ->
                        if back then
                            if pos > 0 && contains units (int text.[pos - 1]) then
                                pos <- pos - 1
                                pc <- pc + 1
                            else fail <- true
                        elif pos < n && contains units (int text.[pos]) then
                            pos <- pos + 1
                            pc <- pc + 1
                        else fail <- true
                    | Split(first, second) ->
                        push 0 (pc + second) pos 0L
                        pc <- pc + first
                    | Jump offset -> pc <- pc + offset
                    | TextStart -> if pos = 0 then pc <- pc + 1 else fail <- true
                    | TextEnd -> if pos = n then pc <- pc + 1 else fail <- true
                    | WordBoundary negated ->
                        if (isWordAt (pos - 1) <> isWordAt pos) <> negated then pc <- pc + 1 else fail <- true
                    | GroupOpen reg ->
                        set reg (int64 pos)
                        pc <- pc + 1
                    | GroupClose(group, reg, back) ->
                        // 22.2.2.8: the capture runs from where the group
                        // started to here, forward, or from here back to
                        // where it started in a lookbehind.
                        let other = regs.[reg]
                        if back then
                            set (2 * group) (int64 pos)
                            set (2 * group + 1) other
                        else
                            set (2 * group) other
                            set (2 * group + 1) (int64 pos)
                        pc <- pc + 1
                    | BackReference(group, back) ->
                        let s = regs.[2 * group]
                        let e = regs.[2 * group + 1]
                        if s < 0L || e < 0L then pc <- pc + 1
                        else
                            let length = int (e - s)
                            if back then
                                if pos - length >= 0 && sameUnits (int s) (pos - length) length then
                                    pos <- pos - length
                                    pc <- pc + 1
                                else fail <- true
                            elif pos + length <= n && sameUnits (int s) pos length then
                                pos <- pos + length
                                pc <- pc + 1
                            else fail <- true
                    | RepeatInit counter ->
                        set counter 0L
                        pc <- pc + 1
                    | RepeatLoop(counter, min, max, greedy, exit) ->
                        // Steps 1, 6, 7 and 8 of RepeatMatcher, with min and
                        // max less the iterations made so far.
                        let count = regs.[counter]
                        if count >= max then pc <- pc + exit
                        elif count < min then pc <- pc + 1
                        elif greedy then
                            push 0 (pc + exit) pos 0L
                            pc <- pc + 1
                        else
                            push 0 (pc + 1) pos 0L
                            pc <- pc + exit
                    | RepeatIter(startReg, firstGroup, groups) ->
                        // Steps 3 to 5: the captures of the atom are cleared.
                        set startReg (int64 pos)
                        for g = firstGroup to firstGroup + groups - 1 do
                            budget.Add 1L
                            set (2 * g) -1L
                            set (2 * g + 1) -1L
                        pc <- pc + 1
                    | RepeatNext(counter, startReg, min, loop) ->
                        // Step 2: an iteration that matched empty fails once
                        // the minimum is met.
                        let count = regs.[counter]
                        if count >= min && int64 pos = regs.[startReg] then fail <- true
                        else
                            set counter (count + 1L)
                            pc <- pc + loop
                    | UnitLoop(units, min, max, greedy, back) ->
                        let step = if back then -1 else 1
                        let fits (at: int) =
                            if back then at > 0 && contains units (int text.[at - 1])
                            else at < n && contains units (int text.[at])
                        let mutable count = 0L
                        let target = if greedy then max else min
                        while count < target && fits pos do
                            pos <- pos + step
                            count <- count + 1L
                            if count &&& 0xFFFL = 0L then budget.Add 4096L
                        if count < min then fail <- true
                        else
                            if greedy then
                                if count > min then
                                    // The furthest back it may give: the end
                                    // of the minimum.
                                    let floor = int64 pos - int64 step * (count - min)
                                    push 2 pc pos floor
                            else push 3 pc pos count
                            pc <- pc + 1
                    | Look(_, _) ->
                        looks.Push(stack.Count)
                        push 1 pc pos 0L
                        pc <- pc + 1
                    | LookEnd ->
                        // The body matched. The lookaround is not entered
                        // again: its choices are dropped, its captures kept.
                        let at = looks.Pop()
                        let entry = stack.[at]
                        stack.Truncate at
                        match ops.[entry.Pc] with
                        | Look(false, skip) ->
                            pos <- entry.Pos
                            pc <- entry.Pc + skip
                        | _ -> fail <- true
                    | Succeed ->
                        // The result: two per capture group, at most M + 2
                        // ints, one step as JYRO-ADR-185 (2) accepts.
                        let captures = Array.zeroCreate<int> captureSlots
                        captures.[0] <- start
                        captures.[1] <- pos
                        for k = 2 to captureSlots - 1 do
                            budget.Add 1L
                            captures.[k] <- int regs.[k]
                        result <- captures
                        running <- false
                    // Backtrack to the most recent choice that is left.
                    while fail do
                        if stack.Count = 0 then
                            fail <- false
                            running <- false
                        else
                            budget.Add 1L
                            let entry = stack.[stack.Count - 1]
                            stack.Truncate(stack.Count - 1)
                            undo entry.Log
                            match entry.Kind with
                            | 0 ->
                                pos <- entry.Pos
                                pc <- entry.Pc
                                fail <- false
                            | 1 ->
                                looks.Pop() |> ignore
                                match ops.[entry.Pc] with
                                | Look(true, skip) ->
                                    // The body of a negative lookaround found
                                    // no match, so the lookaround succeeds.
                                    pos <- entry.Pos
                                    pc <- entry.Pc + skip
                                    fail <- false
                                | _ -> ()
                            | 2 ->
                                match ops.[entry.Pc] with
                                | UnitLoop(_, _, _, _, back) ->
                                    let given = if back then entry.Pos + 1 else entry.Pos - 1
                                    if int64 given <> entry.Aux then push 2 entry.Pc given entry.Aux
                                    pos <- given
                                    pc <- entry.Pc + 1
                                    fail <- false
                                | _ -> ()
                            | _ ->
                                match ops.[entry.Pc] with
                                | UnitLoop(units, _, max, _, back) ->
                                    let fits =
                                        if back then entry.Pos > 0 && contains units (int text.[entry.Pos - 1])
                                        else entry.Pos < n && contains units (int text.[entry.Pos])
                                    if entry.Aux < max && fits then
                                        let taken = if back then entry.Pos - 1 else entry.Pos + 1
                                        push 3 entry.Pc taken (entry.Aux + 1L)
                                        pos <- taken
                                        pc <- entry.Pc + 1
                                        fail <- false
                                | _ -> ()
                start <- start + 1
            result

    // The pattern `pattern` compiled. Raises `InvalidPatternException` when
    // it is not a valid pattern of ECMA-262, 15th edition, with no flags and
    // without Annex B.
    //
    // The pattern is data from the script, so the parse and the compile
    // call `check` - the time limit and the cancellation signal of the run -
    // once every `ExecutionCheck.ChunkLength` units of work, and raise
    // whatever it raises (spec 100.2.5). No step reads a run of the pattern
    // whole: a run of digits is read a digit at a time and saturated.
    let compileChecked (pattern: string) (check: unit -> unit) : Pattern =
        let meter = Meter(check)
        let count, names = scanGroups meter pattern
        let ops, registers = Compiler(pattern, count, names, meter).Compile()
        Pattern(ops, registers, count)

    // `compileChecked` under the check the run installed on this thread
    // (`ExecutionCheck.Check`), which does nothing outside a run.
    let compile (pattern: string) : Pattern = compileChecked pattern ExecutionCheck.Check

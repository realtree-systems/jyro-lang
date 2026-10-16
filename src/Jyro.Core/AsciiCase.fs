namespace RealTree.Jyro

// The comparison "with no case sensitivity for ASCII letters" of spec 10.7.1,
// defined once for every keyword the language reads that way: the strings
// `true` and `false` of a boolean coercion (30.12.7, 30.12.8), the direction
// of `SortByField` (92.21.5 to 92.21.7), and the unit and part names of
// `DateAdd`, `DateDiff` and `DatePart` (94.5.5, 94.6.6, 94.7.5).
//
// Only the code units U+0041 to U+005A fold, each onto the code unit 0x20
// above it. No Unicode case mapping is read, so `String.ToLowerInvariant`,
// which maps U+212A (KELVIN SIGN) to `k`, and a comparison of the culture
// cannot stand in for it.
module internal AsciiCase =

    // The code unit an ASCII upper-case letter folds onto, and every other
    // code unit unchanged.
    let inline private fold (c: char) : char =
        if c >= 'A' && c <= 'Z' then char (int c + 0x20) else c

    // Whether `a` and `b` are equal with no case sensitivity for ASCII
    // letters (spec 10.7.1): the same length in UTF-16 code units, and at each
    // position the same code unit, or the upper-case and the lower-case form
    // of one ASCII letter. Both strings MUST be non-null.
    let equals (a: string) (b: string) : bool =
        if a.Length <> b.Length then false
        else
            let mutable same = true
            let mutable i = 0
            while same && i < a.Length do
                let x = a.[i]
                let y = b.[i]
                // Two different code units are equal only when one is an
                // ASCII upper-case letter and the other its lower-case form:
                // `fold` changes nothing else, so any other pair stays unequal.
                if x <> y && fold x <> fold y then same <- false
                i <- i + 1
            same

    // The first of `names` that `text` equals under `equals`, or `null` when
    // it equals none of them.
    let find (names: string[]) (text: string) : string | null =
        let mutable found : string | null = null
        let mutable i = 0
        while isNull found && i < names.Length do
            if equals text names.[i] then found <- names.[i]
            i <- i + 1
        found

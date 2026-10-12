namespace RealTree.Jyro

open System
open System.Collections.Generic

// Which elements of an array equal, by the script's `==` (30.10,
// `EqualsValue`), an element at a lower position, found with O(n log n)
// comparisons on every input but one that only a host can make
// (JYRO-ADR-173, amending JYRO-ADR-172). `firstOccurrences` serves
// `ValidateSchema`'s `uniqueItems` (96.2.4), whose data is never deeper than
// the maximum nesting depth; `distinctKeep` serves `Distinct` (92.10), with
// a key read only down to that depth (JYRO-ADR-174, JYRO-REQ-1708).
//
// 30.10's number equality is exact when both numbers are exact and in
// double precision otherwise, so it is not transitive (JYRO-ADR-34): exact
// 0.1 equals an inexact 0.1, which equals exact
// 0.1000000000000000000000000001, but the two exact numbers differ. No order
// consistent with it exists, so the elements are first sorted by a coarser
// total preorder P in which every equal pair is P-equal, and equality is then
// decided inside each run of P-equal elements.
//
// P: null, booleans, numbers, strings, arrays, objects, by that rank of
// type; `false` before `true`; a number by its double (`JyroNumber.Value`),
// compared with `<` and `>`, never by bit pattern, so -0 and 0 are P-equal;
// strings by ordinal UTF-16 code units, a chunk at a time with a check
// between chunks; arrays by length, then element by element under P;
// objects by their number of properties, then their keys sorted ordinally,
// then their values in that key order under P. Two values equal by 30.10
// have equal doubles at every number leaf, the same lengths, keys and
// counts, so they are P-equal.
//
// Within a run, in input order (the sort is stable): a run of numbers
// decides each member at once (an inexact number equals every number of its
// double; two exact numbers are equal when their decimals are); a run of
// null, booleans or strings is one value repeated; a run of arrays or
// objects compares each member with `EqualsValue` against every member at a
// lower position, which is pairwise only inside the run.
//
// Every comparison of two values, at any depth, is one count, as
// `EqualsValue` counts an element. A comparison of two containers enters the
// value-nesting guard as `==` does, starting at the two compared values
// (100.7.4, JYRO-REQ-1705), so it raises `ValueNestingLimitExceeded` where
// the comparison goes deeper than the maximum nesting depth (30.10.11).
module internal EqualRuns =

    let private valueOf (v: JyroValue) =
        if obj.ReferenceEquals(v, null) then JyroNull.Instance :> JyroValue else v

    let private rank (v: JyroValue) =
        match v.ValueType with
        | JyroValueType.Null -> 0
        | JyroValueType.Boolean -> 1
        | JyroValueType.Number -> 2
        | JyroValueType.String -> 3
        | JyroValueType.Array -> 4
        | JyroValueType.Object -> 5
        | _ -> 6

    // An array of `n` elements, the one at `i` being `value i`, filled one
    // element to a count rather than in one unchecked call over the input's
    // length (spec 100.2.5).
    let private filled (ticker: Bounds.Ticker) (n: int) (value: int -> 'T) : 'T[] =
        let a = Array.zeroCreate<'T> n
        for i = 0 to n - 1 do
            ticker.Tick()
            a.[i] <- value i
        a

    // For each element of `items`, true when it is not equal by 30.10 to any
    // element at a lower position: the elements `Distinct` keeps (REQ-670,
    // 672), and, when any is false, a duplicate for `uniqueItems`.
    let firstOccurrences (ticker: Bounds.Ticker) (items: IReadOnlyList<JyroValue>) : bool[] =
        let n = items.Count
        let keep = filled ticker n (fun _ -> true)
        let sortedKeys = Dictionary<JyroObject, string[]>(HashIdentity.Reference)
        let keysOf (o: JyroObject) =
            match sortedKeys.TryGetValue o with
            | true, keys -> keys
            | _ ->
                let keys = Array.zeroCreate<string> o.Count
                let mutable i = 0
                for kvp in o.Properties do
                    ticker.Tick()
                    keys.[i] <- kvp.Key
                    i <- i + 1
                StableSort.sortInPlace keys (fun x y -> BoundedText.compareOrdinal x y) ticker.Tick
                sortedKeys.[o] <- keys
                keys
        let rec project (x: JyroValue) (y: JyroValue) : int =
            ticker.Tick()
            let rx = rank x
            let ry = rank y
            if rx <> ry then compare rx ry
            else
                match x, y with
                | (:? JyroBoolean as a), (:? JyroBoolean as b) -> compare a.Value b.Value
                | (:? JyroNumber as a), (:? JyroNumber as b) ->
                    let p = a.Value
                    let q = b.Value
                    if p < q then -1 elif p > q then 1 else 0
                | (:? JyroString as a), (:? JyroString as b) -> BoundedText.compareOrdinal a.Value b.Value
                | (:? JyroArray as a), (:? JyroArray as b) ->
                    if a.Count <> b.Count then compare a.Count b.Count
                    else
                        ValueRecursionGuard.Enter()
                        try
                            let mutable result = 0
                            let mutable i = 0
                            while result = 0 && i < a.Count do
                                result <- project (valueOf a.Items.[i]) (valueOf b.Items.[i])
                                i <- i + 1
                            result
                        finally
                            ValueRecursionGuard.Exit()
                | (:? JyroObject as a), (:? JyroObject as b) ->
                    if a.Count <> b.Count then compare a.Count b.Count
                    else
                        ValueRecursionGuard.Enter()
                        try
                            let ka = keysOf a
                            let kb = keysOf b
                            let mutable result = 0
                            let mutable i = 0
                            while result = 0 && i < ka.Length do
                                ticker.Tick()
                                result <- BoundedText.compareOrdinal ka.[i] kb.[i]
                                i <- i + 1
                            i <- 0
                            while result = 0 && i < ka.Length do
                                result <- project (valueOf a.Properties.[ka.[i]]) (valueOf b.Properties.[kb.[i]])
                                i <- i + 1
                            result
                        finally
                            ValueRecursionGuard.Exit()
                // A function equals only itself (30.10): every function is
                // P-equal to every other, and its run decides by EqualsValue.
                | _ -> 0
        let order = filled ticker n id
        StableSort.sortInPlace order (fun i j -> project (valueOf items.[i]) (valueOf items.[j])) ticker.Tick
        // Decides the run order.[start .. finish - 1], whose members are in
        // input order.
        let decide (start: int) (finish: int) =
            match valueOf items.[order.[start]] with
            | :? JyroNumber ->
                let exacts = HashSet<decimal>()
                let mutable inexactBelow = false
                for k = start to finish - 1 do
                    ticker.Tick()
                    let number = items.[order.[k]] :?> JyroNumber
                    let equalBelow =
                        k > start
                        && (inexactBelow || not number.IsExact || exacts.Contains number.DecimalValue)
                    keep.[order.[k]] <- not equalBelow
                    if number.IsExact then exacts.Add(number.DecimalValue) |> ignore
                    else inexactBelow <- true
            | :? JyroArray | :? JyroObject | :? JyroFunction ->
                for k = start + 1 to finish - 1 do
                    let item = valueOf items.[order.[k]]
                    let mutable equalBelow = false
                    let mutable m = start
                    while not equalBelow && m < k do
                        ticker.Tick()
                        if item.EqualsValue(valueOf items.[order.[m]]) then equalBelow <- true
                        m <- m + 1
                    keep.[order.[k]] <- not equalBelow
            | _ ->
                for k = start + 1 to finish - 1 do
                    ticker.Tick()
                    keep.[order.[k]] <- false
        let mutable start = 0
        while start < n do
            let mutable finish = start + 1
            while finish < n && project (valueOf items.[order.[finish - 1]]) (valueOf items.[order.[finish]]) = 0 do
                finish <- finish + 1
            if finish - start > 1 then decide start finish
            start <- finish
        keep

    // For each element of `items`, true when `Distinct` keeps it
    // (JYRO-REQ-1708, JYRO-ADR-174, amending JYRO-ADR-173 (4) and (5)).
    //
    // The elements are sorted stably by a key K that orders exactly the
    // values that are *alike above the bound*: P above, read only down to the
    // maximum nesting depth N (`ValueRecursionGuard.MaxDepth`), each element
    // counted as depth 1 as the walk of `==` counts it (100.7.4). An array or
    // an object at depth N + 1 or more is equal under K to every other of its
    // type, whatever it holds, and is not read; a function is equal to every
    // other function. So computing K reads nothing past the bound, a cyclic
    // element included, and never raises, and two values that `==` finds
    // equal without raising are alike.
    //
    // Within each run of alike elements, in ascending position (the sort is
    // stable), each element is compared by `==` (`EqualsValue`, with its own
    // depth guard) with each lower element of its run in ascending position,
    // the lower element as the left operand, until one comparison is true; it
    // is omitted if one is, and kept otherwise. No other pair is compared, so
    // `ValueNestingLimitExceeded` comes only from one of these comparisons.
    // Every comparison, of keys at any depth and by `==`, is one count.
    let distinctKeep (ticker: Bounds.Ticker) (items: IReadOnlyList<JyroValue>) : bool[] =
        let n = items.Count
        let keep = filled ticker n (fun _ -> true)
        let bound = ValueRecursionGuard.MaxDepth
        let sortedKeys = Dictionary<JyroObject, string[]>(HashIdentity.Reference)
        let keysOf (o: JyroObject) =
            match sortedKeys.TryGetValue o with
            | true, keys -> keys
            | _ ->
                let keys = Array.zeroCreate<string> o.Count
                let mutable i = 0
                for kvp in o.Properties do
                    ticker.Tick()
                    keys.[i] <- kvp.Key
                    i <- i + 1
                StableSort.sortInPlace keys (fun x y -> BoundedText.compareOrdinal x y) ticker.Tick
                sortedKeys.[o] <- keys
                keys
        // K of `x` and `y`, both at `depth`. Recursion is at most N + 1
        // frames deep, whatever the values hold.
        let rec alike (depth: int) (x: JyroValue) (y: JyroValue) : int =
            ticker.Tick()
            let rx = rank x
            let ry = rank y
            if rx <> ry then compare rx ry
            else
                match x, y with
                | (:? JyroBoolean as a), (:? JyroBoolean as b) -> compare a.Value b.Value
                | (:? JyroNumber as a), (:? JyroNumber as b) ->
                    let p = a.Value
                    let q = b.Value
                    if p < q then -1 elif p > q then 1 else 0
                | (:? JyroString as a), (:? JyroString as b) -> BoundedText.compareOrdinal a.Value b.Value
                | (:? JyroArray), (:? JyroArray) when depth > bound -> 0
                | (:? JyroObject), (:? JyroObject) when depth > bound -> 0
                | (:? JyroArray as a), (:? JyroArray as b) ->
                    if a.Count <> b.Count then compare a.Count b.Count
                    else
                        let mutable result = 0
                        let mutable i = 0
                        while result = 0 && i < a.Count do
                            result <- alike (depth + 1) (valueOf a.Items.[i]) (valueOf b.Items.[i])
                            i <- i + 1
                        result
                | (:? JyroObject as a), (:? JyroObject as b) ->
                    if a.Count <> b.Count then compare a.Count b.Count
                    else
                        let ka = keysOf a
                        let kb = keysOf b
                        let mutable result = 0
                        let mutable i = 0
                        while result = 0 && i < ka.Length do
                            ticker.Tick()
                            result <- BoundedText.compareOrdinal ka.[i] kb.[i]
                            i <- i + 1
                        i <- 0
                        while result = 0 && i < ka.Length do
                            result <- alike (depth + 1) (valueOf a.Properties.[ka.[i]]) (valueOf b.Properties.[kb.[i]])
                            i <- i + 1
                        result
                // Both of another type, of the same type: alike.
                | _ -> 0
        let key (i: int) (j: int) = alike 1 (valueOf items.[i]) (valueOf items.[j])
        let order = filled ticker n id
        StableSort.sortInPlace order key ticker.Tick
        let mutable start = 0
        while start < n do
            let mutable finish = start + 1
            while finish < n && key order.[finish - 1] order.[finish] = 0 do
                finish <- finish + 1
            for k = start + 1 to finish - 1 do
                let item = valueOf items.[order.[k]]
                let mutable equalBelow = false
                let mutable m = start
                while not equalBelow && m < k do
                    ticker.Tick()
                    if (valueOf items.[order.[m]]).EqualsValue(item) then equalBelow <- true
                    m <- m + 1
                keep.[order.[k]] <- not equalBelow
            start <- finish
        keep

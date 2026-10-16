namespace RealTree.Jyro

open System
open System.Collections.Generic
open System.Security.Cryptography

module internal ArrayFunctions =

    /// <summary>Gives the size of a string, an array or an object.</summary>
    /// <arg name="source">The value to measure. A boolean, a number or a function is a runtime error.</arg>
    /// <returns>For a string, its length in UTF-16 code units; for an array, its number of elements; for an object, its number of properties; for null, 0. A whole number, exact. Never null.</returns>
    /// <example><c>Length("abc")</c> returns 3; <c>Length([1, 2])</c> returns 2; <c>Length(null)</c> returns 0.</example>
    type LengthFunction() =
        inherit JyroFunctionBase("Length", Signatures.unary "source" ParameterType.Any ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let value = args.[0]
            let length =
                match value with
                | :? JyroString as s -> s.Length
                | :? JyroArray as a -> a.Count
                | :? JyroObject as o -> o.Count
                | :? JyroNull -> 0
                // A boolean, a number or a function has no size (92.1.6):
                // the 1 it used to give hid a script passing the wrong value.
                // The accepted types are the code's template; the arguments
                // are the function and the actual type (JYRO-ADR-164 item 3).
                | other ->
                    JyroError.raiseRuntime MessageCode.StringArrayOrObjectRequired
                        [| box (this.Name + "()"); box (TypeNames.typeOfName other.ValueType) |]
            // A count, so exact (92.1.7, 30.2.3).
            JyroNumber(length) :> JyroValue

    /// <summary>Adds a value to the end of an array.</summary>
    /// <arg name="items">The array to add to, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="value">The value to add, of any type, null included.</arg>
    /// <returns>A new array holding the elements of items in order, followed by value. Never null.</returns>
    /// <example><c>Append([1, 2], 3)</c> returns <c>[1, 2, 3]</c>.</example>
    type AppendFunction() =
        inherit JyroFunctionBase("Append", Signatures.binary "items" ParameterType.Array "value"ParameterType.Any ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let value = args.[1]
            Bounds.requireSize (int64 arr.Count + 1L)
            let items = arr.Items
            let result = Bounds.copyOf (Bounds.Ticker ctx) items.Count (fun i -> items.[i])
            result.Add(value)
            result :> JyroValue

    /// <summary>Adds a value to the start of an array.</summary>
    /// <arg name="items">The array to add to, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="value">The value to add, of any type, null included.</arg>
    /// <returns>A new array holding value, followed by the elements of items in order. Never null.</returns>
    /// <example><c>Prepend([2, 3], 1)</c> returns <c>[1, 2, 3]</c>.</example>
    type PrependFunction() =
        inherit JyroFunctionBase("Prepend", Signatures.binary "items" ParameterType.Array "value"ParameterType.Any ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let value = args.[1]
            Bounds.requireSize (int64 arr.Count + 1L)
            let items = arr.Items
            let ticker = Bounds.Ticker ctx
            let result = JyroArray(items.Count + 1)
            result.Add(value)
            for i = 0 to items.Count - 1 do
                ticker.Tick()
                result.Add(items.[i])
            result :> JyroValue

    /// <summary>Gives the first element of an array.</summary>
    /// <arg name="items">The array to read. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>The element at position 0. Null when items is empty or null, and when its first element is itself null.</returns>
    /// <example><c>First([7, 8, 9])</c> returns 7; <c>First([])</c> returns null.</example>
    type FirstFunction() =
        inherit JyroFunctionBase("First", Signatures.unary "items" ParameterType.Array ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let arr = args.GetArray(0)
            if arr.Items.Count > 0 then arr.Items.[0]
            else JyroNull.Instance :> JyroValue

    /// <summary>Gives the last element of an array.</summary>
    /// <arg name="items">The array to read. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>The element at the highest position. Null when items is empty or null, and when its last element is itself null.</returns>
    /// <example><c>Last([7, 8, 9])</c> returns 9; <c>Last([])</c> returns null.</example>
    type LastFunction() =
        inherit JyroFunctionBase("Last", Signatures.unary "items" ParameterType.Array ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let arr = args.GetArray(0)
            if arr.Items.Count > 0 then arr.Items.[arr.Items.Count - 1]
            else JyroNull.Instance :> JyroValue

    /// <summary>Finds the position of the first occurrence of a piece of text in a string, or of a value in an array.</summary>
    /// <arg name="source">The string or array to search. Null holds nothing. A boolean, a number, an object or a function is a runtime error.</arg>
    /// <arg name="value">What to look for. In an array, any value, null included, found at the first element equal to it by value. In a string, a string found by exact match, case included; null is found in no string, and any other type is a runtime error.</arg>
    /// <returns>The position of the first match, counted from 0: in UTF-16 code units for a string, in elements for an array. -1 when value is not found, or when source is null. A whole number, exact. Never null.</returns>
    /// <example><c>IndexOf("banana", "na")</c> returns 2; <c>IndexOf([10, 20, 30], 30)</c> returns 2; <c>IndexOf([1, 2], 5)</c> returns -1.</example>
    type IndexOfFunction() =
        inherit JyroFunctionBase("IndexOf", Signatures.binary "source" ParameterType.Any "value"ParameterType.Any ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override _.Execute(args, ctx) =
            let source = args.[0]
            let search = args.[1]
            let ticker = Bounds.Ticker ctx
            // Every position is a count of code units or elements, so exact,
            // -1 included (92.6.10, 30.2.3).
            match source with
            | :? JyroString as sourceStr ->
                match search with
                | :? JyroString as searchStr ->
                    let pos = Bounds.indexOfOrdinal ticker sourceStr.Value searchStr.Value 0
                    JyroNumber(pos) :> JyroValue
                // A string searched for null is "not found" (92.6.7).
                | _ when search.IsNull -> JyroNumber(-1) :> JyroValue
                | _ -> JyroError.raiseRuntime MessageCode.StringOrArrayRequired [| box "IndexOf()" |]
            | :? JyroArray as arr ->
                let mutable index = -1
                let mutable i = 0
                while i < arr.Count && index < 0 do
                    ticker.Tick()
                    if arr.Items.[i].EqualsValue(search) then
                        index <- i
                    i <- i + 1
                JyroNumber(index) :> JyroValue
            // An absent source is "not found", as for Contains (92.6.7).
            | _ when source.IsNull -> JyroNumber(-1) :> JyroValue
            // A boolean, a number, an object or a function has no positions,
            // whatever `value` is, null included (92.6.9): `source` is
            // checked before `value` is read (90.3.8).
            | _ ->
                JyroError.raiseRuntime MessageCode.StringOrArrayRequired [| box "IndexOf()" |]

    /// <summary>Reverses the order of the elements of an array.</summary>
    /// <arg name="items">The array to reverse, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array holding the elements of items from the last to the first. Never null.</returns>
    /// <example><c>Reverse([1, 2, 3])</c> returns <c>[3, 2, 1]</c>.</example>
    type ReverseFunction() =
        inherit JyroFunctionBase("Reverse", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let items = arr.Items
            let n = items.Count
            Bounds.copyOf (Bounds.Ticker ctx) n (fun i -> items.[n - 1 - i]) :> JyroValue

    /// <summary>Takes the elements of an array between two positions.</summary>
    /// <arg name="items">The array to take from, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="start">The position of the first element to take, counted from 0. A whole number of 0 or more; a negative or fractional number, or null, is a runtime error. A start at or past the end gives the empty array.</arg>
    /// <arg name="finish">The position just after the last element to take, so the element at finish is not taken. A whole number of 0 or more; a negative or fractional number is a runtime error. A finish past the end is read as the end, and a finish at or before start gives the empty array. When it is left out, or null, the elements are taken to the end of items.</arg>
    /// <returns>A new array holding the elements of items from start up to, but not including, finish, in order. Never null.</returns>
    /// <example><c>Slice([10, 20, 30, 40], 1, 3)</c> returns <c>[20, 30]</c>; <c>Slice([10, 20, 30, 40], 2)</c> returns <c>[30, 40]</c>.</example>
    type SliceFunction() =
        inherit JyroFunctionBase("Slice",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("start", ParameterType.Number)
                  Parameter.Optional("finish", ParameterType.Number) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            // Both endpoints are non-negative: a negative start used to clamp
            // to zero and silently return the whole array, which is the same
            // class of silent acceptance as a fractional index. The upper end
            // still clamps - Slice(xs, 0, 1000) meaning "to the end" is a
            // deliberate idiom, and clamping a range is not the same as
            // accepting a nonsense position.
            let start = JyroArguments.ReadNonNegativeInt32(args, 1, "Slice()", "start")
            let endIdx =
                // A `finish` of null is a finish not given (90.13.1).
                if not (Bounds.isAbsent args 2) then JyroArguments.ReadNonNegativeInt32(args, 2, "Slice()", "finish")
                else arr.Items.Count
            let actualStart = min arr.Items.Count start
            let actualEnd = min arr.Items.Count endIdx
            let items = arr.Items
            Bounds.copyOf (Bounds.Ticker ctx) (max 0 (actualEnd - actualStart)) (fun i -> items.[actualStart + i]) :> JyroValue

    /// <summary>Joins two arrays into one.</summary>
    /// <arg name="first">The array whose elements come first, left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="second">The array whose elements come after those of first, left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array holding the elements of first in order, followed by the elements of second in order. Never null.</returns>
    /// <example><c>Concatenate([1, 2], [3])</c> returns <c>[1, 2, 3]</c>.</example>
    type ConcatenateFunction() =
        inherit JyroFunctionBase("Concatenate", Signatures.binary "first" ParameterType.Array "second"ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr1 = args.GetArray(0)
            let arr2 = args.GetArray(1)
            let first = arr1.Items
            let second = arr2.Items
            // The length is known once both arguments are checked, and no
            // later error can occur, so it is refused before anything is
            // copied (spec 100.8.8, 100.8.11).
            Bounds.requireSize (int64 first.Count + int64 second.Count)
            Bounds.copyOf (Bounds.Ticker ctx) (first.Count + second.Count) (fun i ->
                if i < first.Count then first.[i] else second.[i - first.Count]) :> JyroValue

    /// <summary>Removes the repeated elements of an array, keeping the first of each.</summary>
    /// <arg name="items">The array to read, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array holding each element of items that is not equal, by the <c>==</c> operator, to any element before it, in their original order. Never null.</returns>
    /// <example><c>Distinct([1, 2, 1, 3, 2])</c> returns <c>[1, 2, 3]</c>.</example>
    type DistinctFunction() =
        inherit JyroFunctionBase("Distinct", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let ticker = Bounds.Ticker ctx
            // Each element that equals no element at a lower position, kept
            // or not (92.10.2, 92.10.4), by `==` (92.10.3), which is not
            // transitive across exact and inexact numbers (JYRO-ADR-34): each
            // element is compared by `==` only with the lower elements alike
            // to it above the nesting bound, found by a sort whose key never
            // raises, with a count per comparison (JYRO-REQ-1708,
            // JYRO-ADR-174, EqualRuns.fs). The kept elements themselves, in
            // input order (92.10.10).
            let keep = EqualRuns.distinctKeep ticker arr.Items
            let result = JyroArray()
            for i = 0 to arr.Count - 1 do
                ticker.Tick()
                if keep.[i] then result.AddBounded(arr.Items.[i])
            result :> JyroValue

    /// <summary>Sorts the elements of an array in ascending order.</summary>
    /// <remarks>Values of different types never cause an error: null comes first, then booleans, numbers, strings, arrays, objects and functions. Booleans sort false before true, numbers by value, and strings by their UTF-16 code units, so case counts and upper-case letters come before lower-case ones. Two arrays, or two objects, compare equal: their contents are not compared. The sort is stable: elements that compare equal keep their original order.</remarks>
    /// <arg name="items">The array to sort, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array holding the elements of items in ascending order. Never null.</returns>
    /// <example><c>Sort([3, 1, 2])</c> returns <c>[1, 2, 3]</c>; <c>Sort(["b", null, 1])</c> returns <c>[null, 1, "b"]</c>.</example>
    type SortFunction() =
        inherit JyroFunctionBase("Sort", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let ticker = Bounds.Ticker ctx
            Bounds.requireSize (int64 arr.Count)
            let items = Array.init arr.Count (fun i -> ticker.Tick(); arr.Items.[i])
            // A merge sort of our own rather than List.Sort: the check raises
            // from inside the comparison, and List.Sort wrapped that in an
            // InvalidOperationException, so a time limit reached mid-sort was
            // reported as RuntimeError (spec 100.4.3).
            StableSort.sortInPlace items (fun a b -> JyroValue.CompareForSort(a, b)) ticker.Tick
            Bounds.copyOf ticker items.Length (fun i -> items.[i]) :> JyroValue

    /// <summary>Flattens an array of arrays, at every level of nesting, into one array.</summary>
    /// <arg name="items">The array to flatten, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error. An array that holds itself, directly or through other arrays, is a runtime error.</arg>
    /// <returns>A new array holding every element of items that is not an array, in order, with each nested array replaced by its own elements, flattened the same way. Objects, and null elements, are kept as they are. Never null.</returns>
    /// <example><c>Flatten([1, [2, [3, [4]]]])</c> returns <c>[1, 2, 3, 4]</c>.</example>
    type FlattenFunction() =
        inherit JyroFunctionBase("Flatten", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let result = JyroArray()
            let ticker = Bounds.Ticker ctx
            // Guard native recursion: a cyclic array (an array reachable from
            // itself) would otherwise recurse forever and overflow the CLR
            // stack (uncatchable). The guard trips a catchable
            // ValueNestingLimitExceeded instead. See ValueRecursionGuard.
            //
            // A value that holds one array at many places flattens into far
            // more elements than it holds, so the walk checks the limits per
            // element (spec 100.2.5) and refuses the element that would take
            // the result past the maximum value size (spec 100.8.7) - unless
            // the arrays it walks are cyclic or too deep, which is reported
            // as such whichever the walk reaches first (spec 100.8.9).
            let rec flatten (items: IReadOnlyList<JyroValue>) =
                ValueRecursionGuard.Enter()
                try
                    for item in items do
                        ticker.Tick()
                        match item with
                        | :? JyroArray as nested -> flatten nested.Items
                        | other ->
                            if result.Count >= ValueSize.Maximum then
                                ValueWalk.RequireShallowAcyclic(arr, true)
                                ValueSize.Raise()
                            result.Add(other)
                finally
                    ValueRecursionGuard.Exit()
            flatten arr.Items
            result :> JyroValue

    // The most elements `Range` returns: the maximum of spec 100.8.2 that
    // PadLeft, PadRight and RandomString share (91.12.7, 100.8.5).
    let private maxRangeLength = 1_000_000

    /// <summary>Makes an array of numbers that count from a start toward a finish by a step.</summary>
    /// <remarks>Each element is computed as start plus its position times step, never by adding step to the element before, so an inexact step does not build up error along the range. A range of more than 1,000,000 elements is a runtime error, as is an element too large to represent.</remarks>
    /// <arg name="start">The first number of the range. Null, or any other type that is not a number, is a runtime error.</arg>
    /// <arg name="finish">The number the range stops at, which is not included. Null, or any other type that is not a number, is a runtime error.</arg>
    /// <arg name="step">The difference between one element and the next: positive to count up, negative to count down. Zero is a runtime error. When it is left out, or null, the step is the exact number 1.</arg>
    /// <returns>A new array of the numbers start, start + step, start + 2 * step and so on, while each is below finish for a positive step, or above finish for a negative one. The empty array when start is already at or past finish in that direction. Never null.</returns>
    /// <example><c>Range(0, 5)</c> returns <c>[0, 1, 2, 3, 4]</c>; <c>Range(10, 0, -3)</c> returns <c>[10, 7, 4, 1]</c>.</example>
    type RangeFunction() =
        inherit JyroFunctionBase("Range",
            Signatures.create
                [ Parameter.Required("start", ParameterType.Number)
                  Parameter.Required("finish", ParameterType.Number)
                  Parameter.Optional("step", ParameterType.Number) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            // Each argument through its own checks, in parameter order (spec
            // 90.3.8, step 1): the types, then a zero step (92.13.10). A step
            // of null is a step not given (92.13.13), and the default is the
            // exact number 1 (92.13.2).
            let start = args.Get<JyroNumber>(0)
            let finish = args.Get<JyroNumber>(1)
            let step = args.GetOptional<JyroNumber>(2, JyroNumber(1))
            let zero = JyroNumber(0)
            let direction = JyroNumber.Compare(step, zero)
            if direction = 0 then
                JyroError.raiseRuntime MessageCode.InvalidRange
                    // One name and value: a zero step has no second value
                    // to name, so no second pair is given (JYRO-ADR-128).
                    [| box "Range()"; box "step"; box (step.ToStringValue()) |]
            // Element k is start + k * step in the arithmetic of 30.2.9 to
            // 30.2.11, never the previous element plus step (92.13.8): the
            // product, then the sum, each raising NumberOverflow for an
            // infinity (30.2.14).
            let elementAtGeneral (k: int) : JyroNumber =
                let offset = JyroNumber(k).EvaluateBinary(Multiply, step)
                start.EvaluateBinary(Add, offset).AsNumber()
            // The same two operations taken directly on the decimals when
            // `start` and `step` are exact, so a range of a million elements
            // makes one number per element rather than three. The operator
            // promotes to double (30.2.11) where decimal overflows and where
            // decimal had to round the result, which it does silently past its
            // 96-bit significand; the second is decided by the operator's own
            // check, JyroNumber.IsExactResult. Either way the element is then
            // computed again through the operators, which give the inexact
            // result or NumberOverflow.
            let bothExact = start.IsExact && step.IsExact
            let startD = if bothExact then start.DecimalValue else 0m
            let stepD = if bothExact then step.DecimalValue else 0m
            // With an inexact `step`, the product is inexact whatever `start`
            // is, and so is the sum: both are the double operations of 30.2.10.
            // Taken directly too, and an infinity in either is left to the
            // operators, which raise NumberOverflow with its message.
            let stepIsInexact = not step.IsExact
            let startF = start.Value
            let stepF = step.Value
            let elementAt (k: int) : JyroNumber =
                if bothExact then
                    let kD = decimal k
                    let mutable exact = 0m
                    let fits =
                        try
                            let product = kD * stepD
                            if JyroNumber.IsExactResult(Multiply, kD, stepD, product) then
                                exact <- startD + product
                                JyroNumber.IsExactResult(Add, startD, product, exact)
                            else false
                        with :? OverflowException -> false
                    if fits then JyroNumber(exact) else elementAtGeneral k
                elif stepIsInexact then
                    let product = float k * stepF
                    let sum = startF + product
                    if Double.IsInfinity product || Double.IsInfinity sum then elementAtGeneral k
                    else JyroNumber(sum)
                else elementAtGeneral k
            // Below `finish` for a positive step (92.13.3), above it for a
            // negative one (92.13.4), compared as `<` and `>` compare (92.13.7).
            let inRange (e: JyroNumber) = JyroNumber.Compare(e, finish) * direction < 0
            // The result is the one the positions k = 0, 1, 2 in order give
            // (92.13.16): the first overflow raises, the first element not in
            // range ends the range, and an element in range at position
            // 1,000,000 raises GeneratedLengthExceeded (100.8.5). Overflow is
            // monotone in k, and so is the range test, so position 1,000,000
            // is tried first (ADR 0080): when it computes and is in range,
            // every earlier position computes and is in range too, and the
            // length error is the result, with nothing allocated. Otherwise
            // the elements are computed in order, and the first overflow or
            // the first element not in range decides.
            let atMaximum =
                try Some (elementAt maxRangeLength)
                with :? JyroRuntimeException as ex when ex.Code = MessageCode.NumberOverflow -> None
            match atMaximum with
            | Some e when inRange e ->
                // The length shown is an estimate, not the count: (finish -
                // start) / step rounded up, in decimal when all three are
                // exact, in the double views when one is inexact or the
                // decimal quotient overflows. Both divisions round their
                // quotient before it is rounded up, so a quotient just above
                // an integer can land on it and the estimate fall to the
                // maximum or below, although the element at position
                // maxRangeLength is in range. The length shown is then
                // maxRangeLength + 1, which is the count there (JYRO-ADR-134),
                // so it always exceeds the maximum it is compared with. In the
                // double views either operation can overflow: the subtraction
                // (finite bounds of opposite sign whose difference exceeds
                // Double.MaxValue) or the division (a finite difference over a
                // small step, with only finish inexact). An infinite estimate
                // is not a number in the elements' arithmetic, and it passes
                // a '>' test, so a non-finite estimate (NaN cannot arise from
                // finite views over a non-zero step, but is caught the same
                // way) is shown as Double.MaxValue, the largest finite number
                // and still a lower bound of the true quotient (JYRO-ADR-135).
                // The decimal fallback below cannot overflow the double path:
                // exact operands are at most about 7.9e28 and an exact step at
                // least 1e-28.
                let requested : obj =
                    let atLeastOverMaximum (estimate: float) =
                        if not (Double.IsFinite estimate) then Double.MaxValue
                        elif estimate > float maxRangeLength then estimate
                        else float (maxRangeLength + 1)
                    let asDouble () =
                        atLeastOverMaximum (Math.Ceiling((finish.Value - start.Value) / step.Value)) :> obj
                    if bothExact && finish.IsExact then
                        try
                            let estimate = Decimal.Ceiling((finish.DecimalValue - startD) / stepD)
                            (if estimate > decimal maxRangeLength then estimate
                             else decimal (maxRangeLength + 1)) :> obj
                        with :? OverflowException -> asDouble ()
                    else asDouble ()
                JyroError.raiseRuntime MessageCode.GeneratedLengthExceeded
                    [| box "Range()"; requested; box maxRangeLength |]
            | _ -> ()
            // Created at the length the range will have (JYRO-ADR-133 (2)),
            // found by the same element test the loop below applies, not
            // estimated: whether position k ends the range (its element is
            // not in range, or it overflows) is monotone in k, as above, and
            // position maxRangeLength ends it, so a binary search over
            // [0, maxRangeLength] finds the first position that does, in
            // about 20 elements computed in the elements' own arithmetic.
            // That position is the result's length (or the elements before
            // the overflow the loop then raises), never more.
            let endsAt (k: int) =
                try not (inRange (elementAt k))
                with :? JyroRuntimeException as ex when ex.Code = MessageCode.NumberOverflow -> true
            let capacity =
                let mutable lo = 0
                let mutable hi = maxRangeLength
                while lo < hi do
                    ctx.CheckExecutionLimits()
                    let mid = lo + (hi - lo) / 2
                    if endsAt mid then hi <- mid else lo <- mid + 1
                lo
            let result = JyroArray(capacity)
            let mutable k = 0
            let mutable ended = false
            while not ended do
                ctx.CheckExecutionLimits()
                let e = elementAt k
                if inRange e then
                    result.Add(e)
                    k <- k + 1
                else ended <- true
            result :> JyroValue

    /// <summary>Drops a number of elements from the start of an array.</summary>
    /// <arg name="items">The array to read, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="count">How many elements to drop. A whole number of 0 or more; a negative or fractional number, or null, is a runtime error. A count at or past the length of items gives the empty array.</arg>
    /// <returns>A new array holding the elements of items after the first count, in order. Never null.</returns>
    /// <example><c>Skip([1, 2, 3, 4], 2)</c> returns <c>[3, 4]</c>.</example>
    type SkipFunction() =
        inherit JyroFunctionBase("Skip", Signatures.binary "items" ParameterType.Array "count"ParameterType.Number ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            // Skip(xs, -1) used to reach `arr.Items.[-1]` and escape as a raw
            // ArgumentOutOfRangeException; Skip(xs, 1e20) wrapped to
            // Int32.MinValue first and did the same.
            let count = JyroArguments.ReadNonNegativeInt32(args, 1, "Skip()", "count")
            let items = arr.Items
            let startIdx = min count items.Count
            Bounds.copyOf (Bounds.Ticker ctx) (items.Count - startIdx) (fun i -> items.[startIdx + i]) :> JyroValue

    /// <summary>Inserts a value into an array at a given position.</summary>
    /// <arg name="items">The array to insert into, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="index">The position the value takes, counted from 0. A whole number from 0 to the length of items; the length itself adds the value at the end. A negative, fractional or larger number, or null, is a runtime error.</arg>
    /// <arg name="value">The value to insert, of any type, null included.</arg>
    /// <returns>A new array holding the elements of items with value placed at index, the elements from index onward each moved one position later. Never null.</returns>
    /// <example><c>Insert([1, 3], 1, 2)</c> returns <c>[1, 2, 3]</c>.</example>
    type InsertFunction() =
        inherit JyroFunctionBase("Insert",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("index", ParameterType.Number)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let value = args.[2]
            let idx = JyroArguments.ReadInt32(args, 1, "Insert()", "index")
            // One past the end is the append position, so the bound is Length
            // rather than Length - 1.
            if idx < 0 || idx > arr.Count then
                JyroError.raiseRuntime MessageCode.IndexOutOfRange [| box "Insert()"; box idx; box arr.Count |]
            Bounds.requireSize (int64 arr.Count + 1L)
            let items = arr.Items
            Bounds.copyOf (Bounds.Ticker ctx) (items.Count + 1) (fun i ->
                if i < idx then items.[i] elif i = idx then value else items.[i - 1]) :> JyroValue

    /// <summary>Removes the element at a given position of an array.</summary>
    /// <arg name="items">The array to remove from, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="index">The position of the element to remove, counted from 0. A whole number that names an element of items, from 0 to one less than its length; a negative, fractional or larger number, or null, is a runtime error.</arg>
    /// <returns>A new array holding the elements of items without the one at index, in order. Never null.</returns>
    /// <example><c>RemoveAt([1, 2, 3], 1)</c> returns <c>[1, 3]</c>.</example>
    type RemoveAtFunction() =
        inherit JyroFunctionBase("RemoveAt", Signatures.binary "items" ParameterType.Array "index"ParameterType.Number ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let idx = JyroArguments.ReadInt32(args, 1, "RemoveAt()", "index")
            // Its twin `Insert` rejected an out-of-bounds index while this one
            // filtered on `i <> idx` and silently returned an unchanged copy,
            // so RemoveAt(xs, 99) looked like it had removed something. An
            // index must name an element that exists, so the bound is
            // Length - 1 here where Insert's is Length.
            if idx < 0 || idx >= arr.Count then
                JyroError.raiseRuntime MessageCode.IndexOutOfRange [| box "RemoveAt()"; box idx; box arr.Count |]
            let items = arr.Items
            Bounds.copyOf (Bounds.Ticker ctx) (items.Count - 1) (fun i ->
                if i < idx then items.[i] else items.[i + 1]) :> JyroValue

    /// <summary>Removes the first element of an array.</summary>
    /// <arg name="items">The array to remove from, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array holding every element of items but the first, in order; the empty array when items is empty or null. Never null.</returns>
    /// <example><c>RemoveFirst([1, 2, 3])</c> returns <c>[2, 3]</c>.</example>
    type RemoveFirstFunction() =
        inherit JyroFunctionBase("RemoveFirst", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let items = arr.Items
            Bounds.copyOf (Bounds.Ticker ctx) (max 0 (items.Count - 1)) (fun i -> items.[i + 1]) :> JyroValue

    /// <summary>Removes the last element of an array.</summary>
    /// <arg name="items">The array to remove from, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array holding every element of items but the last, in order; the empty array when items is empty or null. Never null.</returns>
    /// <example><c>RemoveLast([1, 2, 3])</c> returns <c>[1, 2]</c>.</example>
    type RemoveLastFunction() =
        inherit JyroFunctionBase("RemoveLast", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let items = arr.Items
            Bounds.copyOf (Bounds.Ticker ctx) (max 0 (items.Count - 1)) (fun i -> items.[i]) :> JyroValue

    /// <summary>Flattens one level of nesting in an array of arrays.</summary>
    /// <arg name="items">The array to flatten, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>A new array in which each element of items that is an array is replaced by its own elements, which are kept as they are even when they are arrays, and every other element, null included, is kept as it is, all in order. Never null.</returns>
    /// <example><c>FlattenOnce([1, [2, [3]]])</c> returns <c>[1, 2, [3]]</c>.</example>
    type FlattenOnceFunction() =
        inherit JyroFunctionBase("FlattenOnce", Signatures.unary "items" ParameterType.Array ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let input = args.GetArray(0)
            let result = JyroArray()
            let ticker = Bounds.Ticker ctx
            for item in input.Items do
                ticker.Tick()
                match item with
                | :? JyroArray as arr ->
                    for inner in arr.Items do
                        ticker.Tick()
                        result.AddBounded(inner)
                // Every other element, null included, is kept as it is
                // (92.19.4): a null is a value, not a gap.
                | v -> result.AddBounded(v)
            result :> JyroValue

    /// <summary>Picks an element of an array at random.</summary>
    /// <remarks>Returns a random value: each element has the same chance of being picked, and two calls with the same array can give different results.</remarks>
    /// <arg name="items">The array to pick from. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <returns>One element of items. Null when items is empty or null, and when the element picked is itself null.</returns>
    /// <example><c>RandomChoice(["red", "green", "blue"])</c> returns one of the three strings.</example>
    type RandomChoiceFunction() =
        inherit JyroFunctionBase("RandomChoice", Signatures.unary "items" ParameterType.Array ParameterType.Any)
        override _.Effect = FunctionEffect.NonDeterministic
        override this.Execute(args, _) =
            let arr = args.GetArray(0)
            if arr.Count = 0 then
                JyroNull.Instance :> JyroValue
            else
                let idx = RandomNumberGenerator.GetInt32(0, arr.Count)
                arr.Items.[idx]

    /// <summary>Sorts an array of objects by the value of one of their properties.</summary>
    /// <remarks>The values are ordered as <c>Sort</c> orders them: null first, then booleans, numbers, strings, arrays, objects and functions, never causing an error. An element that is not an object, or an object without the property, sorts as null. The sort is stable: elements whose values compare equal keep their original order.</remarks>
    /// <arg name="items">The array to sort, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the property to sort by, matched exactly, case included. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="direction">The order: <c>"asc"</c> for ascending or <c>"desc"</c> for descending, with no case sensitivity for ASCII letters, so <c>"Desc"</c> is accepted too. Any other string, null, or a value of another type is a runtime error.</arg>
    /// <returns>A new array holding the elements of items ordered by the value of fieldName, in the given direction. Never null.</returns>
    /// <example><c>SortByField([{"n": 2}, {"n": 1}], "n", "asc")</c> returns <c>[{"n": 1}, {"n": 2}]</c>.</example>
    type SortByFieldFunction() =
        inherit JyroFunctionBase("SortByField",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String)
                  Parameter.Required("direction", ParameterType.String) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let direction = args.GetString(2)
            // Checked after the three arguments' types and before any work
            // (spec 90.3.8, 90.3.9): a direction that is neither "asc" nor
            // "desc" is refused, not read as ascending (spec 92.21.7), so a
            // typing error such as "decs" is reported.
            // The one argument is the direction as given, a `null`, a number
            // or a boolean as its string form (40.4, JYRO-REQ-1866): a `null`
            // is `null`, not the empty string `GetString` reads it as.
            let isDesc =
                if args.[2] :? JyroNull then
                    JyroError.raiseRuntime MessageCode.InvalidSortDirection [| box "null" |]
                elif AsciiCase.equals direction "desc" then true
                elif AsciiCase.equals direction "asc" then false
                else JyroError.raiseRuntime MessageCode.InvalidSortDirection [| box direction |]
            let ticker = Bounds.Ticker ctx
            Bounds.requireSize (int64 arr.Count)
            let items = Array.init arr.Count (fun i -> ticker.Tick(); arr.Items.[i])
            // The value of the property, or null for an element that is not
            // an object (92.21.4), so every pair is ordered by one key and the
            // order is consistent. It used to tie a non-object element with
            // every element.
            let keyOf (item: JyroValue) : JyroValue =
                match item with
                | :? JyroObject as o -> o.GetProperty(fieldName)
                | _ -> JyroNull.Instance :> JyroValue
            // A merge sort of our own, for the reason given on `Sort`.
            StableSort.sortInPlace items (fun a b ->
                let cmp = JyroValue.CompareForSort(keyOf a, keyOf b)
                if isDesc then -cmp else cmp) ticker.Tick
            Bounds.copyOf ticker items.Length (fun i -> items.[i]) :> JyroValue

    /// <summary>Groups the objects of an array by the value of one of their properties.</summary>
    /// <arg name="items">The array to group, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error. Elements that are not objects are left out.</arg>
    /// <arg name="fieldName">The name of the property to group by, matched exactly, case included. A number or a boolean is read as its string form, and null as the empty string. A property whose value is an array, an object or a function is a runtime error.</arg>
    /// <returns>A new object with one property per group. Its key is the string form of the property's value, and the text null for an object whose value is null or that lacks the property. Its value is an array of the objects in that group, in their original order. The empty object when items holds no object. Never null.</returns>
    /// <example><c>GroupBy([{"t": "a"}, {"t": "b"}, {"t": "a"}], "t")</c> returns <c>{"a": [{"t": "a"}, {"t": "a"}], "b": [{"t": "b"}]}</c>.</example>
    type GroupByFunction() =
        inherit JyroFunctionBase("GroupBy",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String) ]
                ParameterType.Object)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let result = JyroObject()
            let ticker = Bounds.Ticker ctx
            for item in arr.Items do
                ticker.Tick()
                match item with
                | :? JyroObject as obj ->
                    let fieldValue = obj.GetProperty(fieldName)
                    let groupKey =
                        match fieldValue.ValueType with
                        // An array, an object or a function has no string
                        // form (40.4.2), so it cannot be a key (92.22.6); its
                        // JSON text would make a key from syntax nobody wrote.
                        // The value is read as its string form, so the
                        // expected type is string (JYRO-ADR-164 item 2).
                        | JyroValueType.Array | JyroValueType.Object | JyroValueType.Function ->
                            JyroError.raiseRuntime MessageCode.ArgumentTypeMismatch
                                [| box (TypeNames.typeOfName JyroValueType.String)
                                   box (TypeNames.typeOfName fieldValue.ValueType) |]
                        | _ when fieldValue.IsNull -> "null"
                        | _ -> ValueWalk.ScriptStringFor(fieldValue, this.Name + "()")
                    let existingGroup = result.GetProperty(groupKey)
                    let groupArray =
                        match existingGroup with
                        | :? JyroArray as existing -> existing
                        | _ ->
                            let newArr = JyroArray()
                            result.SetPropertyBounded(groupKey, newArr)
                            newArr
                    groupArray.AddBounded(item)
                | _ -> ()
            result :> JyroValue

    /// <summary>Collects the elements of an array property of each object in an array into one array.</summary>
    /// <arg name="items">The array of objects to read, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error. Elements that are not objects are left out.</arg>
    /// <arg name="fieldName">The name of the property that holds an array, matched exactly, case included. A number or a boolean is read as its string form, and null as the empty string. An object whose property is missing, or is not an array, adds nothing.</arg>
    /// <returns>A new array holding the elements of the fieldName array of each object, in the order of the objects and then of each array's elements. Nested arrays among those elements are kept as they are. Never null.</returns>
    /// <example><c>SelectMany([{"tags": ["a", "b"]}, {"tags": ["c"]}], "tags")</c> returns <c>["a", "b", "c"]</c>.</example>
    type SelectManyFunction() =
        inherit JyroFunctionBase("SelectMany",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let result = JyroArray()
            let ticker = Bounds.Ticker ctx
            for item in arr.Items do
                ticker.Tick()
                match item with
                | :? JyroObject as obj ->
                    let fieldValue = obj.GetProperty(fieldName)
                    match fieldValue with
                    | :? JyroArray as nested ->
                        for nestedItem in nested.Items do
                            ticker.Tick()
                            result.AddBounded(nestedItem)
                    | _ -> ()
                | _ -> ()
            result :> JyroValue

    let getAll () : IJyroFunction list =
        [ LengthFunction()
          AppendFunction()
          PrependFunction()
          FirstFunction()
          LastFunction()
          IndexOfFunction()
          ReverseFunction()
          SliceFunction()
          ConcatenateFunction()
          DistinctFunction()
          SortFunction()
          FlattenFunction()
          RangeFunction()
          SkipFunction()
          InsertFunction()
          RemoveAtFunction()
          RemoveFirstFunction()
          RemoveLastFunction()
          FlattenOnceFunction()
          RandomChoiceFunction()
          SortByFieldFunction()
          GroupByFunction()
          SelectManyFunction() ]

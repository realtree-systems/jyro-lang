namespace RealTree.Jyro

open System.Collections.Generic

// Higher-order functions that accept lambda callbacks
module internal LambdaFunctions =

    // The callback parameters below declare `ParameterType.Function`, not `ParameterType.Any`.
    //
    // They said `ParameterType.Any` while `ParameterType.Function` sat unused in the enum, which
    // made the signature wrong in the one place it carries the most
    // information: nothing in the declared metadata said that `Map`'s second
    // argument must be a lambda, on the eight functions where that is the only
    // thing a caller needs to know about it.
    //
    // Declared types are not enforced - `getCallback` below does the real
    // check, at run time, and still does. But being unenforced is exactly why
    // they have to be accurate: their whole justification is that they are the
    // only machine-readable record of what a function accepts, and a record
    // that says "anything" where it means "a function" is not one.

    // The one gate every higher-order function passes its callback through.
    // `arity` is the number of arguments the caller will supply - one for
    // every function here except `Reduce`, which supplies the accumulator and
    // the item.
    //
    // The arity was not checked, and a mismatch left the engine entirely: a
    // callback declaring more parameters than the caller supplies reached
    // `args.[1]` on a one-element list, so `Map([1, 2], (x, y) => 1)` reported
    // "Index was out of range. Must be non-negative and less than the size of
    // the collection. (Parameter 'index')" as a generic RuntimeError - a
    // platform exception string, from untrusted script input, naming nothing
    // about the script. `SortBy` was worse again: the failure happened inside
    // the comparer, so it arrived as "Failed to compare two elements in the
    // array."
    //
    // A callback declaring FEWER is refused too, though it happened to work.
    // The rule is that a callback's parameter list says what it receives, and
    // a list that disagrees with what it receives is a mistake in the script
    // whichever direction it disagrees in. One rule reads better than a rule
    // with an exception for the direction that does not happen to crash.
    let private getCallback (args: IReadOnlyList<JyroValue>) (index: int) (funcName: string) (arity: int) : JyroFunction =
        if index >= args.Count then
            JyroError.raiseRuntime MessageCode.ArgumentNotProvided [| box index |]
        let arg = args.[index]
        if isNull (box arg) then
            JyroError.raiseRuntime MessageCode.CallbackExpected [| box funcName; box "null" |]
        match arg with
        | :? JyroFunction as f ->
            if f.ParameterCount <> arity then
                JyroError.raiseRuntime MessageCode.CallbackArityMismatch
                    [| box funcName; box arity; box f.ParameterCount |]
            f
        | other -> JyroError.raiseRuntime MessageCode.CallbackExpected [| box funcName; box (TypeNames.typeOfName other.ValueType) |]

    // The elements `items` has when the function starts, which are the ones
    // every function here gives its callback (98.1.9), as `foreach` iterates
    // (50.6.5). The functions used to read `items` live while the callback
    // ran, so a callback that changed an element, appended one (Find, All
    // and Any also re-read the length) or emptied the array changed what the
    // function went on to see - and an append at every call never ended.
    // Taken after the callback is checked, so the argument errors come
    // first, and a copy of references only: an element that is itself a
    // container is the same value the callback can change.
    let private snapshot (arr: JyroArray) (ctx: JyroExecutionContext) : JyroValue[] =
        let items = arr.Items
        let ticker = Bounds.Ticker ctx
        Array.init items.Count (fun i -> ticker.Tick(); items.[i])

    /// <summary>Builds a new array from the result of a lambda for each element of an array.</summary>
    /// <remarks>The lambda is called once for each element, in order. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to read, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; its result, of any type, null included, becomes the element of the new array at the same position. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>A new array of the lambda's results, one for each element of items, in the same order; the empty array when items is empty. Never null.</returns>
    /// <example><c>Map([1, 2, 3], x =&gt; x * 2)</c> returns <c>[2, 4, 6]</c>.</example>
    type MapFunction() =
        inherit JyroFunctionBase("Map",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "Map" 1
            let items = snapshot arr ctx
            // One element per element of `items`, at most the maximum value
            // size, which AddBounded refuses past (JYRO-ADR-133 (2)).
            let result = JyroArray(min items.Length ValueSize.Maximum)
            for i = 0 to items.Length - 1 do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                let mapped = callback.Invoke([| item |] :> IReadOnlyList<JyroValue>)
                // After the callback for this element, so an error it raises
                // comes before the size of the result (spec 100.8.11).
                result.AddBounded(mapped)
            result :> JyroValue

    /// <summary>Keeps the elements of an array for which a lambda gives a truthy result.</summary>
    /// <remarks>The lambda is called once for each element, in order. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to filter, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; the element is kept when the lambda's result is truthy and left out otherwise. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>A new array of the elements of items that were kept, in their original order; the empty array when none is kept. Never null.</returns>
    /// <example><c>Where([1, 2, 3, 4], x =&gt; x &gt; 2)</c> returns <c>[3, 4]</c>.</example>
    type WhereFunction() =
        inherit JyroFunctionBase("Where",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "Where" 1
            let items = snapshot arr ctx
            let result = JyroArray()
            for i = 0 to items.Length - 1 do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                let test = callback.Invoke([| item |] :> IReadOnlyList<JyroValue>)
                if test.IsTruthy() then
                    result.AddBounded(item)
            result :> JyroValue

    /// <summary>Combines the elements of an array into one value, by a lambda applied to each element in turn.</summary>
    /// <remarks>The lambda is called once for each element, in order. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to read, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly two parameters: the first receives the value combined so far (initial, for the first element), and the second receives the element. Its result, of any type, null included, is the value combined so far for the next element. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <arg name="initial">The value to start from, of any type, null included. It must be given.</arg>
    /// <returns>The lambda's result for the last element, or initial when items is empty. Null when that value is null: when the last result of the lambda is null, or when items is empty and initial is null.</returns>
    /// <example><c>Reduce([1, 2, 3], (total, x) =&gt; total + x, 0)</c> returns 6.</example>
    type ReduceFunction() =
        inherit JyroFunctionBase("Reduce",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function)
                  Parameter.Required("initial", ParameterType.Any) ]
                ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "Reduce" 2
            let items = snapshot arr ctx
            let mutable acc = args.[2]
            for i = 0 to items.Length - 1 do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                acc <- callback.Invoke([| acc; item |] :> IReadOnlyList<JyroValue>)
            acc

    /// <summary>Calls a lambda once for each element of an array.</summary>
    /// <remarks>The lambda is called in order, for what it does rather than for its result. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to read. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; its result is ignored. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>Null, always.</returns>
    /// <example><c>Each(items, item => item.count++)</c> adds one to the <c>count</c> of each element of <c>items</c>.</example>
    type EachFunction() =
        inherit JyroFunctionBase("Each",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Null)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "Each" 1
            let items = snapshot arr ctx
            for i = 0 to items.Length - 1 do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                callback.Invoke([| item |] :> IReadOnlyList<JyroValue>) |> ignore
            JyroNull.Instance :> JyroValue

    /// <summary>Finds the first element of an array for which a lambda gives a truthy result.</summary>
    /// <remarks>The lambda is called for each element in order, and not for any element after the first match. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to search. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; the element matches when the lambda's result is truthy. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>The first element that matches. Null when no element matches, or when the element that matches is itself null.</returns>
    /// <example><c>Find([1, 5, 8], x =&gt; x &gt; 4)</c> returns 5.</example>
    type FindFunction() =
        inherit JyroFunctionBase("Find",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "Find" 1
            let items = snapshot arr ctx
            // Whether an element matched is kept apart from the element: a
            // matched null or false is the answer (98.5.2). The search used to
            // go on while the element found was null, so a matched null was
            // passed over and a later element returned instead.
            let mutable found: JyroValue = JyroNull.Instance
            let mutable matched = false
            let mutable i = 0
            while i < items.Length && not matched do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                let test = callback.Invoke([| item |] :> IReadOnlyList<JyroValue>)
                if test.IsTruthy() then
                    found <- item
                    matched <- true
                i <- i + 1
            found

    /// <summary>Tells whether a lambda gives a truthy result for every element of an array.</summary>
    /// <remarks>The lambda is called for each element in order, and not for any element after the first one that gives a result that is not truthy. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to test. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; its result is tested for being truthy. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>True when the lambda's result is truthy for every element, and so true for an empty array; false otherwise. Never null.</returns>
    /// <example><c>All([2, 4, 6], x =&gt; x % 2 == 0)</c> returns true.</example>
    type AllFunction() =
        inherit JyroFunctionBase("All",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "All" 1
            let items = snapshot arr ctx
            let mutable result = true
            let mutable i = 0
            while i < items.Length && result do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                let test = callback.Invoke([| item |] :> IReadOnlyList<JyroValue>)
                if not (test.IsTruthy()) then
                    result <- false
                i <- i + 1
            JyroBoolean.FromBoolean(result) :> JyroValue

    /// <summary>Tells whether a lambda gives a truthy result for at least one element of an array.</summary>
    /// <remarks>The lambda is called for each element in order, and not for any element after the first one that gives a truthy result. It is given the elements the array holds when the call starts, so a change the lambda makes to the array does not change which elements it is given.</remarks>
    /// <arg name="items">The array to test. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; its result is tested for being truthy. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>True when the lambda's result is truthy for at least one element; false otherwise, and so false for an empty array. Never null.</returns>
    /// <example><c>Any([1, 3, 4], x =&gt; x % 2 == 0)</c> returns true.</example>
    type AnyFunction() =
        inherit JyroFunctionBase("Any",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "Any" 1
            let items = snapshot arr ctx
            let mutable result = false
            let mutable i = 0
            while i < items.Length && not result do
                ctx.CheckExecutionLimits()
                let item = items.[i]
                let test = callback.Invoke([| item |] :> IReadOnlyList<JyroValue>)
                if test.IsTruthy() then
                    result <- true
                i <- i + 1
            JyroBoolean.FromBoolean(result) :> JyroValue

    /// <summary>Sorts the elements of an array in ascending order of a key that a lambda gives for each.</summary>
    /// <remarks>The lambda is called exactly once for each element, in order, before any key is compared. It is given the elements the array holds when the call starts. Keys are compared as <c>Sort</c> compares values, and keys of different types never cause an error: null comes first, then booleans, numbers, strings, arrays, objects and functions. Booleans sort false before true, numbers by value, and strings by their UTF-16 code units, so case counts. The sort is stable: elements whose keys compare equal keep their original order.</remarks>
    /// <arg name="items">The array to sort, which is left unchanged. Null is read as the empty array; any other type that is not an array is a runtime error.</arg>
    /// <arg name="callback">A lambda of exactly one parameter, which receives the element; its result, of any type, null included, is the key the element is sorted by. A lambda of any other number of parameters, or a value that is not a lambda, null included, is a runtime error.</arg>
    /// <returns>A new array holding the elements of items in ascending order of their keys. Never null.</returns>
    /// <example><c>SortBy([{"n": 3}, {"n": 1}], x =&gt; x.n)</c> returns <c>[{"n": 1}, {"n": 3}]</c>.</example>
    type SortByFunction() =
        inherit JyroFunctionBase("SortBy",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("callback", ParameterType.Function) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let callback = getCallback args 1 "SortBy" 1
            let items = snapshot arr ctx
            // One key per element, in order of position, before any key is
            // compared (98.8.4). The callback used to run inside List.Sort's
            // comparer, twice per comparison, and List.Sort wrapped any error
            // it raised - the callback's own, a time limit, a cancellation -
            // in InvalidOperationException, so it reached the script as
            // RuntimeError (98.8.7, spec 100.4.3).
            let keyed =
                Array.init items.Length (fun i ->
                    ctx.CheckExecutionLimits()
                    let item = items.[i]
                    struct (callback.Invoke([| item |] :> IReadOnlyList<JyroValue>), item))
            // Every callback has run, so the size of the result is the one
            // error left to meet before it is built (spec 100.8.11).
            Bounds.requireSize (int64 keyed.Length)
            let ticker = Bounds.Ticker ctx
            StableSort.sortInPlace keyed (fun (struct (ka, _)) (struct (kb, _)) -> JyroValue.CompareForSort(ka, kb)) ticker.Tick
            Bounds.copyOf ticker keyed.Length (fun i -> let struct (_, item) = keyed.[i] in item) :> JyroValue

    // Get all lambda/higher-order functions
    let getAll () : IJyroFunction list =
        [ MapFunction()
          WhereFunction()
          ReduceFunction()
          EachFunction()
          FindFunction()
          AllFunction()
          AnyFunction()
          SortByFunction() ]

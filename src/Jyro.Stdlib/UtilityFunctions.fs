namespace RealTree.Jyro

open System

// Utility functions
module internal UtilityFunctions =

    // A string argument that is a string or null, null read as the empty
    // string (160.3.1). A number or a boolean is not read as its string form
    // here: it raises `ArgumentTypeMismatch`, naming `string` and the actual
    // type (95.7, 95.11, 130.6.5, JYRO-ADR-164), as a container or a
    // function does through `GetString`.
    let private stringOrNullArgument (args: Collections.Generic.IReadOnlyList<JyroValue>) (index: int) : string =
        if index < args.Count then
            match args.[index] with
            | :? JyroNumber
            | :? JyroBoolean ->
                JyroError.raiseRuntime MessageCode.ArgumentTypeMismatch
                    [| box (TypeNames.typeOfName JyroValueType.String)
                       box (TypeNames.typeOfName args.[index].ValueType) |]
            | _ -> ()
        args.GetString(index)

    /// <summary>Gives the name of a value's type.</summary>
    /// <arg name="value">The value whose type to name. Any value, null included.</arg>
    /// <returns>One of the strings <c>"null"</c>, <c>"boolean"</c>, <c>"number"</c>, <c>"string"</c>, <c>"array"</c>, <c>"object"</c> or <c>"function"</c>. Never null.</returns>
    /// <example><c>TypeOf([1, 2])</c> returns <c>"array"</c>; <c>TypeOf(null)</c> returns <c>"null"</c>.</example>
    type TypeOfFunction() =
        inherit JyroFunctionBase("TypeOf", Signatures.unary "value" ParameterType.Any ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override _.Execute(args, _) =
            let value = args.[0]
            let typeName =
                match value.ValueType with
                | JyroValueType.Null -> "null"
                | JyroValueType.Boolean -> "boolean"
                | JyroValueType.Number -> "number"
                | JyroValueType.String -> "string"
                | JyroValueType.Array -> "array"
                | JyroValueType.Object -> "object"
                | JyroValueType.Function -> "function"
                // Enum values outside the declared set shouldn't be reachable
                // but C# enums allow arbitrary underlying ints; map any
                // unexpected value to "unknown" rather than crashing.
                | _ -> "unknown"
            JyroString(typeName) :> JyroValue

    /// <summary>Gives the first element of an array that is not null.</summary>
    /// <arg name="values">The array to search, from its first element to its last. Null is read as the empty array. Any other value that is not an array is a runtime error.</arg>
    /// <returns>The first element that is not null, as it is. Null when the array is empty or every element in it is null.</returns>
    /// <example><c>Coalesce([null, "", "x"])</c> returns <c>""</c>; <c>Coalesce([null, null])</c> returns null.</example>
    type CoalesceFunction() =
        inherit JyroFunctionBase("Coalesce", Signatures.unary "values" ParameterType.Array ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let input = args.GetArray(0)
            let ticker = Bounds.Ticker ctx
            let mutable result: JyroValue = JyroNull.Instance
            let mutable i = 0
            while i < input.Count && result.IsNull do
                ticker.Tick()
                if not input.Items.[i].IsNull then
                    result <- input.Items.[i]
                i <- i + 1
            result

    /// <summary>Converts a value to its string form.</summary>
    /// <arg name="value">The value to convert. A string is returned as it is; a number gives its text, as <c>1.5</c> for <c>1.50</c>; a boolean gives <c>true</c> or <c>false</c>; null gives the text null; an array or an object gives its JSON text, as <c>ToJson</c> writes it. A function, at any depth, is a runtime error, as is an array or an object that holds itself, is nested too deeply or whose text would pass the host's maximum value size.</arg>
    /// <returns>The string form of value. Never null.</returns>
    /// <example><c>ToString(42)</c> returns <c>"42"</c>; <c>ToString([1, "a"])</c> returns <c>"[1,\"a\"]"</c>.</example>
    type ToStringFunction() =
        inherit JyroFunctionBase("ToString", Signatures.unary "value" ParameterType.Any ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            match args.[0] with
            // Returned as it was given (95.3.9), which the maximum value size
            // exempts whatever its length (spec 100.8.7).
            | :? JyroString as s -> s :> JyroValue
            // The script's string form: refused above the maximum value size,
            // and for a cyclic or too-deep value first (spec 100.8.7, 100.8.9).
            // An unpaired surrogate names this function (JYRO-ADR-128).
            | value -> JyroString(ValueWalk.ScriptStringFor(value, this.Name + "()")) :> JyroValue

    /// <summary>Converts a value to a boolean by the language's truthiness rule.</summary>
    /// <arg name="value">The value to convert. Any value, null included.</arg>
    /// <returns>False for null, false, the number zero and the empty string; true for every other value, an empty array, an empty object and a function included. Never null.</returns>
    /// <example><c>ToBoolean("")</c> returns false; <c>ToBoolean([])</c> returns true.</example>
    type ToBooleanFunction() =
        inherit JyroFunctionBase("ToBoolean", Signatures.unary "value" ParameterType.Any ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override _.Execute(args, _) =
            JyroBoolean.FromBoolean(args.[0].IsTruthy()) :> JyroValue

    /// <summary>Lists the property names of an object.</summary>
    /// <arg name="source">The object whose property names to list. Null is read as an empty object. Any other value that is not an object is a runtime error.</arg>
    /// <returns>An array of the property names, as strings, in the order the object holds them. The empty array for an empty object. Never null.</returns>
    /// <example><c>Keys({"a": 1, "b": 2})</c> returns <c>["a", "b"]</c>.</example>
    type KeysFunction() =
        inherit JyroFunctionBase("Keys", Signatures.unary "source" ParameterType.Object ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let obj = args.GetObject(0)
            let ticker = Bounds.Ticker ctx
            Bounds.requireSize (int64 obj.Count)
            let result = JyroArray(obj.Count)
            for key in obj.Properties.Keys do
                ticker.Tick()
                result.Add(JyroString(key))
            result :> JyroValue

    /// <summary>Lists the property values of an object.</summary>
    /// <arg name="source">The object whose property values to list. Null is read as an empty object. Any other value that is not an object is a runtime error.</arg>
    /// <returns>An array of the property values, as they are, in the order the object holds them, so in the same order as <c>Keys</c> gives the names. The empty array for an empty object. Never null.</returns>
    /// <example><c>Values({"a": 1, "b": 2})</c> returns <c>[1, 2]</c>.</example>
    type ValuesFunction() =
        inherit JyroFunctionBase("Values", Signatures.unary "source" ParameterType.Object ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let obj = args.GetObject(0)
            let ticker = Bounds.Ticker ctx
            Bounds.requireSize (int64 obj.Count)
            let result = JyroArray(obj.Count)
            for value in obj.Properties.Values do
                ticker.Tick()
                result.Add(value)
            result :> JyroValue

    /// <summary>Tells whether an object has a property of a given name.</summary>
    /// <arg name="source">The object to look in. Null is read as an empty object, which has no property. Any other value that is not an object is a runtime error.</arg>
    /// <arg name="name">The property name to look for, matched exactly, case included. Null is read as the empty string. A number, a boolean, an array, an object or a function is a runtime error: it is not read as its string form.</arg>
    /// <returns>True when source has a property named name, whatever its value, null included; false otherwise. Never null.</returns>
    /// <example><c>HasProperty({"a": null}, "a")</c> returns true; <c>HasProperty({"a": 1}, "A")</c> returns false.</example>
    type HasPropertyFunction() =
        inherit JyroFunctionBase("HasProperty", Signatures.binary "source" ParameterType.Object "name"ParameterType.String ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let obj = args.GetObject(0)
            // A key is a string (30.5.2): a number or a boolean `name` is
            // ArgumentTypeMismatch, not read as its string form (95.7).
            let key = stringOrNullArgument args 1
            JyroBoolean.FromBoolean(obj.Properties.ContainsKey(key)) :> JyroValue

    /// <summary>Combines the properties of several objects into one new object.</summary>
    /// <arg name="values">An array of the objects to combine, in order. A null element is skipped; an element of any other type is a runtime error, found before anything is combined. Null is read as the empty array.</arg>
    /// <returns>A new object holding every property of every object in values. When more than one object has a property of the same name, the value of the last of them is kept. The objects in values are not changed. The empty object when values holds no object. Never null.</returns>
    /// <example><c>Merge([{"a": 1, "b": 2}, {"b": 3}])</c> returns <c>{"a": 1, "b": 3}</c>.</example>
    type MergeFunction() =
        inherit JyroFunctionBase("Merge", Signatures.unary "values" ParameterType.Array ParameterType.Object)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let input = args.GetArray(0)
            let result = JyroObject()
            let ticker = Bounds.Ticker ctx
            // Each element is an object or null (95.8): any other is
            // ArgumentTypeMismatch naming `object` and its type (130.6.5,
            // JYRO-ADR-164), checked over the whole array before anything is
            // merged, so the refusal does not depend on how large the merge
            // before it would have been. A null element is skipped.
            for item in input.Items do
                ticker.Tick()
                match item with
                | :? JyroObject
                | :? JyroNull -> ()
                | other ->
                    JyroError.raiseRuntime MessageCode.ArgumentTypeMismatch
                        [| box (TypeNames.typeOfName JyroValueType.Object)
                           box (TypeNames.typeOfName other.ValueType) |]
            for item in input.Items do
                ticker.Tick()
                match item with
                | :? JyroObject as obj ->
                    for kvp in obj.Properties do
                        ticker.Tick()
                        result.SetPropertyBounded(kvp.Key, kvp.Value)
                | _ -> ()
            result :> JyroValue

    /// <summary>Makes a deep copy of a value.</summary>
    /// <arg name="value">The value to copy. Any value, null included. An array or an object is copied with every array and object inside it, so changing the copy never changes the original. A value that holds itself, is nested too deeply, or whose copy would pass the host's maximum value size is a runtime error.</arg>
    /// <returns>A copy equal to value. An array or an object that appears more than once inside value is copied once for each place it appears, so the copy shares no container with value or within itself. Null when value is null; never null otherwise.</returns>
    /// <example><c>copy = Clone(data)</c> followed by <c>copy.name = "x"</c> leaves <c>data.name</c> unchanged.</example>
    type CloneFunction() =
        inherit JyroFunctionBase("Clone", Signatures.unary "value" ParameterType.Any ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override _.Execute(args, ctx) =
            let ticker = Bounds.Ticker ctx
            // Clone makes a new container for every container its walk reaches,
            // once per path (95.9.5), so its result is as large as the tree the
            // argument unfolds into. That size (spec 100.8.6) is counted before
            // anything is copied, and a copy above the maximum value size is
            // refused (spec 100.8.7, 100.8.8) - a cyclic or too-deep argument
            // as such, whichever the count reaches first (spec 100.8.9).
            match args.[0] with
            | :? JyroArray
            | :? JyroObject ->
                let maximum = int64 ValueSize.Maximum
                if ValueWalk.UnfoldedSize(args.[0], maximum) > maximum then
                    ValueWalk.RequireShallowAcyclic(args.[0], false)
                    ValueSize.Raise()
            | _ -> ()
            // Guard native recursion: a cyclic input (`a.self = a`) or a
            // pathologically deep one would otherwise recurse forever and
            // overflow the CLR stack (uncatchable). The guard trips a
            // catchable ValueNestingLimitExceeded instead. See
            // ValueRecursionGuard.
            let rec clone (value: JyroValue) : JyroValue =
                match value with
                | :? JyroNull -> JyroNull.Instance :> JyroValue
                | :? JyroBoolean as b -> JyroBoolean.FromBoolean(b.Value) :> JyroValue
                // Returned as-is, like the two scalars above. This built the
                // copy from `n.Value` - the lossy double view - so the one
                // function whose entire job is to preserve a value was the one
                // that rounded it: `Clone(9007199254740993)` came back as
                // `...992` while the same literal left alone round-tripped
                // exactly. It also flipped `IsExact`, so the copy serialized
                // differently from then on.
                //
                // Reconstructing it preserving the representation was the first
                // fix, and was necessary only because `JyroNumber` carried a
                // mutable `ForceFloat` flag that aliasing would have shared.
                // With that member gone the type is immutable, so the copy that
                // preserves everything is the value itself - which is what
                // `JyroNull` and `JyroBoolean` already do.
                | :? JyroNumber as n -> n :> JyroValue
                | :? JyroString as s -> JyroString(s.Value) :> JyroValue
                | :? JyroArray as arr ->
                    ValueRecursionGuard.Enter()
                    try
                        // At its final size (JYRO-ADR-133): the whole value was
                        // checked against the maximum value size above.
                        let result = JyroArray(arr.Count)
                        for item in arr.Items do
                            ticker.Tick()
                            result.Add(clone item)
                        result :> JyroValue
                    finally
                        ValueRecursionGuard.Exit()
                | :? JyroObject as obj ->
                    ValueRecursionGuard.Enter()
                    try
                        let result = JyroObject(obj.Count)
                        for kvp in obj.Properties do
                            ticker.Tick()
                            result.SetProperty(kvp.Key, clone kvp.Value)
                        result :> JyroValue
                    finally
                        ValueRecursionGuard.Exit()
                | _ -> value
            clone args.[0]

    /// <summary>Makes a new random UUID string.</summary>
    /// <remarks>Returns a random value: each call can give a different UUID.</remarks>
    /// <returns>A UUID of 36 characters in lower case: 32 hexadecimal digits in groups of 8, 4, 4, 4 and 12, joined by hyphens. Never null.</returns>
    /// <example><c>NewGuid()</c> returns a string such as <c>"3f2b8c1e-9a4d-4e6f-8b2a-1c5d7e9f0a3b"</c>.</example>
    type NewGuidFunction() =
        inherit JyroFunctionBase("NewGuid",
            Signatures.create [] ParameterType.String)
        override _.Effect = FunctionEffect.NonDeterministic
        override _.Execute(_, _) =
            let guid = Guid.NewGuid()
            JyroString(guid.ToString("D").ToLowerInvariant()) :> JyroValue

    /// <summary>Encodes text as Base64.</summary>
    /// <arg name="text">The text to encode, taken as its UTF-8 bytes. Null is read as the empty string. A number, a boolean, an array, an object or a function is a runtime error: it is not read as its string form. Text that holds an unpaired surrogate is a runtime error, as is a result that would pass the host's maximum value size.</arg>
    /// <returns>The Base64 text of the UTF-8 bytes of text, in the standard alphabet with <c>=</c> padding and no line breaks; the empty string for empty text. <c>Base64Decode</c> gives the text back. Never null.</returns>
    /// <example><c>Base64Encode("Hello")</c> returns <c>"SGVsbG8="</c>.</example>
    type Base64EncodeFunction() =
        inherit JyroFunctionBase("Base64Encode", Signatures.unary "text" ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            // A string or null, null read as the empty string: a number or a
            // boolean is ArgumentTypeMismatch, not encoded as its string form
            // (95.11).
            let str = stringOrNullArgument args 0
            // UTF8.GetBytes replaces an unpaired surrogate with U+FFFD, so the
            // round-trip this function's own documentation promises "for any
            // string s" quietly failed: encode then decode returned a different
            // string, and reported success both times.
            let bad = JyroValue.FirstUnpairedSurrogate str
            if bad >= 0 then
                // The index first, then the function (JYRO-ADR-128).
                JyroError.raiseRuntime MessageCode.UnpairedSurrogate [| box bad; box (this.Name + "()") |]
            // Four code units for every three bytes, counted before either
            // is allocated (spec 100.8.7, 100.8.8). The encoding, the count
            // and the Base64 text are each made a chunk at a time with a check
            // between chunks (spec 100.2.5); Base64 of whole 3-byte groups
            // joins end to end.
            let byteCount = BoundedText.utf8ByteCount str
            let length = 4L * ((byteCount + 2L) / 3L)
            Bounds.requireSize length
            let bytes = BoundedText.utf8Bytes str
            let ticker = Bounds.Ticker ctx
            // Written in place: each chunk of 3,072 bytes is 4,096 code
            // units, so the chunk of bytes a chunk of the text encodes is
            // found from where it starts (JYRO-ADR-101).
            JyroString(
                Bounds.create ticker (int length) (ChunkWriter(fun destination at ->
                    let i = at / 4 * 3
                    let n = min (3 * 1024) (bytes.Length - i)
                    let mutable w = 0
                    if not (Convert.TryToBase64Chars(ReadOnlySpan<byte>(bytes, i, n), destination.Slice(at), &w)) then
                        invalidOp "Base64 encoding found no room for its text."
                    w))) :> JyroValue

    /// <summary>Decodes Base64 text back to the text it encodes.</summary>
    /// <arg name="text">The Base64 text to decode, in the standard alphabet with <c>=</c> padding. Spaces, tabs, carriage returns and line feeds in it are ignored. It must be a string: any other value, null included, is a runtime error.</arg>
    /// <returns>The text whose UTF-8 bytes text encodes. Null when text is not valid Base64, that is when, without the ignored characters, its length is not a multiple of four, it holds a character outside the alphabet, or <c>=</c> appears anywhere but in its last one or two places; null too when the decoded bytes are not valid UTF-8. A result that would pass the host's maximum value size is a runtime error.</returns>
    /// <example><c>Base64Decode("SGVsbG8=")</c> returns <c>"Hello"</c>; <c>Base64Decode("abc")</c> returns null, so <c>Base64Decode(input) ?? ""</c> gives a fallback.</example>
    type Base64DecodeFunction() =
        inherit JyroFunctionBase("Base64Decode", Signatures.unary "text" ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            // A string only: any other type, null included, is
            // ArgumentTypeMismatch (spec 90.4).
            let str = args.Get<JyroString>(0).Value
            // Null on malformed input, matching ToNumber, FromJson and
            // ParseDate. Jyro has no try/catch, so a raising parse function
            // cannot be recovered from inside a script - the failure is fatal
            // to the run even when the script has a perfectly good fallback in
            // mind. `??` is the language's existing idiom for exactly this.
            //
            // Made a chunk at a time with a count per chunk (spec 100.2.5):
            // the four white-space characters FromBase64String skips are
            // dropped, whole groups of four are decoded, and the bytes are
            // decoded as UTF-8 once their length in code units is known and
            // allowed (spec 100.8.7, 100.8.8). Bytes that are not valid UTF-8
            // give null (95.12.9), never a replacement character.
            let ticker = Bounds.Ticker ctx
            let isSkipped (c: char) = c = ' ' || c = '\t' || c = '\r' || c = '\n'
            let mutable kept = 0
            for i = 0 to str.Length - 1 do
                if i % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
                if not (isSkipped str.[i]) then kept <- kept + 1
            let clean = Array.zeroCreate<char> kept
            let mutable k = 0
            for i = 0 to str.Length - 1 do
                if i % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
                let c = str.[i]
                if not (isSkipped c) then
                    clean.[k] <- c
                    k <- k + 1
            if kept % 4 <> 0 then JyroNull.Instance :> JyroValue
            else
                let padding =
                    if kept >= 2 && clean.[kept - 1] = '=' && clean.[kept - 2] = '=' then 2
                    elif kept >= 1 && clean.[kept - 1] = '=' then 1
                    else 0
                // `=` only in the last one or two places of the encoded text
                // (95.12.6), checked over the whole text: each chunk below is
                // decoded as a complete input, which would accept padding at
                // the end of an inner chunk.
                let mutable paddedEarly = false
                for i = 0 to kept - padding - 1 do
                    if i % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
                    if clean.[i] = '=' then paddedEarly <- true
                if paddedEarly then JyroNull.Instance :> JyroValue
                else
                let bytes = Array.zeroCreate<byte> (kept / 4 * 3 - padding)
                let chunk = ExecutionCheck.ChunkLength
                let mutable ok = true
                let mutable i = 0
                let mutable written = 0
                while ok && i < kept do
                    ticker.Tick()
                    let n = min chunk (kept - i)
                    let mutable w = 0
                    ok <- Convert.TryFromBase64Chars(ReadOnlySpan<char>(clean, i, n), Span<byte>(bytes, written, bytes.Length - written), &w)
                    written <- written + w
                    i <- i + n
                if not ok then JyroNull.Instance :> JyroValue
                else
                    // The bytes are decoded as UTF-8 in chunks that each end
                    // after a whole character: a chunk ends before the lead
                    // byte of a sequence the chunk would cut. The encoding
                    // keeps no state between two calls, so a character split
                    // across two chunks would read as invalid.
                    //
                    // The encoding throws on invalid bytes rather than
                    // putting U+FFFD for them (95.12.9, 90.4.3): a lone
                    // continuation byte, an overlong form, an encoded
                    // surrogate, a value above U+10FFFF or a sequence cut
                    // short. Every chunk is checked by the count below, before
                    // anything is written, and a failure there is null. A
                    // valid sequence has at most three continuation bytes, so
                    // a chunk boundary never cuts one; and chunks that are
                    // each valid UTF-8 join to valid UTF-8.
                    let utf8 = Text.UTF8Encoding(false, true)
                    let chunkEndAt (start: int) =
                        let mutable finish = min bytes.Length (start + chunk)
                        if finish < bytes.Length then
                            // Back over up to three continuation bytes to
                            // the lead byte they follow.
                            let mutable back = 0
                            while back < 3 && finish - 1 > start && (bytes.[finish] &&& 0xC0uy) = 0x80uy do
                                finish <- finish - 1
                                back <- back + 1
                        finish
                    let mutable charCount = 0L
                    let mutable valid = true
                    i <- 0
                    while valid && i < bytes.Length do
                        ticker.Tick()
                        let finish = chunkEndAt i
                        try
                            charCount <- charCount + int64 (utf8.GetCharCount(bytes, i, finish - i))
                        with :? Text.DecoderFallbackException ->
                            valid <- false
                        i <- finish
                    if not valid then JyroNull.Instance :> JyroValue
                    else
                    Bounds.requireSize charCount
                    // Written in place, the same chunks as the count
                    // (JYRO-ADR-101).
                    let next = ref 0
                    JyroString(
                        Bounds.create ticker (int charCount) (ChunkWriter(fun destination at ->
                            let start = next.Value
                            let finish = chunkEndAt start
                            next.Value <- finish
                            utf8.GetChars(ReadOnlySpan<byte>(bytes, start, finish - start), destination.Slice(at))))) :> JyroValue

    /// <summary>Reads JSON text into a value.</summary>
    /// <arg name="text">The JSON text to read. Comments and trailing commas are accepted. It must be a string: any other value, null included, is a runtime error.</arg>
    /// <returns>The value the text describes: an object, an array, a string, a number, a boolean or null. Null when text is not JSON, holds a number too large to represent, or holds a string with an unpaired surrogate escape; null too when the text is the JSON literal <c>null</c>. Text that, read from its start, nests arrays or objects too deeply or passes the host's maximum value size before the point where it stops being JSON is a runtime error, not null.</returns>
    /// <example><c>FromJson("{\"a\": [1, 2]}")</c> returns <c>{"a": [1, 2]}</c>; <c>FromJson("not json")</c> returns null.</example>
    type FromJsonFunction() =
        inherit JyroFunctionBase("FromJson", Signatures.unary "text" ParameterType.String ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // A string only: any other type, null included, is
            // ArgumentTypeMismatch (spec 90.4), raised outside the reading
            // below so it is never turned into null.
            let jsonStr = args.Get<JyroString>(0).Value
            // Read by the same reader as the host's JSON doors (`ReadDocument`),
            // but in the script's dialect (spec 95.13): trailing commas and
            // comments are accepted, and the maximum value size applies
            // (`FromJsonBounded`). Returns null for text that is not JSON
            // rather than raising.
            //
            // Read a token at a time from the start of the text, checking the
            // limits as it goes (spec 100.2.5). Where the text exceeds the
            // maximum nesting depth or the maximum value size before it stops
            // being something FromJson can interpret, the reading raises
            // `ValueNestingLimitExceeded` or `ValueSizeLimitExceeded`, and
            // those are not turned into null; where it stops first, the
            // reading raises `InvalidJson`, or `NumberOverflow` for a number
            // outside the range of a double (spec 95.13), and the result is
            // null (spec 90.4.2, 100.7, 100.8, JYRO-REQ-1655, 1656, 1658,
            // ADR-104).
            //
            // Only those two codes are "text that is not JSON". Every other
            // exception - a time limit, a cancellation, the size and depth
            // codes, and an engine failure such as out-of-memory - is not a
            // verdict on the text and reaches the run, which reports an
            // unexpected one as its internal error (JYRO-DEF-708).
            try
                JyroValue.FromJsonBounded(jsonStr)
            with
            | :? JyroRuntimeException as ex
                when ex.Code = MessageCode.InvalidJson
                     || ex.Code = MessageCode.NumberOverflow -> JyroNull.Instance :> JyroValue

    /// <summary>Writes a value as JSON text.</summary>
    /// <arg name="value">The value to write. Any value, null included, except a function: a function, at any depth, is a runtime error. So is a value that holds itself, is nested too deeply or whose text would pass the host's maximum value size, and a string or property name that holds an unpaired surrogate.</arg>
    /// <returns>The JSON text of value, with no spaces or line breaks between its parts. A number is written in its string form, as <c>1.5</c> for <c>1.50</c>; null is written as <c>null</c>. Properties are written in the order the object holds them. Never null.</returns>
    /// <example><c>ToJson({"a": [1, true, null]})</c> returns <c>"{\"a\":[1,true,null]}"</c>.</example>
    type ToJsonFunction() =
        inherit JyroFunctionBase("ToJson", Signatures.unary "value" ParameterType.Any ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // Refused above the maximum value size, and for a cyclic or
            // too-deep value first (spec 100.8.7, 100.8.9). An unpaired
            // surrogate names this function (JYRO-ADR-128).
            JyroString(ValueWalk.ScriptJson(args.[0], this.Name + "()")) :> JyroValue

    /// <summary>Pauses the script for a number of milliseconds.</summary>
    /// <remarks>Waits: the script does nothing else until the pause is over. The pause counts toward the run's time limit; when that limit is reached, or the host cancels the run, during the pause, the run ends with that error at once rather than after the full pause.</remarks>
    /// <arg name="milliseconds">How long to pause, in milliseconds: a whole number from 0 to 2147483647. Zero returns at once. A fractional number, a negative number, a number above that range, null or any other type is a runtime error.</arg>
    /// <returns>Always null, once the full pause has passed.</returns>
    /// <example><c>Sleep(250)</c> pauses for a quarter of a second.</example>
    type SleepFunction() =
        inherit JyroFunctionBase("Sleep",
            Signatures.create
                [ Parameter.Required("milliseconds", ParameterType.Number) ]
                ParameterType.Null)
        override _.Effect = FunctionEffect.Blocking
        override this.Execute(args, ctx) =
            let ms = JyroArguments.ReadNonNegativeInt32(args, 0, "Sleep()", "milliseconds")
            let pause = TimeSpan.FromMilliseconds(float ms)
            let token = ctx.CancellationToken
            let budget =
                if ctx.HasLimiter && ctx.Limiter.Options.MaxExecutionTime < TimeSpan.MaxValue then
                    Some ctx.Limiter.Options.MaxExecutionTime
                else None
            let raiseForToken () =
                // The token fired but neither limit check did: report the
                // cause it fired for, the host's cancellation or the time
                // budget, rather than sleeping on through it.
                if ctx.HostCancellationRequested then
                    JyroError.raiseRuntime MessageCode.CanceledByHost [||]
                else
                    let budgetMs =
                        match budget with
                        | Some b -> int b.TotalMilliseconds
                        | None -> 0
                    JyroError.raiseRuntime MessageCode.ExecutionTimeLimitExceeded [| box budgetMs |]
            let sw = System.Diagnostics.Stopwatch.StartNew()
            if OperatingSystem.IsBrowser() then
                // The browser (WebAssembly) runtime has one thread: a timed
                // wait there has no other thread to end it, and a Task-based
                // wait deadlocks. So Sleep waits by reading its own Stopwatch
                // until the full `ms` has passed, and needs no other thread
                // (spec 90.12.1, JYRO-ADR-169 point 2, JYRO-DEF-707).
                //
                // Between readings it checks the run's limits, which read the
                // limiter's own Stopwatch and so need no timer either: the
                // time limit ends the pause (90.12.2). It checks the token
                // too, so a cancel already made, from a host function on this
                // thread, ends it (90.12.3). The page's thread is blocked for
                // the whole pause, as the Jyro.Api README says.
                while sw.Elapsed < pause do
                    ctx.CheckExecutionLimits()
                    if token.IsCancellationRequested then raiseForToken ()
                ctx.CheckExecutionLimits()
            else
                // A kernel timed wait on the run's own thread, never a Task
                // (JYRO-ADR-168 point 2). `Task.Delay(...).GetResult()` woke
                // only from a timer callback on the shared thread pool, so a
                // busy pool held the script seconds past its time limit
                // (JYRO-DEF-698), and it counted `ms` on the timer queue's
                // coarse tick, so `Sleep(1)` could end almost at once
                // (JYRO-DEF-495).
                //
                // The pause is over only when this Stopwatch shows the full
                // `ms`: a timed wait may itself time out up to an OS tick
                // early, so its timeout ends nothing. Each wait lasts at most
                // the rest of the pause or the rest of the time limit,
                // whichever is less, so the limit is noticed on time even if
                // the token were late; the token's handle is set at the limit
                // by the engine's own deadline threads and at once by the host's
                // `Cancel()` on the host's thread, so neither needs a pool
                // thread to wake this wait.
                while sw.Elapsed < pause do
                    ctx.CheckExecutionLimits()
                    if token.IsCancellationRequested then raiseForToken ()
                    let pauseLeft = pause - sw.Elapsed
                    let waitFor =
                        match budget with
                        | Some b ->
                            let budgetLeft = b - ctx.Limiter.ElapsedTime
                            if budgetLeft < pauseLeft then budgetLeft else pauseLeft
                        | None -> pauseLeft
                    // Whole milliseconds, rounded up, and never 0 while time
                    // remains, so the loop does not spin.
                    let waitMs =
                        let m = Math.Ceiling(waitFor.TotalMilliseconds)
                        if m < 1.0 then 1
                        elif m >= float Int32.MaxValue then Int32.MaxValue - 1
                        else int m
                    if token.CanBeCanceled then
                        token.WaitHandle.WaitOne(waitMs) |> ignore
                    else
                        System.Threading.Thread.Sleep(waitMs)
                ctx.CheckExecutionLimits()
            JyroNull.Instance :> JyroValue

    /// <summary>Compares two objects property by property.</summary>
    /// <arg name="first">The object before the change. Null is read as an empty object. Any other value that is not an object is a runtime error.</arg>
    /// <arg name="second">The object after the change. Null is read as an empty object. Any other value that is not an object is a runtime error.</arg>
    /// <returns>A new object with three properties, each an object: <c>added</c> holds each property of second whose name first does not have, with its value in second; <c>removed</c> holds each property of first whose name second does not have, with its value in first; <c>changed</c> holds, for each name both have with values that are not equal, an object with <c>from</c>, the value in first, and <c>to</c>, the value in second. Values are compared by value, so equal arrays or objects, and two nulls, are not a change. Only the top-level properties are compared. Never null.</returns>
    /// <example><c>Diff({"a": 1, "b": 2}, {"b": 3, "c": 4})</c> returns <c>{"added": {"c": 4}, "removed": {"a": 1}, "changed": {"b": {"from": 2, "to": 3}}}</c>.</example>
    type DiffFunction() =
        inherit JyroFunctionBase("Diff", Signatures.binary "first" ParameterType.Object "second"ParameterType.Object ParameterType.Object)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let obj1 = args.GetObject(0)
            let obj2 = args.GetObject(1)
            let ticker = Bounds.Ticker ctx

            let added = JyroObject()
            let removed = JyroObject()
            let changed = JyroObject()

            for kvp in obj1.Properties do
                ticker.Tick()
                match obj2.Properties.TryGetValue(kvp.Key) with
                | true, v2 ->
                    // Record a change when the values differ. Equality is
                    // reflexive on null (Type System 30.10), so two nulls are
                    // equal and correctly produce no change.
                    if not (kvp.Value.EqualsValue(v2)) then
                        let pair = JyroObject()
                        pair.SetProperty("from", kvp.Value)
                        pair.SetProperty("to", v2)
                        changed.SetPropertyBounded(kvp.Key, pair)
                | false, _ ->
                    removed.SetPropertyBounded(kvp.Key, kvp.Value)

            for kvp in obj2.Properties do
                ticker.Tick()
                if not (obj1.Properties.ContainsKey(kvp.Key)) then
                    added.SetPropertyBounded(kvp.Key, kvp.Value)

            let result = JyroObject()
            result.SetProperty("added", added)
            result.SetProperty("removed", removed)
            result.SetProperty("changed", changed)
            result :> JyroValue

    // Get all utility functions
    let getAll () : IJyroFunction list =
        [ TypeOfFunction()
          CoalesceFunction()
          ToStringFunction()
          ToBooleanFunction()
          KeysFunction()
          ValuesFunction()
          HasPropertyFunction()
          MergeFunction()
          CloneFunction()
          NewGuidFunction()
          Base64EncodeFunction()
          Base64DecodeFunction()
          FromJsonFunction()
          ToJsonFunction()
          SleepFunction()
          DiffFunction() ]

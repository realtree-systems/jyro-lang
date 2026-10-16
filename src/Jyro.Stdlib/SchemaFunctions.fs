namespace RealTree.Jyro

open System
open System.Collections.Generic

// Schema validation functions (Part 96).
//
// `ValidateSchema` validates with a draft 2020-12 validator of Jyro's own
// over Jyro values (JYRO-ADR-149), not a schema library: one reading of the
// schema (JYRO-ADR-144, as amended by JYRO-ADR-148) refuses what 96.2 refuses
// and resolves every `$ref`, and one evaluation applies the keywords of
// 96.2.4 to 96.2.6 to the data. Each keyword applied to a value is one
// counted unit, every text read or built (a key, a reference, a pattern, a
// path, a message) is counted a chunk of `ExecutionCheck.ChunkLength` code
// units at a time, and the path of a failure is built only when the failure
// is reported (spec 100.2.5). Neither walk recurses on the CLR stack, so no
// chain of references or of subschemas can exhaust it.
module internal SchemaFunctions =

    // Counts the work of hashing or comparing `text` whole: one unit, and one
    // more for each `ExecutionCheck.ChunkLength` code units of it (spec
    // 100.2.5), so a long name or pattern is not one count.
    let private countText (ticker: Bounds.Ticker) (text: string) : unit =
        ticker.Tick()
        for _ in 1 .. text.Length / ExecutionCheck.ChunkLength do ticker.Tick()

    // True when `text` is empty or holds only code units with the Unicode
    // property White_Space of Unicode 15.0.0 (spec 10.6.1, 96.1.6,
    // JYRO-ADR-143), every one of which is in the Basic Multilingual Plane;
    // read with a count per chunk.
    let private isBlank (ticker: Bounds.Ticker) (text: string) : bool =
        let mutable blank = true
        let mutable i = 0
        while blank && i < text.Length do
            if i % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
            blank <- Unicode15.isWhiteSpace (int text.[i])
            i <- i + 1
        blank

    /// <summary>Checks that an object has a usable value for each of a list of field names.</summary>
    /// <remarks>A field fails when the object has no property of that name (reason <c>"missing"</c>), when its value is null (reason <c>"null"</c>), or when its value is a string that is empty or holds only white space (reason <c>"empty"</c>). Any other value passes, including <c>0</c>, <c>false</c>, an empty array and an empty object. Each name is one key, looked up as written: a name with a dot in it is never read as a path.</remarks>
    /// <arg name="source">The object to check. Null is read as an empty object, so every field is missing. Any other type that is not an object is a runtime error.</arg>
    /// <arg name="fields">An array of the field names to check, each a string. Null is read as an empty array. An element that is not a string is a runtime error, raised before any field is checked.</arg>
    /// <returns>An object with two properties: <c>valid</c>, true when no field failed, and <c>errors</c>, an array holding one object of <c>field</c> (the name) and <c>reason</c> (<c>"missing"</c>, <c>"null"</c> or <c>"empty"</c>) for each name that failed, in the order of fields; a name listed twice that fails is reported twice. Never null.</returns>
    /// <example><c>ValidateRequired({ "name": " ", "age": 3 }, ["name", "age", "email"])</c> returns <c>{ "valid": false, "errors": [{ "field": "name", "reason": "empty" }, { "field": "email", "reason": "missing" }] }</c>.</example>
    type ValidateRequiredFunction() =
        inherit JyroFunctionBase("ValidateRequired",
            Signatures.create
                [ Parameter.Required("source", ParameterType.Object)
                  Parameter.Required("fields", ParameterType.Array) ]
                ParameterType.Object)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            // `null` for either is the empty container (96.1.12, 96.1.13).
            let target = args.GetObject(0)
            let fields = args.GetArray(1)
            let ticker = Bounds.Ticker ctx
            // Every element of `fields` is a string, checked before any name
            // is looked up: it is a check of the argument (96.1.11, 90.3.8).
            for field in fields.Items do
                ticker.Tick()
                match field with
                | :? JyroString -> ()
                | other ->
                    let actual = if obj.ReferenceEquals(other, null) then JyroNull.Instance :> JyroValue else other
                    JyroError.raiseRuntime MessageCode.ArgumentTypeMismatch
                        [| box "string"; box (JyroArguments.TypeName actual) |]
            let missing = JyroString("missing")
            let nullReason = JyroString("null")
            let empty = JyroString("empty")
            let errors = JyroArray()
            // One element per name of `fields`, in its order, a name that
            // occurs twice giving its error twice (96.1.8 to 96.1.10): an
            // object of `field` and then `reason`. The list is refused at the
            // maximum value size (spec 100.8.7).
            let report (name: JyroString) (reason: JyroString) =
                let error = JyroObject(2)
                error.SetProperty("field", name)
                error.SetProperty("reason", reason)
                errors.AddBounded(error)
            for field in fields.Items do
                ticker.Tick()
                let name = field :?> JyroString
                // A key with a dot in it is one key (30.5.6): the name is
                // looked up as it is, never as a path.
                countText ticker name.Value
                match target.Properties.TryGetValue(name.Value) with
                | true, value when obj.ReferenceEquals(value, null) || value.IsNull -> report name nullReason
                | true, (:? JyroString as text) when isBlank ticker text.Value -> report name empty
                | true, _ -> ()
                | _ -> report name missing
            let result = JyroObject(2)
            result.SetProperty("valid", JyroBoolean.FromBoolean(errors.Count = 0))
            result.SetProperty("errors", errors)
            result :> JyroValue

    // The keyword whose value names the target of a reference, and the
    // function name every diagnostic from this module carries.
    let [<Literal>] private RefKeyword = "$ref"
    let [<Literal>] private FunctionName = "ValidateSchema()"

    // The text a `SchemaInvalid` gives for a keyword's value: a string as it
    // is, a number, a boolean or `null` as its string form, and a container
    // or a function by its type, so that the diagnostic is not made by
    // writing a whole subschema.
    let private describe (value: JyroValue) : string =
        match value with
        | v when obj.ReferenceEquals(v, null) -> TypeNames.typeOfName JyroValueType.Null
        | :? JyroString as s -> s.Value
        | v when v.ValueType = JyroValueType.Array || v.ValueType = JyroValueType.Object
                 || v.ValueType = JyroValueType.Function -> TypeNames.typeOfName v.ValueType
        | v -> v.ToStringValue()

    let private invalid (keyword: string) (shown: string) : 'T =
        JyroError.raiseRuntime MessageCode.SchemaInvalid [| box FunctionName; box keyword; box shown |]

    // The reading of one schema (96.2.3 to 96.2.12, 96.2.19, 96.2.34).
    module private SchemaReading =

        // How a keyword of 96.2.4 to 96.2.6 is read: one subschema, a
        // non-empty list of subschemas, a map from names to subschemas, the
        // reference of `$ref`, or a value that holds no subschema.
        type Kind =
            | One
            | Many
            | Named
            | Reference
            | Plain

        // The kind of the member `key` of a subschema, or none when 96.2.4
        // to 96.2.6 do not list it (96.2.7). Only a member of a subschema is
        // a keyword (JYRO-ADR-144): the members of the value of `enum`,
        // `const`, `default` or `examples` are data.
        let kindOf (key: string) : Kind voption =
            match key with
            | "not" | "if" | "then" | "else" | "items" | "contains" | "additionalProperties" | "propertyNames" ->
                ValueSome One
            | "allOf" | "anyOf" | "oneOf" | "prefixItems" -> ValueSome Many
            | "properties" | "patternProperties" | "$defs" -> ValueSome Named
            | RefKeyword -> ValueSome Reference
            | "type" | "enum" | "const" | "multipleOf" | "maximum" | "exclusiveMaximum" | "minimum"
            | "exclusiveMinimum" | "maxLength" | "minLength" | "pattern" | "maxItems" | "minItems"
            | "uniqueItems" | "maxProperties" | "minProperties" | "required" | "dependentRequired"
            | "$schema" | "$id" | "$comment" | "$anchor" | "title" | "description" | "default" | "examples"
            | "deprecated" | "readOnly" | "writeOnly" | "format" -> ValueSome Plain
            | _ -> ValueNone

        // The simple types of draft 2020-12.
        let simpleTypes = [| "array"; "boolean"; "integer"; "null"; "number"; "object"; "string" |]

        let private zero = JyroNumber(0)

        let private isSchema (value: JyroValue) =
            value :? JyroObject || value :? JyroBoolean

        let private isNonNegativeInteger (value: JyroValue) =
            match value with
            | :? JyroNumber as n -> n.IsInteger && JyroNumber.Compare(n, zero) >= 0
            | _ -> false

        // An array of strings, each once (draft 2020-12's `stringArray`).
        let private isStringSet (ticker: Bounds.Ticker) (value: JyroValue) =
            match value with
            | :? JyroArray as a ->
                let seen = HashSet<string>(StringComparer.Ordinal)
                a.Items |> Seq.forall (fun item ->
                    ticker.Tick()
                    match item with
                    | :? JyroString as s ->
                        countText ticker s.Value
                        seen.Add(s.Value)
                    | _ -> false)
            | _ -> false

        // `^[A-Za-z_][-A-Za-z0-9._]*$`, the form of `$anchor`.
        let private isAnchor (ticker: Bounds.Ticker) (text: string) =
            let first (c: char) = Char.IsAsciiLetter c || c = '_'
            let rest (c: char) = Char.IsAsciiLetterOrDigit c || c = '-' || c = '.' || c = '_'
            let mutable ok = text.Length > 0 && first text.[0]
            let mutable i = 1
            while ok && i < text.Length do
                if i % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
                ok <- rest text.[i]
                i <- i + 1
            ok

        // True when the value of `key` has the form that draft 2020-12's
        // meta-schemas give it (96.2.8); `kind` is `kindOf key`.
        let formOk (ticker: Bounds.Ticker) (key: string) (kind: Kind) (value: JyroValue) : bool =
            match kind with
            | One -> isSchema value
            | Many ->
                match value with
                | :? JyroArray as a ->
                    a.Count > 0 && a.Items |> Seq.forall (fun item -> ticker.Tick(); isSchema item)
                | _ -> false
            | Named ->
                match value with
                | :? JyroObject as map -> map.Properties.Values |> Seq.forall (fun item -> ticker.Tick(); isSchema item)
                | _ -> false
            | Reference -> value :? JyroString
            | Plain ->
                match key, value with
                | "type", (:? JyroString as s) -> Array.IndexOf(simpleTypes, s.Value) >= 0
                | "type", (:? JyroArray as a) ->
                    // Non-empty, each a simple type, each once.
                    let mutable seen = 0
                    let mutable ok = a.Count > 0
                    let mutable i = 0
                    while ok && i < a.Count do
                        ticker.Tick()
                        match a.Items.[i] with
                        | :? JyroString as s ->
                            let bit = Array.IndexOf(simpleTypes, s.Value)
                            if bit < 0 || (seen &&& (1 <<< bit)) <> 0 then ok <- false
                            else seen <- seen ||| (1 <<< bit)
                        | _ -> ok <- false
                        i <- i + 1
                    ok
                | "type", _ -> false
                | ("enum" | "examples"), v -> v :? JyroArray
                | ("const" | "default"), _ -> true
                | "multipleOf", (:? JyroNumber as n) -> JyroNumber.Compare(n, zero) > 0
                | ("maximum" | "exclusiveMaximum" | "minimum" | "exclusiveMinimum"), v -> v :? JyroNumber
                | ("maxLength" | "minLength" | "maxItems" | "minItems" | "maxProperties" | "minProperties"), v ->
                    isNonNegativeInteger v
                | ("pattern" | "$schema" | "$comment" | "title" | "description" | "format"), v -> v :? JyroString
                // A URI reference with no fragment but an empty one.
                | "$id", (:? JyroString as s) ->
                    let hash = Bounds.indexOfOrdinal ticker s.Value "#" 0
                    hash < 0 || hash = s.Value.Length - 1
                | "$anchor", (:? JyroString as s) -> isAnchor ticker s.Value
                | ("uniqueItems" | "deprecated" | "readOnly" | "writeOnly"), v -> v :? JyroBoolean
                | "required", v -> isStringSet ticker v
                | "dependentRequired", (:? JyroObject as map) ->
                    map.Properties.Values |> Seq.forall (fun names -> ticker.Tick(); isStringSet ticker names)
                | _ -> false

        // The text after `#` of a local `$ref`, read as a URI fragment
        // (96.2.10, RFC 6901 section 6): each `%` and the two hexadecimal
        // digits after it are the byte they name, every other character its
        // UTF-8 bytes, and the bytes are read as UTF-8. None when a `%` is not
        // followed by two hexadecimal digits or the bytes are not UTF-8,
        // which is a reference that does not resolve. Read and written a
        // chunk at a time with a count per chunk.
        let private decodeFragment (ticker: Bounds.Ticker) (reference: string) : string option =
            let hex (c: char) =
                if c >= '0' && c <= '9' then int c - int '0'
                elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
                elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
                else -1
            let utf8 = Text.UTF8Encoding(false, true)
            let bytes = ResizeArray<byte>(reference.Length)
            let one = Array.zeroCreate<byte> 4
            let mutable ok = true
            let mutable i = 1
            let mutable nextCount = i
            while ok && i < reference.Length do
                if i >= nextCount then
                    ticker.Tick()
                    nextCount <- i + ExecutionCheck.ChunkLength
                let c = reference.[i]
                if c = '%' then
                    if i + 2 < reference.Length && hex reference.[i + 1] >= 0 && hex reference.[i + 2] >= 0 then
                        bytes.Add(byte (hex reference.[i + 1] * 16 + hex reference.[i + 2]))
                        i <- i + 3
                    else ok <- false
                elif int c < 0x80 then
                    bytes.Add(byte c)
                    i <- i + 1
                else
                    // The schema was checked for unpaired surrogates, so a
                    // surrogate here is half of a pair.
                    let width = if Char.IsHighSurrogate c && i + 1 < reference.Length then 2 else 1
                    try
                        let n = utf8.GetBytes(reference.AsSpan(i, width), Span<byte>(one))
                        for k = 0 to n - 1 do bytes.Add(one.[k])
                    with :? ArgumentException -> ok <- false
                    i <- i + width
            if not ok then None
            else
                let decoder = utf8.GetDecoder()
                let all = Runtime.InteropServices.CollectionsMarshal.AsSpan(bytes)
                let chars = Array.zeroCreate<char> (ExecutionCheck.ChunkLength + 4)
                let text = Text.StringBuilder(bytes.Count)
                try
                    let mutable at = 0
                    while at < all.Length do
                        ticker.Tick()
                        let n = min ExecutionCheck.ChunkLength (all.Length - at)
                        let last = at + n = all.Length
                        let written = decoder.GetChars(all.Slice(at, n), Span<char>(chars), last)
                        text.Append(chars, 0, written) |> ignore
                        at <- at + n
                    Some (Bounds.ofBuilder ticker text)
                with :? ArgumentException -> None

        // The reference token of `pointer` from `start` to `finish`, with
        // `~1` read as `/` and `~0` as `~` (RFC 6901), or None for a `~`
        // followed by anything else. Written a chunk at a time.
        let private token (ticker: Bounds.Ticker) (pointer: string) (start: int) (finish: int) : string option =
            let text = Text.StringBuilder(finish - start)
            let mutable ok = true
            let mutable i = start
            while ok && i < finish do
                ticker.Tick()
                let stop = min finish (i + ExecutionCheck.ChunkLength)
                while ok && i < stop do
                    let c = pointer.[i]
                    if c = '~' then
                        let next = if i + 1 < finish then pointer.[i + 1] else ' '
                        if next = '0' || next = '1' then
                            text.Append(if next = '0' then '~' else '/') |> ignore
                            i <- i + 2
                        else ok <- false
                    else
                        text.Append(c) |> ignore
                        i <- i + 1
            if ok then Some (Bounds.ofBuilder ticker text) else None

        // The value the JSON Pointer `pointer` (RFC 6901) names in `root`, or
        // None. An array index is `0` or a decimal integer with no leading
        // zero, below the array's length.
        let private resolvePointer (ticker: Bounds.Ticker) (root: JyroValue) (pointer: string) : JyroValue option =
            if pointer.Length = 0 then Some root
            elif pointer.[0] <> '/' then None
            else
                let mutable current = Some root
                let mutable at = 1
                while current.IsSome && at <= pointer.Length do
                    ticker.Tick()
                    let next = Bounds.indexOfOrdinal ticker pointer "/" at
                    let finish = if next < 0 then pointer.Length else next
                    current <-
                        match token ticker pointer at finish, current with
                        | Some name, Some (:? JyroObject as o) ->
                            countText ticker name
                            match o.Properties.TryGetValue(name) with
                            | true, v when not (obj.ReferenceEquals(v, null)) -> Some v
                            | true, _ -> Some (JyroNull.Instance :> JyroValue)
                            | _ -> None
                        | Some name, Some (:? JyroArray as a) ->
                            // The length test comes first, and the parse runs
                            // only on a token it admits: at most 10 digits, so
                            // the parse never reads a token whose length the
                            // script chooses (spec 100.2.5).
                            let isIndex =
                                name.Length > 0 && name.Length <= 10 && name |> Seq.forall Char.IsAsciiDigit
                                && (name = "0" || name.[0] <> '0')
                            if not isIndex then None
                            else
                                match Int32.TryParse(name, Globalization.NumberStyles.None, Globalization.CultureInfo.InvariantCulture) with
                                | true, n when n < a.Count -> Some a.Items.[n]
                                | _ -> None
                        | _ -> None
                    at <- finish + 1
                current

        // The target of the reference `reference` (96.2.10), or
        // `SchemaInvalid` when it does not start with `#`, does not resolve
        // in `root` as written, or names a value that is neither an object
        // nor a boolean (96.2.12).
        let private target (ticker: Bounds.Ticker) (root: JyroObject) (reference: string) : JyroValue =
            if reference.Length = 0 || reference.[0] <> '#' then invalid RefKeyword reference
            match decodeFragment ticker reference |> Option.bind (resolvePointer ticker root) with
            | Some ((:? JyroObject | :? JyroBoolean) as found) -> found
            | _ -> invalid RefKeyword reference

        // What the reading of a schema gives its evaluation: the target of
        // each subschema's `$ref`, the compiled `pattern` of each subschema,
        // and the keys of each subschema's `patternProperties`, each with its
        // compiled pattern and its subschema. Keyed by the subschema itself.
        type Index =
            { Targets: Dictionary<JyroObject, JyroValue>
              Patterns: Dictionary<JyroObject, EcmaRegex.Pattern>
              Keys: Dictionary<JyroObject, struct (EcmaRegex.Pattern * JyroValue)[]> }

        // The one walk of the schema (JYRO-ADR-144, JYRO-ADR-148): every
        // subschema, the root included, each once, in document order. A
        // subschema is the root, the value of `not`, `if`, `then`, `else`,
        // `items`, `contains`, `additionalProperties` and `propertyNames`,
        // each element of `allOf`, `anyOf`, `oneOf` and `prefixItems`, each
        // member value of `properties`, `patternProperties` and `$defs`, and
        // the target of each `$ref` wherever it sits: under a name that is
        // not a keyword, at a map, or inside the value of `default`, `enum`,
        // `const` or `examples` (96.2.10). Every member of every subschema is
        // checked as the walk meets it: one that 96.2.4 to 96.2.6 do not list
        // (96.2.7), a value of another form than draft 2020-12 gives (96.2.8)
        // and a `$ref` that names no subschema (96.2.12) raise
        // `SchemaInvalid`, whatever the data reaches. Iterative, with a count
        // per member and per subschema.
        let private walk (ticker: Bounds.Ticker) (root: JyroObject)
                         : ResizeArray<JyroObject> * Dictionary<JyroObject, JyroValue> =
            let found = ResizeArray<JyroObject>()
            let targets = Dictionary<JyroObject, JyroValue>(HashIdentity.Reference)
            let seen = HashSet<JyroObject>(HashIdentity.Reference)
            let stack = Stack<JyroValue>()
            stack.Push(root)
            while stack.Count > 0 do
                ticker.Tick()
                match stack.Pop() with
                | :? JyroObject as o when seen.Add(o) ->
                    found.Add(o)
                    let below = ResizeArray<JyroValue>()
                    for kvp in o.Properties do
                        ticker.Tick()
                        let key = kvp.Key
                        let value = if obj.ReferenceEquals(kvp.Value, null) then JyroNull.Instance :> JyroValue else kvp.Value
                        match kindOf key with
                        | ValueNone -> invalid key (describe value)
                        | ValueSome kind ->
                            if not (formOk ticker key kind value) then invalid key (describe value)
                            match kind with
                            | One -> below.Add(value)
                            | Many -> for item in (value :?> JyroArray).Items do below.Add(item)
                            | Named -> for entry in (value :?> JyroObject).Properties do below.Add(entry.Value)
                            | Reference ->
                                let reached = target ticker root (value :?> JyroString).Value
                                targets.[o] <- reached
                                below.Add(reached)
                            | Plain -> ()
                    for i = below.Count - 1 downto 0 do
                        ticker.Tick()
                        stack.Push(below.[i])
                | _ -> ()
            found, targets

        // Refuses a schema that holds a chain of steps returning to the
        // subschema it starts at (96.2.34): a step goes from a subschema to
        // the target of its `$ref`, or to a subschema of its `allOf`,
        // `anyOf`, `oneOf`, `not`, `if`, `then` or `else`, each of which
        // applies to the same value, so evaluating such a chain would never
        // end. Every subschema is a start, so a chain in a `$defs` entry that
        // no `$ref` names is found too. A depth-first search, iterative, so a
        // long chain cannot exhaust the stack; the step that closes the chain
        // names it.
        let private refuseStepCycles (ticker: Bounds.Ticker) (subschemas: ResizeArray<JyroObject>)
                                     (targets: Dictionary<JyroObject, JyroValue>) : unit =
            let steps (o: JyroObject) : struct (string * string * JyroObject) list =
                [ match targets.TryGetValue(o) with
                  | true, (:? JyroObject as t) ->
                      yield struct (RefKeyword, (o.Properties.[RefKeyword] :?> JyroString).Value, t)
                  | _ -> ()
                  for keyword in [ "allOf"; "anyOf"; "oneOf" ] do
                      match o.Properties.TryGetValue(keyword) with
                      | true, (:? JyroArray as a) ->
                          for i = 0 to a.Count - 1 do
                              ticker.Tick()
                              match a.Items.[i] with
                              | :? JyroObject as t -> yield struct (keyword, string i, t)
                              | _ -> ()
                      | _ -> ()
                  for keyword in [ "not"; "if"; "then"; "else" ] do
                      match o.Properties.TryGetValue(keyword) with
                      | true, (:? JyroObject as t) -> yield struct (keyword, keyword, t)
                      | _ -> () ]
            // 1: on the current path; 2: finished, no cycle through it.
            let state = Dictionary<JyroObject, int>(HashIdentity.Reference)
            for start in subschemas do
                ticker.Tick()
                if not (state.ContainsKey start) then
                    let stack = Stack<struct (JyroObject * struct (string * string * JyroObject) list)>()
                    state.[start] <- 1
                    stack.Push(struct (start, steps start))
                    while stack.Count > 0 do
                        ticker.Tick()
                        let struct (node, pending) = stack.Pop()
                        match pending with
                        | [] -> state.[node] <- 2
                        | struct (keyword, via, next) :: rest ->
                            stack.Push(struct (node, rest))
                            match state.TryGetValue next with
                            | true, 1 -> invalid keyword via
                            | true, _ -> ()
                            | _ ->
                                state.[next] <- 1
                                stack.Push(struct (next, steps next))

        // Reads `schema` (96.2.3): the walk with its refusals, the step-cycle
        // search, and then every pattern of every subschema compiled by the
        // matcher of the regular expression functions (96.2.19, REQ-1244), so
        // one that is not valid raises `RegexInvalidPattern` whatever `data`
        // is (96.2.21). A pattern met twice is compiled once.
        let read (ticker: Bounds.Ticker) (schema: JyroObject) : Index =
            let subschemas, targets = walk ticker schema
            refuseStepCycles ticker subschemas targets
            // Keyed with a comparer that hashes and compares a long pattern
            // a chunk at a time with a check between (spec 100.2.5).
            let compiled = Dictionary<string, EcmaRegex.Pattern>(EcmaRegex.OrdinalComparer(ticker.CheckNow))
            let compile (pattern: string) =
                countText ticker pattern
                match compiled.TryGetValue pattern with
                | true, regex -> regex
                | _ ->
                    let regex = StringFunctions.compilePattern FunctionName ticker pattern
                    compiled.[pattern] <- regex
                    regex
            let patterns = Dictionary<JyroObject, EcmaRegex.Pattern>(HashIdentity.Reference)
            let keys = Dictionary<JyroObject, struct (EcmaRegex.Pattern * JyroValue)[]>(HashIdentity.Reference)
            for o in subschemas do
                ticker.Tick()
                match o.Properties.TryGetValue("pattern") with
                | true, (:? JyroString as s) -> patterns.[o] <- compile s.Value
                | _ -> ()
                match o.Properties.TryGetValue("patternProperties") with
                | true, (:? JyroObject as map) ->
                    keys.[o] <-
                        [| for entry in map.Properties do
                             ticker.Tick()
                             yield struct (compile entry.Key, entry.Value) |]
                | _ -> ()
            { Targets = targets; Patterns = patterns; Keys = keys }

    // A location in `data`: its parent, and the key of a property (`Key`)
    // or, when `Key` is null, the index of an element. The root is null.
    // Extending a location is one fixed step that copies no text; the JSON
    // Pointer of a location is written only when a failure there is
    // reported (JYRO-ADR-149).
    [<AllowNullLiteral>]
    type private Place(parent: Place, key: string | null, index: int) =
        member _.Parent = parent
        member _.Key = key
        member _.Index = index

    // Where the failures of the subschemas applied to a value go. The
    // result's sink keeps every failure, with its location and keyword. A
    // probe, for a subschema whose failures are not reported (`anyOf`,
    // `oneOf`, `not`, `if`, `contains`, `propertyNames`), only says whether
    // one failed, and stops the evaluation under it at the first.
    [<Sealed>]
    type private Sink(keep: bool) =
        let failures = ResizeArray<struct (Place * string)>()
        let mutable failed = false
        member _.Failed = failed
        member _.Stopped = failed && not keep
        member _.Fail(place: Place, keyword: string) =
            failed <- true
            if keep then failures.Add(struct (place, keyword))
        member _.Failures = failures

    // True when two elements of `items` are equal as 30.10 gives (the
    // script's `==`, `EqualsValue`), which is not transitive across exact
    // and inexact numbers (JYRO-ADR-34): the elements are sorted by the
    // projection of every number to its double and equality is decided
    // inside each run of equal projection, with a count per comparison, in
    // O(n log n) comparisons but for runs only a host can make
    // (JYRO-ADR-173, EqualRuns.fs). Nothing in the result depends on the
    // order.
    let private hasDuplicate (ticker: Bounds.Ticker) (items: JyroArray) : bool =
        EqualRuns.firstOccurrences ticker items.Items |> Array.exists not

    // Applies the root schema to `data` (96.2.4 to 96.2.6, as draft 2020-12
    // gives them) and returns its failures: the location of the value the
    // failed keyword was applied to and the keyword reported (96.2.23 to
    // 96.2.27, 96.2.36 to 96.2.42).
    //
    // The evaluation runs from a stack of tasks, never by recursion, so
    // neither a long chain of `$ref` and `allOf` steps nor deep data can
    // exhaust the CLR stack. A subschema whose result decides a keyword is
    // evaluated into a probe, with the task that reads the probe pushed
    // beneath it, so it runs once everything the subschema pushed has run.
    // Each subschema applied and each keyword applied to a value is one
    // count; a key looked up is counted by its length; a pattern is matched
    // under the regex timeout with the limits checked before and during the
    // match (90.10.3, spec 100.2.5).
    let private evaluate (ctx: JyroExecutionContext) (ticker: Bounds.Ticker) (index: SchemaReading.Index)
                         (root: JyroObject) (data: JyroValue) : ResizeArray<struct (Place * string)> =
        let tasks = Stack<unit -> unit>()
        let valueOf (v: JyroValue) = if obj.ReferenceEquals(v, null) then JyroNull.Instance :> JyroValue else v
        let matches (regex: EcmaRegex.Pattern) (text: string) =
            ticker.CheckNow()
            not (isNull (StringFunctions.execPattern FunctionName ctx ticker regex text 0))
        let isType (name: string) (instance: JyroValue) =
            match name, instance with
            | "null", v -> v.IsNull
            | "boolean", (:? JyroBoolean) -> true
            | "number", (:? JyroNumber) -> true
            // Every number whose value is an integer, 5.0 included (96.2.16).
            | "integer", (:? JyroNumber as n) -> n.IsInteger
            | "string", (:? JyroString) -> true
            | "array", (:? JyroArray) -> true
            | "object", (:? JyroObject) -> true
            | _ -> false
        // By value, as 30.10 and 30.11 give (96.2.17): exactly when both are
        // exact, else in double precision.
        let isMultiple (n: JyroNumber) (m: JyroNumber) =
            let inexact () = n.Value % m.Value = 0.0
            if n.IsExact && m.IsExact then
                try n.DecimalValue % m.DecimalValue = 0m with :? OverflowException -> inexact ()
            else inexact ()
        let above (count: int) (bound: JyroValue) = JyroNumber.Compare(JyroNumber(count), bound :?> JyroNumber) > 0
        let below (count: int) (bound: JyroValue) = JyroNumber.Compare(JyroNumber(count), bound :?> JyroNumber) < 0
        let at (place: Place) (key: string) = Place(place, key, -1)
        let element (place: Place) (i: int) = Place(place, null, i)

        let rec apply (schema: JyroValue) (instance: JyroValue) (place: Place) (sink: Sink) (by: string) : unit =
            tasks.Push(fun () -> run schema instance place sink by)

        // One subschema applied to one value. A boolean subschema `false`
        // fails with the keyword that applied it (96.2.39).
        and run (schema: JyroValue) (instance: JyroValue) (place: Place) (sink: Sink) (by: string) : unit =
            if not sink.Stopped then
                ticker.Tick()
                match schema with
                | :? JyroBoolean as b -> if not b.Value then sink.Fail(place, by)
                | :? JyroObject as o ->
                    for kvp in o.Properties do
                        if not sink.Stopped then
                            ticker.Tick()
                            keyword o kvp.Key (valueOf kvp.Value) instance place sink
                | _ -> ()

        // One keyword of the subschema `o` applied to one value.
        and keyword (o: JyroObject) (key: string) (value: JyroValue) (instance: JyroValue) (place: Place) (sink: Sink) : unit =
            let fail () = sink.Fail(place, key)
            match key with
            | "type" ->
                let ok =
                    match value with
                    | :? JyroString as s -> isType s.Value instance
                    | :? JyroArray as a -> a.Items |> Seq.exists (fun t -> isType (t :?> JyroString).Value instance)
                    | _ -> true
                if not ok then fail ()
            | "enum" ->
                let a = value :?> JyroArray
                let mutable found = false
                let mutable i = 0
                while not found && i < a.Count do
                    ticker.Tick()
                    if (valueOf a.Items.[i]).EqualsValue(instance) then found <- true
                    i <- i + 1
                if not found then fail ()
            | "const" -> if not (value.EqualsValue(instance)) then fail ()
            | "multipleOf" | "maximum" | "exclusiveMaximum" | "minimum" | "exclusiveMinimum" ->
                match instance with
                | :? JyroNumber as n ->
                    let m = value :?> JyroNumber
                    let failed =
                        match key with
                        | "multipleOf" -> not (isMultiple n m)
                        | "maximum" -> JyroNumber.Compare(n, m) > 0
                        | "exclusiveMaximum" -> JyroNumber.Compare(n, m) >= 0
                        | "minimum" -> JyroNumber.Compare(n, m) < 0
                        | _ -> JyroNumber.Compare(n, m) <= 0
                    if failed then fail ()
                | _ -> ()
            // UTF-16 code units (96.2.18).
            | "maxLength" ->
                match instance with
                | :? JyroString as s -> if above s.Length value then fail ()
                | _ -> ()
            | "minLength" ->
                match instance with
                | :? JyroString as s -> if below s.Length value then fail ()
                | _ -> ()
            // A match at any position (96.2.20).
            | "pattern" ->
                match instance with
                | :? JyroString as s -> if not (matches index.Patterns.[o] s.Value) then fail ()
                | _ -> ()
            | "maxItems" ->
                match instance with
                | :? JyroArray as a -> if above a.Count value then fail ()
                | _ -> ()
            | "minItems" ->
                match instance with
                | :? JyroArray as a -> if below a.Count value then fail ()
                | _ -> ()
            | "uniqueItems" ->
                match value, instance with
                | (:? JyroBoolean as b), (:? JyroArray as a) when b.Value ->
                    if hasDuplicate ticker a then fail ()
                | _ -> ()
            | "prefixItems" ->
                match instance with
                | :? JyroArray as a ->
                    let prefix = value :?> JyroArray
                    for i = min a.Count prefix.Count - 1 downto 0 do
                        ticker.Tick()
                        apply prefix.Items.[i] (valueOf a.Items.[i]) (element place i) sink key
                | _ -> ()
            // The elements after those of `prefixItems`.
            | "items" ->
                match instance with
                | :? JyroArray as a ->
                    let start =
                        match o.Properties.TryGetValue("prefixItems") with
                        | true, (:? JyroArray as prefix) -> prefix.Count
                        | _ -> 0
                    for i = a.Count - 1 downto start do
                        ticker.Tick()
                        apply value (valueOf a.Items.[i]) (element place i) sink key
                | _ -> ()
            // One failure at the array, none of the elements (96.2.41).
            | "contains" ->
                match instance with
                | :? JyroArray as a ->
                    let rec from (i: int) =
                        if i >= a.Count then fail ()
                        elif not sink.Stopped then
                            ticker.Tick()
                            let probe = Sink(false)
                            tasks.Push(fun () -> if probe.Failed then from (i + 1))
                            apply value (valueOf a.Items.[i]) (element place i) probe key
                    from 0
                | _ -> ()
            | "maxProperties" ->
                match instance with
                | :? JyroObject as subject -> if above subject.Count value then fail ()
                | _ -> ()
            | "minProperties" ->
                match instance with
                | :? JyroObject as subject -> if below subject.Count value then fail ()
                | _ -> ()
            // At the location of the absent property (96.2.24).
            | "required" ->
                match instance with
                | :? JyroObject as subject ->
                    for name in (value :?> JyroArray).Items do
                        ticker.Tick()
                        let name = (name :?> JyroString).Value
                        countText ticker name
                        if not (subject.Properties.ContainsKey name) then sink.Fail(at place name, key)
                | _ -> ()
            // One failure for each absent property, at its location (96.2.36).
            | "dependentRequired" ->
                match instance with
                | :? JyroObject as subject ->
                    for entry in (value :?> JyroObject).Properties do
                        ticker.Tick()
                        countText ticker entry.Key
                        if subject.Properties.ContainsKey entry.Key then
                            for name in (entry.Value :?> JyroArray).Items do
                                ticker.Tick()
                                let name = (name :?> JyroString).Value
                                countText ticker name
                                if not (subject.Properties.ContainsKey name) then sink.Fail(at place name, key)
                | _ -> ()
            | "properties" ->
                match instance with
                | :? JyroObject as subject ->
                    for entry in (value :?> JyroObject).Properties do
                        if not sink.Stopped then
                            ticker.Tick()
                            countText ticker entry.Key
                            match subject.Properties.TryGetValue(entry.Key) with
                            | true, v -> apply (valueOf entry.Value) (valueOf v) (at place entry.Key) sink key
                            | _ -> ()
                | _ -> ()
            // Every key that matches the name, each at any position.
            | "patternProperties" ->
                match instance with
                | :? JyroObject as subject ->
                    let keys = index.Keys.[o]
                    for kvp in subject.Properties do
                        for struct (regex, subschema) in keys do
                            if not sink.Stopped then
                                ticker.Tick()
                                if matches regex kvp.Key then
                                    apply (valueOf subschema) (valueOf kvp.Value) (at place kvp.Key) sink key
                | _ -> ()
            // The names that `properties` does not list and no key of
            // `patternProperties` matches.
            | "additionalProperties" ->
                match instance with
                | :? JyroObject as subject ->
                    let listed =
                        match o.Properties.TryGetValue("properties") with
                        | true, (:? JyroObject as map) -> Some map
                        | _ -> None
                    let keys =
                        match index.Keys.TryGetValue(o) with
                        | true, keys -> keys
                        | _ -> Array.empty
                    for kvp in subject.Properties do
                        if not sink.Stopped then
                            ticker.Tick()
                            countText ticker kvp.Key
                            let isListed =
                                match listed with
                                | Some map -> map.Properties.ContainsKey kvp.Key
                                | None -> false
                            if not isListed && not (keys |> Array.exists (fun (struct (regex, _)) -> matches regex kvp.Key)) then
                                apply value (valueOf kvp.Value) (at place kvp.Key) sink key
                | _ -> ()
            // One failure for each name that fails, at its property, none of
            // the subschema's (96.2.37).
            | "propertyNames" ->
                match instance with
                | :? JyroObject as subject ->
                    for kvp in subject.Properties do
                        if not sink.Stopped then
                            ticker.Tick()
                            let name = kvp.Key
                            let probe = Sink(false)
                            tasks.Push(fun () -> if probe.Failed then sink.Fail(at place name, key))
                            apply value (JyroString(name) :> JyroValue) null probe key
                | _ -> ()
            | "allOf" ->
                let branches = value :?> JyroArray
                for i = branches.Count - 1 downto 0 do
                    ticker.Tick()
                    apply (valueOf branches.Items.[i]) instance place sink key
            // One failure, none of the branches' (96.2.27).
            | "anyOf" ->
                let branches = value :?> JyroArray
                let rec from (i: int) =
                    if i >= branches.Count then fail ()
                    elif not sink.Stopped then
                        ticker.Tick()
                        let probe = Sink(false)
                        tasks.Push(fun () -> if probe.Failed then from (i + 1))
                        apply (valueOf branches.Items.[i]) instance place probe key
                from 0
            | "oneOf" ->
                let branches = value :?> JyroArray
                let rec from (i: int) (passed: int) =
                    if passed > 1 then fail ()
                    elif i >= branches.Count then (if passed = 0 then fail ())
                    elif not sink.Stopped then
                        ticker.Tick()
                        let probe = Sink(false)
                        tasks.Push(fun () -> from (i + 1) (if probe.Failed then passed else passed + 1))
                        apply (valueOf branches.Items.[i]) instance place probe key
                from 0 0
            // One failure at the value (96.2.40).
            | "not" ->
                let probe = Sink(false)
                tasks.Push(fun () -> if not probe.Failed then fail ())
                apply value instance place probe key
            // `if` itself is never reported; the failures of `then` or
            // `else` are (96.2.42, 96.2.38).
            | "if" ->
                let branch (name: string) =
                    match o.Properties.TryGetValue(name) with
                    | true, s -> Some (valueOf s)
                    | _ -> None
                let thenSchema = branch "then"
                let elseSchema = branch "else"
                if thenSchema.IsSome || elseSchema.IsSome then
                    let probe = Sink(false)
                    tasks.Push(fun () ->
                        let chosen, name = if probe.Failed then elseSchema, "else" else thenSchema, "then"
                        match chosen with
                        | Some s -> apply s instance place sink name
                        | None -> ())
                    apply value instance place probe key
            | RefKeyword -> apply index.Targets.[o] instance place sink key
            // `then` and `else` are applied by `if`; `$defs` holds subschemas
            // only for `$ref`; the annotations and `format` have no effect
            // (96.2.5, 96.2.6).
            | _ -> ()

        let result = Sink(true)
        apply (root :> JyroValue) data null result ""
        while tasks.Count > 0 do
            (tasks.Pop()) ()
        result.Failures

    // The JSON Pointer (RFC 6901) of `place`, each key with `~` written as
    // `~0` and `/` as `~1`, and the empty string for the root (96.2.23).
    // Written a chunk at a time with a count per chunk, and refused at the
    // maximum value size (spec 100.8.7).
    let private pathText (ticker: Bounds.Ticker) (place: Place) : string =
        if isNull place then String.Empty
        else
            let tokens = Stack<Place>()
            let mutable p = place
            while not (isNull p) do
                ticker.Tick()
                tokens.Push(p)
                p <- p.Parent
            let text = Text.StringBuilder()
            let fits () = if text.Length > ValueSize.Maximum then ValueSize.Raise()
            while tokens.Count > 0 do
                ticker.Tick()
                let t = tokens.Pop()
                text.Append('/') |> ignore
                if isNull t.Key then text.Append(t.Index) |> ignore
                else
                    let key = nonNull t.Key
                    let mutable i = 0
                    while i < key.Length do
                        ticker.Tick()
                        let stop = min key.Length (i + ExecutionCheck.ChunkLength)
                        while i < stop do
                            match key.[i] with
                            | '~' -> text.Append("~0") |> ignore
                            | '/' -> text.Append("~1") |> ignore
                            | c -> text.Append(c) |> ignore
                            i <- i + 1
                        fits ()
                fits ()
            Bounds.ofBuilder ticker text

    // The locations from the root down to `place`, the root itself left
    // out: the segments of its JSON Pointer, in order. One count each.
    let private segments (ticker: Bounds.Ticker) (place: Place) : Place[] =
        let found = ResizeArray<Place>()
        let mutable p = place
        while not (isNull p) do
            ticker.Tick()
            found.Add(p)
            p <- p.Parent
        found.Reverse()
        found.ToArray()

    // The code units of the JSON Pointer of a location (96.2.23), from its
    // segment `start` on, read one at a time without writing the pointer: a
    // `/` before each segment, then the key with `~` as `~0` and `/` as `~1`,
    // or the index in decimal digits. `Next` gives -1 at the end, which
    // orders before every code unit, so a pointer orders before every
    // pointer it is a prefix of. One count per `ExecutionCheck.ChunkLength`
    // code units read.
    [<Sealed>]
    type private PointerReader(tokens: Place[], start: int, ticker: Bounds.Ticker) =
        let digits = Array.zeroCreate<char> 11
        let mutable digitCount = 0
        let mutable segment = start
        // -1: the `/` of `segment` is next.
        let mutable position = -1
        let mutable pending = -1
        let mutable read = 0
        member this.Next() : int =
            read <- read + 1
            if read % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
            if pending >= 0 then
                let c = pending
                pending <- -1
                c
            elif segment >= tokens.Length then -1
            elif position < 0 then
                position <- 0
                let t = tokens.[segment]
                if isNull t.Key then
                    // The digits of a non-negative index, most significant first.
                    let mutable n = t.Index
                    digitCount <- 0
                    let mutable go = true
                    while go do
                        digits.[digitCount] <- char (int '0' + n % 10)
                        digitCount <- digitCount + 1
                        n <- n / 10
                        go <- n > 0
                    Array.Reverse(digits, 0, digitCount)
                int '/'
            else
                let t = tokens.[segment]
                if isNull t.Key then
                    if position < digitCount then
                        position <- position + 1
                        int digits.[position - 1]
                    else
                        segment <- segment + 1
                        position <- -1
                        this.Next()
                elif position < (nonNull t.Key).Length then
                    let c = (nonNull t.Key).[position]
                    position <- position + 1
                    match c with
                    | '~' -> pending <- int '0'; int '~'
                    | '/' -> pending <- int '1'; int '~'
                    | c -> int c
                else
                    segment <- segment + 1
                    position <- -1
                    this.Next()

    // Orders two locations by their JSON Pointers, by ordinal comparison of
    // UTF-16 code units (96.2.28), without writing either: the segments the
    // two share (the same location objects from the root) write the same
    // code units and are passed over at once, and the rest is read a code
    // unit at a time from both.
    let private comparePlaces (ticker: Bounds.Ticker) (a: Place[]) (b: Place[]) : int =
        if obj.ReferenceEquals(a, b) then 0
        else
            let mutable shared = 0
            while shared < a.Length && shared < b.Length && obj.ReferenceEquals(a.[shared], b.[shared]) do
                ticker.Tick()
                shared <- shared + 1
            if shared = a.Length && shared = b.Length then 0
            else
                let left = PointerReader(a, shared, ticker)
                let right = PointerReader(b, shared, ticker)
                let mutable result = 0
                let mutable finished = false
                while not finished do
                    let x = left.Next()
                    let y = right.Next()
                    if x <> y then
                        result <- (if x < y then -1 else 1)
                        finished <- true
                    elif x < 0 then finished <- true
                result

    // The number of code units in the JSON Pointer of the location whose
    // segments are `tokens`, counted without writing it, a chunk at a time.
    let private pointerLength (ticker: Bounds.Ticker) (tokens: Place[]) : int64 =
        let mutable length = 0L
        for t in tokens do
            ticker.Tick()
            length <- length + 1L
            if isNull t.Key then
                let mutable n = t.Index
                length <- length + 1L
                while n >= 10 do
                    length <- length + 1L
                    n <- n / 10
            else
                let key = nonNull t.Key
                length <- length + int64 key.Length
                for i = 0 to key.Length - 1 do
                    if i % ExecutionCheck.ChunkLength = 0 then ticker.Tick()
                    let c = key.[i]
                    if c = '~' || c = '/' then length <- length + 1L
        length

    // The result of `ValidateSchema` from its failures: each an object of
    // `path`, `keyword` and `message` (96.2.22, 96.2.26), in order of `path`
    // and then of `keyword` by ordinal comparison of UTF-16 code units
    // (96.2.28), and one for each `path` and `keyword` (96.2.29).
    //
    // The failures are ordered and made unique as locations and keywords,
    // with no text written (JYRO-DEF-719). Text is written only for an
    // element of the result: before its path and message are written, the
    // element's size is added to a running total of the result's size, as
    // spec 100.8.6 counts it, and the total passing the maximum value size
    // raises `ValueSizeLimitExceeded` (spec 100.8.7), so no more than that
    // much text is ever held. Path and message are written a chunk at a
    // time with a count per chunk.
    let private report (ticker: Bounds.Ticker) (failures: ResizeArray<struct (Place * string)>) : JyroArray =
        let entries =
            Array.init failures.Count (fun i ->
                ticker.Tick()
                let struct (place, keyword) = failures.[i]
                struct (segments ticker place, keyword))
        let order (struct (p1: Place[], k1: string)) (struct (p2: Place[], k2: string)) =
            let c = comparePlaces ticker p1 p2
            if c <> 0 then c else String.CompareOrdinal(k1, k2)
        StableSort.sortInPlace entries order ticker.Tick
        let errors = JyroArray()
        // The array itself (spec 100.8.6).
        let mutable size = 1L
        for i = 0 to entries.Length - 1 do
            ticker.Tick()
            let struct (tokens, keyword) = entries.[i]
            let repeated = i > 0 && order entries.[i - 1] entries.[i] = 0
            if not repeated then
                let pathLength = pointerLength ticker tokens
                let locationLength = if pathLength = 0L then 6L else pathLength
                let messageLength = int64 keyword.Length + 4L + locationLength
                // The object, its three keys, and its three strings.
                size <- size + 1L + int64 ("path".Length + "keyword".Length + "message".Length)
                        + (1L + pathLength) + (1L + int64 keyword.Length) + (1L + messageLength)
                if size > int64 ValueSize.Maximum then ValueSize.Raise()
                let path = if tokens.Length = 0 then String.Empty else pathText ticker tokens.[tokens.Length - 1]
                let location = if path.Length = 0 then "(root)" else path
                let text = Text.StringBuilder(int messageLength)
                text.Append(keyword).Append(" at ") |> ignore
                Bounds.appendChunked ticker text location
                let error = JyroObject(3)
                error.SetProperty("path", JyroString(path))
                error.SetProperty("keyword", JyroString(keyword))
                error.SetProperty("message", JyroString(Bounds.ofBuilder ticker text))
                errors.AddBounded(error)
        errors

    /// <summary>Validates a value against a JSON Schema and lists every failure.</summary>
    /// <remarks>
    /// The schema is read as JSON Schema draft 2020-12, limited to these keywords: <c>type</c>, <c>enum</c>, <c>const</c>, <c>multipleOf</c>, <c>maximum</c>, <c>exclusiveMaximum</c>, <c>minimum</c>, <c>exclusiveMinimum</c>, <c>maxLength</c>, <c>minLength</c>, <c>pattern</c>, <c>maxItems</c>, <c>minItems</c>, <c>uniqueItems</c>, <c>prefixItems</c>, <c>items</c>, <c>contains</c>, <c>maxProperties</c>, <c>minProperties</c>, <c>required</c>, <c>dependentRequired</c>, <c>properties</c>, <c>patternProperties</c>, <c>additionalProperties</c>, <c>propertyNames</c>, <c>allOf</c>, <c>anyOf</c>, <c>oneOf</c>, <c>not</c>, <c>if</c>, <c>then</c>, <c>else</c>, <c>$ref</c> and <c>$defs</c>, together with <c>$schema</c>, <c>$id</c>, <c>$anchor</c>, <c>$comment</c>, <c>title</c>, <c>description</c>, <c>default</c>, <c>examples</c>, <c>deprecated</c>, <c>readOnly</c>, <c>writeOnly</c> and <c>format</c>, which are accepted and have no effect on the result.
    /// The whole schema is checked before the value is, whatever the value is. A runtime error is raised for a keyword not in that list, for a keyword whose value has the wrong form for draft 2020-12, for a <c>$ref</c> that is not a local reference starting with <c>#</c> or that does not name an object or a boolean in the schema, for a chain of <c>$ref</c>, <c>allOf</c>, <c>anyOf</c>, <c>oneOf</c>, <c>not</c>, <c>if</c>, <c>then</c> or <c>else</c> steps that comes back to where it started, and for a <c>pattern</c> or <c>patternProperties</c> key that is not a valid regular expression.
    /// The type <c>"integer"</c> accepts every number whose value is whole, <c>5.0</c> included. <c>maxLength</c> and <c>minLength</c> count UTF-16 code units. A <c>pattern</c> matches when it is found anywhere in the string, with the same regular expressions as the regex functions. <c>enum</c>, <c>const</c> and <c>uniqueItems</c> compare values as the <c>==</c> operator does.
    /// </remarks>
    /// <arg name="data">The value to validate: any value that can be written as JSON, null included. A function value, a value that contains itself, nesting deeper than the limit, or a string with an unpaired surrogate is a runtime error, as for <c>ToJson</c>.</arg>
    /// <arg name="schema">The schema, an object. Null, or any other type, is a runtime error; boolean subschemas <c>true</c> and <c>false</c> are accepted inside it.</arg>
    /// <returns>An array of failures, empty when data is valid. Each failure is an object of <c>path</c>, the JSON Pointer of the value that failed (the empty string for data itself, and for <c>required</c> and <c>dependentRequired</c> the location of the absent property), <c>keyword</c>, the keyword that failed, and <c>message</c>, the keyword followed by <c>" at "</c> and the path, or <c>"(root)"</c> for the empty path. <c>anyOf</c>, <c>oneOf</c>, <c>not</c> and <c>contains</c> each give one failure at the value, without the failures of their subschemas; <c>propertyNames</c> gives one failure at each property whose name fails; <c>if</c> is never reported, only the failures of <c>then</c> or <c>else</c>. The failures are sorted by path and then by keyword, comparing code unit by code unit, and each pair of path and keyword appears once. Never null.</returns>
    /// <example><c>ValidateSchema({ "age": "ten" }, { "type": "object", "properties": { "age": { "type": "integer" } }, "required": ["name"] })</c> returns <c>[{ "path": "/age", "keyword": "type", "message": "type at /age" }, { "path": "/name", "keyword": "required", "message": "required at /name" }]</c>.</example>
    type ValidateSchemaFunction() =
        inherit JyroFunctionBase("ValidateSchema",
            Signatures.create
                [ Parameter.Required("data", ParameterType.Any)
                  Parameter.Required("schema", ParameterType.Object) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure

        override this.Execute(args, ctx) =
            let ticker = Bounds.Ticker ctx
            // `data` is read as the JSON value `ToJson` gives for it, `null`
            // as `null` (96.2.13, 96.2.32), and first, through all of its own
            // checks, because it is the first parameter (90.3.8): a function
            // value, a cycle, too deep a nesting and an unpaired surrogate are
            // refused as `ToJson` refuses them (96.2.14, 96.2.15, 96.2.35),
            // naming this function (JYRO-ADR-128). It is not written: the
            // evaluation reads the value itself.
            let data =
                if args.Count = 0 || obj.ReferenceEquals(args.[0], null) then JyroNull.Instance :> JyroValue
                else args.[0]
            ValueWalk.RequireJsonValue(data, FunctionName, true)
            // `null` is not a schema (96.2.30).
            let schema = args.Get<JyroObject>(1)
            // The walks below rely on the schema being acyclic and within the
            // maximum nesting depth.
            ValueWalk.RequireJsonValue(schema, FunctionName, false)
            // Properties of the schema alone, so raised whatever `data` is.
            let index = SchemaReading.read ticker schema
            report ticker (evaluate ctx ticker index schema data) :> JyroValue

    // Get all schema functions
    let getAll () : IJyroFunction list =
        [ ValidateRequiredFunction()
          ValidateSchemaFunction() ]

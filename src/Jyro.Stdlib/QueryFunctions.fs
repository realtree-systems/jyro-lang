namespace RealTree.Jyro

open System

// Query and predicate functions for arrays: AnyByField, AllByField,
// FindByField, WhereByField, CountIf, Select, Project and Omit.
module internal QueryFunctions =

    // Relational comparison for the `<`, `<=`, `>` and `>=` string operators
    // these functions accept.
    //
    // This must agree with the language's relational operators, because the
    // operator is spelled the same way and a script author reasonably expects
    // `WhereByField(rows, "n", "<", 5)` to select the rows where `row.n < 5`.
    // It previously carried its own hand-rolled comparison and admitted one
    // case the operators reject: booleans. `CountIf(rows, "active", ">", false)`
    // counted rows while `row.active > false` was a runtime error, so the two
    // spellings of one question disagreed.
    //
    // Numbers route through the canonical numeric ordering rather than a
    // second copy of it. Note this is deliberately *not* `CompareForSort`,
    // which is a total order that never raises: sorting must place every pair
    // of values somehow, whereas asking whether one object is less than
    // another has no answer and should say so.
    let private compareValues (left: JyroValue) (right: JyroValue) : int =
        match left, right with
        | :? JyroNumber as ln, (:? JyroNumber as rn) -> JyroNumber.Compare(ln, rn)
        | :? JyroString as ls, (:? JyroString as rs) -> BoundedText.compareOrdinal ls.Value rs.Value
        | _ ->
            JyroError.raiseRuntime MessageCode.IncomparableTypes [| box (TypeNames.typeOfName left.ValueType); box (TypeNames.typeOfName right.ValueType) |]

    let private evaluateComparison (left: JyroValue) (op: string) (right: JyroValue) : bool =
        match op with
        | "==" -> left.EqualsValue(right)
        | "!=" -> not (left.EqualsValue(right))
        | "<" -> compareValues left right < 0
        | "<=" -> compareValues left right <= 0
        | ">" -> compareValues left right > 0
        | ">=" -> compareValues left right >= 0
        | _ ->
            JyroError.raiseRuntime MessageCode.UnsupportedComparisonOperator [| box op |]

    // The `operator` argument of a field test, at index 2, after `items` and
    // `fieldName` have passed their checks (90.3.8). It is read as a string
    // parameter is (160.3.7): null as the empty string, a number or a
    // boolean as its string form (97.1.15, 97.1.22), a container or a
    // function refused with ArgumentTypeMismatch. Then it must be one of the
    // six operators of 97.1.3, checked here, before any element is examined
    // and whatever `items` holds (97.1.4): the check used to be made only
    // inside a comparison, so a misspelled operator over an empty array, or
    // over elements none of which was an object, passed unnoticed.
    let private readOperator (args: Collections.Generic.IReadOnlyList<JyroValue>) : string =
        let op = args.GetString(2)
        match op with
        | "==" | "!=" | "<" | "<=" | ">" | ">=" -> op
        | _ -> JyroError.raiseRuntime MessageCode.UnsupportedComparisonOperator [| box op |]

    // Matches a single object's field value against a query value under a
    // string operator. Equality (`==`/`!=`) is reflexive on null and flows
    // straight through evaluateComparison (Type System 30.10), so a null
    // field matches `== null` and a null query value. Relational operators
    // are undefined against a null field, so such rows are treated as
    // non-matching rather than raising.
    let private fieldMatches (fieldValue: JyroValue) (op: string) (compareValue: JyroValue) : bool =
        if fieldValue.IsNull && op <> "==" && op <> "!=" then false
        else evaluateComparison fieldValue op compareValue

    /// <summary>Tells whether any object in an array has a field that passes a comparison.</summary>
    /// <remarks>Elements that are not objects are passed over. A field the object does not have is read as null. With <c>"=="</c> and <c>"!="</c> values are compared as the <c>==</c> operator compares them, so a null field equals null. With the other operators a null field never passes; otherwise both sides must be numbers or both strings (strings compared code unit by code unit), and any other pair is a runtime error.</remarks>
    /// <arg name="items">The array to search. Null is read as an empty array. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the field to compare, one key as written: a name with a dot in it is never read as a path. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="operator">The comparison, one of <c>"=="</c>, <c>"!="</c>, <c>"&lt;"</c>, <c>"&lt;="</c>, <c>"&gt;"</c> and <c>"&gt;="</c>, with the field on the left and value on the right. Any other value, null included, is a runtime error, raised before any element is examined, even for an empty array.</arg>
    /// <arg name="value">The value to compare each field with. Any value, null included.</arg>
    /// <returns>True when at least one object's field passes the comparison, false otherwise, and false for an empty array. Never null.</returns>
    /// <example><c>AnyByField([{ "n": 1 }, { "n": 7 }], "n", "&gt;", 5)</c> returns true.</example>
    type AnyByFieldFunction() =
        inherit JyroFunctionBase("AnyByField",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String)
                  Parameter.Required("operator", ParameterType.String)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let op = readOperator args
            let compareValue = args.[3]
            let mutable i = 0
            let mutable found = false
            while i < arr.Count && not found do
                ctx.CheckExecutionLimits()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as obj ->
                    let fieldValue = obj.GetProperty(fieldName)
                    if fieldMatches fieldValue op compareValue then
                        found <- true
                | _ -> ()
                i <- i + 1
            JyroBoolean.FromBoolean(found) :> JyroValue

    /// <summary>Tells whether every element of an array is an object whose field passes a comparison.</summary>
    /// <remarks>An element that is not an object fails. A field the object does not have is read as null. With <c>"=="</c> and <c>"!="</c> values are compared as the <c>==</c> operator compares them, so a null field equals null. With the other operators a null field never passes; otherwise both sides must be numbers or both strings (strings compared code unit by code unit), and any other pair is a runtime error. The search stops at the first element that fails.</remarks>
    /// <arg name="items">The array to test. Null is read as an empty array. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the field to compare, one key as written: a name with a dot in it is never read as a path. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="operator">The comparison, one of <c>"=="</c>, <c>"!="</c>, <c>"&lt;"</c>, <c>"&lt;="</c>, <c>"&gt;"</c> and <c>"&gt;="</c>, with the field on the left and value on the right. Any other value, null included, is a runtime error, raised before any element is examined, even for an empty array.</arg>
    /// <arg name="value">The value to compare each field with. Any value, null included.</arg>
    /// <returns>True when every element is an object whose field passes the comparison, and true for an empty array; false otherwise. Never null.</returns>
    /// <example><c>AllByField([{ "n": 6 }, { "n": 7 }], "n", "&gt;", 5)</c> returns true.</example>
    type AllByFieldFunction() =
        inherit JyroFunctionBase("AllByField",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String)
                  Parameter.Required("operator", ParameterType.String)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let op = readOperator args
            let compareValue = args.[3]
            let mutable i = 0
            let mutable allMatch = true
            while i < arr.Count && allMatch do
                ctx.CheckExecutionLimits()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as obj ->
                    let fieldValue = obj.GetProperty(fieldName)
                    if not (fieldMatches fieldValue op compareValue) then
                        allMatch <- false
                | _ ->
                    allMatch <- false
                i <- i + 1
            JyroBoolean.FromBoolean(allMatch) :> JyroValue

    /// <summary>Finds the first object in an array whose field passes a comparison.</summary>
    /// <remarks>Elements that are not objects are passed over. A field the object does not have is read as null. With <c>"=="</c> and <c>"!="</c> values are compared as the <c>==</c> operator compares them, so a null field equals null. With the other operators a null field never passes; otherwise both sides must be numbers or both strings (strings compared code unit by code unit), and any other pair is a runtime error.</remarks>
    /// <arg name="items">The array to search. Null is read as an empty array. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the field to compare, one key as written: a name with a dot in it is never read as a path. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="operator">The comparison, one of <c>"=="</c>, <c>"!="</c>, <c>"&lt;"</c>, <c>"&lt;="</c>, <c>"&gt;"</c> and <c>"&gt;="</c>, with the field on the left and value on the right. Any other value, null included, is a runtime error, raised before any element is examined, even for an empty array.</arg>
    /// <arg name="value">The value to compare each field with. Any value, null included.</arg>
    /// <returns>The first object, in array order, whose field passes the comparison: the element itself, not a copy. Null when no object passes, including for an empty array.</returns>
    /// <example><c>FindByField([{ "id": 1 }, { "id": 2 }], "id", "==", 2)</c> returns <c>{ "id": 2 }</c>.</example>
    type FindByFieldFunction() =
        inherit JyroFunctionBase("FindByField",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String)
                  Parameter.Required("operator", ParameterType.String)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let op = readOperator args
            let compareValue = args.[3]
            let mutable i = 0
            let mutable result = Unchecked.defaultof<JyroValue>
            while i < arr.Count && obj.ReferenceEquals(result, null) do
                ctx.CheckExecutionLimits()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as jobj ->
                    let fieldValue = jobj.GetProperty(fieldName)
                    if fieldMatches fieldValue op compareValue then
                        result <- item
                | _ -> ()
                i <- i + 1
            if obj.ReferenceEquals(result, null) then JyroNull.Instance :> JyroValue else result

    /// <summary>Selects the objects in an array whose field passes a comparison.</summary>
    /// <remarks>Elements that are not objects are left out. A field the object does not have is read as null. With <c>"=="</c> and <c>"!="</c> values are compared as the <c>==</c> operator compares them, so a null field equals null. With the other operators a null field never passes; otherwise both sides must be numbers or both strings (strings compared code unit by code unit), and any other pair is a runtime error.</remarks>
    /// <arg name="items">The array to filter. Null is read as an empty array. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the field to compare, one key as written: a name with a dot in it is never read as a path. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="operator">The comparison, one of <c>"=="</c>, <c>"!="</c>, <c>"&lt;"</c>, <c>"&lt;="</c>, <c>"&gt;"</c> and <c>"&gt;="</c>, with the field on the left and value on the right. Any other value, null included, is a runtime error, raised before any element is examined, even for an empty array.</arg>
    /// <arg name="value">The value to compare each field with. Any value, null included.</arg>
    /// <returns>A new array of the objects whose field passes the comparison, in their order in items; the objects themselves, not copies. Empty when none passes. Never null.</returns>
    /// <example><c>WhereByField([{ "n": 1 }, { "n": 7 }], "n", "&lt;", 5)</c> returns <c>[{ "n": 1 }]</c>.</example>
    type WhereByFieldFunction() =
        inherit JyroFunctionBase("WhereByField",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String)
                  Parameter.Required("operator", ParameterType.String)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let op = readOperator args
            let compareValue = args.[3]
            let result = JyroArray()
            for i = 0 to arr.Count - 1 do
                ctx.CheckExecutionLimits()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as jobj ->
                    let fieldValue = jobj.GetProperty(fieldName)
                    if fieldMatches fieldValue op compareValue then
                        result.AddBounded(item)
                | _ -> ()
            result :> JyroValue

    /// <summary>Takes the value of one field from each element of an array.</summary>
    /// <arg name="items">The array to read. Null is read as an empty array. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the field to take, one key as written: a name with a dot in it is never read as a path. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <returns>A new array with one element for each element of items, in order: the value of the field, or null when the element is not an object or has no such field. Never null itself.</returns>
    /// <example><c>Select([{ "name": "a" }, { "id": 2 }, 5], "name")</c> returns <c>["a", null, null]</c>.</example>
    type SelectFunction() =
        inherit JyroFunctionBase("Select",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            // One element per element of `items` (JYRO-ADR-133 (2)).
            let result = JyroArray(min arr.Count ValueSize.Maximum)
            for i = 0 to arr.Count - 1 do
                ctx.CheckExecutionLimits()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as jobj ->
                    result.AddBounded(jobj.GetProperty(fieldName))
                | _ ->
                    result.AddBounded(JyroNull.Instance)
            result :> JyroValue

    /// <summary>Builds, for each object in an array, a new object with only the listed fields.</summary>
    /// <arg name="items">The array to read. Null is read as an empty array. Elements that are not objects are left out. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fields">An array of the field names to keep, each one key as written: a name with a dot in it is never read as a path. Elements that are not strings are ignored. Null is read as an empty array, which gives empty objects.</arg>
    /// <returns>A new array with one new object for each object in items, in order, holding each listed field in the order of fields, with null for a field the object does not have. Never null.</returns>
    /// <example><c>Project([{ "a": 1, "b": 2 }], ["a", "c"])</c> returns <c>[{ "a": 1, "c": null }]</c>.</example>
    type ProjectFunction() =
        inherit JyroFunctionBase("Project",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fields", ParameterType.Array) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fields = args.GetArray(1)
            let ticker = Bounds.Ticker ctx
            let fieldNames =
                fields.Items
                |> Seq.choose (fun f ->
                    ticker.Tick()
                    match f with
                    | :? JyroString as s -> Some s.Value
                    | _ -> None)
                |> Seq.toList
            let result = JyroArray()
            for i = 0 to arr.Count - 1 do
                ticker.Tick()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as sourceObj ->
                    let projected = JyroObject()
                    // Each field is one key, kept as written, a dot included
                    // (97.6.3, 30.5.6). The key used to be the text after the
                    // last dot, so "x.b" and "y.b" wrote one property "b".
                    for fieldName in fieldNames do
                        ticker.Tick()
                        projected.SetPropertyBounded(fieldName, sourceObj.GetProperty(fieldName))
                    result.AddBounded(projected)
                | _ -> ()
            result :> JyroValue

    /// <summary>Builds, for each object in an array, a new object without the listed fields.</summary>
    /// <arg name="items">The array to read. Null is read as an empty array. Elements that are not objects are left out. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fields">An array of the field names to remove, each matched exactly against the keys, case included. Elements that are not strings are ignored. Null is read as an empty array, which removes nothing.</arg>
    /// <returns>A new array with one new object for each object in items, in order, holding every other field of that object in its original order. Never null.</returns>
    /// <example><c>Omit([{ "a": 1, "b": 2 }], ["b"])</c> returns <c>[{ "a": 1 }]</c>.</example>
    type OmitFunction() =
        inherit JyroFunctionBase("Omit",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fields", ParameterType.Array) ]
                ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fields = args.GetArray(1)
            let ticker = Bounds.Ticker ctx
            let omitSet =
                fields.Items
                |> Seq.choose (fun f ->
                    ticker.Tick()
                    match f with
                    | :? JyroString as s -> Some s.Value
                    | _ -> None)
                |> Set.ofSeq
            let result = JyroArray()
            for i = 0 to arr.Count - 1 do
                ticker.Tick()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as sourceObj ->
                    let omitted = JyroObject()
                    for key in sourceObj.Properties.Keys do
                        ticker.Tick()
                        if not (omitSet.Contains(key)) then
                            omitted.SetPropertyBounded(key, sourceObj.Properties.[key])
                    result.AddBounded(omitted)
                | _ -> ()
            result :> JyroValue

    /// <summary>Counts the objects in an array whose field passes a comparison.</summary>
    /// <remarks>Elements that are not objects are not counted. A field the object does not have is read as null. With <c>"=="</c> and <c>"!="</c> values are compared as the <c>==</c> operator compares them, so a null field equals null. With the other operators a null field never passes; otherwise both sides must be numbers or both strings (strings compared code unit by code unit), and any other pair is a runtime error.</remarks>
    /// <arg name="items">The array to count in. Null is read as an empty array. Any other type that is not an array is a runtime error.</arg>
    /// <arg name="fieldName">The name of the field to compare, one key as written: a name with a dot in it is never read as a path. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="operator">The comparison, one of <c>"=="</c>, <c>"!="</c>, <c>"&lt;"</c>, <c>"&lt;="</c>, <c>"&gt;"</c> and <c>"&gt;="</c>, with the field on the left and value on the right. Any other value, null included, is a runtime error, raised before any element is examined, even for an empty array.</arg>
    /// <arg name="value">The value to compare each field with. Any value, null included.</arg>
    /// <returns>The number of objects whose field passes the comparison, a whole number, 0 when none does. Never null.</returns>
    /// <example><c>CountIf([{ "n": 1 }, { "n": 7 }, { "n": 9 }], "n", "&gt;=", 7)</c> returns 2.</example>
    type CountIfFunction() =
        inherit JyroFunctionBase("CountIf",
            Signatures.create
                [ Parameter.Required("items", ParameterType.Array)
                  Parameter.Required("fieldName", ParameterType.String)
                  Parameter.Required("operator", ParameterType.String)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let fieldName = args.GetString(1)
            let op = readOperator args
            let compareValue = args.[3]
            let mutable count = 0
            for i = 0 to arr.Count - 1 do
                ctx.CheckExecutionLimits()
                let item = arr.Items.[i]
                match item with
                | :? JyroObject as jobj ->
                    let fieldValue = jobj.GetProperty(fieldName)
                    if fieldMatches fieldValue op compareValue then
                        count <- count + 1
                | _ -> ()
            // A count, so exact (97.8.5, 30.2).
            JyroNumber(count) :> JyroValue

    let getAll () : IJyroFunction list =
        [ AnyByFieldFunction()
          AllByFieldFunction()
          FindByFieldFunction()
          WhereByFieldFunction()
          SelectFunction()
          ProjectFunction()
          OmitFunction()
          CountIfFunction() ]

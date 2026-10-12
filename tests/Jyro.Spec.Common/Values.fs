namespace RealTree.Jyro.Spec

open RealTree.Jyro

/// Building, reading and converting Jyro values through the public API.
module Values =

    /// Fail the calling test with `message` (an xUnit failure, not a crash).
    let fail (message: string) : 'T = raise (Xunit.Sdk.XunitException message)

    // -- Making values ------------------------------------------------------

    /// Parse JSON text to a Jyro value (`JyroValue.FromJson`).
    let json (text: string) : JyroValue = JyroValue.FromJson(text)

    /// An exact number from a decimal (`JyroNumber(Decimal)`).
    let exact (d: decimal) : JyroValue = JyroNumber(d) :> JyroValue

    /// A number from a host `double` (`JyroNumber(Double)`), the host-side way to
    /// make an inexact number. This helper does not assert the representation;
    /// assert it with `isExact` where the test depends on it.
    let inexact (d: float) : JyroValue = JyroNumber(d) :> JyroValue

    /// A number from an int (`JyroNumber(Int32)`).
    let int (n: int) : JyroValue = JyroNumber(n) :> JyroValue

    /// A string value.
    let str (s: string) : JyroValue = JyroString(s) :> JyroValue

    /// A boolean value.
    let bool (b: bool) : JyroValue = JyroBoolean.FromBoolean(b) :> JyroValue

    /// The null value.
    let nul : JyroValue = JyroNull.Instance :> JyroValue

    /// An array of the given values, in order.
    let arr (values: JyroValue seq) : JyroValue =
        let a = JyroArray()
        for v in values do a.Add(v)
        a :> JyroValue

    /// An object with the given properties, set in order.
    let obj (pairs: (string * JyroValue) seq) : JyroValue =
        let o = JyroObject()
        for (k, v) in pairs do o.SetProperty(k, v)
        o :> JyroValue

    /// An empty object.
    let emptyObj () : JyroValue = JyroObject() :> JyroValue

    // -- Reading values -----------------------------------------------------

    /// The JSON text of a value (`JyroValue.ToJson()`).
    let toJson (v: JyroValue) : string = v.ToJson()

    /// True when the value is a number held exactly (`JyroNumber.IsExact`).
    /// Fails the test when the value is not a number.
    let isExact (v: JyroValue) : bool =
        match v with
        | :? JyroNumber as n -> n.IsExact
        | _ when isNull (box v) -> fail "isExact: expected a number, got a .NET null"
        | _ -> fail (sprintf "isExact: expected a number, got %A (%s)" v.ValueType (v.ToJson()))

    /// The property `name` of an object value (`GetProperty`).
    let prop (name: string) (v: JyroValue) : JyroValue = v.GetProperty(name)

    /// The number as a double. Fails unless the value is a number.
    let asFloat (v: JyroValue) : float =
        match v with
        | :? JyroNumber as n -> n.Value
        | _ -> fail (sprintf "asFloat: expected a number, got %s" (if isNull (box v) then "<null>" else v.ToJson()))

    /// The number as a decimal (`DecimalValue`). Fails unless the value is a number.
    let asDecimal (v: JyroValue) : decimal =
        match v with
        | :? JyroNumber as n -> n.DecimalValue
        | _ -> fail (sprintf "asDecimal: expected a number, got %s" (if isNull (box v) then "<null>" else v.ToJson()))

    /// The text of a string value. Fails unless the value is a string.
    let asString (v: JyroValue) : string =
        match v with
        | :? JyroString as s -> s.Value
        | _ -> fail (sprintf "asString: expected a string, got %s" (if isNull (box v) then "<null>" else v.ToJson()))

    /// The value of a boolean. Fails unless the value is a boolean.
    let asBool (v: JyroValue) : bool =
        match v with
        | :? JyroBoolean as b -> b.Value
        | _ -> fail (sprintf "asBool: expected a boolean, got %s" (if isNull (box v) then "<null>" else v.ToJson()))

    /// The elements of an array. Fails unless the value is an array.
    let asList (v: JyroValue) : JyroValue list =
        match v with
        | :? JyroArray as a -> List.ofSeq a
        | _ -> fail (sprintf "asList: expected an array, got %s" (if isNull (box v) then "<null>" else v.ToJson()))

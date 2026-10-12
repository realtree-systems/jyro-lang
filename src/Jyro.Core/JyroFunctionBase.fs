namespace RealTree.Jyro

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.CompilerServices

// The arguments of one call, with the function they are passed to, so that
// a reader of `JyroArguments` can name the function being called and the
// parameter its signature declares at an index (JYRO-ADR-192 item 2). The
// compiler passes one to every standard library and host function, and
// `JyroFunctionBase` makes one when a host calls `IJyroFunction.Execute`
// itself. Read-only, and every read goes to the values it holds, so a
// negative index fails as it would on them (JYRO-ADR-118).
[<Sealed>]
type internal CallArguments(func: IJyroFunction, values: IReadOnlyList<JyroValue>) =
    // The function the arguments are passed to.
    member _.Function : IJyroFunction = func

    // The arguments themselves.
    member _.Values : IReadOnlyList<JyroValue> = values

    // The arguments of a call of `func`: `values` itself when it already is
    // the arguments of a call of `func`.
    static member For(func: IJyroFunction, values: IReadOnlyList<JyroValue>) : IReadOnlyList<JyroValue> =
        match values with
        | :? CallArguments as given when obj.ReferenceEquals(given.Function, func) -> values
        | _ -> CallArguments(func, values) :> IReadOnlyList<JyroValue>

    interface IReadOnlyList<JyroValue> with
        member _.Count = values.Count
        member _.Item with get (index: int) = values.[index]
        member _.GetEnumerator() : IEnumerator<JyroValue> = values.GetEnumerator()
        member _.GetEnumerator() : IEnumerator = (values :> IEnumerable).GetEnumerator()

/// <summary>
/// Base class for implementing Jyro functions.
///
/// A host function overrides <c>Effect</c> and <c>Execute</c>, reads its arguments
/// through <c>JyroArguments</c>, and reports an error by throwing an exception
/// made by <c>JyroError.Host</c> or <c>JyroError.Runtime</c>.
///
/// Instances are shared across (potentially concurrent) executions - see the
/// thread-safety contract on IJyroFunction. Keep per-invocation state on the
/// JyroExecutionContext, not on the subclass; hold no mutable instance state
/// unless you synchronize it yourself.
///
/// A function with side effects weakens the sandbox in proportion to what it
/// does: a script reaches nothing outside its input and output
/// except through the functions a host gives it, so a function that reads a
/// file, calls a service or changes shared state hands that power to every
/// script that can call it. Give it the narrowest inputs it can work with,
/// declare its true <c>Effect</c>, and cap it with <c>JyroCompiler.WithMaxEffect</c>.
/// </summary>
[<AbstractClass>]
type JyroFunctionBase(name: string, signature: JyroFunctionSignature) =

    /// <summary>
    /// The name the script calls this function by.
    /// </summary>
    member _.Name : string = name

    /// <summary>
    /// The declared signature, used by the linker to resolve named arguments
    /// and check arity.
    /// </summary>
    member _.Signature : JyroFunctionSignature = signature

    /// <summary>
    /// Observable effect produced by this function. Subclasses must declare
    /// their effect; there is no default. Pure for referentially transparent
    /// helpers, NonDeterministic for reads of the clock or a random number generator, Blocking for calls
    /// that can pause execution, Mutating for reads or writes of shared
    /// state.
    /// </summary>
    abstract member Effect: FunctionEffect

    /// <summary>
    /// Implements the function's behavior, and is the implementation of
    /// <c>IJyroFunction.Execute</c>. Arguments have already passed the linker's
    /// arity check but not a type check - see <c>ParameterType</c> - so an
    /// implementation validates argument types itself, typically with the
    /// readers of <c>JyroArguments</c>.
    /// </summary>
    abstract member Execute: arguments: IReadOnlyList<JyroValue> * context: JyroExecutionContext -> JyroValue

    /// <summary>
    /// Checks one call of this function at link time, before any script runs; see <c>IJyroCallCheck</c>. This one accepts every call.
    /// Override it to refuse a call by throwing an exception made by
    /// <c>JyroError.Host</c>, which reports it as HostLinkError placed at the call.
    /// </summary>
    abstract member CheckCall: call: JyroCallSite -> unit
    default _.CheckCall(call: JyroCallSite) = ignore call

    /// <summary>
    /// F# implements interfaces explicitly, so <c>Name</c>, <c>Signature</c>, <c>Effect</c>
    /// and <c>Execute</c> would reach a C# host only through a cast to
    /// <c>IJyroFunction</c>. The members above put them on the type itself; these
    /// forward to them so there is still one implementation of each.
    /// </summary>
    interface IJyroFunction with
        member this.Name = this.Name
        member this.Signature = this.Signature
        member this.Effect = this.Effect
        member this.Execute(args, ctx) = this.Execute(CallArguments.For(this, args), ctx)

    interface IJyroCallCheck with
        member this.CheckCall(call) = this.CheckCall(call)

/// <summary>
/// The readers a host function reads its arguments with: extension methods on the arguments, each taking the index of the
/// argument it reads.
///
/// Every reader reads an index at or beyond the number of arguments as an
/// argument that is <c>null</c>: a script that leaves out an optional parameter
/// passes nothing for it. A negative index is a host's mistake
/// and fails through the list's indexer like every other argument read. A diagnostic that names a function or a parameter names the
/// function being called and the parameter its signature declares at the
/// index read when the call comes during a run or through
/// <c>IJyroFunction.Execute</c>. A host that calls a <c>JyroFunctionBase</c> subclass's
/// <c>Execute</c> override directly passes a plain list, and such a diagnostic then
/// names neither: call through <c>IJyroFunction</c> to get the names.
/// </summary>
[<AbstractClass; Sealed; Extension>]
type JyroArguments =

    // The Jyro name for a CLR type, for diagnostics.
    //
    // A script author reads these messages and has never heard of
    // `JyroNumber`, so `Floor("12")` says "number", not "JyroNumber".
    static member private DescribeType(t: Type) : string =
        if t = typeof<JyroString> then "string"
        elif t = typeof<JyroNumber> then "number"
        elif t = typeof<JyroBoolean> then "boolean"
        elif t = typeof<JyroArray> then "array"
        elif t = typeof<JyroObject> then "object"
        elif t = typeof<JyroFunction> then "function"
        elif t = typeof<JyroNull> then "null"
        else t.Name

    // The Jyro name of a value's type, as `TypeOf` gives it (spec 95.1.2):
    // the actual type an `ArgumentTypeMismatch` names (130.6.5). The enum's
    // own names are capitalized, which is not how the language spells them.
    static member internal TypeName(value: JyroValue) : string =
        match value.ValueType with
        | JyroValueType.Null -> "null"
        | JyroValueType.Boolean -> "boolean"
        | JyroValueType.Number -> "number"
        | JyroValueType.String -> "string"
        | JyroValueType.Array -> "array"
        | JyroValueType.Object -> "object"
        | JyroValueType.Function -> "function"
        | other -> string other

    // The arguments of an `IntegerOutOfRange`: the value received, the least
    // and the greatest value accepted, then the function and the parameter
    // when there are such (JYRO-ADR-128 item 2). The two that can be absent
    // come last, so leaving one out moves no other; a null `functionName` or
    // `parameterName` gives no argument for it.
    static member internal OutOfRangeArgs
        (value: string, least: obj, greatest: obj, functionName: string | null, parameterName: string | null) : obj[] =
        [| yield box value
           yield least
           yield greatest
           if not (isNull functionName) then
               yield box functionName
               if not (isNull parameterName) then yield box parameterName |]

    // The function being called, as a diagnostic names it (`Name()`), or
    // null when the arguments do not say which it is.
    static member private FunctionName(arguments: IReadOnlyList<JyroValue>) : string | null =
        match arguments with
        | :? CallArguments as call ->
            let name = call.Function.Name
            if String.IsNullOrEmpty name then null else name + "()"
        | _ -> null

    // The name of the parameter the signature of the function being called
    // declares at `index`, or null when it declares none there or the
    // arguments do not say which function it is (JYRO-ADR-128 item 4).
    static member private ParameterName(arguments: IReadOnlyList<JyroValue>, index: int) : string | null =
        match arguments with
        | :? CallArguments as call ->
            let signature = call.Function.Signature
            let parameters = if isNull (box signature) then null else signature.Parameters
            if isNull (box parameters) || index < 0 || index >= parameters.Count then null
            else
                let p = parameters.[index]
                if isNull (box p) || String.IsNullOrEmpty p.Name then null else p.Name
        | _ -> null

    // Raises `ArgumentTypeMismatch` naming the expected type and the actual
    // type of `actual`, in that order (spec 160.3.3, 130.6.5).
    static member private Mismatch(expected: string, actual: JyroValue) : 'T =
        JyroError.raiseRuntime MessageCode.ArgumentTypeMismatch
            [| box expected; box (JyroArguments.TypeName actual) |]

    // The argument at `index`, or `null` when there is none (spec 160.3.11).
    // Only `index >= arguments.Count` is a missing argument; a negative index
    // fails through the indexer (JYRO-ADR-118).
    static member private ArgumentOrNull(arguments: IReadOnlyList<JyroValue>, index: int) : JyroValue =
        if index >= arguments.Count then JyroNull.Instance :> JyroValue
        else
            let value = arguments.[index]
            if isNull (box value) then JyroNull.Instance :> JyroValue else value

    /// <summary>
    /// The argument at <c>index</c> as a <c>T</c>, as given. This is a
    /// typed read, not one of the converting helpers such as <c>GetNumber</c>: it converts
    /// nothing, and <c>null</c> is a <c>T</c> only when <c>T</c> is <c>JyroValue</c> or <c>JyroNull</c>.
    /// Any other argument raises <c>ArgumentTypeMismatch</c> naming the expected
    /// and the actual Jyro types. An index with no argument reads as <c>null</c>,
    /// so <c>Get&lt;JyroValue&gt;</c> returns <c>null</c> for it and every other <c>T</c> raises
    /// <c>ArgumentTypeMismatch</c> naming <c>null</c>.
    /// </summary>
    [<Extension>]
    static member Get<'T when 'T :> JyroValue and 'T : not null>(arguments: IReadOnlyList<JyroValue>, index: int) : 'T =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? 'T as value -> value
        | other -> JyroArguments.Mismatch(JyroArguments.DescribeType typeof<'T>, other)

    /// <summary>
    /// The argument at <c>index</c> as a <c>T</c>, or <c>defaultValue</c> when there is no
    /// argument at <c>index</c> or it is <c>null</c>, as for the standard library,
    /// for every <c>T</c>, <c>JyroValue</c> included.
    ///
    /// A supplied argument of the wrong type is an error, not a reason to use
    /// the default: only an omitted argument is something a default answers.
    /// </summary>
    [<Extension>]
    static member GetOptional<'T when 'T :> JyroValue and 'T : not null>(arguments: IReadOnlyList<JyroValue>, index: int, defaultValue: 'T) : 'T =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroNull -> defaultValue
        | :? 'T as value -> value
        | other -> JyroArguments.Mismatch(JyroArguments.DescribeType typeof<'T>, other)

    /// <summary>
    /// A string argument.
    ///
    /// Null, and no argument, yield the empty string, and a number or boolean
    /// yields its canonical text: each of the three scalars has one written
    /// form a script author would recognize and intend.
    ///
    /// A container is refused with <c>ArgumentTypeMismatch</c> naming <c>string</c> and
    /// its type. An object or an array has no textual form, only a rendering,
    /// and treating that rendering as data means the caller's later string
    /// operations run over syntax nobody wrote.
    /// </summary>
    [<Extension>]
    static member GetString(arguments: IReadOnlyList<JyroValue>, index: int) : string =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroString as s -> s.Value
        | :? JyroNull -> ""
        | :? JyroNumber as n -> n.ToStringValue()
        | :? JyroBoolean as b -> b.ToStringValue()
        | other -> JyroArguments.Mismatch("string", other)

    /// <summary>
    /// A number argument as a <c>double</c>. Null, and no argument,
    /// raise <c>ArgumentTypeMismatch</c>.
    ///
    /// Loses precision: a <c>double</c> holds 53 bits, so a value above 2^53 or an
    /// exact decimal comes back rounded. <c>JyroNumber</c> stores
    /// <c>9007199254740993</c> exactly and this returns <c>9007199254740992</c>. Use it
    /// for genuinely floating-point math - powers, roots, logarithms - and use
    /// <c>GetDecimal</c> or <c>GetInt64</c> for a value that has to survive the trip.
    /// </summary>
    [<Extension>]
    static member GetNumber(arguments: IReadOnlyList<JyroValue>, index: int) : float =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroNumber as n -> n.Value
        | other -> JyroArguments.Mismatch("number", other)

    /// <summary>
    /// A number argument as an exact <c>decimal</c>, for a value that must not be
    /// rounded - money, or an identifier above 2^53. Null, and no argument,
    /// raise <c>ArgumentTypeMismatch</c>; an inexact value outside the range of
    /// <c>decimal</c> raises <c>IntegerOutOfRange</c>.
    /// </summary>
    [<Extension>]
    static member GetDecimal(arguments: IReadOnlyList<JyroValue>, index: int) : decimal =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroNumber as n ->
            if n.IsExact then n.DecimalValue
            else
                // An inexact value converts only if it is inside decimal's
                // range; outside it, `decimal` raises OverflowException, which
                // would escape as the RuntimeError catch-all. `float
                // Decimal.MaxValue` rounds up to exactly 2^96, which is itself
                // out of range, so the bound is strict.
                let v = n.Value
                if Double.IsNaN v || Double.IsInfinity v
                   || v <= float Decimal.MinValue || v >= float Decimal.MaxValue then
                    JyroError.raiseRuntime MessageCode.IntegerOutOfRange
                        (JyroArguments.OutOfRangeArgs(
                            n.ToStringValue(), box Decimal.MinValue, box Decimal.MaxValue,
                            JyroArguments.FunctionName arguments, JyroArguments.ParameterName(arguments, index)))
                decimal v
        | other -> JyroArguments.Mismatch("number", other)

    /// <summary>
    /// A boolean argument. Null, no argument and any other
    /// type raise <c>ArgumentTypeMismatch</c>: a boolean has no empty value to
    /// stand for an absent one, and a reader named for a type returns that
    /// type's value rather than a truthiness.
    /// </summary>
    [<Extension>]
    static member GetBoolean(arguments: IReadOnlyList<JyroValue>, index: int) : bool =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroBoolean as b -> b.Value
        | other -> JyroArguments.Mismatch("boolean", other)

    /// <summary>
    /// An array argument. Null, and no argument, yield an empty <c>JyroArray</c>,
    /// the same choice <c>GetString</c> makes; any other type raises
    /// <c>ArgumentTypeMismatch</c>.
    /// </summary>
    [<Extension>]
    static member GetArray(arguments: IReadOnlyList<JyroValue>, index: int) : JyroArray =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroArray as a -> a
        | :? JyroNull -> JyroArray()
        | other -> JyroArguments.Mismatch("array", other)

    /// <summary>
    /// An object argument. Null, and no argument, yield an empty
    /// <c>JyroObject</c>, the same choice <c>GetString</c> makes; any other type raises
    /// <c>ArgumentTypeMismatch</c>.
    /// </summary>
    [<Extension>]
    static member GetObject(arguments: IReadOnlyList<JyroValue>, index: int) : JyroObject =
        match JyroArguments.ArgumentOrNull(arguments, index) with
        | :? JyroObject as o -> o
        | :? JyroNull -> JyroObject()
        | other -> JyroArguments.Mismatch("object", other)

    // The `int` of an integral number, or `IntegerRequired` for a fractional
    // one and `IntegerOutOfRange` for one outside `int` range. Both checks
    // are needed: `IsInteger` is true for 1e20, which `Convert.ToInt32` would
    // turn into an `OverflowException`.
    static member private ToBoundedInt(n: JyroNumber, functionName: string | null, parameterName: string | null) : int =
        if not n.IsInteger then
            JyroError.raiseRuntime MessageCode.IntegerRequired
                [| box functionName; box parameterName; box (n.ToStringValue()) |]
        let withinInt32 =
            if n.IsExact then
                n.DecimalValue >= decimal Int32.MinValue && n.DecimalValue <= decimal Int32.MaxValue
            else
                n.Value >= float Int32.MinValue && n.Value <= float Int32.MaxValue
        if not withinInt32 then
            JyroError.raiseRuntime MessageCode.IntegerOutOfRange
                (JyroArguments.OutOfRangeArgs(
                    n.ToStringValue(), box Int32.MinValue, box Int32.MaxValue,
                    functionName, parameterName))
        n.ToInt32()

    // `GetInt32` with the function and the parameter its diagnostics name
    // given by the caller: the standard library's own reader, which names
    // them whether or not the arguments say which function they belong to.
    static member internal ReadInt32
        (arguments: IReadOnlyList<JyroValue>, index: int, functionName: string | null, parameterName: string | null) : int =
        JyroArguments.ToBoundedInt(JyroArguments.Get<JyroNumber>(arguments, index), functionName, parameterName)

    // `GetNonNegativeInt32` with the function and the parameter its
    // diagnostics name given by the caller; see `ReadInt32`.
    static member internal ReadNonNegativeInt32
        (arguments: IReadOnlyList<JyroValue>, index: int, functionName: string | null, parameterName: string | null) : int =
        let value = JyroArguments.ReadInt32(arguments, index, functionName, parameterName)
        if value < 0 then
            JyroError.raiseRuntime MessageCode.NonNegativeIntegerRequired
                // The parameter's name alone, then its value and the least
                // value it accepts; the words are the template's, not an
                // argument's (spec 130.6.2, JYRO-ADR-125 item 2).
                [| box functionName; box parameterName; box value; box 0 |]
        value

    /// <summary>
    /// An integer argument as an <c>int</c>, of either sign. Null, and no
    /// argument, raise <c>ArgumentTypeMismatch</c>; a fractional value raises
    /// <c>IntegerRequired</c> and one outside <c>int</c> range <c>IntegerOutOfRange</c>,
    /// each naming the function being called and the parameter at <c>index</c>
    /// when the call comes during a run or through <c>IJyroFunction.Execute</c>;
    /// a direct call of the <c>Execute</c> override gives neither name.
    /// </summary>
    [<Extension>]
    static member GetInt32(arguments: IReadOnlyList<JyroValue>, index: int) : int =
        JyroArguments.ReadInt32(
            arguments, index, JyroArguments.FunctionName arguments, JyroArguments.ParameterName(arguments, index))

    /// <summary>
    /// An integer argument that must not be negative - a count, a width, a
    /// duration, or an index into a container. As <c>GetInt32</c>, and a negative
    /// value raises <c>NonNegativeIntegerRequired</c>.
    /// </summary>
    [<Extension>]
    static member GetNonNegativeInt32(arguments: IReadOnlyList<JyroValue>, index: int) : int =
        JyroArguments.ReadNonNegativeInt32(
            arguments, index, JyroArguments.FunctionName arguments, JyroArguments.ParameterName(arguments, index))

    /// <summary>
    /// An integer argument as an <c>int64</c>, with the same checks as <c>GetInt32</c>
    /// against the range of <c>int64</c>. Exact: a timestamp or a version read
    /// through <c>GetNumber</c> would lose precision above 2^53.
    /// </summary>
    [<Extension>]
    static member GetInt64(arguments: IReadOnlyList<JyroValue>, index: int) : int64 =
        let n = JyroArguments.Get<JyroNumber>(arguments, index)
        let functionName = JyroArguments.FunctionName arguments
        let parameterName = JyroArguments.ParameterName(arguments, index)
        if not n.IsInteger then
            JyroError.raiseRuntime MessageCode.IntegerRequired
                [| box functionName; box parameterName; box (n.ToStringValue()) |]
        let withinInt64 =
            if n.IsExact then
                n.DecimalValue >= decimal Int64.MinValue && n.DecimalValue <= decimal Int64.MaxValue
            else
                // Strictly below 2^63: `float Int64.MaxValue` rounds up to
                // 2^63 itself, so `<=` passed 9223372036854775808, one above
                // the range, into a conversion that wraps (spec 160.3.8).
                // -2^63 is exactly representable and in range.
                n.Value >= float Int64.MinValue && n.Value < 9223372036854775808.0
        if not withinInt64 then
            JyroError.raiseRuntime MessageCode.IntegerOutOfRange
                (JyroArguments.OutOfRangeArgs(
                    n.ToStringValue(), box Int64.MinValue, box Int64.MaxValue,
                    functionName, parameterName))
        if n.IsExact then int64 n.DecimalValue else int64 n.Value

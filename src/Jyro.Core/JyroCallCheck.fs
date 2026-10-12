namespace RealTree.Jyro

open System.Collections.Generic
open System.Runtime.InteropServices

/// <summary>
/// One call of a host function, as its call check sees it at link time.
///
/// Opaque by design: it exposes no syntax tree, which the engine keeps
/// private, and no location, which the diagnostic of a refusal takes from the
/// call itself. A host cannot construct one; the linker makes one for each
/// call it checks.
/// </summary>
[<Sealed>]
type JyroCallSite internal (name: string, argumentNames: IReadOnlyList<string>, literals: JyroValue[]) =
    /// <summary>
    /// The name of the function called.
    /// </summary>
    member _.Name : string = name

    /// <summary>
    /// The number of arguments written in the call.
    /// </summary>
    member _.ArgumentCount : int = literals.Length

    /// <summary>
    /// The names of the call's named arguments in the order they are
    /// written, and empty for a call with none.
    /// </summary>
    member _.ArgumentNames : IReadOnlyList<string> = argumentNames

    /// <summary>
    /// Whether the argument at <c>index</c>, counted from 0 in the order the
    /// arguments are written, is a literal; if it is, <c>value</c> is its value,
    /// read-only. False for an argument that reads a variable, calls a
    /// function or is any other expression, and for an index that is not the
    /// index of an argument of the call.
    /// </summary>
    member _.TryGetLiteral(index: int, [<Out; System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] value: byref<JyroValue>) : bool =
        if index >= 0 && index < literals.Length && not (isNull (box literals.[index])) then
            value <- literals.[index]
            true
        else
            value <- Unchecked.defaultof<JyroValue>
            false

/// <summary>
/// Implemented by a host function that checks each call of it before any
/// script runs.
///
/// The compilation calls <c>CheckCall</c> once for each call of the function in
/// the script and in each library, during Link, after the call has passed the
/// linker's own checks of it: its name resolves, its arguments match the
/// signature, and its effect is within the maximum. A load of a <c>.jyrx</c>
/// artifact does the same for each call in the program it loads. To refuse
/// the call, throw an exception made by <c>JyroError.Host</c>: the call is
/// reported as <c>HostLinkError</c>, whose
/// arguments are the reason and the values, placed at the call, and the
/// compilation fails. Returning accepts the call.
/// </summary>
type IJyroCallCheck =
    /// <summary>
    /// Checks one call of this function; returns to accept it.
    /// </summary>
    abstract member CheckCall: call: JyroCallSite -> unit

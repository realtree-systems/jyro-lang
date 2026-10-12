namespace RealTree.Jyro

/// <summary>
/// Observable effect produced by a Jyro function. Forms a linear ordering
/// from least to most observable, which constrains the functions that may be
/// called inside a given compilation context (e.g. library bodies cap at
/// NonDeterministic).
///
/// An enumeration whose values are that order, so a C# host compares two effects with <c>&lt;</c> and switches on one
/// with constant patterns. Being an enumeration, it is equal by value and
/// ordered by value, from least to most observable, with no structural
/// interface, and its <c>ToString</c> is the member's name.
/// </summary>
type FunctionEffect =
    /// <summary>
    /// Referentially transparent: the same arguments give the same result.
    /// </summary>
    | Pure = 0
    /// <summary>
    /// Reads the clock, a random source or other state that changes by itself.
    /// </summary>
    | NonDeterministic = 1
    /// <summary>
    /// Can pause execution.
    /// </summary>
    | Blocking = 2
    /// <summary>
    /// Reads or writes shared state.
    /// </summary>
    | Mutating = 3

module internal FunctionEffect =

    // Whether `e` is one of the four effects (spec 120.4.1): a value a host
    // made by casting an integer outside them is not (JYRO-REQ-1723).
    [<CompiledName("IsOneOfFour")>]
    let isOneOfFour (e: FunctionEffect) : bool =
        e >= FunctionEffect.Pure && e <= FunctionEffect.Mutating

    // The place of `e` in the order of spec 120.4.2, from 0 for `Pure`.
    [<CompiledName("Rank")>]
    let rank (e: FunctionEffect) : int = int e

    [<CompiledName("Max")>]
    let max (a: FunctionEffect) (b: FunctionEffect) : FunctionEffect =
        if rank a >= rank b then a else b

    [<CompiledName("Exceeds")>]
    let exceeds (effect: FunctionEffect) (ceiling: FunctionEffect) : bool =
        rank effect > rank ceiling

    [<CompiledName("Name")>]
    let name (e: FunctionEffect) : string =
        match e with
        | FunctionEffect.Pure -> "Pure"
        | FunctionEffect.NonDeterministic -> "NonDeterministic"
        | FunctionEffect.Blocking -> "Blocking"
        | FunctionEffect.Mutating -> "Mutating"
        | other -> string (int other)

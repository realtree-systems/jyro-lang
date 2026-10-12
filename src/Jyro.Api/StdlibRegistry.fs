namespace RealTree.Jyro

open System.Collections.Generic
open System.Runtime.InteropServices

/// <summary>The functions of the Jyro standard library.</summary>
module StdlibRegistry =

    /// <summary>Gets every standard library function, a new instance of each.</summary>
    /// <returns>The standard library functions.</returns>
    [<CompiledName("GetAll")>]
    let getAll () : IReadOnlyList<IJyroFunction> = StdlibFunctions.getAll ()

    /// <summary>Looks up a standard library function by name.</summary>
    /// <remarks>Each name it knows gives the same object on every call in a process.</remarks>
    /// <param name="name">The name of the function.</param>
    /// <param name="func">The function, when the name is one of the standard library.</param>
    /// <returns>Whether the standard library has a function of that name.</returns>
    [<CompiledName("TryGetFunction")>]
    let tryGetFunction (name: string | null, [<Out; System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] func: byref<IJyroFunction>) : bool =
        match name with
        | null ->
            func <- Unchecked.defaultof<IJyroFunction>
            false
        | name -> StdlibFunctions.tryGetFunction (name, &func)

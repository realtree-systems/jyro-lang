namespace RealTree.Jyro

open System.Collections.Generic

// The standard library functions, for the compiler and for StdlibRegistry,
// the public door to them in Jyro.Api (JYRO-ADR-202 item 1: Jyro.Stdlib
// exports no public type).
module internal StdlibFunctions =

    // Every standard library function, a new instance of each, in the order
    // of the categories.
    let getAll () : IReadOnlyList<IJyroFunction> =
        List.concat [
            StringFunctions.getAll ()
            ArrayFunctions.getAll ()
            MathFunctions.getAll ()
            DateTimeFunctions.getAll ()
            UtilityFunctions.getAll ()
            SchemaFunctions.getAll ()
            QueryFunctions.getAll ()
            LambdaFunctions.getAll ()
        ]
        |> List.toArray
        :> IReadOnlyList<IJyroFunction>

    // The standard library functions by name, made once in a process, so a
    // lookup builds no function and gives one object per name, whatever was
    // looked up before (JYRO-REQ-1935). The functions hold no state, so one
    // instance serves every caller.
    let private byName =
        lazy (
            let table = Dictionary<string, IJyroFunction>(System.StringComparer.Ordinal)
            for f in getAll () do table.[f.Name] <- f
            table)

    // The function named `name`, the same object on every call in a process.
    let tryGetFunction (name: string, func: byref<IJyroFunction>) : bool =
        byName.Value.TryGetValue(name, &func)

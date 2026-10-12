namespace RealTree.Jyro.Compilation

open RealTree.Jyro

open System

// Loads the engine member table, which every compilation does before its
// first stage, so that a stale name fails every compilation and not only one
// of a script that uses the construct it serves.
//
// This module is a file of its own, after EngineMembers.fs, and must stay
// one: F# initializes every top-level value of a file in one type
// initializer, so a guard written in the table's own file would run that
// initializer, and fail with it, before its own try could catch anything.
// Here the table's initializer runs when `loaded` is forced, inside the try.
module internal EngineBinding =

    let private loaded =
        lazy (
            try EngineMembers.all.Length
            with ex ->
                match EngineBindingException.Within ex with
                | Some binding -> raise (EngineBindingException(binding.Message, ex))
                | None ->
                    match ex with
                    | :? TypeInitializationException ->
                        let rec innermost (e: exn) =
                            match e.InnerException with
                            | null -> e
                            | inner -> innermost inner
                        raise (EngineBindingException(
                                sprintf "The engine member table did not load: %s" (innermost ex).Message, ex))
                    | _ -> reraise ())

    // Raises EngineBindingException, naming the type and the member, when a
    // member of the table is missing or ambiguous; the same exception again
    // on every later call.
    let ensureLoaded () : unit = loaded.Force() |> ignore

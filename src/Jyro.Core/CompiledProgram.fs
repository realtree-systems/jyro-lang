namespace RealTree.Jyro

open System
open System.Collections.Generic

/// <summary>A compiled Jyro program ready for execution.</summary>
/// <remarks>
/// A host makes one only through compilation or a load, and runs one only
/// through <c>JyroRunner</c>: it has no public
/// constructor and no public delegate, so no host can run a delegate that
/// was never parsed, validated and linked, or run the program with an input
/// the run has not made read-only. Two programs are equal only when they are
/// the same object.
/// </remarks>
[<Sealed>]
type CompiledProgram internal
    (compiledFunction: Func<JyroValue, JyroValue, JyroExecutionContext, JyroValue>,
     requiredFunctions: IReadOnlyList<string>) =

    // The compiled delegate that executes the program: input, output and the
    // execution context in, the resulting output out.
    member internal _.CompiledFunction = compiledFunction

    /// <summary>
    /// The names of the standard library and host functions that the program
    /// calls, each once, in the order of the function table of an artifact of
    /// the program.
    /// </summary>
    /// <remarks>
    /// For a program loaded from an artifact, this is the function table of
    /// that artifact.
    /// </remarks>
    member _.RequiredFunctions : IReadOnlyList<string> = requiredFunctions

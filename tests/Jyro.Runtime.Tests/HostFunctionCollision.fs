module RealTree.Jyro.Tests.HostFunctionCollisionTests

open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Registration-collision contract for host-provided functions.
///
/// The linker resolves functions through a `Map<string, IJyroFunction>`, so
/// two functions registered under the same name would silently collapse to one
/// (last-write-wins) with no diagnostic. The compiler rejects both collision
/// shapes at compile time instead:
///   * a custom function whose name matches an included stdlib function ->
///     `HostFunctionOverridesStdlib` (there is no silent override);
///   * two custom functions registered under the same name ->
///     `DuplicateFunction`.
/// A user- or library-script `func` shadowing a builtin is a separate event
/// (`FunctionOverride`), covered elsewhere. The script's check is made in
/// Validate (JYRO-ADR-116); a library's is still the linker's until
/// JYRO-DEF-510 moves it to Validate too.
module HostFunctionCollisionTests =

    /// A no-op host function registered under an arbitrary name. Collision
    /// detection is purely registration-based, so the body never runs.
    type private HostFn(name: string) =
        inherit JyroFunctionBase(
            name,
            JyroFunctionSignature.Create(ParameterType.Any))
        override _.Effect = FunctionEffect.Pure
        override _.Execute(_: IReadOnlyList<JyroValue>, _: JyroExecutionContext) =
            JyroNull.Instance :> JyroValue

    let private host (name: string) : IJyroFunction = HostFn(name) :> IJyroFunction

    /// `ToUpper` is a standard-library string function (StringFunctions.fs).
    let private stdlibName = "ToUpper"

    [<Fact>]
    let ``custom function colliding with a stdlib name is a hard error`` () =
        let result =
            JyroCompiler()
                .WithStdlib()
                .WithSource("Output.result = 1")
                .AddFunction(host stdlibName)
                .Compile()
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.HostFunctionOverridesStdlib, codes)

    [<Fact>]
    let ``collision is detected even when the script never calls the function`` () =
        // Detection is registration-based, so a colliding registration fails
        // compilation regardless of whether the script references the name.
        // Asserted on `Compile` rather than a run: registration is a
        // compile-side concern, and since the split there is no way to reach a
        // run without a program, so the compile diagnostic *is* what a host
        // sees.
        let result =
            JyroCompiler()
                .WithStdlib()
                .WithSource("Output.result = 1")
                .AddFunction(host stdlibName)
                .Compile()
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.HostFunctionOverridesStdlib, codes)

    [<Fact>]
    let ``custom function reusing a stdlib name without UseStdlib is allowed`` () =
        // No stdlib is included, so `ToUpper` is not a builtin here and the
        // registration is a plain custom function - no collision.
        let result =
            JyroCompiler()
                .WithSource("Output.result = 1")
                .AddFunction(host stdlibName)
                .Compile()
        Assert.True(result.IsSuccess,
            sprintf "Expected compilation to succeed; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``two custom functions with the same name are a hard error`` () =
        let result =
            JyroCompiler()
                .WithSource("Output.result = 1")
                .AddFunctions([ host "myHelper"; host "myHelper" ])
                .Compile()
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.DuplicateFunction, codes)

    [<Fact>]
    let ``a stdlib collision is reported once, as the stdlib collision`` () =
        // A custom registered twice under a stdlib name is the stdlib-collision
        // event (the more actionable message), reported exactly once - not also
        // as a duplicate.
        let result =
            JyroCompiler()
                .WithStdlib()
                .WithSource("Output.result = 1")
                .AddFunctions([ host stdlibName; host stdlibName ])
                .Compile()
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Equal<MessageCode>([ MessageCode.HostFunctionOverridesStdlib ], codes)

    [<Fact>]
    let ``distinct custom function names alongside stdlib compile cleanly`` () =
        let result =
            JyroCompiler()
                .WithStdlib()
                .WithSource("Output.result = 1")
                .AddFunctions([ host "alpha"; host "beta" ])
                .Compile()
        Assert.True(result.IsSuccess,
            sprintf "Expected compilation to succeed; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

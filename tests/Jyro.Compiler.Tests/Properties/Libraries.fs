namespace RealTree.Jyro.Tests.Compiler

open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Tests for the per-script library feature: parsing/validation of library
/// scripts, effect-based linker validation, and end-to-end invocation of
/// library functions from a caller script.
module Libraries =

    /// A no-op host function we can register with an arbitrary name and
    /// effect to exercise the linker's effect-ceiling rules.
    type private Stub(name: string, effect: FunctionEffect) =
        inherit JyroFunctionBase(
            name,
            JyroFunctionSignature.Create(ParameterType.Any))
        override _.Effect = effect
        override _.Execute(_: IReadOnlyList<JyroValue>, _: JyroExecutionContext) =
            JyroNull.Instance :> JyroValue

    let private pureHost: IJyroFunction = Stub("Pure", FunctionEffect.Pure) :> IJyroFunction
    let private nondetHost: IJyroFunction = Stub("NonDet", FunctionEffect.NonDeterministic) :> IJyroFunction
    let private blockingHost: IJyroFunction = Stub("Block", FunctionEffect.Blocking) :> IJyroFunction
    let private mutatingHost: IJyroFunction = Stub("Mutate", FunctionEffect.Mutating) :> IJyroFunction

    let private parse (src: string) : Program =
        let parsed = Parser.parse src
        match parsed with
        | Produced program -> program
        | _ ->
            failwithf "Expected source to parse; got: %A"
                (parsed.Messages |> List.ofSeq |> List.map (fun m -> m.Code))

    // -- LibraryValidator: only func/union allowed at top level ----------

    [<Theory>]
    [<InlineData("var x = 5")>]
    [<InlineData("Output.x = 1")>]
    [<InlineData("if true then\n  var y = 1\nend")>]
    [<InlineData("while true do\n  break\nend")>]
    [<InlineData("for i in 1 to 10 do\n  var y = i\nend")>]
    let ``library with non-declaration top-level is rejected`` (src: string) =
        let program = parse src
        let result = LibraryValidator.validateLibrary program
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.LibraryStatementForbidden, codes)

    [<Fact>]
    let ``library containing only func and union declarations is accepted`` () =
        let src = "func F()\n  return 1\nend\nunion U\n  A\n  B\nend"
        let program = parse src
        let result = LibraryValidator.validateLibrary program
        Assert.True(result.IsSuccess,
            sprintf "Expected library to validate; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    // -- Linker: effect-ceiling resolution -------------------------------

    /// Links a caller and one library straight through `Linker.link`, after
    /// validating each alone. It skips the compilation's Validate stage, which
    /// checks the caller's and the libraries' names against each other
    /// (FunctionShadowsLibrary, a library's DuplicateFunction: 80.3, JYRO-ADR-115),
    /// so it is only for checks the linker owns, such as the effect ceiling.
    let private link
        (callerSrc: string)
        (librarySrc: string)
        (hosts: IJyroFunction list)
        : JyroResult<Linker.LinkedProgram> =
        let caller = parse callerSrc
        let library = parse librarySrc
        match Validator.validate caller with
        | Failed msgs ->
            failwithf "Caller did not validate: %A" (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
        | _ -> ()
        match LibraryValidator.validateLibrary library with
        | Failed msgs ->
            failwithf "Library did not validate: %A" (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
        | _ -> ()
        Linker.link caller hosts library.Statements FunctionEffect.Mutating FunctionEffect.NonDeterministic

    // -- Names across the caller and its libraries: the Validate stage ----
    //
    // Whether a name is declared twice across source texts is a check of the
    // compilation's Validate stage (80.3.1, 80.3.3, 160.6, JYRO-ADR-115), so
    // these go through the compiler with the libraries as their own source
    // texts, not through one pass.

    let private stageOf (m: DiagnosticMessage) = string m.Stage

    [<Fact>]
    let ``caller declaring a function with a library name is rejected at Validate`` () =
        let result =
            JyroCompiler()
                .WithSource("func Greet()\n  return \"hi\"\nend")
                .AddLibrarySource("func Greet()\n  return \"hello\"\nend")
                .Compile()
        Assert.False(result.IsSuccess)
        let shadows =
            List.ofSeq result.Messages |> List.filter (fun m -> m.Code = MessageCode.FunctionShadowsLibrary)
        Assert.NotEmpty(shadows)
        Assert.All(shadows, fun m -> Assert.Equal("Validate", stageOf m))

    [<Fact>]
    let ``two libraries declaring the same function are rejected at Validate`` () =
        let result =
            JyroCompiler()
                .WithSource("Output.v = F()")
                .AddLibrarySource("func F()\n  return 1\nend")
                .AddLibrarySource("func F()\n  return 2\nend")
                .Compile()
        Assert.False(result.IsSuccess)
        let duplicates =
            List.ofSeq result.Messages |> List.filter (fun m -> m.Code = MessageCode.DuplicateFunction)
        Assert.NotEmpty(duplicates)
        Assert.All(duplicates, fun m -> Assert.Equal("Validate", stageOf m))

    [<Fact>]
    let ``linker reports each declaration diagnostic exactly once`` () =
        // Regression: `link` used to append the pre-pass and user pass-1
        // message lists on top of the pass-2 results, which already carried
        // them as a suffix, so every declaration diagnostic reached the host
        // twice - and at non-adjacent indices once there was more than one.
        // This also pins the documented order: library declarations first,
        // then user declarations, each group in source order.
        //
        // The diagnostic used is VariantShadowsFunction, which Linker.link
        // raises in both its library and its script pass. FunctionOverride is
        // not used: the script's is raised in Validate since 750e503
        // (JYRO-ADR-116), and the library's moves there too (JYRO-DEF-510).
        let caller = parse "union S\n  Pure\n  NonDet\nend"
        let library = parse "union L\n  Block\nend"
        Assert.True((Validator.validate caller).IsSuccess)
        Assert.True((LibraryValidator.validateLibrary library).IsSuccess)
        let result =
            Linker.link caller [ pureHost; nondetHost; blockingHost ] library.Statements
                FunctionEffect.Mutating FunctionEffect.NonDeterministic
        Assert.False(result.IsSuccess)
        let messages = List.ofSeq result.Messages
        Assert.Equal(3, messages.Length)
        Assert.All(messages, fun m -> Assert.Equal(MessageCode.VariantShadowsFunction, m.Code))
        Assert.Contains("'Block'", messages.[0].Message)
        Assert.Contains("'Pure'", messages.[1].Message)
        Assert.Contains("'NonDet'", messages.[2].Message)

    [<Fact>]
    let ``library function calling a Mutating host is rejected`` () =
        let result =
            link
                ""
                "func Bad()\n  return Mutate()\nend"
                [ mutatingHost ]
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.FunctionEffectExceedsMax, codes)

    [<Fact>]
    let ``library function calling a Blocking host is rejected`` () =
        let result =
            link
                ""
                "func Bad()\n  return Block()\nend"
                [ blockingHost ]
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.FunctionEffectExceedsMax, codes)

    [<Fact>]
    let ``library function calling a Pure host is allowed`` () =
        let result =
            link
                ""
                "func Good()\n  return Pure()\nend"
                [ pureHost ]
        Assert.True(result.IsSuccess,
            sprintf "Expected link success; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``library function calling a NonDeterministic host is allowed`` () =
        let result =
            link
                ""
                "func Good()\n  return NonDet()\nend"
                [ nondetHost ]
        Assert.True(result.IsSuccess,
            sprintf "Expected link success; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``library function calling another library function is allowed`` () =
        let result =
            link
                ""
                "func A()\n  return 1\nend\nfunc B()\n  return A()\nend"
                []
        Assert.True(result.IsSuccess,
            sprintf "Expected link success; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``library function transitively reaching a Blocking host is rejected``
        ()
        =
        // A calls B; B calls Block. Inference must propagate Block's effect
        // up through B, then through A, and the library ceiling must reject
        // both call sites.
        let result =
            link
                ""
                "func B()\n  return Block()\nend\nfunc A()\n  return B()\nend"
                [ blockingHost ]
        Assert.False(result.IsSuccess)
        let codes = List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)
        Assert.Contains(MessageCode.FunctionEffectExceedsMax, codes)

    [<Fact>]
    let ``caller calling a library function is allowed`` () =
        let result =
            link
                "Output.greeting = Greet()"
                "func Greet()\n  return \"hello\"\nend"
                []
        Assert.True(result.IsSuccess,
            sprintf "Expected link success; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``user script calling a Mutating host is allowed (user ceiling is Mutating)``
        ()
        =
        let result =
            link
                "Output.x = Mutate()"
                ""
                [ mutatingHost ]
        Assert.True(result.IsSuccess,
            sprintf "Expected link success; got: %A"
                (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``user function transitively reaching a Mutating host carries Mutating effect``
        ()
        =
        // Validate that the inferred effect map propagates Mutating through
        // a user function: B calls Mutate, A calls B; A's effect should be
        // Mutating after inference.
        let caller = parse "func B()\n  return Mutate()\nend\nfunc A()\n  return B()\nend\nOutput.x = A()"
        Assert.True((Validator.validate caller).IsSuccess)
        let linked =
            Linker.link caller [ mutatingHost ] [] FunctionEffect.Mutating FunctionEffect.NonDeterministic
        Assert.True(linked.IsSuccess)
        let lp = linked.TryValue.Value
        let a = lp.UserFunctions.["A"] :> IJyroFunction
        let b = lp.UserFunctions.["B"] :> IJyroFunction
        Assert.Equal(FunctionEffect.Mutating, a.Effect)
        Assert.Equal(FunctionEffect.Mutating, b.Effect)

    // -- End-to-end: library function actually executes ------------------

    let private executeWith (callerSrc: string) (librarySrc: string) : JyroValue =
        let libraryProgram = parse librarySrc
        match LibraryValidator.validateLibrary libraryProgram with
        | Failed msgs ->
            failwithf "Library did not validate: %A" (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
        | _ -> ()
        let compiled =
            Compiler.compileFromSource callerSrc [] libraryProgram.Statements
                FunctionEffect.Mutating FunctionEffect.NonDeterministic
        match compiled with
        | Failed msgs ->
            failwithf "Compile failed: %A" (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
        | Produced program ->
            let executed =
                Compiler.execute program (JyroNull.Instance :> JyroValue) (JyroExecutionContext())
            match executed with
            | Failed msgs ->
                failwithf "Execute failed: %A" (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
            | Produced v -> v
            | _ -> failwith "execute returned no value"
        | _ -> failwith "compile returned no program"

    [<Fact>]
    let ``library function is callable from caller end-to-end`` () =
        let result =
            executeWith
                "Output.greeting = Greet(\"world\")"
                "func Greet(name)\n  return \"Hello, \" & name\nend"
        let obj = result :?> JyroObject
        let greeting = obj.["greeting"] :?> JyroString
        Assert.Equal("Hello, world", greeting.Value)

    [<Fact>]
    let ``library-to-library composition works end-to-end`` () =
        let library =
            "func Inner(x)\n  return x * 2\nend\nfunc Outer(x)\n  return Inner(x) + 1\nend"
        let result =
            executeWith
                "Output.value = Outer(5)"
                library
        let obj = result :?> JyroObject
        let value = obj.["value"] :?> JyroNumber
        Assert.Equal(11.0, value.Value)

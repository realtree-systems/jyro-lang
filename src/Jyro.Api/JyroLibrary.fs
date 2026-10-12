namespace RealTree.Jyro

open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// <summary>
/// A library script parsed and validated once, ready to be added to many
/// compilations.
///
/// A <c>JyroLibrary</c> is opaque: it holds the library's declarations in the
/// compiler's own form, which no host reads, so a library is parsed once and
/// reused without the compiler's representation becoming part of the API.
///
/// Library scripts may contain only <c>func</c> and <c>union</c> declarations. A script
/// that parses but breaks that rule fails here rather than at <c>Compile()</c>,
/// which is the point of parsing it up front. Every check that reads only the
/// library is made here; every check that reads the compilation - function,
/// union and variant names repeated in another source text, a <c>match</c> naming
/// a variant the library does not declare, and every call - is made by each
/// <c>Compile()</c> the library is added to.
/// </summary>
[<Sealed>]
type JyroLibrary internal (statements: Stmt list, holdsDeferredMatch: bool) =
    let declaredNames =
        statements
        |> List.choose (fun stmt ->
            match Ast.tryAsFuncDef stmt with
            | Some (name, _, _, _) -> Some name
            | None -> Ast.tryAsUnionDef stmt |> Option.map (fun (name, _, _) -> name))
        |> List.distinct
        |> Array.ofList

    member internal _.Statements = statements

    // Whether the library holds a `match` that `Parse` left to each
    // compilation, which then validates the library again in its scope;
    // a library that holds none has only its union and variant names
    // checked there (JYRO-ADR-140).
    member internal _.HoldsDeferredMatch = holdsDeferredMatch

    /// <summary>
    /// The number of top-level declarations the library contributes. Present
    /// so a host can assert it loaded what it meant to without the type having
    /// to hand back the declarations themselves.
    /// </summary>
    member _.DeclarationCount = List.length statements

    /// <summary>
    /// The names of the functions and unions that the library declares, each
    /// once, in the order of the source text.
    /// </summary>
    member _.DeclaredNames : System.Collections.Generic.IReadOnlyList<string> = declaredNames

    /// <summary>
    /// Parses and validates a library script.
    ///
    /// A null source is refused at the call rather than returned as a failure.
    /// The result type carries the script's faults - what did not parse, what
    /// a library may not contain - and a caller that passed nothing has not
    /// written a faulty library, it has made a mistake in its own code. That is
    /// the same line <c>JyroCompiler.WithSource</c> and <c>JyroRunner</c>'s constructor
    /// draw, and the same one that makes <c>JyroString(null)</c> reject while
    /// <c>FromObject(null)</c> normalizes.
    ///
    /// Bounded by the default compilation time limit of 5 seconds, as a compilation is: a parse that reaches it returns a
    /// failed result holding one <c>CompilationTimeLimitExceeded</c> of the stage
    /// Parse, and does not throw. <c>Parse(source, maxCompilationTime)</c> sets
    /// another limit.
    /// </summary>
    static member Parse(source: string) : JyroResult<JyroLibrary> =
        JyroLibrary.Parse(source, CompilationRun.DefaultTimeLimit)

    /// <summary>
    /// Parses and validates a library script under the compilation time
    /// limit <c>maxCompilationTime</c>, accepted and refused as
    /// <c>JyroCompiler.WithMaxCompilationTime</c> accepts and refuses one:
    /// <c>TimeSpan.MaxValue</c> is no limit; any value other than that and more
    /// than zero and at most 24 hours throws <c>ArgumentOutOfRangeException</c>
    /// naming <c>maxCompilationTime</c>. A parse that reaches the limit returns a
    /// failed result holding one <c>CompilationTimeLimitExceeded</c> of the stage
    /// Parse, and does not throw. The limit is not enforced on single-threaded
    /// browser WebAssembly.
    /// </summary>
    static member Parse(source: string, maxCompilationTime: System.TimeSpan) : JyroResult<JyroLibrary> =
        if obj.ReferenceEquals(source, null) then raise (System.ArgumentNullException(nameof source))
        JyroExecutionOptions.RequireTimeLimit(maxCompilationTime, nameof maxCompilationTime, CompilationRun.TimeLimitRange)
        // On a thread of its own with a stack of 16 MiB, as `JyroCompiler`
        // compiles: the parser and the validator recurse once per level of
        // nesting, up to the maximum of 256 (spec 100.10.1), and the host
        // thread that asks may have a small stack (JYRO-REQ-1717, 1719). The
        // whole parse is the Parse stage of a library parsed ahead (spec
        // 160.6), so a limit reached names Parse.
        CompilationRun.runStaged MessageStage.Parse (fun _ -> JyroLibrary.ParseHere source) maxCompilationTime

    static member private ParseHere(source: string) : JyroResult<JyroLibrary> =
        let parsed = Parser.parse source
        match parsed with
        // Parse's diagnostics go through the one function every result path
        // assembles a stage's diagnostics with (JYRO-ADR-126);
        // validateParsedLibrary does the same for Validate.
        | Failed msgs -> JyroResult<JyroLibrary>.Failure(Validator.inSourceOrder [ List.ofSeq msgs ])
        | Produced program ->
            let validated, holdsDeferredMatch = LibraryValidator.validateParsedLibrary program
            match validated with
            | Failed msgs -> JyroResult<JyroLibrary>.Failure(msgs)
            | Produced library ->
                JyroResult<JyroLibrary>.Success(JyroLibrary(library.Statements, holdsDeferredMatch))
            | Empty -> JyroResult<JyroLibrary>.Failure(validated.Messages)
        | Empty -> JyroResult<JyroLibrary>.Failure(parsed.Messages)

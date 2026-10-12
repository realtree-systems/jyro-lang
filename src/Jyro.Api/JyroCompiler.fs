namespace RealTree.Jyro

open RealTree.Jyro.Compilation

open System
open System.Collections.Generic

/// <summary>
/// Fluent builder that compiles a source text into a <c>CompiledProgram</c>, or
/// writes it as a <c>.jyrx</c> artifact. It compiles source only: an artifact is
/// loaded by <c>JyroArtifactLoader</c>. Configuration that governs
/// compilation lives here; configuration that governs a run lives on
/// <c>JyroRunner</c>.
///
/// Each <c>With</c> and <c>Add</c> member changes the compiler it is called on and
/// returns that compiler. A null argument throws <c>ArgumentNullException</c> at
/// the call, naming the parameter, and leaves the compiler as it was.
///
/// The stages a compilation records in the statistics given to <c>WithStats</c>:
/// <c>Compile()</c> and <c>CompileToArtifact()</c> both record Parse, Validate, Link and
/// Compile, writing the artifact being part of the Compile stage, and neither
/// records Deserialize. A load by <c>JyroArtifactLoader.Load()</c> records
/// Deserialize, Validate, Link and Compile, and no Parse.
///
/// Limits are a property of the run, not of the program. Neither a
/// <c>CompiledProgram</c> nor a <c>.jyrx</c> artifact carries a resource limit, so the
/// <c>JyroRunner</c> that executes decides them. The compilation time limit set
/// here bounds compilation and nothing else.
/// </summary>
[<Sealed>]
type JyroCompiler() =
    let mutable sourceText: string option = None
    let registration = FunctionRegistration()
    let mutable compilationTimeout: TimeSpan option = None
    let mutable statsCollector: JyroPipelineStats option = None
    // The library scripts, in the order the host added them: source text to
    // parse and validate in the compilation's own Parse and Validate stages,
    // beside the script (JYRO-ADR-115 point 3), or the declarations of a
    // `JyroLibrary` parsed up front.
    let libraries = ResizeArray<Compiler.LibraryInput>()
    // The effect maximum of library bodies: up to NonDeterministic (stdlib
    // excluding Sleep, plus Pure/NonDeterministic helpers) unless the host
    // sets another. The script's is on `registration`.
    let mutable libraryMaxEffect: FunctionEffect = FunctionEffect.NonDeterministic

    /// <summary>
    /// Sets the source text of a later <c>Compile()</c> or <c>CompileToArtifact()</c>;
    /// when called more than once, the one called last gives it. Changes this
    /// compiler and returns it. The empty string is a source text.
    /// </summary>
    member this.WithSource(source: string) =
        // Rejected at the call, not stored. A null reaching the parser
        // surfaces as a NullReferenceException from inside `Compile()`, far
        // from the member that accepted it.
        if obj.ReferenceEquals(source, null) then raise (ArgumentNullException(nameof source))
        sourceText <- Some source
        this

    /// <summary>
    /// Adds a host function to the compilation. Changes this compiler and
    /// returns it.
    ///
    /// A function with side effects weakens the sandbox in proportion to what it
    /// does: a script reaches nothing outside its input and output
    /// except through the functions a host gives it, so a function that reads a
    /// file, calls a service or changes shared state hands that power to every
    /// script that can call it. Give it the narrowest inputs it can work with,
    /// declare its true <c>Effect</c>, and cap it with <c>JyroCompiler.WithMaxEffect</c>.
    /// </summary>
    member this.AddFunction(``function``: IJyroFunction) =
        registration.Add(``function``, "function")
        this

    /// <summary>
    /// Adds host functions to the compilation, enumerating <c>functions</c> once.
    /// Changes this compiler and returns it. A sequence that holds a null
    /// throws <c>ArgumentNullException</c> and adds none.
    ///
    /// A function with side effects weakens the sandbox in proportion to what it
    /// does; give each the narrowest inputs it can work with, declare its true
    /// <c>Effect</c>, and cap them with <c>JyroCompiler.WithMaxEffect</c>.
    /// </summary>
    member this.AddFunctions(functions: IEnumerable<IJyroFunction>) =
        registration.AddRange(functions, nameof functions)
        this

    /// <summary>
    /// Adds a library script from source to the compilation, after the
    /// library scripts already added. Changes this compiler and returns it.
    /// Library scripts may only contain <c>func</c> and <c>union</c> declarations;
    /// other top-level statements are rejected. The library is parsed and
    /// validated when the script is, in the same stages: its
    /// parse errors are reported with the script's, its validation errors with
    /// the script's, and neither stops the script's own Parse or Validate.
    /// </summary>
    member this.AddLibrarySource(source: string) =
        if obj.ReferenceEquals(source, null) then raise (ArgumentNullException(nameof source))
        libraries.Add(Compiler.LibrarySource source)
        this

    /// <summary>
    /// Adds a library parsed ahead with <c>JyroLibrary.Parse</c> to the
    /// compilation, after the library scripts already added. Changes this
    /// compiler and returns it. Use this when the same library is shared
    /// across many compilers, so it is parsed once rather than once per
    /// compilation.
    /// </summary>
    member this.AddLibrary(library: JyroLibrary) =
        if obj.ReferenceEquals(library, null) then raise (ArgumentNullException(nameof library))
        libraries.Add(Compiler.LibraryDeclarations(library.Statements, library.HoldsDeferredMatch))
        this

    /// <summary>
    /// Sets the maximum observable effect tolerated by the script's calls.
    /// Changes this compiler and returns it. Defaults to <c>Mutating</c> (no
    /// restriction). Tightening to <c>Blocking</c> or lower rejects scripts that
    /// call functions with stronger effects.
    ///
    /// It is the maximum of this compilation only: an artifact records none,
    /// so the loading host applies its own (<c>JyroArtifactLoader.WithMaxEffect</c>),
    /// and a program already compiled was linked under the maximum of the
    /// compiler that made it, which is why a host serving tenants under
    /// different policies compiles per policy.
    ///
    /// An effect that is none of the four throws <c>ArgumentOutOfRangeException</c>
    /// naming <c>maxEffect</c> and leaves the maximum as it was.
    /// </summary>
    member this.WithMaxEffect(maxEffect: FunctionEffect) =
        FunctionRegistration.RequireEffect(maxEffect, nameof maxEffect)
        registration.MaxEffect <- maxEffect
        this

    /// <summary>
    /// Sets the maximum observable effect tolerated by the calls of library
    /// bodies, for scripts added with <c>AddLibrarySource</c> or <c>AddLibrary</c>.
    /// Changes this compiler and returns it. Defaults to <c>NonDeterministic</c>
    /// (stdlib excluding Sleep, plus Pure/NonDeterministic host helpers). An
    /// effect that is none of the four throws <c>ArgumentOutOfRangeException</c>
    /// naming <c>maxEffect</c> and leaves the maximum as it was.
    /// </summary>
    member this.WithLibraryMaxEffect(maxEffect: FunctionEffect) =
        FunctionRegistration.RequireEffect(maxEffect, nameof maxEffect)
        libraryMaxEffect <- maxEffect
        this

    /// <summary>
    /// Includes every standard library function in the compilation (they
    /// are excluded by default). Changes this compiler and returns it.
    /// </summary>
    member this.WithStdlib() =
        registration.IncludeStdlib()
        this

    /// <summary>
    /// Sets the compilation time limit of every later <c>Compile()</c> and
    /// <c>CompileToArtifact()</c>. Changes this compiler and returns it.
    ///
    /// With no call to this member a compilation has a limit of 5 seconds, the default execution time limit, so compiling an
    /// untrusted source is bounded by default. The limit bounds every stage
    /// before Execute - Parse, Validate, Link and Compile - and not Execute; reaching it fails the compilation with
    /// <c>CompilationTimeLimitExceeded</c> and the stage that was running.
    ///
    /// <c>time</c> is <c>TimeSpan.MaxValue</c>, which is no limit, or more than zero
    /// and at most 24 hours. Any other value - zero, a
    /// negative value, <c>Timeout.InfiniteTimeSpan</c> included, or more than 24
    /// hours - throws <c>ArgumentOutOfRangeException</c> naming <c>time</c> and leaves
    /// the limit the compiler had.
    ///
    /// On single-threaded browser WebAssembly the limit, the default
    /// included, is not enforced: a compilation runs on the thread that asks
    /// for it, and with no second thread nothing can keep time while it runs.
    /// </summary>
    member this.WithMaxCompilationTime(time: TimeSpan) =
        JyroExecutionOptions.RequireTimeLimit(time, nameof time, CompilationRun.TimeLimitRange)
        compilationTimeout <- Some time
        this

    /// <summary>
    /// Sets a pipeline stats collector, in which every later <c>Compile()</c> and
    /// <c>CompileToArtifact()</c> records the time of Parse, Validate, Link and
    /// Compile (writing the artifact is part of the Compile stage), and
    /// leaves Deserialize at zero. Changes this compiler and returns it.
    /// </summary>
    member this.WithStats(stats: JyroPipelineStats) =
        if obj.ReferenceEquals(stats, null) then raise (ArgumentNullException(nameof stats))
        statsCollector <- Some stats
        this

    // The compilation time limit: the one the host set, or 5 seconds
    // (JYRO-ADR-194). `TimeSpan.MaxValue` waits for the worker without a
    // deadline, so `CompilationTimeLimitExceeded` is never reported.
    member private _.CompilationTimeout : TimeSpan =
        defaultArg compilationTimeout CompilationRun.DefaultTimeLimit

    /// <summary>
    /// Compiles the source text, and returns the program, or the diagnostics
    /// of the compilation when it gives an error. With no source text set, the
    /// result has the one diagnostic <c>NoSourceProvided</c>, of the stage Validate.
    /// </summary>
    member this.Compile() : JyroResult<CompiledProgram> =
        match sourceText with
        | None ->
            // `NoSourceProvided` has the stage Validate (spec 160.1.12,
            // 160.7.14): a refusal of the request, made before any text
            // reaches the parser.
            JyroResult<CompiledProgram>.Failure(
                DiagnosticMessage.Of(MessageCode.NoSourceProvided))
        | Some src ->
            // Handed to the stages, each reported with the stage its code
            // has (JYRO-ADR-120). Detected inside `runStaged`, because it
            // reads each host function's `Name`, and a `Name` that throws is
            // reported through the result, not thrown (JYRO-ADR-130).
            let detect () = registration.DetectCollisions()
            let allFunctions = registration.All()
            let libs = List.ofSeq libraries
            let userCeil = registration.MaxEffect
            let libCeil = libraryMaxEffect
            let collector = statsCollector
            CompilationRun.runStaged
                MessageStage.Parse
                (fun stage -> Compiler.compileFromSourcesStaged libs src allFunctions userCeil libCeil (detect ()) collector stage)
                this.CompilationTimeout

    /// <summary>
    /// Compiles and returns the program, throwing a <c>JyroException</c> that
    /// holds the diagnostics when the compilation fails. The counterpart to
    /// <c>JyroRunner.ExecuteOrThrow</c>.
    /// </summary>
    member this.CompileOrThrow() : CompiledProgram =
        this.Compile().GetValueOrThrow()

    /// <summary>
    /// Compiles the source text and writes it as a <c>.jyrx</c> artifact,
    /// which <c>JyroArtifactLoader</c> loads. Records Parse, Validate, Link
    /// and Compile, writing the artifact being part of the Compile stage. An
    /// artifact holds no library scripts, so a compilation with one is refused
    /// with <c>ArtifactCannotEncodeLibraries</c> once its earlier stages have run
    /// clean.
    /// </summary>
    member this.CompileToArtifact() : JyroResult<byte[]> =
        match sourceText with
        | None ->
            JyroResult<byte[]>.Failure(
                DiagnosticMessage.Of(MessageCode.NoSourceProvided))
        | Some src ->
            // Each configuration diagnostic is reported with the stage its
            // code has, once every earlier stage has run clean on the
            // libraries and the script (JYRO-ADR-120, JYRO-DEF-524):
            // `ArtifactCannotEncodeLibraries` is Compile, so a script with
            // errors gets its errors first and the refusal once it is clean.
            let libraryRefusal =
                if libraries.Count > 0 then [ DiagnosticMessage.Of(MessageCode.ArtifactCannotEncodeLibraries) ] else []
            let config () = registration.DetectCollisions() @ libraryRefusal
            let allFunctions = registration.All()
            let userCeil = registration.MaxEffect
            let libCeil = libraryMaxEffect
            let libs = List.ofSeq libraries
            let collector = statsCollector
            CompilationRun.runStaged
                MessageStage.Parse
                (fun stage -> Compiler.compileToJyrxFromSourcesStaged libs src allFunctions userCeil libCeil (config ()) collector stage)
                this.CompilationTimeout

/// <summary>
/// Fluent builder that loads a <c>.jyrx</c> artifact written by
/// <c>JyroCompiler.CompileToArtifact()</c> into a <c>CompiledProgram</c>. It has no member that adds a library script: an artifact
/// holds none.
///
/// Each <c>With</c> and <c>Add</c> member changes the loader it is called on and
/// returns that loader, and registers functions, sets the maximum effect, the
/// compilation time limit and the statistics with the outcomes and
/// diagnostics that the member of the same name gives on <c>JyroCompiler</c>. A
/// null argument throws <c>ArgumentNullException</c> at the call, naming the
/// parameter, and leaves the loader as it was.
///
/// A load runs Deserialize, Validate, Link and Compile, in that order, and records the time of each in the statistics given to
/// <c>WithStats</c>; it records no Parse.
/// </summary>
[<Sealed>]
type JyroArtifactLoader(artifact: byte[]) =
    do if obj.ReferenceEquals(artifact, null) then
        raise (ArgumentNullException(nameof artifact))

    let registration = FunctionRegistration()
    let mutable compilationTimeout: TimeSpan option = None
    let mutable statsCollector: JyroPipelineStats option = None

    /// <summary>
    /// Includes every standard library function, for the artifact's function
    /// table to resolve against. Changes this loader and returns it.
    /// </summary>
    member this.WithStdlib() =
        registration.IncludeStdlib()
        this

    /// <summary>
    /// Adds a host function, for the artifact's function table to resolve
    /// against. Changes this loader and returns it.
    /// </summary>
    member this.AddFunction(``function``: IJyroFunction) =
        registration.Add(``function``, "function")
        this

    /// <summary>
    /// Adds host functions, enumerating <c>functions</c> once. Changes this loader
    /// and returns it. A sequence that holds a null throws
    /// <c>ArgumentNullException</c> and adds none.
    /// </summary>
    member this.AddFunctions(functions: IEnumerable<IJyroFunction>) =
        registration.AddRange(functions, nameof functions)
        this

    /// <summary>
    /// Sets the maximum observable effect of the functions of the artifact's
    /// table. Changes this loader and returns it. Defaults to
    /// <c>Mutating</c>. An artifact records no maximum, so the loading host's
    /// applies, and a function above it refuses the load with
    /// <c>ArtifactEffectExceedsMax</c>. An effect that is none of the four throws
    /// <c>ArgumentOutOfRangeException</c> naming <c>maxEffect</c> and leaves the maximum
    /// as it was.
    /// </summary>
    member this.WithMaxEffect(maxEffect: FunctionEffect) =
        FunctionRegistration.RequireEffect(maxEffect, nameof maxEffect)
        registration.MaxEffect <- maxEffect
        this

    /// <summary>
    /// Sets the compilation time limit of every later <c>Load()</c>, as
    /// <c>JyroCompiler.WithMaxCompilationTime</c> does: 5 seconds when it is not
    /// called, <c>TimeSpan.MaxValue</c> for no limit, or more than zero and at
    /// most 24 hours; any other value throws <c>ArgumentOutOfRangeException</c>
    /// naming <c>time</c> and leaves the limit as it was. Changes this loader and
    /// returns it. The limit bounds Deserialize, Validate, Link and Compile,
    /// and is not enforced on single-threaded browser WebAssembly.
    /// </summary>
    member this.WithMaxCompilationTime(time: TimeSpan) =
        JyroExecutionOptions.RequireTimeLimit(time, nameof time, CompilationRun.TimeLimitRange)
        compilationTimeout <- Some time
        this

    /// <summary>
    /// Sets a pipeline stats collector, in which every later <c>Load()</c> records
    /// the time of Deserialize, Validate, Link and Compile, and leaves Parse
    /// at zero. Changes this loader and returns it.
    /// </summary>
    member this.WithStats(stats: JyroPipelineStats) =
        if obj.ReferenceEquals(stats, null) then raise (ArgumentNullException(nameof stats))
        statsCollector <- Some stats
        this

    /// <summary>
    /// Loads the artifact, with the functions, the maximum effect, the compilation
    /// time limit and the statistics that were set. A refusal is a diagnostic
    /// of the result, never an exception.
    /// </summary>
    member _.Load() : JyroResult<CompiledProgram> =
        let detect () = registration.DetectCollisions()
        let allFunctions = registration.All()
        let userCeil = registration.MaxEffect
        let collector = statsCollector
        // Loading an artifact walks its expressions recursively, once per
        // nesting level up to the 256 a reader must accept (spec 150.9.5),
        // so it runs on the larger stack; and it is compilation, so the time
        // limit bounds it as it bounds the source path (spec 100.9.1). Each
        // stage is published as it starts (JYRO-ADR-100), so a limit reached
        // names it (spec 100.9.3).
        CompilationRun.runStaged
            MessageStage.Deserialize
            (fun stage -> Compiler.compileFromJyrxConfiguredStaged artifact allFunctions userCeil (detect ()) collector stage)
            (defaultArg compilationTimeout CompilationRun.DefaultTimeLimit)

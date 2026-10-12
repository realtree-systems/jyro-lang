namespace RealTree.Jyro

open System
open System.Diagnostics

/// <summary>
/// Read-only telemetry of one pipeline: the time of each stage
/// and the resource counters of the run. Create one, pass it to
/// <c>JyroCompiler.WithStats</c>, <c>JyroArtifactLoader.WithStats</c> or
/// <c>JyroRunner.WithStats</c> (the same instance can go to more than one to get
/// the whole pipeline in one place), then read it. Only the pipeline writes
/// it.
///
/// The stages each path records:
///   * a compilation from source, by <c>JyroCompiler.Compile()</c> or
///     <c>JyroCompiler.CompileToArtifact()</c>, records Parse, Validate, Link and
///     Compile, and no Deserialize; writing the artifact is part of its
///     Compile stage;
///   * a load by <c>JyroArtifactLoader.Load()</c> records Deserialize, Validate,
///     Link and Compile, and no Parse;
///   * a run by <c>JyroRunner.Execute()</c> records Execute and the counters.
/// A stage that a path did not run reads as zero.
/// </summary>
[<Sealed>]
type JyroPipelineStats() =
    let mutable parse = TimeSpan.Zero
    let mutable validate = TimeSpan.Zero
    let mutable link = TimeSpan.Zero
    let mutable compile = TimeSpan.Zero
    let mutable execute = TimeSpan.Zero
    let mutable deserialize = TimeSpan.Zero
    // 64-bit, following the limiter's counters. They wrapped as `int` past
    // Int32.MaxValue, and these are where a host reads them.
    let mutable statementCount = 0L
    let mutable loopIterations = 0L
    let mutable peakCallDepth = 0
    let mutable limiterElapsed = TimeSpan.Zero

    /// <summary>
    /// Time spent in the Parse stage (source to program). Zero for a load.
    /// </summary>
    member _.ParseTime with get() = parse and internal set(v) = parse <- v

    /// <summary>
    /// Time spent in the Validate stage, on a compilation from source and on
    /// a load alike.
    /// </summary>
    member _.ValidateTime with get() = validate and internal set(v) = validate <- v

    /// <summary>
    /// Time spent in the Link stage. On a load this covers resolving the
    /// artifact's function table and applying the loading host's maximum
    /// effect.
    /// </summary>
    member _.LinkTime with get() = link and internal set(v) = link <- v

    /// <summary>
    /// Time spent in the Compile stage: building the executable form, and,
    /// for <c>CompileToArtifact()</c>, writing the artifact.
    /// </summary>
    member _.CompileTime with get() = compile and internal set(v) = compile <- v

    /// <summary>
    /// Time spent in the Execute stage (running the compiled program).
    /// </summary>
    member _.ExecuteTime with get() = execute and internal set(v) = execute <- v

    /// <summary>
    /// Time spent in the Deserialize stage (decoding an artifact). Zero for a
    /// compilation from source.
    /// </summary>
    member _.DeserializeTime with get() = deserialize and internal set(v) = deserialize <- v

    /// <summary>
    /// Number of statements the run executed.
    /// </summary>
    member _.StatementCount with get() = statementCount and internal set(v) = statementCount <- v

    /// <summary>
    /// Loop iterations of the run, across all its loops.
    /// </summary>
    member _.LoopIterations with get() = loopIterations and internal set(v) = loopIterations <- v

    /// <summary>
    /// The greatest call depth the run reached: what
    /// <c>JyroExecutionContext.PeakCallDepth</c> reads at the end of the run.
    /// </summary>
    member _.PeakCallDepth with get() = peakCallDepth and internal set(v) = peakCallDepth <- v

    /// <summary>
    /// The time the resource limiter measured for the run, from its start to
    /// the end of the run. Distinct from <c>ExecuteTime</c>, which is the time of
    /// the stage.
    /// </summary>
    member _.LimiterElapsedTime with get() = limiterElapsed and internal set(v) = limiterElapsed <- v

    /// <summary>
    /// The sum of the times of the six stages.
    /// </summary>
    member _.TotalTime =
        parse + validate + link + compile + execute + deserialize

    // Times a function and records the elapsed time through the given setter.
    static member internal TimeStage(setter: Action<TimeSpan>, f: Func<'T>) : 'T =
        let sw = Stopwatch.StartNew()
        let result = f.Invoke()
        sw.Stop()
        setter.Invoke(sw.Elapsed)
        result

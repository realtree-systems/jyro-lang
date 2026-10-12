namespace RealTree.Jyro

open System

// The regex timeout of each `JyroExecutionOptions`, held beside the record
// by reference, so that the record keeps its four-limit constructor
// (JYRO-REQ-1908) while the regex timeout is a settable property of it
// (JYRO-REQ-1906). It lives exactly as long as its options.
type internal RegexTimes private () =
    static let table = System.Runtime.CompilerServices.ConditionalWeakTable<obj, obj>()

    // The regex timeout of `options`, or zero, not set, when none was set.
    static member Get(options: obj) : TimeSpan =
        match table.TryGetValue(options) with
        | true, time -> unbox<TimeSpan> time
        | _ -> TimeSpan.Zero

    // Sets the regex timeout of `options`.
    static member Set(options: obj, time: TimeSpan) : unit =
        table.AddOrUpdate(options, box time)

/// <summary>
/// Configuration for the resource limits of a run, and its regex timeout.
///
/// It has a parameterless constructor and settable properties, so a host can
/// bind it from configuration with
/// <c>services.Configure&lt;JyroExecutionOptions&gt;()</c> or a JSON serializer, and a
/// constructor of the four limits for code. A
/// value left at zero is one the host did not set: a count limit of 0 takes
/// its default, and a <c>MaxRegexTime</c> of zero the
/// default regex timeout of 1 second. A <c>MaxExecutionTime</c>
/// of zero is refused where it is applied.
///
/// <c>MaxRegexTime</c> is a property, not a field of the record: an F# copy
/// expression <c>{ options with ... }</c> makes options whose regex timeout is not
/// set. A runner keeps a copy of the options it is given, so a change made to
/// them afterwards does not reach it unchecked.
///
/// It implements no structural equality or comparison interface and has no
/// ordering; its <c>ToString</c> is one line, and an F# copy expression works on
/// it. Two options are <c>Equals</c> when their four limits are equal.
/// </summary>
[<CLIMutable; CustomEquality; NoComparison>]
type JyroExecutionOptions =
    { /// <summary>
      /// Maximum execution time (default: 5 seconds). <c>TimeSpan.MaxValue</c> is
      /// no limit; any other value is more than zero and at most 24 hours.
      /// </summary>
      MaxExecutionTime: TimeSpan
      /// <summary>
      /// Maximum statement count (default: 10,000). 0 is not set; a negative
      /// value is refused.
      /// </summary>
      MaxStatements: int
      /// <summary>
      /// Maximum cumulative loop iterations (default: 1,000). 0 is not set; a
      /// negative value is refused.
      /// </summary>
      MaxLoopIterations: int
      /// <summary>
      /// Maximum call stack depth (default: 64). 0 is not set; a negative
      /// value is refused; a value above <c>MaxSafeCallDepth</c> is reduced to it -
      /// the CLR stack can't safely accommodate more frames than that.
      /// </summary>
      MaxCallDepth: int }

    /// <summary>
    /// Whether <c>other</c> has the same four limits. A null <c>other</c> is not equal.
    /// </summary>
    member this.Equals(other: JyroExecutionOptions | null) : bool =
        match other with
        | null -> false
        | other ->
            this.MaxExecutionTime = other.MaxExecutionTime
            && this.MaxStatements = other.MaxStatements
            && this.MaxLoopIterations = other.MaxLoopIterations
            && this.MaxCallDepth = other.MaxCallDepth

    /// <summary>
    /// Whether <c>other</c> is a <c>JyroExecutionOptions</c> equal to this one.
    /// </summary>
    override this.Equals(other: obj | null) : bool =
        match other with
        | :? JyroExecutionOptions as options -> this.Equals(options)
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.MaxExecutionTime, this.MaxStatements, this.MaxLoopIterations, this.MaxCallDepth)

    /// <summary>
    /// One line naming each limit and its value.
    /// </summary>
    override this.ToString() : string =
        String.Format(
            Globalization.CultureInfo.InvariantCulture,
            "MaxExecutionTime {0}, MaxStatements {1}, MaxLoopIterations {2}, MaxCallDepth {3}, MaxRegexTime {4}",
            this.MaxExecutionTime, this.MaxStatements, this.MaxLoopIterations, this.MaxCallDepth, this.MaxRegexTime)

    interface IEquatable<JyroExecutionOptions> with
        member this.Equals(other: JyroExecutionOptions | null) = this.Equals(other)

    /// <summary>
    /// The regex timeout of the run: how long one evaluation
    /// of a regular expression function may take. <c>TimeSpan.Zero</c> is not set,
    /// and leaves the timeout the runner had, 1 second unless
    /// <c>JyroRunner.WithMaxRegexTime</c> set another; any other value is
    /// more than zero and at most 24 hours.
    /// </summary>
    member this.MaxRegexTime
        with get () : TimeSpan = RegexTimes.Get(this)
        and set (time: TimeSpan) = RegexTimes.Set(this, time)

    // A copy of these options, the regex timeout included, which a later
    // change to these does not reach.
    member internal this.Copy() : JyroExecutionOptions =
        let copy = { this with MaxExecutionTime = this.MaxExecutionTime }
        RegexTimes.Set(copy, this.MaxRegexTime)
        copy

    // These options with `time` as their regex timeout.
    static member internal WithRegexTime(options: JyroExecutionOptions, time: TimeSpan) : JyroExecutionOptions =
        RegexTimes.Set(options, time)
        options

    /// <summary>
    /// Hard ceiling on <c>MaxCallDepth</c>. Hosts that configure higher are
    /// clamped to this value - the sandbox guarantee depends on it.
    ///
    /// The stack it assumes is 1 MB: execution runs on the host's own
    /// thread, and 1 MB is what a Windows thread-pool thread or a web
    /// request thread gets. At this depth, on a thread of that size, the worst shapes
    /// the language allows must finish without overflowing the stack: a
    /// recursive call inside nested blocks, the limit tripping, a runtime
    /// error, <c>fail</c> and <c>exit</c> raised at the deepest call. Its cost model is stack per Jyro call, with nothing more
    /// paid on the way out when an exception unwinds: compiled code has no
    /// catch handler that throws or rethrows, so an exception leaves in one
    /// dispatch whatever the call depth and statement nesting. A host thread
    /// smaller than 1 MB needs a lower <c>MaxCallDepth</c>.
    /// </summary>
    static member MaxSafeCallDepth = 96

    // The regex timeout of a run that sets none (spec 90.10.4): 1 second.
    static member internal DefaultRegexTime = TimeSpan.FromSeconds(1.0)

    /// <summary>
    /// The default execution options: a 5 second time limit, 10,000
    /// statements, 1,000 loop iterations, a call depth of 64, and the regex
    /// timeout of 1 second. A new instance on each read, so a
    /// change a host makes to one does not reach a later read.
    /// </summary>
    static member Default : JyroExecutionOptions =
        JyroExecutionOptions.WithRegexTime(
            { MaxExecutionTime = TimeSpan.FromSeconds(5.0)
              MaxStatements = 10_000
              MaxLoopIterations = 1_000
              MaxCallDepth = 64 },
            JyroExecutionOptions.DefaultRegexTime)

    /// <summary>
    /// Opts out of time / statement / loop limits. It keeps the call depth
    /// limited, at the safe maximum <c>MaxSafeCallDepth</c>:
    /// removing the call-depth cap would let a script stack-overflow the host
    /// and escape the sandbox entirely. A new instance on each read, so a
    /// change a host makes to one does not reach a later read.
    /// </summary>
    static member Unlimited : JyroExecutionOptions =
        { MaxExecutionTime = TimeSpan.MaxValue
          MaxStatements = Int32.MaxValue
          MaxLoopIterations = Int32.MaxValue
          MaxCallDepth = JyroExecutionOptions.MaxSafeCallDepth }

    // `options` with every count limit the host did not set given its 100.1
    // default (JYRO-REQ-1405, JYRO-ADR-181). A count limit of 0 -
    // `MaxStatements`, `MaxLoopIterations` or `MaxCallDepth` - is one the
    // host did not set: the parameterless constructor zero-fills, so an
    // object initializer or a configuration section naming only some of the
    // limits leaves the others at 0. Without this they ran as limits of 0,
    // and every script then failed its first statement with
    // `StatementLimitExceeded`.
    //
    // `MaxExecutionTime` is not covered and is returned as given: a time
    // limit of `TimeSpan.Zero` stays refused where a time limit is checked
    // (JYRO-REQ-1667, 1669). Nor is `MaxRegexTime`, whose zero the runner
    // reads as not set.
    static member internal WithDefaultsForUnset(options: JyroExecutionOptions) : JyroExecutionOptions =
        let d = JyroExecutionOptions.Default
        JyroExecutionOptions.WithRegexTime(
            { MaxExecutionTime = options.MaxExecutionTime
              MaxStatements = if options.MaxStatements = 0 then d.MaxStatements else options.MaxStatements
              MaxLoopIterations = if options.MaxLoopIterations = 0 then d.MaxLoopIterations else options.MaxLoopIterations
              MaxCallDepth = if options.MaxCallDepth = 0 then d.MaxCallDepth else options.MaxCallDepth },
            options.MaxRegexTime)

    // The longest compilation or execution time limit a host can set, other
    // than no limit (spec 100.1, 100.9, ADR-102): 24 hours, the range the
    // regex timeout already has.
    static member internal MaxTimeLimit = TimeSpan.FromHours(24.0)

    // Whether `time` is a compilation or execution time limit a host can set:
    // `TimeSpan.MaxValue`, which is no limit, or more than zero and at most
    // 24 hours (spec 100.1, 100.9, ADR-102).
    //
    // Every accepted value is in the domain of the platform waits it is
    // handed to - `Thread.Join` for compilation (at most `Int32.MaxValue`
    // milliseconds) and the limiter's execution deadline, which also falls
    // back to `CancellationTokenSource.CancelAfter` on WebAssembly - so no
    // compile or run fails with an internal error for its limit, and
    // the milliseconds that `CompilationTimeLimitExceeded` and
    // `ExecutionTimeLimitExceeded` quote fit an `int`.
    static member internal IsAcceptedTimeLimit(time: TimeSpan) : bool =
        time = TimeSpan.MaxValue
        || (time > TimeSpan.Zero && time <= JyroExecutionOptions.MaxTimeLimit)

    // Whether `time` is a regex timeout a host can set: more than zero and
    // at most 24 hours (spec 90.10.3, JYRO-REQ-1905). There is no "no
    // limit": a regular expression a script supplies is one of the few
    // things in a run that can take unbounded time without reaching a
    // checkpoint.
    static member internal IsAcceptedRegexTime(time: TimeSpan) : bool =
        time > TimeSpan.Zero && time <= JyroExecutionOptions.MaxTimeLimit

    // Throws `ArgumentOutOfRangeException`, naming `paramName`, when `time`
    // is not a time limit a host can set (see `IsAcceptedTimeLimit`).
    static member internal RequireTimeLimit(time: TimeSpan, paramName: string, message: string) : unit =
        if not (JyroExecutionOptions.IsAcceptedTimeLimit time) then
            raise (ArgumentOutOfRangeException(paramName, box time, message))

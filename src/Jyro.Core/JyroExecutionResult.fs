namespace RealTree.Jyro

open System.Collections.Generic

/// <summary>
/// The result of a run: a <c>JyroResult&lt;JyroValue&gt;</c> whose value is <c>Output</c>,
/// with the exit message, which only the result of a run has. <c>JyroRunner.Execute()</c> and the members of <c>JyroScript</c>
/// return it.
/// </summary>
[<Sealed>]
type JyroExecutionResult internal
    (value: JyroValue, hasValue: bool, messages: IReadOnlyList<DiagnosticMessage>, isSuccess: bool, exitMessage: string | null) =
    inherit JyroResult<JyroValue>(value, hasValue, messages, isSuccess)

    /// <summary>
    /// The exit message: the text of the value of
    /// the <c>exit</c> that ended the run, or <c>null</c> in every other case - an
    /// <c>exit</c> with no value, the end of the last statement, <c>fail</c> and an
    /// error. So an <c>exit ""</c> gives the empty string, not <c>null</c>.
    ///
    /// Not a diagnostic: an <c>exit</c> with a value is a clean end, and a message
    /// in <c>Messages</c> would be read as something to report.
    /// </summary>
    member _.ExitMessage : string | null = exitMessage

    // `result`, a run's, with `exitMessage` as its exit message.
    static member internal Of(result: JyroResult<JyroValue>, exitMessage: string | null) : JyroExecutionResult =
        JyroExecutionResult(result.HeldValue, result.HasValue, result.Messages, result.IsSuccess, exitMessage)

    // `result`, a run's that ended without an exit message.
    static member internal Of(result: JyroResult<JyroValue>) : JyroExecutionResult =
        JyroExecutionResult.Of(result, null)

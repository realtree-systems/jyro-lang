/// Helpers local to the chapter 40-60 suite, built on Jyro.Spec.Common.
module RealTree.Jyro.Spec.Tests.P40_60.Support

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// JSON text normalized through the implementation's own serializer, so that
/// comparisons do not depend on white space.
let norm (json: string) : string = Values.toJson (Values.json json)

/// The value of `Output.r` after a successful run with the defaults.
let rOf (source: string) : JyroValue = Script.outputOf source |> Values.prop "r"

/// `Output.r` as JSON text.
let rJson (source: string) : string = rOf source |> Values.toJson

/// Assert that the run succeeds and `Output.r` equals the JSON value `expected`.
let assertR (expected: string) (source: string) : unit = Assert.Equal(norm expected, rJson source)

/// Assert that the run succeeds and `Output.r` is exactly the string `expected`.
let assertRStr (expected: string) (source: string) : unit = Assert.Equal(expected, Values.asString (rOf source))

/// Assert that the run succeeds and `Output.r` is the boolean `expected`.
let assertRBool (expected: bool) (source: string) : unit = Assert.Equal(expected, Values.asBool (rOf source))

/// Assert that the run succeeds and the whole `Output` equals the JSON value `expected`.
let assertOut (expected: string) (source: string) : unit =
    Assert.Equal(norm expected, Values.toJson (Script.outputOf source))

/// Assert that the run with `Input` given as JSON succeeds and `Output` equals `expected`.
let assertOutWith (inputJson: string) (expected: string) (source: string) : unit =
    Assert.Equal(norm expected, Values.toJson (Script.output (Script.runJson inputJson source)))

/// Assert that `source` compiles (stdlib on) with no Error diagnostic.
let compiles (source: string) : unit =
    let r = Script.compile source
    if not r.IsSuccess then
        Values.fail (sprintf "Expected the script to compile. Diagnostics:\n%s\nSource:\n%s" (Script.describe r) source)

/// Assert that `source` compiles with no diagnostic of any severity.
let compilesClean (source: string) : unit =
    let r = Script.compile source
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "Expected a clean compile. Diagnostics:\n%s\nSource:\n%s" (Script.describe r) source)

/// Assert that compiling `source` fails with at least one Error diagnostic.
let compileFails (source: string) : unit =
    let r = Script.compile source
    if r.IsSuccess then Values.fail (sprintf "Expected the compile to fail, but it succeeded. Source:\n%s" source)
    if List.isEmpty (Script.errorCodes r) then
        Values.fail (sprintf "The compile failed with no Error diagnostic. Source:\n%s" source)

/// Assert that compiling `source` fails and an Error diagnostic has `code`.
let rejects (code: MessageCode) (source: string) : unit =
    let r = Script.compile source
    let codes = Script.errorCodes r
    if r.IsSuccess || not (List.contains code codes) then
        Values.fail (sprintf "Expected the compile to fail with %A (%d). IsSuccess=%b. Diagnostics:\n%s\nSource:\n%s"
                        code (int code) r.IsSuccess (Script.describe r) source)

/// As `rejects`, with the compile under `options`.
let rejectsWith (options: RunOptions) (code: MessageCode) (source: string) : unit =
    let r = Script.compileWith options source
    let codes = Script.errorCodes r
    if r.IsSuccess || not (List.contains code codes) then
        Values.fail (sprintf "Expected the compile to fail with %A (%d). IsSuccess=%b. Diagnostics:\n%s\nSource:\n%s"
                        code (int code) r.IsSuccess (Script.describe r) source)

/// Assert that compiling `source` fails, with `code`, and with no diagnostic of
/// the stage Parse. The stage is the diagnostic's own (160.1.11, 160.1.12), read
/// through `Pending.stageOf`, not a range of code numbers, which the book does not
/// give (d-559). A build without DiagnosticMessage.Stage reports FLOORS-BLOCKED 160.7.5.
/// `owner` is the requirement that gives the code: when the code is not raised, the stage
/// cannot be read, and a test of the stage (160.1.12) is blocked by `owner`, not failed.
let rejectsNotAtParse (owner: string) (code: MessageCode) (source: string) : unit =
    let r = Script.compile source
    if r.IsSuccess || not (List.contains code (Script.errorCodes r)) then
        Pending.requires owner
            (sprintf "Expected the compile to fail with %A (%d). IsSuccess=%b. Diagnostics:\n%s\nSource:\n%s"
                code (int code) r.IsSuccess (Script.describe r) source)
    let parse = [ for m in r.Messages do if Pending.stageOf m = "Parse" then yield m.Code ]
    if not (List.isEmpty parse) then
        Values.fail (sprintf "Expected no parse diagnostic, got %A. Diagnostics:\n%s\nSource:\n%s" parse (Script.describe r) source)

/// Assert that compiling `source` gives no Error diagnostic of `code` (it may still
/// fail for another reason only if `mustCompile` is false).
let notRejectedWith (code: MessageCode) (source: string) : unit =
    let r = Script.compile source
    if List.contains code (Script.codes r) then
        Values.fail (sprintf "Expected no %A diagnostic. Diagnostics:\n%s\nSource:\n%s" code (Script.describe r) source)

/// Assert that the run fails with exactly one Error, of `code`.
let runtimeError (code: MessageCode) (source: string) : unit =
    let r = Script.run source
    try Script.expectError code r
    with :? Xunit.Sdk.XunitException as e -> Values.fail (sprintf "%s\nSource:\n%s" e.Message source)

/// Assert that the run with JSON `Input` fails with exactly one Error, of `code`.
let runtimeErrorWith (inputJson: string) (code: MessageCode) (source: string) : unit =
    let r = Script.runJson inputJson source
    try Script.expectError code r
    with :? Xunit.Sdk.XunitException as e -> Values.fail (sprintf "%s\nSource:\n%s" e.Message source)

/// The final `Output` a failed run carries (130.2.2), as JSON.
let failedOutputJson (r: JyroResult<JyroValue>) : string =
    match Script.value r with
    | Some v -> Values.toJson v
    | None -> Values.fail (sprintf "The failed result carries no value. Diagnostics:\n%s" (Script.describe r))

/// A script function that records a tag in `o.log` and returns `v`. Pass `Output`
/// as `o`: `P(Output, "a", 1)`.
let probe = "func P(o, t, v)\n  o.log = Append(o.log ?? [], t)\n  return v\nend\n"

/// `Output.log` of a successful run, as JSON.
let logOf (source: string) : string = Script.outputOf source |> Values.prop "log" |> Values.toJson

/// Assert that `Output.log` equals the JSON array `expected`.
let assertLog (expected: string) (source: string) : unit = Assert.Equal(norm expected, logOf source)

/// Assert that compiling `source` fails with an Error diagnostic of stage
/// Parse (160.1.11, through `Pending.stageOf`).
let parseError (source: string) =
    let r = Script.compile source
    if r.IsSuccess then Values.fail (sprintf "Expected a parse error, but the compile succeeded. Source:\n%s" source)
    let stages =
        r.Messages
        |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error)
        |> Seq.map (fun m -> m.Code, Pending.stageOf m)
        |> List.ofSeq
    if not (stages |> List.exists (fun (_, st) -> st = "Parse")) then
        Values.fail (sprintf "Expected an Error of stage Parse; the errors have stages %A. Diagnostics:\n%s\nSource:\n%s"
                        stages (Script.describe r) source)

// -- Every limit set explicitly (FLOORS 4.1.11) ------------------------------

/// The four execution limits of 100.1.1, each set explicitly and far above
/// what a test needs, so no run depends on an implementation default.
let allLimits (o: JyroExecutionOptions) : JyroExecutionOptions =
    { o with
        MaxExecutionTime = System.TimeSpan.FromSeconds 30.
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000
        MaxCallDepth = 64 }

/// `Script.defaults` with the four execution limits and a generous
/// compilation time limit (100.9.1) set explicitly.
let limited : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits allLimits)
        CompilationTimeout = Some (System.TimeSpan.FromMinutes 10.) }

/// `limited`, with the regex timeout (160.1.10) set explicitly as well.
let limitedRegex : RunOptions =
    { limited with ConfigureRunner = fun r -> Pending.withRegexTimeout r (System.TimeSpan.FromSeconds 5.) }

/// The whole `Output` of a successful run under `limited`.
let outputL (source: string) : JyroValue = Script.output (Script.runWith limited source)

/// `Output.r` of a successful run under `limited`.
let rOfL (source: string) : JyroValue = outputL source |> Values.prop "r"

/// Assert the run under `limited` succeeds and `Output.r` equals the JSON value `expected`.
let assertRL (expected: string) (source: string) : unit =
    Assert.Equal(norm expected, Values.toJson (rOfL source))

/// Assert the run under `limited` succeeds and `Output.r` is exactly the string `expected`.
let assertRStrL (expected: string) (source: string) : unit =
    Assert.Equal(expected, Values.asString (rOfL source))

/// Assert the run under `limited` succeeds and the whole `Output` equals `expected`.
let assertOutL (expected: string) (source: string) : unit =
    Assert.Equal(norm expected, Values.toJson (outputL source))

/// Assert `r` has at least one Error and every Error has the stage `stage`
/// (160.1.12, read through `Pending.stageOf`).
let assertAllStages (stage: string) (r: JyroResult<'T>) : unit =
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    if List.isEmpty errors then
        Values.fail (sprintf "Expected Error diagnostics of stage %s, got none. Diagnostics:\n%s" stage (Script.describe r))
    let stages = errors |> List.map (fun m -> m.Code, Pending.stageOf m)
    if stages |> List.exists (fun (_, s) -> s <> stage) then
        Values.fail (sprintf "Expected every error to have the stage %s; the errors have %A. Diagnostics:\n%s"
                        stage stages (Script.describe r))

/// Run `source` under `options` with a host function `Mark()` called as the
/// very first statement, so a test can see whether any statement ran.
let runMarked (options: RunOptions) (source: string) : JyroResult<JyroValue> * RecordingFunction =
    let mark = Host.recorder "Mark"
    let r = Script.runWith { options with Functions = options.Functions @ [ mark.AsFunction ] } ("Mark()\n" + source)
    r, mark

/// Run under `limited` and assert it was rejected before any statement ran:
/// not IsSuccess, at least one Error, and `Mark()` never called.
let rejectedBeforeRun (source: string) : JyroResult<JyroValue> =
    let r, mark = runMarked limited source
    if r.IsSuccess || List.isEmpty (Script.errorCodes r) then
        Values.fail (sprintf "Expected the script to be rejected. Diagnostics:\n%s\nSource:\n%s" (Script.describe r) source)
    if mark.CallCount <> 0 then
        Values.fail (sprintf "The script was rejected only after it started running. Diagnostics:\n%s\nSource:\n%s" (Script.describe r) source)
    r

/// A parse error: rejected before anything runs, every Error of stage Parse (130.3.1).
let parseErrorL (source: string) : JyroResult<JyroValue> =
    let r = rejectedBeforeRun source
    assertAllStages "Parse" r
    r

/// Rejected before anything runs, with an Error of `code`, every Error of
/// `code`, and every Error of stage `stage`.
let rejectedAt (code: MessageCode) (stage: string) (source: string) : JyroResult<JyroValue> =
    let r = rejectedBeforeRun source
    let codes = Script.errorCodes r
    if codes |> List.exists (fun c -> c <> code) then
        Values.fail (sprintf "Expected every error to be %A (%d). Diagnostics:\n%s\nSource:\n%s" code (int code) (Script.describe r) source)
    assertAllStages stage r
    r

/// Rejected before anything runs, with an Error of `code` among others, and
/// every Error of stage `stage`.
let rejectedHasAt (code: MessageCode) (stage: string) (source: string) : JyroResult<JyroValue> =
    let r = rejectedBeforeRun source
    if not (List.contains code (Script.errorCodes r)) then
        Values.fail (sprintf "Expected an error %A (%d). Diagnostics:\n%s\nSource:\n%s" code (int code) (Script.describe r) source)
    assertAllStages stage r
    r

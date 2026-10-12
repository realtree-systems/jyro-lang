/// Helpers for the chapter 20 suites, on top of Jyro.Spec.Common.
/// Written from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.Support

open System
open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// The text of one Unicode code point (a surrogate pair above U+FFFF).
let cp (codePoint: int) : string = Char.ConvertFromUtf32 codePoint

/// One UTF-16 code unit as a string - also a lone surrogate.
let cu (unit: int) : string = string (char unit)

/// True when `v` is an object that has the property `name`.
let has (name: string) (v: JyroValue) : bool =
    match v with
    | :? JyroObject as o -> (o :> IReadOnlyDictionary<string, JyroValue>).ContainsKey name
    | _ -> false

/// Compare two Jyro values by content: numbers by value, strings ordinal,
/// arrays in order, objects by key set (any order).
let rec sameValue (expected: JyroValue) (actual: JyroValue) : bool =
    match expected, actual with
    | (:? JyroNull), (:? JyroNull) -> true
    | (:? JyroBoolean as a), (:? JyroBoolean as b) -> a.Value = b.Value
    | (:? JyroNumber as a), (:? JyroNumber as b) -> a.Value = b.Value
    | (:? JyroString as a), (:? JyroString as b) -> String.Equals(a.Value, b.Value, StringComparison.Ordinal)
    | (:? JyroArray as a), (:? JyroArray as b) ->
        let xs = List.ofSeq a
        let ys = List.ofSeq b
        xs.Length = ys.Length && List.forall2 sameValue xs ys
    | (:? JyroObject as a), (:? JyroObject as b) ->
        let da = a :> IReadOnlyDictionary<string, JyroValue>
        let db = b :> IReadOnlyDictionary<string, JyroValue>
        da.Count = db.Count
        && da.Keys |> Seq.forall (fun k -> db.ContainsKey k && sameValue da.[k] db.[k])
    | _ -> false

let private show (v: JyroValue) = if isNull (box v) then "<.NET null>" else v.ToJson()

/// Assert that `actual` has the content of the JSON text `expectedJson`.
let assertJson (expectedJson: string) (actual: JyroValue) : unit =
    let expected = Values.json expectedJson
    if not (sameValue expected actual) then
        Values.fail (sprintf "Expected %s, got %s" expectedJson (show actual))

/// Run `source` with the defaults, assert success, and return `Output.<name>`
/// (failing when Output has no such property).
let outProp (name: string) (source: string) : JyroValue =
    let o = Script.outputOf source
    if not (has name o) then
        Values.fail (sprintf "Output has no property %s. Output: %s" name (show o))
    o.GetProperty(name)

/// `Output.v` of a successful run.
let outV (source: string) : JyroValue = outProp "v" source

/// Run `source` with a host function `Mark()` called as the very first
/// statement, so that a test can see whether any statement ran.
let runMarked (options: RunOptions) (source: string) : JyroResult<JyroValue> * RecordingFunction =
    let mark = Host.recorder "Mark"
    let r =
        Script.runWith
            { options with Functions = options.Functions @ [ mark.AsFunction ] }
            ("Mark()\n" + source)
    r, mark

/// Assert the result failed with at least one Error diagnostic.
let assertFailed (r: JyroResult<'T>) : unit =
    if Script.ok r then
        Values.fail (sprintf "Expected the script to be rejected, but it succeeded. Diagnostics:\n%s" (Script.describe r))
    if List.isEmpty (Script.errorCodes r) then
        Values.fail (sprintf "Expected an Error diagnostic, got none. Diagnostics:\n%s" (Script.describe r))

/// Assert `source` is rejected before any statement runs (a `Mark()` first
/// statement is never called), under `options`. Returns the result.
let rejectedWith (options: RunOptions) (source: string) : JyroResult<JyroValue> =
    let r, mark = runMarked options source
    assertFailed r
    if mark.CallCount <> 0 then
        Values.fail (sprintf "The script was rejected only after it started running. Diagnostics:\n%s" (Script.describe r))
    r

/// `rejectedWith` under the defaults.
let rejected (source: string) : JyroResult<JyroValue> = rejectedWith Script.defaults source

/// Assert the only Error of `r` is `code`.
let assertOnly (code: MessageCode) (r: JyroResult<'T>) : unit = Script.expectError code r

/// Assert `r` has at least one Error and every Error is `code`.
let assertAllErrors (code: MessageCode) (r: JyroResult<'T>) : unit =
    let errs = Script.errorCodes r
    if List.isEmpty errs || errs |> List.exists (fun c -> c <> code) then
        Values.fail (sprintf "Expected every error to be %A (%d). Diagnostics:\n%s" code (int code) (Script.describe r))

/// Assert `r` has an Error of `code` (among possibly others).
let assertHasError (code: MessageCode) (r: JyroResult<'T>) : unit =
    if not (Script.errorCodes r |> List.contains code) then
        Values.fail (sprintf "Expected an error %A (%d). Diagnostics:\n%s" code (int code) (Script.describe r))

/// Rejected before running, with exactly one Error, of `code`.
let rejectedWithCode (code: MessageCode) (source: string) : unit =
    rejected source |> assertOnly code

/// Assert `r` has an Error diagnostic whose stage (160.1.11, read through
/// `Pending.stageOf`) is `Parse`: a parse error in the sense of 130.3.
let assertParseStage (r: JyroResult<'T>) : unit =
    let errors =
        r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    let stages = errors |> List.map (fun m -> m.Code, Pending.stageOf m)
    if not (stages |> List.exists (fun (_, s) -> s = "Parse")) then
        Values.fail (sprintf "Expected a parse error (an Error of stage Parse); the errors have stages %A. Diagnostics:\n%s"
                        stages (Script.describe r))

/// Rejected before running, as a parse error.
let parseError (source: string) : unit =
    rejected source |> assertParseStage

/// Compile `source` exactly as given (no statement prepended) and assert it
/// fails as a parse error. For sources whose first character matters.
let parseErrorRaw (source: string) : unit =
    let r = Script.compile source
    assertFailed r
    assertParseStage r

/// Assert `source` compiles (with the defaults).
let compiles (source: string) : unit =
    let r = Script.compile source
    if not (Script.ok r) then
        Values.fail (sprintf "Expected the script to compile. Diagnostics:\n%s" (Script.describe r))

// -- Every limit set explicitly (FLOORS 4.1.11) ------------------------------

/// The four execution limits of 100.1.1, each set explicitly and far above
/// what a test needs, so no run depends on an implementation default.
let allLimits (o: JyroExecutionOptions) : JyroExecutionOptions =
    { o with
        MaxExecutionTime = TimeSpan.FromSeconds 30.
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000
        MaxCallDepth = 64 }

/// `Script.defaults` with the four execution limits and a generous
/// compilation time limit (100.9.1) set explicitly.
let limited : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits allLimits)
        CompilationTimeout = Some (TimeSpan.FromMinutes 10.) }

/// `limited`, with the regex timeout (160.1.10) set explicitly as well, for a
/// run that calls a regular expression function.
let limitedRegex : RunOptions =
    { limited with ConfigureRunner = fun r -> Pending.withRegexTimeout r (TimeSpan.FromSeconds 5.) }

/// Assert `r` has at least one Error and every Error has the stage `stage`
/// (160.1.11, 160.1.12, read through `Pending.stageOf`).
let assertAllStages (stage: string) (r: JyroResult<'T>) : unit =
    let errors =
        r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    if List.isEmpty errors then
        Values.fail (sprintf "Expected Error diagnostics of stage %s, got none. Diagnostics:\n%s" stage (Script.describe r))
    let stages = errors |> List.map (fun m -> m.Code, Pending.stageOf m)
    if stages |> List.exists (fun (_, s) -> s <> stage) then
        Values.fail (sprintf "Expected every error to have the stage %s; the errors have %A. Diagnostics:\n%s"
                        stage stages (Script.describe r))

/// `Output.<name>` of a successful run under `limited`.
let outPropL (name: string) (source: string) : JyroValue =
    let o = Script.output (Script.runWith limited source)
    if not (has name o) then
        Values.fail (sprintf "Output has no property %s. Output: %s" name (show o))
    o.GetProperty(name)

/// `Output.v` of a successful run under `limited`.
let outVL (source: string) : JyroValue = outPropL "v" source

/// The whole `Output` of a successful run under `limited`.
let outputL (source: string) : JyroValue = Script.output (Script.runWith limited source)

/// A parse error under `limited`: rejected, every Error of stage Parse, and a
/// `Mark()` first statement never called (130.3.1). Returns the result.
let parseErrorL (source: string) : JyroResult<JyroValue> =
    let r = rejectedWith limited source
    assertAllStages "Parse" r
    r

/// Rejected under `limited` before anything runs, with every Error of `code`
/// and of stage `stage`.
let rejectedAt (code: MessageCode) (stage: string) (source: string) : JyroResult<JyroValue> =
    let r = rejectedWith limited source
    assertAllErrors code r
    assertAllStages stage r
    r

/// The line, column and length (130.1.2, JYRO-REQ-2122) of the first Error of `code`
/// that compiling `source` under `limited` gives. Judged by the code and the place,
/// not by the stage (JYRO-ADR-105 item 4).
let placeOf (code: MessageCode) (source: string) : int * int * int =
    let r = Script.compileWith limited source
    let ms =
        r.Messages
        |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error && m.Code = code)
        |> List.ofSeq
    if ms.IsEmpty then
        Values.fail (sprintf "No error %A for %A. Diagnostics:\n%s" code source (Script.describe r))
    let m = ms.Head
    if not m.Location.HasValue then
        Values.fail (sprintf "%A has no location. Diagnostics:\n%s" code (Script.describe r))
    let l = m.Location.Value
    l.Line, l.Column, l.Length

/// The standard 38 reserved words of 20.4.1.
let reservedWords : string list =
    [ "and"; "array"; "boolean"; "break"; "by"; "case"; "continue"; "default"; "delete"; "do"
      "downto"; "else"; "elseif"; "end"; "exit"; "fail"; "false"; "for"; "foreach"; "func"
      "if"; "in"; "is"; "match"; "not"; "null"; "number"; "object"; "or"; "return"
      "string"; "switch"; "then"; "to"; "true"; "union"; "var"; "while" ]

/// Theory rows of one string each.
let rowsOf (xs: string seq) : seq<obj[]> = xs |> Seq.map (fun x -> [| box x |])

/// Helpers for the chapter 97 and 98 suites (query and lambda functions).
/// Written from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.Support

open System
open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

let fail (message: string) : 'T = Values.fail message

let check (condition: bool) (message: string) : unit =
    if not condition then fail message

/// A Jyro double-quoted string literal for a plain string (no quote or backslash in it).
let q (s: string) : string = "\"" + s + "\""

/// Compile and run `source` under `options`, assert success, and return `Output.r`.
let runR (options: RunOptions) (source: string) : JyroValue =
    let result = Script.runWith options source
    if not result.IsSuccess then
        fail (sprintf "Expected success for:\n%s\nDiagnostics:\n%s" source (Script.describe result))
    Script.valueOf result |> Values.prop "r"

/// `runR` with the defaults.
let evalR (source: string) : JyroValue = runR Script.defaults source

/// The value of one expression: `Output.r = <expression>`.
let expr (expression: string) : JyroValue = evalR ("Output.r = " + expression)

/// JSON text normalized through the implementation's own reader and writer, so
/// that a comparison is by value and property order, not by whitespace.
let norm (json: string) : string = Values.toJson (Values.json json)

let assertJson (expectedJson: string) (actual: JyroValue) (context: string) : unit =
    let e = norm expectedJson
    let a = Values.toJson actual
    if e <> a then fail (sprintf "%s\nexpected %s\nactual   %s" context e a)

/// `Output.r = <expression>` must be the value that `expectedJson` gives.
let assertExpr (expectedJson: string) (expression: string) : unit =
    assertJson expectedJson (expr expression) expression

/// The script must fail with exactly one error, of `code`.
let expectErrWith (options: RunOptions) (code: MessageCode) (source: string) : unit =
    let result = Script.runWith options source
    let errors = Script.errorCodes result
    if result.IsSuccess || errors <> [ code ] then
        fail (sprintf "Expected exactly one error %A (%d) for:\n%s\nIsSuccess=%b, diagnostics:\n%s"
                code (int code) source result.IsSuccess (Script.describe result))

let expectErr (code: MessageCode) (source: string) : unit = expectErrWith Script.defaults code source

/// The one error of a failed run (asserts there is exactly one).
let onlyErrorOf (options: RunOptions) (source: string) : DiagnosticMessage =
    let result = Script.runWith options source
    if result.IsSuccess then
        fail (sprintf "Expected a failure for:\n%s\nbut it succeeded" source)
    Script.onlyErrorMessage result

let argsText (m: DiagnosticMessage) : string list =
    if isNull m.Args then [] else m.Args |> Array.map (fun a -> if isNull a then "<null>" else string a) |> List.ofArray

// -- The standard library registry ---------------------------------------

let registry (name: string) : IJyroFunction =
    let mutable f : IJyroFunction = Unchecked.defaultof<IJyroFunction>
    match StdlibRegistry.tryGetFunction (name, &f) with
    | true when not (isNull (box f)) -> f
    | _ -> fail (sprintf "%s is not in StdlibRegistry" name)

/// The function's parameters are these names, in order, and none is optional.
let assertSignature (name: string) (expected: string list) : unit =
    let f = registry name
    let ps = f.Signature.Parameters |> List.ofSeq
    let names = ps |> List.map (fun p -> p.Name)
    check (names = expected) (sprintf "%s parameters: expected %A, got %A" name expected names)
    for p in ps do
        check (not p.IsOptional) (sprintf "%s parameter %s is optional" name p.Name)

/// The function declares the effect Pure, and a call to it compiles under a
/// maximum effect of Pure.
let assertPureEffect (name: string) (callSource: string) : unit =
    let f = registry name
    check (f.Effect = FunctionEffect.Pure) (sprintf "%s declares the effect %A, not Pure" name f.Effect)
    let compiled = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } callSource
    check compiled.IsSuccess
        (sprintf "%s did not compile under WithMaxEffect(Pure):\n%s\n%s" name callSource (Script.describe compiled))

// -- Host probes ------------------------------------------------------------

/// A Pure host function `Probe(...)` that records its arguments and returns null.
let probe () : RecordingFunction = Host.recorder "Probe"

let withFunctions (fs: RecordingFunction list) : RunOptions =
    { Script.defaults with Functions = fs |> List.map (fun f -> f.AsFunction) }

/// The JSON of the first argument of each call.
let firstArgs (p: RecordingFunction) : string list =
    p.Calls |> List.map (fun args -> match args with a :: _ -> Values.toJson a | [] -> "<none>")

/// A host function `MakeProbe()` that returns a function value (made by the JyroFunction constructor)
/// of `paramCount` parameters. Each call of that function value is recorded with
/// every argument it was given, and answered by `reply`.
type FunctionValueProbe(paramCount: int, reply: JyroValue list -> JyroValue) =
    let calls = ResizeArray<JyroValue list>()
    let gate = obj ()
    let maker =
        Host.func "MakeProbe" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
            JyroFunction(
                Func<IReadOnlyList<JyroValue>, JyroValue>(fun args ->
                    let received = List.ofSeq args
                    lock gate (fun () -> calls.Add received)
                    reply received),
                paramCount) :> JyroValue)
    member _.Calls : JyroValue list list = lock gate (fun () -> List.ofSeq calls)
    member _.Maker : RecordingFunction = maker

// -- Function families -------------------------------------------------------

let fieldFns = [ "AnyByField"; "AllByField"; "FindByField"; "WhereByField"; "CountIf" ]
let queryFns = fieldFns @ [ "Select"; "Project"; "Omit" ]
let lambdaFns = [ "Map"; "Where"; "Reduce"; "Each"; "Find"; "All"; "Any"; "SortBy" ]

let ops = [ "=="; "!="; "<"; "<="; ">"; ">=" ]
let relOps = [ "<"; "<="; ">"; ">=" ]

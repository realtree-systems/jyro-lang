/// Helpers of the chapter 91 suite (String Functions), on top of Jyro.Spec.Common.
/// Written from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.P91.Support

open System
open System.Diagnostics
open System.Globalization
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

// -- Text ----------------------------------------------------------------------

/// The high surrogate of U+1F600.
let hi = string (char 0xD83D)
/// The low surrogate of U+1F600.
let lo = string (char 0xDE00)
/// U+1F600 as its surrogate pair.
let pair = hi + lo

/// A string of the given code points (supplementary ones as surrogate pairs).
let cp (points: int list) : string =
    points |> List.map Char.ConvertFromUtf32 |> String.concat ""

/// A Jyro string literal for `s`: printable ASCII as itself, everything else as
/// `\uXXXX` escapes (20.7.2). A surrogate pair becomes two escapes (20.7.4). Do
/// not give it a lone surrogate: the parser rejects one (20.2.5); pass those
/// through `Input` instead.
let lit (s: string) : string =
    let body =
        s
        |> Seq.map (fun c ->
            if c >= ' ' && c < '\u007f' && c <> '"' && c <> '\\' then string c
            else sprintf "\\u%04X" (int c))
        |> String.concat ""
    "\"" + body + "\""

// -- Running an expression -----------------------------------------------------

/// The script that stores the value of `expr` as the only element of `Output.r`
/// (the array keeps a `null` result visible), after the statements `pre`.
let script (pre: string) (expr: string) : string =
    (if String.IsNullOrEmpty pre then "" else pre + "\n") + "Output.r = [" + expr + "]"

/// Run `expr` with the defaults.
let run (expr: string) : JyroResult<JyroValue> = Script.run (script "" expr)

/// Run `expr` after the statements `pre`.
let runPre (pre: string) (expr: string) : JyroResult<JyroValue> = Script.run (script pre expr)

/// Run `expr` with `input` as `Input`.
let runIn (input: JyroValue) (expr: string) : JyroResult<JyroValue> = Script.runInput input (script "" expr)

/// The value of `expr` from a successful run.
let resultOf (result: JyroResult<JyroValue>) : JyroValue =
    match Values.asList (Values.prop "r" (Script.output result)) with
    | [ v ] -> v
    | other -> Values.fail (sprintf "Expected one element in Output.r, got %d" other.Length)

let eval (expr: string) : JyroValue = resultOf (run expr)
let evalPre (pre: string) (expr: string) : JyroValue = resultOf (runPre pre expr)
let evalIn (input: JyroValue) (expr: string) : JyroValue = resultOf (runIn input expr)

let str (expr: string) : string = Values.asString (eval expr)
let strIn (input: JyroValue) (expr: string) : string = Values.asString (evalIn input expr)
let boolOf (expr: string) : bool = Values.asBool (eval expr)
let boolIn (input: JyroValue) (expr: string) : bool = Values.asBool (evalIn input expr)
let strs (expr: string) : string list = Values.asList (eval expr) |> List.map Values.asString
let strsIn (input: JyroValue) (expr: string) : string list = Values.asList (evalIn input expr) |> List.map Values.asString

/// True when the value is the Jyro `null`.
let isNullValue (v: JyroValue) : bool =
    match v with
    | :? JyroNull -> true
    | _ when isNull (box v) -> Values.fail "Got a .NET null where a Jyro value was expected"
    | _ -> false

/// Assert that `expr` gives the Jyro `null`.
let assertNull (expr: string) : unit =
    let v = eval expr
    if not (isNullValue v) then Values.fail (sprintf "Expected null from %s, got %s" expr (Values.toJson v))

/// `TypeOf(expr)` inside the script.
let typeOf (expr: string) : string = str ("TypeOf(" + expr + ")")

/// An `Input` object holding the given string properties.
let inputOf (pairs: (string * string) list) : JyroValue =
    Values.obj (pairs |> List.map (fun (k, v) -> k, Values.str v))

/// The standard `Input` of this suite: `hi`, `lo` (the lone halves of U+1F600)
/// and `pair`.
let surrogates () : JyroValue = inputOf [ "hi", hi; "lo", lo; "pair", pair ]

/// Jyro expressions for an array, an object and a function value: the values a
/// string parameter must refuse with `ArgumentTypeMismatch`.
let badList = [ "[]"; "[\"a\"]"; "{}"; "{a: \"b\"}"; "x => x" ]

/// `badList` as theory data.
let badData : obj[] seq = badList |> Seq.map (fun s -> [| box s |])

// -- Errors ----------------------------------------------------------------------

/// `expr` fails with exactly one error, of `code`.
let expectErr (code: MessageCode) (expr: string) : unit = Script.expectError code (run expr)

/// `expr`, after `pre`, fails with exactly one error, of `code`.
let expectErrPre (code: MessageCode) (pre: string) (expr: string) : unit = Script.expectError code (runPre pre expr)

/// `expr` fails, and one of its errors has `code` (other errors, such as a
/// missing required argument that a wrong name leaves, may come with it).
let expectErrAmong (code: MessageCode) (expr: string) : unit =
    let result = run expr
    Assert.False(Script.ok result, sprintf "Expected %s to fail" expr)
    Assert.Contains(code, Script.errorCodes result)

/// `expr` compiles, then fails when it runs, with at least one Error diagnostic.
/// For a clause that names no code.
let failsAtRuntime (expr: string) : unit =
    let src = script "" expr
    let compiled = Script.compile src
    Assert.True(Script.ok compiled, "Expected the script to compile:\n" + Script.describe compiled)
    let result = Script.run src
    Assert.False(Script.ok result, sprintf "Expected %s to fail at run time, but it succeeded" expr)
    Assert.NotEmpty(Script.errorCodes result)

/// As `failsAtRuntime`, with `Input`.
let failsAtRuntimeIn (input: JyroValue) (expr: string) : unit =
    let src = script "" expr
    let compiled = Script.compile src
    Assert.True(Script.ok compiled, "Expected the script to compile:\n" + Script.describe compiled)
    let result = Script.runInput input src
    Assert.False(Script.ok result, sprintf "Expected %s to fail at run time, but it succeeded" expr)
    Assert.NotEmpty(Script.errorCodes result)

/// `expr` compiles (no error) with the ceiling `WithMaxEffect(effect)` and runs
/// successfully; its value.
let evalUnderMaxEffect (effect: FunctionEffect) (expr: string) : JyroValue =
    let options = { Script.defaults with MaxEffect = Some effect }
    let src = script "" expr
    let compiled = Script.compileWith options src
    Assert.True(Script.ok compiled, "Expected the script to compile under the effect ceiling:\n" + Script.describe compiled)
    Assert.DoesNotContain(MessageCode.FunctionEffectExceedsMax, Script.codes compiled)
    resultOf (Script.runWith options src)

/// The compile result of `expr` under `WithMaxEffect(effect)`.
let compileUnderMaxEffect (effect: FunctionEffect) (expr: string) : JyroResult<CompiledProgram> =
    Script.compileWith { Script.defaults with MaxEffect = Some effect } (script "" expr)

// -- FLOORS 4.1.11: every run sets its limits -------------------------------------

/// The execution limits every run of the 4.1.11 tests sets explicitly: none of
/// them can end a run before the case under test is reached.
let specLimits : JyroExecutionOptions =
    Script.limits (fun o ->
        { o with
            MaxExecutionTime = TimeSpan.FromSeconds 30.
            MaxStatements = 1_000_000
            MaxLoopIterations = 1_000_000
            MaxCallDepth = min 64 JyroExecutionOptions.MaxSafeCallDepth })

/// The defaults with the limits of `specLimits`.
let bounded : RunOptions = { Script.defaults with Limits = Some specLimits }

/// Run `expr` under `bounded`.
let runB (expr: string) : JyroResult<JyroValue> = Script.runWith bounded (script "" expr)
/// Run `expr` after `pre` under `bounded`.
let runPreB (pre: string) (expr: string) : JyroResult<JyroValue> = Script.runWith bounded (script pre expr)
/// Run `expr` with `input` as `Input` under `bounded`.
let runInB (input: JyroValue) (expr: string) : JyroResult<JyroValue> =
    Script.runWith { bounded with Input = ValueInput input } (script "" expr)

let strB (expr: string) : string = Values.asString (resultOf (runB expr))
let strInB (input: JyroValue) (expr: string) : string = Values.asString (resultOf (runInB input expr))
let evalPreB (pre: string) (expr: string) : JyroValue = resultOf (runPreB pre expr)

/// Exactly one error, of `code`, at the stage Execute (160.1.12: a runtime error).
let expectRuntimeError (code: MessageCode) (result: JyroResult<'T>) : unit =
    Script.expectError code result
    Assert.Equal("Execute", Pending.stageOf (Script.onlyErrorMessage result))

/// `expr` compiles, then fails when it runs under `bounded` with exactly one
/// error, of `code`, at the stage Execute.
let expectRuntimeErr (code: MessageCode) (expr: string) : unit =
    let src = script "" expr
    let compiled = Script.compileWith bounded src
    Assert.True(Script.ok compiled, "Expected the script to compile:\n" + Script.describe compiled)
    expectRuntimeError code (Script.runWith bounded src)

// -- Culture ---------------------------------------------------------------------

/// Run `body` with the current thread's culture and UI culture set to `name`
/// ("" for the invariant culture), and restore both after.
let withCulture (name: string) (body: unit -> 'T) : 'T =
    let culture = if name = "" then CultureInfo.InvariantCulture else CultureInfo.GetCultureInfo(name)
    let saved = CultureInfo.CurrentCulture
    let savedUi = CultureInfo.CurrentUICulture
    try
        CultureInfo.CurrentCulture <- culture
        CultureInfo.CurrentUICulture <- culture
        body ()
    finally
        CultureInfo.CurrentCulture <- saved
        CultureInfo.CurrentUICulture <- savedUi

/// The cultures the culture tests run under.
let cultures = [ "tr-TR"; "az-Latn-AZ"; "lt-LT"; "en-US"; "ja-JP"; "" ]

/// The collection of tests that change the culture: not run in parallel.
[<CollectionDefinition("P91 culture", DisableParallelization = true)>]
type CultureCollection() = class end

// -- Statistics ------------------------------------------------------------------

/// The chi-square statistic of `observed` counts against equal expected counts.
let chiSquareUniform (observed: int list) : float =
    let total = float (List.sum observed)
    let expected = total / float observed.Length
    observed |> List.sumBy (fun o -> (float o - expected) ** 2.0 / expected)

/// The chi-square statistic of `observed` against the given expected probabilities.
let chiSquare (observed: int list) (probabilities: float list) : float =
    let total = float (List.sum observed)
    List.zip observed probabilities
    |> List.sumBy (fun (o, p) -> let e = total * p in (float o - e) ** 2.0 / e)

/// The critical chi-square value at p = 1e-6 for `df` degrees of freedom
/// (Wilson-Hilferty; z = 4.7534 for an upper tail of 1e-6).
let chiCritical (df: int) : float =
    let d = float df
    let z = 4.7534
    let t = 1.0 - 2.0 / (9.0 * d) + z * sqrt (2.0 / (9.0 * d))
    // small-df correction: exact values at df 1..3 are 23.93, 27.63, 30.66
    match df with
    | 1 -> 23.93
    | 2 -> 27.63
    | 3 -> 30.66
    | _ -> d * t * t * t

/// Seconds taken by `body`, and its result.
let timed (body: unit -> 'T) : 'T * float =
    let sw = Stopwatch.StartNew()
    let v = body ()
    v, sw.Elapsed.TotalSeconds

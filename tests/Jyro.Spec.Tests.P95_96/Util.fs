/// Helpers local to the chapter 95-96 suite. Written from the book and the
/// public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.Util

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// A lone high surrogate (U+D83D) and a lone low surrogate (U+DE00), made on
/// the host side because a Jyro literal cannot hold one (20.7.4).
let hi : string = string (char 0xD83D)
let lo : string = string (char 0xDE00)
/// U+1F600 as a surrogate pair.
let emoji : string = hi + lo

/// Jyro source that declares `hi` and `lo` as lone surrogates, cut from a pair
/// with Substring (91.10), for scripts that need one without host Input.
let loneDecl = "var pair = '\\uD83D\\uDE00'\nvar hi = Substring(pair, 0, 1)\nvar lo = Substring(pair, 1, 1)\n"

// -- FLOORS 4.1.11: every run sets its limits, and a regex run its timeout -----

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

/// `bounded`, and a regex timeout of 5 s (160.1.10, 160.7.2, reached through
/// `Pending`), for every run that evaluates a schema pattern.
let boundedRegex : RunOptions =
    { bounded with ConfigureRunner = fun r -> Pending.withRegexTimeout r (TimeSpan.FromSeconds 5.) }

/// Exactly one error, of `code`, at the stage Execute (160.1.12: a runtime error).
let expectRuntimeError (code: MessageCode) (result: JyroResult<'T>) : unit =
    Script.expectError code result
    Assert.Equal("Execute", Pending.stageOf (Script.onlyErrorMessage result))

/// `Output.r` of a successful run under `bounded` with `Input` given as a Jyro value.
let rValB (input: JyroValue) (src: string) : JyroValue =
    Script.output (Script.runWith { bounded with Input = ValueInput input } src) |> Values.prop "r"

/// Run `ValidateSchema(Input.d, Input.s)` under `boundedRegex`, with `Input` =
/// {"d": data, "s": schema}.
let validateRawB (dataJson: string) (schemaJson: string) : JyroResult<JyroValue> =
    Script.runWith
        { boundedRegex with Input = JsonInput(sprintf "{\"d\": %s, \"s\": %s}" dataJson schemaJson) }
        "Output.r = ValidateSchema(Input.d, Input.s)\nOutput.after = 1"

/// `Output.r` of a successful run with the defaults.
let r (src: string) : JyroValue = Script.outputOf src |> Values.prop "r"

/// `Output.r` of a successful run with `Input` given as JSON text.
let rIn (inputJson: string) (src: string) : JyroValue =
    Script.output (Script.runJson inputJson src) |> Values.prop "r"

/// `Output.r` of a successful run with `Input` given as a Jyro value.
let rVal (input: JyroValue) (src: string) : JyroValue =
    Script.output (Script.runInput input src) |> Values.prop "r"

let rStr src = Values.asString (r src)
let rBool src = Values.asBool (r src)
let rJson src = Values.toJson (r src)
let rNum src = Values.asDecimal (r src)

/// Assert the run fails with exactly one error, of `code`.
let fails (code: MessageCode) (src: string) = Script.expectError code (Script.run src)
let failsIn (inputJson: string) (code: MessageCode) (src: string) =
    Script.expectError code (Script.runJson inputJson src)
let failsVal (input: JyroValue) (code: MessageCode) (src: string) =
    Script.expectError code (Script.runInput input src)

/// Assert the compile fails with exactly one error, of `code`.
let compileFails (code: MessageCode) (src: string) = Script.expectError code (Script.compile src)

/// JSON text normalized through the host parser and writer, so that two texts
/// that differ only in how the host escapes a character compare equal.
let norm (json: string) : string = Values.toJson (Values.json json)

/// Assert the script runs with success and `Output.r` is JSON-equal to `json`
/// (compared as JSON text after a round trip through the host parser).
let expectJson (json: string) (src: string) =
    Assert.Equal(Values.toJson (Values.json json), rJson src)

let expectJsonIn (input: string) (json: string) (src: string) =
    Assert.Equal(Values.toJson (Values.json json), Values.toJson (rIn input src))

let isNullValue (v: JyroValue) = not (isNull (box v)) && v.ValueType = JyroValueType.Null

let expectNull (v: JyroValue) =
    Assert.True(isNullValue v, sprintf "expected null, got %s" (if isNull (box v) then "<.NET null>" else v.ToJson()))

/// The effect a standard library function declares.
let effectOf (name: string) : FunctionEffect =
    let mutable f = Unchecked.defaultof<IJyroFunction>
    // StdlibRegistry.TryGetFunction(String, out IJyroFunction), as F# sees it
    if StdlibRegistry.tryGetFunction (name, &f) then f.Effect
    else Values.fail (sprintf "StdlibRegistry has no function %s" name)

/// Assert the stdlib function `name` has exactly `effect`, and that `call`
/// compiles under a maximum effect of `effect`.
let assertEffect (name: string) (effect: FunctionEffect) (call: string) =
    Assert.Equal(effect, effectOf name)
    let c = Script.compileWith { Script.defaults with MaxEffect = Some effect } call
    Assert.True(Script.ok c, Script.describe c)

/// Assert `call` is rejected under a maximum effect of `effect`.
let assertRejectedUnder (effect: FunctionEffect) (call: string) =
    Script.expectError MessageCode.FunctionEffectExceedsMax
        (Script.compileWith { Script.defaults with MaxEffect = Some effect } call)

/// A host array nested `depth` levels (depth 1 is `[1]`).
let hostNested (depth: int) : JyroValue =
    let mutable v = Values.int 1
    for _ in 1 .. depth do v <- Values.arr [ v ]
    v

/// The value nesting bound D (100.7.1). The book says the implementation MUST
/// document it; no value is given in the book, so it is found here by the host
/// API's ToJson walk (100.7.7 binds host conversion to the same bound), which
/// no function under test takes part in. Fails when no bound is found by 4096.
let nestingBound : Lazy<int> =
    lazy (
        let works d =
            try
                Values.toJson (hostNested d) |> ignore
                true
            with _ -> false
        if not (works 64) then Values.fail "the value nesting bound is below 64 (100.7.1)"
        if works 4096 then Values.fail "no value nesting bound found up to depth 4096"
        let mutable lo = 64
        let mutable hi = 4096
        while hi - lo > 1 do
            let mid = (lo + hi) / 2
            if works mid then lo <- mid else hi <- mid
        lo)

/// Jyro source that builds `v`, an array nested `depth` levels (`[1]` is 1).
let nestedArraySrc (depth: int) =
    sprintf "var v = 1\nfor i in 0 to %d do v = [v] end\n" depth

/// Jyro source that builds `v`, an object nested `depth` levels (`{k: 1}` is 1).
let nestedObjectSrc (depth: int) =
    sprintf "var v = 1\nfor i in 0 to %d do v = {k: v} end\n" depth

/// Jyro source that declares cyclic values: `ca` (an array holding itself),
/// `co` (an object holding itself), `ci` (an indirect cycle through an object
/// and an array: ci.p = [ {q: ci} ]).
let cyclesDecl =
    "var ca = [1]\nca[0] = ca\nvar co = {}\nco.self = co\nvar ci = {}\nci.p = [{q: ci}]\n"

/// The code units of a string, as hex, for failure messages.
let units (s: string) = s |> Seq.map (fun c -> sprintf "%04X" (int c)) |> String.concat " "

// -- ValidateSchema -------------------------------------------------------

/// One element of a ValidateSchema result.
type SchemaError = { Path: string; Keyword: string; Message: string }

/// The elements of a ValidateSchema result array.
let schemaErrors (v: JyroValue) : SchemaError list =
    Values.asList v
    |> List.map (fun e ->
        { Path = Values.asString (Values.prop "path" e)
          Keyword = Values.asString (Values.prop "keyword" e)
          Message = Values.asString (Values.prop "message" e) })

/// The (path, keyword, message) triples of `ValidateSchema(data, schema)` run
/// under `boundedRegex`, asserting success.
let schemaResultB (dataJson: string) (schemaJson: string) : (string * string * string) list =
    Script.output (validateRawB dataJson schemaJson)
    |> Values.prop "r"
    |> schemaErrors
    |> List.map (fun e -> e.Path, e.Keyword, e.Message)

/// Run `ValidateSchema(Input.d, Input.s)` with `Input` = {"d": data, "s": schema}.
let validateRaw (dataJson: string) (schemaJson: string) : JyroResult<JyroValue> =
    Script.runJson (sprintf "{\"d\": %s, \"s\": %s}" dataJson schemaJson)
        "Output.r = ValidateSchema(Input.d, Input.s)"

/// The (path, keyword) pairs of `ValidateSchema(data, schema)`, asserting success.
let validate (dataJson: string) (schemaJson: string) : (string * string) list =
    let res = validateRaw dataJson schemaJson
    Script.output res |> Values.prop "r" |> schemaErrors |> List.map (fun e -> e.Path, e.Keyword)

/// Assert ValidateSchema gives exactly `expected` (path, keyword) pairs, in order.
let expectErrors (expected: (string * string) list) (dataJson: string) (schemaJson: string) =
    Assert.Equal<(string * string) list>(expected, validate dataJson schemaJson)

/// Assert ValidateSchema passes (empty result).
let passes (dataJson: string) (schemaJson: string) = expectErrors [] dataJson schemaJson

/// Assert ValidateSchema fails the data (non-empty result, no runtime error).
let rejects (dataJson: string) (schemaJson: string) =
    let errs = validate dataJson schemaJson
    Assert.False(List.isEmpty errs, sprintf "expected a non-empty result for data %s against %s" dataJson schemaJson)

/// Assert ValidateSchema raises a runtime error of `code`.
let schemaRaises (code: MessageCode) (dataJson: string) (schemaJson: string) =
    Script.expectError code (validateRaw dataJson schemaJson)

/// Run `ValidateSchema(Input.d, Input.s)` in a child process (this assembly's
/// own entry point, Program.fs), for a call that may overflow the stack: a
/// stack overflow ends the whole .NET process, so running it here would crash
/// the test host. Returns the child's line: "OK <result json>" or
/// "ERR <codes>". Fails the test when the child crashes or hangs.
let validateIsolated (dataJson: string) (schemaJson: string) : string =
    let asm = System.Reflection.Assembly.GetExecutingAssembly().Location
    let psi = Diagnostics.ProcessStartInfo("dotnet", UseShellExecute = false, RedirectStandardOutput = true, RedirectStandardError = true)
    for a in [ asm; "validate"; dataJson; schemaJson ] do psi.ArgumentList.Add a
    use p = new Diagnostics.Process(StartInfo = psi)
    let err = Text.StringBuilder()
    p.ErrorDataReceived.Add(fun e -> if not (isNull e.Data) && err.Length < 2000 then err.AppendLine(e.Data) |> ignore)
    p.Start() |> ignore
    p.BeginErrorReadLine()
    let out = p.StandardOutput.ReadToEndAsync()
    if not (p.WaitForExit 30000) then
        (try p.Kill true with _ -> ())
        Values.fail (sprintf "ValidateSchema did not return within 30 s for schema %s" schemaJson)
    p.WaitForExit()
    if p.ExitCode <> 0 then
        let first = err.ToString().Split('\n') |> Array.truncate 3 |> String.concat " | "
        Values.fail (sprintf "ValidateSchema crashed the process (exit code %d) for schema %s: %s" p.ExitCode schemaJson first)
    out.Result.Trim()

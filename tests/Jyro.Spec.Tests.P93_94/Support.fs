/// Helpers for the chapter 93 and 94 suites (math, date and time functions).
/// Written from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.P93_94.Support

open System
open System.Globalization
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// A decimal from its invariant text (xUnit InlineData cannot carry decimals).
let dec (s: string) : decimal = Decimal.Parse(s, NumberStyles.Float, CultureInfo.InvariantCulture)

/// A double from its invariant text.
let dbl (s: string) : float = Double.Parse(s, NumberStyles.Float, CultureInfo.InvariantCulture)

/// An `Input` object from named values.
let inputOf (pairs: (string * JyroValue) list) : JyroValue = Values.obj pairs

/// Run `source` with `Input` set to an object holding `pairs`.
let runIn (pairs: (string * JyroValue) list) (source: string) : JyroResult<JyroValue> =
    Script.runInput (inputOf pairs) source

/// Evaluate `expr` (with `Input` holding `pairs`) into `Output.r` and return it,
/// asserting the execution succeeded.
let evalIn (pairs: (string * JyroValue) list) (expr: string) : JyroValue =
    runIn pairs ("Output.r = " + expr) |> Script.output |> Values.prop "r"

/// Evaluate `expr` with an empty `Input`.
let eval (expr: string) : JyroValue = evalIn [] expr

/// Evaluate a script whose result is left in `Output.r`.
let evalScriptIn (pairs: (string * JyroValue) list) (source: string) : JyroValue =
    runIn pairs source |> Script.output |> Values.prop "r"

/// True when the value is the Jyro `null`.
let isNullValue (v: JyroValue) : bool =
    not (isNull (box v)) && v.ValueType = JyroValueType.Null

/// Assert the value is `null`.
let assertNull (v: JyroValue) : unit =
    if not (isNullValue v) then
        Values.fail (sprintf "Expected null, got %s" (if isNull (box v) then "<.NET null>" else v.ToJson()))

/// Assert the value is a number held exactly, equal to `expected`.
let assertExact (expected: decimal) (v: JyroValue) : unit =
    if not (Values.isExact v) then
        Values.fail (sprintf "Expected the exact number %M, got an inexact number %s (%.17g)" expected (v.ToJson()) (Values.asFloat v))
    let actual = Values.asDecimal v
    if actual <> expected then
        Values.fail (sprintf "Expected the exact number %M, got the exact number %M" expected actual)

/// Assert the value is a number held inexactly, whose double is `expected`
/// (compared with `=`, so -0.0 equals 0.0; use `assertSignBit` for the sign).
let assertInexact (expected: float) (v: JyroValue) : unit =
    if Values.isExact v then
        Values.fail (sprintf "Expected an inexact number %.17g, got the exact number %M" expected (Values.asDecimal v))
    let actual = Values.asFloat v
    if actual <> expected then
        Values.fail (sprintf "Expected the inexact number %.17g, got %.17g" expected actual)

/// Assert the value is inexact and within `relTol` (relative) of `expected`.
let assertInexactNear (expected: float) (relTol: float) (v: JyroValue) : unit =
    if Values.isExact v then
        Values.fail (sprintf "Expected an inexact number near %.17g, got the exact number %M" expected (Values.asDecimal v))
    let actual = Values.asFloat v
    if abs (actual - expected) > abs expected * relTol then
        Values.fail (sprintf "Expected an inexact number near %.17g (rel %g), got %.17g" expected relTol actual)

/// Assert the number's double value is `expected`, whatever its representation.
let assertNumber (expected: float) (v: JyroValue) : unit =
    let actual = Values.asFloat v
    if actual <> expected then
        Values.fail (sprintf "Expected the number %.17g, got %.17g (%s)" expected actual (v.ToJson()))

/// Assert the double of the number has its sign bit set (true) or clear (false).
let assertSignBit (negative: bool) (v: JyroValue) : unit =
    let actual = Values.asFloat v
    let isNeg = BitConverter.DoubleToInt64Bits(actual) < 0L
    if isNeg <> negative then
        Values.fail (sprintf "Expected %s zero, got %s zero (%.17g)" (if negative then "-" else "+") (if isNeg then "-" else "+") actual)

/// Assert the value is the string `expected`.
let assertString (expected: string) (v: JyroValue) : unit =
    let actual = Values.asString v
    if actual <> expected then Values.fail (sprintf "Expected the string %A, got %A" expected actual)

/// Assert the value is the boolean `true`.
let assertTrue (v: JyroValue) : unit =
    if not (Values.asBool v) then Values.fail "Expected true, got false"

/// A host inexact number, asserting the precondition that `JyroNumber(Double)`
/// held it inexactly (the test would prove nothing otherwise).
let hostInexact (d: float) : JyroValue =
    let v = Values.inexact d
    if Values.isExact v then
        Values.fail (sprintf "Precondition: JyroNumber(%.17g as double) was held exactly; the test needs an inexact number" d)
    if BitConverter.DoubleToInt64Bits(Values.asFloat v) <> BitConverter.DoubleToInt64Bits(d) then
        Values.fail (sprintf "Precondition: JyroNumber(%.17g as double) did not keep its double bits" d)
    v

/// A host exact number, asserting that it is held exactly.
let hostExact (d: decimal) : JyroValue =
    let v = Values.exact d
    if not (Values.isExact v) then Values.fail (sprintf "Precondition: JyroNumber(%M) was held inexactly" d)
    v

// -- Compile-time and runtime errors -----------------------------------------

/// Assert that `source` is rejected before execution (at compile or link time)
/// with exactly one error, of `code`.
let expectCompileError (code: MessageCode) (source: string) : unit =
    let r = Script.compile source
    if r.IsSuccess then Values.fail (sprintf "Expected %A at compile time, but %A compiled" code source)
    Script.expectError code r

/// Assert that `source` is rejected before execution and that one of its errors is `code`.
let expectCompileErrorAmong (code: MessageCode) (source: string) : unit =
    let r = Script.compile source
    if r.IsSuccess then Values.fail (sprintf "Expected %A at compile time, but %A compiled" code source)
    let codes = Script.errorCodes r
    if not (List.contains code codes) then
        Values.fail (sprintf "Expected %A among the compile errors of %A, got:\n%s" code source (Script.describe r))

/// Assert that `source` compiles with no error diagnostic.
let expectCompiles (source: string) : unit =
    let r = Script.compile source
    if not r.IsSuccess then
        Values.fail (sprintf "Expected %A to compile. Diagnostics:\n%s" source (Script.describe r))

/// The single runtime error of a run: `source` must compile, then its run must
/// fail with exactly one error diagnostic, which is returned.
let runtimeErrorIn (pairs: (string * JyroValue) list) (source: string) : DiagnosticMessage =
    let compiled = Script.compile source
    if not compiled.IsSuccess then
        Values.fail (sprintf "Expected a runtime error, but %A was rejected before execution:\n%s" source (Script.describe compiled))
    Script.onlyErrorMessage (runIn pairs source)

/// Assert that `source` compiles and its run fails with exactly one error, of `code`.
let expectRuntimeErrorIn (pairs: (string * JyroValue) list) (code: MessageCode) (source: string) : unit =
    let m = runtimeErrorIn pairs source
    if m.Code <> code then
        Values.fail (sprintf "Expected the runtime error %A (%d) from %A, got %A (%d): %s" code (int code) source m.Code (int m.Code) m.Message)

/// `expectRuntimeErrorIn` with an empty `Input`.
let expectRuntimeError (code: MessageCode) (source: string) : unit = expectRuntimeErrorIn [] code source

/// Assert the evaluation of `expr` raises the runtime error `code`.
let expectExprError (code: MessageCode) (expr: string) : unit =
    expectRuntimeError code ("Output.r = " + expr)

/// Assert the evaluation of `expr` (with `Input` holding `pairs`) raises `code`.
let expectExprErrorIn (pairs: (string * JyroValue) list) (code: MessageCode) (expr: string) : unit =
    expectRuntimeErrorIn pairs code ("Output.r = " + expr)

/// Assert the run succeeded with no diagnostic at all.
let assertNoDiagnostics (r: JyroResult<JyroValue>) : unit =
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "Expected success with no diagnostic. Diagnostics:\n%s" (Script.describe r))

/// The string form of each diagnostic argument.
let argStrings (m: DiagnosticMessage) : string list =
    if isNull m.Args then [] else m.Args |> Array.map (fun a -> if isNull a then "<null>" else string a) |> List.ofArray

// -- Non-number arguments ----------------------------------------------------

/// Non-number arguments as Jyro source text (literals and a lambda held in a variable
/// are prepared by `withBad`).
let badLiterals = [ "null"; "\"1\""; "\"\""; "true"; "false"; "[]"; "[1]"; "{}"; "{a: 1}"; "f" ]

/// A script that declares a function value `f`, then sets `Output.r = expr`.
let withFn (expr: string) : string = "var f = (x) => x\nOutput.r = " + expr

/// Non-number values given through `Input.v` (a runtime path no compiler can see).
let badInputs : (string * JyroValue) list =
    [ "null", Values.nul
      "string", Values.str "1"
      "empty string", Values.str ""
      "true", Values.bool true
      "array", Values.arr [ Values.exact 1m ]
      "object", Values.obj [ "a", Values.exact 1m ] ]

/// Assert `ArgumentTypeMismatch` for each non-number literal substituted for `{}`
/// in `template` (for example `"Absolute({})"`), and for each non-number `Input.v`.
let expectTypeMismatchForAll (template: string) (literals: string list) : unit =
    for lit in literals do
        let expr = template.Replace("{}", lit)
        expectRuntimeError MessageCode.ArgumentTypeMismatch (withFn expr)
    for (_, v) in badInputs do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch (template.Replace("{}", "Input.v"))

// -- Effects ------------------------------------------------------------------

let pureOptions = { Script.defaults with MaxEffect = Some FunctionEffect.Pure }

/// Assert `Output.r = call` compiles and runs under a Pure ceiling (returning
/// `expected`), that the call compiles inside a function body and a lambda
/// under the same ceiling, and that the ceiling refuses a NonDeterministic call.
let assertPure (call: string) (expected: float) : unit =
    let r = Script.runWith pureOptions ("Output.r = " + call)
    if not r.IsSuccess then
        Values.fail (sprintf "%s was refused or failed under a Pure ceiling:\n%s" call (Script.describe r))
    assertNumber expected (Script.output r |> Values.prop "r")
    let inFunc = Script.compileWith pureOptions (sprintf "func g()\n return %s\nend\nOutput.r = g()" call)
    if not inFunc.IsSuccess then
        Values.fail (sprintf "%s in a function body was refused under a Pure ceiling:\n%s" call (Script.describe inFunc))
    let inLambda = Script.compileWith pureOptions (sprintf "var h = (x) => %s\nOutput.r = 1" call)
    if not inLambda.IsSuccess then
        Values.fail (sprintf "%s in a lambda was refused under a Pure ceiling:\n%s" call (Script.describe inLambda))
    let control = Script.compileWith pureOptions "Output.r = RandomInt(1, 2)"
    if control.IsSuccess then Values.fail "Control: a Pure ceiling accepted RandomInt, so the ceiling is not enforced"
    Script.expectError MessageCode.FunctionEffectExceedsMax control

/// Assert `Output.r = call` compiles under a Pure ceiling.
let assertCompilesPure (call: string) : unit =
    let r = Script.compileWith pureOptions ("Output.r = " + call)
    if not r.IsSuccess then
        Values.fail (sprintf "%s was refused under a Pure ceiling:\n%s" call (Script.describe r))

/// Generous limits for loops of many iterations.
let bigLimits =
    Script.limits (fun o ->
        { o with
            MaxLoopIterations = 1_000_000
            MaxStatements = 10_000_000
            MaxExecutionTime = TimeSpan.FromSeconds 30. })

/// Run with generous loop and statement limits.
let runBig (pairs: (string * JyroValue) list) (source: string) : JyroResult<JyroValue> =
    Script.runWith { Script.defaults with Input = ValueInput (inputOf pairs); Limits = Some bigLimits } source

module RealTree.Jyro.Fuzz.Source.FuzzHarness

open System
open System.Diagnostics
open System.Text
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// IMPORTANT: This module must NOT have any top-level `let` bindings that
// instantiate Jyro types. The module cctor runs the first time anything in
// FuzzHarness is touched, which may be before SharpFuzz's libFuzzer driver
// has attached its shared-memory coverage buffer. Instrumented Jyro IL
// would then write to a null SHM pointer and crash with NRE. Keep all
// Jyro-typed values inside `run` so they're only constructed after the
// libFuzzer dispatch loop hands us an input (by which point SHM is live).

// 2s is well above JIT warmup on first invocation (seen at ~800ms)
// but comfortably below libFuzzer's own -timeout=10 ceiling.
let private wallClockBudget = TimeSpan.FromMilliseconds 2_000.0

let run (bytes: ReadOnlySpan<byte>) : unit =
    let utf8 = UTF8Encoding(false, false)
    let fuzzOptions : JyroExecutionOptions =
        { MaxExecutionTime  = TimeSpan.FromMilliseconds 250.0
          MaxStatements     = 2_000
          MaxLoopIterations = 500
          MaxCallDepth      = 32 }
#if CANARY
    // Canary-only: deterministic crash on a known magic prefix so the
    // pipeline (instrumentation + AFL + crash reporting) can be verified
    // end-to-end. Stripped from real-campaign builds.
    if bytes.Length >= 3
       && bytes.[0] = 0xDEuy
       && bytes.[1] = 0xADuy
       && bytes.[2] = 0xBEuy then
        failwith "canary: intentional crash to verify pipeline"
#endif
    let source = utf8.GetString(bytes)
    let sw = Stopwatch.StartNew()
    try
        // The compile budget is set explicitly. It used to ride in on
        // `WithExecutionOptions`, where `MaxExecutionTime` bounded compilation
        // as well as execution - and bounding compilation is most of what this
        // harness is for, since a hostile source hangs in the parser rather
        // than at runtime. Now that the two are separate, dropping it would
        // leave parse hangs to be caught by `wallClockBudget` below and
        // reported as crashes instead of diagnostics.
        let compiled =
            JyroCompiler()
                .WithStdlib()
                .WithSource(source)
                .WithMaxCompilationTime(fuzzOptions.MaxExecutionTime)
                .Compile()
        // Diagnostics are a valid outcome for a fuzzed input; only a hang or an
        // unexpected exception is a finding.
        match compiled.TryValue with
        | Some program ->
            JyroRunner(program)
                .WithExecutionOptions(fuzzOptions)
                .WithInput(JyroNull.Instance :> JyroValue)
                .Execute()
            |> ignore
        | None -> ()
        sw.Stop()
        if sw.Elapsed > wallClockBudget then
            raise (TimeoutException(
                sprintf "Hang: %dms exceeded %dms budget"
                    (int sw.ElapsedMilliseconds)
                    (int wallClockBudget.TotalMilliseconds)))
    with
    | :? OperationCanceledException -> ()
    | :? TimeoutException -> reraise ()

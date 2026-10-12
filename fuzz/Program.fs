module RealTree.Jyro.Fuzz.Source.Program

open System
open SharpFuzz

// No warmup here. Any call to FuzzHarness before Fuzzer.LibFuzzer.Run would
// trigger the module cctor and hit instrumented Jyro IL before SharpFuzz has
// attached its shared-memory coverage buffer, causing an NRE. The first real
// input from libFuzzer pays ~800ms JIT, which sits comfortably under the
// harness's 2s wall-clock budget.

[<EntryPoint>]
let main _argv =
    Fuzzer.LibFuzzer.Run(fun (data: ReadOnlySpan<byte>) ->
        FuzzHarness.run data)
    0

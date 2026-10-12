namespace RealTree.Jyro.Benchmarks

open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type ExecutionBenchmarks() =

    let mutable trivialCompiled = Unchecked.defaultof<CompiledProgram>
    let mutable mediumCompiled = Unchecked.defaultof<CompiledProgram>
    let mutable complexCompiled = Unchecked.defaultof<CompiledProgram>

    [<GlobalSetup>]
    member _.Setup() =
        trivialCompiled <- compileOrFail trivialScript
        mediumCompiled <- compileWithStdlibOrFail mediumScript
        complexCompiled <- compileWithStdlibOrFail complexScript

    [<Benchmark(Description = "Execute trivial (single assignment)", Baseline = true)>]
    member _.ExecuteTrivial() =
        Compiler.executeSimple trivialCompiled (emptyData ())

    [<Benchmark(Description = "Execute medium (functions + loops)")>]
    member _.ExecuteMedium() =
        Compiler.executeSimple mediumCompiled (emptyData ())

    [<Benchmark(Description = "Execute complex (order pipeline)")>]
    member _.ExecuteComplex() =
        Compiler.executeSimple complexCompiled (orderPipelineData ())

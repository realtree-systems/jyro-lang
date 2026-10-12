namespace RealTree.Jyro.Benchmarks

open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type PipelineBenchmarks() =

    let mutable pipelineBytes = Unchecked.defaultof<byte[]>

    [<GlobalSetup>]
    member _.Setup() =
        pipelineBytes <- compileToBytesOrFail complexScript

    [<Benchmark(Description = "E2E trivial: parse + compile + execute")>]
    member _.EndToEndTrivial() =
        JyroRunner(JyroCompiler().WithSource(trivialScript).CompileOrThrow())
            .WithInput(emptyData ())
            .Execute()

    [<Benchmark(Description = "E2E with stdlib: parse + compile + execute", Baseline = true)>]
    member _.EndToEndWithStdlib() =
        JyroRunner(JyroCompiler().WithStdlib().WithSource(complexScript).CompileOrThrow())
            .WithInput(orderPipelineData ())
            .Execute()

    [<Benchmark(Description = "E2E from .jyrx: deserialize + link + execute")>]
    member _.EndToEndFromJyrx() =
        JyroRunner(JyroArtifactLoader(pipelineBytes).WithStdlib().Load().GetValueOrThrow())
            .WithInput(orderPipelineData ())
            .Execute()

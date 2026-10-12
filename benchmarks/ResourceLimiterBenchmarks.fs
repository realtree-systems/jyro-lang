namespace RealTree.Jyro.Benchmarks

open System
open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type ResourceLimiterBenchmarks() =

    let mutable compiled = Unchecked.defaultof<CompiledProgram>

    let elevatedOptions =
        { JyroExecutionOptions.Default with
            MaxLoopIterations = 100_000
            MaxStatements = 100_000
            MaxExecutionTime = TimeSpan.FromSeconds(30.0) }

    [<Params(100, 1000)>]
    member val Iterations = 0 with get, set

    [<GlobalSetup>]
    member _.Setup() =
        compiled <- compileOrFail loopScript

    [<Benchmark(Description = "Loop without limiter", Baseline = true)>]
    member this.WithoutLimiter() =
        Compiler.executeSimple compiled (loopData this.Iterations)

    [<Benchmark(Description = "Loop with limiter (statement + loop counting)")>]
    member this.WithLimiter() =
        let ctx = JyroExecutionContext(JyroResourceLimiter(elevatedOptions))
        Compiler.execute compiled (loopData this.Iterations) ctx

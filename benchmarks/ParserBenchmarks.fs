namespace RealTree.Jyro.Benchmarks

open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type ParserBenchmarks() =

    [<Benchmark(Description = "Parse trivial (single assignment)", Baseline = true)>]
    member _.ParseTrivial() =
        box (Parser.parse trivialScript)

    [<Benchmark(Description = "Parse medium (functions + loops)")>]
    member _.ParseMedium() =
        box (Parser.parse mediumScript)

    [<Benchmark(Description = "Parse complex (order pipeline)")>]
    member _.ParseComplex() =
        box (Parser.parse complexScript)

namespace RealTree.Jyro.Benchmarks

open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type CompilationBenchmarks() =

    let noFunctions: IJyroFunction list = []
    let stdlibFunctions = StdlibRegistry.getAll ()

    [<Benchmark(Description = "Compile trivial (no stdlib)")>]
    member _.CompileNoStdlib() =
        Compiler.compileFromSource trivialScript noFunctions [] FunctionEffect.Mutating FunctionEffect.NonDeterministic

    [<Benchmark(Description = "Compile medium (with stdlib)")>]
    member _.CompileWithStdlib() =
        Compiler.compileFromSource mediumScript stdlibFunctions [] FunctionEffect.Mutating FunctionEffect.NonDeterministic

    [<Benchmark(Description = "Compile complex (order pipeline)", Baseline = true)>]
    member _.CompileComplex() =
        Compiler.compileFromSource complexScript stdlibFunctions [] FunctionEffect.Mutating FunctionEffect.NonDeterministic

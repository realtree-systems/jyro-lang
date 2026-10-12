namespace RealTree.Jyro.Benchmarks

open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type BinaryFormatBenchmarks() =

    let stdlibFunctions = StdlibRegistry.getAll ()
    let mutable jyrxBytes = Unchecked.defaultof<byte[]>

    [<GlobalSetup>]
    member _.Setup() =
        jyrxBytes <- compileToBytesOrFail complexScript

    [<Benchmark(Description = "Serialize to .jyrx")>]
    member _.SerializeToJyrx() =
        Compiler.compileToJyrx complexScript stdlibFunctions FunctionEffect.Mutating

    [<Benchmark(Description = "Compile from .jyrx (deserialize + link)")>]
    member _.CompileFromJyrx() =
        Compiler.compileFromJyrx jyrxBytes stdlibFunctions FunctionEffect.Mutating

    [<Benchmark(Description = "Compile from source (full pipeline)", Baseline = true)>]
    member _.CompileFromSource() =
        Compiler.compileFromSource complexScript stdlibFunctions [] FunctionEffect.Mutating FunctionEffect.NonDeterministic

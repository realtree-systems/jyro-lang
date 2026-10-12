namespace RealTree.Jyro.Benchmarks

open BenchmarkDotNet.Attributes
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Benchmarks.BenchmarkHelpers

[<MemoryDiagnoser>]
type StdlibBenchmarks() =

    let mutable arrayCompiled = Unchecked.defaultof<CompiledProgram>
    let mutable stringCompiled = Unchecked.defaultof<CompiledProgram>

    [<Params(100, 1000)>]
    member val N = 0 with get, set

    [<GlobalSetup>]
    member _.Setup() =
        arrayCompiled <- compileWithStdlibOrFail stdlibArrayScript
        stringCompiled <- compileWithStdlibOrFail stdlibStringScript

    [<Benchmark(Description = "Map + Where + Sort + Sum (array)", Baseline = true)>]
    member this.ArrayMapWhere() =
        Compiler.executeSimple arrayCompiled (arrayData this.N)

    [<Benchmark(Description = "Split + Trim + ToUpper + Join (string)")>]
    member this.StringOperations() =
        Compiler.executeSimple stringCompiled (stringData this.N)

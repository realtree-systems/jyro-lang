module RealTree.Jyro.Benchmarks.Program

open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
    BenchmarkSwitcher
        .FromAssembly(typeof<ParserBenchmarks>.Assembly)
        .Run(args)
    |> ignore
    0

/// The entry point of this test assembly. The test runner does not use it; the
/// child processes of Properties/StackBudget.fs do:
/// `dotnet exec <assembly> stack <scenario>`.
module RealTree.Jyro.Tests.Program

[<EntryPoint>]
let main argv =
    match argv with
    | [| "stack"; scenario |] ->
        try RealTree.Jyro.Tests.Runtime.StackBudget.runChild scenario
        with e ->
            eprintfn "%O" e
            2
    | _ -> 0

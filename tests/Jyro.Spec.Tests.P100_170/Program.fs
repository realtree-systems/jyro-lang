/// The entry point of this test assembly. The test runner does not use it;
/// the child processes of Child.fs do: `dotnet exec <assembly> child <scenario>`.
module RealTree.Jyro.Spec.Tests.Program

[<EntryPoint>]
let main argv =
    match argv with
    | [| "child"; scenario |] ->
        try RealTree.Jyro.Spec.Tests.Child.runScenario scenario
        with e ->
            eprintfn "%O" e
            2
    | _ -> 0

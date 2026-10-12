/// The entry point of the test assembly. xUnit ignores it; the tests of this
/// project start the assembly as a child process with `validate <data> <schema>`
/// to run a ValidateSchema call that might take the whole process down (a stack
/// overflow cannot be caught in .NET), so that such a failure fails one test
/// instead of crashing the test host.
module RealTree.Jyro.Spec.Tests.Program

open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

[<EntryPoint>]
let main argv =
    match argv with
    | [| "validate"; data; schema |] ->
        let res = validateRaw data schema
        if Script.ok res then
            printfn "OK %s" (Values.toJson (Script.output res |> RealTree.Jyro.Spec.Values.prop "r"))
        else
            printfn "ERR %s" (Script.errorCodes res |> List.map string |> String.concat ",")
        0
    | _ -> 0

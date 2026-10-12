/// Part 100: the default execution time limit, on every entry path. Each test
/// takes about five seconds, so they are kept apart from the fast tests.
module RealTree.Jyro.Spec.Tests.S100_02_TimeDefaults

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

[<Theory>]
[<InlineData("Runner")>]
[<InlineData("ExecuteScript")>]
[<InlineData("ExecuteScriptWithJson")>]
[<InlineData("CompiledBytes")>]
[<Trait("requirement", "JYRO-REQ-1404")>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``EveryEntryPath_WhenTimePassesTheDefault_StopsWithExecutionTimeLimitExceeded`` (path: string) =
    let r, elapsed = timed (fun () -> runOnPath (pathNamed path) "Sleep(6000)\nOutput.after = 1")
    fails MessageCode.ExecutionTimeLimitExceeded r
    Assert.False(hasProp "after" (Script.valueOf r))
    Assert.True(elapsed >= TimeSpan.FromSeconds 4.9, sprintf "stopped after %O, before the 5 s default" elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultRunner_WorkWithinFiveSeconds_Succeeds`` () =
    Script.run "Sleep(4500)\nOutput.after = 1" |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1826")>]
let ``JYRO-REQ-1826 Unlimited_RemovesTheTimeLimit`` () =
    let o = { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited }
    Script.runWith o "Sleep(5300)\nOutput.after = 1" |> ok |> ignore

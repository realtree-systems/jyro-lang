/// 100.7: a walk made at the deepest call does not exhaust the host stack (JYRO-REQ-1725). Every
/// scenario is in a child process (Child.fs, DeepWalkCases.fs), so a stack overflow fails the test
/// and never ends the test host.
module RealTree.Jyro.Spec.Tests.S100_07_DeepestCall

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let walkNames : obj[] seq = DeepWalkCases.walks |> Seq.map (fun w -> [| box w.Name |])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1725")>]
let ``JYRO-REQ-1725 The maximum nesting depth and the safe maximum of the call depth are the ones the scenarios use`` () =
    // The scenarios read both from the implementation; the clause is about their sum, so they
    // must be real numbers: a nesting maximum of at least 64 (100.7.1) and a safe call depth above 0.
    Assert.True(maxDepth () >= 64, sprintf "the value nesting maximum is %d" (maxDepth ()))
    Assert.True(maxSafeCallDepth () > 0, sprintf "MaxSafeCallDepth is %d" (maxSafeCallDepth ()))

[<Theory>]
[<MemberData(nameof walkNames)>]
[<Trait("requirement", "JYRO-REQ-1725")>]
[<Trait("requirement", "JYRO-REQ-2047")>]
let ``JYRO-REQ-1725 A walk at the maximum depth made in the deepest call gives what it gives at the top level, and the host keeps running`` (walk: string) =
    let o = Child.runWith 180_000 [] (sprintf "dw|%s|1024" walk)
    Child.expectSuccess o

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1725")>]
let ``JYRO-REQ-1725 The clause's example, ToJson(v) and v == w at the deepest call, gives the same text and true`` () =
    // The same two walks as above, named by the clause; a child that did not finish normally
    // (a stack overflow ends it) fails with the child's output.
    for walk in [ "tojson"; "equal" ] do
        let o = Child.runWith 180_000 [] (sprintf "dw|%s|1024" walk)
        Child.expectSuccess o

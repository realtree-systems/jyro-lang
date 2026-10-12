/// 100.2.5 (an operator that iterates checks the time and the cancellation
/// signal during its iteration) and 100.4.3 (a time limit that ends inside an
/// operator is ExecutionTimeLimitExceeded, not CanceledByHost), for the
/// operators the clause names: `==` and `!=` on two arrays and on two objects,
/// `&`, and `<`, `<=`, `>`, `>=` on two strings.
///
/// The walks use the book's shared tree (`a = [a, a]` 30 times, 100.8.6): 31
/// arrays and 2^30 paths, so an unchecked `a == b` walks for minutes while its
/// memory stays under a kilobyte of containers. Each run is in a child process
/// (Child.fs, ValueSize.fs), killed after 20 s, so a build that does not check
/// fails the test and does not hold the test host. Every run sets all four
/// limits (FLOORS 4.1.11): the time limit of the case, statements and loop
/// iterations at Int32.MaxValue, call depth 64.
module RealTree.Jyro.Spec.Tests.S100_02_OperatorChecks

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.ValueSize

/// A child heap limit of 2 GB, below the test host's.
let private childEnv = [ "DOTNET_GCHeapHardLimit", "80000000" ]

let private runOp (kind: string) (mode: string) : Child.ChildOutcome =
    Child.runWith 20_000 childEnv (sprintf "vs|op|%s|%s" kind mode)

/// Assert the child's run failed with exactly `code`, stage Execute, within `ms`
/// milliseconds of the start of Execute().
let private stopsWith (code: MessageCode) (withinMs: int) (o: Child.ChildOutcome) : unit =
    Child.expectCode code o
    let result = o.Result.Value
    // The stage (160.1.12) is asserted where the build has DiagnosticMessage.Stage (160.7.5).
    if hasStage () then Assert.Equal(Some "Execute", field "stage" result)
    let ms = field "ms" result |> Option.map int |> Option.defaultValue System.Int32.MaxValue
    Assert.True(ms <= withinMs, sprintf "The run stopped after %d ms; the bound is %d ms. %s" ms withinMs o.Describe)

/// Every operator kind the clause names. The walks: `a == b`, `a != b` on two
/// shared trees of 30 array doublings, `o == p`, `o != p` on two of 30 object
/// doublings. The copies and comparisons: a loop `x = Input.s & Input.t` with
/// two strings of M/2 code units, and a loop `x = Input.s < Input.t` (and the
/// other three) with two distinct equal strings of M code units.
let operatorKinds : obj[] seq =
    [ "eq-array"; "ne-array"; "eq-object"; "ne-object"; "concat"; "lt"; "le"; "gt"; "ge" ]
    |> Seq.map (fun k -> [| box k |])

// -- 100.2.5 --------------------------------------------------------------------------------

[<Theory>]
[<MemberData(nameof operatorKinds)>]
[<Trait("requirement", "JYRO-REQ-1411")>]
let ``Operator_WhenTheTimeLimitEndsDuringIt_StopsWithExecutionTimeLimitExceededWithin2Seconds`` (kind: string) =
    // Time limit 1 s, no cancellation token: stopped within 2 s of the limit.
    runOp kind "time" |> stopsWith MessageCode.ExecutionTimeLimitExceeded 3_000

[<Theory>]
[<MemberData(nameof operatorKinds)>]
[<Trait("requirement", "JYRO-REQ-1411")>]
let ``Operator_WhenTheHostCancelsDuringIt_StopsWithCanceledByHostWithin2Seconds`` (kind: string) =
    // Time limit 10 min, a cancel at 500 ms from a thread of its own: stopped within 2 s of the cancel.
    runOp kind "cancel" |> stopsWith MessageCode.CanceledByHost 2_500

// -- 100.4.3 --------------------------------------------------------------------------------

let timeTokenKinds : obj[] seq =
    [ "eq-array"; "ne-array"; "eq-object"; "concat"; "lt" ] |> Seq.map (fun k -> [| box k |])

[<Theory>]
[<MemberData(nameof timeTokenKinds)>]
[<Trait("requirement", "JYRO-REQ-1424")>]
let ``TimeLimitEndsInsideAnOperator_WithAHostTokenNeverCanceled_IsExecutionTimeLimitExceeded_NotCanceledByHost`` (kind: string) =
    // The host gave a cancellation token (never canceled), so an implementation
    // that uses one internal cancellation for both conditions would be seen.
    let o = runOp kind "timetoken"
    Child.expectClean o
    Assert.DoesNotContain(string MessageCode.CanceledByHost, o.Codes)
    o |> stopsWith MessageCode.ExecutionTimeLimitExceeded 3_000

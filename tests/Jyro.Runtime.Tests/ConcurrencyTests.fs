module RealTree.Jyro.Tests.ConcurrencyTests

open System.Collections.Concurrent
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

// Concurrency regression tests for state that is shared across executions.
//
// Jyro runs a single execution on one thread, but the host is expected to
// reuse one compiled program across concurrently executing requests - see
// the "Threading and Concurrency" section of the Jyro.Api README. This
// exercises the mutable shared state that must survive that pattern: the
// .jyrx deserialization recursion-depth guard, which must be isolated per
// call rather than held in module state.

/// Build a moderately deep nested expression so each deserialize recurses far
/// enough to make a shared depth counter observably wrong under concurrency,
/// while staying well under the binary format's max nesting (200).
let private nestedExpr (depth: int) : string =
    let rec build n = if n <= 0 then "1" else sprintf "(%s + 1)" (build (n - 1))
    build depth

[<Fact>]
let ``concurrent .jyrx deserialization does not corrupt the depth guard`` () =
    let source =
        sprintf "Output.a = %s\nOutput.b = %s" (nestedExpr 40) (nestedExpr 40)

    let bytesResult =
        JyroCompiler().WithStdlib().WithSource(source).CompileToArtifact()

    Assert.True(bytesResult.IsSuccess, "setup: script failed to compile to .jyrx")
    let bytes = Option.get bytesResult.TryValue

    // Baseline: a single-threaded deserialize succeeds and yields both stmts.
    let baseline = BinaryFormat.deserialize bytes
    Assert.True(baseline.IsSuccess)
    let expectedStmtCount =
        match baseline.TryValue with
        | Some p -> List.length p.Program.Statements
        | None -> -1
    Assert.Equal(2, expectedStmtCount)

    let threadCount = 24
    let perThread = 8
    let outcomes = ConcurrentBag<Result<int, string>>()

    let tasks =
        Array.init threadCount (fun _ ->
            Task.Run(fun () ->
                for _ in 1..perThread do
                    let r = BinaryFormat.deserialize bytes
                    let outcome =
                        if r.IsSuccess then
                            match r.TryValue with
                            | Some p -> Ok(List.length p.Program.Statements)
                            | None -> Error "success with no program"
                        else
                            Error(List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message) |> String.concat "; ")
                    outcomes.Add(outcome)))

    Task.WaitAll(tasks)

    Assert.Equal(threadCount * perThread, outcomes.Count)
    // If the depth guard were shared module state, interleaved recursion would
    // spuriously trip "AST nesting depth exceeds maximum"; per-call state makes
    // every deserialize succeed with the exact statement count.
    let firstError =
        outcomes |> Seq.tryPick (function Error e -> Some e | Ok _ -> None)
    Assert.True(firstError.IsNone, sprintf "a concurrent deserialize failed: %A" firstError)
    Assert.True(
        outcomes |> Seq.forall (function Ok n -> n = expectedStmtCount | Error _ -> false),
        "a concurrent deserialize returned the wrong statement count")

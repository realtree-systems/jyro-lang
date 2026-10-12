namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.RuntimeGenerators

/// Evaluator determinism: running the same source against the same input
/// must produce the same output twice in a row.
module Determinism =

    /// Deep-structural equality via JSON serialization.
    let private equivalent (a: JyroValue) (b: JyroValue) =
        let aj = if isNull (box a) then "null" else a.ToJson()
        let bj = if isNull (box b) then "null" else b.ToJson()
        aj = bj

    let private runTwice (source: string) (input: JyroValue) : bool =
        // Deep-clone input per run so the second run doesn't see state
        // mutated by the first.
        let clone () =
            if isNull (box input) then input
            else JyroValue.FromJson(input.ToJson())
        let r1 = Runner.run source (clone ())
        let r2 = Runner.run source (clone ())
        r1.IsSuccess = r2.IsSuccess
        && (not r1.IsSuccess || equivalent r1.TryValue.Value r2.TryValue.Value)

    [<Theory>]
    [<InlineData("Output.x = 1 + 2")>]
    [<InlineData("var xs = [3, 1, 2]\nOutput.sorted = Sort(xs)")>]
    [<InlineData("var xs = [1, 2, 3, 4, 5]\nOutput.mapped = Map(xs, x => x * 2)")>]
    [<InlineData("var acc = 0\nfor i in 1 to 10 do\n  acc += i\nend\nOutput.total = acc")>]
    [<InlineData("var s = \"hello\"\nOutput.length = Length(s)")>]
    [<InlineData("var o = {a: 1, b: 2, c: 3}\nOutput.keys = Keys(o)")>]
    let ``same source + same input produces identical output twice`` (src: string) =
        let input = JyroObject() :> JyroValue
        // Confirm the script actually executes - a silent failure that
        // falsifies on both runs would trivially "pass" determinism.
        let r = Runner.run src input
        Assert.True(
            r.IsSuccess,
            sprintf
                "Precondition failed: script should execute successfully.\nDiagnostics: %A\nSource:\n%s"
                (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> (m.Code, m.Message)))
                src)
        Assert.True(runTwice src input, "Non-determinism detected")

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Sort is deterministic on random arrays`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let arr = JyroArray()
        for x in finite do arr.Add(JyroNumber(x))
        let input =
            let o = JyroObject()
            o.SetProperty("xs", arr)
            o :> JyroValue
        runTwice "Output.sorted = Sort(Input.xs)" input

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Map + Where pipeline is deterministic`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let arr = JyroArray()
        for x in finite do arr.Add(JyroNumber(x))
        let input =
            let o = JyroObject()
            o.SetProperty("xs", arr)
            o :> JyroValue
        runTwice "Output.out = Map(Where(Input.xs, x => x > 0), x => x * 2)" input

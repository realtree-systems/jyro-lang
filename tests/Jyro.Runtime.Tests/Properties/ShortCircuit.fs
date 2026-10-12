namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// Short-circuit semantics for `and` / `or` / `??` / ternary: the right
/// operand must not be evaluated once the left determines the result.
///
/// Technique: use `Output.counter++` as the side effect in the right
/// operand and assert the counter's final value.
module ShortCircuit =

    let private runWithCounter (expr: string) =
        let src =
            sprintf "Output.counter = 0\nOutput.result = %s" expr
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        let counter = Runner.prop "counter" output |> Runner.asNumber
        let result = Runner.prop "result" output
        (counter, result)

    /// A non-null expression that bumps `Output.counter` when evaluated.
    let private bumpRhs = "Output.counter++ >= 0"

    // -- and --------------------------------------------------------------

    [<Fact>]
    let ``false and rhs does not evaluate rhs`` () =
        let counter, _ = runWithCounter (sprintf "false and (%s)" bumpRhs)
        Assert.Equal(0.0, counter)

    [<Fact>]
    let ``true and rhs evaluates rhs`` () =
        let counter, _ = runWithCounter (sprintf "true and (%s)" bumpRhs)
        Assert.Equal(1.0, counter)

    // -- or ---------------------------------------------------------------

    [<Fact>]
    let ``true or rhs does not evaluate rhs`` () =
        let counter, _ = runWithCounter (sprintf "true or (%s)" bumpRhs)
        Assert.Equal(0.0, counter)

    [<Fact>]
    let ``false or rhs evaluates rhs`` () =
        let counter, _ = runWithCounter (sprintf "false or (%s)" bumpRhs)
        Assert.Equal(1.0, counter)

    // -- ?? (null coalescing) --------------------------------------------

    [<Fact>]
    let ``non-null ?? rhs does not evaluate rhs`` () =
        let counter, _ = runWithCounter (sprintf "\"hello\" ?? (%s)" bumpRhs)
        Assert.Equal(0.0, counter)

    [<Fact>]
    let ``null ?? rhs evaluates rhs`` () =
        let counter, _ = runWithCounter (sprintf "null ?? (%s)" bumpRhs)
        Assert.Equal(1.0, counter)

    // -- Ternary ----------------------------------------------------------

    [<Fact>]
    let ``ternary evaluates only the chosen branch (true path)`` () =
        let src = """
Output.lhs = 0
Output.rhs = 0
Output.r = true ? Output.lhs++ : Output.rhs++"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "lhs" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "rhs" output |> Runner.asNumber)

    [<Fact>]
    let ``ternary evaluates only the chosen branch (false path)`` () =
        let src = """
Output.lhs = 0
Output.rhs = 0
Output.r = false ? Output.lhs++ : Output.rhs++"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "lhs" output |> Runner.asNumber)
        Assert.Equal(1.0, Runner.prop "rhs" output |> Runner.asNumber)

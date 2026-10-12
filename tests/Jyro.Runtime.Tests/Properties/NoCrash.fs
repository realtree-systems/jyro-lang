namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// Robustness property: executing a well-formed Jyro script must never
/// raise an unhandled .NET exception. Every outcome should be a
/// `JyroResult` - either `IsSuccess = true` with an output, or
/// `IsSuccess = false` with diagnostic messages.
module NoCrash =

    let private crashesWith (source: string) (input: JyroValue) =
        try
            let _ = Runner.run source input
            None
        with ex -> Some(ex.GetType().Name, ex.Message)

    let private mustNotCrash (source: string) =
        let input = JyroObject() :> JyroValue
        match crashesWith source input with
        | None -> ()
        | Some(kind, msg) ->
            Assert.Fail(sprintf "Unhandled exception %s:\n  %s\nSource:\n%s" kind msg source)

    // A small library of programs known to probe error paths. These all
    // must return a `JyroResult` - never throw.
    [<Theory>]
    [<InlineData("var x = 1 / 0")>]
    [<InlineData("var x = \"a\" + 1")>]
    [<InlineData("var x = Data.missing.deeper.still")>]
    [<InlineData("var xs = [1, 2, 3]\nvar x = xs[999]")>]
    [<InlineData("var xs = [1, 2, 3]\nvar x = xs[-1]")>]
    [<InlineData("var x = null\nvar y = x.prop")>]
    [<InlineData("var x = null\nvar y = x[0]")>]
    [<InlineData("var x = 0\nvar y = 5 % x")>]
    [<InlineData("var xs = []\nvar x = xs[0]")>]
    [<InlineData("fail \"boom\"")>]
    [<InlineData("exit \"done\"")>]
    let ``hand-picked error-path programs return JyroResult (never throw)`` (src: string) =
        mustNotCrash src

    [<Fact>]
    let ``infinite loop terminates cleanly via execution limit`` () =
        // The default JyroExecutionOptions caps statements and loop iterations.
        // A while-true without progress must end as a StatementLimitExceeded or
        // LoopIterationLimitExceeded failure, never hang or throw.
        let src = "while true do\n  var x = 1\nend"
        mustNotCrash src

    [<Fact>]
    let ``unbounded recursion is rejected with CallDepthLimitExceeded`` () =
        let src = """
func recurse(n)
  return recurse(n + 1)
end
recurse(0)"""
        let r = Runner.run src (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected call-depth limit to trip")
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.CallDepthLimitExceeded)

    [<Fact>]
    let ``running an empty script succeeds`` () =
        let r = Runner.runEmpty ""
        Assert.True(r.IsSuccess)

    // ---- Regression tests from fuzz findings ----

    [<Fact>]
    let ``fuzz-regression: deeply-backtracking parser input does not stack-overflow the host`` () =
        // libfuzzer-dotnet finding 2026-04-24:
        // A 1678-byte input with an unclosed `switch` and a 105-digit hex
        // literal drove FParsec's combinators into deep error-recovery
        // backtracking that exhausted the CLR's default 1 MiB managed
        // stack and terminated the host process with an uncatchable
        // StackOverflowException. JyroCompiler.Compile() now runs the
        // parser on a worker thread with a 16 MiB stack; this input
        // must now return a JyroResult (either success or diagnostics),
        // never crash the host.
        let src =
            String.concat "\n" [
                "var day = \"Monday\""
                "switch day do"
                "    case \"Monday\" then"
                "        Output.r = 1"
                // unclosed switch (no `end`)
                "var status = \"x\""
                "switch status do"
                "    case \"active\" then"
                "        Output.x = 1"
                // 105-digit hex literal
                "Output.hexZero = 0x7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777770"
                "    default then"
                "        Output.r3 = false"
                "end"
            ]
        mustNotCrash src

    [<Fact>]
    let ``fuzz-regression: unclosed-paren avalanche hits compile timeout rather than hanging`` () =
        // libfuzzer-dotnet finding 2026-04-24:
        // A ~488-byte input of almost-entirely unclosed `(` characters drove
        // FParsec's error-recovery into a multi-second loop with no internal
        // checkpoint. Compilation runs on a worker thread bounded by
        // `WithMaxCompilationTime`; hostile inputs that exceed it surface as
        // CompilationTimeLimitExceeded diagnostics instead of hanging the host.
        //
        // The budget is set on the compiler because that is the only thing it
        // bounds. It used to be `WithExecutionOptions(MaxExecutionTime = 200ms)`
        // on a builder that both compiled and ran, where one field silently did
        // both jobs - so a test about compilation was written in terms of an
        // execution limit.
        //
        // Force a tight 200ms budget so the test completes quickly even if
        // the parser takes longer on slow machines.
        let parens = String.replicate 500 "(("
        let src = parens + "Fa(duAetor(-4" + parens
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let r =
            Runner.compileThenRun
                (JyroCompiler()
                    .WithStdlib()
                    .WithSource(src)
                    .WithMaxCompilationTime(System.TimeSpan.FromMilliseconds 200.0))
                (fun r -> r.WithInput(JyroObject() :> JyroValue))
        sw.Stop()
        // Wall clock must be bounded even if parser would otherwise thrash
        // for seconds. Allow 3x the configured budget for CI slack.
        Assert.True(
            sw.Elapsed.TotalMilliseconds < 3_000.0,
            sprintf "Wall clock %.0fms exceeded 3s ceiling - compile timeout not firing"
                sw.Elapsed.TotalMilliseconds)
        Assert.False(r.IsSuccess)
        // Either parse failure or ExecutionTimeLimitExceeded is acceptable
        // as long as we got a JyroResult and didn't hang.
        Assert.NotEmpty(r.Messages)

    [<Fact>]
    let ``fuzz-regression: duplicate union name with match on stale variant surfaces as diagnostic`` () =
        // libfuzzer-dotnet finding 2026-04-24:
        // Two `union Shape` declarations overwrote DefinedUnions["Shape"] but
        // left VariantToUnion["Rect"] pointing at the now-stale union, causing
        // List.find in the Match validator to throw KeyNotFoundException.
        // Must now surface as DuplicateUnionName diagnostic and not throw.
        let src = """
union Shape
    Circle(radius: number)
    Rect(width: number, height: number)
end

union Shape
    Circle(radius: number)
    Other(x: number)
end

func Area(shape)
    return match shape do
        case Circle(r) then 3.14159 * r * r
        case Rect(w, h) then w * h
    end
end

Output.circleArea = Area(Circle(10))
"""
        let r = Runner.run src (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected duplicate-union-name diagnostic")
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.DuplicateUnionName)

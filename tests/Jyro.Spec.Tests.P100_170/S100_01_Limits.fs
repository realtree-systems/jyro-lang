/// Part 100: the four execution limits, where they are checked, how a breach
/// halts, cancellation, removal of limits and the compilation time limit.
module RealTree.Jyro.Spec.Tests.S100_01_Limits

open System
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// A script that runs exactly `n` statements. Beyond 100 it runs most of them
/// in the body of a function called many times, so that the source stays
/// short (the count does not depend on how the statements are laid out).
let private statements (n: int) =
    let calls = n / 101
    let rest = n - calls * 101
    let body = [ "func F()" ] @ repeatLines 99 "  var a{i} = {i}" @ [ "  return 0"; "end" ]
    if calls = 0 then lines (repeatLines n "Output.s = {i}")
    else lines (body @ repeatLines calls "Output.c{i} = F()" @ repeatLines rest "Output.s{i} = {i}")

let private whileLoop (n: int) = sprintf "var i = 0\nwhile i < %d do i = i + 1 end" n

// ---------------------------------------------------------------------------------
// 100.1.1 Four limits on every execution, on every entry path
// ---------------------------------------------------------------------------------

[<Theory>]
[<InlineData("Runner")>]
[<InlineData("ExecuteScript")>]
[<InlineData("ExecuteScriptWithJson")>]
[<InlineData("CompiledBytes")>]
[<Trait("requirement", "JYRO-REQ-1404")>]
let ``EveryEntryPath_WhenStatementsPassTheDefault_StopsWithStatementLimitExceeded`` (path: string) =
    runOnPath (pathNamed path) (statements 10_001) |> fails MessageCode.StatementLimitExceeded

[<Theory>]
[<InlineData("Runner")>]
[<InlineData("ExecuteScript")>]
[<InlineData("ExecuteScriptWithJson")>]
[<InlineData("CompiledBytes")>]
[<Trait("requirement", "JYRO-REQ-1404")>]
let ``EveryEntryPath_WhenLoopPassesTheDefault_StopsWithLoopIterationLimitExceeded`` (path: string) =
    runOnPath (pathNamed path) (whileLoop 1_001) |> fails MessageCode.LoopIterationLimitExceeded

[<Theory>]
[<InlineData("Runner")>]
[<InlineData("ExecuteScript")>]
[<InlineData("ExecuteScriptWithJson")>]
[<InlineData("CompiledBytes")>]
[<Trait("requirement", "JYRO-REQ-1404")>]
let ``EveryEntryPath_WhenCallsNestPastTheDefault_StopsWithCallDepthLimitExceeded`` (path: string) =
    runOnPath (pathNamed path) (recursion 65) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1404")>]
let ``Runner_WithOptionsSet_EnforcesEachOfTheFourLimits`` () =
    let opts f = withLimits (fun o -> f (roomy o))
    Script.runWith (opts (fun o -> { o with MaxStatements = 3 })) (statements 4)
    |> fails MessageCode.StatementLimitExceeded
    Script.runWith (opts (fun o -> { o with MaxLoopIterations = 3 })) (whileLoop 4)
    |> fails MessageCode.LoopIterationLimitExceeded
    Script.runWith (opts (fun o -> { o with MaxCallDepth = 3 })) (recursion 4)
    |> fails MessageCode.CallDepthLimitExceeded
    let s = stall ()
    Script.runWith
        { opts (fun o -> { o with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with Functions = [ s.AsFunction ] }
        "Stall(300)\nOutput.after = 1"
    |> fails MessageCode.ExecutionTimeLimitExceeded

// ---------------------------------------------------------------------------------
// 100.1.2 Defaults
// ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultOptions_Report5Seconds10000Statements1000Iterations64Calls`` () =
    let d = JyroExecutionOptions.Default
    Assert.Equal(TimeSpan.FromSeconds 5., d.MaxExecutionTime)
    Assert.Equal(10_000, d.MaxStatements)
    Assert.Equal(1_000, d.MaxLoopIterations)
    Assert.Equal(64, d.MaxCallDepth)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultRunner_10000Statements_Succeeds`` () =
    Script.run (statements 10_000) |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultRunner_10001Statements_FailsWithStatementLimitExceeded`` () =
    Script.run (statements 10_001) |> fails MessageCode.StatementLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultRunner_1000Iterations_Succeeds_And1001Fail`` () =
    Script.run (whileLoop 1_000) |> ok |> ignore
    Script.run (whileLoop 1_001) |> fails MessageCode.LoopIterationLimitExceeded
    Script.run ("foreach x in " + arrayLiteral 1000 + " do Output.x = x end") |> ok |> ignore
    Script.run ("foreach x in " + arrayLiteral 1001 + " do Output.x = x end") |> fails MessageCode.LoopIterationLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultRunner_64NestedCalls_Succeed_And65Fail`` () =
    Script.run (recursion 64) |> ok |> ignore
    Script.run (recursion 65) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``DefaultOneCallPath_64NestedCalls_Succeed_And65Fail`` () =
    runOnPath ExecuteScript (recursion 64) |> ok |> ignore
    runOnPath ExecuteScript (recursion 65) |> fails MessageCode.CallDepthLimitExceeded
    runOnPath ExecuteScript (statements 10_000) |> ok |> ignore

// ---------------------------------------------------------------------------------
// 100.1.3 Only the four limits and the compilation time limit are settable
// ---------------------------------------------------------------------------------

/// Every public member of the four API assemblies that a host can use to set
/// something whose name reads as a bound (Max, Limit, Timeout, Depth, Nesting).
let private settableBounds () : string list =
    let assemblies =
        [ typeof<JyroValue>.Assembly; typeof<CompiledProgram>.Assembly
          System.Reflection.Assembly.Load "Jyro.Stdlib"; typeof<JyroCompiler>.Assembly ]
        |> List.distinct
    let boundWord (n: string) =
        [ "Max"; "Limit"; "Timeout"; "Depth"; "Nesting"; "Ceiling" ] |> List.exists (fun w -> n.Contains w)
    [ for a in assemblies do
        for t in a.GetExportedTypes() do
            // Statistics and function signatures are read-outs and descriptions, not bounds.
            if t <> typeof<JyroPipelineStats> && t <> typeof<JyroFunctionSignature> then
                for p in t.GetProperties() do
                    if p.CanWrite && not (isNull (p.GetSetMethod())) && boundWord p.Name then
                        yield sprintf "%s.%s" t.Name p.Name
                for f in t.GetFields() do
                    if not f.IsInitOnly && not f.IsLiteral && boundWord f.Name then
                        yield sprintf "%s.%s" t.Name f.Name
                for m in t.GetMethods() do
                    if (m.Name.StartsWith "With" || m.Name.StartsWith "Set") && boundWord m.Name then
                        yield sprintf "%s.%s" t.Name m.Name ]
    |> List.distinct
    |> List.sort

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1406")>]
let ``PublicApi_SettableBounds_AreOnlyTheFourLimitsTheCompilationTimeAndTheRegexTimeout`` () =
    let allowed =
        set [ "JyroExecutionOptions.MaxExecutionTime"; "JyroExecutionOptions.MaxStatements"
              "JyroExecutionOptions.MaxLoopIterations"; "JyroExecutionOptions.MaxCallDepth"
              "JyroRunner.WithMaxExecutionTime"; "JyroRunner.WithMaxStatements"
              "JyroRunner.WithMaxLoopIterations"; "JyroRunner.WithMaxCallDepth"
              "JyroCompiler.WithMaxCompilationTime"; "JyroArtifactLoader.WithMaxCompilationTime"
              "JyroRunner.WithMaxRegexTime"; "JyroExecutionOptions.MaxRegexTime"
              // Effect ceilings are capabilities of 120.5, not bounds of Part 100.
              "JyroCompiler.WithMaxEffect"; "JyroCompiler.WithLibraryMaxEffect"; "JyroArtifactLoader.WithMaxEffect" ]
    let extra = settableBounds () |> List.filter (fun n -> not (allowed.Contains n))
    Assert.True(extra.IsEmpty, sprintf "Settable bounds outside 100.1.3: %A" extra)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1406")>]
let ``PublicApi_CallDepthCeilingAndNestingBound_HaveNoSetter`` () =
    let safe = typeof<JyroExecutionOptions>.GetProperty("MaxSafeCallDepth")
    Assert.NotNull safe
    Assert.Null(safe.GetSetMethod())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1406")>]
let ``SettingEachLimit_ChangesTheOutcomeAtItsBound`` () =
    let opts f = withLimits (fun o -> f (roomy o))
    Script.runWith (opts (fun o -> { o with MaxStatements = 7 })) (statements 7) |> ok |> ignore
    Script.runWith (opts (fun o -> { o with MaxStatements = 7 })) (statements 8) |> fails MessageCode.StatementLimitExceeded
    Script.runWith (opts (fun o -> { o with MaxLoopIterations = 7 })) (whileLoop 7) |> ok |> ignore
    Script.runWith (opts (fun o -> { o with MaxLoopIterations = 7 })) (whileLoop 8) |> fails MessageCode.LoopIterationLimitExceeded
    Script.runWith (opts (fun o -> { o with MaxCallDepth = 7 })) (recursion 7) |> ok |> ignore
    Script.runWith (opts (fun o -> { o with MaxCallDepth = 7 })) (recursion 8) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1406")>]
[<Trait("requirement", "JYRO-REQ-1826")>]
let ``JYRO-REQ-1826 Unlimited_DoesNotLiftTheGeneratedLengthMaximum`` () =
    let o = { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited }
    Script.runWith o "Output.r = Length(PadLeft(\"a\", 1000001))" |> fails MessageCode.GeneratedLengthExceeded
    Script.runWith o "Output.r = Length(PadRight(\"\", 1000001))" |> fails MessageCode.GeneratedLengthExceeded
    Script.runWith o "Output.r = Length(RandomString(1000001))" |> fails MessageCode.GeneratedLengthExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1406")>]
let ``EveryLimitSetting_LeavesTheNestingBoundWhereItIs`` () =
    let n = maxDepth ()
    let src = lines [ buildNested "a" (n + 1); buildNested "b" (n + 1); "Output.r = a == b" ]
    let settings =
        [ JyroExecutionOptions.Unlimited
          roomy JyroExecutionOptions.Default
          { roomy JyroExecutionOptions.Default with MaxCallDepth = 1 }
          { roomy JyroExecutionOptions.Default with MaxCallDepth = Int32.MaxValue } ]
    for s in settings do
        Script.runWith { Script.defaults with Limits = Some s } src |> fails MessageCode.ValueNestingLimitExceeded

// ---------------------------------------------------------------------------------
// 100.2.1 Execution time checked at the start of every statement
// ---------------------------------------------------------------------------------

let private timeOpts (fns: RecordingFunction list) =
    { withLimits (fun o -> { roomy o with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with
        Functions = fns |> List.map (fun f -> f.AsFunction) }

let private unionDecl = "union U A B end"

[<Theory>]
[<InlineData("var v = Mark(1)")>]
[<InlineData("Output.after = Mark(1)")>]
[<InlineData("Mark(1)")>]
[<InlineData("if Mark(1) then Output.x = 1 end")>]
[<InlineData("while Mark(1) do Output.x = 1 end")>]
[<InlineData("for i in Mark(1) to 2 do Output.x = 1 end")>]
[<InlineData("foreach x in [Mark(1)] do Output.x = 1 end")>]
[<InlineData("switch Mark(1) do case 1 then Output.x = 1 end")>]
[<InlineData("match Mark(1) do case A then Output.x = 1 case B then Output.x = 2 end")>]
[<InlineData("exit Mark(1)")>]
[<InlineData("fail Mark(1)")>]
[<InlineData("delete Output[Mark(1)]")>]
[<Trait("requirement", "JYRO-REQ-1407")>]
let ``TopLevel_StatementAfterTimeRunsOut_DoesNotStart`` (next: string) =
    let s = stall ()
    let m = marker ()
    let r = Script.runWith (timeOpts [ s; m ]) (lines [ unionDecl; "Stall(300)"; next ])
    fails MessageCode.ExecutionTimeLimitExceeded r
    Assert.Equal(0, m.CallCount)

[<Theory>]
[<InlineData("if true then\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("var w = 0\nwhile w < 3 do\n  w = w + 1\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("foreach x in [1] do\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("for i in 0 to 1 do\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("func F()\n  Stall(300)\n  Mark(1)\n  return 1\nend\nF()")>]
[<InlineData("func F()\n  Stall(300)\n  return Mark(1)\nend\nF()")>]
[<InlineData("switch 1 do case 1 then\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("switch 2 do case 1 then Output.x = 1 default then\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("match A() do case A then\n  Stall(300)\n  Mark(1)\ncase B then Output.x = 1 end")>]
[<InlineData("if false then Output.x = 1 elseif true then\n  Stall(300)\n  Mark(1)\nend")>]
[<InlineData("if false then Output.x = 1 else\n  Stall(300)\n  Mark(1)\nend")>]
[<Trait("requirement", "JYRO-REQ-1407")>]
let ``NestedBlock_StatementAfterTimeRunsOut_DoesNotStart`` (body: string) =
    let s = stall ()
    let m = marker ()
    let r = Script.runWith (timeOpts [ s; m ]) (lines [ unionDecl; body ])
    fails MessageCode.ExecutionTimeLimitExceeded r
    Assert.Equal(0, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1407")>]
let ``NextIteration_AfterTimeRunsOut_DoesNotStart`` () =
    // The first iteration's Mark(x) must run before the limit: marginLimitMs
    // (Support.fs) is ten times its longest duration measured under a whole
    // verification run (254.42 ms; floors-process 4.1.18, d-585).
    let s = stall ()
    let m = marker ()
    let src = sprintf "foreach x in [1, 2, 3] do\n  Mark(x)\n  Stall(%d)\nend" marginStallMs
    let r = Script.runWith (marginOpts [ s; m ]) src
    fails MessageCode.ExecutionTimeLimitExceeded r
    Assert.Equal(1, m.CallCount)

// ---------------------------------------------------------------------------------
// 100.2.2 Statement count: +1 at the start of every statement that runs
// ---------------------------------------------------------------------------------

let private stmtOpts (k: int) (fns: RecordingFunction list) =
    // JYRO-ADR-181: a count limit of 0 means "not set" and takes the default, so a limit
    // below 1 can never express "no statements allowed". Refuse it rather than run the default.
    if k < 1 then
        invalidArg (nameof k) $"A statement limit of {k} is not a working limit: under JYRO-ADR-181 a count limit of 0 means the default."
    { withLimits (fun o -> { roomy o with MaxStatements = k }) with Functions = fns |> List.map (fun f -> f.AsFunction) }

/// Scripts and the number of statements each runs (100.2.2, 100.2.9, 100.2.10).
let statementKinds : obj[] seq =
    [ "var", "var a = 1\nvar b = 2", 2
      "assignment", "Output.a = 1\nOutput.b = 2", 2
      "expression", "Mark(1)\nMark(2)", 2
      "return", "func f()\n  return 1\nend\nOutput.a = f()", 2
      "break", "while true do\n  break\nend", 2
      "continue", "var i = 0\nwhile i < 2 do\n  i = i + 1\n  continue\nend", 6
      "delete", "Output.a = 1\ndelete Output.a", 2
      "exit", "Output.a = 1\nexit", 2
      "if", "if true then Output.a = 1 end", 2
      "while", "var i = 0\nwhile i < 3 do i = i + 1 end", 5
      "for", "for i in 0 to 3 do Output.a = i end", 4
      "foreach", "foreach x in [1, 2, 3] do Output.a = x end", 4
      "switch", "switch 1 do case 1 then Output.a = 1 end", 2
      "match", "union U A B end\nmatch A() do case A then Output.a = 1 case B then Output.b = 1 end", 2
      "function body per call", "func f()\n  var a = 1\n  return a\nend\nMark(f())\nMark(f())\nMark(f())", 9 ]
    |> Seq.map (fun (n, s, k) -> [| box n; box s; box k |])

[<Theory>]
[<MemberData(nameof statementKinds)>]
[<Trait("requirement", "JYRO-REQ-1408")>]
let ``StatementKind_RunsWithExactlyItsCount_AndFailsWithOneLess`` (kind: string, source: string, count: int) =
    ignore kind
    Script.runWith (stmtOpts count [ marker () ]) source |> ok |> ignore
    Script.runWith (stmtOpts (count - 1) [ marker () ]) source |> fails MessageCode.StatementLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1408")>]
let ``FailStatement_CountsAsOne`` () =
    let src = "Output.a = 1\nfail \"x\""
    Script.runWith (stmtOpts 2 []) src |> fails MessageCode.ScriptFailure
    Script.runWith (stmtOpts 1 []) src |> fails MessageCode.StatementLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1408")>]
let ``StatementsThatDoNotRun_AddNothing`` () =
    Script.runWith (stmtOpts 1 []) "if false then\n  Output.x = 1\n  Output.y = 2\nend" |> ok |> ignore
    Script.runWith (stmtOpts 1 []) "while false do\n  Output.x = 1\nend" |> ok |> ignore
    Script.runWith (stmtOpts 2 []) "func f()\n  var a = 1\n  return a\nend\nOutput.z = 1\nOutput.y = 2" |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1408")>]
let ``StatementPastTheLimit_IsCheckedAtItsStart_AndNeverRuns`` () =
    let m = marker ()
    let r = Script.runWith (stmtOpts 3 [ m ]) "Output.a = 1\nOutput.b = 2\nOutput.c = 3\nMark(4)\nOutput.d = 5"
    fails MessageCode.StatementLimitExceeded r
    Assert.Equal(0, m.CallCount)
    let d = Script.onlyErrorMessage r
    Assert.True(d.Location.HasValue, "The limit diagnostic has no position")
    Assert.Equal(4, d.Location.Value.Line)
    let v = Script.valueOf r
    Assert.True(hasProp "c" v)
    Assert.False(hasProp "d" v)

// ---------------------------------------------------------------------------------
// 100.2.3 Loop iterations: +1 at the start of every iteration of for, foreach, while
// ---------------------------------------------------------------------------------

let private loopOpts (l: int) (fns: RecordingFunction list) =
    { withLimits (fun o -> { roomy o with MaxLoopIterations = l }) with Functions = fns |> List.map (fun f -> f.AsFunction) }

[<Theory>]
[<InlineData("for i in 0 to {n} do Mark(i) end")>]
[<InlineData("for i in {n} downto 0 do Mark(i) end")>]
[<InlineData("for i in 0 to {n2} by 2 do Mark(i) end")>]
[<InlineData("foreach x in {arr} do Mark(x) end")>]
[<InlineData("var i = 0\nwhile i < {n} do\n  Mark(i)\n  i = i + 1\nend")>]
[<InlineData("func F()\n  foreach x in {arr} do Mark(x) end\n  return 0\nend\nF()")>]
[<Trait("requirement", "JYRO-REQ-1409")>]
let ``Loop_OfLIterations_Passes_AndOfLPlus1_FailsBeforeItsBody`` (template: string) =
    let l = 5
    let src n = template.Replace("{arr}", arrayLiteral n).Replace("{n2}", string (2 * n)).Replace("{n}", string n)
    let m1 = marker ()
    Script.runWith (loopOpts l [ m1 ]) (src l) |> ok |> ignore
    Assert.Equal(l, m1.CallCount)
    let m2 = marker ()
    Script.runWith (loopOpts l [ m2 ]) (src (l + 1)) |> fails MessageCode.LoopIterationLimitExceeded
    Assert.Equal(l, m2.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1409")>]
let ``Iterations_AccumulateAcrossLoopsInOneExecution`` () =
    Script.runWith (loopOpts 5 []) "foreach x in [1, 2, 3] do Output.a = x end\nforeach y in [1, 2] do Output.b = y end"
    |> ok |> ignore
    Script.runWith (loopOpts 5 []) "foreach x in [1, 2, 3] do Output.a = x end\nforeach y in [1, 2, 3] do Output.b = y end"
    |> fails MessageCode.LoopIterationLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1409")>]
let ``NestedLoops_CountOuterAndInnerIterations`` () =
    // outer 1 + inner 4 = 5; outer 2 + inner 4 = 6
    Script.runWith (loopOpts 5 []) "foreach x in [1] do foreach y in [1, 2, 3, 4] do Output.a = y end end" |> ok |> ignore
    Script.runWith (loopOpts 5 []) "foreach x in [1, 2] do foreach y in [1, 2] do Output.a = y end end"
    |> fails MessageCode.LoopIterationLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1409")>]
let ``IterationsEndedByContinueOrBreak_Count`` () =
    let cont n = sprintf "for i in 0 to %d do\n  continue\nend" n
    Script.runWith (loopOpts 5 []) (cont 5) |> ok |> ignore
    Script.runWith (loopOpts 5 []) (cont 6) |> fails MessageCode.LoopIterationLimitExceeded
    let brk n = sprintf "for i in 0 to 100 do\n  if i == %d then break end\nend" n
    // break in iteration k (k counted from 1) means k iterations started
    Script.runWith (loopOpts 5 []) (brk 4) |> ok |> ignore
    Script.runWith (loopOpts 4 []) (brk 4) |> fails MessageCode.LoopIterationLimitExceeded

// ---------------------------------------------------------------------------------
// 100.2.4 Call depth: +1 at the start of every call, -1 when it returns
// ---------------------------------------------------------------------------------

let private depthOpts (d: int) (fns: RecordingFunction list) =
    { withLimits (fun o -> { roomy o with MaxCallDepth = d }) with Functions = fns |> List.map (fun f -> f.AsFunction) }

/// A chain of `depth - 1` script calls whose innermost body evaluates `inner`
/// (itself one call), so that `inner` runs at call depth `depth`.
let chainTo (inner: string) (depth: int) : string =
    let leaf = "func Leaf()\n  return 0\nend"
    if depth = 1 then lines [ leaf; sprintf "Output.r = %s" inner ]
    else
        lines [ leaf
                "func R(n)"
                "  if n > 1 then"
                "    return R(n - 1)"
                "  end"
                sprintf "  return %s" inner
                "end"
                sprintf "Output.r = R(%d)" (depth - 1) ]

[<Theory>]
[<InlineData("Leaf()")>]
[<InlineData("Length([1])")>]
[<InlineData("Mark(1)")>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``EachKindOfCall_AddsOneToTheDepth`` (inner: string) =
    let d = 4
    Script.runWith (depthOpts d [ marker () ]) (chainTo inner d) |> ok |> ignore
    Script.runWith (depthOpts d [ marker () ]) (chainTo inner (d + 1)) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``ReturningCall_TakesOneOffTheDepth`` () =
    let d = 4
    let sequential = lines ([ "func F()"; "  return 1"; "end" ] @ repeatLines 20 "Output.a{i} = F()")
    Script.runWith (depthOpts 1 []) sequential |> ok |> ignore
    let twice = lines [ recursion d; "Output.again = R(3)"; "Output.third = R(3)" ]
    Script.runWith (depthOpts d []) twice |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``CallInACondition_Counts`` () =
    let src (k: int) = lines [ recursion 1; sprintf "if R(%d) == 0 then Output.ok = true end" (k - 1) ]
    Script.runWith (depthOpts 4 []) (src 4) |> ok |> ignore
    Script.runWith (depthOpts 4 []) (src 5) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``NamedArgumentCall_Counts`` () =
    let src (k: int) = lines [ recursion 1; "func G(a)"; "  return R(a)"; "end"; sprintf "Output.g = G(a: %d)" (k - 2) ]
    Script.runWith (depthOpts 4 []) (src 4) |> ok |> ignore
    Script.runWith (depthOpts 4 []) (src 5) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``CallInALambdaBody_CountsWithTheCallThatRunsTheLambda`` () =
    // Map is depth 1; R(k) inside the lambda adds k + 1 levels.
    let src (k: int) = lines [ recursion 1; sprintf "Output.m = Map([%d], x => R(x))" k ]
    Script.runWith (depthOpts 4 []) (src 2) |> ok |> ignore
    Script.runWith (depthOpts 4 []) (src 3) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``CallInAnArgument_StartsAndReturnsBeforeTheOuterCallStarts`` () =
    // The book's example at a call depth limit of 1, with a standard library
    // and a host call in the argument as well, and calls nested three deep.
    let src =
        lines [ "func G() return 1 end"
                "func F(a) return a end"
                "union U V(x) end"
                "Output.p = F(G())"
                "Output.n = F(a: G())"
                "Output.v = V(G())"
                "Output.s = F(Length(\"a\"))"
                "Output.h = F(Mark(1)) ?? 1"
                "Output.d = F(F(F(G())))"
                "Output.w = V(F(a: V(G())))" ]
    let out = Script.runWith (depthOpts 1 [ marker () ]) src |> ok
    for name in [ "p"; "n"; "s"; "h"; "d" ] do
        Assert.Equal(1.0, Values.asFloat (Values.prop name out))
    Assert.Equal(1.0, Values.asFloat (out |> Values.prop "v" |> Values.prop "x"))
    Assert.Equal(1.0, Values.asFloat (out |> Values.prop "w" |> Values.prop "x" |> Values.prop "x"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``VariantCall_AddsOneToTheDepth`` () =
    let inH = "union U V(x) end\nfunc H() return V(1) end\nOutput.v = H()"
    Script.runWith (depthOpts 1 []) inH |> fails MessageCode.CallDepthLimitExceeded
    Script.runWith (depthOpts 2 []) inH |> ok |> ignore
    Script.runWith (depthOpts 1 []) "union U V(x) end\nOutput.v = V(1)" |> ok |> ignore
    let d = 4
    let chain k = "union U V(x) end\n" + chainTo "V(1)" k
    Script.runWith (depthOpts d []) (chain d) |> ok |> ignore
    Script.runWith (depthOpts d []) (chain (d + 1)) |> fails MessageCode.CallDepthLimitExceeded

// ---------------------------------------------------------------------------------
// JYRO-DEF-497: the call site keeps both JYRO-REQ-1410's argument evaluation
// (arguments first, then the depth check) and JYRO-ADR-108's statement position
// (saved before the call, restored when it returns), so an error raised after a
// call returns, in the statement that made it, is placed at that statement and
// not at the last statement the callee ran.
// ---------------------------------------------------------------------------------

/// A host function of no parameters that raises a runtime error carrying no
/// position of its own, so the diagnostic's position is the statement running.
let private boom () =
    Host.func "Boom" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Runtime(MessageCode.DivisionByZero)))

let private boomOpts (d: int) =
    { withLimits (fun o -> { roomy o with MaxCallDepth = d }) with Functions = [ (boom ()).AsFunction ] }

/// Lines 1 to 8: two script functions whose last statement is on lines 3 and 7.
let private calleeLines =
    [ "func G()"
      "  var a = 1"
      "  return a"
      "end"
      "func F(x)"
      "  var b = x"
      "  return b"
      "end" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``JYRO-DEF-497 JYRO-REQ-1410 at a call depth limit of 1 calls in arguments succeed and an error after them is placed at the calling statement``
    () =
    let src =
        lines (calleeLines
               @ [ "Output.p = F(G())"            // line 9
                   "Output.n = F(x: G())"         // line 10
                   "Output.z = F(G()) + Boom()" ]) // line 11
    let r = Script.runWith (boomOpts 1) src
    fails MessageCode.DivisionByZero r
    let d = Script.onlyErrorMessage r
    Assert.True(d.Location.HasValue, "The diagnostic has no position")
    Assert.Equal(11, d.Location.Value.Line)
    let v = Script.valueOf r
    Assert.True(hasProp "p" v, "Output.p = F(G()) did not run at a call depth limit of 1")
    Assert.True(hasProp "n" v, "Output.n = F(x: G()) did not run at a call depth limit of 1")
    Assert.False(hasProp "z" v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1410")>]
let ``JYRO-DEF-497 an error after a call returns inside a function body is placed at the calling statement in that body``
    () =
    let src =
        lines (calleeLines
               @ [ "func H()"                     // line 9
                   "  var c = 0"                  // line 10
                   "  c = F(G()) + Boom()"        // line 11
                   "  return c"                   // line 12
                   "end"                          // line 13
                   "Output.h = H()" ])            // line 14
    let r = Script.runWith (boomOpts 2) src
    fails MessageCode.DivisionByZero r
    let d = Script.onlyErrorMessage r
    Assert.True(d.Location.HasValue, "The diagnostic has no position")
    Assert.Equal(11, d.Location.Value.Line)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1420")>]
let ``JYRO-DEF-497 JYRO-REQ-1420 a call in an argument one past the limit stops with CallDepthLimitExceeded``
    () =
    // H runs at depth 1; F(G()) inside it starts F and G at depth 2.
    let src =
        lines (calleeLines
               @ [ "func H()"                     // line 9
                   "  var c = 0"                  // line 10
                   "  c = F(G())"                 // line 11
                   "  return c"                   // line 12
                   "end"                          // line 13
                   "Output.h = H()" ])            // line 14
    Script.runWith (boomOpts 2) src |> ok |> ignore
    Script.runWith (boomOpts 1) src |> fails MessageCode.CallDepthLimitExceeded

// ---------------------------------------------------------------------------------
// 100.2.5 / 100.2.6 Iterating library functions check time and cancellation,
// and add nothing to the statement or loop count
// ---------------------------------------------------------------------------------

let private setupLines =
    [ "var xs = Range(0, 20000)"
      "var os = Map(xs, x => {a: x, b: [x]})"
      "var s = Join(xs, \",\")"
      "var js = ToJson(xs)"
      "var bo = FromJson(\"{\" & Join(Map(xs, x => \"\\\"k\" & x & \"\\\":\" & x), \",\") & \"}\")" ]

/// Calls of an iterating standard library function; D marks the argument that
/// carries the data (through a host function that cancels or stalls first).
let iteratingCalls : obj[] seq =
    [ "Sort(D(xs))"; "Reverse(D(xs))"; "Distinct(D(xs))"; "IndexOf(D(xs), -1)"; "Join(D(xs), \",\")"
      "Flatten(D(xs))"; "FlattenOnce(D(xs))"; "Concatenate(D(xs), xs)"; "Slice(D(xs), 0)"; "Skip(D(xs), 1)"
      "Sum(D(xs))"; "Average(D(xs))"; "Min(D(xs))"; "Max(D(xs))"; "Median(D(xs))"; "Mode(D(xs))"
      "Clone(D(xs))"; "ToJson(D(xs))"; "ToString(D(xs))"; "Map(D(xs), x => x)"; "Where(D(xs), x => false)"
      "Reduce(D(xs), (a, x) => a, 0)"; "Each(D(xs), x => x)"; "Find(D(xs), x => false)"; "All(D(xs), x => true)"
      "Any(D(xs), x => false)"; "SortBy(D(xs), x => x)"; "SortByField(D(os), \"a\", \"desc\")"
      "GroupBy(D(os), \"a\")"; "WhereByField(D(os), \"a\", \"==\", -1)"; "AnyByField(D(os), \"a\", \"==\", -1)"
      "AllByField(D(os), \"a\", \">=\", 0)"; "FindByField(D(os), \"a\", \"==\", -1)"
      "CountIf(D(os), \"a\", \"==\", -1)"; "Select(D(os), \"a\")"; "Project(D(os), [\"a\"])"; "Omit(D(os), [\"a\"])"
      "SelectMany(D(os), \"b\")"; "Split(D(s), \",\")"; "Replace(D(s), \"1\", \"2\")"; "FromJson(D(js))"
      "RegexMatchAll(D(s), \"[0-9]+\")"; "Range(0, D(1000000))"; "Diff(D(bo), bo)"; "Keys(D(bo))"; "Values(D(bo))"; "Merge([D(bo), bo])"
      "ValidateSchema(D(xs), {type: \"array\", items: {type: \"string\"}})" ]
    |> Seq.map (fun c -> [| box c |])

[<Theory>]
[<MemberData(nameof iteratingCalls)>]
[<Trait("requirement", "JYRO-REQ-1411")>]
let ``IteratingFunction_WhenCanceledBeforeItIterates_StopsWithCanceledByHost`` (call: string) =
    use cts = new CancellationTokenSource()
    let c = canceller cts
    let src = lines (setupLines @ [ "Output.r = " + call.Replace("D(", "CancelNow(") ])
    let r =
        bounded 9. (fun () ->
            Script.runWith
                { withLimits roomy with Functions = [ c.AsFunction ]; Cancellation = Some cts.Token }
                src)
    fails MessageCode.CanceledByHost r

[<Theory>]
[<InlineData("Sort(D(xs))")>]
[<InlineData("Distinct(D(xs))")>]
[<InlineData("Sum(D(xs))")>]
[<InlineData("Map(D(xs), x => x)")>]
[<InlineData("ToJson(D(xs))")>]
[<InlineData("Range(0, D(1000000))")>]
[<InlineData("WhereByField(D(os), \"a\", \"==\", -1)")>]
[<InlineData("Split(D(s), \",\")")>]
[<Trait("requirement", "JYRO-REQ-1411")>]
let ``IteratingFunction_WhenTimeRunsOutBeforeItIterates_StopsWithExecutionTimeLimitExceeded`` (call: string) =
    let s = stall ()
    let src = lines (setupLines @ [ "Output.r = " + call.Replace("D(", "Stall(2500, ") ])
    let r =
        bounded 9. (fun () ->
            Script.runWith
                { withLimits (fun o -> { roomy o with MaxExecutionTime = TimeSpan.FromMilliseconds 2000. }) with
                    Functions = [ s.AsFunction ] }
                src)
    fails MessageCode.ExecutionTimeLimitExceeded r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1411")>]
let ``HostFunctionThatChecksLimits_GetsCanceledByHostAndExecutionTimeLimitExceeded`` () =
    use cts = new CancellationTokenSource()
    let c = canceller cts
    let spin = spinChecked ()
    Script.runWith
        { withLimits roomy with Functions = [ c.AsFunction; spin.AsFunction ]; Cancellation = Some cts.Token }
        "Output.r = Spin(CancelNow(100000))"
    |> fails MessageCode.CanceledByHost
    let spin2 = spinChecked ()
    Script.runWith
        { withLimits (fun o -> { roomy o with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with
            Functions = [ spin2.AsFunction ] }
        "Output.r = Spin(3000000)"
    |> fails MessageCode.ExecutionTimeLimitExceeded

[<Theory>]
[<InlineData("Map(xs, x => x)")>]
[<InlineData("Where(xs, x => true)")>]
[<InlineData("Reduce(xs, (a, x) => a + x, 0)")>]
[<InlineData("Each(xs, x => x)")>]
[<InlineData("SortBy(xs, x => 0 - x)")>]
[<InlineData("Sort(xs)")>]
[<InlineData("Distinct(xs)")>]
[<InlineData("ToJson(xs)")>]
[<InlineData("Range(0, 2000)")>]
[<InlineData("Sum(xs)")>]
[<InlineData("WhereByField(Map(xs, x => {a: x}), \"a\", \">\", 5)")>]
[<Trait("requirement", "JYRO-REQ-1412")>]
let ``IterationInsideALibraryFunction_AddsNothingToTheStatementOrLoopCount`` (call: string) =
    let stats = JyroPipelineStats()
    let r =
        Script.runWith
            { withLimits (fun o -> { o with MaxStatements = 2; MaxLoopIterations = 1 }) with Stats = Some stats }
            (lines [ "var xs = Range(0, 2000)"; "Output.r = TypeOf(" + call + ")" ])
    ok r |> ignore
    Assert.Equal(2L, stats.StatementCount)
    Assert.Equal(0L, stats.LoopIterations)

// ---------------------------------------------------------------------------------
// 100.2.7 No delayed or grouped checks
// ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1413")>]
let ``StatementLimit_NoStatementRunsPastIt`` () =
    let m = marker ()
    let src = lines (repeatLines 50 "Mark({i})")
    Script.runWith (stmtOpts 10 [ m ]) src |> fails MessageCode.StatementLimitExceeded
    Assert.Equal(10, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1413")>]
let ``StatementLimitOfOne_TightLoop_RunsOnlyOneStatement`` () =
    let m = marker ()
    let o = { stmtOpts 1 [ m ] with Limits = Some (Script.limits (fun o -> { o with MaxStatements = 1; MaxLoopIterations = 5 })) }
    Script.runWith o "while Mark(1) == null do Mark(2) end" |> fails MessageCode.StatementLimitExceeded
    // The while statement starts (statement 1) and evaluates its condition; its body never starts.
    Assert.Equal(1, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1413")>]
let ``LoopLimit_NoIterationRunsPastIt`` () =
    let m = marker ()
    let o = { loopOpts 7 [ m ] with Limits = Some (Script.limits (fun o -> { o with MaxLoopIterations = 7; MaxStatements = 1000 })) }
    Script.runWith o "while true do Mark(1) end" |> fails MessageCode.LoopIterationLimitExceeded
    Assert.Equal(7, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1413")>]
let ``CallDepthLimit_NoBodyRunsPastIt`` () =
    // Each body appends its level to the log without a call; the call at
    // depth 7 must not run its body.
    let src =
        lines [ "var log = []"
                "Output.log = log"
                "func R(n, log)"
                "  log[n] = n"
                "  if n < 30 then"
                "    return R(n + 1, log)"
                "  end"
                "  return 0"
                "end"
                "Output.r = R(0, log)" ]
    let r = Script.runWith (depthOpts 6 []) src
    fails MessageCode.CallDepthLimitExceeded r
    Assert.Equal("[0,1,2,3,4,5]", Values.toJson (resultProp "log" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1413")>]
let ``TimeAndCancellation_NoStatementRunsAfterTheCheckFails`` () =
    // Mark(0) must run before the limit: marginLimitMs (Support.fs) is ten times
    // its longest duration measured under a whole verification run (0.58 ms;
    // the longest pre-stall step measured, 254.42 ms; floors-process 4.1.18, d-585).
    let s = stall ()
    let m = marker ()
    Script.runWith (marginOpts [ s; m ]) (sprintf "Mark(0)\nStall(%d)\nMark(1)\nMark(2)" marginStallMs)
    |> fails MessageCode.ExecutionTimeLimitExceeded
    Assert.Equal(1, m.CallCount)
    use cts = new CancellationTokenSource()
    let c = canceller cts
    let m2 = marker ()
    Script.runWith
        { withLimits roomy with Functions = [ c.AsFunction; m2.AsFunction ]; Cancellation = Some cts.Token }
        "Mark(0)\nCancelNow()\nMark(1)\nMark(2)"
    |> fails MessageCode.CanceledByHost
    Assert.Equal(1, m2.CallCount)

// ---------------------------------------------------------------------------------
// 100.2.8 A library call of a function value does not change the depth
// ---------------------------------------------------------------------------------

[<Theory>]
[<InlineData("Map([1, 2, 3], x => {c})")>]
[<InlineData("Where([1, 2, 3], x => {c} > 0)")>]
[<InlineData("Reduce([1, 2, 3], (a, x) => a + {c}, 0)")>]
[<InlineData("Each([1, 2, 3], x => {c})")>]
[<InlineData("Find([1, 2, 3], x => {c} > 5)")>]
[<InlineData("All([1, 2, 3], x => {c} > 0)")>]
[<InlineData("Any([1, 2, 3], x => {c} > 5)")>]
[<InlineData("SortBy([3, 1, 2], x => {c})")>]
[<Trait("requirement", "JYRO-REQ-1414")>]
let ``LambdaFunction_CallingTheLambda_AddsNothing_ButACallInItsBodyAddsOne`` (template: string) =
    let plain = template.Replace("{c}", "x")
    Script.runWith (depthOpts 1 []) ("Output.r = " + plain) |> ok |> ignore
    for inner in [ "Absolute(x)"; "Leaf(x)"; "Mark2(x)" ] do
        let src = lines [ "func Leaf(v)"; "  return v"; "end"; "Output.r = " + template.Replace("{c}", inner) ]
        let host = Host.func "Mark2" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any
                            FunctionEffect.Pure (fun a -> a.Head)
        Script.runWith (depthOpts 1 [ host ]) src |> fails MessageCode.CallDepthLimitExceeded
        Script.runWith (depthOpts 2 [ host ]) src |> ok |> ignore

// ---------------------------------------------------------------------------------
// 100.2.9 Declarations add nothing
// ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2040")>]
let ``OneFunctionDeclarationAndNAssignments_RunUnderALimitOfN`` () =
    let n = 5
    let body = repeatLines n "Output.s{i} = {i}"
    Script.runWith (stmtOpts n []) (lines ("func F()\n  return 1\nend" :: body)) |> ok |> ignore
    Script.runWith (stmtOpts (n - 1) []) (lines ("func F()\n  return 1\nend" :: body)) |> fails MessageCode.StatementLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2040")>]
let ``UnionDeclarationAndNAssignments_RunUnderALimitOfN`` () =
    let n = 5
    Script.runWith (stmtOpts n []) (lines ("union U A B end" :: repeatLines n "Output.s{i} = {i}")) |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2040")>]
let ``ManyDeclarationsAnywhere_AddNothing`` () =
    let n = 4
    let src =
        lines [ "func F0()\n  return 0\nend"
                "Output.a = 1"
                "union U0 A0 B0 end"
                "func F1()\n  return 1\nend"
                "Output.b = 2"
                "union U1 A1 end"
                "union U2 A2(x) end"
                "Output.c = 3"
                "func F2(x)\n  return x\nend"
                "Output.d = 4"
                "func F3()\n  return 3\nend"
                "union U3 A3 end" ]
    Script.runWith (stmtOpts n []) src |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2040")>]
let ``LibraryDeclarations_AddNothing`` () =
    let lib = "func Twice(x)\n  return x * 2\nend\nunion LU LA LB end\nfunc Thrice(x)\n  return x * 3\nend"
    let r =
        Script.runWith
            { stmtOpts 3 [] with Libraries = [ lib ] }
            "Output.a = 1\nOutput.b = 2\nOutput.c = 3"
    ok r |> ignore

// ---------------------------------------------------------------------------------
// 100.2.10 Compound statements add one when they start; branches add nothing
// ---------------------------------------------------------------------------------

let compoundCounts : obj[] seq =
    [ "var c = true\nif c then Output.x = 1 end", 3
      "if false then Output.x = 1 elseif true then Output.y = 1 end", 2
      "if false then Output.x = 1 else Output.y = 1 end", 2
      "if false then Output.x = 1 elseif false then Output.y = 1 else Output.z = 1 end", 2
      "var i = 0\nwhile i < 3 do i = i + 1 end", 5
      "foreach v in [1, 2, 3] do Output.x = v end", 4
      "for i in 0 to 3 do Output.x = i end", 4
      "Output.p = 0\nwhile false do Output.x = 1 end", 2
      "switch 1 do case 1 then Output.x = 1 end", 2
      "switch 9 do case 1 then Output.x = 1 default then Output.y = 1 end", 2
      "switch 2 do case 1, 2, 3 then Output.x = 1 end", 2
      "union S Sq(n) Ci(r) end\nmatch Sq(1) do case Sq(n) then Output.x = n case Ci(r) then Output.y = r end", 2
      "var i = 0\nwhile i < 2 do\n  i = i + 1\n  if true then Output.x = 1 end\nend", 8 ]
    |> Seq.map (fun (s, k) -> [| box s; box k |])

[<Theory>]
[<MemberData(nameof compoundCounts)>]
[<Trait("requirement", "JYRO-REQ-1416")>]
let ``CompoundStatement_CountsAsTheClauseGives`` (source: string, count: int) =
    Script.runWith (stmtOpts count []) source |> ok |> ignore
    Script.runWith (stmtOpts (count - 1) []) source |> fails MessageCode.StatementLimitExceeded

// ---------------------------------------------------------------------------------
// 100.3.x Halting on a breach
// ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1417")>]
let ``TimeLimit_HostStall_StopsAtTheNextCheck_WithOutputAsItWas`` () =
    // Output.a = 1 must run before the limit: marginLimitMs (Support.fs) is ten
    // times the longest pre-stall step measured under a whole verification run
    // (Output.a = 1: 138.29 ms; any shape: 254.42 ms; floors-process 4.1.18, d-581).
    let s = stall ()
    let r = Script.runWith (marginOpts [ s ]) (sprintf "Output.a = 1\nStall(%d)\nOutput.b = 2" marginStallMs)
    fails MessageCode.ExecutionTimeLimitExceeded r
    let v = Script.valueOf r
    Assert.True(hasProp "a" v)
    Assert.False(hasProp "b" v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1417")>]
let ``TimeLimit_Sleep_StopsWithExecutionTimeLimitExceeded`` () =
    let r = Script.runWith (timeOpts []) "Sleep(1000)\nOutput.b = 2"
    fails MessageCode.ExecutionTimeLimitExceeded r
    Assert.False(hasProp "b" (Script.valueOf r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1417")>]
let ``TimeLimit_ScriptWellInsideIt_Succeeds`` () =
    Script.runWith (withLimits (fun o -> { o with MaxExecutionTime = TimeSpan.FromSeconds 2. })) "Output.a = 1" |> ok |> ignore

[<Theory>]
[<InlineData(1)>]
[<InlineData(10)>]
[<InlineData(100)>]
[<Trait("requirement", "JYRO-REQ-1418")>]
let ``StatementLimitN_NPass_NPlus1StopsBeforeTheStatementRuns`` (n: int) =
    Script.runWith (stmtOpts n []) (statements n) |> ok |> ignore
    let m = marker ()
    let src = lines (repeatLines n "Output.s{i} = {i}" @ [ "Mark(1)" ])
    let r = Script.runWith (stmtOpts n [ m ]) src
    fails MessageCode.StatementLimitExceeded r
    Assert.Equal(0, m.CallCount)

[<Theory>]
[<InlineData(1, "for")>]
[<InlineData(10, "for")>]
[<InlineData(1000, "for")>]
[<InlineData(1, "foreach")>]
[<InlineData(10, "foreach")>]
[<InlineData(1000, "foreach")>]
[<InlineData(1, "while")>]
[<InlineData(10, "while")>]
[<InlineData(1000, "while")>]
[<Trait("requirement", "JYRO-REQ-1419")>]
let ``LoopLimitL_LIterationsPass_LPlus1StopsBeforeItsBody`` (l: int, kind: string) =
    let template =
        match kind with
        | "for" -> "for i in 0 to {n} do Mark(i) end"
        | "foreach" -> "foreach x in {arr} do Mark(x) end"
        | _ -> "var i = 0\nwhile i < {n} do\n  Mark(i)\n  i = i + 1\nend"
    let src (k: int) = template.Replace("{arr}", arrayLiteral k).Replace("{n}", string k)
    Script.runWith (loopOpts l [ marker () ]) (src l) |> ok |> ignore
    let m = marker ()
    Script.runWith (loopOpts l [ m ]) (src (l + 1)) |> fails MessageCode.LoopIterationLimitExceeded
    Assert.Equal(l, m.CallCount)

[<Theory>]
[<InlineData(1)>]
[<InlineData(5)>]
[<InlineData(64)>]
[<Trait("requirement", "JYRO-REQ-1420")>]
let ``CallDepthLimitD_DNestedCallsPass_DPlus1Stops`` (d: int) =
    for inner in [ "Leaf()"; "Length([1])"; "Mark(1)" ] do
        Script.runWith (depthOpts d [ marker () ]) (chainTo inner d) |> ok |> ignore
        let m = marker ()
        Script.runWith (depthOpts d [ m ]) (chainTo inner (d + 1)) |> fails MessageCode.CallDepthLimitExceeded
        Assert.Equal(0, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1421")>]
let ``Counters_AreWideEnoughNotToRollOverAt2Pow31`` () =
    Assert.Equal(typeof<int64>, typeof<JyroPipelineStats>.GetProperty("StatementCount").PropertyType)
    Assert.Equal(typeof<int64>, typeof<JyroPipelineStats>.GetProperty("LoopIterations").PropertyType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1421")>]
let ``LimitsOfInt32MaxValue_GiveNoFalseBreach`` () =
    let o =
        withLimits (fun o ->
            { o with MaxStatements = Int32.MaxValue; MaxLoopIterations = Int32.MaxValue; MaxCallDepth = Int32.MaxValue })
    let stats = JyroPipelineStats()
    Script.runWith { o with Stats = Some stats } (lines [ whileLoop 2000; recursion 10 ]) |> ok |> ignore
    Assert.Equal(2000L, stats.LoopIterations)
    Assert.True(stats.StatementCount > 2000L)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1421")>]
let ``Run_CountsPastInt16AndReportsExactCounts`` () =
    // The limiter is not public (JYRO-REQ-1874): the counts of a run are read from its stats.
    let stats = JyroPipelineStats()
    let o = { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited; Stats = Some stats }
    Script.runWith o (whileLoop 100_000) |> ok |> ignore
    Assert.Equal(100_000L, stats.LoopIterations)
    Assert.True(stats.StatementCount > 100_000L, sprintf "StatementCount is %d" stats.StatementCount)

// ---------------------------------------------------------------------------------
// 100.4.x Cancellation
// ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1422")>]
[<Trait("requirement", "JYRO-REQ-2041")>]
let ``TokenCanceledBeforeExecute_NoStatementRuns_CanceledByHost`` () =
    use cts = new CancellationTokenSource()
    cts.Cancel()
    let m = marker ()
    let r = Script.runWith { Script.defaults with Functions = [ m.AsFunction ]; Cancellation = Some cts.Token } "Mark(1)\nOutput.a = 1"
    fails MessageCode.CanceledByHost r
    Assert.Equal(0, m.CallCount)

[<Theory>]
[<InlineData("CancelNow()\nMark(1)")>]
[<InlineData("foreach x in [1, 2] do\n  CancelNow()\n  Mark(1)\nend")>]
[<InlineData("func F()\n  CancelNow()\n  Mark(1)\n  return 1\nend\nOutput.f = F()")>]
[<InlineData("var w = 0\nwhile w < 1000 do\n  w = w + 1\n  CancelNow()\nend")>]
[<Trait("requirement", "JYRO-REQ-1422")>]
[<Trait("requirement", "JYRO-REQ-2041")>]
let ``CanceledDuringExecution_StopsAtTheNextStatement_CanceledByHost`` (src: string) =
    use cts = new CancellationTokenSource()
    let c = canceller cts
    let m = marker ()
    let r = Script.runWith { withLimits roomy with Functions = [ c.AsFunction; m.AsFunction ]; Cancellation = Some cts.Token } src
    fails MessageCode.CanceledByHost r
    Assert.Equal(0, m.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1422")>]
[<Trait("requirement", "JYRO-REQ-2041")>]
let ``CanceledDuringSleep_StopsWithCanceledByHost`` () =
    use cts = new CancellationTokenSource()
    // Compiled first, and timed over Execute alone (floors-process 4.1.14): a
    // cancel not taken during Sleep would let it run to the 30 s time limit. The cancel comes
    // from a thread of its own, and the Sleep is far longer than any late cancel (JYRO-DEF-745).
    let options = { pinned with Cancellation = Some cts.Token }
    let runner = Script.runnerFor options (Script.compileWith options (sprintf "Sleep(%d)\nOutput.after = 1" RealTree.Jyro.Spec.HostCancel.unboundedSleepMs) |> Script.valueOf)
    RealTree.Jyro.Spec.HostCancel.cancelFromThread cts (TimeSpan.FromMilliseconds 100.)
    let r, elapsed = timed (fun () -> runner.Execute())
    fails MessageCode.CanceledByHost r
    Assert.False(hasProp "after" (Script.valueOf r))
    Assert.True(elapsed < TimeSpan.FromSeconds 25., sprintf "took %O: the Sleep was not canceled" elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2041")>]
let ``CanceledInsideAHostFunctionThatChecks_CanceledByHost`` () =
    use cts = new CancellationTokenSource()
    let spin =
        Host.funcWithContext "SpinCancel" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
            cts.Cancel()
            ctx.CheckExecutionLimits()
            Values.nul)
    let r = Script.runWith { withLimits roomy with Functions = [ spin.AsFunction ]; Cancellation = Some cts.Token } "Output.r = SpinCancel()"
    fails MessageCode.CanceledByHost r
    Assert.False(Script.ok r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1424")>]
let ``TimeEndsInsideAHostFunctionThatChecks_ExecutionTimeLimitExceeded`` () =
    let s = stallChecked ()
    Script.runWith (timeOpts [ s ]) "Output.r = StallChecked(300)" |> fails MessageCode.ExecutionTimeLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1424")>]
let ``TimeEndsInsideSleep_ExecutionTimeLimitExceeded_NotCanceledByHost`` () =
    let r = Script.runWith (timeOpts []) "Sleep(1000)\nOutput.x = 1"
    fails MessageCode.ExecutionTimeLimitExceeded r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1424")>]
let ``TimeEndsInsideAnIteratingLibraryFunction_ExecutionTimeLimitExceeded`` () =
    let s = stall ()
    let r =
        Script.runWith
            { withLimits (fun o -> { roomy o with MaxExecutionTime = TimeSpan.FromMilliseconds 1000. }) with
                Functions = [ s.AsFunction ] }
            "var xs = Range(0, 200000)\nOutput.r = Sort(Stall(1500, xs))"
    fails MessageCode.ExecutionTimeLimitExceeded r

// ---------------------------------------------------------------------------------
// 100.5.1 Removal of limits (MAY): the implementation offers Unlimited
// ---------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1826")>]
let ``JYRO-REQ-1826 Unlimited_RemovesTheStatementAndLoopLimits`` () =
    let o = { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited }
    Script.runWith o (statements 20_000) |> ok |> ignore
    Script.runWith o (whileLoop 5_000) |> ok |> ignore
    Script.run (statements 20_000) |> fails MessageCode.StatementLimitExceeded
    Script.run (whileLoop 5_000) |> fails MessageCode.LoopIterationLimitExceeded

// ---------------------------------------------------------------------------------
// 100.9.1 Compilation time limit
// ---------------------------------------------------------------------------------

let private bigSource = lines (repeatLines 2_000 "Output.s = ({i} + 1) * 2 - Length([1, 2, 3])")
let private bigLibrary = lines (repeatLines 1_000 "func F{i}(x)\n  return x + {i}\nend")
let private tick = TimeSpan.FromTicks 1L

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1449")>]
let ``Compile_WhenCompilationExceedsTheLimit_FailsWithCompilationTimeLimitExceeded`` () =
    Script.compileWith { Script.defaults with CompilationTimeout = Some tick } bigSource
    |> fails MessageCode.CompilationTimeLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1449")>]
let ``Compile_WithAGenerousLimit_Succeeds`` () =
    let r = Script.compileWith { Script.defaults with CompilationTimeout = Some (TimeSpan.FromSeconds 60.) } bigSource
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1449")>]
let ``CompileOrThrow_WhenCompilationExceedsTheLimit_ThrowsWithTheCode`` () =
    throwsWithCode MessageCode.CompilationTimeLimitExceeded (fun () ->
        (Script.compilerFor { Script.defaults with CompilationTimeout = Some tick }).WithSource(bigSource).CompileOrThrow())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1449")>]
let ``CompileToArtifact_WhenCompilationExceedsTheLimit_Fails`` () =
    Script.compileBytesWith { Script.defaults with CompilationTimeout = Some tick } bigSource
    |> fails MessageCode.CompilationTimeLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1449")>]
let ``LoadingAnArtifact_WhenLoadExceedsTheLimit_Fails`` () =
    let bytes = Script.compileBytesWith Script.defaults bigSource |> Script.valueOf
    let r = Script.loadBytesWith { Script.defaults with CompilationTimeout = Some tick } (bytes)
    fails MessageCode.CompilationTimeLimitExceeded r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1449")>]
let ``CompileWithALibrary_WhenCompilationExceedsTheLimit_Fails`` () =
    Script.compileWith { Script.defaults with CompilationTimeout = Some tick; Libraries = [ bigLibrary ] } "Output.r = F1(1)"
    |> fails MessageCode.CompilationTimeLimitExceeded

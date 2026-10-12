/// jyro-spec 95.15 Sleep, 95.16 Diff.
module RealTree.Jyro.Spec.Tests.S95_15_16

open System
open System.Diagnostics
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

// =========================================================================
// 95.15 Sleep
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2034")>]
let ``Sleep_TakesOneNumberAndReturnsNull`` () =
    Assert.Equal(norm """["null","null"]""", rJson "var a = Sleep(1)\nvar b = Sleep(milliseconds: 1)\nOutput.r = [TypeOf(a), TypeOf(b)]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2034")>]
let ``Sleep_WithNoArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Sleep()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2034")>]
let ``Sleep_WithTwoArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Sleep(1, 2)"

// Measurement behind every wall-clock bound in this section (floors-process
// 4.1.18, JYRO-DEF-455). ZZ_MarginProbe (not committed): `dotnet test` of this
// project at 4b7eadd, 2026-10-09, 393 samples taken while its other tests ran
// (a run of the P100_170 project overlapped). From the call of Execute() on a
// compiled program to its end, the longest was: `Sleep(0)\nSleep(0)\nOutput.r = 1`
// 2.31 ms; `Output.r = Sleep(0)` 0.11 ms; the 1000-iteration loop with Sleep(0)
// minus the same loop without it 6.46 ms. 15 ms is 136 times 0.11 ms; 200 ms is
// 31 times 6.46 ms; 50 ms is 21 times 2.31 ms.
/// True when `v` is an object that has the property `name`.
let private hasProp (name: string) (v: JyroValue) : bool =
    match v with
    | :? JyroObject as o -> fst (o.TryGetValue name)
    | _ -> false

let private timeRun (src: string) (limits: JyroExecutionOptions) =
    let program = Script.valueOf (Script.compile src)
    let runner = Script.runnerFor { Script.defaults with Limits = Some limits } program
    let sw = Stopwatch.StartNew()
    let res = runner.Execute()
    sw.Stop()
    res, sw.Elapsed

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2163")>]
let ``JYRO-REQ-2163 Sleep_OfZero_ReturnsWithNoPause`` () =
    let unlimited = JyroExecutionOptions.Unlimited
    // warm up both scripts once
    timeRun "var x = Sleep(0)" unlimited |> ignore
    let one, oneTime = timeRun "Output.r = Sleep(0)" unlimited
    Assert.True(Script.ok one, Script.describe one)
    expectNull (Script.output one |> Values.prop "r")
    Assert.True(oneTime < TimeSpan.FromMilliseconds 15., sprintf "one Sleep(0) took %O" oneTime)
    let baseSrc = "var n = 0\nfor i in 0 to 1000 do n = n + 1 end"
    let sleepSrc = "var n = 0\nfor i in 0 to 1000 do n = n + 1\nSleep(0) end"
    timeRun baseSrc unlimited |> ignore
    timeRun sleepSrc unlimited |> ignore
    let b, baseTime = timeRun baseSrc unlimited
    let s, sleepTime = timeRun sleepSrc unlimited
    Assert.True(Script.ok b && Script.ok s, Script.describe s)
    Assert.True(sleepTime - baseTime < TimeSpan.FromMilliseconds 200., sprintf "1000 Sleep(0) added %O" (sleepTime - baseTime))

/// The clause needs a limit under one second (50 ms): its test runs in a
/// collection that never runs in parallel with another test of the project
/// (floors-process 4.1.18). The measurement is the one above: 2.31 ms at most,
/// 50 ms being 21 times it.
[<CollectionDefinition("SubSecondLimit", DisableParallelization = true)>]
type SubSecondLimitCollection() = class end

[<Collection("SubSecondLimit")>]
module SubSecond =
    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2163")>]
    let ``JYRO-REQ-2163 Sleep_OfZero_UnderA50msTimeLimit_RaisesNoError`` () =
        let program = Script.valueOf (Script.compile "Sleep(0)\nSleep(0)\nOutput.r = 1")
        // Warm-up of the same program under no limit, so that the 50 ms covers the run, not a first JIT.
        (Script.runnerFor { Script.defaults with Limits = Some JyroExecutionOptions.Unlimited } program).Execute() |> ignore
        let res, _ = timeRun "Sleep(0)\nSleep(0)\nOutput.r = 1" (Script.limits (fun o -> { o with MaxExecutionTime = TimeSpan.FromMilliseconds 50. }))
        Assert.True(Script.ok res, Script.describe res)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2163")>]
    let ``JYRO-REQ-2163 Sleep_OfZero_AsTheOnlyStatement_UnderA50msTimeLimit_GivesNull`` () =
        let src = "Output.r = Sleep(0)"
        let limits = Script.limits (fun o -> { o with MaxExecutionTime = TimeSpan.FromMilliseconds 50. })
        timeRun src JyroExecutionOptions.Unlimited |> ignore
        let res, elapsed = timeRun src limits
        Assert.True(Script.ok res, Script.describe res)
        expectNull (Script.output res |> Values.prop "r")
        Assert.Empty(res.Messages)
        Assert.True(elapsed < TimeSpan.FromMilliseconds 50., sprintf "Sleep(0) took %O" elapsed)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2163")>]
    let ``JYRO-REQ-2163 Sleep_OfZero_AfterTheHostRequestedCancellation_EndsWithCanceledByHost`` () =
        use cts = new Threading.CancellationTokenSource()
        let stop =
            Host.func "Stop" [] ParameterType.Any FunctionEffect.Pure (fun _ -> cts.Cancel(); Values.nul)
        let res =
            Script.runWith
                { bounded with Functions = [ stop.AsFunction ]; Cancellation = Some cts.Token }
                "Output.r = [Stop(), Sleep(0)]\nOutput.after = 1"
        Script.expectError MessageCode.CanceledByHost res
        Assert.Equal(1, stop.CallCount)
        match Script.value res with
        | Some v ->
            Assert.False(hasProp "r" v, "Output.r is present")
            Assert.False(hasProp "after" v, "the statement after Sleep(0) ran")
        | None -> ()

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2163")>]
    let ``JYRO-REQ-2163 Sleep_OfZero_AfterTheTimeLimitHasEnded_EndsWithExecutionTimeLimitExceeded`` () =
        let slow =
            Host.func "Slow" [] ParameterType.Any FunctionEffect.Pure (fun _ -> Threading.Thread.Sleep 300; Values.nul)
        let limits = { specLimits with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }
        let res =
            Script.runWith
                { bounded with Functions = [ slow.AsFunction ]; Limits = Some limits }
                "Output.r = [Slow(), Sleep(0)]\nOutput.after = 1"
        Script.expectError MessageCode.ExecutionTimeLimitExceeded res
        Assert.Equal(1, slow.CallCount)
        match Script.value res with
        | Some v ->
            Assert.False(hasProp "r" v, "Output.r is present")
            Assert.False(hasProp "after" v, "the statement after Sleep(0) ran")
        | None -> ()

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2035")>]
[<InlineData("null")>]
[<InlineData("x")>]
[<InlineData("'10'")>]
[<InlineData("''")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("[]")>]
[<InlineData("[10]")>]
[<InlineData("{}")>]
[<InlineData("f")>]
let ``Sleep_WhenMsIsNotANumber_RaisesArgumentTypeMismatch`` (arg: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var x = null\nvar f = y => y\nSleep(%s)" arg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1198")>]
let ``Sleep_HasEffectBlocking_AndCompilesUnderBlockingCeiling`` () =
    assertEffect "Sleep" FunctionEffect.Blocking "Sleep(0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1198")>]
let ``Sleep_UnderPureOrNonDeterministicCeiling_IsRejected`` () =
    assertRejectedUnder FunctionEffect.Pure "Sleep(0)"
    assertRejectedUnder FunctionEffect.NonDeterministic "Sleep(0)"

// =========================================================================
// 95.16 Diff
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1199")>]
let ``Diff_TakesTwoObjectsAndReturnsAnObject`` () =
    let src = "Output.r = [TypeOf(Diff({a: 1}, {a: 2})), Diff(first: {}, second: {b: 1}).added, Diff(second: {b: 1}, first: {}).added]"
    Assert.Equal(norm """["object",{"b":1},{"b":1}]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1199")>]
let ``Diff_WithOneArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = Diff({})"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1199")>]
let ``Diff_WithThreeArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = Diff({}, {}, {})"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1200")>]
[<InlineData("{}, {}")>]
[<InlineData("{a: 1}, {b: 2}")>]
[<InlineData("{a: 1}, {a: 2}")>]
[<InlineData("{added: 1, removed: 2, changed: 3}, {changed: 4, added: 1, x: 5}")>]
let ``Diff_ResultHasExactlyAddedRemovedChangedInOrder_EachAnObject`` (args: string) =
    let src = sprintf "var d = Diff(%s)\nOutput.r = [Keys(d), TypeOf(d.added), TypeOf(d.removed), TypeOf(d.changed)]" args
    Assert.Equal(norm """[["added","removed","changed"],"object","object","object"]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1200")>]
let ``Diff_ReturnsANewObjectEachCall`` () =
    Assert.Equal(norm """[{"added":{},"removed":{},"changed":{}},{"added":{"z":1},"removed":{},"changed":{},"w":1}]""",
                 rJson "var a = {}\nvar d1 = Diff(a, a)\nvar d2 = Diff(a, a)\nd2.w = 1\nd2.added.z = 1\nOutput.r = [d1, d2]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1850")>]
[<InlineData("{a: 1, b: 2}, {b: 3, c: null}", "{\"c\":null}")>]
[<InlineData("{a: 1}, {a: 1}", "{}")>]
[<InlineData("{}, {}", "{}")>]
[<InlineData("{}, {a: 1, b: [2]}", "{\"a\":1,\"b\":[2]}")>]
[<InlineData("{a: 1}, {a: 1, b: 2, c: null}", "{\"b\":2,\"c\":null}")>]
[<InlineData("{}, {x: [1]}", "{\"x\":[1]}")>]
[<InlineData("{a: null}, {}", "{}")>]
[<InlineData("{a: 1}, {a: 2}", "{}")>]
let ``JYRO-REQ-1850 Diff_AddedHoldsKeysOnlyInSecond_WithSecondsValue`` (args: string, expected: string) =
    expectJson expected (sprintf "Output.r = Diff(%s).added" args)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1850")>]
[<InlineData("{a: 1, b: 2}", "{b: 3, c: null}")>]
[<InlineData("{a: 1, b: 2}", "{a: 1, b: 2, c: 3}")>]
[<InlineData("{a: 1}", "{a: 2, b: 2}")>]
[<InlineData("{a: null}", "{a: null, b: null}")>]
[<InlineData("{a: [1], b: {c: 1}}", "{a: [1], b: {c: 2}, d: 1}")>]
let ``JYRO-REQ-1850 Diff_Added_HasNoKeyThatBothObjectsHave_ForEqualAndUnequalValues`` (first: string, second: string) =
    let src =
        sprintf "var f = %s\nvar s = %s\nvar d = Diff(f, s)\nvar bad = 0\nvar notInSecond = 0\nforeach k in Keys(d.added) do if HasProperty(f, k) then bad = bad + 1 end\nif HasProperty(s, k) == false then notInSecond = notInSecond + 1 end end\nvar missing = 0\nforeach k in Keys(s) do if HasProperty(f, k) == false and HasProperty(d.added, k) == false then missing = missing + 1 end end\nOutput.r = [bad, notInSecond, missing]" first second
    Assert.Equal(norm "[0,0,0]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1850")>]
let ``JYRO-REQ-1850 Diff_AddedOfTheClausesExample_HasExactlyOnePropertyC_WhoseValueIsNull`` () =
    Assert.Equal(norm """[["c"],null,1]""", rJson "var a = Diff({a: 1, b: 2}, {b: 3, c: null}).added\nOutput.r = [Keys(a), a.c, Length(Keys(a))]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1850")>]
let ``JYRO-REQ-1850 Diff_OfAnEmptyFirstObject_HasAllOfSecondInAdded`` () =
    Assert.Equal(norm """[{"a":1,"b":null},{"a":1,"b":null}]""", rJson "var s = {a: 1, b: null}\nOutput.r = [Diff({}, s).added, s]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1852")>]
[<InlineData("{a: 1, b: 2}, {b: 3, c: null}", "{\"a\":1}")>]
[<InlineData("{a: 1}, {a: 1}", "{}")>]
[<InlineData("{}, {}", "{}")>]
[<InlineData("{a: 1, b: [2]}, {}", "{\"a\":1,\"b\":[2]}")>]
[<InlineData("{a: 1, b: 2, c: null}, {a: 1}", "{\"b\":2,\"c\":null}")>]
[<InlineData("{x: [1]}, {}", "{\"x\":[1]}")>]
[<InlineData("{}, {a: null}", "{}")>]
[<InlineData("{a: 1}, {a: 2}", "{}")>]
let ``JYRO-REQ-1852 Diff_RemovedHoldsKeysOnlyInFirst_WithFirstsValue`` (args: string, expected: string) =
    expectJson expected (sprintf "Output.r = Diff(%s).removed" args)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1852")>]
[<InlineData("{a: 1, b: 2}", "{b: 3, c: null}")>]
[<InlineData("{a: 1, b: 2}", "{a: 1, b: 2, c: 3}")>]
[<InlineData("{a: 1, b: 2}", "{a: 2}")>]
[<InlineData("{a: null, b: null}", "{a: null}")>]
[<InlineData("{a: [1], b: {c: 1}, d: 1}", "{a: [1], b: {c: 2}}")>]
let ``JYRO-REQ-1852 Diff_Removed_HasNoKeyThatBothObjectsHave_ForEqualAndUnequalValues`` (first: string, second: string) =
    let src =
        sprintf "var f = %s\nvar s = %s\nvar d = Diff(f, s)\nvar bad = 0\nvar notInFirst = 0\nforeach k in Keys(d.removed) do if HasProperty(s, k) then bad = bad + 1 end\nif HasProperty(f, k) == false then notInFirst = notInFirst + 1 end end\nvar missing = 0\nforeach k in Keys(f) do if HasProperty(s, k) == false and HasProperty(d.removed, k) == false then missing = missing + 1 end end\nOutput.r = [bad, notInFirst, missing]" first second
    Assert.Equal(norm "[0,0,0]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1852")>]
let ``JYRO-REQ-1852 Diff_RemovedOfTheClausesExample_HasExactlyOnePropertyA_WhoseValueIs1`` () =
    Assert.Equal(norm """[["a"],1]""", rJson "var r = Diff({a: 1, b: 2}, {b: 3, c: null}).removed\nOutput.r = [Keys(r), r.a]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1852")>]
let ``JYRO-REQ-1852 Diff_OfAnEmptySecondObject_HasAllOfFirstInRemoved`` () =
    Assert.Equal(norm """[{"a":1,"b":null},{"a":1,"b":null}]""", rJson "var f = {a: 1, b: null}\nOutput.r = [Diff(f, {}).removed, f]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1855")>]
[<InlineData("{a: 1}, {a: 2}", "{\"a\":{\"from\":1,\"to\":2}}")>]
[<InlineData("{a: 1}, {a: '1'}", "{\"a\":{\"from\":1,\"to\":\"1\"}}")>]
[<InlineData("{a: null}, {a: 0}", "{\"a\":{\"from\":null,\"to\":0}}")>]
[<InlineData("{a: []}, {a: {}}", "{\"a\":{\"from\":[],\"to\":{}}}")>]
[<InlineData("{a: false}, {a: 0}", "{\"a\":{\"from\":false,\"to\":0}}")>]
let ``JYRO-REQ-1855 Diff_ChangedHoldsFromAndToForUnequalValues`` (args: string, expected: string) =
    let src = sprintf "var d = Diff(%s)\nOutput.r = d.changed\nOutput.k = Keys(d.changed.a)" args
    let out = Script.outputOf src
    Assert.Equal(Values.toJson (Values.json expected), Values.toJson (Values.prop "r" out))
    Assert.Equal(norm """["from","to"]""", Values.toJson (Values.prop "k" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1855")>]
let ``JYRO-REQ-1855 Diff_ChangedComparesNumbersByValue_ExactAgainstInexact`` () =
    let input = Values.obj [ "i", Values.inexact 0.1; "j", Values.inexact 0.30000000000000004 ]
    let v = rVal input "Output.r = [Diff({a: 0.1}, {a: Input.i}).changed, Keys(Diff({a: 0.3}, {a: Input.j}).changed)]"
    Assert.Equal(norm """[{},["a"]]""", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1855")>]
let ``JYRO-REQ-1855 Diff_ChangedEntryIsANewObjectEachCall`` () =
    Assert.Equal(norm "[1,9]", rJson "var f = {a: 1}\nvar s = {a: 2}\nvar c1 = Diff(f, s).changed.a\nvar c2 = Diff(f, s).changed.a\nc2.from = 9\nOutput.r = [c1.from, c2.from]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1855")>]
let ``JYRO-REQ-1855 Diff_ChangedOfTheClausesExamples`` () =
    // Exactly the key b, with from 2 and to 3, in that order.
    Assert.Equal(norm """[["b"],["from","to"],2,3]""", rJson "var c = Diff({a: 1, b: 2}, {b: 3, c: null}).changed\nOutput.r = [Keys(c), Keys(c.b), c.b.from, c.b.to]")
    // Equal values, null included, are in no part: changed has no key.
    Assert.Equal(norm """[[],0]""", rJson "var c = Diff({n: null, k: 1}, {n: null, k: 1}).changed\nOutput.r = [Keys(c), Length(Keys(c))]")
    // A nested difference shows as the whole top-level values.
    Assert.Equal(norm """[["x"],[1,[2]],[1,[3]]]""", rJson "var c = Diff({x: [1, [2]]}, {x: [1, [3]]}).changed\nOutput.r = [Keys(c), c.x.from, c.x.to]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1855")>]
[<InlineData("{a: 1, b: 2}", "{b: 3, c: null}", "[\"b\"]")>]
[<InlineData("{a: 1, b: 2}", "{a: 1, b: 2}", "[]")>]
[<InlineData("{a: 1, b: 2}", "{c: 1, d: 2}", "[]")>]
[<InlineData("{a: 1, b: 2, c: 3}", "{a: 1, b: 5, c: 6}", "[\"b\",\"c\"]")>]
[<InlineData("{a: null}", "{a: null}", "[]")>]
[<InlineData("{a: [1, {b: 2}]}", "{a: [1, {b: 2}]}", "[]")>]
[<InlineData("{a: 1}", "{}", "[]")>]
[<InlineData("{}", "{a: 1}", "[]")>]
let ``JYRO-REQ-1855 Diff_Changed_HoldsExactlyTheSharedKeysWithUnequalValues_AndNoOtherKey`` (first: string, second: string, keys: string) =
    expectJson keys (sprintf "Output.r = Keys(Diff(%s, %s).changed)" first second)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1855")>]
let ``JYRO-REQ-1855 Diff_ChangedEntries_HaveExactlyFromThenTo_AndAreNewObjects`` () =
    let src = "var f = {a: [1], b: 2}\nvar s = {a: [2], b: 3}\nvar c = Diff(f, s).changed\nOutput.r = [Keys(c.a), Keys(c.b), c.a.from, c.a.to]"
    Assert.Equal(norm """[["from","to"],["from","to"],[1],[2]]""", rJson src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1204")>]
[<InlineData("{a: 1, b: null, c: [1, {d: 2}], e: 's'}, {a: 1, b: null, c: [1, {d: 2}], e: 's'}")>]
[<InlineData("{n: 0.3}, {n: 0.1 + 0.2}")>]
[<InlineData("{f: g}, {f: g}")>]
[<InlineData("{o: {x: 1, y: 2}}, {o: {y: 2, x: 1}}")>]
let ``Diff_KeyWithEqualValues_IsInNoPart`` (args: string) =
    let src = sprintf "var g = x => x\nOutput.r = Diff(%s)" args
    Assert.Equal(norm """{"added":{},"removed":{},"changed":{}}""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1205")>]
let ``Diff_ComparesTopLevelOnly_NestedDifferenceIsTheWholeValue`` () =
    let src = "var d = Diff({n: {x: 1, y: 2}, l: [1, [2]]}, {n: {x: 1, y: 3}, l: [1, [3]]})\nOutput.r = [d.changed, Keys(d.changed)]"
    Assert.Equal(norm """[{"n":{"from":{"x":1,"y":2},"to":{"x":1,"y":3}},"l":{"from":[1,[2]],"to":[1,[3]]}},["n","l"]]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1206")>]
let ``JYRO-REQ-1206 Diff_AddedFollowsSecond_RemovedAndChangedFollowFirst`` () =
    let src =
        "var f = {}\nf.c = 1\nf.a = 1\nf.b = 1\nf.q = 1\nf.p = 1\n"
        + "var s = {}\ns.z = 1\ns.y = 1\ns.b = 2\ns.a = 2\ns.x = 1\ns.c = 2\n"
        + "var d = Diff(f, s)\nOutput.r = [Keys(d.added), Keys(d.removed), Keys(d.changed)]"
    Assert.Equal(norm """[["z","y","x"],["q","p"],["c","a","b"]]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1206")>]
let ``JYRO-REQ-1206 Diff_AddedFollowsSecondAfterDeleteAndReAdd`` () =
    let src = "var s = {m: 1, n: 1, o: 1}\ndelete s.m\ns.m = 1\nOutput.r = Keys(Diff({}, s).added)"
    Assert.Equal(norm """["n","o","m"]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1206")>]
let ``JYRO-REQ-1206 Diff_RemovedAndChangedFollowFirstAfterDeleteAndReAdd`` () =
    // first's order is b, c, a after the delete and re-add; second's is a, b, c.
    let src =
        "var f = {a: 1, b: 1, c: 1}\ndelete f.a\nf.a = 1\n"
        + "Output.r = [Keys(Diff(f, {}).removed), Keys(Diff(f, {a: 2, b: 2, c: 2}).changed), Keys(Diff(f, {c: 1}).removed)]"
    Assert.Equal(norm """[["b","c","a"],["b","c","a"],["b","a"]]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1206")>]
let ``JYRO-REQ-1206 Diff_OfObjectsWhoseKeysAreInOppositeOrders_EachPartFollowsItsSource`` () =
    // first: k1..k6 then r1, r2; second: s1, s2, then k6..k1 with new values.
    let src =
        "var f = {}\nfor i in 1 to 7 do f['k' & i] = i end\nf.r1 = 1\nf.r2 = 1\n"
        + "var s = {}\ns.s1 = 1\ns.s2 = 1\nfor i in 6 downto 0 do s['k' & i] = i * 10 end\n"
        + "var d = Diff(f, s)\nOutput.r = [Keys(d.added), Keys(d.removed), Keys(d.changed)]"
    Assert.Equal(norm """[["s1","s2"],["r1","r2"],["k1","k2","k3","k4","k5","k6"]]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1206")>]
let ``JYRO-REQ-1206 Diff_OfFromJsonAndHostInput_FollowTheirTextOrder`` () =
    let src = "var d = Diff(Input.f, FromJson('{\"y\":1,\"b\":9,\"x\":1,\"a\":9}'))\nOutput.r = [Keys(d.added), Keys(d.removed), Keys(d.changed)]"
    Assert.Equal(norm """[["y","x"],["d","c"],["b","a"]]""",
                 Values.toJson (rIn """{"f":{"d":1,"b":1,"c":1,"a":1}}""" src))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1207")>]
let ``Diff_ValuesAreThePropertyValuesThemselves`` () =
    let src =
        "var s = [1]\nvar t = {k: 1}\nvar fc = [0]\nvar r = Diff({a: s, c: fc}, {b: t, c: s})\n"
        + "r.removed.a[0] = 9\nr.added.b.k = 5\nr.changed.c.from[0] = 4\nOutput.r = [s[0], t.k, fc[0], r.changed.c.to[0]]"
    Assert.Equal(norm "[9,5,4,9]", rJson src)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1208")>]
[<InlineData("null, {a: 1}", "{\"added\":{\"a\":1},\"removed\":{},\"changed\":{}}")>]
[<InlineData("{a: 1}, null", "{\"added\":{},\"removed\":{\"a\":1},\"changed\":{}}")>]
[<InlineData("null, null", "{\"added\":{},\"removed\":{},\"changed\":{}}")>]
[<InlineData("x, y", "{\"added\":{},\"removed\":{},\"changed\":{}}")>]
let ``Diff_TreatsNullArgumentAsEmptyObject`` (args: string, expected: string) =
    expectJson expected (sprintf "var x = null\nvar y = null\nOutput.r = Diff(%s)" args)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1209")>]
[<InlineData("[]")>]
[<InlineData("[1]")>]
[<InlineData("'a'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``Diff_WhenFirstNotObjectOrNull_RaisesArgumentTypeMismatch`` (arg: string) =
    failsIn "[1]" MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Diff(%s, {})" arg)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1209")>]
[<InlineData("[]")>]
[<InlineData("[1]")>]
[<InlineData("'a'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("f")>]
[<InlineData("Input")>]
let ``Diff_WhenSecondNotObjectOrNull_RaisesArgumentTypeMismatch`` (arg: string) =
    failsIn "[1]" MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = Diff({}, %s)" arg)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1857")>]
[<InlineData("{k: a}, {k: b}")>]
[<InlineData("{k: o}, {k: p}")>]
[<InlineData("{k: a}, {k: a}")>]
[<InlineData("{k: o}, {k: o}")>]
let ``JYRO-REQ-1857 Diff_WhenEqualityMeetsACycle_RaisesValueNestingLimitExceeded`` (args: string) =
    let src = sprintf "var a = [1]\na[0] = a\nvar b = [1]\nb[0] = b\nvar o = {}\no.s = o\nvar p = {}\np.s = p\nOutput.r = Diff(%s)" args
    fails MessageCode.ValueNestingLimitExceeded src

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1857")>]
let ``JYRO-REQ-1857 Diff_TheClausesExample_OfACycleAsTheValueOfASharedKey_Raises`` () =
    fails MessageCode.ValueNestingLimitExceeded "var c = {}\nc.self = c\nOutput.r = Diff({x: c}, {x: c})"
    // As c == c does.
    fails MessageCode.ValueNestingLimitExceeded "var c = {}\nc.self = c\nOutput.r = c == c"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1857")>]
[<InlineData("{x: c}, {x: 1}")>]
[<InlineData("{x: c}, {x: 'a'}")>]
[<InlineData("{x: c}, {x: null}")>]
[<InlineData("{x: 1}, {x: c}")>]
[<InlineData("{x: c}, {x: [1]}")>]
[<InlineData("{x: c}, {}")>]
[<InlineData("{}, {x: c}")>]
[<InlineData("{x: c}, {y: c}")>]
[<InlineData("{x: c, y: 1}, {y: 2}")>]
let ``JYRO-REQ-1857 Diff_WhenNoSharedKeyHasTwoValuesThatEqualityWalksThroughACycle_DoesNotRaise`` (args: string) =
    // Different types are unequal at once (30.10.1) and a key held by one object is not compared: no walk, no error.
    let src = sprintf "var c = {}\nc.self = c\nvar d = Diff(%s)\nOutput.r = [TypeOf(d.added), TypeOf(d.removed), TypeOf(d.changed)]" args
    Assert.Equal(norm """["object","object","object"]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1857")>]
let ``JYRO-REQ-1857 Diff_OfACycleInAddedOrRemoved_IsNotCompared_AndGivesThatKeyThere`` () =
    let src = "var c = {}\nc.self = c\nvar d = Diff({a: 1}, {a: 1, x: c})\nvar e = Diff({a: 1, x: c}, {a: 1})\nOutput.r = [Keys(d.added), Keys(d.removed), Keys(e.removed), Keys(e.added), Keys(d.changed), Keys(e.changed)]"
    Assert.Equal(norm """[["x"],[],["x"],[],[],[]]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1211")>]
let ``Diff_HasEffectPure_AndCompilesUnderPureCeiling`` () =
    assertEffect "Diff" FunctionEffect.Pure "Output.r = Diff({}, {})"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1211")>]
let ``Diff_DoesNotChangeItsArguments`` () =
    let src = "var f = {a: 1, b: [1], c: {d: 1}}\nvar s = {b: [2], c: {d: 1}, e: 5}\nvar sf = ToJson(f)\nvar ss = ToJson(s)\nvar d = Diff(f, s)\nOutput.r = [ToJson(f) == sf, ToJson(s) == ss]"
    Assert.Equal(norm "[true,true]", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1211")>]
let ``Diff_OfReadOnlyArguments_RaisesNoWriteError`` () =
    Assert.Equal(norm """{"added":{"y":2},"removed":{},"changed":{"x":{"from":1,"to":3}}}""",
                 Values.toJson (rIn """{"a":{"x":1},"b":{"x":3,"y":2}}""" "Output.r = Diff(Input.a, Input.b)"))

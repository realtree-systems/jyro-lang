/// jyro-spec 70.4 (Input stays read-only) and 70.5 (no console).
/// From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S70_04_05_InputProtection

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.S70_01_03_InputOutput

let private xJson = "{\"o\":{\"a\":[1,{\"b\":2}]},\"n\":1.5,\"s\":\"x\"}"

/// Assert that a host write to `c` throws a JyroRuntimeException with the code
/// InputValueIsReadOnly (70.4.5).
let assertHostWriteRefused (c: JyroValue) =
    let ex =
        match c with
        | :? JyroArray as a -> Assert.ThrowsAny<JyroRuntimeException>(fun () -> a.Add(Values.int 9))
        | _ -> Assert.ThrowsAny<JyroRuntimeException>(fun () -> c.SetProperty("w", Values.int 9))
    Assert.Equal(MessageCode.InputValueIsReadOnly, ex.Code)

// ===========================================================================
// 70.4.1 Protection applies to the host's Jyro value itself, not a copy
// ===========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_GivenAsJyroValue_ScriptSeesTheVeryInstancesAtEveryLevel`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let o = Values.prop "o" x
    let a = Values.prop "a" o
    let fns = [ sameAs "SameX" x; sameAs "SameO" o; sameAs "SameA" a; probe "Probe" ]
    let r =
        Script.runWith (withFns fns (inputOf x))
            "Output.r = [SameX(Input), SameO(Input.o), SameA(Input.o.a), Probe(Input), Probe(Input.o), Probe(Input.o.a)]"
    Assert.All(Values.asList (outProp "r" r), fun v -> Assert.True(Values.asBool v))
    Assert.True(x.IsFrozen && o.IsFrozen && a.IsFrozen, "the host's own instances are not read-only")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_SameJyroValueRunTwiceInSequence_BothRunWithoutAHostCopy`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let same = sameAs "SameX" x
    for _ in 1 .. 2 do
        let r = Script.runWith (withFns [ same ] (inputOf x)) "Output.s = SameX(Input)"
        Assert.True(Values.asBool (outProp "s" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_SameJyroValueRunTwiceInSequence_GivesTheSameResultBothTimes`` () =
    let x = Values.json "{\"o\":{\"a\":[1,2]},\"n\":3}"
    let source = "var o = Input.o\nOutput.r = [Input.n, o.a[1], Length(o.a), Input.o.a]"
    let first = Script.runWith (inputOf x) source
    let second = Script.runWith (inputOf x) source
    Assert.True(first.IsSuccess, Script.describe first)
    Assert.True(second.IsSuccess, Script.describe second)
    Assert.Equal(Values.toJson (Script.output first), Values.toJson (Script.output second))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_AHostFunctionThatWritesToTheInstanceItReceives_IsRefusedDuringTheExecution`` () =
    let x = Values.json "{\"o\":{\"a\":[1]},\"s\":{}}"
    let outcomes = ResizeArray<string>()
    let attempt (name: string) (target: JyroValue) (write: JyroValue -> unit) =
        try
            write target
            outcomes.Add(name + ": no exception")
        with
        | :? JyroRuntimeException as e -> outcomes.Add(name + ": " + string e.Code)
    let poke =
        Host.func "Poke" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args ->
                let root = args.Head
                attempt "root" root (fun v -> v.SetProperty("k", Values.int 1))
                attempt "object" (Values.prop "o" root) (fun v -> v.SetProperty("k", Values.int 1))
                attempt "array" (Values.prop "a" (Values.prop "o" root)) (fun v -> (v :?> JyroArray).Add(Values.int 2))
                Values.nul)
    let r = Script.runWith (withFns [ poke ] (inputOf x)) "Poke(Input)\nPoke(Input)"
    Assert.True(r.IsSuccess, Script.describe r)
    let expected = [ "root"; "object"; "array" ] |> List.map (fun n -> n + ": " + string MessageCode.InputValueIsReadOnly)
    Assert.Equal<string list>(expected @ expected, List.ofSeq outcomes)
    Assert.Equal("{\"o\":{\"a\":[1]},\"s\":{}}", x.ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_SameJyroValueInManyConcurrentExecutions_NoneFailsAndAllGiveTheSameResult`` () =
    let x = Values.json "{\"o\":{\"a\":[1,2,3]},\"n\":3}"
    let source = "var s = 0\nforeach e in Input.o.a do s += e end\nOutput.r = [s, Input.n, Input.o]"
    let tasks = [ for _ in 1 .. 8 -> background (fun () -> Script.runWith (inputOf x) source) ]
    let results = tasks |> List.map await
    for r in results do Assert.True(r.IsSuccess, Script.describe r)
    let first = Values.toJson (Script.output results.Head)
    for r in results do Assert.Equal(first, Values.toJson (Script.output r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_SameJyroValueInTwoOverlappingExecutions_BothSeeTheSameInstance`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let same = sameAs "SameX" x
    let gate = Gate()
    let a = background (fun () -> Script.runWith (withFns [ same; gate.Function ] (inputOf x)) "Hold()\nOutput.s = SameX(Input)")
    gate.WaitEntered()
    let b = Script.runWith (withFns [ same ] (inputOf x)) "Output.s = SameX(Input)"
    gate.Release()
    Assert.True(Values.asBool (outProp "s" b))
    Assert.True(Values.asBool (outProp "s" (await a)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1846")>]
let ``JYRO-REQ-1846 Input_JyroValueGivenToJyroScriptExecuteScript_IsUsedAsItIsAndStaysReadOnly`` () =
    let x = Values.json "{\"o\":{\"a\":[1,2]},\"n\":3}"
    let source = "Output.r = [Input.n, Input.o.a[1], Input.o.a]"
    let first = JyroScript.executeScript source (box x)
    let second = JyroScript.executeScript source (box x)
    Assert.True(first.IsSuccess, Script.describe first)
    Assert.True(second.IsSuccess, Script.describe second)
    Assert.Equal(Values.toJson (Script.output first), Values.toJson (Script.output second))
    assertHostWriteRefused x
    assertHostWriteRefused (Values.prop "o" x)
    assertHostWriteRefused (Values.prop "a" (Values.prop "o" x))

// ===========================================================================
// 70.4.2 After the execution, the host's Jyro value stays read-only
// ===========================================================================

let endings : obj[] seq =
    [ "success", "Output.r = Input.o.a[0]"
      "fail", "fail \"stop\""
      "runtime error", "Output.r = 1 / 0"
      "write refused", "var o = Input.o\no.k = 1"
      "statement limit", "Output.a = 1\nOutput.b = 1\nOutput.c = 1\nOutput.d = 1\nOutput.e = 1"
      "canceled in Sleep", (sprintf "Sleep(%d)" RealTree.Jyro.Spec.HostCancel.unboundedSleepMs) ]
    |> Seq.map (fun (name, src) -> [| box name; box src |])

let runEnding (name: string) (x: JyroValue) (source: string) : JyroResult<JyroValue> =
    match name with
    | "statement limit" ->
        Script.runWith { inputOf x with Limits = Some(Script.limits (fun o -> { o with MaxStatements = 2 })) } source
    | "canceled in Sleep" ->
        use cts = new CancellationTokenSource()
        // From a thread of its own against a Sleep far longer than any late cancel (JYRO-DEF-745).
        RealTree.Jyro.Spec.HostCancel.cancelFromThread cts (TimeSpan.FromMilliseconds 50.0)
        Script.runWith { allEffects with Input = ValueInput x; Cancellation = Some cts.Token } source
    | _ -> Script.runWith (inputOf x) source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-280")>]
[<MemberData(nameof endings)>]
let ``Input_AfterExecutionEnds_EveryContainerStaysReadOnlyAndRefusesHostWrites`` (ending: string, source: string) =
    let x = Values.json xJson
    let kept = containers x
    let r = runEnding ending x source
    if ending = "success" then Assert.True(r.IsSuccess, Script.describe r)
    else Assert.False(r.IsSuccess, sprintf "%s: expected the execution to fail" ending)
    Assert.All(kept, fun c -> Assert.True(c.IsFrozen, sprintf "%s: a container is no longer read-only" ending))
    for c in kept do assertHostWriteRefused c

[<Fact>]
[<Trait("requirement", "JYRO-REQ-280")>]
let ``Input_AfterASecondUnrelatedExecutionEnds_StaysReadOnly`` () =
    let x = Values.json xJson
    Script.runWith (inputOf x) "Output.r = 1" |> ignore
    Script.runWith (inputOf (Values.json "{\"other\":{}}")) "Output.r = 1" |> ignore
    Assert.True(allFrozen x)
    for c in containers x do assertHostWriteRefused c

// ===========================================================================
// 70.4.3 The content of Input is the same after the execution as before
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-281")>]
[<MemberData(nameof endings)>]
let ``Input_ContentAfterExecution_EqualsContentBefore`` (ending: string, source: string) =
    let x = Values.json xJson
    (x :?> JyroObject).SetProperty("inexact", JyroNumber(0.1))
    let before = snapshot x
    runEnding ending x source |> ignore
    Assert.Equal(before, snapshot x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-281")>]
let ``Input_WhenTooDeepToProtect_ContentAfterEqualsContentBefore`` () =
    let root, _ = nestedObjects (maxDepth () + 1)
    root.SetProperty("n", JyroNumber(2.5m))
    let before = snapshot root
    Script.runWith (inputOf root) "Output.r = 1" |> ignore
    Assert.Equal(before, snapshot root)

let private hostDictionary () =
    let nested = Dictionary<string, obj>()
    nested.["x"] <- box "nx"
    let d = Dictionary<string, obj>()
    d.["list"] <- box (List<obj>([ box 1; box "two"; box true ]))
    d.["nested"] <- box nested
    d.["n"] <- box 7
    d

let private dump (d: Dictionary<string, obj>) = Text.Json.JsonSerializer.Serialize(d)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-281")>]
[<InlineData("Output.r = Input.list[0]")>]
[<InlineData("var i = Input\ni.k = 1")>]
[<InlineData("var l = Input.list\nl[0] = 5")>]
[<InlineData("fail")>]
let ``Input_GivenAsHostDictionary_ItsContentAfterEqualsContentBefore`` (source: string) =
    let d = hostDictionary ()
    let before = dump d
    Script.runWith { Script.defaults with Input = HostInput(box d) } source |> ignore
    Assert.Equal(before, dump d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-281")>]
let ``Input_SharedByTwoConcurrentExecutionsOneWriting_ContentAfterEqualsContentBefore`` () =
    let x = Values.json xJson
    let before = snapshot x
    let gate = Gate()
    let b = background (fun () -> Script.runWith (withFns [ gate.Function ] (inputOf x)) "Hold()\nOutput.r = Input.n")
    gate.WaitEntered()
    let a = Script.runWith (inputOf x) "var o = Input.o\no.k = 1"
    gate.Release()
    Script.expectError MessageCode.InputValueIsReadOnly a
    Assert.True((await b).IsSuccess)
    Assert.Equal(before, snapshot x)

// ===========================================================================
// 70.4.4 A host value or JSON text becomes a new, protected Jyro value
// ===========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenAsHostDictionary_TheNewValueIsReadOnlyAtEveryLevel`` () =
    let d = hostDictionary ()
    let af = allFrozenFn ()
    let r = Script.runWith (withFns [ af ] { Script.defaults with Input = HostInput(box d) }) "Output.p = AllFrozen(Input)"
    Assert.True(Values.asBool (outProp "p" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-282")>]
[<InlineData("var i = Input\ni.k = 1")>]
[<InlineData("var l = Input.list\nl[0] = 1")>]
[<InlineData("var n = Input.nested\nn.x = 1")>]
let ``Input_GivenAsHostDictionary_ScriptWriteRaisesCannotWriteToInputAndHostValueIsUnchanged`` (source: string) =
    let d = hostDictionary ()
    let before = dump d
    let r = Script.runWith { Script.defaults with Input = HostInput(box d) } source
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, dump d)
    // The host value itself was not made read-only.
    d.["added"] <- box 1
    (d.["list"] :?> List<obj>).Add(box 4)
    (d.["nested"] :?> Dictionary<string, obj>).["y"] <- box 2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenAsHostDictionary_ALaterHostChangeDuringTheExecutionIsNotSeen`` () =
    let d = hostDictionary ()
    let mutate =
        Host.func "Mutate" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
            d.["added"] <- box 1
            (d.["list"] :?> List<obj>).[0] <- box 100
            (d.["nested"] :?> Dictionary<string, obj>).["x"] <- box "changed"
            Values.nul)
    let r =
        Script.runWith (withFns [ mutate ] { Script.defaults with Input = HostInput(box d) })
            (lines [ "Mutate()"
                     "Output.l = Input.list[0]"
                     "Output.x = Input.nested.x"
                     "Output.has = HasProperty(Input, \"added\")" ])
    Assert.Equal(1.0, Values.asFloat (outProp "l" r))
    Assert.Equal("nx", Values.asString (outProp "x" r))
    Assert.False(Values.asBool (outProp "has" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenAsHostDictionary_AChangeByAnotherThreadWhileHeldIsNotSeen`` () =
    let d = hostDictionary ()
    let gate = Gate()
    let t =
        background (fun () ->
            Script.runWith (withFns [ gate.Function ] { Script.defaults with Input = HostInput(box d) })
                (lines [ "Hold()"; "Output.l = Input.list[0]"; "Output.x = Input.nested.x"; "Output.n = Input.n" ]))
    gate.WaitEntered()
    (d.["list"] :?> List<obj>).[0] <- box 100
    (d.["nested"] :?> Dictionary<string, obj>).["x"] <- box "changed"
    d.["n"] <- box 8
    gate.Release()
    let r = await t
    Assert.Equal(1.0, Values.asFloat (outProp "l" r))
    Assert.Equal("nx", Values.asString (outProp "x" r))
    Assert.Equal(7.0, Values.asFloat (outProp "n" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenAsJsonText_IsReadOnlyAtEveryLevel`` () =
    let af = allFrozenFn ()
    let r =
        Script.runWith (withFns [ af ] { Script.defaults with Input = JsonInput "{\"o\":{\"a\":[1]}}" })
            "Output.p = AllFrozen(Input)"
    Assert.True(Values.asBool (outProp "p" r))
    let w = Script.runWith { Script.defaults with Input = JsonInput "{\"o\":{\"a\":[1]}}" } "var a = Input.o.a\na[0] = 2"
    Script.expectError MessageCode.InputValueIsReadOnly w

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenAsJsonStream_ALaterChangeToTheStreamIsNotSeen`` () =
    let bytes = Encoding.UTF8.GetBytes "{\"v\":\"aaaa\"}"
    let stream = new MemoryStream(bytes.Length)
    stream.Write(bytes, 0, bytes.Length)
    stream.Position <- 0L
    let mutate =
        Host.func "Mutate" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
            let buffer = stream.GetBuffer()
            let other = Encoding.UTF8.GetBytes "{\"v\":\"bbbb\"}"
            Array.blit other 0 buffer 0 other.Length
            stream.Position <- 0L
            Values.nul)
    let af = allFrozenFn ()
    let r =
        Script.runWith
            (withFns [ mutate; af ] { Script.defaults with Input = NoInput; ConfigureRunner = fun runner -> runner.WithInputJson(stream) })
            (lines [ "Output.p = AllFrozen(Input)"; "Mutate()"; "Output.v = Input.v" ])
    Assert.True(Values.asBool (outProp "p" r))
    Assert.Equal("aaaa", Values.asString (outProp "v" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenToExecuteScriptAsHostValue_WriteIsRefusedAndHostValueIsUnchanged`` () =
    let d = hostDictionary ()
    let before = dump d
    let r = JyroScript.executeScript ("var i = Input\ni.k = 1") (box d)
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, dump d)
    d.["added"] <- box 1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenToExecuteScriptAsHostValueWithAList_WriteIsRefusedAndHostValueIsUnchanged`` () =
    let d = hostDictionary ()
    let before = dump d
    let r = JyroScript.executeScript ("var l = Input.list\nl[0] = 2") (box d)
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, dump d)
    (d.["list"] :?> List<obj>).Add(box 4)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-282")>]
let ``Input_GivenToExecuteScriptWithJson_WriteIsRefused`` () =
    let r = JyroScript.executeScriptWithJson ("var o = Input.o\no.k = 1") ("{\"o\":{}}")
    Script.expectError MessageCode.InputValueIsReadOnly r

// ===========================================================================
// 70.4.5 A host write to a read-only container throws InputValueIsReadOnly
// ===========================================================================

let private frozenFixture () =
    let x = Values.json "{\"o\":{\"k\":1,\"e\":{}},\"a\":[1,2],\"ea\":[],\"d\":{\"x\":{\"y\":{\"z\":{\"q\":1}}}}}"
    let r = Script.runWith (inputOf x) "Output.r = 1"
    Assert.True(r.IsSuccess, Script.describe r)
    x

let hostWrites : obj[] seq =
    // SetIndex is not public (JYRO-REQ-1876) and RemoveProperty is removed (JYRO-REQ-1901), so neither is listed.
    // JyroObject.Add is bound by JYRO-REQ-1901 and has its own test below.
    [ "JyroObject.SetProperty"; "JyroObject.SetProperty same value"
      "JyroObject.Remove"; "JyroObject.Remove absent"
      "JyroObject.Clear"; "JyroObject.Clear empty"; "JyroObject.Item set"
      "JyroArray.Add"; "JyroArray.Insert"; "JyroArray.RemoveAt"; "JyroArray.Clear"; "JyroArray.Clear empty"
      "JyroArray.Item set"
      "JyroValue.SetProperty"
      "depth 4 SetProperty" ]
    |> Seq.map (fun s -> [| box s |])

let private applyWrite (member': string) (x: JyroValue) : unit =
    let o = Values.prop "o" x :?> JyroObject
    let e = Values.prop "e" o :?> JyroObject
    let a = Values.prop "a" x :?> JyroArray
    let ea = Values.prop "ea" x :?> JyroArray
    let v = Values.int 9
    match member' with
    | "JyroObject.SetProperty" -> o.SetProperty("k", v)
    | "JyroObject.SetProperty same value" -> o.SetProperty("k", o.GetProperty "k")
    | "JyroObject.Remove" -> o.Remove("k") |> ignore
    | "JyroObject.Remove absent" -> o.Remove("absent") |> ignore
    | "JyroObject.Clear" -> o.Clear()
    | "JyroObject.Clear empty" -> e.Clear()
    | "JyroObject.Item set" -> o.["k"] <- v
    | "JyroArray.Add" -> a.Add(v)
    | "JyroArray.Insert" -> a.Insert(0, v)
    | "JyroArray.RemoveAt" -> a.RemoveAt(0)
    | "JyroArray.Clear" -> a.Clear()
    | "JyroArray.Clear empty" -> ea.Clear()
    | "JyroArray.Item set" -> a.[0] <- v
    | "JyroValue.SetProperty" -> (o :> JyroValue).SetProperty("k", v)
    | "depth 4 SetProperty" -> (x |> Values.prop "d" |> Values.prop "x" |> Values.prop "y" |> Values.prop "z").SetProperty("q", v)
    | other -> failwithf "unknown member %s" other

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2165")>]
[<MemberData(nameof hostWrites)>]
let ``JYRO-REQ-2165 HostWrite_ToReadOnlyContainer_ThrowsJyroRuntimeExceptionWithCannotWriteToInputAndLeavesItUnchanged`` (member': string) =
    let x = frozenFixture ()
    let before = snapshot x
    let ex = Assert.ThrowsAny<JyroRuntimeException>(fun () -> applyWrite member' x)
    Assert.Equal(MessageCode.InputValueIsReadOnly, ex.Code)
    Assert.Equal(before, snapshot x)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1901")>]
[<InlineData("o")>]
[<InlineData("e")>]
let ``JYRO-REQ-1901 JyroObject.Add to a read-only object throws JyroRuntimeException with InputValueIsReadOnly and leaves it unchanged`` (which: string) =
    let x = frozenFixture ()
    let o = Values.prop "o" x :?> JyroObject
    let target = if which = "o" then o else (Values.prop "e" o :?> JyroObject)
    let before = snapshot x
    let ex = Assert.ThrowsAny<JyroRuntimeException>(fun () -> target.Add("fresh", Values.int 9))
    Assert.Equal(MessageCode.InputValueIsReadOnly, ex.Code)
    Assert.Equal(before, snapshot x)

/// The five endings of an execution that do not need a thread, with a source that
/// reads the fixture of `frozenFixture`.
let endingsOfTheFixture : obj[] seq =
    [ "success", "Output.r = Input.a[0]"
      "fail", "fail \"stop\""
      "runtime error", "Output.r = 1 / 0"
      "write refused", "var o = Input.o\no.k = 1"
      "statement limit", "Output.a = 1\nOutput.b = 1\nOutput.c = 1\nOutput.d = 1\nOutput.e = 1" ]
    |> Seq.collect (fun (name, src) ->
        [ for w in hostWrites -> [| box name; box src; w.[0] |] ])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2165")>]
[<MemberData(nameof endingsOfTheFixture)>]
let ``JYRO-REQ-2165 HostWrite_AfterTheExecutionEnds_ThrowsJyroRuntimeExceptionWithCannotWriteToInputAndLeavesItUnchanged`` (ending: string, source: string, member': string) =
    let x = Values.json "{\"o\":{\"k\":1,\"e\":{}},\"a\":[1,2],\"ea\":[],\"d\":{\"x\":{\"y\":{\"z\":{\"q\":1}}}}}"
    let r = runEnding ending x source
    if ending = "success" then Assert.True(r.IsSuccess, Script.describe r)
    else Assert.False(r.IsSuccess, sprintf "%s: expected the execution to fail" ending)
    let before = snapshot x
    let ex = Assert.ThrowsAny<JyroRuntimeException>(fun () -> applyWrite member' x)
    Assert.Equal(MessageCode.InputValueIsReadOnly, ex.Code)
    Assert.Equal(before, snapshot x)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2165")>]
[<MemberData(nameof hostWrites)>]
let ``JYRO-REQ-2165 HostWrite_FromAHostFunctionDuringTheExecution_ThrowsCannotWriteToInputAndLeavesItUnchanged`` (member': string) =
    let x = Values.json "{\"o\":{\"k\":1,\"e\":{}},\"a\":[1,2],\"ea\":[],\"d\":{\"x\":{\"y\":{\"z\":{\"q\":1}}}}}"
    let outcomes = ResizeArray<string>()
    let apply =
        Host.func "Apply" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args ->
                try
                    applyWrite member' args.Head
                    outcomes.Add "no exception"
                with
                | :? JyroRuntimeException as e -> outcomes.Add(string e.Code)
                | e -> outcomes.Add(e.GetType().FullName)
                Values.nul)
    let before = snapshot x
    let r = Script.runWith (withFns [ apply ] (inputOf x)) "Apply(Input)"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal<string list>([ string MessageCode.InputValueIsReadOnly ], List.ofSeq outcomes)
    Assert.Equal(before, snapshot x)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2165")>]
[<MemberData(nameof hostWrites)>]
let ``JYRO-REQ-2165 HostWrite_ToWritableContainer_Succeeds`` (member': string) =
    let x = Values.json "{\"o\":{\"k\":1,\"e\":{}},\"a\":[1,2],\"ea\":[],\"d\":{\"x\":{\"y\":{\"z\":{\"q\":1}}}}}"
    applyWrite member' x

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2165")>]
[<MemberData(nameof hostWrites)>]
let ``JYRO-REQ-2165 HostWrite_ToAContainerOfTheOutputThatIsAlsoInput_ThrowsInputValueIsReadOnlyAndLeavesItUnchanged`` (member': string) =
    // The containers the script hands to its Output are the Input's own, so they stay read-only
    // after the execution ends (70.4.2), whichever value the host reaches them through.
    let x = Values.json "{\"o\":{\"k\":1,\"e\":{}},\"a\":[1,2],\"ea\":[],\"d\":{\"x\":{\"y\":{\"z\":{\"q\":1}}}}}"
    let r = Script.runWith (inputOf x) "Output.o = Input.o\nOutput.a = Input.a\nOutput.ea = Input.ea\nOutput.d = Input.d"
    Assert.True(r.IsSuccess, Script.describe r)
    let output = Script.output r
    let before = snapshot output
    let ex = Assert.ThrowsAny<JyroRuntimeException>(fun () -> applyWrite member' output)
    Assert.Equal(MessageCode.InputValueIsReadOnly, ex.Code)
    Assert.Equal(before, snapshot output)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2165")>]
let ``JYRO-REQ-2165 HostWrite_DuringTheExecution_ThrowsCannotWriteToInputAndLeavesTheValueUnchanged`` () =
    let x = Values.json "{\"o\":{\"k\":1}}"
    let before = snapshot x
    let codes = ResizeArray<string>()
    let poke =
        Host.func "Poke" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args ->
                try
                    args.Head.SetProperty("k", Values.int 2)
                    codes.Add "no exception"
                with
                | :? JyroRuntimeException as e -> codes.Add(string e.Code)
                | e -> codes.Add(e.GetType().FullName)
                Values.nul)
    let r = Script.runWith (withFns [ poke ] (inputOf x)) "Poke(Input.o)"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal<string list>([ string MessageCode.InputValueIsReadOnly ], List.ofSeq codes)
    Assert.Equal(before, snapshot x)

// ===========================================================================
// 70.5.1 No function writes to the console or a diagnostic stream
// ===========================================================================

type CaptureListener(sink: StringBuilder) =
    inherit TraceListener()
    override _.Write(message: string) = lock sink (fun () -> sink.Append(message) |> ignore)
    override _.WriteLine(message: string) = lock sink (fun () -> sink.AppendLine(message) |> ignore)

/// Run `work` with Console.Out, Console.Error, Trace and Debug captured, and
/// return everything written to them.
let captureStreams (work: unit -> unit) : string =
    let out = new StringWriter()
    let err = new StringWriter()
    let trace = StringBuilder()
    let listener = new CaptureListener(trace)
    let oldOut, oldErr = Console.Out, Console.Error
    Console.SetOut out
    Console.SetError err
    Trace.Listeners.Add listener |> ignore
    try
        work ()
    finally
        Console.SetOut oldOut
        Console.SetError oldErr
        Trace.Listeners.Remove listener
    out.ToString() + err.ToString() + trace.ToString()

[<Collection("ProcessWide")>]
type ``S70_05 No console``() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1854")>]
    member _.``JYRO-REQ-1854 EveryStdlibFunction_CalledValidlyAndInvalidly_WritesNothingToConsoleTraceOrDebug``() =
        let registered = StdlibRegistry.getAll() |> List.ofSeq
        let listed = Inventory.all |> List.map (fun l -> l.Name) |> set
        let calls =
            [ for l in Inventory.all do
                  yield l.FullCall
                  yield sprintf "%s(%s)" l.Name (List.replicate l.MaxArgs "{}" |> String.concat ", ") ]
            @ [ for fn in registered do
                  if not (listed.Contains fn.Name) then
                      let n = fn.Signature.MinArgs
                      yield sprintf "%s(%s)" fn.Name (List.replicate n "null" |> String.concat ", ")
                      yield sprintf "%s(%s)" fn.Name (List.replicate n "{}" |> String.concat ", ") ]
        let written =
            captureStreams (fun () ->
                for call in calls do
                    Script.runWith allEffects ("Output.r = " + call) |> ignore)
        Assert.True((written = ""), sprintf "Something was written to a host stream:\n%s" written)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1854")>]
    member _.``JYRO-REQ-1854 EveryStdlibFunction_GivenArgumentsOfEveryKind_WritesNothingToConsoleTraceOrDebug``() =
        let argumentSets = [ "null"; "-1"; "0"; "\"x\""; "[1, \"a\"]"; "{a: 1}"; "x => x"; "true" ]
        let calls =
            [ for l in Inventory.all do
                  for a in argumentSets do
                      yield sprintf "%s(%s)" l.Name (List.replicate l.MaxArgs a |> String.concat ", ")
                      yield sprintf "%s(%s)" l.Name (List.replicate l.MinArgs a |> String.concat ", ") ]
        let written =
            captureStreams (fun () ->
                for call in calls do
                    Script.runWith allEffects ("Output.r = " + call) |> ignore
                    Script.runWith allEffects ("var r = " + call + "\nfail \"x\"") |> ignore)
        Assert.True((written = ""), sprintf "Something was written to a host stream:\n%s" written)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1854")>]
    member _.``JYRO-REQ-1854 EveryErrorClass_WritesNothingToConsoleTraceOrDebug``() =
        let written =
            captureStreams (fun () ->
                Script.run "fail \"boom\"" |> ignore
                Script.run "exit \"bye\"" |> ignore
                Script.run "Output.r = 1 / 0" |> ignore
                Script.runWith { Script.defaults with Limits = Some(Script.limits (fun o -> { o with MaxStatements = 1 })) }
                    "Output.a = 1\nOutput.b = 2" |> ignore
                Script.runJson "{\"o\":{}}" "var o = Input.o\no.k = 1" |> ignore
                Script.run "Output.r = UnknownFn()" |> ignore
                Script.run "func (" |> ignore)
        Assert.True((written = ""), sprintf "Something was written to a host stream:\n%s" written)

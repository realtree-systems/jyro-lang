/// jyro-spec 70.1 to 70.3: Input and Output are supplied, Input is read-only,
/// Output is mutable. From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S70_01_03_InputOutput

open System
open System.Collections.Generic
open System.IO
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// ---------------------------------------------------------------------------
// Local helpers
// ---------------------------------------------------------------------------

/// True when every array and object reachable from `v` (and `v`) is read-only.
/// Iterative, so it works at any depth, and safe on shared sub-values.
let allFrozen (v: JyroValue) : bool =
    let seen = HashSet<JyroValue>(HashIdentity.Reference)
    let stack = Stack<JyroValue>()
    stack.Push v
    let mutable ok = true
    while ok && stack.Count > 0 do
        let x = stack.Pop()
        match x with
        | :? JyroObject as o when seen.Add x ->
            if not o.IsFrozen then ok <- false
            for kv in o do stack.Push kv.Value
        | :? JyroArray as a when seen.Add x ->
            if not a.IsFrozen then ok <- false
            for e in a do stack.Push e
        | _ -> ()
    ok

/// True when no array or object reachable from `v` is read-only.
let noneFrozen (v: JyroValue) : bool =
    let seen = HashSet<JyroValue>(HashIdentity.Reference)
    let stack = Stack<JyroValue>()
    stack.Push v
    let mutable ok = true
    while ok && stack.Count > 0 do
        let x = stack.Pop()
        match x with
        | :? JyroObject as o when seen.Add x ->
            if o.IsFrozen then ok <- false
            for kv in o do stack.Push kv.Value
        | :? JyroArray as a when seen.Add x ->
            if a.IsFrozen then ok <- false
            for e in a do stack.Push e
        | _ -> ()
    ok

/// Host function `AllFrozen(v)`: every container reachable from v is read-only.
let allFrozenFn () =
    Host.func "AllFrozen" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Boolean
        FunctionEffect.Pure (fun args -> Values.bool (allFrozen args.Head))

/// Host function `Mark()` that only counts its calls.
let markFn () = Host.recorder "Mark"

let withFns (fns: RecordingFunction list) (options: RunOptions) =
    { options with Functions = options.Functions @ (fns |> List.map (fun f -> f.AsFunction)) }

let inputOf (v: JyroValue) = { Script.defaults with Input = ValueInput v }

// ===========================================================================
// 70.1.1 Input holds the host's value, or null
// ===========================================================================

let private readInput = lines [ "Output.isNull = Input == null"; "Output.v = Input" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_WhenHostSuppliesNoInput_IsNullAtFirstStatement`` () =
    let r = Script.runWith { Script.defaults with Input = NoInput } readInput
    Assert.True(Values.asBool (outProp "isNull" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_WhenHostSuppliesJyroNull_IsNull`` () =
    let r = Script.runWith { Script.defaults with Input = ValueInput(JyroNull.Instance) } readInput
    Assert.True(Values.asBool (outProp "isNull" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_WhenHostSuppliesClrNullObject_IsNull`` () =
    let r = Script.runWith { Script.defaults with Input = HostInput(null) } readInput
    Assert.True(Values.asBool (outProp "isNull" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-272")>]
[<InlineData("5")>]
[<InlineData("\"s\"")>]
[<InlineData("true")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("{\"a\":{\"b\":[1,2,{\"c\":null}]},\"d\":\"x\"}")>]
let ``Input_WhenHostSuppliesJyroValue_HoldsThatValueAtFirstStatement`` (json: string) =
    let x = Values.json json
    let expected = Values.json json
    let same = sameAs "Same" x
    let r =
        Script.runWith (withFns [ same ] (inputOf x))
            (lines [ "Output.same = Same(Input)"; "Output.isNull = Input == null"; "Output.v = Input" ])
    Assert.False(Values.asBool (outProp "isNull" r))
    Assert.True((outProp "v" r).EqualsValue expected, Values.toJson (outProp "v" r))
    Assert.True(Values.asBool (outProp "same" r), "Input is not the instance the host gave")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_WhenHostSuppliesDictionary_HoldsItsContent`` () =
    let d = Dictionary<string, obj>()
    d.["n"] <- box 3
    d.["s"] <- box "x"
    d.["list"] <- box (List<obj>([ box 1; box 2 ]))
    let r = Script.runWith { Script.defaults with Input = HostInput(box d) } (lines [ readInput; "Output.l = Input.list[1]" ])
    Assert.Equal(3.0, Values.asFloat (Values.prop "n" (outProp "v" r)))
    Assert.Equal("x", Values.asString (Values.prop "s" (outProp "v" r)))
    Assert.Equal(2.0, Values.asFloat (outProp "l" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-272")>]
[<InlineData("{\"a\":1}")>]
[<InlineData("[1,2]")>]
[<InlineData("7")>]
[<InlineData("\"t\"")>]
let ``Input_WhenHostSuppliesJsonText_HoldsTheValueItDescribes`` (json: string) =
    let r = Script.runWith { Script.defaults with Input = JsonInput json } readInput
    Assert.True((outProp "v" r).EqualsValue(Values.json json), Values.toJson (outProp "v" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_WhenHostSuppliesJsonNull_IsNull`` () =
    let r = Script.runWith { Script.defaults with Input = JsonInput "null" } readInput
    Assert.True(Values.asBool (outProp "isNull" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_WhenHostSuppliesJsonStream_HoldsTheValueItDescribes`` () =
    let stream = new MemoryStream(Encoding.UTF8.GetBytes "{\"a\":[1,2]}")
    let r =
        Script.runWith
            { Script.defaults with Input = NoInput; ConfigureRunner = fun runner -> runner.WithInputJson(stream) }
            readInput
    Assert.True((outProp "v" r).EqualsValue(Values.json "{\"a\":[1,2]}"), Values.toJson (outProp "v" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_ThroughJyroScriptExecuteScript_HoldsTheHostValue`` () =
    let d = Dictionary<string, obj>()
    d.["a"] <- box 1
    let r = JyroScript.executeScript (readInput) (box d)
    Assert.Equal(1.0, Values.asFloat (Values.prop "a" (outProp "v" r)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_ThroughJyroScriptExecuteScriptWithNullObject_IsNull`` () =
    let r = JyroScript.executeScript (readInput) (null)
    Assert.True(Values.asBool (outProp "isNull" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_ThroughJyroScriptExecuteScriptWithJson_HoldsTheValue`` () =
    let r = JyroScript.executeScriptWithJson (readInput) ("{\"a\":2}")
    Assert.Equal(2.0, Values.asFloat (Values.prop "a" (outProp "v" r)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-272")>]
let ``Input_InTwoConcurrentExecutionsOfOneProgram_EachSeesOnlyItsOwnInput`` () =
    let gate = Gate()
    let opts = withFns [ gate.Function ] Script.defaults
    let prog =
        program opts
            (lines [ "Output.first = Input.tag"
                     "if Input.tag == \"A\" then Hold() end"
                     "Output.second = Input.tag" ])
    let a = background (fun () -> execute { opts with Input = JsonInput "{\"tag\":\"A\"}" } prog)
    gate.WaitEntered()
    let b = execute { opts with Input = JsonInput "{\"tag\":\"B\"}" } prog
    gate.Release()
    let a = await a
    Assert.Equal("B", Values.asString (outProp "first" b))
    Assert.Equal("B", Values.asString (outProp "second" b))
    Assert.Equal("A", Values.asString (outProp "first" a))
    Assert.Equal("A", Values.asString (outProp "second" a))

// ===========================================================================
// 70.1.2 Output starts as an empty object, or the host's first value
// ===========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-273")>]
let ``JYRO-REQ-273 Output_WhenScriptIsEmpty_IsAnEmptyObject`` () =
    let v = Script.output (Script.run "")
    match v with
    | :? JyroObject as o -> Assert.Equal(0, o.Count)
    | _ -> Values.fail (sprintf "Expected an empty object, got %s" (Values.toJson v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-273")>]
let ``JYRO-REQ-273 Output_AtFirstStatement_IsAnEmptyObject`` () =
    let r = Script.run (lines [ "Output.keys = Keys(Output)"; "Output.t = TypeOf(Output)" ])
    Assert.Empty(Values.asList (outProp "keys" r))
    Assert.Equal("object", Values.asString (outProp "t" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-273")>]
[<InlineData("{\"p\":1,\"q\":{\"r\":[2]}}")>]
[<InlineData("[1,2]")>]
[<InlineData("5")>]
[<InlineData("\"s\"")>]
[<InlineData("null")>]
let ``JYRO-REQ-273 Output_WhenHostSuppliesFirstValue_IsThatValueAtFirstStatement`` (json: string) =
    let first = if json = "null" then JyroNull.Instance :> JyroValue else Values.json json
    let seen = Host.recorder "Seen"
    let r =
        Script.runWith (withFns [ seen ] { Script.defaults with Output = Some first }) "Seen(Output)"
    let result = Script.output r
    Assert.Equal(1, seen.CallCount)
    let given = seen.Calls.Head.Head
    Assert.True(given.EqualsValue(Values.json json), Values.toJson given)
    // The value is the host's value, not necessarily the very instance: a run starts from a copy of what
    // WithOutput was given (JYRO-REQ-1929), and the host's own value is left as it was.
    Assert.True(first.EqualsValue(Values.json json), Values.toJson first)
    Assert.True(result.EqualsValue(Values.json json), Values.toJson result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-273")>]
let ``JYRO-REQ-273 Output_WhenOneRunnerExecutesTwice_SecondRunStartsEmpty`` () =
    let prog = program Script.defaults (lines [ "Output.k = Keys(Output)"; "Output.a = 1" ])
    let runner = Script.runnerFor Script.defaults prog
    let first = runner.Execute()
    let second = runner.Execute()
    Assert.Empty(Values.asList (outProp "k" first))
    Assert.Empty(Values.asList (outProp "k" second))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-273")>]
let ``JYRO-REQ-273 Output_WhenTwoRunnersOfOneProgramExecute_EachStartsEmpty`` () =
    let prog = program Script.defaults (lines [ "Output.k = Keys(Output)"; "Output.a = 1" ])
    let first = execute Script.defaults prog
    let second = execute Script.defaults prog
    Assert.Empty(Values.asList (outProp "k" first))
    Assert.Empty(Values.asList (outProp "k" second))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-273")>]
let ``JYRO-REQ-273 Output_InTwoConcurrentExecutions_EachStartsEmptyAndSeesNoOtherWrites`` () =
    let gate = Gate()
    let opts = withFns [ gate.Function ] Script.defaults
    let prog =
        program opts
            (lines [ "if Input.tag == \"A\" then"
                     "  Output.a = 1"
                     "  Hold()"
                     "end"
                     "Output.k = Keys(Output)"
                     "if Input.tag == \"B\" then Output.b = 2 end" ])
    let a = background (fun () -> execute { opts with Input = JsonInput "{\"tag\":\"A\"}" } prog)
    gate.WaitEntered()
    let b = execute { opts with Input = JsonInput "{\"tag\":\"B\"}" } prog
    gate.Release()
    let a = await a
    Assert.Empty(Values.asList (outProp "k" b))
    Assert.False(hasKey "a" (Script.output b))
    Assert.Equal(1, (Values.asList (outProp "k" a)).Length)
    Assert.False(hasKey "b" (Script.output a))

// ===========================================================================
// 70.2.1 Input and everything reachable from it are read-only
// ===========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_BeforeFirstStatement_EveryReachableContainerIsReadOnly`` () =
    let x = Values.json "{\"o\":{\"a\":[1,{\"b\":2}]},\"arr\":[[1],{}],\"s\":\"x\"}"
    let kept = containers x
    let p = probe "Probe"
    let r =
        Script.runWith (withFns [ p ] (inputOf x))
            "Output.p = [Probe(Input), Probe(Input.o), Probe(Input.o.a), Probe(Input.o.a[1]), Probe(Input.arr), Probe(Input.arr[0]), Probe(Input.arr[1])]"
    let flags = Values.asList (outProp "p" r) |> List.map Values.asBool
    Assert.All(flags, fun f -> Assert.True(f))
    Assert.All(kept, fun c -> Assert.True(c.IsFrozen, "A container of Input is not read-only after the run"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_ReadOnly_HoldsForTheWholeExecution`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let af = allFrozenFn ()
    let r =
        Script.runWith (withFns [ af ] (inputOf x))
            (lines [ "Output.p0 = AllFrozen(Input)"
                     "var i = 0"
                     "while i < 20 do i = i + 1 end"
                     "Output.p1 = AllFrozen(Input)" ])
    Assert.True(Values.asBool (outProp "p0" r))
    Assert.True(Values.asBool (outProp "p1" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_WhenWide_EveryContainerIsReadOnly`` () =
    let wide = JyroObject()
    for i in 0 .. 9999 do wide.SetProperty(sprintf "k%d" i, Values.int i)
    let arr = JyroArray()
    for i in 0 .. 9999 do arr.Add(Values.obj [ "i", Values.int i ])
    let x = Values.obj [ "w", (wide :> JyroValue); "a", (arr :> JyroValue) ]
    let af = allFrozenFn ()
    let r = Script.runWith (withFns [ af ] (inputOf x)) "Output.p = AllFrozen(Input)"
    Assert.True(Values.asBool (outProp "p" r))
    Assert.True(allFrozen x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_WhenNestedExactlyToTheBound_IsAcceptedAndReadOnlyAtEveryLevel`` () =
    let root, all = nestedObjects (maxDepth ())
    let af = allFrozenFn ()
    let r = Script.runWith (withFns [ af ] (inputOf root)) "Output.p = AllFrozen(Input)"
    Assert.True(Values.asBool (outProp "p" r))
    Assert.All(all, fun c -> Assert.True(c.IsFrozen))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-274")>]
[<InlineData("5")>]
[<InlineData("\"s\"")>]
[<InlineData("null")>]
[<InlineData("true")>]
let ``Input_WhenScalar_RunsNormally`` (json: string) =
    let x = if json = "null" then JyroNull.Instance :> JyroValue else Values.json json
    let r = Script.runWith (inputOf x) "Output.v = Input"
    Assert.True((outProp "v" r).EqualsValue(x))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_WhenOneArrayIsReachableByTwoPaths_ItIsReadOnly`` () =
    let shared = Values.json "[1,{\"z\":1}]"
    let x = Values.obj [ "p", shared; "q", shared ]
    let af = allFrozenFn ()
    let r = Script.runWith (withFns [ af ] (inputOf x)) "Output.p = AllFrozen(Input.q)"
    Assert.True(Values.asBool (outProp "p" r))
    Assert.True(shared.IsFrozen)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_WhenAlsoTheFirstOutput_IsReadOnlyBeforeFirstStatement`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let seen = ref false
    let af =
        Host.func "AllFrozen" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any
            FunctionEffect.Pure (fun args -> seen.Value <- allFrozen args.Head; Values.nul)
    let r =
        Script.runWith (withFns [ af ] { inputOf x with Output = Some x })
            (lines [ "AllFrozen(Input)"; "Output.x = 1" ])
    Assert.Equal(1, af.CallCount)
    Assert.True(seen.Value, "Input was not read-only at the first statement")
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.True(allFrozen x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-274")>]
let ``Input_WhileAnotherExecutionWithTheSameValueStartsAndEnds_StaysReadOnly`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let gate = Gate()
    let optsA = withFns [ gate.Function ] (inputOf x)
    let a =
        background (fun () ->
            Script.runWith optsA (lines [ "Hold()"; "var o = Input.o"; "o.k = 1" ]))
    gate.WaitEntered()
    let b = Script.runWith (inputOf x) "Output.v = Input.o.a[0]"
    Assert.True(b.IsSuccess, Script.describe b)
    let y = Values.obj [ "inner", x ]
    let b2 = Script.runWith (inputOf y) "Output.v = 1"
    Assert.True(b2.IsSuccess, Script.describe b2)
    Assert.True(allFrozen x, "x is not read-only while execution A still runs")
    gate.Release()
    let a = await a
    Script.expectError MessageCode.InputValueIsReadOnly a
    Assert.True(allFrozen x)

// ===========================================================================
// 70.2.2 A script write to a read-only container raises InputValueIsReadOnly
// ===========================================================================

let private inputJson =
    "{\"o\":{\"k\":1},\"a\":[1,2],\"n\":1,\"deep\":{\"x\":{\"y\":[{\"z\":1}]}},\"v\":{\"_variant\":\"V\",\"f\":{}}}"

let private inputLiteral =
    "{o: {k: 1}, a: [1, 2], n: 1, deep: {x: {y: [{z: 1}]}}, v: {_variant: \"V\", f: {}}}"

let writeCases : obj[] seq =
    [ "var o = Input.o\no.k = 2"
      "var o = Input[\"o\"]\no[\"k\"] = 2"
      "var a = Input.a\na[0] = 9"
      "var a = Input.a\na[2] = 9"
      "var i = Input\ni.new = 1"
      "var o = Input.o\no.k += 1"
      "var a = Input.a\na[0] -= 1"
      "var o = Input.o\no.k &= \"x\""
      "var o = Input.o\no.k++"
      "var o = Input.o\nif true then ++o.k end"
      "var o = Input.o\no.k--"
      "var o = Input.o\ndelete o.k"
      "var o = Input.o\ndelete o[\"missing\"]"
      "func f(p) p.k = 2 end\nf(Input.o)"
      "func g(p) p[0] = 0 end\ng(Input.a)"
      "var e = First(Input.deep.x.y)\ne.z = 2"
      "var s = Sort([Input.o])\ns[0].k = 2"
      "var v = Values(Input)\nv[0].k = 2"
      "foreach e in Input.deep.x.y do e.z = 2 end"
      "union U V(f) end\nmatch Input.v do case V(f) then f.k = 1 end"
      "var d = Input.deep\nd.x.y[0].z = 2"
      "Output = Input\nOutput.o.k = 2" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1973")>]
[<MemberData(nameof writeCases)>]
let ``Write_ToReadOnlyInputContainer_RaisesCannotWriteToInputAndLeavesItUnchanged`` (source: string) =
    let x = Values.json inputJson
    let before = snapshot x
    let r = Script.runWith (inputOf x) source
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, snapshot x)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1973")>]
[<MemberData(nameof writeCases)>]
let ``Write_SameAccessPathOnScriptBuiltValue_Succeeds`` (source: string) =
    let r = Script.run ("var src = " + inputLiteral + "\n" + source.Replace("Input", "src"))
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1973")>]
let ``Write_ToReadOnlyInputArrayAtAnIndexPastTheEnd_RaisesCannotWriteToInput`` () =
    let x = Values.json inputJson
    let before = snapshot x
    let r = Script.runWith (inputOf x) "var a = Input.a\na[5] = 9"
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, snapshot x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1973")>]
let ``Write_ByHostFunctionDuringExecution_FailsTheExecutionWithCannotWriteToInput`` () =
    let x = Values.json inputJson
    let before = snapshot x
    let mutator =
        Host.func "Mutate" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args -> args.Head.SetProperty("k", Values.int 2); Values.nul)
    let r = Script.runWith (withFns [ mutator ] (inputOf x)) "Mutate(Input.o)"
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, snapshot x)

// ===========================================================================
// 70.2.3 A cyclic Input is made read-only; a too-deep Input fails first and
// stays as supplied. N is the maximum nesting depth of 100.7.1. Every run sets
// its execution limits and a generous compilation limit (FLOORS 4.1.11); Input
// is supplied as a Jyro value, so no conversion walk (100.7.7) runs.
// ===========================================================================

/// `bounded` with `v` as Input and a generous compilation limit.
let private pinnedInput (v: JyroValue) : RunOptions =
    { bounded with Input = ValueInput v; CompilationTimeout = Some(TimeSpan.FromMinutes 10.) }

/// The too-deep outcome of 70.2.3: exactly one runtime error, ValueNestingLimitExceeded
/// (stage Execute, 160.1.12), and no statement ran (Mark() was never called).
let private assertTooDeepBeforeFirstStatement (mark: RecordingFunction) (r: JyroResult<JyroValue>) =
    expectRuntimeError MessageCode.ValueNestingLimitExceeded r
    Assert.Equal(0, mark.CallCount)

/// A host function `Check()` taking no argument (so no value passes through the host
/// boundary) that answers whether every container of `all` is read-only now.
let private checkFrozen (all: JyroValue list) : RecordingFunction =
    Host.func "Check" [] ParameterType.Boolean FunctionEffect.Pure
        (fun _ -> Values.bool (all |> List.forall (fun c -> c.IsFrozen)))

/// `n` arrays nested one inside the next, outermost first.
let private nestedArrays (n: int) : JyroArray list =
    let all = [ for _ in 1 .. n -> JyroArray() ]
    all |> List.pairwise |> List.iter (fun (outer, inner) -> outer.Add(inner))
    all

let private asValues (xs: 'T list when 'T :> JyroValue) : JyroValue list = xs |> List.map (fun x -> x :> JyroValue)

/// Each container stays writable by the host: an array accepts an element, an object a property.
let private hostWritesEach (all: JyroValue list) =
    for c in all do
        match c with
        | :? JyroArray as a -> a.Add(Values.int 1)
        | _ -> c.SetProperty("w", Values.int 1)

// -- Table row 1: an object whose property `self` is that same object ------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenAnObjectHoldsItselfAsSelf_IsMadeReadOnly_AndOutputOkTrueRuns`` () =
    let o = JyroObject()
    o.SetProperty("self", o)
    let check = checkFrozen [ o ]
    let r = Script.runWith (withFns [ check ] (pinnedInput o)) "Output.ok = true\nOutput.frozen = Check()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(Values.asBool (outProp "ok" r))
    Assert.True(Values.asBool (outProp "frozen" r), "the cyclic Input was not read-only while the script ran")
    Assert.True(o.IsFrozen)
    Assert.True(Object.ReferenceEquals(o.GetProperty "self", o))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenAnObjectHoldsItselfAsSelf_AWriteThroughSelf_RaisesCannotWriteToInputAtTheSecondStatement`` () =
    let o = JyroObject()
    o.SetProperty("self", o)
    let r = Script.runWith (pinnedInput o) "var t = Input.self\nt.x = 1"
    expectRuntimeError MessageCode.InputValueIsReadOnly r
    Assert.Equal(2, (Script.onlyErrorMessage r).Location.Value.Line)
    Assert.False(hasKey "x" o, "the write reached the read-only Input")
    Assert.True(Object.ReferenceEquals(o.GetProperty "self", o))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenAnObjectHoldsItselfAsSelf_TheStatementBeforeTheWriteRuns`` () =
    // The failure is at the write, not before the first statement: Mark() runs first.
    let o = JyroObject()
    o.SetProperty("self", o)
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput o)) "Mark()\nvar t = Input.self\nt.x = 1"
    expectRuntimeError MessageCode.InputValueIsReadOnly r
    Assert.Equal(1, mark.CallCount)
    Assert.Equal(3, (Script.onlyErrorMessage r).Location.Value.Line)

// -- Table row 2: N arrays, the innermost holding the outermost ------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenNArraysFormACycleBackToTheOutermost_IsMadeReadOnlyAndRuns`` () =
    let all = nestedArrays (maxDepth ())
    (List.last all).Add(List.head all)
    let check = checkFrozen (asValues all)
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark; check ] (pinnedInput (List.head all))) "Mark()\nOutput.frozen = Check()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1, mark.CallCount)
    Assert.True(Values.asBool (outProp "frozen" r))
    Assert.All(all, fun c -> Assert.True(c.IsFrozen))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenNArraysFormACycle_AWriteToTheInnermost_RaisesCannotWriteToInput`` () =
    let n = maxDepth ()
    let all = nestedArrays n
    (List.last all).Add(List.head all)
    let source =
        lines [ "var t = Input"
                "var i = 0"
                sprintf "while i < %d do" (n - 1)
                "  t = t[0]"
                "  i = i + 1"
                "end"
                "Output.len = Length(t)"
                "t[0] = 1" ]
    let r = Script.runWith (pinnedInput (List.head all)) source
    expectRuntimeError MessageCode.InputValueIsReadOnly r
    Assert.Equal(8, (Script.onlyErrorMessage r).Location.Value.Line)
    Assert.True(Object.ReferenceEquals((List.last all).[0], List.head all), "the innermost array was changed")

// -- Table row 3: N arrays nested ----------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenNArraysNested_IsMadeReadOnlyAndRuns`` () =
    let all = nestedArrays (maxDepth ())
    let check = checkFrozen (asValues all)
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark; check ] (pinnedInput (List.head all))) "Mark()\nOutput.frozen = Check()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1, mark.CallCount)
    Assert.True(Values.asBool (outProp "frozen" r))
    Assert.All(all, fun c -> Assert.True(c.IsFrozen))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenNestedExactlyToTheBound_Runs`` () =
    let root, all = nestedObjects (maxDepth ())
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput root)) "Mark()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1, mark.CallCount)
    Assert.All(all, fun c -> Assert.True(c.IsFrozen))

// -- Table row 4: N + 1 arrays nested -------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenArraysNestedOneBeyondTheBound_FailsBeforeFirstStatementAndStaysWritable`` () =
    let all = nestedArrays (maxDepth () + 1)
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput (List.head all))) "Mark()\nOutput.r = 1"
    assertTooDeepBeforeFirstStatement mark r
    Assert.All(all, fun c -> Assert.False(c.IsFrozen, "an array of the refused Input became read-only"))
    all |> List.pairwise |> List.iter (fun (outer, inner) ->
        Assert.Equal(1, outer.Count)
        Assert.True(Object.ReferenceEquals(outer.[0], inner)))
    hostWritesEach (asValues all)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenObjectsNestedOneBeyondTheBound_FailsBeforeFirstStatementAndStaysWritable`` () =
    let root, all = nestedObjects (maxDepth () + 1)
    let before = snapshot root
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput root)) "Mark()\nOutput.r = 1"
    assertTooDeepBeforeFirstStatement mark r
    Assert.All(all, fun c -> Assert.False(c.IsFrozen, "A container of the refused Input became read-only"))
    Assert.Equal(before, snapshot root)
    hostWritesEach (asValues all)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenMixedNestingOneBeyondTheBound_FailsBeforeFirstStatement`` () =
    let d = maxDepth () + 1
    let all : JyroValue list = [ for i in 1 .. d -> if i % 2 = 0 then JyroArray() :> JyroValue else JyroObject() :> JyroValue ]
    all |> List.pairwise |> List.iter (fun (outer, inner) ->
        match outer with
        | :? JyroArray as a -> a.Add inner
        | _ -> outer.SetProperty("c", inner))
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput (List.head all))) "Mark()"
    assertTooDeepBeforeFirstStatement mark r
    Assert.All(all, fun c -> Assert.False(c.IsFrozen))
    hostWritesEach all

// -- Depth is the shortest path; a cycle adds no depth ---------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenACycleDoesNotPassThroughTheRoot_IsMadeReadOnlyAndRuns`` () =
    // Input.a.b is Input.a.
    let a = JyroObject()
    let b = JyroObject()
    a.SetProperty("b", b)
    b.SetProperty("a", a)
    let root = Values.obj [ "a", (a :> JyroValue) ]
    let check = checkFrozen [ root; a; b ]
    let r = Script.runWith (withFns [ check ] (pinnedInput root)) "Output.frozen = Check()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(Values.asBool (outProp "frozen" r))
    Assert.True(root.IsFrozen && a.IsFrozen && b.IsFrozen)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenACycleOfNArraysHasOneMoreArrayHungOffTheInnermost_FailsAndStaysWritable`` () =
    // The extra array is at depth N + 1 by its only path, whatever the cycle.
    let all = nestedArrays (maxDepth ())
    let innermost = List.last all
    innermost.Add(List.head all)
    let extra = JyroArray()
    innermost.Add(extra)
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput (List.head all))) "Mark()"
    assertTooDeepBeforeFirstStatement mark r
    Assert.All(extra :: all, fun c -> Assert.False(c.IsFrozen))
    Assert.Equal(2, innermost.Count)
    Assert.True(Object.ReferenceEquals(innermost.[0], List.head all))
    hostWritesEach (asValues (extra :: all))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenAContainerIsAlsoReachedByAShorterPath_ItsShortestPathDepthCounts`` () =
    // N + 1 arrays nested; the outermost also holds the innermost, so the innermost has
    // depth 2 by its shortest path, and no container is deeper than N.
    let all = nestedArrays (maxDepth () + 1)
    (List.head all).Add(List.last all)
    let check = checkFrozen (asValues all)
    let r = Script.runWith (withFns [ check ] (pinnedInput (List.head all))) "Output.frozen = Check()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(Values.asBool (outProp "frozen" r))
    Assert.All(all, fun c -> Assert.True(c.IsFrozen))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenAShortcutLeavesTheDeepestAtExactlyN_Runs_AndAtNPlus1_Fails`` () =
    let n = maxDepth ()
    // N + 1 arrays; array N - 1 also holds array N + 1, so array N + 1 has depth N.
    let atN = nestedArrays (n + 1)
    atN.[n - 2].Add(atN.[n])
    let r = Script.runWith (pinnedInput (List.head atN)) "Output.ok = true"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.All(atN, fun c -> Assert.True(c.IsFrozen))
    // N + 2 arrays with the same shortcut: array N + 2 has depth N + 1.
    let over = nestedArrays (n + 2)
    over.[n - 2].Add(over.[n])
    let mark = markFn ()
    let r2 = Script.runWith (withFns [ mark ] (pinnedInput (List.head over))) "Mark()"
    assertTooDeepBeforeFirstStatement mark r2
    Assert.All(over, fun c -> Assert.False(c.IsFrozen))

// -- A failure leaves the value as the host supplied it ----------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenTooDeepAndHoldsAnAlreadyReadOnlyValue_ThatValueStaysReadOnly`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let first = Script.runWith (pinnedInput x) "Output.v = 1"
    Assert.True(first.IsSuccess, Script.describe first)
    Assert.True(allFrozen x)
    let deep, chain = nestedObjects (maxDepth ())
    let y = JyroObject()
    y.SetProperty("x", x)
    y.SetProperty("deep", deep)
    let mark = markFn ()
    let r = Script.runWith (withFns [ mark ] (pinnedInput y)) "Mark()"
    assertTooDeepBeforeFirstStatement mark r
    Assert.False(y.IsFrozen)
    Assert.All(chain, fun c -> Assert.False(c.IsFrozen))
    Assert.True(allFrozen x, "a value that was already read-only lost its protection")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenTooDeepWhileAnotherExecutionHoldsPartOfIt_FailsAndLeavesBothAsTheyWere`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let gate = Gate()
    let a =
        background (fun () ->
            Script.runWith (withFns [ gate.Function ] (pinnedInput x)) (lines [ "Hold()"; "var o = Input.o"; "o.k = 1" ]))
    gate.WaitEntered()
    let deep, chain = nestedObjects (maxDepth ())
    let y = JyroObject()
    y.SetProperty("x", x)
    y.SetProperty("deep", deep)
    let mark = markFn ()
    let b = Script.runWith (withFns [ mark ] (pinnedInput y)) "Mark()"
    assertTooDeepBeforeFirstStatement mark b
    Assert.False(y.IsFrozen)
    Assert.All(chain, fun c -> Assert.False(c.IsFrozen))
    Assert.True(allFrozen x)
    gate.Release()
    Script.expectError MessageCode.InputValueIsReadOnly (await a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenTheSameTooDeepValueIsGivenToTwoConcurrentExecutions_BothFailAndItStaysWritable`` () =
    let root, all = nestedObjects (maxDepth () + 1)
    let t1 = background (fun () -> Script.runWith (pinnedInput root) "Output.r = 1")
    let t2 = background (fun () -> Script.runWith (pinnedInput root) "Output.r = 1")
    expectRuntimeError MessageCode.ValueNestingLimitExceeded (await t1)
    expectRuntimeError MessageCode.ValueNestingLimitExceeded (await t2)
    Assert.All(all, fun c -> Assert.False(c.IsFrozen))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1974")>]
let ``JYRO-REQ-1974 Input_WhenTheSameCyclicValueIsGivenToTwoConcurrentExecutions_BothRun`` () =
    let o = JyroObject()
    o.SetProperty("self", o)
    let t1 = background (fun () -> Script.runWith (pinnedInput o) "Output.ok = true")
    let t2 = background (fun () -> Script.runWith (pinnedInput o) "Output.ok = true")
    for r in [ await t1; await t2 ] do
        Assert.True(r.IsSuccess, Script.describe r)
        Assert.True(Values.asBool (outProp "ok" r))
    Assert.True(o.IsFrozen)

// ===========================================================================
// 70.3.1 Output is mutable in place, and may be assigned a new value
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2160")>]
[<InlineData("Output.a = 1  Output.b = [1]  Output.b[0] = 2", "{\"a\":1,\"b\":[2]}")>]
[<InlineData("Output.a = {x: {y: [1, 2]}}\nOutput.a.x.y[1] = 9\nOutput.a.x.z = 3\ndelete Output.a.x.y", "{\"a\":{\"x\":{\"z\":3}}}")>]
[<InlineData("Output.a = {x: 5}\nOutput.a.x -= 2\nOutput.a.x *= 3", "{\"a\":{\"x\":9}}")>]
[<InlineData("Output.a = {x: [1, 5]}\nOutput.a.x[1]++\nOutput.a.x[0]--", "{\"a\":{\"x\":[0,6]}}")>]
[<InlineData("Output.a = [1, 2]\nOutput.a[0] += 10\nOutput.a[1]--\ndelete Output[\"a\"]", "{}")>]
[<InlineData("Output.a = 1", "{\"a\":1}")>]
[<InlineData("Output[\"b\"] = 2", "{\"b\":2}")>]
[<InlineData("Output.n = {}\nOutput.n.m = [1]\nOutput.n.m[0] = 5", "{\"n\":{\"m\":[5]}}")>]
[<InlineData("Output.x = 1\ndelete Output.x", "{}")>]
[<InlineData("Output.c = 1\nOutput.c += 1\nOutput.c++", "{\"c\":3}")>]
[<InlineData("Output = [1, 2]", "[1,2]")>]
[<InlineData("Output = 5", "5")>]
[<InlineData("Output = null", "null")>]
[<InlineData("Output = \"s\"", "\"s\"")>]
[<InlineData("Output = true", "true")>]
[<InlineData("Output = {}\nOutput.z = 1", "{\"z\":1}")>]
[<InlineData("Output = [1]\nOutput[0] = 7", "[7]")>]
let ``JYRO-REQ-2160 Output_ChangedInPlaceOrReassigned_ResultReflectsTheChange`` (source: string, expected: string) =
    let v = Script.output (Script.run source)
    Assert.True(v.EqualsValue(Values.json expected), sprintf "got %s" (Values.toJson v))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2160")>]
[<InlineData("Output.k = 2")>]
[<InlineData("delete Output.k")>]
[<InlineData("Output.k += 1")>]
[<InlineData("Output.k++")>]
[<InlineData("Output.k--")>]
[<InlineData("Output.n.m = 2")>]
[<InlineData("Output.n.m++")>]
[<InlineData("delete Output.n.m")>]
[<InlineData("Output.a[0] = 2")>]
[<InlineData("Output.a[0] += 1")>]
[<InlineData("Output.a[0]--")>]
let ``JYRO-REQ-2160 Output_ASharedContainerReachedThroughOutput_RaisesCannotWriteToInput`` (source: string) =
    let x = Values.json "{\"k\":1,\"n\":{\"m\":1},\"a\":[1]}"
    let before = Values.toJson x
    let r = Script.runWith { Script.defaults with Input = ValueInput x; Output = Some x } source
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, Values.toJson x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2160")>]
let ``JYRO-REQ-2160 Output_ANewValueAssignedToOutputIsTheResultWhateverInputWas`` () =
    let x = Values.json "{\"k\":1}"
    let r = Script.runWith { Script.defaults with Input = ValueInput x; Output = Some x } "Output = 5"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((Script.output r).EqualsValue(Values.json "5"))
    let r2 = Script.runWith { Script.defaults with Input = ValueInput x } "Output = Input"
    Assert.True(r2.IsSuccess, Script.describe r2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2160")>]
let ``JYRO-REQ-2160 Output_WhenHostSuppliesFirstObject_InPlaceWritesLandInTheResultAndNotInTheHostsObject`` () =
    // A run starts its Output from a copy of the value given to WithOutput (JYRO-REQ-1929), so the
    // writes are in the result and the host's own object is as it was.
    let o = Values.json "{\"keep\":0}"
    let r = Script.runWith { Script.defaults with Output = Some o } (lines [ "Output.a = 1"; "Output.n = {}"; "Output.n.m = 2" ])
    Assert.True(r.IsSuccess, Script.describe r)
    let result = Script.output r
    Assert.Equal(1.0, Values.asFloat (result.GetProperty "a"))
    Assert.Equal(2.0, Values.asFloat (Values.prop "m" (result.GetProperty "n")))
    Assert.Equal(0.0, Values.asFloat (result.GetProperty "keep"))
    Assert.Equal("{\"keep\":0}", o.ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2160")>]
let ``JYRO-REQ-2160 Output_InTwoConcurrentExecutions_EachResultHoldsOnlyItsOwnWrites`` () =
    let gate = Gate()
    let opts = withFns [ gate.Function ] Script.defaults
    let prog =
        program opts
            (lines [ "Output.tag = Input.tag"
                     "if Input.tag == \"A\" then"
                     "  Output.a = 1"
                     "  Hold()"
                     "  Output = {mine: \"A\"}"
                     "  Output.after = 1"
                     "else"
                     "  Output.b = 1"
                     "  Output = [\"B\"]"
                     "end" ])
    let a = background (fun () -> execute { opts with Input = JsonInput "{\"tag\":\"A\"}" } prog)
    gate.WaitEntered()
    let b = execute { opts with Input = JsonInput "{\"tag\":\"B\"}" } prog
    gate.Release()
    let a = await a
    Assert.True((Script.output b).EqualsValue(Values.json "[\"B\"]"), Values.toJson (Script.output b))
    Assert.True((Script.output a).EqualsValue(Values.json "{\"mine\":\"A\",\"after\":1}"), Values.toJson (Script.output a))

// ===========================================================================
// 70.3.2 The host's first Output is not made read-only, unless shared with Input
// ===========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1975")>]
let ``FirstOutput_NotSharedWithInput_StaysWritableForScriptAndHost`` () =
    let o = Values.json "{\"a\":{\"b\":[1]}}"
    let r =
        Script.runWith { Script.defaults with Output = Some o; Input = JsonInput "{\"i\":{}}" }
            (lines [ "Output.a.b[0] = 2"; "Output.a.c = 1"; "Output.z = 1" ])
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(noneFrozen o, "a container of the host's first Output became read-only")
    o.SetProperty("h", Values.int 1)
    (Values.prop "a" o).SetProperty("h", Values.int 1)
    (Values.prop "b" (Values.prop "a" o) :?> JyroArray).Add(Values.int 3)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1975")>]
let ``FirstOutput_WhenTheSameValueIsInput_WriteThroughOutputRaisesCannotWriteToInput`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let before = snapshot x
    let r = Script.runWith { inputOf x with Output = Some x } "Output.x = 1"
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, snapshot x)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1975")>]
[<InlineData("Output.shared.k = 1")>]
[<InlineData("Output.shared.a[0] = 9")>]
[<InlineData("delete Output.shared.a")>]
let ``FirstOutput_ContainerSharedWithInput_WriteThroughOutputRaisesCannotWriteToInput`` (source: string) =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let shared = Values.prop "o" x
    let first = Values.obj [ "mine", Values.emptyObj (); "shared", shared ]
    let before = snapshot shared
    let r = Script.runWith { inputOf x with Output = Some first } source
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, snapshot shared)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1975")>]
let ``FirstOutput_ContainerNotSharedWithInput_StaysWritableBesideASharedOne`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let mine = Values.emptyObj ()
    let first = Values.obj [ "mine", mine; "shared", Values.prop "o" x ]
    let r = Script.runWith { inputOf x with Output = Some first } (lines [ "Output.mine.k = 1"; "Output.z = 1" ])
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.False(mine.IsFrozen)
    Assert.False(first.IsFrozen)
    Assert.True(allFrozen (Values.prop "o" x))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1975")>]
[<InlineData("Output.v = 1")>]
[<InlineData("fail \"no\"")>]
let ``FirstOutput_WhenInputOfAnEarlierExecution_WriteThroughOutputRaisesCannotWriteToInput`` (earlier: string) =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    Script.runWith (inputOf x) earlier |> ignore
    let before = snapshot x
    let r = Script.runWith { Script.defaults with Output = Some x } "Output.x = 1"
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(before, snapshot x)
    Assert.True(allFrozen x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1975")>]
let ``FirstOutput_WhenInputOfAConcurrentExecution_WriteThroughOutputRaisesCannotWriteToInput`` () =
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let gate = Gate()
    let a = background (fun () -> Script.runWith (withFns [ gate.Function ] (inputOf x)) "Hold()")
    gate.WaitEntered()
    let before = snapshot x
    let b = Script.runWith { Script.defaults with Output = Some x } "Output.x = 1"
    gate.Release()
    Assert.True((await a).IsSuccess)
    Script.expectError MessageCode.InputValueIsReadOnly b
    Assert.Equal(before, snapshot x)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1975")>]
let ``FirstOutput_WhenAConcurrentExecutionLaterTakesItAsInput_TheCopyTakenAtTheCallStaysWritableAndTheValueIsUnchanged`` () =
    // The run took its Output as a copy when Execute() was called (JYRO-REQ-1929), before the other
    // execution made x read-only, so its write is to the copy: it succeeds and x is not changed.
    let x = Values.json "{\"o\":{\"a\":[1]}}"
    let gate = Gate()
    let b =
        background (fun () ->
            Script.runWith (withFns [ gate.Function ] { Script.defaults with Output = Some x }) (lines [ "Hold()"; "Output.x = 1" ]))
    gate.WaitEntered()
    let a = Script.runWith (inputOf x) "Output.r = 1"
    Assert.True(a.IsSuccess, Script.describe a)
    let before = snapshot x
    gate.Release()
    let rb = await b
    Assert.True(rb.IsSuccess, Script.describe rb)
    Assert.Equal(before, snapshot x)

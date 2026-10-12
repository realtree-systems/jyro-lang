/// Part 120: extension modules, non-deterministic functions and effects.
module RealTree.Jyro.Spec.Tests.S120_Sandboxing

open System
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private effects = [ FunctionEffect.Pure; FunctionEffect.NonDeterministic; FunctionEffect.Blocking; FunctionEffect.Mutating ]

let private effectNamed (name: string) : FunctionEffect =
    effects |> List.find (fun e -> string e = name || sprintf "%A" e = name)

let private rank (e: FunctionEffect) : int = effects |> List.findIndex (fun x -> x = e)

let private withMax (e: FunctionEffect) (o: RunOptions) = { o with MaxEffect = Some e }

let private effectCount (r: JyroResult<'T>) =
    Script.errorCodes r |> List.filter (fun c -> c = MessageCode.FunctionEffectExceedsMax) |> List.length

// -- 120.2.2 / 120.2.3 Extension modules ---------------------------------------------------

/// A stand-in extension module: host functions the host registers itself.
let private extensionModule () =
    [ Host.func "HttpGet" [ Parameter.Required("url", ParameterType.String) ] ParameterType.Any
          FunctionEffect.Blocking (fun _ -> Values.str "body") ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1461")>]
let ``ExtensionFunction_IsNotInTheStandardLibrary`` () =
    let names = stdlibAll () |> List.map (fun f -> f.Name)
    for f in extensionModule () do
        Assert.DoesNotContain(f.Name, names)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1461")>]
let ``ExtensionFunction_IsCallableOnlyAfterTheHostRegistersIt`` () =
    Script.compile "Output.r = HttpGet(\"http://127.0.0.1/\")" |> fails MessageCode.UndefinedFunction
    let fns = extensionModule () |> List.map (fun f -> f.AsFunction)
    let v = Script.runWith { Script.defaults with Functions = fns } "Output.r = HttpGet(\"http://127.0.0.1/\")" |> ok
    Assert.Equal("body", Values.asString (Values.prop "r" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1461")>]
let ``ShippedAssemblies_AreOnlyTheLanguageAssemblies`` () =
    // No assembly beside the five language assemblies declares a public Jyro function type.
    let dir = IO.Path.GetDirectoryName(typeof<JyroCompiler>.Assembly.Location)
    let jyro =
        IO.Directory.GetFiles(dir, "Jyro.*.dll")
        |> Array.map IO.Path.GetFileNameWithoutExtension
        |> Array.filter (fun n -> not (n.StartsWith "Jyro.Spec"))
        |> Set.ofArray
    Assert.Equal<Set<string>>(set [ "Jyro.Core"; "Jyro.Parser"; "Jyro.Compiler"; "Jyro.Stdlib"; "Jyro.Api" ], jyro)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2153")>]
let ``JYRO-REQ-2153 ExtensionFunction_CountsAgainstTheCallDepth`` () =
    let fns = extensionModule () |> List.map (fun f -> f.AsFunction)
    let src = "func F()\n  return HttpGet(\"x\")\nend\nOutput.r = F()"
    Script.runWith { withLimits (fun o -> { o with MaxCallDepth = 1 }) with Functions = fns } src
    |> fails MessageCode.CallDepthLimitExceeded
    Script.runWith { withLimits (fun o -> { o with MaxCallDepth = 2 }) with Functions = fns } src |> ok |> ignore

/// A chain of `d` script functions, the innermost calling the extension function: the call of
/// the extension function starts at call depth `d`.
let private chainThenExtension (d: int) =
    let fns = [ for i in 1 .. d -> if i = d then sprintf "func F%d()\n  return HttpGet(\"x\")\nend" i else sprintf "func F%d()\n  return F%d()\nend" i (i + 1) ]
    String.Join("\n", fns) + "\nOutput.r = F1()"

[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
[<InlineData(5)>]
[<InlineData(20)>]
[<Trait("requirement", "JYRO-REQ-2153")>]
let ``JYRO-REQ-2153 ExtensionFunctionCallStartingAtDepthD_UnderLimitD_EndsWithCallDepthLimitExceeded`` (d: int) =
    let fns = extensionModule () |> List.map (fun f -> f.AsFunction)
    let src = chainThenExtension d
    Script.runWith { withLimits (fun o -> { o with MaxCallDepth = d }) with Functions = fns } src
    |> fails MessageCode.CallDepthLimitExceeded
    Script.runWith { withLimits (fun o -> { o with MaxCallDepth = d + 1 }) with Functions = fns } src |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2153")>]
let ``JYRO-REQ-2153 ExtensionFunction_CalledFromTheTopLevel_StartsAtDepthOne`` () =
    let fns = extensionModule () |> List.map (fun f -> f.AsFunction)
    let src = "Output.r = HttpGet(\"x\")"
    Script.runWith { withLimits (fun o -> { o with MaxCallDepth = 1 }) with Functions = fns } src |> ok |> ignore
    Script.runWith { withLimits (fun o -> { o with MaxCallDepth = 1 }) with Functions = fns }
        "func F()\n  return 1\nend\nOutput.r = HttpGet(F())"
    |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2153")>]
let ``JYRO-REQ-2153 ExtensionFunction_ObeysTheTimeLimitAndCancellation`` () =
    let spin = spinChecked ()
    Script.runWith
        { withLimits (fun o -> { o with MaxExecutionTime = TimeSpan.FromMilliseconds 100. }) with Functions = [ spin.AsFunction ] }
        "Output.r = Spin(3000000)"
    |> fails MessageCode.ExecutionTimeLimitExceeded
    use cts = new CancellationTokenSource()
    // The cancel comes from a thread of its own and Spin does not end by itself, so a late
    // cancel only stops the run later (JYRO-DEF-745); the 30 s limit of `roomy` is the backstop.
    let spin2 = spinChecked ()
    let options = { withLimits roomy with Functions = [ spin2.AsFunction ]; Cancellation = Some cts.Token }
    RealTree.Jyro.Spec.HostCancel.cancelFromThread cts (TimeSpan.FromMilliseconds 100.)
    Script.runWith options (sprintf "Output.r = Spin(%d)" RealTree.Jyro.Spec.HostCancel.unboundedSpin)
    |> fails MessageCode.CanceledByHost

// -- 120.3.1 Non-deterministic functions ---------------------------------------------------

let nonDeterministic : obj[] seq =
    [ "Now", "Now()"; "Today", "Today()"; "NewGuid", "NewGuid()"; "RandomInt", "RandomInt(0, 10)"
      "RandomString", "RandomString(5)"; "RandomChoice", "RandomChoice([1, 2])" ]
    |> Seq.map (fun (n, c) -> [| box n; box c |])

[<Theory>]
[<MemberData(nameof nonDeterministic)>]
[<Trait("requirement", "JYRO-REQ-1463")>]
let ``NonDeterministicFunction_DeclaresNonDeterministicOrStronger`` (name: string, call: string) =
    ignore call
    match stdlibTryGet name with
    | None -> Values.fail (sprintf "%s is not in the standard library" name)
    | Some f -> Assert.True(rank f.Effect >= rank FunctionEffect.NonDeterministic, sprintf "%s declares %A" name f.Effect)

[<Theory>]
[<MemberData(nameof nonDeterministic)>]
[<Trait("requirement", "JYRO-REQ-1463")>]
let ``NonDeterministicFunction_IsRejectedUnderAPureMaximum`` (name: string, call: string) =
    ignore name
    Script.compileWith (withMax FunctionEffect.Pure Script.defaults) ("Output.r = " + call)
    |> fails MessageCode.FunctionEffectExceedsMax

// -- 120.3.2 Omit a standard library function and register one in its place --------------

let replaceable : obj[] seq =
    [ "Now", "Now()"; "Today", "Today()"; "NewGuid", "NewGuid()"; "RandomInt", "RandomInt(0, 10)"
      "RandomString", "RandomString(5)"; "RandomChoice", "RandomChoice([1, 2])"; "Length", "Length([1, 2, 3])" ]
    |> Seq.map (fun (n, c) -> [| box n; box c |])

[<Theory>]
[<MemberData(nameof replaceable)>]
[<Trait("requirement", "JYRO-REQ-1464")>]
let ``HostOmitsAStandardFunction_AndRegistersItsOwn_TheScriptGetsTheHostOne`` (name: string, call: string) =
    let mine =
        Host.func name [ for i in 0 .. 2 -> Parameter.Optional(sprintf "a%d" i, ParameterType.Any) ]
            ParameterType.Any FunctionEffect.Pure (fun _ -> Values.str "host")
    for fns in [ stdlibWithout [ name ] @ [ mine.AsFunction ]; mine.AsFunction :: stdlibWithout [ name ] ] do
        let r = Script.runWith { Script.defaults with Stdlib = false; Functions = fns } ("Output.r = " + call)
        Assert.DoesNotContain(MessageCode.HostFunctionOverridesStdlib, Script.codes r)
        Assert.Equal("host", Values.asString (Values.prop "r" (ok r)))

// -- 120.4.x Effects ------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2050")>]
let ``EveryStandardFunction_DeclaresExactlyOneOfTheFourEffects`` () =
    let all = stdlibAll ()
    Assert.NotEmpty all
    for f in all do
        Assert.False(isNull (box f.Effect), sprintf "%s declares no effect" f.Name)
        let matching = effects |> List.filter (fun e -> e = f.Effect) |> List.length
        Assert.True((matching = 1), sprintf "%s declares %A" f.Name f.Effect)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2050")>]
let ``HostFunction_ExposesOneEffect_AndItIsTheOneTheCeilingApplies`` () =
    Assert.Equal(typeof<FunctionEffect>, typeof<IJyroFunction>.GetProperty("Effect").PropertyType)
    for e in effects do
        let f = Host.recorderWithEffect "H" e
        Assert.Equal(e, (f.AsFunction).Effect)
        Assert.Equal(e, (f :> JyroFunctionBase).Effect)
        let under = Script.compileWith { withMax e Script.defaults with Functions = [ f.AsFunction ] } "H()"
        Assert.True(under.IsSuccess, Script.describe under)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2051")>]
let ``Effects_AreOrderedPureNonDeterministicBlockingMutating`` () =
    for a in effects do
        for b in effects do
            Assert.Equal(sign (compare (rank a) (rank b)), sign (compare a b))
            Assert.Equal(sign (compare (rank a) (rank b)), sign ((a :> IComparable).CompareTo(box b)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2051")>]
let ``CeilingMatrix_ACallPassesExactlyWhenItsEffectIsAtMostTheMaximum`` () =
    for fe in effects do
        for maxE in effects do
            let f = Host.recorderWithEffect "H" fe
            let r = Script.compileWith { withMax maxE Script.defaults with Functions = [ f.AsFunction ] } "H()"
            if rank fe <= rank maxE then Assert.True(r.IsSuccess, sprintf "%A under %A: %s" fe maxE (Script.describe r))
            else fails MessageCode.FunctionEffectExceedsMax r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2052")>]
let ``ScriptFunctionCallingABlockingFunction_IsBlocking`` () =
    let r = Script.compileWith (withMax FunctionEffect.NonDeterministic Script.defaults) "func F()\n  Sleep(0)\n  return 1\nend\nOutput.r = F()"
    Assert.False(r.IsSuccess)
    Assert.Equal(2, effectCount r)
    let lines = [ for m in r.Messages -> m.Location.Value.Line ] |> List.sort
    Assert.Equal<int list>([ 2; 5 ], lines)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2052")>]
let ``EffectPropagatesThroughAChainOfScriptFunctions`` () =
    let src = "func G()\n  Sleep(0)\n  return 1\nend\nfunc F()\n  return G()\nend\nOutput.r = F()"
    let r = Script.compileWith (withMax FunctionEffect.NonDeterministic Script.defaults) src
    Assert.Equal(3, effectCount r)
    let r2 = Script.compileWith (withMax FunctionEffect.Blocking Script.defaults) src
    Assert.True(r2.IsSuccess, Script.describe r2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2052")>]
let ``FunctionValueArgument_DoesNotRaiseTheEffectOfTheFunctionReceivingIt`` () =
    let r = Script.compileWith (withMax FunctionEffect.NonDeterministic Script.defaults) "Output.r = Map([1],\n  x => Sleep(0))"
    Assert.Equal(1, effectCount r)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.FunctionEffectExceedsMax)
    Assert.Equal(2, m.Location.Value.Line)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2052")>]
let ``LibraryFunctionCallingNow_IsNonDeterministic`` () =
    let lib = "func Stamp()\n  return Now()\nend"
    let r =
        Script.compileWith
            { Script.defaults with MaxEffect = Some FunctionEffect.Pure; LibraryMaxEffect = Some FunctionEffect.Mutating; Libraries = [ lib ] }
            "Output.r = 1\nOutput.s = Stamp()"
    fails MessageCode.FunctionEffectExceedsMax r
    Assert.Equal(2, (Script.onlyErrorMessage r).Location.Value.Line)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2052")>]
let ``ScriptFunctionCallingOnlyPureFunctions_IsPure`` () =
    let r = Script.compileWith (withMax FunctionEffect.Pure Script.defaults) "func F(x)\n  return Length([x]) + Absolute(x)\nend\nOutput.r = F(1)"
    Assert.True(r.IsSuccess, Script.describe r)

// -- 120.5.x The effect ceiling ---------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1468")>]
let ``Host_CanSetTheMaximumEffectPerCompilation`` () =
    for e in effects do
        let r = Script.compileWith (withMax e Script.defaults) "Output.t = Now()"
        if e = FunctionEffect.Pure then fails MessageCode.FunctionEffectExceedsMax r
        else Assert.True(r.IsSuccess, sprintf "%A: %s" e (Script.describe r))

let callSites : obj[] seq =
    [ "Now()"; "var v = Now()"; "Output.x = Length([Now()])"; "if Now() == 1 then Output.x = 1 end"
      "while Now() == 1 do Output.x = 1 end"; "Output.x = Now() == 1 ? 1 : 2"; "for i in 0 to Length(Now()) do Output.x = i end"
      "foreach x in [Now()] do Output.x = x end"; "switch Now() do case 1 then Output.x = 1 end"
      "switch 1 do case Now() then Output.x = 1 end"; "func F()\n  return Now()\nend"
      "Output.x = Map([1], x => Now())"; "var f = x => Now()\nOutput.x = Map([1], f)"
      "Output.x = Round(value: Now(), decimals: 0)"; "func F()\n  return Now()\nend\nOutput.x = F()" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<MemberData(nameof callSites)>]
[<Trait("requirement", "JYRO-REQ-1469")>]
let ``CallAboveTheMaximum_AtAnyCallSite_IsRejectedAtLinkTime_AndTheScriptDoesNotRun`` (site: string) =
    let m = marker ()
    let r = Script.runWith { withMax FunctionEffect.Pure Script.defaults with Functions = [ m.AsFunction ] } ("Output.before = 1\nMark(1)\n" + site)
    Assert.False(r.IsSuccess)
    Assert.True(effectCount r >= 1, Script.describe r)
    for d in r.Messages |> Seq.filter (fun d -> d.Code = MessageCode.FunctionEffectExceedsMax) do
        Assert.True(d.Location.HasValue && d.Location.Value.Line >= 3, sprintf "%A" d.Location)
    Assert.Equal(0, m.CallCount)
    match Script.value r with
    | Some v -> Assert.False(hasProp "before" v, "The script ran")
    | None -> ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1469")>]
let ``EveryCallAboveTheMaximum_GetsItsOwnDiagnostic`` () =
    let r = Script.compileWith (withMax FunctionEffect.Pure Script.defaults) "Output.a = Now()\nOutput.b = 1\nOutput.c = NewGuid()"
    Assert.Equal(2, effectCount r)
    let ls = [ for m in r.Messages -> m.Location.Value.Line ]
    Assert.Equal<int list>([ 1; 3 ], ls)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1469")>]
let ``HostFunctionAboveTheMaximum_IsRejected`` () =
    let h = Host.recorderWithEffect "Write" FunctionEffect.Mutating
    Script.compileWith { withMax FunctionEffect.Blocking Script.defaults with Functions = [ h.AsFunction ] } "Write(1)"
    |> fails MessageCode.FunctionEffectExceedsMax

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1469")>]
let ``CallInsideALibraryAboveTheLibraryMaximum_IsRejected`` () =
    let lib = "func Stamp()\n  return Now()\nend"
    Script.compileWith { Script.defaults with LibraryMaxEffect = Some FunctionEffect.Pure; Libraries = [ lib ] } "Output.r = 1"
    |> fails MessageCode.FunctionEffectExceedsMax

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 EveryCompilePath_AppliesTheMaximum`` () =
    let pureMax = withMax FunctionEffect.Pure Script.defaults
    let src = "Output.t = Now()"
    fails MessageCode.FunctionEffectExceedsMax (Script.compileWith pureMax src)
    throwsWithCode MessageCode.FunctionEffectExceedsMax (fun () -> (Script.compilerFor pureMax).WithSource(src).CompileOrThrow())
    let bytes = Script.compileBytesWith pureMax src
    Assert.False(bytes.IsSuccess)
    Assert.False(bytes.HasValue && bytes.IsSuccess)
    let written = Script.compileBytesWith (withMax FunctionEffect.Mutating Script.defaults) src |> Script.valueOf
    let loaded = Script.loadBytesWith pureMax (written)
    fails MessageCode.ArtifactEffectExceedsMax loaded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 MaximumSetInAnyOrder_IsApplied`` () =
    let src = "Output.t = Now()"
    let orders : (unit -> JyroResult<CompiledProgram>) list =
        [ fun () -> JyroCompiler().WithMaxEffect(FunctionEffect.Pure).WithStdlib().WithSource(src).Compile()
          fun () -> JyroCompiler().WithStdlib().WithSource(src).WithMaxEffect(FunctionEffect.Pure).Compile()
          fun () -> JyroCompiler().WithSource(src).WithMaxEffect(FunctionEffect.Pure).WithStdlib().Compile()
          fun () -> JyroCompiler().WithMaxEffect(FunctionEffect.Pure).AddFunction(Host.recorder "X").WithStdlib().WithSource(src).Compile()
          fun () -> JyroCompiler().WithStdlib().AddFunction(Host.recorder "X").WithSource(src).WithMaxEffect(FunctionEffect.Pure).Compile() ]
    for f in orders do fails MessageCode.FunctionEffectExceedsMax (f ())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 LibraryPaths_ApplyTheLibraryMaximum`` () =
    let lib = "func Stamp()\n  return Now()\nend"
    let viaSource = JyroCompiler().WithStdlib().WithLibraryMaxEffect(FunctionEffect.Pure).AddLibrarySource(lib).WithSource("Output.r = 1").Compile()
    fails MessageCode.FunctionEffectExceedsMax viaSource
    let parsed = JyroLibrary.Parse lib
    match parsed.TryGetValue() with
    | true, l when parsed.IsSuccess ->
        let viaLib = JyroCompiler().WithStdlib().AddLibrary(l).WithLibraryMaxEffect(FunctionEffect.Pure).WithSource("Output.r = 1").Compile()
        fails MessageCode.FunctionEffectExceedsMax viaLib
    | _ -> Values.fail ("JyroLibrary.Parse failed: " + Script.describe parsed)

// -- 120.5 A maximum that cannot be applied: MaxEffectNotApplicable (JYRO-REQ-2136) ------------------------------

/// What a compile member returned, read without the payload.
type private Outcome =
    { Succeeded: bool
      Valued: bool
      Diagnostics: DiagnosticMessage list }

let private outcomeOf (r: JyroResult<'T>) : Outcome =
    { Succeeded = r.IsSuccess; Valued = r.HasValue; Diagnostics = List.ofSeq r.Messages }

/// `Compile()` or `CompileToArtifact()` of `source` under `o`, failing the test when anything is
/// thrown (the clause: it MUST return, and MUST NOT throw).
let private compileOn (path: string) (o: RunOptions) (source: string) : Outcome =
    try
        if path = "Compile" then outcomeOf ((Script.compilerFor o).WithSource(source).Compile())
        else outcomeOf (Loader.compileToArtifactWith o source)
    with e -> Values.fail (sprintf "%s threw %s: %s" path (e.GetType().FullName) e.Message)

/// `JyroArtifactLoader.Load()` of the artifact `bytes` under `o`, failing the test when anything is thrown.
let private loadOn (o: RunOptions) (bytes: byte[]) : Outcome =
    try outcomeOf (Loader.loadWith o bytes)
    with e -> Values.fail (sprintf "a load threw %s: %s" (e.GetType().FullName) e.Message)

let private describeOutcome (o: Outcome) : string =
    String.Join("; ", o.Diagnostics |> List.map (fun m -> sprintf "%A %A (%s) %A" m.Severity m.Code (Pending.stageOf m) (Tests.CallCheck.argsOf m)))

let private codeIn (code: MessageCode) (o: Outcome) : DiagnosticMessage list =
    o.Diagnostics |> List.filter (fun m -> m.Code = code)

/// The result is a refusal: not a success, no value, and (JYRO-REQ-2136) neither
/// UnknownCompilerError nor FunctionEffectExceedsMax for the function.
let private refused (what: string) (o: Outcome) =
    Assert.False(o.Succeeded, sprintf "%s: IsSuccess. %s" what (describeOutcome o))
    Assert.False(o.Valued, sprintf "%s: HasValue. %s" what (describeOutcome o))
    Assert.Empty(codeIn Tests.CallCheck.unknownCompilerError o)
    Assert.Empty(codeIn MessageCode.FunctionEffectExceedsMax o)

/// One MaxEffectNotApplicable of severity Error, stage Link, with the arguments `name` then `maximum`.
let private oneNotApplicable (what: string) (name: string) (maximum: string) (o: Outcome) : DiagnosticMessage =
    match codeIn Tests.CallCheck.effectMaxNotApplicable o with
    | [ d ] ->
        Assert.Equal(MessageSeverity.Error, d.Severity)
        Assert.Equal("Link", Pending.stageOf d)
        Assert.Equal<string list>([ name; maximum ], Tests.CallCheck.argsOf d)
        d
    | ds -> Values.fail (sprintf "%s: expected one MaxEffectNotApplicable (4106), got %d. %s" what ds.Length (describeOutcome o))

let effectFaultKindData : obj[] seq =
    Tests.CallCheck.effectFaultKinds |> Seq.map (fun k -> [| box k |])

let faultCases : obj[] seq =
    [ for kind in Tests.CallCheck.effectFaultKinds do
        for path in [ "Compile"; "CompileToArtifact" ] -> [| box kind; box path |] ]

[<Theory>]
[<MemberData(nameof faultCases)>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 A call to a function whose Effect gives no effect fails at Link with MaxEffectNotApplicable under Pure`` (kind: string, path: string) =
    let o = { withMax FunctionEffect.Pure Script.defaults with Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", kind) :> IJyroFunction ] }
    let r = compileOn path o "Output.r = Broken()"
    let what = sprintf "%s, Effect %s" path kind
    refused what r
    let d = oneNotApplicable what "Broken" "Pure" r
    Assert.True(d.Location.HasValue && d.Location.Value.Line = 1, sprintf "%s: not placed at the call. %s" what (describeOutcome r))

[<Theory>]
[<MemberData(nameof faultCases)>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 MaxEffectNotApplicable is given whatever the maximum in force, Mutating included`` (kind: string, path: string) =
    for maximum in effects do
        let o = { withMax maximum Script.defaults with Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", kind) :> IJyroFunction ] }
        let r = compileOn path o "Output.r = Broken()"
        let what = sprintf "%s, Effect %s, maximum %A" path kind maximum
        refused what r
        oneNotApplicable what "Broken" (string maximum) r |> ignore

[<Theory>]
[<MemberData(nameof faultCases)>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 Each call to such a function has its own diagnostic, placed at the call`` (kind: string, path: string) =
    let o = { withMax FunctionEffect.Pure Script.defaults with Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", kind) :> IJyroFunction ] }
    let r = compileOn path o "Output.a = Broken()\nOutput.b = 1\nvar c = [Broken()]\nOutput.d = Length([1])"
    refused (sprintf "%s, Effect %s" path kind) r
    let ds = codeIn Tests.CallCheck.effectMaxNotApplicable r
    Assert.Equal<int list>([ 1; 3 ], [ for d in ds -> if d.Location.HasValue then d.Location.Value.Line else 0 ])
    for d in ds do
        Assert.Equal(MessageSeverity.Error, d.Severity)
        Assert.Equal("Link", Pending.stageOf d)
        Assert.Equal<string list>([ "Broken"; "Pure" ], Tests.CallCheck.argsOf d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 The function that gives an effect is not refused beside the one that does not`` () =
    let fine = Host.recorderWithEffect "Fine" FunctionEffect.Pure
    let o =
        { withMax FunctionEffect.Pure Script.defaults with
            Functions = [ fine.AsFunction; Tests.CallCheck.EffectFaultFunction("Broken", "InvalidOperationException") :> IJyroFunction ] }
    let r = compileOn "Compile" o "Output.a = Fine()\nOutput.b = Broken()"
    refused "Fine beside Broken" r
    let d = oneNotApplicable "Fine beside Broken" "Broken" "Pure" r
    Assert.Equal(2, d.Location.Value.Line)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 A function whose Effect throws is not called when the compilation is refused`` () =
    let m = marker ()
    let o = { withMax FunctionEffect.Pure Script.defaults with Functions = [ m.AsFunction; Tests.CallCheck.EffectFaultFunction("Broken", "InvalidOperationException") :> IJyroFunction ] }
    let r = Script.runWith o "Mark(1)\nOutput.r = Broken()"
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.Equal(0, m.CallCount)
    Assert.Contains(Tests.CallCheck.effectMaxNotApplicable, Script.codes r)

[<Theory>]
[<InlineData("InvalidOperationException")>]
[<InlineData("JyroRuntimeException")>]
[<InlineData("TimeoutException")>]
[<InlineData("Tag99")>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 A function whose Effect gives no effect is not a compilation time limit`` (kind: string) =
    let o = { withMax FunctionEffect.Pure Script.defaults with Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", kind) :> IJyroFunction ]; CompilationTimeout = None }
    let r = compileOn "Compile" o "Output.r = Broken()"
    refused kind r
    Assert.Empty(codeIn MessageCode.CompilationTimeLimitExceeded r)
    oneNotApplicable kind "Broken" "Pure" r |> ignore

/// An artifact whose function table names `names`, written by a host whose functions have an
/// effect, and the names that a load reads in the table's order (JyroArtifactInfo.RequiredFunctions).
let private artifactNaming (names: string list) : byte[] * string list =
    let writers = [ for n in names -> (Host.recorderWithEffect n FunctionEffect.Pure).AsFunction ]
    let source = String.Join("\n", [ for n in names -> sprintf "Output.%s = %s()" (n.ToLowerInvariant()) n ])
    let bytes = Script.compileBytesWith { pinned with Functions = writers } source |> Script.valueOf
    let info = JyroArtifactInfo.Read bytes |> Script.valueOf
    bytes, [ for n in info.RequiredFunctions do if List.contains n names then yield n ]

[<Theory>]
[<MemberData(nameof effectFaultKindData)>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 A load whose function table names such a function fails at Link with MaxEffectNotApplicable, with no place`` (kind: string) =
    let bytes, _ = artifactNaming [ "Broken" ]
    let loader = { withMax FunctionEffect.Pure pinned with Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", kind) :> IJyroFunction ] }
    let r = loadOn loader bytes
    refused (sprintf "a load, Effect %s" kind) r
    Assert.Empty(codeIn MessageCode.ArtifactEffectExceedsMax r)
    let d = oneNotApplicable (sprintf "a load, Effect %s" kind) "Broken" "Pure" r
    Assert.False(d.Location.HasValue, sprintf "a load: the diagnostic has a place. %s" (describeOutcome r))

[<Theory>]
[<MemberData(nameof effectFaultKindData)>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 A load gives one diagnostic for each such function of the table, in the table's order`` (kind: string) =
    let bytes, tableOrder = artifactNaming [ "Zeta"; "Alpha"; "Mid"; "Fine" ]
    let broken = [ "Zeta"; "Alpha"; "Mid" ] |> List.map (fun n -> Tests.CallCheck.EffectFaultFunction(n, kind) :> IJyroFunction)
    let loader = { withMax FunctionEffect.Pure pinned with Functions = (Host.recorderWithEffect "Fine" FunctionEffect.Pure).AsFunction :: broken }
    let r = loadOn loader bytes
    refused (sprintf "a load, Effect %s" kind) r
    let ds = codeIn Tests.CallCheck.effectMaxNotApplicable r
    let expected = tableOrder |> List.filter (fun n -> n <> "Fine")
    Assert.Equal<string list>(expected, [ for d in ds -> List.head (Tests.CallCheck.argsOf d) ])
    for d in ds do
        Assert.Equal(MessageSeverity.Error, d.Severity)
        Assert.Equal("Link", Pending.stageOf d)
        Assert.Equal("Pure", List.item 1 (Tests.CallCheck.argsOf d))
        Assert.Equal(2, (Tests.CallCheck.argsOf d).Length)
        Assert.False(d.Location.HasValue, "a load: the diagnostic has a place")

[<Theory>]
[<InlineData("Pure")>]
[<InlineData("NonDeterministic")>]
[<InlineData("Blocking")>]
[<InlineData("Mutating")>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 A load names the maximum of the loading host`` (maximumName: string) =
    let maximum = effectNamed maximumName
    let bytes, _ = artifactNaming [ "Broken" ]
    let loader = { withMax maximum pinned with Functions = [ Tests.CallCheck.EffectFaultFunction("Broken", "InvalidOperationException") :> IJyroFunction ] }
    let r = loadOn loader bytes
    refused maximumName r
    oneNotApplicable maximumName "Broken" maximumName r |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2136")>]
let ``JYRO-REQ-2136 MaxEffectNotApplicable has the number 4106`` () =
    Assert.Equal(4106, int Tests.CallCheck.effectMaxNotApplicable)
    Assert.Equal("MaxEffectNotApplicable", Enum.GetName(typeof<MessageCode>, Tests.CallCheck.effectMaxNotApplicable))

// -- 120.6.x Effects and artifacts ---------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2154")>]
let ``JYRO-REQ-2154 WritingAnArtifact_AppliesTheMaximum`` () =
    let r = Script.compileBytesWith (withMax FunctionEffect.Pure Script.defaults) "Output.t = Now()"
    fails MessageCode.FunctionEffectExceedsMax r
    Assert.False(r.HasValue, "Bytes were given for a refused program")
    let h = Host.recorderWithEffect "Write" FunctionEffect.Mutating
    let r2 = Script.compileBytesWith { withMax FunctionEffect.Blocking Script.defaults with Functions = [ h.AsFunction ] } "Write(1)"
    fails MessageCode.FunctionEffectExceedsMax r2
    Assert.False(r2.HasValue)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2154")>]
let ``JYRO-REQ-2154 WritingAnArtifact_GivesOneDiagnosticForEachRejectedCall_AsACompilationDoes`` () =
    let src = "Output.a = Now()\nOutput.b = 1\nOutput.c = Today()\nOutput.d = Length([1])"
    let o = withMax FunctionEffect.Pure Script.defaults
    let written = Script.compileBytesWith o src
    Assert.False(written.IsSuccess, "the write succeeded")
    Assert.False(written.HasValue, "Bytes were given for a refused program")
    Assert.Equal<MessageCode list>([ MessageCode.FunctionEffectExceedsMax; MessageCode.FunctionEffectExceedsMax ], Script.errorCodes written)
    let compiled = Script.compileWith o src
    Assert.Equal<MessageCode list>(Script.errorCodes compiled, Script.errorCodes written)
    Assert.Equal(2, effectCount compiled)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2154")>]
let ``JYRO-REQ-2154 WritingAnArtifact_WhenTheCompilationUnderTheSameMaximumAccepts_GivesBytes`` () =
    let o = withMax FunctionEffect.Pure Script.defaults
    let src = "Output.a = Length([1, 2])\nOutput.b = 2"
    Assert.True((Script.compileWith o src).IsSuccess, "the same source compiles under Pure")
    let r = Script.compileBytesWith o src
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(r.HasValue)
    Assert.NotEmpty(Script.valueOf r)
    Assert.Equal(0, effectCount r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2053")>]
[<Trait("requirement", "JYRO-REQ-1514")>]
let ``LoadingAnArtifact_AppliesTheLoadingHostsMaximum`` () =
    let bytes = Script.compileBytesWith (withMax FunctionEffect.Mutating Script.defaults) "Sleep(0)\nOutput.r = 1" |> Script.valueOf
    Script.runBytesWith (withMax FunctionEffect.NonDeterministic Script.defaults) bytes |> fails MessageCode.ArtifactEffectExceedsMax
    Script.runBytesWith (withMax FunctionEffect.Pure Script.defaults) bytes |> fails MessageCode.ArtifactEffectExceedsMax
    Script.runBytesWith (withMax FunctionEffect.Blocking Script.defaults) bytes |> ok |> ignore
    Script.runBytesWith (withMax FunctionEffect.Mutating Script.defaults) bytes |> ok |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2053")>]
[<Trait("requirement", "JYRO-REQ-1514")>]
let ``LoadingAnArtifact_UsesTheEffectsTheLoadingHostRegistered`` () =
    let pureH = Host.recorderWithEffect "H" FunctionEffect.Pure
    let bytes = Script.compileBytesWith { Script.defaults with Functions = [ pureH.AsFunction ] } "H(1)\nOutput.r = 1" |> Script.valueOf
    let mutH = Host.recorderWithEffect "H" FunctionEffect.Mutating
    Script.runBytesWith { withMax FunctionEffect.Blocking Script.defaults with Functions = [ mutH.AsFunction ] } bytes
    |> fails MessageCode.ArtifactEffectExceedsMax
    Script.runBytesWith { withMax FunctionEffect.Blocking Script.defaults with Functions = [ pureH.AsFunction ] } bytes
    |> ok |> ignore
    throwsWithCode MessageCode.ArtifactEffectExceedsMax (fun () ->
        Script.loadBytesOrThrow { withMax FunctionEffect.Blocking Script.defaults with Functions = [ mutH.AsFunction ] } bytes)

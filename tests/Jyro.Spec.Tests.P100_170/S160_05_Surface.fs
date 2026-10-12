/// Part 160.5 (the host surface speaks the host language) and 160.6 (library scripts).
module RealTree.Jyro.Spec.Tests.S160_05_Surface

open System
open System.Collections.Generic
open System.Reflection
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private apiAssemblies () =
    [ "Jyro.Core"; "Jyro.Parser"; "Jyro.Compiler"; "Jyro.Stdlib"; "Jyro.Api" ] |> List.map Assembly.Load

let private declared = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

/// Every type named by a public signature of the API: parameter, return,
/// property, field and event types, base types and interfaces, with their
/// generic arguments and element types, each with where it was found.
let private namedTypes () : (string * Type) list =
    let rec expand (t: Type) : Type list =
        if isNull t then []
        else
            let inner =
                (if t.HasElementType then expand (t.GetElementType()) else [])
                @ (if t.IsGenericType then t.GetGenericArguments() |> Array.toList |> List.collect expand else [])
            t :: inner
    [ for a in apiAssemblies () do
        for t in a.GetExportedTypes() do
            for b in (Option.ofObj t.BaseType |> Option.toList) @ List.ofArray (t.GetInterfaces()) do
                for x in expand b -> t.FullName + " : base", x
            for m in t.GetMethods(declared) do
                for x in expand m.ReturnType -> sprintf "%s.%s return" t.Name m.Name, x
                for p in m.GetParameters() do
                    for x in expand p.ParameterType -> sprintf "%s.%s(%s)" t.Name m.Name p.Name, x
            for c in t.GetConstructors() do
                for p in c.GetParameters() do
                    for x in expand p.ParameterType -> sprintf "%s.ctor(%s)" t.Name p.Name, x
            for p in t.GetProperties(declared) do
                for x in expand p.PropertyType -> sprintf "%s.%s" t.Name p.Name, x
            for f in t.GetFields(declared) do
                for x in expand f.FieldType -> sprintf "%s.%s" t.Name f.Name, x ]

let private isFSharpOnly (t: Type) =
    let ns = if isNull t.Namespace then "" else t.Namespace
    ns.StartsWith "Microsoft.FSharp" || t.Name.StartsWith "FSharp"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2100")>]
let ``EveryTypeAHostNames_IsExpressibleInCSharp`` () =
    let offenders = namedTypes () |> List.filter (snd >> isFSharpOnly) |> List.map (fun (w, t) -> sprintf "%s: %s" w t.FullName) |> List.distinct
    Assert.True(offenders.IsEmpty, String.Join("\n", offenders))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2100")>]
let ``EveryExportedType_IsNotAnFSharpModuleOnlyConstruct`` () =
    // A public type must be usable as a plain CLR type: no F#-only kinds such as
    // measure types or type providers leak (by-ref-like or pointer types would not be nameable either).
    for a in apiAssemblies () do
        for t in a.GetExportedTypes() do
            Assert.False(t.IsPointer || t.IsByRefLike, t.FullName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2101")>]
let ``OptionalValues_UseTheHostLanguagesConventionForAbsence`` () =
    let options =
        namedTypes ()
        |> List.filter (fun (_, t) -> t.IsGenericType && (t.GetGenericTypeDefinition().FullName.Contains "FSharpOption" || t.GetGenericTypeDefinition().FullName.Contains "FSharpValueOption"))
    Assert.True(options.IsEmpty, sprintf "%A" (options |> List.map fst))
    Assert.Equal(typeof<Nullable<SourceLocation>>, typeof<DiagnosticMessage>.GetProperty("Location").PropertyType)
    let d = DiagnosticMessage.Error(MessageCode.RuntimeError, MessageStage.Execute)
    Assert.False(d.Location.HasValue)
    let r = JyroResult<int>.Failure(d)
    Assert.False(r.HasValue)
    let has, _ = r.TryGetValue()
    Assert.False(has)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2102")>]
let ``Sequences_HaveTypesTheHostCanCreateAndEnumerate`` () =
    let bad =
        namedTypes ()
        |> List.filter (fun (_, t) ->
            t.IsGenericType
            && (let n = t.GetGenericTypeDefinition().FullName
                n.Contains "FSharpList" || n.Contains "FSharpSet" || n.Contains "FSharpMap" || n.Contains "FSharpSeq"))
    Assert.True(bad.IsEmpty, sprintf "%A" (bad |> List.map fst))
    // Give sequences the host made, and enumerate those it reads.
    // (JyroException has no public constructor, JYRO-REQ-1885: the host reads the sequence of one it catches.)
    let ex = Assert.ThrowsAny<JyroException>(fun () -> JyroCompiler().WithSource("var s: string").CompileOrThrow() |> ignore)
    let mutable n = 0
    for _ in ex.Messages do n <- n + 1
    Assert.Equal(1, n)
    let r = JyroResult<int>.Failure([| DiagnosticMessage.Error(MessageCode.RuntimeError, MessageStage.Execute) |] :> seq<_>)
    Assert.Equal(1, Seq.length r.Messages)
    let c = JyroCompiler().AddFunctions([| (Host.recorder "A").AsFunction; (Host.recorder "B").AsFunction |])
    let p = c.WithSource("A()\nB()").Compile()
    Assert.True(p.IsSuccess, Script.describe p)
    let bytes = Script.compileBytesWith Script.defaults "Output.x = Length([1])" |> Script.valueOf
    let info = JyroArtifactInfo.Read bytes |> Script.valueOf
    Assert.Equal<string list>([ "Length" ], [ for f in info.RequiredFunctions -> f ])
    Assert.NotEmpty([ for f in stdlibAll () -> f.Name ])
    Assert.Equal(1, Seq.length (Host.signature "S" [ Parameter.Required("a", ParameterType.Any) ] ParameterType.Any).Parameters)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2104")>]
let ``InterfaceMembers_AreAlsoOnTheConcreteTypeTheHostDerivesFrom`` () =
    let problems = ResizeArray<string>()
    for a in apiAssemblies () do
        for t in a.GetExportedTypes() |> Array.filter (fun t -> t.IsClass) do
            for i in t.GetInterfaces() |> Array.filter (fun i -> i.Namespace = "RealTree.Jyro") do
                for im in i.GetMethods() do
                    let ps = im.GetParameters() |> Array.map (fun p -> p.ParameterType)
                    let found = t.GetMethod(im.Name, BindingFlags.Public ||| BindingFlags.Instance, null, ps, null)
                    if isNull found then problems.Add(sprintf "%s lacks public %s.%s" t.FullName i.Name im.Name)
    Assert.True(problems.Count = 0, String.Join("\n", problems))

type private Derived() =
    inherit JyroFunctionBase("Derived", Host.signature "Derived" [] ParameterType.Any)
    override _.Effect = FunctionEffect.NonDeterministic
    override _.Execute(_, _) = JyroString("d") :> JyroValue

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2104")>]
let ``DerivedHostFunction_ExposesNameEffectSignatureAndExecute_WithoutACast`` () =
    let d = Derived()
    Assert.Equal("Derived", d.Name)
    Assert.Equal(FunctionEffect.NonDeterministic, d.Effect)
    Assert.Equal(0, d.Signature.MaxArgs)
    Assert.Equal("d", Values.asString (d.Execute([||], JyroExecutionContext())))

// -- 160.6.x Library scripts ------------------------------------------------------------------------

let private twice = "func Twice(x)\n  return x * 2\nend"
let private thrice = "func Thrice(x)\n  return x * 3\nend\nunion Shape Circle(r) end"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1849")>]
let ``JYRO-REQ-1849 Host_CanAddLibraryScripts_ThroughSourceOrParsedLibraries`` () =
    let v = Script.runWith { Script.defaults with Libraries = [ twice; thrice ] } "Output.a = Twice(3)\nOutput.b = Thrice(2)\nOutput.c = Circle(1)" |> ok
    Assert.Equal("{\"a\":6,\"b\":6,\"c\":{\"_variant\":\"Circle\",\"r\":1}}", Values.toJson v)
    let parsed = JyroLibrary.Parse twice |> Script.valueOf
    Assert.Equal(1, parsed.DeclarationCount)
    let p = JyroCompiler().WithStdlib().AddLibrary(parsed).WithSource("Output.a = Twice(3)").Compile() |> Script.valueOf
    Assert.Equal("{\"a\":6}", Values.toJson ((Script.runnerFor Script.defaults p).Execute() |> ok))

let forbiddenStatements : obj[] seq =
    [ "var x = 1"; "Output.a = 1"; "if true then Output.a = 1 end"; "while false do Output.a = 1 end"
      "foreach e in [1] do Output.a = e end"; "for i in 0 to 1 do Output.a = i end"; "switch 1 do case 1 then Output.a = 1 end"
      "return 1"; "fail \"x\""; "break"; "continue"; "Length([1])"; "exit"
      "union LU LA LB end\nmatch LA() do case LA then Output.a = 1 case LB then Output.a = 2 end"; "delete Output.a" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<MemberData(nameof forbiddenStatements)>]
[<Trait("requirement", "JYRO-REQ-1609")>]
let ``LibraryWithAStatementOtherThanADeclaration_IsRejectedWithLibraryStatementForbidden`` (statement: string) =
    let lib = twice + "\n" + statement
    let viaSource = Script.compileWith { Script.defaults with Libraries = [ lib ] } "Output.r = 1"
    Assert.Contains(MessageCode.LibraryStatementForbidden, Script.errorCodes viaSource)
    let parsed = JyroLibrary.Parse lib
    if parsed.IsSuccess then
        let viaLibrary = JyroCompiler().WithStdlib().AddLibrary(Script.valueOf parsed).WithSource("Output.r = 1").Compile()
        Assert.Contains(MessageCode.LibraryStatementForbidden, Script.errorCodes viaLibrary)
    else
        Assert.Contains(MessageCode.LibraryStatementForbidden, Script.errorCodes parsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1609")>]
let ``LibraryOfFunctionAndUnionDeclarations_IsAccepted`` () =
    let r = Script.compileWith { Script.defaults with Libraries = [ twice + "\n" + thrice ] } "Output.r = Twice(1)"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True((JyroLibrary.Parse(twice + "\n" + thrice)).IsSuccess)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1610")>]
let ``ScriptFunctionWithALibraryFunctionsName_IsRejectedWithFunctionShadowsLibrary`` () =
    let lib = "func Helper()\n  return 1\nend"
    let scripts =
        [ "func Helper()\n  return 2\nend\nOutput.r = Helper()"
          "Output.r = Helper()\nfunc Helper()\n  return 2\nend"
          "func Helper(a, b)\n  return a\nend" ]
    let parsed = JyroLibrary.Parse lib |> Script.valueOf
    for s in scripts do
        Assert.Contains(MessageCode.FunctionShadowsLibrary, Script.errorCodes (JyroCompiler().WithStdlib().AddLibrarySource(lib).WithSource(s).Compile()))
        Assert.Contains(MessageCode.FunctionShadowsLibrary, Script.errorCodes (JyroCompiler().WithStdlib().WithSource(s).AddLibrarySource(lib).Compile()))
        Assert.Contains(MessageCode.FunctionShadowsLibrary, Script.errorCodes (JyroCompiler().WithStdlib().AddLibrary(parsed).WithSource(s).Compile()))
        Assert.Contains(MessageCode.FunctionShadowsLibrary, Script.errorCodes (JyroCompiler().WithStdlib().WithSource(s).AddLibrary(parsed).Compile()))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1853")>]
let ``JYRO-REQ-1853 CallsInALibrary_ObeyTheLibraryEffectMaximum`` () =
    let nowLib = "func Stamp()\n  return Now()\nend"
    let sleepLib = "func Nap()\n  Sleep(0)\n  return 1\nend"
    let parsedNow = JyroLibrary.Parse nowLib |> Script.valueOf
    Script.compileWith { Script.defaults with LibraryMaxEffect = Some FunctionEffect.Pure; Libraries = [ nowLib ] } "Output.r = 1"
    |> fails MessageCode.FunctionEffectExceedsMax
    JyroCompiler().WithStdlib().WithLibraryMaxEffect(FunctionEffect.Pure).AddLibrary(parsedNow).WithSource("Output.r = 1").Compile()
    |> fails MessageCode.FunctionEffectExceedsMax
    let within = Script.compileWith { Script.defaults with LibraryMaxEffect = Some FunctionEffect.Mutating; MaxEffect = Some FunctionEffect.Pure; Libraries = [ sleepLib ] } "Output.r = 1"
    Assert.True(within.IsSuccess, Script.describe within)
    Script.compileWith { Script.defaults with LibraryMaxEffect = Some FunctionEffect.Mutating; MaxEffect = Some FunctionEffect.Pure; Libraries = [ sleepLib ] } "Output.r = Nap()"
    |> fails MessageCode.FunctionEffectExceedsMax
    let blocking = Script.compileWith { Script.defaults with LibraryMaxEffect = Some FunctionEffect.Blocking; Libraries = [ sleepLib ] } "Output.r = 1"
    Assert.True(blocking.IsSuccess, Script.describe blocking)
    Script.compileWith { Script.defaults with LibraryMaxEffect = Some FunctionEffect.NonDeterministic; Libraries = [ sleepLib ] } "Output.r = 1"
    |> fails MessageCode.FunctionEffectExceedsMax

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1853")>]
let ``JYRO-REQ-1853 MaximumPureForTheScriptAndNonDeterministicForLibraries_NowInALibraryCompiles_AndSleepIsRejectedAtLink`` () =
    let nowLib = "func Stamp()\n  return Now()\nend"
    let sleepLib = "func Nap()\n  Sleep(1)\n  return 1\nend"
    let options libraries =
        { Script.defaults with MaxEffect = Some FunctionEffect.Pure; LibraryMaxEffect = Some FunctionEffect.NonDeterministic; Libraries = libraries }
    let accepted = Script.compileWith (options [ nowLib ]) "Output.r = 1"
    Assert.True(accepted.IsSuccess, Script.describe accepted)
    let rejected = Script.compileWith (options [ sleepLib ]) "Output.r = 1"
    fails MessageCode.FunctionEffectExceedsMax rejected
    let m = rejected.Messages |> Seq.find (fun m -> m.Code = MessageCode.FunctionEffectExceedsMax)
    Assert.Equal("Link", Pending.stageOf m)
    // The script itself, under Pure, still refuses a call of Now().
    Script.compileWith (options [ nowLib ]) "Output.r = Now()" |> fails MessageCode.FunctionEffectExceedsMax

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1853")>]
let ``JYRO-REQ-1853 AScriptMaximumAboveTheLibraryMaximum_DoesNotRelaxALibrary`` () =
    let nowLib = "func Stamp()\n  return Now()\nend"
    let o = { Script.defaults with MaxEffect = Some FunctionEffect.Mutating; LibraryMaxEffect = Some FunctionEffect.Pure; Libraries = [ nowLib ] }
    Script.compileWith o "Output.r = 1" |> fails MessageCode.FunctionEffectExceedsMax
    // The library maximum above the script's does not relax the script.
    let o2 = { Script.defaults with MaxEffect = Some FunctionEffect.Pure; LibraryMaxEffect = Some FunctionEffect.Mutating; Libraries = [ nowLib ] }
    let r = Script.compileWith o2 "Output.r = 1"
    Assert.True(r.IsSuccess, Script.describe r)
    // The script's own call of Now() is refused under its Pure maximum.
    Script.compileWith o2 "Output.r = Now()" |> fails MessageCode.FunctionEffectExceedsMax

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1853")>]
let ``JYRO-REQ-1853 ACallAtTheLibraryMaximum_IsNotRejectedForItsEffect`` () =
    let nowLib = "func Stamp()\n  return Now()\nend"
    let sleepLib = "func Nap()\n  Sleep(1)\n  return 1\nend"
    for maximum, lib in [ FunctionEffect.NonDeterministic, nowLib; FunctionEffect.Blocking, sleepLib; FunctionEffect.Mutating, sleepLib ] do
        let r = Script.compileWith { Script.defaults with LibraryMaxEffect = Some maximum; Libraries = [ lib ] } "Output.r = 1"
        Assert.True(r.IsSuccess, sprintf "%A: %s" maximum (Script.describe r))

// -- JYRO-REQ-2120 (160.7): a host cannot construct JyroRuntimeException --

/// Every constructor of `t`, of any accessibility, that is public, protected or protected
/// internal (C#'s `protected internal` is IsFamilyOrAssembly). Private, internal and private
/// protected (IsFamilyAndAssembly) constructors are allowed.
let private reachableConstructors (t: Type) : string list =
    t.GetConstructors(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
    |> Array.filter (fun c -> c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly)
    |> Array.map (fun c -> c.ToString())
    |> List.ofArray

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 JyroRuntimeException is a public sealed Exception, derived through JyroException, with a get-only Code and Args`` () =
    let t = typeof<JyroRuntimeException>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsPublic, "JyroRuntimeException is public")
    Assert.True(t.IsSealed, "public sealed class JyroRuntimeException")
    Assert.True(typeof<Exception>.IsAssignableFrom t, "JyroRuntimeException derives from System.Exception")
    // `: JyroException` (JYRO-REQ-1886), reached by name: the class is declared with that one, not with Exception.
    Assert.NotNull(t.BaseType)
    Assert.Equal("RealTree.Jyro.JyroException", t.BaseType.FullName)
    Assert.Equal(typeof<Exception>, t.BaseType.BaseType)
    let code = t.GetProperty("Code", BindingFlags.Public ||| BindingFlags.Instance)
    Assert.True(not (isNull code) && code.PropertyType = typeof<MessageCode> && not (isNull (code.GetGetMethod())), "public MessageCode Code { get; }")
    let args = t.GetProperty("Args", BindingFlags.Public ||| BindingFlags.Instance)
    Assert.True(not (isNull args) && args.PropertyType = typeof<obj[]> && not (isNull (args.GetGetMethod())), "public object[] Args { get; }")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 JyroRuntimeException has no public or protected constructor`` () =
    Assert.Empty(reachableConstructors typeof<JyroRuntimeException>)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 JYRO-DEF-551 no host can give a JyroRuntimeException a message that the diagnostic then loses`` () =
    // JYRO-DEF-551: the constructors (code, message), (code, message, line, column),
    // (code, message, args) and (code, message, args, line, column) took a message that
    // never reached the diagnostic. No constructor a host can reach takes a message, or any
    // other parameter, now.
    let withMessage =
        typeof<JyroRuntimeException>.GetConstructors(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Array.filter (fun c -> (c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly) && c.GetParameters() |> Array.exists (fun p -> p.ParameterType = typeof<string>))
    Assert.Empty(withMessage)

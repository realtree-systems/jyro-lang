/// 160.2.3, written by the setup verifier to prove the shared helpers and the
/// project wiring. The chapter 100-170 verifier owns this file from here on:
/// extend it, rename it or fold it into a fuller S160_02 file as it sees fit.
module RealTree.Jyro.Spec.Tests.S160_02_Setup

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// `ToUpper` is a standard library string function (91).
let private stdlibName = "ToUpper"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``Compile_WhenHostFunctionHasStdlibNameAndStdlibAdded_FailsWithHostFunctionOverridesStdlib`` () =
    let host = Host.recorder stdlibName
    let result =
        Script.compileWith
            { Script.defaults with Functions = [ host.AsFunction ] }
            "Output.r = 1"
    Assert.False(Script.ok result, Script.describe result)
    Script.expectError MessageCode.HostFunctionOverridesStdlib result

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``Run_WhenHostFunctionHasStdlibNameAndStdlibAdded_FailsBeforeAnyStatementRuns`` () =
    let host = Host.recorder stdlibName
    let witness = Host.recorder "Witness"
    let result =
        Script.runWith
            { Script.defaults with Functions = [ host.AsFunction; witness.AsFunction ] }
            "Witness(1)\nOutput.r = ToUpper(\"a\")"
    Script.expectError MessageCode.HostFunctionOverridesStdlib result
    Assert.Equal(0, witness.CallCount)
    Assert.Equal(0, host.CallCount)

// -- Added by the chapter 100-170 verifier: the other orders and paths of 160.2.3 --

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``Compile_WhenTheHostFunctionIsAddedBeforeTheStandardLibrary_FailsWithHostFunctionOverridesStdlib`` () =
    let host = Host.recorder stdlibName
    let r = JyroCompiler().AddFunction(host).WithStdlib().WithSource("Output.r = 1").Compile()
    Script.expectError MessageCode.HostFunctionOverridesStdlib r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``CompileToArtifact_WhenAHostFunctionHasAStandardName_Fails`` () =
    let host = Host.recorder stdlibName
    let r = Script.compileBytesWith { Script.defaults with Functions = [ host.AsFunction ] } "Output.r = 1"
    Script.expectError MessageCode.HostFunctionOverridesStdlib r
    Assert.False(r.HasValue)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``LoadingAnArtifact_WhenAHostFunctionHasAStandardName_FailsBeforeAnyStatementRuns`` () =
    let bytes = Script.compileBytesWith Script.defaults "Output.r = 1" |> Script.valueOf
    let host = Host.recorder stdlibName
    let witness = Host.recorder "Witness"
    let r = Script.runBytesWith { Script.defaults with Functions = [ host.AsFunction; witness.AsFunction ] } bytes
    Script.expectError MessageCode.HostFunctionOverridesStdlib r
    Assert.Equal(0, witness.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``Compile_WhenTheStandardFunctionIsOmitted_AHostFunctionOfItsNameCompiles`` () =
    let host = Host.func stdlibName [ Parameter.Required("t", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure (fun _ -> Values.str "host")
    let fns = Tests.Support.stdlibWithout [ stdlibName ] @ [ host.AsFunction ]
    let r = Script.runWith { Script.defaults with Stdlib = false; Functions = fns } "Output.r = ToUpper(\"a\")"
    Assert.True(r.IsSuccess, Script.describe r)

// -- jyro-spec--d-108: a standard library function by its origin, however it was added --

/// The ways a host can add the standard function `Sum` (a StdlibRegistry function).
let private stdlibWays : (string * (JyroCompiler -> JyroCompiler)) list =
    [ "WithStdlib", (fun c -> c.WithStdlib())
      "AddFunctions-GetAll", (fun c -> c.AddFunctions(Tests.Support.stdlibAll ()))
      "AddFunction-Sum", (fun c -> c.AddFunction((Tests.Support.stdlibTryGet "Sum").Value))
      "AddFunctions-subset", (fun c -> c.AddFunctions([ (Tests.Support.stdlibTryGet "Sum").Value; (Tests.Support.stdlibTryGet "Length").Value ])) ]

let stdlibWayRows : obj[] seq =
    [ for (way, _) in stdlibWays do
        for hostFirst in [ true; false ] do
            for path in [ "Compile"; "CompileToArtifact"; "Run" ] -> [| box way; box hostFirst; box path |] ]

[<Theory>]
[<MemberData(nameof stdlibWayRows)>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``HostFunctionNamedSum_WithTheStandardSumAddedAnyWay_FailsWithHostFunctionOverridesStdlib`` (way: string, hostFirst: bool, path: string) =
    let addStd = stdlibWays |> List.find (fst >> (=) way) |> snd
    let host = Host.recorder "Sum"
    let witness = Host.recorder "Witness"
    let compiler =
        let c = JyroCompiler().AddFunction(witness)
        let c = if hostFirst then (addStd (c.AddFunction(host))) else (addStd c).AddFunction(host)
        c.WithSource("Witness(1)\nOutput.r = Sum([1, 2])")
    let codes, ran =
        match path with
        | "Compile" -> Script.errorCodes (compiler.Compile()), false
        | "CompileToArtifact" -> Script.errorCodes (compiler.CompileToArtifact()), false
        | _ ->
            let r = compiler.Compile()
            if r.IsSuccess then
                let e = (Script.runnerFor Script.defaults (Script.valueOf r)).Execute()
                Script.errorCodes e, true
            else Script.errorCodes r, false
    Assert.Equal<MessageCode list>([ MessageCode.HostFunctionOverridesStdlib ], codes)
    Assert.DoesNotContain(MessageCode.DuplicateFunction, codes)
    Assert.False(ran, "the program compiled and ran")
    Assert.Equal(0, witness.CallCount)
    Assert.Equal(0, host.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``TheBookExample_EveryStdlibFunctionThroughAddFunctionsAndAHostSum_IsHostFunctionOverridesStdlib`` () =
    let r = JyroCompiler().AddFunctions(Tests.Support.stdlibAll ()).AddFunction(Host.recorder "Sum").WithSource("Output.r = 1").Compile()
    Script.expectError MessageCode.HostFunctionOverridesStdlib r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``StdlibWithoutSumThroughAddFunctions_AndAHostSum_Compiles`` () =
    let host = Host.func "Sum" [ Parameter.Required("x", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure (fun _ -> Values.str "host")
    let r =
        JyroCompiler().AddFunctions(Tests.Support.stdlibWithout [ "Sum" ]).AddFunction(host)
            .WithSource("Output.r = Sum([1])").Compile()
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1590")>]
let ``TwoHostFunctionsOfOneNonStandardName_AreNotHostFunctionOverridesStdlib`` () =
    // Neither is a StdlibRegistry function, so whatever else is said of it, it is not an override.
    let r = JyroCompiler().WithStdlib().AddFunction(Host.recorder "Mine").AddFunction(Host.recorder "Mine").WithSource("Output.r = 1").Compile()
    Assert.DoesNotContain(MessageCode.HostFunctionOverridesStdlib, Script.errorCodes r)

// -- JYRO-REQ-1891 (160.2): a host function reports its own failure by a reason and values ----

let private readArgs (d: DiagnosticMessage) : string list =
    d.Args |> Array.map (fun a -> System.Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture)) |> List.ofArray

/// The public members of `t` named `name`, of the binding flags given.
let private publicMembersNamed (t: System.Type) (name: string) : System.Reflection.MemberInfo list =
    let flags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Instance
    t.GetMember(name, System.Reflection.MemberTypes.All, flags) |> List.ofArray

/// The public methods that `t` declares, by name, as `(parameter type name parameter name, ...) : return type`,
/// with `params` marked: the shape that JYRO-REQ-1891 gives each.
let private declaredShapes (t: System.Type) : string list =
    let flags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Instance ||| System.Reflection.BindingFlags.DeclaredOnly
    t.GetMethods flags
    |> Array.filter (fun m -> not m.IsSpecialName)
    |> Array.map (fun m ->
        let ps =
            m.GetParameters()
            |> Array.map (fun p ->
                let isParams = p.GetCustomAttributes(typeof<System.ParamArrayAttribute>, false).Length > 0
                sprintf "%s%s %s" (if isParams then "params " else "") p.ParameterType.Name p.Name)
        sprintf "%s%s(%s) : %s" (if m.IsStatic then "static " else "") m.Name (String.Join(", ", ps)) m.ReturnType.Name)
    |> Array.sort
    |> List.ofArray

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 JyroError is a static class that declares exactly Host and Runtime`` () =
    let je = typeof<JyroError>
    Assert.Equal("RealTree.Jyro", je.Namespace)
    Assert.True(je.IsPublic && je.IsAbstract && je.IsSealed, "JyroError is a public static class")
    Assert.Equal<string list>(
        [ "static Host(String reason, params Object[] values) : JyroRuntimeException"
          "static Runtime(MessageCode code, params Object[] args) : JyroRuntimeException" ],
        declaredShapes je)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 no public type of the shipped assemblies declares RaiseHost, RaiseRuntime or RaiseError`` () =
    let flags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Instance ||| System.Reflection.BindingFlags.DeclaredOnly
    let found =
        [ for name in [ "Jyro.Core"; "Jyro.Parser"; "Jyro.Compiler"; "Jyro.Stdlib"; "Jyro.Api" ] do
            for ty in (System.Reflection.Assembly.Load name).GetExportedTypes() do
                for m in ty.GetMembers flags do
                    let isPublic =
                        match m with
                        | :? System.Reflection.MethodBase as mb -> mb.IsPublic || mb.IsFamily || mb.IsFamilyOrAssembly
                        | _ -> true
                    if isPublic && (m.Name = "RaiseHost" || m.Name = "RaiseRuntime" || m.Name = "RaiseError") then
                        yield sprintf "%s.%s" ty.FullName m.Name ]
    Assert.True(List.isEmpty found, sprintf "members that JYRO-REQ-1891 removes: %A" found)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 Host and Runtime return the exception and do not throw it`` () =
    let host : exn = JyroError.Host("AmountNotPositive", -5)
    let runtime : exn = JyroError.Runtime(MessageCode.ArgumentTypeMismatch, "string", "number")
    Assert.IsType<JyroRuntimeException>(host) |> ignore
    Assert.IsType<JyroRuntimeException>(runtime) |> ignore
    // The Code and Args of 2120: a code of the engine and the arguments given.
    let r = runtime :?> JyroRuntimeException
    Assert.Equal(MessageCode.ArgumentTypeMismatch, r.Code)
    Assert.Equal<string list>([ "string"; "number" ], [ for a in r.Args -> string a ])
    // The same, with no values and with null values, make an exception and do not throw.
    Assert.NotNull(JyroError.Host("Why"))
    Assert.NotNull(JyroError.Host("Why", [||]))
    Assert.NotNull(JyroError.Runtime(MessageCode.InvalidJson))

/// A host function that refuses through `throw JyroError.Host(reason, values)`, and records whether the call returned.
type private Refuser(reason: string, values: obj[] option) =
    inherit JyroFunctionBase("Refuse", Host.signature "Refuse" [] ParameterType.Any)
    member val Returned = false with get, set
    override _.Effect = FunctionEffect.Pure
    override this.Execute(_, _) =
        let ex =
            match values with
            | None -> JyroError.Host(reason)
            | Some vs -> JyroError.Host(reason, vs)
        raise ex

let private onlyHostFunctionError (r: JyroResult<JyroValue>) : DiagnosticMessage =
    match List.ofSeq r.Messages with
    | [ d ] when d.Code = MessageCode.HostFunctionError -> d
    | ms -> Values.fail (sprintf "expected one HostFunctionError, got %A. %s" (ms |> List.map (fun m -> m.Code)) (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 throw JyroError.Host AmountNotPositive and -5 gives one HostFunctionError with those arguments`` () =
    let f = Refuser("AmountNotPositive", Some [| box -5 |])
    let r = Script.runWith { Script.defaults with Functions = [ f ] } "Output.a = Refuse()"
    Assert.False(r.IsSuccess)
    let d = onlyHostFunctionError r
    Assert.Equal(5006, int d.Code)
    Assert.Equal<string list>([ "AmountNotPositive"; "-5" ], readArgs d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 throw JyroError.Host Refused gives one HostFunctionError whose one argument is Refused`` () =
    let h = Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host("Refused")))
    let d = Script.runWith { Script.defaults with Functions = [ h.AsFunction ] } "Output.a = Refuse()" |> onlyHostFunctionError
    Assert.Equal<string list>([ "Refused" ], readArgs d)

let raiseForms : obj[] seq =
    [ for m in [ "Host"; "HostFromBase" ] do
        for v in [ "omitted"; "empty"; "seven-and-x" ] -> [| box m; box v |] ]

[<Theory>]
[<MemberData(nameof raiseForms)>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 the arguments are the reason then each value in order and no other`` (memberName: string, values: string) =
    let vs : obj[] option =
        match values with
        | "omitted" -> None
        | "empty" -> Some [||]
        | _ -> Some [| box 7; box "x" |]
    let fn : IJyroFunction =
        if memberName = "HostFromBase" then Refuser("Why", vs) :> IJyroFunction
        else
            (Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
                match vs with
                | None -> raise (JyroError.Host("Why"))
                | Some a -> raise (JyroError.Host("Why", a)))).AsFunction
    let d = Script.runWith { Script.defaults with Functions = [ fn ] } "Output.a = Refuse()" |> onlyHostFunctionError
    let expected = if values = "seven-and-x" then [ "Why"; "7"; "x" ] else [ "Why" ]
    Assert.Equal<string list>(expected, readArgs d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 a host function that throws JyroError.Host ends the run`` () =
    let f = Refuser("Stop", None)
    let r = Script.runWith { Script.defaults with Functions = [ f ] } "Refuse()\nOutput.after = 1"
    onlyHostFunctionError r |> ignore
    Assert.False(f.Returned, "the throw returned to the host function")
    match Script.value r with
    | Some v -> Assert.False(Tests.Support.hasProp "after" v, "the script went on after the refusal")
    | None -> ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-1891 a throw of JyroError.Host in a function of another result type ends the run`` () =
    let returned = ref false
    let h =
        Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
            let n : int = raise (JyroError.Host("Refused", [| box 1 |]))
            returned.Value <- true
            JyroNumber(n) :> JyroValue)
    let r = Script.runWith { Script.defaults with Functions = [ h.AsFunction ] } "Refuse()\nOutput.after = 1"
    let d = onlyHostFunctionError r
    Assert.Equal<string list>([ "Refused"; "1" ], readArgs d)
    Assert.False(returned.Value, "the throw returned to the host function")
    match Script.value r with
    | Some v -> Assert.False(Tests.Support.hasProp "after" v, "the script went on after the refusal")
    | None -> ()

// -- JYRO-REQ-2119 (160.2): a host function checks each call of it at link time ----------------
//
// The checking host functions are made by Tests.CallCheck, which reaches IJyroCallCheck and
// JyroCallSite without naming them (FLOORS 4.1.7). Every compile sets every limit (`pinned`,
// FLOORS 4.1.11). Not asserted, as the clause leaves them to the implementation: what
// JyroFunctionBase.CheckCall(null) does (the 'null argument' of the clause is the argument of
// CheckCall: a `null` literal in a call is a literal of 20.1.1, whose TryGetLiteral the clause
// binds and which is asserted below), the order of the checks within one compilation, and
// TryGetLiteral for other forms such as -5 or [1, 2].

module private C1677 =
    let pinned = Tests.Support.pinned
    let flags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance
    let withFns (fns: IJyroFunction list) = { pinned with Functions = fns }
    let compile (fns: IJyroFunction list) (src: string) = Script.compileWith (withFns fns) src
    let compileLibs (fns: IJyroFunction list) (libs: string list) (src: string) = Script.compileWith { withFns fns with Libraries = libs } src
    let hostLinkErrors (r: JyroResult<'T>) = Tests.CallCheck.ofCode Tests.CallCheck.hostLinkError r
    /// The literal at `index` of `site` as JSON text, when TryGetLiteral returned true.
    let literalJson (site: Tests.CallCheck.Site) (index: int) : string option =
        match site.LiteralAt index with
        | Tests.CallCheck.Returned (true, v) when not (isNull (box v)) -> Some (Values.toJson v)
        | Tests.CallCheck.Returned (true, _) -> Some "<a .NET null>"
        | _ -> None
    let returnedFalse (site: Tests.CallCheck.Site) (index: int) : bool =
        match site.LiteralAt index with
        | Tests.CallCheck.Returned (false, _) -> true
        | _ -> false
    /// Assert the diagnostic `d` is at line/column/length (130.1.2: the range of the call).
    let at (line: int) (column: int) (length: int) (d: DiagnosticMessage) =
        Assert.True(d.Location.HasValue, sprintf "%A has no location" d.Code)
        let l = d.Location.Value
        Assert.True((l.Line, l.Column, l.Length) = (line, column, length),
                    sprintf "%A: expected %d/%d/%d, got %d/%d/%d" d.Code line column length l.Line l.Column l.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 IJyroCallCheck, JyroCallSite and JyroFunctionBase.CheckCall have the members the clause gives`` () =
    let icc =
        match Tests.CallCheck.callCheckInterface () with
        | Some t -> t
        | None -> Pending.notInBuild "JYRO-REQ-2119" "interface RealTree.Jyro.IJyroCallCheck"
    let site =
        match Tests.CallCheck.callSiteType () with
        | Some t -> t
        | None -> Pending.notInBuild "JYRO-REQ-2119" "class RealTree.Jyro.JyroCallSite"
    // IJyroCallCheck: a public interface whose one member is void CheckCall(JyroCallSite).
    Assert.True(icc.IsInterface && icc.IsPublic, "IJyroCallCheck is a public interface")
    Assert.Equal("RealTree.Jyro", icc.Namespace)
    let members = icc.GetMembers() |> List.ofArray
    Assert.Equal(1, members.Length)
    let cc = icc.GetMethod("CheckCall")
    Assert.NotNull(cc)
    Assert.Equal(typeof<Void>, cc.ReturnType)
    Assert.Equal<Type list>([ site ], cc.GetParameters() |> Array.map (fun p -> p.ParameterType) |> List.ofArray)
    // JyroCallSite: public sealed, no public constructor, and exactly the four public members
    // beside those of System.Object.
    Assert.True(site.IsClass && site.IsPublic && site.IsSealed, "JyroCallSite is a public sealed class")
    Assert.Equal("RealTree.Jyro", site.Namespace)
    Assert.Empty(site.GetConstructors(C1677.flags))
    let own =
        site.GetMembers(System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Instance ||| System.Reflection.BindingFlags.Static)
        |> Array.filter (fun m ->
            match m with
            | :? System.Reflection.MethodInfo as mi -> not mi.IsSpecialName && mi.GetBaseDefinition().DeclaringType <> typeof<obj>
            | _ -> m.DeclaringType <> typeof<obj>)
        |> Array.map (fun m -> m.Name)
        |> Array.sort
        |> List.ofArray
    Assert.Equal<string list>([ "ArgumentCount"; "ArgumentNames"; "Name"; "TryGetLiteral" ], own)
    let p (n: string) = site.GetProperty(n, C1677.flags)
    Assert.Equal(typeof<string>, (p "Name").PropertyType)
    Assert.Equal(typeof<int>, (p "ArgumentCount").PropertyType)
    Assert.Equal(typeof<System.Collections.Generic.IReadOnlyList<string>>, (p "ArgumentNames").PropertyType)
    for n in [ "Name"; "ArgumentCount"; "ArgumentNames" ] do
        Assert.True((p n).CanRead && isNull ((p n).GetSetMethod()), sprintf "JyroCallSite.%s is get-only" n)
    let tgl = site.GetMethod("TryGetLiteral", C1677.flags)
    Assert.NotNull(tgl)
    Assert.Equal(typeof<bool>, tgl.ReturnType)
    let ps = tgl.GetParameters()
    Assert.Equal(2, ps.Length)
    Assert.Equal(typeof<int>, ps.[0].ParameterType)
    Assert.True(ps.[1].IsOut && ps.[1].ParameterType = typeof<JyroValue>.MakeByRefType(), "TryGetLiteral(int index, out JyroValue value)")
    // JyroFunctionBase implements IJyroFunction and IJyroCallCheck; its CheckCall is public and virtual.
    let fb = typeof<JyroFunctionBase>
    Assert.True(typeof<IJyroFunction>.IsAssignableFrom fb, "JyroFunctionBase implements IJyroFunction")
    Assert.True(icc.IsAssignableFrom fb, "JyroFunctionBase implements IJyroCallCheck")
    let fbcc = fb.GetMethod("CheckCall", C1677.flags, null, [| site |], null)
    Assert.True(not (isNull fbcc) && fbcc.IsPublic && fbcc.IsVirtual && not fbcc.IsFinal, "JyroFunctionBase.CheckCall(JyroCallSite) is public virtual")
    Assert.Equal(typeof<Void>, fbcc.ReturnType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 CheckCall is called once for each call in the script and in each library script`` () =
    let q = Tests.CallCheck.query ()
    let lib = "func L()\n  return Query(\"customers\")\nend"
    let src = "Output.a = Query(\"customers\")\nOutput.b = Query(\"customers\", 1)\nvar x = \"customers\"\nOutput.c = Query(x)\nOutput.d = L()"
    let r = C1677.compileLibs [ q ] [ lib ] src
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(4, q.CheckCount)
    Assert.True(q.Sites |> List.forall (fun s -> s.Name = "Query"), sprintf "%A" q.Sites)
    // Also on CompileToArtifact, for the script's three calls (a library cannot be written, 150.9.7).
    let q2 = Tests.CallCheck.query ()
    let b = Script.compileBytesWith (C1677.withFns [ q2 ]) (src.Replace("\nOutput.d = L()", ""))
    Assert.True(b.IsSuccess, Script.describe b)
    Assert.Equal(3, q2.CheckCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 a compilation whose texts do not call the function never calls CheckCall`` () =
    let q = Tests.CallCheck.query ()
    let r = C1677.compileLibs [ q ] [ "func L()\n  return 1\nend" ] "Output.r = 1"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(0, q.CheckCount)
    let b = Script.compileBytesWith (C1677.withFns [ q ]) "Output.r = 1"
    Assert.True(b.IsSuccess, Script.describe b)
    Assert.Equal(0, q.CheckCount)

/// The checks of Link that a call must pass before its CheckCall (the clause's list), each
/// with a call of Query that fails it, beside a call of Query that passes.
let linkFailures : obj[] seq =
    [ "UndefinedFunction", "Output.f = Qeury(\"customers\")"
      "TooFewArguments", "Output.f = Query()"
      "TooManyArguments", "Output.f = Query(\"customers\", 1, 2)"
      "DuplicateNamedArgument", "Output.f = Query(name: \"customers\", name: \"customers\")"
      "UnknownNamedArgument", "Output.f = Query(name: \"customers\", nope: 1)"
      "MissingRequiredNamedArgument", "Output.f = Query(opt: 1)" ]
    |> Seq.map (fun (c, s) -> [| box c; box s |])

[<Theory>]
[<MemberData(nameof linkFailures)>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 a call that fails a check of Link is reported and not given to CheckCall, while a passing call is`` (codeName: string, failing: string) =
    let q = Tests.CallCheck.query ()
    let r = C1677.compile [ q ] ("Output.p = Query(\"customers\")\n" + failing)
    let code = System.Enum.Parse(typeof<MessageCode>, codeName) :?> MessageCode
    Assert.False(r.IsSuccess, Script.describe r)
    if not (List.contains code (Script.errorCodes r)) then
        Values.fail (sprintf "No %s. Diagnostics:\n%s" codeName (Script.describe r))
    // Exactly the passing call was checked: its one argument, the string literal "customers".
    Assert.Equal(1, q.CheckCount)
    let s = q.Sites.Head
    Assert.Equal(1, s.ArgumentCount)
    Assert.Equal(Some "customers", s.StringLiteral 0)
    Assert.Empty(C1677.hostLinkErrors r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 a call refused by FunctionEffectExceedsMax is not given to CheckCall`` () =
    let q = Tests.CallCheck.queryWithEffect FunctionEffect.NonDeterministic
    let other = Tests.CallCheck.accepting "Other"
    let r = Script.compileWith { C1677.withFns [ q; other ] with MaxEffect = Some FunctionEffect.Pure } "Output.p = Other(1)\nOutput.f = Query(\"custmers\")"
    Assert.False(r.IsSuccess, Script.describe r)
    if not (List.contains MessageCode.FunctionEffectExceedsMax (Script.errorCodes r)) then
        Values.fail (sprintf "No FunctionEffectExceedsMax. Diagnostics:\n%s" (Script.describe r))
    Assert.Equal(0, q.CheckCount)
    Assert.Equal(1, other.CheckCount)
    Assert.Empty(C1677.hostLinkErrors r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 the call site gives the name, the count and the names of the arguments as written`` () =
    let p = Tests.CallCheck.accepting "Probe"
    let src =
        String.concat "\n"
            [ "var x = 1"
              "Output.a = Probe()"
              "Output.b = Probe(1, \"s\", x)"
              "Output.c = Probe(c: 1, a: 2, e: 3)"
              "Output.d = Probe(b: x)" ]
    let r = C1677.compile [ p ] src
    Assert.True(r.IsSuccess, Script.describe r)
    let sites = p.Sites |> List.sortBy (fun s -> s.ArgumentCount, s.ArgumentNames)
    Assert.Equal(4, sites.Length)
    for s in sites do Assert.Equal("Probe", s.Name)
    let shape = p.Sites |> List.map (fun s -> s.ArgumentCount, s.ArgumentNames) |> List.sort
    Assert.Equal<(int * string list option) list>(
        [ 0, Some []; 1, Some [ "b" ]; 3, Some []; 3, Some [ "c"; "a"; "e" ] ] |> List.sort,
        shape)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 TryGetLiteral gives each literal argument by its written index, and false for any other`` () =
    let p = Tests.CallCheck.accepting "Probe"
    let other = Host.recorder "Other"
    let src =
        String.concat "\n"
            [ "var x = 7"
              "Output.a = Probe(42, \"s\", true, false, null)"
              "Output.b = Probe(e: null, c: \"t\", a: 2.5)"
              "Output.c = Probe(x, Other(), 1)" ]
    let r = C1677.compile [ p; other.AsFunction ] src
    Assert.True(r.IsSuccess, Script.describe r)
    let byCount n names = p.Sites |> List.find (fun s -> s.ArgumentCount = n && s.ArgumentNames = Some names)
    // Positional literals: number, string, true, false and null (20.1.1).
    let a = byCount 5 []
    Assert.Equal<string option list>([ Some "42"; Some "\"s\""; Some "true"; Some "false"; Some "null" ], [ for i in 0 .. 4 -> C1677.literalJson a i ])
    // Named literals, counted in the order written.
    let b = byCount 3 [ "e"; "c"; "a" ]
    Assert.Equal<string option list>([ Some "null"; Some "\"t\""; Some "2.5" ], [ for i in 0 .. 2 -> C1677.literalJson b i ])
    // A variable and a call are not literals; the literal after them is.
    let c = byCount 3 []
    Assert.True(C1677.returnedFalse c 0, sprintf "a variable: %A" (c.LiteralAt 0))
    Assert.True(C1677.returnedFalse c 1, sprintf "a call: %A" (c.LiteralAt 1))
    Assert.Equal(Some "1", C1677.literalJson c 2)
    // An index that is not the index of an argument: -1 and ArgumentCount.
    for s in [ a; b; c ] do
        Assert.True(C1677.returnedFalse s -1, sprintf "index -1 of %A: %A" s.ArgumentNames (s.LiteralAt -1))
        Assert.True(C1677.returnedFalse s s.ArgumentCount, sprintf "index %d: %A" s.ArgumentCount (s.LiteralAt s.ArgumentCount))
    let empty = Tests.CallCheck.accepting "Empty"
    Assert.True((C1677.compile [ empty ] "Output.e = Empty()").IsSuccess)
    let e = empty.Sites |> List.exactlyOne
    Assert.True(C1677.returnedFalse e 0, sprintf "index 0 of a call with no argument: %A" (e.LiteralAt 0))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 the clause's example Query custmers fails with one HostLinkError at the call, reading TableUnknown and custmers`` () =
    let q = Tests.CallCheck.query ()
    let r = C1677.compile [ q ] "Output.r = Query(\"custmers\")"
    Assert.False(r.IsSuccess, Script.describe r)
    match C1677.hostLinkErrors r with
    | [ d ] ->
        Assert.Equal(4001, int d.Code)
        Assert.Equal(MessageSeverity.Error, d.Severity)
        Assert.Equal<string list>([ "TableUnknown"; "custmers" ], Tests.CallCheck.argsOf d)
        // The range of the call Query("custmers"): line 1, column 12, 17 code units.
        C1677.at 1 12 17 d
    | ds -> Values.fail (sprintf "expected exactly one HostLinkError (4001), got %d. Diagnostics:\n%s" ds.Length (Script.describe r))
    Assert.DoesNotContain(MessageCode.HostFunctionError, Script.codes r)
    Assert.False(q.ReturnedFromRaise, "a refusal returned inside CheckCall")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 each refused call is reported by its own HostLinkError at its own place`` () =
    let q = Tests.CallCheck.query ()
    let r = C1677.compile [ q ] "Output.a = Query(\"custmers\")\nOutput.b = 1\n  Output.c = Query(\"ordrs\", 2)"
    Assert.False(r.IsSuccess, Script.describe r)
    let ds = C1677.hostLinkErrors r |> List.sortBy (fun d -> d.Location.Value.Line)
    Assert.Equal(2, ds.Length)
    Assert.Equal<string list>([ "TableUnknown"; "custmers" ], Tests.CallCheck.argsOf ds.[0])
    Assert.Equal<string list>([ "TableUnknown"; "ordrs" ], Tests.CallCheck.argsOf ds.[1])
    C1677.at 1 12 17 ds.[0]
    C1677.at 3 14 17 ds.[1]
    // The clause's example: the same call twice gives two diagnostics.
    let twice = C1677.compile [ Tests.CallCheck.query () ] "Output.a = Query(\"custmers\")\nOutput.b = Query(\"custmers\")"
    Assert.False(twice.IsSuccess)
    Assert.Equal(2, (C1677.hostLinkErrors twice).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 refusals by two different checking functions are both reported, and a refusal in a library too`` () =
    let q = Tests.CallCheck.query ()
    let v = Tests.CallCheck.checking "Verify" (fun fn _ -> fn.Refuse("NotAllowed", None))
    let r = C1677.compileLibs [ q; v ] [ "func L()\n  return Query(\"nope\")\nend" ] "Output.a = Query(\"custmers\")\nOutput.b = Verify()\nOutput.c = L()"
    Assert.False(r.IsSuccess, Script.describe r)
    let args = C1677.hostLinkErrors r |> List.map Tests.CallCheck.argsOf |> List.sort
    Assert.Equal<string list list>([ [ "NotAllowed" ]; [ "TableUnknown"; "custmers" ]; [ "TableUnknown"; "nope" ] ], args)

let refusalForms : obj[] seq =
    [ for m in [ "Host"; "HostFromBase" ] do
        for v in [ "omitted"; "empty"; "seven-and-x" ] -> [| box m; box v |] ]

[<Theory>]
[<MemberData(nameof refusalForms)>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 the arguments of HostLinkError are the reason then each value, in order, and no other`` (memberName: string, values: string) =
    let vs : obj[] option =
        match values with
        | "omitted" -> None
        | "empty" -> Some [||]
        | _ -> Some [| box 7; box "x" |]
    let returned = ref false
    let f =
        Tests.CallCheck.checking "Guard" (fun fn _ ->
            if memberName = "HostFromBase" then fn.Refuse("Why", vs)
            else
                (match vs with
                 | None -> raise (JyroError.Host("Why"))
                 | Some a -> raise (JyroError.Host("Why", a))) |> ignore
                returned.Value <- true)
    let r = C1677.compile [ f ] "Output.g = Guard()"
    Assert.False(r.IsSuccess, Script.describe r)
    let d = match C1677.hostLinkErrors r with [ d ] -> d | ds -> Values.fail (sprintf "expected one HostLinkError, got %d. %s" ds.Length (Script.describe r))
    Assert.Equal(MessageSeverity.Error, d.Severity)
    let expected = if values = "seven-and-x" then [ "Why"; "7"; "x" ] else [ "Why" ]
    Assert.Equal<string list>(expected, Tests.CallCheck.argsOf d)
    Assert.DoesNotContain(MessageCode.HostFunctionError, Script.codes r)
    // A refusal does not return (the clause's 'MUST NOT return').
    Assert.False(returned.Value || f.ReturnedFromRaise, sprintf "%s returned inside CheckCall" memberName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 a CheckCall that returns, the default CheckCall, and a function without the check all accept the call`` () =
    // A CheckCall that returns accepts the call; the script compiles and runs the function.
    let ok = Tests.CallCheck.accepting "Fine"
    let r1 = Script.runWith (C1677.withFns [ ok ]) "Output.a = Fine(1)"
    Assert.True(r1.IsSuccess, Script.describe r1)
    Assert.Equal(1, ok.CheckCount)
    Assert.Equal(1, ok.RunCount)
    // A JyroFunctionBase that does not override CheckCall: the default accepts every call.
    let plain = Host.recorder "Plain"
    let r2 = Script.runWith (C1677.withFns [ plain.AsFunction ]) "Output.a = Plain(\"custmers\")\nOutput.b = Plain()"
    Assert.True(r2.IsSuccess, Script.describe r2)
    Assert.Equal(2, plain.CallCount)
    // A host function that implements IJyroFunction but not IJyroCallCheck.
    let bare =
        { new IJyroFunction with
            member _.Name = "Bare"
            member _.Signature = Host.signature "Bare" [ Parameter.Optional("a", ParameterType.Any) ] ParameterType.Any
            member _.Effect = FunctionEffect.Pure
            member _.Execute(_, _) = Values.str "ran" }
    let r3 = Script.runWith (C1677.withFns [ bare ]) "Output.a = Bare(\"custmers\")"
    Assert.True(r3.IsSuccess, Script.describe r3)
    Assert.Equal("ran", Values.asString (Values.prop "a" (Script.valueOf r3)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 the default CheckCall of JyroFunctionBase returns for every call`` () =
    // A JyroCallSite has no public constructor, so the sites come from a compilation: inside
    // the check of `Grab`, the default CheckCall of another JyroFunctionBase (which does not
    // override it) is called on the same site, and must return.
    let siteType = match Tests.CallCheck.callSiteType () with Some t -> t | None -> Pending.notInBuild "JYRO-REQ-2119" "class RealTree.Jyro.JyroCallSite"
    let cc = typeof<JyroFunctionBase>.GetMethod("CheckCall", C1677.flags, null, [| siteType |], null)
    if isNull cc then Pending.notInBuild "JYRO-REQ-2119" "JyroFunctionBase.CheckCall(JyroCallSite)"
    let plain = Host.recorder "Plain"
    let outcomes = ResizeArray<string>()
    let grab =
        Tests.CallCheck.make "Grab" Tests.CallCheck.fiveOptional FunctionEffect.Pure (fun _ _ site ->
            try
                cc.Invoke(plain, [| site |]) |> ignore
                outcomes.Add "returned"
            with e -> outcomes.Add (sprintf "threw %s: %s" (e.GetBaseException().GetType().FullName) (e.GetBaseException().Message)))
    let r = C1677.compile [ grab ] "Output.a = Grab(\"custmers\")\nOutput.b = Grab(a: 1, b: null)\nOutput.c = Grab()"
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal<string list>([ "returned"; "returned"; "returned" ], List.ofSeq outcomes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
let ``JYRO-REQ-2119 a load checks each call as a compilation from source does`` () =
    // Written by a compiler given a Query without the check, loaded by one given Query with it.
    let unchecked = Host.func "Query" Tests.CallCheck.queryParameters ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)
    let bytes = Script.compileBytesWith (C1677.withFns [ unchecked.AsFunction ]) "Output.r = Query(\"custmers\")" |> Script.valueOf
    let q = Tests.CallCheck.query ()
    let loaded = Script.loadBytesWith (C1677.withFns [ q ]) bytes
    Assert.False(loaded.IsSuccess, Script.describe loaded)
    match C1677.hostLinkErrors loaded with
    | [ d ] ->
        Assert.Equal(MessageSeverity.Error, d.Severity)
        Assert.Equal<string list>([ "TableUnknown"; "custmers" ], Tests.CallCheck.argsOf d)
    | ds -> Values.fail (sprintf "expected exactly one HostLinkError on the load, got %d. Diagnostics:\n%s" ds.Length (Script.describe loaded))
    // On the run path too: the program does not run.
    let q2 = Tests.CallCheck.query ()
    let ran = Script.runBytesWith (C1677.withFns [ q2 ]) bytes
    Assert.False(ran.IsSuccess, Script.describe ran)
    Assert.Equal(1, (C1677.hostLinkErrors ran).Length)
    Assert.Equal(0, q2.RunCount)
    // A clean artifact loaded with the checking Query: one CheckCall per call.
    let clean = Script.compileBytesWith (C1677.withFns [ unchecked.AsFunction ]) "Output.a = Query(\"customers\")\nOutput.b = Query(\"customers\", 1)" |> Script.valueOf
    let q3 = Tests.CallCheck.query ()
    let ok = Script.loadBytesWith (C1677.withFns [ q3 ]) clean
    Assert.True(ok.IsSuccess, Script.describe ok)
    Assert.Equal(2, q3.CheckCount)
    Assert.Equal<int list>([ 1; 2 ], q3.Sites |> List.map (fun s -> s.ArgumentCount) |> List.sort)

// -- JYRO-DEF-555: a run started inside a CheckCall is a run, not a call check ------------------
//
// JYRO-REQ-2119 gives HostLinkError to a throw of JyroError.Host inside CheckCall as the refusal of
// the call being checked; JYRO-REQ-1891 gives HostFunctionError to them during a run. A run that
// a check starts on its own thread is a run: its host function's refusal is HostFunctionError at
// Execute, in that run's result. The check's own refusal after it is still HostLinkError at Link.

/// A host function `Refuse` that refuses every run by throwing JyroError.Host (JYRO-REQ-1891).
type private InnerRefuser() =
    inherit JyroFunctionBase("Refuse", Host.signature "Refuse" [] ParameterType.Any)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(_, _) = raise (JyroError.Host("Inner", [| box 1 |]))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2119")>]
[<Trait("requirement", "JYRO-REQ-1891")>]
let ``JYRO-REQ-2119 JYRO-DEF-555 a run started inside a CheckCall gives HostFunctionError, and the check's own refusal after it HostLinkError`` () =
    let byHost = Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host("Inner", [| box 1 |])))
    let inners = ResizeArray<string * JyroResult<JyroValue>>()
    let guard =
        Tests.CallCheck.checking "Guard" (fun fn _ ->
            for (label, f) in [ "throw in a lambda", byHost.AsFunction; "throw in a class", InnerRefuser() :> IJyroFunction ] do
                inners.Add(label, Script.runWith (C1677.withFns [ f ]) "Output.i = Refuse()")
            fn.Refuse("Outer", Some [| box 2 |]))
    let r = C1677.compile [ guard ] "Output.g = Guard()"
    // The two runs inside the check.
    Assert.Equal(2, inners.Count)
    for (label, inner) in inners do
        Assert.False(inner.IsSuccess, sprintf "%s: the inner run succeeded. %s" label (Script.describe inner))
        Assert.True((C1677.hostLinkErrors inner).IsEmpty, sprintf "%s in a run inside a CheckCall gave HostLinkError. Diagnostics:\n%s" label (Script.describe inner))
        match inner.Messages |> Seq.filter (fun m -> m.Code = MessageCode.HostFunctionError) |> List.ofSeq with
        | [ d ] ->
            Assert.Equal<string list>([ "Inner"; "1" ], Tests.CallCheck.argsOf d)
            Assert.Equal("Execute", Pending.stageOf d)
        | ds -> Values.fail (sprintf "%s: expected one HostFunctionError in the inner run, got %d. Diagnostics:\n%s" label ds.Length (Script.describe inner))
    // The check's own refusal, after the runs.
    Assert.False(r.IsSuccess, Script.describe r)
    match C1677.hostLinkErrors r with
    | [ d ] ->
        Assert.Equal<string list>([ "Outer"; "2" ], Tests.CallCheck.argsOf d)
        Assert.Equal("Link", Pending.stageOf d)
    | ds -> Values.fail (sprintf "expected one HostLinkError for the check's refusal, got %d. Diagnostics:\n%s" ds.Length (Script.describe r))
    Assert.DoesNotContain(MessageCode.HostFunctionError, Script.codes r)
    Assert.False(guard.ReturnedFromRaise, "a refusal returned inside CheckCall")
    // And a run on this thread after the compilation is a run.
    let after = Script.runWith (C1677.withFns [ byHost.AsFunction ]) "Output.i = Refuse()"
    Assert.Equal<MessageCode list>([ MessageCode.HostFunctionError ], Script.errorCodes after)

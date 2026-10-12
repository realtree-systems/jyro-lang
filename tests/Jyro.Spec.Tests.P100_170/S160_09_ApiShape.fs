/// The shape of the .NET host API after JYRO-ADR-191, 198, 199 and 201 (160.7): the members of
/// JyroExecutionContext (JYRO-REQ-1873), JyroScript and no extension method that runs a script
/// (1878), JyroException (1885, 1886), JyroResult<T> and JyroExecutionResult (1887), JyroArtifactLoader
/// and JyroCompiler (1896), JyroPipelineStats (1927), sealed classes (1928), nullable annotations
/// (1931) and the names reserved for asynchronous execution (1934). Written from the specification
/// alone (FLOORS 4.1.1). The behavior of the same members is in S160_09_HostBehavior.fs.
module RealTree.Jyro.Spec.Tests.S160_09_ApiShape

open System
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.S160_09_Reflect

let private asStrings (xs: string list) : string list = xs |> List.sort

/// `RealTree.Jyro.JyroScript`: an F# module is not a type name in F#, so it is found by name.
let private jyroScriptType : Type = typeof<JyroRunner>.Assembly.GetType("RealTree.Jyro.JyroScript", true)

// -- JYRO-REQ-1896: JyroCompiler compiles, JyroArtifactLoader loads ----------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 JyroCompiler has no member named WithCompiledBytes or CompileToBytes`` () =
    let names = publicMemberNames typeof<JyroCompiler>
    Assert.DoesNotContain<string>("WithCompiledBytes", names)
    Assert.DoesNotContain<string>("CompileToBytes", names)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 JyroCompiler has public JyroResult of byte array CompileToArtifact with no parameter`` () =
    let m = methodOf typeof<JyroCompiler> "CompileToArtifact" []
    Assert.Equal(typeof<JyroResult<byte[]>>, m.ReturnType)
    Assert.False(m.IsStatic)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 JyroArtifactLoader is a public sealed class in RealTree.Jyro`` () =
    let t = typeof<JyroArtifactLoader>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed && not t.IsAbstract)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1896")>]
let ``JYRO-REQ-1896 JyroArtifactLoader declares exactly the eight public members of the clause`` () =
    let expected =
        asStrings
            [ ".ctor(Byte[])"
              "AddFunction(IJyroFunction) : JyroArtifactLoader"
              "AddFunctions(IEnumerable<IJyroFunction>) : JyroArtifactLoader"
              "Load() : JyroResult<CompiledProgram>"
              "WithMaxCompilationTime(TimeSpan) : JyroArtifactLoader"
              "WithMaxEffect(FunctionEffect) : JyroArtifactLoader"
              "WithStats(JyroPipelineStats) : JyroArtifactLoader"
              "WithStdlib() : JyroArtifactLoader" ]
    Assert.Equal<string list>(expected, declaredPublic typeof<JyroArtifactLoader>)

// -- JYRO-REQ-1873: JyroExecutionContext shows a host function only what it may use ----------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 JyroExecutionContext is a sealed class in RealTree.Jyro`` () =
    let t = typeof<JyroExecutionContext>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed && not t.IsAbstract)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 JyroExecutionContext declares exactly the nine public members of the clause`` () =
    let expected =
        asStrings
            [ ".ctor()"
              "CallDepth : Int32 { get }"
              "CancellationToken : CancellationToken { get }"
              "CheckExecutionLimits() : Void"
              "HostCancellationRequested : Boolean { get }"
              "HostState : Object { get }"
              "Input : JyroValue { get }"
              "Output : JyroValue { get }"
              "PeakCallDepth : Int32 { get }" ]
    Assert.Equal<string list>(expected, declaredPublic typeof<JyroExecutionContext>)

[<Theory>]
[<InlineData("CheckAndCountStatement")>]
[<InlineData("CheckAndEnterLoop")>]
[<InlineData("CheckAndEnterCall")>]
[<InlineData("ExitCall")>]
[<InlineData("EnterCall")>]
[<InlineData("OnFunctionResult")>]
[<InlineData("SetReturnMessage")>]
[<InlineData("ReturnMessage")>]
[<InlineData("FunctionResultObserver")>]
[<InlineData("Limiter")>]
[<InlineData("HasLimiter")>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 JyroExecutionContext has no public member of the name`` (name: string) =
    Assert.DoesNotContain<string>(name, publicMemberNames typeof<JyroExecutionContext>)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1873")>]
let ``JYRO-REQ-1873 JyroExecutionContext has no public constructor that takes a parameter and no public setter of Input or Output`` () =
    for c in typeof<JyroExecutionContext>.GetConstructors() do
        Assert.Empty(c.GetParameters())
    for name in [ "Input"; "Output"; "HostState"; "CallDepth"; "PeakCallDepth" ] do
        let p = typeof<JyroExecutionContext>.GetProperty(name)
        Assert.NotNull(p)
        Assert.Null(p.GetSetMethod())

// -- JYRO-REQ-1878: the one-call members are those of JyroScript -----------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1878")>]
let ``JYRO-REQ-1878 JyroScript is a static class with exactly ExecuteScript and ExecuteScriptWithJson`` () =
    let t = jyroScriptType
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsAbstract && t.IsSealed, "JyroScript is a static class")
    let expected =
        asStrings
            [ "static ExecuteScript(String, Object) : JyroExecutionResult"
              "static ExecuteScriptWithJson(String, String) : JyroExecutionResult" ]
    Assert.Equal<string list>(expected, declaredPublic t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1878")>]
let ``JYRO-REQ-1878 the shipped assemblies have no public type named JyroExtensions`` () =
    Assert.True((tryExported "JyroExtensions").IsNone, "JyroExtensions is still a public type")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1878")>]
let ``JYRO-REQ-1878 no public extension method compiles or runs a script`` () =
    let found =
        [ for t in exportedTypes do
            for m in t.GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly) do
                if m.IsDefined(typeof<ExtensionAttribute>, false) then
                    yield sprintf "%s.%s" t.FullName m.Name ]
    // The JyroArguments readers (JYRO-ADR-192) are public extension methods: they read an argument and
    // run or compile no script. Judge a method by its own name, not by the namespace "RealTree.Jyro".
    let scripty =
        found
        |> List.filter (fun n ->
            let name = n.Substring(n.LastIndexOf '.' + 1)
            name.Contains "Execute" || name.Contains "Compile" || name.Contains "Run" || name.Contains "Script")
    Assert.True(List.isEmpty scripty, sprintf "Public extension methods that run or compile a script: %A" scripty)

// -- JYRO-REQ-1885 and 1886: JyroException and JyroRuntimeException ----------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 JyroException is a class in RealTree.Jyro that derives directly from System.Exception`` () =
    let t = typeof<JyroException>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsPublic)
    Assert.Equal(typeof<Exception>, t.BaseType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 JyroException has no constructor that is public or protected`` () =
    let reachable = reachableConstructors typeof<JyroException> |> List.map string
    Assert.True(List.isEmpty reachable, sprintf "JyroException has reachable constructors: %A" reachable)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 JyroException has the public properties Messages, Stage and Location`` () =
    let declared =
        typeof<JyroException>.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
        |> Array.filter (fun p -> p.Name <> "Message") // an override of System.Exception.Message is not a new member
        |> Array.map signature
        |> Array.sort
        |> List.ofArray
    let expected =
        asStrings
            [ "Location : Nullable<SourceLocation> { get }"
              "Messages : IReadOnlyList<DiagnosticMessage> { get }"
              "Stage : MessageStage { get }" ]
    Assert.Equal<string list>(expected, declared)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1885")>]
let ``JYRO-REQ-1885 the shipped assemblies have no public type named JyroExecutionException`` () =
    Assert.True((tryExported "JyroExecutionException").IsNone, "JyroExecutionException is still a public type")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1886")>]
let ``JYRO-REQ-1886 JyroRuntimeException is a sealed class that derives from JyroException`` () =
    let t = typeof<JyroRuntimeException>
    Assert.True(t.IsSealed, "JyroRuntimeException is not sealed")
    Assert.Equal(typeof<JyroException>, t.BaseType)

[<Theory>]
[<InlineData("Line")>]
[<InlineData("Column")>]
[<InlineData("HasLocation")>]
[<Trait("requirement", "JYRO-REQ-1886")>]
let ``JYRO-REQ-1886 JyroRuntimeException declares no public member of the name`` (name: string) =
    Assert.DoesNotContain<string>(name, publicMemberNames typeof<JyroRuntimeException>)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1886")>]
let ``JYRO-REQ-1886 no exported type of the five assemblies is named JyroScriptExit`` () =
    Assert.True((tryExported "JyroScriptExit").IsNone, "JyroScriptExit is still a public type")

// -- JYRO-REQ-1887: JyroResult<T> and JyroExecutionResult --------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 JyroResult has no public or protected constructor and no ExitMessage`` () =
    let t = typedefof<JyroResult<_>>
    let reachable = reachableConstructors t |> List.map string
    Assert.True(List.isEmpty reachable, sprintf "JyroResult has reachable constructors: %A" reachable)
    Assert.DoesNotContain<string>("ExitMessage", publicMemberNames t)
    Assert.DoesNotContain<string>("ExitMessage", publicMemberNames typeof<JyroResult<byte[]>>)
    Assert.DoesNotContain<string>("ExitMessage", publicMemberNames typeof<JyroResult<CompiledProgram>>)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 JyroResult has the public method GetValueOrThrow that returns T`` () =
    let t = typedefof<JyroResult<_>>
    let m = t.GetMethod("GetValueOrThrow", BindingFlags.Public ||| BindingFlags.Instance, null, Type.EmptyTypes, null)
    Assert.NotNull(m)
    Assert.True(m.ReturnType.IsGenericParameter, "GetValueOrThrow does not return T")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 JyroExecutionResult is a sealed class that derives from JyroResult of JyroValue and has ExitMessage`` () =
    let t = typeof<JyroExecutionResult>
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed)
    Assert.Equal(typeof<JyroResult<JyroValue>>, t.BaseType)
    let p = t.GetProperty("ExitMessage", BindingFlags.Public ||| BindingFlags.Instance)
    Assert.NotNull(p)
    Assert.Equal(typeof<string>, p.PropertyType)
    Assert.NotNull(p.GetGetMethod())
    Assert.Null(p.GetSetMethod())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1887")>]
let ``JYRO-REQ-1887 JyroRunner Execute and the members of JyroScript return JyroExecutionResult`` () =
    Assert.Equal(typeof<JyroExecutionResult>, (methodOf typeof<JyroRunner> "Execute" []).ReturnType)
    for m in jyroScriptType.GetMethods(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly) do
        Assert.Equal(typeof<JyroExecutionResult>, m.ReturnType)

// -- JYRO-REQ-1927: JyroPipelineStats is read-only telemetry ---------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 JyroPipelineStats is a sealed class with a public parameterless constructor`` () =
    let t = typeof<JyroPipelineStats>
    Assert.True(t.IsClass && t.IsPublic && t.IsSealed && not t.IsAbstract)
    Assert.NotNull(t.GetConstructor(Type.EmptyTypes))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 JyroPipelineStats has no public static member and no public property with a setter`` () =
    let t = typeof<JyroPipelineStats>
    let statics = t.GetMembers(BindingFlags.Public ||| BindingFlags.Static) |> Array.map (fun m -> m.Name)
    Assert.True(Array.isEmpty statics, sprintf "public static members: %A" statics)
    let settable = t.GetProperties() |> Array.filter (fun p -> not (isNull (p.GetSetMethod()))) |> Array.map (fun p -> p.Name)
    Assert.True(Array.isEmpty settable, sprintf "properties with a public setter: %A" settable)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 JyroPipelineStats declares exactly the eleven public properties of the clause`` () =
    let declared =
        typeof<JyroPipelineStats>.GetProperties(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
        |> Array.map signature
        |> Array.sort
        |> List.ofArray
    let expected =
        asStrings
            [ "CompileTime : TimeSpan { get }"
              "DeserializeTime : TimeSpan { get }"
              "ExecuteTime : TimeSpan { get }"
              "LimiterElapsedTime : TimeSpan { get }"
              "LinkTime : TimeSpan { get }"
              "LoopIterations : Int64 { get }"
              "ParseTime : TimeSpan { get }"
              "PeakCallDepth : Int32 { get }"
              "StatementCount : Int64 { get }"
              "TotalTime : TimeSpan { get }"
              "ValidateTime : TimeSpan { get }" ]
    // Seven times, two counts, PeakCallDepth and LimiterElapsedTime.
    Assert.Equal<string list>(expected, declared)
    Assert.Equal(11, declared.Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 JyroPipelineStats has no public field`` () =
    Assert.Empty(typeof<JyroPipelineStats>.GetFields(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static))

[<Theory>]
[<InlineData("Parse")>]
[<InlineData("Compile")>]
[<InlineData("Execute")>]
[<InlineData("CallDepth")>]
[<InlineData("IsFromJyrx")>]
[<InlineData("TimeStage")>]
[<Trait("requirement", "JYRO-REQ-1927")>]
let ``JYRO-REQ-1927 JyroPipelineStats has no member of the name`` (name: string) =
    Assert.DoesNotContain<string>(name, publicMemberNames typeof<JyroPipelineStats>)

// -- JYRO-REQ-1928: a public class is sealed, static or designed for inheritance --------------------------

let private designedForInheritance : Set<string> =
    set [ "JyroValue"; "JyroFunctionBase"; "JyroException"; "JyroResult`1" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1928")>]
let ``JYRO-REQ-1928 every public class is sealed, static, or one of the four designed for inheritance`` () =
    let offenders =
        exportedClasses
        |> List.filter (fun t -> not t.IsSealed && not (designedForInheritance.Contains t.Name))
        |> List.map (fun t -> t.FullName)
    Assert.True(List.isEmpty offenders, sprintf "Public classes that are neither sealed nor static nor designed for inheritance: %A" offenders)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1928")>]
let ``JYRO-REQ-1928 JyroValue, JyroException and JyroResult have no public or protected constructor`` () =
    for t in [ typeof<JyroValue>; typeof<JyroException>; typedefof<JyroResult<_>> ] do
        let reachable = reachableConstructors t |> List.map string
        Assert.True(List.isEmpty reachable, sprintf "%s has reachable constructors: %A" t.Name reachable)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1928")>]
let ``JYRO-REQ-1928 a public class whose public members are all static is static`` () =
    let offenders =
        exportedClasses
        |> List.filter (fun t -> not t.IsSealed || not t.IsAbstract) // not already static
        |> List.filter (fun t ->
            let instanceMembers =
                t.GetMembers(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun m -> m.MemberType <> MemberTypes.NestedType && m.MemberType <> MemberTypes.TypeInfo)
            // A class with no public member at all (CompiledProgram, made by the pipeline and handed
            // to JyroRunner, JYRO-REQ-1875) is an opaque instance, not a class of static members.
            let staticMembers =
                t.GetMembers(BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun m -> m.MemberType <> MemberTypes.NestedType && m.MemberType <> MemberTypes.TypeInfo)
            instanceMembers.Length = 0 && staticMembers.Length > 0)
        |> List.map (fun t -> t.FullName)
    Assert.True(List.isEmpty offenders, sprintf "Public classes with no public instance member that are not static: %A" offenders)

[<Theory>]
[<InlineData("JyroCompiler")>]
[<InlineData("JyroArtifactLoader")>]
[<InlineData("JyroRunner")>]
[<InlineData("JyroExecutionContext")>]
[<InlineData("JyroRuntimeException")>]
[<Trait("requirement", "JYRO-REQ-1928")>]
let ``JYRO-REQ-1928 the class is sealed`` (name: string) =
    Assert.True((exported name).IsSealed, sprintf "%s is not sealed" name)

[<Theory>]
[<InlineData("JyroScript")>]
[<InlineData("JyroError")>]
[<InlineData("StdlibRegistry")>]
[<Trait("requirement", "JYRO-REQ-1928")>]
let ``JYRO-REQ-1928 the class has no public instance member and is static`` (name: string) =
    let t = exported name
    Assert.True(t.IsAbstract && t.IsSealed, sprintf "%s is not static" name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1928")>]
let ``JYRO-REQ-1928 JyroArguments, where the shipped assemblies have it, is static`` () =
    match tryExported "JyroArguments" with
    | Some t -> Assert.True(t.IsAbstract && t.IsSealed, "JyroArguments is not static")
    | None -> ()

// -- JYRO-REQ-1931: nullable annotations --------------------------------------------------------------------

let private memberKinds =
    BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly

/// The null state of `info` and, below it, of each array element type and generic type argument, at every level:
/// the path ("" for the top, "[]" for an element, "<1>" for the second generic argument) and the state there.
let rec private levels (path: string) (info: NullabilityInfo) : (string * NullabilityInfo) list =
    [ yield (path, info)
      if not (isNull info.ElementType) then yield! levels (path + "[]") info.ElementType
      for i, argument in Array.indexed info.GenericTypeArguments do
          yield! levels (sprintf "%s<%d>" path i) argument ]

/// What a surface entry describes, its null state, and whether a setter or a writable field gives it a write state.
type private Entry = { What: string; Info: NullabilityInfo option; Failure: string option; Writable: bool }

/// Every parameter (constructors, methods, accessors, indexer setters and operators included, overrides included
/// as declared), return value, property (indexers included) and field of a reference type on the public surface.
let private surface () : Entry list =
    let ctx = NullabilityInfoContext()
    let entries = ResizeArray<Entry>()
    let add (what: string) (f: unit -> NullabilityInfo) (writable: bool) =
        try entries.Add { What = what; Info = Some(f ()); Failure = None; Writable = writable }
        with e -> entries.Add { What = what; Info = None; Failure = Some(e.GetType().Name); Writable = writable }
    for t in exportedTypes do
        if not t.IsEnum && not (typeof<Delegate>.IsAssignableFrom t) then
            let owner = t.FullName
            for c in t.GetConstructors(memberKinds) do
                for p in c.GetParameters() do
                    if isAnnotated p.ParameterType then add (sprintf "%s..ctor parameter %s" owner p.Name) (fun () -> ctx.Create p) false
            for m in t.GetMethods(memberKinds) do
                let accessor = m.IsSpecialName && (m.Name.StartsWith "get_" || m.Name.StartsWith "set_")
                if not m.IsSpecialName || m.Name.StartsWith "op_" || accessor then
                    for p in m.GetParameters() do
                        if isAnnotated p.ParameterType then
                            add (sprintf "%s.%s parameter %s" owner m.Name p.Name) (fun () -> ctx.Create p) false
                    if isAnnotated m.ReturnType then
                        add (sprintf "%s.%s return value" owner m.Name) (fun () -> ctx.Create(m.ReturnParameter)) false
            for p in t.GetProperties(memberKinds) do
                if isAnnotated p.PropertyType then
                    let what = if p.GetIndexParameters().Length = 0 then p.Name else p.Name + "[]"
                    add (sprintf "%s.%s" owner what) (fun () -> ctx.Create p) (not (isNull (p.GetSetMethod())))
            for f in t.GetFields(memberKinds) do
                if isAnnotated f.FieldType then add (sprintf "%s.%s" owner f.Name) (fun () -> ctx.Create f) true
    List.ofSeq entries

/// Every place on the public surface where the null state, at any level, reads Unknown.
let private unannotated () : string list =
    [ for entry in surface () do
        match entry.Info, entry.Failure with
        | Some info, _ ->
            for (path, level) in levels "" info do
                if isAnnotated level.Type then
                    if level.ReadState = NullabilityState.Unknown then
                        yield sprintf "%s%s (read state Unknown)" entry.What path
                    elif path = "" && entry.Writable && level.WriteState = NullabilityState.Unknown then
                        yield sprintf "%s (write state Unknown)" entry.What
        | None, failure -> yield sprintf "%s (%s)" entry.What (defaultArg failure "unread") ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 no parameter, return value, property or field of a reference type on the public surface has an Unknown null state at any level`` () =
    let problems = unannotated ()
    Assert.True(
        List.isEmpty problems,
        sprintf "%d public members without nullable annotations; the first 40:\n%s" problems.Length (String.Join("\n", problems |> List.truncate 40)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 every object that is an array element or a generic type argument on the public surface reads Nullable, as the values it holds may be null`` () =
    // Containers of host values hold a null reference for each Jyro null (ToDictionary, ToObjectValue) and take one
    // for each null element (the arguments of a message): obj | null at every level.
    let wrong =
        [ for entry in surface () do
            match entry.Info with
            | Some info ->
                for (path, level) in levels "" info do
                    if path <> "" && level.Type = typeof<obj> && level.ReadState <> NullabilityState.Nullable then
                        yield sprintf "%s%s reads %A" entry.What path level.ReadState
            | None -> () ]
    Assert.True(List.isEmpty wrong, sprintf "%d obj elements or type arguments not Nullable; the first 40:\n%s" wrong.Length (String.Join("\n", wrong |> List.truncate 40)))

[<Theory>]
[<InlineData("JyroArray")>]
[<InlineData("JyroObject")>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the indexer reads NotNull and its setter accepts null: write state and value parameter Nullable`` (name: string) =
    let t = exported name
    let ctx = NullabilityInfoContext()
    let indexers = t.GetProperties(memberKinds) |> Array.filter (fun p -> p.GetIndexParameters().Length = 1 && p.PropertyType = typeof<JyroValue>)
    Assert.NotEmpty indexers
    for indexer in indexers do
        let info = ctx.Create indexer
        Assert.True(info.ReadState = NullabilityState.NotNull, sprintf "%s indexer read state is %A, not NotNull: a read never gives a null reference" name info.ReadState)
        Assert.True(info.WriteState = NullabilityState.Nullable, sprintf "%s indexer write state is %A, not Nullable: its setter stores JyroNull for null" name info.WriteState)
        let setter = indexer.GetSetMethod()
        Assert.NotNull setter
        let parameters = setter.GetParameters()
        let value = ctx.Create(parameters.[parameters.Length - 1])
        // A parameter that accepts null is Nullable (or AllowNull, which this reflection reads as the write state).
        Assert.True(
            value.ReadState = NullabilityState.Nullable || value.WriteState = NullabilityState.Nullable,
            sprintf "%s indexer setter value parameter is read %A, write %A, not Nullable" name value.ReadState value.WriteState)

[<Theory>]
[<InlineData("JyroNull", "Nullable")>]
[<InlineData("JyroBoolean", "NotNull")>]
[<InlineData("JyroNumber", "NotNull")>]
[<InlineData("JyroString", "NotNull")>]
[<InlineData("JyroArray", "NotNull")>]
[<InlineData("JyroObject", "NotNull")>]
[<InlineData("JyroFunction", "NotNull")>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 ToObjectValue of each value type reads Nullable only where it can give null: JyroNull`` (name: string, expected: string) =
    let ctx = NullabilityInfoContext()
    let overrides = (exported name).GetMethods(memberKinds) |> Array.filter (fun m -> m.Name = "ToObjectValue" && m.GetParameters().Length = 0)
    Assert.NotEmpty overrides
    for m in overrides do
        let state = (ctx.Create m.ReturnParameter).ReadState
        Assert.True(string state = expected, sprintf "%s.ToObjectValue return value reads %A, expected %s" name state expected)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the ToObjectValue of JyroValue, which JyroNull answers with null, reads Nullable`` () =
    let ctx = NullabilityInfoContext()
    let m = (exported "JyroValue").GetMethods(memberKinds) |> Array.find (fun m -> m.Name = "ToObjectValue" && m.GetParameters().Length = 0)
    Assert.Equal(NullabilityState.Nullable, (ctx.Create m.ReturnParameter).ReadState)

/// The null state of every parameter, return value, property and field named `memberName` of the public type,
/// with `where` the name of the parameter or "return" or "member".
let private namedInfos (typeName: string) (memberName: string) : (string * NullabilityInfo) list =
    let t = exported typeName
    let ctx = NullabilityInfoContext()
    [ for m in t.GetMethods(memberKinds) do
        if m.Name = memberName then
            for p in m.GetParameters() do
                if isAnnotated p.ParameterType then yield sprintf "%s.%s parameter %s" typeName memberName p.Name, ctx.Create p
            if isAnnotated m.ReturnType then yield sprintf "%s.%s return value" typeName memberName, ctx.Create m.ReturnParameter
      for p in t.GetProperties(memberKinds) do
        if p.Name = memberName && isAnnotated p.PropertyType then yield sprintf "%s.%s" typeName memberName, ctx.Create p
      for f in t.GetFields(memberKinds) do
        if f.Name = memberName && isAnnotated f.FieldType then yield sprintf "%s.%s" typeName memberName, ctx.Create f ]

[<Theory>]
[<InlineData("JyroObject", "ToDictionary")>]
[<InlineData("JyroError", "Host")>]
[<InlineData("JyroError", "Runtime")>]
[<InlineData("JyroRuntimeException", "Args")>]
[<InlineData("MessageTemplates", "Render")>]
[<InlineData("MessageTemplates", "Format")>]
[<InlineData("DiagnosticMessage", "Error")>]
[<InlineData("DiagnosticMessage", "Warning")>]
[<InlineData("DiagnosticMessage", "Info")>]
[<InlineData("DiagnosticMessage", "Args")>]
[<InlineData("StructuredDiagnostic", "Args")>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the element or type argument of obj that a named member takes or gives reads Nullable`` (typeName: string, memberName: string) =
    let nested =
        [ for (what, info) in namedInfos typeName memberName do
            for (path, level) in levels "" info do
                if path <> "" && level.Type = typeof<obj> then yield what, path, level.ReadState ]
    Assert.NotEmpty nested
    let wrong = nested |> List.filter (fun (_, _, state) -> state <> NullabilityState.Nullable)
    Assert.True(List.isEmpty wrong, sprintf "not Nullable: %A" wrong)

[<Theory>]
[<InlineData("JyroError", "Host")>]
[<InlineData("JyroError", "Runtime")>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the array of values that a JyroError factory accepts may be null and reads Nullable`` (typeName: string, memberName: string) =
    let arrays = namedInfos typeName memberName |> List.filter (fun (_, info) -> info.Type = typeof<obj[]>)
    Assert.NotEmpty arrays
    for (what, info) in arrays do
        Assert.True(info.ReadState = NullabilityState.Nullable, sprintf "%s reads %A, not Nullable" what info.ReadState)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 ExitMessage reads as Nullable and DiagnosticMessage Message as NotNull`` () =
    let ctx = NullabilityInfoContext()
    Assert.Equal(NullabilityState.Nullable, (ctx.Create(typeof<JyroExecutionResult>.GetProperty "ExitMessage")).ReadState)
    Assert.Equal(NullabilityState.NotNull, (ctx.Create(typeof<DiagnosticMessage>.GetProperty "Message")).ReadState)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the parameter of JyroRunner WithStats is NotNull and JyroExecutionContext HostState is Nullable`` () =
    let ctx = NullabilityInfoContext()
    let parameter = (methodOf typeof<JyroRunner> "WithStats" [ typeof<JyroPipelineStats> ]).GetParameters().[0]
    Assert.Equal(NullabilityState.NotNull, (ctx.Create parameter).ReadState)
    Assert.Equal(NullabilityState.Nullable, (ctx.Create(typeof<JyroExecutionContext>.GetProperty "HostState")).ReadState)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the parameter of JyroRunner WithHostState accepts null and is Nullable`` () =
    let ctx = NullabilityInfoContext()
    let parameter = (methodOf typeof<JyroRunner> "WithHostState" [ typeof<obj> ]).GetParameters().[0]
    Assert.Equal(NullabilityState.Nullable, (ctx.Create parameter).ReadState)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1931")>]
let ``JYRO-REQ-1931 the out parameter of a Try method carries MaybeNullWhen(false) or NotNullWhen(true)`` () =
    let carries (p: ParameterInfo) =
        p.CustomAttributes
        |> Seq.exists (fun a ->
            let arg = if a.ConstructorArguments.Count = 1 then a.ConstructorArguments.[0].Value else null
            (a.AttributeType.Name = "MaybeNullWhenAttribute" && (arg :? bool) && not (unbox<bool> arg))
            || (a.AttributeType.Name = "NotNullWhenAttribute" && (arg :? bool) && unbox<bool> arg))
    let offenders =
        [ for t in exportedTypes do
            for m in t.GetMethods(memberKinds) do
                if m.Name.StartsWith "Try" then
                    for p in m.GetParameters() do
                        if p.IsOut && isAnnotated p.ParameterType && not (carries p) then
                            yield sprintf "%s.%s parameter %s" t.FullName m.Name p.Name ]
    Assert.True(List.isEmpty offenders, sprintf "Try methods whose out parameter has no MaybeNullWhen(false) or NotNullWhen(true): %A" offenders)

// -- JYRO-REQ-1934: no 1.0 name takes a name that asynchronous execution will need ---------------------------

let private reserved = [ "ExecuteAsync"; "CompileAsync"; "CompileToArtifactAsync"; "LoadAsync"; "ParseAsync" ]

let private isVisible (m: MemberInfo) : bool =
    match m with
    | :? MethodBase as b -> b.IsPublic || b.IsFamily || b.IsFamilyOrAssembly
    | :? PropertyInfo as p ->
        [ p.GetMethod; p.SetMethod ] |> List.exists (fun a -> not (isNull a) && (a.IsPublic || a.IsFamily || a.IsFamilyOrAssembly))
    | :? FieldInfo as f -> f.IsPublic || f.IsFamily || f.IsFamilyOrAssembly
    | :? EventInfo -> true
    | _ -> false

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1934")>]
let ``JYRO-REQ-1934 no public type declares a member named ExecuteAsync, CompileAsync, CompileToArtifactAsync, LoadAsync or ParseAsync`` () =
    let all = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
    let found =
        [ for t in exportedTypes do
            for m in t.GetMembers(all) do
                if List.contains m.Name reserved && isVisible m then yield sprintf "%s.%s" t.FullName m.Name ]
    Assert.True(List.isEmpty found, sprintf "Reserved names declared: %A" found)

[<Theory>]
[<InlineData("JyroRunner")>]
[<InlineData("JyroCompiler")>]
[<InlineData("JyroArtifactLoader")>]
[<InlineData("JyroLibrary")>]
[<InlineData("JyroScript")>]
[<Trait("requirement", "JYRO-REQ-1934")>]
let ``JYRO-REQ-1934 the class has no member named for asynchronous execution`` (name: string) =
    let names = publicMemberNames (exported name)
    for r in reserved do
        Assert.DoesNotContain<string>(r, names)

[<Theory>]
[<InlineData("IJyroAsyncFunction")>]
[<InlineData("JyroAsyncFunctionBase")>]
[<Trait("requirement", "JYRO-REQ-1934")>]
let ``JYRO-REQ-1934 the shipped assemblies have no public type of the name`` (name: string) =
    Assert.True((tryExported name).IsNone, sprintf "%s is a public type" name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1934")>]
let ``JYRO-REQ-1934 every public method whose name ends with Async returns a task type and takes a CancellationToken last`` () =
    let isTaskType (t: Type) =
        t = typeof<Task>
        || t = typeof<ValueTask>
        || (t.IsGenericType
            && (t.GetGenericTypeDefinition() = typedefof<Task<_>> || t.GetGenericTypeDefinition() = typedefof<ValueTask<_>>))
    let all = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
    let asyncMethods =
        [ for t in exportedTypes do
            for m in t.GetMethods(all) do
                if m.Name.EndsWith "Async" then yield t, m ]
    let offenders =
        asyncMethods
        |> List.filter (fun (_, m) ->
            let ps = m.GetParameters()
            not (isTaskType m.ReturnType) || ps.Length = 0 || ps.[ps.Length - 1].ParameterType <> typeof<CancellationToken>)
        |> List.map (fun (t, m) -> sprintf "%s.%s" t.FullName m.Name)
    Assert.True(List.isEmpty offenders, sprintf "Async-suffixed methods that are not awaitable with a token last: %A" offenders)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1934")>]
let ``JYRO-REQ-1934 JyroValue FromJsonAsync returns Task of JyroValue and takes a CancellationToken last`` () =
    let m = staticOf typeof<JyroValue> "FromJsonAsync" [ typeof<IO.Stream>; typeof<CancellationToken> ]
    Assert.Equal(typeof<Task<JyroValue>>, m.ReturnType)
    let ps = m.GetParameters()
    Assert.Equal(typeof<CancellationToken>, ps.[ps.Length - 1].ParameterType)

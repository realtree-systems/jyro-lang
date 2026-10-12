/// The call check of a host function (JYRO-REQ-2119, 160.2): `IJyroCallCheck`,
/// `JyroCallSite` and `JyroFunctionBase.CheckCall`, reached without naming them, so that
/// the suite compiles on a build that does not have them yet (FLOORS 4.1.7). Written from
/// the book alone (FLOORS 4.1.1).
///
/// A checking host function is a `CheckingFunction` whose subclass, made at run time by
/// System.Reflection.Emit, declares `IJyroCallCheck` and a public virtual
/// `CheckCall(JyroCallSite)` that hands the call site to `OnCheckCall`. The method overrides
/// `JyroFunctionBase.CheckCall` where the build declares it virtual (160.2), and implements
/// the interface's member explicitly as well, so the compilation reaches it either way.
/// When the build has no `IJyroCallCheck` or `JyroCallSite`, making a checking function
/// fails the calling test for JYRO-REQ-2119 (`Pending.notInBuild`).
module RealTree.Jyro.Spec.Tests.CallCheck

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// `HostLinkError`, 4001 (160.7.12, JYRO-REQ-1678), by number: the member is not in every build.
let hostLinkError : MessageCode = enum<MessageCode> 4001

/// `MaxEffectNotApplicable`, 4106 (160.7.12, JYRO-REQ-2138), by number: the member is not in every build.
let effectMaxNotApplicable : MessageCode = enum<MessageCode> 4106

/// `UnknownCompilerError`, 6000 (160.7.12).
let unknownCompilerError : MessageCode = enum<MessageCode> 6000

let private apiAssemblies () : Assembly list =
    [ typeof<JyroValue>.Assembly; typeof<IJyroFunction>.Assembly; typeof<JyroFunctionBase>.Assembly
      typeof<JyroCompiler>.Assembly; typeof<JyroRunner>.Assembly ]
    |> List.distinct

let private findType (fullName: string) : Type option =
    apiAssemblies () |> List.tryPick (fun a -> Option.ofObj (a.GetType fullName))

/// The interface `RealTree.Jyro.IJyroCallCheck`, when the build has it.
let callCheckInterface () : Type option = findType "RealTree.Jyro.IJyroCallCheck"

/// The class `RealTree.Jyro.JyroCallSite`, when the build has it.
let callSiteType () : Type option = findType "RealTree.Jyro.JyroCallSite"

/// True when the build has both types of the call check.
let hasCallCheck () : bool = (callCheckInterface ()).IsSome && (callSiteType ()).IsSome

/// What `TryGetLiteral(index, out value)` did: returned, with the value it gave, or threw.
type Literal =
    | Returned of bool * JyroValue
    | Threw of string

/// A copy of what one `CheckCall` was given, read through the public members of
/// `JyroCallSite` while the check ran (160.2).
type Site =
    { Name: string
      ArgumentCount: int
      /// None when `ArgumentNames` was null.
      ArgumentNames: string list option
      /// `TryGetLiteral(i)` for every i from -1 to ArgumentCount, both included.
      Literals: (int * Literal) list }

    /// What `TryGetLiteral(index)` did, read while the check ran.
    member this.LiteralAt(index: int) : Literal =
        match this.Literals |> List.tryFind (fun (i, _) -> i = index) with
        | Some (_, l) -> l
        | None -> Threw (sprintf "index %d was not read" index)

    /// The .NET string of the Jyro string that `TryGetLiteral(index)` gave, if it gave one.
    member this.StringLiteral(index: int) : string option =
        match this.LiteralAt index with
        | Returned (true, (:? JyroString as s)) -> Some s.Value
        | _ -> None

let private flags = BindingFlags.Public ||| BindingFlags.Instance

let private prop (site: obj) (name: string) : obj =
    let p = site.GetType().GetProperty(name, flags)
    if isNull p then Pending.notInBuild "JYRO-REQ-2119" (sprintf "JyroCallSite.%s" name)
    else p.GetValue site

/// `site.TryGetLiteral(index, out value)`, by reflection.
let tryGetLiteral (site: obj) (index: int) : Literal =
    let m = site.GetType().GetMethod("TryGetLiteral", flags, null, [| typeof<int>; typeof<JyroValue>.MakeByRefType() |], null)
    if isNull m then Pending.notInBuild "JYRO-REQ-2119" "JyroCallSite.TryGetLiteral(int, out JyroValue)"
    else
        let args : obj[] = [| box index; null |]
        try
            let ok = m.Invoke(site, args) :?> bool
            Returned (ok, (match args.[1] with :? JyroValue as v -> v | _ -> Unchecked.defaultof<JyroValue>))
        with :? TargetInvocationException as e when not (isNull e.InnerException) ->
            Threw (sprintf "%s: %s" (e.InnerException.GetType().FullName) e.InnerException.Message)

/// Read every public member of the call site `site` (a `JyroCallSite`).
let readSite (site: obj) : Site =
    let count = prop site "ArgumentCount" :?> int
    let names =
        match prop site "ArgumentNames" with
        | null -> None
        | :? IEnumerable<string> as xs -> Some (List.ofSeq xs)
        | other -> Values.fail (sprintf "JyroCallSite.ArgumentNames is a %s" (other.GetType().FullName))
    { Name = prop site "Name" :?> string
      ArgumentCount = count
      ArgumentNames = names
      Literals = [ for i in -1 .. count -> i, tryGetLiteral site i ] }

/// A host function whose call check hands each call site to `check`, after recording it.
/// Make it with `CallCheck.make`, never with its constructor: only the subclass that
/// `make` emits declares `IJyroCallCheck`.
type CheckingFunction
    (name: string,
     signature: JyroFunctionSignature,
     effect: FunctionEffect,
     check: CheckingFunction -> Site -> obj -> unit) =
    inherit JyroFunctionBase(name, signature)

    let sites = ResizeArray<Site>()
    let gate = obj ()
    let mutable runs = 0

    override _.Effect = effect

    override _.Execute(_: IReadOnlyList<JyroValue>, _: JyroExecutionContext) =
        lock gate (fun () -> runs <- runs + 1)
        JyroNull.Instance :> JyroValue

    /// Called by the emitted `CheckCall`: record the site, then run the check.
    member this.OnCheckCall(site: obj) : unit =
        let s = readSite site
        lock gate (fun () -> sites.Add s)
        check this s site

    /// Every call site the check was given, in the order given.
    member _.Sites : Site list = lock gate (fun () -> List.ofSeq sites)

    /// The number of times the check was called.
    member _.CheckCount : int = lock gate (fun () -> sites.Count)

    /// The number of times the function ran.
    member _.RunCount : int = lock gate (fun () -> runs)

    /// Set when a refusal returned to the function (it must not: the refusal is a throw).
    member val ReturnedFromRaise = false with get, set

    /// `throw JyroError.Host(reason)`, or `throw JyroError.Host(reason, values)` (JYRO-REQ-1891), from
    /// inside the function. It never returns.
    member this.Refuse(reason: string, values: obj[] option) : unit =
        match values with
        | None -> raise (JyroError.Host(reason))
        | Some vs -> raise (JyroError.Host(reason, vs))

    member this.AsFunction : IJyroFunction = this :> IJyroFunction

let private emitted : Lazy<Type option> =
    lazy (
        match callCheckInterface (), callSiteType () with
        | Some icc, Some siteT ->
            let ab = AssemblyBuilder.DefineDynamicAssembly(AssemblyName "Jyro.Spec.CallChecks", AssemblyBuilderAccess.Run)
            let mb = ab.DefineDynamicModule "Jyro.Spec.CallChecks"
            let tb = mb.DefineType("CheckingFunctionWithCallCheck", TypeAttributes.Public ||| TypeAttributes.Class, typeof<CheckingFunction>)
            tb.AddInterfaceImplementation icc
            let baseCtor = typeof<CheckingFunction>.GetConstructors() |> Array.exactlyOne
            let ps = baseCtor.GetParameters() |> Array.map (fun p -> p.ParameterType)
            let cb = tb.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, ps)
            let il = cb.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            for i in 1 .. ps.Length do il.Emit(OpCodes.Ldarg_S, byte i)
            il.Emit(OpCodes.Call, baseCtor)
            il.Emit(OpCodes.Ret)
            let checkCall =
                tb.DefineMethod("CheckCall", MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig, typeof<Void>, [| siteT |])
            let il2 = checkCall.GetILGenerator()
            il2.Emit(OpCodes.Ldarg_0)
            il2.Emit(OpCodes.Ldarg_1)
            il2.Emit(OpCodes.Call, typeof<CheckingFunction>.GetMethod("OnCheckCall"))
            il2.Emit(OpCodes.Ret)
            match icc.GetMethod("CheckCall", [| siteT |]) with
            | null -> ()
            | m -> tb.DefineMethodOverride(checkCall, m)
            Some (tb.CreateType())
        | _ -> None)

/// A host function `name` of the parameters given, whose call check runs
/// `check fn site rawSite` for each call it is given. Fails the calling test, for
/// JYRO-REQ-2119, when the build has no call check.
let make
    (name: string)
    (parameters: Parameter list)
    (effect: FunctionEffect)
    (check: CheckingFunction -> Site -> obj -> unit)
    : CheckingFunction =
    match emitted.Value with
    | Some t ->
        let signature = Host.signature name parameters ParameterType.Any
        try
            Activator.CreateInstance(t, [| box name; box signature; box effect; box check |]) :?> CheckingFunction
        with :? TargetInvocationException as e when not (isNull e.InnerException) ->
            Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(e.InnerException).Throw()
            Unchecked.defaultof<_>
    | None -> Pending.notInBuild "JYRO-REQ-2119" "IJyroCallCheck and JyroCallSite (160.2)"

/// The tables the test host knows (the example of JYRO-REQ-2119).
let tables : Set<string> = set [ "customers" ]

/// The two parameters of `Query`: `name`, required, and `opt`, optional.
let queryParameters : Parameter list =
    [ Parameter.Required("name", ParameterType.Any); Parameter.Optional("opt", ParameterType.Any) ]

/// `Query(name, opt?)` of the effect given: its check refuses, with
/// `throw JyroError.Host("TableUnknown", name)`, a call whose first argument is a string
/// literal that is not one of `tables`.
let queryWithEffect (effect: FunctionEffect) : CheckingFunction =
    make "Query" queryParameters effect (fun fn site _ ->
        match site.StringLiteral 0 with
        | Some t when not (tables.Contains t) -> fn.Refuse("TableUnknown", Some [| box t |])
        | _ -> ())

/// `Query(name, opt?)`, Pure (see `queryWithEffect`).
let query () : CheckingFunction = queryWithEffect FunctionEffect.Pure

/// Five optional parameters `a` to `e`.
let fiveOptional : Parameter list = [ for p in [ "a"; "b"; "c"; "d"; "e" ] -> Parameter.Optional(p, ParameterType.Any) ]

/// A checking function `name` of up to five optional arguments `a` to `e` whose check
/// runs `act` with the function and the site.
let checking (name: string) (act: CheckingFunction -> Site -> unit) : CheckingFunction =
    make name fiveOptional FunctionEffect.Pure (fun fn s _ -> act fn s)

/// A checking function `name` of up to five optional arguments that accepts every call.
let accepting (name: string) : CheckingFunction = checking name (fun _ _ -> ())

/// The arguments of a diagnostic read as 160.7.16 reads them.
let argsOf (d: DiagnosticMessage) : string list =
    if isNull d.Args then []
    else d.Args |> Array.map (fun a -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture)) |> List.ofArray

/// The diagnostics of `r` of the code `code`.
let ofCode (code: MessageCode) (r: JyroResult<'T>) : DiagnosticMessage list =
    r.Messages |> Seq.filter (fun m -> m.Code = code) |> List.ofSeq

/// A result of any payload as a result of `obj`, keeping its messages.
let boxed (r: JyroResult<'T>) : JyroResult<obj> =
    if r.IsSuccess then JyroResult<obj>.Success(box (Script.valueOf r), r.Messages)
    else JyroResult<obj>.Failure(r.Messages)

// -- A host function whose Effect gives none of the four effects (JYRO-REQ-2136) ----------------------------

/// The ways that reading `Effect` can fail to give one of the four effects of 120.4.1: it throws
/// an exception of the named type, or it returns a value that is no effect: the book's
/// `(FunctionEffect)99`. FunctionEffect is an enumeration (JYRO-ADR-192), so null is not a
/// value of it and there is no null-effect kind.
let effectFaultKinds : string list =
    [ "InvalidOperationException"; "JyroRuntimeException"; "TimeoutException"; "Tag99" ]

/// Read as `Effect` of a host function would: throw, or give a value that is no effect.
let effectOf (kind: string) : FunctionEffect =
    match kind with
    | "InvalidOperationException" -> raise (InvalidOperationException "the host cannot say the effect")
    | "JyroRuntimeException" -> raise (JyroError.Runtime(MessageCode.InvalidJson))
    | "TimeoutException" -> raise (TimeoutException "the host's registry timed out")
    | _ -> LanguagePrimitives.EnumOfValue<int, FunctionEffect> 99

/// A host function `name` whose `Effect` fails as `kind` says (`effectFaultKinds`) every time it is read.
type EffectFaultFunction(name: string, kind: string) =
    inherit JyroFunctionBase(name, Host.signature name [] ParameterType.Any)
    override _.Effect = effectOf kind
    override _.Execute(_, _) = Values.nul

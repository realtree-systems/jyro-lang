/// jyro-spec 90.1 (availability of the standard library) and 90.2 (the
/// function inventory). From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S90_01_02_Inventory

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Inventory

let inventoryNames () = Inventory.names ()

let private nondeterministic = set [ "Now"; "Today"; "NewGuid"; "RandomString"; "RandomInt"; "RandomChoice" ]

// ===========================================================================
// 90.1.1 Every listed function is available to a host
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-319")>]
[<MemberData(nameof inventoryNames)>]
let ``ListedFunction_IsRegisteredInTheStandardLibrary`` (name: string) =
    Assert.True((stdlibLookup name).IsSome, sprintf "StdlibRegistry.TryGetFunction(%s) is false" name)
    Assert.Contains(name, StdlibRegistry.getAll () |> Seq.map (fun f -> f.Name))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-319")>]
[<MemberData(nameof inventoryNames)>]
let ``ListedFunction_IsCallableFromAScriptWithTheStandardLibrary`` (name: string) =
    let l = byName name
    for call in [ l.RequiredCall; l.FullCall ] do
        let r = Script.runWith allEffects ("Output.r = " + call)
        Assert.True(r.IsSuccess, sprintf "%s failed:\n%s" call (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-319")>]
let ``UnlistedName_IsRejectedWithUndefinedFunction`` () =
    let r = Script.compile "Output.r = NoSuchFunction()"
    Assert.Contains(MessageCode.UndefinedFunction, Script.errorCodes r)

// ===========================================================================
// 90.1.3 No listed function is removed or renamed
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-321")>]
[<MemberData(nameof inventoryNames)>]
let ``ListedFunction_IsReachableUnderExactlyItsListedName`` (name: string) =
    match stdlibLookup name with
    | None -> Values.fail (sprintf "%s is not in the standard library" name)
    | Some f -> Assert.Equal(name, f.Name)
    let count = StdlibRegistry.getAll () |> Seq.filter (fun f -> f.Name = name) |> Seq.length
    Assert.Equal(1, count)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-321")>]
[<MemberData(nameof inventoryNames)>]
let ``ListedFunction_OtherwiseCasedSpelling_DoesNotResolveToIt`` (name: string) =
    let registered = StdlibRegistry.getAll () |> Seq.map (fun f -> f.Name) |> set
    for spelling in [ name.ToLowerInvariant(); name.ToUpperInvariant() ] do
        if spelling <> name && not (registered.Contains spelling) then
            let l = byName name
            let call = spelling + l.FullCall.Substring(name.Length)
            let r = Script.compileWith allEffects ("Output.r = " + call)
            Assert.Contains(MessageCode.UndefinedFunction, Script.errorCodes r)

// ===========================================================================
// 90.1.4 and 90.2.1 to 90.2.8: names, order and optional parameters
// ===========================================================================

/// The signature check: exactly the listed parameter names, in order, with
/// exactly the bracketed ones optional.
let checkSignature (l: Listed) =
    match stdlibLookup l.Name with
    | None -> Values.fail (sprintf "%s is not in the standard library" l.Name)
    | Some f ->
        let actual = f.Signature.Parameters |> Seq.map (fun p -> p.Name, p.IsOptional) |> List.ofSeq
        let expected = l.Params |> List.map (fun (n, o, _) -> n, o)
        Assert.Equal<(string * bool) list>(expected, actual)
        Assert.Equal(l.MinArgs, f.Signature.MinArgs)
        Assert.Equal(l.MaxArgs, f.Signature.MaxArgs)

/// The arity and naming check through a script: required only and all
/// arguments compile; one fewer than required and one more than all are
/// rejected with their codes; a call by the listed names in reverse order
/// compiles and binds each argument by its name.
let checkArity (l: Listed) =
    assertCompiles (Script.compileWith allEffects ("Output.r = " + l.RequiredCall))
    assertCompiles (Script.compileWith allEffects ("Output.r = " + l.FullCall))
    if l.MinArgs > 0 then
        let r = Script.compileWith allEffects ("Output.r = " + l.CallWith(l.MinArgs - 1))
        Assert.Equal<MessageCode list>([ MessageCode.TooFewArguments ], Script.errorCodes r)
    let extra =
        if l.MaxArgs = 0 then sprintf "%s(1)" l.Name
        else l.FullCall.Substring(0, l.FullCall.Length - 1) + ", 1)"
    let r = Script.compileWith allEffects ("Output.r = " + extra)
    Assert.Equal<MessageCode list>([ MessageCode.TooManyArguments ], Script.errorCodes r)
    if l.MaxArgs > 0 then
        let named =
            l.Params |> List.rev |> List.map (fun (n, _, s) -> sprintf "%s: %s" n s) |> String.concat ", "
            |> sprintf "%s(%s)" l.Name
        let r = Script.runWith allEffects (sprintf "Output.a = %s\nOutput.b = %s" l.FullCall named)
        Assert.True(r.IsSuccess, sprintf "%s failed:\n%s" named (Script.describe r))
        if not (nondeterministic.Contains l.Name) then
            let a = outProp "a" r
            let b = outProp "b" r
            Assert.True(a.EqualsValue b, sprintf "%s gave %s but %s gave %s" l.FullCall (Values.toJson a) named (Values.toJson b))

let checkListed (name: string) =
    let l = byName name
    checkSignature l
    checkArity l

[<Theory>]
[<Trait("requirement", "JYRO-REQ-322")>]
[<MemberData(nameof inventoryNames)>]
let ``ListedFunction_KeepsItsParameterNamesOrderAndOptionality`` (name: string) = checkListed name

let stringFunctions () = namesOf "90.2.1"
let arrayFunctions () = namesOf "90.2.2"
let mathFunctions () = namesOf "90.2.3"
let dateFunctions () = namesOf "90.2.4"
let utilityFunctions () = namesOf "90.2.5"
let schemaFunctions () = namesOf "90.2.6"
let queryFunctions () = namesOf "90.2.7"
let lambdaFunctions () = namesOf "90.2.8"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1913")>]
[<MemberData(nameof stringFunctions)>]
let ``StringFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1914")>]
[<MemberData(nameof arrayFunctions)>]
let ``ArrayFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1915")>]
[<MemberData(nameof mathFunctions)>]
let ``MathFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1916")>]
[<MemberData(nameof dateFunctions)>]
let ``DateFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1917")>]
[<MemberData(nameof utilityFunctions)>]
let ``UtilityFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1918")>]
[<MemberData(nameof schemaFunctions)>]
let ``SchemaFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-329")>]
[<MemberData(nameof queryFunctions)>]
let ``QueryFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

[<Theory>]
[<Trait("requirement", "JYRO-REQ-330")>]
[<MemberData(nameof lambdaFunctions)>]
let ``LambdaFunction_HasTheListedNameParametersAndOptionality`` (name: string) = checkListed name

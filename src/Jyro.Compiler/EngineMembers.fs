namespace RealTree.Jyro.Compilation

open RealTree.Jyro

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Reflection

// Every member of the engine's own assemblies that compiled code calls,
// resolved once, when this module is loaded, and not when a script that holds
// a particular construct is compiled (JYRO-ADR-206). A member that is not
// found, or whose name matches more than one member, fails the load with an
// EngineBindingException naming the type and the member; no lookup gives
// null. Each name is written with nameof where F# can, so a rename or a
// removal fails the build; a constructor has no name to give.
//
// A method is looked up by a name the trimmer reads, and a property or a
// constructor on a type annotated as keeping all its properties or
// constructors (JYRO-ADR-202 item 3), so a host published with trimming keeps
// every member the table names.
module internal EngineMembers =

    [<Literal>]
    let private Properties = DynamicallyAccessedMemberTypes.PublicProperties ||| DynamicallyAccessedMemberTypes.NonPublicProperties

    [<Literal>]
    let private Constructors = DynamicallyAccessedMemberTypes.PublicConstructors ||| DynamicallyAccessedMemberTypes.NonPublicConstructors

    [<Literal>]
    let private Binding =
        BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic

    let private fail (t: Type) (name: string) (problem: string) : 'T =
        raise (EngineBindingException(
                sprintf "The engine member %s.%s that compiled code calls %s." t.FullName name problem))

    let private single (t: Type) (name: string) (found: 'M[]) : 'M =
        match found with
        | [| m |] -> m
        | [||] -> fail t name "was not found"
        | _ -> fail t name "matches more than one member"

    // The method `lookup` finds on `t` by `name`: a lookup that finds none,
    // or more than one, fails naming `t` and `name`.
    let private resolved (t: Type) (name: string) (lookup: unit -> MethodInfo | null) : MethodInfo =
        let found =
            try lookup ()
            with :? AmbiguousMatchException -> fail t name "matches more than one member"
        match found with
        | null -> fail t name "was not found"
        | m -> m

    // The one method of `'T` named `name`, of any visibility.
    //
    // Inline, with `'T` and `name` written at each use, so that each lookup
    // in the table is a call of GetMethod on a type and a name the trimmer
    // knows: it keeps the methods of that name and reads no other. An
    // annotation keeping every method of the type would also reach the
    // members that convert a host object, which require unreferenced code,
    // and make the trimmer warn of this table.
    let inline private methodOf<'T> (name: string) : MethodInfo =
        resolved typeof<'T> name (fun () -> typeof<'T>.GetMethod(name, Binding))

    // The method of `'T` named `name` that takes exactly `parameterTypes`.
    let inline private methodWith<'T> (name: string) (parameterTypes: Type[]) : MethodInfo =
        let m = resolved typeof<'T> name (fun () -> typeof<'T>.GetMethod(name, Binding, null, parameterTypes, null))
        if (m.GetParameters() |> Array.map (fun p -> p.ParameterType)) <> parameterTypes then
            fail typeof<'T> name "was not found"
        m

    // The one property of `t` named `name`.
    let private propertyOf ([<DynamicallyAccessedMembers(Properties)>] t: Type) (name: string) : PropertyInfo =
        t.GetProperties(Binding) |> Array.filter (fun p -> p.Name = name) |> single t name

    // The accessor of a property, which must have one.
    let private accessor (t: Type) (name: string) (which: string) (m: MethodInfo | null) : MethodInfo =
        match m with
        | null -> fail t (name + "." + which) "was not found"
        | m -> m

    // The constructor of `t` that takes `parameterTypes`.
    let private constructorWith ([<DynamicallyAccessedMembers(Constructors)>] t: Type) (parameterTypes: Type[]) : ConstructorInfo =
        t.GetConstructors(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Array.filter (fun c -> (c.GetParameters() |> Array.map (fun p -> p.ParameterType)) = parameterTypes)
        |> single t ".ctor"

    // === JyroValue and its kinds ===

    let coerceToType =
        methodWith<JyroValue> (nameof JyroValue.CoerceToType) [| typeof<JyroValue>; typeof<JyroValueType> |]
    let isNull = propertyOf typeof<JyroValue> (nameof Unchecked.defaultof<JyroValue>.IsNull)
    let valueType = propertyOf typeof<JyroValue> (nameof Unchecked.defaultof<JyroValue>.ValueType)
    let getProperty = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.GetProperty)
    let getIndex = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.GetIndex)
    let evaluateBinary = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.EvaluateBinary)
    let evaluateUnary = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.EvaluateUnary)
    let isTruthy = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.IsTruthy)
    let deleteProperty = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.DeleteProperty)
    let equalsValue = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.EqualsValue)
    let toIterable = methodOf<JyroValue> (nameof Unchecked.defaultof<JyroValue>.ToIterable)
    let newObject = constructorWith typeof<JyroObject> Type.EmptyTypes
    let objectSetProperty = methodOf<JyroObject> (nameof Unchecked.defaultof<JyroObject>.SetProperty)
    let newArray = constructorWith typeof<JyroArray> Type.EmptyTypes
    let arrayAdd = methodOf<JyroArray> (nameof Unchecked.defaultof<JyroArray>.Add)
    let createFunction =
        methodWith<JyroFunction> (nameof JyroFunction.Create)
            [| typeof<Func<IReadOnlyList<JyroValue>, JyroValue>>; typeof<int> |]
    let fromBoolean = methodOf<JyroBoolean> (nameof JyroBoolean.FromBoolean)

    // === The bounded walks of a value (spec 100.8.7, 100.8.8) ===

    let scriptString = methodOf<ValueWalk> (nameof ValueWalk.ScriptString)
    let scriptSetProperty = methodOf<ValueWalk> (nameof ValueWalk.ScriptSetProperty)
    let scriptSetIndex = methodOf<ValueWalk> (nameof ValueWalk.ScriptSetIndex)

    // === The execution context, every member internal (JYRO-ADR-191) ===

    let checkAndEnterCall =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.CheckAndEnterCall)
    let exitCall =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.ExitCall)
    let onFunctionResult =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.OnFunctionResult)
    let markLocation =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.MarkLocation)
    let runningLocation =
        propertyOf typeof<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.RunningLocation)
    let checkAndCountStatement =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.CheckAndCountStatement)
    let checkAndEnterLoop =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.CheckAndEnterLoop)
    let setReturnMessage =
        methodOf<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.SetReturnMessage)
    let private outputName = nameof Unchecked.defaultof<JyroExecutionContext>.Output
    let contextOutputSetter =
        propertyOf typeof<JyroExecutionContext> outputName
        |> fun p -> accessor typeof<JyroExecutionContext> outputName "set" (p.GetSetMethod(true))
    let contextInput =
        propertyOf typeof<JyroExecutionContext> (nameof Unchecked.defaultof<JyroExecutionContext>.Input)
    let runningCallWithin = methodOf<RunningCall> (nameof RunningCall.Within)

    // === Calls of functions ===

    let functionExecute = methodOf<IJyroFunction> (nameof Unchecked.defaultof<IJyroFunction>.Execute)
    let newCallArguments =
        constructorWith typeof<CallArguments> [| typeof<IJyroFunction>; typeof<IReadOnlyList<JyroValue>> |]
    let userFunctionDelegate =
        propertyOf typeof<JyroUserFunction> (nameof Unchecked.defaultof<JyroUserFunction>.CompiledDelegate)

    // === The runtime checks of statements ===

    let checkForValues = methodOf<StatementRuntime> (nameof StatementRuntime.CheckForValues)
    let forValue = methodOf<StatementRuntime> (nameof StatementRuntime.ForValue)
    let variantOf = methodOf<StatementRuntime> (nameof StatementRuntime.VariantOf)
    let subjectNotVariant = methodOf<StatementRuntime> (nameof StatementRuntime.SubjectNotVariant)
    let deleteAtIndex = methodOf<StatementRuntime> (nameof StatementRuntime.DeleteAtIndex)
    let newRuntimeException =
        constructorWith typeof<JyroRuntimeException> [| typeof<MessageCode>; typeof<obj[]> |]
    let newScriptExit = constructorWith typeof<JyroScriptExit> Type.EmptyTypes

    // === The readers of host numbers that JyroValue makes delegates of ===

    let numberReaders = HostShapes.numberReaderMethods ()

    // === Members of the base class library that compiled code calls ===

    let listItem = propertyOf typeof<IReadOnlyList<JyroValue>> "Item" |> fun p -> accessor typeof<IReadOnlyList<JyroValue>> "Item" "get" (p.GetGetMethod())
    let collectionCount = propertyOf typeof<IReadOnlyCollection<JyroValue>> "Count"
    let getEnumerator = methodOf<IEnumerable<JyroValue>> "GetEnumerator"
    let moveNext = methodOf<System.Collections.IEnumerator> "MoveNext"
    let enumeratorCurrent = propertyOf typeof<IEnumerator<JyroValue>> "Current"

    // The whole table, read once by `ensureLoaded`; reading it loads the module.
    let all : MemberInfo[] =
        Array.append
            [| coerceToType; isNull; valueType; getProperty; getIndex; evaluateBinary; evaluateUnary
               isTruthy; deleteProperty; equalsValue; toIterable; newObject; objectSetProperty; newArray
               arrayAdd; createFunction; fromBoolean; scriptString; scriptSetProperty; scriptSetIndex
               checkAndEnterCall; exitCall; onFunctionResult; markLocation; runningLocation
               checkAndCountStatement; checkAndEnterLoop; setReturnMessage; contextOutputSetter
               contextInput; runningCallWithin; functionExecute; newCallArguments; userFunctionDelegate
               checkForValues; forValue; variantOf; subjectNotVariant; deleteAtIndex; newRuntimeException
               newScriptExit; listItem; collectionCount; getEnumerator; moveNext; enumeratorCurrent |]
            (numberReaders |> Array.map (fun m -> m :> MemberInfo))

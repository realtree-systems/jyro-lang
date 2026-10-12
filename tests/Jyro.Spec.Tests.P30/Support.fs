/// jyro-spec verification, chapter 30: helpers local to this project.
/// Written from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.P30.Support

open System
open System.Globalization
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// An inexact 1: `Power` with a non-integer exponent computes in double
/// precision and returns an inexact number (93.7.5).
let I1 = "Power(1, 0.5)"

/// A decimal from text, invariant culture, exponent allowed.
let dec (s: string) : decimal = Decimal.Parse(s, NumberStyles.Float, CultureInfo.InvariantCulture)

/// The nearest double of the text (correctly rounded, invariant culture).
let dbl (s: string) : float = Double.Parse(s, NumberStyles.Float, CultureInfo.InvariantCulture)

let bits (d: float) : int64 = BitConverter.DoubleToInt64Bits d

/// The `Output` of a successful run with the defaults.
let outOf (source: string) : JyroValue = Script.outputOf source

/// `Output.r` of a successful run with the defaults.
let rOf (source: string) : JyroValue = outOf source |> Values.prop "r"

/// `Output.r` after `Output.r = <expr>`.
let evalR (expr: string) : JyroValue = rOf ("Output.r = " + expr)

/// The boolean of `Output.r` after `Output.r = <expr>`.
let boolOf (expr: string) : bool = Values.asBool (evalR expr)

/// Run with the defaults and expect exactly one error, of `code`.
let expectRun (code: MessageCode) (source: string) : unit = Script.expectError code (Script.run source)

/// Assert a value is an exact number holding exactly `expected`.
let assertExact (expected: decimal) (v: JyroValue) : unit =
    if not (Values.isExact v) then
        Values.fail (sprintf "Expected an exact %M, got an inexact %s" expected (v.ToJson()))
    Assert.Equal(expected, Values.asDecimal v)

/// Assert a value is an inexact number whose double is bit-for-bit `expected`.
let assertInexact (expected: float) (v: JyroValue) : unit =
    if Values.isExact v then
        Values.fail (sprintf "Expected an inexact %s, got an exact %s" (expected.ToString("R")) (v.ToJson()))
    let actual = Values.asFloat v
    if bits actual <> bits expected then
        Values.fail (sprintf "Expected the double %s, got %s" (expected.ToString("R")) (actual.ToString("R")))

/// Assert `IsSuccess` is false (any diagnostic).
let assertFails (result: JyroResult<'T>) : unit =
    if result.IsSuccess then Values.fail "Expected the script to fail, but it succeeded"

/// The only error of `result` carries `code` (see Script.expectError).
let assertCode (code: MessageCode) (result: JyroResult<'T>) : unit = Script.expectError code result

/// Run `action` on its own thread and fail the test when it has not finished
/// within `seconds`. Exceptions of the action are returned, not thrown.
let within (seconds: float) (action: unit -> 'T) : Result<'T, exn> =
    let task = Task.Run(fun () -> try Ok (action ()) with e -> Error e)
    if not (task.Wait(TimeSpan.FromSeconds seconds)) then
        Values.fail (sprintf "The operation did not finish within %g seconds" seconds)
    task.Result

/// Options with generous loop and statement limits, for scripts that build
/// large values in a loop.
let bigLimits : RunOptions =
    { Script.defaults with
        Limits =
            Some (Script.limits (fun o ->
                { o with
                    MaxLoopIterations = 1_000_000
                    MaxStatements = 5_000_000
                    MaxExecutionTime = TimeSpan.FromSeconds 30.0 })) }

/// The bound of 100.7, as the host API documents it.
/// The value nesting maximum of the reference implementation (JYRO-REQ-2042 states it: 128).
let maxDepth () : int = 128

/// Assert an exception is a `JyroRuntimeException` (or subclass) with `code`.
let assertRuntimeException (code: MessageCode) (e: exn) : unit =
    match e with
    | :? JyroRuntimeException as j ->
        if j.Code <> code then
            Values.fail (sprintf "Expected JyroRuntimeException %A, got %A: %s" code j.Code j.Message)
    | other -> Values.fail (sprintf "Expected JyroRuntimeException %A, got %s: %s" code (other.GetType().FullName) other.Message)

/// Run `action`, expect it to throw a `JyroRuntimeException` with `code`.
let throwsCode (code: MessageCode) (action: unit -> 'T) : unit =
    let outcome = try Ok (action ()) with e -> Error e
    match outcome with
    | Ok v -> Values.fail (sprintf "Expected JyroRuntimeException %A, but a value was returned: %A" code v)
    | Error e -> assertRuntimeException code e

/// The only error of `result` carries `code` and exactly the arguments `args`.
let expectErrorArgs (code: MessageCode) (args: string list) (result: JyroResult<'T>) : unit =
    Script.expectError code result
    let d = result.Messages |> Seq.find (fun m -> m.Severity = MessageSeverity.Error)
    let actual =
        if isNull d.Args then Values.fail (sprintf "The %A diagnostic's Args is null" code)
        else d.Args |> Array.map (fun a -> Convert.ToString(a, CultureInfo.InvariantCulture)) |> List.ofArray
    if actual <> args then
        Values.fail (sprintf "Expected the Args %A of the %A diagnostic, got %A" args code actual)

/// Every diagnostic's code, for failure text.
let describe (r: JyroResult<'T>) = Script.describe r

/// `JyroScript.ExecuteScript(String, Object)` (an F# module function).
let executeScript (script: string, input: obj) : JyroResult<JyroValue> = JyroScript.executeScript script input

/// `JyroScript.ExecuteScriptWithJson(String, String)` (an F# module function).
let executeScriptWithJson (script: string, json: string) : JyroResult<JyroValue> = JyroScript.executeScriptWithJson script json

/// Assert a value is a number equal to `expected`, in either representation
/// (for tests whose requirement does not govern the representation).
let assertNum (expected: decimal) (v: JyroValue) : unit =
    Assert.Equal(float expected, Values.asFloat v)

/// A string of one UTF-16 code unit (a lone surrogate cannot be an F# literal).
let unit (codeUnit: int) : string = String(char codeUnit, 1)

/// Assert a host exception carries `code`: a `JyroRuntimeException` whose
/// `Code` is it, or a `JyroException` whose one message has it (the
/// book names the code a host failure carries, not always the exception type).
let assertCarriesCode (code: MessageCode) (e: exn) : unit =
    match e with
    | :? JyroException as x when not (x :? JyroRuntimeException) ->
        let codes = x.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq
        if codes <> [ code ] then Values.fail (sprintf "Expected the code %A, the exception carries %A" code codes)
    | _ -> assertRuntimeException code e

// -- Host shapes of JYRO-ADR-152 ----------------------------------------------

/// A dictionary that is only an IReadOnlyDictionary<string, 'T> (JYRO-ADR-152 (1)),
/// enumerated in the order its pairs are given.
type ReadOnlyOnly<'T>(pairs: (string * 'T) list) =
    let d = Collections.Generic.Dictionary<string, 'T>()
    do for (k, v) in pairs do d.[k] <- v
    interface Collections.Generic.IReadOnlyDictionary<string, 'T> with
        member _.Item with get (k: string) = d.[k]
        member _.Keys = d.Keys :> seq<string>
        member _.Values = d.Values :> seq<'T>
        member _.ContainsKey(k: string) = d.ContainsKey k
        member _.TryGetValue(k: string, value: byref<'T>) = d.TryGetValue(k, &value)
    interface Collections.Generic.IReadOnlyCollection<Collections.Generic.KeyValuePair<string, 'T>> with
        member _.Count = d.Count
    interface Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string, 'T>> with
        member _.GetEnumerator() = (d :> Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<string, 'T>>).GetEnumerator()
    interface Collections.IEnumerable with
        member _.GetEnumerator() = (d :> Collections.IEnumerable).GetEnumerator()

/// A host object whose type gives a ToString of its own, as the compiler gives
/// a C# record or a C# anonymous type: it is an object of its public properties
/// all the same (JYRO-ADR-152 (4)).
type RecordLike<'T>(x: 'T) =
    member _.X = x
    override _.ToString() = sprintf "RecordLike { X = %O }" (box x)

/// The same with two properties, declared A then B.
type RecordPair<'T>(a: 'T, b: 'T) =
    member _.A = a
    member _.B = b
    override _.ToString() = sprintf "RecordPair { A = %O, B = %O }" (box a) (box b)

// -- Host shapes of JYRO-ADR-156 ----------------------------------------------

/// A struct whose data is in one public field, X, and which has no public
/// property (JYRO-ADR-156 (d)). (An F# `val` without `mutable` compiles to a
/// property; `val mutable` with DefaultValue is a public field.)
[<Struct>]
type FieldStruct<'T> =
    [<DefaultValue(false)>] val mutable X: 'T

/// A FieldStruct holding `x`.
let fieldStruct (x: 'T) : FieldStruct<'T> =
    let mutable s = FieldStruct<'T>()
    s.X <- x
    s

/// A class with one public property, P, and one public field, F, declared F
/// first: its object has P then F, properties before fields (JYRO-ADR-156 (d)).
type PropAndField<'T>(p: 'T) =
    [<DefaultValue(false)>] val mutable F: 'T
    member _.P = p

/// A PropAndField with P = `p` and F = `f`.
let propAndField (p: 'T) (f: 'T) : PropAndField<'T> =
    let c = PropAndField<'T>(p)
    c.F <- f
    c

/// A class whose getter `Value` throws for some instances and not others
/// (JYRO-DEF-615): the property is left out, and the value stays an object
/// (JYRO-ADR-156 (e)).
type Guarded(ok: bool) =
    member _.Ok = ok
    member _.Value : int = if ok then 5 else invalidOp "Guarded has no value"

/// Members that are not public readable instance properties or fields: a
/// static property, an indexer and an internal property (JYRO-ADR-156 (d)).
/// Only `Visible` is one.
type Members() =
    static member Static = 1
    member _.Item with get (i: int) = i
    member _.Visible = 2
    member internal _.Hidden = 3

/// A class with no public property and no public field (JYRO-ADR-156 (f)).
type NoMembers() =
    override _.ToString() = "no members"

/// The public readable instance properties that are not indexers, then the
/// public instance fields, of `t`, each in the order .NET lists it: the keys
/// JYRO-ADR-156 (d) gives an object made from a value of `t`.
let memberKeys (t: Type) : string list =
    let props =
        t.GetProperties(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)
        |> Array.filter (fun p -> p.CanRead && p.GetIndexParameters().Length = 0 && not (isNull (p.GetGetMethod())))
        |> Array.map (fun p -> p.Name)
    let fields =
        t.GetFields(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance) |> Array.map (fun f -> f.Name)
    List.ofArray (Array.append props fields)

// -- Host shapes of JYRO-ADR-157 ----------------------------------------------

/// A host object with one public property, Next, that a host can point back at
/// the object itself, making a cyclic host graph.
type Node() =
    member val Next : obj = null with get, set

/// A Node whose Next is itself.
let cyclicNode () : Node =
    let n = Node()
    n.Next <- box n
    n

/// Host arrays nested `depth` deep: `[[...[1]...]]`, `depth` arrays in all.
let nestedArrays (depth: int) : obj[] =
    let mutable v : obj[] = [| box 1 |]
    for _ in 2 .. depth do
        v <- [| box v |]
    v

/// A System.Text.Json JsonValue made by JsonValue.Create<'T> from a .NET value,
/// which holds that value itself, not its JSON (JYRO-ADR-157 (b1)).
let jsonValueOf (x: 'T) : obj = box (System.Text.Json.Nodes.JsonValue.Create<'T>(x))

// -- Codes reached by name (FLOORS 4.1.7) -------------------------------------

/// The member `name` of `MessageCode`, reached by name so that this project
/// compiles on a build that does not declare it yet. A build without it fails
/// the calling test, reported for `definer`, the requirement that fixes the
/// member and its number (FLOORS-BLOCKED unless the test is `definer`'s own).
let codeNamed (definer: string) (name: string) : MessageCode =
    match Enum.TryParse<MessageCode>(name, false) with
    | true, c when Enum.GetName(typeof<MessageCode>, c) = name -> c
    | _ -> Pending.notInBuild definer (sprintf "MessageCode.%s" name)

/// 30.2.19 (JYRO-REQ-1700, JYRO-ADR-153 (2)): a host number that is NaN, 5921.
let hostNumberNaN () = codeNamed "JYRO-REQ-1700" "HostNumberNaN"

/// 30.2.19 (JYRO-REQ-1700, JYRO-ADR-153 (2)): a host number that is an infinity, 5922.
let hostNumberInfinite () = codeNamed "JYRO-REQ-1700" "HostNumberInfinite"

/// 30.3.6 (JYRO-REQ-68, JYRO-ADR-153 (1)): an index that is not an integer, 5310.
let integerIndexRequired () = codeNamed "JYRO-REQ-68" "IntegerIndexRequired"

/// A code given by name in test data: the three names above are resolved through
/// their definers, every other name is a member every build has.
let codeOf (name: string) : MessageCode =
    match name with
    | "HostNumberNaN" -> hostNumberNaN ()
    | "HostNumberInfinite" -> hostNumberInfinite ()
    | "IntegerIndexRequired" -> integerIndexRequired ()
    | other -> Enum.Parse<MessageCode>(other)

/// The arguments of a diagnostic, each read as 160.7.16 reads it.
let argTexts (d: DiagnosticMessage) : string list =
    Assert.NotNull(d.Args)
    d.Args |> Array.map (fun a -> Convert.ToString(a, CultureInfo.InvariantCulture)) |> List.ofArray

/// Assert the arguments of `d` are exactly `expected`, in order.
let assertArgs (expected: string list) (d: DiagnosticMessage) : unit =
    let actual = argTexts d
    if actual <> expected then
        Values.fail (sprintf "Expected the Args %A of the %A diagnostic, got %A" expected d.Code actual)

/// Run `action`, expect a `JyroRuntimeException` of `code` whose `Args` is the
/// empty array (130.6.2: nothing to name).
let throwsCodeWithNoArgs (code: MessageCode) (action: unit -> 'T) : unit =
    let outcome = try Ok (action ()) with e -> Error e
    match outcome with
    | Ok v -> Values.fail (sprintf "Expected JyroRuntimeException %A, but a value was returned: %A" code v)
    | Error e ->
        assertRuntimeException code e
        let j = e :?> JyroRuntimeException
        if isNull j.Args then Values.fail (sprintf "The %A exception's Args is null, not the empty array" code)
        if j.Args.Length <> 0 then
            let texts = j.Args |> Array.map (fun a -> Convert.ToString(a, CultureInfo.InvariantCulture))
            Values.fail (sprintf "Expected no Args on the %A exception, got %A" code texts)

// -- Every limit set explicitly (FLOORS 4.1.11) ------------------------------

/// The four execution limits of 100.1.1, each set explicitly and far above
/// what a test needs, so no run depends on an implementation default.
let allLimits (o: JyroExecutionOptions) : JyroExecutionOptions =
    { o with
        MaxExecutionTime = TimeSpan.FromSeconds 30.
        MaxStatements = 1_000_000
        MaxLoopIterations = 1_000_000
        MaxCallDepth = 64 }

/// `Script.defaults` with the four execution limits and a generous
/// compilation time limit (100.9.1) set explicitly.
let limited : RunOptions =
    { Script.defaults with
        Limits = Some (Script.limits allLimits)
        CompilationTimeout = Some (TimeSpan.FromMinutes 10.) }

/// `limited`, with the regex timeout (160.1.10) set explicitly as well.
let limitedRegex : RunOptions =
    { limited with ConfigureRunner = fun r -> Pending.withRegexTimeout r (TimeSpan.FromSeconds 5.) }

/// Assert `r` has at least one Error and every Error has the stage `stage`
/// (160.1.12, read through `Pending.stageOf`).
let assertAllStages (stage: string) (r: JyroResult<'T>) : unit =
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    if List.isEmpty errors then
        Values.fail (sprintf "Expected Error diagnostics of stage %s, got none. Diagnostics:\n%s" stage (Script.describe r))
    let stages = errors |> List.map (fun m -> m.Code, Pending.stageOf m)
    if stages |> List.exists (fun (_, s) -> s <> stage) then
        Values.fail (sprintf "Expected every error to have the stage %s; the errors have %A. Diagnostics:\n%s"
                        stage stages (Script.describe r))

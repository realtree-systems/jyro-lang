/// Helpers for the chapter 92 (Array Functions) verification suite. Written
/// from the book and the public host API alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.P92Support

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

// -- Reading and comparing values ---------------------------------------------

/// The JSON text of a value, or a type name when it cannot be written (a
/// function value inside it), for failure messages.
let show (v: JyroValue) : string =
    if isNull (box v) then "<.NET null>"
    else
        try Values.toJson v
        with _ -> sprintf "<%A, not writable as JSON>" v.ValueType

/// The first difference between the value `want` (parsed by System.Text.Json,
/// not by the implementation) and the Jyro value `got`, at `path`; None when
/// they are the same value. Element order and key order count; strings,
/// booleans and null compare as themselves; a number compares by its value
/// alone - neither its string form (40.4.6) nor its representation (30.2.3) -
/// so a clause about which values a function returns is not judged by another
/// clause's text (FLOORS 4.1.15). Two exact numbers are equal when their decimal
/// values are equal (so 0.50 equals 0.5); otherwise the doubles are compared.
let rec private differs (path: string) (want: Text.Json.JsonElement) (got: JyroValue) : string option =
    let kind =
        if isNull (box got) then "<.NET null>"
        else string got.ValueType
    let mismatch () = Some (sprintf "at %s: expected %s, got %s" path (want.GetRawText()) (show got))
    match want.ValueKind, got with
    | _ when isNull (box got) -> mismatch ()
    | Text.Json.JsonValueKind.Null, _ -> if got.ValueType = JyroValueType.Null then None else mismatch ()
    | Text.Json.JsonValueKind.True, (:? JyroBoolean as b) -> if b.Value then None else mismatch ()
    | Text.Json.JsonValueKind.False, (:? JyroBoolean as b) -> if not b.Value then None else mismatch ()
    | Text.Json.JsonValueKind.String, (:? JyroString as s) -> if s.Value = want.GetString() then None else mismatch ()
    | Text.Json.JsonValueKind.Number, (:? JyroNumber as n) ->
        let mutable d = 0m
        let same =
            if n.IsExact && want.TryGetDecimal(&d) then n.DecimalValue = d
            else n.Value = want.GetDouble()
        if same then None else mismatch ()
    | Text.Json.JsonValueKind.Array, (:? JyroArray as a) ->
        let ws = want.EnumerateArray() |> List.ofSeq
        let gs = List.ofSeq a
        if ws.Length <> gs.Length then
            Some (sprintf "at %s: expected %d elements %s, got %d %s" path ws.Length (want.GetRawText()) gs.Length (show got))
        else
            List.zip ws gs |> List.indexed |> List.tryPick (fun (i, (w, g)) -> differs (sprintf "%s[%d]" path i) w g)
    | Text.Json.JsonValueKind.Object, (:? JyroObject as o) ->
        let ws = want.EnumerateObject() |> Seq.map (fun p -> p.Name, p.Value) |> List.ofSeq
        let gs = o |> Seq.map (fun kv -> kv.Key, kv.Value) |> List.ofSeq
        if List.map fst ws <> List.map fst gs then
            Some (sprintf "at %s: expected keys %A in that order, got %A (%s)" path (List.map fst ws) (List.map fst gs) (show got))
        else
            List.zip ws gs |> List.tryPick (fun ((k, w), (_, g)) -> differs (sprintf "%s.%s" path k) w g)
    | _ -> Some (sprintf "at %s: expected %s, got a %s %s" path (want.GetRawText()) kind (show got))

/// Assert that `actual` is the value that the JSON text `expected` describes:
/// element order, key order, strings, booleans and null as written, and each
/// number by its value alone (see `differs`), never by the text the
/// implementation writes for it.
let assertJson (expected: string) (actual: JyroValue) : unit =
    use doc = Text.Json.JsonDocument.Parse(expected)
    match differs "$" doc.RootElement actual with
    | None -> ()
    | Some d -> Values.fail (sprintf "Expected %s, got %s (%s)" expected (show actual) d)

/// Run cases, collect every failure, and fail once listing all of them, so one
/// test reports every value that breaks a clause.
let checkAll (cases: (string * (unit -> unit)) list) : unit =
    let failures =
        cases
        |> List.choose (fun (name, f) ->
            try f (); None
            with e -> Some (sprintf "[%s] %s" name e.Message))
    if not failures.IsEmpty then
        Values.fail (sprintf "%d of %d cases failed:\n%s" failures.Length cases.Length (String.concat "\n---\n" failures))

/// `Output` of a run with the defaults; fails the test unless it succeeded.
let out (source: string) : JyroValue = Script.outputOf source

/// The property `name` of `Output` after running `source` with the defaults.
let outProp (name: string) (source: string) : JyroValue = Values.prop name (out source)

/// Run `Output.r = <expr>` (after `prelude`) and return `Output.r`.
let evalWith (prelude: string) (expr: string) : JyroValue =
    outProp "r" (sprintf "%s\nOutput.r = %s" prelude expr)

/// Run `Output.r = <expr>` and return `Output.r`.
let eval (expr: string) : JyroValue = evalWith "" expr

/// Each `(expr, expectedJson)`: `expr` evaluates, with no error, to that value.
let evalsTo (prelude: string) (cases: (string * string) list) : unit =
    cases
    |> List.map (fun (expr, expected) -> expr, (fun () -> assertJson expected (evalWith prelude expr)))
    |> checkAll

/// Each script `expr` is `true` (run as `Output.r = <expr>` after `prelude`).
let allTrue (prelude: string) (exprs: string list) : unit =
    exprs
    |> List.map (fun e -> e, (fun () ->
        let v = evalWith prelude e
        if show v <> "true" then Values.fail (sprintf "Expected true, got %s" (show v))))
    |> checkAll

/// Each `expr` fails with exactly one error, of code `code`.
let errorsWith (code: MessageCode) (prelude: string) (exprs: string list) : unit =
    exprs
    |> List.map (fun e -> e, (fun () ->
        Script.expectError code (Script.run (sprintf "%s\nOutput.r = %s" prelude e))))
    |> checkAll

/// Each `expr` fails with exactly one error whose `MessageCode` member is named
/// `codeName`. For a code the book names but gives no number, and which this
/// build may not declare (so naming the member would stop the project compiling).
let errorsNamed (codeName: string) (prelude: string) (exprs: string list) : unit =
    exprs
    |> List.map (fun e -> e, (fun () ->
        let r = Script.run (sprintf "%s\nOutput.r = %s" prelude e)
        let code = Script.onlyError r
        let name = System.Enum.GetName(typeof<MessageCode>, code)
        if name <> codeName then
            Values.fail (sprintf "Expected the one error %s, got %A (%d)%s. Diagnostics:\n%s"
                            codeName code (int code)
                            (if isNull (System.Enum.GetNames(typeof<MessageCode>) |> Array.tryFind ((=) codeName) |> Option.toObj)
                             then sprintf "; MessageCode declares no member %s in this build" codeName else "")
                            (Script.describe r))))
    |> checkAll

/// Each `expr` runs with no error (its value is not judged: a control that
/// another clause gives the value of).
let allSucceed (prelude: string) (exprs: string list) : unit =
    exprs
    |> List.map (fun e -> e, (fun () ->
        let r = Script.run (sprintf "%s\nOutput.r = %s" prelude e)
        if not (Script.ok r) then Values.fail (sprintf "Expected success, got:\n%s" (Script.describe r))))
    |> checkAll

/// Each `expr` evaluates to an exact number (30.2.3).
let allExact (prelude: string) (exprs: string list) : unit =
    exprs
    |> List.map (fun e -> e, (fun () ->
        let v = evalWith prelude e
        if not (Values.isExact v) then Values.fail (sprintf "%s is not exact" (show v))))
    |> checkAll

/// `TypeOf` of each expression's value is `expected`.
let typesAre (expected: string) (exprs: string list) : unit =
    exprs
    |> List.map (fun e -> e, (fun () -> assertJson (sprintf "\"%s\"" expected) (eval (sprintf "TypeOf(%s)" e))))
    |> checkAll

// -- Values that are not arrays ------------------------------------------------

/// Script text for a value of every type that is not an `array` and not `null`:
/// booleans, numbers, strings (a string is not an array), objects, a function value.
let notArrayOrNull : string list =
    [ "true"; "false"; "0"; "1.5"; "\"\""; "\"abc\""; "{}"; "{a: 1}"; "x => x" ]

/// Every value of `notArrayOrNull` as the argument that `callOf` places raises
/// `ArgumentTypeMismatch`; each control call succeeds, so the type alone decides.
let typeMismatchFor (callOf: string -> string) (controls: string list) : unit =
    let bad =
        notArrayOrNull
        |> List.map (fun v -> callOf v, (fun () ->
            Script.expectError MessageCode.ArgumentTypeMismatch (Script.run (sprintf "Output.r = %s" (callOf v)))))
    let good =
        controls
        |> List.map (fun c -> "control " + c, (fun () ->
            let r = Script.run (sprintf "Output.r = %s" c)
            if not (Script.ok r) then Values.fail (sprintf "control failed:\n%s" (Script.describe r))))
    checkAll (bad @ good)

// -- The standard library, from the host ---------------------------------------

/// The standard library function `name` (`StdlibRegistry.TryGetFunction`).
let stdlib (name: string) : IJyroFunction =
    // StdlibRegistry is an F# module: its public API names (GetAll,
    // TryGetFunction) are compiled names; from F# the module functions are
    // reached by their source names.
    match StdlibRegistry.getAll () |> Seq.tryFind (fun f -> f.Name = name) with
    | Some f -> f
    | None -> Values.fail (sprintf "StdlibRegistry has no function %s" name)

/// The function's parameters are these names, in this order, each optional or
/// not as given (90.2.2).
let assertSignature (name: string) (parameters: (string * bool) list) : unit =
    let actual =
        (stdlib name).Signature.Parameters
        |> Seq.map (fun p -> p.Name, p.IsOptional)
        |> List.ofSeq
    if actual <> parameters then
        Values.fail (sprintf "%s parameters: expected %A, got %A" name parameters actual)

/// The function declares exactly `Pure`; a compilation with maximum effect
/// `Pure` accepts `call`; two calls with equal arguments give equal results.
let assertPure (name: string) (call: string) : unit =
    let f = stdlib name
    Assert.True(f.Effect = FunctionEffect.Pure, sprintf "%s declares %A, not Pure" name f.Effect)
    Assert.False(f.Effect = FunctionEffect.NonDeterministic || f.Effect = FunctionEffect.Blocking || f.Effect = FunctionEffect.Mutating)
    let compiled =
        Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } (sprintf "Output.r = %s" call)
    Assert.True(Script.ok compiled, Script.describe compiled)
    Assert.DoesNotContain(MessageCode.FunctionEffectExceedsMax, Script.codes compiled)
    let o = out (sprintf "Output.a = %s\nOutput.b = %s\nOutput.eq = Output.a == Output.b" call call)
    Assert.Equal("true", show (Values.prop "eq" o))

// -- Input (read-only under 70.2.1) and host-supplied Output -------------------

/// Run with `Input` given as a host `JyroValue` made from JSON text (so it is
/// read-only under 70.2.1 during the run). A fresh value for each run.
let runOnInput (inputJson: string) (source: string) : JyroResult<JyroValue> =
    Script.runWith { Script.defaults with Input = ValueInput (Values.json inputJson) } source

/// As `runOnInput`, asserting success and no `InputValueIsReadOnly`, returning `Output`.
let outOnInput (inputJson: string) (source: string) : JyroValue =
    let r = runOnInput inputJson source
    Assert.DoesNotContain(MessageCode.InputValueIsReadOnly, Script.codes r)
    Script.output r

/// True when a value is read-only (`IsFrozen`).
let frozen (v: JyroValue) : bool = v.IsFrozen

// -- Templates shared by the functions that return a new array ------------------

/// "Returns a new array": the argument `literal` (JSON text, also valid Jyro) is
/// given to the call that `callOf` makes. In the script, after writes to the
/// result (replace element 0, append one), the argument is equal to its clone
/// taken before the call and has its old length. From the host, the result is
/// not the argument's instance.
let assertNewArray (literal: string) (callOf: string -> string) : unit =
    let script =
        sprintf "var a = %s\nvar before = Clone(a)\nvar n = Length(a)\nvar r = %s\nr[0] = 99\nr[Length(r)] = 100\nOutput.same = a == before\nOutput.len = Length(a) == n\nOutput.r = r"
            literal (callOf "a")
    let o = out script
    Assert.Equal("true", show (Values.prop "same" o))
    Assert.Equal("true", show (Values.prop "len" o))
    let arg = Values.json literal
    let result =
        Script.runWith
            { Script.defaults with Output = Some (Values.obj [ "a", arg ]) }
            (sprintf "Output.r = %s" (callOf "Output.a"))
    let r = Values.prop "r" (Script.output result)
    Assert.False(obj.ReferenceEquals(r, arg), "the result is the argument's own array instance")

/// "Does not change the argument": the argument equals its clone taken before
/// the call, has the same length, and the call on a read-only `Input` succeeds.
let assertUnchanged (literal: string) (callOf: string -> string) : unit =
    let script =
        sprintf "var a = %s\nvar before = Clone(a)\nvar n = Length(a)\nvar r = %s\nOutput.same = a == before\nOutput.len = Length(a) == n"
            literal (callOf "a")
    let o = out script
    Assert.Equal("true", show (Values.prop "same" o))
    Assert.Equal("true", show (Values.prop "len" o))
    let o2 = outOnInput (sprintf "{\"a\": %s}" literal) (sprintf "var r = %s\nOutput.ok = true" (callOf "Input.a"))
    Assert.Equal("true", show (Values.prop "ok" o2))

/// "Each element is the element itself": an object `o` and an array `n` passed
/// in the argument come back at position `index` of the result as the same
/// values: a write through the result is seen through `o` and `n`.
let assertElementsShared (callOf: string -> string) (index: int) : unit =
    let script =
        sprintf "var o = {k: 1}\nvar r1 = %s\nr1[%d].m = 2\nOutput.om = o.m\nvar n = [1]\nvar r2 = %s\nr2[%d][0] = 5\nOutput.n0 = n[0]"
            (callOf "o") index (callOf "n") index
    let o = out script
    assertJson "2" (Values.prop "om" o)
    assertJson "5" (Values.prop "n0" o)

/// "Gives its result on a read-only argument": the call on `Input.a` succeeds
/// with no `InputValueIsReadOnly`, and equals the call on a script-built copy.
let assertReadOnlyArgument (inputJson: string) (callOf: string -> string) : unit =
    let o =
        outOnInput inputJson
            (sprintf "Output.r = %s\nvar copy = Clone(Input.a)\nOutput.c = %s\nOutput.eq = Output.r == Output.c" (callOf "Input.a") (callOf "copy"))
    Assert.Equal("true", show (Values.prop "eq" o))

/// "The result is not read-only": on a read-only `Input.a`, writes to the result
/// succeed, and from the host the result is not frozen. When `emptyCall` is
/// given, it is a call on `Input` that gives an empty array, written at 0.
let assertResultWritable (inputJson: string) (callOf: string -> string) (emptyCall: string option) : unit =
    let emptyPart =
        match emptyCall with
        | Some c -> sprintf "\nvar e = %s\ne[0] = 1\nOutput.e = e" c
        | None -> ""
    let o =
        outOnInput inputJson
            (sprintf "var r = %s\nr[0] = 99\nr[Length(r)] = 100\nOutput.r = r%s" (callOf "Input.a") emptyPart)
    let r = Values.prop "r" o
    Assert.False(frozen r, "the result array is frozen (read-only)")
    assertJson "99" (Values.asList r).Head
    match emptyCall with
    | Some _ ->
        let e = Values.prop "e" o
        Assert.False(frozen e, "the empty result array is frozen (read-only)")
        assertJson "[1]" e
    | None -> ()

/// The ids (`id` property) of the elements of `Output.r`, in order.
let ids (v: JyroValue) : string list =
    Values.asList v |> List.map (fun e -> show (Values.prop "id" e))

/// A deep array built in the script: `d` nested `depth` arrays deep around `1`.
let deepArrayScript (name: string) (depth: int) : string =
    sprintf "var %s = 1\nvar i_%s = 0\nwhile i_%s < %d do\n  %s = [%s]\n  i_%s = i_%s + 1\nend" name name name depth name name name name

/// Limits wide enough for a loop of `n` iterations.
let loopLimits (n: int) : JyroExecutionOptions =
    Script.limits (fun o ->
        { o with
            MaxLoopIterations = max o.MaxLoopIterations (n + 100)
            MaxStatements = max o.MaxStatements (n * 6 + 1000) })

/// Run with limits for loops of up to `n` iterations.
let runLoops (n: int) (source: string) : JyroResult<JyroValue> =
    Script.runWith { Script.defaults with Limits = Some (loopLimits n) } source

/// The value nesting bound (100.7.1), as the implementation documents it.
/// The value nesting maximum of the reference implementation (JYRO-REQ-2042 states it: 128).
let maxDepth () : int = 128

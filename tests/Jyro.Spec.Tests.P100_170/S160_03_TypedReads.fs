/// 160.3.9 to 160.3.11: Get<T> is a typed read, GetOptional<T>
/// treats null as not given, and Get<T> reads a missing argument as null.
module RealTree.Jyro.Spec.Tests.S160_03_TypedReads

open System
open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// A host function `Read(a, b)` (both optional, any type) that reads the
/// argument at `index` with Get<T> for the T that `kind` names, or with
/// GetOptional<T> when `optional` is set (default: the string "DEF" or
/// the number 42). It records the arguments it was given and what it read.
type TypedRead(kind: string, index: int, optional: bool) =
    inherit JyroFunctionBase(
        "Read",
        Host.signature "Read"
            [ Parameter.Optional("a", ParameterType.Any); Parameter.Optional("b", ParameterType.Any) ]
            ParameterType.Any)

    member val Given : JyroValue list = [] with get, set
    member val Returned : obj = null with get, set
    member val Default : JyroValue = Unchecked.defaultof<JyroValue> with get, set

    override _.Effect = FunctionEffect.Pure

    override this.Execute(args: IReadOnlyList<JyroValue>, _ctx) =
        this.Given <- List.ofSeq args
        let got : JyroValue =
            if optional then
                let d : JyroValue =
                    match kind with
                    | "number" -> JyroNumber(42) :> JyroValue
                    | "boolean" -> JyroBoolean.True :> JyroValue
                    | "array" -> JyroArray() :> JyroValue
                    | "object" -> JyroObject() :> JyroValue
                    | _ -> JyroString("DEF") :> JyroValue
                this.Default <- d
                match kind with
                | "string" -> args.GetOptional<JyroString>(index, d :?> JyroString) :> JyroValue
                | "number" -> args.GetOptional<JyroNumber>(index, d :?> JyroNumber) :> JyroValue
                | "boolean" -> args.GetOptional<JyroBoolean>(index, d :?> JyroBoolean) :> JyroValue
                | "array" -> args.GetOptional<JyroArray>(index, d :?> JyroArray) :> JyroValue
                | "object" -> args.GetOptional<JyroObject>(index, d :?> JyroObject) :> JyroValue
                | "value" -> args.GetOptional<JyroValue>(index, d)
                | other -> failwithf "unknown kind %s" other
            else
                match kind with
                | "string" -> args.Get<JyroString>(index) :> JyroValue
                | "number" -> args.Get<JyroNumber>(index) :> JyroValue
                | "boolean" -> args.Get<JyroBoolean>(index) :> JyroValue
                | "array" -> args.Get<JyroArray>(index) :> JyroValue
                | "object" -> args.Get<JyroObject>(index) :> JyroValue
                | "null" -> args.Get<JyroNull>(index) :> JyroValue
                | "value" -> args.Get<JyroValue>(index)
                | other -> failwithf "unknown kind %s" other
        this.Returned <- box got
        if isNull (box got) then JyroNull.Instance :> JyroValue else got

/// Run `Output.r = <call>` with a TypedRead.
let private runRead (fn: TypedRead) (call: string) : JyroResult<JyroValue> =
    Script.runWith { pinned with Functions = [ fn :> IJyroFunction ] } (sprintf "Output.r = %s" call)

/// The Jyro type name of each kind.
let private jyroType (kind: string) = kind

/// Assert `r` failed with only ArgumentTypeMismatch, at stage Execute, whose
/// arguments name `expected` and then `actual` as TypeOf spells them, and no
/// class name of the API (130.6.5, 160.3.9, 160.3.11).
let private mismatch (expected: string) (actual: string) (r: JyroResult<JyroValue>) : unit =
    let d = Script.onlyErrorMessage r
    Assert.Equal(MessageCode.ArgumentTypeMismatch, d.Code)
    Assert.Equal("Execute", Pending.stageOf d)
    assertTypeNameArgs expected actual d

// -- 160.3.9 Get<T> is a typed read -------------------------------------------------------

let ofItsType : obj[] seq =
    [ "string", "\"a\""; "number", "1.5"; "boolean", "true"; "array", "[1]"; "object", "{k: 1}"; "null", "null"
      "value", "\"a\""; "value", "1.5"; "value", "false"; "value", "[1]"; "value", "{k: 1}"; "value", "null" ]
    |> Seq.map (fun (k, a) -> [| box k; box a |])

[<Theory>]
[<MemberData(nameof ofItsType)>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_OfAValueOfTypeT_ReturnsItAsGiven`` (kind: string, arg: string) =
    let fn = TypedRead(kind, 0, false)
    ok (runRead fn (sprintf "Read(%s)" arg)) |> ignore
    Assert.Same(box fn.Given.[0], fn.Returned)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 GetOfJyroValue_ForNull_ReturnsNull`` () =
    let fn = TypedRead("value", 0, false)
    let out = ok (runRead fn "Read(null)")
    Assert.IsType<JyroNull>(fn.Returned) |> ignore
    Assert.Equal("null", Values.toJson (Values.prop "r" out))

let ofAnotherType : obj[] seq =
    [ // 160.3.1 does not apply: null is not the empty value.
      "string", "null", "null"; "array", "null", "null"; "object", "null", "null"
      // 160.3.7 does not apply: a number or boolean is not read as its string form.
      "string", "1.5", "number"; "string", "true", "boolean"
      // 160.3.2 does not apply, but null is refused under this clause.
      "number", "null", "null"; "boolean", "null", "null"
      "number", "\"1\"", "string"; "boolean", "1", "number"
      "null", "1", "number"; "null", "\"\"", "string"
      "array", "{}", "object"; "object", "[]", "array"; "string", "[]", "array"; "number", "x => x", "function" ]
    |> Seq.map (fun (k, a, t) -> [| box k; box a; box t |])

[<Theory>]
[<MemberData(nameof ofAnotherType)>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_OfAValueOfAnotherType_RaisesArgumentTypeMismatchNamingBothJyroTypes`` (kind: string, arg: string, actual: string) =
    let fn = TypedRead(kind, 0, false)
    runRead fn (sprintf "Read(%s)" arg) |> mismatch (jyroType kind) actual

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_TheBookExamples`` () =
    runRead (TypedRead("string", 0, false)) "Read(null)" |> mismatch "string" "null"
    runRead (TypedRead("string", 0, false)) "Read(1.5)" |> mismatch "string" "number"
    let fn = TypedRead("value", 0, false)
    ok (runRead fn "Read(null)") |> ignore
    Assert.IsType<JyroNull>(fn.Returned) |> ignore

// -- 160.3.10 GetOptional<T> treats null as not given -------------------------------------

[<Theory>]
[<InlineData("string")>]
[<InlineData("number")>]
[<InlineData("boolean")>]
[<InlineData("array")>]
[<InlineData("object")>]
[<InlineData("value")>]
[<Trait("requirement", "JYRO-REQ-1937")>]
let ``JYRO-REQ-1937 GetOptional_NotGivenOrNull_ReturnsTheDefault`` (kind: string) =
    for call in [ "Read(1)"; "Read(1, null)"; "Read(a: 1, b: null)" ] do
        let fn = TypedRead(kind, 1, true)
        ok (runRead fn call) |> ignore
        Assert.Same(box fn.Default, fn.Returned)
    let fn = TypedRead(kind, 1, true)
    Script.runWith { Script.defaults with Functions = [ fn :> IJyroFunction ] } "var n = null\nOutput.r = Read(1, n)" |> ok |> ignore
    Assert.Same(box fn.Default, fn.Returned)

let optionalOfItsType : obj[] seq =
    [ "string", "\"a\""; "number", "1.5"; "boolean", "false"; "array", "[1]"; "object", "{k: 1}"
      "value", "\"a\""; "value", "0"; "value", "[]" ]
    |> Seq.map (fun (k, a) -> [| box k; box a |])

[<Theory>]
[<MemberData(nameof optionalOfItsType)>]
[<Trait("requirement", "JYRO-REQ-1937")>]
let ``JYRO-REQ-1937 GetOptional_OfAValueOfTypeT_ReturnsItAsGiven`` (kind: string, arg: string) =
    let fn = TypedRead(kind, 1, true)
    ok (runRead fn (sprintf "Read(1, %s)" arg)) |> ignore
    Assert.Same(box fn.Given.[1], fn.Returned)

let optionalOfAnotherType : obj[] seq =
    [ "string", "1.5", "number"; "string", "true", "boolean"; "string", "[1]", "array"
      "number", "\"1\"", "string"; "boolean", "0", "number"; "array", "{}", "object"; "object", "[]", "array" ]
    |> Seq.map (fun (k, a, t) -> [| box k; box a; box t |])

[<Theory>]
[<MemberData(nameof optionalOfAnotherType)>]
[<Trait("requirement", "JYRO-REQ-1937")>]
let ``JYRO-REQ-1937 GetOptional_OfAValueOfAnotherType_RaisesAndDoesNotReturnTheDefault`` (kind: string, arg: string, actual: string) =
    let fn = TypedRead(kind, 1, true)
    runRead fn (sprintf "Read(1, %s)" arg) |> mismatch kind actual
    Assert.Null(fn.Returned)

// -- 160.3.11 Get<T> reads a missing argument as null ----------------------------------------

/// Calls whose args hold fewer elements than the index read: (call, index).
let private missingCalls = [ "Read(1)", 1; "Read(1)", 5; "Read()", 0; "Read(1, 2)", 2 ]

let missingForNullableT : obj[] seq =
    [ for kind in [ "value"; "null" ] do
        for (call, i) in missingCalls -> [| box kind; box call; box i |] ]

[<Theory>]
[<MemberData(nameof missingForNullableT)>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_OfAMissingArgument_ReturnsNull_WhenTIsJyroValueOrNull`` (kind: string, call: string, index: int) =
    let fn = TypedRead(kind, index, false)
    let out = ok (runRead fn call)
    Assert.True(index >= fn.Given.Length || fn.Given.[index] :? JyroNull, "the argument is present")
    Assert.IsType<JyroNull>(fn.Returned) |> ignore
    Assert.Equal("null", Values.toJson (Values.prop "r" out))

let missingForOtherT : obj[] seq =
    [ for kind in [ "string"; "number"; "boolean"; "array"; "object" ] do
        for (call, i) in missingCalls -> [| box kind; box call; box i |] ]

[<Theory>]
[<MemberData(nameof missingForOtherT)>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_OfAMissingArgument_RaisesArgumentTypeMismatchNamingTheTypeAndNull`` (kind: string, call: string, index: int) =
    let fn = TypedRead(kind, index, false)
    // No .NET index exception reaches the host: the run ends with the Jyro error.
    runRead fn call |> mismatch kind "null"

[<Theory>]
[<InlineData("string")>]
[<InlineData("number")>]
[<InlineData("boolean")>]
[<InlineData("array")>]
[<InlineData("object")>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_OfAMissingArgument_GivesTheSameDiagnosticAsAGivenNull`` (kind: string) =
    let missing = Script.onlyErrorMessage (runRead (TypedRead(kind, 1, false)) "Read(1)")
    let given = Script.onlyErrorMessage (runRead (TypedRead(kind, 1, false)) "Read(1, null)")
    Assert.Equal(given.Code, missing.Code)
    Assert.Equal<string[]>(given.Args |> Array.map string, missing.Args |> Array.map string)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1936")>]
let ``JYRO-REQ-1936 Get_TheBookExamples_AndJustInsideTheBound`` () =
    // With one argument in args.
    runRead (TypedRead("string", 1, false)) "Read(1)" |> mismatch "string" "null"
    let v = TypedRead("value", 1, false)
    ok (runRead v "Read(1)") |> ignore
    Assert.IsType<JyroNull>(v.Returned) |> ignore
    // i = count - 1 reads the given argument.
    let inside = TypedRead("string", 1, false)
    ok (runRead inside "Read(1, \"a\")") |> ignore
    Assert.Same(box inside.Given.[1], inside.Returned)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1937")>]
let ``JYRO-REQ-1937 GetOptional_OfAMissingArgument_StillReturnsItsDefault`` () =
    for (call, i) in missingCalls do
        let fn = TypedRead("string", i, true)
        ok (runRead fn call) |> ignore
        Assert.Same(box fn.Default, fn.Returned)

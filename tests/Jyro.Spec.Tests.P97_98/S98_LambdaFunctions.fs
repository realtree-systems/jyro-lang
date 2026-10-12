/// 98: Map, Where, Reduce, Each, Find, All, Any and SortBy.
module RealTree.Jyro.Spec.Tests.S98_LambdaFunctions

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- Building calls ------------------------------------------------------------------

/// A lambda of the arity the function passes: two for Reduce, one otherwise.
let lam (fn: string) (body: string) =
    if fn = "Reduce" then "(acc, x) => " + body else "x => " + body

/// A call of `fn` over `items` with `callback` (Reduce with initial 0).
let callL (fn: string) (items: string) (callback: string) =
    if fn = "Reduce" then sprintf "Reduce(%s, %s, 0)" items callback
    else sprintf "%s(%s, %s)" fn items callback

/// A body that keeps the function going over every element: falsy for Find and
/// Any, truthy for All, the body itself for the others.
let continuing (fn: string) (body: string) =
    match fn with
    | "Find"
    | "Any" -> sprintf "(%s) == \"never\"" body
    | "All" -> sprintf "(%s) != \"never\"" body
    | _ -> body

/// The result of `fn` for an empty items (Reduce with initial 0).
let emptyL (fn: string) =
    match fn with
    | "Reduce" -> "0"
    | "Each"
    | "Find" -> "null"
    | "All" -> "true"
    | "Any" -> "false"
    | _ -> "[]"

let lambdaFnData : obj[] seq = lambdaFns |> Seq.map (fun f -> [| box f |])

let oneArgFnData : obj[] seq =
    lambdaFns |> List.filter (fun f -> f <> "Reduce") |> Seq.map (fun f -> [| box f |])

/// A host function that raises a runtime error of a chosen code and arguments.
let boom () =
    Host.func "Boom" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
        raise (JyroError.Runtime(MessageCode.InvalidRange, box "boom", box 7)))

/// A Mutating host function Push(array, value): appends while the array is shorter than 10.
let push () =
    Host.func "Push" [ Parameter.Required("xs", ParameterType.Any); Parameter.Required("v", ParameterType.Any) ]
        ParameterType.Any FunctionEffect.Mutating (fun args ->
            match args with
            | (:? JyroArray as a) :: v :: _ when a.Count < 10 -> a.Add(v)
            | _ -> ()
            Values.nul)

/// A Mutating host function Put(array, index, value): replaces an element.
let put () =
    Host.func "Put"
        [ Parameter.Required("xs", ParameterType.Any)
          Parameter.Required("i", ParameterType.Number)
          Parameter.Required("v", ParameterType.Any) ]
        ParameterType.Any FunctionEffect.Mutating (fun args ->
            match args with
            | (:? JyroArray as a) :: i :: v :: _ -> a.[int (Values.asFloat i)] <- v
            | _ -> ()
            Values.nul)

let private assertSameErrorAsOutside (options: RunOptions) (inside: string) (outside: string) (expectedCode: MessageCode option) =
    let i = onlyErrorOf options inside
    let o = onlyErrorOf options outside
    expectedCode |> Option.iter (fun c -> check (o.Code = c) (sprintf "(the error outside a callback is %A, not %A)" o.Code c))
    check (i.Code = o.Code)
        (sprintf "In a callback the error is %A, outside it is %A:\n%s" i.Code o.Code inside)
    check (argsText i = argsText o)
        (sprintf "In a callback the arguments are %A, outside they are %A:\n%s" (argsText i) (argsText o) inside)

// == 98.1 Shared rules =====================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1346")>]
let ``Map_ReturnsAnArray_AndTakesItemsAndCallback`` () =
    assertExpr "\"array\"" "TypeOf(Map([1, 2], x => x * 2))"
    assertExpr "\"array\"" "TypeOf(Map([], x => x))"
    assertSignature "Map" [ "items"; "callback" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1791")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1791 LambdaFunction_WhenCallbackIsNotAFunctionValue_RaisesCallbackExpected`` (fn: string) =
    for cb in [ "null"; "1"; "0"; "\"x => x\""; "true"; "false"; "[]"; "{}"; "[x => x]"; "{f: x => x}" ] do
        expectErr MessageCode.CallbackExpected ("Output.r = " + callL fn "[1, 2]" cb)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1791")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1791 LambdaFunction_WhenCallbackIsNullOrNotAFunctionValue_RaisesCallbackExpected_AlsoFromAVariableAndFromInput`` (fn: string) =
    // null held in a variable, a number held in input, and a null that comes from a property.
    expectErr MessageCode.CallbackExpected ("var cb = null\nOutput.r = " + callL fn "[1, 2]" "cb")
    expectErr MessageCode.CallbackExpected ("var o = {}\nOutput.r = " + callL fn "[1, 2]" "o.missing")
    let options = { Script.defaults with Input = JsonInput "{\"cb\": 7, \"nul\": null, \"s\": \"x\"}" }
    for name in [ "Input.cb"; "Input.nul"; "Input.s" ] do
        expectErrWith options MessageCode.CallbackExpected ("Output.r = " + callL fn "[1, 2]" name)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1791")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1791 LambdaFunction_WhenCallbackIsAFunctionValue_RaisesNoCallbackExpected`` (fn: string) =
    // A lambda held in a variable, and a function value from the host.
    let held = "var cb = " + lam fn (continuing fn "x") + "\nOutput.r = " + callL fn "[1, 2]" "cb"
    check (Script.run held).IsSuccess (sprintf "a lambda held in a variable was refused:\n%s" held)
    let p = FunctionValueProbe((if fn = "Reduce" then 2 else 1), fun _ -> (match fn with "All" -> Values.bool true | "Find" | "Any" -> Values.bool false | _ -> Values.nul))
    let r = Script.runWith (withFunctions [ p.Maker ]) ("var p = MakeProbe()\nOutput.r = " + callL fn "[1, 2]" "p")
    check r.IsSuccess (sprintf "a host function value was refused as the callback of %s:\n%s" fn (Script.describe r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1348")>]
[<MemberData(nameof oneArgFnData)>]
let ``LambdaFunction_CallsTheCallbackWithExactlyTheElement_NeverThePosition`` (fn: string) =
    let reply =
        match fn with
        | "Find"
        | "Any" -> fun (_: JyroValue list) -> Values.bool false
        | "All" -> fun _ -> Values.bool true
        | _ -> fun args -> (match args with a :: _ -> a | [] -> Values.nul)
    let p = FunctionValueProbe(1, reply)
    let source = sprintf "var p = MakeProbe()\nOutput.r = %s([\"a\", \"b\", \"c\"], p)" fn
    runR (withFunctions [ p.Maker ]) source |> ignore
    let calls = p.Calls |> List.map (List.map Values.toJson)
    let elements = [ norm "\"a\""; norm "\"b\""; norm "\"c\"" ]
    // Every call has exactly one argument, and it is an element (never a position).
    for c in calls do
        check (List.length c = 1) (sprintf "%s called the callback with %d arguments: %A" fn (List.length c) c)
        check (List.contains c.Head elements) (sprintf "%s gave the callback %s, which is not an element" fn c.Head)
    // Every element was given.
    let given = calls |> List.map List.head |> List.distinct
    check (given = elements) (sprintf "%s gave the callback %A, expected the elements %A" fn given elements)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1348")>]
let ``Map_ScriptCallback_ReceivesTheElement`` () =
    assertExpr "[\"a\", \"b\"]" "Map([\"a\", \"b\"], x => x)"
    assertExpr "[10, 20]" "Map([10, 20], x => x)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1792")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1792 LambdaFunction_WhenCallbackArityIsWrong_RaisesCallbackArityMismatch`` (fn: string) =
    let wrong =
        if fn = "Reduce" then [ "() => 1"; "x => x"; "(a, b, c) => a" ]
        else [ "() => 1"; "(a, b) => a"; "(a, b, c) => a" ]
    for cb in wrong do
        expectErr MessageCode.CallbackArityMismatch ("Output.r = " + callL fn "[1, 2]" cb)
    let right = lam fn (continuing fn "x")
    let result = Script.run ("Output.r = " + callL fn "[1, 2]" right)
    check result.IsSuccess (sprintf "%s rejected a callback of the right arity:\n%s" fn (Script.describe result))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1792")>]
let ``JYRO-REQ-1792 Map_HostFunctionValueOfWrongParamCount_RaisesCallbackArityMismatch`` () =
    for n in [ 0; 2 ] do
        let p = FunctionValueProbe(n, fun _ -> Values.nul)
        expectErrWith (withFunctions [ p.Maker ]) MessageCode.CallbackArityMismatch
            "var p = MakeProbe()\nOutput.r = Map([1, 2], p)"
        check p.Calls.IsEmpty (sprintf "a function value of %d parameters was called" n)
    let ok = FunctionValueProbe(2, fun _ -> Values.nul)
    runR (withFunctions [ ok.Maker ]) "var p = MakeProbe()\nOutput.r = Reduce([1, 2], p, 0)" |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1792")>]
let ``JYRO-REQ-1792 TheClausesExamples_MapWithTwoParameters_AndReduceWithOne_RaiseCallbackArityMismatch`` () =
    expectErr MessageCode.CallbackArityMismatch "Output.r = Map([1], (a, b) => a)"
    expectErr MessageCode.CallbackArityMismatch "Output.r = Reduce([1], x => x, 0)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1792")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1792 LambdaFunction_WithACallbackOfTheRightArity_RaisesNoCallbackArityMismatch`` (fn: string) =
    // The declared number of parameters is the one the function passes: two for Reduce, one for the others.
    let right = lam fn (continuing fn "x")
    let result = Script.run ("Output.r = " + callL fn "[1, 2]" right)
    check result.IsSuccess (sprintf "%s rejected a callback of the right arity:\n%s" fn (Script.describe result))
    // A lambda held in a variable is checked the same way, and a host function value of the right count too.
    let wrong = if fn = "Reduce" then "x => x" else "(a, b) => a"
    expectErr MessageCode.CallbackArityMismatch ("var cb = " + wrong + "\nOutput.r = " + callL fn "[1, 2]" "cb")
    let ok = FunctionValueProbe((if fn = "Reduce" then 2 else 1), fun _ -> (match fn with "All" -> Values.bool true | "Find" | "Any" -> Values.bool false | _ -> Values.nul))
    let r = Script.runWith (withFunctions [ ok.Maker ]) ("var p = MakeProbe()\nOutput.r = " + callL fn "[1, 2]" "p")
    check r.IsSuccess (sprintf "%s rejected a host function value of the right arity:\n%s" fn (Script.describe r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1350")>]
[<MemberData(nameof lambdaFnData)>]
let ``LambdaFunction_ChecksTheCallbackBeforeTheFirstElement_AlsoForEmptyAndNullItems`` (fn: string) =
    let wrongArity = if fn = "Reduce" then "x => x" else "(a, b) => a"
    for items in [ "[1, 2]"; "[]"; "null" ] do
        for cb in [ "null"; "5"; "\"f\""; "[]"; "{}" ] do
            expectErr MessageCode.CallbackExpected ("Output.r = " + callL fn items cb)
        expectErr MessageCode.CallbackArityMismatch ("Output.r = " + callL fn items wrongArity)
        expectErr MessageCode.CallbackArityMismatch ("Output.r = " + callL fn items "() => 1")
    // A callback of the wrong arity is rejected before any element could be given to it.
    let wrongWithError = if fn = "Reduce" then "x => 10 / 0" else "(a, b) => 10 / 0"
    expectErr MessageCode.CallbackArityMismatch ("Output.r = " + callL fn "[1]" wrongWithError)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1350")>]
let ``LambdaFunction_ChecksItemsBeforeTheCallback_AndCallbackTypeBeforeArity`` () =
    expectErr MessageCode.ArgumentTypeMismatch "Output.r = Map(5, null)"
    expectErr MessageCode.ArgumentTypeMismatch "Output.r = Map({}, (a, b) => a)"
    expectErr MessageCode.ArgumentTypeMismatch "Output.r = Reduce(\"x\", x => x, 0)"
    expectErr MessageCode.CallbackExpected "Output.r = Map([1], 5)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1794")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1794 LambdaFunction_NullItems_IsTreatedAsEmptyArray`` (fn: string) =
    let p = probe ()
    let cb = lam fn "Probe(x)"
    let source =
        if fn = "Reduce" then sprintf "Output.r = Reduce(null, %s, 7)" cb
        else "Output.r = " + callL fn "null" cb
    let expected = if fn = "Reduce" then "7" else emptyL fn
    assertJson expected (runR (withFunctions [ p ]) source) source
    check (p.CallCount = 0) (sprintf "%s called the callback for a null items" fn)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1794")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1794 LambdaFunction_NullItems_GivesTheResultOfAnEmptyArray_AlsoFromAVariableAndFromInput`` (fn: string) =
    let cb = lam fn (continuing fn "x")
    let withEmpty = "Output.r = " + (if fn = "Reduce" then sprintf "Reduce([], %s, 7)" cb else callL fn "[]" cb)
    let fromVariable = "var xs = null\nOutput.r = " + (if fn = "Reduce" then sprintf "Reduce(xs, %s, 7)" cb else callL fn "xs" cb)
    let a = evalR withEmpty
    assertJson (Values.toJson a) (evalR fromVariable) fromVariable
    let options = { Script.defaults with Input = JsonInput "{\"xs\": null}" }
    let fromInput = "Output.r = " + (if fn = "Reduce" then sprintf "Reduce(Input.xs, %s, 7)" cb else callL fn "Input.xs" cb)
    assertJson (Values.toJson a) (runR options fromInput) fromInput

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1796")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1796 LambdaFunction_ItemsNotArrayNorNull_RaisesArgumentTypeMismatch`` (fn: string) =
    for items in [ "{a: 1}"; "{}"; "1"; "0"; "1.5"; "\"abc\""; "\"\""; "true"; "false"; "(y => y)" ] do
        expectErr MessageCode.ArgumentTypeMismatch ("Output.r = " + callL fn items (lam fn "x"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1796")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1796 LambdaFunction_ItemsNotArrayNorNull_RaisesArgumentTypeMismatch_AlsoFromInput_AndNoCallbackRuns`` (fn: string) =
    let p = probe ()
    let options =
        { withFunctions [ p ] with Input = JsonInput "{\"o\": {\"a\": 1}, \"n\": 3, \"s\": \"abc\", \"b\": true}" }
    for name in [ "Input.o"; "Input.n"; "Input.s"; "Input.b" ] do
        expectErrWith options MessageCode.ArgumentTypeMismatch ("Output.r = " + callL fn name (lam fn "Probe(x)"))
    check (p.CallCount = 0) (sprintf "%s called the callback for an items that is not an array" fn)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1796")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1796 LambdaFunction_ItemsAnArrayOrNull_RaisesNoArgumentTypeMismatch`` (fn: string) =
    for items in [ "[]"; "[1, 2]"; "null"; "[[1], {a: 1}, null]" ] do
        let source = "Output.r = " + callL fn items (lam fn (continuing fn "x"))
        let result = Script.run source
        check result.IsSuccess (sprintf "%s refused an items of %s:\n%s" fn items (Script.describe result))

let private checkCallbackErrorPropagates (fn: string) =
    let p = probe ()
    let b = boom ()
    let options = withFunctions [ p; b ]
    // A standard library error: division by zero at the second element.
    let inside = "Output.r = " + callL fn "[1, 0, 2]" (lam fn (continuing fn "Probe(x) ?? 10 / x"))
    assertSameErrorAsOutside options inside "var z = 0\nOutput.r = 10 / z" (Some MessageCode.DivisionByZero)
    check (firstArgs p = [ "1"; "0" ]) (sprintf "%s gave the callback %A after it raised" fn (firstArgs p))
    // An error of a standard library function called in the callback.
    let inside2 =
        "Output.r = " + callL fn "[1, 0, 2]" (lam fn (continuing fn "x == 0 ? ToUpper([1]) : x"))
    assertSameErrorAsOutside options inside2 "Output.r = ToUpper([1])" None
    // A host function's runtime error, with its own code and arguments.
    let inside3 = "Output.r = " + callL fn "[1, 0, 2]" (lam fn (continuing fn "x == 0 ? Boom() : x"))
    assertSameErrorAsOutside options inside3 "Output.r = Boom()" (Some MessageCode.InvalidRange)
    let m = onlyErrorOf options inside3
    check (argsText m = [ "boom"; "7" ]) (sprintf "Boom's arguments became %A" (argsText m))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1797")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1797 LambdaFunction_WhenCallbackRaises_StopsAndPropagatesTheErrorUnchanged`` (fn: string) =
    checkCallbackErrorPropagates fn

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1797")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1797 LambdaFunction_WhenCallbackRaisesOnElement2Of5_IsNotCalledAgain_AndTheErrorKeepsItsCodeAndArguments`` (fn: string) =
    let p = probe ()
    let b = boom ()
    let options = withFunctions [ p; b ]
    let body = continuing fn "Probe(x) ?? (x == 2 ? Boom() : x)"
    let source = "Output.r = " + callL fn "[1, 2, 3, 4, 5]" (lam fn body)
    let m = onlyErrorOf options source
    check (m.Code = MessageCode.InvalidRange) (sprintf "%s: the error is %A, not the callback's InvalidRange" fn m.Code)
    check (argsText m = [ "boom"; "7" ]) (sprintf "%s: the arguments became %A" fn (argsText m))
    // The callback is called for element 1 and for element 2, and for no other element.
    check (firstArgs p = [ "1"; "2" ]) (sprintf "%s gave the callback %A, expected 1 and 2 only" fn (firstArgs p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1797")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1797 LambdaFunction_WhenCallbackRaisesOnTheFirstElement_IsNotCalledAgain`` (fn: string) =
    let p = probe ()
    let b = boom ()
    let body = continuing fn "Probe(x) ?? Boom()"
    expectErrWith (withFunctions [ p; b ]) MessageCode.InvalidRange ("Output.r = " + callL fn "[1, 2, 3]" (lam fn body))
    check (firstArgs p = [ "1" ]) (sprintf "%s gave the callback %A, expected the first element only" fn (firstArgs p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1354")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1354 LambdaFunction_GivesTheCallbackTheElementsItemsHadAtTheStart`` (fn: string) =
    let p = probe ()
    let options = withFunctions [ p; push (); put () ]
    let body = continuing fn "Probe(x) ?? Put(xs, 1, 99) ?? Push(xs, 0)"
    let source = sprintf "var xs = [1, 2, 3]\nvar r = %s\nOutput.r = Length(xs)" (callL fn "xs" (lam fn body))
    let length = runR options source |> Values.asFloat
    check (length > 3.0) "(the host functions did not change xs, so this test proves nothing)"
    // Which values the callback is given, in the order first given (98.1.9 does not
    // count the calls: 98.8.4 and the other count rules do).
    let given = firstArgs p |> List.distinct
    check (given = [ "1"; "2"; "3" ])
        (sprintf "%s gave the callback %A after items was changed in it, expected the values 1, 2, 3" fn (firstArgs p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1354")>]
let ``JYRO-REQ-1354 Map_WhenCallbackIncrementsAnElementOfItems_StillGivesTheOriginalElements`` () =
    let source = "var xs = [1, 2, 3]\nvar r = Map(xs, x => [x, xs[1]++])\nOutput.r = [Map(r, y => y[0]), xs]"
    assertJson "[[1, 2, 3], [1, 5, 3]]" (evalR source) source

/// A script-only change to items in the callback (xs[1]++ in the callback body,
/// no host function), observed through what Reduce, Find and Any compute from the
/// values given: with the elements at the start (1, 2, 3) each gives the first
/// answer; reading xs[1] live (3 after the first call) would give the second.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1354")>]
let ``JYRO-REQ-1354 ReduceFindAny_WhenCallbackIncrementsAnElementOfItems_StillGetTheOriginalElements`` () =
    let cases =
        [ "Reduce(xs, (acc, x) => acc + [x, xs[1]++][0], 0)", "6" // live: 1 + 3 + 3 = 7
          "Find(xs, x => [x, xs[1]++][0] == 2)", "2" // live: null
          "Any(xs, x => [x, xs[1]++][0] == 2)", "true" ] // live: false
    for (call, expected) in cases do
        let source = sprintf "var xs = [1, 2, 3]\nOutput.r = [%s, xs[1] > 2]" call
        assertJson (sprintf "[%s, true]" expected) (evalR source) source

/// The smallest call-depth limit at which one standard library call at the top level runs.
let private baseDepth () =
    let rec find n =
        if n > 20 then fail "ToUpper(\"a\") does not run at any call-depth limit up to 20"
        else
            let r = Script.runWith { Script.defaults with ConfigureRunner = fun r -> r.WithMaxCallDepth(n) } "Output.r = ToUpper(\"a\")"
            if r.IsSuccess then n else find (n + 1)
    find 1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1799")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1799 LambdaFunction_AtACallDepthLimitOf1_TheCallbackRuns_AndAScriptFunctionCallInItsBodyEndsWithCallDepthLimitExceeded`` (fn: string) =
    // The clause's examples, for each of the eight functions, with a call depth limit of 1.
    let atOne = { Script.defaults with ConfigureRunner = fun r -> r.WithMaxCallDepth(1) }
    let plain = "Output.r = " + callL fn "[1]" (lam fn (continuing fn "x"))
    let ok = Script.runWith atOne plain
    check ok.IsSuccess (sprintf "at a call depth limit of 1, %s did not succeed:\n%s\n%s" fn plain (Script.describe ok))
    let withG = "func G(x) return x end\nOutput.r = " + callL fn "[1]" (lam fn (continuing fn "G(x)"))
    expectErrWith atOne MessageCode.CallDepthLimitExceeded withG

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1799")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1799 LambdaFunction_CallingTheCallbackAddsNoCallDepth_CallsInItsBodyDo`` (fn: string) =
    let l = baseDepth ()
    let p = probe ()
    let at (n: int) =
        { withFunctions [ p ] with ConfigureRunner = fun r -> r.WithMaxCallDepth(n) }
    let src body = sprintf "func f(v)\n  return v\nend\nOutput.r = %s" (callL fn "[1, 2]" (lam fn (continuing fn body)))
    let succeeds n body =
        let r = Script.runWith (at n) (src body)
        check r.IsSuccess (sprintf "limit %d (base %d): expected success for\n%s\n%s" n l (src body) (Script.describe r))
    let exceeds n body = expectErrWith (at n) MessageCode.CallDepthLimitExceeded (src body)
    succeeds l "x"
    for body in [ "ToUpper(\"a\")"; "f(x)"; "Probe(x)" ] do
        exceeds l body
        succeeds (l + 1) body
    let nested = "Map([2], y => ToUpper(\"a\"))"
    exceeds (l + 1) nested
    succeeds (l + 2) nested

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1800")>]
[<MemberData(nameof lambdaFnData)>]
let ``JYRO-REQ-1800 LambdaFunction_IsPure_AndCallsInTheCallbackAreCheckedAgainstTheMaximumEffect`` (fn: string) =
    assertPureEffect fn ("Output.r = " + callL fn "[1]" (lam fn "x"))
    let tick = Host.recorderWithEffect "Tick" FunctionEffect.NonDeterministic
    let under effect source = Script.compileWith { withFunctions [ tick ] with MaxEffect = Some effect } source
    let source = "Output.r = " + callL fn "[1]" (lam fn "Tick(x)")
    let underPure = under FunctionEffect.Pure source
    check (not underPure.IsSuccess && Script.errorCodes underPure = [ MessageCode.FunctionEffectExceedsMax ])
        (sprintf "a NonDeterministic call in a callback compiled under Pure:\n%s\n%s" source (Script.describe underPure))
    let nd = under FunctionEffect.NonDeterministic source
    check nd.IsSuccess (sprintf "rejected under NonDeterministic:\n%s" (Script.describe nd))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1800")>]
let ``JYRO-REQ-1800 LambdaNeverPassed_WithACallAboveTheMaximum_IsRejectedWhereItAppears`` () =
    let tick = Host.recorderWithEffect "Tick" FunctionEffect.NonDeterministic
    let r =
        Script.compileWith { withFunctions [ tick ] with MaxEffect = Some FunctionEffect.Pure }
            "var g = x => Tick(x)\nOutput.r = 1"
    check (not r.IsSuccess && Script.errorCodes r = [ MessageCode.FunctionEffectExceedsMax ]) (Script.describe r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1800")>]
[<InlineData("Now()")>]
[<InlineData("NewGuid()")>]
[<InlineData("Sleep(0)")>]
let ``JYRO-REQ-1800 LambdaNeverPassed_WithAStandardLibraryCallAbovePure_IsRejectedUnderAPureMaximum`` (call: string) =
    let source = sprintf "var g = x => %s\nOutput.r = 1" call
    let rejected = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } source
    check (not rejected.IsSuccess && Script.errorCodes rejected = [ MessageCode.FunctionEffectExceedsMax ]) (Script.describe rejected)
    // The same lambda passed to a lambda function is rejected with the same code, and is not rejected under a maximum that admits the call.
    let passed = sprintf "Output.r = Map([1], x => %s)" call
    let rejectedPassed = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } passed
    check (not rejectedPassed.IsSuccess && Script.errorCodes rejectedPassed = [ MessageCode.FunctionEffectExceedsMax ]) (Script.describe rejectedPassed)
    let admitted = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Blocking } source
    check admitted.IsSuccess (Script.describe admitted)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1800")>]
let ``JYRO-REQ-1800 LambdaFunctions_WithACallAtOrBelowTheMaximum_AreNotRejected`` () =
    let pure' = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } "var g = x => ToUpper(x)\nOutput.r = Map([\"a\"], y => Length(y))"
    check pure'.IsSuccess (Script.describe pure')
    let nd = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.NonDeterministic } "var g = x => Now()\nOutput.r = Map([1], y => Now())"
    check nd.IsSuccess (Script.describe nd)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1800")>]
let ``JYRO-REQ-1800 TheEightLambdaFunctions_HaveTheEffectPure`` () =
    for fn in lambdaFns do
        let f = registry fn
        check (f.Effect = FunctionEffect.Pure) (sprintf "%s declares the effect %A, not Pure" fn f.Effect)

// == 98.1 Map ===============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1357")>]
let ``Map_ResultElementAtIIsTheCallbackResultForElementAtI_SameLength`` () =
    assertExpr "[10, 20, 30]" "Map([1, 2, 3], x => x * 10)"
    assertExpr "[\"string\", \"null\", \"object\"]" "Map([\"a\", null, {k: 1}], x => TypeOf(x))"
    assertExpr "[null, null]" "Map([1, 2], x => null)"
    assertExpr "2" "Length(Map([1, 2], x => null))"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1358")>]
let ``Map_CallsTheCallbackOncePerElement_InOrder`` () =
    let source = "var n = 0\nvar r = Map([\"a\", \"b\", \"c\"], x => [x, n++])\nOutput.r = [r, n]"
    assertJson "[[[\"a\", 0], [\"b\", 1], [\"c\", 2]], 3]" (evalR source) source
    let p = probe ()
    runR (withFunctions [ p ]) "Output.r = Map([\"a\", \"b\", \"c\"], x => Probe(x))" |> ignore
    check (firstArgs p = [ "\"a\""; "\"b\""; "\"c\"" ]) (sprintf "%A" (firstArgs p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1359")>]
let ``Map_WhenItemsEmpty_ReturnsEmptyArray_WithoutCallingTheCallback`` () =
    let p = probe ()
    assertJson "[]" (runR (withFunctions [ p ]) "Output.r = Map([], x => Probe(x))") "Map([])"
    assertJson "[]" (runR (withFunctions [ p ]) "Output.r = Map(null, x => Probe(x))") "Map(null)"
    check (p.CallCount = 0) "Map called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1360")>]
let ``Map_ResultIsANewArray_AndItemsIsUnchanged`` () =
    let source =
        "var xs = [1, 2]\nvar before = ToJson(xs)\nvar r = Map(xs, x => x)\nvar same = ToJson(xs) == before\nr[0] = 9\nOutput.r = [xs, same]"
    assertJson "[[1, 2], true]" (evalR source) source
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Map(%s, x => x)" items)

// == 98.2 Where =============================================================================

let private truthCases = "var R = [null, false, 0, 0.0, \"\", true, 1, -1, 0.5, \"0\", \"false\", [], {}]\nvar I = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1361")>]
let ``Where_ReturnsAnArray_AndTakesItemsAndACallback`` () =
    assertExpr "\"array\"" "TypeOf(Where([1, 2], x => true))"
    assertExpr "\"array\"" "TypeOf(Where([], x => x))"
    assertSignature "Where" [ "items"; "callback" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1784")>]
let ``JYRO-REQ-1784 Where_KeepsTheElementsWhoseCallbackResultIsTruthy_InOrder`` () =
    assertExpr "[1, 3]" "Where([0, 1, 2, 3], x => x % 2 == 1)"
    let source = truthCases + "\nOutput.r = Where(I, i => R[i])"
    assertJson "[5, 6, 7, 8, 9, 10, 11, 12]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1784")>]
let ``JYRO-REQ-1784 Where_TheClausesExample_ReturnsExactlyTheElementsWithATruthyResult`` () =
    assertExpr "[1, \"a\", []]" "Where([0, 1, \"\", \"a\", null, [], false], x => x)"
    assertExpr "3" "Length(Where([0, 1, \"\", \"a\", null, [], false], x => x))"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1784")>]
let ``JYRO-REQ-1784 Where_LeavesOutEveryElementWhoseResultIsFalsy`` () =
    // Every element is falsy: none is kept.
    assertExpr "[]" "Where([0, \"\", null, false, 0.0], x => x)"
    assertExpr "[]" "Where([1, 2, 3], x => null)"
    assertExpr "0" "Length(Where([1, 2, 3], x => 0))"
    // Some are falsy: they are absent, and the kept ones keep their order of position.
    assertExpr "[3, 1, 2]" "Where([3, 0, 1, null, 2, false], x => x)"
    assertExpr "[\"b\", \"d\"]" "Where([\"a\", \"b\", \"c\", \"d\"], x => x == \"b\" or x == \"d\")"
    // The callback result is a condition, not a boolean: truthy non-booleans keep the element.
    assertExpr "[1, 2]" "Where([1, 2, 3], x => x < 3 ? \"yes\" : 0)"
    assertExpr "[1, 2, 3]" "Where([1, 2, 3], x => [x])"
    assertExpr "[]" "Where([1, 2, 3], x => \"\")"
    assertExpr "[1]" "Where([1, 2], x => x == 1 ? -1 : 0.0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1363")>]
let ``Where_ResultElementsAreTheElementsThemselves`` () =
    let source = "var xs = [{a: 1}, {a: 2}]\nvar r = Where(xs, x => true)\nr[0].b = 7\nOutput.r = xs[0].b"
    assertJson "7" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1364")>]
let ``Where_CallsTheCallbackOncePerElement_InOrder`` () =
    let source = "var n = 0\nvar r = Where([\"a\", \"b\", \"c\"], x => n++ >= 0)\nOutput.r = [r, n]"
    assertJson "[[\"a\", \"b\", \"c\"], 3]" (evalR source) source
    let p = probe ()
    runR (withFunctions [ p ]) "Output.r = Where([\"a\", \"b\", \"c\", \"d\"], x => Probe(x) ?? (x == \"b\"))" |> ignore
    check (firstArgs p = [ "\"a\""; "\"b\""; "\"c\""; "\"d\"" ]) (sprintf "%A" (firstArgs p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1365")>]
let ``Where_WhenNothingIsTruthyOrItemsEmpty_ReturnsEmptyArray`` () =
    assertExpr "[]" "Where([1, 2], x => false)"
    assertExpr "[]" "Where([1, 2], x => 0)"
    let p = probe ()
    assertJson "[]" (runR (withFunctions [ p ]) "Output.r = Where([], x => Probe(x) ?? true)") "Where([])"
    check (p.CallCount = 0) "Where called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1366")>]
let ``Where_ResultIsANewArray_AlsoWhenEveryElementIsKept_AndItemsIsUnchanged`` () =
    let source =
        "var xs = [1, 2]\nvar before = ToJson(xs)\nvar r = Where(xs, x => true)\nvar same = ToJson(xs) == before\nr[0] = 9\nOutput.r = [xs, same]"
    assertJson "[[1, 2], true]" (evalR source) source
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Where(%s, x => true)" items)

// == 98.3 Reduce ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1367")>]
let ``Reduce_TakesItemsATwoParameterCallbackAndAnyInitial_ReturnsAnyType`` () =
    assertExpr "6" "Reduce([1, 2, 3], (a, x) => a + x, 0)"
    assertExpr "\"ab\"" "Reduce([\"a\", \"b\"], (a, x) => a & x, \"\")"
    assertExpr "[[0, 1], 2]" "Reduce([1, 2], (a, x) => [a, x], 0)"
    assertExpr "null" "Reduce([1], (a, x) => null, 0)"
    assertExpr "{\"n\": 1}" "Reduce([1], (a, x) => a, {n: 1})"
    assertSignature "Reduce" [ "items"; "callback"; "initial" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1368")>]
let ``Reduce_CallsTheCallbackWithTheAccumulatorFirstAndTheElementSecond`` () =
    assertExpr "3" "Reduce([1, 2, 3], (a, x) => x, 0)"
    assertExpr "0" "Reduce([1, 2, 3], (a, x) => a, 0)"
    let p = FunctionValueProbe(2, fun args -> (match args with _ :: x :: _ -> x | _ -> Values.nul))
    runR (withFunctions [ p.Maker ]) "var p = MakeProbe()\nOutput.r = Reduce([1, 2, 3], p, \"init\")" |> ignore
    let calls = p.Calls |> List.map (List.map Values.toJson)
    let expected = [ [ norm "\"init\""; "1" ]; [ "1"; "2" ]; [ "2"; "3" ] ]
    check (calls = expected) (sprintf "Reduce gave the callback %A, expected %A" calls expected)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1369")>]
let ``Reduce_CallsTheCallbackOncePerElement_InOrder`` () =
    assertExpr "\"abc\"" "Reduce([\"a\", \"b\", \"c\"], (a, x) => a & x, \"\")"
    let p = probe ()
    runR (withFunctions [ p ]) "Output.r = Reduce([\"a\", \"b\", \"c\"], (a, x) => Probe(x), 0)" |> ignore
    check (firstArgs p = [ "\"a\""; "\"b\""; "\"c\"" ]) (sprintf "%A" (firstArgs p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1370")>]
let ``Reduce_FirstAccumulatorIsInitial_EachLaterIsThePreviousResult`` () =
    assertExpr "[[[\"init\", 1], 2], 3]" "Reduce([1, 2, 3], (a, x) => [a, x], \"init\")"
    let p = probe ()
    runR (withFunctions [ p ]) "Output.r = Reduce([1, 2, 3], (a, x) => (Probe(a) ?? a) & x, \"i\")" |> ignore
    check (firstArgs p = [ "\"i\""; "\"i1\""; "\"i12\"" ]) (sprintf "accumulators %A" (firstArgs p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1371")>]
let ``Reduce_ReturnsTheResultOfTheLastCall`` () =
    assertExpr "123" "Reduce([1, 2, 3], (a, x) => a * 10 + x, 0)"
    assertExpr "6" "Reduce([5], (a, x) => x + 1, 0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1372")>]
let ``Reduce_WhenItemsEmpty_ReturnsInitial_WithoutCallingTheCallback`` () =
    let p = probe ()
    let options = withFunctions [ p ]
    for v in [ "0"; "null"; "\"s\""; "[1]"; "{\"a\": 1}" ] do
        for items in [ "[]"; "null" ] do
            let source = sprintf "Output.r = Reduce(%s, (a, x) => Probe(x), %s)" items v
            assertJson v (runR options source) source
    let same = "var o = {a: 1}\nvar r = Reduce([], (a, x) => x, o)\nr.b = 2\nOutput.r = o.b"
    assertJson "2" (runR options same) same
    check (p.CallCount = 0) "Reduce called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1373")>]
let ``Reduce_AcceptsAnyInitial_AndGivesItToTheCallbackUnchanged`` () =
    for v in [ "null"; "false"; "0"; "\"\""; "[]"; "{}"; "\"0\"" ] do
        assertExpr v (sprintf "Reduce([1], (a, x) => a, %s)" v)
        let p = probe ()
        runR (withFunctions [ p ]) (sprintf "Output.r = Reduce([1], (a, x) => Probe(a), %s)" v) |> ignore
        check (firstArgs p = [ norm v ]) (sprintf "the first accumulator was %A, not %s" (firstArgs p) v)
    let same = "var o = {a: 1}\nvar r = Reduce([1], (a, x) => a, o)\nr.b = 2\nOutput.r = o.b"
    assertJson "2" (evalR same) same

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1374")>]
let ``Reduce_DoesNotChangeItems`` () =
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Reduce(%s, (a, x) => a, 0)" items)
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Reduce(%s, (a, x) => x, 0)" items)

// == 98.4 Each ==============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1375")>]
let ``Each_ReturnsNull_AndTakesItemsAndACallback`` () =
    assertExpr "\"null\"" "TypeOf(Each([1, 2], x => x))"
    assertSignature "Each" [ "items"; "callback" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1376")>]
let ``Each_CallsTheCallbackOncePerElement_InOrder`` () =
    let source = "var n = 0\nvar r = Each([\"a\", \"b\", \"c\"], x => n++)\nOutput.r = n"
    assertJson "3" (evalR source) source
    let p = probe ()
    runR (withFunctions [ p ]) "Output.r = Each([\"a\", \"b\", \"c\"], x => Probe(x))" |> ignore
    check (firstArgs p = [ "\"a\""; "\"b\""; "\"c\"" ]) (sprintf "%A" (firstArgs p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1377")>]
let ``Each_IgnoresTheCallbackResults_AndReturnsNull`` () =
    assertExpr "null" "Each([1, 2], x => [x])"
    assertExpr "null" "Each([1, 2], x => {v: x})"
    assertExpr "null" "Each([1], x => 42)"
    assertExpr "null" "Each([1], x => true)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1378")>]
let ``Each_WhenItemsEmpty_ReturnsNull_WithoutCallingTheCallback`` () =
    let p = probe ()
    assertJson "null" (runR (withFunctions [ p ]) "Output.r = Each([], x => Probe(x))") "Each([])"
    assertJson "null" (runR (withFunctions [ p ]) "Output.r = Each(null, x => Probe(x))") "Each(null)"
    check (p.CallCount = 0) "Each called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1379")>]
let ``Each_DoesNotChangeItems`` () =
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Each(%s, x => x)" items)

// == 98.5 Find ==============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1380")>]
let ``Find_ReturnsAValueOfAnyType`` () =
    assertExpr "\"number\"" "TypeOf(Find([1], x => true))"
    assertExpr "\"string\"" "TypeOf(Find([\"s\"], x => true))"
    assertExpr "\"array\"" "TypeOf(Find([[1]], x => true))"
    assertExpr "\"object\"" "TypeOf(Find([{a: 1}], x => true))"
    assertExpr "\"boolean\"" "TypeOf(Find([true], x => true))"
    assertExpr "\"function\"" "TypeOf(Find([y => y], x => true))"
    assertExpr "\"null\"" "TypeOf(Find([1], x => false))"
    assertSignature "Find" [ "items"; "callback" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1381")>]
let ``JYRO-REQ-1381 Find_ReturnsTheFirstElementWhoseCallbackResultIsTruthy_FalseAndNullIncluded`` () =
    assertExpr "5" "Find([1, 5, 7, 9], x => x > 4)"
    assertExpr "false" "Find([1, false, 2], x => x == false)"
    // A matched null or false element is the answer, not a reason to go on
    // (each later element would be a different, non-null answer).
    assertExpr "null" "Find([null, false, 3], x => true)"
    assertExpr "null" "Find([null, 3], x => true)"
    assertExpr "false" "Find([false, 3], x => true)"
    assertExpr "false" "Find([false, null, 3], x => true)"
    assertExpr "null" "Find([1, null, 2], x => x == null)"
    assertExpr "null" "Find([1, null, 2, 3], x => x != 1)"
    assertExpr "false" "Find([1, false, true], x => x != 1)"
    assertJson "null" (runR { Script.defaults with Input = ValueInput (Values.json "{\"xs\": [0, null, 7]}") }
                         "Output.r = Find(Input.xs, x => x != 0)") "Find over Input with a null match"
    let source = truthCases + "\nOutput.r = [Find(I, i => R[i]), Find([0, 1, 2, 3, 4], i => R[i]), Find([4, 11], i => R[i]), Find([4, 12], i => R[i]), Find([4, 9], i => R[i])]"
    assertJson "[5, null, 11, 12, 9]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1382")>]
let ``Find_CallsInOrder_AndNotAfterTheFirstTruthyResult`` () =
    let p = probe ()
    runR (withFunctions [ p ]) "Output.r = Find([1, 2, 3, 4], x => Probe(x) ?? x == 2)" |> ignore
    check (firstArgs p = [ "1"; "2" ]) (sprintf "%A" (firstArgs p))
    assertExpr "1" "Find([1, \"a\"], x => x > 0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1383")>]
let ``Find_ReturnsTheElementItself`` () =
    let source = "var xs = [{a: 1}, {a: 2}]\nvar r = Find(xs, x => x.a == 2)\nr.b = 9\nOutput.r = xs[1].b"
    assertJson "9" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1384")>]
let ``Find_WhenNothingIsTruthyOrItemsEmpty_ReturnsNull`` () =
    assertExpr "null" "Find([1, 2], x => false)"
    assertExpr "null" "Find([1, 2], x => 0)"
    assertExpr "null" "Find([1, 2], x => \"\")"
    let p = probe ()
    assertJson "null" (runR (withFunctions [ p ]) "Output.r = Find([], x => Probe(x) ?? true)") "Find([])"
    check (p.CallCount = 0) "Find called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1385")>]
let ``Find_DoesNotChangeItems`` () =
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Find(%s, x => false)" items)
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Find(%s, x => true)" items)

// == 98.6 All ===============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1386")>]
let ``All_ReturnsABoolean_AlsoForNonBooleanCallbackResults`` () =
    assertExpr "[\"boolean\", true]" "[TypeOf(All([1], x => 1)), All([1], x => 1)]"
    assertExpr "[\"boolean\", false]" "[TypeOf(All([1], x => \"\")), All([1], x => \"\")]"
    assertExpr "[\"boolean\", true]" "[TypeOf(All([1], x => [])), All([1], x => [])]"
    assertSignature "All" [ "items"; "callback" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1387")>]
let ``All_IsTrueWhenEveryResultIsTruthy_FalseOtherwise`` () =
    assertExpr "true" "All([1, 2, 3], x => x > 0)"
    assertExpr "false" "All([1, 2, 3], x => x < 3)"
    assertExpr "false" "All([1, 2, 3], x => x > 1)"
    let source =
        truthCases
        + "\nOutput.r = [All([0], i => R[i]), All([1], i => R[i]), All([2], i => R[i]), All([3], i => R[i]), All([4], i => R[i]), All([5, 6, 7, 8, 9, 10, 11, 12], i => R[i])]"
    assertJson "[false, false, false, false, false, true]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1388")>]
let ``All_WhenItemsEmpty_IsTrue_WithoutCallingTheCallback`` () =
    let p = probe ()
    let options = withFunctions [ p ]
    assertJson "true" (runR options "Output.r = All([], x => Probe(x))") "All([])"
    assertJson "true" (runR options "Output.r = All(null, x => Probe(x))") "All(null)"
    assertJson "true" (runR options "Output.r = All([], x => false)") "All([], false)"
    check (p.CallCount = 0) "All called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1389")>]
let ``All_CallsInOrder_AndNotAfterTheFirstFalsyResult`` () =
    let p = probe ()
    assertJson "false" (runR (withFunctions [ p ]) "Output.r = All([1, 2, 3, 4], x => Probe(x) ?? x != 2)") "All"
    check (firstArgs p = [ "1"; "2" ]) (sprintf "%A" (firstArgs p))
    assertExpr "false" "All([0, \"a\"], x => x > 0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1390")>]
let ``All_DoesNotChangeItems`` () =
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "All(%s, x => true)" items)

// == 98.7 Any ===============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1391")>]
let ``Any_ReturnsABoolean_AlsoForNonBooleanCallbackResults`` () =
    assertExpr "[\"boolean\", true]" "[TypeOf(Any([1], x => 1)), Any([1], x => 1)]"
    assertExpr "[\"boolean\", false]" "[TypeOf(Any([1], x => \"\")), Any([1], x => \"\")]"
    assertExpr "[\"boolean\", false]" "[TypeOf(Any([1], x => null)), Any([1], x => null)]"
    assertSignature "Any" [ "items"; "callback" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1392")>]
let ``Any_IsTrueWhenSomeResultIsTruthy_FalseOtherwise`` () =
    assertExpr "true" "Any([1, 2, 3], x => x == 3)"
    assertExpr "false" "Any([1, 2, 3], x => x > 5)"
    let source =
        truthCases
        + "\nOutput.r = [Any([0, 1, 2, 3, 4], i => R[i]), Any([9], i => R[i]), Any([11], i => R[i]), Any([12], i => R[i]), Any([8], i => R[i]), Any([7], i => R[i])]"
    assertJson "[false, true, true, true, true, true]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1393")>]
let ``Any_WhenItemsEmpty_IsFalse_WithoutCallingTheCallback`` () =
    let p = probe ()
    let options = withFunctions [ p ]
    assertJson "false" (runR options "Output.r = Any([], x => Probe(x) ?? true)") "Any([])"
    assertJson "false" (runR options "Output.r = Any(null, x => Probe(x) ?? true)") "Any(null)"
    assertJson "false" (runR options "Output.r = Any([], x => true)") "Any([], true)"
    check (p.CallCount = 0) "Any called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1394")>]
let ``Any_CallsInOrder_AndNotAfterTheFirstTruthyResult`` () =
    let p = probe ()
    assertJson "true" (runR (withFunctions [ p ]) "Output.r = Any([1, 2, 3, 4], x => Probe(x) ?? x == 2)") "Any"
    check (firstArgs p = [ "1"; "2" ]) (sprintf "%A" (firstArgs p))
    assertExpr "true" "Any([1, \"a\"], x => x > 0)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1395")>]
let ``Any_DoesNotChangeItems`` () =
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "Any(%s, x => false)" items)

// == 98.8 SortBy ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1396")>]
let ``SortBy_ReturnsAnArray_AndTakesItemsAndACallback`` () =
    assertExpr "\"array\"" "TypeOf(SortBy([2, 1], x => x))"
    assertExpr "\"array\"" "TypeOf(SortBy([], x => x))"
    assertSignature "SortBy" [ "items"; "callback" ]

let private mixed = "var a = 1\nvar b = 1.0000000000000000000000000001\nvar c = Power(1, 0.5)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1397")>]
let ``SortBy_ReturnsTheElementsSmallestKeyFirst`` () =
    assertExpr "[1, 2, 3]" "SortBy([3, 1, 2], x => x)"
    assertExpr "[\"b\", \"a\"]" "Select(SortBy([{n: 2, id: \"a\"}, {n: 1, id: \"b\"}], x => x.n), \"id\")"
    assertExpr "[{\"n\": 1, \"id\": \"b\"}, {\"n\": 2, \"id\": \"a\"}]" "SortBy([{n: 2, id: \"a\"}, {n: 1, id: \"b\"}], x => x.n)"
    assertExpr "[3, 2, 1]" "SortBy([1, 2, 3], x => 0 - x)"
    assertExpr "[7]" "SortBy([7], x => x)"
    let source = mixed + "\nOutput.r = Select(SortBy([{k: b, n: \"b\"}, {k: c, n: \"c\"}, {k: a, n: \"a\"}], x => x.k), \"n\")"
    assertJson "[\"a\", \"b\", \"c\"]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1398")>]
let ``JYRO-REQ-1398 SortBy_ComparesKeysByTheSortOrder_WithoutErrorForMixedTypes`` () =
    assertExpr "[2, 3, 0, 1]" "Select(SortBy([{k: 1, i: 0}, {k: \"a\", i: 1}, {k: null, i: 2}, {k: true, i: 3}], x => x.k), \"i\")"
    let src =
        "var f = y => y\nOutput.r = Select(SortBy([{k: [1], i: 0}, {k: {}, i: 1}, {k: f, i: 2}, {k: false, i: 3}, {k: \"s\", i: 4}, {k: 0, i: 5}, {k: null, i: 6}], x => x.k), \"i\")"
    assertJson "[6, 3, 5, 4, 0, 1, 2]" (evalR src) src
    assertExpr "[false, true]" "SortBy([true, false], x => x)"
    assertExpr "[\"B\", \"a\", \"b\"]" "SortBy([\"b\", \"a\", \"B\"], x => x)"
    assertExpr "[1, 0]" "Select(SortBy([{k: \"\\uFFFF\", i: 0}, {k: \"\\uD83D\\uDE00\", i: 1}], x => x.k), \"i\")"
    assertExpr "[0, 1, 2]" "Select(SortBy([{k: [2], i: 0}, {k: [1], i: 1}, {k: [], i: 2}], x => x.k), \"i\")"
    assertExpr "[0, 1]" "Select(SortBy([{k: {z: 2}, i: 0}, {k: {a: 1}, i: 1}], x => x.k), \"i\")"
    let perms = [ "a, b, c"; "a, c, b"; "b, a, c"; "b, c, a"; "c, a, b"; "c, b, a" ]
    for p in perms do
        let keys = p.Split([| ", " |], StringSplitOptions.None)
        let items = keys |> Array.map (fun k -> sprintf "{k: %s, n: \"%s\"}" k k) |> String.concat ", "
        let source = sprintf "%s\nOutput.r = Select(SortBy([%s], x => x.k), \"n\")" mixed items
        assertJson "[\"a\", \"b\", \"c\"]" (evalR source) source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1802")>]
let ``JYRO-REQ-1802 SortBy_CallsTheCallbackExactlyOncePerElementOfDuplicatesAndNulls_InOrderOfPosition`` () =
    let cases =
        [ [ ]
          [ "7" ]
          [ "3"; "3"; "1"; "1"; "2" ]
          [ "null"; "5"; "null"; "5"; "0" ]
          [ for i in 1 .. 100 -> string (i % 7) ] ]
    for items in cases do
        let p = probe ()
        let src = sprintf "Output.r = SortBy([%s], x => Probe(x) ?? x)" (String.concat ", " items)
        let r = runR (withFunctions [ p ]) src
        // One call for each element of items, in order of position: the calls are the elements, in order.
        let expectedCalls = items |> List.map norm
        check (p.CallCount = List.length items) (sprintf "%d elements: %d calls of the callback" (List.length items) p.CallCount)
        check (firstArgs p = expectedCalls) (sprintf "%d elements: the callback got %A" (List.length items) (firstArgs p))
        check (Values.asList r |> List.length = List.length items) (sprintf "the result has another length than items: %s" (Values.toJson r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1802")>]
let ``JYRO-REQ-1802 SortBy_CallsTheCallbackOnceForEachElement_AlsoWhenTheKeysAreAlreadyInOrderOrReversed`` () =
    for items in [ [ "1"; "2"; "3"; "4" ]; [ "4"; "3"; "2"; "1" ]; [ "2"; "2"; "2" ] ] do
        let p = probe ()
        runR (withFunctions [ p ]) (sprintf "Output.r = SortBy([%s], x => Probe(x) ?? x)" (String.concat ", " items)) |> ignore
        check (firstArgs p = items) (sprintf "the callback got %A, expected %A" (firstArgs p) items)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1802")>]
let ``JYRO-REQ-1802 SortBy_CallsTheCallbackExactlyOncePerElement_InOrder`` () =
    let source = "var n = 0\nvar r = SortBy([\"c\", \"a\", \"b\"], x => 0 - n++)\nOutput.r = [r, n]"
    assertJson "[[\"b\", \"a\", \"c\"], 3]" (evalR source) source
    for count in [ 1; 2; 5; 50 ] do
        let values = [ for i in count .. -1 .. 1 -> string i ]
        let p = probe ()
        let src = sprintf "Output.r = SortBy([%s], x => Probe(x) ?? x)" (String.concat ", " values)
        let r = runR (withFunctions [ p ]) src
        check (firstArgs p = values) (sprintf "%d elements: the callback got %A" count (firstArgs p))
        assertJson (sprintf "[%s]" (String.concat ", " (List.rev values))) r src

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1400")>]
let ``SortBy_KeepsTheRelativeOrderOfElementsWithEqualKeys`` () =
    let ks = [ 2; 1; 1; 2; 2; 1; 2; 1; 1; 1; 2; 2; 1; 2; 1; 2; 2; 1; 1; 2 ]
    let items = ks |> List.mapi (fun i k -> sprintf "{k: %d, id: %d}" k i) |> String.concat ", "
    let expected =
        (ks |> List.indexed |> List.filter (fun (_, k) -> k = 1) |> List.map fst)
        @ (ks |> List.indexed |> List.filter (fun (_, k) -> k = 2) |> List.map fst)
    assertExpr (sprintf "[%s]" (expected |> List.map string |> String.concat ", "))
        (sprintf "Select(SortBy([%s], x => x.k), \"id\")" items)
    assertExpr "[0, 1, 2]" "Select(SortBy([{k: 1, id: 0}, {k: Power(1, 0.5), id: 1}, {k: 1, id: 2}], x => 5), \"id\")"
    // Equal keys by 90.6: two arrays, two objects.
    assertExpr "[0, 1, 2]" "Select(SortBy([{k: [3], id: 0}, {k: [1], id: 1}, {k: [2], id: 2}], x => x.k), \"id\")"
    assertExpr "[0, 1]" "Select(SortBy([{k: {b: 1}, id: 0}, {k: {a: 1}, id: 1}], x => x.k), \"id\")"
    assertExpr "[3, 2, 1]" "SortBy([3, 2, 1], x => null)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1401")>]
let ``SortBy_WhenItemsEmpty_ReturnsEmptyArray_WithoutCallingTheCallback`` () =
    let p = probe ()
    assertJson "[]" (runR (withFunctions [ p ]) "Output.r = SortBy([], x => Probe(x))") "SortBy([])"
    assertJson "[]" (runR (withFunctions [ p ]) "Output.r = SortBy(null, x => Probe(x))") "SortBy(null)"
    check (p.CallCount = 0) "SortBy called the callback for an empty items"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1402")>]
let ``SortBy_WhenCallbackRaises_StopsAndPropagatesTheErrorUnchanged`` () =
    checkCallbackErrorPropagates "SortBy"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1403")>]
let ``SortBy_ResultIsANewArray_AndItemsIsUnchanged`` () =
    let source = "var xs = [3, 1, 2]\nvar r = SortBy(xs, x => x)\nvar before = [xs[0], xs[1], xs[2]]\nr[0] = 9\nOutput.r = [before, xs]"
    assertJson "[[3, 1, 2], [3, 1, 2]]" (evalR source) source
    let sorted = "var xs = [1, 2, 3]\nvar r = SortBy(xs, x => x)\nr[0] = 9\nOutput.r = xs"
    assertJson "[1, 2, 3]" (evalR sorted) sorted
    S97_02_QueryFunctions.assertItemsUnchanged (fun items -> sprintf "SortBy(%s, x => 0)" items)

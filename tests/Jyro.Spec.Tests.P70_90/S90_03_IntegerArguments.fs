/// jyro-spec 90.3: integer arguments of standard library functions, and the
/// order of argument checks. From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S90_03_IntegerArguments

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// Every whole-quantity argument of the inventory (rule 4.1.8): a label, a call
/// template whose `{0}` is the argument, and whether it cannot be negative.
let paths : (string * string * bool) list =
    [ "Substring.start", "Substring(\"abcdef\", {0})", true
      "Substring.length", "Substring(\"abcdef\", 1, {0})", true
      "PadLeft.length", "PadLeft(\"a\", {0})", true
      "PadRight.length", "PadRight(\"a\", {0})", true
      "RandomString.length", "RandomString({0})", true
      "Slice.start", "Slice([1, 2, 3], {0})", true
      "Slice.finish", "Slice([1, 2, 3], 0, {0})", true
      "Skip.count", "Skip([1, 2, 3], {0})", true
      "Insert.index", "Insert([1, 2, 3], {0}, 0)", false
      "RemoveAt.index", "RemoveAt([1, 2, 3], {0})", false
      "Round.decimals", "Round(1.2345, {0})", false
      "RandomInt.minimum", "RandomInt({0}, 2147483647)", false
      "RandomInt.maximum", "RandomInt(-2147483648, {0})", false
      "DateAdd.amount", "DateAdd(\"2024-01-02\", {0}, \"second\")", false
      "Sleep.ms", "Sleep({0})", true ]

let private template (label: string) = paths |> List.pick (fun (l, t, _) -> if l = label then Some t else None)

let callOf (label: string) (value: string) = (template label).Replace("{0}", value)

/// Run `Output.before = 1`, then the call, then `Output.after = 1`, with every
/// effect allowed and a 3 s execution time limit (so a call that wrongly
/// sleeps cannot stall the suite).
let runCall (call: string) : JyroResult<JyroValue> =
    Script.runWith
        { allEffects with Limits = Some(Script.limits (fun o -> { o with MaxExecutionTime = TimeSpan.FromSeconds 3.0 })) }
        (lines [ "Output.before = 1"; "Output.r = " + call; "Output.after = 1" ])

let cross (labels: string list) (values: string list) : obj[] seq =
    [ for l in labels do for v in values -> [| box l; box v |] ]

let allLabels = paths |> List.map (fun (l, _, _) -> l)
let nonNegativeLabels = paths |> List.filter (fun (_, _, nn) -> nn) |> List.map (fun (l, _, _) -> l)

// ===========================================================================
// 90.3.1 IntegerRequired
// ===========================================================================

let nonIntegers () = cross allLabels [ "1.5"; "0.5"; "2.000001"; "1e-10"; "12345.5"; "(7 / 2)"; "-1.5"; "-0.5" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2150")>]
[<MemberData(nameof nonIntegers)>]
let ``JYRO-REQ-2150 WholeQuantityArgument_ThatIsNotAnInteger_RaisesIntegerRequired`` (path: string, value: string) =
    Script.expectError MessageCode.IntegerRequired (runCall (callOf path value))

/// The statement's own examples.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2150")>]
[<InlineData("Skip([1, 2, 3], 1.5)")>]
[<InlineData("Substring(\"abc\", -0.5)")>]
[<InlineData("Round(1.25, 7 / 2)")>]
let ``JYRO-REQ-2150 StatementExamples_RaiseIntegerRequired`` (call: string) =
    Script.expectError MessageCode.IntegerRequired (runCall call)

let integerControls () = cross allLabels [ "2"; "2.0"; "(4 / 2)"; "(2 * Power(1, 0.5))" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2150")>]
[<MemberData(nameof integerControls)>]
let ``JYRO-REQ-2150 WholeQuantityArgument_ThatIsAnIntegerExactOrInexact_IsNotRejectedAsNonInteger`` (path: string, value: string) =
    assertNoErrorCode MessageCode.IntegerRequired (runCall (callOf path value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2150")>]
[<InlineData("Substring.start")>]
[<InlineData("PadLeft.length")>]
[<InlineData("Skip.count")>]
[<InlineData("Insert.index")>]
let ``JYRO-REQ-2150 WholeQuantityArgument_GivenANumericString_RaisesTheTypeErrorNotIntegerRequired`` (path: string) =
    Script.expectError MessageCode.ArgumentTypeMismatch (runCall (callOf path "\"1.5\""))

/// "Exactly these fifteen": a fractional number given to an argument that is not
/// one of them (a value, an element, a base, a pad character) is not refused
/// with IntegerRequired.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2150")>]
[<InlineData("Power(2, 0.5)")>]
[<InlineData("Round(1.5, 0)")>]
[<InlineData("Min([1.5, 2.5])")>]
[<InlineData("Append([1], 1.5)")>]
[<InlineData("Prepend([1], 1.5)")>]
[<InlineData("Insert([1, 2], 0, 1.5)")>]
[<InlineData("Contains([1.5], 1.5)")>]
[<InlineData("IndexOf([1.5], 1.5)")>]
[<InlineData("Replace(\"a\", \"a\", 1.5)")>]
[<InlineData("Sum([1.5, 2.5])")>]
[<InlineData("Average([1.5, 2.5])")>]
[<InlineData("RandomInt(1, 2)")>]
let ``JYRO-REQ-2150 ArgumentThatIsNotAWholeQuantity_GivenAFraction_IsNotRejectedAsNonInteger`` (call: string) =
    let r = runCall call
    assertNoErrorCode MessageCode.IntegerRequired r
    Assert.True(r.IsSuccess, Script.describe r)

// ===========================================================================
// 90.3.2 IntegerOutOfRange
// ===========================================================================

let outOfRange () =
    cross allLabels [ "2147483648"; "-2147483649"; "1e15"; "1e20"; "(1e20 * Power(1, 0.5))"; "-1e20"; "-3000000000"; "4294967296" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-332")>]
[<MemberData(nameof outOfRange)>]
let ``WholeQuantityArgument_OutsideTheInt32Range_RaisesIntegerOutOfRange`` (path: string, value: string) =
    Script.expectError MessageCode.IntegerOutOfRange (runCall (callOf path value))

let bounds () = cross allLabels [ "2147483647"; "-2147483648" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-332")>]
[<MemberData(nameof bounds)>]
let ``WholeQuantityArgument_AtTheInt32Bounds_IsNotRejectedAsOutOfRange`` (path: string, value: string) =
    let r =
        if path = "Sleep.ms" && value = "2147483647" then
            // Accepted by the range check, the call would sleep; a short time
            // limit ends it (ExecutionTimeLimitExceeded, not IntegerOutOfRange).
            Script.runWith
                { allEffects with Limits = Some(Script.limits (fun o -> { o with MaxExecutionTime = TimeSpan.FromMilliseconds 200.0 })) }
                ("Output.r = " + callOf path value)
        else runCall (callOf path value)
    assertNoErrorCode MessageCode.IntegerOutOfRange r

// ===========================================================================
// 90.3.3 NonNegativeIntegerRequired
// ===========================================================================

let negatives () = cross nonNegativeLabels [ "-1"; "-2"; "-2147483648" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2152")>]
[<MemberData(nameof negatives)>]
let ``JYRO-REQ-2152 NonNegativeArgument_ThatIsNegative_RaisesNonNegativeIntegerRequired`` (path: string, value: string) =
    Script.expectError MessageCode.NonNegativeIntegerRequired (runCall (callOf path value))

/// The statement's own examples.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2152")>]
[<InlineData("PadLeft(\"a\", -1)")>]
[<InlineData("Sleep(-5)")>]
let ``JYRO-REQ-2152 StatementExamples_RaiseNonNegativeIntegerRequired`` (call: string) =
    Script.expectError MessageCode.NonNegativeIntegerRequired (runCall call)

let zeros () = cross nonNegativeLabels [ "0" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2152")>]
[<MemberData(nameof zeros)>]
let ``JYRO-REQ-2152 NonNegativeArgument_OfZero_IsNotRejectedAsNegative`` (path: string, value: string) =
    assertNoErrorCode MessageCode.NonNegativeIntegerRequired (runCall (callOf path value))

let private mayBeNegativeLabels = paths |> List.filter (fun (_, _, nn) -> not nn) |> List.map (fun (l, _, _) -> l)

/// "Exactly these nine": each of the other six whole-quantity arguments takes a
/// negative integer without NonNegativeIntegerRequired.
let mayBeNegative () = cross mayBeNegativeLabels [ "-1"; "-2"; "-2147483648" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2152")>]
[<MemberData(nameof mayBeNegative)>]
let ``JYRO-REQ-2152 ArgumentThatMayBeNegative_GivenANegativeInteger_IsNeverNonNegativeIntegerRequired`` (path: string, value: string) =
    assertNoErrorCode MessageCode.NonNegativeIntegerRequired (runCall (callOf path value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2152")>]
[<InlineData("RemoveAt([1, 2], -1)", "IndexOutOfRange")>]
[<InlineData("Insert([1, 2], -1, 0)", "IndexOutOfRange")>]
[<InlineData("Round(15, -1)", "")>]
[<InlineData("DateAdd(\"2024-01-02\", -1, \"day\")", "")>]
[<InlineData("RandomInt(-5, -1)", "")>]
let ``JYRO-REQ-2152 ArgumentThatMayBeNegative_GivenANegativeInteger_IsNotRejectedAsNegative`` (call: string, code: string) =
    let r = runCall call
    assertNoErrorCode MessageCode.NonNegativeIntegerRequired r
    if code = "" then Assert.True(r.IsSuccess, Script.describe r)
    else Script.expectError (Enum.Parse<MessageCode> code) r

// ===========================================================================
// 90.3.4 IndexOutOfRange
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-334")>]
[<InlineData("Insert([1, 2, 3], -1, 0)")>]
[<InlineData("Insert([1, 2, 3], 4, 0)")>]
[<InlineData("Insert([], 1, 0)")>]
[<InlineData("RemoveAt([1, 2, 3], -1)")>]
[<InlineData("RemoveAt([1, 2, 3], 3)")>]
[<InlineData("RemoveAt([], 0)")>]
[<InlineData("RemoveAt(null, 0)")>]
[<InlineData("Substring(\"abc\", 4)")>]
[<InlineData("Substring(\"abc\", 4, 1)")>]
[<InlineData("Substring(\"\", 1)")>]
let ``JYRO-REQ-334 PositionArgument_NamingNoPosition_RaisesIndexOutOfRange`` (call: string) =
    Script.expectError MessageCode.IndexOutOfRange (runCall call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-334")>]
[<InlineData("Insert([1, 2, 3], 0, 0)", "[0,1,2,3]")>]
[<InlineData("Insert([1, 2, 3], 3, 0)", "[1,2,3,0]")>]
[<InlineData("Insert([], 0, 0)", "[0]")>]
[<InlineData("RemoveAt([1, 2, 3], 0)", "[2,3]")>]
[<InlineData("RemoveAt([1, 2, 3], 2)", "[1,2]")>]
[<InlineData("Substring(\"abc\", 3)", "\"\"")>]
[<InlineData("Substring(\"abc\", 0)", "\"abc\"")>]
let ``JYRO-REQ-334 PositionArgument_AtTheFirstOrLastValidPosition_DoesNotRaise`` (call: string, expected: string) =
    let v = outProp "r" (runCall call)
    Assert.True(v.EqualsValue(Values.json expected), Values.toJson v)

// ===========================================================================
// 90.3.5 A rejected argument is never truncated, rounded, clamped or ignored
// 90.3.6 The failure is a Jyro runtime error
// ===========================================================================

let rejecting () : obj[] seq =
    [ for (l, _, _) in paths do
          yield [| box (callOf l "1.5"); box "IntegerRequired" |]
          yield [| box (callOf l "-0.5"); box "IntegerRequired" |]
          yield [| box (callOf l "2147483648"); box "IntegerOutOfRange" |]
          yield [| box (callOf l "1e20"); box "IntegerOutOfRange" |]
          yield [| box (callOf l "9.3e18"); box "IntegerOutOfRange" |]
          yield [| box (callOf l "-1e20"); box "IntegerOutOfRange" |]
      for (l, _, nn) in paths do
          if nn then yield [| box (callOf l "-1"); box "NonNegativeIntegerRequired" |]
      yield [| box "Substring(\"abcdef\", 1.5)"; box "IntegerRequired" |]
      yield [| box "PadLeft(\"a\", 2.7)"; box "IntegerRequired" |]
      yield [| box "Round(1.2345, 2.5)"; box "IntegerRequired" |]
      yield [| box "RandomInt(1.5, 3)"; box "IntegerRequired" |]
      yield [| box "Sleep(0.5)"; box "IntegerRequired" |]
      yield [| box "Insert([1], 5, 0)"; box "IndexOutOfRange" |]
      yield [| box "RemoveAt([1], 3)"; box "IndexOutOfRange" |]
      yield [| box "Substring(\"abc\", 5)"; box "IndexOutOfRange" |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1976")>]
[<MemberData(nameof rejecting)>]
let ``JYRO-REQ-1976 RejectedArgument_IsNeverTruncatedRoundedClampedOrIgnored_TheCallRaisesAndTheScriptStops`` (call: string, code: string) =
    let r = runCall call
    Script.expectError (Enum.Parse<MessageCode> code) r
    match Script.value r with
    | Some v when (v :? JyroObject) ->
        Assert.False(hasKey "r" v, sprintf "%s gave a result: %s" call (Values.toJson v))
        Assert.False(hasKey "after" v, "the script went on after the rejected call")
    | _ -> ()

let private platformTexts =
    [ "Value was either too large"; "Arithmetic operation resulted in an overflow"; "Index was out of range"
      "Specified argument was out of the range"; "Input string was not in a correct format"; "System." ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-336")>]
[<MemberData(nameof rejecting)>]
let ``JYRO-REQ-336 RejectedArgument_FailsAsAJyroRuntimeErrorWithItsCodeNotAHostError`` (call: string, code: string) =
    let r =
        try runCall call
        with e -> Values.fail (sprintf "%s let a host exception escape: %s" call (e.ToString()))
    let m = Script.onlyErrorMessage r
    Assert.Equal(Enum.Parse<MessageCode> code, m.Code)
    for generic in [ MessageCode.RuntimeError; MessageCode.HostFunctionError; MessageCode.UnknownCompilerError; MessageCode.InternalStageFailure ] do
        Assert.NotEqual(generic, m.Code)
    for text in platformTexts do
        Assert.DoesNotContain(text, m.Message)

// ===========================================================================
// 90.3.8 Which check raises when several fail
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-338")>]
[<InlineData("Substring(\"ab\", 5, -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"ab\", 5, 1.5)", "IntegerRequired")>]
[<InlineData("Substring(\"ab\", 5, \"x\")", "ArgumentTypeMismatch")>]
[<InlineData("Insert([1], 5, 0)", "IndexOutOfRange")>]
[<InlineData("Insert([1], 5.5, 0)", "IntegerRequired")>]
[<InlineData("RemoveAt(null, 3000000000)", "IntegerOutOfRange")>]
[<InlineData("Substring([1], -1)", "ArgumentTypeMismatch")>]
[<InlineData("Substring(\"ab\", -1, -1)", "NonNegativeIntegerRequired")>]
[<InlineData("Substring(\"ab\", 1.5, -1)", "IntegerRequired")>]
[<InlineData("PadLeft(\"a\", -1, [1])", "NonNegativeIntegerRequired")>]
[<InlineData("PadLeft(\"a\", 2000000, [1])", "GeneratedLengthExceeded")>]
[<InlineData("Range(0, \"x\", 0)", "ArgumentTypeMismatch")>]
[<InlineData("Range(0, 1000001, 0)", "InvalidRange")>]
[<InlineData("Map(5, (a, b) => a)", "ArgumentTypeMismatch")>]
[<InlineData("Map([1], 5)", "CallbackExpected")>]
[<InlineData("Sum([1e308, 1e308, \"a\"])", "NumericElementRequired")>]
[<InlineData("Map([1, \"a\"], x => x * 2)", "UnsupportedBinaryOperation")>]
[<InlineData("Substring(\"ab\", 3, null)", "IndexOutOfRange")>]
let ``JYRO-REQ-338 SeveralFailingChecks_OnlyTheFirstInTheOrderRaises`` (call: string, code: string) =
    Script.expectError (Enum.Parse<MessageCode> code) (runCall call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-338")>]
[<InlineData("Substring(\"abc\", 1, null)", "\"bc\"")>]
[<InlineData("Slice([1, 2, 3], 1, null)", "[2,3]")>]
[<InlineData("PadLeft(\"a\", 3, null)", "\"  a\"")>]
[<InlineData("Substring(\"abc\", 3)", "\"\"")>]
let ``JYRO-REQ-338 OptionalArgument_AbsentOrNull_IsNotChecked`` (call: string, expected: string) =
    let v = outProp "r" (runCall call)
    Assert.True(v.EqualsValue(Values.json expected), Values.toJson v)

// ===========================================================================
// 90.3.9 The order of the checks on one argument
// ===========================================================================

let oneArgument () : obj[] seq =
    let rowsFor label values = [ for (v, c) in values -> [| box (callOf label v); box c |] ]
    let typeRows withNull =
        [ yield "\"5\"", "ArgumentTypeMismatch"
          if withNull then yield "null", "ArgumentTypeMismatch"
          yield "true", "ArgumentTypeMismatch"
          yield "[]", "ArgumentTypeMismatch" ]
    let numericRows =
        [ "-1.5", "IntegerRequired"; "0.5", "IntegerRequired"; "1e-10", "IntegerRequired"
          "-3000000000", "IntegerOutOfRange"; "1e20", "IntegerOutOfRange"; "-1e20", "IntegerOutOfRange"
          "2147483648", "IntegerOutOfRange" ]
    [ yield! rowsFor "PadLeft.length" (typeRows true @ numericRows @ [ "-1", "NonNegativeIntegerRequired"; "1000001", "GeneratedLengthExceeded" ])
      yield! rowsFor "Skip.count" (typeRows true @ numericRows @ [ "-1", "NonNegativeIntegerRequired" ])
      yield! rowsFor "Substring.length" (typeRows false @ numericRows @ [ "-1", "NonNegativeIntegerRequired" ])
      yield! rowsFor "Insert.index" (typeRows true @ numericRows)
      yield! rowsFor "RandomString.length" [ "1000001", "GeneratedLengthExceeded"; "-1", "NonNegativeIntegerRequired" ]
      yield [| box "Map([1], 5)"; box "CallbackExpected" |]
      yield [| box "Sum([\"a\", true])"; box "NumericElementRequired" |] ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-339")>]
[<MemberData(nameof oneArgument)>]
let ``ChecksOnOneArgument_RunInTheListedOrder_AndOnlyTheFirstFailureRaises`` (call: string, code: string) =
    Script.expectError (Enum.Parse<MessageCode> code) (runCall call)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-339")>]
let ``ElementChecks_RunFromPositionZeroUpward`` () =
    let m = Script.onlyErrorMessage (runCall "Sum([\"a\", true])")
    Assert.Equal(MessageCode.NumericElementRequired, m.Code)
    Assert.Equal(2, m.Args.Length)
    Assert.Equal(0L, Convert.ToInt64(match m.Args.[1] with :? JyroNumber as n -> box n.Value | o -> o))

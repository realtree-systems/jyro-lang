/// jyro-spec 91.12 PadLeft, 91.13 PadRight (and 91.12.7, the generated-length
/// maximum shared with RandomString).
module RealTree.Jyro.Spec.Tests.P91.S91_12_13_Pad

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P91.Support

let badValues = badData

let code (name: string) = Enum.Parse<MessageCode>(name)

/// 160.7.7: `InvalidPadCharacter` is 5705. Reached by number, because this
/// build may not declare the member yet (4.1.7); 160.7.7 pins the member.
let invalidPadCharacter : MessageCode = enum<MessageCode> 5705

/// The book's padChar values that are not one code unit (91.12.5, 91.13.5),
/// each in every position of the length: padding needed, padding not needed,
/// and a null text (read as "", 91.12.10).
let notOneCodeUnit : obj[] seq =
    [ for pad in [ "\"xy\""; "\"\""; "10"; "true"; "\"\U0001F600\""; "\"\\uD83D\\uDE00\""; "\"ab\""; "1.5"; "false" ] do
          for (text, length) in [ "\"a\"", "3"; "\"abc\"", "1"; "\"abc\"", "3"; "null", "3"; "\"a\"", "0" ] ->
              [| box text; box length; box pad |] ]

/// The order of the checks of 90.3.8 and 90.3.9 when padChar is also wrong:
/// every check of text and length comes before InvalidPadCharacter.
let orderOfChecks : obj[] seq =
    [ "\"a\"", "3", "[\"x\"]", "ArgumentTypeMismatch"            // padChar row 1 (91.12.12, 91.13.11)
      "\"a\"", "-1", "\"xy\"", "NonNegativeIntegerRequired"      // length row 4 (91.12.9, 91.13.8)
      "\"a\"", "1.5", "\"xy\"", "IntegerRequired"                // length row 2
      "\"a\"", "3000000000", "\"xy\"", "IntegerOutOfRange"       // length row 3 (90.3.2)
      "\"a\"", "\"3\"", "\"xy\"", "ArgumentTypeMismatch"         // length row 1 (91.12.14, 91.13.13)
      "\"a\"", "null", "\"xy\"", "ArgumentTypeMismatch"          // length row 1
      "\"a\"", "2000000", "\"xy\"", "GeneratedLengthExceeded"    // length row 5 (91.12.6, 91.13.6)
      "\"a\"", "1000001", "\"\"", "GeneratedLengthExceeded"      // length row 5, just above the maximum
      "[1]", "3", "\"xy\"", "ArgumentTypeMismatch"               // text row 1
      "{}", "-1", "\"xy\"", "ArgumentTypeMismatch" ]             // text first of all
    |> Seq.map (fun (t, l, p, c) -> [| box t; box l; box p; box c |])

/// Bytes allocated on this thread while `body` runs.
let allocatedBy (body: unit -> 'T) : 'T * int64 =
    let before = GC.GetAllocatedBytesForCurrentThread()
    let v = body ()
    v, GC.GetAllocatedBytesForCurrentThread() - before

/// Compile `Output.r = [<fn>("a", Input.n)]` once; then the run for `n`, with the
/// bytes this thread allocated during the run and the seconds it took.
let padRunCost (fn: string) : (int64 -> JyroResult<JyroValue> * int64 * float) =
    let compiled = Script.compile (script "" (fn + "(\"a\", Input.n)"))
    Assert.True(Script.ok compiled, Script.describe compiled)
    let program = Script.valueOf compiled
    fun (n: int64) ->
        let options = { Script.defaults with Input = ValueInput(Values.obj [ "n", Values.exact (decimal n) ]) }
        let runner = Script.runnerFor options program
        let sw = Diagnostics.Stopwatch.StartNew()
        let result, bytes = allocatedBy (fun () -> runner.Execute())
        result, bytes, sw.Elapsed.TotalSeconds

/// The shared checks of 91.12.6 / 91.13.6: a length over the maximum fails
/// with GeneratedLengthExceeded before the result is allocated.
let assertRejectedBeforeAllocation (fn: string) =
    let cost = padRunCost fn
    // warm up, then a baseline run that pads little
    cost 2L |> ignore
    let okResult, baseline, _ = cost 2L
    Assert.True(Script.ok okResult, Script.describe okResult)
    let over, overBytes, _ = cost 1000001L
    Script.expectError MessageCode.GeneratedLengthExceeded over
    // a 1,000,001-code-unit result is about 2 MB; allocating it would show here
    Assert.True(overBytes - baseline < 1_000_000L,
                sprintf "%s(\"a\", 1000001) allocated %d bytes more than a small pad: the result was allocated" fn (overBytes - baseline))
    let huge, hugeBytes, seconds = cost 2147483647L
    Script.expectError MessageCode.GeneratedLengthExceeded huge
    Assert.True(seconds < 1.0, sprintf "%s(\"a\", 2147483647) took %.2fs" fn seconds)
    Assert.True(hugeBytes - baseline < 1_000_000L,
                sprintf "%s(\"a\", 2147483647) allocated %d bytes more than a small pad" fn (hugeBytes - baseline))

// ============================================================================
// 91.12 PadLeft
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-493")>]
let PadLeft_PositionalNamedAndOptionalPadChar_ReturnString () =
    Assert.Equal("  a", str "PadLeft(\"a\", 3)")
    Assert.Equal("xxa", str "PadLeft(\"a\", 3, \"x\")")
    Assert.Equal("xxa", str "PadLeft(text: \"a\", length: 3, padding: \"x\")")
    Assert.Equal("xxa", str "PadLeft(padding: \"x\", length: 3, text: \"a\")")
    Assert.Equal("  a", str "PadLeft(text: \"a\", length: 3)")
    Assert.Equal("string", typeOf "PadLeft(\"a\", 3)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-493")>]
let PadLeft_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "PadLeft(\"a\")"
    expectErr MessageCode.TooManyArguments "PadLeft(\"a\", 3, \"x\", \"y\")"
    expectErrAmong MessageCode.UnknownNamedArgument "PadLeft(text: \"a\", width: 3)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1997")>]
[<InlineData("PadLeft(\"5\", 3, \"0\")", "005")>]
[<InlineData("PadLeft(\"abc\", 4, \"*\")", "*abc")>]
[<InlineData("PadLeft(\"\", 3, \"-\")", "---")>]
[<InlineData("PadLeft(\"ab\", 10, \".\")", "........ab")>]
let PadLeft_LengthOverTextLength_PrependsPadChars (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1997")>]
let PadLeft_LoneSurrogatePadChar_RepeatedAsCodeUnit () =
    let input = surrogates ()
    Assert.Equal(hi + hi + "a", strIn input "PadLeft(\"a\", 3, Input.hi)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-495")>]
[<InlineData("PadLeft(\"abc\", 3)", "abc")>]
[<InlineData("PadLeft(\"abc\", 2)", "abc")>]
[<InlineData("PadLeft(\"abc\", 0)", "abc")>]
[<InlineData("PadLeft(\"abc\", 1, \"x\")", "abc")>]
[<InlineData("PadLeft(\"\", 0)", "")>]
let PadLeft_LengthAtOrBelowTextLength_ReturnsTextUntruncated (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1998")>]
[<InlineData("PadLeft(\"a\", 3)", "  a")>]
[<InlineData("PadLeft(\"a\", 3, null)", "  a")>]
[<InlineData("PadLeft(\"a\", 3, Input.missing)", "  a")>]
[<InlineData("PadLeft(text: \"a\", length: 3, padding: null)", "  a")>]
[<InlineData("PadLeft(\"\", 2, null)", "  ")>]
[<InlineData("PadLeft(null, 2, null)", "  ")>]
[<InlineData("PadLeft(\"abc\", 1, null)", "abc")>]
let PadLeft_AbsentOrNullPadding_PadsWithSpace (expr: string, expected: string) =
    let actual = str expr
    Assert.Equal(expected, actual)
    Assert.True(actual |> Seq.take (actual.Length - expected.TrimStart(' ').Length) |> Seq.forall (fun c -> c = '\u0020'))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1999")>]
[<MemberData(nameof notOneCodeUnit)>]
let ``JYRO-REQ-1999 PadLeft_PaddingNotOneCodeUnit_FailsAtRuntime`` (text: string, length: string, pad: string) =
    expectRuntimeErr invalidPadCharacter (sprintf "PadLeft(%s, %s, %s)" text length pad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1999")>]
let ``JYRO-REQ-1999 PadLeft_PaddingNotOneCodeUnit_TheBookExamples`` () =
    for pad in [ "\"xy\""; "\"\""; "10"; "true"; "\"\U0001F600\"" ] do
        expectRuntimeErr invalidPadCharacter (sprintf "PadLeft(\"a\", 3, %s)" pad)
    // a padding given by name, and one from Input, are checked alike
    expectRuntimeErr invalidPadCharacter "PadLeft(text: \"a\", length: 3, padding: \"xy\")"
    expectRuntimeError invalidPadCharacter (runInB (surrogates ()) "PadLeft(\"a\", 3, Input.pair)")

/// InvalidPadCharacter is a runtime error: its number is 5705 (160.7.7) and 160.1.12 gives it
/// the stage Execute. Moved here from JYRO-REQ-1618's tests (JYRO-ADR-113 item 2,
/// JYRO-DEF-506): this requirement raises the code, so it depends on 1618 and 1680.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1999")>]
let ``JYRO-REQ-1999 PadLeft_InvalidPadCharacter_Is5705AtTheStageExecute`` () =
    let m = Script.onlyErrorMessage (Script.run "Output.r = PadLeft(\"a\", 3, \"xy\")")
    Assert.Equal(5705, int m.Code)
    Assert.Equal("Execute", Pending.stageOf m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1999")>]
let ``JYRO-REQ-1999 PadLeft_PaddingOfOneCodeUnit_Accepted`` () =
    Assert.Equal("xxa", strB "PadLeft(\"a\", 3, \"x\")")
    Assert.Equal("77a", strB "PadLeft(\"a\", 3, 7)")
    Assert.Equal("\u00E9\u00E9a", strB "PadLeft(\"a\", 3, \"\\u00E9\")")
    Assert.Equal(lo + "a", strInB (surrogates ()) "PadLeft(\"a\", 2, Input.lo)")
    Assert.Equal("abc", strB "PadLeft(\"abc\", 1, \"x\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1999")>]
[<MemberData(nameof orderOfChecks)>]
let ``JYRO-REQ-1999 PadLeft_TheOrderOfItsChecks`` (text: string, length: string, pad: string, name: string) =
    expectRuntimeErr (code name) (sprintf "PadLeft(%s, %s, %s)" text length pad)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1999")>]
[<InlineData("PadLeft(\"a\", 3, null)", "  a")>]
[<InlineData("PadLeft(\"abc\", 1, null)", "abc")>]
[<InlineData("PadLeft(null, 2, null)", "  ")>]
[<InlineData("PadLeft(\"a\", 3, Input.missing)", "  a")>]
let ``JYRO-REQ-1999 PadLeft_NullPadding_UsesSpace`` (expr: string, expected: string) =
    Assert.Equal(expected, strB expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-498")>]
let PadLeft_LengthOverMaximum_FailsWithGeneratedLengthExceededBeforeAllocating () =
    expectErr MessageCode.GeneratedLengthExceeded "PadLeft(\"a\", 1000001)"
    expectErr MessageCode.GeneratedLengthExceeded "PadLeft(\"a\", 1000001, \"x\")"
    expectErr MessageCode.GeneratedLengthExceeded "PadLeft(\"a\", 1000001, \"ab\")"
    assertRejectedBeforeAllocation "PadLeft"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-498")>]
[<InlineData("PadLeft(\"a\", 1000000.5)", "IntegerRequired")>]
[<InlineData("PadLeft(\"a\", 3000000000)", "IntegerOutOfRange")>]
[<InlineData("PadLeft([], 1000001)", "ArgumentTypeMismatch")>]
let PadLeft_LengthOverMaximum_EarlierChecksComeFirst (expr: string, name: string) =
    expectErr (code name) expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-499")>]
let GeneratedLengthMaximum_IsOneMillionForPadAndRandomString () =
    Assert.Equal(1000000, (str "PadLeft(\"\", 1000000)").Length)
    Assert.Equal(1000000, (str "PadRight(\"\", 1000000)").Length)
    Assert.Equal(1000000, (str "RandomString(1000000)").Length)
    expectErr MessageCode.GeneratedLengthExceeded "PadLeft(\"\", 1000001)"
    expectErr MessageCode.GeneratedLengthExceeded "PadRight(\"\", 1000001)"
    expectErr MessageCode.GeneratedLengthExceeded "RandomString(1000001)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-500")>]
let PadLeft_TextLongerThanMaximumAndLengthNot_ReturnsTextUnchanged () =
    let pre = "var t = PadLeft(\"\", 1000000, \"a\") & \"bc\""
    Assert.True(Values.asBool (evalPre pre "PadLeft(t, 5) == t"))
    Assert.True(Values.asBool (evalPre pre "PadLeft(t, 1000000) == t"))
    Assert.True(Values.asBool (evalPre pre "PadLeft(t, 1000000, \"x\") == t"))
    let longText = String('a', 1000002)
    Assert.Equal(longText, strIn (inputOf [ "t", longText ]) "PadLeft(Input.t, 1000000)")
    expectErrPre MessageCode.GeneratedLengthExceeded pre "PadLeft(t, 1000001)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1759")>]
[<InlineData("-1", "NonNegativeIntegerRequired")>]
[<InlineData("-2147483648", "NonNegativeIntegerRequired")>]
[<InlineData("1.5", "IntegerRequired")>]
[<InlineData("0.5", "IntegerRequired")>]
[<InlineData("-1.5", "IntegerRequired")>]
[<InlineData("3000000000", "IntegerOutOfRange")>]
[<InlineData("-3000000000", "IntegerOutOfRange")>]
[<InlineData("2147483648", "IntegerOutOfRange")>]
[<InlineData("-2147483649", "IntegerOutOfRange")>]
[<InlineData("1e20", "IntegerOutOfRange")>]
let ``JYRO-REQ-1759 PadLeft_LengthNotNonNegativeInteger_FailsWith90_3Code`` (length: string, name: string) =
    expectErr (code name) (sprintf "PadLeft(\"a\", %s)" length)
    expectErr (code name) (sprintf "PadLeft(\"a\", %s, \"*\")" length)

/// The boundaries of the "exactly when" sets: the largest and the smallest
/// integer of the range, and zero, raise none of the three codes of the check.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1759")>]
[<InlineData("2147483647")>]
[<InlineData("0")>]
[<InlineData("2")>]
let ``JYRO-REQ-1759 PadLeft_LengthIntegerInRangeNotNegative_RaisesNoneOfTheThreeCodes`` (length: string) =
    let result = run (sprintf "PadLeft(\"a\", %s)" length)
    let codes = Script.errorCodes result
    Assert.DoesNotContain(MessageCode.IntegerRequired, codes)
    Assert.DoesNotContain(MessageCode.IntegerOutOfRange, codes)
    Assert.DoesNotContain(MessageCode.NonNegativeIntegerRequired, codes)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1759")>]
[<InlineData("0", "a")>]
[<InlineData("1", "a")>]
[<InlineData("3", "  a")>]
[<InlineData("3.0", "  a")>]
[<InlineData("Power(9, 0.5)", "  a")>]
let ``JYRO-REQ-1759 PadLeft_NonNegativeIntegerLength_Accepted`` (length: string, expected: string) =
    Assert.Equal(expected, str (sprintf "PadLeft(\"a\", %s)" length))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2146")>]
[<InlineData("PadLeft(null, 3)", "   ")>]
[<InlineData("PadLeft(null, 0)", "")>]
[<InlineData("PadLeft(null, 2, \"x\")", "xx")>]
[<InlineData("PadLeft(null, 3, null)", "   ")>]
[<InlineData("PadLeft(Input.missing, 2, \"x\")", "xx")>]
let ``JYRO-REQ-2146 PadLeft_NullText_ReadAsEmptyString`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

/// A null `text` gives what the empty string gives, for the same other arguments.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2146")>]
[<InlineData("5", "\"x\"")>]
[<InlineData("0", "\"x\"")>]
[<InlineData("3", "null")>]
let ``JYRO-REQ-2146 PadLeft_NullText_GivesWhatEmptyTextGives`` (length: string, padding: string) =
    let withNull = str (sprintf "PadLeft(null, %s, %s)" length padding)
    let withEmpty = str (sprintf "PadLeft(\"\", %s, %s)" length padding)
    Assert.Equal(withEmpty, withNull)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2146")>]
let ``JYRO-REQ-2146 PadLeft_NullPadChar_NotReadAsEmptyString`` () =
    Assert.Equal("  a", str "PadLeft(\"a\", 3, null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2000")>]
[<InlineData("PadLeft(7, 3, \"0\")", "007")>]
[<InlineData("PadLeft(1.50, 5, \"0\")", "001.5")>]
[<InlineData("PadLeft(true, 6)", "  true")>]
[<InlineData("PadLeft(false, 5)", "false")>]
[<InlineData("PadLeft(\"1\", 3, 0)", "001")>]
[<InlineData("PadLeft(\"1\", 3, 9)", "991")>]
[<InlineData("PadLeft(ToNumber(\"1e30\"), 7)", "  1e+30")>]
let ``JYRO-REQ-2000 PadLeft_NumberOrBooleanTextOrPadChar_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2000")>]
let ``JYRO-REQ-2000 PadLeft_BooleanOrLongNumberPadChar_ReadAsStringFormAndSoTooLong`` () =
    // "true", "false", "1.5" and "10" are not one code unit: InvalidPadCharacter (91.12.5)
    for pad in [ "true"; "false"; "1.5"; "10" ] do
        expectRuntimeErr invalidPadCharacter (sprintf "PadLeft(\"a\", 3, %s)" pad)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2001")>]
[<MemberData(nameof badValues)>]
let PadLeft_ContainerOrFunctionTextOrPadChar_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadLeft(%s, 3)" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadLeft(\"a\", 3, %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-505")>]
let PadLeft_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal(" a", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "PadLeft(\"a\", 2)"))

/// Length values that are not numbers.
let nonNumberLengths : obj[] seq =
    [ "null"; "Input.missing"; "\"3\""; "\"-1\""; "\"1.5\""; "\"\""; "true"; "false"; "[]"; "[3]"; "{}"; "{a: 3}"; "x => x" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-506")>]
[<MemberData(nameof nonNumberLengths)>]
let PadLeft_LengthNotNumber_FailsWithArgumentTypeMismatch (length: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadLeft(\"a\", %s)" length)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadLeft(\"a\", %s, \"*\")" length)

// ============================================================================
// 91.13 PadRight
// ============================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-507")>]
let PadRight_PositionalNamedAndOptionalPadChar_ReturnString () =
    Assert.Equal("a  ", str "PadRight(\"a\", 3)")
    Assert.Equal("axx", str "PadRight(\"a\", 3, \"x\")")
    Assert.Equal("axx", str "PadRight(text: \"a\", length: 3, padding: \"x\")")
    Assert.Equal("axx", str "PadRight(padding: \"x\", length: 3, text: \"a\")")
    Assert.Equal("a  ", str "PadRight(text: \"a\", length: 3)")
    Assert.Equal("string", typeOf "PadRight(\"a\", 3)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-507")>]
let PadRight_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "PadRight(\"a\")"
    expectErr MessageCode.TooManyArguments "PadRight(\"a\", 3, \"x\", \"y\")"
    expectErrAmong MessageCode.UnknownNamedArgument "PadRight(text: \"a\", width: 3)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2002")>]
[<InlineData("PadRight(\"5\", 3, \"0\")", "500")>]
[<InlineData("PadRight(\"abc\", 4, \"*\")", "abc*")>]
[<InlineData("PadRight(\"\", 3, \"-\")", "---")>]
[<InlineData("PadRight(\"ab\", 10, \".\")", "ab........")>]
let PadRight_LengthOverTextLength_AppendsPadChars (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2002")>]
let PadRight_LoneSurrogatePadChar_RepeatedAsCodeUnit () =
    Assert.Equal("a" + lo + lo, strIn (surrogates ()) "PadRight(\"a\", 3, Input.lo)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-509")>]
[<InlineData("PadRight(\"abc\", 3)", "abc")>]
[<InlineData("PadRight(\"abc\", 2)", "abc")>]
[<InlineData("PadRight(\"abc\", 0)", "abc")>]
[<InlineData("PadRight(\"abc\", 1, \"x\")", "abc")>]
[<InlineData("PadRight(\"\", 0)", "")>]
let PadRight_LengthAtOrBelowTextLength_ReturnsTextUntruncated (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2003")>]
[<InlineData("PadRight(\"a\", 3)", "a  ")>]
[<InlineData("PadRight(\"a\", 3, null)", "a  ")>]
[<InlineData("PadRight(\"a\", 3, Input.missing)", "a  ")>]
[<InlineData("PadRight(text: \"a\", length: 3, padding: null)", "a  ")>]
[<InlineData("PadRight(\"\", 2, null)", "  ")>]
[<InlineData("PadRight(null, 2, null)", "  ")>]
[<InlineData("PadRight(\"abc\", 1, null)", "abc")>]
let PadRight_AbsentOrNullPadding_PadsWithSpace (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2004")>]
[<MemberData(nameof notOneCodeUnit)>]
let ``JYRO-REQ-2004 PadRight_PaddingNotOneCodeUnit_FailsAtRuntime`` (text: string, length: string, pad: string) =
    expectRuntimeErr invalidPadCharacter (sprintf "PadRight(%s, %s, %s)" text length pad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2004")>]
let ``JYRO-REQ-2004 PadRight_PaddingNotOneCodeUnit_TheBookExamples`` () =
    for pad in [ "\"xy\""; "\"\""; "10"; "true"; "\"\U0001F600\"" ] do
        expectRuntimeErr invalidPadCharacter (sprintf "PadRight(\"a\", 3, %s)" pad)
    expectRuntimeErr invalidPadCharacter "PadRight(text: \"a\", length: 3, padding: \"xy\")"
    expectRuntimeError invalidPadCharacter (runInB (surrogates ()) "PadRight(\"a\", 3, Input.pair)")

/// As the PadLeft test of 5705 at the stage Execute, for PadRight (moved from
/// JYRO-REQ-1618's tests; this requirement depends on 1618 and 1680).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2004")>]
let ``JYRO-REQ-2004 PadRight_InvalidPadCharacter_Is5705AtTheStageExecute`` () =
    let m = Script.onlyErrorMessage (Script.run "Output.r = PadRight(\"a\", 3, \"xy\")")
    Assert.Equal(5705, int m.Code)
    Assert.Equal("Execute", Pending.stageOf m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2004")>]
let ``JYRO-REQ-2004 PadRight_PaddingOfOneCodeUnit_Accepted`` () =
    Assert.Equal("axx", strB "PadRight(\"a\", 3, \"x\")")
    Assert.Equal("a77", strB "PadRight(\"a\", 3, 7)")
    Assert.Equal("a\u00E9\u00E9", strB "PadRight(\"a\", 3, \"\\u00E9\")")
    Assert.Equal("a" + hi, strInB (surrogates ()) "PadRight(\"a\", 2, Input.hi)")
    Assert.Equal("abc", strB "PadRight(\"abc\", 1, \"x\")")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2004")>]
[<MemberData(nameof orderOfChecks)>]
let ``JYRO-REQ-2004 PadRight_TheOrderOfItsChecks`` (text: string, length: string, pad: string, name: string) =
    expectRuntimeErr (code name) (sprintf "PadRight(%s, %s, %s)" text length pad)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2004")>]
[<InlineData("PadRight(\"a\", 3, null)", "a  ")>]
[<InlineData("PadRight(\"abc\", 1, null)", "abc")>]
[<InlineData("PadRight(null, 2, null)", "  ")>]
[<InlineData("PadRight(\"a\", 3, Input.missing)", "a  ")>]
let ``JYRO-REQ-2004 PadRight_NullPadding_UsesSpace`` (expr: string, expected: string) =
    Assert.Equal(expected, strB expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-512")>]
let PadRight_LengthOverMaximum_FailsWithGeneratedLengthExceededBeforeAllocating () =
    expectErr MessageCode.GeneratedLengthExceeded "PadRight(\"a\", 1000001)"
    expectErr MessageCode.GeneratedLengthExceeded "PadRight(\"a\", 1000001, \"x\")"
    expectErr MessageCode.GeneratedLengthExceeded "PadRight(\"a\", 1000001, \"ab\")"
    Assert.Equal(1000000, (str "PadRight(\"a\", 1000000)").Length)
    assertRejectedBeforeAllocation "PadRight"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-512")>]
[<InlineData("PadRight(\"a\", 1000000.5)", "IntegerRequired")>]
[<InlineData("PadRight(\"a\", 3000000000)", "IntegerOutOfRange")>]
[<InlineData("PadRight({}, 1000001)", "ArgumentTypeMismatch")>]
let PadRight_LengthOverMaximum_EarlierChecksComeFirst (expr: string, name: string) =
    expectErr (code name) expr

[<Fact>]
[<Trait("requirement", "JYRO-REQ-513")>]
let PadRight_TextLongerThanMaximumAndLengthNot_ReturnsTextUnchanged () =
    let pre = "var t = PadRight(\"\", 1000000, \"a\") & \"bc\""
    Assert.True(Values.asBool (evalPre pre "PadRight(t, 5) == t"))
    Assert.True(Values.asBool (evalPre pre "PadRight(t, 1000000) == t"))
    let longText = String('a', 1000002)
    Assert.Equal(longText, strIn (inputOf [ "t", longText ]) "PadRight(Input.t, 1000000)")
    expectErrPre MessageCode.GeneratedLengthExceeded pre "PadRight(t, 1000001)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1761")>]
[<InlineData("-1", "NonNegativeIntegerRequired")>]
[<InlineData("-2147483649", "IntegerOutOfRange")>]
[<InlineData("-2147483648", "NonNegativeIntegerRequired")>]
[<InlineData("1.5", "IntegerRequired")>]
[<InlineData("0.5", "IntegerRequired")>]
[<InlineData("-1.5", "IntegerRequired")>]
[<InlineData("3000000000", "IntegerOutOfRange")>]
[<InlineData("-3000000000", "IntegerOutOfRange")>]
[<InlineData("2147483648", "IntegerOutOfRange")>]
[<InlineData("1e20", "IntegerOutOfRange")>]
let ``JYRO-REQ-1761 PadRight_LengthNotNonNegativeInteger_FailsWith90_3Code`` (length: string, name: string) =
    expectErr (code name) (sprintf "PadRight(\"a\", %s)" length)
    expectErr (code name) (sprintf "PadRight(\"a\", %s, \"*\")" length)

/// The boundaries of the "exactly when" sets raise none of the three codes.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1761")>]
[<InlineData("2147483647")>]
[<InlineData("0")>]
[<InlineData("2")>]
let ``JYRO-REQ-1761 PadRight_LengthIntegerInRangeNotNegative_RaisesNoneOfTheThreeCodes`` (length: string) =
    let result = run (sprintf "PadRight(\"a\", %s)" length)
    let codes = Script.errorCodes result
    Assert.DoesNotContain(MessageCode.IntegerRequired, codes)
    Assert.DoesNotContain(MessageCode.IntegerOutOfRange, codes)
    Assert.DoesNotContain(MessageCode.NonNegativeIntegerRequired, codes)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1761")>]
[<InlineData("0", "a")>]
[<InlineData("1", "a")>]
[<InlineData("3", "a  ")>]
[<InlineData("3.0", "a  ")>]
[<InlineData("Power(9, 0.5)", "a  ")>]
let ``JYRO-REQ-1761 PadRight_NonNegativeIntegerLength_Accepted`` (length: string, expected: string) =
    Assert.Equal(expected, str (sprintf "PadRight(\"a\", %s)" length))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2147")>]
[<InlineData("PadRight(null, 3)", "   ")>]
[<InlineData("PadRight(null, 0)", "")>]
[<InlineData("PadRight(null, 2, \"x\")", "xx")>]
[<InlineData("PadRight(null, 3, null)", "   ")>]
[<InlineData("PadRight(Input.missing, 2, \"x\")", "xx")>]
let ``JYRO-REQ-2147 PadRight_NullText_ReadAsEmptyString`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

/// A null `text` gives what the empty string gives, for the same other arguments.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-2147")>]
[<InlineData("5", "\"x\"")>]
[<InlineData("0", "\"x\"")>]
[<InlineData("3", "null")>]
let ``JYRO-REQ-2147 PadRight_NullText_GivesWhatEmptyTextGives`` (length: string, padding: string) =
    let withNull = str (sprintf "PadRight(null, %s, %s)" length padding)
    let withEmpty = str (sprintf "PadRight(\"\", %s, %s)" length padding)
    Assert.Equal(withEmpty, withNull)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2147")>]
let ``JYRO-REQ-2147 PadRight_NullPadChar_NotReadAsEmptyString`` () =
    Assert.Equal("a  ", str "PadRight(\"a\", 3, null)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2005")>]
[<InlineData("PadRight(7, 3, \"0\")", "700")>]
[<InlineData("PadRight(1.50, 5, \"0\")", "1.500")>]
[<InlineData("PadRight(false, 6)", "false ")>]
[<InlineData("PadRight(true, 4)", "true")>]
[<InlineData("PadRight(\"1\", 3, 0)", "100")>]
[<InlineData("PadRight(ToNumber(\"1e30\"), 7)", "1e+30  ")>]
let ``JYRO-REQ-2005 PadRight_NumberOrBooleanTextOrPadChar_ReadAsStringForm`` (expr: string, expected: string) =
    Assert.Equal(expected, str expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2005")>]
let ``JYRO-REQ-2005 PadRight_BooleanPadChar_ReadAsStringFormAndSoTooLong`` () =
    // "true", "false", "1.5" and "10" are not one code unit: InvalidPadCharacter (91.13.5)
    for pad in [ "true"; "false"; "1.5"; "10" ] do
        expectRuntimeErr invalidPadCharacter (sprintf "PadRight(\"a\", 3, %s)" pad)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2006")>]
[<MemberData(nameof badValues)>]
let PadRight_ContainerOrFunctionTextOrPadChar_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadRight(%s, 3)" bad)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadRight(\"a\", 3, %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-518")>]
let PadRight_UnderPureCeiling_CompilesAndRuns () =
    Assert.Equal("a ", Values.asString (evalUnderMaxEffect FunctionEffect.Pure "PadRight(\"a\", 2)"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-519")>]
[<MemberData(nameof nonNumberLengths)>]
let PadRight_LengthNotNumber_FailsWithArgumentTypeMismatch (length: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadRight(\"a\", %s)" length)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "PadRight(\"a\", %s, \"*\")" length)

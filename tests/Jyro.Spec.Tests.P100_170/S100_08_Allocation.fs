/// Part 100.8: no operation allocates in proportion to a number the script
/// gives, beyond the bounds of 100.8.4. Allocation is measured with
/// GC.GetAllocatedBytesForCurrentThread around the execution alone (the program
/// is compiled first), and a control run proves the execution allocates on the
/// measured thread.
module RealTree.Jyro.Spec.Tests.S100_08_Allocation

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// Compile `source`, then run it, measuring the bytes the run allocates on this thread.
let private runMeasured (source: string) : JyroResult<JyroValue> * int64 * TimeSpan =
    // A 2 second time limit keeps an unbounded allocation from running away.
    let program = Script.compile source |> Script.valueOf
    let runner =
        Script.runnerFor (withLimits (fun o -> { roomy o with MaxExecutionTime = TimeSpan.FromSeconds 2. })) program
    GC.Collect()
    bounded 9. (fun () ->
        let (r, bytes), elapsed = timed (fun () -> allocating (fun () -> runner.Execute()))
        r, bytes, elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1440")>]
let ``Control_AnExecutionThatAllocatesALot_IsSeenOnTheMeasuredThread`` () =
    let r, bytes, _ = runMeasured "Output.r = Length(PadLeft(\"a\", 1000000))"
    ok r |> ignore
    Assert.True(bytes > oneMB, sprintf "Only %d bytes were seen; the measurement cannot see the execution" bytes)

// -- 100.8.2 PadLeft, PadRight, RandomString share one maximum --------------------------

[<Theory>]
[<InlineData("PadLeft(\"a\", {L})")>]
[<InlineData("PadRight(\"a\", {L})")>]
[<InlineData("RandomString({L})")>]
[<InlineData("RandomString({L}, \"xy\")")>]
[<Trait("requirement", "JYRO-REQ-2048")>]
let ``Generator_AtAndBelowTheMaximum_GivesThatLength`` (call: string) =
    for l in [ 999_999; 1_000_000 ] do
        let v = Script.run ("Output.n = Length(" + call.Replace("{L}", string l) + ")") |> ok
        Assert.Equal(decimal l, Values.asDecimal (Values.prop "n" v))

[<Theory>]
[<InlineData("PadLeft(\"a\", {L})")>]
[<InlineData("PadRight(\"a\", {L})")>]
[<InlineData("RandomString({L})")>]
[<InlineData("RandomString({L}, \"xy\")")>]
[<Trait("requirement", "JYRO-REQ-2048")>]
let ``Generator_AboveTheMaximum_RaisesGeneratedLengthExceeded`` (call: string) =
    for l in [ 1_000_001; 2_147_483_647 ] do
        Script.run ("Output.n = Length(" + call.Replace("{L}", string l) + ")") |> fails MessageCode.GeneratedLengthExceeded

// -- 100.8.3 The error comes before the result is allocated ----------------------------------

[<Theory>]
[<InlineData("PadLeft(\"a\", 2147483647)", "GeneratedLengthExceeded")>]
[<InlineData("PadRight(\"a\", 2147483647)", "GeneratedLengthExceeded")>]
[<InlineData("RandomString(2147483647)", "GeneratedLengthExceeded")>]
[<InlineData("PadLeft(\"a\", 1000001)", "GeneratedLengthExceeded")>]
[<InlineData("Range(0, 2147483647)", "GeneratedLengthExceeded")>]
[<InlineData("Range(0, 1e300)", "GeneratedLengthExceeded")>]
// The step 1 / 1e300 is about 1e-300, inexact, without a literal below the exact range (JYRO-DEF-477).
[<InlineData("Range(0, 1, 1 / 1e300)", "GeneratedLengthExceeded")>]
[<InlineData("Range(0, 1000001)", "GeneratedLengthExceeded")>]
[<Trait("requirement", "JYRO-REQ-1440")>]
let ``OperationAboveItsBound_RaisesItsErrorBeforeAllocating`` (call: string, code: string) =
    let r, bytes, elapsed = runMeasured ("Output.r = " + call)
    fails (Enum.Parse<MessageCode> code) r
    Assert.True(bytes < oneMB, sprintf "%s allocated %d bytes before failing" call bytes)
    Assert.True(elapsed < TimeSpan.FromSeconds 2., sprintf "%s took %O" call elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1440")>]
let ``IndexAssignmentFarPastTheEnd_RaisesIndexOutOfRangeBeforeAllocating_AndLeavesTheArray`` () =
    let r, bytes, _ = runMeasured "var a = [1, 2]\nOutput.a = a\na[2147483647] = 0"
    fails MessageCode.IndexOutOfRange r
    Assert.True(bytes < oneMB, sprintf "allocated %d bytes" bytes)
    Assert.Equal("[1,2]", Values.toJson (resultProp "a" r))
    let r2, _, _ = runMeasured "var a = [1, 2]\nOutput.a = a\na[3] = 0"
    fails MessageCode.IndexOutOfRange r2
    Assert.Equal("[1,2]", Values.toJson (resultProp "a" r2))

// -- 100.8.4 Exactly these operations, with these bounds and errors ---------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2049")>]
let ``IndexAssignment_AtLengthAppends_AtLengthPlus1RaisesIndexOutOfRange`` () =
    let v = Script.run "var a = [1, 2]\na[2] = 3\nOutput.a = a" |> ok
    Assert.Equal("[1,2,3]", Values.toJson (Values.prop "a" v))
    Script.run "var a = [1, 2]\na[3] = 3\nOutput.a = a" |> fails MessageCode.IndexOutOfRange
    let e = Script.run "var a = []\na[0] = 1\nOutput.a = a" |> ok
    Assert.Equal("[1]", Values.toJson (Values.prop "a" e))
    Script.run "var a = []\na[1] = 1" |> fails MessageCode.IndexOutOfRange

[<Theory>]
[<InlineData("PadLeft(\"a\", {L})")>]
[<InlineData("PadRight(\"a\", {L})")>]
[<InlineData("RandomString({L})")>]
[<InlineData("Range(0, {L})")>]
[<Trait("requirement", "JYRO-REQ-2049")>]
let ``BoundedOperation_AtItsBound_Succeeds_AboveIt_RaisesGeneratedLengthExceeded`` (call: string) =
    let v = Script.run ("Output.n = Length(" + call.Replace("{L}", "1000000") + ")") |> ok
    Assert.Equal(1_000_000m, Values.asDecimal (Values.prop "n" v))
    Script.run ("Output.n = Length(" + call.Replace("{L}", "1000001") + ")") |> fails MessageCode.GeneratedLengthExceeded

/// Every other operation that takes a number, driven with an extreme number and
/// small operands.
let otherOperations : obj[] seq =
    [ "Substring(\"abc\", 0, 2147483647)"; "Substring(\"abc\", 2147483647)"; "Slice([1], 0, 2147483647)"
      "Slice([1], -2147483648, 2147483647)"; "Skip([1], 2147483647)"; "Insert([1], 2147483647, 0)"
      "RemoveAt([1], 2147483647)"; "[1][2147483647]"; "\"abc\"[2147483647]"; "Round(1, 2147483647)"
      "Round(1, -2147483648)"; "Round(1.5, 28)"; "Power(10, 1000000)"; "Power(2, 0.5)"; "Log(1e300)"
      "Clamp(1, -1e300, 1e300)"; "RandomInt(0, 2147483647)"; "RandomInt(-2147483648, 2147483647)"
      "DateAdd(\"2024-01-02\", 2147483647, \"day\")"; "ToString(1e300)"; "ToJson(1e-300)"; "Sleep(0)"
      "SquareRoot(1e300)"; "Absolute(-1e300)"; "Floor(1e300)"; "Ceiling(-1e300)"; "1e300 * 1e300"; "2147483647 % 3"
      "Join([1], \"x\")"; "Split(\"abc\", \"b\")"; "Replace(\"abc\", \"b\", \"bb\")" ]
    |> Seq.map (fun c -> [| box c |])

[<Theory>]
[<MemberData(nameof otherOperations)>]
[<Trait("requirement", "JYRO-REQ-1438")>]
[<Trait("requirement", "JYRO-REQ-2049")>]
let ``OtherOperation_WithAnExtremeNumber_AllocatesOnlyInProportionToItsOperands`` (call: string) =
    let r, bytes, elapsed = runMeasured ("Output.r = TypeOf(" + call + ")")
    Assert.DoesNotContain(MessageCode.GeneratedLengthExceeded, Script.codes r)
    Assert.True(bytes < oneMB, sprintf "%s allocated %d bytes" call bytes)
    Assert.True(elapsed < TimeSpan.FromSeconds 2., sprintf "%s took %O" call elapsed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1438")>]
let ``BoundedOperations_WithExtremeNumbers_AllocateNothingInProportion`` () =
    for call in [ "PadLeft(\"a\", 2147483647)"; "RandomString(2147483647)"; "Range(0, 2147483647)"
                  "Range(-2147483648, 2147483647)" ] do
        let r, bytes, _ = runMeasured ("Output.r = " + call)
        Assert.False(r.IsSuccess, call)
        Assert.True(bytes < oneMB, sprintf "%s allocated %d bytes" call bytes)

// -- 100.8.5 Range ---------------------------------------------------------------------------

[<Theory>]
[<InlineData("Range(0, 1000000)", 1000000)>]
[<InlineData("Range(1, 1000001)", 1000000)>]
[<InlineData("Range(0, -1000000, -1)", 1000000)>]
[<InlineData("Range(0, 1, 0.000001)", 1000000)>]
[<InlineData("Range(0.5, 1000000.5)", 1000000)>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``Range_OfAtMostAMillionElements_ReturnsThem`` (call: string, count: int) =
    let v = Script.run ("var r = " + call + "\nOutput.n = Length(r)\nOutput.first = r[0]\nOutput.last = r[Length(r) - 1]") |> ok
    Assert.Equal(decimal count, Values.asDecimal (Values.prop "n" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``RangeZeroToAMillion_FirstIs0_LastIs999999`` () =
    let v = Script.run "var r = Range(0, 1000000)\nOutput.first = r[0]\nOutput.last = r[999999]" |> ok
    Assert.Equal(0m, Values.asDecimal (Values.prop "first" v))
    Assert.Equal(999_999m, Values.asDecimal (Values.prop "last" v))

[<Theory>]
[<InlineData("Range(0, 1000001)")>]
[<InlineData("Range(0, 2147483647)")>]
[<InlineData("Range(0, 1000000.5)")>]
[<InlineData("Range(0, -1000001, -1)")>]
[<InlineData("Range(0, 1, 0.0000009)")>]
[<InlineData("Range(0, 1e300)")>]
// JYRO-DEF-477: the step made by division, not by the literal 1e-300 (JYRO-REQ-49).
[<InlineData("Range(0, 1, 1 / 1e300)")>]
[<InlineData("Range(-1000000, 1)")>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``Range_OfMoreThanAMillionElements_RaisesGeneratedLengthExceeded_WithoutAllocating`` (call: string) =
    let r, bytes, elapsed = runMeasured ("Output.r = " + call)
    fails MessageCode.GeneratedLengthExceeded r
    Assert.True(bytes < oneMB, sprintf "%s allocated %d bytes" call bytes)
    Assert.True(elapsed < TimeSpan.FromSeconds 2., sprintf "%s took %O" call elapsed)

[<Theory>]
[<InlineData("Range(0, 2147483647, 0)", "InvalidRange")>]
[<InlineData("Range(0, 1e300, 0)", "InvalidRange")>]
[<InlineData("Range(\"a\", 1e9)", "ArgumentTypeMismatch")>]
[<InlineData("Range(0, 1e9, \"x\")", "ArgumentTypeMismatch")>]
[<InlineData("Range(0, \"z\")", "ArgumentTypeMismatch")>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``Range_ChecksOfSingleArguments_ComeBeforeTheLengthCheck`` (call: string, code: string) =
    Script.run ("Output.r = " + call) |> fails (Enum.Parse<MessageCode> code)

// -- 100.8.5 The length check is decided at position 1,000,000, in the order of 92.13.16 -------
// Every literal is inexact (30.2.9); MAX is 1.7976931348623157e308. Every run
// sets all four limits (FLOORS 4.1.11).

/// Compile, then run with all four limits set, measuring the bytes the
/// execution allocates on this thread.
let private runPinnedMeasured (source: string) : JyroResult<JyroValue> * int64 =
    let program = Script.compileWith pinned source |> Script.valueOf
    let runner = Script.runnerFor pinned program
    GC.Collect()
    allocating (fun () -> runner.Execute())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``Range_NumberOverflowBeforePosition1000000_ComesBeforeGeneratedLengthExceeded`` () =
    // About 1,700,000 values are below finish, but k * step is an infinity at k = 898,847.
    let r, _ = runPinnedMeasured "Output.r = Range(-1.7e308, 1.7e308, 2e302)"
    ValueSize.failsWith MessageCode.NumberOverflow r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``Range_NumberOverflowAtPosition1000000_ComesBeforeGeneratedLengthExceeded`` () =
    let r, _ = runPinnedMeasured "Output.r = Range(-1.7976931348623157e308, 1.7976931348623157e308, 1.7976940337093327e302)"
    ValueSize.failsWith MessageCode.NumberOverflow r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1442")>]
let ``Range_OverflowOnlyAfterPosition1000000_RaisesGeneratedLengthExceeded_WithoutAllocating`` () =
    // At k = 1,000,000 the element is finite and below finish; the first
    // overflow would be at k = 1,000,001.
    let r, bytes = runPinnedMeasured "Output.r = Range(-1.7976931348623157e308, 1.7976931348623157e308, 1.7976922360161978e302)"
    ValueSize.failsWith MessageCode.GeneratedLengthExceeded r
    Assert.True(bytes < oneMB, sprintf "Range allocated %d bytes before it raised" bytes)

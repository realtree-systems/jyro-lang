/// 50.3 Conditional statements, 50.4 While loops, 50.5 For loops,
/// 50.6 Foreach loops, 50.7 Break and continue.
module RealTree.Jyro.Spec.Tests.P40_60.S50_03_Control

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

/// Run and assert that the run failed with exactly one error `code`, and that
/// the final Output it carries is `expectedOutput` (so the body never ran).
let private failsWithOutput (code: MessageCode) (expectedOutput: string) (source: string) =
    let r = Script.run source
    try Script.expectError code r
    with :? Xunit.Sdk.XunitException as e -> Values.fail (sprintf "%s\nSource:\n%s" e.Message source)
    Assert.Equal(norm expectedOutput, failedOutputJson r)

// ---- 50.3.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-197")>]
[<InlineData("if true then end", "{}")>]
[<InlineData("if true then Output.a = 1 end", "{\"a\": 1}")>]
[<InlineData("if false then Output.a = 1 else Output.a = 2 end", "{\"a\": 2}")>]
[<InlineData("if false then Output.a = 1 elseif true then Output.a = 2 end", "{\"a\": 2}")>]
[<InlineData("if false then Output.a = 1 elseif false then Output.a = 2 elseif true then Output.a = 3 else Output.a = 4 end", "{\"a\": 3}")>]
[<InlineData("if false then elseif false then else end", "{}")>]
[<InlineData("if true then\n  if false then\n  else\n    Output.a = 5\n  end\nend", "{\"a\": 5}")>]
let If_ValidForm_ParsesAndRuns (source: string, expected: string) =
    assertOut expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-197")>]
[<InlineData("if true Output.a = 1 end")>]
[<InlineData("if true then Output.a = 1")>]
[<InlineData("if true then\nelse if false then\nend")>]
[<InlineData("if true then\nelse\nelseif false then\nend")>]
[<InlineData("if true then\nelse\nelse\nend")>]
[<InlineData("if true end")>]
[<InlineData("if false then\nelse true then\nend")>]
[<InlineData("if then end")>]
[<InlineData("elseif true then end")>]
[<InlineData("if true then elseif then end")>]
[<InlineData("if false then elseif true Output.a = 1 end")>]
let If_MalformedShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 50.3.2 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-198")>]
let If_ConditionsEvaluatedInOrderUntilTruthy_LaterNotEvaluated () =
    assertOut """{"log": ["c1", "c2"], "b": 2}"""
        (probe + "if P(Output, \"c1\", false) then\n  Output.b = 1\nelseif P(Output, \"c2\", true) then\n  Output.b = 2\nelseif P(Output, \"c3\", true) then\n  Output.b = 3\nelse\n  Output.b = 4\nend")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-198")>]
let If_TwoTruthyConditions_OnlyFirstBlockRuns () =
    assertOut """{"log": ["c1"], "b": 1}"""
        (probe + "if P(Output, \"c1\", 1) then\n  Output.b = 1\nelseif P(Output, \"c2\", 1) then\n  Output.b = 2\nend")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-198")>]
[<InlineData("0", 2)>]
[<InlineData("0.0", 2)>]
[<InlineData("\"\"", 2)>]
[<InlineData("null", 2)>]
[<InlineData("false", 2)>]
[<InlineData("[]", 1)>]
[<InlineData("{}", 1)>]
[<InlineData("1", 1)>]
[<InlineData("\"false\"", 1)>]
[<InlineData("x => x", 1)>]
let If_ConditionTruthiness_Follows30_9_2 (cond: string, block: int) =
    assertR (string block) (sprintf "var c = %s\nif c then\n  Output.r = 1\nelseif true then\n  Output.r = 2\nend" cond)

// ---- 50.3.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-199")>]
[<InlineData("false", "0", "else")>]
[<InlineData("\"\"", "null", "else")>]
[<InlineData("true", "false", "if")>]
[<InlineData("false", "1", "elseif")>]
let If_ElseRunsExactlyWhenNoConditionIsTruthy (c1: string, c2: string, expected: string) =
    assertRStr expected (sprintf "if %s then\n  Output.r = \"if\"\nelseif %s then\n  Output.r = \"elseif\"\nelse\n  Output.r = \"else\"\nend" c1 c2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-199")>]
let If_NoTruthyConditionAndNoElse_NothingRuns () =
    assertOut "{}" "if false then\n  Output.a = 1\nelseif 0 then\n  Output.a = 2\nend"

// ---- 50.4.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-200")>]
[<InlineData("while false do end")>]
[<InlineData("while false do Output.a = 1 end")>]
[<InlineData("var i = 0\nwhile i < 2 do\n  var j = 0\n  while j < 2 do\n    j++\n  end\n  i++\nend")>]
let While_ValidForm_Parses (source: string) =
    Assert.True(Script.ok (Script.run source))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-200")>]
[<InlineData("while false Output.a = 1 end")>]
[<InlineData("while false do Output.a = 1")>]
[<InlineData("while false then end")>]
[<InlineData("while do end")>]
[<InlineData("while false do end end")>]
[<InlineData("while (false) { }")>]
let While_OtherShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 50.4.2 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1742")>]
let ``JYRO-REQ-1742 While_RunsUntilConditionFalsy`` () =
    assertR "3" "var i = 0\nwhile i < 3 do i++ end\nOutput.r = i"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1742")>]
let ``JYRO-REQ-1742 While_FalseAtStart_RunsZeroIterations`` () =
    assertOut "{}" "while false do Output.x = 1 end"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1742")>]
[<InlineData(0)>]
[<InlineData(1)>]
[<InlineData(3)>]
let ``JYRO-REQ-1742 While_ConditionEvaluatedBeforeEachIteration`` (n: int) =
    let out = Script.outputOf (probe + sprintf "var i = 0\nwhile P(Output, \"c\", i < %d) do i++ end\nOutput.i = i" n)
    Assert.Equal(n + 1, List.length (Values.asList (Values.prop "log" out)))
    Assert.Equal(float n, Values.asFloat (Values.prop "i" out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1742")>]
[<InlineData("0")>]
[<InlineData("\"\"")>]
[<InlineData("null")>]
[<InlineData("false")>]
let ``JYRO-REQ-1742 While_FalsyValueSetInBody_EndsTheLoop`` (falsy: string) =
    assertR "1" (sprintf "var n = 0\nvar v = 1\nwhile v do\n  n++\n  v = %s\nend\nOutput.r = n" falsy)

/// The block runs after an evaluation exactly when it is truthy: the log of a
/// probe in the condition and one in the block shows 3 evaluations and 2 runs,
/// in the order c, b, c, b, c.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1742")>]
let ``JYRO-REQ-1742 While_ConditionThreeTimesAndBlockTwice_InTheOrderOfTheLoop`` () =
    let src = probe + "var k = 0\nwhile P(Output, \"c\", k < 2) do\n  P(Output, \"b\", 0)\n  k++\nend\nOutput.k = k"
    assertLog """["c", "b", "c", "b", "c"]""" src
    Assert.Equal(2.0, Values.asFloat (Values.prop "k" (Script.outputOf src)))

/// A falsy condition at the first evaluation runs the block never; the condition
/// is evaluated once.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1742")>]
[<InlineData("false")>]
[<InlineData("0")>]
[<InlineData("\"\"")>]
[<InlineData("null")>]
let ``JYRO-REQ-1742 While_FalsyAtTheFirstEvaluation_NeverRunsTheBlock`` (falsy: string) =
    assertLog """["c"]""" (probe + sprintf "while P(Output, \"c\", %s) do\n  P(Output, \"b\", 0)\nend" falsy)

/// A truthy value of any type runs the block.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1742")>]
[<InlineData("true")>]
[<InlineData("1")>]
[<InlineData("-1")>]
[<InlineData("0.5")>]
[<InlineData("\"a\"")>]
let ``JYRO-REQ-1742 While_TruthyValueOfAnyType_RunsTheBlock`` (truthy: string) =
    assertR "1" (sprintf "var n = 0\nvar v = %s\nwhile v do\n  n++\n  v = false\nend\nOutput.r = n" truthy)

// ---- 50.5.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-202")>]
[<InlineData("for i in 0 to 3 do l = Append(l, i) end", "[0, 1, 2]")>]
[<InlineData("for i in 3 downto 0 do l = Append(l, i) end", "[3, 2, 1]")>]
[<InlineData("for i in 0 to 3 by 2 do l = Append(l, i) end", "[0, 2]")>]
[<InlineData("for i in 3 downto 0 by 2 do l = Append(l, i) end", "[3, 1]")>]
let For_ValidForm_ParsesAndRuns (loop: string, expected: string) =
    assertR expected (sprintf "var l = []\n%s\nOutput.r = l" loop)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-202")>]
[<InlineData("for i in 0 to 3 end")>]
[<InlineData("for i = 0 to 3 do end")>]
[<InlineData("for i in 0 upto 3 do end")>]
[<InlineData("for i in 0 to 3 by do end")>]
[<InlineData("for 1 in 0 to 3 do end")>]
[<InlineData("for i in 0 to 3 do by 2 end")>]
[<InlineData("for i in 0 to 3 do")>]
[<InlineData("for i in 0 do end")>]
[<InlineData("for in 0 to 3 do end")>]
[<InlineData("for i in 0 to 3 step 2 do end")>]
[<InlineData("for i in 0 downto to 3 do end")>]
let For_OtherShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 50.5.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-203")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("3")>]
let For_StartEndStepEvaluatedOnceInOrder (n: string) =
    assertLog """["s", "e", "st"]"""
        (probe + sprintf "for i in P(Output, \"s\", 0) to P(Output, \"e\", %s) by P(Output, \"st\", 1) do end" n)
    assertLog """["s", "e"]""" (probe + sprintf "for i in P(Output, \"s\", %s) downto P(Output, \"e\", 0) do end" n)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-203")>]
let For_ChangingEndVariableInBody_DoesNotChangeCount () =
    assertR "3" "var e = 3\nvar n = 0\nfor i in 0 to e do\n  e = 10\n  n++\nend\nOutput.r = n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-203")>]
let For_ChangingStepVariableInBody_DoesNotChangeCount () =
    assertR "6" "var s = 1\nvar n = 0\nfor i in 0 to 6 by s do\n  s = 3\n  n++\nend\nOutput.r = n"

// ---- 50.5.3 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-204")>]
let ``JYRO-REQ-204 For_DefaultStep_IsExactOne_LargeIntegersStayExact`` () =
    assertR """["9007199254740993", "9007199254740994"]"""
        "var l = []\nfor i in 9007199254740993 to 9007199254740995 do l = Append(l, i & \"\") end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-204")>]
let ``JYRO-REQ-204 For_DefaultStep_IsExactOne_FractionsStayExact`` () =
    assertR """["0.1", "1.1", "2.1"]""" "var l = []\nfor i in 0.1 to 3 do l = Append(l, i & \"\") end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-204")>]
let ``JYRO-REQ-204 For_DefaultStep_DownTo_IsExactOne`` () =
    assertR """["9007199254740995", "9007199254740994"]"""
        "var l = []\nfor i in 9007199254740995 downto 9007199254740993 do l = Append(l, i & \"\") end\nOutput.r = l"

// ---- 50.5.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-205")>]
[<InlineData("0 to 3", "[0, 1, 2]")>]
[<InlineData("0 to 0", "[]")>]
[<InlineData("3 to 0", "[]")>]
[<InlineData("0 to 1", "[0]")>]
[<InlineData("0 to 5 by 2", "[0, 2, 4]")>]
[<InlineData("0 to 6 by 2", "[0, 2, 4]")>]
[<InlineData("0 to 7 by 2", "[0, 2, 4, 6]")>]
[<InlineData("0.5 to 2", "[\"0.5\", \"1.5\"]")>]
[<InlineData("-2 to 1", "[-2, -1, 0]")>]
[<InlineData("0.1 to 0.2", "[\"0.1\"]")>]
let ``JYRO-REQ-205 ForTo_LoopVariableTakesStartPlusKStepBelowEnd`` (range: string, expected: string) =
    // Fractions are recorded as string forms so the comparison does not depend on JSON number text.
    let body = if expected.Contains("\"") then "l = Append(l, i & \"\")" else "l = Append(l, i)"
    assertR expected (sprintf "var l = []\nfor i in %s do %s end\nOutput.r = l" range body)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-205")>]
let ``JYRO-REQ-205 ForTo_AssigningLoopVariableInBody_DoesNotChangeNextValue`` () =
    assertR "[0, 1, 2]" "var l = []\nfor i in 0 to 3 do\n  l = Append(l, i)\n  i = 100\nend\nOutput.r = l"

/// An inexact start (a host double, 160.8), where `start + k * step` and
/// stepping the previous value by `step` differ (JYRO-DEF-637): from 2^53,
/// start + 1 rounds back to 2^53 and start + 3 rounds up to 2^53 + 4, so the
/// values are 2^53, 2^53, 2^53 + 2 and the loop ends. A loop that adds 1 to
/// the previous value never leaves 2^53.
let private twoTo53 = 9007199254740992.0

let private withInexactStart : RunOptions =
    { limited with Input = ValueInput(Values.obj [ "s", Values.inexact twoTo53; "t", Values.inexact (twoTo53 + 4.0) ]) }

/// Each value of the loop variable, less `Input.s`, read as doubles.
let private loopOffsets (loop: string) : float list =
    let out = Script.output (Script.runWith withInexactStart (sprintf "var l = []\n%s do l = Append(l, i - Input.s) end\nOutput.r = l" loop))
    Values.prop "r" out |> Values.asList |> List.map Values.asFloat

[<Fact>]
[<Trait("requirement", "JYRO-REQ-205")>]
let ``JYRO-REQ-205 ForTo_InexactStart_ValuesAreStartPlusKStep_NotRepeatedAddition`` () =
    let expected = [ for k in 0 .. 2 -> (twoTo53 + float k) - twoTo53 ]
    Assert.Equal<float list>([ 0.0; 0.0; 2.0 ], expected)   // the table agrees with double arithmetic
    Assert.Equal<float list>(expected, loopOffsets "for i in Input.s to Input.t")

// ---- 50.5.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-206")>]
[<InlineData("3 downto 0", "[3, 2, 1]")>]
[<InlineData("0 downto 0", "[]")>]
[<InlineData("0 downto 3", "[]")>]
[<InlineData("1 downto 0", "[1]")>]
[<InlineData("10 downto 0 by 3", "[10, 7, 4, 1]")>]
[<InlineData("9 downto 0 by 3", "[9, 6, 3]")>]
[<InlineData("2.5 downto 0", "[\"2.5\", \"1.5\", \"0.5\"]")>]
[<InlineData("1 downto -2", "[1, 0, -1]")>]
let ``JYRO-REQ-206 ForDownTo_LoopVariableTakesStartMinusKStepAboveEnd`` (range: string, expected: string) =
    let body = if expected.Contains("\"") then "l = Append(l, i & \"\")" else "l = Append(l, i)"
    assertR expected (sprintf "var l = []\nfor i in %s do %s end\nOutput.r = l" range body)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-206")>]
let ``JYRO-REQ-206 ForDownTo_AssigningLoopVariableInBody_DoesNotChangeNextValue`` () =
    assertR "[3, 2, 1]" "var l = []\nfor i in 3 downto 0 do\n  l = Append(l, i)\n  i = -100\nend\nOutput.r = l"

/// From the inexact 2^53 + 4 down to 2^53: start - 1 rounds back to 2^53 + 4
/// and start - 3 rounds down to 2^53, so the values are 2^53 + 4, 2^53 + 4,
/// 2^53 + 2 and the loop ends (JYRO-DEF-637). Offsets are from Input.s = 2^53.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-206")>]
let ``JYRO-REQ-206 ForDownTo_InexactStart_ValuesAreStartMinusKStep_NotRepeatedSubtraction`` () =
    let start = twoTo53 + 4.0
    let expected = [ for k in 0 .. 2 -> (start - float k) - twoTo53 ]
    Assert.Equal<float list>([ 4.0; 4.0; 2.0 ], expected)   // the table agrees with double arithmetic
    Assert.Equal<float list>(expected, loopOffsets "for i in Input.t downto Input.s")

// ---- 50.5.6 --------------------------------------------------------------

let badSteps = [ "0"; "-1"; "1.5"; "0.5"; "\"1\""; "null"; "true"; "[]"; "{}"; "-0.5" ]

let badStepCases : seq<obj[]> =
    seq {
        for s in badSteps do
            for loop in [ "0 to 3"; "3 downto 0"; "3 to 0"; "0 downto 3" ] do
                yield [| box s; box loop |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-207")>]
[<MemberData(nameof badStepCases)>]
let ``JYRO-REQ-207 For_StepNotPositiveInteger_RaisesNonNegativeIntegerRequiredBeforeFirstIteration`` (step: string, loop: string) =
    failsWithOutput MessageCode.NonNegativeIntegerRequired "{}"
        (sprintf "var s = %s\nfor i in %s by s do Output.ran = true end" step loop)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-207")>]
[<InlineData("1", "[0, 1, 2, 3]")>]
[<InlineData("2", "[0, 2]")>]
[<InlineData("2.0", "[0, 2]")>]
[<InlineData("5", "[0]")>]
let ``JYRO-REQ-207 For_PositiveIntegerStep_Runs`` (step: string, expected: string) =
    let got = rOf (sprintf "var s = %s\nvar l = []\nfor i in 0 to 4 by s do l = Append(l, i) end\nOutput.r = l" step)
    Assert.Equal<float list>(Values.json expected |> Values.asList |> List.map Values.asFloat, Values.asList got |> List.map Values.asFloat)

// ---- 50.5.7 --------------------------------------------------------------

let nonNumberBounds = [ "null"; "\"1\""; "true"; "[]"; "{}"; "x => x" ]

let nonNumberBoundCases : seq<obj[]> =
    seq {
        for v in nonNumberBounds do
            for dir in [ "to"; "downto" ] do
                yield [| box v; box dir; box "start" |]
                yield [| box v; box dir; box "end" |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-208")>]
[<MemberData(nameof nonNumberBoundCases)>]
let For_StartOrEndNotNumber_RaisesUnsupportedBinaryOperationBeforeFirstIteration (value: string, direction: string, which: string) =
    let loop = if which = "start" then sprintf "v %s 3" direction else sprintf "3 %s v" direction
    failsWithOutput MessageCode.UnsupportedBinaryOperation "{}"
        (sprintf "var v = %s\nfor i in %s do Output.ran = true end" value loop)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-208")>]
[<InlineData("for i in 5 to \"z\" do Output.ran = true end")>]
[<InlineData("for i in \"a\" downto 9 do Output.ran = true end")>]
[<InlineData("for i in null to 0 do Output.ran = true end")>]
[<InlineData("for i in 0 downto null do Output.ran = true end")>]
let For_NonNumberBoundOnZeroIterationLoop_StillRaisesUnsupportedBinaryOperation (source: string) =
    failsWithOutput MessageCode.UnsupportedBinaryOperation "{}" source

// ---- 50.5.8 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-209")>]
let For_LoopVariableNotVisibleAfterLoop () =
    rejects MessageCode.UndeclaredVariable "for i in 0 to 2 do end\nOutput.r = i"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-209")>]
let For_LoopVariableReadableInBody () =
    assertR "1" "for i in 0 to 2 do Output.r = i end"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-209")>]
let For_LoopVariableInBodyScope_OuterSameNameUntouched () =
    assertR "5" "var i = 5\nfor i in 0 to 2 do end\nOutput.r = i"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-209")>]
let For_LoopVariableIsInBodyScope_BodyDeclarationOfSameNameClashes () =
    rejects MessageCode.VariableAlreadyDeclared "for i in 0 to 2 do\n  var i = 1\nend"

// ---- 50.5.9 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-210")>]
[<InlineData("\"a\"", "to", "3", "0", "UnsupportedBinaryOperation")>]
[<InlineData("5", "to", "\"z\"", "1", "UnsupportedBinaryOperation")>]
[<InlineData("\"a\"", "to", "\"z\"", "-1", "UnsupportedBinaryOperation")>]
[<InlineData("0", "to", "3", "0", "NonNegativeIntegerRequired")>]
[<InlineData("0", "to", "\"z\"", "1.5", "UnsupportedBinaryOperation")>]
[<InlineData("9", "to", "0", "0", "NonNegativeIntegerRequired")>]
[<InlineData("9", "downto", "\"z\"", "0", "UnsupportedBinaryOperation")>]
[<InlineData("null", "downto", "null", "null", "UnsupportedBinaryOperation")>]
[<InlineData("0", "downto", "9", "-2", "NonNegativeIntegerRequired")>]
let ``JYRO-REQ-210 For_ChecksStartThenEndThenStep_AfterEvaluatingAllThree`` (s: string, dir: string, e: string, st: string, code: string) =
    let code = System.Enum.Parse<MessageCode>(code)
    failsWithOutput code """{"log": ["s", "e", "st"]}"""
        (probe + sprintf "for i in P(Output, \"s\", %s) %s P(Output, \"e\", %s) by P(Output, \"st\", %s) do Output.ran = true end" s dir e st)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-210")>]
let ``JYRO-REQ-210 For_RequirementExamples`` () =
    runtimeError MessageCode.UnsupportedBinaryOperation "for i in \"a\" to 3 by 0 do end"
    runtimeError MessageCode.UnsupportedBinaryOperation "for i in 5 to \"z\" do end"

// ---- 50.6.1 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-211")>]
let Foreach_ValidForm_Parses () =
    assertR "[1]" "var l = []\nforeach x in [1] do l = Append(l, x) end\nOutput.r = l"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-211")>]
[<InlineData("foreach x in [1] Output.a = 1 end")>]
[<InlineData("foreach x in [1] do")>]
[<InlineData("foreach x of [1] do end")>]
[<InlineData("foreach (x) in [1] do end")>]
[<InlineData("foreach in [1] do end")>]
[<InlineData("foreach x, y in [1] do end")>]
[<InlineData("foreach x in do end")>]
[<InlineData("foreach x in [1] then end")>]
let Foreach_OtherShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-211")>]
[<InlineData("[]")>]
[<InlineData("[1]")>]
[<InlineData("[1, 2, 3]")>]
[<InlineData("{a: 1, b: 2}")>]
[<InlineData("\"abc\"")>]
let Foreach_CollectionEvaluatedOnceBeforeFirstIteration (collection: string) =
    assertLog """["c"]""" (probe + sprintf "foreach x in P(Output, \"c\", %s) do end" collection)

// ---- 50.6.2 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-212")>]
let Foreach_OverString_GivesEachCodeUnitInOrder () =
    assertR """["a", "b", "c"]""" "var l = []\nforeach c in \"abc\" do l = Append(l, c) end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-212")>]
let Foreach_OverEmptyString_GivesNothing () =
    assertR "0" "var n = 0\nforeach c in \"\" do n++ end\nOutput.r = n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-212")>]
let Foreach_OverSurrogatePair_GivesTwoOneUnitStrings () =
    let out =
        Script.outputOf
            "var s = \"\\uD83D\\uDE00\"\nvar l = []\nforeach c in s do l = Append(l, c) end\nOutput.n = Length(l)\nOutput.lens = Map(l, c => Length(c))\nOutput.joined = (l[0] & l[1]) == s"
    Assert.Equal(norm """{"n": 2, "lens": [1, 1], "joined": true}""", Values.toJson out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-212")>]
let Foreach_OverInputString_GivesEachCodeUnit () =
    assertOutWith """{"s": "xy"}""" """{"r": ["x", "y"]}""" "var l = []\nforeach c in Input.s do l = Append(l, c) end\nOutput.r = l"

// ---- 50.6.3 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-213")>]
let Foreach_OverArray_GivesEachElementInOrderIncludingNull () =
    assertR """[3, null, "a", [1]]""" "var l = []\nforeach x in [3, null, \"a\", [1]] do l = Append(l, x) end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-213")>]
let Foreach_OverEmptyArray_GivesNothing () =
    assertR "0" "var n = 0\nforeach x in [] do n++ end\nOutput.r = n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-213")>]
let Foreach_OverArray_GivesTheElementsThemselves () =
    assertR """[{"v": 2}]""" "var a = [{v: 1}]\nforeach e in a do e.v = 2 end\nOutput.r = a"

// ---- 50.6.4 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-214")>]
let ``JYRO-REQ-214 Foreach_OverObject_GivesValuesInInsertionOrder`` () =
    assertR "[9, 2, 3]" "var o = {b: 1, a: 2}\no.c = 3\no.b = 9\nvar l = []\nforeach v in o do l = Append(l, v) end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-214")>]
let ``JYRO-REQ-214 Foreach_OverObjectAfterDeleteAndReAdd_GivesReAddedLast`` () =
    assertR "[2, 3]" "var o = {a: 1, b: 2}\ndelete o.a\no.a = 3\nvar l = []\nforeach v in o do l = Append(l, v) end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-214")>]
let ``JYRO-REQ-214 Foreach_OverInputObject_GivesDocumentOrder`` () =
    assertOutWith """{"o": {"z": 1, "y": 2, "x": 3}}""" """{"r": [1, 2, 3]}""" "var l = []\nforeach v in Input.o do l = Append(l, v) end\nOutput.r = l"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-214")>]
let ``JYRO-REQ-214 Foreach_OverEmptyObject_GivesNothing`` () =
    assertR "0" "var n = 0\nforeach v in {} do n++ end\nOutput.r = n"

// ---- 50.6.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-215")>]
[<InlineData("var a = [1, 2]\nforeach x in a do\n  l = Append(l, x)\n  a[Length(a)] = 9\nend\nOutput.c = a", "[1, 2]", "[1, 2, 9, 9]")>]
[<InlineData("var a = [1, 2, 3]\nforeach x in a do\n  l = Append(l, x)\n  a[1] = 99\nend\nOutput.c = a", "[1, 2, 3]", "[1, 99, 3]")>]
[<InlineData("var o = {a: 1, b: 2}\nforeach v in o do\n  l = Append(l, v)\n  o.n = 5\nend\nOutput.c = o", "[1, 2]", "{\"a\": 1, \"b\": 2, \"n\": 5}")>]
[<InlineData("var o = {a: 1, b: 2}\nforeach v in o do\n  l = Append(l, v)\n  delete o.b\nend\nOutput.c = o", "[1, 2]", "{\"a\": 1}")>]
[<InlineData("var o = {a: 1, b: 2}\nforeach v in o do\n  l = Append(l, v)\n  o.b = 5\nend\nOutput.c = o", "[1, 2]", "{\"a\": 1, \"b\": 5}")>]
let Foreach_ChangeToCollectionInBody_DoesNotChangeValuesGiven (loop: string, given: string, after: string) =
    let out = Script.outputOf ("var l = []\n" + loop + "\nOutput.l = l")
    Assert.Equal(norm given, Values.toJson (Values.prop "l" out))
    Assert.Equal(norm after, Values.toJson (Values.prop "c" out))

// ---- 50.6.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-216")>]
[<InlineData("var n = 5\nforeach x in n do Output.ran = true end")>]
[<InlineData("var n = null\nforeach x in n do Output.ran = true end")>]
[<InlineData("var n = true\nforeach x in n do Output.ran = true end")>]
[<InlineData("var n = 1.5\nforeach x in n do Output.ran = true end")>]
[<InlineData("var n = x => x\nforeach x in n do Output.ran = true end")>]
[<InlineData("foreach x in Input.n do Output.ran = true end")>]
[<InlineData("foreach x in Input.missing do Output.ran = true end")>]
[<InlineData("foreach x in ToNumber(\"1\") do Output.ran = true end")>]
let Foreach_OverNonCollection_RaisesNotIterable (source: string) =
    let r = Script.runJson """{"n": 3}""" source
    Script.expectError MessageCode.NotIterable r
    Assert.Equal("{}", failedOutputJson r)

// ---- 50.6.7 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-217")>]
[<InlineData("null")>]
[<InlineData("5")>]
[<InlineData("0")>]
[<InlineData("1.5")>]
[<InlineData("1e3")>]
[<InlineData("0x1F")>]
[<InlineData("0b101")>]
[<InlineData("true")>]
[<InlineData("false")>]
let Foreach_OverNullNumberOrBooleanLiteral_IsRejectedWithNotIterableLiteral (literal: string) =
    rejects MessageCode.NotIterableLiteral (sprintf "foreach x in %s do Output.ran = true end" literal)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-217")>]
[<InlineData("\"\"")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
let Foreach_OverStringArrayOrObjectLiteral_IsAccepted (literal: string) =
    notRejectedWith MessageCode.NotIterableLiteral (sprintf "foreach x in %s do end" literal)
    compiles (sprintf "foreach x in %s do end" literal)

// ---- 50.6.8 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-218")>]
let Foreach_LoopVariableNotVisibleAfterLoop () =
    rejects MessageCode.UndeclaredVariable "foreach x in [1] do end\nOutput.r = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-218")>]
let Foreach_LoopVariableInBodyScope_OuterSameNameUntouched () =
    assertR "5" "var x = 5\nforeach x in [1] do end\nOutput.r = x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-218")>]
let Foreach_LoopVariableIsInBodyScope_BodyDeclarationOfSameNameClashes () =
    rejects MessageCode.VariableAlreadyDeclared "foreach x in [1] do\n  var x = 2\nend"

// ---- 50.7.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-219")>]
[<InlineData("var i = 0\nwhile true do\n  i++\n  if i == 3 then break end\n  l = Append(l, i)\nend", "[1, 2]")>]
[<InlineData("for i in 0 to 10 do\n  if i == 2 then break end\n  l = Append(l, i)\nend", "[0, 1]")>]
[<InlineData("for i in 10 downto 0 do\n  if i == 8 then break end\n  l = Append(l, i)\nend", "[10, 9]")>]
[<InlineData("foreach x in [1, 2, 3] do\n  if x == 2 then break end\n  l = Append(l, x)\nend", "[1]")>]
[<InlineData("for i in 0 to 3 do\n  for j in 0 to 3 do\n    if j == 1 then break end\n    l = Append(l, [i, j])\n  end\nend", "[[0, 0], [1, 0], [2, 0]]")>]
[<InlineData("foreach x in [1, 2, 3] do\n  switch x do\n    case 2 then break\n  end\n  l = Append(l, x)\nend", "[1]")>]
[<InlineData("foreach x in [1, 2, 3] do\n  l = Append(l, x)\n  break\n  l = Append(l, 100)\nend", "[1]")>]
let Break_EndsTheInnermostLoop (loop: string, expected: string) =
    assertOut (sprintf """{"r": %s, "after": true}""" expected) ("var l = []\n" + loop + "\nOutput.r = l\nOutput.after = true")

// ---- 50.7.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-220")>]
[<InlineData("for i in 0 to 5 by 2 do\n  if i == 2 then continue end\n  l = Append(l, i)\nend", "[0, 4]")>]
[<InlineData("for i in 0 to 4 do\n  l = Append(l, i)\n  i = 100\n  continue\nend", "[0, 1, 2, 3]")>]
[<InlineData("for i in 7 downto 0 by 3 do\n  if i == 4 then continue end\n  l = Append(l, i)\nend", "[7, 1]")>]
[<InlineData("var i = 0\nwhile i < 5 do\n  i++\n  if i % 2 == 0 then continue end\n  l = Append(l, i)\nend", "[1, 3, 5]")>]
[<InlineData("foreach x in [1, 2, 3] do\n  if x == 2 then continue end\n  l = Append(l, x)\nend", "[1, 3]")>]
[<InlineData("for i in 0 to 2 do\n  for j in 0 to 2 do\n    if j == 0 then continue end\n    l = Append(l, [i, j])\n  end\n  l = Append(l, i)\nend", "[[0, 1], 0, [1, 1], 1]")>]
let ``JYRO-REQ-220 Continue_EndsCurrentIterationOfInnermostLoop`` (loop: string, expected: string) =
    assertR expected ("var l = []\n" + loop + "\nOutput.r = l")

// ---- 50.7.3 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-221")>]
[<InlineData("break")>]
[<InlineData("continue")>]
[<InlineData("if true then break end")>]
[<InlineData("if true then continue end")>]
[<InlineData("switch 1 do\n  case 1 then break\nend")>]
[<InlineData("switch 1 do\n  default then continue\nend")>]
[<InlineData("func f()\n  break\nend")>]
[<InlineData("func f()\n  continue\nend\nwhile false do f() end")>]
[<InlineData("func f()\n  if true then continue end\nend")>]
let BreakOrContinue_NotInsideLoop_IsRejectedWithLoopStatementOutsideOfLoop (source: string) =
    rejects MessageCode.LoopStatementOutsideOfLoop source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-221")>]
[<InlineData("func f()\n  while true do break end\nend")>]
[<InlineData("while false do\n  if true then continue end\nend")>]
[<InlineData("foreach x in [] do\n  switch x do\n    case 1 then break\n  end\nend")>]
let BreakOrContinue_InsideLoop_IsAccepted (source: string) =
    notRejectedWith MessageCode.LoopStatementOutsideOfLoop source
    compiles source

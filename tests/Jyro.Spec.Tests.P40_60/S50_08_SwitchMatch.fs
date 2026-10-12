/// 50.8 Switch, 50.9 Match, 50.10 Unions.
module RealTree.Jyro.Spec.Tests.P40_60.S50_08_SwitchMatch

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// ---- 50.8.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-222")>]
[<InlineData("switch 1 do end", "{}")>]
[<InlineData("switch 1 do case 1 then Output.a = 1 end", "{\"a\": 1}")>]
[<InlineData("switch 2 do case 1, 2, 3 then Output.a = 1 end", "{\"a\": 1}")>]
[<InlineData("switch 1 do default then Output.a = 2 end", "{\"a\": 2}")>]
[<InlineData("switch 3 do case 1 then Output.a = 1 case 2 then Output.a = 2 default then Output.a = 3 end", "{\"a\": 3}")>]
[<InlineData("switch 1 do case 1 then case 2 then default then end", "{}")>]
let Switch_ValidForm_ParsesAndRuns (source: string, expected: string) =
    assertOut expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-222")>]
[<InlineData("switch 1 do default then Output.a = 1 case 1 then Output.a = 2 end")>]
[<InlineData("switch 1 do default then Output.a = 1 default then Output.a = 2 end")>]
[<InlineData("switch 1 do case then Output.a = 1 end")>]
[<InlineData("switch 1 do case 1 Output.a = 1 end")>]
[<InlineData("switch 1 do case 1 then Output.a = 1")>]
[<InlineData("switch 1 do case 1, then Output.a = 1 end")>]
[<InlineData("switch 1 case 1 then Output.a = 1 end")>]
[<InlineData("switch do case 1 then end")>]
[<InlineData("switch 1 do default Output.a = 1 end")>]
let Switch_OtherShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 50.8.2 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-223")>]
[<InlineData("switch P(Output, \"s\", 3) do end")>]
[<InlineData("switch P(Output, \"s\", 3) do case 1 then case 2 then case 3 then end")>]
[<InlineData("switch P(Output, \"s\", 3) do case 1 then case 2 then default then end")>]
[<InlineData("switch P(Output, \"s\", 3) do case 1, 2, 4, 5 then end")>]
let Switch_SubjectEvaluatedOnce (stmt: string) =
    assertLog """["s"]""" (probe + stmt)

// ---- 50.8.3 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-224")>]
let Switch_ComparesInSourceOrderAndStopsAtFirstEqual () =
    assertOut """{"log": ["1", "2"], "b": 1}"""
        (probe + "switch 2 do\n  case P(Output, \"1\", 1), P(Output, \"2\", 2), P(Output, \"3\", 3) then Output.b = 1\n  case P(Output, \"4\", 2) then Output.b = 2\nend")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-224")>]
let Switch_ComparesCaseValuesAcrossCasesInOrder () =
    assertOut """{"log": ["1", "2", "3"], "b": 2}"""
        (probe + "switch 3 do\n  case P(Output, \"1\", 1), P(Output, \"2\", 2) then Output.b = 1\n  case P(Output, \"3\", 3) then Output.b = 2\n  case P(Output, \"4\", 3) then Output.b = 3\nend")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-224")>]
[<InlineData("1", "1.0", true)>]
[<InlineData("\"1\"", "1", false)>]
[<InlineData("1", "\"1\"", false)>]
[<InlineData("[1, 2]", "[1, 2]", true)>]
[<InlineData("[1, 2]", "[2, 1]", false)>]
[<InlineData("{a: 1}", "{a: 1}", true)>]
[<InlineData("{a: 1}", "{a: 2}", false)>]
[<InlineData("null", "null", true)>]
[<InlineData("true", "1", false)>]
[<InlineData("0", "false", false)>]
[<InlineData("\"\"", "null", false)>]
[<InlineData("\"a\"", "\"a\"", true)>]
[<InlineData("0.1 + 0.2", "0.3", true)>]
let Switch_ComparesWithEquality (subject: string, caseValue: string, matches: bool) =
    assertRBool matches (sprintf "var s = %s\nswitch s do\n  case %s then Output.r = true\n  default then Output.r = false\nend" subject caseValue)

// ---- 50.8.4 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1965")>]
let Switch_RunsOnlyTheFirstMatchingCase () =
    assertOut """{"a": 1}""" "switch 2 do\n  case 1, 2 then Output.a = 1\n  case 2 then Output.b = 2\n  default then Output.d = 3\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1965")>]
let Switch_DoesNotFallThrough () =
    assertOut """{"a": 1}""" "switch 1 do\n  case 1 then Output.a = 1\n  case 2 then Output.b = 2\nend"

// ---- 50.8.5 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1966")>]
let Switch_NoEqualCase_RunsDefault () =
    assertOut """{"d": 1}""" "switch 9 do\n  case 1 then Output.a = 1\n  case 2 then Output.b = 2\n  default then Output.d = 1\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1966")>]
let Switch_NoEqualCaseAndNoDefault_RunsNothingAndContinues () =
    assertOut """{"after": 1}""" "switch 9 do\n  case 1 then Output.a = 1\nend\nOutput.after = 1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1966")>]
let Switch_NoCasesWithDefault_RunsDefault () =
    assertOut """{"d": 1}""" "switch 9 do\n  default then Output.d = 1\nend"

// ---- 50.8.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-227")>]
[<InlineData("switch a do\n  case a then Output.c = 1\n  default then Output.d = 1\nend")>]
[<InlineData("switch a do\n  case 1 then Output.e = 1\n  case b then Output.c = 1\n  default then Output.d = 1\nend")>]
[<InlineData("switch b do\n  case \"x\" then Output.e = 1\n  case a then Output.c = 1\n  default then Output.d = 1\nend")>]
let Switch_ComparisonRaises_NoBlockRunsAndErrorPropagates (stmt: string) =
    let r = Script.run ("var a = []\na[0] = a\nvar b = []\nb[0] = b\n" + stmt)
    Script.expectError MessageCode.ValueNestingLimitExceeded r
    Assert.Equal("{}", failedOutputJson r)

// ---- 50.9 shared ---------------------------------------------------------

let shape = "union Shape Circle(r) Square(s) Empty end\n"

let private allCasesStmt = "match v do\n  case Circle(r) then Output.k = r\n  case Square(s) then Output.k = s\n  case Empty then Output.k = 0\nend"

// ---- 50.9.1 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1967")>]
[<InlineData("Circle(2)", "2")>]
[<InlineData("Square(3)", "3")>]
[<InlineData("Empty()", "0")>]
let Match_StatementForm_ParsesAndRuns (subject: string, expected: string) =
    assertOut (sprintf """{"k": %s}""" expected) (shape + sprintf "var v = %s\n" subject + allCasesStmt)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1967")>]
[<InlineData("Circle(2)", "2")>]
[<InlineData("Square(3)", "3")>]
[<InlineData("Empty()", "0")>]
let Match_CaseOrderNotSignificant (subject: string, expected: string) =
    assertOut (sprintf """{"k": %s}""" expected)
        (shape + sprintf "var v = %s\nmatch v do\n  case Empty then Output.k = 0\n  case Square(s) then Output.k = s\n  case Circle(r) then Output.k = r\nend" subject)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1967")>]
[<InlineData("Circle(2)", "2")>]
[<InlineData("Empty()", "0")>]
let Match_ExpressionForm_ParsesAndRuns (subject: string, expected: string) =
    assertR expected (shape + sprintf "var v = %s\nOutput.r = match v do case Circle(r) then r case Square(s) then s case Empty then 0 end" subject)
    assertR expected (shape + sprintf "var v = %s\nOutput.r = match v do case Empty then 0 case Circle(r) then r case Square(s) then s end" subject)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1967")>]
[<InlineData("var v = Empty()\nmatch v do end")>]
[<InlineData("var v = Empty()\nOutput.r = match v do end")>]
[<InlineData("var v = Empty()\nmatch v case Circle(r) then case Square(s) then case Empty then end")>]
[<InlineData("var v = Empty()\nmatch v do case Circle(r) then case Square(s) then case Empty then")>]
[<InlineData("var v = Empty()\nmatch v do case Circle(r) case Square(s) then case Empty then end")>]
[<InlineData("var v = Empty()\nOutput.r = match v do case Circle(r) then r case Square(s) then s case Empty then end")>]
let Match_MalformedShape_IsRejectedBeforeExecution (source: string) =
    compileFails (shape + source)

// ---- 50.9.2 --------------------------------------------------------------

let private allCasesExpr = "Output.r = match v do case Circle(r) then 1 case Square(s) then 2 case Empty then 3"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-229")>]
[<InlineData(" case default then 4 end")>]
[<InlineData(" case default then 4 case default then 5 end")>]
let ``JYRO-REQ-229 Match_ExpressionWithCaseDefault_IsRejectedOnlyWithMatchWithDefault`` (tail: string) =
    let text = shape + "var v = Empty()\n" + allCasesExpr + tail
    // Every error is MatchWithDefault at stage Validate: none is
    // MatchUnknownVariant or MatchDuplicateCase. The count is not asserted.
    let r = rejectedAt MessageCode.MatchWithDefault "Validate" text
    Assert.DoesNotContain(MessageCode.MatchUnknownVariant, Script.codes r)
    Assert.DoesNotContain(MessageCode.MatchDuplicateCase, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-229")>]
let ``JYRO-REQ-229 Match_BookExample_CaseDefaultWithEveryVariant_IsMatchWithDefault`` () =
    let text = "union U A end\nvar u = A()\nOutput.m = match u do case A then 1 case default then 2 end"
    rejectedAt MessageCode.MatchWithDefault "Validate" text |> Script.expectError MessageCode.MatchWithDefault

[<Fact>]
[<Trait("requirement", "JYRO-REQ-229")>]
let ``JYRO-REQ-229 Match_CaseDefaultFirst_IsMatchWithDefaultNotMatchUnknownVariant`` () =
    let text = "union U A end\nvar u = A()\nOutput.m = match u do case default then 0 case A then 1 end"
    rejectedAt MessageCode.MatchWithDefault "Validate" text |> Script.expectError MessageCode.MatchWithDefault

[<Theory>]
[<Trait("requirement", "JYRO-REQ-229")>]
[<InlineData("union U A B end\nvar u = A()\nOutput.m = match u do case A then 1 case default then 2 end")>]
[<InlineData("union U A B end\nvar u = A()\nOutput.m = match u do case B then 1 case default then 2 end")>]
let ``JYRO-REQ-229 Match_CaseDefaultWithAVariantMissing_GivesMatchWithDefaultAndMatchNotExhaustive`` (text: string) =
    let r = rejectedHasAt MessageCode.MatchWithDefault "Validate" text
    let errs = Script.errorCodes r
    Assert.Contains(MessageCode.MatchNotExhaustive, errs)
    Assert.DoesNotContain(MessageCode.MatchUnknownVariant, errs)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-229")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\n  case Square(s) then Output.a = 2\n  case Empty then Output.a = 3\n  case default then Output.a = 4\nend")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\n  case Square(s) then Output.a = 2\n  case Empty then Output.a = 3\n  default then Output.a = 4\nend")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\n  default then Output.a = 4\nend")>]
let ``JYRO-REQ-229 Match_StatementWithCaseDefaultOrDefaultThen_IsAParseError`` (stmt: string) =
    let r = parseErrorL (shape + "var v = Empty()\n" + stmt)
    Assert.DoesNotContain(MessageCode.MatchWithDefault, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-229")>]
let ``JYRO-REQ-229 Match_BookExample_StatementWithExpressionBodies_IsAParseError`` () =
    // A statement that starts with match is a match statement (20.1.4),
    // whatever its case bodies are.
    let r = parseErrorL "union U A end\nvar u = A()\nmatch u do case A then 1 case default then 2 end"
    Assert.DoesNotContain(MessageCode.MatchWithDefault, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-229")>]
let ``JYRO-REQ-229 Match_ParenthesisedAsAStatement_IsMatchWithDefault`` () =
    rejectedAt MessageCode.MatchWithDefault "Validate" "union U A end\nvar u = A()\n(match u do case A then 1 case default then 2 end)"
    |> Script.expectError MessageCode.MatchWithDefault

[<Fact>]
[<Trait("requirement", "JYRO-REQ-229")>]
let ``JYRO-REQ-229 Match_WithoutADefaultCase_Compiles`` () =
    assertOutL """{"r": 3}""" (shape + "var v = Empty()\n" + allCasesExpr + " end")
    assertOutL """{"k": 0}""" (shape + "var v = Empty()\n" + allCasesStmt)

// ---- 50.9.3 --------------------------------------------------------------

let twoUnions = shape + "union B Z end\nvar v = Empty()\n"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1690")>]
[<InlineData("case Triangle(a) then Output.a = 1")>]
[<InlineData("case Length(x) then Output.a = 1")>]
[<InlineData("case Shape then Output.a = 1")>]
[<InlineData("case circle(r) then Output.a = 1")>]
[<InlineData("case Z then Output.a = 1")>]
let Match_CaseNamingNoVariantOfTheMatchUnion_IsRejectedWithMatchUnknownVariant (extraCase: string) =
    rejects MessageCode.MatchUnknownVariant
        (twoUnions + "match v do\n  case Circle(r) then Output.a = 1\n  case Square(s) then Output.a = 2\n  case Empty then Output.a = 3\n  " + extraCase + "\nend")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1690")>]
[<InlineData("case Triangle(a) then 9")>]
[<InlineData("case Z then 9")>]
[<InlineData("case Shape then 9")>]
let Match_ExpressionCaseNamingNoVariantOfTheMatchUnion_IsRejectedWithMatchUnknownVariant (extraCase: string) =
    rejects MessageCode.MatchUnknownVariant
        (twoUnions + "Output.r = match v do case Circle(r) then 1 case Square(s) then 2 case Empty then 3 " + extraCase + " end")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let Match_UnionIsThatOfFirstCaseNamingADeclaredVariant () =
    // Q names nothing; Z decides the union (B); Circle is then of another union.
    let r = Script.compile (twoUnions + "match v do\n  case Q then Output.a = 1\n  case Z then Output.a = 2\n  case Circle(r) then Output.a = 3\nend")
    Assert.False(r.IsSuccess)
    Assert.Equal<MessageCode list>([ MessageCode.MatchUnknownVariant; MessageCode.MatchUnknownVariant ], Script.errorCodes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let Match_AllCasesOfOneUnion_IsAccepted () =
    compiles (twoUnions + allCasesStmt)

// ---- 50.9.4 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1968")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\n  case Square(s) then Output.a = 2\nend")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\nend")>]
[<InlineData("match v do\n  case Empty then Output.a = 1\nend")>]
[<InlineData("Output.r = match v do case Circle(r) then 1 case Square(s) then 2 end")>]
[<InlineData("Output.r = match v do case Empty then 1 end")>]
let ``JYRO-REQ-1968 Match_MissingAVariant_IsRejectedWithMatchNotExhaustive`` (stmt: string) =
    // Rejected by validation: before anything runs, and every error of the stage Validate.
    rejectedAt MessageCode.MatchNotExhaustive "Validate" (shape + "var v = Empty()\n" + stmt) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1968")>]
let ``JYRO-REQ-1968 Match_CaseOfAnotherUnion_GivesMatchUnknownVariantNotMatchNotExhaustive`` () =
    let src = "union A X Y end\nunion B Z end\nvar v = X()\nmatch v do\n  case X then Output.a = 1\n  case Y then Output.a = 2\n  case Z then Output.a = 3\nend"
    rejects MessageCode.MatchUnknownVariant src
    notRejectedWith MessageCode.MatchNotExhaustive src

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1968")>]
let ``JYRO-REQ-1968 Match_CaseOfAnotherUnionAndMissingVariant_GivesBoth`` () =
    let src = "union A X Y end\nunion B Z end\nvar v = X()\nmatch v do\n  case X then Output.a = 1\n  case Z then Output.a = 3\nend"
    rejects MessageCode.MatchUnknownVariant src
    rejects MessageCode.MatchNotExhaustive src

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1968")>]
let ``JYRO-REQ-1968 Match_AllVariants_IsAccepted`` () =
    notRejectedWith MessageCode.MatchNotExhaustive (shape + "var v = Empty()\n" + allCasesStmt)
    compiles (shape + "var v = Empty()\n" + allCasesStmt)

// ---- 50.9.5 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-232")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\n  case Circle(q) then Output.a = 2\n  case Square(s) then Output.a = 3\n  case Empty then Output.a = 4\nend")>]
[<InlineData("match v do\n  case Circle(r) then Output.a = 1\n  case Square(s) then Output.a = 3\n  case Empty then Output.a = 4\n  case Empty then Output.a = 5\nend")>]
[<InlineData("Output.r = match v do case Circle(r) then 1 case Square(s) then 2 case Circle(r) then 3 case Empty then 4 end")>]
let Match_TwoCasesForOneVariant_IsRejectedWithMatchDuplicateCase (stmt: string) =
    rejects MessageCode.MatchDuplicateCase (shape + "var v = Empty()\n" + stmt)

// ---- 50.9.6 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-233")>]
[<InlineData("case Circle then", "case Empty then")>]
[<InlineData("case Circle() then", "case Empty then")>]
[<InlineData("case Circle(a, b) then", "case Empty then")>]
[<InlineData("case Circle(r) then", "case Empty(x) then")>]
let Match_BindingCountDiffersFromFieldCount_IsRejectedWithMatchBindingCountMismatch (circleCase: string, emptyCase: string) =
    rejects MessageCode.MatchBindingCountMismatch
        (shape + sprintf "var v = Empty()\nmatch v do\n  %s Output.a = 1\n  case Square(s) then Output.a = 2\n  %s Output.a = 3\nend" circleCase emptyCase)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-233")>]
[<InlineData("case Empty then")>]
[<InlineData("case Empty() then")>]
let Match_BindingCountEqualsFieldCount_IsAccepted (emptyCase: string) =
    let src = shape + sprintf "var v = Empty()\nmatch v do\n  case Circle(r) then Output.a = 1\n  case Square(s) then Output.a = 2\n  %s Output.a = 3\nend" emptyCase
    notRejectedWith MessageCode.MatchBindingCountMismatch src
    compiles src

// ---- 50.9.7 --------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1969")>]
let Match_SubjectEvaluatedOnce_CaseOfItsVariantRuns () =
    assertOut """{"log": ["s"], "b": 4}"""
        (probe + shape + "match P(Output, \"s\", Square(4)) do\n  case Circle(r) then Output.b = \"c\"\n  case Square(s) then Output.b = s\n  case Empty then Output.b = \"e\"\nend")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1969")>]
let Match_ExpressionSubjectEvaluatedOnce () =
    assertOut """{"log": ["s"], "r": 4}"""
        (probe + shape + "Output.r = match P(Output, \"s\", Square(4)) do case Circle(r) then 0 case Square(s) then s case Empty then 1 end")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1969")>]
let Match_NamesBoundByPosition () =
    assertR "[1, 2]" "union PU Pt(x, y) end\nmatch Pt(1, 2) do\n  case Pt(b, a) then Output.r = [b, a]\nend"

// ---- 50.9.8 --------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1970")>]
[<InlineData("Circle(3)", "6")>]
[<InlineData("Square(5)", "5")>]
[<InlineData("Empty()", "0")>]
let MatchExpression_GivesTheRunningCaseValue (subject: string, expected: string) =
    assertR expected (shape + sprintf "Output.r = match %s do case Circle(r) then r * 2 case Square(s) then s case Empty then 0 end" subject)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1970")>]
let MatchExpression_Nested () =
    assertR "\"sq-e\""
        (shape + "var v = Square(Empty())\nOutput.r = match v do case Circle(r) then \"c\" case Square(s) then \"sq-\" & (match s do case Circle(q) then \"c\" case Square(t) then \"s\" case Empty then \"e\" end) case Empty then \"e\" end")

// ---- 50.9.9 --------------------------------------------------------------

let private rectShape = "union S Circle(r) Rect(w, h) end\n"
let private rectMatch = "\nmatch v do\n  case Circle(r) then Output.k = \"c\"\n  case Rect(w, h) then Output.k = \"r\"\nend"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1971")>]
[<InlineData("Circle(1)", "c")>]
[<InlineData("{_variant: \"Circle\", r: 1}", "c")>]
[<InlineData("{r: 1, _variant: \"Circle\"}", "c")>]
[<InlineData("{\"_variant\": \"Rect\", \"w\": 1, \"h\": 2}", "r")>]
[<InlineData("FromJson('{\"_variant\":\"Rect\",\"w\":1,\"h\":2}')", "r")>]
[<InlineData("{_variant: \"Rect\"}", "r")>]
[<InlineData("{_variant: \"Circle\", r: 1, z: 2}", "c")>]
[<InlineData("Input.rect", "r")>]
let ``JYRO-REQ-1971 MatchSubject_ObjectWhoseVariantPropertyNamesTheVariant_MatchesItsCase`` (subject: string, expected: string) =
    let r = Script.runJson """{"rect": {"_variant": "Rect", "w": 1, "h": 2}}""" (rectShape + sprintf "var v = %s" subject + rectMatch)
    Assert.Equal(norm (sprintf """{"k": "%s"}""" expected), Values.toJson (Script.output r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1971")>]
let ``JYRO-REQ-1971 MatchSubject_FromHostJsonInput_Matches`` () =
    let r = Script.runJson """{"_variant": "Rect", "w": 1, "h": 2}""" (rectShape + "var v = Input" + rectMatch)
    Assert.Equal(norm """{"k": "r"}""", Values.toJson (Script.output r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1971")>]
[<InlineData("null")>]
[<InlineData("1")>]
[<InlineData("\"Circle\"")>]
[<InlineData("true")>]
[<InlineData("[1]")>]
[<InlineData("{}")>]
[<InlineData("{_variant: \"Square\"}")>]
[<InlineData("{_variant: 1}")>]
[<InlineData("{_variant: null}")>]
[<InlineData("{_variant: \"circle\"}")>]
[<InlineData("{_variant: [\"Circle\"]}")>]
[<InlineData("x => x")>]
let ``JYRO-REQ-1971 MatchSubject_NotAValueOfANamedVariant_RaisesMatchSubjectNotVariant`` (subject: string) =
    let r = Script.run (rectShape + sprintf "var v = %s" subject + rectMatch)
    let m = Script.onlyErrorMessage r
    Assert.Equal(5403, int m.Code)
    Assert.Equal(Pending.matchSubjectNotVariant, m.Code)
    Assert.Equal("{}", failedOutputJson r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1971")>]
let ``JYRO-REQ-1971 MatchSubject_NotVariant_InExpressionForm_RaisesMatchSubjectNotVariant`` () =
    let r = Script.run (rectShape + "var v = {}\nOutput.r = match v do case Circle(r) then 1 case Rect(w, h) then 2 end")
    Assert.Equal(5403, int (Script.onlyError r))

/// MatchSubjectNotVariant is a runtime error, so 160.1.12 gives it the stage Execute, in the
/// statement and the expression form alike. Moved here from JYRO-REQ-1670's stage table
/// (JYRO-ADR-113 item 2, JYRO-DEF-505): this requirement raises the code, so it depends on 1670.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1971")>]
[<InlineData("var v = null\nmatch v do case Circle(r) then Output.k = 1 case Rect(w, h) then Output.k = 2 end")>]
[<InlineData("var v = {}\nOutput.r = match v do case Circle(r) then 1 case Rect(w, h) then 2 end")>]
let ``JYRO-REQ-1971 MatchSubjectNotVariant_HasTheStageExecute`` (body: string) =
    let r = Script.run (rectShape + body)
    let m = Script.onlyErrorMessage r
    Assert.Equal(5403, int m.Code)
    Assert.Equal("Execute", Pending.stageOf m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1971")>]
let ``JYRO-REQ-1971 MatchSubjectNotVariant_IsADeclaredMessageCode`` () =
    Pending.requireMatchSubjectNotVariant ()

// ---- 50.9.10 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-237")>]
[<InlineData("{_variant: \"Pt\", y: 2, x: 1}", "[1, 2]")>]
[<InlineData("{_variant: \"Pt\", x: 1}", "[1, null]")>]
[<InlineData("{_variant: \"Pt\", y: 2}", "[null, 2]")>]
[<InlineData("{_variant: \"Pt\"}", "[null, null]")>]
[<InlineData("{_variant: \"Pt\", x: 1, y: 2, z: 3}", "[1, 2]")>]
[<InlineData("{_variant: \"Pt\", x: null, y: 2}", "[null, 2]")>]
[<InlineData("{z: 3, y: 2, _variant: \"Pt\", x: 1}", "[1, 2]")>]
[<InlineData("Pt(1, 2)", "[1, 2]")>]
let MatchBinding_ByFieldNameOrNull_ExtraPropertiesIgnored (subject: string, expected: string) =
    let pre = sprintf "union PU Pt(x, y) end\nvar v = %s\n" subject
    assertR expected (pre + "match v do\n  case Pt(a, b) then Output.r = [a, b]\nend")
    assertR expected (pre + "Output.r = match v do case Pt(a, b) then [a, b] end")

// ---- 50.10.1 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-238")>]
[<InlineData("union U A end")>]
[<InlineData("union U A B(x) C(x, y: number) end")>]
[<InlineData("union U A(a: any, b: function, c: number, d: string, e: boolean, f: object, g: array, h: null) end")>]
[<InlineData("union U\n  A\n  B(x)\nend")>]
[<InlineData("union U A() end")>]
let Union_ValidForm_IsAccepted (source: string) =
    compiles (source + "\nOutput.ok = true")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-238")>]
[<InlineData("union U end")>]
[<InlineData("union U A")>]
[<InlineData("union U A(x,) end")>]
[<InlineData("union U A(x: foo) end")>]
[<InlineData("union U if end")>]
[<InlineData("union U A(if) end")>]
[<InlineData("union end")>]
[<InlineData("union U A(x = 1) end")>]
[<InlineData("union U A(1) end")>]
[<InlineData("union U A(x y) end")>]
let Union_OtherShape_IsRejectedBeforeExecution (source: string) =
    compileFails source

// ---- 50.10.2 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-239")>]
[<InlineData("func f()\n  union U A end\nend")>]
[<InlineData("func f()\n  if true then union U A end end\nend")>]
[<InlineData("func f()\n  if true then\n    while false do\n      union U A end\n    end\n  end\nend")>]
[<InlineData("union S C end\nfunc f(v)\n  match v do\n    case C then union U A end\n  end\nend")>]
[<InlineData("func f(v)\n  switch v do\n    default then union U A end\n  end\nend")>]
[<InlineData("func f(v)\n  foreach x in v do union U A end end\nend")>]
let ``JYRO-REQ-239 Union_InsideFunctionBody_IsRejectedWithUnionInsideFunctionOnly`` (source: string) =
    rejects MessageCode.UnionInsideFunction source
    notRejectedWith MessageCode.UnionNotAtTopLevel source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-239")>]
[<InlineData("if true then union U A end end")>]
[<InlineData("if false then\nelseif true then union U A end\nend")>]
[<InlineData("if false then\nelse union U A end\nend")>]
[<InlineData("while false do union U A end end")>]
[<InlineData("for i in 0 to 1 do union U A end end")>]
[<InlineData("foreach x in [] do union U A end end")>]
[<InlineData("switch 1 do case 1 then union U A end end")>]
[<InlineData("switch 1 do default then union U A end end")>]
[<InlineData("union S C end\nvar v = C()\nmatch v do case C then union U A end end")>]
[<InlineData("if true then\n  while false do union U A end end\nend")>]
let ``JYRO-REQ-239 Union_InsideTopLevelBlock_IsRejectedWithUnionNotAtTopLevel`` (source: string) =
    rejects MessageCode.UnionNotAtTopLevel source
    notRejectedWith MessageCode.UnionInsideFunction source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-239")>]
let ``JYRO-REQ-239 Union_AtTopLevelBeforeAndAfterStatements_IsAccepted`` () =
    compiles "Output.a = 1\nunion U A end\nOutput.b = 2\nunion V B end"

// ---- 50.10.3 -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let Union_SecondWithSameName_IsRejectedWithDuplicateUnionName () =
    rejects MessageCode.DuplicateUnionName "union U A end\nunion U B end"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1688")>]
[<InlineData("union U A end\nunion V B end")>]
[<InlineData("union U A end\nunion u B end")>]
let Union_DistinctNames_AreAccepted (source: string) =
    notRejectedWith MessageCode.DuplicateUnionName source
    compiles source

// ---- 50.10.4 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1689")>]
[<InlineData("union U A A end")>]
[<InlineData("union U A end\nunion V A end")>]
[<InlineData("union U A(x) end\nunion V A end")>]
[<InlineData("union U A(x) B A(y) end")>]
let Variant_NameAlreadyUsed_IsRejectedWithDuplicateVariantName (source: string) =
    rejects MessageCode.DuplicateVariantName source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let Variant_NamesDifferingInCase_AreAccepted () =
    notRejectedWith MessageCode.DuplicateVariantName "union U A end\nunion V a end"

// ---- 50.10.5 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1972")>]
[<InlineData("union U A(_variant) end")>]
[<InlineData("union U A(x, _variant: string) end")>]
let Variant_FieldNamedVariantKey_IsRejectedWithReservedFieldName (source: string) =
    rejects MessageCode.ReservedFieldName source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1972")>]
[<InlineData("union U A(variant) end")>]
[<InlineData("union U A(_Variant) end")>]
[<InlineData("union U A(_variant_) end")>]
let Variant_OtherFieldNames_AreAccepted (source: string) =
    notRejectedWith MessageCode.ReservedFieldName source
    compiles source

// ---- 50.10.6 -------------------------------------------------------------

let private sv = "union S Circle(r) Pt(x, y) Empty end\n"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-243")>]
let VariantCall_ToJsonIsVariantThenFieldsInOrder () =
    assertRStr "{\"_variant\":\"Circle\",\"r\":1}" (sv + "Output.r = ToJson(Circle(1))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-243")>]
[<InlineData("Keys(Pt(1, 2))", "[\"_variant\", \"x\", \"y\"]")>]
[<InlineData("Keys(Empty())", "[\"_variant\"]")>]
[<InlineData("Keys(Circle(null))", "[\"_variant\", \"r\"]")>]
[<InlineData("Values(Pt(1, 2))", "[\"Pt\", 1, 2]")>]
let VariantCall_PropertiesAreVariantThenFieldsOnly (expr: string, expected: string) =
    assertR expected (sv + sprintf "Output.r = %s" expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-243")>]
let VariantCall_ForeachGivesVariantThenFields () =
    assertR """["Pt", 3, 4]""" (sv + "var l = []\nforeach v in Pt(3, 4) do l = Append(l, v) end\nOutput.r = l")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-243")>]
let VariantCall_HostSeesPropertiesInOrderWithNoOther () =
    let p = Script.outputOf (sv + "Output.p = Pt(3, 4)") |> Values.prop "p"
    match p with
    | :? JyroObject as o ->
        Assert.Equal(3, o.Count)
        Assert.Equal<string list>([ "_variant"; "x"; "y" ], o |> Seq.map (fun kv -> kv.Key) |> List.ofSeq)
        Assert.Equal("Pt", Values.asString (Values.prop "_variant" p))
    | _ -> Values.fail "Pt(3, 4) is not an object"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-243")>]
let VariantCall_GivesANewObjectEachCall () =
    assertR "1" (sv + "var a = Circle(1)\nvar b = Circle(1)\na.r = 5\nOutput.r = b.r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-243")>]
let VariantCall_FieldHoldsTheArgumentItself () =
    assertR "[9]" (sv + "var arr = [1]\nvar c = Circle(arr)\narr[0] = 9\nOutput.r = c.r")

// ---- 50.10.7 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-244")>]
[<InlineData("Output.r = Pt(1)", "TooFewArguments")>]
[<InlineData("Output.r = Pt()", "TooFewArguments")>]
[<InlineData("Output.r = Pt(1, 2, 3)", "TooManyArguments")>]
[<InlineData("Output.r = Empty(1)", "TooManyArguments")>]
let ``JYRO-REQ-244 VariantCall_WrongArgumentCount_IsRejectedAtCompile`` (stmt: string, code: string) =
    // TooFewArguments (80.7.2) and TooManyArguments (80.7.3), at link time, by the
    // names the book gives them, not by number (d-556, 10.1.1).
    let expected = System.Enum.Parse<MessageCode> code
    rejectedAt expected "Link" ("union P Pt(x, y) Empty end\n" + stmt) |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-244")>]
[<InlineData("Output.r = Pt(1, 2)", "{\"_variant\": \"Pt\", \"x\": 1, \"y\": 2}")>]
[<InlineData("Output.r = Empty()", "{\"_variant\": \"Empty\"}")>]
[<InlineData("Output.r = Pt(x: 1, y: 2)", "{\"_variant\": \"Pt\", \"x\": 1, \"y\": 2}")>]
let ``JYRO-REQ-244 VariantCall_OneArgumentPerField_IsAccepted`` (stmt: string, expected: string) =
    assertRL expected ("union P Pt(x, y) Empty end\n" + stmt)

/// The table of 50.10.7, for `union U V(a, b) end`.
let variantTable : seq<obj[]> =
    [ "V(1)", "TooFewArguments"
      "V(1, 2, 3)", "TooManyArguments"
      "V(a: 1, b: 2, c: 3)", "UnknownNamedArgument"
      "V(a: 1, a: 2, b: 3)", "DuplicateNamedArgument"
      "V(a: 1)", "MissingRequiredNamedArgument"
      // The same rules for the other field and other orders.
      "V(b: 1)", "MissingRequiredNamedArgument"
      "V()", "TooFewArguments"
      "V(b: 2, a: 1, b: 3)", "DuplicateNamedArgument"
      "V(c: 1, a: 2, b: 3)", "UnknownNamedArgument" ]
    |> Seq.map (fun (call, code) -> [| box call; box code |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-244")>]
[<MemberData(nameof variantTable)>]
let ``JYRO-REQ-244 VariantCall_TheBookTable_GivesTheCodesOf80_7`` (call: string, codeName: string) =
    let code =
        match codeName with
        | "TooFewArguments" -> MessageCode.TooFewArguments
        | "TooManyArguments" -> MessageCode.TooManyArguments
        | "UnknownNamedArgument" -> MessageCode.UnknownNamedArgument
        | "DuplicateNamedArgument" -> MessageCode.DuplicateNamedArgument
        | _ -> MessageCode.MissingRequiredNamedArgument
    rejectedAt code "Link" (sprintf "union U V(a, b) end\nOutput.r = %s" call) |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-244")>]
[<InlineData("V(1, b: 2)")>]
[<InlineData("V(a: 1, 2)")>]
let ``JYRO-REQ-244 VariantCall_MixedForms_IsAParseError`` (call: string) =
    parseErrorL (sprintf "union U V(a, b) end\nOutput.r = %s" call) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-244")>]
let ``JYRO-REQ-244 VariantCall_ByName_BindsByNameInFieldOrder`` () =
    assertRStrL "{\"_variant\":\"V\",\"a\":1,\"b\":2}" "union U V(a, b) end\nOutput.r = ToJson(V(b: 2, a: 1))"
    assertRStrL "{\"_variant\":\"V\",\"a\":1,\"b\":2}" "union U V(a, b) end\nOutput.r = ToJson(V(a: 1, b: 2))"
    // The host sees the same order.
    let r = rOfL "union U V(a, b) end\nOutput.r = V(b: 2, a: 1)"
    match r with
    | :? JyroObject as o ->
        Assert.Equal<string list>([ "_variant"; "a"; "b" ], o |> Seq.map (fun kv -> kv.Key) |> List.ofSeq)
    | _ -> Values.fail "V(b: 2, a: 1) is not an object"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-244")>]
let ``JYRO-REQ-244 VariantWithNoFields_IsCalledWithNoArgument`` () =
    assertRStrL "{\"_variant\":\"W\"}" "union U W end\nOutput.r = ToJson(W())"
    rejectedAt MessageCode.TooManyArguments "Link" "union U W end\nOutput.r = W(1)" |> ignore
    rejectedAt MessageCode.UnknownNamedArgument "Link" "union U W end\nOutput.r = W(a: 1)" |> ignore

// ---- 50.10.8 -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-245")>]
let ``JYRO-REQ-245 VariantCall_HintedFieldArguments_AreCoerced`` () =
    assertR """{"_variant": "C", "r": 2, "s": "5", "b": true, "a": [1]}"""
        "union S C(r: number, s: string, b: boolean, a: any) end\nOutput.r = C(\"2\", 5, \"TRUE\", [1])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-245")>]
let ``JYRO-REQ-245 VariantCall_UnhintedField_IsNotCoerced`` () =
    assertR """{"_variant": "D", "u": "2"}""" "union T D(u) end\nOutput.r = D(\"2\")"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-245")>]
[<InlineData("C(\"x\", 1, true, 1)")>]
[<InlineData("C(null, 1, true, 1)")>]
[<InlineData("C(1, null, true, 1)")>]
[<InlineData("C(1, 1, \"yes\", 1)")>]
[<InlineData("C([], 1, true, 1)")>]
let ``JYRO-REQ-245 VariantCall_HintedFieldNotCoercible_RaisesInvalidType`` (call: string) =
    runtimeError MessageCode.InvalidType (sprintf "union S C(r: number, s: string, b: boolean, a: any) end\nOutput.r = %s" call)

// ---- 50.10.9 -------------------------------------------------------------

/// 50.10.9 rejects "at link time": compiled under `options` (every limit set, 4.1.11), the
/// compile fails with an Error of `code`, no other code, and that Error has the stage Link
/// (160.1.12, read through `Pending.stageOf`).
let private rejectedAtLink (options: RunOptions) (code: MessageCode) (source: string) : unit =
    let r = Script.compileWith { options with Limits = limited.Limits; CompilationTimeout = limited.CompilationTimeout } source
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    if r.IsSuccess || not (errors |> List.exists (fun m -> m.Code = code)) then
        Values.fail (sprintf "Expected the compile to fail with %A. IsSuccess=%b. Diagnostics:\n%s\nSource:\n%s" code r.IsSuccess (Script.describe r) source)
    for m in errors do
        if m.Code <> code then
            Values.fail (sprintf "Expected only %A, also got %A. Diagnostics:\n%s\nSource:\n%s" code m.Code (Script.describe r) source)
        Assert.Equal("Link", Pending.stageOf m)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-246")>]
[<InlineData("union U Foo end")>]
[<InlineData("union U A Foo(x) end\nOutput.r = 1")>]
let ``JYRO-REQ-246 Variant_NamedAsHostFunction_IsRejectedWithVariantShadowsFunction`` (source: string) =
    let foo = Host.recorder "Foo"
    rejectedAtLink { Script.defaults with Functions = [ foo.AsFunction ] } MessageCode.VariantShadowsFunction source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-246")>]
[<InlineData("union U Bar end\nfunc Bar()\nend")>]
[<InlineData("func Bar()\nend\nunion U Bar end")>]
[<InlineData("union U A Bar(x) end\nfunc Bar(y)\n  return y\nend")>]
let ``JYRO-REQ-246 Variant_NamedAsScriptFunction_IsRejectedWithFunctionShadowsVariant`` (source: string) =
    rejectedAtLink Script.defaults MessageCode.FunctionShadowsVariant source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-246")>]
let ``JYRO-REQ-246 Variant_NamedAsLibraryFunction_IsRejectedWithFunctionShadowsVariant`` () =
    rejectedAtLink { Script.defaults with Libraries = [ "func Baz()\n  return 1\nend" ] } MessageCode.FunctionShadowsVariant "union U Baz end"

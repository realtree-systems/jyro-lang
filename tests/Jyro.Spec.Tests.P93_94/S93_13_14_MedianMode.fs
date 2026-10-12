/// jyro-spec 93.13 Median, 93.14 Mode.
module RealTree.Jyro.Spec.Tests.P93_94.S93_13_14_MedianMode

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private bText = "1.0000000000000000000000000001"
let private bDec = dec bText

/// a = exact 1, b = exact 1.0000000000000000000000000001, c = inexact 1.
let private evalAbc (hostC: bool) (expr: string) : JyroValue =
    let decl = if hostC then "var c = Input.c" else "var c = Power(1, 0.5)"
    let pairs = if hostC then [ "c", hostInexact 1.0 ] else []
    evalScriptIn pairs (sprintf "var a = 1\nvar b = %s\n%s\nOutput.r = %s" bText decl expr)

let private permutations =
    [ "[a, b, c]"; "[a, c, b]"; "[b, a, c]"; "[b, c, a]"; "[c, a, b]"; "[c, b, a]" ]

let private nonArrayLiterals = [ "5"; "\"12\""; "\"\""; "true"; "{}"; "{a: 1}"; "f" ]

let private expectNonArrayMismatch (fn: string) =
    for lit in nonArrayLiterals do
        expectRuntimeError MessageCode.ArgumentTypeMismatch (withFn (sprintf "%s(%s)" fn lit))
    for v in [ Values.exact 5m; Values.str "12"; Values.bool true; Values.obj [ "a", Values.exact 1m ] ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch (sprintf "%s(Input.v)" fn)

let private zeros () = [ "n", hostInexact -0.0; "p", hostInexact 0.0 ]

/// The value only, near `expected`, whatever its representation.
let private assertNear (expected: float) (v: JyroValue) =
    let actual = Values.asFloat v
    if abs (actual - expected) > abs expected * 1e-15 then
        Values.fail (sprintf "Expected a number near %.17g, got %.17g" expected actual)

// ================================================================= 93.13 Median

[<Fact>]
[<Trait("requirement", "JYRO-REQ-974")>]
let ``Median_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Median()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Median([1], [2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-974")>]
let ``Median_ParameterIsNamedValues_AndReturnsANumberOrNull`` () =
    assertNumber 1.5 (eval "Median(values: [1, 2])")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Median(items: [1])"
    assertString "number" (eval "TypeOf(Median([1, 2]))")
    assertString "null" (eval "TypeOf(Median([]))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-975")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``JYRO-REQ-975 Median_OfBCA_AndOfCBA_ReturnsB`` (hostC: bool) =
    assertExact bDec (evalAbc hostC "Median([b, c, a])")
    assertExact bDec (evalAbc hostC "Median([c, b, a])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-975")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``JYRO-REQ-975 Median_OfEveryPermutationOfABC_ReturnsB`` (hostC: bool) =
    // The order is a, b, c whatever their positions, so b is in the middle.
    for p in permutations do assertExact bDec (evalAbc hostC (sprintf "Median(%s)" p))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-975")>]
let ``JYRO-REQ-975 Median_OfBCA_ExactProbeIdentifiesB`` () =
    assertTrue (evalAbc false "Median([b, c, a]) - 1 == 0.0000000000000000000000000001")
    assertTrue (evalAbc false "Median([c, b, a]) - 1 == 0.0000000000000000000000000001")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-975")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``JYRO-REQ-975 Median_OfEqualDoubles_UsesTheSortOrder`` (hostC: bool) =
    // [a, c, a] orders as a, a, c; [c, a, c] as a, c, c; [c, b, c] as b, c, c;
    // [b, a, b] as a, b, b; [b, a, c, a, c] as a, a, b, c, c; [c, a, c, b, a] as a, a, b, c, c.
    assertExact 1m (evalAbc hostC "Median([a, c, a])")
    assertInexact 1.0 (evalAbc hostC "Median([c, a, c])")
    assertInexact 1.0 (evalAbc hostC "Median([c, b, c])")
    assertExact bDec (evalAbc hostC "Median([b, a, b])")
    assertExact bDec (evalAbc hostC "Median([b, a, c, a, c])")
    assertExact bDec (evalAbc hostC "Median([c, a, c, b, a])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-975")>]
[<InlineData("[3, 1, 2]", "2")>]
[<InlineData("[5]", "5")>]
[<InlineData("[-1, -3, -2]", "-2")>]
[<InlineData("[0.1000000000000000000000000002, 0.1000000000000000000000000001, 0.1]", "0.1000000000000000000000000001")>]
[<InlineData("[0.1, 0.1000000000000000000000000002, 0.1000000000000000000000000001]", "0.1000000000000000000000000001")>]
[<InlineData("[9, 1, 8, 2, 7]", "7")>]
[<InlineData("[2, 2, 1]", "2")>]
let ``JYRO-REQ-975 Median_OfAnOddCount_ReturnsTheMiddleNumber`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Median(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-975")>]
let ``JYRO-REQ-975 Median_OfDifferentDoubles_TheDoubleValueDecides`` () =
    assertInexact (sqrt 2.0) (eval "Median([2, Power(2, 0.5), 1])")
    // The double nearest 0.3 is below 0.30000000000000004, and above 0.29999999999999993.
    assertExact 0.3m (evalIn [ "h", hostInexact 0.30000000000000004; "l", hostInexact 0.29999999999999993 ] "Median([Input.h, 0.3, Input.l])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-975")>]
let ``JYRO-REQ-975 Median_OfTiedInexactZeros_KeepsTheirOrderInValues`` () =
    // -0 and +0 have one double and are both inexact: tied, so they keep their order.
    let r1 = evalIn (zeros ()) "Median([Input.p, Input.n, 1])"
    assertInexact 0.0 r1
    assertSignBit true r1
    let r2 = evalIn (zeros ()) "Median([Input.n, Input.p, 1])"
    assertInexact 0.0 r2
    assertSignBit false r2
    let r3 = evalIn (zeros ()) "Median([-1, Input.n, Input.p])"
    assertInexact 0.0 r3
    assertSignBit true r3

[<Fact>]
[<Trait("requirement", "JYRO-REQ-976")>]
let ``Median_Of1324_Returns2Point5`` () = assertNumber 2.5 (eval "Median([1, 3, 2, 4])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-976")>]
[<InlineData("[1, 2]", 1.5)>]
[<InlineData("[4, 1]", 2.5)>]
[<InlineData("[-3, -1]", -2.0)>]
[<InlineData("[null, 1, 3, null]", 2.0)>]
[<InlineData("[10, 1, 4, 3, 2, 9]", 3.5)>]
let ``Median_OfAnEvenCount_ReturnsTheMeanOfTheMiddlePair`` (values: string, expected: float) =
    assertNumber expected (eval (sprintf "Median(%s)" values))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-976")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``Median_OfAnEvenCount_UsesTheSortOrderForTheMiddlePair`` (hostC: bool) =
    // Only the value is checked here; exactness is 93.13.6, 93.13.7 and 93.13.9.
    assertNumber 1.0 (evalAbc hostC "Median([b, 3, a, 0])")
    assertNumber 1.0 (evalAbc hostC "Median([3, b, 0, a])")
    assertNumber 1.0 (evalAbc hostC "Median([c, 3, a, 0])")
    assertNumber 1.0 (evalAbc hostC "Median([b, c, 2, a])")
    assertNumber 1.0 (evalAbc hostC "Median([c, a])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-977")>]
let ``Median_OfNull_ReturnsNull`` () =
    assertNull (eval "Median(null)")
    assertNull (eval "Median(values: null)")
    assertNull (evalIn [ "v", Values.nul ] "Median(Input.v)")
    assertNull (eval "Median([null])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-978")>]
let ``Median_OfNonArray_RaisesArgumentTypeMismatch`` () = expectNonArrayMismatch "Median"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-979")>]
let ``JYRO-REQ-979 Median_OfTwoMaxExact_ReturnsExactMax`` () =
    let r = Script.run "Output.r = Median([79228162514264337593543950335, 79228162514264337593543950335])"
    assertNoDiagnostics r
    assertExact (dec "79228162514264337593543950335") (Script.output r |> Values.prop "r")

/// The median is held; for some the total of the middle pair is not, which does not matter.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-979")>]
[<InlineData("[79228162514264337593543950334, 79228162514264337593543950335, 1]", "79228162514264337593543950334")>]
[<InlineData("[-79228162514264337593543950335, -79228162514264337593543950335]", "-79228162514264337593543950335")>]
[<InlineData("[79228162514264337593543950334, 79228162514264337593543950332]", "79228162514264337593543950333")>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950333, 0, 79228162514264337593543950335]", "79228162514264337593543950334")>]
[<InlineData("[50000000000000000000000000000, 60000000000000000000000000000]", "55000000000000000000000000000")>]
[<InlineData("[1, 3, 2, 4]", "2.5")>]
[<InlineData("[0.1, 0.2]", "0.15")>]
[<InlineData("[0, 0.0000000000000000000000000002]", "0.0000000000000000000000000001")>]
let ``JYRO-REQ-979 Median_OfExactNumbersWithAHeldMedian_IsExact`` (values: string, expected: string) =
    let r = Script.run (sprintf "Output.r = Median(%s)" values)
    assertNoDiagnostics r
    assertExact (dec expected) (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-979")>]
let ``JYRO-REQ-979 Median_OfExactNumbers_KeepsExactArithmetic`` () =
    assertTrue (eval "Median([0.1, 0.2]) + 0.05 == 0.2")
    assertExact 0.15m (evalIn [ "a", Values.arr [ hostExact 0.2m; hostExact 0.1m ] ] "Median(Input.a)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-980")>]
let ``JYRO-REQ-980 Median_Of0AndTheSmallestExact_IsInexact`` () =
    assertInexact ((0.0 + 1e-28) / 2.0) (eval "Median([0, 0.0000000000000000000000000001])")

/// The median needs 29 decimal places, beyond the exact representation.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-980")>]
[<InlineData("[0.0000000000000000000000000001, 0.0000000000000000000000000002]", 1e-28, 2e-28)>]
[<InlineData("[1, 0.0000000000000000000000000001]", 1e-28, 1.0)>]
[<InlineData("[b, 3, a, 0]", 1.0, 1.0)>]
[<InlineData("[0.0000000000000000000000000003, 0, 0.0000000000000000000000000004, 5]", 3e-28, 4e-28)>]
let ``JYRO-REQ-980 Median_OfExactNumbersWithoutAHeldMedian_IsInexactDouble`` (values: string, x: float, y: float) =
    let v = evalScriptIn [] (sprintf "var a = 1\nvar b = %s\nOutput.r = Median(%s)" bText values)
    assertInexact ((x + y) / 2.0) v

[<Fact>]
[<Trait("requirement", "JYRO-REQ-981")>]
let ``JYRO-REQ-981 Median_OfCAB_ReturnsExactB_AndOfCAC_ReturnsInexact1`` () =
    for hostC in [ false; true ] do
        assertExact bDec (evalAbc hostC "Median([c, a, b])")
        assertInexact 1.0 (evalAbc hostC "Median([c, a, c])")
        assertExact 1m (evalAbc hostC "Median([a, c, a])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-981")>]
let ``JYRO-REQ-981 Median_OfAnOddCount_ProbesIdentifyTheRepresentation`` () =
    assertTrue (evalAbc false "Median([c, a, b]) - 1 == 0.0000000000000000000000000001")
    assertTrue (evalAbc false "Median([c, a, c]) * 0.1 + 0.2 != 0.3")
    assertTrue (evalAbc false "Median([a, c, a]) * 0.1 + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-981")>]
let ``JYRO-REQ-981 Median_OfAnOddCountWithDifferentDoubles_KeepsTheSelectedRepresentation`` () =
    assertInexact 2.0 (evalIn [ "h", hostInexact 2.0 ] "Median([Input.h, 1, 3])")
    assertExact 2m (evalIn [ "h", hostInexact 1.0 ] "Median([2, Input.h, 3])")
    assertExact 0.2m (evalIn [ "h", hostInexact 5.0 ] "Median([0.1, Input.h, 0.2])")
    assertTrue (evalIn [ "h", hostInexact 5.0 ] "Median([0.1, Input.h, 0.2]) + 0.1 == 0.3")
    assertInexact (sqrt 2.0) (eval "Median([3, Power(2, 0.5), 0.1])")
    assertExact 0.2m (evalIn [ "a", Values.arr [ hostInexact 0.5; hostExact 0.2m; hostInexact 0.1 ] ] "Median(Input.a)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-982")>]
let ``Median_OfAnEvenCountWithAnInexact_IsInexactDouble`` () =
    assertInexact 2.5 (evalIn [ "h", hostInexact 3.0 ] "Median([1, 2, Input.h, 4])")
    assertInexact 1.5 (evalIn [ "h", hostInexact 1.0 ] "Median([Input.h, 2])")
    assertInexact ((0.2 + 5.0) / 2.0) (evalIn [ "h", hostInexact 5.0 ] "Median([0.1, 0.2, Input.h, 6])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-983")>]
let ``JYRO-REQ-983 Median_Of1Point7e308Twice_Returns1Point7e308`` () =
    let r = Script.run "Output.r = Median([1.7e308, 1.7e308])"
    assertNoDiagnostics r
    assertNumber 1.7e308 (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-983")>]
let ``JYRO-REQ-983 Median_WhenTheMiddlePairTotalIsNotFinite_ReturnsTheFiniteMedian`` () =
    for (values, median) in
        [ "[-1.7e308, -1.7e308]", -1.7e308
          "[1.7e308, 1.5e308]", 1.6e308
          "[0, 1.7e308, 1.7e308, 1.7e308]", 1.7e308
          "[1.7e308, -1.7e308, 1.7e308, 1.1e308]", 1.4e308 ] do
        let r = Script.run (sprintf "Output.r = Median(%s)" values)
        assertNoDiagnostics r
        assertNear median (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-983")>]
let ``JYRO-REQ-983 Median_OfHostDoublesWhoseMiddlePairTotalIsNotFinite_ReturnsTheFiniteMedian`` () =
    let a = Values.arr [ hostInexact Double.MaxValue; hostInexact Double.MaxValue ]
    let r = runIn [ "a", a ] "Output.r = Median(Input.a)"
    assertNoDiagnostics r
    assertNear Double.MaxValue (Script.output r |> Values.prop "r")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-984")>]
let ``Median_IsPure_AndDoesNotChangeItsArgumentOrItsOrder`` () =
    assertCompilesPure "Median([3, 1, null, 2])"
    assertTrue (evalScriptIn [] "var a = [3, 1, null, 2]\nvar m = Median(a)\nOutput.r = a == [3, 1, null, 2] and Length(a) == 4")
    assertTrue (evalScriptIn [] "var a = [4, 3, 2, 1]\nvar m = Median(a)\nOutput.r = a == [4, 3, 2, 1]")

// ================================================================= 93.14 Mode

/// a = exact 0.1000000000000000000000000001, b = exact 0.1, d and e = inexact 0.1.
/// a and b are not equal (two exact values, 30.10); each equals d and e (one double).
let private evalMode (expr: string) : JyroValue =
    evalScriptIn [ "d", hostInexact 0.1; "e", hostInexact 0.1 ]
        (sprintf "var a = 0.1000000000000000000000000001\nvar b = 0.1\nvar d = Input.d\nvar e = Input.e\nOutput.r = Mode(%s)" expr)

let private isModeA v = assertExact (dec "0.1000000000000000000000000001") v
let private isModeB v = assertExact 0.1m v
let private isModeD v = assertInexact 0.1 v

[<Fact>]
[<Trait("requirement", "JYRO-REQ-985")>]
let ``Mode_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Mode()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Mode([1], [2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-985")>]
let ``Mode_ParameterIsNamedValues_AndReturnsANumberOrNull`` () =
    assertExact 1m (eval "Mode(values: [1, 1])")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Mode(items: [1])"
    assertString "number" (eval "TypeOf(Mode([1, 1]))")
    assertString "null" (eval "TypeOf(Mode([]))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-986")>]
let ``JYRO-REQ-986 Mode_ReturnsTheFirstNumberOfTheLargestGroup`` () =
    // [a, b, d]: groups {a, d}, {b}.     [b, a, d]: {b, d}, {a}.
    // [a, b, d, e]: {a, d, e}, {b}.      [b, a, b, d]: {b, b, d}, {a}.
    // [b, a, a, d]: {b, d}, {a, a}, a tie that 93.14.3 gives to the group whose first number is first.
    // [d, a, a]: {d, a, a}: d is its first number.
    isModeA (evalMode "[a, b, d]")
    isModeB (evalMode "[b, a, d]")
    isModeA (evalMode "[a, b, d, e]")
    isModeB (evalMode "[b, a, b, d]")
    isModeB (evalMode "[b, a, a, d]")
    isModeD (evalMode "[d, a, a]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-986")>]
[<InlineData("[1, 2, 2, 3]", "2")>]
[<InlineData("[2, 1, 1, 2, 3]", "2")>]
[<InlineData("[1, 2, 3, 3, 2, 3]", "3")>]
[<InlineData("[4, 5, 5, 4, 5]", "5")>]
let ``JYRO-REQ-986 Mode_OfPlainNumbers_ReturnsTheMostFrequent`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Mode(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1755")>]
let ``JYRO-REQ-1755 Mode_Of3322_Returns3`` () = assertExact 3m (eval "Mode([3, 3, 2, 2])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1755")>]
[<InlineData("[2, 3, 3, 2]", "2")>]
[<InlineData("[1, 2, 2, 1, 3, 3]", "1")>]
[<InlineData("[4, 5, 5]", "5")>]
[<InlineData("[null, 2, 1, 1, 2]", "2")>]
[<InlineData("[7, 8, 9, 9, 8]", "8")>]
let ``JYRO-REQ-1755 Mode_BreaksATieByTheLowestFirstOccurrence`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Mode(%s)" values))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1755")>]
[<InlineData("[3, 3, 2, 2]", "3")>]
[<InlineData("[2, 2, 3, 3]", "2")>]
[<InlineData("[1, 1, 2, 2, 3, 3]", "1")>]
[<InlineData("[3, 3, 2, 2, 1, 1]", "3")>]
[<InlineData("[2, 3, 1, 1, 3, 2]", "2")>]
[<InlineData("[4, 5, 5, 4, 4, 5]", "4")>]
[<InlineData("[5, 4, 4, 5, 5, 4]", "5")>]
[<InlineData("[null, 3, null, 3, 2, 2]", "3")>]
[<InlineData("[2, null, 3, 3, 2]", "2")>]
[<InlineData("[null, null, 2, 3, 3, 2]", "2")>]
[<InlineData("[-1, 0, 0, -1]", "-1")>]
[<InlineData("[0.5, 0.25, 0.25, 0.5]", "0.5")>]
let ``JYRO-REQ-1755 Mode_OfTiedGroups_ReturnsTheNumberOfTheGroupThatStartsFirst`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Mode(%s)" values))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1755")>]
[<InlineData("[1, 2, 2, 3, 3, 3]", "3")>]
[<InlineData("[2, 2, 3, 3, 3]", "3")>]
[<InlineData("[3, 3, 2, 2, 2]", "2")>]
[<InlineData("[1, 1, 2, 2, 3, 3, 3, 1]", "1")>]
[<InlineData("[null, 2, 2, 3, 3, 3]", "3")>]
let ``JYRO-REQ-1755 Mode_WhenOneGroupIsLarger_TheTieRuleDoesNotApply`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Mode(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1755")>]
let ``JYRO-REQ-1755 Mode_OfTiedGroupsOfExactNumbersThatAreNotEqual_ReturnsTheGroupThatStartsFirst`` () =
    // a and b are exact and not equal (30.10), so [b, a, a, b] forms the groups {b, b} and {a, a}.
    isModeB (evalMode "[b, a, a, b]")
    isModeA (evalMode "[a, b, b, a]")
    isModeA (evalMode "[a, a, b, b]")
    isModeB (evalMode "[b, b, a, a]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-988")>]
[<InlineData("[5, 3, 1]", "5")>]
[<InlineData("[null, 3, 5]", "3")>]
[<InlineData("[7]", "7")>]
[<InlineData("[-1, 0, 1]", "-1")>]
let ``Mode_WhenEveryCountIsOne_ReturnsTheFirstNumber`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Mode(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-989")>]
let ``Mode_OfNull_ReturnsNull`` () =
    assertNull (eval "Mode(null)")
    assertNull (eval "Mode(values: null)")
    assertNull (evalIn [ "v", Values.nul ] "Mode(Input.v)")
    assertNull (eval "Mode([])")
    assertNull (eval "Mode([null])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-990")>]
let ``Mode_OfNonArray_RaisesArgumentTypeMismatch`` () = expectNonArrayMismatch "Mode"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-990")>]
let ``Mode_WithANonNumberElement_RaisesNumericElementRequired_NotArgumentTypeMismatch`` () =
    let m = runtimeErrorIn [] "Output.r = Mode([1, \"a\"])"
    Assert.Equal(MessageCode.NumericElementRequired, m.Code)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-991")>]
[<InlineData("[0.1, 0.1, 0.2]", "0.1")>]
[<InlineData("[79228162514264337593543950335, 79228162514264337593543950335]", "79228162514264337593543950335")>]
[<InlineData("[3, 1]", "3")>]
let ``Mode_OfExactNumbers_IsExact`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Mode(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-992")>]
let ``JYRO-REQ-992 Mode_OfMixed_ReturnsTheFirstElementOfTheWinningValue`` () =
    let h () = [ "h", hostInexact 1.0 ]
    // [h, 1, 2]: {h, 1}, {2}.  [1, h, 2]: {1, h}, {2}.  [2, h, 1]: {2}, {h, 1}.
    assertInexact 1.0 (evalIn (h ()) "Mode([Input.h, 1, 2])")
    assertExact 1m (evalIn (h ()) "Mode([1, Input.h, 2])")
    assertInexact 1.0 (evalIn (h ()) "Mode([2, Input.h, 1])")
    assertExact 1m (evalIn (h ()) "Mode([2, 1, Input.h, 1])")
    assertTrue (evalIn (h ()) "Mode([1, Input.h, 2]) * 0.1 + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-992")>]
let ``JYRO-REQ-992 Mode_OfMixedInScript_ReturnsTheFirstElementOfTheWinningValue`` () =
    assertInexact 1.0 (evalScriptIn [] "var c = Power(1, 0.5)\nOutput.r = Mode([c, 1, 2])")
    assertExact 0.1m (evalScriptIn [] "var c = 0.1 * Power(1, 0.5)\nOutput.r = Mode([0.1, c, 5])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-993")>]
let ``JYRO-REQ-993 Mode_ExactElementsAreTheSameValueOnlyWhenTheirExactValuesAreEqual`` () =
    // 0.1000000000000000000000000001 and 0.1 have one double but are two values:
    // counted as one, they would tie with {5, 5} and win by the lower first position.
    assertExact 5m (eval "Mode([0.1000000000000000000000000001, 0.1, 5, 5])")
    assertExact 5m (evalIn [ "h", hostInexact 7.0 ] "Mode([0.1000000000000000000000000001, 0.1, 5, 5, Input.h])")
    // Counted as one, 0.1 would have three and win.
    assertExact (dec "0.1000000000000000000000000001")
        (evalIn [ "h", hostInexact 7.0 ] "Mode([0.1, 0.1000000000000000000000000001, 0.1000000000000000000000000001, Input.h])")
    assertExact 2m (evalIn [ "h", hostInexact 7.0 ] "Mode([1, 1.0000000000000000000000000001, 2, 2, Input.h])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-994")>]
let ``Mode_IsPure_AndDoesNotChangeItsArgument`` () =
    assertCompilesPure "Mode([2, 1, 1, null])"
    assertTrue (evalScriptIn [] "var a = [2, 1, 1, null]\nvar m = Mode(a)\nOutput.r = a == [2, 1, 1, null] and Length(a) == 4")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-995")>]
let ``JYRO-REQ-995 Mode_OfABDAndDAB_ReturnsTheFirstOfTheEarliestGroup`` () =
    // The book's cases: [a, b, d] forms {a, d}, {b}; [d, a, b] forms {d, a}, {b}.
    isModeA (evalMode "[a, b, d]")
    isModeD (evalMode "[d, a, b]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-995")>]
let ``JYRO-REQ-995 Mode_EachNumberJoinsTheEarliestGroupItFullyEquals`` () =
    // [d, b, a]: {d, b}, {a} (a equals d but not b).
    // [a, b, d, b]: {a, d}, {b, b}: a tie, the earlier group wins.
    // [a, d, b, e]: {a, d, e}, {b}.
    // [b, d, a, a]: {b, d}, {a, a}: a tie, the earlier group wins.
    // [d, b, a, a]: {d, b}, {a, a}: a tie, the earlier group wins.
    // [d, a, b, b]: {d, a}, {b, b}: a tie, the earlier group wins.
    // [a, b, d, e, b, b]: {a, d, e}, {b, b, b}: a tie, the earlier group wins.
    // [a, a, d, b, b, b]: {a, a, d}, {b, b, b}: a tie, the earlier group wins.
    isModeD (evalMode "[d, b, a]")
    isModeA (evalMode "[a, b, d, b]")
    isModeA (evalMode "[a, d, b, e]")
    isModeB (evalMode "[b, d, a, a]")
    isModeD (evalMode "[d, b, a, a]")
    isModeD (evalMode "[d, a, b, b]")
    isModeA (evalMode "[a, b, d, e, b, b]")
    isModeA (evalMode "[a, a, d, b, b, b]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-995")>]
let ``JYRO-REQ-995 Mode_AGroupGrowsOnlyWithNumbersEqualToEveryMember`` () =
    // [d, a, b, b, b]: {d, a}, {b, b, b}: b equals d but not a, so the b's form their own group, which is larger.
    isModeB (evalMode "[d, a, b, b, b]")
    // [d, b, a, a, a]: {d, b}, {a, a, a}.
    isModeA (evalMode "[d, b, a, a, a]")

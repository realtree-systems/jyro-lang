/// jyro-spec 93.5 Min, 93.6 Max.
///
/// a = exact 1, b = exact 1.0000000000000000000000000001, c = inexact 1.
/// In script, c is `Power(1, 0.5)`; from the host, `Input.c` = JyroNumber(1.0 double).
module RealTree.Jyro.Spec.Tests.P93_94.S93_05_06_MinMax

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private bText = "1.0000000000000000000000000001"
let private bDec = dec bText

/// Evaluate `expr` with `a`, `b` and `c` declared (c from script or from the host).
let private evalAbc (hostC: bool) (expr: string) : JyroValue =
    let decl = if hostC then "var c = Input.c" else "var c = Power(1, 0.5)"
    let pairs = if hostC then [ "c", hostInexact 1.0 ] else []
    evalScriptIn pairs (sprintf "var a = 1\nvar b = %s\n%s\nOutput.r = %s" bText decl expr)

let private permutations =
    [ "[a, b, c]"; "[a, c, b]"; "[b, a, c]"; "[b, c, a]"; "[c, a, b]"; "[c, b, a]" ]

let private isA (v: JyroValue) = assertExact 1m v
let private isB (v: JyroValue) = assertExact bDec v
let private isC (v: JyroValue) = assertInexact 1.0 v

/// Signed zeros from the host: n = -0.0, p = +0.0 (both inexact).
let private zeros () = [ "n", hostInexact -0.0; "p", hostInexact 0.0 ]

// ================================================================= Min

[<Fact>]
[<Trait("requirement", "JYRO-REQ-904")>]
let ``Min_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Min()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Min([1], [2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-904")>]
let ``Min_ParameterIsNamedValues`` () =
    assertExact 1m (eval "Min(values: [3, 1])")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Min(items: [1])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-904")>]
let ``Min_ReturnsANumberOrNull`` () =
    assertString "number" (eval "TypeOf(Min([3, 1]))")
    assertString "null" (eval "TypeOf(Min([]))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-905")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``JYRO-REQ-905 Min_OfBCA_AndOfCA_ReturnsExactA`` (hostC: bool) =
    isA (evalAbc hostC "Min([b, c, a])")
    isA (evalAbc hostC "Min([c, a])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-905")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``JYRO-REQ-905 Min_OfEveryPermutationOfABC_ReturnsExactA`` (hostC: bool) =
    // The order is a, b, c (a and c have one double; an exact number comes first; b is above a by exact value).
    for p in permutations do isA (evalAbc hostC (sprintf "Min(%s)" p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-905")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``JYRO-REQ-905 Min_OfEqualDoubles_PutsExactBeforeInexact_AndOrdersExactByValue`` (hostC: bool) =
    isB (evalAbc hostC "Min([c, b])")
    isB (evalAbc hostC "Min([b, c])")
    isA (evalAbc hostC "Min([b, a])")
    isA (evalAbc hostC "Min([a, b])")
    isA (evalAbc hostC "Min([c, a])")
    isA (evalAbc hostC "Min([a, c])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-905")>]
let ``JYRO-REQ-905 Min_OfEqualDoubles_ExactProbesIdentifyTheResult`` () =
    assertTrue (evalAbc false "Min([b, c, a]) * 0.1 + 0.2 == 0.3")
    assertTrue (evalAbc false "Min([c, b]) - 1 == 0.0000000000000000000000000001")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-905")>]
[<InlineData("[3, 1, 2]", "1")>]
[<InlineData("[-1, -5]", "-5")>]
[<InlineData("[0.1, 0.01]", "0.01")>]
[<InlineData("[7]", "7")>]
[<InlineData("[0.1000000000000000000000000001, 0.1]", "0.1")>]
[<InlineData("[0.1, 0.1000000000000000000000000001]", "0.1")>]
[<InlineData("[-0.1, -0.1000000000000000000000000001]", "-0.1000000000000000000000000001")>]
[<InlineData("[null, 4, null, 2]", "2")>]
[<InlineData("[79228162514264337593543950335, -79228162514264337593543950335, 0]", "-79228162514264337593543950335")>]
let ``JYRO-REQ-905 Min_ReturnsTheLeastNumber`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Min(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-905")>]
let ``JYRO-REQ-905 Min_OfDifferentDoubles_TheDoubleValueDecides`` () =
    assertInexact (sqrt 2.0) (eval "Min([2, Power(2, 0.5)])")
    // The double nearest 0.3 is below 0.30000000000000004.
    assertExact 0.3m (evalIn [ "h", hostInexact 0.30000000000000004 ] "Min([Input.h, 0.3])")
    assertInexact 0.29999999999999993 (evalIn [ "h", hostInexact 0.29999999999999993 ] "Min([0.3, Input.h])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-905")>]
let ``JYRO-REQ-905 Min_OfAnInputArray_UsesTheSameOrder`` () =
    let a = Values.arr [ hostExact bDec; hostInexact 1.0; hostExact 1m ]
    assertExact 1m (evalIn [ "a", a ] "Min(Input.a)")
    let cb = Values.arr [ hostInexact 1.0; hostExact bDec ]
    assertExact bDec (evalIn [ "a", cb ] "Min(Input.a)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-906")>]
let ``Min_OfNull_ReturnsNull`` () =
    assertNull (eval "Min(null)")
    assertNull (eval "Min(values: null)")
    assertNull (evalIn [ "v", Values.nul ] "Min(Input.v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-907")>]
let ``Min_OfNonArray_RaisesArgumentTypeMismatch`` () =
    for lit in [ "5"; "\"abc\""; "\"\""; "true"; "{}"; "{a: 1}"; "f" ] do
        expectRuntimeError MessageCode.ArgumentTypeMismatch (withFn (sprintf "Min(%s)" lit))
    for v in [ Values.exact 5m; Values.str "abc"; Values.bool false; Values.obj [ "a", Values.exact 1m ] ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "Min(Input.v)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-908")>]
[<InlineData("[3, 1.5, 2]", "1.5")>]
[<InlineData("[0.1, 0.2]", "0.1")>]
[<InlineData("[79228162514264337593543950335, -79228162514264337593543950335]", "-79228162514264337593543950335")>]
let ``Min_OfExactNumbers_IsExact`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Min(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-908")>]
let ``Min_OfExactNumbers_KeepsExactArithmetic`` () =
    assertTrue (eval "Min([0.1, 0.5]) + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-909")>]
let ``JYRO-REQ-909 Min_OfMixed_ReturnsTheSelectedElementsOwnRepresentation`` () =
    assertTrue (eval "Min([0.1, Power(2, 0.5)]) + 0.2 == 0.3")
    assertExact 0.1m (eval "Min([0.1, Power(2, 0.5)])")
    assertExact 0.1m (evalIn [ "h", hostInexact 0.5 ] "Min([Input.h, 0.1])")
    assertInexact 0.05 (evalIn [ "h", hostInexact 0.05 ] "Min([0.1, Input.h])")
    assertInexact (sqrt 2.0) (eval "Min([3, Power(2, 0.5), 2])")
    assertExact 1m (evalIn [ "h", hostInexact 3.0 ] "Min([2, Input.h, 1, null])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-909")>]
let ``JYRO-REQ-909 Min_OfAMixedInputArray_ReturnsTheSelectedElementsOwnRepresentation`` () =
    let a = Values.arr [ hostInexact 0.5; hostExact 0.1m; hostInexact 0.7 ]
    assertExact 0.1m (evalIn [ "a", a ] "Min(Input.a)")
    assertTrue (evalIn [ "a", a ] "Min(Input.a) + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-910")>]
let ``JYRO-REQ-910 Min_OfTiedInexactZeros_ReturnsTheOneAtTheLowestPosition`` () =
    let r1 = evalIn (zeros ()) "Min([Input.n, Input.p, 1])"
    assertInexact 0.0 r1
    assertSignBit true r1
    let r2 = evalIn (zeros ()) "Min([Input.p, Input.n, 1])"
    assertInexact 0.0 r2
    assertSignBit false r2
    let r3 = evalIn (zeros ()) "Min([2, Input.p, Input.n])"
    assertInexact 0.0 r3
    assertSignBit false r3

[<Fact>]
[<Trait("requirement", "JYRO-REQ-910")>]
let ``JYRO-REQ-910 Min_AnExactNumberNeverTiesAnInexactOne`` () =
    // Not a tie in the order of 92.11.6: the exact 1 comes before the inexact 1 wherever it stands.
    assertExact 1m (evalIn [ "h", hostInexact 1.0 ] "Min([Input.h, 1, 2])")
    assertExact 1m (evalIn [ "h", hostInexact 1.0 ] "Min([2, Input.h, 1])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-910")>]
let ``JYRO-REQ-910 Min_OfTiedInexactZerosMadeInScript_ReturnsTheOneAtTheLowestPosition`` () =
    // 0 * -Power(1, 0.5) is an inexact -0, and 0 * Power(1, 0.5) an inexact +0 (IEEE 754 multiplication).
    let src first second = sprintf "var n = 0 * -Power(1, 0.5)\nvar p = 0 * Power(1, 0.5)\nOutput.r = Min([%s, %s, 1])" first second
    let r1 = evalScriptIn [] (src "n" "p")
    assertInexact 0.0 r1
    assertSignBit true r1
    let r2 = evalScriptIn [] (src "p" "n")
    assertInexact 0.0 r2
    assertSignBit false r2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-910")>]
let ``JYRO-REQ-910 Min_OfTiedExactNumbers_ReturnsTheValue`` () =
    assertExact 1m (eval "Min([2, 1, 1])")
    assertExact 1m (eval "Min([1, 1.0])")
    assertExact 1m (eval "Min([1.00, 3, 1])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-911")>]
let ``Min_IsPure`` () =
    assertCompilesPure "Min([3, 1])"
    assertPure "Min([3, 1])" 1.0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-911")>]
let ``Min_DoesNotChangeItsArgument`` () =
    assertTrue (evalScriptIn [] "var a = [3, 1, null, 2]\nvar m = Min(a)\nOutput.r = a == [3, 1, null, 2] and Length(a) == 4")

// ================================================================= Max

[<Fact>]
[<Trait("requirement", "JYRO-REQ-912")>]
let ``Max_Arity_IsOneArgument`` () =
    expectCompileError MessageCode.TooFewArguments "Output.r = Max()"
    expectCompileError MessageCode.TooManyArguments "Output.r = Max([1], [2])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-912")>]
let ``Max_ParameterIsNamedValues`` () =
    assertExact 3m (eval "Max(values: [3, 1])")
    expectCompileErrorAmong MessageCode.UnknownNamedArgument "Output.r = Max(items: [1])"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-912")>]
let ``Max_ReturnsANumberOrNull`` () =
    assertString "number" (eval "TypeOf(Max([3, 1]))")
    assertString "null" (eval "TypeOf(Max([]))")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-913")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``Max_OfACB_AndOfCB_ReturnsInexactC`` (hostC: bool) =
    isC (evalAbc hostC "Max([a, c, b])")
    isC (evalAbc hostC "Max([c, b])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-913")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``Max_OfEveryPermutationOfABC_ReturnsInexactC`` (hostC: bool) =
    for p in permutations do isC (evalAbc hostC (sprintf "Max(%s)" p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-913")>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``Max_OfEqualDoubles_PutsInexactAfterExact_AndOrdersExactByValue`` (hostC: bool) =
    isC (evalAbc hostC "Max([c, a])")
    isC (evalAbc hostC "Max([a, c])")
    isB (evalAbc hostC "Max([a, b])")
    isB (evalAbc hostC "Max([b, a])")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-913")>]
[<InlineData("[3, 1, 2]", "3")>]
[<InlineData("[-1, -5]", "-1")>]
[<InlineData("[0.1, 0.01]", "0.1")>]
[<InlineData("[7]", "7")>]
[<InlineData("[0.1000000000000000000000000001, 0.1]", "0.1000000000000000000000000001")>]
[<InlineData("[0.1, 0.1000000000000000000000000001]", "0.1000000000000000000000000001")>]
[<InlineData("[null, 4, null, 2]", "4")>]
let ``Max_ReturnsTheGreatestNumber`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Max(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-913")>]
let ``Max_OfDifferentDoubles_TheDoubleValueDecides`` () =
    assertInexact (sqrt 2.0) (eval "Max([1, Power(2, 0.5)])")
    assertInexact 0.30000000000000004 (evalIn [ "h", hostInexact 0.30000000000000004 ] "Max([Input.h, 0.3])")
    assertInexact 0.30000000000000004 (evalIn [ "h", hostInexact 0.30000000000000004 ] "Max([0.3, Input.h])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-913")>]
let ``Max_OfNoNumbers_ReturnsNull`` () =
    assertNull (eval "Max([])")
    assertNull (eval "Max([null])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-914")>]
let ``Max_OfNull_ReturnsNull`` () =
    assertNull (eval "Max(null)")
    assertNull (eval "Max(values: null)")
    assertNull (evalIn [ "v", Values.nul ] "Max(Input.v)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-915")>]
let ``Max_OfNonArray_RaisesArgumentTypeMismatch`` () =
    for lit in [ "5"; "\"abc\""; "\"\""; "true"; "{}"; "{a: 1}"; "f" ] do
        expectRuntimeError MessageCode.ArgumentTypeMismatch (withFn (sprintf "Max(%s)" lit))
    for v in [ Values.exact 5m; Values.str "abc"; Values.bool false; Values.obj [ "a", Values.exact 1m ] ] do
        expectExprErrorIn [ "v", v ] MessageCode.ArgumentTypeMismatch "Max(Input.v)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-916")>]
[<InlineData("[3, 1.5, 2]", "3")>]
[<InlineData("[0.1, 0.05]", "0.1")>]
[<InlineData("[79228162514264337593543950335, 0]", "79228162514264337593543950335")>]
let ``Max_OfExactNumbers_IsExact`` (values: string, expected: string) =
    assertExact (dec expected) (eval (sprintf "Max(%s)" values))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-916")>]
let ``Max_OfExactNumbers_KeepsExactArithmetic`` () =
    assertTrue (eval "Max([0.1, -0.5]) + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-917")>]
let ``JYRO-REQ-917 Max_OfMixed_ReturnsTheSelectedElementsOwnRepresentation`` () =
    assertTrue (eval "Max([0.1, -Power(2, 0.5)]) + 0.2 == 0.3")
    assertExact 0.1m (eval "Max([0.1, -Power(2, 0.5)])")
    assertExact 0.1m (evalIn [ "h", hostInexact 0.05 ] "Max([Input.h, 0.1])")
    assertInexact 0.5 (evalIn [ "h", hostInexact 0.5 ] "Max([0.1, Input.h])")
    assertInexact (sqrt 2.0) (eval "Max([1, Power(2, 0.5), -3])")
    assertExact 3m (evalIn [ "h", hostInexact 1.0 ] "Max([2, Input.h, 3, null])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-917")>]
let ``JYRO-REQ-917 Max_OfAMixedInputArray_ReturnsTheSelectedElementsOwnRepresentation`` () =
    let a = Values.arr [ hostInexact 0.05; hostExact 0.1m; hostInexact 0.07 ]
    assertExact 0.1m (evalIn [ "a", a ] "Max(Input.a)")
    assertTrue (evalIn [ "a", a ] "Max(Input.a) + 0.2 == 0.3")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-918")>]
let ``Max_OfTiedInexactZeros_ReturnsTheOneAtTheLowestPosition`` () =
    let r1 = evalIn (zeros ()) "Max([Input.n, Input.p, -1])"
    assertInexact 0.0 r1
    assertSignBit true r1
    let r2 = evalIn (zeros ()) "Max([Input.p, Input.n, -1])"
    assertInexact 0.0 r2
    assertSignBit false r2
    let r3 = evalIn (zeros ()) "Max([-1, Input.n, Input.p])"
    assertInexact 0.0 r3
    assertSignBit true r3

[<Fact>]
[<Trait("requirement", "JYRO-REQ-918")>]
let ``Max_AnExactNumberNeverTiesAnInexactOne`` () =
    assertInexact 3.0 (evalIn [ "h", hostInexact 3.0 ] "Max([3, Input.h, 1])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-918")>]
let ``Max_OfTiedExactNumbers_ReturnsTheValue`` () =
    assertExact 3m (eval "Max([1, 3, 3])")
    assertExact 3m (eval "Max([3, 3.0])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-919")>]
let ``Max_IsPure`` () =
    assertCompilesPure "Max([3, 1])"
    assertPure "Max([3, 1])" 3.0

[<Fact>]
[<Trait("requirement", "JYRO-REQ-919")>]
let ``Max_DoesNotChangeItsArgument`` () =
    assertTrue (evalScriptIn [] "var a = [3, 1, null, 2]\nvar m = Max(a)\nOutput.r = a == [3, 1, null, 2] and Length(a) == 4")

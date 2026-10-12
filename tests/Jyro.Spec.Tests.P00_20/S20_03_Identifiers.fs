/// 20.3 - identifiers: the characters of a name, and case significance.
module RealTree.Jyro.Spec.Tests.S20_03_Identifiers

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 20.3.1 ------------------------------------------------------------------

/// Names by label, so theory rows stay plain ASCII.
let private acceptedNames =
    dict [ "underscore", "_"
           "underscore_x1", "_x1"
           "x_1", "x_1"
           "abc", "abc"
           "underscore_9", "_9"
           "Ll_e_acute", cp 0x00E9
           "Lu_omega_mega", cp 0x03A9 + "mega"
           "Lo_cjk", cp 0x65E5 + cp 0x672C
           "Lm_modifier_h", cp 0x02B0 + "x"
           "Lt_titlecase_dz", cp 0x01C5 + "x"
           "Nd_arabic_indic_continuation", "x" + cp 0x0661 + cp 0x0662
           "Nd_nko_continuation", "x" + cp 0x07C0
           "L_outside_bmp", cp 0x1D49C + "b" ]

let private rejectedNames =
    dict [ "ascii_digit_first", "1x"
           "nd_digit_first", cp 0x0661 + "x"
           "dollar_first", "$x"
           "dollar_last", "x$"
           "Mn_combining_acute", "e" + cp 0x0301
           "Nl_roman_numeral", "x" + cp 0x216B
           "Pc_undertie", "x" + cp 0x203F + "y"
           "emoji", "x" + cp 0x1F600
           "Cf_zero_width_joiner", "x" + cp 0x200D + "y" ]

let acceptedLabels : seq<obj[]> = rowsOf acceptedNames.Keys
let rejectedLabels : seq<obj[]> = rowsOf rejectedNames.Keys

/// The outcome of a rejected name, as the book gives it: a name starting with
/// an ASCII digit is a number token that does not match number_literal, so
/// InvalidNumberFormat (20.6.6); every other row is outside the grammar
/// (20.1.1, 20.3.1). Either way a parse error: every error of stage Parse, and
/// nothing runs (130.3.1, 160.1.12). Other parse errors may follow the first.
let private rejectedOutcome (label: string) (source: string) : unit =
    let r = parseErrorL source
    if label = "ascii_digit_first" then assertHasError MessageCode.InvalidNumberFormat r

[<Theory>]
[<MemberData(nameof acceptedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_LetterOrUnderscoreThenLettersDigitsUnderscores_IsAcceptedAsVariable`` (label: string) =
    let n = acceptedNames.[label]
    outVL (sprintf "var %s = 41\nOutput.v = %s + 1" n n) |> assertJson "42"

[<Theory>]
[<MemberData(nameof acceptedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_Accepted_AsFunctionName`` (label: string) =
    let n = acceptedNames.[label]
    outVL (sprintf "func %s() return 42 end\nOutput.v = %s()" n n) |> assertJson "42"

[<Theory>]
[<MemberData(nameof acceptedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_Accepted_AsParameter`` (label: string) =
    let n = acceptedNames.[label]
    outVL (sprintf "func g(%s) return %s end\nOutput.v = g(42)" n n) |> assertJson "42"

[<Theory>]
[<MemberData(nameof acceptedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_Accepted_AsLambdaParameter`` (label: string) =
    let n = acceptedNames.[label]
    outVL (sprintf "Output.v = Map([42], %s => %s)" n n) |> assertJson "[42]"

[<Theory>]
[<MemberData(nameof acceptedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_Accepted_AsLoopVariable`` (label: string) =
    let n = acceptedNames.[label]
    outVL (sprintf "foreach %s in [42] do Output.v = %s end" n n) |> assertJson "42"

[<Theory>]
[<MemberData(nameof rejectedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_NotStartingWithLetterOrUnderscore_OrWithOtherCharacters_IsRejectedAsVariable`` (label: string) =
    let n = rejectedNames.[label]
    rejectedOutcome label (sprintf "var %s = 1\nOutput.v = %s" n n)

[<Theory>]
[<MemberData(nameof rejectedLabels)>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_Rejected_AsFunctionName`` (label: string) =
    let n = rejectedNames.[label]
    rejectedOutcome label (sprintf "func %s() return 1 end\nOutput.v = %s()" n n)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-22")>]
let ``JYRO-REQ-22 Identifier_CategoriesAreThoseOfUnicode15_0`` () =
    // U+1E030 (Lm) and U+11F04 (Lo), new in 15.0, start a name.
    for c in [ cp 0x1E030; cp 0x11F04 ] do
        outVL (sprintf "var %sx = 1\nOutput.v = %sx" c c) |> assertJson "1"
        outVL (sprintf "func %s() return 2 end\nOutput.v = %s()" c c) |> assertJson "2"
    // U+11F50 (Nd), new in 15.0, continues one.
    let d = cp 0x11F50
    outVL (sprintf "var a%s = 3\nOutput.v = a%s" d d) |> assertJson "3"
    // U+2EBF0 (Lo only from 15.1) and U+1C89 (Lu only from 16.0) are not letters.
    for c in [ cp 0x2EBF0; cp 0x1C89 ] do
        parseErrorL (sprintf "var %s = 1\nOutput.v = 1" c) |> ignore
        parseErrorL (sprintf "var a%s = 1\nOutput.v = 1" c) |> ignore

// -- 20.3.2 ------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_VariablesDifferingOnlyInCase_AreDistinct`` () =
    Script.outputOf "var x = 1\nvar X = 2\nOutput.a = x\nOutput.b = X" |> assertJson "{\"a\":1,\"b\":2}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_TwoDeclarationsOnOneLine_DifferingInCase_DeclareTwoVariables`` () =
    Script.outputOf "var a = 1  var A = 2\nOutput.a = a\nOutput.b = A" |> assertJson "{\"a\":1,\"b\":2}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_VariableReferenceInOtherCase_IsUndeclaredVariable`` () =
    rejectedWithCode MessageCode.UndeclaredVariable "var abc = 1\nOutput.v = ABC"
    rejectedWithCode MessageCode.UndeclaredVariable "var x = 1\nOutput.v = X"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1816")>]
[<InlineData(0x212A, 0x004B)>] // U+212A KELVIN SIGN and K
[<InlineData(0xFF21, 0x0041)>] // U+FF21 FULLWIDTH A and A
[<InlineData(0x00C5, 0x212B)>] // U+00C5 and U+212B ANGSTROM SIGN, canonically equivalent
[<InlineData(0x00B5, 0x03BC)>] // U+00B5 MICRO SIGN and U+03BC GREEK SMALL LETTER MU
let ``JYRO-REQ-1816 Identifier_CompatibilityOrCanonicalEquivalent_IsAnotherName`` (first: int, second: int) =
    let a = cp first
    let b = cp second
    Script.outputOf (sprintf "var %s = 1\nvar %s = 2\nOutput.a = %s\nOutput.b = %s" a b a b) |> assertJson "{\"a\":1,\"b\":2}"
    rejectedWithCode MessageCode.UndeclaredVariable (sprintf "var %s = 1\nOutput.v = %s" a b)
    rejectedWithCode MessageCode.UndeclaredVariable (sprintf "var %s = 1\nOutput.v = %s" b a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_PrecomposedAndDecomposedHangul_AreDifferentNames`` () =
    let composed = cp 0xAC00
    let decomposed = cp 0x1100 + cp 0x1161
    Script.outputOf (sprintf "var %s = 1\nvar %s = 2\nOutput.a = %s\nOutput.b = %s" composed decomposed composed decomposed)
    |> assertJson "{\"a\":1,\"b\":2}"
    rejectedWithCode MessageCode.UndeclaredVariable (sprintf "var %s = 1\nOutput.v = %s" composed decomposed)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_FunctionCallInOtherCase_IsUndefinedFunction`` () =
    rejectedWithCode MessageCode.UndefinedFunction "func f() return 1 end\nOutput.v = F()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_FunctionsDifferingOnlyInCase_AreDistinct`` () =
    Script.outputOf "func g() return 1 end\nfunc G() return 2 end\nOutput.a = g()\nOutput.b = G()"
    |> assertJson "{\"a\":1,\"b\":2}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_StdlibFunctionInOtherCase_IsUndefinedFunction`` () =
    outV "Output.v = ToUpper(\"a\")" |> assertJson "\"A\""
    rejectedWithCode MessageCode.UndefinedFunction "Output.v = toupper(\"a\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_ParameterReferenceInOtherCase_IsUndeclaredVariable`` () =
    rejectedWithCode MessageCode.UndeclaredVariable "func h(value) return VALUE end\nOutput.v = h(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_NamedArgumentLabelInOtherCase_IsUnknownNamedArgument`` () =
    rejectedWithCode MessageCode.UnknownNamedArgument "func h(value = 0) return value end\nOutput.v = h(Value: 1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1816")>]
let ``JYRO-REQ-1816 Identifier_VariantsDifferingOnlyInCase_AreDistinct`` () =
    Script.outputOf "union U V end\nunion u v end\nOutput.a = V()\nOutput.b = v()"
    |> assertJson "{\"a\":{\"_variant\":\"V\"},\"b\":{\"_variant\":\"v\"}}"

/// 30.12 Type coercion at typed assignment.
module RealTree.Jyro.Spec.Tests.P30.S30_12_Coercion

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests
open RealTree.Jyro.Spec.Tests.P30.Support

/// A value of each hint's own type, to seed a hinted variable before a later assignment.
let private seedFor (hint: string) =
    match hint with
    | "number" -> "0"
    | "string" -> "\"\""
    | "boolean" -> "false"
    | "array" -> "[]"
    | "object" -> "{}"
    | "function" -> "(y => y)"
    | _ -> "null"

/// Every coercion site of 30.12: each gives a script fragment that coerces
/// `value` under `hint` and leaves the result in the top-level variable `x`.
let private sites : (string * (string -> string -> string)) list =
    [ "var (50.1.5)", (fun hint value -> "var x: " + hint + " = " + value + "\n")
      "later assignment (50.1.6)", (fun hint value -> "var x: " + hint + " = " + seedFor hint + "\nx = " + value + "\n")
      "property (50.2.2)", (fun hint value -> "var o = {}\no.p: " + hint + " = " + value + "\nvar x = o.p\n")
      "element (50.2.2)", (fun hint value -> "var e = [null]\ne[0]: " + hint + " = " + value + "\nvar x = e[0]\n")
      "parameter (80.6.1)", (fun hint value -> "func f(p: " + hint + ")\n return p\nend\nvar x = f(" + value + ")\n")
      "variant field (50.10.8)", (fun hint value -> "union U W(p: " + hint + ") end\nvar x = W(" + value + ").p\n") ]

let siteNames () : obj[] seq = sites |> Seq.map (fun (n, _) -> [| box n |])

/// The site whose coercion is another work order's rule: whether a variant
/// field coerces its argument at all is 50.10.8 (JYRO-REQ-245, JYRO-WRK-5).
/// The conversions of 30.12.2 to 30.12.9 (JYRO-WRK-3) are judged at the other
/// sites, so a variant field that does not coerce cannot fail them.
let private variantSite = "variant field (50.10.8)"

/// The sites at which the conversions of 30.12.2 to 30.12.9 are judged.
let private conversionSites = sites |> List.filter (fun (n, _) -> n <> variantSite)

let conversionSiteNames () : obj[] seq = conversionSites |> Seq.map (fun (n, _) -> [| box n |])

let private site name = sites |> List.find (fun (n, _) -> n = name) |> snd

/// Run a site under `hint` with `value`, then `after` (which reads `x`).
let private runSite (siteName: string) (hint: string) (value: string) (after: string) =
    Script.run (site siteName hint value + after)

let private crossSites (cases: (string * 'a) list) : obj[] seq =
    seq {
        for (s, _) in sites do
            for (v, e) in cases do
                yield [| box s; box v; box e |]
    }

// ---------------------------------------------------------------- 30.12.1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-121")>]
[<MemberData(nameof siteNames)>]
let ``A number that already has the hinted type is assigned unchanged`` (siteName: string) =
    let exact = runSite siteName "number" "0.1" "Output.x = x\nOutput.sum = x + 0.2 == 0.3" |> Script.output
    assertExact 0.1M (Values.prop "x" exact)
    Assert.True(Values.asBool (Values.prop "sum" exact))
    let inexact = runSite siteName "number" "Power(2, 0.5)" "Output.x = x" |> Script.output
    assertInexact (sqrt 2.0) (Values.prop "x" inexact)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-121")>]
[<MemberData(nameof siteNames)>]
let ``A string, a boolean and null that already have the hinted type are assigned unchanged`` (siteName: string) =
    Assert.Equal("\"s\"", runSite siteName "string" "\"s\"" "Output.x = x" |> Script.output |> Values.prop "x" |> Values.toJson)
    Assert.Equal("true", runSite siteName "boolean" "true" "Output.x = x" |> Script.output |> Values.prop "x" |> Values.toJson)
    Assert.Equal("false", runSite siteName "boolean" "false" "Output.x = x" |> Script.output |> Values.prop "x" |> Values.toJson)
    Assert.Equal("[null]", runSite siteName "null" "null" "Output.x = [x]" |> Script.output |> Values.prop "x" |> Values.toJson)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-121")>]
[<MemberData(nameof siteNames)>]
let ``A container that already has the hinted type is assigned as the same container`` (siteName: string) =
    let arr = Script.run ("var orig = [1]\n" + site siteName "array" "orig" + "orig[0] = 9\nOutput.r = x[0]") |> Script.output
    assertNum 9M (Values.prop "r" arr)
    let obj = Script.run ("var orig = {k: 1}\n" + site siteName "object" "orig" + "orig.k = 9\nOutput.r = x.k") |> Script.output
    assertNum 9M (Values.prop "r" obj)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-121")>]
[<MemberData(nameof siteNames)>]
let ``A function value under the hint function is assigned as the same value`` (siteName: string) =
    let out = Script.run ("var orig = y => y + 1\n" + site siteName "function" "orig" + "Output.r = [x == orig, Map([1], x)[0]]") |> Script.output
    Assert.Equal("[true,2]", Values.toJson (Values.prop "r" out))

let anyValues () : obj[] seq =
    seq {
        for (s, _) in sites do
            for v in [ "null"; "1"; "0.1"; "\"s\""; "true"; "[1]"; "{a: 1}"; "orig" ] do
                yield [| box s; box v |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-121")>]
[<MemberData(nameof anyValues)>]
let ``Under the hint any every value is assigned unchanged`` (siteName: string, value: string) =
    let source = "var orig = y => y\n" + site siteName "any" value + "Output.r = [x == " + value + ", TypeOf(x) == TypeOf(" + value + ")]\nOutput.x = x"
    let out = Script.run source |> Script.output
    Assert.Equal("[true,true]", Values.toJson (Values.prop "r" out))
    if value = "0.1" then assertExact 0.1M (Values.prop "x" out)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-121")>]
[<MemberData(nameof siteNames)>]
let ``Under the hint any a container is the same container`` (siteName: string) =
    let out = Script.run ("var orig = [1]\n" + site siteName "any" "orig" + "orig[0] = 7\nOutput.r = x[0]") |> Script.output
    assertNum 7M (Values.prop "r" out)

// ---------------------------------------------------------------- 30.12.2

let private acceptedNumbers =
    [ "\"0.1\"", "0.1", true
      "\"-0.1\"", "-0.1", true
      "\"1E2\"", "100", true
      "\"0\"", "0", true
      "\"-0\"", "0", true
      "\"79228162514264337593543950335\"", "79228162514264337593543950335", true
      "\"1e29\"", "1e29", false
      "\"0.12345678901234567890123456789\"", "0.12345678901234567890123456789", false
      "\"1e-29\"", "1e-29", false
      "\" 5\"", "5", true
      "\"5 \"", "5", true
      "\"\\t5\"", "5", true
      "\"5\\t\"", "5", true
      "\"\\n5\\n\"", "5", true
      "\"\\r5\\r\"", "5", true
      "\" \\t\\r\\n5\\r\\n\"", "5", true
      "\"\\u00205\\u0009\"", "5", true ]

let acceptedData () : obj[] seq =
    seq {
        for (s, _) in conversionSites do
            for (text, value, exact) in acceptedNumbers do
                yield [| box s; box text; box value; box exact |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-122")>]
[<MemberData(nameof acceptedData)>]
let ``JYRO-REQ-122 A string matching the JSON number grammar after JSON white space is trimmed coerces to its value`` (siteName: string, text: string, value: string, exact: bool) =
    let out = runSite siteName "number" text "Output.x = x" |> Script.output |> Values.prop "x"
    if exact then assertExact (dec value) out else assertInexact (dbl value) out

[<Theory>]
[<Trait("requirement", "JYRO-REQ-122")>]
[<MemberData(nameof conversionSiteNames)>]
let ``JYRO-REQ-122 A string coerced to number is exact as JSON gives, so 0.1 subtracts exactly`` (siteName: string) =
    let out = runSite siteName "number" "\"0.1\"" "Output.r = 0.3 - x == 0.2" |> Script.output
    Assert.True(Values.asBool (Values.prop "r" out))

let private otherSpaces = [ "\\u00A0"; "\\u2028"; "\\u2029"; "\\u000B"; "\\u000C"; "\\u0085"; "\\u3000"; "\\uFEFF"; "\\u2003" ]

let otherSpaceData () : obj[] seq =
    seq {
        for (s, _) in conversionSites do
            for sp in otherSpaces do
                yield [| box s; box ("\"" + sp + "5\"") |]
                yield [| box s; box ("\"5" + sp + "\"") |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-122")>]
[<MemberData(nameof otherSpaceData)>]
let ``JYRO-REQ-122 Only the four JSON white-space characters are trimmed`` (siteName: string, text: string) =
    Script.expectError MessageCode.InvalidType (runSite siteName "number" text "Output.x = x")

/// Strings far longer than 768 significant digits, coerced to number: exact
/// when the value is m / 10^s, else the double nearest the whole value, as
/// 30.2.7 gives for the same text (JYRO-ADR-151 (1), (2), (5)(b)). The string
/// comes from Input, so the source stays short.
let longTextData () : obj[] seq =
    seq {
        for (s, _) in conversionSites do
            for (name, _, _) in P30.S30_02_Representation.longNumbers do
                yield [| box s; box name |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-122")>]
[<MemberData(nameof longTextData)>]
let ``JYRO-REQ-122 A long numeric string coerces as JSON text of the same number reads`` (siteName: string, name: string) =
    let (_, text, expected) = P30.S30_02_Representation.longNumbers |> List.find (fun (n, _, _) -> n = name)
    let r =
        Script.runWith { limited with Input = ValueInput (Values.obj [ "s", Values.str (" " + text + "\n") ]) }
            (site siteName "number" "Input.s" + "Output.x = x")
    let x = Script.output r |> Values.prop "x"
    match expected with
    | Choice1Of2 d -> assertExact d x
    | Choice2Of2 f -> assertInexact f x

// ---------------------------------------------------------------- 30.12.3

let private rejectedNumbers =
    [ "\"\""; "\" \""; "\"abc\""; "\"+1\""; "\"01\""; "\".5\""; "\"5.\""; "\"0x10\""; "\"1,234\""; "\"1_000\""
      "\"1e\""; "\"--1\""; "\"1 2\""; "\"NaN\""; "\"Infinity\""; "\"-Infinity\""; "\"1e400\""; "\"-1e400\""
      "\"1.5e308x\""; "\"\\u00A05\""; "\"5\\u3000\""; "\"\\uFEFF5\"" ]

let rejectedData () : obj[] seq =
    seq {
        for (s, _) in conversionSites do
            for text in rejectedNumbers do
                yield [| box s; box text |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-123")>]
[<MemberData(nameof rejectedData)>]
let ``JYRO-REQ-123 A string that is not a finite JSON number raises InvalidType when coerced to number`` (siteName: string, text: string) =
    Script.expectError MessageCode.InvalidType (runSite siteName "number" text "Output.x = x")

// ---------------------------------------------------------------- 30.12.4

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1728")>]
[<MemberData(nameof conversionSiteNames)>]
let ``JYRO-REQ-1728 A boolean coerced to number gives 0 for false and 1 for true`` (siteName: string) =
    let f = runSite siteName "number" "false" "Output.x = x\nOutput.t = TypeOf(x)\nOutput.y = x + 1" |> Script.output
    assertNum 0M (Values.prop "x" f)
    Assert.Equal("number", Values.asString (Values.prop "t" f))
    assertNum 1M (Values.prop "y" f)
    let t = runSite siteName "number" "true" "Output.x = x\nOutput.t = TypeOf(x)" |> Script.output
    assertNum 1M (Values.prop "x" t)
    Assert.Equal("number", Values.asString (Values.prop "t" t))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1728")>]
[<MemberData(nameof conversionSiteNames)>]
let ``JYRO-REQ-1728 A boolean coerced to number is the exact 0 or 1`` (siteName: string) =
    // The exact 1 keeps exact arithmetic exact: 1 + 0.1 + 0.2 is the exact 1.3, and 0 + 0.1 + 0.2 the
    // exact 0.3, as with the literals 1 and 0 (JYRO-REQ-49). An inexact 0 or 1 makes both false.
    let t = runSite siteName "number" "true" "Output.a = x + 0.1 + 0.2 == 1.3\nOutput.b = x + 0.1 + 0.2" |> Script.output
    Assert.True(Values.asBool (Values.prop "a" t), "true coerced to number: t + 0.1 + 0.2 == 1.3 is false")
    let f = runSite siteName "number" "false" "Output.a = x + 0.1 + 0.2 == 0.3\nOutput.b = x + 0.1 + 0.2" |> Script.output
    Assert.True(Values.asBool (Values.prop "a" f), "false coerced to number: f + 0.1 + 0.2 == 0.3 is false")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1728")>]
let ``JYRO-REQ-1728 The clause's examples, var t and var f with a number hint, give the exact 1 and 0`` () =
    let out =
        Script.run "var t: number = true\nvar f: number = false\nOutput.a = t + 0.1 + 0.2 == 1.3\nOutput.b = f + 0.1 + 0.2 == 0.3\nOutput.c = t == 1\nOutput.d = f == 0"
        |> Script.output
    for key in [ "a"; "b"; "c"; "d" ] do
        Assert.True(Values.asBool (Values.prop key out), sprintf "Output.%s is not true" key)

// ---------------------------------------------------------------- 30.12.5

/// Numbers and booleans whose string forms differ in kind: exact with trailing
/// zeros, negative zero, large and small, inexact with and without exponent.
/// What each string form is (40.4.6, 40.4.7) is Part 40's rule (JYRO-REQ-150,
/// JYRO-REQ-151, JYRO-WRK-4); 30.12.5 binds only that the coercion gives the
/// same text as `&`, so the test compares with `&` and with no fixed text.
let private stringForms =
    [ "0.1"; "1.50"; "-0.5"; "0"; "-0"; "1e21"; "79228162514264337593543950335"
      "Power(2, 0.5)"; "1e21 * Power(1, 0.5)"; "0.1 * Power(1, 0.5) + 0.2"; "Power(2, -1074 * Power(1, 0.5))"
      "1e-7 * Power(1, 0.5)"; "-0 * Power(1, 0.5)"; "true"; "false" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-125")>]
[<MemberData(nameof conversionSiteNames)>]
let ``JYRO-REQ-125 A number or a boolean coerced to string gives its string form as & gives it`` (siteName: string) =
    for value in stringForms do
        let r = runSite siteName "string" value ("Output.x = x\nOutput.amp = (" + value + ") & \"\"")
        if not (Script.ok r) then Values.fail (sprintf "%s: the run failed. Diagnostics:\n%s" value (Script.describe r))
        let out = Script.output r
        let xv = Values.prop "x" out
        if xv.ValueType <> JyroValueType.String then
            Values.fail (sprintf "%s: coerced to string gave a %A, not a string" value xv.ValueType)
        let x = Values.asString xv
        let amp = Values.asString (Values.prop "amp" out)
        Assert.True((x = amp), sprintf "%s: coerced %A, & gives %A" value x amp)

// ---------------------------------------------------------------- 30.12.6

let private numberTruth =
    [ "0", false; "-0", false; "0.0", false; "0 * Power(2, 0.5)", false; "0 * -Power(2, 0.5)", false
      "1", true; "-1", true; "0.5", true; "1e-28", true; "Power(2, -1074 * Power(1, 0.5))", true
      "79228162514264337593543950335", true; "1e300", true ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-126")>]
[<MemberData(nameof conversionSiteNames)>]
let ``JYRO-REQ-126 A number coerced to boolean gives the truthiness conversion`` (siteName: string) =
    for (value, truth) in numberTruth do
        // The conversion of 30.9.2 is read through `? :`, a boolean context of
        // 30.9.1, not ToBoolean (JYRO-REQ-1092, another work order).
        let out = runSite siteName "boolean" value ("Output.x = x\nOutput.b = (" + value + ") ? true : false") |> Script.output
        let x = Values.asBool (Values.prop "x" out)
        Assert.True((x = truth), sprintf "%s coerced to boolean gave %b" value x)
        Assert.Equal(Values.asBool (Values.prop "b" out), x)

// ---------------------------------------------------------------- 30.12.7 and 30.12.8

/// Every site at which a string is coerced under the hint `boolean` (4.1.8).
/// Each takes the Jyro expression of the string and leaves the result in the
/// top-level variable `x`. The last field is false for a site whose value must
/// be a literal (a default, 80.4.5), which a host string cannot reach.
let private boolSites : (string * (string -> string) * bool) list =
    [ "var (50.1.5)", (fun v -> "var x: boolean = " + v + "\n"), true
      "later assignment (50.1.6)", (fun v -> "var x: boolean = false\nx = " + v + "\n"), true
      "property (50.2.2)", (fun v -> "var o = {}\no.p: boolean = " + v + "\nvar x = o.p\n"), true
      "element (50.2.2)", (fun v -> "var e = [null]\ne[0]: boolean = " + v + "\nvar x = e[0]\n"), true
      "positional argument (80.6.1)", (fun v -> "func f(p: boolean)\n return p\nend\nvar x = f(" + v + ")\n"), true
      "named argument (80.6.1)", (fun v -> "func f(q, p: boolean)\n return p\nend\nvar x = f(q: 0, p: " + v + ")\n"), true
      "default, no argument (80.5.1, 80.6.1)", (fun v -> "func f(p: boolean = " + v + ")\n return p\nend\nvar x = f()\n"), false
      "default, null argument (80.5.2, 80.6.1)", (fun v -> "func f(p: boolean = " + v + ")\n return p\nend\nvar x = f(null)\n"), false
      "parameter reassigned (80.6.2)", (fun v -> "func f(p: boolean)\n p = " + v + "\n return p\nend\nvar x = f(false)\n"), true
      "variant field (50.10.8)", (fun v -> "union U W(p: boolean) end\nvar x = W(" + v + ").p\n"), true ]

/// How the string reaches the site: as a Jyro literal, or as a host string
/// (a `JyroString` in `Input.s`).
let private forms = [ "literal"; "host" ]

/// Run the site with the string `s` in the form `form`, then `Output.x = x`.
let private runBoolSite (siteName: string) (form: string) (s: string) : JyroResult<JyroValue> =
    let _, fragment, _ = boolSites |> List.find (fun (n, _, _) -> n = siteName)
    if form = "literal" then
        Script.runWith limited (fragment (AsciiCase.literal s) + "Output.x = x")
    else
        Script.runWith { limited with Input = ValueInput(Values.obj [ "s", Values.str s ]) } (fragment "Input.s" + "Output.x = x")

/// Theory rows: every site but the variant field (see `variantSite`), every
/// form it can take, every case label.
let private boolRows (cases: string list) : obj[] seq =
    seq {
        for (siteName, _, takesHost) in boolSites |> List.filter (fun (n, _, _) -> n <> variantSite) do
            for form in forms do
                for s in cases do
                    let reachable =
                        if form = "literal" then not (AsciiCase.hasUnpairedSurrogate s) else takesHost
                    if reachable then yield [| box siteName; box form; box (AsciiCase.label s) |]
    }

let private hasProp (name: string) (v: JyroValue) : bool =
    match v with
    | :? JyroObject as o -> fst (o.TryGetValue name)
    | _ -> false

let private assertCoercesTo (expected: bool) (label: string) (r: JyroResult<JyroValue>) =
    let out = Script.output r
    Assert.True((Values.asBool (Values.prop "x" out) = expected), sprintf "%s coerced to boolean gave %s" label (Values.toJson (Values.prop "x" out)))

let private assertRefusedAsBoolean (r: JyroResult<JyroValue>) =
    // The code first (4.1.11); then the run stopped at the coercion.
    Script.expectError MessageCode.InvalidType r
    match Script.value r with
    | Some v -> Assert.False(hasProp "x" v, "Output.x was set after the refused coercion")
    | None -> ()

/// The strings equal to "true" or "false" by 10.7.1 (30.12.7).
let private trueFalseCases =
    [ "true"; "TRUE"; "True"; "tRuE"; "false"; "FALSE"; "False"; "fAlSe" ]

let private truthOf (s: string) = s.Length = 4

let trueFalseRows () : obj[] seq = boolRows trueFalseCases

[<Theory>]
[<Trait("requirement", "JYRO-REQ-127")>]
[<MemberData(nameof trueFalseRows)>]
let ``JYRO-REQ-127 TrueAndFalse_WithNoCaseSensitivityForAsciiLetters_CoerceToTheBoolean`` (siteName: string, form: string, caseLabel: string) =
    let s = AsciiCase.byLabel trueFalseCases caseLabel
    assertCoercesTo (truthOf s) caseLabel (runBoolSite siteName form s)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-127")>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<InlineData("tr-TR")>]
[<InlineData("")>]
let ``JYRO-REQ-127 JYRO-REQ-1941 TrueAndFalse_UnderAnotherCulture_StillCoerce`` (culture: string) =
    // 10.7.1 reads no case mapping, so the current culture changes nothing.
    AsciiCase.underCulture culture (fun () ->
        for s in [ "TRUE"; "tRuE"; "FALSE"; "False"; "true" ] do
            for form in forms do
                assertCoercesTo (truthOf s) (culture + " " + s) (runBoolSite "var (50.1.5)" form s))

/// Strings that are not a spelling of "true" or "false" at all (30.12.8).
let private otherBoolCases =
    [ "no"; "yes"; ""; " "; " true"; "true "; "\ttrue"; "1"; "0"; "t"; "f"; "tru"; "truee"; "null"; "on"; "off" ]

let otherBoolRows () : obj[] seq = boolRows otherBoolCases

[<Theory>]
[<Trait("requirement", "JYRO-REQ-128")>]
[<MemberData(nameof otherBoolRows)>]
let ``JYRO-REQ-128 AnyOtherString_RaisesInvalidType_WhenCoercedToBoolean`` (siteName: string, form: string, caseLabel: string) =
    assertRefusedAsBoolean (runBoolSite siteName form (AsciiCase.byLabel otherBoolCases caseLabel))

/// Strings that differ from "true" or "false" only where 10.7.1 decides: a
/// code unit that is not an ASCII letter in place of a letter (U+017F, U+0130,
/// U+0131, U+212A), another length, or an unpaired surrogate (host form only).
let private lookAlikeBoolCases =
    [ "fal" + AsciiCase.longS + "e"
      "FAL" + AsciiCase.longS + "E"
      "fal" + AsciiCase.longS
      "tru" + AsciiCase.longS
      AsciiCase.dottedI
      "fal" + AsciiCase.dotlessI + "e"
      "fals"
      "falsee"
      "false" + AsciiCase.loneLow
      "tr" + AsciiCase.loneLow + "e" ]

let lookAlikeBoolRows () : obj[] seq = boolRows lookAlikeBoolCases

[<Theory>]
[<Trait("requirement", "JYRO-REQ-128")>]
[<MemberData(nameof lookAlikeBoolRows)>]
let ``JYRO-REQ-128 NonAsciiLookAlikeOrOtherLength_RaisesInvalidType_WhenCoercedToBoolean`` (siteName: string, form: string, caseLabel: string) =
    assertRefusedAsBoolean (runBoolSite siteName form (AsciiCase.byLabel lookAlikeBoolCases caseLabel))

/// 10.7.1 (JYRO-REQ-1941) through its users 30.12.7 and 30.12.8, at every site but the
/// variant field: whether a variant field coerces its value at all is 50.10.8's rule
/// (JYRO-REQ-245, another work order), which a look-alike cannot tell apart from 10.7.1.
let lookAlikeBoolRowsBut5010 () : obj[] seq =
    lookAlikeBoolRows () |> Seq.filter (fun row -> (row.[0] :?> string) <> "variant field (50.10.8)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1941")>]
[<MemberData(nameof lookAlikeBoolRowsBut5010)>]
let ``JYRO-REQ-1941 a non-ASCII look-alike or another length of true or false is refused as a boolean`` (siteName: string, form: string, caseLabel: string) =
    assertRefusedAsBoolean (runBoolSite siteName form (AsciiCase.byLabel lookAlikeBoolCases caseLabel))

// ---------------------------------------------------------------- 30.12.9

let private samples =
    [ "null", "null"; "boolean", "true"; "number", "1"; "string", "\"s\""
      "array", "[1]"; "object", "{a: 1}"; "function", "(y => y)" ]

/// The pairs 30.12.1 to 30.12.8 give: same type, and the conversions listed there.
let private given (valueType: string) (hint: string) =
    valueType = hint
    || (hint = "number" && (valueType = "string" || valueType = "boolean"))
    || (hint = "string" && (valueType = "number" || valueType = "boolean"))
    || (hint = "boolean" && (valueType = "number" || valueType = "string"))

let invalidPairData () : obj[] seq =
    seq {
        for (s, _) in conversionSites do
            for (hint, _) in samples do
                for (valueType, value) in samples do
                    if not (given valueType hint) then
                        yield [| box s; box hint; box valueType; box value |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-129")>]
[<MemberData(nameof invalidPairData)>]
let ``JYRO-REQ-129 Every pair of value type and hint that 30.12 does not give raises InvalidType`` (siteName: string, hint: string, _valueType: string, value: string) =
    Script.expectError MessageCode.InvalidType (runSite siteName hint value "Output.x = [x]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-129")>]
[<MemberData(nameof invalidPairData)>]
let ``JYRO-REQ-129 the InvalidType of a pair 30.12 does not give has exactly the value type then the hinted type`` (siteName: string, hint: string, valueType: string, value: string) =
    expectErrorArgs MessageCode.InvalidType [ valueType; hint ] (runSite siteName hint value "Output.x = [x]")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-129")>]
[<InlineData("var a = [1]\na[0]: number = \"x\"", "string", "number")>]
[<InlineData("var n: boolean = [1]", "array", "boolean")>]
let ``JYRO-REQ-129 the clause's examples give the arguments value type then hinted type`` (source: string, valueType: string, hint: string) =
    expectErrorArgs MessageCode.InvalidType [ valueType; hint ] (Script.run source)

/// 30.12.3's InvalidType (a string that is not a finite JSON number), at every
/// conversion site: the same two arguments, string then number.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-129")>]
[<MemberData(nameof rejectedData)>]
let ``JYRO-REQ-129 the InvalidType of 30.12.3 has exactly the arguments string then number`` (siteName: string, text: string) =
    expectErrorArgs MessageCode.InvalidType [ "string"; "number" ] (runSite siteName "number" text "Output.x = x")

/// 30.12.8's InvalidType (any other string under boolean), at every site where
/// a hint coerces a string to boolean: the same two arguments, string then boolean.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-129")>]
[<MemberData(nameof otherBoolRows)>]
let ``JYRO-REQ-129 the InvalidType of 30.12.8 has exactly the arguments string then boolean`` (siteName: string, form: string, caseLabel: string) =
    expectErrorArgs MessageCode.InvalidType [ "string"; "boolean" ] (runBoolSite siteName form (AsciiCase.byLabel otherBoolCases caseLabel))

let givenPairData () : obj[] seq =
    seq {
        for (s, _) in conversionSites do
            for (hint, _) in samples do
                for (valueType, value) in samples do
                    if valueType = hint then
                        yield [| box s; box hint; box value |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-129")>]
[<MemberData(nameof givenPairData)>]
let ``JYRO-REQ-129 A value of the hinted type does not raise`` (siteName: string, hint: string, value: string) =
    let r = runSite siteName hint value "Output.t = TypeOf(x)"
    Assert.True(Script.ok r, Script.describe r)
    Assert.Equal(hint, Values.asString (Values.prop "t" (Script.output r)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-129")>]
let ``JYRO-REQ-129 the host API's CoerceToType takes no target name`` () =
    // No argument names the target (30.12.9), so no public coercion member takes
    // one: a parameter with no effect is not published (JYRO-DEF-613, JYRO-ADR-155 (5)).
    let flags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Instance
    // CoerceToType is not public at all (JYRO-REQ-1876), so the host API has no coercion member; if one
    // were public it must not take a name.
    let members = typeof<JyroValue>.GetMethods(flags) |> Array.filter (fun m -> m.Name.Contains "Coerce")
    for m in members do
        let ps = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
        Assert.True(
            not (ps |> Array.contains typeof<string>),
            sprintf "%s(%s) takes a name" m.Name (String.concat ", " (ps |> Array.map (fun t -> t.Name))))

// ---------------------------------------------------------------- 30.12.10

[<Theory>]
[<Trait("requirement", "JYRO-REQ-130")>]
[<InlineData("var o = {p: 1}\no.p: number += 1")>]
[<InlineData("var o = {p: 1}\no.p: number -= 1")>]
[<InlineData("var o = {p: 1}\no.p: number *= 2")>]
[<InlineData("var o = {p: 1}\no.p: number /= 2")>]
[<InlineData("var o = {p: 1}\no.p: number %= 2")>]
[<InlineData("var o = {p: \"a\"}\no.p: string &= \"a\"")>]
[<InlineData("var a = [1]\na[0]: number += 1")>]
[<InlineData("var a = [1]\na[0]: any += 1")>]
[<InlineData("var o = {p: 1}\no[\"q\"]: number *= 3")>]
let ``A compound assignment with a type hint on a property or an element is rejected at validation`` (source: string) =
    Script.expectError MessageCode.TypeHintOnCompoundAssignment (Script.compile source)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-130")>]
[<InlineData("var x = 1\nx: number += 1")>]
[<InlineData("var x = 1\nx: number -= 1")>]
[<InlineData("var x = 1\nx: number *= 2")>]
[<InlineData("var x = 1\nx: number /= 2")>]
[<InlineData("var x = 1\nx: number %= 2")>]
[<InlineData("var s = \"\"\ns: string &= \"a\"")>]
let ``A compound assignment with a type hint on a variable is rejected at validation`` (source: string) =
    let r = Script.compile source
    assertFails r
    Assert.Contains(MessageCode.TypeHintOnCompoundAssignment, Script.errorCodes r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-130")>]
[<InlineData("var y: number = 1")>]
[<InlineData("var x = 1\nx += 1")>]
[<InlineData("var o = {}\no.p: number = 1")>]
[<InlineData("var s = \"\"\ns &= \"a\"")>]
let ``An assignment with a hint and =, or a compound one with no hint, is not rejected`` (source: string) =
    let r = Script.compile source
    Assert.DoesNotContain(MessageCode.TypeHintOnCompoundAssignment, Script.codes r)
    Assert.True(Script.ok r, Script.describe r)

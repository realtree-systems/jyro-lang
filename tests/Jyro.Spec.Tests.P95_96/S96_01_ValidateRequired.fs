/// jyro-spec 96.1 ValidateRequired.
module RealTree.Jyro.Spec.Tests.S96_01

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

/// The errors of a ValidateRequired result as (field, reason) pairs.
let private errorsOf (v: JyroValue) =
    Values.asList (Values.prop "errors" v)
    |> List.map (fun e -> Values.asString (Values.prop "field" e), Values.asString (Values.prop "reason" e))

let private vr (args: string) : JyroValue = r (sprintf "var f = x => x\nOutput.r = ValidateRequired(%s)" args)

let private expectReasons (expected: (string * string) list) (args: string) =
    let v = vr args
    Assert.Equal<(string * string) list>(expected, errorsOf v)
    Assert.Equal(List.isEmpty expected, Values.asBool (Values.prop "valid" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2036")>]
let ``ValidateRequired_TakesObjAndFieldsAndReturnsAnObject`` () =
    let src = "Output.r = [TypeOf(ValidateRequired({a: 1}, ['a'])), ValidateRequired(source: {a: 1}, fields: ['a']).valid, ValidateRequired(fields: ['b'], source: {a: 1}).valid]"
    Assert.Equal(norm """["object",true,false]""", rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2036")>]
let ``ValidateRequired_WithOneArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = ValidateRequired({})"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2036")>]
let ``ValidateRequired_WithThreeArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = ValidateRequired({}, [], 1)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1213")>]
[<InlineData("{a: 1}, ['a']")>]
[<InlineData("{}, ['a']")>]
[<InlineData("{}, []")>]
[<InlineData("null, null")>]
let ``ValidateRequired_ResultHasExactlyValidThenErrors`` (args: string) =
    Assert.Equal(norm """["valid","errors"]""", rJson (sprintf "Output.r = Keys(ValidateRequired(%s))" args))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1214")>]
[<InlineData("{a: 1}, ['a']", 0)>]
[<InlineData("{}, []", 0)>]
[<InlineData("{}, ['a']", 1)>]
[<InlineData("{b: null, c: ''}, ['a', 'b', 'c']", 3)>]
let ``ValidateRequired_ValidIsTrueExactlyWhenErrorsIsEmpty`` (args: string, count: int) =
    let src = sprintf "var v = ValidateRequired(%s)\nOutput.r = [TypeOf(v.valid), v.valid, Length(v.errors)]" args
    Assert.Equal(sprintf """["boolean",%b,%d]""" (count = 0) count, rJson src)

// =========================================================================
// 96.1.4 missing: a name is one key, compared by its code units (30.5.2, 30.5.6)
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2037")>]
[<InlineData("{}, ['a']", "a", true)>]
[<InlineData("{b: 1}, ['a']", "a", true)>]
[<InlineData("{'a.b': 1}, ['a.b']", "a.b", false)>]
[<InlineData("{a: {b: 1}}, ['a.b']", "a.b", true)>]
[<InlineData("{'a.b': {c: 1}}, ['a.b.c']", "a.b.c", true)>]
[<InlineData("{A: 1}, ['a']", "a", true)>]
[<InlineData("{a: 1}, ['A']", "A", true)>]
[<InlineData("{'\\u00E9': 1}, ['e\\u0301']", "é", true)>]
[<InlineData("{'e\\u0301': 1}, ['\\u00E9']", "é", true)>]
[<InlineData("{'\\u00E9': 1}, ['\\u00E9']", "é", false)>]
[<InlineData("{a: 1}, [' a']", " a", true)>]
[<InlineData("{a: 1}, ['a ']", "a ", true)>]
[<InlineData("{a: 1, b: 2}, ['b', 'a']", "", false)>]
[<InlineData("{'': 1}, ['']", "", false)>]
let ``JYRO-REQ-2037 ValidateRequired_ReportsMissingForANameWithNoSuchKey`` (args: string, name: string, missing: bool) =
    expectReasons (if missing then [ name, "missing" ] else []) args

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2037")>]
let ``JYRO-REQ-2037 ValidateRequired_ReportsMissingForAKeyAHostObjectLacks`` () =
    let v = rIn """{"o":{"x":1,"a.b":2}}""" "Output.r = ValidateRequired(Input.o, ['x', 'a', 'a.b', 'b'])"
    Assert.Equal<(string * string) list>([ "a", "missing"; "b", "missing" ], errorsOf v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2037")>]
let ``JYRO-REQ-2037 ValidateRequired_ReportsMissingForADeletedKey`` () =
    let v = r "var o = {a: 1, b: 2}\ndelete o.a\nOutput.r = ValidateRequired(o, ['a', 'b'])"
    Assert.Equal<(string * string) list>([ "a", "missing" ], errorsOf v)

// =========================================================================
// 96.1.5 null
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1216")>]
[<InlineData("var o = {a: null}")>]
[<InlineData("var o = {}\no.a = null")>]
[<InlineData("var o = FromJson('{\"a\":null}')")>]
[<InlineData("var o = Input")>]
let ``JYRO-REQ-1216 ValidateRequired_ReportsNullForANullValue`` (decl: string) =
    let v = rIn """{"a":null}""" (decl + "\nOutput.r = ValidateRequired(o, ['a'])")
    Assert.Equal<(string * string) list>([ "a", "null" ], errorsOf v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1216")>]
let ``JYRO-REQ-1216 ValidateRequired_NullIsNeitherMissingNorEmpty`` () =
    // a null value is reported as null: the key is there, so not missing, and
    // the value is not a string, so not empty. The string 'null' is no error.
    expectReasons [ "n", "null"; "m", "missing" ] "{n: null, s: 'null', e: 1}, ['n', 's', 'm', 'e']"

// =========================================================================
// 96.1.6 empty: empty or only White_Space of Unicode 15.0.0 (10.6.1)
// =========================================================================

/// The 25 code points with the property White_Space in Unicode 15.0.0
/// (PropList.txt): U+0009..U+000D, U+0020, U+0085, U+00A0, U+1680,
/// U+2000..U+200A, U+2028, U+2029, U+202F, U+205F and U+3000.
let whiteSpaceCodePoints =
    [ 0x0009; 0x000A; 0x000B; 0x000C; 0x000D; 0x0020; 0x0085; 0x00A0; 0x1680
      0x2000; 0x2001; 0x2002; 0x2003; 0x2004; 0x2005; 0x2006; 0x2007; 0x2008; 0x2009; 0x200A
      0x2028; 0x2029; 0x202F; 0x205F; 0x3000 ]

let whiteSpaceStrings : obj[] seq =
    [ yield ""
      for cp in whiteSpaceCodePoints do yield string (char cp)
      yield (whiteSpaceCodePoints |> List.map (fun cp -> string (char cp)) |> String.concat "")
      yield " \u0009　\u000A" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1217")>]
[<MemberData(nameof whiteSpaceStrings)>]
let ``JYRO-REQ-1217 ValidateRequired_ReportsEmptyForEmptyOrWhiteSpaceString`` (s: string) =
    let v = rValB (Values.obj [ "o", Values.obj [ "a", Values.str s ] ]) "Output.r = ValidateRequired(Input.o, ['a'])"
    Assert.Equal<(string * string) list>([ "a", "empty" ], errorsOf v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1217")>]
let ``JYRO-REQ-1217 ValidateRequired_ReportsEmptyForStringsMadeInTheScript`` () =
    expectReasons [ "a", "empty"; "b", "empty"; "c", "empty" ] "{a: '', b: '   ', c: '\\u0009\\u000A\\u3000'}, ['a', 'b', 'c']"

/// Strings that hold a code point that is not White_Space in Unicode 15.0.0:
/// U+200B ZERO WIDTH SPACE, U+FEFF ZERO WIDTH NO-BREAK SPACE, U+180E MONGOLIAN
/// VOWEL SEPARATOR (White_Space only until Unicode 6.3), U+001C and U+001F
/// (separators, not White_Space), U+0000, a digit and letters.
let nonWhiteSpaceStrings : obj[] seq =
    [ "a"; " a "; "​"; "﻿"; "᠎"; " ᠎ "; "\u001C"; "\u001F"; "0"; "null"; "\u0000"; "　​" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1217")>]
[<MemberData(nameof nonWhiteSpaceStrings)>]
let ``JYRO-REQ-1217 ValidateRequired_ReportsNoErrorForStringWithNonWhiteSpace`` (s: string) =
    let v = rValB (Values.obj [ "o", Values.obj [ "a", Values.str s ] ]) "Output.r = ValidateRequired(Input.o, ['a'])"
    Assert.Equal<(string * string) list>([], errorsOf v)
    Assert.True(Values.asBool (Values.prop "valid" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2166")>]
let ``JYRO-REQ-2166 ValidateRequired_ReportsNoErrorForNumberBooleanContainerOrFunction`` () =
    expectReasons [] "{n: 0, m: 0 * -1, i: 0.5, t: true, fa: false, a: [], o: {}, fn: f, big: 1e308, z: SquareRoot(2) - SquareRoot(2)}, ['n', 'm', 'i', 't', 'fa', 'a', 'o', 'fn', 'big', 'z']"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2166")>]
let ``JYRO-REQ-2166 ValidateRequired_OfHostInputWithFalsyValues_ReportsNoError`` () =
    let v = rIn """{"n":0,"f":false,"a":[],"o":{}}""" "Output.r = ValidateRequired(Input, ['n', 'f', 'a', 'o'])"
    Assert.Equal<(string * string) list>([], errorsOf v)
    Assert.True(Values.asBool (Values.prop "valid" v))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2166")>]
[<InlineData("{a: 0}")>]
[<InlineData("{a: false}")>]
[<InlineData("{a: []}")>]
[<InlineData("{a: {}}")>]
[<InlineData("{a: 'x'}")>]
[<InlineData("{a: ' x '}")>]
[<InlineData("{a: f}")>]
[<InlineData("{a: 1.5}")>]
[<InlineData("{a: true}")>]
[<InlineData("{a: [null]}")>]
[<InlineData("{a: {b: null}}")>]
[<InlineData("{a: 'null'}")>]
[<InlineData("{a: 'false'}")>]
[<InlineData("{a: '0'}")>]
let ``JYRO-REQ-2166 ValidateRequired_TheClausesExamples_GiveValidTrueAndAnEmptyErrors`` (obj: string) =
    let v = vr (sprintf "%s, ['a']" obj)
    Assert.True(Values.asBool (Values.prop "valid" v))
    Assert.Equal(norm "[]", Values.toJson (Values.prop "errors" v))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2166")>]
[<InlineData("{}", "missing")>]
[<InlineData("{b: 1}", "missing")>]
[<InlineData("{a: null}", "null")>]
[<InlineData("{a: ''}", "empty")>]
[<InlineData("{a: ' '}", "empty")>]
[<InlineData("{a: '  \\u0009\\u000A\\u3000'}", "empty")>]
[<InlineData("{a: '\\u00A0'}", "empty")>]
[<InlineData("{a: '\\u2003'}", "empty")>]
let ``JYRO-REQ-2166 ValidateRequired_ReportsExactlyOneErrorForAMissingNullEmptyOrWhiteSpaceOnlyValue`` (obj: string, reason: string) =
    expectReasons [ "a", reason ] (sprintf "%s, ['a']" obj)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2166")>]
let ``JYRO-REQ-2166 ValidateRequired_ReportsAnErrorForExactlyTheNamesThatAreMissingNullOrEmpty`` () =
    // Names that are fine are not reported; the others are, each once and in the order of fields.
    expectReasons [ "m", "missing"; "n", "null"; "e", "empty"; "w", "empty" ]
        "{z: 0, f: false, a: [], o: {}, s: 'x', p: ' x ', fn: f, n: null, e: '', w: '\\u00A0'}, ['z', 'm', 'f', 'n', 'a', 'o', 's', 'p', 'fn', 'e', 'w']"

// =========================================================================
// 96.1.8 element shape, 96.1.9 order, 96.1.10 each name on its own
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1219")>]
let ``JYRO-REQ-1219 ValidateRequired_EachErrorIsAnObjectWithFieldThenReason`` () =
    let src =
        "var v = ValidateRequired({b: null, c: ''}, ['a', 'b', 'c', 'x.y', '\\u00E9\\u4E2D'])\nvar out = []\n"
        + "foreach e in v.errors do out = Append(out, [TypeOf(e), Keys(e), e.field, e.reason, TypeOf(e.field), TypeOf(e.reason)]) end\nOutput.r = out"
    let row (f: string) (r: string) = sprintf """["object",["field","reason"],%s,"%s","string","string"]""" (Values.toJson (Values.str f)) r
    let expected =
        "[" + String.concat "," [ row "a" "missing"; row "b" "null"; row "c" "empty"; row "x.y" "missing"; row "é中" "missing" ] + "]"
    Assert.Equal(Values.toJson (Values.json expected), rJson src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1219")>]
let ``JYRO-REQ-1219 ValidateRequired_ErrorsAreWholeObjectsOfExactlyTwoProperties`` () =
    // compared as whole values, so an extra property or another order fails
    Assert.Equal(
        norm """{"valid":false,"errors":[{"field":"a","reason":"missing"},{"field":"b","reason":"null"},{"field":"c","reason":"empty"}]}""",
        rJson "Output.r = ValidateRequired({b: null, c: ' '}, ['a', 'b', 'c'])")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1220")>]
let ``JYRO-REQ-1220 ValidateRequired_ErrorsFollowTheOrderOfFields`` () =
    expectReasons [ "z", "missing"; "a", "missing"; "m", "missing" ] "{q: 1, p: 2}, ['z', 'a', 'm']"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1220")>]
let ``JYRO-REQ-1220 ValidateRequired_ErrorsFollowFieldsNotObjKeysOrReason`` () =
    expectReasons [ "z", "empty"; "b", "missing"; "a", "null"; "c", "empty" ] "{a: null, c: ' ', z: '', ok: 1}, ['z', 'ok', 'b', 'a', 'c']"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1221")>]
[<InlineData("{}, ['a', 'a']", "a:missing,a:missing")>]
[<InlineData("{a: null}, ['a', 'b', 'a']", "a:null,b:missing,a:null")>]
[<InlineData("{a: ''}, ['a', 'a', 'a']", "a:empty,a:empty,a:empty")>]
[<InlineData("{a: 1}, ['a', 'a']", "")>]
[<InlineData("{a: 1}, ['b', 'a', 'b']", "b:missing,b:missing")>]
let ``JYRO-REQ-1221 ValidateRequired_ChecksEachNameOnItsOwn_DuplicateNameTwice`` (args: string, expected: string) =
    let exp =
        if expected = "" then []
        else expected.Split(',') |> Array.map (fun p -> let a = p.Split(':') in a.[0], a.[1]) |> List.ofArray
    expectReasons exp args

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1222")>]
[<InlineData("{a: 1}, [1]")>]
[<InlineData("{a: 1}, ['a', 1]")>]
[<InlineData("{a: 1}, ['a', null]")>]
[<InlineData("{a: 1}, [null]")>]
[<InlineData("{a: 1}, [true]")>]
[<InlineData("{a: 1}, [['a']]")>]
[<InlineData("{a: 1}, [{}]")>]
[<InlineData("{a: 1}, [f]")>]
[<InlineData("{a: 1}, [1, 'a', 'a']")>]
[<InlineData("{a: 1}, ['a', false, 'a']")>]
[<InlineData("{}, ['a', 'b', 0]")>]
[<InlineData("{}, [0, 'a', 'b']")>]
[<InlineData("null, [1]")>]
let ``JYRO-REQ-1222 ValidateRequired_WhenAFieldsElementIsNotAString_RaisesArgumentTypeMismatch`` (args: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = ValidateRequired(%s)" args)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1222")>]
let ``JYRO-REQ-1222 ValidateRequired_OfHostFieldsWithANumber_RaisesArgumentTypeMismatch`` () =
    failsIn """{"o":{"a":1},"f":["a",1]}""" MessageCode.ArgumentTypeMismatch "Output.r = ValidateRequired(Input.o, Input.f)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2038")>]
[<InlineData("null, ['a', 'b']", "a:missing,b:missing")>]
[<InlineData("x, ['a']", "a:missing")>]
[<InlineData("null, []", "")>]
[<InlineData("null, ['a', 'a']", "a:missing,a:missing")>]
[<InlineData("null, ['']", ":missing")>]
let ``JYRO-REQ-2038 ValidateRequired_TreatsNullObjAsEmptyObject`` (args: string, expected: string) =
    let exp =
        if expected = "" then []
        else expected.Split(',') |> Array.map (fun p -> let a = p.Split(':') in a.[0], a.[1]) |> List.ofArray
    let v = r (sprintf "var x = null\nOutput.r = ValidateRequired(%s)" args)
    Assert.Equal<(string * string) list>(exp, errorsOf v)
    Assert.Equal(List.isEmpty exp, Values.asBool (Values.prop "valid" v))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2038")>]
let ``JYRO-REQ-2038 ValidateRequired_TreatsAMissingInputPropertyAsEmptyObject`` () =
    let v = rIn "{}" "Output.r = ValidateRequired(Input.none, ['a'])"
    Assert.Equal(norm """{"valid":false,"errors":[{"field":"a","reason":"missing"}]}""", Values.toJson v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1224")>]
[<InlineData("{}, null")>]
[<InlineData("{a: null}, null")>]
[<InlineData("null, null")>]
[<InlineData("{}, y")>]
let ``ValidateRequired_TreatsNullFieldsAsEmptyArray`` (args: string) =
    let v = r (sprintf "var y = null\nOutput.r = ValidateRequired(%s)" args)
    Assert.Equal(norm """{"valid":true,"errors":[]}""", Values.toJson v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2039")>]
[<InlineData("[]")>]
[<InlineData("['a']")>]
[<InlineData("'a'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("f")>]
let ``ValidateRequired_WhenObjNotObjectOrNull_RaisesArgumentTypeMismatch`` (obj: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = ValidateRequired(%s, ['a'])" obj)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2039")>]
[<InlineData("{}")>]
[<InlineData("{a: 1}")>]
[<InlineData("'a'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("f")>]
let ``ValidateRequired_WhenFieldsNotArrayOrNull_RaisesArgumentTypeMismatch`` (fields: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = ValidateRequired({a: 1}, %s)" fields)

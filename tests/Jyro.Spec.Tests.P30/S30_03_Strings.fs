/// 30.3 Strings.
module RealTree.Jyro.Spec.Tests.P30.S30_03_Strings

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

/// Jyro source text for the string "a", U+1F600 (a surrogate pair), "b".
let private aEmojiB = """ "a😀b" """
let private emoji = """ "😀" """

// ---------------------------------------------------------------- 30.3.1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``Length counts UTF-16 code units`` () =
    let out = outOf ("var s = " + aEmojiB + "\nOutput.a = Length(s)\nOutput.e = Length(" + emoji + ")")
    assertNum 4M (Values.prop "a" out)
    assertNum 2M (Values.prop "e" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``Indexing counts UTF-16 code units from 0`` () =
    let out = outOf ("var s = " + aEmojiB + "\nOutput.r = [s[0], s[1], s[2], s[3]]")
    let parts = Values.asList (Values.prop "r" out) |> List.map Values.asString
    Assert.Equal<string list>([ "a"; unit 0xD83D; unit 0xDE00; "b" ], parts)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``foreach over a string gives one code unit at a time`` () =
    let out = outOf ("var s = " + aEmojiB + "\nvar r = []\nvar i = 0\nforeach c in s do\n r[i] = c\n i = i + 1\nend\nOutput.r = r")
    let parts = Values.asList (Values.prop "r" out) |> List.map Values.asString
    Assert.Equal<string list>([ "a"; unit 0xD83D; unit 0xDE00; "b" ], parts)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``Substring counts positions in code units`` () =
    let out =
        outOf (
            "var s = " + aEmojiB + "\n"
            + "Output.pair = Substring(s, 1, 2) == " + emoji + "\n"
            + "Output.one = Substring(s, 1, 1) == s[1]\n"
            + "Output.oneLength = Length(Substring(s, 1, 1))\n"
            + "Output.tail = Substring(s, 3)")
    Assert.True(Values.asBool (Values.prop "pair" out))
    Assert.True(Values.asBool (Values.prop "one" out))
    assertNum 1M (Values.prop "oneLength" out)
    Assert.Equal("b", Values.asString (Values.prop "tail" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``IndexOf and RegexMatchDetail report positions in code units`` () =
    let out =
        outOf (
            "var s = " + aEmojiB + "\n"
            + "Output.i = IndexOf(s, \"b\")\n"
            + "Output.d = RegexMatchDetail(s, \"b\").index")
    assertNum 3M (Values.prop "i" out)
    assertNum 3M (Values.prop "d" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``Padding counts the length in code units`` () =
    let out =
        outOf (
            "Output.l = Length(PadLeft(" + emoji + ", 3))\n"
            + "Output.r = Length(PadRight(" + emoji + ", 3))\n"
            + "Output.same = PadLeft(" + emoji + ", 2) == " + emoji)
    assertNum 3M (Values.prop "l" out)
    assertNum 3M (Values.prop "r" out)
    Assert.True(Values.asBool (Values.prop "same" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``RandomString length is in code units`` () =
    assertNum 4M (evalR "Length(RandomString(4))")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-63")>]
let ``ValidateSchema minLength and maxLength count code units`` () =
    let out =
        outOf (
            "Output.tooLong = Length(ValidateSchema(" + emoji + ", {maxLength: 1}))\n"
            + "Output.fits = Length(ValidateSchema(" + emoji + ", {minLength: 2, maxLength: 2}))")
    assertNum 1M (Values.prop "tooLong" out)
    assertNum 0M (Values.prop "fits" out)

// ---------------------------------------------------------------- 30.3.2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1795")>]
let ``JYRO-REQ-1795 No string operation changes the original string`` () =
    let out =
        outOf (
            "var s = \"abc\"\nvar t = s\n"
            + "var u = ToUpper(s)\nvar r = Replace(s, \"a\", \"z\")\nvar c = s & \"d\"\n"
            + "var sub = Substring(s, 1)\nvar tr = Trim(\" \" & s)\nvar p = PadLeft(s, 5)\n"
            + "t &= \"x\"\n"
            + "Output.r = [s, t, u, r, c, sub, tr, p]")
    let parts = Values.asList (Values.prop "r" out) |> List.map Values.asString
    Assert.Equal<string list>([ "abc"; "abcx"; "ABC"; "zbc"; "abcd"; "bc"; "abc"; "  abc" ], parts)

/// Run `source` with `Output` an object whose `s` is the string "abc", held also
/// by an array and an object of the host's `Output`, and with the string "abc"
/// as `Input`; return the result and every holder, to read after the run.
let private runOnHeldString (source: string) =
    let original = JyroString("abc")
    let holderArray = JyroArray()
    holderArray.Add(original)
    let holderObject = JyroObject()
    holderObject.SetProperty("p", original)
    let input = JyroString("abc")
    let output = Values.obj [ "s", original :> JyroValue; "arr", holderArray :> JyroValue; "obj", holderObject :> JyroValue ]
    let r = Script.runWith { Script.defaults with Output = Some output; Input = ValueInput input } source
    r, original, holderArray, holderObject, input

let private assertStringsUnchanged (original: JyroString, holderArray: JyroArray, holderObject: JyroObject, input: JyroString) =
    Assert.Equal("abc", original.Value)
    Assert.Equal("abc", Values.asString (holderArray.[0]))
    Assert.Equal("abc", Values.asString (holderObject.GetProperty("p")))
    Assert.Equal("abc", input.Value)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1793")>]
[<InlineData("0", "\"z\"")>]
[<InlineData("1", "1")>]
[<InlineData("2", "null")>]
[<InlineData("3", "\"abc\"")>]
[<InlineData("99", "[1]")>]
[<InlineData("-1", "{}")>]
[<InlineData("1.5", "true")>]
[<InlineData("0.5", "x => x")>]
[<InlineData("null", "\"z\"")>]
[<InlineData("\"x\"", "\"z\"")>]
[<InlineData("\"0\"", "\"z\"")>]
[<InlineData("true", "1")>]
[<InlineData("[0]", "1")>]
[<InlineData("{}", "1")>]
let ``JYRO-REQ-1793 s[i] = v raises SetIndexOnNonContainer and leaves every holder of s unchanged`` (index: string, value: string) =
    let r, original, arr, obj, input =
        runOnHeldString ("var s = Output.s\nvar v = " + value + "\ns[" + index + "] = v\nOutput.after = s")
    Script.expectError MessageCode.SetIndexOnNonContainer r
    assertStringsUnchanged (original, arr, obj, input)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1793")>]
[<InlineData("k", "\"z\"")>]
[<InlineData("length", "3")>]
[<InlineData("Length", "null")>]
[<InlineData("name", "[1]")>]
[<InlineData("abc", "\"abc\"")>]
[<InlineData("x", "{}")>]
[<InlineData("y", "false")>]
[<InlineData("z", "x => x")>]
let ``JYRO-REQ-1793 s.k = v raises SetPropertyOnNonObject and leaves every holder of s unchanged`` (name: string, value: string) =
    let r, original, arr, obj, input =
        runOnHeldString ("var s = Output.s\nvar v = " + value + "\ns." + name + " = v\nOutput.after = s")
    Script.expectError MessageCode.SetPropertyOnNonObject r
    assertStringsUnchanged (original, arr, obj, input)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1793")>]
let ``JYRO-REQ-1793 A cyclic value written to a string gives the same errors`` () =
    let r1, o1, a1, b1, i1 = runOnHeldString "var s = Output.s\nvar v = [1]\nv[1] = v\ns[0] = v"
    Script.expectError MessageCode.SetIndexOnNonContainer r1
    assertStringsUnchanged (o1, a1, b1, i1)
    let r2, o2, a2, b2, i2 = runOnHeldString "var s = Output.s\nvar v = {}\nv.me = v\ns.k = v"
    Script.expectError MessageCode.SetPropertyOnNonObject r2
    assertStringsUnchanged (o2, a2, b2, i2)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1793")>]
let ``JYRO-REQ-1793 A write to the string that Input is raises the same errors and leaves Input as it was`` () =
    let input = JyroString("abc")
    let r1 = Script.runInput input "var s = Input\ns[0] = \"z\""
    Script.expectError MessageCode.SetIndexOnNonContainer r1
    Assert.Equal("abc", input.Value)
    let r2 = Script.runInput input "var s = Input\nvar t = s\nt.k = 1"
    Script.expectError MessageCode.SetPropertyOnNonObject r2
    Assert.Equal("abc", input.Value)

/// Operations that take the string held by `v`, as statements.
let private stringOperations =
    [ "ToUpper", "var x = ToUpper(v)"
      "ToLower", "var x = ToLower(v)"
      "Trim", "var x = Trim(v)"
      "Replace", "var x = Replace(v, \"b\", \"z\")"
      "Contains", "var x = Contains(v, \"b\")"
      "StartsWith", "var x = StartsWith(v, \" \")"
      "EndsWith", "var x = EndsWith(v, \" \")"
      "Split", "var x = Split(v, \",\")"
      "Join", "var x = Join([v, v], \"-\")"
      "Substring", "var x = Substring(v, 1, 2)"
      "ToNumber", "var x = ToNumber(v)"
      "PadLeft", "var x = PadLeft(v, 10, \"*\")"
      "PadRight", "var x = PadRight(v, 10, \"*\")"
      "RegexTest", "var x = RegexTest(v, \"b\")"
      "RegexMatch", "var x = RegexMatch(v, \"A.\")"
      "RegexMatchAll", "var x = RegexMatchAll(v, \".\")"
      "RegexMatchDetail", "var x = RegexMatchDetail(v, \"b\")"
      "Length", "var x = Length(v)"
      "IndexOf", "var x = IndexOf(v, \"b\")"
      "Base64Encode", "var x = Base64Encode(v)"
      "ToString", "var x = ToString(v)"
      "ToJson", "var x = ToJson(v)"
      "ToBoolean", "var x = ToBoolean(v)"
      "TypeOf", "var x = TypeOf(v)"
      "Coalesce", "var x = Coalesce([null, v])"
      "Clone", "var x = Clone(v)"
      "&", "var x = v & \"x\"\nvar y = \"x\" & v\nvar z = v & v"
      "&= on a copy", "var w = v\nw &= \"x\""
      "comparison", "var x = [v == v, v != \"q\", v < \"zz\", v >= \"\"]"
      "foreach", "var n = 0\nforeach c in v do n = n + 1 end"
      "index read", "var x = [v[0], v[1], v[Length(v) - 1]]"
      "FromJson", "var x = FromJson(ToJson(v))"
      "Sort", "var x = Sort([v, \"a\"])"
      "Distinct", "var x = Distinct([v, v])" ]

let stringOperationData () : obj[] seq = stringOperations |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1795")>]
[<MemberData(nameof stringOperationData)>]
let ``JYRO-REQ-1795 An operator or a library function leaves every holder of its string operand as it was`` (name: string) =
    let operation = stringOperations |> List.find (fun (n, _) -> n = name) |> snd
    let text = " Ab,c "
    let input = JyroString(text)
    let script =
        "var v = Input\nvar arr = [Input, v]\nvar o = {p: Input, q: v}\n"
        + operation
        + "\nOutput.held = [v, arr[0], arr[1], o.p, o.q, Input]"
    let r = Script.runInput input script
    Assert.True(Script.ok r, Script.describe r)
    let held = Values.asList (Values.prop "held" (Script.output r)) |> List.map Values.asString
    Assert.Equal<string list>(List.replicate 6 text, held)
    Assert.Equal(text, input.Value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1795")>]
let ``JYRO-REQ-1795 A host string given as Input is unchanged by the script`` () =
    let input = JyroString("abc")
    let r = Script.runInput input "var t = Input & \"x\"\nOutput.r = ToUpper(Input)"
    Assert.Equal("ABC", Values.asString (Values.prop "r" (Script.output r)))
    Assert.Equal("abc", input.Value)

// ---------------------------------------------------------------- 30.3.3

[<Theory>]
[<Trait("requirement", "JYRO-REQ-65")>]
[<InlineData("0", "a")>]
[<InlineData("1", "b")>]
[<InlineData("2", "c")>]
[<InlineData("1.0", "b")>]
[<InlineData("2 * Power(1, 0.5)", "c")>]
[<InlineData("-0", "a")>]
let ``s[i] gives the one code unit at an integer position`` (index: string, expected: string) =
    let v = evalR ("\"abc\"[" + index + "]")
    Assert.Equal(expected, Values.asString v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-65")>]
let ``s[0] of a one-character string is that character`` () =
    Assert.Equal("q", Values.asString (evalR "\"q\"[0]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-65")>]
let ``Each half of a surrogate pair is returned alone`` () =
    let out = outOf ("var e = " + emoji + "\nOutput.h = e[0]\nOutput.l = e[1]\nOutput.n = [Length(e[0]), Length(e[1])]")
    Assert.Equal(unit 0xD83D, Values.asString (Values.prop "h" out))
    Assert.Equal(unit 0xDE00, Values.asString (Values.prop "l" out))
    Assert.Equal("[1,1]", Values.toJson (Values.prop "n" out))

// ---------------------------------------------------------------- 30.3.4

[<Theory>]
[<Trait("requirement", "JYRO-REQ-66")>]
[<InlineData("\"abc\"", "3")>]
[<InlineData("\"abc\"", "100")>]
[<InlineData("\"abc\"", "-1")>]
[<InlineData("\"abc\"", "-100")>]
[<InlineData("\"\"", "0")>]
[<InlineData("\"\"", "-1")>]
[<InlineData("\"abc\"", "79228162514264337593543950335")>]
[<InlineData("\"abc\"", "-79228162514264337593543950335")>]
[<InlineData("\"abc\"", "1e300")>]
[<InlineData("\"abc\"", "3 * Power(1, 0.5)")>]
let ``s[i] gives null for an integer outside the string`` (s: string, index: string) =
    let v = evalR ("[" + s + "[" + index + "]]")
    Assert.Equal("[null]", Values.toJson v)

// ---------------------------------------------------------------- 30.3.5

[<Theory>]
[<Trait("requirement", "JYRO-REQ-67")>]
[<InlineData("\"0\"")>]
[<InlineData("\"a\"")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("[0]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``s[i] gives null when i is not a number`` (index: string) =
    let out = outOf ("var i = " + index + "\nOutput.r = [\"abc\"[i], \"\"[i]]")
    Assert.Equal("[null,null]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.3.6

[<Theory>]
[<Trait("requirement", "JYRO-REQ-68")>]
[<InlineData("0.5")>]
[<InlineData("1.5")>]
[<InlineData("-0.5")>]
[<InlineData("2.9999")>]
[<InlineData("99.5")>]
[<InlineData("-99.5")>]
[<InlineData("0.1 * Power(1, 0.5)")>]
[<InlineData("1e-28")>]
let ``JYRO-REQ-68 s[i] raises IntegerIndexRequired when i is a number that is not an integer`` (index: string) =
    let r = Script.run ("var s = \"abc\"\nOutput.r = s[" + index + "]")
    Script.expectError (integerIndexRequired ()) r
    Assert.DoesNotContain(MessageCode.IntegerRequired, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-68")>]
let ``JYRO-REQ-68 s[i] on the empty string raises IntegerIndexRequired for a fraction`` () =
    let r = Script.run "var s = \"\"\nOutput.r = s[0.5]"
    Script.expectError (integerIndexRequired ()) r
    Assert.DoesNotContain(MessageCode.IntegerRequired, Script.codes r)

/// The clause's examples and other exact fractions written plainly, whose text
/// as an argument is the same on every reading (160.7.16).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-68")>]
[<InlineData("\"abc\"", "1.5")>]
[<InlineData("\"\"", "0.5")>]
[<InlineData("\"abc\"", "0.5")>]
[<InlineData("\"abc\"", "-0.5")>]
[<InlineData("\"abc\"", "99.5")>]
[<InlineData("\"abc\"", "2.25")>]
let ``JYRO-REQ-68 the IntegerIndexRequired of s[i] has exactly one argument, i`` (s: string, index: string) =
    expectErrorArgs (integerIndexRequired ()) [ index ] (Script.run ("var s = " + s + "\nOutput.r = s[" + index + "]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-68")>]
let ``JYRO-REQ-68 MessageCode IntegerIndexRequired is 5310`` () =
    Assert.Equal(5310, int (integerIndexRequired ()))

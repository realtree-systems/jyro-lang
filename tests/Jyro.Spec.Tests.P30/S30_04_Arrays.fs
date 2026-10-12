/// 30.4 Arrays.
module RealTree.Jyro.Spec.Tests.P30.S30_04_Arrays

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

/// Run `source` with a host-made `Output` of `{arr: <array>}`, and return the
/// result and the host's own array, for inspection after a failure.
let private runOnHostArray (elements: JyroValue list) (source: string) =
    let arr = JyroArray()
    for e in elements do arr.Add e
    let output = Values.obj [ "arr", arr :> JyroValue ]
    let r = Script.runWith { Script.defaults with Output = Some output } source
    r, arr

let private one = Values.int 1
let private two = Values.int 2
let private three = Values.int 3

let private assertElements (expected: string) (arr: JyroArray) =
    Assert.Equal(expected, arr.ToJson())

// ---------------------------------------------------------------- 30.4.1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-69")>]
let ``An array is changed in place through every reference`` () =
    let out =
        outOf (
            "func setOne(p)\n p[1] = 8\nend\n"
            + "var a = [1, 2, 3]\nvar b = a\n"
            + "a[0] = 9\na[3] = 4\nsetOne(a)\n"
            + "var o = {arr: a}\no.arr[2] = 7\n"
            + "Output.b = b\nOutput.a = a")
    Assert.Equal("[9,8,7,4]", Values.toJson (Values.prop "b" out))
    Assert.Equal("[9,8,7,4]", Values.toJson (Values.prop "a" out))
    Assert.Same(Values.prop "a" out, Values.prop "b" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 A script write to an array of the seed of Output lands in the result and leaves the host's own array as it was`` () =
    let r, arr = runOnHostArray [ one; two; three ] "Output.arr[0] = 5\nOutput.arr[3] = 6"
    Assert.True(Script.ok r, Script.describe r)
    Assert.Equal("[5,2,3,6]", (Script.output r |> Values.prop "arr").ToJson())
    assertElements "[1,2,3]" arr

// ---------------------------------------------------------------- 30.4.2

[<Theory>]
[<Trait("requirement", "JYRO-REQ-70")>]
[<InlineData("0", "10")>]
[<InlineData("1", "\"x\"")>]
[<InlineData("2", "null")>]
[<InlineData("3", "[1]")>]
[<InlineData("4", "{\"k\":1}")>]
[<InlineData("1.0", "\"x\"")>]
[<InlineData("1 * Power(1, 0.5)", "\"x\"")>]
[<InlineData("-0", "10")>]
let ``a[i] gives the element at an integer position`` (index: string, json: string) =
    let out = outOf ("var a = [10, \"x\", null, [1], {k: 1}]\nOutput.r = [a[" + index + "]]")
    Assert.Equal("[" + json + "]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-70")>]
let ``a[i] gives the stored container itself`` () =
    let out = outOf "var a = [10, [1], {k: 1}]\nvar e = a[1]\ne[0] = 5\nvar o = a[2]\no.k = 6\nOutput.a = a"
    Assert.Equal("[10,[5],{\"k\":6}]", Values.toJson (Values.prop "a" out))

// ---------------------------------------------------------------- 30.4.3

[<Theory>]
[<Trait("requirement", "JYRO-REQ-71")>]
[<InlineData("[1, 2, 3]", "3")>]
[<InlineData("[1, 2, 3]", "100")>]
[<InlineData("[1, 2, 3]", "-1")>]
[<InlineData("[1, 2, 3]", "-100")>]
[<InlineData("[]", "0")>]
[<InlineData("[]", "-1")>]
[<InlineData("[1, 2, 3]", "79228162514264337593543950335")>]
[<InlineData("[1, 2, 3]", "-79228162514264337593543950335")>]
[<InlineData("[1, 2, 3]", "1e300")>]
let ``a[i] gives null for an integer outside the array`` (array: string, index: string) =
    let out = outOf ("var a = " + array + "\nOutput.r = [a[" + index + "]]")
    Assert.Equal("[null]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.4.4

[<Theory>]
[<Trait("requirement", "JYRO-REQ-72")>]
[<InlineData("\"0\"")>]
[<InlineData("\"x\"")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("[0]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``a[i] gives null when i is not a number`` (index: string) =
    let out = outOf ("var i = " + index + "\nvar a = [1, 2, 3]\nvar e = []\nOutput.r = [a[i], e[i]]")
    Assert.Equal("[null,null]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.4.5

[<Theory>]
[<Trait("requirement", "JYRO-REQ-73")>]
[<InlineData("0", "9", "[9,2,3]")>]
[<InlineData("2", "\"z\"", "[1,2,\"z\"]")>]
[<InlineData("1", "null", "[1,null,3]")>]
[<InlineData("1.0", "5", "[1,5,3]")>]
[<InlineData("1 * Power(1, 0.5)", "6", "[1,6,3]")>]
let ``a[i] = v replaces the element at an integer position`` (index: string, value: string, expected: string) =
    let out = outOf ("var a = [1, 2, 3]\na[" + index + "] = " + value + "\nOutput.a = a\nOutput.n = Length(a)")
    Assert.Equal(expected, Values.toJson (Values.prop "a" out))
    assertNum 3M (Values.prop "n" out)

// ---------------------------------------------------------------- 30.4.6

[<Fact>]
[<Trait("requirement", "JYRO-REQ-74")>]
let ``a[i] = v at i equal to the length appends`` () =
    let out =
        outOf (
            "var a = []\nvar b = a\n"
            + "a[0] = 1\nOutput.n1 = Length(a)\n"
            + "a[1] = 2\na[Length(a)] = 3\na[3.0] = 4\n"
            + "Output.b = b\nOutput.n = Length(a)")
    assertNum 1M (Values.prop "n1" out)
    Assert.Equal("[1,2,3,4]", Values.toJson (Values.prop "b" out))
    assertNum 4M (Values.prop "n" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-74")>]
let ``a[i] = v at an inexact index equal to the length appends`` () =
    let out = outOf ("var a = [1]\na[1 * " + I1 + "] = 2\nOutput.a = a")
    Assert.Equal("[1,2]", Values.toJson (Values.prop "a" out))

// ---------------------------------------------------------------- 30.4.7

[<Theory>]
[<Trait("requirement", "JYRO-REQ-75")>]
[<InlineData("4")>]
[<InlineData("100")>]
[<InlineData("-1")>]
[<InlineData("-100")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("1e300")>]
let ``a[i] = v outside 0 to Length raises IndexOutOfRange and leaves the array`` (index: string) =
    let r, arr = runOnHostArray [ one; two; three ] ("Output.arr[" + index + "] = 9")
    Script.expectError MessageCode.IndexOutOfRange r
    assertElements "[1,2,3]" arr
    Assert.Equal(3, arr.Count)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-75")>]
[<InlineData("1")>]
[<InlineData("-1")>]
let ``a[i] = v on an empty array outside 0 raises IndexOutOfRange`` (index: string) =
    let r, arr = runOnHostArray [] ("Output.arr[" + index + "] = 9")
    Script.expectError MessageCode.IndexOutOfRange r
    Assert.Equal(0, arr.Count)

// ---------------------------------------------------------------- 30.4.8

[<Theory>]
[<Trait("requirement", "JYRO-REQ-76")>]
[<InlineData("\"0\"")>]
[<InlineData("\"x\"")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("[0]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``a[i] = v raises InvalidIndexType when i is not a number`` (index: string) =
    let r, arr = runOnHostArray [ one; two; three ] ("var i = " + index + "\nOutput.arr[i] = 9")
    Script.expectError MessageCode.InvalidIndexType r
    assertElements "[1,2,3]" arr

// ---------------------------------------------------------------- 30.4.9

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2149")>]
[<InlineData("0")>]
[<InlineData("2")>]
[<InlineData("3")>]
[<InlineData("4")>]
[<InlineData("99")>]
[<InlineData("2147483648")>]
[<InlineData("-1")>]
[<InlineData("-0.5")>]
[<InlineData("\"x\"")>]
[<InlineData("\"0\"")>]
[<InlineData("1.5")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("[0]")>]
[<InlineData("{}")>]
[<InlineData("x => x")>]
let ``JYRO-REQ-2149 A write to a read-only array raises InputValueIsReadOnly and no other error, whatever the index`` (index: string) =
    let input = Values.json "{\"arr\":[1,2,3]}"
    let r = Script.runInput input ("var a = Input.arr\na[" + index + "] = 9")
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal("{\"arr\":[1,2,3]}", Values.toJson input)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2149")>]
[<InlineData("0")>]
[<InlineData("3")>]
[<InlineData("4")>]
[<InlineData("99")>]
[<InlineData("-1")>]
let ``JYRO-REQ-2149 A host write to a read-only array reports the read-only state before the index`` (index: string) =
    let input = Values.json "{\"arr\":[1,2,3]}"
    let r = Script.runInput input "Output.r = 1"
    Assert.True(Script.ok r, Script.describe r)
    let arr = Values.prop "arr" input :?> JyroArray
    // SetIndex is not public (JYRO-REQ-1876): the host writes through the indexer, which takes an int.
    throwsCode MessageCode.InputValueIsReadOnly (fun () -> arr.[int index] <- Values.int 9)
    Assert.Equal("[1,2,3]", Values.toJson arr)

// ---------------------------------------------------------------- 30.4.10

[<Theory>]
[<Trait("requirement", "JYRO-REQ-78")>]
[<InlineData("0.5")>]
[<InlineData("1.5")>]
[<InlineData("-0.5")>]
[<InlineData("99.5")>]
[<InlineData("0.1 * Power(1, 0.5)")>]
[<InlineData("1e-28")>]
let ``JYRO-REQ-78 a[i] raises IntegerIndexRequired when i is not an integer`` (index: string) =
    let r = Script.run ("var a = [1, 2, 3]\nOutput.r = a[" + index + "]")
    Script.expectError (integerIndexRequired ()) r
    Assert.DoesNotContain(MessageCode.IntegerRequired, Script.codes r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-78")>]
[<InlineData("0.5")>]
[<InlineData("3.5")>]
[<InlineData("-0.5")>]
[<InlineData("99.5")>]
[<InlineData("2.5 * Power(1, 0.5)")>]
let ``JYRO-REQ-78 a[i] = v raises IntegerIndexRequired when i is not an integer and leaves the array`` (index: string) =
    let r, arr = runOnHostArray [ one; two; three ] ("Output.arr[" + index + "] = 9")
    Script.expectError (integerIndexRequired ()) r
    Assert.DoesNotContain(MessageCode.IntegerRequired, Script.codes r)
    assertElements "[1,2,3]" arr

/// The clause's examples, and other exact fractions written plainly, read and
/// written: exactly one argument, the index (160.7.16).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-78")>]
[<InlineData("Output.r = [1, 2][0.5]", "0.5")>]
[<InlineData("var a = [1, 2]\na[0.5] = 1", "0.5")>]
[<InlineData("var a = [1, 2, 3]\nOutput.r = a[1.5]", "1.5")>]
[<InlineData("var a = [1, 2, 3]\nOutput.r = a[-0.5]", "-0.5")>]
[<InlineData("var a = [1, 2, 3]\na[99.5] = 1", "99.5")>]
[<InlineData("var a = []\na[2.25] = 1", "2.25")>]
let ``JYRO-REQ-78 the IntegerIndexRequired of a[i] and a[i] = v has exactly one argument, i`` (source: string, index: string) =
    let compiled = Script.compile source
    Assert.True(Script.ok compiled, Script.describe compiled)
    expectErrorArgs (integerIndexRequired ()) [ index ] (Script.run source)

/// 30.9 Truthiness.
module RealTree.Jyro.Spec.Tests.P30.S30_09_Truthiness

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

/// Each boolean context of 30.9.1, as a script that sets Output.r to whether
/// the context treated `v` as true.
let private contexts =
    [ "if", "if v then Output.r = true else Output.r = false end"
      "elseif", "if false then Output.r = 0 elseif v then Output.r = true else Output.r = false end"
      "while", "var r = false\nvar c = v\nwhile c do\n r = true\n c = false\nend\nOutput.r = r"
      "and (left)", "Output.r = (v and \"T\") == \"T\""
      "and (right, both)", "Output.r = not not (true and v)"
      "or (left)", "Output.r = (v or \"F\") != \"F\""
      "or (right)", "Output.r = not not (false or v)"
      "not", "Output.r = not (not v)"
      "? :", "Output.r = v ? true : false" ]

let private values =
    [ "null", false
      "false", false
      "0", false
      "-0", false
      "0.0", false
      "0 * Power(2, 0.5)", false
      "0 * -Power(2, 0.5)", false
      "\"\"", false
      "true", true
      "1", true
      "-1", true
      "0.0000000000000000000000000001", true
      "Power(2, -1074 * Power(1, 0.5))", true
      "\"0\"", true
      "\" \"", true
      "\"false\"", true
      "[]", true
      "{}", true
      "[0]", true
      "{a: null}", true
      "x => x", true ]

let contextValueData () : obj[] seq =
    seq {
        for (c, _) in contexts do
            for (v, t) in values do
                yield [| box c; box v; box t |]
    }

let valueData () : obj[] seq = values |> Seq.map (fun (v, t) -> [| box v; box t |])

let private context name = contexts |> List.find (fun (n, _) -> n = name) |> snd

// ---------------------------------------------------------------- 30.9.1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-102")>]
[<MemberData(nameof contextValueData)>]
let ``JYRO-REQ-102 Every boolean context converts its value as 30.9.2 gives`` (ctx: string, value: string, truthy: bool) =
    let r = rOf ("var v = " + value + "\n" + context ctx)
    Assert.Equal(truthy, Values.asBool r)

/// Which operand `and` and `or` give is Part 40's rule (40.5), not 30.9.1's,
/// so this test names no requirement of Part 30.
[<Fact>]
let ``and and or give an operand itself while if takes the branch`` () =
    let out = outOf "Output.r = [0 and 1, \"\" or 2, [] and 3, null or 0]"
    Assert.Equal("[0,2,3,0]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.9.2

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1951")>]
[<MemberData(nameof valueData)>]
let ``JYRO-REQ-1951 The conversion gives false only for null, false, zero and the empty string`` (value: string, truthy: bool) =
    // Read through boolean contexts of 30.9.1 only; ToBoolean is JYRO-REQ-1092's (another work order).
    let out =
        outOf (
            "var v = " + value + "\n"
            + "if v then Output.i = true else Output.i = false end\n"
            + "Output.c = v ? true : false\nOutput.n = not v")
    Assert.Equal(truthy, Values.asBool (Values.prop "i" out))
    Assert.Equal(truthy, Values.asBool (Values.prop "c" out))
    Assert.Equal(not truthy, Values.asBool (Values.prop "n" out))

// ---------------------------------------------------------------- 30.9.3

let private hardValues =
    [ "cyclic array", "var v = [1]\nv[1] = v\n"
      "cyclic object", "var v = {}\nv.self = v\n"
      "deep", "var v = []\nvar i = 0\nwhile i < 20000 do\n v = [v]\n i = i + 1\nend\n"
      "function", "var v = x => x\n"
      "long string", "var v = PadLeft(\"\", 1000000, \"x\")\n"
      "largest double", "var v = 1.7976931348623157e308 * Power(1, 0.5)\n"
      "largest exact", "var v = 79228162514264337593543950335\n"
      "input object", "var v = Input\n" ]

let hardData () : obj[] seq =
    seq {
        for (h, _) in hardValues do
            for (c, _) in contexts do
                yield [| box h; box c |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1780")>]
[<MemberData(nameof hardData)>]
let ``JYRO-REQ-1780 Truthiness conversion raises no error for any value`` (valueName: string, ctx: string) =
    let make = hardValues |> List.find (fun (n, _) -> n = valueName) |> snd
    let r = Script.runWith bigLimits (make + context ctx + "\nOutput.b = ToBoolean(v)")
    let out = Script.output r
    Assert.True(Values.asBool (Values.prop "r" out))
    Assert.True(Values.asBool (Values.prop "b" out))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1780")>]
[<MemberData(nameof contextValueData)>]
let ``JYRO-REQ-1780 No boolean context raises an error for a null, a boolean, a number, a string, an array, an object or a function`` (ctx: string, value: string, _truthy: bool) =
    let r = Script.run ("var v = " + value + "\n" + context ctx)
    Assert.True(Script.ok r, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1780")>]
let ``JYRO-REQ-1780 A value nested deeper than the bound converts to true`` () =
    let depth = maxDepth () + 5
    let r =
        Script.runWith bigLimits (
            "var v = []\nvar i = 0\nwhile i < " + string depth + " do\n v = {k: v}\n i = i + 1\nend\n"
            + "Output.r = [v ? 1 : 2, not v, ToBoolean(v)]")
    Assert.Equal("[1,false,true]", Values.toJson (Values.prop "r" (Script.output r)))

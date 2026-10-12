/// 20.8 - boolean, null, array and object literals.
module RealTree.Jyro.Spec.Tests.S20_08_Literals

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 20.8.1 ------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-40")>]
let ``Literal_TrueFalseNull_AreTheBooleanAndNullLiterals`` () =
    let o = Script.outputOf "Output.t = true\nOutput.f = false\nOutput.n = null"
    let t = o.GetProperty "t"
    let f = o.GetProperty "f"
    Assert.Equal(JyroValueType.Boolean, t.ValueType)
    Assert.True(Values.asBool t)
    Assert.Equal(JyroValueType.Boolean, f.ValueType)
    Assert.False(Values.asBool f)
    Assert.True(has "n" o, "Output.n was not set")
    Assert.Equal(JyroValueType.Null, (o.GetProperty "n").ValueType)
    assertJson "{\"t\":true,\"f\":false,\"n\":null}" o

[<Theory>]
[<InlineData("true == true", "true")>]
[<InlineData("true != false", "true")>]
[<InlineData("false == false", "true")>]
[<InlineData("null == null", "true")>]
[<InlineData("true is boolean", "true")>]
[<InlineData("false is boolean", "true")>]
[<InlineData("null is null", "true")>]
[<Trait("requirement", "JYRO-REQ-40")>]
let ``Literal_TrueFalseNull_HaveTheirMeaningInExpressions`` (expr: string, expected: string) =
    outV ("Output.v = " + expr) |> assertJson expected

[<Theory>]
[<InlineData("True")>]
[<InlineData("FALSE")>]
[<InlineData("False")>]
[<InlineData("Null")>]
[<InlineData("NULL")>]
[<InlineData("TRUE")>]
[<Trait("requirement", "JYRO-REQ-40")>]
let ``Literal_OtherCaseSpelling_IsNotALiteral_ButAnUndeclaredName`` (word: string) =
    rejectedWithCode MessageCode.UndeclaredVariable ("Output.v = " + word)

[<Theory>]
[<InlineData("True")>]
[<InlineData("FALSE")>]
[<InlineData("Null")>]
[<Trait("requirement", "JYRO-REQ-40")>]
let ``Literal_OtherCaseSpelling_CanBeDeclaredAsAVariable`` (word: string) =
    outV (sprintf "var %s = 1\nOutput.v = %s" word word) |> assertJson "1"

// -- 20.8.2 ------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ArrayLiteral_ElementsAreInOrderOfPosition`` () =
    let items = Values.asList (outV "Output.v = [3, 1, 2]")
    Assert.Equal(3, items.Length)
    Assert.Equal<float list>([ 3.0; 1.0; 2.0 ], items |> List.map Values.asFloat)

[<Theory>]
[<InlineData("[]", "[]")>]
[<InlineData("[1, \"a\", true, null, [2], {}]", "[1,\"a\",true,null,[2],{}]")>]
[<InlineData("[[1, 2], [3]]", "[[1,2],[3]]")>]
[<InlineData("[1 + 1, 2 * 3]", "[2,6]")>]
[<InlineData("[\n  1,\n  2\n]", "[1,2]")>]
[<InlineData("[ 1 , 2 ]", "[1,2]")>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ArrayLiteral_FormsGiveTheirArray`` (literal: string, expected: string) =
    outV ("Output.v = " + literal) |> assertJson expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ArrayLiteral_LengthIsTheNumberOfExpressions_AndElementIIsExpressionIPlusOne`` () =
    let v = outV "var n = 7\nOutput.v = [n, n + 1, [n], \"x\"]"
    let items = Values.asList v
    Assert.Equal(4, items.Length)
    assertJson "7" items.[0]
    assertJson "8" items.[1]
    assertJson "[7]" items.[2]
    assertJson "\"x\"" items.[3]
    Assert.Equal(0, (Values.asList (outV "Output.v = []")).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ArrayLiteral_ElementExpressionsAreEvaluatedInOrderOfPosition`` () =
    let log =
        Host.func "Trace" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args -> args.Head)
    let r = Script.runWith { Script.defaults with Functions = [ log.AsFunction ] } "var x = 5\nOutput.v = [Trace(1), x, Trace(2), Trace(3)]"
    Script.output r |> Values.prop "v" |> assertJson "[1,5,2,3]"
    Assert.Equal<float list>([ 1.0; 2.0; 3.0 ], log.Calls |> List.map (fun a -> Values.asFloat a.Head))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ObjectLiteral_HoldsEachKeyWithItsValue_KeysIdentifierOrString`` () =
    let o = outV "Output.v = { a: 1, \"b c\": 2, 'd': 3 }"
    assertJson "{\"a\":1,\"b c\":2,\"d\":3}" o
    Assert.Equal(3, (o :?> JyroObject).Count)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ObjectLiteral_KeyThatIsAReservedWord_IsThatWord`` () =
    let o = outV "Output.v = { if: 1, \"a b\": 2 }"
    Assert.Equal(2, (o :?> JyroObject).Count)
    assertJson "1" (Values.prop "if" o)
    assertJson "2" (Values.prop "a b" o)

[<Theory>]
[<InlineData("{ a: 1, a: 2 }")>]
[<InlineData("{ a: 1, \"a\": 2 }")>]
[<InlineData("{ \"a\": 1, a: 2 }")>]
[<InlineData("{ a: 0, a: 1, a: 2 }")>]
[<InlineData("{ \"a\": 1, 'a': 2 }")>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ObjectLiteral_KeyNamedTwice_HoldsOnePropertyWithTheLastValue`` (literal: string) =
    let o = outV ("Output.v = " + literal)
    Assert.Equal(1, (o :?> JyroObject).Count)
    assertJson "{\"a\":2}" o

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ObjectLiteral_KeyNamedTwice_StillEvaluatesTheEarlierExpression`` () =
    let out = Script.outputOf "var n = 0\nOutput.v = { a: n++, a: 5 }\nOutput.n = n"
    assertJson "{\"a\":5}" (Values.prop "v" out)
    Assert.Equal(1, (Values.prop "v" out :?> JyroObject).Count)
    assertJson "1" (Values.prop "n" out)
    // and in order of position: the earlier expression first.
    let log =
        Host.func "Trace" [ Parameter.Required("v", ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args -> args.Head)
    let r = Script.runWith { Script.defaults with Functions = [ log.AsFunction ] } "Output.v = { a: Trace(1), b: Trace(2), a: Trace(3) }"
    assertJson "{\"a\":3,\"b\":2}" (Script.output r |> Values.prop "v")
    Assert.Equal<float list>([ 1.0; 2.0; 3.0 ], log.Calls |> List.map (fun a -> Values.asFloat a.Head))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ObjectLiteral_EmptyLiteral_HoldsNoProperty`` () =
    Assert.Equal(0, (outV "Output.v = {}" :?> JyroObject).Count)

[<Theory>]
[<InlineData("{}", "{}")>]
[<InlineData("{ }", "{}")>]
[<InlineData("{ a: { b: [1] } }", "{\"a\":{\"b\":[1]}}")>]
[<InlineData("{\n  a: 1,\n  b: 2\n}", "{\"a\":1,\"b\":2}")>]
[<InlineData("{ a: 1 + 1 }", "{\"a\":2}")>]
[<Trait("requirement", "JYRO-REQ-2151")>]
let ``JYRO-REQ-2151 ObjectLiteral_FormsGiveTheirObject`` (literal: string, expected: string) =
    outV ("Output.v = " + literal) |> assertJson expected

// -- 20.8.3 ------------------------------------------------------------------

[<Theory>]
[<InlineData("[1, 2,]")>]
[<InlineData("[1,]")>]
[<InlineData("{ a: 1, }")>]
[<InlineData("{ a: 1, b: 2, }")>]
[<InlineData("[1, 2, # comment\n]")>]
[<InlineData("{ a: 1, # comment\n}")>]
[<InlineData("[[1,], 2]")>]
[<InlineData("{ a: [1,] }")>]
[<InlineData("[,]")>]
[<InlineData("{,}")>]
[<Trait("requirement", "JYRO-REQ-42")>]
let ``JYRO-REQ-42 TrailingComma_InArrayOrObjectLiteral_IsParseErrorAndNothingRuns`` (literal: string) =
    parseError ("Output.v = " + literal)

[<Theory>]
[<InlineData("[1, 2]", "[1,2]")>]
[<InlineData("{ a: 1, b: 2 }", "{\"a\":1,\"b\":2}")>]
[<InlineData("[]", "[]")>]
[<InlineData("{}", "{}")>]
[<Trait("requirement", "JYRO-REQ-42")>]
let ``JYRO-REQ-42 TrailingComma_Control_LiteralsWithoutTrailingComma_Compile`` (literal: string, expected: string) =
    outV ("Output.v = " + literal) |> assertJson expected

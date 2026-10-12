/// 30.1 Value types.
module RealTree.Jyro.Spec.Tests.P30.S30_01_ValueTypes

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

let private typeNames = [ "null"; "boolean"; "number"; "string"; "array"; "object"; "function" ]

let private hostValue =
    Host.func "HostValue" [] ParameterType.Any FunctionEffect.Pure (fun _ -> Values.obj [ "k", Values.int 1 ])

/// Checks one value `v` (declared by `decl`) through TypeOf and the seven `is`.
let private checkKinds (options: RunOptions) (decl: string) (expected: string) =
    let source =
        decl + "\n"
        + "Output.t = TypeOf(v)\n"
        + "Output.k = [v is null, v is boolean, v is number, v is string, v is array, v is object, v is function]\n"
        + "Output.v = v"
    let out = Script.output (Script.runWith options source)
    Assert.Equal(expected, Values.asString (Values.prop "t" out))
    let flags = Values.asList (Values.prop "k" out) |> List.map Values.asBool
    Assert.Equal(1, flags |> List.filter id |> List.length)
    Assert.True(flags.[List.findIndex ((=) expected) typeNames], sprintf "`v is %s` should be the true one" expected)
    let hostType = (Values.prop "v" out).ValueType
    let expectedHost =
        match expected with
        | "null" -> JyroValueType.Null
        | "boolean" -> JyroValueType.Boolean
        | "number" -> JyroValueType.Number
        | "string" -> JyroValueType.String
        | "array" -> JyroValueType.Array
        | "object" -> JyroValueType.Object
        | _ -> JyroValueType.Function
    Assert.Equal(expectedHost, hostType)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1943")>]
[<InlineData("var v = null", "null")>]
[<InlineData("var v = true", "boolean")>]
[<InlineData("var v = false", "boolean")>]
[<InlineData("var v = 1", "number")>]
[<InlineData("var v = Power(2, 0.5)", "number")>]
[<InlineData("var v = \"\"", "string")>]
[<InlineData("var v = \"x\"", "string")>]
[<InlineData("var v = []", "array")>]
[<InlineData("var v = [1]", "array")>]
[<InlineData("var v = {}", "object")>]
[<InlineData("var v = {a: 1}", "object")>]
[<InlineData("var v = x => x", "function")>]
let ``Every script-made value has exactly one of the seven types`` (decl: string, expected: string) =
    checkKinds Script.defaults decl expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1943")>]
[<InlineData("null", "null")>]
[<InlineData("true", "boolean")>]
[<InlineData("1.5", "number")>]
[<InlineData("\"s\"", "string")>]
[<InlineData("[1]", "array")>]
[<InlineData("{\"a\":1}", "object")>]
let ``Every value read from Input has exactly one of the seven types`` (json: string, expected: string) =
    checkKinds { Script.defaults with Input = JsonInput ("{\"a\":" + json + "}") } "var v = Input.a" expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1943")>]
let ``A value a host function returns has exactly one of the seven types`` () =
    checkKinds { Script.defaults with Functions = [ hostValue.AsFunction ] } "var v = HostValue()" "object"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1943")>]
let ``The host value type enumeration has exactly the seven types`` () =
    let names = Enum.GetNames(typeof<JyroValueType>) |> Array.sort
    Assert.Equal<string[]>([| "Array"; "Boolean"; "Function"; "Null"; "Number"; "Object"; "String" |], names)

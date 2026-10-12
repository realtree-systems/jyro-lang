namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// Truthiness table per the language reference:
///   null, false, 0, "" are falsy; everything else (including empty
///   arrays and empty objects) is truthy.
module Truthiness =

    let private truthinessOf (literal: string) : string =
        let src =
            sprintf
                """
var v = %s
if v then
  Output.r = "t"
else
  Output.r = "f"
end"""
                literal
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Runner.prop "r" output |> Runner.asString

    [<Theory>]
    [<InlineData("null", "f")>]
    [<InlineData("false", "f")>]
    [<InlineData("0", "f")>]
    [<InlineData("\"\"", "f")>]
    [<InlineData("true", "t")>]
    [<InlineData("1", "t")>]
    [<InlineData("-1", "t")>]
    [<InlineData("0.5", "t")>]
    [<InlineData("\"x\"", "t")>]
    [<InlineData("\" \"", "t")>]
    [<InlineData("[]", "t")>]
    [<InlineData("{}", "t")>]
    [<InlineData("[0]", "t")>]
    [<InlineData("[null]", "t")>]
    let ``truthiness matches the spec`` (literal: string) (expected: string) =
        Assert.Equal(expected, truthinessOf literal)

    [<Theory>]
    [<InlineData("null")>]
    [<InlineData("false")>]
    [<InlineData("0")>]
    [<InlineData("\"\"")>]
    [<InlineData("true")>]
    [<InlineData("1")>]
    [<InlineData("\"x\"")>]
    [<InlineData("[]")>]
    [<InlineData("{}")>]
    let ``not v agrees with the branch an if would pick`` (literal: string) =
        let viaIf = truthinessOf literal
        let viaNot =
            let src = sprintf "Output.r = not (%s) ? \"f\" : \"t\"" literal
            let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
            Runner.prop "r" output |> Runner.asString
        Assert.Equal(viaIf, viaNot)

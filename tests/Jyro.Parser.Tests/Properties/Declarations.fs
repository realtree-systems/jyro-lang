namespace RealTree.Jyro.Tests.Properties

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests.ParseShape

/// Targeted parse checks for top-level declarations (func, union).
/// Whole-program round-trip coverage lives in ProgramProperties.
module DeclarationProperties =

    [<Fact>]
    let ``zero-parameter func parses`` () =
        let src = "func noop()\n  return\nend"
        match Parser.parse src with
        | ParseSuccess { Statements = [ FuncDef("noop", [], [ Return(None, _) ], _) ] } -> ()
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``func with typed params and default parses`` () =
        let src = "func add(a: number, b: number = 0)\n  return a\nend"
        match Parser.parse src with
        | ParseSuccess { Statements = [ FuncDef("add", [ p1; p2 ], _, _) ] } ->
            Assert.Equal("a", p1.Name)
            Assert.Equal(Some NumberType, p1.TypeHint)
            Assert.Equal(None, p1.DefaultValue)
            Assert.Equal("b", p2.Name)
            Assert.Equal(Some NumberType, p2.TypeHint)
            Assert.True(p2.DefaultValue.IsSome)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``union with zero and multi-field variants parses`` () =
        let src = "union Shape Circle(r: number) Square(side: number) Point end"
        match Parser.parse src with
        | ParseSuccess { Statements = [ UnionDef("Shape", variants, _) ] } ->
            Assert.Equal(3, List.length variants)
            let names = variants |> List.map (fun v -> v.Name)
            Assert.Equal<string list>([ "Circle"; "Square"; "Point" ], names)
            Assert.Equal(1, List.length variants.[0].Fields)
            Assert.Equal(1, List.length variants.[1].Fields)
            Assert.Equal(0, List.length variants.[2].Fields)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``match expression with multiple cases parses`` () =
        let src = "var r = match s do case A then 1 case B then 2 end"
        match Parser.parse src with
        | ParseSuccess { Statements = [ VarDecl(_, _, Some(MatchExpr(_, cases, _)), _) ] } ->
            Assert.Equal(2, List.length cases)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Fact>]
    let ``match statement with bindings parses`` () =
        let src = "match s do\n  case Circle(r) then\n    var a = r\n  case Square(side) then\n    var a = side\nend"
        match Parser.parse src with
        | ParseSuccess { Statements = [ Match(_, cases, _) ] } ->
            Assert.Equal(2, List.length cases)
            Assert.Equal<string list>([ "r" ], cases.[0].Bindings)
            Assert.Equal<string list>([ "side" ], cases.[1].Bindings)
        | other -> Assert.Fail(sprintf "%A" other)

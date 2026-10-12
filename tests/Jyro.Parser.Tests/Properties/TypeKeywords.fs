namespace RealTree.Jyro.Tests.Properties

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests.ParseShape

/// Every `JyroType` case must be spellable in every position that accepts a
/// type annotation, and must survive `print` -> `parse`.
///
/// `any` was missing from the lexer's `typeKeyword` while `Printer` emitted
/// "any" for `AnyType`, so any AST carrying `AnyType` - reachable via a
/// host-supplied AST or a `.jyrx` binary, both of which the compiler accepts -
/// printed to source the parser rejected. These tests pin the grammar and the
/// printer to the same set of keywords.
module TypeKeywordProperties =

    let private pos = { Line = 1; Column = 1; StartIndex = 0; EndIndex = 0 }

    let private typeOf (kw: string) : JyroType =
        match kw with
        | "any" -> AnyType
        | "number" -> NumberType
        | "string" -> StringType
        | "boolean" -> BooleanType
        | "object" -> ObjectType
        | "array" -> ArrayType
        | "null" -> NullType
        | "function" -> FunctionType
        | other -> failwithf "unknown type keyword '%s'" other

    let private parseOne (src: string) : Stmt =
        match Parser.parse src with
        | ParseSuccess { Statements = [ stmt ] } -> stmt
        | other -> failwithf "expected exactly one statement from %s; got %A" src other

    // -- Every keyword parses in every type position ----------------------

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("number")>]
    [<InlineData("string")>]
    [<InlineData("boolean")>]
    [<InlineData("object")>]
    [<InlineData("array")>]
    [<InlineData("null")>]
    [<InlineData("function")>]
    let ``type keyword parses in a var declaration`` (kw: string) =
        match parseOne (sprintf "var x: %s = 1" kw) with
        | VarDecl("x", hint, _, _) -> Assert.Equal(Some(typeOf kw), hint)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("number")>]
    [<InlineData("string")>]
    [<InlineData("boolean")>]
    [<InlineData("object")>]
    [<InlineData("array")>]
    [<InlineData("null")>]
    [<InlineData("function")>]
    let ``type keyword parses in an assignment`` (kw: string) =
        match parseOne (sprintf "x: %s = 1" kw) with
        | Assignment(_, Assign, _, hint, _) -> Assert.Equal(Some(typeOf kw), hint)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("number")>]
    [<InlineData("string")>]
    [<InlineData("boolean")>]
    [<InlineData("object")>]
    [<InlineData("array")>]
    [<InlineData("null")>]
    [<InlineData("function")>]
    let ``type keyword parses on a function parameter`` (kw: string) =
        match parseOne (sprintf "func F(p: %s)\n  return 1\nend" kw) with
        | FuncDef("F", [ p ], _, _) -> Assert.Equal(Some(typeOf kw), p.TypeHint)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("number")>]
    [<InlineData("string")>]
    [<InlineData("boolean")>]
    [<InlineData("object")>]
    [<InlineData("array")>]
    [<InlineData("null")>]
    [<InlineData("function")>]
    let ``type keyword parses on a union variant field`` (kw: string) =
        match parseOne (sprintf "union U V(f: %s) end" kw) with
        | UnionDef("U", [ v ], _) ->
            Assert.Equal<(string * JyroType option) list>([ ("f", Some(typeOf kw)) ], v.Fields)
        | other -> Assert.Fail(sprintf "%A" other)

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("number")>]
    [<InlineData("string")>]
    [<InlineData("boolean")>]
    [<InlineData("object")>]
    [<InlineData("array")>]
    [<InlineData("null")>]
    [<InlineData("function")>]
    let ``type keyword parses in a type check`` (kw: string) =
        match parseOne (sprintf "x is %s" kw) with
        | ExprStmt(TypeCheck(Identifier("x", _), t, false, _), _) -> Assert.Equal(typeOf kw, t)
        | other -> Assert.Fail(sprintf "%A" other)

    // -- print -> parse round-trip for a hand-built AST -------------------
    // The regression this file exists for: these ASTs are legal and reachable
    // without ever going through the parser, so the printer's output for them
    // has to be re-parseable.

    let private one = Literal(JyroNumber(1m) :> JyroValue, pos)

    let private roundTripPositions (t: JyroType) : (string * Stmt) list =
        [ "var declaration", VarDecl("x", Some t, Some one, pos)
          "assignment", Assignment(Identifier("x", pos), Assign, one, Some t, pos)
          "function parameter",
              FuncDef("F", [ { Name = "p"; TypeHint = Some t; DefaultValue = None } ],
                      [ Return(Some one, pos) ], pos)
          "union variant field",
              UnionDef("U", [ RealTree.Jyro.Tests.Generators.mkVariant "V" [ ("f", Some t) ] ], pos)
          "type check", ExprStmt(TypeCheck(Identifier("x", pos), t, false, pos), pos) ]

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("number")>]
    [<InlineData("string")>]
    [<InlineData("boolean")>]
    [<InlineData("object")>]
    [<InlineData("array")>]
    [<InlineData("null")>]
    [<InlineData("function")>]
    let ``printed type annotation re-parses in every position`` (kw: string) =
        for (position, stmt) in roundTripPositions (typeOf kw) do
            let printed = Printer.printStatement stmt
            match Parser.parse printed with
            | ParseSuccess _ -> ()
            | ParseFailure(msg, line, col) ->
                Assert.Fail(
                    sprintf "%s: Printer emitted %s which failed to re-parse at %d:%d - %s"
                        position (printed.Trim()) line col msg)

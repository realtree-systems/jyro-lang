namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// `x is T` must match the actual runtime type of x, and exactly one
/// type keyword matches a given value.
module TypeCheck =

    let private typeKeywords =
        [ "number"; "string"; "boolean"; "object"; "array"; "null" ]

    let private evalIsForEveryType (literal: string) : (string * bool) list =
        let assignments =
            typeKeywords
            |> List.mapi (fun i kw -> sprintf "Output.r%d = (%s) is %s" i literal kw)
            |> String.concat "\n"
        let output = Runner.expectSuccess assignments (JyroObject() :> JyroValue)
        typeKeywords
        |> List.mapi (fun i kw ->
            kw, Runner.prop (sprintf "r%d" i) output |> Runner.asBool)

    [<Theory>]
    [<InlineData("42", "number")>]
    [<InlineData("3.14", "number")>]
    [<InlineData("0", "number")>]
    [<InlineData("\"hello\"", "string")>]
    [<InlineData("\"\"", "string")>]
    [<InlineData("true", "boolean")>]
    [<InlineData("false", "boolean")>]
    [<InlineData("null", "null")>]
    [<InlineData("[]", "array")>]
    [<InlineData("[1, 2, 3]", "array")>]
    [<InlineData("{}", "object")>]
    [<InlineData("{a: 1}", "object")>]
    let ``is keyword matches exactly one type per value`` (literal: string) (expected: string) =
        let results = evalIsForEveryType literal
        let matching = results |> List.filter snd |> List.map fst
        Assert.Equal<string list>([ expected ], matching)

    [<Theory>]
    [<InlineData("42", "number")>]
    [<InlineData("\"hi\"", "string")>]
    [<InlineData("true", "boolean")>]
    [<InlineData("null", "null")>]
    [<InlineData("[]", "array")>]
    [<InlineData("{}", "object")>]
    let ``is T and is-not OTHER agree for every other type`` (literal: string) (trueType: string) =
        for kw in typeKeywords do
            let src =
                if kw = trueType then
                    sprintf "Output.r = (%s) is %s" literal kw
                else
                    sprintf "Output.r = (%s) is not %s" literal kw
            let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
            Assert.True(
                Runner.prop "r" output |> Runner.asBool,
                sprintf "Failed for %s is[-not] %s" literal kw)

    // -- `function` is a type keyword like any other ----------------------
    // Clauses 40.6 and 40.10 both require `is function`, but the grammar had
    // no `function` keyword and `JyroType` had no case for it, so the operator
    // the spec promised twice would not parse.

    [<Fact>]
    let ``a lambda satisfies is function and nothing else`` () =
        let src =
            typeKeywords @ [ "function" ]
            |> List.mapi (fun i kw -> sprintf "Output.r%d = f is %s" i kw)
            |> String.concat "\n"
        let output = Runner.expectSuccess ("var f = x => x + 1\n" + src) (JyroObject() :> JyroValue)
        let matching =
            typeKeywords @ [ "function" ]
            |> List.mapi (fun i kw -> kw, Runner.prop (sprintf "r%d" i) output |> Runner.asBool)
            |> List.filter snd
            |> List.map fst
        Assert.Equal<string list>([ "function" ], matching)

    [<Theory>]
    [<InlineData("42")>]
    [<InlineData("\"hi\"")>]
    [<InlineData("true")>]
    [<InlineData("null")>]
    [<InlineData("[]")>]
    [<InlineData("{}")>]
    let ``no non-lambda value satisfies is function`` (literal: string) =
        let output = Runner.expectSuccess (sprintf "Output.r = (%s) is function" literal) (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "r" output |> Runner.asBool)

    [<Fact>]
    let ``function is usable as a type hint`` () =
        let output = Runner.expectSuccess "var f: function = x => x + 1\nOutput.r = f is function" (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "r" output |> Runner.asBool)

    [<Fact>]
    let ``a function hint rejects a non-function value`` () =
        let codes = Runner.expectFailure "var f: function = 1\nOutput.r = 1" (JyroObject() :> JyroValue)
        Assert.Contains(MessageCode.InvalidType, codes)

    [<Theory>]
    [<InlineData("var function = 1\nOutput.r = function")>]
    [<InlineData("var any = 1\nOutput.r = any")>]
    let ``the contextual type keywords stay usable as identifiers`` (src: string) =
        // `any` and `function` are deliberately absent from `reservedKeywords`:
        // both are only ever read in a type position, so reserving them would
        // break scripts for no gain.
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "r" output |> Runner.asNumber)

    // -- `is any` evaluates its operand -----------------------------------
    // The answer is a constant, but the operand is not. Folding the whole
    // expression to `true` made `is any` the only operator in the language
    // that deletes its operand.

    [<Fact>]
    let ``is any is true for every value`` () =
        for literal in [ "42"; "\"hi\""; "true"; "null"; "[]"; "{}"; "x => x" ] do
            let output = Runner.expectSuccess (sprintf "Output.r = (%s) is any" literal) (JyroObject() :> JyroValue)
            Assert.True(Runner.prop "r" output |> Runner.asBool, sprintf "%s is any" literal)

    [<Fact>]
    let ``is any evaluates its operand for effect`` () =
        // Measured before the fix: n stayed 0.
        let output = Runner.expectSuccess "var n = 0\nvar t = n++ is any\nOutput.n = n" (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "n" output |> Runner.asNumber)

    [<Fact>]
    let ``is any propagates an error raised by its operand`` () =
        // Measured before the fix: `(1 / 0) is any` yielded true.
        let codes = Runner.expectFailure "Output.r = (1 / 0) is any" (JyroObject() :> JyroValue)
        Assert.Contains(MessageCode.DivisionByZero, codes)

    [<Fact>]
    let ``is not any is false for every value`` () =
        for literal in [ "42"; "null"; "[]" ] do
            let output = Runner.expectSuccess (sprintf "Output.r = (%s) is not any" literal) (JyroObject() :> JyroValue)
            Assert.False(Runner.prop "r" output |> Runner.asBool, sprintf "%s is not any" literal)

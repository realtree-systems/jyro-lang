namespace RealTree.Jyro.Tests.Compiler

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Compile-time rejections the parser accepts but the Validator rejects.
/// Each `[<Theory>]` entry is a program that should parse successfully and
/// then fail `Validator.validate` with the expected MessageCode.
module SemanticRejections =

    /// Parse then validate; return the MessageCodes on any error, or None
    /// if validation succeeded.
    let private validate (src: string) : MessageCode list option =
        match Parser.parse src with
        | Failed msgs ->
            // Parser-level failure isn't what we're testing here - surface it
            // so a misconfigured test can't silently pass.
            failwithf
                "Expected source to parse; got parse error(s): %A"
                (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
        | Produced program ->
            let result = Validator.validate program
            if result.IsSuccess then None
            else Some (List.ofSeq result.Messages |> List.ofSeq |> List.map (fun m -> m.Code))
        | _ -> failwith "unreachable"

    let private expectCode (expected: MessageCode) (src: string) =
        match validate src with
        | None -> Assert.Fail(sprintf "Expected validator to reject:\n%s" src)
        | Some codes ->
            Assert.True(
                List.contains expected codes,
                sprintf "Expected code %A among diagnostics %A.\nSource:\n%s"
                    expected codes src)

    // -- Nested declarations --------------------------------------------------

    [<Theory>]
    [<InlineData("if true then\n  func nope()\n    return\n  end\nend")>]
    [<InlineData("while true do\n  func nope()\n    return\n  end\nend")>]
    let ``func nested in a block is rejected with FunctionNotAtTopLevel`` (src: string) =
        expectCode MessageCode.FunctionNotAtTopLevel src

    [<Fact>]
    let ``func nested in another func is rejected with NestedFunctionDeclaration`` () =
        expectCode MessageCode.NestedFunctionDeclaration
            "func outer()\n  func inner()\n    return\n  end\nend"

    [<Fact>]
    let ``union nested in a block is rejected with UnionNotAtTopLevel`` () =
        expectCode MessageCode.UnionNotAtTopLevel
            "if true then\n  union U A B end\nend"

    [<Fact>]
    let ``union nested in a func is rejected with UnionInsideFunction`` () =
        expectCode MessageCode.UnionInsideFunction
            "func outer()\n  union U A B end\nend"

    // -- Delete target --------------------------------------------------------

    [<Theory>]
    [<InlineData("var x = 5\ndelete x")>]
    [<InlineData("delete 42")>]
    let ``delete on a non-target is rejected with InvalidDeleteTarget`` (src: string) =
        expectCode MessageCode.InvalidDeleteTarget src

    // -- Assignment target ----------------------------------------------------

    [<Theory>]
    [<InlineData("5 = 10")>]
    [<InlineData("(a + b) = 5")>]
    let ``assignment to non-target is rejected with InvalidAssignmentTarget`` (src: string) =
        expectCode MessageCode.InvalidAssignmentTarget src

    // -- Type hint on compound assignment -------------------------------------

    [<Theory>]
    [<InlineData("var x = 0\nx: number += 1")>]
    [<InlineData("var x = \"\"\nx: string &= \"y\"")>]
    let ``type hint on compound assignment is rejected with TypeHintOnCompoundAssignment`` (src: string) =
        expectCode MessageCode.TypeHintOnCompoundAssignment src

    // -- Increment / decrement target -----------------------------------------
    // `x++` writes to `x`, so it answers to the same two rules as `x = x + 1`.
    // The validator used to recurse into the operand and check nothing else,
    // which let every case below reach the compiler's `failwith` and surface
    // as "Internal compiler error ... Please report this".

    [<Theory>]
    [<InlineData("var x = 5++")>]
    [<InlineData("var x = 5--")>]
    [<InlineData("var x = ++5")>]
    [<InlineData("var a = 1\nvar b = 2\nvar x = (a + b)++")>]
    let ``increment of a non-target is rejected with InvalidIncrementTarget`` (src: string) =
        expectCode MessageCode.InvalidIncrementTarget src

    [<Theory>]
    [<InlineData("Input++")>]
    [<InlineData("++Input")>]
    [<InlineData("Input.a++")>]
    [<InlineData("Input.a.b++")>]
    [<InlineData("Input[0]++")>]
    [<InlineData("Input.a--")>]
    let ``increment rooted at Input is rejected with InputIsReadOnly`` (src: string) =
        // Rooted, not shape-matched: `Input.a.b++` writes into the host's graph
        // exactly as `Input.a.b = 1` does, and refusing only the first level
        // would admit every level below it.
        expectCode MessageCode.InputIsReadOnly src

    [<Theory>]
    [<InlineData("Output = 5\nOutput++")>]
    [<InlineData("Output = 5\nOutput += 1")>]
    [<InlineData("Output = 'a'\nOutput &= 'b'")>]
    let ``compound assignment and increment on Output are accepted`` (src: string) =
        // `Output` reads like any other identifier, so `Output = Output + 1`
        // always worked. Refusing the sugar was a hole in the sugar, not a
        // rule about Output.
        Assert.True((validate src).IsNone, sprintf "Expected validator to accept:\n%s" src)

    // -- Names the host supplies ---------------------------------------------

    // `Input` and `Output` are supplied by the host, and a script could rebind
    // either at five of the six sites where a script binds a name. The binding
    // was accepted, allocated, assigned - and then never read, because
    // `compileIdentifier` special-cases both names *before* it consults the
    // variable table. So the declaration compiled to a dead store and every
    // later mention still meant the host's value:
    //
    //   var Input = 99      then  Output.d = Input   ->  the host's input
    //   foreach Input in [1, 2]   Output.x = Input   ->  the host's input
    //   Map([1, 2], (Input) => Input)                ->  the host's input, twice
    //
    // Silently accepting a statement that does nothing is the defect this
    // audit has fixed twice before (`Output += 1`, the late `RegisterFunction`),
    // and the half-real part made it worse: a *second* `var Input` reported
    // VariableAlreadyDeclared, because the validator registered the name in
    // scope even though the compiler would never resolve to it.
    //
    // Function parameters were the one site with a rule, which is what makes
    // this a missed case rather than a decision. The rule is now stated once,
    // for every site that binds a name.

    [<Theory>]
    [<InlineData("var Input = 99")>]
    [<InlineData("var Output = 99")>]
    [<InlineData("var Input: number = 1")>]
    let ``a variable declaration cannot rebind a host name`` (src: string) =
        expectCode MessageCode.ReservedName src

    [<Theory>]
    [<InlineData("foreach Input in [1, 2] do\n  Output.x = Input\nend")>]
    [<InlineData("foreach Output in [1, 2] do\n  Output.x = 1\nend")>]
    let ``a foreach loop variable cannot rebind a host name`` (src: string) =
        expectCode MessageCode.ReservedName src

    [<Theory>]
    [<InlineData("for Input in 1 to 2 do\n  Output.x = Input\nend")>]
    [<InlineData("for Output in 1 to 2 do\n  Output.x = 1\nend")>]
    let ``a for loop variable cannot rebind a host name`` (src: string) =
        expectCode MessageCode.ReservedName src

    [<Theory>]
    [<InlineData("Output.r = Map([1, 2], (Input) => Input)")>]
    [<InlineData("Output.r = Map([1, 2], (Output) => 1)")>]
    [<InlineData("Output.r = Reduce([1, 2], (acc, Input) => acc, 0)")>]
    let ``a lambda parameter cannot rebind a host name`` (src: string) =
        expectCode MessageCode.ReservedName src

    [<Theory>]
    [<InlineData("func f(Input)\n  return Input\nend")>]
    [<InlineData("func f(a, Output)\n  return a\nend")>]
    let ``a function parameter cannot rebind a host name`` (src: string) =
        // This site always had a rule. It reported `ReservedParameterName`,
        // which is now retired: one rule that holds at six sites reads better
        // as one code than as two, and the diagnostic carries the location, so
        // nothing is lost by not naming the enclosing function in the text.
        expectCode MessageCode.ReservedName src

    [<Theory>]
    [<InlineData("union U\n  A(x)\nend\nvar u = A(1)\nOutput.r = match u do\n  case A(Input) then Input\nend")>]
    [<InlineData("union U\n  A(x)\nend\nvar u = A(1)\nOutput.r = match u do\n  case A(Output) then 1\nend")>]
    let ``a match case binding cannot rebind a host name`` (src: string) =
        expectCode MessageCode.ReservedName src

    [<Theory>]
    [<InlineData("var input = 1\nOutput.v = input")>]
    [<InlineData("var output = 1\nOutput.v = output")>]
    [<InlineData("var Inputs = 1\nOutput.v = Inputs")>]
    [<InlineData("foreach InputRow in [1, 2] do\n  Output.x = InputRow\nend")>]
    let ``a name that merely resembles a host name is still allowed`` (src: string) =
        // The rule is the two exact names, not a prefix or a case-insensitive
        // match. Jyro identifiers are case-sensitive everywhere else.
        Assert.Equal<MessageCode list option>(None, validate src)

    [<Theory>]
    [<InlineData("union U\n  Input(x)\n  B(y)\nend")>]
    [<InlineData("union U\n  Output(x)\n  B(y)\nend")>]
    let ``a union variant cannot take a host name`` (src: string) =
        // The seventh binding site, missed when the rule was written for the
        // other six. A variant name is a callable introduced by the script, and
        // this one compiled: `union U Input(x) end` then made `Input(1)`
        // construct the variant while `Input.marker` in the same script still
        // read the host's value. One name, two meanings, no diagnostic.
        //
        // `func Input()` was already refused, so the two halves of one
        // namespace - declared functions and variant constructors - disagreed.
        expectCode MessageCode.ReservedName src

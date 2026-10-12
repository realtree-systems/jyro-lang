/// 20.4 - reserved keywords, where they are permitted, and `any`/`function`.
module RealTree.Jyro.Spec.Tests.S20_04_Keywords

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 20.4.1: reserved words are not names a script can declare ---------------

/// Every place a script declares a name; {K} is the name.
let private declarationSites =
    dict [ "var", "var {K} = 1"
           "func", "func {K}() end"
           "parameter", "func f({K}) end"
           "lambda_bare_parameter", "Output.x = Map([1], {K} => 1)"
           "lambda_second_parameter", "Output.x = Map([1], (a, {K}) => 1)"
           "for_variable", "for {K} in 0 to 1 do end"
           "foreach_variable", "foreach {K} in [1] do end"
           "union_name", "union {K} A end"
           "variant_name", "union U {K} end"
           "variant_field", "union U A({K}) end"
           "match_statement_binding", "union U A(v) end\nvar u = A(1)\nmatch u do case A({K}) then end"
           "match_expression_binding", "union U A(v) end\nvar u = A(1)\nOutput.m = match u do case A({K}) then 1 end" ]

let private at (site: string) (word: string) = declarationSites.[site].Replace("{K}", word)

let reservedAtEverySite : seq<obj[]> =
    seq {
        for w in reservedWords do
            for s in declarationSites.Keys do
                yield [| box w; box s |]
    }

let private notReserved = [ "any"; "function"; "If"; "TRUE"; "Null"; "endx"; "_end"; "var1" ]

let reservedRowsForCase : seq<obj[]> = rowsOf reservedWords

let controlsAtEverySite : seq<obj[]> =
    seq {
        for w in notReserved do
            for s in declarationSites.Keys do
                yield [| box w; box s |]
    }

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1817")>]
let ``JYRO-REQ-1817 Reserved_TheListHas38Words`` () =
    Assert.Equal(38, reservedWords |> List.distinct |> List.length)
    let expected =
        [ "and"; "array"; "boolean"; "break"; "by"; "case"; "continue"; "default"; "delete"; "do"
          "downto"; "else"; "elseif"; "end"; "exit"; "fail"; "false"; "for"; "foreach"; "func"
          "if"; "in"; "is"; "match"; "not"; "null"; "number"; "object"; "or"; "return"
          "string"; "switch"; "then"; "to"; "true"; "union"; "var"; "while" ]
    Assert.Equal<string list>(List.sort expected, List.sort reservedWords)

[<Theory>]
[<MemberData(nameof reservedAtEverySite)>]
[<Trait("requirement", "JYRO-REQ-1817")>]
let ``JYRO-REQ-1817 Reserved_WordAsADeclaredName_IsAParseErrorAndTheScriptDoesNotRun`` (word: string, site: string) =
    parseError (at site word)

[<Theory>]
[<MemberData(nameof controlsAtEverySite)>]
[<Trait("requirement", "JYRO-REQ-1817")>]
let ``JYRO-REQ-1817 Reserved_Control_WordNotInTheList_IsAcceptedAsADeclaredName`` (word: string, site: string) =
    compiles (at site word)

[<Theory>]
[<MemberData(nameof reservedRowsForCase)>]
[<Trait("requirement", "JYRO-REQ-1817")>]
let ``JYRO-REQ-1817 Reserved_WordInAnotherCase_IsAnOrdinaryIdentifier`` (word: string) =
    let upper = word.ToUpperInvariant()
    let capital = string (System.Char.ToUpperInvariant word.[0]) + word.Substring(1)
    for w in [ upper; capital ] do
        outV (sprintf "var %s = 7\nOutput.v = %s" w w) |> assertJson "7"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1817")>]
let ``JYRO-REQ-1817 Reserved_TheStatementExamples_Behave_AsTheClauseGives`` () =
    parseError "var while = 1"
    parseError "func f(if) return 1 end"
    compiles "var If = 1"
    compiles "var any = 1"

// -- 20.4.2: reserved words where a name is not declared ----------------------

let reservedRows : seq<obj[]> = rowsOf reservedWords

[<Theory>]
[<MemberData(nameof reservedRows)>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_Word_IsAcceptedAsAnObjectLiteralKey`` (word: string) =
    let o = outProp "o" (sprintf "Output.o = { %s: 1 }" word)
    assertJson (sprintf "{\"%s\":1}" word) o

[<Theory>]
[<MemberData(nameof reservedRows)>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_Word_IsAcceptedAsAQuotedObjectLiteralKey`` (word: string) =
    let o = outProp "o" (sprintf "Output.o = { \"%s\": 1 }" word)
    Assert.True(has word o, sprintf "the object has no key %s: %s" word (Values.toJson o))

[<Theory>]
[<MemberData(nameof reservedRows)>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_Word_IsAcceptedAsAPropertyNameAfterDot_ForWritingAndReading`` (word: string) =
    let o = Script.outputOf (sprintf "Output.%s = 2\nOutput.v = Output.%s\nvar o = {}\no.%s = 3\nOutput.w = o.%s" word word word word)
    o |> Values.prop word |> assertJson "2"
    o |> Values.prop "v" |> assertJson "2"
    o |> Values.prop "w" |> assertJson "3"

[<Theory>]
[<MemberData(nameof reservedRows)>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_Word_IsAcceptedAsANamedArgumentLabel_BindingAHostParameter`` (word: string) =
    let f =
        Host.func "F" [ Parameter.Required(word, ParameterType.Any) ] ParameterType.Any FunctionEffect.Pure
            (fun args -> args.Head)
    let r = Script.runWith { Script.defaults with Functions = [ f.AsFunction ] } (sprintf "Output.n = F(%s: 4)" word)
    Script.output r |> Values.prop "n" |> assertJson "4"

[<Theory>]
[<MemberData(nameof reservedRows)>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_Word_AsANamedArgumentLabelWithNoSuchParameter_IsOnlyUnknownNamedArgument`` (word: string) =
    rejectedWithCode MessageCode.UnknownNamedArgument (sprintf "func g(x = 0) return x end\nOutput.r = g(%s: 1)" word)

let private matchWithVariant (word: string) =
    sprintf "union U A end\nvar u = A()\nOutput.m = match u do case A then 1 case %s then 2 end" word

/// All reserved words but `default`, which 20.4.2 and 50.9.2 give MatchWithDefault.
let reservedRowsButDefault : seq<obj[]> = rowsOf (reservedWords |> List.filter (fun w -> w <> "default"))

[<Theory>]
[<MemberData(nameof reservedRowsButDefault)>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_Word_AsAVariantNameInAMatchExpression_IsRefusedOnlyWithMatchUnknownVariant`` (word: string) =
    // Accepted by the parser; validation refuses it with MatchUnknownVariant alone.
    rejected (matchWithVariant word) |> assertOnly MessageCode.MatchUnknownVariant

[<Fact>]
[<Trait("requirement", "JYRO-REQ-25")>]
let ``Reserved_WordDefault_AsACaseInAMatchExpression_IsRefusedOnlyWithMatchWithDefault`` () =
    // jyro-spec--d-97, settled: `case default` is accepted by the parser and
    // refused with MatchWithDefault (50.9.2), not MatchUnknownVariant.
    let r = rejected (matchWithVariant "default")
    assertOnly MessageCode.MatchWithDefault r
    Assert.DoesNotContain(MessageCode.MatchUnknownVariant, Script.codes r)

// -- 20.4.3: `any` and `function` ---------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-26")>]
let ``AnyFunction_AfterColonOfVarHint_AreTypeKeywords`` () =
    Script.outputOf "var a: any = 1\nvar f: function = x => x\nOutput.a = a\nOutput.f = f is function"
    |> assertJson "{\"a\":1,\"f\":true}"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-26")>]
let ``AnyFunction_AfterColonOfParameterHint_AreTypeKeywords`` () =
    outV "func h(p: any, q: function) return p end\nOutput.v = h(1, x => x)" |> assertJson "1"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-26")>]
let ``AnyFunction_AfterColonOfAssignmentHint_IsTypeKeyword`` () =
    outV "Output.v: any = 2" |> assertJson "2"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-26")>]
let ``AnyFunction_AfterColonOfVariantFieldHint_IsTypeKeyword`` () =
    outV "union U A(v: any, g: function) end\nvar u = A(5, x => x)\nOutput.v = match u do case A(v, g) then v end"
    |> assertJson "5"

[<Theory>]
[<InlineData("5 is any", "true")>]
[<InlineData("null is any", "true")>]
[<InlineData("(x => x) is function", "true")>]
[<InlineData("5 is function", "false")>]
[<InlineData("5 is not function", "true")>]
[<InlineData("5 is not any", "false")>]
[<Trait("requirement", "JYRO-REQ-26")>]
let ``AnyFunction_AfterIsAndIsNot_AreTypeKeywords`` (expr: string, expected: string) =
    outV ("Output.v = " + expr) |> assertJson expected

let ordinaryPositions : seq<obj[]> =
    [ "var_any", "var any = 1\nOutput.v = any", "1"
      "var_function", "var function = 2\nOutput.v = function", "2"
      "func_any", "func any() return 3 end\nOutput.v = any()", "3"
      "func_function", "func function() return 3 end\nOutput.v = function()", "3"
      "parameter_function", "func g(function) return function end\nOutput.v = g(4)", "4"
      "lambda_parameter_any", "Output.v = Map([1, 2], any => any * 10)", "[10,20]"
      "for_variable_any", "for any in 1 to 2 do Output.v = any end", "1"
      "foreach_variable_function", "foreach function in [7] do Output.v = function end", "7"
      "object_value", "var any = 1\nOutput.v = { a: any }", "{\"a\":1}"
      "ternary_branches", "var any = 1\nvar function = 2\nOutput.v = true ? any : function", "1"
      "ternary_second_branch", "var any = 1\nvar function = 2\nOutput.v = false ? any : function", "2"
      "named_argument_label_and_value", "func g(function) return function end\nvar any = 9\nOutput.v = g(function: any)", "9"
      "property_name", "var any = 5\nOutput.v = { }\nOutput.v.any = any\nOutput.v.function = 6", "{\"any\":5,\"function\":6}"
      "name_then_type", "var any: any = 7\nOutput.v = any", "7"
      "reference_in_arithmetic", "var function = 2\nOutput.v = function * 3", "6" ]
    |> Seq.map (fun (l, s, e) -> [| box l; box s; box e |])

[<Theory>]
[<MemberData(nameof ordinaryPositions)>]
[<Trait("requirement", "JYRO-REQ-26")>]
let ``AnyFunction_InEveryOtherPosition_AreOrdinaryIdentifiers`` (label: string, source: string, expected: string) =
    ignore label
    outV source |> assertJson expected

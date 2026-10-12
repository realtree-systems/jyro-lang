/// jyro-spec 80: Functions and the type hint system.
/// From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S80_Functions

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Run with a host function Mark() available, and return the result and Mark.
let runMarked (options: RunOptions) (source: string) =
    let mark = Host.recorder "Mark"
    let r = Script.runWith { options with Functions = options.Functions @ [ mark.AsFunction ] } source
    r, mark

/// Assert the script is rejected before it runs: an Error, and Mark() (the
/// script's first statement) never called.
let assertRejectedBeforeRun (options: RunOptions) (source: string) =
    let r, mark = runMarked options ("Mark()\n" + source)
    assertFailedWithError r
    Assert.Equal(0, mark.CallCount)
    r

/// Assert the script is rejected before it runs with exactly one Error, of `code`.
let assertRejectedWith (code: MessageCode) (options: RunOptions) (source: string) =
    let r = assertRejectedBeforeRun options source
    Script.expectError code r
    r

/// Assert the script is rejected before it runs, with `code` among its errors.
let assertRejectedIncluding (code: MessageCode) (options: RunOptions) (source: string) =
    let r = assertRejectedBeforeRun options source
    Assert.True(List.contains code (Script.errorCodes r), sprintf "Expected %A. Diagnostics:\n%s" code (Script.describe r))
    r

/// Assert the script is rejected before it runs, every Error being `code`
/// (one conflict may be reported once for each clause that names it).
let assertRejectedWithOnly (code: MessageCode) (options: RunOptions) (source: string) =
    let r = assertRejectedBeforeRun options source
    Assert.Equal<MessageCode list>([ code ], Script.errorCodes r |> List.distinct)

/// The value of Output.r after a successful run.
let resultOf (source: string) : JyroValue = outProp "r" (Script.run source)

let assertR (expectedJson: string) (source: string) =
    let v = resultOf source
    Assert.True(v.EqualsValue(Values.json expectedJson), sprintf "expected %s, got %s" expectedJson (Values.toJson v))

let rows (xs: 'a list) : obj[] seq = xs |> Seq.map (fun x -> [| box x |])

/// Assert the script fails with a parse error (130.3): rejected before it runs
/// (Mark() never called), with at least one Error and every Error of the stage
/// Parse (160.1.12).
let assertParseError (options: RunOptions) (source: string) =
    let r = assertRejectedBeforeRun options source
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    Assert.NotEmpty(errors)
    for m in errors do
        Assert.True(Pending.stageOf m = "Parse",
                    sprintf "Expected stage Parse, got %s for %A. Diagnostics:\n%s" (Pending.stageOf m) m.Code (Script.describe r))

/// Every diagnostic of `code` in the result has the stage `stage` (160.1.12),
/// read from the diagnostic's own stage, and there is at least one.
let assertStageOfCode (code: MessageCode) (stage: string) (result: JyroResult<'T>) =
    let ofCode = [ for m in result.Messages do if m.Code = code then yield m ]
    Assert.False(List.isEmpty ofCode, sprintf "Expected %A. Diagnostics:\n%s" code (Script.describe result))
    for m in ofCode do
        Assert.Equal(stage, Pending.stageOf m)

// ===========================================================================
// 80.1 Declaration form
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1803")>]
[<InlineData("func f(a, b) return a end\nOutput.r = f(5, 6)", "5")>]
[<InlineData("func f() end\nOutput.r = f()", "null")>]
[<InlineData("func f() return 1 end\nOutput.r = f()", "1")>]
[<InlineData("func f(a) return a end\nOutput.r = f(2)", "2")>]
[<InlineData("func f(a, b: number, c = 1, d: string = \"x\") return [a, b, c, d] end\nOutput.r = f(0, 2)", "[0,2,1,\"x\"]")>]
[<InlineData("func f(a)\n  var b = a + 1\n  var c = b * 2\n  return c\nend\nOutput.r = f(1)", "4")>]
[<InlineData("func f(a) var b = a return b end Output.r = f(3)", "3")>]
let ``JYRO-REQ-1803 Declaration_OfTheFormFuncNameParametersBlockEnd_CompilesAndIsCallable`` (source: string, expected: string) =
    assertR expected source

/// The form is read with no parse error, and with no error at all.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1803")>]
[<InlineData("func f() end")>]
[<InlineData("func f(a, b) return a end")>]
[<InlineData("func f(a)\n  var b = a\n  return b\nend")>]
let ``JYRO-REQ-1803 Declaration_OfTheForm_HasNoParseError`` (source: string) =
    assertCompiles (Script.compile source)

/// Any other form is a parse error at Parse, and the script does not run.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1803")>]
[<InlineData("func f() return 1")>]
[<InlineData("func f return 1 end")>]
[<InlineData("func f end")>]
[<InlineData("func (a) end")>]
[<InlineData("func f() { return 1 }")>]
[<InlineData("function f() end")>]
[<InlineData("func () end")>]
[<InlineData("func 1() end")>]
[<InlineData("func 1f() end")>]
[<InlineData("func f(a b) end")>]
[<InlineData("func f(a,) end")>]
[<InlineData("func f() end end")>]
[<InlineData("func f(] end")>]
let ``JYRO-REQ-1803 Declaration_InAnyOtherForm_IsAParseErrorAtParseAndNothingRuns`` (source: string) =
    assertParseError Script.defaults source

/// A rejected declaration leaves no output: the statements before it did not run.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1803")>]
let ``JYRO-REQ-1803 Declaration_InAnyOtherForm_ProducesNoOutputAndRunsNoEarlierStatement`` () =
    let r, mark = runMarked Script.defaults "Mark()\nOutput.x = 1\nfunc f end"
    assertFailedWithError r
    Assert.Equal(0, mark.CallCount)
    match Script.value r with
    | Some v -> Assert.False(hasKey "x" v)
    | None -> ()

let private F = "func g() return 1 end"

let blockKinds : obj[] seq =
    rows
        [ "if true then {F} end"
          "if false then var q = 1 elseif true then {F} end"
          "if false then var q = 1 else {F} end"
          "while false do {F} end"
          "for i in 0 to 1 do {F} end"
          "foreach x in [1] do {F} end"
          "switch 1 do case 1 then {F} end"
          "switch 1 do default then {F} end"
          "match A() do case A then {F} end"
          "while false do if true then {F} end end" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-287")>]
[<MemberData(nameof blockKinds)>]
let ``JYRO-REQ-287 Declaration_InABlockOutsideAnyFunctionBody_IsRejectedWithFunctionNotAtTopLevel`` (template: string) =
    let source = "union U A end\n" + template.Replace("{F}", F)
    // "Validation MUST reject": the diagnostic has the stage Validate (160.1.12).
    assertRejectedWith MessageCode.FunctionNotAtTopLevel Script.defaults source
    |> assertStageOfCode MessageCode.FunctionNotAtTopLevel "Validate"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-287")>]
let ``JYRO-REQ-287 Declaration_AtTheTopLevelBeforeAndAfterStatements_Compiles`` () =
    assertR "3" "func a() return 1 end\nvar x = a()\nfunc b() return 2 end\nOutput.r = x + b()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-287")>]
let ``JYRO-REQ-287 Declaration_InABlockInsideAFunctionBody_IsNotFunctionNotAtTopLevel`` () =
    let r = Script.compile "func f() if true then func g() end end end"
    Assert.False(List.contains MessageCode.FunctionNotAtTopLevel (Script.errorCodes r), Script.describe r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-288")>]
[<MemberData(nameof blockKinds)>]
let ``JYRO-REQ-288 Declaration_InAnyBlockOfAFunctionBody_IsRejectedWithNestedFunctionDeclarationOnly`` (template: string) =
    let source = "union U A end\nfunc f()\n" + template.Replace("{F}", F) + "\nend"
    let r = assertRejectedWith MessageCode.NestedFunctionDeclaration Script.defaults source
    Assert.DoesNotContain(MessageCode.FunctionNotAtTopLevel, Script.codes r)
    // "Validation MUST reject": the diagnostic has the stage Validate (160.1.12).
    assertStageOfCode MessageCode.NestedFunctionDeclaration "Validate" r

[<Theory>]
[<Trait("requirement", "JYRO-REQ-288")>]
[<InlineData("func f() func g() end end")>]
[<InlineData("func f() if true then func g() end end end")>]
[<InlineData("func f() if true then while false do foreach x in [1] do func g() end end end end end")>]
let ``JYRO-REQ-288 Declaration_DirectlyOrDeepInAFunctionBody_IsRejectedWithNestedFunctionDeclarationOnly`` (source: string) =
    let r = assertRejectedWith MessageCode.NestedFunctionDeclaration Script.defaults source
    Assert.DoesNotContain(MessageCode.FunctionNotAtTopLevel, Script.codes r)
    assertStageOfCode MessageCode.NestedFunctionDeclaration "Validate" r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-288")>]
let ``JYRO-REQ-288 TwoTopLevelFunctions_Compile`` () =
    assertR "3" "func f() return 1 end\nfunc g() return 2 end\nOutput.r = f() + g()"

// ===========================================================================
// 80.2 Whole-script registration
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1809")>]
[<InlineData("Output.r = f(2)\nfunc f(x) return x * 2 end", "4")>]
[<InlineData("func f() return 1 end\nOutput.r = f()", "1")>]
[<InlineData("func g() return f() + 1 end\nOutput.r = g()\nfunc f() return 1 end", "2")>]
[<InlineData("func g() return f() + 1 end\nfunc f() return 1 end\nOutput.r = g()", "2")>]
[<InlineData("func g() return f() end\nfunc f() return 7 end\nOutput.r = g()", "7")>]
[<InlineData("var s = 0\nvar i = 0\nwhile i < 3 do\n  s = s + f(i)\n  i = i + 1\nend\nOutput.r = s\nfunc f(x) return x end", "3")>]
[<InlineData("Output.r = Map([1, 2], x => f(x))\nfunc f(x) return x + 1 end", "[2,3]")>]
[<InlineData("if true then Output.r = f() end\nfunc f() return \"late\" end", "\"late\"")>]
let ``JYRO-REQ-1809 Call_WhenDeclarationIsLaterInTheSource_Works`` (source: string, expected: string) =
    assertR expected source

/// The statement's own example, in both orders, gives the same `Output.x`; the
/// script compiles with no error.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1809")>]
[<InlineData("Output.x = f()\nfunc f() return 1 end")>]
[<InlineData("func f() return 1 end\nOutput.x = f()")>]
let ``JYRO-REQ-1809 Call_BeforeOrAfterItsDeclaration_GivesOutputXOne`` (source: string) =
    assertCompiles (Script.compile source)
    let r = Script.run source
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1.0, Values.asFloat (outProp "x" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1812")>]
[<InlineData(0, true)>]
[<InlineData(1, false)>]
[<InlineData(10, true)>]
[<InlineData(11, false)>]
let ``JYRO-REQ-1812 TwoFunctions_CallingEachOther_GiveTheRightResult`` (n: int, even: bool) =
    let evenFirst =
        "func isEven(n) if n == 0 then return true end return isOdd(n - 1) end\n"
        + "func isOdd(n) if n == 0 then return false end return isEven(n - 1) end\n"
    let oddFirst =
        "func isOdd(n) if n == 0 then return false end return isEven(n - 1) end\n"
        + "func isEven(n) if n == 0 then return true end return isOdd(n - 1) end\n"
    for decls in [ evenFirst; oddFirst ] do
        Assert.Equal(even, Values.asBool (resultOf (decls + sprintf "Output.r = isEven(%d)" n)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1812")>]
let ``JYRO-REQ-1812 ThreeFunctions_InACycle_GiveTheRightResult`` () =
    assertR "[\"a\",\"b\",\"c\",\"a\",\"b\",\"c\",\"a\"]"
        (lines [ "func a(n, acc) if n == 0 then return acc end return b(n - 1, Append(acc, \"a\")) end"
                 "func b(n, acc) if n == 0 then return acc end return c(n - 1, Append(acc, \"b\")) end"
                 "func c(n, acc) if n == 0 then return acc end return a(n - 1, Append(acc, \"c\")) end"
                 "Output.r = a(7, [])" ])

// ===========================================================================
// 80.3 Function names
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-291")>]
[<InlineData("func f() end\nfunc f() end")>]
[<InlineData("func f(a) end\nfunc f(a, b) end")>]
[<InlineData("func f() end\nvar x = 1\nOutput.x = x\nfunc f() return 2 end")>]
[<InlineData("func f() end\nfunc g() end\nfunc f() end\nOutput.r = g()")>]
let ``SecondDeclaration_WithTheSameName_IsRejectedWithDuplicateFunction`` (source: string) =
    let r = assertRejectedBeforeRun Script.defaults source
    Assert.Equal<MessageCode list>([ MessageCode.DuplicateFunction ], Script.errorCodes r |> List.distinct)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``ThreeDeclarations_WithTheSameName_AreRejectedWithDuplicateFunction`` () =
    let r = assertRejectedBeforeRun Script.defaults "func f() end\nfunc f() end\nfunc f() end"
    Assert.Equal<MessageCode list>([ MessageCode.DuplicateFunction ], Script.errorCodes r |> List.distinct)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``SecondDeclaration_WithTheSameName_IsRejectedWithDuplicateFunction_OnCompileToArtifact`` () =
    let r = Script.compileBytesWith Script.defaults "func f() return 1 end\nfunc f() return 2 end"
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.Equal<MessageCode list>([ MessageCode.DuplicateFunction ], Script.errorCodes r |> List.distinct)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-291")>]
[<InlineData("func L() return 1 end\nfunc L() return 2 end", "")>]
[<InlineData("func L() return 1 end", "func L() return 2 end")>]
let ``SecondDeclaration_WithTheSameName_InLibraries_IsRejectedWithDuplicateFunction`` (first: string, second: string) =
    let libraries = if second = "" then [ first ] else [ first; second ]
    let r = assertRejectedBeforeRun { Script.defaults with Libraries = libraries } "Output.x = 1"
    Assert.Equal<MessageCode list>([ MessageCode.DuplicateFunction ], Script.errorCodes r |> List.distinct)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-291")>]
[<InlineData("func f() return 1 end\nfunc F() return 2 end\nOutput.r = f() + F()")>]
[<InlineData("func f() return 1 end\nfunc g() return 2 end\nOutput.r = f() + g()")>]
let ``Declarations_WithDifferentNames_Compile`` (source: string) =
    assertR "3" source

let inventoryNames () = Inventory.names ()

[<Theory>]
[<Trait("requirement", "JYRO-REQ-292")>]
[<MemberData(nameof inventoryNames)>]
let ``Declaration_NamedAsAnInventoryFunction_WithStdlib_IsRejectedWithFunctionOverride`` (name: string) =
    assertRejectedWith MessageCode.FunctionOverride Script.defaults (sprintf "func %s() return 0 end" name) |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-292")>]
[<MemberData(nameof inventoryNames)>]
let ``Declaration_NamedAsAnInventoryFunction_WithoutStdlib_IsRejectedWithFunctionOverride`` (name: string) =
    assertRejectedWith MessageCode.FunctionOverride { Script.defaults with Stdlib = false } (sprintf "func %s(x) return 0 end" name)
    |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-292")>]
[<MemberData(nameof inventoryNames)>]
let ``Declaration_NamedAsAnInventoryFunction_RegisteredAsHostFunction_IsRejectedWithFunctionOverride`` (name: string) =
    let host = Host.recorder name
    assertRejectedWith MessageCode.FunctionOverride
        { Script.defaults with Stdlib = false; Functions = [ host.AsFunction ] }
        (sprintf "func %s() return 0 end" name)
    |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-292")>]
let ``Declaration_NamedAsARegisteredHostFunction_IsRejectedWithFunctionOverride`` () =
    let foo = Host.recorder "Foo"
    assertRejectedWith MessageCode.FunctionOverride { Script.defaults with Functions = [ foo.AsFunction ] } "func Foo() end"
    |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-292")>]
let ``Declaration_NamedAsAFurtherStdlibFunction_IsRejectedWithFunctionOverride`` () =
    let listed = Inventory.all |> List.map (fun l -> l.Name) |> set
    let further = StdlibRegistry.getAll () |> Seq.map (fun f -> f.Name) |> Seq.filter (listed.Contains >> not) |> List.ofSeq
    for name in further do
        assertRejectedWith MessageCode.FunctionOverride Script.defaults (sprintf "func %s() return 0 end" name) |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-292")>]
[<InlineData("length")>]
[<InlineData("TOUPPER")>]
[<InlineData("myLength")>]
let ``Declaration_NameDifferingFromEveryInventoryName_CompilesWithoutStdlib`` (name: string) =
    let r = Script.runWith { Script.defaults with Stdlib = false } (sprintf "func %s() return 1 end\nOutput.r = %s()" name name)
    Assert.Equal(1.0, Values.asFloat (outProp "r" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-292")>]
let ``Declaration_NamedAsALibraryFunction_IsNotFunctionOverride`` () =
    let r = Script.compileWith { Script.defaults with Libraries = [ "func Helper() return 1 end" ] } "func Helper() return 2 end"
    Assert.DoesNotContain(MessageCode.FunctionOverride, Script.errorCodes r)
    Assert.Contains(MessageCode.FunctionShadowsLibrary, Script.errorCodes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-293")>]
let ``Declaration_NamedAsAFunctionOfALibrarySource_IsRejectedWithFunctionShadowsLibrary`` () =
    assertRejectedWith MessageCode.FunctionShadowsLibrary
        { Script.defaults with Libraries = [ "func Helper() return 1 end" ] } "func Helper() return 2 end"
    |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-293")>]
let ``Declaration_NamedAsAFunctionOfAParsedLibrary_IsRejectedWithFunctionShadowsLibrary`` () =
    let lib = Script.valueOf (JyroLibrary.Parse "func Helper() return 1 end")
    assertRejectedWith MessageCode.FunctionShadowsLibrary
        { Script.defaults with ConfigureCompiler = fun c -> c.AddLibrary(lib) } "func Helper() return 2 end"
    |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-293")>]
let ``Declaration_WithAnotherNameThanTheLibraryFunctions_CompilesAndCanCallThem`` () =
    let r =
        Script.runWith { Script.defaults with Libraries = [ "func Helper() return 1 end" ] }
            "func helper() return 10 end\nfunc Mine() return Helper() + 1 end\nOutput.r = Mine() + helper()"
    Assert.Equal(12.0, Values.asFloat (outProp "r" r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-294")>]
[<InlineData("union Shape Circle(r) Square(s) end\nfunc Circle() return 1 end")>]
[<InlineData("union U A end\nfunc A() end")>]
[<InlineData("func Square() return 1 end\nunion Shape Circle(r) Square(s) end")>]
let ``JYRO-REQ-294 Declaration_NamedAsAVariant_IsRejectedWithFunctionShadowsVariant`` (source: string) =
    assertRejectedWithOnly MessageCode.FunctionShadowsVariant Script.defaults source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-294")>]
let ``JYRO-REQ-294 Declaration_NamedAsAVariantOfALibrary_IsRejectedWithFunctionShadowsVariant`` () =
    assertRejectedWithOnly MessageCode.FunctionShadowsVariant
        { Script.defaults with Libraries = [ "union Shape Circle(r) end" ] } "func Circle() return 1 end"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-294")>]
[<InlineData("union U A end\nfunc A() end\nOutput.r = 1")>]
[<InlineData("union U A end\nfunc never() return A() end\nfunc A() end")>]
let ``JYRO-REQ-294 Declaration_NamedAsAVariant_IsRejectedEvenWhenNeitherIsCalled`` (source: string) =
    assertRejectedWithOnly MessageCode.FunctionShadowsVariant Script.defaults source

/// The distinct Error codes of a rejection before running, sorted.
let private codeSet (options: RunOptions) (source: string) =
    assertRejectedBeforeRun options source |> Script.errorCodes |> List.distinct |> List.sort

/// JYRO-ADR-162: a call of a name declared both as a function and as a variant is
/// checked as a call to the variant, whichever declaration comes first.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-307")>]
[<InlineData("union U A(x) end\nfunc A() end\nOutput.r = A(1, 2)")>]
[<InlineData("func A() end\nunion U A(x) end\nOutput.r = A(1, 2)")>]
[<InlineData("func A(a, b) end\nunion U A(x) end\nOutput.r = A(1, 2)")>]
[<InlineData("union U A(x) end\nfunc A(a, b) end\nOutput.r = A(1, 2)")>]
[<InlineData("union U A(x) end\nfunc A() end\nfunc never() return A(1, 2) end")>]
let ``JYRO-REQ-307 JYRO-DEF-640 CallOfAFunctionVariantClash_WithMoreArgumentsThanTheVariantsFields_GivesTooManyArguments`` (source: string) =
    let expected = [ MessageCode.TooManyArguments; MessageCode.FunctionShadowsVariant ] |> List.sort
    Assert.Equal<MessageCode list>(expected, codeSet Script.defaults source)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-306")>]
[<InlineData("func A() end\nunion U A(x) end\nOutput.r = A()")>]
[<InlineData("union U A(x) end\nfunc A() end\nOutput.r = A()")>]
[<InlineData("union U A(x, y) end\nfunc A(a) end\nOutput.r = A(1)")>]
let ``JYRO-REQ-306 JYRO-DEF-640 CallOfAFunctionVariantClash_WithFewerArgumentsThanTheVariantsFields_GivesTooFewArguments`` (source: string) =
    let expected = [ MessageCode.TooFewArguments; MessageCode.FunctionShadowsVariant ] |> List.sort
    Assert.Equal<MessageCode list>(expected, codeSet Script.defaults source)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-294")>]
[<InlineData("union U A(x) end\nfunc A() end\nOutput.r = A(1)")>]
[<InlineData("func A() end\nunion U A(x) end\nOutput.r = A(1)")>]
[<InlineData("union U A(x) end\nfunc A(a, b) end\nOutput.r = A(1)")>]
let ``JYRO-REQ-294 JYRO-DEF-640 CallOfAFunctionVariantClash_ThatFitsTheVariant_GivesFunctionShadowsVariantOnly`` (source: string) =
    assertRejectedWithOnly MessageCode.FunctionShadowsVariant Script.defaults source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-307")>]
let ``JYRO-REQ-307 JYRO-DEF-640 CallOfAScriptFunctionNamedAsALibraryVariant_IsCheckedAgainstTheVariant`` () =
    let options = { Script.defaults with Libraries = [ "union Shape Circle(r) end" ] }
    let expected = [ MessageCode.TooManyArguments; MessageCode.FunctionShadowsVariant ] |> List.sort
    Assert.Equal<MessageCode list>(expected, codeSet options "func Circle() return 1 end\nOutput.r = Circle(1, 2)")
    assertRejectedWithOnly MessageCode.FunctionShadowsVariant options "func Circle() return 1 end\nOutput.r = Circle(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-294")>]
let ``JYRO-REQ-294 LibraryFunction_NamedAsAScriptVariant_IsRejectedWithFunctionShadowsVariant`` () =
    assertRejectedIncluding MessageCode.FunctionShadowsVariant
        { Script.defaults with Libraries = [ "func Circle() return 1 end" ] } "union Shape Circle(r) end"
    |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-294")>]
let ``JYRO-REQ-294 Declaration_WithANameNoVariantHas_Compiles`` () =
    assertR "{\"_variant\":\"Circle\",\"r\":2}" "union Shape Circle(r) end\nfunc make() return Circle(2) end\nOutput.r = make()"

// ===========================================================================
// 80.4 Parameter list
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1808")>]
[<InlineData("func f(a, b: number, c = 1, d: string = \"x\") return [a, b, c, d] end\nOutput.r = f(0, 2)", "[0,2,1,\"x\"]")>]
[<InlineData("func f(a) return a end\nOutput.r = f(1)", "1")>]
[<InlineData("func f(a: number) return a end\nOutput.r = f(1)", "1")>]
[<InlineData("func f(a = 1) return a end\nOutput.r = f()", "1")>]
[<InlineData("func f(a: string = \"x\") return a end\nOutput.r = f()", "\"x\"")>]
[<InlineData("func f(a: any) return a end\nOutput.r = f(1)", "1")>]
[<InlineData("func f(a: function) return TypeOf(a) end\nOutput.r = f(x => x)", "\"function\"")>]
[<InlineData("func f(a: string) return a end\nOutput.r = f(\"s\")", "\"s\"")>]
[<InlineData("func f(a: boolean) return a end\nOutput.r = f(true)", "true")>]
[<InlineData("func f(a: object) return a end\nOutput.r = f({})", "{}")>]
[<InlineData("func f(a: array) return a end\nOutput.r = f([])", "[]")>]
[<InlineData("func f(a: null) return a end\nOutput.r = f(null)", "null")>]
[<InlineData("func f(a = 0x10) return a end\nOutput.r = f()", "16")>]
[<InlineData("func f(a = 0b11) return a end\nOutput.r = f()", "3")>]
[<InlineData("func f(a = 's') return a end\nOutput.r = f()", "\"s\"")>]
[<InlineData("func f(a = true) return a end\nOutput.r = f()", "true")>]
[<InlineData("func f(a = false) return a end\nOutput.r = f()", "false")>]
[<InlineData("func f(a = null) return a end\nOutput.r = f()", "null")>]
[<InlineData("func f(a = -1) return a end\nOutput.r = f()", "-1")>]
[<InlineData("func f(a: number = -0x10) return a end\nOutput.r = f()", "-16")>]
[<InlineData("func f(a = - 1) return a end\nOutput.r = f()", "-1")>]
[<InlineData("func f(a = -0b11) return a end\nOutput.r = f()", "-3")>]
[<InlineData("func f(a = -1.5e3) return a end\nOutput.r = f()", "-1500")>]
let ``JYRO-REQ-1808 Parameter_InAnAcceptedForm_IsAccepted`` (source: string, expected: string) =
    assertR expected source

/// A parameter in any other form is a parse error at Parse, and nothing runs.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1808")>]
[<InlineData("func f(a:) end")>]
[<InlineData("func f(a =) end")>]
[<InlineData("func f(= 1) end")>]
[<InlineData("func f(: number) end")>]
[<InlineData("func f(a = 1: number) end")>]
[<InlineData("func f(a b) end")>]
[<InlineData("func f((a)) end")>]
[<InlineData("func f(1) end")>]
[<InlineData("func f(a: number: string) end")>]
[<InlineData("func f(a: number string) end")>]
let ``JYRO-REQ-1808 Parameter_InAnyOtherForm_IsAParseErrorAtParseAndNothingRuns`` (source: string) =
    assertParseError Script.defaults source

/// A type that is not a type of a type hint is also not one of the four forms.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1808")>]
let ``JYRO-REQ-1808 Parameter_WithANameThatIsNotATypeOfATypeHint_IsRejectedBeforeExecution`` () =
    assertRejectedBeforeRun Script.defaults "func f(a: integer) end" |> ignore

/// What may stand as `default` is decided by 80.4.5, not by the form: an
/// expression that is not a literal reads with no parse error and is refused
/// with DefaultValueNotLiteral, at a stage that is not Parse.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1808")>]
[<InlineData("func f(a = [1]) end")>]
[<InlineData("func f(a = 1 + 2) end")>]
[<InlineData("func f(a = Length([])) end")>]
[<InlineData("func f(a: number = x) end")>]
let ``JYRO-REQ-1808 Parameter_DefaultIsAnyExpression_NonLiteralIsNotAParseError`` (source: string) =
    let r = assertRejectedBeforeRun Script.defaults source
    Assert.Contains(MessageCode.DefaultValueNotLiteral, Script.errorCodes r)
    for m in r.Messages do
        if m.Severity = MessageSeverity.Error then
            Assert.NotEqual<string>("Parse", Pending.stageOf m)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-296")>]
[<InlineData("func f(a, a) end")>]
[<InlineData("func f(a, b, a) end")>]
[<InlineData("func f(a: number, a: string) end")>]
[<InlineData("func f(a, a = 1) end")>]
[<InlineData("func f(a, a) end\nfunc g() return 1 end\nOutput.r = g()")>]
let ``Function_WithTwoParametersOfTheSameName_IsRejectedWithDuplicateParameterName`` (source: string) =
    assertRejectedWith MessageCode.DuplicateParameterName Script.defaults source |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-296")>]
let ``Function_WithParameterNamesDifferingInCase_Compiles`` () =
    assertR "[1,2]" "func f(a, A) return [a, A] end\nOutput.r = f(1, 2)"

let keywords () : obj[] seq =
    rows
        [ "and"; "array"; "boolean"; "break"; "by"; "case"; "continue"; "default"; "delete"; "do"; "downto"
          "else"; "elseif"; "end"; "exit"; "fail"; "false"; "for"; "foreach"; "func"; "if"; "in"; "is"
          "match"; "not"; "null"; "number"; "object"; "or"; "return"; "string"; "switch"; "then"; "to"
          "true"; "union"; "var"; "while" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-297")>]
[<MemberData(nameof keywords)>]
let ``Parameter_NamedAsAReservedKeyword_IsRejectedBeforeExecution`` (keyword: string) =
    assertRejectedBeforeRun Script.defaults (sprintf "func f(%s) return 1 end" keyword) |> ignore
    assertRejectedBeforeRun Script.defaults (sprintf "func f(a, %s: number = 1) return 1 end" keyword) |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-297")>]
[<InlineData("any")>]
[<InlineData("function")>]
[<InlineData("If")>]
[<InlineData("END")>]
let ``Parameter_NamedAsANonReservedWord_Compiles`` (name: string) =
    assertR "5" (sprintf "func f(%s) return %s end\nOutput.r = f(5)" name name)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-298")>]
[<InlineData("func f(a = 1, b) end")>]
[<InlineData("func f(a, b = 1, c) end")>]
[<InlineData("func f(a: number = 1, b: string) end")>]
[<InlineData("func f(a = null, b) end")>]
let ``Parameter_WithoutDefaultAfterOneWithDefault_IsRejectedWithRequiredParamAfterOptional`` (source: string) =
    assertRejectedWith MessageCode.RequiredParamAfterOptional Script.defaults source |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-298")>]
[<InlineData("func f(a, b = 1) return [a, b] end\nOutput.r = f(0)", "[0,1]")>]
[<InlineData("func f(a = 1, b = 2) return [a, b] end\nOutput.r = f()", "[1,2]")>]
[<InlineData("func f(a, b, c = 1) return [a, b, c] end\nOutput.r = f(0, 0)", "[0,0,1]")>]
let ``Parameters_WithDefaultsOnlyAtTheEnd_Compile`` (source: string, expected: string) =
    assertR expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-299")>]
[<InlineData("1", "1")>]
[<InlineData("0x10", "16")>]
[<InlineData("0b11", "3")>]
[<InlineData("1.5e3", "1500")>]
[<InlineData("\"s\"", "\"s\"")>]
[<InlineData("'s'", "\"s\"")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("null", "null")>]
[<InlineData("-1", "-1")>]
[<InlineData("- 1", "-1")>]
[<InlineData("-0x10", "-16")>]
[<InlineData("-0b11", "-3")>]
[<InlineData("-1.5e3", "-1500")>]
[<InlineData("-0", "0")>]
let ``JYRO-REQ-299 DefaultValue_ThatIsALiteralOrMinusANumberLiteral_IsAccepted`` (value: string, expected: string) =
    assertR expected (sprintf "func f(a = %s) return a end\nOutput.r = f()" value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-299")>]
let ``JYRO-REQ-299 DefaultValue_MinusANumberLiteralWithATypeHint_IsAccepted`` () =
    assertR "-1" "func f(a: number = -1) return a end\nOutput.r = f()"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-299")>]
[<InlineData("1 + 1")>]
[<InlineData("x")>]
[<InlineData("-(1)")>]
[<InlineData("-true")>]
[<InlineData("[1]")>]
[<InlineData("{}")>]
[<InlineData("(1)")>]
[<InlineData("- -1")>]
[<InlineData("-\"a\"")>]
[<InlineData("-null")>]
[<InlineData("g()")>]
[<InlineData("x => 1")>]
[<InlineData("\"a\" & \"b\"")>]
[<InlineData("not true")>]
[<InlineData("null ?? 1")>]
let ``JYRO-REQ-299 DefaultValue_ThatIsNotALiteral_IsRejectedWithDefaultValueNotLiteral`` (value: string) =
    let source = sprintf "func g() return 1 end\nfunc f(a = %s) return a end" value
    // A test of JYRO-REQ-299 alone (JYRO-ADR-113 item 4): it reads the stage, which 160.1.12
    // (JYRO-REQ-1670) gives, so 299 depends on 1670, not the other way.
    let r = assertRejectedBeforeRun Script.defaults source
    Assert.True(List.contains MessageCode.DefaultValueNotLiteral (Script.errorCodes r), Script.describe r)
    // Rejected at validation, not at parse (80.4.5 and its query answer): no
    // diagnostic has the stage Parse (160.1.12), read from the diagnostic's own
    // stage, not from a code (d-570, FLOORS 4.1.16).
    let parse = [ for m in r.Messages do if Pending.stageOf m = "Parse" then yield m.Code ]
    Assert.True(List.isEmpty parse, sprintf "Expected no diagnostic of the stage Parse, got %A. Diagnostics:\n%s" parse (Script.describe r))
    // "Validation MUST reject": DefaultValueNotLiteral has the stage Validate (160.1.12).
    assertStageOfCode MessageCode.DefaultValueNotLiteral "Validate" r

// ===========================================================================
// 80.5 Default parameter values
// ===========================================================================

let private fDecl = "func f(a, b = 10, c = \"x\") return [a, b, c] end\n"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-300")>]
[<InlineData("f(1)", "[1,10,\"x\"]")>]
[<InlineData("f(1, 2)", "[1,2,\"x\"]")>]
[<InlineData("f(a: 1, c: \"y\")", "[1,10,\"y\"]")>]
[<InlineData("f(a: 1)", "[1,10,\"x\"]")>]
[<InlineData("f(c: \"y\", a: 1)", "[1,10,\"y\"]")>]
let ``Parameter_WithDefault_GetsTheDefaultWhenNoArgumentIsSupplied`` (call: string, expected: string) =
    assertR expected (fDecl + "Output.r = " + call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-300")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
[<InlineData("null", "null")>]
[<InlineData("\"s\"", "\"s\"")>]
[<InlineData("0xFF", "255")>]
let ``Parameter_WithADefaultOfEachLiteralKind_GetsItWhenOmitted`` (value: string, expected: string) =
    assertR expected (sprintf "func f(a = %s) return a end\nOutput.r = f()" value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-300")>]
let ``Parameter_WithDefault_GetsAFreshDefaultOnEveryCall`` () =
    assertR "[\"x!\",\"x!\"]" "func f(s = \"x\") s = s & \"!\"\nreturn s end\nOutput.r = [f(), f()]"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-301")>]
[<InlineData("f(1, null)", "[1,10,\"x\"]")>]
[<InlineData("f(1, null, null)", "[1,10,\"x\"]")>]
[<InlineData("f(a: 1, b: null)", "[1,10,\"x\"]")>]
[<InlineData("f(a: 1, b: null, c: null)", "[1,10,\"x\"]")>]
[<InlineData("f(1, n, n)", "[1,10,\"x\"]")>]
let ``Parameter_WithDefault_GetsTheDefaultWhenTheCallSuppliesNull`` (call: string, expected: string) =
    assertR expected (fDecl + "var n = null\nOutput.r = " + call)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-301")>]
let ``Parameter_WithDefaultNull_GivenNull_IsNull`` () =
    assertR "null" "func f(a = null) return a end\nOutput.r = f(null)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-301")>]
let ``Parameter_WithoutDefault_GivenNull_KeepsNull`` () =
    assertR "[null,10,\"x\"]" (fDecl + "Output.r = f(null)")

// ===========================================================================
// 80.6 Type hints on parameters
// ===========================================================================

let private invalid = "!InvalidType"

let assertHinted (source: string) (expected: string) =
    let r = Script.run source
    if expected = invalid then
        Script.expectError MessageCode.InvalidType r
    else
        let v = outProp "r" r
        Assert.True(v.EqualsValue(Values.json expected), sprintf "expected %s, got %s" expected (Values.toJson v))
        Assert.Equal((Values.json expected).ValueType, v.ValueType)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-302")>]
[<InlineData("n: number", "5", "5")>]
[<InlineData("n: number", "\" 5 \"", "5")>]
[<InlineData("n: number", "\"1e3\"", "1000")>]
[<InlineData("n: number", "true", "1")>]
[<InlineData("n: number", "false", "0")>]
[<InlineData("n: number", "\"abc\"", "!InvalidType")>]
[<InlineData("n: number", "\"1e400\"", "!InvalidType")>]
[<InlineData("n: number", "null", "!InvalidType")>]
[<InlineData("n: number", "[1]", "!InvalidType")>]
[<InlineData("n: string", "1.5", "\"1.5\"")>]
[<InlineData("n: string", "true", "\"true\"")>]
[<InlineData("n: string", "{}", "!InvalidType")>]
[<InlineData("n: string", "null", "!InvalidType")>]
[<InlineData("n: boolean", "\"TRUE\"", "true")>]
[<InlineData("n: boolean", "\"False\"", "false")>]
[<InlineData("n: boolean", "\"yes\"", "!InvalidType")>]
[<InlineData("n: boolean", "0", "false")>]
[<InlineData("n: boolean", "2", "true")>]
[<InlineData("n: array", "{}", "!InvalidType")>]
[<InlineData("n: array", "[1]", "[1]")>]
[<InlineData("n: object", "[]", "!InvalidType")>]
[<InlineData("n: object", "{a: 1}", "{\"a\":1}")>]
[<InlineData("n: any", "\"5\"", "\"5\"")>]
[<InlineData("n: any", "null", "null")>]
[<InlineData("n: null", "1", "!InvalidType")>]
[<InlineData("n: number = 3", "", "3")>]
[<InlineData("n: number = 3", "null", "3")>]
[<InlineData("n: number = \"7\"", "", "7")>]
[<InlineData("n: number = 3", "n: \"4\"", "4")>]
[<InlineData("n: boolean = \"true\"", "null", "true")>]
let ``Argument_ForAHintedParameter_IsCoercedAfterDefaultsApply`` (parameter: string, argument: string, expected: string) =
    assertHinted (sprintf "func f(%s) return n end\nOutput.r = f(%s)" parameter argument) expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-302")>]
let ``Argument_ForAFunctionHintedParameter_AcceptsALambda`` () =
    assertR "\"function\"" "func f(x: function) return TypeOf(x) end\nOutput.r = f(y => y)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-303")>]
[<InlineData("n: number", "1", "n = \"5\"", "5")>]
[<InlineData("n: number", "1", "n = \"abc\"", "!InvalidType")>]
[<InlineData("n: number", "1", "n &= \"5\"", "15")>]
[<InlineData("n: number", "1", "n += 1", "2")>]
[<InlineData("n: number", "1", "n++", "2")>]
[<InlineData("n: number", "1", "n--", "0")>]
[<InlineData("n: number", "1", "n = true", "1")>]
[<InlineData("n: number", "1", "n = null", "!InvalidType")>]
[<InlineData("n: string", "\"a\"", "n = 12", "\"12\"")>]
[<InlineData("n: string", "\"a\"", "n &= 1", "\"a1\"")>]
[<InlineData("n: string", "\"a\"", "n = [1]", "!InvalidType")>]
[<InlineData("n: boolean", "false", "n = \"true\"", "true")>]
[<InlineData("n: boolean", "false", "n = 3", "true")>]
[<InlineData("n: boolean", "true", "n &= \"x\"", "!InvalidType")>]
[<InlineData("n: number", "1", "if true then n = \"7\" end", "7")>]
[<InlineData("n: number", "1", "var i = 0\nwhile i < 1 do\n  n = \"8\"\n  i = i + 1\nend", "8")>]
[<InlineData("n: any", "1", "n = \"5\"", "\"5\"")>]
[<InlineData("n: any", "1", "n = \"s\"\nn = [1]", "[1]")>]
let ``HintedParameter_InTheBody_EveryLaterWriteIsCoerced`` (parameter: string, argument: string, body: string, expected: string) =
    assertHinted (sprintf "func f(%s)\n%s\nreturn n end\nOutput.r = f(%s)" parameter body argument) expected

[<Fact>]
[<Trait("requirement", "JYRO-REQ-303")>]
let ``Hint_OnOneParameter_DoesNotApplyToAnotherParameterOrACallerVariable`` () =
    assertR "[\"5\",\"6\"]"
        (lines [ "func f(n: number, m) m = \"5\""
                 "return m end"
                 "var n = \"x\""
                 "var r1 = f(1, 2)"
                 "n = \"6\""
                 "Output.r = [r1, n]" ])

let mustValidate : obj[] seq =
    rows
        [ "func f(n: number) return n & \"x\" end"
          "func f(s: string) return s * 2 end"
          "var n: number = \"abc\""
          "var s: string = [1]"
          "func f(a: array) return a.x end"
          "var b: boolean = 5"
          "func f(n: number) return n end\nfunc g(s: string) return f(s) end"
          "var n: number = 1\nn = \"q\""
          "var s: string = \"a\"\nOutput.r = s & 1"
          "var o: object = []\nvar a: array = {}"
          "func f(x: null) return x end\nOutput.r = f(1)"
          // Other hints on `+=`, `++` and `--` do not reject (only `string` does, 40.2.8, 40.2.9).
          "var n: number = Input.x\nn++"
          "var a: any = Input.x\na++"
          "var b: boolean = Input.x\nb += 1"
          "var o: object = Input.x\n--o"
          "var n: number = Input.x\nn += 1"
          // Hints in the forms 50.1.3 does not bind, and on an assignment 50.2.1 does not reject.
          "func f(s: string) return s end"
          "func f(n: number = \"x\") return n end"
          "union U V(s: string, n: number) end\nOutput.r = V(1, \"a\")"
          "var o = {}\no.a: number = \"1\""
          "var x = 1\nvar o = {}\no.a: boolean = x"
          // A hint other than `string` on an operand of `+` does not reject.
          "var b: boolean = Input.x\nOutput.r = b + 1"
          "var a: array = Input.x\nOutput.r = a + 1"
          "var n: null = null\nOutput.r = n + 1"
          "var f: function = Input.x\nOutput.r = f + 1" ]

[<Theory>]
[<Trait("requirement", "JYRO-REQ-304")>]
[<MemberData(nameof mustValidate)>]
let ``JYRO-REQ-304 TypeHint_ThatCouldFailAtRunTime_DoesNotMakeValidationReject`` (source: string) =
    assertCompiles (Script.compile source)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-304")>]
[<InlineData("var n: number", "TypedVariableNeedsInitializer")>]
[<InlineData("var s: string", "TypedVariableNeedsInitializer")>]
[<InlineData("var s: string = Input.x\nOutput.r = 1 + s", "StringOperandInAddition")>]
[<InlineData("var n = 1\nn: number = 2", "DuplicateTypeHint")>]
[<InlineData("var o = {}\no.a: number += 1", "TypeHintOnCompoundAssignment")>]
[<InlineData("var s: string = Input.x\nOutput.r = s + 1", "StringOperandInAddition")>]
[<InlineData("var s: string = Input.x\ns += 1", "StringOperandInAddition")>]
[<InlineData("var s: string = Input.x\ns++", "StringOperandInAddition")>]
[<InlineData("var s: string = Input.x\ns--", "StringOperandInAddition")>]
[<InlineData("var s: string = Input.x\n++s", "StringOperandInAddition")>]
[<InlineData("var s: string = Input.x\n--s", "StringOperandInAddition")>]
let ``JYRO-REQ-304 TypeHint_InEachOfTheFourExceptions_IsRejectedWithItsCode`` (source: string, code: string) =
    // The code by the name the book gives it, not by a number (d-556, 10.1.1).
    let r = Script.compile source
    Assert.Equal<MessageCode list>([ Enum.Parse<MessageCode> code ], Script.errorCodes r)
    Assert.False(r.IsSuccess)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-304")>]
let ``JYRO-REQ-304 TypeHint_AnyWithoutInitializer_Compiles`` () =
    assertCompiles (Script.compile "var n: any")

// ===========================================================================
// 80.7 Arguments at the call site
// ===========================================================================

let private h2 () =
    Host.func "H2" [ Parameter.Required("a", ParameterType.Any); Parameter.Required("b", ParameterType.Any) ]
        ParameterType.Any FunctionEffect.Pure (fun _ -> Values.nul)

let private withH2 () = { Script.defaults with Functions = [ (h2 ()).AsFunction ] }

let private decl = "func f(a, b, c = 1) return [a, b, c] end\n"

let placements (call: string) : string list =
    [ "Output.r = " + call
      "if false then Output.r = " + call + " end"
      "func never() return " + call + " end" ]

/// Rejected before running, every Error being `code` (a call may break the
/// same rule more than once, as a named call omitting two parameters does).
let private assertOnlyCode (code: MessageCode) (options: RunOptions) (source: string) =
    let r = assertRejectedBeforeRun options source
    Assert.Equal<MessageCode list>([ code ], Script.errorCodes r |> List.distinct)

let private assertLinkRejected (code: MessageCode) (options: RunOptions) (call: string) =
    for source in placements call do
        assertOnlyCode code options (decl + source)

let private assertStageLink (options: RunOptions) (call: string) =
    let r = Script.compileWith options (decl + "Output.r = " + call)
    Assert.Equal("Link", Pending.stageOf (Script.onlyErrorMessage r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1815")>]
[<InlineData("f(1, b: 2)")>]
[<InlineData("f(a: 1, 2)")>]
[<InlineData("Substring(\"abc\", start: 1)")>]
[<InlineData("Substring(text: \"abc\", 1)")>]
[<InlineData("H2(1, b: 2)")>]
let ``JYRO-REQ-1815 Call_MixingPositionalAndNamedArguments_IsAParseErrorAtParseEvenInDeadCode`` (call: string) =
    for source in placements call do
        assertParseError (withH2 ()) (decl + source)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1815")>]
[<InlineData("f(1, 2)", "[1,2,1]")>]
[<InlineData("f(a: 1, b: 2)", "[1,2,1]")>]
let ``JYRO-REQ-1815 Call_AllPositionalOrAllNamed_Compiles`` (call: string, expected: string) =
    assertR expected (decl + "Output.r = " + call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-306")>]
[<InlineData("f()")>]
[<InlineData("f(1)")>]
[<InlineData("Substring(\"a\")")>]
[<InlineData("Length()")>]
[<InlineData("H2(1)")>]
let ``JYRO-REQ-306 PositionalCall_WithTooFewArguments_IsRejectedAtLinkTimeWithTooFewArguments`` (call: string) =
    assertLinkRejected MessageCode.TooFewArguments (withH2 ()) call

[<Theory>]
[<Trait("requirement", "JYRO-REQ-306")>]
[<InlineData("f(1)")>]
[<InlineData("H2(1)")>]
let ``JYRO-REQ-306 PositionalCall_WithTooFewArguments_IsReportedByTheLinkStage`` (call: string) =
    assertStageLink (withH2 ()) call

[<Fact>]
[<Trait("requirement", "JYRO-REQ-306")>]
let ``JYRO-REQ-306 PositionalCall_WithExactlyTheRequiredArguments_Compiles`` () =
    assertR "[1,2,1]" (decl + "Output.r = f(1, 2)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-307")>]
[<InlineData("f(1, 2, 3, 4)")>]
[<InlineData("g(1)")>]
[<InlineData("Length([1], 2)")>]
[<InlineData("Now(1)")>]
[<InlineData("H2(1, 2, 3)")>]
let ``JYRO-REQ-307 PositionalCall_WithTooManyArguments_IsRejectedAtLinkTimeWithTooManyArguments`` (call: string) =
    for source in placements call do
        assertOnlyCode MessageCode.TooManyArguments (withH2 ()) (decl + "func g() return 0 end\n" + source)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-307")>]
[<InlineData("f(1, 2, 3, 4)")>]
[<InlineData("H2(1, 2, 3)")>]
let ``JYRO-REQ-307 PositionalCall_WithTooManyArguments_IsReportedByTheLinkStage`` (call: string) =
    assertStageLink (withH2 ()) call

[<Fact>]
[<Trait("requirement", "JYRO-REQ-307")>]
let ``JYRO-REQ-307 PositionalCall_WithEveryParameter_Compiles`` () =
    assertR "[1,2,3]" (decl + "Output.r = f(1, 2, 3)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-308")>]
[<InlineData("f(a: 1, b: 2, d: 2)")>]
[<InlineData("f(a: 1, b: 2, A: 1)")>]
[<InlineData("f(a: 1, b: 2, then: 3)")>]
[<InlineData("Length(source: [1], src: [1])")>]
[<InlineData("H2(a: 1, b: 2, x: 2)")>]
let ``JYRO-REQ-308 NamedArgument_NamingNoParameter_IsRejectedAtLinkTimeWithUnknownNamedArgument`` (call: string) =
    assertLinkRejected MessageCode.UnknownNamedArgument (withH2 ()) call

[<Fact>]
[<Trait("requirement", "JYRO-REQ-308")>]
let ``JYRO-REQ-308 NamedArgument_NamingNoParameter_IsReportedByTheLinkStage`` () =
    assertStageLink Script.defaults "f(a: 1, b: 2, d: 2)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-309")>]
[<InlineData("f(a: 1, a: 2, b: 3)")>]
[<InlineData("f(b: 1, a: 1, b: 2)")>]
[<InlineData("Length(source: [1], source: [2])")>]
[<InlineData("H2(a: 1, b: 2, a: 3)")>]
let ``JYRO-REQ-309 Call_NamingTheSameArgumentTwice_IsRejectedAtLinkTimeWithDuplicateNamedArgument`` (call: string) =
    assertLinkRejected MessageCode.DuplicateNamedArgument (withH2 ()) call

[<Fact>]
[<Trait("requirement", "JYRO-REQ-309")>]
let ``JYRO-REQ-309 Call_NamingTheSameArgumentTwice_IsReportedByTheLinkStage`` () =
    assertStageLink Script.defaults "f(a: 1, a: 2, b: 3)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-310")>]
[<InlineData("f(b: 1)")>]
[<InlineData("f(c: 1)")>]
[<InlineData("f(a: 1, c: 2)")>]
[<InlineData("Substring(start: 1)")>]
[<InlineData("H2(b: 1)")>]
let ``JYRO-REQ-310 NamedCall_OmittingARequiredParameter_IsRejectedAtLinkTimeWithMissingRequiredNamedArgument`` (call: string) =
    assertLinkRejected MessageCode.MissingRequiredNamedArgument (withH2 ()) call

[<Fact>]
[<Trait("requirement", "JYRO-REQ-310")>]
let ``JYRO-REQ-310 NamedCall_OmittingARequiredParameter_IsReportedByTheLinkStage`` () =
    assertStageLink Script.defaults "f(a: 1, c: 2)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-310")>]
let ``JYRO-REQ-310 NamedCall_WithEveryRequiredParameter_Compiles`` () =
    assertR "[1,2,1]" (decl + "Output.r = f(a: 1, b: 2)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-311")>]
[<InlineData("func f(a, b, c = 3) return [a, b, c] end\nOutput.r = f(b: 2, a: 1)", "[1,2,3]")>]
[<InlineData("func f(a, b, c = 3) return [a, b, c] end\nOutput.r = f(c: 9, b: 2, a: 1)", "[1,2,9]")>]
[<InlineData("func f(a, b, c = 3) return [a, b, c] end\nOutput.r = f(a: 1, b: 2)", "[1,2,3]")>]
[<InlineData("Output.r = Substring(start: 1, text: \"abc\")", "\"bc\"")>]
[<InlineData("Output.r = PadLeft(padding: \"*\", length: 3, text: \"a\")", "\"**a\"")>]
let ``NamedCall_EachArgumentBindsToItsNamedParameterWhateverTheOrder`` (source: string, expected: string) =
    assertR expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-311")>]
let ``NamedCall_ToAHostFunction_BindsEachArgumentByName`` () =
    let h =
        Host.func "H2" [ Parameter.Required("a", ParameterType.Any); Parameter.Required("b", ParameterType.Any) ]
            ParameterType.Any FunctionEffect.Pure (fun args -> Values.arr args)
    let r = Script.runWith { Script.defaults with Functions = [ h.AsFunction ] } "Output.r = H2(b: \"B\", a: \"A\")"
    Assert.True((outProp "r" r).EqualsValue(Values.json "[\"A\",\"B\"]"), Values.toJson (outProp "r" r))

// ===========================================================================
// 80.8 Return values
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-312")>]
[<InlineData("return 1")>]
[<InlineData("return")>]
[<InlineData("if true then return 1 end")>]
[<InlineData("while true do return end")>]
[<InlineData("for i in 0 to 1 do return i end")>]
[<InlineData("foreach x in [1] do return x end")>]
[<InlineData("switch 1 do case 1 then return 1 end")>]
[<InlineData("union U A end\nmatch A() do case A then return 1 end")>]
let ``Return_OutsideAFunctionBody_IsRejectedWithReturnOutsideFunction`` (source: string) =
    assertRejectedWith MessageCode.ReturnOutsideFunction Script.defaults source |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-312")>]
let ``Return_InBlocksInsideAFunctionBody_Compiles`` () =
    assertCompiles (
        Script.compile (
            lines [ "union U A end"
                    "func f(x)"
                    "  if x == 1 then return 1 end"
                    "  while x == 2 do return 2 end"
                    "  for i in 0 to 1 do if x == 3 then return 3 end end"
                    "  foreach y in [1] do if x == 4 then return 4 end end"
                    "  switch x do case 5 then return 5 end"
                    "  match A() do case A then return 6 end"
                    "end"
                    "Output.r = f(1)" ]))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-313")>]
[<InlineData("func f() return 1\nreturn 2 end\nOutput.r = f()", "1")>]
[<InlineData("func f()\nvar i = 0\nwhile true do\ni = i + 1\nif i == 3 then return i end\nend\nend\nOutput.r = f()", "3")>]
[<InlineData("func f() foreach x in [1, 2, 3] do if x == 2 then return x end end\nreturn 0 end\nOutput.r = f()", "2")>]
[<InlineData("func f() for i in 0 to 10 do if i == 4 then return i end end\nreturn -1 end\nOutput.r = f()", "4")>]
[<InlineData("func f() if true then if true then return \"in\" end end\nreturn \"out\" end\nOutput.r = f()", "\"in\"")>]
[<InlineData("func f() return \"s\" end\nOutput.r = f()", "\"s\"")>]
[<InlineData("func f() return [1, {a: 2}] end\nOutput.r = f()", "[1,{\"a\":2}]")>]
[<InlineData("func f() return null end\nOutput.r = f()", "null")>]
[<InlineData("func f() return x => x + 1 end\nOutput.r = Map([1], f())", "[2]")>]
[<InlineData("var o = {a: 1}\nfunc f(x) return x end\nvar p = f(o)\np.b = 2\nOutput.r = o", "{\"a\":1,\"b\":2}")>]
let ``Return_WithAValue_EndsTheFunctionAndTheCallGivesTheValue`` (source: string, expected: string) =
    assertR expected source

[<Fact>]
[<Trait("requirement", "JYRO-REQ-313")>]
let ``Return_WithAValue_NoLaterStatementOfTheBodyRuns`` () =
    let r, mark = runMarked Script.defaults "func f() Mark()\nreturn 5\nMark() end\nOutput.r = f()"
    Assert.Equal(5.0, Values.asFloat (outProp "r" r))
    Assert.Equal(1, mark.CallCount)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-314")>]
[<InlineData("func f() return end\nOutput.r = f()")>]
[<InlineData("func f() end\nOutput.r = f()")>]
[<InlineData("func f() var x = 1 end\nOutput.r = f()")>]
[<InlineData("func f() if true then return end\nreturn 1 end\nOutput.r = f()")>]
let ``Return_WithoutAValueOrEndOfBody_GivesNull`` (source: string) =
    assertR "null" source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-314")>]
[<InlineData(true, 0)>]
[<InlineData(false, 1)>]
let ``Return_WithoutAValue_EndsTheFunction`` (flag: bool, marks: int) =
    let r, mark =
        runMarked Script.defaults (sprintf "func f(a) if a then return end\nMark() end\nOutput.r = f(%b)" flag)
    Assert.True((outProp "r" r).IsNull)
    Assert.Equal(marks, mark.CallCount)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-315")>]
[<InlineData("func f() return\n5 end\nOutput.r = f()", "null")>]
[<InlineData("func f() return\r\n5 end\r\nOutput.r = f()", "null")>]
[<InlineData("func f() return 1 +\n2 end\nOutput.r = f()", "3")>]
[<InlineData("func f() return (\n1) end\nOutput.r = f()", "1")>]
[<InlineData("func f() return # comment\n5 end\nOutput.r = f()", "null")>]
[<InlineData("func f() return 7 end\nOutput.r = f()", "7")>]
let ``Return_ValueMustStartOnTheSameLine`` (source: string, expected: string) =
    assertR expected source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-315")>]
[<InlineData("\n")>]
[<InlineData("\r\n")>]
let ``Return_FollowedByALineEnd_HasNoValueAndTheNextLineDoesNotRun`` (eol: string) =
    let r, mark = runMarked Script.defaults (sprintf "func f() return%sMark() end%sOutput.r = f()" eol eol)
    Assert.True((outProp "r" r).IsNull)
    Assert.Equal(0, mark.CallCount)

// ===========================================================================
// 80.9 Recursion and resource limits
// ===========================================================================

let private generous (change: JyroExecutionOptions -> JyroExecutionOptions) =
    Some(Script.limits (fun o -> change { o with MaxLoopIterations = 1000000; MaxStatements = 10000000 }))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1812")>]
let ``JYRO-REQ-1812 Function_CallingItselfDirectly_GivesTheRightResult`` () =
    assertR "3628800" "func fact(n) if n <= 1 then return 1 end\nreturn n * fact(n - 1) end\nOutput.r = fact(10)"

/// The statement's own examples: even/odd and fact, as `Output.x` and `Output.y`.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1812")>]
let ``JYRO-REQ-1812 StatementExamples_EvenOddAndFactorial`` () =
    let r =
        Script.run
            (lines [ "func even(n) if n == 0 then return true end return odd(n - 1) end"
                     "func odd(n) if n == 0 then return false end return even(n - 1) end"
                     "func fact(n) if n <= 1 then return 1 end return n * fact(n - 1) end"
                     "Output.x = even(10)"
                     "Output.y = fact(5)" ])
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.True(Values.asBool (outProp "x" r))
    Assert.Equal(120.0, Values.asFloat (outProp "y" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1812")>]
let ``JYRO-REQ-1812 Function_CallingItselfThroughAnother_GivesTheRightResult`` () =
    assertR "20"
        "func a(n) if n <= 0 then return 0 end\nreturn 1 + b(n - 1) end\nfunc b(n) if n <= 0 then return 0 end\nreturn 1 + a(n - 1) end\nOutput.r = a(20)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1812")>]
let ``JYRO-REQ-1812 Function_RecursingWithinTheCallDepthLimit_Succeeds`` () =
    let r =
        Script.runWith { Script.defaults with Limits = generous (fun o -> { o with MaxCallDepth = 64 }) }
            "func s(n) if n <= 0 then return 0 end\nreturn n + s(n - 1) end\nOutput.r = s(50)"
    Assert.Equal(1275.0, Values.asFloat (outProp "r" r))

/// A script whose script-function calls nest `depth` deep, whose innermost
/// function body then evaluates `inner` (a call or a literal).
let chain (depth: int) (inner: string) : string =
    if depth = 0 then sprintf "Output.v = %s" inner
    else sprintf "func r(k) if k <= 1 then return %s end\nreturn r(k - 1) end\nOutput.v = r(%d)" inner depth

let depthLimits () : obj[] seq =
    [ 1; 5; 64 ] |> List.map (fun n -> min n JyroExecutionOptions.MaxSafeCallDepth) |> List.distinct |> rows

let private hostH () = Host.func "H" [] ParameterType.Any FunctionEffect.Pure (fun _ -> Values.int 1)

let private depthRun (n: int) (source: string) =
    Script.runWith
        { Script.defaults with Limits = generous (fun o -> { o with MaxCallDepth = n }); Functions = [ (hostH ()).AsFunction ] }
        source

[<Theory>]
[<Trait("requirement", "JYRO-REQ-317")>]
[<MemberData(nameof depthLimits)>]
let ``JYRO-REQ-317 ScriptCalls_NestedToTheLimit_SucceedAndOneDeeperFailsWithCallDepthLimitExceeded`` (n: int) =
    let ok = depthRun n (chain n "1")
    Assert.True(ok.IsSuccess, Script.describe ok)
    Script.expectError MessageCode.CallDepthLimitExceeded (depthRun n (chain (n + 1) "1"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-317")>]
[<MemberData(nameof depthLimits)>]
let ``JYRO-REQ-317 StdlibCall_AtTheLimitAddsOneToTheCallDepth`` (n: int) =
    let ok = depthRun n (chain (n - 1) "Length([1])")
    Assert.True(ok.IsSuccess, Script.describe ok)
    Script.expectError MessageCode.CallDepthLimitExceeded (depthRun n (chain n "Length([1])"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-317")>]
[<MemberData(nameof depthLimits)>]
let ``JYRO-REQ-317 HostCall_AtTheLimitAddsOneToTheCallDepth`` (n: int) =
    let ok = depthRun n (chain (n - 1) "H()")
    Assert.True(ok.IsSuccess, Script.describe ok)
    Script.expectError MessageCode.CallDepthLimitExceeded (depthRun n (chain n "H()"))

/// A function that calls itself: each call gives the value of its body while the
/// depth stays within the limit (80.9.2), and the call one deeper ends the run.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1812")>]
[<MemberData(nameof depthLimits)>]
let ``JYRO-REQ-1812 DirectRecursion_ToTheLimit_GivesTheValueAndOneDeeperFails`` (n: int) =
    let src (k: int) = sprintf "func s(k) if k <= 1 then return 1 end\nreturn 1 + s(k - 1) end\nOutput.v = s(%d)" k
    let ok = depthRun n (src n)
    Assert.True(ok.IsSuccess, Script.describe ok)
    Assert.Equal(float n, Values.asFloat (outProp "v" ok))
    Script.expectError MessageCode.CallDepthLimitExceeded (depthRun n (src (n + 1)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-317")>]
let ``JYRO-REQ-317 VariantCall_AddsOneToTheCallDepth`` () =
    let inFunction = "union U V(x) end\nfunc W() return V(1) end\nOutput.v = W()"
    Script.expectError MessageCode.CallDepthLimitExceeded (depthRun 1 inFunction)
    let atTwo = depthRun 2 inFunction
    Assert.True(atTwo.IsSuccess, Script.describe atTwo)
    let topLevel = depthRun 1 "union U V(x) end\nOutput.v = V(1)"
    Assert.True(topLevel.IsSuccess, Script.describe topLevel)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-317")>]
let ``JYRO-REQ-317 CallInAnArgument_StartsAfterTheOuterCallsArgumentsAtTheOuterDepth`` () =
    let r =
        depthRun 1
            (lines [ "func G() return 1 end"
                     "func F(a) return a end"
                     "union U V(x) end"
                     "Output.p = F(G())"
                     "Output.n = F(a: G())"
                     "Output.v = V(G())"
                     "Output.s = F(Length([1]))"
                     "Output.h = F(H())" ])
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1.0, Values.asFloat (outProp "p" r))
    Assert.Equal(1.0, Values.asFloat (outProp "n" r))
    Assert.Equal(1.0, Values.asFloat (Values.prop "x" (outProp "v" r)))
    Assert.Equal(1.0, Values.asFloat (outProp "s" r))
    Assert.Equal(1.0, Values.asFloat (outProp "h" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-317")>]
let ``JYRO-REQ-317 Calls_WhenTheyReturn_SubtractFromTheCallDepth`` () =
    let r =
        depthRun 2
            (lines [ "func f() return Length([1]) + H() end"
                     "var i = 0"
                     "var s = 0"
                     "while i < 500 do"
                     "  s = s + f()"
                     "  i = i + 1"
                     "end"
                     "Output.v = s" ])
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(1000.0, Values.asFloat (outProp "v" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-317")>]
let ``JYRO-REQ-317 CallDepth_InTwoConcurrentExecutions_IsCountedPerExecution`` () =
    let n = min 8 JyroExecutionOptions.MaxSafeCallDepth
    let gate = Gate()
    let opts = { Script.defaults with Limits = generous (fun o -> { o with MaxCallDepth = n }); Functions = [ gate.AsFunction ] }
    let prog =
        program opts
            (lines [ "func a(k) if k <= 1 then return Hold() end"
                     "return a(k - 1) end"
                     "func r(k) if k <= 1 then return 1 end"
                     "return r(k - 1) end"
                     sprintf "if Input.tag == \"A\" then a(%d) end" (n - 1)
                     sprintf "Output.v = r(%d)" n ])
    let a = background (fun () -> execute { opts with Input = JsonInput "{\"tag\":\"A\"}" } prog)
    gate.WaitEntered()
    let b = execute { opts with Input = JsonInput "{\"tag\":\"B\"}" } prog
    gate.Release()
    let a = await a
    Assert.True(b.IsSuccess, "B: " + Script.describe b)
    Assert.True(a.IsSuccess, "A: " + Script.describe a)

let private statsRun (limits: JyroExecutionOptions option) (source: string) =
    let stats = JyroPipelineStats()
    let r = Script.runWith { Script.defaults with Stats = Some stats; Limits = limits } source
    r, stats

[<Fact>]
[<Trait("requirement", "JYRO-REQ-318")>]
let ``StatementsInAFunctionBody_CountLikeTopLevelStatements`` () =
    let decl = "func f()\nvar a = 1\nvar b = 2\nvar c = 3\nend\n"
    let r1, s1 = statsRun None (decl + "f()")
    let r2, s2 = statsRun None (decl + "f()\nf()")
    Assert.True(r1.IsSuccess && r2.IsSuccess)
    Assert.Equal(4L, s2.StatementCount - s1.StatementCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-318")>]
let ``StatementLimit_ReachedInsideAFunctionBody_RaisesStatementLimitExceeded`` () =
    let source = "func f()\nvar a = 1\nvar b = 2\nvar c = 3\nvar d = 4\nend\nf()\nf()"
    let _, stats = statsRun None source
    Assert.Equal(10L, stats.StatementCount)
    let limit n = Some(Script.limits (fun o -> { o with MaxStatements = n }))
    let ok, _ = statsRun (limit 10) source
    Assert.True(ok.IsSuccess, Script.describe ok)
    let over, _ = statsRun (limit 9) source
    Script.expectError MessageCode.StatementLimitExceeded over

[<Fact>]
[<Trait("requirement", "JYRO-REQ-318")>]
let ``LoopIterationsInAFunctionBody_CountAgainstTheLoopLimit`` () =
    let source = "func f()\nvar i = 0\nwhile i < 5 do i = i + 1 end\nend\nf()"
    let _, stats = statsRun None source
    Assert.Equal(5L, stats.LoopIterations)
    let limit n = Some(Script.limits (fun o -> { o with MaxLoopIterations = n }))
    let ok, _ = statsRun (limit 5) source
    Assert.True(ok.IsSuccess, Script.describe ok)
    let over, _ = statsRun (limit 4) source
    Script.expectError MessageCode.LoopIterationLimitExceeded over

[<Fact>]
[<Trait("requirement", "JYRO-REQ-318")>]
let ``LoopIterationsInAFunctionAndAtTopLevel_CountTogether`` () =
    let source =
        "func f()\nvar i = 0\nwhile i < 5 do i = i + 1 end\nend\nvar j = 0\nwhile j < 3 do\nf()\nj = j + 1\nend"
    let _, stats = statsRun None source
    Assert.Equal(18L, stats.LoopIterations)
    let limit n = Some(Script.limits (fun o -> { o with MaxLoopIterations = n }))
    let ok, _ = statsRun (limit 18) source
    Assert.True(ok.IsSuccess, Script.describe ok)
    let over, _ = statsRun (limit 17) source
    Script.expectError MessageCode.LoopIterationLimitExceeded over

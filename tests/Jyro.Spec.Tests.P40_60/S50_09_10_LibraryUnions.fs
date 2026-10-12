/// A library union has one scope across the compilation (JYRO-ADR-121, for JYRO-DEF-525 and
/// JYRO-DEF-526). 50.10.3 DuplicateUnionName across source texts (JYRO-REQ-1688), 50.10.4
/// DuplicateVariantName across source texts (JYRO-REQ-1689) and 50.9.3 the union of a match
/// and MatchUnknownVariant, with a declared variant being one declared in any source text
/// (JYRO-REQ-1690). Written from the requirements alone.
///
/// A diagnostic names no source text. Every declaration and every match case of a case is
/// therefore put on a line that no other declaration or case of that case uses, so that the
/// line tells which source text a diagnostic is on. The diagnostics are compared in the order
/// of 130.5.4 (JYRO-REQ-1489): each library in the order the host added it, then the script,
/// and within one source text by position.
module RealTree.Jyro.Spec.Tests.P40_60.S50_09_10_LibraryUnions

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

// -- Helpers ---------------------------------------------------------------------------------

/// `text` starting on line `n` of its source text (n - 1 blank lines before it).
let private at (n: int) (text: string) : string = String.replicate (n - 1) "\n" + text

/// Compile `script` with the library scripts `libs`, added in that order, under `limited`.
let private compileL (libs: string list) (script: string) : JyroResult<CompiledProgram> =
    Script.compileWith { limited with Libraries = libs } script

/// The two ways a host adds a library to a compilation (160.6, JYRO-REQ-2125): from source with
/// AddLibrarySource, or parsed ahead with JyroLibrary.Parse and then added with AddLibrary.
type private Way =
    | FromSource
    | ParsedAhead

/// `lib` parsed ahead. A library whose matches name another text's variants, or whose calls name
/// another text's functions, is parsed with no diagnostic (JYRO-REQ-1698): those checks are the
/// compilation's.
let private parsedAhead (lib: string) : JyroLibrary =
    let r = JyroLibrary.Parse lib
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "JyroLibrary.Parse reported diagnostics for a library whose only faults depend on the compilation (JYRO-REQ-1698):\n%s\n--- library ---\n%s"
                        (Script.describe r) lib)
    Script.valueOf r

/// Compile `script` with `libs` added in that order, by `way`, under `limited`.
let private compileBy (way: Way) (libs: string list) (script: string) : JyroResult<CompiledProgram> =
    match way with
    | FromSource -> compileL libs script
    | ParsedAhead ->
        let parsed = libs |> List.map parsedAhead
        let compiler = parsed |> List.fold (fun (c: JyroCompiler) l -> c.AddLibrary l) (Script.compilerFor limited)
        compiler.WithSource(script).Compile()

let private errorsOf (r: JyroResult<'T>) : DiagnosticMessage list =
    r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq

let private lineOf (m: DiagnosticMessage) : int = if m.Location.HasValue then m.Location.Value.Line else -1

/// Every Error as (code, line), in the result's order.
let private placedErrors (r: JyroResult<'T>) : (MessageCode * int) list =
    errorsOf r |> List.map (fun m -> m.Code, lineOf m)

/// Every diagnostic, for failure text: severity, code, stage, line:column, message.
let private shown (r: JyroResult<'T>) : string =
    r.Messages
    |> Seq.map (fun m ->
        let pos = if m.Location.HasValue then sprintf "%d:%d" m.Location.Value.Line m.Location.Value.Column else "-"
        sprintf "%A %A [%s] at %s: %s" m.Severity m.Code (Pending.stageOf m) pos m.Message)
    |> String.concat "\n"

let private describeCase (libs: string list) (script: string) : string =
    let ls = libs |> List.mapi (fun i l -> sprintf "--- library added %d ---\n%s" (i + 1) l) |> String.concat "\n"
    sprintf "%s\n--- script ---\n%s" ls script

/// The codes that a repetition of a union or variant name MUST NOT be reported with
/// (JYRO-REQ-1689), whatever else is reported.
let private notForARepetition = [ MessageCode.DuplicateFunction; MessageCode.VariantShadowsFunction ]

let private failsBeforeLink (what: string) (r: JyroResult<'T>) (ctx: unit -> string) : unit =
    if r.IsSuccess then Values.fail (sprintf "%s: expected the compile to fail, but it succeeded. %s" what (ctx ()))
    for code in notForARepetition do
        if List.contains code (Script.codes r) then
            Values.fail (sprintf "%s: %A is reported (JYRO-REQ-1689 forbids it for a repeated name). %s" what code (ctx ()))
    for m in errorsOf r do
        if Pending.stageOf m <> "Validate" then
            Values.fail (sprintf "%s: %A has the stage %s, not Validate. %s" what m.Code (Pending.stageOf m) (ctx ()))
    if r.Messages |> Seq.exists (fun m -> Pending.stageOf m = "Link") then
        Values.fail (sprintf "%s: a diagnostic of stage Link is reported, though Validate failed. %s" what (ctx ()))

/// Assert the compilation fails before Link with exactly the Errors `expected`, as (code, line),
/// in that order; every Error of stage Validate; no diagnostic of stage Link; none of
/// DuplicateFunction or VariantShadowsFunction.
let private exactlyBy (way: Way) (what: string) (expected: (MessageCode * int) list) (libs: string list) (script: string) : unit =
    let r = compileBy way libs script
    let what = sprintf "%s [%A]" what way
    let ctx () = sprintf "\nDiagnostics:\n%s\n%s" (shown r) (describeCase libs script)
    let got = placedErrors r
    if got <> expected then
        Values.fail (sprintf "%s: expected the errors %A (code, line), got %A. %s" what expected got (ctx ()))
    failsBeforeLink what r ctx

let private exactly = exactlyBy FromSource

/// As `exactly`, with the Errors compared as a multiset: for two diagnostics on one line,
/// whose order within the line this test does not fix.
let private exactlyInAnyOrder (what: string) (expected: (MessageCode * int) list) (libs: string list) (script: string) : unit =
    let r = compileL libs script
    let ctx () = sprintf "\nDiagnostics:\n%s\n%s" (shown r) (describeCase libs script)
    let got = placedErrors r
    if List.sort got <> List.sort expected then
        Values.fail (sprintf "%s: expected exactly the errors %A (code, line), got %A. %s" what expected got (ctx ()))
    failsBeforeLink what r ctx

/// Assert the compilation succeeds with no diagnostic of any severity.
let private cleanBy (way: Way) (what: string) (libs: string list) (script: string) : unit =
    let r = compileBy way libs script
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "%s [%A]: expected a clean compile. Diagnostics:\n%s\n%s" what way (shown r) (describeCase libs script))

let private clean = cleanBy FromSource

let private dupUnion = MessageCode.DuplicateUnionName
let private dupVariant = MessageCode.DuplicateVariantName
let private unknown = MessageCode.MatchUnknownVariant
let private undeclared = MessageCode.UndeclaredVariable

// ================================================================================================
// JYRO-REQ-1688 (50.10.3): a union name repeated anywhere in the compilation
// ================================================================================================

/// (1) Both unions in one source text, the script or one library: one, on the second.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 a union name repeated in one source text gives DuplicateUnionName on the second`` () =
    exactly "two in the script" [ dupUnion, 2 ] [] "union U A end\nunion U B end"
    exactly "two in the script, a statement between" [ dupUnion, 3 ] [] "union U A end\nOutput.r = 1\nunion U B end"
    exactly "two in one library" [ dupUnion, 2 ] [ "union U A end\nunion U B end" ] "Output.r = 1"
    exactly "two in the second of two libraries" [ dupUnion, 3 ]
        [ "func F()\n  return 1\nend"; at 2 "union U A end\nunion U B end" ] "Output.r = 1"
    exactly "three in the script" [ dupUnion, 2; dupUnion, 3 ] [] "union U A end\nunion U B end\nunion U C end"

/// (2) A library and the script: one, on the script's declaration, even when the script's
/// declaration is on an earlier line than the library's (the requirement's first example).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 a union of a library repeated in the script gives one DuplicateUnionName on the script's declaration`` () =
    exactly "library line 1, script line 3" [ dupUnion, 3 ] [ at 1 "union U A end" ] (at 3 "union U B end")
    exactly "library line 4, script line 1" [ dupUnion, 1 ] [ at 4 "union U A end" ] (at 1 "union U B end")
    exactly "library line 1, script line 3, then a statement" [ dupUnion, 3 ]
        [ at 1 "union U A end" ] (at 3 "union U B end\nOutput.r = 1")
    exactly "fields, and a library function" [ dupUnion, 2 ]
        [ "union U A(x) end\nfunc F()\n  return 1\nend" ] (at 2 "union U B(y) C end")

/// (3) Two libraries: one, on the declaration of the library added second, in either order
/// (the requirement's second example).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 a union repeated in two libraries gives one DuplicateUnionName on the library added second`` () =
    let l1 = at 1 "union U A end"
    let l2 = at 2 "union U B end"
    exactly "L1 then L2" [ dupUnion, 2 ] [ l1; l2 ] "Output.r = 1"
    exactly "L2 then L1" [ dupUnion, 1 ] [ l2; l1 ] "Output.r = 1"
    // A library between them, declaring another union, changes nothing.
    exactly "L1, other, L2" [ dupUnion, 2 ] [ l1; at 3 "union W C end"; l2 ] "Output.r = 1"

/// (4) Three source texts: two, on the second library's declaration and then the script's.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 a union in two libraries and the script gives DuplicateUnionName on each declaration after the first`` () =
    let l1 = at 1 "union U A end"
    let l2 = at 2 "union U B end"
    exactly "L1, L2, script" [ dupUnion, 2; dupUnion, 3 ] [ l1; l2 ] (at 3 "union U C end")
    exactly "L2, L1, script" [ dupUnion, 1; dupUnion, 3 ] [ l2; l1 ] (at 3 "union U C end")
    exactly "L1, L2, script on line 1" [ dupUnion, 2; dupUnion, 1 ] [ l1; l2 ] (at 1 "union U C end")

/// (5) No repetition: distinct names in three source texts, and names differing only in case.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 distinct union names across source texts are accepted, names differing in case included`` () =
    clean "U, V, W" [ "union U A end"; "union V B end" ] "union W C end"
    clean "U in a library, u in the script" [ "union U A end" ] "union u B end"
    clean "U and u in two libraries" [ "union U A end"; "union u B end" ] "Output.r = 1"

/// (6) The repetition is reported beside another Validate error of the script.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 a union repeated across source texts is reported beside another Validate error`` () =
    exactly "library union, script undeclared variable then union" [ undeclared, 1; dupUnion, 3 ]
        [ at 1 "union U A end" ] "Output.x = zzz\n\nunion U B end"
    exactly "two libraries, script undeclared variable" [ dupUnion, 2; undeclared, 1 ]
        [ at 1 "union U A end"; at 2 "union U B end" ] "Output.x = zzz"

/// The union repeated whole, with its variant: one DuplicateUnionName for U and one
/// DuplicateVariantName for A, both on the later declaration, and nothing else (the
/// requirement's third example).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1688")>]
let ``JYRO-REQ-1688 a union repeated whole gives exactly DuplicateUnionName and DuplicateVariantName on the later declaration`` () =
    exactlyInAnyOrder "library then script, one line each" [ dupUnion, 3; dupVariant, 3 ] [ at 1 "union U A end" ] (at 3 "union U A end")
    exactlyInAnyOrder "library on a later line than the script" [ dupUnion, 1; dupVariant, 1 ] [ at 4 "union U A end" ] (at 1 "union U A end")
    exactlyInAnyOrder "two libraries" [ dupUnion, 2; dupVariant, 2 ] [ at 1 "union U A end"; at 2 "union U A end" ] "Output.r = 1"
    exactlyInAnyOrder "two libraries, the other order" [ dupUnion, 1; dupVariant, 1 ] [ at 2 "union U A end"; at 1 "union U A end" ] "Output.r = 1"
    // The script's declaration over three lines: U on its first, A on its second.
    exactly "library then script over three lines" [ dupUnion, 3; dupVariant, 4 ] [ at 1 "union U A end" ] (at 3 "union U\n  A\nend")

// ================================================================================================
// JYRO-REQ-1689 (50.10.4): a variant name repeated anywhere in the compilation
// ================================================================================================

/// (1) Within one union, in the script and in a library: one, on the second A.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant repeated in one union gives DuplicateVariantName on the second`` () =
    exactly "script, one per line" [ dupVariant, 3 ] [] "union U\n  A\n  A\nend"
    exactly "library, one per line" [ dupVariant, 3 ] [ "union U\n  A\n  A\nend" ] "Output.r = 1"
    exactly "script, one line" [ dupVariant, 1 ] [] "union U A A end"
    exactly "library, one line" [ dupVariant, 1 ] [ "union U A A end" ] "Output.r = 1"

/// (2) Two unions of one source text: one, on V's A.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant repeated in two unions of one source text gives DuplicateVariantName on the later`` () =
    exactly "script" [ dupVariant, 2 ] [] "union U A end\nunion V A end"
    exactly "one library" [ dupVariant, 2 ] [ "union U A end\nunion V A end" ] "Output.r = 1"
    exactly "one library, fields differ" [ dupVariant, 2 ] [ "union U A(x) end\nunion V A(y, z) end" ] "Output.r = 1"

/// (3) A library and the script: one, on the script's A, also when the script's line is earlier
/// (the requirement's first example).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant of a library repeated in the script gives one DuplicateVariantName on the script's`` () =
    exactly "library line 1, script line 3" [ dupVariant, 3 ] [ at 1 "union U A end" ] (at 3 "union V A end")
    exactly "library line 4, script line 1" [ dupVariant, 1 ] [ at 4 "union U A end" ] (at 1 "union V A end")
    exactly "the script calls the variant" [ dupVariant, 3 ] [ at 1 "union U A end" ] (at 3 "union V A end\nOutput.r = A()")
    exactly "fields differ" [ dupVariant, 2 ] [ "union U A(x) end" ] (at 2 "union V B A(y) end")
    exactly "the library has a function too" [ dupVariant, 5 ] [ "func F()\n  return 1\nend\nunion U A end" ] (at 5 "union V A end")

/// (4) Two libraries: one, on the A of the library added second, in either order (the
/// requirement's second example).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant repeated in two libraries gives one DuplicateVariantName on the library added second`` () =
    let l1 = at 1 "union U A end"
    let l2 = at 2 "union V A end"
    exactly "L1 then L2" [ dupVariant, 2 ] [ l1; l2 ] "Output.r = 1"
    exactly "L2 then L1" [ dupVariant, 1 ] [ l2; l1 ] "Output.r = 1"
    exactly "L1 then L2, each with a function" [ dupVariant, 5 ]
        [ "func F()\n  return 1\nend\nunion U A end"; at 5 "union V A end\nfunc G()\n  return 1\nend" ] "Output.r = 1"

/// (5) Three source texts: two, the second library's A and then the script's.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant in two libraries and the script gives DuplicateVariantName on each after the first`` () =
    let l1 = at 1 "union U A end"
    let l2 = at 2 "union V A end"
    exactly "L1, L2, script" [ dupVariant, 2; dupVariant, 3 ] [ l1; l2 ] (at 3 "union W A end")
    exactly "L2, L1, script" [ dupVariant, 1; dupVariant, 3 ] [ l2; l1 ] (at 3 "union W A end")

/// (6) A repeat within the script's union and across texts at once: two, both the script's.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant repeated within the script's union and in a library gives DuplicateVariantName on each of the script's`` () =
    exactly "library A, script V A A" [ dupVariant, 4; dupVariant, 5 ] [ at 1 "union U A end" ] (at 3 "union V\n  A\n  A\nend")

/// (7) Names are compared exactly: A and a are two names.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 variant names differing only in case are accepted across source texts`` () =
    clean "library A, script a" [ "union U A end" ] "union V a end"
    clean "two libraries A and a" [ "union U A end"; "union V a end" ] "Output.r = 1"
    clean "distinct variants in three texts" [ "union U A end"; "union V B end" ] "union W C end"

/// (8) The repetition across a library and the script is reported beside another Validate error
/// of the script (JYRO-DEF-525 found it lost at Link).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a variant repeated across source texts is reported beside another Validate error`` () =
    exactly "library A, script undeclared variable then V A" [ undeclared, 1; dupVariant, 3 ]
        [ at 1 "union U A end" ] "Output.x = zzz\n\nunion V A end"
    exactly "two libraries, script undeclared variable" [ dupVariant, 2; undeclared, 1 ]
        [ at 1 "union U A end"; at 2 "union V A end" ] "Output.x = zzz"

/// The union repeated whole: one DuplicateVariantName on the script's A, beside the
/// DuplicateUnionName for U (the requirement's third example).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1689")>]
let ``JYRO-REQ-1689 a union repeated whole gives one DuplicateVariantName beside DuplicateUnionName`` () =
    exactlyInAnyOrder "library then script" [ dupUnion, 3; dupVariant, 3 ] [ at 1 "union U A end" ] (at 3 "union U A end")
    exactlyInAnyOrder "two libraries" [ dupUnion, 2; dupVariant, 2 ] [ at 1 "union U A end"; at 2 "union U A end" ] "Output.r = 1"
    exactly "script's A on its own line" [ dupUnion, 3; dupVariant, 4 ] [ at 1 "union U A end" ] (at 3 "union U\n  A\nend")
    exactly "two variants, both repeated, one per line" [ dupUnion, 3; dupVariant, 4; dupVariant, 5 ]
        [ at 1 "union U A B end" ] (at 3 "union U\n  A\n  B\nend")

// ================================================================================================
// JYRO-REQ-1690 (50.9.3): a declared variant is one declared in any source text
// ================================================================================================

let private uAB = "union U A B end"

/// The cases, one per line, as a match statement setting `Output.r`.
let private stmtMatch (subject: string) (cases: string list) : string =
    let lines = cases |> List.mapi (fun i c -> sprintf "  case %s then Output.r = %d" c (i + 1))
    "match " + subject + " do\n" + String.concat "\n" lines + "\nend"

/// The cases, one per line, as a match statement whose bodies return, for a function body.
let private stmtReturnMatch (subject: string) (cases: string list) : string =
    let lines = cases |> List.mapi (fun i c -> sprintf "    case %s then return %d" c (i + 1))
    "match " + subject + " do\n" + String.concat "\n" lines + "\n  end"

/// The cases, one per line, as a match expression.
let private exprMatch (subject: string) (cases: string list) : string =
    let lines = cases |> List.mapi (fun i c -> sprintf "  case %s then %d" c (i + 1))
    "match " + subject + " do\n" + String.concat "\n" lines + "\nend"

/// A function `f(u)` of one line that returns the match expression of `cases` on `u`; its first
/// case is on its line 3.
let private matchFn (cases: string list) : string = "func f(u)\n  return " + exprMatch "u" cases + "\nend"

/// A placement of a match: the libraries, the script, and the line (in the source text that
/// holds the match) of its first case, each later case on the next line.
type private Placement = { Name: string; Libs: string list; Script: string; FirstCase: int }

/// The placements of 1690's procedure, (a) to (f), for a match with `cases`.
let private placements (cases: string list) : Placement list =
    let fn = matchFn cases
    [ { Name = "(a) script top level, statement"; Libs = [ uAB ]
        Script = "var u = A()\n" + stmtMatch "u" cases; FirstCase = 3 }
      { Name = "(a) script top level, expression"; Libs = [ uAB ]
        Script = "var u = A()\nOutput.r = " + exprMatch "u" cases; FirstCase = 3 }
      { Name = "(b) a function of the script"; Libs = [ uAB ]
        Script = fn + "\nOutput.r = f(A())"; FirstCase = 3 }
      { Name = "(b) a statement match in a function of the script"; Libs = [ uAB ]
        Script = "func g(u)\n  " + stmtReturnMatch "u" cases + "\n  return 0\nend\nOutput.r = g(A())"; FirstCase = 3 }
      { Name = "(c) a function of the declaring library"; Libs = [ uAB + "\n" + fn ]
        Script = "Output.r = f(A())"; FirstCase = 4 }
      { Name = "(d) a function of a second library added after"; Libs = [ uAB; fn ]
        Script = "Output.r = f(A())"; FirstCase = 3 }
      { Name = "(e) a function of a second library added before"; Libs = [ fn; uAB ]
        Script = "Output.r = f(A())"; FirstCase = 3 }
      { Name = "(f) the union in the script, the match in a library"; Libs = [ fn ]
        Script = uAB + "\nOutput.r = f(A())"; FirstCase = 3 } ]

let private acceptedInEveryPlacement (way: Way) : unit =
    for p in placements [ "A"; "B" ] do
        cleanBy way p.Name p.Libs p.Script
    for p in placements [ "B"; "A" ] do
        cleanBy way (p.Name + ", cases B then A") p.Libs p.Script

let private unknownInEveryPlacement (way: Way) : unit =
    for p in placements [ "A"; "B"; "C" ] do
        exactlyBy way p.Name [ unknown, p.FirstCase + 2 ] p.Libs p.Script
    for p in placements [ "A"; "C"; "B" ] do
        exactlyBy way (p.Name + ", C between") [ unknown, p.FirstCase + 1 ] p.Libs p.Script

let private anotherUnionsVariant (way: Way) : unit =
    let exactly = exactlyBy way
    // U A B in L1, V C in the script: cases A, B, C make the match U's; C is V's.
    exactly "U in L1, V in the script, cases A B C" [ unknown, 6 ]
        [ uAB ] ("union V C end\nvar u = A()\n" + stmtMatch "u" [ "A"; "B"; "C" ])
    // V in a second library, added after or before.
    exactly "U in L1, V in L2, cases A B C" [ unknown, 5 ]
        [ uAB; "union V C end" ] ("var u = A()\n" + stmtMatch "u" [ "A"; "B"; "C" ])
    exactly "V in L1, U in L2, cases A B C" [ unknown, 5 ]
        [ "union V C end"; uAB ] ("var u = A()\n" + stmtMatch "u" [ "A"; "B"; "C" ])
    // The first case names V's C, so the match is V's, and A and B are reported.
    exactly "U in L1, V in the script, cases C A B" [ unknown, 5; unknown, 6 ]
        [ uAB ] ("union V C end\nvar u = C()\n" + stmtMatch "u" [ "C"; "A"; "B" ])
    exactly "U in L1, V in L2, cases C A B" [ unknown, 4; unknown, 5 ]
        [ uAB; "union V C end" ] ("var u = C()\n" + stmtMatch "u" [ "C"; "A"; "B" ])
    // In a third library's function, with U and V in the two others.
    exactly "U in L1, V in L2, match in L3, cases A B C" [ unknown, 5 ]
        [ uAB; "union V C end"; matchFn [ "A"; "B"; "C" ] ] "Output.r = f(A())"

let private firstDeclaredCaseDecides (way: Way) : unit =
    let exactly = exactlyBy way
    exactly "U in L1, cases Z A B in the script" [ unknown, 3 ]
        [ uAB ] ("var u = A()\n" + stmtMatch "u" [ "Z"; "A"; "B" ])
    exactly "U in L1, cases Z A B in L2" [ unknown, 3 ]
        [ uAB; matchFn [ "Z"; "A"; "B" ] ] "Output.r = f(A())"
    exactly "U in the script, cases Z A B in L1" [ unknown, 3 ]
        [ matchFn [ "Z"; "A"; "B" ] ] (uAB + "\nOutput.r = f(A())")

/// Cases A and B, wherever the match is: no diagnostic at all.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 a match naming the variants of a union of another source text is accepted in every placement`` () =
    acceptedInEveryPlacement FromSource

/// A case C that no source text declares gives MatchUnknownVariant on C, at Validate, and only
/// it, wherever the match is.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 a case naming no declared variant gives MatchUnknownVariant in every placement`` () =
    unknownInEveryPlacement FromSource

/// A case naming a variant of another union, declared in another source text, is
/// MatchUnknownVariant: the union of the match is the first case's.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 a case naming a variant of another union of another source text gives MatchUnknownVariant`` () =
    anotherUnionsVariant FromSource

/// The first case that names a declared variant decides the union, across source texts: Z,
/// declared nowhere, does not.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 the first case naming a declared variant of another source text decides the union`` () =
    firstDeclaredCaseDecides FromSource

// -- The same with every library parsed ahead (JyroLibrary.Parse, then AddLibrary): JYRO-DEF-569,
// JYRO-ADR-139. Each library parses with no diagnostic, and the compilation gives what it gives
// with the libraries added from source.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 a match naming the variants of a union of another source text is accepted in every placement, libraries parsed ahead`` () =
    acceptedInEveryPlacement ParsedAhead

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 a case naming no declared variant gives MatchUnknownVariant in every placement, libraries parsed ahead`` () =
    unknownInEveryPlacement ParsedAhead

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 a case naming a variant of another union of another source text gives MatchUnknownVariant, libraries parsed ahead`` () =
    anotherUnionsVariant ParsedAhead

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1690")>]
let ``JYRO-REQ-1690 the first case naming a declared variant of another source text decides the union, libraries parsed ahead`` () =
    firstDeclaredCaseDecides ParsedAhead

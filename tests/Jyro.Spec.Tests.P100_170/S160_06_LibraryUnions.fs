/// 160.6: a union declared in any source text of a compilation is in scope in every source text
/// of it (JYRO-REQ-1687, JYRO-ADR-121, for JYRO-DEF-526). Each arrangement compiles with no
/// diagnostic and is run, and its Output read, so that a match that compiles but runs the wrong
/// case fails. Written from the requirement alone.
module RealTree.Jyro.Spec.Tests.S160_06_LibraryUnions

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private uAB = "union U A B end"

/// Every diagnostic, for failure text.
let private shown (r: JyroResult<'T>) : string =
    r.Messages
    |> Seq.map (fun m ->
        let pos = if m.Location.HasValue then sprintf "%d:%d" m.Location.Value.Line m.Location.Value.Column else "-"
        sprintf "%A %A [%s] at %s: %s" m.Severity m.Code (Pending.stageOf m) pos m.Message)
    |> String.concat "\n"

let private describeCase (libs: string list) (script: string) : string =
    let ls = libs |> List.mapi (fun i l -> sprintf "--- library added %d ---\n%s" (i + 1) l) |> String.concat "\n"
    sprintf "%s\n--- script ---\n%s" ls script

/// Run a compiled result under `pinned` after asserting it compiled with no diagnostic, and
/// assert the run succeeds with no diagnostic and `Output` equals the JSON `expected`.
let private runsTo (what: string) (expected: string) (compiled: JyroResult<CompiledProgram>) (ctx: string) : unit =
    if not compiled.IsSuccess || compiled.Messages.Count > 0 then
        Values.fail (sprintf "%s: expected a clean compile. Diagnostics:\n%s\n%s" what (shown compiled) ctx)
    let program = Script.valueOf compiled
    let run = (Script.runnerFor pinned program).Execute()
    if not run.IsSuccess || run.Messages.Count > 0 then
        Values.fail (sprintf "%s: expected a clean run. Diagnostics:\n%s\n%s" what (shown run) ctx)
    let got = Values.toJson (Script.valueOf run)
    let want = Values.toJson (Values.json expected)
    if got <> want then
        Values.fail (sprintf "%s: expected Output %s, got %s.\n%s" what want got ctx)

/// Compile `script` with the library sources `libs` (AddLibrarySource, in order), run it, and
/// check `Output`.
let private outputIs (what: string) (expected: string) (libs: string list) (script: string) : unit =
    runsTo what expected (Script.compileWith { pinned with Libraries = libs } script) (describeCase libs script)

/// `lib` parsed ahead with JyroLibrary.Parse, asserting it parses with no diagnostic: a library
/// whose only references to other source texts are calls and match cases is parsed clean
/// (JYRO-REQ-1698).
let private parsedAhead (lib: string) : JyroLibrary =
    let r = JyroLibrary.Parse lib
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "JyroLibrary.Parse reported diagnostics (JYRO-REQ-1698 leaves them to the compilation):\n%s\n--- library ---\n%s" (shown r) lib)
    Script.valueOf r

/// As `outputIs`, with each library of `libs` parsed ahead and added with AddLibrary, in order;
/// `fromSource` picks, by index, the libraries added with AddLibrarySource instead (a mix).
let private outputIsMixed (what: string) (expected: string) (fromSource: int -> bool) (libs: string list) (script: string) : unit =
    let compiler =
        libs
        |> List.indexed
        |> List.fold (fun (c: JyroCompiler) (i, l) -> if fromSource i then c.AddLibrarySource l else c.AddLibrary(parsedAhead l))
            (Script.compilerFor pinned)
    runsTo what expected (compiler.WithSource(script).Compile()) (describeCase libs script)

let private outputIsParsed (what: string) (expected: string) (libs: string list) (script: string) : unit =
    outputIsMixed (what + " [parsed ahead]") expected (fun _ -> false) libs script

/// The match of the requirement's example on `subject`: A gives 1, B gives 2.
let private matchExpr (subject: string) = sprintf "match %s do case A then 1 case B then 2 end" subject

/// (1) The requirement's first example: a library declares U, the script builds A() and matches.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 the script calls and matches the variants of a library's union`` () =
    // The statement's own text, on one line.
    outputIs "the example" """{"x":1}""" [ uAB ] ("var u = A()  Output.x = " + matchExpr "u")
    outputIs "the example over lines" """{"x":1}""" [ uAB ] ("var u = A()\nOutput.x = " + matchExpr "u")
    outputIs "B" """{"x":2}""" [ uAB ] ("var u = B()\nOutput.x = " + matchExpr "u")
    // The statement form of match.
    outputIs "statement form, A" """{"x":1}""" [ uAB ] "var u = A()\nmatch u do case A then Output.x = 1 case B then Output.x = 2 end"
    outputIs "statement form, B" """{"x":2}""" [ uAB ] "var u = B()\nmatch u do case A then Output.x = 1 case B then Output.x = 2 end"
    // Variants with fields, bound in the case.
    outputIs "fields bound" """{"x":7}""" [ "union S Circle(r) Square(s) end" ]
        "var v = Square(7)\nOutput.x = match v do case Circle(r) then r case Square(s) then s end"

/// (1) The same, with the library added as a parsed JyroLibrary (AddLibrary), 160.6's other way.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 the script calls and matches the variants of a union of a library added as a JyroLibrary`` () =
    let parsed = JyroLibrary.Parse uAB
    if not parsed.IsSuccess then
        Values.fail (sprintf "JyroLibrary.Parse refused %s. Diagnostics:\n%s" uAB (shown parsed))
    let lib = Script.valueOf parsed
    let script = "var u = A()\nOutput.x = " + matchExpr "u"
    runsTo "AddLibrary" """{"x":1}""" ((Script.compilerFor pinned).AddLibrary(lib).WithSource(script).Compile()) script
    let script2 = "var u = B()\nmatch u do case A then Output.x = 1 case B then Output.x = 2 end"
    runsTo "AddLibrary, statement form" """{"x":2}""" ((Script.compilerFor pinned).AddLibrary(lib).WithSource(script2).Compile()) script2

/// (2) In a function of the script, called from the script.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a function of the script calls and matches the variants of a library's union`` () =
    outputIs "builds and matches" """{"x":1}""" [ uAB ] ("func f()\n  var u = A()\n  return " + matchExpr "u" + "\nend\nOutput.x = f()")
    outputIs "matches its argument" """{"x":2}""" [ uAB ] ("func f(u)\n  return " + matchExpr "u" + "\nend\nOutput.x = f(B())")
    outputIs "statement match" """{"x":2}""" [ uAB ]
        "func f(u)\n  match u do\n    case A then return 1\n    case B then return 2\n  end\n  return 0\nend\nOutput.x = f(B())"

/// (3) In a function of the declaring library, called from the script.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a function of the declaring library matches its union for the script`` () =
    let lib = uAB + "\nfunc f(u)\n  return " + matchExpr "u" + "\nend"
    outputIs "script builds A" """{"x":1}""" [ lib ] "Output.x = f(A())"
    outputIs "script builds B" """{"x":2}""" [ lib ] "Output.x = f(B())"

/// (4) A second library added after the declaring one: the requirement's second example.
/// (5) The same added before it: a library sees a union of a library added after it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a second library calls and matches the variants of a library's union, added after or before it`` () =
    let l2 = "func g()\n  return " + matchExpr "B()" + "\nend"
    outputIs "(4) L2 after L1" """{"y":2}""" [ uAB; l2 ] "Output.y = g()"
    outputIs "(5) L2 before L1" """{"y":2}""" [ l2; uAB ] "Output.y = g()"
    let l2a = "func h()\n  var u = A()\n  return " + matchExpr "u" + "\nend"
    outputIs "(4) L2 after L1, A" """{"y":1}""" [ uAB; l2a ] "Output.y = h()"
    outputIs "(5) L2 before L1, A" """{"y":1}""" [ l2a; uAB ] "Output.y = h()"
    // A third library between them changes nothing.
    outputIs "L1, other, L2" """{"y":2}""" [ uAB; "union W C end"; l2 ] "Output.y = g()"

/// (6) A union declared in the script is in scope in a library.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a library calls and matches the variants of a union of the script`` () =
    let lib = "func h()\n  return " + matchExpr "A()" + "\nend"
    outputIs "the library builds A" """{"x":1}""" [ lib ] (uAB + "\nOutput.x = h()")
    let libArg = "func k(u)\n  return " + matchExpr "u" + "\nend"
    outputIs "the script builds B, the library matches" """{"x":2}""" [ libArg ] (uAB + "\nOutput.x = k(B())")

/// (7) A value built in one source text and matched in another.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a variant value built in a library is matched in the script and in another library`` () =
    let l1 = uAB + "\nfunc mk()\n  return B()\nend"
    outputIs "L1 builds, the script matches" """{"x":2}""" [ l1 ] ("Output.x = " + matchExpr "mk()")
    let l2 = "func m2()\n  return " + matchExpr "mk()" + "\nend"
    outputIs "L1 builds, L2 matches" """{"x":2}""" [ l1; l2 ] "Output.x = m2()"
    outputIs "L1 builds, L2 (added first) matches" """{"x":2}""" [ l2; l1 ] "Output.x = m2()"
    // Built in the script, matched in the declaring library.
    let l3 = uAB + "\nfunc which(u)\n  return " + matchExpr "u" + "\nend"
    outputIs "the script builds, L1 matches" """{"x":1}""" [ l3 ] "var u = A()\nOutput.x = which(u)"

// -- The same arrangements with the libraries parsed ahead (JyroLibrary.Parse, then AddLibrary):
// JYRO-DEF-569, JYRO-ADR-139. A library whose functions call or match another text's variants is
// parsed with no diagnostic, and the compilation it is added to resolves them.

/// (2), (3) A function of the script, and of the declaring library, with the library parsed ahead.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a function of the script or of the declaring library matches the union of a library parsed ahead`` () =
    outputIsParsed "(2) script function" """{"x":2}""" [ uAB ] ("func f(u)\n  return " + matchExpr "u" + "\nend\nOutput.x = f(B())")
    let lib = uAB + "\nfunc f(u)\n  return " + matchExpr "u" + "\nend"
    outputIsParsed "(3) declaring library" """{"x":1}""" [ lib ] "Output.x = f(A())"

/// (4), (5) A second library, parsed ahead, calls and matches the variants of another library's
/// union: the requirement's second example, after and before the declaring library.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a second library parsed ahead calls and matches the variants of another library's union, added after or before it`` () =
    let l2 = "func g()\n  return " + matchExpr "B()" + "\nend"
    outputIsParsed "(4) L2 after L1" """{"y":2}""" [ uAB; l2 ] "Output.y = g()"
    outputIsParsed "(5) L2 before L1" """{"y":2}""" [ l2; uAB ] "Output.y = g()"
    let l2a = "func h()\n  var u = A()\n  return " + matchExpr "u" + "\nend"
    outputIsParsed "(4) L2 after L1, A" """{"y":1}""" [ uAB; l2a ] "Output.y = h()"
    outputIsParsed "(5) L2 before L1, A" """{"y":1}""" [ l2a; uAB ] "Output.y = h()"
    // A mix: L1 from source and L2 parsed ahead, and the other way round, in both orders.
    // `firstFromSource`: the library added first is added from source, the other parsed ahead.
    let firstFromSource i = i = 0
    let secondFromSource i = i = 1
    outputIsMixed "L1 source, L2 parsed" """{"y":2}""" firstFromSource [ uAB; l2 ] "Output.y = g()"
    outputIsMixed "L1 parsed, L2 source" """{"y":2}""" secondFromSource [ uAB; l2 ] "Output.y = g()"
    outputIsMixed "L2 source first, L1 parsed" """{"y":2}""" firstFromSource [ l2; uAB ] "Output.y = g()"
    outputIsMixed "L2 parsed first, L1 source" """{"y":2}""" secondFromSource [ l2; uAB ] "Output.y = g()"

/// (6) A library parsed ahead calls and matches the variants of a union of the script.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a library parsed ahead calls and matches the variants of a union of the script`` () =
    let lib = "func h()\n  return " + matchExpr "A()" + "\nend"
    outputIsParsed "the library builds A" """{"x":1}""" [ lib ] (uAB + "\nOutput.x = h()")
    let libArg = "func k(u)\n  return " + matchExpr "u" + "\nend"
    outputIsParsed "the script builds B, the library matches" """{"x":2}""" [ libArg ] (uAB + "\nOutput.x = k(B())")

/// (7) A value built in one source text and matched in another, the libraries parsed ahead.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 a variant value built in a library is matched in another library, the libraries parsed ahead`` () =
    let l1 = uAB + "\nfunc mk()\n  return B()\nend"
    let l2 = "func m2()\n  return " + matchExpr "mk()" + "\nend"
    outputIsParsed "L1 builds, the script matches" """{"x":2}""" [ l1 ] ("Output.x = " + matchExpr "mk()")
    outputIsParsed "L1 builds, L2 matches" """{"x":2}""" [ l1; l2 ] "Output.x = m2()"
    outputIsParsed "L1 builds, L2 (added first) matches" """{"x":2}""" [ l2; l1 ] "Output.x = m2()"

/// One parsed library in two compilations: the union it matches is declared by another text of
/// each; the library is not changed by the first compilation.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1687")>]
let ``JYRO-REQ-1687 one library parsed ahead matches the union of each compilation it is added to`` () =
    let lib = parsedAhead ("func k(u)\n  return " + matchExpr "u" + "\nend")
    let first = (Script.compilerFor pinned).AddLibrarySource(uAB).AddLibrary(lib).WithSource("Output.x = k(A())").Compile()
    runsTo "with the union in a library" """{"x":1}""" first "L1 source, lib parsed"
    let second = (Script.compilerFor pinned).AddLibrary(lib).WithSource(uAB + "\nOutput.x = k(B())").Compile()
    runsTo "with the union in the script" """{"x":2}""" second "lib parsed, union in the script"

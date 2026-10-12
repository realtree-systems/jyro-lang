/// 160.6: a library parsed ahead of a compilation (JyroLibrary.Parse) gives the diagnostics it
/// would give as source (JYRO-REQ-1698, JYRO-ADR-139, for JYRO-DEF-569). Parsing ahead reports
/// only the checks that read the library alone; every check that depends on the compilation is
/// made by each compilation the parsed library is added to, with the same code, stage, place and
/// order as the same text added from source. Written from the requirement alone.
module RealTree.Jyro.Spec.Tests.S160_06_ParsedLibraries

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// What a diagnostic is compared by: code, severity, stage (160.1.12) and place (130.1.2).
type private Seen =
    { Code: MessageCode
      Severity: MessageSeverity
      Stage: string
      Line: int
      Column: int
      Length: int }

let private seen (m: DiagnosticMessage) : Seen =
    let l, c, n = if m.Location.HasValue then m.Location.Value.Line, m.Location.Value.Column, m.Location.Value.Length else -1, -1, -1
    { Code = m.Code; Severity = m.Severity; Stage = Pending.stageOf m; Line = l; Column = c; Length = n }

let private seenAll (r: JyroResult<'T>) : Seen list = r.Messages |> Seq.map seen |> List.ofSeq

let private shown (r: JyroResult<'T>) : string =
    seenAll r
    |> List.map (fun s -> sprintf "%A %A [%s] at %d:%d+%d" s.Severity s.Code s.Stage s.Line s.Column s.Length)
    |> String.concat "\n"

let private describeCase (libs: string list) (script: string) : string =
    let ls = libs |> List.mapi (fun i l -> sprintf "--- library added %d ---\n%s" (i + 1) l) |> String.concat "\n"
    sprintf "%s\n--- script ---\n%s" ls script

/// `lib` parsed ahead, asserting that parsing it reports no diagnostic: its faults, if any, are
/// all of rows 2 to 5 of 160.6, which parsing ahead does not report.
let private parsedClean (lib: string) : JyroLibrary =
    let r = JyroLibrary.Parse lib
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "JyroLibrary.Parse reported diagnostics that JYRO-REQ-1698 leaves to the compilation:\n%s\n--- library ---\n%s" (shown r) lib)
    Script.valueOf r

/// Compile `script` under `o` with `libs` added from source (AddLibrarySource, in order).
let private fromSource (o: RunOptions) (libs: string list) (script: string) : JyroResult<CompiledProgram> =
    Script.compileWith { o with Libraries = libs } script

/// Compile `script` under `o` with each of `libs` parsed ahead and added with AddLibrary, in order.
let private fromParsed (o: RunOptions) (libs: string list) (script: string) : JyroResult<CompiledProgram> =
    let parsed = libs |> List.map parsedClean
    let compiler = parsed |> List.fold (fun (c: JyroCompiler) l -> c.AddLibrary l) (Script.compilerFor { o with Libraries = [] })
    compiler.WithSource(script).Compile()

/// Assert the compilation with every library parsed ahead gives exactly the diagnostics, in the
/// same order, that it gives with every library added from source; return the source result.
let private sameBothWays (what: string) (o: RunOptions) (libs: string list) (script: string) : JyroResult<CompiledProgram> =
    let s = fromSource o libs script
    let p = fromParsed o libs script
    if seenAll s <> seenAll p || s.IsSuccess <> p.IsSuccess then
        Values.fail (sprintf "%s: the parsed libraries give other diagnostics than the same text from source.\nFrom source (IsSuccess %b):\n%s\nParsed ahead (IsSuccess %b):\n%s\n%s"
                        what s.IsSuccess (shown s) p.IsSuccess (shown p) (describeCase libs script))
    s

/// As `sameBothWays`, and the compilation fails with an Error of `code`.
let private rejectedBothWays (what: string) (code: MessageCode) (o: RunOptions) (libs: string list) (script: string) : unit =
    let s = sameBothWays what o libs script
    if s.IsSuccess || not (List.contains code (Script.errorCodes s)) then
        Values.fail (sprintf "%s: expected the compilation to fail with %A. Diagnostics:\n%s\n%s" what code (shown s) (describeCase libs script))

// -- (1) The book examples -----------------------------------------------------------------------------

let private l1 = "union U A B end"
let private l2 = "func g()\n  return match B() do\n    case A then 1\n    case B then 2\n  end\nend"
let private l2c = "func g()\n  return match B() do\n    case A then 1\n    case B then 2\n    case C then 3\n  end\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 the book example, a library matching another library's variants, is parsed ahead clean and compiles`` () =
    let parsed = parsedClean l2
    let r = (Script.compilerFor pinned).AddLibrarySource(l1).AddLibrary(parsed).WithSource("Output.y = g()").Compile()
    if not r.IsSuccess || r.Messages.Count > 0 then
        Values.fail (sprintf "expected a clean compile. Diagnostics:\n%s" (shown r))
    let run = (Script.runnerFor pinned (Script.valueOf r)).Execute()
    Assert.Equal("""{"y":2}""", Values.toJson (Script.output run))
    // The same with L2 from source.
    let s = fromSource pinned [ l1; l2 ] "Output.y = g()"
    Assert.Equal<Seen list>(seenAll s, seenAll r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 the book example, a case C declared nowhere, is parsed ahead clean and gives MatchUnknownVariant at Validate in the compilation`` () =
    let parsed = parsedClean l2c
    let r = (Script.compilerFor pinned).AddLibrarySource(l1).AddLibrary(parsed).WithSource("Output.y = g()").Compile()
    Assert.False(r.IsSuccess, shown r)
    let unknown = seenAll r |> List.filter (fun s -> s.Code = MessageCode.MatchUnknownVariant)
    match unknown with
    | [ u ] ->
        Assert.Equal("Validate", u.Stage)
        // C is on line 5 of L2, `    case C then 3`.
        Assert.Equal(5, u.Line)
    | other -> Values.fail (sprintf "expected one MatchUnknownVariant, got %A. Diagnostics:\n%s" other (shown r))
    // As it does with the same L2 added from source.
    let s = fromSource pinned [ l1; l2c ] "Output.y = g()"
    Assert.Equal<Seen list>(seenAll s, seenAll r)

// -- (2) Row 1: what parsing ahead reports -------------------------------------------------------------

/// Libraries with a fault that reads only the library, each with the code it gives.
let private readsOnlyTheLibrary : (string * string * MessageCode option) list =
    [ "parse error", "func f(\n  return 1\nend", None
      "LibraryStatementForbidden", "func f()\n  return 1\nend\nvar x = 1", Some MessageCode.LibraryStatementForbidden
      "DuplicateParameterName", "func f(a, a)\n  return a\nend", Some MessageCode.DuplicateParameterName
      "a match over the library's own union that is not exhaustive",
        "union U A B end\nfunc f(u)\n  return match u do\n    case A then 1\n  end\nend", Some MessageCode.MatchNotExhaustive
      "a match over the library's own union with a wrong binding count",
        "union U A(x) end\nfunc f(u)\n  return match u do\n    case A(p, q) then 1\n  end\nend", Some MessageCode.MatchBindingCountMismatch
      "a variant repeated within the library", "union U\n  A\n  A\nend", Some MessageCode.DuplicateVariantName
      "a union repeated within the library", "union U A end\nunion U B end", Some MessageCode.DuplicateUnionName ]

let readsOnlyTheLibraryRows : obj[] seq = readsOnlyTheLibrary |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof readsOnlyTheLibraryRows)>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 a check that reads only the library is reported by Parse as from source, and the library is not parsed`` (name: string) =
    let _, lib, code = readsOnlyTheLibrary |> List.find (fun (n, _, _) -> n = name)
    let parsed = JyroLibrary.Parse lib
    Assert.False(parsed.IsSuccess, sprintf "%s: Parse succeeded" name)
    Assert.False(parsed.HasValue, sprintf "%s: Parse gave a value" name)
    code |> Option.iter (fun c -> Assert.Contains(c, Script.errorCodes parsed))
    // The same diagnostics, codes, stages and places, as the library added from source to a
    // compilation whose script is clean.
    let s = fromSource pinned [ lib ] "Output.r = 1"
    if seenAll s <> seenAll parsed then
        Values.fail (sprintf "%s: Parse reports other diagnostics than the library from source.\nFrom source:\n%s\nParse:\n%s" name (shown s) (shown parsed))

// -- (3) Rows 2 to 5: left to the compilation -----------------------------------------------------------

let private withHostFoo : RunOptions = { pinned with Functions = [ (Host.recorder "Foo").AsFunction ] }

/// One case per check of rows 2 to 5: options, libraries, script, and the code.
let private leftToTheCompilation : (string * RunOptions * string list * string * MessageCode) list =
    [ // Row 2: function names.
      "DuplicateFunction, two libraries", pinned, [ "func g()\n  return 1\nend"; "\nfunc g()\n  return 2\nend" ], "Output.r = g()", MessageCode.DuplicateFunction
      "FunctionOverride, a registered host function", withHostFoo, [ "func Foo()\n  return 1\nend" ], "Output.r = 1", MessageCode.FunctionOverride
      "FunctionOverride, a standard library function", pinned, [ "func Trim(x)\n  return x\nend" ], "Output.r = 1", MessageCode.FunctionOverride
      // Row 3: union and variant names.
      "DuplicateUnionName, two libraries", pinned, [ "union U A end"; "\nunion U B end" ], "Output.r = 1", MessageCode.DuplicateUnionName
      "DuplicateUnionName, a library and the script", pinned, [ "union U A end" ], "\n\nunion U B end", MessageCode.DuplicateUnionName
      "DuplicateVariantName, two libraries", pinned, [ "union U A end"; "\nunion V A end" ], "Output.r = 1", MessageCode.DuplicateVariantName
      "DuplicateVariantName, a library and the script", pinned, [ "union U A end" ], "\n\nunion V B A end", MessageCode.DuplicateVariantName
      // Row 4: a match naming a variant the library does not declare.
      "MatchUnknownVariant", pinned, [ l1; "func g(u)\n  return match u do\n    case A then 1\n    case C then 3\n  end\nend" ], "Output.r = g(A())", MessageCode.MatchUnknownVariant
      "MatchBindingCountMismatch", pinned, [ "union U A(x) B end"; "func g(u)\n  return match u do\n    case A(p, q) then 1\n    case B then 2\n  end\nend" ], "Output.r = g(B())", MessageCode.MatchBindingCountMismatch
      "MatchDuplicateBinding", pinned, [ "union U A(x, y) B end"; "func g(u)\n  return match u do\n    case A(p, p) then 1\n    case B then 2\n  end\nend" ], "Output.r = g(B())", MessageCode.MatchDuplicateBinding
      "MatchNotExhaustive, the union in another library", pinned, [ l1; "func g(u)\n  return match u do\n    case A then 1\n  end\nend" ], "Output.r = g(A())", MessageCode.MatchNotExhaustive
      "MatchNotExhaustive, the union in the script", pinned, [ "func g(u)\n  return match u do\n    case A then 1\n  end\nend" ], l1 + "\nOutput.r = g(A())", MessageCode.MatchNotExhaustive
      // Row 5: the checks of a call.
      "UndefinedFunction", pinned, [ "func g()\n  return Nope()\nend" ], "Output.r = g()", MessageCode.UndefinedFunction
      "TooManyArguments, another library's function", pinned, [ "func h(a)\n  return a\nend"; "func g()\n  return h(1, 2)\nend" ], "Output.r = g()", MessageCode.TooManyArguments
      "TooFewArguments, another library's function", pinned, [ "func h(a)\n  return a\nend"; "func g()\n  return h()\nend" ], "Output.r = g()", MessageCode.TooFewArguments
      "TooFewArguments, another library's variant", pinned, [ "union U A(x) end"; "func g()\n  return A()\nend" ], "Output.r = g()", MessageCode.TooFewArguments
      "TooManyArguments, the script's variant", pinned, [ "func g()\n  return A(1)\nend" ], "union U A end\nOutput.r = g()", MessageCode.TooManyArguments
      "FunctionEffectExceedsMax, the libraries' maximum", { pinned with LibraryMaxEffect = Some FunctionEffect.Pure }, [ "func Stamp()\n  return Now()\nend" ], "Output.r = 1", MessageCode.FunctionEffectExceedsMax ]

let leftToTheCompilationRows : obj[] seq = leftToTheCompilation |> Seq.map (fun (n, _, _, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof leftToTheCompilationRows)>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 a check that depends on the compilation is not reported by Parse and is reported by the compilation as from source`` (name: string) =
    let _, o, libs, script, code = leftToTheCompilation |> List.find (fun (n, _, _, _, _) -> n = name)
    // parsedClean (inside rejectedBothWays) asserts each library parses with no diagnostic.
    rejectedBothWays name code o libs script

// -- (3a) Rows 3 and 4 at once: a repeated name and the library's own match --------------------------------
//
// A parsed library whose match names only its own variants is checked at Parse, but an earlier
// source text that declares one of its union or variant names changes what that match resolves to
// in the compilation (JYRO-DEF-573): from source the first declaration of a name is the one in
// scope, so the match is resolved against the earlier text's union. The parsed library must give
// the same diagnostics, the match's included. Each case is run in both orders of the libraries.

/// One case per combination: the declaring library, the repeating library with its own match, the
/// repeated-name code, and the match codes the source path gives beside it when the declaring
/// library is added first.
let private repeatedNameWithOwnMatch : (string * string * string * MessageCode * MessageCode list) list =
    [ "a repeated variant name, the library's match on its own union",
        "union U A B end", "union V A C end\nfunc f(x)\n  return match x do\n    case A then 1\n    case C then 2\n  end\nend",
        MessageCode.DuplicateVariantName, [ MessageCode.MatchUnknownVariant; MessageCode.MatchNotExhaustive ]
      "a repeated union name, the library's match on its own union",
        "union U A B end", "union U C D end\nfunc f(x)\n  return match x do\n    case C then 1\n    case D then 2\n  end\nend",
        MessageCode.DuplicateUnionName, [ MessageCode.MatchUnknownVariant ]
      "a repeated variant name with fields, the library's match binding none",
        "union U A(p) B end", "union V A C end\nfunc f(x)\n  return match x do\n    case A then 1\n    case C then 2\n  end\nend",
        MessageCode.DuplicateVariantName, [ MessageCode.MatchBindingCountMismatch ] ]

let repeatedNameWithOwnMatchRows : obj[] seq =
    seq {
        for (n, _, _, _, _) in repeatedNameWithOwnMatch do
            yield [| box n; box true |]
            yield [| box n; box false |] }

[<Theory>]
[<MemberData(nameof repeatedNameWithOwnMatchRows)>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 a parsed library that repeats another text's union or variant name gives its own match's diagnostics as from source`` (name: string, declaringFirst: bool) =
    let _, declaring, repeating, nameCode, matchCodes = repeatedNameWithOwnMatch |> List.find (fun (n, _, _, _, _) -> n = name)
    let what = sprintf "%s (%s first)" name (if declaringFirst then "declaring library" else "repeating library")
    let libs = if declaringFirst then [ declaring; repeating ] else [ repeating; declaring ]
    let script = "Output.r = 1"
    // Every library parsed ahead (parsedClean asserts Parse reports nothing), against all from source.
    let s = sameBothWays what pinned libs script
    // The case reaches the combination: from source the repeated name is rejected, and when the
    // declaring library is first the match is resolved against its union.
    Assert.False(s.IsSuccess, shown s)
    let codes = Script.errorCodes s
    Assert.Contains(nameCode, codes)
    if declaringFirst then
        for c in matchCodes do
            Assert.Contains(c, codes)
    // And with the declaring library from source and only the repeating one parsed ahead.
    let parsedRepeating = parsedClean repeating
    let c0 = Script.compilerFor pinned
    let mixedCompiler =
        if declaringFirst then c0.AddLibrarySource(declaring).AddLibrary(parsedRepeating)
        else c0.AddLibrary(parsedRepeating).AddLibrarySource(declaring)
    let mixed = mixedCompiler.WithSource(script).Compile()
    if seenAll s <> seenAll mixed || s.IsSuccess <> mixed.IsSuccess then
        Values.fail (sprintf "%s: the declaring library from source and the repeating one parsed ahead give other diagnostics than both from source.\nFrom source (IsSuccess %b):\n%s\nMixed (IsSuccess %b):\n%s\n%s"
                        what s.IsSuccess (shown s) mixed.IsSuccess (shown mixed) (describeCase libs script))

// -- (4) One parsed library in two compilations -----------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 one parsed library is checked in each compilation it is added to, and is not changed by one`` () =
    let lib = parsedClean "func k(u)\n  return match u do\n    case A then 1\n    case B then 2\n  end\nend"
    let declaring () = (Script.compilerFor pinned).AddLibrarySource(l1).AddLibrary(lib).WithSource("Output.x = k(B())").Compile()
    let without () = (Script.compilerFor pinned).AddLibrary(lib).WithSource("Output.x = 1").Compile()
    let compiles (r: JyroResult<CompiledProgram>) =
        if not r.IsSuccess || r.Messages.Count > 0 then Values.fail (sprintf "expected a clean compile:\n%s" (shown r))
    let unknown (r: JyroResult<CompiledProgram>) =
        Assert.False(r.IsSuccess, shown r)
        Assert.Contains(MessageCode.MatchUnknownVariant, Script.errorCodes r)
    compiles (declaring ())
    unknown (without ())
    compiles (declaring ())
    unknown (without ())

// -- (5) Library order, by either way of adding ---------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1698")>]
let ``JYRO-REQ-1698 the diagnostics of parsed libraries and libraries from source are in the order the libraries were added`` () =
    // Each library has a match on a variant no text declares, on its own line.
    let la = "func fa(u)\n  return match u do\n    case Xa then 1\n  end\nend"
    let lb = "\n\n\nfunc fb(u)\n  return match u do\n    case Xb then 1\n  end\nend"
    let lines (r: JyroResult<CompiledProgram>) =
        seenAll r |> List.filter (fun s -> s.Code = MessageCode.MatchUnknownVariant) |> List.map (fun s -> s.Line)
    let pa, pb = parsedClean la, parsedClean lb
    let add (c: JyroCompiler) (lib: string, parsed: JyroLibrary, asSource: bool) = if asSource then c.AddLibrarySource lib else c.AddLibrary parsed
    for aSource in [ true; false ] do
        for bSource in [ true; false ] do
            let ab = [ (la, pa, aSource); (lb, pb, bSource) ] |> List.fold add (Script.compilerFor pinned)
            let ba = [ (lb, pb, bSource); (la, pa, aSource) ] |> List.fold add (Script.compilerFor pinned)
            let rab = ab.WithSource("Output.r = 1").Compile()
            let rba = ba.WithSource("Output.r = 1").Compile()
            Assert.Equal<int list>([ 3; 6 ], lines rab)
            Assert.Equal<int list>([ 6; 3 ], lines rba)
            // And as the same texts from source.
            Assert.Equal<Seen list>(seenAll (fromSource pinned [ la; lb ] "Output.r = 1"), seenAll rab)
            Assert.Equal<Seen list>(seenAll (fromSource pinned [ lb; la ] "Output.r = 1"), seenAll rba)

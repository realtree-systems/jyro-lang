/// 100.10: one maximum nesting depth, 256, for a source text and an artifact (JYRO-ADR-177,
/// JYRO-DEF-499). A source deeper than the maximum fails to compile with
/// SourceNestingLimitExceeded at Parse; a source within it compiles; and no compilation and no run
/// within the maximum stops the host process.
///
/// What could end the process runs in a child process on a thread of a fixed small stack
/// (NestingCases.fs, Child.fs): a stack overflow then fails the test, never the test run.
/// SourceNestingLimitExceeded is reached by its number, 2002, and its name: the member is not in
/// every build (FLOORS 4.1.7).
module RealTree.Jyro.Spec.Tests.S100_10_SourceNesting

open System
open System.IO
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.NestingCases

let private field = RealTree.Jyro.Spec.Tests.ValueSize.field

// -- Reading a result ------------------------------------------------------------------------------

let private nestingCode : MessageCode = enum<MessageCode> 2002

/// The diagnostic is SourceNestingLimitExceeded: the number 2002 and the member of that name.
let private isNesting (m: DiagnosticMessage) : bool =
    m.Code = nestingCode && Enum.GetName(typeof<MessageCode>, m.Code) = "SourceNestingLimitExceeded"

let private compileScript (source: string) : JyroResult<CompiledProgram> =
    bounded 120. (fun () -> Script.compileWith pinned source)

let private compileWithLibrary (library: string) (source: string) : JyroResult<CompiledProgram> =
    bounded 120. (fun () -> Script.compileWith { pinned with Libraries = [ library ] } source)

/// Assert the compilation gave no diagnostic at all (100.10.3).
let private assertCompiles (what: string) (r: JyroResult<'T>) : unit =
    Assert.True(r.IsSuccess && Seq.isEmpty r.Messages, sprintf "%s: expected no diagnostic:\n%s" what (Script.describe r))

/// Assert `r` is the refusal of 100.10.4: exactly one diagnostic, SourceNestingLimitExceeded (2002),
/// the stage Parse, the one argument 256 and a message that names it. Gives the diagnostic.
let private assertRefused (what: string) (r: JyroResult<'T>) : DiagnosticMessage =
    let all = List.ofSeq r.Messages
    Assert.False(r.IsSuccess, sprintf "%s: compiled" what)
    Assert.True((all.Length = 1), sprintf "%s: expected exactly one diagnostic:\n%s" what (Script.describe r))
    let m = all.Head
    Assert.True(isNesting m, sprintf "%s: expected SourceNestingLimitExceeded (2002):\n%s" what (Script.describe r))
    Assert.Equal("Parse", Pending.stageOf m)
    Assert.Equal<string list>([ "256" ], m.Args |> Array.map argText |> List.ofArray)
    Assert.Contains("256", m.Message)
    m

/// Assert the place of `m` (130.1.2): line, column, length.
let private assertPlace (what: string) (line: int, column: int, length: int) (m: DiagnosticMessage) : unit =
    Assert.True(m.Location.HasValue, sprintf "%s: the diagnostic has no location" what)
    let l = m.Location.Value
    Assert.True((l.Line, l.Column, l.Length) = (line, column, length),
                sprintf "%s: expected %d/%d/%d, got %d/%d/%d" what line column length l.Line l.Column l.Length)

let private outputOf (what: string) (key: string) (r: JyroResult<JyroValue>) : string =
    Assert.True(r.IsSuccess, sprintf "%s: did not run:\n%s" what (Script.describe r))
    Values.toJson (Values.prop key (Script.valueOf r))

// -- 100.10.1 The maximum is 256, for a source and an artifact --------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2130")>]
let ``JYRO-REQ-2130 the book example, 254 operators +, compiles, runs to 255, is written, and its artifact loads and runs to 255`` () =
    let source = "Output.x = 1" + rep 254 " + 1"
    Assert.Equal("255", outputOf "from source" "x" (bounded 60. (fun () -> Script.runWith pinned source)))
    let written = bounded 60. (fun () -> Script.compileBytesWith pinned source)
    Assert.True(written.IsSuccess, "the artifact was not written:\n" + Script.describe written)
    Assert.DoesNotContain(MessageCode.ArtifactCannotEncode, Script.codes written)
    let bytes = Script.valueOf written
    Assert.Equal("255", outputOf "from the artifact" "x" (bounded 60. (fun () -> Script.runBytesWith pinned bytes)))

/// A program whose source depth is 256 is written and loaded again, whatever its shape.
[<Theory>]
[<InlineData("parens")>]
[<InlineData("operators")>]
[<InlineData("brackets")>]
[<InlineData("calls")>]
[<InlineData("ifs")>]
[<Trait("requirement", "JYRO-REQ-2130")>]
[<Trait("requirement", "JYRO-REQ-2164")>]
let ``JYRO-REQ-2130 a program that compiles from source at depth 256 can be written as an artifact and loaded`` (name: string) =
    let shape = shapeNamed name
    let source = shape.Script (scriptLevels shape maximum)
    let written = bounded 60. (fun () -> Script.compileBytesWith pinned source)
    Assert.True(written.IsSuccess, sprintf "%s: not written:\n%s" name (Script.describe written))
    let loaded = bounded 60. (fun () -> Script.loadBytesWith pinned (Script.valueOf written))
    Assert.True(loaded.IsSuccess, sprintf "%s: the artifact did not load:\n%s" name (Script.describe loaded))

/// The script and each library script are measured on their own (160.6): a library at 256 and a
/// script at 256 compile together, and each at 257 is refused.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2130")>]
let ``JYRO-REQ-2130 the script and a library script are each measured on their own`` () =
    let ops = shapeNamed "operators"
    let library256 = ops.Library (libraryLevels ops maximum)
    let script256 = ops.Script (scriptLevels ops maximum)
    assertCompiles "library 256 and script 256" (compileWithLibrary library256 script256)
    let library257 = ops.Library (libraryLevels ops (maximum + 1))
    let script257 = ops.Script (scriptLevels ops (maximum + 1))
    assertRefused "library 256 and script 257" (compileWithLibrary library256 script257) |> ignore
    assertRefused "library 257 and script 256" (compileWithLibrary library257 script256) |> ignore

/// A host cannot raise, lower or remove the maximum (100.1.3): with every option of the host API
/// set to its least and its greatest, a script at 256 compiles and one at 257 is refused.
let private hostOptions : (string * RunOptions) list =
    let unlimited = JyroExecutionOptions.Unlimited
    [ "defaults", Script.defaults
      "pinned", pinned
      "unlimited", { Script.defaults with Limits = Some unlimited }
      "greatest", { Script.defaults with
                      Limits = Some { unlimited with MaxCallDepth = Int32.MaxValue; MaxStatements = Int32.MaxValue; MaxLoopIterations = Int32.MaxValue } }
      "least", { Script.defaults with
                   Limits = Some { JyroExecutionOptions.Default with MaxCallDepth = 1; MaxStatements = 1; MaxLoopIterations = 1 } }
      "no stdlib", { Script.defaults with Stdlib = false }
      "long compile limit", { Script.defaults with CompilationTimeout = Some(TimeSpan.FromHours 1.) }
      "short compile limit", { Script.defaults with CompilationTimeout = Some(TimeSpan.FromMinutes 5.) } ]

let hostOptionRows : obj[] seq = hostOptions |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof hostOptionRows)>]
[<Trait("requirement", "JYRO-REQ-2130")>]
let ``JYRO-REQ-2130 no option of the host changes the maximum`` (name: string) =
    let options = hostOptions |> List.find (fun (n, _) -> n = name) |> snd
    let ops = shapeNamed "operators"
    let at depth = ops.Script (scriptLevels ops depth)
    assertCompiles name (bounded 120. (fun () -> Script.compileWith options (at maximum)))
    assertRefused name (bounded 120. (fun () -> Script.compileWith options (at (maximum + 1)))) |> ignore

/// No public member of the compiler, the runner or the execution options offers to set a nesting
/// depth (100.1.1 names the bounds that a host sets).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2130")>]
let ``JYRO-REQ-2130 the host API offers no member that sets a nesting depth`` () =
    let flags = Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Static
    let names (t: Type) =
        [ for m in t.GetMembers flags do
            yield m.Name
            match m with
            | :? Reflection.MethodBase as mb -> for p in mb.GetParameters() do yield p.Name
            | _ -> () ]
    let offending =
        [ typeof<JyroCompiler>; typeof<JyroRunner>; typeof<JyroExecutionOptions> ]
        |> List.collect (fun t -> names t |> List.filter (fun n -> not (isNull n) && n.IndexOf("Nesting", StringComparison.OrdinalIgnoreCase) >= 0) |> List.map (fun n -> t.Name + "." + n))
    Assert.True(List.isEmpty offending, sprintf "100.10.1: members that name a nesting depth: %A" offending)

/// The maximum is documented, as 100.5.2 requires of a safe maximum: the README of the host API
/// has a section about nesting that gives 256 for a source text.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2130")>]
let ``JYRO-REQ-2130 the Jyro.Api README documents the maximum source nesting depth, 256`` () =
    let rec up (dir: DirectoryInfo) =
        if isNull dir then failwith "Jyro.slnx not found above the test assembly"
        elif File.Exists(Path.Combine(dir.FullName, "Jyro.slnx")) then dir.FullName
        else up dir.Parent
    let root = up (DirectoryInfo(AppContext.BaseDirectory))
    let doc = File.ReadAllLines(Path.Combine(root, "src", "Jyro.Api", "README.md"))
    let headingLevel (line: string) : int option =
        let t = line.TrimStart()
        if t.StartsWith "#" then
            let n = t |> Seq.takeWhile ((=) '#') |> Seq.length
            if n < t.Length && t.[n] = ' ' then Some n else None
        else None
    // Every section whose heading mentions nesting, with its sub-sections.
    let sections =
        [ for i in 0 .. doc.Length - 1 do
            match headingLevel doc.[i] with
            | Some level when doc.[i].IndexOf("nesting", StringComparison.OrdinalIgnoreCase) >= 0 ->
                let body =
                    doc
                    |> Array.skip (i + 1)
                    |> Array.takeWhile (fun l -> match headingLevel l with Some n -> n > level | None -> true)
                yield String.Join("\n", body)
            | _ -> () ]
    let documented =
        sections |> List.exists (fun s -> s.Contains "256" && s.IndexOf("source", StringComparison.OrdinalIgnoreCase) >= 0)
    Assert.True(documented, "100.5.2, 100.10.1: no section of the Jyro.Api README about nesting gives 256 as the maximum depth of a source text")

/// JYRO-DEF-764: the README names two bounds "maximum nesting depth", values (128, 100.7) and
/// source and artifact (256, 100.10). The section of each says where the other is: it gives the
/// other's number. The heading "Maximum nesting depth" stays (JYRO-DEF-497).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2130")>]
let ``JYRO-DEF-764 the README sections of the two nesting bounds each name the other`` () =
    let rec up (dir: DirectoryInfo) =
        if isNull dir then failwith "Jyro.slnx not found above the test assembly"
        elif File.Exists(Path.Combine(dir.FullName, "Jyro.slnx")) then dir.FullName
        else up dir.Parent
    let doc = File.ReadAllLines(Path.Combine(up (DirectoryInfo(AppContext.BaseDirectory)), "src", "Jyro.Api", "README.md"))
    let headingLevel (line: string) : int option =
        let t = line.TrimStart()
        if t.StartsWith "#" then
            let n = t |> Seq.takeWhile ((=) '#') |> Seq.length
            if n < t.Length && t.[n] = ' ' then Some n else None
        else None
    let sections =
        [ for i in 0 .. doc.Length - 1 do
            match headingLevel doc.[i] with
            | Some level when doc.[i].IndexOf("nesting", StringComparison.OrdinalIgnoreCase) >= 0 ->
                let body = doc |> Array.skip (i + 1) |> Array.takeWhile (fun l -> match headingLevel l with Some n -> n > level | None -> true)
                yield doc.[i], String.Join("\n", body)
            | _ -> () ]
    // Told apart by heading: each section names both numbers, so its body cannot tell them apart.
    let has (word: string) (h: string) = h.IndexOf(word, StringComparison.OrdinalIgnoreCase) >= 0
    let valueSection = sections |> List.tryFind (fun (h, b) -> has "value" h && not (has "source" h) && b.Contains "128")
    let sourceSection = sections |> List.tryFind (fun (h, b) -> has "source" h && b.Contains "256")
    Assert.True(valueSection.IsSome, "no README section about nesting gives the value bound, 128")
    Assert.True(sourceSection.IsSome, "no README section about nesting gives the source bound, 256")
    Assert.True(fst valueSection.Value <> fst sourceSection.Value, "one heading serves both bounds")
    Assert.True((snd valueSection.Value).Contains "256", "the section of the value bound does not name the source bound, 256")
    Assert.True((snd sourceSection.Value).Contains "128", "the section of the source bound does not name the value bound, 128")
    Assert.True(sections |> List.exists (fun (h, _) -> h.IndexOf("Maximum nesting depth", StringComparison.OrdinalIgnoreCase) >= 0), "no heading contains 'Maximum nesting depth' (JYRO-DEF-497)")

// -- 100.10.2 The nesting depth of a source text ----------------------------------------------------

type private DepthCase =
    { Name: string
      /// Top-level declarations that the statement uses (a union), which cannot be nested.
      Prefix: string
      Statement: string
      /// The greatest depth in the statement when it stands at depth 1.
      Depth: int }

let private depthCases : DepthCase list =
    let c name prefix statement depth = { Name = name; Prefix = prefix; Statement = statement; Depth = depth }
    [ // The clause's own examples.
      c "example" "" "Output.x = -(-(1))" 6
      c "chain-of-2" "" "Output.s = 1 + 1 + 1" 4
      c "array" "" "Output.x = [[1]]" 4
      c "calls" "" "Output.x = Length(ToUpper(\"a\"))" 4
      c "if-in-if" "" "if true then Output.x = 1 end" 4
      // Each operand, argument, element, value and body is one deeper than what holds it.
      c "binary-right-operand" "" "Output.x = 1 + 2 * 3" 4
      c "call-argument" "" "Output.x = Length(\"a\")" 3
      c "array-element" "" "Output.x = [1]" 3
      c "object-value" "" "Output.x = {k: 1}" 3
      c "unary-operand" "" "Output.x = not true" 3
      c "parentheses" "" "Output.x = (1)" 3
      c "parentheses-twice" "" "Output.x = ((1))" 4
      c "elseif-body" "" "if false then Output.x = 0 elseif true then Output.x = 1 end" 4
      c "else-body" "" "if false then Output.x = 0 else Output.x = 1 end" 4
      c "while-body" "" "while false do Output.x = 1 end" 4
      c "foreach-body" "" "foreach i in [1] do Output.x = i end" 4
      c "switch-case-body" "" "switch 1 do case 1 then Output.x = 1 end" 4
      c "switch-default-body" "" "switch 1 do default then Output.x = 1 end" 4
      c "match-case-body" "union U A end" "match A() do case A then Output.x = 1 end" 4
      // A structure that is not a node adds nothing.
      c "match-expression-case" "union U A end" "Output.x = match A() do case A then [1] end" 4
      c "named-argument" "" "Output.x = Round(value: Length(\"a\"), decimals: 0)" 4
      c "object-entry" "" "Output.x = {k: [1]}" 4
      c "lambda-parameter-and-body" "" "Output.x = Map([1], v => [v])" 5 ]

let private depthCaseNamed (name: string) : DepthCase = depthCases |> List.find (fun c -> c.Name = name)

/// The case as a source whose greatest depth is `total`: its statement inside `total - Depth` ifs.
let private paddedTo (total: int) (c: DepthCase) : string =
    let wrapped = rep (total - c.Depth) "if true then " + c.Statement + rep (total - c.Depth) " end"
    if c.Prefix = "" then wrapped else c.Prefix + "\n" + wrapped

let depthCaseRows : obj[] seq = depthCases |> Seq.map (fun c -> [| box c.Name |])

[<Theory>]
[<MemberData(nameof depthCaseRows)>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 a source of depth 256 compiles with no diagnostic`` (name: string) =
    assertCompiles name (compileScript (paddedTo maximum (depthCaseNamed name)))

[<Theory>]
[<MemberData(nameof depthCaseRows)>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 the same source one level deeper, 257, is refused`` (name: string) =
    assertRefused name (compileScript (paddedTo (maximum + 1) (depthCaseNamed name))) |> ignore

/// The depth of an artifact is never above the depth of its source (100.10.2): the artifact of a
/// source of depth 256 is written and loads.
[<Theory>]
[<MemberData(nameof depthCaseRows)>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 the artifact of a source of depth 256 is written and loads`` (name: string) =
    let source = paddedTo maximum (depthCaseNamed name)
    let written = bounded 120. (fun () -> Script.compileBytesWith pinned source)
    Assert.True(written.IsSuccess, sprintf "%s: not written:\n%s" name (Script.describe written))
    let loaded = bounded 120. (fun () -> Script.loadBytesWith pinned (Script.valueOf written))
    Assert.True(loaded.IsSuccess, sprintf "%s: the artifact did not load:\n%s" name (Script.describe loaded))

/// The book's examples, each a whole source text, at the depth the clause gives.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 the books examples have the depths the clause gives`` () =
    // k nested ifs around the assignment: depth k + 3; n operators +: depth n + 2.
    for k in [ 1; 2; 10; 253 ] do
        assertCompiles (sprintf "%d ifs" k) (compileScript (rep k "if true then " + "Output.x = 1" + rep k " end"))
    assertRefused "254 ifs" (compileScript (rep 254 "if true then " + "Output.x = 1" + rep 254 " end")) |> ignore
    for n in [ 2; 3; 100; 254 ] do
        assertCompiles (sprintf "%d operators" n) (compileScript ("Output.s = 1" + rep n " + 1"))
    assertRefused "255 operators" (compileScript ("Output.s = 1" + rep 255 " + 1")) |> ignore

/// Each parenthesised expression is one level, and the expression inside it is one more.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 each parenthesised expression counts one level`` () =
    // 127 negations, each with its parentheses: Unary at 2i, parentheses at 2i + 1, the Literal at 256.
    let negations n = "Output.x = " + rep n "-(" + "1" + rep n ")"
    assertCompiles "127 negations" (compileScript (negations 127))
    assertRefused "128 negations" (compileScript (negations 128)) |> ignore
    // The artifact of 127 negations has depth 129, which the parentheses do not add to (150.9.9).
    let written = bounded 60. (fun () -> Script.compileBytesWith pinned (negations 127))
    Assert.True(written.IsSuccess, Script.describe written)
    // 254 parentheses and no node between them: depth 256 and 257.
    assertCompiles "254 parentheses" (compileScript ("Output.x = " + rep 254 "(" + "1" + rep 254 ")"))
    assertRefused "255 parentheses" (compileScript ("Output.x = " + rep 255 "(" + "1" + rep 255 ")")) |> ignore

/// A function declaration holds its body one level deeper; its parameters add nothing.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 the body of a function is one deeper than its declaration and its parameters add nothing`` () =
    // FuncDef 1, m ifs 2 to m + 1, the Return m + 2, the Array m + 3, the Literal m + 4.
    let source m = "func F(a, b, c)\n  " + rep m "if true then " + "return [1]" + rep m " end" + "\nend"
    assertCompiles "body at 256" (compileScript (source 252))
    assertRefused "body at 257" (compileScript (source 253)) |> ignore

/// Width adds nothing: statements, elements, entries, arguments and elseif pairs side by side.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 width adds nothing to the depth`` () =
    // The book's example: a variable and 10,000 statements, depth 3.
    assertCompiles "10,000 statements" (compileScript ("var a = [1, 2, 3, 4, 5]\n" + lines (repeatLines 10_000 "Output.x = 1")))
    assertCompiles "10,000 elseif pairs" (compileScript ("if false then Output.x = 0 " + rep 10_000 "elseif false then Output.x = 1 " + "end"))
    // 10,000 elements, the Array one deeper than the Assignment and the elements one deeper again:
    // inside 253 ifs the depth is 256 and the width changes nothing; inside 254 it is 257.
    let wide m = rep m "if true then " + "Output.x = " + arrayLiteral 10_000 + rep m " end"
    assertCompiles "10,000 elements at 256" (compileScript (wide 253))
    assertRefused "10,000 elements at 257" (compileScript (wide 254)) |> ignore

/// Comments and white space add nothing.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2131")>]
let ``JYRO-REQ-2131 comments and white space add nothing to the depth`` () =
    let source m =
        rep m "# a comment\n\n\t  if   true   then  # another\n\n" + "Output.x = 1\n" + rep m "\n\n   end   # closed\n"
    assertCompiles "253 ifs with comments" (compileScript (source 253))
    assertRefused "254 ifs with comments" (compileScript (source 254)) |> ignore

// -- 100.10.3 A source text at or under the maximum is compiled -------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2173")>]
let ``JYRO-REQ-2173 the three sources at depth 256 compile with no diagnostic and give their programs`` () =
    let run (shape: string) =
        let s = shapeNamed shape
        let source = s.Script (scriptLevels s maximum)
        assertCompiles shape (compileScript source)
        bounded 60. (fun () -> Script.runWith pinned source)
    Assert.Equal("1", outputOf "parentheses" "x" (run "parens"))
    Assert.Equal("255", outputOf "operators" "s" (run "operators"))
    Assert.Equal("1", outputOf "ifs" "x" (run "ifs"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2173")>]
let ``JYRO-REQ-2173 the same three sources as a library script at depth 256 compile with no diagnostic`` () =
    for shape, expected in [ "parens", "1"; "operators", "254"; "ifs", "1" ] do
        let s = shapeNamed shape
        let library = s.Library (libraryLevels s maximum)
        assertCompiles (shape + " library") (compileWithLibrary library "Output.x = F()")
        let r = bounded 60. (fun () -> Script.runWith { pinned with Libraries = [ library ] } "Output.x = F()")
        Assert.Equal(expected, outputOf (shape + " library") "x" r)
        assertCompiles (shape + " library alone") (bounded 60. (fun () -> JyroLibrary.Parse library))

/// The sources at 256 on host threads of 256 KB, 384 KB and 1 MB, in a child process.
let compileAtMaximumRows : obj[] seq =
    seq { for shape in [ "parens"; "operators"; "ifs" ] do
            for kind in [ "script"; "library" ] do
                for kb in [ 256; 384; 1024 ] do
                    yield [| box shape; box kind; box kb |] }

[<Theory>]
[<MemberData(nameof compileAtMaximumRows)>]
[<Trait("requirement", "JYRO-REQ-2173")>]
let ``JYRO-REQ-2173 a source at depth 256 compiles on a host thread with a small stack, with no diagnostic`` (shape: string, kind: string, kb: int) =
    let s = shapeNamed shape
    let k = if kind = "library" then libraryLevels s maximum else scriptLevels s maximum
    let o = Child.runWith 120_000 [] (sprintf "nest|compile|%s|%d|%s|%d" shape k kind kb)
    Child.expectClean o
    Assert.True(o.Ok, "the source of depth 256 did not compile: " + o.Describe)
    Assert.Equal(Some "0", o.Result |> Option.bind (field "msgs"))

// -- 100.10.4 A source text over the maximum is refused at Parse ------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 255 parentheses around 1 fail at line 1, column 267, length 1`` () =
    let m = assertRefused "255 parentheses" (compileScript ("Output.x = " + rep 255 "(" + "1" + rep 255 ")"))
    assertPlace "255 parentheses" (1, 267, 1) m

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 255 operators + fail at line 1, column 12, length 1`` () =
    let m = assertRefused "255 operators" (compileScript ("Output.s = 1" + rep 255 " + 1"))
    assertPlace "255 operators" (1, 12, 1) m

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 254 nested ifs around the assignment fail at the first token past the maximum`` () =
    // The ifs are at 1 to 254, the Assignment at 255, the PropertyAccess and the Literal at 256 and
    // the Identifier `Output` at 257: the first node above the maximum, at the token `Output`.
    let source = rep 254 "if true then " + "Output.x = 1" + rep 254 " end"
    let m = assertRefused "254 ifs" (compileScript source)
    assertPlace "254 ifs" (1, 254 * 13 + 1, 6) m

/// Every construct that a source can nest, one level over the maximum, as a script and as a library.
let refusedRows : obj[] seq =
    seq { for shape in [ "parens"; "operators"; "brackets"; "calls"; "ifs" ] do
            for kind in [ "script"; "library" ] do
                yield [| box shape; box kind |] }

[<Theory>]
[<MemberData(nameof refusedRows)>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 a source of depth 257 fails with exactly one diagnostic, at Parse`` (shape: string, kind: string) =
    let s = shapeNamed shape
    if kind = "library" then
        let library = s.Library (libraryLevels s (maximum + 1))
        assertRefused (shape + " as a library") (compileWithLibrary library "Output.x = F()") |> ignore
        assertRefused (shape + " as a library alone") (bounded 60. (fun () -> JyroLibrary.Parse library)) |> ignore
    else
        assertRefused shape (compileScript (s.Script (scriptLevels s (maximum + 1)))) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 a library script over the maximum fails with the place in that library`` () =
    // "func F()", then "  return 1 + 1 ..." with 254 operators: the Return 2, the operators 3 to 256,
    // the first operand 257, which is the first `1` of line 2, at column 10.
    let ops = shapeNamed "operators"
    let library = ops.Library 254
    let m = assertRefused "library" (compileWithLibrary library "Output.x = F()")
    assertPlace "library" (2, 10, 1) m
    assertPlace "library alone" (2, 10, 1) (assertRefused "library alone" (bounded 60. (fun () -> JyroLibrary.Parse library)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 the boundary pair, 256 compiled and 257 refused, holds for the three sources of the clause`` () =
    for name in [ "parens"; "operators"; "ifs" ] do
        let s = shapeNamed name
        assertCompiles (name + " 256") (compileScript (s.Script (scriptLevels s maximum)))
        assertRefused (name + " 257") (compileScript (s.Script (scriptLevels s (maximum + 1)))) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 a source over the maximum that also has a parse error fails with at least one of the two diagnostics`` () =
    // 300 opening parentheses and 299 closing ones: too deep, and the end of the source comes first.
    let source = "Output.x = " + rep 300 "(" + "1" + rep 299 ")"
    let r = compileScript source
    Assert.False(r.IsSuccess, "compiled")
    let errors = [ for m in r.Messages do if m.Severity = MessageSeverity.Error then yield m ]
    Assert.False(List.isEmpty errors, "no error diagnostic")
    let parseCodes = [ "UnknownParserError"; "UnexpectedEndOfSource"; "UnterminatedString"; "InvalidEscapeSequence"; "InvalidNumberFormat" ]
    let isOne (m: DiagnosticMessage) = isNesting m || List.contains (Enum.GetName(typeof<MessageCode>, m.Code)) parseCodes
    Assert.True(errors |> List.exists isOne, "neither SourceNestingLimitExceeded nor a parse error of 130.3:\n" + Script.describe r)
    for m in errors do
        Assert.True(isOne m, sprintf "a diagnostic of another kind:\n%s" (Script.describe r))
        Assert.Equal("Parse", Pending.stageOf m)

/// JYRO-DEF-499: 10,000 `var` statements, then the sum of the 10,000 variables. It stopped the host
/// process with a stack overflow; it must fail with one diagnostic at the token `a0` of the last line.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2132")>]
[<Trait("requirement", "JYRO-REQ-2133")>]
let ``JYRO-DEF-499 the source of 10,000 variables and their sum fails with SourceNestingLimitExceeded at a0 of the last line`` () =
    let o = Child.runWith 180_000 [] "nest|def499|1024"
    Child.expectClean o
    let get key = o.Result |> Option.bind (field key)
    Assert.False(o.Ok, "the source compiled: " + o.Describe)
    Assert.Equal<string list>([ "SourceNestingLimitExceeded" ], o.Codes)
    Assert.Equal(Some "1", get "msgs")
    Assert.Equal(Some "Parse", get "stage")
    Assert.Equal(Some "Parse", get "stages")
    Assert.Equal(Some "10001", get "line")
    Assert.Equal(Some "12", get "col")
    Assert.Equal(Some "2", get "len")
    Assert.Equal(Some "256", get "args")

/// JYRO-ADR-178: the place is the first token whose reading count passes 256, and the depth decides
/// only when no reading count does. Past 1,024 levels the two places differ in these sources.
let placeRows : obj[] seq = placeCases |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof placeRows)>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 a source nested 100,000 levels is placed at the first token whose reading count passes 256`` (name: string) =
    let _, _, (line, column, length) = placeCases |> List.find (fun (n, _, _) -> n = name)
    let o = Child.runWith 180_000 [] (sprintf "nest|place|%s|1024" name)
    Child.expectClean o
    let get key = o.Result |> Option.bind (field key)
    Assert.False(o.Ok, "the source compiled: " + o.Describe)
    Assert.Equal<string list>([ "SourceNestingLimitExceeded" ], o.Codes)
    Assert.Equal(Some "1", get "msgs")
    Assert.Equal(Some "Parse", get "stage")
    Assert.Equal(Some "256", get "args")
    Assert.Equal((Some(string line), Some(string column), Some(string length)), (get "line", get "col", get "len"))

/// The same sources at 300 levels, which a bounded re-read also places exactly: the place is the same.
[<Theory>]
[<InlineData(300)>]
[<InlineData(2000)>]
[<Trait("requirement", "JYRO-REQ-2132")>]
let ``JYRO-REQ-2132 the place of nested parentheses after a chain does not depend on how deep the source goes`` (levels: int) =
    let source = "Output.s = 1 + " + rep levels "(" + "1" + rep levels ")" + rep 300 " + 1"
    let m = assertRefused "chain then parentheses" (compileScript source)
    assertPlace "chain then parentheses" (1, 271, 1) m
    // Nested ternaries in the else branch: the then-branch of the 255th is the first reading count above 256.
    let ternaries = "Output.x = " + rep levels "true ? 1 : " + "1"
    assertPlace "else ternaries" (1, 12 + 11 * 254 + 7, 1) (assertRefused "else ternaries" (compileScript ternaries))

// -- 100.10.5 No source text stops the host process ---------------------------------------------------

let survivalRows : obj[] seq =
    seq { for shape in shapes do
            for kind in [ "script"; "library" ] do
                for kb in [ 256; 384; 1024 ] do
                    yield [| box shape.Name; box kind; box kb |] }

[<Theory>]
[<MemberData(nameof survivalRows)>]
[<Trait("requirement", "JYRO-REQ-2133")>]
let ``JYRO-REQ-2133 a source nested 100,000 levels ends with diagnostics and leaves the process running`` (shape: string, kind: string, kb: int) =
    let s = shapeNamed shape
    let o = Child.runWith 180_000 [] (sprintf "nest|compile|%s|100000|%s|%d" shape kind kb)
    Child.expectClean o
    Assert.False(o.Ok, "the source compiled: " + o.Describe)
    Assert.False(List.isEmpty o.Codes, "no error diagnostic: " + o.Describe)
    // A source that is well formed beyond its depth is refused for its depth (100.10.4).
    if s.WellFormed then
        Assert.Equal<string list>([ "SourceNestingLimitExceeded" ], o.Codes)

// -- 100.10.6 Running a program does not stop the host process -----------------------------------------

let private runChild (name: string) (via: string) : Child.ChildOutcome =
    Child.runWith 180_000 [] (sprintf "nest|run|%s|%s|1024" name via)

[<Theory>]
[<InlineData("source")>]
[<InlineData("bytes")>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 254 operators + run to 255 on a host thread of 1 MB`` (via: string) =
    let o = runChild "operators254" via
    Child.expectSuccess o
    Assert.Equal(Some "255", o.Result |> Option.bind (field "s"))

let selfCallRows : obj[] seq = [ 4; 64; 128; 255; 256 ] |> Seq.map (fun d -> [| box d |])

[<Theory>]
[<MemberData(nameof selfCallRows)>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 a function whose body has the depth calls itself until CallDepthLimitExceeded on a host thread of 1 MB`` (depth: int) =
    Child.expectCode MessageCode.CallDepthLimitExceeded (runChild (sprintf "selfcall:%d" depth) "source")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 a function of body depth 256 run from its artifact calls itself until CallDepthLimitExceeded`` () =
    Child.expectCode MessageCode.CallDepthLimitExceeded (runChild "selfcall:256" "bytes")

let divZeroRows : obj[] seq = [ 6; 64; 128; 255; 256 ] |> Seq.map (fun d -> [| box d |])

[<Theory>]
[<MemberData(nameof divZeroRows)>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 a division by zero at the greatest call depth the limit allows ends with DivisionByZero on a host thread of 1 MB`` (depth: int) =
    let o = Child.runWith 300_000 [] (sprintf "nest|divzero|%d" depth)
    Child.expectClean o
    Assert.True(o.Ok, "no depth of the recursion ended with DivisionByZero: " + o.Describe)
    let number key = o.Result |> Option.bind (field key) |> Option.map int |> Option.defaultValue -1
    let greatest, limit = number "greatest", number "limit"
    Assert.True(limit > 0, o.Describe)
    // The recursion reaches the call depth limit, give or take the call of the script itself.
    Assert.True(greatest >= limit - 2 && greatest <= limit, sprintf "the greatest recursion that ended with DivisionByZero is %d; the call depth limit is %d" greatest limit)

let endRows : obj[] seq = actionNames |> Seq.map (fun a -> [| box a |])

[<Theory>]
[<MemberData(nameof endRows)>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 a run at nesting 256 and the greatest call depth ends in each way without stopping the process`` (action: string) =
    let o = runChild ("action:" + action) "source"
    match action with
    | "fail" -> Child.expectCode MessageCode.ScriptFailure o
    | "exit" -> Child.expectSuccess o
    | "statements" -> Child.expectCode MessageCode.StatementLimitExceeded o
    | "loops" -> Child.expectCode MessageCode.LoopIterationLimitExceeded o
    | "cancel" -> Child.expectCode MessageCode.CanceledByHost o
    | other -> failwithf "unknown action %s" other

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 a run from an artifact ends with a failure at nesting 256 and the greatest call depth`` () =
    Child.expectCode MessageCode.ScriptFailure (runChild "action:fail" "bytes")

let rec private negations (k: int) (e: Expr) : Expr = if k = 0 then e else EUn(Op.negate, negations (k - 1) e, p0)

/// The artifact of 254 negations (depth 256) is loaded and runs to 1, on a host thread of 1 MB.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 the artifact of 254 negations loads and runs to Output.x = 1 on a host thread of 1 MB`` () =
    let path = Path.Combine(Path.GetTempPath(), sprintf "jyro-nest-%s.jyrx" (Guid.NewGuid().ToString("N")))
    File.WriteAllBytes(path, build [ setOut "x" (negations 254 (num 1m)) ])
    try
        let o = Child.runWith 120_000 [] (sprintf "nest|file|%s|1024" path)
        Child.expectSuccess o
        Assert.Equal(Some "1", o.Result |> Option.bind (field "x"))
    finally
        File.Delete path

/// The programs of the run tests are at the depth that the tests say: 256 compiles and 257 is
/// refused, so the tests are not run at a depth short of the maximum.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2134")>]
let ``JYRO-REQ-2134 the programs that run at nesting 256 have nesting 256`` () =
    let withCancel =
        let cts = new System.Threading.CancellationTokenSource()
        { pinned with Functions = [ (canceller cts).AsFunction ] }
    let compile options source = bounded 120. (fun () -> Script.compileWith options source)
    assertCompiles "self-calling function" (compile pinned (selfCallSource maximum))
    assertRefused "self-calling function" (compile pinned (selfCallSource (maximum + 1))) |> ignore
    assertCompiles "division by zero" (compile pinned (divZeroSource maximum 5))
    assertRefused "division by zero" (compile pinned (divZeroSource (maximum + 1) 5)) |> ignore
    for action in actionNames do
        assertCompiles action (compile withCancel (actionSource maximum 5 (actionOf action)))
        assertRefused action (compile withCancel (actionSource (maximum + 1) 5 (actionOf action))) |> ignore
    assertCompiles "operators" (compile pinned ("Output.s = 1" + rep 254 " + 1"))

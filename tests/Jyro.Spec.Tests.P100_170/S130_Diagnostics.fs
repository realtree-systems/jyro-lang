/// Part 130: diagnostics and the result envelope.
module RealTree.Jyro.Spec.Tests.S130_Diagnostics

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.MessageCodeTable

/// A result of any payload type, reduced to what Part 130 speaks about.
type Outcome =
    { Name: string
      Ok: bool
      HasValue: bool
      Messages: DiagnosticMessage list }

let private outcome name (r: JyroResult<'T>) =
    { Name = name; Ok = r.IsSuccess; HasValue = r.HasValue; Messages = List.ofSeq r.Messages }

/// Failing and succeeding sources across every stage.
let private corpusSources =
    [ "parse-var", "var = 1"
      "parse-unterminated", "Output.x = \"abc"
      "parse-escape", "Output.x = \"a\\qb\""
      "parse-paren", "Output.x = (1 + "
      "validate-undeclared", "Output.x = zzz"
      "validate-redeclared", "var a = 1\nvar a = 2"
      "validate-break", "break"
      "validate-return", "return 1"
      "validate-input", "Input = 1"
      "validate-nested", "func F()\n  func G()\n  end\n  return 1\nend"
      "validate-output", "func F()\n  Output.a = 1\n  return 1\nend"
      "validate-several", "Output.a = u1\nbreak\nreturn 2\nvar b = 1\nvar b = 2"
      "link-undefined", "Output.x = Nope()"
      "link-arity", "Output.x = Length(1, 2)"
      "runtime-divide", "Output.a = 1\nOutput.x = 1 / 0"
      "runtime-index", "Output.x = [1][5]"
      "runtime-fail", "Output.a = 1\nfail \"bad\""
      "runtime-fail-empty", "fail"
      "runtime-type", "Output.x = Length(1)"
      "runtime-exit", "Output.a = 1\nexit \"done\""
      "success", "Output.a = 1" ]

let corpus () : Outcome list =
    [ for name, src in corpusSources do
        yield outcome name (Script.run src)
      yield outcome "effect" (Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } "Output.t = Now()")
      yield outcome "limit" (Script.runWith (withLimits (fun o -> { o with MaxStatements = 1 })) "Output.a = 1\nOutput.b = 2")
      yield outcome "artifact-magic" (Script.loadBytesWith Script.defaults ([| 1uy; 2uy; 3uy |]))
      yield outcome "artifact-libraries" (Script.compileBytesWith { Script.defaults with Libraries = [ "func L()\n  return 1\nend" ] } "Output.a = 1")
      yield outcome "library-forbidden" (Script.compileWith { Script.defaults with Libraries = [ "var x = 1\nfunc L()\n  return 1\nend" ] } "Output.a = L()")
      yield outcome "host-collision" (Script.compileWith { Script.defaults with Functions = [ (Host.recorder "Length").AsFunction ] } "Output.a = 1") ]

// -- 130.1.x Diagnostic structure --------------------------------------------------------

/// Further failing sources for the clauses that bind every diagnostic (130.1.1):
/// each stage, many library functions, the limits, a cancel, a host throw, a
/// compilation time limit and malformed Input. No expected code is asserted of
/// them; they only have to give diagnostics. Every limit is set (FLOORS 4.1.11).
let private wideSources =
    [ "Output.x = 1 % 0"
      "Output.x = [1, 2][\"a\"]"
      "Output.x = ToUpper(1)"
      "Output.x = Substring(\"abc\", -1)"
      "Output.x = RegexTest(\"a\", \"(\")"
      "Output.x = ParseDate(\"nope\")"
      "Output.x = FormatDate(\"2024-01-01T00:00:00Z\", \"QQQ\")"
      "Output.x = DateAdd(\"2024-01-01T00:00:00Z\", 1, \"fortnight\")"
      "Output.x = Round(1.5, 0, \"sideways\")"
      "Output.x = Log(0)"
      "Output.x = Sqrt(-1)"
      "Output.x = 1e308 * 10"
      "Output.x = Range(0, 10000000)"
      "Output.x = PadLeft(\"a\", 3, \"xy\")"
      "Output.x = SortByField([{a: 1}], \"a\", \"decs\")"
      "Output.x = Map([1], 5)"
      "Output.x = Map([1], (a, b, c) => a)"
      "Output.x = FromJson(\"{bad\")"
      "Output.x = 1 < \"a\""
      "Output.x = -\"a\""
      "var o = 1\no.a = 2"
      "Output.x = Base64Decode(\"@@@\")"
      "Output.x = Min([\"a\", 1])"
      "Output.x = 12abc"
      "Output.x = \"\\uD800\""
      "var s: string"
      "var a = 1\na: number = 2"
      "func F(a = 1, b)\n  return a\nend"
      "func F(a, a)\n  return a\nend"
      "union U A A end"
      "foreach x in 5 do end"
      "5++"
      "func F(a)\n  return a\nend\nOutput.x = F()"
      "Output.x = Round(value: 1, nope: 2)"
      "union S Circle(r) end\nfunc Circle(x)\n  return x\nend" ]

/// `fail` statements that have a value: the message of their ScriptFailure is
/// the text of the value (130.1.1, 50.11.4), which may be empty. The outcomes
/// are named "fail-value-*", and the name, not the code, marks them.
let private failWithValueSources =
    [ "fail \"x\""
      "fail \"\""
      "var e = \"\"\nfail e"
      "func F()\n  fail \"\"\nend\nF()"
      "fail Substring(\"abc\", 3)"
      "Output.a = 1\nfail {a: 1}" ]

/// The outcomes whose sources are a `fail` with a value (the corpus's own
/// "runtime-fail" is `fail "bad"`).
let private isFailWithAValue (o: Outcome) : bool =
    o.Name = "runtime-fail" || o.Name.StartsWith("fail-value-", StringComparison.Ordinal)

let private wideCorpus () : Outcome list =
    let st = stall ()
    use cts = new Threading.CancellationTokenSource()
    let c = canceller cts
    let thrower = Host.func "Throw" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (InvalidOperationException "boom"))
    let limited f = { pinned with Limits = Some(Script.limits (fun o -> f (allLimits o))) }
    corpus ()
    @ [ for i, src in List.indexed wideSources -> outcome (sprintf "wide-%d" i) (Script.runWith pinned src) ]
    @ [ for i, src in List.indexed failWithValueSources -> outcome (sprintf "fail-value-%d" i) (Script.runWith pinned src) ]
    @ [ outcome "loop-limit" (Script.runWith (limited (fun o -> { o with MaxLoopIterations = 1 })) "var i = 0\nwhile i < 5 do i = i + 1 end")
        outcome "depth-limit" (Script.runWith (limited (fun o -> { o with MaxCallDepth = 2 })) (recursion 10))
        outcome "time-limit" (Script.runWith { limited (fun o -> { o with MaxExecutionTime = TimeSpan.FromMilliseconds 50. }) with Functions = [ st.AsFunction ] } "Stall(200)\nOutput.a = 1")
        outcome "cancel" (Script.runWith { pinned with Functions = [ c.AsFunction ]; Cancellation = Some cts.Token } "CancelNow()\nOutput.a = 1")
        outcome "host-throw" (Script.runWith { pinned with Functions = [ thrower.AsFunction ] } "Output.a = Throw()")
        outcome "compile-time" (Script.compileWith { pinned with CompilationTimeout = Some(TimeSpan.FromTicks 1L) } (manyStatements 5_000))
        outcome "bad-input" (Script.runWith { pinned with Input = JsonInput "{bad" } "Output.a = 1")
        outcome "no-source" ((JyroCompiler()).WithMaxCompilationTime(generousCompile).Compile()) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1474")>]
let ``EveryDiagnostic_HasASeverityOfErrorWarningOrInfo`` () =
    let all = wideCorpus ()
    let count = all |> List.sumBy (fun o -> o.Messages.Length)
    Assert.True(count >= 50, sprintf "the corpus gave only %d diagnostics" count)
    for o in all do
        for d in o.Messages do
            Assert.True(
                d.Severity = MessageSeverity.Error || d.Severity = MessageSeverity.Warning || d.Severity = MessageSeverity.Info,
                sprintf "%s: %A has the severity %d" o.Name d.Code (int d.Severity))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1474")>]
let ``EveryDiagnosticMessage_ExceptTheTextOfFail_IsNotEmpty`` () =
    for o in wideCorpus () do
        for d in o.Messages do
            Assert.True(not (isNull d.Message), sprintf "%s: %A has a null message" o.Name d.Code)
            // A string whose length in UTF-16 code units is greater than 0, except
            // the text of a fail value (recognized by its source, not by its code).
            if not (isFailWithAValue o && o.Messages.Length = 1) then
                Assert.True(d.Message.Length > 0, sprintf "%s: %A has an empty message" o.Name d.Code)

let emptyFailPaths : obj[] seq =
    [ "fail \"\""
      "var e = \"\"\nfail e"
      "func F()\n  fail \"\"\nend\nF()"
      "fail Substring(\"abc\", 3)" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<MemberData(nameof emptyFailPaths)>]
[<Trait("requirement", "JYRO-REQ-1474")>]
[<Trait("requirement", "JYRO-REQ-250")>]
let ``FailOfTheEmptyString_GivesAScriptFailureWhoseMessageIsEmpty`` (source: string) =
    let d = Script.runWith pinned source |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.ScriptFailure, d.Code)
    Assert.NotNull(d.Message)
    Assert.Equal("", d.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1474")>]
[<Trait("requirement", "JYRO-REQ-250")>]
[<Trait("requirement", "JYRO-REQ-252")>]
let ``JYRO-REQ-252 FailWithAValue_MessageIsItsText_AndFailWithNone_MessageIsNotEmpty`` () =
    for source, message in [ "fail \"boom\"", "boom"; "fail {a: 1}", "{\"a\":1}"; "fail", "Script execution failed" ] do
        let d = Script.runWith pinned source |> Script.onlyErrorMessage
        Assert.Equal(MessageCode.ScriptFailure, d.Code)
        Assert.Equal(message, d.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1474")>]
[<Trait("requirement", "JYRO-REQ-2138")>]
let ``JYRO-REQ-2138 EveryDiagnosticCode_WithARowOfTheTable_HasTheTablesNumber`` () =
    tableAsCopiedIsWhole ()
    // With a HostLinkError from a refusing call check (JYRO-REQ-2119), when the build has the
    // call check; without it, the members of the table are checked by the S160_07 tests.
    let hostLink =
        if Tests.CallCheck.hasCallCheck () then
            [ outcome "host-link" (Script.compileWith { pinned with Functions = [ (Tests.CallCheck.query ()).AsFunction ] } "Output.r = Query(\"custmers\")") ]
        else []
    for o in wideCorpus () @ hostLink do
        for d in o.Messages do
            match Enum.GetName(typeof<MessageCode>, d.Code) |> Option.ofObj |> Option.bind numberOf with
            | Some n -> Assert.True((int d.Code = n), sprintf "%s: %A carries %d; the table of 160.7.12 gives %d" o.Name d.Code (int d.Code) n)
            | None -> ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1474")>]
let ``EveryDiagnosticCode_WithNoRowOfTheTable_HasANumberNoOtherCodeHas`` () =
    let fields = typeof<MessageCode>.GetFields(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Static)
    for o in wideCorpus () do
        for d in o.Messages do
            let name = Enum.GetName(typeof<MessageCode>, d.Code)
            if isNull name || (numberOf name).IsNone then
                let same = fields |> Array.filter (fun f -> Convert.ToInt64(f.GetRawConstantValue()) = int64 (int d.Code))
                Assert.True(same.Length <= 1,
                            sprintf "%s: the code %d is the number of %s" o.Name (int d.Code) (String.Join(", ", same |> Array.map (fun f -> f.Name))))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1474")>]
[<Trait("requirement", "JYRO-REQ-2170")>]
let ``InvalidSortDirection_TheDiagnosticCarries5920`` () =
    // The clause's example: the diagnostic of 92.21.7 carries 5920. The member is
    // defined by 160.7.12; its absence blocks this test (floors-process 4.2.5).
    let r = Script.runWith pinned "Output.x = SortByField([{a: 2}, {a: 1}], \"a\", \"decs\")"
    let d = Script.onlyErrorMessage r
    if Enum.GetName(typeof<MessageCode>, enum<MessageCode> 5920) <> "InvalidSortDirection" then
        Pending.notInBuild "160.7.12" "MessageCode.InvalidSortDirection = 5920"
    Assert.Equal("InvalidSortDirection", Enum.GetName(typeof<MessageCode>, d.Code))
    Assert.Equal(5920, int d.Code)

/// One row per kind of diagnostic of 130.1.2: the code (or "parse" for a parse
/// error, which has no code of its own), the source, how it is run, and the exact
/// line, column and length of its place. Each row is judged by its code and its
/// place alone (JYRO-ADR-105 item 4). Under JYRO-ADR-122 the rows that need another
/// work order's token rule are tested with that requirement: InvalidNumberFormat of
/// `12abc` (JYRO-REQ-34), UnpairedSurrogate (JYRO-REQ-38) and the leading U+FEFF
/// column (JYRO-REQ-20); a loaded program's place is JYRO-REQ-2076's.
let private placeRows () : (string * string * string * int * int * int) list =
    [ // The three examples of the book.
      "TypedVariableNeedsInitializer", "var s: string", "compile", 1, 1, 13
      "ArgumentTypeMismatch", "Output.x = 1\nOutput.y = ToUpper([1])", "run", 2, 12, 12
      "parse", "var x =", "compile", 1, 8, 0
      // A parse error at the token where the parser stops.
      "parse", "Output.x = )", "compile", 1, 12, 1
      // A lexical error: its token, to the end of the source when unterminated.
      "UnterminatedString", "Output.x = \"abc", "compile", 1, 12, 4
      // A validation error: the node the rule rejects (40.7.1 rejects the assignment).
      "InvalidAssignmentTarget", "1 = 2", "compile", 1, 1, 5
      // A link error: the Call of TooFewArguments.
      "TooFewArguments", "func f(a)\n  return a\nend\nOutput.x = f()", "compile", 4, 12, 3
      // A runtime error: the Call of a library function, the operator node otherwise.
      "ArgumentTypeMismatch", "Output.a = 1\nOutput.b = 2\n    Output.x = PadLeft([1], 3)", "run", 3, 16, 15
      "DivisionByZero", "Output.a = 1\nOutput.b = 2\n    Output.x = 1 / 0", "run", 3, 16, 5
      "ModuloByZero", "Output.a = 1\nOutput.b = 2\n    Output.x = 1 % 0", "run", 3, 16, 5
      "DivisionByZero", "func F(x)\n  var y = 1\n    return x / 0\nend\nOutput.r = F(1)", "run", 3, 12, 5
      // Counting: a CRLF is one line end.
      "ArgumentTypeMismatch", "Output.x = 1\r\nOutput.y = ToUpper([1])", "run", 2, 12, 12
      // Link errors at the Call: of an undefined function, of too many arguments, of a variant.
      "UndefinedFunction", "Output.a = 1\nOutput.x = Nope()", "compile", 2, 12, 6
      "TooManyArguments", "func f(a)\n  return a\nend\nOutput.x = f(1, 2)", "compile", 4, 12, 7
      "TooFewArguments", "union S Circle(r) end\nOutput.x = Circle()", "compile", 2, 12, 8
      // JYRO-DEF-533: a construct left open by an unterminated string ends at the empty
      // range just after the last code unit (16 code units, so column 17), not past it.
      "parse", "Output.x = (\"abc", "compile", 1, 17, 0
      "parse", "if true then\n  Output.x = \"abc", "compile", 2, 18, 0
      // JYRO-DEF-533: after `var` the parser stops at a string token holding a bad escape
      // (blanked by the scan); the place is that token as the script writes it, 5 code units.
      "parse", "var \"a\\q\" = 1", "compile", 1, 5, 5
      // JYRO-REQ-2122: DuplicateVariantName is at the repeated variant, from its name to the end
      // of its fields (150.8.5), not at the union. The book example, ⏎ one LF.
      "DuplicateVariantName", "union U\n  A\n  A\nend", "compile", 3, 3, 1
      // With fields: the second A, through its closing parenthesis.
      "DuplicateVariantName", "union U A(x) A(y, z) end", "compile", 1, 14, 7
      // In a later union of the same text.
      "DuplicateVariantName", "union U A end\nunion V A end", "compile", 2, 9, 1
      // The other checks of a union stay at the node, the union.
      "DuplicateFieldName", "union U A(x, x) end", "compile", 1, 1, 19
      "ReservedFieldName", "union U A(_variant) end", "compile", 1, 1, 23 ]

let placeRowIndexes : obj[] seq = placeRows () |> List.mapi (fun i _ -> [| box i |]) |> Seq.ofList

/// Assert the diagnostic `d` is at line/column/length.
let private at (line: int) (column: int) (length: int) (d: DiagnosticMessage) : unit =
    Assert.True(d.Location.HasValue, sprintf "%A has no location" d.Code)
    let l = d.Location.Value
    Assert.True((l.Line, l.Column, l.Length) = (line, column, length),
                sprintf "%A: expected %d/%d/%d, got %d/%d/%d" d.Code line column length l.Line l.Column l.Length)

[<Theory>]
[<MemberData(nameof placeRowIndexes)>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a diagnostic at a place has its line, column and length`` (row: int) =
    let code, source, how, line, column, length = (placeRows ()).[row]
    let r = if how = "run" then Script.runWith pinned source else (Script.compileWith pinned source |> fun c -> JyroResult<JyroValue>.Failure(c.Messages))
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    Assert.False(errors.IsEmpty, sprintf "no error for %A" source)
    if code = "parse" then
        // A parse error has no code of its own: the error at the place where the parser
        // stops is the last one, after any lexical error before it (130.5.1 orders by place).
        at line column length (List.last errors)
    else
        let c = Enum.Parse<MessageCode> code
        fails c r
        at line column length errors.Head

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a runtime error of a host function is at its call`` () =
    let h = Host.func "Bad" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Runtime(MessageCode.ArgumentTypeMismatch, box "string", box "number")))
    let r = Script.runWith { pinned with Functions = [ h.AsFunction ] } "Output.a = 1\n  Output.x = Bad()"
    fails MessageCode.ArgumentTypeMismatch r
    at 2 14 5 (Script.onlyErrorMessage r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a diagnostic in a library has its line, column and length`` () =
    // 160.6.2 rejects the statement `var x = 1`: line 4, column 5, its 9 code units.
    let lib = "func L()\n  return 1\nend\n    var x = 1"
    let r = Script.compileWith { pinned with Libraries = [ lib ] } "Output.a = L()"
    let d = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.LibraryStatementForbidden)
    at 4 5 9 d

/// Every DuplicateVariantName of `r`, as (line, column, length), in order.
let private duplicateVariantPlaces (r: JyroResult<'T>) : (int * int * int) list =
    [ for m in r.Messages do
        if m.Code = MessageCode.DuplicateVariantName then
            if not m.Location.HasValue then Values.fail (sprintf "DuplicateVariantName with no location:\n%s" (Script.describe r))
            let l = m.Location.Value
            yield l.Line, l.Column, l.Length ]

let private assertVariantPlaces (what: string) (expected: (int * int * int) list) (r: JyroResult<'T>) : unit =
    Assert.False(r.IsSuccess, sprintf "%s: compiled" what)
    let got = duplicateVariantPlaces r
    if got <> expected then
        Values.fail (sprintf "%s: expected DuplicateVariantName at %A (line, column, length), got %A.\n%s" what expected got (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a variant named three times gives DuplicateVariantName at each repeat, at two places`` () =
    assertVariantPlaces "A A A" [ 1, 11, 1; 1, 13, 1 ] (Script.compileWith pinned "union U A A A end")
    assertVariantPlaces "A over lines" [ 3, 3, 1; 4, 3, 4 ] (Script.compileWith pinned "union U\n  A\n  A\n  A(x)\nend")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a variant repeated across a library and the script is placed at the later variant`` () =
    // The library's A is first (130.5.4); the script's A, on line 2 column 11, is the repeat.
    let r = Script.compileWith { pinned with Libraries = [ "union U A end" ] } "Output.r = 1\nunion V B A(q) end"
    assertVariantPlaces "library, then script" [ 2, 11, 4 ] r
    // Two libraries: the second library's A.
    let r2 = Script.compileWith { pinned with Libraries = [ "union U A end"; "\n\n  union V A end" ] } "Output.r = 1"
    assertVariantPlaces "two libraries" [ 3, 11, 1 ] r2

// -- 130.1.2 for a program loaded from an artifact: the variant's position as written (JYRO-REQ-2122) --

module Codec = RealTree.Jyro.Spec.Tests.Artifact

let private loadBytes (bytes: byte[]) : JyroResult<CompiledProgram> =
    Script.loadBytesWith pinned (bytes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a loaded program written from source places DuplicateVariantName as the source does`` () =
    // A writer refuses the repeat, so the source "union U\n  A\n  B\nend" is written and its B is
    // renamed A in the artifact, every position kept as written: the load gives the place the
    // source "union U\n  A\n  A\nend" gives, line 3, column 3, length 1.
    let _, a = Codec.writtenBy pinned "union U\n  A\n  B\nend"
    let program =
        a.Program
        |> List.map (function
            | Codec.SUnion(n, vs, p) -> Codec.SUnion(n, vs |> List.map (fun (vn, fs, vp) -> (if vn = "B" then "A" else vn), fs, vp), p)
            | s -> s)
    let r = loadBytes (Codec.encode { a with Program = program })
    assertVariantPlaces "load" [ 3, 3, 1 ] r
    assertVariantPlaces "source" [ 3, 3, 1 ] (Script.compileWith pinned "union U\n  A\n  A\nend")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2122")>]
let ``JYRO-REQ-2122 a loaded program gives DuplicateVariantName at each repeated variant's position as written, not the union's`` () =
    let pos line col start len : Codec.Pos = { Line = line; Column = col; Start = start; End = start + len }
    let union =
        Codec.SUnion("U",
                     [ "A", [], pos 2 3 10 1
                       "A", [], pos 5 7 40 1
                       "A", [], pos 9 2 80 4 ],
                     pos 1 1 0 100)
    let r = loadBytes (Codec.build [ union ])
    assertVariantPlaces "A A A" [ 5, 7, 1; 9, 2, 4 ] r
    // Two unions, U A and V A, with V's A written at line 4, column 6.
    let r2 =
        loadBytes (Codec.build [ Codec.SUnion("U", [ "A", [], pos 1 9 8 1 ], pos 1 1 0 13)
                                 Codec.SUnion("V", [ "A", [], pos 4 6 30 1 ], pos 3 1 20 13) ])
    assertVariantPlaces "U A, V A" [ 4, 6, 1 ] r2
// A loaded program's place is JYRO-REQ-2076's (150.4.7), under JYRO-ADR-122 item 3:
// 1475's artifact sentence is context, and these tests are 1535's alone.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``JYRO-REQ-2076 LoadedProgram_UnchangedArtifact_GivesTheSourceLocation`` () =
    let bytes = Script.compileBytesWith pinned "Output.x = 1\nOutput.y = ToUpper([1])" |> Script.valueOf
    let r = Script.runBytesWith pinned bytes
    fails MessageCode.ArgumentTypeMismatch r
    at 2 12 12 (Script.onlyErrorMessage r)

let loadedRows : obj[] seq =
    [ 2, 12, 21, 22, 2, 12, 1
      -3, 0, 50, 40, -3, 0, 0
      1, 1, 0, Int32.MaxValue, 1, 1, Int32.MaxValue
      1, 1, Int32.MinValue, Int32.MaxValue, 1, 1, 0 ]
    |> Seq.map (fun (a, b, c, d, e, f, g) -> [| box a; box b; box c; box d; box e; box f; box g |])

[<Theory>]
[<MemberData(nameof loadedRows)>]
[<Trait("requirement", "JYRO-REQ-2076")>]
let ``JYRO-REQ-2076 LoadedProgram_PatchedPosition_IsReportedAsWritten`` (line: int, column: int, start: int, stop: int, eLine: int, eColumn: int, eLength: int) =
    // For a loaded program the place is the node's position as written (150.4.7);
    // the last row's difference is above 2,147,483,647, so its length is 0.
    let _, a = Artifact.writtenBy pinned "Output.x = 1\nOutput.y = ToUpper([1])"
    let p : Artifact.Pos = { Line = line; Column = column; Start = start; End = stop }
    let program =
        match a.Program with
        | [ s1; Artifact.SAssign(t, op, Artifact.ECall(n, args, _), h, sp) ] -> [ s1; Artifact.SAssign(t, op, Artifact.ECall(n, args, p), h, sp) ]
        | other -> Values.fail (sprintf "unexpected program %A" other)
    let r = Script.runBytesWith pinned (Artifact.encode { a with Program = program })
    fails MessageCode.ArgumentTypeMismatch r
    at eLine eColumn eLength (Script.onlyErrorMessage r)

// -- 130.2.x The result envelope -----------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1478")>]
let ``Result_HasAnOptionalValue_TheDiagnostics_AndASuccessIndicator`` () =
    for src in [ "Output.a = 1"; "Output.a = 1 / 0"; "var = ("; "fail \"x\""; "exit \"y\"" ] do
        let r = Script.run src
        let has, _ = r.TryGetValue()
        Assert.Equal(has, r.HasValue)
        Assert.NotNull(r.Messages)
        Assert.Equal(r.IsSuccess, not (r.Messages |> Seq.exists (fun m -> m.Severity = MessageSeverity.Error)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1478")>]
[<Trait("requirement", "JYRO-REQ-1486")>]
let ``SeveralDiagnostics_AreInPositionOrder`` () =
    let src = lines [ "Output.a = 1"; "Output.b = 2"; "Output.c = u3"; "u1 = 1"; "Output.d = u5 + u6" ]
    let r = Script.compile src
    let pos = [ for m in r.Messages -> m.Location.Value.Line, m.Location.Value.Column ]
    Assert.True(pos.Length >= 3, Script.describe r)
    Assert.Equal<(int * int) list>(List.sort pos, pos)

let private startedCases =
    [ "runtime", "Output.a = 1\nOutput.x = 1 / 0"
      "fail", "Output.a = 1\nfail \"x\""
      "statements", "Output.a = 1\n" + statementScript 10_001
      "loops", "Output.a = 1\nvar i = 0\nwhile i < 2000 do i = i + 1 end"
      "depth", "Output.a = 1\n" + recursion 70
      "nesting", "Output.a = 1\n" + buildNested "n" 200 + "\nOutput.j = ToJson(n)"
      "exit", "Output.a = 1\nexit \"bye\"" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1479")>]
[<Trait("requirement", "JYRO-REQ-1480")>]
let ``ExecutionStarted_TheValueIsTheFinalOutput_OnEveryEntryPath`` () =
    for path in allPaths do
        for name, src in startedCases do
            let r = runOnPath path src
            Assert.True(r.HasValue, sprintf "%A %s: no value. %s" path name (Script.describe r))
            let v = Script.valueOf r
            Assert.True(hasProp "a" v, sprintf "%A %s: Output.a is missing" path name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1479")>]
let ``ExecutionStarted_TimeLimitCancellationAndHostThrow_TheValueIsTheFinalOutput`` () =
    // Output.a = 1 must run before the limit: marginLimitMs (Support.fs) is ten
    // times the longest pre-stall step measured under a whole verification run
    // (Output.a = 1: 138.29 ms; any shape: 254.42 ms; floors-process 4.1.18, d-585).
    let st = stall ()
    let r = Script.runWith (marginOpts [ st ]) (sprintf "Output.a = 1\nStall(%d)\nOutput.b = 2" marginStallMs)
    Assert.True(hasProp "a" (Script.valueOf r))
    use cts = new Threading.CancellationTokenSource()
    let c = canceller cts
    let r2 = Script.runWith { Script.defaults with Functions = [ c.AsFunction ]; Cancellation = Some cts.Token } "Output.a = 1\nCancelNow()\nOutput.b = 2"
    Assert.True(hasProp "a" (Script.valueOf r2))
    let thrower = Host.func "Throw" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (InvalidOperationException "boom"))
    let r3 = Script.runWith { Script.defaults with Functions = [ thrower.AsFunction ] } "Output.a = 1\nThrow()"
    Assert.False(r3.IsSuccess)
    Assert.True(hasProp "a" (Script.valueOf r3))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1479")>]
let ``HostFirstOutput_TheValueIsItAfterTheChanges`` () =
    let first = Values.obj [ "pre", Values.int 1 ]
    let r = Script.runWith { Script.defaults with Output = Some first } "Output.a = 1\nOutput.x = 1 / 0"
    let v = Script.valueOf r
    Assert.True(hasProp "pre" v && hasProp "a" v, Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1481")>]
let ``SuccessIndicator_IsTrueExactlyWhenThereIsNoError_InTheCorpus`` () =
    for o in corpus () do
        let hasError = o.Messages |> List.exists (fun m -> m.Severity = MessageSeverity.Error)
        Assert.True((o.Ok = not hasError), sprintf "%s: IsSuccess %b with errors %b" o.Name o.Ok hasError)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1481")>]
let ``SuccessIndicator_OfResultsBuiltByThePublicFactories_FollowsTheDiagnostics`` () =
    let err = DiagnosticMessage.Error(MessageCode.RuntimeError, MessageStage.Execute)
    let warn = DiagnosticMessage.Warning(MessageCode.RuntimeError, MessageStage.Execute)
    let info = DiagnosticMessage.Info(MessageCode.RuntimeError, MessageStage.Execute)
    // A factory may refuse a combination; a result it does give must follow the rule.
    let check (expected: bool) (make: unit -> JyroResult<int>) =
        match (try Some(make ()) with :? ArgumentException -> None) with
        | Some r -> Assert.Equal(expected, r.IsSuccess)
        | None -> ()
    check false (fun () -> JyroResult<int>.Success(1, [ err ]))
    check true (fun () -> JyroResult<int>.Success(1, [ warn; info ]))
    check true (fun () -> JyroResult<int>.Failure(1, [ warn ]))
    check true (fun () -> JyroResult<int>.Failure([ info ]))
    check false (fun () -> JyroResult<int>.Failure(err))
    check false (fun () -> JyroResult<int>.Failure(1, [ err; warn ]))
    check true (fun () -> JyroResult<int>.Success(1))

// -- 130.3.x / 130.4.x Parse and validation errors block execution ---------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1482")>]
let ``ParseError_TheScriptDoesNotRun_AndLaterStagesDoNotStart`` () =
    let side = Host.recorder "SideEffect"
    let o, s = withStats { Script.defaults with Functions = [ side.AsFunction ] }
    let r = Script.runWith o "Output.a = 1\nSideEffect()\nvar x = ("
    Assert.False(r.IsSuccess)
    Assert.Equal(0, side.CallCount)
    match Script.value r with
    | Some v -> Assert.False(hasProp "a" v, "The script ran")
    | None -> ()
    for name in [ "Validate"; "Link"; "Compile"; "Execute" ] do
        let t = stageTimes s |> List.find (fun (n, _) -> n = name) |> snd
        Assert.True((t = TimeSpan.Zero), sprintf "%s ran after a parse error (%O)" name t)
    for d in r.Messages do
        Assert.DoesNotContain(d.Code, [ MessageCode.UndeclaredVariable; MessageCode.UndefinedFunction; MessageCode.ScriptFailure ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1483")>]
let ``JYRO-REQ-1483 SeveralParseErrors_AllReachTheHost`` () =
    let src = lines [ "Output.a = \"\\q\""; "Output.b = 2"; "Output.c = 3"; "Output.d = 4"; "Output.e = \"\\z\""; "Output.f = \"open" ]
    let r = Script.compile src
    let found = [ for m in r.Messages -> m.Code, (if m.Location.HasValue then m.Location.Value.Line else 0) ]
    Assert.Contains((MessageCode.InvalidEscapeSequence, 1), found)
    Assert.Contains((MessageCode.InvalidEscapeSequence, 5), found)
    Assert.Contains((MessageCode.UnterminatedString, 6), found)

[<Theory>]
[<InlineData("Output.q = zzz", "UndeclaredVariable")>]
[<InlineData("var a = 1\nvar a = 2", "VariableAlreadyDeclared")>]
[<InlineData("break", "LoopStatementOutsideOfLoop")>]
[<InlineData("return 1", "ReturnOutsideFunction")>]
[<InlineData("Input = 1", "InputIsReadOnly")>]
[<InlineData("1 = 2", "InvalidAssignmentTarget")>]
[<InlineData("func F()\n  func G()\n  end\n  return 1\nend", "NestedFunctionDeclaration")>]
[<InlineData("func F()\n  Output.z = 1\n  return 1\nend", "OutputAccessInFunction")>]
[<Trait("requirement", "JYRO-REQ-1484")>]
let ``ValidationError_TheScriptDoesNotRun`` (bad: string, code: string) =
    let side = Host.recorder "SideEffect"
    let r = Script.runWith { Script.defaults with Functions = [ side.AsFunction ] } ("Output.a = 1\nSideEffect()\n" + bad)
    Assert.Contains(Enum.Parse<MessageCode> code, Script.errorCodes r)
    Assert.Equal(0, side.CallCount)
    match Script.value r with
    | Some v -> Assert.False(hasProp "a" v, "The script ran")
    | None -> ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1485")>]
let ``Validation_ContinuesAfterTheFirstError`` () =
    let r = Script.compile (lines [ "Output.a = 1"; "Output.b = zzz"; "Output.c = 3"; "break"; "Output.d = 5"; "return 6" ])
    let codes = Script.errorCodes r
    Assert.Contains(MessageCode.UndeclaredVariable, codes)
    Assert.Contains(MessageCode.LoopStatementOutsideOfLoop, codes)
    Assert.Contains(MessageCode.ReturnOutsideFunction, codes)

// -- 130.5.x Order and duplicates -----------------------------------------------------------------

let private positioned (ms: DiagnosticMessage list) =
    ms |> List.filter (fun m -> m.Location.HasValue) |> List.map (fun m -> m.Location.Value.Line, m.Location.Value.Column)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1486")>]
let ``DiagnosticsOfOneSource_AreOrderedByLineThenColumn`` () =
    let src =
        lines [ "func F()"
                "  return q1 + q2"
                "end"
                "u1 = u2"
                "Output.a = u3"
                "Output.b = u4 + u5"
                ""
                ""
                "Output.c = u9"
                "Output.d = u10"
                "Output.e = u11 + u12 + u13" ]
    let r = Script.compile src
    let pos = positioned (List.ofSeq r.Messages)
    Assert.True(pos.Length >= 8, Script.describe r)
    Assert.Equal<(int * int) list>(List.sort pos, pos)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1486")>]
let ``DiagnosticsWithNoPosition_ComeAfterEveryPositionedOne_InTheCorpus`` () =
    for o in corpus () do
        let flags = o.Messages |> List.map (fun m -> m.Location.HasValue)
        let firstUnpositioned = flags |> List.tryFindIndex not
        match firstUnpositioned with
        | Some i -> Assert.True(flags |> List.skip i |> List.forall not, sprintf "%s: a positioned diagnostic follows one without a position" o.Name)
        | None -> ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1487")>]
let ``NoResult_HasTwoDiagnosticsWithTheSameCodePositionAndArguments`` () =
    let extra =
        [ outcome "twice-on-a-line" (Script.compile "Output.a = zzz + zzz")
          outcome "twice-in-a-loop" (Script.compile "foreach x in [1, 2] do\n  Output.a = zzz\nend")
          outcome "runtime-via-host" (
            let h = Host.func "Boom" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Runtime(MessageCode.DivisionByZero)))
            Script.runWith { Script.defaults with Functions = [ h.AsFunction ] } "Output.a = Boom()") ]
    for o in corpus () @ extra do
        let keys =
            o.Messages |> List.map (fun m -> m.Code, (if m.Location.HasValue then string m.Location.Value else "-"), String.Join("|", m.Args |> Array.map string))
        Assert.True((List.length keys = List.length (List.distinct keys)), sprintf "%s has a duplicate diagnostic: %A" o.Name keys)

// JYRO-REQ-1488's tests are with the further cases of JYRO-WRK-14 below
// ("diagnostics at one line and column are in stage order").

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1489")>]
let ``LibraryDiagnostics_AreGroupedBySourceInTheOrderAdded_ThenTheScript`` () =
    let l1 = "func A()\n  return 1\nend\nvar x1 = 1\nfunc B()\n  return 2\nend\nvar y1 = 2"
    let l2 = "var x2 = 1\nfunc C()\n  return 1\nend\nvar y2 = 2"
    let check (libs: string list) (expected: int list) =
        let r = Script.compileWith { Script.defaults with Libraries = libs } "Output.a = zzz"
        let libLines =
            r.Messages
            |> Seq.filter (fun m -> m.Code = MessageCode.LibraryStatementForbidden)
            |> Seq.map (fun m -> m.Location.Value.Line)
            |> List.ofSeq
        Assert.Equal<int list>(expected, libLines)
        // The script's diagnostic is reported (JYRO-ADR-115 point 3: a library's diagnostic does
        // not stop the script's own Parse or Validate; JYRO-DEF-512), and comes after every
        // library diagnostic. The test fails when the diagnostic it set up is absent.
        let idx = r.Messages |> Seq.tryFindIndex (fun m -> m.Code = MessageCode.UndeclaredVariable)
        let lastLib = r.Messages |> Seq.findIndexBack (fun m -> m.Code = MessageCode.LibraryStatementForbidden)
        match idx with
        | Some i -> Assert.True(i > lastLib, "The script's diagnostic precedes a library diagnostic. " + Script.describe r)
        | None -> Values.fail ("The script's UndeclaredVariable is not reported beside the libraries' diagnostics. " + Script.describe r)
    check [ l1; l2 ] [ 4; 8; 1; 5 ]
    check [ l2; l1 ] [ 1; 5; 4; 8 ]

// -- 130.6.x Messages ---------------------------------------------------------------------------

let private sampleArgs : obj[] = [| box "a0"; box "a1"; box "a2"; box "a3"; box "a4"; box "a5" |]

/// The key of a diagnostic for 130.6.1: its code and its arguments, each read by
/// its invariant string form (160.7.16).
let private codeAndArgs (d: DiagnosticMessage) : MessageCode * string list =
    d.Code, (d.Args |> Array.map (fun a -> if isNull a then "<null>" else argText a) |> List.ofArray)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 Message_IsMadeFromTheCodeAndItsArguments_InTheCorpus`` () =
    // The oracle is the clause itself (floors-process 4.1.17): the book declares
    // no table of templates, so what 130.6.1 pins is that the message is a
    // function of the code and the arguments alone. Every corpus source is run
    // again two lines down and indented, which moves each diagnostic and changes
    // nothing it is made from; every diagnostic of the same code and arguments,
    // wherever it was raised, must then have the same message.
    let moved = [ for name, src in corpusSources -> outcome (name + "-moved") (Script.run ("\n\n  " + src)) ]
    let all = [ for o in corpus () @ moved do for d in o.Messages -> o.Name, d ]
    for _, d in all do Assert.False(isNull d.Message, sprintf "%A has a null message" d.Code)
    let groups = all |> List.groupBy (fun (_, d) -> codeAndArgs d)
    for (code, args), members in groups do
        match members |> List.map (fun (_, d) -> d.Message) |> List.distinct with
        | [ _ ] -> ()
        | messages ->
            let where = members |> List.map fst |> String.concat ", "
            Values.fail (sprintf "%A with the arguments %A has the messages %A (%s)" code args messages where)
    // The check can fail: most diagnostics are met twice, once at each place.
    let paired = groups |> List.filter (fun (_, ms) -> ms.Length >= 2) |> List.length
    Assert.True(paired >= 15, sprintf "only %d code-and-arguments pairs were met more than once" paired)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
[<Trait("requirement", "JYRO-REQ-250")>]
let ``JYRO-REQ-2155 Message_OfTheScriptFailureOfFail_IsTheTextOrTheFixedSentence_InTheCorpus`` () =
    // The corpus's two fails, with the messages 50.11.4 and 160.7.16 give: the
    // one argument, the text, and the message that text unchanged; no argument
    // and the message `Script execution failed`.
    let outcomes = corpus ()
    let failureOf (name: string) =
        let o = outcomes |> List.find (fun o -> o.Name = name)
        match o.Messages |> List.filter (fun m -> m.Code = MessageCode.ScriptFailure) with
        | [ d ] -> d
        | l -> Values.fail (sprintf "%s: expected one ScriptFailure, got %d" name l.Length)
    let withValue = failureOf "runtime-fail"
    Assert.Equal<string list>([ "bad" ], withValue.Args |> Array.map argText |> List.ofArray)
    Assert.Equal("bad", withValue.Message)
    let noValue = failureOf "runtime-fail-empty"
    Assert.NotNull(noValue.Args)
    Assert.True(noValue.Args.Length = 0, sprintf "Args of fail with no value is %A, not empty" (snd (codeAndArgs noValue)))
    Assert.Equal("Script execution failed", noValue.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 Message_OfEveryCode_DependsOnlyOnTheCodeAndItsArguments`` () =
    // For every code, two diagnostics made from the same code and arguments - one
    // with a place and one without, the arguments as other objects of the same
    // text - have the same message (the book declares no template to compare
    // with, floors-process 4.1.17). Two codes whose messages differ show the
    // message is made from the code.
    for c in Enum.GetValues(typeof<MessageCode>) |> Seq.cast<MessageCode> do
        let a = DiagnosticMessage.Error(c, MessageStage.Execute, sampleArgs)
        let b = DiagnosticMessage.Error(c, MessageStage.Execute, Nullable(SourceLocation(7, 3, 1)), sampleArgs |> Array.map (fun o -> box (String((string o).ToCharArray()))))
        Assert.False(isNull a.Message, sprintf "%A has a null message" c)
        Assert.True((a.Message = b.Message), sprintf "%A: %A without a place, %A at 7:3" c a.Message b.Message)
    let messages = [ for c in Enum.GetValues(typeof<MessageCode>) |> Seq.cast<MessageCode> -> (DiagnosticMessage.Error(c, MessageStage.Execute, sampleArgs)).Message ]
    Assert.True((List.distinct messages).Length > 1, "every code has the same message")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 SameCodeAndArguments_GiveTheSameMessage_WhereverRaised`` () =
    let a = Script.run "Output.x = 1 / 0" |> Script.onlyErrorMessage
    let b = Script.run "func F()\n  return 2 / 0\nend\nOutput.y = F()" |> Script.onlyErrorMessage
    Assert.Equal(a.Code, b.Code)
    if a.Args = b.Args then Assert.Equal(a.Message, b.Message)

let private mismatchOf (what: string) (r: JyroResult<'T>) : DiagnosticMessage =
    match r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.ArgumentTypeMismatch) |> List.ofSeq with
    | [ m ] -> m
    | l -> Values.fail (sprintf "%s: expected one ArgumentTypeMismatch, got %d. %s" what l.Length (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 ArgumentTypeMismatch_OfToUpper_AtLine1AndAtLine3_HasTheSameMessage`` () =
    let at1 = mismatchOf "line 1" (Script.run "Output.x = ToUpper([1])")
    let at3 = mismatchOf "line 3" (Script.run "Output.a = 1\nOutput.b = 2\nOutput.x = ToUpper([1])")
    Assert.Equal(1, at1.Location.Value.Line)
    Assert.Equal(3, at3.Location.Value.Line)
    Assert.Equal<string list>((at1.Args |> Array.map argText |> List.ofArray), (at3.Args |> Array.map argText |> List.ofArray))
    Assert.False(String.IsNullOrEmpty at1.Message)
    Assert.Equal(at1.Message, at3.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 ArgumentTypeMismatch_InALibraryScript_HasTheMessageOfTheSameMismatchInAScript`` () =
    let inScript = mismatchOf "script" (Script.run "var v = [1]\nOutput.x = ToUpper(v)")
    let lib = "\n\nfunc Shout(v)\n  return ToUpper(v)\nend"
    let inLibrary = mismatchOf "library" (Script.runWith { Script.defaults with Libraries = [ lib ] } "Output.x = Shout([1])")
    Assert.NotEqual(inScript.Location.Value.Line, inLibrary.Location.Value.Line)
    Assert.Equal<string list>((inScript.Args |> Array.map argText |> List.ofArray), (inLibrary.Args |> Array.map argText |> List.ofArray))
    Assert.Equal(inScript.Message, inLibrary.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 ArgumentTypeMismatch_OnARunFromAnArtifact_HasTheMessageOfARunFromSource`` () =
    let src = "var v = [1]\nOutput.x = ToUpper(v)"
    let fromSource = mismatchOf "source" (Script.run src)
    let bytes = Script.compileBytesWith Script.defaults src |> Script.valueOf
    let fromArtifact = mismatchOf "artifact" (Script.runBytesWith Script.defaults bytes)
    Assert.Equal<string list>((fromSource.Args |> Array.map argText |> List.ofArray), (fromArtifact.Args |> Array.map argText |> List.ofArray))
    Assert.Equal(fromSource.Message, fromArtifact.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2155")>]
let ``JYRO-REQ-2155 SecondCode_AtTwoPlaces_HasTheSameMessage_AndDifferentArgumentsMayDiffer`` () =
    let a = Script.run "Output.x = 1\nOutput.y = 1 / 0" |> Script.onlyErrorMessage
    let b = Script.run "\n\n\nfunc F()\n  return 2 / 0\nend\nOutput.y = F()" |> Script.onlyErrorMessage
    Assert.Equal(a.Code, b.Code)
    Assert.Equal<string list>((a.Args |> Array.map argText |> List.ofArray), (b.Args |> Array.map argText |> List.ofArray))
    Assert.Equal(a.Message, b.Message)
    // Different arguments of the same code may differ (the message is a function of them).
    let n = mismatchOf "array" (Script.run "var v = [1]\nOutput.x = ToUpper(v)")
    let m = mismatchOf "object" (Script.run "var v = {a: 1}\nOutput.x = ToUpper(v)")
    Assert.NotEqual<string list>((n.Args |> Array.map argText |> List.ofArray), (m.Args |> Array.map argText |> List.ofArray))

let private marker7f3a = "Marker sentence 7f3a."

let private platformMessage (f: unit -> unit) : string =
    try
        f ()
        ""
    with e -> e.Message

let private argsText (ms: DiagnosticMessage seq) : string list =
    [ for m in ms do for a in m.Args -> string a ]

/// A .NET class name, such as the type of an exception, is not a name the specification
/// gives (1491's rationale, JYRO-ADR-125).
let private isDotNetTypeName (s: string) : bool =
    Regex.IsMatch(s, @"^System\.|Exception$|^Microsoft\.|^FSharp\.")

/// A number followed by a noun, such as `2 elements` (130.6.2: the noun belongs in the template).
let private numberWithANoun = Regex(@"^[-+]?\d[\d.,eE+-]*\s+[A-Za-z]")

/// An English article and a noun, such as `a list`.
let private articleAndNoun = Regex(@"^(a|an|the)\s+\S", RegexOptions.IgnoreCase)

/// The fixed phrases JYRO-DEF-535 found as arguments; none is a value of a script or a name
/// the specification gives.
let private fixedPhrases =
    set [ "end of source"; "operands"; "a list"; "a string"; "a literal"; "the function table"; "JSON input"; "Compiled artifact" ]

let private typeNames = set [ "null"; "boolean"; "number"; "string"; "array"; "object"; "function" ]
let private stageNames = set [ "Deserialize"; "Parse"; "Validate"; "Link"; "Compile"; "Execute" ]

/// Names a clause of a code lists: `Input` for the source of a number (30.2), the parts
/// of an artifact that ArtifactCannotEncode names (150.11), and the effects of Part 120
/// that FunctionEffectExceedsMax compares (120.5.2).
let private listedNames =
    set ([ "Input"; "File"; "String"; "List"; "FunctionTable"; "NestingDepth"; "ObjectLiteral"; "If"; "FuncDef"; "Call"; "UnionVariant"; "Literal" ]
         @ [ "Pure"; "NonDeterministic"; "Mutating"; "Blocking" ])

let private stdlibNames = lazy (stdlibAll () |> List.map (fun f -> f.Name) |> set)
let private stdlibParameterNames = lazy (set [ for f in stdlibAll () do for p in f.Signature.Parameters -> p.Name ])

let private functionCall = Regex(@"^([A-Za-z_][A-Za-z0-9_]*)\(\)$")

/// An operator as the source writes it: punctuation only.
let private isOperator (s: string) : bool = s <> "" && Regex.IsMatch(s, @"^[+\-*/%<>=!&|?^~.]+$")

/// A name that this specification gives (1491's second kind), known without the script: a
/// function name with `()` of the 90.2 inventory, a parameter name of 130.6.4, a Jyro type
/// of 130.6.5, an operator, a stage of 160.1.11, or a name a code's clause lists.
let private isNameKind (s: string) : bool =
    let m = functionCall.Match s
    (m.Success && stdlibNames.Value.Contains m.Groups.[1].Value)
    || stdlibParameterNames.Value.Contains s
    || typeNames.Contains s
    || isOperator s
    || stageNames.Contains s
    || listedNames.Contains s

let private isNumberText (s: string) : bool =
    let st = Globalization.NumberStyles.Float
    let ci = Globalization.CultureInfo.InvariantCulture
    fst (Decimal.TryParse(s, st, ci)) || fst (Double.TryParse(s, st, ci))

/// A value's text: a JSON value (an array, an object, a quoted string, true, false, null).
let private isJsonText (s: string) : bool =
    let t = s.Trim()
    t <> "" && (t.StartsWith "[" || t.StartsWith "{" || t.StartsWith "\"" || t = "true" || t = "false" || t = "null")
    && (try JsonDocument.Parse(t) |> ignore; true with _ -> false)

/// Why the argument `a` of a diagnostic of a case whose script, input and host give the text
/// `text` is not of a kind 1491 allows, or None.
let private argumentFault (text: string) (a: string) : string option =
    let inText = text.Contains a
    if a.Contains "\n" then Some "several lines"
    elif isDotNetTypeName a then Some "a .NET type name"
    elif not inText && fixedPhrases.Contains a then Some "a fixed phrase"
    elif not inText && articleAndNoun.IsMatch a then Some "an article with a noun"
    elif not inText && numberWithANoun.IsMatch a then Some "a number with a noun"
    else
        let m = functionCall.Match a
        let ok =
            inText || isNumberText a || isJsonText a || isNameKind a
            || (m.Success && text.Contains m.Groups.[1].Value)
        if ok then None else Some "neither a value of the script nor a name the specification gives"

/// A host function that refuses a non-positive amount by throwing JyroError.Host (JYRO-ADR-124).
type private RefusingHost() =
    inherit JyroFunctionBase("Charge", Host.signature "Charge" [ Parameter.Required("amount", ParameterType.Number) ] ParameterType.Number)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(args, _) =
        let n = args.GetDecimal(0)
        if n <= 0m then raise (JyroError.Host("AmountNotPositive", box n)) else JyroNumber(n) :> JyroValue

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 HostFunctionException_ItsMessageIsNotAnArgument`` () =
    let h = Host.func "Throw" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (InvalidOperationException marker7f3a))
    let r = Script.runWith { Script.defaults with Functions = [ h.AsFunction ] } "Output.a = Throw()"
    Assert.False(r.IsSuccess)
    for a in argsText r.Messages do
        Assert.DoesNotContain("7f3a", a)
        Assert.False(isDotNetTypeName a, sprintf "Argument is a .NET type name: %s" a)

let platformCases : obj[] seq =
    [ "FromJson(\"{bad\")", "json"
      "RegexTest(\"a\", \"(\")", "regex"
      "Base64Decode(\"@@@\")", "base64"
      "ParseDate(\"2024-13-45\")", "date"
      "ToNumber(\"junk\")", "number" ]
    |> Seq.map (fun (c, k) -> [| box c; box k |])

[<Theory>]
[<MemberData(nameof platformCases)>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 PlatformException_ItsMessageIsNotAnArgument`` (call: string, kind: string) =
    let platform =
        match kind with
        | "json" -> platformMessage (fun () -> JsonDocument.Parse("{bad") |> ignore)
        | "regex" -> platformMessage (fun () -> Regex("(") |> ignore)
        | "base64" -> platformMessage (fun () -> Convert.FromBase64String("@@@") |> ignore)
        | "date" -> platformMessage (fun () -> DateTime.Parse("2024-13-45", Globalization.CultureInfo.InvariantCulture) |> ignore)
        | _ -> platformMessage (fun () -> Decimal.Parse("junk", Globalization.CultureInfo.InvariantCulture) |> ignore)
    let r = Script.run ("Output.a = " + call)
    for a in argsText r.Messages do
        if platform <> "" then Assert.DoesNotContain(platform, a)
        Assert.False(isDotNetTypeName a, sprintf "Argument is a .NET type name: %s" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 MalformedJsonFromTheHost_ItsParserMessageIsNotAnArgument`` () =
    let platform = platformMessage (fun () -> JsonDocument.Parse("{bad") |> ignore)
    let r = Script.runWith { Script.defaults with Input = JsonInput "{bad" } "Output.a = 1"
    Assert.False(r.IsSuccess)
    for a in argsText r.Messages do
        Assert.DoesNotContain(platform, a)
        Assert.DoesNotContain("LineNumber", a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 Arguments_AreTheVaryingValues`` () =
    let d = Script.compile "Output.a = someName" |> Script.onlyErrorMessage
    Assert.Contains("someName", argsText [ d ])
    let t = Script.run "Output.a = PadLeft([1], 3)" |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.ArgumentTypeMismatch, t.Code)
    let args = argsText [ t ] |> List.map (fun s -> s.ToLowerInvariant())
    Assert.True(args |> List.exists (fun a -> a.Contains "array"), sprintf "%A" args)

// -- 130.6.4 A diagnostic names a parameter by an argument ----------------------------

[<Theory>]
[<InlineData("Log(0, 1)")>]
[<InlineData("Log(0, 10)")>]
[<InlineData("Log(-1, 2)")>]
[<InlineData("Log(value: 0, base: 1)")>]
[<Trait("requirement", "JYRO-REQ-2055")>]
let ``JYRO-REQ-2055 LogOfZero_TheDiagnosticHasTheArgumentValue`` (call: string) =
    let d = Script.run ("Output.a = " + call) |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.NumberDomainError, d.Code)
    // One whole argument, spelled and cased as the signature of 90.2 gives it.
    Assert.Contains("value", argsText [ d ])

let logOfValueOutOfDomain : obj[] seq =
    [ "Log(0, 1)", "0"; "Log(0, 10)", "0"; "Log(-1, 2)", "-1"; "Log(value: 0, base: 1)", "0" ]
    |> Seq.map (fun (c, v) -> [| box c; box v |])

[<Theory>]
[<MemberData(nameof logOfValueOutOfDomain)>]
[<Trait("requirement", "JYRO-REQ-2055")>]
let ``JYRO-REQ-2055 Log names value in its own argument, followed by its value, and joins no names`` (call: string, value: string) =
    // JYRO-ADR-123: one parameter at fault gives the function's name, that parameter's
    // name and its value, each its own argument; no argument joins two names.
    let d = Script.run ("Output.a = " + call) |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.NumberDomainError, d.Code)
    let args = argsText [ d ]
    let i = List.findIndex ((=) "value") args
    Assert.True(i + 1 < args.Length, sprintf "no argument follows value: %A" args)
    Assert.Equal(value, args.[i + 1])
    Assert.DoesNotContain("base", args)
    for a in args do
        Assert.False(a.Contains "value" && a <> "value", sprintf "an argument joins value with other text: %A" args)

[<Theory>]
[<InlineData("x", "y")>]
[<InlineData("2026-02-30", "2025-02-29")>]
[<InlineData("", "")>]
[<Trait("requirement", "JYRO-REQ-2055")>]
let ``JYRO-REQ-2055 DateDiffWithNeitherDateReadable_TheDiagnosticHasTheArgumentStartDate`` (startDate: string, endDate: string) =
    let input = Values.obj [ "s", Values.str startDate; "e", Values.str endDate ]
    let d = Script.runInput input "Output.a = DateDiff(Input.s, Input.e, \"day\")" |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.DateFormatInvalid, d.Code)
    Assert.Contains("startDate", argsText [ d ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 CorpusArguments_AreNotFormattedText`` () =
    for o in corpus () do
        for d in o.Messages do
            if d.Code <> MessageCode.ScriptFailure then
                for a in d.Args do
                    let s = string a
                    Assert.False(s.Contains "\n", sprintf "%s: %A has a multi-line argument %A" o.Name d.Code s)

type private FixedProvider(t: string) =
    interface IMessageTemplateProvider with
        member _.TryGetTemplate(_code: MessageCode, [<Runtime.InteropServices.Out>] template: byref<string>) =
            template <- t
            true

type private EmptyProvider() =
    interface IMessageTemplateProvider with
        member _.TryGetTemplate(_code: MessageCode, [<Runtime.InteropServices.Out>] template: byref<string>) =
            template <- null
            false

let private placeholder = Regex(@"\{\d+\}")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 HostTemplates_GoThroughTheSameSubstitution_ForEveryCode`` () =
    // Every code but ScriptFailure: rc.9 excepts the ScriptFailure message of
    // fail, which is the one 50.11.4 gives whatever the template (tested with
    // real fails in HostTemplate_DoesNotChangeTheScriptFailureMessage below).
    // The book does not define what the substitution gives (d-578), so the
    // oracle is the clause's own words, not the implementation's templates
    // (floors-process 4.1.17): the same substitution for every code means the
    // same template and arguments give the same message whatever the code, and
    // no placeholder is left.
    let codes = Enum.GetValues(typeof<MessageCode>) |> Seq.cast<MessageCode> |> Seq.filter (fun c -> c <> MessageCode.ScriptFailure) |> List.ofSeq
    for template in [ "[L]{0}|{1}|{2}|{3}|{4}|{5}"; "{5}<{0}>{5}"; "no placeholder" ] do
        let p = TemplateProvider(template, None)
        let byCode = [ for c in codes -> c, localizedMessage p (DiagnosticMessage.Error(c, MessageStage.Execute, sampleArgs)) ]
        for c, m in byCode do
            Assert.False(placeholder.IsMatch m, sprintf "%A under %s leaves a placeholder: %s" c template m)
        match byCode |> List.map snd |> List.distinct with
        | [ _ ] -> ()
        | many ->
            let firstOf m = byCode |> List.find (fun (_, x) -> x = m) |> fst
            Values.fail (sprintf "under %s the codes give %d messages: %A" template many.Length [ for m in many -> firstOf m, m ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 HostTemplate_NamingAMissingArgument_LeavesNoPlaceholder`` () =
    let d = DiagnosticMessage.Error(MessageCode.UndeclaredVariable, MessageStage.Validate, [| box "x" |])
    let s = formatLocalized (FixedProvider("{0} and {5} and {1}")) d
    Assert.False(placeholder.IsMatch s, s)
    Assert.Contains("x", s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 ProviderWithNoTemplate_LeavesNoPlaceholder`` () =
    for c in Enum.GetValues(typeof<MessageCode>) |> Seq.cast<MessageCode> do
        let d = DiagnosticMessage.Error(c, MessageStage.Execute, sampleArgs)
        let s = formatLocalized (EmptyProvider()) d
        Assert.False(placeholder.IsMatch s, sprintf "%A: %s" c s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 ArgumentHoldingPlaceholderText_IsNotSubstitutedAgain`` () =
    let d = DiagnosticMessage.Error(MessageCode.UndeclaredVariable, MessageStage.Validate, [| box "{1}"; box "b" |])
    let s = formatLocalized (FixedProvider("<{0}-{1}>")) d
    Assert.Contains("<{1}-b>", s)

// -- JYRO-REQ-1713: the substitution of a host template, row by row -----------------------------

/// The message of `NumericElementRequired` with the arguments `args` under the template `t`.
let private substituted (t: string) (args: obj[]) : string =
    localizedMessage (TemplateProvider(t, None)) (DiagnosticMessage.Error(MessageCode.NumericElementRequired, MessageStage.Execute, args))

/// The message that `Sum([1, null, "a"])` gives, under the template `t`: its arguments are `Sum` and `2` (90.7.4).
let private sumMessage (t: string) : string =
    let d = Script.runWith pinned "Output.r = Sum([1, null, \"a\"])" |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.NumericElementRequired, d.Code)
    localizedMessage (TemplateProvider(t, Some MessageCode.NumericElementRequired)) d

[<Theory>]
[<InlineData("X{0}Y", "XSumY")>]
[<InlineData("{1} in {0}", "2 in Sum")>]
[<InlineData("X{0}Y {2}", "XSumY")>]
[<InlineData("{{0}} is {0}", "{0} is Sum")>]
[<InlineData("[{1,3}] [{1,-3}]", "[  2] [2  ]")>]
[<InlineData("{x} {", "{x} {")>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_TheClausesTable_ForSumOfAnArrayWithANonNumber`` (template: string, expected: string) =
    Assert.Equal(expected, sumMessage template)

[<Theory>]
// Escapes, and the first row that starts at a position wins.
[<InlineData("{{}}", "{}")>]
[<InlineData("}}{{", "}{")>]
[<InlineData("{{{0}}}", "{Sum}")>]
[<InlineData("{{{{", "{{")>]
[<InlineData("a}b", "a}b")>]
[<InlineData("a{b", "a{b")>]
[<InlineData("{0", "{0")>]
[<InlineData("{0 }", "{0 }")>]
[<InlineData("{}", "{}")>]
[<InlineData("{,3}", "{,3}")>]
[<InlineData("{0,}", "{0,}")>]
[<InlineData("{0:x{1}", "{0:x2")>]
// Leading zeros in the index; an index not less than the count removes the placeholder.
[<InlineData("{00}|{01}", "Sum|2")>]
[<InlineData("X{2}Y", "XY")>]
[<InlineData("X{99999999999999999999}Y", "XY")>]
// White space before a kept placeholder is kept, and nothing is trimmed when nothing was removed.
[<InlineData("A  {0}", "A  Sum")>]
[<InlineData("  A {0}  ", "  A Sum  ")>]
[<InlineData(" {0} ", " Sum ")>]
// A removal takes the white space before the placeholder, then the message is trimmed at both ends.
[<InlineData("A {2}B", "AB")>]
[<InlineData("A \t {2} B", "A B")>]
[<InlineData("  A  {2}  ", "A")>]
[<InlineData("  {2}  A", "A")>]
[<InlineData("   {2}   ", "")>]
[<InlineData("{5}", "")>]
[<InlineData("  {0}  {7}", "Sum")>]
// Unicode White_Space (10.6.1) counts, other code points do not.
[<InlineData("A {2}B", "AB")>]
[<InlineData("A {0}", "A Sum")>]
[<InlineData("　A{2}", "A")>]
[<InlineData("A​{2}", "A​")>]
// Alignment and its white space; the format is that of composite formatting.
[<InlineData("[{1, 3}]", "[  2]")>]
// .NET composite formatting skips only the space after the comma, so a tab makes String.Format throw
// FormatException and the clause gives the unpadded 160.7.16 string.
[<InlineData("[{1,\t-3}]", "[2]")>]
// An index not less than the count removes the placeholder whatever its alignment, with the white space before it.
[<InlineData("[{5,\t3}]", "[]")>]
[<InlineData("A {5,\t3}", "A")>]
[<InlineData("[{0,6}]", "[   Sum]")>]
[<InlineData("[{0,-6}]", "[Sum   ]")>]
[<InlineData("[{1,2:}]", "[ 2]")>]
// Only the ASCII digits make an index or an alignment: other decimal digits are copied.
[<InlineData("a{٣}b", "a{٣}b")>]
[<InlineData("{0٣}", "{0٣}")>]
[<InlineData("{٠}", "{٠}")>]
[<InlineData("[{0,٣}]", "[{0,٣}]")>]
[<InlineData("A {٣}", "A {٣}")>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_EveryRuleOfTheClause`` (template: string, expected: string) =
    Assert.Equal(expected, substituted template [| box "Sum"; box "2" |])

[<Theory>]
[<InlineData("{0}", "{1}", "{1}")>]
[<InlineData("{0}", "{{", "{{")>]
[<InlineData("{0}", "}}x{{", "}}x{{")>]
[<InlineData("<{0}|{1}>", "{1}", "<{1}|a>")>]
[<InlineData("{1} {0}", "{0}", "a {0}")>]
[<InlineData("{0}", "  {2}  ", "  {2}  ")>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_AnArgumentIsCopiedAsItIsAndNotReadForPlaceholders`` (template: string, arg0: string, expected: string) =
    // The second argument is `a` in every row.
    Assert.Equal(expected, substituted template [| box arg0; box "a" |])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_AnArgumentOfWhiteSpaceOnlyIsNotTrimmedWhenNothingWasRemoved`` () =
    Assert.Equal("[  ]", substituted "[{0}]" [| box "  " |])
    Assert.Equal("  ", substituted "{0}" [| box "  " |])

/// Run `f` with the current culture of the thread set to `name`, and restore it.
let private underCulture (name: string) (f: unit -> unit) : unit =
    let saved = Globalization.CultureInfo.CurrentCulture
    let savedUi = Globalization.CultureInfo.CurrentUICulture
    try
        Globalization.CultureInfo.CurrentCulture <- Globalization.CultureInfo.GetCultureInfo name
        Globalization.CultureInfo.CurrentUICulture <- Globalization.CultureInfo.GetCultureInfo name
        f ()
    finally
        Globalization.CultureInfo.CurrentCulture <- saved
        Globalization.CultureInfo.CurrentUICulture <- savedUi

[<Theory>]
[<InlineData("de-DE")>]
[<InlineData("fr-FR")>]
[<InlineData("")>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_TheStringOfAnArgumentIsTheInvariantOne_WhateverTheThreadCulture`` (culture: string) =
    underCulture culture (fun () ->
        let args = [| box "Sum"; box 1234.5; box 0.25m; box (DateTime(2024, 3, 5, 7, 8, 9, DateTimeKind.Utc)) |]
        Assert.Equal("1234.5", substituted "{1}" args)
        Assert.Equal("0.25", substituted "{2}" args)
        Assert.Equal("1,234.5", substituted "{1:N1}" args)
        Assert.Equal("[  1,234.5]", substituted "[{1,9:N1}]" args)
        Assert.Equal("0.25", substituted "{2:0.00}" args)
        Assert.Equal("2024-03-05", substituted "{3:yyyy-MM-dd}" args))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_AFormatThatThrowsFormatException_GivesTheStringOf160_7_16`` () =
    // Enum.ToString("Q") throws FormatException: the string of the element is then used.
    let e = box DayOfWeek.Monday
    let got = substituted "[{0:Q}]" [| e |]
    Assert.Equal("[" + argText e + "]", got)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_NoPlaceholderSurvives_WhateverTheTemplateNamesAndTheArgumentCount`` () =
    let placeholderLike = Text.RegularExpressions.Regex(@"\{[0-9]+(,\s*-?[0-9]+)?(:[^{}]*)?\}")
    for template in [ "{0}"; "{1}"; "{2}"; "{3} {4}"; "a{0,5}b{9:x}"; "{0:N2}{7}" ] do
        for args in [ [||]; [| box "x" |]; [| box "x"; box "y" |] ] do
            for c in Enum.GetValues(typeof<MessageCode>) |> Seq.cast<MessageCode> |> Seq.filter (fun c -> c <> MessageCode.ScriptFailure) do
                let m = localizedMessage (TemplateProvider(template, None)) (DiagnosticMessage.Error(c, MessageStage.Execute, args))
                Assert.False(placeholderLike.IsMatch m, sprintf "%A %A under %s left a placeholder: %s" c args template m)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 Substitution_ReadsTheTemplateOnlyForTheCodeTheProviderGives`` () =
    // The provider gives a template for NumericElementRequired only: the message of another code
    // is not made from it.
    let d = Script.runWith pinned "Output.r = 1 / 0" |> Script.onlyErrorMessage
    let withOther = localizedMessage (TemplateProvider("X{0}Y", Some MessageCode.NumericElementRequired)) d
    Assert.Equal(d.Message, withOther)

// -- 130.6.2 and 130.6.3: the one exception, the ScriptFailure of fail -------------------------

let private inFunctionBody (statement: string) : string = "func F()\n  " + statement + "\nend\nF()"

let failPlaces : obj[] seq = [ [| box "top level" |]; [| box "function body" |] ]

let private placed (place: string) (statement: string) : string =
    if place = "top level" then statement else inFunctionBody statement

[<Theory>]
[<MemberData(nameof failPlaces)>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 Fail_WithASentence_ItsOneArgumentIsTheText`` (place: string) =
    // The clause's example: the sentence is the one argument, read by its string form.
    let d = Script.runWith pinned (placed place "fail \"The order total is negative.\"") |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.ScriptFailure, d.Code)
    Assert.Equal<string list>([ "The order total is negative." ], d.Args |> Array.map argText |> List.ofArray)

/// The providers of 130.6.3's example: `Failed: {0}` for ScriptFailure only,
/// and `Failed: {0}` for every code.
let templateProviders : obj[] seq =
    [ for place in [ "top level"; "function body" ] do
        for scope in [ "ScriptFailure only"; "every code" ] -> [| box place; box scope |] ]

let private failedProvider (scope: string) : IMessageTemplateProvider =
    TemplateProvider("Failed: {0}", (if scope = "every code" then None else Some MessageCode.ScriptFailure)) :> _

[<Theory>]
[<MemberData(nameof templateProviders)>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 HostTemplate_DoesNotChangeTheScriptFailureMessage`` (place: string, scope: string) =
    // The message is the one 50.11.4 gives whatever the template, and the text of
    // the fail value is copied unchanged, not read as a template.
    for statement, message in [ "fail \"boom\"", "boom"; "fail \"{0}\"", "{0}"; "fail \"\"", ""; "fail", "Script execution failed" ] do
        let d = Script.runWith pinned (placed place statement) |> Script.onlyErrorMessage
        Assert.Equal(MessageCode.ScriptFailure, d.Code)
        let got = localizedMessage (failedProvider scope) d
        Assert.True((got = message), sprintf "%s %s under Failed: {0} (%s): the message is %A, not %A" place statement scope got message)

[<Theory>]
[<InlineData("ScriptFailure only")>]
[<InlineData("every code")>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 HostTemplate_TopLevelFailBoom_IsTheClausesExample`` (scope: string) =
    let d = Script.runWith pinned "fail \"boom\"" |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.ScriptFailure, d.Code)
    Assert.Equal("[JM5004] Ln 1, Col 1: boom", formatLocalized (failedProvider scope) d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 HostTemplate_DoesNotChangeDiagnosticMessageMessage`` () =
    // DiagnosticMessage.Message is not made from a host's template.
    for source in [ "fail \"boom\""; "var s: string"; "Output.x = 1 / 0"; "fail" ] do
        let d = Script.runWith pinned source |> Script.onlyErrorMessage
        let before = d.Message
        formatLocalized (TemplateProvider("Z{0}Z", None)) d |> ignore
        Assert.Equal(before, d.Message)

// -- JYRO-WRK-14: further cases, named by the requirement each verifies ------------------------

/// Five lexical errors, one on each line, each independent of the others: every one is a
/// parse error the result must give (130.3.2), with its code and its line. Only the codes
/// of requirements that pass outside this work order are used (InvalidEscapeSequence,
/// 20.7, JYRO-REQ-37; UnterminatedString, JYRO-REQ-39), so the test shows 130.3.2 alone.
let private fiveParseErrors =
    lines [ "Output.a = \"\\q\""
            "Output.b = 2"
            "Output.c = \"\\z\""
            "Output.d = \"x\\y\""
            "Output.e = \"\\k\""
            "Output.f = \"open" ]

let private fiveParseErrorsExpected =
    [ MessageCode.InvalidEscapeSequence, 1
      MessageCode.InvalidEscapeSequence, 3
      MessageCode.InvalidEscapeSequence, 4
      MessageCode.InvalidEscapeSequence, 5
      MessageCode.UnterminatedString, 6 ]

let entryPaths : obj[] seq = pathNames

let private codesAndLines (r: JyroResult<'T>) : (MessageCode * int) list =
    [ for m in r.Messages -> m.Code, (if m.Location.HasValue then m.Location.Value.Line else 0) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1483")>]
let ``JYRO-REQ-1483 every parse error of a compilation reaches the host`` () =
    let r = Script.compileWith pinned fiveParseErrors
    Assert.False(r.IsSuccess)
    let found = codesAndLines r
    for e in fiveParseErrorsExpected do
        Assert.True(List.contains e found, sprintf "%A is missing from %A" e found)
    for m in r.Messages do Assert.Equal("Parse", Pending.stageOf m)

[<Theory>]
[<MemberData(nameof entryPaths)>]
[<Trait("requirement", "JYRO-REQ-1483")>]
let ``JYRO-REQ-1483 every parse error reaches the host on every entry path`` (path: string) =
    let r = runOnPath (pathNamed path) fiveParseErrors
    Assert.False(r.IsSuccess)
    let found = codesAndLines r
    for e in fiveParseErrorsExpected do
        Assert.True(List.contains e found, sprintf "%s: %A is missing from %A" path e found)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1483")>]
let ``JYRO-REQ-1483 parse errors before a syntax error all reach the host`` () =
    // Two lexical errors and then a token at which the parser can read no further.
    let r = Script.compileWith pinned (lines [ "Output.a = \"\\q\""; "Output.b = \"\\z\""; "Output.c = )" ])
    let found = codesAndLines r
    Assert.True(List.contains (MessageCode.InvalidEscapeSequence, 1) found, sprintf "%A" found)
    Assert.True(List.contains (MessageCode.InvalidEscapeSequence, 2) found, sprintf "%A" found)
    Assert.True(found |> List.exists (fun (_, l) -> l = 3), sprintf "no parse error on line 3: %A" found)

/// Sources in which a Validate and a Link diagnostic can fall on one line and column:
/// the assignment or increment rejected by validation starts where the Call that link
/// rejects starts.
let private sharedPositionSources =
    [ "Nope() = 1"
      "Nope()++"
      "func f(a)\n  return a\nend\nf() = 1"
      "func f(a)\n  return a\nend\nf(1, 2)--"
      "union S Circle(r) end\nCircle() = 1"
      "Output.a = 1\nNope() = Nope()" ]

let private stageOrder = [ "Deserialize"; "Parse"; "Validate"; "Link"; "Compile"; "Execute" ]

/// For every two diagnostics of `ms` at one line and column (130.5.2's position, not the
/// whole Location with its length), the earlier in the list is of a stage no later than
/// the other's. Every pair is checked, not only adjacent ones. Returns the number of such
/// pairs whose stages differ.
///
/// The cross-stage check is a guard: it binds only a build that reports diagnostics of
/// two stages for one source. 130.3.1 and 130.4.1 stop the later stages, the code table
/// gives no Warning or Info code, and continuing after an error (130.4.2) is a SHOULD,
/// so no source can be required to give such a pair, and 1488's rationale says so. The
/// tests do not assert that a pair exists; they order any pair a build gives.
let private assertStageOrderAtEachPosition (name: string) (ms: DiagnosticMessage list) : int =
    let placed = ms |> List.filter (fun m -> m.Location.HasValue) |> List.indexed
    let mutable crossStage = 0
    for i, a in placed do
        for j, b in placed do
            if i < j && a.Location.Value.Line = b.Location.Value.Line && a.Location.Value.Column = b.Location.Value.Column then
                let sa, sb = Pending.stageOf a, Pending.stageOf b
                let ia, ib = List.findIndex ((=) sa) stageOrder, List.findIndex ((=) sb) stageOrder
                if ia <> ib then crossStage <- crossStage + 1
                Assert.True(ia <= ib, sprintf "%s: %A (%s) comes before %A (%s) at %d:%d" name a.Code sa b.Code sb a.Location.Value.Line a.Location.Value.Column)
    crossStage

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1488")>]
let ``JYRO-REQ-1488 diagnostics at one line and column are in stage order`` () =
    for i, src in List.indexed sharedPositionSources do
        let r = Script.compileWith pinned src
        Assert.False(r.IsSuccess, sprintf "%A compiled" src)
        assertStageOrderAtEachPosition (sprintf "shared-%d" i) (List.ofSeq r.Messages) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1488")>]
let ``JYRO-REQ-1488 diagnostics at one line and column are in stage order across the corpus`` () =
    // wideCorpus holds the corpus of 130.2 and the further sources of 130.1.
    for o in wideCorpus () do
        assertStageOrderAtEachPosition o.Name o.Messages |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1488")>]
let ``JYRO-REQ-1488 every diagnostic in the corpus has one of the six stages`` () =
    for o in wideCorpus () do
        for m in o.Messages do
            let s = Pending.stageOf m
            Assert.True(List.contains s stageOrder, sprintf "%s: %A has the stage %s" o.Name m.Code s)

[<Theory>]
[<InlineData("fail \"The order total is negative.\"", "The order total is negative.")>]
[<InlineData("fail \"First sentence. Second sentence {0}.\"", "First sentence. Second sentence {0}.")>]
[<InlineData("fail {a: 1}", "{\"a\":1}")>]
[<InlineData("fail 1.5", "1.5")>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 the one argument of the ScriptFailure of fail is the text of its value`` (statement: string, text: string) =
    for place in [ "top level"; "function body" ] do
        let d = Script.runWith pinned (placed place statement) |> Script.onlyErrorMessage
        Assert.Equal(MessageCode.ScriptFailure, d.Code)
        Assert.Equal<string list>([ text ], d.Args |> Array.map argText |> List.ofArray)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 the exception message of a host function is not an argument`` () =
    for make in [ (fun () -> InvalidOperationException marker7f3a :> exn); (fun () -> ArgumentException marker7f3a :> exn); (fun () -> FormatException marker7f3a :> exn) ] do
        let h = Host.func "Throw" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (make ()))
        let r = Script.runWith { pinned with Functions = [ h.AsFunction ] } "Output.a = Throw()"
        Assert.False(r.IsSuccess)
        for d in r.Messages do
            for a in d.Args do
                let s = if isNull a then "" else argText a
                Assert.False(s.Contains "7f3a", sprintf "%A has the argument %A" d.Code s)
                Assert.False(isDotNetTypeName s, sprintf "%A has the .NET type name %A as an argument" d.Code s)

// -- 130.6.2 as revised (JYRO-ADR-124, JYRO-ADR-125): what an argument may be --------------

let private argStrings (d: DiagnosticMessage) : string list =
    d.Args |> Array.map (fun a -> if isNull a then "" else argText a) |> List.ofArray

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a host function's throw of JyroError.Host gives HostFunctionError whose arguments are the reason then the values`` () =
    // JYRO-ADR-124 item 5: the reason is a name the host chooses, the values what changes.
    let h = Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host("QuotaExceeded", [| box 3; box "orders" |])))
    let r = Script.runWith { pinned with Functions = [ h.AsFunction ] } "Output.a = 1\nRefuse()"
    let d = Script.onlyErrorMessage r
    Assert.Equal(MessageCode.HostFunctionError, d.Code)
    Assert.Equal<string list>([ "QuotaExceeded"; "3"; "orders" ], argStrings d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a host function's throw of JyroError.Host with a reason alone gives that one argument`` () =
    let h = Host.func "Refuse" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host("ResourceUnavailable")))
    let d = Script.runWith { pinned with Functions = [ h.AsFunction ] } "Refuse()" |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.HostFunctionError, d.Code)
    Assert.Equal<string list>([ "ResourceUnavailable" ], argStrings d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a host function that derives from JyroFunctionBase and throws JyroError.Host gives HostFunctionError whose arguments are the reason then the values`` () =
    let d = Script.runWith { pinned with Functions = [ RefusingHost() ] } "Output.a = Charge(-7)" |> Script.onlyErrorMessage
    Assert.Equal(MessageCode.HostFunctionError, d.Code)
    Assert.Equal<string list>([ "AmountNotPositive"; "-7" ], argStrings d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a RuntimeError from a host function's exception has the function's name with () as its one argument`` () =
    // The spec writer's answer (journal of JYRO-WRK-14, 2026-10-04 06:24 and 06:25): the
    // argument of RuntimeError is the name of the function whose call raised it, with (),
    // never a .NET exception type or message. The diagnostic is the one Error of the run: which
    // code reports a host function's exception is left to the implementation (JYRO-REQ-1681),
    // so the code is not asserted (JYRO-DEF-770).
    for name in [ "Throw"; "Explode" ] do
        let h = Host.func name [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (InvalidOperationException marker7f3a))
        let d = Script.runWith { pinned with Functions = [ h.AsFunction ] } (sprintf "Output.a = 1\nOutput.b = %s()" name) |> Script.onlyErrorMessage
        Assert.Equal<string list>([ name + "()" ], argStrings d)
        for a in argStrings d do
            Assert.DoesNotContain(marker7f3a, a)
            Assert.DoesNotContain("InvalidOperationException", a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 ArtifactUnreadable has no argument`` () =
    // Every cut of a valid artifact from 4 bytes up: those that end inside its structure are
    // ArtifactUnreadable (150.11.3), and each of those must have no argument.
    let valid = Script.compileBytesWith pinned "Output.x = 1\nOutput.y = \"text\"" |> Script.valueOf
    let unreadable =
        [ for n in 4 .. valid.Length - 1 do
            let r = Script.loadBytesWith pinned (valid.[0 .. n - 1])
            for m in r.Messages do
                if m.Code = MessageCode.ArtifactUnreadable then yield n, m ]
    Assert.True(not unreadable.IsEmpty, "no cut of the artifact gave ArtifactUnreadable")
    for n, d in unreadable do
        Assert.True(d.Args.Length = 0, sprintf "cut at %d: ArtifactUnreadable has the arguments %A" n (argStrings d))

/// A host function whose effect cannot be read, and one whose signature cannot be read:
/// faults met while a call is resolved, which the implementation reports as an internal
/// error (UnknownCompilerError or InternalStageFailure).
type private EffectFault1491(name: string) =
    inherit JyroFunctionBase(name, Host.signature name [] ParameterType.Any)
    override _.Effect = raise (InvalidOperationException marker7f3a)
    override _.Execute(_, _) = Values.nul

type private SignatureFault1491(name: string) =
    interface IJyroFunction with
        member _.Name = name
        member _.Signature = raise (InvalidOperationException marker7f3a)
        member _.Effect = FunctionEffect.Pure
        member _.Execute(_, _) = Values.nul

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 UnknownCompilerError and InternalStageFailure have no argument`` () =
    let src = "Output.x = Faulty()"
    let results =
        [ "effect", Script.compileWith { pinned with Functions = [ EffectFault1491 "Faulty" ] } src |> fun r -> List.ofSeq r.Messages
          "signature", Script.compileWith { pinned with Functions = [ SignatureFault1491 "Faulty" ] } src |> fun r -> List.ofSeq r.Messages ]
    let internals =
        [ for name, ms in results do
            for m in ms do
                if m.Code = MessageCode.UnknownCompilerError || m.Code = MessageCode.InternalStageFailure then yield name, m ]
    Assert.True(not internals.IsEmpty, sprintf "no internal error was given: %A" [ for n, ms in results -> n, ms |> List.map (fun m -> m.Code) ])
    for name, m in internals do
        Assert.True(m.Args.Length = 0, sprintf "%s: %A has the arguments %A" name m.Code (argStrings m))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a parse error at the end of the source has no argument for the token that does not exist`` () =
    // 130.1.2: when the source ends first there is no token, so no argument, and no word
    // or marker in its place.
    for src in [ "var x ="; "Output.x = (1 + "; "Output.x = (\"abc" ] do
        let errors = (Script.compileWith pinned src).Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        let last = List.last errors
        Assert.True(last.Args.Length = 0, sprintf "%A: the parse error at the end has the arguments %A" src (argStrings last))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a parse error at a token the scan blanked quotes the token as the script writes it`` () =
    // JYRO-DEF-533: after `var` the parser needs a name, so it stops at the string token
    // "a\q", whose escape is a lexical error the scan blanks; the syntax error's argument
    // is that token as written, not the repaired copy.
    let src = "var \"a\\q\" = 1"
    // The lexical error InvalidEscapeSequence of the same token is also given (1483); the
    // syntax error is the other error at that token.
    let r = Script.compileWith pinned src
    let syntax =
        r.Messages
        |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error && m.Code <> MessageCode.InvalidEscapeSequence)
        |> List.ofSeq
    Assert.False(syntax.IsEmpty, sprintf "no syntax error at the token. %s" (Script.describe r))
    let d = List.last syntax
    Assert.Contains("\"a\\q\"", argStrings d)
    for a in argStrings d do
        Assert.True(src.Contains a, sprintf "the argument %A is not text of the script" a)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 no argument of the NumberOverflow of Sum is a number with a noun`` () =
    for src in [ "Output.x = Sum([1e308, 1e308])"; "Output.x = Average([1e308, 1e308, 1e308])"; "Output.x = Median([1e308, 1e308])" ] do
        let ds = Script.runWith pinned src |> fun r -> r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.NumberOverflow) |> List.ofSeq
        if src.Contains "Sum" then Assert.False(ds.IsEmpty, "Sum([1e308, 1e308]) gave no NumberOverflow")
        for d in ds do
            for a in argStrings d do
                Assert.NotEqual<string>("2 elements", a)
                Assert.False(numberWithANoun.IsMatch a, sprintf "%s: the argument %A is a number with a noun" src a)

let private dateUnitWords =
    [ "millisecond"; "second"; "minute"; "hour"; "day"; "week"; "month"; "quarter"; "year" ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 DateUnitInvalid does not list the valid units, and what is the same in both is a name`` () =
    let diag unit = Script.runWith pinned (sprintf "Output.x = DateAdd(\"2024-01-01\", 1, \"%s\")" unit) |> Script.onlyErrorMessage
    let a, b = diag "eon", diag "fortnight"
    Assert.Equal(MessageCode.DateUnitInvalid, a.Code)
    Assert.Equal(MessageCode.DateUnitInvalid, b.Code)
    for d in [ a; b ] do
        for s in argStrings d do
            let named = dateUnitWords |> List.filter (fun w -> s.ToLowerInvariant().Contains w)
            Assert.True(named.Length <= 1, sprintf "the argument %A lists the units %A" s named)
    for s in Set.intersect (set (argStrings a)) (set (argStrings b)) do
        Assert.True(isNameKind s, sprintf "%A is in both DateUnitInvalid diagnostics and is not a name the specification gives" s)

/// The cases of the kinds test: each a name, the text the script and its input and its
/// host give (where a value of the script may be found as written), and the diagnostics.
type private ArgCase = { Name: string; Text: string; Messages: DiagnosticMessage list }

let private caseOf (name: string) (text: string) (r: JyroResult<'T>) : ArgCase =
    { Name = name; Text = text; Messages = List.ofSeq r.Messages }

let private scriptCase (name: string) (src: string) : ArgCase = caseOf name src (Script.runWith pinned src)

let private hostRaising (name: string) (reason: string) (values: obj[]) : IJyroFunction =
    (Host.func name [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Host(reason, values)))).AsFunction

let private hostThrowing (name: string) : IJyroFunction =
    (Host.func name [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (InvalidOperationException marker7f3a))).AsFunction

let private hostCase (name: string) (fn: IJyroFunction) (src: string) (given: string list) : ArgCase =
    caseOf name (String.Join("\n", src :: given)) (Script.runWith { pinned with Functions = [ fn ] } src)

/// The plaintext corpus, tests/plaintext/**.jyro, beside this test project's folder.
let private plaintextScripts () : (string * string) list =
    let root = IO.Path.GetFullPath(IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "plaintext"))
    if not (IO.Directory.Exists root) then Values.fail (sprintf "the plaintext corpus is not at %s" root)
    else
        [ for f in IO.Directory.EnumerateFiles(root, "*.jyro", IO.SearchOption.AllDirectories) |> Seq.sort ->
            IO.Path.GetRelativePath(root, f).Replace('\\', '/'), IO.File.ReadAllText f ]

/// How the diagnostic judged is found in a case: by a code the book gives the input, or, where the
/// book leaves the code to the implementation (JYRO-REQ-1681), as the one Error of a stage.
type private OccurrencePick =
    | ByCode of MessageCode
    | ErrorOfStage of string

let private pickFrom (pick: OccurrencePick) (c: ArgCase) : DiagnosticMessage option =
    match pick with
    | ByCode code -> c.Messages |> List.tryFind (fun m -> m.Code = code)
    | ErrorOfStage stage ->
        c.Messages
        |> List.filter (fun m -> m.Severity = MessageSeverity.Error && (try Pending.stageOf m = stage with _ -> false))
        |> List.tryHead

/// Pairs of cases that raise one code from different inputs (JYRO-DEF-535's process rule,
/// 1491's own criterion): every value differs between the two, so an argument the same in
/// both is either a name the specification gives or template text.
let private twoOccurrences () : (OccurrencePick * ArgCase * ArgCase) list =
    let s = scriptCase
    let code c = ByCode c
    [ code MessageCode.UndeclaredVariable, s "undeclared-a" "Output.a = foo", s "undeclared-b" "Output.b = barBaz"
      code MessageCode.UndefinedFunction, s "undefined-a" "Output.a = Nope()", s "undefined-b" "Output.b = Gone(1)"
      code MessageCode.TooFewArguments, s "few-a" "func f(a, b)\n  return a\nend\nOutput.x = f(1)", s "few-b" "func g(p, q, r)\n  return p\nend\nOutput.y = g()"
      code MessageCode.TooManyArguments, s "many-a" "func f(a)\n  return a\nend\nOutput.x = f(1, 2)", s "many-b" "func g()\n  return 3\nend\nOutput.y = g(4, 5, 6)"
      code MessageCode.ArgumentTypeMismatch, s "type-a" "Output.x = PadLeft({a: 1}, 4)", s "type-b" "Output.y = PadLeft([2], 3)"
      code MessageCode.InvalidEscapeSequence, s "escape-a" "Output.x = \"\\q\"", s "escape-b" "Output.y = \"ab\\z\""
      // JYRO-DEF-770: the code of a parse error is left to the implementation (JYRO-REQ-1681),
      // so the diagnostic is the Error of stage Parse, and the two are compared when they agree.
      ErrorOfStage "Parse", s "parse-a" "Output.x = )", s "parse-b" "Output.y = ]"
      code MessageCode.NumberOverflow, s "sum-a" "Output.x = Sum([1e308, 1e308])", s "sum-b" "Output.y = Sum([9e307, 9e307, 5e307])"
      code MessageCode.NumberOverflow, s "mul-a" "Output.x = 1e308 * 10", s "mul-b" "Output.y = 7e307 * 3e2"
      code MessageCode.NumberDomainError, s "power-a" "Output.x = Power(-8, 0.5)", s "power-b" "Output.y = Power(-2, 0.25)"
      code MessageCode.NumberDomainError, s "log-a" "Output.x = Log(0, 1)", s "log-b" "Output.y = Log(-3, 2)"
      code MessageCode.NumberDomainError, s "sqrt-a" "Output.x = SquareRoot(-1)", s "sqrt-b" "Output.y = SquareRoot(-4)"
      code MessageCode.DateUnitInvalid, s "unit-a" "Output.x = DateAdd(\"2024-01-01\", 1, \"eon\")", s "unit-b" "Output.y = DateAdd(\"2025-02-03\", 2, \"fortnight\")"
      code MessageCode.RegexInvalidPattern, s "regex-a" "Output.x = RegexTest(\"a\", \"(\")", s "regex-b" "Output.y = RegexTest(\"b\", \"[\")"
      code MessageCode.InvalidRoundingMode, s "round-a" "Output.x = Round(1.5, 0, \"sideways\")", s "round-b" "Output.y = Round(2.5, 1, \"upward\")"
      code MessageCode.InvalidSortDirection, s "sort-a" "Output.x = SortByField([{a: 2}], \"a\", \"decs\")", s "sort-b" "Output.y = SortByField([{b: 1}], \"b\", \"up\")"
      // JYRO-DEF-550: raised only for the field _variant, so _variant is template text.
      code MessageCode.ReservedFieldName, s "reserved-a" "union U A(_variant) end", s "reserved-b" "union V B(x, _variant) end"
      code MessageCode.HostFunctionError,
        hostCase "host-a" (hostRaising "Refuse" "QuotaExceeded" [| box 3 |]) "Output.x = Refuse()" [ "QuotaExceeded"; "3" ],
        hostCase "host-b" (hostRaising "Deny" "AmountNotPositive" [| box -2 |]) "Output.y = Deny()" [ "AmountNotPositive"; "-2" ]
      ErrorOfStage "Execute",
        hostCase "throw-a" (hostThrowing "Throw") "Output.x = Throw()" [],
        hostCase "throw-b" (hostThrowing "Explode") "Output.y = Explode()" [] ]

/// Every case of the kinds test: the corpus and the further sources of Part 130, the
/// pairs above, host cases and the plaintext corpus.
let private argumentCases () : ArgCase list =
    let thrower = hostThrowing "Throw"
    let limited f = { pinned with Limits = Some(Script.limits (fun o -> f (allLimits o))) }
    [ for name, src in corpusSources -> scriptCase name src ]
    @ [ for i, src in List.indexed wideSources -> scriptCase (sprintf "wide-%d" i) src ]
    @ [ caseOf "effect" "Output.t = Now()" (Script.compileWith { pinned with MaxEffect = Some FunctionEffect.Pure } "Output.t = Now()")
        caseOf "limit" "Output.a = 1\nOutput.b = 2" (Script.runWith (limited (fun o -> { o with MaxStatements = 1 })) "Output.a = 1\nOutput.b = 2")
        caseOf "loop-limit" "var i = 0\nwhile i < 5 do i = i + 1 end" (Script.runWith (limited (fun o -> { o with MaxLoopIterations = 1 })) "var i = 0\nwhile i < 5 do i = i + 1 end")
        caseOf "depth-limit" (recursion 10) (Script.runWith (limited (fun o -> { o with MaxCallDepth = 2 })) (recursion 10))
        // The host's bytes are its input; their text is where a value of them is found.
        caseOf "artifact-magic" (String([| '\001'; '\002'; '\003' |])) (Script.loadBytesWith pinned ([| 1uy; 2uy; 3uy |]))
        caseOf "library-forbidden" "var x = 1\nfunc L()\n  return 1\nend\nOutput.a = L()" (Script.compileWith { pinned with Libraries = [ "var x = 1\nfunc L()\n  return 1\nend" ] } "Output.a = L()")
        caseOf "host-collision" "Length\nOutput.a = 1" (Script.compileWith { pinned with Functions = [ (Host.recorder "Length").AsFunction ] } "Output.a = 1")
        caseOf "host-throw" "Throw\nOutput.a = Throw()" (Script.runWith { pinned with Functions = [ thrower ] } "Output.a = Throw()")
        caseOf "bad-input" "{bad\nOutput.a = 1" (Script.runWith { pinned with Input = JsonInput "{bad" } "Output.a = 1")
        caseOf "no-source" "" ((JyroCompiler()).WithMaxCompilationTime(generousCompile).Compile()) ]
    @ [ for _, a, b in twoOccurrences () do yield a; yield b ]
    @ [ for name, src in plaintextScripts () -> scriptCase ("plaintext " + name) src ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 every argument is a value of the script or a name the specification gives`` () =
    // 1491's table of kinds: a token, literal or number as the script or its input gives
    // it, or a name the specification gives; never an implementation-chosen word or phrase,
    // an article with a noun, a number with a noun, a .NET type name or several lines.
    // The ScriptFailure of fail is the one exception.
    let faults =
        [ for c in argumentCases () do
            for d in c.Messages do
                if d.Code <> MessageCode.ScriptFailure then
                    for a in argStrings d do
                        match argumentFault c.Text a with
                        | Some why -> yield sprintf "%s: %A %A (%s)" c.Name d.Code a why
                        | None -> () ]
    Assert.True(faults.IsEmpty, sprintf "%d arguments are not of a kind 1491 allows:\n%s" faults.Length (String.Join("\n", List.distinct faults)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 a code raised from two different inputs has no argument the same in both that is not a name`` () =
    // 1491's own criterion and JYRO-DEF-535's process rule: an argument that is the same
    // in every occurrence of its code is template text.
    let faults =
        [ for pick, a, b in twoOccurrences () do
            match pickFrom pick a, pickFrom pick b with
            | Some da, Some db when (match pick with ErrorOfStage _ -> da.Code <> db.Code | ByCode _ -> false) ->
                // A code the book leaves to the implementation: the two are compared only when
                // they give the same code (JYRO-DEF-770); otherwise no clause compares them.
                ()
            | Some da, Some db ->
                let code = da.Code
                let xa, xb = argStrings da, argStrings db
                for i in 0 .. (min xa.Length xb.Length) - 1 do
                    if xa.[i] = xb.[i] && not (isNameKind xa.[i]) then
                        yield sprintf "%A: argument %d is %A in both %s and %s" code i xa.[i] a.Name b.Name
            | _ ->
                yield sprintf "%A was not given by both %s (%A) and %s (%A)" pick a.Name (a.Messages |> List.map (fun m -> m.Code)) b.Name (b.Messages |> List.map (fun m -> m.Code)) ]
    Assert.True(faults.IsEmpty, String.Join("\n", faults))

// -- 130.6.2 at the sites a host function reaches (JYRO-ADR-128 item 6) ------------------------
//
// JyroNumber.ToInt32(), ToInt64() and DecimalValue, JyroFunctionBase.GetDecimal and the
// JSON writer behind ValidateSchema, JyroValue.ToJson() and WriteJson are reached from a script
// only through a host function (JYRO-DEF-546, JYRO-DEF-547). Each case is judged by the kinds
// table of 1491 and by the message rendering the values in their places: what is always present
// first, a function and parameter after it and only where one exists (JYRO-ADR-128 items 1 to 5).

let private loneHigh = string (char 0xD83D)

/// A host function `name(n)` that hands its number to `use`, as a host converting it would.
let private hostUsingNumber (name: string) (useIt: JyroNumber -> unit) : IJyroFunction =
    (Host.func name [ Parameter.Required("n", ParameterType.Number) ] ParameterType.Any FunctionEffect.Pure
        (fun args -> useIt (List.head args :?> JyroNumber); Values.nul)).AsFunction

/// A host function `Convert(amount)` that reads its argument through GetDecimal.
type private DecimalReading1491() =
    inherit JyroFunctionBase("Convert", Host.signature "Convert" [ Parameter.Required("amount", ParameterType.Number) ] ParameterType.Number)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(args, _) = JyroNumber(args.GetDecimal(0)) :> JyroValue

/// A host function `name()` that returns a string holding an unpaired surrogate.
let private hostLoneString (name: string) : IJyroFunction =
    (Host.func name [] ParameterType.String FunctionEffect.Pure (fun _ -> Values.str ("ab" + loneHigh))).AsFunction

/// A host function `name()` that serializes such a string itself, through a host member.
let private hostWritingJson (name: string) (write: JyroValue -> unit) : IJyroFunction =
    (Host.func name [] ParameterType.Any FunctionEffect.Pure
        (fun _ -> write (JyroString("ab" + loneHigh)); Values.nul)).AsFunction

let private writeThroughWriter (v: JyroValue) : unit =
    use stream = new IO.MemoryStream()
    use writer = new Utf8JsonWriter(stream)
    v.WriteJson(writer)
    writer.Flush()

/// The one diagnostic of `code` a run with the host function `fn` gives.
let private hostSite (fn: IJyroFunction) (src: string) (code: MessageCode) : DiagnosticMessage =
    let r = Script.runWith { pinned with Functions = [ fn ] } src
    match r.Messages |> Seq.filter (fun m -> m.Code = code) |> List.ofSeq with
    | [ d ] -> d
    | ds -> Values.fail (sprintf "%s: expected one %A, got %d. %s" src code ds.Length (Script.describe r))

let private sameNumber (expected: float) (text: string) : bool =
    let ok, v = Double.TryParse(text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture)
    ok && (v = expected || abs (v - expected) <= abs expected * 1e-15)

/// 1491's kinds table over every argument, and the default message: every argument in it, in
/// the order of the arguments (ADR-128 item 2: the template reads each value in its place), and
/// no placeholder left; a host template of positional placeholders gives the arguments in order.
let private judgeSite (text: string) (d: DiagnosticMessage) : unit =
    let args = argStrings d
    for a in args do
        match argumentFault text a with
        | Some why -> Values.fail (sprintf "%A: the argument %A is %s" d.Code a why)
        | None -> ()
    Assert.False(placeholder.IsMatch d.Message, sprintf "%A leaves a placeholder: %s" d.Code d.Message)
    let mutable from = 0
    for a in args do
        let i = d.Message.IndexOf(a, from, StringComparison.Ordinal)
        Assert.True(i >= 0, sprintf "%A: the message %A does not give the argument %A after position %d (arguments %A)" d.Code d.Message a from args)
        from <- i + a.Length
    let positional = String.Join("|", [ for i in 0 .. args.Length - 1 -> sprintf "{%d}" i ])
    Assert.Equal(String.Join("|", args), localizedMessage (FixedProvider(positional)) d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 IntegerOutOfRange of ToInt32 of 3e9 in a host function is the value, the least and the greatest`` () =
    let src = "Output.a = 1\nNarrow(3e9)"
    let d = hostSite (hostUsingNumber "Narrow" (fun n -> n.ToInt32() |> ignore)) src MessageCode.IntegerOutOfRange
    let args = argStrings d
    Assert.True(args.Length = 3, sprintf "ToInt32 names no function or parameter (ADR-128 item 3), so three arguments; got %A" args)
    Assert.True(sameNumber 3e9 args.[0], sprintf "the first argument is the value 3e9: %A" args)
    Assert.True(sameNumber (float Int32.MinValue) args.[1], sprintf "the second is the least int: %A" args)
    Assert.True(sameNumber (float Int32.MaxValue) args.[2], sprintf "the third is the greatest int: %A" args)
    judgeSite (src + "\nNarrow\nn") d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 IntegerOutOfRange of ToInt64 of 1e19 in a host function is the value, the least and the greatest`` () =
    let src = "Output.a = 1\nNarrow(1e19)"
    let d = hostSite (hostUsingNumber "Narrow" (fun n -> n.ToInt64() |> ignore)) src MessageCode.IntegerOutOfRange
    let args = argStrings d
    Assert.True(args.Length = 3, sprintf "ToInt64 names no function or parameter, so three arguments; got %A" args)
    Assert.True(sameNumber 1e19 args.[0], sprintf "the first argument is the value 1e19: %A" args)
    Assert.True(sameNumber (float Int64.MinValue) args.[1], sprintf "the second is the least int64: %A" args)
    Assert.True(sameNumber (float Int64.MaxValue) args.[2], sprintf "the third is the greatest int64: %A" args)
    judgeSite (src + "\nNarrow\nn") d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 IntegerOutOfRange of DecimalValue of 1e30 in a host function is the value, the least and the greatest`` () =
    let src = "Output.a = 1\nWiden(1e30)"
    let d = hostSite (hostUsingNumber "Widen" (fun n -> n.DecimalValue |> ignore)) src MessageCode.IntegerOutOfRange
    let args = argStrings d
    Assert.True(args.Length = 3, sprintf "DecimalValue names no function or parameter, so three arguments; got %A" args)
    Assert.True(sameNumber 1e30 args.[0], sprintf "the first argument is the value 1e30: %A" args)
    Assert.True(sameNumber (float Decimal.MinValue) args.[1], sprintf "the second is the least decimal: %A" args)
    Assert.True(sameNumber (float Decimal.MaxValue) args.[2], sprintf "the third is the greatest decimal: %A" args)
    for a in args do
        Assert.False(a.Contains "DecimalValue" || a.Contains "ToInt", sprintf "a .NET member name is an argument: %A" args)
    judgeSite (src + "\nWiden\nn") d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 IntegerOutOfRange of GetDecimal of 1e30 names the host function and its parameter last`` () =
    let src = "Output.a = 1\nOutput.b = Convert(1e30)"
    let d = hostSite (DecimalReading1491()) src MessageCode.IntegerOutOfRange
    let args = argStrings d
    Assert.True(args.Length = 5, sprintf "value, least, greatest, function, parameter (ADR-128 items 2 and 4); got %A" args)
    Assert.True(sameNumber 1e30 args.[0], sprintf "the first argument is the value 1e30: %A" args)
    Assert.True(sameNumber (float Decimal.MinValue) args.[1], sprintf "the second is the least decimal: %A" args)
    Assert.True(sameNumber (float Decimal.MaxValue) args.[2], sprintf "the third is the greatest decimal: %A" args)
    Assert.Equal("Convert()", args.[3])
    Assert.Equal("amount", args.[4])
    Assert.DoesNotContain("GetDecimal", args)
    Assert.DoesNotContain("value", args)
    judgeSite (src + "\nConvert\namount") d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 UnpairedSurrogate of ValidateSchema of a host string is the index and ValidateSchema()`` () =
    let src = "Output.a = 1\nOutput.r = ValidateSchema(Lone(), {})"
    let d = hostSite (hostLoneString "Lone") src MessageCode.UnpairedSurrogate
    let args = argStrings d
    Assert.True(args.Length = 2, sprintf "index, function (ADR-128 items 2 and 5); got %A" args)
    Assert.True(isNumberText args.[0], sprintf "the first argument is the index: %A" args)
    Assert.Equal("ValidateSchema()", args.[1])
    Assert.DoesNotContain("ToJson()", args)
    judgeSite (src + "\nLone") d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 UnpairedSurrogate of ValidateSchema of a key the script makes is the index and ValidateSchema()`` () =
    let src = "Output.a = 1\nvar o = {}\no['k' & Lone()] = 1\nOutput.r = ValidateSchema(o, {})"
    let d = hostSite (hostLoneString "Lone") src MessageCode.UnpairedSurrogate
    let args = argStrings d
    Assert.True(args.Length = 2, sprintf "index, function; got %A" args)
    Assert.True(isNumberText args.[0], sprintf "the first argument is the index: %A" args)
    Assert.Equal("ValidateSchema()", args.[1])
    judgeSite (src + "\nLone") d

let hostJsonMembers : obj[] seq = [ [| box "WriteJson" |]; [| box "ToJson" |] ]

[<Theory>]
[<MemberData(nameof hostJsonMembers)>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 UnpairedSurrogate of a host member called in a host function is the index alone`` (memberName: string) =
    // ADR-128 item 5: a host member called with no script function gives no function, and
    // no name of its own (not ToJson(), nor the host function, which the member cannot know).
    let write =
        match memberName with
        | "WriteJson" -> writeThroughWriter
        | _ -> fun (v: JyroValue) -> v.ToJson() |> ignore
    let src = "Output.a = 1\nEmit()"
    let d = hostSite (hostWritingJson "Emit" write) src MessageCode.UnpairedSurrogate
    let args = argStrings d
    Assert.True(args.Length = 1, sprintf "%s: the index alone; got %A" memberName args)
    Assert.True(isNumberText args.[0], sprintf "%s: the argument is the index: %A" memberName args)
    judgeSite src d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 UnpairedSurrogate of ToJson of a host string still names ToJson()`` () =
    // The library's own caller of the shared writer keeps its name, now last.
    let src = "Output.a = 1\nOutput.r = ToJson(Lone())"
    let d = hostSite (hostLoneString "Lone") src MessageCode.UnpairedSurrogate
    let args = argStrings d
    Assert.True(args.Length = 2, sprintf "index, function; got %A" args)
    Assert.True(isNumberText args.[0], sprintf "the first argument is the index: %A" args)
    Assert.Equal("ToJson()", args.[1])
    judgeSite (src + "\nLone") d

// -- JYRO-REQ-1674 (130.3): a parse error at the end of the source is UnexpectedEndOfSource --------

/// The diagnostic has the code UnexpectedEndOfSource, 2001 (160.7.12, JYRO-REQ-2138). Read by
/// number and name, so the tests compile before the member exists.
let private isEndOfSource (d: DiagnosticMessage) : bool =
    int d.Code = 2001 && Enum.GetName(typeof<MessageCode>, d.Code) = "UnexpectedEndOfSource"

let private compileErrors (src: string) : DiagnosticMessage list =
    (Script.compileWith pinned src).Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq

/// The line and column of the empty range just after the last code unit of `src` (150.4.4:
/// a CRLF is one line end).
let private endOf (src: string) : int * int =
    let mutable line, col, i = 1, 1, 0
    while i < src.Length do
        if src.[i] = '\r' && i + 1 < src.Length && src.[i + 1] = '\n' then line <- line + 1; col <- 1; i <- i + 2
        elif src.[i] = '\n' then line <- line + 1; col <- 1; i <- i + 1
        else col <- col + 1; i <- i + 1
    line, col

/// `src` gives a parse error of code UnexpectedEndOfSource at the empty range after its last
/// code unit, with an empty Args.
let private assertEndOfSource (src: string) : unit =
    let errors = compileErrors src
    let eos = errors |> List.filter isEndOfSource
    Assert.True(not eos.IsEmpty, sprintf "%A: no UnexpectedEndOfSource; the errors are %A" src [ for e in errors -> e.Code, argStrings e ])
    let line, column = endOf src
    for d in eos do
        at line column 0 d
        Assert.NotNull(d.Args)
        Assert.True(d.Args.Length = 0, sprintf "%A: UnexpectedEndOfSource has the arguments %A" src (argStrings d))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 var x = gives UnexpectedEndOfSource at 1 8 0 with no argument`` () =
    assertEndOfSource "var x ="
    let d = compileErrors "var x =" |> List.find isEndOfSource
    at 1 8 0 d

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 if true then gives UnexpectedEndOfSource`` () =
    assertEndOfSource "if true then"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 var x = ) gives no UnexpectedEndOfSource`` () =
    let errors = compileErrors "var x = )"
    Assert.False(errors.IsEmpty)
    Assert.False(errors |> List.exists isEndOfSource, sprintf "%A" [ for e in errors -> e.Code ])

[<Theory>]
[<InlineData("Output.a = foo(1,")>]
[<InlineData("var a = [1,")>]
[<InlineData("var o = {\"k\":")>]
[<InlineData("while true do")>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 a source that ends before the parser can read further gives UnexpectedEndOfSource`` (src: string) =
    assertEndOfSource src

[<Theory>]
[<InlineData("var x = )")>]
[<InlineData("var = 1")>]
[<InlineData("if then end")>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 a parse error at a token is not UnexpectedEndOfSource`` (src: string) =
    let errors = compileErrors src
    Assert.False(errors.IsEmpty, sprintf "%A compiled" src)
    Assert.False(errors |> List.exists isEndOfSource, sprintf "%A: %A" src [ for e in errors -> e.Code ])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 an unterminated string keeps its own code`` () =
    let errors = compileErrors "var s = \"abc"
    Assert.Contains(MessageCode.UnterminatedString, errors |> List.map (fun e -> e.Code))
    let s = errors |> List.find (fun e -> e.Code = MessageCode.UnterminatedString)
    Assert.False(isEndOfSource s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1674")>]
let ``JYRO-REQ-1674 every UnexpectedEndOfSource of a prefix of the plaintext corpus is at its end with no argument`` () =
    // The proper prefixes of every plaintext script that end where a token can end: just
    // before a space, tab or line end, or before a bracket, comma or colon. (Every prefix
    // took five minutes a run; these keep each place a construct is left open.) A prefix
    // ending inside a CRLF is skipped, since a lone CR is not a line end the book defines.
    let cut (c: char) = Char.IsWhiteSpace c || "()[]{},:".IndexOf c >= 0
    let faults =
        [ for name, text in plaintextScripts () do
            for n in 1 .. text.Length - 1 do
                if cut text.[n] && not (text.[n - 1] = '\r' && text.[n] = '\n') && not (Char.IsWhiteSpace text.[n - 1]) then
                    let prefix = text.Substring(0, n)
                    let line, column = endOf prefix
                    for d in (Script.compileWith pinned prefix).Messages do
                        if isEndOfSource d then
                            let l = d.Location
                            let placed = l.HasValue && (l.Value.Line, l.Value.Column, l.Value.Length) = (line, column, 0)
                            if not placed || d.Args.Length <> 0 then
                                yield sprintf "%s, first %d code units: at %A, arguments %A" name n (if l.HasValue then Some(l.Value.Line, l.Value.Column, l.Value.Length) else None) (argStrings d) ]
    Assert.True(faults.IsEmpty, sprintf "%d prefixes:\n%s" faults.Length (String.Join("\n", faults |> List.truncate 40)))

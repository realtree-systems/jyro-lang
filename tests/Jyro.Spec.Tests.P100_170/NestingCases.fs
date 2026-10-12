/// 100.10: the source texts and the child scenarios of the nesting bound. A source text that is
/// deep enough to overflow a stack must not be able to end the test host, so every compilation or
/// run that could stop the process runs in a child process (Child.fs, "nest|..."), on a thread of
/// a fixed small stack: a crash then fails the test and not the test run (JYRO-DEF-499, JYRO-ADR-177).
///
/// The sources are built from the clauses 100.10.1 and 100.10.2 alone: a top-level statement has
/// depth 1, a parenthesised expression has the depth of what holds it plus one and the expression
/// inside it is one deeper again, an operand, an argument, an element, a property value and a
/// statement of a body is one deeper than what holds it.
module RealTree.Jyro.Spec.Tests.NestingCases

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Text
open System.Threading
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// The maximum nesting depth of 100.10.1.
let maximum : int = 256

/// `n` copies of `s`.
let rep (n: int) (s: string) : string =
    let sb = StringBuilder(max 16 (n * s.Length))
    for _ in 1 .. n do sb.Append s |> ignore
    sb.ToString()

// -- Shapes: one source for each construct of 100.10.2 that a source can nest -------------------

/// A source text of one shape at `k` levels, as a script and as a library script (160.6).
type Shape =
    { Name: string
      Script: int -> string
      Library: int -> string
      /// False when the source is not well formed, or may not be (`--` may be one token): any
      /// diagnostic is then accepted, and not only the one of 100.10.4.
      WellFormed: bool
      /// The depth of the script at `k` levels is `k + ScriptBase`.
      ScriptBase: int
      /// The depth of the library script at `k` levels is `k + LibraryBase`.
      LibraryBase: int }

let private libraryReturning (expr: string) : string = "func F()\n  return " + expr + "\nend"

/// `opener` k times, `inner`, `closer` k times, as the value of `Output.x`.
let private nested (name: string) (opener: string) (inner: string) (closer: string) (wellFormed: bool) : Shape =
    let expr k = rep k opener + inner + rep k closer
    { Name = name
      Script = fun k -> "Output.x = " + expr k
      Library = fun k -> libraryReturning (expr k)
      WellFormed = wellFormed
      ScriptBase = 2
      LibraryBase = 3 }

/// The seven source texts of JYRO-REQ-2133, in the order of its clause.
let shapes : Shape list =
    [ nested "parens" "(" "1" ")" true
      nested "parens-open" "(" "1" "" false
      { Name = "operators"
        Script = fun k -> "Output.s = 1" + rep k " + 1"
        Library = fun k -> libraryReturning ("1" + rep k " + 1")
        WellFormed = true
        ScriptBase = 2
        LibraryBase = 3 }
      nested "minus" "-" "1" "" false
      nested "brackets" "[" "1" "]" true
      nested "calls" "Length(" "\"a\"" ")" true
      { Name = "ifs"
        Script = fun k -> rep k "if true then " + "Output.x = 1" + rep k " end"
        Library = fun k -> "func F()\n  " + rep k "if true then " + "return 1" + rep k " end" + "\nend"
        WellFormed = true
        ScriptBase = 3
        LibraryBase = 3 } ]

let shapeNamed (name: string) : Shape = shapes |> List.find (fun s -> s.Name = name)

/// The number of levels `k` that gives the script of `shape` the nesting depth `depth`.
let scriptLevels (shape: Shape) (depth: int) : int = depth - shape.ScriptBase

/// The number of levels `k` that gives the library script of `shape` the nesting depth `depth`.
let libraryLevels (shape: Shape) (depth: int) : int = depth - shape.LibraryBase

/// The source of JYRO-DEF-499: 10,000 statements `var a0 = 0` to `var a9999 = 9999`, then the
/// sum of the 10,000 variables on one last line.
let def499Source : string =
    let declarations = [ for i in 0 .. 9999 -> sprintf "var a%d = %d" i i ]
    let sum = String.Join(" + ", [ for i in 0 .. 9999 -> sprintf "a%d" i ])
    String.Join("\n", declarations @ [ "Output.s = " + sum ])

// -- Programs that run: a function whose body is brought to a depth, and calls itself -------------

/// `func F(n)` whose body is `m` nested `if true then` around `inner`, then `Output.r = F(0)`.
let private functionWith (m: int) (inner: string) : string =
    "func F(n)\n  " + rep m "if true then " + inner + rep m " end" + "\nend\nOutput.r = F(0)"

/// A function of body depth `depth` (at least 4) that calls itself, with no end: FuncDef 1, the
/// `m` ifs 2 to m + 1, the Return m + 2, the Call m + 3, the Identifier `n` m + 4.
let selfCallSource (depth: int) : string = functionWith (depth - 4) "return F(n)"

/// The same, bringing the body to `depth` (at least 6) with a ternary, calling itself while `n`
/// is below `limit`, and dividing by zero at the deepest point. The deepest nodes are the operands
/// of `n + 1` and of `1 / 0`: the Return m + 2, the conditional m + 3, its branches m + 4, their
/// operands m + 5, and the operands of `n + 1` inside the call m + 6.
let divZeroSource (depth: int) (limit: int) : string =
    functionWith (depth - 6) (sprintf "return n < %d ? F(n + 1) : 1 / 0" limit)

/// A function of body depth `depth` (at least 6) that calls itself while `n` is below `limit`,
/// then runs `action` at the deepest point: the ifs 2 to m + 1, the inner `if` m + 2, the Return
/// m + 3, the Call m + 4, the argument `n + 1` m + 5 and its operands m + 6.
let actionSource (depth: int) (limit: int) (action: string) : string =
    functionWith (depth - 6) (sprintf "if n < %d then\n    return F(n + 1)\n  end\n  %s" limit action)

/// The statement that ends a run at the deepest point, by the name of the way it ends.
let actionOf (name: string) : string =
    match name with
    | "fail" -> "fail \"boom\""
    | "exit" -> "exit \"done\""
    // A function cannot read or write Output (3504), so the loop counts in the parameter.
    | "statements" | "loops" -> "while true do n = n + 1 end"
    | "cancel" -> "CancelNow(0)\n  while true do n = n + 1 end"
    | other -> failwithf "unknown action %s" other

/// The actions of the clause, in the order of its list of ways a run ends.
let actionNames : string list = [ "fail"; "exit"; "statements"; "loops"; "cancel" ]

// -- In the child -------------------------------------------------------------------------------

/// Run `f` on a new thread of `kb` KB of stack and give what it returns. A stack overflow of the
/// thread ends the process, which is what the test in the parent looks for.
let private onThread (kb: int) (f: unit -> 'a) : 'a =
    let mutable result = Unchecked.defaultof<'a>
    let mutable error : exn = null
    let t =
        Thread(
            (fun () ->
                try result <- f ()
                with e -> error <- e),
            kb * 1024)
    t.Start()
    t.Join()
    if not (isNull error) then ExceptionDispatchInfo.Capture(error).Throw()
    result

/// The line of a compile's result: the codes, the number of diagnostics, the stages, and the
/// stage, place and arguments of the first error.
let private describeCompile (r: JyroResult<'T>) : string =
    let errors = [ for m in r.Messages do if m.Severity = MessageSeverity.Error then yield m ]
    let codes = errors |> List.map (fun m -> string m.Code)
    let stages = r.Messages |> Seq.map Pending.stageOf |> Seq.distinct |> List.ofSeq
    let first = List.tryHead errors
    let stage = match first with Some m -> Pending.stageOf m | None -> "none"
    let line, column, length =
        match first with
        | Some m when m.Location.HasValue -> m.Location.Value.Line, m.Location.Value.Column, m.Location.Value.Length
        | _ -> 0, 0, 0
    let args =
        match first with
        | Some m -> String.Join("|", m.Args |> Array.map (fun a -> if isNull a then "<null>" else argText a))
        | None -> ""
    sprintf "ok=%b codes=%s msgs=%d stage=%s stages=%s line=%d col=%d len=%d args=%s"
        r.IsSuccess (String.Join(",", codes)) (Seq.length r.Messages) stage (String.Join("/", stages)) line column length args

let private compileKind (kind: string) (source: string) : JyroResult<CompiledProgram> =
    let c = JyroCompiler().WithStdlib().WithMaxCompilationTime(generousCompile)
    if kind = "library" then c.AddLibrarySource(source).WithSource("Output.x = F()").Compile()
    else c.WithSource(source).Compile()

/// "nest|compile|<shape>|<k>|<script or library>|<kb>": compile the source of a shape at k levels
/// on a thread of kb KB.
let private compileScenario (shape: string) (k: int) (kind: string) (kb: int) : string =
    let s = shapeNamed shape
    let source = if kind = "library" then s.Library k else s.Script k
    describeCompile (onThread kb (fun () -> compileKind kind source))

/// "nest|def499|<kb>": compile the source of JYRO-DEF-499 on a thread of kb KB.
let private def499Scenario (kb: int) : string =
    describeCompile (onThread kb (fun () -> compileKind "script" def499Source))

/// The sources of JYRO-REQ-2132 (JYRO-ADR-178) whose place is decided by the reading count, with
/// the line, column and length that the clause gives them.
///
/// "chain-then-parens": the book's example, `Output.s = 1 + ` then 100,000 `(`, `1`, 100,000 `)`
/// and ` + 1` 300 times. The first `(` is an operand of the first `+`, so it has reading count 2,
/// and the 256th `(` has 257, at column 15 + 255 = 270 + 1 = 271 (the `(` run starts at column 16).
/// The first `1`, at column 12, is above the maximum only by depth (303).
///
/// "else-ternaries": `Output.x = ` and 100,000 times `true ? 1 : `, then `1`. The ternary k
/// (read at reading count k + 1) starts at column 12 + 11 (k - 1); the 255th has reading count
/// 256, and its then-branch `1`, at 7 columns in, has 257. Its condition `true` is above the
/// maximum by depth (257) but has the reading count of the ternary, 256.
let placeCases : (string * string * (int * int * int)) list =
    [ "chain-then-parens",
      "Output.s = 1 + " + rep 100_000 "(" + "1" + rep 100_000 ")" + rep 300 " + 1",
      (1, 271, 1)
      "else-ternaries",
      "Output.x = " + rep 100_000 "true ? 1 : " + "1",
      (1, 12 + 11 * 254 + 7, 1) ]

/// What a run needs: the source, the limits, and whether the host cancels in it.
type RunSpec =
    { Source: string
      Limits: JyroExecutionOptions
      Cancels: bool }

let private baseLimits () : JyroExecutionOptions =
    { JyroExecutionOptions.Default with
        MaxCallDepth = maxSafeCallDepth ()
        MaxStatements = 100_000_000
        MaxLoopIterations = 100_000_000
        MaxExecutionTime = TimeSpan.FromSeconds 100. }

/// The depth of the recursion of the actions: far down, and below the call depth limit.
let actionLimit () : int = maxSafeCallDepth () - 4

let runSpecOf (name: string) : RunSpec =
    let plain source = { Source = source; Limits = baseLimits (); Cancels = false }
    match name.Split(':') with
    | [| "operators254" |] -> plain ("Output.s = 1" + rep 254 " + 1")
    | [| "selfcall"; depth |] -> plain (selfCallSource (int depth))
    | [| "action"; action |] ->
        let source = actionSource maximum (actionLimit ()) (actionOf action)
        match action with
        | "statements" ->
            // The limit falls inside the loop at the deepest point, or earlier in the recursion.
            let perCall = (maximum - 6) + 4
            { Source = source
              Limits = { baseLimits () with MaxStatements = (actionLimit () + 1) * perCall + 5000 }
              Cancels = false }
        | "loops" -> { Source = source; Limits = { baseLimits () with MaxLoopIterations = 1000 }; Cancels = false }
        | "cancel" -> { Source = source; Limits = baseLimits (); Cancels = true }
        | _ -> plain source
    | _ -> failwithf "unknown run %s" name

let private describeRun (r: JyroResult<JyroValue>) : string =
    let codes = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> Seq.map (fun m -> string m.Code)
    let outputs =
        match r.TryGetValue() with
        | true, o when r.IsSuccess ->
            [ for key in [ "x"; "s"; "r" ] do
                if hasProp key o then yield sprintf " %s=%s" key (Values.toJson (Values.prop key o)) ]
        | _ -> []
    sprintf "ok=%b codes=%s%s" r.IsSuccess (String.Join(",", codes)) (String.Join("", outputs))

/// Compile `spec` (from the source, or from an artifact written from it), and run it, all on a
/// thread of kb KB.
let private runSpec (kb: int) (via: string) (spec: RunSpec) : JyroResult<JyroValue> =
    use cts = new CancellationTokenSource()
    onThread kb (fun () ->
        let make () =
            let c = JyroCompiler().WithStdlib().WithMaxCompilationTime(generousCompile)
            if spec.Cancels then c.AddFunction((canceller cts).AsFunction) else c
        let compiled =
            if via = "bytes" then
                let written = (make ()).WithSource(spec.Source).CompileToArtifact()
                match written.TryGetValue() with
                | true, bytes when written.IsSuccess ->
                    let l = JyroArtifactLoader(bytes).WithStdlib().WithMaxCompilationTime(generousCompile)
                    (if spec.Cancels then l.AddFunction((canceller cts).AsFunction) else l).Load()
                | _ -> JyroResult<CompiledProgram>.Failure(written.Messages)
            else (make ()).WithSource(spec.Source).Compile()
        match compiled.TryGetValue() with
        | true, program when compiled.IsSuccess ->
            let runner = JyroRunner(program).WithInputJson("{}").WithExecutionOptions(spec.Limits)
            (if spec.Cancels then runner.WithCancellationToken(cts.Token) else runner).Execute()
        | _ -> JyroResult<JyroValue>.Failure(compiled.Messages))

/// "nest|run|<name>|<source or bytes>|<kb>".
let private runScenario' (name: string) (via: string) (kb: int) : string =
    describeRun (runSpec kb via (runSpecOf name))

/// "nest|divzero|<depth>": the greatest limit `L` for which the function of `divZeroSource depth L`
/// ends with DivisionByZero at the deepest call, found by bisection on a thread of 1 MB.
let private divZeroScenario (depth: int) : string =
    let d = maxSafeCallDepth ()
    let codesOf (limit: int) =
        let spec = { Source = divZeroSource depth limit; Limits = baseLimits (); Cancels = false }
        runSpec 1024 "source" spec
        |> fun r -> r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> Seq.map (fun m -> string m.Code) |> List.ofSeq
    let good limit = codesOf limit = [ "DivisionByZero" ]
    if not (good 0) then
        sprintf "ok=false codes=%s greatest=-1 limit=%d" (String.Join(",", codesOf 0)) d
    else
        let mutable lo = 0
        let mutable hi = d + 3
        while hi - lo > 1 do
            let mid = (lo + hi) / 2
            if good mid then lo <- mid else hi <- mid
        sprintf "ok=true codes= greatest=%d limit=%d" lo d

/// "nest|file|<path>|<kb>": load the artifact in `path` and run it, on a thread of kb KB.
let private fileScenario (path: string) (kb: int) : string =
    let bytes = File.ReadAllBytes path
    let r =
        onThread kb (fun () ->
            let compiled = JyroArtifactLoader(bytes).WithStdlib().WithMaxCompilationTime(generousCompile).Load()
            match compiled.TryGetValue() with
            | true, program when compiled.IsSuccess ->
                (JyroRunner(program).WithInputJson("{}").WithExecutionOptions(baseLimits ()).Execute() :> JyroResult<JyroValue>)
            | _ -> JyroResult<JyroValue>.Failure(compiled.Messages))
    describeRun r

/// The child's entry for these scenarios ("nest|...").
let runScenario (parts: string[]) : string =
    match parts with
    | [| "compile"; shape; k; kind; kb |] -> compileScenario shape (int k) kind (int kb)
    | [| "def499"; kb |] -> def499Scenario (int kb)
    | [| "place"; name; kb |] ->
        let _, source, _ = placeCases |> List.find (fun (n, _, _) -> n = name)
        describeCompile (onThread (int kb) (fun () -> compileKind "script" source))
    | [| "run"; name; via; kb |] -> runScenario' name via (int kb)
    | [| "divzero"; depth |] -> divZeroScenario (int depth)
    | [| "file"; path; kb |] -> fileScenario path (int kb)
    | _ -> failwithf "unknown nesting scenario %A" parts

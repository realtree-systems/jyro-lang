/// 100.7: the walks of a value at the deepest call (JYRO-REQ-1725). A walk made inside the
/// deepest call that the safe maximum of the call depth allows MUST give what the same walk gives
/// at the top level, and MUST NOT end the host process. A stack overflow ends the process, so
/// every scenario runs in a child process (Child.fs, "dw|...") on a thread of 1 MB, the stack that
/// the safe maximum of the call depth is measured against (the README of Jyro.Api, Call depth).
module RealTree.Jyro.Spec.Tests.DeepWalkCases

open System
open System.Runtime.ExceptionServices
open System.Threading
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// A walk of JYRO-REQ-2047: the statements that build the values `a` (and `b`) at the depth that
/// `Depth` gives from the maximum `n`, and the expression that walks them.
type Walk =
    { Name: string
      Depth: int -> int
      Objects: bool
      Expression: string }

let walks : Walk list =
    let w name depth objects expr = { Name = name; Depth = depth; Objects = objects; Expression = expr }
    let atN n = n
    let belowN n = n - 1
    [ w "tojson" atN false "ToJson(a)"                                         // the spec's example
      w "equal" atN false "a == b"                                             // the spec's example
      w "notequal" atN false "a != b"
      w "equal-object" atN true "a == b"
      w "tostring" atN false "ToString(a)"
      w "tojson-object" atN true "ToJson(a)"
      w "fromjson" atN false "ToJson(FromJson(ToJson(a)))"
      w "clone" atN false "ToJson(Clone(a))"
      w "flatten" atN false "Length(Flatten(a))"
      w "validateschema" atN false "TypeOf(ValidateSchema(a, {}))"
      w "distinct" belowN false "Length(Distinct([a, b]))"
      w "indexof" belowN false "IndexOf([a], b)"
      w "countif" belowN false "CountIf([{f: a}], \"f\", \"==\", b)" ]

let walkNamed (name: string) : Walk = walks |> List.find (fun w -> w.Name = name)

/// The statements that build `a` and `b`, equal values of the depth of the walk.
let private setup (w: Walk) : string list =
    let depth = w.Depth (maxDepth ())
    if w.Objects then [ buildNestedObj "a" depth; buildNestedObj "b" depth ]
    else [ buildNested "a" depth; buildNested "b" depth ]

/// The walk at the top level of the script.
let topSource (w: Walk) : string =
    lines (setup w @ [ "Output.r = " + w.Expression ])

/// A call of a function (a standard library function counts, as a host function does, 120.2.3)
/// is one more call; the operators `==` and `!=` are not calls. So a walk made by a call is made
/// at the deepest call when the script functions are one fewer than the safe maximum.
let private callsOfTheWalk (w: Walk) : int = if w.Expression.Contains "(" then 1 else 0

/// The walk inside the deepest call: a function that recurses until the call depth limit
/// allows no further call (the walk's own call is the last that it allows), and there builds
/// the values and walks them.
let deepSource (w: Walk) : string =
    lines ([ "func R(n)"; "  if n > 0 then"; "    return R(n - 1)"; "  end" ]
           @ setup w
           @ [ "  return " + w.Expression; "end"; sprintf "Output.r = R(%d)" (maxSafeCallDepth () - 1 - callsOfTheWalk w) ])

/// Run `f` on a new thread of `kb` KB of stack. A stack overflow of the thread ends the process,
/// which is what the test in the parent looks for.
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

/// Every execution limit set explicitly (FLOORS 4.1.11): the call depth at its safe maximum.
let private limits () : JyroExecutionOptions =
    { JyroExecutionOptions.Default with
        MaxCallDepth = maxSafeCallDepth ()
        MaxStatements = 100_000_000
        MaxLoopIterations = 100_000_000
        MaxExecutionTime = TimeSpan.FromSeconds 100. }

let private run (source: string) : JyroResult<JyroValue> =
    let c = JyroCompiler().WithStdlib().WithMaxCompilationTime(generousCompile).WithSource(source).Compile()
    match c.TryGetValue() with
    | true, program when c.IsSuccess -> JyroRunner(program).WithInputJson("{}").WithExecutionOptions(limits ()).Execute()
    | _ -> JyroResult<JyroValue>.Failure(c.Messages)

/// What a run gave: the JSON of Output.r, or the codes of its errors.
let private outcome (r: JyroResult<JyroValue>) : Choice<string, string> =
    if r.IsSuccess then Choice1Of2 (Values.toJson (Values.prop "r" (Script.valueOf r)))
    else
        let codes = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> Seq.map (fun m -> string m.Code)
        Choice2Of2 (String.Join(",", codes))

/// "dw|<walk>|<kb>": the walk at the top level and at the deepest call, on a thread of kb KB.
let private scenario (walk: string) (kb: int) : string =
    let w = walkNamed walk
    let top, deep = onThread kb (fun () -> outcome (run (topSource w)), outcome (run (deepSource w)))
    match top, deep with
    | Choice1Of2 t, Choice1Of2 d when t = d -> sprintf "ok=true codes= length=%d" t.Length
    | Choice1Of2 t, Choice1Of2 d -> sprintf "ok=false codes= top=%s deep=%s" (Values.toJson (Values.str t)) (Values.toJson (Values.str d))
    | Choice2Of2 c, _ -> sprintf "ok=false codes=%s top=failed" c
    | Choice1Of2 _, Choice2Of2 c -> sprintf "ok=false codes=%s deep=failed" c

/// The child's entry for these scenarios ("dw|...").
let runScenario (parts: string[]) : string =
    match parts with
    | [| walk; kb |] -> scenario walk (int kb)
    | _ -> failwithf "unknown deep walk scenario %A" parts

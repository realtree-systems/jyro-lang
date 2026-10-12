/// Scenarios that could overflow the stack or hang (deep recursion, cyclic
/// values) run in a child process, so that a failure of the implementation
/// shows as a failed test and never kills the test host. The child is this
/// test assembly itself, started with `dotnet exec <assembly> child <scenario>`
/// (see Program.fs); it prints one line "RESULT ..." and exits 0.
module RealTree.Jyro.Spec.Tests.Child

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- In the child ---------------------------------------------------------------

let private describeResult (r: JyroResult<JyroValue>) : string =
    let codes = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> Seq.map (fun m -> string m.Code)
    sprintf "ok=%b codes=%s" r.IsSuccess (String.Join(",", codes))

let private describeCall (f: unit -> unit) : string =
    try
        f ()
        "ok=true codes="
    with e ->
        let codes = codesOfExn e |> List.map string
        sprintf "ok=false codes=%s exn=%s" (String.Join(",", codes)) (e.GetType().Name)

let private compileOrFail (source: string) : CompiledProgram =
    let c = JyroCompiler().WithStdlib().WithSource(source).Compile()
    match c.TryGetValue() with
    | true, p when c.IsSuccess -> p
    | _ -> failwithf "compile failed: %s" (Script.describe c)

/// A host function `CallBack(f, v)` that calls the function value `f` with `v`.
type CallBackFunction() =
    inherit JyroFunctionBase(
        "CallBack",
        Host.signature "CallBack"
            [ Parameter.Required("f", ParameterType.Function); Parameter.Required("v", ParameterType.Any) ]
            ParameterType.Any)
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _ctx) =
        match args.[0] with
        | :? JyroFunction as f -> f.Invoke([| args.[1] |])
        | _ -> JyroNull.Instance :> JyroValue

let private depthScenario (config: string) (depth: int) : string =
    let src = recursion depth
    let runner (f: JyroRunner -> JyroRunner) =
        let p = compileOrFail src
        describeResult ((f (JyroRunner(p).WithInputJson("{}"))).Execute())
    let m = maxSafeCallDepth ()
    match config with
    | "default" -> runner id
    | "unlimited" -> runner (fun r -> r.WithExecutionOptions(JyroExecutionOptions.Unlimited))
    | "maxint" -> runner (fun r -> r.WithMaxCallDepth(Int32.MaxValue))
    | "above" -> runner (fun r -> r.WithMaxCallDepth(m + 1000))
    | "options-m1" -> runner (fun r -> r.WithExecutionOptions({ JyroExecutionOptions.Default with MaxCallDepth = m + 1 }))
    | "options-maxint" ->
        runner (fun r -> r.WithExecutionOptions({ JyroExecutionOptions.Unlimited with MaxCallDepth = Int32.MaxValue }))
    | "ExecuteScript" -> describeResult (JyroScript.executeScript src (box (Dictionary<string, obj>())))
    | "ExecuteScriptWithJson" -> describeResult (JyroScript.executeScriptWithJson src "{}")
    | "bytes-unlimited" ->
        let bytes = JyroCompiler().WithStdlib().WithSource(src).CompileToArtifact()
        let p = JyroArtifactLoader(Script.valueOf bytes).WithStdlib().Load() |> Script.valueOf
        describeResult (JyroRunner(p).WithInputJson("{}").WithExecutionOptions(JyroExecutionOptions.Unlimited).Execute())
    | "map-unlimited" ->
        let s =
            lines [ "func R(n)"
                    "  if n > 0 then"
                    "    return Map([n], x => R(x - 1))[0]"
                    "  end"
                    "  return 0"
                    "end"
                    sprintf "Output.r = R(%d)" depth ]
        let p = compileOrFail s
        describeResult (JyroRunner(p).WithInputJson("{}").WithExecutionOptions(JyroExecutionOptions.Unlimited).Execute())
    | "host-unlimited" ->
        let s =
            lines [ "func R(n)"
                    "  if n > 0 then"
                    "    return CallBack(x => R(x), n - 1)"
                    "  end"
                    "  return 0"
                    "end"
                    sprintf "Output.r = R(%d)" depth ]
        let c = JyroCompiler().WithStdlib().AddFunction(CallBackFunction()).WithSource(s).Compile()
        let p = Script.valueOf c
        describeResult (JyroRunner(p).WithInputJson("{}").WithExecutionOptions(JyroExecutionOptions.Unlimited).Execute())
    | other -> failwithf "unknown depth config %s" other

/// Script walks of a cyclic value (100.7.5, 30.8.4). `a` is an array that holds
/// itself; `o` an object whose property points at itself.
let cycleScripts : (string * string) list =
    [ "equal-array", "Output.r = a == b"
      "notequal-array", "Output.r = a != b"
      "equal-object", "Output.r = o == p"
      "tostring-array", "Output.r = ToString(a)"
      "tostring-object", "Output.r = ToString(o)"
      "tojson-array", "Output.r = ToJson(a)"
      "tojson-object", "Output.r = ToJson(o)"
      "clone", "Output.r = Length(Clone(a))"
      "flatten", "Output.r = Length(Flatten(a))"
      "distinct", "Output.r = Length(Distinct([a, b]))"
      "indexof", "Output.r = IndexOf([a], b)"
      "diff", "Output.r = Diff(o, p)"
      "switch", "switch a do case b then Output.r = 1 end"
      "exit", "exit a"
      "output", "Output.r = a" ]

let private cycleScenario (walk: string) : string =
    let body = cycleScripts |> List.find (fun (n, _) -> n = walk) |> snd
    let src =
        lines [ "var a = []"
                "a[0] = a"
                "var b = []"
                "b[0] = b"
                "var o = {}"
                "o.self = o"
                "var p = {}"
                "p.self = p"
                body ]
    describeResult (Script.runWith Script.defaults src)

/// Host API walks of a host-built cyclic value (100.7.5, 100.7.6).
///
/// WithInput gives a cyclic host graph (a Dictionary or a List that holds itself), whose
/// conversion into a Jyro value is a walk (100.7.7) and must fail. A cyclic Jyro value given
/// as Input is not walked: JYRO-REQ-1974 and JYRO-ADR-70 require it to run, and its own tests
/// (P70_90, 70.2.3) cover that (JYRO-DEF-484).
let hostCycleOps : string list =
    [ "ToJson"; "ToStringValue"; "ToObjectValue"; "WriteJson"; "ToDictionary"; "WithInputDictionary"; "WithInputList" ]

let private hostCycleScenario (op: string) : string =
    let a = JyroArray()
    a.Add(a)
    let o = JyroObject()
    o.SetProperty("self", o)
    let runWithHostInput (input: obj) =
        let p = compileOrFail "Output.r = 1"
        describeCall (fun () ->
            let r = JyroRunner(p).WithInput(input).Execute()
            r.GetValueOrThrow() |> ignore)
    match op with
    | "ToJson" -> describeCall (fun () -> a.ToJson() |> ignore)
    | "ToStringValue" -> describeCall (fun () -> a.ToStringValue() |> ignore)
    | "ToObjectValue" -> describeCall (fun () -> a.ToObjectValue() |> ignore)
    | "WriteJson" ->
        describeCall (fun () ->
            use ms = new MemoryStream()
            use w = new System.Text.Json.Utf8JsonWriter(ms)
            o.WriteJson(w))
    | "ToDictionary" -> describeCall (fun () -> o.ToDictionary() |> ignore)
    | "WithInputDictionary" ->
        let d = Dictionary<string, obj>()
        d.["self"] <- box d
        runWithHostInput (box d)
    | "WithInputList" ->
        let l = List<obj>()
        l.Add(box l)
        runWithHostInput (box l)
    | other -> failwithf "unknown host cycle op %s" other

/// SHA-256 of the artifact of a program that holds a literal with a fraction and calls two
/// registered functions (JYRO-REQ-2157).
let registeredFunctionsHash () : string =
    let fns = [ (Host.recorder "Alpha").AsFunction; (Host.recorder "Beta").AsFunction ]
    let source = "Output.x = 1.5\nOutput.a = Alpha(1)\nOutput.b = Beta(2.25)"
    let bytes = Script.compileBytesWith { Script.defaults with Functions = fns } source |> Script.valueOf
    Convert.ToHexString(System.Security.Cryptography.SHA256.HashData bytes)

/// The hashes of `determinismHashes` and `registeredFunctionsHash` under the culture `name`,
/// set for the process (JYRO-REQ-2157).
let private cultureHashes (name: string) : string =
    let c = System.Globalization.CultureInfo(name)
    System.Globalization.CultureInfo.DefaultThreadCurrentCulture <- c
    System.Globalization.CultureInfo.DefaultThreadCurrentUICulture <- c
    System.Globalization.CultureInfo.CurrentCulture <- c
    System.Globalization.CultureInfo.CurrentUICulture <- c
    sprintf "%s;%s;%s" name (determinismHashes ()) (registeredFunctionsHash ())

/// The child's entry: run one scenario and print its RESULT line.
let runScenario (name: string) : int =
    let parts = name.Split('|')
    let line =
        match parts with
        | [| "depth"; config; d |] -> depthScenario config (int d)
        | [| "cycle"; walk |] -> cycleScenario walk
        | [| "hostcycle"; op |] -> hostCycleScenario op
        | [| "probe" |] -> "ok=true codes="
        | [| "hashes" |] -> "ok=true codes= " + determinismHashes ()
        | [| "culturehashes"; name |] -> "ok=true codes= " + cultureHashes name
        | p when p.Length >= 2 && p.[0] = "vs" -> RealTree.Jyro.Spec.Tests.ValueSize.runScenario (Array.skip 1 p)
        | p when p.Length >= 2 && p.[0] = "ic" -> RealTree.Jyro.Spec.Tests.IterationCases.runScenario (Array.skip 1 p)
        | p when p.Length >= 2 && p.[0] = "nest" -> RealTree.Jyro.Spec.Tests.NestingCases.runScenario (Array.skip 1 p)
        | p when p.Length >= 2 && p.[0] = "dw" -> RealTree.Jyro.Spec.Tests.DeepWalkCases.runScenario (Array.skip 1 p)
        | _ -> failwithf "unknown scenario %s" name
    Console.Out.WriteLine("RESULT " + line)
    Console.Out.Flush()
    0

// -- In the test host -------------------------------------------------------------

/// What a child run gave.
type ChildOutcome =
    { ExitCode: int
      TimedOut: bool
      Result: string option
      Output: string }

    member this.Ok = this.Result |> Option.exists (fun r -> r.StartsWith "ok=true")

    member this.Codes : string list =
        match this.Result with
        | Some r ->
            let i = r.IndexOf "codes="
            let rest = r.Substring(i + 6)
            let stop = rest.IndexOf ' '
            let codes = if stop >= 0 then rest.Substring(0, stop) else rest
            codes.Split(',', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        | None -> []

    member this.Describe =
        sprintf "exit %d%s; result %A; output:\n%s" this.ExitCode (if this.TimedOut then " (timed out)" else "")
            this.Result this.Output

let private dotnetHost () : string =
    match Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
    | null | "" ->
        let p = Environment.ProcessPath
        if not (isNull p) && Path.GetFileNameWithoutExtension(p).Equals("dotnet", StringComparison.OrdinalIgnoreCase)
        then p
        else "dotnet"
    | p -> p

/// Run `scenario` in a child process, killed after `timeoutMs`, with the
/// environment variables `env` added (for example a GC heap limit).
let runWith (timeoutMs: int) (env: (string * string) list) (scenario: string) : ChildOutcome =
    let assembly = typeof<CallBackFunction>.Assembly.Location
    let psi = ProcessStartInfo(dotnetHost ())
    psi.ArgumentList.Add "exec"
    psi.ArgumentList.Add assembly
    psi.ArgumentList.Add "child"
    psi.ArgumentList.Add scenario
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    for (k, v) in env do psi.Environment.[k] <- v
    use proc = Process.Start psi
    let out = proc.StandardOutput.ReadToEndAsync()
    let err = proc.StandardError.ReadToEndAsync()
    let finished = proc.WaitForExit timeoutMs
    if not finished then
        try proc.Kill true with _ -> ()
        proc.WaitForExit()
    let text = out.Result + err.Result
    let result =
        text.Split('\n')
        |> Array.tryFindBack (fun l -> l.StartsWith "RESULT ")
        |> Option.map (fun l -> l.Substring(7).Trim())
    { ExitCode = (if finished then proc.ExitCode else -1)
      TimedOut = not finished
      Result = result
      Output = if text.Length > 4000 then text.Substring(0, 4000) else text }

/// Run `scenario` in a child process with a 10 second limit.
let run (scenario: string) : ChildOutcome = runWith 10_000 [] scenario

/// Assert the child finished normally (no crash, no hang).
let expectClean (o: ChildOutcome) : unit =
    if o.TimedOut || o.ExitCode <> 0 || o.Result.IsNone then
        Values.fail ("The child process did not finish normally: " + o.Describe)

/// Assert the child finished normally and its execution succeeded.
let expectSuccess (o: ChildOutcome) : unit =
    expectClean o
    if not o.Ok then Values.fail ("Expected success: " + o.Describe)

/// Assert the child finished normally and failed with exactly `code`.
let expectCode (code: MessageCode) (o: ChildOutcome) : unit =
    expectClean o
    if o.Ok || o.Codes <> [ string code ] then
        Values.fail (sprintf "Expected the one error %A: %s" code o.Describe)

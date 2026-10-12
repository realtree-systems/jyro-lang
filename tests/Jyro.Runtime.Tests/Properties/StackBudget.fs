namespace RealTree.Jyro.Tests.Runtime

open System
open System.Diagnostics
open System.IO
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Compilation

/// JYRO-DEF-487, JYRO-DEF-488, decision JYRO-ADR-108: the call depth ceiling
/// (96) and the default (64) are safe on a 1 MB thread for the worst shapes the
/// language allows: a recursive call inside nested blocks, with the limit
/// tripping, an ordinary runtime error, `fail`, or `exit` raised at the deepest
/// permitted call.
///
/// A stack overflow cannot be caught and kills the process, so every scenario
/// runs in a child process (this test assembly, started as
/// `dotnet exec <assembly> stack <scenario>`, see Program.fs), and inside the
/// child on a thread of exactly 1 MB. An overflow then shows as one failed test
/// whose output reads "Stack overflow", and the rest of the run carries on.
module StackBudget =

    /// The smallest host thread the ceiling is measured against (ADR-108).
    let stackBytes = 1024 * 1024

    // -- The scripts --------------------------------------------------------

    /// Opening and closing lines of `kind` blocks, `count` deep. `mixed`
    /// cycles through if, while and foreach.
    let private blocks (kind: string) (count: int) : (string * string) list =
        let one k =
            match k with
            | "if" -> "if true then", "end"
            | "while" -> "while true do", "end"
            | "foreach" -> "foreach x{i} in [1] do", "end"
            | other -> failwithf "unknown block kind %s" other
        [ for i in 0 .. count - 1 ->
            match kind with
            | "mixed" -> one ([| "if"; "while"; "foreach" |].[i % 3])
            | k -> one k ]

    /// The leaf statement run by the deepest call.
    let private leafOf (leaf: string) : string =
        match leaf with
        | "div" -> "return 1 / 0"
        | "fail" -> "fail \"deep\""
        | "exit" -> "exit"
        | "return" -> "return 0"
        | other -> failwithf "unknown leaf %s" other

    /// The line (1-based) of the leaf statement in `script`.
    let leafLine = 4

    /// `R(n, t)` recurses with its call `nesting` blocks deep, recording the
    /// depth reached in `t` (which is `Output.t`). When `leaf` is "limit" it
    /// never stops, so the call depth limit trips; otherwise the call at depth
    /// `depth` (the first call is depth 1) runs the leaf.
    let script (kind: string) (nesting: int) (leaf: string) (depth: int) : string =
        let bs = blocks kind nesting
        let pad i = String(' ', 2 * (i + 1))
        [ yield "func R(n, t)"
          yield "  t.reached = n"
          if leaf <> "limit" then
              yield sprintf "  if n >= %d then" depth
              yield "    " + leafOf leaf
              yield "  end"
          for i, (opening, _) in List.indexed bs do
              yield pad i + opening.Replace("{i}", string i)
          yield pad bs.Length + "return R(n + 1, t)"
          for i, (_, closing) in List.rev (List.indexed bs) do
              yield pad i + closing
          yield "  return 0"
          yield "end"
          yield "Output.t = {}"
          yield "Output.r = R(1, Output.t)" ]
        |> String.concat "\n"

    // -- In the child -------------------------------------------------------

    let private describe (r: JyroResult<JyroValue>) : string =
        let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
        let codes = errors |> List.map (fun m -> string m.Code)
        let line =
            errors
            |> List.tryHead
            |> Option.bind (fun m -> Option.ofNullable m.Location)
            |> Option.map (fun l -> string l.Line)
            |> Option.defaultValue "none"
        let out =
            match r.TryValue with
            | Some v -> v.ToJson().Replace("\n", "").Replace("\r", "").Replace(" ", "")
            | None -> "none"
        sprintf "ok=%b codes=%s line=%s out=%s" r.IsSuccess (String.Join(",", codes)) line out

    /// Scenario "<limit>|<kind>|<nesting>|<leaf>": `limit` is "default" (no
    /// limit set) or a number given to WithMaxCallDepth. Leaves other than
    /// "limit" run at the deepest call the limit permits.
    let private scenario (name: string) : string =
        match name.Split('|') with
        | [| limit; kind; nesting; leaf |] ->
            let max = if limit = "default" then JyroExecutionOptions.Default.MaxCallDepth else int limit
            let src = script kind (int nesting) leaf max
            let program =
                match JyroCompiler().WithStdlib().WithSource(src).Compile() with
                | Produced p -> p
                | c -> failwithf "the probe script failed to compile: %A\n%s" (List.ofSeq c.Messages) src
            let mutable line = ""
            let body () =
                let runner = JyroRunner(program).WithInput(JyroObject() :> JyroValue)
                let runner = if limit = "default" then runner else runner.WithMaxCallDepth(max)
                line <- describe (runner.Execute())
            let t = Thread(body, stackBytes)
            t.Start()
            t.Join()
            line
        | [| "probe" |] -> "ok=true codes= line=none out=none"
        | _ -> failwithf "unknown stack scenario %s" name

    /// The child's entry: run one scenario and print its RESULT line.
    let runChild (name: string) : int =
        Console.Out.WriteLine("RESULT " + scenario name)
        Console.Out.Flush()
        0

    // -- In the test host ---------------------------------------------------

    type Outcome =
        { ExitCode: int
          TimedOut: bool
          Result: string option
          Output: string }

        member this.Field(key: string) : string =
            match this.Result with
            | Some r ->
                r.Split(' ')
                |> Array.tryFind (fun p -> p.StartsWith(key + "="))
                |> Option.map (fun p -> p.Substring(key.Length + 1))
                |> Option.defaultValue ""
            | None -> ""

        member this.Describe =
            sprintf "exit code %d%s; result %A; output:\n%s" this.ExitCode
                (if this.TimedOut then " (timed out)" else "") this.Result this.Output

    let private dotnetHost () : string =
        match Environment.GetEnvironmentVariable "DOTNET_HOST_PATH" with
        | null | "" ->
            let p = Environment.ProcessPath
            if not (isNull p) && Path.GetFileNameWithoutExtension(p).Equals("dotnet", StringComparison.OrdinalIgnoreCase)
            then p
            else "dotnet"
        | p -> p

    let run (name: string) : Outcome =
        let psi = ProcessStartInfo(dotnetHost ())
        psi.ArgumentList.Add "exec"
        psi.ArgumentList.Add(typeof<Outcome>.Assembly.Location)
        psi.ArgumentList.Add "stack"
        psi.ArgumentList.Add name
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        use proc = Process.Start psi
        let out = proc.StandardOutput.ReadToEndAsync()
        let err = proc.StandardError.ReadToEndAsync()
        let finished = proc.WaitForExit 60_000
        if not finished then
            try proc.Kill true with _ -> ()
            proc.WaitForExit()
        let text = out.Result + err.Result
        { ExitCode = (if finished then proc.ExitCode else -1)
          TimedOut = not finished
          Result =
            text.Split('\n')
            |> Array.tryFindBack (fun l -> l.StartsWith "RESULT ")
            |> Option.map (fun l -> l.Substring(7).Trim())
          Output = if text.Length > 4000 then text.Substring(0, 4000) else text }

    let private expectClean (o: Outcome) =
        if o.TimedOut || o.ExitCode <> 0 || o.Result.IsNone then
            Assert.Fail("The run did not finish normally on a 1 MB thread (a stack overflow kills the process): " + o.Describe)

    let private expectError (code: MessageCode) (o: Outcome) =
        expectClean o
        Assert.True(o.Field "ok" = "false" && o.Field "codes" = string code,
                    sprintf "Expected the one error %A: %s" code o.Describe)

    let private expectSuccessReaching (depth: int) (o: Outcome) =
        expectClean o
        Assert.True(o.Field "ok" = "true", "Expected success: " + o.Describe)
        Assert.True(o.Field("out").Contains(sprintf "\"reached\":%d" depth),
                    sprintf "Expected Output.t.reached = %d: %s" depth o.Describe)

    let private limitOf (limit: string) =
        if limit = "default" then JyroExecutionOptions.Default.MaxCallDepth else int limit

    let private name limit kind nesting leaf = sprintf "%s|%s|%d|%s" limit kind nesting leaf

    // -- The tests ----------------------------------------------------------

    [<Fact>]
    let ``JYRO-DEF-487 the child process harness runs a scenario on a 1 MB thread`` () =
        run "probe" |> expectClean

    [<Fact>]
    let ``JYRO-DEF-487 the ceiling and default the tests measure are 96 and 64`` () =
        // ADR-108 keeps both values; the scenarios below rest on them.
        Assert.Equal(96, JyroExecutionOptions.MaxSafeCallDepth)
        Assert.Equal(64, JyroExecutionOptions.Default.MaxCallDepth)

    [<Theory>]
    [<InlineData("default", "if", 0)>]
    [<InlineData("default", "if", 1)>]
    [<InlineData("default", "if", 2)>]
    [<InlineData("default", "if", 3)>]
    [<InlineData("default", "while", 3)>]
    [<InlineData("default", "foreach", 3)>]
    [<InlineData("default", "mixed", 6)>]
    [<InlineData("96", "if", 0)>]
    [<InlineData("96", "if", 1)>]
    [<InlineData("96", "if", 2)>]
    [<InlineData("96", "if", 3)>]
    [<InlineData("96", "while", 3)>]
    [<InlineData("96", "foreach", 3)>]
    [<InlineData("96", "mixed", 6)>]
    let ``JYRO-DEF-487 the call depth limit trips cleanly on a 1 MB thread with the recursive call in nested blocks``
        (limit: string, kind: string, nesting: int) =
        run (name limit kind nesting "limit") |> expectError MessageCode.CallDepthLimitExceeded

    [<Theory>]
    [<InlineData("default", "if", 2)>]
    [<InlineData("default", "if", 3)>]
    [<InlineData("default", "mixed", 6)>]
    [<InlineData("96", "if", 2)>]
    [<InlineData("96", "if", 3)>]
    [<InlineData("96", "mixed", 6)>]
    let ``JYRO-DEF-488 a runtime error at the deepest permitted call in nested blocks fails cleanly on a 1 MB thread``
        (limit: string, kind: string, nesting: int) =
        let o = run (name limit kind nesting "div")
        expectError MessageCode.DivisionByZero o
        // ADR-108: the location is attached once, still the statement that raised it.
        Assert.True(o.Field "line" = string leafLine, "Expected the error located at the leaf statement: " + o.Describe)

    [<Theory>]
    [<InlineData("default", "if", 2)>]
    [<InlineData("default", "if", 3)>]
    [<InlineData("default", "mixed", 6)>]
    [<InlineData("96", "if", 2)>]
    [<InlineData("96", "if", 3)>]
    [<InlineData("96", "mixed", 6)>]
    let ``JYRO-DEF-488 fail at the deepest permitted call in nested blocks fails cleanly on a 1 MB thread``
        (limit: string, kind: string, nesting: int) =
        let o = run (name limit kind nesting "fail")
        expectError MessageCode.ScriptFailure o
        Assert.True(o.Field "line" = string leafLine, "Expected the failure located at the fail statement: " + o.Describe)

    [<Theory>]
    [<InlineData("default", "if", 2)>]
    [<InlineData("default", "if", 3)>]
    [<InlineData("default", "mixed", 6)>]
    [<InlineData("96", "if", 2)>]
    [<InlineData("96", "if", 3)>]
    [<InlineData("96", "mixed", 6)>]
    let ``JYRO-DEF-488 exit at the deepest permitted call in nested blocks succeeds with its Output on a 1 MB thread``
        (limit: string, kind: string, nesting: int) =
        run (name limit kind nesting "exit") |> expectSuccessReaching (limitOf limit)

    [<Theory>]
    [<InlineData("default", "mixed", 6)>]
    [<InlineData("96", "mixed", 6)>]
    let ``JYRO-DEF-488 returning normally from the deepest permitted call in nested blocks succeeds on a 1 MB thread``
        (limit: string, kind: string, nesting: int) =
        run (name limit kind nesting "return") |> expectSuccessReaching (limitOf limit)

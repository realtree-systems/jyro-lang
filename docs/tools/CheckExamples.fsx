// Runs every ```jyro block of the jyro-doc manual and checks it against the block
// that follows it.
//
//   dotnet build -c Release src/Jyro.Api/Jyro.Api.fsproj
//   dotnet fsi docs/tools/CheckExamples.fsx [path-under-manual]
//
// A jyro block may start with directive comment lines:
//   # fragment              the block is not a complete script and is skipped
//   # input: <json>         the Input of the run (default {})
//
// The block that follows a jyro block, separated by blank lines only, states
// what the run gives:
//   ```json                 the Output, compared structurally
//   ```text starting [JM    the diagnostics, one formatted message per line
//   ```text starting Exit message:   the exit message
// A jyro block with no such block must simply succeed.

#r "nuget: FParsec, 1.1.1"
#r "../../src/Jyro.Api/bin/Release/net8.0/Jyro.Core.dll"
#r "../../src/Jyro.Api/bin/Release/net8.0/Jyro.Parser.dll"
#r "../../src/Jyro.Api/bin/Release/net8.0/Jyro.Compiler.dll"
#r "../../src/Jyro.Api/bin/Release/net8.0/Jyro.Stdlib.dll"
#r "../../src/Jyro.Api/bin/Release/net8.0/Jyro.Api.dll"

open System
open System.IO
open System.Text.Json
open RealTree.Jyro

// The manual lives in the jyro-doc repository: a sibling of this one, or the
// root named by the JYRO_DOC environment variable.
let root =
    match Environment.GetEnvironmentVariable "JYRO_DOC" with
    | null | "" -> Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "jyro-doc", "manual"))
    | root -> Path.Combine(root, "manual")
let scope =
    match fsi.CommandLineArgs |> Array.skip 1 with
    | [| p |] -> Path.Combine(root, p)
    | _ -> root

type Fence = { Lang: string; Lines: string list; StartLine: int; TrailingBlankOnly: bool }

/// Every fenced block of a page, with whether only blank lines separate it
/// from the block before it.
let fences (text: string) =
    let lines = text.Replace("\r\n", "\n").Split('\n')
    let result = ResizeArray<Fence>()
    let mutable i = 0
    let mutable sinceLast = -1   // -1: no previous fence; otherwise the number of non-blank lines since it closed
    while i < lines.Length do
        let line = lines.[i]
        if line.StartsWith("```") then
            let lang = line.Substring(3).Trim()
            let start = i + 1
            let body = ResizeArray<string>()
            i <- i + 1
            while i < lines.Length && not (lines.[i].StartsWith("```")) do
                body.Add(lines.[i])
                i <- i + 1
            result.Add({ Lang = lang; Lines = List.ofSeq body; StartLine = start; TrailingBlankOnly = sinceLast = 0 })
            sinceLast <- 0
        elif sinceLast >= 0 && line.Trim() <> "" then
            sinceLast <- sinceLast + 1
        i <- i + 1
    List.ofSeq result

let rec jsonEqual (a: JsonElement) (b: JsonElement) =
    if a.ValueKind <> b.ValueKind then false
    else
        match a.ValueKind with
        | JsonValueKind.Object ->
            let pa = a.EnumerateObject() |> Seq.map (fun p -> p.Name, p.Value) |> List.ofSeq
            let pb = b.EnumerateObject() |> Seq.map (fun p -> p.Name, p.Value) |> List.ofSeq
            pa.Length = pb.Length
            && List.forall2 (fun (na, va) (nb, vb) -> na = nb && jsonEqual va vb) pa pb
        | JsonValueKind.Array ->
            let ea = a.EnumerateArray() |> List.ofSeq
            let eb = b.EnumerateArray() |> List.ofSeq
            ea.Length = eb.Length && List.forall2 jsonEqual ea eb
        | JsonValueKind.Number ->
            match a.TryGetDecimal(), b.TryGetDecimal() with
            | (true, x), (true, y) -> x = y
            | _ -> a.GetDouble() = b.GetDouble()
        | JsonValueKind.String -> a.GetString() = b.GetString()
        | _ -> true

let mutable passed = 0
let failures = ResizeArray<string>()

let fail (page: string) (line: int) (what: string) =
    failures.Add(sprintf "%s:%d: %s" page line what)

let check (page: string) (block: Fence) (expected: Fence option) =
    let directives =
        block.Lines |> List.filter (fun l -> l.StartsWith("# fragment") || l.StartsWith("# input:"))
    let body = block.Lines
    if directives |> List.exists (fun l -> l.StartsWith("# fragment")) then ()
    else
        let input =
            directives
            |> List.tryPick (fun l -> if l.StartsWith("# input:") then Some(l.Substring(8).Trim()) else None)
            |> Option.defaultValue "{}"
        let source = String.Join("\n", body)
        let result = JyroScript.executeScriptWithJson source input
        let messages = result.Messages |> Seq.map DiagnosticFormatter.formatMessage |> List.ofSeq
        let actualJson () =
            match result.TryGetValue() with
            | true, v -> (try v.ToJson() with e -> sprintf "<ToJson failed: %s>" e.Message)
            | _ -> "<no value>"
        match expected with
        | Some { Lang = "json"; Lines = lines } ->
            let text = String.Join("\n", lines)
            if not result.IsSuccess then
                fail page block.StartLine (sprintf "expected success, got %s" (String.Join(" | ", messages)))
            else
                let ok =
                    try jsonEqual (JsonDocument.Parse(text).RootElement) (JsonDocument.Parse(actualJson ()).RootElement)
                    with _ -> false
                if ok then passed <- passed + 1
                else fail page block.StartLine (sprintf "output differs\n    expected %s\n    actual   %s" (text.Replace("\n", " ")) (actualJson ()))
        | Some { Lang = "text"; Lines = lines } when lines |> List.exists (fun l -> l.StartsWith("[JM")) ->
            let expectedLines = lines |> List.filter (fun l -> l.Trim() <> "")
            if expectedLines = messages then passed <- passed + 1
            else fail page block.StartLine (sprintf "diagnostics differ\n    expected %s\n    actual   %s" (String.Join(" | ", expectedLines)) (String.Join(" | ", messages)))
        | Some { Lang = "text"; Lines = first :: _ } when first.StartsWith("Exit message:") ->
            let expectedExit = first.Substring(13).Trim()
            let actualExit = if isNull result.ExitMessage then "<null>" else result.ExitMessage
            if result.IsSuccess && actualExit = expectedExit then passed <- passed + 1
            else fail page block.StartLine (sprintf "exit message differs: expected %s, actual %s (%s)" expectedExit actualExit (String.Join(" | ", messages)))
        | _ ->
            if result.IsSuccess then passed <- passed + 1
            else fail page block.StartLine (sprintf "expected success, got %s" (String.Join(" | ", messages)))

let pages =
    if File.Exists scope then [| scope |]
    else Directory.GetFiles(scope, "*.md", SearchOption.AllDirectories)

for path in pages |> Array.sort do
    let page = Path.GetRelativePath(root, path)
    let all = fences (File.ReadAllText path)
    let rec walk = function
        | (b: Fence) :: rest when b.Lang = "jyro" ->
            let expected =
                match rest with
                | next :: _ when next.TrailingBlankOnly && (next.Lang = "json" || next.Lang = "text") -> Some next
                | _ -> None
            check page b expected
            walk rest
        | _ :: rest -> walk rest
        | [] -> ()
    walk all

printfn "%d example(s) passed, %d failed" passed failures.Count
for f in failures do printfn "%s" f
exit (if failures.Count = 0 then 0 else 1)

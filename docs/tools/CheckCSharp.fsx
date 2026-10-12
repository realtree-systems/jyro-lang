// Compiles every ```csharp block of the jyro-doc manual against the Release build.
//
//   dotnet build -c Release src/Jyro.Api/Jyro.Api.fsproj
//   dotnet fsi docs/tools/CheckCSharp.fsx [scratch-project-dir]
//
// Each block becomes one file in the scratch project's Examples folder. A
// block whose first line is `// fragment` is skipped. A block that declares a
// type at column 0 is emitted as top-level declarations in the namespace
// `Shared`; any other block is wrapped in a static method in a namespace of
// its own that opens `Shared`. `using` directives are hoisted.

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Diagnostics

// The manual lives in the jyro-doc repository: a sibling of this one, or the
// root named by the JYRO_DOC environment variable.
let manual =
    match Environment.GetEnvironmentVariable "JYRO_DOC" with
    | null | "" -> Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "jyro-doc", "manual"))
    | root -> Path.Combine(root, "manual")
let project =
    match fsi.CommandLineArgs |> Array.skip 1 with
    | [| p |] -> p
    | _ -> Path.Combine(Path.GetTempPath(), "jyro-docs-csharp-check")

let examples = Path.Combine(project, "Examples")
if Directory.Exists examples then Directory.Delete(examples, true)
Directory.CreateDirectory examples |> ignore

if not (File.Exists(Path.Combine(project, "check.csproj"))) then
    let bin = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Jyro.Api", "bin", "Release", "net8.0"))
    let refs =
        [ "Jyro.Core"; "Jyro.Parser"; "Jyro.Compiler"; "Jyro.Stdlib"; "Jyro.Api" ]
        |> List.map (fun n -> sprintf "    <Reference Include=\"%s\"><HintPath>%s</HintPath></Reference>" n (Path.Combine(bin, n + ".dll")))
        |> String.concat "\n"
    File.WriteAllText(Path.Combine(project, "check.csproj"), sprintf """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Library</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <NoWarn>CS8600;CS8602;CS8604;CS8625;CS0219;CS0168;CS1998</NoWarn>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="FParsec" Version="1.1.1" />
%s
  </ItemGroup>
</Project>
""" refs)

let usingDirective = Regex(@"^using (static )?[A-Za-z_][\w.]*( = [\w.<>, ]+)?;$")
let typeDecl = Regex(@"^(public |internal |private )?(sealed |static |abstract |partial )*(class|record|struct|interface|enum) ", RegexOptions.Multiline)

let mutable count = 0
for path in Directory.GetFiles(manual, "*.md", SearchOption.AllDirectories) |> Array.sort do
    let lines = (File.ReadAllText path).Replace("\r\n", "\n").Split('\n')
    let mutable i = 0
    let mutable n = 0
    while i < lines.Length do
        if lines.[i].Trim() = "```csharp" then
            let body = ResizeArray<string>()
            i <- i + 1
            while i < lines.Length && not (lines.[i].StartsWith("```")) do
                body.Add(lines.[i])
                i <- i + 1
            n <- n + 1
            if not (body.Count > 0 && body.[0].Trim() = "// fragment") then
                count <- count + 1
                let usings, rest = body |> List.ofSeq |> List.partition (fun l -> usingDirective.IsMatch(l.Trim()))
                let usingText = String.Join("\n", usings)
                let text = String.Join("\n", rest)
                let file =
                    if typeDecl.IsMatch text then
                        sprintf "%s\nnamespace Shared;\n%s\n" usingText text
                    else
                        sprintf "%s\nusing Shared;\nnamespace Block%d;\npublic static class Example\n{\n    public static void Run()\n    {\n%s\n    }\n}\n" usingText count text
                let name = Path.GetFileNameWithoutExtension(path).Replace(" ", "_")
                File.WriteAllText(Path.Combine(examples, sprintf "%s_%d.cs" name n), sprintf "// %s block %d\n%s" (Path.GetRelativePath(manual, path)) n file, UTF8Encoding(false))
        i <- i + 1

// The Shared namespace must exist even when no block declares a type.
File.WriteAllText(Path.Combine(examples, "_Shared.cs"), "namespace Shared { internal static class Anchor { } }\n")

printfn "%d block(s) written to %s" count examples
let psi = ProcessStartInfo("dotnet", "build --nologo -v q", WorkingDirectory = project, RedirectStandardOutput = true, RedirectStandardError = true, UseShellExecute = false)
let p = Process.Start psi
let out = p.StandardOutput.ReadToEnd() + p.StandardError.ReadToEnd()
p.WaitForExit()
for line in out.Split('\n') |> Seq.distinct do
    if line.Contains("error") then printfn "%s" (line.Trim())
printfn "%s" (if p.ExitCode = 0 then "all blocks compile" else "build failed")
exit p.ExitCode

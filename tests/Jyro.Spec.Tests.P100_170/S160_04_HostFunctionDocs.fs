/// Part 160.4 (host function side effects): the XML documentation of the public
/// host-function registration surface warns that a host function with side
/// effects weakens the sandbox (JYRO-REQ-1602, whose procedure names the XML
/// docs of AddFunction, AddFunctions and IJyroFunction). The compiler writes the
/// XML documentation files of Jyro.Api and Jyro.Core beside the test assembly;
/// a test reads the entry of each member from them.
module RealTree.Jyro.Spec.Tests.S160_04_HostFunctionDocs

open System
open System.IO
open System.Xml.Linq
open Xunit
open RealTree.Jyro.Spec.Tests.Support

/// The documentation text (summary, remarks and the rest) of the member whose
/// XML documentation id starts with `idPrefix`, read from `file` beside the
/// test assembly. Fails the test when the file or the member is missing.
let private docOf (file: string) (idPrefix: string) : string =
    let path = Path.Combine(AppContext.BaseDirectory, file)
    if not (File.Exists path) then Assert.Fail(sprintf "The XML documentation file %s is not beside the test assembly (%s)." file path)
    let members = XDocument.Load(path).Descendants(XName.Get "member") |> Seq.toList
    match members |> List.tryFind (fun m -> (m.Attribute(XName.Get "name").Value).StartsWith idPrefix) with
    | Some m -> m.Value
    | None -> Assert.Fail(sprintf "%s has no documentation entry for %s." file idPrefix); ""

/// A warning in the sense of 160.4.1: the documentation says that side effects
/// weaken the sandbox.
let private warnsOfSideEffects (doc: string) : bool =
    let d = doc.ToLowerInvariant()
    d.Contains "side effect" && d.Contains "sandbox" && d.Contains "weaken"

let private assertWarns (what: string) (doc: string) =
    Assert.True(warnsOfSideEffects doc, sprintf "The XML documentation of %s does not warn that a host function with side effects weakens the sandbox. It reads: %s" what (doc.Trim()))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1602")>]
[<Trait("defect", "JYRO-DEF-739")>]
let ``JYRO-REQ-1602 JYRO-DEF-739 TheXmlDocOfAddFunction_WarnsThatSideEffectsWeakenTheSandbox`` () =
    assertWarns "JyroCompiler.AddFunction" (docOf "Jyro.Api.xml" "M:RealTree.Jyro.JyroCompiler.AddFunction(")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1602")>]
[<Trait("defect", "JYRO-DEF-739")>]
let ``JYRO-REQ-1602 JYRO-DEF-739 TheXmlDocOfAddFunctions_WarnsThatSideEffectsWeakenTheSandbox`` () =
    assertWarns "JyroCompiler.AddFunctions" (docOf "Jyro.Api.xml" "M:RealTree.Jyro.JyroCompiler.AddFunctions(")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1602")>]
[<Trait("defect", "JYRO-DEF-739")>]
let ``JYRO-REQ-1602 JYRO-DEF-739 TheXmlDocOfIJyroFunction_WarnsThatSideEffectsWeakenTheSandbox`` () =
    assertWarns "IJyroFunction" (docOf "Jyro.Core.xml" "T:RealTree.Jyro.IJyroFunction")

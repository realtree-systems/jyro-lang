namespace RealTree.Jyro.Tests.Properties

open System.IO
open System.Reflection
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests.ParseShape

/// Negative tests: inputs that must NOT parse.
/// Mix of hand-crafted snippets and a file-based corpus under corpus/invalid/.
module NegativeProperties =

    /// "rejected" means either a ParseFailure result OR a .NET exception
    /// during parsing. Some error paths (invalid assignment target, delete
    /// on a non-target) raise via `failwith` rather than returning
    /// ParseFailure - both are legitimate rejection signals.
    let private rejects (src: string) =
        try
            match Parser.parse src with
            | ParseFailure _ -> true
            | ParseSuccess _ -> false
        with _ -> true

    // -- Structural mutations -------------------------------------------------

    [<Theory>]
    [<InlineData("if true then\n  var x = 1\n")>]                       // missing end
    [<InlineData("while true do\n  break")>]                             // missing end
    [<InlineData("func f()\n  return")>]                                 // missing end
    [<InlineData("union U A B")>]                                        // missing end
    [<InlineData("if true\n  var x = 1\nend")>]                          // missing then
    [<InlineData("while true\n  break\nend")>]                           // missing do
    [<InlineData("for i in 0 to 10\n  break\nend")>]                     // missing do
    [<InlineData("foreach x in xs\n  break\nend")>]                      // missing do
    [<InlineData("switch x\n  case 1 then\n    break\nend")>]            // missing do after switch
    [<InlineData("match x\n  case A then\n    break\nend")>]             // missing do after match
    let ``structurally malformed programs are rejected`` (src: string) =
        Assert.True(rejects src, sprintf "Expected parse to fail:\n%s" src)

    [<Theory>]
    [<InlineData("var true = 5")>]           // keyword as identifier
    [<InlineData("var if = 5")>]             // keyword as identifier
    [<InlineData("var x = var")>]            // keyword in expression position
    [<InlineData("var = 5")>]                // missing identifier after var
    let ``keyword and target mis-use is rejected`` (src: string) =
        Assert.True(rejects src, sprintf "Expected parse to fail:\n%s" src)

    /// Semantic target validation (delete/assign LHS shape) is a
    /// compiler-layer check - the parser builds the permissive AST;
    /// `Ast.isDeleteTarget` / `Ast.isAssignmentTarget` gate downstream.
    [<Theory>]
    [<InlineData("delete x")>]
    [<InlineData("delete 42")>]
    [<InlineData("5 = x")>]
    [<InlineData("(a + b) = 5")>]
    let ``non-target as delete or assign LHS parses (rejection is compile-time)`` (src: string) =
        match Parser.parse src with
        | ParseSuccess _ -> ()
        | ParseFailure(msg, _, _) ->
            Assert.Fail(sprintf "Expected parser to accept %A, got: %s" src msg)

    /// Function calls are only valid on bare identifiers. Non-identifier
    /// bases (grouped expressions, property-access chains) are rejected.
    [<Theory>]
    [<InlineData("(a + b)(x)")>]
    [<InlineData("obj.method(x)")>]
    let ``call on non-identifier is a parse-time rejection`` (src: string) =
        Assert.True(rejects src, sprintf "Expected parse to fail:\n%s" src)

    [<Theory>]
    [<InlineData("match x do end")>]         // match needs at least one case
    [<InlineData("x is blargh")>]            // not a real type keyword
    [<InlineData("x is")>]                   // dangling `is`
    [<InlineData("\"unterminated")>]         // unterminated string
    [<InlineData("'unterminated")>]          // unterminated string
    [<InlineData("0xGG")>]                   // invalid hex digits
    let ``lexical and syntactic anomalies are rejected`` (src: string) =
        Assert.True(rejects src, sprintf "Expected parse to fail:\n%s" src)

    // -- Token-mutation property ---------------------------------------------
    //
    // Take a known-valid program and swap an identifier position for a
    // reserved keyword. The mutated program must not parse. We don't
    // FsCheck-generate this one because shrinkers would shrink the program
    // away from the mutation - a focused Theory gives us tighter signal.

    [<Theory>]
    [<InlineData("var x = 1", "var if = 1")>]
    [<InlineData("var x = 1", "var x = while")>]
    [<InlineData("aa + bb", "aa + end")>]
    [<InlineData("func go()\n  return\nend", "func return()\n  return\nend")>]
    let ``identifier-to-keyword swap is rejected`` (valid: string) (mutated: string) =
        Assert.True(
            (match Parser.parse valid with
             | ParseSuccess _ -> true
             | _ -> false),
            "Control case should parse")
        Assert.True(rejects mutated, sprintf "Mutated source should fail:\n%s" mutated)

    // -- File-based invalid corpus -------------------------------------------
    //
    // Each .jyro file under corpus/invalid/ must fail to parse. Acts as a
    // regression anchor for error-path behavior.

    let invalidCorpusFiles : obj[] seq =
        let dir =
            Path.Combine(
                Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                "corpus", "invalid")
        if Directory.Exists(dir) then
            Directory.EnumerateFiles(dir, "*.jyro")
            |> Seq.map (fun f -> [| box f |])
        else
            Seq.empty

    [<Theory>]
    [<MemberData(nameof invalidCorpusFiles)>]
    let ``corpus invalid/*.jyro programs are rejected`` (path: string) =
        let src = File.ReadAllText(path)
        Assert.True(
            rejects src,
            sprintf "Expected %s to fail parsing" (Path.GetFileName(path)))

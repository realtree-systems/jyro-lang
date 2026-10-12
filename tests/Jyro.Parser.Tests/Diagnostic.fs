namespace RealTree.Jyro.Tests

open FsCheck
open FsCheck.FSharp
open global.Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Tests.ParseShape

/// A manual diagnostic harness for tracking down round-trip failures.
/// Runs the generator directly so we can print the exact source that
/// broke. Not part of the property suite - invoked with
/// `dotnet test --filter FullyQualifiedName~Diagnostic`.
module Diagnostic =

    let private sampleOne (size: int) (g: Gen<'a>) : 'a =
        let arr : 'a[] = FsCheck.FSharp.Gen.sampleWithSize size 1 g
        arr.[0]

    let private findFailingStmt (count: int) : (Stmt * string * string) option =
        // Start at size 1 and grow; first failure we find is the smallest.
        let sizes = [ 1; 2; 3; 4; 5 ]
        let mutable result = None
        let mutable i = 0
        while result.IsNone && i < count do
            for size in sizes do
                if result.IsNone then
                    let s = sampleOne size (Generators.genStmt size)
                    let src = Printer.printStatement s
                    match Parser.parse src with
                    | ParseFailure(msg, line, col) ->
                        result <- Some(s, src, sprintf "parse failed at %d:%d: %s" line col msg)
                    | ParseSuccess p when List.length p.Statements <> 1 ->
                        result <- Some(s, src, sprintf "produced %d statements" (List.length p.Statements))
                    | ParseSuccess p ->
                        if not (AstEquality.eqStmt s p.Statements.[0]) then
                            result <- Some(s, src, sprintf "AST mismatch.\nExpected: %A\nActual:   %A" s p.Statements.[0])
            i <- i + 1
        result

    [<Fact>]
    let ``find first failing statement round-trip`` () =
        match findFailingStmt 200 with
        | None -> ()
        | Some(s, src, why) ->
            Assert.Fail(sprintf "Failing statement:\n%A\n\n-- printed source --\n%s\n-- reason --\n%s" s src why)

    let private findFailingProgram (count: int) : (Program * string * string) option =
        let sizes = [ 1; 2; 3; 4 ]
        let mutable result = None
        let mutable i = 0
        while result.IsNone && i < count do
            for size in sizes do
                if result.IsNone then
                    let p = sampleOne size Generators.genProgram
                    let src = Printer.printProgram p
                    match Parser.parse src with
                    | ParseFailure(msg, line, col) ->
                        result <- Some(p, src, sprintf "parse failed at %d:%d: %s" line col msg)
                    | ParseSuccess reparsed ->
                        if not (AstEquality.eqProgram p reparsed) then
                            result <- Some(p, src, sprintf "AST mismatch\nExpected: %A\nActual:   %A" p reparsed)
            i <- i + 1
        result

    [<Fact>]
    let ``find first failing program round-trip`` () =
        match findFailingProgram 200 with
        | None -> ()
        | Some(_, src, why) ->
            Assert.Fail(sprintf "Failing program:\n\n-- printed source --\n%s\n-- reason --\n%s" src why)

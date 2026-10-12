namespace RealTree.Jyro.Tests.Compiler

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Match-exhaustiveness tests: the compiler requires every variant of a
/// union to be covered by a `match` statement (or expression). Omitting a
/// case must produce `MatchNotExhaustive`.
module Exhaustiveness =

    let private validate (src: string) =
        match Parser.parse src with
        | Failed msgs ->
            failwithf "Expected parse to succeed; got: %A" (msgs |> List.ofSeq |> List.map (fun m -> m.Code))
        | Produced program ->
            Validator.validate program
        | _ -> failwith "unreachable"

    let private expectSuccess (src: string) =
        let r = validate src
        Assert.True(r.IsSuccess, sprintf "Expected validate to succeed.\nDiagnostics: %A\nSource:\n%s"
                                    (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)) src)

    let private expectCode (expected: MessageCode) (src: string) =
        let r = validate src
        Assert.False(r.IsSuccess, sprintf "Expected validate to fail with %A:\n%s" expected src)
        Assert.True(
            List.ofSeq r.Messages |> List.ofSeq |> List.exists (fun m -> m.Code = expected),
            sprintf "Expected code %A among diagnostics %A" expected (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Code)))

    [<Fact>]
    let ``match covering every variant is accepted`` () =
        let src = """
union Shape Circle(r: number) Square(s: number) end
func area(sh)
  match sh do
    case Circle(r) then
      return r
    case Square(s) then
      return s
  end
  return 0
end"""
        expectSuccess src

    [<Fact>]
    let ``match missing a variant is rejected with MatchNotExhaustive`` () =
        let src = """
union Shape Circle(r: number) Square(s: number) end
func area(sh)
  match sh do
    case Circle(r) then
      return r
  end
  return 0
end"""
        expectCode MessageCode.MatchNotExhaustive src

    [<Fact>]
    let ``match expression missing a variant is rejected`` () =
        let src = """
union Shape Circle(r: number) Square(s: number) end
func describe(sh)
  return match sh do
    case Circle(r) then r
  end
end"""
        expectCode MessageCode.MatchNotExhaustive src

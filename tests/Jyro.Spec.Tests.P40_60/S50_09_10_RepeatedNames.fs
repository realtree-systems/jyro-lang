/// 50.10 Unions: a variant that declares two fields of one name (JYRO-REQ-1683,
/// DuplicateFieldName). 50.9 Match: a case that gives one name at two of its positions
/// (JYRO-REQ-1684, MatchDuplicateBinding). Both under JYRO-ADR-136, for JYRO-DEF-530 and
/// JYRO-DEF-531. Written from the requirements alone.
///
/// The two codes are reached by name through `MessageCode`, by reflection, so that this
/// project compiles on a build that does not declare them yet (FLOORS 4.1.7). Their numbers
/// (3610, 3611) are JYRO-REQ-1685's and JYRO-REQ-1686's, tested in P100_170; a build without
/// the member reports FLOORS-BLOCKED by the requirement that defines the member.
module RealTree.Jyro.Spec.Tests.P40_60.S50_09_10_RepeatedNames

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

/// The member `name` of `MessageCode`, or the test fails as blocked by `definer`.
let private codeNamed (definer: string) (name: string) : MessageCode =
    match Enum.TryParse<MessageCode>(name, false) with
    | true, c when Enum.GetName(typeof<MessageCode>, c) = name -> c
    | _ -> Pending.notInBuild definer (sprintf "MessageCode.%s" name)

let private duplicateFieldName () = codeNamed "JYRO-REQ-1685" "DuplicateFieldName"
let private matchDuplicateBinding () = codeNamed "JYRO-REQ-1686" "MatchDuplicateBinding"

/// Rejected before anything runs, every Error of `code`, every Error of stage Validate.
let private validationRejects (code: MessageCode) (source: string) : unit =
    rejectedAt code "Validate" source |> ignore

// ================================================================================================
// JYRO-REQ-1683 (50.10): a variant that declares two fields of the same name
// ================================================================================================

/// The requirement's own examples, and the same rule in other shapes: a third field, a hint on
/// either or both, the name three times, the repeat in a later variant of the union, and in a
/// later union.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1683")>]
[<InlineData("union U A(x, x) end")>]
[<InlineData("union U A(x, x: number) end")>]
[<InlineData("union U A(x: number, x) end")>]
[<InlineData("union U A(x: string, x: number) end")>]
[<InlineData("union U A(x: any, x: any) end")>]
[<InlineData("union U A(x, y, x) end")>]
[<InlineData("union U A(x, x, x) end")>]
[<InlineData("union U A(a, b, c, b) end")>]
[<InlineData("union U A B(y, y) end")>]
[<InlineData("union U A(x) B(y, z, y) C end")>]
[<InlineData("union U A end\nunion V B(q, q) end")>]
[<InlineData("union U\n  A(\n    x,\n    x\n  )\nend")>]
let ``JYRO-REQ-1683 a variant with two fields of one name is rejected with DuplicateFieldName`` (source: string) =
    validationRejects (duplicateFieldName ()) source

/// A script that uses the union beside the declaration is still refused, before it runs.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1683")>]
[<InlineData("union U A(x, x) end\nOutput.r = 1")>]
[<InlineData("union U A(x, x: number) end\nOutput.r = A(1, 2)")>]
let ``JYRO-REQ-1683 a script declaring a variant with a repeated field does not run`` (source: string) =
    let code = duplicateFieldName ()
    let r = rejectedBeforeRun source
    Assert.Contains(code, Script.errorCodes r)

/// Names that differ only in case are different names; a type hint is not part of the name.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1683")>]
[<InlineData("union U A(x, X) end")>]
[<InlineData("union U A(x: number, X: number) end")>]
[<InlineData("union U A(value, Value, VALUE) end")>]
[<InlineData("union U A(x, y) end")>]
[<InlineData("union U A(x, x1, x_) end")>]
let ``JYRO-REQ-1683 fields whose names differ, if only in case, are accepted`` (source: string) =
    let code = duplicateFieldName ()
    notRejectedWith code source
    compiles source

/// The rule is per variant: two variants may each have a field of one name.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1683")>]
[<InlineData("union U A(x) B(x) end")>]
[<InlineData("union U A(x, y) B(y, x) end")>]
[<InlineData("union U A(x) end\nunion V B(x) end")>]
let ``JYRO-REQ-1683 one field name in two variants is accepted`` (source: string) =
    let code = duplicateFieldName ()
    notRejectedWith code source
    compiles source

/// The accepted example `union U A(x, X) end` holds both fields, each with its own argument.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1683")>]
let ``JYRO-REQ-1683 the fields x and X are two fields, each holding its argument`` () =
    duplicateFieldName () |> ignore
    assertRStr "{\"_variant\":\"A\",\"x\":1,\"X\":2}" "union U A(x, X) end\nOutput.r = ToJson(A(1, 2))"
    assertR "[1, 2]" "union U A(x, X) end\nOutput.r = match A(1, 2) do case A(p, q) then [p, q] end"

// ================================================================================================
// JYRO-REQ-1684 (50.9): a match case that gives one name at two of its positions
// ================================================================================================

let private az = "union U A(x, z) end\nvar v = A(1, 2)\n"

/// The requirement's own examples, in the statement and the expression form.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1684")>]
[<InlineData("match v do case A(y, y) then Output.r = y end")>]
[<InlineData("match v do case A(_, _) then Output.r = 0 end")>]
[<InlineData("Output.r = match v do case A(y, y) then y end")>]
[<InlineData("Output.r = match v do case A(_, _) then 0 end")>]
[<InlineData("match v do\n  case A(y, y) then\n    Output.r = 1\nend")>]
let ``JYRO-REQ-1684 a case giving one name twice is rejected with MatchDuplicateBinding`` (stmt: string) =
    validationRejects (matchDuplicateBinding ()) (az + stmt)

/// The same rule in other shapes: a field's own name repeated, three positions, one case of
/// several, a match inside a function body, and a match expression nested in a case.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1684")>]
[<InlineData("union U A(x, z) end\nvar v = A(1, 2)\nmatch v do case A(x, x) then Output.r = x end")>]
[<InlineData("union U A(x, z) end\nvar v = A(1, 2)\nmatch v do case A(z, z) then Output.r = z end")>]
[<InlineData("union U A(a, b, c) end\nvar v = A(1, 2, 3)\nmatch v do case A(p, q, p) then Output.r = p end")>]
[<InlineData("union U A(a, b, c) end\nvar v = A(1, 2, 3)\nmatch v do case A(p, p, p) then Output.r = p end")>]
[<InlineData("union U A(a, b, c) end\nvar v = A(1, 2, 3)\nmatch v do case A(_, q, _) then Output.r = q end")>]
[<InlineData("union U A(x) B(x, y) C end\nvar v = C()\nmatch v do\n  case A(p) then Output.r = 1\n  case B(q, q) then Output.r = 2\n  case C then Output.r = 3\nend")>]
[<InlineData("union U A(x, z) end\nfunc F(v)\n  return match v do case A(y, y) then y end\nend\nOutput.r = F(A(1, 2))")>]
[<InlineData("union U A(x, z) end\nfunc F(v)\n  match v do\n    case A(y, y) then return y\n  end\n  return 0\nend\nOutput.r = F(A(1, 2))")>]
[<InlineData("union U A(x, z) end\nvar v = A(A(1, 2), 3)\nOutput.r = match v do case A(p, q) then (match p do case A(s, s) then s end) end")>]
let ``JYRO-REQ-1684 one name at two positions of a case is rejected with MatchDuplicateBinding in every shape`` (source: string) =
    validationRejects (matchDuplicateBinding ()) source

/// A script whose match has a repeated name is refused before it runs.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1684")>]
let ``JYRO-REQ-1684 a script whose case repeats a name does not run`` () =
    let code = matchDuplicateBinding ()
    let r = rejectedBeforeRun (az + "match v do case A(y, y) then Output.r = y end")
    Assert.Contains(code, Script.errorCodes r)

/// The requirement's accepted examples, which differ only in case or reuse the names of the
/// fields at other positions, and other distinct names, `_` among them once.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1684")>]
[<InlineData("match v do case A(y, Y) then Output.r = [y, Y] end", "[1, 2]")>]
[<InlineData("match v do case A(z, x) then Output.r = [z, x] end", "[1, 2]")>]
[<InlineData("Output.r = match v do case A(y, Y) then [y, Y] end", "[1, 2]")>]
[<InlineData("Output.r = match v do case A(z, x) then [z, x] end", "[1, 2]")>]
[<InlineData("match v do case A(_, y) then Output.r = [_, y] end", "[1, 2]")>]
[<InlineData("match v do case A(x, z) then Output.r = [x, z] end", "[1, 2]")>]
let ``JYRO-REQ-1684 a case whose names all differ, if only in case, is accepted and binds by position`` (stmt: string, expected: string) =
    let code = matchDuplicateBinding ()
    notRejectedWith code (az + stmt)
    compiles (az + stmt)
    assertR expected (az + stmt)

/// The rule is per case: two cases may each bind one name.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1684")>]
let ``JYRO-REQ-1684 one name in two cases is accepted`` () =
    let code = matchDuplicateBinding ()
    let src = "union U A(x) B(x, y) end\nvar v = B(1, 2)\nmatch v do\n  case A(y) then Output.r = y\n  case B(y, w) then Output.r = [y, w]\nend"
    notRejectedWith code src
    compiles src
    assertR "[1, 2]" src

// ================================================================================================
// A repeated name beside another Validate fault of the same declaration (JYRO-ADR-115 point 4)
// ================================================================================================

/// Refused before it runs, every Error of stage Validate, and each of `expected` among them.
let private rejectedWithAll (expected: MessageCode list) (source: string) : JyroResult<JyroValue> =
    let r = rejectedBeforeRun source
    assertAllStages "Validate" r
    let codes = Script.errorCodes r
    for code in expected do
        Assert.Contains(code, codes)
    r

/// JYRO-DEF-563: a union whose name repeats an earlier union's is rejected with
/// DuplicateUnionName (JYRO-REQ-240), and a variant of it that declares two fields of one name
/// is still rejected with DuplicateFieldName (JYRO-REQ-1683, which makes no exception).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1683")>]
[<InlineData("union U A end\nunion U B(x, x) end")>]
[<InlineData("union U A(x) end\nunion U B(x, y, x) end")>]
[<InlineData("union U A end\nunion U B C(q, q) end")>]
[<InlineData("union U A(x, x) end\nunion U B(y, y) end")>]
[<InlineData("union U A end\nunion U B(x, x) end\nOutput.r = 1")>]
let ``JYRO-DEF-563 a repeated field in a union whose name repeats an earlier union gives DuplicateFieldName and DuplicateUnionName`` (source: string) =
    let field = duplicateFieldName ()
    rejectedWithAll [ field; MessageCode.DuplicateUnionName ] source |> ignore

/// JYRO-DEF-563: the repeated field of the first union and that of the repeated union are both
/// reported, one DuplicateFieldName each.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1683")>]
let ``JYRO-DEF-563 a repeated field in each of two unions of one name gives a DuplicateFieldName for each`` () =
    let field = duplicateFieldName ()
    let r = rejectedWithAll [ field; MessageCode.DuplicateUnionName ] "union U A(x, x) end\nunion U B(y, y) end"
    let n = Script.errorCodes r |> Seq.filter (fun c -> c = field) |> Seq.length
    Assert.Equal(2, n)

/// JYRO-DEF-564: a case that names a variant an earlier case already named is rejected with
/// MatchDuplicateCase (JYRO-REQ-232), and when its count is right and it gives one name twice it
/// is still rejected with MatchDuplicateBinding (JYRO-REQ-1684), in the statement and the
/// expression form.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1684")>]
[<InlineData("match v do\n  case A(p, q) then Output.r = 1\n  case A(y, y) then Output.r = 2\nend")>]
[<InlineData("Output.r = match v do\n  case A(p, q) then 1\n  case A(y, y) then 2\nend")>]
[<InlineData("match v do\n  case A(y, y) then Output.r = 1\n  case A(w, w) then Output.r = 2\nend")>]
[<InlineData("Output.r = match v do\n  case A(p, q) then 1\n  case A(_, _) then 2\nend")>]
let ``JYRO-DEF-564 a repeated case that gives one name twice gives MatchDuplicateCase and MatchDuplicateBinding`` (stmt: string) =
    let binding = matchDuplicateBinding ()
    rejectedWithAll [ MessageCode.MatchDuplicateCase; binding ] (az + stmt) |> ignore

/// JYRO-DEF-564, in a function body and in a union of several variants.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1684")>]
[<InlineData("union U A(x, z) end\nfunc F(v)\n  return match v do\n    case A(p, q) then 1\n    case A(y, y) then y\n  end\nend\nOutput.r = F(A(1, 2))")>]
[<InlineData("union U A(x) B(x, y) end\nvar v = A(1)\nmatch v do\n  case A(p) then Output.r = 1\n  case B(q, w) then Output.r = 2\n  case B(s, s) then Output.r = 3\nend")>]
let ``JYRO-DEF-564 a repeated case giving one name twice is rejected with both codes in every shape`` (source: string) =
    let binding = matchDuplicateBinding ()
    rejectedWithAll [ MessageCode.MatchDuplicateCase; binding ] source |> ignore

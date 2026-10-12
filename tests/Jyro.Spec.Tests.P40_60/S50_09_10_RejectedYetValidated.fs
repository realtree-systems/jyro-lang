/// A union declaration or a match case that one Validate check rejects is still checked by every
/// other Validate check of it (JYRO-ADR-115 point 4; JYRO-REQ-1485, the host gets every
/// validation error in one result). For JYRO-DEF-565 (a union whose name repeats an earlier
/// union's: JYRO-REQ-240 with JYRO-REQ-241, 242 and 264) and JYRO-DEF-566 (a match case rejected
/// for its variant or its count: JYRO-REQ-229, 230, 232 and 233 with the validation of its names
/// and its body). Written from the requirements and the defects alone.
module RealTree.Jyro.Spec.Tests.P40_60.S50_09_10_RejectedYetValidated

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

/// Refused before it runs, every Error of stage Validate, and each of `expected` among them.
let private rejectedWithAll (expected: MessageCode list) (source: string) : JyroResult<JyroValue> =
    let r = rejectedBeforeRun source
    assertAllStages "Validate" r
    let codes = Script.errorCodes r
    for code in expected do
        if not (List.contains code codes) then
            Values.fail (sprintf "Expected %A among the errors. Diagnostics:\n%s\nSource:\n%s" code (Script.describe r) source)
    r

/// The number of Errors of `code` in `r`.
let private count (code: MessageCode) (r: JyroResult<'T>) : int =
    Script.errorCodes r |> List.filter (fun c -> c = code) |> List.length

// ================================================================================================
// JYRO-DEF-565: a union whose name repeats an earlier union's
// ================================================================================================

/// A variant named `Input` or `Output` in the repeated union is still ReservedName (JYRO-REQ-264).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-264")>]
[<InlineData("union U A end\nunion U Input end")>]
[<InlineData("union U A end\nunion U Output end")>]
[<InlineData("union U A end\nunion U Input(x) end")>]
[<InlineData("union U A end\nunion U B Output(y) end")>]
[<InlineData("union U A end\nunion U B end\nunion U Input end")>]
[<InlineData("union U A end\nunion U Input end\nOutput.r = 1")>]
let ``JYRO-DEF-565 a reserved variant name in a repeated union gives ReservedName and DuplicateUnionName`` (source: string) =
    rejectedWithAll [ MessageCode.DuplicateUnionName; MessageCode.ReservedName ] source |> ignore

/// A field named `_variant` in the repeated union is still ReservedFieldName (JYRO-REQ-1972).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1972")>]
[<InlineData("union U A end\nunion U B(_variant) end")>]
[<InlineData("union U A end\nunion U B(x, _variant) end")>]
[<InlineData("union U A end\nunion U B(_variant: string) end")>]
[<InlineData("union U A end\nunion U B C(_variant) end")>]
[<InlineData("union U A(x) end\nunion U B end\nunion U C(_variant) end")>]
let ``JYRO-DEF-565 a field named _variant in a repeated union gives ReservedFieldName and DuplicateUnionName`` (source: string) =
    rejectedWithAll [ MessageCode.DuplicateUnionName; MessageCode.ReservedFieldName ] source |> ignore

/// A variant name of the repeated union that another variant already has, in that union, in the
/// earlier union of its name, or in another union, is still DuplicateVariantName (JYRO-REQ-241).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1689")>]
[<InlineData("union U A end\nunion U B B end")>]
[<InlineData("union U A end\nunion U A end")>]
[<InlineData("union U A(x) end\nunion U A(y) end")>]
[<InlineData("union U A end\nunion U B(x) C B end")>]
[<InlineData("union V Q end\nunion U A end\nunion U Q end")>]
[<InlineData("union U A end\nunion U B end\nunion U A end")>]
let ``JYRO-DEF-565 a repeated variant name in a repeated union gives DuplicateVariantName and DuplicateUnionName`` (source: string) =
    rejectedWithAll [ MessageCode.DuplicateUnionName; MessageCode.DuplicateVariantName ] source |> ignore

/// Every fault of one repeated union is reported together: a reserved variant name, a field
/// named `_variant` and a repeated variant name, beside DuplicateUnionName.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1485")>]
let ``JYRO-DEF-565 every fault of a repeated union is reported with DuplicateUnionName`` () =
    rejectedWithAll
        [ MessageCode.DuplicateUnionName
          MessageCode.ReservedName
          MessageCode.ReservedFieldName
          MessageCode.DuplicateVariantName ]
        "union U A end\nunion U Input B(_variant) B end"
    |> ignore

/// A faulty repeated union beside a use of its variants and a match on them is refused before it
/// runs, with its codes, not with a failure of the compiler.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("union U A end\nunion U B(_variant) end\nvar v = B(1)\nmatch v do case B(p) then Output.r = p end", "ReservedFieldName")>]
[<InlineData("union U A end\nunion U Input end\nvar v = A()\nOutput.r = match v do case A then 1 end", "ReservedName")>]
[<InlineData("union U A end\nunion U A end\nvar v = A()\nmatch v do case A then Output.r = 1 end", "DuplicateVariantName")>]
let ``JYRO-DEF-565 a script using a faulty repeated union is refused with each code`` (source: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    rejectedWithAll [ MessageCode.DuplicateUnionName; c ] source |> ignore

// ================================================================================================
// JYRO-DEF-566: a match case rejected for its variant or its count
// ================================================================================================

let private ab = "union U A(x) B end\nvar v = A(1)\n"

/// A case rejected with MatchDuplicateCase (a variant an earlier case named) still has its body
/// validated and its names checked, in the statement and the expression form.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("match v do\n  case A(p) then Output.r = 1\n  case A(q) then Output.r = zzz\nend", "UndeclaredVariable")>]
[<InlineData("Output.r = match v do\n  case A(p) then 1\n  case A(q) then zzz\nend", "UndeclaredVariable")>]
[<InlineData("match v do\n  case A(p) then Output.r = 1\n  case A(Input) then Output.r = 2\nend", "ReservedName")>]
[<InlineData("Output.r = match v do\n  case A(p) then 1\n  case A(Output) then 2\nend", "ReservedName")>]
[<InlineData("match v do\n  case A(p) then Output.r = 1\n  case A(q) then var s: string\nend", "TypedVariableNeedsInitializer")>]
[<InlineData("match v do\n  case B then Output.r = 1\n  case B then Output.r = zzz\nend", "UndeclaredVariable")>]
let ``JYRO-DEF-566 a duplicate case is still validated: MatchDuplicateCase and the fault in it`` (stmt: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    rejectedWithAll [ MessageCode.MatchDuplicateCase; c ] (ab + stmt) |> ignore

/// A case rejected with MatchBindingCountMismatch still has its body validated and its names
/// checked (JYRO-REQ-264 for `Input` and `Output` as match bindings).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-264")>]
[<InlineData("match v do case A(Input, y) then Output.r = 1 end", "ReservedName")>]
[<InlineData("match v do case A(p, Output) then Output.r = 1 end", "ReservedName")>]
[<InlineData("Output.r = match v do case A(Input, y) then 1 end", "ReservedName")>]
[<InlineData("match v do case A(p, q) then Output.r = zzz end", "UndeclaredVariable")>]
[<InlineData("Output.r = match v do case A(p, q) then zzz end", "UndeclaredVariable")>]
[<InlineData("match v do case A then Output.r = zzz end", "UndeclaredVariable")>]
[<InlineData("match v do case B(p) then var s: string end", "TypedVariableNeedsInitializer")>]
let ``JYRO-DEF-566 a case with the wrong count is still validated: MatchBindingCountMismatch and the fault in it`` (stmt: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    rejectedWithAll [ MessageCode.MatchBindingCountMismatch; c ] (ab + stmt) |> ignore

/// A case rejected with MatchUnknownVariant still has its body validated and its names checked.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("match v do case Z(p) then Output.r = zzz end", "UndeclaredVariable")>]
[<InlineData("Output.r = match v do case Z(p) then zzz end", "UndeclaredVariable")>]
[<InlineData("match v do case Z(Input) then Output.r = 1 end", "ReservedName")>]
[<InlineData("Output.r = match v do case Z(Output) then 1 end", "ReservedName")>]
[<InlineData("match v do case Z then var s: string end", "TypedVariableNeedsInitializer")>]
let ``JYRO-DEF-566 a case naming an unknown variant is still validated: MatchUnknownVariant and the fault in it`` (stmt: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    rejectedWithAll [ MessageCode.MatchUnknownVariant; c ] (ab + stmt) |> ignore

/// A `case default` of a match expression (JYRO-REQ-229: in a match statement it is a parse
/// error) rejected with MatchWithDefault still has its body validated: an undeclared name in it,
/// and a match nested in it that gives one name twice (JYRO-REQ-1684).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("union U A(x) B end\nvar v = A(1)\nOutput.r = match v do\n  case A(p) then 1\n  case default then zzz\nend", "UndeclaredVariable")>]
[<InlineData("union U A(x) end\nvar v = A(1)\nOutput.r = match v do case A(p) then 1 case default then zzz end", "UndeclaredVariable")>]
[<InlineData("union U A(x) end\nvar v = A(1)\nOutput.r = match v do case A(p) then 1 case default then [1, zzz] end", "UndeclaredVariable")>]
[<InlineData("union U A(x, z) end\nvar v = A(1, 2)\nOutput.r = match v do case A(p, q) then 1 case default then (match v do case A(w, w) then w end) end", "MatchDuplicateBinding")>]
let ``JYRO-DEF-566 a default case is still validated: MatchWithDefault and the fault in it`` (source: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    rejectedWithAll [ MessageCode.MatchWithDefault; c ] source |> ignore

/// A match nested in a rejected case's body is validated too: its own repeated name is
/// MatchDuplicateBinding (JYRO-REQ-1684).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("union U A(x, z) end\nvar v = A(1, 2)\nmatch v do\n  case A(p, q) then Output.r = 1\n  case A(s, t) then\n    match s do case A(w, w) then Output.r = w end\nend", "MatchDuplicateCase")>]
[<InlineData("union U A(x, z) end\nvar v = A(1, 2)\nOutput.r = match v do case A(s) then (match s do case A(w, w) then w end) end", "MatchBindingCountMismatch")>]
let ``JYRO-DEF-566 a match nested in a rejected case is validated: MatchDuplicateBinding beside the case's code`` (source: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    rejectedWithAll [ c; MessageCode.MatchDuplicateBinding ] source |> ignore

/// The rejected case in a function body is validated too.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("union U A(x) end\nfunc F(v)\n  match v do\n    case A(p) then return 1\n    case A(q) then return zzz\n  end\n  return 0\nend\nOutput.r = F(A(1))", "MatchDuplicateCase", "UndeclaredVariable")>]
[<InlineData("union U A(x) end\nfunc F(v)\n  return match v do case A(Input, y) then 1 end\nend\nOutput.r = F(A(1))", "MatchBindingCountMismatch", "ReservedName")>]
let ``JYRO-DEF-566 a rejected case in a function body is validated`` (source: string, caseCode: string, bodyCode: string) =
    let a = System.Enum.Parse<MessageCode>(caseCode)
    let b = System.Enum.Parse<MessageCode>(bodyCode)
    rejectedWithAll [ a; b ] source |> ignore

/// A rejected case's names are bound in its body as written, whatever its variant or count: the
/// body using them is not UndeclaredVariable.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1485")>]
[<InlineData("match v do\n  case A(p) then Output.r = 1\n  case A(q) then Output.r = q\nend", "MatchDuplicateCase")>]
[<InlineData("Output.r = match v do\n  case A(p) then 1\n  case A(q) then q\nend", "MatchDuplicateCase")>]
[<InlineData("match v do case A(p, q) then Output.r = p + q end", "MatchBindingCountMismatch")>]
[<InlineData("Output.r = match v do case A(p, q) then [p, q] end", "MatchBindingCountMismatch")>]
[<InlineData("match v do case A(p) then Output.r = p case Z(q) then Output.r = q end", "MatchUnknownVariant")>]
[<InlineData("Output.r = match v do case A(p) then p case Z(q, w) then [q, w] end", "MatchUnknownVariant")>]
let ``JYRO-DEF-566 a rejected case binds its names as written`` (stmt: string, code: string) =
    let c = System.Enum.Parse<MessageCode>(code)
    let r = rejectedWithAll [ c ] ("union U A(x) end\nvar v = A(1)\n" + stmt)
    Assert.Equal(0, count MessageCode.UndeclaredVariable r)

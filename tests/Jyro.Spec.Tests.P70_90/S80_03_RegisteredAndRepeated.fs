/// 80.3: a function declaration whose name is both a registered function's (JYRO-REQ-292,
/// FunctionOverride) and an earlier declaration's (JYRO-REQ-291, DuplicateFunction) is rejected
/// with both codes, in a library as in a script (JYRO-ADR-115 point 4: one Validate check does not
/// hide another; JYRO-REQ-1485). For JYRO-DEF-567. Written from the requirements and the defect
/// alone.
module RealTree.Jyro.Spec.Tests.S80_03_RegisteredAndRepeated

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// Run `source` under `options` with a host function `Mark()` called first, and assert it was
/// refused before it ran, every Error of stage Validate, with each of `expected` among them.
let private rejectedWithAll (expected: MessageCode list) (options: RunOptions) (source: string) =
    let mark = Host.recorder "Mark"
    let r = Script.runWith { options with Functions = options.Functions @ [ mark.AsFunction ] } ("Mark()\n" + source)
    let errors = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> List.ofSeq
    if r.IsSuccess || List.isEmpty errors then
        Values.fail (sprintf "Expected the script to be refused. Diagnostics:\n%s" (Script.describe r))
    Assert.Equal(0, mark.CallCount)
    let stages = errors |> List.map (fun m -> m.Code, Pending.stageOf m)
    if stages |> List.exists (fun (_, s) -> s <> "Validate") then
        Values.fail (sprintf "Expected every error to have the stage Validate; the errors have %A. Diagnostics:\n%s"
                        stages (Script.describe r))
    let codes = Script.errorCodes r
    for code in expected do
        if not (List.contains code codes) then
            Values.fail (sprintf "Expected %A among the errors. Diagnostics:\n%s" code (Script.describe r))
    r

let private count (code: MessageCode) (r: JyroResult<'T>) : int =
    Script.errorCodes r |> List.filter (fun c -> c = code) |> List.length

let private both = [ MessageCode.FunctionOverride; MessageCode.DuplicateFunction ]

let private length0 = "func Length(x)\n  return 0\nend"
let private length1 = "func Length(x)\n  return 1\nend"

/// Two libraries that each declare a function named after a standard library function: each
/// declaration is FunctionOverride, and the second is also DuplicateFunction.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``JYRO-DEF-567 two libraries declaring a registered name give FunctionOverride for each and DuplicateFunction`` () =
    let r = rejectedWithAll both { Script.defaults with Libraries = [ length0; length1 ] } "Output.r = 1"
    Assert.Equal(2, count MessageCode.FunctionOverride r)
    Assert.Equal(1, count MessageCode.DuplicateFunction r)

/// The same in one library that declares the registered name twice.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``JYRO-DEF-567 one library declaring a registered name twice gives FunctionOverride and DuplicateFunction`` () =
    let r = rejectedWithAll both { Script.defaults with Libraries = [ length0 + "\n" + length1 ] } "Output.r = 1"
    Assert.Equal(2, count MessageCode.FunctionOverride r)
    Assert.Equal(1, count MessageCode.DuplicateFunction r)

/// Three declarations across three libraries: three FunctionOverride, and DuplicateFunction for
/// the second and the third.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``JYRO-DEF-567 three libraries declaring a registered name give DuplicateFunction for each later one`` () =
    let r = rejectedWithAll both { Script.defaults with Libraries = [ length0; length1; length0 ] } "Output.r = 1"
    Assert.Equal(3, count MessageCode.FunctionOverride r)
    Assert.Equal(2, count MessageCode.DuplicateFunction r)

/// Without the standard library the inventory name is still FunctionOverride (JYRO-REQ-292), and
/// the second declaration still DuplicateFunction.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-292")>]
let ``JYRO-DEF-567 without the standard library two libraries declaring an inventory name give both codes`` () =
    rejectedWithAll both { Script.defaults with Stdlib = false; Libraries = [ length0; length1 ] } "Output.r = 1" |> ignore

/// A registered host function's name declared in two libraries gives both codes.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-292")>]
let ``JYRO-DEF-567 two libraries declaring a host function's name give both codes`` () =
    let hosted = Host.recorder "Hosted"
    let decl = "func Hosted()\n  return 1\nend"
    rejectedWithAll both { Script.defaults with Functions = [ hosted.AsFunction ]; Libraries = [ decl; decl ] } "Output.r = 1"
    |> ignore

/// A library's repeated registered name is reported beside a script that calls it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1485")>]
let ``JYRO-DEF-567 a script calling the name two libraries declare is refused with both codes`` () =
    rejectedWithAll both { Script.defaults with Libraries = [ length0; length1 ] } "Output.r = Length([1, 2])" |> ignore

/// The script's own path, for comparison: a registered name declared twice in the script gives
/// both codes (the behavior the library path must match).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``JYRO-DEF-567 a script declaring a registered name twice gives both codes`` () =
    let r = rejectedWithAll both Script.defaults (length0 + "\n" + length1)
    Assert.Equal(1, count MessageCode.DuplicateFunction r)

/// A name that is not registered, declared in two libraries, is DuplicateFunction and not
/// FunctionOverride: the codes stay independent.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-291")>]
let ``JYRO-DEF-567 two libraries declaring an unregistered name give DuplicateFunction only`` () =
    let decl = "func Helper()\n  return 1\nend"
    let r = rejectedWithAll [ MessageCode.DuplicateFunction ] { Script.defaults with Libraries = [ decl; decl ] } "Output.r = 1"
    Assert.Equal(0, count MessageCode.FunctionOverride r)

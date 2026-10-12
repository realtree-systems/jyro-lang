/// jyro-spec 93.4 Round: JYRO-DEF-761, a mode that is a cyclic value.
module RealTree.Jyro.Spec.Tests.P93_94.S93_04_RoundCyclicMode

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

/// Declares cyclic values: `ca` holds itself, `co` holds itself, `ci.p = [{q: ci}]`.
let private cyclesDecl =
    "var ca = [1]\nca[0] = ca\nvar co = {}\nco.self = co\nvar ci = {}\nci.p = [{q: ci}]\n"

[<Theory>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<InlineData("ci")>]
[<InlineData("ci.p")>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 JYRO-DEF-761 Round_WithACyclicMode_RaisesInvalidRoundingMode`` (mode: string) =
    let r = Script.run (cyclesDecl + sprintf "Output.r = Round(2.5, 0, %s)" mode)
    Script.expectError MessageCode.InvalidRoundingMode r

[<Theory>]
[<InlineData("ca")>]
[<InlineData("co")>]
[<Trait("requirement", "JYRO-REQ-895")>]
let ``JYRO-REQ-895 JYRO-DEF-761 Round_WithACyclicModeByName_RaisesInvalidRoundingMode`` (mode: string) =
    let r = Script.run (cyclesDecl + sprintf "Output.r = Round(value: 2.5, decimals: 0, mode: %s)" mode)
    Script.expectError MessageCode.InvalidRoundingMode r

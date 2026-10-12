/// 50.1 and 50.2: a variable that shadows a hinted variable of the same name is a different
/// variable, and has no hint of its own unless it is declared with one (JYRO-DEF-762).
module RealTree.Jyro.Spec.Tests.P40_60.S50_01_ShadowedHints

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 AssignmentToAShadowingVariable_IsNotCoercedToTheOuterHint`` () =
    assertR "2" "var x: string = 'a'\nif true then\n  var x = 1\n  x = 2\n  Output.r = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 AssignmentToAShadowingVariable_IsNotRefusedByTheOuterHint`` () =
    assertR "\"b\"" "var x: number = 1\nif true then\n  var x = 'a'\n  x = 'b'\n  Output.r = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 CompoundAssignmentToAShadowingVariable_IsNotCoercedToTheOuterHint`` () =
    assertR "3" "var x: string = 'a'\nif true then\n  var x = 1\n  x += 2\n  Output.r = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 IncrementOfAShadowingVariable_IsNotCoercedToTheOuterHint`` () =
    assertR "2" "var x: string = 'a'\nif true then\n  var x = 1\n  x++\n  Output.r = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 AssignmentToAForeachVariableShadowingAHintedVariable_IsNotCoerced`` () =
    assertR "5" "var x: string = 'a'\nforeach x in [1] do\n  x = 5\n  Output.r = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 AssignmentToAShadowingVariableInAFunction_IsNotCoercedToTheOuterHint`` () =
    assertR "7" "var x: string = 'a'\nfunc F()\n  var x = 1\n  x = 7\n  return x\nend\nOutput.r = F()"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 ShadowingVariableWithItsOwnHint_IsCoercedToItsOwnHintOnly`` () =
    assertR "\"2\"" "var x: number = 1\nif true then\n  var x: string = 'a'\n  x = 2\n  Output.r = x\nend"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-193")>]
let ``JYRO-REQ-193 JYRO-DEF-762 TheOuterVariable_KeepsItsHintAfterTheScopeOfTheShadowEnds`` () =
    assertR "\"2\"" "var x: string = 'a'\nif true then\n  var x = 1\n  x = 2\nend\nx = 2\nOutput.r = x"

/// 10.6.3 - an inexact number is an IEEE 754-2019 binary64 double, with
/// roundTiesToEven. Every run sets the four execution limits explicitly
/// (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.P30.S10_06_Binary64

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

// Inexact operands are made in the script by `* Power(1, 0.5)` (93.7.5 gives an
// inexact 1), so the product is the double of the literal (30.2.10). The earlier
// second way, adding the literal 1e-29, rests on whether 1e-29 and the sum are
// held exactly (30.2.6, JYRO-REQ-49, and 30.2.11, JYRO-REQ-54, of another work
// order), not on binary64, so it no longer decides a row of 10.6.3.

/// The ways of making a literal inexact.
let makers : seq<obj[]> = [ [| box "times_power" |] ]

/// The literal `lit` made inexact the way `maker` names.
let private inexactOf (maker: string) (lit: string) : string =
    ignore maker
    sprintf "(%s * Power(1, 0.5))" lit

/// Run `source` two ways: compiled from source, and compiled to an artifact
/// then loaded from its bytes (150.6.2). Returns the Output of each.
let private bothPaths (source: string) : (string * JyroValue) list =
    let direct = Script.output (Script.runWith limited source)
    let bytes = Script.compileBytesWith limited source
    if not bytes.IsSuccess then
        Values.fail (sprintf "CompileToArtifact failed. Diagnostics:\n%s" (Script.describe bytes))
    let loaded = Script.output (Script.runBytesWith limited (Script.valueOf bytes))
    [ "source", direct; "artifact", loaded ]

[<Theory>]
[<MemberData(nameof makers)>]
[<Trait("requirement", "JYRO-REQ-11")>]
let ``JYRO-REQ-11 InexactSum_Of0_1And0_2_HasTheStringForm0_30000000000000004`` (maker: string) =
    let a = inexactOf maker "0.1"
    let b = inexactOf maker "0.2"
    let source =
        sprintf "var a = %s\nvar b = %s\nOutput.a = a\nOutput.sa = ToString(a)\nOutput.v = a + b\nOutput.s = ToString(a + b)\nOutput.c = (a + b) & \"\"" a b
    for path, out in bothPaths source do
        // The inexact 0.1 is the double nearest 0.1.
        assertInexact 0.1 (Values.prop "a" out)
        Assert.True("0.1" = Values.asString (Values.prop "sa" out), sprintf "%s: ToString(a) gave %s" path (Values.asString (Values.prop "sa" out)))
        // The sum is the binary64 sum, 0x3FD3333333333334.
        assertInexact (0.1 + 0.2) (Values.prop "v" out)
        Assert.Equal(0x3FD3333333333334L, bits (Values.asFloat (Values.prop "v" out)))
        Assert.True("0.30000000000000004" = Values.asString (Values.prop "s" out), sprintf "%s: ToString gave %s" path (Values.asString (Values.prop "s" out)))
        Assert.Equal("0.30000000000000004", Values.asString (Values.prop "c" out))

[<Theory>]
[<MemberData(nameof makers)>]
[<Trait("requirement", "JYRO-REQ-11")>]
let ``JYRO-REQ-11 InexactAddition_RoundsTiesToEven`` (maker: string) =
    let big = inexactOf maker "9007199254740992"
    let source =
        sprintf "var big = %s\nOutput.b = big\nOutput.p1 = big + 1\nOutput.s1 = ToString(big + 1)\nOutput.p3 = big + 3\nOutput.s3 = ToString(big + 3)\nOutput.p5 = big + 5\nOutput.s5 = ToString(big + 5)" big
    for _, out in bothPaths source do
        assertInexact 9007199254740992.0 (Values.prop "b" out)
        // 2^53 + 1 is halfway between 2^53 and 2^53 + 2: the even one is 2^53.
        // (Ties away from zero would give ...994.)
        assertInexact 9007199254740992.0 (Values.prop "p1" out)
        Assert.Equal("9007199254740992", Values.asString (Values.prop "s1" out))
        // 2^53 + 3 is halfway between 2^53 + 2 and 2^53 + 4: the even one is 2^53 + 4.
        assertInexact 9007199254740996.0 (Values.prop "p3" out)
        Assert.Equal("9007199254740996", Values.asString (Values.prop "s3" out))
        // 2^53 + 5 is halfway between 2^53 + 4 and 2^53 + 6: the even one is 2^53 + 4.
        // (Ties away from zero would give ...998.)
        assertInexact 9007199254740996.0 (Values.prop "p5" out)
        Assert.Equal("9007199254740996", Values.asString (Values.prop "s5" out))

[<Theory>]
[<MemberData(nameof makers)>]
[<Trait("requirement", "JYRO-REQ-11")>]
let ``JYRO-REQ-11 InexactSum_IsNotEqualToExact0_3`` (maker: string) =
    let a = inexactOf maker "0.1"
    let b = inexactOf maker "0.2"
    let source =
        sprintf "Output.eq = %s + %s == 0.3\nOutput.ne = %s + %s != 0.3\nOutput.one = %s == 0.1" a b a b a
    for _, out in bothPaths source do
        Assert.False(Values.asBool (Values.prop "eq" out), "the binary64 sum is not the double of 0.3")
        Assert.True(Values.asBool (Values.prop "ne" out))
        Assert.True(Values.asBool (Values.prop "one" out), "the inexact 0.1 is the double of 0.1")

module RealTree.Jyro.Tests.TargetEvaluation

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.Runner

// Four defects that a grammar sweep found past the parser, in the stages that
// give a parsed form its meaning:
//
// (1) A compound assignment compiled its target into both the read and the
//     write, so `a[i++] += 5` stepped `i` twice and `G(o).k += 1` called `G`
//     twice. `exit` inside a function compiled its message twice in the same
//     way. Each part of a target is now evaluated once.
//
// (2) `func F(a = -1)` was refused as "not a literal", because the parser
//     reads `-1` as Negate applied to the literal 1.
//
// (3) A union field named `_variant` overwrote the tag that every constructed
//     variant carries, so the value no longer matched its own variant.
//
// (4) `delete a[0]` passed the validator and then failed with "Cannot set
//     property", a message about a write the script never made.

let private output (src: string) = expectSuccess src (JyroObject() :> JyroValue)

let private codes (src: string) = expectFailure src (JyroObject() :> JyroValue)

// -- (1) Each part of a target is evaluated once --------------------------------

[<Fact>]
let ``compound index assignment evaluates the index once`` () =
    let out = output "var a = [0, 0, 0]\nvar i = 0\na[i++] += 5\nOutput.a = a\nOutput.i = i"
    Assert.Equal(1.0, out |> prop "i" |> asNumber)
    Assert.Equal("[5,0,0]", (out |> prop "a").ToJson())

[<Fact>]
let ``compound property assignment evaluates the object once`` () =
    let src =
        "func G(c, o) c.n += 1 return o end\n" +
        "var c = {n: 0}\nvar o = {k: 1}\n" +
        "G(c, o).k += 1\n" +
        "Output.n = c.n\nOutput.k = o.k"
    let out = output src
    Assert.Equal(1.0, out |> prop "n" |> asNumber)
    Assert.Equal(2.0, out |> prop "k" |> asNumber)

[<Fact>]
let ``compound index assignment evaluates the object once`` () =
    let src =
        "func G(c, a) c.n += 1 return a end\n" +
        "var c = {n: 0}\nvar a = [1]\n" +
        "G(c, a)[0] *= 3\n" +
        "Output.n = c.n\nOutput.v = a[0]"
    let out = output src
    Assert.Equal(1.0, out |> prop "n" |> asNumber)
    Assert.Equal(3.0, out |> prop "v" |> asNumber)

[<Fact>]
let ``exit inside a function evaluates its message once`` () =
    let src =
        "func Next(c) c.n += 1 return \"bye\" end\n" +
        "func F(c) exit Next(c) end\n" +
        "var c = {n: 0}\nOutput.c = c\nF(c)"
    let out = output src
    Assert.Equal(1.0, out |> prop "c" |> prop "n" |> asNumber)

// -- (2) A negative number is a literal default ---------------------------------

[<Fact>]
let ``a negative number is accepted as a parameter default`` () =
    let out = output "func F(a = -1) return a end\nOutput.x = F()"
    Assert.Equal(-1.0, out |> prop "x" |> asNumber)

[<Fact>]
let ``a negated variable is still not a literal default`` () =
    Assert.Contains(MessageCode.DefaultValueNotLiteral, codes "var x = 1\nfunc F(a = -x) return a end")

// -- (3) `_variant` is not a field name -----------------------------------------

[<Fact>]
let ``a variant field named _variant is refused`` () =
    Assert.Contains(MessageCode.ReservedFieldName, codes "union U C(_variant: string) end\nOutput.x = C(\"z\")")

[<Fact>]
let ``a library variant field named _variant is refused`` () =
    let r = JyroLibrary.Parse("union U C(_variant) end")
    Assert.False(r.IsSuccess)
    Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.ReservedFieldName)

// -- (4) delete on a value with no properties -----------------------------------

[<Fact>]
let ``delete of an array element reports DeleteOnNonObject`` () =
    Assert.Equal<MessageCode list>([ MessageCode.DeleteOnNonObject ], codes "var a = [1, 2]\ndelete a[0]")

[<Fact>]
let ``delete by string index on an object still works`` () =
    let out = output "var o = {k: 1, m: 2}\ndelete o[\"k\"]\nOutput.o = o"
    Assert.Equal("{\"m\":2}", (out |> prop "o").ToJson())

module RealTree.Jyro.Tests.OutputBinding

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// `Output` is an ordinary readable identifier bound to the script's result,
/// so every assignment form that works on a variable works on it. The forms
/// that rebind it - plain assignment, compound assignment, increment - must
/// also push the new binding back onto the execution context, because `exit`
/// returns `ctx.Output` rather than the compiled body's local parameter.

let private outputOf (src: string) = Runner.expectSuccess src (JyroObject() :> JyroValue)

[<Theory>]
[<InlineData("Output = 5\nOutput += 1", 6.0)>]
[<InlineData("Output = 5\nOutput -= 2", 3.0)>]
[<InlineData("Output = 5\nOutput *= 3", 15.0)>]
[<InlineData("Output = 6\nOutput /= 2", 3.0)>]
[<InlineData("Output = 7\nOutput %= 4", 3.0)>]
[<InlineData("Output = 5\nOutput++", 6.0)>]
[<InlineData("Output = 5\nOutput--", 4.0)>]
let ``compound assignment and increment rebind Output`` (src: string, expected: float) =
    Assert.Equal(expected, Runner.asNumber (outputOf src))

[<Fact>]
let ``concat assignment rebinds Output`` () =
    Assert.Equal("ab", Runner.asString (outputOf "Output = 'a'\nOutput &= 'b'"))

[<Fact>]
let ``compound assignment agrees with the desugared form`` () =
    // The rule these forms have to obey: `Output op= v` is `Output = Output op v`.
    // The desugared form always worked; the sugar used to reach a `failwith`.
    Assert.Equal(
        Runner.asNumber (outputOf "Output = 5\nOutput = Output + 1"),
        Runner.asNumber (outputOf "Output = 5\nOutput += 1"))

[<Fact>]
let ``prefix increment on Output yields the new value`` () =
    Assert.Equal(6.0, Runner.asNumber (outputOf "Output = 5\nvar k = ++Output\nOutput = k"))

[<Fact>]
let ``postfix increment on Output yields the old value`` () =
    Assert.Equal(5.0, Runner.asNumber (outputOf "Output = 5\nvar k = Output++\nOutput = k"))

[<Fact>]
let ``a rebound Output survives exit`` () =
    // `exit` returns `ctx.Output`, so a form that rebinds the local parameter
    // without syncing the context would silently lose the assignment here.
    Assert.Equal("ab", Runner.asString (outputOf "Output = 'a'\nOutput &= 'b'\nexit"))

[<Fact>]
let ``incrementing Output while it holds an object is a runtime type error`` () =
    // Accepting the form is not the same as promising it works on every value:
    // Output starts as an object, and object + 1 has no meaning. The point is
    // that it fails as a typed runtime error rather than an internal compiler
    // error.
    let codes = Runner.expectFailure "Output++" (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.UnsupportedBinaryOperation, codes)
    Assert.DoesNotContain(MessageCode.UnknownCompilerError, codes)

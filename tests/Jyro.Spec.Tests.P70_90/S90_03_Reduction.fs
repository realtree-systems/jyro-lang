/// jyro-spec 90.3.7: only the bounds of its table are reduced to the length of
/// the operand. From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S90_03_Reduction

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// Run `Output.r = <call>` then `Output.after = 1` under `bounded` (4.1.11).
let private runCall (call: string) =
    Script.runWith bounded (lines [ "Output.r = " + call; "Output.after = 1" ])

let private assertGives (call: string) (expectedJson: string) =
    let v = outProp "r" (runCall call)
    Assert.Equal(Values.toJson (Values.json expectedJson), Values.toJson v)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1977")>]
[<InlineData("Slice([1, 2, 3], 0, 1000)", "[1,2,3]")>]           // 92.8.4
[<InlineData("Slice([1, 2, 3], 0, 4)", "[1,2,3]")>]              // 92.8.4, just above
[<InlineData("Slice([1, 2, 3], 1, 2147483647)", "[2,3]")>]       // 92.8.4
[<InlineData("Slice([1, 2, 3], 4)", "[]")>]                      // 92.8.6, the book's example
[<InlineData("Slice([1, 2, 3], 4, 10)", "[]")>]                  // 92.8.6
[<InlineData("Slice([1, 2, 3], 2147483647)", "[]")>]             // 92.8.6
[<InlineData("Substring(\"abc\", 1, 10)", "\"bc\"")>]            // 91.10.4, the book's example
[<InlineData("Substring(\"abc\", 1, 3)", "\"bc\"")>]             // 91.10.4, just above
[<InlineData("Substring(\"abc\", 0, 2147483647)", "\"abc\"")>]   // 91.10.4
let ``JYRO-REQ-1977 Reduction_TheThreeCasesOfTheTable`` (call: string, expected: string) =
    assertGives call expected

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1977")>]
[<InlineData("Substring(\"abc\", 4)")>]          // 91.10.6, the book's example
[<InlineData("Substring(\"abc\", 4, 10)")>]      // 91.10.6
[<InlineData("Insert([1, 2, 3], 4, 0)")>]        // 92.15.4: Length(items) + 1
[<InlineData("Insert([], 1, 0)")>]               // 92.15.4
[<InlineData("Insert([1, 2, 3], -1, 0)")>]       // 92.15.4: below 0, not reduced to 0
[<InlineData("RemoveAt([1], 1)")>]               // 92.16.3: Length(items)
[<InlineData("RemoveAt([1, 2, 3], 3)")>]         // 92.16.3
[<InlineData("RemoveAt([1, 2, 3], -1)")>]        // 92.16.3: below 0
let ``JYRO-REQ-1977 Position_NotInTheTable_IsCheckedNotReduced`` (call: string) =
    let r = runCall call
    expectRuntimeError MessageCode.IndexOutOfRange r
    match Script.value r with
    | Some v when (v :? JyroObject) ->
        Assert.False(hasKey "r" v, sprintf "%s gave a result" call)
        Assert.False(hasKey "after" v, "the script went on after the rejected call")
    | _ -> ()

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1977")>]
[<InlineData("Substring(\"abc\", 3)", "\"\"")>]          // 91.10.5: start = Length(text) gives ""
[<InlineData("Insert([1, 2, 3], 3, 0)", "[1,2,3,0]")>]   // 92.15.3: index = Length(items)
[<InlineData("RemoveAt([1, 2, 3], 2)", "[1,2]")>]        // 92.16.3: Length(items) - 1
let ``JYRO-REQ-1977 Position_AtTheLastPermittedValue_IsAccepted`` (call: string, expected: string) =
    assertGives call expected

/// JYRO-REQ-2171 (160.7.12): when seven codes of the table are raised is left to the implementation, and
/// UnknownCompilerError and RuntimeError have one condition each beyond it. The clause allows an
/// implementation to raise these codes under conditions of its own, so what can be shown is the
/// negative side: a condition for which another clause gives a code never gives one of these, and the
/// conditions that the clause names do give theirs. Written from the book alone (FLOORS 4.1.1).
module RealTree.Jyro.Spec.Tests.S160_07_CodeConditions

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// The seven codes that no clause gives a condition for: UnknownParserError (2000), ValueIsReadOnly (5005),
/// InvalidCast (5102), ArgumentNotProvided (5103), UndefinedFunctionRuntime (5500), InternalStageFailure
/// (6002) and ArtifactUnwritable (7008), by their numbers.
let private sevenCodes : int list = [ 2000; 5005; 5102; 5103; 5500; 6002; 7008 ]

/// UnknownCompilerError (6000) and RuntimeError (5002), by their numbers.
let private internalCodes : int list = [ 6000; 5002 ]

/// Conditions for which another clause of the book gives the code named: a source and that code.
let private conditions : (string * string * MessageCode) list =
    [ "division by zero (40.3.1)", "Output.x = 1 / 0", MessageCode.DivisionByZero
      "modulo by zero", "Output.x = 5 % 0", MessageCode.ModuloByZero
      "an index out of range", "var a = [1, 2]\na[5] = 9", MessageCode.IndexOutOfRange
      "fail", "fail \"stop\"", MessageCode.ScriptFailure
      "an undeclared variable", "Output.x = missing", MessageCode.UndeclaredVariable
      "an undefined function", "Output.x = NoSuchFunction(1)", MessageCode.UndefinedFunction
      "a typed variable with no initial value", "var s: string", MessageCode.TypedVariableNeedsInitializer
      "an unterminated string", "Output.x = \"abc", MessageCode.UnterminatedString
      // StringArrayOrObjectRequired, 5706 (JYRO-REQ-1702), by its number: the member is not in every build.
      "a number where a string, an array or an object is required", "Output.x = Length(5)", enum<MessageCode> 5706
      "a wrong argument type", "Output.x = ToUpper([1])", MessageCode.ArgumentTypeMismatch ]

let conditionNames : obj[] seq = conditions |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof conditionNames)>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 a condition that another clause gives a code is never given one of the seven codes, UnknownCompilerError or RuntimeError`` (name: string) =
    let _, source, expected = conditions |> List.find (fun (n, _, _) -> n = name)
    let r = Script.runWith pinned source
    let codes = Script.codes r
    Assert.False(r.IsSuccess, sprintf "%s: the run succeeded" name)
    Assert.True(List.contains expected codes, sprintf "%s: no %A. Diagnostics:\n%s" name expected (Script.describe r))
    for code in sevenCodes @ internalCodes do
        Assert.False(codes |> List.exists (fun c -> int c = code), sprintf "%s: the code %d was raised. Diagnostics:\n%s" name code (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 reporting RuntimeError for 1 / 0 fails 40.3.1, which requires DivisionByZero`` () =
    let r = Script.runWith pinned "Output.x = 1 / 0"
    Script.expectError MessageCode.DivisionByZero r
    Assert.DoesNotContain(MessageCode.RuntimeError, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 a property write to a string is SetPropertyOnNonObject and an index write to a string is SetIndexOnNonContainer`` () =
    Script.expectError MessageCode.SetPropertyOnNonObject (Script.runWith pinned "var s = \"abc\"\ns.x = 1")
    Script.expectError MessageCode.SetIndexOnNonContainer (Script.runWith pinned "var s = \"abc\"\ns[0] = \"z\"")
    // Their numbers, 5306 and 5307, are the ones of the table (160.7.12).
    Assert.Equal(5306, int MessageCode.SetPropertyOnNonObject)
    Assert.Equal(5307, int MessageCode.SetIndexOnNonContainer)

/// A host function that never throws: a compilation with it gives no UnknownCompilerError.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 UnknownCompilerError is not raised for a compilation in which no member of a host function throws`` () =
    let calm = Host.recorder "Calm"
    for source in [ "Output.x = Calm(1)"; "Output.x = Missing(1)"; "var s: string"; "Output.x = \"abc"; "Output.x = 1 +" ] do
        let r = Script.compileWith { pinned with Functions = [ calm.AsFunction ] } source
        Assert.DoesNotContain(MessageCode.UnknownCompilerError, Script.codes r)
        Assert.DoesNotContain(MessageCode.InternalStageFailure, Script.codes r)
        let artifact = Script.compileBytesWith { pinned with Functions = [ calm.AsFunction ] } source
        Assert.DoesNotContain(MessageCode.UnknownCompilerError, Script.codes artifact)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 UnknownCompilerError is raised for a member of a host function that throws during compilation`` () =
    // The one condition the clause names (JYRO-REQ-2140 gives its detail).
    let f = Tests.CallCheck.checking "Faulty" (fun _ _ -> raise (InvalidOperationException "the host's check failed"))
    let r = Script.compileWith { pinned with Functions = [ f.AsFunction ] } "Output.x = Faulty(1)"
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.Contains(Tests.CallCheck.unknownCompilerError, Script.codes r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 the artifact codes are given only when an artifact is read or written, never by a compilation or a run of source`` () =
    let artifactCodes = [ 7000 .. 7008 ]
    for _, source, _ in conditions do
        let codes = Script.codes (Script.runWith pinned source) @ Script.codes (Script.compileBytesWith pinned source)
        for code in artifactCodes do
            Assert.False(codes |> List.exists (fun c -> int c = code), sprintf "%s: the artifact code %d was raised by source" source code)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2171")>]
let ``JYRO-REQ-2171 RuntimeError is raised for Replace with an empty oldValue`` () =
    // The one condition JYRO-REQ-1736 gives (91.x Replace); an implementation MAY raise it elsewhere.
    let r = Script.runWith pinned "Output.x = Replace(\"abc\", \"\", \"x\")"
    Assert.False(r.IsSuccess, Script.describe r)
    Assert.Contains(MessageCode.RuntimeError, Script.codes r)

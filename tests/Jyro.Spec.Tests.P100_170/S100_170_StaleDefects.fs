/// Faults found in the stale-defect sweeps that belong to Parts 100 to 170:
/// JYRO-DEF-756 (the Compile time of CompileToArtifact), JYRO-DEF-757 (the limits a host leaves unset)
/// and JYRO-DEF-758 (GetDecimal at 2^96).
module RealTree.Jyro.Spec.Tests.S100_170_StaleDefects

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

// -- 110.1.1: CompileToArtifact gives the time of the Compile stage (JYRO-DEF-756) ----------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1452")>]
let ``JYRO-REQ-1452 JYRO-DEF-756 CompileToArtifact_WithStats_GivesACompileTime`` () =
    let o, s = withStats Script.defaults
    Script.compileBytesWith o (manyStatements 800) |> Script.valueOf |> ignore
    Assert.True(s.CompileTime > TimeSpan.Zero, sprintf "Compile time is %O" s.CompileTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1452")>]
let ``JYRO-REQ-1452 JYRO-DEF-756 CompileToArtifact_WithStats_GivesParseValidateAndLinkTimesToo`` () =
    let o, s = withStats Script.defaults
    Script.compileBytesWith o (manyStatements 800) |> Script.valueOf |> ignore
    for name, t in stageTimes s |> List.filter (fun (n, _) -> List.contains n [ "Parse"; "Validate"; "Link" ]) do
        Assert.True(t > TimeSpan.Zero, sprintf "%s time is %O" name t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1455")>]
let ``JYRO-REQ-1455 JYRO-DEF-756 CompileToArtifact_WithStats_LeavesExecuteAndDeserializeAtZero`` () =
    let o, s = withStats Script.defaults
    Script.compileBytesWith o (manyStatements 800) |> Script.valueOf |> ignore
    Assert.Equal(TimeSpan.Zero, s.ExecuteTime)
    Assert.Equal(TimeSpan.Zero, s.DeserializeTime)

// -- 100.1.2: a limit the host does not set is its default (JYRO-DEF-757) ----------------------

/// What a C# host gets from `new JyroExecutionOptions { MaxExecutionTime = ... }`, and what a
/// configuration binder makes before it sets the keys the section holds: the parameterless
/// constructor, then the setter of the one limit the host set.
let private hostOptionsSettingOnlyTime () : JyroExecutionOptions =
    let t = typeof<JyroExecutionOptions>
    let o = Activator.CreateInstance(t) :?> JyroExecutionOptions
    t.GetProperty("MaxExecutionTime").SetValue(box o, box (TimeSpan.FromSeconds 2.))
    o

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``JYRO-REQ-1405 JYRO-DEF-757 OptionsMadeByTheParameterlessConstructor_GiveTheDefaultsForTheLimitsNotSet`` () =
    // The defaults apply when a run uses the options, so this asserts what a run
    // does, not what the property reads (the record holds the host's own values).
    let o = hostOptionsSettingOnlyTime ()
    Assert.Equal(TimeSpan.FromSeconds 2., o.MaxExecutionTime)
    let opts = { Script.defaults with Limits = Some o }
    // 10,001 statements pass the default statement limit of 10,000.
    let calls = 10_001 / 101
    let rest = 10_001 - calls * 101
    let body = [ "func F()" ] @ repeatLines 99 "  var a{i} = {i}" @ [ "  return 0"; "end" ]
    let statements = lines (body @ repeatLines calls "Output.c{i} = F()" @ repeatLines rest "Output.s{i} = {i}")
    Script.runWith opts statements |> fails MessageCode.StatementLimitExceeded
    // 1,001 iterations pass the default loop limit of 1,000.
    Script.runWith opts "var i = 0\nwhile i < 1001 do i = i + 1 end" |> fails MessageCode.LoopIterationLimitExceeded
    // A call depth of 65 passes the default of 64.
    Script.runWith opts (recursion 65) |> fails MessageCode.CallDepthLimitExceeded

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``JYRO-REQ-1405 JYRO-DEF-757 OptionsWithOnlyTheTimeSet_RunAScriptWithinTheDefaults`` () =
    let r = Script.runWith { Script.defaults with Limits = Some (hostOptionsSettingOnlyTime ()) } "Output.a = 1"
    Assert.True(r.IsSuccess, Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1405")>]
let ``JYRO-REQ-1405 JYRO-DEF-757 OptionsWithOnlyTheTimeSet_StopAtTheDefaultNumberOfLoopIterations`` () =
    let opts = { Script.defaults with Limits = Some (hostOptionsSettingOnlyTime ()) }
    Script.runWith opts "var i = 0\nwhile i < 1000 do i = i + 1 end" |> ok |> ignore
    Script.runWith opts "var i = 0\nwhile i < 1001 do i = i + 1 end" |> fails MessageCode.LoopIterationLimitExceeded

// -- 160.3: GetDecimal at the edge of decimal (JYRO-DEF-758) ----------------------------

/// A host function `Convert(amount)` that reads its argument through GetDecimal.
type private DecimalReader() =
    inherit JyroFunctionBase("Convert", Host.signature "Convert" [ Parameter.Required("amount", ParameterType.Number) ] ParameterType.Number)
    override _.Effect = FunctionEffect.Pure
    override this.Execute(args, _) = JyroNumber(args.GetDecimal(0)) :> JyroValue

[<Theory>]
[<InlineData("79228162514264337593543950336")>]
[<InlineData("-79228162514264337593543950336")>]
[<Trait("requirement", "JYRO-REQ-2054")>]
let ``JYRO-REQ-2054 JYRO-DEF-758 GetDecimal_OfAnInexactNumberJustPastDecimal_RaisesIntegerOutOfRange`` (literal: string) =
    let r = Script.runWith { pinned with Functions = [ DecimalReader() :> IJyroFunction ] } (sprintf "Output.a = 1\nOutput.b = Convert(%s)" literal)
    Assert.DoesNotContain(MessageCode.RuntimeError, Script.codes r)
    fails MessageCode.IntegerOutOfRange r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2099")>]
let ``JYRO-REQ-2099 JYRO-DEF-758 GetDecimal_OfTheGreatestDecimal_ReadsItWithNoLoss`` () =
    let r = Script.runWith { pinned with Functions = [ DecimalReader() :> IJyroFunction ] } "Output.b = Convert(79228162514264337593543950335)"
    let v = Script.valueOf r |> Values.prop "b"
    Assert.Equal(Decimal.MaxValue, Values.asDecimal v)

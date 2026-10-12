/// jyro-spec 94.1 Now, 94.2 Today. Clock functions: the tests pin the form and
/// bound the value by host clock readings taken just before and just after.
module RealTree.Jyro.Spec.Tests.P93_94.S94_01_02_NowToday

open System
open System.Globalization
open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P93_94.Support

let private instantForm = Regex(@"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$", RegexOptions.CultureInvariant)
let private dateForm = Regex(@"^\d{4}-\d{2}-\d{2}$", RegexOptions.CultureInvariant)

let private parseInstant (s: string) : DateTime =
    DateTime.ParseExact(s, "yyyy-MM-dd'T'HH:mm:ss.fff'Z'", CultureInfo.InvariantCulture,
                        DateTimeStyles.AdjustToUniversal ||| DateTimeStyles.AssumeUniversal)

let private truncMs (t: DateTime) : DateTime = DateTime(t.Ticks - t.Ticks % TimeSpan.TicksPerMillisecond, DateTimeKind.Utc)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1005")>]
let ``Now_TakesNoParameters`` () =
    expectCompileError MessageCode.TooManyArguments "Output.r = Now(1)"
    let r = Script.compile "Output.r = Now(x: 1)"
    Assert.False(r.IsSuccess, "Now(x: 1) must be rejected before execution")
    let codes = Script.errorCodes r
    Assert.True(List.contains MessageCode.UnknownNamedArgument codes || List.contains MessageCode.TooManyArguments codes,
                Script.describe r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1005")>]
let ``Now_ReturnsAString`` () =
    assertString "string" (eval "TypeOf(Now())")
    Assert.Equal(JyroValueType.String, (eval "Now()").ValueType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1006")>]
let ``Now_DiscardsTheSmallerUnits_NeverRounds`` () =
    let mutable before = DateTime.MinValue
    let failures = ResizeArray<string>()
    let mutable checkedCount = 0
    let mark =
        Host.func "HostMark" [] ParameterType.Any FunctionEffect.NonDeterministic (fun _ ->
            before <- DateTime.UtcNow
            Values.nul)
    let check =
        Host.func "HostCheck" [ Parameter.Required("s", ParameterType.Any) ] ParameterType.Any
            FunctionEffect.NonDeterministic (fun args ->
                let after = DateTime.UtcNow
                checkedCount <- checkedCount + 1
                let s = Values.asString args.Head
                if not (instantForm.IsMatch s) then
                    failures.Add(sprintf "%s is not in the form yyyy-MM-ddTHH:mm:ss.fffZ" s)
                else
                    let n = parseInstant s
                    let lo = truncMs before
                    let hi = truncMs after
                    if n < lo || n > hi then
                        failures.Add(sprintf "%s is outside [%s, %s]" s (lo.ToString("o")) (hi.ToString("o")))
                Values.nul)
    let r =
        Script.runWith
            { Script.defaults with
                Functions = [ mark.AsFunction; check.AsFunction ]
                Limits = Some bigLimits }
            "for i in 1 to 2000 do\n  HostMark()\n  HostCheck(Now())\nend"
    if not r.IsSuccess then Values.fail (Script.describe r)
    Assert.True(checkedCount >= 1999, sprintf "only %d readings checked" checkedCount)
    Assert.True(failures.Count = 0, String.Join("\n", failures |> Seq.truncate 10))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1007")>]
let ``ParseDate_OfNow_EqualsThatNow`` () =
    let src =
        "var bad = 0\nvar last = \"\"\nfor i in 1 to 100 do\n  var n = Now()\n  last = n\n  if ParseDate(n) != n then\n    bad += 1\n  end\nend\nOutput.r = [bad, last]"
    match evalScriptIn [] src |> Values.asList with
    | [ bad; last ] ->
        Assert.Equal(0.0, Values.asFloat bad)
        Assert.Matches(instantForm, Values.asString last)
    | other -> Values.fail (sprintf "Unexpected result %A" other)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1008")>]
let ``Today_TakesNoParameters`` () =
    expectCompileError MessageCode.TooManyArguments "Output.r = Today(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1008")>]
let ``Today_ReturnsAStringOfTheCurrentUtcDate`` () =
    assertString "string" (eval "TypeOf(Today())")
    let before = DateTime.UtcNow.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
    let t = eval "Today()"
    let after = DateTime.UtcNow.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
    Assert.Equal(JyroValueType.String, t.ValueType)
    let s = Values.asString t
    Assert.Matches(dateForm, s)
    Assert.True(s = before || s = after, sprintf "Today() gave %s; the host UTC date was %s / %s" s before after)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1009")>]
let ``ParseDate_OfToday_IsThatDateAtMidnightUtc`` () =
    match evalScriptIn [] "var t = Today()\nOutput.r = [t, ParseDate(t)]" |> Values.asList with
    | [ t; p ] -> Assert.Equal(Values.asString t + "T00:00:00.000Z", Values.asString p)
    | other -> Values.fail (sprintf "Unexpected result %A" other)

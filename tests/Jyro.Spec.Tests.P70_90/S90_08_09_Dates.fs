/// jyro-spec 90.8 (date and time) and 90.9 (determinism and the clock).
/// From the book and the public host API alone.
module RealTree.Jyro.Spec.Tests.S90_08_09_Dates

open System
open System.Globalization
open System.Text.RegularExpressions
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let rows (xs: 'a list) : obj[] seq = xs |> Seq.map (fun x -> [| box x |])

let private nowPattern = Regex(@"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$")
let private todayPattern = Regex(@"^\d{4}-\d{2}-\d{2}$")

let private truncMs (d: DateTime) = DateTime(d.Ticks - d.Ticks % TimeSpan.TicksPerMillisecond, DateTimeKind.Utc)

let private parseUtc (s: string) =
    DateTime.ParseExact(s, "yyyy-MM-dd'T'HH:mm:ss.fff'Z'", CultureInfo.InvariantCulture,
                        DateTimeStyles.AssumeUniversal ||| DateTimeStyles.AdjustToUniversal)

/// Run `source` with `Input.s` set to the host string `s`.
let runS (s: string) (source: string) = Script.runInput (Values.obj [ "s", Values.str s ]) source

/// Run with the host's time zone set to `zone` (Unix TZ), then restore it.
let inZone (zone: string) (work: unit -> unit) =
    let old = Environment.GetEnvironmentVariable "TZ"
    Environment.SetEnvironmentVariable("TZ", zone)
    TimeZoneInfo.ClearCachedData()
    try
        Assert.True(
            TimeZoneInfo.Local.Id = zone || TimeZoneInfo.Local.Id.EndsWith zone,
            sprintf "The test could not set the host time zone to %s (it is %s)" zone TimeZoneInfo.Local.Id)
        work ()
    finally
        Environment.SetEnvironmentVariable("TZ", old)
        TimeZoneInfo.ClearCachedData()

// ---------------------------------------------------------------------------
// 90.8.1 UTC everywhere
// ---------------------------------------------------------------------------

let assertNowIsCurrentUtc () =
    let before = truncMs DateTime.UtcNow
    let r = Script.run "Output.n = Now()"
    let after = DateTime.UtcNow
    let n = Values.asString (outProp "n" r)
    Assert.EndsWith("Z", n)
    let t = parseUtc n
    Assert.True(before <= t && t <= after, sprintf "Now() gave %s outside [%O, %O] (UTC)" n before after)

let assertTodayIsUtcDate () =
    let mutable ok = false
    let mutable attempts = 0
    while not ok && attempts < 3 do
        let before = DateTime.UtcNow.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        let today = Values.asString (outProp "t" (Script.run "Output.t = Today()"))
        let after = DateTime.UtcNow.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)
        attempts <- attempts + 1
        if before = after then
            Assert.Equal(before, today)
            ok <- true
    Assert.True(ok, "UTC midnight fell inside every attempt")

let assertOffsetReadAsUtc () =
    let r =
        Script.run (
            lines [ "var s = \"2024-01-01T23:30:00-02:00\""
                    "Output.p = ParseDate(s)"
                    "Output.day = DatePart(s, \"day\")"
                    "Output.hour = DatePart(s, \"hour\")"
                    "Output.add = DateAdd(s, 0, \"day\")"
                    "Output.fmt = FormatDate(s, \"yyyy-MM-dd HH:mm\")"
                    "Output.diff = DateDiff(\"2024-01-01T00:00:00+01:00\", \"2023-12-31T23:00:00Z\", \"second\")" ])
    Assert.Equal("2024-01-02T01:30:00.000Z", Values.asString (outProp "p" r))
    Assert.Equal(2.0, Values.asFloat (outProp "day" r))
    Assert.Equal(1.0, Values.asFloat (outProp "hour" r))
    Assert.Equal("2024-01-02T01:30:00.000Z", Values.asString (outProp "add" r))
    Assert.Equal("2024-01-02 01:30", Values.asString (outProp "fmt" r))
    Assert.Equal(0.0, Values.asFloat (outProp "diff" r))

let assertDaylightSavingIgnored () =
    let r = Script.run "Output.r = DateAdd(\"2024-03-10T01:00:00Z\", 1, \"day\")\nOutput.h = DateDiff(\"2024-03-10T01:00:00Z\", \"2024-03-11T01:00:00Z\", \"hour\")"
    Assert.Equal("2024-03-11T01:00:00.000Z", Values.asString (outProp "r" r))
    Assert.Equal(24.0, Values.asFloat (outProp "h" r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-354")>]
let ``Now_GivesTheCurrentTimeInUtc`` () = assertNowIsCurrentUtc ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-354")>]
let ``Today_GivesTheUtcDate`` () = assertTodayIsUtcDate ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-354")>]
let ``DateFunctions_ReadADateWithAnOffsetAsItsUtcInstant`` () = assertOffsetReadAsUtc ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-354")>]
let ``DateAdd_AcrossADaylightSavingChange_AddsExactly24Hours`` () = assertDaylightSavingIgnored ()

// ---------------------------------------------------------------------------
// 90.8.2 Now, 90.8.3 Today
// ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-355")>]
let ``Now_IsTheCurrentUtcTimeInTheFormWithThreeFractionDigits`` () = assertNowIsCurrentUtc ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-355")>]
let ``Now_OneThousandCalls_AllHaveTheForm`` () =
    let r =
        Script.runWith
            { Script.defaults with Limits = Some(Script.limits (fun o -> { o with MaxStatements = 1000000; MaxLoopIterations = 1000000 })) }
            (lines [ "var a = []"; "var i = 0"; "while i < 1000 do"; "  a = Append(a, Now())"; "  i = i + 1"; "end"; "Output.a = a" ])
    let all = Values.asList (outProp "a" r) |> List.map Values.asString
    Assert.Equal(1000, all.Length)
    for n in all do Assert.Matches(nowPattern, n)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-356")>]
let ``Today_IsTheUtcDateInTheFormYyyyMmDd`` () =
    let t = Values.asString (outProp "t" (Script.run "Output.t = Today()"))
    Assert.Matches(todayPattern, t)
    assertTodayIsUtcDate ()

// ---------------------------------------------------------------------------
// 90.8.4 No time zone parameter
// ---------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-357")>]
let ``StandardLibrary_HasNoFunctionWithATimeZoneParameter`` () =
    let zoneLike (p: string) =
        let q = p.ToLowerInvariant()
        q = "tz" || q.Contains "zone" || q.Contains "timezone" || q.Contains "offset" || q.Contains "utc"
    let found =
        [ for f in StdlibRegistry.getAll () do
              for p in f.Signature.Parameters do
                  if zoneLike p.Name then yield sprintf "%s(%s)" f.Name p.Name ]
    Assert.Empty(found)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-357")>]
[<InlineData("FormatDate(\"2024-01-02\", \"yyyy\", \"UTC\")")>]
[<InlineData("Now(\"Europe/Paris\")")>]
[<InlineData("Today(\"+02:00\")")>]
[<InlineData("DateAdd(\"2024-01-02\", 1, \"day\", \"+02:00\")")>]
[<InlineData("DatePart(\"2024-01-02\", \"hour\", \"UTC\")")>]
[<InlineData("ParseDate(\"2024-01-02\", \"UTC\")")>]
[<InlineData("DateDiff(\"2024-01-02\", \"2024-01-01\", \"day\", \"UTC\")")>]
let ``DateFunction_GivenAZoneAsAnExtraArgument_IsRejectedWithTooManyArguments`` (call: string) =
    Assert.Equal<MessageCode list>([ MessageCode.TooManyArguments ], Script.errorCodes (Script.compile ("Output.r = " + call)))

// ---------------------------------------------------------------------------
// 90.8.5 to 90.8.8 Reading a date from a string
// ---------------------------------------------------------------------------

let acceptedDates =
    [ "2024-01-02"; "2024-01-02T03:04:05Z"; "2024-01-02t03:04:05.123z"; "2024-01-02T03:04:05+05:30"
      "2024-01-02T03:04:05-00:00"; "2024-01-02T03:04:05"; "2024-01-02T03:04:05z"; "2024-01-02T03:04:05.1Z"
      "2024-01-02T03:04:05.1234567Z"; "2024-01-02T03:04:05.12345678901234567890Z"; "2024-06-30T23:59:60Z"
      "2024-01-02T03:04:05-23:59"; "2024-01-02T03:04:05+23:59"; "2024-01-02T03:04:05.5" ]

let rejectedDates =
    [ "2024-01-02 03:04:05Z"; "2024-01-02T03:04Z"; "2024-01-02T03:04:05.Z"; "2024-01-02Z"; "2024-01-02T24:00:00Z"
      "2024-01-02T03:60:00Z"; "2024-01-02T03:04:05+0530"; "2024-01-02T03:04:05+05"; "2024-01-02T03:04:05+24:00"
      "٢٠٢٤-01-02"; " 2024-01-02"; "2024-01-02 "; "2024-01-02T"; ""; "today"; "2024"; "2024-06"
      "20240601"; "2024-6-1"; "2024/06/01"; "01-06-2024"; "2024-06-01T12"; "2024-06-01T12:34:56 +02:00"
      "2024-06-01T12:34:56GMT"; "June 1, 2024"; "2024-06-01T12:34:56Z trailing"; "2024-02-30" ]

/// The calls, other than ParseDate, that read `Input.s` as a date.
let readerCalls =
    [ "FormatDate(Input.s, \"yyyy\")"; "DateAdd(Input.s, 0, \"day\")"; "DateDiff(Input.s, \"2024-01-01\", \"day\")"
      "DateDiff(\"2024-01-01\", Input.s, \"day\")"; "DatePart(Input.s, \"year\")" ]

let accepted () = rows acceptedDates
let rejected () = rows rejectedDates

[<Theory>]
[<Trait("requirement", "JYRO-REQ-358")>]
[<MemberData(nameof accepted)>]
let ``JYRO-REQ-358 DateString_MatchingTheGrammar_IsAcceptedByEveryReader`` (s: string) =
    let p = outProp "p" (runS s "Output.p = ParseDate(Input.s)")
    Assert.False(p.IsNull, sprintf "ParseDate(%A) gave null" s)
    for call in readerCalls do
        let r = runS s ("Output.r = " + call)
        Assert.True(r.IsSuccess, sprintf "%s with %A: %s" call s (Script.describe r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-358")>]
[<MemberData(nameof rejected)>]
let ``JYRO-REQ-358 DateString_NotMatchingTheGrammar_IsNotAccepted`` (s: string) =
    let p = outProp "p" (runS s "Output.p = ParseDate(Input.s)")
    Assert.True(p.IsNull, sprintf "ParseDate(%A) gave %s" s (Values.toJson p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-358")>]
[<InlineData("2024-01-02", "2024-01-02T00:00:00.000Z")>]
[<InlineData("2024-01-02T03:04:05+05:30", "2024-01-01T21:34:05.000Z")>]
[<InlineData("2024-01-02t03:04:05.123z", "2024-01-02T03:04:05.123Z")>]
[<InlineData("2024-01-02T03:04:05-00:00", "2024-01-02T03:04:05.000Z")>]
[<InlineData("2024-01-02T03:04:05Z", "2024-01-02T03:04:05.000Z")>]
let ``JYRO-REQ-358 DateString_Accepted_HasTheMeaningOfTheGrammarAndMinusZeroIsZero`` (s: string, expected: string) =
    Assert.Equal(expected, Values.asString (outProp "p" (runS s "Output.p = ParseDate(Input.s)")))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-359")>]
[<InlineData("2024-06-01T12:34:56", "2024-06-01T12:34:56.000Z")>]
[<InlineData("2024-06-01T12:34:56.789", "2024-06-01T12:34:56.789Z")>]
[<InlineData("2024-06-01t12:34:56", "2024-06-01T12:34:56.000Z")>]
let ``DateTime_WithNoZoneOrOffset_IsReadAsUtcByEveryReader`` (s: string, utc: string) =
    let r =
        runS s
            (lines [ "Output.p = ParseDate(Input.s)"
                     "Output.h = DatePart(Input.s, \"hour\")"
                     "Output.d = DateDiff(Input.s, Input.s & \"Z\", \"second\")"
                     "Output.a = DateAdd(Input.s, 0, \"day\")"
                     "Output.f = FormatDate(Input.s, \"HH:mm\")" ])
    Assert.Equal(utc, Values.asString (outProp "p" r))
    Assert.Equal(12.0, Values.asFloat (outProp "h" r))
    Assert.Equal(0.0, Values.asFloat (outProp "d" r))
    Assert.Equal(utc, Values.asString (outProp "a" r))
    Assert.Equal("12:34", Values.asString (outProp "f" r))

/// 90.8.7: ParseDate gives null for it (90.4.4), and every other reader fails
/// with a Jyro runtime error and no result. Which code each other reader
/// raises is its own clause's (94.4 to 94.7, DateFormatInvalid), not 90.8.7's.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-360")>]
[<MemberData(nameof rejected)>]
let ``JYRO-REQ-360 DateString_NotMatchingTheGrammar_IsUninterpretableForEveryReader`` (s: string) =
    let p = runS s "Output.p = ParseDate(Input.s)"
    Assert.True(p.IsSuccess, sprintf "ParseDate(%A) raised: %s" s (Script.describe p))
    Assert.True((outProp "p" p).IsNull, sprintf "ParseDate(%A) gave %s" s (Values.toJson (outProp "p" p)))
    for call in readerCalls do
        let r =
            try runS s (lines [ "Output.r = " + call; "Output.after = true" ])
            with e -> Values.fail (sprintf "%s with %A let a host exception escape: %s" call s (e.ToString()))
        Assert.False(r.IsSuccess, sprintf "%s interpreted %A" call s)
        assertFailedWithError r
        match Script.value r with
        | Some v when (v :? JyroObject) ->
            Assert.False(hasKey "r" v, sprintf "%s gave a result for %A" call s)
            Assert.False(hasKey "after" v)
        | _ -> ()

let private parts = [ "year"; "month"; "day"; "hour"; "minute"; "second"; "dayofweek"; "dayofyear" ]

/// The accepted strings with at most three fraction digits. A string with more
/// is accepted (90.8.5), but the instant that every reader gives it is one
/// millisecond's precision only under 94.3.10 (JYRO-REQ-1019): without that
/// rule, ParseDate's output form cannot hold the instant the other readers
/// read, so the comparison below would test 94.3.10, not 90.8.8.
let acceptedToTheMillisecond () =
    acceptedDates
    |> List.filter (fun s ->
        let m = Regex.Match(s, @"\.(\d+)")
        not m.Success || m.Groups.[1].Value.Length <= 3)
    |> rows

[<Theory>]
[<Trait("requirement", "JYRO-REQ-361")>]
[<MemberData(nameof acceptedToTheMillisecond)>]
let ``JYRO-REQ-361 DateString_Accepted_HasTheSameInstantInEveryReader`` (s: string) =
    let r =
        runS s
            (lines [ yield "var p = ParseDate(Input.s)"
                     yield "Output.p = p"
                     yield "Output.f = FormatDate(Input.s, \"yyyy-MM-ddTHH:mm:ss.fffZ\")"
                     yield "Output.a = DateAdd(Input.s, 0, \"second\")"
                     yield "Output.d1 = DateDiff(Input.s, p, \"second\")"
                     yield "Output.d2 = DateDiff(p, Input.s, \"second\")"
                     for part in parts do
                         yield sprintf "Output.%s = DatePart(Input.s, \"%s\") == DatePart(p, \"%s\")" part part part ])
    let p = Values.asString (outProp "p" r)
    Assert.Equal(p, Values.asString (outProp "f" r))
    Assert.Equal(p, Values.asString (outProp "a" r))
    Assert.Equal(0.0, Values.asFloat (outProp "d1" r))
    Assert.Equal(0.0, Values.asFloat (outProp "d2" r))
    for part in parts do Assert.True(Values.asBool (outProp part r), sprintf "DatePart %s differs" part)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-361")>]
[<MemberData(nameof rejected)>]
let ``JYRO-REQ-361 DateString_Rejected_IsRejectedByEveryReader`` (s: string) =
    Assert.True((outProp "p" (runS s "Output.p = ParseDate(Input.s)")).IsNull)
    for call in readerCalls do
        let r = runS s ("Output.r = " + call)
        Assert.False(r.IsSuccess, sprintf "%s accepted %A" call s)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-361")>]
let ``JYRO-REQ-361 DateString_WithMinusZeroOffset_IsTheSameInstantAsZInEveryReader`` () =
    let r =
        Script.run (
            lines [ "var m = \"2024-01-02T03:04:05-00:00\""
                    "var z = \"2024-01-02T03:04:05Z\""
                    "Output.p = ParseDate(m) == ParseDate(z)"
                    "Output.f = FormatDate(m, \"HH:mm:ss\") == FormatDate(z, \"HH:mm:ss\")"
                    "Output.a = DateAdd(m, 1, \"hour\") == DateAdd(z, 1, \"hour\")"
                    "Output.d = DateDiff(m, z, \"second\") == 0"
                    "Output.h = DatePart(m, \"hour\") == DatePart(z, \"hour\")" ])
    for k in [ "p"; "f"; "a"; "d"; "h" ] do Assert.True(Values.asBool (outProp k r), k)

// ---------------------------------------------------------------------------
// Host time zone independence (process-wide: the TZ variable)
// ---------------------------------------------------------------------------

[<Collection("ProcessWide")>]
type ``S90_08 Host time zone``() =

    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-354")>]
    [<InlineData("America/New_York")>]
    [<InlineData("Pacific/Chatham")>]
    member _.``DateFunctions_InANonUtcHostZone_StillGiveAndReadUtc``(zone: string) =
        inZone zone (fun () ->
            assertNowIsCurrentUtc ()
            assertTodayIsUtcDate ()
            assertOffsetReadAsUtc ()
            assertDaylightSavingIgnored ())

    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-356")>]
    [<InlineData("Pacific/Kiritimati")>]
    [<InlineData("Etc/GMT+12")>]
    member _.``Today_InAZoneWhoseDateDiffersFromUtc_IsStillTheUtcDate``(zone: string) =
        inZone zone (fun () ->
            let t = Values.asString (outProp "t" (Script.run "Output.t = Today()"))
            Assert.Matches(todayPattern, t)
            assertTodayIsUtcDate ())

    [<Theory>]
    [<Trait("requirement", "JYRO-REQ-359")>]
    [<InlineData("America/New_York")>]
    [<InlineData("Asia/Kolkata")>]
    member _.``DateTime_WithNoZoneOrOffset_IsUtcWhateverTheHostZone``(zone: string) =
        inZone zone (fun () ->
            let r =
                runS "2024-06-01T12:34:56"
                    "Output.p = ParseDate(Input.s)\nOutput.h = DatePart(Input.s, \"hour\")\nOutput.a = DateAdd(Input.s, 0, \"day\")"
            Assert.Equal("2024-06-01T12:34:56.000Z", Values.asString (outProp "p" r))
            Assert.Equal(12.0, Values.asFloat (outProp "h" r))
            Assert.Equal("2024-06-01T12:34:56.000Z", Values.asString (outProp "a" r)))

// ===========================================================================
// 90.9.1 Now and Today are NonDeterministic
// ===========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-362")>]
[<InlineData("Now")>]
[<InlineData("Today")>]
let ``ClockFunction_HasTheEffectNonDeterministic`` (name: string) =
    match stdlibLookup name with
    | None -> Values.fail (sprintf "%s is not in the standard library" name)
    | Some f -> Assert.Equal(FunctionEffect.NonDeterministic, f.Effect)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-362")>]
[<InlineData("Now")>]
[<InlineData("Today")>]
let ``ClockFunction_UnderAPureMaximum_IsRejectedWithFunctionEffectExceedsMax`` (name: string) =
    let pureR = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.Pure } (sprintf "Output.r = %s()" name)
    Assert.Contains(MessageCode.FunctionEffectExceedsMax, Script.errorCodes pureR)
    let nd = Script.compileWith { Script.defaults with MaxEffect = Some FunctionEffect.NonDeterministic } (sprintf "Output.r = %s()" name)
    assertCompiles nd

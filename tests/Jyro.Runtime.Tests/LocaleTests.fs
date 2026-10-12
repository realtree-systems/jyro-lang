module RealTree.Jyro.Tests.LocaleTests

open System.Globalization
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation

/// Regression tests: Jyro script output MUST be byte-for-byte identical
/// regardless of the host machine's CurrentCulture. Two failure classes are
/// covered:
///   * Numbers must format with a '.' decimal separator, never the German/
///     French ',' (which under Join would even collide with the separator).
///   * Dates must use the Gregorian calendar with invariant formatting - not
///     the th-TH Thai Buddhist year (2024 -> 2567), and ar-SA (Hijri calendar)
///     must still be able to parse an ISO date at all (it previously threw).
/// These run under deliberately hostile cultures. See the locale-formatting
/// audit finding.

/// Run `f` with CurrentCulture pinned to `name`, restoring it afterwards.
/// Jyro executes synchronously on the calling thread (single-thread
/// guarantee), so pinning the thread culture governs the whole run.
let private withCulture (name: string) (f: unit -> 'a) : 'a =
    let prev = CultureInfo.CurrentCulture
    try
        CultureInfo.CurrentCulture <- CultureInfo(name)
        f ()
    finally
        CultureInfo.CurrentCulture <- prev

let private run (script: string) : JyroResult<JyroValue> =
    Runner.compileThenRun
        (JyroCompiler().WithStdlib().WithSource(script))
        (fun r -> r.WithInputJson("{}"))

let private strProp (script: string) (prop: string) : string =
    let r = run script
    Assert.True(r.IsSuccess, sprintf "script failed: %A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    match r.TryValue with
    | Some v ->
        use doc = JsonDocument.Parse(v.ToJson())
        doc.RootElement.GetProperty(prop).GetString()
    | None -> failwith "no output"

let private numProp (script: string) (prop: string) : float =
    let r = run script
    Assert.True(r.IsSuccess, sprintf "script failed: %A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
    match r.TryValue with
    | Some v ->
        use doc = JsonDocument.Parse(v.ToJson())
        doc.RootElement.GetProperty(prop).GetDouble()
    | None -> failwith "no output"

// -- Number formatting (concat & Join) -----------------------------------

[<Theory>]
[<InlineData("en-US")>]
[<InlineData("de-DE")>]
[<InlineData("fr-FR")>]
let ``number concat uses invariant decimal point`` (culture: string) =
    withCulture culture (fun () ->
        Assert.Equal("v=5.5", strProp "Output.s = \"v=\" & 5.5" "s"))

[<Theory>]
[<InlineData("en-US")>]
[<InlineData("de-DE")>]
let ``Join renders numbers with invariant decimal point`` (culture: string) =
    // Under de-DE an uncultured ToString would emit "1,5;2,5"; worse, a ','
    // separator would collide with the decimal comma.
    withCulture culture (fun () ->
        Assert.Equal("1.5;2.5", strProp "Output.s = Join([1.5, 2.5], \";\")" "s"))

// -- Date parsing / formatting -------------------------------------------

[<Theory>]
[<InlineData("en-US")>]
[<InlineData("th-TH")>]
[<InlineData("ar-SA")>]
let ``ParseDate normalizes ISO date on the Gregorian calendar`` (culture: string) =
    // th-TH would yield year 1481 (Thai year misread); ar-SA previously failed
    // to parse entirely and raised a runtime error.
    withCulture culture (fun () ->
        Assert.Equal("2024-01-15T00:00:00.000Z", strProp "Output.d = ParseDate(\"2024-01-15\")" "d"))

[<Theory>]
[<InlineData("en-US")>]
[<InlineData("th-TH")>]
[<InlineData("ar-SA")>]
let ``FormatDate emits Gregorian year`` (culture: string) =
    // th-TH renders 2567, ar-SA renders 1445 without the invariant fix.
    withCulture culture (fun () ->
        Assert.Equal("2024-01-15", strProp "Output.d = FormatDate(\"2024-01-15\", \"yyyy-MM-dd\")" "d"))

[<Theory>]
[<InlineData("en-US")>]
[<InlineData("th-TH")>]
let ``DateAdd stays on the Gregorian calendar`` (culture: string) =
    withCulture culture (fun () ->
        Assert.Equal("2024-02-15T00:00:00.000Z", strProp "Output.d = DateAdd(\"2024-01-15\", 1, \"month\")" "d"))

[<Theory>]
[<InlineData("en-US")>]
[<InlineData("th-TH")>]
let ``DatePart year is Gregorian`` (culture: string) =
    withCulture culture (fun () ->
        Assert.Equal(2024.0, numProp "Output.y = DatePart(\"2024-01-15\", \"year\")" "y"))

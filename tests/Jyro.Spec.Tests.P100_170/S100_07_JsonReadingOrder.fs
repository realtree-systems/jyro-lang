/// Reading JSON text: where the nesting bound is decided against a malformation
/// (JYRO-REQ-1655, 100.7), `InvalidJson` from the host API for text that is not
/// JSON (JYRO-REQ-2113, 160.1), and the script's `FromJson` for text it cannot
/// interpret (JYRO-REQ-1658, 90.4.2) or that is nested too deep (JYRO-REQ-1659,
/// 95.13.12). Written from the book and the public host API alone (FLOORS 4.1.1).
///
/// Every door is given the same text. The doors: the script function
/// `FromJson`; `JyroRunner.WithInputJson(String)` and `JyroRunner.WithInputJson(Stream)`
/// (the UTF-8 bytes), then `Execute()`; `JyroScript.ExecuteScriptWithJson`; and
/// `JyroValue.FromJson(String)`, `JyroValue.FromJson(Stream)` and
/// `JyroValue.FromJsonAsync`. (`JyroValue.FromJson(String, JsonDocumentOptions)`
/// is removed by JYRO-ADR-106, so it is not a door.)
///
/// The failure channel of each host door: for the runner and the one-call
/// member, the diagnostics of the run's result (1657's example: "a run given
/// the text ... MUST fail with InvalidJson"); for the `JyroValue` members, which
/// return a value and have no result, an exception carrying the code. No clause
/// names the exception type of the `JyroValue` members, so any exception that
/// carries the code (codesOfExn) is accepted; the type is a query in the
/// journal of JYRO-WRK-1.
///
/// The texts that exceed the maximum value size (JYRO-REQ-1656, and 1658 (c)
/// and (d)) are in S100_08_FromJsonSize.fs, in the LargeValues collection.
module RealTree.Jyro.Spec.Tests.S100_07_JsonReadingOrder

open System
open System.IO
open System.Text
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// N, the maximum nesting depth of 100.7.1.
let private n () = maxDepth ()

let private nesting = MessageCode.ValueNestingLimitExceeded
let private invalid = MessageCode.InvalidJson

// -- The doors ----------------------------------------------------------------------------------------

/// What a door gave for a text.
type DoorOutcome =
    /// The door made a value: for the script's FromJson, the value FromJson
    /// returned; for a runner or one-call door, the run's Output; for a
    /// JyroValue member, the value it returned.
    | Made of JyroValue
    /// The door failed through the run's result, with these error codes.
    | ResultFailed of MessageCode list * string
    /// The door threw, with the codes the exception carries.
    | Threw of MessageCode list * string

let private describeOutcome (o: DoorOutcome) : string =
    match o with
    | Made v -> sprintf "made a value (%s)" (try v.ToJson() with _ -> "unprintable")
    | ResultFailed(_, d) -> sprintf "the result failed:\n%s" d
    | Threw(_, d) -> sprintf "threw %s" d

let private threw (e: exn) : DoorOutcome =
    Threw(codesOfExn e, sprintf "%s (codes %A): %s" (e.GetType().FullName) (codesOfExn e) e.Message)

let private ofResult (r: JyroResult<JyroValue>) : DoorOutcome =
    if r.IsSuccess then Made(Script.valueOf r) else ResultFailed(Script.errorCodes r, Script.describe r)

/// A program that reads nothing of Input, compiled with every limit set.
let private program = lazy (Script.compileWith pinned "Output.ok = true" |> Script.valueOf)

let private limits () = Script.limits allLimits

let private utf8 (text: string) : Stream = new MemoryStream(Encoding.UTF8.GetBytes text) :> Stream

/// The host doors of 1655 and 1657.
let hostDoors =
    [ "WithInputJson"; "WithInputJsonStream"; "ExecuteScriptWithJson"
      "JyroValue.FromJson"; "JyroValue.FromJsonStream"; "JyroValue.FromJsonAsync" ]

/// Every door of 1655: the script's FromJson and the host doors.
let allDoors = "FromJson" :: hostDoors

/// True for the doors whose failure channel is the run's result.
let private isRunDoor (door: string) = not (door.StartsWith "JyroValue." || door = "FromJson")

/// Give `text` to `door`.
let private through (door: string) (text: string) : DoorOutcome =
    try
        match door with
        | "FromJson" ->
            let r =
                Script.runWith
                    { pinned with Input = ValueInput(Values.obj [ "t", Values.str text ]) }
                    "Output.r = FromJson(Input.t)\nOutput.after = true"
            if r.IsSuccess then Made(Script.valueOf r |> Values.prop "r") else ResultFailed(Script.errorCodes r, Script.describe r)
        | "WithInputJson" -> ofResult (JyroRunner(program.Force()).WithExecutionOptions(limits ()).WithInputJson(text).Execute())
        | "WithInputJsonStream" ->
            ofResult (JyroRunner(program.Force()).WithExecutionOptions(limits ()).WithInputJson(utf8 text).Execute())
        | "ExecuteScriptWithJson" -> ofResult (JyroScript.executeScriptWithJson "Output.ok = true" text)
        | "JyroValue.FromJson" -> Made(JyroValue.FromJson text)
        | "JyroValue.FromJsonStream" -> Made(JyroValue.FromJson(utf8 text))
        | "JyroValue.FromJsonAsync" -> Made(JyroValue.FromJsonAsync(utf8 text, Threading.CancellationToken.None).GetAwaiter().GetResult())
        | other -> failwithf "unknown door %s" other
    with e -> threw e

/// Assert that `door` refused `text` through its own failure channel with
/// `code`, and not with `other`.
let private refuses (door: string) (code: MessageCode) (other: MessageCode) (text: string) : unit =
    let o = through door text
    let shown = if text.Length > 80 then text.Substring(0, 80) + "..." else text
    let wrong () =
        Values.fail (sprintf "%s of %A: expected %A (and not %A) through its own failure channel; it %s" door shown code other (describeOutcome o))
    match o with
    | ResultFailed(codes, _) when door = "FromJson" ->
        // The script's FromJson raises a runtime error: the run fails with it alone.
        if codes <> [ code ] then wrong ()
    | ResultFailed(codes, _) when isRunDoor door ->
        if not (List.contains code codes) || List.contains other codes then wrong ()
    | Threw(codes, _) when not (isRunDoor door) && door <> "FromJson" ->
        if not (List.contains code codes) || List.contains other codes then wrong ()
    | _ -> wrong ()

/// Assert that the script's FromJson returned null for `text` and raised nothing.
let private fromJsonNull (text: string) : unit =
    match through "FromJson" text with
    | Made v when v.IsNull -> ()
    | o -> Values.fail (sprintf "FromJson of %A: expected null and no error; the run %s" (if text.Length > 80 then text.Substring(0, 80) + "..." else text) (describeOutcome o))

/// Assert that `door` accepted `text`.
let private accepts (door: string) (text: string) : unit =
    match through door text with
    | Made v when door = "FromJson" && v.IsNull -> Values.fail (sprintf "FromJson of a well-formed text returned null")
    | Made _ -> ()
    | o -> Values.fail (sprintf "%s refused a well-formed text: it %s" door (describeOutcome o))

let private rowsOf (xs: string list) : obj[] seq = xs |> Seq.map (fun x -> [| box x |])

let allDoorRows : obj[] seq = rowsOf allDoors
let hostDoorRows : obj[] seq = rowsOf hostDoors

// -- The texts of JYRO-REQ-1655 -----------------------------------------------------------------------

/// (a) N + 1 characters `[` and nothing else.
let private openBrackets (k: int) : string = String('[', k)

/// (b) `[x` followed by N + 1 characters `[`: malformed at the x, before the bound.
let private malformedFirst () : string = "[x" + String('[', n () + 1)

/// (c) N + 1 repetitions of `{"k":` and nothing else.
let private openObjects (k: int) : string = String.replicate k "{\"k\":"

/// (d) N + 1 characters `[`, then a stray x and unbalanced closers.
let private tooDeepThenJunk () : string = String('[', n () + 1) + "x]]]}}"

/// (e) The tie: N - 1 characters `[`, then `{`, then `[`. The `[` is both the
/// first character no JSON text can have there (an object member begins with a
/// string) and the opening of a container at depth N + 1.
let private tie () : string = String('[', n () - 1) + "{["

// ================================================================================================
// JYRO-REQ-1655 The nesting bound is decided where the text first exceeds it
// ================================================================================================

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 the book's example, N+1 open brackets and nothing else, gives ValueNestingLimitExceeded at every door`` (door: string) =
    refuses door nesting invalid (openBrackets (n () + 1))

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 the book's example, a malformation before N+1 open brackets, gives null from FromJson and InvalidJson from the host`` (door: string) =
    if door = "FromJson" then fromJsonNull (malformedFirst ())
    else refuses door invalid nesting (malformedFirst ())

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 N+1 unclosed objects give ValueNestingLimitExceeded at every door`` (door: string) =
    refuses door nesting invalid (openObjects (n () + 1))

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 a text too deep before it goes wrong gives ValueNestingLimitExceeded whatever follows`` (door: string) =
    refuses door nesting invalid (tooDeepThenJunk ())

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 a malformation at the same character as the opening past the bound comes first`` (door: string) =
    if door = "FromJson" then fromJsonNull (tie ())
    else refuses door invalid nesting (tie ())

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 a well-formed text of depth exactly N is accepted at every door`` (door: string) =
    accepts door (nestedArrayJson (n ()))
    accepts door (nestedObjectJson (n ()))

[<Theory>]
[<MemberData(nameof allDoorRows)>]
[<Trait("requirement", "JYRO-REQ-1655")>]
let ``JYRO-REQ-1655 N open brackets and then a malformation is malformed first, the bound never reached`` (door: string) =
    // Depth N is within the bound; the x after it is the first thing met.
    let text = String('[', n ()) + "x"
    if door = "FromJson" then fromJsonNull text else refuses door invalid nesting text

// ================================================================================================
// JYRO-REQ-2113 A member of the host API refuses text that is not JSON with InvalidJson
// ================================================================================================

/// Texts that are not JSON and are within every bound.
let notJsonTexts = [ "[1,"; "{\"a\":1"; "x"; ""; "[1,]"; "'a'"; "1 2" ]

let notJsonRows : obj[] seq =
    [ for door in hostDoors do
        for t in notJsonTexts -> [| box door; box t |] ]

[<Theory>]
[<MemberData(nameof notJsonRows)>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 text that is not JSON is refused with InvalidJson`` (door: string, text: string) =
    let o = through door text
    let codes =
        match o with
        | ResultFailed(codes, _) when isRunDoor door -> Some codes
        | Threw(codes, _) when not (isRunDoor door) -> Some codes
        | _ -> None
    match codes with
    | Some cs when List.distinct cs = [ invalid ] -> ()
    | _ -> Values.fail (sprintf "%s of %A: expected InvalidJson and no other code through its own failure channel; it %s" door text (describeOutcome o))

[<Theory>]
[<InlineData("WithInputJson")>]
[<InlineData("WithInputJsonStream")>]
[<InlineData("ExecuteScriptWithJson")>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 the book's example, a run given the text [1, fails with InvalidJson`` (door: string) =
    match through door "[1," with
    | ResultFailed(codes, _) when List.contains invalid codes -> ()
    | o -> Values.fail (sprintf "%s of [1,: expected the run to fail with InvalidJson; it %s" door (describeOutcome o))

[<Theory>]
[<MemberData(nameof hostDoorRows)>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 a JSON text is accepted by every member`` (door: string) =
    accepts door "{\"a\":[1,2]}"

// ================================================================================================
// JYRO-REQ-1658 FromJson returns null for text it cannot interpret, unless a bound comes first
// ================================================================================================

/// Texts FromJson cannot interpret, within every bound.
let uninterpretable : obj[] seq = rowsOf [ "[1,"; "{\"a\":"; "x"; ""; "\"unterminated" ]

[<Theory>]
[<MemberData(nameof uninterpretable)>]
[<Trait("requirement", "JYRO-REQ-1658")>]
let ``JYRO-REQ-1658 text it cannot interpret within every bound gives null and the run goes on`` (text: string) =
    let r =
        Script.runWith
            { pinned with Input = ValueInput(Values.obj [ "t", Values.str text ]) }
            "Output.r = FromJson(Input.t)\nOutput.d = FromJson(Input.t) ?? {}\nOutput.after = true"
    let out = ok r
    Assert.Empty(Script.errorCodes r)
    Assert.True((Values.prop "r" out).IsNull, sprintf "FromJson(%A) gave %s" text (Values.toJson (Values.prop "r" out)))
    Assert.Equal("{}", Values.toJson (Values.prop "d" out))
    Assert.True(Values.asBool (Values.prop "after" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1658")>]
let ``JYRO-REQ-1658 text that exceeds the nesting bound before it goes wrong raises ValueNestingLimitExceeded`` () =
    refuses "FromJson" nesting invalid (openBrackets (n () + 1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1658")>]
let ``JYRO-REQ-1658 text that goes wrong before it exceeds the nesting bound gives null`` () =
    fromJsonNull (malformedFirst ())

// ================================================================================================
// JYRO-REQ-1659 FromJson raises ValueNestingLimitExceeded for text too deep before it goes wrong
// ================================================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 N+1 open brackets and nothing else raise ValueNestingLimitExceeded`` () =
    refuses "FromJson" nesting invalid (openBrackets (n () + 1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 N+1 unclosed objects raise ValueNestingLimitExceeded`` () =
    refuses "FromJson" nesting invalid (openObjects (n () + 1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 a malformation before N+1 open brackets gives null`` () =
    fromJsonNull (malformedFirst ())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1659")>]
let ``JYRO-REQ-1659 a well-formed text of depth exactly N returns the value`` () =
    match through "FromJson" (nestedArrayJson (n ())) with
    | Made v when not v.IsNull ->
        // Depth N: N arrays, the innermost holding 1.
        let mutable cur = v
        let mutable levels = 0
        while (cur :? JyroArray) do
            levels <- levels + 1
            cur <- (cur :?> JyroArray).[0]
        Assert.Equal(n (), levels)
        Assert.Equal(1m, Values.asDecimal cur)
    | o -> Values.fail (sprintf "FromJson of a text of depth N: it %s" (describeOutcome o))

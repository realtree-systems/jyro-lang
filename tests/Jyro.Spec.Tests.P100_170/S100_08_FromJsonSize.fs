/// `FromJson` decides the size bound where its result would first exceed it
/// (JYRO-REQ-1656, 100.8), and the size half of JYRO-REQ-1658 (90.4.2). Written
/// from the book and the public host API alone (FLOORS 4.1.1).
///
/// M is the maximum value size of 100.8.7 (16,777,216) and N the maximum nesting
/// depth of 100.7.1. Every text is built by the host and given in `Input` as a
/// Jyro string, which 100.8.7 does not bind (a value the host supplies). The
/// texts are about M characters, and FromJson may build values of about M
/// elements or properties before it decides, so these tests are in the
/// LargeValues collection (ValueSize.fs): one at a time, under the test host's
/// heap limit. Every run sets all four limits (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S100_08_FromJsonSize

open System
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.ValueSize

/// Run `FromJson(Input.t)` on `text`, keeping only whether it was null and, for
/// an array, its length and the length of its first element when a string
/// (-1 otherwise).
let private fromJson (text: string) : JyroResult<JyroValue> =
    Script.runWith
        (largeInput (objOf [ "t", JyroString(text) ]))
        (lines [ "var v = FromJson(Input.t)"
                 "Output.isNull = v == null"
                 "Output.n = -1"
                 "Output.first = -1"
                 "if v is array then"
                 "  Output.n = Length(v)"
                 "  if Length(v) > 0 then"
                 "    if v[0] is string then Output.first = Length(v[0]) end"
                 "  end"
                 "end"
                 "v = null" ])

/// Assert that FromJson returned null and the run went on with no error.
let private gaveNull (what: string) (r: JyroResult<JyroValue>) : unit =
    if not r.IsSuccess then Values.fail (sprintf "%s: expected FromJson to return null; %s" what (outcomeOf r))
    Assert.Empty(Script.errorCodes r)
    Assert.True(Values.asBool (Values.prop "isNull" (Script.valueOf r)), sprintf "%s: FromJson did not return null" what)

/// `["` then `k` characters `a`.
let private openString (k: int) : string = "[\"" + String('a', k)

/// `[`, then `k` elements `0` each followed by a comma, then `tail`.
let private zeros (k: int) (tail: string) : string =
    let sb = StringBuilder(2 * k + 4)
    sb.Append('[') |> ignore
    for _ in 1 .. k do sb.Append("0,") |> ignore
    sb.Append(tail).ToString()

/// A short distinct key for property i.
let private key (i: int) : string =
    let digits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let sb = StringBuilder()
    let mutable v = i
    sb.Append(digits.[v % 62]) |> ignore
    v <- v / 62
    while v > 0 do
        sb.Append(digits.[v % 62]) |> ignore
        v <- v / 62
    sb.ToString()

/// `{`, then `k` properties `"key":0` each followed by a comma, then `tail`.
let private properties (k: int) (tail: string) : string =
    let sb = StringBuilder(9 * k + 4)
    sb.Append('{') |> ignore
    for i in 0 .. k - 1 do sb.Append('"').Append(key i).Append("\":0,") |> ignore
    sb.Append(tail).ToString()

// -- A comment before the element or name that passes M: differential with Utf8JsonReader --------
//
// JYRO-REQ-1869 (90.11.2) has the script's FromJson accept comments and trailing commas, which
// RFC 8259 has neither, and states the rule for a // comment: it ends at the next LF or CR or at
// the end of text, and one that holds U+2028 or U+2029 makes the text one FromJson cannot
// interpret. JYRO-ADR-107 (accepted) decided that refusal first. FromJson reads that dialect as Utf8JsonReader does
// under the script's options (comments skipped, trailing commas allowed), with one addition
// (JYRO-ADR-186, JYRO-DEF-788): a comment between a key and its colon is accepted, which the
// reader refuses. So the reader is no longer a full oracle: for a text with a comment there, its
// verdict is wrong, and no row below puts one there. Where it finds the size point past the last
// token the reader hands over, it MUST stop where Utf8JsonReader stops on every text that has no
// comment between a key and its colon, and a differential test against the reader pins that.
// This is that test (JYRO-DEF-483).
//
// JYRO-REQ-1656 places the size point at the last character of the element or property that
// takes the container past M, and at the closing " of a name longer than M. Each row puts a
// comment, ended by one separator, just before that element or name. It gives Utf8JsonReader a
// short text with the same comment in the same place. If the reader reaches the element or name
// (the comment ended and the reader went on), the long text MUST raise ValueSizeLimitExceeded.
// If the reader stops first (the comment ran on to the end of the text, or held a character the
// reader refuses), the long text is malformed at or before the point where it could pass M, and
// FromJson MUST give null (JYRO-REQ-1658). FromJson of the short text must agree with the reader
// as well: the value when the reader reads it, null when the reader refuses it.

/// The separators put after `//c`: every line ending; the end of the text (nothing after it, so
/// the comment runs on over the rest); U+2028 and U+2029, alone and before LF; and near misses
/// that end no comment (U+0085 NEL, U+2027, U+202A), alone or before LF.
let commentSeparators : (string * string) list =
    [ "LF", "\n"
      "CR", "\r"
      "CR LF", "\r\n"
      "LF CR", "\n\r"
      "end of text", ""
      "U+2028", " "
      "U+2029", " "
      "U+2028 LF", " \n"
      "U+2029 LF", " \n"
      "U+0085", "\u0085"
      "U+0085 LF", "\u0085\n"
      "U+2027 LF", "‧\n"
      "U+202A LF", "‪\n" ]

/// The places a comment can stand before the element or name that passes M.
let commentShapes : string list =
    [ "array, after the comma, then a malformation"
      "array, after the comma, at the end of the text"
      "array, before the comma"
      "object, before the name"
      "object, after the colon"
      "a name longer than M, after the brace"
      "a name longer than M, after the comma"
      "a name longer than M, after a /* */ comment" ]

let commentRows () : obj[] seq =
    seq { for (s, _) in commentSeparators do for shape in commentShapes -> [| box s; box shape |] }

/// The token that shows Utf8JsonReader reached the element or name that passes M.
type Marker =
    | NumberOne
    | Name of string

/// The long text; the short text with the same comment in the same place; the strict-JSON form
/// of the short text's value; and the token that shows the reader reached the element or name.
let private commentCase (shape: string) (sep: string) : string * string * string * Marker =
    let c = "//c" + sep
    let longName = "\"" + String('a', M + 1) + "\"x"
    match shape with
    | "array, after the comma, then a malformation" -> zeros M (c + "1x]"), "[0," + c + "1]", "[0,1]", NumberOne
    | "array, after the comma, at the end of the text" -> zeros M (c + "1"), "[0," + c + "1]", "[0,1]", NumberOne
    | "array, before the comma" -> zeros (M - 1) ("0" + c + ",1x]"), "[0" + c + ",1]", "[0,1]", NumberOne
    | "object, before the name" ->
        properties M (c + "\"_\":1x}"), "{\"a\":0," + c + "\"_\":1}", "{\"a\":0,\"_\":1}", Name "_"
    | "object, after the colon" ->
        properties M ("\"_\":" + c + "1x}"), "{\"a\":0,\"_\":" + c + "1}", "{\"a\":0,\"_\":1}", NumberOne
    | "a name longer than M, after the brace" -> "{" + c + longName, "{" + c + "\"n\":1}", "{\"n\":1}", Name "n"
    | "a name longer than M, after the comma" ->
        "{\"a\":0," + c + longName, "{\"a\":0," + c + "\"n\":1}", "{\"a\":0,\"n\":1}", Name "n"
    | "a name longer than M, after a /* */ comment" ->
        let b = "/*c" + sep + "*/"
        "{" + b + longName, "{" + b + "\"n\":1}", "{\"n\":1}", Name "n"
    | other -> failwithf "unknown shape %s" other

/// Utf8JsonReader on `text` under the script FromJson's options (JYRO-ADR-107: comments
/// skipped, trailing commas allowed). Returns whether it reached `marker`, and whether it read
/// the whole text without refusing it.
let private readerOn (text: string) (marker: Marker) : bool * bool =
    let bytes = Encoding.UTF8.GetBytes text
    let options =
        Text.Json.JsonReaderOptions(CommentHandling = Text.Json.JsonCommentHandling.Skip, AllowTrailingCommas = true)
    let mutable reader = Text.Json.Utf8JsonReader(ReadOnlySpan<byte>(bytes), options)
    let mutable reached = false
    let mutable whole = false
    try
        while reader.Read() do
            match reader.TokenType, marker with
            | Text.Json.JsonTokenType.Number, NumberOne when Encoding.UTF8.GetString(reader.ValueSpan.ToArray()) = "1" ->
                reached <- true
            | Text.Json.JsonTokenType.PropertyName, Name n when reader.GetString() = n -> reached <- true
            | _ -> ()
        whole <- reader.BytesConsumed = int64 bytes.Length && reader.CurrentDepth = 0
    with :? Text.Json.JsonException -> ()
    reached, whole

/// How this FromJson reads `text`: Some true when it gives the value `expected` (white space
/// aside), Some false when it gives null, and None otherwise.
let private readsAs (text: string) (expected: string) : bool option =
    let r =
        Script.runWith
            (largeInput (objOf [ "t", JyroString(text) ]))
            (lines [ "var v = FromJson(Input.t)"
                     "Output.isNull = v == null"
                     "Output.j = \"\""
                     "if v != null then Output.j = ToJson(v) end" ])
    let out = ok r
    if Values.asBool (Values.prop "isNull" out) then Some false
    else
        let j = Values.asString (Values.prop "j" out)
        let strip (s: string) = String(s.ToCharArray() |> Array.filter (fun ch -> not (Char.IsWhiteSpace ch)))
        if strip j = strip expected then Some true else None

[<Collection("LargeValues")>]
type FromJsonSizeBound() =

    // Each row builds texts of about M characters and FromJson may build a value of about M
    // elements. The garbage of the row before must be gone, LOH included, before the next row
    // starts, or the 4 GiB heap of the test host runs out and the engine reports it as an
    // internal error instead of the code the row expects.
    let collect () =
        System.Runtime.GCSettings.LargeObjectHeapCompactionMode <- System.Runtime.GCLargeObjectHeapCompactionMode.CompactOnce
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)

    do collect ()

    interface IDisposable with
        member _.Dispose() = collect ()

    static member CommentRows : obj[] seq = commentRows ()

    [<Theory>]
    [<MemberData(nameof FromJsonSizeBound.CommentRows)>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 after a comment, the element or name that passes M decides where Utf8JsonReader reads that comment``
        (sepName: string, shape: string) =
        let sep = commentSeparators |> List.find (fun (n, _) -> n = sepName) |> snd
        let long, short, expected, marker = commentCase shape sep
        let reached, whole = readerOn short marker
        // The short text: FromJson reads it as the reader does.
        match whole, readsAs short expected with
        | true, Some true
        | false, Some false -> ()
        | _, got ->
            Values.fail (sprintf "FromJson of the short text %A disagrees with Utf8JsonReader: the reader %s it; FromJson gave %s"
                             short (if whole then "reads" else "refuses")
                             (match got with Some true -> expected | Some false -> "null" | None -> "another value"))
        // The long text: the size point is where the reader would reach the element or name.
        if reached && whole then fromJson long |> failsForSize
        elif not reached && not whole then
            fromJson long |> gaveNull (sprintf "M then a comment with %s (%s), which Utf8JsonReader does not read past" sepName shape)
        else Values.fail (sprintf "the short text %A does not decide the row: reached %b, read whole %b" short reached whole)

    // -- A string longer than M ------------------------------------------------------------------

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 the book's example, a closed string of M+1 and nothing else, raises ValueSizeLimitExceeded``() =
        fromJson (openString (M + 1) + "\"") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 the book's example, an unterminated string of M+1, gives null``() =
        fromJson (openString (M + 1)) |> gaveNull "an unterminated string of M + 1"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 the book's example, a string of M+1 then N open brackets, raises ValueNestingLimitExceeded``() =
        // The array opened by [" is at depth 1, so N more [ open one at depth N + 1.
        fromJson (openString (M + 1) + "\"," + String('[', maxDepth ())) |> failsWith MessageCode.ValueNestingLimitExceeded

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 a string of M+1 then a stray character raises ValueSizeLimitExceeded whatever follows``() =
        fromJson (openString (M + 1) + "\"x") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 a string of exactly M in a well-formed text is accepted``() =
        let r = fromJson (openString M + "\"]")
        let out = ok r
        Assert.Equal(1m, Values.asDecimal (Values.prop "n" out))
        Assert.Equal(decimal M, Values.asDecimal (Values.prop "first" out))

    // -- An array of more than M elements ----------------------------------------------------------

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 an array of M+1 elements then a malformation raises ValueSizeLimitExceeded``() =
        fromJson (zeros (M + 1) "x]") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 an array of M elements then the same malformation gives null``() =
        fromJson (zeros M "x]") |> gaveNull "an array of M elements then x"

    // -- An object of more than M properties --------------------------------------------------------

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 an object of M+1 properties then a malformation raises ValueSizeLimitExceeded``() =
        fromJson (properties (M + 1) "x}") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 an object of M properties then the same malformation gives null``() =
        fromJson (properties M "x}") |> gaveNull "an object of M properties then x"

    // -- The element or name that passes M directly followed by a malformation or the end ----------
    //
    // 1656 places the bound at the last character of the element or property that takes the
    // container past M, and at the closing " of a string longer than M, a property name included
    // (a name is a string, RFC 8259 section 4). The rows above put a comma after every element, so
    // a reader that returns a token only when it sees what follows it decides them right anyway.
    // These rows put the malformation, or the end of the text, directly after that character.
    // The end of a text comes after its last character: the book's own example, a closed string of
    // M+1 and nothing else, raises ValueSizeLimitExceeded although the text is malformed at its end.
    // "_" is no key that `key` makes, so the last property is distinct from the M before it.

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an array whose M+1th element is a number directly followed by a malformation raises ValueSizeLimitExceeded``() =
        fromJson (zeros M "1x]") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an array whose Mth element is a number directly followed by a malformation gives null``() =
        fromJson (zeros (M - 1) "1x]") |> gaveNull "an array whose Mth element 1 is followed by x"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an array whose M+1th element is a number at the end of the text raises ValueSizeLimitExceeded``() =
        fromJson (zeros M "1") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an array whose Mth element is a number at the end of the text gives null``() =
        fromJson (zeros (M - 1) "1") |> gaveNull "an array whose Mth element 1 ends the text"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an object whose M+1th property is a number directly followed by a malformation raises ValueSizeLimitExceeded``() =
        fromJson (properties M "\"_\":1x}") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an object whose Mth property is a number directly followed by a malformation gives null``() =
        fromJson (properties (M - 1) "\"_\":1x}") |> gaveNull "an object whose Mth property 1 is followed by x"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an object whose M+1th property is a number at the end of the text raises ValueSizeLimitExceeded``() =
        fromJson (properties M "\"_\":1") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 an object whose Mth property is a number at the end of the text gives null``() =
        fromJson (properties (M - 1) "\"_\":1") |> gaveNull "an object whose Mth property 1 ends the text"

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 a property name of M+1 directly followed by a malformation raises ValueSizeLimitExceeded``() =
        fromJson ("{\"" + String('a', M + 1) + "\"x") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 a property name of M+1 directly followed by a comma raises ValueSizeLimitExceeded``() =
        fromJson ("{\"" + String('a', M + 1) + "\",}") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 a property name of M+1 at the end of the text raises ValueSizeLimitExceeded``() =
        fromJson ("{\"" + String('a', M + 1) + "\"") |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    [<Trait("requirement", "JYRO-REQ-1658")>]
    member _.``JYRO-REQ-1656 JYRO-REQ-1658 a property name of exactly M directly followed by a malformation gives null``() =
        fromJson ("{\"" + String('a', M) + "\"x") |> gaveNull "a property name of M followed by x"

    // -- A well-formed text that the heap cannot hold ------------------------------------------------
    //
    // 90.4.2 gives null for a text FromJson cannot interpret as JSON, and only for that. A well-formed
    // array of 8,000,000 empty objects (below M) is JSON; a child process whose heap (320 MB) cannot
    // hold the value it makes must not have it reported to the script as not JSON. The run may fail,
    // or succeed if the value fits; it may not succeed with FromJson having given null.

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1656")>]
    member _.``JYRO-REQ-1656 a well-formed text that the heap cannot hold is never reported as null``() =
        let o = Child.runWith 120_000 [ "DOTNET_GCHeapHardLimit", "14000000" ] "vs|fromjson|short-of-heap"
        Child.expectClean o
        match o.Result with
        | Some r when (field "isNull" r) = Some "true" ->
            Values.fail ("FromJson gave null for a well-formed text of 8,000,000 elements when the heap ran short: " + o.Describe)
        | _ -> ()

    // -- A long number that passes M ---------------------------------------------------------------
    //
    // JYRO-ADR-151 (5)(d) (JYRO-DEF-609): where FromJson must read a number that takes its result
    // past M, it reads the token where the text holds it, a chunk at a time under the execution
    // check, and copies none of it whole. Two texts of one length are compared: an array of M
    // elements 0 then, as its M+1th element, a number of `digits` digits, and the same array with
    // 0.1 as that element and white space to the same length, each number at the end of the
    // text. Both raise
    // ValueSizeLimitExceeded. Reading white space copies nothing, so what the long number costs
    // beyond the padded text is what reading its digits allocates: a copy of the token as a .NET
    // string costs twice its length, and reading it in place a bounded amount. The collection
    // runs alone, so the process's allocations are this run's.

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-REQ-1411 FromJson reads a long number that takes its result past M where it lies, not copied whole``() =
        let digits = 4_000_000
        let longNumber = zeros M ("0." + String('1', digits))
        let padded = zeros M ("0.1" + String(' ', digits - 1))
        Assert.Equal(longNumber.Length, padded.Length)
        let measure (text: string) =
            let before = GC.GetTotalAllocatedBytes(true)
            let r = fromJson text
            let after = GC.GetTotalAllocatedBytes(true)
            failsForSize r
            after - before
        // Once to warm the path up; each text is then measured alone.
        fromJson padded |> failsForSize
        let extra = measure longNumber - measure padded
        if extra >= int64 (digits / 2) then
            Values.fail (sprintf "a number of %d digits that passes M at the end of the text allocated %d bytes more than the same length of white space"
                             digits extra)

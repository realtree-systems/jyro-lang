/// A chunked read gives the same value as a read in one call (JYRO-REQ-1411, 100.2.5;
/// JYRO-DEF-556). Written from the book and the public host API alone.
///
/// 100.2.5 has FromJson check the limits at bounded intervals while it decodes a long
/// string token, so a long token is read in pieces. The check must not change the value:
/// FromJson of valid JSON gives the string the JSON denotes (RFC 8259 section 7), whatever
/// piece a character falls in. A character of two, three or four UTF-8 bytes (U+00E9,
/// U+20AC, U+1F600) is put at every byte offset from 8 before to 4 after 4,096 and 8,192 in
/// the text, in a string value and in a property name, alone and right after an escape of 2
/// or 6 bytes. The interval of 100.2.5 is not in the book, so the rows cover the powers of
/// two where a piece would end; the offsets on both sides cover a token starting at byte 1
/// or 2 of the text. Runs are small (texts of about 11 KB to 20 KB), so every limit is set
/// explicitly but none is near.
module RealTree.Jyro.Spec.Tests.S100_02_ChunkBoundaries

open System
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.ValueSize

/// The characters: (name, text) for one character of 2, 3 and 4 UTF-8 bytes.
let private characters : (string * string) list =
    [ "U+00E9 (2 bytes)", "é"
      "U+20AC (3 bytes)", "€"
      "U+1F600 (4 bytes)", Char.ConvertFromUtf32 0x1F600 ]

/// The escapes put right before the character: (JSON text, the text it denotes).
let private escapes : (string * string) list =
    [ "", ""
      "\\n", "\n"
      "\\u00e9", "é" ]

let private boundaries = [ 4096; 8192 ]

/// One row: a description, the JSON text, whether the long string is a property name,
/// and the string FromJson must give.
type private Row = { What: string; Json: string; IsKey: bool; Want: string }

/// Build a row with the character's lead byte at byte `at` of the JSON text. All before it
/// is ASCII, so byte and code-unit offsets agree. The string goes on past the next
/// boundary so the following piece is read too.
let private row (charName: string, ch: string) (escJson: string, escText: string) (isKey: bool) (at: int) : Row =
    let opening = if isKey then "{\"" else "[\""
    let closing = if isKey then "\":1}" else "\"]"
    let pad = at - opening.Length - escJson.Length
    let before = String('a', pad)
    let after = String('z', 4200)
    { What = sprintf "%s at byte %d of the text, %s, after escape %A" charName at (if isKey then "name" else "value") escJson
      Json = opening + before + escJson + ch + after + closing
      IsKey = isKey
      Want = before + escText + ch + after }

/// A row of only the character, 5,000 times, after `shift` ASCII characters: every piece
/// boundary falls on some byte of some character.
let private runRow (charName: string, ch: string) (isKey: bool) (shift: int) : Row =
    let opening = if isKey then "{\"" else "[\""
    let closing = if isKey then "\":1}" else "\"]"
    let body = String('a', shift) + String.replicate 5000 ch
    { What = sprintf "5,000 x %s after %d ASCII, %s" charName shift (if isKey then "name" else "value")
      Json = opening + body + closing
      IsKey = isKey
      Want = body }

let private rows () : Row list =
    [ for c in characters do
        for isKey in [ false; true ] do
            for e in escapes do
                for b in boundaries do
                    for at in b - 8 .. b + 4 -> row c e isKey at
            for shift in 0 .. 7 -> runRow c isKey shift ]

/// Run FromJson over every row in one script and return the indices whose result differs
/// from the row's string (null, another type, or other text).
let private mismatches (rs: Row list) : int list =
    let texts = JyroArray()
    let wants = JyroArray()
    let keys = JyroArray()
    for r in rs do
        texts.Add(JyroString(r.Json))
        wants.Add(JyroString(r.Want))
        keys.Add(Values.bool r.IsKey)
    let r =
        Script.runWith
            (largeInput (objOf [ "texts", texts; "wants", wants; "keys", keys ]))
            (lines [ "var bad = []"
                     "var i = 0"
                     "while i < Length(Input.texts) do"
                     "  var v = FromJson(Input.texts[i])"
                     "  var got = null"
                     "  if Input.keys[i] then"
                     "    if v is object then got = Keys(v)[0] end"
                     "  else"
                     "    if v is array then got = v[0] end"
                     "  end"
                     "  if got != Input.wants[i] then bad = Append(bad, i) end"
                     "  i = i + 1"
                     "end"
                     "Output.read = i"
                     "Output.bad = bad" ])
    let out = ok r
    // Every row was read, the last included.
    Assert.Equal(decimal rs.Length, Values.asDecimal (Values.prop "read" out))
    match Values.prop "bad" out with
    | :? JyroArray as a -> [ for x in a -> int (Values.asDecimal x) ]
    | other -> Values.fail (sprintf "the script did not give its list of mismatches: %A" other)

[<Collection("LargeValues")>]
type FromJsonChunkBoundaries() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1411")>]
    member _.``JYRO-REQ-1411 FromJson gives back a long string with a 2-, 3- or 4-byte character at every offset around 4,096 and 8,192``() =
        let rs = rows ()
        let bad = mismatches rs
        if not bad.IsEmpty then
            let shown = bad |> List.truncate 20 |> List.map (fun i -> rs.[i].What)
            Values.fail (sprintf "FromJson gave another value for %d of %d rows, first: %s" bad.Length rs.Length (String.concat "; " shown))

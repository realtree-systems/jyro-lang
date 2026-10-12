/// Strings for the comparison "with no case sensitivity for ASCII letters"
/// of 10.7.1, shared by the suites of its users (30.12.7, 30.12.8, 92.21.5 to
/// 92.21.7, 94.5.5, 94.6.6, 94.7.5); the projects of chapters 92 and 93-94
/// compile this file by a link. Written from the book alone (FLOORS 4.1.1).
///
/// Every character outside ASCII is made from its code unit number, never
/// written as a literal of the character: a text store may normalize U+212A
/// (KELVIN SIGN) to `K`, and an F# "\uDC00" literal becomes U+FFFD.
module RealTree.Jyro.Spec.Tests.AsciiCase

open System
open System.Globalization
open System.Text

/// A string of the one UTF-16 code unit `codeUnit`.
let cu (codeUnit: int) : string = String(char codeUnit, 1)

/// U+017F LATIN SMALL LETTER LONG S (`ſ`), which Unicode case folding maps to `s`.
let longS : string = cu 0x017F

/// U+212A KELVIN SIGN, which Unicode lower-cases to `k`.
let kelvin : string = cu 0x212A

/// U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE (`İ`).
let dottedI : string = cu 0x0130

/// U+0131 LATIN SMALL LETTER DOTLESS I (`ı`).
let dotlessI : string = cu 0x0131

/// A low surrogate with no high surrogate before it: an unpaired surrogate.
let loneLow : string = cu 0xDC00

/// True when `s` holds a surrogate code unit that is not part of a pair.
let hasUnpairedSurrogate (s: string) : bool =
    let mutable i = 0
    let mutable found = false
    while i < s.Length do
        if Char.IsHighSurrogate s.[i] && i + 1 < s.Length && Char.IsLowSurrogate s.[i + 1] then i <- i + 2
        elif Char.IsSurrogate s.[i] then
            found <- true
            i <- i + 1
        else i <- i + 1
    found

/// A Jyro string literal whose value is `s` (20.7): `"` and `\` escaped, and
/// every code unit outside printable ASCII written as a `\u` escape of four
/// hexadecimal digits. Not for a string with an unpaired surrogate, which a
/// literal cannot hold (20.2.5, 20.7.4): such a string reaches a script only
/// from the host.
let literal (s: string) : string =
    if hasUnpairedSurrogate s then invalidArg "s" "a Jyro literal cannot hold an unpaired surrogate (20.7.4)"
    let b = StringBuilder("\"")
    for c in s do
        if c = '"' then b.Append("\\\"") |> ignore
        elif c = '\\' then b.Append("\\\\") |> ignore
        elif c < ' ' || c > '~' then b.Append("\\u").Append((int c).ToString("X4")) |> ignore
        else b.Append(c) |> ignore
    b.Append('"').ToString()

/// A readable name for `s`, for a test case: every code unit outside
/// printable ASCII shown as U+XXXX.
let label (s: string) : string =
    let b = StringBuilder()
    for c in s do
        if c < ' ' || c > '~' then b.Append("<U+").Append((int c).ToString("X4")).Append(">") |> ignore
        else b.Append(c) |> ignore
    if b.Length = 0 then "<empty>" else b.ToString()

/// Run `f` with `CultureInfo.CurrentCulture` and `CurrentUICulture` set to the
/// culture `name` ("" for the invariant culture), restoring both after.
let underCulture (name: string) (f: unit -> 'T) : 'T =
    let culture, uiCulture = CultureInfo.CurrentCulture, CultureInfo.CurrentUICulture
    let c = if name = "" then CultureInfo.InvariantCulture else CultureInfo.GetCultureInfo name
    // The case is only meaningful where the culture's own casing is in force.
    if name = "tr-TR" && "i".ToUpper(c) <> dottedI then
        raise (Xunit.Sdk.XunitException "tr-TR does not upper-case i to U+0130 here (invariant globalization?); the culture case cannot run")
    CultureInfo.CurrentCulture <- c
    CultureInfo.CurrentUICulture <- c
    try f ()
    finally
        CultureInfo.CurrentCulture <- culture
        CultureInfo.CurrentUICulture <- uiCulture

/// The cultures a comparison by 10.7.1 is run under: tr-TR, where `I`
/// lower-cases to U+0131 and `i` upper-cases to U+0130, and the invariant one.
let cultures : string list = [ "tr-TR"; "" ]

/// Look up the string of a case by its label (Theory data carries the label,
/// never the string, so that no unpaired surrogate is serialized).
let byLabel (cases: string list) (name: string) : string =
    match cases |> List.tryFind (fun s -> label s = name) with
    | Some s -> s
    | None -> invalidArg "name" (sprintf "no case is labeled %s" name)

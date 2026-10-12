// Generates Unicode15.fs, the Unicode 15.0.0 data that Jyro reads (spec 10.6.1,
// JYRO-REQ-9, JYRO-ADR-143), from the three files of the Unicode Character
// Database of Unicode 15.0.0 that the specification names:
//
//   UnicodeData.txt            general categories L, Nd and Zs, and the simple
//                              uppercase and lowercase mappings
//   PropList.txt               White_Space
//   DerivedCoreProperties.txt  ID_Start and ID_Continue
//
// Usage, from this folder:
//
//   dotnet fsi Unicode15.generate.fsx [ucd-folder]
//
// The files are read from `ucd-folder`, and any that is missing there is
// downloaded into it from https://www.unicode.org/Public/15.0.0/ucd/. Each file
// must have the SHA-256 that Unicode15.ucd.sha256 records beside this script,
// or nothing is written. No host Unicode API is read: every datum comes from
// the files.

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Text

let here = __SOURCE_DIRECTORY__
let ucdFolder =
    match fsi.CommandLineArgs |> Array.tail with
    | [| folder |] -> Path.GetFullPath folder
    | _ -> Path.Combine(Path.GetTempPath(), "ucd-15.0.0")
Directory.CreateDirectory(ucdFolder) |> ignore

let baseUrl = "https://www.unicode.org/Public/15.0.0/ucd/"
let files = [ "UnicodeData.txt"; "PropList.txt"; "DerivedCoreProperties.txt" ]

let expected =
    File.ReadAllLines(Path.Combine(here, "Unicode15.ucd.sha256"))
    |> Array.filter (fun l -> l.Trim().Length > 0 && not (l.StartsWith "#"))
    |> Array.map (fun l ->
        let parts = l.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        parts.[1], parts.[0].ToLowerInvariant())
    |> Map.ofArray

let sha256 (path: string) =
    use stream = File.OpenRead path
    Convert.ToHexString(SHA256.HashData(stream)).ToLowerInvariant()

let text =
    use http = new HttpClient()
    files
    |> List.map (fun name ->
        let path = Path.Combine(ucdFolder, name)
        if not (File.Exists path) then
            printfn "downloading %s" name
            let bytes = http.GetByteArrayAsync(baseUrl + name).GetAwaiter().GetResult()
            File.WriteAllBytes(path, bytes)
        let actual = sha256 path
        match Map.tryFind name expected with
        | Some hash when hash = actual -> ()
        | Some hash -> failwithf "%s has SHA-256 %s, not %s" name actual hash
        | None -> failwithf "Unicode15.ucd.sha256 records no hash for %s" name
        name, File.ReadAllLines(path, Encoding.UTF8))
    |> Map.ofList

let hex (s: string) = Convert.ToInt32(s.Trim(), 16)

// UnicodeData.txt: one line per code point, or a <..., First> and <..., Last>
// pair for a range that shares its fields.
let category = Collections.Generic.Dictionary<int, string>()
let upper = Collections.Generic.SortedDictionary<int, int>()
let lower = Collections.Generic.SortedDictionary<int, int>()
let mutable rangeStart = -1
for line in text.["UnicodeData.txt"] do
    if line.Length > 0 then
        let f = line.Split(';')
        let cp = hex f.[0]
        let gc = f.[2]
        if f.[1].EndsWith(", First>") then rangeStart <- cp
        elif f.[1].EndsWith(", Last>") then
            for c in rangeStart .. cp do category.[c] <- gc
            rangeStart <- -1
        else
            category.[cp] <- gc
            if f.[12] <> "" then upper.[cp] <- hex f.[12]
            if f.[13] <> "" then lower.[cp] <- hex f.[13]

// PropList.txt and DerivedCoreProperties.txt: `cp[..cp] ; Property # ...`.
let property (file: string) (name: string) =
    let set = Collections.Generic.HashSet<int>()
    for line in text.[file] do
        let content = (match line.IndexOf('#') with -1 -> line | i -> line.Substring(0, i)).Trim()
        if content.Length > 0 then
            let parts = content.Split(';')
            if parts.[1].Trim() = name then
                let range = parts.[0].Trim()
                match range.IndexOf("..") with
                | -1 -> set.Add(hex range) |> ignore
                | i -> for c in hex (range.Substring(0, i)) .. hex (range.Substring(i + 2)) do set.Add c |> ignore
    set

let byCategory (test: string -> bool) =
    let set = Collections.Generic.HashSet<int>()
    for kv in category do if test kv.Value then set.Add kv.Key |> ignore
    set

let ranges (set: Collections.Generic.HashSet<int>) =
    let sorted = set |> Seq.sort |> Array.ofSeq
    let result = ResizeArray<int * int>()
    for cp in sorted do
        if result.Count > 0 && snd result.[result.Count - 1] = cp - 1 then
            result.[result.Count - 1] <- (fst result.[result.Count - 1], cp)
        else result.Add((cp, cp))
    result

let letters = byCategory (fun gc -> gc.StartsWith "L")
let decimalDigits = byCategory ((=) "Nd")
let spaceSeparators = byCategory ((=) "Zs")
let whiteSpace = property "PropList.txt" "White_Space"
let idStart = property "DerivedCoreProperties.txt" "ID_Start"
let idContinue = property "DerivedCoreProperties.txt" "ID_Continue"

// A simple case mapping never moves a code point between the Basic
// Multilingual Plane and the planes above it, so a mapped text has the length
// in UTF-16 code units of the text; ToUpper and ToLower rely on it.
for map in [ upper; lower ] do
    for kv in map do
        if (kv.Key > 0xFFFF) <> (kv.Value > 0xFFFF) then
            failwithf "U+%04X maps across planes to U+%04X" kv.Key kv.Value

let out = StringBuilder()
let w (s: string) = out.Append(s).Append('\n') |> ignore
let rangeTable (name: string) (doc: string) (set: Collections.Generic.HashSet<int>) =
    w (sprintf "    // %s, as sorted inclusive ranges." doc)
    w (sprintf "    let private %s : int[] =" name)
    w "        [|"
    let items = ranges set |> Seq.map (fun (a, b) -> sprintf "0x%X; 0x%X" a b) |> Array.ofSeq
    for chunk in Array.chunkBySize 6 items do w ("            " + String.Join("; ", chunk) + ";")
    w "        |]"
    w ""
let mapTable (name: string) (doc: string) (map: Collections.Generic.SortedDictionary<int, int>) =
    w (sprintf "    // %s, as pairs of a code point and its mapping, sorted." doc)
    w (sprintf "    let private %s : int[] =" name)
    w "        [|"
    let items = map |> Seq.map (fun kv -> sprintf "0x%X; 0x%X" kv.Key kv.Value) |> Array.ofSeq
    for chunk in Array.chunkBySize 6 items do w ("            " + String.Join("; ", chunk) + ";")
    w "        |]"
    w ""

w "// <auto-generated>"
w "// Generated by Unicode15.generate.fsx from the Unicode Character Database of"
w "// Unicode 15.0.0. Do not edit: change the generator and run it again. Inputs:"
for name in files do w (sprintf "//   %s  SHA-256 %s" name expected.[name])
w "// </auto-generated>"
w "namespace RealTree.Jyro"
w ""
w "// The Unicode data that Jyro reads, in the version that spec 10.6.1 names,"
w "// Unicode 15.0.0, whatever version the host platform carries (JYRO-REQ-9,"
w "// JYRO-ADR-143). Every table is generated from the files of the Unicode"
w "// Character Database of Unicode 15.0.0 by Unicode15.generate.fsx, and no"
w "// host Unicode API is read."
w "module internal Unicode15 ="
w ""
rangeTable "letters" "General category L (Lu, Ll, Lt, Lm, Lo) of UnicodeData.txt" letters
rangeTable "decimalDigits" "General category Nd of UnicodeData.txt" decimalDigits
rangeTable "spaceSeparators" "General category Zs of UnicodeData.txt" spaceSeparators
rangeTable "whiteSpace" "The property White_Space of PropList.txt" whiteSpace
rangeTable "idStart" "The property ID_Start of DerivedCoreProperties.txt" idStart
rangeTable "idContinue" "The property ID_Continue of DerivedCoreProperties.txt" idContinue
mapTable "upperMap" "The simple uppercase mapping of UnicodeData.txt (field 12)" upper
mapTable "lowerMap" "The simple lowercase mapping of UnicodeData.txt (field 13)" lower
w """    // The code point is in one of the sorted inclusive `ranges`, each held
    // as two elements.
    let private inRanges (ranges: int[]) (cp: int) : bool =
        let mutable lo = 0
        let mutable hi = ranges.Length / 2 - 1
        let mutable found = false
        while not found && lo <= hi do
            let mid = (lo + hi) >>> 1
            if cp < ranges.[2 * mid] then hi <- mid - 1
            elif cp > ranges.[2 * mid + 1] then lo <- mid + 1
            else found <- true
        found

    // The mapping of `cp` in the sorted pairs `map`, or `cp` when it has none.
    let private mapped (map: int[]) (cp: int) : int =
        let mutable lo = 0
        let mutable hi = map.Length / 2 - 1
        let mutable result = cp
        while lo <= hi do
            let mid = (lo + hi) >>> 1
            let key = map.[2 * mid]
            if cp < key then hi <- mid - 1
            elif cp > key then lo <- mid + 1
            else
                result <- map.[2 * mid + 1]
                lo <- hi + 1
        result

    // The code point is of general category L.
    let isLetter (cp: int) : bool = inRanges letters cp

    // The code point is of general category Nd.
    let isDecimalDigit (cp: int) : bool = inRanges decimalDigits cp

    // The code point is of general category Zs.
    let isSpaceSeparator (cp: int) : bool = inRanges spaceSeparators cp

    // The code point has the property White_Space.
    let isWhiteSpace (cp: int) : bool = inRanges whiteSpace cp

    // The code point has the property ID_Start (ECMA-262 UnicodeIDStart).
    let isIdStart (cp: int) : bool = inRanges idStart cp

    // The code point has the property ID_Continue (ECMA-262 UnicodeIDContinue).
    let isIdContinue (cp: int) : bool = inRanges idContinue cp

    // The simple uppercase mapping of the code point, or the code point.
    let toUpper (cp: int) : int = mapped upperMap cp

    // The simple lowercase mapping of the code point, or the code point.
    let toLower (cp: int) : int = mapped lowerMap cp

    // `identifier_start` of 20.1.1: a letter or `_`.
    let isIdentifierStart (cp: int) : bool = cp = int '_' || isLetter cp

    // `identifier_char` of 20.1.1: a letter, a decimal digit or `_`.
    let isIdentifierChar (cp: int) : bool = cp = int '_' || isLetter cp || isDecimalDigit cp

    // The code point at `index` of `s` and its width in code units: a
    // surrogate pair is one code point of two units; an unpaired surrogate
    // is its own code unit, which no table above holds.
    let codePointAt (s: string) (index: int) : struct (int * int) =
        let c = int s.[index]
        if c >= 0xD800 && c <= 0xDBFF && index + 1 < s.Length
           && int s.[index + 1] >= 0xDC00 && int s.[index + 1] <= 0xDFFF then
            struct (0x10000 + ((c - 0xD800) <<< 10) + (int s.[index + 1] - 0xDC00), 2)
        else
            struct (c, 1)"""

File.WriteAllText(Path.Combine(here, "Unicode15.fs"), out.ToString().Replace("\r\n", "\n"))
printfn "wrote Unicode15.fs: %d letters, %d Nd, %d Zs, %d White_Space, %d ID_Start, %d ID_Continue, %d upper, %d lower"
    letters.Count decimalDigits.Count spaceSeparators.Count whiteSpace.Count idStart.Count idContinue.Count upper.Count lower.Count

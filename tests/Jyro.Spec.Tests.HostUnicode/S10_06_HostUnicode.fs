/// 10.6.1 - Unicode 15.0.0 "whatever version the host platform carries", on a
/// host that carries Unicode 13.0 case data (ICU 68.2, loaded app-locally by
/// this project). Each case below has a simple case mapping in UnicodeData.txt
/// of Unicode 15.0.0 that Unicode 14.0 first gave, so the host does not have it:
/// ToUpper and ToLower must still give it. Every run sets the four execution
/// limits explicitly (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S10_06_HostUnicode

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec

let private cp (codePoint: int) : string = Char.ConvertFromUtf32 codePoint

let private limited : RunOptions =
    { Script.defaults with
        Limits =
            Some (Script.limits (fun o ->
                { o with
                    MaxExecutionTime = TimeSpan.FromSeconds 30.
                    MaxStatements = 1_000_000
                    MaxLoopIterations = 1_000_000
                    MaxCallDepth = 64 })) }

/// `fn(Input.s)` of a successful run, as a string.
let private call (fn: string) (s: string) : string =
    let r = Script.runWith { limited with Input = ValueInput(Values.obj [ "s", Values.str s ]) } (sprintf "Output.v = %s(Input.s)" fn)
    if not (Script.ok r) then
        Values.fail (sprintf "%s failed. Diagnostics:\n%s" fn (Script.describe r))
    Values.asString ((Script.output r).GetProperty "v")

/// The host must lack the mapping, or the row cannot show a host read: fail
/// rather than pass when it does not (the process rule of JYRO-DEF-578).
let private assertHostLacks (description: string) (hostResult: string) (unmapped: string) =
    if hostResult <> unmapped then
        Values.fail (sprintf "This host already carries %s, so the row cannot show that ToUpper or ToLower reads the host: the test is blocked here, not passed." description)

/// (lowercase, uppercase) pairs that UnicodeData.txt of Unicode 15.0.0 maps both
/// ways and Unicode 13.0 does not: each first assigned, or first cased, in 14.0.
///   U+2C5F / U+2C2F GLAGOLITIC CAUDATE CHRIVI
///   U+A7C1 / U+A7C0 LATIN OLD POLISH O
///   U+A7D1 / U+A7D0 LATIN CLOSED INSULAR G
///   U+A7D7 / U+A7D6 LATIN MIDDLE SCOTS S
///   U+A7D9 / U+A7D8 LATIN SIGMOID S
///   U+10597 / U+10570 VITHKUQI A
let casedIn14Rows () : obj[] seq =
    [ 0x2C5F, 0x2C2F
      0xA7C1, 0xA7C0
      0xA7D1, 0xA7D0
      0xA7D7, 0xA7D6
      0xA7D9, 0xA7D8
      0x10597, 0x10570 ]
    |> Seq.map (fun (l, u) -> [| box l; box u |])

[<Theory>]
[<MemberData(nameof casedIn14Rows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 ToUpper_ReadsUnicode15_0_NotTheHost`` (lower: int, upper: int) =
    assertHostLacks (sprintf "the uppercase of U+%04X" lower) ((cp lower).ToUpperInvariant()) (cp lower)
    Assert.Equal(cp upper, call "ToUpper" (cp lower))
    Assert.Equal("A" + cp upper + "B", call "ToUpper" ("a" + cp lower + "b"))

[<Theory>]
[<MemberData(nameof casedIn14Rows)>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 ToLower_ReadsUnicode15_0_NotTheHost`` (lower: int, upper: int) =
    assertHostLacks (sprintf "the lowercase of U+%04X" upper) ((cp upper).ToLowerInvariant()) (cp upper)
    Assert.Equal(cp lower, call "ToLower" (cp upper))
    Assert.Equal("a" + cp lower + "b", call "ToLower" ("A" + cp upper + "B"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-9")>]
let ``JYRO-REQ-9 ToUpper_And_ToLower_OnThisHost_StillMapWhatBothVersionsMap`` () =
    // Mappings older than Unicode 13.0 are unchanged, and a code point that 15.0.0
    // leaves unmapped stays so: U+0264 has no uppercase in 15.0.0 (16.0 gives U+A7CB).
    Assert.Equal("ABC" + cp 0x0391 + cp 0x10400, call "ToUpper" ("abc" + cp 0x03B1 + cp 0x10428))
    Assert.Equal("abc" + cp 0x03B1 + cp 0x10428, call "ToLower" ("ABC" + cp 0x0391 + cp 0x10400))
    Assert.Equal(cp 0x0264, call "ToUpper" (cp 0x0264))
    Assert.Equal(cp 0xA7CB, call "ToLower" (cp 0xA7CB))
    // UnicodeData.txt: U+0131 uppercases to I and U+0130 lowercases to i.
    Assert.Equal("I", call "ToUpper" (cp 0x0131))
    Assert.Equal("i", call "ToLower" (cp 0x0130))

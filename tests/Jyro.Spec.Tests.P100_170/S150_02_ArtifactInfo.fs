/// 150.2.7 and 150.2.8: JyroArtifactInfo.Read reads the header and the
/// function table only, and refuses its bytes by six checks in a fixed order.
module RealTree.Jyro.Spec.Tests.S150_02_ArtifactInfo

open System
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.S150_01_Artifact

let private maxSize = 10_485_760

/// `JyroArtifactInfo.Read`, asserting that it answers through its result and
/// does not throw.
let private read (bytes: byte[]) : JyroResult<JyroArtifactInfo> =
    try JyroArtifactInfo.Read bytes
    with e -> Values.fail (sprintf "JyroArtifactInfo.Read threw %s: %s" (e.GetType().FullName) e.Message)

/// Assert that Read refuses `bytes` through its result, with exactly the one code.
let private refusedBy (code: MessageCode) (bytes: byte[]) : unit =
    let r = read bytes
    Assert.False(r.IsSuccess, sprintf "Read accepted %d bytes" bytes.Length)
    Script.expectError code r
    // A refusal of JyroArtifactInfo.Read has the stage Deserialize (160.1.12, 160.7.9).
    stageOfCode code "Deserialize" r

/// Assert that Read accepts `bytes`, and return what it read.
let private accepted (bytes: byte[]) : JyroArtifactInfo =
    let r = read bytes
    Assert.True(r.IsSuccess, Script.describe r)
    Script.valueOf r

/// A header of 44 bytes: magic, version, reserved, a hash, and a table count.
let private headerWith (magic: byte[]) (version: int) (reserved: int) (count: uint32) : byte[] =
    let w = Writer(1uy)
    w.Raw magic
    w.U16 version
    w.U16 reserved
    w.Raw(sha256 "header")
    w.U32 count
    w.Bytes()

let private jyrx = "JYRX"B

/// The format version of this specification, 1 (150.10.4, JYRO-REQ-1920).
let private v = int formatVersion

/// A header of the format version of this specification.
let private header (count: uint32) = headerWith jyrx v 0 count

/// The function table bytes of `names`.
let private tableBytes (names: string list) : byte[] =
    let w = Writer(1uy)
    for n in names do w.Str n
    w.Bytes()

/// A real artifact from CompileToArtifact, and the offset where its program starts.
let private realArtifact () : byte[] * int =
    let bytes = Script.compileBytesWith Script.defaults "Output.a = Length([1])\nOutput.b = ToUpper(\"x\")" |> Script.valueOf
    let count = int (BitConverter.ToUInt32(bytes, 40))
    let mutable offset = 44
    for _ in 1 .. count do
        let n = int (BitConverter.ToUInt16(bytes, offset))
        offset <- offset + 2 + n
    bytes, offset

/// The header and table of a real artifact followed by `program`.
let private withProgram (program: byte[]) : byte[] =
    let bytes, programStart = realArtifact ()
    Array.append bytes.[0 .. programStart - 1] program

// -- 150.2.7 The header and the function table only ---------------------------------------------

let programReplacements : obj[] seq =
    [ "count-65535-no-statements", [| 0xFFuy; 0xFFuy |]
      "no-program-bytes", [||]
      "one-byte", [| 0x01uy |]
      "unassigned-statement-tag", [| 0x01uy; 0x00uy; 0xFFuy |]
      "garbage", Array.init 300 (fun i -> byte ((i * 73 + 5) % 256)) ]
    |> Seq.map (fun (n, b) -> [| box n; box b |])

[<Theory>]
[<MemberData(nameof programReplacements)>]
[<Trait("requirement", "JYRO-REQ-2065")>]
let ``Read_DoesNotDecodeTheProgram_SoItsBytesDoNotChangeTheResult`` (name: string, program: byte[]) =
    ignore name
    let original, _ = realArtifact ()
    let expected = accepted original
    let info = accepted (withProgram program)
    Assert.Equal(expected.FormatVersion, info.FormatVersion)
    Assert.Equal<byte[]>(expected.SourceHash.ToArray(), info.SourceHash.ToArray())
    Assert.Equal<string list>(List.ofSeq expected.RequiredFunctions, List.ofSeq info.RequiredFunctions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2065")>]
let ``Read_TheBookExample_ProgramCount65535WithNoStatements_IsReadWithoutError`` () =
    let bytes = withProgram [| 0xFFuy; 0xFFuy |]
    accepted bytes |> ignore
    // A load of the same bytes decodes the program, and refuses it.
    Assert.False((loadOnly bytes).IsSuccess)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2065")>]
let ``Read_IsNotALoad_SoATableNameNoHostRegistered_IsRead`` () =
    let bytes = encode { artifactOf [ setOut "a" (num 1m) ] with Functions = [ "NoSuchFunction" ] }
    let info = accepted bytes
    Assert.Equal<string list>([ "NoSuchFunction" ], List.ofSeq info.RequiredFunctions)
    fails MessageCode.UndefinedFunction (loadOnly bytes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2065")>]
let ``Read_ProgramBytesChangeTheResultOnlyThroughTheFileSize`` () =
    let _, programStart = realArtifact ()
    let atMax = withProgram (Array.zeroCreate (maxSize - programStart))
    Assert.Equal(maxSize, atMax.Length)
    accepted atMax |> ignore
    let overMax = withProgram (Array.zeroCreate (maxSize + 1 - programStart))
    refusedBy MessageCode.ArtifactTooLarge overMax

// -- 150.2.8 The checks, in order ---------------------------------------------------------------

let private table1001 = [ for i in 0 .. 1000 -> sprintf "F%04d" i ]
let private table1000 = [ for i in 0 .. 999 -> sprintf "F%04d" i ]

let eachCheckAlone : obj[] seq =
    let valid = build [ setOut "x" (num 1m) ]
    [ // 1: size
      "not-jyrx-11000000", Array.create 11_000_000 0x41uy, MessageCode.ArtifactTooLarge
      "valid-header-10485761", Array.append valid (Array.zeroCreate (maxSize + 1 - valid.Length)), MessageCode.ArtifactTooLarge
      // 2: magic
      "empty", [||], MessageCode.ArtifactInvalidMagic
      "JYR", "JYR"B, MessageCode.ArtifactInvalidMagic
      "JYRY-44", headerWith "JYRY"B v 0 0u, MessageCode.ArtifactInvalidMagic
      "jyrx-lowercase", Array.append "jyrx"B valid.[4..], MessageCode.ArtifactInvalidMagic
      // 3: version - every version other than 1 (150.10.2, 150.10.3, 150.10.5; JYRO-REQ-1921)
      "version-2", headerWith jyrx 2 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-7", headerWith jyrx 7 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-8", headerWith jyrx 8 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-9", headerWith jyrx 9 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-10", headerWith jyrx 10 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-256-bytes-00-01", headerWith jyrx 0x0100 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-65535", headerWith jyrx 0xFFFF 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-2-then-nothing", Array.append jyrx [| 2uy; 0uy |], MessageCode.ArtifactVersionUnsupported
      "version-9-then-nothing", Array.append jyrx [| 9uy; 0uy |], MessageCode.ArtifactVersionUnsupported
      "version-10-then-nothing", Array.append jyrx [| 10uy; 0uy |], MessageCode.ArtifactVersionUnsupported
      // Version 0, which no encoding defines (150.2.8 check 3, 150.10.3; jyro-spec--d-410 settled).
      "version-0", headerWith jyrx 0 0 0u, MessageCode.ArtifactVersionUnsupported
      "version-0-then-nothing", Array.append jyrx [| 0uy; 0uy |], MessageCode.ArtifactVersionUnsupported
      // 4: table count
      "count-1001", Array.append (header 1001u) (tableBytes table1001), MessageCode.ArtifactUnreadable
      "count-1001-no-entries", header 1001u, MessageCode.ArtifactUnreadable
      // 5: decodable
      "JYRX-version-1", Array.append jyrx [| 1uy; 0uy |], MessageCode.ArtifactUnreadable
      "JYRX-alone", jyrx, MessageCode.ArtifactUnreadable
      "JYRX-01", Array.append jyrx [| 1uy |], MessageCode.ArtifactUnreadable
      "header-43-bytes", (header 0u).[0 .. 42], MessageCode.ArtifactUnreadable
      "header-40-bytes", (header 0u).[0 .. 39], MessageCode.ArtifactUnreadable
      "table-ends-before-its-count", Array.append (header 3u) (tableBytes [ "Length" ]), MessageCode.ArtifactUnreadable
      "name-length-past-the-end", Array.concat [ header 1u; [| 0x10uy; 0x00uy |]; "Len"B ], MessageCode.ArtifactUnreadable
      "name-not-utf8", Array.concat [ header 1u; [| 2uy; 0uy; 0xC3uy; 0x28uy |] ], MessageCode.ArtifactUnreadable
      // Check 5 includes a name that is not well-formed UTF-8 (150.4.6).
      "name-C080-overlong", Array.concat [ header 1u; [| 2uy; 0uy; 0xC0uy; 0x80uy |] ], MessageCode.ArtifactUnreadable
      "name-EDA080-surrogate", Array.concat [ header 1u; [| 3uy; 0uy; 0xEDuy; 0xA0uy; 0x80uy |] ], MessageCode.ArtifactUnreadable
      "name-F4908080-above-10FFFF", Array.concat [ header 1u; [| 4uy; 0uy; 0xF4uy; 0x90uy; 0x80uy; 0x80uy |] ], MessageCode.ArtifactUnreadable
      "name-E282-truncated", Array.concat [ header 1u; [| 2uy; 0uy; 0xE2uy; 0x82uy |] ], MessageCode.ArtifactUnreadable
      // 6: duplicate names
      "duplicate-name", Array.concat [ header 2u; tableBytes [ "Length"; "Length" ]; [| 0uy; 0uy |] ], MessageCode.ArtifactUnreadable ]
    |> Seq.map (fun (n, b, c) -> [| box n; box b; box c |])

[<Theory>]
[<MemberData(nameof eachCheckAlone)>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 each check of Read refuses through its result with its code`` (name: string, bytes: byte[], code: MessageCode) =
    ignore name
    refusedBy code bytes

let checksInPairs : obj[] seq =
    [ // 1 before 2
      "oversized-and-bad-magic", Array.create (maxSize + 1) 0x00uy, MessageCode.ArtifactTooLarge
      // 2 before 3
      "bad-magic-and-version-10", headerWith "JYRY"B 10 0 0u, MessageCode.ArtifactInvalidMagic
      "bad-magic-and-version-8", headerWith "JYRY"B 8 0 0u, MessageCode.ArtifactInvalidMagic
      // 3 before 4
      "version-8-and-count-1001", Array.append (headerWith jyrx 8 0 1001u) (tableBytes table1001), MessageCode.ArtifactVersionUnsupported
      "version-10-and-count-1001", Array.append (headerWith jyrx 10 0 1001u) (tableBytes table1001), MessageCode.ArtifactVersionUnsupported
      // 3 before 5
      "version-8-and-short-header", Array.append jyrx [| 8uy; 0uy; 0uy; 0uy; 1uy; 2uy |], MessageCode.ArtifactVersionUnsupported
      "version-10-and-short-header", Array.append jyrx [| 10uy; 0uy; 0uy; 0uy; 1uy; 2uy |], MessageCode.ArtifactVersionUnsupported
      // 4 before 5
      "count-1001-and-truncated-table", Array.append (header 1001u) (tableBytes [ "A"; "B" ]), MessageCode.ArtifactUnreadable
      // 5 before 6
      "truncated-and-duplicate", Array.append (header 3u) (tableBytes [ "A"; "A" ]), MessageCode.ArtifactUnreadable ]
    |> Seq.map (fun (n, b, c) -> [| box n; box b; box c |])

[<Theory>]
[<MemberData(nameof checksInPairs)>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 when two checks of Read fail the first in order gives the code`` (name: string, bytes: byte[], code: MessageCode) =
    ignore name
    refusedBy code bytes

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 the book examples of Read`` () =
    refusedBy MessageCode.ArtifactTooLarge (Array.create 11_000_000 0x20uy)
    refusedBy MessageCode.ArtifactInvalidMagic "JYR"B
    // The 6 bytes JYRX and the format version of this specification (01 00).
    refusedBy MessageCode.ArtifactUnreadable (Array.append jyrx [| 1uy; 0uy |])
    refusedBy MessageCode.ArtifactUnreadable (header 0xFFFFFFFFu)
    // The 44-byte header JYRX, version 0, reserved 0, a hash and 0 entries (150.2.8, 150.10.3).
    refusedBy MessageCode.ArtifactVersionUnsupported (headerWith jyrx 0 0 0u)
    // JYRX, version 1, reserved FF FF, a hash, 1 entry, the name NoSuchFunction and no program: read without error.
    let info = accepted (Array.append (headerWith jyrx 1 0xFFFF 1u) (tableBytes [ "NoSuchFunction" ]))
    Assert.Equal(1, info.FormatVersion)
    Assert.Equal<string list>([ "NoSuchFunction" ], List.ofSeq info.RequiredFunctions)
    // The same with version 2 is refused at check 3 (JYRO-REQ-1921).
    refusedBy MessageCode.ArtifactVersionUnsupported (Array.append (headerWith jyrx 2 0xFFFF 1u) (tableBytes [ "NoSuchFunction" ]))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 a huge table count is refused by Read without allocating in proportion`` () =
    let _, baseline = allocating (fun () -> read (build [ setOut "x" (num 1m) ]))
    for bytes in [ header 0xFFFFFFFFu; Array.concat [ header 0xFFFFFFFFu; tableBytes [ "abc" ] ]; header 1001u ] do
        let r, allocated = allocating (fun () -> read bytes)
        Script.expectError MessageCode.ArtifactUnreadable r
        Assert.True(allocated < baseline + oneMB, sprintf "Read allocated %d bytes (a valid read: %d)" allocated baseline)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 a table of 1000 entries is read`` () =
    let bytes = Array.append (header 1000u) (tableBytes table1000)
    let info = accepted bytes
    Assert.Equal(1000, info.RequiredFunctions.Count)

/// A union whose variant ends after its last field hint, before its position (150.8.5, JYRO-REQ-2121).
let private variantCutBeforeItsPosition () : byte[] =
    let w = Writer(0x01uy)
    w.U8 0x2Euy; w.Str "U"; w.U16 1; w.Str "A"; w.U16 1; w.Str "x"; w.U8 1uy; w.U8 Hint.number
    build [ SRaw(w.Bytes()) ]

let notBindingRead : obj[] seq =
    [ "reserved-bytes-nonzero", Array.append (headerWith jyrx v 0xABCD 1u) (tableBytes [ "Length" ])
      "wrong-source-hash", Array.append (header 1u) (tableBytes [ "Length" ])
      "table-out-of-order", Array.append (header 2u) (tableBytes [ "Trim"; "Append" ])
      "name-no-call-uses", encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Nope" ] }
      "program-with-unassigned-tag", build [ SRaw(Array.append [| 0xFFuy |] (posBytes p0)) ]
      "program-list-above-10000", Array.append (header 0u) [| 0x11uy; 0x27uy |]
      // The outcome table of 150.2.8: bytes after the program (150.3.9), reserved FF FF (150.3.7),
      // an ill-formed string in the program (150.2.7), a name above the host's effect maximum (150.2.4).
      "bytes-after-program", Array.append (build [ setOut "x" (num 1m) ]) [| 0uy; 0xFFuy |]
      "reserved-FFFF", Array.append (headerWith jyrx v 0xFFFF 1u) (tableBytes [ "Now" ])
      "ill-formed-string-in-program",
        build [ setOut "x" (ERaw(Array.concat [ [| 0x01uy; 0x04uy; 3uy; 0uy; 0xEDuy; 0xA0uy; 0x80uy |]; posBytes p0 ])) ]
      // A union variant cut before its position is in the program, which Read does not decode.
      "union-variant-cut-before-its-position", variantCutBeforeItsPosition () ]
    |> Seq.map (fun (n, b) -> [| box n; box b |])

[<Theory>]
[<MemberData(nameof notBindingRead)>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 no other clause binds Read, so bytes that break only other rules are read`` (name: string, bytes: byte[]) =
    ignore name
    accepted bytes |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2124")>]
let ``JYRO-REQ-2124 checks 3 and 4 do not fail on absent bytes, check 5 refuses them`` () =
    refusedBy MessageCode.ArtifactUnreadable jyrx
    refusedBy MessageCode.ArtifactUnreadable (Array.append jyrx [| 1uy |])
    refusedBy MessageCode.ArtifactUnreadable (Array.append jyrx [| 1uy; 0uy |])
    // A version 1 header with its table count cut short.
    refusedBy MessageCode.ArtifactUnreadable (header 0u).[0 .. 41]

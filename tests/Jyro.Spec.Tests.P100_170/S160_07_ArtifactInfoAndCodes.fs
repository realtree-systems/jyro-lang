/// 160.7.7 (InvalidPadCharacter is 5705), JYRO-REQ-1702 (StringArrayOrObjectRequired is 5706), 160.7.12 (the numbers of MessageCode), 160.7.8 (the members of
/// JyroArtifactInfo), 160.7.9 (Read refuses through its result) and 160.7.10
/// (SourceLocation carries line, column and length as Int32). Members the build
/// may lack are reached by reflection or by number (FLOORS 4.1.7).
module RealTree.Jyro.Spec.Tests.S160_07_ArtifactInfoAndCodes

open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open System.Security.Cryptography
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.Artifact
open RealTree.Jyro.Spec.Tests.MessageCodeTable

// -- 160.7.7 InvalidPadCharacter is 5705 -------------------------------------------------------------------

/// `MessageCode.InvalidPadCharacter` by its number (160.7.7); the member is not in every build.
let invalidPadCharacter : MessageCode = enum<MessageCode> 5705

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1618")>]
let ``MessageCode_InvalidPadCharacter_Is5705`` () =
    Assert.Equal("RealTree.Jyro", typeof<MessageCode>.Namespace)
    if not (Enum.IsDefined(typeof<MessageCode>, invalidPadCharacter)) then Pending.notInBuild "160.7.7" "MessageCode.InvalidPadCharacter = 5705"
    Assert.Equal("InvalidPadCharacter", Enum.GetName(typeof<MessageCode>, invalidPadCharacter))
    Assert.Equal(5705, Convert.ToInt32(Enum.Parse(typeof<MessageCode>, "InvalidPadCharacter")))

// That PadLeft and PadRight raise 5705 at all is JYRO-REQ-1999's and 511's; their tests
// (S91_12_13_Pad) read the number and the stage Execute (JYRO-ADR-113 item 2).

// -- 160.7.8 The members of JyroArtifactInfo -------------------------------------------------------------

let private t = typeof<JyroArtifactInfo>

/// `ReadOnlySpan<byte>`, which F# cannot name as a type argument of `typeof`.
let private spanOfByte : Type = Type.GetType("System.ReadOnlySpan`1").MakeGenericType([| typeof<byte> |])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 JyroArtifactInfo is a public sealed class in RealTree.Jyro`` () =
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsSealed && t.IsPublic)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 JyroArtifactInfo implements IEquatable of itself and no other interface`` () =
    let names = t.GetInterfaces() |> Array.map (fun i -> i.FullName) |> Array.sort |> List.ofArray
    let expected = [ typeof<IEquatable<JyroArtifactInfo>>.FullName ]
    Assert.Equal<string list>(expected, names)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 JyroArtifactInfo has no constructor that is public or protected`` () =
    let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance
    let reachable =
        t.GetConstructors(flags)
        |> Array.filter (fun c -> c.IsPublic || c.IsFamily || c.IsFamilyOrAssembly)
        |> Array.map (fun c -> c.ToString())
    Assert.True(Array.isEmpty reachable, sprintf "JyroArtifactInfo has a public or protected constructor: %A" reachable)

/// A readable signature of a declared public member.
let private signature (m: MemberInfo) : string =
    let ps (xs: ParameterInfo[]) = String.Join(", ", xs |> Array.map (fun p -> p.ParameterType.Name))
    match m with
    | :? ConstructorInfo as c -> sprintf ".ctor(%s)" (ps (c.GetParameters()))
    | :? PropertyInfo as p ->
        let setter = if isNull (p.GetSetMethod()) then "" else " set"
        sprintf "%s : %s { get%s }" p.Name p.PropertyType.Name setter
    | :? MethodInfo as mi ->
        sprintf "%s%s(%s) : %s" (if mi.IsStatic then "static " else "") mi.Name (ps (mi.GetParameters())) mi.ReturnType.Name
    | other -> sprintf "%A %s" other.MemberType other.Name

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 JyroArtifactInfo declares exactly the nine public members`` () =
    let flags = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
    let declared =
        t.GetMembers(flags)
        |> Array.filter (fun m ->
            match m with
            // Property accessors are folded into their properties.
            | :? MethodInfo as mi -> not mi.IsSpecialName || mi.Name.StartsWith "op_"
            | _ -> true)
        |> Array.map signature
        |> Array.sort
        |> List.ofArray
    let expected =
        [ "FormatVersion : Int32 { get }"
          "SourceHash : ReadOnlyMemory`1 { get }"
          "RequiredFunctions : IReadOnlyList`1 { get }"
          "static Read(Byte[]) : JyroResult`1"
          "static Read(ReadOnlySpan`1) : JyroResult`1"
          "Equals(JyroArtifactInfo) : Boolean"
          "Equals(Object) : Boolean"
          "GetHashCode() : Int32"
          "ToString() : String" ]
        |> List.sort
    Assert.Equal<string list>(expected, declared)
    // The generic arguments of the generic types.
    Assert.Equal(typeof<IReadOnlyList<string>>, t.GetProperty("RequiredFunctions").PropertyType)
    Assert.Equal(typeof<ReadOnlyMemory<byte>>, t.GetProperty("SourceHash").PropertyType)
    Assert.Equal(typeof<JyroResult<JyroArtifactInfo>>, t.GetMethod("Read", [| typeof<byte[]> |]).ReturnType)
    Assert.Equal(typeof<JyroResult<JyroArtifactInfo>>, t.GetMethod("Read", [| spanOfByte |]).ReturnType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 JyroArtifactInfo declares no forbidden member`` () =
    let flags = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
    let names = t.GetMembers(flags) |> Array.map (fun m -> m.Name) |> Set.ofArray
    for forbidden in [ "op_Equality"; "op_Inequality"; "Deconstruct"; "CompareTo"; "Clone"; "<Clone>$" ] do
        Assert.False(names.Contains forbidden, sprintf "JyroArtifactInfo declares %s" forbidden)

let private read (bytes: byte[]) : JyroResult<JyroArtifactInfo> =
    try JyroArtifactInfo.Read bytes
    with e -> Values.fail (sprintf "JyroArtifactInfo.Read threw %s: %s" (e.GetType().FullName) e.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 Read of the book example, the Trim artifact`` () =
    let source = "Output.x = Trim(\" a \")"
    let bytes = Script.compileBytesWith pinned source |> Script.valueOf
    let info = read bytes |> Script.valueOf
    // FormatVersion 1, the format version of this specification (150.10.4, JYRO-REQ-1920).
    Assert.Equal(1, info.FormatVersion)
    Assert.Equal(int formatVersion, info.FormatVersion)
    Assert.Equal(32, info.SourceHash.Length)
    Assert.Equal<byte[]>(SHA256.HashData(Encoding.ASCII.GetBytes source), info.SourceHash.ToArray())
    Assert.Equal<byte[]>(bytes.[8..39], info.SourceHash.ToArray())
    Assert.Equal<string list>([ "Trim" ], List.ofSeq info.RequiredFunctions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 RequiredFunctions are the table in the order it holds, decoded from UTF-8`` () =
    // As written (150.3.3): ordinal order.
    let bytes = Script.compileBytesWith pinned "Output.a = Trim(\" x \")\nOutput.b = Length([1])\nOutput.c = Append([1], 2)" |> Script.valueOf
    Assert.Equal<string list>([ "Append"; "Length"; "Trim" ], List.ofSeq (read bytes |> Script.valueOf).RequiredFunctions)
    // Out of order, as a reader accepts it (150.3.6): the order held.
    let outOfOrder = encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Trim"; "Append"; "Length" ] }
    Assert.Equal<string list>([ "Trim"; "Append"; "Length" ], List.ofSeq (read outOfOrder |> Script.valueOf).RequiredFunctions)
    // Names with 2-, 3- and 4-byte UTF-8 code points.
    let names = [ "Fé"; "G日"; "H\U0001F600" ]
    let info = read (encode { artifactOf [ setOut "x" (num 1m) ] with Functions = names }) |> Script.valueOf
    Assert.Equal<string list>(names, List.ofSeq info.RequiredFunctions)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 FormatVersion and SourceHash are those of the header`` () =
    let hash = Array.init 32 byte
    let a = { artifactOf [ setOut "x" (num 1m) ] with Hash = hash }
    let info = read (encode a) |> Script.valueOf
    Assert.Equal(1, info.FormatVersion)
    Assert.Equal<byte[]>(hash, info.SourceHash.ToArray())

// The test that Read of a span gives the same result as Read of an array is in the C# tests
// (tests/Jyro.CSharp.Tests/HostErrorTests.cs): a ReadOnlySpan<byte> cannot be boxed for reflection and
// F# cannot pass one through a lambda.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 two values are Equals exactly when version, hash bytes and names in order are equal, with equal hash codes`` () =
    let source = "Output.x = Trim(\" a \")"
    let bytes = Script.compileBytesWith pinned source |> Script.valueOf
    let a = read bytes |> Script.valueOf
    let b = read (Array.copy bytes) |> Script.valueOf
    Assert.True(a.Equals b)
    Assert.True((a :> obj).Equals(b :> obj))
    Assert.Equal(a.GetHashCode(), b.GetHashCode())
    Assert.False(a.Equals(Unchecked.defaultof<JyroArtifactInfo>))
    Assert.False((a :> obj).Equals("not an info"))
    // A different hash: not equal.
    let otherHash = { artifactOf [ setOut "x" (num 1m) ] with Hash = Array.create 32 7uy; Functions = [ "Trim" ] }
    Assert.False(a.Equals(read (encode otherHash) |> Script.valueOf))
    // The same hash and names in a different order: not equal; in the same order: equal.
    let hash = Array.init 32 byte
    let ab = read (encode { artifactOf [ setOut "x" (num 1m) ] with Hash = hash; Functions = [ "A"; "B" ] }) |> Script.valueOf
    let ab2 = read (encode { artifactOf [ setOut "y" (num 2m) ] with Hash = hash; Functions = [ "A"; "B" ] }) |> Script.valueOf
    let ba = read (encode { artifactOf [ setOut "x" (num 1m) ] with Hash = hash; Functions = [ "B"; "A" ] }) |> Script.valueOf
    Assert.True(ab.Equals ab2)
    Assert.Equal(ab.GetHashCode(), ab2.GetHashCode())
    Assert.False(ab.Equals ba)
    // A different set of names: not equal.
    let abc = read (encode { artifactOf [ setOut "x" (num 1m) ] with Hash = hash; Functions = [ "A"; "B"; "C" ] }) |> Script.valueOf
    Assert.False(ab.Equals abc)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 ToString is one line with the decimal version, the 64 lowercase hexadecimal digits of the hash and each name`` () =
    let source = "Output.x = Trim(\" a \")"
    let bytes = Script.compileBytesWith pinned source |> Script.valueOf
    let info = read bytes |> Script.valueOf
    let text = info.ToString()
    let hex = String.Join("", SHA256.HashData(Encoding.UTF8.GetBytes source) |> Array.map (fun b -> b.ToString("x2")))
    Assert.Equal(64, hex.Length)
    Assert.Contains("1", text)
    Assert.Contains(hex, text)
    Assert.Contains("Trim", text)
    Assert.DoesNotContain("\n", text)
    Assert.DoesNotContain("\r", text)
    // Not the F# array form of the hash that a record printed.
    Assert.DoesNotContain("[|", text)
    // Several names: each appears.
    let many = read (encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ "Append"; "Length"; "Trim" ] }) |> Script.valueOf
    for name in [ "Append"; "Length"; "Trim" ] do Assert.Contains(name, many.ToString())
    Assert.DoesNotContain("\n", many.ToString())

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1898")>]
[<InlineData("a\nb", "a\\u000ab")>]
[<InlineData("a\rb", "a\\u000db")>]
[<InlineData("a\u0000b", "a\\u0000b")>]
[<InlineData("a\u007fb", "a\\u007fb")>]
[<InlineData("a\u0085b", "a\\u0085b")>]
[<InlineData("a b", "a\\u2028b")>]
[<InlineData("a b", "a\\u2029b")>]
[<InlineData("a\\b", "a\\\\b")>]
[<InlineData("a\\u000ab", "a\\\\u000ab")>]
[<InlineData("aé日b", "aé日b")>]
let ``JYRO-REQ-1898 ToString writes a function name escaped, so that it is one line for every name 150.2.8 accepts`` (name: string, written: string) =
    let info = read (encode { artifactOf [ setOut "x" (num 1m) ] with Functions = [ name ] }) |> Script.valueOf
    // The names are held as they are.
    Assert.Equal<string list>([ name ], List.ofSeq info.RequiredFunctions)
    let text = info.ToString()
    Assert.Contains(written, text)
    Assert.DoesNotContain("\n", text)
    Assert.DoesNotContain("\r", text)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1898")>]
let ``JYRO-REQ-1898 a value read does not change when the bytes it was read from change`` () =
    let bytes = Script.compileBytesWith pinned "Output.x = Trim(\" a \")" |> Script.valueOf
    let info = read bytes |> Script.valueOf
    let hashBefore = info.SourceHash.ToArray()
    let namesBefore = List.ofSeq info.RequiredFunctions
    for i in 0 .. bytes.Length - 1 do bytes.[i] <- 0xFFuy
    Assert.Equal<byte[]>(hashBefore, info.SourceHash.ToArray())
    Assert.Equal<string list>(namesBefore, List.ofSeq info.RequiredFunctions)
    Assert.Equal(1, info.FormatVersion)

// -- 160.7.9 Read refuses through its result, not by throwing -------------------------------------------------

let private headerWith (version: int) (count: uint32) : byte[] =
    let w = Writer(1uy)
    w.Raw "JYRX"B
    w.U16 version
    w.U16 0
    w.Raw(sha256 "h")
    w.U32 count
    w.Bytes()

let private failedChecks () : (string * byte[] * MessageCode) list =
    [ "1-size", Array.create 11_000_000 0x41uy, MessageCode.ArtifactTooLarge
      "2-magic", "JYR"B, MessageCode.ArtifactInvalidMagic
      "3-version-0", headerWith 0 0u, MessageCode.ArtifactVersionUnsupported
      "4-count", headerWith 1 0xFFFFFFFFu, MessageCode.ArtifactUnreadable
      "5-JYRX-version-1", Array.append "JYRX"B [| 1uy; 0uy |], MessageCode.ArtifactUnreadable ]
// The checks of 150.2.8 that only JYRO-REQ-2075 (a table name that is not well-formed UTF-8,
// C0 80) and JYRO-REQ-2070 (a table name two times) make are in those requirements' tests
// (S150_11_HeaderStringsFlagsPositions, S150_03_Layout), which read the refusal in this
// form (JYRO-ADR-113 item 2, JYRO-DEF-506).

let failedCheckNames : obj[] seq = failedChecks () |> Seq.map (fun (n, _, _) -> [| box n |])

/// Assert the failure form of 160.7.9.
let private failedForm (code: MessageCode) (r: JyroResult<JyroArtifactInfo>) : unit =
    Assert.False(r.IsSuccess)
    Assert.False(r.HasValue)
    let found, _ = r.TryGetValue()
    Assert.False(found)
    Assert.Equal(1, r.Messages.Count)
    let m = r.Messages.[0]
    Assert.Equal(MessageSeverity.Error, m.Severity)
    Assert.Equal(code, m.Code)
    Assert.Equal("Deserialize", Pending.stageOf m)
    Assert.False(m.Location.HasValue, "a refusal of Read has no source location")

[<Theory>]
[<MemberData(nameof failedCheckNames)>]
[<Trait("requirement", "JYRO-REQ-2106")>]
let ``Read_EachFailedCheck_ReturnsAFailedResultWithOneDeserializeError`` (check: string) =
    let _, bytes, code = failedChecks () |> List.find (fun (n, _, _) -> n = check)
    failedForm code (read bytes)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2106")>]
let ``Read_WhenEveryCheckPasses_Succeeds`` () =
    let bytes = Script.compileBytesWith pinned "Output.x = Trim(\" a \")" |> Script.valueOf
    let r = read bytes
    Assert.True(r.IsSuccess)
    Assert.True(r.HasValue)
    let found, v = r.TryGetValue()
    Assert.True(found)
    Assert.NotNull v
    Assert.Empty r.Messages

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2106")>]
let ``Read_Null_ThrowsArgumentNullException`` () =
    Assert.Throws<ArgumentNullException>(fun () -> JyroArtifactInfo.Read null |> ignore) |> ignore

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2106")>]
let ``Read_TheBookExample_JYR`` () =
    failedForm MessageCode.ArtifactInvalidMagic (read "JYR"B)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2106")>]
let ``Read_NeverThrows_ForAnyNonNullArray`` () =
    let valid = Script.compileBytesWith pinned "Output.x = Trim(\" a \")\nOutput.y = Length([1, 2])" |> Script.valueOf
    let tableEnd =
        let count = int (BitConverter.ToUInt32(valid, 40))
        let mutable offset = 44
        for _ in 1 .. count do offset <- offset + 2 + int (BitConverter.ToUInt16(valid, offset))
        offset
    let inputs =
        [ for n in 0 .. valid.Length - 1 -> valid.[0 .. n - 1]
          for i in 0 .. tableEnd - 1 do
            for b in [ 0x00uy; 0x01uy; 0x7Fuy; 0x80uy; 0xFFuy ] do
                let c = Array.copy valid
                c.[i] <- b
                yield c
          let rng = Random(1606)
          for _ in 1 .. 10_000 do
            let a = Array.zeroCreate<byte> (rng.Next(0, 201))
            rng.NextBytes a
            if a.Length >= 4 && rng.Next(2) = 0 then Array.blit "JYRX"B 0 a 0 4
            yield a ]
    for bytes in inputs do
        let r =
            try JyroArtifactInfo.Read bytes
            with e -> Values.fail (sprintf "Read threw %s for %d bytes: %s" (e.GetType().Name) bytes.Length e.Message)
        if not r.IsSuccess then
            Assert.Equal(1, r.Messages.Count)
            Assert.Equal(MessageSeverity.Error, r.Messages.[0].Severity)
            Assert.False(r.HasValue)
        else
            Assert.True(r.HasValue)
            Assert.Empty r.Messages

// -- 160.7.10 SourceLocation carries line, column and length as Int32 --------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2107")>]
let ``SourceLocation_Shape`` () =
    let loc = typeof<DiagnosticMessage>.GetProperty("Location", BindingFlags.Public ||| BindingFlags.Instance)
    Assert.NotNull loc
    Assert.NotNull(loc.GetGetMethod())
    Assert.Equal(typeof<Nullable<SourceLocation>>, loc.PropertyType)
    let s = typeof<SourceLocation>
    Assert.Equal("RealTree.Jyro", s.Namespace)
    Assert.True(s.IsValueType && s.IsPublic, "SourceLocation is a public struct")
    for name in [ "Line"; "Column"; "Length" ] do
        let p = s.GetProperty(name, BindingFlags.Public ||| BindingFlags.Instance)
        Assert.True(not (isNull p), sprintf "SourceLocation.%s is missing" name)
        Assert.Equal(typeof<int>, p.PropertyType)
        Assert.NotNull(p.GetGetMethod())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2107")>]
let ``Location_TheBookExample`` () =
    let r = Script.compileWith pinned "var s: string"
    let d = Script.onlyErrorMessage r
    Assert.Equal(MessageCode.TypedVariableNeedsInitializer, d.Code)
    Assert.Equal("Validate", Pending.stageOf d)
    Assert.True(d.Location.HasValue, "no Location")
    Assert.Equal(1, d.Location.Value.Line)
    Assert.Equal(1, d.Location.Value.Column)
    Assert.Equal(13, d.Location.Value.Length)

// The place of a runtime error of a call on a source (ToUpper([1]) at 2/12/12, 130.1.2) is
// JYRO-REQ-2122's (which replaced JYRO-REQ-1475): S130_Diagnostics, 'JYRO-REQ-2122 a diagnostic at a place has its line, column and length'.
// The place on a loaded node written as -3/0/50/40 (-3, 0, 0, 150.4.7) is JYRO-REQ-2076's
// alone under JYRO-ADR-122 item 3: S130_Diagnostics, 'JYRO-REQ-2076
// LoadedProgram_PatchedPosition_IsReportedAsWritten'. Neither is asserted again here
// (JYRO-ADR-113 item 2, JYRO-DEF-506, JYRO-DEF-545).

// -- 160.7.11 ValueSizeLimitExceeded is 5919 -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1622")>]
let ``MessageCode_ValueSizeLimitExceeded_Is5919`` () =
    // By number and by name: the member is not in every build (FLOORS 4.1.7).
    Assert.Equal("RealTree.Jyro", typeof<MessageCode>.Namespace)
    let code = enum<MessageCode> 5919
    if not (Enum.IsDefined(typeof<MessageCode>, code)) then Pending.notInBuild "160.7.11" "MessageCode.ValueSizeLimitExceeded = 5919"
    Assert.Equal("ValueSizeLimitExceeded", Enum.GetName(typeof<MessageCode>, code))
    Assert.Equal(5919, Convert.ToInt32(Enum.Parse(typeof<MessageCode>, "ValueSizeLimitExceeded")))

// -- 160.7.12 The numbers of MessageCode -------------------------------------------------------------------

let messageCodeRows : obj[] seq = messageCodeTable |> Seq.map (fun (n, name) -> [| box name; box n |])

/// The numeric value of the member `name` of MessageCode, or None when there is no such member.
let private memberValue (name: string) : int64 option =
    let f = typeof<MessageCode>.GetField(name, BindingFlags.Public ||| BindingFlags.Static)
    if isNull f then None else Some(Convert.ToInt64(f.GetRawConstantValue()))

[<Theory>]
[<MemberData(nameof messageCodeRows)>]
[<Trait("requirement", "JYRO-REQ-2138")>]
let ``JYRO-REQ-2138 MessageCode_HasEachMemberOfTheTable_WithTheTablesNumber`` (name: string, number: int) =
    tableAsCopiedIsWhole ()
    let t = typeof<MessageCode>
    Assert.True(t.IsEnum && t.IsPublic, "MessageCode is a public enumeration")
    Assert.Equal("RealTree.Jyro", t.Namespace)
    match memberValue name with
    | None -> Values.fail (sprintf "160.7.12: MessageCode has no member %s (the table gives it %d)" name number)
    | Some v -> Assert.True((v = int64 number), sprintf "160.7.12: MessageCode.%s is %d; the table gives %d" name v number)

[<Theory>]
[<InlineData("UnexpectedEndOfSource", 2001)>]
[<InlineData("SourceNestingLimitExceeded", 2002)>]
[<InlineData("TypedVariableNeedsInitializer", 3302)>]
[<InlineData("HostLinkError", 4001)>]
[<InlineData("MaxEffectNotApplicable", 4106)>]
[<InlineData("TooFewArguments", 4200)>]
[<InlineData("StringOperandInAdditionRuntime", 5210)>]
[<InlineData("InvalidSortDirection", 5920)>]
[<Trait("requirement", "JYRO-REQ-2138")>]
let ``JYRO-REQ-2138 MessageCode_TheClausesExamples_HaveTheirNumbers`` (name: string, number: int) =
    match memberValue name with
    | None -> Values.fail (sprintf "160.7.12: MessageCode has no member %s" name)
    | Some v -> Assert.Equal(int64 number, v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2138")>]
let ``JYRO-REQ-2138 MessageCode_NoTwoMembersShareANumber`` () =
    // Over the declared fields, so that an alias (two names, one value) is seen:
    // Enum.GetValues would fold it into one value.
    let shared =
        typeof<MessageCode>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.groupBy (fun f -> Convert.ToInt64(f.GetRawConstantValue()))
        |> Array.filter (fun (_, fs) -> fs.Length > 1)
        |> Array.map (fun (v, fs) -> sprintf "%d: %s" v (String.Join(", ", fs |> Array.map (fun f -> f.Name))))
    Assert.True(Array.isEmpty shared, "160.7.12: members share a number: " + String.Join("; ", shared))

/// The repository root: the nearest directory above the test assembly that holds Jyro.slnx.
let private repoRootOf1716 () : string =
    let rec up (dir: IO.DirectoryInfo) =
        if isNull dir then failwith "Jyro.slnx not found above the test assembly"
        elif IO.File.Exists(IO.Path.Combine(dir.FullName, "Jyro.slnx")) then dir.FullName
        else up dir.Parent
    up (IO.DirectoryInfo(AppContext.BaseDirectory))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2138")>]
let ``JYRO-REQ-2138 no number of the table is a retired number`` () =
    // tests/Jyro.CSharp.Tests/RetiredMessageCodes.txt: a number, then the names it has carried.
    let path = IO.Path.Combine(repoRootOf1716 (), "tests", "Jyro.CSharp.Tests", "RetiredMessageCodes.txt")
    let retired =
        IO.File.ReadAllLines path
        |> Array.filter (fun l -> l.Trim() <> "" && not (l.TrimStart().StartsWith "#"))
        |> Array.choose (fun l ->
            match Int32.TryParse((l.Trim().Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)).[0]) with
            | true, n -> Some n
            | _ -> None)
        |> Set.ofArray
    tableAsCopiedIsWhole ()
    let clash = messageCodeTable |> List.filter (fun (n, _) -> retired.Contains n)
    Assert.True(List.isEmpty clash, sprintf "160.7.12: numbers of the table that are retired: %A" clash)

/// A member outside the table is allowed (160.7.12) if its number is not one the table gives.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-2138")>]
let ``JYRO-REQ-2138 a member outside the table has a number that the table does not give`` () =
    let tableNumbers = messageCodeTable |> List.map fst |> Set.ofList
    let tableNames = messageCodeTable |> List.map snd |> Set.ofList
    let extra =
        typeof<MessageCode>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.filter (fun f -> not (tableNames.Contains f.Name))
        |> Array.filter (fun f -> tableNumbers.Contains(Convert.ToInt32(f.GetRawConstantValue())))
        |> Array.map (fun f -> f.Name)
    Assert.True(Array.isEmpty extra, "160.7.12: members outside the table that carry a number of the table: " + String.Join(", ", extra))

// -- JYRO-REQ-1919 Identifiers spell Artifact and Canceled, and each family of codes has one name shape -----------
//
// The members of 160.7.12 that JYRO-ADR-196 renames, with their numbers and the older name each must not have.
//
// The older names that are British spellings are made from the new name at run time, so that no file of
// the tests holds a form that the spelling check of JYRO-REQ-1938 flags.

/// `name` with each US form respelled as the British form it replaced: "Arte" + "fact", "Cance" + "lled".
let private britishOf (name: string) : string =
    name.Replace("Artifact", "Arte" + "fact").Replace("Canceled", "Cance" + "lled")

let renamedCodes : obj[] seq =
    [ 4106, "MaxEffectNotApplicable", "EffectMaxNotApplicable"
      5003, "CanceledByHost", britishOf "CanceledByHost"
      5005, "ValueIsReadOnly", "ReadOnlyValue"
      5007, "InputValueIsReadOnly", "CannotWriteToInput"
      5210, "StringOperandInAdditionRuntime", "AddWithStringOperand"
      7000, "ArtifactTooLarge", britishOf "ArtifactTooLarge"
      7001, "ArtifactInvalidMagic", britishOf "ArtifactInvalidMagic"
      7002, "ArtifactVersionUnsupported", britishOf "ArtifactVersionUnsupported"
      7003, "ArtifactUnreadable", britishOf "ArtifactUnreadable"
      7004, "ArtifactEffectExceedsMax", britishOf "ArtifactEffectExceedsMax"
      7005, "ArtifactCannotEncodeLibraries", britishOf "ArtifactCannotEncodeLibraries"
      7006, "ArtifactCannotUseLibraries", britishOf "ArtifactCannotUseLibraries"
      7007, "ArtifactCannotEncode", britishOf "ArtifactCannotEncode"
      7008, "ArtifactUnwritable", britishOf "ArtifactUnwritable" ]
    |> Seq.map (fun (n, name, old) -> [| box n; box name; box old |])

[<Theory>]
[<MemberData(nameof renamedCodes)>]
[<Trait("requirement", "JYRO-REQ-1919")>]
let ``JYRO-REQ-1919 MessageCode has the renamed member with its number and not the older name`` (number: int, name: string, notAMember: string) =
    let t = typeof<MessageCode>
    match memberValue name with
    | None -> Values.fail (sprintf "JYRO-REQ-1919: MessageCode has no member %s (the number is %d)" name number)
    | Some v -> Assert.True((v = int64 number), sprintf "JYRO-REQ-1919: MessageCode.%s is %d, not %d" name v number)
    Assert.Equal(name, Enum.GetName(t, enum<MessageCode> number))
    Assert.False(Enum.IsDefined(t, notAMember), sprintf "JYRO-REQ-1919: MessageCode still has the member %s" notAMember)
    Assert.True(isNull (t.GetField(notAMember, BindingFlags.Public ||| BindingFlags.Static)), sprintf "JYRO-REQ-1919: MessageCode still declares %s" notAMember)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1919")>]
let ``JYRO-REQ-1919 the casts and the Enum.IsDefined examples of the clause`` () =
    Assert.Equal(5007, int MessageCode.InputValueIsReadOnly)
    Assert.Equal(7003, int MessageCode.ArtifactUnreadable)
    Assert.Equal(5003, int MessageCode.CanceledByHost)
    Assert.False(Enum.IsDefined(typeof<MessageCode>, britishOf "ArtifactUnreadable"))
    Assert.False(Enum.IsDefined(typeof<MessageCode>, britishOf "CanceledByHost"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1919")>]
let ``JYRO-REQ-1919 a write by a script to Input raises InputValueIsReadOnly`` () =
    // A write through a variable that holds part of Input is governed at run time (70.2.2).
    let r = Script.runJson "{\"a\":{\"k\":1}}" "var t = Input.a\nt.k = 2"
    Script.expectError MessageCode.InputValueIsReadOnly r
    Assert.Equal(5007, int (Script.onlyError r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1919")>]
let ``JYRO-REQ-1919 no public type or member of the shipped assemblies has a name with a British form of Artifact or Canceled`` () =
    let shipped = [ "Jyro.Core"; "Jyro.Parser"; "Jyro.Compiler"; "Jyro.Stdlib"; "Jyro.Api" ]
    let flags = BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
    let offenders =
        [ for assemblyName in shipped do
            for ty in (Assembly.Load assemblyName).GetExportedTypes() do
                yield ty.FullName
                for m in ty.GetMembers flags do
                    yield sprintf "%s.%s" ty.FullName m.Name ]
        |> List.filter (fun n ->
            n.Contains(britishOf "Artifact", StringComparison.OrdinalIgnoreCase) || n.Contains(britishOf "Canceled", StringComparison.OrdinalIgnoreCase))
    Assert.True(List.isEmpty offenders, sprintf "JYRO-REQ-1919: public names with a British form: %A" offenders)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1919")>]
let ``JYRO-REQ-1919 the type that describes an artifact is JyroArtifactInfo`` () =
    let found = typeof<JyroCompiler>.Assembly.GetType "RealTree.Jyro.JyroArtifactInfo"
    Assert.NotNull found
    Assert.True(isNull (typeof<JyroCompiler>.Assembly.GetType ("RealTree.Jyro.Jyro" + britishOf "ArtifactInfo")))
    Assert.Equal(typeof<JyroArtifactInfo>, found)

// -- JYRO-REQ-1685 DuplicateFieldName is 3610; JYRO-REQ-1686 MatchDuplicateBinding is 3611 --------------
//
// JYRO-ADR-136, for JYRO-DEF-530 and JYRO-DEF-531. The members are reached by name and by number,
// by reflection: they are not in every build (FLOORS 4.1.7). Neither is a row of 160.7.12's table,
// which JYRO-REQ-2138 lets MessageCode exceed, so the table is not changed here.

/// The member `name` of MessageCode, asserted to have `number`; a build without it fails the
/// test as `ref`'s own failure.
let private requireCode (ref: string) (name: string) (number: int) : MessageCode =
    let t = typeof<MessageCode>
    Assert.True(t.IsEnum && t.IsPublic, "MessageCode is a public enumeration")
    Assert.Equal("RealTree.Jyro", t.Namespace)
    match memberValue name with
    | None -> Pending.notInBuild ref (sprintf "MessageCode.%s = %d" name number)
    | Some v ->
        Assert.True((v = int64 number), sprintf "%s: MessageCode.%s is %d, not %d" ref name v number)
        Assert.Equal(name, Enum.GetName(t, enum<MessageCode> number))
        Assert.Equal(number, Convert.ToInt32(Enum.Parse(t, name)))
        enum<MessageCode> number

/// `r` failed, and has an Error of `code` (by its number) at the stage Validate.
let private reportsAtValidate (what: string) (code: MessageCode) (r: JyroResult<'T>) : unit =
    let all = [ for m in r.Messages -> sprintf "%A (%d) %A at %s" m.Code (int m.Code) m.Severity (Pending.stageOf m) ]
    Assert.False(r.IsSuccess, sprintf "%s: succeeded. Diagnostics: %A" what all)
    let hits = [ for m in r.Messages do if int m.Code = int code && m.Severity = MessageSeverity.Error then yield m ]
    if hits.IsEmpty then Values.fail (sprintf "%s: no Error with the code %A (%d). Diagnostics: %A" what code (int code) all)
    for m in hits do
        Assert.True((m.Code = code), sprintf "%s: the diagnostic's code is not MessageCode.%A" what code)
        Assert.True((Pending.stageOf m = "Validate"), sprintf "%s: %A has the stage %s, not Validate" what code (Pending.stageOf m))

/// The text, given as a script to Compile() and CompileToArtifact(), as a library to the compiler
/// and to JyroLibrary.Parse (160.1.1, 160.6): each reports `code` at Validate.
let private reportedOnEveryPath (code: MessageCode) (text: string) : unit =
    reportsAtValidate (sprintf "%A (Compile)" text) code (Script.compileWith pinned text)
    reportsAtValidate (sprintf "%A (CompileToArtifact)" text) code (Script.compileBytesWith pinned text)
    reportsAtValidate (sprintf "%A (library)" text) code (Script.compileWith { pinned with Libraries = [ text ] } "Output.x = 1")
    reportsAtValidate (sprintf "%A (JyroLibrary.Parse)" text) code (JyroLibrary.Parse text)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1685")>]
let ``JYRO-REQ-1685 MessageCode.DuplicateFieldName is 3610`` () =
    requireCode "JYRO-REQ-1685" "DuplicateFieldName" 3610 |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1685")>]
[<InlineData("union U A(x, x) end")>]
[<InlineData("union U A(x, x: number) end")>]
let ``JYRO-REQ-1685 a repeated field is reported through the host API with the code 3610`` (text: string) =
    let code = requireCode "JYRO-REQ-1685" "DuplicateFieldName" 3610
    reportedOnEveryPath code text

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1686")>]
let ``JYRO-REQ-1686 MessageCode.MatchDuplicateBinding is 3611`` () =
    requireCode "JYRO-REQ-1686" "MatchDuplicateBinding" 3611 |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1686")>]
[<InlineData("union U A(x, z) end\nfunc F(v)\n  return match v do case A(y, y) then y end\nend")>]
[<InlineData("union U A(x, z) end\nfunc F(v)\n  match v do\n    case A(_, _) then return 0\n  end\n  return 1\nend")>]
let ``JYRO-REQ-1686 a name repeated in a match case is reported through the host API with the code 3611`` (text: string) =
    let code = requireCode "JYRO-REQ-1686" "MatchDuplicateBinding" 3611
    reportedOnEveryPath code text

/// The two numbers are the next free ones of the 3600 band and are not retired: no other member
/// of MessageCode has either number (160.7.12 lets the enumeration have further members).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1685")>]
[<Trait("requirement", "JYRO-REQ-1686")>]
let ``JYRO-REQ-1685 JYRO-REQ-1686 3610 and 3611 are each one member's number`` () =
    for name, number in [ "DuplicateFieldName", 3610; "MatchDuplicateBinding", 3611 ] do
        let holders =
            typeof<MessageCode>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.filter (fun f -> Convert.ToInt64(f.GetRawConstantValue()) = int64 number)
            |> Array.map (fun f -> f.Name)
            |> List.ofArray
        Assert.Equal<string list>([ name ], holders)

// -- JYRO-REQ-1702 StringArrayOrObjectRequired is 5706 ------------------------------------------------------
//
// JYRO-ADR-164 item 3, for JYRO-DEF-651: the code of Length's source (JYRO-REQ-1701, 92.1.6). Reached by
// name and by number, by reflection: it is not in every build (FLOORS 4.1.7). Not a row of 160.7.12's
// table, which JYRO-REQ-2138 lets MessageCode exceed, so the table is not changed here.

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1702")>]
let ``JYRO-REQ-1702 MessageCode.StringArrayOrObjectRequired is 5706`` () =
    requireCode "JYRO-REQ-1702" "StringArrayOrObjectRequired" 5706 |> ignore

/// 5706 is the next free number of the 5700 band and is not retired: no other member has it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1702")>]
let ``JYRO-REQ-1702 5706 is one member's number`` () =
    let holders =
        typeof<MessageCode>.GetFields(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.filter (fun f -> Convert.ToInt64(f.GetRawConstantValue()) = 5706L)
        |> Array.map (fun f -> f.Name)
        |> List.ofArray
    Assert.Equal<string list>([ "StringArrayOrObjectRequired" ], holders)

/// The diagnostic of `Length(5)`, run through the host API, has the code 5706.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1702")>]
let ``JYRO-REQ-1702 Length of a number is reported through the host API with the code 5706`` () =
    let code = requireCode "JYRO-REQ-1702" "StringArrayOrObjectRequired" 5706
    let r = Script.run "Output.r = Length(5)"
    let m = Script.onlyErrorMessage r
    Assert.True((int m.Code = 5706), sprintf "Length(5) reports %A (%d), not 5706. Diagnostics:\n%s" m.Code (int m.Code) (Script.describe r))
    Assert.Equal(code, m.Code)

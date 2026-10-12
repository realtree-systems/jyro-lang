namespace RealTree.Jyro

open System
open System.Collections.Generic
open RealTree.Jyro.Compilation

/// <summary>
/// What a compiled <c>.jyrx</c> artifact declares about itself, readable without
/// decoding the program inside it.
///
/// The question the header answers is a real host question, and jyro-cli's
/// <c>validate</c> command asks it: does this artifact correspond to this source
/// file, and what does it need to run?
///
/// Reading this reads the header and the function table only. It does not
/// decode the program, so it stays cheap for a pre-flight check over many
/// artifacts.
///
/// A sealed class with no public constructor and value equality: a host gets one only from <c>Read</c>, nothing on it can be
/// changed, and two reads of the same artifact are <c>Equals</c>.
/// </summary>
[<Sealed>]
type JyroArtifactInfo private (formatVersion: int, sourceHash: byte[], requiredFunctions: string[]) =
    // Copies the reader made, held by no one else: the hash is handed out
    // as a read-only view and the names as a read-only list.
    let names = Array.AsReadOnly(requiredFunctions)

    /// <summary>
    /// The <c>.jyrx</c> format version of the header, not the
    /// version of the script inside.
    /// </summary>
    member _.FormatVersion : int = formatVersion

    /// <summary>
    /// The 32 bytes of the hash (Secure Hash Algorithm 2, 256 bits) of the source the artifact was built
    /// from, in the order they are written, for checking that
    /// a <c>.jyrx</c> still corresponds to the <c>.jyro</c> beside it.
    /// </summary>
    member _.SourceHash : ReadOnlyMemory<byte> = ReadOnlyMemory<byte>(sourceHash)

    /// <summary>
    /// The names of the function table, in the order the table holds them:
    /// the functions the artifact expects the host to supply at load time. An
    /// artifact carries no record of the effect maximum it was built under,
    /// so the loading host applies its own.
    /// </summary>
    member _.RequiredFunctions : IReadOnlyList<string> = names

    /// <summary>
    /// Reads an artifact's header and function table. Fails
    /// with <c>ArtifactTooLarge</c>, <c>ArtifactInvalidMagic</c>,
    /// <c>ArtifactVersionUnsupported</c> or <c>ArtifactUnreadable</c> rather than
    /// throwing, like every other entry point that returns a <c>JyroResult</c>: the failed result has no value and one diagnostic, of
    /// the stage Deserialize and with no location. A null array is
    /// a mistake in the caller's own code, not bytes to refuse, so it throws
    /// <c>ArgumentNullException</c>.
    /// </summary>
    static member Read(bytes: byte[]) : JyroResult<JyroArtifactInfo> =
        if obj.ReferenceEquals(bytes, null) then raise (ArgumentNullException(nameof bytes))
        let read = BinaryFormat.readHeader bytes
        match read with
        | Produced header ->
            JyroResult<JyroArtifactInfo>.Success(
                JyroArtifactInfo(header.Version, Array.copy header.SourceHash, List.toArray header.RequiredFunctions))
        | _ ->
            JyroResult<JyroArtifactInfo>.Failure(
                read.Messages |> Seq.map (fun (m: DiagnosticMessage) -> m.WithStage MessageStage.Deserialize))

    /// <summary>
    /// Reads an artifact's header and function table from a span, with the
    /// result <c>Read(byte[])</c> gives for the same bytes.
    /// </summary>
    static member Read(bytes: ReadOnlySpan<byte>) : JyroResult<JyroArtifactInfo> =
        JyroArtifactInfo.Read(bytes.ToArray())

    /// <summary>
    /// Whether <c>other</c> has the same format version, the same hash bytes and
    /// the same function names in the same order.
    /// A null <c>other</c> is not equal.
    /// </summary>
    member this.Equals(other: JyroArtifactInfo | null) : bool =
        match other with
        | null -> false
        | other ->
            formatVersion = other.FormatVersion
            && this.SourceHash.Span.SequenceEqual(other.SourceHash.Span)
            && names.Count = other.RequiredFunctions.Count
            && Seq.forall2 (fun (a: string) (b: string) -> String.Equals(a, b, StringComparison.Ordinal))
                   names other.RequiredFunctions

    /// <summary>
    /// Whether <c>other</c> is a <c>JyroArtifactInfo</c> equal to this one; a null
    /// <c>other</c> is not.
    /// </summary>
    override this.Equals(other: obj | null) : bool =
        match other with
        | :? JyroArtifactInfo as info -> this.Equals(info)
        | _ -> false

    override _.GetHashCode() : int =
        let hash = HashCode()
        hash.Add(formatVersion)
        for b in sourceHash do hash.Add(b)
        for name in requiredFunctions do hash.Add(name, StringComparer.Ordinal)
        hash.ToHashCode()

    /// <summary>
    /// One line: the format version, the hash as 64 lowercase hexadecimal
    /// digits, and the required function names, each escaped:
    /// a backslash as two backslashes, and each control character (general
    /// category Cc) and U+2028 and U+2029 as <c>\u</c> and the 4 lowercase
    /// hexadecimal digits of its code point. A name read from an artifact the
    /// host has not trusted yet can hold a line break, and written as it is it
    /// could forge a second line of a log.
    /// </summary>
    override _.ToString() : string =
        let digits = "0123456789abcdef"
        let hex =
            String(
                [| for b in sourceHash do
                       yield digits.[int b >>> 4]
                       yield digits.[int b &&& 0xF] |])
        let escape (name: string) =
            let text = Text.StringBuilder(name.Length)
            for c in name do
                if c = '\\' then
                    text.Append("\\\\") |> ignore
                elif Char.IsControl c || c = ' ' || c = ' ' then
                    text.Append("\\u").Append((int c).ToString("x4", Globalization.CultureInfo.InvariantCulture)) |> ignore
                else
                    text.Append(c) |> ignore
            text.ToString()
        let functions = String.Join(", ", requiredFunctions |> Array.map escape)
        String.Format(
            Globalization.CultureInfo.InvariantCulture,
            "FormatVersion {0}, SourceHash {1}, RequiredFunctions [{2}]",
            formatVersion, hex, functions)

    interface IEquatable<JyroArtifactInfo> with
        member this.Equals(other: JyroArtifactInfo | null) = this.Equals(other)

namespace RealTree.Jyro.Tests.Compiler

open System
open System.Security.Cryptography
open System.Text
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests.ParseShape

/// Tests for the `.jyrx` artifact format's encoding boundaries.
module ArtifactFormat =

    let private hashOf (source: string) =
        SHA256.HashData(Encoding.UTF8.GetBytes(source))

    /// `serialize` returns a `JyroResult` now, matching `deserialize`. These
    /// tests are about what the bytes contain, so they unwrap and fail loudly.
    let private serializeOrFail program names hash : byte[] =
        match BinaryFormat.serialize program names hash with
        | Produced bytes -> bytes
        | r -> failwithf "serialize failed: %A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message))

    let private programOf (source: string) =
        match Parser.parse source with
        | ParseSuccess program -> program
        | ParseFailure(msg, line, col) ->
            failwithf "probe source failed to parse at %d:%d - %s" line col msg

    [<Fact>]
    let ``a parsed program round-trips through serialize and deserialize`` () =
        let src = "var a = 1\nvar b = 'two'\nvar c = true\nvar d = null\nOutput.v = a"
        let program = programOf src
        let bytes = serializeOrFail program [] (hashOf src)
        let result = BinaryFormat.deserialize bytes
        Assert.True(result.IsSuccess, sprintf "%A" result.Messages)
        Assert.Equal(program.Statements.Length, result.TryValue.Value.Program.Statements.Length)

    [<Fact>]
    let ``JYRO-REQ-1920 JYRO-REQ-1921 the header carries format version 1, and the reader refuses any other`` () =
        // jyro-spec 150.10.4 and 150.10.5 (JYRO-ADR-197): the format version is 1, written
        // 01 00, and a reader refuses every other version, the 9 of earlier builds included.
        // Pinned because nothing else enforces it and the number is a promise:
        // a reader refuses every version but its own, so a bump makes each
        // artifact already written unreadable. Bytes 4 and 5 are the version
        // field, little-endian, straight after the four magic bytes.
        let src = "Output.v = 1"
        let bytes = serializeOrFail (programOf src) [] (hashOf src)
        Assert.Equal<byte[]>([| 1uy; 0uy |], bytes.[4..5])

        for other in [ 0uy; 2uy; 7uy; 8uy; 9uy; 10uy ] do
            let bumped = Array.copy bytes
            bumped.[4] <- other
            let rejected = BinaryFormat.deserialize bumped
            Assert.False(rejected.IsSuccess)
            Assert.Contains(rejected.Messages, fun m -> m.Code = MessageCode.ArtifactVersionUnsupported)

    [<Fact>]
    let ``serializing an unencodable literal fails loudly rather than writing null`` () =
        // Not reachable from parsed source - the parser only ever emits null,
        // boolean, number and string literals - but `serialize` is public and
        // takes an AST, so a host can hand it one containing an array or object
        // value. That used to be written as a null tag, so the artifact
        // deserialized to a DIFFERENT program than the one compiled, with no
        // diagnostic anywhere. Corrupting output silently is worse than
        // refusing to write it.
        let arrayLiteral = JyroArray() :> JyroValue
        let program =
            { Statements =
                [ VarDecl("a", None, Some(Literal(arrayLiteral, Position.None)), Position.None) ] }
        // "Loudly" now means a diagnostic rather than an exception: `serialize`
        // reports through the same channel as `deserialize`, its inverse.
        //
        // It reports `ArtifactCannotEncode`, not `ArtifactUnreadable`. This
        // assertion used to name the read-side code, which is how the
        // mislabeling survived review: a write failure reported as unreadable,
        // pinned by a test that agreed with it.
        let result = BinaryFormat.serialize program [] (hashOf "var a = []")
        Assert.False(result.IsSuccess)
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.ArtifactCannotEncode)
        Assert.Contains("cannot encode", result.Messages.[0].Message)

    [<Fact>]
    let ``every literal kind the parser can emit survives the round trip`` () =
        let src = "var n = 1.5\nvar i = 42\nvar s = 'x'\nvar b = false\nvar z = null"
        let bytes = serializeOrFail (programOf src) [] (hashOf src)
        let result = BinaryFormat.deserialize bytes
        Assert.True(result.IsSuccess, sprintf "%A" result.Messages)
        Assert.Equal(5, result.TryValue.Value.Program.Statements.Length)

    // -- The effect ceiling is a sandbox policy, so it survives the format ---
    // `.jyrx` records no ceiling. Before these tests, `compileToJyrx` hardcoded
    // `Mutating` (so `WithMaxEffect(Pure)` was silently downgraded on the way
    // in) and `compileFromJyrx` skipped the linker entirely (so nothing was
    // checked on the way out). Between them the format was a hole straight
    // through the whole mechanism.

    /// A host function with a configurable effect, used to drive the ceiling.
    type private EffectfulFunction(name: string, effect: FunctionEffect) =
        interface IJyroFunction with
            member _.Name = name
            member _.Effect = effect
            member _.Signature =
                JyroFunctionSignature.Create(ParameterType.Number)
            member _.Execute(_, _) = JyroNumber(1.0) :> JyroValue

    let private blockingFn = EffectfulFunction("Block", FunctionEffect.Blocking) :> IJyroFunction

    [<Fact>]
    let ``compileToJyrx applies the caller's effect ceiling`` () =
        let src = "Output.v = Block()"
        let underMutating = Compiler.compileToJyrx src [ blockingFn ] FunctionEffect.Mutating
        Assert.True(underMutating.IsSuccess, sprintf "%A" underMutating.Messages)

        let underPure = Compiler.compileToJyrx src [ blockingFn ] FunctionEffect.Pure
        Assert.False(underPure.IsSuccess, "a Pure ceiling must refuse a Blocking call")
        Assert.Contains(underPure.Messages, fun m -> m.Code = MessageCode.FunctionEffectExceedsMax)

    [<Fact>]
    let ``loading an artifact re-applies the loading host's effect ceiling`` () =
        // The artifact is built by a permissive host and loaded by a strict
        // one. Nothing in the bytes records which ceiling built them, so the
        // check has to happen at load or it does not happen at all.
        let bytes =
            match Compiler.compileToJyrx "Output.v = Block()" [ blockingFn ] FunctionEffect.Mutating with
            | Produced b -> b
            | r -> failwithf "artifact failed to build: %A" r.Messages

        let strict = Compiler.compileFromJyrx bytes [ blockingFn ] FunctionEffect.Pure
        Assert.False(strict.IsSuccess, "a Pure ceiling must refuse the loaded artifact")
        Assert.Contains(strict.Messages, fun m -> m.Code = MessageCode.ArtifactEffectExceedsMax)

        let permissive = Compiler.compileFromJyrx bytes [ blockingFn ] FunctionEffect.Mutating
        Assert.True(permissive.IsSuccess, sprintf "%A" permissive.Messages)

    [<Fact>]
    let ``the timed load path applies the same ceiling as the untimed one`` () =
        // The two paths carried duplicate resolution bodies; the untimed one
        // is the one that gets read, so a check added there alone would have
        // left `WithStats` as an unguarded back door.
        let bytes =
            match Compiler.compileToJyrx "Output.v = Block()" [ blockingFn ] FunctionEffect.Mutating with
            | Produced b -> b
            | r -> failwithf "artifact failed to build: %A" r.Messages
        let timed =
            Compiler.compileFromJyrxTimed bytes [ blockingFn ] FunctionEffect.Pure (JyroPipelineStats())
        Assert.False(timed.IsSuccess)
        Assert.Contains(timed.Messages, fun m -> m.Code = MessageCode.ArtifactEffectExceedsMax)

    [<Fact>]
    let ``a malformed artifact reports the code it violates, not an internal error`` () =
        // The load path skips parsing and validation, so an artifact can carry
        // an AST no source could produce. That used to reach a `failwith` in
        // the compiler and surface as `UnknownCompilerError: Internal compiler
        // error ... Please report this` - blaming the engine for the input.
        let p = Position.None
        let program =
            { Statements =
                [ Assignment(
                    Literal(JyroNumber(1.0) :> JyroValue, p),
                    Assign,
                    Literal(JyroNumber(2.0) :> JyroValue, p),
                    None, p) ] }
        let bytes = serializeOrFail program [] (hashOf "n/a")
        let result = Compiler.compileFromJyrx bytes [] FunctionEffect.Mutating
        Assert.False(result.IsSuccess)
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.InvalidAssignmentTarget)
        Assert.DoesNotContain(result.Messages, fun m -> m.Code = MessageCode.UnknownCompilerError)

    // -- Output stays out of function and lambda bodies on this path too ----
    // Clause 40.11 forbids a function or lambda body from referencing
    // `Output`, and its rationale calls the alternative a back door: a
    // top-level lambda that captures `Output` and is then passed into a
    // function gives that function indirect access, defeating the
    // explicit-parameter rule of 50.7 and 50.8. The validator enforces it for
    // source, and the artifact path skips the validator.

    let private smuggle (statements: Stmt list) =
        let bytes = serializeOrFail { Statements = statements } [] (hashOf "n/a")
        Compiler.compileFromJyrx bytes [] FunctionEffect.Mutating

    let private writeToOutput =
        Assignment(PropertyAccess(Identifier("Output", Position.None), "leaked", Position.None),
                   Assign, Literal(JyroNumber(1.0) :> JyroValue, Position.None), None, Position.None)

    [<Fact>]
    let ``an artifact cannot reach Output from a function body`` () =
        // Measured before the fix: the emitted tree referenced an unbound
        // `__noOutput` sentinel and surfaced as UnknownCompilerError, which
        // told the host to file a bug about someone else's modified artifact.
        let r = smuggle [ FuncDef("Leak", [], [ writeToOutput ], Position.None) ]
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.OutputAccessInFunction)
        Assert.DoesNotContain(r.Messages, fun m -> m.Code = MessageCode.UnknownCompilerError)

    [<Fact>]
    let ``an artifact cannot reach Output from a top-level lambda`` () =
        // This one is the reason the compiler needs its own check rather than
        // a tidier sentinel. Measured before the fix: `OutputParam` really was
        // in scope at the top level, so the reference compiled and the program
        // EXECUTED, writing through the captured Output.
        let r =
            smuggle [ VarDecl("f", None,
                              Some(Lambda([], Identifier("Output", Position.None), Position.None)),
                              Position.None) ]
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.OutputAccessInFunction)

    [<Fact>]
    let ``an artifact cannot reach Output from a lambda inside a function`` () =
        let r =
            smuggle [ FuncDef("Leak", [],
                              [ VarDecl("g", None,
                                        Some(Lambda([], Identifier("Output", Position.None), Position.None)),
                                        Position.None) ], Position.None) ]
        Assert.False(r.IsSuccess)
        Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.OutputAccessInFunction)

    // -- Counts and lengths are narrowed unchecked ---------------------------
    //
    // Every count and length in the format is a `uint16`, and `uint16 n` in F#
    // wraps rather than raising. The readers cap what they will accept, but a
    // wrapped value arrives *under* the cap and passes it.
    //
    // Measured before the writers were guarded: a 70,000-byte string literal
    // wrote a length field of 4,464 (70000 mod 65536) followed by all 70,000
    // bytes, and `CompileToArtifact` reported success. Reading it back failed with
    // "Unknown JyroType tag: 97" - 97 is `a`, the reader having taken 4,464
    // bytes as the string and then parsed AST tags out of the middle of the
    // literal. The artifact was corrupt when written, blamed the format when
    // read, and with a luckier byte pattern would have deserialized to a
    // different valid program rather than failing at all.

    [<Fact>]
    let ``a string literal too long for the length field is refused at write time`` () =
        let big = String.replicate 70000 "a"
        let src = sprintf "Output.s = '%s'" big
        let program = programOf src
        let result = BinaryFormat.serialize program [] (hashOf src)
        Assert.False(result.IsSuccess, "a string the format cannot address was serialized anyway")
        // A refusal the writer decided on, so it carries the specific code
        // rather than the catch-all - the caller can tell "shorten this
        // literal" from "report a bug".
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.ArtifactCannotEncode)
        // The diagnostic names what could not be written and its length, since
        // the caller's next question is which literal to shorten. Its arguments
        // are exactly String and the length in bytes (150.11, JYRO-REQ-2117);
        // the bound is the same in every occurrence, so it is in the template,
        // not an argument (130.6.2).
        let refusal = result.Messages |> Seq.find (fun m -> m.Code = MessageCode.ArtifactCannotEncode)
        let args =
            refusal.Args
            |> Array.map (fun a -> Convert.ToString(a, Globalization.CultureInfo.InvariantCulture))
            |> List.ofArray
        Assert.Equal<string list>([ "String"; "70000" ], args)

    [<Fact>]
    let ``a string literal at the limit still serializes`` () =
        // The boundary in the other direction: 65535 bytes is addressable and
        // must keep working, so the guard is off-by-one-free.
        let atLimit = String.replicate 65535 "a"
        let src = sprintf "Output.s = '%s'" atLimit
        let program = programOf src
        let bytes = serializeOrFail program [] (hashOf src)
        let result = BinaryFormat.deserialize bytes
        Assert.True(result.IsSuccess, sprintf "%A" result.Messages)

    [<Fact>]
    let ``a function name too long for the length field is refused`` () =
        // The function table goes through the same writer, so it has the same
        // exposure and the same guard.
        let program = programOf "Output.v = 1"
        let result = BinaryFormat.serialize program [ String.replicate 70000 "f" ] (hashOf "Output.v = 1")
        Assert.False(result.IsSuccess, "an unaddressable function name was serialized anyway")
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.ArtifactCannotEncode)

    [<Fact>]
    let ``a list longer than the format's cap is refused at write time`` () =
        // Lists wrap the same way, and worse: MaxListCount is 10,000, so a
        // 70,000-element list wrote 4,464 and slipped *under* the reader's cap
        // rather than tripping it.
        let elements = List.replicate 70000 "1" |> String.concat ", "
        let src = sprintf "Output.a = [%s]" elements
        let program = programOf src
        let result = BinaryFormat.serialize program [] (hashOf src)
        Assert.False(result.IsSuccess, "a list the reader would reject was serialized anyway")
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.ArtifactCannotEncode)

    [<Fact>]
    let ``a write failure is reported as unwritable, never as unreadable`` () =
        // The two directions had one code between them. `serialize` caught
        // everything and reported `ArtifactUnreadable`, so a host routing on
        // the code was sent to the wrong half of the pipeline - and the test
        // above agreed with it, which is why it lasted.
        let big = String.replicate 70000 "a"
        let src = sprintf "Output.s = '%s'" big
        let result = BinaryFormat.serialize (programOf src) [] (hashOf src)
        Assert.False(result.IsSuccess)
        Assert.DoesNotContain(result.Messages, fun m -> m.Code = MessageCode.ArtifactUnreadable)

    [<Fact>]
    let ``a read failure is still reported as unreadable`` () =
        // The other half of the same guarantee: splitting the codes must not
        // have moved the read side.
        let corrupt = Array.append (Encoding.ASCII.GetBytes "JYRX") (Array.zeroCreate 64)
        let result = BinaryFormat.deserialize corrupt
        Assert.False(result.IsSuccess)
        Assert.DoesNotContain(result.Messages, fun m ->
            m.Code = MessageCode.ArtifactCannotEncode || m.Code = MessageCode.ArtifactUnwritable)

    // -- The load path validates ------------------------------------------

    // A `.jyrx` artifact is a file. A host loads one from wherever it keeps
    // them, and the package's own security note says a host that cannot trust
    // the source of an artifact should compile from source instead - but the
    // reason it gives is provenance, not that the loader waives its checks.
    //
    // It waived them. `compileFromJyrx` went deserialize -> relink -> compile,
    // and `relinkArtifact` re-resolved the required-function names and
    // re-applied the effect ceiling and nothing else. `Validator.validate` was
    // called on all three source paths and neither artifact path, so every
    // rule the validator holds - Input is read-only, a name is declared once, a
    // match over a union covers it - held for a script and not for the same
    // script shipped as bytes.
    //
    // The compiler behind it assumes a validated AST, which is what made this
    // more than a missing check: an artifact carrying a duplicate declaration
    // reached an `InvalidOperationException` and was reported as [JM6000]
    // "Internal compiler error ... Please report this at <issues URL>" - a
    // file from outside the process inviting the host to file a bug about it.
    //
    // Validation now runs inside `relinkArtifact` rather than in its two
    // callers, so the timed load path (`WithStats`) cannot be the door that
    // was left open. That is the same shape as the effect-ceiling repair
    // recorded in AUDIT.md, which is why it is placed the same way.

    let private tamperedArtifact (source: string) (mutate: Program -> Program) =
        serializeOrFail (mutate (programOf source)) [] (hashOf source)

    let private loadArtifact (bytes: byte[]) =
        Compiler.compileFromJyrx bytes Seq.empty FunctionEffect.Mutating

    [<Fact>]
    let ``an artifact declaring a name twice is refused, not reported as a compiler bug`` () =
        // `var x` twice in one scope. The parser accepts it and the validator
        // is what refuses it, so duplicating the statement in the AST produces
        // exactly what a tampered artifact carries.
        let bytes =
            tamperedArtifact "var x = 1\nOutput.v = x" (fun p ->
                { Statements = p.Statements.Head :: p.Statements })
        let result = loadArtifact bytes
        Assert.False(result.IsSuccess, "a program the validator refuses loaded from an artifact")
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.VariableAlreadyDeclared)
        Assert.DoesNotContain(result.Messages, fun m -> m.Code = MessageCode.UnknownCompilerError)

    [<Fact>]
    let ``an artifact writing to Input is refused`` () =
        // Input being read-only is a sandbox rule, and it is the validator that
        // holds it - `execute` freezes the input value as the backstop, so this
        // one failed closed. The point is that it was the backstop holding it.
        let bytes = tamperedArtifact "Input.a = 1" id
        let result = loadArtifact bytes
        Assert.False(result.IsSuccess, "an artifact that writes to Input loaded")
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.InputIsReadOnly)

    [<Fact>]
    let ``an artifact whose match does not cover its union is refused`` () =
        // Exhaustiveness is a validator rule with nothing behind it: the
        // compiled match falls through to null and the run reports success, so
        // a script that silently produced `{}` was indistinguishable from one
        // that meant to.
        let source =
            String.concat "\n"
                [ "union Shape"
                  "  Circle(r)"
                  "  Square(s)"
                  "end"
                  "var c = Circle(1)"
                  "Output.v = match c do"
                  "  case Circle(r) then r"
                  "  case Square(s) then s"
                  "end" ]
        let dropLastCase (p: Program) =
            { Statements =
                p.Statements
                |> List.map (fun stmt ->
                    match stmt with
                    | Assignment(target, op, MatchExpr(subject, cases, mp), hint, pos) ->
                        Assignment(target, op, MatchExpr(subject, [ List.head cases ], mp), hint, pos)
                    | other -> other) }
        let result = loadArtifact (tamperedArtifact source dropLastCase)
        Assert.False(result.IsSuccess, "a non-exhaustive match loaded from an artifact")
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.MatchNotExhaustive)

    [<Fact>]
    let ``an untampered artifact still loads`` () =
        // The control. Validation is being added to a path that never had it,
        // so the thing most worth pinning is that every artifact the writer
        // produces still passes it - a validator rule that a round-trip
        // happens to break would turn this repair into a load-path outage.
        let source =
            String.concat "\n"
                [ "union Shape"
                  "  Circle(r)"
                  "  Square(s)"
                  "end"
                  "func area(shape)"
                  "  return match shape do"
                  "    case Circle(r) then 3 * r * r"
                  "    case Square(s) then s * s"
                  "  end"
                  "end"
                  "var shapes = [Circle(2), Square(3)]"
                  "var total = 0"
                  "foreach s in shapes do"
                  "  total = total + area(s)"
                  "end"
                  "Output.total = total" ]
        let result = loadArtifact (tamperedArtifact source id)
        Assert.True(result.IsSuccess, sprintf "a valid artifact was refused: %A" (List.ofSeq result.Messages |> List.map (fun m -> m.Message)))

    // -- The writer and the two readers agree on the format -----------------

    // Every count in the format is capped, and the writers were taught to
    // enforce those caps rather than leave it to the readers - so that an
    // artifact is refused where it is written, not blamed on the format where
    // it is read. One count was missed: the function table. It is the format's
    // only `uint32`, so it is written directly instead of through `writeCount`,
    // and going around that helper is exactly how it escaped. A host with more
    // than a thousand called functions got bytes its own reader refuses.
    //
    // The other half of the same disagreement is that there are two readers.
    // `readHeader` - public as `JyroArtifactInfo.Read`, and what jyro-cli's
    // `validate` command is built on - checked the magic bytes and then read
    // the version without comparing it, the file without measuring it, and the
    // function table with no cap. So it reported success for artifacts the load
    // path refuses, which is backwards for a pre-flight check.

    [<Fact>]
    let ``a function table past the format's cap is refused at write time`` () =
        let names = [ for i in 1 .. 1001 -> sprintf "f%d" i ]
        let result = BinaryFormat.serialize (programOf "Output.v = 1") names (hashOf "Output.v = 1")
        Assert.False(result.IsSuccess, "an artifact the reader would refuse was written anyway")
        Assert.Contains(result.Messages, fun m -> m.Code = MessageCode.ArtifactCannotEncode)

    [<Fact>]
    let ``a function table at the cap still writes and loads`` () =
        // The boundary, so the guard is off-by-one-free in the direction that
        // would break working hosts.
        let names = [ for i in 1 .. 1000 -> sprintf "f%d" i ]
        let src = "Output.v = 1"
        let bytes = serializeOrFail (programOf src) names (hashOf src)
        let header = BinaryFormat.readHeader bytes
        Assert.True(header.IsSuccess, sprintf "%A" header.Messages)

    let private patched (bytes: byte[]) (offset: int) (replacement: byte[]) =
        let copy = Array.copy bytes
        Array.blit replacement 0 copy offset replacement.Length
        copy

    [<Fact>]
    let ``the header reader refuses a version the load path refuses`` () =
        let src = "Output.v = 1"
        let bytes = serializeOrFail (programOf src) [] (hashOf src)
        // Bytes 4 and 5 are the version field, little-endian.
        let tampered = patched bytes 4 [| 7uy; 0uy |]
        let header = BinaryFormat.readHeader tampered
        Assert.False(header.IsSuccess, "the header reader accepted a version the loader refuses")
        Assert.Contains(header.Messages, fun m -> m.Code = MessageCode.ArtifactVersionUnsupported)
        // The two readers must give the same verdict, which is the whole point.
        let loaded = BinaryFormat.deserialize tampered
        Assert.False(loaded.IsSuccess)
        Assert.Contains(loaded.Messages, fun m -> m.Code = MessageCode.ArtifactVersionUnsupported)

    [<Fact>]
    let ``the header reader refuses a function count the load path refuses`` () =
        let src = "Output.v = 1"
        let bytes = serializeOrFail (programOf src) [] (hashOf src)
        // Magic (4) + version (2) + flags (2) + hash (32) = 40, then the count.
        let tampered = patched bytes 40 (BitConverter.GetBytes(1001u))
        // More than 1,000 table entries is refused with ArtifactUnreadable and no
        // argument, by the header reader (150.2.8 check 4, JYRO-REQ-2124) and by
        // the load (150.2.9 row 5, JYRO-REQ-2066). The book gives the cap and a
        // truncated table the same code and no argument (130.6.2, JYRO-ADR-125),
        // so the two readers agree on the code, which is the whole point.
        let onlyUnreadable (r: JyroResult<'a>) =
            let ms = List.ofSeq r.Messages
            Assert.Single(ms) |> ignore
            Assert.Equal(MessageCode.ArtifactUnreadable, ms.Head.Code)
            Assert.True(isNull ms.Head.Args || ms.Head.Args.Length = 0,
                        sprintf "ArtifactUnreadable carries arguments %A" ms.Head.Args)
        let header = BinaryFormat.readHeader tampered
        Assert.False(header.IsSuccess, "the header reader accepted a function count the loader refuses")
        onlyUnreadable header
        let loaded = BinaryFormat.deserialize tampered
        Assert.False(loaded.IsSuccess)
        onlyUnreadable loaded

    [<Fact>]
    let ``the header reader refuses a file the load path refuses for size`` () =
        let src = "Output.v = 1"
        let bytes = serializeOrFail (programOf src) [] (hashOf src)
        let oversized = Array.append bytes (Array.zeroCreate (10 * 1024 * 1024 + 1))
        let header = BinaryFormat.readHeader oversized
        Assert.False(header.IsSuccess, "the header reader accepted a file the loader refuses")
        Assert.Contains(header.Messages, fun m -> m.Code = MessageCode.ArtifactTooLarge)
        let loaded = BinaryFormat.deserialize oversized
        Assert.False(loaded.IsSuccess)
        Assert.Contains(loaded.Messages, fun m -> m.Code = MessageCode.ArtifactTooLarge)

    [<Fact>]
    let ``JYRO-REQ-1920 an untampered header still reads, at version 1`` () =
        let src = "Output.v = Trim(' x ')"
        let bytes = serializeOrFail (programOf src) [ "Trim" ] (hashOf src)
        match BinaryFormat.readHeader bytes with
        | Produced header ->
            // The format version of this specification, 1 (150.10.4, JYRO-REQ-1920).
            Assert.Equal(1, header.Version)
            Assert.Equal<string list>([ "Trim" ], header.RequiredFunctions)
            Assert.Equal<byte[]>(hashOf src, header.SourceHash)
        | r -> Assert.Fail(sprintf "a valid header was refused: %A" (List.ofSeq r.Messages |> List.map (fun m -> m.Message)))

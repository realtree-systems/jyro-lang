namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast

open System
open System.Collections.Generic
open System.IO
open System.Text

/// Header metadata from a .jyrx binary file (no AST deserialization)
type internal JyrxHeader =
    { Version: int
      SourceHash: byte[]
      RequiredFunctions: string list }

/// Result of deserializing a .jyrx binary file
type internal DeserializedProgram =
    { Program: Program
      RequiredFunctions: string list }

/// Binary serialization format for compiled Jyro programs (.jyrx files).
/// Encodes the validated AST and function dependency table into a compact binary representation.
module internal BinaryFormat =
    let [<Literal>] private MagicBytes = "JYRX"
    // The format version of the specification is 1 (spec 150.10.4,
    // JYRO-ADR-197): a writer writes `01 00` at bytes 4 and 5, and both
    // readers - a load and JyroArtifactInfo.Read - refuse every other
    // version, 0 and 2 to 65,535, with ArtifactVersionUnsupported (150.10.5).
    // The numbering restarted at 1 for the first release: the encoding is the
    // one development builds wrote as version 9, and an artifact they wrote is
    // refused and rebuilt from source. The field guards recognition rather
    // than compatibility, so it bumps only when the encoding changes in a way
    // an existing reader would decode wrongly (150.10.1).
    let [<Literal>] private FormatVersion = 1us

    // Safety limits shared by both directions. They were declared beside the
    // readers, when only the readers enforced them; the writers enforce them
    // too now, so they sit ahead of both.
    let [<Literal>] private MaxFileSize = 10 * 1024 * 1024  // 10 MB
    /// The longest string the format can address, being the maximum of the
    /// `uint16` length field itself. The reader's check against it therefore
    /// cannot fire - the field is incapable of holding a larger number - so the
    /// writer's is the one that does the work.
    let [<Literal>] private MaxStringLength = 65535
    let [<Literal>] private MaxListCount = 10000
    /// The function table's cap. It was declared with the reader helpers, two
    /// hundred lines below the others, and so was the one limit the writer did
    /// not learn when the writers took the rest on: a host with more than a
    /// thousand called functions got an artifact its own reader refuses. It
    /// escaped `writeCount` because the function count is the format's only
    /// `uint32` and that helper writes a `uint16`.
    let [<Literal>] private MaxFunctionCount = 1000
    /// The deepest nesting of expression and statement nodes the format holds
    /// (spec 150.9.5), counted as 150.9.9 gives: a top-level statement has
    /// depth 1, each node one more than the node that holds it, and a
    /// structure that is not a node - a case, a pair, a parameter, a variant -
    /// adds nothing. The reader and the writer count it alike, so a writer
    /// refuses exactly the programs the reader would (spec 150.9.7). It is the
    /// one maximum nesting depth of a source text and an artifact, 256 (spec
    /// 100.10.1, JYRO-ADR-177), so a program that compiles from source is
    /// never too deep to write.
    let [<Literal>] private MaxRecursionDepth = RealTree.Jyro.Ast.Ast.MaxNestingDepth

    // --- Writer helpers ---

    /// BinaryWriter that carries the nesting depth of a single serialize call,
    /// the writer's counterpart of the reader's `DepthTrackingReader`: held on
    /// the instance, so concurrent writes cannot corrupt each other's depth.
    type private DepthTrackingWriter(stream: Stream) =
        inherit BinaryWriter(stream, Encoding.UTF8, true)
        member val Depth = 0 with get, set

    /// Enters a node. The first node deeper than the bound is refused with
    /// `NestingDepth` and no second argument (spec 150.9.7, 150.11.4).
    let private enterWriteDepth (w: BinaryWriter) =
        let dw = w :?> DepthTrackingWriter
        dw.Depth <- dw.Depth + 1
        if dw.Depth > MaxRecursionDepth then
            JyroError.raiseRuntime MessageCode.ArtifactCannotEncode [| box "NestingDepth" |]

    let private exitWriteDepth (w: BinaryWriter) =
        let dw = w :?> DepthTrackingWriter
        dw.Depth <- dw.Depth - 1

    // Every count and length in this format is a `uint16`, and `uint16 n` in F#
    // is an *unchecked* narrowing: it wraps rather than raising. The readers
    // enforce caps, but a wrapped value arrives under them and passes.
    //
    // Measured before this guard existed: a script with a 70,000-byte string
    // literal wrote a length field of 4,464 followed by all 70,000 bytes, and
    // the writer reported success. Loading that artifact back failed with
    // "Unknown JyroType tag: 97" - decimal 97 is `a`, the reader having taken
    // 4,464 bytes as the string and then read AST tags out of the middle of the
    // literal. The artifact was corrupt at the moment it was written, blamed
    // the format when it was read, and in a less lucky case would have
    // deserialized to a *different valid program* rather than failing at all.
    //
    // This is the same reasoning as `writeLiteralValue`'s fallback below: a
    // caller that cannot serialize something needs to know at the point of
    // writing, not to discover it as wrong output later.
    //
    // `what` is the name of the part that holds the count, a name the
    // specification gives (spec 130.6.2, 150.11): `String`, `List`,
    // `FunctionTable`, or the node that holds it, `ObjectLiteral`, `If`,
    // `FuncDef`, `Call` or `UnionVariant`. `n` is the count, a number alone;
    // the bound is the same in every occurrence, so it is not an argument.
    let private checkCount (what: string) (n: int) (limit: int) =
        if n > limit then
            JyroError.raiseRuntime MessageCode.ArtifactCannotEncode
                [| box what; box n |]

    let private writeCount (w: BinaryWriter) (what: string) (n: int) (limit: int) =
        checkCount what n limit
        w.Write(uint16 n)

    let private writeString (w: BinaryWriter) (s: string) =
        let bytes = Encoding.UTF8.GetBytes(s)
        writeCount w "String" bytes.Length MaxStringLength
        w.Write(bytes)

    let private writePosition (w: BinaryWriter) (pos: Position) =
        w.Write(pos.Line)
        w.Write(pos.Column)
        w.Write(pos.StartIndex)
        w.Write(pos.EndIndex)

    let private writeOption (w: BinaryWriter) (writeValue: BinaryWriter -> 'a -> unit) (opt: 'a option) =
        match opt with
        | None -> w.Write(0uy)
        | Some v ->
            w.Write(1uy)
            writeValue w v

    let private writeList (w: BinaryWriter) (writeItem: BinaryWriter -> 'a -> unit) (items: 'a list) =
        writeCount w "List" items.Length MaxListCount
        items |> List.iter (writeItem w)

    let private writeJyroType (w: BinaryWriter) (t: JyroType) =
        let tag =
            match t with
            | AnyType -> 0uy
            | NumberType -> 1uy
            | StringType -> 2uy
            | BooleanType -> 3uy
            | ObjectType -> 4uy
            | ArrayType -> 5uy
            | NullType -> 6uy
            | FunctionType -> 7uy
        w.Write(tag)

    let private writeBinaryOp (w: BinaryWriter) (op: BinaryOp) =
        let tag =
            match op with
            | Add -> 0uy
            | Subtract -> 1uy
            | Multiply -> 2uy
            | Divide -> 3uy
            | Modulo -> 4uy
            | Equal -> 5uy
            | NotEqual -> 6uy
            | LessThan -> 7uy
            | LessThanOrEqual -> 8uy
            | GreaterThan -> 9uy
            | GreaterThanOrEqual -> 10uy
            | And -> 11uy
            | Or -> 12uy
            | Coalesce -> 13uy
            | Concat -> 14uy
        w.Write(tag)

    let private writeUnaryOp (w: BinaryWriter) (op: UnaryOp) =
        let tag =
            match op with
            | Negate -> 0uy
            | Not -> 1uy
        w.Write(tag)

    let private writeAssignOp (w: BinaryWriter) (op: AssignOp) =
        let tag =
            match op with
            | Assign -> 0uy
            | AddAssign -> 1uy
            | SubtractAssign -> 2uy
            | MultiplyAssign -> 3uy
            | DivideAssign -> 4uy
            | ModuloAssign -> 5uy
            | ConcatAssign -> 6uy
        w.Write(tag)

    /// An exact number in its one encoding (spec 150.6.3, 150.6.7): the
    /// significand in 12 bytes, two reserved zero bytes, the scale, and the
    /// sign, with the smallest scale that holds the value, so the significand
    /// is a multiple of 10 only at scale 0, and zero as significand 0, scale 0
    /// and sign `0x00`. A .NET `decimal` keeps the scale it was made with, so
    /// `1.50` holds significand 150 at scale 2; written as it is, two
    /// spellings of one value gave two artifacts.
    let private writeExact (w: BinaryWriter) (d: decimal) =
        let bits = Decimal.GetBits d
        let mutable significand =
            UInt128(uint64 (uint32 bits.[2]), (uint64 (uint32 bits.[1]) <<< 32) ||| uint64 (uint32 bits.[0]))
        let mutable scale = (bits.[3] >>> 16) &&& 0xFF
        let ten = UInt128(0UL, 10UL)
        while scale > 0 && significand % ten = UInt128.Zero do
            significand <- significand / ten
            scale <- scale - 1
        let isZero = significand = UInt128.Zero
        if isZero then scale <- 0
        let negative = not isZero && bits.[3] < 0
        let low = uint64 (significand &&& UInt128(0UL, UInt64.MaxValue))
        let high = uint64 (significand >>> 64)
        w.Write(uint32 low)
        w.Write(uint32 (low >>> 32))
        w.Write(uint32 high)
        w.Write(0uy)
        w.Write(0uy)
        w.Write(byte scale)
        w.Write(if negative then 0x80uy else 0x00uy)

    let private writeLiteralValue (w: BinaryWriter) (value: JyroValue) =
        match value with
        | :? JyroNull ->
            w.Write(1uy)
        | :? JyroBoolean as b ->
            w.Write(2uy)
            w.Write(b.Value)
        | :? JyroNumber as n ->
            w.Write(3uy)
            // Preserve the exact representation: kind byte 1 = exact decimal
            // (16 bytes), 0 = double (8 bytes).
            if n.IsExact then
                w.Write(1uy)
                writeExact w n.DecimalValue
            else
                w.Write(0uy)
                w.Write(n.Value)
        | :? JyroString as s ->
            w.Write(4uy)
            writeString w s.Value
        | other ->
            // Not reachable from a parsed program: the parser emits `Literal`
            // only for null, boolean, number and string, and `readLiteralValue`
            // reconstructs only those four. It IS reachable from a hand-built
            // AST passed to the public `serialize`, and this used to write a
            // null tag - so an array or object literal was silently replaced by
            // null and the artifact deserialized to a different program than it
            // was compiled from. Fail instead: a caller that cannot serialize a
            // value needs to know at the point of writing, not to discover it
            // as wrong output later.
            JyroError.raiseRuntime MessageCode.ArtifactCannotEncode
                [| box "Literal"; box (TypeNames.typeOfName other.ValueType) |]

    let rec private writeExpr (w: BinaryWriter) (expr: Expr) =
        // A refusal abandons the write, so the depth needs no restoring on
        // the way out of one.
        enterWriteDepth w
        writeExprNode w expr
        exitWriteDepth w

    and private writeExprNode (w: BinaryWriter) (expr: Expr) =
        match expr with
        | Literal(value, pos) ->
            w.Write(0x01uy)
            writeLiteralValue w value
            writePosition w pos
        | Identifier(name, pos) ->
            w.Write(0x02uy)
            writeString w name
            writePosition w pos
        | Binary(left, op, right, pos) ->
            w.Write(0x03uy)
            writeExpr w left
            writeBinaryOp w op
            writeExpr w right
            writePosition w pos
        | Unary(op, operand, pos) ->
            w.Write(0x04uy)
            writeUnaryOp w op
            writeExpr w operand
            writePosition w pos
        | Ternary(cond, thenExpr, elseExpr, pos) ->
            w.Write(0x05uy)
            writeExpr w cond
            writeExpr w thenExpr
            writeExpr w elseExpr
            writePosition w pos
        | Call(name, args, pos) ->
            w.Write(0x06uy)
            writeString w name
            match args with
            | PositionalArgs exprs ->
                w.Write(0uy)
                writeList w writeExpr exprs
            | NamedArgs pairs ->
                w.Write(1uy)
                writeCount w "Call" pairs.Length MaxListCount
                for (argName, expr) in pairs do
                    writeString w argName
                    writeExpr w expr
            writePosition w pos
        | PropertyAccess(target, prop, pos) ->
            w.Write(0x07uy)
            writeExpr w target
            writeString w prop
            writePosition w pos
        | IndexAccess(target, index, pos) ->
            w.Write(0x08uy)
            writeExpr w target
            writeExpr w index
            writePosition w pos
        | ObjectLiteral(props, pos) ->
            w.Write(0x09uy)
            writeCount w "ObjectLiteral" props.Length MaxListCount
            props |> List.iter (fun (key, value) ->
                writeString w key
                writeExpr w value)
            writePosition w pos
        | ArrayLiteral(elements, pos) ->
            w.Write(0x0Auy)
            writeList w writeExpr elements
            writePosition w pos
        | Lambda(parameters, body, pos) ->
            w.Write(0x0Buy)
            writeList w writeString parameters
            writeExpr w body
            writePosition w pos
        | TypeCheck(expr, typeName, isNegated, pos) ->
            w.Write(0x0Cuy)
            writeExpr w expr
            writeJyroType w typeName
            w.Write(if isNegated then 1uy else 0uy)
            writePosition w pos
        | IncrementDecrement(expr, isIncrement, isPrefix, pos) ->
            w.Write(0x0Duy)
            writeExpr w expr
            w.Write(if isIncrement then 1uy else 0uy)
            w.Write(if isPrefix then 1uy else 0uy)
            writePosition w pos
        | MatchExpr(expr, cases, pos) ->
            w.Write(0x0Euy)
            writeExpr w expr
            writeList w writeMatchExprCase cases
            writePosition w pos

    and private writeMatchExprCase (w: BinaryWriter) (case: MatchExprCase) =
        writeString w case.VariantName
        writeList w writeString case.Bindings
        writeExpr w case.Body
        writePosition w case.Pos

    and private writeSwitchCase (w: BinaryWriter) (case: SwitchCase) =
        writeList w writeExpr case.Values
        writeList w writeStmt case.Body

    and private writeUnionVariant (w: BinaryWriter) (variant: UnionVariant) =
        writeString w variant.Name
        writeCount w "UnionVariant" variant.Fields.Length MaxListCount
        variant.Fields |> List.iter (fun (name, typeHint) ->
            writeString w name
            writeOption w writeJyroType typeHint)
        // The variant's own range, after its fields (spec 150.8.5,
        // JYRO-ADR-138): where DuplicateVariantName is placed on a load.
        writePosition w variant.Pos

    and private writeMatchCase (w: BinaryWriter) (case: MatchCase) =
        writeString w case.VariantName
        writeList w writeString case.Bindings
        writeList w writeStmt case.Body
        writePosition w case.Pos

    and private writeStmt (w: BinaryWriter) (stmt: Stmt) =
        // Each statement, nested ones included, is where an abandoned
        // compilation stops (JYRO-ADR-100).
        CompilationStage.CheckAmbient()
        enterWriteDepth w
        writeStmtNode w stmt
        exitWriteDepth w

    and private writeStmtNode (w: BinaryWriter) (stmt: Stmt) =
        match stmt with
        | VarDecl(name, typeHint, init, pos) ->
            w.Write(0x20uy)
            writeString w name
            writeOption w writeJyroType typeHint
            writeOption w writeExpr init
            writePosition w pos
        | Assignment(target, op, value, typeHint, pos) ->
            w.Write(0x21uy)
            writeExpr w target
            writeAssignOp w op
            writeExpr w value
            writeOption w writeJyroType typeHint
            writePosition w pos
        | If(cond, thenBlock, elseIfs, elseBlock, pos) ->
            w.Write(0x22uy)
            writeExpr w cond
            writeList w writeStmt thenBlock
            writeCount w "If" elseIfs.Length MaxListCount
            elseIfs |> List.iter (fun (e, stmts) ->
                writeExpr w e
                writeList w writeStmt stmts)
            writeOption w (fun wr stmts -> writeList wr writeStmt stmts) elseBlock
            writePosition w pos
        | While(cond, body, pos) ->
            w.Write(0x23uy)
            writeExpr w cond
            writeList w writeStmt body
            writePosition w pos
        | ForEach(varName, collection, body, pos) ->
            w.Write(0x24uy)
            writeString w varName
            writeExpr w collection
            writeList w writeStmt body
            writePosition w pos
        | For(varName, startExpr, endExpr, stepExpr, direction, body, pos) ->
            w.Write(0x25uy)
            writeString w varName
            writeExpr w startExpr
            writeExpr w endExpr
            writeOption w writeExpr stepExpr
            w.Write(match direction with Ascending -> 0uy | Descending -> 1uy)
            writeList w writeStmt body
            writePosition w pos
        | Switch(expr, cases, defaultCase, pos) ->
            w.Write(0x26uy)
            writeExpr w expr
            writeList w writeSwitchCase cases
            writeOption w (fun wr stmts -> writeList wr writeStmt stmts) defaultCase
            writePosition w pos
        | Return(value, pos) ->
            w.Write(0x27uy)
            writeOption w writeExpr value
            writePosition w pos
        | Fail(message, pos) ->
            w.Write(0x28uy)
            writeOption w writeExpr message
            writePosition w pos
        | Break pos ->
            w.Write(0x29uy)
            writePosition w pos
        | Continue pos ->
            w.Write(0x2Auy)
            writePosition w pos
        | ExprStmt(expr, pos) ->
            w.Write(0x2Buy)
            writeExpr w expr
            writePosition w pos
        | FuncDef(name, parameters, body, pos) ->
            w.Write(0x2Cuy)
            writeString w name
            writeCount w "FuncDef" parameters.Length MaxListCount
            parameters |> List.iter (fun (p: FuncParam) ->
                writeString w p.Name
                writeOption w writeJyroType p.TypeHint
                writeOption w writeExpr p.DefaultValue)
            writeList w writeStmt body
            writePosition w pos
        | Exit(value, pos) ->
            w.Write(0x2Duy)
            writeOption w writeExpr value
            writePosition w pos
        | UnionDef(name, variants, pos) ->
            w.Write(0x2Euy)
            writeString w name
            writeList w writeUnionVariant variants
            writePosition w pos
        | Match(expr, cases, pos) ->
            w.Write(0x2Fuy)
            writeExpr w expr
            writeList w writeMatchCase cases
            writePosition w pos
        | Delete(target, pos) ->
            w.Write(0x30uy)
            writeExpr w target
            writePosition w pos

    // --- Reader helpers ---

    /// Strict UTF-8 (spec 150.4.6): an invalid byte, a truncated sequence, an
    /// overlong form or an encoded surrogate throws, which the readers report
    /// as `ArtifactUnreadable`. `Encoding.UTF8` put U+FFFD in their place, so
    /// a changed name or value read back as another one with no report.
    let private strictUtf8 = UTF8Encoding(false, true)

    let private readString (r: BinaryReader) =
        let len = int (r.ReadUInt16())
        if len > MaxStringLength then
            failwithf "String length %d exceeds maximum of %d" len MaxStringLength
        let bytes = r.ReadBytes(len)
        // `ReadBytes` returns fewer bytes than asked for at the end of the
        // input, with no error: a string cut short is not decodable (150.11.3).
        if bytes.Length <> len then
            failwithf "String of %d bytes ends after %d" len bytes.Length
        strictUtf8.GetString(bytes)

    /// A function table that holds a name two times is unreadable (spec
    /// 150.3.5): a writer never repeats a name, so a repeat means the file
    /// was changed. Names are compared exactly, as identifiers are.
    let private checkTableHasNoRepeatedName (names: string list) =
        let seen = HashSet<string>(StringComparer.Ordinal)
        for name in names do
            if not (seen.Add name) then
                failwithf "Function table holds the name %s two times" name

    let private readPosition (r: BinaryReader) =
        { Line = r.ReadInt32()
          Column = r.ReadInt32()
          StartIndex = r.ReadInt32()
          EndIndex = r.ReadInt32() }

    let private readOption (r: BinaryReader) (readValue: BinaryReader -> 'a) : 'a option =
        match r.ReadByte() with
        | 0uy -> None
        | _ -> Some (readValue r)

    let private readList (r: BinaryReader) (readItem: BinaryReader -> 'a) : 'a list =
        let count = int (r.ReadUInt16())
        if count > MaxListCount then
            failwithf "List count %d exceeds maximum of %d" count MaxListCount
        [ for _ in 1 .. count -> readItem r ]

    let private readJyroType (r: BinaryReader) =
        match r.ReadByte() with
        | 0uy -> AnyType
        | 1uy -> NumberType
        | 2uy -> StringType
        | 3uy -> BooleanType
        | 4uy -> ObjectType
        | 5uy -> ArrayType
        | 6uy -> NullType
        | 7uy -> FunctionType
        | tag -> failwithf "Unknown JyroType tag: %d" tag

    let private readBinaryOp (r: BinaryReader) =
        match r.ReadByte() with
        | 0uy -> Add
        | 1uy -> Subtract
        | 2uy -> Multiply
        | 3uy -> Divide
        | 4uy -> Modulo
        | 5uy -> Equal
        | 6uy -> NotEqual
        | 7uy -> LessThan
        | 8uy -> LessThanOrEqual
        | 9uy -> GreaterThan
        | 10uy -> GreaterThanOrEqual
        | 11uy -> And
        | 12uy -> Or
        | 13uy -> Coalesce
        | 14uy -> Concat
        | tag -> failwithf "Unknown BinaryOp tag: %d" tag

    let private readUnaryOp (r: BinaryReader) =
        match r.ReadByte() with
        | 0uy -> Negate
        | 1uy -> Not
        | tag -> failwithf "Unknown UnaryOp tag: %d" tag

    let private readAssignOp (r: BinaryReader) =
        match r.ReadByte() with
        | 0uy -> Assign
        | 1uy -> AddAssign
        | 2uy -> SubtractAssign
        | 3uy -> MultiplyAssign
        | 4uy -> DivideAssign
        | 5uy -> ModuloAssign
        | 6uy -> ConcatAssign
        | tag -> failwithf "Unknown AssignOp tag: %d" tag

    let private readLiteralValue (r: BinaryReader) : JyroValue =
        match r.ReadByte() with
        | 1uy -> JyroNull.Instance :> JyroValue
        | 2uy -> JyroBoolean.FromBoolean(r.ReadBoolean()) :> JyroValue
        | 3uy ->
            match r.ReadByte() with
            | 0uy ->
                // Checked on the way in, not merely on the way out. An
                // artifact built before a Jyro number was required to be
                // finite carries its infinity in eight bytes and would
                // reconstitute it on every future run, so the source it came
                // from no longer being compilable is not enough - this is the
                // one door where a value outlives the rule that admitted it.
                // Eight arbitrary bytes are also whatever a caller supplies,
                // so this doubles as validation of a hostile artifact.
                let d = r.ReadDouble()
                // Refused as an artifact that cannot be decoded, the code
                // spec 150.6.6 gives it, with no argument: no function
                // computed the number, so `NumberOverflow`'s first argument
                // would have nothing to name.
                if Double.IsNaN d || Double.IsInfinity d then
                    failwith "Inexact number literal is not finite"
                JyroNumber(d) :> JyroValue
            | 1uy -> JyroNumber(r.ReadDecimal()) :> JyroValue
            | k -> failwithf "Unknown number kind byte: %d" k
        | 4uy -> JyroString(readString r) :> JyroValue
        | tag -> failwithf "Unknown literal value tag: %d" tag

    /// BinaryReader that carries the AST-nesting depth for a single deserialize
    /// call. Holding the counter on the reader instance rather than in module
    /// state keeps the recursion guard isolated per call, so concurrent
    /// deserializations on different threads cannot corrupt each other's depth.
    type private DepthTrackingReader(stream: Stream) =
        inherit BinaryReader(stream, Encoding.UTF8, true)
        member val Depth = 0 with get, set

    let private enterDepth (r: BinaryReader) =
        let dr = r :?> DepthTrackingReader
        dr.Depth <- dr.Depth + 1
        if dr.Depth > MaxRecursionDepth then
            failwithf "AST nesting depth exceeds maximum of %d" MaxRecursionDepth

    let private exitDepth (r: BinaryReader) =
        let dr = r :?> DepthTrackingReader
        dr.Depth <- dr.Depth - 1

    let rec private readExpr (r: BinaryReader) : Expr =
        enterDepth r
        try
            match r.ReadByte() with
            | 0x01uy ->
                let value = readLiteralValue r
                let pos = readPosition r
                Literal(value, pos)
            | 0x02uy ->
                let name = readString r
                let pos = readPosition r
                Identifier(name, pos)
            | 0x03uy ->
                let left = readExpr r
                let op = readBinaryOp r
                let right = readExpr r
                let pos = readPosition r
                Binary(left, op, right, pos)
            | 0x04uy ->
                let op = readUnaryOp r
                let operand = readExpr r
                let pos = readPosition r
                Unary(op, operand, pos)
            | 0x05uy ->
                let cond = readExpr r
                let thenExpr = readExpr r
                let elseExpr = readExpr r
                let pos = readPosition r
                Ternary(cond, thenExpr, elseExpr, pos)
            | 0x06uy ->
                let name = readString r
                let args =
                    match r.ReadByte() with
                    | 0uy -> PositionalArgs(readList r readExpr)
                    | _ ->
                        let count = int (r.ReadUInt16())
                        if count > MaxListCount then
                            failwithf "Named argument count %d exceeds maximum of %d" count MaxListCount
                        let pairs = [ for _ in 1 .. count -> (readString r, readExpr r) ]
                        NamedArgs(pairs)
                let pos = readPosition r
                Call(name, args, pos)
            | 0x07uy ->
                let target = readExpr r
                let prop = readString r
                let pos = readPosition r
                PropertyAccess(target, prop, pos)
            | 0x08uy ->
                let target = readExpr r
                let index = readExpr r
                let pos = readPosition r
                IndexAccess(target, index, pos)
            | 0x09uy ->
                let count = int (r.ReadUInt16())
                if count > MaxListCount then
                    failwithf "Object property count %d exceeds maximum of %d" count MaxListCount
                let props = [ for _ in 1 .. count -> (readString r, readExpr r) ]
                let pos = readPosition r
                ObjectLiteral(props, pos)
            | 0x0Auy ->
                let elements = readList r readExpr
                let pos = readPosition r
                ArrayLiteral(elements, pos)
            | 0x0Buy ->
                let parameters = readList r readString
                let body = readExpr r
                let pos = readPosition r
                Lambda(parameters, body, pos)
            | 0x0Cuy ->
                let expr = readExpr r
                let typeName = readJyroType r
                let isNegated = r.ReadByte() <> 0uy
                let pos = readPosition r
                TypeCheck(expr, typeName, isNegated, pos)
            | 0x0Duy ->
                let expr = readExpr r
                let isIncrement = r.ReadByte() <> 0uy
                let isPrefix = r.ReadByte() <> 0uy
                let pos = readPosition r
                IncrementDecrement(expr, isIncrement, isPrefix, pos)
            | 0x0Euy ->
                let expr = readExpr r
                let cases = readList r readMatchExprCase
                let pos = readPosition r
                MatchExpr(expr, cases, pos)
            | tag -> failwithf "Unknown Expr tag: 0x%02X" tag
        finally
            exitDepth r

    and private readSwitchCase (r: BinaryReader) : SwitchCase =
        let values = readList r readExpr
        let body = readList r readStmt
        { Values = values; Body = body }

    and private readUnionVariant (r: BinaryReader) : UnionVariant =
        let name = readString r
        let fieldCount = int (r.ReadUInt16())
        if fieldCount > MaxListCount then
            failwithf "Field count %d exceeds maximum of %d" fieldCount MaxListCount
        let fields = [ for _ in 1 .. fieldCount -> (readString r, readOption r readJyroType) ]
        let pos = readPosition r
        { Name = name; Fields = fields; Pos = pos }

    and private readMatchCase (r: BinaryReader) : MatchCase =
        let variantName = readString r
        let bindings = readList r readString
        let body = readList r readStmt
        let pos = readPosition r
        { VariantName = variantName; Bindings = bindings; Body = body; Pos = pos }

    and private readMatchExprCase (r: BinaryReader) : MatchExprCase =
        let variantName = readString r
        let bindings = readList r readString
        let body = readExpr r
        let pos = readPosition r
        { VariantName = variantName; Bindings = bindings; Body = body; Pos = pos }

    and private readStmt (r: BinaryReader) : Stmt =
        // Each statement, nested ones included, is where an abandoned load
        // stops (JYRO-ADR-100).
        CompilationStage.CheckAmbient()
        enterDepth r
        try
            match r.ReadByte() with
            | 0x20uy ->
                let name = readString r
                let typeHint = readOption r readJyroType
                let init = readOption r readExpr
                let pos = readPosition r
                VarDecl(name, typeHint, init, pos)
            | 0x21uy ->
                let target = readExpr r
                let op = readAssignOp r
                let value = readExpr r
                let typeHint = readOption r readJyroType
                let pos = readPosition r
                Assignment(target, op, value, typeHint, pos)
            | 0x22uy ->
                let cond = readExpr r
                let thenBlock = readList r readStmt
                let elseIfCount = int (r.ReadUInt16())
                if elseIfCount > MaxListCount then
                    failwithf "ElseIf count %d exceeds maximum of %d" elseIfCount MaxListCount
                let elseIfs = [ for _ in 1 .. elseIfCount -> (readExpr r, readList r readStmt) ]
                let elseBlock = readOption r (fun rd -> readList rd readStmt)
                let pos = readPosition r
                If(cond, thenBlock, elseIfs, elseBlock, pos)
            | 0x23uy ->
                let cond = readExpr r
                let body = readList r readStmt
                let pos = readPosition r
                While(cond, body, pos)
            | 0x24uy ->
                let varName = readString r
                let collection = readExpr r
                let body = readList r readStmt
                let pos = readPosition r
                ForEach(varName, collection, body, pos)
            | 0x25uy ->
                let varName = readString r
                let startExpr = readExpr r
                let endExpr = readExpr r
                let stepExpr = readOption r readExpr
                // An enumeration tag, not a flag (spec 150.5): `0x00` and
                // `0x01` are assigned and every other byte is refused
                // (150.2.2). It read every non-zero byte as Descending.
                let direction =
                    match r.ReadByte() with
                    | 0uy -> Ascending
                    | 1uy -> Descending
                    | tag -> failwithf "Unknown for direction tag: %d" tag
                let body = readList r readStmt
                let pos = readPosition r
                For(varName, startExpr, endExpr, stepExpr, direction, body, pos)
            | 0x26uy ->
                let expr = readExpr r
                let cases = readList r readSwitchCase
                let defaultCase = readOption r (fun rd -> readList rd readStmt)
                let pos = readPosition r
                Switch(expr, cases, defaultCase, pos)
            | 0x27uy ->
                let value = readOption r readExpr
                let pos = readPosition r
                Return(value, pos)
            | 0x28uy ->
                let message = readOption r readExpr
                let pos = readPosition r
                Fail(message, pos)
            | 0x29uy ->
                let pos = readPosition r
                Break pos
            | 0x2Auy ->
                let pos = readPosition r
                Continue pos
            | 0x2Buy ->
                let expr = readExpr r
                let pos = readPosition r
                ExprStmt(expr, pos)
            | 0x2Cuy ->
                let name = readString r
                let paramCount = int (r.ReadUInt16())
                if paramCount > MaxListCount then
                    failwithf "Parameter count %d exceeds maximum of %d" paramCount MaxListCount
                let parameters =
                    [ for _ in 1 .. paramCount ->
                        { Name = readString r
                          TypeHint = readOption r readJyroType
                          DefaultValue = readOption r readExpr } ]
                let body = readList r readStmt
                let pos = readPosition r
                FuncDef(name, parameters, body, pos)
            | 0x2Duy ->
                let value = readOption r readExpr
                let pos = readPosition r
                Exit(value, pos)
            | 0x2Euy ->
                let name = readString r
                let variants = readList r readUnionVariant
                let pos = readPosition r
                UnionDef(name, variants, pos)
            | 0x2Fuy ->
                let expr = readExpr r
                let cases = readList r readMatchCase
                let pos = readPosition r
                Match(expr, cases, pos)
            | 0x30uy ->
                let target = readExpr r
                let pos = readPosition r
                Delete(target, pos)
            | tag -> failwithf "Unknown Stmt tag: 0x%02X" tag
        finally
            exitDepth r

    // --- Public API ---

    /// Serializes a validated program, its function dependency names and its
    /// source hash into .jyrx binary format.
    ///
    /// Returns a `JyroResult` rather than throwing, matching `deserialize` -
    /// the two are inverses and reported failure two different ways, so a host
    /// calling one had to handle a result and the other an exception. The
    /// encoder still refuses loudly rather than writing something it cannot
    /// read back (an array or object literal has no encoding); what changed is
    /// that the refusal arrives through the same channel as every other stage.
    let serialize (program: Program) (functionNames: string list) (sourceHash: byte[]) : JyroResult<byte[]> =
        try
            use ms = new MemoryStream()
            use w = new DepthTrackingWriter(ms)

            w.Write(Encoding.ASCII.GetBytes(MagicBytes))
            w.Write(FormatVersion)
            w.Write(0us) // flags reserved
            w.Write(sourceHash) // 32 bytes SHA256
            // Checked, then written directly: this is the one count the format
            // holds in a `uint32`, so `writeCount`'s `uint16` does not fit it.
            // Going around the helper is also how it came to be unchecked.
            checkCount "FunctionTable" functionNames.Length MaxFunctionCount
            w.Write(uint32 functionNames.Length)

            functionNames |> List.iter (fun name ->
                CompilationStage.CheckAmbient()
                writeString w name)

            writeList w writeStmt program.Statements

            w.Flush()
            // The artifact ends with the last byte of the program (spec
            // 150.3.9), so its size is known only now. One larger than a
            // reader accepts (150.9.1) is refused with `File` and no second
            // argument (150.9.7, 150.11.4), not written.
            if ms.Length > int64 MaxFileSize then
                JyroError.raiseRuntime MessageCode.ArtifactCannotEncode [| box "File" |]
            JyroResult<byte[]>.Success(ms.ToArray())
        with
        // An abandoned compilation stopping at a check: nobody reads its
        // result, and an unwritable artifact would misreport it.
        | :? OperationCanceledException -> reraise ()
        // A refusal the writer decided on passes through with its own code and
        // arguments. Wrapping it would discard both, and would report a
        // condition the engine recognized as though something had merely gone
        // wrong - the distinction `HostFunctionError` keeps from `RuntimeError`.
        | :? JyroRuntimeException as ex when ex.Code = MessageCode.ArtifactCannotEncode ->
            JyroResult<byte[]>.Failure(DiagnosticMessage.Of(ex.Code, args = ex.Args))
        // Anything else is unexpected. It reports as *unwritable*, not
        // unreadable: this handler used to borrow the read side's code, so a
        // failure to write arrived labeled as a failure to read and a host
        // routing on the code looked at the wrong half of the pipeline.
        // No argument: the exception's type and message are the host
        // platform's, not names the specification gives (spec 130.6.2).
        | _ ->
            JyroResult<byte[]>.Failure(DiagnosticMessage.Of(MessageCode.ArtifactUnwritable))

    /// Deserialize a .jyrx binary into the program AST and required function names.
    let deserialize (data: byte[]) : JyroResult<DeserializedProgram> =
        if data.Length > MaxFileSize then
            JyroResult<DeserializedProgram>.Failure(
                DiagnosticMessage.Of(MessageCode.ArtifactTooLarge,
                    args = [| box data.Length; box MaxFileSize |]))
        else
        try
            use ms = new MemoryStream(data)
            use r = new DepthTrackingReader(ms)

            let magic = Encoding.ASCII.GetString(r.ReadBytes(4))
            if magic <> MagicBytes then
                JyroResult<DeserializedProgram>.Failure(
                    DiagnosticMessage.Of(MessageCode.ArtifactInvalidMagic,
                        args = [| box magic |]))
            else

            let version = r.ReadUInt16()
            if version <> FormatVersion then
                JyroResult<DeserializedProgram>.Failure(
                    DiagnosticMessage.Of(MessageCode.ArtifactVersionUnsupported,
                        args = [| box (int version); box (int FormatVersion) |]))
            else
            let _flags = r.ReadUInt16()
            // Read to advance past the hash, not for its value: `deserialize`
            // no longer carries it. A caller checking a .jyrx against its
            // source reads the header instead - see `readHeader`, which is
            // what jyro-cli's validate command uses.
            let _sourceHash = r.ReadBytes(32)
            // Compared unsigned, as `readHeader` compares it: read as an `int`,
            // a count of 2^31 or more was negative, passed the check, and was
            // read as an empty table (spec 150.9.4, 150.2.9 row 5).
            let rawCount = r.ReadUInt32()
            if rawCount > uint32 MaxFunctionCount then
                failwithf "Function count %d exceeds maximum of %d" rawCount MaxFunctionCount
            let funcCount = int rawCount

            let functionNames =
                [ for _ in 1 .. funcCount do
                    CompilationStage.CheckAmbient()
                    yield readString r ]
            checkTableHasNoRepeatedName functionNames

            let statements = readList r readStmt
            // Nothing follows the program (spec 150.3.9, 150.2.9 row 7): a
            // byte after it means the file was changed or joined to another.
            if ms.Position <> ms.Length then
                failwithf "%d bytes follow the program" (ms.Length - ms.Position)
            let program = { Statements = statements }

            let result =
                { Program = program
                  RequiredFunctions = functionNames }

            JyroResult<DeserializedProgram>.Success(result)
        with
        // An abandoned load stopping at a check: nobody reads its result,
        // and an unreadable artifact would misreport it.
        | :? OperationCanceledException -> reraise ()
        // No argument: the exception's type and message are the host
        // platform's, not names the specification gives (spec 130.6.2).
        | _ ->
            JyroResult<DeserializedProgram>.Failure(DiagnosticMessage.Of(MessageCode.ArtifactUnreadable))

    /// Read only the header and function table from a .jyrx binary, without deserializing the AST.
    /// Use this for pre-flight validation (hash comparison, function availability checks) without
    /// paying the cost of full AST deserialization.
    ///
    /// It applies the same three guards `deserialize` does - file size, format
    /// version, function count - and it applied none of them. It read the
    /// version without comparing it, the file without measuring it, and the
    /// function table with no cap, so it reported success for artifacts the
    /// load path refuses: a header patched to version 7 read cleanly and then
    /// failed to compile, which is precisely backwards for a pre-flight check.
    /// That mattered little while nothing called it; it is public API now, and
    /// jyro-cli's `validate` command is built on it.
    ///
    /// Two readers of one format have to agree on what a valid artifact is, or
    /// the weaker one is not checking the format - it is checking a subset of
    /// it and calling the answer valid.
    let readHeader (data: byte[]) : JyroResult<JyrxHeader> =
        if data.Length > MaxFileSize then
            JyroResult<JyrxHeader>.Failure(
                DiagnosticMessage.Of(MessageCode.ArtifactTooLarge,
                    args = [| box data.Length; box MaxFileSize |]))
        else
        try
            use ms = new MemoryStream(data)
            use r = new BinaryReader(ms, Encoding.UTF8, true)

            let magic = Encoding.ASCII.GetString(r.ReadBytes(4))
            if magic <> MagicBytes then
                JyroResult<JyrxHeader>.Failure(
                    DiagnosticMessage.Of(MessageCode.ArtifactInvalidMagic,
                        args = [| box magic |]))
            else

            let version = r.ReadUInt16()
            if version <> FormatVersion then
                JyroResult<JyrxHeader>.Failure(
                    DiagnosticMessage.Of(MessageCode.ArtifactVersionUnsupported,
                        args = [| box (int version); box (int FormatVersion) |]))
            else
            let _flags = r.ReadUInt16()
            let sourceHash = r.ReadBytes(32)
            // Compared unsigned: read as an `int`, a count of 2^31 or more is
            // negative, passed the check, and read as an empty table, so a
            // header that gives 4,294,967,295 entries was reported as valid.
            let rawCount = r.ReadUInt32()
            if rawCount > uint32 MaxFunctionCount then
                failwithf "Function count %d exceeds maximum of %d" rawCount MaxFunctionCount
            let funcCount = int rawCount
            // Every name decodes (check 5 of spec 150.2.8) before any two are
            // compared (check 6).
            let functionNames = [ for _ in 1 .. funcCount -> readString r ]
            checkTableHasNoRepeatedName functionNames

            JyroResult<JyrxHeader>.Success(
                { Version = int version
                  SourceHash = sourceHash
                  RequiredFunctions = functionNames })
        with _ ->
            // No argument, as in `deserialize` (spec 130.6.2).
            JyroResult<JyrxHeader>.Failure(DiagnosticMessage.Of(MessageCode.ArtifactUnreadable))

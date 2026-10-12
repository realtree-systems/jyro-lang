/// A compiled-artifact encoder and decoder written from Part 150 of the book
/// alone (150.3 to 150.8). The tests use it to read what the implementation
/// writes (CompileToArtifact) and to hand-build artifacts the implementation's
/// reader must accept or reject. It follows the book's tables field for field:
/// every multi-byte number little-endian, every string a uint16 byte count then
/// UTF-8, every list a uint16 count then its items, every option a presence byte
/// (0x00 absent) then the value, every position four int32 (line, column,
/// start index, end index).
module RealTree.Jyro.Spec.Tests.Artifact

open System
open System.IO
open System.Security.Cryptography
open System.Text

type Pos = { Line: int; Column: int; Start: int; End: int }

let p0 = { Line = 1; Column = 1; Start = 0; End = 0 }

/// A literal value (150.6.1). The Raw cases write their bytes as given, for
/// artifacts the reader must reject.
type Lit =
    | LNull
    | LBool of byte
    | LInexact of float
    | LExact of decimal
    /// Representation byte 0x01 and the 16 exact bytes as given.
    | LExactBytes of byte[]
    /// Tag 0x03, then this representation byte and payload as given.
    | LNumRaw of byte * byte[]
    | LStr of string
    /// This literal tag and payload as given.
    | LRaw of byte * byte[]

type Expr =
    | ELit of Lit * Pos
    | EId of string * Pos
    | EBin of Expr * byte * Expr * Pos
    | EUn of byte * Expr * Pos
    | ETern of Expr * Expr * Expr * Pos
    | ECall of string * Args * Pos
    | EProp of Expr * string * Pos
    | EIdx of Expr * Expr * Pos
    | EObj of (string * Expr) list * Pos
    | EArr of Expr list * Pos
    | ELambda of string list * Expr * Pos
    | ETypeCheck of Expr * byte * byte * Pos
    | EIncDec of Expr * byte * byte * Pos
    | EMatch of Expr * (string * string list * Expr * Pos) list * Pos
    /// These bytes as given, tag included.
    | ERaw of byte[]

and Args =
    /// Arg-form byte 0x00, then a list of expressions.
    | Positional of Expr list
    /// This arg-form byte (non-zero), a uint16 count and (name, expression) pairs.
    | Named of byte * (string * Expr) list

/// A union variant (150.8.5): name, fields (name, option of a hint tag), then its position.
type Variant = string * (string * byte option) list * Pos

type Stmt =
    | SVar of string * byte option * Expr option * Pos
    | SAssign of Expr * byte * Expr * byte option * Pos
    | SIf of Expr * Stmt list * (Expr * Stmt list) list * Stmt list option * Pos
    | SWhile of Expr * Stmt list * Pos
    | SForEach of string * Expr * Stmt list * Pos
    | SFor of string * Expr * Expr * Expr option * byte * Stmt list * Pos
    | SSwitch of Expr * (Expr list * Stmt list) list * Stmt list option * Pos
    | SReturn of Expr option * Pos
    | SFail of Expr option * Pos
    | SBreak of Pos
    | SContinue of Pos
    | SExpr of Expr * Pos
    | SFunc of string * (string * byte option * Expr option) list * Stmt list * Pos
    | SExit of Expr option * Pos
    /// A union: its name, its variants and its position. A variant is its name, its fields
    /// (name, option of a hint tag) and its own position (150.8.5, format version 1).
    | SUnion of string * Variant list * Pos
    | SMatch of Expr * (string * string list * Stmt list * Pos) list * Pos
    | SDelete of Expr * Pos
    /// These bytes as given, tag included.
    | SRaw of byte[]

type Artifact =
    { Magic: byte[]
      Version: uint16
      Reserved: uint16
      Hash: byte[]
      /// The function table, written in this order.
      Functions: string list
      /// Written in place of the table's length when set.
      FunctionCount: uint32 option
      Program: Stmt list }

// -- Tags (150.5, 150.7.1, 150.8.1) ------------------------------------------------

module Hint =
    let any, number, string, boolean, object, array, nul, func = 0uy, 1uy, 2uy, 3uy, 4uy, 5uy, 6uy, 7uy

module Op =
    let add, sub, mul, div, modulo = 0x00uy, 0x01uy, 0x02uy, 0x03uy, 0x04uy
    let eq, ne, lt, le, gt, ge = 0x05uy, 0x06uy, 0x07uy, 0x08uy, 0x09uy, 0x0Auy
    let andOp, orOp, coalesce, concat = 0x0Buy, 0x0Cuy, 0x0Duy, 0x0Euy
    let negate, notOp = 0x00uy, 0x01uy
    let assign, addAssign, subAssign, mulAssign, divAssign, modAssign, concatAssign =
        0uy, 1uy, 2uy, 3uy, 4uy, 5uy, 6uy
    let upTo, downTo = 0uy, 1uy

// -- Writing ----------------------------------------------------------------------

/// Writes the encoding. `present` is the byte written for a present option
/// (150.4.3: any non-zero byte means present).
type Writer(present: byte) =
    let ms = new MemoryStream()
    let w = new BinaryWriter(ms)

    member _.Bytes() : byte[] = w.Flush(); ms.ToArray()
    member _.U8(b: byte) = w.Write b
    /// Refuses a value its 16 bits cannot hold rather than wrap it, so that a
    /// helper cannot write a length that disagrees with the data (JYRO-DEF-735).
    /// A test that means a wrong count patches the bytes instead.
    member _.U16(n: int) =
        if n < 0 || n > 65_535 then
            invalidArg "n" (sprintf "%d does not fit an unsigned 16-bit field" n)
        w.Write(uint16 n)
    member _.U32(n: uint32) = w.Write n
    member _.I32(n: int) = w.Write n
    member _.Raw(b: byte[]) = w.Write b

    member this.Str(s: string) =
        let b = Encoding.UTF8.GetBytes s
        this.U16 b.Length
        this.Raw b

    member this.Pos(p: Pos) =
        this.I32 p.Line
        this.I32 p.Column
        this.I32 p.Start
        this.I32 p.End

    member this.Opt(v: 'T option, write: 'T -> unit) =
        match v with
        | None -> this.U8 0uy
        | Some x ->
            this.U8 present
            write x

    member this.List(items: 'T list, write: 'T -> unit) =
        this.U16 items.Length
        for i in items do write i

    member this.Exact(d: decimal) =
        let bits = Decimal.GetBits d
        this.I32 bits.[0]
        this.I32 bits.[1]
        this.I32 bits.[2]
        this.U8 0uy
        this.U8 0uy
        this.U8(byte ((bits.[3] >>> 16) &&& 0xFF))
        this.U8(if bits.[3] < 0 then 0x80uy else 0x00uy)

    member this.Lit(l: Lit) =
        match l with
        | LNull -> this.U8 0x01uy
        | LBool b -> this.U8 0x02uy; this.U8 b
        | LInexact f -> this.U8 0x03uy; this.U8 0x00uy; w.Write(f)
        | LExact d -> this.U8 0x03uy; this.U8 0x01uy; this.Exact d
        | LExactBytes b -> this.U8 0x03uy; this.U8 0x01uy; this.Raw b
        | LNumRaw(rep, b) -> this.U8 0x03uy; this.U8 rep; this.Raw b
        | LStr s -> this.U8 0x04uy; this.Str s
        | LRaw(tag, b) -> this.U8 tag; this.Raw b

    member this.Expr(e: Expr) =
        match e with
        | ELit(l, p) -> this.U8 0x01uy; this.Lit l; this.Pos p
        | EId(n, p) -> this.U8 0x02uy; this.Str n; this.Pos p
        | EBin(l, op, r, p) -> this.U8 0x03uy; this.Expr l; this.U8 op; this.Expr r; this.Pos p
        | EUn(op, x, p) -> this.U8 0x04uy; this.U8 op; this.Expr x; this.Pos p
        | ETern(c, t, f, p) -> this.U8 0x05uy; this.Expr c; this.Expr t; this.Expr f; this.Pos p
        | ECall(n, args, p) ->
            this.U8 0x06uy
            this.Str n
            match args with
            | Positional xs ->
                this.U8 0x00uy
                this.List(xs, this.Expr)
            | Named(form, pairs) ->
                this.U8 form
                this.U16 pairs.Length
                for (k, v) in pairs do
                    this.Str k
                    this.Expr v
            this.Pos p
        | EProp(t, n, p) -> this.U8 0x07uy; this.Expr t; this.Str n; this.Pos p
        | EIdx(t, i, p) -> this.U8 0x08uy; this.Expr t; this.Expr i; this.Pos p
        | EObj(pairs, p) ->
            this.U8 0x09uy
            this.U16 pairs.Length
            for (k, v) in pairs do
                this.Str k
                this.Expr v
            this.Pos p
        | EArr(xs, p) -> this.U8 0x0Auy; this.List(xs, this.Expr); this.Pos p
        | ELambda(ps, body, p) -> this.U8 0x0Buy; this.List(ps, this.Str); this.Expr body; this.Pos p
        | ETypeCheck(x, hint, neg, p) -> this.U8 0x0Cuy; this.Expr x; this.U8 hint; this.U8 neg; this.Pos p
        | EIncDec(x, inc, prefix, p) -> this.U8 0x0Duy; this.Expr x; this.U8 inc; this.U8 prefix; this.Pos p
        | EMatch(x, cases, p) ->
            this.U8 0x0Euy
            this.Expr x
            this.List(cases, fun (v, bs, body, cp) ->
                this.Str v
                this.List(bs, this.Str)
                this.Expr body
                this.Pos cp)
            this.Pos p
        | ERaw b -> this.Raw b

    member this.Block(b: Stmt list) = this.List(b, this.Stmt)

    member this.Stmt(s: Stmt) =
        match s with
        | SVar(n, hint, init, p) ->
            this.U8 0x20uy; this.Str n; this.Opt(hint, this.U8); this.Opt(init, this.Expr); this.Pos p
        | SAssign(t, op, v, hint, p) ->
            this.U8 0x21uy; this.Expr t; this.U8 op; this.Expr v; this.Opt(hint, this.U8); this.Pos p
        | SIf(c, thenB, elseifs, elseB, p) ->
            this.U8 0x22uy
            this.Expr c
            this.Block thenB
            this.U16 elseifs.Length
            for (ec, eb) in elseifs do
                this.Expr ec
                this.Block eb
            this.Opt(elseB, this.Block)
            this.Pos p
        | SWhile(c, b, p) -> this.U8 0x23uy; this.Expr c; this.Block b; this.Pos p
        | SForEach(v, c, b, p) -> this.U8 0x24uy; this.Str v; this.Expr c; this.Block b; this.Pos p
        | SFor(v, st, en, step, dir, b, p) ->
            this.U8 0x25uy; this.Str v; this.Expr st; this.Expr en; this.Opt(step, this.Expr); this.U8 dir
            this.Block b; this.Pos p
        | SSwitch(subj, cases, def, p) ->
            this.U8 0x26uy
            this.Expr subj
            this.List(cases, fun (vals, body) ->
                this.List(vals, this.Expr)
                this.Block body)
            this.Opt(def, this.Block)
            this.Pos p
        | SReturn(v, p) -> this.U8 0x27uy; this.Opt(v, this.Expr); this.Pos p
        | SFail(v, p) -> this.U8 0x28uy; this.Opt(v, this.Expr); this.Pos p
        | SBreak p -> this.U8 0x29uy; this.Pos p
        | SContinue p -> this.U8 0x2Auy; this.Pos p
        | SExpr(e, p) -> this.U8 0x2Buy; this.Expr e; this.Pos p
        | SFunc(n, ps, body, p) ->
            this.U8 0x2Cuy
            this.Str n
            this.U16 ps.Length
            for (pn, hint, def) in ps do
                this.Str pn
                this.Opt(hint, this.U8)
                this.Opt(def, this.Expr)
            this.Block body
            this.Pos p
        | SExit(v, p) -> this.U8 0x2Duy; this.Opt(v, this.Expr); this.Pos p
        | SUnion(n, variants, p) ->
            this.U8 0x2Euy
            this.Str n
            this.List(variants, fun (vn, fields, vp) ->
                this.Str vn
                this.U16 fields.Length
                for (fname, hint) in fields do
                    this.Str fname
                    this.Opt(hint, this.U8)
                this.Pos vp)
            this.Pos p
        | SMatch(x, cases, p) ->
            this.U8 0x2Fuy
            this.Expr x
            this.List(cases, fun (v, bs, body, cp) ->
                this.Str v
                this.List(bs, this.Str)
                this.Block body
                this.Pos cp)
            this.Pos p
        | SDelete(t, p) -> this.U8 0x30uy; this.Expr t; this.Pos p
        | SRaw b -> this.Raw b

// -- Names the program calls ---------------------------------------------------------

let rec private exprCalls (e: Expr) : string list =
    match e with
    | ELit _ | EId _ | ERaw _ -> []
    | EBin(l, _, r, _) -> exprCalls l @ exprCalls r
    | EUn(_, x, _) -> exprCalls x
    | ETern(a, b, c, _) -> exprCalls a @ exprCalls b @ exprCalls c
    | ECall(n, args, _) ->
        n :: (match args with
              | Positional xs -> List.collect exprCalls xs
              | Named(_, ps) -> ps |> List.collect (snd >> exprCalls))
    | EProp(t, _, _) -> exprCalls t
    | EIdx(t, i, _) -> exprCalls t @ exprCalls i
    | EObj(ps, _) -> ps |> List.collect (snd >> exprCalls)
    | EArr(xs, _) -> List.collect exprCalls xs
    | ELambda(_, b, _) -> exprCalls b
    | ETypeCheck(x, _, _, _) | EIncDec(x, _, _, _) -> exprCalls x
    | EMatch(x, cases, _) -> exprCalls x @ (cases |> List.collect (fun (_, _, b, _) -> exprCalls b))

and private stmtCalls (s: Stmt) : string list =
    let opt f = function Some x -> f x | None -> []
    let block = List.collect stmtCalls
    match s with
    | SVar(_, _, i, _) -> opt exprCalls i
    | SAssign(t, _, v, _, _) -> exprCalls t @ exprCalls v
    | SIf(c, t, eis, e, _) ->
        exprCalls c @ block t @ (eis |> List.collect (fun (c, b) -> exprCalls c @ block b)) @ opt block e
    | SWhile(c, b, _) -> exprCalls c @ block b
    | SForEach(_, c, b, _) -> exprCalls c @ block b
    | SFor(_, a, b, st, _, body, _) -> exprCalls a @ exprCalls b @ opt exprCalls st @ block body
    | SSwitch(x, cases, d, _) ->
        exprCalls x @ (cases |> List.collect (fun (vs, b) -> List.collect exprCalls vs @ block b)) @ opt block d
    | SReturn(v, _) | SFail(v, _) | SExit(v, _) -> opt exprCalls v
    | SBreak _ | SContinue _ | SRaw _ | SUnion _ -> []
    | SExpr(e, _) | SDelete(e, _) -> exprCalls e
    | SFunc(_, ps, b, _) -> (ps |> List.collect (fun (_, _, d) -> opt exprCalls d)) @ block b
    | SMatch(x, cases, _) -> exprCalls x @ (cases |> List.collect (fun (_, _, b, _) -> block b))

/// The function table 150.3.3 gives for a program: every name a call names,
/// except declared functions and union variants, once, in ordinal order.
let tableFor (program: Stmt list) : string list =
    let declared =
        program
        |> List.collect (function
            | SFunc(n, _, _, _) -> [ n ]
            | SUnion(_, vs, _) -> vs |> List.map (fun (n, _, _) -> n)
            | _ -> [])
        |> Set.ofList
    program
    |> List.collect stmtCalls
    |> List.filter (fun n -> not (declared.Contains n))
    |> List.distinct
    |> List.sortWith (fun a b -> String.CompareOrdinal(a, b))

let sha256 (source: string) : byte[] = SHA256.HashData(Encoding.UTF8.GetBytes source)

/// The format version of this specification (150.10.4): the one version a
/// reader of this specification reads (150.10.5).
let formatVersion : uint16 = 1us

/// An artifact of `program` with a correct header and function table, at the
/// format version of this specification (1, JYRO-REQ-1920).
let artifactOf (program: Stmt list) : Artifact =
    { Magic = Encoding.ASCII.GetBytes "JYRX"
      Version = formatVersion
      Reserved = 0us
      Hash = sha256 "hand-built"
      Functions = tableFor program
      FunctionCount = None
      Program = program }

let encodeWith (present: byte) (a: Artifact) : byte[] =
    let w = Writer(present)
    w.Raw a.Magic
    w.U16(int a.Version)
    w.U16(int a.Reserved)
    w.Raw a.Hash
    w.U32(defaultArg a.FunctionCount (uint32 a.Functions.Length))
    for f in a.Functions do w.Str f
    w.Block a.Program
    w.Bytes()

let encode (a: Artifact) : byte[] = encodeWith 0x01uy a

/// The bytes of a hand-built artifact of `program`.
let build (program: Stmt list) : byte[] = encode (artifactOf program)

/// The bytes of one expression (for splicing into ERaw).
let exprBytes (e: Expr) : byte[] =
    let w = Writer(0x01uy)
    w.Expr e
    w.Bytes()

/// The bytes of one statement (for splicing into SRaw).
let stmtBytes (s: Stmt) : byte[] =
    let w = Writer(0x01uy)
    w.Stmt s
    w.Bytes()

/// The bytes of one position.
let posBytes (p: Pos) : byte[] =
    let w = Writer(0x01uy)
    w.Pos p
    w.Bytes()

// -- Reading -------------------------------------------------------------------------

exception DecodeError of string

/// Decodes the encoding strictly: any unassigned tag, a string that is not
/// UTF-8, a count over 10,000 or a byte left over is a DecodeError.
type Reader(bytes: byte[]) =
    let mutable pos = 0
    let need n = if pos + n > bytes.Length then raise (DecodeError(sprintf "truncated at %d (need %d)" pos n))

    member _.Offset = pos
    member _.AtEnd = pos = bytes.Length

    member _.U8() : byte =
        need 1
        let b = bytes.[pos]
        pos <- pos + 1
        b

    member _.U16() : int =
        need 2
        let v = int (BitConverter.ToUInt16(bytes, pos))
        pos <- pos + 2
        v

    member _.U32() : uint32 =
        need 4
        let v = BitConverter.ToUInt32(bytes, pos)
        pos <- pos + 4
        v

    member _.I32() : int =
        need 4
        let v = BitConverter.ToInt32(bytes, pos)
        pos <- pos + 4
        v

    member _.Take(n: int) : byte[] =
        need n
        let b = bytes.[pos .. pos + n - 1]
        pos <- pos + n
        b

    member this.Str() : string =
        let n = this.U16()
        let b = this.Take n
        try UTF8Encoding(false, true).GetString b
        with _ -> raise (DecodeError "string is not UTF-8")

    member this.Pos() : Pos =
        let l = this.I32()
        let c = this.I32()
        let s = this.I32()
        let e = this.I32()
        { Line = l; Column = c; Start = s; End = e }

    member this.Count() : int =
        let n = this.U16()
        if n > 10_000 then raise (DecodeError(sprintf "count %d above 10,000" n))
        n

    member this.List(read: unit -> 'T) : 'T list = [ for _ in 1 .. this.Count() -> read () ]

    member this.Opt(read: unit -> 'T) : 'T option =
        match this.U8() with
        | 0uy -> None
        | _ -> Some(read ())

    member this.Tag(max: byte) : byte =
        let b = this.U8()
        if b > max then raise (DecodeError(sprintf "unassigned tag %d" b))
        b

    member this.Hint() = this.Tag 0x07uy

    member this.Exact() : decimal =
        let lo = this.I32()
        let mid = this.I32()
        let hi = this.I32()
        let r1 = this.U8()
        let r2 = this.U8()
        let scale = this.U8()
        let sign = this.U8()
        if r1 <> 0uy || r2 <> 0uy then raise (DecodeError "reserved bytes of an exact number")
        if scale > 28uy then raise (DecodeError "scale above 28")
        if sign <> 0uy && sign <> 0x80uy then raise (DecodeError "sign byte")
        Decimal(lo, mid, hi, (sign = 0x80uy), scale)

    member this.Lit() : Lit =
        match this.U8() with
        | 0x01uy -> LNull
        | 0x02uy -> LBool(this.U8())
        | 0x03uy ->
            match this.U8() with
            | 0x00uy ->
                let f = BitConverter.ToDouble(this.Take 8, 0)
                if Double.IsNaN f || Double.IsInfinity f then raise (DecodeError "not finite")
                LInexact f
            | 0x01uy -> LExact(this.Exact())
            | r -> raise (DecodeError(sprintf "representation %d" r))
        | 0x04uy -> LStr(this.Str())
        | t -> raise (DecodeError(sprintf "literal tag %d" t))

    member this.Expr() : Expr =
        match this.U8() with
        | 0x01uy -> let l = this.Lit() in ELit(l, this.Pos())
        | 0x02uy -> let n = this.Str() in EId(n, this.Pos())
        | 0x03uy ->
            let l = this.Expr()
            let op = this.Tag 0x0Euy
            let r = this.Expr()
            EBin(l, op, r, this.Pos())
        | 0x04uy ->
            let op = this.Tag 0x01uy
            let x = this.Expr()
            EUn(op, x, this.Pos())
        | 0x05uy ->
            let c = this.Expr()
            let t = this.Expr()
            let f = this.Expr()
            ETern(c, t, f, this.Pos())
        | 0x06uy ->
            let n = this.Str()
            let form = this.U8()
            let args =
                if form = 0uy then Positional(this.List this.Expr)
                else
                    let count = this.Count()
                    Named(form, [ for _ in 1 .. count -> (let k = this.Str() in k, this.Expr()) ])
            ECall(n, args, this.Pos())
        | 0x07uy ->
            let t = this.Expr()
            let n = this.Str()
            EProp(t, n, this.Pos())
        | 0x08uy ->
            let t = this.Expr()
            let i = this.Expr()
            EIdx(t, i, this.Pos())
        | 0x09uy ->
            let count = this.Count()
            let pairs = [ for _ in 1 .. count -> (let k = this.Str() in k, this.Expr()) ]
            EObj(pairs, this.Pos())
        | 0x0Auy -> let xs = this.List this.Expr in EArr(xs, this.Pos())
        | 0x0Buy ->
            let ps = this.List this.Str
            let b = this.Expr()
            ELambda(ps, b, this.Pos())
        | 0x0Cuy ->
            let x = this.Expr()
            let h = this.Hint()
            let neg = this.U8()
            ETypeCheck(x, h, neg, this.Pos())
        | 0x0Duy ->
            let x = this.Expr()
            let inc = this.U8()
            let pre = this.U8()
            EIncDec(x, inc, pre, this.Pos())
        | 0x0Euy ->
            let x = this.Expr()
            let cases =
                this.List(fun () ->
                    let v = this.Str()
                    let bs = this.List this.Str
                    let b = this.Expr()
                    (v, bs, b, this.Pos()))
            EMatch(x, cases, this.Pos())
        | t -> raise (DecodeError(sprintf "expression tag %d" t))

    member this.Block() : Stmt list = this.List this.Stmt

    member this.Stmt() : Stmt =
        match this.U8() with
        | 0x20uy ->
            let n = this.Str()
            let h = this.Opt this.Hint
            let i = this.Opt this.Expr
            SVar(n, h, i, this.Pos())
        | 0x21uy ->
            let t = this.Expr()
            let op = this.Tag 0x06uy
            let v = this.Expr()
            let h = this.Opt this.Hint
            SAssign(t, op, v, h, this.Pos())
        | 0x22uy ->
            let c = this.Expr()
            let t = this.Block()
            let count = this.Count()
            let eis = [ for _ in 1 .. count -> (let c = this.Expr() in c, this.Block()) ]
            let e = this.Opt this.Block
            SIf(c, t, eis, e, this.Pos())
        | 0x23uy ->
            let c = this.Expr()
            let b = this.Block()
            SWhile(c, b, this.Pos())
        | 0x24uy ->
            let v = this.Str()
            let c = this.Expr()
            let b = this.Block()
            SForEach(v, c, b, this.Pos())
        | 0x25uy ->
            let v = this.Str()
            let a = this.Expr()
            let b = this.Expr()
            let st = this.Opt this.Expr
            let dir = this.Tag 0x01uy
            let body = this.Block()
            SFor(v, a, b, st, dir, body, this.Pos())
        | 0x26uy ->
            let x = this.Expr()
            let cases = this.List(fun () -> let vs = this.List this.Expr in vs, this.Block())
            let d = this.Opt this.Block
            SSwitch(x, cases, d, this.Pos())
        | 0x27uy -> let v = this.Opt this.Expr in SReturn(v, this.Pos())
        | 0x28uy -> let v = this.Opt this.Expr in SFail(v, this.Pos())
        | 0x29uy -> SBreak(this.Pos())
        | 0x2Auy -> SContinue(this.Pos())
        | 0x2Buy -> let e = this.Expr() in SExpr(e, this.Pos())
        | 0x2Cuy ->
            let n = this.Str()
            let count = this.Count()
            let ps =
                [ for _ in 1 .. count ->
                    let pn = this.Str()
                    let h = this.Opt this.Hint
                    let d = this.Opt this.Expr
                    (pn, h, d) ]
            let b = this.Block()
            SFunc(n, ps, b, this.Pos())
        | 0x2Duy -> let v = this.Opt this.Expr in SExit(v, this.Pos())
        | 0x2Euy ->
            let n = this.Str()
            let vs =
                this.List(fun () ->
                    let vn = this.Str()
                    let count = this.Count()
                    let fields = [ for _ in 1 .. count -> (let f = this.Str() in f, this.Opt this.Hint) ]
                    vn, fields, this.Pos())
            SUnion(n, vs, this.Pos())
        | 0x2Fuy ->
            let x = this.Expr()
            let cases =
                this.List(fun () ->
                    let v = this.Str()
                    let bs = this.List this.Str
                    let b = this.Block()
                    (v, bs, b, this.Pos()))
            SMatch(x, cases, this.Pos())
        | 0x30uy -> let t = this.Expr() in SDelete(t, this.Pos())
        | t -> raise (DecodeError(sprintf "statement tag %d" t))

/// Decode a whole artifact. Fails (DecodeError) on anything the book does not
/// allow, and on bytes left over after the program.
let decode (bytes: byte[]) : Artifact =
    let r = Reader(bytes)
    let magic = r.Take 4
    let version = uint16 (r.U16())
    let reserved = uint16 (r.U16())
    let hash = r.Take 32
    let count = r.U32()
    let table = [ for _ in 1 .. int count -> r.Str() ]
    let program = r.Block()
    if not r.AtEnd then raise (DecodeError(sprintf "%d bytes left over" (bytes.Length - r.Offset)))
    { Magic = magic
      Version = version
      Reserved = reserved
      Hash = hash
      Functions = table
      FunctionCount = None
      Program = program }

// -- Building blocks for hand-built programs -----------------------------------------------

let num (d: decimal) : Expr = ELit(LExact d, p0)
let inexact (f: float) : Expr = ELit(LInexact f, p0)
let str (s: string) : Expr = ELit(LStr s, p0)
let tru : Expr = ELit(LBool 1uy, p0)
let fls : Expr = ELit(LBool 0uy, p0)
let nul : Expr = ELit(LNull, p0)
let ident (n: string) : Expr = EId(n, p0)
let prop (t: Expr) (n: string) : Expr = EProp(t, n, p0)
let out (n: string) : Expr = prop (ident "Output") n
let call (n: string) (args: Expr list) : Expr = ECall(n, Positional args, p0)
let bin (l: Expr) (op: byte) (r: Expr) : Expr = EBin(l, op, r, p0)
let setOut (n: string) (e: Expr) : Stmt = SAssign(out n, Op.assign, e, None, p0)
let assign (t: Expr) (e: Expr) : Stmt = SAssign(t, Op.assign, e, None, p0)
let var (n: string) (e: Expr) : Stmt = SVar(n, None, Some e, p0)

/// A union at p0 whose variants, each a name and its fields, are all at p0.
let unionOf (n: string) (variants: (string * (string * byte option) list) list) : Stmt =
    SUnion(n, variants |> List.map (fun (vn, fs) -> vn, fs, p0), p0)

/// The variants of a union without their positions, for comparing shapes.
let variantShapes (variants: Variant list) : (string * (string * byte option) list) list =
    variants |> List.map (fun (vn, fs, _) -> vn, fs)
let arr (xs: Expr list) : Expr = EArr(xs, p0)

/// The positions of a program's statements, with every Pos set to p0 (for
/// comparing the shape of what a writer wrote with what a test expects).
let rec stripExpr (e: Expr) : Expr =
    match e with
    | ELit(l, _) -> ELit(l, p0)
    | EId(n, _) -> EId(n, p0)
    | EBin(l, op, r, _) -> EBin(stripExpr l, op, stripExpr r, p0)
    | EUn(op, x, _) -> EUn(op, stripExpr x, p0)
    | ETern(a, b, c, _) -> ETern(stripExpr a, stripExpr b, stripExpr c, p0)
    | ECall(n, Positional xs, _) -> ECall(n, Positional(List.map stripExpr xs), p0)
    | ECall(n, Named(f, ps), _) -> ECall(n, Named(f, ps |> List.map (fun (k, v) -> k, stripExpr v)), p0)
    | EProp(t, n, _) -> EProp(stripExpr t, n, p0)
    | EIdx(t, i, _) -> EIdx(stripExpr t, stripExpr i, p0)
    | EObj(ps, _) -> EObj(ps |> List.map (fun (k, v) -> k, stripExpr v), p0)
    | EArr(xs, _) -> EArr(List.map stripExpr xs, p0)
    | ELambda(ps, b, _) -> ELambda(ps, stripExpr b, p0)
    | ETypeCheck(x, h, n, _) -> ETypeCheck(stripExpr x, h, n, p0)
    | EIncDec(x, i, pr, _) -> EIncDec(stripExpr x, i, pr, p0)
    | EMatch(x, cases, _) -> EMatch(stripExpr x, cases |> List.map (fun (v, bs, b, _) -> v, bs, stripExpr b, p0), p0)
    | ERaw b -> ERaw b

and stripStmt (s: Stmt) : Stmt =
    let block = List.map stripStmt
    let opt f = Option.map f
    match s with
    | SVar(n, h, i, _) -> SVar(n, h, opt stripExpr i, p0)
    | SAssign(t, op, v, h, _) -> SAssign(stripExpr t, op, stripExpr v, h, p0)
    | SIf(c, t, eis, e, _) -> SIf(stripExpr c, block t, eis |> List.map (fun (c, b) -> stripExpr c, block b), opt block e, p0)
    | SWhile(c, b, _) -> SWhile(stripExpr c, block b, p0)
    | SForEach(v, c, b, _) -> SForEach(v, stripExpr c, block b, p0)
    | SFor(v, a, b, st, d, body, _) -> SFor(v, stripExpr a, stripExpr b, opt stripExpr st, d, block body, p0)
    | SSwitch(x, cases, d, _) ->
        SSwitch(stripExpr x, cases |> List.map (fun (vs, b) -> List.map stripExpr vs, block b), opt block d, p0)
    | SReturn(v, _) -> SReturn(opt stripExpr v, p0)
    | SFail(v, _) -> SFail(opt stripExpr v, p0)
    | SBreak _ -> SBreak p0
    | SContinue _ -> SContinue p0
    | SExpr(e, _) -> SExpr(stripExpr e, p0)
    | SFunc(n, ps, b, _) -> SFunc(n, ps |> List.map (fun (pn, h, d) -> pn, h, opt stripExpr d), block b, p0)
    | SExit(v, _) -> SExit(opt stripExpr v, p0)
    | SUnion(n, vs, _) -> SUnion(n, vs |> List.map (fun (vn, fs, _) -> vn, fs, p0), p0)
    | SMatch(x, cases, _) -> SMatch(stripExpr x, cases |> List.map (fun (v, bs, b, _) -> v, bs, block b, p0), p0)
    | SDelete(t, _) -> SDelete(stripExpr t, p0)
    | SRaw b -> SRaw b

/// Compile `source` to an artifact (default options) and decode it with the
/// book's layout. Fails the test when the writer refuses or the bytes do not
/// decode.
let writtenBy (options: RealTree.Jyro.Spec.RunOptions) (source: string) : byte[] * Artifact =
    let r = RealTree.Jyro.Spec.Script.compileBytesWith options source
    let bytes = RealTree.Jyro.Spec.Script.valueOf r
    if not r.IsSuccess then
        RealTree.Jyro.Spec.Values.fail ("CompileToArtifact failed:\n" + RealTree.Jyro.Spec.Script.describe r)
    let a =
        try decode bytes
        with DecodeError m -> RealTree.Jyro.Spec.Values.fail (sprintf "The artifact does not decode by Part 150: %s" m)
    bytes, a

/// The decoded program of `source`, positions stripped.
let programOf (source: string) : Stmt list =
    (writtenBy RealTree.Jyro.Spec.Script.defaults source |> snd).Program |> List.map stripStmt

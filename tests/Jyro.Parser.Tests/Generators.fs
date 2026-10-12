namespace RealTree.Jyro.Tests

open FsCheck
open FsCheck.FSharp
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing

/// Generators for AST nodes. Produce well-formed trees - generated source
/// must parse back to an AST that satisfies `AstEquality.eqProgram`.
///
/// Conventions:
/// - Identifiers avoid reserved keywords (single source of truth:
///   `Lexer.reservedKeywords`).
/// - Positions are always `Position.None`; equality helpers ignore them.
/// - Depth-bounded via `Gen.sized` / explicit size params.
module Generators =

    let private pos = Position.None

    // -- Primitives -----------------------------------------------------------

    let private startChars =
        [ for c in 'a' .. 'z' -> c ] @ [ for c in 'A' .. 'Z' -> c ] @ [ '_' ]

    let private restChars =
        startChars @ [ for c in '0' .. '9' -> c ]

    /// A valid Jyro identifier that is not a reserved keyword.
    let genIdentifier : Gen<string> =
        gen {
            let! first = Gen.elements startChars
            let! tailLen = Gen.choose(0, 6)
            let! tail = Gen.listOfLength tailLen (Gen.elements restChars)
            let name = System.String(Array.ofList (first :: tail))
            return name
        }
        |> Gen.filter (fun s -> not (Lexer.reservedKeywords.Contains(s)))

    /// A distinct identifier - used where we need a known name (e.g. a func
    /// defined at the top of a program that expressions can then call).
    let genIdentifiers (count: int) : Gen<string list> =
        Gen.listOfLength count genIdentifier
        |> Gen.filter (fun xs -> List.length (List.distinct xs) = count)

    /// A union-variant name: capitalized, no keyword collisions, not "default".
    let genVariantName : Gen<string> =
        gen {
            let! first = Gen.elements [ for c in 'A' .. 'Z' -> c ]
            let! tailLen = Gen.choose(0, 6)
            let! tail = Gen.listOfLength tailLen (Gen.elements restChars)
            return System.String(Array.ofList (first :: tail))
        }
        |> Gen.filter (fun s ->
            not (Lexer.reservedKeywords.Contains(s)) && s <> "default")

    /// Every `JyroType` case, `AnyType` included - the printer emits a keyword
    /// for all seven, so the round-trip property has to cover all seven.
    let internal genJyroType : Gen<JyroType> =
        Gen.elements [ AnyType; NumberType; StringType; BooleanType; ObjectType; ArrayType; NullType; FunctionType ]

    /// A finite, non-negative decimal - numeric literals now parse to exact
    /// decimals, so generating decimals keeps `parse(print(x)) == x` meaningful.
    /// Negatives are represented as `Unary(Negate, Literal(positive))`.
    let genNumberValue : Gen<decimal> =
        Gen.frequency [
            5, Gen.choose(0, 1000) |> Gen.map decimal
            2, gen {
                let! n = Gen.choose(0, 1000000)
                let! d = Gen.choose(1, 1000)
                return decimal n / decimal d
            }
            1, Gen.elements [ 0m; 1m; 0.5m; 3.14159m ]
        ]

    let genBool : Gen<bool> = Gen.elements [ true; false ]

    /// A string whose printed escape survives round-trip through the lexer.
    let genStringValue : Gen<string> =
        gen {
            let! len = Gen.choose(0, 12)
            let! chars =
                Gen.listOfLength len (
                    Gen.frequency [
                        10, Gen.elements [ for c in 'a' .. 'z' -> c ]
                        5, Gen.elements [ for c in 'A' .. 'Z' -> c ]
                        3, Gen.elements [ for c in '0' .. '9' -> c ]
                        2, Gen.elements [ ' '; '.'; ','; '!'; '?'; '-'; '_' ]
                        1, Gen.elements [ '\n'; '\t'; '"'; '\\' ]
                    ])
            return System.String(Array.ofList chars)
        }

    let private genLiteral : Gen<Expr> =
        Gen.oneof [
            Gen.constant (Literal(JyroNull.Instance, pos))
            genBool |> Gen.map (fun b -> Literal(JyroBoolean.FromBoolean(b), pos))
            genNumberValue |> Gen.map (fun n -> Literal(JyroNumber(n), pos))
            genStringValue |> Gen.map (fun s -> Literal(JyroString(s), pos))
        ]

    let private genIdentifierExpr : Gen<Expr> =
        genIdentifier |> Gen.map (fun n -> Identifier(n, pos))

    // -- Operator sets --------------------------------------------------------

    let private binaryOps =
        [ Add; Subtract; Multiply; Divide; Modulo
          Concat
          Equal; NotEqual
          LessThan; LessThanOrEqual; GreaterThan; GreaterThanOrEqual
          And; Or
          Coalesce ]

    let private assignOps =
        [ Assign; AddAssign; SubtractAssign; MultiplyAssign
          DivideAssign; ModuloAssign; ConcatAssign ]

    // -- Statement-adjacency sanitizer ----------------------------------------
    //
    // Jyro has no statement separator; a statement ending in an expression
    // can "absorb" the next statement if that next statement begins with a
    // postfix-like token (`[`, `(`, `++`, `--`, `.`) or a `-` that becomes a
    // binary subtract. The sanitizer drops any statement that would be
    // absorbed by its predecessor so round-trip holds.

    let private endsInExpression = function
        | ExprStmt _
        | Return(Some _, _)
        | Exit(Some _, _)
        | Fail(Some _, _)
        | VarDecl(_, _, Some _, _)
        | Assignment _
        | Delete _ -> true
        | _ -> false

    /// Defer to the actual printer so this check stays in sync with real
    /// output (e.g. `Lambda` emits a leading `(` because its exprPrec is
    /// below any statement-level context).
    let private isAbsorbableStmt (s: Stmt) : bool =
        match s with
        | ExprStmt(e, _) ->
            let src = Printer.printExpr e
            src.Length > 0
            && (let c = src.[0] in c = '-' || c = '+' || c = '[' || c = '(' || c = '.')
        | _ -> false

    let private sanitizeBlock (stmts: Stmt list) : Stmt list =
        let rec loop acc =
            function
            | [] -> List.rev acc
            | s :: rest ->
                match acc with
                | prev :: _ when endsInExpression prev && isAbsorbableStmt s ->
                    loop acc rest
                | _ -> loop (s :: acc) rest
        loop [] stmts

    // -- Expression generator -------------------------------------------------

    /// Expression generator. `size` bounds recursion depth - halved at each
    /// recursive step so total node count is O(size).
    let rec internal genExpr (size: int) : Gen<Expr> =
        if size <= 0 then
            Gen.oneof [ genLiteral; genIdentifierExpr ]
        else
            let half = size / 2
            Gen.frequency [
                2, genLiteral
                2, genIdentifierExpr
                2, gen {
                    let! l = genExpr half
                    let! op = Gen.elements binaryOps
                    let! r = genExpr half
                    return Binary(l, op, r, pos)
                }
                1, gen {
                    let! e = genExpr (size - 1)
                    return Unary(Not, e, pos)
                }
                1, gen {
                    let! e = genExpr (size - 1)
                    return Unary(Negate, e, pos)
                }
                1, gen {
                    let! c = genExpr half
                    let! t = genExpr half
                    let! e = genExpr half
                    return Ternary(c, t, e, pos)
                }
                1, gen {
                    let! e = genExpr (size - 1)
                    let! t = genJyroType
                    let! neg = genBool
                    return TypeCheck(e, t, neg, pos)
                }
                1, gen {
                    let! n = Gen.choose(0, 3)
                    let! elems = Gen.listOfLength n (genExpr (size / (n + 1)))
                    return ArrayLiteral(elems, pos)
                }
                1, gen {
                    let! n = Gen.choose(0, 3)
                    let! keys =
                        Gen.listOfLength n genIdentifier
                        |> Gen.filter (fun ks -> List.length (List.distinct ks) = n)
                    let! values = Gen.listOfLength n (genExpr (size / (n + 1)))
                    return ObjectLiteral(List.zip keys values, pos)
                }
                1, gen {
                    let! n = Gen.choose(0, 2)
                    let! parameters = genIdentifiers n
                    let! body = genExpr (size - 1)
                    return Lambda(parameters, body, pos)
                }
                1, gen {
                    let! name = genIdentifier
                    let! args = genCallArgs size
                    return Call(name, args, pos)
                }
                1, gen {
                    let! target = genPostfixBase half
                    let! prop = genIdentifier
                    return PropertyAccess(target, prop, pos)
                }
                1, gen {
                    let! target = genPostfixBase half
                    let! idx = genExpr half
                    return IndexAccess(target, idx, pos)
                }
                1, gen {
                    let! e = genPostfixBase (size - 1)
                    let! isInc = genBool
                    let! isPrefix = genBool
                    return IncrementDecrement(e, isInc, isPrefix, pos)
                }
            ]

    /// Only identifiers, property access, and index access can chain further
    /// postfix operations (the parser rejects `f(x).y` at the applyPostfix
    /// site because Call consumes the base). Use this when a postfix op
    /// needs a base that can itself carry more postfix ops.
    and private genPostfixBase (size: int) : Gen<Expr> =
        if size <= 0 then
            genIdentifierExpr
        else
            Gen.frequency [
                3, genIdentifierExpr
                1, gen {
                    let! target = genPostfixBase (size - 1)
                    let! prop = genIdentifier
                    return PropertyAccess(target, prop, pos)
                }
                1, gen {
                    let! target = genPostfixBase (size - 1)
                    let! idx = genExpr (size - 1)
                    return IndexAccess(target, idx, pos)
                }
            ]

    and private genCallArgs (size: int) : Gen<CallArgs> =
        gen {
            let! n = Gen.choose(0, 3)
            let half = max 1 (size / (n + 1))
            let! usesNamed = genBool
            if usesNamed && n > 0 then
                let! names = genIdentifiers n
                let! values = Gen.listOfLength n (genExpr half)
                return NamedArgs(List.zip names values)
            else
                let! values = Gen.listOfLength n (genExpr half)
                return PositionalArgs values
        }

    /// A "simple" expression generator - no TypeCheck nesting, no Lambda at
    /// the root. Used where the parser is more restrictive (e.g. as the
    /// scrutinee of `match`, or the LHS of another TypeCheck).
    let rec internal genSimpleExpr (size: int) : Gen<Expr> =
        genExpr size
        |> Gen.filter (function
            | Lambda _ -> false
            | TypeCheck _ -> false
            | _ -> true)

    // -- Statement generator (non-declaration) --------------------------------

    /// Valid assignment target: identifier, property access, or index access.
    let private genAssignTarget (size: int) : Gen<Expr> =
        Gen.frequency [
            3, genIdentifierExpr
            1, gen {
                let! target = genPostfixBase (max 1 (size - 1))
                let! prop = genIdentifier
                return PropertyAccess(target, prop, pos)
            }
            1, gen {
                let! target = genPostfixBase (max 1 (size - 1))
                let! idx = genExpr (max 1 (size - 1))
                return IndexAccess(target, idx, pos)
            }
        ]

    /// Delete target must be property access or index access (not a bare identifier).
    let private genDeleteTarget (size: int) : Gen<Expr> =
        Gen.frequency [
            1, gen {
                let! target = genPostfixBase (max 1 (size - 1))
                let! prop = genIdentifier
                return PropertyAccess(target, prop, pos)
            }
            1, gen {
                let! target = genPostfixBase (max 1 (size - 1))
                let! idx = genExpr (max 1 (size - 1))
                return IndexAccess(target, idx, pos)
            }
        ]

    /// Generate a non-declaration statement. Declarations (FuncDef/UnionDef)
    /// are only valid at the program level and are emitted by `genProgram`.
    let rec internal genStmt (size: int) : Gen<Stmt> =
        if size <= 0 then
            Gen.oneof [
                genExpr 0 |> Gen.map (fun e -> ExprStmt(e, pos))
                Gen.constant (Break pos)
                Gen.constant (Continue pos)
            ]
        else
            let half = size / 2
            Gen.frequency [
                2, gen {
                    let! name = genIdentifier
                    let! typeHint = Gen.optionOf genJyroType
                    let! init = Gen.optionOf (genExpr half)
                    return VarDecl(name, typeHint, init, pos)
                }
                2, gen {
                    let! target = genAssignTarget half
                    let! op = Gen.elements assignOps
                    let! typeHint =
                        // Type hints are only allowed on plain assignment, not compound.
                        if op = Assign then Gen.optionOf genJyroType
                        else Gen.constant None
                    let! value = genExpr half
                    return Assignment(target, op, value, typeHint, pos)
                }
                2, genExpr size |> Gen.map (fun e -> ExprStmt(e, pos))
                1, gen {
                    let! cond = genExpr half
                    let! thenB = genBlock half
                    let! elifN = Gen.choose(0, 2)
                    let! elifs =
                        Gen.listOfLength elifN (
                            gen {
                                let! c = genExpr half
                                let! b = genBlock half
                                return (c, b)
                            })
                    let! elseB = Gen.optionOf (genBlock half)
                    return If(cond, thenB, elifs, elseB, pos)
                }
                1, gen {
                    let! cond = genExpr half
                    let! body = genBlock half
                    return While(cond, body, pos)
                }
                1, gen {
                    let! name = genIdentifier
                    let! coll = genExpr half
                    let! body = genBlock half
                    return ForEach(name, coll, body, pos)
                }
                1, gen {
                    let! name = genIdentifier
                    let! s = genExpr half
                    let! e = genExpr half
                    let! step = Gen.optionOf (genExpr half)
                    let! dir = Gen.elements [ Ascending; Descending ]
                    let! body = genBlock half
                    return For(name, s, e, step, dir, body, pos)
                }
                1, gen {
                    let! scrutinee = genExpr half
                    let! caseN = Gen.choose(1, 3)
                    let! cases =
                        Gen.listOfLength caseN (
                            gen {
                                let! valN = Gen.choose(1, 2)
                                let! values = Gen.listOfLength valN (genExpr half)
                                let! body = genBlock half
                                return { Values = values; Body = body }
                            })
                    let! defBody = Gen.optionOf (genBlock half)
                    return Switch(scrutinee, cases, defBody, pos)
                }
                1, gen {
                    let! value = Gen.optionOf (genExpr half)
                    return Return(value, pos)
                }
                1, gen {
                    let! value = Gen.optionOf (genExpr half)
                    return Exit(value, pos)
                }
                1, gen {
                    let! value = Gen.optionOf (genExpr half)
                    return Fail(value, pos)
                }
                1, Gen.constant (Break pos)
                1, Gen.constant (Continue pos)
                1, gen {
                    let! target = genDeleteTarget half
                    return Delete(target, pos)
                }
            ]

    and private genBlock (size: int) : Gen<Stmt list> =
        gen {
            let! n = Gen.choose(0, 3)
            let! raw = Gen.listOfLength n (genStmt (max 1 (size / (n + 1))))
            return sanitizeBlock raw
        }

    // -- Program generator ----------------------------------------------------

    /// A leaf generator: parameter defaults are kept to literals, so there is
    /// no recursion for a size to bound.
    let private genFuncParam () : Gen<FuncParam> =
        gen {
            let! name = genIdentifier
            let! typeHint = Gen.optionOf genJyroType
            let! defaultValue =
                // Default values are parsed as literal-or-bare-expression; keep simple.
                Gen.optionOf genLiteral
            return { Name = name; TypeHint = typeHint; DefaultValue = defaultValue }
        }

    let private genFuncDef (size: int) : Gen<Stmt> =
        gen {
            let! name = genIdentifier
            let! pn = Gen.choose(0, 3)
            let! parameters =
                Gen.listOfLength pn (genFuncParam ())
                |> Gen.filter (fun ps ->
                    let names = ps |> List.map (fun p -> p.Name)
                    List.length (List.distinct names) = List.length names)
            let! body = genBlock (max 1 (size / 2))
            return FuncDef(name, parameters, body, pos)
        }

    /// A UnionVariant of `name` and `fields`, built through reflection so that the generator
    /// does not depend on the record's other fields: a union variant carries its own position
    /// (150.8.5, JYRO-ADR-138), here `Position.None` like every other generated position.
    let internal mkVariant (name: string) (fields: 'F) : UnionVariant =
        let t = typeof<UnionVariant>
        let flags = System.Reflection.BindingFlags.Public ||| System.Reflection.BindingFlags.NonPublic
        let values =
            Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(t, flags)
            |> Array.map (fun f ->
                match f.Name with
                | "Name" -> box name
                | "Fields" -> box fields
                | _ when f.PropertyType = typeof<Position> -> box Position.None
                | _ when f.PropertyType.IsValueType -> System.Activator.CreateInstance f.PropertyType
                | _ -> null)
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(t, values, flags) :?> UnionVariant

    let private genVariant : Gen<UnionVariant> =
        gen {
            let! name = genVariantName
            let! fn = Gen.choose(0, 3)
            let! fields =
                Gen.listOfLength fn (
                    gen {
                        let! fname = genIdentifier
                        let! typeHint = Gen.optionOf genJyroType
                        return (fname, typeHint)
                    })
                |> Gen.filter (fun fs ->
                    let names = fs |> List.map fst
                    List.length (List.distinct names) = List.length names)
            return mkVariant name fields
        }

    /// Also a leaf: a variant carries field names and type hints, never an
    /// expression.
    let private genUnionDef () : Gen<Stmt> =
        gen {
            let! name = genIdentifier
            let! vn = Gen.choose(1, 3)
            let! variants =
                Gen.listOfLength vn genVariant
                |> Gen.filter (fun vs ->
                    let names = vs |> List.map (fun v -> v.Name)
                    List.length (List.distinct names) = List.length names)
            return UnionDef(name, variants, pos)
        }

    /// Top-level statement: everything `genStmt` produces plus declarations.
    let internal genTopStmt (size: int) : Gen<Stmt> =
        Gen.frequency [
            5, genStmt size
            1, genFuncDef size
            1, genUnionDef ()
        ]

    let internal genProgram : Gen<Program> =
        Gen.sized (fun s ->
            gen {
                let! n = Gen.choose(0, min 6 (s / 2 + 1))
                let! raw = Gen.listOfLength n (genTopStmt (max 1 (s / 2)))
                return { Statements = sanitizeBlock raw }
            })

    // -- Arbitrary registrations ---------------------------------------------

    /// Wrapper type so we can register a custom Arbitrary<Program> without
    /// clashing with any default instance.
    type GenProgram = internal { ProgramValue: Program }

    /// Wrapper for a single expression (any precedence level).
    type GenExpr = internal { ExprValue: Expr }

    /// Wrapper for a single statement (non-declaration).
    type GenStmt = internal { StmtValue: Stmt }

    /// Wrapper for a valid identifier.
    type GenIdent = { IdentValue: string }

    /// Wrapper for a generated non-negative finite number.
    type GenNumber = { NumberValue: decimal }

    /// Wrapper for a generated string (printable + a sprinkle of escapes).
    type GenString = { StringValue: string }

    type AstArbitraries =
        static member Program() : Arbitrary<GenProgram> =
            let g = genProgram |> Gen.map (fun p -> { ProgramValue = p })
            let shr (w: GenProgram) =
                Shrinkers.shrinkProgram w.ProgramValue
                |> Seq.map (fun p -> { ProgramValue = { Statements = sanitizeBlock p.Statements } })
            Arb.fromGenShrink(g, shr)

        static member Expr() : Arbitrary<GenExpr> =
            let g =
                Gen.sized (fun s -> genExpr s)
                |> Gen.map (fun e -> { ExprValue = e })
            let shr (w: GenExpr) =
                Shrinkers.shrinkExpr w.ExprValue
                |> Seq.map (fun e -> { ExprValue = e })
            Arb.fromGenShrink(g, shr)

        static member Stmt() : Arbitrary<GenStmt> =
            let g =
                Gen.sized (fun s -> genStmt s)
                |> Gen.map (fun s -> { StmtValue = s })
            let shr (w: GenStmt) =
                Shrinkers.shrinkStmt w.StmtValue
                |> Seq.map (fun s -> { StmtValue = s })
            Arb.fromGenShrink(g, shr)

        static member Ident() : Arbitrary<GenIdent> =
            genIdentifier
            |> Gen.map (fun n -> { IdentValue = n })
            |> Arb.fromGen

        static member Number() : Arbitrary<GenNumber> =
            genNumberValue
            |> Gen.map (fun n -> { NumberValue = n })
            |> Arb.fromGen

        static member JyroStr() : Arbitrary<GenString> =
            genStringValue
            |> Gen.map (fun s -> { StringValue = s })
            |> Arb.fromGen

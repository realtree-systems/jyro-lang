namespace RealTree.Jyro.Compilation

open RealTree.Jyro
open RealTree.Jyro.Ast


/// Validates that a parsed program is a valid *library* - i.e. it contains
/// only function and union declarations at the top level. Library function
/// bodies are still subjected to the normal `Validator.validate` rules.
module internal LibraryValidator =
    let private isDeclaration (stmt: Stmt) =
        match stmt with
        | FuncDef _
        | UnionDef _ -> true
        | VarDecl _
        | Assignment _
        | If _
        | While _
        | ForEach _
        | For _
        | Switch _
        | Return _
        | Fail _
        | Exit _
        | Break _
        | Continue _
        | ExprStmt _
        | Match _
        | Delete _ -> false

    /// The declarations a library keeps: its `func` and `union` statements.
    let declarations (program: Program) : Stmt list =
        program.Statements |> List.filter isDeclaration

    /// Validate a program as a library. Reports `LibraryStatementForbidden`
    /// for every top-level statement other than `FuncDef` or `UnionDef`, and
    /// validates the declarations the library keeps with `Validator.validate`,
    /// so library authors get the same diagnostics they would for an ordinary
    /// script. Both are Validate checks of one source text, so neither stops
    /// the other: a forbidden statement does not hide an error in a function
    /// body (spec 130.4, JYRO-ADR-115 point 4, JYRO-DEF-511). The forbidden
    /// statements are not validated themselves; they are refused whole.
    ///
    /// `unions` are the unions of the compilation the library is validated
    /// in: those of the source texts before it, which its union and variant
    /// names are checked against, and those of every source text, which its
    /// `match` cases may name (spec 160.6, JYRO-ADR-121).
    let rec validateLibraryIn (unions: Validator.UnionScope) (program: Program) : JyroResult<Program> =
        fst (validateLibraryText unions false program)

    /// `validateLibraryIn`, and with `deferForeignMatches` a library parsed
    /// ahead of any compilation: a `match` naming a variant the library does
    /// not declare is checked in each compilation instead (JYRO-ADR-139).
    /// Returns the result and whether the library holds such a match
    /// (JYRO-ADR-140 point 1).
    and private validateLibraryText (unions: Validator.UnionScope) (deferForeignMatches: bool) (program: Program) : JyroResult<Program> * bool =
        let topLevelErrors =
            program.Statements
            |> List.choose (fun stmt ->
                if isDeclaration stmt then None
                else
                    // No argument: the place is the statement, and a phrase
                    // for its kind, such as `variable declaration`, would be
                    // words the implementation chose (spec 130.6.2).
                    let loc = stmt.Position.Location
                    Some (DiagnosticMessage.Of(MessageCode.LibraryStatementForbidden, location = loc)))
        let kept = { program with Statements = declarations program }
        let validated, holdsDeferredMatch = Validator.validateText RegisteredNames.None unions deferForeignMatches kept
        let messages = Validator.inSourceOrder [ topLevelErrors @ List.ofSeq validated.Messages ]
        let result =
            if messages |> List.exists (fun m -> m.Severity = MessageSeverity.Error) then
                JyroResult<Program>.Failure(messages)
            else
                JyroResult<Program>.Success(kept, messages)
        result, holdsDeferredMatch

    /// Validate a program as a library parsed ahead of any compilation
    /// (`JyroLibrary.Parse`): only the checks that read the library alone
    /// (spec 160.6, JYRO-REQ-1698). Its function, union and variant names
    /// against the other source texts, the matches that name a variant it
    /// does not declare, and its calls are checked by each compilation it is
    /// added to (JYRO-ADR-139). Returns the result and whether the library
    /// holds a match left to the compilation, which each compilation reads
    /// to choose its checks (JYRO-ADR-140 point 1).
    let validateParsedLibrary (program: Program) : JyroResult<Program> * bool =
        validateLibraryText Validator.UnionScope.None true program

    /// `validateParsedLibrary`'s result alone.
    let validateLibrary (program: Program) : JyroResult<Program> =
        fst (validateParsedLibrary program)

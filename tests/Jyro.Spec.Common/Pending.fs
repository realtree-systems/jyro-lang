namespace RealTree.Jyro.Spec

open System
open System.Reflection
open System.Runtime.ExceptionServices
open RealTree.Jyro

/// Accessors for the members of 160.7 that the book declares but the build may
/// not have yet. Tests reach them only through here, by reflection or by
/// number, so that every test project compiles now (FLOORS rule 4.1.7). Each
/// accessor fails the calling test when the member is missing; once the member
/// exists they simply work.
///
/// A missing member is reported for the requirement that DEFINES it
/// (floors-process 4.2.5), in the line
///
///     FLOORS-BLOCKED <ref>: member <m> is not in this build
///
/// so that `floors verify` records the calling test's requirement as blocked
/// by <ref>, not as fail. When every Req tag of the calling test is <ref>
/// itself (a test of the defining requirement), the member's absence is that
/// requirement's own failure, and the message is the plain
/// "<ref> member <m> is not in this build", with no FLOORS-BLOCKED line.
module Pending =

    let private isTestAttribute (t: Type) : bool =
        let rec walk (t: Type) =
            not (isNull t) && (t.FullName = "Xunit.FactAttribute" || walk t.BaseType)
        walk t

    let private reqTagsOf (data: Collections.Generic.IList<CustomAttributeData>) : string list =
        [ for a in data do
            if a.AttributeType.FullName = "Xunit.TraitAttribute"
               && a.ConstructorArguments.Count = 2
               && (a.ConstructorArguments.[0].Value :?> string) = "requirement" then
                yield (a.ConstructorArguments.[1].Value :?> string) ]

    /// The Req tags of the test method on the current stack (its own and its
    /// declaring types'), or None when no test method is found on the stack.
    let private callingTestTags () : string list option =
        let frames = Diagnostics.StackTrace(false).GetFrames()
        frames
        |> Array.tryPick (fun f ->
            match f.GetMethod() with
            | null -> None
            | m ->
                let data = try m.GetCustomAttributesData() with _ -> [||] :> _
                if data |> Seq.exists (fun a -> isTestAttribute a.AttributeType) then
                    let rec typeTags (t: Type) =
                        if isNull t then [] else reqTagsOf (t.GetCustomAttributesData()) @ typeTags t.DeclaringType
                    Some (reqTagsOf data @ typeTags m.DeclaringType)
                else None)

    /// The message for the member `memberName` of requirement `ref` missing
    /// from the build: a FLOORS-BLOCKED line for `ref`, unless every Req tag
    /// of the calling test is `ref`.
    let notInBuildMessage (ref: string) (memberName: string) : string =
        match callingTestTags () with
        | Some tags when not tags.IsEmpty && tags |> List.forall ((=) ref) ->
            sprintf "%s member %s is not in this build" ref memberName
        | _ ->
            sprintf "FLOORS-BLOCKED %s: member %s is not in this build" ref memberName

    /// Fails the calling test: the member `memberName`, which requirement
    /// `ref` defines, is not in this build (see `notInBuildMessage`).
    let notInBuild (ref: string) (memberName: string) : 'T =
        raise (Xunit.Sdk.XunitException(notInBuildMessage ref memberName))

    let private missing (clause: string) (memberName: string) : 'T = notInBuild clause memberName

    /// Fails the calling test because what requirement `ref` requires, and the test needs in
    /// order to show another requirement, did not happen (`what` says what, with the
    /// diagnostics). As `notInBuild`: a FLOORS-BLOCKED line for `ref`, unless every Req tag
    /// of the calling test is `ref`, when it is that requirement's own failure.
    let requires (ref: string) (what: string) : 'T =
        match callingTestTags () with
        | Some tags when not tags.IsEmpty && tags |> List.forall ((=) ref) ->
            raise (Xunit.Sdk.XunitException(sprintf "%s: %s" ref what))
        | _ -> raise (Xunit.Sdk.XunitException(sprintf "FLOORS-BLOCKED %s: %s" ref what))

    /// JYRO-REQ-1887 `JyroExecutionResult.ExitMessage : string?`, as an option
    /// (`None` for a null exit message). The member exists only on the execution result
    /// that `JyroRunner.Execute()` returns: for any other result (a compile failure, a
    /// compile result) there is no exit message, and this is `None`.
    let exitMessage (result: JyroResult<'T>) : string option =
        match box result with
        | :? JyroExecutionResult as e -> Option.ofObj e.ExitMessage
        | _ -> None

    /// JYRO-REQ-2113 `JyroRunner.WithMaxRegexTime(TimeSpan) : JyroRunner`. An
    /// exception the member throws (`ArgumentOutOfRangeException`) reaches the
    /// caller as itself, so `Assert.Throws` works.
    let withRegexTimeout (runner: JyroRunner) (timeout: TimeSpan) : JyroRunner =
        runner.WithMaxRegexTime(timeout)

    /// The type `RealTree.Jyro.MessageStage` of 160.7.4, found in the
    /// assemblies of the public API.
    let messageStageType () : Type =
        let found =
            [ typeof<DiagnosticMessage>.Assembly; typeof<JyroRunner>.Assembly ]
            |> List.distinct
            |> List.tryPick (fun a -> Option.ofObj (a.GetType("RealTree.Jyro.MessageStage")))
        match found with
        | Some t -> t
        | None -> missing "JYRO-REQ-1615" "enum RealTree.Jyro.MessageStage"

    /// 160.7.5 `DiagnosticMessage.Stage`, returned as the enum member's name
    /// ("Deserialize", "Parse", "Validate", "Link", "Compile" or "Execute").
    let stageOf (message: DiagnosticMessage) : string =
        let p = typeof<DiagnosticMessage>.GetProperty("Stage", BindingFlags.Public ||| BindingFlags.Instance)
        if isNull p then missing "JYRO-REQ-2105" "DiagnosticMessage.Stage"
        else
            match p.GetValue(message) with
            | null -> Values.fail "160.7.5 DiagnosticMessage.Stage returned null"
            | v -> v.ToString()

    /// 160.7.6 `MessageCode.MatchSubjectNotVariant`: the value 5403 cast to
    /// `MessageCode`. Comparing a diagnostic's code with it works whether or not
    /// the enum member is declared yet; to pin that the member exists, call
    /// `requireMatchSubjectNotVariant ()`.
    let matchSubjectNotVariant : MessageCode = enum<MessageCode> 5403

    /// Fails unless `MessageCode` declares a member named
    /// `MatchSubjectNotVariant` with the value 5403 (160.7.6).
    let requireMatchSubjectNotVariant () : unit =
        let name = Enum.GetName(typeof<MessageCode>, matchSubjectNotVariant)
        if name <> "MatchSubjectNotVariant" then
            missing "JYRO-REQ-1617" "MessageCode.MatchSubjectNotVariant = 5403"

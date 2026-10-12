namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests

/// Coverage inventory: asserts every stdlib function is either
/// property-tested (listed in `coveredFunctions` below) or explicitly
/// opted-out (listed in `acknowledgedUncovered`). A function that's
/// neither fails the test - catches new stdlib additions that land
/// without coverage.
///
/// The breakdown is echoed via `ITestOutputHelper` so CI logs carry a
/// quick inventory of stdlib test surface.
module StdlibCoverage =

    /// Stdlib functions that have at least one property/fact in
    /// `StdlibLaws`. Keep this list in lockstep with tests: adding a
    /// property → add the name here; removing the last property for a
    /// function → remove it.
    let private coveredFunctions : Set<string> =
        Set.ofList [
            // Array / iteration
            "Length"
            "Reverse"
            "Sort"
            "SortBy"
            "Concatenate"
            "Where"
            "Map"
            "Reduce"
            "All"
            "Any"
            "Find"
            "Distinct"
            "Append"
            "Prepend"
            "Slice"
            "First"
            "Last"
            "IndexOf"
            // Array manipulation
            "Flatten"
            "FlattenOnce"
            "GroupBy"
            "Insert"
            "Range"
            "RemoveAt"
            "RemoveFirst"
            "RemoveLast"
            "Skip"
            // Object
            "Keys"
            "Values"
            "HasProperty"
            "Merge"
            "Clone"
            // Strings
            "Contains"
            "StartsWith"
            "EndsWith"
            "ToUpper"
            "ToLower"
            "Trim"
            "Substring"
            "Split"
            "Join"
            // Math
            "Absolute"
            "Average"
            "Ceiling"
            "Clamp"
            "Floor"
            "Log"
            "Max"
            "Median"
            "Min"
            "Mode"
            "Power"
            "Round"
            "SquareRoot"
            "Sum"
            // By-field query helpers
            "AllByField"
            "AnyByField"
            "CountIf"
            "FindByField"
            "Project"
            "Select"
            "SelectMany"
            "SortByField"
            "WhereByField"
            // Object helpers
            "Coalesce"
            "Diff"
            "Omit"
            // Encoding
            "Base64Decode"
            "Base64Encode"
            // String helpers (padding + replace)
            "PadLeft"
            "PadRight"
            "Replace"
            // Date / time
            "DateAdd"
            "DateDiff"
            "DatePart"
            "FormatDate"
            "ParseDate"
            // Regex
            "RegexMatch"
            "RegexMatchAll"
            "RegexMatchDetail"
            "RegexTest"
            // Non-deterministic (tested via invariants: range, uniqueness, format)
            "NewGuid"
            "Now"
            "RandomChoice"
            "RandomInt"
            "RandomString"
            "Sleep"
            "Today"
            // Schema validation
            "ValidateRequired"
            "ValidateSchema"
            // Introspection / JSON
            "TypeOf"
            "ToJson"
            "FromJson"
            // Conversion and iteration, covered directly in StdlibSemantics
            // after their opt-out reasons turned out to be false.
            "Each"
            "ToBoolean"
            "ToNumber"
            "ToString"
        ]

    /// Functions we knowingly skip in property tests. Each entry carries
    /// a one-line reason. If the reason no longer holds, move the name
    /// into `coveredFunctions` and write a property.
    /// Three entries were removed from this map because their reasons were
    /// false, not because coverage arrived by another route. `Each` was
    /// "side-effecting twin of Map; covered indirectly" - but `EachFunction`
    /// is a separate implementation with its own callback dispatch, so no test
    /// of `Map` enters it. `ToNumber` was "covered by numeric literal +
    /// type-check tests" - nothing called it at all. `ToBoolean` was "covered
    /// by Truthiness property tests", which cover the truthiness *rule* while
    /// never entering `ToBooleanFunction.Execute`.
    ///
    /// An opt-out list is a claim about coverage, so a wrong reason here is
    /// worse than a missing entry: it reports a posture the repository does not
    /// have. All three are now exercised directly in `StdlibSemantics`.
    /// `ToString` was the fourth, and outlived the pass that corrected the
    /// other three. Its reason - "covered indirectly via Truthiness and type
    /// tests" - was false, not stale: neither file names `ToString`, and
    /// neither can reach `ToStringFunction.Execute`. It is now exercised
    /// directly in `StdlibSemantics` like its siblings, so the list is empty.
    ///
    /// Empty is the intended state. An entry here is a claim about coverage
    /// that no assertion checks, so it is the one place in this file where a
    /// sentence can be wrong indefinitely without any test noticing.
    let private acknowledgedUncovered : Map<string, string> = Map.empty

    /// Functions we have NOT yet property-tested and have NO alternative
    /// coverage for. Tracked here so the inventory surfaces them as a
    /// clear TODO rather than an opt-out. Adding property tests →
    /// promote to `coveredFunctions`. Adding an opt-out reason →
    /// promote to `acknowledgedUncovered`.
    /// No pending entries right now. If a new stdlib function lands
    /// without placement, the `every registered stdlib function is
    /// classified` ratchet surfaces it on the next CI run.
    let private pendingCoverage : Set<string> = Set.empty

    type StdlibCoverageFixture(output: Xunit.Abstractions.ITestOutputHelper) =

        [<Fact>]
        member _.``every registered stdlib function is classified`` () =
            // Ratchet: every stdlib function must live in exactly one of
            // `coveredFunctions`, `acknowledgedUncovered`, or
            // `pendingCoverage`. A new stdlib addition that isn't placed
            // explicitly fails this test.
            let registered =
                StdlibRegistry.getAll () |> List.ofSeq
                |> List.map (fun f -> f.Name)
                |> Set.ofList
            let classified name =
                coveredFunctions.Contains(name)
                || acknowledgedUncovered.ContainsKey(name)
                || pendingCoverage.Contains(name)
            let unclassified = registered |> Set.filter (not << classified)
            Assert.True(
                Set.isEmpty unclassified,
                sprintf
                    "Unclassified stdlib functions:\n  %s\nPlace each in `coveredFunctions`, `acknowledgedUncovered`, or `pendingCoverage`."
                    (String.concat "\n  " (Set.toList unclassified)))

        [<Fact>]
        member _.``no stale entries in coverage lists`` () =
            let registered =
                StdlibRegistry.getAll () |> List.ofSeq
                |> List.map (fun f -> f.Name)
                |> Set.ofList
            let staleCovered = coveredFunctions - registered
            let staleAck =
                acknowledgedUncovered
                |> Map.toList
                |> List.map fst
                |> Set.ofList
                |> fun s -> s - registered
            let stalePending = pendingCoverage - registered
            let stale =
                Set.unionMany [ staleCovered; staleAck; stalePending ]
            Assert.True(
                Set.isEmpty stale,
                sprintf
                    "Coverage lists reference names that are no longer in the stdlib:\n  %s"
                    (String.concat "\n  " (Set.toList stale)))

        [<Fact>]
        member _.``stdlib coverage inventory`` () =
            let registered =
                StdlibRegistry.getAll () |> List.ofSeq
                |> List.map (fun f -> f.Name)
                |> List.sort
            let total = List.length registered
            let covered =
                registered |> List.filter (fun n -> coveredFunctions.Contains(n))
            let acknowledged =
                registered |> List.filter (fun n -> acknowledgedUncovered.ContainsKey(n))
            let pending =
                registered |> List.filter (fun n -> pendingCoverage.Contains(n))

            let pct n = 100.0 * float n / float total
            output.WriteLine(sprintf "Stdlib coverage (%d functions)" total)
            output.WriteLine(String.replicate 54 "-")
            output.WriteLine(
                sprintf
                    "  %-22s %4d (%5.1f%%)"
                    "Property-tested"
                    (List.length covered)
                    (pct (List.length covered)))
            output.WriteLine(
                sprintf
                    "  %-22s %4d (%5.1f%%)"
                    "Acknowledged opt-out"
                    (List.length acknowledged)
                    (pct (List.length acknowledged)))
            output.WriteLine(
                sprintf
                    "  %-22s %4d (%5.1f%%)"
                    "Pending coverage"
                    (List.length pending)
                    (pct (List.length pending)))
            output.WriteLine("")
            output.WriteLine("By function:")
            output.WriteLine(sprintf "  %-24s %s" "Function" "Status")
            output.WriteLine(String.replicate 54 "-")
            for name in registered do
                let status =
                    if coveredFunctions.Contains(name) then "tested"
                    elif acknowledgedUncovered.ContainsKey(name) then
                        sprintf "opt-out (%s)" (acknowledgedUncovered.[name])
                    elif pendingCoverage.Contains(name) then "pending"
                    else "UNCLASSIFIED"
                output.WriteLine(sprintf "  %-24s %s" name status)

            // Sanity: every registered function lives in exactly one list.
            let classified =
                registered
                |> List.filter (fun n ->
                    coveredFunctions.Contains(n)
                    || acknowledgedUncovered.ContainsKey(n)
                    || pendingCoverage.Contains(n))
                |> List.length
            Assert.Equal(total, classified)

    // -- Parameter names are caller-visible API -------------------------------
    // The linker resolves a named-argument call against `Signature.Parameters`,
    // so a parameter name is not private detail. Two signature helpers used to
    // hardcode theirs - `value` for unary, `left`/`right` for binary - which
    // gave 58 of the 97 functions meaningless names: `Contains(text:, search:)`
    // read well while its sibling had to be called `IndexOf(left:, right:)`.

    let private allSignatures =
        StdlibRegistry.getAll ()
        |> Seq.map (fun f -> f.Name, f.Signature.Parameters |> List.ofSeq |> List.map (fun p -> p.Name))
        |> Seq.toList

    [<Fact>]
    let ``no stdlib parameter is named for its position`` () =
        // `left` and `right` describe where an argument sits, not what it is.
        let positional =
            allSignatures
            |> List.filter (fun (_, names) -> names |> List.exists (fun n -> n = "left" || n = "right"))
        Assert.True(positional.IsEmpty, sprintf "positionally-named parameters: %A" positional)

    [<Fact>]
    let ``every stdlib parameter name is a legal identifier`` () =
        // A named-argument label is an identifier, so a parameter named for a
        // reserved keyword cannot be used - which rules out the otherwise
        // obvious `array`, `object`, `string` and `number`. The oracle is the
        // language's own: a name is legal when a script can declare a variable
        // of that name and read it back, which uses the identifier tables of
        // Unicode 15.0.0 (JYRO-REQ-9), not the letter categories of the runtime.
        let isIdentifier (n: string) =
            not (System.String.IsNullOrWhiteSpace(n))
            && (let r =
                    Runner.compileThenRun
                        (JyroCompiler().WithSource(sprintf "var %s = 1\nOutput.v = %s" n n))
                        (fun r -> r.WithInput(JyroObject() :> JyroValue))
                r.IsSuccess && r.TryValue.Value.GetProperty("v").ToStringValue() = "1")
        let illegal =
            allSignatures
            |> List.collect (fun (fn, names) ->
                names
                |> List.filter (fun n -> not (isIdentifier n))
                |> List.map (fun n -> fn, n))
        Assert.True(illegal.IsEmpty, sprintf "unusable parameter names: %A" illegal)

    [<Fact>]
    let ``no stdlib signature repeats a parameter name`` () =
        let dupes =
            allSignatures
            |> List.filter (fun (_, names) -> List.length (List.distinct names) <> List.length names)
        Assert.True(dupes.IsEmpty, sprintf "duplicate parameter names: %A" dupes)

    [<Fact>]
    let ``a binary stdlib function is callable by its declared names`` () =
        // The end-to-end property: the names in the signature are the names a
        // script may use, in any order.
        let r =
            Runner.compileThenRun
                (JyroCompiler().WithStdlib().WithSource("Output.v = Join(separator: '-', values: ['a','b'])"))
                (fun r -> r.WithInput(JyroObject() :> JyroValue))
        Assert.True(r.IsSuccess, sprintf "%A" (List.ofSeq r.Messages |> List.ofSeq |> List.map (fun m -> m.Message)))
        Assert.Equal("a-b", (r.TryValue.Value.GetProperty("v")).ToStringValue())

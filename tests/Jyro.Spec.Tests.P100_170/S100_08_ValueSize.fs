/// 100.8.4 (an operation that unfolds a shared value allocates in proportion
/// to its unfolded size), 100.8.6 (the size of a value), 100.8.7 (no operation
/// makes a string, array or object above the maximum value size M =
/// 16,777,216), 100.8.8 (the check comes before the allocation), 100.8.9 (a
/// cyclic or too-deep value fails as too deep) and 100.8.11 (the size check
/// comes where the value would first exceed M).
///
/// The tests that hold values of size about M are in the collection
/// LargeValues (ValueSize.fs): one at a time, after every other collection,
/// under the test host's GC heap limit. Those where a build that does not
/// conform would walk 2^30 paths or a cycle run in a child process. Every run
/// sets all four limits explicitly (FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S100_08_ValueSize

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.ValueSize

let private one : JyroValue = JyroNumber(1) :> JyroValue
let private nul : JyroValue = JyroNull.Instance :> JyroValue

/// The number `name` of the Output of a run that must succeed.
let private numberOf (name: string) (r: JyroResult<JyroValue>) : decimal = ok r |> Values.prop name |> Values.asDecimal

/// A child heap limit of 2 GB, below the test host's.
let private childEnv = [ "DOTNET_GCHeapHardLimit", "80000000" ]

/// Run a value-size scenario in a child process (killed after 30 s).
let private child (scenario: string) : Child.ChildOutcome = Child.runWith 30_000 childEnv ("vs|" + scenario)

/// Assert a child's outcome carries ValueNestingLimitExceeded and not ValueSizeLimitExceeded.
let private childFailsForDepth (o: Child.ChildOutcome) : unit =
    Child.expectClean o
    if o.Ok || not (List.contains (string MessageCode.ValueNestingLimitExceeded) o.Codes)
       || List.contains valueSizeName o.Codes then
        Values.fail ("Expected ValueNestingLimitExceeded and not ValueSizeLimitExceeded: " + o.Describe)

// ================================================================================================
// 100.8.4 An operation that unfolds a shared value allocates in proportion to its unfolded size
// ================================================================================================

/// Run `source`, which calls Mark() twice, and give the bytes the executing
/// thread allocated between the two marks.
let private between (source: string) : int64 =
    let mark = AllocationMark(true)
    let r = Script.runWith { pinned with Functions = [ mark.AsFunction ] } source
    ok r |> ignore
    match mark.Marks with
    | [ a; b ] -> b - a
    | ms -> Values.fail (sprintf "Expected two marks, got %A" ms)

let unfoldingOperations : obj[] seq = [ "Clone"; "ToJson"; "ToString"; "Flatten" ] |> Seq.map (fun o -> [| box o |])

[<Theory>]
[<MemberData(nameof unfoldingOperations)>]
[<Trait("requirement", "JYRO-REQ-2049")>]
let ``UnfoldingOperation_OnTheSharedTree_SucceedsAndAllocatesLinearlyInItsUnfoldedSize`` (op: string) =
    // The tree after k doublings has k + 1 arrays and size 3 x 2^k - 1 (100.8.6):
    // each step of k doubles the unfolded size, so the allocation may at most
    // (about) double; 2.5 leaves room for noise and none for a quadratic walk.
    let bytes =
        [ for k in 16 .. 19 ->
            k, between (lines (treeDoubling "a" k @ [ "Mark()"; sprintf "var r = %s(a)" op; "Mark()"; "Output.n = Length(r)" ])) ]
    for ((k1, b1), (k2, b2)) in List.pairwise bytes do
        Assert.True(float b2 <= 2.5 * float b1 + 65536.,
                    sprintf "%s: k = %d allocated %d bytes, k = %d allocated %d (more than 2.5 times)" op k1 b1 k2 b2)

[<Theory>]
[<InlineData("Concatenate")>]
[<InlineData("Ampersand")>]
[<Trait("requirement", "JYRO-REQ-2049")>]
let ``OperandGivenTwice_AllocatesInProportionToTwiceItsSize`` (op: string) =
    let run k =
        match op with
        | "Concatenate" -> between (lines (arrayDoubling k @ [ "Mark()"; "var b = Concatenate(a, a)"; "Mark()" ]))
        | _ -> between (lines (stringDoubling k @ [ "Mark()"; "var t = s & s"; "Mark()" ]))
    let small, big = run 18, run 19
    Assert.True(float big <= 2.5 * float small + 65536.,
                sprintf "%s(x, x): %d bytes for 2^18 units, %d for 2^19 (more than 2.5 times)" op small big)

// ================================================================================================
// 100.8.6 The size of a value
// ================================================================================================

[<Collection("LargeValues")>]
type SizeOfAValue() =

    /// Accept: ToObjectValue and WriteJson (to Stream.Null). Refuse: all five
    /// members of 160.8.1 that every value has.
    static member private Accepts (what: string) (v: JyroValue) =
        for m in [ "ToObjectValue"; "WriteJson" ] do
            try convert m v |> ignore
            with e -> Values.fail (sprintf "%s of %s (size M) threw %s (codes %A): %s" m what (e.GetType().Name) (codesOfExn e) e.Message)

    static member private Refuses (what: string) (v: JyroValue) =
        for m in valueMembers do throwsForSize (sprintf "%s of %s" m what) (fun () -> convert m v)

    [<Theory>]
    [<InlineData("null")>]
    [<InlineData("boolean")>]
    [<InlineData("number")>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``JYRO-REQ-2167 NullBooleanNumber_EachCount1_AnArrayOfMMinus1Converts_AnArrayOfMThrows``(kind: string) =
        let first, second =
            match kind with
            | "null" -> nul, nul
            | "boolean" -> JyroBoolean.True :> JyroValue, JyroBoolean.False :> JyroValue
            // A number counts 1 whatever its representation and digits: 28 exact digits and a double.
            | _ -> JyroNumber(1234567890123456789012345678m) :> JyroValue, JyroNumber(0.1) :> JyroValue
        // M - 3 nulls then the two values: 1 + (M - 1) = M when each value counts 1.
        // A count that grew with a number's digits or with a boolean's text
        // would pass M here.
        let a = nulls (M - 3)
        a.Add first
        a.Add second
        SizeOfAValue.Accepts (sprintf "M - 3 nulls and two %s values (size M)" kind) a
        a.Add first
        SizeOfAValue.Refuses (sprintf "M - 3 nulls and three %s values (size M + 1)" kind) a

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``JYRO-REQ-2167 String_Counts1PlusItsLength_MMinus1CodeUnitsConverts_MThrows``() =
        let s = chars (M - 1) 'x'
        for m in valueMembers do convert m s |> ignore
        SizeOfAValue.Refuses "a string of M code units (size M + 1)" (chars M 'x')

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``JYRO-REQ-2167 Object_Counts1PlusKeyLengthPlusValueSize``() =
        // One key of M - 2 code units and a null value: 1 + (M - 2) + 1 = M.
        let atM = objOf [ String('k', M - 2), nul ]
        SizeOfAValue.Accepts "an object with a key of M - 2 (size M)" atM
        atM.ToDictionary() |> ignore
        let above = objOf [ String('k', M - 1), nul ]
        SizeOfAValue.Refuses "an object with a key of M - 1 (size M + 1)" above
        throwsForSize "ToDictionary of an object with a key of M - 1" (fun () -> above.ToDictionary())
        // {k: s}: 1 + 1 + (1 + Length(s)) = 3 + Length(s).
        SizeOfAValue.Accepts "{k: s} with s of M - 3 (size M)" (objOf [ "k", chars (M - 3) 'x' ])
        SizeOfAValue.Refuses "{k: s} with s of M - 2 (size M + 1)" (objOf [ "k", chars (M - 2) 'x' ])

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``JYRO-REQ-2167 SharedContainer_IsCountedEachTimeTheWalkReachesIt``() =
        // [b, b, null] with b of M/2 - 2 nulls: 1 + 2 x (M/2 - 1) + 1 = M, from about M/2 nodes.
        let b = nulls (M / 2 - 2)
        let a = JyroArray()
        a.Add b
        a.Add b
        a.Add nul
        SizeOfAValue.Accepts "[b, b, null] with b of M/2 - 2 nulls (size M)" a
        // b of M/2 - 1 nulls: 1 + 2 x (M/2) + 1 = M + 2.
        b.Add nul
        SizeOfAValue.Refuses "[b, b, null] with b of M/2 - 1 nulls (size M + 2)" a

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    [<Trait("requirement", "JYRO-REQ-1630")>]
    member _.``JYRO-REQ-2167 SharedTree_Of22DoublingsConverts_Of23DoublingsThrows``() =
        // 3 x 2^22 - 1 = 12,582,911 and 3 x 2^23 - 1 = 25,165,823 (the book's figures).
        Assert.Equal(12_582_911L, treeSize 22)
        Assert.Equal(25_165_823L, treeSize 23)
        SizeOfAValue.Accepts "the shared tree of 22 doublings" (hostTree 22)
        SizeOfAValue.Refuses "the shared tree of 23 doublings" (hostTree 23)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``JYRO-REQ-2167 FunctionValue_Counts1_AndTheValuesItCapturesAreNotCounted``() =
        // Append(Input.a, f) has 1 + (M - 2) + 1 = M when f counts 1, although f
        // captures Input, whose size is M + 1 on its own.
        let source = lines [ "var f = x => Input"; "Output.n = Length(Clone(Append(Input.a, f)))" ]
        let r = Script.runWith (largeInput (objOf [ "a", nulls (M - 2) ])) source
        Assert.Equal(decimal (M - 1), numberOf "n" r)
        // With M - 1 nulls the copy would have size M + 1.
        Script.runWith (largeInput (objOf [ "a", nulls (M - 1) ])) source |> failsForSize

// ================================================================================================
// 100.8.7 The maximum value size
// ================================================================================================

[<Collection("LargeValues")>]
type StringRow() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``StringDoubledBy24Statements_HasLengthM_The25thRaisesValueSizeLimitExceeded``() =
        let r = Script.runWith large (lines (stringDoubling 24 @ [ "Output.n = Length(s)" ]))
        Assert.Equal(decimal M, numberOf "n" r)
        Script.runWith large (lines (stringDoubling 25 @ [ "Output.n = Length(s)" ])) |> failsForSize

    [<Theory>]
    // s has M/2 code units (23 doublings) in each.
    [<InlineData("Join([s, s], \"\")", "Join([s, s], \"x\")")>]
    [<InlineData("Replace(s, \"x\", \"xx\")", "Replace(s & \"x\", \"x\", \"xx\")")>]
    [<InlineData("s & s", "s & s & \"x\"")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``StringFunction_ReturningMCodeUnits_Succeeds_ReturningMore_Raises``(atM: string, aboveM: string) =
        let r = Script.runWith large (lines (stringDoubling 23 @ [ sprintf "Output.n = Length(%s)" atM ]))
        Assert.Equal(decimal M, numberOf "n" r)
        Script.runWith large (lines (stringDoubling 23 @ [ sprintf "Output.n = Length(%s)" aboveM ])) |> failsForSize

    [<Theory>]
    [<InlineData("ToJson")>]
    [<InlineData("ToString")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``TextOfAnArray_OfMCodeUnitsOrLess_Succeeds_OfMore_Raises``(fn: string) =
        // [s] with s of M/2 is M/2 + 4 code units of text; [s, s] is M + 7.
        let r = Script.runWith large (lines (stringDoubling 23 @ [ sprintf "Output.n = Length(%s([s]))" fn ]))
        Assert.Equal(decimal (M / 2 + 4), numberOf "n" r)
        Script.runWith large (lines (stringDoubling 23 @ [ sprintf "Output.n = Length(%s([s, s]))" fn ])) |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``Base64Encode_Of12582912Characters_ReturnsM_Of12582913_Raises``() =
        let run n = Script.runWith (largeInput (objOf [ "s", chars n 'a' ])) "Output.n = Length(Base64Encode(Input.s))"
        Assert.Equal(decimal M, numberOf "n" (run 12_582_912))
        run 12_582_913 |> failsForSize // 16,777,220 code units

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``ToUpper_OfALowerCaseInputStringOfMPlus1_Raises``() =
        Script.runWith (largeInput (objOf [ "s", chars (M + 1) 'x' ])) "Output.n = Length(ToUpper(Input.s))" |> failsForSize

/// The arrays given as Input to the array row: M - 1, M and M + 1 ones.
type ArrayInputs() =
    member val MinusOne = filled (M - 1) one
    member val AtM = filled M one
    member val PlusOne = filled (M + 1) one

[<Collection("LargeValues")>]
type ArrayRow(fx: ArrayInputs) =

    member private _.Input() : JyroValue =
        objOf [ "m1", fx.MinusOne :> JyroValue; "m", fx.AtM; "p1", fx.PlusOne ] :> JyroValue

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``ArrayDoubledBy24Concatenates_HasMElements_The25thRaisesValueSizeLimitExceeded``() =
        let r = Script.runWith large (lines (arrayDoubling 24 @ [ "Output.n = Length(a)" ]))
        Assert.Equal(decimal M, numberOf "n" r)
        Script.runWith large (lines (arrayDoubling 25 @ [ "Output.n = Length(a)" ])) |> failsForSize

    [<Theory>]
    [<InlineData("Append(X, 1)")>]
    [<InlineData("Prepend(X, 1)")>]
    [<InlineData("Insert(X, 0, 1)")>]
    [<InlineData("Flatten([X, [1]])")>]
    [<InlineData("FlattenOnce([X, [1]])")>]
    [<InlineData("SelectMany([{t: X}, {t: [1]}], \"t\")")>]
    [<InlineData("Concatenate(X, [1])")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member this.``ArrayFunction_ReturningMElements_Succeeds_ReturningMPlus1_Raises``(call: string) =
        let run x = Script.runWith (largeInput (this.Input())) (sprintf "Output.n = Length(%s)" (call.Replace("X", x)))
        Assert.Equal(decimal M, numberOf "n" (run "Input.m1"))
        run "Input.m" |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``Split_IntoMParts_Succeeds_IntoMPlus1Parts_Raises``() =
        // The result has one more element than the occurrences of the separator (91.8.5).
        let run n = Script.runWith (largeInput (objOf [ "c", chars n ',' ])) "Output.n = Length(Split(Input.c, \",\"))"
        Assert.Equal(decimal M, numberOf "n" (run (M - 1)))
        run M |> failsForSize

    [<Theory>]
    [<InlineData("Reverse(X)")>]
    [<InlineData("Where(X, x => true)")>]
    [<InlineData("Map(X, x => x)")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member this.``NewArrayFromAnInputArray_OfM_Succeeds_OfMPlus1_Raises``(call: string) =
        let run x = Script.runWith (largeInput (this.Input())) (sprintf "Output.n = Length(%s)" (call.Replace("X", x)))
        Assert.Equal(decimal M, numberOf "n" (run "Input.m"))
        run "Input.p1" |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member this.``Sort_OfAnInputArrayOfMPlus1_Raises``() =
        Script.runWith (largeInput (this.Input())) "Output.n = Length(Sort(Input.p1))" |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``FromJson_OfAnArrayOfMPlus1Zeros_Raises``() =
        // The array boundary at M is pinned by the other rows; this parse alone takes seconds.
        let text n = "[" + String.Join(",", Array.create n "0") + "]"
        Script.runWith (largeInput (objOf [ "t", JyroString(text (M + 1)) ])) "Output.n = Length(FromJson(Input.t))"
        |> failsForSize

    interface IClassFixture<ArrayInputs>

/// A key for property i: base 62, so the M keys stay short.
let private key (i: int) : string =
    let digits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let sb = Text.StringBuilder()
    let mutable n = i
    sb.Append(digits.[n % 62]) |> ignore
    n <- n / 62
    while n > 0 do
        sb.Append(digits.[n % 62]) |> ignore
        n <- n / 62
    sb.ToString()

/// A host object of M properties (about 1.8 GB), built once for the object row.
/// Each test that changes it puts it back as it was.
type ObjectOfM() =
    let o = JyroObject()
    do
        for i in 0 .. M - 1 do o.SetProperty(key i, JyroNull.Instance)
    member _.Object = o
    member _.FirstKey = key 0

[<Collection("LargeValues")>]
type ObjectRow(fx: ObjectOfM) =

    member private _.WithOutput() : RunOptions = { large with Output = Some(fx.Object :> JyroValue) }

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member this.``OutputOfMProperties_AddingOne_RaisesValueSizeLimitExceeded_AndTheObjectIsUnchanged``() =
        try
            let r = Script.runWith (this.WithOutput()) "Output.extra = 1"
            failsForSize r
            // 130.2.2: the result's value is the final Output, also after an error.
            let v = Script.valueOf r :?> JyroObject
            Assert.Equal(M, v.Count)
            Assert.False(fst (v.TryGetValue "extra"), "The object that the write would have grown has the new property")
        finally
            fx.Object.Remove "extra" |> ignore

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member this.``OutputOfMMinus1Properties_AddingOne_Succeeds``() =
        fx.Object.Remove fx.FirstKey |> ignore
        try
            let r = Script.runWith (this.WithOutput()) "Output.extra = 1"
            let v = ok r :?> JyroObject
            Assert.Equal(M, v.Count)
        finally
            fx.Object.Remove "extra" |> ignore
            fx.Object.SetProperty(fx.FirstKey, JyroNull.Instance)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member this.``Merge_OfOutputOfMPropertiesAndOneMore_RaisesValueSizeLimitExceeded``() =
        Script.runWith (this.WithOutput()) "var m = Merge([Output, {extra: 1}])" |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``Keys_OfAHostObjectOfM_ReturnsM_OfMPlus1_RaisesValueSizeLimitExceeded``() =
        let big = ReturnsHostValue("Big", fx.Object)
        let options = { large with Functions = [ big.AsFunction ] }
        Assert.Equal(decimal M, numberOf "n" (Script.runWith options "Output.n = Length(Keys(Big()))"))
        fx.Object.SetProperty("extra", JyroNull.Instance)
        try
            Script.runWith options "Output.n = Length(Keys(Big()))" |> failsForSize
        finally
            fx.Object.Remove "extra" |> ignore

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    [<Trait("requirement", "JYRO-REQ-1929")>]
    member this.``FirstOutputOfMPlus1Properties_IsExempt_AlsoAsTheCopyOfTheSeedOfAReusedRunner``() =
        // A value the host supplies is not bound (100.8.7), nor is the copy of the WithOutput seed (JYRO-ADR-205).
        fx.Object.SetProperty("extra", JyroNull.Instance)
        try
            Script.runWith (this.WithOutput()) "var z = 1" |> ok |> ignore
        finally
            fx.Object.Remove "extra" |> ignore

    interface IClassFixture<ObjectOfM>

[<Collection("LargeValues")>]
type NodeRow() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    [<Trait("requirement", "JYRO-REQ-2167")>]
    member _.``JYRO-REQ-2167 Clone_OfTheSharedTreeAfter22Doublings_ReturnsTheCopy_After23_RaisesValueSizeLimitExceeded``() =
        // Sizes 12,582,911 and 25,165,823 (100.8.6); every array of the copy has length 2.
        let r = Script.runWith large (lines (treeDoubling "a" 22 @ [ "var c = Clone(a)"; "Output.n = Length(c)" ]))
        Assert.Equal(2m, numberOf "n" r)
        Script.runWith large (lines (treeDoubling "a" 23 @ [ "var c = Clone(a)" ])) |> failsForSize

/// Values the host makes or supplies, and arguments returned as given, are not bound.
[<Collection("LargeValues")>]
type Exemptions() =

    [<Theory>]
    [<InlineData("Output.s = Input.s")>]
    [<InlineData("Output.c = Clone(Input.s)")>]
    [<InlineData("Output.t = ToString(Input.s)")>]
    [<InlineData("Output.u = ToUpper(Input.u)")>]
    [<InlineData("Output.x = First(Input.list)")>]
    [<InlineData("Output.n = Length(Input.a)")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``InputValueAboveM_OrAnArgumentReturnedAsGiven_IsNotBound``(statement: string) =
        let big = filled (M + 1) one
        let list = JyroArray()
        list.Add big
        let input =
            objOf [ "s", chars (M + 1) 'x' :> JyroValue
                    "u", chars (M + 1) 'X'
                    "list", list
                    "a", big ]
        Script.runWith (largeInput input) statement |> ok |> ignore

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``HostFunctionResultAboveM_IsNotBound``() =
        let f = ReturnsHostValue("Big", filled (M + 1) one)
        let r = Script.runWith { large with Functions = [ f.AsFunction ] } "Output.a = Big()\nOutput.n = Length(Output.a)"
        Assert.Equal(decimal (M + 1), numberOf "n" r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1444")>]
let ``TheOperationsOf10084_KeepGeneratedLengthExceeded_NotValueSizeLimitExceeded`` () =
    for call in [ "PadLeft(\"x\", 1000001)"; "PadRight(\"x\", 1000001)"; "RandomString(1000001)"; "Range(0, 1000001)" ] do
        Script.runWith pinned (sprintf "Output.n = Length(%s)" call) |> failsWith MessageCode.GeneratedLengthExceeded

// ================================================================================================
// 100.8.8 The size check comes before the allocation
// ================================================================================================

/// Run `source`, which calls Mark(), under `options`; give the result, the
/// marks (process-wide allocated bytes; LargeValues runs alone) and the
/// process-wide allocated bytes after Execute returned.
let private runMarked (options: RunOptions) (source: string) : JyroResult<JyroValue> * int64 list * int64 =
    let mark = AllocationMark(false)
    let r = Script.runWith { options with Functions = mark.AsFunction :: options.Functions } source
    let after = GC.GetTotalAllocatedBytes(true)
    r, mark.Marks, after

[<Collection("LargeValues")>]
type BeforeAllocation() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member _.``AppendByIndexToAnArrayOfM_RaisesBeforeAllocating_AndLeavesTheArrayAtM``() =
        let r, marks, after =
            runMarked large (lines (arrayDoubling 23 @ [ "Mark()"; "a = Concatenate(a, a)"; "Mark()"; "Output.a = a"; "a[Length(a)] = 1" ]))
        failsForSize r
        let m1, m2 = marks.[0], marks.[1]
        let perElement = float (m2 - m1) / float M
        let spent = after - m2
        Assert.True(float spent < perElement * float M + float oneMB,
                    sprintf "The failing a[Length(a)] = 1 allocated %d bytes; storage for M elements is about %.0f" spent (perElement * float M))
        let a = Script.valueOf r |> Values.prop "a" :?> JyroArray
        Assert.Equal(M, a.Count)
        Assert.Equal(1m, Values.asDecimal a.[M - 1])

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member _.``AmpersandAboveM_RaisesBeforeAllocatingMoreThanMCodeUnits``() =
        let r, marks, after = runMarked large (lines (stringDoubling 24 @ [ "Mark()"; "var t = s & s" ]))
        failsForSize r
        let spent = after - marks.[0]
        Assert.True(spent < 2L * int64 M + oneMB, sprintf "The failing s & s allocated %d bytes; M code units are %d bytes" spent (2 * M))

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member _.``ConcatenateAboveM_RaisesBeforeAllocatingMoreThanMElements``() =
        let r, marks, after =
            runMarked large (lines (arrayDoubling 23 @ [ "Mark()"; "a = Concatenate(a, a)"; "Mark()"; "var b = Concatenate(a, a)" ]))
        failsForSize r
        let perElement = float (marks.[1] - marks.[0]) / float M
        let spent = after - marks.[1]
        Assert.True(float spent < perElement * float M + float oneMB,
                    sprintf "The failing Concatenate allocated %d bytes; storage for M elements is about %.0f" spent (perElement * float M))

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member _.``ToJsonOfControlCharacters_RaisesBeforeAllocatingMoreThanMCodeUnits``() =
        // d-502: s is 2^23 U+0001 characters (size 2^23 + 1, below M), and its
        // JSON escapes each as six code units, 6 x 2^23 in all, above M. The
        // text must be refused before storage for more than M code units
        // (2 x M bytes) is allocated; a growing buffer may hold that twice, so
        // the bound is 4 x M bytes + 1 MB, where the whole escaped text alone is
        // 6 x M bytes and more.
        let source = lines ([ "var s = \"\\u0001\"" ] @ List.replicate 23 "s = s & s" @ [ "Mark()"; "var t = ToJson(s)" ])
        let r, marks, after = runMarked large source
        failsForSize r
        let spent = after - marks.[0]
        Assert.True(spent < 4L * int64 M + oneMB,
                    sprintf "The failing ToJson allocated %d bytes; M code units are %d bytes" spent (2 * M))

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member _.``CloneAboveM_RaisesBeforeAllocatingMoreThanMNodes``() =
        let r22, marks22, _ = runMarked large (lines (treeDoubling "a" 22 @ [ "Mark()"; "var c = Clone(a)"; "Mark()" ]))
        ok r22 |> ignore
        let perNode = float (marks22.[1] - marks22.[0]) / float (treeSize 22)
        let r, marks, after = runMarked large (lines (treeDoubling "a" 23 @ [ "Mark()"; "var c = Clone(a)" ]))
        failsForSize r
        let spent = after - marks.[0]
        Assert.True(float spent < 1.1 * perNode * float M + float oneMB,
                    sprintf "The failing Clone allocated %d bytes; M nodes of a copy are about %.0f" spent (perNode * float M))

// -- 100.8.8 The walk that decides 100.8.9 records the distinct containers, not the tree ---------

/// The operations that 100.8.7 binds and that walk a container operand before
/// they raise ValueSizeLimitExceeded, each as a statement on `Input.v` (4.1.8):
/// Clone (95.9), ToJson (95.14), ToString (95.3), and the text of an exit or a
/// fail value (50.11.6). `&` refuses a container (40.4.2) and walks none.
let private walkStatement (op: string) : string =
    match op with
    | "Clone" -> "var c = Clone(Input.v)"
    | "ToJson" -> "var t = ToJson(Input.v)"
    | "ToString" -> "var t = ToString(Input.v)"
    | "exit" -> "exit Input.v"
    | "fail" -> "fail Input.v"
    | other -> failwithf "unknown operation %s" other

/// An array of n distinct empty objects then k nulls, supplied by the host (so
/// 100.8.7 does not bound it): D = n + 1, size 1 + n + k (100.8.6).
let private distinctThenNulls (n: int) (k: int) : JyroValue =
    let a = JyroArray()
    for _ in 1 .. n do a.Add(JyroObject())
    for _ in 1 .. k do a.Add nul
    a :> JyroValue

/// The bytes allocated from the Mark() immediately before the one statement
/// `op` on `Input.v` to the return of Execute (FLOORS 4.1.14: the value is
/// built, and the program compiled, before). The statement must fail with
/// ValueSizeLimitExceeded (the code first, FLOORS 4.1.11). Process-wide bytes:
/// LargeValues runs alone.
let private walkBytes (op: string) (v: JyroValue) : int64 =
    // A test measures several values one after another: give back what the last one built.
    reclaimHeap ()
    let r, marks, after = runMarked (largeInput (objOf [ "v", v ])) (lines [ "Mark()"; walkStatement op ])
    failsForSize r
    after - marks.[0]

[<Collection("LargeValues")>]
type WalkBeforeTheSizeError(output: Xunit.Abstractions.ITestOutputHelper) =

    // The fit of c for each operation: a, the bytes for n = 0 (M nulls; this holds
    // the storage 100.8.8 allows for M units before the error, and the run's own
    // fixed cost), and b, the bytes per distinct container, the slope between
    // n = 2^16 and n = 2^20. A pass is at most a + 1.5 x b x D + 1 MB for n = 2^22,
    // for k = M - n and k = M - n + 2^22, and for shared containers whose size
    // passes M (D = 2). Measured at f40b637 (the plan of 100.8.8, a whole
    // LargeValues run): Clone 1,585,984 bytes at n = 0; 5,647,032 at n = 65,536;
    // 77,018,568 at n = 1,048,576; 326,014,832 at n = 4,194,304; 38 KB more for
    // 2^20 more nulls; 1,575,184 for [{} x M] shared and 919,792 for
    // [{k: null} x M/2] shared; ToJson, ToString, exit and fail within 12 KB of
    // Clone. A failure means the walk records the unfolded tree, or every node,
    // not the distinct containers. Each test prints what it measured.

    [<Theory>]
    [<InlineData("Clone")>]
    [<InlineData("ToJson")>]
    [<InlineData("ToString")>]
    [<InlineData("exit")>]
    [<InlineData("fail")>]
    [<Trait("requirement", "JYRO-REQ-1445")>]
    member _.``WalkBeforeTheSizeError_GrowsWithTheDistinctContainers``(op: string) =
        let at n k = walkBytes op (distinctThenNulls n (k n))
        let fill n = M - n
        let a = at 0 fill
        let small = at (1 <<< 16) fill
        let large = at (1 <<< 20) fill
        let b = max 0.0 (float (large - small) / float ((1 <<< 20) - (1 <<< 16)))
        let bound (d: int) = a + int64 (1.5 * b * float d) + oneMB
        output.WriteLine("{0}: a = {1} (n = 0); {2} (n = 2^16); {3} (n = 2^20); b = {4:F1} bytes per container", op, a, small, large, b)
        let n = 1 <<< 22
        // The largest k is M - n + 2^22, not + 2^24: a list of about M + 2^24 elements and its
        // growth steps do not fit the heap beside what the collection keeps (JYRO-DEF-635).
        for k in [ M - n; M - n + (1 <<< 22) ] do
            let spent = walkBytes op (distinctThenNulls n k)
            output.WriteLine("{0}: n = 2^22, k = {1}: {2} bytes, bound {3}", op, k, spent, bound (n + 1))
            Assert.True(spent <= bound (n + 1),
                        sprintf "%s of 2^22 distinct objects and %d nulls allocated %d bytes; the fit gives at most %d" op k spent (bound (n + 1)))
        // The same n with 2^22 more nulls: what the walk keeps does not grow with the size.
        let more = at (1 <<< 16) (fun n -> M - n + (1 <<< 22))
        output.WriteLine("{0}: n = 2^16 with 2^22 more nulls: {1} bytes", op, more)
        Assert.True(abs (more - small) <= oneMB, sprintf "%s: n = 2^16 allocated %d bytes, and %d with 2^22 more nulls" op small more)
        // Shared containers whose size passes M, D = 2.
        let shared =
            [ "[{} x M]", (fun () -> filled M (JyroObject()) :> JyroValue)
              "[{k: null} x M/2]", (fun () -> filled (M / 2) (objOf [ "k", nul ]) :> JyroValue) ]
        for name, make in shared do
            let spent = walkBytes op (make ())
            output.WriteLine("{0} of {1}: {2} bytes, bound {3}", op, name, spent, bound 2)
            Assert.True(spent <= bound 2, sprintf "%s of %s (D = 2) allocated %d bytes; the fit gives at most %d" op name spent (bound 2))

// ================================================================================================
// 100.8.9 A cyclic or too-deep value fails as too deep, whatever its size
// ================================================================================================

[<Theory>]
[<InlineData("clone")>]
[<InlineData("tojson")>]
[<Trait("requirement", "JYRO-REQ-1446")>]
let ``BookExample_CyclicArrayOfTwoLongStrings_RaisesValueNestingLimitExceeded`` (case: string) =
    // var s = "x", 23 doublings, var a = [s, s], a[Length(a)] = a: Clone(a) and ToJson(a).
    let o = child ("nest|" + case)
    Child.expectCode MessageCode.ValueNestingLimitExceeded o

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1446")>]
let ``Flatten_OfTwoHalvesOfMAndACycle_RaisesValueNestingLimitExceeded`` () =
    // Flatten([b, b, c]) with b of M/2 + 1 nulls passes M elements before it reaches c.
    child "nest|flatten" |> Child.expectCode MessageCode.ValueNestingLimitExceeded

[<Theory>]
[<InlineData("ToObjectValue")>]
[<InlineData("ToDictionary")>]
[<InlineData("ToJson")>]
[<InlineData("WriteJson")>]
[<InlineData("ToStringValue")>]
[<InlineData("ToString")>]
[<Trait("requirement", "JYRO-REQ-1446")>]
let ``HostConversion_OfTwoLongStringsAndACycle_ThrowsValueNestingLimitExceeded`` (memberName: string) =
    child (sprintf "conv|sscyclic|%s" memberName) |> childFailsForDepth

[<Theory>]
[<InlineData("ToObjectValue")>]
[<InlineData("ToDictionary")>]
[<InlineData("ToJson")>]
[<InlineData("WriteJson")>]
[<InlineData("ToStringValue")>]
[<InlineData("ToString")>]
[<Trait("requirement", "JYRO-REQ-1446")>]
let ``HostConversion_OfTheOutputOfTheBookExample_ThrowsValueNestingLimitExceeded`` (memberName: string) =
    let o = child (sprintf "conv|outbook|%s" memberName)
    // The run itself succeeds: no walk of Output is made (160.8.3).
    Child.expectClean o
    Assert.Equal(Some "true", o.Result |> Option.bind runOf |> Option.bind (field "ok"))
    o |> childFailsForDepth

[<Collection("LargeValues")>]
type NestingPrecedence() =

    /// `var a = [s, s, d]` with s of M/2 code units and d nested `depth` deep.
    static member private Source (depth: int) (walk: string) : string =
        lines (stringDoubling 23 @ [ buildNested "d" depth; "var a = [s, s, d]"; sprintf "Output.r = Length(%s)" walk ])

    [<Theory>]
    [<InlineData("Clone(a)")>]
    [<InlineData("ToJson(a)")>]
    [<Trait("requirement", "JYRO-REQ-1446")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``TooDeepValueAboveM_RaisesValueNestingLimitExceeded_OneLevelLess_RaisesValueSizeLimitExceeded``(walk: string) =
        let n = maxDepth ()
        // d of N makes a of depth N + 1: too deep, though the walk passes M first.
        Script.runWith large (NestingPrecedence.Source n walk) |> failsWith MessageCode.ValueNestingLimitExceeded
        // d of N - 1 makes a of depth N: not too deep, so the size bound is what fails.
        Script.runWith large (NestingPrecedence.Source (n - 1) walk) |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1446")>]
    [<Trait("requirement", "JYRO-REQ-1444")>]
    member _.``Flatten_OfTwoHalvesOfMAndATooDeepArray_RaisesValueNestingLimitExceeded``() =
        let n = maxDepth ()
        let input () = objOf [ "b", nulls (M / 2 + 1) ]
        let source depth = lines [ buildNested "d" depth; "Output.r = Length(Flatten([Input.b, Input.b, d]))" ]
        Script.runWith (largeInput (input ())) (source n) |> failsWith MessageCode.ValueNestingLimitExceeded
        // One level less: the result would have M + 3 elements.
        Script.runWith (largeInput (input ())) (source (n - 1)) |> failsForSize

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1446")>]
    member _.``HostConversion_OfTwoLongStringsAndATooDeepArray_ThrowsValueNestingLimitExceeded``() =
        // [s, s', d] with d nested N deep: depth N + 1, and size above M before d.
        let a = hostTwoStringsAnd (nestedArrayValue (maxDepth ()))
        for m in valueMembers do throwsForDepth m (fun () -> convert m a)
        throwsForDepth "ToDictionary" (fun () -> (objOf [ "k", a ]).ToDictionary())

// ================================================================================================
// 100.8.11 The size check comes where the value would first exceed M
// ================================================================================================

/// Input arrays for the evaluation order: M ones, M + 1 ones, and M ones then a 0.
type OrderInputs() =
    member val AtM = filled M one
    member val PlusOne = filled (M + 1) one
    member val LastZero =
        let a = filled M one
        a.Add(JyroNumber(0))
        a

[<Collection("LargeValues")>]
type EvaluationOrder(fx: OrderInputs) =

    member private _.Input() : JyroValue =
        objOf [ "p1", fx.PlusOne :> JyroValue; "z", fx.LastZero; "m", fx.AtM; "s", chars M 'x' ] :> JyroValue

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1448")>]
    member this.``Map_OverMPlus1WhoseLastIs0_RaisesDivisionByZero``() =
        // The callback for the last element runs before its result would become element M + 1.
        Script.runWith (largeInput (this.Input())) "Output.n = Length(Map(Input.z, x => 1 / x))"
        |> failsWith MessageCode.DivisionByZero

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1448")>]
    member this.``Map_OverMPlus1Ones_RaisesValueSizeLimitExceeded_AfterCallingTheCallbackForEveryElement``() =
        let count = CountingFunction()
        let r = Script.runWith { largeInput (this.Input()) with Functions = [ count.AsFunction ] } "Output.n = Length(Map(Input.p1, x => Count(x)))"
        failsForSize r
        Assert.Equal(int64 (M + 1), count.Count)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1448")>]
    member this.``Map_TheBooksExample_XToX_OverMPlus1Ones_RaisesValueSizeLimitExceeded``() =
        Script.runWith (largeInput (this.Input())) "Output.n = Length(Map(Input.p1, x => x))" |> failsForSize

    [<Theory>]
    [<InlineData("Output.n = Length(Map(Input.p1, 5))", "CallbackExpected")>]
    [<InlineData("Output.n = Length(Concatenate(Input.m, 5))", "ArgumentTypeMismatch")>]
    [<InlineData("Output.n = Length(Input.m & Input.s)", "ConcatWithInvalidType")>]
    [<Trait("requirement", "JYRO-REQ-1448")>]
    member this.``ArgumentChecks_ComeBeforeTheSizeCheck``(source: string, code: string) =
        Script.runWith (largeInput (this.Input())) source |> failsWith (Enum.Parse<MessageCode> code)

    [<Theory>]
    [<InlineData("a[\"x\"] = 1", "InvalidIndexType")>]
    [<InlineData("a[Length(a) + 1] = 1", "IndexOutOfRange")>]
    [<Trait("requirement", "JYRO-REQ-1448")>]
    member _.``IndexChecks_OnAnArrayOfM_ComeBeforeTheSizeCheck``(statement: string, code: string) =
        Script.runWith large (lines (arrayDoubling 24 @ [ statement ])) |> failsWith (Enum.Parse<MessageCode> code)

    interface IClassFixture<OrderInputs>

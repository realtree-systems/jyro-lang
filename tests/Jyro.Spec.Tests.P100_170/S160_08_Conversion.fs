/// 160.8.1 (a host's conversion of a value above the maximum size throws
/// JyroRuntimeException ValueSizeLimitExceeded), 160.8.2 (a conversion that
/// fails for its size or its depth makes nothing), 160.8.3 (the size of Output
/// is reported by the conversion, not by the run), and the carrying of
/// 160.7.11 (ValueSizeLimitExceeded is 5919), 160.8.4 (ToJson() and WriteJson
/// refuse an unpaired surrogate in a key as in a value, after size and depth)
/// and 160.8.5 (WriteJson that refuses one has written nothing).
///
/// The members of 160.8.1 are exactly ToObjectValue(), JyroObject.ToDictionary(),
/// ToJson(), WriteJson(Utf8JsonWriter), ToStringValue() and ToString(), on every
/// class that derives from JyroValue. They are driven on each path a host has
/// (FLOORS 4.1.8): on the value of a result after a run, on host-built values
/// with no execution, and from a host function during an execution. Where a
/// build that does not conform would unfold 2^30 paths or a cycle, the call is
/// made in a child process (ValueSize.fs), killed after 30 s, under a 2 GB heap.
module RealTree.Jyro.Spec.Tests.S160_08_Conversion

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.ValueSize

let private nul : JyroValue = JyroNull.Instance :> JyroValue

/// A child heap limit of 2 GB, below the test host's.
let private childEnv = [ "DOTNET_GCHeapHardLimit", "80000000" ]

let private child (scenario: string) : Child.ChildOutcome = Child.runWith 30_000 childEnv ("vs|" + scenario)

/// The six members of 160.8.1.
let allMembers : obj[] seq =
    [ "ToObjectValue"; "ToDictionary"; "ToJson"; "WriteJson"; "ToStringValue"; "ToString" ] |> Seq.map (fun m -> [| box m |])

/// Assert the child's call threw JyroRuntimeException with Code ValueSizeLimitExceeded.
let private childThrowsForSize (o: Child.ChildOutcome) : unit =
    Child.expectClean o
    let result = o.Result.Value
    match tryValueSizeCode () with
    | None ->
        let own =
            if o.Ok || field "exn" result <> Some "JyroRuntimeException" then
                Some "Expected JyroRuntimeException with Code ValueSizeLimitExceeded"
            else None
        missingSizeCode own (sprintf "The child gave: %s" o.Describe)
    | Some code ->
        if o.Ok || o.Codes <> [ string code ] || field "exn" result <> Some "JyroRuntimeException" then
            Values.fail ("Expected JyroRuntimeException with Code ValueSizeLimitExceeded: " + o.Describe)

/// Assert the child's call failed with `code` (an exception carrying it).
let private childThrows (code: MessageCode) (o: Child.ChildOutcome) : unit =
    Child.expectClean o
    if o.Ok || not (List.contains (string code) o.Codes) || List.contains valueSizeName o.Codes then
        Values.fail (sprintf "Expected an exception carrying %A: %s" code o.Describe)

/// The run part of the child's line: it succeeded with no diagnostic.
let private runSucceededCleanly (o: Child.ChildOutcome) : unit =
    Child.expectClean o
    let run = o.Result |> Option.bind runOf
    Assert.True(run.IsSome, o.Describe)
    Assert.Equal(Some "true", field "ok" run.Value)
    Assert.Equal(Some "0", field "msgs" run.Value)
    Assert.Equal(Some "true", field "hasValue" run.Value)

// ================================================================================================
// 160.8.1 After a run: the book's script of 30 doublings
// ================================================================================================

[<Theory>]
[<MemberData(nameof allMembers)>]
[<Trait("requirement", "JYRO-REQ-1630")>]
let ``ResultOf30Doublings_EachMember_ThrowsValueSizeLimitExceededWithin10Seconds`` (memberName: string) =
    // var a = [1], a = [a, a] 30 times, Output.x = a. ToDictionary is called on
    // the result's value, which is the object Output.
    let o = child (sprintf "conv|out30|%s" memberName)
    o |> childThrowsForSize
    let ms = field "ms" o.Result.Value |> Option.map int64 |> Option.defaultValue Int64.MaxValue
    Assert.True(ms <= 10_000L, sprintf "%s took %d ms" memberName ms)

// ================================================================================================
// 160.8.1 With no execution: host-built values of size M and M + 1
// ================================================================================================

/// A value of size M and one of size M + 1 or more (100.8.6), host-built.
let private pairOf (kind: string) : JyroValue * JyroValue =
    match kind with
    | "string" -> chars (M - 1) 'x' :> JyroValue, chars M 'x' :> JyroValue
    | "array" -> nulls (M - 1) :> JyroValue, nulls M :> JyroValue
    | "objectkey" -> objOf [ String('k', M - 2), nul ] :> JyroValue, objOf [ String('k', M - 1), nul ] :> JyroValue
    | "shared" ->
        let shared n =
            let b = nulls n
            let a = JyroArray()
            a.Add b
            a.Add b
            a.Add nul
            a :> JyroValue
        shared (M / 2 - 2), shared (M / 2 - 1)
    | "dictionary" ->
        // {"": a}: 1 + 0 + size(a), for ToDictionary on an object.
        objOf [ "", nulls (M - 2) ] :> JyroValue, objOf [ "", nulls (M - 1) ] :> JyroValue
    | other -> failwithf "unknown kind %s" other

[<Collection("LargeValues")>]
type NoExecution() =

    [<Theory>]
    [<InlineData("string")>]
    [<InlineData("array")>]
    [<InlineData("objectkey")>]
    [<InlineData("shared")>]
    [<InlineData("dictionary")>]
    [<Trait("requirement", "JYRO-REQ-1630")>]
    member _.``HostBuiltValue_OfSizeM_EachMemberConverts_OfSizeAboveM_EachMemberThrows``(kind: string) =
        let members (v: JyroValue) = if v :? JyroObject then "ToDictionary" :: valueMembers else valueMembers
        let atM, above = pairOf kind
        for m in members atM do convert m atM |> ignore
        for m in members above do throwsForSize (sprintf "%s of a %s of size above M" m kind) (fun () -> convert m above)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1630")>]
    [<Trait("requirement", "JYRO-REQ-1632")>]
    member _.``LegalScriptValues_AStringOfLengthMAndAnArrayOfMElements_RunSucceeds_EveryConversionThrows``() =
        // Each is legal in the script (100.8.7) and has size M + 1 (100.8.6).
        let r = Script.runWith large (lines (stringDoubling 24 @ arrayDoubling 24 @ [ "Output.s = s"; "Output.a = a" ]))
        Assert.True(r.IsSuccess, outcomeOf r)
        Assert.Empty(r.Messages)
        let output = Script.valueOf r
        Assert.Equal(M, (Values.prop "s" output :?> JyroString).Length)
        Assert.Equal(M, (Values.prop "a" output :?> JyroArray).Count)
        for target, v in [ "Output.s", Values.prop "s" output; "Output.a", Values.prop "a" output ] do
            for m in valueMembers do throwsForSize (sprintf "%s of %s" m target) (fun () -> convert m v)
        for m in "ToDictionary" :: valueMembers do throwsForSize (m + " of Output") (fun () -> convert m output)

// ================================================================================================
// 160.8.1 During an execution: a host function converts the shared tree
// ================================================================================================

[<Theory>]
[<MemberData(nameof allMembers)>]
[<Trait("requirement", "JYRO-REQ-1630")>]
let ``DuringAnExecution_AHostFunctionConvertingTheSharedTreeOf30Doublings_GetsValueSizeLimitExceeded`` (memberName: string) =
    // The host function catches what the member throws, so the run itself succeeds.
    child (sprintf "during|%s" memberName) |> childThrowsForSize

// ================================================================================================
// 160.8.1 ToString() returns the text of ToStringValue(); values of size 1 never throw it
// ================================================================================================

let private outcomeText (f: unit -> string) : string =
    try "text:" + f () with e -> sprintf "throws:%s %A" (e.GetType().Name) (codesOfExn e)

let smallValues : (string * (unit -> JyroValue)) list =
    [ "null", (fun () -> nul)
      "true", (fun () -> JyroBoolean.True :> JyroValue)
      "false", (fun () -> JyroBoolean.False :> JyroValue)
      "0", (fun () -> JyroNumber(0) :> JyroValue)
      "exact", (fun () -> JyroNumber(1234567890123456789012345678m) :> JyroValue)
      "double", (fun () -> JyroNumber(0.1) :> JyroValue)
      "empty string", (fun () -> JyroString("") :> JyroValue)
      "quote", (fun () -> JyroString("a\"b") :> JyroValue)
      "[]", (fun () -> JyroArray() :> JyroValue)
      "[1, \"x\", null]", (fun () -> Values.json "[1, \"x\", null]")
      "{}", (fun () -> JyroObject() :> JyroValue)
      "{k: [1, {j: 2}]}", (fun () -> Values.json "{\"k\": [1, {\"j\": 2}]}")
      "function", (fun () -> JyroFunction((fun _ -> JyroNull.Instance :> JyroValue), 1) :> JyroValue) ]

let smallValueNames : obj[] seq = smallValues |> Seq.map (fun (n, _) -> [| box n |])

[<Theory>]
[<MemberData(nameof smallValueNames)>]
[<Trait("requirement", "JYRO-REQ-1630")>]
let ``ToString_ReturnsTheSameTextAsToStringValue`` (name: string) =
    let v = (smallValues |> List.find (fun (n, _) -> n = name) |> snd) ()
    Assert.Equal(outcomeText (fun () -> v.ToStringValue()), outcomeText (fun () -> v.ToString()))

[<Theory>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("exact")>]
[<InlineData("double")>]
[<InlineData("function")>]
[<Trait("requirement", "JYRO-REQ-1630")>]
let ``ValueOfSize1_NoMemberThrowsValueSizeLimitExceeded`` (name: string) =
    let v = (smallValues |> List.find (fun (n, _) -> n = name) |> snd) ()
    for m in valueMembers do neverSizeError (sprintf "%s of %s" m name) (fun () -> convert m v)

// ================================================================================================
// 160.8.2 A conversion that fails for its size or its depth makes nothing
// ================================================================================================

/// A writer on a stream that has written "[1" (committed) and ",2" (pending).
let private startedWriter () : MemoryStream * Utf8JsonWriter =
    let ms = new MemoryStream()
    let w = new Utf8JsonWriter(ms)
    w.WriteStartArray()
    w.WriteNumberValue(1)
    w.Flush()
    w.WriteNumberValue(2)
    ms, w

/// Call WriteJson(v) on a started writer; assert `check` of the call, then that
/// the writer and its stream are as they were.
let private writeJsonMakesNothing (check: string -> (unit -> unit) -> unit) (v: JyroValue) : unit =
    let ms, w = startedWriter ()
    use _ms = ms
    use _w = w
    let pending, committed, length = w.BytesPending, w.BytesCommitted, ms.Length
    check "WriteJson" (fun () -> v.WriteJson(w))
    Assert.Equal(pending, w.BytesPending)
    Assert.Equal(committed, w.BytesCommitted)
    Assert.Equal(length, ms.Length)

/// Call each member other than WriteJson; assert `check`, then that the calling
/// thread allocated less than 16 MB during the call.
let private othersMakeNothing (check: string -> (unit -> obj) -> unit) (v: JyroValue) : unit =
    for m in [ "ToObjectValue"; "ToDictionary"; "ToJson"; "ToStringValue"; "ToString" ] do
        let target = if m = "ToDictionary" && not (v :? JyroObject) then objOf [ "k", v ] :> JyroValue else v
        let before = GC.GetAllocatedBytesForCurrentThread()
        check m (fun () -> convert m target)
        let spent = GC.GetAllocatedBytesForCurrentThread() - before
        Assert.True(spent < 16L * oneMB, sprintf "%s allocated %d bytes before it threw" m spent)

[<Collection("LargeValues")>]
type MakesNothing() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1631")>]
    member _.``WriteJson_OfAnArrayOfMNulls_ThrowsForSize_AndHasNotWrittenToTheWriter``() =
        writeJsonMakesNothing throwsForSize (nulls M)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1631")>]
    member _.``EveryOtherMember_OfAnArrayOfMNulls_ThrowsForSize_BeforeMakingAHostValueOrText``() =
        // A converted array of M elements needs at least 128 MB, and its text 80 MB.
        othersMakeNothing throwsForSize (nulls M)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1631")>]
let ``WriteJson_OfAValueNestedPastTheMaximum_ThrowsForDepth_AndHasNotWrittenToTheWriter`` () =
    writeJsonMakesNothing throwsForDepth (nestedArrayValue (maxDepth () + 1))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1631")>]
let ``EveryOtherMember_OfAValueNestedPastTheMaximum_ThrowsForDepth_BeforeMakingAnything`` () =
    othersMakeNothing throwsForDepth (nestedArrayValue (maxDepth () + 1))

/// Values that run in a child: the host-built shared tree of 30 doublings (size
/// 3 x 2^30 - 1), a cyclic array, and [s, s', cycle] with strings of M/2.
let childValues : obj[] seq =
    [ for v in [ "tree30"; "cyclic"; "sscyclic" ] do
        for m in [ "ToObjectValue"; "ToDictionary"; "ToJson"; "WriteJson"; "ToStringValue"; "ToString" ] ->
            [| box v; box m |] ]

[<Theory>]
[<MemberData(nameof childValues)>]
[<Trait("requirement", "JYRO-REQ-1631")>]
let ``ConversionFailingForSizeOrDepth_MakesNothing`` (value: string, memberName: string) =
    let o = child (sprintf "conv|%s|%s" value memberName)
    if value = "tree30" then childThrowsForSize o else childThrows MessageCode.ValueNestingLimitExceeded o
    let result = o.Result.Value
    if memberName = "WriteJson" then
        Assert.True((field "unchanged" result = Some "true"), "WriteJson wrote to the writer before it threw: " + o.Describe)
    else
        let spent = field "alloc" result |> Option.map int64 |> Option.defaultValue Int64.MaxValue
        Assert.True(spent < 16L * oneMB, sprintf "%s allocated %d bytes before it threw: %s" memberName spent o.Describe)

// ================================================================================================
// 160.8.2 What a refusal allocates grows with the distinct containers, not with the size
// ================================================================================================

/// An array whose elements at 0 to n - 1 are n distinct empty objects and at n
/// to n + k - 1 are null: D = n + 1 distinct containers, size 1 + n + k (100.8.6).
let private distinctThenNulls (n: int) (k: int) : JyroValue =
    let a = JyroArray()
    for _ in 1 .. n do a.Add(JyroObject())
    for _ in 1 .. k do a.Add nul
    a :> JyroValue

/// The value 160.8.2 gives for the script of 160.8.1: an object whose property
/// x holds the 31 distinct arrays of 30 doublings (32 containers), built by the host.
let private resultOf30Doublings () : JyroValue = objOf [ "x", hostTree 30 ] :> JyroValue

/// Bytes the calling thread allocates in one call of `memberName` on `v`, which
/// must throw a JyroRuntimeException whose Code is the member named `codeName`
/// (the code first, FLOORS 4.1.11). Measured on that call alone (FLOORS 4.1.14):
/// the value and the writer are made before, and the member is called once
/// before to warm it (JIT, statics). ToDictionary is called on `v` when it is an
/// object and on {k: v} otherwise. WriteJson is given a writer holding "[1"
/// committed and ",2" pending, which must be as it was after the call. The call
/// runs on a thread of its own, bounded to 120 s, so that a build that unfolds
/// the value fails the test instead of hanging the run.
let private refusalBytes (codeName: string) (memberName: string) (v: JyroValue) : int64 =
    // The heap budget of the LargeValues collection (4 GiB) is shared by every test
    // of a whole run: give back what the previous measurement built, the large
    // object heap included, before this one starts (JYRO-DEF-710).
    reclaimHeap ()
    bounded 120.0 (fun () ->
        let target = if memberName = "ToDictionary" && not (v :? JyroObject) then objOf [ "k", v ] :> JyroValue else v
        let attempt (w: Utf8JsonWriter) : exn option =
            try
                (match memberName with
                 | "WriteJson" -> target.WriteJson(w)
                 | m -> convert m target |> ignore)
                None
            with e -> Some e
        let ms0, w0 = startedWriter ()
        use _ms0 = ms0
        use _w0 = w0
        attempt w0 |> ignore
        let ms, w = startedWriter ()
        use _ms = ms
        use _w = w
        let pending, committed = w.BytesPending, w.BytesCommitted
        let before = GC.GetAllocatedBytesForCurrentThread()
        let e = attempt w
        let spent = GC.GetAllocatedBytesForCurrentThread() - before
        match e with
        | Some (:? JyroRuntimeException as j) when Enum.GetName(typeof<MessageCode>, j.Code) = codeName -> ()
        | Some other -> Values.fail (sprintf "%s: expected JyroRuntimeException %s; threw %s (codes %A): %s" memberName codeName (other.GetType().FullName) (codesOfExn other) other.Message)
        | None -> Values.fail (sprintf "%s: expected JyroRuntimeException %s; the call returned normally" memberName codeName)
        if memberName = "WriteJson" then
            Assert.True((w.BytesPending, w.BytesCommitted) = (pending, committed), "WriteJson wrote to the writer before it threw")
        spent)

/// The per-member fit of 160.8.2's c: `a`, the bytes of a refusal of [null x M]
/// (D = 1), and `b`, the bytes per distinct container, the slope between n = 2^16
/// and n = 2^20 distinct empty objects followed by M - n nulls.
type private Fit = { A: int64; B: float; Detail: string }

let private fitFor (memberName: string) : Fit =
    let a = refusalBytes valueSizeName memberName (distinctThenNulls 0 M)
    let small = refusalBytes valueSizeName memberName (distinctThenNulls (1 <<< 16) (M - (1 <<< 16)))
    let large = refusalBytes valueSizeName memberName (distinctThenNulls (1 <<< 20) (M - (1 <<< 20)))
    let b = max 0.0 (float (large - small) / float ((1 <<< 20) - (1 <<< 16)))
    { A = a; B = b; Detail = sprintf "a = %d bytes for [null x M]; %d for n = 2^16; %d for n = 2^20; b = %.1f bytes per container" a small large b }

/// 64 KB: the slack beside every bound of 160.8.2 for what a call allocates
/// whatever the value (the exception, its message, a writer's scratch).
let private slack = 64L * 1024L

/// The bound of 160.8.2 for D distinct containers under the fit: c x (D + 1),
/// with c taken as 1.5 x b (the 1.5 allows a hash set's growth steps) above a.
let private bound (fit: Fit) (d: int) : int64 = fit.A + int64 (1.5 * fit.B * float d) + slack

[<Collection("LargeValues")>]
type AllocationBeforeARefusal(output: Xunit.Abstractions.ITestOutputHelper) =

    // Measured at f40b637 (the plan of 160.8.2, a whole LargeValues run): ToJson
    // 10,624 bytes for [null x M]; 8,462,760 for n = 100,000; 75,454,008 for
    // n = 1,000,000; 324,450,272 for n = 4,000,000 (75 to 85 bytes per
    // container); the same bytes for every k; the other members within 10 KB of
    // ToJson. Measured again at f40b637 with these tests, alone in a LargeValues
    // run, every member alike: 1,704 bytes for [null x M]; 4,073,552 for
    // n = 2^16; 75,445,088 for n = 2^20 (b = 72.6 bytes per container);
    // 324,441,352 for n = 2^22 (bound 456,845,179); 1,704 for [{} x M],
    // [{k: null} x M/2] and [null x (M + 2^20)] (bound about 67,500); 4,944 for
    // the value of 160.8.1 (bound 70,724); 12,864 for a cyclic array and an
    // array nested past the maximum depth. A failure of a bound means the
    // refusal allocates in proportion to the size of the value, not to its
    // distinct containers, as d-572 (313 MB) and d-573 (403 MB) did. Each test
    // prints what it measured.

    [<Theory>]
    [<InlineData("ToObjectValue")>]
    [<InlineData("ToDictionary")>]
    [<InlineData("ToJson")>]
    [<InlineData("WriteJson")>]
    [<InlineData("ToStringValue")>]
    [<InlineData("ToString")>]
    [<Trait("requirement", "JYRO-REQ-1631")>]
    member _.``AllocationBeforeARefusal_GrowsWithTheDistinctContainers_NotWithTheSize``(memberName: string) =
        let fit = fitFor memberName
        output.WriteLine("{0}: {1}", memberName, fit.Detail)
        // A third n: n = 2^22 distinct objects then M - n nulls, D = 2^22 + 1.
        let n = 1 <<< 22
        let third = refusalBytes valueSizeName memberName (distinctThenNulls n (M - n))
        output.WriteLine("{0}: {1} bytes for n = 2^22, bound {2}", memberName, third, bound fit (n + 1))
        Assert.True(third <= bound fit (n + 1),
                    sprintf "%s allocated %d bytes refusing n = 2^22 distinct objects; the fit gives at most %d (%s)" memberName third (bound fit (n + 1)) fit.Detail)

    [<Theory>]
    [<InlineData("ToObjectValue")>]
    [<InlineData("ToDictionary")>]
    [<InlineData("ToJson")>]
    [<InlineData("WriteJson")>]
    [<InlineData("ToStringValue")>]
    [<InlineData("ToString")>]
    [<Trait("requirement", "JYRO-REQ-1631")>]
    member _.``AllocationBeforeARefusal_IsTheSameForEveryK``(memberName: string) =
        // 160.8.2's example: n distinct empty objects then k nulls, size above M:
        // the same bound for every k. For each n, the bytes over k = M - n,
        // M - n + 2^20 and M - n + 2^22 differ by at most the slack. The largest k
        // stays at M - n + 2^22 (about 21M elements for n = 2^22, not 33.5M), so that
        // the builder's list and its growth copy fit the 4 GiB heap of a whole run.
        for n in [ 1 <<< 16; 1 <<< 20; 1 <<< 22 ] do
            let bytes =
                [ for k in [ M - n; M - n + (1 <<< 20); M - n + (1 <<< 22) ] ->
                    refusalBytes valueSizeName memberName (distinctThenNulls n k) ]
            output.WriteLine("{0}: n = {1}: {2}", memberName, n, String.Join(", ", bytes))
            Assert.True(List.max bytes - List.min bytes <= slack,
                        sprintf "%s: for n = %d the bytes over k were %A, which grow with the size" memberName n bytes)

    [<Theory>]
    [<InlineData("ToObjectValue")>]
    [<InlineData("ToDictionary")>]
    [<InlineData("ToJson")>]
    [<InlineData("WriteJson")>]
    [<InlineData("ToStringValue")>]
    [<InlineData("ToString")>]
    [<Trait("requirement", "JYRO-REQ-1631")>]
    member _.``SharedContainersAboveM_AllocateForTheirFewDistinctContainers``(memberName: string) =
        // Values whose size passes M while D stays small: the object paths that
        // the arrays of nulls and trees of arrays never reached (d-572, d-573).
        let fit = fitFor memberName
        let shared (count: int) (o: JyroValue) : JyroValue = filled count o :> JyroValue
        let cases : (string * string * int * (unit -> JyroValue)) list =
            [ "[{k: null} x M/2]", valueSizeName, 2, (fun () -> shared (M / 2) (objOf [ "k", nul ] :> JyroValue))
              "[{} x M]", valueSizeName, 2, (fun () -> shared M (JyroObject() :> JyroValue))
              "[null x M]", valueSizeName, 1, (fun () -> nulls M :> JyroValue)
              "[null x (M + 2^20)]", valueSizeName, 1, (fun () -> nulls (M + (1 <<< 20)) :> JyroValue)
              "the value of the script of 160.8.1", valueSizeName, 32, resultOf30Doublings
              "a cyclic array", "ValueNestingLimitExceeded", 1, (fun () -> hostCycle () :> JyroValue)
              "an array nested past the maximum depth", "ValueNestingLimitExceeded", maxDepth () + 1, (fun () -> nestedArrayValue (maxDepth () + 1)) ]
        for name, codeName, d, make in cases do
            let d = if memberName = "ToDictionary" && name <> "the value of the script of 160.8.1" then d + 1 else d
            let spent = refusalBytes codeName memberName (make ())
            output.WriteLine("{0} of {1}: {2} bytes, bound {3} (D = {4})", memberName, name, spent, bound fit d, d)
            Assert.True(spent <= bound fit d,
                        sprintf "%s of %s (D = %d) allocated %d bytes; the fit gives at most %d (%s)" memberName name d spent (bound fit d) fit.Detail)
        // 160.8.2's example: the value of the script of 160.8.1 at most 33 x c.
        let spent = refusalBytes valueSizeName memberName (resultOf30Doublings ())
        Assert.True(spent <= bound fit 33, sprintf "%s of the value of 160.8.1 allocated %d bytes; 33 x c is at most %d" memberName spent (bound fit 33))

// ================================================================================================
// 160.8.3 The size of Output is reported by the conversion, not by the run
// ================================================================================================

[<Theory>]
[<InlineData("Execute")>]
[<InlineData("ExecuteOrThrow")>]
[<InlineData("ExecuteScript")>]
[<Trait("requirement", "JYRO-REQ-1632")>]
let ``ScriptOf30Doublings_OnEveryPath_SucceedsWithNoDiagnostic`` (path: string) =
    // Execute and ExecuteOrThrow set all four limits; ExecuteScript
    // takes none, and the script's 32 statements are far inside
    // every default (100.1.2).
    let o = child ("path|" + path)
    Child.expectSuccess o
    let result = o.Result.Value
    Assert.Equal(Some "0", field "msgs" result)
    Assert.Equal(Some "true", field "hasValue" result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1632")>]
let ``ScriptOf30Doublings_OnlyTheLaterToJsonOfItsValueThrowsValueSizeLimitExceeded`` () =
    let o = child "conv|out30|ToJson"
    runSucceededCleanly o
    childThrowsForSize o

[<Collection("LargeValues")>]
type OutputAboveM() =

    [<Theory>]
    [<InlineData("Execute")>]
    [<InlineData("ExecuteScript")>]
    [<Trait("requirement", "JYRO-REQ-1632")>]
    member _.``OutputHoldingAStringOfLengthM_OnEveryPath_SucceedsWithNoDiagnostic``(path: string) =
        // Output {s: s} with s of M code units has size M + 3.
        let source = lines (stringDoubling 24 @ [ "Output.s = s" ])
        let r =
            match path with
            | "Execute" -> Script.runWith large source
            | _ -> JyroScript.executeScript source (box (Dictionary<string, obj>()))
        Assert.True(r.IsSuccess, outcomeOf r)
        Assert.Empty(r.Messages)
        Assert.True(r.HasValue)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1632")>]
    member _.``OutputHoldingAStringOfLengthM_ExecuteOrThrow_ReturnsTheValue``() =
        let program = Script.compileWith large (lines (stringDoubling 24 @ [ "Output.s = s" ])) |> Script.valueOf
        let v = JyroRunner(program).WithInputJson("{}").WithExecutionOptions(bigLimits (TimeSpan.FromMinutes 3.)).ExecuteOrThrow()
        Assert.Equal(M, (Values.prop "s" v :?> JyroString).Length)
        throwsForSize "ToJson of the Output" (fun () -> v.ToJson())

// ================================================================================================
// 160.7.11 The error of 100.8.7 and the exception of 160.8.1 carry the member 5919
// ================================================================================================

[<Collection("LargeValues")>]
type CarriesTheCode() =

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1622")>]
    member _.``The25thDoubling_RaisesTheMemberValueSizeLimitExceeded_5919_StageExecute``() =
        let r = Script.runWith large (lines (stringDoubling 25))
        let d = Script.onlyErrorMessage r
        Assert.Equal(5919, int d.Code)
        Assert.Equal("ValueSizeLimitExceeded", Enum.GetName(typeof<MessageCode>, d.Code))
        assertExecuteStage d

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1622")>]
    member _.``ToJson_OfAValueOfSizeMPlus1_ThrowsJyroRuntimeExceptionWithTheMember5919``() =
        let e = Assert.ThrowsAny<exn>(fun () -> (nulls M).ToJson() |> ignore)
        match e with
        | :? JyroRuntimeException as j ->
            Assert.Equal(5919, int j.Code)
            Assert.Equal("ValueSizeLimitExceeded", Enum.GetName(typeof<MessageCode>, j.Code))
        | other -> Values.fail (sprintf "Expected JyroRuntimeException; threw %s: %s" (other.GetType().FullName) other.Message)

// ================================================================================================
// 160.8.4 The host's JSON members refuse an unpaired surrogate in a key as in a value
// ================================================================================================

/// Single surrogate code units, made from their numbers: an F# string literal
/// "\uD800" does not hold the code unit U+D800.
let private d800 : string = string (char 0xD800)
let private dc00 : string = string (char 0xDC00)
let private d83d : string = string (char 0xD83D)

/// Strings that hold an unpaired surrogate (95.14.4): a high alone, a low alone,
/// a high in the middle, a reversed pair (two unpaired), and a high at the end.
let private surrogateStrings : (string * string) list =
    [ "high", d800
      "low", dc00
      "middle", "a" + d800 + "b"
      "reversed", dc00 + d800
      "high-at-end", "x" + d83d ]

let private str (s: string) : JyroValue = JyroString(s) :> JyroValue

let private arrOf (xs: JyroValue list) : JyroValue =
    let a = JyroArray()
    for x in xs do a.Add x
    a :> JyroValue

let private objWith (pairs: (string * JyroValue) list) : JyroValue =
    let o = JyroObject()
    for k, v in pairs do o.SetProperty(k, v)
    o :> JyroValue

/// Where the surrogate string `s` stands in the value a member is called on:
/// as a string value, and as a property key, at the top and nested, first and
/// after valid parts that a writer in order would meet before it.
let private positioned (position: string) (s: string) : JyroValue =
    match position with
    | "self" -> str s
    | "element" -> arrOf [ str s ]
    | "element-after-valid" -> arrOf [ str "a"; Values.int 1; str s ]
    | "property-value" -> objWith [ "k", str s ]
    | "property-value-after-valid" -> objWith [ "a", str "v"; "b", Values.int 1; "k", str s ]
    | "value-three-deep" -> objWith [ "a", arrOf [ objWith [ "b", str s ] ] ]
    | "key-top" -> objWith [ s, Values.int 1 ]
    | "key-after-valid" -> objWith [ "a", Values.int 1; s, Values.int 2 ]
    | "key-in-array" -> arrOf [ str "a"; objWith [ s, Values.int 1 ] ]
    | "key-in-object" -> objWith [ "o", objWith [ "p", Values.int 0; s, Values.int 1 ] ]
    | "key-three-deep" -> arrOf [ objWith [ "a", objWith [ "b", objWith [ s, nul ] ] ] ]
    | other -> failwithf "unknown position %s" other

let private valuePositions = [ "self"; "element"; "element-after-valid"; "property-value"; "property-value-after-valid"; "value-three-deep" ]
let private keyPositions = [ "key-top"; "key-after-valid"; "key-in-array"; "key-in-object"; "key-three-deep" ]

let private cases (positions: string list) : obj[] seq =
    [ for m in [ "ToJson"; "WriteJson" ] do
        for p in positions do
            for s, _ in surrogateStrings -> [| box m; box p; box s |] ]

let surrogateValueCases : obj[] seq = cases valuePositions
let surrogateKeyCases : obj[] seq = cases keyPositions
let surrogateAllCases : obj[] seq = cases (valuePositions @ keyPositions)

let private surrogateNamed (name: string) : string = surrogateStrings |> List.find (fun (n, _) -> n = name) |> snd

/// Call `memberName` on `v`: ToJson() returns its text; WriteJson writes to a
/// fresh writer on a stream, flushed after the call whether it threw or not.
/// Gives the exception, if any, and the text the member returned or wrote.
let private callJsonMember (memberName: string) (v: JyroValue) : exn option * string =
    match memberName with
    | "ToJson" ->
        try None, v.ToJson()
        with e -> Some e, ""
    | _ ->
        use ms = new MemoryStream()
        let w = new Utf8JsonWriter(ms)
        let e = try v.WriteJson(w); None with e -> Some e
        (try w.Flush() with _ -> ())
        (try w.Dispose() with _ -> ())
        e, Text.Encoding.UTF8.GetString(ms.ToArray())

/// Assert the exception is a JyroRuntimeException whose Code is UnpairedSurrogate (the code first, FLOORS 4.1.11).
let private assertUnpairedSurrogate (what: string) (e: exn option) : unit =
    match e with
    | None -> Values.fail (sprintf "%s: expected JyroRuntimeException UnpairedSurrogate; the call returned normally" what)
    | Some (:? JyroRuntimeException as j) ->
        Assert.True((j.Code = MessageCode.UnpairedSurrogate),
                    sprintf "%s: expected Code UnpairedSurrogate, got %A (%d)" what j.Code (int j.Code))
    | Some other -> Values.fail (sprintf "%s: expected JyroRuntimeException UnpairedSurrogate; threw %s: %s" what (other.GetType().FullName) other.Message)

[<Theory>]
[<MemberData(nameof surrogateValueCases)>]
[<Trait("requirement", "JYRO-REQ-1633")>]
let ``EachMember_WhenAStringValueHoldsAnUnpairedSurrogate_ThrowsUnpairedSurrogate`` (memberName: string, position: string, surrogate: string) =
    let v = positioned position (surrogateNamed surrogate)
    callJsonMember memberName v |> fst |> assertUnpairedSurrogate (sprintf "%s of %s (%s)" memberName position surrogate)

[<Theory>]
[<MemberData(nameof surrogateKeyCases)>]
[<Trait("requirement", "JYRO-REQ-1633")>]
let ``EachMember_WhenAPropertyKeyHoldsAnUnpairedSurrogate_ThrowsUnpairedSurrogate`` (memberName: string, position: string, surrogate: string) =
    let v = positioned position (surrogateNamed surrogate)
    callJsonMember memberName v |> fst |> assertUnpairedSurrogate (sprintf "%s of %s (%s)" memberName position surrogate)

[<Theory>]
[<InlineData("ToJson")>]
[<InlineData("WriteJson")>]
[<Trait("requirement", "JYRO-REQ-1633")>]
let ``TheClausesExamples_AKeyAndAValueOfU_D800_EachThrowUnpairedSurrogate`` (memberName: string) =
    let byKey = objWith [ d800, Values.int 1 ]
    let byValue = objWith [ "k", str d800 ]
    callJsonMember memberName byKey |> fst |> assertUnpairedSurrogate (memberName + " of {U+D800: 1}")
    callJsonMember memberName byValue |> fst |> assertUnpairedSurrogate (memberName + " of {k: U+D800}")

/// The texts a member would write in place of `s` if it replaced each unpaired
/// surrogate: by U+FFFD, by "?", or by nothing.
let private replacedForms (s: string) : string list =
    let replace (by: string) =
        let sb = Text.StringBuilder()
        let mutable i = 0
        while i < s.Length do
            let c = s.[i]
            if Char.IsHighSurrogate c && i + 1 < s.Length && Char.IsLowSurrogate s.[i + 1] then
                sb.Append(c).Append(s.[i + 1]) |> ignore
                i <- i + 2
            else
                (if Char.IsSurrogate c then sb.Append(by) else sb.Append(c)) |> ignore
                i <- i + 1
        sb.ToString()
    [ replace "�"; replace "?"; replace "" ]

[<Theory>]
[<MemberData(nameof surrogateAllCases)>]
[<Trait("requirement", "JYRO-REQ-1633")>]
let ``NeitherMember_WritesU_FFFDOrAnotherCodeUnitInPlaceOfTheSurrogate`` (memberName: string, position: string, surrogate: string) =
    let s = surrogateNamed surrogate
    let _, text = callJsonMember memberName (positioned position s)
    Assert.False(text.Contains '�', sprintf "%s of %s (%s) wrote U+FFFD: %s" memberName position surrogate text)
    for form in replacedForms s do
        // The form as a JSON string, as a writer would write it (a key or a value).
        let quoted = JsonSerializer.Serialize(form)
        Assert.False(text.Contains quoted, sprintf "%s of %s (%s) wrote %s in place of the surrogate: %s" memberName position surrogate quoted text)

// -- 160.8.4 Size and depth come before the surrogate -----------------------------------------------

[<Theory>]
[<InlineData("ToJson")>]
[<InlineData("WriteJson")>]
[<Trait("requirement", "JYRO-REQ-1633")>]
[<Trait("requirement", "JYRO-REQ-1446")>]
let ``Cyclic_WithASurrogateFirst_ThrowsValueNestingLimitExceeded_NotUnpairedSurrogate`` (memberName: string) =
    // In a child: a build that does not conform could walk the cycle without end.
    child (sprintf "conv|surrcyclic|%s" memberName) |> childThrows MessageCode.ValueNestingLimitExceeded

[<Theory>]
[<InlineData("ToJson")>]
[<InlineData("WriteJson")>]
[<Trait("requirement", "JYRO-REQ-1633")>]
[<Trait("requirement", "JYRO-REQ-1446")>]
let ``NestedPastTheMaximum_WithASurrogateFirst_ThrowsValueNestingLimitExceeded_NotUnpairedSurrogate`` (memberName: string) =
    // [U+D800, nested maxDepth deep]: depth maxDepth + 1 (100.7.1), the surrogate met first.
    let v = arrOf [ str d800; nestedArrayValue (maxDepth ()) ]
    throwsForDepth (memberName + " of a too-deep value with a surrogate first") (fun () -> convert memberName v)

[<Collection("LargeValues")>]
type SurrogateAboveTheSize() =

    [<Theory>]
    [<InlineData("ToJson")>]
    [<InlineData("WriteJson")>]
    [<Trait("requirement", "JYRO-REQ-1633")>]
    [<Trait("requirement", "JYRO-REQ-1630")>]
    member _.``AboveTheMaximumSize_WithASurrogateFirst_ThrowsValueSizeLimitExceeded_NotUnpairedSurrogate``(memberName: string) =
        // [U+D800, null x M]: size above M (100.8.6), the surrogate met first.
        let a = nulls M
        a.Insert(0, str d800)
        throwsForSize (memberName + " of a value above M with a surrogate first") (fun () -> convert memberName (a :> JyroValue))

// ================================================================================================
// 160.8.5 A JSON member that refuses an unpaired surrogate writes nothing
// ================================================================================================

/// Values that WriteJson must refuse, the surrogate in a key or a string value,
/// at each place in the order of writing: first, last, after valid siblings, deep.
let private refusedValues : (string * (unit -> JyroValue)) list =
    [ "example", (fun () -> arrOf [ str "a"; str dc00 ])
      "key-last", (fun () -> objWith [ "a", Values.int 1; dc00, Values.int 2 ])
      "value-last", (fun () -> objWith [ "a", str "x"; "k", str d800 ])
      "key-three-deep", (fun () -> arrOf [ Values.int 1; objWith [ "a", Values.int 2; "b", objWith [ "c", objWith [ "d", nul; d800, nul ] ] ] ])
      "only", (fun () -> str d800)
      "key-first", (fun () -> objWith [ dc00, Values.int 1; "a", Values.int 2 ]) ]

/// A writer on a fresh stream in one of three states: fresh; with pending bytes
/// only; with bytes committed and more pending.
let private writerIn (state: string) : MemoryStream * Utf8JsonWriter =
    let ms = new MemoryStream()
    let w = new Utf8JsonWriter(ms)
    match state with
    | "fresh" -> ()
    | "pending" ->
        w.WriteStartArray()
        w.WriteStringValue("pre")
    | _ ->
        w.WriteStartArray()
        w.WriteStringValue("pre")
        w.Flush()
        w.WriteNumberValue(7)
    ms, w

let refusedCases : obj[] seq =
    [ for name, _ in refusedValues do
        for state in [ "fresh"; "pending"; "committed" ] -> [| box name; box state |] ]

[<Theory>]
[<MemberData(nameof refusedCases)>]
[<Trait("requirement", "JYRO-REQ-1634")>]
[<Trait("requirement", "JYRO-REQ-1633")>]
let ``WriteJson_WhenItThrowsUnpairedSurrogate_LeavesTheWriterAsItWas`` (value: string, state: string) =
    let v = (refusedValues |> List.find (fun (n, _) -> n = value) |> snd) ()
    let ms, w = writerIn state
    use _ms = ms
    let pending, committed = w.BytesPending, w.BytesCommitted
    let e = try v.WriteJson(w); None with e -> Some e
    assertUnpairedSurrogate (sprintf "WriteJson of %s on a %s writer" value state) e
    Assert.Equal(pending, w.BytesPending)
    Assert.Equal(committed, w.BytesCommitted)
    // Flushed, the stream holds what a writer in the same state that was never
    // given the value holds.
    (try w.Flush() with _ -> ())
    let control, cw = writerIn state
    use _control = control
    cw.Flush()
    Assert.Equal<byte[]>(control.ToArray(), ms.ToArray())

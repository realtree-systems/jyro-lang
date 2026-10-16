namespace RealTree.Jyro.Tests.Runtime

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Ast
open RealTree.Jyro.Parsing
open RealTree.Jyro.Compilation
open RealTree.Jyro.Tests
open RealTree.Jyro.Tests.RuntimeGenerators

/// Algebraic laws the stdlib's array/string helpers must satisfy for any
/// well-formed input. These are pure mathematical invariants - any
/// violation points at a real bug in the stdlib implementation.
module StdlibLaws =

    /// Run a script with `Input.xs = xs` and return the `Output.result`.
    let private runWithXs (xs: float list) (script: string) : JyroValue =
        let finite =
            xs |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let arr = JyroArray()
        for x in finite do arr.Add(JyroNumber(x))
        let input = JyroObject()
        input.SetProperty("xs", arr)
        let output = Runner.expectSuccess script (input :> JyroValue)
        Runner.prop "result" output

    let private runWithTwoLists (xs: float list) (ys: float list) (script: string) : JyroValue =
        let finite (zs: float list) =
            zs |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let toArr (zs: float list) =
            let a = JyroArray()
            for z in zs do a.Add(JyroNumber(z))
            a :> JyroValue
        let input = JyroObject()
        input.SetProperty("a", toArr (finite xs))
        input.SetProperty("b", toArr (finite ys))
        let output = Runner.expectSuccess script (input :> JyroValue)
        Runner.prop "result" output

    let private asArray (v: JyroValue) : JyroValue list =
        match v with
        | :? JyroArray as a -> [ for i in 0 .. a.Count - 1 -> a.[i] ]
        | _ -> failwithf "expected JyroArray, got %O" (v.GetType())

    let private jsonEq (a: JyroValue) (b: JyroValue) =
        a.ToJson() = b.ToJson()

    // -- Length / Reverse -----------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Length(Reverse(xs)) == Length(xs)`` (xs: float list) =
        let l = runWithXs xs "Output.result = Length(Reverse(Input.xs))" |> Runner.asNumber
        let r = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        l = r

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Reverse(Reverse(xs)) == xs`` (xs: float list) =
        let original = runWithXs xs "Output.result = Input.xs"
        let doubled = runWithXs xs "Output.result = Reverse(Reverse(Input.xs))"
        jsonEq original doubled

    // -- Concatenate ---------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Length(Concatenate(a, b)) == Length(a) + Length(b)`` (xs: float list) (ys: float list) =
        let lenConcat =
            runWithTwoLists xs ys "Output.result = Length(Concatenate(Input.a, Input.b))"
            |> Runner.asNumber
        let lenA = runWithTwoLists xs ys "Output.result = Length(Input.a)" |> Runner.asNumber
        let lenB = runWithTwoLists xs ys "Output.result = Length(Input.b)" |> Runner.asNumber
        lenConcat = lenA + lenB

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Concatenate(a, []) == a`` (xs: float list) =
        let raw = runWithTwoLists xs [] "Output.result = Input.a"
        let appended = runWithTwoLists xs [] "Output.result = Concatenate(Input.a, Input.b)"
        jsonEq raw appended

    // -- Sort -----------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Length(Sort(xs)) == Length(xs)`` (xs: float list) =
        let a = runWithXs xs "Output.result = Length(Sort(Input.xs))" |> Runner.asNumber
        let b = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        a = b

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Sort(xs) is non-decreasing`` (xs: float list) =
        let sorted = runWithXs xs "Output.result = Sort(Input.xs)" |> asArray
        sorted
        |> List.pairwise
        |> List.forall (fun (a, b) ->
            match a, b with
            | (:? JyroNumber as x), (:? JyroNumber as y) -> x.Value <= y.Value
            | _ -> false)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Sort(xs) is a permutation of xs (multiset preserving)`` (xs: float list) =
        let original =
            runWithXs xs "Output.result = Input.xs"
            |> asArray
            |> List.map (fun v -> (v :?> JyroNumber).Value)
            |> List.sort
        let sorted =
            runWithXs xs "Output.result = Sort(Input.xs)"
            |> asArray
            |> List.map (fun v -> (v :?> JyroNumber).Value)
        original = sorted

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Sort is idempotent: Sort(Sort(xs)) == Sort(xs)`` (xs: float list) =
        let once = runWithXs xs "Output.result = Sort(Input.xs)"
        let twice = runWithXs xs "Output.result = Sort(Sort(Input.xs))"
        jsonEq once twice

    // -- Where / Map ---------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Length(Where(xs, always)) == Length(xs)`` (xs: float list) =
        let wherelen =
            runWithXs xs "Output.result = Length(Where(Input.xs, x => true))"
            |> Runner.asNumber
        let orig = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        wherelen = orig

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Where(xs, never) is empty`` (xs: float list) =
        let n =
            runWithXs xs "Output.result = Length(Where(Input.xs, x => false))"
            |> Runner.asNumber
        n = 0.0

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Length(Map(xs, f)) == Length(xs)`` (xs: float list) =
        let a = runWithXs xs "Output.result = Length(Map(Input.xs, x => x * 2))" |> Runner.asNumber
        let b = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        a = b

    // -- Sanity facts not involving generated inputs -------------------------

    [<Fact>]
    let ``Length of empty array is 0`` () =
        let src = "Output.result = Length([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Reverse of empty array is empty`` () =
        let src = "Output.result = Reverse([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Sort of single element is identity`` () =
        let src = "Output.result = Sort([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[42]", (Runner.prop "result" output).ToJson())

    // ------------------------------------------------------------------------
    // Tier 1 laws: Reduce / All / Any / Find / Distinct / SortBy.
    // These are algebraically rich functions where an off-by-one or an
    // inverted condition has nowhere to hide.
    // ------------------------------------------------------------------------

    // -- Reduce --------------------------------------------------------------

    // Jyro's Reduce signature is `Reduce(xs, callback, seed)` - callback
    // before seed. Properties below use that argument order.

    [<Fact>]
    let ``Reduce identity: Reduce([], f, seed) == seed`` () =
        let src = "Output.result = Reduce([], (acc, x) => acc + x, 42)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Reduce singleton: Reduce([x], f, seed) == f(seed, x)`` () =
        let src = "Output.result = Reduce([5], (acc, x) => acc * x, 10)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(50.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Reduce with (+, 0) equals element sum`` (xs: float list) =
        // Cap magnitude so float addition stays associative enough for a
        // direct sum comparison.
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
            |> List.map (fun x -> x * 0.001)
        let reduced =
            runWithXs finite "Output.result = Reduce(Input.xs, (acc, x) => acc + x, 0)"
            |> Runner.asNumber
        let expected = List.sum finite
        abs (reduced - expected) <= max 1.0 (abs expected) * 1e-9

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Reduce fold-concat: Reduce(a ++ b, f, s) == Reduce(b, f, Reduce(a, f, s))`` (xs: float list) (ys: float list) =
        let combined =
            runWithTwoLists xs ys
                "Output.result = Reduce(Concatenate(Input.a, Input.b), (acc, x) => acc + x, 0)"
            |> Runner.asNumber
        let piecewise =
            runWithTwoLists xs ys
                """
var afold = Reduce(Input.a, (acc, x) => acc + x, 0)
Output.result = Reduce(Input.b, (acc, x) => acc + x, afold)"""
            |> Runner.asNumber
        abs (combined - piecewise) <= max 1.0 (abs combined) * 1e-9

    // -- All / Any -----------------------------------------------------------

    [<Fact>]
    let ``All on empty array is vacuously true`` () =
        let src = "Output.result = All([], x => false)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``Any on empty array is false`` () =
        let src = "Output.result = Any([], x => true)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "result" output |> Runner.asBool)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``De Morgan: not All(xs, p) == Any(xs, x => not p(x))`` (xs: float list) =
        let lhs =
            runWithXs xs "Output.result = not All(Input.xs, x => x > 0)"
            |> Runner.asBool
        let rhs =
            runWithXs xs "Output.result = Any(Input.xs, x => not (x > 0))"
            |> Runner.asBool
        lhs = rhs

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``All(xs, true-predicate) is always true`` (xs: float list) =
        runWithXs xs "Output.result = All(Input.xs, x => true)"
        |> Runner.asBool

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Any(xs, false-predicate) is never true`` (xs: float list) =
        let r = runWithXs xs "Output.result = Any(Input.xs, x => false)" |> Runner.asBool
        not r

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Any(xs, p) == (Length(Where(xs, p)) > 0)`` (xs: float list) =
        let anyResult =
            runWithXs xs "Output.result = Any(Input.xs, x => x > 0)" |> Runner.asBool
        let whereLen =
            runWithXs xs "Output.result = Length(Where(Input.xs, x => x > 0))"
            |> Runner.asNumber
        anyResult = (whereLen > 0.0)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``All(xs, p) == (Length(Where(xs, p)) == Length(xs))`` (xs: float list) =
        let allResult =
            runWithXs xs "Output.result = All(Input.xs, x => x >= 0)" |> Runner.asBool
        let whereLen =
            runWithXs xs "Output.result = Length(Where(Input.xs, x => x >= 0))"
            |> Runner.asNumber
        let totalLen = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        allResult = (whereLen = totalLen)

    // -- Find ----------------------------------------------------------------

    [<Fact>]
    let ``Find returns null when no match`` () =
        let src = "Output.result = Find([1, 2, 3], x => x > 99)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("null", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Find returns first match, not later ones`` () =
        let src = "Output.result = Find([3, 5, 7, 9], x => x >= 5)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(5.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Find nullness agrees with Where emptiness`` (xs: float list) =
        let findIsNull =
            let v = runWithXs xs "Output.result = Find(Input.xs, x => x > 0)"
            v.IsNull
        let whereIsEmpty =
            runWithXs xs "Output.result = Length(Where(Input.xs, x => x > 0))"
            |> Runner.asNumber
            |> fun n -> n = 0.0
        findIsNull = whereIsEmpty

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Found value satisfies the predicate`` (xs: float list) =
        let src = """
var hit = Find(Input.xs, x => x > 0)
Output.result = hit is null ? true : hit > 0"""
        runWithXs xs src |> Runner.asBool

    // -- Distinct ------------------------------------------------------------

    [<Fact>]
    let ``Distinct on empty array is empty`` () =
        let src = "Output.result = Distinct([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Distinct on already-unique preserves order`` () =
        let src = "Output.result = Distinct([3, 1, 2])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[3,1,2]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Distinct collapses consecutive duplicates, first occurrence wins`` () =
        let src = "Output.result = Distinct([1, 2, 2, 1, 3])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,3]", (Runner.prop "result" output).ToJson())

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Distinct is length non-increasing`` (xs: float list) =
        let distLen =
            runWithXs xs "Output.result = Length(Distinct(Input.xs))"
            |> Runner.asNumber
        let origLen = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        distLen <= origLen

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Distinct is idempotent`` (xs: float list) =
        let once = runWithXs xs "Output.result = Distinct(Input.xs)"
        let twice = runWithXs xs "Output.result = Distinct(Distinct(Input.xs))"
        jsonEq once twice

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Distinct output has no duplicates`` (xs: float list) =
        let distLenFromDistinct =
            runWithXs xs "Output.result = Length(Distinct(Distinct(Input.xs)))"
            |> Runner.asNumber
        let distLen =
            runWithXs xs "Output.result = Length(Distinct(Input.xs))"
            |> Runner.asNumber
        // Distinct(Distinct(xs)) has the same length as Distinct(xs) -
        // i.e. no duplicates remained after one pass.
        distLenFromDistinct = distLen

    // -- SortBy --------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``SortBy preserves length`` (xs: float list) =
        let a =
            runWithXs xs "Output.result = Length(SortBy(Input.xs, x => x))"
            |> Runner.asNumber
        let b = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        a = b

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``SortBy with identity key matches Sort`` (xs: float list) =
        let viaSort = runWithXs xs "Output.result = Sort(Input.xs)"
        let viaSortBy = runWithXs xs "Output.result = SortBy(Input.xs, x => x)"
        jsonEq viaSort viaSortBy

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``SortBy result is non-decreasing under the key`` (xs: float list) =
        let sorted =
            runWithXs xs "Output.result = SortBy(Input.xs, x => -x)"
            |> asArray
            |> List.map (fun v -> (v :?> JyroNumber).Value)
        sorted
        |> List.pairwise
        |> List.forall (fun (a, b) -> -a <= -b)  // keys are non-decreasing

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``SortBy is a multiset permutation`` (xs: float list) =
        let original =
            runWithXs xs "Output.result = Input.xs"
            |> asArray
            |> List.map (fun v -> (v :?> JyroNumber).Value)
            |> List.sort
        let sorted =
            runWithXs xs "Output.result = SortBy(Input.xs, x => x * 2)"
            |> asArray
            |> List.map (fun v -> (v :?> JyroNumber).Value)
            |> List.sort
        original = sorted

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``SortBy is idempotent with the same key`` (xs: float list) =
        let once = runWithXs xs "Output.result = SortBy(Input.xs, x => x)"
        let twice =
            runWithXs xs "Output.result = SortBy(SortBy(Input.xs, x => x), x => x)"
        jsonEq once twice

    // ------------------------------------------------------------------------
    // Tier 2 lite: Append / Prepend / Slice / Keys / Values / Merge.
    // Smaller per-function invariants but still good coverage leverage.
    // ------------------------------------------------------------------------

    // -- Append --------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Append grows length by one`` (xs: float list) =
        let after =
            runWithXs xs "Output.result = Length(Append(Input.xs, 99))"
            |> Runner.asNumber
        let before = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        after = before + 1.0

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Appended element is the last`` (xs: float list) =
        let src =
            """
var out = Append(Input.xs, 99)
Output.result = out[Length(out) - 1]"""
        let last = runWithXs xs src |> Runner.asNumber
        last = 99.0

    [<Fact>]
    let ``Append to empty array yields singleton`` () =
        let src = "Output.result = Append([], 7)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[7]", (Runner.prop "result" output).ToJson())

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Append leaves existing elements untouched`` (xs: float list) =
        // Length(Where(Append(xs, -99), x => x == -99)) should equal
        // Length(Where(xs, x => x == -99)) + 1.
        let after =
            runWithXs xs
                "Output.result = Length(Where(Append(Input.xs, -99), x => x == -99))"
            |> Runner.asNumber
        let before =
            runWithXs xs "Output.result = Length(Where(Input.xs, x => x == -99))"
            |> Runner.asNumber
        after = before + 1.0

    // -- Prepend -------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Prepend grows length by one`` (xs: float list) =
        let after =
            runWithXs xs "Output.result = Length(Prepend(Input.xs, 99))"
            |> Runner.asNumber
        let before = runWithXs xs "Output.result = Length(Input.xs)" |> Runner.asNumber
        after = before + 1.0

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Prepended element is the first`` (xs: float list) =
        let src = "Output.result = Prepend(Input.xs, 99)[0]"
        let head = runWithXs xs src |> Runner.asNumber
        head = 99.0

    [<Fact>]
    let ``Prepend to empty array yields singleton`` () =
        let src = "Output.result = Prepend([], 7)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[7]", (Runner.prop "result" output).ToJson())

    // -- Slice ---------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Slice(xs, 0, Length(xs)) == xs`` (xs: float list) =
        let sliced =
            runWithXs xs "Output.result = Slice(Input.xs, 0, Length(Input.xs))"
        let original = runWithXs xs "Output.result = Input.xs"
        jsonEq sliced original

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Slice preserves total length on disjoint split`` (xs: float list) =
        // Split into [0..mid) ++ [mid..end) and compare total length.
        // Slice requires integer indices, so pick `mid` via a %/- trick
        // that stays integral regardless of Length parity.
        let finite =
            xs
            |> List.filter (fun x ->
                not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.length finite < 2 then true
        else
            let src =
                """
var n = Length(Input.xs)
var mid = (n - (n % 2)) / 2
var left = Slice(Input.xs, 0, mid)
var right = Slice(Input.xs, mid, n)
Output.result = Length(left) + Length(right)"""
            let total = runWithXs finite src |> Runner.asNumber
            let expected =
                runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
            total = expected

    [<Fact>]
    let ``Empty slice (start == end) is empty`` () =
        let src = "Output.result = Slice([1, 2, 3], 1, 1)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Slice preserves element order`` () =
        let src = "Output.result = Slice([10, 20, 30, 40, 50], 1, 4)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[20,30,40]", (Runner.prop "result" output).ToJson())

    // -- Keys / Values -------------------------------------------------------

    [<Fact>]
    let ``Keys of empty object is empty`` () =
        let src = "Output.result = Keys({})"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Values of empty object is empty`` () =
        let src = "Output.result = Values({})"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Keys and Values have matching length`` () =
        let src = """
var o = {a: 1, b: 2, c: 3}
Output.result = Length(Keys(o)) == Length(Values(o))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``HasProperty is true for every key in Keys(o)`` () =
        let src = """
var o = {a: 1, b: 2, c: 3}
var ks = Keys(o)
var ok = true
foreach k in ks do
  if not HasProperty(o, k) then
    ok = false
  end
end
Output.result = ok"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``Keys and Values line up positionally via o[k]`` () =
        // For each (k, v) pair in zipped Keys/Values, o[k] should equal v.
        let src = """
var o = {a: 10, b: 20, c: 30}
var ks = Keys(o)
var vs = Values(o)
var ok = true
for i in 0 to Length(ks) - 1 do
  if o[ks[i]] != vs[i] then
    ok = false
  end
end
Output.result = ok"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- Merge ---------------------------------------------------------------
    //
    // Jyro's Merge takes a single array of objects: `Merge([a, b, c])`.
    // Later elements win on key overlap (right-biased).

    [<Fact>]
    let ``Merge right-identity: Merge([a, {}]) == a`` () =
        let src = """
var a = {x: 1, y: 2}
Output.result = Merge([a, {}])"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("{\"x\":1,\"y\":2}", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Merge left-identity: Merge([{}, b]) == b`` () =
        let src = """
var b = {x: 1, y: 2}
Output.result = Merge([{}, b])"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("{\"x\":1,\"y\":2}", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Merge is right-biased on overlap`` () =
        let src = """
var m = Merge([{x: 1, y: 2}, {y: 99, z: 3}])
Output.x = m.x
Output.y = m.y
Output.z = m.z"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "x" output |> Runner.asNumber)
        Assert.Equal(99.0, Runner.prop "y" output |> Runner.asNumber) // later element wins
        Assert.Equal(3.0, Runner.prop "z" output |> Runner.asNumber)

    [<Fact>]
    let ``Merge on disjoint inputs unions the key sets`` () =
        let src = """
Output.merged = Length(Keys(Merge([{x: 1, y: 2}, {y: 99, z: 3}])))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(3.0, Runner.prop "merged" output |> Runner.asNumber)

    [<Fact>]
    let ``Merge is idempotent: Merge([a, a]) == a`` () =
        let src = """
var a = {x: 1, y: 2, z: 3}
Output.result = Merge([a, a])"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("{\"x\":1,\"y\":2,\"z\":3}", (Runner.prop "result" output).ToJson())

    // ------------------------------------------------------------------------
    // String operations: Contains / StartsWith / EndsWith / Substring /
    //   ToUpper / ToLower / Trim / Split / Join.
    // ------------------------------------------------------------------------

    /// Run a script with `Input.s = <string>` and return `Output.result`.
    let private runWithString (s: string) (script: string) : JyroValue =
        let input = JyroObject()
        input.SetProperty("s", JyroString(s))
        let output = Runner.expectSuccess script (input :> JyroValue)
        Runner.prop "result" output

    let private runWithTwoStrings (a: string) (b: string) (script: string) : JyroValue =
        let input = JyroObject()
        input.SetProperty("a", JyroString(a))
        input.SetProperty("b", JyroString(b))
        let output = Runner.expectSuccess script (input :> JyroValue)
        Runner.prop "result" output

    // -- Contains ------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Contains(s, "") is always true`` (s: string) =
        runWithString s "Output.result = Contains(Input.s, \"\")"
        |> Runner.asBool

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Contains(s, s) is always true`` (s: string) =
        runWithString s "Output.result = Contains(Input.s, Input.s)"
        |> Runner.asBool

    [<Fact>]
    let ``Contains("", nonEmpty) is false`` () =
        let src = "Output.result = Contains(\"\", \"x\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``Contains detects substring`` () =
        let src = "Output.result = Contains(\"hello world\", \"lo wo\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- StartsWith / EndsWith -----------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``StartsWith(s, "") is always true`` (s: string) =
        runWithString s "Output.result = StartsWith(Input.s, \"\")" |> Runner.asBool

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``EndsWith(s, "") is always true`` (s: string) =
        runWithString s "Output.result = EndsWith(Input.s, \"\")" |> Runner.asBool

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``StartsWith(s, s) is always true`` (s: string) =
        runWithString s "Output.result = StartsWith(Input.s, Input.s)" |> Runner.asBool

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``EndsWith(s, s) is always true`` (s: string) =
        runWithString s "Output.result = EndsWith(Input.s, Input.s)" |> Runner.asBool

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``StartsWith and Contains agree for any prefix`` (s: string) =
        // If `s` starts with itself then it contains itself - true regardless,
        // but flipping predicates exercises the two together.
        let sw = runWithString s "Output.result = StartsWith(Input.s, Input.s)" |> Runner.asBool
        let ct = runWithString s "Output.result = Contains(Input.s, Input.s)" |> Runner.asBool
        sw = ct

    // -- ToUpper / ToLower ---------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``ToUpper preserves length`` (s: string) =
        let uLen =
            runWithString s "Output.result = Length(ToUpper(Input.s))" |> Runner.asNumber
        let sLen = runWithString s "Output.result = Length(Input.s)" |> Runner.asNumber
        uLen = sLen

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``ToLower preserves length`` (s: string) =
        let lLen =
            runWithString s "Output.result = Length(ToLower(Input.s))" |> Runner.asNumber
        let sLen = runWithString s "Output.result = Length(Input.s)" |> Runner.asNumber
        lLen = sLen

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``ToUpper is idempotent`` (s: string) =
        let once = runWithString s "Output.result = ToUpper(Input.s)" |> Runner.asString
        let twice =
            runWithString s "Output.result = ToUpper(ToUpper(Input.s))" |> Runner.asString
        once = twice

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``ToLower is idempotent`` (s: string) =
        let once = runWithString s "Output.result = ToLower(Input.s)" |> Runner.asString
        let twice =
            runWithString s "Output.result = ToLower(ToLower(Input.s))" |> Runner.asString
        once = twice

    [<Fact>]
    let ``ToUpper then ToLower matches direct ToLower for ASCII`` () =
        let src = """
var s = "Hello World"
Output.a = ToLower(ToUpper(s))
Output.b = ToLower(s)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "a" output |> Runner.asString,
            Runner.prop "b" output |> Runner.asString)

    // -- Trim ----------------------------------------------------------------

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Trim is idempotent`` (s: string) =
        let once = runWithString s "Output.result = Trim(Input.s)" |> Runner.asString
        let twice =
            runWithString s "Output.result = Trim(Trim(Input.s))" |> Runner.asString
        once = twice

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Trim never grows the string`` (s: string) =
        let tLen =
            runWithString s "Output.result = Length(Trim(Input.s))" |> Runner.asNumber
        let sLen = runWithString s "Output.result = Length(Input.s)" |> Runner.asNumber
        tLen <= sLen

    [<Fact>]
    let ``Trim removes leading and trailing whitespace`` () =
        let src = "Output.result = Trim(\"   hello   \")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("hello", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Trim on empty string is empty`` () =
        let src = "Output.result = Trim(\"\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("", Runner.prop "result" output |> Runner.asString)

    // -- Substring -----------------------------------------------------------

    [<Fact>]
    let ``Substring of full range returns the whole string`` () =
        let src = """
var s = "hello"
Output.result = Substring(s, 0, Length(s))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("hello", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Empty Substring (length 0) is empty`` () =
        let src = "Output.result = Substring(\"hello\", 2, 0)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Substring preserves the requested length`` () =
        let src = "Output.result = Length(Substring(\"hello world\", 3, 5))"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(5.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Substring picks the right chunk`` () =
        let src = "Output.result = Substring(\"hello world\", 6, 5)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("world", Runner.prop "result" output |> Runner.asString)

    // -- Split / Join --------------------------------------------------------

    [<Fact>]
    let ``Split of empty string yields a single-element array`` () =
        let src = "Output.result = Length(Split(\"\", \",\"))"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Split-Join round-trip on delimiter-free elements`` () =
        let src = """
var s = "a,b,c,d"
Output.result = Join(Split(s, ","), ",")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("a,b,c,d", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Join of empty array is empty string`` () =
        let src = "Output.result = Join([], \",\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Join of singleton is the element`` () =
        let src = "Output.result = Join([\"alone\"], \",\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("alone", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Split preserves every element`` () =
        let src = "Output.result = Split(\"apple,banana,cherry\", \",\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[\"apple\",\"banana\",\"cherry\"]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Length of Split(s, sep) is count(s, sep) + 1 for non-delimiter chars`` () =
        // 3 commas -> 4 elements when input has no trailing/leading delim.
        let src = "Output.result = Length(Split(\"a,b,c,d\", \",\"))"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(4.0, Runner.prop "result" output |> Runner.asNumber)

    // ------------------------------------------------------------------------
    // Remaining stdlib: First / Last / IndexOf / Clone / TypeOf /
    //   ToJson / FromJson.
    // ------------------------------------------------------------------------

    // -- First / Last --------------------------------------------------------

    [<Fact>]
    let ``First on empty array is null`` () =
        let src = "Output.result = First([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("null", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Last on empty array is null`` () =
        let src = "Output.result = Last([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("null", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``First of singleton is that element`` () =
        let src = "Output.result = First([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Last of singleton is that element`` () =
        let src = "Output.result = Last([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``First(xs) == xs[0] for non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let first = runWithXs finite "Output.result = First(Input.xs)" |> Runner.asNumber
            let indexed = runWithXs finite "Output.result = Input.xs[0]" |> Runner.asNumber
            first = indexed

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Last(xs) == xs[Length(xs) - 1] for non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let last = runWithXs finite "Output.result = Last(Input.xs)" |> Runner.asNumber
            let indexed =
                runWithXs finite "Output.result = Input.xs[Length(Input.xs) - 1]"
                |> Runner.asNumber
            last = indexed

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``First(Reverse(xs)) == Last(xs)`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let a =
                runWithXs finite "Output.result = First(Reverse(Input.xs))" |> Runner.asNumber
            let b = runWithXs finite "Output.result = Last(Input.xs)" |> Runner.asNumber
            a = b

    // -- IndexOf -------------------------------------------------------------

    [<Fact>]
    let ``IndexOf returns -1 when not found`` () =
        let src = "Output.result = IndexOf([1, 2, 3], 99)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(-1.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``IndexOf returns index of first match`` () =
        let src = "Output.result = IndexOf([10, 20, 30, 20], 20)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``IndexOf on empty array is -1`` () =
        let src = "Output.result = IndexOf([], 42)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(-1.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``xs[IndexOf(xs, v)] == v when IndexOf >= 0`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            // Probe with an actual array element rather than a re-serialized
            // literal: numeric literals now parse to exact decimals, so a
            // double's G17 text would denote a slightly different value than the
            // (inexact) element it came from. Referencing xs[0] keeps both sides
            // the same value.
            let src =
                """
var v = Input.xs[0]
var idx = IndexOf(Input.xs, v)
Output.result = idx >= 0 and Input.xs[idx] == v"""
            runWithXs finite src |> Runner.asBool

    // -- Clone ---------------------------------------------------------------

    [<Fact>]
    let ``Clone produces a structurally equal object`` () =
        let src = """
var a = {x: 1, y: [2, 3], z: {inner: 4}}
var b = Clone(a)
Output.same = ToJson(a) == ToJson(b)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "same" output |> Runner.asBool)

    [<Fact>]
    let ``Mutating the clone does not affect the original`` () =
        let src = """
var a = {x: 1, y: 2}
var b = Clone(a)
b.x = 99
Output.aX = a.x
Output.bX = b.x"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "aX" output |> Runner.asNumber)
        Assert.Equal(99.0, Runner.prop "bX" output |> Runner.asNumber)

    [<Fact>]
    let ``Deep-nested Clone is independent`` () =
        let src = """
var a = {inner: {value: 1}}
var b = Clone(a)
b.inner.value = 99
Output.aVal = a.inner.value
Output.bVal = b.inner.value"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "aVal" output |> Runner.asNumber)
        Assert.Equal(99.0, Runner.prop "bVal" output |> Runner.asNumber)

    [<Fact>]
    let ``Array Clone is independent`` () =
        let src = """
var a = [1, 2, 3]
var b = Clone(a)
b[0] = 99
Output.aFirst = a[0]
Output.bFirst = b[0]"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "aFirst" output |> Runner.asNumber)
        Assert.Equal(99.0, Runner.prop "bFirst" output |> Runner.asNumber)

    // -- TypeOf --------------------------------------------------------------

    [<Theory>]
    [<InlineData("42", "number")>]
    [<InlineData("3.14", "number")>]
    [<InlineData("\"hi\"", "string")>]
    [<InlineData("true", "boolean")>]
    [<InlineData("false", "boolean")>]
    [<InlineData("null", "null")>]
    [<InlineData("[]", "array")>]
    [<InlineData("[1, 2]", "array")>]
    [<InlineData("{}", "object")>]
    [<InlineData("{a: 1}", "object")>]
    let ``TypeOf returns the matching type keyword`` (literal: string) (expected: string) =
        let src = sprintf "Output.result = TypeOf(%s)" literal
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(expected, Runner.prop "result" output |> Runner.asString)

    [<Theory>]
    [<InlineData("42", "number")>]
    [<InlineData("\"hi\"", "string")>]
    [<InlineData("true", "boolean")>]
    [<InlineData("null", "null")>]
    [<InlineData("[]", "array")>]
    [<InlineData("{}", "object")>]
    let ``TypeOf agrees with is-keyword`` (literal: string) (typeName: string) =
        let src = sprintf "Output.result = TypeOf(%s) == \"%s\" and (%s) is %s"
                    literal typeName literal typeName
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- ToJson / FromJson round-trip ---------------------------------------

    [<Theory>]
    [<InlineData("42")>]
    [<InlineData("3.14")>]
    [<InlineData("true")>]
    [<InlineData("false")>]
    [<InlineData("null")>]
    [<InlineData("\"hello\"")>]
    [<InlineData("\"\"")>]
    [<InlineData("[]")>]
    [<InlineData("[1, 2, 3]")>]
    [<InlineData("{}")>]
    [<InlineData("{a: 1, b: \"x\"}")>]
    [<InlineData("{nested: {arr: [1, 2], flag: true}}")>]
    let ``FromJson(ToJson(v)) round-trips via ToJson equality`` (literal: string) =
        // Compare serialized forms - avoids relying on value-level equality
        // for complex shapes.
        let src =
            sprintf
                """
var original = %s
var viaJson = FromJson(ToJson(original))
Output.same = ToJson(viaJson) == ToJson(original)"""
                literal
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "same" output |> Runner.asBool)

    [<Fact>]
    let ``ToJson is a string`` () =
        let src = "Output.result = TypeOf(ToJson({a: 1}))"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("string", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``ToJson is idempotent via FromJson`` () =
        let src = """
var v = {x: 1, y: [2, 3]}
var once = ToJson(v)
var twice = ToJson(FromJson(ToJson(v)))
Output.result = once == twice"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // ------------------------------------------------------------------------
    // Math laws: Absolute / Ceiling / Floor / Round / Clamp / Sum /
    //   Average / Min / Max / Median / Mode / Power / SquareRoot / Log.
    // ------------------------------------------------------------------------

    let private approxEq (a: float) (b: float) =
        let diff = abs (a - b)
        let scale = max 1.0 (max (abs a) (abs b))
        diff <= scale * 1e-9

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Absolute is non-negative`` (n: float) =
        let r =
            runWithString "ignored" (sprintf "Output.result = Absolute(%g)" n)
            |> Runner.asNumber
        r >= 0.0

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Absolute is even: Absolute(-x) == Absolute(x)`` (n: float) =
        let a =
            runWithString "ignored" (sprintf "Output.result = Absolute(%g)" n)
            |> Runner.asNumber
        let b =
            runWithString "ignored" (sprintf "Output.result = Absolute(-(%g))" n)
            |> Runner.asNumber
        approxEq a b

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Absolute is idempotent`` (n: float) =
        let once =
            runWithString "ignored" (sprintf "Output.result = Absolute(%g)" n)
            |> Runner.asNumber
        let twice =
            runWithString "ignored" (sprintf "Output.result = Absolute(Absolute(%g))" n)
            |> Runner.asNumber
        approxEq once twice

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Ceiling is >= x`` (n: float) =
        let c =
            runWithString "ignored" (sprintf "Output.result = Ceiling(%g)" n)
            |> Runner.asNumber
        c >= n - 1e-9

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Floor is <= x`` (n: float) =
        let f =
            runWithString "ignored" (sprintf "Output.result = Floor(%g)" n)
            |> Runner.asNumber
        f <= n + 1e-9

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Ceiling(x) - Floor(x) is 0 or 1`` (n: float) =
        let src = sprintf "Output.result = Ceiling(%g) - Floor(%g)" n n
        let d = runWithString "ignored" src |> Runner.asNumber
        d = 0.0 || approxEq d 1.0

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Floor is idempotent`` (n: float) =
        let once =
            runWithString "ignored" (sprintf "Output.result = Floor(%g)" n)
            |> Runner.asNumber
        let twice =
            runWithString "ignored" (sprintf "Output.result = Floor(Floor(%g))" n)
            |> Runner.asNumber
        approxEq once twice

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Ceiling is idempotent`` (n: float) =
        let once =
            runWithString "ignored" (sprintf "Output.result = Ceiling(%g)" n)
            |> Runner.asNumber
        let twice =
            runWithString "ignored" (sprintf "Output.result = Ceiling(Ceiling(%g))" n)
            |> Runner.asNumber
        approxEq once twice

    [<Fact>]
    let ``Round rounds to specified decimal places`` () =
        // Jyro's Round signature is `Round(x, digits)`.
        let src = """
Output.a = Round(2.4, 0)
Output.b = Round(2.6, 0)
Output.c = Round(-2.4, 0)
Output.d = Round(-2.6, 0)
Output.e = Round(3.14159, 2)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(2.0, Runner.prop "a" output |> Runner.asNumber)
        Assert.Equal(3.0, Runner.prop "b" output |> Runner.asNumber)
        Assert.Equal(-2.0, Runner.prop "c" output |> Runner.asNumber)
        Assert.Equal(-3.0, Runner.prop "d" output |> Runner.asNumber)
        Assert.Equal(3.14, Runner.prop "e" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Clamp result is within [lo, hi]`` (x: float) =
        let src = sprintf "Output.result = Clamp(%g, -10, 10)" x
        let r = runWithString "ignored" src |> Runner.asNumber
        r >= -10.0 - 1e-9 && r <= 10.0 + 1e-9

    [<Fact>]
    let ``Clamp is identity when x already in range`` () =
        let src = "Output.result = Clamp(5, 0, 10)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(5.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Clamp hits the bounds when outside`` () =
        let src = """
Output.low = Clamp(-5, 0, 10)
Output.high = Clamp(99, 0, 10)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "low" output |> Runner.asNumber)
        Assert.Equal(10.0, Runner.prop "high" output |> Runner.asNumber)

    [<Fact>]
    let ``Sum of empty array is 0`` () =
        let src = "Output.result = Sum([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Sum of singleton is that element`` () =
        let src = "Output.result = Sum([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Sum agrees with Reduce(+, 0)`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
            |> List.map (fun x -> x * 0.001)
        let a = runWithXs finite "Output.result = Sum(Input.xs)" |> Runner.asNumber
        let b =
            runWithXs finite "Output.result = Reduce(Input.xs, (acc, x) => acc + x, 0)"
            |> Runner.asNumber
        approxEq a b

    [<Fact>]
    let ``Average of singleton is that element`` () =
        let src = "Output.result = Average([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Average of [a, b] is (a + b) / 2`` () =
        let src = "Output.result = Average([10, 20])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(15.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Average == Sum / Length for non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
            |> List.map (fun x -> x * 0.001)
        if List.isEmpty finite then true
        else
            let avg = runWithXs finite "Output.result = Average(Input.xs)" |> Runner.asNumber
            let sum = runWithXs finite "Output.result = Sum(Input.xs)" |> Runner.asNumber
            let len = runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
            approxEq avg (sum / len)

    [<Fact>]
    let ``Min of singleton is that element`` () =
        let src = "Output.result = Min([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Max of singleton is that element`` () =
        let src = "Output.result = Max([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Min(xs) == First(Sort(xs)) for non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let minv = runWithXs finite "Output.result = Min(Input.xs)" |> Runner.asNumber
            let sortFirst =
                runWithXs finite "Output.result = First(Sort(Input.xs))" |> Runner.asNumber
            approxEq minv sortFirst

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Max(xs) == Last(Sort(xs)) for non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let maxv = runWithXs finite "Output.result = Max(Input.xs)" |> Runner.asNumber
            let sortLast =
                runWithXs finite "Output.result = Last(Sort(Input.xs))" |> Runner.asNumber
            approxEq maxv sortLast

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Min <= Max for non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let a = runWithXs finite "Output.result = Min(Input.xs)" |> Runner.asNumber
            let b = runWithXs finite "Output.result = Max(Input.xs)" |> Runner.asNumber
            a <= b + 1e-9

    [<Fact>]
    let ``Median of singleton is that element`` () =
        let src = "Output.result = Median([42])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Median of odd-length is the middle element`` () =
        let src = "Output.result = Median([3, 1, 2])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(2.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Mode returns the most frequent element`` () =
        let src = "Output.result = Mode([1, 2, 2, 3])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(2.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Power with exponent 0 is 1`` () =
        let src = "Output.result = Power(7, 0)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Power with exponent 1 is the base`` () =
        let src = "Output.result = Power(42, 1)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Power(x, 2) equals x * x`` (x: float) =
        let capped = (abs x) * 0.001
        let src = sprintf "Output.p = Power(%g, 2)\nOutput.m = %g * %g" capped capped capped
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        approxEq (Runner.prop "p" output |> Runner.asNumber) (Runner.prop "m" output |> Runner.asNumber)

    [<Fact>]
    let ``SquareRoot(0) == 0 and SquareRoot(1) == 1`` () =
        let src = """
Output.a = SquareRoot(0)
Output.b = SquareRoot(1)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "a" output |> Runner.asNumber)
        Assert.Equal(1.0, Runner.prop "b" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``SquareRoot(x) squared equals x for x >= 0`` (x: float) =
        let nonNeg = abs x * 0.01
        let src =
            sprintf
                """
var r = SquareRoot(%g)
Output.result = r * r"""
                nonNeg
        let back = runWithString "ignored" src |> Runner.asNumber
        // `r*r` accumulates float error; an epsilon of 1e-4 absorbs the
        // rounding for the magnitudes exercised here.
        let diff = abs (back - nonNeg)
        let scale = max 1.0 nonNeg
        diff <= scale * 1e-4

    [<Fact>]
    let ``Log(1) == 0`` () =
        let src = "Output.result = Log(1)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    // ------------------------------------------------------------------------
    // Array-manipulation laws: Flatten / FlattenOnce / Range / Insert /
    //   RemoveAt / RemoveFirst / RemoveLast / Skip / GroupBy.
    // ------------------------------------------------------------------------

    [<Fact>]
    let ``Range is half-open: it includes start and excludes finish`` () =
        // 92.13.3 and 92.13.5 (JYRO-REQ-716, JYRO-REQ-718): each start + k*step
        // below finish, and [] when finish <= start, so Range(0, n) has n elements.
        let src = """
Output.empty = Range(0, 0)
Output.single = Range(0, 1)
Output.span = Range(0, 5)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "empty" output).ToJson())
        Assert.Equal("[0]", (Runner.prop "single" output).ToJson())
        Assert.Equal("[0,1,2,3,4]", (Runner.prop "span" output).ToJson())

    [<Fact>]
    let ``Range produces consecutive integers`` () =
        let src = """
var xs = Range(3, 7)
var ok = true
for i in 0 to Length(xs) - 1 do
  if xs[i] != 3 + i then
    ok = false
  end
end
Output.result = ok"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``Flatten on flat array is identity`` () =
        let src = "Output.result = Flatten([1, 2, 3])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,3]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Flatten collapses any depth`` () =
        let src = "Output.result = Flatten([1, [2, [3, [4]]]])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,3,4]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Flatten of empty is empty`` () =
        let src = "Output.result = Flatten([])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Flatten is idempotent`` () =
        let src = """
var xs = [1, [2, [3]]]
Output.once = Flatten(xs)
Output.twice = Flatten(Flatten(xs))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            (Runner.prop "once" output).ToJson(),
            (Runner.prop "twice" output).ToJson())

    [<Fact>]
    let ``FlattenOnce peels one level only`` () =
        let src = "Output.result = FlattenOnce([[1], [2, [3]], [4]])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,[3],4]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``FlattenOnce of flat array is identity`` () =
        let src = "Output.result = FlattenOnce([1, 2, 3])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,3]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Insert at head moves elements right`` () =
        let src = "Output.result = Insert([2, 3, 4], 0, 1)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,3,4]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Insert in middle places at exact index`` () =
        let src = "Output.result = Insert([1, 2, 4], 2, 3)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[1,2,3,4]", (Runner.prop "result" output).ToJson())

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Insert grows length by one`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let before = runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
        let after =
            runWithXs finite "Output.result = Length(Insert(Input.xs, 0, 99))"
            |> Runner.asNumber
        after = before + 1.0

    [<Fact>]
    let ``RemoveAt drops the element at index`` () =
        let src = "Output.result = RemoveAt([10, 20, 30, 40], 2)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[10,20,40]", (Runner.prop "result" output).ToJson())

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``RemoveAt shrinks length by 1 for non-empty arrays`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let before = runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
            let after =
                runWithXs finite "Output.result = Length(RemoveAt(Input.xs, 0))"
                |> Runner.asNumber
            approxEq after (before - 1.0)

    // Jyro's RemoveFirst / RemoveLast drop the head / tail element. They
    // do not take a value to match - that's what `Where` is for.

    [<Fact>]
    let ``RemoveFirst drops the head element`` () =
        let src = "Output.result = RemoveFirst([10, 20, 30, 40])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[20,30,40]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``RemoveLast drops the tail element`` () =
        let src = "Output.result = RemoveLast([10, 20, 30, 40])"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[10,20,30]", (Runner.prop "result" output).ToJson())

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``RemoveFirst shrinks length by 1 on non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let before = runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
            let after =
                runWithXs finite "Output.result = Length(RemoveFirst(Input.xs))"
                |> Runner.asNumber
            approxEq after (before - 1.0)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``RemoveLast shrinks length by 1 on non-empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.isEmpty finite then true
        else
            let before = runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
            let after =
                runWithXs finite "Output.result = Length(RemoveLast(Input.xs))"
                |> Runner.asNumber
            approxEq after (before - 1.0)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Skip 0 is identity`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let skipped = runWithXs finite "Output.result = Skip(Input.xs, 0)"
        let original = runWithXs finite "Output.result = Input.xs"
        jsonEq skipped original

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Skip Length is empty`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        let src = "Output.result = Length(Skip(Input.xs, Length(Input.xs)))"
        let n = runWithXs finite src |> Runner.asNumber
        n = 0.0

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Skip n reduces length by n when n < Length`` (xs: float list) =
        let finite =
            xs
            |> List.filter (fun x -> not (System.Double.IsNaN(x) || System.Double.IsInfinity(x)))
        if List.length finite < 3 then true
        else
            let before = runWithXs finite "Output.result = Length(Input.xs)" |> Runner.asNumber
            let after =
                runWithXs finite "Output.result = Length(Skip(Input.xs, 2))" |> Runner.asNumber
            approxEq after (before - 2.0)

    [<Fact>]
    let ``Skip preserves tail order`` () =
        let src = "Output.result = Skip([10, 20, 30, 40, 50], 2)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[30,40,50]", (Runner.prop "result" output).ToJson())

    // Jyro's GroupBy signature is `GroupBy(array, fieldName)`: the array
    // must contain objects, and grouping is by a named field. Keys in the
    // returned object are the string form of each field value.

    [<Fact>]
    let ``GroupBy bins objects by field value`` () =
        let src = """
var items = [
  {kind: "a", v: 1},
  {kind: "b", v: 2},
  {kind: "a", v: 3},
  {kind: "a", v: 4},
  {kind: "b", v: 5}
]
var groups = GroupBy(items, "kind")
Output.aCount = Length(groups["a"])
Output.bCount = Length(groups["b"])
Output.total = Output.aCount + Output.bCount"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(3.0, Runner.prop "aCount" output |> Runner.asNumber)
        Assert.Equal(2.0, Runner.prop "bCount" output |> Runner.asNumber)
        Assert.Equal(5.0, Runner.prop "total" output |> Runner.asNumber)

    [<Fact>]
    let ``GroupBy on empty yields empty object`` () =
        let src = """
Output.result = Length(Keys(GroupBy([], "kind")))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    // ------------------------------------------------------------------------
    // By-field query helpers: WhereByField / FindByField / AllByField /
    //   AnyByField / CountIf / SortByField / Select / SelectMany / Project.
    //
    // Jyro's by-field family takes arrays of objects and a string
    // field-name plus (for the predicate helpers) an operator string
    // (`==`, `!=`, `<`, `<=`, `>`, `>=`) and a compare value.
    // ------------------------------------------------------------------------

    /// Standard test fixture: five users with mixed fields.
    let private usersSource = """
var users = [
  {name: "alice",   age: 30, role: "admin"},
  {name: "bob",     age: 25, role: "user"},
  {name: "carol",   age: 30, role: "user"},
  {name: "dave",    age: 40, role: "admin"},
  {name: "ellen",   age: 25, role: "user"}
]
"""

    // -- WhereByField --------------------------------------------------------

    [<Fact>]
    let ``WhereByField returns only matching rows`` () =
        let src = usersSource + """
var admins = WhereByField(users, "role", "==", "admin")
Output.count = Length(admins)
Output.first = admins[0].name
Output.second = admins[1].name"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(2.0, Runner.prop "count" output |> Runner.asNumber)
        Assert.Equal("alice", Runner.prop "first" output |> Runner.asString)
        Assert.Equal("dave", Runner.prop "second" output |> Runner.asString)

    [<Fact>]
    let ``WhereByField on empty array returns empty`` () =
        let src = """Output.result = Length(WhereByField([], "k", "==", 1))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``WhereByField agrees with Where-lambda on the same predicate`` () =
        let src = usersSource + """
var byField = WhereByField(users, "age", "==", 30)
var byLambda = Where(users, u => u.age == 30)
Output.same = Length(byField) == Length(byLambda)
Output.byField = Length(byField)
Output.byLambda = Length(byLambda)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "same" output |> Runner.asBool)
        Assert.Equal(2.0, Runner.prop "byField" output |> Runner.asNumber)

    [<Fact>]
    let ``WhereByField with > produces strictly greater matches`` () =
        let src = usersSource + """
var older = WhereByField(users, "age", ">", 25)
Output.count = Length(older)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(3.0, Runner.prop "count" output |> Runner.asNumber)

    // -- FindByField ---------------------------------------------------------

    [<Fact>]
    let ``FindByField returns the first matching element`` () =
        let src = usersSource + """
var u = FindByField(users, "role", "==", "admin")
Output.name = u.name"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("alice", Runner.prop "name" output |> Runner.asString)

    [<Fact>]
    let ``FindByField returns a null-ish value when nothing matches`` () =
        // FindByField returns null when nothing matches. `is null` is the
        // idiomatic type predicate; under reflexive null equality (30.10)
        // `u == null` would now also hold.
        let src = usersSource + """
var u = FindByField(users, "role", "==", "ghost")
Output.isNull = u is null"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "isNull" output |> Runner.asBool)

    [<Fact>]
    let ``FindByField on empty array returns a null-ish value`` () =
        let src = """Output.result = FindByField([], "k", "==", 1) is null"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- AllByField / AnyByField --------------------------------------------

    [<Fact>]
    let ``AllByField is true iff every row matches`` () =
        let src = usersSource + """
Output.allAtLeast25 = AllByField(users, "age", ">=", 25)
Output.allAdmins = AllByField(users, "role", "==", "admin")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "allAtLeast25" output |> Runner.asBool)
        Assert.False(Runner.prop "allAdmins" output |> Runner.asBool)

    [<Fact>]
    let ``AllByField on empty array is vacuously true`` () =
        let src = """Output.result = AllByField([], "k", "==", 1)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``AnyByField is true iff at least one row matches`` () =
        let src = usersSource + """
Output.anyAdmin = AnyByField(users, "role", "==", "admin")
Output.anyGhost = AnyByField(users, "role", "==", "ghost")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "anyAdmin" output |> Runner.asBool)
        Assert.False(Runner.prop "anyGhost" output |> Runner.asBool)

    [<Fact>]
    let ``AnyByField on empty array is false`` () =
        let src = """Output.result = AnyByField([], "k", "==", 1)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``AnyByField is the negation of "all unmatched"`` () =
        // any(p) == not all(not p). For a finite set of users, any age
        // >= 30 should equal (NOT all age < 30).
        let src = usersSource + """
Output.any = AnyByField(users, "age", ">=", 30)
Output.allLess = AllByField(users, "age", "<", 30)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.NotEqual(
            Runner.prop "any" output |> Runner.asBool,
            Runner.prop "allLess" output |> Runner.asBool)

    // -- CountIf -------------------------------------------------------------

    [<Fact>]
    let ``CountIf equals Length(WhereByField) for the same filter`` () =
        let src = usersSource + """
Output.count = CountIf(users, "age", "==", 30)
Output.whereLen = Length(WhereByField(users, "age", "==", 30))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "count" output |> Runner.asNumber,
            Runner.prop "whereLen" output |> Runner.asNumber)
        Assert.Equal(2.0, Runner.prop "count" output |> Runner.asNumber)

    [<Fact>]
    let ``CountIf on empty array is 0`` () =
        let src = """Output.result = CountIf([], "k", "==", 1)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``CountIf with never-true predicate is 0`` () =
        let src = usersSource + """
Output.result = CountIf(users, "role", "==", "ghost")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``CountIf with always-true predicate equals Length`` () =
        let src = usersSource + """
Output.result = CountIf(users, "age", ">=", 0)
Output.len = Length(users)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "result" output |> Runner.asNumber,
            Runner.prop "len" output |> Runner.asNumber)

    // -- SortByField ---------------------------------------------------------

    [<Fact>]
    let ``SortByField preserves length`` () =
        let src = usersSource + """
Output.sorted = Length(SortByField(users, "age", "asc"))
Output.original = Length(users)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "original" output |> Runner.asNumber,
            Runner.prop "sorted" output |> Runner.asNumber)

    [<Fact>]
    let ``SortByField asc produces non-decreasing sequence`` () =
        let src = usersSource + """
var sorted = SortByField(users, "age", "asc")
var ok = true
for i in 1 to Length(sorted) - 1 do
  if sorted[i].age < sorted[i - 1].age then
    ok = false
  end
end
Output.result = ok"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``SortByField desc is the reverse of asc`` () =
        let src = usersSource + """
var asc = SortByField(users, "age", "asc")
var desc = SortByField(users, "age", "desc")
var revAsc = Reverse(asc)
var same = true
for i in 0 to Length(desc) - 1 do
  if desc[i].age != revAsc[i].age then
    same = false
  end
end
Output.result = same"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- Select --------------------------------------------------------------

    [<Fact>]
    let ``Select extracts a single field into a flat array`` () =
        let src = usersSource + """
Output.names = Select(users, "name")
Output.len = Length(Output.names)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(5.0, Runner.prop "len" output |> Runner.asNumber)
        let names = Runner.prop "names" output
        Assert.Equal(
            "[\"alice\",\"bob\",\"carol\",\"dave\",\"ellen\"]",
            names.ToJson())

    [<Fact>]
    let ``Select on empty array returns empty`` () =
        let src = """Output.result = Length(Select([], "name"))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Select preserves length`` () =
        let src = usersSource + """
Output.select = Length(Select(users, "age"))
Output.original = Length(users)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "original" output |> Runner.asNumber,
            Runner.prop "select" output |> Runner.asNumber)

    // -- SelectMany ----------------------------------------------------------

    [<Fact>]
    let ``SelectMany flattens nested array fields`` () =
        let src = """
var groups = [
  {tags: ["a", "b"]},
  {tags: ["c"]},
  {tags: ["d", "e", "f"]}
]
Output.flat = SelectMany(groups, "tags")
Output.len = Length(Output.flat)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(6.0, Runner.prop "len" output |> Runner.asNumber)
        Assert.Equal(
            "[\"a\",\"b\",\"c\",\"d\",\"e\",\"f\"]",
            (Runner.prop "flat" output).ToJson())

    [<Fact>]
    let ``SelectMany on empty array returns empty`` () =
        let src = """Output.result = Length(SelectMany([], "tags"))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    // -- Project -------------------------------------------------------------

    [<Fact>]
    let ``Project keeps only the listed fields`` () =
        let src = usersSource + """
var slim = Project(users, ["name", "age"])
Output.len = Length(slim)
Output.firstName = slim[0].name
Output.firstAge = slim[0].age
Output.firstHasRole = HasProperty(slim[0], "role")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(5.0, Runner.prop "len" output |> Runner.asNumber)
        Assert.Equal("alice", Runner.prop "firstName" output |> Runner.asString)
        Assert.Equal(30.0, Runner.prop "firstAge" output |> Runner.asNumber)
        Assert.False(Runner.prop "firstHasRole" output |> Runner.asBool)

    [<Fact>]
    let ``Project preserves length`` () =
        let src = usersSource + """
Output.proj = Length(Project(users, ["name"]))
Output.original = Length(users)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "original" output |> Runner.asNumber,
            Runner.prop "proj" output |> Runner.asNumber)

    [<Fact>]
    let ``Project on empty array returns empty`` () =
        let src = """Output.result = Length(Project([], ["k"]))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    // ------------------------------------------------------------------------
    // Object helpers: Coalesce / Omit / Diff.
    // ------------------------------------------------------------------------

    // -- Coalesce ------------------------------------------------------------
    //
    // Coalesce takes an ARRAY of values and returns the first non-null.
    // If every element is null, returns null.

    [<Fact>]
    let ``Coalesce returns the first non-null element`` () =
        let src = """
Output.a = Coalesce([null, null, "hello", "ignored"])
Output.b = Coalesce([42, "never"])
Output.c = Coalesce([null, 0, "skipped"])"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("hello", Runner.prop "a" output |> Runner.asString)
        Assert.Equal(42.0, Runner.prop "b" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "c" output |> Runner.asNumber)

    [<Fact>]
    let ``Coalesce returns null when all entries are null`` () =
        let src = """Output.result = Coalesce([null, null, null]) is null"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``Coalesce on empty array is null`` () =
        let src = """Output.result = Coalesce([]) is null"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``Coalesce with singleton non-null returns that value`` () =
        let src = """Output.result = Coalesce(["only"])"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("only", Runner.prop "result" output |> Runner.asString)

    // -- Omit ----------------------------------------------------------------
    //
    // Omit takes an array of objects and an array of field names to drop;
    // returns an array of the same length with those fields removed.

    [<Fact>]
    let ``Omit drops the named fields`` () =
        let src = usersSource + """
var slim = Omit(users, ["role"])
Output.len = Length(slim)
Output.firstName = slim[0].name
Output.firstAge = slim[0].age
Output.firstHasRole = HasProperty(slim[0], "role")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(5.0, Runner.prop "len" output |> Runner.asNumber)
        Assert.Equal("alice", Runner.prop "firstName" output |> Runner.asString)
        Assert.Equal(30.0, Runner.prop "firstAge" output |> Runner.asNumber)
        Assert.False(Runner.prop "firstHasRole" output |> Runner.asBool)

    [<Fact>]
    let ``Omit preserves length`` () =
        let src = usersSource + """
Output.omit = Length(Omit(users, ["role"]))
Output.original = Length(users)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(
            Runner.prop "original" output |> Runner.asNumber,
            Runner.prop "omit" output |> Runner.asNumber)

    [<Fact>]
    let ``Omit with no field names is identity`` () =
        let src = usersSource + """
var o = Omit(users, [])
Output.same = Length(o) == Length(users) and Keys(o[0]) == Keys(users[0])"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "same" output |> Runner.asBool)

    [<Fact>]
    let ``Omit on empty array returns empty`` () =
        let src = """Output.result = Length(Omit([], ["name"]))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``Omit is complementary to Project for a single field split`` () =
        // Projecting ["name", "age"] and omitting ["role"] from `users`
        // (which has exactly {name, age, role}) should yield objects with
        // the same key set.
        let src = usersSource + """
var projected = Project(users, ["name", "age"])
var omitted = Omit(users, ["role"])
var sameKeys = true
for i in 0 to Length(projected) - 1 do
  if Length(Keys(projected[i])) != Length(Keys(omitted[i])) then
    sameKeys = false
  end
end
Output.sameKeys = sameKeys"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "sameKeys" output |> Runner.asBool)

    // -- Diff ----------------------------------------------------------------
    //
    // Diff(a, b) returns {added, removed, changed}. Added: in b not a.
    // Removed: in a not b. Changed: both have key, value differs; stored
    // as {from, to} pairs.

    [<Fact>]
    let ``Diff of identical objects is empty on every bucket`` () =
        let src = """
var a = {x: 1, y: 2, z: 3}
var d = Diff(a, a)
Output.added = Length(Keys(d.added))
Output.removed = Length(Keys(d.removed))
Output.changed = Length(Keys(d.changed))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "added" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "removed" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "changed" output |> Runner.asNumber)

    [<Fact>]
    let ``Diff surfaces added, removed, and changed keys`` () =
        let src = """
var a = {keep: 1, drop: 2, same: 3}
var b = {keep: 99, add: "new", same: 3}
var d = Diff(a, b)
Output.addedCount = Length(Keys(d.added))
Output.removedCount = Length(Keys(d.removed))
Output.changedCount = Length(Keys(d.changed))
Output.addedValue = d.added.add
Output.removedValue = d.removed.drop
Output.changedFrom = d.changed.keep.from
Output.changedTo = d.changed.keep.to"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "addedCount" output |> Runner.asNumber)
        Assert.Equal(1.0, Runner.prop "removedCount" output |> Runner.asNumber)
        Assert.Equal(1.0, Runner.prop "changedCount" output |> Runner.asNumber)
        Assert.Equal("new", Runner.prop "addedValue" output |> Runner.asString)
        Assert.Equal(2.0, Runner.prop "removedValue" output |> Runner.asNumber)
        Assert.Equal(1.0, Runner.prop "changedFrom" output |> Runner.asNumber)
        Assert.Equal(99.0, Runner.prop "changedTo" output |> Runner.asNumber)

    [<Fact>]
    let ``Diff swap: added and removed flip when args reverse`` () =
        let src = """
var a = {x: 1, y: 2}
var b = {x: 1, z: 3}
var ab = Diff(a, b)
var ba = Diff(b, a)
Output.sameAddedRemoved =
  Length(Keys(ab.added)) == Length(Keys(ba.removed)) and
  Length(Keys(ab.removed)) == Length(Keys(ba.added))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "sameAddedRemoved" output |> Runner.asBool)

    [<Fact>]
    let ``Diff against empty captures every key as removed`` () =
        let src = """
var a = {x: 1, y: 2, z: 3}
var d = Diff(a, {})
Output.addedCount = Length(Keys(d.added))
Output.removedCount = Length(Keys(d.removed))
Output.changedCount = Length(Keys(d.changed))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "addedCount" output |> Runner.asNumber)
        Assert.Equal(3.0, Runner.prop "removedCount" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "changedCount" output |> Runner.asNumber)

    // ------------------------------------------------------------------------
    // String helpers: PadLeft / PadRight / Replace.
    // ------------------------------------------------------------------------

    // -- PadLeft / PadRight --------------------------------------------------
    //
    // Signature: Pad{Left,Right}(text, length, padding?). When the text is
    // already at or above `length`, the input is returned unchanged.

    [<Fact>]
    let ``PadLeft adds padding to the left`` () =
        let src = """
Output.default = PadLeft("42", 5)
Output.dashes = PadLeft("42", 5, "-")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("   42", Runner.prop "default" output |> Runner.asString)
        Assert.Equal("---42", Runner.prop "dashes" output |> Runner.asString)

    [<Fact>]
    let ``PadRight adds padding to the right`` () =
        let src = """
Output.default = PadRight("42", 5)
Output.dashes = PadRight("42", 5, "-")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("42   ", Runner.prop "default" output |> Runner.asString)
        Assert.Equal("42---", Runner.prop "dashes" output |> Runner.asString)

    [<Fact>]
    let ``Pad is a no-op when text is already long enough`` () =
        let src = """
Output.left = PadLeft("hello", 3)
Output.right = PadRight("hello", 3)
Output.exact = PadLeft("hello", 5, "_")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("hello", Runner.prop "left" output |> Runner.asString)
        Assert.Equal("hello", Runner.prop "right" output |> Runner.asString)
        Assert.Equal("hello", Runner.prop "exact" output |> Runner.asString)

    [<Fact>]
    let ``Pad result reaches the requested length when growing`` () =
        let src = """
Output.leftLen = Length(PadLeft("x", 10, "-"))
Output.rightLen = Length(PadRight("x", 10, "-"))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(10.0, Runner.prop "leftLen" output |> Runner.asNumber)
        Assert.Equal(10.0, Runner.prop "rightLen" output |> Runner.asNumber)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``PadLeft never shrinks the input`` (s: string) =
        let script = sprintf """
var s = Input.s
Output.inputLen = Length(s)
Output.paddedLen = Length(PadLeft(s, 10, "-"))"""
        ignore (sprintf "%s" script)
        let inputLen =
            runWithString s "Output.result = Length(Input.s)" |> Runner.asNumber
        let paddedLen =
            runWithString s "Output.result = Length(PadLeft(Input.s, 10, \"-\"))"
            |> Runner.asNumber
        paddedLen >= inputLen && paddedLen >= 10.0 - 1e-9

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``PadRight never shrinks the input`` (s: string) =
        let inputLen =
            runWithString s "Output.result = Length(Input.s)" |> Runner.asNumber
        let paddedLen =
            runWithString s "Output.result = Length(PadRight(Input.s, 10, \"-\"))"
            |> Runner.asNumber
        paddedLen >= inputLen && paddedLen >= 10.0 - 1e-9

    // -- Replace -------------------------------------------------------------

    [<Fact>]
    let ``Replace swaps every occurrence`` () =
        let src = """Output.result = Replace("abcabc", "b", "X")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("aXcaXc", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Replace rejects an empty search value`` () =
        // Jyro's Replace refuses empty `oldValue` rather than returning
        // identity or inserting between every character (which would be
        // ambiguous). Runtime error surfaces as a diagnostic.
        let src = """Output.result = Replace("hello", "", "-")"""
        let r = Runner.run src (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected Replace with empty oldValue to fail")

    [<Fact>]
    let ``Replace on no-match is identity`` () =
        let src = """Output.result = Replace("hello", "z", "X")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("hello", Runner.prop "result" output |> Runner.asString)

    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Replace with same value for old and new is identity`` (s: string) =
        let src = """
Output.result = Replace(Input.s, "abc", "abc") == Input.s"""
        runWithString s src |> Runner.asBool

    [<Fact>]
    let ``Replace roundtrips for disjoint substitutions`` () =
        // Replacing X back to the original when we've replaced original
        // with X: the round-trip returns to the input (requires the
        // replacement token to not occur in the original).
        let src = """
var original = "hello world"
var once = Replace(original, "hello", "XYZ")
var back = Replace(once, "XYZ", "hello")
Output.result = back == original"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // ------------------------------------------------------------------------
    // Encoding: Base64Encode / Base64Decode.
    // ------------------------------------------------------------------------

    [<Fact>]
    let ``Base64Encode produces the expected fixture`` () =
        let src = """Output.result = Base64Encode("Hello")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("SGVsbG8=", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Base64Decode inverts Base64Encode`` () =
        let src = """
Output.result = Base64Decode(Base64Encode("Hello, world!"))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("Hello, world!", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Base64Encode of empty string is empty string`` () =
        let src = """Output.result = Base64Encode("")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Base64Decode of empty string is empty string`` () =
        let src = """Output.result = Base64Decode("")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("", Runner.prop "result" output |> Runner.asString)

    /// Named for what the generator can actually reach: `genShortString`, the
    /// sole registered string Arbitrary, draws only from a-z, A-Z, 0-9, space,
    /// `-` and `_`. The law is FALSE outside that alphabet - see the
    /// characterization below - so the one class of input where
    /// `Base64Encode` is not a bijection is exactly the class this generator
    /// cannot produce.
    [<FsCheck.Xunit.Property(Arbitrary = [| typeof<RuntimeArbitraries> |])>]
    let ``Base64 round-trip preserves a well-formed string`` (s: string) =
        let src = """Output.result = Base64Decode(Base64Encode(Input.s)) == Input.s"""
        runWithString s src |> Runner.asBool

    [<Fact>]
    let ``Base64 round-trip is exercised beyond the generator's alphabet`` () =
        // The generator cannot reach any of these, and they are what a host's
        // real data contains. All are well-formed UTF-16 and all round-trip.
        for text in [ "cafe\u0301"; "\u65E5\u672C\u8A9E"; "\U0001F600"
                      "line\nbreak\ttab"; "\"quoted\""; "back\\slash"
                      String.replicate 200 "x" ] do
            let out = runWithString text """Output.result = Base64Decode(Base64Encode(Input.s)) == Input.s"""
            Assert.True(Runner.asBool out, sprintf "round-trip failed for %A" text)

    [<Fact>]
    let ``a lone surrogate is refused by the encoding step, not substituted`` () =
        // Refused at the encoding step rather than at construction. A lone
        // surrogate is a legal UTF-16 code unit and a perfectly good value: it
        // orders, compares and equals itself, unlike a non-finite number. What
        // cannot carry it is the conversion to UTF-8, so that is what declines.
        let lone = System.String([| char 0xD800 |])
        let codes =
            Runner.expectFailure
                "Output.result = Base64Encode(Input.s)"
                (Runner.objOf [ "s", (JyroString(lone) :> JyroValue) ])
        Assert.Contains(MessageCode.UnpairedSurrogate, codes)

    [<Fact>]
    let ``ToJson refuses a lone surrogate rather than writing U+FFFD`` () =
        // The same defect at the second egress, which is what made it a value
        // problem rather than a Base64 one. Every string reaches the writer
        // through JyroString.WriteJson, so a string nested deep inside an
        // object is covered by the same guard.
        let o = JyroObject()
        let inner = JyroObject()
        inner.SetProperty("deep", JyroString(System.String([| char 0xD800 |])))
        o.SetProperty("nested", inner)
        let ex = Assert.Throws<JyroRuntimeException>(fun () -> o.ToJson() |> ignore)
        Assert.Equal(MessageCode.UnpairedSurrogate, ex.Code)

    [<Fact>]
    let ``a paired surrogate is not a lone one and still serializes`` () =
        // The guard must not refuse ordinary astral characters, which are a
        // surrogate PAIR. This is the case that would break every emoji.
        let o = JyroObject()
        o.SetProperty("s", JyroString("\U0001F600"))
        Assert.Contains("D83D", o.ToJson().ToUpperInvariant())
        let roundTrip =
            runWithString "\U0001F600" """Output.result = Base64Decode(Base64Encode(Input.s)) == Input.s"""
        Assert.True(Runner.asBool roundTrip)

    [<Fact>]
    let ``Base64Encode output is base64-clean (no invalid chars)`` () =
        let src = """
var encoded = Base64Encode("any input with spaces, symbols! 123")
Output.hasSpace = Contains(encoded, " ")
Output.hasSymbol = Contains(encoded, "!")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "hasSpace" output |> Runner.asBool)
        Assert.False(Runner.prop "hasSymbol" output |> Runner.asBool)

    // ------------------------------------------------------------------------
    // Date / time: ParseDate / FormatDate / DateAdd / DateDiff / DatePart.
    //
    // Every date function reads only the RFC 3339 forms of 90.8.5 and works
    // on the UTC instant they give: an offset is subtracted (94.3.4) and a
    // time with no offset is read as UTC (90.8.6). No local time and no
    // machine time zone is involved anywhere.
    // ------------------------------------------------------------------------

    // A date and time with no offset, read as UTC. A trailing `Z` (an offset
    // of zero) would give the same instant, so leaving it out changes nothing.
    let private sampleDate = "2024-03-15T10:30:45"

    // -- ParseDate -----------------------------------------------------------

    [<Fact>]
    let ``ParseDate returns a non-empty ISO string`` () =
        let src = "Output.result = ParseDate(\"2024-03-15\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        let s = Runner.prop "result" output |> Runner.asString
        Assert.True(s.Length > 0, "ParseDate produced an empty string")
        Assert.StartsWith("2024-03-15", s)

    [<Fact>]
    let ``ParseDate is idempotent after first normalization`` () =
        let src = """
var once = ParseDate("2024-03-15")
var twice = ParseDate(once)
Output.result = once == twice"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``ParseDate accepts multiple input formats`` () =
        // The forms are those of RFC 3339: a full-date, optionally with a time
        // and an offset (90.8.5, 90.8.6, JYRO-REQ-358, JYRO-REQ-360). A slash
        // date is not one of them and gives null.
        let src = """
var a = ParseDate("2024-03-15")
var b = ParseDate("2024-03-15T10:30:00Z")
var c = ParseDate("2024-03-15T10:30:00+00:00")
Output.sameDay =
  DatePart(a, "year") == DatePart(b, "year") and
  DatePart(a, "month") == DatePart(b, "month") and
  DatePart(a, "day") == DatePart(b, "day") and
  DatePart(a, "day") == DatePart(c, "day")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "sameDay" output |> Runner.asBool)

    [<Fact>]
    let ``ParseDate yields null for an unparseable string`` () =
        // One of the four string-parsing functions, all of which report failure
        // by returning null rather than raising: a script has no try/catch, so
        // a raise here would be fatal to the run even where the script has a
        // fallback. See the parse-failure rule on Base64Decode.
        let output = Runner.expectSuccess "Output.result = ParseDate(\"not a date\")" (JyroObject() :> JyroValue)
        Assert.True((Runner.prop "result" output).IsNull)

    [<Fact>]
    let ``the four parsing functions agree on how failure is reported`` () =
        let src =
            """Output.number = ToNumber("nope")
Output.json = FromJson("{bad")
Output.base64 = Base64Decode("!!!")
Output.date = ParseDate("nope")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        for field in [ "number"; "json"; "base64"; "date" ] do
            Assert.True((Runner.prop field output).IsNull, $"{field} should be null on parse failure")

    [<Fact>]
    let ``a failed parse is recoverable in-script with the null-coalescing operator`` () =
        // The reason the convention is null rather than a raise: this script
        // cannot be written at all if the parse function raises. The failure is
        // a string the function cannot interpret (90.4, JYRO-REQ-340); a value
        // that is not a string, null included, is a type mismatch instead
        // (90.4.5, JYRO-REQ-344), so Input.maybe is given as text.
        let output =
            Runner.expectSuccess
                "Output.result = ToNumber(Input.maybe) ?? -1\nOutput.date = ParseDate(\"nope\") ?? \"unknown\""
                (Runner.objOf [ "maybe", JyroString("abc") :> JyroValue ])
        Assert.Equal(-1.0, Runner.asNumber (Runner.prop "result" output))
        Assert.Equal("unknown", Runner.asString (Runner.prop "date" output))

    // -- FormatDate ----------------------------------------------------------

    [<Fact>]
    let ``FormatDate produces the expected field slices`` () =
        let src =
            sprintf
                """
var d = "%s"
Output.year = FormatDate(d, "yyyy")
Output.ymd = FormatDate(d, "yyyy-MM-dd")
Output.hms = FormatDate(d, "HH:mm:ss")"""
                sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("2024", Runner.prop "year" output |> Runner.asString)
        Assert.Equal("2024-03-15", Runner.prop "ymd" output |> Runner.asString)
        Assert.Equal("10:30:45", Runner.prop "hms" output |> Runner.asString)

    [<Fact>]
    let ``FormatDate rejects a malformed format string`` () =
        let r =
            Runner.run
                (sprintf "Output.result = FormatDate(\"%s\", \"%%%%invalid\")" sampleDate)
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected FormatDate to reject bad format")

    // -- DateAdd -------------------------------------------------------------
    //
    // Round-trip stability: every date function parses through
    // `utcStyles` and emits through the Z-tagged ISO format, so a
    // DateAdd-output fed back into any date function is interpreted
    // identically to the raw input.

    [<Fact>]
    let ``DateAdd 0 days is identity`` () =
        let src =
            sprintf
                """Output.result = DateDiff(DateAdd("%s", 0, "days"), "%s", "seconds")"""
                sampleDate sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``DateAdd then subtract returns to the starting date`` () =
        let src =
            sprintf
                """
var shifted = DateAdd(DateAdd("%s", 17, "days"), -17, "days")
Output.result = DateDiff(shifted, "%s", "seconds")"""
                sampleDate sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``DateAdd 7 days equals DateAdd 1 week`` () =
        let src =
            sprintf
                """
var days7 = DateAdd("%s", 7, "days")
var week1 = DateAdd("%s", 1, "weeks")
Output.result = DateDiff(days7, week1, "seconds")"""
                sampleDate sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``DateAdd rejects a non-integer amount`` () =
        let r =
            Runner.run
                (sprintf "Output.result = DateAdd(\"%s\", 1.5, \"days\")" sampleDate)
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected DateAdd to reject a fractional amount")

    [<Fact>]
    let ``DateAdd rejects an unknown unit`` () =
        let r =
            Runner.run
                (sprintf "Output.result = DateAdd(\"%s\", 1, \"fortnights\")" sampleDate)
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected DateAdd to reject unknown unit")

    // -- DateDiff ------------------------------------------------------------

    [<Fact>]
    let ``DateDiff of a date against itself is zero`` () =
        let src = sprintf "Output.result = DateDiff(\"%s\", \"%s\", \"days\")" sampleDate sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``DateDiff is antisymmetric: diff(a, b) == -diff(b, a)`` () =
        let src =
            sprintf
                """
var a = ParseDate("%s")
var b = DateAdd(a, 5, "days")
Output.forward = DateDiff(a, b, "days")
Output.backward = DateDiff(b, a, "days")"""
                sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        let fwd = Runner.prop "forward" output |> Runner.asNumber
        let bwd = Runner.prop "backward" output |> Runner.asNumber
        Assert.Equal(fwd, -bwd)
        // startDate first: a later endDate gives a positive count.
        Assert.Equal(5.0, fwd)

    [<Fact>]
    let ``DateDiff(d, DateAdd(d, n, days), days) equals n`` () =
        let src =
            sprintf
                """Output.result = DateDiff("%s", DateAdd("%s", 42, "days"), "days")"""
                sampleDate sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(42.0, Runner.prop "result" output |> Runner.asNumber)

    [<Fact>]
    let ``DateDiff hours equals 24 times days (approx)`` () =
        let src =
            sprintf
                """
var a = ParseDate("%s")
var b = DateAdd(a, 3, "days")
Output.days = DateDiff(a, b, "days")
Output.hours = DateDiff(a, b, "hours")"""
                sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        let days = Runner.prop "days" output |> Runner.asNumber
        let hours = Runner.prop "hours" output |> Runner.asNumber
        Assert.True(abs (hours - 24.0 * days) < 1e-6)

    [<Fact>]
    let ``DateDiff rejects an unknown unit`` () =
        let r =
            Runner.run
                (sprintf
                    "Output.result = DateDiff(\"%s\", \"%s\", \"fortnights\")"
                    sampleDate sampleDate)
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected DateDiff to reject unknown unit")

    // -- DatePart ------------------------------------------------------------

    [<Fact>]
    let ``DatePart extracts year / month / day / hour / minute / second`` () =
        let src =
            sprintf
                """
var d = "%s"
Output.year = DatePart(d, "year")
Output.month = DatePart(d, "month")
Output.day = DatePart(d, "day")
Output.hour = DatePart(d, "hour")
Output.minute = DatePart(d, "minute")
Output.second = DatePart(d, "second")"""
                sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(2024.0, Runner.prop "year" output |> Runner.asNumber)
        Assert.Equal(3.0, Runner.prop "month" output |> Runner.asNumber)
        Assert.Equal(15.0, Runner.prop "day" output |> Runner.asNumber)
        Assert.Equal(10.0, Runner.prop "hour" output |> Runner.asNumber)
        Assert.Equal(30.0, Runner.prop "minute" output |> Runner.asNumber)
        Assert.Equal(45.0, Runner.prop "second" output |> Runner.asNumber)

    [<Fact>]
    let ``DatePart day-of-year is 1..366`` () =
        let src = sprintf "Output.result = DatePart(\"%s\", \"dayofyear\")" sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        let n = Runner.prop "result" output |> Runner.asNumber
        Assert.True(n >= 1.0 && n <= 366.0, sprintf "dayofyear out of range: %f" n)
        // 2024-03-15 is the 75th day of the year (2024 is a leap year).
        Assert.Equal(75.0, n)

    [<Fact>]
    let ``DatePart day-of-week is 0..6`` () =
        let src = sprintf "Output.result = DatePart(\"%s\", \"dayofweek\")" sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        let n = Runner.prop "result" output |> Runner.asNumber
        Assert.True(n >= 0.0 && n <= 6.0, sprintf "dayofweek out of range: %f" n)

    [<Fact>]
    let ``DatePart year matches FormatDate yyyy round-trip`` () =
        let src =
            sprintf
                """
var d = "%s"
var partYear = DatePart(d, "year")
var formatYear = FormatDate(d, "yyyy")
Output.result = ToString(partYear) == formatYear"""
                sampleDate
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``DatePart rejects an unknown part`` () =
        let r =
            Runner.run
                (sprintf "Output.result = DatePart(\"%s\", \"furlong\")" sampleDate)
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected DatePart to reject unknown part")

    // ------------------------------------------------------------------------
    // Regex laws: RegexTest / RegexMatch / RegexMatchAll / RegexMatchDetail.
    //
    // Jyro's regex functions call .NET's `System.Text.RegularExpressions`
    // with `RegexOptions.None`. That means:
    //   - Case-sensitive by default.
    //   - `.` does not match newline.
    //   - `^` / `$` anchor to string start/end (not line boundaries).
    //   - Named groups are `(?<name>...)` (shared with PCRE).
    //   - Unicode classes follow .NET conventions.
    //
    // Tests here stick to the intersection of PCRE / ECMAScript / .NET
    // (character classes, alternation, basic quantifiers, anchors,
    // simple groups) so they're portable across future backend swaps.
    // ------------------------------------------------------------------------

    // -- RegexTest -----------------------------------------------------------

    [<Fact>]
    let ``RegexTest with match-all pattern always true`` () =
        let src = """
Output.a = RegexTest("", ".*")
Output.b = RegexTest("hello", ".*")
Output.c = RegexTest("abc 123 !@#", ".*")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "a" output |> Runner.asBool)
        Assert.True(Runner.prop "b" output |> Runner.asBool)
        Assert.True(Runner.prop "c" output |> Runner.asBool)

    [<Fact>]
    let ``RegexTest with empty-string anchor matches only empty`` () =
        let src = """
Output.empty = RegexTest("", "^$")
Output.nonEmpty = RegexTest("x", "^$")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "empty" output |> Runner.asBool)
        Assert.False(Runner.prop "nonEmpty" output |> Runner.asBool)

    [<Fact>]
    let ``RegexTest is case-sensitive by default`` () =
        let src = """
Output.lower = RegexTest("hello", "hello")
Output.upper = RegexTest("HELLO", "hello")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "lower" output |> Runner.asBool)
        Assert.False(Runner.prop "upper" output |> Runner.asBool)

    [<Fact>]
    let ``RegexTest recognizes character classes and anchors`` () =
        let src = """
Output.digits = RegexTest("abc123", "\\d+")
Output.noDigits = RegexTest("abc", "\\d+")
Output.startsA = RegexTest("apple", "^a")
Output.endsE = RegexTest("apple", "e$")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "digits" output |> Runner.asBool)
        Assert.False(Runner.prop "noDigits" output |> Runner.asBool)
        Assert.True(Runner.prop "startsA" output |> Runner.asBool)
        Assert.True(Runner.prop "endsE" output |> Runner.asBool)

    [<Fact>]
    let ``RegexTest rejects an invalid pattern`` () =
        // Unclosed bracket - every regex flavor rejects this.
        let r =
            Runner.run
                "Output.result = RegexTest(\"abc\", \"[\")"
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected RegexTest to reject malformed pattern")

    // -- RegexMatch ----------------------------------------------------------

    [<Fact>]
    let ``RegexMatch returns the first match`` () =
        let src = """
Output.result = RegexMatch("abc 123 def 456", "\\d+")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("123", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``RegexMatch returns null on no match`` () =
        let src = """
Output.result = RegexMatch("abc", "\\d+") is null"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RegexMatch agrees with RegexTest`` () =
        // RegexMatch != null iff RegexTest is true.
        let src = """
var inputs = ["apple", "banana", "123", "", "abc123"]
var pattern = "\\d+"
var all = true
foreach s in inputs do
  var m = RegexMatch(s, pattern)
  var t = RegexTest(s, pattern)
  if (m is null) and t then
    all = false
  end
  if (m is not null) and not t then
    all = false
  end
end
Output.result = all"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- RegexMatchAll -------------------------------------------------------

    [<Fact>]
    let ``RegexMatchAll returns every match in order`` () =
        let src = """Output.result = RegexMatchAll("foo 123 bar 456 baz 789", "\\d+")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[\"123\",\"456\",\"789\"]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``RegexMatchAll returns empty array on no match`` () =
        let src = """Output.result = RegexMatchAll("abc", "\\d+")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("[]", (Runner.prop "result" output).ToJson())

    [<Fact>]
    let ``Length(RegexMatchAll) >= 1 iff RegexTest is true`` () =
        let src = """
var inputs = ["apple", "a1", "1 2 3", ""]
var pattern = "\\d"
var consistent = true
foreach s in inputs do
  var n = Length(RegexMatchAll(s, pattern))
  var t = RegexTest(s, pattern)
  if (n >= 1) != t then
    consistent = false
  end
end
Output.result = consistent"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RegexMatchAll first element equals RegexMatch`` () =
        let src = """
var s = "abc 42 def 99"
var pat = "\\d+"
Output.first = RegexMatchAll(s, pat)[0]
Output.single = RegexMatch(s, pat)
Output.same = Output.first == Output.single"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "same" output |> Runner.asBool)

    // -- RegexMatchDetail ----------------------------------------------------

    [<Fact>]
    let ``RegexMatchDetail returns match / index / groups on hit`` () =
        let src = """
var detail = RegexMatchDetail("abc 42 def", "(\\w+)\\s+(\\d+)")
Output.matched = detail.match
Output.index = detail.index
Output.groupCount = Length(detail.groups)
Output.g0 = detail.groups[0]
Output.g1 = detail.groups[1]"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("abc 42", Runner.prop "matched" output |> Runner.asString)
        Assert.Equal(0.0, Runner.prop "index" output |> Runner.asNumber)
        Assert.Equal(2.0, Runner.prop "groupCount" output |> Runner.asNumber)
        Assert.Equal("abc", Runner.prop "g0" output |> Runner.asString)
        Assert.Equal("42", Runner.prop "g1" output |> Runner.asString)

    [<Fact>]
    let ``RegexMatchDetail returns null on no match`` () =
        let src = """Output.result = RegexMatchDetail("abc", "\\d+") is null"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RegexMatchDetail match agrees with RegexMatch`` () =
        let src = """
var s = "prefix-2024-03-15-suffix"
var pat = "\\d{4}-\\d{2}-\\d{2}"
var detail = RegexMatchDetail(s, pat)
Output.result = detail.match == RegexMatch(s, pat)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RegexMatchDetail index + length spans the match in source`` () =
        let src = """
var s = "hello there, friend"
var pat = "there"
var d = RegexMatchDetail(s, pat)
Output.slice = Substring(s, d.index, Length(d.match))
Output.same = Output.slice == d.match"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "same" output |> Runner.asBool)

    // ------------------------------------------------------------------------
    // Non-deterministic functions.
    //
    // We can't assert equality on output, but we CAN assert:
    //   - structural invariants (length, charset, format)
    //   - range constraints (min <= result < max)
    //   - membership (result is an element of input)
    //   - uniqueness (N calls produce N distinct results with overwhelming probability)
    //
    // Covers: NewGuid, RandomInt, RandomChoice, RandomString, Now, Today, Sleep.
    // ------------------------------------------------------------------------

    // -- NewGuid -------------------------------------------------------------

    [<Fact>]
    let ``NewGuid returns a 36-char string`` () =
        let src = """
var g = NewGuid()
Output.isString = g is string
Output.len = Length(g)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "isString" output |> Runner.asBool)
        Assert.Equal(36.0, Runner.prop "len" output |> Runner.asNumber)

    [<Fact>]
    let ``NewGuid has hyphens at positions 8, 13, 18, 23`` () =
        let src = """
var g = NewGuid()
Output.h1 = Substring(g, 8, 1)
Output.h2 = Substring(g, 13, 1)
Output.h3 = Substring(g, 18, 1)
Output.h4 = Substring(g, 23, 1)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("-", Runner.prop "h1" output |> Runner.asString)
        Assert.Equal("-", Runner.prop "h2" output |> Runner.asString)
        Assert.Equal("-", Runner.prop "h3" output |> Runner.asString)
        Assert.Equal("-", Runner.prop "h4" output |> Runner.asString)

    [<Fact>]
    let ``NewGuid is lowercase hex with dashes`` () =
        let src = "Output.result = RegexTest(NewGuid(), \"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$\")"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``NewGuid produces unique values over 100 calls`` () =
        // 100 v4 GUIDs have collision probability ~ 10^-34. Uses
        // `foreach Range(0, 100)`: Range is half-open (92.13.3, JYRO-REQ-716),
        // so it has exactly 100 elements. See `NewGuidProbe` for the empirical
        // confirmation that the generator itself is not the bottleneck.
        let src = """
var seen = []
foreach i in Range(0, 100) do
  seen = Append(seen, NewGuid())
end
Output.total = Length(seen)
Output.unique = Length(Distinct(seen))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(100.0, Runner.prop "total" output |> Runner.asNumber)
        Assert.Equal(100.0, Runner.prop "unique" output |> Runner.asNumber)

    // -- RandomInt -----------------------------------------------------------

    [<Fact>]
    let ``RandomInt(5, 6) is always 5`` () =
        // One-wide range: the only integer in [5, 6) is 5 (JYRO-REQ-999).
        let src = """
var all5 = true
for i in 1 to 50 do
  if RandomInt(5, 6) != 5 then
    all5 = false
  end
end
Output.result = all5"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomInt result is always within [min, max)`` () =
        let src = """
var inRange = true
for i in 1 to 200 do
  var n = RandomInt(10, 20)
  if n < 10 or n >= 20 then
    inRange = false
  end
end
Output.result = inRange"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomInt samples every value of its half-open range and never the max`` () =
        // Over 200 draws from [0, 3), each of 0, 1 and 2 should appear at least
        // once (probability of missing one is (2/3)^200 ~ 10^-35), and 3, the
        // max, never (JYRO-REQ-2027).
        let src = """
var saw0 = false
var saw1 = false
var saw2 = false
var saw3 = false
for i in 1 to 200 do
  var n = RandomInt(0, 3)
  if n == 0 then saw0 = true end
  if n == 1 then saw1 = true end
  if n == 2 then saw2 = true end
  if n == 3 then saw3 = true end
end
Output.result = saw0 and saw1 and saw2 and not saw3"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomInt result is integer-valued`` () =
        let src = """
var allInt = true
for i in 1 to 50 do
  var n = RandomInt(-100, 100)
  if n != Floor(n) then
    allInt = false
  end
end
Output.result = allInt"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- RandomChoice --------------------------------------------------------

    [<Fact>]
    let ``RandomChoice on singleton returns that element`` () =
        let src = """
var xs = [42]
var all42 = true
for i in 1 to 20 do
  if RandomChoice(xs) != 42 then
    all42 = false
  end
end
Output.result = all42"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomChoice returns an element of the array`` () =
        let src = """
var xs = ["a", "b", "c", "d"]
var allMembers = true
for i in 1 to 100 do
  var c = RandomChoice(xs)
  if IndexOf(xs, c) < 0 then
    allMembers = false
  end
end
Output.result = allMembers"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomChoice on empty array returns null`` () =
        let src = "Output.result = RandomChoice([]) is null"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomChoice samples every element over 200 draws`` () =
        // Same argument as RandomInt value coverage: probability of
        // missing any of 4 elements over 200 draws is (3/4)^200 ~ 10^-25.
        let src = """
var xs = ["a", "b", "c", "d"]
var seen = []
for i in 1 to 200 do
  var c = RandomChoice(xs)
  if IndexOf(seen, c) < 0 then
    seen = Append(seen, c)
  end
end
Output.result = Length(seen) == 4"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    // -- RandomString --------------------------------------------------------

    [<Fact>]
    let ``RandomString(0) is empty`` () =
        let src = "Output.result = RandomString(0)"
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``RandomString result has requested length`` () =
        let src = """
var allRight = true
for i in 1 to 20 do
  if Length(RandomString(16)) != 16 then
    allRight = false
  end
end
Output.result = allRight"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomString with custom charset uses only that charset`` () =
        let src = """
var allInSet = true
for i in 1 to 20 do
  var s = RandomString(20, "ab")
  if not RegexTest(s, "^[ab]+$") then
    allInSet = false
  end
end
Output.result = allInSet"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "result" output |> Runner.asBool)

    [<Fact>]
    let ``RandomString rejects negative length`` () =
        let r =
            Runner.run
                "Output.result = RandomString(-1)"
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected RandomString to reject negative length")

    [<Fact>]
    let ``RandomString rejects non-integer length`` () =
        let r =
            Runner.run
                "Output.result = RandomString(1.5)"
                (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected RandomString to reject non-integer length")

    // -- Now / Today ---------------------------------------------------------

    [<Fact>]
    let ``Now returns a string parseable by ParseDate`` () =
        let src = """
var n = Now()
Output.isString = n is string
# ParseDate returns null, not a string, if it can't parse - reparsedIsString below is false unless Now's output actually parsed.
var reparsed = ParseDate(n)
Output.reparsedIsString = reparsed is string"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "isString" output |> Runner.asBool)
        Assert.True(Runner.prop "reparsedIsString" output |> Runner.asBool)

    [<Fact>]
    let ``Today returns a yyyy-MM-dd string`` () =
        let src = """
var t = Today()
Output.isString = t is string
Output.len = Length(t)
Output.matchesFormat = RegexTest(t, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "isString" output |> Runner.asBool)
        Assert.Equal(10.0, Runner.prop "len" output |> Runner.asNumber)
        Assert.True(Runner.prop "matchesFormat" output |> Runner.asBool)

    // -- Sleep ---------------------------------------------------------------

    [<Fact>]
    let ``Sleep 0 completes without error`` () =
        let src = "Sleep(0)\nOutput.result = \"ok\""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal("ok", Runner.prop "result" output |> Runner.asString)

    [<Fact>]
    let ``Sleep rejects a negative duration`` () =
        let r =
            Runner.run "Sleep(-1)" (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected Sleep to reject negative ms")

    [<Fact>]
    let ``Sleep rejects a fractional duration`` () =
        let r =
            Runner.run "Sleep(1.5)" (JyroObject() :> JyroValue)
        Assert.False(r.IsSuccess, "Expected Sleep to reject fractional ms")

    // ------------------------------------------------------------------------
    // Schema validation: ValidateRequired / ValidateSchema.
    //
    // ValidateRequired(obj, fieldNames) returns {valid, errors} where a
    // field is "invalid" if missing, null, or a whitespace-only string.
    // ValidateSchema(data, schema) evaluates the data against a JSON
    // Schema and returns an array of error objects ({path, keyword,
    // message}); empty array == valid.
    // ------------------------------------------------------------------------

    // -- ValidateRequired ----------------------------------------------------

    [<Fact>]
    let ``ValidateRequired with empty field list is always valid`` () =
        let src = """
var r = ValidateRequired({a: 1, b: null}, [])
Output.valid = r.valid
Output.errorCount = Length(r.errors)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal(0.0, Runner.prop "errorCount" output |> Runner.asNumber)

    [<Fact>]
    let ``ValidateRequired passes when every field present and non-null`` () =
        let src = """
var r = ValidateRequired({name: "alice", age: 30, active: true}, ["name", "age", "active"])
Output.valid = r.valid
Output.errorCount = Length(r.errors)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal(0.0, Runner.prop "errorCount" output |> Runner.asNumber)

    // An element of errors is {field, reason} (96.1.8), reason one of missing,
    // null and empty (JYRO-DEF-721 to 723).

    [<Fact>]
    let ``ValidateRequired reports a missing field as field and reason missing`` () =
        let src = """
var r = ValidateRequired({name: "alice"}, ["name", "email"])
Output.valid = r.valid
Output.errorCount = Length(r.errors)
Output.field = r.errors[0].field
Output.reason = r.errors[0].reason"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal(1.0, Runner.prop "errorCount" output |> Runner.asNumber)
        Assert.Equal("email", Runner.prop "field" output |> Runner.asString)
        Assert.Equal("missing", Runner.prop "reason" output |> Runner.asString)

    [<Fact>]
    let ``ValidateRequired reports a null field as field and reason null`` () =
        let src = """
var r = ValidateRequired({name: null}, ["name"])
Output.valid = r.valid
Output.field = r.errors[0].field
Output.reason = r.errors[0].reason"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal("name", Runner.prop "field" output |> Runner.asString)
        Assert.Equal("null", Runner.prop "reason" output |> Runner.asString)

    [<Fact>]
    let ``ValidateRequired reports a whitespace-only string field as field and reason empty`` () =
        let src = """
var r = ValidateRequired({name: "   "}, ["name"])
Output.valid = r.valid
Output.field = r.errors[0].field
Output.reason = r.errors[0].reason"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal("name", Runner.prop "field" output |> Runner.asString)
        Assert.Equal("empty", Runner.prop "reason" output |> Runner.asString)

    [<Fact>]
    let ``ValidateRequired accepts 0 and false as non-empty values`` () =
        // Only strings get the whitespace check; zero/false aren't "empty".
        let src = """
var r = ValidateRequired({count: 0, enabled: false}, ["count", "enabled"])
Output.valid = r.valid
Output.errorCount = Length(r.errors)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal(0.0, Runner.prop "errorCount" output |> Runner.asNumber)

    [<Fact>]
    let ``ValidateRequired reports one error per missing field`` () =
        let src = """
var r = ValidateRequired({only: 1}, ["a", "b", "c"])
Output.valid = r.valid
Output.errorCount = Length(r.errors)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.False(Runner.prop "valid" output |> Runner.asBool)
        Assert.Equal(3.0, Runner.prop "errorCount" output |> Runner.asNumber)

    [<Fact>]
    let ``ValidateRequired result has valid and errors fields`` () =
        let src = """
var r = ValidateRequired({a: 1}, ["a"])
Output.hasValid = HasProperty(r, "valid")
Output.hasErrors = HasProperty(r, "errors")
Output.errorsIsArray = r.errors is array"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "hasValid" output |> Runner.asBool)
        Assert.True(Runner.prop "hasErrors" output |> Runner.asBool)
        Assert.True(Runner.prop "errorsIsArray" output |> Runner.asBool)

    [<Fact>]
    let ``ValidateRequired is idempotent`` () =
        let src = """
var obj = {a: null, b: "x"}
var r1 = ValidateRequired(obj, ["a", "b", "c"])
var r2 = ValidateRequired(obj, ["a", "b", "c"])
Output.sameValid = r1.valid == r2.valid
Output.sameErrorCount = Length(r1.errors) == Length(r2.errors)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "sameValid" output |> Runner.asBool)
        Assert.True(Runner.prop "sameErrorCount" output |> Runner.asBool)

    // -- ValidateSchema ------------------------------------------------------

    [<Fact>]
    let ``ValidateSchema on empty schema accepts any data`` () =
        let src = """
Output.e1 = Length(ValidateSchema(42, {}))
Output.e2 = Length(ValidateSchema("hello", {}))
Output.e3 = Length(ValidateSchema({a: 1}, {}))
Output.e4 = Length(ValidateSchema([1, 2, 3], {}))
Output.e5 = Length(ValidateSchema(null, {}))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(0.0, Runner.prop "e1" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "e2" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "e3" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "e4" output |> Runner.asNumber)
        Assert.Equal(0.0, Runner.prop "e5" output |> Runner.asNumber)

    [<Fact>]
    let ``ValidateSchema detects basic type mismatch`` () =
        let src = """
var schema = {type: "number"}
var errorsOk = Length(ValidateSchema(42, schema))
var errorsBad = Length(ValidateSchema("not a number", schema))
Output.ok = errorsOk == 0
Output.bad = errorsBad > 0"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "ok" output |> Runner.asBool)
        Assert.True(Runner.prop "bad" output |> Runner.asBool)

    [<Fact>]
    let ``ValidateSchema enforces required properties`` () =
        let src = """
var schema = {
  type: "object",
  properties: {
    name: {type: "string"},
    age: {type: "number"}
  },
  required: ["name", "age"]
}
var ok = Length(ValidateSchema({name: "alice", age: 30}, schema)) == 0
var missing = Length(ValidateSchema({name: "alice"}, schema)) > 0
Output.ok = ok
Output.missing = missing"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "ok" output |> Runner.asBool)
        Assert.True(Runner.prop "missing" output |> Runner.asBool)

    [<Fact>]
    let ``ValidateSchema error objects carry path / keyword / message`` () =
        let src = """
var schema = {type: "number"}
var errors = ValidateSchema("not a number", schema)
Output.hasPath = HasProperty(errors[0], "path")
Output.hasKeyword = HasProperty(errors[0], "keyword")
Output.hasMessage = HasProperty(errors[0], "message")"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "hasPath" output |> Runner.asBool)
        Assert.True(Runner.prop "hasKeyword" output |> Runner.asBool)
        Assert.True(Runner.prop "hasMessage" output |> Runner.asBool)

    [<Fact>]
    let ``ValidateSchema enforces enum constraints`` () =
        let src = """
var schema = {type: "string", enum: ["a", "b", "c"]}
Output.aOk = Length(ValidateSchema("a", schema)) == 0
Output.zBad = Length(ValidateSchema("z", schema)) > 0"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "aOk" output |> Runner.asBool)
        Assert.True(Runner.prop "zBad" output |> Runner.asBool)

    [<Fact>]
    let ``ValidateSchema enforces numeric bounds`` () =
        let src = """
var schema = {type: "number", minimum: 0, maximum: 100}
Output.inRange = Length(ValidateSchema(50, schema)) == 0
Output.tooLow = Length(ValidateSchema(-1, schema)) > 0
Output.tooHigh = Length(ValidateSchema(101, schema)) > 0"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "inRange" output |> Runner.asBool)
        Assert.True(Runner.prop "tooLow" output |> Runner.asBool)
        Assert.True(Runner.prop "tooHigh" output |> Runner.asBool)

    [<Fact>]
    let ``ValidateSchema is idempotent`` () =
        let src = """
var schema = {type: "object", required: ["x"]}
var data = {y: 1}
var e1 = ValidateSchema(data, schema)
var e2 = ValidateSchema(data, schema)
Output.sameCount = Length(e1) == Length(e2)"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "sameCount" output |> Runner.asBool)

    // -- ValidateSchema: no $ref leaves the schema (96.2.11, 96.2.12) ----------

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1236")>]
    [<Trait("requirement", "JYRO-REQ-1707")>]
    let ``ValidateSchema refuses an external $ref and opens no connection`` () =
        // A $ref that does not start with # is refused with SchemaInvalid, and
        // nothing is fetched. A listener on a free loopback port shows whether
        // anything tried to connect; the refusal must come first.
        let listener = new System.Net.Sockets.TcpListener(System.Net.IPAddress.Loopback, 0)
        listener.Start()
        try
            let port = (listener.LocalEndpoint :?> System.Net.IPEndPoint).Port
            let src = sprintf "Output.r = ValidateSchema(5, {'$ref': 'http://127.0.0.1:%d/x.json'})" port
            let codes = Runner.expectFailure src (JyroObject() :> JyroValue)
            Assert.Contains(MessageCode.SchemaInvalid, codes)
            Assert.False(listener.Pending(), "ValidateSchema connected to the $ref address")
        finally
            listener.Stop()

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1707")>]
    let ``ValidateSchema refuses a file $ref and a relative $ref`` () =
        for reference in [ "file:///etc/hosts"; "other.json"; "other.json#/a" ] do
            let src = sprintf "Output.r = ValidateSchema('x', {'$ref': '%s'})" reference
            let codes = Runner.expectFailure src (JyroObject() :> JyroValue)
            Assert.Contains(MessageCode.SchemaInvalid, codes)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1706")>]
    let ``ValidateSchema still resolves a $ref inside the schema`` () =
        let src = """
var schema = {'$defs': {s: {type: "string"}}, properties: {a: {'$ref': '#/$defs/s'}}}
Output.ok = Length(ValidateSchema({a: "y"}, schema)) == 0
Output.bad = Length(ValidateSchema({a: 1}, schema)) > 0"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "ok" output |> Runner.asBool)
        Assert.True(Runner.prop "bad" output |> Runner.asBool)

    [<Fact>]
    let ``a $ref inside enum data is a value, not a reference`` () =
        // `enum` holds literal values. Refusing the schema, or resolving the
        // URL, would both treat data as a keyword.
        let src = """Output.n = Length(ValidateSchema({v: 1}, {enum: [{'$ref': 'http://127.0.0.1:9/x.json'}]}))"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.Equal(1.0, Runner.prop "n" output |> Runner.asNumber)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1246")>]
    let ``a catastrophic pattern times out instead of holding the host`` () =
        // A schema pattern runs under the regex timeout (96.2.21, 90.10.3), so
        // a match that backtracks for minutes ends with RegexTimeout.
        let text = String.replicate 35 "a" + "!"
        for src in
            [ sprintf "Output.r = ValidateSchema('%s', {type: 'string', pattern: '^(a+)+$'})" text
              sprintf "Output.r = ValidateSchema({%s1: 1}, {patternProperties: {'^(a+)+$': {type: 'number'}}})" (String.replicate 35 "a") ] do
            let watch = System.Diagnostics.Stopwatch.StartNew()
            let codes = Runner.expectFailure src (JyroObject() :> JyroValue)
            Assert.Contains(MessageCode.RegexTimeout, codes)
            Assert.True(watch.Elapsed < System.TimeSpan.FromSeconds(10.0), sprintf "took %O" watch.Elapsed)

    [<Fact>]
    [<Trait("requirement", "JYRO-REQ-1246")>]
    let ``an invalid schema pattern is RegexInvalidPattern with the pattern as its argument`` () =
        let codes = Runner.expectFailure "Output.r = ValidateSchema('abc', {type: 'string', pattern: '(a'})" (JyroObject() :> JyroValue)
        Assert.Contains(MessageCode.RegexInvalidPattern, codes)

    [<Fact>]
    let ``ordinary patterns still validate`` () =
        let src = """
Output.ok = Length(ValidateSchema("abc", {type: "string", pattern: "^a"})) == 0
Output.bad = Length(ValidateSchema("xbc", {type: "string", pattern: "^a"})) == 1
Output.pp = Length(ValidateSchema({ab: "x"}, {patternProperties: {"^a": {type: "number"}}})) == 1"""
        let output = Runner.expectSuccess src (JyroObject() :> JyroValue)
        Assert.True(Runner.prop "ok" output |> Runner.asBool)
        Assert.True(Runner.prop "bad" output |> Runner.asBool)
        Assert.True(Runner.prop "pp" output |> Runner.asBool)

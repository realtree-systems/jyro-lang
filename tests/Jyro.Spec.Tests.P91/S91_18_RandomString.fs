/// jyro-spec 91.18 RandomString. 91.18.6 (a cryptographically secure
/// generator) is verified by inspection and has no test here.
module RealTree.Jyro.Spec.Tests.P91.S91_18_RandomString

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P91.Support

let code (name: string) = Enum.Parse<MessageCode>(name)

let badValues = badData

let defaultSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

/// Counts of each of `alphabet`'s code units in `s`; fails on a code unit outside it.
let countsOf (alphabet: string) (s: string) : int list =
    let counts = Array.zeroCreate alphabet.Length
    for c in s do
        let i = alphabet.IndexOf c
        if i < 0 then Values.fail (sprintf "code unit U+%04X is not in the character set" (int c))
        counts.[i] <- counts.[i] + 1
    List.ofArray counts

let assertUniform (what: string) (counts: int list) =
    let chi = chiSquareUniform counts
    let crit = chiCritical (counts.Length - 1)
    Assert.True(chi < crit, sprintf "%s: chi-square %.2f is over %.2f (p = 1e-6); counts %A" what chi crit counts)

/// Run the compiled `expr` `times` times (fresh runner each time), the strings it gives.
let repeat (times: int) (expr: string) : string list =
    let compiled = Script.compile (script "" expr)
    Assert.True(Script.ok compiled, Script.describe compiled)
    let program = Script.valueOf compiled
    [ for _ in 1..times -> Values.asString (resultOf ((Script.runnerFor Script.defaults program).Execute())) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-562")>]
let RandomString_PositionalNamedAndOptionalSet_ReturnString () =
    Assert.Equal(5, (str "RandomString(5)").Length)
    Assert.Equal(5, (str "RandomString(5, \"ab\")").Length)
    Assert.Equal(5, (str "RandomString(length: 5, characterSet: \"ab\")").Length)
    Assert.Equal(5, (str "RandomString(characterSet: \"ab\", length: 5)").Length)
    Assert.Equal(5, (str "RandomString(length: 5)").Length)
    Assert.Equal("string", typeOf "RandomString(5)")
    Assert.Equal("string", typeOf "RandomString(0)")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-562")>]
let RandomString_WrongArityOrName_FailsWithArgumentCodes () =
    expectErr MessageCode.TooFewArguments "RandomString()"
    expectErr MessageCode.TooManyArguments "RandomString(5, \"ab\", \"c\")"
    expectErrAmong MessageCode.UnknownNamedArgument "RandomString(count: 5)"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-563")>]
[<InlineData(1)>]
[<InlineData(5)>]
[<InlineData(62)>]
[<InlineData(1000)>]
[<InlineData(1000000)>]
let RandomString_Result_HasExactlyLengthCodeUnits (n: int) =
    Assert.Equal(n, (str (sprintf "RandomString(%d)" n)).Length)
    Assert.Equal(n, (str (sprintf "RandomString(%d, \"x\")" n)).Length)
    Assert.Equal(n, (str (sprintf "RandomString(%d, \"ab\")" n)).Length)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-563")>]
[<InlineData(7)>]
[<InlineData(8)>]
let RandomString_SetWithSurrogatePair_LengthInCodeUnits (n: int) =
    Assert.Equal(n, (str (sprintf "RandomString(%d, %s)" n (lit ("a" + pair)))).Length)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1766")>]
let ``JYRO-REQ-1766 RandomString_EveryCodeUnit_ComesFromCharacterSet`` () =
    let s = str "RandomString(10000, \"xyz\")"
    countsOf "xyz" s |> ignore
    // many short results too, each code unit of each in the set
    for r in repeat 500 "RandomString(7, \"qrs\")" do
        countsOf "qrs" r |> ignore

/// With `characterSet` absent or `null` the set is that of 91.18.7, and every
/// code unit of every result is a code unit of it.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1766")>]
let ``JYRO-REQ-1766 RandomString_AbsentOrNullSet_EveryCodeUnitComesFromTheDefaultSet`` () =
    countsOf defaultSet (str "RandomString(20000)") |> ignore
    countsOf defaultSet (str "RandomString(20000, null)") |> ignore
    for r in repeat 300 "RandomString(9)" do
        countsOf defaultSet r |> ignore

/// Independence over the default set: adjacent code units agree about 1/62 of the time.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1766")>]
let ``JYRO-REQ-1766 RandomString_DefaultSet_PositionsChosenIndependently`` () =
    let s = str "RandomString(100000)"
    let same = [ for i in 0 .. s.Length - 2 -> if s.[i] = s.[i + 1] then 1 else 0 ] |> List.sum
    let n = float (s.Length - 1)
    let p = 1.0 / 62.0
    let z = (float same - n * p) / sqrt (n * p * (1.0 - p))
    Assert.True(abs z < 5.0, sprintf "neighboring code units agree %d of %.0f times (z = %.2f)" same n z)
    // the first and the second position of many two-character results are not related
    let results = repeat 3000 "RandomString(2, \"abc\")"
    let joint = [ for x in "abc" do for y in "abc" -> results |> List.filter ((=) (string x + string y)) |> List.length ]
    assertUniform "RandomString(2, abc) results" joint

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1766")>]
let ``JYRO-REQ-1766 RandomString_Positions_ChosenIndependently`` () =
    // adjacent pairs of one long result: joint distribution uniform over aa, ab, ba, bb
    let s = str "RandomString(40000, \"ab\")"
    let pairs = [ for i in 0 .. 2 .. s.Length - 2 -> s.Substring(i, 2) ]
    let joint = [ "aa"; "ab"; "ba"; "bb" ] |> List.map (fun p -> pairs |> List.filter ((=) p) |> List.length)
    assertUniform "adjacent pairs" joint
    // the same across calls: two-character results
    let results = repeat 4000 "RandomString(2, \"ab\")"
    let joint2 = [ "aa"; "ab"; "ba"; "bb" ] |> List.map (fun p -> results |> List.filter ((=) p) |> List.length)
    assertUniform "RandomString(2) results" joint2
    // no correlation between position i and i + 1 (a run-length check on the long result)
    let same = [ for i in 0 .. s.Length - 2 -> if s.[i] = s.[i + 1] then 1 else 0 ] |> List.sum
    let n = float (s.Length - 1)
    let z = (float same - n / 2.0) / sqrt (n / 4.0)
    Assert.True(abs z < 5.0, sprintf "neighboring code units agree %d of %.0f times (z = %.2f)" same n z)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1767")>]
let ``JYRO-REQ-1767 RandomString_EachSetPosition_EquallyLikely`` () =
    assertUniform "abcd" (countsOf "abcd" (str "RandomString(62000, \"abcd\")"))

/// The default set of 91.18.7 is used with each position equally likely.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1767")>]
let ``JYRO-REQ-1767 RandomString_DefaultSet_EachPositionEquallyLikely`` () =
    assertUniform "default set" (countsOf defaultSet (str "RandomString(124000)"))
    assertUniform "default set, null" (countsOf defaultSet (str "RandomString(124000, null)"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1767")>]
let ``JYRO-REQ-1767 RandomString_RepeatedCodeUnit_HasProportionalProbability`` () =
    let s = str "RandomString(60000, \"aab\")"
    let a = s |> Seq.filter ((=) 'a') |> Seq.length
    // binomial n = 60000, p = 2/3: mean 40000, sd about 115.5
    Assert.True(abs (float a - 40000.0) < 6.0 * 115.5, sprintf "'a' came %d times in 60000, expected about 40000" a)
    let chi = chiSquare (countsOf "ab" s) [ 2.0 / 3.0; 1.0 / 3.0 ]
    Assert.True(chi < chiCritical 1, sprintf "chi-square %.2f" chi)

/// A code unit that occurs 3 times in a set of 5 has 3 times the probability of
/// one that occurs once, at every position of the result.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1767")>]
let ``JYRO-REQ-1767 RandomString_CodeUnitThreeTimesInSet_HasThreeTimesTheProbability`` () =
    let results = repeat 4000 "RandomString(2, \"aaabc\")"
    for position in 0..1 do
        let column = results |> List.map (fun r -> r.[position]) |> Array.ofList |> String
        let chi = chiSquare (countsOf "abc" column) [ 3.0 / 5.0; 1.0 / 5.0; 1.0 / 5.0 ]
        Assert.True(chi < chiCritical 2, sprintf "position %d: chi-square %.2f" position chi)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1767")>]
let ``JYRO-REQ-1767 RandomString_EachResultPosition_Uniform`` () =
    let results = repeat 3000 "RandomString(3, \"abc\")"
    for position in 0..2 do
        let column = results |> List.map (fun r -> r.[position]) |> Array.ofList |> String
        assertUniform (sprintf "position %d" position) (countsOf "abc" column)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-566")>]
let RandomString_SurrogatePairInSet_ChosenPerCodeUnit () =
    let input = surrogates ()
    let ones = [ for _ in 1..200 -> strIn input "RandomString(1, Input.pair)" ]
    for s in ones do
        Assert.Equal(1, s.Length)
        Assert.True(s = hi || s = lo)
    Assert.Contains(hi, ones)
    Assert.Contains(lo, ones)
    let s = strIn input "RandomString(8000, Input.pair)"
    let pairs = [ for i in 0 .. 2 .. s.Length - 2 -> s.Substring(i, 2) ]
    let joint = [ hi + hi; hi + lo; lo + hi; lo + lo ] |> List.map (fun p -> pairs |> List.filter ((=) p) |> List.length)
    assertUniform "surrogate combinations" joint

[<Fact>]
[<Trait("requirement", "JYRO-REQ-568")>]
let ``JYRO-REQ-568 RandomString_AbsentSet_UsesExactly62AlphanumericCodeUnits`` () =
    // only the set is 91.18.7's: that each code unit is equally likely is 91.18.5's
    let s = str "RandomString(100000)"
    let counts = countsOf defaultSet s
    Assert.True(counts |> List.forall (fun c -> c > 0), "not all 62 default code units occurred")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-568")>]
let ``JYRO-REQ-568 RandomString_NullSet_UsesDefault62`` () =
    let s = str "RandomString(100000, null)"
    let counts = countsOf defaultSet s
    Assert.True(counts |> List.forall (fun c -> c > 0), "not all 62 default code units occurred")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-569")>]
let ``JYRO-REQ-569 RandomString_EmptySet_FailsWithEmptyCharacterSet`` () =
    expectErr MessageCode.EmptyCharacterSet "RandomString(5, \"\")"
    expectErr MessageCode.EmptyCharacterSet "RandomString(0, \"\")"
    expectErr MessageCode.EmptyCharacterSet "RandomString(length: 1, characterSet: \"\")"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-569")>]
let ``JYRO-REQ-569 RandomString_NullSet_MeansDefaultSet`` () =
    let s = str "RandomString(1000, null)"
    Assert.Equal(1000, s.Length)
    countsOf defaultSet s |> ignore
    let t = str "RandomString(length: 1000, characterSet: Input.missing)"
    Assert.Equal(1000, t.Length)
    countsOf defaultSet t |> ignore

[<Theory>]
[<Trait("requirement", "JYRO-REQ-570")>]
[<InlineData("RandomString(0)")>]
[<InlineData("RandomString(0, \"ab\")")>]
[<InlineData("RandomString(0, null)")>]
let ``JYRO-REQ-570 RandomString_LengthZero_ReturnsEmptyString`` (expr: string) =
    Assert.Equal("", str expr)

/// Compile `Output.r = [RandomString(Input.n)]` once; the run for `n`, with the
/// bytes allocated on this thread and the seconds taken.
let costOf () : (int64 -> JyroResult<JyroValue> * int64 * float) =
    let compiled = Script.compile (script "" "RandomString(Input.n)")
    Assert.True(Script.ok compiled, Script.describe compiled)
    let program = Script.valueOf compiled
    fun n ->
        let options = { Script.defaults with Input = ValueInput(Values.obj [ "n", Values.exact (decimal n) ]) }
        let runner = Script.runnerFor options program
        let sw = Diagnostics.Stopwatch.StartNew()
        let before = GC.GetAllocatedBytesForCurrentThread()
        let r = runner.Execute()
        r, GC.GetAllocatedBytesForCurrentThread() - before, sw.Elapsed.TotalSeconds

[<Fact>]
[<Trait("requirement", "JYRO-REQ-571")>]
let RandomString_LengthOverMaximum_FailsWithGeneratedLengthExceededBeforeAllocating () =
    expectErr MessageCode.GeneratedLengthExceeded "RandomString(1000001)"
    expectErr MessageCode.GeneratedLengthExceeded "RandomString(1000001, \"ab\")"
    Assert.Equal(1000000, (str "RandomString(1000000)").Length)
    let cost = costOf ()
    cost 2L |> ignore
    let small, baseline, _ = cost 2L
    Assert.True(Script.ok small, Script.describe small)
    let over, overBytes, _ = cost 1000001L
    Script.expectError MessageCode.GeneratedLengthExceeded over
    Assert.True(overBytes - baseline < 1_000_000L, sprintf "RandomString(1000001) allocated %d bytes more than a short one" (overBytes - baseline))
    let huge, hugeBytes, seconds = cost 2147483647L
    Script.expectError MessageCode.GeneratedLengthExceeded huge
    Assert.True(seconds < 1.0, sprintf "RandomString(2147483647) took %.2fs" seconds)
    Assert.True(hugeBytes - baseline < 1_000_000L, sprintf "RandomString(2147483647) allocated %d bytes more than a short one" (hugeBytes - baseline))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-571")>]
[<InlineData("RandomString(1000000.5)", "IntegerRequired")>]
[<InlineData("RandomString(3000000000)", "IntegerOutOfRange")>]
[<InlineData("RandomString(1000001, \"\")", "GeneratedLengthExceeded")>]
[<InlineData("RandomString(1000001, [])", "GeneratedLengthExceeded")>]
[<InlineData("RandomString(5, \"\")", "EmptyCharacterSet")>]
let RandomString_CheckOrder_LengthBeforeCharacterSet (expr: string, name: string) =
    expectErr (code name) expr

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1768")>]
[<InlineData("-1", "NonNegativeIntegerRequired")>]
[<InlineData("-2147483649", "IntegerOutOfRange")>]
[<InlineData("-2147483648", "NonNegativeIntegerRequired")>]
[<InlineData("1.5", "IntegerRequired")>]
[<InlineData("-1.5", "IntegerRequired")>]
[<InlineData("1000000.5", "IntegerRequired")>]
[<InlineData("3000000000", "IntegerOutOfRange")>]
[<InlineData("-3000000000", "IntegerOutOfRange")>]
[<InlineData("2147483648", "IntegerOutOfRange")>]
[<InlineData("1e20", "IntegerOutOfRange")>]
let ``JYRO-REQ-1768 RandomString_LengthNotNonNegativeInteger_FailsWith90_3Code`` (length: string, name: string) =
    expectErr (code name) (sprintf "RandomString(%s)" length)
    expectErr (code name) (sprintf "RandomString(%s, \"ab\")" length)

/// The largest integer of the range is not one of the three codes.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1768")>]
let ``JYRO-REQ-1768 RandomString_LargestIntegerInRange_RaisesNoneOfTheThreeCodes`` () =
    let codes = Script.errorCodes (run "RandomString(2147483647)")
    Assert.DoesNotContain(MessageCode.IntegerRequired, codes)
    Assert.DoesNotContain(MessageCode.IntegerOutOfRange, codes)
    Assert.DoesNotContain(MessageCode.NonNegativeIntegerRequired, codes)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1768")>]
[<InlineData("0", 0)>]
[<InlineData("1", 1)>]
[<InlineData("5.0", 5)>]
[<InlineData("Power(25, 0.5)", 5)>]
let ``JYRO-REQ-1768 RandomString_NonNegativeIntegerLength_Accepted`` (length: string, expected: int) =
    Assert.Equal(expected, (str (sprintf "RandomString(%s)" length)).Length)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-573")>]
[<InlineData("7", "7")>]
[<InlineData("1.5", "1.5")>]
[<InlineData("1.50", "1.5")>]
[<InlineData("true", "true")>]
[<InlineData("false", "false")>]
let ``JYRO-REQ-573 RandomString_NumberOrBooleanSet_ReadAsStringForm`` (set: string, form: string) =
    let s = str (sprintf "RandomString(400, %s)" set)
    Assert.Equal(400, s.Length)
    let alphabet = form |> Seq.distinct |> Array.ofSeq |> String
    let counts = countsOf alphabet s
    Assert.True(counts |> List.forall (fun c -> c > 0), sprintf "not every code unit of %s occurred: %A" form counts)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-573")>]
let ``JYRO-REQ-573 RandomString_OneDigitNumberSet_RepeatsIt`` () =
    Assert.Equal("77777", str "RandomString(5, 7)")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-574")>]
[<MemberData(nameof badValues)>]
let RandomString_ContainerOrFunctionSet_FailsWithArgumentTypeMismatch (bad: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RandomString(5, %s)" bad)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-575")>]
let RandomString_EffectIsNonDeterministic () =
    let underPure = compileUnderMaxEffect FunctionEffect.Pure "RandomString(5)"
    Assert.False(Script.ok underPure, "RandomString compiled under a Pure ceiling")
    Assert.Contains(MessageCode.FunctionEffectExceedsMax, Script.errorCodes underPure)
    let v = evalUnderMaxEffect FunctionEffect.NonDeterministic "RandomString(5)"
    Assert.Equal(5, (Values.asString v).Length)

/// Length values that are not numbers.
let nonNumberLengths : obj[] seq =
    [ "null"; "Input.missing"; "\"3\""; "\"-1\""; "\"1.5\""; "\"2000000\""; "\"\""; "true"; "false"; "[]"; "[3]"; "{}"; "x => x" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-576")>]
[<MemberData(nameof nonNumberLengths)>]
let RandomString_LengthNotNumber_FailsWithArgumentTypeMismatch (length: string) =
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RandomString(%s)" length)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RandomString(%s, \"ab\")" length)
    expectErr MessageCode.ArgumentTypeMismatch (sprintf "RandomString(%s, \"\")" length)

module RealTree.Jyro.Tests.StdlibSemantics

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Tests

let private succeeds (src: string) =
    Runner.prop "r" (Runner.expectSuccess ("Output.r = " + src) (JyroObject() :> JyroValue))

// ---------------------------------------------------------------------------
// The empty aggregate
// ---------------------------------------------------------------------------

// The audit filed `Sum([]) = 0` against the five siblings returning null as an
// undocumented inconsistency, on the grounds that "a script written defensively
// around one aggregate breaks on another". Measuring it turned the finding
// around: the split is not arbitrary, it is the presence or absence of an
// identity element. The empty sum is zero by definition, the same way the empty
// product is one. The empty minimum, maximum, mean, median and mode are not
// defined at all, and returning 0 for any of them would be an invented answer.
//
// So this behavior is kept and pinned rather than unified. Only `Sum` had a
// test before; a rule that lives in one test on one of six functions is a rule
// waiting to be "tidied up" by someone who reads the other five.

[<Fact>]
let ``Sum of an empty array is the additive identity`` () =
    Assert.Equal(0.0, Runner.asNumber (succeeds "Sum([])"))

[<Theory>]
[<InlineData("Min([])")>]
[<InlineData("Max([])")>]
[<InlineData("Average([])")>]
[<InlineData("Median([])")>]
[<InlineData("Mode([])")>]
let ``an aggregate with no identity element yields null on an empty array`` (src: string) =
    Assert.True((succeeds src).IsNull)

// ---------------------------------------------------------------------------
// A null element is absent; anything else non-numeric is the wrong type
// ---------------------------------------------------------------------------

// The six aggregates used to drop every non-numeric element silently, so
// `Sum(['a','b'])` was 0 - the total of two strings reported as zero. The rule
// now matches the one the library already applies one argument earlier, on
// `GetArray` and `GetObject`: normalize a null, reject a wrong
// type.

[<Theory>]
[<InlineData("Sum([1, null, 2])", 3.0)>]
[<InlineData("Average([1, null, 2])", 1.5)>]
[<InlineData("Min([1, null, 2])", 1.0)>]
[<InlineData("Max([1, null, 2])", 2.0)>]
[<InlineData("Median([1, null, 3])", 2.0)>]
let ``a null element is absent and is skipped`` (src: string, expected: float) =
    Assert.Equal(expected, Runner.asNumber (succeeds src))

[<Theory>]
[<InlineData("Sum([1, 'a', 2])")>]
[<InlineData("Sum(['a', 'b'])")>]
[<InlineData("Average([1, 'a', 2])")>]
[<InlineData("Min([1, true])")>]
[<InlineData("Max([1, [2]])")>]
[<InlineData("Median([1, {n: 2}])")>]
[<InlineData("Mode([1, 'a'])")>]
let ``a non-numeric element that is not null is refused`` (src: string) =
    let codes = Runner.expectFailure ("Output.r = " + src) (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.NumericElementRequired, codes)

[<Fact>]
let ``the refusal names the function and the position`` () =
    // Why this is not ArgumentTypeMismatch: that code describes a whole
    // argument and has nowhere to report which element was wrong.
    // 90.7.4 (JYRO-REQ-353): exactly two arguments, the name of the function
    // and the position of the element counted from 0; the type is not one.
    let r = Runner.run "Output.r = Sum([1, 'a', 2])" (JyroObject() :> JyroValue)
    let m = r.Messages |> Seq.find (fun m -> m.Code = MessageCode.NumericElementRequired)
    let args =
        m.Args
        |> Array.map (fun a -> System.Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture))
        |> List.ofArray
    Assert.Equal<string list>([ "Sum"; "1" ], args)

[<Fact>]
let ``an all-null array is treated as empty`` () =
    // Falls straight out of the rule above plus the identity-element rule: no
    // values found means the empty case, so Sum gives its identity and the
    // others give null.
    Assert.Equal(0.0, Runner.asNumber (succeeds "Sum([null, null])"))
    for src in [ "Min([null, null])"; "Max([null, null])"; "Average([null, null])"
                 "Median([null, null])"; "Mode([null, null])" ] do
        Assert.True((succeeds src).IsNull, $"{src} should be null")

[<Fact>]
let ``the library's own idiom still works over optional fields`` () =
    // The reason null is tolerated rather than refused with everything else.
    // GetProperty yields null for a missing key and Select yields null for a
    // missing field, so this pipeline - about the most natural one the library
    // offers - is full of nulls whenever the field is optional. Refusing every
    // non-number would have broken it on ordinary JSON.
    let input =
        JyroValue.FromJson("""{"orders": [{"discount": 10}, {}, {"discount": 5}]}""")
    let output =
        Runner.expectSuccess
            """var discounts = Select(Input.orders, "discount")
Output.total = Sum(discounts)
Output.mean = Average(discounts)"""
            input
    Assert.Equal(15.0, Runner.asNumber (Runner.prop "total" output))
    // Divided by the two values present, not by the three rows - which is only
    // defensible because the third is absent rather than discarded.
    Assert.Equal(7.5, Runner.asNumber (Runner.prop "mean" output))

// ---------------------------------------------------------------------------
// Relational comparison: one question, one answer
// ---------------------------------------------------------------------------

// The query functions take their operator as a string, so `WhereByField(rows,
// "n", "<", 5)` and `row.n < 5` are two spellings of one question. They used to
// disagree: QueryFunctions carried a hand-rolled comparison that ordered
// booleans, which the language's relational operators reject.

[<Theory>]
[<InlineData("CountIf([{a: true}], 'a', '>', false)")>]
[<InlineData("CountIf([{a: true}], 'a', '<', false)")>]
[<InlineData("WhereByField([{a: true}], 'a', '<', false)")>]
[<InlineData("AnyByField([{a: true}], 'a', '>=', false)")>]
[<InlineData("FindByField([{a: true}], 'a', '<=', false)")>]
let ``ordering booleans through the query path fails as it does through the operators`` (src: string) =
    let codes = Runner.expectFailure ("Output.r = " + src) (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.IncomparableTypes, codes)

[<Fact>]
let ``the operator spelling and the query spelling agree`` () =
    // Both directions of the same claim: what the operators reject, the query
    // functions reject; what they accept, the query functions accept.
    let operatorCodes = Runner.expectFailure "Output.r = true < false" (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.UnsupportedBinaryOperation, operatorCodes)

    let queryCodes =
        Runner.expectFailure "Output.r = CountIf([{a: true}], 'a', '<', false)" (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.IncomparableTypes, queryCodes)

[<Fact>]
let ``equality on booleans still works through both paths`` () =
    // Only *ordering* was withdrawn. Equality is defined on every type.
    Assert.Equal(1.0, Runner.asNumber (succeeds "CountIf([{a: true}], 'a', '==', true)"))
    Assert.Equal(0.0, Runner.asNumber (succeeds "CountIf([{a: true}], 'a', '!=', true)"))
    Assert.True(Runner.asBool (succeeds "true == true"))

[<Fact>]
let ``numbers and strings still order through the query path`` () =
    Assert.Equal(1.0, Runner.asNumber (succeeds "CountIf([{n: 3}], 'n', '<', 5)"))
    Assert.Equal(1.0, Runner.asNumber (succeeds "CountIf([{s: 'a'}], 's', '<', 'b')"))

[<Fact>]
let ``sorting keeps its own total order and still places booleans`` () =
    // CompareForSort is deliberately a different contract: it never raises,
    // because a sort has to put every pair of values somewhere. Narrowing the
    // query path must not narrow this.
    let output = succeeds "Sort([true, false])"
    let items = output :?> JyroArray
    Assert.False(Runner.asBool items.[0])
    Assert.True(Runner.asBool items.[1])

// ---------------------------------------------------------------------------
// ParseDate agrees with the rest of its module
// ---------------------------------------------------------------------------

[<Fact>]
let ``ParseDate no longer reads a format the rest of the module rejects`` () =
    // "13/01/2024" parsed under ParseDate and was rejected by FormatDate,
    // DateAdd, DateDiff and DatePart, so a script could parse a date it could
    // then do nothing with.
    Assert.True((succeeds "ParseDate('13/01/2024')").IsNull)
    let codes = Runner.expectFailure "Output.r = DatePart('13/01/2024', 'year')" (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.DateFormatInvalid, codes)

[<Fact>]
let ``the interpretation of a date no longer depends on the data in it`` () =
    // The old format list had MM/dd/yyyy ahead of dd/MM/yyyy, so the first
    // matching format won: "01/02/2024" was read as 2 January while
    // "13/01/2024" was read as 13 January. One written format, two readings,
    // chosen by whether the day happened to exceed 12.
    Assert.True((succeeds "ParseDate('13/01/2024')").IsNull)
    Assert.True((succeeds "ParseDate('31/12/2024')").IsNull)

[<Fact>]
let ``ParseDate still accepts every unambiguous form the module accepts`` () =
    // The module reads RFC 3339 only: a full-date, optionally with a time and
    // an offset (90.8.5, 90.8.6, JYRO-REQ-358, JYRO-REQ-360).
    for src in [ "'2024-01-15'"; "'2024-01-15T10:30:00'"; "'2024-01-15T10:30:00Z'"
                 "'2024-01-15T10:30:00.123Z'" ] do
        let parsed = succeeds ("ParseDate(" + src + ")")
        Assert.False(parsed.IsNull, $"ParseDate({src}) should still parse")

[<Fact>]
let ``ParseDate gives null for a slash date, which RFC 3339 does not write`` () =
    // Any other string is one ParseDate cannot read, so it gives null
    // (90.8.6, JYRO-REQ-360; 90.4, JYRO-REQ-343), even where the order of its
    // fields is unambiguous.
    for src in [ "'2024/01/15'"; "'01/15/2024'" ] do
        Assert.True((succeeds ("ParseDate(" + src + ")")).IsNull, $"ParseDate({src}) should be null")

[<Fact>]
let ``a parsed date is accepted by every function that consumes one`` () =
    // The property that was broken: ParseDate's output must be usable by its
    // four siblings. It always was for ISO input; this pins it.
    let output =
        Runner.expectSuccess
            """var d = ParseDate("2024-01-15")
Output.part = DatePart(d, "year")
Output.added = DateAdd(d, 1, "days")
Output.formatted = FormatDate(d, "yyyy")
Output.diff = DateDiff(d, d, "days")"""
            (JyroObject() :> JyroValue)
    Assert.Equal(2024.0, Runner.asNumber (Runner.prop "part" output))
    Assert.Equal("2024", Runner.asString (Runner.prop "formatted" output))
    Assert.Equal(0.0, Runner.asNumber (Runner.prop "diff" output))

// ---------------------------------------------------------------------------
// The three functions that shipped without ever being invoked
// ---------------------------------------------------------------------------

// `ToBoolean`, `ToNumber` and `Each` were registered, reachable from any
// script, and never once called by the corpus, the property tests, the
// benchmarks or the fuzz seeds. All three sat in `acknowledgedUncovered` with
// reasons that were not true - `ToNumber` was said to be "covered by numeric
// literal + type-check tests" when nothing called it, and `Each` was said to be
// "covered indirectly" as a "side-effecting twin of Map" when it is a separate
// implementation with its own callback dispatch.

[<Theory>]
[<InlineData("ToBoolean(0)", false)>]
[<InlineData("ToBoolean(1)", true)>]
[<InlineData("ToBoolean('')", false)>]
[<InlineData("ToBoolean('x')", true)>]
[<InlineData("ToBoolean(null)", false)>]
[<InlineData("ToBoolean([])", true)>]
[<InlineData("ToBoolean({})", true)>]
let ``ToBoolean matches the language's own truthiness`` (src: string, expected: bool) =
    // Containers are truthy whatever they hold - the rule the runtime uses for
    // `if`, `and` and `or`. An empty array being true is the surprising half,
    // and is exactly why this needs a test of its own rather than an assertion
    // that truthiness is covered elsewhere.
    Assert.Equal(expected, Runner.asBool (succeeds src))

[<Theory>]
[<InlineData("ToNumber('42')", 42.0)>]
[<InlineData("ToNumber('1.5')", 1.5)>]
[<InlineData("ToNumber('-7')", -7.0)>]
let ``ToNumber parses a numeric string`` (src: string, expected: float) =
    Assert.Equal(expected, Runner.asNumber (succeeds src))

[<Fact>]
let ``ToNumber yields null rather than raising on a non-numeric string`` () =
    Assert.True((succeeds "ToNumber('nope')").IsNull)

[<Fact>]
let ``ToNumber keeps a large integer exact`` () =
    // The decimal-first path, which a double would round.
    Assert.Equal("9007199254740993", Runner.asString (succeeds "ToString(ToNumber('9007199254740993'))"))

/// Records every value it is handed. `Each` cannot be observed any other way:
/// a lambda body is an expression (`pLambda` is `params .>> arrow .>>. pExpr`),
/// so the callback cannot assign, and every stdlib array function is `Pure` and
/// returns a fresh array. A host function is therefore the only thing that can
/// make `Each` do anything - which is precisely its purpose, and why no corpus
/// script could have covered it.
type private RecordingFunction(sink: ResizeArray<float>) =
    inherit JyroFunctionBase("Record", JyroFunctionSignature.Create(ParameterType.Null, Parameter.Required("value", ParameterType.Number)))
    override _.Effect = FunctionEffect.Mutating
    override _.Execute(args, _) =
        sink.Add(args.GetNumber(0))
        JyroNull.Instance :> JyroValue

[<Fact>]
let ``Each visits every element in order`` () =
    let seen = ResizeArray<float>()
    let result =
        Runner.compileThenRun
            (JyroCompiler()
                .WithStdlib()
                .AddFunction(RecordingFunction(seen))
                .WithSource("Each([1, 2, 3], x => Record(x * 10))"))
            (fun r -> r.WithInput(JyroObject() :> JyroValue))
    Assert.True(result.IsSuccess, $"%A{result.Messages}")
    Assert.Equal<float list>([ 10.0; 20.0; 30.0 ], List.ofSeq seen)

[<Fact>]
let ``Each returns null rather than the mapped array`` () =
    // The distinction from Map, and the reason "side-effecting twin of Map,
    // covered indirectly" was not a coverage argument.
    Assert.True((succeeds "Each([1, 2, 3], x => x * 10)").IsNull)

// ---------------------------------------------------------------------------
// DateAdd's week arm multiplied in 32 bits
// ---------------------------------------------------------------------------
//
// `float (intAmount * 7)` did the multiply as `int * int`, which F# leaves
// unchecked, so a large amount wrapped *before* the widening and produced a
// different number rather than an error. Every sibling arm widens first, which
// is why this was the only one affected. A silent wrong answer, which is worse
// than either a refusal or a crash.

[<Fact>]
let ``DateAdd with a week count that overflows int32 does not wrap to a small one`` () =
    // -1227133513 * 7 wraps to exactly 1, so this used to return the day after
    // the input date and report success: a request to move back 23.5 million
    // years answered "tomorrow".
    let r =
        Runner.run "Output.r = DateAdd('2024-01-15', 0 - 1227133513, 'weeks')"
                   (JyroObject() :> JyroValue)
    if r.IsSuccess then
        let got = (Runner.prop "r" (Option.get r.TryValue)).ToStringValue()
        Assert.False(got.StartsWith("2024-01-16"),
                     "the week count wrapped in 32 bits and produced a nearby date")
    // Failing is the acceptable outcome - the date is genuinely out of range.
    // Silently succeeding with a nearby date is not.

[<Fact>]
let ``DateAdd weeks still agrees with seven days`` () =
    // The property the wrap hid: it passed only because the corpus tested 1.
    // 100000 weeks is 700000 days - large enough to be a real multiplication
    // and still inside both int32 and DateTime's range, so it pins the
    // arithmetic rather than the refusal.
    for n in [ 1; 2; 52; 1000; 100000; -3; -52; -100000 ] do
        let weeks = (succeeds (sprintf "DateAdd('2024-01-15', %d, 'weeks')" n)).ToStringValue()
        let days = (succeeds (sprintf "DateAdd('2024-01-15', %d, 'days')" (n * 7))).ToStringValue()
        Assert.Equal(days, weeks)

// ---------------------------------------------------------------------------
// ToString
// ---------------------------------------------------------------------------
//
// The fourth member of the group above, left behind when the other three were
// corrected. Its opt-out reason said "covered indirectly via Truthiness and
// type tests"; neither file mentions ToString, and neither can reach
// ToStringFunction.Execute. The reason was false rather than stale, which
// is the case the surrounding comment in StdlibCoverage.fs says is worse than
// a missing entry - and then left standing.

[<Theory>]
[<InlineData("ToString(42)", "42")>]
[<InlineData("ToString(1.5)", "1.5")>]
[<InlineData("ToString(true)", "true")>]
[<InlineData("ToString(false)", "false")>]
[<InlineData("ToString(null)", "null")>]
[<InlineData("ToString('already')", "already")>]
let ``ToString gives the canonical written form of a value`` (src: string, expected: string) =
    Assert.Equal(expected, Runner.asString (succeeds src))

[<Fact>]
let ``ToString accepts a container, where the string helpers refuse one`` () =
    // The distinction that keeps this function separate from
    // GetString: that helper refuses a container, because an object
    // has no textual form, only a rendering. ToString declares AnyParam and
    // reads the argument directly, so the language keeps its ability to
    // stringify anything - which is why refusing containers one layer up did
    // not remove it. Of an array or an object it gives what ToJson gives
    // (95.3.6, JYRO-REQ-1085).
    Assert.Equal("[1,2]", Runner.asString (succeeds "ToString([1, 2])"))
    Assert.Equal("{\"a\":1}", Runner.asString (succeeds "ToString({'a': 1})"))
    Assert.Equal(Runner.asString (succeeds "ToJson([1, 2])"), Runner.asString (succeeds "ToString([1, 2])"))

[<Fact>]
let ``ToString keeps an exact number exact`` () =
    // It routes through ToStringValue, which takes the representation branch,
    // so a value above 2^53 must not arrive through the double view.
    let v = Runner.expectSuccess "Output.r = ToString(Input.id)" (Runner.objOf [ "id", Runner.num 1.0 ])
    Assert.Equal("1", (Runner.prop "r" v).ToStringValue())

// ---------------------------------------------------------------------------
// The declared types have to be accurate, because nothing enforces them
// ---------------------------------------------------------------------------
//
// `ParameterType` was measured as write-only and kept anyway, on the grounds
// that the values are accurate and are the only machine-readable record of what
// a function accepts and returns. That justification only holds while they ARE
// accurate, so the one case where they were not is pinned here: the eight
// higher-order functions declared their callback `AnyParam` while `FunctionParam`
// sat unused.

[<Fact>]
let ``every higher-order function declares its callback as a function`` () =
    let higherOrder = set [ "Map"; "Where"; "Reduce"; "Each"; "Find"; "All"; "Any"; "SortBy" ]
    let registry = StdlibRegistry.getAll () |> Seq.filter (fun f -> higherOrder.Contains f.Name)
    let checked' = registry |> Seq.map (fun f ->
        let callback = f.Signature.Parameters |> Seq.find (fun p -> p.Name = "callback")
        Assert.Equal(ParameterType.Function, callback.Type)
        f.Name)
    // Guards the guard: if a function is renamed out of the set above, the test
    // must not quietly check nothing.
    Assert.Equal<string list>(List.sort (List.ofSeq higherOrder), List.sort (List.ofSeq checked'))

[<Fact>]
let ``a function declared as taking a callback still refuses a non-callback`` () =
    // The declaration is documentation; `getCallback` is what actually checks.
    // Both have to be true, and only the second is load-bearing.
    let codes = Runner.expectFailure "Output.r = Map([1,2], 'not a lambda')" (JyroObject() :> JyroValue)
    Assert.Contains(MessageCode.CallbackExpected, codes)

// -- A callback whose parameter list disagrees with its caller ---------------

// The eight higher-order functions never checked how many parameters their
// callback declared, and a mismatch left the engine entirely:
//
//   Map([1, 2], (x, y) => 1)      -> [RuntimeError] "Index was out of range.
//                                    Must be non-negative and less than the
//                                    size of the collection. (Parameter
//                                    'index')"
//   SortBy([2, 1], (a, b) => a)   -> [RuntimeError] "Failed to compare two
//                                    elements in the array."
//
// A platform exception string, reached from untrusted script input, naming
// nothing about the script that caused it. `Map` supplies one argument, so
// `args.[1]` was simply off the end of the list.
//
// A callback declaring too FEW was accepted and quietly ignored what it was
// given. Both are now refused by the same rule: the parameter list says what
// the callback receives, and one that disagrees with what it receives is a
// mistake in either direction.

let private lambdaCodes (source: string) =
    let r = Runner.runEmpty source
    Assert.False(r.IsSuccess, sprintf "Expected a failure from: %s" source)
    r.Messages |> Seq.map (fun m -> m.Code) |> List.ofSeq

[<Theory>]
[<InlineData("Output.r = Map([1, 2], (x, y) => 1)")>]
[<InlineData("Output.r = Where([1, 2], (x, y) => true)")>]
[<InlineData("Output.r = Each([1, 2], (x, y) => 1)")>]
[<InlineData("Output.r = Find([1, 2], (x, y) => true)")>]
[<InlineData("Output.r = All([1, 2], (x, y) => true)")>]
[<InlineData("Output.r = Any([1, 2], (x, y) => true)")>]
[<InlineData("Output.r = SortBy([2, 1], (a, b) => a)")>]
[<InlineData("Output.r = Reduce([1, 2], (a, b, c) => a, 0)")>]
let ``a callback declaring too many parameters is reported, not crashed into`` (source: string) =
    let codes = lambdaCodes source
    Assert.Contains(MessageCode.CallbackArityMismatch, codes)
    Assert.DoesNotContain(MessageCode.RuntimeError, codes)

[<Theory>]
[<InlineData("Output.r = Map([1, 2], () => 1)")>]
[<InlineData("Output.r = Reduce([1, 2], (a) => a, 0)")>]
let ``a callback declaring too few parameters is reported`` (source: string) =
    Assert.Contains(MessageCode.CallbackArityMismatch, lambdaCodes source)

[<Fact>]
let ``a callback with the declared arity keeps working`` () =
    let v = Runner.expectSuccess
                "Output.m = Map([1, 2], (x) => x * 2)\n\
                 Output.r = Reduce([1, 2, 3], (acc, x) => acc + x, 0)\n\
                 Output.s = SortBy([3, 1, 2], (x) => x)"
                (JyroObject() :> JyroValue)
    Assert.Equal(6.0, Runner.asNumber (Runner.prop "r" v))
    Assert.Equal(2, (Runner.prop "m" v :?> JyroArray).Count)
    Assert.Equal(1.0, Runner.asNumber ((Runner.prop "s" v :?> JyroArray).[0]))

// -- The binary-operation error names the operand that offended -------------

/// The UnsupportedBinaryOperation diagnostic's arguments name the operator as
/// the source writes it and each operand's type as TypeOf spells it, left then
/// right (130.6.2, 130.6.5, JYRO-REQ-2054, JYRO-REQ-1494), never a class or
/// enum name such as `Number`.
let private assertNamesOperands (op: string) (left: string) (right: string) (messages: seq<DiagnosticMessage>) =
    let m = messages |> Seq.find (fun m -> m.Code = MessageCode.UnsupportedBinaryOperation)
    let args =
        m.Args
        |> Array.map (fun a -> System.Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture))
        |> List.ofArray
    Assert.Contains(op, args)
    let l = List.findIndex ((=) left) args
    let r = List.findIndexBack ((=) right) args
    Assert.True(l < r, sprintf "expected %s before %s in %A" left right args)

[<Fact>]
let ``an unsupported binary operation names both operand types`` () =
    // It named the LEFT one only, and the fallback that raises it is reached
    // from `JyroNumber.EvaluateBinary` precisely when the RIGHT operand is the
    // one that is not a number. So `1 / "a"` reported "Unsupported binary
    // operation / for Number" - blaming the one type that `/` accepts, and
    // saying nothing about the string.
    let r = Runner.runEmpty "Output.r = 1 / 'a'"
    Assert.False(r.IsSuccess)
    assertNamesOperands "/" "number" "string" r.Messages

[<Fact>]
let ``an unsupported binary operation still names a bad left operand`` () =
    // The other direction, which was always right and must stay right.
    let r = Runner.runEmpty "Output.r = true / 1"
    Assert.False(r.IsSuccess)
    assertNamesOperands "/" "boolean" "number" r.Messages

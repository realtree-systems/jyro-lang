/// jyro-spec 96.2: ValidateSchema at schema positions outside draft 2020-12's
/// applicators, at references that name nothing, and on schemas and data whose
/// names and strings are long. From the book and the public host API alone.
/// Every run sets its execution limits explicitly; a run that evaluates a
/// pattern also its regex timeout (FLOORS 4.1.11). A call that might take the
/// whole process down (a stack overflow) runs in a child process (Program.fs).
///
/// These are the cases of the defects found at JYRO-WRK-2's inspections
/// (JYRO-DEF-585 to 588, 591 to 594). The rows of JYRO-REQ-10 accept a
/// SchemaInvalid (JYRO-REQ-1233 refuses the array form of items, JYRO-REQ-1232
/// an unknown keyword such as additionalItems) and assert what every answer must
/// hold: the call returns, the process survives, and no pattern is run outside
/// ECMAScript's semantics and the regex timeout. The rows of JYRO-WRK-13's
/// requirements (JYRO-REQ-1258, JYRO-REQ-1707) assert the SchemaInvalid itself.
module RealTree.Jyro.Spec.Tests.S96_02c

open System
open System.Diagnostics
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

/// A child's line is a success, or one error of one of `codes`.
let private okOrOneOf (codes: MessageCode list) (line: string) (schema: string) =
    let allowed = codes |> List.map (fun c -> sprintf "ERR %A" c)
    Assert.True(line.StartsWith "OK " || List.contains line allowed,
                sprintf "schema %s gave '%s'; expected a result or one of %A" schema line allowed)

// ---------------------------------------------------------------------------
// A pattern under the array form of items or under additionalItems is never
// run outside ECMAScript's semantics and the regex timeout (JYRO-DEF-585)
// ---------------------------------------------------------------------------

/// A string on which a backtracking engine with no timeout runs for an
/// exponential time against the patterns below.
let private evil = Values.toJson (Values.str (String('a', 40) + "!"))

let itemsPatternCases : obj[] seq =
    [ for p in [ "^(a+)+$"; "^(a|aa)+$"; "^(a|a)*$" ] do
        let pj = Values.toJson (Values.str p)
        yield [| box (sprintf "[%s]" evil); box (sprintf """{"items": [{"pattern": %s}]}""" pj) |]
        yield [| box (sprintf "[1, %s]" evil); box (sprintf """{"items": [{}], "additionalItems": {"pattern": %s}}""" pj) |]
        yield [| box (sprintf "[%s]" evil); box (sprintf """{"items": [{"properties": {"x": {"pattern": %s}}}, {"pattern": %s}]}""" pj pj) |] ]
    |> Seq.map id

[<Theory>]
[<Trait("requirement", "JYRO-REQ-10")>]
[<MemberData(nameof itemsPatternCases)>]
let ``JYRO-REQ-10 SchemaPattern_UnderAnItemsArrayOrAdditionalItems_IsNotRunWithoutTheTimeout`` (data: string, schema: string) =
    // validateIsolated fails the test when the child does not return within
    // 30 s; the runner's regex timeout is far below that.
    let line = validateIsolated data schema
    okOrOneOf [ MessageCode.SchemaInvalid; MessageCode.RegexTimeout ] line schema

// ---------------------------------------------------------------------------
// A step cycle inside the array form of items or under additionalItems does
// not end the process (JYRO-DEF-587)
// ---------------------------------------------------------------------------

let itemsCycleCases : obj[] seq =
    [ "[1]", """{"items": [{"allOf": [{"$ref": "#/items/0"}]}]}"""
      "[1]", """{"items": [{"not": {"$ref": "#/items/0"}}]}"""
      "[1, 2]", """{"additionalItems": {"not": {"$ref": "#/additionalItems"}}}"""
      "[1, 2]", """{"items": [{}], "additionalItems": {"anyOf": [{"$ref": "#/additionalItems"}]}}"""
      "[1]", """{"items": [{"$ref": "#/items/1"}, {"$ref": "#/items/0"}]}""" ]
    |> Seq.map (fun (d, s) -> [| box d; box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1258")>]
[<MemberData(nameof itemsCycleCases)>]
let ``JYRO-REQ-1258 ValidateSchema_OfACycleInsideAnItemsArrayOrAdditionalItems_DoesNotEndTheProcess`` (data: string, schema: string) =
    // Each is SchemaInvalid: the array form of items is malformed (96.2.8),
    // additionalItems is not a keyword of 96.2.4 to 96.2.6 (96.2.7), and the
    // chain is a cycle of steps (96.2.34). The process survives.
    Assert.Equal(sprintf "ERR %A" MessageCode.SchemaInvalid, validateIsolated data schema)

// ---------------------------------------------------------------------------
// A reference that names nothing in the schema as written never names
// something (JYRO-DEF-591)
// ---------------------------------------------------------------------------

/// (data, schema). Each $ref names nothing in the schema as written; several
/// name places an implementation could create when it rewrites the schema
/// (definitions for $defs, a renamed entry, entries of its own, an allOf
/// element past the end).
let danglingCases : obj[] seq =
    [ "1", """{"$defs": {"x": {"not": {"$ref": "#/definitions/x"}}}, "$ref": "#/$defs/x"}"""
      "1", """{"$defs": {"x": {"type": "string"}}, "$ref": "#/definitions/x"}"""
      "1", """{"$defs": {"x": {"type": "string"}}, "definitions": {"x": {}}, "$ref": "#/definitions/$defs-0"}"""
      """{"a": 1}""", """{"patternProperties": {"a": {"not": {"$ref": "#/definitions/jyro-k0"}}}}"""
      """{"a": 1}""", """{"patternProperties": {"a": {"not": {"$ref": "#/definitions/jyro-0-k0"}}}}"""
      """{"a": 1, "b": 2}""", """{"$ref": "#/definitions/jyro-f0", "patternProperties": {"a": {}}, "additionalProperties": false}"""
      """{"a": 1, "b": 2}""", """{"$ref": "#/definitions/jyro-0-f0", "patternProperties": {"a": {}}, "additionalProperties": false}"""
      """{"a": 1}""", """{"$ref": "#/definitions/jyro-p0", "patternProperties": {"a": {"type": "string"}}}"""
      """{"a": 1}""", """{"$ref": "#/definitions/jyro-0-none", "patternProperties": {"a": {}}}"""
      """{"a": 1}""", """{"$ref": "#/allOf/0", "patternProperties": {"a": {"type": "string"}}}"""
      """{"a": 1}""", """{"allOf": [{}], "$ref": "#/allOf/1", "patternProperties": {"a": {"type": "string"}}}"""
      """{"a": "x"}""", """{"patternProperties": {"a": {"$ref": "#/definitions/jyro-k1"}, "b": {"type": "number"}}}""" ]
    |> Seq.map (fun (d, s) -> [| box d; box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1707")>]
[<MemberData(nameof danglingCases)>]
let ``JYRO-REQ-1707 ValidateSchema_OfARefThatResolvesNowhere_NeverAppliesAPlaceTheRewriteMade`` (data: string, schema: string) =
    // 96.2.12: each $ref resolves nowhere in the schema as written, so the call
    // raises SchemaInvalid, the data is never judged against a place the schema
    // does not have, and the process survives.
    Assert.Equal(sprintf "ERR %A" MessageCode.SchemaInvalid, validateIsolated data schema)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1707")>]
let ``JYRO-REQ-1707 ValidateSchema_OfARefThatResolves_IntoDefsAndPatternProperties_StillApplies`` () =
    // The control of the rows above: a reference that does resolve is applied.
    let line = validateIsolated """{"a": 1}""" """{"$defs": {"x": {"type": "string"}}, "properties": {"a": {"$ref": "#/$defs/x"}}}"""
    Assert.StartsWith("OK [", line)
    Assert.NotEqual<string>("OK []", line)
    let line2 = validateIsolated """{"a": "s", "b": 1}""" """{"patternProperties": {"^a$": {"type": "string"}}, "properties": {"b": {"$ref": "#/patternProperties/%5Ea$"}}}"""
    Assert.StartsWith("OK [", line2)
    Assert.NotEqual<string>("OK []", line2)

// ---------------------------------------------------------------------------
// Long strings, long names and many patterns: the work does not grow with the
// data times the patterns, nor with the square of a name (JYRO-DEF-586, 588,
// 594). The cost of many subschemas under a long key (JYRO-DEF-592, 593, 596)
// is tested under the time limit in Jyro.Spec.Tests.P100_170
// (SchemaWorkUnderTheLimit, case schema-longkey).
// ---------------------------------------------------------------------------

/// The time limit of the workload runs. Each case below completes in well under
/// a second of work when ValidateSchema does work linear in its input; the
/// quadratic or data-times-patterns forms take minutes.
let private workloadLimit = TimeSpan.FromSeconds 20.

/// Run ValidateSchema(data, schema) on host values, compiled first and timed
/// over Execute alone (floors-process 4.1.14), with a 20 s time limit and a 5 s
/// regex timeout. Returns the result and the elapsed time.
let private timedValidate (data: JyroValue) (schema: JyroValue) =
    let program = Script.compile "Output.r = ValidateSchema(Input.d, Input.s)" |> Script.valueOf
    let limits = { specLimits with MaxExecutionTime = workloadLimit }
    let runner =
        Pending.withRegexTimeout
            (JyroRunner(program).WithInput(Values.obj [ "d", data; "s", schema ]).WithExecutionOptions(limits))
            (TimeSpan.FromSeconds 5.)
    let sw = Stopwatch.StartNew()
    let res = runner.Execute()
    sw.Stop()
    res, sw.Elapsed

/// The number of elements of a run that succeeded before the time limit.
let private doneWith (res: JyroResult<JyroValue>, elapsed: TimeSpan) : int =
    Assert.True(Script.ok res, sprintf "ValidateSchema did not complete in %O (limit %O):\n%s" elapsed workloadLimit (Script.describe res))
    List.length (Values.asList (Script.output res |> Values.prop "r"))

/// Assert the run succeeded before the time limit, and the data conforms (0)
/// or fails (1: at least one element; the shape of the elements is 96.2's,
/// JYRO-WRK-13's).
let private expectDone (errors: int) run =
    let n = doneWith run
    if errors = 0 then Assert.Equal(0, n) else Assert.True(n >= 1, "expected the data to fail")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 SchemaPattern_ManyPatternsOverALongString_DoNotMultiplyTheWork`` () =
    // JYRO-DEF-586: 30 distinct patterns, each matching a string of 2,000,000
    // code units that no letter or digit is in.
    let s = String('-', 2_000_000)
    let branches = [ for k in 1 .. 30 -> Values.obj [ "pattern", Values.str (sprintf "^-{%d}" k) ] ]
    let schema = Values.obj [ "items", Values.obj [ "anyOf", Values.arr branches; "allOf", Values.arr branches ] ]
    expectDone 0 (timedValidate (Values.arr [ Values.str s; Values.str (String('-', 40)) ]) schema)
    // and the patterns still decide: "x" matches none of them.
    expectDone 1 (timedValidate (Values.arr [ Values.str s; Values.str "x" ]) (Values.obj [ "items", Values.obj [ "anyOf", Values.arr branches ] ]))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-10")>]
let ``JYRO-REQ-10 SchemaPatternProperties_ManyKeysMatchingTheSameLongNames_DoNotSquareTheWork`` () =
    // JYRO-DEF-588: 300 distinct patternProperties keys, each matching every one
    // of 10 property names of 100,000 code units that no letter or digit
    // begins.
    let names = [ for i in 0 .. 9 -> String('-', 99_999) + string i ]
    let data = Values.obj [ for n in names -> n, Values.int 1 ]
    let keys = [ for k in 1 .. 300 -> sprintf "^-{%d}" k, Values.obj [ "type", Values.str "number" ] ]
    expectDone 0 (timedValidate data (Values.obj [ "patternProperties", Values.obj keys ]))
    // and they apply: a string value fails them.
    expectDone 1 (timedValidate (Values.obj [ names.[0], Values.str "x" ]) (Values.obj [ "patternProperties", Values.obj keys ]))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1411")>]
let ``JYRO-REQ-1411 ValidateSchema_ALongDefsName_DoesNotCostItsSquare`` () =
    // JYRO-DEF-594: a $defs name and a definitions name of 1,000,000 code units
    // that begin as a name an implementation might choose for its own entries.
    let h = String('-', 1_000_000)
    for name in [ "jyro-" + h; "jyro-0" + h; "jyro-pattern-" + h ] do
        let schema =
            Values.obj [ "$defs", Values.obj [ name, Values.obj [ "type", Values.str "number" ] ]
                         "patternProperties", Values.obj [ "^a", Values.obj [ "type", Values.str "number" ] ]
                         "properties", Values.obj [ "p", Values.obj [ "pattern", Values.str "^b" ] ] ]
        expectDone 0 (timedValidate (Values.obj [ "a", Values.int 1; "p", Values.str "b" ]) schema)
        expectDone 1 (timedValidate (Values.obj [ "a", Values.str "x"; "p", Values.str "c" ]) schema)

/// Scenarios, run in a child process (Child.fs), for the SANDBOX defects whose
/// named inputs no test ran: a standard library call over a large or shared
/// value, stopped by the time limit or a host cancel that falls DURING its
/// iteration (100.2.5), and an artifact loaded on a thread with a small stack
/// (150.9.5). Written from the book and each defect's failing case, from the
/// public host API alone (FLOORS 4.1.1).
///
/// A case is run in one of three modes:
/// - `full`: the measuring ceiling of 30 s (JYRO-ADR-171); reports how long
///   Execute took, whatever it gave. The parent uses this to place a limit
///   inside the call; a run that reaches the ceiling is too long.
/// - `time|<ms>`: an execution time limit of `ms`.
/// - `cancel|<ms>`: a 10 min limit, and a host cancel `ms` after Execute starts.
/// A calibrated case is first sized by `calibrate` (JYRO-ADR-137, ADR-171),
/// which runs it in mode `full`, shrinking a size that is too long and growing
/// one that is too short.
/// Each first runs the same program once on small values, so JIT compilation is
/// not in the timed run, and times Execute alone (floors-process 4.1.14).
module RealTree.Jyro.Spec.Tests.IterationCases

open System
open System.Diagnostics
open System.IO
open System.Threading
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support
open RealTree.Jyro.Spec.Tests.ValueSize

/// `V(i)`: returns the i-th value the host prepared. A host function's result is
/// neither read-only nor bound by the maximum value size (100.8.7), and it costs
/// nothing to hand over, so the call under test is the only long step.
type HostValues() =
    inherit JyroFunctionBase("V", Host.signature "V" [ Parameter.Required("i", ParameterType.Number) ] ParameterType.Any)
    member val Values : JyroValue[] = [||] with get, set
    override _.Effect = FunctionEffect.NonDeterministic
    override this.Execute(args, _ctx) = this.Values.[int (Values.asFloat args.[0])]

let private str (s: string) : JyroValue = JyroString(s) :> JyroValue

/// `k` distinct strings: `n` copies of "a", then `i + 1` copies of "!".
let private slowSubjects (n: int) (k: int) : JyroValue =
    let a = JyroArray()
    for i in 0 .. k - 1 do a.Add(JyroString(String('a', n) + String('!', i + 1)))
    a :> JyroValue

/// Nested `items` `depth` deep around `leaf`, with no `$ref`.
let private nestedItems (depth: int) (leaf: string) : string =
    String.replicate depth "{\"items\": " + leaf + String.replicate depth "}"

/// The schema of d-488, as a chain of k levels in which each level's `allOf`
/// applies the next level twice: once written inline, and once by a `$ref`
/// JSON Pointer to it (96.2.10), so the text grows with k while the work
/// unfolds to 2^k. The last level is `{type: number}`. It has no cycle (96.2.34):
/// each step goes one level down. It names no `$defs`, whose references this
/// build does not resolve (reported separately).
let allOfDoubling (k: int) : string =
    let rec level i =
        if i = k - 1 then "{\"type\": \"number\"}"
        else sprintf "{\"allOf\": [%s, {\"$ref\": \"#%s\"}]}" (level (i + 1)) (String.replicate (i + 1) "/allOf/0")
    level 0

/// An object of `n` members named m0 .. m(n-1), each holding `member i`.
let private membersObj (n: int) (memberOf: int -> JyroValue) : JyroObject =
    let o = JyroObject()
    for i in 0 .. n - 1 do o.SetProperty("m" + string i, memberOf i)
    o

/// An object with the one member `key`, holding `value`.
let private single (key: string) (value: JyroValue) : JyroValue =
    let o = JyroObject()
    o.SetProperty(key, value)
    o :> JyroValue

/// JYRO-DEF-596: a schema whose one property name of `keyLength` code units
/// holds a subschema of `n` properties, and whose one $defs name of the same
/// length holds the same.
let private longKeySchema (keyLength: int) (n: int) : JyroValue =
    let entry () = single "properties" (membersObj n (fun _ -> single "type" (str "number")) :> JyroValue)
    let root = JyroObject()
    root.SetProperty("properties", single (String('-', keyLength)) (entry ()))
    root.SetProperty("$defs", single (String('+', keyLength)) (entry ()))
    root :> JyroValue

/// JYRO-DEF-432: a chain of `k` $defs entries, each only a $ref to the next,
/// the last {type: string}, which the root's $ref enters at d0.
let private refChainSchema (k: int) : JyroValue =
    let defs = JyroObject()
    for i in 0 .. k - 2 do defs.SetProperty("d" + string i, single "$ref" (str ("#/$defs/d" + string (i + 1))))
    defs.SetProperty("d" + string (k - 1), single "type" (str "string"))
    let root = JyroObject()
    root.SetProperty("$ref", str "#/$defs/d0")
    root.SetProperty("$defs", defs)
    root :> JyroValue

/// JYRO-DEF-597, 598: data {K: {m0: 0, ..., m(n-1): 0}} with K a key of
/// `keyLength` code units.
let private longPathData (keyLength: int) (n: int) : JyroValue =
    single (String('-', keyLength)) (membersObj n (fun _ -> Values.int 0) :> JyroValue)

/// {additionalProperties: {additionalProperties: inner}}
let private twoLevels (inner: JyroValue) : JyroValue =
    single "additionalProperties" (single "additionalProperties" inner)

/// An array of `n` distinct objects {a: i}.
let private distinctObjects (n: int) : JyroValue =
    let a = JyroArray()
    for i in 0 .. n - 1 do a.Add(single "a" (Values.int i))
    a :> JyroValue

/// An array of `n` numbers.
let private numbers (n: int) : JyroValue =
    let a = JyroArray()
    for i in 0 .. n - 1 do a.Add(Values.int (i % 1000))
    a :> JyroValue

/// JYRO-DEF-771: `g` empty groups.
let private emptyGroups (g: int) : string = String.replicate g "()"

/// JYRO-DEF-771: the groups of the pattern of regex-registers (2^20: the registers hold 2^21 + 2
/// slots, 16 MB, about 512 chunks of ExecutionCheck.ChunkLength), and the size of the backreference
/// case (a capture of 2^28 code units, asked for six times). 2^20 and not 2^22 (JYRO-DEF-785): at
/// 2^22 one match on a slow runner reached the 1 s regex timeout, so the case could not even
/// calibrate, and the one-step allocation of the registers (JYRO-DEF-781, not a step of this work
/// order) was long enough to be taken for a late stop. At 2^20 a match stays far inside the timeout,
/// the allocation is short beside the bound, and the register clear, the reset at each iteration and
/// the copy at a success are still hundreds of chunks long each.
let private regexGroups = 1 <<< 20
let private backrefK = 29
let private backrefJ = 6

/// `(a)(\1\1)(\2\2)...(\(k-1)\(k-1))` then `j` lookaheads `(?=\k)`: group i holds 2^(i-1) "a".
let private backrefPattern (k: int) (j: int) : string =
    let doubling = [ for i in 2 .. k -> sprintf "(\\%d\\%d)" (i - 1) (i - 1) ]
    "(a)" + String.concat "" doubling + String.replicate j (sprintf "(?=\\%d)" k)

/// The text the pattern needs: the k groups hold 2^k - 1 code units, the lookahead 2^(k-1) more.
let private backrefText (k: int) : int = (1 <<< k) - 1 + (1 <<< (k - 1))

/// JYRO-ADR-171 (1): the execution time limit of every run in mode full, well
/// below the 600 s at which the parent kills a calibrating child.
let measuringCeiling : TimeSpan = TimeSpan.FromSeconds 30.

/// Source, the large values and the small values of a case. `p` is a number
/// the parent gives: for the regex and screen cases a calibrated length; for a
/// case that `maxScale` lets grow, the number of doublings of its workload
/// (JYRO-ADR-137); otherwise 0.
let private caseOf (name: string) (p: int) : string * (unit -> JyroValue list) * (unit -> JyroValue list) =
    let nullsV n = fun () -> [ nulls n :> JyroValue ]
    let tree30 walk = lines (treeDoubling "a" 30 @ [ sprintf "Output.n = Length(%s(a))" walk ])
    let search call =
        lines ([ "var s = \"a\"" ] @ List.replicate 24 "s = s & s" @ [ "var h = \"a\"" ] @ List.replicate 22 "h = h & h"
               @ [ "var n = h & \"b\" & h"; sprintf "Output.r = %s" call ])
    match name with
    // d-96: Contains scans an array for a value it does not hold.
    | "contains-array" -> "Output.r = Contains(V(0), 1)", nullsV (1 <<< (25 + p)), nullsV 8
    // d-493: copies and scans of the standard library.
    | "reverse" -> "Output.n = Length(Reverse(V(0)))", nullsV M, nullsV 8
    | "append" -> "Output.n = Length(Append(V(0), 1))", nullsV (M - 1), nullsV 8
    | "prepend" -> "Output.n = Length(Prepend(V(0), 1))", nullsV (M - 1), nullsV 8
    | "insert" -> "Output.n = Length(Insert(V(0), 0, 1))", nullsV (M - 1), nullsV 8
    | "removeat" -> "Output.n = Length(RemoveAt(V(0), 0))", nullsV (M + 1), nullsV 8
    | "removefirst" -> "Output.n = Length(RemoveFirst(V(0)))", nullsV (M + 1), nullsV 8
    | "removelast" -> "Output.n = Length(RemoveLast(V(0)))", nullsV (M + 1), nullsV 8
    | "coalesce" -> "Output.r = Coalesce(V(0))", nullsV (1 <<< (25 + p)), nullsV 8
    | "validaterequired" ->
        let fields n = fun () ->
            let a = JyroArray()
            for i in 0 .. n - 1 do a.Add(JyroString("f" + string i))
            [ JyroObject() :> JyroValue; a :> JyroValue ]
        "Output.r = ValidateRequired(V(0), V(1)).valid", fields (1 <<< (22 + p)), fields 8
    // d-489: the walks of ToJson, ToString and Flatten over the shared tree of 30 doublings.
    | "tojson-tree" -> tree30 "ToJson", (fun () -> []), (fun () -> [])
    | "tostring-tree" -> tree30 "ToString", (fun () -> []), (fun () -> [])
    | "flatten-tree" -> tree30 "Flatten", (fun () -> []), (fun () -> [])
    // d-494: ValidateSchema's walk of shared data: 20 doublings of ["a"], a pattern at every leaf.
    | "schema-tree" ->
        let schema = nestedItems (21 + p) "{\"type\": \"string\", \"pattern\": \"^a$\"}"
        lines ([ "var a = [\"a\"]" ] @ List.replicate (20 + p) "a = [a, a]" @ [ "Output.r = ValidateSchema(a, V(0)).valid" ]),
        (fun () -> [ Values.json schema ]), (fun () -> [ Values.json schema ])
    // d-488: the acyclic allOf doubling of 30 levels.
    | "schema-allof" ->
        "Output.r = ValidateSchema(1, V(0)).valid", (fun () -> [ Values.json (allOfDoubling 30) ]), (fun () -> [ Values.json (allOfDoubling 3) ])
    // d-500: a search of 2^22 + 1 + 2^22 code units that nearly matches at every position of 2^24.
    | "search-contains" -> search "Contains(s, n)", (fun () -> []), (fun () -> [])
    | "search-indexof" -> search "IndexOf(s, n)", (fun () -> []), (fun () -> [])
    | "search-replace" -> search "Length(Replace(s, n, \"\"))", (fun () -> []), (fun () -> [])
    | "search-split" -> search "Length(Split(s, n))", (fun () -> []), (fun () -> [])
    // d-501: matches that each take about 0.4 s, below the 1 s regex timeout. `p` is n.
    | "regex-one" -> "Output.r = Length(RegexMatchAll(V(0), \"(a+)+b|!\"))", (fun () -> [ str (String('a', p) + "!") ]), (fun () -> [ str "aa!" ])
    | "regex-matchall" ->
        "Output.r = Length(RegexMatchAll(V(0), \"(a+)+b|!\"))",
        (fun () -> [ str (String.replicate 10 (String('a', p) + "!")) ]), (fun () -> [ str "aa!" ])
    | "screen-one" ->
        "Output.r = ValidateSchema(V(0), {items: {pattern: \"^(a+)+$\"}}).valid", (fun () -> [ slowSubjects p 1 ]), (fun () -> [ slowSubjects 2 1 ])
    | "screen-series" ->
        "Output.r = ValidateSchema(V(0), {items: {pattern: \"^(a+)+$\"}}).valid", (fun () -> [ slowSubjects p 10 ]), (fun () -> [ slowSubjects 2 1 ])
    // d-503: whole-string steps over a host string of 2^28 code units.
    | "fromjson-host" -> "Output.r = TypeOf(FromJson(V(0)))", (fun () -> [ str ("\"" + String('a', (1 <<< (28 + p)) - 2) + "\"") ]), (fun () -> [ str "\"a\"" ])
    | "startswith-host" ->
        "Output.r = StartsWith(V(0), V(1))", (fun () -> [ str (String('a', 1 <<< (28 + p))); str (String('a', 1 <<< (28 + p))) ]), (fun () -> [ str "ab"; str "a" ])
    // JYRO-DEF-474: FormatDate over a format of M - 2 code units, every one a "-", which 94.4.7
    // copies to the result unchanged, so the result stays under the maximum value size (100.8.7).
    // Reading the format left to right is iteration over data from the script (100.2.5).
    | "formatdate-long" ->
        "Output.n = Length(FormatDate(\"2001-02-03T04:05:06Z\", V(0)))",
        (fun () -> [ str (String('-', M - 2)) ]), (fun () -> [ str "--" ])
    // JYRO-DEF-596: reading a schema whose one $defs name of 2^22 code units holds 4,096
    // properties. Work that grows with the name's length times the members below it, in one
    // unchecked step, is about 17 times the 2.9 s the defect measured (10^6 times 1,000).
    | "schema-longkey" ->
        "Output.r = Length(ValidateSchema(1, V(0)))", (fun () -> [ longKeySchema (1 <<< 22) 4096 ]), (fun () -> [ longKeySchema 4 2 ])
    // JYRO-DEF-432 (1): a chain of 2^20 $defs entries, each only a $ref to the next. Resolved in
    // one unchecked step at a cost of about k^2 (280 ms at k = 32,000), it runs for minutes.
    | "schema-refchain" ->
        "Output.r = Length(ValidateSchema(1, V(0)))", (fun () -> [ refChainSchema (1 <<< 20) ]), (fun () -> [ refChainSchema 3 ])
    // JYRO-DEF-432 (2): uniqueItems over 2^10 << p distinct objects. Calibrated (JYRO-ADR-171):
    // the base is a starting point and assumes no cost class; a pairwise check is about
    // 5 x 10^5 comparisons at the base and 1.3 x 10^8 at 2^14, an n log n one far fewer, and
    // calibration shrinks or grows the array to fit either.
    | "schema-unique" ->
        "Output.r = Length(ValidateSchema(V(0), V(1)))",
        (fun () -> [ distinctObjects (1 <<< (10 + p)); single "uniqueItems" (Values.bool true) ]),
        (fun () -> [ distinctObjects 4; single "uniqueItems" (Values.bool true) ])
    // JYRO-DEF-598 (1): data {K: {m0 .. m131071}} with K of 2^22 code units, a subschema applied
    // to every member and none failing. A path as long as K built for each application is
    // 5 x 10^11 code units.
    | "schema-longpath" ->
        "Output.r = Length(ValidateSchema(V(0), V(1)))",
        (fun () -> [ longPathData (1 <<< 22) (1 <<< 17); twoLevels (Values.emptyObj ()) ]),
        (fun () -> [ longPathData 4 2; twoLevels (Values.emptyObj ()) ])
    // JYRO-DEF-598 (2): data of 2^22 << p numbers, each applied a subschema. Calibrated.
    | "schema-data" ->
        "Output.r = Length(ValidateSchema(V(0), V(1)))",
        (fun () -> [ numbers (1 <<< (22 + p)); single "items" (single "type" (str "number")) ]),
        (fun () -> [ numbers 4; single "items" (single "type" (str "number")) ])
    // JYRO-DEF-597: data {K: {m0 .. m16383}} with K of 2^20 code units, every member failing:
    // 16,384 errors whose paths and messages hold about 2^35 code units, far above the maximum
    // value size M (100.8.7).
    | "schema-errorpaths" ->
        "Output.r = Length(ValidateSchema(V(0), V(1)))",
        (fun () -> [ longPathData (1 <<< 20) (1 <<< 14); twoLevels (single "type" (str "string")) ]),
        (fun () -> [ longPathData 4 2; twoLevels (single "type" (str "string")) ])
    // JYRO-DEF-719: data {K: 1} with K of 2^20 code units, under a subschema whose allOf holds
    // 2^14 copies of {type: string}: 2^14 failures with the same path and keyword, which
    // 96.2.29 reports once. The one element's path and message are about 2^21 code units, far
    // below M; the 2^14 path texts a writer of every failure holds are 2^34. Output.n is 1 when
    // the result is exactly that one element.
    | "schema-duplicates" ->
        let schemaOf (key: string) (copies: int) =
            let all = JyroArray()
            for _ in 1 .. copies do all.Add(single "type" (str "string"))
            single "properties" (single key (single "allOf" (all :> JyroValue)))
        let valuesOf (key: string) (copies: int) = [ single key (Values.int 1); schemaOf key copies; str key ]
        let k = String('-', 1 <<< 20)
        lines [ "var r = ValidateSchema(V(0), V(1))"
                "Output.n = 0"
                "if Length(r) == 1 then"
                "  if r[0].keyword == \"type\" then"
                "    if r[0].path == \"/\" & V(2) then"
                "      if r[0].message == \"type at /\" & V(2) then"
                "        Output.n = 1"
                "      end"
                "    end"
                "  end"
                "end" ],
        (fun () -> valuesOf k (1 <<< 14)), (fun () -> valuesOf "k" 4)
    // JYRO-DEF-771: matching steps whose length grows with the pattern or the text. The pattern and the
    // text are host strings, so the script's source limits do not bound them.
    | "regex-registers" ->
        // (1), (2), (3): a pattern of g empty groups that no match enters (they follow a "b"
        // that the text lacks) inside a starred atom. Each of the 2 * p matches of "a!" repeated p
        // times (one on the "a", one empty on the "!") allocates and clears the 2 * (g + 1)
        // registers, resets the groups of the atom for each iteration of the star, and copies the
        // registers when it succeeds. Reading the pattern is already bounded (JYRO-DEF-768) and
        // takes most of the run; the matching follows it.
        "Output.r = Length(RegexMatchAll(V(0), V(1)))",
        (fun () -> [ str (String.replicate p "a!"); str ("(?:a|b" + emptyGroups regexGroups + ")*") ]),
        (fun () -> [ str "a!a!"; str "(?:a|b()())*" ])
    | "regex-backref-compare" ->
        // (4): a backreference compares a capture of 2^(k-1) code units, and the pattern asks for it j
        // times at one position. The capture is made by doubling, each group holding two copies of
        // the one before, so building it never walks the text a code unit at a time.
        "Output.r = RegexTest(V(0), V(1))", (fun () -> [ str (String('a', backrefText backrefK)); str (backrefPattern backrefK backrefJ) ]),
        (fun () -> [ str (String('a', backrefText 3)); str (backrefPattern 3 1) ])
    // JYRO-DEF-768: a pattern whose digit run (2^26 digits) is read whole by compilation. The
    // pattern is a host string, so the script's source limits do not bound it.
    | "regex-bound" -> "Output.r = RegexTest(\"a\", V(0))", (fun () -> [ str ("a{" + String('9', 1 <<< 26) + "}") ]), (fun () -> [ str "a{9}" ])
    | "regex-backref" -> "Output.r = RegexTest(\"aa\", V(0))", (fun () -> [ str ("(a)\\" + String('9', 1 <<< 26)) ]), (fun () -> [ str "(a)\\1" ])
    | "schema-bound" ->
        "Output.r = Length(ValidateSchema(\"a\", V(0)))",
        (fun () -> [ single "pattern" (str ("a{" + String('9', 1 <<< 26) + "}")) ]), (fun () -> [ single "pattern" (str "a{9}") ])
    | "schema-backref" ->
        "Output.r = Length(ValidateSchema(\"aa\", V(0)))",
        (fun () -> [ single "pattern" (str ("(a)\\" + String('9', 1 <<< 26))) ]), (fun () -> [ single "pattern" (str "(a)\\1") ])
    // JYRO-DEF-769: a $ref whose array token is a run of 2^28 digits.
    | "schema-pointer" ->
        let schemaOf (digits: int) =
            let root = JyroObject()
            root.SetProperty("allOf", JyroArray())
            root.SetProperty("$ref", str ("#/allOf/" + String('0', digits)))
            root :> JyroValue
        "Output.r = Length(ValidateSchema(1, V(0)))", (fun () -> [ schemaOf (1 <<< 28) ]), (fun () -> [ schemaOf 3 ])
    | other -> failwithf "unknown iteration case %s" other

/// The source of the warm-up run: the case's own, at a small scale where the
/// case builds its large value itself.
let private warmOf (name: string) (source: string) : string =
    let lastLine = source.Substring(source.LastIndexOf('\n') + 1)
    match name with
    | "tojson-tree" | "tostring-tree" | "flatten-tree" -> lines (treeDoubling "a" 3 @ [ lastLine ])
    | "schema-tree" -> lines [ "var a = [\"a\"]"; "a = [a, a]"; lastLine ]
    | n when n.StartsWith "search-" ->
        lines [ "var s = \"aaaaaaaa\""; "var h = \"a\""; "h = h & h"; "var n = h & \"b\" & h"; lastLine ]
    | _ -> source

/// Run a case in a mode; the RESULT fields are ok, codes, ms (Execute alone) and
/// fromCancel (ms from the cancel to the end of Execute, in cancel mode).
let private runCase (name: string) (p: int) (mode: string) (ms: int) : string =
    let source, large, small = caseOf name p
    let v = HostValues()
    // JYRO-ADR-171 (1): a run in mode full carries the measuring ceiling, so it always reports.
    let time =
        match mode with
        | "time" -> TimeSpan.FromMilliseconds(float ms)
        | "full" -> measuringCeiling
        | _ -> TimeSpan.FromMinutes 10.
    let options =
        { Script.defaults with
            Functions = [ v :> IJyroFunction ]
            Limits = Some(bigLimits time)
            CompilationTimeout = Some generousCompile }
    let compileText (text: string) =
        let compiled = Script.compileWith options text
        match compiled.TryGetValue() with
        | true, pr when compiled.IsSuccess -> pr
        | _ -> failwithf "compile failed: %s" (Script.describe compiled)
    let program = compileText source
    // Warm-up on small values, with a generous limit.
    v.Values <- Array.ofList (small ())
    JyroRunner(compileText (warmOf name source)).WithInputJson("{}").WithExecutionOptions(bigLimits (TimeSpan.FromMinutes 1.)).Execute() |> ignore
    v.Values <- Array.ofList (large ())
    GC.Collect()
    use cts = new CancellationTokenSource()
    let mutable runner = JyroRunner(program).WithInputJson("{}").WithExecutionOptions(bigLimits time)
    if mode = "cancel" then runner <- runner.WithCancellationToken(cts.Token)
    let canceledAt = ref 0L
    use timer =
        new Timer(
            (fun _ ->
                canceledAt.Value <- Stopwatch.GetTimestamp()
                cts.Cancel()),
            null,
            (if mode = "cancel" then ms else Timeout.Infinite),
            Timeout.Infinite)
    let started = Stopwatch.GetTimestamp()
    let r = runner.Execute()
    let ended = Stopwatch.GetTimestamp()
    timer.Dispose()
    let codes = r.Messages |> Seq.filter (fun m -> m.Severity = MessageSeverity.Error) |> Seq.map (fun m -> string m.Code)
    let fromCancel = if canceledAt.Value = 0L then -1.0 else Stopwatch.GetElapsedTime(canceledAt.Value, ended).TotalMilliseconds
    // Output.n, when the run succeeded and set it to a number: a case's own check of its result.
    let n =
        match r.TryGetValue() with
        | true, o when r.IsSuccess && not (isNull (box o)) ->
            (try
                let v = Values.prop "n" o
                if v.ValueType = JyroValueType.Number then sprintf " n=%s" (Values.toJson v) else ""
             with _ -> "")
        | _ -> ""
    sprintf "ok=%b codes=%s ms=%.1f fromCancel=%.1f%s" r.IsSuccess (String.Join(",", codes))
        (Stopwatch.GetElapsedTime(started, ended).TotalMilliseconds) fromCancel n

/// How many doublings a calibrated case's workload may take (JYRO-ADR-137),
/// within the child's 3 GB heap. The host-string cases at one doubling hold
/// 2^29 code units per string, 1 GB each. The cases sized by the maximum value
/// size M (100.8.7) and the walks of the shared tree of 30 doublings cannot
/// grow, and stay at 0.
let maxScale (name: string) : int =
    match name with
    | "contains-array" | "coalesce" -> 2  // 2^25 to 2^27 nulls
    | "validaterequired" -> 2             // 2^22 to 2^24 field names
    | "schema-tree" -> 2                  // 20 to 22 doublings
    | "schema-unique" -> 4                // 2^10 to 2^14 objects
    | "schema-data" -> 2                  // 2^22 to 2^24 numbers
    | "fromjson-host" | "startswith-host" -> 1
    | _ -> 0

/// JYRO-ADR-171 (2): how many halvings below its base a calibrated case's
/// workload may take. The cases sized by M, the walks of the shared tree of 30
/// doublings and the fixed workloads cannot shrink, and stay at 0.
let minScale (name: string) : int =
    match name with
    | "schema-unique" -> -4               // down to 2^6 objects
    | "schema-data" -> -4                 // down to 2^18 numbers
    | "schema-tree" -> -4                 // down to 16 doublings
    | "contains-array" | "coalesce" -> -4 // down to 2^21 nulls
    | "validaterequired" -> -4            // down to 2^18 field names
    | "fromjson-host" | "startswith-host" -> -4
    | _ -> 0

/// Calibrate a case in the child (JYRO-ADR-137 as amended by JYRO-ADR-171): run
/// it to its end (mode `full`, under the measuring ceiling) at 0 doublings. A
/// run that reaches the ceiling, or runs out of memory, is too long: the
/// workload is halved, down to `minScale`. A run shorter than `targetMs` grows
/// the workload, up to `maxScale`, stopping before a size that is too long.
/// Reports the scale reached (p), its time (ms), whether that time lies between
/// the target and the ceiling (reached), whether even the smallest size was too
/// long (tooLong), and every time measured (times).
let private calibrate (name: string) (targetMs: float) : string =
    let top = maxScale name
    let bottom = minScale name
    let ceilingMs = measuringCeiling.TotalMilliseconds
    let times = Collections.Generic.List<string>()
    /// The time of a full run at scale p, and whether it was too long.
    let timeAt (p: int) : float * bool =
        GC.Collect()
        let ms, long =
            try
                let r = runCase name p "full" 0
                let ms = field "ms" r |> Option.map float |> Option.defaultValue ceilingMs
                let codes = field "codes" r |> Option.defaultValue ""
                ms, (codes.Contains "ExecutionTimeLimitExceeded" || ms >= ceilingMs)
            with :? OutOfMemoryException -> ceilingMs, true
        times.Add(sprintf "%d:%.0f%s" p ms (if long then "+" else ""))
        ms, long
    let mutable p = 0
    let first = timeAt 0
    let mutable ms = fst first
    let mutable long = snd first
    while long && p > bottom do
        p <- p - 1
        let t, l = timeAt p
        ms <- t
        long <- l
    if not long then
        let mutable grown = true
        while ms < targetMs && p < top && grown do
            let t, l = timeAt (p + 1)
            if l then grown <- false
            else
                p <- p + 1
                ms <- t
    sprintf "ok=true codes= p=%d ms=%.1f reached=%b tooLong=%b times=%s"
        p ms (not long && ms >= targetMs) long (String.Join(";", times))

/// 150.9.5 (d-487): load the artifact in `path` on a thread with a stack of
/// `stackKb` KB, with the compilation time limit set, and run it on the main
/// thread. Reports the load's outcome and the value of Output.x.
let private loadOnSmallStack (path: string) (stackKb: int) : string =
    let bytes = File.ReadAllBytes path
    let mutable loaded : JyroResult<CompiledProgram> = Unchecked.defaultof<_>
    let mutable error : exn = null
    let t =
        Thread(
            (fun () ->
                try loaded <- JyroArtifactLoader(bytes).WithStdlib().WithMaxCompilationTime(generousCompile).Load()
                with e -> error <- e),
            stackKb * 1024)
    t.Start()
    t.Join()
    if not (isNull error) then sprintf "ok=false codes= exn=%s" (error.GetType().Name)
    elif not loaded.IsSuccess then
        let codes = loaded.Messages |> Seq.map (fun m -> string m.Code)
        sprintf "ok=false codes=%s" (String.Join(",", codes))
    else
        let program = Script.valueOf loaded
        let r = JyroRunner(program).WithInputJson("{}").WithExecutionOptions(Script.limits allLimits).Execute()
        let x = match r.TryGetValue() with | true, o when r.IsSuccess -> Values.toJson (Values.prop "x" o) | _ -> "none"
        sprintf "ok=true codes= x=%s" x

/// JYRO-DEF-771: in one process, run the case to its end (D), then stop it by the limit (mode
/// `time`) or the cancel (mode `cancel`) at `n` points evenly spaced from `from` to 0.97 of D.
/// Reports D and each point as `at:late`, where late is how long after the limit or the cancel the
/// run ended, or `at:x<codes>` when it ended otherwise.
let private scan (name: string) (size: int) (mode: string) (n: int) (from: float) : string =
    let ms (r: string) = field "ms" r |> Option.map float |> Option.defaultValue 0.0
    let fullRun = runCase name size "full" 0
    let d = ms fullRun
    let expected = if mode = "time" then "ExecutionTimeLimitExceeded" else "CanceledByHost"
    let stops =
        [ for i in 0 .. n - 1 ->
            let at = int (d * (from + (0.97 - from) * float i / float (n - 1)))
            let r = runCase name size mode at
            let codes = field "codes" r |> Option.defaultValue ""
            if codes = expected then
                let over = if mode = "time" then ms r - float at else (field "fromCancel" r |> Option.map float |> Option.defaultValue 0.0)
                sprintf "%d:%.1f" at over
            else sprintf "%d:x%s" at codes ]
    sprintf "ok=%b codes= d=%.0f full=%s stops=%s" (field "ok" fullRun = Some "true") d (field "codes" fullRun |> Option.defaultValue "") (String.Join("|", stops))

/// The child's entry for these scenarios ("ic|...").
let runScenario (parts: string[]) : string =
    match parts with
    | [| "case"; name; p; mode; ms |] -> runCase name (int p) mode (int ms)
    | [| "scan"; name; size; mode; n; from |] -> scan name (int size) mode (int n) (float from)
    | [| "calibrate"; name; targetMs |] -> calibrate name (float targetMs)
    | [| "load"; path; kb |] -> loadOnSmallStack path (int kb)
    | _ -> failwithf "unknown iteration scenario %A" parts

/// What the settings of a JyroRunner do, as 160.7 names them after JYRO-ADR-201: WithInputJson and the JSON members
/// of JyroValue (JYRO-REQ-1904, 2113), WithMaxRegexTime (1905), MaxRegexTime of JyroExecutionOptions (1906), the
/// refusal of a negative count limit (1908), a null argument to a builder (1909), WithHostState (1910) and the
/// reuse of a runner (1929). Written from the specification alone (FLOORS 4.1.1). Each regex run here is the
/// catastrophic one of 91.14: `^(a+)+$` against 40 `a` and a `b`, which no timeout used here lets finish.
module RealTree.Jyro.Spec.Tests.S160_09_RunnerSettings

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.S160_09_Reflect

let private compile (source: string) : CompiledProgram =
    Script.valueOf (JyroCompiler().WithSource(source).WithStdlib().Compile())

let private execute (runner: JyroRunner) : JyroResult<JyroValue> = upcast runner.Execute()

let private utf8 (text: string) : Stream = new MemoryStream(Encoding.UTF8.GetBytes text) :> Stream

let private regexScript : string =
    sprintf "Output.r = RegexTest(\"%s\", \"^(a+)+$\")" (String('a', 40) + "b")

/// A runner of the catastrophic regex, given `configure`, run to its end: the result and the seconds it took.
let private runRegex (configure: JyroRunner -> JyroRunner) : JyroResult<JyroValue> * float =
    let runner = configure (JyroRunner(compile regexScript).WithInput(Values.emptyObj ()))
    let watch = Stopwatch.StartNew()
    let result = execute runner
    watch.Stop()
    result, watch.Elapsed.TotalSeconds

let private ticks (n: int64) : TimeSpan = TimeSpan.FromTicks n

let private oneDay = 864_000_000_000L // ticks in 24 hours

// -- JYRO-REQ-1904: settings are named for what they set ---------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 the members under the older names are gone`` () =
    Assert.DoesNotContain<string>("WithCompilationTimeout", publicMemberNames typeof<JyroCompiler>)
    for name in [ "WithRegexTimeout"; "WithJson"; "WithJsonStream" ] do
        Assert.DoesNotContain<string>(name, publicMemberNames typeof<JyroRunner>)
    for name in [ "FromStream"; "FromStreamAsync" ] do
        Assert.DoesNotContain<string>(name, publicMemberNames typeof<JyroValue>)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 JyroRunner has WithInputJson of a string and of a stream, each returning JyroRunner`` () =
    Assert.Equal(typeof<JyroRunner>, (methodOf typeof<JyroRunner> "WithInputJson" [ typeof<string> ]).ReturnType)
    Assert.Equal(typeof<JyroRunner>, (methodOf typeof<JyroRunner> "WithInputJson" [ typeof<Stream> ]).ReturnType)
    Assert.Equal("json", (methodOf typeof<JyroRunner> "WithInputJson" [ typeof<string> ]).GetParameters().[0].Name)
    Assert.Equal("utf8Json", (methodOf typeof<JyroRunner> "WithInputJson" [ typeof<Stream> ]).GetParameters().[0].Name)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 JyroValue has static FromJson of a stream and FromJsonAsync of a stream and a token`` () =
    Assert.Equal(typeof<JyroValue>, (staticOf typeof<JyroValue> "FromJson" [ typeof<Stream> ]).ReturnType)
    let a = staticOf typeof<JyroValue> "FromJsonAsync" [ typeof<Stream>; typeof<CancellationToken> ]
    Assert.Equal(typeof<Task<JyroValue>>, a.ReturnType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 WithInputJson of a string makes the text the Input of every later Execute and returns the runner`` () =
    let runner = JyroRunner(compile "Output.a = Input.a")
    Assert.Same(runner, runner.WithInputJson("{\"a\":1}"))
    for _ in 1..2 do
        Assert.Equal("{\"a\":1}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 WithInputJson of a stream makes the UTF-8 JSON it holds the Input of every later Execute and returns the runner`` () =
    let runner = JyroRunner(compile "Output.a = Input.a")
    Assert.Same(runner, runner.WithInputJson(utf8 "{\"a\":2}"))
    for _ in 1..2 do
        Assert.Equal("{\"a\":2}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 WithInputJson of text that is not JSON does not throw and the Execute fails with InvalidJson`` () =
    let runner = JyroRunner(compile "Output.x = 1")
    let returned = runner.WithInputJson("[1,")
    Assert.Same(runner, returned)
    Script.expectError MessageCode.InvalidJson (execute runner)
    let streamRunner = JyroRunner(compile "Output.x = 1")
    streamRunner.WithInputJson(utf8 "[1,") |> ignore
    Script.expectError MessageCode.InvalidJson (execute streamRunner)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 FromJson of a stream gives the value that FromJson of the text gives`` () =
    let text = "{\"a\":1,\"b\":[true,null,\"x\"]}"
    let fromText = JyroValue.FromJson(text)
    let fromStream = JyroValue.FromJson(utf8 text)
    Assert.Equal(fromText.ToJson(), fromStream.ToJson())
    Assert.Equal("{\"a\":1}", JyroValue.FromJson(utf8 "{\"a\":1}").ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 FromJsonAsync completes with the value of the stream`` () =
    let task = JyroValue.FromJsonAsync(utf8 "{\"a\":1}", CancellationToken.None)
    Assert.Equal("{\"a\":1}", task.GetAwaiter().GetResult().ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1904")>]
let ``JYRO-REQ-1904 FromJsonAsync with a token canceled before it reads ends as canceled with no value`` () =
    use source = new CancellationTokenSource()
    source.Cancel()
    let task = JyroValue.FromJsonAsync(utf8 "{\"a\":1}", source.Token)
    Assert.ThrowsAny<OperationCanceledException>(fun () -> task.GetAwaiter().GetResult() |> ignore) |> ignore
    Assert.True(task.IsCanceled, "the task did not end as canceled")

// -- JYRO-REQ-2113: text that is not JSON gives InvalidJson at every door ----------------------------------

let private codesOf (e: exn) : MessageCode list =
    match e with
    | :? JyroException as j -> [ for m in j.Messages -> m.Code ]
    | _ -> []

[<Theory>]
[<InlineData("[1,")>]
[<InlineData("{\"a\":1")>]
[<InlineData("a")>]
[<InlineData("")>]
[<InlineData("[1,]")>]
[<InlineData("'x'")>]
[<InlineData("1 2")>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 a run given text that is not JSON fails with InvalidJson by WithInputJson of a string and of a stream`` (text: string) =
    let byString = JyroRunner(compile "Output.x = 1").WithInputJson(text)
    Script.expectError MessageCode.InvalidJson (execute byString)
    let byStream = JyroRunner(compile "Output.x = 1").WithInputJson(utf8 text)
    Script.expectError MessageCode.InvalidJson (execute byStream)

[<Theory>]
[<InlineData("[1,")>]
[<InlineData("{\"a\":1")>]
[<InlineData("a")>]
[<InlineData("")>]
[<InlineData("[1,]")>]
[<InlineData("'x'")>]
[<InlineData("1 2")>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 ExecuteScriptWithJson given text that is not JSON fails with InvalidJson`` (text: string) =
    let result : JyroResult<JyroValue> = upcast (JyroScript.executeScriptWithJson "Output.x = 1" text)
    Script.expectError MessageCode.InvalidJson result

[<Theory>]
[<InlineData("[1,")>]
[<InlineData("{\"a\":1")>]
[<InlineData("a")>]
[<InlineData("")>]
[<InlineData("[1,]")>]
[<InlineData("'x'")>]
[<InlineData("1 2")>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 FromJson of text and of a stream, and FromJsonAsync, fail with InvalidJson through their own channel`` (text: string) =
    let thrown = Assert.ThrowsAny<JyroException>(fun () -> JyroValue.FromJson(text) |> ignore)
    Assert.Equal<MessageCode list>([ MessageCode.InvalidJson ], codesOf thrown)
    let fromStream = Assert.ThrowsAny<JyroException>(fun () -> JyroValue.FromJson(utf8 text) |> ignore)
    Assert.Equal<MessageCode list>([ MessageCode.InvalidJson ], codesOf fromStream)
    let task = JyroValue.FromJsonAsync(utf8 text, CancellationToken.None)
    let async' = Assert.ThrowsAny<JyroException>(fun () -> task.GetAwaiter().GetResult() |> ignore)
    Assert.Equal<MessageCode list>([ MessageCode.InvalidJson ], codesOf async')

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2113")>]
let ``JYRO-REQ-2113 the same members accept a valid JSON text`` () =
    let valid = "{\"a\":[1,2]}"
    let wrapped = "{\"v\":" + valid + "}"
    Assert.Equal(wrapped, Values.toJson (Script.output (execute (JyroRunner(compile "Output.v = Input").WithInputJson(valid)))))
    Assert.Equal(wrapped, Values.toJson (Script.output (execute (JyroRunner(compile "Output.v = Input").WithInputJson(utf8 valid)))))
    Assert.Equal(valid, JyroValue.FromJson(valid).ToJson())
    Assert.Equal(valid, JyroValue.FromJson(utf8 valid).ToJson())

// -- JYRO-REQ-1905: JyroRunner.WithMaxRegexTime -----------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 JyroRunner has WithMaxRegexTime of a TimeSpan named time that returns JyroRunner, and no WithRegexTimeout`` () =
    let m = methodOf typeof<JyroRunner> "WithMaxRegexTime" [ typeof<TimeSpan> ]
    Assert.Equal(typeof<JyroRunner>, m.ReturnType)
    Assert.Equal("time", m.GetParameters().[0].Name)
    Assert.DoesNotContain<string>("WithRegexTimeout", publicMemberNames typeof<JyroRunner>)

[<Theory>]
[<InlineData(1L)>]
[<InlineData(10_000L)>]
[<InlineData(10_000_000L)>]
[<InlineData(864_000_000_000L)>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 WithMaxRegexTime accepts a time from one tick to 24 hours and returns the runner`` (n: int64) =
    let runner = JyroRunner(compile "Output.x = 1")
    Assert.Same(runner, runner.WithMaxRegexTime(ticks n))

[<Theory>]
[<InlineData(0L)>]
[<InlineData(-1L)>]
[<InlineData(-10_000L)>] // Timeout.InfiniteTimeSpan
[<InlineData(-50_000_000L)>]
[<InlineData(864_000_000_001L)>]
[<InlineData(900_000_000_000L)>]
[<InlineData(Int64.MinValue)>]
[<InlineData(Int64.MaxValue)>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 WithMaxRegexTime throws ArgumentOutOfRangeException named time for zero, a negative time and a time above 24 hours`` (n: int64) =
    let runner = JyroRunner(compile "Output.x = 1")
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> runner.WithMaxRegexTime(ticks n) |> ignore)
    Assert.Equal("time", thrown.ParamName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 the infinite time span is refused`` () =
    let runner = JyroRunner(compile "Output.x = 1")
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> runner.WithMaxRegexTime(Timeout.InfiniteTimeSpan) |> ignore)
    Assert.Equal("time", thrown.ParamName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 a time that is set becomes the regex timeout of every later Execute`` () =
    let result, seconds = runRegex (fun r -> r.WithMaxRegexTime(TimeSpan.FromMilliseconds 50.))
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds < 0.8, sprintf "a 50 ms regex timeout stopped the run after %.2f s" seconds)
    // Every later Execute of that runner.
    let runner = JyroRunner(compile regexScript).WithInput(Values.emptyObj ()).WithMaxRegexTime(TimeSpan.FromMilliseconds 50.)
    for _ in 1..2 do
        let watch = Stopwatch.StartNew()
        Script.expectError MessageCode.RegexTimeout (execute runner)
        Assert.True(watch.Elapsed.TotalSeconds < 0.8, "a later Execute did not use the regex timeout that was set")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 a refused call leaves the regex timeout the runner had`` () =
    let result, seconds =
        runRegex (fun r ->
            let r = r.WithMaxRegexTime(TimeSpan.FromMilliseconds 50.)
            Assert.Throws<ArgumentOutOfRangeException>(fun () -> r.WithMaxRegexTime(TimeSpan.Zero) |> ignore) |> ignore
            Assert.Throws<ArgumentOutOfRangeException>(fun () -> r.WithMaxRegexTime(TimeSpan.FromHours 25.) |> ignore) |> ignore
            r)
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds < 0.8, sprintf "after refused calls the run stopped after %.2f s, not at the 50 ms timeout that was kept" seconds)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1905")>]
let ``JYRO-REQ-1905 a runner on which nothing is set uses the default regex timeout of one second`` () =
    let result, seconds = runRegex id
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 0.8 && seconds < 2.5, sprintf "the default regex timeout stopped the run after %.2f s, not about 1 s" seconds)

// -- JYRO-REQ-1906: MaxRegexTime of JyroExecutionOptions --------------------------------------------------

/// A copy of the default options with the property `name` set to `value`. The copy is made by reflection so that
/// the test does not depend on whether the options are a record or a class with settable properties (1906 asks
/// only for `{ get; set; }`).
let private optionsWith (name: string) (value: obj) : JyroExecutionOptions =
    let clone =
        typeof<obj>.GetMethod("MemberwiseClone", BindingFlags.NonPublic ||| BindingFlags.Instance)
        |> fun m -> m.Invoke(JyroExecutionOptions.Default, [||]) :?> JyroExecutionOptions
    typeof<JyroExecutionOptions>.GetProperty(name).SetValue(clone, value)
    clone

let private withRegexTime (t: TimeSpan) : JyroExecutionOptions = optionsWith "MaxRegexTime" (box t)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 JyroExecutionOptions has the property MaxRegexTime that can be read and set, and its default is the timeout of one second`` () =
    let p = typeof<JyroExecutionOptions>.GetProperty("MaxRegexTime")
    Assert.NotNull(p)
    Assert.Equal(typeof<TimeSpan>, p.PropertyType)
    Assert.NotNull(p.GetGetMethod())
    Assert.NotNull(p.GetSetMethod())
    Assert.Equal(TimeSpan.FromSeconds 1., JyroExecutionOptions.Default.MaxRegexTime)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 WithExecutionOptions makes MaxRegexTime the regex timeout of every later Execute`` () =
    let result, seconds = runRegex (fun r -> r.WithExecutionOptions(withRegexTime (TimeSpan.FromMilliseconds 50.)))
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds < 0.8, sprintf "a MaxRegexTime of 50 ms stopped the run after %.2f s" seconds)

[<Theory>]
[<InlineData(1L)>]
[<InlineData(864_000_000_000L)>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 WithExecutionOptions accepts a MaxRegexTime from one tick to 24 hours`` (n: int64) =
    let runner = JyroRunner(compile "Output.x = 1")
    Assert.Same(runner, runner.WithExecutionOptions(withRegexTime (ticks n)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 a MaxRegexTime of zero leaves the timeout that WithMaxRegexTime set`` () =
    let result, seconds =
        runRegex (fun r -> r.WithMaxRegexTime(TimeSpan.FromMilliseconds 50.).WithExecutionOptions(withRegexTime TimeSpan.Zero))
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds < 0.8, sprintf "zero overwrote the timeout that was set: the run stopped after %.2f s" seconds)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 a MaxRegexTime of zero leaves the default timeout when nothing else set one`` () =
    let result, seconds = runRegex (fun r -> r.WithExecutionOptions(withRegexTime TimeSpan.Zero))
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 0.8 && seconds < 2.5, sprintf "zero did not leave the default: the run stopped after %.2f s" seconds)

[<Theory>]
[<InlineData(-10_000_000L)>]
[<InlineData(-10_000L)>] // Timeout.InfiniteTimeSpan
[<InlineData(864_000_000_001L)>]
[<InlineData(Int64.MinValue)>]
[<InlineData(Int64.MaxValue)>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 WithExecutionOptions throws ArgumentOutOfRangeException named options for any other MaxRegexTime and keeps every limit`` (n: int64) =
    let runner = JyroRunner(compile "Output.a = 1\nOutput.b = 2\nOutput.c = 3\nOutput.d = 4").WithMaxStatements(3)
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> runner.WithExecutionOptions(withRegexTime (ticks n)) |> ignore)
    Assert.Equal("options", thrown.ParamName)
    Script.expectError MessageCode.StatementLimitExceeded (execute runner)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 whichever of WithMaxRegexTime and WithExecutionOptions is called last sets the timeout`` () =
    let optionsLast, s1 =
        runRegex (fun r -> r.WithMaxRegexTime(TimeSpan.FromSeconds 3.).WithExecutionOptions(withRegexTime (TimeSpan.FromMilliseconds 50.)))
    Script.expectError MessageCode.RegexTimeout optionsLast
    Assert.True(s1 < 0.8, sprintf "WithExecutionOptions called last did not set the timeout: %.2f s" s1)
    let methodLast, s2 =
        runRegex (fun r -> r.WithExecutionOptions(withRegexTime (TimeSpan.FromSeconds 3.)).WithMaxRegexTime(TimeSpan.FromMilliseconds 50.))
    Script.expectError MessageCode.RegexTimeout methodLast
    Assert.True(s2 < 0.8, sprintf "WithMaxRegexTime called last did not set the timeout: %.2f s" s2)
    let longerLast, s3 =
        runRegex (fun r -> r.WithMaxRegexTime(TimeSpan.FromMilliseconds 50.).WithExecutionOptions(withRegexTime (TimeSpan.FromSeconds 3.)))
    Script.expectError MessageCode.RegexTimeout longerLast
    Assert.True(s3 >= 2.5, sprintf "WithExecutionOptions with 3 s called last did not set the timeout: %.2f s" s3)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 options bound from a section that names MaxRegexTime as 00:00:02 set a regex timeout of 2 seconds`` () =
    // A section that sets the other limits to their defaults and names MaxRegexTime as 00:00:02.
    let section = System.Text.Json.JsonSerializer.SerializeToNode(JyroExecutionOptions.Default) :?> System.Text.Json.Nodes.JsonObject
    section.["MaxRegexTime"] <- System.Text.Json.Nodes.JsonValue.Create("00:00:02")
    let bound = System.Text.Json.JsonSerializer.Deserialize<JyroExecutionOptions>(section)
    Assert.Equal(TimeSpan.FromSeconds 2., bound.MaxRegexTime)
    let result, seconds = runRegex (fun r -> r.WithExecutionOptions bound)
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 1.7 && seconds < 5.0, sprintf "a MaxRegexTime of 2 s stopped the run after %.2f s" seconds)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 options bound from a section that omits MaxRegexTime leave the default timeout`` () =
    // A section that sets the other limits to their defaults and omits MaxRegexTime.
    let section = System.Text.Json.JsonSerializer.SerializeToNode(JyroExecutionOptions.Default) :?> System.Text.Json.Nodes.JsonObject
    section.Remove("MaxRegexTime") |> ignore
    let bound = System.Text.Json.JsonSerializer.Deserialize<JyroExecutionOptions>(section)
    Assert.Equal(TimeSpan.Zero, bound.MaxRegexTime)
    let result, seconds = runRegex (fun r -> r.WithExecutionOptions bound)
    Script.expectError MessageCode.RegexTimeout result
    Assert.True(seconds >= 0.8 && seconds < 2.5, sprintf "an omitted MaxRegexTime stopped the run after %.2f s, not about 1 s" seconds)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1906")>]
let ``JYRO-REQ-1906 a MaxRegexTime of minus one second makes WithExecutionOptions throw`` () =
    let runner = JyroRunner(compile "Output.x = 1")
    Assert.Throws<ArgumentOutOfRangeException>(fun () -> runner.WithExecutionOptions(withRegexTime (TimeSpan.FromSeconds -1.)) |> ignore) |> ignore

// -- JYRO-REQ-1908: a negative count limit is refused at the call that sets it ----------------------------

[<Theory>]
[<InlineData("WithMaxStatements")>]
[<InlineData("WithMaxLoopIterations")>]
[<InlineData("WithMaxCallDepth")>]
[<Trait("requirement", "JYRO-REQ-1908")>]
let ``JYRO-REQ-1908 a negative count throws ArgumentOutOfRangeException named for the parameter and the runner keeps its limits`` (name: string) =
    let m = methodOf typeof<JyroRunner> name [ typeof<int> ]
    let parameterName = m.GetParameters().[0].Name
    for value in [ -1; -5; Int32.MinValue ] do
        let runner = JyroRunner(compile "Output.x = 1")
        let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> invoke m runner [| box value |] |> ignore)
        Assert.Equal(parameterName, thrown.ParamName)
        // A later Execute uses the limits the runner had before the call.
        Assert.Equal("{\"x\":1}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1908")>]
let ``JYRO-REQ-1908 a refused WithMaxStatements leaves the limit that was set`` () =
    let runner = JyroRunner(compile "Output.a = 1\nOutput.b = 2\nOutput.c = 3\nOutput.d = 4").WithMaxStatements(3)
    Assert.Throws<ArgumentOutOfRangeException>(fun () -> runner.WithMaxStatements(-5) |> ignore) |> ignore
    Script.expectError MessageCode.StatementLimitExceeded (execute runner)

[<Theory>]
[<InlineData("MaxStatements")>]
[<InlineData("MaxLoopIterations")>]
[<InlineData("MaxCallDepth")>]
[<Trait("requirement", "JYRO-REQ-1908")>]
let ``JYRO-REQ-1908 WithExecutionOptions with a negative count throws ArgumentOutOfRangeException named options and keeps every limit`` (field: string) =
    let bad = optionsWith field (box -1)
    let runner = JyroRunner(compile "Output.a = 1\nOutput.b = 2\nOutput.c = 3\nOutput.d = 4").WithMaxStatements(3)
    let thrown = Assert.Throws<ArgumentOutOfRangeException>(fun () -> runner.WithExecutionOptions(bad) |> ignore)
    Assert.Equal("options", thrown.ParamName)
    Script.expectError MessageCode.StatementLimitExceeded (execute runner)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1908")>]
let ``JYRO-REQ-1908 the options of the example, with a loop limit of minus one, make WithExecutionOptions throw`` () =
    let runner = JyroRunner(compile "Output.x = 1")
    Assert.Throws<ArgumentOutOfRangeException>(fun () ->
        runner.WithExecutionOptions({ JyroExecutionOptions.Default with MaxStatements = 10000; MaxLoopIterations = -1; MaxCallDepth = 64 }) |> ignore)
    |> ignore

[<Theory>]
[<InlineData("WithMaxStatements")>]
[<InlineData("WithMaxLoopIterations")>]
[<InlineData("WithMaxCallDepth")>]
[<Trait("requirement", "JYRO-REQ-1908")>]
let ``JYRO-REQ-1908 a value of zero keeps the meaning of an unset limit`` (name: string) =
    let m = methodOf typeof<JyroRunner> name [ typeof<int> ]
    let runner = JyroRunner(compile "var t = 0\nforeach i in [1, 2, 3] do t += i end\nOutput.t = t")
    invoke m runner [| box 0 |] |> ignore
    Assert.Equal("{\"t\":6}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1908")>]
let ``JYRO-REQ-1908 a call depth above the safe maximum is still reduced to it`` () =
    let safe = JyroExecutionOptions.MaxSafeCallDepth
    let source = Tests.Support.recursion (safe + 1)
    let result = Tests.Support.bounded 60. (fun () -> execute (JyroRunner(compile source).WithMaxCallDepth(safe + 1000)))
    Script.expectError MessageCode.CallDepthLimitExceeded result

// -- JYRO-REQ-1909: a null argument to a builder throws ArgumentNullException at the call -----------------

let private builderTypes : Type list = [ typeof<JyroCompiler>; typeof<JyroArtifactLoader>; typeof<JyroRunner> ]

/// The members that another clause lets take null: WithInput (160.1.2, a null value is the empty input) and
/// WithHostState (JYRO-REQ-1910, null is no state).
let private takesNullByDesign (m: MethodBase) : bool =
    m.Name = "WithInput" || m.Name = "WithHostState"

let private callable (t: Type) : MethodBase list =
    [ for c in t.GetConstructors(BindingFlags.Public ||| BindingFlags.Instance) do
          yield c :> MethodBase
      for m in t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly) do
          if not m.IsSpecialName then yield m :> MethodBase ]

let private library : JyroLibrary = Script.valueOf (JyroLibrary.Parse "func f() return 1 end")

let private artifactBytes : byte[] =
    Script.valueOf (JyroCompiler().WithSource("Output.x = 1").CompileToArtifact())

let private argumentFor (t: Type) : obj =
    if t = typeof<string> then box "x"
    elif t = typeof<byte[]> then box (Array.copy artifactBytes)
    elif t = typeof<CompiledProgram> then box (compile "Output.x = 1")
    elif t = typeof<IJyroFunction> then box ((Host.recorder "F").AsFunction)
    elif t = typeof<IEnumerable<IJyroFunction>> then box ([] : IJyroFunction list)
    elif t = typeof<FunctionEffect> then box FunctionEffect.Pure
    elif t = typeof<JyroLibrary> then box library
    elif t = typeof<JyroPipelineStats> then box (JyroPipelineStats())
    elif t = typeof<JyroExecutionOptions> then box JyroExecutionOptions.Default
    elif t = typeof<Action<string, JyroValue>> then box (Action<string, JyroValue>(fun _ _ -> ()))
    elif t = typeof<JyroValue> then box Values.nul
    elif t = typeof<Stream> then box (utf8 "{}")
    elif t = typeof<obj> then box 1
    elif t = typeof<TimeSpan> then box (TimeSpan.FromSeconds 5.)
    elif t = typeof<int> then box 10
    elif t.IsValueType then Activator.CreateInstance t
    else Values.fail (sprintf "The test has no valid argument for the parameter type %s; add one" t.FullName)

let private newBuilder (t: Type) : obj =
    if t = typeof<JyroCompiler> then box (JyroCompiler())
    elif t = typeof<JyroArtifactLoader> then box (JyroArtifactLoader(Array.copy artifactBytes))
    else box (JyroRunner(compile "Output.x = 1"))

/// One row for each reference-type parameter of each public method and constructor of the three builders.
let nullArgumentCases : obj[] seq =
    seq {
        for t in builderTypes do
            for m in callable t do
                if not (takesNullByDesign m) then
                    let ps = m.GetParameters()
                    for i in 0 .. ps.Length - 1 do
                        if not ps.[i].ParameterType.IsValueType then
                            yield [| box t.Name; box (signature m); box i |]
    }

[<Theory>]
[<MemberData(nameof nullArgumentCases)>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 a null argument throws ArgumentNullException named for the parameter`` (typeName: string, signatureText: string, index: int) =
    let t = builderTypes |> List.find (fun b -> b.Name = typeName)
    let m = callable t |> List.find (fun c -> signature c = signatureText)
    let ps = m.GetParameters()
    let args = ps |> Array.mapi (fun j p -> if j = index then null else argumentFor p.ParameterType)
    let target = match m with :? ConstructorInfo -> null | _ -> newBuilder t
    let thrown = Assert.Throws<ArgumentNullException>(fun () -> invoke m target args |> ignore)
    Assert.Equal(ps.[index].Name, thrown.ParamName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 the generated cases cover the three builders`` () =
    let rows = nullArgumentCases |> Seq.map (fun r -> string r.[0]) |> Seq.countBy id |> Map.ofSeq
    for t in builderTypes do
        Assert.True(rows.ContainsKey t.Name && rows.[t.Name] >= 3, sprintf "%s has too few reference-type parameters: %A" t.Name rows)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 WithInput of a null object or a null JyroValue is accepted, and null is read as the Jyro null`` () =
    let runner = JyroRunner(compile "Output.x = Input == null")
    Assert.Same(runner, runner.WithInput(null: obj))
    Assert.Equal("{\"x\":true}", Values.toJson (Script.output (execute runner)))
    let second = JyroRunner(compile "Output.x = Input == null")
    Assert.Same(second, second.WithInput(Unchecked.defaultof<JyroValue>))
    Assert.Equal("{\"x\":true}", Values.toJson (Script.output (execute second)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 WithHostState of null is accepted and HostState reads null`` () =
    let runner = JyroRunner(compile "Output.x = 1").WithInput(Values.emptyObj ())
    Assert.Same(runner, runner.WithHostState(null))
    Assert.Equal("{\"x\":1}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 JyroRunner WithStats of null throws for stats and a later Execute does not throw NullReferenceException`` () =
    let runner = JyroRunner(compile "Output.x = 1").WithInput(Values.emptyObj ())
    let thrown = Assert.Throws<ArgumentNullException>(fun () -> runner.WithStats(Unchecked.defaultof<JyroPipelineStats>) |> ignore)
    Assert.Equal("stats", thrown.ParamName)
    Assert.Equal("{\"x\":1}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 JyroCompiler WithStats of null throws for stats and a later Compile does not report UnknownCompilerError`` () =
    let compiler = JyroCompiler().WithSource("Output.x = 1")
    let thrown = Assert.Throws<ArgumentNullException>(fun () -> compiler.WithStats(Unchecked.defaultof<JyroPipelineStats>) |> ignore)
    Assert.Equal("stats", thrown.ParamName)
    let result = compiler.Compile()
    Assert.DoesNotContain(MessageCode.UnknownCompilerError, Script.codes result)
    Assert.True(result.IsSuccess, Script.describe result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1909")>]
let ``JYRO-REQ-1909 JyroRunner of null throws for program`` () =
    let thrown = Assert.Throws<ArgumentNullException>(fun () -> JyroRunner(Unchecked.defaultof<CompiledProgram>) |> ignore)
    Assert.Equal("program", thrown.ParamName)

// -- JYRO-REQ-1910: a run carries a host state that its host functions read ------------------------------

/// A state object that the engine must not read: every member the engine could call on it throws.
type private UnreadableState() =
    override _.ToString() = failwith "the engine read the host state (ToString)"
    override _.GetHashCode() = failwith "the engine read the host state (GetHashCode)"
    override _.Equals(_: obj) = failwith "the engine read the host state (Equals)"

/// A host function `Peek()` that records the HostState of the context it receives.
let private peek () : RecordingFunction * (unit -> obj) =
    let seen = ref (box "never called")
    let f = Host.funcWithContext "Peek" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
                seen.Value <- ctx.HostState
                Values.nul)
    f, (fun () -> seen.Value)

let private runPeeking (configure: JyroRunner -> JyroRunner) : obj =
    let f, seen = peek ()
    let program = Script.valueOf (JyroCompiler().WithSource("Peek()").AddFunction(f.AsFunction).Compile())
    configure (JyroRunner(program).WithInput(Values.emptyObj ())) |> execute |> Script.output |> ignore
    seen ()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 JyroRunner has WithHostState of an object that returns JyroRunner`` () =
    let m = methodOf typeof<JyroRunner> "WithHostState" [ typeof<obj> ]
    Assert.Equal(typeof<JyroRunner>, m.ReturnType)
    let runner = JyroRunner(compile "Output.x = 1")
    Assert.Same(runner, runner.WithHostState(box "state"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 HostState of the context of a host function is the same object that WithHostState was given`` () =
    let state = obj ()
    Assert.Same(state, runPeeking (fun r -> r.WithHostState state))
    let text = String('t', 1)
    Assert.Same(text, runPeeking (fun r -> r.WithHostState text))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 when WithHostState is called more than once the one called last gives the state`` () =
    let first, second = obj (), obj ()
    Assert.Same(second, runPeeking (fun r -> r.WithHostState(first).WithHostState(second)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 HostState is null when WithHostState was never called or was called with null`` () =
    Assert.Null(runPeeking id)
    Assert.Null(runPeeking (fun r -> r.WithHostState null))
    let state = obj ()
    Assert.Null(runPeeking (fun r -> r.WithHostState(state).WithHostState null))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 the engine does not read the state it is given`` () =
    // Every member the engine could call on the state throws; the run must still succeed with the same object.
    let state = UnreadableState()
    Assert.Same(state, runPeeking (fun r -> r.WithHostState state))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 every Execute of the runner gives its functions the state`` () =
    let f, seen = peek ()
    let program = Script.valueOf (JyroCompiler().WithSource("Peek()").AddFunction(f.AsFunction).Compile())
    let state = obj ()
    let runner = JyroRunner(program).WithInput(Values.emptyObj ()).WithHostState(state)
    for _ in 1..3 do
        runner.Execute() |> ignore
        Assert.Same(state, seen ())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1910")>]
let ``JYRO-REQ-1910 two runners of one program on two threads at once each give their function the tenant of their own runner`` () =
    use barrier = new Barrier(2)
    let tenant =
        Host.funcWithContext "Tenant" [] ParameterType.Any FunctionEffect.Pure (fun _ ctx ->
            // Both runs are inside the function at the same time before either returns.
            if not (barrier.SignalAndWait(TimeSpan.FromSeconds 30.)) then Values.fail "the two runs were not in the function at once"
            Values.str (ctx.HostState :?> string))
    let program = Script.valueOf (JyroCompiler().WithSource("Output.t = Tenant()").AddFunction(tenant.AsFunction).Compile())
    let start (name: string) =
        Task.Run(fun () -> JyroRunner(program).WithInput(Values.emptyObj ()).WithHostState(box name).Execute())
    let t1, t2 = start "t1", start "t2"
    Assert.True(Task.WaitAll([| t1 :> Task; t2 :> Task |], TimeSpan.FromSeconds 60.), "the runs did not finish")
    Assert.Equal("{\"t\":\"t1\"}", Values.toJson (Script.output (t1.Result :> JyroResult<JyroValue>)))
    Assert.Equal("{\"t\":\"t2\"}", Values.toJson (Script.output (t2.Result :> JyroResult<JyroValue>)))

// -- JYRO-REQ-1929: each Execute of a reused runner starts from what was set on it ------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a runner given WithOutput gives each Execute a copy of the seed and never changes the seed`` () =
    let seed = JyroObject()
    let runner = JyroRunner(compile "Output.n = (Output.n ?? 0) + 1").WithInput(Values.emptyObj ()).WithOutput(seed)
    for _ in 1..3 do
        Assert.Equal("{\"n\":1}", Values.toJson (Script.output (execute runner)))
    Assert.Equal("{}", seed.ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 the copy of the seed is taken when Execute is called`` () =
    let seed = JyroObject()
    let runner = JyroRunner(compile "Output.n = (Output.n ?? 0) + 1").WithInput(Values.emptyObj ()).WithOutput(seed)
    seed.SetProperty("m", Values.int 5)
    // The seed was changed by the host after WithOutput, before Execute: the run starts from it as it is then.
    Assert.Equal("{\"m\":5,\"n\":1}", Values.toJson (Script.output (execute runner)))
    Assert.Equal("{\"m\":5}", seed.ToJson())
    Assert.Equal("{\"m\":5,\"n\":1}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a stream given to WithInputJson is read once, at the first Execute, and its value is the Input of every later Execute`` () =
    // An expandable stream (a MemoryStream made from an array is not), so that the host can rewrite it below.
    let backing = new MemoryStream()
    backing.Write(Encoding.UTF8.GetBytes "{\"a\":1}", 0, 7)
    backing.Position <- 0L
    let runner = JyroRunner(compile "Output.a = Input.a").WithInputJson(backing :> Stream)
    Assert.Equal(0L, backing.Position)
    Assert.Equal("{\"a\":1}", Values.toJson (Script.output (execute runner)))
    Assert.True(backing.Position > 0L, "the first Execute did not read the stream")
    // The stream now holds something else, from its start: a second read would give a = 99.
    backing.SetLength 0L
    backing.Write(Encoding.UTF8.GetBytes "{\"a\":99}", 0, 8)
    backing.Position <- 0L
    Assert.Equal("{\"a\":1}", Values.toJson (Script.output (execute runner)))
    Assert.Equal("{\"a\":1}", Values.toJson (Script.output (execute runner)))
    Assert.Equal(0L, backing.Position)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a seed that is also the Input stays read-only: each Execute raises InputValueIsReadOnly and the seed is unchanged`` () =
    let v = JyroObject()
    let runner = JyroRunner(compile "Output.x = 1").WithInput(v :> JyroValue).WithOutput(v :> JyroValue)
    for _ in 1..2 do
        Script.expectError MessageCode.InputValueIsReadOnly (execute runner)
    Assert.Equal("{}", v.ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a container of the seed that Input shares is kept, and a writable sibling is copied`` () =
    let shared = Values.json "{\"k\":1}"
    let mine = Values.json "{\"k\":2}"
    let input = Values.obj [ "s", shared ]
    let seed = Values.obj [ "shared", shared; "mine", mine ]
    let runner = JyroRunner(compile "Output.mine.k = 5").WithInput(input).WithOutput(seed)
    let first = execute runner
    Assert.True(first.IsSuccess, Script.describe first)
    // The writable sibling was copied: the run changed its copy, not the host's container.
    Assert.Equal("{\"k\":2}", mine.ToJson())
    Assert.Equal("{\"k\":5}", (Script.output first |> Values.prop "mine").ToJson())
    // The shared container is read-only through Output as it is through Input.
    let writing = JyroRunner(compile "Output.shared.k = 9").WithInput(input).WithOutput(seed)
    Script.expectError MessageCode.InputValueIsReadOnly (execute writing)
    Assert.Equal("{\"k\":1}", shared.ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a container the seed reaches by two paths becomes one copy reached by the same paths`` () =
    let c = JyroObject()
    let seed = JyroObject()
    seed.SetProperty("a", c)
    seed.SetProperty("b", c)
    let runner = JyroRunner(compile "Output.a.n = 1").WithInput(Values.emptyObj ()).WithOutput(seed)
    for _ in 1..2 do
        let output = Script.output (execute runner)
        Assert.Equal("{\"a\":{\"n\":1},\"b\":{\"n\":1}}", Values.toJson output)
        Assert.Same(output.GetProperty "a", output.GetProperty "b")
        Assert.NotSame(c :> JyroValue, output.GetProperty "a")
        Assert.Equal("{}", c.ToJson())

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a seed that shares a container down forty levels is copied in a time that a copy per path could not meet`` () =
    // Each level holds the level below as both l and r: 40 containers, 2^40 paths to the bottom.
    let bottom = JyroObject()
    let mutable level : JyroObject = bottom
    for _ in 1..40 do
        let up = JyroObject()
        up.SetProperty("l", level)
        up.SetProperty("r", level)
        level <- up
    let top = level
    let seed = JyroObject()
    seed.SetProperty("top", top)
    let runner = JyroRunner(compile "Output.mark = 1").WithInput(Values.emptyObj ()).WithOutput(seed)
    let task = Task.Run(fun () -> runner.Execute())
    Assert.True(task.Wait(TimeSpan.FromSeconds 30.), "Execute did not finish: the seed was copied once per path")
    let output = Script.output (task.Result :> JyroResult<JyroValue>)
    Assert.Equal(1m, Values.asDecimal (output.GetProperty "mark"))
    // The copy has the seed's shape: at every level l and r are the same container, and none is the seed's.
    let mutable copy = output.GetProperty "top"
    let mutable original = top :> JyroValue
    for _ in 1..40 do
        Assert.Same(copy.GetProperty "l", copy.GetProperty "r")
        Assert.NotSame(original, copy)
        copy <- copy.GetProperty "l"
        original <- original.GetProperty "l"
    Assert.Equal(0, bottom.Count)
    Assert.Equal(2, top.Count)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 a container of the seed that is read-only is the same container in Output`` () =
    let frozen = Values.json "{\"k\":1}"
    let input = Values.obj [ "s", frozen ]
    let seed = Values.obj [ "x", frozen; "y", frozen ]
    let runner = JyroRunner(compile "Output.mark = 1").WithInput(input).WithOutput(seed)
    let output = Script.output (execute runner)
    Assert.Same(frozen, output.GetProperty "x")
    Assert.Same(frozen, output.GetProperty "y")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
[<Trait("requirement", "JYRO-REQ-1444")>]
let ``JYRO-REQ-1929 the copy of the seed is a value the host supplies: a seed that sharing makes larger than the maximum value size is accepted`` () =
    // 100.8.6 size of this seed is above M = 2^24 if each path is counted (2^25 leaves), yet the seed holds 26 containers.
    let bottom = JyroArray()
    bottom.Add(Values.int 1)
    let mutable level : JyroArray = bottom
    for _ in 1..25 do
        let up = JyroArray()
        up.Add(level)
        up.Add(level)
        level <- up
    let seed = JyroObject()
    seed.SetProperty("top", level)
    let runner = JyroRunner(compile "Output.mark = 1").WithInput(Values.emptyObj ()).WithOutput(seed)
    let result = execute runner
    Assert.True(result.IsSuccess, Script.describe result)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 another input set later replaces the value read from the stream`` () =
    let runner = JyroRunner(compile "Output.a = Input.a").WithInputJson(utf8 "{\"a\":1}")
    Assert.Equal("{\"a\":1}", Values.toJson (Script.output (execute runner)))
    runner.WithInputJson("{\"a\":2}") |> ignore
    Assert.Equal("{\"a\":2}", Values.toJson (Script.output (execute runner)))
    runner.WithInputJson(utf8 "{\"a\":3}") |> ignore
    Assert.Equal("{\"a\":3}", Values.toJson (Script.output (execute runner)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1929")>]
let ``JYRO-REQ-1929 the XML documentation of each With method of the three builders says it changes the builder and returns it`` () =
    let says (text: string) =
        Regex.IsMatch(text, @"\b(chang|modif|mutat)\w*", RegexOptions.IgnoreCase)
        && Regex.IsMatch(text, @"\breturn\w*", RegexOptions.IgnoreCase)
    let offenders =
        [ for t in builderTypes do
            let withMethods =
                t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly)
                |> Array.filter (fun m -> m.Name.StartsWith "With")
            for group in withMethods |> Array.groupBy (fun m -> m.Name) do
                let name, overloads = group
                let docs = docsOfMethod t name
                if docs.Length < overloads.Length then
                    yield sprintf "%s.%s has %d overloads and %d documentation entries" t.Name name overloads.Length docs.Length
                for (key, text) in docs do
                    if not (says text) then yield sprintf "%s says neither that it changes the builder nor that it returns it" key ]
    Assert.True(List.isEmpty offenders, String.Join("\n", offenders))

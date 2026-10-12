/// 160.7.16 (DiagnosticMessage.Args), 160.7.17 (IMessageTemplateProvider and
/// DiagnosticFormatter.FormatLocalized) and 160.7.18 (FormatLocalized refuses a
/// null argument). From the book's Interface clauses alone (FLOORS 4.1.1).
///
/// FormatLocalized is reached by the C# name and parameter types 160.7.17
/// declares (Support.formatLocalized). The diagnostics it formats are taken from
/// real results: 160.7.15 declares no constructor of DiagnosticMessage. Every
/// run sets every limit explicitly (`pinned`, FLOORS 4.1.11).
module RealTree.Jyro.Spec.Tests.S160_07_DiagnosticArgsAndFormatter

open System
open System.Globalization
open System.Reflection
open System.Threading
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private flags = BindingFlags.Public ||| BindingFlags.Instance

/// The only error of running `source`.
let private errorOf (source: string) : DiagnosticMessage =
    Script.runWith pinned source |> Script.onlyErrorMessage

/// `source` at the top level, and as the body of a function called from the
/// top level (the `fail` is then at line 2, column 3).
let private inFunction (statement: string) : string =
    "func F()\n  " + statement + "\nend\nF()"

let private bothPlaces (statement: string) : (string * string) list =
    [ "top level", statement; "function body", inFunction statement ]

/// The elements of Args, each read as 160.7.16 reads it.
let private argsOf (d: DiagnosticMessage) : string list =
    Assert.NotNull(d.Args)
    d.Args |> Array.map argText |> List.ofArray

let private underCulture (name: string) (f: unit -> unit) =
    let c, u = CultureInfo.CurrentCulture, CultureInfo.CurrentUICulture
    let culture = CultureInfo.GetCultureInfo name
    CultureInfo.CurrentCulture <- culture
    CultureInfo.CurrentUICulture <- culture
    try f ()
    finally
        CultureInfo.CurrentCulture <- c
        CultureInfo.CurrentUICulture <- u

// ================================================================================================
// 160.7.16 DiagnosticMessage.Args
// ================================================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2110")>]
let ``DiagnosticMessage_HasPublicGetOnlyArgsOfTypeObjectArray`` () =
    let p = typeof<DiagnosticMessage>.GetProperty("Args", flags)
    if isNull p then Values.fail "160.7.16: DiagnosticMessage has no public instance property Args"
    Assert.Equal(typeof<obj[]>, p.PropertyType)
    Assert.NotNull(p.GetGetMethod())
    Assert.Null(p.GetSetMethod())

let failPlaces : obj[] seq =
    [ "top level"; "function body" ] |> Seq.map (fun p -> [| box p |])

let private placeOf (place: string) (statement: string) : string =
    bothPlaces statement |> List.find (fst >> (=) place) |> snd

[<Theory>]
[<MemberData(nameof failPlaces)>]
[<Trait("requirement", "JYRO-REQ-2110")>]
[<Trait("requirement", "JYRO-REQ-250")>]
let ``Args_OfFailWithAValue_IsTheOneText`` (place: string) =
    let d = errorOf (placeOf place "fail \"The order total is negative.\"")
    Assert.Equal(MessageCode.ScriptFailure, d.Code)
    Assert.Equal<string list>([ "The order total is negative." ], argsOf d)
    // Read by the invariant culture whatever the current one is.
    underCulture "de-DE" (fun () -> Assert.Equal<string list>([ "The order total is negative." ], argsOf d))

[<Theory>]
[<MemberData(nameof failPlaces)>]
[<Trait("requirement", "JYRO-REQ-2110")>]
[<Trait("requirement", "JYRO-REQ-250")>]
let ``Args_OfFailWithNoValue_IsEmpty`` (place: string) =
    let d = errorOf (placeOf place "fail")
    Assert.Equal(MessageCode.ScriptFailure, d.Code)
    Assert.NotNull(d.Args)
    Assert.True(d.Args.Length = 0, sprintf "Args of fail with no value is %A, not empty" (argsOf d))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2110")>]
[<Trait("requirement", "JYRO-REQ-353")>]
let ``Args_OfSumWithANonNumericElement_AreSumThen2`` () =
    let d = errorOf "Output.x = Sum([1, null, \"a\"])"
    Assert.Equal(MessageCode.NumericElementRequired, d.Code)
    // Exactly two elements, reading Sum and 2, in that order: not "Sum()", not a third.
    Assert.Equal<string list>([ "Sum"; "2" ], argsOf d)
    underCulture "de-DE" (fun () -> Assert.Equal<string list>([ "Sum"; "2" ], argsOf d))

/// Results of each member of the host API that returns diagnostics, and of
/// each kind of failure (4.1.8).
let private resultsOnEveryPath () : (string * DiagnosticMessage list) list =
    let msgs (r: JyroResult<'T>) = List.ofSeq r.Messages
    let thrower = Host.func "Throw" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (InvalidOperationException "boom"))
    use cts = new CancellationTokenSource()
    let c = canceller cts
    let limited f = { pinned with Limits = Some(Script.limits (fun o -> f (allLimits o))) }
    [ "compile error", msgs (Script.compileWith pinned "var s: string")
      "parse error", msgs (Script.compileWith pinned "var = (")
      "link error", msgs (Script.compileWith pinned "Output.x = Nope()")
      "stdlib runtime error", msgs (Script.runWith pinned "Output.x = PadLeft([1], 3)")
      "operator runtime error", msgs (Script.runWith pinned "Output.x = 1 / 0")
      "fail", msgs (Script.runWith pinned "fail \"x\"")
      "fail with no value", msgs (Script.runWith pinned "fail")
      "host function throws", msgs (Script.runWith { pinned with Functions = [ thrower.AsFunction ] } "Output.a = Throw()")
      "malformed JSON Input", msgs (Script.runWith { pinned with Input = JsonInput "{bad" } "Output.a = 1")
      "statement limit", msgs (Script.runWith (limited (fun o -> { o with MaxStatements = 1 })) "Output.a = 1\nOutput.b = 2")
      "cancel", msgs (Script.runWith { pinned with Functions = [ c.AsFunction ]; Cancellation = Some cts.Token } "CancelNow()\nOutput.a = 1")
      "no source", msgs ((JyroCompiler()).WithMaxCompilationTime(generousCompile).Compile())
      "compile to bytes", msgs (Script.compileBytesWith pinned "var s: string")
      "load of a bad artifact", msgs (Script.loadBytesWith pinned ([| 1uy; 2uy; 3uy |]))
      "artifact info", msgs (JyroArtifactInfo.Read "JYR"B) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2110")>]
let ``Args_AreNeverNull_OnEveryPath`` () =
    let all = resultsOnEveryPath ()
    for (name, ms) in all do
        Assert.False(ms.IsEmpty, sprintf "%s gave no diagnostic" name)
        for d in ms do
            Assert.True(not (isNull d.Args), sprintf "%s: %A has Args null" name d.Code)

// ================================================================================================
// 160.7.17 IMessageTemplateProvider and DiagnosticFormatter.FormatLocalized
// ================================================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``IMessageTemplateProvider_IsAPublicInterfaceWithTryGetTemplate`` () =
    let t = typeof<IMessageTemplateProvider>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsInterface && t.IsPublic)
    let m = t.GetMethod("TryGetTemplate", flags, null, [| typeof<MessageCode>; typeof<string>.MakeByRefType() |], null)
    if isNull m then Values.fail "160.7.17: IMessageTemplateProvider has no TryGetTemplate(MessageCode, out string)"
    Assert.Equal(typeof<bool>, m.ReturnType)
    Assert.True(m.GetParameters().[1].IsOut, "the second parameter of TryGetTemplate is out")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``DiagnosticFormatter_IsAPublicStaticClassWithFormatLocalized`` () =
    let m = formatLocalizedMethod ()
    let t = m.DeclaringType
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.Equal("DiagnosticFormatter", t.Name)
    // A C# static class is abstract and sealed.
    Assert.True(t.IsClass && t.IsPublic && t.IsAbstract && t.IsSealed)
    Assert.True(m.IsPublic && m.IsStatic)
    Assert.Equal(typeof<string>, m.ReturnType)

let noTemplateProviders : obj[] seq =
    [ [| box "returns false" |]; [| box "returns true with null" |] ]

let private providerNamed (name: string) : IMessageTemplateProvider =
    if name = "returns false" then NoTemplateProvider() :> _ else NullTemplateProvider() :> _

[<Theory>]
[<MemberData(nameof noTemplateProviders)>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``FormatLocalized_TopLevelFailBoom_IsTheClausesExample`` (provider: string) =
    let d = errorOf "fail \"boom\""
    Assert.Equal(MessageCode.ScriptFailure, d.Code)
    Assert.Equal("[JM5004] Ln 1, Col 1: boom", formatLocalized (providerNamed provider) d)

/// Sources whose one error is at a place: at line 1, in a function body
/// (line 2), at a two-digit line, and of several codes.
let private placedSources =
    [ "fail \"boom\""
      inFunction "fail \"boom\""
      "var s: string"
      String.Join("\n", [ for i in 1 .. 11 -> sprintf "Output.a%d = %d" i i ]) + "\nOutput.x = 1 / 0"
      "Output.a = 1\nOutput.b = 2\n    Output.x = PadLeft([1], 3)"
      "Output.x = zzz" ]

[<Theory>]
[<MemberData(nameof noTemplateProviders)>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``FormatLocalized_WithNoTemplate_IsThePrefixThenTheMessage`` (provider: string) =
    for source in placedSources do
        let d = Script.runWith pinned source |> Script.onlyErrorMessage
        Assert.True(d.Location.HasValue && d.Location.Value.Line > 0, sprintf "%A has no place" d.Code)
        let expected = formatPrefix d + d.Message
        Assert.Equal(expected, formatLocalized (providerNamed provider) d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``FormatLocalized_TheLineAndColumnAreThoseOfTheLocation`` () =
    // Two digits of line: the 12th line; the column of a statement indented by 4.
    let d = Script.runWith pinned (String.Join("\n", [ for i in 1 .. 11 -> sprintf "Output.a%d = %d" i i ]) + "\nOutput.x = 1 / 0") |> Script.onlyErrorMessage
    Assert.Equal(12, d.Location.Value.Line)
    let s = formatLocalized (NoTemplateProvider()) d
    Assert.StartsWith(sprintf "[JM%04d] Ln 12, Col %d: " (int d.Code) d.Location.Value.Column, s)
    let f = Script.runWith pinned (inFunction "fail \"boom\"") |> Script.onlyErrorMessage
    let l = f.Location.Value
    Assert.Equal(sprintf "[JM5004] Ln %d, Col %d: boom" l.Line l.Column, formatLocalized (NoTemplateProvider()) f)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``FormatLocalized_WithNoLocation_IsTheCodeThenAColon`` () =
    // Diagnostics with no place, from the paths that give them; none is chosen by its code.
    let unplaced =
        [ for (_, ms) in resultsOnEveryPath () do
            for d in ms do
                if not d.Location.HasValue then yield d ]
    Assert.False(unplaced.IsEmpty, "no path gave a diagnostic with no location")
    for d in unplaced do
        let expected = "[JM" + (int d.Code).ToString("D4", CultureInfo.InvariantCulture) + "]: " + d.Message
        Assert.Equal(expected, formatLocalized (NoTemplateProvider()) d)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1628")>]
let ``FormatLocalized_TheCodeIsItsNumberInFourDigits`` () =
    for source, number in [ "fail \"boom\"", "5004"; "var s: string", "3302"; "Output.x = 1 / 0", "5200"; "Output.x = zzz", "3100" ] do
        let d = Script.runWith pinned source |> Script.onlyErrorMessage
        Assert.StartsWith("[JM" + number + "]", formatLocalized (NoTemplateProvider()) d)

// Tagged JYRO-REQ-1713 alone (JYRO-ADR-113 item 4): that no placeholder is left is 130.6.3's
// rule, which 1713 states; 160.7.17 (JYRO-REQ-1628) only names the members it is read through.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1713")>]
let ``JYRO-REQ-1713 FormatLocalized_WithATemplate_IsThePrefixThenAMessageWithNoPlaceholder`` () =
    // 160.7.17 gives the message under a template as 130.6.1 and 130.6.3 give it;
    // what the book pins of it is that the line is the prefix then the message,
    // with no placeholder left (130.6.3). The ScriptFailure of fail, the exception
    // of 130.6.3, is tested in S130_Diagnostics.
    let placeholder = Text.RegularExpressions.Regex(@"\{\d+\}")
    for source in placedSources |> List.filter (fun s -> not (s.Contains "fail")) do
        let d = Script.runWith pinned source |> Script.onlyErrorMessage
        for template in [ "X{0}Y"; "{0} {1} {2}"; "no placeholder" ] do
            let message = localizedMessage (TemplateProvider(template, None)) d
            Assert.False(placeholder.IsMatch message, sprintf "%A under %A left a placeholder: %s" d.Code template message)

// ================================================================================================
// 160.7.18 FormatLocalized refuses a null argument
// ================================================================================================

let private refusal (f: unit -> string) : ArgumentNullException =
    let e = Assert.ThrowsAny<exn>(fun () -> f () |> ignore)
    Assert.IsType<ArgumentNullException>(e)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1629")>]
let ``FormatLocalized_NullProviderAndNullMsg_ThrowsForProviderFirst`` () =
    // The clause's example.
    let e = refusal (fun () -> formatLocalized Unchecked.defaultof<IMessageTemplateProvider> Unchecked.defaultof<DiagnosticMessage>)
    Assert.Equal("provider", e.ParamName)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1629")>]
let ``FormatLocalized_NullProvider_ThrowsArgumentNullExceptionProvider`` () =
    let d = errorOf "fail \"boom\""
    let e = refusal (fun () -> formatLocalized Unchecked.defaultof<IMessageTemplateProvider> d)
    Assert.Equal("provider", e.ParamName)

[<Theory>]
[<MemberData(nameof noTemplateProviders)>]
[<Trait("requirement", "JYRO-REQ-1629")>]
let ``FormatLocalized_NullMsg_ThrowsArgumentNullExceptionMsg`` (provider: string) =
    let e = refusal (fun () -> formatLocalized (providerNamed provider) Unchecked.defaultof<DiagnosticMessage>)
    Assert.Equal("msg", e.ParamName)
    let t = refusal (fun () -> formatLocalized (TemplateProvider("X{0}Y", None)) Unchecked.defaultof<DiagnosticMessage>)
    Assert.Equal("msg", t.ParamName)

// -- JYRO-REQ-2120 (160.7): a JyroRuntimeException makes its Message from its code and arguments ------

/// The Message (160.7.15) of a diagnostic of the code and the arguments of `e`: the message
/// that 130.6.1 makes with the default templates and no host template.
let private diagnosticMessageOf (e: JyroRuntimeException) : string =
    DiagnosticMessage.Error(e.Code, MessageStage.Execute, (if isNull e.Args then [||] else e.Args)).Message

/// What `f` throws, when it throws a JyroRuntimeException.
let private runtimeExceptionOf (f: unit -> unit) : Result<JyroRuntimeException, string> =
    try
        f ()
        Error "returned normally"
    with
    | :? JyroRuntimeException as e -> Ok e
    | e -> Error (sprintf "threw %s: %s" (e.GetType().FullName) e.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 the clause's example, ToJson of an unpaired surrogate, throws a Message equal to the diagnostic's`` () =
    let v = Values.obj [ "k", Values.str (string (char 0xD800)) ]
    match runtimeExceptionOf (fun () -> v.ToJson() |> ignore) with
    | Ok e ->
        Assert.Equal(diagnosticMessageOf e, e.Message)
    | Error what -> Values.fail ("ToJson of an unpaired surrogate (160.8.4): expected a JyroRuntimeException; it " + what)

/// Members of the host API a host can make throw a JyroRuntimeException.
let private throwSites : (string * (unit -> unit)) list =
    [ "ToJson, unpaired surrogate", (fun () -> (Values.str (string (char 0xDC00))).ToJson() |> ignore)
      "FromJson, invalid JSON", (fun () -> JyroValue.FromJson("{bad") |> ignore)
      "FromJson, a lone value with text after it", (fun () -> JyroValue.FromJson("1 2") |> ignore)
      "Item set, an index out of range", (fun () -> (JyroArray()).[5] <- Values.int 1)
      "ToInt32 of a fraction", (fun () -> (JyroNumber(1.5) :> JyroValue).ToInt32() |> ignore)
      "ToInt32 above Int32", (fun () -> (JyroNumber(1e12) :> JyroValue).ToInt32() |> ignore)
      "ToInt64 above Int64", (fun () -> (JyroNumber(1e300) :> JyroValue).ToInt64() |> ignore)
      "AsNumber of a string", (fun () -> (Values.str "a").AsNumber() |> ignore)
      "AsArray of a number", (fun () -> (Values.int 1).AsArray() |> ignore) ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 every JyroRuntimeException a host API member throws has the Message of a diagnostic of its Code and Args`` () =
    let mutable seen = 0
    for what, f in throwSites do
        match runtimeExceptionOf f with
        | Ok e ->
            seen <- seen + 1
            Assert.True((e.Message = diagnosticMessageOf e),
                        sprintf "%s: %A (args %A) has the Message %A; the diagnostic's is %A" what e.Code e.Args e.Message (diagnosticMessageOf e))
        | Error _ -> ()
    // The sites that throw are the members' own business beyond 160.8.4; at least the
    // clause's example must be among them.
    Assert.True(seen >= 1, "no site threw a JyroRuntimeException")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 a JyroRuntimeException thrown while a script runs has the Message of the diagnostic that ends the run`` () =
    // The clause's example: a host function that catches the exception of
    // raise (JyroError.Runtime(MessageCode.InvalidJson)) reads in its Message the Message of the
    // diagnostic with which the same call, not caught, ends the run. Also with arguments.
    for args in [ [||]; [| box "at"; box 7 |] ] do
        let caught = ref ""
        let catching =
            Host.func "Try" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
                try raise (JyroError.Runtime(MessageCode.InvalidJson, args))
                with :? JyroRuntimeException as e ->
                    caught.Value <- e.Message
                    Values.nul)
        let r1 = Script.runWith { pinned with Functions = [ catching.AsFunction ] } "Output.a = Try()"
        Assert.True(r1.IsSuccess, Script.describe r1)
        let raising = Host.func "Try" [] ParameterType.Any FunctionEffect.Pure (fun _ -> raise (JyroError.Runtime(MessageCode.InvalidJson, args)))
        let d = Script.runWith { pinned with Functions = [ raising.AsFunction ] } "Output.a = Try()" |> Script.onlyErrorMessage
        Assert.Equal(MessageCode.InvalidJson, d.Code)
        Assert.Equal(d.Message, caught.Value)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2120")>]
let ``JYRO-REQ-2120 JYRO-DEF-551 the exceptions a host function raises carry the diagnostic's message, not a host's text`` () =
    // Raised inside a host function during a run, where JYRO-REQ-1891 binds JyroError.Host, and
    // caught there: each JyroRuntimeException's Message is the diagnostic's of its Code and Args.
    // JyroError.Runtime makes a JyroRuntimeException (the clause's example catches one); which
    // exception type RaiseHost throws no clause gives, so its cases are judged only when it is one.
    let raisers : (string * bool * (unit -> JyroValue)) list =
        [ "JyroError.Runtime(InvalidJson)", true, (fun () -> raise (JyroError.Runtime(MessageCode.InvalidJson)))
          "JyroError.Runtime(DivisionByZero)", true, (fun () -> raise (JyroError.Runtime(MessageCode.DivisionByZero)))
          "JyroError.Host(AmountNotPositive, -5)", false, (fun () -> raise (JyroError.Host("AmountNotPositive", [| box -5 |])))
          "JyroError.Host(Refused)", false, (fun () -> raise (JyroError.Host("Refused"))) ]
    for what, required, raiser in raisers do
        let seen = ref None
        let f =
            Host.func "Try" [] ParameterType.Any FunctionEffect.Pure (fun _ ->
                try raiser ()
                with :? JyroRuntimeException as e ->
                    seen.Value <- Some (e.Message, diagnosticMessageOf e, e.Code)
                    Values.nul)
        let r = Script.runWith { pinned with Functions = [ f.AsFunction ] } "Output.a = Try()"
        match seen.Value with
        | Some (message, expected, code) ->
            Assert.True((message = expected), sprintf "%s: %A has the Message %A; the diagnostic's is %A" what code message expected)
        | None when required -> Values.fail (sprintf "%s did not throw a JyroRuntimeException. %s" what (Script.describe r))
        | None -> ()

/// 30.6 Function values and 30.7 Reference semantics.
module RealTree.Jyro.Spec.Tests.P30.S30_06_Functions

open System
open System.Collections.Generic
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support

/// A host function `CallIt(f, v)` that calls the function value `f` with `v`.
let private callIt () =
    Host.func "CallIt"
        [ Parameter.Required("f", ParameterType.Function); Parameter.Required("v", ParameterType.Any) ]
        ParameterType.Any FunctionEffect.Pure
        (fun args ->
            match args with
            | [ f; v ] ->
                Assert.Equal(JyroValueType.Function, f.ValueType)
                f.AsFunction().Invoke([| v |] :> IReadOnlyList<JyroValue>)
            | _ -> Values.fail "CallIt takes two arguments")

/// A host function `MakeAdder(n)` that returns a function value adding `n`.
let private makeAdder () =
    Host.func "MakeAdder" [ Parameter.Required("n", ParameterType.Number) ] ParameterType.Function FunctionEffect.Pure
        (fun args ->
            let n = Values.asFloat args.Head
            JyroFunction((fun (xs: IReadOnlyList<JyroValue>) -> Values.inexact (Values.asFloat xs.[0] + n)), 1) :> JyroValue)

// ---------------------------------------------------------------- 30.6.1

[<Theory>]
[<Trait("requirement", "JYRO-REQ-89")>]
[<InlineData("x => x")>]
[<InlineData("(x) => x")>]
[<InlineData("() => 1")>]
[<InlineData("(a, b) => a + b")>]
[<InlineData("[x => x * 2][0]")>]
[<InlineData("{f: y => y}.f")>]
let ``A lambda expression produces a function value`` (lambda: string) =
    let out = outOf ("var l = " + lambda + "\nOutput.t = TypeOf(l)\nOutput.f = l is function\nOutput.l = l")
    Assert.Equal("function", Values.asString (Values.prop "t" out))
    Assert.True(Values.asBool (Values.prop "f" out))
    Assert.Equal(JyroValueType.Function, (Values.prop "l" out).ValueType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-89")>]
let ``A lambda in a match expression case produces a function value`` () =
    let out =
        outOf (
            "union Op Inc end\n"
            + "var l = match Inc() do case Inc then x => x + 1 end\n"
            + "Output.t = TypeOf(l)\nOutput.r = Map([1], l)")
    Assert.Equal("function", Values.asString (Values.prop "t" out))
    Assert.Equal("[2]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.6.2

[<Fact>]
[<Trait("requirement", "JYRO-REQ-90")>]
let ``A function value is accepted as an argument to a standard library function`` () =
    let out =
        outOf (
            "var f = x => x * 2\nvar arr = [f]\nvar o = {g: f}\n"
            + "Output.r = [Map([1, 2], x => x * 2), Map([1, 2], f), Map([1, 2], arr[0]), Map([1, 2], o.g)]")
    Assert.Equal("[[2,4],[2,4],[2,4],[2,4]]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-90")>]
let ``A function value is accepted as an argument to a script function`` () =
    let out =
        outOf (
            "func apply(f, v)\n return Map([v], f)[0]\nend\n"
            + "func applyTyped(f: function, v)\n return Map([v], f)[0]\nend\n"
            + "Output.r = [apply(x => x + 1, 1), applyTyped(x => x + 2, 1)]")
    Assert.Equal("[2,3]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-90")>]
let ``A function value is accepted as an argument to a host function`` () =
    let f = callIt ()
    let r = Script.runWith { Script.defaults with Functions = [ f.AsFunction ] } "var g = x => x * 10\nOutput.r = [CallIt(x => x + 1, 1), CallIt(g, 2)]"
    Assert.Equal("[2,20]", Values.toJson (Values.prop "r" (Script.output r)))
    Assert.Equal(2, f.CallCount)

// ---------------------------------------------------------------- 30.6.3

[<Fact>]
[<Trait("requirement", "JYRO-REQ-91")>]
let ``A function value is accepted as the return value of a script function`` () =
    let out =
        outOf (
            "func mk(n)\n return x => x + n\nend\n"
            + "func fromArray()\n var fs = [x => x * 3]\n return fs[0]\nend\n"
            + "var f = mk(10)\n"
            + "Output.r = [Map([1], f)[0], Map([1], mk(10))[0], Map([2], fromArray())[0]]\n"
            + "Output.t = TypeOf(mk(1))")
    Assert.Equal("[11,11,6]", Values.toJson (Values.prop "r" out))
    Assert.Equal("function", Values.asString (Values.prop "t" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-91")>]
let ``A function value is accepted as the return value of a host function`` () =
    let f = makeAdder ()
    let r = Script.runWith { Script.defaults with Functions = [ f.AsFunction ] } "var g = MakeAdder(5)\nOutput.t = TypeOf(g)\nOutput.r = Map([1, 2], g)"
    let out = Script.output r
    Assert.Equal("function", Values.asString (Values.prop "t" out))
    Assert.Equal("[6,7]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.6.4

[<Fact>]
[<Trait("requirement", "JYRO-REQ-92")>]
let ``A function value is accepted as a variable, an element and a property`` () =
    let out =
        outOf (
            "var f = x => x + 1\nvar fs: function = x => x\n"
            + "var arr = [x => x * 2]\nvar o = {g: x => x * 3}\n"
            + "arr[1] = f\no.h = f\n"
            + "Output.r = [Map([1], f)[0], Map([1], arr[0])[0], Map([1], o.g)[0], Map([1], arr[1])[0], Map([1], o.h)[0], Map([1], fs)[0]]\n"
            + "Output.fn = f")
    Assert.Equal("[2,2,3,2,2,1]", Values.toJson (Values.prop "r" out))
    Assert.Equal(JyroValueType.Function, (Values.prop "fn" out).ValueType)

// ---------------------------------------------------------------- 30.6.5

/// `CallTargetNotName` (2003, the stage Parse), by its number: the member is
/// JYRO-REQ-1798's, and a test of another requirement does not need it declared.
let private callTargetNotName : MessageCode = enum<MessageCode> 2003

let callTargetSources () : obj[] seq =
    [ "var f = x => Rec(x)\nOutput.r = (f)(1)"
      "var o = {f: x => Rec(x)}\nOutput.r = o.f(1)"
      "var a = [x => Rec(x)]\nOutput.r = a[0](1)"
      "Output.r = (x => Rec(x))(1)"
      "func mk()\n return x => Rec(x)\nend\nOutput.r = mk()(1)"
      "Output.r = Input.f(1)"
      "Output.r = ((Length))([1])"
      "Output.r = Input.a.b.c(1)"
      "Output.r = \"abc\"(1)"
      "Output.r = [1](0)" ]
    |> Seq.map (fun s -> [| box s |])

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1798")>]
let ``JYRO-REQ-1798 CallTargetNotName is the member of 2003`` () =
    Assert.Equal("CallTargetNotName", Enum.GetName(typeof<MessageCode>, callTargetNotName))

[<Theory>]
[<MemberData(nameof callTargetSources)>]
[<Trait("requirement", "JYRO-REQ-1798")>]
let ``JYRO-REQ-1798 A call on an expression that is not a bare name is rejected at parse time and nothing runs`` (source: string) =
    let recorder = Host.recorder "Rec"
    let r = Script.runWith { Script.defaults with Functions = [ recorder.AsFunction ] } ("Rec(0)\n" + source)
    Assert.False(Script.ok r, "Expected the script to be rejected")
    Assert.Contains(callTargetNotName, Script.errorCodes r)
    for m in r.Messages do
        if m.Severity = MessageSeverity.Error && m.Code = callTargetNotName then
            Assert.Equal("Parse", Pending.stageOf m)
    Assert.Equal(0, recorder.CallCount)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1798")>]
[<InlineData("(f)(1)", "var f = 1\nOutput.r = {S}")>]
[<InlineData("o.f(1)", "var o = {f: 1}\nOutput.r = {S}")>]
[<InlineData("a[0](1)", "var a = [1]\nOutput.r = {S}")>]
[<InlineData("(x => x)(1)", "Output.r = {S}")>]
[<InlineData("mk()(1)", "func mk()\n return 1\nend\nOutput.r = {S}")>]
[<InlineData("Input.f(1)", "Output.r = {S}")>]
let ``JYRO-REQ-1798 The clause's six examples are each rejected with CallTargetNotName`` (call: string, template: string) =
    let source = template.Replace("{S}", call)
    let r = Script.compile source
    Assert.False(Script.ok r)
    Assert.Contains(callTargetNotName, Script.errorCodes r)

/// The refusal inside a lambda body and a named argument's value, where an enclosing
/// backtracking alternative could replace it with a generic parse error.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1798")>]
[<InlineData("Output.r = Map(Input.xs, x => x.f(1))")>]
[<InlineData("var g = x => (y)(1)")>]
[<InlineData("var g = (a) => o.f(1)")>]
[<InlineData("Output.r = Length(a: o.f(1))")>]
[<InlineData("Output.r = F(a: 1, b: (g)(1))")>]
let ``JYRO-REQ-1798 A call on a non-name inside a lambda body or a named argument is rejected with CallTargetNotName`` (source: string) =
    let r = Script.compile source
    Assert.False(Script.ok r)
    Assert.Contains(callTargetNotName, Script.errorCodes r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1798")>]
[<InlineData("func f(x)\n return x\nend\nOutput.r = f(1)")>]
[<InlineData("Output.r = Length([1])")>]
[<InlineData("Output.r = Input.a\n(1)")>]
[<InlineData("var x = 1\nOutput.r = x\n(2)")>]
let ``JYRO-REQ-1798 A plain call on a name, and a parenthesis on the next line, are not rejected with CallTargetNotName`` (source: string) =
    let r = Script.compile source
    Assert.DoesNotContain(callTargetNotName, Script.codes r)

/// UndefinedFunction (4100), the stage Link, for a call whose name is a variable, a
/// parameter or a lambda parameter and not a function.
let callNameSources () : obj[] seq =
    [ "var f = x => Rec(x)\nOutput.r = f(1)"
      "var f = 1\nOutput.r = f(1)"
      "var f = null\nOutput.r = f(1)"
      "var f = [x => Rec(x)]\nOutput.r = f(1)"
      "var f = x => Rec(x)\nvar g = f\nOutput.r = g(1)"
      "func apply(f, v)\n return f(v)\nend\nOutput.r = apply(x => Rec(x), 1)"
      "Output.r = Map([1], x => x(1))"
      "Output.r = Map([x => Rec(x)], g => g(1))"
      "var f = x => Rec(x)\nOutput.r = Map([1], y => f(y))" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<MemberData(nameof callNameSources)>]
[<Trait("requirement", "JYRO-REQ-1801")>]
let ``JYRO-REQ-1801 A call on a name that is a variable or a parameter is rejected with UndefinedFunction at Link and no function value is called`` (source: string) =
    let recorder = Host.recorder "Rec"
    let r = Script.runWith { Script.defaults with Functions = [ recorder.AsFunction ] } source
    Assert.False(Script.ok r, "Expected the script to be rejected")
    Assert.Contains(MessageCode.UndefinedFunction, Script.errorCodes r)
    for m in r.Messages do
        if m.Severity = MessageSeverity.Error && m.Code = MessageCode.UndefinedFunction then
            Assert.Equal("Link", Pending.stageOf m)
    Assert.Equal(0, recorder.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1801")>]
let ``JYRO-REQ-1801 The clause's example is rejected by the compile alone`` () =
    let recorder = Host.recorder "Rec"
    let r = Script.compileWith { Script.defaults with Functions = [ recorder.AsFunction ] } "var f = x => Rec(x)\nOutput.r = f(1)"
    Assert.False(Script.ok r)
    Assert.Contains(MessageCode.UndefinedFunction, Script.errorCodes r)
    Assert.Equal(0, recorder.CallCount)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1801")>]
let ``A host function and a standard library function can call a function value`` () =
    let f = callIt ()
    let r = Script.runWith { Script.defaults with Functions = [ f.AsFunction ] } "var g = x => x + 1\nOutput.r = [CallIt(g, 1), Map([1], g)[0], Find([1, 2], x => x > 1)]"
    Assert.Equal("[2,2,2]", Values.toJson (Values.prop "r" (Script.output r)))

// ---------------------------------------------------------------- 30.6.6

[<Theory>]
[<Trait("requirement", "JYRO-REQ-94")>]
[<InlineData("var g = f")>]
[<InlineData("Output.x = f")>]
[<InlineData("Output.x = Map([1], f)")>]
[<InlineData("Output.x = [f]")>]
[<InlineData("Output.x = {k: f}")>]
[<InlineData("Output.x = f == f")>]
[<InlineData("Output.x = TypeOf(f)")>]
[<InlineData("func g()\n return f\nend")>]
[<InlineData("Output.x = f is function")>]
[<InlineData("var h = x => f")>]
let ``The name of a declared function is not usable as a value`` (usage: string) =
    let r = Script.compile ("func f()\n return 1\nend\n" + usage)
    assertFails r
    let errors = Script.errorCodes r
    Assert.NotEmpty(errors)
    Assert.All(errors, fun c -> Assert.Equal(MessageCode.UndeclaredVariable, c))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-94")>]
let ``The name of a declared function is usable in a call`` () =
    assertNum 1M (rOf "func f()\n return 1\nend\nOutput.r = f()")

// ---------------------------------------------------------------- 30.7.1

[<Fact>]
[<Trait("requirement", "JYRO-REQ-95")>]
let ``Assigning an array or an object to a variable does not copy it`` () =
    let out = outOf "var a = [1]\nvar b = a\nb[0] = 9\nvar o = {k: 1}\nvar p = o\np.k = 9\nOutput.r = [a[0], o.k]"
    Assert.Equal("[9,9]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-95")>]
let ``Storing an array or an object as an element or a property does not copy it`` () =
    let out =
        outOf (
            "var a = [1]\nvar arr = [a]\narr[0][0] = 8\n"
            + "var o = {k: 1}\nvar h = {o: o}\nh.o.k = 8\n"
            + "var later = []\nlater[0] = a\nlater[0][0] = 7\n"
            + "var ho = {}\nho.x = o\nho.x.k = 7\n"
            + "Output.r = [a[0], o.k]")
    Assert.Equal("[7,7]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-95")>]
let ``Passing an array or an object as an argument does not copy it`` () =
    let out =
        outOf (
            "func poke(p, q)\n p[0] = 5\n q.k = 5\nend\n"
            + "var a = [1]\nvar o = {k: 1}\npoke(a, o)\n"
            + "First([a])[0] = 6\nFind([o], x => true).k = 6\n"
            + "Output.r = [a[0], o.k]")
    Assert.Equal("[6,6]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-95")>]
let ``Returning an array or an object from a function does not copy it`` () =
    let out =
        outOf (
            "func same(p)\n return p\nend\n"
            + "var a = [1]\nvar o = {k: 1}\n"
            + "same(a)[0] = 4\nsame(o).k = 4\n"
            + "var b = same(a)\nb[1] = 3\n"
            + "Output.r = [a, o.k]")
    Assert.Equal("[[4,3],4]", Values.toJson (Values.prop "r" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-95")>]
let ``A host function receives the script's own array`` () =
    let poke =
        Host.func "Poke" [ Parameter.Required("a", ParameterType.Array) ] ParameterType.Any FunctionEffect.Pure
            (fun args -> (args.Head :?> JyroArray).[0] <- Values.int 5; Values.nul)
    let r = Script.runWith { Script.defaults with Functions = [ poke.AsFunction ] } "var a = [1, 2]\nPoke(a)\nOutput.r = a"
    Assert.Equal("[5,2]", Values.toJson (Values.prop "r" (Script.output r)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-95")>]
let ``A function value stays the same value wherever it is stored or passed`` () =
    let out =
        outOf (
            "func same(p)\n return p\nend\n"
            + "var f = x => x\nvar g = f\n"
            + "Output.r = [g == f, [f][0] == f, {k: f}.k == f, same(f) == f, Find([f], x => true) == f]")
    Assert.Equal("[true,true,true,true,true]", Values.toJson (Values.prop "r" out))

// ---------------------------------------------------------------- 30.7.3

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1783")>]
let ``JYRO-REQ-1783 A variant call gives an object that is not its argument`` () =
    let out =
        outOf (
            "union Shape Circle(r) end\n"
            + "var a = {k: 1}\nvar c = Circle(a)\nc.extra = 2\n"
            + "Output.a = a\nOutput.c = c")
    Assert.Equal("{\"k\":1}", Values.toJson (Values.prop "a" out))
    Assert.NotSame(Values.prop "a" out, Values.prop "c" out)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1783")>]
let ``JYRO-REQ-1783 Clone gives no container that is the argument or reachable from it`` () =
    let out =
        outOf (
            "var o = {a: [1, {b: 2}], c: {d: [3]}}\nvar k = Clone(o)\n"
            + "k.x = 1\nk.a[0] = 9\nk.a[1].b = 9\nk.c.d[0] = 9\n"
            + "Output.o = o\nOutput.k = k")
    Assert.Equal("{\"a\":[1,{\"b\":2}],\"c\":{\"d\":[3]}}", Values.toJson (Values.prop "o" out))
    let o = Values.prop "o" out
    let k = Values.prop "k" out
    Assert.NotSame(o, k)
    Assert.NotSame(Values.prop "a" o, Values.prop "a" k)
    Assert.NotSame(Values.prop "c" o, Values.prop "c" k)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1783")>]
[<InlineData("Slice(a, 0)")>]
[<InlineData("Skip(a, 0)")>]
[<InlineData("Insert(a, 0, 0)")>]
[<InlineData("RemoveAt(a, 0)")>]
[<InlineData("Sort(a)")>]
[<InlineData("Append(a, 4)")>]
[<InlineData("Reverse(a)")>]
[<InlineData("Distinct(a)")>]
[<InlineData("Where(a, x => true)")>]
let ``JYRO-REQ-1783 An operation that returns a new array does not return its argument`` (call: string) =
    let out = outOf ("var a = [1, 2, 3]\nvar r = " + call + "\nr[0] = 99\nOutput.a = a\nOutput.r = r")
    Assert.Equal("[1,2,3]", Values.toJson (Values.prop "a" out))
    Assert.NotSame(Values.prop "a" out, Values.prop "r" out)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1783")>]
[<InlineData("Project(items, [\"a\"])")>]
[<InlineData("Omit(items, [\"b\"])")>]
let ``JYRO-REQ-1783 Project and Omit return new objects, not the elements`` (call: string) =
    let out = outOf ("var items = [{a: 1, b: 2}]\nvar r = " + call + "\nr[0].a = 99\nr[1] = 5\nOutput.items = items\nOutput.r = r")
    Assert.Equal("[{\"a\":1,\"b\":2}]", Values.toJson (Values.prop "items" out))
    Assert.NotSame(Values.prop "items" out, Values.prop "r" out)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1783")>]
[<InlineData("Clone(5)", "5")>]
[<InlineData("Clone(\"s\")", "\"s\"")>]
[<InlineData("Clone(null)", "null")>]
[<InlineData("Clone(true)", "true")>]
[<InlineData("ToUpper(\"ABC\")", "\"ABC\"")>]
[<InlineData("Trim(\"x\")", "\"x\"")>]
let ``JYRO-REQ-1783 A scalar result of a new-value operation is checked only for its value`` (call: string, json: string) =
    let out = outOf ("Output.r = [" + call + "]")
    Assert.Equal("[" + json + "]", Values.toJson (Values.prop "r" out))

/// Every container reachable from `v` (including `v`), by instance.
let rec private reachable (v: JyroValue) : JyroValue list =
    match v with
    | :? JyroArray -> (v :: (Values.asList v |> List.collect reachable))
    | :? JyroObject as o -> (v :: (o |> Seq.toList |> List.collect (fun kv -> reachable kv.Value)))
    | _ -> []

/// Calls that the specification requires to return a new value, each over the
/// argument `a` that the data gives; the sweep of JYRO-REQ-1783.
let private newValueCalls =
    [ "Concatenate", "[[1], {k: 2}, 3]", "Concatenate(a, [4])"
      "Concatenate second", "[[1], {k: 2}, 3]", "Concatenate([0], a)"
      "Distinct", "[[1], {k: 2}, 3, 3]", "Distinct(a)"
      "Sort", "[3, 1, 2]", "Sort(a)"
      "Slice", "[[1], {k: 2}, 3]", "Slice(a, 0)"
      "Skip", "[[1], {k: 2}, 3]", "Skip(a, 0)"
      "Reverse", "[[1], {k: 2}, 3]", "Reverse(a)"
      "Append", "[[1], {k: 2}, 3]", "Append(a, 4)"
      "Prepend", "[[1], {k: 2}, 3]", "Prepend(a, 4)"
      "Insert", "[[1], {k: 2}, 3]", "Insert(a, 0, 4)"
      "RemoveAt", "[[1], {k: 2}, 3]", "RemoveAt(a, 0)"
      "RemoveFirst", "[[1], {k: 2}, 3]", "RemoveFirst(a)"
      "RemoveLast", "[[1], {k: 2}, 3]", "RemoveLast(a)"
      "Where", "[[1], {k: 2}, 3]", "Where(a, x => true)"
      "Map", "[[1], {k: 2}, 3]", "Map(a, x => x)"
      "Flatten", "[[1], [[2]], 3]", "Flatten(a)"
      "FlattenOnce", "[[1], [[2]], 3]", "FlattenOnce(a)"
      "Merge", "[{p: 1}, {q: [2]}]", "Merge(a)"
      "Clone array", "[[1], {k: 2}, 3]", "Clone(a)"
      "Clone object", "{p: [1], q: {r: 2}}", "Clone(a)"
      "Project", "[{a: 1, b: [2]}, {c: 3}]", "Project(a, [\"a\"])"
      "Omit", "[{a: 1, b: [2]}, {c: 3}]", "Omit(a, [\"b\"])" ]

let newValueCallData () : obj[] seq = newValueCalls |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1783")>]
[<MemberData(nameof newValueCallData)>]
let ``JYRO-REQ-1783 A returned container is no argument and none reachable from it, and a write to it changes no argument`` (name: string) =
    let _, argument, call = newValueCalls |> List.find (fun (n, _, _) -> n = name)
    let out =
        outOf (
            "var a = " + argument + "\nvar before = ToJson(a)\nvar r = " + call + "\n"
            + "Output.a = a\nOutput.r = r\n"
            + "if r is array then\n if Length(r) > 0 then r[0] = \"W\" end\n r = Append(r, \"x\")\nelse\n r.written = \"W\"\nend\n"
            + "Output.same = before == ToJson(a)")
    let a = Values.prop "a" out
    let r = Values.prop "r" out
    for c in reachable a do
        Assert.False(obj.ReferenceEquals(c, r), sprintf "%s returned its argument or a container reachable from it" name)
    Assert.True(Values.asBool (Values.prop "same" out), "a write to the result changed the argument")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1783")>]
let ``JYRO-REQ-1783 A write to a returned array element or property of a flat result leaves the argument as it was`` () =
    let out =
        outOf (
            "var a = [1, 2, 3]\nvar b = {x: 1}\n"
            + "var s = Slice(a, 0)\ns[0] = 9\nvar c = Concatenate(a, a)\nc[1] = 8\n"
            + "var m = Merge([b])\nm.x = 7\nvar k = Clone(b)\nk.x = 6\n"
            + "Output.r = [a, b]")
    Assert.Equal("[[1,2,3],{\"x\":1}]", Values.toJson (Values.prop "r" out))

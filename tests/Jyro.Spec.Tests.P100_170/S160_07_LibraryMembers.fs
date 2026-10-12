/// 160.7: JyroLibrary.Parse parses a library ahead, and JyroCompiler.AddLibrary and
/// AddLibrarySource add a library (JYRO-REQ-2125, JYRO-ADR-139, for JYRO-DEF-569). By
/// reflection and by calls. Parameter names, null arguments and the other members of
/// JyroLibrary are left to the implementation and not tested. Written from the requirement alone.
module RealTree.Jyro.Spec.Tests.S160_07_LibraryMembers

open System
open System.Reflection
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

let private describe (r: JyroResult<'T>) : string =
    r.Messages
    |> Seq.map (fun m ->
        let pos = if m.Location.HasValue then sprintf "%d:%d" m.Location.Value.Line m.Location.Value.Column else "-"
        sprintf "%A %A [%s] at %s" m.Severity m.Code (Pending.stageOf m) pos)
    |> String.concat "\n"

// -- (1) The members -------------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 JyroLibrary is a public sealed class in RealTree.Jyro with a static Parse(String) returning JyroResult of JyroLibrary`` () =
    let t = typeof<JyroLibrary>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    Assert.True(t.IsClass && t.IsSealed && t.IsPublic, "JyroLibrary is not a public sealed class")
    let parse = t.GetMethod("Parse", BindingFlags.Public ||| BindingFlags.Static, null, [| typeof<string> |], null)
    Assert.NotNull(parse)
    Assert.Equal(typeof<JyroResult<JyroLibrary>>, parse.ReturnType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 JyroCompiler has the instance methods AddLibrarySource(String) and AddLibrary(JyroLibrary), each returning JyroCompiler`` () =
    let t = typeof<JyroCompiler>
    Assert.Equal("RealTree.Jyro", t.Namespace)
    let flags = BindingFlags.Public ||| BindingFlags.Instance
    let fromSource = t.GetMethod("AddLibrarySource", flags, null, [| typeof<string> |], null)
    Assert.NotNull(fromSource)
    Assert.Equal(typeof<JyroCompiler>, fromSource.ReturnType)
    let parsed = t.GetMethod("AddLibrary", flags, null, [| typeof<JyroLibrary> |], null)
    Assert.NotNull(parsed)
    Assert.Equal(typeof<JyroCompiler>, parsed.ReturnType)

// -- (2) The book examples -----------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 Parse of a library statement fails with LibraryStatementForbidden and no value`` () =
    let r = JyroLibrary.Parse "Output.x = 1"
    Assert.False(r.IsSuccess, describe r)
    Assert.False(r.HasValue)
    let found, _ = r.TryGetValue()
    Assert.False(found)
    Assert.Contains(r.Messages, fun m -> m.Code = MessageCode.LibraryStatementForbidden)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 Parse of a library matching a variant it does not declare succeeds with a value`` () =
    let r = JyroLibrary.Parse "func f() return match A() do case A then 1 end end"
    Assert.True(r.IsSuccess, describe r)
    Assert.True(r.HasValue)
    Assert.NotNull(Script.valueOf r)

// -- (3) Parse's result ------------------------------------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 Parse fails with no value when it reports an Error, and succeeds with the library and no Error otherwise`` () =
    for failing in [ "func f(\n  return 1\nend"; "var x = 1"; "func f(a, a)\n  return a\nend"; "union U A B end\nfunc f(u)\n  return match u do case A then 1 end\nend" ] do
        let r = JyroLibrary.Parse failing
        Assert.False(r.IsSuccess, sprintf "%A: %s" failing (describe r))
        Assert.False(r.HasValue, sprintf "%A has a value" failing)
        Assert.Contains(r.Messages, fun m -> m.Severity = MessageSeverity.Error)
    for passing in [ "func f()\n  return 1\nend"; "union U A B end"; "func g()\n  return Nope()\nend"; "func h(u)\n  return match u do case Q then 1 end\nend" ] do
        let r = JyroLibrary.Parse passing
        Assert.True(r.IsSuccess, sprintf "%A: %s" passing (describe r))
        Assert.True(r.HasValue, sprintf "%A has no value" passing)
        Assert.DoesNotContain(r.Messages, fun m -> m.Severity = MessageSeverity.Error)

// -- (4) The add methods return their compiler, and add in order -------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 AddLibrarySource and AddLibrary return the compiler they were called on`` () =
    let c = JyroCompiler()
    Assert.Same(c, c.AddLibrarySource "func f()\n  return 1\nend")
    let lib = JyroLibrary.Parse "func g()\n  return 2\nend" |> Script.valueOf
    Assert.Same(c, c.AddLibrary lib)
    // The calls chain: both libraries are in the compilation.
    let r = c.WithStdlib().WithSource("Output.r = f() + g()").Compile()
    Assert.True(r.IsSuccess, describe r)
    let run = (Script.runnerFor pinned (Script.valueOf r)).Execute()
    Assert.Equal("""{"r":3}""", Values.toJson (Script.output run))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 each add method adds one library after those already added by either method`` () =
    // Two libraries each holding an error on its own line: Nope1 on line 2, Nope2 on line 5.
    let a = "func fa()\n  return Nope1()\nend"
    let b = "\n\n\nfunc fb()\n  return Nope2()\nend"
    let pa = JyroLibrary.Parse a |> Script.valueOf
    let pb = JyroLibrary.Parse b |> Script.valueOf
    let lines (r: JyroResult<CompiledProgram>) =
        [ for m in r.Messages do
            if m.Code = MessageCode.UndefinedFunction && m.Location.HasValue then yield m.Location.Value.Line ]
    let check what (c: JyroCompiler) (expected: int list) =
        let r = c.WithSource("Output.r = 1").Compile()
        Assert.False(r.IsSuccess, what)
        Assert.True((lines r = expected), sprintf "%s: expected UndefinedFunction on lines %A, got %A.\n%s" what expected (lines r) (describe r))
    check "source a, then parsed b" ((Script.compilerFor pinned).AddLibrarySource(a).AddLibrary(pb)) [ 2; 5 ]
    check "parsed a, then source b" ((Script.compilerFor pinned).AddLibrary(pa).AddLibrarySource(b)) [ 2; 5 ]
    check "source a, then source b" ((Script.compilerFor pinned).AddLibrarySource(a).AddLibrarySource(b)) [ 2; 5 ]
    check "parsed a, then parsed b" ((Script.compilerFor pinned).AddLibrary(pa).AddLibrary(pb)) [ 2; 5 ]
    check "source b, then parsed a" ((Script.compilerFor pinned).AddLibrarySource(b).AddLibrary(pa)) [ 5; 2 ]
    check "parsed b, then source a" ((Script.compilerFor pinned).AddLibrary(pb).AddLibrarySource(a)) [ 5; 2 ]
    check "source b, then source a" ((Script.compilerFor pinned).AddLibrarySource(b).AddLibrarySource(a)) [ 5; 2 ]
    check "parsed b, then parsed a" ((Script.compilerFor pinned).AddLibrary(pb).AddLibrary(pa)) [ 5; 2 ]

// -- (5) AddLibrary leaves the checks that depend on the compilation to Compile() ------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2125")>]
let ``JYRO-REQ-2125 AddLibrary leaves the compilation's checks to Compile, which gives MatchUnknownVariant or compiles`` () =
    let lib = JyroLibrary.Parse "func f() return match A() do case A then 1 end end" |> Script.valueOf
    // Adding gives no diagnostic and does not throw; Compile() makes the checks.
    let alone = (Script.compilerFor pinned).AddLibrary(lib)
    let r = alone.WithSource("Output.x = f()").Compile()
    Assert.False(r.IsSuccess, describe r)
    let unknown = r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.MatchUnknownVariant) |> List.ofSeq
    Assert.False(unknown.IsEmpty, describe r)
    for m in unknown do Assert.Equal("Validate", Pending.stageOf m)
    // With another library, from source, declaring `union U A end`, it compiles and runs.
    let ok = (Script.compilerFor pinned).AddLibrarySource("union U A end").AddLibrary(lib).WithSource("Output.x = f()").Compile()
    Assert.True(ok.IsSuccess && ok.Messages.Count = 0, describe ok)
    let run = (Script.runnerFor pinned (Script.valueOf ok)).Execute()
    Assert.Equal("""{"x":1}""", Values.toJson (Script.output run))

/// 50.11 Exit and fail, 50.12 Delete.
module RealTree.Jyro.Spec.Tests.P40_60.S50_11_ExitFailDelete

open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P40_60.Support

let private noStdlib = { Script.defaults with Stdlib = false }

let private scriptFailures (r: JyroResult<JyroValue>) =
    r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.ScriptFailure) |> List.ofSeq

/// The single ScriptFailure diagnostic of a failed run.
let private onlyScriptFailure (r: JyroResult<JyroValue>) : DiagnosticMessage =
    Assert.False(r.IsSuccess, "the run fails")
    match scriptFailures r with
    | [ m ] -> m
    | l -> Values.fail (sprintf "Expected one ScriptFailure, got %d:\n%s" l.Length (Script.describe r))

let private argStrings (m: DiagnosticMessage) : string list =
    m.Args |> Array.map (fun a -> if isNull a then "<null>" else string a) |> List.ofArray

// ---- 50.11.1 -------------------------------------------------------------

[<Fact>]
[<Trait("requirement", "JYRO-REQ-247")>]
let Exit_EndsTheScript_SucceedsWithOutput () =
    let r = Script.run "Output.a = 1\nexit\nOutput.b = 2"
    Assert.True(r.IsSuccess)
    Assert.Equal(norm """{"a": 1}""", Values.toJson (Script.valueOf r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-247")>]
let Exit_InFunctionBody_EndsTheWholeScript () =
    let r = Script.run "func stop()\n  exit\nend\nforeach x in [1, 2, 3] do\n  Output.l = Append(Output.l ?? [], x)\n  if x == 2 then stop() end\n  Output.m = x\nend\nOutput.after = true"
    Assert.True(r.IsSuccess)
    Assert.Equal(norm """{"l": [1, 2], "m": 1}""", Values.toJson (Script.valueOf r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-247")>]
let Exit_WithValue_SucceedsWithOutputAsValue () =
    let r = Script.run "Output.a = 1\nexit \"msg\"\nOutput.b = 2"
    Assert.True(r.IsSuccess)
    Assert.Equal(norm """{"a": 1}""", Values.toJson (Script.valueOf r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-247")>]
let Exit_AfterOutputReplaced_ResultIsTheNewOutput () =
    let r = Script.run "Output = [1]\nexit\nOutput = [2]"
    Assert.True(r.IsSuccess)
    Assert.Equal(norm "[1]", Values.toJson (Script.valueOf r))

// ---- 50.11.2 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-248")>]
[<InlineData("exit \"done\"", "done")>]
[<InlineData("exit 42", "42")>]
[<InlineData("exit true", "true")>]
[<InlineData("exit false", "false")>]
[<InlineData("exit \"\"", "")>]
let ``JYRO-REQ-248 Exit_WithValue_CarriesTheTextAsExitMessage`` (source: string, expected: string) =
    let r = Script.run source
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(Some expected, Pending.exitMessage r)

/// The exit message is the text of the value (50.11.6), which is what `ToString` gives.
/// Compared with `ToString` rather than with a literal, so this tests that the result
/// carries the text, not the string form of a number (95.3.3, 95.3.4).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-248")>]
[<InlineData("1.50")>]
[<InlineData("-0.10")>]
[<InlineData("1e21")>]
[<InlineData("Power(2, 0.5)")>]
[<InlineData("[1, \"a\"]")>]
let ``JYRO-REQ-248 Exit_WithNumberOrContainerValue_CarriesItsToStringText`` (value: string) =
    let expected = Values.asString (rOfL (sprintf "Output.r = ToString(%s)" value))
    let r = Script.runWith limited (sprintf "Output.a = 1\nexit %s\nOutput.a = 2" value)
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(Some expected, Pending.exitMessage r)
    Assert.Equal(norm """{"a": 1}""", Values.toJson (Script.valueOf r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-248")>]
let ``JYRO-REQ-248 Exit_WithValue_ExitMessageIsNotADiagnostic`` () =
    let r = Script.run "Output.a = 1\nexit \"done-xyzzy\"\nOutput.a = 2"
    Assert.True(r.IsSuccess)
    Assert.Equal(Some "done-xyzzy", Pending.exitMessage r)
    Assert.Empty(r.Messages |> Seq.filter (fun m -> m.Message.Contains("done-xyzzy") || (argStrings m |> List.exists (fun a -> a.Contains "done-xyzzy"))))
    Assert.Empty(r.Messages |> Seq.filter (fun m -> m.Code = MessageCode.ScriptFailure))
    Assert.Equal(norm """{"a": 1}""", Values.toJson (Script.valueOf r))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-248")>]
[<InlineData("exit")>]
[<InlineData("Output.a = 1\nexit")>]
[<InlineData("func f()\n  exit\nend\nf()")>]
let ``JYRO-REQ-248 Exit_WithoutValue_CarriesNoExitMessage`` (source: string) =
    let r = Script.run source
    Assert.True(r.IsSuccess)
    Assert.Equal(None, Pending.exitMessage r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-248")>]
[<InlineData("Output.a = 1")>]
[<InlineData("fail \"x\"")>]
let ``JYRO-REQ-248 NoExit_CarriesNoExitMessage`` (source: string) =
    Assert.Equal(None, Pending.exitMessage (Script.run source))

// ---- 50.11.3 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-249")>]
[<InlineData("Output.a = 1\nfail\nOutput.b = 2")>]
[<InlineData("Output.a = 1\nfail \"x\"\nOutput.b = 2")>]
[<InlineData("func stop()\n  fail \"x\"\nend\nforeach q in [1, 2] do\n  Output.a = q\n  stop()\n  Output.b = 2\nend\nOutput.c = 3")>]
[<InlineData("func stop()\n  if true then fail end\nend\nOutput.a = 1\nstop()\nOutput.b = 2")>]
let Fail_EndsTheScriptWithScriptFailure (source: string) =
    let r = Script.run source
    Script.expectError MessageCode.ScriptFailure r
    Assert.Equal(norm """{"a": 1}""", failedOutputJson r)

// ---- 50.11.4 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-250")>]
[<InlineData("fail \"boom\"", "boom")>]
[<InlineData("fail \"\"", "")>]
[<InlineData("fail 5", "5")>]
[<InlineData("fail true", "true")>]
[<InlineData("fail \"Script execution failed\"", "Script execution failed")>]
[<InlineData("fail \"{0} it's\"", "{0} it's")>]
[<InlineData("fail \"a\\\"b\"", "a\"b")>]
let Fail_WithValue_HasOneArgumentTheTextAndMessageUnchanged (source: string, text: string) =
    let m = onlyScriptFailure (Script.run source)
    Assert.Equal<string list>([ text ], argStrings m)
    Assert.Equal(text, m.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-250")>]
let Fail_WithoutValue_HasNoArgumentAndDefaultMessage () =
    let m = onlyScriptFailure (Script.run "fail")
    Assert.Empty(m.Args)
    Assert.Equal("Script execution failed", m.Message)

// ---- 50.11.5 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-251")>]
[<InlineData("exit\nOutput.a = 1")>]
[<InlineData("exit\r\nOutput.a = 1")>]
[<InlineData("exit\n\"x\"")>]
[<InlineData("exit   \n  42")>]
let ``JYRO-REQ-251 Exit_LineEndAfterKeyword_MeansNoValue`` (source: string) =
    let r = Script.run source
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal("{}", Values.toJson (Script.valueOf r))
    Assert.Equal(None, Pending.exitMessage r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-251")>]
[<InlineData("fail\nOutput.a = 1")>]
[<InlineData("fail\r\nOutput.a = 1")>]
[<InlineData("fail\n\"x\"")>]
let ``JYRO-REQ-251 Fail_LineEndAfterKeyword_MeansNoValue`` (source: string) =
    let r = Script.run source
    let m = onlyScriptFailure r
    Assert.Empty(m.Args)
    Assert.Equal("{}", failedOutputJson r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-251")>]
[<InlineData("exit (\n1)", "1")>]
[<InlineData("exit (\r\n1 + 1)", "2")>]
[<InlineData("exit [\n1]", "[1]")>]
let ``JYRO-REQ-251 Exit_ValueStartingOnKeywordLine_MayContinueOnNextLine`` (source: string, expected: string) =
    let r = Script.run source
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(Some expected, Pending.exitMessage r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-251")>]
let ``JYRO-REQ-251 Fail_ValueOnKeywordLine_IsTheValue`` () =
    Assert.Equal<string list>([ "x" ], argStrings (onlyScriptFailure (Script.run "fail \"x\"")))
    Assert.Equal<string list>([ "3" ], argStrings (onlyScriptFailure (Script.run "fail (\n3)")))

// ---- 50.11.6 -------------------------------------------------------------

let private toStringOf (valueExpr: string) : string =
    rOf (sprintf "Output.r = ToString(%s)" valueExpr) |> Values.asString

[<Theory>]
[<Trait("requirement", "JYRO-REQ-252")>]
[<InlineData("[1]", "[1]", true)>]
[<InlineData("{a: 1}", "{\"a\":1}", true)>]
[<InlineData("[1]", "[1]", false)>]
[<InlineData("{a: 1}", "{\"a\":1}", false)>]
let ``JYRO-REQ-252 Exit_ContainerValue_TextIsToStringForm`` (value: string, expected: string, stdlib: bool) =
    let r = Script.runWith { Script.defaults with Stdlib = stdlib } (sprintf "exit %s" value)
    Assert.True(r.IsSuccess, Script.describe r)
    Assert.Equal(Some expected, Pending.exitMessage r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-252")>]
[<InlineData("{a: 1}", "{\"a\":1}", true)>]
[<InlineData("{a: 1}", "{\"a\":1}", false)>]
[<InlineData("[1]", "[1]", true)>]
[<InlineData("[1]", "[1]", false)>]
let ``JYRO-REQ-252 Fail_ContainerValue_ArgumentAndMessageAreToStringForm`` (value: string, expected: string, stdlib: bool) =
    let m = onlyScriptFailure (Script.runWith { Script.defaults with Stdlib = stdlib } (sprintf "fail %s" value))
    Assert.Equal<string list>([ expected ], argStrings m)
    Assert.Equal(expected, m.Message)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-252")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("0")>]
[<InlineData("-1.5")>]
[<InlineData("1e21")>]
[<InlineData("0.1")>]
[<InlineData("1e300")>]
[<InlineData("\"q\\\"t\"")>]
[<InlineData("{a: [1, {b: null}]}")>]
[<InlineData("[\"x\", true, null, 2.50]")>]
let ``JYRO-REQ-252 ExitAndFail_Text_EqualsToStringWithOrWithoutStdlib`` (value: string) =
    let expected = toStringOf value
    for stdlib in [ true; false ] do
        let opts = { Script.defaults with Stdlib = stdlib }
        let e = Script.runWith opts (sprintf "exit %s" value)
        Assert.Equal(Some expected, Pending.exitMessage e)
        let m = onlyScriptFailure (Script.runWith opts (sprintf "fail %s" value))
        Assert.Equal<string list>([ expected ], argStrings m)
        Assert.Equal(expected, m.Message)

// ---- 50.11.7 -------------------------------------------------------------

let private toStringError (setup: string) (valueExpr: string) : DiagnosticMessage =
    Script.onlyErrorMessage (Script.run (setup + sprintf "\nOutput.t = ToString(%s)" valueExpr))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-253")>]
[<InlineData("exit", "", "x => x", "InvalidType", true)>]
[<InlineData("fail", "", "x => x", "InvalidType", true)>]
[<InlineData("exit", "", "[x => x]", "InvalidType", true)>]
[<InlineData("fail", "", "{f: x => x}", "InvalidType", true)>]
[<InlineData("exit", "var o = {}\no.self = o", "o", "ValueNestingLimitExceeded", true)>]
[<InlineData("fail", "var o = {}\no.self = o", "o", "ValueNestingLimitExceeded", true)>]
[<InlineData("exit", "", "x => x", "InvalidType", false)>]
[<InlineData("fail", "", "x => x", "InvalidType", false)>]
[<InlineData("exit", "var o = {}\no.self = o", "o", "ValueNestingLimitExceeded", false)>]
[<InlineData("fail", "var a = []\na[0] = a", "a", "ValueNestingLimitExceeded", false)>]
let ``JYRO-REQ-253 ExitOrFail_ValueWithNoText_RaisesToStringsErrorInstead`` (keyword: string, setup: string, value: string, code: string, stdlib: bool) =
    let code = System.Enum.Parse<MessageCode>(code)
    let reference = toStringError setup value
    Assert.Equal(code, reference.Code)
    let src = (if setup = "" then "" else setup + "\n") + sprintf "Output.a = 1\n%s (%s)\nOutput.b = 2" keyword value
    let r = Script.runWith { Script.defaults with Stdlib = stdlib } src
    let m = Script.onlyErrorMessage r
    Assert.Equal(code, m.Code)
    Assert.Equal<string list>(argStrings reference, argStrings m)
    Assert.False(r.IsSuccess)
    Assert.Empty(scriptFailures r)
    Assert.Equal(None, Pending.exitMessage r)
    Assert.Equal(norm """{"a": 1}""", failedOutputJson r)

/// `exit` of a value with no text fails the statement, so the result of every member that
/// executes a script has no exit message (160.7.1: null after an error). Moved here from
/// JYRO-REQ-1584/1887's exit-message theory (JYRO-ADR-113 item 2, JYRO-DEF-505): this
/// requirement makes the case an error, so it depends on 1887.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-253")>]
let ``JYRO-REQ-253 Exit_OfAFunctionValue_FailsAndLeavesNoExitMessage_OnEveryMember`` () =
    let source = "exit (x => x)"
    let dict () = System.Collections.Generic.Dictionary<string, obj>()
    let results : (string * JyroResult<JyroValue>) list =
        [ "JyroRunner.Execute", Script.run source
          "JyroScript.ExecuteScript", JyroScript.executeScript source (box (dict ()))
          "JyroScript.ExecuteScriptWithJson", JyroScript.executeScriptWithJson source "{}" ]
    for name, r in results do
        Assert.False(r.IsSuccess, sprintf "%s: `exit (x => x)` succeeded. %s" name (Script.describe r))
        Assert.True((Pending.exitMessage r = None), sprintf "%s: exit message %A, expected none" name (Pending.exitMessage r))

// ---- 50.12.1 -------------------------------------------------------------

let private ddecls = "var x = 0\nvar k = \"a\"\nvar o = {a: {b: 1}}\nvar a = [{b: 1}]\nfunc f()\n  return {a: 1}\nend\n"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-254")>]
[<InlineData("delete o.a")>]
[<InlineData("delete o[\"a\"]")>]
[<InlineData("delete f().a")>]
[<InlineData("delete (o).a")>]
[<InlineData("delete a[0].b")>]
[<InlineData("delete o.a.b")>]
[<InlineData("delete o[k]")>]
[<InlineData("delete f()[\"a\"]")>]
let ``JYRO-REQ-254 Delete_PropertyOrIndexAccessTarget_PassesValidation`` (stmt: string) =
    notRejectedWith MessageCode.InvalidDeleteTarget (ddecls + stmt)
    compiles (ddecls + stmt)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-254")>]
[<InlineData("delete x")>]
[<InlineData("delete f()")>]
[<InlineData("delete 1")>]
[<InlineData("delete (o.a)")>]
[<InlineData("delete (o[\"a\"])")>]
[<InlineData("delete \"s\"")>]
[<InlineData("delete x++")>]
[<InlineData("delete -o.a")>]
[<InlineData("delete [1]")>]
[<InlineData("delete o.a ?? o.b")>]
[<InlineData("delete null")>]
[<InlineData("delete Output")>]
let ``JYRO-REQ-254 Delete_OtherTarget_IsRejectedWithInvalidDeleteTarget`` (stmt: string) =
    rejectsNotAtParse "JYRO-REQ-254" MessageCode.InvalidDeleteTarget (ddecls + stmt)

// ---- 50.12.2 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-255")>]
[<InlineData("var o = {a: 1, k: 2}\ndelete o.k", "[\"a\"]")>]
[<InlineData("var o = {a: 1, k: 2}\ndelete o[\"k\"]", "[\"a\"]")>]
[<InlineData("var o = {k: 2, a: 1}\nvar key = \"k\"\ndelete o[key]", "[\"a\"]")>]
[<InlineData("var o = {\"a.b\": 1, c: 2}\ndelete o[\"a.b\"]", "[\"c\"]")>]
[<InlineData("var o = {end: 1, c: 2}\ndelete o.end", "[\"c\"]")>]
[<InlineData("var o = {k: 1}\ndelete o.k", "[]")>]
[<InlineData("var o = {a: {k: 1}, k: 2}\ndelete o.a.k\ndelete o.k\no = o.a", "[]")>]
let ``JYRO-REQ-255 Delete_RemovesTheProperty`` (source: string, keys: string) =
    let out = Script.outputOf (source + "\nOutput.keys = Keys(o)\nOutput.has = HasProperty(o, \"k\")")
    Assert.Equal(norm keys, Values.toJson (Values.prop "keys" out))
    Assert.False(Values.asBool (Values.prop "has" out))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-255")>]
let ``JYRO-REQ-255 Delete_OnOutputProperty_RemovesIt`` () =
    assertOut """{"y": 2}""" "Output.x = 1\nOutput.y = 2\ndelete Output.x"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-255")>]
let ``JYRO-REQ-255 Delete_RemovedPropertyReadsNull`` () =
    assertR "null" "var o = {k: 2}\ndelete o.k\nOutput.r = o.k"

// ---- 50.12.3 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-256")>]
[<InlineData("var o = {a: 1}\ndelete o.zz", "{\"a\": 1}")>]
[<InlineData("var o = {a: 1}\ndelete o[\"zz\"]", "{\"a\": 1}")>]
[<InlineData("var o = {a: 1}\ndelete o.a\ndelete o.a", "{}")>]
[<InlineData("var o = {}\ndelete o.x", "{}")>]
[<InlineData("var o = {a: {}}\ndelete o.a.b", "{\"a\": {}}")>]
let Delete_MissingProperty_HasNoEffectAndNoError (source: string, expected: string) =
    assertR expected (source + "\nOutput.r = o")

// ---- 50.12.4 -------------------------------------------------------------

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1744")>]
[<InlineData("var a = [1]\ndelete a[0]")>]
[<InlineData("var a = [1]\ndelete a.x")>]
[<InlineData("var a = [1]\ndelete a[\"x\"]")>]
[<InlineData("var s = \"ab\"\ndelete s.x")>]
[<InlineData("var n = null\ndelete n.x")>]
[<InlineData("var n = 5\ndelete n.x")>]
[<InlineData("var b = true\ndelete b[\"x\"]")>]
[<InlineData("var f = x => x\ndelete f.x")>]
[<InlineData("var o = {a: 1}\ndelete o.a.b")>]
[<InlineData("var o = {}\ndelete o.missing.b")>]
let ``JYRO-REQ-1744 Delete_ValueNotAnObject_RaisesDeleteOnNonObject`` (source: string) =
    runtimeError MessageCode.DeleteOnNonObject source

/// Whatever `k` is: a value that is not an object raises DeleteOnNonObject for
/// a key of every type, and not another code.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1744")>]
[<InlineData("[1]", "0")>]
[<InlineData("[1]", "5")>]
[<InlineData("[1]", "-1")>]
[<InlineData("[1]", "1.5")>]
[<InlineData("[1]", "\"x\"")>]
[<InlineData("[1]", "null")>]
[<InlineData("[1]", "true")>]
[<InlineData("[1]", "[0]")>]
[<InlineData("[1]", "{}")>]
[<InlineData("null", "\"k\"")>]
[<InlineData("null", "0")>]
[<InlineData("null", "null")>]
[<InlineData("null", "[]")>]
[<InlineData("\"ab\"", "\"k\"")>]
[<InlineData("\"ab\"", "0")>]
[<InlineData("\"ab\"", "{}")>]
[<InlineData("5", "\"k\"")>]
[<InlineData("5", "false")>]
[<InlineData("true", "0")>]
[<InlineData("x => x", "\"k\"")>]
[<InlineData("x => x", "1")>]
let ``JYRO-REQ-1744 Delete_ValueNotAnObject_WhateverTheKeyIs_RaisesDeleteOnNonObject`` (value: string, key: string) =
    runtimeError MessageCode.DeleteOnNonObject (sprintf "var v = %s\nvar k = %s\ndelete v[k]" value key)

/// The "exactly when" other side: a value that is an object never raises it, for
/// a property it has and for one it has not.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1744")>]
[<InlineData("var o = {a: 1}\ndelete o.missing")>]
[<InlineData("var o = {a: 1}\ndelete o[\"missing\"]")>]
[<InlineData("var o = {a: 1}\ndelete o.a")>]
[<InlineData("var o = {}\ndelete o.k")>]
[<InlineData("var o = {a: {}}\ndelete o.a.b")>]
[<InlineData("var o = {a: [1]}\ndelete o.a")>]
let ``JYRO-REQ-1744 Delete_ValueIsAnObject_DoesNotRaiseDeleteOnNonObject`` (source: string) =
    let r = Script.run source
    Assert.True(Script.ok r, Script.describe r)

// ---- 50.12.5 -------------------------------------------------------------

/// Keys that have no text (95.3.8): an object and an array that hold
/// themselves. JYRO-REQ-258 gives one code for each case whatever the key is,
/// so a key whose text cannot be made must not change the code (JYRO-DEF-638,
/// JYRO-ADR-161).
let private cyclicKeys = "var ck = {}\nck.self = ck\nvar ca = [1]\nca[0] = ca\n"

[<Theory>]
[<Trait("requirement", "JYRO-REQ-258")>]
[<InlineData("1")>]
[<InlineData("1.5")>]
[<InlineData("true")>]
[<InlineData("null")>]
[<InlineData("[]")>]
[<InlineData("{}")>]
[<InlineData("fn")>]
[<InlineData("[1, {a: 2}]")>]
[<InlineData("{a: [1]}")>]
[<InlineData("ck")>]
[<InlineData("ca")>]
let ``JYRO-REQ-258 Delete_ObjectWithNonStringKey_RaisesInvalidIndexType`` (key: string) =
    runtimeError MessageCode.InvalidIndexType (sprintf "%svar fn = x => x\nvar o = {}\ndelete o[%s]" cyclicKeys key)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-258")>]
let ``JYRO-REQ-258 Delete_ObjectWithNumberKey_IsNotReadAsStringKeyNorAsMissingProperty`` () =
    let r = Script.run "var o = {\"1\": 1}\nOutput.o = o\ndelete o[1]"
    Script.expectError MessageCode.InvalidIndexType r
    Assert.Equal(norm """{"o": {"1": 1}}""", failedOutputJson r)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-258")>]
[<InlineData("var c = [1, 2]\nOutput.c = c\ndelete c[0]", "{\"c\": [1, 2]}")>]
[<InlineData("var c = [1, 2]\nOutput.c = c\ndelete c[\"x\"]", "{\"c\": [1, 2]}")>]
[<InlineData("var c = null\ndelete c[1]", "{}")>]
[<InlineData("var c = 5\ndelete c[1]", "{}")>]
[<InlineData("var c = \"ab\"\ndelete c[0]", "{}")>]
[<InlineData("var c = true\ndelete c[1]", "{}")>]
[<InlineData("var c = x => x\ndelete c[0]", "{}")>]
[<InlineData("var c = null\ndelete c[null]", "{}")>]
[<InlineData("var c = [1, 2]\nOutput.c = c\ndelete c[[0]]", "{\"c\": [1, 2]}")>]
[<InlineData("var c = [1, 2]\nOutput.c = c\ndelete c[{a: 1}]", "{\"c\": [1, 2]}")>]
[<InlineData("var c = 5\ndelete c[x => x]", "{}")>]
[<InlineData("var c = [1, 2]\nOutput.c = c\ndelete c[ck]", "{\"c\": [1, 2]}")>]
[<InlineData("var c = [1, 2]\nOutput.c = c\ndelete c[ca]", "{\"c\": [1, 2]}")>]
[<InlineData("var c = null\ndelete c[ck]", "{}")>]
[<InlineData("var c = 5\ndelete c[ca]", "{}")>]
[<InlineData("var c = \"ab\"\ndelete c[ck]", "{}")>]
let ``JYRO-REQ-258 Delete_IndexOnNonObject_RaisesDeleteOnNonObjectWhateverTheKey`` (source: string, output: string) =
    let r = Script.run (cyclicKeys + source)
    Script.expectError MessageCode.DeleteOnNonObject r
    Assert.Equal(norm output, failedOutputJson r)

/// 30.2.12 to 30.2.14 and 30.2.17 to 30.2.19: no number is NaN or an infinity.
module RealTree.Jyro.Spec.Tests.P30.S30_02_NonFinite

open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Text
open System.Text.Json.Nodes
open Newtonsoft.Json.Linq
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.P30.Support
open RealTree.Jyro.Spec.Tests.P30.HostNumbers

/// A host object with one public property.
type Holder(value: obj) =
    member _.Value = value

/// A host object with two public properties, declared A then B.
type Pair(a: obj, b: obj) =
    member _.A = a
    member _.B = b

/// Every number of a Jyro value, at every depth.
let rec private numbersOf (v: JyroValue) : float list =
    match v with
    | :? JyroNumber as n -> [ n.ToDouble() ]
    | :? JyroArray as a -> a |> Seq.collect numbersOf |> List.ofSeq
    | :? JyroObject as o -> o |> Seq.collect (fun kv -> numbersOf kv.Value) |> List.ofSeq
    | _ -> []

let private allFinite (v: JyroValue) =
    for d in numbersOf v do
        Assert.True(Double.IsFinite d, sprintf "a number that is not finite reached the host: %s" (d.ToString("R")))

// ---------------------------------------------------------------- 30.2.12

[<Fact>]
[<Trait("requirement", "JYRO-REQ-55")>]
let ``JYRO-REQ-55 A literal that is not finite is refused`` () =
    Script.expectError MessageCode.InvalidNumberFormat (Script.compile "Output.r = 1e400")

[<Theory>]
[<Trait("requirement", "JYRO-REQ-55")>]
[<InlineData("1e308 * Power(1, 0.5) * 10", MessageCode.NumberOverflow)>]
[<InlineData("-1e308 * Power(1, 0.5) * 10", MessageCode.NumberOverflow)>]
[<InlineData("Power(-8, 0.5)", MessageCode.NumberDomainError)>]
[<InlineData("Sum([1e308, 1e308])", MessageCode.NumberOverflow)>]
[<InlineData("Power(1e200, 2)", MessageCode.NumberOverflow)>]
let ``JYRO-REQ-55 Arithmetic that would give a value that is not finite raises instead`` (expr: string, code: MessageCode) =
    expectRun code ("Output.r = " + expr)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-55")>]
let ``JYRO-REQ-55 FromJson gives null for a number that is not finite`` () =
    let out = outOf "Output.r = FromJson(\"1e400\")\nOutput.s = FromJson(\"[1, -1e400]\")"
    Assert.Equal(JyroValueType.Null, (Values.prop "r" out).ValueType)
    Assert.Equal(JyroValueType.Null, (Values.prop "s" out).ValueType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-55")>]
let ``JYRO-REQ-55 The host cannot make a number that is not finite`` () =
    // The codes are 30.2.19's (JYRO-REQ-1700); 30.2.12 asks only that the host is refused.
    throwsCode (hostNumberNaN ()) (fun () -> JyroNumber(Double.NaN))
    throwsCode (hostNumberInfinite ()) (fun () -> JyroNumber(Double.PositiveInfinity))
    throwsCode (hostNumberInfinite ()) (fun () -> JyroValue.FromObject(box Double.NegativeInfinity))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-55")>]
let ``JYRO-REQ-55 Input holding NaN fails the execution before it starts`` () =
    let r = Script.runWith { Script.defaults with Input = HostInput (box [| box 1.0; box Double.NaN |]) } "Output.r = Input"
    assertFails r
    Assert.DoesNotContain(MessageCode.ScriptFailure, Script.codes r)
    Assert.Equal<MessageCode list>([ hostNumberNaN () ], Script.codes r)

/// JSON text of a number token far longer than 768 significant digits, and
/// whether its value is finite. Whether it is finite is decided from its digits
/// and exponent, at any length (JYRO-ADR-151 (5)(b), (5)(d)).
let private longTokens : (string * string * bool) list =
    let zeros n = String('0', n)
    [ "1 then 400 zero digits", "1" + zeros 400, false
      "-1 then 400 zero digits", "-1" + zeros 400, false
      "0. then 100000 zeros, 1e100400 (1e399)", "0." + zeros 100000 + "1e100400", false
      "0. then 100000 zeros, 1e100300 (1e299)", "0." + zeros 100000 + "1e100300", true
      "1 then 100000 zero digits, e-100000 (1)", "1" + zeros 100000 + "e-100000", true
      "11 then 1000 zero digits (1.1e1001)", "11" + zeros 1000, false
      "0. then 100000 ones", "0." + String('1', 100000), true
      "100000 nines, e-100200 (about 1e-200)", String('9', 100000) + "e-100200", true
      "0. then 100000 zeros, 1 (underflows to 0)", "0." + zeros 100000 + "1", true ]

let longTokenNames () : obj[] seq = longTokens |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-55")>]
[<MemberData(nameof longTokenNames)>]
let ``JYRO-REQ-55 FromJson decides whether a long number token is finite from its whole value`` (name: string) =
    let (_, text, finite) = longTokens |> List.find (fun (n, _, _) -> n = name)
    let r =
        Script.runWith { limited with Input = ValueInput (Values.obj [ "t", Values.str text; "u", Values.str ("[" + text + "]") ]) }
            "Output.r = FromJson(Input.t)\nOutput.s = FromJson(Input.u)[0]"
    let out = Script.output r
    for key in [ "r"; "s" ] do
        let v = Values.prop key out
        if finite then
            Assert.True((v.ValueType = JyroValueType.Number), sprintf "%s: %s gave %A, not a number" name key v.ValueType)
            Assert.True(Double.IsFinite(Values.asFloat v))
        else
            Assert.True((v.ValueType = JyroValueType.Null), sprintf "%s: %s gave %A, not null" name key v.ValueType)

/// The Jyro.Api package README, as the package ships it.
let private apiReadme () : string =
    use s = Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream("Jyro.Api.README.md")
    if isNull s then Values.fail "the Jyro.Api README is not embedded in the test assembly"
    use r = new StreamReader(s)
    r.ReadToEnd()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-55")>]
let ``JYRO-REQ-55 the Jyro.Api README does not tell a host that a number can be NaN or an infinity`` () =
    // JYRO-DEF-606: the package's own documentation said a number is a double for
    // "values out of decimal range, NaN, Infinity", and gave the exact range as
    // "28 to 29 significant digits". Every number is finite (30.2.10), a host value
    // holding NaN or an infinity is refused with HostNumberNaN or HostNumberInfinite
    // (30.2.17, 30.2.19), and the exact range is m / 10^s (30.2.4).
    let text = apiReadme ()
    Assert.DoesNotContain("NaN, Infinity", text)
    Assert.DoesNotContain("28 to 29 significant digits", text)
    Assert.Contains("HostNumberNaN", text)
    Assert.Contains("HostNumberInfinite", text)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-55")>]
let ``JYRO-REQ-55 An artifact holding a NaN number is refused`` () =
    let bytes = Script.valueOf (Script.compileBytesWith Script.defaults "Output.r = 1.5e300")
    let target = BitConverter.GetBytes(1.5e300)
    let at =
        [ 0 .. bytes.Length - 10 ]
        |> List.tryFind (fun i -> bytes.[i] = 0x03uy && bytes.[i + 1] = 0x00uy && bytes.[i + 2 .. i + 9] = target)
    match at with
    | None -> Values.fail "the inexact literal 1.5e300 is not in the artifact as 150.6.2 encodes it"
    | Some i ->
        let patched = Array.copy bytes
        Array.blit (BitConverter.GetBytes(Double.NaN)) 0 patched (i + 2) 8
        let r = Script.runBytesWith Script.defaults patched
        Script.expectError MessageCode.ArtifactUnreadable r

[<Theory>]
[<Trait("requirement", "JYRO-REQ-55")>]
[<InlineData("1.7976931348623157e308 * Power(1, 0.5) + 1")>]
[<InlineData("5e-324 * Power(1, 0.5) / 2")>]
[<InlineData("Power(10, -400)")>]
[<InlineData("[1e308 * Power(1, 0.5), {a: -1.7976931348623157e308}]")>]
let ``JYRO-REQ-55 Every number a successful execution gives the host is finite`` (expr: string) =
    allFinite (outOf ("Output.r = " + expr))

// ---------------------------------------------------------------- 30.2.13

[<Theory>]
[<Trait("requirement", "JYRO-REQ-56")>]
[<InlineData("Power(-8, 0.5)")>]
[<InlineData("Power(-8 * Power(1, 0.5), 0.5)")>]
[<InlineData("SquareRoot(-2 * Power(1, 0.5))")>]
[<InlineData("Log(-1 * Power(1, 0.5))")>]
[<InlineData("Power(-0.5 * Power(1, 0.5), 1.5)")>]
let ``An inexact operation whose result is NaN raises NumberDomainError`` (expr: string) =
    let r = Script.run ("Output.r = " + expr)
    Script.expectError MessageCode.NumberDomainError r

[<Theory>]
[<Trait("requirement", "JYRO-REQ-56")>]
[<InlineData("Power(8, 0.5)")>]
[<InlineData("SquareRoot(0 * Power(1, 0.5))")>]
[<InlineData("Power(-8, 3 * Power(1, 0.5))")>]
let ``An inexact operation next to the NaN cases succeeds`` (expr: string) =
    allFinite (outOf ("Output.r = " + expr))

// ---------------------------------------------------------------- 30.2.14

[<Theory>]
[<Trait("requirement", "JYRO-REQ-57")>]
[<InlineData("1e308 * i1 * 10")>]
[<InlineData("-1e308 * i1 * 10")>]
[<InlineData("1.7976931348623157e308 * i1 + 1e292")>]
[<InlineData("-1.7976931348623157e308 * i1 - 1e292")>]
[<InlineData("1 / Power(2, -1074 * Power(1, 0.5))")>]
[<InlineData("Power(1e200, 2)")>]
[<InlineData("Sum([1e308 * i1, 1e308])")>]
[<InlineData("1e308 * i1 + 1e308")>]
let ``An inexact operation whose result is an infinity raises NumberOverflow`` (expr: string) =
    let r = Script.run ("var i1 = " + I1 + "\nOutput.r = " + expr)
    Script.expectError MessageCode.NumberOverflow r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-57")>]
let ``An inexact sum that rounds back to the largest double succeeds`` () =
    assertInexact Double.MaxValue (evalR ("1.7976931348623157e308 * " + I1 + " + 1"))

// ---------------------------------------------------------------- 30.2.17

/// The bare host numbers that are not finite, and the name of the code 30.2.19
/// (JYRO-REQ-1700) gives each. Codes are given by name: see Support.codeOf.
let private nonFinite : (string * obj * string) list =
    [ "double NaN", box Double.NaN, "HostNumberNaN"
      "double +Inf", box Double.PositiveInfinity, "HostNumberInfinite"
      "double -Inf", box Double.NegativeInfinity, "HostNumberInfinite"
      "float NaN", box Single.NaN, "HostNumberNaN"
      "float +Inf", box Single.PositiveInfinity, "HostNumberInfinite"
      "float -Inf", box Single.NegativeInfinity, "HostNumberInfinite" ]

let private dictOf (pairs: (string * obj) list) : IDictionary<string, obj> =
    let d = Dictionary<string, obj>()
    for (k, v) in pairs do d.[k] <- v
    d :> IDictionary<string, obj>

/// Containers typed by the host number's own type, double or float, which a
/// host passes as they are (JYRO-ADR-152 (1), (2), (4); JYRO-DEF-602).
let private typed (v: obj) (ofDouble: float -> float -> obj) (ofSingle: float32 -> float32 -> obj) : obj =
    match v with
    | :? float as d -> ofDouble 1.0 d
    | :? float32 as f -> ofSingle 1.0f f
    | other -> failwithf "not a host double or float: %A" other

let private typedDict (one: 'T) (x: 'T) : obj =
    let d = Dictionary<string, 'T>()
    d.["a"] <- one
    d.["b"] <- x
    box d

/// A dictionary keyed by int, holding `one` at 1 and `x` at 2 (JYRO-ADR-154 (1b)).
let private intKeyed (one: 'T) (x: 'T) : obj =
    let d = Dictionary<int, 'T>()
    d.[1] <- one
    d.[2] <- x
    box d

/// A dictionary keyed by the number `x` itself (JYRO-ADR-154 (1d): a key is walked).
let private numberKeyed (x: 'T) : obj =
    let d = Dictionary<'T, int>()
    d.[x] <- 1
    box d

/// The shapes that hold a value at some depth, for each member of 30.2.17.
let private shapes : (string * (obj -> obj)) list =
    [ "object[]", (fun v -> box [| box 1; v |])
      "List<object>", (fun v -> box (List<obj>([ box 1; v ])))
      "Dictionary", (fun v -> box (dictOf [ "a", box 1; "b", v ]))
      "POCO", (fun v -> box (Holder(v)))
      "depth 3", (fun v -> box [| box (dictOf [ "k", box [| box 2; v |] ]) |])
      // JYRO-DEF-602: typed by the number, a non-generic dictionary, and types whose ToString is generated.
      "Dictionary<string, T>", (fun v -> typed v typedDict typedDict)
      "IReadOnlyDictionary<string, T>",
      (fun v -> typed v (fun o x -> box (ReadOnlyOnly<float>([ "a", o; "b", x ]))) (fun o x -> box (ReadOnlyOnly<float32>([ "a", o; "b", x ]))))
      "Hashtable", (fun v -> let h = Collections.Hashtable() in h.["b"] <- v; box h)
      "T[]", (fun v -> typed v (fun o x -> box [| o; x |]) (fun o x -> box [| o; x |]))
      "List<T>", (fun v -> typed v (fun o x -> box (List<float>([ o; x ]))) (fun o x -> box (List<float32>([ o; x ]))))
      "record-like (ToString of its own)", (fun v -> typed v (fun _ x -> box (RecordLike<float>(x))) (fun _ x -> box (RecordLike<float32>(x))))
      "ValueTuple", (fun v -> typed v (fun o x -> box (struct (o, x))) (fun o x -> box (struct (o, x))))
      "Dictionary<string, T> in an object[]", (fun v -> box [| typed v typedDict typedDict |])
      // JYRO-ADR-154 (1b), (1c), (1d): a dictionary not keyed by string is an array of
      // {Key, Value} entries, and each Key and Value is walked.
      "Dictionary<int, T>, as a value", (fun v -> typed v intKeyed intKeyed)
      "Dictionary<T, int>, as a key", (fun v -> typed v (fun _ x -> numberKeyed x) (fun _ x -> numberKeyed x))
      "Dictionary<object, object> with a string key", (fun v -> let d = Dictionary<obj, obj>() in d.[box "b"] <- v; box d)
      "Hashtable with an int key", (fun v -> let h = Collections.Hashtable() in h.[box 1] <- v; box h)
      // JYRO-ADR-156 (d): public fields, after public properties, for every type.
      "struct with a public field", (fun v -> typed v (fun _ x -> box (fieldStruct x)) (fun _ x -> box (fieldStruct x)))
      "class field after a property", (fun v -> typed v (fun o x -> box (propAndField o x)) (fun o x -> box (propAndField o x)))
      "Vector2", (fun v -> box (Vector2(float32 (Convert.ToDouble v), 0.0f)))
      "Quaternion", (fun v -> box (Quaternion(float32 (Convert.ToDouble v), 0.0f, 0.0f, 1.0f)))
      "Matrix4x4", (fun v -> let mutable m = Matrix4x4.Identity in m.M23 <- float32 (Convert.ToDouble v); box m)
      // JYRO-ADR-156 (e): a getter that throws for some values does not change the walk.
      "F# ValueSome", (fun v -> box (ValueSome v))
      "beside a throwing getter", (fun v -> box [| box (Guarded(false)); v |])
      // JYRO-ADR-157 (b1): a JsonValue made from a .NET value is that value, walked
      // as FromObject walks it passed directly (JYRO-DEF-620).
      "JsonValue of the number", (fun v -> typed v (fun _ x -> box (JsonValue.Create(x))) (fun _ x -> box (JsonValue.Create(x))))
      "JsonValue of an object[]", (fun v -> jsonValueOf [| box 1; v |])
      "JsonValue of T[]", (fun v -> typed v (fun o x -> jsonValueOf [| o; x |]) (fun o x -> jsonValueOf [| o; x |]))
      "JsonValue of a POCO", (fun v -> jsonValueOf (Holder(v)))
      "JsonValue of a POCO in a JsonObject",
      (fun v ->
          let o = JsonObject()
          o.["a"] <- JsonValue.Create(1)
          o.["b"] <- JsonValue.Create<Holder>(Holder(v))
          box o)
      // JYRO-ADR-157 (b2): a Newtonsoft JValue is its Value, walked (JYRO-DEF-623).
      "JValue", (fun v -> box (JValue(v)))
      "JArray of JValues",
      (fun v ->
          let a = JArray()
          a.Add(JValue(1L))
          a.Add(JValue(v))
          box a)
      "JObject",
      (fun v ->
          let o = JObject()
          o.Add("a", JValue(1L))
          o.Add("b", JValue(v))
          box o)
      // JYRO-ADR-157 (a1): a binary floating-point type from outside the BCL (JYRO-DEF-622).
      "ThirdPartyFloat", (fun v -> box [| box 1; box (ThirdPartyFloat(Convert.ToDouble v)) |]) ]

let nonFiniteData () : obj[] seq =
    nonFinite |> Seq.map (fun (label, v, code) -> [| box label; v; box code |])

let nonFiniteShapeData () : obj[] seq =
    seq {
        for (label, v, code) in nonFinite do
            for (shape, _) in shapes do
                yield [| box label; v; box code; box shape |]
    }

let private shape name = shapes |> List.find (fun (n, _) -> n = name) |> snd

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1947")>]
let ``JYRO-REQ-1947 the clause's examples raise the host-number code with no argument`` () =
    throwsCodeWithNoArgs (hostNumberNaN ()) (fun () -> JyroNumber(Double.NaN))
    throwsCodeWithNoArgs (hostNumberInfinite ()) (fun () -> JyroValue.FromObject(box [| box Double.PositiveInfinity |]))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof nonFiniteData)>]
let ``JYRO-REQ-1947 The JyroNumber double constructor refuses a number that is not finite`` (_label: string, value: obj, code: string) =
    let d = Convert.ToDouble(value)
    throwsCodeWithNoArgs (codeOf code) (fun () -> JyroNumber(d))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof nonFiniteData)>]
let ``JYRO-REQ-1947 FromObject refuses a host number that is not finite`` (_label: string, value: obj, code: string) =
    throwsCodeWithNoArgs (codeOf code) (fun () -> JyroValue.FromObject(value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof nonFiniteShapeData)>]
let ``JYRO-REQ-1947 FromObject refuses a host value holding a number that is not finite`` (_label: string, value: obj, code: string, shapeName: string) =
    throwsCodeWithNoArgs (codeOf code) (fun () -> JyroValue.FromObject((shape shapeName) value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof nonFiniteData)>]
let ``JYRO-REQ-1947 FromObject refuses a sequence holding a number that is not finite`` (_label: string, value: obj, code: string) =
    let code = codeOf code
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ box 1; value ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ box [| value |] ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ box (dictOf [ "x", box [| box 0; value |] ]) ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ box (Holder(value)) ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ (shape "Dictionary<string, T>") value ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ (shape "record-like (ToString of its own)") value ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ (shape "ValueTuple") value ])))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof nonFiniteData)>]
let ``JYRO-REQ-1947 FromObject refuses a dictionary holding a number that is not finite`` (_label: string, value: obj, code: string) =
    let code = codeOf code
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", value ]))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", box [| box 1; value |] ]))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", box (dictOf [ "b", box (List<obj>([ value ])) ]) ]))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", box (Holder(value)) ]))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", (shape "Hashtable") value ]))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", (shape "IReadOnlyDictionary<string, T>") value ]))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof nonFiniteData)>]
let ``JYRO-REQ-1947 FromObject refuses a plain object holding a number that is not finite`` (_label: string, value: obj, code: string) =
    let code = codeOf code
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder(value)))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder([| box 1; value |])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder(dictOf [ "k", box [| value |] ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder(Holder(value))))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject((shape "record-like (ToString of its own)") value))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder((shape "Dictionary<string, T>") value)))
    // JYRO-ADR-156 (d), (e): a plain object is read for its fields too, and the walk goes past a throwing getter.
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject((shape "struct with a public field") value))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject((shape "class field after a property") value))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject((shape "Vector2") value))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder((shape "Dictionary<int, T>, as a value") value)))

/// Host numbers of the other .NET number types that are not finite, or whose
/// nearest double is an infinity, and the code 30.2.19 decides (JYRO-ADR-156 (a):
/// every IBinaryInteger and IFloatingPointIeee754 type is a number).
let private otherNumberTypes () : (string * obj * string) list =
    let big = BigInteger.Pow(BigInteger(10), 400)
    [ "Half NaN", box Half.NaN, "HostNumberNaN"
      "Half +Inf", box Half.PositiveInfinity, "HostNumberInfinite"
      "Half -Inf", box Half.NegativeInfinity, "HostNumberInfinite"
      "NFloat NaN", box (Runtime.InteropServices.NFloat(Double.NaN)), "HostNumberNaN"
      "NFloat +Inf", box (Runtime.InteropServices.NFloat(Double.PositiveInfinity)), "HostNumberInfinite"
      "BigInteger 10^400", box big, "HostNumberInfinite"
      "BigInteger -10^400", box (BigInteger.Negate big), "HostNumberInfinite"
      // JYRO-ADR-157 (a1): types from outside the BCL, read through the members their
      // interfaces require (JYRO-DEF-621, JYRO-DEF-622).
      "ThirdPartyInteger 10^400", box (ThirdPartyInteger(big)), "HostNumberInfinite"
      "ThirdPartyInteger -10^400", box (ThirdPartyInteger(BigInteger.Negate big)), "HostNumberInfinite"
      "ThirdPartyFloat NaN", box (ThirdPartyFloat(Double.NaN)), "HostNumberNaN"
      "ThirdPartyFloat +Inf", box (ThirdPartyFloat(Double.PositiveInfinity)), "HostNumberInfinite"
      "ThirdPartyFloat -Inf", box (ThirdPartyFloat(Double.NegativeInfinity)), "HostNumberInfinite"
      // JYRO-ADR-158 (a2): a subclass of a number class is read as the T it is a T of
      // (JYRO-DEF-625), and a class that is a T of itself and of its base as itself
      // (JYRO-DEF-627); deciding which T never throws.
      "DerivedInteger 10^400", box (DerivedInteger(big)), "HostNumberInfinite"
      "DerivedFloat NaN", box (DerivedFloat(Double.NaN)), "HostNumberNaN"
      "DerivedFloat +Inf", box (DerivedFloat(Double.PositiveInfinity)), "HostNumberInfinite"
      "BaseFloat NaN", box (BaseFloat(Double.NaN)), "HostNumberNaN"
      "SelfAndBaseInteger 10^400", box (SelfAndBaseInteger(big)), "HostNumberInfinite"
      "SelfAndBaseInteger -10^400", box (SelfAndBaseInteger(BigInteger.Negate big)), "HostNumberInfinite"
      // JYRO-ADR-158 (a3), (a4): a binary float written by IEEE's conventions, and a
      // decimal-radix float, whose value is past the double range (JYRO-DEF-626, 628, 629).
      "IeeeConventionFloat NaN", box (IeeeConventionFloat(Double.NaN)), "HostNumberNaN"
      "IeeeConventionFloat -Inf", box (IeeeConventionFloat(Double.NegativeInfinity)), "HostNumberInfinite"
      "DecimalRadixFloat NaN", box DecimalRadixFloat.NaNValue, "HostNumberNaN"
      "DecimalRadixFloat 1e400", box (DecimalRadixFloat.Of(false, BigInteger.One, 400)), "HostNumberInfinite"
      "DecimalRadixFloat -15e399", box (DecimalRadixFloat.Of(true, BigInteger(15), 399)), "HostNumberInfinite"
      // JYRO-ADR-157 (b1): a JsonValue made from a .NET number is that number (JYRO-DEF-620).
      "JsonValue of NFloat NaN", jsonValueOf (Runtime.InteropServices.NFloat(Double.NaN)), "HostNumberNaN"
      "JsonValue of BigInteger 10^400", jsonValueOf big, "HostNumberInfinite"
      "JsonValue of ThirdPartyFloat NaN", jsonValueOf (ThirdPartyFloat(Double.NaN)), "HostNumberNaN"
      // JYRO-ADR-157 (b2): a JValue is its Value (JYRO-DEF-623).
      "JValue of double NaN", box (JValue(Double.NaN)), "HostNumberNaN"
      "JValue of BigInteger 10^400", box (JValue(box big)), "HostNumberInfinite" ]

let otherNumberTypeData () : obj[] seq =
    seq {
        for (n, _, _) in otherNumberTypes () do
            for s in [ "bare"; "object[]"; "Dictionary"; "POCO"; "class field after a property" ] do
                yield [| box n; box s |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof otherNumberTypeData)>]
let ``JYRO-REQ-1947 FromObject refuses a number of any dotnet number type, in any wrapper, that is not finite as a double`` (name: string, where: string) =
    let (_, v, code) = otherNumberTypes () |> List.find (fun (n, _, _) -> n = name)
    let value =
        match where with
        | "bare" -> v
        | "object[]" -> box [| box 1; v |]
        | "Dictionary" -> box (dictOf [ "a", box 1; "b", v ])
        | "POCO" -> box (Holder(v))
        | _ -> box (propAndField (box 1) v)
    throwsCodeWithNoArgs (codeOf code) (fun () -> JyroValue.FromObject(value))
    throwsCodeWithNoArgs (codeOf code) (fun () -> JyroValue.FromObject(List<obj>([ value ])))
    throwsCodeWithNoArgs (codeOf code) (fun () -> JyroValue.FromObject(dictOf [ "k", value ]))

/// The values JYRO-ADR-157 names that hold a number that is not finite inside a
/// JSON DOM, and the code 30.2.19 decides. Each must raise that code with no
/// argument, never System.Text.Json's ArgumentException or JsonException.
let private domNonFinite () : (string * obj * string) list =
    [ "JsonValue.Create(double[] {NaN})", jsonValueOf [| Double.NaN |], "HostNumberNaN"
      "JsonValue.Create(a POCO with +Infinity)", jsonValueOf (Holder(box Double.PositiveInfinity)), "HostNumberInfinite"
      "JsonValue.Create(NFloat.NaN)", jsonValueOf (Runtime.InteropServices.NFloat(Double.NaN)), "HostNumberNaN"
      "JsonArray holding JsonValue.Create(a POCO with NaN)", box (JsonArray(JsonValue.Create<Holder>(Holder(box Double.NaN)))), "HostNumberNaN"
      "JValue(NaN)", box (JValue(Double.NaN)), "HostNumberNaN"
      "JArray.Parse of [1, -Infinity]", box (JArray.Parse "[1, -Infinity]"), "HostNumberInfinite"
      "JObject.Parse of {a: NaN}", box (JObject.Parse "{\"a\": NaN}"), "HostNumberNaN" ]

let domNonFiniteNames () : obj[] seq = domNonFinite () |> Seq.map (fun (n, _, _) -> [| box n |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1947")>]
[<MemberData(nameof domNonFiniteNames)>]
let ``JYRO-REQ-1947 every host member refuses a JSON DOM value holding a number that is not finite with the host code`` (name: string) =
    let (_, v, code) = domNonFinite () |> List.find (fun (n, _, _) -> n = name)
    let code = codeOf code
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(v))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(List<obj>([ box 1; v ])))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(dictOf [ "a", v ]))
    throwsCodeWithNoArgs code (fun () -> JyroValue.FromObject(Holder(v)))

let private finiteEdges : obj list =
    [ box Double.MaxValue; box (-Double.MaxValue); box Double.Epsilon; box -0.0; box Single.MaxValue ]

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1947")>]
let ``JYRO-REQ-1947 The host members accept the finite extremes and keep them`` () =
    for e in finiteEdges do
        let d = Convert.ToDouble(e)
        Assert.Equal(d, JyroNumber(d).ToDouble())
        Assert.Equal(d, (JyroValue.FromObject(e)).ToDouble())
        Assert.Equal(d, (JyroValue.FromObject(box [| e |]) |> Values.asList |> List.head).ToDouble())
        Assert.Equal(d, (JyroValue.FromObject(List<obj>([ e ])) |> Values.asList |> List.head).ToDouble())
        Assert.Equal(d, (JyroValue.FromObject(dictOf [ "a", box [| e |] ]) |> Values.prop "a" |> Values.asList |> List.head).ToDouble())
        Assert.Equal(d, (JyroValue.FromObject(Holder(e)) |> Values.prop "Value").ToDouble())

// ---------------------------------------------------------------- 30.2.18

/// `JyroRunner.WithInputJson(String)` and `WithInputJson(Stream)` (JYRO-REQ-1948, which binds them; JYRO-ADR-201
/// item 5, JYRO-REQ-1904), by reflection. A build without them fails the calling test as JYRO-REQ-1948's own failure.
let private invokeInputJson (runner: JyroRunner) (parameterType: Type) (argument: obj) : JyroRunner =
    let m =
        typeof<JyroRunner>.GetMethod(
            "WithInputJson",
            Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance,
            null,
            [| parameterType |],
            null)
    if isNull m then Pending.notInBuild "JYRO-REQ-1948" (sprintf "JyroRunner.WithInputJson(%s)" parameterType.Name)
    else
        try m.Invoke(runner, [| argument |]) :?> JyroRunner
        with :? Reflection.TargetInvocationException as e when not (isNull e.InnerException) -> raise e.InnerException

let private withInputJson (runner: JyroRunner) (json: string) : JyroRunner = invokeInputJson runner typeof<string> (box json)

let private withInputJsonStream (runner: JyroRunner) (stream: Stream) : JyroRunner = invokeInputJson runner typeof<Stream> (box stream)

/// The Input members of 30.2.18 that take a host value. Script: `fail "ran"`.
/// (`JyroExtensions.ExecuteJyro` is not among them: it is removed, JYRO-REQ-1878.)
let private hostInputPaths : (string * (obj -> JyroResult<JyroValue>)) list =
    let program () = Script.valueOf (Script.compile "fail \"ran\"")
    [ "JyroRunner.WithInput(Object)", (fun v -> JyroRunner(program ()).WithInput(v).Execute())
      "JyroScript.ExecuteScript", (fun v -> executeScript("fail \"ran\"", v)) ]

/// The Input members of 30.2.18 that take JSON text.
let private jsonInputPaths : (string * (string -> JyroResult<JyroValue>)) list =
    let program () = Script.valueOf (Script.compile "fail \"ran\"")
    [ "JyroRunner.WithInputJson(String)", (fun t -> (withInputJson (JyroRunner(program ())) t).Execute())
      "JyroRunner.WithInputJson(Stream)",
      (fun t -> (withInputJsonStream (JyroRunner(program ())) (new MemoryStream(Encoding.UTF8.GetBytes t))).Execute())
      "JyroScript.executeScriptWithJson", (fun t -> executeScriptWithJson("fail \"ran\"", t)) ]

let private assertRefusedBeforeRunning (code: MessageCode) (r: JyroResult<JyroValue>) =
    assertFails r
    Assert.DoesNotContain(MessageCode.ScriptFailure, Script.codes r)
    Assert.True(r.Messages.Count = 1, sprintf "Expected exactly one diagnostic, got:\n%s" (Script.describe r))
    Assert.Equal(code, r.Messages.[0].Code)

let hostInputData () : obj[] seq =
    seq {
        for (path, _) in hostInputPaths do
            for (label, v, code) in nonFinite do
                yield [| box path; box label; box "top"; v; box code |]
                for (shapeName, _) in shapes do
                    yield [| box path; box label; box shapeName; v; box code |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1948")>]
[<MemberData(nameof hostInputData)>]
let ``JYRO-REQ-1948 A host Input holding a number that is not finite fails before the first statement`` (path: string, _label: string, shapeName: string, value: obj, code: string) =
    let input = if shapeName = "top" then value else (shape shapeName) value
    let run = hostInputPaths |> List.find (fun (n, _) -> n = path) |> snd
    let r = run input
    assertRefusedBeforeRunning (codeOf code) r

let jsonInputData () : obj[] seq =
    seq {
        for (path, _) in jsonInputPaths do
            for text in [ "1e400"; "-1e400"; "1.8e308"; "{\"a\":[1,{\"b\":1e400}]}"; "[2, -1.8e308]" ] do
                yield [| box path; box text |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1948")>]
[<MemberData(nameof jsonInputData)>]
let ``JYRO-REQ-1948 JSON Input holding a number beyond the double range fails before the first statement with NumberOverflow`` (path: string, text: string) =
    let run = jsonInputPaths |> List.find (fun (n, _) -> n = path) |> snd
    assertRefusedBeforeRunning MessageCode.NumberOverflow (run text)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1948")>]
[<InlineData("JyroRunner.WithInputJson(String)")>]
[<InlineData("JyroRunner.WithInputJson(Stream)")>]
[<InlineData("JyroScript.executeScriptWithJson")>]
let ``JYRO-REQ-1948 JSON Input at the edge of the double range is not refused`` (path: string) =
    let program () = Script.valueOf (Script.compile "Output.v = Input")
    let run (t: string) : JyroResult<JyroValue> =
        match path with
        | "JyroRunner.WithInputJson(String)" -> (withInputJson (JyroRunner(program ())) t).Execute()
        | "JyroRunner.WithInputJson(Stream)" -> (withInputJsonStream (JyroRunner(program ())) (new MemoryStream(Encoding.UTF8.GetBytes t))).Execute()
        | _ -> executeScriptWithJson("Output.v = Input", t)
    Assert.Equal(Double.MaxValue, (Script.output (run "1.7976931348623157e308") |> Values.prop "v").ToDouble())
    Assert.Equal(0.0, (Script.output (run "1e-400") |> Values.prop "v").ToDouble())

let domInputData () : obj[] seq =
    seq {
        for (path, _) in hostInputPaths do
            for (n, _, _) in domNonFinite () do
                yield [| box path; box n |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1948")>]
[<MemberData(nameof domInputData)>]
let ``JYRO-REQ-1948 A JSON DOM Input holding a number that is not finite fails before the first statement`` (path: string, name: string) =
    // JYRO-ADR-157 (b1), (b2): a JsonValue made from a .NET value and a Newtonsoft JToken (JYRO-DEF-620, JYRO-DEF-623).
    let (_, v, code) = domNonFinite () |> List.find (fun (n, _, _) -> n = name)
    let run = hostInputPaths |> List.find (fun (n, _) -> n = path) |> snd
    assertRefusedBeforeRunning (codeOf code) (run v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1948")>]
let ``JYRO-REQ-1948 A dictionary Input with NaN before an infinity reports HostNumberNaN once`` () =
    let d = dictOf [ "a", box Double.NaN; "b", box Double.PositiveInfinity ]
    Assert.Equal<string list>([ "a"; "b" ], d.Keys |> List.ofSeq)
    let typedD = Dictionary<string, float>()
    typedD.["a"] <- Double.NaN
    typedD.["b"] <- Double.PositiveInfinity
    for (_, run) in hostInputPaths do
        assertRefusedBeforeRunning (hostNumberNaN ()) (run (box d))
        assertRefusedBeforeRunning (hostNumberNaN ()) (run (box typedD))

// ---------------------------------------------------------------- 30.2.19

/// Cases of the walk order: the host value and the code its first non-finite number decides.
let private walkCases () : (string * obj * string) list =
    let aFirst = Dictionary<string, obj>()
    aFirst.["a"] <- box [| box 1; box [| box Double.NaN |] |]
    aFirst.["b"] <- box Double.PositiveInfinity
    let bFirst = Dictionary<string, obj>()
    bFirst.["b"] <- box Double.PositiveInfinity
    bFirst.["a"] <- box [| box 1; box [| box Double.NaN |] |]
    let typedNaNFirst = Dictionary<string, float>()
    typedNaNFirst.["a"] <- Double.NaN
    typedNaNFirst.["b"] <- Double.PositiveInfinity
    let typedInfFirst = Dictionary<string, float>()
    typedInfFirst.["a"] <- Double.NegativeInfinity
    typedInfFirst.["b"] <- Double.NaN
    let keyInfValueNaN = Dictionary<float, float>()
    keyInfValueNaN.[Double.PositiveInfinity] <- Double.NaN
    let keyNaNValueInf = Dictionary<float, float>()
    keyNaNValueInf.[Double.NaN] <- Double.PositiveInfinity
    [ "[+Inf, NaN]", box [| box Double.PositiveInfinity; box Double.NaN |], "HostNumberInfinite"
      "[NaN, +Inf]", box [| box Double.NaN; box Double.PositiveInfinity |], "HostNumberNaN"
      "[-Inf, NaN]", box [| box Double.NegativeInfinity; box Double.NaN |], "HostNumberInfinite"
      "{a: [1, [NaN]], b: +Inf}", box aFirst, "HostNumberNaN"
      "{b: +Inf, a: [1, [NaN]]}", box bFirst, "HostNumberInfinite"
      "[[1, [+Inf]], NaN]", box [| box [| box 1; box [| box Double.PositiveInfinity |] |]; box Double.NaN |], "HostNumberInfinite"
      "[[1, [NaN]], -Inf]", box [| box [| box 1; box [| box Double.NaN |] |]; box Double.NegativeInfinity |], "HostNumberNaN"
      "POCO A=NaN, B=+Inf", box (Pair(box Double.NaN, box Double.PositiveInfinity)), "HostNumberNaN"
      "POCO A=+Inf, B=NaN", box (Pair(box Double.PositiveInfinity, box Double.NaN)), "HostNumberInfinite"
      "List [+Inf, NaN]", box (List<obj>([ box Double.PositiveInfinity; box Double.NaN ])), "HostNumberInfinite"
      "List [-Inf, NaN]", box (List<obj>([ box Double.NegativeInfinity; box Double.NaN ])), "HostNumberInfinite"
      "float NaN", box Single.NaN, "HostNumberNaN"
      "float +Inf", box Single.PositiveInfinity, "HostNumberInfinite"
      "float -Inf", box Single.NegativeInfinity, "HostNumberInfinite"
      "NaN", box Double.NaN, "HostNumberNaN"
      "+Inf", box Double.PositiveInfinity, "HostNumberInfinite"
      "-Inf", box Double.NegativeInfinity, "HostNumberInfinite"
      // JYRO-DEF-602 under JYRO-ADR-152: containers typed by the number, and a type whose ToString is its own.
      "Dictionary<string, double> {a: NaN, b: +Inf}", box typedNaNFirst, "HostNumberNaN"
      "Dictionary<string, double> {a: -Inf, b: NaN}", box typedInfFirst, "HostNumberInfinite"
      "double[] [NaN, +Inf]", box [| Double.NaN; Double.PositiveInfinity |], "HostNumberNaN"
      "List<double> [+Inf, NaN]", box (List<float>([ Double.PositiveInfinity; Double.NaN ])), "HostNumberInfinite"
      "record-like A=NaN, B=+Inf", box (RecordPair<float>(Double.NaN, Double.PositiveInfinity)), "HostNumberNaN"
      "record-like A=+Inf, B=NaN", box (RecordPair<float>(Double.PositiveInfinity, Double.NaN)), "HostNumberInfinite"
      "ValueTuple (+Inf, NaN)", box (struct (Double.PositiveInfinity, Double.NaN)), "HostNumberInfinite"
      "ValueTuple (NaN, -Inf)", box (struct (Double.NaN, Double.NegativeInfinity)), "HostNumberNaN"
      // JYRO-ADR-154 (1d): an entry of a dictionary not keyed by string is {Key, Value}, Key first.
      "Dictionary<double, double> {+Inf: NaN}", box keyInfValueNaN, "HostNumberInfinite"
      "Dictionary<double, double> {NaN: +Inf}", box keyNaNValueInf, "HostNumberNaN"
      "Dictionary<int, double> {1: NaN, 2: +Inf}", intKeyed Double.NaN Double.PositiveInfinity, "HostNumberNaN"
      "Dictionary<int, double> {1: -Inf, 2: NaN}", intKeyed Double.NegativeInfinity Double.NaN, "HostNumberInfinite"
      // JYRO-ADR-156 (d): public properties, then public fields, are the object's order and the walk's.
      "[struct X=NaN, +Inf]", box [| box (fieldStruct Double.NaN); box Double.PositiveInfinity |], "HostNumberNaN"
      "[struct X=+Inf, NaN]", box [| box (fieldStruct Double.PositiveInfinity); box Double.NaN |], "HostNumberInfinite"
      "class P=+Inf, field F=NaN", box (propAndField Double.PositiveInfinity Double.NaN), "HostNumberInfinite"
      "class P=NaN, field F=-Inf", box (propAndField Double.NaN Double.NegativeInfinity), "HostNumberNaN"
      "[Vector2(NaN, 0), +Inf]", box [| box (Vector2(Single.NaN, 0.0f)); box Double.PositiveInfinity |], "HostNumberNaN"
      "[Quaternion(+Inf, 0, 0, 1), NaN]", box [| box (Quaternion(Single.PositiveInfinity, 0.0f, 0.0f, 1.0f)); box Double.NaN |], "HostNumberInfinite"
      // JYRO-ADR-156 (a): every .NET number type is a number of the walk.
      "[Half NaN, +Inf]", box [| box Half.NaN; box Double.PositiveInfinity |], "HostNumberNaN"
      "[BigInteger 10^400, NaN]", box [| box (BigInteger.Pow(BigInteger(10), 400)); box Double.NaN |], "HostNumberInfinite"
      // JYRO-ADR-156 (e): a getter that throws leaves its property out, and the walk goes on.
      "[ValueNone, Guarded(false), NaN]", box [| box (ValueNone: float voption); box (Guarded(false)); box Double.NaN |], "HostNumberNaN"
      // JYRO-ADR-157 (b1): a JsonValue made from a .NET value is walked as that value (JYRO-DEF-620).
      "[JsonValue of [NaN], +Inf]", box [| jsonValueOf [| Double.NaN |]; box Double.PositiveInfinity |], "HostNumberNaN"
      "[JsonValue of [+Inf], NaN]", box [| jsonValueOf [| Double.PositiveInfinity |]; box Double.NaN |], "HostNumberInfinite"
      "JsonValue of POCO A=+Inf, B=NaN", jsonValueOf (Pair(box Double.PositiveInfinity, box Double.NaN)), "HostNumberInfinite"
      "JsonValue of POCO A=NaN, B=-Inf", jsonValueOf (Pair(box Double.NaN, box Double.NegativeInfinity)), "HostNumberNaN"
      "[JsonValue of NFloat NaN, +Inf]", box [| jsonValueOf (Runtime.InteropServices.NFloat(Double.NaN)); box Double.PositiveInfinity |], "HostNumberNaN"
      // JYRO-ADR-157 (b2): a JArray and a JObject are walked in their order (JYRO-DEF-623).
      "JArray [+Inf, NaN]", box (JArray(JValue(Double.PositiveInfinity), JValue(Double.NaN))), "HostNumberInfinite"
      "JObject {a: NaN, b: +Inf}", box (JObject(JProperty("a", Double.NaN), JProperty("b", Double.PositiveInfinity))), "HostNumberNaN"
      "JObject {a: -Inf, b: NaN}", box (JObject(JProperty("a", Double.NegativeInfinity), JProperty("b", Double.NaN))), "HostNumberInfinite"
      // JYRO-ADR-157 (a1): number types from outside the BCL (JYRO-DEF-621, JYRO-DEF-622).
      "[ThirdPartyFloat NaN, +Inf]", box [| box (ThirdPartyFloat(Double.NaN)); box Double.PositiveInfinity |], "HostNumberNaN"
      "[ThirdPartyInteger 10^400, NaN]", box [| box (ThirdPartyInteger(BigInteger.Pow(BigInteger(10), 400))); box Double.NaN |], "HostNumberInfinite"
      // JYRO-ADR-158: number classes outside the BCL (JYRO-DEF-625 to JYRO-DEF-629).
      "[DerivedFloat NaN, +Inf]", box [| box (DerivedFloat(Double.NaN)); box Double.PositiveInfinity |], "HostNumberNaN"
      "[DerivedInteger 5, SelfAndBaseInteger 7, +Inf, NaN]",
      box [| box (DerivedInteger(BigInteger(5))); box (SelfAndBaseInteger(BigInteger(7))); box Double.PositiveInfinity; box Double.NaN |], "HostNumberInfinite"
      "[IeeeConventionFloat smallest subnormal, NaN]", box [| box (IeeeConventionFloat(Double.Epsilon)); box Double.NaN |], "HostNumberNaN"
      "[DecimalRadixFloat 1.5, DecimalRadixFloat 1e400, NaN]",
      box [| box (DecimalRadixFloat.Of(false, BigInteger(15), -1)); box (DecimalRadixFloat.Of(false, BigInteger.One, 400)); box Double.NaN |], "HostNumberInfinite" ]

let walkCaseNames () : obj[] seq = walkCases () |> Seq.map (fun (n, _, _) -> [| box n |])

let private walkCase name = walkCases () |> List.find (fun (n, _, _) -> n = name)

/// Every member of 30.2.18 that takes a host value, for every walk case.
let walkInputData () : obj[] seq =
    seq {
        for (path, _) in hostInputPaths do
            for (n, _, _) in walkCases () do
                yield [| box path; box n |]
    }

/// The code is never one of an operation's result (30.2.13, 30.2.14).
let private assertNotOperationCode (code: MessageCode) =
    Assert.NotEqual(MessageCode.NumberDomainError, code)
    Assert.NotEqual(MessageCode.NumberOverflow, code)

/// Run `action`; it must throw the JyroRuntimeException of `code`, which is
/// neither NumberDomainError nor NumberOverflow.
let private throwsHostCode (code: MessageCode) (action: unit -> 'T) =
    let caught = try action () |> ignore; None with e -> Some e
    match caught with
    | Some (:? JyroRuntimeException as j) -> assertNotOperationCode j.Code
    | _ -> ()
    throwsCode code action

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1700")>]
let ``JYRO-REQ-1700 The preconditions of the walk cases hold in this host`` () =
    let props (t: Type) = t.GetProperties() |> Array.map (fun p -> p.Name)
    Assert.Equal<string[]>([| "A"; "B" |], props typeof<Pair>)
    Assert.Equal<string[]>([| "A"; "B" |], props typeof<RecordPair<float>>)
    let keys (name: string) =
        let (_, d, _) = walkCase name
        match d with
        | :? Dictionary<string, obj> as o -> o.Keys |> List.ofSeq
        | :? Dictionary<string, float> as f -> f.Keys |> List.ofSeq
        | _ -> []
    Assert.Equal<string list>([ "a"; "b" ], keys "{a: [1, [NaN]], b: +Inf}")
    Assert.Equal<string list>([ "b"; "a" ], keys "{b: +Inf, a: [1, [NaN]]}")
    Assert.Equal<string list>([ "a"; "b" ], keys "Dictionary<string, double> {a: NaN, b: +Inf}")
    Assert.Equal<string list>([ "a"; "b" ], keys "Dictionary<string, double> {a: -Inf, b: NaN}")
    // JYRO-ADR-156 (d): the members the walk reads, in order.
    Assert.Equal<string list>([ "P"; "F" ], memberKeys typeof<PropAndField<float>>)
    Assert.Equal<string list>([ "X" ], memberKeys typeof<FieldStruct<float>>)
    Assert.Contains("X", memberKeys typeof<Vector2>)
    let (_, d, _) = walkCase "Dictionary<int, double> {1: NaN, 2: +Inf}"
    Assert.Equal<int list>([ 1; 2 ], (d :?> Dictionary<int, float>).Keys |> List.ofSeq)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1700")>]
let ``JYRO-REQ-1700 the clause's examples, from FromObject and from WithInput`` () =
    throwsHostCode (hostNumberInfinite ()) (fun () -> JyroValue.FromObject(box [| box Double.PositiveInfinity; box Double.NaN |]))
    let (_, d, _) = walkCase "{a: [1, [NaN]], b: +Inf}"
    throwsHostCode (hostNumberNaN ()) (fun () -> JyroValue.FromObject(d))
    let program = Script.valueOf (Script.compile "fail \"ran\"")
    assertRefusedBeforeRunning (hostNumberNaN ()) (JyroRunner(program).WithInput(d).Execute())

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1700")>]
[<MemberData(nameof walkCaseNames)>]
let ``JYRO-REQ-1700 FromObject raises the code of the first non-finite number in the depth-first walk`` (name: string) =
    let (_, value, code) = walkCase name
    throwsHostCode (codeOf code) (fun () -> JyroValue.FromObject(value))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1700")>]
[<MemberData(nameof walkInputData)>]
let ``JYRO-REQ-1700 a run given the host value reports the code of the first non-finite number in the depth-first walk`` (path: string, name: string) =
    let (_, value, code) = walkCase name
    let run = hostInputPaths |> List.find (fun (n, _) -> n = path) |> snd
    let r = run value
    for m in r.Messages do assertNotOperationCode m.Code
    assertRefusedBeforeRunning (codeOf code) r

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1700")>]
let ``JYRO-REQ-1700 FromObject follows the same walk for a sequence, a dictionary and a plain object`` () =
    let inf, nan = hostNumberInfinite (), hostNumberNaN ()
    throwsHostCode inf (fun () -> JyroValue.FromObject(List<obj>([ box Double.PositiveInfinity; box Double.NaN ])))
    throwsHostCode nan (fun () -> JyroValue.FromObject(List<obj>([ box [| box Double.NaN |]; box Double.PositiveInfinity ])))
    throwsHostCode inf (fun () -> JyroValue.FromObject(List<obj>([ box (List<float>([ Double.NegativeInfinity; Double.NaN ])) ])))
    throwsHostCode nan (fun () -> JyroValue.FromObject(List<obj>([ box (RecordPair<float>(Double.NaN, Double.PositiveInfinity)) ])))
    let (_, a, _) = walkCase "{a: [1, [NaN]], b: +Inf}"
    throwsHostCode nan (fun () -> JyroValue.FromObject(a :?> IDictionary<string, obj>))
    let (_, b, _) = walkCase "{b: +Inf, a: [1, [NaN]]}"
    throwsHostCode inf (fun () -> JyroValue.FromObject(b :?> IDictionary<string, obj>))
    let (_, t, _) = walkCase "Dictionary<string, double> {a: -Inf, b: NaN}"
    throwsHostCode inf (fun () -> JyroValue.FromObject(dictOf [ "x", t ]))
    throwsHostCode nan (fun () -> JyroValue.FromObject(Pair(box Double.NaN, box Double.PositiveInfinity)))
    throwsHostCode inf (fun () -> JyroValue.FromObject(Pair(box [| box Double.NegativeInfinity |], box Double.NaN)))
    throwsHostCode inf (fun () -> JyroValue.FromObject(RecordPair<float>(Double.PositiveInfinity, Double.NaN)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1700")>]
let ``JYRO-REQ-1700 MessageCode HostNumberNaN is 5921 and HostNumberInfinite is 5922`` () =
    Assert.Equal(5921, int (hostNumberNaN ()))
    Assert.Equal(5922, int (hostNumberInfinite ()))

// ---------------------------------------------------------------- 30.2.18: Input names the source

/// The one diagnostic of `code` of a failed run. This only selects it: that the
/// run fails before its first statement with exactly one diagnostic, of the code
/// 30.2.19 decides, is JYRO-REQ-1948's and JYRO-REQ-1700's.
let private diagnosticOf (code: MessageCode) (r: JyroResult<JyroValue>) : DiagnosticMessage =
    assertFails r
    match r.Messages |> Seq.filter (fun m -> m.Code = code) |> List.ofSeq with
    | [ d ] -> d
    | ds -> Values.fail (sprintf "Expected one %A diagnostic, got %d. Diagnostics:\n%s" code ds.Length (Script.describe r))

/// A host value's diagnostic has neither code of an operation's result.
let private assertNoOperationDiagnostic (r: JyroResult<JyroValue>) =
    for m in r.Messages do
        if m.Code = MessageCode.NumberDomainError || m.Code = MessageCode.NumberOverflow then
            Values.fail (sprintf "A host value that is not finite gave a %A diagnostic. Diagnostics:\n%s" m.Code (Script.describe r))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2118")>]
let ``JYRO-REQ-2118 the clause's JSON example names Input and then 1e999`` () =
    let program = Script.valueOf (Script.compile "fail \"ran\"")
    let r = (withInputJson (JyroRunner(program)) "{\"a\": 1e999}").Execute()
    assertArgs [ "Input"; "1e999" ] (diagnosticOf MessageCode.NumberOverflow r)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2118")>]
let ``JYRO-REQ-2118 the clause's host example has HostNumberNaN and no argument`` () =
    let program = Script.valueOf (Script.compile "fail \"ran\"")
    let r = JyroRunner(program).WithInput(box [| box Double.NaN |]).Execute()
    assertNoOperationDiagnostic r
    assertArgs [] (diagnosticOf (hostNumberNaN ()) r)

/// JSON texts with one number beyond the double range, and that number as the text writes it.
let private overflowTexts =
    [ "1E+999", "1E+999"
      "-1e999", "-1e999"
      "[2.5e400]", "2.5e400"
      "{\"o\": [1, {\"n\": -1.8e308}]}", "-1.8e308"
      "  { \"a\" : 1E400 }  ", "1E400"
      "[0.5, 1.0e309, 2]", "1.0e309" ]

let jsonArgData () : obj[] seq =
    seq {
        for (path, _) in jsonInputPaths do
            for (text, number) in overflowTexts do
                yield [| box path; box text; box number |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2118")>]
[<MemberData(nameof jsonArgData)>]
let ``JYRO-REQ-2118 JSON Input beyond the double range names Input and then the number as the text writes it`` (path: string, text: string, number: string) =
    let run = jsonInputPaths |> List.find (fun (n, _) -> n = path) |> snd
    assertArgs [ "Input"; number ] (diagnosticOf MessageCode.NumberOverflow (run text))

/// Host values with one number that is not finite, and the code 30.2.19 decides.
let private hostArgCases () : (string * obj * string) list =
    let deep = Dictionary<string, obj>()
    deep.["a"] <- box [| box 1; box (dictOf [ "b", box Double.NaN ]) |]
    [ "+Inf", box Double.PositiveInfinity, "HostNumberInfinite"
      "-Inf", box Double.NegativeInfinity, "HostNumberInfinite"
      "NaN", box Double.NaN, "HostNumberNaN"
      "[NaN]", box [| box Double.NaN |], "HostNumberNaN"
      "{a: [1, {b: NaN}]}", box deep, "HostNumberNaN"
      "POCO Value=float +Inf", box (Holder(box Single.PositiveInfinity)), "HostNumberInfinite"
      "Dictionary<string, double> {b: NaN}", (shape "Dictionary<string, T>") (box Double.NaN), "HostNumberNaN"
      "record-like X=float -Inf", box (RecordLike<float32>(Single.NegativeInfinity)), "HostNumberInfinite"
      "JsonValue of [NaN]", jsonValueOf [| Double.NaN |], "HostNumberNaN"
      "JValue +Inf", box (JValue(Double.PositiveInfinity)), "HostNumberInfinite"
      "ThirdPartyFloat NaN", box (ThirdPartyFloat(Double.NaN)), "HostNumberNaN" ]

let hostArgData () : obj[] seq =
    seq {
        for (path, _) in hostInputPaths do
            for (name, _, _) in hostArgCases () do
                yield [| box path; box name |]
    }

[<Theory>]
[<Trait("requirement", "JYRO-REQ-2118")>]
[<MemberData(nameof hostArgData)>]
let ``JYRO-REQ-2118 a host Input that is not finite has the host-number code and no argument`` (path: string, name: string) =
    let (_, value, code) = hostArgCases () |> List.find (fun (n, _, _) -> n = name)
    let run = hostInputPaths |> List.find (fun (n, _) -> n = path) |> snd
    let r = run value
    assertNoOperationDiagnostic r
    assertArgs [] (diagnosticOf (codeOf code) r)

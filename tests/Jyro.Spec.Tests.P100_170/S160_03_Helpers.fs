/// Part 160.3: the helpers a host function uses to read its arguments.
module RealTree.Jyro.Spec.Tests.S160_03_Helpers

open System
open System.IO
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Support

/// A host function `Probe(v)` that reads its argument with the helper `mode`
/// names and returns what the helper gave, as a Jyro value.
type Probe(mode: string) =
    inherit JyroFunctionBase("Probe", Host.signature "Probe" [ Parameter.Optional("v", ParameterType.Any) ] ParameterType.Any)
    override _.Effect = FunctionEffect.Pure
    override _.Execute(args, _ctx) =
        match mode with
        | "string" -> JyroString(args.GetString(0)) :> JyroValue
        | "array" -> args.GetArray(0) :> JyroValue
        | "object" -> args.GetObject(0) :> JyroValue
        | "number" -> JyroNumber(args.GetNumber(0)) :> JyroValue
        | "decimal" -> JyroNumber(args.GetDecimal(0)) :> JyroValue
        | "boolean" -> JyroBoolean.FromBoolean(args.GetBoolean(0)) :> JyroValue
        | "integer" -> JyroNumber(args.GetInt32(0)) :> JyroValue
        | "int64" -> JyroString(string (args.GetInt64(0))) :> JyroValue
        | "nonnegative" -> JyroNumber(args.GetNonNegativeInt32(0)) :> JyroValue
        | "get-string" -> args.Get<JyroString>(0) :> JyroValue
        | "get-array" -> args.Get<JyroArray>(0) :> JyroValue
        | "get-object" -> args.Get<JyroObject>(0) :> JyroValue
        | "get-number" -> args.Get<JyroNumber>(0) :> JyroValue
        | "get-boolean" -> args.Get<JyroBoolean>(0) :> JyroValue
        | "opt-string" -> args.GetOptional<JyroString>(0, JyroString("d")) :> JyroValue
        | "opt-number" -> args.GetOptional<JyroNumber>(0, JyroNumber(5)) :> JyroValue
        | "opt-bool" -> args.GetOptional<JyroBoolean>(0, JyroBoolean.True) :> JyroValue
        | "opt-array" -> args.GetOptional<JyroArray>(0, JyroArray()) :> JyroValue
        | other -> failwithf "unknown mode %s" other

/// Run `Output.r = Probe(<arg>)` with the helper `mode`; `Input.v` may carry a host-made value.
let private probe (mode: string) (arg: string) (input: JyroValue option) : JyroResult<JyroValue> =
    let o = { pinned with Functions = [ Probe(mode) :> IJyroFunction ] }
    let o = match input with Some v -> { o with Input = ValueInput(Values.obj [ "v", v ]) } | None -> o
    Script.runWith o (sprintf "Output.r = Probe(%s)" arg)

let private probeValue mode arg = probe mode arg None |> ok |> Values.prop "r"

let private argsOf (r: JyroResult<JyroValue>) =
    (Script.onlyErrorMessage r).Args |> Array.map (fun a -> (string a).ToLowerInvariant()) |> List.ofArray

// -- 160.3.1 Empty values for null ------------------------------------------------------------------

[<Theory>]
[<InlineData("string", "\"\"")>]
[<InlineData("array", "[]")>]
[<InlineData("object", "{}")>]
[<Trait("requirement", "JYRO-REQ-2096")>]
let ``StringArrayOrObjectHelper_GivesTheEmptyValueForNull`` (mode: string, expected: string) =
    Assert.Equal(expected, Values.toJson (probeValue mode "null"))

// -- 160.3.2 Number and boolean helpers raise for null ------------------------------------------------

[<Theory>]
[<InlineData("number")>]
[<InlineData("decimal")>]
[<InlineData("boolean")>]
[<InlineData("integer")>]
[<InlineData("int64")>]
[<InlineData("nonnegative")>]
[<Trait("requirement", "JYRO-REQ-2097")>]
let ``NumberOrBooleanHelper_RaisesArgumentTypeMismatchForNull`` (mode: string) =
    probe mode "null" None |> fails MessageCode.ArgumentTypeMismatch

// -- 160.3.3 Wrong types ------------------------------------------------------------------------------

let private typeArgs =
    [ "number", "1"; "string", "\"s\""; "boolean", "true"; "array", "[1]"; "object", "{a: 1}"; "function", "(x => x)" ]

/// Each helper, the Jyro type it expects, and the types it does not accept.
let private helperTypes =
    [ "string", "string", [ "array"; "object"; "function" ]
      "array", "array", [ "number"; "string"; "boolean"; "object"; "function" ]
      "object", "object", [ "number"; "string"; "boolean"; "array"; "function" ]
      "number", "number", [ "string"; "boolean"; "array"; "object"; "function" ]
      "decimal", "number", [ "string"; "boolean"; "array"; "object"; "function" ]
      "boolean", "boolean", [ "number"; "string"; "array"; "object"; "function" ]
      "integer", "number", [ "string"; "boolean"; "array"; "object"; "function" ]
      "int64", "number", [ "string"; "boolean"; "array"; "object"; "function" ]
      "nonnegative", "number", [ "string"; "boolean"; "array"; "object"; "function" ]
      "get-array", "array", [ "number"; "string"; "boolean"; "object"; "function" ]
      "get-object", "object", [ "number"; "string"; "boolean"; "array"; "function" ]
      "get-number", "number", [ "string"; "boolean"; "array"; "object"; "function" ]
      "get-boolean", "boolean", [ "number"; "string"; "array"; "object"; "function" ] ]

let wrongTypeRows : obj[] seq =
    [ for mode, expected, wrongs in helperTypes do
        for w in wrongs -> [| box mode; box expected; box w |] ]

[<Theory>]
[<MemberData(nameof wrongTypeRows)>]
[<Trait("requirement", "JYRO-REQ-2098")>]
let ``Helper_GivenATypeItDoesNotAccept_RaisesArgumentTypeMismatch_NamingBothTypes`` (mode: string, expected: string, actual: string) =
    let arg = typeArgs |> List.find (fst >> (=) actual) |> snd
    let r = probe mode arg None
    fails MessageCode.ArgumentTypeMismatch r
    stageOfCode MessageCode.ArgumentTypeMismatch "Execute" r
    // The expected type, then the actual type, as TypeOf spells them; no class name (130.6.5).
    assertTypeNameArgs expected actual (Script.onlyErrorMessage r)

[<Theory>]
[<InlineData("number", "number")>]
[<InlineData("decimal", "number")>]
[<InlineData("boolean", "boolean")>]
[<Trait("requirement", "JYRO-REQ-2098")>]
let ``NumberOrBooleanHelper_ForNull_NamesTheTypeThenNull`` (mode: string, expected: string) =
    // 160.3.2 does not accept null for these helpers, so 160.3.3 names both types (130.6.5).
    let r = probe mode "null" None
    fails MessageCode.ArgumentTypeMismatch r
    stageOfCode MessageCode.ArgumentTypeMismatch "Execute" r
    assertTypeNameArgs expected "null" (Script.onlyErrorMessage r)

// -- 160.3.4 No default for a wrong type; no text from a container --------------------------------------

[<Theory>]
[<InlineData("opt-number", "\"s\"")>]
[<InlineData("opt-number", "[1]")>]
[<InlineData("opt-bool", "5")>]
[<InlineData("opt-string", "[1, 2]")>]
[<InlineData("opt-string", "{a: 1}")>]
[<InlineData("opt-array", "\"s\"")>]
[<Trait("requirement", "JYRO-REQ-1594")>]
let ``OptionalHelper_GivenAWrongType_RaisesInsteadOfGivingItsDefault`` (mode: string, arg: string) =
    probe mode arg None |> fails MessageCode.ArgumentTypeMismatch

[<Theory>]
[<InlineData("string", "[1, 2]")>]
[<InlineData("string", "{a: 1}")>]
[<InlineData("string", "(x => x)")>]
[<InlineData("opt-string", "[1, 2]")>]
[<Trait("requirement", "JYRO-REQ-1594")>]
let ``StringHelper_DoesNotConvertAContainerToText`` (mode: string, arg: string) =
    probe mode arg None |> fails MessageCode.ArgumentTypeMismatch

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1594")>]
let ``OptionalHelper_WithTheArgumentAbsent_GivesItsDefault`` () =
    let o = { Script.defaults with Functions = [ Probe("opt-number") :> IJyroFunction ] }
    Assert.Equal(5.0, Script.runWith o "Output.r = Probe()" |> ok |> Values.prop "r" |> Values.asFloat)

// -- 160.3.5 A lossless number read ------------------------------------------------------------------------

[<Theory>]
[<InlineData("9007199254740993")>]
[<InlineData("0.1")>]
[<InlineData("79228162514264337593543950335")>]
[<InlineData("0.0000000000000000000000000001")>]
[<InlineData("12345678901234567890.123456789")>]
[<Trait("requirement", "JYRO-REQ-2099")>]
let ``DecimalHelper_ReadsANumberWithNoLoss`` (literal: string) =
    let v = probeValue "decimal" literal
    Assert.Equal(Decimal.Parse(literal, Globalization.CultureInfo.InvariantCulture), Values.asDecimal v)
    let neg = probeValue "decimal" ("-" + literal)
    Assert.Equal(-Decimal.Parse(literal, Globalization.CultureInfo.InvariantCulture), Values.asDecimal neg)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-2099")>]
let ``Documentation_SaysWhichNumberHelpersLosePrecision`` () =
    let xml = Path.ChangeExtension(typeof<JyroFunctionBase>.Assembly.Location, ".xml")
    Assert.True(File.Exists xml, "No XML documentation beside Jyro.Core")
    let doc = Xml.Linq.XDocument.Load xml
    let docOf (memberPrefix: string) =
        doc.Descendants(Xml.Linq.XName.Get "member")
        |> Seq.filter (fun m -> (m.Attribute(Xml.Linq.XName.Get "name").Value).StartsWith memberPrefix)
        |> Seq.map (fun m -> m.Value.ToLowerInvariant())
        |> String.concat " "
    let text = docOf "M:RealTree.Jyro.JyroArguments.GetNumber"
    Assert.True(
        [ "precision"; "lossy"; "lose"; "loss"; "double" ] |> List.exists text.Contains,
        sprintf "JyroArguments.GetNumber's documentation does not say it loses precision: %s" text)

// -- 160.3.6 / 160.3.8 Integer helpers ---------------------------------------------------------------------

let integerCases : obj[] seq =
    [ // helper, argument, expected code ("" = accepted with this value)
      "integer", "\"5\"", "ArgumentTypeMismatch", ""
      "integer", "true", "ArgumentTypeMismatch", ""
      "integer", "[]", "ArgumentTypeMismatch", ""
      "integer", "{}", "ArgumentTypeMismatch", ""
      "integer", "1.5", "IntegerRequired", ""
      "integer", "0.5", "IntegerRequired", ""
      "integer", "-1.5", "IntegerRequired", ""
      "integer", "2147483647", "", "2147483647"
      "integer", "-2147483648", "", "-2147483648"
      "integer", "2147483648", "IntegerOutOfRange", ""
      "integer", "-2147483649", "IntegerOutOfRange", ""
      "integer", "1e20", "IntegerOutOfRange", ""
      "integer", "-3000000000", "IntegerOutOfRange", ""
      "integer", "3.0", "", "3"
      "nonnegative", "\"5\"", "ArgumentTypeMismatch", ""
      "nonnegative", "1.5", "IntegerRequired", ""
      "nonnegative", "-1.5", "IntegerRequired", ""
      "nonnegative", "2147483647", "", "2147483647"
      "nonnegative", "0", "", "0"
      "nonnegative", "-1", "NonNegativeIntegerRequired", ""
      "nonnegative", "-2147483648", "NonNegativeIntegerRequired", ""
      "nonnegative", "-3000000000", "IntegerOutOfRange", ""
      "nonnegative", "2147483648", "IntegerOutOfRange", ""
      "int64", "\"5\"", "ArgumentTypeMismatch", ""
      "int64", "true", "ArgumentTypeMismatch", ""
      "int64", "1.5", "IntegerRequired", ""
      "int64", "-1.5", "IntegerRequired", ""
      "int64", "1099511627776", "", "1099511627776"
      "int64", "9223372036854775807", "", "9223372036854775807"
      "int64", "-9223372036854775808", "", "-9223372036854775808"
      "int64", "9223372036854775808", "IntegerOutOfRange", ""
      "int64", "-9223372036854775809", "IntegerOutOfRange", ""
      "int64", "9007199254740993", "", "9007199254740993"
      "int64", "1e20", "IntegerOutOfRange", "" ]
    |> Seq.map (fun (h, a, c, v) -> [| box h; box a; box c; box v |])

[<Theory>]
[<MemberData(nameof integerCases)>]
[<Trait("requirement", "JYRO-REQ-1596")>]
let ``IntegerHelper_AppliesTheChecksInOrder_WithItsRange`` (helper: string, arg: string, code: string, value: string) =
    let r = probe helper arg None
    if code = "" then
        let v = ok r |> Values.prop "r"
        let text = if helper = "int64" then Values.asString v else string (Values.asDecimal v)
        Assert.Equal(value, text)
    else
        fails (Enum.Parse<MessageCode> code) r

[<Theory>]
[<InlineData("9223372036854775807", "9223372036854775807")>]
[<InlineData("-9223372036854775808", "-9223372036854775808")>]
[<InlineData("1099511627776", "1099511627776")>]
[<InlineData("9007199254740993", "9007199254740993")>]
[<InlineData("0", "0")>]
[<Trait("requirement", "JYRO-REQ-1598")>]
let ``Int64Helper_ReturnsEveryIntegerInItsRangeWithNoLoss`` (arg: string, expected: string) =
    Assert.Equal(expected, probeValue "int64" arg |> Values.asString)

[<Theory>]
[<InlineData("9223372036854775808")>]
[<InlineData("-9223372036854775809")>]
[<InlineData("1e20")>]
[<InlineData("-1e20")>]
[<Trait("requirement", "JYRO-REQ-1598")>]
let ``Int64Helper_OutsideItsRange_RaisesIntegerOutOfRange`` (arg: string) =
    probe "int64" arg None |> fails MessageCode.IntegerOutOfRange

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1598")>]
let ``Int64Helper_ReadsInexactIntegersExactly`` () =
    let read (d: float) = probe "int64" "Input.v" (Some(Values.inexact d))
    Assert.Equal("4611686018427387904", read 4611686018427387904.0 |> ok |> Values.prop "r" |> Values.asString)
    Assert.Equal("9223372036854774784", read 9223372036854774784.0 |> ok |> Values.prop "r" |> Values.asString)
    read 9223372036854775808.0 |> fails MessageCode.IntegerOutOfRange
    Assert.Equal("3", read 3.0 |> ok |> Values.prop "r" |> Values.asString)

// -- 160.3.7 A string helper reads a number or a boolean as its string form ------------------------------

// The string forms that only JYRO-REQ-150 (an exact number with trailing zeros, 1.50) and
// JYRO-REQ-151 (an inexact number whose Number::toString has an exponent, 1e+21) make hold are read through a helper in
// those requirements' tests (S40_04_Concatenation), JYRO-ADR-113 item 2: 160.3.7 gives the
// string form of 40.4, and these instances of it are not this work order's.
[<Theory>]
[<InlineData("string", "0", "0")>]
[<InlineData("string", "-2", "-2")>]
[<InlineData("string", "0.1", "0.1")>]
[<InlineData("string", "true", "true")>]
[<InlineData("string", "false", "false")>]
[<InlineData("string", "1000000000000000000000", "1000000000000000000000")>]
[<Trait("requirement", "JYRO-REQ-1597")>]
let ``StringHelper_ReadsANumberOrABooleanAsItsStringForm`` (mode: string, arg: string, expected: string) =
    Assert.Equal(expected, probeValue mode arg |> Values.asString)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1597")>]
let ``StringHelper_ReadsAnInexactNumberAsItsStringForm`` () =
    let read (d: float) = probe "string" "Input.v" (Some(Values.inexact d)) |> ok |> Values.prop "r" |> Values.asString
    Assert.Equal("0.1", read 0.1)
    Assert.Equal("123.456", read 123.456)
    Assert.Equal("0.5", read 0.5)
    Assert.Equal("100", read 100.0)


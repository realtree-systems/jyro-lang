/// jyro-spec 96.2 ValidateSchema, requirements 96.2.22 to 96.2.42.
module RealTree.Jyro.Spec.Tests.S96_02b

open System
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

/// The full elements (path, keyword, message) of ValidateSchema(data, schema).
let private elements (dataJson: string) (schemaJson: string) : SchemaError list =
    Script.output (validateRaw dataJson schemaJson) |> Values.prop "r" |> schemaErrors

/// "path:keyword,path:keyword" as a list of pairs; "" is the empty list.
let private pairs (expected: string) =
    if expected = "" then []
    else expected.Split(',') |> Array.map (fun p -> let i = p.LastIndexOf(':') in p.Substring(0, i), p.Substring(i + 1)) |> List.ofArray

/// Failing cases used by 96.2.22 and 96.2.26: data, schema.
let failingCases : obj[] seq =
    [ "1", """{"type":"string"}"""
      """{"o":{}}""", """{"required":["a"],"properties":{"o":{"required":["x","y"]}}}"""
      """{"a":"x"}""", """{"properties":{"a":{"minLength":2}}}"""
      "true", """{"anyOf":[{"type":"string"},{"type":"number"}]}"""
      """[1,"q"]""", """{"items":{"enum":[1,2]}}"""
      """{"a/b":1,"m~n":2}""", """{"additionalProperties":{"type":"string"}}"""
      """{"l":[1,1]}""", """{"properties":{"l":{"uniqueItems":true,"minItems":3}}}"""
      "[]", """{"minItems":1,"contains":{}}"""
      """{"":{"x":1}}""", """{"properties":{"":{"additionalProperties":false}}}"""
      """{"a":1}""", """{"dependentRequired":{"a":["b"]},"propertyNames":{"maxLength":0}}""" ]
    |> Seq.map (fun (d, s) -> [| box d; box s |])

// =========================================================================
// 96.2.22 element shape
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1247")>]
[<MemberData(nameof failingCases)>]
let ``ValidateSchema_EachElementHasExactlyPathKeywordMessage`` (data: string, schema: string) =
    let res = validateRaw data schema
    let arr = Script.output res |> Values.prop "r" |> Values.asList
    Assert.NotEmpty(arr)
    for e in arr do
        Assert.Equal(JyroValueType.Object, e.ValueType)
        let keys = (e :?> JyroObject) |> Seq.map (fun kv -> kv.Key) |> List.ofSeq
        Assert.Equal<string list>([ "path"; "keyword"; "message" ], keys)
        for k in keys do Assert.Equal(JyroValueType.String, (Values.prop k e).ValueType)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1247")>]
let ``ValidateSchema_ElementKeysReadInScriptAreInOrder`` () =
    Assert.Equal(norm """["path","keyword","message"]""", rJson "Output.r = Keys(ValidateSchema(1, {type: 'string'})[0])")

// =========================================================================
// 96.2.23 path
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1248")>]
[<InlineData("1", """{"type":"string"}""", "")>]
[<InlineData("""{"a":1}""", """{"properties":{"a":{"type":"string"}}}""", "/a")>]
[<InlineData("""["x",1]""", """{"items":{"type":"string"}}""", "/1")>]
[<InlineData("""{"a":{"b":[0,{"c":1}]}}""", """{"properties":{"a":{"properties":{"b":{"items":{"properties":{"c":{"type":"string"}}}}}}}}""", "/a/b/1/c")>]
[<InlineData("""{"a/b":1}""", """{"additionalProperties":{"type":"string"}}""", "/a~1b")>]
[<InlineData("""{"m~n":1}""", """{"additionalProperties":{"type":"string"}}""", "/m~0n")>]
[<InlineData("""{"~1":1}""", """{"additionalProperties":{"type":"string"}}""", "/~01")>]
[<InlineData("""{"/~":1}""", """{"additionalProperties":{"type":"string"}}""", "/~1~0")>]
[<InlineData("""{"":1}""", """{"additionalProperties":{"type":"string"}}""", "/")>]
[<InlineData("""{"":{"":1}}""", """{"additionalProperties":{"additionalProperties":{"type":"string"}}}""", "//")>]
[<InlineData("""{"a%b":1}""", """{"additionalProperties":{"type":"string"}}""", "/a%b")>]
[<InlineData("""{"a b":1}""", """{"additionalProperties":{"type":"string"}}""", "/a b")>]
[<InlineData("""{"0":1}""", """{"additionalProperties":{"type":"string"}}""", "/0")>]
[<InlineData("""{"é中":1}""", """{"additionalProperties":{"type":"string"}}""", "/é中")>]
[<InlineData("""[[[1,2,3,4,5,6,7,8,9,10,"x"]]]""", """{"items":{"items":{"items":{"type":"number"}}}}""", "/0/0/10")>]
let ``JYRO-REQ-1248 ValidateSchema_PathIsTheJsonPointerOfTheFailingValue`` (data: string, schema: string, path: string) =
    Assert.Equal<(string * string) list>([ path, "type" ], validate data schema)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1248")>]
[<InlineData("[1]", """{"minItems":2}""", "", "minItems")>]
[<InlineData("[1,2]", """{"maxItems":1}""", "", "maxItems")>]
[<InlineData("[1,1]", """{"uniqueItems":true}""", "", "uniqueItems")>]
[<InlineData("[1,2,3,2]", """{"uniqueItems":true}""", "", "uniqueItems")>]
[<InlineData("[1]", """{"contains":{"type":"string"}}""", "", "contains")>]
[<InlineData("{}", """{"minProperties":1}""", "", "minProperties")>]
[<InlineData("""{"a":1,"b":2}""", """{"maxProperties":1}""", "", "maxProperties")>]
[<InlineData("""{"list":[1,1]}""", """{"properties":{"list":{"uniqueItems":true}}}""", "/list", "uniqueItems")>]
[<InlineData("""{"list":[1]}""", """{"properties":{"list":{"minItems":2}}}""", "/list", "minItems")>]
[<InlineData("""{"list":[1,2]}""", """{"properties":{"list":{"maxItems":1}}}""", "/list", "maxItems")>]
[<InlineData("""{"list":[1]}""", """{"properties":{"list":{"contains":{"type":"string"}}}}""", "/list", "contains")>]
[<InlineData("""{"o":{}}""", """{"properties":{"o":{"minProperties":1}}}""", "/o", "minProperties")>]
[<InlineData("""{"o":{"a":1,"b":2}}""", """{"properties":{"o":{"maxProperties":1}}}""", "/o", "maxProperties")>]
[<InlineData("""[[1,1]]""", """{"items":{"uniqueItems":true}}""", "/0", "uniqueItems")>]
let ``JYRO-REQ-1248 ValidateSchema_WholeContainerKeywordsReportTheContainersPath`` (data: string, schema: string, path: string, keyword: string) =
    Assert.Equal<(string * string) list>([ path, keyword ], validate data schema)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1248")>]
[<InlineData("""{"a":{"b":1}}""", """{"properties":{"a":{"enum":[1]}}}""", "/a", "enum")>]
[<InlineData("""{"a":[1]}""", """{"properties":{"a":{"const":[2]}}}""", "/a", "const")>]
[<InlineData("""{"a":{}}""", """{"properties":{"a":{"type":"array"}}}""", "/a", "type")>]
let ``JYRO-REQ-1248 ValidateSchema_AKeywordAboutAContainerValueReportsTheContainer`` (data: string, schema: string, path: string, keyword: string) =
    Assert.Equal<(string * string) list>([ path, keyword ], validate data schema)

// =========================================================================
// 96.2.24 required path
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1249")>]
[<InlineData("{}", """{"required":["a"]}""", "/a")>]
[<InlineData("""{"o":{}}""", """{"properties":{"o":{"required":["x"]}}}""", "/o/x")>]
[<InlineData("{}", """{"required":["a/b"]}""", "/a~1b")>]
[<InlineData("{}", """{"required":["m~n"]}""", "/m~0n")>]
[<InlineData("{}", """{"required":[""]}""", "/")>]
[<InlineData("""[{}]""", """{"items":{"required":["k"]}}""", "/0/k")>]
[<InlineData("""{"A":1}""", """{"required":["a"]}""", "/a")>]
let ``JYRO-REQ-1249 ValidateSchema_RequiredPathIsTheAbsentProperty`` (data: string, schema: string, path: string) =
    Assert.Equal<(string * string) list>([ path, "required" ], validate data schema)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1249")>]
let ``JYRO-REQ-1249 ValidateSchema_TwoAbsentNamesGiveTwoElements`` () =
    expectErrors [ "/a", "required"; "/b", "required" ] """{"c":1}""" """{"required":["b","a","c"]}"""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1249")>]
let ``JYRO-REQ-1249 ValidateSchema_RequiredMessageNamesTheAbsentProperty`` () =
    Assert.Equal<string list>([ "required at /o/x" ], elements """{"o":{}}""" """{"properties":{"o":{"required":["x"]}}}""" |> List.map (fun e -> e.Message))

// =========================================================================
// 96.2.25 keyword
// =========================================================================

let assertionKeywords : obj[] seq =
    [ "1", """{"type":"string"}""", "type"
      "3", """{"enum":[1,2]}""", "enum"
      "3", """{"const":1}""", "const"
      "3", """{"multipleOf":2}""", "multipleOf"
      "3", """{"maximum":2}""", "maximum"
      "2", """{"exclusiveMaximum":2}""", "exclusiveMaximum"
      "1", """{"minimum":2}""", "minimum"
      "2", """{"exclusiveMinimum":2}""", "exclusiveMinimum"
      "\"abc\"", """{"maxLength":2}""", "maxLength"
      "\"a\"", """{"minLength":2}""", "minLength"
      "\"a\"", """{"pattern":"^b"}""", "pattern"
      "[1,2]", """{"maxItems":1}""", "maxItems"
      "[]", """{"minItems":1}""", "minItems"
      "[1,1]", """{"uniqueItems":true}""", "uniqueItems"
      """{"a":1,"b":1}""", """{"maxProperties":1}""", "maxProperties"
      "{}", """{"minProperties":1}""", "minProperties"
      "{}", """{"required":["a"]}""", "required"
      """{"a":1}""", """{"dependentRequired":{"a":["b"]}}""", "dependentRequired" ]
    |> Seq.map (fun (d, s, k) -> [| box d; box s; box k |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1250")>]
[<MemberData(nameof assertionKeywords)>]
let ``JYRO-REQ-1250 ValidateSchema_KeywordIsTheNameOfTheFailedKeyword`` (data: string, schema: string, keyword: string) =
    let errs = validate data schema
    Assert.Equal<string list>([ keyword ], errs |> List.map snd)

let innerKeywords : obj[] seq =
    [ """{"a":"x"}""", """{"properties":{"a":{"minLength":2}}}""", "minLength"
      "1", """{"allOf":[{"maximum":0}]}""", "maximum"
      "1", """{"$defs":{"s":{"type":"string"}},"$ref":"#/$defs/s"}""", "type"
      "[1]", """{"items":{"type":"string"}}""", "type"
      "[1]", """{"prefixItems":[{"minimum":5}]}""", "minimum"
      """{"x1":1}""", """{"patternProperties":{"^x":{"type":"string"}}}""", "type"
      """{"z":1}""", """{"additionalProperties":{"type":"string"}}""", "type"
      "\"a\"", """{"if":{"type":"string"},"then":{"minLength":2}}""", "minLength"
      "1", """{"if":{"type":"string"},"else":{"minimum":2}}""", "minimum"
      "true", """{"anyOf":[{"type":"string"},{"type":"number"}]}""", "anyOf"
      "true", """{"oneOf":[{"type":"string"}]}""", "oneOf"
      """{"abc":1}""", """{"propertyNames":{"maxLength":2}}""", "propertyNames"
      "\"s\"", """{"not":{"type":"string"}}""", "not"
      "[1]", """{"contains":{"type":"string"}}""", "contains"
      """{"a":1}""", """{"properties":{"a":false}}""", "properties"
      "1", """{"allOf":[{"allOf":[{"$ref":"#/$defs/s"}]}],"$defs":{"s":{"multipleOf":2}}}""", "multipleOf" ]
    |> Seq.map (fun (d, s, k) -> [| box d; box s; box k |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1250")>]
[<MemberData(nameof innerKeywords)>]
let ``JYRO-REQ-1250 ValidateSchema_KeywordInsideASubschemaIsTheOneTheRulesName`` (data: string, schema: string, keyword: string) =
    Assert.Equal<string list>([ keyword ], validate data schema |> List.map snd)

// =========================================================================
// 96.2.26 message
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1251")>]
[<MemberData(nameof failingCases)>]
let ``JYRO-REQ-1251 ValidateSchema_MessageIsKeywordAtPathOrRoot`` (data: string, schema: string) =
    let els = elements data schema
    Assert.NotEmpty(els)
    for e in els do
        let where = if e.Path = "" then "(root)" else e.Path
        Assert.Equal(e.Keyword + " at " + where, e.Message)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1251")>]
let ``JYRO-REQ-1251 ValidateSchema_MessageExamples`` () =
    Assert.Equal<string list>([ "type at (root)" ], elements "1" """{"type":"string"}""" |> List.map (fun e -> e.Message))
    Assert.Equal<string list>([ "required at /a" ], elements "{}" """{"required":["a"]}""" |> List.map (fun e -> e.Message))
    Assert.Equal<string list>([ "required at /" ], elements "{}" """{"required":[""]}""" |> List.map (fun e -> e.Message))
    Assert.Equal<string list>([ "type at /a~1b/0" ], elements """{"a/b":[1]}""" """{"additionalProperties":{"items":{"type":"string"}}}""" |> List.map (fun e -> e.Message))
    Assert.Equal<string list>([ "anyOf at (root)" ], elements "true" """{"anyOf":[{"type":"string"}]}""" |> List.map (fun e -> e.Message))

// =========================================================================
// 96.2.27 anyOf / oneOf
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1252")>]
[<InlineData("5", """{"anyOf":[{"type":"string"},{"type":"number","minimum":10}]}""", "", "anyOf")>]
[<InlineData("true", """{"anyOf":[{"type":"string"},{"type":"number","minimum":10}]}""", "", "anyOf")>]
[<InlineData("true", """{"oneOf":[{"type":"string"},{"type":"number","minimum":10}]}""", "", "oneOf")>]
[<InlineData("20", """{"oneOf":[{"type":"number"},{"minimum":10}]}""", "", "oneOf")>]
[<InlineData("""{"a":5}""", """{"properties":{"a":{"anyOf":[{"type":"string"},{"minimum":10}]}}}""", "/a", "anyOf")>]
[<InlineData("""{"x":""}""", """{"anyOf":[{"required":["y"]},{"properties":{"x":{"minLength":1}}}]}""", "", "anyOf")>]
[<InlineData("""{"x":""}""", """{"oneOf":[{"required":["y"]},{"properties":{"x":{"minLength":1}}}]}""", "", "oneOf")>]
[<InlineData("1", """{"anyOf":[false]}""", "", "anyOf")>]
[<InlineData("1", """{"oneOf":[true,true]}""", "", "oneOf")>]
[<InlineData("[1]", """{"items":{"oneOf":[{"type":"string"},{"type":"boolean"}]}}""", "/0", "oneOf")>]
let ``JYRO-REQ-1252 ValidateSchema_FailingAnyOfOrOneOf_ReportsOneErrorAndNoBranchErrors`` (data: string, schema: string, path: string, keyword: string) =
    expectErrors [ path, keyword ] data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1252")>]
let ``JYRO-REQ-1252 ValidateSchema_AnyOfBesideOtherKeywords_ReportsItsOneErrorAmongTheirs`` () =
    expectErrors [ "", "anyOf"; "", "maximum" ] "5" """{"maximum":1,"anyOf":[{"type":"string"},{"minimum":10}]}"""
    passes "5" """{"anyOf":[{"type":"string"},{"minimum":1}]}"""

// =========================================================================
// 96.2.28 order
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1253")>]
let ``JYRO-REQ-1253 ValidateSchema_ElementsAreOrderedByPathThenKeywordOrdinally`` () =
    let data = """{"z":1,"é":1,"a":["x",1,2,3,4,5,6,7,8,9,10,11],"B":1}"""
    let schema =
        """{"type":"array","properties":{"z":{"type":"string"},"é":{"type":"string"},"B":{"type":"string"},"""
        + """"a":{"maxItems":1,"items":{"type":"string"},"prefixItems":[{"maxLength":0,"pattern":"^y","enum":["q"]}]}}}"""
    let expected =
        [ "", "type"; "/B", "type"; "/a", "maxItems"; "/a/0", "enum"; "/a/0", "maxLength"; "/a/0", "pattern"
          "/a/1", "type"; "/a/10", "type"; "/a/11", "type"
          "/a/2", "type"; "/a/3", "type"; "/a/4", "type"; "/a/5", "type"; "/a/6", "type"; "/a/7", "type"; "/a/8", "type"; "/a/9", "type"
          "/z", "type"; "/é", "type" ]
    Assert.Equal<(string * string) list>(expected, validate data schema)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1253")>]
let ``JYRO-REQ-1253 ValidateSchema_OrdersByUtf16CodeUnitsNotCodePoints`` () =
    // U+1F600 is D83D DE00 in UTF-16, below U+FF21: ordinal order of code
    // units puts it first, where code point order would put it last.
    let data = sprintf """{"Ａ":1,%s:1,"":1}""" (Values.toJson (Values.str emoji))
    expectErrors [ "/" + emoji, "type"; "/", "type"; "/Ａ", "type" ] data """{"additionalProperties":{"type":"string"}}"""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1253")>]
let ``JYRO-REQ-1253 ValidateSchema_OrdersKeywordsOrdinallyAtOnePath`` () =
    // "$ref" (U+0024) before letters; minimum does not apply to a string
    expectErrors [ "", "$ref"; "", "maxLength"; "", "pattern"; "", "type" ]
        "\"abc\""
        """{"$defs":{"f":false},"$ref":"#/$defs/f","type":"number","pattern":"^z","maxLength":1,"minimum":9}"""
    expectErrors [ "", "maxProperties"; "/a", "minLength"; "/a", "type"; "/z", "required" ]
        """{"a":"x","b":1}"""
        """{"required":["z"],"maxProperties":1,"properties":{"a":{"minLength":2,"allOf":[{"type":"number"}]}}}"""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1253")>]
let ``JYRO-REQ-1253 ValidateSchema_APathThatIsAPrefixComesFirst`` () =
    expectErrors [ "/a", "required"; "/ab", "type"; "/b", "type" ]
        """{"ab":1,"b":1}"""
        """{"required":["a"],"additionalProperties":{"type":"string"}}"""

// =========================================================================
// 96.2.29 no duplicates
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1254")>]
[<InlineData("1", """{"allOf":[{"type":"string"},{"type":"string"}]}""", "", "type")>]
[<InlineData("1", """{"$defs":{"s":{"type":"string"}},"allOf":[{"$ref":"#/$defs/s"},{"$ref":"#/$defs/s"}]}""", "", "type")>]
[<InlineData("""{"ab":"x"}""", """{"properties":{"ab":{"minLength":2}},"patternProperties":{"^a":{"minLength":2}}}""", "/ab", "minLength")>]
[<InlineData("{}", """{"allOf":[{"required":["a"]},{"required":["a"]}]}""", "/a", "required")>]
[<InlineData("{}", """{"required":["a"],"allOf":[{"required":["a"]}]}""", "/a", "required")>]
[<InlineData("""{"a":1}""", """{"dependentRequired":{"a":["c"]},"allOf":[{"dependentRequired":{"a":["c"]}}]}""", "/c", "dependentRequired")>]
[<InlineData("1", """{"type":"string","allOf":[{"type":"boolean"}]}""", "", "type")>]
[<InlineData("""{"x":1}""", """{"patternProperties":{"x":{"type":"string"},"^x$":{"type":"boolean"}}}""", "/x", "type")>]
let ``JYRO-REQ-1254 ValidateSchema_HasNoTwoElementsWithTheSamePathAndKeyword`` (data: string, schema: string, path: string, keyword: string) =
    expectErrors [ path, keyword ] data schema

// =========================================================================
// 96.2.30 null schema, 96.2.31 non-object schema
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1255")>]
[<InlineData("ValidateSchema(1, null)")>]
[<InlineData("ValidateSchema({}, x)")>]
[<InlineData("ValidateSchema(null, null)")>]
[<InlineData("ValidateSchema(1, Input.missing)")>]
[<InlineData("ValidateSchema(1, Input.n)")>]
[<InlineData("ValidateSchema(schema: null, data: 1)")>]
let ``JYRO-REQ-1255 ValidateSchema_OfNullSchema_RaisesArgumentTypeMismatch`` (call: string) =
    failsIn """{"n":null}""" MessageCode.ArgumentTypeMismatch (sprintf "var x = null\nOutput.r = %s" call)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1256")>]
[<InlineData("true")>]
[<InlineData("false")>]
[<InlineData("[]")>]
[<InlineData("[{}]")>]
[<InlineData("'{}'")>]
[<InlineData("''")>]
[<InlineData("0")>]
[<InlineData("1")>]
[<InlineData("f")>]
let ``ValidateSchema_OfNonObjectSchema_RaisesArgumentTypeMismatch`` (schema: string) =
    fails MessageCode.ArgumentTypeMismatch (sprintf "var f = x => x\nOutput.r = ValidateSchema(1, %s)" schema)

// =========================================================================
// 96.2.32 null data
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1257")>]
[<InlineData("""{"type":"null"}""", false)>]
[<InlineData("""{"type":"object"}""", true)>]
[<InlineData("""{"type":["object","null"]}""", false)>]
[<InlineData("""{"required":["a"]}""", false)>]
[<InlineData("""{"enum":[null]}""", false)>]
[<InlineData("""{"const":null}""", false)>]
[<InlineData("""{"minLength":1}""", false)>]
[<InlineData("""{"minProperties":1}""", false)>]
[<InlineData("""{"minimum":1}""", false)>]
[<InlineData("""{"items":false}""", false)>]
[<InlineData("""{"type":"string"}""", true)>]
let ``JYRO-REQ-1257 ValidateSchema_ChecksNullDataAsJsonNull`` (schema: string, typeError: bool) =
    let got = Script.output (Script.runJson (sprintf "{\"s\": %s}" schema) "Output.r = ValidateSchema(null, Input.s)") |> Values.prop "r" |> schemaErrors |> List.map (fun e -> e.Path, e.Keyword)
    Assert.Equal<(string * string) list>((if typeError then [ "", "type" ] else []), got)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1257")>]
let ``JYRO-REQ-1257 ValidateSchema_NullDataFailsEnumAndConstThatLackNull`` () =
    let run s = Script.output (Script.runJson (sprintf "{\"s\": %s}" s) "var x = null\nOutput.r = ValidateSchema(x, Input.s)") |> Values.prop "r" |> schemaErrors |> List.map (fun e -> e.Path, e.Keyword)
    Assert.Equal<(string * string) list>([ "", "enum" ], run """{"enum":[0,"",false,[],{}]}""")
    Assert.Equal<(string * string) list>([ "", "const" ], run """{"const":0}""")
    Assert.Equal<(string * string) list>([ "", "not" ], run """{"not":{"type":"null"}}""")

// =========================================================================
// 96.2.34 cycles of schema steps
// =========================================================================

let cyclicSchemas : obj[] seq =
    [ """{"$ref":"#"}"""
      """{"allOf":[{"$ref":"#"}]}"""
      """{"anyOf":[{"$ref":"#"}]}"""
      """{"oneOf":[true,{"$ref":"#"}]}"""
      """{"not":{"$ref":"#"}}"""
      """{"if":{"$ref":"#"}}"""
      """{"if":true,"then":{"$ref":"#"}}"""
      """{"if":false,"else":{"$ref":"#"}}"""
      """{"$defs":{"a":{"$ref":"#/$defs/b"},"b":{"$ref":"#/$defs/a"}},"$ref":"#/$defs/a"}"""
      """{"$defs":{"a":{"$ref":"#/$defs/b"},"b":{"$ref":"#/$defs/a"}}}"""
      """{"$defs":{"a":{"$ref":"#/$defs/a"}}}"""
      """{"$defs":{"a":{"allOf":[{"$ref":"#/$defs/a"}]}}}"""
      """{"$defs":{"a":{"$ref":"#/$defs/b"},"b":{"$ref":"#/$defs/c"},"c":{"$ref":"#/$defs/a"}}}"""
      // a chain in a subschema held by a property named after a keyword whose
      // value is data (JYRO-DEF-581)
      """{"properties":{"default":{"not":{"$ref":"#/properties/default"}}}}"""
      """{"properties":{"enum":{"allOf":[{"$ref":"#/properties/enum"}]}}}"""
      """{"$defs":{"const":{"$ref":"#/$defs/examples"},"examples":{"$ref":"#/$defs/const"}}}"""
      """{"patternProperties":{"default":{"anyOf":[{"$ref":"#/patternProperties/default"}]}}}"""
      // a chain through the target of a $ref that sits in data (96.2.10)
      """{"$defs":{"d":{"default":{"not":{"$ref":"#/$defs/d/default"}}}},"$ref":"#/$defs/d/default"}"""
      """{"enum":[{"allOf":[{"$ref":"#/enum/0"}]}],"$ref":"#/enum/0"}"""
      """{"$ref":"#/const","const":{"$ref":"#"}}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1258")>]
[<MemberData(nameof cyclicSchemas)>]
let ``JYRO-REQ-1258 ValidateSchema_OfSchemaWithAStepChainCycle_RaisesSchemaInvalidForEveryData`` (schema: string) =
    // in a child process: an implementation that follows the cycle overflows the stack
    for data in [ "null"; "1"; "\"x\""; "[]"; "{}"; """{"default":1,"enum":1}""" ] do
        Assert.Equal(sprintf "ERR %A" MessageCode.SchemaInvalid, validateIsolated data schema)

let recursiveSchemas : obj[] seq =
    [ """{"properties":{"next":{"$ref":"#"}},"required":["v"]}""", """{"v":1,"next":{"v":2,"next":{"v":3,"next":{"v":4,"next":{"v":5}}}}}"""
      """{"items":{"$ref":"#"},"type":"array"}""", "[[[[[]]]]]"
      """{"prefixItems":[{"$ref":"#"}],"type":"array"}""", "[[[[[]]]]]"
      """{"additionalProperties":{"$ref":"#"},"type":"object"}""", """{"a":{"b":{"c":{"d":{}}}}}"""
      """{"patternProperties":{"x":{"$ref":"#"}},"type":"object"}""", """{"x":{"x":{"x":{"x":{}}}}}"""
      """{"contains":{"$ref":"#"}}""", "[[[[[1]]]]]"
      """{"$defs":{"s":{"maxLength":3}},"propertyNames":{"$ref":"#/$defs/s"},"additionalProperties":{"$ref":"#"}}""", """{"a":{"b":{"c":{}}}}"""
      """{"properties":{"default":{"properties":{"default":{"$ref":"#/properties/default"}}}}}""", """{"default":{"default":{"default":{}}}}"""
      """{"allOf":[{"properties":{"n":{"allOf":[{"$ref":"#"}]}}}]}""", """{"n":{"n":{"n":1}}}""" ]
    |> Seq.map (fun (s, d) -> [| box s; box d |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1258")>]
[<MemberData(nameof recursiveSchemas)>]
let ``JYRO-REQ-1258 ValidateSchema_OfRecursionThroughApplicatorsOnData_IsValid`` (schema: string, data: string) =
    Assert.Equal("OK []", validateIsolated data schema)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1258")>]
let ``JYRO-REQ-1258 ValidateSchema_TheValidExampleOfTheClause`` () =
    Assert.Equal("OK []", validateIsolated """{"next":{"next":{}}}""" """{"properties":{"next":{"$ref":"#"}}}""")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1258")>]
let ``JYRO-REQ-1258 ValidateSchema_RecursionThroughProperties_StillValidatesDeepData`` () =
    expectErrors [ "/next/next/v", "type" ] """{"v":1,"next":{"v":2,"next":{"v":"x"}}}""" """{"properties":{"next":{"$ref":"#"},"v":{"type":"integer"}}}"""

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1258")>]
[<InlineData("f", "InvalidType")>]
[<InlineData("ca", "ValueNestingLimitExceeded")>]
[<InlineData("hi", "UnpairedSurrogate")>]
let ``JYRO-REQ-1258 ValidateSchema_DataErrorComesBeforeTheSchemaCycle`` (dataExpr: string, code: string) =
    let expected = Enum.Parse<MessageCode>(code)
    let res = Script.runJson """{"s":{"$ref":"#"}}""" (loneDecl + cyclesDecl + sprintf "var f = x => x\nOutput.r = ValidateSchema(%s, Input.s)" dataExpr)
    Script.expectError expected res

// =========================================================================
// 96.2.35 unpaired surrogates in data
// =========================================================================

let private validateHost (data: JyroValue) (schemaJson: string) =
    Script.runInput (Values.obj [ "d", data; "s", Values.json schemaJson ]) "Output.r = ValidateSchema(Input.d, Input.s)"

let surrogateData : obj[] seq =
    [ "hi"; "lo"; "['a', hi]"; "{k: hi}"; "kl"; "[{a: [{b: [{c: hi}]}]}]" ] |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1259")>]
[<MemberData(nameof surrogateData)>]
let ``ValidateSchema_OfDataWithUnpairedSurrogate_RaisesUnpairedSurrogate`` (dataExpr: string) =
    for schema in [ """{}"""; """{"type":"string"}"""; """{"minLength":0}""" ] do
        let src = loneDecl + sprintf "var kl = {}\nkl['x' & lo] = 1\nOutput.r = ValidateSchema(%s, Input.s)" dataExpr
        Script.expectError MessageCode.UnpairedSurrogate (Script.runJson (sprintf "{\"s\": %s}" schema) src)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1259")>]
let ``ValidateSchema_OfHostDataWithUnpairedSurrogate_RaisesUnpairedSurrogate`` () =
    Script.expectError MessageCode.UnpairedSurrogate (validateHost (Values.str ("a" + hi)) "{}")
    Script.expectError MessageCode.UnpairedSurrogate (validateHost (Values.obj [ lo, Values.int 1 ]) "{}")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1259")>]
let ``ValidateSchema_OfDataWithSurrogatePairs_RaisesNoError`` () =
    let res = validateHost (Values.obj [ emoji, Values.str emoji ]) """{"additionalProperties":{"type":"string","minLength":2}}"""
    Assert.Equal(norm "[]", Values.toJson (Script.output res |> Values.prop "r"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1259")>]
let ``ValidateSchema_UnpairedSurrogateIsReportedBeforeAnInvalidSchema`` () =
    Script.expectError MessageCode.UnpairedSurrogate (validateHost (Values.str hi) """{"type":"bogus"}""")

// =========================================================================
// 96.2.36 dependentRequired
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1260")>]
[<InlineData("""{"a":1}""", """{"dependentRequired":{"a":["b","c"]}}""", "/b,/c")>]
[<InlineData("""{"a":1}""", """{"dependentRequired":{"a":["c","b"]}}""", "/b,/c")>]
[<InlineData("""{"a":1,"b":2}""", """{"dependentRequired":{"a":["b","c"]}}""", "/c")>]
[<InlineData("""{"b":1}""", """{"dependentRequired":{"a":["b","c"]}}""", "")>]
[<InlineData("""{"a":1,"b":1}""", """{"dependentRequired":{"a":["c"],"b":["c"]}}""", "/c")>]
[<InlineData("""{"a":1,"b":1}""", """{"dependentRequired":{"a":["c"],"b":["d"]}}""", "/c,/d")>]
[<InlineData("""{"x":{"a":1}}""", """{"properties":{"x":{"dependentRequired":{"a":["b"]}}}}""", "/x/b")>]
[<InlineData("""{"a":1}""", """{"dependentRequired":{"a":["m/n"]}}""", "/m~1n")>]
[<InlineData("""{"a":null}""", """{"dependentRequired":{"a":["b"]}}""", "/b")>]
[<InlineData("""{"a":1}""", """{"dependentRequired":{"a":[]}}""", "")>]
let ``JYRO-REQ-1260 ValidateSchema_DependentRequiredReportsEachAbsentPropertyAtItsPath`` (data: string, schema: string, paths: string) =
    let expected = if paths = "" then [] else paths.Split(',') |> Array.map (fun p -> p, "dependentRequired") |> List.ofArray
    expectErrors expected data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1260")>]
let ``JYRO-REQ-1260 ValidateSchema_DependentRequiredMessage`` () =
    Assert.Equal<string list>([ "dependentRequired at /b" ], elements """{"a":1}""" """{"dependentRequired":{"a":["b"]}}""" |> List.map (fun e -> e.Message))

// =========================================================================
// 96.2.37 propertyNames
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1261")>]
[<InlineData("""{"ab":1,"abc":2,"abcd":3}""", """{"propertyNames":{"maxLength":2}}""", "/abc,/abcd")>]
[<InlineData("""{"a/b":1}""", """{"propertyNames":{"pattern":"^[a-z]+$"}}""", "/a~1b")>]
[<InlineData("""{"x":1}""", """{"propertyNames":false}""", "/x")>]
[<InlineData("""{"abc":1}""", """{"propertyNames":{"minLength":5,"pattern":"^z"}}""", "/abc")>]
[<InlineData("""{"o":{"abc":1}}""", """{"properties":{"o":{"propertyNames":{"maxLength":2}}}}""", "/o/abc")>]
[<InlineData("""{"a":1}""", """{"propertyNames":{"not":{"const":"a"}}}""", "/a")>]
[<InlineData("""{"a":1}""", """{"propertyNames":{"anyOf":[{"minLength":3},{"maxLength":0}]}}""", "/a")>]
[<InlineData("""{"a":1}""", """{"propertyNames":{"$ref":"#/$defs/n"},"$defs":{"n":{"enum":["b"]}}}""", "/a")>]
let ``JYRO-REQ-1261 ValidateSchema_PropertyNamesReportsOneElementAtThePropertyAndNoInnerFailures`` (data: string, schema: string, paths: string) =
    let expected = paths.Split(',') |> Array.map (fun p -> p, "propertyNames") |> List.ofArray
    expectErrors expected data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1261")>]
let ``JYRO-REQ-1261 ValidateSchema_PropertyNamesBesideAFailingValueReportsBoth`` () =
    expectErrors [ "/abc", "propertyNames"; "/abc", "type" ] """{"abc":1}""" """{"propertyNames":{"maxLength":2},"additionalProperties":{"type":"string"}}"""

// =========================================================================
// 96.2.38 applicators report the inner failures
// =========================================================================

let applicatorCases : obj[] seq =
    [ """{"a":1}""", """{"properties":{"a":{"type":"string"}}}""", "/a:type"
      """{"x1":1}""", """{"patternProperties":{"^x":{"type":"string"}}}""", "/x1:type"
      """{"z":1}""", """{"additionalProperties":{"type":"string"}}""", "/z:type"
      "[1]", """{"items":{"type":"string"}}""", "/0:type"
      "[1]", """{"prefixItems":[{"type":"string"}]}""", "/0:type"
      "3", """{"allOf":[{"minimum":5},{"maximum":0}]}""", ":maximum,:minimum"
      "1", """{"$defs":{"s":{"type":"string"}},"$ref":"#/$defs/s"}""", ":type"
      "\"a\"", """{"if":{"type":"string"},"then":{"minLength":2}}""", ":minLength"
      "1", """{"if":{"type":"string"},"else":{"minimum":5}}""", ":minimum"
      """{"a":[1]}""", """{"properties":{"a":{"items":{"type":"string"}}}}""", "/a/0:type"
      """{"a":"x"}""", """{"properties":{"a":{"minLength":2,"pattern":"^y"}}}""", "/a:minLength,/a:pattern"
      """{"a":{"b":1}}""", """{"properties":{"a":{"$ref":"#/$defs/o"}},"$defs":{"o":{"properties":{"b":{"allOf":[{"type":"string"}]}}}}}""", "/a/b:type"
      """[1,"x",2]""", """{"prefixItems":[{"type":"string"}],"items":{"type":"string"}}""", "/0:type,/2:type" ]
    |> Seq.map (fun (d, s, e) -> [| box d; box s; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1262")>]
[<MemberData(nameof applicatorCases)>]
let ``JYRO-REQ-1262 ValidateSchema_ApplicatorReportsTheInnerFailuresAndNotItself`` (data: string, schema: string, expected: string) =
    let got = validate data schema
    Assert.Equal<(string * string) list>(pairs expected, got)
    let applicators = set [ "properties"; "patternProperties"; "additionalProperties"; "items"; "prefixItems"; "allOf"; "$ref"; "then"; "else" ]
    Assert.False(got |> List.exists (fun (_, k) -> applicators.Contains k), sprintf "an applying keyword was reported: %A" got)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1262")>]
let ``JYRO-REQ-1262 ValidateSchema_TheExampleOfTheClause`` () =
    expectErrors [ "/a", "type" ] """{"a":1}""" """{"properties":{"a":{"type":"string"}}}"""

// =========================================================================
// 96.2.39 false subschemas
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1263")>]
[<InlineData("""{"a":1}""", """{"properties":{"a":false}}""", "/a:properties")>]
[<InlineData("""{"b":1}""", """{"additionalProperties":false}""", "/b:additionalProperties")>]
[<InlineData("""{"b":1,"c":2}""", """{"additionalProperties":false}""", "/b:additionalProperties,/c:additionalProperties")>]
[<InlineData("[1,2]", """{"items":false}""", "/0:items,/1:items")>]
[<InlineData("[1,2]", """{"prefixItems":[true,false]}""", "/1:prefixItems")>]
[<InlineData("""{"x":1}""", """{"patternProperties":{"^x":false}}""", "/x:patternProperties")>]
[<InlineData("1", """{"allOf":[false]}""", ":allOf")>]
[<InlineData("1", """{"$defs":{"f":false},"$ref":"#/$defs/f"}""", ":$ref")>]
[<InlineData("1", """{"if":true,"then":false}""", ":then")>]
[<InlineData("1", """{"if":false,"else":false}""", ":else")>]
[<InlineData("[]", """{"items":false}""", "")>]
[<InlineData("""{"a":1,"b":2}""", """{"properties":{"a":true},"additionalProperties":false}""", "/b:additionalProperties")>]
[<InlineData("""{"o":{"a":1}}""", """{"properties":{"o":{"properties":{"a":false}}}}""", "/o/a:properties")>]
[<InlineData("[1]", """{"items":{"allOf":[false]}}""", "/0:allOf")>]
let ``JYRO-REQ-1263 ValidateSchema_FailingFalseSchemaReportsTheApplyingKeyword`` (data: string, schema: string, expected: string) =
    expectErrors (pairs expected) data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1263")>]
let ``JYRO-REQ-1263 ValidateSchema_TheExamplesOfTheClause`` () =
    expectErrors [ "/a", "properties" ] """{"a":1}""" """{"properties":{"a":false}}"""
    expectErrors [ "/b", "additionalProperties" ] """{"b":1}""" """{"additionalProperties":false}"""
    expectErrors [ "/0", "items"; "/1", "items" ] "[1, 2]" """{"items":false}"""

// =========================================================================
// 96.2.40 not, 96.2.41 contains, 96.2.42 if
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1264")>]
[<InlineData("\"x\"", """{"not":{"type":"string"}}""", "", true)>]
[<InlineData("1", """{"not":true}""", "", true)>]
[<InlineData("1", """{"not":{}}""", "", true)>]
[<InlineData("""{"a":5}""", """{"properties":{"a":{"not":{"minimum":0}}}}""", "/a", true)>]
[<InlineData("1", """{"not":{"not":{"type":"string"}}}""", "", true)>]
[<InlineData("1", """{"not":{"type":"string"}}""", "", false)>]
[<InlineData("1", """{"not":false}""", "", false)>]
[<InlineData("[1]", """{"items":{"not":{"type":"number"}}}""", "/0", true)>]
let ``JYRO-REQ-1264 ValidateSchema_FailingNotReportsOneElementAtTheValue`` (data: string, schema: string, path: string, fails: bool) =
    expectErrors (if fails then [ path, "not" ] else []) data schema

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1265")>]
[<InlineData("[1,2]", """{"contains":{"type":"string"}}""", "", true)>]
[<InlineData("[]", """{"contains":{"type":"string"}}""", "", true)>]
[<InlineData("[]", """{"contains":true}""", "", true)>]
[<InlineData("""{"l":[1]}""", """{"properties":{"l":{"contains":{"minimum":5}}}}""", "/l", true)>]
[<InlineData("[1]", """{"contains":false}""", "", true)>]
[<InlineData("""[{"a":1},{"a":2}]""", """{"contains":{"properties":{"a":{"const":3}}}}""", "", true)>]
[<InlineData("[1,\"x\"]", """{"contains":{"type":"string"}}""", "", false)>]
[<InlineData("5", """{"contains":{"type":"string"}}""", "", false)>]
[<InlineData("{}", """{"contains":{"type":"string"}}""", "", false)>]
let ``JYRO-REQ-1265 ValidateSchema_FailingContainsReportsOneElementAtTheArray`` (data: string, schema: string, path: string, fails: bool) =
    expectErrors (if fails then [ path, "contains" ] else []) data schema

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1266")>]
[<InlineData("1", """{"if":{"type":"string"}}""", "")>]
[<InlineData("1", """{"if":false}""", "")>]
[<InlineData("1", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", ":minimum")>]
[<InlineData("\"a\"", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", ":minLength")>]
[<InlineData("\"ab\"", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", "")>]
[<InlineData("7", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", "")>]
[<InlineData("1", """{"if":false,"else":{"type":"string"}}""", ":type")>]
[<InlineData("1", """{"if":{"type":"string"},"then":false}""", "")>]
[<InlineData("""{"p":1}""", """{"properties":{"p":{"if":{"minimum":5},"then":{"type":"string"},"else":{"maximum":0}}}}""", "/p:maximum")>]
[<InlineData("""{"p":9}""", """{"properties":{"p":{"if":{"minimum":5},"then":{"type":"string"},"else":{"maximum":0}}}}""", "/p:type")>]
[<InlineData("""{"a":1}""", """{"if":{"properties":{"a":{"type":"string"}}},"then":{"required":["t"]},"else":{"required":["e"]}}""", "/e:required")>]
let ``JYRO-REQ-1266 ValidateSchema_IfFailureIsNotReported_ThenOrElseFailuresAre`` (data: string, schema: string, expected: string) =
    let got = validate data schema
    Assert.Equal<(string * string) list>(pairs expected, got)
    Assert.False(got |> List.exists (fun (_, k) -> k = "if"), "an element has keyword if")

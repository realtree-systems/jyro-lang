/// jyro-spec 96.2 ValidateSchema, requirements 96.2.1 to 96.2.21.
module RealTree.Jyro.Spec.Tests.S96_02a

open System
open System.Diagnostics
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading.Tasks
open Xunit
open RealTree.Jyro
open RealTree.Jyro.Spec
open RealTree.Jyro.Spec.Tests.Util

/// Validate a script expression `dataExpr` against the schema given as JSON.
let private validateExprRaw (dataExpr: string) (schemaJson: string) =
    Script.runJson (sprintf "{\"s\": %s}" schemaJson) (sprintf "var f = x => x\n%sOutput.r = ValidateSchema(%s, Input.s)" cyclesDecl dataExpr)

let private validateExpr (dataExpr: string) (schemaJson: string) =
    Script.output (validateExprRaw dataExpr schemaJson) |> Values.prop "r" |> schemaErrors |> List.map (fun e -> e.Path, e.Keyword)

/// The (path, keyword) pairs of ValidateSchema(data, schema) under `boundedRegex`
/// (explicit limits and a regex timeout, FLOORS 4.1.11), asserting success.
let private validateB (dataJson: string) (schemaJson: string) =
    schemaResultB dataJson schemaJson |> List.map (fun (p, k, _) -> p, k)

let private expectErrorsB (expected: (string * string) list) (dataJson: string) (schemaJson: string) =
    Assert.Equal<(string * string) list>(expected, validateB dataJson schemaJson)

/// Assert ValidateSchema under `boundedRegex` raises one runtime error of `code`.
let private raisesB (code: MessageCode) (dataJson: string) (schemaJson: string) =
    expectRuntimeError code (validateRawB dataJson schemaJson)

/// Data of every JSON type, none holding a function, a cycle or a lone surrogate.
let private everyData = [ "null"; "true"; "1"; "\"x\""; "[]"; "[1]"; "{}"; """{"a":1}""" ]

// =========================================================================
// 96.2.1 signature
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1226")>]
[<InlineData("1")>]
[<InlineData("null")>]
[<InlineData("true")>]
[<InlineData("'s'")>]
[<InlineData("[1]")>]
[<InlineData("{a: 1}")>]
let ``ValidateSchema_TakesDataOfAnyTypeAndReturnsAnArray`` (data: string) =
    Assert.Equal("array", rStr (sprintf "Output.r = TypeOf(ValidateSchema(%s, {}))" data))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1226")>]
let ``ValidateSchema_WithNamedArgumentsInEitherOrder_ReturnsAnArray`` () =
    Assert.Equal(norm "[[],1]", rJson "Output.r = [ValidateSchema(data: 1, schema: {}), Length(ValidateSchema(schema: {type: 'string'}, data: 1))]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1226")>]
let ``ValidateSchema_WithOneArgument_FailsToCompileWithTooFewArguments`` () =
    compileFails MessageCode.TooFewArguments "Output.r = ValidateSchema(1)"

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1226")>]
let ``ValidateSchema_WithThreeArguments_FailsToCompileWithTooManyArguments`` () =
    compileFails MessageCode.TooManyArguments "Output.r = ValidateSchema(1, {}, 2)"

// =========================================================================
// 96.2.2 conforming data
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1860")>]
[<InlineData("""{"x":[1,{"y":null}]}""", """{}""")>]
[<InlineData("""1""", """{"type":"number"}""")>]
[<InlineData("""{"a":"x"}""", """{"type":"object","required":["a"],"properties":{"a":{"type":"string","minLength":1}}}""")>]
[<InlineData("""[1,2]""", """{"type":"array","items":{"type":"integer"},"uniqueItems":true}""")>]
[<InlineData("""5""", """{"maximum":5}""")>]
[<InlineData("""5""", """{"minimum":5}""")>]
[<InlineData("""4.9""", """{"exclusiveMaximum":5}""")>]
[<InlineData("""5.1""", """{"exclusiveMinimum":5}""")>]
[<InlineData("\"ab\"", """{"minLength":2}""")>]
[<InlineData("\"ab\"", """{"maxLength":2}""")>]
[<InlineData("""[1,2]""", """{"minItems":2,"maxItems":2}""")>]
[<InlineData("""{"a":1,"b":2}""", """{"minProperties":2,"maxProperties":2}""")>]
[<InlineData("""10""", """{"multipleOf":5}""")>]
[<InlineData("""2""", """{"enum":[1,2]}""")>]
[<InlineData("""2""", """{"const":2}""")>]
[<InlineData("""[1,"x"]""", """{"contains":{"type":"string"}}""")>]
[<InlineData("""{"a":1,"b":1}""", """{"dependentRequired":{"a":["b"]}}""")>]
[<InlineData("""{"ab":1}""", """{"propertyNames":{"maxLength":2}}""")>]
[<InlineData("""3""", """{"not":{"type":"string"}}""")>]
let ``JYRO-REQ-1860 ValidateSchema_WhenDataConforms_ReturnsEmptyArray`` (data: string, schema: string) =
    passes data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1860")>]
let ``JYRO-REQ-1860 ValidateSchema_TheClausesExamples_AreEmptyWhenTheDataConformsAndNotEmptyWhenItDoesNot`` () =
    Assert.Equal(norm "[]", rJson "Output.r = ValidateSchema(5, {type: 'number'})")
    Assert.Equal(norm "[]", rJson "Output.r = ValidateSchema('ab', {minLength: 2})")
    for call in [ "ValidateSchema('a', {minLength: 2})"; "ValidateSchema(5, {type: 'string'})" ] do
        Assert.Equal("true", rJson (sprintf "Output.r = Length(%s) >= 1" call))
        Assert.Equal("array", rStr (sprintf "Output.r = TypeOf(%s)" call))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1860")>]
let ``JYRO-REQ-1860 ValidateSchema_TheTestLengthOfTheResultIsZeroExactlyWhenTheDataConforms`` () =
    // A script tests Length(result) == 0: it is sound only if a failure is never empty.
    Assert.Equal(norm "[true,false,true,false]", rJson "Output.r = [Length(ValidateSchema(5, {type: 'number'})) == 0, Length(ValidateSchema(5, {type: 'string'})) == 0, Length(ValidateSchema('ab', {minLength: 2})) == 0, Length(ValidateSchema('a', {minLength: 2})) == 0]")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1860")>]
let ``JYRO-REQ-1860 ValidateSchema_OfNonConformingData_IsNeverEmpty_ForOneCasePerKeywordOf96_2`` () =
    let cases =
        [ "type", "1", """{"type":"string"}"""
          "enum", "3", """{"enum":[1,2]}"""
          "const", "3", """{"const":2}"""
          "multipleOf", "7", """{"multipleOf":5}"""
          "maximum", "6", """{"maximum":5}"""
          "exclusiveMaximum", "5", """{"exclusiveMaximum":5}"""
          "minimum", "4", """{"minimum":5}"""
          "exclusiveMinimum", "5", """{"exclusiveMinimum":5}"""
          "maxLength", "\"abc\"", """{"maxLength":2}"""
          "minLength", "\"a\"", """{"minLength":2}"""
          "pattern", "\"xbc\"", """{"pattern":"^a"}"""
          "maxItems", "[1,2,3]", """{"maxItems":2}"""
          "minItems", "[1]", """{"minItems":2}"""
          "uniqueItems", "[1,1]", """{"uniqueItems":true}"""
          "prefixItems", "[1]", """{"prefixItems":[{"type":"string"}]}"""
          "items", "[\"a\",1]", """{"items":{"type":"string"}}"""
          "contains", "[1,2]", """{"contains":{"type":"string"}}"""
          "maxProperties", """{"a":1,"b":2}""", """{"maxProperties":1}"""
          "minProperties", "{}", """{"minProperties":1}"""
          "required", """{"b":1}""", """{"required":["a"]}"""
          "dependentRequired", """{"a":1}""", """{"dependentRequired":{"a":["b"]}}"""
          "properties", """{"a":1}""", """{"properties":{"a":{"type":"string"}}}"""
          "patternProperties", """{"x1":1}""", """{"patternProperties":{"^x":{"type":"string"}}}"""
          "additionalProperties", """{"a":1,"b":2}""", """{"properties":{"a":{}},"additionalProperties":false}"""
          "propertyNames", """{"abc":1}""", """{"propertyNames":{"maxLength":2}}"""
          "allOf", "10", """{"allOf":[{"minimum":1},{"maximum":9}]}"""
          "anyOf", "0", """{"anyOf":[{"type":"string"},{"minimum":1}]}"""
          "oneOf", "0", """{"oneOf":[{"type":"string"},{"minimum":1}]}"""
          "not", "\"s\"", """{"not":{"type":"string"}}"""
          "false subschema", """{"a":1}""", """{"properties":{"a":false}}""" ]
    for (keyword, data, schema) in cases do
        let errs = validate data schema
        Assert.True(errs.Length >= 1, sprintf "%s: expected one or more elements for %s against %s" keyword data schema)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1860")>]
let ``JYRO-REQ-1860 ValidateSchema_ReturnsANewEmptyArrayEachCall`` () =
    Assert.Equal(norm "[[1],[]]", rJson "var a = ValidateSchema(1, {})\nvar b = ValidateSchema(1, {})\na = Append(a, 1)\nvar c = ValidateSchema(1, {})\nOutput.r = [a, c]")

// =========================================================================
// 96.2.3 draft 2020-12 only
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1228")>]
[<InlineData("""{"definitions":{"a":{}}}""")>]
[<InlineData("""{"dependencies":{"a":["b"]}}""")>]
[<InlineData("""{"id":"http://x/y"}""")>]
[<InlineData("""{"$recursiveRef":"#"}""")>]
[<InlineData("""{"$recursiveAnchor":true}""")>]
[<InlineData("""{"$dynamicRef":"#a"}""")>]
[<InlineData("""{"$dynamicAnchor":"a"}""")>]
[<InlineData("""{"$vocabulary":{}}""")>]
[<InlineData("""{"unevaluatedProperties":false}""")>]
[<InlineData("""{"unevaluatedItems":false}""")>]
[<InlineData("""{"dependentSchemas":{"a":{}}}""")>]
[<InlineData("""{"minContains":1,"contains":{}}""")>]
[<InlineData("""{"maxContains":1,"contains":{}}""")>]
[<InlineData("""{"contentEncoding":"base64"}""")>]
[<InlineData("""{"contentMediaType":"text/plain"}""")>]
[<InlineData("""{"contentSchema":{}}""")>]
[<InlineData("""{"additionalItems":false}""")>]
[<InlineData("""{"foo":1}""")>]
[<InlineData("""{"properties":{"a":{"definitions":{}}}}""")>]
[<InlineData("""{"items":{"dependencies":{}}}""")>]
[<InlineData("""{"allOf":[{"unevaluatedItems":true}]}""")>]
[<InlineData("""{"$defs":{"x":{"additionalItems":{}}}}""")>]
[<InlineData("""{"exclusiveMaximum":true,"maximum":5}""")>]
[<InlineData("""{"$schema":"http://json-schema.org/draft-07/schema#","items":[{"type":"integer"}]}""")>]
[<InlineData("""{"$schema":"http://json-schema.org/draft-04/schema#","exclusiveMinimum":true,"minimum":0}""")>]
[<InlineData("""{"$schema":"http://json-schema.org/draft-07/schema#","definitions":{"a":{}},"$ref":"#/definitions/a"}""")>]
[<InlineData("""{"items":[{}],"additionalItems":{"type":"string"}}""")>]
let ``JYRO-REQ-1228 ValidateSchema_OfKeywordsOfOtherDrafts_RaisesSchemaInvalid`` (schema: string) =
    for data in [ "[1]"; "null"; "{}" ] do
        schemaRaises MessageCode.SchemaInvalid data schema

// =========================================================================
// 96.2.4 the applied keywords
// =========================================================================

let keywordCases : obj[] seq =
    [ // type
      "\"x\"", """{"type":["string","null"]}""", true
      "null", """{"type":["string","null"]}""", true
      "1", """{"type":["string","null"]}""", false
      "1", """{"type":"string"}""", false
      "1.0", """{"type":"integer"}""", true
      "[]", """{"type":"object"}""", false
      "{}", """{"type":"array"}""", false
      "true", """{"type":"number"}""", false
      // enum
      """{"a":[1]}""", """{"enum":[1,{"a":[1]}]}""", true
      """{"a":[2]}""", """{"enum":[1,{"a":[1]}]}""", false
      "1.0", """{"enum":[1]}""", true
      "\"1\"", """{"enum":[1]}""", false
      "null", """{"enum":[0,false,""]}""", false
      // const
      """{"a":[1]}""", """{"const":{"a":[1]}}""", true
      """{"a":[1,2]}""", """{"const":{"a":[1]}}""", false
      """{"b":2,"a":1}""", """{"const":{"a":1,"b":2}}""", true
      "false", """{"const":0}""", false
      // multipleOf
      "1.5", """{"multipleOf":0.5}""", true
      "1.2", """{"multipleOf":0.5}""", false
      "\"x\"", """{"multipleOf":7}""", true
      // maximum / exclusiveMaximum
      "5", """{"maximum":5}""", true
      "5.01", """{"maximum":5}""", false
      "4.99", """{"exclusiveMaximum":5}""", true
      "5", """{"exclusiveMaximum":5}""", false
      "\"z\"", """{"maximum":0}""", true
      // minimum / exclusiveMinimum
      "5", """{"minimum":5}""", true
      "4.99", """{"minimum":5}""", false
      "5.01", """{"exclusiveMinimum":5}""", true
      "5", """{"exclusiveMinimum":5}""", false
      "[]", """{"minimum":5}""", true
      // maxLength / minLength
      "\"ab\"", """{"maxLength":2}""", true
      "\"abc\"", """{"maxLength":2}""", false
      "\"ab\"", """{"minLength":2}""", true
      "\"a\"", """{"minLength":2}""", false
      "1", """{"minLength":2}""", true
      // pattern
      "\"abc\"", """{"pattern":"^a"}""", true
      "\"xbc\"", """{"pattern":"^a"}""", false
      "12", """{"pattern":"^a"}""", true
      // maxItems / minItems
      "[1,2]", """{"maxItems":2}""", true
      "[1,2,3]", """{"maxItems":2}""", false
      "[1,2]", """{"minItems":2}""", true
      "[1]", """{"minItems":2}""", false
      "{}", """{"minItems":2}""", true
      // uniqueItems
      "[1,2]", """{"uniqueItems":true}""", true
      "[1,1.0]", """{"uniqueItems":true}""", false
      """[{"a":1},{"a":1}]""", """{"uniqueItems":true}""", false
      """[{"a":1,"b":2},{"b":2,"a":1}]""", """{"uniqueItems":true}""", false
      "[[1],[1]]", """{"uniqueItems":true}""", false
      "[1,\"1\",true,null,[1],{\"a\":1}]", """{"uniqueItems":true}""", true
      "[1,1]", """{"uniqueItems":false}""", true
      // items with prefixItems
      "[\"a\",1,2]", """{"prefixItems":[{"type":"string"}],"items":{"type":"integer"}}""", true
      "[\"a\",1,\"b\"]", """{"prefixItems":[{"type":"string"}],"items":{"type":"integer"}}""", false
      "[1]", """{"prefixItems":[{"type":"string"}]}""", false
      "[\"a\",{}]", """{"prefixItems":[{"type":"string"}]}""", true
      "[]", """{"prefixItems":[{"type":"string"}]}""", true
      "[\"a\"]", """{"prefixItems":[{"type":"string"}],"items":false}""", true
      "[\"a\",1]", """{"prefixItems":[{"type":"string"}],"items":false}""", false
      "{\"0\":1}", """{"items":{"type":"string"}}""", true
      // contains
      "[1,\"x\"]", """{"contains":{"type":"string"}}""", true
      "[1,2]", """{"contains":{"type":"string"}}""", false
      "\"s\"", """{"contains":{"type":"number"}}""", true
      // maxProperties / minProperties
      """{"a":1}""", """{"maxProperties":1}""", true
      """{"a":1,"b":2}""", """{"maxProperties":1}""", false
      """{"a":1}""", """{"minProperties":1}""", true
      """{}""", """{"minProperties":1}""", false
      "[]", """{"minProperties":1}""", true
      // required
      """{"a":null}""", """{"required":["a"]}""", true
      """{"b":1}""", """{"required":["a"]}""", false
      "[\"a\"]", """{"required":["a"]}""", true
      // dependentRequired
      """{"b":1}""", """{"dependentRequired":{"a":["b"]}}""", true
      """{"a":1}""", """{"dependentRequired":{"a":["b"]}}""", false
      // properties
      """{"a":"x"}""", """{"properties":{"a":{"type":"string"}}}""", true
      """{"a":1}""", """{"properties":{"a":{"type":"string"}}}""", false
      """{"b":1}""", """{"properties":{"a":{"type":"string"}}}""", true
      // patternProperties
      """{"x1":"s","y":1}""", """{"patternProperties":{"^x":{"type":"string"}}}""", true
      """{"x1":1}""", """{"patternProperties":{"^x":{"type":"string"}}}""", false
      // additionalProperties ignores properties and patternProperties matches
      """{"a":1,"x1":2}""", """{"properties":{"a":{}},"patternProperties":{"^x":{}},"additionalProperties":false}""", true
      """{"a":1,"b":2}""", """{"properties":{"a":{}},"patternProperties":{"^x":{}},"additionalProperties":false}""", false
      """{"a":1,"b":"s"}""", """{"properties":{"a":{"type":"integer"}},"additionalProperties":{"type":"string"}}""", true
      // propertyNames
      """{"ab":1}""", """{"propertyNames":{"maxLength":2}}""", true
      """{"abc":1}""", """{"propertyNames":{"maxLength":2}}""", false
      """{}""", """{"propertyNames":false}""", true
      // allOf / anyOf / oneOf
      "5", """{"allOf":[{"minimum":1},{"maximum":9}]}""", true
      "10", """{"allOf":[{"minimum":1},{"maximum":9}]}""", false
      "5", """{"anyOf":[{"type":"string"},{"minimum":1}]}""", true
      "0", """{"anyOf":[{"type":"string"},{"minimum":1}]}""", false
      "5", """{"oneOf":[{"type":"string"},{"minimum":1}]}""", true
      "0", """{"oneOf":[{"type":"string"},{"minimum":1}]}""", false
      "5", """{"oneOf":[{"type":"number"},{"minimum":1}]}""", false
      // not
      "1", """{"not":{"type":"string"}}""", true
      "\"s\"", """{"not":{"type":"string"}}""", false
      // if / then / else
      "\"ab\"", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", true
      "\"a\"", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", false
      "7", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", true
      "1", """{"if":{"type":"string"},"then":{"minLength":2},"else":{"minimum":5}}""", false
      "1", """{"if":{"type":"string"}}""", true
      "1", """{"then":{"type":"string"}}""", true
      "1", """{"else":{"type":"string"}}""", true
      // $ref and $defs
      "\"x\"", """{"$defs":{"s":{"type":"string"}},"$ref":"#/$defs/s"}""", true
      "1", """{"$defs":{"s":{"type":"string"}},"$ref":"#/$defs/s"}""", false
      "1", """{"$defs":{"s":{"type":"string"}}}""", true
      "1", """{"$ref":"#/$defs/s","$defs":{"s":{"minimum":0}},"maximum":0}""", false ]
    |> Seq.map (fun (d, s, ok) -> [| box d; box s; box ok |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1229")>]
[<MemberData(nameof keywordCases)>]
let ``JYRO-REQ-1229 ValidateSchema_AppliesEachKeywordAsDraft2020_12Gives`` (data: string, schema: string, conforms: bool) =
    if conforms then passes data schema else rejects data schema

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1860")>]
[<MemberData(nameof keywordCases)>]
let ``JYRO-REQ-1860 ValidateSchema_IsEmptyExactlyWhenTheDataConforms_ForEveryKeywordCase`` (data: string, schema: string, conforms: bool) =
    let errs = validate data schema
    if conforms then Assert.True(List.isEmpty errs, sprintf "expected an empty result for %s against %s, got %A" data schema errs)
    else Assert.True(errs.Length >= 1, sprintf "expected one or more elements for %s against %s" data schema)

/// uniqueItems decides equality as 30.10 does, which is not transitive across
/// exact and inexact numbers (JYRO-DEF-731): with a an exact 0.1, b an inexact
/// 0.1 from the host and c an exact 0.1000000000000000000000000001, [a, 2] and
/// [b, 2] are equal (a == b) wherever [c, 0] sits, and uniqueItems fails.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1229")>]
let ``JYRO-REQ-1229 ValidateSchema_UniqueItemsFindsADuplicateAcrossExactAndInexactNumbers`` () =
    let a2 () = Values.json "[0.1, 2]"
    let c0 () = Values.json "[0.1000000000000000000000000001, 0]"
    let b2 () = Values.arr [ Values.inexact 0.1; Values.int 2 ]
    let run (items: JyroValue list) =
        rValB (Values.obj [ "d", Values.arr items; "s", Values.json """{"uniqueItems":true}""" ]) "Output.r = ValidateSchema(Input.d, Input.s)"
        |> schemaErrors |> List.map (fun e -> e.Path, e.Keyword)
    // the inspector's case, and the other orders of its three elements
    for items in
        [ [ a2 (); c0 (); b2 () ]; [ c0 (); b2 (); a2 () ]; [ b2 (); c0 (); a2 () ]
          [ a2 (); b2 (); c0 () ]; [ c0 (); a2 (); b2 () ]; [ b2 (); a2 (); c0 () ] ] do
        Assert.Equal<(string * string) list>([ "", "uniqueItems" ], run items)
    // the bare numbers: a == b and b == c
    Assert.Equal<(string * string) list>([ "", "uniqueItems" ], run [ Values.json "0.1"; Values.json "0.1000000000000000000000000001"; Values.inexact 0.1 ])
    // contrasts: the two exact arrays differ, and [b, 1] equals neither
    Assert.Equal<(string * string) list>([], run [ a2 (); c0 () ])
    Assert.Equal<(string * string) list>([], run [ a2 (); c0 (); Values.arr [ Values.inexact 0.1; Values.int 1 ] ])

// =========================================================================
// 96.2.5 annotation keywords, 96.2.6 format
// =========================================================================

let annotationKeywords : obj[] seq =
    [ "\"$schema\":\"https://json-schema.org/draft/2020-12/schema\""
      "\"$schema\":\"http://example.invalid/unknown-meta\""
      "\"$id\":\"https://example.invalid/s\""
      "\"$comment\":\"note\""
      "\"$anchor\":\"here\""
      "\"title\":\"T\""
      "\"description\":\"D\""
      "\"default\":{\"wrong\":true}"
      "\"default\":{\"type\":\"number\",\"pattern\":\"(?i:a)\",\"foo\":1}"
      "\"examples\":[\"bad\",42]"
      "\"examples\":[{\"minLength\":1.5}]"
      "\"deprecated\":true"
      "\"readOnly\":true"
      "\"writeOnly\":true" ]
    |> Seq.map (fun k -> [| box k |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1230")>]
[<MemberData(nameof annotationKeywords)>]
let ``JYRO-REQ-1230 ValidateSchema_AnnotationKeywordIsAcceptedAndHasNoEffect`` (keyword: string) =
    let bare = """{"type":"object","required":["a"],"properties":{"a":{"type":"string"}}}"""
    let withRoot = sprintf """{%s,"type":"object","required":["a"],"properties":{"a":{"type":"string"}}}""" keyword
    let withNested = sprintf """{"type":"object","required":["a"],"properties":{"a":{%s,"type":"string"}}}""" keyword
    for data in [ """{"a":"x"}"""; """{"a":1}"""; """{}"""; "5"; """{"a":"A"}""" ] do
        let expected = validateB data bare
        Assert.Equal<(string * string) list>(expected, validateB data withRoot)
        Assert.Equal<(string * string) list>(expected, validateB data withNested)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1230")>]
let ``JYRO-REQ-1230 ValidateSchema_DefaultDoesNotFillInAMissingRequiredProperty`` () =
    expectErrors [ "/a", "required" ] "{}" """{"required":["a"],"properties":{"a":{"default":1}}}"""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1230")>]
let ``JYRO-REQ-1230 ValidateSchema_ReadOnlyAndWriteOnlyDoNotRejectPresentData`` () =
    passes """{"a":1,"b":2}""" """{"properties":{"a":{"readOnly":true},"b":{"writeOnly":true}}}"""
    passes """{"a":1}""" """{"properties":{"a":{"deprecated":true}}}"""

let formats : obj[] seq =
    [ "email"; "date-time"; "uri"; "ipv4"; "uuid"; "regex"; "foo" ] |> Seq.map (fun f -> [| box f |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1231")>]
[<MemberData(nameof formats)>]
let ``JYRO-REQ-1231 ValidateSchema_FormatIsAcceptedAndHasNoEffect`` (format: string) =
    for data in [ "\"not an email\""; "\"2024-13-45\""; "\"[\""; "\"a@b.c\""; "5"; "null"; "{}" ] do
        passes data (sprintf """{"format":"%s"}""" format)
        Assert.Equal<(string * string) list>(validate data """{"type":"string"}""", validate data (sprintf """{"type":"string","format":"%s"}""" format))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1231")>]
let ``JYRO-REQ-1231 ValidateSchema_FormatRegexDoesNotReadTheStringAsAPattern`` () =
    // a string that is no valid pattern, under format regex, is no error and raises nothing
    passes "\"(?i:a\"" """{"format":"regex"}"""
    passes "\"[\"" """{"properties":{"p":{"format":"regex"}}}"""

// =========================================================================
// 96.2.7 unknown keywords
// =========================================================================

let unknownPlaces : obj[] seq =
    [ """{"foo":1}"""
      """{"properties":{"a":{"foo":1}}}"""
      """{"patternProperties":{"^a":{"foo":1}}}"""
      """{"additionalProperties":{"foo":1}}"""
      """{"items":{"foo":1}}"""
      """{"prefixItems":[{},{"foo":1}]}"""
      """{"contains":{"foo":1}}"""
      """{"propertyNames":{"foo":1}}"""
      """{"allOf":[{"foo":1}]}"""
      """{"anyOf":[{},{"foo":1}]}"""
      """{"oneOf":[{"foo":1}]}"""
      """{"not":{"foo":1}}"""
      """{"if":{"foo":1}}"""
      """{"then":{"foo":1}}"""
      """{"else":{"foo":1}}"""
      """{"$defs":{"x":{"foo":1}}}"""
      """{"$defs":{"x":{"properties":{"y":{"foo":1}}}}}"""
      // a subschema held by a property, or a $defs entry, named after a keyword
      // whose value is data (JYRO-DEF-580, 581) or a map (JYRO-DEF-579)
      """{"properties":{"default":{"foo":1}}}"""
      """{"properties":{"enum":{"foo":1}}}"""
      """{"properties":{"const":{"foo":1}}}"""
      """{"properties":{"examples":{"foo":1}}}"""
      """{"$defs":{"default":{"foo":1}}}"""
      """{"patternProperties":{"enum":{"foo":1}}}"""
      """{"properties":{"patternProperties":{"^a":{}}}}"""
      """{"properties":{"properties":{"a":{}}}}"""
      // an unknown keyword that a $ref names a place under (JYRO-ADR-148)
      """{"properties":{"x":{"$ref":"#/x-defs/a"}},"x-defs":{"a":{"type":"string"}}}"""
      """{"x-defs":{"a":{}}}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1232")>]
[<MemberData(nameof unknownPlaces)>]
let ``JYRO-REQ-1232 ValidateSchema_OfUnknownKeywordAnywhere_RaisesSchemaInvalid`` (schema: string) =
    for data in [ "null"; "{}"; "[]"; "\"s\""; """{"x":1,"a":1,"default":1}""" ] do
        schemaRaises MessageCode.SchemaInvalid data schema

let notKeywordPlaces : obj[] seq =
    [ """{"properties":{"minimum":{},"foo":{}}}"""
      """{"patternProperties":{"foo":{}}}"""
      """{"$defs":{"foo":{}}}"""
      """{"const":{"foo":1}}"""
      """{"enum":[{"foo":1}]}"""
      """{"default":{"foo":1}}"""
      """{"examples":[{"foo":1}]}"""
      """{"default":{"properties":{"a":{"foo":1}},"x-defs":{}}}"""
      """{"dependentRequired":{"foo":["bar"]}}"""
      """{"required":["foo","$ref"]}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1232")>]
[<MemberData(nameof notKeywordPlaces)>]
let ``JYRO-REQ-1232 ValidateSchema_OfNamesThatAreNotKeywords_RaisesNoSchemaInvalid`` (schema: string) =
    validate """{"foo":1}""" schema |> ignore

// =========================================================================
// 96.2.8 malformed keyword values
// =========================================================================

let malformed : obj[] seq =
    [ """{"type":"bogus"}"""; """{"type":["string","string"]}"""; """{"type":1}"""; """{"type":["string",1]}"""
      """{"minLength":1.5}"""; """{"minLength":-1}"""; """{"minLength":"2"}"""; """{"minProperties":1.5}"""
      """{"maxItems":-1}"""; """{"multipleOf":0}"""; """{"multipleOf":-2}"""; """{"multipleOf":"2"}"""
      """{"required":"a"}"""; """{"required":[1]}"""; """{"required":["a","a"]}"""
      """{"enum":1}"""; """{"properties":[]}"""; """{"items":[{}]}"""; """{"items":5}"""
      """{"prefixItems":{}}"""; """{"prefixItems":[]}"""; """{"pattern":5}"""
      """{"dependentRequired":{"a":"b"}}"""; """{"dependentRequired":{"a":["b","b"]}}"""; """{"dependentRequired":{"a":[1]}}"""
      """{"allOf":[]}"""; """{"allOf":{}}"""; """{"allOf":[1]}"""
      """{"anyOf":[]}"""; """{"oneOf":[]}"""; """{"not":5}"""; """{"$defs":[]}"""; """{"$defs":{"a":1}}"""
      """{"$ref":5}"""; """{"if":"x"}"""; """{"uniqueItems":"yes"}"""; """{"maximum":"5"}"""; """{"exclusiveMinimum":"1"}"""
      """{"$comment":1}"""; """{"title":1}"""; """{"description":[]}"""; """{"deprecated":"yes"}"""; """{"readOnly":"yes"}"""
      """{"examples":1}"""; """{"format":1}"""
      """{"properties":{"a":5}}"""; """{"patternProperties":{"^a":"x"}}"""; """{"additionalProperties":1}"""
      """{"propertyNames":5}"""; """{"contains":"x"}"""
      """{"properties":{"a":{"type":"bogus"}}}"""; """{"items":{"minLength":1.5}}"""
      """{"properties":{"default":{"minLength":1.5}}}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1233")>]
[<MemberData(nameof malformed)>]
let ``JYRO-REQ-1233 ValidateSchema_OfMalformedKeywordValue_RaisesSchemaInvalid`` (schema: string) =
    for data in [ "null"; """{"a":1}""" ] do
        schemaRaises MessageCode.SchemaInvalid data schema

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1233")>]
[<InlineData("""{"minLength":5.0}""")>]
[<InlineData("""{"type":["string","null"]}""")>]
[<InlineData("""{"required":[]}""")>]
[<InlineData("""{"multipleOf":0.1}""")>]
[<InlineData("""{"maxItems":0}""")>]
[<InlineData("""{"enum":[]}""")>]
[<InlineData("""{"dependentRequired":{"a":[]}}""")>]
[<InlineData("""{"default":{"minLength":1.5}}""")>]
[<InlineData("""{"const":{"type":"bogus"}}""")>]
let ``JYRO-REQ-1233 ValidateSchema_OfWellFormedContrasts_RaisesNoError`` (schema: string) =
    validate "null" schema |> ignore

// =========================================================================
// 96.2.9 boolean subschemas
// =========================================================================

let allData = [ "null"; "true"; "1"; "\"s\""; "[1]"; """{"a":1}""" ]

let trueSchemas : obj[] seq =
    [ """{"properties":{"a":true}}"""; """{"items":true}"""; """{"additionalProperties":true}"""
      """{"allOf":[true]}"""; """{"not":false}"""; """{"anyOf":[false,true]}"""
      """{"$defs":{"x":true},"$ref":"#/$defs/x"}"""; """{"prefixItems":[true]}"""; """{"contains":true}"""
      """{"if":true,"then":true,"else":false}"""; """{"propertyNames":true}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1234")>]
[<MemberData(nameof trueSchemas)>]
let ``JYRO-REQ-1234 ValidateSchema_TrueSubschemaAcceptsEveryValue`` (schema: string) =
    // the only array of allData, [1], has an element for contains to match
    for d in allData do passes d schema

let falseSchemas : obj[] seq =
    [ """{"allOf":[false]}"""; """{"not":true}"""; """{"anyOf":[false]}"""; """{"oneOf":[false]}"""
      """{"$defs":{"x":false},"$ref":"#/$defs/x"}"""; """{"if":true,"then":false}"""; """{"if":false,"else":false}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1234")>]
[<MemberData(nameof falseSchemas)>]
let ``JYRO-REQ-1234 ValidateSchema_FalseSubschemaRejectsEveryValue`` (schema: string) =
    for d in allData do rejects d schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1234")>]
let ``JYRO-REQ-1234 ValidateSchema_FalseSubschemaUnderPropertiesItemsAdditional_RejectsTheValuesItReaches`` () =
    rejects """{"a":1}""" """{"properties":{"a":false}}"""
    rejects """{"a":null}""" """{"properties":{"a":false}}"""
    rejects "[null]" """{"items":false}"""
    rejects """{"b":{}}""" """{"additionalProperties":false}"""
    rejects "[1]" """{"prefixItems":[false]}"""
    rejects """{"x":1}""" """{"patternProperties":{"x":false}}"""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1234")>]
let ``JYRO-REQ-1234 ValidateSchema_FalseSubschemaNoDataReaches_DoesNotFail`` () =
    passes """{"b":1}""" """{"properties":{"a":false}}"""
    passes "[]" """{"items":false}"""
    passes "5" """{"properties":{"a":false},"items":false}"""
    passes "[]" """{"prefixItems":[false]}"""
    passes "1" """{"$defs":{"never":false}}"""
    passes "1" """{"if":false,"then":false}"""

// =========================================================================
// 96.2.10 local $ref resolution, and the target read as a subschema (JYRO-ADR-148)
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1706")>]
let ``JYRO-REQ-1706 ValidateSchema_RefHashNamesTheRoot`` () =
    expectErrors [ "/n", "type" ] """{"n":1}""" """{"properties":{"n":{"$ref":"#"}},"type":"object"}"""
    passes """{"n":{"n":{}}}""" """{"properties":{"n":{"$ref":"#"}},"type":"object"}"""

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1706")>]
[<InlineData("#/$defs/a%20b", "a b")>]
[<InlineData("#/$defs/a%25b", "a%b")>]
[<InlineData("#/$defs/a~1b", "a/b")>]
[<InlineData("#/$defs/a~0b", "a~b")>]
[<InlineData("#/$defs/a%7E1b", "a/b")>]
[<InlineData("#/$defs/a%7e0b", "a~b")>]
[<InlineData("#/$defs/%C3%A9", "\u00E9")>]
[<InlineData("#/$defs/%c3%a9", "\u00E9")>]
[<InlineData("#/$defs/%F0%9F%98%80", "\U0001F600")>]
[<InlineData("#/$defs/%61", "a")>]
[<InlineData("#/$defs/", "")>]
let ``JYRO-REQ-1706 ValidateSchema_RefIsPercentDecodedThenReadAsJsonPointer`` (reference: string, key: string) =
    let keyJson = Values.toJson (Values.str key)
    // the named key holds a string schema; every other key holds an integer schema
    let schema =
        sprintf """{"$defs":{%s:{"type":"string"},"a%%20b":{"type":"integer"},"a%%7E1b":{"type":"integer"},"decoy":{"type":"integer"}},"$ref":"%s"}""" keyJson reference
    expectErrors [ "", "type" ] "1" schema
    passes "\"s\"" schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1706")>]
let ``JYRO-REQ-1706 ValidateSchema_RefIntoPrefixItemsResolves`` () =
    let schema = """{"prefixItems":[{"type":"string"}],"properties":{"p":{"$ref":"#/prefixItems/0"}}}"""
    expectErrors [ "/p", "type" ] """{"p":1}""" schema
    passes """{"p":"x"}""" schema

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1706")>]
[<InlineData("#/$defs/a%2")>]
[<InlineData("#/$defs/a%")>]
[<InlineData("#/$defs/a%zz")>]
[<InlineData("#/$defs/a%2g")>]
[<InlineData("#/$defs/a%FF")>]
[<InlineData("#/$defs/%C3")>]
[<InlineData("#/$defs/%C3%28")>]
[<InlineData("#/$defs/%ED%A0%80")>]
[<InlineData("#/$defs/missing")>]
[<InlineData("#foo")>]
let ``JYRO-REQ-1706 ValidateSchema_RefWithBadEscapeOrBytes_RaisesSchemaInvalid`` (reference: string) =
    // decoys: keys an implementation would name if it read the text without
    // decoding it, or decoded the bytes as Latin-1
    let schema = sprintf """{"$defs":{"a":{},"a%%2":{},"a%%":{},"a%%zz":{},"a%%2g":{},"\u00c3":{},"\u00c3(":{},"%%C3":{}},"$ref":"%s"}""" reference
    for data in [ "1"; "null" ] do
        schemaRaises MessageCode.SchemaInvalid data schema

/// The four examples of 96.2.10 (JYRO-REQ-1706).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1706")>]
let ``JYRO-REQ-1706 ValidateSchema_TheExamplesOfTheClause`` () =
    expectErrorsB [ "", "type" ] "1" """{"$ref":"#/$defs/d/default","$defs":{"d":{"default":{"type":"string"}}}}"""
    raisesB MessageCode.RegexInvalidPattern "\"A\"" """{"$ref":"#/$defs/d/default","$defs":{"d":{"default":{"pattern":"(?i:a)"}}}}"""
    schemaRaises MessageCode.SchemaInvalid "{}" """{"properties":{"a":{"type":"string"}},"$ref":"#/properties"}"""
    schemaRaises MessageCode.SchemaInvalid """{"x":1}""" """{"properties":{"x":{"$ref":"#/x-defs/a"}},"x-defs":{"a":{"type":"string"}}}"""

/// (data, schema, expected "path:keyword" list). The target of each $ref sits
/// where 96.2 otherwise reads data: inside default, enum, const or examples, at
/// a map of names, or under a name that is not a keyword inside such data. It
/// is applied as a subschema, and its failures are reported with their own
/// keyword and path (96.2.38).
let targetCases : obj[] seq =
    [ "1", """{"$ref":"#/$defs/d/default","$defs":{"d":{"default":{"type":"string"}}}}""", ":type"
      "\"s\"", """{"$ref":"#/$defs/d/default","$defs":{"d":{"default":{"type":"string"}}}}""", ""
      "1", """{"$ref":"#/default","default":{"type":"string"}}""", ":type"
      "1", """{"enum":[1,{"type":"string"}],"$ref":"#/enum/1"}""", ":type"
      """{"type":"string"}""", """{"enum":[1,{"type":"string"}],"$ref":"#/enum/1"}""", ":type"
      "\"s\"", """{"enum":[1,{"type":"string"}],"$ref":"#/enum/1"}""", ":enum"
      "3", """{"const":{"minimum":5},"$ref":"#/const"}""", ":const,:minimum"
      """{"minimum":5}""", """{"const":{"minimum":5},"$ref":"#/const"}""", ""
      "\"ab\"", """{"examples":[{"maxLength":1}],"$ref":"#/examples/0"}""", ":maxLength"
      "\"a\"", """{"examples":[{"maxLength":1}],"$ref":"#/examples/0"}""", ""
      """{"a":1}""", """{"$ref":"#/default","default":{"properties":{"a":{"type":"string"}}}}""", "/a:type"
      """{"not":1}""", """{"properties":{"not":{}},"$ref":"#/properties"}""", ":not"
      "5", """{"properties":{"not":{}},"$ref":"#/properties"}""", ":not"
      "1", """{"default":{"x-defs":{"a":{"type":"string"}}},"$ref":"#/default/x-defs/a"}""", ":type"
      "1", """{"properties":{"p":{"default":{"t":{"type":"string"}}}},"$ref":"#/properties/p/default/t"}""", ":type"
      "1", """{"$ref":"#/default","default":{"$ref":"#/$defs/s"},"$defs":{"s":{"type":"string"}}}""", ":type"
      """{"p":1}""", """{"properties":{"p":{"$ref":"#/enum/0"}},"enum":[{"type":"string"},{"p":1}]}""", "/p:type" ]
    |> Seq.map (fun (d, s, e) -> [| box d; box s; box e |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1706")>]
[<MemberData(nameof targetCases)>]
let ``JYRO-REQ-1706 ValidateSchema_AppliesTheTargetOfARefWhereverItSits`` (data: string, schema: string, expected: string) =
    let exp =
        if expected = "" then []
        else expected.Split(',') |> Array.map (fun p -> let i = p.LastIndexOf(':') in p.Substring(0, i), p.Substring(i + 1)) |> List.ofArray
    expectErrorsB exp data schema

/// Every rule of 96.2 for a schema inside `schema` applies to a target that
/// sits in data: an unknown keyword (96.2.7), a malformed value (96.2.8), a
/// pattern (96.2.19), each raised whatever the data. The same values with no
/// $ref naming them are data and raise nothing (96.2.5).
let targetRuleCases : obj[] seq =
    [ """{"$ref":"#/default","default":{"foo":1}}""", "SchemaInvalid"
      """{"$ref":"#/enum/0","enum":[{"foo":1}]}""", "SchemaInvalid"
      """{"$ref":"#/const","const":{"foo":1}}""", "SchemaInvalid"
      """{"$ref":"#/examples/0","examples":[{"foo":1}]}""", "SchemaInvalid"
      """{"$ref":"#/$defs","$defs":{"x":{}}}""", "SchemaInvalid"
      """{"$ref":"#/patternProperties","patternProperties":{"^a":{}}}""", "SchemaInvalid"
      """{"$ref":"#/properties","properties":{"a":{}}}""", "SchemaInvalid"
      """{"$ref":"#/default","default":{"properties":{"a":{"foo":1}}}}""", "SchemaInvalid"
      """{"$ref":"#/const","const":{"minLength":1.5}}""", "SchemaInvalid"
      """{"$ref":"#/default","default":{"type":"bogus"}}""", "SchemaInvalid"
      """{"$ref":"#/examples/0","examples":[{"items":[{}]}]}""", "SchemaInvalid"
      """{"$ref":"#/default","default":{"pattern":"(?i:a)"}}""", "RegexInvalidPattern"
      """{"$ref":"#/enum/0","enum":[{"pattern":"\\Aa"}]}""", "RegexInvalidPattern"
      """{"$ref":"#/const","const":{"patternProperties":{"[":{}}}}""", "RegexInvalidPattern" ]
    |> Seq.map (fun (s, c) -> [| box s; box c |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1706")>]
[<MemberData(nameof targetRuleCases)>]
let ``JYRO-REQ-1706 ValidateSchema_AppliesEveryRuleOfASchemaToTheTarget`` (schema: string, code: string) =
    let expected = Enum.Parse<MessageCode>(code)
    for data in [ "\"A\""; "1"; "null"; "{}"; """{"a":"A"}""" ] do
        raisesB expected data schema
    // the same value, named by no $ref, is data
    let withoutRef = schema.Substring(0, 1) + schema.Substring(schema.IndexOf(',') + 1)
    for data in [ "\"A\""; "null" ] do
        Assert.True(Script.ok (validateRawB data withoutRef), sprintf "%s raised: %s" withoutRef (Script.describe (validateRawB data withoutRef)))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1706")>]
let ``JYRO-REQ-1706 ValidateSchema_APatternInATargetIsReadAsARegularExpressionFunctionReadsIt`` () =
    let schema = """{"$ref":"#/default","default":{"pattern":"^\\d$"}}"""
    expectErrorsB [ "", "pattern" ] "\"\u0663\"" schema
    expectErrorsB [] "\"3\"" schema

// =========================================================================
// 96.2.11 no network, no file
// =========================================================================

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1236")>]
let ``ValidateSchema_OpensNoNetworkConnectionForRefIdOrSchema`` () =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    listener.Start()
    try
        let port = (listener.LocalEndpoint :?> IPEndPoint).Port
        let refs =
            [ sprintf """{"$ref":"http://127.0.0.1:%d/s.json"}""" port, true
              sprintf """{"$id":"http://127.0.0.1:%d/root.json","$ref":"other.json"}""" port, true
              sprintf """{"properties":{"a":{"$ref":"http://127.0.0.1:%d/a.json"}}}""" port, true
              sprintf """{"$defs":{"x":{"$ref":"http://127.0.0.1:%d/x.json"}}}""" port, true
              sprintf """{"$schema":"http://127.0.0.1:%d/meta"}""" port, false
              sprintf """{"$id":"http://127.0.0.1:%d/x"}""" port, false ]
        for schema, invalid in refs do
            let res = validateRaw """{"a":1}""" schema
            if invalid then Script.expectError MessageCode.SchemaInvalid res
            else Assert.True(Script.ok res, Script.describe res)
            Assert.False(listener.Pending(), sprintf "a connection reached the listener for %s" schema)
        let accept = listener.AcceptTcpClientAsync()
        Assert.False(accept.Wait(500), "a connection reached the listener")
    finally
        listener.Stop()

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1236")>]
let ``ValidateSchema_ReadsNoFileForRef`` () =
    if OperatingSystem.IsWindows() then () else
    let dir = Path.Combine(Path.GetTempPath(), "jyro-9602-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    let fifo = Path.Combine(dir, "s.json")
    try
        use mk = Process.Start(ProcessStartInfo("mkfifo", fifo, UseShellExecute = false))
        mk.WaitForExit()
        Assert.Equal(0, mk.ExitCode)
        let uri = Uri(fifo).AbsoluteUri
        for reference in [ uri; fifo; "s.json" ] do
            let schema = sprintf """{"$ref":%s}""" (Values.toJson (Values.str reference))
            let call = Task.Run(fun () -> validateRaw "1" schema)
            // A reader of the FIFO blocks until a writer opens it, so a call that
            // opens the file does not return.
            let finished = call.Wait(3000)
            if not finished then
                // release the blocked reader so the test host is not left hanging
                use w = new FileStream(fifo, FileMode.Open, FileAccess.Write)
                w.WriteByte(byte '{')
                call.Wait(3000) |> ignore
            Assert.True(finished, sprintf "ValidateSchema blocked on the FIFO for $ref %s: it opened the file" reference)
            Script.expectError MessageCode.SchemaInvalid call.Result
    finally
        try Directory.Delete(dir, true) with _ -> ()

// =========================================================================
// 96.2.12 a $ref that does not start with #, does not resolve, or names no schema
// =========================================================================

let badRefs : obj[] seq =
    [ "http://example.com/s"; "s.json"; "/abs"; "urn:x"; ""; "$defs/a"
      "#/$defs/missing"; "#/properties/a/items"; "#/allOf/5"; "#/allOf/-"; "#/allOf/01"; "#/allOf/ 0"
      "#/required/0"; "#/required"; "#/allOf"; "#/minProperties"; "#/title"; "#/$defs/n"; "#/required/0/x" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1707")>]
[<MemberData(nameof badRefs)>]
let ``JYRO-REQ-1707 ValidateSchema_OfRefThatDoesNotStartWithHashResolveOrNameASchema_RaisesSchemaInvalid`` (reference: string) =
    let refJson = Values.toJson (Values.str reference)
    let atRoot = sprintf """{"$defs":{"a":{}},"allOf":[{}],"required":["a"],"minProperties":0,"title":"t","properties":{"a":{}},"$ref":%s}""" refJson
    let nested = sprintf """{"$defs":{"a":{}},"allOf":[{}],"required":["a"],"minProperties":0,"title":"t","properties":{"a":{"$ref":%s}}}""" refJson
    if reference = "#/$defs/n" then
        // a $defs entry null is refused by 96.2.8 on its own, so the null target
        // is placed in data, where only the $ref makes it a would-be schema
        schemaRaises MessageCode.SchemaInvalid """{"a":1}""" """{"default":{"n":null},"$ref":"#/default/n"}"""
        schemaRaises MessageCode.SchemaInvalid """{"a":1}""" """{"properties":{"a":{"$ref":"#/examples/0"}},"examples":[null]}"""
    else
        schemaRaises MessageCode.SchemaInvalid """{"a":1}""" atRoot
        schemaRaises MessageCode.SchemaInvalid """{"a":1}""" nested

/// The examples of 96.2.12 (JYRO-REQ-1707).
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1707")>]
[<InlineData("https://example.com/s")>]
[<InlineData("#/$defs/missing")>]
[<InlineData("#/required/2")>]
[<InlineData("#/required/0")>]
let ``JYRO-REQ-1707 ValidateSchema_TheExamplesOfTheClause`` (reference: string) =
    for data in [ "{}"; """{"a":1,"b":2}"""; "1" ] do
        schemaRaises MessageCode.SchemaInvalid data (sprintf """{"required":["a","b"],"$ref":"%s"}""" reference)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1707")>]
let ``JYRO-REQ-1707 ValidateSchema_TheExampleOfARefTheDataDoesNotReach`` () =
    schemaRaises MessageCode.SchemaInvalid "{}" """{"properties":{"p":{"$ref":"#/required/0"}},"required":["a"]}"""

/// A $ref that 96.2.12 refuses, where no data reaches it: in a $defs entry that
/// no $ref names, under a property the data lacks, under items for data that
/// is not an array, in a branch that if does not choose, beside a branch that
/// already passes.
let unreachedRefs : obj[] seq =
    [ """{"$defs":{"u":{"$ref":"#/$defs/missing"}}}"""
      """{"$defs":{"u":{"properties":{"q":{"$ref":"#/required/0"}}}},"required":["a"]}"""
      """{"properties":{"p":{"$ref":"#/nowhere"}}}"""
      """{"properties":{"p":{"$ref":"http://example.com/s"}}}"""
      """{"items":{"$ref":"#/minimum"},"minimum":0}"""
      """{"if":false,"then":{"$ref":"#/nope"}}"""
      """{"if":true,"else":{"$ref":"#/nope"}}"""
      """{"anyOf":[true,{"$ref":"#/nope"}]}"""
      """{"propertyNames":{"$ref":"#/title"},"title":"t"}"""
      """{"$defs":{"u":{"$ref":"#/default"}},"default":"text"}""" ]
    |> Seq.map (fun s -> [| box s |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1707")>]
[<MemberData(nameof unreachedRefs)>]
let ``JYRO-REQ-1707 ValidateSchema_RefusesARefNoDataReaches_ForEveryData`` (schema: string) =
    for data in everyData do
        schemaRaises MessageCode.SchemaInvalid data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1707")>]
let ``JYRO-REQ-1707 ValidateSchema_OfResolvingRefInDefs_RaisesNoError`` () =
    passes """{"a":1}""" """{"$defs":{"a":{"$ref":"#/$defs/b"},"b":{"type":"integer"}},"properties":{"a":{"$ref":"#/$defs/a"}}}"""
    expectErrors [ "/a", "type" ] """{"a":"x"}""" """{"$defs":{"a":{"$ref":"#/$defs/b"},"b":{"type":"integer"}},"properties":{"a":{"$ref":"#/$defs/a"}}}"""

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1707")>]
let ``JYRO-REQ-1707 ValidateSchema_ABooleanTargetIsASchema`` () =
    // the example of the clause: {$defs: {t: true}, $ref: "#/$defs/t"} is not refused
    for data in everyData do passes data """{"$defs":{"t":true},"$ref":"#/$defs/t"}"""
    // a false target rejects every value, reported with the keyword that applied it (96.2.39)
    for data in everyData do expectErrors [ "", "$ref" ] data """{"$defs":{"f":false},"$ref":"#/$defs/f"}"""
    // a boolean target that sits in data
    expectErrors [ "", "$ref" ] "false" """{"const":false,"$ref":"#/const"}"""
    expectErrors [ "", "$ref"; "", "const" ] "true" """{"const":false,"$ref":"#/const"}"""
    passes "true" """{"const":true,"$ref":"#/const"}"""
    // the false is applied by the $ref inside properties' subschema: the $ref is
    // the keyword that applied it (96.2.39), and properties reports the inner
    // failure, not itself (96.2.38)
    expectErrors [ "/p", "$ref" ] """{"p":1}""" """{"properties":{"p":{"$ref":"#/enum/1"}},"enum":[{"p":1},false]}"""

// =========================================================================
// 96.2.13 data read as its JSON value
// =========================================================================

let typeKinds : obj[] seq =
    [ "null", "null"; "true", "boolean"; "1", "number"; "1.5", "number"; "1", "integer"
      "'s'", "string"; "[]", "array"; "{}", "object"; "false", "boolean"; "''", "string"; "[null]", "array" ]
    |> Seq.map (fun (d, t) -> [| box d; box t |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1238")>]
[<MemberData(nameof typeKinds)>]
let ``JYRO-REQ-1238 ValidateSchema_ReadsDataAsItsJsonType`` (dataExpr: string, jsonType: string) =
    Assert.Equal<(string * string) list>([], validateExpr dataExpr (sprintf """{"type":"%s"}""" jsonType))
    for other in [ "null"; "boolean"; "number"; "integer"; "string"; "array"; "object" ] do
        let alsoPasses = (other = jsonType) || (other = "number" && jsonType = "integer") || (other = "integer" && dataExpr = "1")
        if not alsoPasses then
            Assert.Equal<(string * string) list>([ "", "type" ], validateExpr dataExpr (sprintf """{"type":"%s"}""" other))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1238")>]
let ``JYRO-REQ-1238 ValidateSchema_ReadsNumbersByValueInEitherRepresentation`` () =
    let input = Values.obj [ "i", Values.inexact 0.1; "s", Values.json """{"const":0.1}""" ]
    let v = rVal input "Output.r = [ValidateSchema(Input.i, Input.s), ValidateSchema(0.1, Input.s)]"
    Assert.Equal(norm "[[],[]]", Values.toJson v)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1238")>]
let ``JYRO-REQ-1238 ValidateSchema_PropertyOrderDoesNotChangeTheResult`` () =
    let schema = """{"properties":{"a":{"type":"string"},"b":{"type":"string"}},"required":["a","b","c"]}"""
    let one = validateExpr "{b: 1, a: 2}" schema
    Assert.Equal<(string * string) list>(one, validateExpr "{a: 2, b: 1}" schema)
    Assert.Equal<(string * string) list>([ "/a", "type"; "/b", "type"; "/c", "required" ], one)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1238")>]
let ``JYRO-REQ-1238 ValidateSchema_OfNestedDataFromInputAndLiteral_GivesTheSameResult`` () =
    let schema = """{"items":{"properties":{"a":{"prefixItems":[{"type":"null"},{"type":"boolean"}],"items":false}}}}"""
    Assert.Equal<(string * string) list>([], validateExpr "[{a: [null, true]}]" schema)
    Assert.Equal<(string * string) list>([], validate """[{"a":[null,true]}]""" schema)
    Assert.Equal<(string * string) list>(validateExpr "[{a: [1, true, 3]}]" schema, validate """[{"a":[1,true,3]}]""" schema)
    Assert.Equal<(string * string) list>([ "/0/a/0", "type"; "/0/a/2", "items" ], validate """[{"a":[1,true,3]}]""" schema)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1238")>]
let ``JYRO-REQ-1238 ValidateSchema_ReadsAValueBuiltInTheScriptAsItsJson`` () =
    // a value made by script operations is read as the JSON ToJson gives for it
    let src = "var o = {}\no.k = [1, 'x']\no.k[0] = o.k[0] + 1\nOutput.r = [ToJson(o), ValidateSchema(o, Input.s)]"
    let v = Script.output (Script.runJson """{"s":{"const":{"k":[2,"x"]}}}""" src) |> Values.prop "r"
    Assert.Equal(norm """["{\"k\":[2,\"x\"]}",[]]""", Values.toJson v)

// =========================================================================
// 96.2.14 function values, 96.2.15 cycles
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1239")>]
[<InlineData("f", """{}""")>]
[<InlineData("[1, f]", """{}""")>]
[<InlineData("{a: {b: f}}", """{"type":"object"}""")>]
[<InlineData("{a: {b: f}}", """{"type":"string"}""")>]
[<InlineData("[f]", """{"items":false}""")>]
[<InlineData("{z: f}", """{"properties":{"a":{}}}""")>]
[<InlineData("f", """{"type":"bogus"}""")>]
let ``JYRO-REQ-1239 ValidateSchema_OfDataContainingFunction_RaisesToJsonsError`` (dataExpr: string, schema: string) =
    let toJson = Script.run (sprintf "var f = x => x\nOutput.r = ToJson(%s)" dataExpr)
    let res = validateExprRaw dataExpr schema
    Script.expectError MessageCode.InvalidType res
    Assert.Equal(Script.onlyError toJson, Script.onlyError res)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1240")>]
[<InlineData("ca", """{}""")>]
[<InlineData("co", """{"type":"object"}""")>]
[<InlineData("ci", """{}""")>]
[<InlineData("{x: 1, y: co}", """{"properties":{"x":{"type":"integer"}}}""")>]
[<InlineData("{x: 1, y: ca}", """{"type":"string"}""")>]
let ``ValidateSchema_OfCyclicData_RaisesValueNestingLimitExceeded`` (dataExpr: string, schema: string) =
    Script.expectError MessageCode.ValueNestingLimitExceeded (validateExprRaw dataExpr schema)

// =========================================================================
// 96.2.16 integer, 96.2.17 numbers by value, 96.2.18 lengths
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1241")>]
[<InlineData("5")>]
[<InlineData("5.0")>]
[<InlineData("5.00")>]
[<InlineData("-3")>]
[<InlineData("-3.0")>]
[<InlineData("0")>]
[<InlineData("0 * -1")>]
[<InlineData("1e20 * SquareRoot(1)")>]
[<InlineData("9007199254740993")>]
[<InlineData("Power(2, 53) * 1.0")>]
[<InlineData("FromJson('1.0')")>]
[<InlineData("FromJson('-0.0')")>]
[<InlineData("FromJson('1e2')")>]
[<InlineData("1e300 * 10")>]
[<InlineData("0.5 + 0.5")>]
[<InlineData("10 / 4 * 2")>]
let ``JYRO-REQ-1241 ValidateSchema_IntegerAcceptsEveryIntegerValue`` (expr: string) =
    Assert.Equal<(string * string) list>([], validateExpr expr """{"type":"integer"}""")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1241")>]
let ``JYRO-REQ-1241 ValidateSchema_IntegerAcceptsHostInexactWholeNumbers`` () =
    let input = Values.obj [ "a", Values.inexact 5.0; "b", Values.inexact 1e20; "c", Values.inexact -0.0; "s", Values.json """{"type":"integer"}""" ]
    Assert.Equal(norm "[[],[],[]]", Values.toJson (rVal input "Output.r = [ValidateSchema(Input.a, Input.s), ValidateSchema(Input.b, Input.s), ValidateSchema(Input.c, Input.s)]"))

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1241")>]
let ``JYRO-REQ-1241 ValidateSchema_IntegerAcceptsIntegerValuesInsideData`` () =
    expectErrors [ "/2", "type" ] "[1, 2.0, 2.5, 1e2]" """{"items":{"type":"integer"}}"""
    passes """{"a":3.0}""" """{"properties":{"a":{"type":["integer","string"]}}}"""

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1241")>]
[<InlineData("5.5")>]
[<InlineData("0.1")>]
[<InlineData("1e-7")>]
[<InlineData("SquareRoot(2)")>]
[<InlineData("'5'")>]
[<InlineData("true")>]
[<InlineData("null")>]
[<InlineData("[5]")>]
[<InlineData("5.0000000000000000000000000001")>]
let ``JYRO-REQ-1241 ValidateSchema_IntegerRejectsNonIntegersWithKeywordType`` (expr: string) =
    Assert.Equal<(string * string) list>([ "", "type" ], validateExpr expr """{"type":"integer"}""")

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1241")>]
let ``JYRO-REQ-1241 ValidateSchema_IntegerRejectsInexactJustAboveFive`` () =
    let input = Values.obj [ "a", Values.inexact 5.000000000000001; "s", Values.json """{"type":"integer"}""" ]
    Assert.Equal(norm """[{"path":"","keyword":"type","message":"type at (root)"}]""", Values.toJson (rVal input "Output.r = ValidateSchema(Input.a, Input.s)"))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1242")>]
[<InlineData("0.3", """{"multipleOf":0.1}""", true)>]
[<InlineData("0.1 + 0.2", """{"maximum":0.3}""", true)>]
[<InlineData("0.1 + 0.2", """{"minimum":0.3}""", true)>]
[<InlineData("0.3", """{"exclusiveMaximum":0.3}""", false)>]
[<InlineData("0.1 + 0.2", """{"enum":[0.3]}""", true)>]
[<InlineData("0.1 + 0.2", """{"const":0.3}""", true)>]
[<InlineData("1.0", """{"const":1}""", true)>]
[<InlineData("1", """{"const":1.00}""", true)>]
[<InlineData("[1.0, {a: 2.50}]", """{"const":[1,{"a":2.5}]}""", true)>]
[<InlineData("0", """{"minimum":1e-28}""", false)>]
[<InlineData("1e-28", """{"minimum":1e-28}""", true)>]
[<InlineData("1e-28", """{"exclusiveMinimum":1e-28}""", false)>]
[<InlineData("1234567890.123456789012345679", """{"maximum":1234567890.123456789012345678}""", false)>]
[<InlineData("1234567890.123456789012345677", """{"maximum":1234567890.123456789012345678}""", true)>]
[<InlineData("1234567890.123456789012345678", """{"const":1234567890.123456789012345678}""", true)>]
[<InlineData("1234567890.123456789012345679", """{"enum":[1234567890.123456789012345678]}""", false)>]
[<InlineData("1234567890.123456789012345679", """{"exclusiveMinimum":1234567890.123456789012345678}""", true)>]
[<InlineData("SquareRoot(2)", """{"maximum":1.4142135623730951}""", true)>]
[<InlineData("SquareRoot(2)", """{"minimum":1.4142135623730951}""", true)>]
[<InlineData("SquareRoot(2)", """{"exclusiveMaximum":1.4142135623730951}""", false)>]
[<InlineData("SquareRoot(2)", """{"exclusiveMinimum":1.4142135623730951}""", false)>]
[<InlineData("SquareRoot(2)", """{"const":1.4142135623730951}""", true)>]
[<InlineData("SquareRoot(2)", """{"enum":[1.4142135623730950]}""", false)>]
[<InlineData("0.7", """{"multipleOf":0.1}""", true)>]
[<InlineData("0.75", """{"multipleOf":0.1}""", false)>]
[<InlineData("1.1", """{"multipleOf":0.01}""", true)>]
[<InlineData("10", """{"multipleOf":2.5}""", true)>]
let ``JYRO-REQ-1242 ValidateSchema_ComparesNumbersByValue`` (expr: string, schema: string, conforms: bool) =
    let errs = validateExpr expr schema
    Assert.True((conforms = List.isEmpty errs), sprintf "%s against %s gave %A" expr schema errs)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1243")>]
[<InlineData("'\\uD83D\\uDE00'", """{"minLength":2}""", true)>]
[<InlineData("'\\uD83D\\uDE00'", """{"maxLength":1}""", false)>]
[<InlineData("'e\\u0301'", """{"maxLength":1}""", false)>]
[<InlineData("'\\u00E9'", """{"maxLength":1}""", true)>]
[<InlineData("'a\\uD83D\\uDE00b\\u00E9'", """{"minLength":5,"maxLength":5}""", true)>]
[<InlineData("'a\\uD83D\\uDE00b\\u00E9'", """{"minLength":6}""", false)>]
[<InlineData("'a\\uD83D\\uDE00b\\u00E9'", """{"maxLength":4}""", false)>]
let ``ValidateSchema_LengthsCountUtf16CodeUnits`` (expr: string, schema: string, conforms: bool) =
    let errs = validateExpr expr schema
    Assert.True((conforms = List.isEmpty errs), sprintf "%s against %s gave %A" expr schema errs)

// =========================================================================
// 96.2.19 pattern syntax, 96.2.20 unanchored, 96.2.21 errors
// =========================================================================

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1244")>]
[<InlineData("\"\u0663\"", """{"pattern":"^\\d$"}""", false)>]
[<InlineData("\"3\"", """{"pattern":"^\\d$"}""", true)>]
[<InlineData("\"a\\n\"", """{"pattern":"^a$"}""", false)>]
[<InlineData("\"\u00e9\"", """{"pattern":"^\\w$"}""", false)>]
[<InlineData("\"_\"", """{"pattern":"^\\w$"}""", true)>]
[<InlineData("\"\\n\"", """{"pattern":"^.$"}""", false)>]
[<InlineData("\"a\"", """{"pattern":"^A$"}""", false)>]
[<InlineData("\"\u00e9\"", """{"pattern":"^\\s$"}""", false)>]
[<InlineData("\"\u00a0\"", """{"pattern":"^\\s$"}""", true)>]
[<InlineData("\"\uFF13\"", """{"pattern":"^\\d$"}""", false)>]
[<InlineData("\"\u0663\"", """{"pattern":"^\\D$"}""", true)>]
let ``JYRO-REQ-1244 ValidateSchema_PatternUsesEcmaScriptSyntaxWithNoFlags`` (data: string, schema: string, conforms: bool) =
    let errs = schemaResultB data schema
    Assert.Equal(conforms, List.isEmpty errs)

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1244")>]
[<InlineData("{\"\u0663\":1}", false)>]
[<InlineData("{\"3\":1}", true)>]
let ``JYRO-REQ-1244 ValidateSchema_PatternPropertiesUsesEcmaScriptSyntax`` (data: string, matched: bool) =
    // a key the pattern matches must be a string; 1 is not
    let errs = schemaResultB data """{"patternProperties":{"^\\d$":{"type":"string"}}}"""
    Assert.Equal(matched, not (List.isEmpty errs))

/// The book's two examples of 96.2.19.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1244")>]
let ``JYRO-REQ-1244 Schema_TheBookExamples`` () =
    // ValidateSchema("Aa", {pattern: "\\Aa"}) MUST raise RegexInvalidPattern (90.10.5)
    let r = validateRawB "\"Aa\"" """{"pattern":"\\Aa"}"""
    expectRuntimeError MessageCode.RegexInvalidPattern r
    match Script.value r with
    | Some (:? JyroObject as o) -> Assert.False((o :> Collections.Generic.IReadOnlyDictionary<string, JyroValue>).ContainsKey "after", "the script went on after the rejected pattern")
    | _ -> ()
    // ValidateSchema("\u0663", {pattern: "^\\d$"}) MUST fail pattern (91.14.3), with one element (96.2.22 to 96.2.26)
    let v = Script.output (validateRawB "\"\u0663\"" """{"pattern":"^\\d$"}""") |> Values.prop "r"
    Assert.Equal(norm """[{"path":"","keyword":"pattern","message":"pattern at (root)"}]""", Values.toJson v)

/// 90.10.5 through 96.2.19: every Annex B form the book names is refused in
/// `pattern` and in `patternProperties`.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1244")>]
[<InlineData(@"\Aa")>]
[<InlineData(@"a\Z")>]
[<InlineData(@"\q")>]
[<InlineData("a]")>]
[<InlineData("a{")>]
[<InlineData(@"\8")>]
[<InlineData(@"\01")>]
let ``JYRO-REQ-1244 Schema_AnnexBSyntax_RaisesRegexInvalidPattern`` (pattern: string) =
    let p = Values.toJson (Values.str pattern)
    expectRuntimeError MessageCode.RegexInvalidPattern (validateRawB "\"Aa\"" (sprintf """{"pattern":%s}""" p))
    expectRuntimeError MessageCode.RegexInvalidPattern (validateRawB """{"Aa":1}""" (sprintf """{"patternProperties":{%s:{}}}""" p))

/// 90.10.1 with 10.6.2 through 96.2.19: the 15th edition.
[<Theory>]
[<Trait("requirement", "JYRO-REQ-1244")>]
[<InlineData("(?i:a)")>]
[<InlineData("(?-i:a)")>]
[<InlineData("(?<n>a)|(?<n>b)")>]
let ``JYRO-REQ-1244 Schema_ReadsThe2024Edition_Rejects`` (pattern: string) =
    let p = Values.toJson (Values.str pattern)
    expectRuntimeError MessageCode.RegexInvalidPattern (validateRawB "\"a\"" (sprintf """{"pattern":%s}""" p))
    expectRuntimeError MessageCode.RegexInvalidPattern (validateRawB """{"a":1}""" (sprintf """{"patternProperties":{%s:{}}}""" p))

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1244")>]
[<InlineData("\"ab\"", "(?<=a)b", true)>]
[<InlineData("\"cb\"", "(?<=a)b", false)>]
[<InlineData("\"a\"", "(?<n>a)", true)>]
[<InlineData("\"A\"", "a", false)>]
[<InlineData("\"a\\nb\"", "^b", false)>]
[<InlineData("\"\u212a\"", "^\\w$", false)>]
[<InlineData("\"\u3000\"", "^\\s$", true)>]
[<InlineData("\"\u180e\"", "\\s", false)>]
let ``JYRO-REQ-1244 Schema_ReadsThe2024Edition_Accepts`` (data: string, pattern: string, conforms: bool) =
    let errs = schemaResultB data (sprintf """{"pattern":%s}""" (Values.toJson (Values.str pattern)))
    if conforms then Assert.Empty(errs)
    else Assert.Equal<(string * string * string) list>([ "", "pattern", "pattern at (root)" ], errs)

/// A patternProperties subschema applies only to the keys its pattern matches
/// under the same rules (96.2.19); its failures are reported with their own
/// keyword and path (96.2.38).
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1244")>]
let ``JYRO-REQ-1244 SchemaPatternProperties_AppliesOnlyToMatchingKeys`` () =
    let schema = """{"patternProperties":{"^\\d$":{"type":"string"}}}"""
    Assert.Empty(schemaResultB "{\"\u0663\":1}" schema)
    Assert.Equal<(string * string * string) list>([ "/3", "type", "type at /3" ], schemaResultB """{"3":1}""" schema)
    Assert.Empty(schemaResultB """{"3a":1,"a3":1}""" schema)
    let caseSchema = """{"patternProperties":{"^a$":{"type":"string"}}}"""
    Assert.Empty(schemaResultB """{"A":1}""" caseSchema)
    Assert.Equal<(string * string * string) list>([ "/a", "type", "type at /a" ], schemaResultB """{"a":1,"A":1}""" caseSchema)

/// A pattern in a subschema held by a property named after a keyword whose
/// value is data (default, enum, const, examples) is a pattern of a schema,
/// read under the same rules (JYRO-DEF-580); the same pattern as the value of
/// default itself is data, and is never read.
[<Fact>]
[<Trait("requirement", "JYRO-REQ-1244")>]
let ``JYRO-REQ-1244 Schema_APatternUnderAPropertyNamedDefaultIsAPattern`` () =
    raisesB MessageCode.RegexInvalidPattern """{"default":"a"}""" """{"properties":{"default":{"pattern":"(?i:a)"}}}"""
    expectErrorsB [ "/enum", "pattern" ] "{\"enum\":\"\u0663\"}" """{"properties":{"enum":{"pattern":"^\\d$"}}}"""
    expectErrorsB [] """{"enum":"3"}""" """{"properties":{"enum":{"pattern":"^\\d$"}}}"""
    expectErrorsB [ "/const/examples", "pattern" ] """{"const":{"examples":"x"}}""" """{"properties":{"const":{"properties":{"examples":{"pattern":"^a"}}}}}"""
    expectErrorsB [] "\"A\"" """{"default":{"pattern":"(?i:a)"},"examples":[{"pattern":"\\Aa"}]}"""

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1245")>]
[<InlineData("\"abc\"", "b", true)>]
[<InlineData("\"abc\"", "^b", false)>]
[<InlineData("\"abc\"", "c$", true)>]
[<InlineData("\"xabc\"", "^abc$", false)>]
[<InlineData("\"anything\"", "", true)>]
[<InlineData("\"\"", "", true)>]
let ``ValidateSchema_PatternPassesWhenItMatchesAnywhere`` (data: string, pattern: string, conforms: bool) =
    let schema = sprintf """{"pattern":%s}""" (Values.toJson (Values.str pattern))
    if conforms then passes data schema else rejects data schema

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1245")>]
let ``ValidateSchema_PatternPropertiesMatchesKeyAnywhere`` () =
    rejects """{"abc":1}""" """{"patternProperties":{"b":{"type":"string"}}}"""
    passes """{"xyz":1}""" """{"patternProperties":{"b":{"type":"string"}}}"""

let badPatterns : obj[] seq =
    [ "["; "("; "a**"; "(?i)a"; "(?<n>a)\\k<m>"; "a{2,1}"; "(?<1>a)"; "[b-a]" ] |> Seq.map (fun p -> [| box p |])

[<Theory>]
[<Trait("requirement", "JYRO-REQ-1246")>]
[<MemberData(nameof badPatterns)>]
let ``JYRO-REQ-1246 ValidateSchema_OfInvalidPattern_RaisesRegexInvalidPattern`` (pattern: string) =
    let p = Values.toJson (Values.str pattern)
    raisesB MessageCode.RegexInvalidPattern "\"a\"" (sprintf """{"pattern":%s}""" p)
    raisesB MessageCode.RegexInvalidPattern "5" (sprintf """{"pattern":%s}""" p)
    raisesB MessageCode.RegexInvalidPattern """{"a":1}""" (sprintf """{"patternProperties":{%s:{}}}""" p)
    raisesB MessageCode.RegexInvalidPattern "5" (sprintf """{"patternProperties":{%s:{}}}""" p)
    raisesB MessageCode.RegexInvalidPattern """{"a":"a"}""" (sprintf """{"properties":{"a":{"pattern":%s}}}""" p)

[<Fact>]
[<Trait("requirement", "JYRO-REQ-1246")>]
let ``JYRO-REQ-1246 ValidateSchema_WhenPatternEvaluationRunsPastTheTimeout_RaisesRegexTimeoutPromptly`` () =
    let data = Values.toJson (Values.str (String('a', 40) + "!"))
    // Compiled first, and timed over Execute alone (floors-process 4.1.14),
    // with explicit limits and a regex timeout of 1 s (4.1.11).
    let program = Script.compile "Output.r = ValidateSchema(Input.d, Input.s)" |> Script.valueOf
    let runner =
        Pending.withRegexTimeout
            (JyroRunner(program).WithInputJson(sprintf "{\"d\": %s, \"s\": %s}" data """{"pattern":"^(a|aa)+\\1$"}""").WithExecutionOptions(specLimits))
            (TimeSpan.FromSeconds 1.)
    let sw = Stopwatch.StartNew()
    let res = runner.Execute()
    sw.Stop()
    Assert.True(sw.Elapsed < TimeSpan.FromSeconds 4., sprintf "the call took %O with a 1 s regex timeout" sw.Elapsed)
    if Script.ok res then
        // the engine finished first: the result must be correct (no match, so one error)
        Assert.Equal<(string * string) list>([ "", "pattern" ], Script.output res |> Values.prop "r" |> schemaErrors |> List.map (fun e -> e.Path, e.Keyword))
    else
        Script.expectError MessageCode.RegexTimeout res

/// The function inventory of 90.2.1 to 90.2.8, written out from the book, with a
/// valid sample argument (a Jyro expression) for every parameter.
module RealTree.Jyro.Spec.Tests.Inventory

/// One listed function: its 90.2 category (the clause), its name, and each
/// parameter as (name, optional, sample argument).
type Listed =
    { Clause: string
      Name: string
      Params: (string * bool * string) list }

    member this.Required = this.Params |> List.filter (fun (_, o, _) -> not o)
    member this.MinArgs = this.Required.Length
    member this.MaxArgs = this.Params.Length
    /// A call with the first `n` sample arguments, by position.
    member this.CallWith(n: int) =
        let args = this.Params |> List.truncate n |> List.map (fun (_, _, s) -> s)
        sprintf "%s(%s)" this.Name (String.concat ", " args)
    /// A call with every argument, by position.
    member this.FullCall = this.CallWith this.MaxArgs
    /// A call with the required arguments only, by position.
    member this.RequiredCall = this.CallWith this.MinArgs
    override this.ToString() = this.Name

let private r name sample = (name, false, sample)
let private o name sample = (name, true, sample)
let private f clause name ps = { Clause = clause; Name = name; Params = ps }

let all : Listed list =
    [ // 90.2.1 string functions (18)
      f "90.2.1" "ToUpper" [ r "text" "\"a\"" ]
      f "90.2.1" "ToLower" [ r "text" "\"A\"" ]
      f "90.2.1" "Trim" [ r "text" "\" a \"" ]
      f "90.2.1" "Replace" [ r "text" "\"abc\""; r "oldValue" "\"b\""; r "newValue" "\"x\"" ]
      f "90.2.1" "Contains" [ r "source" "\"abc\""; r "value" "\"b\"" ]
      f "90.2.1" "StartsWith" [ r "text" "\"abc\""; r "prefix" "\"a\"" ]
      f "90.2.1" "EndsWith" [ r "text" "\"abc\""; r "suffix" "\"c\"" ]
      f "90.2.1" "Split" [ r "text" "\"a,b\""; r "separator" "\",\"" ]
      f "90.2.1" "Join" [ r "values" "[\"a\", \"b\"]"; r "separator" "\",\"" ]
      f "90.2.1" "Substring" [ r "text" "\"abcdef\""; r "start" "1"; o "length" "2" ]
      f "90.2.1" "ToNumber" [ r "text" "\"1\"" ]
      f "90.2.1" "PadLeft" [ r "text" "\"a\""; r "length" "3"; o "padding" "\"*\"" ]
      f "90.2.1" "PadRight" [ r "text" "\"a\""; r "length" "3"; o "padding" "\"*\"" ]
      f "90.2.1" "RegexTest" [ r "text" "\"abc\""; r "pattern" "\"b\"" ]
      f "90.2.1" "RegexMatch" [ r "text" "\"abc\""; r "pattern" "\"b\"" ]
      f "90.2.1" "RegexMatchAll" [ r "text" "\"abc\""; r "pattern" "\"b\"" ]
      f "90.2.1" "RegexMatchDetail" [ r "text" "\"abc\""; r "pattern" "\"b\"" ]
      f "90.2.1" "RandomString" [ r "length" "5"; o "characterSet" "\"ab\"" ]
      // 90.2.2 array functions (23)
      f "90.2.2" "Length" [ r "source" "[1, 2]" ]
      f "90.2.2" "Append" [ r "items" "[1]"; r "value" "2" ]
      f "90.2.2" "Prepend" [ r "items" "[1]"; r "value" "0" ]
      f "90.2.2" "First" [ r "items" "[1, 2]" ]
      f "90.2.2" "Last" [ r "items" "[1, 2]" ]
      f "90.2.2" "IndexOf" [ r "source" "[1, 2]"; r "value" "2" ]
      f "90.2.2" "Reverse" [ r "items" "[1, 2]" ]
      f "90.2.2" "Slice" [ r "items" "[1, 2, 3]"; r "start" "1"; o "finish" "2" ]
      f "90.2.2" "Concatenate" [ r "first" "[1]"; r "second" "[2]" ]
      f "90.2.2" "Distinct" [ r "items" "[1, 1]" ]
      f "90.2.2" "Sort" [ r "items" "[2, 1]" ]
      f "90.2.2" "Flatten" [ r "items" "[[1], [2]]" ]
      f "90.2.2" "Range" [ r "start" "0"; r "finish" "3"; o "step" "1" ]
      f "90.2.2" "Skip" [ r "items" "[1, 2]"; r "count" "1" ]
      f "90.2.2" "Insert" [ r "items" "[1]"; r "index" "0"; r "value" "0" ]
      f "90.2.2" "RemoveAt" [ r "items" "[1, 2]"; r "index" "0" ]
      f "90.2.2" "RemoveFirst" [ r "items" "[1, 2]" ]
      f "90.2.2" "RemoveLast" [ r "items" "[1, 2]" ]
      f "90.2.2" "FlattenOnce" [ r "items" "[[1], [2]]" ]
      f "90.2.2" "RandomChoice" [ r "items" "[1, 2]" ]
      f "90.2.2" "SortByField" [ r "items" "[{a: 2}, {a: 1}]"; r "fieldName" "\"a\""; r "direction" "\"asc\"" ]
      f "90.2.2" "GroupBy" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\"" ]
      f "90.2.2" "SelectMany" [ r "items" "[{a: [1]}]"; r "fieldName" "\"a\"" ]
      // 90.2.3 math functions (15)
      f "90.2.3" "Absolute" [ r "value" "-1" ]
      f "90.2.3" "Floor" [ r "value" "1.5" ]
      f "90.2.3" "Ceiling" [ r "value" "1.5" ]
      f "90.2.3" "Round" [ r "value" "1.25"; r "decimals" "1"; o "mode" "\"halfEven\"" ]
      f "90.2.3" "Min" [ r "values" "[1, 2]" ]
      f "90.2.3" "Max" [ r "values" "[1, 2]" ]
      f "90.2.3" "Power" [ r "value" "2"; r "exponent" "3" ]
      f "90.2.3" "SquareRoot" [ r "value" "4" ]
      f "90.2.3" "Log" [ r "value" "8"; o "base" "2" ]
      f "90.2.3" "Sum" [ r "values" "[1, 2]" ]
      f "90.2.3" "Average" [ r "values" "[1, 2]" ]
      f "90.2.3" "Clamp" [ r "value" "5"; r "min" "0"; r "max" "3" ]
      f "90.2.3" "Median" [ r "values" "[1, 2, 3]" ]
      f "90.2.3" "Mode" [ r "values" "[1, 1, 2]" ]
      f "90.2.3" "RandomInt" [ r "min" "0"; r "max" "3" ]
      // 90.2.4 date and time functions (7)
      f "90.2.4" "Now" []
      f "90.2.4" "Today" []
      f "90.2.4" "ParseDate" [ r "text" "\"2024-01-02\"" ]
      f "90.2.4" "FormatDate" [ r "date" "\"2024-01-02\""; r "format" "\"yyyy\"" ]
      f "90.2.4" "DateAdd" [ r "date" "\"2024-01-02\""; r "amount" "1"; r "unit" "\"day\"" ]
      f "90.2.4" "DateDiff" [ r "startDate" "\"2024-01-02\""; r "endDate" "\"2024-01-03\""; r "unit" "\"day\"" ]
      f "90.2.4" "DatePart" [ r "date" "\"2024-01-02\""; r "part" "\"year\"" ]
      // 90.2.5 utility functions (16)
      f "90.2.5" "TypeOf" [ r "value" "1" ]
      f "90.2.5" "Coalesce" [ r "values" "[null, 1]" ]
      f "90.2.5" "ToString" [ r "value" "1" ]
      f "90.2.5" "ToBoolean" [ r "value" "1" ]
      f "90.2.5" "Keys" [ r "source" "{a: 1}" ]
      f "90.2.5" "Values" [ r "source" "{a: 1}" ]
      f "90.2.5" "HasProperty" [ r "source" "{a: 1}"; r "name" "\"a\"" ]
      f "90.2.5" "Merge" [ r "values" "[{a: 1}, {b: 2}]" ]
      f "90.2.5" "Clone" [ r "value" "{a: 1}" ]
      f "90.2.5" "NewGuid" []
      f "90.2.5" "Base64Encode" [ r "text" "\"a\"" ]
      f "90.2.5" "Base64Decode" [ r "text" "\"YQ==\"" ]
      f "90.2.5" "FromJson" [ r "text" "\"1\"" ]
      f "90.2.5" "ToJson" [ r "value" "1" ]
      f "90.2.5" "Sleep" [ r "milliseconds" "0" ]
      f "90.2.5" "Diff" [ r "first" "{a: 1}"; r "second" "{a: 2}" ]
      // 90.2.6 schema functions (2)
      f "90.2.6" "ValidateRequired" [ r "source" "{a: 1}"; r "fields" "[\"a\"]" ]
      f "90.2.6" "ValidateSchema" [ r "data" "{a: 1}"; r "schema" "{type: \"object\"}" ]
      // 90.2.7 query functions (8)
      f "90.2.7" "AnyByField" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\""; r "operator" "\"==\""; r "value" "1" ]
      f "90.2.7" "AllByField" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\""; r "operator" "\"==\""; r "value" "1" ]
      f "90.2.7" "FindByField" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\""; r "operator" "\"==\""; r "value" "1" ]
      f "90.2.7" "WhereByField" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\""; r "operator" "\"==\""; r "value" "1" ]
      f "90.2.7" "Select" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\"" ]
      f "90.2.7" "Project" [ r "items" "[{a: 1, b: 2}]"; r "fields" "[\"a\"]" ]
      f "90.2.7" "Omit" [ r "items" "[{a: 1, b: 2}]"; r "fields" "[\"a\"]" ]
      f "90.2.7" "CountIf" [ r "items" "[{a: 1}]"; r "fieldName" "\"a\""; r "operator" "\"==\""; r "value" "1" ]
      // 90.2.8 lambda functions (8)
      f "90.2.8" "Map" [ r "items" "[1, 2]"; r "callback" "x => x * 2" ]
      f "90.2.8" "Where" [ r "items" "[1, 2]"; r "callback" "x => x > 1" ]
      f "90.2.8" "Reduce" [ r "items" "[1, 2]"; r "callback" "(acc, x) => acc + x"; r "initial" "0" ]
      f "90.2.8" "Each" [ r "items" "[1, 2]"; r "callback" "x => x" ]
      f "90.2.8" "Find" [ r "items" "[1, 2]"; r "callback" "x => x > 1" ]
      f "90.2.8" "All" [ r "items" "[1, 2]"; r "callback" "x => x > 0" ]
      f "90.2.8" "Any" [ r "items" "[1, 2]"; r "callback" "x => x > 1" ]
      f "90.2.8" "SortBy" [ r "items" "[2, 1]"; r "callback" "x => x" ] ]

let byName (name: string) : Listed = all |> List.find (fun l -> l.Name = name)

/// xUnit member data: one row per listed function name.
let names () : obj[] seq = all |> Seq.map (fun l -> [| box l.Name |])

/// xUnit member data: one row per listed function name of one clause.
let namesOf (clause: string) : obj[] seq =
    all |> Seq.filter (fun l -> l.Clause = clause) |> Seq.map (fun l -> [| box l.Name |])

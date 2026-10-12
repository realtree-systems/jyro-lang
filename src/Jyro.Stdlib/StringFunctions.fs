namespace RealTree.Jyro

open System
open System.Security.Cryptography


module internal StringFunctions =

    /// <summary>Converts text to upper case.</summary>
    /// <arg name="text">The text to convert. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <returns>The text with each letter in upper case, one character for one character, by the simple case mappings of Unicode version 15. Never null.</returns>
    /// <example><c>ToUpper("ok")</c> returns <c>"OK"</c>.</example>
    type ToUpperFunction() =
        inherit JyroFunctionBase("ToUpper", Signatures.unary "text" ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            JyroString(Bounds.changeCase (Bounds.Ticker ctx) true (args.GetString(0))) :> JyroValue

    /// <summary>Converts text to lower case.</summary>
    /// <arg name="text">The text to convert. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <returns>The text with each letter in lower case, one character for one character, by the simple case mappings of Unicode version 15. Never null.</returns>
    /// <example><c>ToLower("Hello")</c> returns <c>"hello"</c>.</example>
    type ToLowerFunction() =
        inherit JyroFunctionBase("ToLower", Signatures.unary "text" ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            JyroString(Bounds.changeCase (Bounds.Ticker ctx) false (args.GetString(0))) :> JyroValue

    /// <summary>Removes white space from both ends of text.</summary>
    /// <arg name="text">The text to trim. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <returns>The text without the white space at its start and its end; white space inside it is kept. White space is every character that Unicode version 15 gives the White_Space property. Never null.</returns>
    /// <example><c>Trim("  a b  ")</c> returns <c>"a b"</c>.</example>
    type TrimFunction() =
        inherit JyroFunctionBase("Trim", Signatures.unary "text" ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let text = args.GetString(0)
            let ticker = Bounds.Ticker ctx
            // The code units of White_Space of Unicode 15.0.0 (spec 10.6.1,
            // 91.3.2, JYRO-ADR-143), every one of which is in the Basic
            // Multilingual Plane, found a code unit at a time with a count
            // per code unit rather than in one call.
            let mutable first = 0
            while first < text.Length && Unicode15.isWhiteSpace (int text.[first]) do
                ticker.Tick()
                first <- first + 1
            let mutable last = text.Length - 1
            while last >= first && Unicode15.isWhiteSpace (int text.[last]) do
                ticker.Tick()
                last <- last - 1
            JyroString(Bounds.slice ticker text first (last - first + 1)) :> JyroValue

    /// <summary>Replaces every occurrence of one piece of text with another.</summary>
    /// <arg name="text">The text to search. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="oldValue">The text to find. Matching is exact: case and every character count. The empty string is a runtime error, and so is null, which is read as the empty string.</arg>
    /// <arg name="newValue">The text to put in place of each occurrence. Null is read as the empty string, which removes each occurrence.</arg>
    /// <returns>The text with each occurrence of oldValue, found from left to right without overlapping, replaced by newValue; the text unchanged when oldValue does not occur in it. Never null.</returns>
    /// <example><c>Replace("a-b-c", "-", "+")</c> returns <c>"a+b+c"</c>.</example>
    type ReplaceFunction() =
        inherit JyroFunctionBase("Replace",
            Signatures.create
                [ Parameter.Required("text", ParameterType.String)
                  Parameter.Required("oldValue", ParameterType.String)
                  Parameter.Required("newValue", ParameterType.String) ]
                ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let oldValue = args.GetString(1)
            let newValue = args.GetString(2)
            if oldValue.Length = 0 then
                // Unchanged: String.Replace refuses an empty oldValue.
                JyroString(str.Replace(oldValue, newValue)) :> JyroValue
            else
                let ticker = Bounds.Ticker ctx
                // Counted first, so the length of the result is known and a
                // result above the maximum value size is refused before it is
                // built (spec 100.8.7, 100.8.8). A result equal to `text` is
                // `text`, which the bound exempts.
                let mutable count = 0L
                let mutable at = Bounds.indexOfOrdinal ticker str oldValue 0
                while at >= 0 do
                    count <- count + 1L
                    at <- Bounds.indexOfOrdinal ticker str oldValue (at + oldValue.Length)
                if count = 0L || String.Equals(oldValue, newValue, StringComparison.Ordinal) then
                    JyroString(str) :> JyroValue
                else
                    let length = int64 str.Length + count * (int64 newValue.Length - int64 oldValue.Length)
                    Bounds.requireSize length
                    let result = Text.StringBuilder(int length)
                    let appendRange (start: int) (finish: int) =
                        let mutable i = start
                        while i < finish do
                            ticker.Tick()
                            let n = min ExecutionCheck.ChunkLength (finish - i)
                            result.Append(str, i, n) |> ignore
                            i <- i + n
                    let mutable position = 0
                    let mutable next = Bounds.indexOfOrdinal ticker str oldValue 0
                    while next >= 0 do
                        appendRange position next
                        Bounds.appendChunked ticker result newValue
                        position <- next + oldValue.Length
                        next <- Bounds.indexOfOrdinal ticker str oldValue position
                    appendRange position str.Length
                    JyroString(Bounds.ofBuilder ticker result) :> JyroValue

    /// <summary>Tells whether a string holds a piece of text, or an array holds a value.</summary>
    /// <arg name="source">The string or array to search. Null holds nothing. A number, a boolean, an object or a function is a runtime error.</arg>
    /// <arg name="value">What to look for. In an array, any value, null included, found when an element is equal to it by value. In a string, text found by exact match, case included; a number or a boolean is read as its string form, null is found in no string, and an array, an object or a function is a runtime error.</arg>
    /// <returns>True when source holds value, false otherwise. Never null.</returns>
    /// <example><c>Contains("haystack", "st")</c> returns true; <c>Contains([1, 2, 3], 2)</c> returns true.</example>
    type ContainsFunction() =
        inherit JyroFunctionBase("Contains",
            Signatures.create
                [ Parameter.Required("source", ParameterType.Any)
                  Parameter.Required("value", ParameterType.Any) ]
                ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let source = args.[0]
            let search = args.[1]
            let ticker = Bounds.Ticker ctx
            // Array membership scans by value equality, which is reflexive on
            // null (Type System 30.10), so a null element is found like any
            // other value, and any value may be searched for.
            match source with
            | :? JyroArray as arr ->
                let mutable found = false
                let mutable i = 0
                let items = arr.Items
                while not found && i < items.Count do
                    ticker.Tick()
                    if items.[i].EqualsValue(search) then
                        found <- true
                    i <- i + 1
                JyroBoolean.FromBoolean(found) :> JyroValue
            // A string source: a number or boolean `value` is read as its
            // string form (91.5.8, 40.4), a container or a function is
            // ArgumentTypeMismatch (91.5.9), and a null `value` is found in
            // no string.
            | :? JyroString as sourceStr ->
                if search.IsNull then JyroBoolean.False :> JyroValue
                else
                    let searchText = args.GetString(1)
                    JyroBoolean.FromBoolean(Bounds.indexOfOrdinal ticker sourceStr.Value searchText 0 >= 0) :> JyroValue
            | _ when source.IsNull ->
                JyroBoolean.False :> JyroValue
            // Any other source - a number, a boolean, an object or a function -
            // has no members to search, whatever `value` is (91.5.10).
            | _ ->
                JyroError.raiseRuntime MessageCode.StringOrArrayRequired [| box "Contains()" |]

    /// <summary>Tells whether text begins with a given prefix.</summary>
    /// <arg name="text">The text to test. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="prefix">The prefix to look for, matched exactly, case included. Every text begins with the empty string.</arg>
    /// <returns>True when text begins with prefix, false otherwise. Never null.</returns>
    /// <example><c>StartsWith("filename.txt", "file")</c> returns true.</example>
    type StartsWithFunction() =
        inherit JyroFunctionBase("StartsWith", Signatures.binary "text" ParameterType.String "prefix"ParameterType.String ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let str = args.GetString(0)
            let prefix = args.GetString(1)
            // Ordinal (91.6.2), compared a chunk at a time with a check
            // between chunks (spec 100.2.5). String.StartsWith(string) compared
            // by the host's culture, in one call over the whole prefix.
            JyroBoolean.FromBoolean(prefix.Length <= str.Length && BoundedText.regionEquals str 0 prefix) :> JyroValue

    /// <summary>Tells whether text ends with a given suffix.</summary>
    /// <arg name="text">The text to test. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="suffix">The suffix to look for, matched exactly, case included. Every text ends with the empty string.</arg>
    /// <returns>True when text ends with suffix, false otherwise. Never null.</returns>
    /// <example><c>EndsWith("filename.txt", ".txt")</c> returns true.</example>
    type EndsWithFunction() =
        inherit JyroFunctionBase("EndsWith", Signatures.binary "text" ParameterType.String "suffix"ParameterType.String ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let str = args.GetString(0)
            let suffix = args.GetString(1)
            // Ordinal (91.7.2), a chunk at a time, as StartsWith.
            JyroBoolean.FromBoolean(
                suffix.Length <= str.Length && BoundedText.regionEquals str (str.Length - suffix.Length) suffix) :> JyroValue

    /// <summary>Splits text into an array of parts at each occurrence of a separator.</summary>
    /// <arg name="text">The text to split. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="separator">The text that separates the parts, matched exactly, case included. An empty separator, or null, is a runtime error.</arg>
    /// <returns>An array of the strings between the separators, in order. Separators next to each other, or at the start or the end of text, give empty strings, so the array has one more element than there are separators; text with no separator gives an array of text alone. Never null.</returns>
    /// <example><c>Split("a,b,,c", ",")</c> returns <c>["a", "b", "", "c"]</c>.</example>
    type SplitFunction() =
        inherit JyroFunctionBase("Split", Signatures.binary "text" ParameterType.String "separator"ParameterType.String ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let delimiter = args.GetString(1)
            // An empty separator, `null` read as one (91.8.8), occurs at every
            // position and is refused (91.8.7), as `Replace` refuses an empty
            // `oldValue` (91.4.7): with the same diagnostic, RuntimeError
            // naming the function.
            if delimiter.Length = 0 then
                JyroError.raiseRuntime MessageCode.RuntimeError [| box "Split()" |]
            let ticker = Bounds.Ticker ctx
            let arr = JyroArray()
            // Each separator is found by ordinal comparison, one part at a
            // time. Each part is refused above the maximum value size unless
            // it is the whole of `text` (spec 100.8.7), and so is the part
            // that would take the array past it.
            let add (start: int) (finish: int) =
                ticker.Tick()
                arr.AddBounded(JyroString(Bounds.slice ticker str start (finish - start)))
            let mutable position = 0
            let mutable next = Bounds.indexOfOrdinal ticker str delimiter 0
            while next >= 0 do
                add position next
                position <- next + delimiter.Length
                next <- Bounds.indexOfOrdinal ticker str delimiter position
            add position str.Length
            arr :> JyroValue

    /// <summary>Joins the elements of an array into one string, with a separator between each two.</summary>
    /// <arg name="values">The array to join. Each element is written in its string form: a string as it is, a number or a boolean as its text, and null as the text null. An element that is an array, an object or a function is a runtime error. Null is read as the empty array.</arg>
    /// <arg name="separator">The text to put between each two elements. Null is read as the empty string.</arg>
    /// <returns>The elements in order with separator between each two; the empty string for an empty array. Never null.</returns>
    /// <example><c>Join(["a", 1, true], "-")</c> returns <c>"a-1-true"</c>.</example>
    type JoinFunction() =
        inherit JyroFunctionBase("Join", Signatures.binary "values" ParameterType.Array "separator"ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let arr = args.GetArray(0)
            let items = arr.Items
            let ticker = Bounds.Ticker ctx
            // An array, an object or a function has no string form (40.4.2),
            // so an element that is one is refused as `&` refuses it (91.9.6).
            // The check is one of `values` (90.3.9 row 5), so it comes before
            // any check of `separator` (90.3.8, JYRO-ADR-1), and before any
            // text is built: the refusal does not depend on how long the text
            // before it would have been.
            for item in items do
                ticker.Tick()
                match item.ValueType with
                | JyroValueType.Array | JyroValueType.Object | JyroValueType.Function ->
                    JyroError.raiseRuntime MessageCode.ConcatWithInvalidType
                        [| box (this.Name + "()"); box (TypeNames.typeOfName item.ValueType) |]
                | _ -> ()
            let separator = args.GetString(1)
            match items.Count with
            // One string is returned as it was given, which the bound
            // exempts whatever its length (spec 100.8.7).
            | 1 when (items.[0] :? JyroString) -> items.[0]
            | _ ->
                let text = Text.StringBuilder()
                // Each element in its string form, the same projection `&`
                // and `ToString` use. The text is refused before it passes the
                // maximum value size (spec 100.8.7).
                for i = 0 to items.Count - 1 do
                    ticker.Tick()
                    if i > 0 then
                        if int64 text.Length + int64 separator.Length > int64 ValueSize.Maximum then
                            ValueSize.Raise()
                        Bounds.appendChunked ticker text separator
                    ValueWalk.AppendText(text, items.[i], ValueSize.Maximum, ignore)
                JyroString(Bounds.ofBuilder ticker text) :> JyroValue

    /// <summary>Takes part of text, from a start position, of a given length or to the end.</summary>
    /// <arg name="text">The text to take part of. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="start">The position of the first character to take, counted from 0 in UTF-16 code units. A whole number from 0 to the length of text; the length itself gives the empty string. A larger number is a runtime error, as is a negative or fractional one.</arg>
    /// <arg name="length">The number of characters to take, in UTF-16 code units: a whole number of 0 or more, a negative or fractional one being a runtime error. A length that reaches past the end of text takes what is there. When it is left out, or null, the rest of text from start is taken.</arg>
    /// <returns>The part of text that begins at start. Never null.</returns>
    /// <example><c>Substring("abcdef", 2, 3)</c> returns <c>"cde"</c>; <c>Substring("abcdef", 4)</c> returns <c>"ef"</c>.</example>
    type SubstringFunction() =
        inherit JyroFunctionBase("Substring",
            Signatures.create
                [ Parameter.Required("text", ParameterType.String)
                  Parameter.Required("start", ParameterType.Number)
                  Parameter.Optional("length", ParameterType.Number) ]
                ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            // Each argument through its own checks, in parameter order, before
            // the position is compared with the text (spec 90.3.8, JYRO-ADR-1):
            // so Substring("ab", 5, -1) is NonNegativeIntegerRequired for
            // `length`, not IndexOutOfRange for `start`. A `length` of null is
            // a length not given (90.13.1) and is not checked.
            let text = args.GetString(0)
            let start = JyroArguments.ReadNonNegativeInt32(args, 1, "Substring()", "start")
            let requested =
                if Bounds.isAbsent args 2 then ValueNone
                else ValueSome (JyroArguments.ReadNonNegativeInt32(args, 2, "Substring()", "length"))
            // `start` names a position in `text`: from 0 to its length, the
            // length itself being the position after the last code unit. A
            // start beyond it is refused, never reduced (90.3.4, 90.3.5).
            if start > text.Length then
                JyroError.raiseRuntime MessageCode.IndexOutOfRange [| box "Substring()"; box start; box text.Length |]
            // Only the end, start + length, is reduced to the length of the
            // text (90.3.6, 91.10.4).
            let available = text.Length - start
            let length =
                match requested with
                | ValueSome len -> min len available
                | ValueNone -> available
            if length = 0 then
                JyroString(String.Empty) :> JyroValue
            else
                JyroString(Bounds.slice (Bounds.Ticker ctx) text start length) :> JyroValue

    /// <summary>Reads a number from text written as a JSON number.</summary>
    /// <arg name="text">The text to read: a string only. White space at either end, as JSON defines it (space, tab, line feed and carriage return), is ignored. Any other type, null included, is a runtime error.</arg>
    /// <returns>The number the text writes, or null when the text is not a JSON number, for example "1,234", "+5", ".5", "01", "Infinity" or the empty string, and when it writes a number too large to represent, such as "1e400".</returns>
    /// <example><c>ToNumber(" 12.5 ")</c> returns 12.5; <c>ToNumber("abc") ?? 0</c> returns 0.</example>
    type ToNumberFunction() =
        inherit JyroFunctionBase("ToNumber", Signatures.unary "text" ParameterType.String ParameterType.Any)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // A string only: null, a number, a boolean or a container is
            // ArgumentTypeMismatch, not text read as its string form (spec 90.4).
            let str = args.Get<JyroString>(0).Value
            // The text, trimmed of the four JSON white-space characters only,
            // read by the JSON number grammar and held as JSON holds the same
            // text (91.11.2 to 91.11.4, 30.12.2, 30.2.7, JYRO-ADR-17): the one
            // reading a `number` type hint uses. The platform's parsers took
            // `1,234`, `$5`, `(5)`, `+5`, `.5`, `01` and `Infinity`, and
            // rounded `1e-29` to an exact zero.
            //
            // Text that writes no number, a non-finite one included, is null
            // rather than a report (90.5): this is the parse family, and
            // `ToNumber(Input.raw) ?? 0` must keep working.
            match JyroValue.NumberOfText str with
            | null -> JyroNull.Instance :> JyroValue
            | n -> n

    // Cap on any string this module builds from a count rather than from an
    // input's size - the pad widths and RandomString's length. Shared so the
    // three cannot drift apart.
    let private maxGeneratedLength = 1_000_000

    // The pad character of `PadLeft` or `PadRight`, read after every check
    // of `text` and `length` (90.3.8): a space when `padding` is absent or
    // `null` (90.13.1), else the one UTF-16 code unit of its string, a
    // number or boolean read as its string form (91.12.11, 91.13.10). A
    // container or a function is ArgumentTypeMismatch; a string of any other
    // length, the empty string and a surrogate pair included, is
    // InvalidPadCharacter (91.12.5, 91.13.5).
    let private padCharacter (fn: JyroFunctionBase) (args: Collections.Generic.IReadOnlyList<JyroValue>) : char =
        if Bounds.isAbsent args 2 then ' '
        else
            let s = args.GetString(2)
            let functionName = fn.Name + "()"
            if s.Length <> 1 then
                JyroError.raiseRuntime MessageCode.InvalidPadCharacter [| box functionName; box s |]
            s.[0]

    // `text` padded with `padding` to `totalWidth` code units, on the left
    // or the right, as `String.PadLeft` and `String.PadRight` give it: `text`
    // itself when it is already that long. Written in place a chunk at a
    // time with a count per chunk, not by one library call over the whole
    // result (spec 100.2.5, JYRO-ADR-101).
    let private pad (ticker: Bounds.Ticker) (left: bool) (text: string) (totalWidth: int) (padChar: char) : string =
        if totalWidth <= text.Length then text
        else
            let padding = totalWidth - text.Length
            let textStart = if left then padding else 0
            Bounds.create ticker totalWidth (ChunkWriter(fun destination at ->
                let n = min ExecutionCheck.ChunkLength (totalWidth - at)
                // The chunk is either all padding or all text, so it ends
                // where the two meet.
                if at >= textStart && at < textStart + text.Length then
                    let n = min n (textStart + text.Length - at)
                    text.AsSpan(at - textStart, n).CopyTo(destination.Slice(at, n))
                    n
                else
                    let boundary = if at < textStart then textStart else totalWidth
                    let n = min n (boundary - at)
                    destination.Slice(at, n).Fill(padChar)
                    n))

    /// <summary>Pads text on the left to a given length.</summary>
    /// <arg name="text">The text to pad. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="length">The length the result must reach, in UTF-16 code units: a whole number from 0 to 1000000. A negative, fractional or larger number is a runtime error. Text that is already this long or longer is returned unchanged.</arg>
    /// <arg name="padding">The character to pad with: a string of exactly one UTF-16 code unit; any other length, the empty string included, is a runtime error. A number or a boolean is read as its string form. When it is left out, or null, a space is used.</arg>
    /// <returns>The text with padding added before it until it is length long, or the text unchanged when it is already that long. Never null.</returns>
    /// <example><c>PadLeft("42", 5, "0")</c> returns <c>"00042"</c>.</example>
    type PadLeftFunction() =
        inherit JyroFunctionBase("PadLeft",
            Signatures.create
                [ Parameter.Required("text", ParameterType.String)
                  Parameter.Required("length", ParameterType.Number)
                  Parameter.Optional("padding", ParameterType.String) ]
                ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let totalWidth = JyroArguments.ReadNonNegativeInt32(args, 1, "PadLeft()", "length")
            if totalWidth > maxGeneratedLength then
                JyroError.raiseRuntime MessageCode.GeneratedLengthExceeded [| box "PadLeft()"; box totalWidth; box maxGeneratedLength |]
            let padChar = padCharacter this args
            JyroString(pad (Bounds.Ticker ctx) true str totalWidth padChar) :> JyroValue

    /// <summary>Pads text on the right to a given length.</summary>
    /// <arg name="text">The text to pad. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="length">The length the result must reach, in UTF-16 code units: a whole number from 0 to 1000000. A negative, fractional or larger number is a runtime error. Text that is already this long or longer is returned unchanged.</arg>
    /// <arg name="padding">The character to pad with: a string of exactly one UTF-16 code unit; any other length, the empty string included, is a runtime error. A number or a boolean is read as its string form. When it is left out, or null, a space is used.</arg>
    /// <returns>The text with padding added after it until it is length long, or the text unchanged when it is already that long. Never null.</returns>
    /// <example><c>PadRight("ab", 4, ".")</c> returns <c>"ab.."</c>.</example>
    type PadRightFunction() =
        inherit JyroFunctionBase("PadRight",
            Signatures.create
                [ Parameter.Required("text", ParameterType.String)
                  Parameter.Required("length", ParameterType.Number)
                  Parameter.Optional("padding", ParameterType.String) ]
                ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let totalWidth = JyroArguments.ReadNonNegativeInt32(args, 1, "PadRight()", "length")
            if totalWidth > maxGeneratedLength then
                JyroError.raiseRuntime MessageCode.GeneratedLengthExceeded [| box "PadRight()"; box totalWidth; box maxGeneratedLength |]
            let padChar = padCharacter this args
            JyroString(pad (Bounds.Ticker ctx) false str totalWidth padChar) :> JyroValue

    // The regex timeout of the run (spec 90.10.3): the one the host set with
    // `JyroRunner.WithMaxRegexTime` or `JyroExecutionOptions.MaxRegexTime`, or 1 second (90.10.4). Internal rather
    // than private because `ValidateSchema` matches its patterns under the
    // same bound (SchemaFunctions.fs), so one limit governs every regular
    // expression a script can supply. A function a host calls directly with
    // no context gets the default.
    let internal regexTimeout (ctx: JyroExecutionContext) : TimeSpan =
        if isNull (box ctx) then TimeSpan.FromSeconds(1.0) else ctx.RegexTimeout

    // `pattern` compiled by Jyro's ECMAScript matcher (JYRO-ADR-142), or
    // `RegexInvalidPattern` naming `functionName`. The pattern is the
    // argument, not a message about it, which would be a sentence (spec
    // 130.6.2). Internal because `ValidateSchema` reads its patterns the
    // same way (96.2.19, 96.2.21). The pattern is data from the script, so
    // its compile checks the run's time limit and cancellation through
    // `ticker` as it goes (spec 100.2.5).
    let internal compilePattern (functionName: string) (ticker: Bounds.Ticker) (pattern: string) : EcmaRegex.Pattern =
        try EcmaRegex.compileChecked pattern ticker.CheckNow
        with :? EcmaRegex.InvalidPatternException ->
            JyroError.raiseRuntime MessageCode.RegexInvalidPattern [| box functionName; box pattern |]

    // The first match of `regex` in `text` at `from` or after, under the
    // regex timeout of the run (90.10.3), checking the run's own limits and
    // cancellation as it goes (spec 100.2.5); `RegexTimeout` naming
    // `functionName` when it runs longer.
    let internal execPattern (functionName: string) (ctx: JyroExecutionContext) (ticker: Bounds.Ticker)
                             (regex: EcmaRegex.Pattern) (text: string) (from: int) : int[] =
        try regex.Exec(text, from, regexTimeout ctx, ticker.CheckNow)
        with :? EcmaRegex.MatchTimeoutException ->
            JyroError.raiseRuntime MessageCode.RegexTimeout [| box functionName |]

    /// <summary>Tells whether a regular expression matches anywhere in text.</summary>
    /// <arg name="text">The text to search. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="pattern">The regular expression, in ECMAScript syntax with no flags. A pattern that is not valid is a runtime error, as is a search that runs past the time limit for regular expressions the host sets (one second unless the host sets another).</arg>
    /// <returns>True when pattern matches some part of text, false otherwise. Never null.</returns>
    /// <example><c>RegexTest("order-123", "[0-9]+")</c> returns true.</example>
    type RegexTestFunction() =
        inherit JyroFunctionBase("RegexTest", Signatures.binary "text" ParameterType.String "pattern"ParameterType.String ParameterType.Boolean)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let pattern = args.GetString(1)
            let ticker = Bounds.Ticker ctx
            let regex = compilePattern "RegexTest()" ticker pattern
            let m = execPattern "RegexTest()" ctx ticker regex str 0
            JyroBoolean.FromBoolean(not (isNull m)) :> JyroValue

    /// <summary>Finds the first match of a regular expression in text.</summary>
    /// <arg name="text">The text to search. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="pattern">The regular expression, in ECMAScript syntax with no flags. A pattern that is not valid is a runtime error, as is a search that runs past the time limit for regular expressions the host sets (one second unless the host sets another).</arg>
    /// <returns>The text of the first, leftmost match, which may be the empty string; null when pattern matches nowhere in text.</returns>
    /// <example><c>RegexMatch("order-123", "[0-9]+")</c> returns <c>"123"</c>.</example>
    type RegexMatchFunction() =
        inherit JyroFunctionBase("RegexMatch", Signatures.binary "text" ParameterType.String "pattern"ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let pattern = args.GetString(1)
            let ticker = Bounds.Ticker ctx
            let regex = compilePattern "RegexMatch()" ticker pattern
            let m = execPattern "RegexMatch()" ctx ticker regex str 0
            if isNull m then JyroNull.Instance :> JyroValue
            else
                // A match is part of `text`; only the whole of it is exempt
                // from the maximum value size (spec 100.8.7). It is copied a
                // chunk at a time with a count per chunk.
                JyroString(Bounds.slice ticker str m.[0] (m.[1] - m.[0])) :> JyroValue

    /// <summary>Finds every match of a regular expression in text.</summary>
    /// <arg name="text">The text to search. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="pattern">The regular expression, in ECMAScript syntax with no flags. A pattern that is not valid is a runtime error, as is a search that runs past the time limit for regular expressions the host sets (one second unless the host sets another).</arg>
    /// <returns>An array of the text of each match, from left to right, the matches not overlapping; after an empty match the search goes on from the next character. The empty array when there is no match. Never null.</returns>
    /// <example><c>RegexMatchAll("a1b22c333", "[0-9]+")</c> returns <c>["1", "22", "333"]</c>.</example>
    type RegexMatchAllFunction() =
        inherit JyroFunctionBase("RegexMatchAll", Signatures.binary "text" ParameterType.String "pattern"ParameterType.String ParameterType.Array)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let pattern = args.GetString(1)
            let ticker = Bounds.Ticker ctx
            let regex = compilePattern "RegexMatchAll()" ticker pattern
            // One match at a time, each its own evaluation under the regex
            // timeout. After an empty match the next search starts one code
            // unit further on (91.16, AdvanceStringIndex with no `u` flag).
            let arr = JyroArray()
            let mutable from = 0
            let mutable m = execPattern "RegexMatchAll()" ctx ticker regex str from
            while not (isNull m) do
                ticker.CheckNow()
                arr.AddBounded(JyroString(Bounds.slice ticker str m.[0] (m.[1] - m.[0])))
                from <- if m.[1] = m.[0] then m.[1] + 1 else m.[1]
                m <- if from > str.Length then null else execPattern "RegexMatchAll()" ctx ticker regex str from
            arr :> JyroValue

    /// <summary>Finds the first match of a regular expression in text, with its position and its capture groups.</summary>
    /// <arg name="text">The text to search. A number or a boolean is read as its string form, and null as the empty string.</arg>
    /// <arg name="pattern">The regular expression, in ECMAScript syntax with no flags. A pattern that is not valid is a runtime error, as is a search that runs past the time limit for regular expressions the host sets (one second unless the host sets another).</arg>
    /// <returns>An object for the first, leftmost match, with three properties: match, the text matched; index, the position in text where it begins, counted from 0 in UTF-16 code units; and groups, an array of the text of each capture group in order, null for a group that took no part in the match. Null when pattern matches nowhere in text.</returns>
    /// <example><c>RegexMatchDetail("id: 42", "([a-z]+): ([0-9]+)")</c> returns <c>{"match": "id: 42", "index": 0, "groups": ["id", "42"]}</c>.</example>
    type RegexMatchDetailFunction() =
        inherit JyroFunctionBase("RegexMatchDetail",
            Signatures.binary "text" ParameterType.String "pattern"ParameterType.String ParameterType.Object)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let str = args.GetString(0)
            let pattern = args.GetString(1)
            let ticker = Bounds.Ticker ctx
            let regex = compilePattern "RegexMatchDetail()" ticker pattern
            let m = execPattern "RegexMatchDetail()" ctx ticker regex str 0
            if isNull m then
                JyroNull.Instance :> JyroValue
            else
                let result = JyroObject()
                result.SetProperty("match", JyroString(Bounds.slice ticker str m.[0] (m.[1] - m.[0])))
                // An integer, held exactly (91.17.6).
                result.SetProperty("index", JyroNumber(decimal m.[0]))
                let groups = JyroArray(min regex.GroupCount ValueSize.Maximum)
                for g = 1 to regex.GroupCount do
                    ticker.Tick()
                    let s = m.[2 * g]
                    let e = m.[2 * g + 1]
                    // A group that took part in no match is undefined in
                    // ECMAScript, which Jyro spells `null` (91.17).
                    if s < 0 || e < 0 then groups.AddBounded(JyroNull.Instance)
                    else groups.AddBounded(JyroString(Bounds.slice ticker str s (e - s)))
                result.SetProperty("groups", groups)
                result :> JyroValue

    [<Literal>]
    let private DefaultCharset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

    /// <summary>Makes a string of random characters.</summary>
    /// <remarks>Returns a random value: each call can give a different string, its characters drawn from a cryptographically secure random source.</remarks>
    /// <arg name="length">The number of characters to make: a whole number from 0 to 1000000. A negative, fractional or larger number is a runtime error.</arg>
    /// <arg name="characterSet">The characters to draw from, each UTF-16 code unit equally likely at each position; a character written twice is twice as likely. The empty string is a runtime error. A number or a boolean is read as its string form. When it is left out, or null, the 26 capital letters, the 26 small letters and the 10 digits are used.</arg>
    /// <returns>A string of length characters, each drawn at random from characterSet. Never null.</returns>
    /// <example><c>RandomString(8, "0123456789")</c> returns eight random digits, such as <c>"40912785"</c>.</example>
    type RandomStringFunction() =
        inherit JyroFunctionBase("RandomString",
            Signatures.create
                [ Parameter.Required("length", ParameterType.Number)
                  Parameter.Optional("characterSet", ParameterType.String) ]
                ParameterType.String)
        override _.Effect = FunctionEffect.NonDeterministic
        override this.Execute(args, ctx) =
            let length = JyroArguments.ReadNonNegativeInt32(args, 0, "RandomString()", "length")
            // Same bound as PadLeft/PadRight, and for the same reason: the
            // allocation is set by the *value* of an argument rather than the
            // size of one, so no statement or loop budget can see it coming
            // (Resource Limits 100.8). RandomString(500000000) is a single
            // statement asking for a gigabyte.
            if length > maxGeneratedLength then
                JyroError.raiseRuntime MessageCode.GeneratedLengthExceeded [| box "RandomString()"; box length; box maxGeneratedLength |]
            let charset =
                if not (Bounds.isAbsent args 1) then
                    let s = args.GetString(1)
                    if String.IsNullOrEmpty(s) then
                        JyroError.raiseRuntime MessageCode.EmptyCharacterSet [| box "RandomString()" |]
                    s
                else DefaultCharset
            // Drawn and written in place a chunk at a time with a count per
            // chunk, not by one loop over the whole length (spec 100.2.5,
            // JYRO-ADR-101).
            JyroString(
                Bounds.create (Bounds.Ticker ctx) length (ChunkWriter(fun destination at ->
                    let n = min ExecutionCheck.ChunkLength (length - at)
                    for k in at .. at + n - 1 do
                        destination.[k] <- charset.[RandomNumberGenerator.GetInt32(0, charset.Length)]
                    n))) :> JyroValue

    let getAll () : IJyroFunction list =
        [ ToUpperFunction()
          ToLowerFunction()
          TrimFunction()
          ReplaceFunction()
          ContainsFunction()
          StartsWithFunction()
          EndsWithFunction()
          SplitFunction()
          JoinFunction()
          SubstringFunction()
          ToNumberFunction()
          PadLeftFunction()
          PadRightFunction()
          RegexTestFunction()
          RegexMatchFunction()
          RegexMatchAllFunction()
          RegexMatchDetailFunction()
          RandomStringFunction() ]

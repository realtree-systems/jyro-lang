namespace RealTree.Jyro

open System
open System.Collections.Generic
open System.Globalization

// DateTime functions
module internal DateTimeFunctions =

    // ISO 8601 output format matching C# implementation. The literal
    // `Z` suffix asserts UTC, so callers MUST ensure the value has
    // `DateTimeKind.Utc` before formatting - either by parsing through
    // `readDate` (which gives the UTC instant) or by calling
    // `.ToUniversalTime()`.
    [<Literal>]
    let private Iso8601Format = "yyyy-MM-ddTHH:mm:ss.fffZ"

    // The date a string gives, as a UTC `DateTime`, when the string matches
    // `full-date [ ("T" / "t") partial-time [time-offset] ]` with the rules
    // of RFC 3339 section 5.6, and `false` for every other string (spec
    // 90.8.5 to 90.8.7, JYRO-ADR-18). `T` and `Z` match in either case; a
    // space for `T`, a time with no seconds, a fraction with no digit, an
    // offset with no colon and any text before or after are refused.
    //
    // The one reader of every function that reads a date - ParseDate,
    // FormatDate, DateAdd, DateDiff and DatePart - so each accepts the same
    // strings and gives each the same instant (90.8.8): a date alone is
    // midnight, a time with no offset is UTC, `-00:00` is an offset of zero,
    // a second of 60 is read as 59 with its fraction kept, and the fraction
    // is kept to the millisecond, its further digits discarded. The day must
    // exist in its month (RFC 3339 section 5.7; year 0000 is a leap year, as
    // in the proleptic Gregorian calendar).
    //
    // The range of 94.3.8 is checked on the instant, after the second of 60
    // is read as 59, the fraction cut to the millisecond and the offset
    // subtracted: `0000-12-31T23:30:00-01:00` is `0001-01-01T00:30:00.000Z`,
    // and an instant before `0001-01-01T00:00:00.000Z` or after
    // `9999-12-31T23:59:59.999Z` is refused (JYRO-REQ-1017).
    //
    // The fraction can be any number of digits, so they are read a chunk at
    // a time with a check between chunks (spec 100.2.5).
    let internal readDate (text: string) : bool * DateTime =
        let len = text.Length
        let refused = (false, DateTime.MinValue)
        // The last millisecond a date can hold, 9999-12-31T23:59:59.999Z,
        // counted from 0001-01-01T00:00:00.000Z.
        let lastMillisecond = DateTime.MaxValue.Ticks / TimeSpan.TicksPerMillisecond
        // The instant of the local time with `offset` minutes east of UTC, or
        // refused when it is outside the range. Year 0000 has the calendar of
        // year 2000 (both are leap years), and its days come before day 0.
        let instant (year: int) (month: int) (day: int) (hour: int) (minute: int) (second: int) (millisecond: int) (offset: int) =
            let dayNumber =
                if year = 0 then int64 (DateTime(2000, month, day).DayOfYear - 1 - 366)
                else DateTime(year, month, day).Ticks / TimeSpan.TicksPerDay
            let ms =
                dayNumber * 86_400_000L + int64 hour * 3_600_000L + int64 minute * 60_000L
                + int64 second * 1_000L + int64 millisecond - int64 offset * 60_000L
            if ms < 0L || ms > lastMillisecond then refused
            else (true, DateTime(ms * TimeSpan.TicksPerMillisecond, DateTimeKind.Utc))
        // The `n` digits at `at` as a number, or -1 when any is not a digit
        // or the text ends first.
        let digits (at: int) (n: int) =
            if at + n > len then -1
            else
                let mutable value = 0
                let mutable k = 0
                while k < n && value >= 0 do
                    let c = text.[at + k]
                    value <- if c >= '0' && c <= '9' then value * 10 + int c - int '0' else -1
                    k <- k + 1
                value
        if len < 10 || text.[4] <> '-' || text.[7] <> '-' then refused
        else
            let year = digits 0 4
            let month = digits 5 2
            let day = digits 8 2
            if year < 0 || month < 1 || month > 12 || day < 1
               || day > DateTime.DaysInMonth((if year = 0 then 2000 else year), month) then refused
            elif len = 10 then instant year month day 0 0 0 0 0
            elif len < 19 || (text.[10] <> 'T' && text.[10] <> 't') || text.[13] <> ':' || text.[16] <> ':' then refused
            else
                let hour = digits 11 2
                let minute = digits 14 2
                let second = digits 17 2
                if hour < 0 || hour > 23 || minute < 0 || minute > 59 || second < 0 || second > 60 then refused
                else
                    let mutable at = 19
                    let mutable millisecond = 0
                    let mutable fractionOk = true
                    if at < len && text.[at] = '.' then
                        at <- at + 1
                        let first = at
                        while at < len && text.[at] >= '0' && text.[at] <= '9' do
                            if (at - first) % ExecutionCheck.ChunkLength = 0 && at > first then ExecutionCheck.Check()
                            if at - first < 3 then millisecond <- millisecond * 10 + int text.[at] - int '0'
                            at <- at + 1
                        let count = at - first
                        if count = 0 then fractionOk <- false
                        elif count = 1 then millisecond <- millisecond * 100
                        elif count = 2 then millisecond <- millisecond * 10
                    // The offset in minutes east of UTC, or None when what
                    // follows the time is not an offset.
                    let offset =
                        if not fractionOk then None
                        elif at = len then Some 0
                        elif (text.[at] = 'Z' || text.[at] = 'z') && at + 1 = len then Some 0
                        elif (text.[at] = '+' || text.[at] = '-') && at + 6 = len && text.[at + 3] = ':' then
                            let oh = digits (at + 1) 2
                            let om = digits (at + 4) 2
                            if oh < 0 || oh > 23 || om < 0 || om > 59 then None
                            else Some ((if text.[at] = '-' then -1 else 1) * (oh * 60 + om))
                        else None
                    match offset with
                    | None -> refused
                    | Some minutes -> instant year month day hour minute (min second 59) millisecond minutes

    // Every parse and format MUST use the invariant culture. The host's
    // CurrentCulture would otherwise silently corrupt output: th-TH renders
    // the Thai Buddhist year (2024 -> 2567), ar-SA the Hijri calendar
    // (2024-01-15 -> 1445-07-03) and cannot even parse an ISO date, and
    // some cultures emit non-ASCII digits. Jyro output must be identical
    // on every machine.
    let private inv = CultureInfo.InvariantCulture

    // The unit names of `DateAdd` and `DateDiff` (spec 94.5.4).
    let private unitNames =
        [| "day"; "days"; "week"; "weeks"; "month"; "months"; "year"; "years"
           "hour"; "hours"; "minute"; "minutes"; "second"; "seconds" |]

    // The part names of `DatePart` (spec 94.7.3).
    let private partNames =
        [| "year"; "month"; "day"; "hour"; "minute"; "second"; "dayofweek"; "dayofyear" |]

    // The date argument at `index`, named `name`, as its text and its
    // instant. Every check of the argument is made here, at once: its type
    // (`ArgumentTypeMismatch` for anything but a string, `null` included,
    // spec 90.4.5) and then whether `readDate` can read it
    // (`DateFormatInvalid`). A function reads each later argument only after
    // this returns, so a date that cannot be read is reported before any
    // check of a later parameter (spec 90.3.8, JYRO-ADR-1, JYRO-DEF-648).
    let private dateArgument (args: IReadOnlyList<JyroValue>) (index: int) (name: string) : string * DateTime =
        let text = args.Get<JyroString>(index).Value
        match readDate text with
        | true, dt -> (text, dt)
        | _ -> JyroError.raiseRuntime MessageCode.DateFormatInvalid [| box text; box name |]

    // The string argument at `index`: a string only, any other type `null`
    // included raising `ArgumentTypeMismatch` (spec 90.4.5), not read as its
    // string form.
    let private stringArgument (args: IReadOnlyList<JyroValue>) (index: int) : string =
        args.Get<JyroString>(index).Value

    /// <summary>Gives the current date and time in UTC.</summary>
    /// <remarks>Reads the clock: each call can give a different result.</remarks>
    /// <returns>The current instant as an ISO 8601 string in UTC, to the millisecond, in the form <c>yyyy-MM-ddTHH:mm:ss.fffZ</c>, for example <c>"2024-01-15T09:30:00.000Z"</c>. Never null.</returns>
    /// <example><c>Now()</c> returns a string such as <c>"2024-01-15T09:30:00.000Z"</c>.</example>
    type NowFunction() =
        inherit JyroFunctionBase("Now", Signatures.create [] ParameterType.String)
        override _.Effect = FunctionEffect.NonDeterministic
        override _.Execute(_, _) =
            JyroString(DateTime.UtcNow.ToString(Iso8601Format, inv)) :> JyroValue

    /// <summary>Gives the current date in UTC.</summary>
    /// <remarks>Reads the clock: each call can give a different result.</remarks>
    /// <returns>The current date in UTC as a string in the form <c>yyyy-MM-dd</c>, with no time, for example <c>"2024-01-15"</c>. Never null.</returns>
    /// <example><c>Today()</c> returns a string such as <c>"2024-01-15"</c>.</example>
    type TodayFunction() =
        inherit JyroFunctionBase("Today", Signatures.create [] ParameterType.String)
        override _.Effect = FunctionEffect.NonDeterministic
        override _.Execute(_, _) =
            JyroString(DateTime.UtcNow.Date.ToString("yyyy-MM-dd", inv)) :> JyroValue

    /// <summary>Reads a date written as an ISO 8601 string and gives it in the standard UTC form.</summary>
    /// <remarks>The forms accepted here are the ones every date function accepts. A date is four digits of year, two of month and two of day, as in <c>2024-01-15</c>, and the day must exist in its month. It may be followed by <c>T</c> and a time of two digits each of hour, minute and second, as in <c>T09:30:00</c>; then, optionally, a point and one or more digits of fraction; then, optionally, <c>Z</c> or an offset of the form <c>+HH:mm</c> or <c>-HH:mm</c>. <c>T</c> and <c>Z</c> may be in either case. A date alone is midnight; a time with no offset is UTC. A second of 60 is read as 59, and a fraction is kept to the millisecond, its further digits discarded. A space in place of <c>T</c>, a time with no seconds, an offset with no colon, or any text before or after is not accepted, and neither is an instant before <c>0001-01-01T00:00:00.000Z</c> or after <c>9999-12-31T23:59:59.999Z</c>.</remarks>
    /// <arg name="text">The text to read: a string only. Any other type, null included, is a runtime error.</arg>
    /// <returns>The instant text gives, in UTC to the millisecond, in the form <c>yyyy-MM-ddTHH:mm:ss.fffZ</c>; null when text is not a date in one of the accepted forms.</returns>
    /// <example><c>ParseDate("2024-01-15T10:30:00+02:00")</c> returns <c>"2024-01-15T08:30:00.000Z"</c>; <c>ParseDate("15/01/2024")</c> returns null.</example>
    type ParseDateFunction() =
        inherit JyroFunctionBase("ParseDate", Signatures.unary "text" ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // A string only: any other type, null included, is
            // ArgumentTypeMismatch (spec 90.4), not text read as its string form.
            let dateStr = args.Get<JyroString>(0).Value
            // One parse path, `readDate`, shared with FormatDate, DateAdd,
            // DateDiff and DatePart, so all five accept the same strings and
            // read each the same way (spec 90.8.8). A string it refuses is
            // null here and an error in the other four (90.4.4).
            match readDate dateStr with
            | true, dt -> JyroString(dt.ToString(Iso8601Format, inv)) :> JyroValue
            | _ -> JyroNull.Instance :> JyroValue

    // `dt` formatted by `format` as FormatDate gives it (spec 94.4.5 to
    // 94.4.10). The format is read from left to right:
    //
    // - A single quote followed by another is one single quote. Any other
    //   single quote starts quoted text, copied without its quotes up to the
    //   next single quote that is not followed by another; inside it, two
    //   single quotes are one single quote (94.4.6). Quoted text that is not
    //   closed raises `DateFormatStringInvalid` (94.4.9).
    // - An ASCII letter starts a run, the longest string of copies of that
    //   letter. A run of only `T` or only `Z` is copied as it is (94.4.7); a
    //   run that is a token of 94.4.5 gives its value, in ASCII digits from
    //   the UTC instant; every other run raises `DateFormatStringInvalid`
    //   (94.4.8).
    // - Every other character is copied as it is (94.4.7).
    //
    // No .NET format string is involved, so no .NET specifier, standard
    // format or culture can reach the result. An empty format gives the
    // empty string (94.4.10).
    //
    // The format is read with a count per `ExecutionCheck.ChunkLength`
    // code units (spec 100.2.5). Each token gives at most two code units
    // per code unit of the format (`M` gives up to `12`), so the result can
    // be longer than the format; it is refused above the maximum value size
    // (spec 100.8.7), but only once the whole format has been read, so a
    // format that is not valid is reported as `DateFormatStringInvalid`
    // wherever its fault is (spec 90.3.8: a check of the argument comes
    // before an error of the computation).
    let private formatDate (ticker: Bounds.Ticker) (dt: DateTime) (format: string) : string =
        let length = format.Length
        let chunk = ExecutionCheck.ChunkLength
        let text = Text.StringBuilder()
        let mutable oversize = false
        let mutable steps = 0
        let step () =
            if steps = 0 then ticker.Tick()
            steps <- steps + 1
            if steps >= chunk then steps <- 0
        let fits (count: int) =
            if not oversize && int64 text.Length + int64 count > int64 ValueSize.Maximum then oversize <- true
            not oversize
        let appendChar (c: char) = if fits 1 then text.Append(c) |> ignore
        let appendText (s: string) = if fits s.Length then text.Append(s) |> ignore
        let appendRepeated (c: char) (count: int) =
            if fits count then
                let mutable left = count
                while left > 0 do
                    ticker.Tick()
                    let n = min chunk left
                    text.Append(c, n) |> ignore
                    left <- left - n
        let invalid () : unit =
            JyroError.raiseRuntime MessageCode.DateFormatStringInvalid [| box format |]
        let two (value: int) = value.ToString("D2", inv)
        let one (value: int) = value.ToString(inv)
        let mutable i = 0
        while i < length do
            step ()
            let c = format.[i]
            if c = '\'' then
                if i + 1 < length && format.[i + 1] = '\'' then
                    appendChar '\''
                    i <- i + 2
                else
                    i <- i + 1
                    let mutable closed = false
                    while not closed do
                        if i >= length then invalid ()
                        step ()
                        if format.[i] <> '\'' then
                            appendChar format.[i]
                            i <- i + 1
                        elif i + 1 < length && format.[i + 1] = '\'' then
                            appendChar '\''
                            i <- i + 2
                        else
                            closed <- true
                            i <- i + 1
            elif (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then
                let mutable j = i + 1
                while j < length && format.[j] = c do
                    step ()
                    j <- j + 1
                let run = j - i
                match c, run with
                | ('T' | 'Z'), _ -> appendRepeated c run
                | 'y', 4 -> appendText (dt.Year.ToString("D4", inv))
                | 'M', 2 -> appendText (two dt.Month)
                | 'M', 1 -> appendText (one dt.Month)
                | 'd', 2 -> appendText (two dt.Day)
                | 'd', 1 -> appendText (one dt.Day)
                | 'H', 2 -> appendText (two dt.Hour)
                | 'H', 1 -> appendText (one dt.Hour)
                | 'm', 2 -> appendText (two dt.Minute)
                | 'm', 1 -> appendText (one dt.Minute)
                | 's', 2 -> appendText (two dt.Second)
                | 's', 1 -> appendText (one dt.Second)
                | 'f', 3 -> appendText (dt.Millisecond.ToString("D3", inv))
                | _ -> invalid ()
                i <- j
            else
                appendChar c
                i <- i + 1
        if oversize then ValueSize.Raise()
        Bounds.ofBuilder ticker text

    /// <summary>Writes a date as text, following a format pattern.</summary>
    /// <remarks>The values written are those of the date in UTC, with ASCII digits, the same on every machine.</remarks>
    /// <arg name="date">The date to write: a string in one of the forms <c>ParseDate</c> accepts. A string in no accepted form is a runtime error, as is any other type, null included.</arg>
    /// <arg name="format">The pattern, read from left to right. These runs of letters are replaced by a value: <c>yyyy</c> the four-digit year; <c>MM</c> and <c>M</c> the month; <c>dd</c> and <c>d</c> the day; <c>HH</c> and <c>H</c> the hour, from 0 to 23; <c>mm</c> and <c>m</c> the minute; <c>ss</c> and <c>s</c> the second; <c>fff</c> the millisecond, in three digits. The two-letter forms write two digits, with a leading zero; the one-letter forms write no leading zero. A run of <c>T</c> or of <c>Z</c> is copied as it is. Text between single quotes is copied without its quotes, and two single quotes write one single quote, inside quoted text or outside it. Every character that is not an ASCII letter is copied as it is. Any other run of an ASCII letter, such as <c>yy</c>, <c>ddd</c> or <c>h</c>, and quoted text that is not closed, are a runtime error, as is a format that is not a string, null included. An empty format gives the empty string.</arg>
    /// <returns>The text the pattern gives for date. Never null.</returns>
    /// <example><c>FormatDate("2024-01-15T09:05:00Z", "dd/MM/yyyy HH:mm")</c> returns <c>"15/01/2024 09:05"</c>; <c>FormatDate("2024-01-15", "'Day' d")</c> returns <c>"Day 15"</c>.</example>
    type FormatDateFunction() =
        inherit JyroFunctionBase("FormatDate", Signatures.binary "date" ParameterType.String "format"ParameterType.String ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            // `date` through all its checks, then `format` (spec 90.3.8).
            let _, dt = dateArgument args 0 "date"
            let format = stringArgument args 1
            JyroString(formatDate (Bounds.Ticker ctx) dt format) :> JyroValue

    /// <summary>Adds an amount of a unit of time to a date.</summary>
    /// <remarks>Adding months or years keeps the day of the month where the new month has it, and otherwise gives the last day of that month: one month after 2024-01-31 is 2024-02-29. The time of day is kept for every unit but hours, minutes and seconds.</remarks>
    /// <arg name="date">The date to add to: a string in one of the forms <c>ParseDate</c> accepts. A string in no accepted form is a runtime error, as is any other type, null included.</arg>
    /// <arg name="amount">The number of units to add: a whole number from -2147483648 to 2147483647, negative to go back in time. A fractional number, a number outside that range, or a value that is not a number, null included, is a runtime error.</arg>
    /// <arg name="unit">The unit of amount, one of <c>"day"</c>, <c>"days"</c>, <c>"week"</c>, <c>"weeks"</c>, <c>"month"</c>, <c>"months"</c>, <c>"year"</c>, <c>"years"</c>, <c>"hour"</c>, <c>"hours"</c>, <c>"minute"</c>, <c>"minutes"</c>, <c>"second"</c> or <c>"seconds"</c>, matched with no regard to the case of ASCII letters. Any other string is a runtime error, as is a value that is not a string, null included.</arg>
    /// <returns>The date that results, in UTC to the millisecond, in the form <c>yyyy-MM-ddTHH:mm:ss.fffZ</c>. A result before <c>0001-01-01T00:00:00.000Z</c> or after <c>9999-12-31T23:59:59.999Z</c> is a runtime error. Never null.</returns>
    /// <example><c>DateAdd("2024-01-15", 10, "days")</c> returns <c>"2024-01-25T00:00:00.000Z"</c>; <c>DateAdd("2024-01-15T12:00:00Z", -2, "hours")</c> returns <c>"2024-01-15T10:00:00.000Z"</c>.</example>
    type DateAddFunction() =
        inherit JyroFunctionBase("DateAdd",
            Signatures.create
                [ Parameter.Required("date", ParameterType.String)
                  Parameter.Required("amount", ParameterType.Number)
                  Parameter.Required("unit", ParameterType.String) ]
                ParameterType.String)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // Each argument through all its checks before the next is read,
            // in parameter order (spec 90.3.8): `date` (type, then whether it
            // can be read), `amount` (type, IntegerRequired,
            // IntegerOutOfRange), `unit` (type, then DateUnitInvalid). Only
            // then the result, whose range is an error of the computation.
            let dateStr, dt = dateArgument args 0 "date"
            // Was its own DateAddAmountNotInteger, whose template carried no
            // arguments and so could not say what it received. The condition is
            // the library-wide one, so it reports the library-wide code.
            let intAmount = JyroArguments.ReadInt32(args, 1, "DateAdd()", "amount")
            // Compared with no case sensitivity for ASCII letters (spec 94.5.5,
            // 10.7.1), which `ToLowerInvariant` is not: it maps U+212A to `k`.
            // `unit` is the name matched, or null for none.
            let unitText = stringArgument args 2
            let unit = AsciiCase.find unitNames unitText
            if isNull unit then
                // The one argument is the text as the script wrote it,
                // not folded: `ToLowerInvariant` reported "wee" + U+212A
                // as "week", a valid unit. The valid names are the same
                // in every occurrence, so they are in the template, not
                // an argument (spec 130.6.2, JYRO-ADR-125).
                JyroError.raiseRuntime MessageCode.DateUnitInvalid [| box unitText |]
            // Every arm below can land outside the range a DateTime can
            // hold, and each raises its own ArgumentOutOfRangeException
            // when it does - `AddYears` above 10000, `AddMonths` above
            // 120000, the rest wherever the result leaves the range. Those
            // escaped as the RuntimeError catch-all carrying a .NET string
            // naming neither the function nor the parameter, which is the
            // loss the argument helpers were introduced to remove.
            //
            // `GetInt32` above has already established that the
            // amount is a representable integer; this is the separate
            // question of whether the *result* exists, and it is reported
            // separately. A date has a precision of one millisecond and
            // every unit is a whole number of milliseconds, so the range a
            // DateTime holds is the range of 94.3.8 here.
            let result =
              try
                match unit with
                | "day" | "days" -> dt.AddDays(float intAmount)
                // Widen before multiplying. `intAmount * 7` is F#'s
                // unchecked `int * int`, so it wrapped in 32 bits *before*
                // the conversion to float and produced a different number
                // rather than an error: `DateAdd(d, -1227133513, "weeks")`
                // wrapped to exactly 1 and returned the day after `d`,
                // reporting success. A request to move back 23.5 million
                // years answered "tomorrow". Every sibling arm widens
                // first, which is why this was the only one affected.
                | "week" | "weeks" -> dt.AddDays(float intAmount * 7.0)
                | "month" | "months" -> dt.AddMonths(intAmount)
                | "year" | "years" -> dt.AddYears(intAmount)
                | "hour" | "hours" -> dt.AddHours(float intAmount)
                | "minute" | "minutes" -> dt.AddMinutes(float intAmount)
                | _ -> dt.AddSeconds(float intAmount)
              with :? ArgumentOutOfRangeException ->
                // The unit as the script gave it, not the name it matched
                // (spec 130.6.2): `DAYS` is reported as `DAYS`, not `days`.
                JyroError.raiseRuntime MessageCode.DateOutOfRange
                    [| box "DateAdd()"; box intAmount; box unitText; box dateStr |]
            JyroString(result.ToString(Iso8601Format, inv)) :> JyroValue

    // The number of milliseconds in each unit of `DateDiff` that is a fixed
    // duration (spec 94.5.4).
    let private unitMilliseconds (unit: string) : int64 =
        match unit with
        | "second" | "seconds" -> 1_000L
        | "minute" | "minutes" -> 60_000L
        | "hour" | "hours" -> 3_600_000L
        | "day" | "days" -> 86_400_000L
        | _ -> 604_800_000L

    // `ms / unit` as a number: exact when the quotient is a decimal fraction
    // (spec 94.6.8, 30.2.4), and the nearest double otherwise. The quotient
    // is a decimal fraction exactly when, once the fraction is reduced, its
    // denominator has no prime factor but 2 and 5. Every unit divides
    // 2^10 * 3^3 * 5^5 * 7, so such a quotient has at most ten digits after
    // the point, and `ms` is below 3.2e14 in magnitude: it is always within
    // the exact representation, and decimal division gives it with no
    // rounding.
    let private elapsed (ms: int64) (unit: int64) : JyroValue =
        let rec gcd (a: int64) (b: int64) = if b = 0L then a else gcd b (a % b)
        let mutable denominator = unit / gcd (abs ms) unit
        while denominator % 2L = 0L do denominator <- denominator / 2L
        while denominator % 5L = 0L do denominator <- denominator / 5L
        if denominator = 1L then JyroNumber(decimal ms / decimal unit) :> JyroValue
        else JyroNumber(float ms / float unit) :> JyroValue

    // The whole number `n` of calendar units (a month, or a year when
    // `years`) with the largest magnitude such that
    // `DateAdd(startDate, n, unit)` does not pass `endDate` (spec 94.6.5):
    // not after it when `endDate` is not before `startDate`, not before it
    // otherwise. A result `DateAdd` could not give (DateOutOfRange) passes
    // it. `DateAdd` moves the date monotonically with `n`, so the count is
    // found from the difference of the two months by a step or two.
    let private calendarUnits (startDate: DateTime) (endDate: DateTime) (years: bool) : int =
        let add (n: int) : DateTime voption =
            try ValueSome (if years then startDate.AddYears n else startDate.AddMonths n)
            with :? ArgumentOutOfRangeException -> ValueNone
        let forward = endDate >= startDate
        let direction = if forward then 1 else -1
        let notPast (n: int) =
            match add n with
            | ValueSome reached -> if forward then reached <= endDate else reached >= endDate
            | ValueNone -> false
        let months = (endDate.Year - startDate.Year) * 12 + endDate.Month - startDate.Month
        let mutable n = if years then months / 12 else months
        while n <> 0 && not (notPast n) do n <- n - direction
        while notPast (n + direction) do n <- n + direction
        n

    /// <summary>Measures the time from one date to another, in a given unit.</summary>
    /// <remarks>For weeks, days, hours, minutes and seconds the result is the exact elapsed time in that unit and can have a fraction: 36 hours is 1.5 days. For months and years it is a whole number: the largest count n, in magnitude, for which <c>DateAdd(startDate, n, unit)</c> does not go past endDate. The result is negative when endDate is before startDate.</remarks>
    /// <arg name="startDate">The date to measure from: a string in one of the forms <c>ParseDate</c> accepts. A string in no accepted form is a runtime error, as is any other type, null included.</arg>
    /// <arg name="endDate">The date to measure to: a string in one of the forms <c>ParseDate</c> accepts. A string in no accepted form is a runtime error, as is any other type, null included.</arg>
    /// <arg name="unit">The unit of the result, one of <c>"day"</c>, <c>"days"</c>, <c>"week"</c>, <c>"weeks"</c>, <c>"month"</c>, <c>"months"</c>, <c>"year"</c>, <c>"years"</c>, <c>"hour"</c>, <c>"hours"</c>, <c>"minute"</c>, <c>"minutes"</c>, <c>"second"</c> or <c>"seconds"</c>, matched with no regard to the case of ASCII letters. Any other string is a runtime error, as is a value that is not a string, null included.</arg>
    /// <returns>The number of units from startDate to endDate. Never null.</returns>
    /// <example><c>DateDiff("2024-01-01", "2024-01-15", "days")</c> returns 14; <c>DateDiff("2024-01-31", "2024-03-30", "months")</c> returns 1.</example>
    type DateDiffFunction() =
        inherit JyroFunctionBase("DateDiff",
            Signatures.create
                [ Parameter.Required("startDate", ParameterType.String)
                  Parameter.Required("endDate", ParameterType.String)
                  Parameter.Required("unit", ParameterType.String) ]
                ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // Each argument through all its checks before the next is read
            // (spec 90.3.8): `startDate`, then `endDate`, then `unit`. When
            // neither date can be read, `startDate` is the one named (spec
            // 94.6.11, 130.6.4, JYRO-REQ-2055): it is examined first.
            let _, startDate = dateArgument args 0 "startDate"
            let _, endDate = dateArgument args 1 "endDate"
            // As in `DateAdd` (spec 94.6.6, 10.7.1).
            let unitText = stringArgument args 2
            let unit = AsciiCase.find unitNames unitText
            if isNull unit then
                // As in `DateAdd`: the text as written, one argument.
                JyroError.raiseRuntime MessageCode.DateUnitInvalid [| box unitText |]
            match unit with
            | "month" | "months" -> JyroNumber(decimal (calendarUnits startDate endDate false)) :> JyroValue
            | "year" | "years" -> JyroNumber(decimal (calendarUnits startDate endDate true)) :> JyroValue
            | _ ->
                // From the millisecond values of the two dates (spec 94.6.9):
                // `readDate` gives each at a whole millisecond.
                let ms = (endDate.Ticks - startDate.Ticks) / TimeSpan.TicksPerMillisecond
                elapsed ms (unitMilliseconds unit)

    /// <summary>Gives one part of a date, such as its year or its day of the week, as a number.</summary>
    /// <remarks>The part is taken from the date in UTC.</remarks>
    /// <arg name="date">The date to read: a string in one of the forms <c>ParseDate</c> accepts. A string in no accepted form is a runtime error, as is any other type, null included.</arg>
    /// <arg name="part">The part to give, one of <c>"year"</c>, <c>"month"</c> (1 to 12), <c>"day"</c> (the day of the month, from 1), <c>"hour"</c> (0 to 23), <c>"minute"</c>, <c>"second"</c>, <c>"dayofweek"</c> (0 for Sunday to 6 for Saturday) or <c>"dayofyear"</c> (1 to 366), matched with no regard to the case of ASCII letters. Any other string is a runtime error, as is a value that is not a string, null included.</arg>
    /// <returns>The part asked for, as a whole number. Never null.</returns>
    /// <example><c>DatePart("2024-01-15T09:30:00Z", "hour")</c> returns 9; <c>DatePart("2024-01-15", "dayofweek")</c> returns 1.</example>
    type DatePartFunction() =
        inherit JyroFunctionBase("DatePart", Signatures.binary "date" ParameterType.String "part"ParameterType.String ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            // `date` through all its checks, then `part` (spec 90.3.8).
            let _, dt = dateArgument args 0 "date"
            // Compared with no case sensitivity for ASCII letters (spec 94.7.5,
            // 10.7.1); `part` is the name matched, or null for none.
            let partText = stringArgument args 1
            let part = AsciiCase.find partNames partText
            // An exact integer (spec 94.7.7).
            let result =
                match part with
                | "year" -> dt.Year
                | "month" -> dt.Month
                | "day" -> dt.Day
                | "hour" -> dt.Hour
                | "minute" -> dt.Minute
                | "second" -> dt.Second
                | "dayofweek" -> int dt.DayOfWeek
                | "dayofyear" -> dt.DayOfYear
                | _ ->
                    // The text as written, one argument; the valid names
                    // are in the template (spec 130.6.2, JYRO-ADR-125).
                    JyroError.raiseRuntime MessageCode.DatePartInvalid [| box partText |]
            JyroNumber(decimal result) :> JyroValue

    // Get all datetime functions
    let getAll () : IJyroFunction list =
        [ NowFunction()
          TodayFunction()
          ParseDateFunction()
          FormatDateFunction()
          DateAddFunction()
          DateDiffFunction()
          DatePartFunction() ]

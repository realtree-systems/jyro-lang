namespace RealTree.Jyro

open System
open System.Collections.Generic
open System.Globalization
open System.Numerics
open System.Security.Cryptography

module internal MathFunctions =

    // -- Exact-preservation helpers ------------------------------------------
    // Numeric stdlib keeps results EXACT when every operand is exact and the
    // exact representation (spec 30.2.4) holds the result with no rounding,
    // and falls back to double otherwise. Whether a result is held is decided
    // in integers, never by asking `decimal`, which rounds to 28 digits and
    // reports success. Power, SquareRoot and Log are exact only in the cases
    // their chapter names (spec 93.7.4, 93.8.3, 93.9.7).

    // The numbers in an array, for the six aggregates.
    //
    // A null element is *absent* and is skipped; anything else non-numeric is
    // the wrong type and is refused. That is the rule the library already
    // applies one argument earlier - `GetArray` and
    // `GetObject` normalize a null and reject a wrong type - and this
    // was the one place that did not follow it. Every non-numeric element used
    // to be dropped silently, so `Sum(['a','b'])` was 0: the total of two
    // strings reported as zero, which is a fabricated answer rather than a
    // lenient one.
    //
    // Tolerating null is not a softening of that rule but the other half of
    // it, and it is what keeps the library's own idiom working: `GetProperty`
    // yields null for a missing key and `Select` yields null for a missing
    // field, so `Sum(Select(orders, "discount"))` is full of nulls whenever
    // the field is optional - which is the ordinary shape of JSON.
    //
    // It also makes `Average` correct rather than accidentally correct. It
    // divides by the count of values found, which is right when the missing
    // ones are absent and indefensible when they are values silently thrown
    // away. Same divisor either way; only under this rule is it right for a
    // reason.
    //
    // Single pass over a script/host-supplied array: check the execution
    // limits and cancellation token per element so a very large input
    // cannot bypass the time budget (Resource Limits 100.1/100.2/100.4).
    let private numbersOf (ctx: JyroExecutionContext) (functionName: string) (arr: JyroArray) : JyroNumber list =
        let result = ResizeArray<JyroNumber>(arr.Count)
        for i = 0 to arr.Count - 1 do
            ctx.CheckExecutionLimits()
            match arr.Items.[i] with
            | :? JyroNumber as n -> result.Add(n)
            | :? JyroNull -> ()
            | _ ->
                // Exactly two arguments: the function's name as the book
                // spells it (`Sum`) and the position from 0 (spec 90.7.4,
                // 160.7.16).
                JyroError.raiseRuntime MessageCode.NumericElementRequired
                    [| box functionName; box i |]
        // The list is built a number at a time, counted as the first pass
        // is, not by one unchecked copy of the whole (spec 100.2.5).
        let ticker = Bounds.Ticker ctx
        let mutable numbers = []
        for i = result.Count - 1 downto 0 do
            ticker.Tick()
            numbers <- result.[i] :: numbers
        numbers

    // Every pass over the numbers below counts each one for the execution
    // check, as `numbersOf` does: none is one library call over the whole
    // list with no check inside (spec 100.2.5).

    let private allExact (ctx: JyroExecutionContext) (ns: JyroNumber list) =
        let ticker = Bounds.Ticker ctx
        let mutable rest = ns
        let mutable exact = true
        while exact && not rest.IsEmpty do
            ticker.Tick()
            exact <- rest.Head.IsExact
            rest <- rest.Tail
        exact

    // The count of `ns`, walking it with a check.
    let private checkedLength (ctx: JyroExecutionContext) (ns: 'T list) : int =
        let ticker = Bounds.Ticker ctx
        let mutable rest = ns
        let mutable count = 0
        while not rest.IsEmpty do
            ticker.Tick()
            count <- count + 1
            rest <- rest.Tail
        count

    // The total of `xs` in order of position, from 0.0, as `List.sum` adds
    // them, with a check.
    let private checkedSum (ctx: JyroExecutionContext) (xs: float list) : float =
        let ticker = Bounds.Ticker ctx
        let mutable total = 0.0
        for x in xs do
            ticker.Tick()
            total <- total + x
        total

    // `project` of each number, for a pass after `numbersOf`, counting each
    // one for the execution check as the first pass does (spec 100.2.5).
    let private checkedMap (ctx: JyroExecutionContext) (project: JyroNumber -> 'T) (ns: JyroNumber list) : 'T list =
        let ticker = Bounds.Ticker ctx
        ns |> List.map (fun n -> ticker.Tick(); project n)

    // The number order of spec 92.11.6, the order `Sort` gives numbers
    // (90.6.3): by double value; with equal double values, two exact numbers
    // by exact value and an exact number before an inexact one.
    let private numberOrder (a: JyroNumber) (b: JyroNumber) : int = JyroValue.CompareForSort(a, b)

    // `ns` in the number order of 92.11.6 by a stable merge sort, so tied
    // numbers keep their order in `values` (90.6.2), counting each
    // comparison for the execution check (spec 100.2.5).
    let private sortedNumbers (ctx: JyroExecutionContext) (ns: JyroNumber list) : JyroNumber[] =
        let ticker = Bounds.Ticker ctx
        let sorted = Array.zeroCreate<JyroNumber> (checkedLength ctx ns)
        let mutable i = 0
        for n in ns do
            ticker.Tick()
            sorted.[i] <- n
            i <- i + 1
        StableSort.sortInPlace sorted numberOrder ticker.Tick
        sorted

    // The element of `ns` that no other element comes `before` in the number
    // order of 92.11.6, the one at the lowest position among tied elements,
    // returned as it is, with its own representation (spec 93.5.6, 93.5.7,
    // 93.6.6, 93.6.7). `before c` is whether an element whose comparison
    // with the one held so far is `c` replaces it.
    let private selectNumber (ctx: JyroExecutionContext) (before: int -> bool) (ns: JyroNumber list) : JyroValue =
        let ticker = Bounds.Ticker ctx
        let selected =
            ns |> List.reduce (fun held n ->
                ticker.Tick()
                if before (numberOrder n held) then n else held)
        selected :> JyroValue

    let private ten = BigInteger(10)

    // 10^n, for `n` at least 0.
    let private pow10 (n: int) : BigInteger = BigInteger.Pow(ten, n)

    // The value `n / d` in the exact representation, when it holds it with
    // no rounding (spec 30.2.4): reduced, its denominator must be
    // `2^a * 5^b`, and then it is `n * 2^(k-a) * 5^(k-b) / 10^k` with
    // `k = max(a, b)`, which must be within the significand and the scale.
    let private exactOfRatio (n: BigInteger) (d: BigInteger) : decimal voption =
        if d.IsZero then ValueNone
        else
            let n, d = if d.Sign < 0 then -n, -d else n, d
            let g = BigInteger.GreatestCommonDivisor(n, d)
            let mutable n = n / g
            let mutable d = d / g
            let two = BigInteger(2)
            let five = BigInteger(5)
            let mutable twos = 0
            let mutable fives = 0
            while twos <= ExactNumber.MaxScale && d.IsEven do
                d <- d / two
                twos <- twos + 1
            while fives <= ExactNumber.MaxScale && (d % five).IsZero do
                d <- d / five
                fives <- fives + 1
            if not d.IsOne then ValueNone
            else
                let k = max twos fives
                if k > ExactNumber.MaxScale then ValueNone
                else
                    n <- n * BigInteger.Pow(two, k - twos) * BigInteger.Pow(five, k - fives)
                    ExactNumber.ofParts n k

    // The total of exact `ns` exactly, as a significand and a scale: the
    // total is `m / 10^s`. `decimal` adds at the larger scale and gives up
    // scale only when it rounds, so a running total is kept in `decimal`
    // while every step keeps its scale, and the total is counted in
    // integers once one does not or one overflows (JYRO-REQ-951, spec
    // 93.10.6).
    let private exactTotal (ctx: JyroExecutionContext) (ns: JyroNumber list) : struct (BigInteger * int) =
        let ticker = Bounds.Ticker ctx
        let mutable acc = 0m
        let mutable held = true
        let mutable rest = ns
        while held && not rest.IsEmpty do
            ticker.Tick()
            let x = rest.Head.DecimalValue
            try
                let r = acc + x
                if r.Scale = max acc.Scale x.Scale then acc <- r else held <- false
            with :? OverflowException -> held <- false
            rest <- rest.Tail
        if held then ExactNumber.parts acc
        else
            let parts = ns |> checkedMap ctx (fun n -> ExactNumber.parts n.DecimalValue)
            let mutable scale = 0
            for struct (_, sx) in parts do
                ticker.Tick()
                scale <- max scale sx
            // The powers of ten a part is raised by, at most the largest
            // scale, made once rather than once for each number.
            let powers = Array.init (scale + 1) pow10
            let mutable total = BigInteger.Zero
            for struct (m, sx) in parts do
                ticker.Tick()
                total <- total + m * powers.[scale - sx]
            struct (total, scale)

    // The IEEE 754 double nearest to `m * 10^e`, ties to even, from the
    // platform's correctly rounded parser; an infinity when it is beyond the
    // range of a double.
    let private nearestDouble (m: BigInteger) (e: int64) : float =
        Double.Parse(m.ToString(CultureInfo.InvariantCulture) + "E" + e.ToString(CultureInfo.InvariantCulture),
                     NumberStyles.Float, CultureInfo.InvariantCulture)

    // The exact binary value of a finite double: `f = m * 2^e`.
    let private binaryParts (f: float) : struct (BigInteger * int) =
        let bits = BitConverter.DoubleToInt64Bits f
        let biased = int ((bits >>> 52) &&& 0x7FFL)
        let fraction = bits &&& 0xFFFFFFFFFFFFFL
        let mantissa, exponent =
            if biased = 0 then fraction, -1074
            else (fraction ||| (1L <<< 52)), biased - 1075
        struct ((if bits < 0L then BigInteger(-mantissa) else BigInteger(mantissa)), exponent)

    // The mean of two doubles in double precision, halving before adding
    // only when the total is not finite, so a finite mean of two values near
    // the top of the range is returned (spec 93.11.8, 93.13.10).
    let private meanOfPair (a: float) (b: float) : float =
        let total = a + b
        if Double.IsFinite total then total / 2.0 else a / 2.0 + b / 2.0

    // The mean of `xs`, of which there are `count`, in double precision: the
    // total, in order of position, over the count. When that total is not
    // finite, each value is scaled down by a power of two at least the count
    // first, which is exact but for subnormal parts far below the total's
    // precision, so a finite mean is still returned (spec 93.11.8).
    let private meanOfDoubles (ctx: JyroExecutionContext) (xs: float list) (count: int) : float =
        let total = checkedSum ctx xs
        if Double.IsFinite total then total / float count
        else
            let mutable shift = 1
            while (1L <<< shift) < int64 count do shift <- shift + 1
            let ticker = Bounds.Ticker ctx
            let mutable scaled = 0.0
            for x in xs do
                ticker.Tick()
                scaled <- scaled + Math.ScaleB(x, -shift)
            Math.ScaleB(scaled / float count, shift)

    // A natural logarithm in double precision. An exact number whose nearest
    // double is 1 but which is not 1 has its logarithm from its distance to
    // 1, `ln(1 + t) = t - t^2 / 2` to double precision for such a small `t`,
    // so a base such as `1.0000000000000000000000000001`, which is not 1, is
    // not divided by a logarithm of zero.
    let private naturalLog (x: JyroNumber) : float =
        if x.IsExact && x.Value = 1.0 && x.DecimalValue <> 1m then
            let t = ExactNumber.toDouble (x.DecimalValue - 1m)
            t - t * t / 2.0
        else Math.Log(x.Value)

    // The integer square root of `n`, at least 0: the greatest `r` with
    // `r * r <= n`.
    let private integerSquareRoot (n: BigInteger) : BigInteger =
        if n.Sign <= 0 then BigInteger.Zero
        else
            let mutable r = BigInteger(Math.Sqrt(float n))
            while r * r > n do r <- r - BigInteger.One
            while (r + BigInteger.One) * (r + BigInteger.One) <= n do r <- r + BigInteger.One
            r

    // How `Round` resolves a value exactly halfway between two multiples
    // (JYRO-REQ-891 to JYRO-REQ-893, spec 93.4.9). Every other value goes to
    // the nearest one.
    [<RequireQualifiedAccess>]
    type private Halfway =
        | Up
        | Even
        | Down

    // `p / q`, `q` above 0, rounded to an integer: to the nearer of the two
    // integers around it, and by `halfway` when it is exactly between them.
    let private roundQuotient (p: BigInteger) (q: BigInteger) (halfway: Halfway) : BigInteger =
        let truncated = BigInteger.Divide(p, q)
        let remainder = p - truncated * q
        let floor, above =
            if remainder.Sign < 0 then truncated - BigInteger.One, remainder + q
            else truncated, remainder
        let c = (above * BigInteger(2)).CompareTo(q)
        if c < 0 then floor
        elif c > 0 then floor + BigInteger.One
        else
            match halfway with
            | Halfway.Up -> if p.Sign >= 0 then floor + BigInteger.One else floor
            | Halfway.Down -> if p.Sign >= 0 then floor else floor + BigInteger.One
            | Halfway.Even -> if floor.IsEven then floor else floor + BigInteger.One

    // Renders a double for a diagnostic, culture-invariantly, so a message
    // reads the same wherever it is produced.
    let private fmt (v: float) = v.ToString(CultureInfo.InvariantCulture)

    // A function's name as the book writes it in a diagnostic, `Power()`.
    let private callName (f: JyroFunctionBase) = f.Name + "()"

    // The name of `f`'s parameter at `index`, read from its signature, never
    // written as a literal, so a diagnostic cannot name a parameter the
    // function does not have (JYRO-ADR-123, spec 130.6.4).
    let private parameterName (f: JyroFunctionBase) (index: int) = f.Signature.Parameters.[index].Name

    // For each value in signature order, the parameter's name and then the
    // value, each its own argument (JYRO-ADR-123).
    let private namedValues (f: JyroFunctionBase) (values: string list) : (obj | null)[] =
        values
        |> List.mapi (fun i v -> [ box (parameterName f i); box v ])
        |> List.concat
        |> Array.ofList

    // `NumberDomainError` naming the parameter at `index` and its value, for
    // a pre-check where that one parameter is at fault.
    let private domainError (f: JyroFunctionBase) (index: int) (value: string) : 'T =
        JyroError.raiseRuntime MessageCode.NumberDomainError
            [| box (callName f); box (parameterName f index); box value |]

    // The result of `f` computed from `values`, its parameters' values in
    // signature order, or the diagnostic `OfComputed` gives when it has no
    // finite value.
    let private computed (f: JyroFunctionBase) (values: string list) (result: float) : JyroValue =
        JyroNumber.OfComputed(callName f, namedValues f values, result)

    /// <summary>Gives the absolute value of a number.</summary>
    /// <arg name="value">The number. Null, or a value that is not a number, is a runtime error.</arg>
    /// <returns>The number without its sign: value itself when it is zero or more, and its negation when it is below zero. The result is exact when value is exact. Never null.</returns>
    /// <example><c>Absolute(-3.5)</c> returns <c>3.5</c>.</example>
    type AbsoluteFunction() =
        inherit JyroFunctionBase("Absolute", Signatures.unary "value" ParameterType.Number ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let n = args.Get<JyroNumber>(0)
            if n.IsExact then JyroNumber(Math.Abs(n.DecimalValue)) :> JyroValue
            else JyroNumber(Math.Abs(n.Value)) :> JyroValue

    /// <summary>Rounds a number down to the nearest integer.</summary>
    /// <arg name="value">The number to round. Null, or a value that is not a number, is a runtime error.</arg>
    /// <returns>The greatest integer that is not above value, so a negative number goes away from zero. The result is exact when value is exact. Never null.</returns>
    /// <example><c>Floor(2.7)</c> returns <c>2</c>; <c>Floor(-2.1)</c> returns <c>-3</c>.</example>
    type FloorFunction() =
        inherit JyroFunctionBase("Floor", Signatures.unary "value" ParameterType.Number ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let n = args.Get<JyroNumber>(0)
            if n.IsExact then JyroNumber(Math.Floor(n.DecimalValue)) :> JyroValue
            else JyroNumber(Math.Floor(n.Value)) :> JyroValue

    /// <summary>Rounds a number up to the nearest integer.</summary>
    /// <arg name="value">The number to round. Null, or a value that is not a number, is a runtime error.</arg>
    /// <returns>The least integer that is not below value, so a negative number goes toward zero. The result is exact when value is exact. Never null.</returns>
    /// <example><c>Ceiling(2.1)</c> returns <c>3</c>; <c>Ceiling(-2.7)</c> returns <c>-2</c>.</example>
    type CeilingFunction() =
        inherit JyroFunctionBase("Ceiling", Signatures.unary "value" ParameterType.Number ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let n = args.Get<JyroNumber>(0)
            if n.IsExact then JyroNumber(Math.Ceiling(n.DecimalValue)) :> JyroValue
            else JyroNumber(Math.Ceiling(n.Value)) :> JyroValue

    /// <summary>Rounds a number to a given number of digits after the point.</summary>
    /// <remarks>An exact number is rounded by its exact value. A number that is not exact is rounded by the value it actually holds, which can lie slightly above or below the number as written, so a value that looks halfway may round the other way.</remarks>
    /// <arg name="value">The number to round. Null, or a value that is not a number, is a runtime error.</arg>
    /// <arg name="decimals">How many digits after the point to keep: an integer from -2147483648 to 2147483647. Zero rounds to a whole number; a negative count rounds to tens, hundreds and so on, so -2 rounds to a multiple of 100. A number with a fractional part, a number outside that range, null, or a value that is not a number is a runtime error.</arg>
    /// <arg name="mode">How a value exactly halfway between the two nearest candidates is rounded; every other value goes to the nearer one. Exactly one of these strings, case included: <c>"halfUp"</c>, away from zero; <c>"halfEven"</c>, to the candidate whose last kept digit is even; <c>"halfDown"</c>, toward zero. When it is left out, or null, it is <c>"halfUp"</c>. Any other string, or any value of another type, is a runtime error.</arg>
    /// <returns>The value rounded to decimals digits after the point. When value already has no more digits than that, it is returned unchanged. The result is exact when value is exact and the result can be held exactly; otherwise it is the nearest floating-point value. A result too large to hold is a runtime error. Never null.</returns>
    /// <example><c>Round(1.25, 1)</c> returns <c>1.3</c>; <c>Round(2.5, 0, "halfEven")</c> returns <c>2</c>; <c>Round(1250, -2)</c> returns <c>1300</c>.</example>
    type RoundFunction() =
        inherit JyroFunctionBase("Round",
            Signatures.create
                [ Parameter.Required("value", ParameterType.Number)
                  Parameter.Required("decimals", ParameterType.Number)
                  Parameter.Optional("mode", ParameterType.String) ]
                ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let valueArg = args.Get<JyroNumber>(0)
            // Signed, anywhere in the Int32 range: a negative digit count
            // rounds to tens, hundreds and so on (JYRO-REQ-888, JYRO-REQ-889).
            let decimals = JyroArguments.ReadInt32(args, 1, "Round()", "decimals")
            let halfway =
                // A `mode` of null is a mode not given, which is "halfUp"
                // (90.13.1, JYRO-REQ-894). Any other value that is not exactly one
                // of the three strings, whatever its type, is
                // `InvalidRoundingMode`: it is the type code for `mode` that
                // 90.3.9 (order 1) permits, so no `ArgumentTypeMismatch` is
                // raised for it (spec 93.4.11). The argument is the mode's
                // text: a string's own, a number's, a boolean's, an array's
                // or an object's. A function value has no text the book
                // gives, so it gives no argument (REQ-1491, JYRO-ADR-128). An
                // array's or an object's text is its JSON text, or its type
                // where it has none within the limits (JYRO-ADR-161).
                if Bounds.isAbsent args 2 then Halfway.Up
                else
                    match args.[2] with
                    | :? JyroString as s when s.Value = "halfUp" -> Halfway.Up
                    | :? JyroString as s when s.Value = "halfEven" -> Halfway.Even
                    | :? JyroString as s when s.Value = "halfDown" -> Halfway.Down
                    | :? JyroString as s -> JyroError.raiseRuntime MessageCode.InvalidRoundingMode [| box s.Value |]
                    | :? JyroFunction -> JyroError.raiseRuntime MessageCode.InvalidRoundingMode Array.empty<obj>
                    // Made without raising (JYRO-ADR-161): a cyclic, too-deep
                    // or too-large array or object is named by its type, and
                    // the code stays InvalidRoundingMode (JYRO-REQ-895).
                    | other -> JyroError.raiseRuntime MessageCode.InvalidRoundingMode [| box (ValueWalk.MessageText other) |]
            // The value as given is `num / den` exactly: an exact number by
            // its decimal value, an inexact one by its double's binary value
            // (spec 93.4.2). `places` is how many digits after the point the
            // value has, so rounding to that many or more cannot move it.
            let struct (num, den, places) =
                if valueArg.IsExact then
                    let struct (m, s) = ExactNumber.parts valueArg.DecimalValue
                    struct (m, pow10 s, s)
                else
                    let struct (m, e) = binaryParts valueArg.Value
                    if e >= 0 then struct (m * BigInteger.Pow(BigInteger(2), e), BigInteger.One, 0)
                    else struct (m, BigInteger.Pow(BigInteger(2), -e), -e)
            // A number from a multiple `k` of 10^-decimals: exact when the
            // value is exact and the exact representation holds it, and
            // otherwise the double nearest to it, ties to even (spec 93.4.13,
            // JYRO-REQ-903); a multiple beyond the range of a double is
            // `NumberOverflow` (spec 93.4.16, 30.2.14).
            let ofMultiple (k: BigInteger) : JyroValue =
                let exponent = -(int64 decimals)
                let held =
                    if not valueArg.IsExact then ValueNone
                    elif decimals >= 0 then ExactNumber.ofParts k decimals
                    else ExactNumber.ofParts (k * pow10 (int exponent)) 0
                match held with
                | ValueSome d -> JyroNumber(d) :> JyroValue
                | ValueNone ->
                    computed this [ fmt valueArg.Value; string decimals ] (nearestDouble k exponent)
            if decimals >= 0 && decimals >= places then
                // Rounding to as many places as the value has, or more, gives
                // the value itself, with its own representation.
                valueArg :> JyroValue
            elif decimals >= 0 then
                // Here `decimals` is below `places`, at most 1074.
                ofMultiple (roundQuotient (num * pow10 decimals) den halfway)
            else
                // Every number is below 10^309 in magnitude, so a multiple of
                // a greater power of ten that is nearest to it is zero, and
                // the power is not computed (JYRO-REQ-890).
                let power = -(int64 decimals)
                if power > 330L then
                    if valueArg.IsExact then JyroNumber(0m) :> JyroValue else JyroNumber(0.0) :> JyroValue
                else
                    ofMultiple (roundQuotient num (den * pow10 (int power)) halfway)

    /// <summary>Finds the least number in an array.</summary>
    /// <arg name="values">The array of numbers. A null element is skipped; any other element that is not a number is a runtime error. Null is read as an empty array, and a value that is not an array is a runtime error.</arg>
    /// <returns>The least of the numbers, returned as it is in the array; of several equal least numbers, the first. Null when the array holds no numbers: when it is empty, null, or holds only null elements.</returns>
    /// <example><c>Min([3, 1, null, 2])</c> returns <c>1</c>; <c>Min([])</c> returns null.</example>
    type MinFunction() =
        inherit JyroFunctionBase("Min", Signatures.unary "values" ParameterType.Array ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let ns = numbersOf ctx "Min" (args.GetArray(0))
            if ns.IsEmpty then JyroNull.Instance :> JyroValue
            // The least in the number order of 92.11.6, the first of tied
            // ones, as it is (JYRO-REQ-905, spec 93.5.6, 93.5.7).
            else selectNumber ctx (fun c -> c < 0) ns

    /// <summary>Finds the greatest number in an array.</summary>
    /// <arg name="values">The array of numbers. A null element is skipped; any other element that is not a number is a runtime error. Null is read as an empty array, and a value that is not an array is a runtime error.</arg>
    /// <returns>The greatest of the numbers, returned as it is in the array; of several equal greatest numbers, the first. Null when the array holds no numbers: when it is empty, null, or holds only null elements.</returns>
    /// <example><c>Max([3, 1, null, 2])</c> returns <c>3</c>; <c>Max([])</c> returns null.</example>
    type MaxFunction() =
        inherit JyroFunctionBase("Max", Signatures.unary "values" ParameterType.Array ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let ns = numbersOf ctx "Max" (args.GetArray(0))
            if ns.IsEmpty then JyroNull.Instance :> JyroValue
            // The greatest in the number order of 92.11.6, the first of tied
            // ones, as it is (spec 93.6.6, 93.6.7).
            else selectNumber ctx (fun c -> c > 0) ns

    /// <summary>Raises a number to a power.</summary>
    /// <arg name="value">The number to raise. Null, or a value that is not a number, is a runtime error.</arg>
    /// <arg name="exponent">The power to raise value to. A negative value with an exponent that is not an integer has no real result and is a runtime error. Null, or a value that is not a number, is a runtime error.</arg>
    /// <returns>value raised to exponent; any number to the power 0 is 1. The result is exact when value is exact, exponent is an exact integer, and the result can be held exactly; otherwise the result is rounded to the nearest floating-point value. A result too large to hold, such as <c>Power(0, -1)</c>, is a runtime error. Never null.</returns>
    /// <example><c>Power(2, 10)</c> returns <c>1024</c>; <c>Power(9, 0.5)</c> returns <c>3</c>.</example>
    type PowerFunction() =
        inherit JyroFunctionBase("Power", Signatures.binary "value" ParameterType.Number "exponent"ParameterType.Number ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let baseVal = args.GetNumber(0)
            let exp = args.GetNumber(1)
            let valueArg = args.Get<JyroNumber>(0)
            let exponentArg = args.Get<JyroNumber>(1)
            // A negative value to an exponent that is not an integer has no
            // real answer: `NumberDomainError`, exactly then (JYRO-REQ-1752).
            // Both conditions are decided on the numbers as they are, an
            // exact one by its exact value, before any double is made: an
            // exact exponent such as 2.00000000000000001, or 9007199254740992.5
            // above 2^53, is not an integer but its nearest double is, so a
            // NaN from Math.Pow cannot mark the case. An inexact value is
            // tested as the double it is (-0.0 is not negative). No parameter
            // alone is at fault, so the diagnostic names both, each with its
            // value as written (JYRO-ADR-123).
            let negative =
                if valueArg.IsExact then valueArg.DecimalValue < 0m else baseVal < 0.0
            if negative && not exponentArg.IsInteger then
                JyroError.raiseRuntime MessageCode.NumberDomainError
                    (Array.append
                        [| box (callName this) |]
                        (namedValues this [ valueArg.ToStringValue(); exponentArg.ToStringValue() ]))
            // An exact value to an exact integer exponent is exact when the
            // exact representation holds the result (spec 93.7.4); every
            // other case is computed in double (JYRO-REQ-924).
            let exact =
                if not (valueArg.IsExact && exponentArg.IsExact && exponentArg.IsInteger) then ValueNone
                else
                    let v = valueArg.DecimalValue
                    let e = exponentArg.DecimalValue
                    if e = 0m then ValueSome 1m
                    elif v = 0m then (if e > 0m then ValueSome 0m else ValueNone)
                    elif v = 1m then ValueSome 1m
                    elif v = -1m then ValueSome (if Decimal.Remainder(e, 2m) = 0m then 1m else -1m)
                    // Any other value has a magnitude of at least 2 in its
                    // significand or a scale of at least 1, so above 96 its
                    // power needs more than 96 bits or 28 places.
                    elif abs e > 96m then ValueNone
                    else
                        let n = int e
                        let struct (m, s) = ExactNumber.parts v
                        if n > 0 then exactOfRatio (BigInteger.Pow(m, n)) (pow10 (s * n))
                        else exactOfRatio (pow10 (s * -n)) (BigInteger.Pow(m, -n))
            match exact with
            | ValueSome d -> JyroNumber(d) :> JyroValue
            | ValueNone ->
                // The domain was checked above, so a NaN cannot come from a
                // negative value here; `OfComputed` turns an infinity into
                // `NumberOverflow` (spec 93.7.7, `Power(0, -1)` among them).
                // No parameter alone is at fault, so the diagnostic names
                // both, each name and value its own argument (JYRO-ADR-123).
                computed this [ fmt baseVal; fmt exp ] (Math.Pow(baseVal, exp))

    /// <summary>Gives the square root of a number.</summary>
    /// <arg name="value">The number, zero or more. A negative number is a runtime error, and so is null or a value that is not a number.</arg>
    /// <returns>The non-negative square root of value. The result is exact when value is exact and its square root can be held exactly, as for <c>2.25</c>; otherwise it is rounded to the nearest floating-point value. Never null.</returns>
    /// <example><c>SquareRoot(2.25)</c> returns <c>1.5</c>.</example>
    type SquareRootFunction() =
        inherit JyroFunctionBase("SquareRoot", Signatures.unary "value" ParameterType.Number ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let n = args.GetNumber(0)
            let valueArg = args.Get<JyroNumber>(0)
            // Checked before computing rather than after, so the diagnostic
            // names the parameter, `value`, and its value (spec 130.6.4).
            if n < 0.0 then domainError this 0 (fmt n)
            // An exact value whose square root is exact and held gives it
            // exact (spec 93.8.3). `m / 10^s` with `s` even has the root
            // `sqrt(m) / 10^(s/2)`, which is exact only when `m` is a perfect
            // square; an odd scale is made even first.
            let exact =
                if not valueArg.IsExact then ValueNone
                else
                    let struct (m, s) = ExactNumber.parts valueArg.DecimalValue
                    let m, s = if s % 2 = 1 then m * ten, s + 1 else m, s
                    let r = integerSquareRoot m
                    if r * r = m then ExactNumber.ofParts r (s / 2) else ValueNone
            match exact with
            | ValueSome d -> JyroNumber(d) :> JyroValue
            | ValueNone -> computed this [ fmt n ] (Math.Sqrt(n))

    /// <summary>Gives the logarithm of a number, natural or to a given base.</summary>
    /// <arg name="value">The number, above zero. Zero or a negative number is a runtime error, and so is null or a value that is not a number.</arg>
    /// <arg name="base">The base of the logarithm, above zero and not 1; zero, a negative number or 1 is a runtime error, and so is a value that is not a number. When it is left out, or null, the natural logarithm, to base e, is given.</arg>
    /// <returns>The logarithm of value to base. When value and base are both exact and value is base raised to an integer power, that integer is returned exactly; otherwise the result is rounded to the nearest floating-point value. Never null.</returns>
    /// <example><c>Log(1000, 10)</c> returns <c>3</c>; <c>Log(1)</c> returns <c>0</c>.</example>
    type LogFunction() =
        inherit JyroFunctionBase("Log",
            Signatures.create
                [ Parameter.Required("value", ParameterType.Number)
                  Parameter.Optional("base", ParameterType.Number) ]
                ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let n = args.GetNumber(0)
            // The logarithm is defined on positive values only. Zero gave
            // -Infinity and a negative gave NaN, both of which used to travel
            // as numbers and fail later at the JSON boundary.
            // The diagnostic names the parameter it is about, `value` or
            // `base`, as an argument (spec 93.9.6, 130.6.4).
            if n <= 0.0 then domainError this 0 (fmt n)
            let valueArg = args.Get<JyroNumber>(0)
            // A `base` of null is a base not given (90.13.1).
            if not (Bounds.isAbsent args 1) then
                let baseVal = args.GetNumber(1)
                let baseArg = args.Get<JyroNumber>(1)
                // Base 1 has no logarithm to divide by; a base of zero or less
                // has no logarithm to take (JYRO-REQ-941). An exact base is
                // tested by its exact value, so a base that is not 1 but
                // whose nearest double is 1 is not refused.
                let refused =
                    if baseArg.IsExact then baseArg.DecimalValue <= 0m || baseArg.DecimalValue = 1m
                    else baseVal <= 0.0 || baseVal = 1.0
                if refused then domainError this 1 (fmt baseVal)
                // An exact value that is an exact base to an integer power
                // gives that integer, exact (spec 93.9.7). The integer is near
                // the quotient of the logarithms, and a held value is within
                // about 2^±96, so only a quotient of at most 200 in magnitude
                // is tested, by the power counted in integers.
                let ratio = naturalLog valueArg / naturalLog baseArg
                let exact =
                    if not (valueArg.IsExact && baseArg.IsExact) || not (Double.IsFinite ratio) || abs ratio > 200.0 then
                        ValueNone
                    else
                        let struct (mv, sv) = ExactNumber.parts valueArg.DecimalValue
                        let struct (mb, sb) = ExactNumber.parts baseArg.DecimalValue
                        let isPower (k: int) =
                            if k >= 0 then BigInteger.Pow(mb, k) * pow10 sv = mv * pow10 (sb * k)
                            else mv * BigInteger.Pow(mb, -k) = pow10 (sv + sb * -k)
                        let near = int (Math.Round ratio)
                        [ near; near - 1; near + 1 ] |> List.tryFind isPower
                        |> function Some k -> ValueSome (decimal k) | None -> ValueNone
                match exact with
                | ValueSome k -> JyroNumber(k) :> JyroValue
                | ValueNone -> computed this [ fmt n; fmt baseVal ] ratio
            else
                computed this [ fmt n ] (naturalLog valueArg)

    /// <summary>Adds up the numbers in an array.</summary>
    /// <arg name="values">The array of numbers. A null element is skipped; any other element that is not a number is a runtime error. Null is read as an empty array, and a value that is not an array is a runtime error.</arg>
    /// <returns>The total of the numbers; 0 when the array holds none. The total is exact when every number is exact and the total can be held exactly; otherwise the numbers are added in order as floating-point values. A total too large to hold, such as <c>Sum([1e300, 1e300])</c>, is a runtime error. Never null.</returns>
    /// <example><c>Sum([1, 2, null, 4])</c> returns <c>7</c>; <c>Sum([])</c> returns <c>0</c>.</example>
    type SumFunction() =
        inherit JyroFunctionBase("Sum", Signatures.unary "values" ParameterType.Array ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let values = args.GetArray(0)
            let ns = numbersOf ctx "Sum" values
            // The double fallback is where a total of finite elements can
            // still exceed what a number can hold - `Sum([1e300, 1e300])`.
            // The diagnostic names `values` and gives its value, the array,
            // as SquareRoot gives its number (JYRO-ADR-123); a count with a
            // noun is not an argument (JYRO-ADR-125 item 2).
            let doubleSum () =
                computed this [ values.ToStringValue() ]
                    (ns |> checkedMap ctx (fun n -> n.Value) |> checkedSum ctx)
            // Exact exactly when the exact representation holds the total
            // with no rounding, whatever happens to a running total
            // (JYRO-REQ-951, spec 93.10.6).
            if allExact ctx ns then
                let struct (total, scale) = exactTotal ctx ns
                match ExactNumber.ofParts total scale with
                | ValueSome d -> JyroNumber(d) :> JyroValue
                | ValueNone -> doubleSum ()
            else doubleSum ()

    /// <summary>Gives the mean of the numbers in an array.</summary>
    /// <arg name="values">The array of numbers. A null element is skipped and not counted; any other element that is not a number is a runtime error. Null is read as an empty array, and a value that is not an array is a runtime error.</arg>
    /// <returns>The total of the numbers divided by how many there are. The mean is exact when every number is exact and the mean can be held exactly; otherwise it is computed with floating-point values, and a finite mean is still given when the total itself would be too large to hold. Null when the array holds no numbers: when it is empty, null, or holds only null elements.</returns>
    /// <example><c>Average([1, 2, null, 6])</c> returns <c>3</c>; <c>Average([])</c> returns null.</example>
    type AverageFunction() =
        inherit JyroFunctionBase("Average", Signatures.unary "values" ParameterType.Array ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let values = args.GetArray(0)
            let ns = numbersOf ctx "Average" values
            if ns.IsEmpty then JyroNull.Instance :> JyroValue
            else
                let count = checkedLength ctx ns
                // A finite mean is returned even when the total is not
                // (spec 93.11.8); the report names the aggregate.
                let doubleAvg () =
                    computed this [ values.ToStringValue() ]
                        (meanOfDoubles ctx (ns |> checkedMap ctx (fun n -> n.Value)) count)
                // Exact exactly when the exact representation holds the mean
                // with no rounding, also when it cannot hold the total
                // (JYRO-REQ-960, spec 93.11.6).
                if allExact ctx ns then
                    let struct (total, scale) = exactTotal ctx ns
                    match exactOfRatio total (BigInteger(count) * pow10 scale) with
                    | ValueSome d -> JyroNumber(d) :> JyroValue
                    | ValueNone -> doubleAvg ()
                else doubleAvg ()


    /// <summary>Limits a number to a range.</summary>
    /// <arg name="value">The number to limit. Null, or a value that is not a number, is a runtime error.</arg>
    /// <arg name="min">The lowest number allowed. When it is greater than max, the call is a runtime error. Null, or a value that is not a number, is a runtime error.</arg>
    /// <arg name="max">The highest number allowed. Null, or a value that is not a number, is a runtime error.</arg>
    /// <returns>min when value is below min, max when value is above max, and value otherwise; the number chosen is returned as it was given. Never null.</returns>
    /// <example><c>Clamp(15, 0, 10)</c> returns <c>10</c>; <c>Clamp(5, 0, 10)</c> returns <c>5</c>.</example>
    type ClampFunction() =
        inherit JyroFunctionBase("Clamp",
            Signatures.create
                [ Parameter.Required("value", ParameterType.Number)
                  Parameter.Required("min", ParameterType.Number)
                  Parameter.Required("max", ParameterType.Number) ]
                ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, _) =
            let valueArg = args.Get<JyroNumber>(0)
            let minArg = args.Get<JyroNumber>(1)
            let maxArg = args.Get<JyroNumber>(2)
            // `Math.Clamp` raises a raw ArgumentException ("'min' cannot be
            // greater than 'max'") when the bounds are the wrong way round, so
            // `Clamp(5, 10, 1)` - three small exact integers - escaped as the
            // RuntimeError catch-all carrying a .NET string that named neither
            // the function nor which bound was wrong. Its sibling `RandomInt`
            // had the identical defect and was fixed; `Clamp` was missed.
            if JyroNumber.Compare(minArg, maxArg) > 0 then
                JyroError.raiseRuntime MessageCode.InvalidRange
                    [| box "Clamp()"; box "min"; box (minArg.ToStringValue())
                       box "max"; box (maxArg.ToStringValue()) |]
            // Compared as `<` and `>` compare (spec 30.11.1, 30.11.2): two
            // exact numbers by exact value, any other pair by double. The
            // argument selected is returned as it is, with its own
            // representation, also when `min` equals `max` (spec 93.12.2,
            // 93.12.7).
            if JyroNumber.Compare(valueArg, minArg) < 0 then minArg :> JyroValue
            elif JyroNumber.Compare(valueArg, maxArg) > 0 then maxArg :> JyroValue
            else valueArg :> JyroValue

    /// <summary>Gives the middle value of the numbers in an array.</summary>
    /// <arg name="values">The array of numbers, in any order. A null element is skipped and not counted; any other element that is not a number is a runtime error. Null is read as an empty array, and a value that is not an array is a runtime error.</arg>
    /// <returns>With the numbers in ascending order, the middle one, returned as it is in the array, when there is an odd count; the mean of the two middle ones when there is an even count. That mean is exact when both are exact and it can be held exactly, and otherwise is computed with floating-point values. Null when the array holds no numbers: when it is empty, null, or holds only null elements.</returns>
    /// <example><c>Median([3, 1, 2])</c> returns <c>2</c>; <c>Median([4, 1, 3, 2])</c> returns <c>2.5</c>.</example>
    type MedianFunction() =
        inherit JyroFunctionBase("Median", Signatures.unary "values" ParameterType.Array ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let values = args.GetArray(0)
            let ns = numbersOf ctx "Median" values
            if ns.IsEmpty then
                JyroNull.Instance :> JyroValue
            else
                // The number order of 92.11.6, tied numbers in their order in
                // `values` (spec 93.13.2, 90.6.2).
                let sorted = sortedNumbers ctx ns
                let mid = sorted.Length / 2
                if sorted.Length % 2 = 1 then
                    // The middle number, as it is (spec 93.13.8).
                    sorted.[mid] :> JyroValue
                else
                    let a = sorted.[mid - 1]
                    let b = sorted.[mid]
                    // The mean of the middle pair is computed in double, and
                    // halved before it is added when the pair's total is not
                    // finite, so a finite median is returned (JYRO-REQ-982,
                    // spec 93.13.10).
                    let doubleMedian () = computed this [ values.ToStringValue() ] (meanOfPair a.Value b.Value)
                    if allExact ctx ns then
                        // Exact exactly when the exact representation holds
                        // the mean, also when it cannot hold the pair's total
                        // (JYRO-REQ-979, spec 93.13.7).
                        let struct (total, scale) = exactTotal ctx [ a; b ]
                        match exactOfRatio total (BigInteger(2) * pow10 scale) with
                        | ValueSome d -> JyroNumber(d) :> JyroValue
                        | ValueNone -> doubleMedian ()
                    else doubleMedian ()

    // A group of `Mode` (spec 93.14.11): the numbers that joined it, each
    // equal under 30.10 to every other. All share one double. Its exact
    // members, if any, share one exact value, `Exact`; an inexact number
    // equals every member, and an exact one every member but an exact one
    // of another exact value.
    [<Sealed; AllowNullLiteral>]
    type private ModeGroup(first: JyroNumber) =
        member val Count = 1 with get, set
        member val Exact : decimal voption = (if first.IsExact then ValueSome first.DecimalValue else ValueNone) with get, set
        member _.First = first
        // Whether `n`, which has this group's double, equals every member.
        member this.Admits(n: JyroNumber) =
            not n.IsExact
            || (match this.Exact with
                | ValueNone -> true
                | ValueSome d -> d = n.DecimalValue)
        member this.Join(n: JyroNumber) =
            this.Count <- this.Count + 1
            if n.IsExact && this.Exact.IsNone then this.Exact <- ValueSome n.DecimalValue

    /// <summary>Finds the number that occurs most often in an array.</summary>
    /// <arg name="values">The array of numbers. Numbers equal in value count as the same, so <c>1</c> and <c>1.0</c> are one number. A null element is skipped; any other element that is not a number is a runtime error. Null is read as an empty array, and a value that is not an array is a runtime error.</arg>
    /// <returns>The number that occurs most often, as it first appears in the array; when several occur equally often, the one that appears first. Null when the array holds no numbers: when it is empty, null, or holds only null elements.</returns>
    /// <example><c>Mode([1, 2, 2, 3, 3])</c> returns <c>2</c>; <c>Mode([])</c> returns null.</example>
    type ModeFunction() =
        inherit JyroFunctionBase("Mode", Signatures.unary "values" ParameterType.Array ParameterType.Number)
        override _.Effect = FunctionEffect.Pure
        override this.Execute(args, ctx) =
            let ns = numbersOf ctx "Mode" (args.GetArray(0))
            if ns.IsEmpty then
                JyroNull.Instance :> JyroValue
            else
                // The numbers are taken in order of position, and each joins
                // the earliest-formed group every member of which it equals
                // under 30.10, or forms a new one (spec 93.14.11). Numbers
                // that are equal have the same double, so the groups are kept
                // by double, both zeros under one key, each list in the order
                // its groups were formed.
                let ticker = Bounds.Ticker ctx
                let byDouble = Dictionary<float, ResizeArray<ModeGroup>>()
                let groups = ResizeArray<ModeGroup>()
                for n in ns do
                    ticker.Tick()
                    let key = if n.Value = 0.0 then 0.0 else n.Value
                    let candidates =
                        match byDouble.TryGetValue key with
                        | true, list -> list
                        | _ ->
                            let list = ResizeArray<ModeGroup>()
                            byDouble.[key] <- list
                            list
                    let mutable joined = false
                    let mutable i = 0
                    while not joined && i < candidates.Count do
                        ticker.Tick()
                        if candidates.[i].Admits n then
                            candidates.[i].Join n
                            joined <- true
                        i <- i + 1
                    if not joined then
                        let group = ModeGroup(n)
                        candidates.Add group
                        groups.Add group
                // The first number of the largest group; of groups of the
                // same size, the one formed first (JYRO-REQ-986, spec
                // 93.14.3).
                let mutable best = groups.[0]
                for group in groups do
                    ticker.Tick()
                    if group.Count > best.Count then best <- group
                best.First :> JyroValue

    /// <summary>Returns a random integer from a range.</summary>
    /// <remarks>Returns a random value: each call can give a different result.</remarks>
    /// <arg name="min">The lowest integer that can be returned, from -2147483648 to 2147483647. A number with a fractional part, a number outside that range, null, or a value that is not a number is a runtime error.</arg>
    /// <arg name="max">One more than the highest integer that can be returned, so max itself is never returned; from -2147483648 to 2147483647. When it is not greater than min, the range is empty and the call is a runtime error. A number with a fractional part, a number outside that range, null, or a value that is not a number is a runtime error.</arg>
    /// <returns>An integer at least min and below max, every one of them equally likely. The integer is exact. Never null.</returns>
    /// <example><c>RandomInt(1, 7)</c> returns one of 1, 2, 3, 4, 5 or 6.</example>
    type RandomIntFunction() =
        inherit JyroFunctionBase("RandomInt",
            Signatures.binary "min" ParameterType.Number "max" ParameterType.Number ParameterType.Number)
        override _.Effect = FunctionEffect.NonDeterministic
        override this.Execute(args, _) =
            // Every failure here used to escape as a raw ArgumentOutOfRangeException
            // through the RuntimeError catch-all: RandomInt(5, 3) reported
            // "Range of random number does not contain at least one possibility",
            // naming neither the function nor which bound was wrong. Its two
            // siblings already validated - RandomString raises named codes and
            // RandomChoice guards its degenerate case - so this was the only
            // one of the three with no guard at all.
            let minVal = JyroArguments.ReadInt32(args, 0, "RandomInt()", "min")
            let maxVal = JyroArguments.ReadInt32(args, 1, "RandomInt()", "max")
            // The range is half-open, min <= n < max (spec 93.15.4), so it is
            // empty, and refused, when min is not below max (93.15.6).
            if minVal >= maxVal then
                JyroError.raiseRuntime MessageCode.InvalidRange
                    [| box "RandomInt()"; box "min"; box minVal; box "max"; box maxVal |]
            // GetInt32's upper bound is exclusive too, and it draws by
            // rejection, not modulo, so every integer of the range is equally
            // likely (JYRO-REQ-1000), up to the widest, Int32.MinValue to
            // Int32.MaxValue. The integer is exact (JYRO-REQ-1002).
            JyroNumber(RandomNumberGenerator.GetInt32(minVal, maxVal)) :> JyroValue

    let getAll () : IJyroFunction list =
        [ AbsoluteFunction()
          FloorFunction()
          CeilingFunction()
          RoundFunction()
          MinFunction()
          MaxFunction()
          PowerFunction()
          SquareRootFunction()
          LogFunction()
          SumFunction()
          AverageFunction()
          ClampFunction()
          MedianFunction()
          ModeFunction()
          RandomIntFunction() ]

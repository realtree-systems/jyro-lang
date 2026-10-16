namespace RealTree.Jyro

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Buffers
open System.Text.Json

// A segment in a chained ReadOnlySequence for streaming large JSON data.
type internal ChunkedMemorySegment(memory: ReadOnlyMemory<byte>, runningIndex: int64) =
    inherit ReadOnlySequenceSegment<byte>()
    do
        base.Memory <- memory
        base.RunningIndex <- runningIndex
    member this.SetNext(next: ChunkedMemorySegment) =
        base.Next <- next

/// <summary>
/// The types of values that can exist in the Jyro runtime
/// </summary>
type JyroValueType =
    | Null = 0
    | Boolean = 1
    | Number = 2
    | String = 3
    | Array = 4
    | Object = 5
    | Function = 6

// The names a diagnostic gives a Jyro type.
module internal TypeNames =
    // The name of `t` as `TypeOf` gives it (spec 95.1.2), the form in which
    // a diagnostic names a type (spec 130.6.5): `null`, `boolean`, `number`,
    // `string`, `array`, `object` or `function`. The enum's own names are
    // capitalized, which is not how the language spells them.
    let typeOfName (t: JyroValueType) : string =
        match t with
        | JyroValueType.Null -> "null"
        | JyroValueType.Boolean -> "boolean"
        | JyroValueType.Number -> "number"
        | JyroValueType.String -> "string"
        | JyroValueType.Array -> "array"
        | JyroValueType.Object -> "object"
        | JyroValueType.Function -> "function"
        | other -> string other

// What `JyroValue.FromObject` needs to know about a host type's shape
// (JYRO-ADR-152, as amended by JYRO-ADR-154 and JYRO-ADR-156), decided once
// per type and kept. Every answer here is a property of the type, never of
// the value at hand, so every value of one type has one Jyro type.
module internal HostShapes =
    // The types whose value becomes its text (JYRO-ADR-152 (3), JYRO-ADR-156
    // (c)): a closed list, none of which holds a binary floating-point
    // number, so no NaN or infinity can reach a script as text. An enum value
    // is its name, and is tested apart (`Type.IsEnum`).
    let private textTypes =
        HashSet<Type>(
            [ typeof<DateTime>; typeof<DateTimeOffset>; typeof<DateOnly>; typeof<TimeOnly>
              typeof<TimeSpan>; typeof<Guid>; typeof<Version>; typeof<char>
              typeof<System.Text.StringBuilder> ])

    // The text-form types tested by assignability, so that a subclass is
    // text too: `Uri`, and `CultureInfo`, whose `Parent` chain ends at
    // itself (JYRO-ADR-156 (c)). Reflected by its properties, a `CultureInfo`
    // would lead the walk back to an object it is visiting, and be refused
    // as a cyclic host graph.
    let private textBaseTypes = [| typeof<Uri>; typeof<CultureInfo> |]

    // Whether a value of `t` becomes its text.
    let isTextForm (t: Type) : bool =
        t.IsEnum || textTypes.Contains t
        || textBaseTypes |> Array.exists (fun b -> b.IsAssignableFrom t)

    // The types no Jyro value stands for, tested by assignability, so that a
    // subclass is refused too (JYRO-ADR-200 item 2, amending JYRO-ADR-156 (c)
    // and (g)): a delegate; reflection metadata, `System.Type` and every
    // other `MemberInfo`, `Assembly`, `Module`, `ParameterInfo`; and a task,
    // `Task` and every `Task<T>`, and `ValueTask`. `ValueTask<T>` is a
    // generic struct, tested by its definition.
    let private refusedBaseTypes =
        [| typeof<Delegate>
           typeof<Reflection.MemberInfo>
           typeof<Reflection.Assembly>
           typeof<Reflection.Module>
           typeof<Reflection.ParameterInfo>
           typeof<Threading.Tasks.Task>
           typeof<Threading.Tasks.ValueTask> |]

    let private refusedTypes = System.Collections.Concurrent.ConcurrentDictionary<Type, bool>()

    // Whether a value of `t` is refused with `HostValueNotConvertible`,
    // decided by the type alone, so no member of the value is read.
    let isRefused (t: Type) : bool =
        refusedTypes.GetOrAdd(t, fun (t: Type) ->
            refusedBaseTypes |> Array.exists (fun b -> b.IsAssignableFrom t)
            || (t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Threading.Tasks.ValueTask<_>>))

    // How a host type is a number (JYRO-ADR-156 (a), JYRO-ADR-158 (a2)).
    type NumberKind =
        // Not a number type.
        | NotNumber = 0
        // Implements `System.Numerics.IBinaryInteger<T>`.
        | Integer = 1
        // Implements `System.Numerics.IFloatingPointIeee754<T>`, radix 2.
        | BinaryFloat = 2
        // Implements `System.Numerics.IFloatingPointIeee754<T>`, radix 10.
        | DecimalFloat = 3

    // The double nearest to `magnitude * 2^exponent`, ties to even, for a
    // `magnitude` of zero or more: an infinity when that is past the largest
    // double, zero when it is below half the smallest subnormal. Exact for
    // every value a double holds.
    let nearestDouble (magnitude: Numerics.BigInteger) (exponent: Numerics.BigInteger) : float =
        if magnitude.IsZero then 0.0
        else
            // The exponent of the value's leading bit.
            let top = Numerics.BigInteger(magnitude.GetBitLength()) - Numerics.BigInteger.One + exponent
            if top > Numerics.BigInteger(1023) then Double.PositiveInfinity
            elif top < Numerics.BigInteger(-1076) then 0.0
            else
                // The exponent of the last bit a double keeps at this
                // magnitude: 52 bits below the leading one, or the
                // subnormals' fixed 2^-1074.
                let last = Numerics.BigInteger.Max(top - Numerics.BigInteger(52), Numerics.BigInteger(-1074))
                let shift = last - exponent
                if shift.Sign <= 0 then
                    // At most 53 bits, held exactly.
                    Math.ScaleB(float magnitude, int exponent)
                else
                    let s = int shift
                    let kept = magnitude >>> s
                    let dropped = magnitude - (kept <<< s)
                    let half = Numerics.BigInteger.One <<< (s - 1)
                    let rounded =
                        if dropped > half || (dropped = half && not kept.IsEven) then kept + Numerics.BigInteger.One
                        else kept
                    Math.ScaleB(float rounded, int last)

    // The double nearest to `numerator / denominator`, ties to even, both
    // positive. The quotient is taken to at least 56 bits, and a remainder
    // is kept as a sticky bit below them, so `nearestDouble` rounds it as it
    // would the exact quotient.
    let nearestDoubleOfRatio (numerator: Numerics.BigInteger) (denominator: Numerics.BigInteger) : float =
        if numerator.IsZero then 0.0
        else
            let k = max 0L (56L - (numerator.GetBitLength() - denominator.GetBitLength()))
            let scaled = numerator <<< int k
            let quotient = Numerics.BigInteger.Divide(scaled, denominator)
            let remainder = Numerics.BigInteger.Remainder(scaled, denominator)
            let sticky = if remainder.IsZero then Numerics.BigInteger.Zero else Numerics.BigInteger.One
            nearestDouble ((quotient <<< 1) + sticky) (Numerics.BigInteger(-k - 1L))

    let private ten = Numerics.BigInteger(10)
    let private decimalLimit = Numerics.BigInteger.One <<< 96

    // `m * 10^q`, negated when `negative`, as a Jyro number takes it
    // (JYRO-ADR-158 (a4)): a boxed `decimal` when that value is a
    // `System.Decimal` exactly, else the boxed nearest double, which may be
    // an infinity. Zero is the decimal zero.
    let decimalTimesPowerOfTen (negative: bool) (m: Numerics.BigInteger) (q: Numerics.BigInteger) : obj =
        if m.IsZero then 0m :> obj
        else
            // The same value with no trailing zero in its coefficient, so a
            // decimal needs exactly -q places for it.
            let mutable m = m
            let mutable q = q
            while (Numerics.BigInteger.Remainder(m, ten)).IsZero do
                m <- Numerics.BigInteger.Divide(m, ten)
                q <- q + Numerics.BigInteger.One
            // A decimal is a coefficient below 2^96 over 10^0 to 10^28; 10^29
            // is past 2^96 already.
            let exact =
                if q.Sign >= 0 then
                    if q > Numerics.BigInteger(28) then ValueNone
                    else
                        let n = m * Numerics.BigInteger.Pow(ten, int q)
                        if n < decimalLimit then ValueSome(struct (n, 0)) else ValueNone
                elif q >= Numerics.BigInteger(-28) && m < decimalLimit then ValueSome(struct (m, int -q))
                else ValueNone
            match exact with
            | ValueSome(struct (c, scale)) ->
                let bytes = Array.zeroCreate<byte> 12
                let written = c.ToByteArray(true, false)
                Array.blit written 0 bytes 0 written.Length
                Decimal(
                    BitConverter.ToInt32(bytes, 0), BitConverter.ToInt32(bytes, 4), BitConverter.ToInt32(bytes, 8),
                    negative, byte scale) :> obj
            | ValueNone ->
                // The value lies in [10^top, 10^(top+1)).
                let top = Numerics.BigInteger(m.ToString(CultureInfo.InvariantCulture).Length - 1) + q
                let magnitude =
                    if top > Numerics.BigInteger(400) then Double.PositiveInfinity
                    elif top < Numerics.BigInteger(-400) then 0.0
                    elif q.Sign >= 0 then nearestDouble (m * Numerics.BigInteger.Pow(ten, int q)) Numerics.BigInteger.Zero
                    else nearestDoubleOfRatio m (Numerics.BigInteger.Pow(ten, int -q))
                (if negative then -magnitude else magnitude) :> obj

    // The significand `TryWriteSignificandLittleEndian` writes, an unsigned
    // integer with its leading digit (JYRO-ADR-158 (a3), (a4)).
    let private significandOf<'T when 'T :> Numerics.IFloatingPointIeee754<'T>> (v: 'T) : Numerics.BigInteger =
        let bytes = Array.zeroCreate<byte> (v.GetSignificandByteCount())
        let mutable written = 0
        if not (v.TryWriteSignificandLittleEndian(Span<byte>(bytes), &written)) then
            raise (InvalidOperationException($"{typeof<'T>.FullName} did not write its significand in the bytes it asked for."))
        Numerics.BigInteger(ReadOnlySpan<byte>(bytes, 0, written), true, false)

    // Readers of a host number read as `'T`, the type of the interface it
    // implements (JYRO-ADR-158 (a2)), each made into a delegate once per type
    // so that an exception a number type throws passes out as it was thrown,
    // not wrapped by reflection (JYRO-ADR-157 (a1)).
    type NumberReaders =
        // A CoreLib binary float widened by `Double.CreateTruncating<T>`,
        // which for every such type no wider than double is exact, and keeps
        // a NaN or an infinity as one (JYRO-ADR-156 (a)).
        static member TruncatingToDouble<'T when 'T :> Numerics.INumberBase<'T>>(value: obj) : float =
            Double.CreateTruncating<'T>(value :?> 'T)

        // `INumberBase<T>.Radix`, the base of a float type's significand.
        static member RadixOf<'T when 'T :> Numerics.INumberBase<'T>>() : int =
            'T.Radix

        // A radix-2 float outside CoreLib through the members
        // `IFloatingPointIeee754<T>` requires (JYRO-ADR-158 (a3)): `IsNaN`,
        // `IsInfinity` and `IsNegative`; otherwise its significand `m`, of
        // bit length `L`, is `m * 2^(ILogB(v) - (L - 1))`, rounded to the
        // nearest double. `ILogB` is IEEE 754's logB, the exponent of the
        // leading bit, for a normal and a subnormal value alike, so no bias
        // or subnormal convention of the exponent writer is assumed, and
        // neither it nor `GetSignificandBitLength` is read.
        static member IeeeToDouble<'T when 'T :> Numerics.IFloatingPointIeee754<'T>>(value: obj) : float =
            let v = value :?> 'T
            if 'T.IsNaN(v) then Double.NaN
            elif 'T.IsInfinity(v) then
                if 'T.IsNegative(v) then Double.NegativeInfinity else Double.PositiveInfinity
            else
                let m = significandOf v
                let magnitude =
                    if m.IsZero then 0.0
                    else
                        nearestDouble m (Numerics.BigInteger('T.ILogB(v)) - Numerics.BigInteger(m.GetBitLength() - 1L))
                if 'T.IsNegative(v) then -magnitude else magnitude

        // A radix-10 float (JYRO-ADR-158 (a4)): NaN, infinity and sign as a
        // radix-2 one; otherwise its significand `m`, of `D` decimal digits,
        // is `m * 10^(ILogB(v) - (D - 1))`, a boxed `decimal` when that is a
        // `System.Decimal` exactly, else its nearest double, boxed.
        static member IeeeDecimalToNumber<'T when 'T :> Numerics.IFloatingPointIeee754<'T>>(value: obj) : obj =
            let v = value :?> 'T
            if 'T.IsNaN(v) then Double.NaN :> obj
            elif 'T.IsInfinity(v) then
                (if 'T.IsNegative(v) then Double.NegativeInfinity else Double.PositiveInfinity) :> obj
            else
                let m = significandOf v
                if m.IsZero then 0m :> obj
                else
                    let digits = m.ToString(CultureInfo.InvariantCulture).Length
                    decimalTimesPowerOfTen
                        ('T.IsNegative(v)) m (Numerics.BigInteger('T.ILogB(v)) - Numerics.BigInteger(digits - 1))

        // An integer through the members `IBinaryInteger<T>` requires of
        // every implementer (JYRO-ADR-157 (a1)): `IsNegative`, `GetByteCount`
        // and `TryWriteLittleEndian`, its two's complement read as a
        // `BigInteger`. Never through a `ToString` format or an optional
        // `TryConvert` member.
        static member IntegerToBigInteger<'T when 'T :> Numerics.IBinaryInteger<'T>>(value: obj) : Numerics.BigInteger =
            let v = value :?> 'T
            let negative = 'T.IsNegative(v)
            let bytes = Array.zeroCreate<byte> (v.GetByteCount())
            let mutable written = 0
            if not (v.TryWriteLittleEndian(Span<byte>(bytes), &written)) then
                raise (InvalidOperationException($"{typeof<'T>.FullName} did not write its value in the bytes it asked for."))
            // An unsigned type's value fills every bit it writes, so the
            // bytes are read as signed only for a negative value.
            Numerics.BigInteger(ReadOnlySpan<byte>(bytes, 0, written), not negative, false)

    // How a host type is a number, and its reader, made once per type.
    [<NoComparison; NoEquality>]
    type HostNumber =
        { Kind: NumberKind
          // For `Integer`.
          ToBigInteger: Func<obj, Numerics.BigInteger> | null
          // For `BinaryFloat`.
          ToDouble: Func<obj, float> | null
          // For `DecimalFloat`: a boxed `decimal` or `float`.
          ToNumber: Func<obj, obj> | null }

    let private notNumber =
        { Kind = NumberKind.NotNumber; ToBigInteger = null; ToDouble = null; ToNumber = null }

    // The method of `NumberReaders` named `name` (JYRO-ADR-206): a name that
    // matches no method, or more than one, raises EngineBindingException
    // naming the type and the method, rather than reading every host number
    // as not a number. The type is annotated as keeping its members, so a
    // trimmed host keeps every reader.
    let private readerMethod
        ([<Diagnostics.CodeAnalysis.DynamicallyAccessedMembers(Diagnostics.CodeAnalysis.DynamicallyAccessedMemberTypes.All)>] t: Type)
        (name: string)
        : Reflection.MethodInfo =
        let found =
            t.GetMethods(Reflection.BindingFlags.Static ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
            |> Array.filter (fun m -> m.Name = name)
        match found with
        | [| m |] -> m
        | [||] -> raise (EngineBindingException($"The engine member {t.FullName}.{name} that reads host numbers was not found."))
        | _ -> raise (EngineBindingException($"The engine member {t.FullName}.{name} that reads host numbers matches more than one member."))

    // The five readers, found together once, at the first use of any. Not
    // values of this file: a failure in this file's type initializer would
    // surface as a TypeInitializationException naming no member, at the
    // first touch of anything in this file. Lazy keeps the
    // EngineBindingException, and raises it again at every later use.
    [<NoComparison; NoEquality>]
    type private Readers =
        { IntegerToBigInteger: Reflection.MethodInfo
          TruncatingToDouble: Reflection.MethodInfo
          RadixOf: Reflection.MethodInfo
          IeeeToDouble: Reflection.MethodInfo
          IeeeDecimalToNumber: Reflection.MethodInfo }

    let private readers =
        lazy (
            { IntegerToBigInteger = readerMethod typeof<NumberReaders> (nameof NumberReaders.IntegerToBigInteger)
              TruncatingToDouble = readerMethod typeof<NumberReaders> (nameof NumberReaders.TruncatingToDouble)
              RadixOf = readerMethod typeof<NumberReaders> (nameof NumberReaders.RadixOf)
              IeeeToDouble = readerMethod typeof<NumberReaders> (nameof NumberReaders.IeeeToDouble)
              IeeeDecimalToNumber = readerMethod typeof<NumberReaders> (nameof NumberReaders.IeeeDecimalToNumber) })

    // The readers, for the compiler's table of the engine members it binds
    // (JYRO-ADR-206). Raises EngineBindingException when one is missing or
    // ambiguous.
    let numberReaderMethods () : Reflection.MethodInfo[] =
        let r = readers.Value
        [| r.IntegerToBigInteger; r.TruncatingToDouble; r.RadixOf; r.IeeeToDouble; r.IeeeDecimalToNumber |]

    // A reader made from `m` at `arg`, or None when `arg` breaks the method's
    // constraint, which is not a failure: the type is then not read that way.
    // Only reflection runs here, never a member of the host type, so no
    // exception of the host type's own is lost. The generic parameters of the
    // readers carry no trimming annotation, so any type argument is safe.
    [<Diagnostics.CodeAnalysis.UnconditionalSuppressMessage("Trimming", "IL2060",
        Justification = "The generic parameters of the NumberReaders methods carry no trimming annotation.")>]
    let private readerAt<'R when 'R :> Delegate> (m: Reflection.MethodInfo) (arg: Type) : 'R option =
        let made =
            try Some (m.MakeGenericMethod([| arg |]))
            with :? ArgumentException -> None
        made |> Option.map (fun g -> Delegate.CreateDelegate(typeof<'R>, g) |> unbox<'R>)

    // The reflection over the type of a host value that converting it reads
    // (JYRO-ADR-156): its interfaces, its public instance properties and
    // fields. The type is the host's, known only at run time, so no annotation
    // can keep its members, and trimming may remove them. Nothing here
    // silences that: every member that reaches this reflection carries
    // RequiresUnreferencedCode, up to the public members that convert a host
    // object (`JyroValue.FromObject`, `JyroRunner.WithInput(object)`,
    // `JyroScript.ExecuteScript`), so a host that trims is warned at its own
    // call. None of these members is called from a closure, whose code the
    // trimmer does not count as the member's own.
    [<Literal>]
    let ReflectsHostTypes =
        "Converts a host object by the public properties and fields of its type, which trimming may remove. A host that trims must keep the members of the types it converts, or pass JSON or a JyroValue instead."

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    [<Runtime.CompilerServices.MethodImpl(Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let internal interfacesOfHostType (t: Type) : Type[] = t.GetInterfaces()

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    [<Runtime.CompilerServices.MethodImpl(Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let internal propertiesOfHostType (t: Type) : Reflection.PropertyInfo[] =
        t.GetProperties(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    [<Runtime.CompilerServices.MethodImpl(Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let internal fieldsOfHostType (t: Type) : Reflection.FieldInfo[] =
        t.GetFields(Reflection.BindingFlags.Public ||| Reflection.BindingFlags.Instance)

    // The depth of `c` in its chain of base classes.
    let rec private classDepth (c: Type | null) : int =
        match c with
        | null -> 0
        | c -> 1 + classDepth c.BaseType

    // The `T`s for which `t` implements the generic interface `definition`
    // and to which `t` can be cast, most derived first (JYRO-ADR-158 (a2)):
    // `t` itself, then its base classes from the nearest, then interfaces,
    // the one with more interfaces of its own first. Taken from the
    // interfaces `t` implements, never by `Type.GetInterface(string)`, which
    // throws for a type implementing one interface for two `T`.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let private numberArguments (t: Type) (definition: Type) : Type[] =
        let candidates =
            interfacesOfHostType t
            |> Array.filter (fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = definition)
            |> Array.map (fun i -> i.GetGenericArguments().[0])
            |> Array.filter (fun a -> not a.ContainsGenericParameters && a.IsAssignableFrom t)
            |> Array.distinct
        // The sort keys, read here and not in a closure of the sort.
        let keys = Array.zeroCreate<struct (int * int * string)> candidates.Length
        for i in 0 .. candidates.Length - 1 do
            let a = candidates.[i]
            keys.[i] <-
                if a = t then struct (0, 0, "")
                elif not a.IsInterface then struct (1, -(classDepth a), "")
                else struct (2, -((interfacesOfHostType a).Length), string a)
        Array.init candidates.Length id
        |> Array.sortBy (fun i -> keys.[i])
        |> Array.map (fun i -> candidates.[i])

    // The generic definitions `IBinaryInteger<>` and
    // `IFloatingPointIeee754<>`, from CoreLib, which defines both.
    // Named as types, not used as constraints, which FS3536 warns of.
#nowarn "3536"
    let private binaryIntegerDefinition = typedefof<Numerics.IBinaryInteger<int>>
    let private floatingPointIeee754Definition = typedefof<Numerics.IFloatingPointIeee754<float>>
#warnon "3536"

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let private classify (t: Type) : HostNumber =
        if t = typeof<char> then notNumber
        else
            let r = readers.Value
            let integer =
                numberArguments t binaryIntegerDefinition
                |> Array.tryPick (fun a -> readerAt<Func<obj, Numerics.BigInteger>> r.IntegerToBigInteger a)
            match integer with
            | Some read -> { notNumber with Kind = NumberKind.Integer; ToBigInteger = read }
            | None ->
                let float =
                    numberArguments t floatingPointIeee754Definition
                    |> Array.tryPick (fun a ->
                        if a.Assembly = typeof<float>.Assembly then
                            readerAt<Func<obj, float>> r.TruncatingToDouble a
                            |> Option.map (fun read -> { notNumber with Kind = NumberKind.BinaryFloat; ToDouble = read })
                        else
                            match readerAt<Func<int>> r.RadixOf a,
                                  readerAt<Func<obj, float>> r.IeeeToDouble a,
                                  readerAt<Func<obj, obj>> r.IeeeDecimalToNumber a with
                            | Some radix, Some binary, Some dec ->
                                // Radix is the type's own member: an exception
                                // it throws passes out.
                                match radix.Invoke() with
                                | 2 -> Some { notNumber with Kind = NumberKind.BinaryFloat; ToDouble = binary }
                                | 10 -> Some { notNumber with Kind = NumberKind.DecimalFloat; ToNumber = dec }
                                // Any other radix is not a number type.
                                | _ -> Some notNumber
                            | _ -> None)
                defaultArg float notNumber

    let private hostNumbers = System.Collections.Concurrent.ConcurrentDictionary<Type, HostNumber>()

    // Whether `t` is a number type and how, decided by the interfaces it
    // implements, which any assembly may (JYRO-ADR-157 (a1), JYRO-ADR-158
    // (a2)): `BigInteger`, `Int128`, `UInt128`, `nint`, `nuint`, `NFloat`,
    // and a type outside the BCL, read as the most derived `T` of
    // `IBinaryInteger<T>`, else of `IFloatingPointIeee754<T>`, that `t` can
    // be cast to. A type implementing either only for a `T` it cannot be cast
    // to is not a number, nor is a float type whose radix is neither 2 nor
    // 10. `char` implements `IBinaryInteger<char>` but is a text-form type
    // (JYRO-ADR-152 (3)), and `decimal` implements neither interface.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let hostNumber (t: Type) : HostNumber =
        match hostNumbers.TryGetValue t with
        | true, found -> found
        | _ -> hostNumbers.GetOrAdd(t, classify t)

    // How `t` is a number (see `hostNumber`).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let numberKind (t: Type) : NumberKind = (hostNumber t).Kind

    // A value of a binary floating-point type, widened to double: a CoreLib
    // type by `Double.CreateTruncating<T>`, any other through the members
    // its interface requires (JYRO-ADR-158 (a3)). A NaN or an infinity stays
    // one.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let binaryFloatToDouble (value: obj) : float =
        (nonNull (hostNumber (value.GetType())).ToDouble).Invoke(value)

    // A value of a decimal floating-point type (JYRO-ADR-158 (a4)): a boxed
    // `decimal` when exact, else a boxed `float`, which may be a NaN or an
    // infinity.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let decimalFloatToNumber (value: obj) : obj =
        (nonNull (hostNumber (value.GetType())).ToNumber).Invoke(value)

    // A value of an integer type, as a `BigInteger` (JYRO-ADR-157 (a1)).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let integerToBigInteger (value: obj) : Numerics.BigInteger =
        match value with
        | :? Numerics.BigInteger as b -> b
        | _ -> (nonNull (hostNumber (value.GetType())).ToBigInteger).Invoke(value)

    // How a host value is a Newtonsoft.Json `JToken` (JYRO-ADR-157 (b2)).
    type JTokenKind =
        // Not a `JToken`.
        | NotJToken = 0
        // A `JObject`.
        | Object = 1
        // A `JArray`.
        | Array = 2
        // A `JValue` other than a `JRaw`.
        | Value = 3
        // Any other `JToken`: a `JRaw`, a `JConstructor`, a `JProperty`.
        | Other = 4

    let private jTokenKinds = System.Collections.Concurrent.ConcurrentDictionary<Type, JTokenKind>()

    // How `t` is a `JToken`, recognized by the full names of its type and
    // its base types, so that Jyro.Core takes no reference to Newtonsoft.Json
    // (JYRO-ADR-157 (b2)). The most derived known name decides: a `JRaw` is a
    // `JValue` whose value is JSON text, and is its text.
    let jTokenKind (t: Type) : JTokenKind =
        jTokenKinds.GetOrAdd(t, fun (t: Type) ->
            let rec find (t: Type | null) =
                match t with
                | null -> JTokenKind.NotJToken
                | t ->
                    match t.FullName with
                    | "Newtonsoft.Json.Linq.JObject" -> JTokenKind.Object
                    | "Newtonsoft.Json.Linq.JArray" -> JTokenKind.Array
                    | "Newtonsoft.Json.Linq.JRaw" -> JTokenKind.Other
                    | "Newtonsoft.Json.Linq.JValue" -> JTokenKind.Value
                    | "Newtonsoft.Json.Linq.JToken" -> JTokenKind.Other
                    | _ -> find t.BaseType
            find t)

    let private jValueMembers =
        System.Collections.Concurrent.ConcurrentDictionary<Type, struct (Reflection.PropertyInfo * Reflection.PropertyInfo)>()

    // A `JValue`'s `Type` (its `JTokenType`, by name) and its `Value`.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let jValueParts (value: obj) : struct (string * (obj | null)) =
        let t = value.GetType()
        let struct (typeProperty, valueProperty) =
            match jValueMembers.TryGetValue t with
            | true, found -> found
            | _ ->
                let properties = propertiesOfHostType t
                let named (name: string) =
                    properties |> Array.find (fun p -> p.Name = name && p.GetIndexParameters().Length = 0)
                jValueMembers.GetOrAdd(t, struct (named "Type", named "Value"))
        struct (string (typeProperty.GetValue(value)), valueProperty.GetValue(value))

    let private dictionaryValueTypes = System.Collections.Concurrent.ConcurrentDictionary<Type, Type>()
    let private noDictionary = typeof<Void>

    let private isGenericDictionaryInterface (i: Type) =
        i.IsGenericType
        && (let d = i.GetGenericTypeDefinition()
            d = typedefof<IDictionary<_, _>> || d = typedefof<IReadOnlyDictionary<_, _>>)

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let private interfacesOf (t: Type) = Array.append [| t |] (interfacesOfHostType t)

    // `T` when `t` implements `IDictionary<string, T>` or
    // `IReadOnlyDictionary<string, T>` (JYRO-ADR-154 (1a)), else null.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let dictionaryValueType (t: Type) : Type | null =
        let found =
            match dictionaryValueTypes.TryGetValue t with
            | true, found -> found
            | _ ->
                let valueType =
                    interfacesOf t
                    |> Array.tryPick (fun i ->
                        if isGenericDictionaryInterface i && i.GetGenericArguments().[0] = typeof<string> then
                            Some (i.GetGenericArguments().[1])
                        else None)
                    |> Option.defaultValue noDictionary
                dictionaryValueTypes.GetOrAdd(t, valueType)
        if found = noDictionary then null else found

    let private genericDictionaries = System.Collections.Concurrent.ConcurrentDictionary<Type, bool>()

    // Whether `t` implements `IDictionary<K, T>` or `IReadOnlyDictionary<K,
    // T>` for any `K`. A type that does so only for a `K` that is not string
    // is a sequence of its entries, empty or not, whatever keys it holds
    // (JYRO-ADR-154 (1b)); only a type that implements none of them is
    // decided by its keys (1c).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let isGenericDictionary (t: Type) : bool =
        match genericDictionaries.TryGetValue t with
        | true, found -> found
        | _ -> genericDictionaries.GetOrAdd(t, interfacesOf t |> Array.exists isGenericDictionaryInterface)

    // The converter of a dictionary whose values are of type `T`, made once
    // per `T` by the caller and kept here.
    let dictionaryConverters = System.Collections.Concurrent.ConcurrentDictionary<Type, Delegate>()

    // The converter of a `KeyValuePair<K, T>`, made once per pair type by the
    // caller and kept here.
    let pairConverters = System.Collections.Concurrent.ConcurrentDictionary<Type, Delegate>()

    // Whether `t` is a `KeyValuePair<K, T>`, an entry of a dictionary that is
    // a sequence (JYRO-ADR-154 (1d)).
    let isKeyValuePair (t: Type) : bool =
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<KeyValuePair<_, _>>

    // The public readable instance properties a host object becomes an
    // object of, in the order .NET lists them: an indexer is not one, nor is
    // a property whose getter is not public (JYRO-ADR-156 (d)).
    let private readablePropertiesOf = System.Collections.Concurrent.ConcurrentDictionary<Type, Reflection.PropertyInfo[]>()

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let readableProperties (t: Type) : Reflection.PropertyInfo[] =
        match readablePropertiesOf.TryGetValue t with
        | true, found -> found
        | _ ->
            let readable =
                propertiesOfHostType t
                // Indexers must be skipped, not just unreadable ones: an indexed
                // property reports CanRead = true but needs arguments, so
                // GetValue(value) raises TargetParameterCountException. Reached by
                // any host type carrying an indexer - `string`, `List<T>` and
                // `JyroObject` itself all do.
                |> Array.filter (fun p ->
                    p.CanRead && (match p.GetMethod with | null -> false | getter -> getter.IsPublic)
                    && p.GetIndexParameters().Length = 0)
            readablePropertiesOf.GetOrAdd(t, readable)

    // The public instance fields a host object's properties are followed by,
    // for every type (JYRO-ADR-156 (d)): a `ValueTuple`'s elements, the
    // components of a `System.Numerics.Vector2`, a struct's public fields.
    let private fieldsOf = System.Collections.Concurrent.ConcurrentDictionary<Type, Reflection.FieldInfo[]>()

    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let publicFields (t: Type) : Reflection.FieldInfo[] =
        match fieldsOf.TryGetValue t with
        | true, found -> found
        | _ -> fieldsOf.GetOrAdd(t, fieldsOfHostType t)

    // Whether a host object of `t` has any member it becomes an object of;
    // one with none becomes its text (JYRO-ADR-156 (f)). Decided by the type,
    // not by whether a getter throws for this value (JYRO-ADR-156 (e)).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(ReflectsHostTypes)>]
    let hasMembers (t: Type) : bool =
        (readableProperties t).Length > 0 || (publicFields t).Length > 0

// Internal numeric representation for JyroNumber: an exact base-10 decimal,
// or an IEEE-754 double for values a decimal cannot hold (magnitudes outside
// the decimal range and transcendental results).
//
// The `Inexact` arm does NOT exist to carry NaN or the infinities. A Jyro
// number is finite and `JyroNumber`'s double constructor rejects them.
type internal NumberRep =
    | Exact of decimal
    | Inexact of float

/// <summary>
/// Base class for all Jyro runtime values.
///
/// It has no public or protected constructor: the
/// seven sealed classes <c>JyroNull</c>, <c>JyroBoolean</c>, <c>JyroNumber</c>, <c>JyroString</c>,
/// <c>JyroArray</c>, <c>JyroObject</c> and <c>JyroFunction</c> are its only subclasses, so
/// every walk of the engine sees one of seven kinds.
/// </summary>
[<AbstractClass>]
type JyroValue internal () =
    /// <summary>
    /// Gets the specific runtime type of this value
    /// </summary>
    abstract member ValueType: JyroValueType

    /// <summary>
    /// Gets whether this represents a null value
    /// </summary>
    abstract member IsNull: bool
    default _.IsNull = false

    /// <summary>
    /// Reads a property by key. The default returns null for any type with no
    /// properties; <c>JyroObject</c> is the only override with real data to read.
    /// </summary>
    abstract member GetProperty: key: string -> JyroValue
    default _.GetProperty(key: string) = ignore key; JyroNull.Instance

    /// <summary>
    /// Writes a property by key. The default raises <c>SetPropertyOnNonObject</c>;
    /// only <c>JyroObject</c> can hold one.
    /// </summary>
    abstract member SetProperty: key: string * value: (JyroValue | null) -> unit
    default this.SetProperty(key: string, value: JyroValue | null) =
        ignore value
        JyroError.raiseRuntime MessageCode.SetPropertyOnNonObject [| box key; box (TypeNames.typeOfName this.ValueType) |]

    // `delete o[key]` and `delete o.key` (spec 50.12): removes a property of
    // an object, returning whether it was present, and raises
    // `DeleteOnNonObject` for any other value. Internal: a host removes a
    // property with `JyroObject.Remove` (JYRO-ADR-200 item 3).
    member internal this.DeleteProperty(key: string) : bool =
        match this with
        | :? JyroObject as o -> o.Remove(key)
        | _ -> JyroError.raiseRuntime MessageCode.DeleteOnNonObject [| box key; box (TypeNames.typeOfName this.ValueType) |]

    // The evaluation hooks below - `GetIndex`, `SetIndex`, `ToIterable`,
    // `EvaluateBinary` and `EvaluateUnary` - are internal (JYRO-ADR-191 item
    // 6). F# gives an abstract slot the visibility of its type, so they cannot
    // be internal virtuals: each is an internal member that dispatches on the
    // seven sealed kinds, the only subclasses there are, to that kind's own
    // internal member (`OwnGetIndex`, `OwnEvaluateBinary`, ...), and what was
    // the virtual's default is the `Default...` member a kind falls back to.

    // Reads by index or key, depending on the value's own shape. Null for any
    // value but an array, an object or a string, each of which reads its own
    // index type.
    member internal this.GetIndex(index: JyroValue) : JyroValue =
        match this with
        | :? JyroArray as a -> a.OwnGetIndex(index)
        | :? JyroObject as o -> o.OwnGetIndex(index)
        | :? JyroString as s -> s.OwnGetIndex(index)
        | _ -> JyroNull.Instance

    // Writes by index or key. Raises `SetIndexOnNonContainer` for any value
    // but an array or an object.
    member internal this.SetIndex(index: JyroValue, value: JyroValue) : unit =
        match this with
        | :? JyroArray as a -> a.OwnSetIndex(index, value)
        | :? JyroObject as o -> o.OwnSetIndex(index, value)
        | _ ->
            JyroError.raiseRuntime MessageCode.SetIndexOnNonContainer [| box (TypeNames.typeOfName this.ValueType) |]

    // Converts to an enumerable for foreach loops. Raises `NotIterable` for
    // any value but an array, an object or a string.
    member internal this.ToIterable() : JyroValue seq =
        match this with
        | :? JyroArray as a -> a.OwnToIterable()
        | :? JyroObject as o -> o.OwnToIterable()
        | :? JyroString as s -> s.OwnToIterable()
        | _ ->
            JyroError.raiseRuntime MessageCode.NotIterable [| box (TypeNames.typeOfName this.ValueType) |]

    // Evaluates a binary operator with this value as the left operand: the
    // kind's own operators, then the ones every type shares
    // (`DefaultEvaluateBinary`).
    member internal this.EvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match this with
        | :? JyroNumber as n -> n.OwnEvaluateBinary(op, right)
        | :? JyroString as s -> s.OwnEvaluateBinary(op, right)
        | :? JyroBoolean as b -> b.OwnEvaluateBinary(op, right)
        | :? JyroNull as n -> n.OwnEvaluateBinary(op, right)
        | :? JyroFunction as f -> f.OwnEvaluateBinary(op, right)
        | _ -> this.DefaultEvaluateBinary(op, right)

    // Evaluates a unary operator: `-` on a number, else the shared rule
    // (`DefaultEvaluateUnary`).
    member internal this.EvaluateUnary(op: UnaryOp) : JyroValue =
        match this with
        | :? JyroNumber as n -> n.OwnEvaluateUnary(op)
        | _ -> this.DefaultEvaluateUnary(op)

    // The binary operators every type shares (`==`, `!=`, `and`, `or`, `??`,
    // string `&`); raises `UnsupportedBinaryOperation` for the rest. A kind
    // with operators of its own falls back to it for the others.
    member internal this.DefaultEvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match op with
        | Equal -> JyroBoolean.FromBoolean(this.EqualsValue(right))
        | NotEqual -> JyroBoolean.FromBoolean(not (this.EqualsValue(right)))
        | And -> if not (this.IsTruthy()) then this else right
        | Or -> if this.IsTruthy() then this else right
        | Coalesce -> if this.IsNull then right else this
        | Concat ->
            // An array, an object or a function value has no string form `&`
            // can use (spec 40.4.3): a function's `<function>` text is
            // withdrawn.
            let refused (t: JyroValueType) =
                t = JyroValueType.Array || t = JyroValueType.Object || t = JyroValueType.Function
            // The operation is the first argument, the operator as written,
            // as `Join` names itself there (JYRO-ADR-125 item 3, JYRO-ADR-128
            // item 5); the type refused is the second.
            if refused this.ValueType then
                JyroError.raiseRuntime MessageCode.ConcatWithInvalidType
                    [| box (Operators.binaryOpToString op); box (TypeNames.typeOfName this.ValueType) |]
            elif refused right.ValueType then
                JyroError.raiseRuntime MessageCode.ConcatWithInvalidType
                    [| box (Operators.binaryOpToString op); box (TypeNames.typeOfName right.ValueType) |]
            else
                // The copy checks the time and the cancellation signal as it
                // goes (spec 100.2.5) and refuses a result above the maximum
                // value size before it allocates one (spec 100.8.7, 100.8.8).
                JyroString(BoundedText.concat (ValueWalk.OperandText this) (ValueWalk.OperandText right)) :> JyroValue
        // `+` with a string on either side raises `StringOperandInAdditionRuntime`
        // (spec 40.2.2), whatever type the other operand is. The rule is
        // over either operand, so it is checked here, once, for the left and
        // the right in turn (JYRO-ADR-159), not in each subclass: every
        // type's `+` that is not number + number reaches this arm.
        | Add when (this :? JyroString) || (right :? JyroString) ->
            JyroError.raiseRuntime MessageCode.StringOperandInAdditionRuntime Array.empty
        // Arithmetic and ordering are subclass-specific; the base value
        // type does not support them.
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual ->
            // Both operands are named, because this arm cannot tell which one
            // is at fault. It is reached two ways: directly, when the left
            // operand's type has no arithmetic - `true / 1` - and through
            // `JyroNumber.EvaluateBinary`'s fall-through, when the left operand
            // is a perfectly good number and the RIGHT one is not. Naming only
            // `this.ValueType` meant `1 / "a"` reported "Unsupported binary
            // operation / for Number", blaming the one type `/` accepts and
            // saying nothing about the string that caused it.
            //
            // Naming both is the honest answer and needs no guess. The Concat
            // guard above distinguishes the sides because it tests each in
            // turn; this arm has already fallen through every such test.
            //
            // `right` is read through a null check rather than directly. It is
            // a public entry point, so a host can hand it a CLR null, and this
            // arm previously named only `this` and so never touched it - adding
            // the second name turned that call into a NullReferenceException,
            // which the failure ratchet caught. A missing value is spelled Null
            // here, which is what the caller meant by it.
            let rightType =
                if obj.ReferenceEquals(right, null) then JyroValueType.Null
                else right.ValueType
            raise (JyroRuntimeException(MessageCode.UnsupportedBinaryOperation,
                [| box (Operators.binaryOpToString op); box (TypeNames.typeOfName this.ValueType); box (TypeNames.typeOfName rightType) |]))

    // The unary rule every type shares: `not`, through `IsTruthy`; raises
    // `UnsupportedUnaryOperation` for `-`, which only `JyroNumber` has.
    member internal this.DefaultEvaluateUnary(op: UnaryOp) : JyroValue =
        match op with
        | Not -> JyroBoolean.FromBoolean(not (this.IsTruthy()))
        // Negate is subclass-specific (numbers); the base value type does
        // not support it.
        | Negate ->
            raise (JyroRuntimeException(MessageCode.UnsupportedUnaryOperation,
                [| box (Operators.unaryOpToString op); box (TypeNames.typeOfName this.ValueType) |]))

    /// <summary>
    /// Casts to <c>JyroObject</c>, raising <c>InvalidCast</c> for anything else.
    /// Overridden only by <c>JyroObject</c>, which returns itself.
    /// </summary>
    abstract member AsObject: unit -> JyroObject
    default this.AsObject() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "object" |]))

    /// <summary>
    /// Casts to <c>JyroArray</c>, raising <c>InvalidCast</c> for anything else.
    /// Overridden only by <c>JyroArray</c>, which returns itself.
    /// </summary>
    abstract member AsArray: unit -> JyroArray
    default this.AsArray() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "array" |]))

    /// <summary>
    /// Casts to <c>JyroString</c>, raising <c>InvalidCast</c> for anything else.
    /// Overridden only by <c>JyroString</c>, which returns itself.
    /// </summary>
    abstract member AsString: unit -> JyroString
    default this.AsString() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "string" |]))

    /// <summary>
    /// Casts to <c>JyroNumber</c>, raising <c>InvalidCast</c> for anything else.
    /// Overridden only by <c>JyroNumber</c>, which returns itself.
    /// </summary>
    abstract member AsNumber: unit -> JyroNumber
    default this.AsNumber() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "number" |]))

    /// <summary>
    /// Casts to <c>JyroBoolean</c>, raising <c>InvalidCast</c> for anything else.
    /// Overridden only by <c>JyroBoolean</c>, which returns itself.
    /// </summary>
    abstract member AsBoolean: unit -> JyroBoolean
    default this.AsBoolean() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "boolean" |]))

    /// <summary>
    /// Casts to <c>JyroFunction</c>, raising <c>InvalidCast</c> for anything else:
    /// the typed accessor for a lambda a host receives from <c>Output</c> or as an
    /// argument to a host function.
    /// </summary>
    abstract member AsFunction: unit -> JyroFunction
    default this.AsFunction() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "function" |]))

    /// <summary>
    /// Converts to a plain CLR value suitable for <c>FromObject</c>'s reverse
    /// direction - a boxed primitive, <c>Dictionary&lt;string, obj | null&gt;</c>, or
    /// <c>List&lt;obj | null&gt;</c> depending on the runtime type. <c>JyroNull</c> gives
    /// null, and so does each element or property of a container that holds
    /// the Jyro null. Every override but <c>JyroNull</c>'s gives a non-null value.
    /// </summary>
    abstract member ToObjectValue: unit -> obj | null
    /// <summary>
    /// Converts to the value's string form (not JSON) - the same text <c>+</c>
    /// and string coercion use. The default raises <c>InvalidCast</c>; every
    /// concrete type overrides it.
    /// </summary>
    abstract member ToStringValue: unit -> string
    default this.ToStringValue() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "string" |]))

    /// <summary>
    /// Narrows to <c>int</c>, reporting <c>IntegerOutOfRange</c> rather than raising a
    /// BCL overflow exception when the value has none. The default raises
    /// <c>InvalidCast</c>; only <c>JyroNumber</c> overrides it.
    /// </summary>
    abstract member ToInt32: unit -> int
    default this.ToInt32() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "number" |]))

    /// <summary>
    /// Narrows to <c>int64</c>. See <c>ToInt32</c> for the reporting rule. The default
    /// raises <c>InvalidCast</c>; only <c>JyroNumber</c> overrides it.
    /// </summary>
    abstract member ToInt64: unit -> int64
    default this.ToInt64() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "number" |]))

    /// <summary>
    /// Converts to <c>double</c>. The default raises <c>InvalidCast</c>; only
    /// <c>JyroNumber</c> overrides it.
    /// </summary>
    abstract member ToDouble: unit -> float
    default this.ToDouble() = raise (JyroRuntimeException(MessageCode.InvalidCast, [| box (TypeNames.typeOfName this.ValueType); box "number" |]))

    /// <summary>
    /// The value's truthiness: the rule the language applies to the condition
    /// of <c>if</c>, <c>while</c> and the ternary, and to the operands of <c>and</c>, <c>or</c> and
    /// <c>not</c>. Arrays and objects are truthy even when empty. Total - it never
    /// raises, whatever the value is.
    ///
    /// It is not a conversion: the <c>To...</c> members, <c>ToInt32</c>, <c>ToDouble</c> and
    /// <c>ToStringValue</c>, are strict conversions that raise <c>InvalidCast</c>. A host that
    /// wants "this must actually be a boolean" wants <c>AsBoolean</c>, which raises
    /// <c>InvalidCast</c> for anything that is not one.
    /// </summary>
    member this.IsTruthy() : bool =
        match this with
        | :? JyroNull -> false
        | :? JyroBoolean as b -> b.Value
        | :? JyroNumber as n ->
            // Every non-zero number is true (spec 30.9.2), the smallest
            // subnormal double included: this compared the magnitude with
            // `Double.Epsilon`, which IS that subnormal, so it was false.
            if n.IsExact then n.DecimalValue <> 0m else n.Value <> 0.0
        | :? JyroString as s -> not (String.IsNullOrEmpty(s.Value))
        | :? JyroArray -> true
        | :? JyroObject -> true
        | :? JyroFunction -> true
        | _ -> false

    /// <summary>
    /// Equality comparison. This is the script-level <c>==</c>: structural, so two
    /// distinct arrays with equal elements are equal, and bounded by the
    /// maximum nesting depth, so a cyclic operand raises
    /// <c>ValueNestingLimitExceeded</c> rather than recursing forever.
    ///
    /// It is deliberately not what <c>Object.Equals</c> does. <c>Equals</c> and
    /// <c>GetHashCode</c> are overridden on the four immutable scalars - null,
    /// boolean, number, string - and left as reference identity on <c>JyroArray</c>,
    /// <c>JyroObject</c> and <c>JyroFunction</c>. Two reasons, and both have to hold:
    ///
    /// - <c>GetHashCode</c> must not change while a value is a dictionary key.
    ///   Containers are mutable, so a structural hash would strand every entry
    ///   keyed by one the moment a script or host wrote to it.
    /// - <c>Equals</c> must not throw. <c>EqualsValue</c> raises on a cyclic operand, as
    ///   the language requires, so delegating would put a
    ///   throwing <c>Equals</c> into every BCL collection operation.
    ///
    /// The split also matches the language, whose containers are referenced
    /// rather than copied, so two structurally-equal arrays genuinely are two
    /// different objects. A host wanting the script's answer calls
    /// <c>EqualsValue</c>; a host wanting a dictionary key gets identity, which is
    /// the only thing stable enough to be one.
    /// </summary>
    abstract member EqualsValue: other: JyroValue -> bool

    /// <summary>
    /// Makes this value read-only, and every array and object reachable from
    /// it. Returns the same value, so a host can freeze inline.
    ///
    /// Declared on every value, so a host freezes a shared graph through the
    /// <c>JyroValue</c> it holds - the result of <c>FromJson</c>, the value of a
    /// result, <c>ctx.Input</c>, <c>ctx.Output</c> - without a downcast.
    ///
    /// On a scalar this is a no-op: null, boolean, number and string have no
    /// mutable state, so they are already safe to share.
    /// </summary>
    abstract member Freeze: unit -> JyroValue
    default this.Freeze() = this

    /// <summary>
    /// True when this value cannot be mutated. The scalars report <c>true</c>
    /// unconditionally - they have nothing to mutate - so across the whole
    /// hierarchy this reads as "safe to share", which is the question a host
    /// is actually asking.
    /// </summary>
    abstract member IsFrozen: bool
    default _.IsFrozen = true

    // The index of the first unpaired surrogate in `text`, or -1.
    //
    // A surrogate is legal on its own as a UTF-16 code unit and illegal on its
    // own as a character, so a string can hold one and no conversion to UTF-8
    // can carry it. `Encoding.UTF8.GetBytes` and `Utf8JsonWriter` both replace
    // it with U+FFFD without saying so.
    //
    // Scanned at the encoding step rather than checked at construction. That
    // is the decision recorded in AUDIT.md: unlike a non-finite number, a lone
    // surrogate is a perfectly good *value* - it orders, compares and equals
    // itself - so the thing that cannot carry it is the encoding, not the
    // language, and the encoding is where it reports. Checking at construction
    // would also have meant `Substring` refusing to split an emoji, which is a
    // separate question about what a string index means.
    static member internal FirstUnpairedSurrogate(text: string) : int =
        if obj.ReferenceEquals(text, null) then -1
        else
            let mutable i = 0
            let mutable found = -1
            let mutable nextCheck = ExecutionCheck.ChunkLength
            while found < 0 && i < text.Length do
                // A check every chunk of code units (spec 100.2.5).
                if i >= nextCheck then
                    ExecutionCheck.Check()
                    nextCheck <- i + ExecutionCheck.ChunkLength
                let c = text.[i]
                if Char.IsHighSurrogate c then
                    if i + 1 < text.Length && Char.IsLowSurrogate text.[i + 1] then i <- i + 2
                    else found <- i
                elif Char.IsLowSurrogate c then found <- i
                else i <- i + 1
            found

    // Refuses a null writer, naming the parameter.
    //
    // A `Utf8JsonWriter` is a resource, not a value, so the settled null rule
    // refuses it rather than normalizing. Every `WriteJson` override calls
    // this first: each dereferences the writer immediately, so without it the
    // failure was a `NullReferenceException` - which names nothing, and is not
    // a type a host is told to catch.
    //
    // One helper rather than the check written out eight times, for the same
    // reason `failReadOnly` and `OfComputed` are single points: the decision
    // and its wording live in one place and cannot drift apart.
    static member internal RequireWriter(writer: Utf8JsonWriter) =
        if obj.ReferenceEquals(writer, null) then
            raise (ArgumentNullException("writer"))

    /// <summary>
    /// Writes this value as JSON. A null writer throws
    /// <c>ArgumentNullException</c> naming <c>writer</c>.
    /// </summary>
    abstract member WriteJson: Utf8JsonWriter -> unit
    default _.WriteJson(writer) =
        JyroValue.RequireWriter writer
        writer.WriteNullValue()

    /// <summary>
    /// Renders this value as a JSON string.
    ///
    /// A host conversion: it raises <c>ValueSizeLimitExceeded</c>
    /// when the size of the value is above the maximum value
    /// size, and <c>ValueNestingLimitExceeded</c> when the value is cyclic or too
    /// deep, before it writes anything.
    /// </summary>
    member this.ToJson() : string = ValueWalk.HostJson(this)

    /// <summary>
    /// Converts an arbitrary CLR value to a <c>JyroValue</c> by its shape:
    /// a number of any number type of the host platform, a string, a
    /// <c>JsonElement</c>, a <c>JsonNode</c> or a Newtonsoft.Json <c>JToken</c> as its JSON
    /// (a <c>JsonValue</c> made from a host value as that value), an
    /// <c>XObject</c> or an <c>XmlNode</c> as its XML text, a dictionary whose key type is
    /// string, a sequence of any element
    /// type (a dictionary whose key type is not string among them, as an
    /// array of <c>{Key, Value}</c> entries), a value of a closed list of
    /// text-form types, or any other object by its public properties and
    /// fields. Every number of the walk goes through the
    /// <c>JyroNumber</c> double constructor, so a NaN or an infinity at any depth
    /// raises <c>HostNumberNaN</c> or <c>HostNumberInfinite</c>.
    ///
    /// It is the one public member that makes a Jyro value from a host value
    /// of any type. A value that is, or holds at any
    /// depth, a delegate, reflection metadata (<c>System.Type</c> and every other
    /// <c>MemberInfo</c>, <c>Assembly</c>, <c>Module</c>, <c>ParameterInfo</c>) or a task (<c>Task</c>,
    /// every <c>Task&lt;T&gt;</c>, <c>ValueTask</c>, <c>ValueTask&lt;T&gt;</c>) is refused with a
    /// <c>JyroRuntimeException</c> of the code <c>HostValueNotConvertible</c> and no
    /// argument, and no member of it is read. The first refused value or
    /// non-finite number the walk meets decides the code.
    ///
    /// Conversion from a host value is a walk the maximum nesting depth
    /// applies to:
    /// a host graph deeper than the maximum, or a cyclic one, raises a
    /// <c>JyroRuntimeException</c> with <c>ValueNestingLimitExceeded</c>.
    ///
    /// It reads an object by the public properties and fields of its type,
    /// which trimming may remove, so it warns a host that trims: such a host
    /// keeps the members of the types it converts, or passes JSON or a
    /// <c>JyroValue</c> instead.
    /// </summary>
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member FromObject(value: obj | null) : JyroValue =
        match value with
        | null -> JyroNull.Instance :> JyroValue
        | value -> JyroValue.FromHostValue(value)

    // `FromObject` of a value that is not null.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromHostValue(value: obj) : JyroValue =
        match value with
        | :? JyroValue as jv -> jv
        | :? bool as b -> JyroBoolean.FromBoolean(b) :> JyroValue
        // Every CLR integral primitive, not the two that happened to be here.
        // The chain used to name `int` and `int64` only, so a host field typed
        // `byte`, `short`, `uint` or `ulong` fell through to the stringify
        // fallback and reached the script as a *string* - `Input.n + 1` was
        // then concatenation or a type error rather than arithmetic. Exact,
        // because an integer read from a host field is a whole number and
        // decimal is where this engine keeps those. `uint64` fits: decimal
        // reaches ~7.9e28, well past 1.8e19.
        | :? int as i -> JyroNumber(decimal i) :> JyroValue
        | :? int64 as l -> JyroNumber(decimal l) :> JyroValue
        | :? sbyte as i -> JyroNumber(decimal i) :> JyroValue
        | :? byte as i -> JyroNumber(decimal i) :> JyroValue
        | :? int16 as i -> JyroNumber(decimal i) :> JyroValue
        | :? uint16 as i -> JyroNumber(decimal i) :> JyroValue
        | :? uint32 as i -> JyroNumber(decimal i) :> JyroValue
        | :? uint64 as i -> JyroNumber(decimal i) :> JyroValue
        // A NaN or an infinity is refused, not normalized (spec 30.2.17,
        // JYRO-ADR-5, JYRO-ADR-35): the double constructor raises
        // `HostNumberNaN` or `HostNumberInfinite`. It used to become null,
        // which hid the error in the host's data. The walk of a host value is
        // depth-first and raises at the first such number it meets, so that
        // number decides the code (spec 30.2.19). `WithInput(obj)` and the
        // other members that take `Input` report it in the result instead
        // (spec 30.2.18).
        | :? float as f -> JyroNumber(f) :> JyroValue
        // `float32` reached none of this and stringified, so `Single.NaN`
        // arrived as the literal string "NaN" - the exact outcome the
        // finiteness rule exists to prevent, through the one door with no
        // finiteness check. Widened to double rather than rounded to decimal: a
        // float32 is a binary float and stays one, where reading `0.1f` back as
        // the exact 0.1 would claim precision the value does not carry. A
        // `Single` NaN or infinity widens to the double one, and is refused.
        | :? float32 as f -> JyroNumber(float f) :> JyroValue
        | :? decimal as d -> JyroNumber(d) :> JyroValue
        | :? string as s -> JyroString(s) :> JyroValue
        // A delegate, reflection metadata or a task is refused (JYRO-ADR-200
        // item 2), decided by its type before any member of it is read: a
        // pending `Task<T>` is never asked for its `Result`, so nothing
        // blocks. The walk is depth-first and raises at the first refused
        // value or non-finite number it meets, so that one decides the code.
        | _ when HostShapes.isRefused (value.GetType()) ->
            JyroError.raiseRuntime MessageCode.HostValueNotConvertible Array.empty<obj>
        // Every other .NET number type, decided by interface (JYRO-ADR-156
        // (a), JYRO-ADR-158 (a2)): `BigInteger`, `Int128`, `UInt128`, `nint`,
        // `nuint`, `Half`, `NFloat`, and a type outside the BCL. The arms
        // above are this rule for the commonest types, without the lookup.
        | _ when HostShapes.numberKind (value.GetType()) <> HostShapes.NumberKind.NotNumber ->
            JyroValue.FromHostNumber(value)
        | :? JsonElement as je -> JyroValue.FromJsonElement(je)
        // A `JsonNode` is the JSON it holds, as a `JsonElement` is
        // (JYRO-ADR-156 (b)); before the dictionary arm, which a `JsonObject`
        // would otherwise take, and before its `Parent` and `Root` are ever
        // reflected.
        | :? System.Text.Json.Nodes.JsonNode as node -> JyroValue.FromJsonNode(node)
        // A Newtonsoft.Json `JToken` is its JSON (JYRO-ADR-157 (b2)),
        // recognized by its type's name; before the dictionary and sequence
        // arms, which would make every `JValue`, a sequence of no children,
        // an empty array.
        | _ when HostShapes.jTokenKind (value.GetType()) <> HostShapes.JTokenKind.NotJToken ->
            JyroValue.FromJToken(value)
        // LINQ to XML and System.Xml nodes are their XML text (JYRO-ADR-157
        // (b3)), ahead of the sequence arm, which an `XmlNode` would take as
        // the sequence of its children, and of the reflection of an
        // `XNode`'s `Parent` and `Document`, which lead back up the tree.
        | :? System.Xml.Linq.XNode as x ->
            JyroString(x.ToString(System.Xml.Linq.SaveOptions.DisableFormatting)) :> JyroValue
        | :? System.Xml.Linq.XObject as x ->
            JyroString(x.ToString() |> Option.ofObj |> Option.defaultValue "") :> JyroValue
        | :? System.Xml.XmlNode as x -> JyroString(x.OuterXml) :> JyroValue
        // A dictionary (JYRO-ADR-152 (1), JYRO-ADR-154): an object of its
        // entries, in the order the host enumerates them (spec 30.5.10), each
        // value converted by this walk. Decided by the type, not by the keys
        // it holds: `IDictionary<string, obj>` first, which needs no
        // reflection; then `IDictionary<string, T>` or
        // `IReadOnlyDictionary<string, T>` for any other `T` - .NET variance
        // does not reach a `KeyValuePair`, so `Dictionary<string, double>` is
        // not `IDictionary<string, obj>`. A type that implements a generic
        // dictionary interface only for a key type that is not string
        // (`Dictionary<int, T>`, `Dictionary<object, T>`) is a sequence of its
        // entries, empty or not, below (1b). Only a type that implements none
        // of them, such as `Hashtable`, is decided by its keys (1c).
        | :? IDictionary<string, obj | null> as dict -> JyroObject.FromDictionary(dict) :> JyroValue
        | _ when not (isNull (HostShapes.dictionaryValueType (value.GetType()))) ->
            JyroValue.FromTypedDictionary(value, nonNull (HostShapes.dictionaryValueType (value.GetType())))
        | :? System.Collections.IDictionary as dict
            when not (HostShapes.isGenericDictionary (dict.GetType())) && JyroValue.HasOnlyStringKeys dict ->
            JyroObject.FromNonGenericDictionary(dict) :> JyroValue
        // A sequence of any element type (JYRO-ADR-152 (2)): an array of its
        // elements in enumeration order, each converted by this walk.
        | :? IEnumerable<obj | null> as seq -> JyroArray.FromEnumerable(seq) :> JyroValue
        | :? System.Collections.IEnumerable as seq ->
            let items = ResizeArray<obj | null>()
            for item in seq do items.Add(item)
            JyroArray.FromEnumerable(items) :> JyroValue
        // The entry of a dictionary that is a sequence: exactly Key then
        // Value, each converted by this walk, so the first-number walk of
        // spec 30.2.19 visits the Key first (JYRO-ADR-154 (1d)).
        | :? System.Collections.DictionaryEntry as e -> JyroObject.FromEntry(e.Key, e.Value) :> JyroValue
        | _ when HostShapes.isKeyValuePair (value.GetType()) -> JyroValue.FromKeyValuePair(value)
        | other -> JyroValue.FromUnrecognized(other)

    // A `KeyValuePair<K, T>`, read by `JyroObject.FromPair<K, T>` through a
    // delegate made once per pair type, so that its `Key` and `Value` are read
    // as the members they are, not found by reflection over the pair's type.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromKeyValuePair(value: obj) : JyroValue =
        let t = value.GetType()
        let convert =
            match HostShapes.pairConverters.TryGetValue t with
            | true, found -> found
            | _ ->
                let name = nameof JyroObject.FromPair<obj, obj>
                let m =
                    match
                        typeof<JyroObject>.GetMethod(
                            nameof JyroObject.FromPair<obj, obj>,
                            Reflection.BindingFlags.Static ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Public)
                    with
                    | null ->
                        raise (EngineBindingException(
                                $"The engine member {typeof<JyroObject>.FullName}.{name} that converts a key-value pair was not found."))
                    | m -> m
                HostShapes.pairConverters.GetOrAdd(
                    t, Delegate.CreateDelegate(typeof<Func<obj, JyroValue>>, m.MakeGenericMethod(t.GetGenericArguments())))
        (convert :?> Func<obj, JyroValue>).Invoke(value)

    // A host number of a type the arms of `FromObject` do not name
    // (JYRO-ADR-156 (a), JYRO-ADR-157 (a1)), read only through members its
    // interface requires, so an exception one of them throws passes out
    // unchanged. An integer is exact when it lies within decimal's
    // range, and otherwise its nearest double, through the double
    // constructor, so one whose nearest double is an infinity raises
    // `HostNumberInfinite`. A binary float is widened to double, through the
    // same constructor (JYRO-ADR-158 (a3)), and a decimal float is exact or
    // its nearest double (JYRO-ADR-158 (a4)).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromHostNumber(value: obj) : JyroValue =
        match HostShapes.numberKind (value.GetType()) with
        | HostShapes.NumberKind.BinaryFloat -> JyroNumber(HostShapes.binaryFloatToDouble value) :> JyroValue
        // A radix-10 float is exact when its value is a decimal exactly, and
        // otherwise its nearest double, as a wide integer is (JYRO-ADR-158
        // (a4)); a NaN or an infinity is refused by the double constructor.
        | HostShapes.NumberKind.DecimalFloat ->
            match HostShapes.decimalFloatToNumber value with
            | :? decimal as d -> JyroNumber(d) :> JyroValue
            | n -> JyroNumber(unbox<float> n) :> JyroValue
        | _ ->
            // Read through the members `IBinaryInteger<T>` requires of every
            // implementer, as a `BigInteger`, with no text (JYRO-ADR-157
            // (a1)). Exact when its magnitude is below 2^96, the range of
            // decimal; otherwise its nearest double, rounded here because the
            // platform's own conversion from `BigInteger` to double
            // truncates. Past 2^1024 that is an infinity, which the double
            // constructor refuses.
            let b = HostShapes.integerToBigInteger value
            let magnitude = Numerics.BigInteger.Abs b
            if magnitude.GetBitLength() <= 96L then JyroNumber(decimal b) :> JyroValue
            else
                let nearest = HostShapes.nearestDouble magnitude Numerics.BigInteger.Zero
                JyroNumber(if b.Sign < 0 then -nearest else nearest) :> JyroValue

    // A `System.Text.Json.Nodes.JsonNode`, as the same JSON as a
    // `JsonElement` (JYRO-ADR-156 (b)): a `JsonObject` an object of its
    // properties in their order, a `JsonArray` an array, a parsed `JsonValue`
    // its JSON, numbers read as the JSON doors read them (spec 30.2.7), and a
    // `JsonValue` made from a .NET value that value, by this walk
    // (JYRO-ADR-157 (b1)). Its `Parent`, `Root` and `Options` are navigation and
    // configuration, not data, and are never walked.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromJsonNode(node: System.Text.Json.Nodes.JsonNode) : JyroValue =
        match node with
        | :? System.Text.Json.Nodes.JsonObject as o ->
            ValueRecursionGuard.Enter()
            try
                let result = JyroObject()
                for kvp in o do
                    result.SetProperty(kvp.Key, JyroValue.FromObject(kvp.Value))
                result :> JyroValue
            finally
                ValueRecursionGuard.Exit()
        | :? System.Text.Json.Nodes.JsonArray as a ->
            let items = ResizeArray<obj | null>(a.Count)
            for item in a do items.Add(item)
            JyroArray.FromEnumerable(items) :> JyroValue
        | _ ->
            let v = node.AsValue()
            let mutable element = Unchecked.defaultof<JsonElement>
            match v.TryGetValue<JsonElement>(&element) with
            // A parsed node holds the element it was read from.
            | true -> JyroValue.FromJsonElement(element)
            | _ ->
                // A node made from a .NET value (`JsonValue.Create<T>`) is
                // that value, converted by this walk exactly as it is passed
                // directly (JYRO-ADR-157 (b1)): its shape is the walk's, and a
                // NaN, an infinity, a depth or a cycle in it gives the walk's
                // code. Never written through System.Text.Json's serializer,
                // whose contract and exceptions are not the engine's.
                match v.TryGetValue<obj>() with
                | true, held -> JyroValue.FromObject(held)
                | _ ->
                    // Every JsonValue the platform makes gives up either its
                    // element or its value; one that gives up neither is a
                    // JSON primitive the platform holds as text, written here
                    // and read as the JSON doors read it. No serializer runs
                    // for it.
                    let buffer = ArrayBufferWriter<byte>()
                    do
                        use writer = new Utf8JsonWriter(buffer)
                        node.WriteTo(writer)
                    use document =
                        JsonDocument.Parse(buffer.WrittenMemory, JsonDocumentOptions(MaxDepth = JyroValue.JsonReaderMaxDepth))
                    JyroValue.FromJsonElement(document.RootElement)

    // A Newtonsoft.Json `JToken`, as its JSON (JYRO-ADR-157 (b2)): a
    // `JObject` an object of its properties in their order, a `JArray` an
    // array, a `JValue` its `Value` converted by this walk (so a NaN in it
    // is refused as any host NaN is), null for a `JTokenType.Null` or
    // `Undefined`; any other token (a `JRaw`, a comment, a `JConstructor`,
    // a `JProperty` passed on its own) its text. A `JProperty` is otherwise
    // reached only as an entry of its `JObject`.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromJToken(value: obj) : JyroValue =
        let asText () = JyroString(value.ToString() |> Option.ofObj |> Option.defaultValue "") :> JyroValue
        match HostShapes.jTokenKind (value.GetType()) with
        | HostShapes.JTokenKind.Object ->
            // `JObject` implements `IDictionary<string, JToken>`, which
            // enumerates its properties in their order.
            match HostShapes.dictionaryValueType (value.GetType()) with
            | null -> asText ()
            | valueType -> JyroValue.FromTypedDictionary(value, valueType)
        | HostShapes.JTokenKind.Array ->
            // `JArray` is an `IEnumerable<JToken>` of its elements.
            JyroArray.FromEnumerable(value :?> IEnumerable<obj | null>) :> JyroValue
        | HostShapes.JTokenKind.Value ->
            let struct (tokenType, held) = HostShapes.jValueParts value
            match tokenType with
            | "Null"
            | "Undefined" -> JyroNull.Instance :> JyroValue
            | "Comment"
            | "Raw" -> asText ()
            | _ -> JyroValue.FromObject(held)
        | _ -> asText ()

    // Whether every key of a non-generic dictionary is a string.
    static member private HasOnlyStringKeys(dict: System.Collections.IDictionary) : bool =
        let mutable all = true
        let keys = dict.Keys.GetEnumerator()
        while all && keys.MoveNext() do
            all <- keys.Current :? string
        all

    // An `IDictionary<string, T>` or `IReadOnlyDictionary<string, T>` with
    // `T` = `valueType`, converted by `JyroObject.FromEntries<T>`, whose
    // delegate is made once per `T`.
    // A name that matches no method fails naming the type and the method
    // (JYRO-ADR-206).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromTypedDictionary(value: obj, valueType: Type) : JyroValue =
        let convert =
            match HostShapes.dictionaryConverters.TryGetValue valueType with
            | true, found -> found
            | _ ->
                let name = nameof JyroObject.FromEntries<obj>
                let m =
                    match
                        typeof<JyroObject>.GetMethod(
                            nameof JyroObject.FromEntries<obj>,
                            Reflection.BindingFlags.Static ||| Reflection.BindingFlags.NonPublic ||| Reflection.BindingFlags.Public)
                    with
                    | null ->
                        raise (EngineBindingException(
                                $"The engine member {typeof<JyroObject>.FullName}.{name} that converts a typed dictionary was not found."))
                    | m -> m
                HostShapes.dictionaryConverters.GetOrAdd(
                    valueType, Delegate.CreateDelegate(typeof<Func<obj, JyroValue>>, m.MakeGenericMethod([| valueType |])))
        (convert :?> Func<obj, JyroValue>).Invoke(value)

    // The fallback arm of `FromObject`: a value that is not a number, a
    // string, a JSON value, a dictionary or a sequence (JYRO-ADR-152 (3),
    // (4), as amended by JYRO-ADR-156 (c) to (f)).
    //
    // A value of one of a closed list of types becomes its text: `DateTime`,
    // `DateTimeOffset`, `DateOnly`, `TimeOnly`, `TimeSpan`, `Guid`, `Uri`,
    // `Version`, `char`, `StringBuilder`, `CultureInfo`, and an enum value
    // (its name); a delegate, reflection metadata and a task never reach
    // here, `FromObject` refuses them (JYRO-ADR-200 item 2); an `XObject`
    // or an `XmlNode` is its XML text, matched in `FromObject` ahead of the
    // sequence arm (JYRO-ADR-157 (b3)). Every other
    // object - a class, a struct, a C# record, an anonymous type, a
    // `ValueTuple` - becomes an object of its public readable instance
    // properties followed by its public instance fields, each converted by
    // this walk, whether or not its type overrides `ToString`. Deciding by an
    // override of `ToString` turned every C# record, anonymous object, tuple
    // and typed dictionary entry into a string, which hid a NaN in its text
    // (JYRO-DEF-602) and lost the shape of finite data (JYRO-DEF-607):
    // compilers generate that override for data types.
    //
    // A property whose getter throws is left out, and the object stays an
    // object (JYRO-ADR-156 (e)), so whether a value becomes an object or its
    // text is decided by its type alone: only a type with no public readable
    // property and no public field becomes its text (f).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member private FromUnrecognized(value: obj) : JyroValue =
        // A host type is free to return null from ToString(). JyroString's
        // constructor rejects null, so without this defaulting `FromObject`
        // would throw ArgumentNullException for a value whose only fault is
        // that its text form is null - a host value no requirement refuses.
        let asString () =
            JyroString(value.ToString() |> Option.ofObj |> Option.defaultValue "") :> JyroValue
        let t = value.GetType()
        if HostShapes.isTextForm t then asString ()
        // A back-reference: this object is already being reflected further up
        // the same walk, so the path of the walk never ends and would exceed
        // the maximum nesting depth. That walk raises
        // `ValueNestingLimitExceeded` (spec 100.7.5, 100.7.6), as a cyclic
        // dictionary or list does; raising here, rather than recursing until
        // the depth guard trips, keeps the heavy reflection frames off the
        // stack. It used to render the object as its text form, which turned
        // a cyclic host graph into a value that silently differed from it.
        // No public readable property and no public field: reflection would
        // find nothing to convert, and the text is the only information the
        // type gives (JYRO-ADR-156 (f)).
        elif not (HostShapes.hasMembers t) then asString ()
        elif HostGraphCycleGuard.IsVisiting(value) then
            JyroError.raiseRuntime MessageCode.ValueNestingLimitExceeded
                [| box ValueRecursionGuard.MaxDepth |]
        else
            // Annotated because `JyroObject` is declared later in this
            // recursive type group, so its members are not yet inferred here.
            let converted : JyroObject = JyroObject.FromComplexObject(value)
            converted :> JyroValue

    /// <summary>
    /// Parses JSON text (RFC 8259). Strict: a trailing comma, a comment, empty
    /// text and text after the value are not JSON, and raise <c>InvalidJson</c>.
    ///
    /// The text is read from its start, a token at a time, and the first
    /// problem met decides: an array or
    /// object opened past the maximum nesting depth raises
    /// <c>ValueNestingLimitExceeded</c> at its bracket, whatever follows it, and
    /// text that stops being JSON before that point raises <c>InvalidJson</c>.
    ///
    /// Raises a <c>JyroRuntimeException</c> carrying the code: this member returns
    /// a <c>JyroValue</c> and has nowhere to put a diagnostic. The runner members
    /// that read JSON report it through their result instead - see
    /// <c>JyroRunner.WithInputJson</c>.
    /// </summary>
    static member FromJson(json: string) : JyroValue =
        if obj.ReferenceEquals(json, null) then raise (ArgumentNullException(nameof json))
        JyroValue.ReadHostText(json, JyroValue.HostReaderOptions)

    // The depth given to every `System.Text.Json` reader of the engine.
    //
    // The readers' own default of 64 used to stop JSON text at 64 levels with
    // `InvalidJson`, below the one maximum nesting depth that every walk of a
    // value applies (spec 100.7.1, 100.7.5): a value the engine accepts from a
    // script could not come in as JSON, and text deeper than the maximum was
    // refused as malformed rather than as too deep. The reader now leaves
    // depth to `ReadDocument`, which raises `ValueNestingLimitExceeded` on
    // opening a container past the maximum (spec 100.7.6). The readers parse
    // iteratively, so a deep text costs them memory in proportion to its
    // length, not native stack.
    static member internal JsonReaderMaxDepth = Int32.MaxValue

    // The dialect of the host's JSON doors: JSON as RFC 8259 gives it, with
    // no trailing comma and no comment (spec 160.1, JYRO-REQ-1657). They used
    // to accept both, and the stream doors also took a second value after the
    // first.
    static member private HostReaderOptions =
        JsonReaderOptions(
            AllowTrailingCommas = false,
            CommentHandling = JsonCommentHandling.Disallow,
            MaxDepth = JyroValue.JsonReaderMaxDepth)

    /// <summary>
    /// Reads the UTF-8 JSON a stream holds, read to its end, and gives the
    /// value <c>FromJson(string)</c> gives for the same text. A null stream is a
    /// missing resource rather than a value, so it throws <c>ArgumentNullException</c>.
    /// </summary>
    static member FromJson(utf8Json: Stream) : JyroValue =
        if obj.ReferenceEquals(utf8Json, null) then raise (ArgumentNullException(nameof utf8Json))
        JyroValue.ReadHostSequence(JyroValue.ReadStreamToSequence(utf8Json))

    /// <summary>
    /// Reads UTF-8 JSON text and gives the value, or throws the exception,
    /// that <c>FromJson(string)</c> gives for the same text.
    /// </summary>
    /// <param name="utf8Json">The JSON text, encoded as UTF-8.</param>
    /// <returns>The value the text holds.</returns>
    static member FromJson(utf8Json: ReadOnlySpan<byte>) : JyroValue =
        JyroValue.ReadHostSequence(System.Buffers.ReadOnlySequence<byte>(utf8Json.ToArray()))

    /// <summary>
    /// Asynchronous counterpart of <c>FromJson(Stream)</c>, with the same null
    /// contract: the task completes with the value that <c>FromJson</c> gives for
    /// the stream's text, and ends as canceled, with no value, when
    /// <c>cancellationToken</c> is canceled before the stream has been read.
    ///
    /// A null stream throws <c>ArgumentNullException</c> at the call, not
    /// through the task, as the synchronous member does.
    /// </summary>
    static member FromJsonAsync(utf8Json: Stream, cancellationToken: System.Threading.CancellationToken)
        : System.Threading.Tasks.Task<JyroValue> =
        if obj.ReferenceEquals(utf8Json, null) then raise (ArgumentNullException(nameof utf8Json))
        if cancellationToken.IsCancellationRequested then
            System.Threading.Tasks.Task.FromCanceled<JyroValue>(cancellationToken)
        else
            task {
                let! sequence = JyroValue.ReadStreamToSequenceAsync(utf8Json, cancellationToken)
                return JyroValue.ReadHostSequence(sequence)
            }

    // Raises `InvalidJson`, for text that is not JSON. It has no argument:
    // the reader's message is a sentence of the host platform, which is not
    // an argument (spec 130.6.2).
    static member private InvalidJson(_reason: exn) : JyroValue =
        JyroError.raiseRuntime MessageCode.InvalidJson Array.empty

    // The host's text, read by `ReadDocument`.
    //
    // The text is UTF-16 and the reader reads UTF-8, so it is encoded first.
    // An unpaired surrogate has no UTF-8 encoding, and the text is not JSON
    // from it on, so only the text before it is encoded, and read as a text
    // that does not end there: the reader meets whatever comes first, a
    // container too deep or the end of what it was given.
    static member private ReadHostText(json: string, options: JsonReaderOptions) : JyroValue =
        let mutable unpaired = -1
        let mutable i = 0
        while unpaired < 0 && i < json.Length do
            let c = json.[i]
            if Char.IsHighSurrogate c && i + 1 < json.Length && Char.IsLowSurrogate json.[i + 1] then i <- i + 2
            elif Char.IsSurrogate c then unpaired <- i
            else i <- i + 1
        let whole = unpaired < 0
        let bytes = Text.Encoding.UTF8.GetBytes(json, 0, (if whole then json.Length else unpaired))
        try
            let mutable reader = Utf8JsonReader(ReadOnlySpan<byte>(bytes), whole, JsonReaderState(options))
            JyroValue.ReadDocument(&reader, null, whole)
        with
        | :? JsonException as ex -> JyroValue.InvalidJson ex
        // A `\u` escape of an unpaired surrogate, which `GetString` cannot
        // make a string of.
        | :? InvalidOperationException as ex -> JyroValue.InvalidJson ex

    // A host stream's bytes, read by `ReadDocument`.
    //
    // Exists so the stream doors can wrap their read in a `try`: a
    // `Utf8JsonReader` is a ref struct and cannot be held across a handler, so
    // the reader has to be confined to a call the handler wraps.
    static member private ReadHostSequence(sequence: System.Buffers.ReadOnlySequence<byte>) : JyroValue =
        try
            let mutable reader = Utf8JsonReader(sequence, JyroValue.HostReaderOptions)
            JyroValue.ReadDocument(&reader, null, true)
        with
        | :? JsonException as ex -> JyroValue.InvalidJson ex
        | :? InvalidOperationException as ex -> JyroValue.InvalidJson ex

    static member private ReadStreamToSequence(stream: Stream) : System.Buffers.ReadOnlySequence<byte> =
        let chunkSize = 64 * 1024
        let segments = ResizeArray<byte[]>()
        let mutable totalRead = 0L
        let mutable keepReading = true
        while keepReading do
            let buffer = Array.zeroCreate<byte> chunkSize
            let mutable offset = 0
            let mutable chunkDone = false
            while not chunkDone do
                let bytesRead = stream.Read(buffer, offset, chunkSize - offset)
                if bytesRead = 0 then
                    chunkDone <- true
                    keepReading <- false
                else
                    offset <- offset + bytesRead
                    if offset = chunkSize then chunkDone <- true
            if offset > 0 then
                if offset < chunkSize then
                    segments.Add(buffer.[..offset-1])
                else
                    segments.Add(buffer)
                totalRead <- totalRead + int64 offset
        if segments.Count = 0 then
            System.Buffers.ReadOnlySequence<byte>.Empty
        elif segments.Count = 1 then
            System.Buffers.ReadOnlySequence<byte>(System.ReadOnlyMemory(segments.[0]))
        else
            JyroValue.CreateChainedSequence(segments)

    static member private ReadStreamToSequenceAsync(stream: Stream, cancellationToken: System.Threading.CancellationToken)
        : System.Threading.Tasks.Task<System.Buffers.ReadOnlySequence<byte>> =
        task {
            let chunkSize = 64 * 1024
            let segments = ResizeArray<byte[]>()
            let mutable totalRead = 0L
            let mutable keepReading = true
            while keepReading do
                let buffer = Array.zeroCreate<byte> chunkSize
                let mutable offset = 0
                let mutable chunkDone = false
                while not chunkDone do
                    let! bytesRead = stream.ReadAsync(buffer, offset, chunkSize - offset, cancellationToken)
                    if bytesRead = 0 then
                        chunkDone <- true
                        keepReading <- false
                    else
                        offset <- offset + bytesRead
                        if offset = chunkSize then chunkDone <- true
                if offset > 0 then
                    if offset < chunkSize then
                        segments.Add(buffer.[..offset-1])
                    else
                        segments.Add(buffer)
                    totalRead <- totalRead + int64 offset
            if segments.Count = 0 then
                return System.Buffers.ReadOnlySequence<byte>.Empty
            elif segments.Count = 1 then
                return System.Buffers.ReadOnlySequence<byte>(System.ReadOnlyMemory(segments.[0]))
            else
                return JyroValue.CreateChainedSequence(segments)
        }

    static member private CreateChainedSequence(segments: ResizeArray<byte[]>) : System.Buffers.ReadOnlySequence<byte> =
        let first = ChunkedMemorySegment(System.ReadOnlyMemory(segments.[0]), 0L)
        let mutable current = first
        for i in 1 .. segments.Count - 1 do
            let offset = current.RunningIndex + int64 current.Memory.Length
            let next = ChunkedMemorySegment(System.ReadOnlyMemory(segments.[i]), offset)
            current.SetNext(next)
            current <- next
        System.Buffers.ReadOnlySequence<byte>(first, 0, current, current.Memory.Length)

    // The number at the reader: exact when the exact representation holds
    // its value with no rounding, and otherwise the double nearest to it
    // (spec 30.2.4, 30.2.7, JYRO-ADR-4). `TryGetDecimal` is not asked: it
    // rounds a 29-digit fraction, or `1e-29`, to 28 digits and reports
    // success.
    //
    // The token is read where the reader holds it, its span or the segments
    // of its sequence, a chunk at a time with the execution check; it is not
    // first made a string, and no platform call reads it whole
    // (JYRO-ADR-151 (5)(a)). The nearest double is parsed from a text of
    // fixed bound (5)(b), and the number as the text writes it is copied, a
    // chunk at a time, only for the diagnostic of a number that is not
    // finite (5)(c).
    static member private NumberToken(reader: byref<Utf8JsonReader>) : JyroValue =
        let digits =
            if reader.HasValueSequence then ExactNumber.readSequence reader.ValueSequence
            else ExactNumber.readBytes reader.ValueSpan
        match digits.Exact with
        | ValueSome d -> JyroNumber(d) :> JyroValue
        | ValueNone ->
            let nearest = digits.Nearest
            if Double.IsFinite nearest then JyroNumber(nearest) :> JyroValue
            else
                // JSON's number grammar is ASCII, so a byte is a character.
                let text =
                    if reader.HasValueSequence then ExactNumber.asciiTextOfSequence reader.ValueSequence
                    else ExactNumber.asciiText reader.ValueSpan
                JyroValue.FiniteFromJson(nearest, text)

    // The Jyro number of the JSON number written as `text`, as
    // `NumberToken` reads it, for a reader that holds no `Utf8JsonReader`.
    static member private NumberFromJsonText(text: string) : JyroValue =
        let digits = ExactNumber.readIn text 0 text.Length
        match digits.Exact with
        | ValueSome d -> JyroNumber(d) :> JyroValue
        | ValueNone -> JyroValue.FiniteFromJson(digits.Nearest, text)

    // Whether a JSON number token, read where the reader holds it, is
    // within the range of a double, decided from the bounded text of
    // JYRO-ADR-151 (5)(b), not by parsing the whole token (5)(d).
    static member private IsFiniteToken(reader: byref<Utf8JsonReader>) : bool =
        let digits =
            if reader.HasValueSequence then ExactNumber.readSequence reader.ValueSequence
            else ExactNumber.readBytes reader.ValueSpan
        Double.IsFinite digits.Nearest

    // The string at the reader. For a script (`script`, the bytes its reader
    // reads; null for the host), null when it is longer than the maximum
    // value size (spec 100.8.7).
    //
    // A script's token longer than one chunk is unescaped and decoded by
    // `BoundedText.jsonString` a chunk at a time with a check between chunks,
    // not by one `GetString` or `CopyString` over the whole token (spec
    // 100.2.5, JYRO-ADR-101 (5)); a shorter one costs a fixed amount and is
    // taken with `GetString`.
    static member private StringToken(reader: byref<Utf8JsonReader>, script: byte[] | null) : string | null =
        match script with
        | NonNull script when not reader.HasValueSequence && reader.ValueSpan.Length > ExecutionCheck.ChunkLength ->
            let value = reader.ValueSpan
            let mutable offset = 0
            if not (ReadOnlySpan<byte>(script).Overlaps(value, &offset)) then
                // An engine failure, not a verdict on the text: not an
                // `InvalidOperationException`, which `FromJsonBounded` reads
                // as text that is not JSON (JYRO-DEF-708).
                failwith "The token is not in the script's text."
            BoundedText.jsonString script offset value.Length (int64 ValueSize.Maximum)
        | _ -> reader.GetString() |> Option.ofObj |> Option.defaultValue ""

    // Whether a script's text (`text`, the bytes its reader reads), which the
    // reader has just refused, exceeds the maximum value size (spec 100.8.7)
    // before the point at which it stops being JSON (JYRO-REQ-1656).
    //
    // `Utf8JsonReader` hands over most tokens at their last character, where
    // `ReadDocument` sizes them, but not two: a number is handed over only
    // once the reader has seen the character after it and found it a
    // delimiter, and a property name only once it has read the `:` after it.
    // When what follows is not JSON, or the text ends, the reader throws
    // instead, and the number or the name is never sized (JYRO-DEF-478,
    // JYRO-DEF-480). So the text after the last token handed over - from
    // `start`, that token being `last` - is read here in the reader's own
    // dialect (white space, comments, an optional trailing comma) for the
    // one token that would come next:
    //
    // - in an array (`top`) of `M` elements, a complete number, the element
    //   that would make it hold `M + 1`: exceeded at its last digit;
    // - in an object of `M` properties, after the name `name` not already in
    //   it, a complete number, the property that would make it hold `M + 1`;
    // - in an object, a property name longer than `M` code units: exceeded at
    //   its closing `"`.
    //
    // The token is read as the reader reads it: a number is the longest run
    // that the JSON grammar lets continue, so `1.` or `1e` is not a complete
    // number, while `0` before `1` is. Everything from `start` to the point
    // is checked to be JSON, so the reader's refusal comes after the point. A
    // number outside the range of a double is one `FromJson` cannot interpret
    // (spec 95.13), so it decides nothing here. Counts each byte read for the
    // execution check (spec 100.2.5).
    static member private ExceedsSizeInTail
        (text: byte[], start: int, last: JsonTokenType, top: JyroValue, name: string | null) : bool =
        let n = text.Length
        let i = ref start
        let ok = ref true
        let at c = i.Value < n && text.[i.Value] = byte c
        let isDigit () = i.Value < n && text.[i.Value] >= byte '0' && text.[i.Value] <= byte '9'
        let advance () =
            ExecutionCheck.Tick()
            i.Value <- i.Value + 1
        // White space and comments, as the reader skips them under
        // `JsonCommentHandling.Skip`. Each rule is the reader's own, checked
        // against it (JYRO-DEF-483, JYRO-ADR-107):
        // - white space is space, tab, LF and CR;
        // - a `//` comment ends at the first LF or CR (a CR LF is a CR that
        //   ends it, then an LF of white space), or at the end of the text;
        //   the reader refuses U+2028 or U+2029 (E2 80 A8, E2 80 A9) inside
        //   one, so the text stops being JSON there; any other byte, invalid
        //   UTF-8 included, is passed over;
        // - a `/* */` comment ends at the first `*/`, whatever it holds, U+2028
        //   and U+2029 included; unterminated, the text is not JSON.
        let skip () =
            let mutable going = true
            while ok.Value && going do
                if i.Value < n && (text.[i.Value] = 0x20uy || text.[i.Value] = 0x09uy || text.[i.Value] = 0x0Auy || text.[i.Value] = 0x0Duy) then
                    advance ()
                elif at '/' && i.Value + 1 < n && text.[i.Value + 1] = byte '/' then
                    let mutable ended = false
                    while ok.Value && not ended && i.Value < n do
                        let c = text.[i.Value]
                        if c = 0x0Auy || c = 0x0Duy then ended <- true
                        elif c = 0xE2uy && i.Value + 2 < n && text.[i.Value + 1] = 0x80uy
                             && (text.[i.Value + 2] = 0xA8uy || text.[i.Value + 2] = 0xA9uy) then
                            ok.Value <- false
                        else advance ()
                elif at '/' && i.Value + 1 < n && text.[i.Value + 1] = byte '*' then
                    i.Value <- i.Value + 2
                    let mutable closed = false
                    while not closed && i.Value < n do
                        if text.[i.Value] = byte '*' && i.Value + 1 < n && text.[i.Value + 1] = byte '/' then
                            i.Value <- i.Value + 2
                            closed <- true
                        else advance ()
                    if not closed then ok.Value <- false
                else going <- false
        let expect c =
            if at c then advance () else ok.Value <- false
        let digits () =
            if isDigit () then
                while isDigit () do advance ()
            else ok.Value <- false
        // A complete number within the range of a double.
        let number () =
            let from = i.Value
            if at '-' then advance ()
            if at '0' then advance ()
            elif isDigit () then digits ()
            else ok.Value <- false
            if ok.Value && at '.' then
                advance ()
                digits ()
            if ok.Value && (at 'e' || at 'E') then
                advance ()
                if at '+' || at '-' then advance ()
                digits ()
            // Whether it is within the range of a double is decided from the
            // bounded text of the number, read in place, not by copying and
            // parsing it whole (JYRO-ADR-151 (5)(d), JYRO-DEF-609).
            ok.Value
            && Double.IsFinite (ExactNumber.readBytes(ReadOnlySpan<byte>(text, from, i.Value - from))).Nearest
        // The UTF-16 length of a complete string, or -1.
        let stringLength () : int64 =
            expect '"'
            let mutable length = 0L
            let mutable closed = false
            while ok.Value && not closed do
                if i.Value >= n then ok.Value <- false
                else
                    let c = text.[i.Value]
                    if c = byte '"' then
                        advance ()
                        closed <- true
                    elif c = byte '\\' then
                        // Judged by the decoder's own reading of an escape, so
                        // an escape it refuses - a `\u` escape of an unpaired
                        // surrogate included (JYRO-REQ-1179) - makes the
                        // string one FromJson cannot interpret here too
                        // (JYRO-DEF-742).
                        let struct (_, second, taken) = BoundedText.tryJsonEscape text i.Value n
                        if taken < 0 then ok.Value <- false
                        else
                            for _ in 1 .. taken do advance ()
                            length <- length + (if second >= 0 then 2L else 1L)
                    elif c < 0x20uy then ok.Value <- false
                    else
                        // One code unit per UTF-8 sequence, two for a
                        // four-byte one; a continuation byte adds none.
                        if c &&& 0xC0uy <> 0x80uy then
                            length <- length + (if c >= 0xF0uy then 2L else 1L)
                        advance ()
            if ok.Value then length else -1L
        let separate () =
            skip ()
            if ok.Value && last <> JsonTokenType.StartArray && last <> JsonTokenType.StartObject then
                expect ','
                skip ()
        match top with
        | :? JyroArray as a when a.Count >= ValueSize.Maximum ->
            separate ()
            ok.Value && number ()
        | :? JyroObject as o when last = JsonTokenType.PropertyName ->
            skip ()
            ok.Value && o.Count >= ValueSize.Maximum && not (o.PropertyStore.ContainsKey (nonNull name)) && number ()
        | :? JyroObject ->
            separate ()
            ok.Value && stringLength () > int64 ValueSize.Maximum
        | _ -> false

    // Reads one JSON document at `reader`, which has not yet been advanced,
    // and makes the Jyro value it describes. `whole` is false when the reader
    // was given only the start of the text, and what follows is not JSON.
    //
    // Read a token at a time, iteratively, so the first problem met, from the
    // start of the text, decides (spec 100.7, 100.8, ADR-104):
    //
    // - an array or object opened at a nesting depth above the maximum
    //   raises `ValueNestingLimitExceeded` at its `[` or `{`, whatever
    //   follows (JYRO-REQ-1655);
    // - text that is not JSON - the reader's `JsonException`, or the end of
    //   the text before the value is complete - raises `JsonException` to
    //   the caller, which reports it as its door does: `InvalidJson` for the
    //   host, null for the script's `FromJson`;
    // - for a script, a string longer than the maximum value size, or an
    //   element or property that would take an array or object above it, is
    //   where the text exceeds the size bound (JYRO-REQ-1656). The value is
    //   no longer built after that point, but the text is still read, because
    //   a container past the maximum nesting depth before the text stops
    //   being JSON raises `ValueNestingLimitExceeded` instead (spec 100.8.9);
    //   otherwise it raises `ValueSizeLimitExceeded`, whatever follows. A
    //   number or a property name that the reader refuses to hand over,
    //   because what follows it is not JSON, is sized by `ExceedsSizeInTail`.
    //
    // `script` is the script's text, the bytes the reader reads, and null for
    // the host, whose doors apply no size bound. A container is added to the
    // one that holds it when it closes, at its last character, which is where
    // 100.8 places the element or property that it is. A script's reading
    // counts each token for the execution check (spec 100.2.5).
    static member private ReadDocument(reader: byref<Utf8JsonReader>, script: byte[] | null, whole: bool) : JyroValue =
        let bounded = not (isNull script)
        // The containers being made, innermost on top, and for each the name
        // it will have in the object that holds it (null in an array).
        let containers = Stack<JyroValue>()
        let names = Stack<string | null>()
        let mutable name : string | null = null
        let mutable depth = 0
        let mutable root = Unchecked.defaultof<JyroValue>
        let mutable oversize = false
        let mutable finished = false
        // The last token the reader handed over, and the end of it.
        let mutable last = JsonTokenType.None
        let mutable lastEnd = 0L
        try
            if not (reader.Read()) then raise (JsonException("The text holds no JSON value."))
            while not finished do
                last <- reader.TokenType
                lastEnd <- reader.BytesConsumed
                if bounded then ExecutionCheck.Tick()
                let mutable completed = Unchecked.defaultof<JyroValue>
                match reader.TokenType with
                | JsonTokenType.StartArray
                | JsonTokenType.StartObject ->
                    if depth >= ValueRecursionGuard.MaxDepth then
                        JyroError.raiseRuntime MessageCode.ValueNestingLimitExceeded
                            [| box ValueRecursionGuard.MaxDepth |]
                    depth <- depth + 1
                    if not oversize then
                        names.Push(name)
                        name <- null
                        containers.Push(
                            if reader.TokenType = JsonTokenType.StartArray then JyroArray() :> JyroValue
                            else JyroObject() :> JyroValue)
                | JsonTokenType.EndArray
                | JsonTokenType.EndObject ->
                    depth <- depth - 1
                    if not oversize then
                        completed <- containers.Pop()
                        name <- names.Pop()
                | JsonTokenType.PropertyName
                | JsonTokenType.String when oversize -> ()
                | JsonTokenType.PropertyName ->
                    match JyroValue.StringToken(&reader, script) with
                    | null -> oversize <- true
                    | text -> name <- text
                | JsonTokenType.String ->
                    match JyroValue.StringToken(&reader, script) with
                    | null -> oversize <- true
                    | text -> completed <- JyroString(text) :> JyroValue
                | JsonTokenType.Number when oversize ->
                    // A number outside the range of a double is one a script's
                    // `FromJson` cannot interpret (spec 95.13).
                    // Decided from the bounded text of the token, not by
                    // parsing it whole (JYRO-ADR-151 (5)(d), JYRO-DEF-609).
                    if not (JyroValue.IsFiniteToken(&reader)) then
                        raise (JsonException("A number is outside the range of a double."))
                | JsonTokenType.Number -> completed <- JyroValue.NumberToken(&reader)
                | JsonTokenType.True -> completed <- JyroBoolean.True :> JyroValue
                | JsonTokenType.False -> completed <- JyroBoolean.False :> JyroValue
                | JsonTokenType.Null -> completed <- JyroNull.Instance :> JyroValue
                | JsonTokenType.Comment -> ()
                | _ -> raise (JsonException("Unexpected JSON token."))
                // Past the size point the value is no longer built: a `true`,
                // `false` or `null` read after it is not added to the
                // container that was open there, whose name for it went
                // with the token that passed the bound (JYRO-REQ-1656).
                if not oversize && not (isNull (box completed)) then
                    if depth = 0 then
                        root <- completed
                    else
                        match containers.Peek() with
                        | :? JyroArray as a ->
                            if bounded && a.Count >= ValueSize.Maximum then oversize <- true
                            else a.Add(completed)
                        | :? JyroObject as o ->
                            if bounded && o.Count >= ValueSize.Maximum && not (o.PropertyStore.ContainsKey (nonNull name)) then
                                oversize <- true
                            else
                                o.SetProperty(nonNull name, completed)
                                name <- null
                        | _ -> ()
                if depth = 0 && (not (isNull (box root)) || oversize) then finished <- true
                elif not (reader.Read()) then
                    raise (JsonException("The text ends before its value is complete."))
            // The value is complete: anything after it but white space is not
            // JSON (a script's `FromJson` also skips comments after it).
            if not whole then raise (JsonException("The text is not JSON after its value."))
            if reader.Read() then raise (JsonException("Text follows the JSON value."))
            if oversize then ValueSize.Raise() else root
        with :? JsonException when
                oversize
                || (bounded
                    && containers.Count > 0
                    && JyroValue.ExceedsSizeInTail(nonNull script, int lastEnd, last, containers.Peek(), name)) ->
            ValueSize.Raise()

    // Makes white space, in place, of each comment that stands between a
    // string and the `:` after it in a script's `FromJson` text (`text`, the
    // bytes its reader will read).
    //
    // JYRO-REQ-1869 reads a comment as nothing wherever RFC 8259 permits
    // white space, which includes between an object's key and its colon.
    // `Utf8JsonReader` skips comments everywhere else under
    // `JsonCommentHandling.Skip`, but refuses one after a property name
    // (JYRO-DEF-788). Each such comment is overwritten with spaces, byte for
    // byte, so every offset in the text is kept: the reader's own tokens,
    // `StringToken`'s place of a token in the text and `ExceedsSizeInTail`'s
    // re-reading are unchanged.
    //
    // Strings and comments are found by the reader's own rules (as
    // `ExceedsSizeInTail` copies them, JYRO-ADR-107): a string runs from `"`
    // to the next `"` not escaped by `\`; a `//` comment ends at the first LF
    // or CR, or at the end of the text, and holds no U+2028 or U+2029, which
    // the reader refuses there and JYRO-REQ-1869 makes a text `FromJson`
    // cannot interpret; a `/* */` comment ends at the first `*/`. A
    // text that stops being JSON at a string or a comment - one that is not
    // closed, or one the reader refuses - is left as it is from there, for the
    // reader to refuse. Only a comment followed, past white space and other
    // comments, by `:` is blanked: after a string that is not a key a `:` is
    // not JSON whatever precedes it, so no text the reader refuses becomes one
    // it reads other than by this rule. Counts each byte read for the
    // execution check (spec 100.2.5).
    static member private BlankCommentsBeforeColons(text: byte[]) : unit =
        let n = text.Length
        let isSpace (b: byte) = b = 0x20uy || b = 0x09uy || b = 0x0Auy || b = 0x0Duy
        let startsComment at =
            at + 1 < n && text.[at] = byte '/' && (text.[at + 1] = byte '/' || text.[at + 1] = byte '*')
        // The index after the comment that starts at `at`, or -1 when the
        // text stops being JSON in it.
        let commentEnd at =
            let mutable j = at + 2
            if text.[at + 1] = byte '/' then
                let mutable refused = false
                while not refused && j < n && text.[j] <> 0x0Auy && text.[j] <> 0x0Duy do
                    if text.[j] = 0xE2uy && j + 2 < n && text.[j + 1] = 0x80uy
                       && (text.[j + 2] = 0xA8uy || text.[j + 2] = 0xA9uy) then
                        refused <- true
                    else
                        ExecutionCheck.Tick()
                        j <- j + 1
                if refused then -1 else j
            else
                let mutable closed = -1
                while closed < 0 && j < n do
                    if text.[j] = byte '*' && j + 1 < n && text.[j + 1] = byte '/' then closed <- j + 2
                    else
                        ExecutionCheck.Tick()
                        j <- j + 1
                closed
        let mutable i = 0
        let mutable stop = false
        while not stop && i < n do
            ExecutionCheck.Tick()
            if text.[i] = byte '"' then
                let mutable j = i + 1
                let mutable closed = false
                while not closed && j < n do
                    ExecutionCheck.Tick()
                    if text.[j] = byte '\\' then j <- j + 2
                    elif text.[j] = byte '"' then
                        closed <- true
                        j <- j + 1
                    else j <- j + 1
                if not closed then stop <- true
                else
                    // White space and comments after the string, then `:`?
                    let mutable k = j
                    let mutable going = true
                    let mutable comment = false
                    while going && k < n do
                        if isSpace text.[k] then
                            ExecutionCheck.Tick()
                            k <- k + 1
                        elif startsComment k then
                            let e = commentEnd k
                            if e < 0 then going <- false
                            else
                                comment <- true
                                k <- e
                        else going <- false
                    if comment && k < n && text.[k] = byte ':' then
                        for p in j .. k - 1 do
                            text.[p] <- 0x20uy
                    i <- j
            elif startsComment i then
                let e = commentEnd i
                if e < 0 then stop <- true else i <- e
            else i <- i + 1

    // `FromJson` for a script (the standard library's `FromJson`): the same
    // reading as the host's `FromJson`, in the dialect 95.13 allows -
    // trailing commas and comments are accepted - counting each token for
    // the execution check (spec 100.2.5), and refusing a string, an array or
    // an object above the maximum value size where the text first exceeds it
    // (spec 100.8.7, JYRO-REQ-1656). Text that is not JSON raises
    // `InvalidJson`, which the script's `FromJson` makes null.
    static member internal FromJsonBounded(json: string) : JyroValue =
        if obj.ReferenceEquals(json, null) then raise (ArgumentNullException(nameof json))
        // An unpaired surrogate code unit in the text itself, not written as a
        // `\u` escape, has no UTF-8 encoding: the encoder would put U+FFFD in
        // its place and the text would be read as JSON. The text cannot be
        // interpreted from that code unit on (JYRO-REQ-1726), so, as the
        // host's `ReadHostText` does, only the text before it is encoded, and
        // read as a text that does not end there: the reader meets whatever
        // comes first, a limit of the text before it or the end of what it
        // was given, which is `InvalidJson`.
        let unpaired = JyroValue.FirstUnpairedSurrogate json
        let whole = unpaired < 0
        // Encoded a chunk at a time with a check between chunks, not in one
        // call over what may be a host string of any length (spec 100.2.5).
        let bytes = BoundedText.utf8Bytes (if whole then json else json.Substring(0, unpaired))
        JyroValue.BlankCommentsBeforeColons bytes
        try
            let mutable reader =
                Utf8JsonReader(
                    ReadOnlySpan<byte>(bytes),
                    whole,
                    JsonReaderState(
                        JsonReaderOptions(
                            AllowTrailingCommas = true,
                            CommentHandling = JsonCommentHandling.Skip,
                            MaxDepth = JyroValue.JsonReaderMaxDepth)))
            JyroValue.ReadDocument(&reader, bytes, whole)
        with
        | :? JsonException as ex -> JyroValue.InvalidJson ex
        // A `\u` escape of an unpaired surrogate, which `GetString` and
        // `BoundedText.jsonString` cannot make a string of, as the host doors
        // read it, for example `["\uDC00"]`. Every other exception, out-of-memory
        // included, is not a verdict on the text and reaches the caller as it
        // is (JYRO-DEF-708). Invalid UTF-8 cannot reach here: the bytes come
        // from `BoundedText.utf8Bytes`, so a `DecoderFallbackException` is an
        // engine failure (JYRO-DEF-741).
        | :? InvalidOperationException as ex -> JyroValue.InvalidJson ex

    // Admits a JSON number only if it is a Jyro number.
    //
    // JSON's grammar has no token for NaN or infinity, but it does not bound
    // an exponent, so `{"v": 1e400}` is well-formed JSON that `TryGetDouble`
    // reads as +Infinity. That value used to enter the engine unchallenged and
    // fail much later - most visibly on the way back out through `ToJson`,
    // where the document that produced it could not be re-emitted.
    //
    // Refused at the door rather than normalized, because this is a container
    // the host handed over, not a value: the host has `try`/`catch` and can
    // tell which document was bad. A script reaching this through the standard
    // library's own `FromJson` still gets null, because that wrapper makes
    // null of `NumberOverflow` as it does of `InvalidJson`, the two codes of
    // text it cannot interpret (spec 90.4.2, 95.13, JYRO-REQ-1658), so clause
    // 90.5 is satisfied without a second policy written here.
    //
    // The diagnostic names the source of the number, `Input`, where a
    // function's diagnostic names the function, then the number as the JSON
    // text writes it (`text`), which is what the host can find in its own
    // document (spec 130.6.2, JYRO-ADR-125 item 5).
    static member private FiniteFromJson(value: float, text: string) : JyroValue =
        if Double.IsNaN value || Double.IsInfinity value then
            JyroError.raiseRuntime MessageCode.NumberOverflow
                [| box "Input"; box text |]
        JyroNumber(value) :> JyroValue

    static member private FromJsonElement(element: JsonElement) : JyroValue =
        match element.ValueKind with
        | JsonValueKind.Null | JsonValueKind.Undefined -> JyroNull.Instance :> JyroValue
        | JsonValueKind.True -> JyroBoolean.True :> JyroValue
        | JsonValueKind.False -> JyroBoolean.False :> JyroValue
        // `GetRawText` copies the token whole: the one accepted exception of
        // JYRO-ADR-151 (6), as net8.0 gives no access to an element's bytes in
        // place. Bound: the token's length in the host's own document. The
        // text is then read as `NumberToken` reads a token.
        | JsonValueKind.Number -> JyroValue.NumberFromJsonText(element.GetRawText())
        | JsonValueKind.String -> JyroString(element.GetString() |> Option.ofObj |> Option.defaultValue "") :> JyroValue
        // The two recursive arms charge a guard level per container, as
        // FromDictionary and FromEnumerable do. This is reached only from
        // `FromObject`, for a JsonElement a host parsed itself: JSON text read
        // by the host's `FromJson` and the script's `FromJson` goes through
        // `ReadDocument`, which counts depth itself. A JsonElement deeper than
        // the maximum raises `ValueNestingLimitExceeded` here (spec 100.7.6,
        // JYRO-REQ-1433, 1435), so no value is built deeper than every later
        // guarded walk over it - ToJson, ToObjectValue, Freeze, EqualsValue,
        // Clone - can handle.
        | JsonValueKind.Array ->
            ValueRecursionGuard.Enter()
            try
                let arr = JyroArray()
                for item in element.EnumerateArray() do
                    arr.Add(JyroValue.FromJsonElement(item))
                arr :> JyroValue
            finally
                ValueRecursionGuard.Exit()
        | JsonValueKind.Object ->
            ValueRecursionGuard.Enter()
            try
                let result = JyroObject()
                for prop in element.EnumerateObject() do
                    result.SetProperty(prop.Name, JyroValue.FromJsonElement(prop.Value))
                result :> JyroValue
            finally
                ValueRecursionGuard.Exit()
        | _ -> JyroNull.Instance :> JyroValue

    // The number `text` writes, or a null reference when it writes none:
    // the text, with the four JSON white-space characters removed from each
    // end and no others, matches the JSON number grammar, and is held as
    // JSON gives the same text (spec 30.12.2, 30.12.3, 30.2.7, JYRO-ADR-17).
    // Text with no finite value writes no number. The one reading of text as
    // a number, shared by a `number` type hint and `ToNumber` (91.11), so
    // the two give one value for one text.
    //
    // Read in place, between the white space, so no copy of a long string
    // is made.
    static member internal NumberOfText(text: string) : JyroValue | null =
        let isJsonSpace (c: char) = c = ' ' || c = '\t' || c = '\n' || c = '\r'
        let mutable start = 0
        let mutable finish = text.Length
        while start < finish && isJsonSpace text.[start] do
            ExecutionCheck.Tick()
            start <- start + 1
        while finish > start && isJsonSpace text.[finish - 1] do
            ExecutionCheck.Tick()
            finish <- finish - 1
        if not (ExactNumber.isJsonNumberIn text start finish) then null
        else
            let digits = ExactNumber.readIn text start finish
            match digits.Exact with
            | ValueSome d -> JyroNumber(d) :> JyroValue | null
            | ValueNone ->
                // The nearest double is parsed from a text of fixed bound
                // (JYRO-ADR-151 (5)(b)).
                let n = digits.Nearest
                if Double.IsFinite n then (JyroNumber(n) :> JyroValue | null) else null

    // Coerces a value to the target type for type hint enforcement.
    // Returns the coerced value if conversion succeeds, or throws JyroRuntimeException.
    //
    // A refusal raises `InvalidType` with exactly two arguments, the value's
    // type as `TypeOf` gives it and the hinted type (spec 30.12.9,
    // JYRO-ADR-153 (3)). No argument names the target: an element has no
    // name, and the diagnostic's place shows which target it was.
    //
    // Internal (JYRO-ADR-191 item 6): compiled code calls it, bound with the
    // non-public binding flags.
    static member internal CoerceToType(value: JyroValue | null, targetType: JyroValueType) : JyroValue =
        // A null reference is the absent value, as at every other door that
        // takes a *value* - `Add`, `SetProperty`, `WithInput`, `CompareForSort`.
        // Dereferencing it on the next line made this raise
        // NullReferenceException instead of coercing, or refusing to.
        let value : JyroValue =
            match value with
            | null -> JyroNull.Instance :> JyroValue
            | value -> value
        if value.ValueType = targetType then value
        else
            let fail () : JyroValue =
                JyroError.raiseRuntime MessageCode.InvalidType
                    [| box (TypeNames.typeOfName value.ValueType); box (TypeNames.typeOfName targetType) |]
            match targetType with
            | JyroValueType.Number ->
                match value with
                | :? JyroString as s ->
                    // The text read as JSON reads it (spec 30.12.2, 30.12.3,
                    // JYRO-ADR-17). The platform's parsers accepted group
                    // separators, a currency symbol, `(5)`, `Infinity` and
                    // any Unicode white space, and rounded `1e-29` to an
                    // exact zero. Text with no finite value is text this
                    // coercion cannot perform, as text that is not a number is.
                    match JyroValue.NumberOfText s.Value with
                    | null -> fail ()
                    | n -> n
                | :? JyroBoolean as b ->
                    // The exact 0 or 1 (spec 30.2.4, JYRO-REQ-1728).
                    JyroNumber(if b.Value then 1M else 0M) :> JyroValue
                | _ -> fail ()
            | JyroValueType.String ->
                match value with
                | :? JyroNumber | :? JyroBoolean ->
                    JyroString(value.ToStringValue()) :> JyroValue
                | _ -> fail ()
            | JyroValueType.Boolean ->
                match value with
                | :? JyroNumber as n ->
                    JyroBoolean.FromBoolean(n.IsTruthy()) :> JyroValue
                | :? JyroString as s ->
                    // With no case sensitivity for ASCII letters (spec 30.12.7,
                    // 30.12.8, 10.7.1).
                    if AsciiCase.equals s.Value "true" then JyroBoolean.True :> JyroValue
                    elif AsciiCase.equals s.Value "false" then JyroBoolean.False :> JyroValue
                    else fail ()
                | _ -> fail ()
            | _ ->
                // Array, Object, Null, Function - no cross-type coercion.
                // A value already of the target type short-circuits above, so
                // reaching here for one of these means a genuine mismatch.
                fail ()

    /// <summary>
    /// Total ordering used by the sorting functions (Sort, SortByField,
    /// SortBy). Unlike the relational operators it never raises: null sorts
    /// first, values of different types are ordered by a stable type rank,
    /// and values of the same comparable type compare by value. Every sort of
    /// the standard library uses this one order.
    /// </summary>
    static member CompareForSort(a: JyroValue | null, b: JyroValue | null) : int =
        // Total, and that has to include a null reference. A host that ignores
        // the nullable annotations - or a host function returning a null -
        // can hand one in, and `rank` below dereferences its argument. This
        // raised a `NullReferenceException` from the one member documented
        // never to raise, and documented that way for a reason: a sort has to
        // place every pair of values somewhere, which is why this exists
        // separately from the relational operators, which refuse.
        //
        // Normalized rather than refused, because a null *value* becomes
        // `JyroNull` at every other door - `Add`, `SetProperty`, `WithInput`,
        // `FromObject`. Sorting a null therefore sorts it exactly where a
        // stored null sorts: first, by the rank table below.
        let a : JyroValue = match a with | null -> JyroNull.Instance :> JyroValue | a -> a
        let b : JyroValue = match b with | null -> JyroNull.Instance :> JyroValue | b -> b
        let rank (v: JyroValue) =
            match v.ValueType with
            | JyroValueType.Null -> 0
            | JyroValueType.Boolean -> 1
            | JyroValueType.Number -> 2
            | JyroValueType.String -> 3
            | JyroValueType.Array -> 4
            | JyroValueType.Object -> 5
            | _ -> 6
        match a, b with
        // Numbers by their double value; with equal double values, two exact
        // numbers by exact value, an exact number before an inexact one, and
        // two inexact numbers tied (spec 90.6.3, 92.11.6, JYRO-ADR-34). Not
        // `JyroNumber.Compare`, which ties an exact and an inexact number of
        // equal double value and so is not transitive when the two kinds mix.
        | (:? JyroNumber as an), (:? JyroNumber as bn) ->
            let byDouble = an.Value.CompareTo(bn.Value)
            if byDouble <> 0 then sign byDouble
            else
                match an.IsExact, bn.IsExact with
                | true, true -> sign (an.DecimalValue.CompareTo(bn.DecimalValue))
                | true, false -> -1
                | false, true -> 1
                | false, false -> 0
        | (:? JyroString as aStr), (:? JyroString as bStr) -> BoundedText.compareOrdinal aStr.Value bStr.Value
        | (:? JyroBoolean as ab), (:? JyroBoolean as bb) -> ab.Value.CompareTo(bb.Value)
        | _ -> compare (rank a) (rank b)

    // Freezes any value, marking it as `Input` so a rejected write names the
    // right thing. Scalars are already immutable, so a non-container is a
    // no-op rather than an error.
    //
    // The runtime calls this on the host's input before a script runs. It is
    // what actually makes `Input` read-only: the validator refuses a write
    // whose target is rooted at `Input`, but values are referenced rather than
    // copied, so `var t = Input.a` hands the script the host's own object with
    // no syntactic trace of where it came from. Only the value itself can
    // refuse that write.
    //
    // Re-entering the collection pass on an already-marked graph is cheap and
    // deliberate: an input value the host mutated between runs, or a subgraph
    // grafted on after an earlier freeze, is picked up rather than skipped.
    static member internal FreezeAsInput(value: JyroValue) : unit =
        //
        // A cyclic value is made read-only, and the depth bound is counted on
        // the shortest path to each container (spec 70.2.3, JYRO-ADR-70): see
        // `CollectInputContainers`. Nothing is marked when it raises.
        match value with
        | :? JyroArray | :? JyroObject ->
            let acc = ResizeArray<JyroValue>()
            JyroArray.CollectInputContainers(value, acc)
            JyroArray.ApplyReadOnly(acc, true)
        | _ -> ()

/// <summary>
/// Represents a null value (singleton)
/// </summary>
and [<Sealed>] JyroNull private () =
    inherit JyroValue()

    static let instance = JyroNull()
    /// <summary>
    /// The single <c>JyroNull</c> instance. <c>JyroValueType.Null</c> values always
    /// reference this - there is no other way to construct one.
    /// </summary>
    static member Instance = instance

    override _.ValueType = JyroValueType.Null
    override _.IsNull = true
    override _.ToObjectValue() = null
    override _.ToStringValue() = "null"
    // Equality on null is reflexive: null equals null and nothing else.
    // See Jyro Language Specification 30.10 (Equality of null).
    override _.EqualsValue(other) =
        match other with
        | :? JyroNull -> true
        | _ -> false
    override _.WriteJson(writer) =
        JyroValue.RequireWriter writer
        writer.WriteNullValue()

    // See the note on `JyroValue.Equals` above: the four immutable scalars
    // carry .NET value equality; the mutable containers deliberately do not.
    override _.Equals(other) = match other with :? JyroNull -> true | _ -> false
    override _.GetHashCode() = 0
    override _.ToString() = "null"

    member internal this.OwnEvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match op with
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | Concat
        | Equal
        | NotEqual
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | And
        | Or
        | Coalesce -> this.DefaultEvaluateBinary(op, right)

/// <summary>
/// Represents a boolean value (flyweight pattern)
/// </summary>
and [<Sealed>] JyroBoolean internal (value: bool) =
    inherit JyroValue()

    static let trueInstance = JyroBoolean(true)
    static let falseInstance = JyroBoolean(false)

    /// <summary>
    /// The singleton <c>true</c> value.
    /// </summary>
    static member True = trueInstance
    /// <summary>
    /// The singleton <c>false</c> value.
    /// </summary>
    static member False = falseInstance
    /// <summary>
    /// The singleton for <c>value</c>: <c>True</c> or <c>False</c>.
    /// </summary>
    static member FromBoolean(value: bool) = if value then trueInstance else falseInstance

    /// <summary>
    /// The underlying boolean.
    /// </summary>
    member _.Value = value
    override _.ValueType = JyroValueType.Boolean
    override this.AsBoolean() = this
    [<return: System.Diagnostics.CodeAnalysis.NotNull>]
    override _.ToObjectValue() : obj = nonNull (box value)
    override _.ToStringValue() = if value then "true" else "false"
    override _.WriteJson(writer) =
        JyroValue.RequireWriter writer
        writer.WriteBooleanValue(value)
    override _.EqualsValue(other) =
        match other with
        | :? JyroBoolean as b -> b.Value = value
        | _ -> false

    override _.Equals(other) = match other with :? JyroBoolean as b -> b.Value = value | _ -> false
    override _.GetHashCode() = if value then 1 else 0
    override _.ToString() = if value then "true" else "false"

    member internal this.OwnEvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match op with
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | Concat
        | Equal
        | NotEqual
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | And
        | Or
        | Coalesce -> this.DefaultEvaluateBinary(op, right)

/// <summary>
/// Represents a numeric value. Backed by either an exact base-10 decimal
/// (the default for JSON integers/decimals, host int/int64/decimal, and
/// numeric literals - lossless for IDs and money) or an IEEE-754 double for
/// values a decimal cannot hold (magnitudes outside the decimal range and
/// transcendental results).
///
/// A Jyro number is finite. NaN and the infinities are not Jyro numbers
/// and cannot be constructed; the double constructor rejects them. Producing
/// an infinity or a NaN silently is hazardous in a script, which is why
/// division by zero raises <c>DivisionByZero</c>, and the same holds on every
/// route by which a number is made.
/// </summary>
and [<Sealed>] JyroNumber private (rep: NumberRep) =
    inherit JyroValue()

    // Only meaningful for the Inexact path: whether an integer-valued double
    // serializes as a float (6.0 not 6). Exact numbers carry the distinction
    // in their decimal scale instead.
    let mutable forceFloat =
        match rep with
        | Inexact f -> abs(f % 1.0) >= Double.Epsilon
        | Exact _ -> false

    /// <summary>
    /// Construct from an IEEE-754 double (inexact).
    ///
    /// Refuses a value that is not finite with a <c>JyroRuntimeException</c> whose
    /// code is <c>HostNumberNaN</c> for NaN and <c>HostNumberInfinite</c> for an
    /// infinity, with no argument. Every
    /// number of a host value reaches the engine through here, so the first
    /// one of a walk that is not finite decides the code. An
    /// operation's result that is not finite is refused before it gets here,
    /// with <c>NumberDomainError</c> or <c>NumberOverflow</c>.
    /// </summary>
    new(value: float) =
        if Double.IsNaN value then
            JyroError.raiseRuntime MessageCode.HostNumberNaN Array.empty<obj>
        elif Double.IsInfinity value then
            JyroError.raiseRuntime MessageCode.HostNumberInfinite Array.empty<obj>
        JyroNumber(Inexact value)
    /// <summary>
    /// Construct from an exact base-10 decimal.
    /// </summary>
    new(value: decimal) = JyroNumber(Exact value)
    /// <summary>
    /// Construct from a 32-bit integer, exactly, so that <c>new JyroNumber(1)</c>
    /// binds without a suffix. Integers are exact by construction, so this
    /// routes to the decimal representation.
    /// </summary>
    new(value: int) = JyroNumber(Exact(decimal value))
    /// <summary>
    /// Construct from a 64-bit integer, exactly. Every <c>int64</c> is inside the
    /// exact decimal range, so no value is lost - which is the whole point of
    /// having it rather than letting the call bind to <c>double</c>.
    /// </summary>
    new(value: int64) = JyroNumber(Exact(decimal value))

    /// <summary>
    /// True when this number is stored as an exact decimal.
    /// </summary>
    member _.IsExact = match rep with Exact _ -> true | Inexact _ -> false
    /// <summary>
    /// The exact decimal value. Meaningful only when IsExact is true.
    ///
    /// An inexact value converts only if it is inside <c>decimal</c>'s range, which
    /// is far narrower than a double's - about 7.9e28 against 1.8e308. Outside
    /// it, this raises a <c>JyroRuntimeException</c> with <c>IntegerOutOfRange</c>, the
    /// code <c>JyroArguments.GetDecimal</c> gives for the same condition, and not
    /// the <c>OverflowException</c> of the conversion, which a host's
    /// <c>catch (JyroRuntimeException)</c> would not see.
    ///
    /// Its arguments are the value, the least and the greatest only: this
    /// member is called by a host, not by a function of the standard library,
    /// so it has no function and no parameter to name.
    /// </summary>
    member this.DecimalValue : decimal =
        match rep with
        | Exact d -> d
        | Inexact f ->
            try decimal f
            with :? OverflowException ->
                JyroError.raiseRuntime MessageCode.IntegerOutOfRange
                    [| box (this.ToStringValue())
                       box Decimal.MinValue
                       box Decimal.MaxValue |]
    /// <summary>
    /// The double nearest to the number. For an inexact number that is its
    /// value; for an exact number it loses precision whenever the decimal has
    /// no double of the same value, as 9007199254740993 and 0.1 have none.
    /// <c>DecimalValue</c> gives the exact value of an exact number. The double
    /// is the one nearest the exact decimal value, rounded once, ties to even.
    /// </summary>
    member _.Value : float = match rep with Exact d -> ExactNumber.toDouble d | Inexact f -> f
    /// <summary>
    /// True when the value has no fractional part, in whichever
    /// representation it is stored.
    /// </summary>
    member _.IsInteger =
        match rep with
        | Exact d -> d = Decimal.Truncate d
        | Inexact f -> not (Double.IsNaN f) && not (Double.IsInfinity f) && abs(f % 1.0) < Double.Epsilon
    /// <summary>
    /// The number with the opposite sign and the same representation, exact
    /// or inexact.
    /// </summary>
    member _.Negate() : JyroNumber = match rep with Exact d -> JyroNumber(-d) | Inexact f -> JyroNumber(-f)
    // `ForceFloat` was a public get/set over the `forceFloat` field below. It
    // had no reader and no writer anywhere - not in this repository, not in
    // either consumer - and it was the only mutable member on a value type
    // whose whole design is immutability.
    //
    // It also cost something. `Clone` could not simply return the number it was
    // given, which is the obvious implementation for an immutable value and the
    // one `JyroNull` and `JyroBoolean` already use, because returning the same
    // instance would have aliased this flag. Removing the setter is what makes
    // `JyroNumber` genuinely immutable, and `Clone` now says so.
    //
    // The field survives: it is read at `ToObjectValue` and `WriteJson` below,
    // where it decides whether an integer-valued double serializes as `6` or
    // `6.0`. It is derived once in the constructor and never assigned again.

    override _.ValueType = JyroValueType.Number
    override this.AsNumber() = this
    [<return: System.Diagnostics.CodeAnalysis.NotNull>]
    override this.ToObjectValue() : obj =
        nonNull (
            match rep with
            | Exact d ->
                if d = Decimal.Truncate d then
                    try box (Convert.ToInt64 d) with :? OverflowException -> box d
                else box d
            | Inexact f ->
                if this.IsInteger && not forceFloat then
                    try box (Convert.ToInt64 f) with :? OverflowException -> box f
                else box f)
    /// <summary>
    /// Narrows to <c>int</c>, reporting rather than raising a BCL exception when the
    /// value has no <c>int</c> to narrow to.
    ///
    /// Like every <c>As*</c> and <c>To*</c> member, it reports a failed conversion as a
    /// <c>JyroRuntimeException</c>: a value outside the range of <c>int</c> raises one
    /// with <c>IntegerOutOfRange</c>, not an <c>OverflowException</c>. The value is
    /// rounded as <c>Convert.ToInt32</c> rounds it, and is out of range exactly when
    /// that conversion refuses it.
    ///
    /// The diagnostic's arguments are the value, the least and the greatest
    /// only: a host calls this member, so there is no function of the standard
    /// library and no parameter to name.
    /// </summary>
    override this.ToInt32() =
        try match rep with Exact d -> Convert.ToInt32 d | Inexact f -> Convert.ToInt32 f
        with :? OverflowException ->
            JyroError.raiseRuntime MessageCode.IntegerOutOfRange
                [| box (this.ToStringValue())
                   box Int32.MinValue
                   box Int32.MaxValue |]

    /// <summary>
    /// Narrows to <c>int64</c>, as <c>ToInt32</c> narrows to <c>int</c>: a value outside the
    /// range of <c>int64</c> raises a <c>JyroRuntimeException</c> with
    /// <c>IntegerOutOfRange</c>, whose arguments are the value, the least and the
    /// greatest.
    /// </summary>
    override this.ToInt64() =
        try match rep with Exact d -> Convert.ToInt64 d | Inexact f -> Convert.ToInt64 f
        with :? OverflowException ->
            JyroError.raiseRuntime MessageCode.IntegerOutOfRange
                [| box (this.ToStringValue())
                   box Int64.MinValue
                   box Int64.MaxValue |]

    // Converts to `int` when the value has one, and reports failure rather
    // than raising when it does not.
    //
    // This exists so that converting an index is never the step that decides
    // the index's fate. `Convert.ToInt32` raises `OverflowException` for
    // 1e30, NaN and infinity, so an index door that converted before testing
    // its own bounds surfaced a CLR failure for what is simply an index
    // outside the container - and never reached the rule it was supposed to
    // apply. Callers test this first, then apply their own out-of-range rule
    // to a `false` exactly as they would to a number that is merely too big.
    //
    // A fractional index never reaches here from an index door: `s[i]`,
    // `a[i]` and `a[i] = v` refuse it first with `IntegerIndexRequired` (spec
    // 30.3.6, 30.4.10, `RequireIntegerIndex`).
    member internal _.TryToInt32(result: outref<int>) : bool =
        try
            result <- (match rep with Exact d -> Convert.ToInt32 d | Inexact f -> Convert.ToInt32 f)
            true
        with :? OverflowException ->
            result <- 0
            false

    override this.ToDouble() = this.Value
    override _.ToStringValue() =
        match rep with
        // The string form of an exact number is its decimal value: no
        // exponent, no trailing zeros after the point, no point for an
        // integer, and `0` for zero (spec 40.4.6). The decimal's scale is
        // not part of the value, so `1.50` and `1.5` give `1.5`.
        | Exact d ->
            if d = 0m then "0"
            else d.ToString("0.############################", CultureInfo.InvariantCulture)
        // The string form of an inexact number is ECMAScript's
        // Number::toString (spec 40.4.7, 10.6.2): `1e+21`, never `1E+21`.
        | Inexact f -> NumberText.ecmaScript f
    override this.WriteJson(writer) =
        JyroValue.RequireWriter writer
        match rep with
        | Exact d -> writer.WriteNumberValue(d)
        | Inexact f ->
            if this.IsInteger && not forceFloat then
                // Render integer-valued doubles as integers, but never crash
                // on values outside int64 range (e.g. >= 2^63) - fall back to
                // the double form instead.
                try writer.WriteNumberValue(Convert.ToInt64 f)
                with :? OverflowException -> writer.WriteNumberValue(f)
            else
                writer.WriteNumberValue(f)

    // Builds a number from a computed double, reporting rather than
    // constructing when the computation has no finite answer.
    //
    // Every site that computes a double - the arithmetic operators here, and
    // the standard library's math functions - goes through this, so the
    // finiteness rule is applied once and reported the same way everywhere.
    // Without it each site would either construct an invalid number or invent
    // its own wording for the same condition.
    //
    // The two outcomes are split because they are corrected differently, the
    // same reason `IntegerRequired` and `IntegerOutOfRange` are separate
    // codes. NaN means no answer exists for these arguments, and the fix is
    // to guard the input. An infinity means an answer exists but has no
    // representable magnitude, and the fix is to scale the computation.
    //
    // The arguments of the diagnostic are `operation` followed by `operands`,
    // each element its own argument (spec 130.6.2, JYRO-ADR-123). A library
    // function gives, for each parameter in signature order, its name read
    // from the function's `Signature` and then its value; an operator, which
    // has no parameters, gives the values of its operands alone. No argument
    // joins two names or two values.
    static member internal OfComputed(operation: string, operands: (obj | null)[], value: float) : JyroValue =
        if Double.IsNaN value then
            JyroError.raiseRuntime MessageCode.NumberDomainError
                (Array.append [| box operation |] operands)
        elif Double.IsInfinity value then
            JyroError.raiseRuntime MessageCode.NumberOverflow
                (Array.append [| box operation |] operands)
        JyroNumber(value) :> JyroValue

    // Raises `IntegerIndexRequired` when `index`, the index of `s[i]`, `a[i]`
    // or `a[i] = v`, is not an integer (spec 30.3.6, 30.4.10). Its one
    // argument is the index: an index operator has no function and no
    // parameter to name, so it has a code of its own rather than
    // `IntegerRequired`'s [function; parameter; value] (JYRO-ADR-153 (1)).
    static member internal RequireIntegerIndex(index: JyroNumber) : unit =
        if not index.IsInteger then
            JyroError.raiseRuntime MessageCode.IntegerIndexRequired
                [| box (index.ToStringValue()) |]

    /// <summary>
    /// Canonical numeric ordering shared by relational operators, equality,
    /// and sorting. Exact/exact compares exactly in decimal; any inexact
    /// operand compares in double.
    /// </summary>
    static member Compare(a: JyroNumber, b: JyroNumber) : int =
        if a.IsExact && b.IsExact then a.DecimalValue.CompareTo(b.DecimalValue)
        else a.Value.CompareTo(b.Value)

    // Exact decimal arithmetic. When the exact representation cannot hold
    // the result without rounding - it is too large, or it needs more digits
    // than the representation has, as `1 / 3` does - the operation is done
    // in IEEE 754 double precision and the result is inexact (spec 30.2.11,
    // JYRO-ADR-4). Divisor-zero is guarded by the caller.
    //
    // `decimal` rounds such a result to 28 digits and reports success, so
    // its result is checked against the operands in integers: `r = a op b`
    // exactly, with `x = mx / 10^sx` for each of the three. The remainder of
    // two exact operands is always exact, so `%` is not checked.
    static member private ExactArith(op: BinaryOp, a: decimal, b: decimal) : JyroValue =
        let inexact () = JyroNumber.InexactArith(op, ExactNumber.toDouble a, ExactNumber.toDouble b)
        let computed =
            try
                match op with
                | Add -> ValueSome (a + b)
                | Subtract -> ValueSome (a - b)
                | Multiply -> ValueSome (a * b)
                | Divide -> ValueSome (a / b)
                | Modulo -> ValueSome (a % b)
                | _ -> failwith "ExactArith called with a non-arithmetic operator"
            with :? OverflowException -> ValueNone
        match computed with
        | ValueNone -> inexact ()
        | ValueSome r ->
            if op = Modulo || JyroNumber.IsExactResult(op, a, b, r) then JyroNumber(r) :> JyroValue
            else inexact ()

    // Whether `r` is exactly `a op b`, for `+`, `-`, `*` and `/`. Internal so
    // that a fast path redoing an operator's decimal arithmetic (Range's
    // elements) decides exactness by this same rule rather than restating it.
    static member internal IsExactResult(op: BinaryOp, a: decimal, b: decimal, r: decimal) : bool =
        // `decimal` adds at the larger scale and multiplies at the sum of the
        // scales, and gives up scale only when it rounds; at that scale the
        // result is exact, with no need to count.
        match op with
        | Add | Subtract when int r.Scale = max (int a.Scale) (int b.Scale) -> true
        | Multiply when int r.Scale = int a.Scale + int b.Scale -> true
        | _ -> JyroNumber.IsExactResultCounted(op, a, b, r)

    // `IsExactResult`, counted in integers.
    static member private IsExactResultCounted(op: BinaryOp, a: decimal, b: decimal, r: decimal) : bool =
        let struct (ma, sa) = ExactNumber.parts a
        let struct (mb, sb) = ExactNumber.parts b
        let struct (mr, sr) = ExactNumber.parts r
        let pow (n: int) = System.Numerics.BigInteger.Pow(System.Numerics.BigInteger(10), n)
        match op with
        | Add | Subtract ->
            // a op b at the scale s = max(sa, sb), then against r at sr.
            let s = max sa sb
            let sum =
                let x = ma * pow (s - sa)
                let y = mb * pow (s - sb)
                if op = Add then x + y else x - y
            sum * pow sr = mr * pow s
        | Multiply -> ma * mb * pow sr = mr * pow (sa + sb)
        // r * b = a: mr * mb / 10^(sr + sb) = ma / 10^sa.
        | Divide -> mr * mb * pow sa = ma * pow (sr + sb)
        | _ -> true

    // Inexact (double) arithmetic.
    //
    // The result goes through `OfComputed` because a zero divisor is not the
    // only way an operator reaches a non-finite value: `1e300 / 1e-28` and
    // `1e300 * 1e300` both overflow to infinity with entirely ordinary
    // operands, and the `DivisionByZero` guard above cannot see either. The
    // specification's rationale for that guard is written about *producing*
    // `Infinity` or `NaN`, so this is the same rule reaching the rest of its
    // own scope.
    static member private InexactArith(op: BinaryOp, a: float, b: float) : JyroValue =
        // The operator as written, then each operand's value as its own
        // argument: an operator has no parameter names to give (spec 130.6.2,
        // JYRO-ADR-125 item 1).
        let operands () =
            [| box (a.ToString(CultureInfo.InvariantCulture))
               box (b.ToString(CultureInfo.InvariantCulture)) |]
        let computed v = JyroNumber.OfComputed(Operators.binaryOpToString op, operands (), v)
        match op with
        | Add -> computed (a + b)
        | Subtract -> computed (a - b)
        | Multiply -> computed (a * b)
        | Divide -> computed (a / b)
        | Modulo -> computed (a % b)
        | _ -> failwith "InexactArith called with a non-arithmetic operator"

    member internal this.OwnEvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match right with
        | :? JyroNumber as rn ->
            let bothExact = this.IsExact && rn.IsExact
            let arith () =
                if bothExact then JyroNumber.ExactArith(op, this.DecimalValue, rn.DecimalValue)
                else JyroNumber.InexactArith(op, this.Value, rn.Value)
            let divisorIsZero () =
                if rn.IsExact then rn.DecimalValue = 0m else rn.Value = 0.0
            match op with
            | Add | Subtract | Multiply -> arith ()
            | Divide ->
                if divisorIsZero () then JyroError.raiseRuntime MessageCode.DivisionByZero Array.empty<obj>
                arith ()
            | Modulo ->
                if divisorIsZero () then JyroError.raiseRuntime MessageCode.ModuloByZero Array.empty<obj>
                arith ()
            | LessThan -> JyroBoolean.FromBoolean(JyroNumber.Compare(this, rn) < 0) :> JyroValue
            | LessThanOrEqual -> JyroBoolean.FromBoolean(JyroNumber.Compare(this, rn) <= 0) :> JyroValue
            | GreaterThan -> JyroBoolean.FromBoolean(JyroNumber.Compare(this, rn) > 0) :> JyroValue
            | GreaterThanOrEqual -> JyroBoolean.FromBoolean(JyroNumber.Compare(this, rn) >= 0) :> JyroValue
            | Concat
            | Equal
            | NotEqual
            | And
            | Or
            | Coalesce -> this.DefaultEvaluateBinary(op, right)
        // A string right operand of `+` falls to the base, which raises
        // `StringOperandInAdditionRuntime` for either side (JYRO-ADR-159).
        | _ -> this.DefaultEvaluateBinary(op, right)

    member internal this.OwnEvaluateUnary(op: UnaryOp) : JyroValue =
        match op with
        | Negate ->
            match rep with
            | Exact d -> JyroNumber(-d) :> JyroValue
            | Inexact f -> JyroNumber(-f) :> JyroValue
        | Not -> this.DefaultEvaluateUnary(op)

    /// <summary>
    /// Whether <c>other</c> is a number of the same value, as the script-level
    /// <c>==</c> compares two numbers: equal exactly when the ordering of the
    /// relational operators puts neither before the other.
    /// </summary>
    override this.EqualsValue(other) =
        match other with
        | :? JyroNumber as n -> JyroNumber.Compare(this, n) = 0
        | _ -> false

    // The decimal whose value is exactly that of the double `f`, when
    // `System.Decimal` holds one: `f` is `m * 2^e`, an integer when `e` is
    // zero or more, and otherwise `m * 5^-e / 10^-e`, which needs `-e`
    // decimal places. None when the coefficient is past 2^96 or the places
    // past 28, as for the double nearest 0.1, whose exact value has 55.
    static member private ExactDecimalOf(f: float) : decimal voption =
        if f = 0.0 then ValueSome 0m
        else
            let bits = BitConverter.DoubleToInt64Bits f
            let negative = bits < 0L
            let field = int ((bits >>> 52) &&& 0x7FFL)
            let fraction = bits &&& 0xFFFFFFFFFFFFFL
            let mutable m = if field = 0 then fraction else fraction ||| (1L <<< 52)
            let mutable e = if field = 0 then -1074 else field - 1075
            while e < 0 && (m &&& 1L) = 0L do
                m <- m >>> 1
                e <- e + 1
            let limit = Numerics.BigInteger.One <<< 96
            let coefficient, scale =
                if e >= 0 then Numerics.BigInteger(m) <<< e, 0
                elif e >= -28 then Numerics.BigInteger(m) * Numerics.BigInteger.Pow(Numerics.BigInteger(5), -e), -e
                else limit, 0
            if coefficient >= limit then ValueNone
            else
                let bytes = Array.zeroCreate<byte> 12
                let written = coefficient.ToByteArray(true, false)
                Array.blit written 0 bytes 0 written.Length
                ValueSome(
                    Decimal(
                        BitConverter.ToInt32(bytes, 0), BitConverter.ToInt32(bytes, 4), BitConverter.ToInt32(bytes, 8),
                        negative, byte scale))

    /// <summary>
    /// Whether <c>obj</c> is a <c>JyroNumber</c> of the same mathematical value.
    /// The value of an exact number is its
    /// decimal; that of an inexact number is the exact value of its double,
    /// every bit of it, not a decimal rounding of it. So the inexact
    /// 9007199254740992.0 equals the exact 9007199254740992 and not the exact
    /// 9007199254740993, the inexact 1.0 equals the exact 1, and the inexact
    /// 0.1 equals no exact number. The relation is reflexive, symmetric and
    /// transitive, so a number can be a key of a <c>Dictionary</c> or a member of a
    /// <c>HashSet</c>.
    ///
    /// It is not the language's <c>==</c>, which is <c>EqualsValue</c> and
    /// compares a mixed pair in double: there the exact 9007199254740993 and
    /// the inexact 9007199254740992.0 are equal. The two may disagree.
    /// </summary>
    override this.Equals(other) =
        match other with
        | :? JyroNumber as n ->
            match rep, n.Rep with
            | Exact a, Exact b -> a = b
            | Inexact f, Inexact g -> f = g
            | Exact d, Inexact f
            | Inexact f, Exact d ->
                match JyroNumber.ExactDecimalOf f with
                | ValueSome x -> x = d
                | ValueNone -> false
        | _ -> false

    member internal _.Rep : NumberRep = rep

    /// <summary>
    /// The hash of the double nearest the number. Two numbers that <c>Equals</c>
    /// relates have one mathematical value and so one nearest double; zero
    /// and negative zero are hashed alike. Two exact decimals differing only
    /// below double precision then collide, which is permitted.
    /// </summary>
    override this.GetHashCode() =
        let v = this.Value
        if v = 0.0 then 0 else v.GetHashCode()
    override this.ToString() = this.ToStringValue()

/// <summary>
/// Represents a string value
/// </summary>
and [<Sealed>] JyroString(value: string) =
    inherit JyroValue()

    do if obj.ReferenceEquals(value, null) then raise (ArgumentNullException(nameof value))

    /// <summary>
    /// The underlying string.
    /// </summary>
    member _.Value = value
    /// <summary>
    /// The number of UTF-16 code units, matching <c>.Length</c> on the underlying
    /// string.
    /// </summary>
    member _.Length = value.Length
    // No indexer (JYRO-ADR-200 item 5): `Value[i]` gives a character, and the
    // indexer was the only member that gave a `JyroValue` for one.

    override _.ValueType = JyroValueType.String
    override this.AsString() = this
    // The three conversions and `ToString` are host conversions (spec
    // 160.8.1): a string of `M` code units has size `M + 1` (spec 100.8.6), so
    // they refuse one that long, although a script can hold it. Engine code
    // that wants the text reads `Value`, which is not bound.
    [<return: System.Diagnostics.CodeAnalysis.NotNull>]
    override this.ToObjectValue() : obj =
        ValueWalk.RequireHostSize this
        nonNull (box value)
    override this.ToStringValue() =
        ValueWalk.RequireHostSize this
        value
    override this.WriteJson(writer) =
        JyroValue.RequireWriter writer
        ValueWalk.RequireHostSize this
        // `Utf8JsonWriter` would substitute U+FFFD here and report success, so
        // `ToJson` silently changed the value on the way out. Only a host
        // reaches this override (the engine's writers write a string
        // themselves), so there is no function to name: the index alone
        // (spec 130.6.2, JYRO-ADR-128).
        let bad = JyroValue.FirstUnpairedSurrogate value
        if bad >= 0 then
            JyroError.raiseRuntime MessageCode.UnpairedSurrogate [| box bad |]
        writer.WriteStringValue(value)

    // Out of range yields null, and an index with no `int` form is out of
    // range - see `JyroNumber.TryToInt32`. Converting first made `s[1e30]`
    // raise `OverflowException` instead, which is a CLR failure standing in
    // for a miss this door already had an answer for.
    //
    // A number that is not an integer raises `IntegerIndexRequired`, whatever the
    // length of the string (spec 30.3.6): it used to be rounded, which read a
    // position the script did not name.
    member internal this.OwnGetIndex(index: JyroValue) : JyroValue =
        match index with
        | :? JyroNumber as n ->
            JyroNumber.RequireIntegerIndex n
            let mutable i = 0
            if n.TryToInt32(&i) && i >= 0 && i < value.Length then
                JyroString(value.[i].ToString()) :> JyroValue
            else
                JyroNull.Instance :> JyroValue
        | _ -> JyroNull.Instance :> JyroValue

    member internal this.OwnToIterable() : seq<JyroValue> =
        value |> Seq.map (fun c -> JyroString(c.ToString()) :> JyroValue)

    member internal this.OwnEvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match op with
        // `+` falls to the base, which raises `StringOperandInAdditionRuntime` for a
        // string on either side (JYRO-ADR-159).
        | Add -> this.DefaultEvaluateBinary(op, right)
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | Concat
        | Equal
        | NotEqual
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | And
        | Or
        | Coalesce ->
            match right with
            | :? JyroString as rs ->
                // Compared a chunk of code units at a time, with the time and
                // the cancellation signal checked between chunks (spec
                // 100.2.5), rather than in one call over the whole string.
                match op with
                | LessThan -> JyroBoolean.FromBoolean(BoundedText.compareOrdinal value rs.Value < 0) :> JyroValue
                | LessThanOrEqual -> JyroBoolean.FromBoolean(BoundedText.compareOrdinal value rs.Value <= 0) :> JyroValue
                | GreaterThan -> JyroBoolean.FromBoolean(BoundedText.compareOrdinal value rs.Value > 0) :> JyroValue
                | GreaterThanOrEqual -> JyroBoolean.FromBoolean(BoundedText.compareOrdinal value rs.Value >= 0) :> JyroValue
                | Add
                | Subtract
                | Multiply
                | Divide
                | Modulo
                | Concat
                | Equal
                | NotEqual
                | And
                | Or
                | Coalesce -> this.DefaultEvaluateBinary(op, right)
            | _ -> this.DefaultEvaluateBinary(op, right)

    override _.EqualsValue(other) =
        match other with
        | :? JyroString as s -> BoundedText.equalsOrdinal s.Value value
        | _ -> false

    override _.Equals(other) =
        match other with
        | :? JyroString as s -> String.Equals(s.Value, value, StringComparison.Ordinal)
        | _ -> false
    override _.GetHashCode() = value.GetHashCode()
    /// <summary>
    /// The same text as <c>ToStringValue</c>, under the same bound.
    /// </summary>
    override this.ToString() = this.ToStringValue()

/// <summary>
/// Represents an array value. A host creates an array empty, with the
/// parameterless constructor, and adds its elements.
/// </summary>
and [<Sealed>] JyroArray internal (capacity: int) =
    inherit JyroValue()

    let items = ResizeArray<JyroValue>(capacity)
    let itemsView = JyroArrayView(items) :> IReadOnlyList<JyroValue>
    let mutable frozen = false
    // Why it was frozen, so the error names the value the author actually hit.
    let mutable frozenAsInput = false

    // Single point at which a rejected mutation picks its code.
    let failReadOnly () : unit =
        if frozenAsInput then JyroError.raiseRuntime MessageCode.InputValueIsReadOnly [||]
        else JyroError.raiseRuntime MessageCode.ValueIsReadOnly [||]

    // The elements, as a view that cannot be written through, for the
    // engine. Internal: a host reads the array itself, which is an
    // `IReadOnlyList<JyroValue>` (JYRO-ADR-200 item 3).
    //
    // This MUST wrap rather than upcast. `items :> IReadOnlyList<_>` compiles
    // and reads as read-only, but the interface is only a view: a caller can
    // cast the same reference back to `List<JyroValue>` and mutate it, which
    // bypasses `Freeze` entirely. The wrapper is allocated once because this
    // is on hot paths (stdlib iteration).
    member internal _.Items = itemsView
    /// <summary>
    /// The number of elements.
    /// </summary>
    member _.Count = items.Count

    /// <summary>
    /// Creates an empty array.
    /// </summary>
    new() = JyroArray(0)

    /// <summary>
    /// True once this array has been frozen (made read-only) by the host.
    /// Implies every array/object reachable from it is frozen - see <c>Freeze</c>.
    /// </summary>
    override _.IsFrozen = frozen

    // Sets the flag without traversing. Only `Freeze`'s marking pass may call
    // this, and only after the collection pass has walked the whole reachable
    // graph successfully.
    //
    // `asInput` only ever turns on. A value frozen as `Input` that a host also
    // froze explicitly is still `Input` to a script author, and that is whose
    // error message this decides.
    member internal _.MarkReadOnly(asInput: bool) =
        frozen <- true
        if asInput then frozenAsInput <- true

    // Collects every array/object reachable from `root` into `acc`.
    //
    // `seen` uses reference identity and is what terminates a cycle - it is
    // deliberately NOT the frozen flag. Using the flag as the visited set is
    // what made the previous implementation wrong: a node marked by an earlier
    // freeze stopped the traversal, so anything unmarked below it was never
    // reached, and `IsFrozen` could be true over a writable subtree. An
    // already-frozen node is therefore still descended through here.
    //
    // Bounded by `ValueRecursionGuard`, so a graph deeper than the cap raises
    // a catchable error rather than exhausting the CLR stack.
    static member internal CollectContainers
        (root: JyroValue, seen: HashSet<JyroValue>, acc: ResizeArray<JyroValue>) : unit =
        let isContainer =
            match root with
            | :? JyroArray | :? JyroObject -> true
            | _ -> false
        if isContainer && seen.Add(root) then
            acc.Add(root)
            ValueRecursionGuard.Enter()
            try
                // Annotated because `JyroObject` is declared later in this
                // recursive group, so its member types are not yet inferred here.
                match root with
                | :? JyroArray as a ->
                    for item in (a.Items : IReadOnlyList<JyroValue>) do
                        JyroArray.CollectContainers(item, seen, acc)
                | :? JyroObject as o ->
                    for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do
                        JyroArray.CollectContainers(kvp.Value, seen, acc)
                | _ -> ()
            finally
                ValueRecursionGuard.Exit()

    // Collects every array/object reachable from `root`, the value of
    // `Input`, into `acc`, and raises `ValueNestingLimitExceeded` when one of
    // them has a depth above the maximum nesting depth of 100.7.1 (spec
    // 70.2.3, JYRO-REQ-276, JYRO-ADR-70).
    //
    // Making `Input` read-only is not a walk under 100.7.5, so a cycle never
    // makes it fail. A container's depth is the number of containers on the
    // shortest path from `root` to it, both ends counted, so this visits
    // breadth first, a level at a time, with a visited set by reference: a
    // container reached again is not counted again, and one reached along
    // two paths has the depth of the shorter. Iterative, so no depth of the
    // value can reach the CLR stack.
    static member internal CollectInputContainers(root: JyroValue, acc: ResizeArray<JyroValue>) : unit =
        let isContainer (v: JyroValue) =
            match v with
            | :? JyroArray | :? JyroObject -> true
            | _ -> false
        if isContainer root then
            let seen = HashSet<JyroValue>(HashIdentity.Reference)
            seen.Add(root) |> ignore
            let mutable level = ResizeArray<JyroValue>([ root ])
            let mutable depth = 1
            while level.Count > 0 do
                if depth > ValueRecursionGuard.MaxDepth then
                    JyroError.raiseRuntime MessageCode.ValueNestingLimitExceeded
                        [| box ValueRecursionGuard.MaxDepth |]
                acc.AddRange(level)
                let next = ResizeArray<JyroValue>()
                let visit (child: JyroValue) =
                    if isContainer child && seen.Add(child) then next.Add(child)
                for container in level do
                    // Annotated because `JyroObject` is declared later in this
                    // recursive group, so its member types are not yet inferred here.
                    match container with
                    | :? JyroArray as a ->
                        for item in (a.Items : IReadOnlyList<JyroValue>) do visit item
                    | :? JyroObject as o ->
                        for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do visit kvp.Value
                    | _ -> ()
                level <- next
                depth <- depth + 1

    // Applies the collected marking. Cannot fail, which is what makes `Freeze`
    // all-or-nothing.
    static member internal ApplyReadOnly(acc: ResizeArray<JyroValue>, asInput: bool) : unit =
        for v in acc do
            match v with
            | :? JyroArray as a -> a.MarkReadOnly(asInput)
            | :? JyroObject as o -> o.MarkReadOnly(asInput)
            | _ -> ()

    /// <summary>
    /// Deeply and idempotently freezes this array and every nested array/object, making
    /// it read-only: any later mutation raises a runtime error. Scalars are already
    /// immutable. Reads are unaffected. Returns the array for chaining.
    ///
    /// All or nothing: if the graph is too deep, no value is marked and the
    /// caller's values are exactly as they were.
    ///
    /// Invariant: <c>IsFrozen</c> implies every array/object reachable from this one
    /// is frozen, for cyclic graphs too.
    /// </summary>
    override this.Freeze() : JyroValue =
        if not frozen then
            let seen = HashSet<JyroValue>(HashIdentity.Reference)
            let acc = ResizeArray<JyroValue>()
            JyroArray.CollectContainers(this, seen, acc)
            JyroArray.ApplyReadOnly(acc, false)
        this :> JyroValue

    /// <summary>
    /// Reads or writes the element at <c>index</c>. A read out of range gives
    /// <c>JyroNull.Instance</c>, never a null reference. A write may replace an
    /// existing element or append exactly one past the end, and any other
    /// index raises <c>IndexOutOfRange</c>. A null reference written is stored as
    /// <c>JyroNull.Instance</c>.
    /// </summary>
    member _.Item
        with get(index: int) =
            if index >= 0 && index < items.Count then items.[index]
            else JyroNull.Instance
        and set(index: int) ([<System.Diagnostics.CodeAnalysis.AllowNull>] value: JyroValue) =
            if frozen then failReadOnly ()
            // An index write may replace an existing element or append exactly
            // one past the end. It MUST NOT pad.
            //
            // Padding made a single statement allocate without bound:
            // `arr[50000000] = 1` on an empty array added fifty million nulls,
            // and the resource limits of Part 100 could not see it. They bound
            // how many operations a script performs - statements, loop
            // iterations, call depth - not how much any one of them allocates.
            // Measured before this change: six statements retained 1.5 GB,
            // inside every default limit, while the honest route (appending in
            // a loop) tripped the 1,000-iteration budget after 57 ms.
            //
            // Growing an array now costs one operation per element, which is
            // exactly what the loop budget already bounds. A negative index no
            // longer silently discards the write either.
            if index < 0 || index > items.Count then
                JyroError.raiseRuntime MessageCode.IndexOutOfRange
                    [| box "array"; box index; box items.Count |]
            let stored = if obj.ReferenceEquals(value, null) then JyroNull.Instance :> JyroValue else value
            if index = items.Count then items.Add(stored)
            else items.[index] <- stored

    /// <summary>
    /// Appends a value. A null reference is stored as <c>JyroNull.Instance</c>.
    /// </summary>
    member _.Add(value: JyroValue | null) =
        if frozen then failReadOnly ()
        items.Add(match value with | null -> JyroNull.Instance :> JyroValue | value -> value)

    // `Add` for an array a script operation makes or grows: refuses the
    // element that would take it above the maximum value size (spec 100.8.7),
    // leaving the array as it was (spec 100.8.8).
    //
    // Separate from `Add` because the bound does not apply to a value the
    // host makes (spec 100.8.7), and `Add` is how a host makes one.
    member internal this.AddBounded(value: JyroValue) =
        if not frozen && items.Count >= ValueSize.Maximum then ValueSize.Raise()
        this.Add(value)

    // `SetIndex` for the script's assignment `a[i] = v`: the append position
    // is refused once the array holds the maximum number of elements (spec
    // 100.8.7, 100.8.8). Immutability and the index are judged first, as
    // `SetIndex` judges them.
    member internal this.SetIndexBounded(index: JyroValue, value: JyroValue) =
        if not frozen then
            match index with
            | :? JyroNumber as n ->
                JyroNumber.RequireIntegerIndex n
                let mutable i = 0
                if n.TryToInt32(&i) && i = items.Count && items.Count >= ValueSize.Maximum then
                    ValueSize.Raise()
            | _ -> ()
        this.SetIndex(index, value)

    /// <summary>
    /// Inserts at <c>index</c>, which may be <c>Count</c> - that position appends.
    ///
    /// An index outside 0 to <c>Count</c> raises a <c>JyroRuntimeException</c> with
    /// <c>IndexOutOfRange</c>, as the standard library's <c>Insert</c> does. A frozen
    /// array raises its read-only error first, whatever the index.
    /// </summary>
    member _.Insert(index: int, value: JyroValue | null) =
        if frozen then failReadOnly ()
        if index < 0 || index > items.Count then
            JyroError.raiseRuntime MessageCode.IndexOutOfRange
                [| box "array"; box index; box items.Count |]
        items.Insert(index, match value with | null -> JyroNull.Instance :> JyroValue | value -> value)

    /// <summary>
    /// Removes the element at <c>index</c>, which must name one that exists.
    ///
    /// The bound is <c>Count - 1</c> where <c>Insert</c>'s is <c>Count</c>, and the
    /// difference is load-bearing rather than an oversight: <c>Insert</c> at
    /// <c>Count</c> appends, while there is no element at <c>Count</c> to remove. Its
    /// stdlib twin draws the line in the same place and for the same reason.
    /// </summary>
    member _.RemoveAt(index: int) =
        if frozen then failReadOnly ()
        if index < 0 || index >= items.Count then
            JyroError.raiseRuntime MessageCode.IndexOutOfRange
                [| box "array"; box index; box items.Count |]
        items.RemoveAt(index)
    /// <summary>
    /// Removes every element.
    /// </summary>
    member _.Clear() =
        if frozen then failReadOnly ()
        items.Clear()

    // Converts a host sequence. See the note on `JyroObject.FromDictionary`
    // for the null contract and the depth guard shared by all three host-graph
    // converters. Internal: `JyroValue.FromObject` is the one public converter
    // from .NET values (JYRO-ADR-200 item 1).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromEnumerable(source: IEnumerable<obj | null>) =
        if obj.ReferenceEquals(source, null) then raise (ArgumentNullException(nameof source))
        ValueRecursionGuard.Enter()
        try
            let result = JyroArray()
            for item in source do
                result.Add(JyroValue.FromObject(item))
            result
        finally
            ValueRecursionGuard.Exit()

    override _.ValueType = JyroValueType.Array
    override this.AsArray() = this
    // A snapshot of the elements, not a live view.
    //
    // This used to be `items |> Seq.cast`, which is lazy over the underlying
    // `List<T>`, so a script that wrote to the array it was iterating hit the
    // enumerator's version check and the run died with a raw .NET string -
    // "Collection was modified; enumeration operation may not execute." -
    // carried as a generic RuntimeError naming nothing about the script.
    //
    // Worse than the message was the incoherence. `List<T>` bumps its version
    // on an indexer *set* and `Dictionary<K,V>` does not, and
    // `Dictionary.Remove` stopped invalidating enumerators in .NET Core 3.0 -
    // so `a[0] = 99` aborted while `o.a = 99` completed, and which of those a
    // Jyro loop did was decided by the host runtime's enumerator rules rather
    // than by this language. Those rules have already changed once underneath
    // this engine, in a language whose specification expects re-implementations
    // on other platforms.
    //
    // Copying costs one shallow array of references per loop, bounded by a
    // collection that already exists, and buys one rule for both containers
    // and every operation: the loop visits what the collection held when it
    // began. No working script is affected, because every shape whose meaning
    // this changes used to abort.
    member internal this.OwnToIterable() : seq<JyroValue> = items.ToArray() :> seq<JyroValue>

    // Out of range yields null, and an index with no `int` form is out of
    // range - see `JyroNumber.TryToInt32`. Converting first made `a[1e30]`
    // raise `OverflowException` instead, which is a CLR failure standing in
    // for a miss this door already had an answer for.
    //
    // A number that is not an integer raises `IntegerIndexRequired` (spec
    // 30.4.10), in range or not: it used to be rounded.
    member internal this.OwnGetIndex(index: JyroValue) : JyroValue =
        match index with
        | :? JyroNumber as n ->
            JyroNumber.RequireIntegerIndex n
            let mutable i = 0
            if n.TryToInt32(&i) && i >= 0 && i < items.Count then items.[i]
            else JyroNull.Instance
        | _ -> JyroNull.Instance

    member internal this.OwnSetIndex(index: JyroValue, value: JyroValue) : unit =
        // Immutability first, before the index is judged at all. `Insert` and
        // `RemoveAt` already draw the line here - "a frozen array's index
        // validity is not the fact the caller needs to hear about" - and the
        // numeric arm below inherited it by writing through the indexer. The
        // wrong-type arm did not, so a frozen array reported `InvalidIndexType`
        // where its own sibling members reported `ValueIsReadOnly`, and a write
        // through an alias of `Input` blamed the index expression for a value
        // that was never the script's to write.
        //
        // Immutability is the fact the author has to act on, and it does not
        // stop being true if they also correct the index.
        if frozen then failReadOnly ()
        match index with
        // No `i >= 0` guard: the indexer reports an out-of-range write rather
        // than discarding it, and that includes a negative index.
        //
        // An index with no `int` form is out of range too, and must reach the
        // same report rather than `OverflowException` from the conversion.
        // The two doors diverge here on purpose: the getter above answers a
        // miss with null, while a write that names no position is reported,
        // because silently discarding it lets the script proceed as though it
        // happened. The index is named by the number the caller actually
        // wrote, since 1e30 has no `int` form to name it by.
        | :? JyroNumber as n ->
            // Not rounded: a fraction names no position (spec 30.4.10), and
            // the array is left as it was.
            JyroNumber.RequireIntegerIndex n
            let mutable i = 0
            if n.TryToInt32(&i) then this.[i] <- value
            else
                JyroError.raiseRuntime MessageCode.IndexOutOfRange
                    [| box "array"; box (n.ToStringValue()); box items.Count |]
        // An index that is not a number names no position at all, and that is
        // reported for the same reason a number out of range is: the write has
        // nowhere to go, and discarding it silently lets the script carry on as
        // though it had happened. This arm used to be `| _ -> ()`, so
        // `a['x'] = 9` did nothing and said nothing while `a[-1] = 9` was
        // reported - two halves of one door with different rules.
        | _ ->
            JyroError.raiseRuntime MessageCode.InvalidIndexType
                [| box (TypeNames.typeOfName JyroValueType.Array); box (TypeNames.typeOfName index.ValueType) |]

    // No override of EvaluateBinary: arithmetic has one meaning, on numbers
    // (spec 40.2.4), so `[1] + [2]` raises `UnsupportedBinaryOperation` from
    // the base like any other non-number operand, except where 40.2.2
    // applies: `[1] + s` with `s` a string raises `StringOperandInAdditionRuntime`,
    // also from the base (JYRO-ADR-159). It used to give the concatenation
    // of the two arrays.

    // The three conversions and `ToString` are host conversions (spec
    // 160.8.1). Each first measures the value - its size over the tree a walk
    // unfolds (spec 100.8.6), and whether it is cyclic or too deep - and
    // refuses it before making a host value or writing text (spec 160.8.2).
    // See `ValueWalk`.
    [<return: System.Diagnostics.CodeAnalysis.NotNull>]
    override this.ToObjectValue() : obj = ValueWalk.HostObject(this)

    override this.ToStringValue() = ValueWalk.HostText(this)

    // The text of `ToStringValue`, under the same bound (spec 160.8.1). This
    // gave the shape (`JyroArray[n]`), so a debugger or a log call rendering a
    // value did not walk it; the specification now binds it instead. Equality
    // is left as reference identity - see the note on `JyroValue.EqualsValue`.
    override this.ToString() = this.ToStringValue()

    override this.WriteJson(writer) =
        JyroValue.RequireWriter writer
        ValueWalk.HostWriteJson(writer, this)

    override _.EqualsValue(other) =
        match other with
        | :? JyroArray as a when a.Items.Count = items.Count ->
            // An indexed loop (no enumerator/closure) keeps the per-level
            // CLR frame minimal so the depth guard trips well before the
            // native stack is exhausted; it also short-circuits on the first
            // mismatch.
            ValueRecursionGuard.Enter()
            try
                let mutable equal = true
                let mutable i = 0
                while equal && i < items.Count do
                    // A walk of two containers checks the time and the
                    // cancellation signal (spec 100.2.5): a value that holds
                    // one array at many places unfolds into a tree far larger
                    // than the value.
                    ExecutionCheck.Tick()
                    if not (items.[i].EqualsValue(a.Items.[i])) then equal <- false
                    i <- i + 1
                equal
            finally
                ValueRecursionGuard.Exit()
        | _ -> false

    /// <summary>
    /// Index of the first element equal to <c>value</c> by the script's <c>==</c>, or -1.
    /// </summary>
    member _.IndexOf(value: JyroValue) : int =
        let mutable i = 0
        let mutable found = -1
        while found < 0 && i < items.Count do
            ExecutionCheck.Tick()
            if items.[i].EqualsValue(value) then found <- i
            i <- i + 1
        found

    /// <summary>
    /// True when some element equals <c>value</c> by the script's <c>==</c> (see
    /// <c>IndexOf</c>).
    /// </summary>
    member this.Contains(value: JyroValue) : bool = this.IndexOf(value) >= 0

    /// <summary>
    /// Reads an element without conflating a miss with a stored null, which is
    /// what the indexer does - it yields <c>JyroNull</c> for both. The array's
    /// counterpart of <c>JyroObject.TryGetValue</c>.
    /// </summary>
    member _.TryGetItem(index: int, [<System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] value: outref<JyroValue>) : bool =
        if index >= 0 && index < items.Count then
            value <- items.[index]
            true
        else
            value <- JyroNull.Instance
            false

    // The array is the host's read-only view of its elements: LINQ, `Count`,
    // indexing and `foreach` work on it directly, and a collection
    // initializer works through `Add`.
    interface IReadOnlyList<JyroValue> with
        member _.Count = items.Count
        // Null out of range, matching the class indexer above and `Items`.
        member _.Item
            with get(index: int) =
                if index >= 0 && index < items.Count then items.[index]
                else JyroNull.Instance :> JyroValue
        member _.GetEnumerator() = items.GetEnumerator() :> IEnumerator<JyroValue>
        member _.GetEnumerator() = items.GetEnumerator() :> System.Collections.IEnumerator

/// <summary>
/// Represents an object value (key-value map)
///
/// The constructor that takes a capacity is internal, as on
/// <c>JyroArray</c>: an object result of known count made during a run is created
/// at that count, so growth does not rehash it.
/// </summary>
and [<Sealed>] JyroObject internal (capacity: int) =
    inherit JyroValue()

    // In the order the properties were added (spec 30.5.9): see
    // `OrderedProperties`.
    let properties = OrderedProperties<JyroValue>(capacity)
    let propertiesView = JyroObjectView(properties) :> IReadOnlyDictionary<string, JyroValue>
    let mutable frozen = false
    // See JyroArray: why it was frozen, so the error names the right value.
    let mutable frozenAsInput = false

    let failReadOnly () : unit =
        if frozenAsInput then JyroError.raiseRuntime MessageCode.InputValueIsReadOnly [||]
        else JyroError.raiseRuntime MessageCode.ValueIsReadOnly [||]

    // The properties, as a view that cannot be written through, for the
    // engine. Internal: a host reads the object itself, which is an
    // `IReadOnlyDictionary<string, JyroValue>` (JYRO-ADR-200 item 3). See
    // the note on `JyroArray.Items`: this MUST wrap rather than upcast, or a
    // caller can cast back to `Dictionary<_,_>` and mutate a frozen object.
    member internal _.Properties = propertiesView
    // The storage itself, for the engine's walks that must not allocate per
    // node: its `GetEnumerator` is a struct, where the view's, reached
    // through the interface, is boxed on every call (spec 160.8.2).
    member internal _.PropertyStore : OrderedProperties<JyroValue> = properties
    /// <summary>
    /// The number of properties.
    /// </summary>
    member _.Count = properties.Count

    /// <summary>
    /// Creates an empty object.
    /// </summary>
    new() = JyroObject(0)

    /// <summary>
    /// True once this object has been frozen (made read-only) by the host.
    /// Implies every array/object reachable from it is frozen - see <c>Freeze</c>.
    /// </summary>
    override _.IsFrozen = frozen

    // Sets the flag without traversing. See `JyroArray.MarkReadOnly`.
    member internal _.MarkReadOnly(asInput: bool) =
        frozen <- true
        if asInput then frozenAsInput <- true

    /// <summary>
    /// Deeply and idempotently freezes this object and every nested array/object, making
    /// it read-only: any later mutation raises a runtime error. Scalars are already
    /// immutable. Reads are unaffected. Returns the object for chaining.
    ///
    /// See <c>JyroArray.Freeze</c> for why this is two passes and what invariant the
    /// split buys.
    /// </summary>
    override this.Freeze() : JyroValue =
        if not frozen then
            let seen = HashSet<JyroValue>(HashIdentity.Reference)
            let acc = ResizeArray<JyroValue>()
            JyroArray.CollectContainers(this, seen, acc)
            JyroArray.ApplyReadOnly(acc, false)
        this :> JyroValue

    /// <summary>
    /// Reads or writes a property by key. A read of a missing key gives
    /// <c>JyroNull.Instance</c>, never a null reference. A null reference written
    /// is stored as <c>JyroNull.Instance</c>.
    /// </summary>
    member _.Item
        with get(key: string) =
            match properties.TryGetValue(key) with
            | true, v -> v
            | _ -> JyroNull.Instance
        and set(key: string) ([<System.Diagnostics.CodeAnalysis.AllowNull>] value: JyroValue) =
            if frozen then failReadOnly ()
            properties.Set(key, (if obj.ReferenceEquals(value, null) then JyroNull.Instance :> JyroValue else value))

    /// <summary>
    /// Looks a property up by its key, exactly as written. A key containing a
    /// dot is a key, not a path: <c>GetProperty("a.b")</c> reads the property named
    /// <c>a.b</c>, as the indexer, <c>TryGetValue</c> and a script's <c>o['a.b']</c> do, and
    /// after <c>SetProperty("x.y", v)</c> it gives <c>v</c>. A missing key gives
    /// <c>JyroNull.Instance</c>. A host that navigates a path splits it and reads
    /// each key in turn.
    /// </summary>
    override _.GetProperty(key: string) =
        match properties.TryGetValue(key) with
        | true, v -> v
        | _ -> JyroNull.Instance

    override _.SetProperty(key: string, value: JyroValue | null) =
        if frozen then failReadOnly ()
        properties.Set(key, (match value with | null -> JyroNull.Instance :> JyroValue | value -> value))

    // `SetProperty` for an object a script operation makes or grows: a new
    // key is refused once the object holds the maximum number of properties
    // (spec 100.8.7), leaving it as it was (spec 100.8.8). Writing a key it
    // already holds does not grow it.
    //
    // Separate from `SetProperty` because the bound does not apply to a value
    // the host makes (spec 100.8.7), and `SetProperty` is how a host makes one.
    member internal this.SetPropertyBounded(key: string, value: JyroValue) =
        if not frozen && properties.Count >= ValueSize.Maximum && not (properties.ContainsKey key) then
            ValueSize.Raise()
        this.SetProperty(key, value)

    member internal this.OwnGetIndex(index: JyroValue) : JyroValue =
        match index with
        | :? JyroString as s -> this.GetProperty(s.Value)
        | _ -> JyroNull.Instance

    member internal this.OwnSetIndex(index: JyroValue, value: JyroValue) : unit =
        // Immutability first, for the reason given on `JyroArray.SetIndex`.
        if frozen then failReadOnly ()
        match index with
        | :? JyroString as s -> this.SetProperty(s.Value, value)
        // The array's counterpart, and the same rule: `o[1] = 5` named no key,
        // so there was nothing to write and nothing was written - silently.
        // The read at the same index still answers null, which is not an
        // inconsistency: a read has a settled answer for every miss and a write
        // has none.
        | _ ->
            JyroError.raiseRuntime MessageCode.InvalidIndexType
                [| box (TypeNames.typeOfName JyroValueType.Object); box (TypeNames.typeOfName index.ValueType) |]

    // A snapshot of the values, for the reason given on `JyroArray`'s
    // equivalent. This side was the more misleading of the two: adding a key
    // during iteration aborted the run, while setting an existing key or
    // deleting one completed - three operations on one container with two
    // answers, decided by `Dictionary<K,V>`'s version bookkeeping rather than
    // by anything this language says.
    member internal this.OwnToIterable() : seq<JyroValue> = properties.ValuesArray() :> seq<JyroValue>

    /// <summary>
    /// Look up a property without allocating a miss. On a miss the out
    /// parameter is set to <c>JyroNull.Instance</c>, never to <c>null</c>, so a caller
    /// that ignores the bool still gets a usable <c>JyroValue</c>.
    /// </summary>
    member _.TryGetValue(key: string, [<System.Diagnostics.CodeAnalysis.MaybeNullWhen(false)>] value: outref<JyroValue>) =
        match properties.TryGetValue(key) with
        | true, v -> value <- v; true
        | _ -> value <- JyroNull.Instance; false

    /// <summary>
    /// Whether the object has a property of <c>key</c>, exactly as written.
    /// </summary>
    member _.ContainsKey(key: string) : bool = properties.ContainsKey(key)

    /// <summary>
    /// The keys of the properties, in the order of the object.
    /// </summary>
    member _.Keys : IEnumerable<string> = properties.Keys

    /// <summary>
    /// The values of the properties, in the order of the object.
    /// </summary>
    member _.Values : IEnumerable<JyroValue> = properties.Values

    /// <summary>
    /// Adds a property of <c>key</c> at the end of the object, so
    /// that a collection initializer makes an object. A
    /// null reference for <c>value</c> is stored as <c>JyroNull.Instance</c>.
    ///
    /// Raises the error a host write to a read-only object gets
    /// when the object is read-only, and otherwise <c>ArgumentException</c>, whose
    /// <c>ParamName</c> is <c>key</c>, when the object already has a property of <c>key</c>,
    /// leaving the object unchanged in both cases.
    /// </summary>
    member _.Add(key: string, value: JyroValue | null) : unit =
        if obj.ReferenceEquals(key, null) then raise (ArgumentNullException(nameof key))
        if frozen then failReadOnly ()
        if properties.ContainsKey(key) then
            raise (ArgumentException($"The object already has a property with the key '{key}'.", nameof key))
        properties.Set(key, (match value with | null -> JyroNull.Instance :> JyroValue | value -> value))

    /// <summary>
    /// Removes a property by key, returning whether it was present.
    /// </summary>
    member _.Remove(key: string) =
        if frozen then failReadOnly ()
        properties.Remove(key)
    /// <summary>
    /// Removes every property.
    /// </summary>
    member _.Clear() =
        if frozen then failReadOnly ()
        properties.Clear()

    /// <summary>
    /// Host-facing convenience: the same conversion as <c>ToObjectValue</c>, but
    /// typed as the concrete dictionary so callers need no downcast.
    /// It is bounded by the maximum nesting depth, as <c>ToObjectValue</c> is.
    /// A value is a null reference for each property that holds the Jyro
    /// null.
    /// </summary>
    member this.ToDictionary() : Dictionary<string, obj | null> =
        this.ToObjectValue() :?> Dictionary<string, obj | null>

    // Converts a host dictionary.
    //
    // The three host-graph converters - this, `FromComplexObject` and
    // `JyroArray.FromEnumerable` - share one contract. All three are
    // internal, reached only through `JyroValue.FromObject`, the one public
    // converter from .NET values (JYRO-ADR-200 item 1).
    //
    // **Null.** Across the host-facing conversion surface the rule is:
    // normalize a null *value*, reject a null *container or resource*.
    // `JyroValue.FromObject`, `JyroArray.Add` and `JyroObject.SetProperty`
    // each take a value, so null becomes `JyroNull.Instance`. These three
    // converters take something to read from, as do `JyroValue.FromJson` and
    // `JyroString`'s constructor, so null is `ArgumentNullException`.
    //
    // The rule is deliberately NOT "reject wherever `JyroNull` is
    // inexpressible" - that formulation is false in both directions, so do
    // not restate it: `FromJson` returns a `JyroValue` and still rejects,
    // while `JyroArguments.GetArray` returns a `JyroArray` and
    // still normalizes. That last one is a different layer: the argument
    // helpers are script-facing, where `JyroNull` is a legitimate value
    // rather than a missing container.
    //
    // **Depth.** A cyclic or very deep host graph passed via `WithInput(obj)`
    // recurses through `FromObject` and would otherwise overflow the CLR
    // stack, so each converter charges one `ValueRecursionGuard` level for
    // the container it builds.
    //
    // Validation runs BEFORE `Enter()`. That is not for counter balance - a
    // check inside the `try` is balanced by the `finally` too, as was the
    // unchecked dereference this replaced. It is for determinism: after
    // `Enter()`, a call made while the counter already sits at the cap would
    // trip the guard first and report a null argument as
    // `ValueNestingLimitExceeded`, making argument validation depend on how
    // deep the current walk happens to be.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromDictionary(dict: IDictionary<string, obj | null>) =
        if obj.ReferenceEquals(dict, null) then raise (ArgumentNullException(nameof dict))
        ValueRecursionGuard.Enter()
        try
            let result = JyroObject()
            for kvp in dict do
                result.SetProperty(kvp.Key, JyroValue.FromObject(kvp.Value))
            result
        finally
            ValueRecursionGuard.Exit()

    // Converts a host dictionary whose values are of type `'T`, an
    // `IDictionary<string, 'T>` or `IReadOnlyDictionary<string, 'T>`, in
    // the order it enumerates its entries (JYRO-ADR-152 (1), spec 30.5.10).
    // `JyroValue.FromObject` reaches it by a delegate made once per `'T`.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromEntries<'T>(source: obj) : JyroValue =
        let entries = source :?> IEnumerable<KeyValuePair<string, 'T>>
        ValueRecursionGuard.Enter()
        try
            let result = JyroObject()
            for kvp in entries do
                result.SetProperty(kvp.Key, JyroValue.FromObject(box kvp.Value))
            result :> JyroValue
        finally
            ValueRecursionGuard.Exit()

    // Converts a non-generic host dictionary whose keys are all strings, in
    // the order it enumerates its entries (JYRO-ADR-152 (1)).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromNonGenericDictionary(dict: System.Collections.IDictionary) =
        ValueRecursionGuard.Enter()
        try
            let result = JyroObject()
            let e = dict.GetEnumerator()
            while e.MoveNext() do
                result.SetProperty(e.Key :?> string, JyroValue.FromObject(e.Value))
            result
        finally
            ValueRecursionGuard.Exit()

    // The entry of a host dictionary that is a sequence of its entries, a
    // `KeyValuePair<K, T>` or a `DictionaryEntry`: an object of exactly two
    // properties, `Key` then `Value`, each converted by `FromObject`
    // (JYRO-ADR-154 (1d)).
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromEntry(key: obj | null, value: obj | null) : JyroObject =
        ValueRecursionGuard.Enter()
        try
            let result = JyroObject()
            result.SetProperty("Key", JyroValue.FromObject(key))
            result.SetProperty("Value", JyroValue.FromObject(value))
            result
        finally
            ValueRecursionGuard.Exit()

    // A `KeyValuePair<'K, 'T>`, its `Key` and `Value` read as members of the
    // pair, not by reflection. `JyroValue.FromObject` reaches it by a
    // delegate made once per pair type.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromPair<'K, 'T>(source: obj) : JyroValue =
        let pair = source :?> KeyValuePair<'K, 'T>
        JyroObject.FromEntry(box pair.Key, box pair.Value) :> JyroValue

    // Converts a host POCO by reflecting over its public readable properties
    // and its public fields (JYRO-ADR-156 (d), (e)). See the note on
    // `FromDictionary` for the null contract and the depth guard shared by
    // all three host-graph converters.
    //
    // This walk is mutually recursive with `FromObject`, which routes every
    // object that is not a number, a string, a JSON value, a dictionary, a
    // sequence or a value of a text-form type here (JYRO-ADR-152 (4)), and
    // converts each property and field value. A property whose getter
    // throws is left out, as it is when `FromObject` reaches this. It charges a level of the depth guard,
    // because the POCO IS a level of nesting and its siblings each charge for
    // the container they build, and it enters `HostGraphCycleGuard`, because
    // a self-referencing property would otherwise recurse until the CLR stack
    // ran out - an uncatchable StackOverflowException. A cyclic host graph is
    // refused: when a property leads back to an object already being
    // reflected, `FromObject` raises `ValueNestingLimitExceeded` (spec
    // 100.7.5, 100.7.6), and that exception passes out through here.
    [<Diagnostics.CodeAnalysis.RequiresUnreferencedCode(HostShapes.ReflectsHostTypes)>]
    static member internal FromComplexObject(value: obj) =
        if obj.ReferenceEquals(value, null) then raise (ArgumentNullException(nameof value))
        ValueRecursionGuard.Enter()
        // Bounds depth AND repetition. The depth guard alone cannot save a
        // self-referencing host object, because `FromObject` routes back here -
        // reflection frames are heavy enough that the CLR stack goes first.
        HostGraphCycleGuard.Enter(value)
        try
            let result = JyroObject()
            let t = value.GetType()
            // Public readable instance properties, then public instance
            // fields, each group in the order .NET lists it (JYRO-ADR-156
            // (d)); that is the object's order under spec 30.5.10, and so the
            // order of the first-number walk of spec 30.2.19. A static member
            // is the type's, not this value's.
            for prop in HostShapes.readableProperties t do
                // A property whose getter throws is left out, and the object
                // stays an object (JYRO-ADR-156 (e)): `MemoryStream`'s
                // `ReadTimeout`, an F# `ValueNone`'s `Value`, a faulted
                // task's `Result`. The getter's exception is the host's; only
                // reading the property is guarded, so the walk's own
                // `JyroRuntimeException` for the value read still passes out.
                let read =
                    try ValueSome (prop.GetValue(value))
                    with _ -> ValueNone
                match read with
                | ValueSome propValue -> result.SetProperty(prop.Name, JyroValue.FromObject(propValue))
                | ValueNone -> ()
            // A `ValueTuple`'s elements, `Item1`, `Item2`, ..., `Rest`, the
            // components of a `System.Numerics.Vector2`, a struct's data.
            for field in HostShapes.publicFields t do
                result.SetProperty(field.Name, JyroValue.FromObject(field.GetValue(value)))
            result
        finally
            HostGraphCycleGuard.Exit(value)
            ValueRecursionGuard.Exit()

    override _.ValueType = JyroValueType.Object
    override this.AsObject() = this

    // Host conversions (spec 160.8.1, 160.8.2). See the note on `JyroArray`'s.
    [<return: System.Diagnostics.CodeAnalysis.NotNull>]
    override this.ToObjectValue() : obj = ValueWalk.HostObject(this)

    override this.ToStringValue() = ValueWalk.HostText(this)

    override this.WriteJson(writer) =
        JyroValue.RequireWriter writer
        ValueWalk.HostWriteJson(writer, this)

    /// <summary>
    /// The text of <c>ToStringValue</c>. See the note on <c>JyroArray.ToString</c>.
    /// </summary>
    override this.ToString() = this.ToStringValue()

    override _.EqualsValue(other) =
        match other with
        | :? JyroObject as o when o.Count = properties.Count ->
            // Enumerate manually so we stop on the first mismatch (as the old
            // Seq.forall did); the guard bounds native recursion depth so a
            // cyclic or pathologically deep object cannot overflow the CLR stack.
            ValueRecursionGuard.Enter()
            try
                let mutable equal = true
                let mutable e = properties.GetEnumerator()
                while equal && e.MoveNext() do
                    ExecutionCheck.Tick()
                    let kvp = e.Current
                    match o.Properties.TryGetValue(kvp.Key) with
                    | true, v -> if not (kvp.Value.EqualsValue(v)) then equal <- false
                    | _ -> equal <- false
                equal
            finally
                ValueRecursionGuard.Exit()
        | _ -> false

    // The object is the host's read-only view of its properties; `Count`,
    // `ContainsKey`, `Keys`, `Values` and `TryGetValue` are also public on the
    // class, so a C# host calls them without a cast. Enumerating a JyroObject
    // yields key/value pairs, while the script's `foreach` (`ToIterable`)
    // yields values: each matches its own convention.
    interface IReadOnlyDictionary<string, JyroValue> with
        member _.Count = properties.Count
        // Null on a miss, matching the class indexer, `GetProperty`,
        // `TryGetValue`, `GetIndex` and `Properties`.
        member _.Item
            with get(key: string) =
                match properties.TryGetValue(key) with
                | true, v -> v
                | _ -> JyroNull.Instance :> JyroValue
        member _.Keys = properties.Keys
        member _.Values = properties.Values
        member _.ContainsKey(key: string) = properties.ContainsKey(key)
        member _.TryGetValue(key: string, value: byref<JyroValue>) = properties.TryGetValue(key, &value)
        member _.GetEnumerator() = properties.GetEnumerator() :> IEnumerator<KeyValuePair<string, JyroValue>>
        member _.GetEnumerator() = properties.GetEnumerator() :> System.Collections.IEnumerator

// A read-only view over a `JyroArray`'s storage whose indexer answers an
// out-of-range read with null.
//
// `ReadOnlyCollection<T>` was used here and throws instead, which left three
// doors onto the same data giving two answers: `arr[5]` was null,
// `arr.Items[5]` and the cast `((IReadOnlyList<JyroValue>)arr)[5]` threw. The
// sharpest part was that `Items` and the cast have the *same static type*, so
// the behavior depended on how the reference had been obtained.
//
// It still MUST wrap rather than upcast the underlying list: the interface is
// only a view, and a caller who casts the same reference back to
// `List&lt;JyroValue&gt;` could mutate it, bypassing `Freeze` entirely. Allocated
// once per array, because stdlib iteration is a hot path.
and [<Sealed>] internal JyroArrayView(items: ResizeArray<JyroValue>) =
    interface IReadOnlyList<JyroValue> with
        member _.Count = items.Count
        member _.Item
            with get(index: int) =
                if index >= 0 && index < items.Count then items.[index]
                else JyroNull.Instance :> JyroValue
        member _.GetEnumerator() : IEnumerator<JyroValue> =
            (items :> seq<JyroValue>).GetEnumerator()
        member _.GetEnumerator() : System.Collections.IEnumerator =
            (items :> System.Collections.IEnumerable).GetEnumerator()

// The `JyroObject` counterpart. `ReadOnlyDictionary` raises
// `KeyNotFoundException` on a miss where every other lookup door on the type
// yields null.
and [<Sealed>] internal JyroObjectView(properties: OrderedProperties<JyroValue>) =
    interface IReadOnlyDictionary<string, JyroValue> with
        member _.Count = properties.Count
        member _.Item
            with get(key: string) =
                match properties.TryGetValue(key) with
                | true, v -> v
                | _ -> JyroNull.Instance :> JyroValue
        member _.Keys = properties.Keys
        member _.Values = properties.Values
        member _.ContainsKey(key: string) = properties.ContainsKey(key)
        member _.TryGetValue(key: string, value: byref<JyroValue>) =
            properties.TryGetValue(key, &value)
        member _.GetEnumerator() : IEnumerator<KeyValuePair<string, JyroValue>> =
            (properties :> seq<KeyValuePair<string, JyroValue>>).GetEnumerator()
        member _.GetEnumerator() : System.Collections.IEnumerator =
            (properties :> System.Collections.IEnumerable).GetEnumerator()

/// <summary>
/// Represents a lambda function value (used as inline arguments to higher-order stdlib functions).
/// The execution context is captured by the lambda closure, so Invoke only takes args.
/// </summary>
and [<Sealed>] JyroFunction(invoke: Func<IReadOnlyList<JyroValue>, JyroValue>, paramCount: int) =
    inherit JyroValue()

    /// <summary>
    /// The number of parameters the underlying delegate expects.
    /// </summary>
    member _.ParameterCount = paramCount
    /// <summary>
    /// Calls the underlying delegate with <c>args</c>.
    /// </summary>
    member _.Invoke(args: IReadOnlyList<JyroValue>) = invoke.Invoke(args)

    // Static factory method for creating JyroFunction values from expression trees.
    // Using Expression.Call with a static method avoids Mono WASM interpreter issues
    // with nested Expression.Lambda inside Expression.New. Internal: a host
    // makes a function by the constructor (JYRO-ADR-200 item 5).
    static member internal Create(invoke: Func<IReadOnlyList<JyroValue>, JyroValue>, paramCount: int) : JyroValue =
        JyroFunction(invoke, paramCount) :> JyroValue

    override _.ValueType = JyroValueType.Function
    override this.AsFunction() = this
    [<return: System.Diagnostics.CodeAnalysis.NotNull>]
    override _.ToObjectValue() : obj = nonNull (box "<function>")
    override _.ToStringValue() = "<function>"
    /// <summary>
    /// The text of <c>ToStringValue</c>.
    /// </summary>
    override _.ToString() = "<function>"
    override _.WriteJson(writer) =
        JyroValue.RequireWriter writer
        writer.WriteStringValue("<function>")
    override this.EqualsValue(other) = Object.ReferenceEquals(this, other)

    member internal this.OwnEvaluateBinary(op: BinaryOp, right: JyroValue) : JyroValue =
        match op with
        | Equal -> JyroBoolean.FromBoolean(Object.ReferenceEquals(this, right)) :> JyroValue
        | NotEqual -> JyroBoolean.FromBoolean(not (Object.ReferenceEquals(this, right))) :> JyroValue
        | Add
        | Subtract
        | Multiply
        | Divide
        | Modulo
        | Concat
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | And
        | Or
        | Coalesce -> this.DefaultEvaluateBinary(op, right)

// The walks of a value that the bounds of Part 100 need: its size over the
// tree a walk unfolds (spec 100.8.6), whether it is cyclic or too deep, and
// the text, JSON and host forms made under the maximum value size (spec
// 100.8.7, 160.8.1).
//
// Every walk counts one unit of work per node for the execution check (spec
// 100.2.5): a value that holds one container at many places is small, and the
// tree it unfolds into is not.
and [<AbstractClass; Sealed>] internal ValueWalk =

    static member private TooDeep() : 'T =
        JyroError.raiseRuntime MessageCode.ValueNestingLimitExceeded [| box ValueRecursionGuard.MaxDepth |]

    // Adds the size of `value` to `total`, stopping once `total` passes
    // `limit`. `depth` is the number of containers above `value`.
    //
    // When `scan` is set, it also records in `surrogate` the index of the
    // first unpaired surrogate it meets in a string or a key, while
    // `surrogate` is still -1. It records rather than raises, so a value
    // that is also too deep or too large gets that error (spec 160.8.4). It
    // scans only while `total` is within `limit`, so the scan reads no more
    // code units than the size bound allows (spec 160.8.5).
    static member private Count
        (value: JyroValue, depth: int, limit: int64, total: int64 ref, scan: bool, surrogate: int ref) : unit =
        ExecutionCheck.Tick()
        // The scan is written out at each of its two sites rather than as a
        // local function: a local function capturing this state is a closure
        // the Debug build allocates on every call, and this walk must make
        // nothing before a conversion refuses the value for size (160.8.2).
        match value with
        | :? JyroString as s ->
            total.Value <- total.Value + 1L + int64 s.Length
            if scan && surrogate.Value < 0 && total.Value <= limit then
                surrogate.Value <- JyroValue.FirstUnpairedSurrogate s.Value
        | :? JyroArray as a ->
            let depth = depth + 1
            if depth > ValueRecursionGuard.MaxDepth then ValueWalk.TooDeep()
            total.Value <- total.Value + 1L
            let items = a.Items
            let mutable i = 0
            while total.Value <= limit && i < items.Count do
                ValueWalk.Count(items.[i], depth, limit, total, scan, surrogate)
                i <- i + 1
        | :? JyroObject as o ->
            let depth = depth + 1
            if depth > ValueRecursionGuard.MaxDepth then ValueWalk.TooDeep()
            total.Value <- total.Value + 1L
            // The Dictionary's own struct enumerator: through the view's
            // interface it is boxed, 56 bytes for every object node (d-572).
            let mutable e = o.PropertyStore.GetEnumerator()
            while total.Value <= limit && e.MoveNext() do
                let kvp = e.Current
                total.Value <- total.Value + int64 kvp.Key.Length
                if scan && surrogate.Value < 0 && total.Value <= limit then
                    surrogate.Value <- JyroValue.FirstUnpairedSurrogate kvp.Key
                ValueWalk.Count(kvp.Value, depth, limit, total, scan, surrogate)
        | _ -> total.Value <- total.Value + 1L

    // The size of `value` (spec 100.8.6), or some number above `limit` once
    // the count passes it: the walk stops there, so it costs no more than a
    // walk of a value of size `limit`. Raises `ValueNestingLimitExceeded`
    // when the walk goes deeper than the maximum nesting depth, which a cycle
    // does unless the count passes `limit` first.
    static member UnfoldedSize(value: JyroValue, limit: int64) : int64 =
        let total = ref 0L
        ValueWalk.Count(value, 0, limit, total, false, ref -1)
        total.Value

    // The height of container `c` - the most containers on a path down from
    // it, itself included - at `depth` containers from the root.
    static member private Height
        (c: JyroValue, depth: int, arraysOnly: bool,
         onPath: HashSet<JyroValue>, heights: Dictionary<JyroValue, int>) : int =
        if depth > ValueRecursionGuard.MaxDepth || not (onPath.Add c) then ValueWalk.TooDeep()
        let mutable best = 0
        // The children are read by index and by the Dictionary's struct
        // enumerator, not as a `seq`: an enumerator reached through an
        // interface is a heap object, and this runs on the path that must
        // make nothing before a conversion refuses the value (spec 160.8.2).
        match c with
        | :? JyroArray as a ->
            let items = a.Items
            let mutable i = 0
            while i < items.Count do
                let h = ValueWalk.ChildHeight(items.[i], depth, arraysOnly, onPath, heights)
                if h > best then best <- h
                i <- i + 1
        | :? JyroObject as o ->
            let mutable e = o.PropertyStore.GetEnumerator()
            while e.MoveNext() do
                let h = ValueWalk.ChildHeight(e.Current.Value, depth, arraysOnly, onPath, heights)
                if h > best then best <- h
        | _ -> ()
        onPath.Remove c |> ignore
        heights.[c] <- best + 1
        best + 1

    // The height of `child` of a container at `depth`, or 0 when the walk
    // does not descend into it; raises `ValueNestingLimitExceeded` when it
    // is too deep there.
    static member private ChildHeight
        (child: JyroValue, depth: int, arraysOnly: bool,
         onPath: HashSet<JyroValue>, heights: Dictionary<JyroValue, int>) : int =
        ExecutionCheck.Tick()
        let walked =
            match child with
            | :? JyroArray -> true
            | :? JyroObject -> not arraysOnly
            | _ -> false
        if not walked then 0
        else
            // The out-parameter form: `match heights.TryGetValue child with
            // | true, h` makes a Tuple<bool, int> per child in Debug (d-573).
            let mutable h = 0
            if not (heights.TryGetValue(child, &h)) then
                h <- ValueWalk.Height(child, depth + 1, arraysOnly, onPath, heights)
            if depth + h > ValueRecursionGuard.MaxDepth then ValueWalk.TooDeep()
            h

    // Raises `ValueNestingLimitExceeded` when `value` is cyclic or holds a
    // container deeper than the maximum nesting depth (spec 100.8.9).
    //
    // Visits each distinct container once, so it costs what the value holds,
    // not what its tree unfolds into. A walk that reaches the size bound calls
    // this before it reports the size, so which of the two errors a script
    // sees does not depend on the order of the walk. `arraysOnly` is for a
    // walk that descends into arrays only, as `Flatten` does.
    static member RequireShallowAcyclic(value: JyroValue, arraysOnly: bool) : unit =
        let walked =
            match value with
            | :? JyroArray -> true
            | :? JyroObject -> not arraysOnly
            | _ -> false
        if walked then
            let onPath = HashSet<JyroValue>(HashIdentity.Reference)
            let heights = Dictionary<JyroValue, int>(HashIdentity.Reference)
            ValueWalk.Height(value, 1, arraysOnly, onPath, heights) |> ignore

    // The check a host conversion makes before it makes anything (spec
    // 160.8.1, 160.8.2): `ValueNestingLimitExceeded` for a cyclic or too-deep
    // value, then `ValueSizeLimitExceeded` when the size (spec 100.8.6) is
    // above the maximum value size.
    static member RequireHostSize(value: JyroValue) : unit =
        let maximum = int64 ValueSize.Maximum
        match value with
        | :? JyroString as s -> if 1L + int64 s.Length > maximum then ValueSize.Raise()
        | :? JyroArray
        | :? JyroObject ->
            if ValueWalk.UnfoldedSize(value, maximum) > maximum then
                ValueWalk.RequireShallowAcyclic(value, false)
                ValueSize.Raise()
        | _ -> ()

    // The text of an operand of `&`: a string's own code units, and the
    // string form of anything else `&` accepts.
    static member OperandText(value: JyroValue) : string =
        match value with
        | :? JyroString as s -> s.Value
        | other -> other.ToStringValue()

    // Appends the string form of `value` to `text`, raising
    // `ValueSizeLimitExceeded` before `text` would pass `limit` code units.
    // `beforeSizeError` runs first, so a walk that reaches the bound still
    // reports a cyclic or too-deep value as such (spec 100.8.9).
    static member AppendText
        (text: System.Text.StringBuilder, value: JyroValue, limit: int, beforeSizeError: unit -> unit) : unit =
        ExecutionCheck.Tick()
        let append (piece: string) =
            if int64 text.Length + int64 piece.Length > int64 limit then
                beforeSizeError ()
                ValueSize.Raise()
            ValueWalk.AppendChunked(text, piece)
        match value with
        | :? JyroString as s -> append s.Value
        | :? JyroArray as a ->
            ValueRecursionGuard.Enter()
            try
                append "["
                let items = a.Items
                for i = 0 to items.Count - 1 do
                    if i > 0 then append ", "
                    ValueWalk.AppendText(text, items.[i], limit, beforeSizeError)
                append "]"
            finally
                ValueRecursionGuard.Exit()
        | :? JyroObject as o ->
            ValueRecursionGuard.Enter()
            try
                append "{"
                let mutable first = true
                for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do
                    if not first then append ", "
                    first <- false
                    append kvp.Key
                    append ": "
                    ValueWalk.AppendText(text, kvp.Value, limit, beforeSizeError)
                append "}"
            finally
                ValueRecursionGuard.Exit()
        | other -> append (other.ToStringValue())

    // The string form of `value` for a script: a string is its own text, and
    // a string made from an array or an object is refused above the maximum
    // value size (spec 100.8.7), a cyclic or too-deep value first (100.8.9).
    //
    // The compiler calls this where a statement needs the text of a value -
    // the message of `exit` and `fail` - and `ToString` returns it. It raises
    // for a value with no text; a diagnostic's argument uses `MessageText`.
    //
    // A statement is not a function, so an `UnpairedSurrogate` raised here
    // names none; a library function calls `ScriptStringFor` with its own
    // name (JYRO-ADR-128).
    static member ScriptString(value: JyroValue) : string =
        ValueWalk.ScriptStringFor(value, null)

    // `ScriptString` for a library function: `functionName` is the name of
    // the function whose call this is, with `()`, which an
    // `UnpairedSurrogate` gives after the index; null for none.
    static member ScriptStringFor(value: JyroValue, functionName: string | null) : string =
        match value with
        | :? JyroString as s -> s.Value
        // The string form of an array or an object is its JSON text (95.3.6),
        // with ToJson's bounds: this wrote the host's debug form, `[a, b]`,
        // with a string element's quotes dropped.
        | :? JyroArray
        | :? JyroObject -> ValueWalk.ScriptJson(value, functionName)
        // A function has no text: the error `ToJson` raises for it (spec
        // 95.3, 95.14.7), which `exit` and `fail` then raise (JYRO-ADR-37).
        | :? JyroFunction -> raise (ValueWalk.FunctionRefused())
        | other -> other.ToStringValue()

    // The error a script's `ToJson` raises for a function value, alone or
    // inside an array or an object (spec 95.14.7), and so `ToString` too
    // (95.3): `InvalidType`, a value of type `function` given where a string
    // is made.
    static member private FunctionRefused() : exn =
        JyroRuntimeException(
            MessageCode.InvalidType,
            [| box (TypeNames.typeOfName JyroValueType.Function)
               box (TypeNames.typeOfName JyroValueType.String) |]) :> exn

    // The text of a script value as an argument of a diagnostic's message,
    // made without raising (JYRO-ADR-161): a string is its own text, a
    // number, boolean or null its `ToString` text, and an array or object
    // its JSON text when it can be rendered within the limits. An array or
    // object that cannot - cyclic, too deep, too large, or holding a
    // function or an unpaired surrogate - and a function are named by their
    // type as `TypeOf` spells it, so a message argument never changes the
    // diagnostic's code. Only the time limit and the host's cancellation,
    // which end the run whatever is being done, pass through.
    static member MessageText(value: JyroValue) : string =
        match value with
        | _ when isNull (box value) -> TypeNames.typeOfName JyroValueType.Null
        | :? JyroString as s -> s.Value
        | :? JyroArray
        | :? JyroObject ->
            try ValueWalk.ScriptJson(value, null)
            with
            | :? JyroRuntimeException as ex
                when ex.Code <> MessageCode.ExecutionTimeLimitExceeded
                     && ex.Code <> MessageCode.CanceledByHost ->
                TypeNames.typeOfName value.ValueType
        | :? JyroFunction -> TypeNames.typeOfName JyroValueType.Function
        | other -> other.ToStringValue()

    // `ToStringValue` of an array or an object, for the host (spec 160.8.1).
    static member HostText(value: JyroValue) : string =
        ValueWalk.RequireHostSize value
        let text = System.Text.StringBuilder()
        ValueWalk.AppendText(text, value, Int32.MaxValue, ignore)
        // Copied out a chunk at a time with a check between chunks, not by
        // one `ToString` over the whole text (spec 100.2.5, JYRO-ADR-101).
        BoundedText.ofBuilder text ExecutionCheck.Check

    // `ToObjectValue` of an array or an object, for the host (spec 160.8.1).
    static member HostObject(value: JyroValue) : obj =
        ValueWalk.RequireHostSize value
        // Only an array or an object reaches here, and each gives a container.
        nonNull (ValueWalk.ToHost value)

    // Gives a null reference for the Jyro null, so the containers it builds
    // hold `obj | null` (JYRO-REQ-1931).
    static member private ToHost(value: JyroValue) : obj | null =
        ExecutionCheck.Tick()
        match value with
        | :? JyroArray as a ->
            ValueRecursionGuard.Enter()
            try
                let result = ResizeArray<obj | null>(a.Count)
                for item in a.Items do
                    result.Add(ValueWalk.ToHost item)
                box result
            finally
                ValueRecursionGuard.Exit()
        | :? JyroObject as o ->
            ValueRecursionGuard.Enter()
            try
                let dict = Dictionary<string, obj | null>()
                for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do
                    dict.[kvp.Key] <- ValueWalk.ToHost kvp.Value
                box dict
            finally
                ValueRecursionGuard.Exit()
        | :? JyroString as s -> box s.Value
        | other -> other.ToObjectValue()

    // Writes `value` as JSON, raising `ValueSizeLimitExceeded` once the
    // writer holds more than `limit` bytes.
    static member private WriteTree(writer: Utf8JsonWriter, value: JyroValue, limit: int64) : unit =
        ExecutionCheck.Tick()
        match value with
        | :? JyroArray as a ->
            ValueRecursionGuard.Enter()
            try
                writer.WriteStartArray()
                for item in a.Items do
                    ValueWalk.WriteTree(writer, item, limit)
                writer.WriteEndArray()
            finally
                ValueRecursionGuard.Exit()
        | :? JyroObject as o ->
            ValueRecursionGuard.Enter()
            try
                writer.WriteStartObject()
                for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do
                    writer.WritePropertyName(kvp.Key : string)
                    ValueWalk.WriteTree(writer, kvp.Value, limit)
                writer.WriteEndObject()
            finally
                ValueRecursionGuard.Exit()
        // Every key and string was checked for an unpaired surrogate by
        // `RequireHostJson` before the first write (spec 160.8.5), so none
        // reaches the writer, which would write it as U+FFFD (spec 160.8.4).
        | :? JyroString as s -> writer.WriteStringValue(s.Value)
        | other -> other.WriteJson(writer)
        if int64 writer.BytesPending + writer.BytesCommitted > limit then ValueSize.Raise()

    // Appends `piece` to `text` a chunk of code units at a time, with a
    // check between chunks (spec 100.2.5), rather than in one call.
    static member AppendChunked(text: System.Text.StringBuilder, piece: string) : unit =
        let mutable i = 0
        while i < piece.Length do
            if i > 0 then ExecutionCheck.Check()
            let n = min ExecutionCheck.ChunkLength (piece.Length - i)
            text.Append(piece, i, n) |> ignore
            i <- i + n

    // Appends `piece` to the JSON text, refusing first when the text would
    // pass `limit` code units (spec 100.8.8).
    static member private AppendJson(text: System.Text.StringBuilder, piece: ReadOnlySpan<char>, limit: int64) : unit =
        if int64 text.Length + int64 piece.Length > limit then ValueSize.Raise()
        text.Append(piece) |> ignore

    // Escapes `source` into `scratch` as ECMAScript's QuoteJSONString does
    // (spec 95.14.3, 10.6.2): `"` and `\` with a backslash, U+0008, U+0009,
    // U+000A, U+000C and U+000D as `\b`, `\t`, `\n`, `\f` and `\r`, every
    // other code unit below U+0020 as `\u00` and two lower-case hexadecimal
    // digits, and every other code unit as itself. Returns the number of
    // code units written; `scratch` holds six for each of `source`.
    static member private QuoteJsonString(source: ReadOnlySpan<char>, scratch: char[]) : int =
        let w = ref 0
        let put (c: char) =
            scratch.[w.Value] <- c
            w.Value <- w.Value + 1
        let hex = "0123456789abcdef"
        for c in source do
            match c with
            | '"' -> put '\\'; put '"'
            | '\\' -> put '\\'; put '\\'
            | '\b' -> put '\\'; put 'b'
            | '\t' -> put '\\'; put 't'
            | '\n' -> put '\\'; put 'n'
            | '\012' -> put '\\'; put 'f'
            | '\r' -> put '\\'; put 'r'
            | c when c < ' ' ->
                put '\\'; put 'u'; put '0'; put '0'
                put hex.[int c >>> 4]; put hex.[int c &&& 0xF]
            | c -> put c
        w.Value

    // Appends a string or a key as a JSON string, a chunk of code units at a
    // time: each escaped chunk is measured against `limit` before it is
    // appended, and the limits are checked between chunks. With `ecma` it is
    // escaped as ECMAScript's QuoteJSONString escapes it (spec 95.14.3), as a
    // script's `ToJson` writes it; without, as `Utf8JsonWriter` escapes it
    // with its default encoder.
    static member private AppendJsonString
        (text: System.Text.StringBuilder, value: string, limit: int64, scratch: char[], functionName: string | null, ecma: bool) : unit =
        let bad = JyroValue.FirstUnpairedSurrogate value
        if bad >= 0 then
            JyroError.raiseRuntime MessageCode.UnpairedSurrogate (ValueWalk.SurrogateArgs(bad, functionName))
        ValueWalk.AppendJson(text, "\"".AsSpan(), limit)
        let encoder = System.Text.Encodings.Web.JavaScriptEncoder.Default
        let mutable i = 0
        while i < value.Length do
            if i > 0 then ExecutionCheck.Check()
            let finish = BoundedText.chunkEnd value i
            let written =
                if ecma then ValueWalk.QuoteJsonString(value.AsSpan(i, finish - i), scratch)
                else
                    let mutable consumed = 0
                    let mutable written = 0
                    let status = encoder.Encode(value.AsSpan(i, finish - i), Span<char>(scratch), &consumed, &written, true)
                    if status <> System.Buffers.OperationStatus.Done then
                        raise (InvalidOperationException(sprintf "JSON escaping stopped with %A" status))
                    written
            ValueWalk.AppendJson(text, ReadOnlySpan<char>(scratch, 0, written), limit)
            i <- finish
        ValueWalk.AppendJson(text, "\"".AsSpan(), limit)

    // The arguments of an `UnpairedSurrogate`: the index, then the function
    // whose call raised it when there is one (JYRO-ADR-128). `functionName`
    // comes from that function, never a literal of the shared writer, which
    // serves `ToJson`, `ToString`, `GroupBy`, statements and the host alike,
    // and of `RequireJsonValue`, which serves `ValidateSchema`.
    static member private SurrogateArgs(index: int, functionName: string | null) : (obj | null)[] =
        if isNull functionName then [| box index |] else [| box index; box functionName |]

    static member private WriteText
        (text: System.Text.StringBuilder, value: JyroValue, limit: int64,
         scratch: char[], numbers: System.Buffers.ArrayBufferWriter<byte>, numberWriter: Utf8JsonWriter,
         functionName: string | null, ecma: bool) : unit =
        ExecutionCheck.Tick()
        match value with
        | :? JyroArray as a ->
            ValueRecursionGuard.Enter()
            try
                ValueWalk.AppendJson(text, "[".AsSpan(), limit)
                let items = a.Items
                for i = 0 to items.Count - 1 do
                    if i > 0 then ValueWalk.AppendJson(text, ",".AsSpan(), limit)
                    ValueWalk.WriteText(text, items.[i], limit, scratch, numbers, numberWriter, functionName, ecma)
                ValueWalk.AppendJson(text, "]".AsSpan(), limit)
            finally
                ValueRecursionGuard.Exit()
        | :? JyroObject as o ->
            ValueRecursionGuard.Enter()
            try
                ValueWalk.AppendJson(text, "{".AsSpan(), limit)
                let mutable first = true
                for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do
                    if not first then ValueWalk.AppendJson(text, ",".AsSpan(), limit)
                    first <- false
                    ValueWalk.AppendJsonString(text, kvp.Key, limit, scratch, functionName, ecma)
                    ValueWalk.AppendJson(text, ":".AsSpan(), limit)
                    ValueWalk.WriteText(text, kvp.Value, limit, scratch, numbers, numberWriter, functionName, ecma)
                ValueWalk.AppendJson(text, "}".AsSpan(), limit)
            finally
                ValueRecursionGuard.Exit()
        | :? JyroString as s -> ValueWalk.AppendJsonString(text, s.Value, limit, scratch, functionName, ecma)
        | :? JyroNull -> ValueWalk.AppendJson(text, "null".AsSpan(), limit)
        | :? JyroBoolean as b -> ValueWalk.AppendJson(text, (if b.Value then "true" else "false").AsSpan(), limit)
        // A script's `ToJson` (the `ecma` text, which only `ScriptJson`
        // writes) refuses a function at any depth (spec 95.14.7); the host's
        // and the engine's JSON keep writing it as a string (160.8.1).
        | :? JyroFunction when ecma -> raise (ValueWalk.FunctionRefused())
        | :? JyroFunction -> ValueWalk.AppendJsonString(text, "<function>", limit, scratch, functionName, ecma)
        // A script's `ToJson` writes a number in its string form (spec
        // 95.14, 40.4.6, 40.4.7): `1.50` as `1.5`, an inexact 1e21 as
        // `1e+21`. Every string form is a JSON number.
        | :? JyroNumber as n when ecma -> ValueWalk.AppendJson(text, n.ToStringValue().AsSpan(), limit)
        | other ->
            // A number, or a value class a host derived: written by its own
            // `WriteJson`, which is a few bytes, into a scratch writer.
            numbers.Clear()
            numberWriter.Reset(numbers)
            other.WriteJson(numberWriter)
            numberWriter.Flush()
            let piece = System.Text.Encoding.UTF8.GetString(numbers.WrittenSpan)
            ValueWalk.AppendJson(text, piece.AsSpan(), limit)

    // The JSON text of `value`, built a piece at a time: every piece is
    // measured against `limit` code units before it is appended (spec
    // 100.8.8), and a string is escaped and appended a chunk at a time with a
    // check between chunks (spec 100.2.5). A string or a key is escaped as
    // QuoteJSONString escapes it with `ecma`, else as `Utf8JsonWriter` with
    // its default encoder. `functionName` is the function whose call this
    // is, with `()`, for an `UnpairedSurrogate`; null for none.
    static member private Json(value: JyroValue, limit: int64, functionName: string | null, ecma: bool) : string =
        let text = System.Text.StringBuilder()
        let scratch = Array.zeroCreate<char> (6 * ExecutionCheck.ChunkLength)
        let numbers = System.Buffers.ArrayBufferWriter<byte>(64)
        use numberWriter = new Utf8JsonWriter(numbers)
        ValueWalk.WriteText(text, value, limit, scratch, numbers, numberWriter, functionName, ecma)
        // Copied out a chunk at a time with a check between chunks, not by
        // one `ToString` over the whole text (spec 100.2.5, JYRO-ADR-101).
        BoundedText.ofBuilder text ExecutionCheck.Check

    // The check a host's JSON member makes before it writes anything (spec
    // 160.8.2, 160.8.4, 160.8.5): the checks of `RequireHostSize` first, then
    // `UnpairedSurrogate` for a string or a key, at any depth, that holds an
    // unpaired surrogate. One walk finds all three, the surrogate recorded
    // and raised only once the walk has found the value neither too deep nor
    // too large (ADR 0084). A host member has no function to name, so the
    // `UnpairedSurrogate` gives the index alone (JYRO-ADR-128).
    static member RequireHostJson(value: JyroValue) : unit =
        match value with
        | :? JyroArray
        | :? JyroObject ->
            let maximum = int64 ValueSize.Maximum
            let total = ref 0L
            let surrogate = ref -1
            ValueWalk.Count(value, 0, maximum, total, true, surrogate)
            if total.Value > maximum then
                ValueWalk.RequireShallowAcyclic(value, false)
                ValueSize.Raise()
            if surrogate.Value >= 0 then
                JyroError.raiseRuntime MessageCode.UnpairedSurrogate [| box surrogate.Value |]
        | _ -> ValueWalk.RequireHostSize value

    // `WriteJson` of an array or an object, for the host: nothing is written
    // when the value is refused (spec 160.8.2, 160.8.5).
    static member HostWriteJson(writer: Utf8JsonWriter, value: JyroValue) : unit =
        ValueWalk.RequireHostJson value
        ValueWalk.WriteTree(writer, value, Int64.MaxValue)

    // `ToJson` for the host (spec 160.8.1, 160.8.2, 160.8.4).
    static member HostJson(value: JyroValue) : string =
        ValueWalk.RequireHostJson value
        ValueWalk.Json(value, Int64.MaxValue, null, false)

    // `ToJson` for a script: refused above the maximum value size (spec
    // 100.8.7), a cyclic or too-deep value first (spec 100.8.9).
    //
    // The JSON text of a value is never shorter than its size (spec 100.8.6):
    // every node writes at least one code unit, a string its own code units
    // and two quotes, and a key its code units, two quotes and a colon. So a
    // value above the maximum is refused before anything is written. Below
    // it, the text can still grow past the maximum - a number writes several
    // code units, an escape six - so each piece of the text is measured
    // before it is appended, and the piece that would pass `M` is refused.
    //
    // `functionName` is the library function whose call this is, with `()`
    // (`ToJson()`, `ToString()`, `GroupBy()`), which an `UnpairedSurrogate`
    // gives after the index; null for a statement, which names none.
    static member ScriptJson(value: JyroValue, functionName: string | null) : string =
        let maximum = int64 ValueSize.Maximum
        if ValueWalk.UnfoldedSize(value, maximum) > maximum then
            ValueWalk.RequireShallowAcyclic(value, false)
            ValueSize.Raise()
        ValueWalk.Json(value, maximum, functionName, true)

    // The checks a script's `ToJson` makes of `value` as it writes it, in the
    // same order, with no text written and no size bound: a cyclic or
    // too-deep value raises `ValueNestingLimitExceeded` (spec 100.7), a
    // string or a key holding an unpaired surrogate `UnpairedSurrogate`
    // (95.14.4, 95.14.5), and, with `refuseFunction`, a function at any
    // depth `InvalidType` (95.14.7). `ValidateSchema` reads its data and its
    // schema as JSON values this way (96.2.13 to 96.2.15, 96.2.35), and then
    // walks them itself. It checks the time and the cancellation signal as
    // it goes. `functionName` is the function whose call this is, with `()`,
    // for an `UnpairedSurrogate` (JYRO-ADR-128).
    static member RequireJsonValue(value: JyroValue, functionName: string | null, refuseFunction: bool) : unit =
        ExecutionCheck.Tick()
        let requirePaired (text: string) =
            let bad = JyroValue.FirstUnpairedSurrogate text
            if bad >= 0 then
                JyroError.raiseRuntime MessageCode.UnpairedSurrogate (ValueWalk.SurrogateArgs(bad, functionName))
        match value with
        | :? JyroArray as a ->
            ValueRecursionGuard.Enter()
            try
                let items = a.Items
                for i = 0 to items.Count - 1 do
                    ValueWalk.RequireJsonValue(items.[i], functionName, refuseFunction)
            finally
                ValueRecursionGuard.Exit()
        | :? JyroObject as o ->
            ValueRecursionGuard.Enter()
            try
                for kvp in (o.Properties : IReadOnlyDictionary<string, JyroValue>) do
                    requirePaired kvp.Key
                    ValueWalk.RequireJsonValue(kvp.Value, functionName, refuseFunction)
            finally
                ValueRecursionGuard.Exit()
        | :? JyroString as s -> requirePaired s.Value
        | :? JyroFunction when refuseFunction -> raise (ValueWalk.FunctionRefused())
        | _ -> ()

    // The script's assignment `o.k = v`: a new key is refused at the maximum
    // value size (spec 100.8.7, 100.8.8).
    static member ScriptSetProperty(target: JyroValue, key: string, value: JyroValue) : unit =
        match target with
        | :? JyroObject as o -> o.SetPropertyBounded(key, value)
        | _ -> target.SetProperty(key, value)

    // The script's assignment `a[i] = v` and `o[k] = v`: an append or a new
    // key is refused at the maximum value size (spec 100.8.7, 100.8.8).
    static member ScriptSetIndex(target: JyroValue, index: JyroValue, value: JyroValue) : unit =
        match target, index with
        | (:? JyroArray as a), _ -> a.SetIndexBounded(index, value)
        | (:? JyroObject as o), (:? JyroString as key) when not o.IsFrozen -> o.SetPropertyBounded(key.Value, value)
        | _ -> target.SetIndex(index, value)

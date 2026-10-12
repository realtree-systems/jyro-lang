using System;
using System.Buffers.Binary;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;

namespace RealTree.Jyro.Spec.Tests.P30.HostNumbers;

/// <summary>
/// IFloatingPointIeee754&lt;TSelf&gt; for a class holding a double, every member that
/// names TSelf given here once. Its arithmetic is a double's, so every member the
/// interface requires gives a double's answer. The members that do not name TSelf
/// (the exponent and significand writers, the text) are the class's own, so a
/// class can write them by a convention other than .NET's (JYRO-ADR-158 (a3)).
/// </summary>
public interface IClassFloatCore<TSelf> : IFloatingPointIeee754<TSelf> where TSelf : IClassFloatCore<TSelf>
{
    double Value { get; }

    static abstract TSelf Make(double value);

    // The conversions the interface leaves to the implementer: refused.
    static bool INumberBase<TSelf>.TryConvertFromChecked<TOther>(TOther value, out TSelf result) { result = default!; return false; }
    static bool INumberBase<TSelf>.TryConvertFromSaturating<TOther>(TOther value, out TSelf result) { result = default!; return false; }
    static bool INumberBase<TSelf>.TryConvertFromTruncating<TOther>(TOther value, out TSelf result) { result = default!; return false; }
    static bool INumberBase<TSelf>.TryConvertToChecked<TOther>(TSelf value, [MaybeNullWhen(false)] out TOther result) { result = default; return false; }
    static bool INumberBase<TSelf>.TryConvertToSaturating<TOther>(TSelf value, [MaybeNullWhen(false)] out TOther result) { result = default; return false; }
    static bool INumberBase<TSelf>.TryConvertToTruncating<TOther>(TSelf value, [MaybeNullWhen(false)] out TOther result) { result = default; return false; }

    static TSelf INumberBase<TSelf>.One => TSelf.Make(1.0);
    static int INumberBase<TSelf>.Radix => 2;
    static TSelf INumberBase<TSelf>.Zero => TSelf.Make(0.0);
    static TSelf IAdditiveIdentity<TSelf, TSelf>.AdditiveIdentity => TSelf.Make(0.0);
    static TSelf IMultiplicativeIdentity<TSelf, TSelf>.MultiplicativeIdentity => TSelf.Make(1.0);
    static TSelf ISignedNumber<TSelf>.NegativeOne => TSelf.Make(-1.0);

    static TSelf INumberBase<TSelf>.Abs(TSelf value) => TSelf.Make(double.Abs(value.Value));
    static bool INumberBase<TSelf>.IsCanonical(TSelf value) => true;
    static bool INumberBase<TSelf>.IsComplexNumber(TSelf value) => false;
    static bool INumberBase<TSelf>.IsEvenInteger(TSelf value) => double.IsEvenInteger(value.Value);
    static bool INumberBase<TSelf>.IsFinite(TSelf value) => double.IsFinite(value.Value);
    static bool INumberBase<TSelf>.IsImaginaryNumber(TSelf value) => false;
    static bool INumberBase<TSelf>.IsInfinity(TSelf value) => double.IsInfinity(value.Value);
    static bool INumberBase<TSelf>.IsInteger(TSelf value) => double.IsInteger(value.Value);
    static bool INumberBase<TSelf>.IsNaN(TSelf value) => double.IsNaN(value.Value);
    static bool INumberBase<TSelf>.IsNegative(TSelf value) => double.IsNegative(value.Value);
    static bool INumberBase<TSelf>.IsNegativeInfinity(TSelf value) => double.IsNegativeInfinity(value.Value);
    static bool INumberBase<TSelf>.IsNormal(TSelf value) => double.IsNormal(value.Value);
    static bool INumberBase<TSelf>.IsOddInteger(TSelf value) => double.IsOddInteger(value.Value);
    static bool INumberBase<TSelf>.IsPositive(TSelf value) => double.IsPositive(value.Value);
    static bool INumberBase<TSelf>.IsPositiveInfinity(TSelf value) => double.IsPositiveInfinity(value.Value);
    static bool INumberBase<TSelf>.IsRealNumber(TSelf value) => double.IsRealNumber(value.Value);
    static bool INumberBase<TSelf>.IsSubnormal(TSelf value) => double.IsSubnormal(value.Value);
    static bool INumberBase<TSelf>.IsZero(TSelf value) => value.Value == 0.0;
    static TSelf INumberBase<TSelf>.MaxMagnitude(TSelf x, TSelf y) => TSelf.Make(double.MaxMagnitude(x.Value, y.Value));
    static TSelf INumberBase<TSelf>.MaxMagnitudeNumber(TSelf x, TSelf y) => TSelf.Make(double.MaxMagnitudeNumber(x.Value, y.Value));
    static TSelf INumberBase<TSelf>.MinMagnitude(TSelf x, TSelf y) => TSelf.Make(double.MinMagnitude(x.Value, y.Value));
    static TSelf INumberBase<TSelf>.MinMagnitudeNumber(TSelf x, TSelf y) => TSelf.Make(double.MinMagnitudeNumber(x.Value, y.Value));

    static TSelf INumberBase<TSelf>.Parse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider) => TSelf.Make(double.Parse(s, style, provider));
    static TSelf INumberBase<TSelf>.Parse(string s, NumberStyles style, IFormatProvider? provider) => TSelf.Make(double.Parse(s, style, provider));
    static TSelf ISpanParsable<TSelf>.Parse(ReadOnlySpan<char> s, IFormatProvider? provider) => TSelf.Make(double.Parse(s, provider));
    static TSelf IParsable<TSelf>.Parse(string s, IFormatProvider? provider) => TSelf.Make(double.Parse(s, provider));
    static bool INumberBase<TSelf>.TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider, out TSelf result)
    { var ok = double.TryParse(s, style, provider, out var v); result = TSelf.Make(v); return ok; }
    static bool INumberBase<TSelf>.TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, out TSelf result)
    { var ok = double.TryParse(s, style, provider, out var v); result = TSelf.Make(v); return ok; }
    static bool ISpanParsable<TSelf>.TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, out TSelf result)
    { var ok = double.TryParse(s, provider, out var v); result = TSelf.Make(v); return ok; }
    static bool IParsable<TSelf>.TryParse([NotNullWhen(true)] string? s, IFormatProvider? provider, out TSelf result)
    { var ok = double.TryParse(s, provider, out var v); result = TSelf.Make(v); return ok; }

    int IComparable<TSelf>.CompareTo(TSelf? other) => other is null ? 1 : Value.CompareTo(other.Value);
    bool IEquatable<TSelf>.Equals(TSelf? other) => other is not null && Value.Equals(other.Value);

    static TSelf IFloatingPointConstants<TSelf>.E => TSelf.Make(double.E);
    static TSelf IFloatingPointConstants<TSelf>.Pi => TSelf.Make(double.Pi);
    static TSelf IFloatingPointConstants<TSelf>.Tau => TSelf.Make(double.Tau);
    static TSelf IFloatingPointIeee754<TSelf>.Epsilon => TSelf.Make(double.Epsilon);
    static TSelf IFloatingPointIeee754<TSelf>.NaN => TSelf.Make(double.NaN);
    static TSelf IFloatingPointIeee754<TSelf>.NegativeInfinity => TSelf.Make(double.NegativeInfinity);
    static TSelf IFloatingPointIeee754<TSelf>.NegativeZero => TSelf.Make(double.NegativeZero);
    static TSelf IFloatingPointIeee754<TSelf>.PositiveInfinity => TSelf.Make(double.PositiveInfinity);

    static TSelf IFloatingPointIeee754<TSelf>.Atan2(TSelf y, TSelf x) => TSelf.Make(double.Atan2(y.Value, x.Value));
    static TSelf IFloatingPointIeee754<TSelf>.Atan2Pi(TSelf y, TSelf x) => TSelf.Make(double.Atan2Pi(y.Value, x.Value));
    static TSelf IFloatingPointIeee754<TSelf>.BitDecrement(TSelf x) => TSelf.Make(double.BitDecrement(x.Value));
    static TSelf IFloatingPointIeee754<TSelf>.BitIncrement(TSelf x) => TSelf.Make(double.BitIncrement(x.Value));
    static TSelf IFloatingPointIeee754<TSelf>.FusedMultiplyAdd(TSelf left, TSelf right, TSelf addend) => TSelf.Make(double.FusedMultiplyAdd(left.Value, right.Value, addend.Value));
    static TSelf IFloatingPointIeee754<TSelf>.Ieee754Remainder(TSelf left, TSelf right) => TSelf.Make(double.Ieee754Remainder(left.Value, right.Value));
    static int IFloatingPointIeee754<TSelf>.ILogB(TSelf x) => double.ILogB(x.Value);
    static TSelf IFloatingPointIeee754<TSelf>.ScaleB(TSelf x, int n) => TSelf.Make(double.ScaleB(x.Value, n));
    static TSelf IFloatingPoint<TSelf>.Round(TSelf x, int digits, MidpointRounding mode) => TSelf.Make(double.Round(x.Value, digits, mode));

    static TSelf IExponentialFunctions<TSelf>.Exp(TSelf x) => TSelf.Make(double.Exp(x.Value));
    static TSelf IExponentialFunctions<TSelf>.Exp10(TSelf x) => TSelf.Make(double.Exp10(x.Value));
    static TSelf IExponentialFunctions<TSelf>.Exp2(TSelf x) => TSelf.Make(double.Exp2(x.Value));
    static TSelf IHyperbolicFunctions<TSelf>.Acosh(TSelf x) => TSelf.Make(double.Acosh(x.Value));
    static TSelf IHyperbolicFunctions<TSelf>.Asinh(TSelf x) => TSelf.Make(double.Asinh(x.Value));
    static TSelf IHyperbolicFunctions<TSelf>.Atanh(TSelf x) => TSelf.Make(double.Atanh(x.Value));
    static TSelf IHyperbolicFunctions<TSelf>.Cosh(TSelf x) => TSelf.Make(double.Cosh(x.Value));
    static TSelf IHyperbolicFunctions<TSelf>.Sinh(TSelf x) => TSelf.Make(double.Sinh(x.Value));
    static TSelf IHyperbolicFunctions<TSelf>.Tanh(TSelf x) => TSelf.Make(double.Tanh(x.Value));
    static TSelf ILogarithmicFunctions<TSelf>.Log(TSelf x) => TSelf.Make(double.Log(x.Value));
    static TSelf ILogarithmicFunctions<TSelf>.Log(TSelf x, TSelf newBase) => TSelf.Make(double.Log(x.Value, newBase.Value));
    static TSelf ILogarithmicFunctions<TSelf>.Log10(TSelf x) => TSelf.Make(double.Log10(x.Value));
    static TSelf ILogarithmicFunctions<TSelf>.Log2(TSelf x) => TSelf.Make(double.Log2(x.Value));
    static TSelf IPowerFunctions<TSelf>.Pow(TSelf x, TSelf y) => TSelf.Make(double.Pow(x.Value, y.Value));
    static TSelf IRootFunctions<TSelf>.Cbrt(TSelf x) => TSelf.Make(double.Cbrt(x.Value));
    static TSelf IRootFunctions<TSelf>.Hypot(TSelf x, TSelf y) => TSelf.Make(double.Hypot(x.Value, y.Value));
    static TSelf IRootFunctions<TSelf>.RootN(TSelf x, int n) => TSelf.Make(double.RootN(x.Value, n));
    static TSelf IRootFunctions<TSelf>.Sqrt(TSelf x) => TSelf.Make(double.Sqrt(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.Acos(TSelf x) => TSelf.Make(double.Acos(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.AcosPi(TSelf x) => TSelf.Make(double.AcosPi(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.Asin(TSelf x) => TSelf.Make(double.Asin(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.AsinPi(TSelf x) => TSelf.Make(double.AsinPi(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.Atan(TSelf x) => TSelf.Make(double.Atan(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.AtanPi(TSelf x) => TSelf.Make(double.AtanPi(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.Cos(TSelf x) => TSelf.Make(double.Cos(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.CosPi(TSelf x) => TSelf.Make(double.CosPi(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.Sin(TSelf x) => TSelf.Make(double.Sin(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.SinPi(TSelf x) => TSelf.Make(double.SinPi(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.Tan(TSelf x) => TSelf.Make(double.Tan(x.Value));
    static TSelf ITrigonometricFunctions<TSelf>.TanPi(TSelf x) => TSelf.Make(double.TanPi(x.Value));
    static (TSelf Sin, TSelf Cos) ITrigonometricFunctions<TSelf>.SinCos(TSelf x)
    { var (s, c) = double.SinCos(x.Value); return (TSelf.Make(s), TSelf.Make(c)); }
    static (TSelf SinPi, TSelf CosPi) ITrigonometricFunctions<TSelf>.SinCosPi(TSelf x)
    { var (s, c) = double.SinCosPi(x.Value); return (TSelf.Make(s), TSelf.Make(c)); }

    static TSelf IUnaryPlusOperators<TSelf, TSelf>.operator +(TSelf value) => value;
    static TSelf IAdditionOperators<TSelf, TSelf, TSelf>.operator +(TSelf left, TSelf right) => TSelf.Make(left.Value + right.Value);
    static TSelf IUnaryNegationOperators<TSelf, TSelf>.operator -(TSelf value) => TSelf.Make(-value.Value);
    static TSelf ISubtractionOperators<TSelf, TSelf, TSelf>.operator -(TSelf left, TSelf right) => TSelf.Make(left.Value - right.Value);
    static TSelf IIncrementOperators<TSelf>.operator ++(TSelf value) => TSelf.Make(value.Value + 1);
    static TSelf IDecrementOperators<TSelf>.operator --(TSelf value) => TSelf.Make(value.Value - 1);
    static TSelf IMultiplyOperators<TSelf, TSelf, TSelf>.operator *(TSelf left, TSelf right) => TSelf.Make(left.Value * right.Value);
    static TSelf IDivisionOperators<TSelf, TSelf, TSelf>.operator /(TSelf left, TSelf right) => TSelf.Make(left.Value / right.Value);
    static TSelf IModulusOperators<TSelf, TSelf, TSelf>.operator %(TSelf left, TSelf right) => TSelf.Make(left.Value % right.Value);
    static bool IEqualityOperators<TSelf, TSelf, bool>.operator ==(TSelf? left, TSelf? right) => left is null ? right is null : right is not null && left.Value == right.Value;
    static bool IEqualityOperators<TSelf, TSelf, bool>.operator !=(TSelf? left, TSelf? right) => !(left is null ? right is null : right is not null && left.Value == right.Value);
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator <(TSelf left, TSelf right) => left.Value < right.Value;
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator >(TSelf left, TSelf right) => left.Value > right.Value;
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator <=(TSelf left, TSelf right) => left.Value <= right.Value;
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator >=(TSelf left, TSelf right) => left.Value >= right.Value;
}

/// <summary>
/// The class side of a float held as a double: its text, which is not a
/// number's, and its exponent and significand writers, by .NET's own convention
/// unless a subclass writes them otherwise.
/// </summary>
public abstract class DoubleBackedFloat
{
    public double Value { get; }

    protected DoubleBackedFloat(double value) { Value = value; }

    public override string ToString() => GetType().Name;
    public string ToString(string? format, IFormatProvider? formatProvider) => ToString();

    public bool TryFormat(Span<char> destination, out int charsWritten, ReadOnlySpan<char> format, IFormatProvider? provider)
    {
        var s = ToString();
        if (s.Length > destination.Length) { charsWritten = 0; return false; }
        s.AsSpan().CopyTo(destination);
        charsWritten = s.Length;
        return true;
    }

    public bool TryFormat(Span<byte> utf8Destination, out int bytesWritten, ReadOnlySpan<char> format, IFormatProvider? provider)
    {
        var s = ToString();
        if (s.Length > utf8Destination.Length) { bytesWritten = 0; return false; }
        for (var i = 0; i < s.Length; i++) utf8Destination[i] = (byte)s[i];
        bytesWritten = s.Length;
        return true;
    }

    public int CompareTo(object? obj) => obj is DoubleBackedFloat o ? Value.CompareTo(o.Value) : 1;
    public override bool Equals(object? obj) => obj is DoubleBackedFloat o && o.GetType() == GetType() && Value.Equals(o.Value);
    public override int GetHashCode() => Value.GetHashCode();

    private IFloatingPoint<double> F => Value;

    public virtual int GetExponentByteCount() => F.GetExponentByteCount();
    public virtual int GetExponentShortestBitLength() => F.GetExponentShortestBitLength();
    public virtual int GetSignificandBitLength() => F.GetSignificandBitLength();
    public virtual int GetSignificandByteCount() => F.GetSignificandByteCount();
    public virtual bool TryWriteExponentBigEndian(Span<byte> destination, out int bytesWritten) => F.TryWriteExponentBigEndian(destination, out bytesWritten);
    public virtual bool TryWriteExponentLittleEndian(Span<byte> destination, out int bytesWritten) => F.TryWriteExponentLittleEndian(destination, out bytesWritten);
    public virtual bool TryWriteSignificandBigEndian(Span<byte> destination, out int bytesWritten) => F.TryWriteSignificandBigEndian(destination, out bytesWritten);
    public virtual bool TryWriteSignificandLittleEndian(Span<byte> destination, out int bytesWritten) => F.TryWriteSignificandLittleEndian(destination, out bytesWritten);
}

/// <summary>
/// A binary float class from outside the BCL that is not sealed (JYRO-DEF-625). It
/// implements IFloatingPointIeee754&lt;BaseFloat&gt; and writes by .NET's convention.
/// </summary>
public class BaseFloat : DoubleBackedFloat, IClassFloatCore<BaseFloat>
{
    public BaseFloat(double value) : base(value) { }

    public static BaseFloat Make(double value) => new(value);
}

/// <summary>
/// A class derived from <see cref="BaseFloat"/> that adds nothing: it implements
/// IFloatingPointIeee754 only for its base, so it is a number read as a BaseFloat
/// (JYRO-ADR-158 (a2), JYRO-DEF-625).
/// </summary>
public sealed class DerivedFloat : BaseFloat
{
    public DerivedFloat(double value) : base(value) { }
}

/// <summary>
/// A binary64 float from outside the BCL whose writers follow IEEE 754 where
/// .NET's own types do not (JYRO-ADR-158 (a3); JYRO-DEF-626, JYRO-DEF-629): a
/// subnormal (and zero) writes IEEE's least exponent emin = -1022, not -1023, and
/// GetSignificandBitLength reports the trailing significand field's width t = 52,
/// not 53. The significand written is the whole significand, the leading bit
/// included for a normal value, as the interface's writers give it.
/// </summary>
public sealed class IeeeConventionFloat : DoubleBackedFloat, IClassFloatCore<IeeeConventionFloat>
{
    public IeeeConventionFloat(double value) : base(value) { }

    public static IeeeConventionFloat Make(double value) => new(value);

    private const int Bias = 1023;
    private const int EMin = -1022;

    private long BiasedExponent => (BitConverter.DoubleToInt64Bits(Value) >> 52) & 0x7FF;
    private ulong Fraction => (ulong)BitConverter.DoubleToInt64Bits(Value) & 0xF_FFFF_FFFF_FFFFUL;

    private short Exponent => BiasedExponent == 0 ? (short)EMin : (short)(BiasedExponent - Bias);
    private ulong Significand => BiasedExponent == 0 ? Fraction : Fraction | (1UL << 52);

    public override int GetExponentByteCount() => sizeof(short);

    public override int GetExponentShortestBitLength() => ShortestBits(Exponent);

    /// <summary>The bits of e in two's complement, its sign bit included.</summary>
    internal static int ShortestBits(int e) => 33 - BitOperations.LeadingZeroCount((uint)(e >= 0 ? e : ~e));

    public override int GetSignificandBitLength() => 52;
    public override int GetSignificandByteCount() => sizeof(ulong);

    public override bool TryWriteExponentBigEndian(Span<byte> destination, out int bytesWritten)
    {
        if (destination.Length < 2) { bytesWritten = 0; return false; }
        BinaryPrimitives.WriteInt16BigEndian(destination, Exponent);
        bytesWritten = 2;
        return true;
    }

    public override bool TryWriteExponentLittleEndian(Span<byte> destination, out int bytesWritten)
    {
        if (destination.Length < 2) { bytesWritten = 0; return false; }
        BinaryPrimitives.WriteInt16LittleEndian(destination, Exponent);
        bytesWritten = 2;
        return true;
    }

    public override bool TryWriteSignificandBigEndian(Span<byte> destination, out int bytesWritten)
    {
        if (destination.Length < 8) { bytesWritten = 0; return false; }
        BinaryPrimitives.WriteUInt64BigEndian(destination, Significand);
        bytesWritten = 8;
        return true;
    }

    public override bool TryWriteSignificandLittleEndian(Span<byte> destination, out int bytesWritten)
    {
        if (destination.Length < 8) { bytesWritten = 0; return false; }
        BinaryPrimitives.WriteUInt64LittleEndian(destination, Significand);
        bytesWritten = 8;
        return true;
    }
}

/// <summary>
/// A decimal (Radix 10) IEEE 754 float from outside the BCL, in the manner of
/// decimal64 (JYRO-ADR-158 (a4), JYRO-DEF-628): a value is a coefficient c, an
/// unsigned integer, times 10 to the power q, with a sign. The significand writers
/// write c, the exponent writers q, and ILogB gives the exponent of c's leading
/// digit, q + digits(c) - 1. Its double, which the arithmetic members use, is only
/// the nearest double of that value; no reader should take it from there.
/// </summary>
public sealed class DecimalRadixFloat : DoubleBackedFloat, IClassFloatCore<DecimalRadixFloat>
{
    public BigInteger Coefficient { get; }
    public int Quantum { get; }
    public bool Negative { get; }
    public bool IsNotANumber { get; }

    private DecimalRadixFloat(double nearest, bool negative, BigInteger coefficient, int quantum, bool nan) : base(nearest)
    {
        Negative = negative;
        Coefficient = coefficient;
        Quantum = quantum;
        IsNotANumber = nan;
    }

    /// <summary>The value (-1)^negative * coefficient * 10^quantum, finite at any quantum.</summary>
    public static DecimalRadixFloat Of(bool negative, BigInteger coefficient, int quantum)
    {
        var text = (negative ? "-" : "") + coefficient.ToString(CultureInfo.InvariantCulture) + "e" + quantum.ToString(CultureInfo.InvariantCulture);
        return new(double.Parse(text, NumberStyles.Float, CultureInfo.InvariantCulture), negative, coefficient, quantum, false);
    }

    public static DecimalRadixFloat NaNValue => new(double.NaN, false, BigInteger.Zero, 0, true);

    public static DecimalRadixFloat Make(double value) => new(value, double.IsNegative(value), BigInteger.Zero, 0, double.IsNaN(value));

    public static int Radix => 10;

    public static int ILogB(DecimalRadixFloat x) =>
        x.IsNotANumber ? int.MaxValue
        : x.Coefficient.IsZero ? int.MinValue
        : x.Quantum + x.Coefficient.ToString(CultureInfo.InvariantCulture).Length - 1;

    // Its class: a finite value at every quantum, whatever its nearest double is.
    public static bool IsNaN(DecimalRadixFloat x) => x.IsNotANumber;
    public static bool IsInfinity(DecimalRadixFloat x) => false;
    public static bool IsPositiveInfinity(DecimalRadixFloat x) => false;
    public static bool IsNegativeInfinity(DecimalRadixFloat x) => false;
    public static bool IsFinite(DecimalRadixFloat x) => !x.IsNotANumber;
    public static bool IsNegative(DecimalRadixFloat x) => !x.IsNotANumber && x.Negative;
    public static bool IsPositive(DecimalRadixFloat x) => !x.IsNotANumber && !x.Negative;
    public static bool IsZero(DecimalRadixFloat x) => !x.IsNotANumber && x.Coefficient.IsZero;
    public static bool IsNormal(DecimalRadixFloat x) => !x.IsNotANumber && !x.Coefficient.IsZero;
    public static bool IsSubnormal(DecimalRadixFloat x) => false;

    public override int GetExponentByteCount() => sizeof(int);
    public override int GetExponentShortestBitLength() => IeeeConventionFloat.ShortestBits(Quantum);
    public override int GetSignificandBitLength() => 54;
    public override int GetSignificandByteCount() => Math.Max(1, Coefficient.GetByteCount(isUnsigned: true));

    public override bool TryWriteExponentBigEndian(Span<byte> destination, out int bytesWritten)
    {
        if (destination.Length < 4) { bytesWritten = 0; return false; }
        BinaryPrimitives.WriteInt32BigEndian(destination, Quantum);
        bytesWritten = 4;
        return true;
    }

    public override bool TryWriteExponentLittleEndian(Span<byte> destination, out int bytesWritten)
    {
        if (destination.Length < 4) { bytesWritten = 0; return false; }
        BinaryPrimitives.WriteInt32LittleEndian(destination, Quantum);
        bytesWritten = 4;
        return true;
    }

    public override bool TryWriteSignificandBigEndian(Span<byte> destination, out int bytesWritten)
    {
        var n = GetSignificandByteCount();
        if (destination.Length < n) { bytesWritten = 0; return false; }
        destination[..n].Clear();
        Coefficient.TryWriteBytes(destination[..n], out _, isUnsigned: true, isBigEndian: true);
        bytesWritten = n;
        return true;
    }

    public override bool TryWriteSignificandLittleEndian(Span<byte> destination, out int bytesWritten)
    {
        var n = GetSignificandByteCount();
        if (destination.Length < n) { bytesWritten = 0; return false; }
        destination[..n].Clear();
        Coefficient.TryWriteBytes(destination[..n], out _, isUnsigned: true, isBigEndian: false);
        bytesWritten = n;
        return true;
    }
}

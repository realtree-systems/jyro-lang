using System;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;

namespace RealTree.Jyro.Spec.Tests.P30.HostNumbers;

/// <summary>
/// IBinaryInteger&lt;TSelf&gt; for a class holding a BigInteger, every member that
/// names TSelf given here once, so one class can implement the interface for
/// itself and for its base class (JYRO-ADR-158 (a2)). The members that do not name
/// TSelf (the byte writers, the text) are the class's own, shared by every T.
/// </summary>
public interface IClassIntegerCore<TSelf> : IBinaryInteger<TSelf> where TSelf : IClassIntegerCore<TSelf>
{
    BigInteger Value { get; }

    static abstract TSelf Make(BigInteger value);

    // The text and conversions the interface leaves to the implementer: refused.
    static bool INumberBase<TSelf>.TryConvertFromChecked<TOther>(TOther value, out TSelf result) { result = default!; return false; }
    static bool INumberBase<TSelf>.TryConvertFromSaturating<TOther>(TOther value, out TSelf result) { result = default!; return false; }
    static bool INumberBase<TSelf>.TryConvertFromTruncating<TOther>(TOther value, out TSelf result) { result = default!; return false; }
    static bool INumberBase<TSelf>.TryConvertToChecked<TOther>(TSelf value, [MaybeNullWhen(false)] out TOther result) { result = default; return false; }
    static bool INumberBase<TSelf>.TryConvertToSaturating<TOther>(TSelf value, [MaybeNullWhen(false)] out TOther result) { result = default; return false; }
    static bool INumberBase<TSelf>.TryConvertToTruncating<TOther>(TSelf value, [MaybeNullWhen(false)] out TOther result) { result = default; return false; }

    static bool IBinaryInteger<TSelf>.TryReadBigEndian(ReadOnlySpan<byte> source, bool isUnsigned, out TSelf value)
    { value = TSelf.Make(new BigInteger(source, isUnsigned, isBigEndian: true)); return true; }
    static bool IBinaryInteger<TSelf>.TryReadLittleEndian(ReadOnlySpan<byte> source, bool isUnsigned, out TSelf value)
    { value = TSelf.Make(new BigInteger(source, isUnsigned, isBigEndian: false)); return true; }

    static TSelf INumberBase<TSelf>.One => TSelf.Make(BigInteger.One);
    static int INumberBase<TSelf>.Radix => 2;
    static TSelf INumberBase<TSelf>.Zero => TSelf.Make(BigInteger.Zero);
    static TSelf IAdditiveIdentity<TSelf, TSelf>.AdditiveIdentity => TSelf.Make(BigInteger.Zero);
    static TSelf IMultiplicativeIdentity<TSelf, TSelf>.MultiplicativeIdentity => TSelf.Make(BigInteger.One);

    static TSelf INumberBase<TSelf>.Abs(TSelf value) => TSelf.Make(BigInteger.Abs(value.Value));
    static bool INumberBase<TSelf>.IsCanonical(TSelf value) => true;
    static bool INumberBase<TSelf>.IsComplexNumber(TSelf value) => false;
    static bool INumberBase<TSelf>.IsEvenInteger(TSelf value) => value.Value.IsEven;
    static bool INumberBase<TSelf>.IsFinite(TSelf value) => true;
    static bool INumberBase<TSelf>.IsImaginaryNumber(TSelf value) => false;
    static bool INumberBase<TSelf>.IsInfinity(TSelf value) => false;
    static bool INumberBase<TSelf>.IsInteger(TSelf value) => true;
    static bool INumberBase<TSelf>.IsNaN(TSelf value) => false;
    static bool INumberBase<TSelf>.IsNegative(TSelf value) => value.Value.Sign < 0;
    static bool INumberBase<TSelf>.IsNegativeInfinity(TSelf value) => false;
    static bool INumberBase<TSelf>.IsNormal(TSelf value) => !value.Value.IsZero;
    static bool INumberBase<TSelf>.IsOddInteger(TSelf value) => !value.Value.IsEven;
    static bool INumberBase<TSelf>.IsPositive(TSelf value) => value.Value.Sign >= 0;
    static bool INumberBase<TSelf>.IsPositiveInfinity(TSelf value) => false;
    static bool INumberBase<TSelf>.IsRealNumber(TSelf value) => true;
    static bool INumberBase<TSelf>.IsSubnormal(TSelf value) => false;
    static bool INumberBase<TSelf>.IsZero(TSelf value) => value.Value.IsZero;
    static bool IBinaryNumber<TSelf>.IsPow2(TSelf value) => BigInteger.IsPow2(value.Value);
    static TSelf IBinaryNumber<TSelf>.Log2(TSelf value) => TSelf.Make(BigInteger.Log2(value.Value));
    static TSelf INumberBase<TSelf>.MaxMagnitude(TSelf x, TSelf y) => TSelf.Make(BigInteger.MaxMagnitude(x.Value, y.Value));
    static TSelf INumberBase<TSelf>.MaxMagnitudeNumber(TSelf x, TSelf y) => TSelf.Make(BigInteger.MaxMagnitude(x.Value, y.Value));
    static TSelf INumberBase<TSelf>.MinMagnitude(TSelf x, TSelf y) => TSelf.Make(BigInteger.MinMagnitude(x.Value, y.Value));
    static TSelf INumberBase<TSelf>.MinMagnitudeNumber(TSelf x, TSelf y) => TSelf.Make(BigInteger.MinMagnitude(x.Value, y.Value));
    static TSelf IBinaryInteger<TSelf>.PopCount(TSelf value) => TSelf.Make(BigInteger.PopCount(value.Value));
    static TSelf IBinaryInteger<TSelf>.TrailingZeroCount(TSelf value) => TSelf.Make(BigInteger.TrailingZeroCount(value.Value));

    static TSelf INumberBase<TSelf>.Parse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider) => TSelf.Make(BigInteger.Parse(s, style, provider));
    static TSelf INumberBase<TSelf>.Parse(string s, NumberStyles style, IFormatProvider? provider) => TSelf.Make(BigInteger.Parse(s, style, provider));
    static TSelf ISpanParsable<TSelf>.Parse(ReadOnlySpan<char> s, IFormatProvider? provider) => TSelf.Make(BigInteger.Parse(s, provider));
    static TSelf IParsable<TSelf>.Parse(string s, IFormatProvider? provider) => TSelf.Make(BigInteger.Parse(s, provider));
    static bool INumberBase<TSelf>.TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider, out TSelf result)
    { var ok = BigInteger.TryParse(s, style, provider, out var v); result = TSelf.Make(v); return ok; }
    static bool INumberBase<TSelf>.TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, out TSelf result)
    { var ok = BigInteger.TryParse(s, style, provider, out var v); result = TSelf.Make(v); return ok; }
    static bool ISpanParsable<TSelf>.TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, out TSelf result)
    { var ok = BigInteger.TryParse(s, provider, out var v); result = TSelf.Make(v); return ok; }
    static bool IParsable<TSelf>.TryParse([NotNullWhen(true)] string? s, IFormatProvider? provider, out TSelf result)
    { var ok = BigInteger.TryParse(s, provider, out var v); result = TSelf.Make(v); return ok; }

    int IComparable<TSelf>.CompareTo(TSelf? other) => other is null ? 1 : Value.CompareTo(other.Value);
    bool IEquatable<TSelf>.Equals(TSelf? other) => other is not null && Value.Equals(other.Value);

    static TSelf IUnaryPlusOperators<TSelf, TSelf>.operator +(TSelf value) => value;
    static TSelf IAdditionOperators<TSelf, TSelf, TSelf>.operator +(TSelf left, TSelf right) => TSelf.Make(left.Value + right.Value);
    static TSelf IUnaryNegationOperators<TSelf, TSelf>.operator -(TSelf value) => TSelf.Make(-value.Value);
    static TSelf ISubtractionOperators<TSelf, TSelf, TSelf>.operator -(TSelf left, TSelf right) => TSelf.Make(left.Value - right.Value);
    static TSelf IBitwiseOperators<TSelf, TSelf, TSelf>.operator ~(TSelf value) => TSelf.Make(~value.Value);
    static TSelf IIncrementOperators<TSelf>.operator ++(TSelf value) => TSelf.Make(value.Value + 1);
    static TSelf IDecrementOperators<TSelf>.operator --(TSelf value) => TSelf.Make(value.Value - 1);
    static TSelf IMultiplyOperators<TSelf, TSelf, TSelf>.operator *(TSelf left, TSelf right) => TSelf.Make(left.Value * right.Value);
    static TSelf IDivisionOperators<TSelf, TSelf, TSelf>.operator /(TSelf left, TSelf right) => TSelf.Make(left.Value / right.Value);
    static TSelf IModulusOperators<TSelf, TSelf, TSelf>.operator %(TSelf left, TSelf right) => TSelf.Make(left.Value % right.Value);
    static TSelf IBitwiseOperators<TSelf, TSelf, TSelf>.operator &(TSelf left, TSelf right) => TSelf.Make(left.Value & right.Value);
    static TSelf IBitwiseOperators<TSelf, TSelf, TSelf>.operator |(TSelf left, TSelf right) => TSelf.Make(left.Value | right.Value);
    static TSelf IBitwiseOperators<TSelf, TSelf, TSelf>.operator ^(TSelf left, TSelf right) => TSelf.Make(left.Value ^ right.Value);
    static TSelf IShiftOperators<TSelf, int, TSelf>.operator <<(TSelf value, int shiftAmount) => TSelf.Make(value.Value << shiftAmount);
    static TSelf IShiftOperators<TSelf, int, TSelf>.operator >>(TSelf value, int shiftAmount) => TSelf.Make(value.Value >> shiftAmount);
    static TSelf IShiftOperators<TSelf, int, TSelf>.operator >>>(TSelf value, int shiftAmount) => TSelf.Make(value.Value >>> shiftAmount);
    static bool IEqualityOperators<TSelf, TSelf, bool>.operator ==(TSelf? left, TSelf? right) => left is null ? right is null : right is not null && left.Value == right.Value;
    static bool IEqualityOperators<TSelf, TSelf, bool>.operator !=(TSelf? left, TSelf? right) => !(left is null ? right is null : right is not null && left.Value == right.Value);
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator <(TSelf left, TSelf right) => left.Value < right.Value;
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator >(TSelf left, TSelf right) => left.Value > right.Value;
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator <=(TSelf left, TSelf right) => left.Value <= right.Value;
    static bool IComparisonOperators<TSelf, TSelf, bool>.operator >=(TSelf left, TSelf right) => left.Value >= right.Value;
}

/// <summary>
/// An integer class from outside the BCL that is not sealed (JYRO-DEF-625). It
/// implements IBinaryInteger&lt;BaseInteger&gt;; its text is not a number's.
/// </summary>
public class BaseInteger : IClassIntegerCore<BaseInteger>
{
    public BigInteger Value { get; }

    public BaseInteger(BigInteger value) { Value = value; }

    public static BaseInteger Make(BigInteger value) => new(value);

    public override string ToString() => "0x" + Value.ToString("X", CultureInfo.InvariantCulture);
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

    public int GetByteCount() => Value.GetByteCount(isUnsigned: false);
    public int GetShortestBitLength() => (int)Value.GetBitLength();
    public bool TryWriteBigEndian(Span<byte> destination, out int bytesWritten) =>
        Value.TryWriteBytes(destination, out bytesWritten, isUnsigned: false, isBigEndian: true);
    public bool TryWriteLittleEndian(Span<byte> destination, out int bytesWritten) =>
        Value.TryWriteBytes(destination, out bytesWritten, isUnsigned: false, isBigEndian: false);

    public int CompareTo(object? obj) => obj is BaseInteger o ? Value.CompareTo(o.Value) : 1;
    public override bool Equals(object? obj) => obj is BaseInteger o && Value.Equals(o.Value);
    public override int GetHashCode() => Value.GetHashCode();
}

/// <summary>
/// A class derived from <see cref="BaseInteger"/> that adds nothing: it
/// implements IBinaryInteger only for its base, so it is a number read as a
/// BaseInteger (JYRO-ADR-158 (a2), JYRO-DEF-625).
/// </summary>
public sealed class DerivedInteger : BaseInteger
{
    public DerivedInteger(BigInteger value) : base(value) { }
}

/// <summary>
/// A class derived from <see cref="BaseInteger"/> that is a number in its own
/// right: it implements IBinaryInteger for itself and, through its base, for
/// BaseInteger, two different T (JYRO-ADR-158 (a2), JYRO-DEF-627).
/// </summary>
public sealed class SelfAndBaseInteger : BaseInteger, IClassIntegerCore<SelfAndBaseInteger>
{
    public SelfAndBaseInteger(BigInteger value) : base(value) { }

    public static new SelfAndBaseInteger Make(BigInteger value) => new(value);
}

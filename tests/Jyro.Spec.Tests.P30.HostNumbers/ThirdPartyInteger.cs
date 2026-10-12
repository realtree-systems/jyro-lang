using System;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;

namespace RealTree.Jyro.Spec.Tests.P30.HostNumbers;

/// <summary>
/// An integer type from outside the BCL (JYRO-ADR-157 (a1), JYRO-DEF-621). Its
/// arithmetic is a BigInteger's, so every member the interface requires gives
/// the right answer. What the interface leaves to the implementer differs from
/// the BCL's own integers: every ToString and TryFormat writes hexadecimal with
/// a 0x prefix, whatever the format (so "D" is not decimal digits), and every
/// TryConvertTo* and TryConvertFrom* returns false.
/// </summary>
public readonly struct ThirdPartyInteger : IBinaryInteger<ThirdPartyInteger>
{
    private readonly BigInteger _value;
    private readonly bool _broken;

    public ThirdPartyInteger(BigInteger value) { _value = value; _broken = false; }

    private ThirdPartyInteger(BigInteger value, bool broken) { _value = value; _broken = broken; }

    /// <summary>A value whose required members GetByteCount, GetShortestBitLength and
    /// TryWrite*Endian throw <see cref="BrokenNumberException"/>.</summary>
    public static ThirdPartyInteger Broken(BigInteger value) => new(value, true);

    private static ThirdPartyInteger Of(BigInteger v) => new(v);

    private void ThrowIfBroken()
    {
        if (_broken) throw new BrokenNumberException("the host's integer type is broken");
    }

    // -- the text the interface leaves to the implementer --------------------------

    public override string ToString() => "0x" + _value.ToString("X", CultureInfo.InvariantCulture);
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

    // -- conversions the interface lets an implementer refuse -----------------------

    public static bool TryConvertFromChecked<TOther>(TOther value, out ThirdPartyInteger result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertFromSaturating<TOther>(TOther value, out ThirdPartyInteger result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertFromTruncating<TOther>(TOther value, out ThirdPartyInteger result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertToChecked<TOther>(ThirdPartyInteger value, [MaybeNullWhen(false)] out TOther result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertToSaturating<TOther>(ThirdPartyInteger value, [MaybeNullWhen(false)] out TOther result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertToTruncating<TOther>(ThirdPartyInteger value, [MaybeNullWhen(false)] out TOther result) where TOther : INumberBase<TOther> { result = default; return false; }

    // -- the members every implementer must support ---------------------------------

    public int GetByteCount() { ThrowIfBroken(); return _value.GetByteCount(isUnsigned: false); }

    public int GetShortestBitLength() { ThrowIfBroken(); return (int)_value.GetBitLength(); }

    public bool TryWriteBigEndian(Span<byte> destination, out int bytesWritten)
    {
        ThrowIfBroken();
        return _value.TryWriteBytes(destination, out bytesWritten, isUnsigned: false, isBigEndian: true);
    }

    public bool TryWriteLittleEndian(Span<byte> destination, out int bytesWritten)
    {
        ThrowIfBroken();
        return _value.TryWriteBytes(destination, out bytesWritten, isUnsigned: false, isBigEndian: false);
    }

    public static bool TryReadBigEndian(ReadOnlySpan<byte> source, bool isUnsigned, out ThirdPartyInteger value)
    { value = Of(new BigInteger(source, isUnsigned, isBigEndian: true)); return true; }

    public static bool TryReadLittleEndian(ReadOnlySpan<byte> source, bool isUnsigned, out ThirdPartyInteger value)
    { value = Of(new BigInteger(source, isUnsigned, isBigEndian: false)); return true; }

    public static ThirdPartyInteger One => Of(BigInteger.One);
    public static int Radix => 2;
    public static ThirdPartyInteger Zero => Of(BigInteger.Zero);
    public static ThirdPartyInteger AdditiveIdentity => Zero;
    public static ThirdPartyInteger MultiplicativeIdentity => One;

    public static ThirdPartyInteger Abs(ThirdPartyInteger value) => Of(BigInteger.Abs(value._value));
    public static bool IsCanonical(ThirdPartyInteger value) => true;
    public static bool IsComplexNumber(ThirdPartyInteger value) => false;
    public static bool IsEvenInteger(ThirdPartyInteger value) => value._value.IsEven;
    public static bool IsFinite(ThirdPartyInteger value) => true;
    public static bool IsImaginaryNumber(ThirdPartyInteger value) => false;
    public static bool IsInfinity(ThirdPartyInteger value) => false;
    public static bool IsInteger(ThirdPartyInteger value) => true;
    public static bool IsNaN(ThirdPartyInteger value) => false;
    public static bool IsNegative(ThirdPartyInteger value) => value._value.Sign < 0;
    public static bool IsNegativeInfinity(ThirdPartyInteger value) => false;
    public static bool IsNormal(ThirdPartyInteger value) => !value._value.IsZero;
    public static bool IsOddInteger(ThirdPartyInteger value) => !value._value.IsEven;
    public static bool IsPositive(ThirdPartyInteger value) => value._value.Sign >= 0;
    public static bool IsPositiveInfinity(ThirdPartyInteger value) => false;
    public static bool IsRealNumber(ThirdPartyInteger value) => true;
    public static bool IsSubnormal(ThirdPartyInteger value) => false;
    public static bool IsZero(ThirdPartyInteger value) => value._value.IsZero;
    public static bool IsPow2(ThirdPartyInteger value) => BigInteger.IsPow2(value._value);
    public static ThirdPartyInteger Log2(ThirdPartyInteger value) => Of(BigInteger.Log2(value._value));
    public static ThirdPartyInteger MaxMagnitude(ThirdPartyInteger x, ThirdPartyInteger y) => Of(BigInteger.MaxMagnitude(x._value, y._value));
    public static ThirdPartyInteger MaxMagnitudeNumber(ThirdPartyInteger x, ThirdPartyInteger y) => MaxMagnitude(x, y);
    public static ThirdPartyInteger MinMagnitude(ThirdPartyInteger x, ThirdPartyInteger y) => Of(BigInteger.MinMagnitude(x._value, y._value));
    public static ThirdPartyInteger MinMagnitudeNumber(ThirdPartyInteger x, ThirdPartyInteger y) => MinMagnitude(x, y);
    public static ThirdPartyInteger PopCount(ThirdPartyInteger value) => Of(BigInteger.PopCount(value._value));
    public static ThirdPartyInteger TrailingZeroCount(ThirdPartyInteger value) => Of(BigInteger.TrailingZeroCount(value._value));

    public static ThirdPartyInteger Parse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider) => Of(BigInteger.Parse(s, style, provider));
    public static ThirdPartyInteger Parse(string s, NumberStyles style, IFormatProvider? provider) => Of(BigInteger.Parse(s, style, provider));
    public static ThirdPartyInteger Parse(ReadOnlySpan<char> s, IFormatProvider? provider) => Of(BigInteger.Parse(s, provider));
    public static ThirdPartyInteger Parse(string s, IFormatProvider? provider) => Of(BigInteger.Parse(s, provider));

    public static bool TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider, out ThirdPartyInteger result)
    { var ok = BigInteger.TryParse(s, style, provider, out var v); result = Of(v); return ok; }
    public static bool TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, out ThirdPartyInteger result)
    { var ok = BigInteger.TryParse(s, style, provider, out var v); result = Of(v); return ok; }
    public static bool TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, out ThirdPartyInteger result)
    { var ok = BigInteger.TryParse(s, provider, out var v); result = Of(v); return ok; }
    public static bool TryParse([NotNullWhen(true)] string? s, IFormatProvider? provider, out ThirdPartyInteger result)
    { var ok = BigInteger.TryParse(s, provider, out var v); result = Of(v); return ok; }

    public int CompareTo(object? obj) => obj is ThirdPartyInteger o ? CompareTo(o) : 1;
    public int CompareTo(ThirdPartyInteger other) => _value.CompareTo(other._value);
    public bool Equals(ThirdPartyInteger other) => _value.Equals(other._value);
    public override bool Equals(object? obj) => obj is ThirdPartyInteger o && Equals(o);
    public override int GetHashCode() => _value.GetHashCode();

    public static ThirdPartyInteger operator +(ThirdPartyInteger value) => value;
    public static ThirdPartyInteger operator +(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value + right._value);
    public static ThirdPartyInteger operator -(ThirdPartyInteger value) => Of(-value._value);
    public static ThirdPartyInteger operator -(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value - right._value);
    public static ThirdPartyInteger operator ~(ThirdPartyInteger value) => Of(~value._value);
    public static ThirdPartyInteger operator ++(ThirdPartyInteger value) => Of(value._value + 1);
    public static ThirdPartyInteger operator --(ThirdPartyInteger value) => Of(value._value - 1);
    public static ThirdPartyInteger operator *(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value * right._value);
    public static ThirdPartyInteger operator /(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value / right._value);
    public static ThirdPartyInteger operator %(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value % right._value);
    public static ThirdPartyInteger operator &(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value & right._value);
    public static ThirdPartyInteger operator |(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value | right._value);
    public static ThirdPartyInteger operator ^(ThirdPartyInteger left, ThirdPartyInteger right) => Of(left._value ^ right._value);
    public static ThirdPartyInteger operator <<(ThirdPartyInteger value, int shiftAmount) => Of(value._value << shiftAmount);
    public static ThirdPartyInteger operator >>(ThirdPartyInteger value, int shiftAmount) => Of(value._value >> shiftAmount);
    public static ThirdPartyInteger operator >>>(ThirdPartyInteger value, int shiftAmount) => Of(value._value >>> shiftAmount);
    public static bool operator ==(ThirdPartyInteger left, ThirdPartyInteger right) => left._value == right._value;
    public static bool operator !=(ThirdPartyInteger left, ThirdPartyInteger right) => left._value != right._value;
    public static bool operator <(ThirdPartyInteger left, ThirdPartyInteger right) => left._value < right._value;
    public static bool operator >(ThirdPartyInteger left, ThirdPartyInteger right) => left._value > right._value;
    public static bool operator <=(ThirdPartyInteger left, ThirdPartyInteger right) => left._value <= right._value;
    public static bool operator >=(ThirdPartyInteger left, ThirdPartyInteger right) => left._value >= right._value;
}

/// <summary>What a broken host number's required member throws.</summary>
public sealed class BrokenNumberException(string message) : Exception(message);

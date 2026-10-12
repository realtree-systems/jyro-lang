using System;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Numerics;

namespace RealTree.Jyro.Spec.Tests.P30.HostNumbers;

/// <summary>
/// A binary floating-point type from outside the BCL (JYRO-ADR-157 (a1),
/// JYRO-DEF-622). It holds an IEEE 754 double and its arithmetic is a double's,
/// so every member the interface requires gives the right answer, its exponent
/// and significand writers included. What the interface leaves to the
/// implementer differs from the BCL's: every TryConvertTo* and TryConvertFrom*
/// returns false, so Double.CreateTruncating cannot convert it, and its text
/// is not a number's.
/// </summary>
public readonly struct ThirdPartyFloat : IFloatingPointIeee754<ThirdPartyFloat>
{
    private readonly double _v;

    public ThirdPartyFloat(double value) { _v = value; }

    private static ThirdPartyFloat Of(double v) => new(v);

    // -- the text and conversions the interface leaves to the implementer ----------

    public override string ToString() => "ThirdPartyFloat";
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

    public static bool TryConvertFromChecked<TOther>(TOther value, out ThirdPartyFloat result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertFromSaturating<TOther>(TOther value, out ThirdPartyFloat result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertFromTruncating<TOther>(TOther value, out ThirdPartyFloat result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertToChecked<TOther>(ThirdPartyFloat value, [MaybeNullWhen(false)] out TOther result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertToSaturating<TOther>(ThirdPartyFloat value, [MaybeNullWhen(false)] out TOther result) where TOther : INumberBase<TOther> { result = default; return false; }
    public static bool TryConvertToTruncating<TOther>(ThirdPartyFloat value, [MaybeNullWhen(false)] out TOther result) where TOther : INumberBase<TOther> { result = default; return false; }

    // -- IFloatingPoint<T>: the exponent and significand writers -------------------

    private static IFloatingPoint<double> F(ThirdPartyFloat x) => x._v;

    public int GetExponentByteCount() => F(this).GetExponentByteCount();
    public int GetExponentShortestBitLength() => F(this).GetExponentShortestBitLength();
    public int GetSignificandBitLength() => F(this).GetSignificandBitLength();
    public int GetSignificandByteCount() => F(this).GetSignificandByteCount();
    public bool TryWriteExponentBigEndian(Span<byte> destination, out int bytesWritten) => F(this).TryWriteExponentBigEndian(destination, out bytesWritten);
    public bool TryWriteExponentLittleEndian(Span<byte> destination, out int bytesWritten) => F(this).TryWriteExponentLittleEndian(destination, out bytesWritten);
    public bool TryWriteSignificandBigEndian(Span<byte> destination, out int bytesWritten) => F(this).TryWriteSignificandBigEndian(destination, out bytesWritten);
    public bool TryWriteSignificandLittleEndian(Span<byte> destination, out int bytesWritten) => F(this).TryWriteSignificandLittleEndian(destination, out bytesWritten);

    // -- INumberBase<T> ------------------------------------------------------------

    public static ThirdPartyFloat One => Of(1.0);
    public static int Radix => 2;
    public static ThirdPartyFloat Zero => Of(0.0);
    public static ThirdPartyFloat AdditiveIdentity => Zero;
    public static ThirdPartyFloat MultiplicativeIdentity => One;
    public static ThirdPartyFloat NegativeOne => Of(-1.0);

    public static ThirdPartyFloat Abs(ThirdPartyFloat value) => Of(double.Abs(value._v));
    public static bool IsCanonical(ThirdPartyFloat value) => true;
    public static bool IsComplexNumber(ThirdPartyFloat value) => false;
    public static bool IsEvenInteger(ThirdPartyFloat value) => double.IsEvenInteger(value._v);
    public static bool IsFinite(ThirdPartyFloat value) => double.IsFinite(value._v);
    public static bool IsImaginaryNumber(ThirdPartyFloat value) => false;
    public static bool IsInfinity(ThirdPartyFloat value) => double.IsInfinity(value._v);
    public static bool IsInteger(ThirdPartyFloat value) => double.IsInteger(value._v);
    public static bool IsNaN(ThirdPartyFloat value) => double.IsNaN(value._v);
    public static bool IsNegative(ThirdPartyFloat value) => double.IsNegative(value._v);
    public static bool IsNegativeInfinity(ThirdPartyFloat value) => double.IsNegativeInfinity(value._v);
    public static bool IsNormal(ThirdPartyFloat value) => double.IsNormal(value._v);
    public static bool IsOddInteger(ThirdPartyFloat value) => double.IsOddInteger(value._v);
    public static bool IsPositive(ThirdPartyFloat value) => double.IsPositive(value._v);
    public static bool IsPositiveInfinity(ThirdPartyFloat value) => double.IsPositiveInfinity(value._v);
    public static bool IsRealNumber(ThirdPartyFloat value) => double.IsRealNumber(value._v);
    public static bool IsSubnormal(ThirdPartyFloat value) => double.IsSubnormal(value._v);
    public static bool IsZero(ThirdPartyFloat value) => value._v == 0.0;
    public static ThirdPartyFloat MaxMagnitude(ThirdPartyFloat x, ThirdPartyFloat y) => Of(double.MaxMagnitude(x._v, y._v));
    public static ThirdPartyFloat MaxMagnitudeNumber(ThirdPartyFloat x, ThirdPartyFloat y) => Of(double.MaxMagnitudeNumber(x._v, y._v));
    public static ThirdPartyFloat MinMagnitude(ThirdPartyFloat x, ThirdPartyFloat y) => Of(double.MinMagnitude(x._v, y._v));
    public static ThirdPartyFloat MinMagnitudeNumber(ThirdPartyFloat x, ThirdPartyFloat y) => Of(double.MinMagnitudeNumber(x._v, y._v));

    public static ThirdPartyFloat Parse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider) => Of(double.Parse(s, style, provider));
    public static ThirdPartyFloat Parse(string s, NumberStyles style, IFormatProvider? provider) => Of(double.Parse(s, style, provider));
    public static ThirdPartyFloat Parse(ReadOnlySpan<char> s, IFormatProvider? provider) => Of(double.Parse(s, provider));
    public static ThirdPartyFloat Parse(string s, IFormatProvider? provider) => Of(double.Parse(s, provider));

    public static bool TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider, out ThirdPartyFloat result)
    { var ok = double.TryParse(s, style, provider, out var v); result = Of(v); return ok; }
    public static bool TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, out ThirdPartyFloat result)
    { var ok = double.TryParse(s, style, provider, out var v); result = Of(v); return ok; }
    public static bool TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, out ThirdPartyFloat result)
    { var ok = double.TryParse(s, provider, out var v); result = Of(v); return ok; }
    public static bool TryParse([NotNullWhen(true)] string? s, IFormatProvider? provider, out ThirdPartyFloat result)
    { var ok = double.TryParse(s, provider, out var v); result = Of(v); return ok; }

    public int CompareTo(object? obj) => obj is ThirdPartyFloat o ? CompareTo(o) : 1;
    public int CompareTo(ThirdPartyFloat other) => _v.CompareTo(other._v);
    public bool Equals(ThirdPartyFloat other) => _v.Equals(other._v);
    public override bool Equals(object? obj) => obj is ThirdPartyFloat o && Equals(o);
    public override int GetHashCode() => _v.GetHashCode();

    // -- IFloatingPointConstants, IFloatingPointIeee754 ----------------------------

    public static ThirdPartyFloat E => Of(double.E);
    public static ThirdPartyFloat Pi => Of(double.Pi);
    public static ThirdPartyFloat Tau => Of(double.Tau);
    public static ThirdPartyFloat Epsilon => Of(double.Epsilon);
    public static ThirdPartyFloat NaN => Of(double.NaN);
    public static ThirdPartyFloat NegativeInfinity => Of(double.NegativeInfinity);
    public static ThirdPartyFloat NegativeZero => Of(double.NegativeZero);
    public static ThirdPartyFloat PositiveInfinity => Of(double.PositiveInfinity);

    public static ThirdPartyFloat Atan2(ThirdPartyFloat y, ThirdPartyFloat x) => Of(double.Atan2(y._v, x._v));
    public static ThirdPartyFloat Atan2Pi(ThirdPartyFloat y, ThirdPartyFloat x) => Of(double.Atan2Pi(y._v, x._v));
    public static ThirdPartyFloat BitDecrement(ThirdPartyFloat x) => Of(double.BitDecrement(x._v));
    public static ThirdPartyFloat BitIncrement(ThirdPartyFloat x) => Of(double.BitIncrement(x._v));
    public static ThirdPartyFloat FusedMultiplyAdd(ThirdPartyFloat left, ThirdPartyFloat right, ThirdPartyFloat addend) => Of(double.FusedMultiplyAdd(left._v, right._v, addend._v));
    public static ThirdPartyFloat Ieee754Remainder(ThirdPartyFloat left, ThirdPartyFloat right) => Of(double.Ieee754Remainder(left._v, right._v));
    public static int ILogB(ThirdPartyFloat x) => double.ILogB(x._v);
    public static ThirdPartyFloat ScaleB(ThirdPartyFloat x, int n) => Of(double.ScaleB(x._v, n));

    public static ThirdPartyFloat Round(ThirdPartyFloat x, int digits, MidpointRounding mode) => Of(double.Round(x._v, digits, mode));

    public static ThirdPartyFloat Exp(ThirdPartyFloat x) => Of(double.Exp(x._v));
    public static ThirdPartyFloat Exp10(ThirdPartyFloat x) => Of(double.Exp10(x._v));
    public static ThirdPartyFloat Exp2(ThirdPartyFloat x) => Of(double.Exp2(x._v));
    public static ThirdPartyFloat Acosh(ThirdPartyFloat x) => Of(double.Acosh(x._v));
    public static ThirdPartyFloat Asinh(ThirdPartyFloat x) => Of(double.Asinh(x._v));
    public static ThirdPartyFloat Atanh(ThirdPartyFloat x) => Of(double.Atanh(x._v));
    public static ThirdPartyFloat Cosh(ThirdPartyFloat x) => Of(double.Cosh(x._v));
    public static ThirdPartyFloat Sinh(ThirdPartyFloat x) => Of(double.Sinh(x._v));
    public static ThirdPartyFloat Tanh(ThirdPartyFloat x) => Of(double.Tanh(x._v));
    public static ThirdPartyFloat Log(ThirdPartyFloat x) => Of(double.Log(x._v));
    public static ThirdPartyFloat Log(ThirdPartyFloat x, ThirdPartyFloat newBase) => Of(double.Log(x._v, newBase._v));
    public static ThirdPartyFloat Log10(ThirdPartyFloat x) => Of(double.Log10(x._v));
    public static ThirdPartyFloat Log2(ThirdPartyFloat x) => Of(double.Log2(x._v));
    public static ThirdPartyFloat Pow(ThirdPartyFloat x, ThirdPartyFloat y) => Of(double.Pow(x._v, y._v));
    public static ThirdPartyFloat Cbrt(ThirdPartyFloat x) => Of(double.Cbrt(x._v));
    public static ThirdPartyFloat Hypot(ThirdPartyFloat x, ThirdPartyFloat y) => Of(double.Hypot(x._v, y._v));
    public static ThirdPartyFloat RootN(ThirdPartyFloat x, int n) => Of(double.RootN(x._v, n));
    public static ThirdPartyFloat Sqrt(ThirdPartyFloat x) => Of(double.Sqrt(x._v));
    public static ThirdPartyFloat Acos(ThirdPartyFloat x) => Of(double.Acos(x._v));
    public static ThirdPartyFloat AcosPi(ThirdPartyFloat x) => Of(double.AcosPi(x._v));
    public static ThirdPartyFloat Asin(ThirdPartyFloat x) => Of(double.Asin(x._v));
    public static ThirdPartyFloat AsinPi(ThirdPartyFloat x) => Of(double.AsinPi(x._v));
    public static ThirdPartyFloat Atan(ThirdPartyFloat x) => Of(double.Atan(x._v));
    public static ThirdPartyFloat AtanPi(ThirdPartyFloat x) => Of(double.AtanPi(x._v));
    public static ThirdPartyFloat Cos(ThirdPartyFloat x) => Of(double.Cos(x._v));
    public static ThirdPartyFloat CosPi(ThirdPartyFloat x) => Of(double.CosPi(x._v));
    public static ThirdPartyFloat Sin(ThirdPartyFloat x) => Of(double.Sin(x._v));
    public static ThirdPartyFloat SinPi(ThirdPartyFloat x) => Of(double.SinPi(x._v));
    public static ThirdPartyFloat Tan(ThirdPartyFloat x) => Of(double.Tan(x._v));
    public static ThirdPartyFloat TanPi(ThirdPartyFloat x) => Of(double.TanPi(x._v));

    public static (ThirdPartyFloat Sin, ThirdPartyFloat Cos) SinCos(ThirdPartyFloat x)
    { var (s, c) = double.SinCos(x._v); return (Of(s), Of(c)); }

    public static (ThirdPartyFloat SinPi, ThirdPartyFloat CosPi) SinCosPi(ThirdPartyFloat x)
    { var (s, c) = double.SinCosPi(x._v); return (Of(s), Of(c)); }

    // -- operators -----------------------------------------------------------------

    public static ThirdPartyFloat operator +(ThirdPartyFloat value) => value;
    public static ThirdPartyFloat operator +(ThirdPartyFloat left, ThirdPartyFloat right) => Of(left._v + right._v);
    public static ThirdPartyFloat operator -(ThirdPartyFloat value) => Of(-value._v);
    public static ThirdPartyFloat operator -(ThirdPartyFloat left, ThirdPartyFloat right) => Of(left._v - right._v);
    public static ThirdPartyFloat operator ++(ThirdPartyFloat value) => Of(value._v + 1);
    public static ThirdPartyFloat operator --(ThirdPartyFloat value) => Of(value._v - 1);
    public static ThirdPartyFloat operator *(ThirdPartyFloat left, ThirdPartyFloat right) => Of(left._v * right._v);
    public static ThirdPartyFloat operator /(ThirdPartyFloat left, ThirdPartyFloat right) => Of(left._v / right._v);
    public static ThirdPartyFloat operator %(ThirdPartyFloat left, ThirdPartyFloat right) => Of(left._v % right._v);
    public static bool operator ==(ThirdPartyFloat left, ThirdPartyFloat right) => left._v == right._v;
    public static bool operator !=(ThirdPartyFloat left, ThirdPartyFloat right) => left._v != right._v;
    public static bool operator <(ThirdPartyFloat left, ThirdPartyFloat right) => left._v < right._v;
    public static bool operator >(ThirdPartyFloat left, ThirdPartyFloat right) => left._v > right._v;
    public static bool operator <=(ThirdPartyFloat left, ThirdPartyFloat right) => left._v <= right._v;
    public static bool operator >=(ThirdPartyFloat left, ThirdPartyFloat right) => left._v >= right._v;
}

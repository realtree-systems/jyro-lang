using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RealTree.Jyro;
using Xunit;

namespace JyroHostSurface;

// JYRO-REQ-1930: what a program needs, what a library declares, and JSON from bytes.
public class HostConveniencesTests
{
    private const string TwoCalls = "Output.a = Trim(\" x \")\nOutput.b = ToUpper(Trim(\"y\"))";

    private static CompiledProgram Compile(string source) =>
        new JyroCompiler().WithSource(source).WithStdlib().CompileOrThrow();

    private static byte[] Artifact(string source) =>
        new JyroCompiler().WithSource(source).WithStdlib().CompileToArtifact().GetValueOrThrow();

    // -- CompiledProgram.RequiredFunctions -------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1930 RequiredFunctions of Trim and ToUpper(Trim) holds exactly Trim and ToUpper")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void RequiredFunctionsHoldsEachNameOnce()
    {
        IReadOnlyList<string> required = Compile(TwoCalls).RequiredFunctions;
        Assert.Equal(2, required.Count);
        Assert.Equal(new[] { "Trim", "ToUpper" }.OrderBy(n => n), required.OrderBy(n => n));
    }

    [Fact(DisplayName = "JYRO-REQ-1930 RequiredFunctions equals RequiredFunctions of JyroArtifactInfo.Read of the artifact, in order")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void RequiredFunctionsEqualsTheArtifactInfoList()
    {
        var bytes = Artifact(TwoCalls);
        var info = JyroArtifactInfo.Read(bytes).GetValueOrThrow();
        Assert.Equal(2, info.RequiredFunctions.Count);
        Assert.Equal(info.RequiredFunctions, Compile(TwoCalls).RequiredFunctions);
    }

    [Fact(DisplayName = "JYRO-REQ-1930 a program loaded from an artifact has the RequiredFunctions of JyroArtifactInfo.Read")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void ALoadedProgramEqualsTheArtifactInfoList()
    {
        var bytes = Artifact(TwoCalls);
        var info = JyroArtifactInfo.Read(bytes).GetValueOrThrow();
        var loaded = new JyroArtifactLoader(bytes).WithStdlib().Load().GetValueOrThrow();
        Assert.Equal(info.RequiredFunctions, loaded.RequiredFunctions);
        Assert.Equal(info.RequiredFunctions.OrderBy(n => n), new[] { "ToUpper", "Trim" });
    }

    [Fact(DisplayName = "JYRO-REQ-1930 a function called twice is listed once, and a program that calls none lists none")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void ANameIsListedOnce()
    {
        var twice = Compile("Output.a = Trim(\" x \")\nOutput.b = Trim(\" y \")");
        Assert.Equal(new[] { "Trim" }, twice.RequiredFunctions);
        Assert.Empty(Compile("Output.x = 1").RequiredFunctions);
    }

    [Fact(DisplayName = "JYRO-REQ-1930 a host function the program calls is listed")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void AHostFunctionIsListed()
    {
        const string source = "Output.a = ReverseString(\"ab\")\nOutput.b = Trim(\" y \")";
        var program = new JyroCompiler()
            .WithSource(source)
            .WithStdlib()
            .AddFunction(new ReverseStringFunction())
            .CompileOrThrow();
        Assert.Equal(new[] { "ReverseString", "Trim" }, program.RequiredFunctions.OrderBy(n => n));
        var bytes = new JyroCompiler()
            .WithSource(source)
            .WithStdlib()
            .AddFunction(new ReverseStringFunction())
            .CompileToArtifact().GetValueOrThrow();
        Assert.Equal(JyroArtifactInfo.Read(bytes).GetValueOrThrow().RequiredFunctions, program.RequiredFunctions);
    }

    // -- JyroLibrary.DeclaredNames ----------------------------------------------

    [Fact(DisplayName = "JYRO-REQ-1930 DeclaredNames of a library with func f and union U is f then U")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void DeclaredNamesFollowTheSourceOrder()
    {
        IReadOnlyList<string> names = JyroLibrary.Parse("func f() return 1 end\nunion U A end").GetValueOrThrow().DeclaredNames;
        Assert.Equal(new[] { "f", "U" }, names);
    }

    [Fact(DisplayName = "JYRO-REQ-1930 DeclaredNames is in the order of the source text whichever kind comes first")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void DeclaredNamesFollowTheSourceOrderUnionFirst()
    {
        var library = JyroLibrary.Parse("union U A end\nfunc g() return 2 end\nfunc a() return 3 end").GetValueOrThrow();
        Assert.Equal(new[] { "U", "g", "a" }, library.DeclaredNames);
        Assert.Equal(3, library.DeclarationCount);
    }

    // -- JyroValue.FromJson(ReadOnlySpan<byte>) ---------------------------------

    [Fact(DisplayName = "JYRO-REQ-1930 FromJson of the bytes [1,2] gives the array [1, 2]")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void FromJsonBytesGivesTheArray()
    {
        var value = JyroValue.FromJson("[1,2]"u8);
        var array = Assert.IsType<JyroArray>(value);
        Assert.Equal(2, array.Count);
        Assert.Equal("[1,2]", value.ToJson());
    }

    [Theory(DisplayName = "JYRO-REQ-1930 FromJson of bytes gives the value FromJson of the text gives")]
    [Trait("requirement", "JYRO-REQ-1930")]
    [InlineData("{\"a\":[1,2,{\"b\":null}],\"c\":\"\\u00e9\"}")]
    [InlineData("\"caf\u00e9\"")]
    [InlineData("-0.5e3")]
    [InlineData("true")]
    [InlineData("null")]
    [InlineData("  [ ]  ")]
    public void FromJsonBytesAgreesWithTheTextForValidJson(string json)
    {
        var fromText = JyroValue.FromJson(json);
        var fromBytes = JyroValue.FromJson(Encoding.UTF8.GetBytes(json).AsSpan());
        Assert.Equal(fromText.ToJson(), fromBytes.ToJson());
        Assert.Equal(fromText.GetType(), fromBytes.GetType());
    }

    [Theory(DisplayName = "JYRO-REQ-1930 FromJson of bytes throws the exception FromJson of the text throws")]
    [Trait("requirement", "JYRO-REQ-1930")]
    [InlineData("{bad")]
    [InlineData("[1,")]
    [InlineData("1 2")]
    public void FromJsonBytesThrowsWhatTheTextThrows(string json)
    {
        var fromText = Assert.ThrowsAny<JyroException>(() => JyroValue.FromJson(json));
        var fromBytes = Assert.ThrowsAny<JyroException>(() => JyroValue.FromJson(Encoding.UTF8.GetBytes(json).AsSpan()));
        Assert.Equal(fromText.GetType(), fromBytes.GetType());
        Assert.Equal(fromText.Message, fromBytes.Message);
    }

    [Fact(DisplayName = "JYRO-REQ-1930 FromJson of bytes nested past the nesting limit throws what the text throws")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void FromJsonBytesThrowsForNestingPastTheLimit()
    {
        var json = new string('[', 100000) + new string(']', 100000);
        var fromText = Assert.ThrowsAny<JyroException>(() => JyroValue.FromJson(json));
        var fromBytes = Assert.ThrowsAny<JyroException>(() => JyroValue.FromJson(Encoding.UTF8.GetBytes(json).AsSpan()));
        Assert.Equal(fromText.GetType(), fromBytes.GetType());
        Assert.Equal(fromText.Message, fromBytes.Message);
    }

    [Fact(DisplayName = "JYRO-REQ-1930 FromJson of bytes that are not UTF-8 throws a JyroException and does not give a value")]
    [Trait("requirement", "JYRO-REQ-1930")]
    public void FromJsonBytesThrowsForInvalidUtf8()
    {
        var bytes = new byte[] { (byte)'"', 0xC3, 0x28, (byte)'"' };
        var thrown = Assert.ThrowsAny<JyroException>(() => JyroValue.FromJson(bytes.AsSpan()));
        Assert.False(string.IsNullOrWhiteSpace(thrown.Message));
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;
using RealTree.Jyro;

namespace JyroHostSurface;

// An effect or parameter type outside its enumeration is refused where the host gives it (JYRO-REQ-2176).
// Written from the specification alone.
public class EnumerationBoundaryTests
{
    private static readonly FunctionEffect[] BadEffects = { (FunctionEffect)99, (FunctionEffect)(-1), (FunctionEffect)4 };
    private static readonly ParameterType[] BadTypes = { (ParameterType)8, (ParameterType)99, (ParameterType)(-1) };

    private sealed class EffectFunction : JyroFunctionBase
    {
        private readonly FunctionEffect _effect;

        public EffectFunction(string name, FunctionEffect effect)
            : base(name, JyroFunctionSignature.Create(ParameterType.Number))
        {
            _effect = effect;
        }

        public override FunctionEffect Effect => _effect;

        public override JyroValue Execute(IReadOnlyList<JyroValue> arguments, JyroExecutionContext context) => new JyroNumber(1);
    }

    private static readonly IJyroFunction Pure = new EffectFunction("PureOne", FunctionEffect.Pure);
    private static readonly IJyroFunction Random = new EffectFunction("RandomOne", FunctionEffect.NonDeterministic);

    private static string[] Args(DiagnosticMessage d) =>
        d.Args.Select(a => Convert.ToString(a, System.Globalization.CultureInfo.InvariantCulture)!).ToArray();

    private static JyroCompiler PureCompiler(string source) =>
        new JyroCompiler().WithStdlib().AddFunction(Pure).AddFunction(Random).WithSource(source).WithMaxEffect(FunctionEffect.Pure);

    private static void AssertPureMaximumStillHolds(JyroCompiler compiler, string randomSource)
    {
        var result = compiler.Compile();
        Assert.False(result.IsSuccess);
        var d = Assert.Single(result.Messages, m => m.Code == MessageCode.FunctionEffectExceedsMax);
        Assert.Contains("Pure", Args(d).Append(d.Message));
    }

    [Fact(DisplayName = "JYRO-REQ-2176 JyroCompiler.WithMaxEffect refuses an effect outside the four with ArgumentOutOfRangeException naming maxEffect")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void CompilerWithMaxEffectRefusesAnEffectOutsideTheFour()
    {
        foreach (var bad in BadEffects)
        {
            var thrown = Assert.Throws<ArgumentOutOfRangeException>(() => new JyroCompiler().WithMaxEffect(bad));
            Assert.Equal("maxEffect", thrown.ParamName);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 JyroCompiler.WithLibraryMaxEffect refuses an effect outside the four with ArgumentOutOfRangeException naming maxEffect")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void CompilerWithLibraryMaxEffectRefusesAnEffectOutsideTheFour()
    {
        foreach (var bad in BadEffects)
        {
            var thrown = Assert.Throws<ArgumentOutOfRangeException>(() => new JyroCompiler().WithLibraryMaxEffect(bad));
            Assert.Equal("maxEffect", thrown.ParamName);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 JyroArtifactLoader.WithMaxEffect refuses an effect outside the four with ArgumentOutOfRangeException naming maxEffect")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void LoaderWithMaxEffectRefusesAnEffectOutsideTheFour()
    {
        var written = new JyroCompiler().WithStdlib().AddFunction(Pure).WithSource("Output.v = PureOne()").CompileToArtifact();
        Assert.True(written.TryGetValue(out var bytes), "the source did not compile to an artifact");
        foreach (var bad in BadEffects)
        {
            var thrown = Assert.Throws<ArgumentOutOfRangeException>(() => new JyroArtifactLoader(bytes).WithMaxEffect(bad));
            Assert.Equal("maxEffect", thrown.ParamName);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 each of the four effects is still accepted by the three setters")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void TheFourEffectsAreStillAccepted()
    {
        var written = new JyroCompiler().WithStdlib().AddFunction(Pure).WithSource("Output.v = PureOne()").CompileToArtifact();
        Assert.True(written.TryGetValue(out var bytes));
        foreach (var effect in new[] { FunctionEffect.Pure, FunctionEffect.NonDeterministic, FunctionEffect.Blocking, FunctionEffect.Mutating })
        {
            new JyroCompiler().WithMaxEffect(effect);
            new JyroCompiler().WithLibraryMaxEffect(effect);
            new JyroArtifactLoader(bytes).WithMaxEffect(effect);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 a refused WithMaxEffect leaves the earlier maximum Pure in force for the compile")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void ARefusedCompilerSetterLeavesTheEarlierMaximum()
    {
        foreach (var bad in BadEffects)
        {
            var compiler = PureCompiler("Output.v = RandomOne()");
            Assert.Throws<ArgumentOutOfRangeException>(() => compiler.WithMaxEffect(bad));
            AssertPureMaximumStillHolds(compiler, "Output.v = RandomOne()");

            var pureOnly = PureCompiler("Output.v = PureOne()");
            Assert.Throws<ArgumentOutOfRangeException>(() => pureOnly.WithMaxEffect(bad));
            Assert.True(pureOnly.Compile().IsSuccess);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 a refused WithMaxEffect on a compiler with no maximum set leaves the default in force")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void ARefusedSetterLeavesTheDefault()
    {
        var untouched = new JyroCompiler().WithStdlib().AddFunction(Random).WithSource("Output.v = RandomOne()").Compile();

        foreach (var bad in BadEffects)
        {
            var compiler = new JyroCompiler().WithStdlib().AddFunction(Random).WithSource("Output.v = RandomOne()");
            Assert.Throws<ArgumentOutOfRangeException>(() => compiler.WithMaxEffect(bad));
            var result = compiler.Compile();
            Assert.Equal(untouched.IsSuccess, result.IsSuccess);
            Assert.Equal(untouched.Messages.Select(m => m.Code), result.Messages.Select(m => m.Code));
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 a refused WithMaxEffect on the loader leaves the earlier maximum Pure in force for the load")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void ARefusedLoaderSetterLeavesTheEarlierMaximum()
    {
        var written = new JyroCompiler().WithStdlib().AddFunction(Random).WithSource("Output.v = RandomOne()").CompileToArtifact();
        Assert.True(written.TryGetValue(out var randomBytes), "the source did not compile to an artifact");
        var pureWritten = new JyroCompiler().WithStdlib().AddFunction(Pure).WithSource("Output.v = PureOne()").CompileToArtifact();
        Assert.True(pureWritten.TryGetValue(out var pureBytes), "the source did not compile to an artifact");

        foreach (var bad in BadEffects)
        {
            var loader = new JyroArtifactLoader(randomBytes).WithStdlib().AddFunction(Random).WithMaxEffect(FunctionEffect.Pure);
            Assert.Throws<ArgumentOutOfRangeException>(() => loader.WithMaxEffect(bad));
            var refused = loader.Load();
            Assert.False(refused.IsSuccess);
            Assert.Contains(refused.Messages, m => m.Code == MessageCode.ArtifactEffectExceedsMax);

            var pureLoader = new JyroArtifactLoader(pureBytes).WithStdlib().AddFunction(Pure).WithMaxEffect(FunctionEffect.Pure);
            Assert.Throws<ArgumentOutOfRangeException>(() => pureLoader.WithMaxEffect(bad));
            Assert.True(pureLoader.Load().IsSuccess);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 Parameter.Required and Parameter.Optional refuse a parameter type outside the eight, naming type")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void ParameterFactoriesRefuseATypeOutsideTheEight()
    {
        foreach (var bad in BadTypes)
        {
            Parameter? made = null;
            var required = Assert.Throws<ArgumentOutOfRangeException>(() => made = Parameter.Required("x", bad));
            Assert.Equal("type", required.ParamName);
            Assert.Null(made);

            var optional = Assert.Throws<ArgumentOutOfRangeException>(() => made = Parameter.Optional("x", bad));
            Assert.Equal("type", optional.ParamName);
            Assert.Null(made);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 both JyroFunctionSignature.Create overloads refuse a return type outside the eight, naming returnType")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void SignatureCreateRefusesAReturnTypeOutsideTheEight()
    {
        foreach (var bad in BadTypes)
        {
            JyroFunctionSignature? made = null;
            var first = Assert.Throws<ArgumentOutOfRangeException>(
                () => made = JyroFunctionSignature.Create(bad, Parameter.Required("x", ParameterType.String)));
            Assert.Equal("returnType", first.ParamName);
            Assert.Null(made);

            var second = Assert.Throws<ArgumentOutOfRangeException>(
                () => made = JyroFunctionSignature.Create(new[] { Parameter.Required("x", ParameterType.String) }, bad));
            Assert.Equal("returnType", second.ParamName);
            Assert.Null(made);
        }
    }

    [Fact(DisplayName = "JYRO-REQ-2176 all eight ParameterType members are still accepted by the factories")]
    [Trait("requirement", "JYRO-REQ-2176")]
    public void TheEightTypesAreStillAccepted()
    {
        for (var value = 0; value < 8; value++)
        {
            var type = (ParameterType)value;
            Assert.Equal(type, Parameter.Required("x", type).Type);
            Assert.Equal(type, Parameter.Optional("x", type).Type);
            Assert.Equal(type, JyroFunctionSignature.Create(type, Parameter.Required("x", ParameterType.Any)).ReturnType);
            Assert.Equal(type, JyroFunctionSignature.Create(new[] { Parameter.Required("x", ParameterType.Any) }, type).ReturnType);
        }
    }
}

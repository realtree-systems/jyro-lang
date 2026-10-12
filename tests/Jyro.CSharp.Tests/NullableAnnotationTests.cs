using System;
using RealTree.Jyro;
using Xunit;

// JYRO-REQ-1931: a C# host compiled with <Nullable>enable</Nullable> sees the annotations of the API.
//
// This project compiles with nullable enabled and warnings as errors, so each line below that the annotations
// would make a warning fails the build of the project: `msg.Message.Length` is warning CS8602 if Message were
// annotated as nullable, and the assignment of ExitMessage to a non-nullable string would be CS8600 if it were
// annotated as not null. The reflection tests of the same requirement (NullabilityInfoContext over the whole
// public surface) are in tests/Jyro.Spec.Tests.P100_170/S160_09_ApiShape.fs. A line that must give a warning
// (`result.ExitMessage.Length`) cannot be written in a project that treats warnings as errors.

namespace JyroHostSurface;

public class NullableAnnotationTests
{
    [Fact(DisplayName = "JYRO-REQ-1931 a host with nullable enabled reads ExitMessage as nullable, DiagnosticMessage.Message as not null and HostState as nullable")]
    [Trait("requirement", "JYRO-REQ-1931")]
    public void TheCompilerSeesTheAnnotations()
    {
        CompiledProgram program = new JyroCompiler().WithSource("exit \"done\"").CompileOrThrow();
        JyroExecutionResult result = new JyroRunner(program).WithInput(new JyroObject()).Execute();

        string? exit = result.ExitMessage;
        Assert.Equal("done", exit);
        Assert.Equal(4, exit?.Length);

        var failed = new JyroCompiler().WithSource("var s: string").Compile();
        DiagnosticMessage msg = failed.Messages[0];
        int length = msg.Message.Length; // CS8602 here if Message were annotated as nullable
        Assert.True(length > 0);

        object? state = new JyroExecutionContext().HostState;
        Assert.Null(state);
    }

    [Fact(DisplayName = "JYRO-REQ-1931 a parameter that accepts null is annotated as nullable: WithHostState takes null")]
    [Trait("requirement", "JYRO-REQ-1931")]
    public void WithHostStateAcceptsNull()
    {
        CompiledProgram program = new JyroCompiler().WithSource("Output.x = 1").CompileOrThrow();
        var runner = new JyroRunner(program).WithInput(new JyroObject()).WithHostState(null); // no CS8625 warning
        Assert.True(runner.Execute().IsSuccess);
    }
}

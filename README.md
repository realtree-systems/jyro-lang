# Jyro

A secure, sandboxed scripting language for .NET. The compiler converts scripts to LINQ expression trees. The runtime executes them with resource limits that are on by default. It is safe to run untrusted code in your application.

## Packages

| Package | Description | README |
|---------|-------------|--------|
| [RealTree.Jyro](https://www.nuget.org/packages/RealTree.Jyro) | Core language - parser, compiler, standard library, and host API | [Docs](src/Jyro.Api/README.md) |

## Quick start

```csharp
CompiledProgram program = new JyroCompiler()
    .WithStdlib()
    .WithSource("Output.greeting = Input.name & ' is ' & Input.age & ' years old'")
    .CompileOrThrow();

var result = new JyroRunner(program)
    .WithInputJson("""{ "name": "Alice", "age": 30 }""")
    .Execute();

if (result.TryGetValue(out var output))
    Console.WriteLine(output.ToJson()); // {"greeting":"Alice is 30 years old"}
```

Compile once, run many times: a program holds the script, its host functions, and its capability ceiling; a runner holds one run's input, limits, and cancellation.

## Features

- **Sandboxed execution** - a script has no access to the file system, the network, or reflection.
- **Resource limits** - time, statement count, loop iterations, and call depth. The limits are on by default and are configurable.
- **LINQ expression tree compilation** - a script compiles to a native .NET delegate for fast execution.
- **JSON-native** - input and output are JSON values. Scripts operate on objects, arrays, strings, numbers, and booleans.
- **Exact numbers** - integer IDs and money round-trip without loss (backed by `decimal`). IEEE-754 doubles apply only to out-of-range and transcendental values.
- **Standard library** - string, array, math, date and time, query, and schema functions.
- **Custom host functions** - extend the language with application functions through `JyroFunctionBase`.
- **Compilation caching** - compile one time, then execute many times with different data.
- **Diagnostic messages** - structured errors with stable codes and line and column positions.
- **One public namespace** - the full host API is in `RealTree.Jyro`, with no F# types on the surface.

## Project structure

```
src/
  Jyro.Core/        Value types, diagnostics, function base classes
  Jyro.Parser/      FParsec-based parser (internal)
  Jyro.Compiler/    LINQ expression tree compiler (internal)
  Jyro.Stdlib/      Standard library functions
  Jyro.Api/         JyroCompiler / JyroArtifactLoader / JyroRunner entry points (RealTree.Jyro NuGet package)
```

## Building

```bash
dotnet build
```

## License

MIT

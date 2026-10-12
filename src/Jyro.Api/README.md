# Jyro

A secure, sandboxed scripting language. It runs untrusted scripts inside .NET applications.

## Installation

The `RealTree.Jyro` NuGet package contains the parser, the compiler, the standard library, and the host API. Install it with the .NET CLI:

```
dotnet add package RealTree.Jyro
```

All public types are in the `RealTree.Jyro` namespace.

## Overview

You embed Jyro in a .NET application to run scripts that you do not trust. The compiler converts each script to a LINQ expression tree. The runtime then evaluates the script against input data that the host supplies. A compilation from source has the stages Parse, Validate, Link and Compile, and a run of the compiled program has Execute. Three classes control those stages: `JyroCompiler` produces a program from source, `JyroArtifactLoader` produces one from a compiled `.jyrx` artifact, and `JyroRunner` executes one.

The public surface carries nullable reference type annotations: a member that can give `null`, such as `JyroExecutionResult.ExitMessage`, is annotated as nullable, and every other one is not.

## Build and run scripts

`JyroCompiler` turns a script into a `CompiledProgram`. `JyroRunner` executes that program against input data. Both have a fluent interface: each `With` and `Add` method changes the builder it is called on and returns that same builder. A minimal example follows:

```csharp
CompiledProgram program = new JyroCompiler()
    .WithStdlib()
    .WithSource("Output.name = Input.name")
    .CompileOrThrow();

var result = new JyroRunner(program)
    .WithInputJson("""{ "name": "Alice" }""")
    .Execute();

if (result.TryGetValue(out var output))
    Console.WriteLine(output.ToJson()); // {"name":"Alice"}
```

The two are separate types because the two halves are configured by different concerns and often in different places. A script is compiled once; it is run many times, in a different process or on a different machine, under whatever policy that run needs. **Limits, cancellation, and input belong to the run. The script, its libraries, its host functions, and its capability ceiling belong to the compilation.** Neither a `CompiledProgram` nor a `.jyrx` artifact carries a resource limit, so the runner that executes decides them; a runner configured with nothing runs at `JyroExecutionOptions.Default`, not at anything the compiler was told. Keeping the members on separate types is what makes that visible - there is no limit method on `JyroCompiler` to look as though it traveled.

For a script you compile and run in one breath, `JyroScript.ExecuteScript` does both in one call (see [F# helper functions](#f-helper-functions)).

### Supply the script source

`JyroCompiler` compiles source only: use `WithSource` to supply Jyro source code as a string. A script that you compiled before, in the `.jyrx` binary format, is loaded by `JyroArtifactLoader` instead (see [Binary precompilation](#binary-precompilation)). A load runs the Deserialize, Validate, Link and Compile stages: an artifact is a file, and the header records a hash of the source rather than a signature over the AST, so the validator runs on what is loaded.

A `null` argument to a method or constructor of `JyroCompiler`, `JyroArtifactLoader` or `JyroRunner` throws `ArgumentNullException` naming the parameter, and leaves the builder as it was. Two members give `null` a meaning instead: `JyroRunner.WithInput`, where it is the Jyro `null`, and `JyroRunner.WithHostState`, where it is no state.

### Supply the input data

Use one of the `WithInput` or `WithInputJson` overloads on `JyroRunner` to supply input data. The `WithInput(JyroValue)` overload accepts a Jyro value that you constructed. The `WithInput(object)` overload accepts a .NET object and converts it by its shape:

- A `bool` or a `string` becomes its Jyro equivalent.
- A value of any .NET number type becomes a Jyro number. A number type is one that implements `IBinaryInteger<T>` or `IFloatingPointIeee754<T>`, so `BigInteger`, `Int128`, `UInt128`, `nint`, `nuint` and `NFloat` are numbers too, and so is a type of your own that implements one of them for a `T` it can be cast to; a value is read as the most derived such `T`, so a subclass of your number class is a number of that class. A type that implements the interface only for a `T` it cannot be cast to is not a number. A `decimal` is exact, and so is an integer within the range of `decimal`; a larger integer is its nearest `double`, and one whose nearest `double` is an infinity is refused as an infinity is. A `double`, a `float`, a `Half` or an `NFloat` is inexact, and so is any other floating-point type of radix 2, rounded to its nearest `double`. A floating-point type of radix 10 is exact when its value is exactly a `decimal`, and otherwise its nearest `double`; a floating-point type of any other radix is not a number. A number type is read only through the members its interface requires: an integer through `IsNegative`, `GetByteCount` and `TryWriteLittleEndian`, a floating-point type through `Radix`, `IsNaN`, `IsInfinity`, `IsNegative`, `ILogB` and its significand writer, as significand × radix^(`ILogB` − (digits of the significand − 1)). An exception one of those members throws passes out of the conversion unchanged.
- A `JsonElement` or a `System.Text.Json.Nodes.JsonNode` (a `JsonObject`, a `JsonArray` or a `JsonValue`) becomes the JSON it holds, read as `WithInputJson` reads it. A `JsonValue` that you made from a .NET value with `JsonValue.Create` is that value, converted by these rules exactly as if you had passed it directly: `JsonValue.Create(0.1)` is the inexact `double` 0.1, and a `JsonValue` of a list or an object has the shape the list or object has here. The conversion never calls `JsonSerializer`. To have System.Text.Json's contract decide the shape, with its attributes and converters, serialize the value first with `JsonSerializer.SerializeToElement` and pass the element.
- A Newtonsoft.Json `JToken` becomes its JSON: a `JObject` a Jyro object of its properties in their order, a `JArray` a Jyro array, and a `JValue` its `Value`, converted by these rules, or `null` for a null or undefined token. Any other token, such as a `JRaw`, a comment or a `JConstructor`, becomes its text.
- A LINQ to XML node (any `System.Xml.Linq.XObject`, such as an `XElement` or an `XDocument`) becomes its XML text, without formatting, as `ToString(SaveOptions.DisableFormatting)` writes it. A `System.Xml.XmlNode` becomes its `OuterXml`.
- A dictionary becomes a Jyro object of its entries, in the order the dictionary enumerates them. A dictionary is a value whose type implements `IDictionary<string, T>` or `IReadOnlyDictionary<string, T>` for any `T`, such as `Dictionary<string, double>`. A value whose type implements these interfaces only for a key type that is not `string`, such as `Dictionary<int, double>` or `Dictionary<object, double>`, is not a dictionary: it becomes a Jyro array of its entries, even when it is empty and even when every key it holds is a string. A non-generic `IDictionary` that implements none of the generic interfaces, such as a `Hashtable`, is a dictionary when every key is a string, and otherwise an array of its entries. Each entry of such an array is a Jyro object with exactly two properties, `Key` then `Value`.
- Any other enumerable becomes a Jyro array of its elements, whatever its element type.
- A `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly`, `TimeSpan`, `Guid`, `Uri`, `Version`, `char` or `StringBuilder` becomes its text, and an enum value its name. So does a `CultureInfo`.
- A delegate, reflection metadata (a `System.Type`, any other `MemberInfo`, an `Assembly`, a `Module` or a `ParameterInfo`) and a task (a `Task`, any `Task<T>`, a `ValueTask` or a `ValueTask<T>`) have no Jyro value and are refused, with `HostValueNotConvertible`. No member of the refused value is read, so a pending task never blocks the conversion.
- Every other object, a class, a struct, a C# record, an anonymous object or a `ValueTuple`, becomes a Jyro object of its public readable properties followed by its public fields, each group in the order .NET lists it. A property whose getter throws is left out, and the value is still an object. An object with no public readable property and no public field becomes its text.

The conversion calls every public getter it reads. A getter that does work does it: an unforced `Lazy<T>` is forced. A type whose value is not in its public members, or whose getters do work, such as a `Regex`, a `Stream`, a `Lazy<T>` or an entity with navigation properties, is yours to convert before you pass it. A thrown exception is refused too, because its `TargetSite` is reflection metadata.

Each element and property value is converted the same way. A `NaN` or an infinity anywhere in the value is not a Jyro number, and a refused value anywhere in it has no Jyro value: the run fails before its first statement with `HostNumberNaN`, `HostNumberInfinite` or `HostValueNotConvertible`, decided by the first such number or value in a depth-first walk of the value. `JyroValue.FromObject`, the one member that converts a .NET value of any type, throws a `JyroRuntimeException` with the same code instead. The `WithInputJson(string)` overload parses JSON text. The `WithInputJson(Stream)` overload reads UTF-8 JSON from a stream: it reads the stream once, to its end, at the first `Execute()` after the call, and the value it read is the `Input` of that run and of every later one until you set another input. Both read strict JSON (RFC 8259): a trailing comma, a comment, empty text, or a second value after the first is not JSON. Text that is not valid JSON does not raise from these methods. The fault is recorded and reported by `Execute()` as an `InvalidJson` diagnostic, and the script does not run, so a host that checks `IsSuccess` needs no `try` around the chain. A `null` argument is a different case and is refused immediately with `ArgumentNullException`.

In the script, the identifier `Input` gives read-only access to the input data. The script writes its results to `Output`. `Output` starts as an empty object, and the host receives it after execution. A script can also assign `Output` directly, for example `Output = [1, 2, 3]`, to return a value that is not an object. Use `WithOutput(value)` on the runner to start `Output` from a value that you supply instead of the empty object. Each run starts from a copy of that value, taken when `Execute()` is called: no run changes the value you gave, and you read the script's edits from the result. Each writable container of the value is copied once, so a container that the value reaches by more than one path becomes one copy reached by the same paths. A container of the value that is read-only - one reached from an `Input` - is kept as it is and stays read-only. The copy is the first value of `Output`, so the maximum value size does not apply to it.

A runner can be executed more than once. Each `Execute()` starts from what was set on the runner, not from what an earlier run left.

### Pass per-run state to your functions

`JyroRunner.WithHostState(state)` sets an object that every host function of every later run reads as `ctx.HostState`, the same object by reference. The last call holds, and `null`, or no call, is no state. The engine never reads, changes or keeps it, so a multi-tenant host can give each runner of one shared program its own tenant:

```csharp
var result = new JyroRunner(program)
    .WithHostState(tenant)
    .WithInputJson(json)
    .Execute();
```

### Compile and execute

`JyroCompiler` has three terminal methods:

- `Compile` compiles the script. It returns a `JyroResult<CompiledProgram>`. You can keep the program and execute it many times.
- `CompileOrThrow` does the same, but it throws a `JyroException` on failure. The exception carries the full diagnostic list in its `Messages` property.
- `CompileToArtifact` compiles the source and writes it in the `.jyrx` binary format. It returns a `JyroResult<byte[]>`. You can store the bytes and load them later with `JyroArtifactLoader`.

`JyroArtifactLoader` has one, `Load`, which returns a `JyroResult<CompiledProgram>`.

`JyroRunner` has two:

- `Execute` evaluates the program against the input data. It returns a `JyroExecutionResult`, the `JyroResult<JyroValue>` of a run.
- `ExecuteOrThrow` does the same, but it throws a `JyroException` on failure.

A parse, validation, or link error therefore arrives from the compiler, and a runtime error from the runner. Both report through `JyroResult<T>` and the same exception type, so a host can pick one style and use it for the whole pipeline. `JyroResult<T>.GetValueOrThrow()` turns any result into that style: it returns the value of a result whose `IsSuccess` is `true`, and throws a `JyroException` holding its `Messages` otherwise.

### Use the results

Each pipeline method returns a `JyroResult<T>`. The result has these parts:

- `IsSuccess` is `true` if, and only if, the diagnostic list contains no message with `Error` severity.
- `TryGetValue(out T value)` gives the value when one is present. A value is always present when `IsSuccess` is `true`. A value can also be present on failure: a script that writes to `Output` and then fails keeps its partial results, and the host can read them. A value is absent only when execution did not start.
- `HasValue` asks the same question without an out-parameter.
- `Messages` is an `IReadOnlyList<DiagnosticMessage>`. Each message carries a `MessageCode`, a `Severity` (`Info`, `Warning`, or `Error`), a `Stage`, a `Message` string, and a `Location`.
- `GetValueOrThrow()` gives the value, or throws a `JyroException` holding `Messages`.

`JyroException` is the one exception that reports a failed pipeline. Its `Messages` hold at least one diagnostic of severity `Error`, and its `Stage` and `Location` are those of the first of them. `JyroRuntimeException`, the exception of a single code, derives from it, so one `catch (JyroException)` handles every exception that carries a Jyro code. A host constructs neither.

There is deliberately no bare `Value` property. One that returned `default` when absent would be read by accident instead of asked about, and one typed as an option would put an F# type on the surface of the type every host touches on every call. `TryGetValue` is the door in both languages - F# supports out-parameters directly, and F# callers can also pattern-match the tuple form, `match result.TryGetValue() with`. Nothing in this package's public API requires `FSharpOption`, `FSharpList`, or any other F# type.

```csharp
var result = runner.Execute();

if (!result.IsSuccess)
{
    foreach (var msg in result.Messages)
        Console.Error.WriteLine(msg.Message);
}

// Partial output survives a failed run.
if (result.TryGetValue(out var partial))
    Console.WriteLine(partial.ToJson());
```

A host can also construct results directly, for example for an early error return:

```csharp
var success = JyroResult<JyroValue>.Success(myValue);

var failure = JyroResult<JyroValue>.Failure(
    DiagnosticMessage.Error(MessageCode.RuntimeError, MessageStage.Execute, "LoadOrder()"));
```

A success holds no error: `Success(value, messages)` throws `ArgumentException` when `messages` holds a diagnostic of severity `Error`. Make such a result with `Failure`.

Convert a `JyroValue` back to .NET types with `ToStringValue()`, `ToDouble()`, `ToInt32()`, `ToInt64()`, or `ToObjectValue()`. Each is a strict conversion and throws `InvalidCast` for a value of another type. `IsTruthy()` gives the truthiness the language uses for `if` and `not`, and never throws. The `ToJson()` method serializes the value to a JSON string. Downcast compound values with `AsObject()` or `AsArray()` to reach the `JyroObject` and `JyroArray` members. A `JyroObject` is an `IReadOnlyDictionary<string, JyroValue>` of its entries, with `Count`, `ContainsKey`, `Keys`, `Values` and `TryGetValue`. A `JyroArray` is an `IReadOnlyList<JyroValue>` of its elements, with `Count`.

`JyroNumber.Value` is the `double` nearest the number: for an exact number it loses precision whenever the decimal has no `double` of the same value, and `DecimalValue` gives the exact value. `JyroNumber.Equals` compares mathematical values, so the exact `1` equals the inexact `1.0`, and the exact `9007199254740993` does not equal the inexact `9007199254740992.0`; a number can be the key of a `Dictionary`. The language operator `==` compares a mixed pair as `double`s and can disagree with `Equals`: `EqualsValue` gives its answer.

### Find out what a value is

Every `JyroValue` reports its kind through `ValueType`, a `JyroValueType` enum. It is a real enum, so it works in a `switch` and with `Enum.Parse`:

```csharp
switch (value.ValueType)
{
    case JyroValueType.Null:     return "nothing";
    case JyroValueType.Number:   return value.ToDouble().ToString();
    case JyroValueType.String:   return value.ToStringValue();
    case JyroValueType.Boolean:  return value.AsBoolean().Value.ToString();
    case JyroValueType.Array:    return $"{value.AsArray().Count} items";
    case JyroValueType.Object:   return $"{value.AsObject().Count} properties";
    case JyroValueType.Function: return "a lambda";
    default:                     return "unknown";
}
```

`IsNull` is a shorthand for the first case. Prefer `ValueType` over a type test when you handle several kinds, because it is exhaustive and the compiler can tell you when you have missed one.

`JyroValueType` describes a value you are holding. `ParameterType`, used in function signatures, describes what a function *declares* - a different question, which is why the two are separate types and why `ParameterType` has an `Any` member that `JyroValueType` has no use for. The other members of `ParameterType` have the names and the values of `JyroValueType`, so you can cast between the two.

### Construct Jyro values

Construct values with the concrete subclasses when you build input data or return values from custom functions:

```csharp
// Primitives
var str = new JyroString("hello");
var count = new JyroNumber(42);      // int: exact
var price = new JyroNumber(19.99m);  // decimal: exact
var ratio = new JyroNumber(0.5);     // double: inexact
var flag = JyroBoolean.True;         // or JyroBoolean.False
var nil = JyroNull.Instance;

// Objects
var obj = new JyroObject
{
    { "name", new JyroString("Alice") },
    { "age", new JyroNumber(30) },
    { "active", JyroBoolean.True },
};

// Arrays
var arr = new JyroArray { new JyroString("one"), new JyroNumber(2), obj };
```

`JyroObject.Add` adds a property at the end of the object and throws `ArgumentException` when the object already has the key; `SetProperty` and the indexer (`obj["key"] = value`) add or replace. `JyroObject` also has `ContainsKey`, `Keys`, `Values`, `TryGetValue`, `Remove`, `Clear` and `Count`. `JyroArray` has indexer access, `TryGetItem`, `Insert`, `RemoveAt`, `Clear`, and `Count`. A `JyroFunction` is made by its constructor, and `ParameterCount` gives its number of parameters.

For bulk conversion, use `JyroValue.FromJson(jsonString)` to parse a JSON string, or `JyroValue.FromObject(value)` to convert a .NET object by the rules above. `WithInput(object)` on the runner converts a .NET object the same way.

### Numbers

Jyro has one numeric type. The type keeps a value exact when it can. A number is exact when it is `m / 10^s`, with `m` an integer of magnitude at most 2^96 - 1 (79228162514264337593543950335) and `s` from 0 to 28, and the value needs no rounding to be held that way. Every other number is an IEEE-754 `double`, the one nearest to its value: `1e-29`, a fraction of 29 significant digits, a value above 2^96 - 1, and the results of transcendental functions (`SquareRoot`, `Log`, and others). The rule is: *a number is exact unless it cannot be held exactly without rounding.*

Every number is finite. There is no `NaN` and no infinity: an operation whose result would be one fails with `NumberDomainError` or `NumberOverflow`.

- **Large integer IDs are safe.** The JSON value `{"id": 175928847299117063}` reads, stores, and writes back without change. There is no silent corruption above 2^53.
- **Money is exact.** `0.1 + 0.2` equals `0.3`. `Sum`, `Average`, `Round`, `Min`, and `Max` over exact values stay exact.
- Arithmetic between two exact values stays exact when the exact form can hold the result without rounding. When it cannot, as for `1 / 3`, the operation is done in `double` and the result is inexact. An operation that mixes an exact value with a `double`, or a transcendental function, gives a `double`.

The `JyroNumber` constructors follow the same rule. The `int`, `long`, and `decimal` constructors give exact values. The `double` constructor gives an inexact value, and throws a `JyroRuntimeException` with the code `HostNumberNaN` for `NaN` or `HostNumberInfinite` for an infinity. JSON numbers are exact when the exact form holds them without rounding, and otherwise the nearest `double`. `ToObjectValue()` returns a boxed `long` for an exact integer in `long` range, a `decimal` for other exact values, and a `double` for inexact values. The `.jyrx` binary format carries the exact representation in format version 1. The reader rejects every other version with `ArtifactVersionUnsupported`, the version 9 of development builds included: recompile such an artifact from source.

## Diagnostics and error management

Each diagnostic carries a unique `MessageCode` in the `JMXXXX` format. The first digit identifies the pipeline stage:

| Band | Subsystem |
|---|---|
| `1xxx` | lexer |
| `2xxx` | parser |
| `3xxx` | validator |
| `4xxx` | linker |
| `5xxx` | runtime |
| `6xxx` | compiler |
| `7xxx` | artifact (`.jyrx` read and write) |

In each band, the hundreds digit groups related errors. For example, `52xx` codes are arithmetic errors and `53xx` codes are index and property errors.

### The stage of a diagnostic

The band of a code says where the code was defined. `DiagnosticMessage.Stage` says which stage produced the diagnostic, as a `MessageStage`: `Deserialize`, `Parse`, `Validate`, `Link`, `Compile` or `Execute`. The two differ where a code is raised outside its band's stage. An unpaired surrogate in a string literal is a parse error with a runtime-band code. A refusal to read a `.jyrx` artifact has the stage `Deserialize`, and a refusal to write one the stage `Compile`. `CompilationTimeLimitExceeded` has the stage that was running when the limit was reached. `NoSourceProvided` has the stage `Validate`. Every diagnostic of a run has the stage `Execute`. A diagnostic of a library script has the stage the same diagnostic has in a script.

### The exit message

A script that ends with `exit` and a value succeeds, and the text of the value is the exit message. Read it from `JyroExecutionResult.ExitMessage`, the result of a run; no other result has one. It is `null` when the script ended any other way: `exit` with no value, the end of the last statement, `fail`, or an error. `exit ""` gives the empty string, not `null`. The exit message is not a diagnostic, so it is not in `Messages`.

### Format diagnostics

The `DiagnosticFormatter` type gives a standard formatter. Its output has a fixed shape that tools can parse:

```csharp
foreach (var msg in result.Messages)
{
    Console.Error.WriteLine(DiagnosticFormatter.FormatMessage(msg));
}
// [JM5200] Ln 12, Col 5: Division by zero
// [JM3100] Ln 3, Col 1: Undeclared variable 'foo'
```

`DiagnosticMessage.ToString()` gives the same text as `FormatMessage`.

### Structured diagnostics

Use `DiagnosticFormatter.ToStructured` to convert a diagnostic to a `StructuredDiagnostic`. It carries the code as text, the numeric code, the severity, the stage, the subsystem name, the message, the args and the location, each as a read-only property. The shape is suitable for JSON serialization and for UI display.

```csharp
var structured = DiagnosticFormatter.ToStructured(msg);
// structured.Code        = "JM5200"
// structured.NumericCode = 5200
// structured.Severity    = MessageSeverity.Error
// structured.Stage       = MessageStage.Execute
// structured.Subsystem   = "runtime"
// structured.Message     = "Division by zero"
// structured.Location    = SourceLocation?
```

`Location` is a `SourceLocation?`. It has no value when the diagnostic has no source position. A real position is thus always different from an absent one. `Subsystem` is the name of the band of the code, from the table above.

### Localization

Each diagnostic carries an `Args` array with the raw values of its message. The `Message` string is derived from the code's template and these args; it is not stored. A host can supply translated templates. Implement `IMessageTemplateProvider` and return a template for each code that you translate. The `DiagnosticFormatter.FormatLocalized` method applies your template to the diagnostic's args. It falls back to the default English message when you return `false`.

```csharp
public sealed class FrenchTemplates : IMessageTemplateProvider
{
    public bool TryGetTemplate(MessageCode code, out string template)
    {
        switch (code)
        {
            case MessageCode.DivisionByZero:
                template = "Division par zéro";
                return true;
            case MessageCode.InvalidType:
                template = "Impossible de donner une valeur de type {0} à une cible de type {1}";
                return true;
            default:
                template = null!;
                return false;
        }
    }
}

var formatted = DiagnosticFormatter.FormatLocalized(new FrenchTemplates(), msg);
```

Use `MessageTemplates.TryGet(code, out template)` to read the default English template for a code.

### Construct diagnostics

A host can construct diagnostic messages, for example for a custom error result. `DiagnosticMessage` has no public constructor: use its static factory methods `Error`, `Warning` and `Info`. Each takes the code and the stage, then optionally a location, then the args:

```csharp
// An error. The args fill the code's message template.
var error = DiagnosticMessage.Error(
    MessageCode.RuntimeError, MessageStage.Execute, "LoadOrder()");

// An error with a source location: line 10, column 5, 12 code units long.
var located = DiagnosticMessage.Error(
    MessageCode.UndefinedFunction,
    MessageStage.Link,
    new SourceLocation(10, 5, 12),
    "LoadOrder");

// Warning and Info have the same two overloads.
var warning = DiagnosticMessage.Warning(
    MessageCode.UndefinedFunction, MessageStage.Link, "LegacyTotal");
```

Each `DiagnosticMessage` has `Code` (`MessageCode`), `Severity` (`MessageSeverity`), `Stage` (`MessageStage`), `Message` (`string`, derived from the code and the args), `Args` (`object[]`), and `Location` (`SourceLocation?` with `Line`, `Column`, and `Length`). No location is a `SourceLocation?` with no value. Two diagnostics are `Equals` when their code, severity, stage and location are equal and their args have the same invariant strings in the same order.

The arguments are the values that change from one occurrence of a code to the next: a name, a type, a limit, the token the parser stopped at. They are never sentences, so a translated template can place them. The one exception is the text of a `fail` value, which is the one argument of its `ScriptFailure`. Where there is nothing to name, a diagnostic has no argument: a source that ends before the parser can read further gives `UnexpectedEndOfSource` with an empty `Args`. An exception that escapes a host function is reported as `RuntimeError` whose one argument is the function's name with `()`, such as `LoadOrder()`. Neither the exception's type nor its message is an argument. `UnknownCompilerError`, `InternalStageFailure`, `ArtifactUnreadable` and `ArtifactUnwritable` have no argument.

The location of a diagnostic is the place in the source that it is about. `Line` and `Column` are 1-based and count UTF-16 code units, a `CRLF` counted as one line end. `Length` is the number of code units of the place:

| Diagnostic | Place |
| --- | --- |
| A syntax error | The token at which the parser could read no further. At the end of the source it is the empty range after the last code unit, with `Length` 0. |
| A lexical error, such as `UnterminatedString` | The token. A string literal with no closing quote runs to the end of the source. |
| A validation or link error | The node that the rule rejects, such as the `var` declaration of `TypedVariableNeedsInitializer` or the call of `TooFewArguments`. |
| A runtime error | The node whose evaluation raised it: the call, for an error of a standard library, host or script function or a variant; the operator or the access; otherwise the statement. |

A node's text runs from its first token to its last, with no white space or comment around it; the parentheses of a grouped expression belong to the node that holds it. A result gives every parse error: each lexical error of the source, and the syntax error at which the parser stops, in the order of their positions.

## Resource limits

Jyro runs untrusted code, so the runtime limits what a script can consume. **The limits are on by default.** When you do not configure options, the runner installs a limiter with `JyroExecutionOptions.Default`. To remove the time, statement, and loop limits, you must set `JyroExecutionOptions.Unlimited` yourself.

**Every limit below belongs to the run.** They live on `JyroRunner`, they are read when `Execute` builds its limiter, and nothing records them in a `CompiledProgram` or a `.jyrx` artifact. A host that caches a program and executes it under two policies configures two runners, not two programs.

Configure the limits with `WithExecutionOptions`, or set single limits with the convenience methods:

| Limit | Default | `JyroRunner` method |
|---|---|---|
| Maximum execution time | 5 seconds | `WithMaxExecutionTime(TimeSpan)` |
| Maximum statement count | 10,000 | `WithMaxStatements(int)` |
| Maximum cumulative loop iterations | 1,000 | `WithMaxLoopIterations(int)` |
| Maximum call stack depth | 64 | `WithMaxCallDepth(int)` |

When a script exceeds a limit, the run stops. The result is a failure with the applicable diagnostic code, for example `StatementLimitExceeded` or `ExecutionTimeLimitExceeded`.

A count limit is 0, which is a limit not set and takes its default, or more. `WithMaxStatements`, `WithMaxLoopIterations` and `WithMaxCallDepth` throw `ArgumentOutOfRangeException` for a negative value, naming their parameter, and `WithExecutionOptions` throws it, naming `options`, when one of its count limits is negative. After the exception the runner keeps every limit it had.

Compilation has a time limit of 5 seconds, the default execution time limit, unless you set another with `JyroCompiler.WithMaxCompilationTime(TimeSpan)` or `JyroArtifactLoader.WithMaxCompilationTime(TimeSpan)`. So compiling an untrusted source is bounded by default, as running it is: the limits above are checked at statement, loop, and call boundaries during execution, and compilation reaches none of those, so a hostile source could otherwise spin the parser for seconds before anything runs. A correct script compiles in milliseconds; a host that compiles very large trusted scripts under heavy load can set a longer limit, or `TimeSpan.MaxValue` for none. The limit covers every stage before Execute: Parse, Validate, Link and Compile from source, and Deserialize, Validate, Link and Compile when you load a `.jyrx` artifact. It does not cover Execute, and the execution time limit does not cover compilation. A compilation that reaches its limit fails with `CompilationTimeLimitExceeded`, whose stage is the stage that was running when the limit was reached. `JyroLibrary.Parse(source)` is bounded by the same default, and `JyroLibrary.Parse(source, maxCompilationTime)` by the limit you give it.

On .NET on single-threaded browser WebAssembly (Blazor WebAssembly) the compilation time limit, the default included, is not enforced. That runtime cannot start a second thread, so compilation runs on the caller's thread and nothing can keep time while it runs. See [Threading and concurrency](#threading-and-concurrency).

The regular expression functions stop an evaluation that takes longer than the regex timeout, with `RegexTimeout`. The timeout is 1 second. Set another for every later `Execute()` of a runner with `JyroRunner.WithMaxRegexTime(TimeSpan)`, or with the `MaxRegexTime` of `JyroExecutionOptions`: a duration greater than zero and at most 24 hours. `WithMaxRegexTime` throws `ArgumentOutOfRangeException` naming `time` for `TimeSpan.Zero`, a negative duration (`Timeout.InfiniteTimeSpan` included), or more than 24 hours, and the runner keeps the regex timeout it had. In `JyroExecutionOptions`, a `MaxRegexTime` of `TimeSpan.Zero` is not set and leaves the timeout as it was; of the two ways, the one called last sets it.

A compilation or execution time limit is `TimeSpan.MaxValue`, which means no limit, or a duration greater than zero and at most 24 hours. `WithMaxCompilationTime`, `WithMaxExecutionTime` and `WithExecutionOptions` throw `ArgumentOutOfRangeException` for any other value: `TimeSpan.Zero`, a negative duration (`Timeout.InfiniteTimeSpan` included), or more than 24 hours. After the exception, the compiler, the loader or the runner keeps the limits it had, and `WithExecutionOptions` applies none of the options it was given.

```csharp
var result = new JyroRunner(program)
    .WithInputJson(json)
    .WithMaxExecutionTime(TimeSpan.FromSeconds(2))
    .WithMaxLoopIterations(500)
    .Execute();
```

### Direct construction

`JyroExecutionOptions` has a parameterless constructor and settable properties. You can thus construct it with an object initializer, or bind it from configuration for dependency injection, the regex timeout `MaxRegexTime` with the other limits. It also has a constructor that takes the four resource limits:

```csharp
var options = new JyroExecutionOptions
{
    MaxExecutionTime = TimeSpan.FromSeconds(10),
    MaxStatements = 20_000,
    MaxLoopIterations = 2_000,
    MaxCallDepth = 128,
    MaxRegexTime = TimeSpan.FromSeconds(2),
};

var result = new JyroRunner(program)
    .WithInputJson(json)
    .WithExecutionOptions(options)
    .Execute();
```

A count limit you leave unset keeps its default from the table above. The runner treats a `MaxStatements`, `MaxLoopIterations`, or `MaxCallDepth` of zero as unset. An initializer that sets only `MaxExecutionTime` therefore still runs with 10,000 statements, 1,000 loop iterations, and a call depth of 64. A configuration section that names only some of the count limits works the same way. `MaxExecutionTime` has no such default: `WithExecutionOptions` refuses a `MaxExecutionTime` of `TimeSpan.Zero` with `ArgumentOutOfRangeException`, so always set it.

Two prepared instances are available. `JyroExecutionOptions.Default` holds the values in the table above, and the regex timeout of 1 second. `JyroExecutionOptions.Unlimited` removes the time, statement, and loop limits. `MaxCallDepth` stays at `JyroExecutionOptions.MaxSafeCallDepth` (96) even under `Unlimited`. Recursion without a cap would let a script overflow the host stack and escape the sandbox. The limiter clamps values above `MaxSafeCallDepth` without an error.

### Call depth

`MaxSafeCallDepth` assumes a thread stack of 1 MB. A script runs on the thread that calls `Execute`, and 1 MB is the stack size of a Windows thread-pool or ASP.NET request thread. On a 1 MB stack, a script at the ceiling cannot overflow the stack: not with a recursive call inside nested blocks, and not when the call depth limit trips or the deepest call raises an error, runs `fail`, or runs `exit`. If you execute scripts on a thread with a smaller stack, set `MaxCallDepth` lower.

### Maximum nesting depth of a value

This section is about values, whose maximum nesting depth is **128**. It is a different bound from the maximum source nesting depth of **256**, which bounds a script's source text and a `.jyrx` artifact (see [Maximum source nesting depth](#maximum-source-nesting-depth)).

Every walk of a value's structure stops at one maximum value nesting depth of **128**. The walks include equality, the string form, `ToJson` and `FromJson`, conversion to and from host values, reading JSON input, and every standard library function that walks an argument. The depth of a container is the number of arrays and objects on the path from the root of the walk to it, with the root counted as one. The width of a value does not add to its depth. A walk that would go past 128 raises `ValueNestingLimitExceeded` before it goes deeper, so a walk of a cyclic value raises it too. A cyclic `Input` is still accepted, because making `Input` read-only is not such a walk. The maximum is fixed: a host cannot raise, lower, or remove it. It is sized together with `MaxSafeCallDepth`, because a walk's frames sit on top of the script's call frames on one stack.

JSON text is read from its start, and the first problem met decides the outcome. Text that opens an array or object at depth 129 fails with `ValueNestingLimitExceeded` at that bracket, whatever follows it. Text that stops being JSON before that point fails with `InvalidJson`. So 129 `[` characters and nothing else give `ValueNestingLimitExceeded`, and `[x` followed by 129 `[` characters gives `InvalidJson`. The script's `FromJson` follows the same order: it returns `null` where a host member reports `InvalidJson`. For text that is not JSON, `JyroValue.FromJson`, for text or for a stream, and `FromJsonAsync` throw a `JyroRuntimeException` with the code `InvalidJson`, `WithInputJson` leaves it to `Execute` to report, and `ExecuteScriptWithJson` returns a failed result.

A host API member that walks a value outside a run fails through its own channel. `JyroValue.FromJson`, `FromJsonAsync`, `FromObject`, `ToJson`, `ToObjectValue` and `ToStringValue` throw a `JyroRuntimeException` with the code `ValueNestingLimitExceeded`. `JyroRunner.WithInput` and `WithInputJson` do not throw for input that is too deep or a cyclic host object: `Execute` reports it as a failed result, as it reports malformed JSON. `ExecuteScript` and `ExecuteScriptWithJson` return a failed result.

`JyroValue.FromJsonAsync(stream, cancellationToken)` completes with the value `FromJson` gives for the stream, and ends as canceled, with no value, when the token is canceled before the stream has been read.

### Maximum source nesting depth

This section is about source text and artifacts, whose maximum nesting depth is **256**. It is a different bound from the maximum value nesting depth of **128**, which bounds the walks of a value (see [Maximum nesting depth of a value](#maximum-nesting-depth-of-a-value)).

A source text nests at most **256** levels deep. The maximum applies to every source text that you compile: the script, and each library script on its own. The same maximum bounds a `.jyrx` artifact, so every program that compiles from source can be written as an artifact and loaded again.

The depth is counted on the parsed program. A top-level statement has depth 1. Each expression and each statement is one level deeper than the expression, statement or parenthesized expression that holds it, and a parenthesized expression is one level too. So each operand of an operator, each argument of a call, each element of an array literal, each property value of an object literal, and each statement in the body of an `if`, a loop, a `switch` or `match` case or a function is one level deeper than what holds it. A chain of left-associative operators nests on its left: in `Output.s = 1 + 1 + 1` the first `1` is at depth 4, and with `n` operators `+` it is at depth `n + 2`, so a chain of 255 or more operators in one expression is too deep and must be split over statements. Width does not add to the depth: the number of statements in a body, of elements, entries or arguments side by side, and of `elseif` branches. Comments and white space do not add to it either.

A source text deeper than 256 fails at the parse stage with one `SourceNestingLimitExceeded` diagnostic (2002), whose one argument is the maximum, 256. Whether a source text is refused depends on its depth alone. The place of the diagnostic is where the parser stops: the parser keeps a reading count as it reads, which is the depth except that an operand of a binary operator (`??` among them), the condition of a ternary, the operand of `is`, and the target of a member access, an index, a call or a postfix `++` or `--` count as the node that holds them, because the parser learns what holds them only after it has read them. The place is the first token of the first expression, statement or parenthesized expression whose reading count is above 256. When no reading count is above 256, the parser reads the whole text, and the place is the first token of the first one whose depth is above 256. So `Output.s = 1 + 1 + ... + 1` with 255 operators fails at the first `1`, and `Output.s = 1 + ` followed by 100,000 `(`, `1`, 100,000 `)` and 300 times ` + 1` fails at the 256th `(`. No later stage runs. The parser checks the reading count before it recurses and never recurses past the maximum, so no source text, however deep and whether or not it is well formed, can exhaust a stack or stop the host process. A program at or under the maximum compiles and runs on a host thread of 1 MB or more. On a single-threaded runtime such as browser WebAssembly, compilation runs on the caller's thread, and this guarantee holds where that thread's stack is at least 1 MB.

The maximum is fixed: a host cannot raise, lower, or remove it. An artifact deeper than 256 is refused when it is loaded, with `ArtifactUnreadable`.

### Cancellation

The limiter checks the execution-time limit and the host's cancellation token at every statement and every loop iteration, so a canceled token stops a script written in Jyro at the next of either. Call boundaries check call depth only - time is checked at every statement, which bounds how far a run can drift between calls.

Two things sit outside those checkpoints. A custom function that does blocking I/O, for example an HTTP request, does not reach one while it waits; and a standard-library function iterating a large collection has no statement boundaries of its own, so it calls `ctx.CheckExecutionLimits()` per element instead. `JyroRunner` accepts a cooperative cancellation token through `WithCancellationToken`, which covers all three:

```csharp
using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
var result = new JyroRunner(program)
    .WithInputJson(json)
    .WithMaxExecutionTime(TimeSpan.FromSeconds(5))
    .WithCancellationToken(cts.Token)
    .Execute();
```

The token governs execution only. Compilation is bounded by its time limit instead (`WithMaxCompilationTime`), because the parser has no cooperative checkpoint at which a token could be observed.

The limiter creates a `CancellationTokenSource` that cancels when the run reaches `MaxExecutionTime`. When you also supply an external token, the limiter links the two. Either token can then stop the run. A custom function receives the combined token through `ctx.CancellationToken`. Pass that token to your blocking operations. When the token fires, the operations stop at once. They do not wait for the next checkpoint.

The token fires on time even when your process's thread pool is busy. The time limit is not a `CancelAfter` timer, whose callback would wait for a free thread-pool thread. The engine starts its own background threads, none of them from the thread pool: one deadline thread per process, started on first use, which notices each run's deadline, and canceller threads, which cancel the run's token at that deadline. When you cancel your own token, the run's token is canceled at once, on the thread that called `Cancel()`.

Callbacks registered on `ctx.CancellationToken` run on the thread that canceled it: at the time limit, an engine canceller thread. A callback that blocks there holds that one thread of your process until it returns, but it delays no other run's time limit: the engine starts another canceller when none is free. Keep callbacks short all the same.

`Sleep` waits on the token's `WaitHandle` in the same way, so it too ends on time when the pool is busy. Like any blocking call, it holds the thread that called `Execute()` for the whole pause.

On the browser (Blazor WebAssembly) runtime there is only one thread, so `Sleep` cannot wait on a handle. It reads its own clock until the pause is over instead, checking the time limit and the token between readings. It still pauses for the full time it was given, but it blocks the page's thread for the whole pause, and the page does not respond until it returns. The time limit still ends it. A cancel can come only from a host function the script called before the `Sleep`, because no other code can run during the pause.

The two causes are reported apart. A run you canceled fails with `CanceledByHost`; a run that spent its time budget fails with `ExecutionTimeLimitExceeded`, naming the budget. The engine tests the budget first and reads your token rather than the linked one, so a timeout is never reported as a cancellation. A custom function that blocks on `ctx.CancellationToken` and catches the resulting `OperationCanceledException` can tell them apart the same way, through `ctx.HostCancellationRequested`.

## Pipeline statistics

Pass a `JyroPipelineStats` instance through `WithStats` to collect timing data for each stage. It is read-only telemetry: only the pipeline writes it. `JyroCompiler`, `JyroArtifactLoader` and `JyroRunner` accept one, and each records the stages its path runs:

| Path | Records | Reads as zero |
| --- | --- | --- |
| `JyroCompiler.Compile()` and `JyroCompiler.CompileToArtifact()` | `ParseTime`, `ValidateTime`, `LinkTime` and `CompileTime`; writing the artifact is part of the Compile stage | `DeserializeTime` |
| `JyroArtifactLoader.Load()` | `DeserializeTime`, `ValidateTime`, `LinkTime` and `CompileTime` | `ParseTime` |
| `JyroRunner.Execute()` | `ExecuteTime` and the counters of the run: `StatementCount`, `LoopIterations`, `PeakCallDepth` (the greatest call depth the run reached) and `LimiterElapsedTime` (the time the limiter measured) | |

A stage that a path did not run reads as zero. Hand the same instance to both halves to get the whole pipeline in one place. `TotalTime` is the sum of the six stage times.

```csharp
var stats = new JyroPipelineStats();

CompiledProgram program = new JyroCompiler()
    .WithStdlib()
    .WithSource(script)
    .WithStats(stats)
    .CompileOrThrow();

var result = new JyroRunner(program)
    .WithInputJson(json)
    .WithStats(stats)
    .Execute();

Console.WriteLine($"Parse: {stats.ParseTime.TotalMilliseconds}ms");
Console.WriteLine($"Execute: {stats.ExecuteTime.TotalMilliseconds}ms");
Console.WriteLine($"Statements: {stats.StatementCount}");
Console.WriteLine($"Total: {stats.TotalTime.TotalMilliseconds}ms");
```

## Standard library

The standard library gives the script functions for strings, arrays, math, dates and times, schema validation, queries, and general utilities. The standard library is opt-in. Call `WithStdlib()` on the compiler to include it. Do not call it when you want a minimal or fully custom function set.

Use `StdlibRegistry.GetAll()` to enumerate the standard library as an `IReadOnlyList<IJyroFunction>`. Use `StdlibRegistry.TryGetFunction(name, out func)` to find one function by name: it gives the same object for a name on every call in a process. `AddFunctions` on the compiler and on the loader enumerates the sequence it is given once.

## Custom functions

A host can add its own functions to the script's function set. Derive from `JyroFunctionBase`, and read the arguments with the extension methods of `JyroArguments`, for example `args.GetString(0)`, `args.GetNumber(1)`, `args.GetArray(0)` and `args.GetObject(0)`.

A custom function must define four things:

- A **name**. The script calls the function by this identifier.
- A **signature**. It declares the parameter names, the parameter types, optionality, and the return type.
- An **effect**. The `Effect` property is abstract, so you must override it. Use `Pure` for a function with no observable effect, `NonDeterministic` for clock or random reads, `Blocking` for a function that can pause, and `Mutating` for a function that reads or writes shared state. `FunctionEffect` is an enumeration in that order, so `FunctionEffect.Blocking < FunctionEffect.Mutating`.
- An **implementation**. Override `Execute`.

**A function with side effects weakens the sandbox.** A script can reach nothing outside its input and output except through the functions you give it. A function that reads a file, calls a service, writes to a database or changes shared state hands that power to every script that can call it, and the sandbox is then only as strong as what the function allows. The weakening is in proportion to what the function does: a function that reads one configuration value gives away little, and a function that runs a query the script writes gives away a great deal. Give such a function the narrowest inputs it can work with, check its arguments, declare its true effect, and use the capability ceilings below to keep it from scripts that do not need it.

### Capability ceilings

The effect of a function is what a host caps to decide what scripts may reach. `JyroCompiler.WithMaxEffect(effect)` sets the strongest effect a user script's call sites may have; it defaults to `Mutating`, which restricts nothing. Lower it to reject a script that calls beyond the ceiling, with `FunctionEffectExceedsMax` at link time - `Pure` for a script that must be a pure transformation, `NonDeterministic` to allow the clock and random but not blocking or mutation.

`WithLibraryMaxEffect(effect)` sets the same ceiling for calls made from *library* bodies, and defaults to `NonDeterministic`. The two are separate because a library is host-supplied code and a user script is not, so they are trusted differently: a host can allow its own library to read the clock while forbidding the script that calls it from doing anything else.

The compiler has these two compilation contexts and no other, each with its own maximum:

| Context | Call sites it covers | Set with | Default |
| --- | --- | --- | --- |
| Script | the user script's own statements and functions | `WithMaxEffect` | `Mutating` |
| Library | the bodies of library scripts added with `AddLibrarySource` or `AddLibrary` | `WithLibraryMaxEffect` | `NonDeterministic` |

The ceiling is a compile-time decision, applied when the program is linked. See [Precompilation](#precompilation) for what that means for a cached program and for a `.jyrx` artifact.

### Observing function results

`JyroRunner.WithFunctionResultObserver(Action<string, JyroValue>)` invokes a callback after every host-function call returns, with the function name and its result. It is attached to the per-run context, so it observes any program however it was compiled. Use it for tracing and audit logs; it is per-run configuration, not part of the program.

### Function signatures

`JyroFunctionSignature.Create` makes a signature from the return type and the parameters, in order. Parameter names are part of the call surface: a script can pass arguments by name.

```csharp
// One required parameter
JyroFunctionSignature.Create(ParameterType.String,
    Parameter.Required("text", ParameterType.String))

// Two required parameters and an optional one
JyroFunctionSignature.Create(ParameterType.Number,
    Parameter.Required("haystack", ParameterType.String),
    Parameter.Required("needle", ParameterType.String),
    Parameter.Optional("caseSensitive", ParameterType.Boolean))
```

The second overload, `Create(parameters, returnType)`, accepts any sequence of parameters. Either way the signature derives the argument counts from the parameters: `MinArgs` is the number of required parameters and `MaxArgs` the number of parameters. A signature cannot change once made, and it keeps its own copy of the parameters, so changing the array or the list you passed changes nothing. The name of the function is given to `JyroFunctionBase`, not to the signature.

### Implement a custom function

This example reverses a string:

```csharp
public sealed class ReverseStringFunction : JyroFunctionBase
{
    public ReverseStringFunction()
        : base("ReverseString",
               JyroFunctionSignature.Create(ParameterType.String,
                   Parameter.Required("text", ParameterType.String)))
    {
    }

    public override FunctionEffect Effect => FunctionEffect.Pure;

    public override JyroValue Execute(
        IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
    {
        var text = args.GetString(0);
        var chars = text.ToCharArray();
        Array.Reverse(chars);
        return new JyroString(new string(chars));
    }
}
```

Register custom functions on the compiler with `AddFunction` or `AddFunctions`. They are resolved at link time and baked into the program, which is why they belong to the compilation rather than the run:

```csharp
CompiledProgram program = new JyroCompiler()
    .WithStdlib()
    .WithSource("Output.result = ReverseString(Input.text)")
    .AddFunction(new ReverseStringFunction())
    .CompileOrThrow();

var result = new JyroRunner(program)
    .WithInputJson("""{ "text": "hello" }""")
    .Execute();
```

To unit test a function without the pipeline, call it through `IJyroFunction` with a bare context:

```csharp
var ctx = new JyroExecutionContext();
IJyroFunction reverse = new ReverseStringFunction();
var value = reverse.Execute(new JyroValue[] { new JyroString("abc") }, ctx);
// value.ToStringValue() == "cba"
```

Call through `IJyroFunction`, as here, and not the `Execute` override itself: only a call through the interface tells the readers which function and parameter they read for.

### Read arguments

The readers of `JyroArguments` are extension methods on the arguments, and each takes the index of the argument it reads. They follow one rule. A `null` argument gives the identity of the requested type where one exists: the empty string, the empty array, or the empty object. An argument of a different type is an error that names the expected and the actual Jyro types. Numbers and booleans have no identity, so their readers reject `null`. An index the caller left out reads as `null`, so an optional parameter needs no check of `args.Count` first.

The numeric readers differ in precision. Select the one that matches your data:

| Reader | Returns | Precision |
|---|---|---|
| `GetNumber` | `double` | **Lossy** for exact integers above 2^53 and for high-precision decimals. Use it for floating-point math. |
| `GetDecimal` | `decimal` | Exact. Use it for IDs, money, and counters. |
| `GetInt64` | `long` | Exact. Rejects fractional and out-of-range values. |
| `GetInt32` | `int` | Exact. Rejects fractional and out-of-range values. |
| `GetNonNegativeInt32` | `int` | Exact. Also rejects negative values. |

The rest of the family:

| Reader | Returns | Notes |
|---|---|---|
| `GetString` | `string` | Coerces a scalar to its written form. Refuses an array or object: a container has no textual form, only a rendering. |
| `GetBoolean` | `bool` | Refuses `null` and every other non-boolean, as the number readers do. |
| `GetArray` / `GetObject` | `JyroArray` / `JyroObject` | `null` reads as the empty container. |
| `Get<T>` | `T` | The argument as given, converted to nothing, or a refusal naming the Jyro types expected and received. `null`, and an index the caller left out, are a `T` only when `T` is `JyroValue` or `JyroNull`. |
| `GetOptional<T>` | `T` | The argument if the caller supplied one, otherwise your default. `null` counts as not supplied. An argument of the wrong type is still refused - the default covers absence, not error. |

A refusal is a runtime error of code `ArgumentTypeMismatch`. Its two arguments are the expected and the actual type, spelled as `TypeOf` spells them: `null`, `boolean`, `number`, `string`, `array`, `object` or `function`. During a run, or when you call your function through `IJyroFunction.Execute`, a diagnostic of the integer readers names your function, by its `Name`, and the parameter your signature declares at the index read. A direct call of your `Execute` override names neither.

### Report a failure your function chose

Throw an exception made by `JyroError.Host` when your function rejects
something on purpose - a value outside the range it accepts, an unavailable
resource, a business rule:

```csharp
public override JyroValue Execute(
    IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
{
    var amount = args.GetDecimal(0);
    if (amount <= 0) throw JyroError.Host("AmountNotPositive", amount);
    ...
}
```

`JyroError.Host(reason, values)` returns the exception and does not throw
it, so you throw it, from any method. The run fails with `HostFunctionError`.
Its arguments are the reason, then the values you passed, in order: here
`AmountNotPositive` and the amount. The reason is a name for the condition
that your host can switch on, not a sentence; a diagnostic's arguments are
never sentences. To report a reason with no values, throw
`JyroError.Host("ResourceUnavailable")`.

`JyroError.Runtime(code, args)` makes an exception of a code of the argument
and conversion codes the standard library raises, such as
`ArgumentTypeMismatch` or `InvalidJson`, and the run fails with that code. It
refuses, with `ArgumentOutOfRangeException` naming `code`, a code that only the
engine raises: every code below 5000 or from 6000 up, `RuntimeError`,
`CanceledByHost`, `ScriptFailure`, `HostFunctionError`, the resource limits
5900 to 5903, `ValueNestingLimitExceeded` and `ValueSizeLimitExceeded`.

To show a sentence, or a translated one, make it from the diagnostic's `Args`
by the reason, or give `HostFunctionError` a template in your
`IMessageTemplateProvider`. Without either, the default message names the
reason and the first nine values. Every value stays in `Args`, however many
you pass.

Prefer `JyroError.Host` over throwing an exception of your own. An exception
of your own that escapes your function is reported as `RuntimeError`, the catch-all the runtime uses for anything that
crashed. Its one argument is the name of your function, with `()`, and its
location is the call. It carries neither the exception's type nor its
message. A condition you chose to reject would then be indistinguishable from
a fault you did not intend.

The execution context shows your function what it may use of the run: the
cancellation signal and whether the host requested it, `CheckExecutionLimits()`,
`Input` and `Output` (read-only), the call depth and its peak, and the
`HostState` of the runner. The hooks that keep the call depth and the time
limit are internal, so a function cannot end the limits of the run it is
called in.

The execution context gives each function a `CancellationToken`. Pass `ctx.CancellationToken` to blocking operations, so the limiter and the host can stop work in flight:

```csharp
public override JyroValue Execute(
    IReadOnlyList<JyroValue> args, JyroExecutionContext ctx)
{
    var response = _httpClient.Send(request, ctx.CancellationToken);
    ...
}
```

The token fires at the time limit, or when the host cancels, without waiting for a thread-pool thread (see [Cancellation](#cancellation)). If your function waits on its own, wait on `ctx.CancellationToken.WaitHandle` with a timeout rather than blocking on a `Task`, whose continuation would need a free pool thread. When the token's handle is the one set, `ctx.CheckExecutionLimits()` raises the right error, the time limit or the host's cancellation:

```csharp
// `done` is a WaitHandle your function's work sets when it finishes.
var handles = new[] { ctx.CancellationToken.WaitHandle, done };
if (WaitHandle.WaitAny(handles, TimeSpan.FromSeconds(30)) == 0)
    ctx.CheckExecutionLimits();
```

A custom function with the same name as a standard library function fails at compile time with `HostFunctionOverridesStdlib`. There is no silent override. Rename the function. Two custom functions with the same name fail with `DuplicateFunction`. A script `func` declaration that shadows a built-in function is a different case, reported as `FunctionOverride`.

### Check a call before the script runs

Override `CheckCall` to refuse a call of your function at compile time, before
any script runs. The compiler calls it once for each call of your function in
the script and in each library, after the call has passed its own checks of
the name, the arguments and the effect. Loading a `.jyrx` artifact calls it
the same way.

```csharp
public override void CheckCall(JyroCallSite call)
{
    if (call.TryGetLiteral(0, out var table)
        && table is JyroString name
        && !_tables.Contains(name.Value))
    {
        throw JyroError.Host("TableUnknown", name.Value);
    }
}
```

`JyroCallSite` gives the function's `Name`, the `ArgumentCount`, the
`ArgumentNames` of a call with named arguments, and `TryGetLiteral`, which
gives an argument's value when it is written as a literal and returns `false`
when it is any other expression. Return to accept the call. An exception made
by `JyroError.Host` refuses it: the compilation fails with `HostLinkError`,
placed at the call, whose arguments are the reason, then the values. Each
refused call gives its own diagnostic. A function that does not derive from
`JyroFunctionBase` implements `IJyroCallCheck` to have a check.

An exception your check throws, or that your function's `Name`, `Signature`
or `Effect` throws during compilation, is reported through the result of
`Compile()`, `CompileToArtifact()` and `Load()`, not thrown.

When `Effect` throws, of any type, or returns a value that is none of `Pure`,
`NonDeterministic`, `Blocking` and `Mutating`, the maximum effect cannot be
applied to the function, and the compilation fails with
`MaxEffectNotApplicable` at stage Link, whatever the maximum is. Its arguments
are the function's name, then the maximum in force. A source gives one for
each call to the function, placed at the call; a load gives one for each such
function of the artifact's function table, in table order, with no place.

In a check, an exception made by `JyroError.Runtime` fails the compilation
with the code and arguments you give it, at stage Link. Any other exception that escapes your
check gives `UnknownCompilerError` at stage Link. That includes a
`JyroRuntimeException` thrown by the engine's own members that your check
calls, such as `InvalidCast` from `AsNumber()` on a string literal: it reports
a fault in your check, not in the script, so it does not keep its code.

### Script libraries

A library is a script that contains only `func` and `union` declarations. Use `AddLibrarySource` to parse a library on one compiler. Use `JyroLibrary.Parse` to parse a library one time and share it across many engines:

```csharp
var parsed = JyroLibrary.Parse(librarySource);
if (!parsed.TryGetValue(out var library))
    return; // read parsed.Messages

CompiledProgram program = new JyroCompiler()
    .WithStdlib()
    .WithSource("Output.r = Triple(Input.n)")
    .AddLibrary(library)
    .CompileOrThrow();

var result = new JyroRunner(program)
    .WithInputJson("""{ "n": 2 }""")
    .Execute();
```

A library that contains other statements fails at `JyroLibrary.Parse`, not later at `Compile`.

`JyroLibrary.Parse` reports the faults that a library has whatever compilation you add it to: a parse error, a statement a library may not contain, and every other check that reads only the library. Each `Compile` that the library is added to reports the faults that depend on the other source texts, with the stage, place and order that the same text added with `AddLibrarySource` gets:

- a function name that another source text declares or that a registered function has (`DuplicateFunction`, `FunctionOverride`)
- a union or variant name that another source text declares (`DuplicateUnionName`, `DuplicateVariantName`)
- a `match` with a case that names a variant the library does not declare, which may be a variant of another library or of the script (`MatchUnknownVariant`, `MatchBindingCountMismatch`, `MatchDuplicateBinding`, `MatchNotExhaustive`)
- any `match` in a library that repeats a union or variant name an earlier source text declares, whose cases may then resolve against that text's union
- every call, and the effect maximum for libraries

So a library whose `match` names a variant that no source text declares parses clean and fails at every `Compile` it is added to.

A library cannot travel through the `.jyrx` format, which encodes no library context. Configuring one and calling `CompileToArtifact` fails with `ArtifactCannotEncodeLibraries`, a refusal of the Compile stage reported once the libraries and the script have parsed, validated and linked clean, so a script with errors gets its own errors first. It refuses rather than ignores the library, because a dropped library turns into an `UndefinedFunction` error against the script later, blaming the script for the host's configuration. `JyroArtifactLoader` has no method that adds a library, so a load with one cannot be written.

## Precompilation

When the same script runs many times against different input data, compile it one time and reuse the result.

### In-memory caching

`Compile` returns a `JyroResult<CompiledProgram>`, and `CompileOrThrow` returns the program directly. Cache the program and hand it to a fresh `JyroRunner` per run. Include the custom functions at compile time; the compiler bakes them into the program, so the runner needs no function set of its own.

```csharp
// Compile one time.
CompiledProgram program = new JyroCompiler()
    .WithStdlib()
    .WithSource(script)
    .AddFunction(new MyCustomFunction())
    .CompileOrThrow();

// Execute many times with different data, and with per-run limits.
var result = new JyroRunner(program)
    .WithInput(JyroValue.FromJson(json))
    .WithMaxExecutionTime(TimeSpan.FromSeconds(2))
    .Execute();
```

This is the recommended pattern for server applications. Load and compile each script one time. Execute it per request.

One consequence is worth stating: the capability ceiling of a cached program is the ceiling of the compiler that built it. `WithMaxEffect` is checked when the program is linked, and a `CompiledProgram` records no ceiling for anyone to re-check. A host serving tenants under different capability policies therefore compiles one program per policy, rather than caching one program and varying the ceiling per run. Resource limits are the opposite - they are decided per run, so one program serves every policy.

### Binary precompilation

Use the `.jyrx` binary format when you store a compiled script on disk, send it over a network, or cache it externally. `JyroCompiler.CompileToArtifact()` writes it, and `JyroArtifactLoader` loads it. A load runs the stages Deserialize, Validate, Link and Compile, in that order, and no Parse: the artifact carries the program the parser would have produced, and the validator checks it exactly as it checks source. Link then resolves the functions the artifact names and applies the loading host's capability ceiling:

```csharp
// Compile one time.
var artifactResult = new JyroCompiler()
    .WithStdlib()
    .WithSource(script)
    .CompileToArtifact();

if (!artifactResult.TryGetValue(out var artifact))
    return; // read artifactResult.Messages

// Load, then execute many times.
CompiledProgram program = new JyroArtifactLoader(artifact)
    .WithStdlib()
    .Load()
    .GetValueOrThrow();

var result = new JyroRunner(program)
    .WithInputJson(json)
    .Execute();
```

`JyroArtifactLoader` registers functions and sets the maximum effect, the compilation time limit and the statistics with the methods of the same names that `JyroCompiler` has, and with the same outcomes. It has no method that adds a library script.

An artifact records no capability ceiling, so the loading host applies its own: `WithMaxEffect` on the loader is enforced against the functions the artifact requires, and a program that needs more fails with `ArtifactEffectExceedsMax`. The host also has to supply the functions the artifact names, which is what `JyroArtifactInfo.RequiredFunctions` below is for.

Use `JyroArtifactInfo.Read` to examine an artifact without a load of the program inside it:

```csharp
var infoResult = JyroArtifactInfo.Read(artifact);
if (infoResult.TryGetValue(out var info))
{
    // info.FormatVersion      - the .jyrx format version, 1
    // info.SourceHash         - the 32 bytes of the SHA-256 of the source the artifact was built from
    // info.RequiredFunctions  - the functions the host must supply at load time
}
```

`JyroArtifactInfo` is a read-only class with value equality: two reads of the same artifact are `Equals`, and `ToString()` gives one line with the format version, the hash as 64 lowercase hexadecimal digits, and the required functions. The read parses only the header and the function table, so it stays cheap for a pre-flight check over many artifacts. It applies the format's guards - file size, format version, function-table size - so an artifact it accepts is one the load path will accept too. It does not read the AST, so it cannot tell you the program inside is valid; for that, load it.

## Threading and concurrency

Script execution is single-threaded and synchronous. The runtime invokes the compiled delegate on the calling thread. There is no internal thread pool, no async continuation, and no parallelism in a run. Besides the caller's thread, a run depends only on the engine's own background threads, which are not pool threads: the deadline thread, shared by every run in the process, and the canceller threads, which cancel each run's token at its time limit (see [Cancellation](#cancellation)). They run no script code. A canceller runs the callbacks registered on the token it cancels, so one that blocks holds that canceller, and no other run waits for it.

Compilation, a load of an artifact, and `JyroLibrary.Parse` run on a thread of the engine's own with a 16 MiB stack, so that a source nested to the maximum depth fits whatever the caller's stack. .NET on single-threaded browser WebAssembly (Blazor WebAssembly) is the exception. That runtime cannot start a thread, so there they run on the caller's thread, with the caller's stack, and the compilation time limit, its 5-second default included, is not enforced.

Concurrency comes from the host: run many executions in parallel. The safe unit to share is the immutable `CompiledProgram`. Compile one time, cache the program, and execute it from as many threads as you need. Do not share objects that hold per-execution state:

| Type | Concurrent use |
|---|---|
| `CompiledProgram` | Safe to share and execute concurrently. |
| `JyroCompiler`, `JyroArtifactLoader` | Not thread-safe. Use one per configuration. |
| `JyroRunner` | Not thread-safe, and cheap. Use one per thread; it may be executed more than once, each run starting from its settings. |
| `JyroExecutionContext` | Per-execution. `Execute` makes a fresh one for each run. |
| `IJyroFunction` implementations | Shared across executions and invoked concurrently. See below. |

**Custom functions.** The compiler bakes each function instance into the compiled program. Every call reuses that instance, so `Execute` can run on the same instance from many threads at the same time. Keep no mutable instance state, or synchronize the state that you keep. Pass per-run state through `JyroRunner.WithHostState`, which each function reads as `ctx.HostState`, never on the function instance. Most functions are stateless and need no lock. Only a function whose `Effect` is `Mutating` needs internal synchronization.

**Input data.** `JyroValue` graphs (`JyroObject` and `JyroArray`) are mutable and not synchronized. When two concurrent runs share one input graph and a script mutates it, that is a data race. Give each execution its own input, or call `Freeze()` on the shared graph first. A frozen graph is read-only, and a mutation raises a `ValueIsReadOnly` error.

## IL trimming

The assemblies of the package are trimmable. A host that publishes with IL trimming (`<PublishTrimmed>true</PublishTrimmed>`, which Blazor WebAssembly and .NET MAUI turn on by default) needs no `TrimmerRootAssembly` entry for a Jyro assembly. It compiles and runs every script as an untrimmed host does, lambda functions such as `Map`, `Where` and `SortBy` included.

Jyro compiles scripts to LINQ expression trees, and the compiler finds the engine members that the compiled code calls through reflection. Each of those members carries a trimming annotation that keeps it, so the trimmer removes none of them, and the publish reports no trim warning from a Jyro assembly.

One conversion reads the host's own types: `JyroValue.FromObject` turns an object into a Jyro object through the public properties and fields of the object's type, which the trimmer may remove. `FromObject`, `JyroRunner.WithInput(object)` and `ExecuteScript(string, object)` therefore carry `RequiresUnreferencedCode`, so a host that trims gets warning IL2026 at its own call to one of them. Such a host keeps the members of the types it converts, as it would for any reflection-based serializer, or passes its input as JSON (`WithInputJson`, `ExecuteScriptWithJson`) or as a `JyroValue`, none of which warns.

## F# helper functions

From F#, the `JyroScript` module (which is `AutoOpen`) gives shorthand functions that compile and run in one call, with the standard library included. Each one recompiles, so use `JyroCompiler` and `JyroRunner` directly when a program is reused:

```fsharp
// One-shot execution with an object
let result = executeScript "Output.result = Input.x + 1" {| x = 10 |}

// One-shot execution with JSON
let result = executeScriptWithJson "Output.result = Input.x + 1" """{ "x": 10 }"""
```

C# hosts reach the same two as `JyroScript.ExecuteScript` and `JyroScript.ExecuteScriptWithJson`, which return a `JyroExecutionResult`. They are the only members that compile and run a script in one call; there is no extension method that runs a script.

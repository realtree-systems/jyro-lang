# Jyro.Spec.Common

Shared helpers for the `jyro-spec` verification suites (`tests/Jyro.Spec.Tests.*`).
Written from the book and the public host API alone (FLOORS 4.1.1). Verifiers
do not edit this project; a helper you need that is missing goes in your own
test project.

Open it with:

```fsharp
open Xunit
open RealTree.Jyro        // the public API: JyroValue, MessageCode, ...
open RealTree.Jyro.Spec   // these helpers
```

Every helper that "fails the test" raises `Xunit.Sdk.XunitException` with a
message that lists the diagnostics, so a failure reads as a failed assertion.

## Running a script - module `Script`

Every run is `JyroCompiler` (compile) then `JyroRunner` (execute), through the
public API. A compile failure comes back as a failed `JyroResult<JyroValue>`
carrying the compiler's own `DiagnosticMessage` objects and no value.

```fsharp
Script.run       : source: string -> JyroResult<JyroValue>               // defaults
Script.runJson   : inputJson: string -> source: string -> JyroResult<JyroValue>
Script.runInput  : input: JyroValue -> source: string -> JyroResult<JyroValue>
Script.runWith   : RunOptions -> source: string -> JyroResult<JyroValue>
Script.compile   : source: string -> JyroResult<CompiledProgram>         // compile only
Script.compileWith      : RunOptions -> source: string -> JyroResult<CompiledProgram>
Script.compileBytesWith : RunOptions -> source: string -> JyroResult<byte[]>   // CompileToArtifact
Script.runBytesWith     : RunOptions -> byte[] -> JyroResult<JyroValue>        // JyroArtifactLoader.Load, then run
Script.loadBytesWith    : RunOptions -> byte[] -> JyroResult<CompiledProgram>  // JyroArtifactLoader, Load
Script.loadBytesOrThrow : RunOptions -> byte[] -> CompiledProgram              // Load, then GetValueOrThrow
Script.loaderFor        : RunOptions -> byte[] -> JyroArtifactLoader           // not loaded; libraries are not applied
Script.compilerFor : RunOptions -> JyroCompiler                          // no source set
Script.runnerFor   : RunOptions -> CompiledProgram -> JyroRunner          // not executed
Script.limits    : (JyroExecutionOptions -> JyroExecutionOptions) -> JyroExecutionOptions
```

`RunOptions` is a record; start from `Script.defaults` and copy-and-update:

| Field | Default | Maps to |
| --- | --- | --- |
| `Stdlib: bool` | `true` | `JyroCompiler.WithStdlib()` |
| `Input: Input` | `JsonInput "{}"` | `NoInput` (no call), `JsonInput text` (`WithInputJson`), `ValueInput v` (`WithInput(JyroValue)`), `HostInput o` (`WithInput(Object)`) |
| `Output: JyroValue option` | `None` | `JyroRunner.WithOutput` |
| `Limits: JyroExecutionOptions option` | `None` | `JyroRunner.WithExecutionOptions` |
| `MaxEffect: FunctionEffect option` | `None` | `JyroCompiler.WithMaxEffect` |
| `LibraryMaxEffect: FunctionEffect option` | `None` | `JyroCompiler.WithLibraryMaxEffect` |
| `Functions: IJyroFunction list` | `[]` | `JyroCompiler.AddFunction`, in order |
| `Libraries: string list` | `[]` | `JyroCompiler.AddLibrarySource`, in order |
| `Cancellation: CancellationToken option` | `None` | `JyroRunner.WithCancellationToken` |
| `CompilationTimeout: TimeSpan option` | `None` | `JyroCompiler.WithMaxCompilationTime` (and `JyroArtifactLoader.WithMaxCompilationTime` on a load) |
| `Stats: JyroPipelineStats option` | `None` | `WithStats` on compiler and runner |
| `ConfigureCompiler` | `id` | last step on the compiler |
| `ConfigureRunner` | `id` | last step on the runner |

```fsharp
let r =
    Script.runWith
        { Script.defaults with
            Stdlib = false
            Limits = Some (Script.limits (fun o -> { o with MaxLoopIterations = 3 }))
            ConfigureRunner = fun runner -> Pending.withRegexTimeout runner (TimeSpan.FromMilliseconds 50.) }
        "var i = 0\nwhile i < 10 do i = i + 1 end"
```

`JyroExecutionOptions` is an F# record: its fields cannot be assigned from F#,
so change it with `{ o with ... }` as above.

### Reading a result

```fsharp
Script.ok          : JyroResult<'T> -> bool                 // IsSuccess
Script.value       : JyroResult<'T> -> 'T option            // a failed `fail` still has Output (140.4.3)
Script.valueOf     : JyroResult<'T> -> 'T                   // fails the test if no value
Script.output      : JyroResult<JyroValue> -> JyroValue     // asserts success, then the value
Script.outputOf    : source: string -> JyroValue            // run with defaults, then output
Script.codes       : JyroResult<'T> -> MessageCode list     // every diagnostic, every severity
Script.errorCodes  : JyroResult<'T> -> MessageCode list     // severity Error only
Script.onlyErrorMessage : JyroResult<'T> -> DiagnosticMessage // exactly one Error and not IsSuccess
Script.onlyError   : JyroResult<'T> -> MessageCode          // its code
Script.expectError : MessageCode -> JyroResult<'T> -> unit  // exactly one Error, of that code
Script.describe    : JyroResult<'T> -> string               // diagnostics, one per line
```

## Values - module `Values`

```fsharp
Values.json     : string -> JyroValue      // JyroValue.FromJson
Values.toJson   : JyroValue -> string      // JyroValue.ToJson()
Values.isExact  : JyroValue -> bool        // JyroNumber.IsExact; fails if not a number
Values.inexact  : float -> JyroValue       // JyroNumber(Double): the host-side inexact number
Values.exact    : decimal -> JyroValue     // JyroNumber(Decimal)
Values.int      : int -> JyroValue         // JyroNumber(Int32)
Values.str / Values.bool / Values.nul / Values.arr / Values.obj / Values.emptyObj ()
Values.prop     : string -> JyroValue -> JyroValue   // GetProperty
Values.asFloat / asDecimal / asString / asBool / asList  // fail if the wrong type
Values.fail     : string -> 'T             // fail the test with a message
```

`Values.inexact` does not assert the representation; assert it with
`Values.isExact` where the test depends on it.

## Host functions - module `Host`, type `RecordingFunction`

A `RecordingFunction` is a `JyroFunctionBase` that records every call (its
arguments, in order; thread-safe) and answers with a reply function.

```fsharp
Host.func      : name -> Parameter list -> returns: ParameterType -> FunctionEffect
                 -> reply: (JyroValue list -> JyroValue) -> RecordingFunction
Host.funcWithContext : ... -> reply: (JyroValue list -> JyroExecutionContext -> JyroValue) -> RecordingFunction
Host.recorder  : name -> RecordingFunction                   // Pure, 0..8 args of any type, returns null
Host.recorderWithEffect : name -> FunctionEffect -> RecordingFunction
Host.signature : name -> Parameter list -> ParameterType -> JyroFunctionSignature

fn.Calls      : JyroValue list list
fn.CallCount  : int
fn.AsFunction : IJyroFunction          // for RunOptions.Functions
```

Parameters: `Parameter.Required("n", ParameterType.Number)`,
`Parameter.Optional("x", ParameterType.Any)`. To raise a Jyro error from
the reply, throw `JyroError.Runtime(code, args)` or `JyroError.Host(reason, values)`
(in F#, `raise (JyroError.Host(reason, values))`): the reason is a name for the
condition, such as `AmountNotPositive`, not a sentence, and the values are what
changes, such as the amount. `HostFunctionError`'s arguments are the reason, then
the values. `JyroError.Host(reason)` makes the exception with no values. Both
return the exception and do not throw it (JYRO-REQ-1891).

```fsharp
let twice =
    Host.func "Twice" [ Parameter.Required("n", ParameterType.Number) ]
        ParameterType.Number FunctionEffect.Pure
        (fun args -> Values.int (2 * int (Values.asFloat args.Head)))
let r = Script.runWith { Script.defaults with Functions = [ twice.AsFunction ] } "Output.r = Twice(21)"
Assert.Equal(1, twice.CallCount)
```

## Members that do not exist yet - module `Pending` (160.7)

Never name these members directly: the project would stop compiling. Each
accessor fails the calling test when the member is missing, reporting it for
the requirement that defines the member (floors-process 4.2.5) with the line

    FLOORS-BLOCKED 160.7.x: member ... is not in this build

so that `floors verify` records the test's requirement as blocked by 160.7.x,
not as fail. When every `Req` tag of the calling test is 160.7.x itself, the
message is the plain `160.7.x member ... is not in this build`: the absence is
that requirement's own failure. Any other member a test finds missing that
another requirement defines is reported the same way:

```fsharp
Pending.notInBuild        : ref: string -> memberName: string -> 'T      // fails the test
Pending.notInBuildMessage : ref: string -> memberName: string -> string  // the message alone
```

```fsharp
Pending.exitMessage      : JyroResult<'T> -> string option    // JyroExecutionResult.ExitMessage; None for null, and for any result that is not an execution result
Pending.withRegexTimeout : JyroRunner -> TimeSpan -> JyroRunner // JyroRunner.WithMaxRegexTime; its exception reaches the caller as itself
Pending.stageOf          : DiagnosticMessage -> string        // 160.7.5 Stage, as the enum name
Pending.messageStageType : unit -> Type                       // 160.7.4 enum RealTree.Jyro.MessageStage
Pending.matchSubjectNotVariant : MessageCode                  // 160.7.6: (MessageCode)5403
Pending.requireMatchSubjectNotVariant : unit -> unit          // fails unless the member is declared
```

`Pending.withRegexTimeout` calls `WithMaxRegexTime` directly, so
`Assert.Throws<ArgumentOutOfRangeException>(fun () -> Pending.withRegexTimeout runner TimeSpan.Zero |> ignore)`
pins JYRO-REQ-1905. `Pending.matchSubjectNotVariant` is a plain number, so comparing a
code with it works on any build; a test that needs the member to be declared
calls `Pending.requireMatchSubjectNotVariant ()`.

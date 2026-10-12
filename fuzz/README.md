# Jyro.Fuzz.Source

Coverage-guided mutation fuzzing of the compile-and-run pipeline via [SharpFuzz](https://github.com/Metalnem/sharpfuzz) + [AFL++](https://github.com/AFLplusplus/AFLplusplus). Complements the FsCheck property tests in [tests/Jyro.Runtime.Tests/Properties/SandboxSecurity.fs](../tests/Jyro.Runtime.Tests/Properties/SandboxSecurity.fs): those run ~100 generated cases per property on every build; this harness runs millions of mutated inputs over hours, hunting for the inputs no one thought to write.

## What this harness tests

Bytes → `UTF8.GetString` (replacement fallback) → `JyroCompiler().WithStdlib().WithSource(...).WithMaxCompilationTime(tight).Compile()`, then `JyroRunner(program).WithExecutionOptions(tight).Execute()` when a program came back. Both budgets are set explicitly: most of what this harness finds is a parse hang, which the compile budget bounds and the execution options do not.

| Outcome | Classification |
|---|---|
| `JyroResult` success or failure with diagnostics | Happy path |
| `OperationCanceledException` from sandbox timeout | Happy path |
| Wall-clock overrun past 500 ms (2× configured budget) | **Finding (hang)** |
| Any other CLR exception | **Finding (sandbox escape)** |

Tightened sandbox limits inside the harness keep per-exec wall-clock short so AFL throughput stays high: `MaxExecutionTime=250 ms`, `MaxStatements=2000`, `MaxLoopIterations=500`, `MaxCallDepth=32`.

## Prerequisites

Native AFL and the SharpFuzz CLI. This harness targets Linux; on Windows use WSL2.

```bash
# AFL++
sudo apt install afl++          # Debian/Ubuntu

# SharpFuzz CLI (global .NET tool)
dotnet tool install --global SharpFuzz.CommandLine

# Kernel config (required by AFL on first run)
echo core | sudo tee /proc/sys/kernel/core_pattern
```

Under WSL2, `scaling_governor` is unwritable; `run-afl.sh` already exports `AFL_SKIP_CPUFREQ=1` to work around this.

## First-run verification

Before a long campaign, verify the pipeline end-to-end:

```bash
bash canary.sh
```

`canary.sh` builds the harness with `-p:DefineConstants=CANARY`, injects a deterministic crash on the byte prefix `0xDE 0xAD 0xBE`, runs AFL for 60 seconds, and asserts at least one crash was recorded in `findings/default/crashes/`. If it fails, the pipeline is miswired (instrumentation skipped, AFL misconfigured, or `CANARY` symbol not reached).

Rebuild without `CANARY` before starting a real campaign:

```bash
dotnet build -c Release Jyro.Fuzz.Source.fsproj
```

## Running a campaign

```bash
bash run-afl.sh
```

The script builds, stages seeds from `tests/plaintext/`, instruments four Jyro DLLs (`Parser`, `Compiler`, `Core`, `Stdlib`), then launches `afl-fuzz` with the token dictionary. Stop with `Ctrl+C`.

Expect 150–400 exec/s on a modern laptop in WSL2. Findings appear in `findings/default/crashes/` and `findings/default/hangs/`. Each `id:000000,...` file is a minimized input that reproduces a crash or hang.

### Plateau signal

AFL's status screen shows when the campaign has explored what it can:

- `pending_fav` = 0
- `last_new_path` > 30 minutes
- `cycles_done` > 10

At that point, fix the findings and either extend the dictionary with new tokens found during fuzzing or move on to the next fuzz target (JSON, `.jyrx` binary, regex).

## Reproducing a crash outside AFL

```bash
dotnet bin/Release/net8.0/Jyro.Fuzz.Source.dll findings/default/crashes/id:000000,sig:06,*
```

SharpFuzz's libFuzzer mode runs the harness once on the given file and exits. Useful for stepping through under a debugger.

## What happens to findings

Each confirmed finding should:

1. Be minimized with `afl-tmin` to the smallest reproducer.
2. Be added as an `[<InlineData>]` case in [tests/Jyro.Runtime.Tests/Properties/NoCrash.fs](../tests/Jyro.Runtime.Tests/Properties/NoCrash.fs) (or a new `FuzzRegressions.fs` once the list grows). This ensures every bug found stays fixed.
3. Be fixed in the runtime; the fix re-runs through the full CI suite.

## Not covered by this harness

- **JSON input** via `WithInputJson` - separate fuzzer, future iteration.
- **`.jyrx` binary format** via `JyroArtifactLoader` - highest-risk surface, separate fuzzer.
- **Regex pattern + subject pairs** - separate fuzzer isolating the `RegexTest`/`RegexMatch` surface.

See the plan at `~/.claude/plans/using-our-new-fscheck-dreamy-emerson.md` (local) for the full roadmap.

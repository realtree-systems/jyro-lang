#!/usr/bin/env bash
# Build, instrument, and launch AFL++ against the Jyro source fuzzer.
#
# Prereqs:
#   - AFL++ installed (apt install afl++ on Debian/Ubuntu)
#   - sharpfuzz CLI: dotnet tool install --global SharpFuzz.CommandLine
#   - WSL kernel: echo core | sudo tee /proc/sys/kernel/core_pattern
#
# Stop with Ctrl+C. Findings land in fuzz/findings/default/crashes/ and /hangs/.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/.." && pwd)"
OUT="$HERE/bin/Release/net8.0"

echo "==> building fuzz harness (Release)"
dotnet build -c Release "$HERE/Jyro.Fuzz.Source.fsproj"

echo "==> staging seed corpus from tests/plaintext"
mkdir -p "$HERE/seeds"
find "$REPO/tests/plaintext" -name '*.jyro' -exec cp -n {} "$HERE/seeds/" \;
echo "    $(ls -1 "$HERE/seeds" | wc -l) seed files"

echo "==> instrumenting Jyro DLLs"
bash "$HERE/instrument.sh"

# .NET 8 coverage accuracy: disable tiered compilation and R2R so the
# instrumented IL is actually what runs.
export DOTNET_TieredCompilation=0
export DOTNET_ReadyToRun=0

# AFL++ runtime flags
export AFL_SKIP_BIN_CHECK=1      # dotnet host is not a native AFL binary
export AFL_DISABLE_TRIM=1        # preserve large valid scripts early on
export AFL_SKIP_CPUFREQ=1        # WSL2 can't write scaling_governor

echo "==> launching afl-fuzz (Ctrl+C to stop)"
afl-fuzz \
    -i "$HERE/seeds" \
    -o "$HERE/findings" \
    -t 1000 \
    -m none \
    -x "$HERE/jyro.dict" \
    -- dotnet "$OUT/Jyro.Fuzz.Source.dll"

#!/usr/bin/env bash
# Pipeline verification: inject a deterministic crash into the harness,
# run AFL for 60 seconds, assert that findings/default/crashes/ is
# non-empty. Proves build + instrumentation + AFL + crash reporting work
# end-to-end before committing to a long campaign.
#
# The canary patch lives in FuzzHarness.fs behind `#if CANARY` and fires
# when the input's first three bytes are 0xDE 0xAD 0xBE.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/.." && pwd)"
OUT="$HERE/bin/Release/net8.0"

echo "==> building harness with CANARY defined"
dotnet build -c Release \
    -p:DefineConstants=CANARY \
    "$HERE/Jyro.Fuzz.Source.fsproj"

echo "==> staging seeds + a canary-triggering seed"
mkdir -p "$HERE/seeds"
find "$REPO/tests/plaintext" -name '*.jyro' -exec cp -n {} "$HERE/seeds/" \;
printf '\xDE\xAD\xBEvar x = 1' > "$HERE/seeds/_canary_trigger.bin"

echo "==> instrumenting"
bash "$HERE/instrument.sh"

rm -rf "$HERE/findings"

export DOTNET_TieredCompilation=0
export DOTNET_ReadyToRun=0
export AFL_SKIP_BIN_CHECK=1
export AFL_SKIP_CPUFREQ=1

echo "==> running afl-fuzz for 60 seconds"
timeout 60 afl-fuzz \
    -i "$HERE/seeds" \
    -o "$HERE/findings" \
    -t 1000 \
    -m none \
    -x "$HERE/jyro.dict" \
    -- dotnet "$OUT/Jyro.Fuzz.Source.dll" || true

crashes=$(find "$HERE/findings/default/crashes" -type f -name 'id:*' 2>/dev/null | wc -l)
rm -f "$HERE/seeds/_canary_trigger.bin"

if [[ $crashes -gt 0 ]]; then
    echo "==> PASS: $crashes crash(es) recorded - pipeline wired correctly"
    echo "    rebuild without CANARY before a real campaign:"
    echo "    dotnet build -c Release $HERE/Jyro.Fuzz.Source.fsproj"
    exit 0
else
    echo "==> FAIL: no crashes recorded in 60s"
    echo "    check: afl installed? DLLs instrumented? CANARY symbol reached?"
    exit 1
fi

#!/usr/bin/env bash
# Inject SharpFuzz coverage instrumentation into the four Jyro DLLs that
# carry the fuzz-reachable bug surface. Idempotent: re-running after a
# rebuild is safe - sharpfuzz skips DLLs it has already touched.
#
# Prereq: `dotnet tool install --global SharpFuzz.CommandLine`
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="$HERE/bin/Release/net8.0"

if [[ ! -d "$OUT" ]]; then
    echo "error: $OUT does not exist - run 'dotnet build -c Release' first" >&2
    exit 1
fi

cd "$OUT"

for dll in Jyro.Parser.dll Jyro.Compiler.dll Jyro.Core.dll Jyro.Stdlib.dll; do
    if [[ ! -f "$dll" ]]; then
        echo "error: $dll missing from $OUT" >&2
        exit 1
    fi
    echo "instrumenting $dll"
    sharpfuzz "$dll" || true
done

echo "instrumentation complete"

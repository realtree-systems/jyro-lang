#!/usr/bin/env bash
# One-shot post-reboot setup for fuzzing inside a fresh WSL2 Ubuntu.
#
# Run this INSIDE Ubuntu after `wsl --install` has completed and Ubuntu
# has been launched at least once (to create the UNIX user):
#
#   cd /mnt/c/GitHub/jyro
#   bash fuzz/wsl-setup.sh
#
# Idempotent: safe to re-run if any phase fails.
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
DOTNET_SDK_CHANNEL="10.0"
DOTNET_RUNTIME_CHANNEL="8.0"

echo "====================================================="
echo "  Jyro fuzz harness - WSL2 setup"
echo "  Repo: $REPO"
echo "====================================================="

# ---------------------------------------------------------------
# 1. System packages: afl++ and build prerequisites
# ---------------------------------------------------------------
echo ""
echo "==> [1/5] System packages (afl++, curl, apt-transport-https)"

sudo apt update
sudo apt install -y curl apt-transport-https ca-certificates gnupg lsb-release afl++

# ---------------------------------------------------------------
# 2. .NET SDK and runtime (dotnet-install.sh, into ~/.dotnet)
# ---------------------------------------------------------------
# The source builds with the SDK global.json names (10.0, F# 9 is the
# language floor: JYRO-ADR-209); the fuzz harness targets net8.0, so it
# runs on the .NET 8 runtime, installed beside the SDK.
echo ""
echo "==> [2/5] .NET ${DOTNET_SDK_CHANNEL} SDK and .NET ${DOTNET_RUNTIME_CHANNEL} runtime"

export DOTNET_ROOT="$HOME/.dotnet"
export PATH="$DOTNET_ROOT:$PATH"

if ! dotnet --list-sdks 2>/dev/null | grep -q "^${DOTNET_SDK_CHANNEL}\."; then
    curl -sSL https://dot.net/v1/dotnet-install.sh -o /tmp/dotnet-install.sh
    bash /tmp/dotnet-install.sh --channel "$DOTNET_SDK_CHANNEL" --install-dir "$DOTNET_ROOT"
else
    echo "    .NET ${DOTNET_SDK_CHANNEL} SDK already present"
fi

if ! dotnet --list-runtimes 2>/dev/null | grep -q "^Microsoft.NETCore.App ${DOTNET_RUNTIME_CHANNEL}\."; then
    [ -f /tmp/dotnet-install.sh ] || curl -sSL https://dot.net/v1/dotnet-install.sh -o /tmp/dotnet-install.sh
    bash /tmp/dotnet-install.sh --channel "$DOTNET_RUNTIME_CHANNEL" --runtime dotnet --install-dir "$DOTNET_ROOT"
else
    echo "    .NET ${DOTNET_RUNTIME_CHANNEL} runtime already present"
fi
rm -f /tmp/dotnet-install.sh

# Persist the SDK location for future shells
if ! grep -qs 'DOTNET_ROOT=' "$HOME/.bashrc"; then
    echo 'export DOTNET_ROOT="$HOME/.dotnet"' >> "$HOME/.bashrc"
    echo 'export PATH="$DOTNET_ROOT:$PATH"' >> "$HOME/.bashrc"
fi

# ---------------------------------------------------------------
# 3. SharpFuzz global CLI tool
# ---------------------------------------------------------------
echo ""
echo "==> [3/5] SharpFuzz.CommandLine (dotnet global tool)"

export PATH="$PATH:$HOME/.dotnet/tools"

if ! command -v sharpfuzz >/dev/null 2>&1; then
    dotnet tool install --global SharpFuzz.CommandLine
else
    echo "    sharpfuzz already installed; updating"
    dotnet tool update --global SharpFuzz.CommandLine || true
fi

# Persist PATH update for future shells
if ! grep -qs '.dotnet/tools' "$HOME/.bashrc"; then
    echo 'export PATH="$PATH:$HOME/.dotnet/tools"' >> "$HOME/.bashrc"
fi

# ---------------------------------------------------------------
# 4. Kernel params + resource limits AFL needs
# ---------------------------------------------------------------
echo ""
echo "==> [4/5] Kernel / ulimit config"

# AFL refuses to start if core_pattern pipes coredumps to a helper (which
# is the default on most distros) - redirect to a plain file.
echo core | sudo tee /proc/sys/kernel/core_pattern >/dev/null

# WSL2 exposes scaling_governor read-only; the run script already sets
# AFL_SKIP_CPUFREQ=1 to bypass the check.
ulimit -n 65536 || true

# ---------------------------------------------------------------
# 5. Build and run the canary
# ---------------------------------------------------------------
echo ""
echo "==> [5/5] Building fuzz harness and running canary"

cd "$REPO"
dotnet build -c Release fuzz/Jyro.Fuzz.Source.fsproj

echo ""
echo "====================================================="
echo "  Setup complete. Next steps:"
echo "====================================================="
echo ""
echo "  1. Verify pipeline:    bash fuzz/canary.sh"
echo "  2. Launch campaign:    bash fuzz/run-afl.sh"
echo "  3. Ctrl+C to stop AFL. Findings in fuzz/findings/default/"
echo ""
echo "  Tip: the repo is on the Windows filesystem (/mnt/c/...)."
echo "  For maximum throughput, consider copying to Linux-native FS:"
echo "    cp -r /mnt/c/GitHub/jyro ~/jyro && cd ~/jyro"
echo "  and running the campaign from there. Cross-FS I/O in WSL2 is"
echo "  ~5-10x slower than native ext4."
echo ""

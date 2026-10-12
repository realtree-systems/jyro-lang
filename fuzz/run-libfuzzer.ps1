# Launch libfuzzer-dotnet against the Jyro source fuzzer on Windows.
#
# Prereqs (already done in initial setup):
#   - libfuzzer-dotnet.exe at C:\GitHub\libfuzzer-dotnet\libfuzzer-dotnet.exe
#   - SharpFuzz.CommandLine installed (dotnet tool install --global SharpFuzz.CommandLine)
#   - fuzz\publish-win\ populated by `dotnet publish ...`
#   - Jyro.Parser/Compiler/Core/Stdlib DLLs in publish-win instrumented with sharpfuzz
#
# Stop with Ctrl+C. Findings land in fuzz\findings-libfuzzer\.

$ErrorActionPreference = 'Stop'

$here      = Split-Path -Parent $MyInvocation.MyCommand.Definition
$repo      = Split-Path -Parent $here
$driver    = "C:\GitHub\libfuzzer-dotnet\libfuzzer-dotnet.exe"
$targetDll = Join-Path $here "publish-win\Jyro.Fuzz.Source.dll"
$seeds     = Join-Path $here "seeds"
$dict      = Join-Path $here "jyro.dict"
$findings  = Join-Path $here "findings-libfuzzer"

if (-not (Test-Path $driver))    { throw "Missing libfuzzer-dotnet driver: $driver" }
if (-not (Test-Path $targetDll)) { throw "Missing target DLL: $targetDll" }
if (-not (Test-Path $seeds))     { throw "Missing seed corpus: $seeds" }

# Fresh findings directory each run
if (Test-Path $findings) { Remove-Item -Recurse -Force $findings }
New-Item -ItemType Directory -Path $findings | Out-Null

# .NET 8 coverage accuracy: disable tiering and R2R so JIT'd code matches
# the instrumented IL SharpFuzz produced.
$env:DOTNET_TieredCompilation = '0'
$env:DOTNET_ReadyToRun         = '0'

# Find dotnet.exe (libfuzzer-dotnet needs the absolute path)
$dotnet = (Get-Command dotnet).Source
Write-Host "driver    : $driver"
Write-Host "dotnet    : $dotnet"
Write-Host "target    : $targetDll"
Write-Host "seeds     : $seeds (count: $((Get-ChildItem $seeds).Count))"
Write-Host "findings  : $findings"
Write-Host "dictionary: $dict"
Write-Host ""
Write-Host "Launching. Ctrl+C to stop."
Write-Host ""

& $driver `
    "--target_path=$dotnet" `
    "--target_arg=$targetDll" `
    "-timeout=10" `
    "-dict=$dict" `
    "-artifact_prefix=$findings\" `
    $seeds

$ErrorActionPreference = 'Stop'

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location (Split-Path -Parent $scriptDir)

Write-Host "Running safe devtools check (Quarto temporarily removed from PATH)..."
Rscript "tools/check_safe_windows.R"

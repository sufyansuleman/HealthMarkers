@echo off
REM Quarto shim to handle Windows builds where R passes env-like arg `TMPDIR=...` as an argument.
REM If the first argument begins with "TMPDIR=", drop it and call the real quarto.exe with remaining args.
setlocal enabledelayedexpansion
if "%~1"=="" (
  quarto.exe
  exit /b %ERRORLEVEL%
)
set first=%~1
echo %first% | findstr /b /c:"TMPDIR=" >nul
if %ERRORLEVEL%==0 (
  shift
)
quarto.exe %*
endlocal

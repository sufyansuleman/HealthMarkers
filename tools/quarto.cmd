@echo off
REM Quarto shim: strips TMPDIR=... args that R's system2() wrongly passes as CLI args on Windows.
REM This file must precede quarto.exe in PATH (done via .Rprofile).
setlocal enabledelayedexpansion

REM Find the real quarto.exe (where quarto.exe skips .cmd/.bat files)
set "QUARTO_REAL="
for /f "delims=" %%Q in ('where quarto.exe 2^>nul') do (
    if "!QUARTO_REAL!"=="" set "QUARTO_REAL=%%Q"
)
if "!QUARTO_REAL!"=="" (
    echo Error: quarto.exe not found in PATH >&2
    exit /b 1
)

REM Build arg list, skipping any arg that starts with TMPDIR=
set "NEWARGS="
:LOOP
if "%~1"=="" goto EXEC
set "CURARG=%~1"
if /i "!CURARG:~0,7!"=="TMPDIR=" (
    shift
    goto LOOP
)
if "!NEWARGS!"=="" (
    set "NEWARGS=%1"
) else (
    set "NEWARGS=!NEWARGS! %1"
)
shift
goto LOOP

:EXEC
if defined NEWARGS (
    "!QUARTO_REAL!" !NEWARGS!
) else (
    "!QUARTO_REAL!"
)
exit /b !ERRORLEVEL!

@echo off
setlocal
cd /d "%~dp0\.."
echo Running safe devtools check (Quarto temporarily removed from PATH)...
Rscript "tools/check_safe_windows.R"
endlocal

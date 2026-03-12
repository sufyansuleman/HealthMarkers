# Safe check runner for Windows when Quarto CLI fails with TMPDIR=... argument
# Usage: Rscript tools/check_safe_windows.R

old_path <- Sys.getenv("PATH")
on.exit(Sys.setenv(PATH = old_path), add = TRUE)

parts <- strsplit(old_path, ";", fixed = TRUE)[[1]]
# Remove common Quarto install paths for this session only
parts <- parts[!grepl("Quarto[/\\]bin", parts, ignore.case = TRUE)]
Sys.setenv(PATH = paste(parts, collapse = ";"))

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required. Install it with install.packages('devtools').")
}

devtools::check(
  pkg = normalizePath(".", winslash = "/", mustWork = TRUE),
  args = c("--as-cran", "--no-manual")
)

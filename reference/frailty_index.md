# Compute Frailty (Deficit) Index using di::di with QA and verbose summaries

Thin wrapper around the di package's di() that:

- Validates inputs and arguments.

- Coerces tibbles to base data.frames (for di()'s class checks).

- Auto-selects numeric deficit columns when cols = NULL (excluding age
  if supplied).

- Optionally scans for missing/out-of-range values with warnings or
  errors.

- Provides step-by-step verbose output and a completion summary.

- Optionally returns a tidy tibble instead of the original list.

## Usage

``` r
frailty_index(
  data,
  cols = NULL,
  invert = NULL,
  rescale = TRUE,
  age = NULL,
  rescale.custom = NULL,
  rescale.avoid = NULL,
  bins = 7,
  visible = FALSE,
  na_action = c("ignore", "warn", "error", "keep", "omit"),
  na_warn_prop = 0.2,
  check_extreme = NULL,
  extreme_action = c("warn", "ignore", "error", "cap", "NA"),
  return = c("list", "data"),
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame or tibble of health deficits (ideally binary/logical or
  scaled to \\\[0,1\]\\). Non-binary numeric columns can be rescaled by
  di::di when rescale = TRUE.

- cols:

  Character vector of deficit column names to use. If NULL (default),
  all numeric columns are used except age (if supplied).

- invert:

  Character vector of column names whose values should be inverted by
  di::di (e.g., where higher values indicate better health).

- rescale:

  Logical; if TRUE, non-binary columns will be rescaled to \\\[0,1\]\\
  by di::di. Default TRUE.

- age:

  Optional name of the column holding age (used by di for plotting and
  optional age-binned outputs; excluded from auto-selected cols).

- rescale.custom:

  Advanced argument passed through to di::di. See di::di documentation
  for syntax.

- rescale.avoid:

  Advanced argument passed through to di::di; see di::di documentation
  for syntax.

- bins:

  Integer; number of age bins for FI-by-age plots. Default 7.

- visible:

  Logical; if TRUE and age is provided, di will draw a plot (via
  plot.di()). Default FALSE.

- na_action:

  One of `c("keep","omit","error","warn","ignore")`. Controls handling
  of missing values in selected deficit columns. `"keep"` (and its
  backward-compatible alias `"ignore"`) passes NAs through to di::di.
  `"warn"` emits a warning and then keeps NAs (alias for `"keep"` with a
  missingness warning). `"omit"` drops rows with any NA in selected
  deficits before computing. `"error"` stops if any NA is detected.
  Default `"ignore"` (retained for backward compatibility; equivalent to
  `"keep"`).

- na_warn_prop:

  Proportion in \\\[0,1\]\\ above which a high-missingness warning is
  emitted (per column) when na_action = "warn". Default 0.2.

- check_extreme:

  NULL/TRUE/FALSE gate for out-of-range scan:

  - NULL (default): legacy behavior (scan only when rescale = FALSE)

  - TRUE: always scan selected deficits for values \< 0 or \> 1 before
    di::di

  - FALSE: never scan for extremes

- extreme_action:

  One of "warn","ignore","error","cap","NA" for out-of-range handling
  when scanning is enabled.

  - "cap": truncate to \\\[0,1\]\\

  - "NA": set out-of-range to NA Default "warn".

- return:

  One of c("list","data"). "list" (default) returns the original di::di
  result (backward compatible). "data" returns a tibble with one row per
  individual, columns: di (the frailty index) plus the selected deficit
  columns (post-capping if applied). Age is included if present.

- verbose:

  Logical; if TRUE, prints progress and a completion summary. Default
  FALSE.

## Value

- If return = "list" (default): the object returned by di::di (typically
  a list with di and columns).

- If return = "data": a tibble with di and the selected columns.

## Details

Background The Frailty Index (FI) is computed as the proportion of
health deficits present in an individual across a set of candidate
deficits. The approach was introduced and formalized by Rockwood and
Mitnitski and subsequently standardized for construction and reporting.

## References

Mitnitski AB, Mogilner AJ, Rockwood K (2001). “Accumulation of Deficits
as a Proxy Measure of Aging.” *The Scientific World Journal*, **1**,
323–336. [doi:10.1100/tsw.2001.58](https://doi.org/10.1100/tsw.2001.58)
. Rockwood K, Mitnitski A (2007). “Frailty in Relation to the
Accumulation of Deficits.” *Journals of Gerontology Series A*,
**62**(7), 722–727.
[doi:10.1093/gerona/62.7.722](https://doi.org/10.1093/gerona/62.7.722) .
Searle SD, Mitnitski A, Gahbauer EA, Gill TM, Rockwood K (2008). “A
Standard Procedure for Creating a Frailty Index.” *BMC Geriatrics*,
**8**, 24.
[doi:10.1186/1471-2318-8-24](https://doi.org/10.1186/1471-2318-8-24) .
Rockwood K, Theou O (2020). “Using the Clinical Frailty Scale in
Allocating Scarce Health Care Resources.” *Canadian Geriatrics Journal*,
**23**(3), 210–215.
[doi:10.5770/cgj.23.463](https://doi.org/10.5770/cgj.23.463) . Cesari M,
Gambassi G, van Kan GA, Vellas B (2014). “The Frailty Phenotype and the
Frailty Index: Different Instruments for Different Purposes.” *Age and
Ageing*, **43**(1), 10–12.
[doi:10.1093/ageing/aft160](https://doi.org/10.1093/ageing/aft160) .

## Examples

``` r
# Minimal example (runs only if the 'di' package is installed)
if (requireNamespace("di", quietly = TRUE)) {
  df <- data.frame(
    age = c(70, 75, 80),
    d1 = c(0, 1, 1),
    d2 = c(0.2, 0.8, 1.0),
    d3 = c(TRUE, FALSE, TRUE)
  )
  # Auto-select numeric deficits; returns list (di, columns)
  res <- frailty_index(df, cols = NULL, age = "age", verbose = TRUE)
  # Tidy tibble return
  tb  <- frailty_index(df, cols = c("d1","d2","d3"), age = "age",
                       return = "data", verbose = TRUE)
}
#> frailty_index(): preparing inputs (3 rows, 2 deficits, age provided)
#> frailty_index(): column map: d1, d2
#> frailty_index(): results: di range [0.000, 1.000] (3 rows, 2 deficits)
#> frailty_index(): preparing inputs (3 rows, 3 deficits, age provided)
#> frailty_index(): column map: d1, d2, d3
#> frailty_index(): results: di 3/3
```

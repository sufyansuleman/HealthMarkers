# Compute atherogenic indices

Calculates:

- AIP: Atherogenic Index of Plasma = log10(TG / HDL_c)

- CRI_I: Castelli Risk Index I = TC / HDL_c

- CRI_II: Castelli Risk Index II = LDL_c / HDL_c

## Usage

``` r
atherogenic_indices(
  data,
  col_map,
  na_action = c("keep", "omit", "error"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  normalize = c("none", "log10"),
  verbose = FALSE
)
```

## Arguments

- data:

  data.frame/tibble with lipid columns.

- col_map:

  named list mapping keys to columns, e.g. list(TG="TG", HDL_c="HDL_c",
  TC="TC", LDL_c="LDL_c").

- na_action:

  one of c("keep","omit","error").

- check_extreme:

  logical; if TRUE, screen inputs for extremes using `extreme_rules`.

- extreme_action:

  one of c("warn","cap","error","ignore","NA").

- extreme_rules:

  optional named list of bounds per key or column, each c(min, max).

- normalize:

  one of c("none","log10"). Reserved; AIP always uses log10(TG/HDL_c).

- verbose:

  logical; prints step messages via hm_inform when TRUE.

## Value

tibble with columns AIP, CRI_I, CRI_II

## Details

Behavior:

- Required keys: TG, HDL_c. Optional: TC, LDL_c.

- NA policy via `na_action`: "keep" (default), "omit" (drop rows with
  any NA in used lipids), "error".

- Extreme screening via `check_extreme` and `extreme_action`
  ("warn","cap","error","ignore","NA"). Default bounds (mg/dL) used only
  for screening: TG (0, 10000), HDL_c (0, 1000), LDL_c (0, 10000), TC
  (0, 10000). Note: All indices are unitless ratios; units cancel in
  computations.

- Emits progress via
  [`hm_inform()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_inform.md)
  when `verbose = TRUE` or when package option enables logs.

## References

Dobiášová M (2004). “Atherogenic Index of Plasma (AIP)
log(Triglycerides/HDL-Cholesterol): Theoretical and Practical
Implications.” *Clinical Chemistry*, **50**(7), 1113–1115.
[doi:10.1373/clinchem.2004.033175](https://doi.org/10.1373/clinchem.2004.033175)
. ; Castelli WP, Doyle JT, Gordon T, et al. (1977). “High-Density
Lipoprotein Cholesterol and Other Lipids in Coronary Heart Disease: The
Framingham Study.” *American Journal of Medicine*, **62**(5), 707–714.
[doi:10.1016/0002-9343(77)90874-9](https://doi.org/10.1016/0002-9343%2877%2990874-9)
.

## Examples

``` r
df <- tibble::tibble(
  TG = c(150, 200),
  HDL_c = c(50, 40),
  TC = c(200, 220),
  LDL_c = c(120, 150)
)
cm <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
atherogenic_indices(df, col_map = cm, verbose = FALSE)
#> # A tibble: 2 × 3
#>     AIP CRI_I CRI_II
#>   <dbl> <dbl>  <dbl>
#> 1 0.477   4     2.4 
#> 2 0.699   5.5   3.75
```

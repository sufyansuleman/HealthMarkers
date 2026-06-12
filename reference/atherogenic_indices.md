# Compute atherogenic indices

Calculates:

- AIP: Atherogenic Index of Plasma = log10(TG / HDL_c)

- CRI_I: Castelli Risk Index I = TC / HDL_c

- CRI_II: Castelli Risk Index II = LDL_c / HDL_c

## Usage

``` r
atherogenic_indices(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  normalize = c("none", "log10"),
  verbose = TRUE
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

- normalize:

  one of c("none","log10"). Reserved; AIP always uses log10(TG/HDL_c).

- verbose:

  Logical; if `TRUE` (default), prints column mapping, the list of
  indices being computed, and a per-column results summary.

## Value

tibble with columns AIP, CRI_I, CRI_II. If an ID column is detected in
`data` (e.g. `id`, `IID`, `participant_id`), it is prepended.

## Details

Units (package convention): TG, HDL_c, TC, LDL_c in mmol/L.

- AIP (Dobiasova 2004) is defined with TG and HDL_c in mmol/L and is NOT
  scale-invariant: TG and HDL-c convert to mg/dL with different factors
  (88.57 vs 38.67), so mg/dL inputs shift AIP by +log10(88.57/38.67)
  approx +0.36 and invalidate the published risk strata. Provide mmol/L.

- CRI_I (TC/HDL_c) and CRI_II (LDL_c/HDL_c) are ratios of cholesterol
  fractions and ARE scale-invariant (identical in mg/dL or mmol/L).

Behavior:

- Required keys: TG, HDL_c. Optional: TC, LDL_c.

- NA policy via `na_action`: "keep" (default), "omit" (drop rows with
  any NA in used lipids), "error".

- Emits progress via `hm_inform()` when `verbose = TRUE` or when package
  option enables logs.

## References

Dobiášová M (2004). “Atherogenic Index of Plasma (AIP)
log(Triglycerides/HDL-Cholesterol): Theoretical and Practical
Implications.” *Clinical Chemistry*, **50**(7), 1113–1115.
[doi:10.1373/clinchem.2004.033175](https://doi.org/10.1373/clinchem.2004.033175)
. Castelli WP, Doyle JT, Gordon T, et al. (1977). “High-Density
Lipoprotein Cholesterol and Other Lipids in Coronary Heart Disease: The
Framingham Study.” *American Journal of Medicine*, **62**(5), 707–714.
[doi:10.1016/0002-9343(77)90874-9](https://doi.org/10.1016/0002-9343%2877%2990874-9)
.

## Examples

``` r
# Units: mmol/L (TG, HDL_c, TC, LDL_c)
df <- tibble::tibble(
  TG = c(1.7, 2.3),
  HDL_c = c(1.3, 1.0),
  TC = c(5.2, 5.7),
  LDL_c = c(3.1, 3.9)
)
cm <- list(TG = "TG", HDL_c = "HDL_c", TC = "TC", LDL_c = "LDL_c")
atherogenic_indices(df, col_map = cm)
#> atherogenic_indices(): reading input 'df' — 2 rows × 4 variables
#> atherogenic_indices(): col_map (4 columns — 4 specified)
#>   TG                ->  'TG'
#>   HDL_c             ->  'HDL_c'
#>   TC                ->  'TC'
#>   LDL_c             ->  'LDL_c'
#> atherogenic_indices(): computing markers:
#>   AIP        [log10(TG / HDL_c)]
#>   CRI_I      [TC / HDL_c [if TC available]]
#>   CRI_II     [LDL_c / HDL_c [if LDL_c available]]
#> atherogenic_indices(): results: AIP 2/2, CRI_I 2/2, CRI_II 2/2
#> # A tibble: 2 × 3
#>     AIP CRI_I CRI_II
#>   <dbl> <dbl>  <dbl>
#> 1 0.117   4     2.38
#> 2 0.362   5.7   3.9 
```

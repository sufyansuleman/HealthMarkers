# Neurofilament Light Chain (NfL) Biomarker

Incorporates a neurofilament light chain (NfL) measurement into the
analysis pipeline. Placeholder for future NfL-based computations;
returns provided values with input checks.

## Usage

``` r
nfl_marker(
  data,
  col_map = list(nfl = "NfL"),
  verbose = FALSE,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL
)
```

## Arguments

- data:

  A data.frame or tibble with an NfL concentration column.

- col_map:

  Named list with `nfl` indicating the NfL column name.

- verbose:

  Logical; if TRUE, emits progress messages.

- na_action:

  One of c("keep","omit","error","ignore","warn").

- check_extreme:

  Logical; if TRUE, scan inputs for plausible ranges.

- extreme_action:

  One of c("warn","cap","error","ignore","NA").

- extreme_rules:

  Optional overrides; default: list(nfl = c(0, 1e6)) in input units.

## Value

A tibble with one column: nfl_value (numeric; same units as input).

## Details

NfL is released during neuroaxonal injury; elevated levels in CSF or
blood indicate neuroaxonal damage and typically increase with age and in
neurological diseases. Interpretation requires context-specific and
age-adjusted references. This function simply returns the input NfL
values (assumed in a single matrix/fluid, e.g., plasma pg/mL) without
classification.

## References

Simrén J, Ashton NJ, Blennow K, Zetterberg H, et al. (2022). “Reference
values for plasma neurofilament light in healthy individuals.” *Brain
Communications*, **4**(4), fcac174.
[doi:10.1093/braincomms/fcac174](https://doi.org/10.1093/braincomms/fcac174)
. ; Disanto G, Barro C, Benkert P, et al. (2017). “Serum neurofilament
light: a biomarker of neuronal damage in multiple sclerosis.” *Annals of
Neurology*, **88**(9), 857–870.
[doi:10.1002/ana.24954](https://doi.org/10.1002/ana.24954) .

## Examples

``` r
df <- data.frame(NfL = c(8.5, 14.2, 22.1))
nfl_marker(df)
#> # A tibble: 3 × 1
#>   nfl_value
#>       <dbl>
#> 1       8.5
#> 2      14.2
#> 3      22.1
```

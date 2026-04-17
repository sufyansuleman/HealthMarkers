# Neurofilament Light Chain (NfL) Biomarker

Incorporates a neurofilament light chain (NfL) measurement into the
analysis pipeline. Placeholder for future NfL-based computations;
returns provided values with input checks.

## Usage

``` r
nfl_marker(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with an NfL concentration column.

- col_map:

  Named list with `nfl` indicating the NfL column name.

- na_action:

  One of c("keep","omit","error","ignore","warn").

- verbose:

  Logical; if TRUE (default), emits progress messages.

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
#> nfl_marker(): reading input 'df' — 3 rows × 1 variables
#> nfl_marker(): col_map (1 column — 1 inferred from data)
#>   nfl               ->  'NfL'    (inferred)
#> nfl_marker(): computing markers:
#>   nfl_value  [passthrough of NfL input]
#> nfl_marker(): results: nfl_value 3/3
#> # A tibble: 3 × 1
#>   nfl_value
#>       <dbl>
#> 1       8.5
#> 2      14.2
#> 3      22.1
```

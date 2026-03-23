# Impute missing values via random forest (missForest)

Wraps missForest to impute numeric columns using non-parametric random
forests. Requires the missForest package to be installed (Suggests).
Non-numeric columns are untouched. If no numeric columns contain NAs,
`data` is returned unchanged.

## Usage

``` r
impute_missforest(data, ntree = 100, cols = NULL, verbose = FALSE, ...)
```

## Arguments

- data:

  A data.frame or tibble containing missing values.

- ntree:

  Integer; number of trees to grow in each forest (passed to
  missForest). Default 100.

- cols:

  Optional character vector of numeric columns to impute. Defaults to
  all numeric columns with at least one NA.

- verbose:

  Logical; if TRUE, prints progress and a completion summary. Default
  FALSE.

- ...:

  Additional arguments passed to missForest::missForest().

## Value

A data.frame/tibble with numeric columns imputed by missForest (or mean
fallback).

## Details

Notes:

- missForest uses iterative RF training; it can be slow on wide/high-NA
  data.

- Errors (e.g., insufficient unique values) are caught and a
  deterministic mean imputation fallback is applied to the selected
  numeric columns.

## References

Stekhoven DJ, Buhlmann P (2012). “MissForest—non-parametric missing
value imputation for mixed-type data.” *Bioinformatics*, **28**(1),
112–118.
[doi:10.1093/bioinformatics/btr597](https://doi.org/10.1093/bioinformatics/btr597)
.

## Examples

``` r
# \donttest{
if (requireNamespace("missForest", quietly = TRUE)) {
  df <- tibble::tibble(a = c(1, NA, 3), b = c(2, 4, NA), c = 5)
  impute_missforest(df, ntree = 50, verbose = TRUE)
}
#> Warning: impute_missforest(): missForest failed; falling back to mean imputation.
#> Warning: impute_missing(): column 'a' has high missingness (33.3%).
#> Warning: impute_missing(): column 'b' has high missingness (33.3%).
#> # A tibble: 3 × 3
#>       a     b     c
#>   <dbl> <dbl> <dbl>
#> 1     1     2     5
#> 2     2     4     5
#> 3     3     3     5
# }
```

# Impute missing values via Multiple Imputation (mice)

Wraps mice to impute only numeric columns; non-numeric columns are
untouched. Requires the mice package to be installed (Suggests). If no
numeric columns contain NAs, `data` is returned unchanged.

## Usage

``` r
impute_mice(data, m = 5, cols = NULL, verbose = FALSE, ...)
```

## Arguments

- data:

  A data.frame or tibble containing missing values.

- m:

  Integer; number of imputations to run (passed to mice). Default 5.

- cols:

  Optional character vector of numeric columns to impute. Defaults to
  all numeric columns with at least one NA.

- verbose:

  Logical; if TRUE, prints progress and a completion summary. Default
  FALSE.

- ...:

  Additional arguments passed to mice::mice().

## Value

A data.frame/tibble with numeric columns imputed by mice.

## Details

Notes:

- At least two numeric columns are typically needed by mice to borrow
  strength.

- This function runs `m` imputations and returns the first completed
  dataset.

- Messages from mice are suppressed; use `verbose = TRUE` here for
  high-level progress.

## References

Rubin DB (1987). *Multiple Imputation for Nonresponse in Surveys*.
Wiley.
[doi:10.1002/9780470316696](https://doi.org/10.1002/9780470316696) . ;
van Buuren S, Groothuis-Oudshoorn K (2011). “mice: Multivariate
Imputation by Chained Equations in R.” *Journal of Statistical
Software*, **45**(3), 1–67.
[doi:10.18637/jss.v045.i03](https://doi.org/10.18637/jss.v045.i03) .

## Examples

``` r
# \donttest{
if (requireNamespace("mice", quietly = TRUE)) {
  df <- tibble::tibble(a = c(1, NA, 3), b = c(2, 4, NA), c = 5)
  impute_mice(df, m = 2, verbose = TRUE)
}
#> impute_mice(): preparing inputs (3 rows, 2 columns)
#> impute_mice(): results: imputed 2 values across 2 columns.
#> # A tibble: 3 × 3
#>       a     b     c
#>   <dbl> <dbl> <dbl>
#> 1     1     2     5
#> 2     3     4     5
#> 3     3     4     5
# }
```

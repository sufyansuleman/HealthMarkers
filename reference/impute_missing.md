# Impute missing values in a data.frame or tibble (simple, column-wise)

Performs deterministic, per-column imputation for numeric variables:

- "mean": replace NAs with the column mean

- "median": replace NAs with the column median

- "zero": replace NAs with 0

- "constant": replace NAs with the single value given in `constant`

## Usage

``` r
impute_missing(
  data,
  method = c("mean", "median", "zero", "constant"),
  cols = NULL,
  constant = NULL,
  na_warn_prop = 0.2,
  verbose = FALSE
)
```

## Arguments

- data:

  A data.frame or tibble containing missing values.

- method:

  Character; one of c("mean","median","zero","constant").

- cols:

  Optional character vector of column names to impute. Defaults to all
  numeric columns in `data` that contain at least one NA.

- constant:

  Numeric; single value to use when `method = "constant"`.

- na_warn_prop:

  Numeric in \\\[0,1\]\\; threshold for high-missingness warnings per
  column. Default 0.2.

- verbose:

  Logical; if TRUE, prints progress and a completion summary. Default
  FALSE.

## Value

A data.frame/tibble of the same dimensions as `data`, with the specified
columns' missing values imputed.

## Details

Non-numeric columns are left untouched. If `cols = NULL`, all numeric
columns that have at least one NA are selected automatically. NA
positions are the only values modified; non-NA entries are preserved
as-is.

Quality checks:

- Warns for high-missingness columns (\>= `na_warn_prop`).

- Warns and skips imputation when a column has no non-NA values
  (mean/median undefined).

- Coerces only numeric columns; non-numerics in `cols` are skipped with
  a warning.

## Examples

``` r
df <- tibble::tibble(a = c(1, NA, 3), b = c(NA, NA, 2), c = letters[1:3])
impute_missing(df, method = "mean")
#> Warning: impute_missing(): column 'a' has high missingness (33.3%).
#> Warning: impute_missing(): column 'b' has high missingness (66.7%).
#> # A tibble: 3 × 3
#>       a     b c    
#>   <dbl> <dbl> <chr>
#> 1     1     2 a    
#> 2     2     2 b    
#> 3     3     2 c    
impute_missing(df, method = "median", verbose = TRUE)
#> Warning: impute_missing(): column 'a' has high missingness (33.3%).
#> Warning: impute_missing(): column 'b' has high missingness (66.7%).
#> # A tibble: 3 × 3
#>       a     b c    
#>   <dbl> <dbl> <chr>
#> 1     1     2 a    
#> 2     2     2 b    
#> 3     3     2 c    
impute_missing(df, method = "constant", constant = -1, cols = "a")
#> Warning: impute_missing(): column 'a' has high missingness (33.3%).
#> # A tibble: 3 × 3
#>       a     b c    
#>   <dbl> <dbl> <chr>
#> 1     1    NA a    
#> 2    -1    NA b    
#> 3     3     2 c    
```

# Summarize selected numeric marker columns

Lightweight summary for numeric columns: n, n_na, mean, sd, median, p25,
p75.

## Usage

``` r
health_summary(x, cols = NULL)
```

## Arguments

- x:

  A data.frame or tibble.

- cols:

  Optional character vector of column names to summarize. If NULL, all
  numeric columns are summarized.

## Value

A tibble with one row per summarized column.

## Examples

``` r
df <- data.frame(a = c(1, 2, NA), b = c(3, 4, 5), c = factor("x"))
health_summary(df)
#> # A tibble: 2 × 8
#>   measure     n  n_na  mean    sd median   p25   p75
#>   <chr>   <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1 a           3     1   1.5 0.707    1.5  1.25  1.75
#> 2 b           3     0   4   1        4    3.5   4.5 
```

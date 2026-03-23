# Summarize marker outputs

Computes simple summaries (mean, SD, IQR) for numeric columns.

## Usage

``` r
marker_summary(x, verbose = FALSE)
```

## Arguments

- x:

  Data frame returned by marker functions

- verbose:

  Logical; if TRUE, prints progress messages

## Value

Tibble with columns: variable, mean, sd, iqr

## Examples

``` r
df <- data.frame(glucose = c(5.5, 6.1, 4.9), insulin = c(60, 88, 55),
                 bmi = c(24, 27, 22))
marker_summary(df)
#> # A tibble: 3 × 4
#>   variable  mean     sd    iqr
#>   <chr>    <dbl>  <dbl>  <dbl>
#> 1 glucose    5.5  0.600  0.600
#> 2 insulin   67.7 17.8   16.5  
#> 3 bmi       24.3  2.52   2.5  
```

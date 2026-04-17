# RiskScorescvd calculator

Passthrough to
[`RiskScorescvd::calc_scores()`](https://dvicencio.github.io/RiskScorescvd/reference/RiskScoresCalc.html)
with graceful fallback to NA.

## Usage

``` r
cvd_risk_scorescvd(data, ...)
```

## Arguments

- data:

  Data required by
  [`RiskScorescvd::calc_scores()`](https://dvicencio.github.io/RiskScorescvd/reference/RiskScoresCalc.html).

- ...:

  Passed to
  [`RiskScorescvd::calc_scores()`](https://dvicencio.github.io/RiskScorescvd/reference/RiskScoresCalc.html).

## Value

Object returned by
[`RiskScorescvd::calc_scores()`](https://dvicencio.github.io/RiskScorescvd/reference/RiskScoresCalc.html).

## Examples

``` r
# \donttest{
  if (requireNamespace("RiskScorescvd", quietly = TRUE)) {
    df <- data.frame(
      Age = 55, Sex = 0, Smoking_status = 1,
      systolic.bp = 140, Total_cholesterol = 5.5,
      HDL.cholesterol = 1.3
    )
    cvd_risk_scorescvd(df)
  }
#> # A tibble: 1 × 4
#>   model          year  risk value
#>   <chr>         <int> <dbl> <dbl>
#> 1 RiskScorescvd    NA    NA    NA
# }
```

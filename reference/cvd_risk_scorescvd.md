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
if (requireNamespace("RiskScorescvd", quietly = TRUE)) {
  # cvd_risk_scorescvd(your_data_frame)
}
#> NULL
```

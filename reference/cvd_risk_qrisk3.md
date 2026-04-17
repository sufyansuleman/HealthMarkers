# QRISK3 10-year risk (UK QRISK3-2017)

Wrapper around
[`QRISK3::QRISK3_2017()`](https://rdrr.io/pkg/QRISK3/man/QRISK3_2017.html)
that auto-generates a `patid` if one is not supplied. Adds input
validation and quiet failure to NA on backend error.

## Usage

``` r
cvd_risk_qrisk3(data, ..., patid = NULL, na_warn_prop = 0.2, verbose = TRUE)
```

## Arguments

- data:

  A data frame with variables required by
  [`QRISK3::QRISK3_2017()`](https://rdrr.io/pkg/QRISK3/man/QRISK3_2017.html).

- ...:

  Passed to
  [`QRISK3::QRISK3_2017()`](https://rdrr.io/pkg/QRISK3/man/QRISK3_2017.html).

- patid:

  Optional vector of patient IDs (default: `1:nrow(data)`).

- na_warn_prop:

  Proportion (0-1) to flag high missingness warnings (default 0.2). Only
  used when `verbose = TRUE`.

- verbose:

  Logical; if TRUE, prints progress and a short summary.

## Value

A tibble with columns `model`, `year`, `risk`.

## References

Hippisley-Cox J, Coupland C, Brindle P, et al. (2017). “Development and
validation of QRISK3 risk prediction algorithms.” *BMJ*, **357**, j2099.
[doi:10.1136/bmj.j2099](https://doi.org/10.1136/bmj.j2099) .

## Examples

``` r
# \donttest{
  if (requireNamespace("QRISK3", quietly = TRUE)) {
    df <- data.frame(
      gender = 1L, age = 55L,
      atrial_fibrillation = 0L, atypical_antipsy = 0L,
      regular_steroid_tablets = 0L, erectile_disfunction = 0L,
      migraine = 0L, rheumatoid_arthritis = 0L,
      chronic_kidney_disease = 0L, severe_mental_illness = 0L,
      systemic_lupus_erythematosis = 0L, blood_pressure_treatment = 0L,
      diabetes1 = 0L, diabetes2 = 0L,
      weight = 80, height = 175,
      ethnicity = 1L, heart_attack_relative = 0L,
      cholesterol_HDL_ratio = 4.2, systolic_blood_pressure = 130,
      std_systolic_blood_pressure = 7, smoke = 0L, townsend = 0
    )
    cvd_risk_qrisk3(df)
  }
#> cvd_risk_qrisk3(): preparing inputs
#> 
#> This unofficial R package was based on open-sourced free original QRISK3-2017 algorithm
#> 
#> You may find the source code and the official disclaimer of the original open-sourced
#> 
#> QRISK3-2017 algorithm (published by ClinRisk Ltd.) from below path by runing following in R
#> 
#> sourcePath <- system.file("extdata/QRISK3_2017_src.txt", package = "QRISK3")
#> 
#> print(sourcePath)
#> 
#> The risk score calculated from this R package can only be used for research purpose.
#> 
#> You may want to visit official QRISK3 website for more information
#> 
#> https://qrisk.org/
#> # A tibble: 1 × 3
#>   model   year  risk
#>   <chr>  <int> <dbl>
#> 1 QRISK3    10    NA
# }
```

# QRISK3 10-year risk (UK QRISK3-2017)

Wrapper around
[`QRISK3::QRISK3_2017()`](https://rdrr.io/pkg/QRISK3/man/QRISK3_2017.html)
that auto-generates a `patid` if one is not supplied. Adds input
validation and quiet failure to NA on backend error.

## Usage

``` r
cvd_risk_qrisk3(data, ..., patid = NULL, na_warn_prop = 0.2, verbose = FALSE)
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
if (requireNamespace("QRISK3", quietly = TRUE)) {
  # cvd_risk_qrisk3(your_data_frame, verbose = TRUE)
}
#> NULL
```

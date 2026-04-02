# Compute Bone Health & Body-Composition Markers (HM-CS v3)

Given DXA, anthropometry, and optional bone-turnover markers, computes:

- OSTA: (weight - age) x 0.2

- ALMI: Appendicular Lean Mass Index = ALM / height^2

- FMI: Fat Mass Index = FM / height^2

- BMD_Tscore: (BMD - ref_mean) / ref_sd and (if in `col_map` + data)
  passes through: TBS, HSA, PINP, CTX, BSAP, Osteocalcin.

## Usage

``` r
bone_markers(
  data,
  col_map,
  na_action = c("keep", "omit", "error"),
  check_extreme = FALSE,
  sds_limit = 6,
  extreme_action = c("cap", "NA", "error"),
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame` or tibble with subject-level DXA/anthropometry data.

- col_map:

  Named list mapping keys to column names. Required keys:

  - `age`, `weight`, `height`, `ALM`, `FM`, `BMD`, `BMD_ref_mean`,
    `BMD_ref_sd` Optional (passed-through if present and found in data):
    `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin`.

- na_action:

  One of "keep", "omit", or "error" controlling how missing/non-finite
  input values are treated.

- check_extreme:

  Logical; if `TRUE`, scan mapped columns whose key names contain "sds"
  (case-insensitive) for absolute values \> `sds_limit`.

- sds_limit:

  Positive numeric; SDS magnitude limit used when `check_extreme` is
  TRUE.

- extreme_action:

  One of "cap", "NA", or "error" for handling extreme SDS-like values.

- verbose:

  Logical; if TRUE, emits progress messages via `hm_inform()`.

## Value

A tibble with columns: `OSTA`, `ALMI`, `FMI`, `BMD_Tscore`, and
optionally `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin` (in that
order).

## Details

Notes:

- Units: height in meters; ALM, FM, weight in kilograms; BMD in g/cm^2;
  ALMI/FMI in kg/m^2.

- Non-finite values are treated as NA; division by zero is prevented by
  input checks.

## References

Koh LK, Ben Sedrine W, Torralba TP, Kung A, others (2001). “A Simple
Tool to Identify Asian Women at Increased Risk of Osteoporosis.”
*Osteoporosis International*, **12**(8), 699–705.
[doi:10.1007/s001980170070](https://doi.org/10.1007/s001980170070) . ;
Kelly TL, Wilson KE, Heymsfield SB (2009). “Dual Energy X-Ray
Absorptiometry Body Composition Reference Values from NHANES.” *PLoS
ONE*, **33**(6), 783–789.
[doi:10.1371/journal.pone.0007038](https://doi.org/10.1371/journal.pone.0007038)
. ; World Health Organization (1994). *Assessment of Fracture Risk and
Its Application to Screening for Postmenopausal Osteoporosis*, volume
843 of *Technical Report Series*. World Health Organization. No DOI for
this WHO report; see URL, <https://iris.who.int/handle/10665/39142>.

## Examples

``` r
library(tibble)
df <- tibble(
  age = c(60, 72), weight = c(65, 50), height = c(1.65, 1.58),
  ALM = c(18.2, 14.7), FM = c(22.0, 20.5),
  BMD = c(0.95, 0.80), BMD_ref_mean = c(1.00, 1.00), BMD_ref_sd = c(0.12, 0.12)
)
col_map <- list(
  age = "age", weight = "weight", height = "height",
  ALM = "ALM", FM = "FM", BMD = "BMD",
  BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd"
)
bone_markers(df, col_map)
#> # A tibble: 2 × 10
#>    OSTA  ALMI   FMI BMD_Tscore   TBS   HSA  PINP   CTX  BSAP Osteocalcin
#>   <dbl> <dbl> <dbl>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>
#> 1   1    6.69  8.08     -0.417    NA    NA    NA    NA    NA          NA
#> 2  -4.4  5.89  8.21     -1.67     NA    NA    NA    NA    NA          NA
```

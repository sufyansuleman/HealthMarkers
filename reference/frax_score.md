# FRAX 10-Year Fracture Risk Score (simplified placeholder)

Estimates the 10-year probabilities of major osteoporotic and hip
fracture using a simplified, non-validated approximation based on FRAX
risk factors. This is for development/demo only and does not implement
the proprietary FRAX algorithm.

## Usage

``` r
frax_score(
  data,
  col_map = list(age = "Age", sex = "Sex", bmd_t = NULL),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  country = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame or tibble with inputs.

- col_map:

  Named list mapping required and optional inputs:

  - Required: age, sex

  - Optional binary risk factors: prior_fracture, parent_fracture,
    steroids, rheumatoid, secondary_op, smoker, alcohol

  - Optional: bmd (T-score; if present and in (-5,0\], used to adjust
    risk)

- na_action:

  One of c("keep","omit","error","ignore","warn").

- check_extreme:

  Logical; if TRUE, scan inputs for plausible ranges.

- extreme_action:

  One of c("warn","cap","error","ignore","NA").

- extreme_rules:

  Optional overrides for defaults (age, bmd).

- country:

  optional country/region code for FRAX calibration (accepted, currently
  unused).

- verbose:

  Logical; if TRUE, emits progress via rlang::inform.

## Value

Tibble with frax_major_percent and frax_hip_percent.

## References

Kanis JA, Johnell O, Oden A, et al. (2008). “FRAX and the Assessment of
Fracture Probability in Men and Women from the UK.” *Osteoporosis
International*, **19**(4), 385–397.
[doi:10.1007/s00198-007-0543-5](https://doi.org/10.1007/s00198-007-0543-5)
. ; Kanis JA, McCloskey EV, Johansson H, Oden A, others (2010).
“Development and use of FRAX® in osteoporosis.” *Osteoporosis
International*, **21**(S2), 407–413.
[doi:10.1007/s00198-010-1253-y](https://doi.org/10.1007/s00198-010-1253-y)
.

## Examples

``` r
df <- data.frame(Age = c(65, 72, 58), Sex = c("female", "female", "male"))
frax_score(df)
#> # A tibble: 3 × 5
#>   frax_major_percent frax_hip_percent frax_sex_norm frax_age_used
#>                <dbl>            <dbl> <chr>                 <dbl>
#> 1                6.5              3.6 female                   65
#> 2                8.6              5.3 female                   72
#> 3                2.6              1.2 male                     58
#> # ℹ 1 more variable: frax_bmd_tscore <dbl>
```

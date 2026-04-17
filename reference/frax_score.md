# FRAX 10-Year Fracture Risk Score (simplified placeholder)

Estimates the 10-year probabilities of major osteoporotic and hip
fracture using a simplified, non-validated approximation based on FRAX
risk factors. This is for development/demo only and does not implement
the proprietary FRAX algorithm.

## Usage

``` r
frax_score(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  country = NULL,
  verbose = TRUE
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
#> frax_score(): reading input 'df' — 3 rows × 2 variables
#> frax_score(): col_map (2 columns — 2 inferred from data)
#>   age               ->  'Age'    (inferred)
#>   sex               ->  'Sex'    (inferred)
#> frax_score(): computing markers:
#>   frax_major_percent [age, sex, risk factors, bmd]
#>   frax_hip_percent [age, sex, risk factors, bmd]
#> frax_score(): results: frax_major_percent 3/3, frax_hip_percent 3/3, frax_sex_norm 3/3, frax_age_used 3/3, frax_bmd_tscore 0/3
#> # A tibble: 3 × 5
#>   frax_major_percent frax_hip_percent frax_sex_norm frax_age_used
#>                <dbl>            <dbl> <chr>                 <dbl>
#> 1                6.5              3.6 female                   65
#> 2                8.6              5.3 female                   72
#> 3                2.6              1.2 male                     58
#> # ℹ 1 more variable: frax_bmd_tscore <dbl>
```

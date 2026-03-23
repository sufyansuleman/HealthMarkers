# Appendicular Lean Mass to BMI Index

Calculates the ratio of appendicular lean mass (ALM) to body mass index
(BMI), and flags low muscle mass based on FNIH Sarcopenia Project
cut-points.

## Usage

``` r
alm_bmi_index(
  data,
  col_map = list(alm = "ALM_kg", bmi = "BMI", sex = "Sex"),
  verbose = FALSE,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL
)
```

## Arguments

- data:

  A data.frame or tibble with ALM, BMI, and sex columns.

- col_map:

  Named list with:

  - alm: appendicular lean mass column name (kg)

  - bmi: body mass index column name (kg/m^2)

  - sex: sex column name ("Male"/"Female" or m/f; case-insensitive)

- verbose:

  Logical; if TRUE, emits progress messages.

- na_action:

  One of c("keep","omit","error","ignore","warn").

- check_extreme:

  Logical; if TRUE, scan inputs for plausible ranges.

- extreme_action:

  One of c("warn","cap","error","ignore","NA").

- extreme_rules:

  Optional list with bounds for `alm` and `bmi` (defaults: alm = c(5,
  40), bmi = c(10, 60)).

## Value

A tibble with:

- alm_bmi_ratio (numeric)

- low_muscle_mass (logical; TRUE if below sex-specific cut-point; NA if
  sex unknown or ratio NA)

## Details

ALM/BMI reflects muscle mass relative to body size. FNIH cut-points:

- Men: ALM/BMI \< 0.789

- Women: ALM/BMI \< 0.512

ALM should be in kilograms and BMI in kg/m^2.

## References

McLean RR, Shardell MD, Alley DE, et al. (2014). “Criteria for
clinically relevant weakness and low lean mass: FNIH Sarcopenia
Project.” *Journal of Gerontology A: Biological Sciences and Medical
Sciences*, **69**(5), 576–583.
[doi:10.1093/gerona/glu012](https://doi.org/10.1093/gerona/glu012) . ;
Studenski SA, Peters KW, Alley DE, et al. (2014). “The FNIH Sarcopenia
Project: Rationale, Study Description, Conference Recommendations, and
Final Estimates.” *Journal of Gerontology A: Biological Sciences and
Medical Sciences*, **69**(5), 564–570.
[doi:10.1093/gerona/glu010](https://doi.org/10.1093/gerona/glu010) .

## Examples

``` r
df <- data.frame(ALM_kg = c(7.2, 5.8, 6.5), BMI = c(24, 28, 22),
                 Sex = c("male", "female", "male"))
alm_bmi_index(df)
#> # A tibble: 3 × 2
#>   alm_bmi_ratio low_muscle_mass
#>           <dbl> <lgl>          
#> 1         0.3   TRUE           
#> 2         0.207 TRUE           
#> 3         0.295 TRUE           
```

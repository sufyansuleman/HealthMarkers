# Obesity Indices

## Scope

Compute anthropometric adiposity/shape indices (BMI + category,
WHR/WHtR, AVI, BAI, ABSI, BRI, Conicity Index, waist/BMI and
weight/height ratios, optional WHRadjBMI and RFM) with unit conversion,
NA/extreme handling, and safe-division guards.

## When to use

- You have weight/height/waist/hip (and optionally sex) and want a
  comprehensive set of body composition proxies in one call.
- You need built-in unit conversion (kg/lb, cm/m) and NA policy
  controls.
- You want optional BMI-adjusted WHR and Relative Fat Mass when sex
  coding is available.

## Inputs

- `data`: data frame/tibble with numeric columns for weight, height,
  waist, hip; `sex` needed only if `include_RFM = TRUE` (coded 0=male,
  1=female).
- Column arguments are unquoted: `weight`, `height`, `waist`, `hip`,
  optional `sex`.
- Units: weight kg (or lb if `weight_unit = "lb"`), height m (or cm if
  `height_unit = "cm"`), waist/hip cm.
- `weight_unit` / `height_unit`: convert to kg/m internally.
- `adjust_WHR`: add BMI-adjusted WHR residuals.
- `include_RFM`: compute Relative Fat Mass (requires `sex`).
- `na_action`: `keep` (default) propagates NA; `omit` drops rows missing
  required inputs; `error` aborts when required inputs contain NA.
- `na_warn_prop`: threshold for high-missingness debug messages (default
  0.2).
- `verbose`: optional progress and summary logging.

## Quick start

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  wt    = c(70, 80),   # kg
  ht    = c(175, 165), # cm
  waist = c(80, 90),   # cm
  hip   = c(100, 95),  # cm
  sex   = c(0, 1)
)

obesity_indices(
  data = df,
  weight = wt,
  height = ht,
  waist = waist,
  hip = hip,
  sex = sex,
  weight_unit = "kg",
  height_unit = "cm",
  adjust_WHR = TRUE,
  include_RFM = TRUE,
  na_action = "keep",
  verbose = FALSE
)
#> # A tibble: 2 × 15
#>   weight_kg height_m   BMI BMI_cat         WHR WHRadjBMI waist_to_height_ratio
#>       <dbl>    <dbl> <dbl> <chr>         <dbl>     <dbl>                 <dbl>
#> 1        70     1.75  22.9 Normal weight 0.8           0                  45.7
#> 2        80     1.65  29.4 Overweight    0.947         0                  54.5
#> # ℹ 8 more variables: waist_to_BMI_ratio <dbl>, weight_to_height_ratio <dbl>,
#> #   AVI <dbl>, BAI <dbl>, ABSI <dbl>, BRI <dbl>, CI <dbl>, RFM <dbl>
```

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
obesity_indices(
  data = df,
  weight = wt,
  height = ht,
  waist = waist,
  hip = hip,
  sex = sex,
  weight_unit = "kg",
  height_unit = "cm",
  adjust_WHR = FALSE,
  include_RFM = TRUE,
  na_action = "omit",
  verbose = TRUE
)
#> obesity_indices(): reading input 'df' — 2 rows × 5 variables
#> obesity_indices(): col_map: weight -> 'wt', height -> 'ht', waist -> 'waist', hip -> 'hip', sex -> 'sex'
#> obesity_indices(): computing markers:
#>   BMI, BMI_cat, WHR, WHtR, AVI, BAI, ABSI, BRI, CI, RFM
#> obesity_indices(): omitting 0 rows with NA in required inputs
#> obesity_indices(): results: weight_kg 2/2, height_m 2/2, BMI 2/2, BMI_cat 2/2, WHR 2/2, waist_to_height_ratio 2/2, waist_to_BMI_ratio 2/2, weight_to_height_ratio 2/2, AVI 2/2, BAI 2/2, ABSI 2/2, BRI 2/2, CI 2/2, RFM 2/2
#> # A tibble: 2 × 14
#>   weight_kg height_m   BMI BMI_cat         WHR waist_to_height_ratio
#>       <dbl>    <dbl> <dbl> <chr>         <dbl>                 <dbl>
#> 1        70     1.75  22.9 Normal weight 0.8                    45.7
#> 2        80     1.65  29.4 Overweight    0.947                  54.5
#> # ℹ 8 more variables: waist_to_BMI_ratio <dbl>, weight_to_height_ratio <dbl>,
#> #   AVI <dbl>, BAI <dbl>, ABSI <dbl>, BRI <dbl>, CI <dbl>, RFM <dbl>
options(old_opt)
```

## Missing-data policy

``` r
try(
  obesity_indices(
    data = df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    na_action = "error"
  )
)
#> # A tibble: 2 × 13
#>   weight_kg height_m   BMI BMI_cat         WHR waist_to_height_ratio
#>       <dbl>    <dbl> <dbl> <chr>         <dbl>                 <dbl>
#> 1        70     1.75  22.9 Normal weight 0.8                    45.7
#> 2        80     1.65  29.4 Overweight    0.947                  54.5
#> # ℹ 7 more variables: waist_to_BMI_ratio <dbl>, weight_to_height_ratio <dbl>,
#> #   AVI <dbl>, BAI <dbl>, ABSI <dbl>, BRI <dbl>, CI <dbl>
```

## Outputs and expectations

- Adds: `weight_kg`, `height_m`, `BMI`, `BMI_cat`, `WHR`,
  `waist_to_height_ratio`, `AVI`, `BAI`, `ABSI`, `BRI`, `CI`,
  `waist_to_BMI_ratio`, `weight_to_height_ratio`, optional `WHRadjBMI`,
  optional `RFM`.
- Zero denominators yield `NA` with warnings; WHRadjBMI requires
  variance in WHR and BMI; RFM requires sex coded 0/1.

## Tips

- Set weight/height units correctly; waist/hip are assumed cm.
- Enable `include_RFM = TRUE` only with valid sex coding; leave FALSE
  otherwise.
- WHRadjBMI needs variation in WHR and BMI; otherwise it returns `NA`
  with a warning.
- Use `na_action = "omit"` for row-complete outputs; keep during QA to
  see missingness.

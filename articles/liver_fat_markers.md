# Liver Fat Surrogates: HSI and NAFLD-LFS

## Overview

[`liver_fat_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/liver_fat_markers.md)
computes two non-invasive surrogate indexes for hepatic steatosis:

- **HSI** (Hepatic Steatosis Index):
  `8 × (ALT/AST) + BMI + 2 (female) + 2 (diabetes)`
- **NAFLD-LFS** (NAFLD Liver Fat Score): requires metabolic syndrome
  status and fasting insulin

Both indexes are returned as a tibble with columns `HSI` and
`NAFLD_LFS`.

## Minimal example (HSI only)

Only `ALT`, `AST`, and `BMI` are strictly required. Without sex/diabetes
the female and diabetes terms default to 0.

``` r
df <- data.frame(ALT = c(30, 45, 60), AST = c(20, 30, 25), BMI = c(24, 28, 33))

liver_fat_markers(
  df,
  col_map = list(ALT = "ALT", AST = "AST", BMI = "BMI")
)
#> # A tibble: 3 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  36          NA
#> 2  40          NA
#> 3  52.2        NA
```

## With optional inputs

Providing `sex`, `diabetes`, and `insulin` enables the full NAFLD-LFS
formula.

``` r
df2 <- data.frame(
  ALT      = c(30, 45, 60),
  AST      = c(20, 30, 25),
  BMI      = c(24, 28, 33),
  sex      = c("female", "male", "female"),
  diabetes = c(FALSE, TRUE, FALSE),
  insulin  = c(10, 22, 15)
)

liver_fat_markers(
  df2,
  col_map = list(
    ALT      = "ALT",
    AST      = "AST",
    BMI      = "BMI",
    sex      = "sex",
    diabetes = "diabetes",
    insulin  = "insulin"
  )
)
#> # A tibble: 3 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  38          NA
#> 2  42          NA
#> 3  54.2        NA
```

## MetS derivation

When `MetS` is not available directly, the function derives it via
NCEP-ATP III criteria if `waist`, `TG`, `HDL_c`, a blood pressure
column, and `glucose` are provided.

``` r
df3 <- data.frame(
  ALT = 55, AST = 30, BMI = 31,
  sex = "female", diabetes = FALSE, insulin = 18,
  waist = 92, TG = 1.9, HDL_c = 1.1, bp_sys = 135, glucose = 6.0
)

liver_fat_markers(
  df3,
  col_map = list(
    ALT = "ALT", AST = "AST", BMI = "BMI",
    sex = "sex", diabetes = "diabetes", insulin = "insulin",
    waist = "waist", TG = "TG", HDL_c = "HDL_c",
    bp_sys = "bp_sys", glucose = "glucose"
  )
)
#> # A tibble: 1 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  47.7      1.68
```

## Missing-value handling

Use `na_action` to control how rows with missing required values are
treated.

``` r
df_na <- data.frame(ALT = c(30, NA, 50), AST = c(20, 25, 30), BMI = c(25, 28, 31))

# omit: return only complete rows
liver_fat_markers(
  df_na,
  col_map  = list(ALT = "ALT", AST = "AST", BMI = "BMI"),
  na_action = "omit"
)
#> # A tibble: 2 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  37          NA
#> 2  44.3        NA

# keep: return all rows, NA for incomplete ones
liver_fat_markers(
  df_na,
  col_map  = list(ALT = "ALT", AST = "AST", BMI = "BMI"),
  na_action = "keep"
)
#> # A tibble: 3 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  37          NA
#> 2  NA          NA
#> 3  44.3        NA
```

## Verbose diagnostics

Enable step-by-step messages to trace exactly which columns are mapped
and how many rows were computed successfully.

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- data.frame(ALT = c(30, 45), AST = c(20, 30), BMI = c(24, 28),
                   sex = c("male", "female"), diabetes = c(FALSE, TRUE),
                   insulin = c(10, 22))

liver_fat_markers(
  df_v,
  col_map = list(
    ALT = "ALT", AST = "AST", BMI = "BMI",
    sex = "sex", diabetes = "diabetes", insulin = "insulin"
  ),
  verbose = TRUE
)
#> liver_fat_markers(): preparing inputs
#> liver_fat_markers(): column map: ALT -> 'ALT', AST -> 'AST', BMI -> 'BMI'
#> liver_fat_markers(): results: HSI 2/2, NAFLD_LFS 0/2
#> # A tibble: 2 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1    36        NA
#> 2    44        NA

options(old_opt)
```

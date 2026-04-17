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
#> liver_fat_markers(): reading input 'df_v' — 2 rows × 6 variables
#> liver_fat_markers(): col_map (7 columns — 6 specified, 1 inferred from data)
#>   ALT               ->  'ALT'
#>   AST               ->  'AST'
#>   BMI               ->  'BMI'
#>   sex               ->  'sex'
#>   diabetes          ->  'diabetes'
#>   insulin           ->  'insulin'
#>   I0                ->  'insulin'    (inferred)
#> liver_fat_markers(): computing markers:
#>   HSI        [8*(ALT/AST) + BMI + sex + diabetes]
#>   NAFLD_LFS  [MetS/insulin/diabetes required; NA if unavailable]
#> liver_fat_markers(): results: HSI 2/2, NAFLD_LFS 0/2
#> # A tibble: 2 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1    36        NA
#> 2    44        NA

options(old_opt)
```

## Non-standard column names

When columns carry site-specific names, specify the mapping explicitly:

``` r
df_ns <- data.frame(
  alt_IU_L     = c(28, 42, 65, 38),
  ast_IU_L     = c(18, 28, 35, 22),
  bmi_kgm2     = c(23.5, 29.0, 34.2, 27.8),
  gender       = c("female", "male", "female", "male"),
  t2dm_flag    = c(FALSE, FALSE, TRUE, FALSE),
  insulin_uIUmL = c(9, 18, 28, 12)
)

liver_fat_markers(
  df_ns,
  col_map = list(
    ALT      = "alt_IU_L",
    AST      = "ast_IU_L",
    BMI      = "bmi_kgm2",
    sex      = "gender",
    diabetes = "t2dm_flag",
    insulin  = "insulin_uIUmL"
  )
)
#> # A tibble: 4 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  37.9        NA
#> 2  41          NA
#> 3  53.1        NA
#> 4  41.6        NA
```

## Extreme values

Extreme ALT or AST values will produce extreme index scores. Pre-filter
implausible inputs before calling.

``` r
df_ext <- data.frame(
  ALT = c(30, 650, 60),  # 650 is implausibly extreme; pre-filter
  AST = c(20,  30, 25),
  BMI = c(24,  28, 33)
)

# Pre-filter before calling
df_ext$ALT[df_ext$ALT > 500] <- NA

liver_fat_markers(
  df_ext,
  col_map = list(ALT = "ALT", AST = "AST", BMI = "BMI")
)
#> liver_fat_markers(): reading input 'df_ext' — 3 rows × 3 variables
#> liver_fat_markers(): col_map (3 columns — 3 specified)
#>   ALT               ->  'ALT'
#>   AST               ->  'AST'
#>   BMI               ->  'BMI'
#> liver_fat_markers(): computing markers:
#>   HSI        [8*(ALT/AST) + BMI + sex + diabetes]
#>   NAFLD_LFS  [MetS/insulin/diabetes required; NA if unavailable]
#> liver_fat_markers(): results: HSI 2/3, NAFLD_LFS 0/3
#> # A tibble: 3 × 2
#>     HSI NAFLD_LFS
#>   <dbl>     <dbl>
#> 1  36          NA
#> 2  NA          NA
#> 3  52.2        NA
```

## Using real cohort data

The bundled simulated dataset has `ALT`, `AST`, `BMI`, and `sex`
columns, enabling HSI (and partial NAFLD-LFS) computation with no column
renaming:

``` r
sim_path  <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim_small <- dplyr::slice_head(readRDS(sim_path), n = 50)

lf_sim <- liver_fat_markers(
  sim_small,
  col_map   = list(ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex"),
  na_action = "keep"
)

dplyr::bind_cols(dplyr::select(sim_small, ALT, AST, BMI), lf_sim) |> head()
#>   ALT AST  BMI   id      HSI NAFLD_LFS
#> 1  86  25 32.0 P001 59.52000  8.066744
#> 2  80  27 29.7 P002 55.40370  2.147750
#> 3  93  49 27.0 P003 44.18367  7.814731
#> 4  43  73 26.2 P004 32.91233  6.474186
#> 5  95  18 24.1 P005 68.32222 10.746895
#> 6  64  25 36.6 P006 59.08000  4.837813
```

> **Column recognition tip:** Run `hm_col_report(your_data)` to check
> which liver enzyme and metabolic columns are auto-detected before
> building your `col_map`. See the [Multi-Biobank
> Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
> article for recognised synonyms across major biobanks (e.g.,
> `alanine_aminotransferase`, `ALT_UL`, `ALAT`).

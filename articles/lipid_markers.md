# lipid_markers

## Scope

Compute lipid ratios and composite indices (non-HDL-C, remnant-C,
TC/HDL, TG/HDL, LDL/HDL, ApoB/ApoA1, VAI, LAP, TyG-BMI) with optional
LDL estimation and NA/extreme handling.

## Load packages and data

``` r
library(HealthMarkers)
library(tibble)

sim <- readRDS(system.file("extdata/simulated_hm_data.rds", package = "HealthMarkers"))
lipids <- sim |>
  dplyr::slice_head(n = 30) |>
  dplyr::mutate(glucose = G0) |>
  dplyr::select(TC, HDL_c, TG, LDL_c, ApoB, ApoA1, waist, BMI, glucose)
```

## Column map

- Required: `TC`, `HDL_c`, `TG` (mmol/L).
- Optional when present/mapped: `LDL_c` (mmol/L), `ApoB`, `ApoA1`,
  `waist` (cm), `BMI` (kg/m^2), `glucose` (mmol/L; needed for TyG-BMI).
- Use `col_map` to rename; numeric-like inputs are coerced and
  non-finite set to `NA` before policies.

## Core calculation

``` r
lm_out <- lipid_markers(
  data = lipids,
  col_map = list(
    TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c",
    ApoB = "ApoB", ApoA1 = "ApoA1", waist = "waist", BMI = "BMI", glucose = "glucose"
  ),
  na_action = "keep",
  check_extreme = FALSE,
  verbose = FALSE
)
lm_out |> dplyr::slice_head(n = 5)
#> # A tibble: 5 × 11
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1 VAI_Men
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>   <dbl>
#> 1      2.81    -0.171         2.83        0.693          1.94      0.473   0.689
#> 2      4.41     1.81          3.95        1.49           1.74      0.637   2.08 
#> 3      3.31     0.960         3.70        0.952          1.92      0.392   1.28 
#> 4      3.72    -0.198         4.92        2.04           4.12      0.804   2.67 
#> 5      4.03     0.789         4.09        1.42           2.48      0.516   2.08 
#> # ℹ 4 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>,
#> #   TyG_BMI <dbl>
```

## Missing data & extremes

- `na_action`: `keep/ignore` retain rows with missing inputs (derived
  outputs become `NA`); `omit` drops them; `error` aborts; `warn`
  retains with warnings. `na_warn_prop` flags high missingness when
  warning.
- `check_extreme` with `extreme_action` (`warn`, `cap`, `NA`, `error`,
  `ignore`) scans broad defaults (TC/HDL/TG/LDL 0-50 mmol/L; ApoB/ApoA1
  0-10; waist 30-250 cm; BMI 10-80 kg/m^2; glucose 0-50 mmol/L).
  Override via `extreme_rules`.
- If `LDL_c` is absent, LDL is estimated via Friedewald (TC - HDL -
  TG/5) when TG is available.

## Expectations

- Outputs: `non_HDL_c`, `remnant_c`, `ratio_TC_HDL`, `ratio_TG_HDL`,
  `ratio_LDL_HDL`, `ApoB_ApoA1` (needs ApoB & ApoA1), `VAI_Men/Women`
  (needs waist & BMI), `LAP_Men/Women` (needs waist), `TyG_BMI` (needs
  glucose & BMI).
- Units follow inputs; TyG-BMI converts TG/glucose to mg/dL internally.

## Tips

- Provide waist and BMI to enable VAI/LAP; supply glucose and BMI to
  enable TyG-BMI.
- Use `check_extreme = TRUE` with `cap` to bound implausible lipid or
  anthropometric values.
- Set `na_action = "omit"` when you need complete-case derived lipid
  indices.

## Missing-data policy

``` r
missing <- lipids
missing$TG[4] <- NA

lipid_markers(
  data = missing,
  col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c", BMI = "BMI"),
  na_action = "omit"
)
#> # A tibble: 29 × 7
#>    non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#>        <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>
#>  1      2.81   -0.171          2.83        0.693          1.94         NA
#>  2      4.41    1.81           3.95        1.49           1.74         NA
#>  3      3.31    0.960          3.70        0.952          1.92         NA
#>  4      4.03    0.789          4.09        1.42           2.48         NA
#>  5      3.88    0.668          3.97        0.678          2.46         NA
#>  6      3.34    0.0349         3.09        1.51           2.07         NA
#>  7      4.80    1.20           5.23        1.58           3.17         NA
#>  8      4.03    0.623          3.95        1.66           2.49         NA
#>  9      2.83   -0.521          2.82        1.08           2.15         NA
#> 10      5.17    3.06           4.97        0.962          1.62         NA
#> # ℹ 19 more rows
#> # ℹ 1 more variable: TyG_BMI <dbl>

lipid_markers(
  data = missing,
  col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c", BMI = "BMI"),
  na_action = "warn"
)
#> # A tibble: 30 × 7
#>    non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#>        <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>
#>  1      2.81   -0.171          2.83        0.693          1.94         NA
#>  2      4.41    1.81           3.95        1.49           1.74         NA
#>  3      3.31    0.960          3.70        0.952          1.92         NA
#>  4      3.72   -0.198          4.92       NA              4.12         NA
#>  5      4.03    0.789          4.09        1.42           2.48         NA
#>  6      3.88    0.668          3.97        0.678          2.46         NA
#>  7      3.34    0.0349         3.09        1.51           2.07         NA
#>  8      4.80    1.20           5.23        1.58           3.17         NA
#>  9      4.03    0.623          3.95        1.66           2.49         NA
#> 10      2.83   -0.521          2.82        1.08           2.15         NA
#> # ℹ 20 more rows
#> # ℹ 1 more variable: TyG_BMI <dbl>
```

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
lipid_markers(
  data    = dplyr::slice_head(lipids, n = 5),
  col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG",
                 LDL_c = "LDL_c", ApoB = "ApoB", ApoA1 = "ApoA1",
                 waist = "waist", BMI = "BMI"),
  verbose = TRUE
)
#> lipid_markers(): preparing inputs
#> lipid_markers(): column map: TC -> 'TC', HDL_c -> 'HDL_c', TG -> 'TG'
#> lipid_markers(): results: non_HDL_c 5/5, remnant_c 5/5, ratio_TC_HDL 5/5, ratio_TG_HDL 5/5, ratio_LDL_HDL 5/5, ApoB_ApoA1 5/5, VAI_Men 5/5, VAI_Women 5/5, LAP_Men 5/5, LAP_Women 5/5, TyG_BMI 5/5
#> # A tibble: 5 × 11
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1 VAI_Men
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>   <dbl>
#> 1      2.81    -0.171         2.83        0.693          1.94      0.473   0.689
#> 2      4.41     1.81          3.95        1.49           1.74      0.637   2.08 
#> 3      3.31     0.960         3.70        0.952          1.92      0.392   1.28 
#> 4      3.72    -0.198         4.92        2.04           4.12      0.804   2.67 
#> 5      4.03     0.789         4.09        1.42           2.48      0.516   2.08 
#> # ℹ 4 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>,
#> #   TyG_BMI <dbl>
options(old_opt)
```

## Tips

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
#> 1      4.47     0.409         5.95        1.07           4.50      0.586   1.36 
#> 2      3.34     0.230         4.34        1.86           3.11      1.00    2.22 
#> 3      4.15     1.44          3.68        0.797          1.75      0.740   0.953
#> 4      4.11     1.14          5.06        1.59           2.93      0.603   2.46 
#> 5      3.32     0.253         3.09        1.01           1.93      0.474   1.14 
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
#>  1      4.47     0.409         5.95        1.07           4.50         NA
#>  2      3.34     0.230         4.34        1.86           3.11         NA
#>  3      4.15     1.44          3.68        0.797          1.75         NA
#>  4      3.32     0.253         3.09        1.01           1.93         NA
#>  5      3.57    -0.279         3.56        1.59           2.77         NA
#>  6      3.88    -0.204         4.45        2.06           3.63         NA
#>  7      4.94     1.90          5.37        1.58           2.69         NA
#>  8      4.85     2.03          4.36        1.67           1.95         NA
#>  9      4.65     2.16          4.73        1.04           2.00         NA
#> 10      3.67     1.66          3.94        1.49           1.61         NA
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
#>  1      4.47     0.409         5.95        1.07           4.50         NA
#>  2      3.34     0.230         4.34        1.86           3.11         NA
#>  3      4.15     1.44          3.68        0.797          1.75         NA
#>  4      4.11     1.14          5.06       NA              2.93         NA
#>  5      3.32     0.253         3.09        1.01           1.93         NA
#>  6      3.57    -0.279         3.56        1.59           2.77         NA
#>  7      3.88    -0.204         4.45        2.06           3.63         NA
#>  8      4.94     1.90          5.37        1.58           2.69         NA
#>  9      4.85     2.03          4.36        1.67           1.95         NA
#> 10      4.65     2.16          4.73        1.04           2.00         NA
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
#> 1      4.47     0.409         5.95        1.07           4.50      0.586   1.36 
#> 2      3.34     0.230         4.34        1.86           3.11      1.00    2.22 
#> 3      4.15     1.44          3.68        0.797          1.75      0.740   0.953
#> 4      4.11     1.14          5.06        1.59           2.93      0.603   2.46 
#> 5      3.32     0.253         3.09        1.01           1.93      0.474   1.14 
#> # ℹ 4 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>,
#> #   TyG_BMI <dbl>
options(old_opt)
```

## Tips

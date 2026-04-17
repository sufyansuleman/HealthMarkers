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
  verbose = FALSE
)
lm_out |> dplyr::slice_head(n = 5)
#> # A tibble: 5 × 11
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1 VAI_Men
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>   <dbl>
#> 1      5.14     1.43          4.95        2.42           2.85      0.690   3.04 
#> 2      4.58     0.33          4.75        0.590          3.48      0.412   0.783
#> 3      5.7      0.49          5.52        0.849          4.13      0.992   0.991
#> 4      2.97     0.420         3.02        0.626          1.73      1.08    0.815
#> 5      3.12     0.75          2.96        1.03           1.49      0.674   1.10 
#> # ℹ 4 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>,
#> #   TyG_BMI <dbl>
```

## Non-standard column names

When your data frame uses different column names, supply a `col_map` to
redirect any key that differs. Keys you omit are auto-inferred from
common synonym patterns (e.g. `total_chol`, `total_cholesterol`, `tc`
all resolve to `TC`).

``` r
# Columns use non-standard names for weight and height
df_mapped <- data.frame(
  TC = 5.0, HDL_c = 1.3, TG = 1.5, LDL_c = 3.0,
  body_weight = 80,    # kg
  body_height = 175,   # cm
  waist = 92,
  glucose = 5.8
)
lm_mapped <- lipid_markers(
  df_mapped,
  col_map = list(weight = "body_weight", height = "body_height"),
  verbose = FALSE
)
# BMI was derived from body_weight / body_height; VAI and TyG-BMI are now available
lm_mapped[, c("VAI_Men", "TyG_BMI")]
#> # A tibble: 1 × 2
#>   VAI_Men TyG_BMI
#>     <dbl>   <dbl>
#> 1    1.52    231.
```

Only `weight` and `height` had to be overridden; `TC`, `HDL_c`, `TG`,
`LDL_c`, `waist`, and `glucose` were matched by the built-in synonym
dictionary.

## Missing data

- `na_action`: `keep/ignore` retain rows with missing inputs (derived
  outputs become `NA`); `omit` drops them; `error` aborts; `warn`
  retains with warnings. `na_warn_prop` flags high missingness when
  warning.
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
#> # A tibble: 29 × 11
#>    non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#>        <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>
#>  1      5.14     1.43          4.95        2.42           2.85      0.690
#>  2      4.58     0.33          4.75        0.590          3.48      0.412
#>  3      5.7      0.49          5.52        0.849          4.13      0.992
#>  4      3.12     0.75          2.96        1.03           1.49      0.674
#>  5      4        0.58          3.38        0.762          2.04      0.451
#>  6      4.27     2.13          3.70        2.96           1.35      0.383
#>  7      4.79     0.730         5.44        1.48           3.76      1.16 
#>  8      2.83     1.32          2.97        2.02           1.05      1.08 
#>  9      5.2      1.05          4.82        1.70           3.05      0.986
#> 10      4.47     0.99          4.89        1.90           3.03      0.876
#> # ℹ 19 more rows
#> # ℹ 5 more variables: VAI_Men <dbl>, VAI_Women <dbl>, LAP_Men <dbl>,
#> #   LAP_Women <dbl>, TyG_BMI <dbl>

lipid_markers(
  data = missing,
  col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c", BMI = "BMI"),
  na_action = "warn"
)
#> # A tibble: 30 × 11
#>    non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1
#>        <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>
#>  1      5.14     1.43          4.95        2.42           2.85      0.690
#>  2      4.58     0.33          4.75        0.590          3.48      0.412
#>  3      5.7      0.49          5.52        0.849          4.13      0.992
#>  4      2.97     0.420         3.02       NA              1.73      1.08 
#>  5      3.12     0.75          2.96        1.03           1.49      0.674
#>  6      4        0.58          3.38        0.762          2.04      0.451
#>  7      4.27     2.13          3.70        2.96           1.35      0.383
#>  8      4.79     0.730         5.44        1.48           3.76      1.16 
#>  9      2.83     1.32          2.97        2.02           1.05      1.08 
#> 10      5.2      1.05          4.82        1.70           3.05      0.986
#> # ℹ 20 more rows
#> # ℹ 5 more variables: VAI_Men <dbl>, VAI_Women <dbl>, LAP_Men <dbl>,
#> #   LAP_Women <dbl>, TyG_BMI <dbl>
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
#> lipid_markers(): reading input 'data' — 5 rows × 9 variables
#> lipid_markers(): col_map (9 columns — 8 specified, 1 inferred from data)
#>   TC                ->  'TC'
#>   HDL_c             ->  'HDL_c'
#>   TG                ->  'TG'
#>   LDL_c             ->  'LDL_c'
#>   ApoB              ->  'ApoB'
#>   ApoA1             ->  'ApoA1'
#>   waist             ->  'waist'
#>   BMI               ->  'BMI'
#>   glucose           ->  'glucose'    (inferred)
#> lipid_markers(): optional inputs
#>   present:  ApoB, ApoA1, waist, BMI, glucose
#> lipid_markers(): range note (informational, values not altered):
#>   TG: 5 value(s) outside plausible range
#> lipid_markers(): computing markers:
#>   non_HDL_c     [TC, HDL_c]
#>   remnant_c     [TC, HDL_c, LDL_c]
#>   ratio_TC_HDL  [TC, HDL_c]
#>   ratio_TG_HDL  [TG, HDL_c]
#>   ratio_LDL_HDL [LDL_c, HDL_c]
#>   ApoB_ApoA1    [ApoB, ApoA1]
#>   VAI_Men/Women [waist, BMI, TG, HDL_c]
#>   LAP_Men/Women [waist, TG]
#>   TyG_BMI       [TG, glucose, BMI]
#> lipid_markers(): results: non_HDL_c 5/5, remnant_c 5/5, ratio_TC_HDL 5/5, ratio_TG_HDL 5/5, ratio_LDL_HDL 5/5, ApoB_ApoA1 5/5, VAI_Men 5/5, VAI_Women 5/5, LAP_Men 5/5, LAP_Women 5/5, TyG_BMI 5/5
#> # A tibble: 5 × 11
#>   non_HDL_c remnant_c ratio_TC_HDL ratio_TG_HDL ratio_LDL_HDL ApoB_ApoA1 VAI_Men
#>       <dbl>     <dbl>        <dbl>        <dbl>         <dbl>      <dbl>   <dbl>
#> 1      5.14     1.43          4.95        2.42           2.85      0.690   3.04 
#> 2      4.58     0.33          4.75        0.590          3.48      0.412   0.783
#> 3      5.7      0.49          5.52        0.849          4.13      0.992   0.991
#> 4      2.97     0.420         3.02        0.626          1.73      1.08    0.815
#> 5      3.12     0.75          2.96        1.03           1.49      0.674   1.10 
#> # ℹ 4 more variables: VAI_Women <dbl>, LAP_Men <dbl>, LAP_Women <dbl>,
#> #   TyG_BMI <dbl>
options(old_opt)
```

## Tips

# 

title: “Hormone marker ratios” output: rmarkdown::html_vignette
vignette: \> % % % —

## Scope

Compute nine hormone ratios with built-in QA: `FAI`, `LH_FSH`, `E2_P`,
`T3_T4`, `ARR`, `Ins_Glu`, `GH_IGF1`, `PRL_T`, and `CAR_slope`. Inputs
are coerced to numeric, non-finite become `NA`, and optional extreme
screening can warn, cap, blank, or error on out-of-range labs.

## When to use

- You have a complete hormone panel (mapped via `col_map`) and need
  quick ratio features.
- You want explicit NA handling and optional extreme-value screening to
  avoid crashes on bad labs.
- You need outputs that preserve row counts unless you explicitly `omit`
  missing rows.

## Inputs and requirements

- All keys are required: total testosterone, SHBG, LH, FSH, estradiol,
  progesterone, free T3, free T4, aldosterone, renin, insulin, glucagon,
  GH, IGF1, prolactin, cortisol_0, cortisol_30.
- `col_map`: map each required key to your column names; missing
  mappings or columns abort.
- Units: keep units consistent within each ratio (e.g., cortisol nmol/L;
  aldosterone/renin in matching units).
- `na_action`: `keep`/`ignore` retain rows with NA outputs; `omit` drops
  rows with missing required inputs; `error` aborts.
- `check_extreme`: optional; `extreme_action` =
  `warn`/`cap`/`NA`/`error`/`ignore`.

## Load packages and demo data

Use a small synthetic tibble (covers all required inputs) so the chunk
runs anywhere. Replace `horm_demo` with your lab data.

``` r
library(HealthMarkers)
library(tibble)

horm_demo <- tibble::tibble(
  tt = c(5.0, 7.0, 4.5, 6.2),
  shbg = c(30, 40, 28, 35),
  lh = c(6, 7, 5, 6),
  fsh = c(8, 9, 7, 8),
  e2 = c(120, 140, 110, 135),
  p4 = c(2, 3, 1.8, 2.5),
  ft3 = c(4.5, 4.8, 4.3, 4.6),
  ft4 = c(16, 15, 17, 16),
  aldo = c(150, 120, 140, 130),
  renin = c(12, 10, 11, 9),
  ins = c(15, 20, 18, 17),
  gluc = c(80, 70, 75, 78),
  gh = c(1.2, 0.9, 1.0, 1.1),
  igf1 = c(180, 160, 170, 165),
  prl = c(10, 12, 11, 9),
  cort0 = c(300, 280, 290, 310),
  cort30 = c(480, 450, 470, 500)
)
```

## Column map (required)

Map every required key to your column names; all are mandatory for
computation.

- `total_testosterone`, `SHBG`
- `LH`, `FSH`
- `estradiol`, `progesterone`
- `free_T3`, `free_T4`
- `aldosterone`, `renin`
- `insulin`, `glucagon`
- `GH`, `IGF1`
- `prolactin`
- `cortisol_0`, `cortisol_30` (nmol/L; slope uses the 30-minute delta /
  30)

``` r
col_map <- list(
  total_testosterone = "tt",
  SHBG = "shbg",
  LH = "lh",
  FSH = "fsh",
  estradiol = "e2",
  progesterone = "p4",
  free_T3 = "ft3",
  free_T4 = "ft4",
  aldosterone = "aldo",
  renin = "renin",
  insulin = "ins",
  glucagon = "gluc",
  GH = "gh",
  IGF1 = "igf1",
  prolactin = "prl",
  cortisol_0 = "cort0",
  cortisol_30 = "cort30"
)
```

## Core calculation

Compute all ratios and preview the outputs. Non-finite values are set to
`NA`; rows are retained when `na_action = "keep"`/`"ignore"`.

``` r
hm_out <- hormone_markers(
  data = horm_demo,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  verbose = FALSE
)

hm_out
#> # A tibble: 4 × 9
#>     FAI LH_FSH  E2_P T3_T4   ARR Ins_Glu GH_IGF1 PRL_T CAR_slope
#>   <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>     <dbl>
#> 1  16.7  0.75   60   0.281  12.5   0.188 0.00667  2         6   
#> 2  17.5  0.778  46.7 0.32   12     0.286 0.00562  1.71      5.67
#> 3  16.1  0.714  61.1 0.253  12.7   0.24  0.00588  2.44      6   
#> 4  17.7  0.75   54   0.288  14.4   0.218 0.00667  1.45      6.33
```

## Missing data and row handling

Show the difference between keeping rows (propagating `NA`) and omitting
rows when required inputs are missing.

``` r
horm_demo_na <- horm_demo
horm_demo_na$ft3[2] <- NA

keep_res <- hormone_markers(
  data = horm_demo_na,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE
)

omit_res <- hormone_markers(
  data = horm_demo_na,
  col_map = col_map,
  na_action = "omit",
  check_extreme = FALSE
)

list(keep_rows = nrow(keep_res), omit_rows = nrow(omit_res))
#> $keep_rows
#> [1] 4
#> 
#> $omit_rows
#> [1] 3
```

## Extreme screening

Enable range scanning and cap out-of-range inputs into allowed bounds.

``` r
horm_demo_ext <- horm_demo
horm_demo_ext$aldo[1] <- 2000  # intentionally extreme

cap_res <- hormone_markers(
  data = horm_demo_ext,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)

cap_res[1, c("ARR", "FAI")]
#> # A tibble: 1 × 2
#>     ARR   FAI
#>   <dbl> <dbl>
#> 1  83.3  16.7
```

`extreme_action = "cap"` trims inputs into the allowed ranges; `warn` or
`error` emit warnings or abort instead.

## Expectations

- All required inputs must be mapped and present; missing
  mappings/columns abort.
- `na_action`: `keep`/`ignore` retains rows with `NA` outputs; `omit`
  drops rows with missing required inputs; `error` stops on missingness.
- Extreme screening is optional; when enabled, choose how to handle
  flagged values with `extreme_action`.
- Outputs are numeric ratios; divisions by zero/non-finite are converted
  to `NA`.

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, the column
map, and a results summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
df_v <- data.frame(
  total_testosterone = 10, SHBG = 2, LH = 8, FSH = 4,
  estradiol = 100, progesterone = 50, free_T3 = 5, free_T4 = 10,
  aldosterone = 20, renin = 4, insulin = 20, glucagon = 10,
  GH = 2, IGF1 = 4, prolactin = 12, cortisol_0 = 200, cortisol_30 = 260
)
hormone_markers(df_v,
  col_map = setNames(as.list(names(df_v)), names(df_v)),
  verbose = TRUE
)
#> hormone_markers(): preparing inputs
#> hormone_markers(): column map: total_testosterone -> 'total_testosterone', SHBG -> 'SHBG', LH -> 'LH', FSH -> 'FSH', estradiol -> 'estradiol', progesterone -> 'progesterone', free_T3 -> 'free_T3', free_T4 -> 'free_T4', aldosterone -> 'aldosterone', renin -> 'renin', insulin -> 'insulin', glucagon -> 'glucagon', GH -> 'GH', IGF1 -> 'IGF1', prolactin -> 'prolactin', cortisol_0 -> 'cortisol_0', cortisol_30 -> 'cortisol_30'
#> hormone_markers(): results: FAI 1/1, LH_FSH 1/1, E2_P 1/1, T3_T4 1/1, ARR 1/1, Ins_Glu 1/1, GH_IGF1 1/1, PRL_T 1/1, CAR_slope 1/1
#> # A tibble: 1 × 9
#>     FAI LH_FSH  E2_P T3_T4   ARR Ins_Glu GH_IGF1 PRL_T CAR_slope
#>   <dbl>  <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>     <dbl>
#> 1   500      2     2   0.5     5       2     0.5   1.2         2
options(old_opt)
```

## Tips

- Keep units consistent across rows (e.g., cortisol nmol/L,
  aldosterone/renin in matching units) to make ratios meaningful.
- Use `na_action = "omit"` when you prefer complete-case ratios;
  keep/ignore if you want row counts preserved.
- Tighten `extreme_rules` if your lab reference ranges are narrower than
  the broad defaults.
- Turn on `verbose = TRUE` to see coercion, missingness scan, and
  extreme handling summaries.

# ALM/BMI Index

## Scope

Compute ALM/BMI and flag low muscle mass using FNIH cut-points (men \<
0.789; women \< 0.512) with optional NA and extreme handling.

## When to use this

- You need a quick sarcopenia risk flag based on ALM relative to body
  size.
- You have ALM (kg), BMI (kg/m^2), and sex, and want FNIH cut-points
  applied automatically.
- You want the option to cap implausible ALM/BMI values and control
  missing-data policy.

## What you need (rich guide)

- Required columns: `alm` (kg), `bmi` (kg/m^2), `sex` (m/f/1/0; other
  values yield `NA` for the flag).
- Defaults: ALM in kg, BMI in kg/m^2. No unit conversion is performed.
- Policies: `na_action` (`keep`/`warn`/`ignore` behave like keep; `omit`
  drops; `error` aborts). `check_extreme` + `extreme_action` can
  cap/NA/error on implausible ALM/BMI.
- FNIH cut-points are built in; nothing to configure beyond column
  mapping.

## Load packages and data

Use a small subset of the simulated data. Here we derive a simple ALM
proxy for illustration.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)

# Example proxy for ALM (replace with measured ALM when available)
sim_small <- sim_small %>%
  mutate(ALM_kg = pmax(10, pmin(35, weight * 0.35)))
```

## Column map

Map your columns; sex is normalized by first letter (m/f/1/0).

``` r
col_map <- list(
  alm = "ALM_kg",
  bmi = "BMI",
  sex = "sex"
)
```

## Walkthrough (compute ALM/BMI and flag)

``` r
alm_out <- alm_bmi_index(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  verbose = FALSE
)

alm_out %>%
  slice_head(n = 5) %>%
  select(alm_bmi_ratio, low_muscle_mass)
#> # A tibble: 5 × 2
#>   alm_bmi_ratio low_muscle_mass
#>           <dbl> <lgl>          
#> 1         0.929 FALSE          
#> 2         0.964 FALSE          
#> 3         1.16  FALSE          
#> 4         0.941 FALSE          
#> 5         1.00  FALSE
```

Interpretation: `alm_bmi_ratio` is ALM/BMI. `low_muscle_mass` is `TRUE`
if below the sex-specific FNIH cut-point; `NA` if sex or ratio is `NA`.

## Missing data and extremes

Compare NA handling and optional capping.

``` r
demo <- sim_small[1:8, c("ALM_kg", "BMI", "sex")]
demo$ALM_kg[3] <- NA
demo$BMI[4] <- 2    # implausible, will be capped

alm_keep <- alm_bmi_index(
  data = demo,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  extreme_rules = list(alm = c(5, 40), bmi = c(10, 60)),
  verbose = FALSE
)

alm_omit <- alm_bmi_index(
  data = demo,
  col_map = col_map,
  na_action = "omit",
  check_extreme = TRUE,
  extreme_action = "cap",
  extreme_rules = list(alm = c(5, 40), bmi = c(10, 60)),
  verbose = FALSE
)

list(
  keep_rows = nrow(alm_keep),
  omit_rows = nrow(alm_omit),
  preview = alm_keep %>% select(alm_bmi_ratio, low_muscle_mass) %>% slice_head(n = 3)
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
#> 
#> $preview
#> # A tibble: 3 × 2
#>   alm_bmi_ratio low_muscle_mass
#>           <dbl> <lgl>          
#> 1         0.929 FALSE          
#> 2         0.964 FALSE          
#> 3        NA     NA
```

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each required key
    (`alm`, `bmi`, `sex`) resolved to. Example:
    `alm_bmi_index(): column map: alm -> 'ALM_kg', bmi -> 'BMI', sex -> 'Sex'`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `alm_bmi_index(): results: alm_bmi_index 30/30, low_alm_bmi_flag 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

df_v <- data.frame(
  ALM_kg = c(14.0, 9.5),
  BMI    = c(24.0, 22.0),
  Sex    = c("male", "female")
)
alm_bmi_index(df_v,
              col_map = list(alm = "ALM_kg", bmi = "BMI", sex = "Sex"),
              verbose  = TRUE)
#> alm_bmi_index(): preparing inputs
#> alm_bmi_index(): column map: alm -> 'ALM_kg', bmi -> 'BMI', sex -> 'Sex'
#> alm_bmi_index(): results: alm_bmi_ratio 2/2, low_muscle_mass 2/2
#> # A tibble: 2 × 2
#>   alm_bmi_ratio low_muscle_mass
#>           <dbl> <lgl>          
#> 1         0.583 TRUE           
#> 2         0.432 TRUE

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Expectations

- Required columns must be mapped; missing columns abort.
- `na_action` controls row handling (`keep`/`warn`/`ignore` behave like
  keep; `omit` drops; `error` aborts).
- `check_extreme` screens raw ALM/BMI; `extreme_action` can
  warn/cap/NA/error; tune `extreme_rules` to your lab limits.
- Sex must normalize to m/f/1/0 for the flag to evaluate; otherwise the
  flag is `NA`.

## Common pitfalls

- Feeding ALM in pounds or BMI in non-metric units will misclassify;
  keep ALM in kg and BMI in kg/m^2.
- A single implausible BMI (e.g., \<=0) zeroes the ratio; enable
  `check_extreme` to cap or NA such inputs.
- Using an ALM proxy (like weight \* 0.35) is illustrative only; use
  measured ALM when possible.
- `na_action = keep` leaves missing inputs as `NA`; choose `omit` for
  stricter pipelines.

## Validation notes

- Spot-check one row: ratio = ALM/BMI; compare to FNIH thresholds (0.789
  men, 0.512 women) to verify `low_muscle_mass`.
- With `check_extreme = TRUE` and `extreme_action = cap`, ALM/BMI will
  be truncated into your specified ranges before the ratio.
- Expect `low_muscle_mass` to be `NA` when sex cannot be normalized.

## See also

- [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md)
  /
  [`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)
  for standardizing BMI or related measures.
- [`frailty_index()`](https://sufyansuleman.github.io/HealthMarkers/reference/frailty_index.md)
  for a broader frailty construct.

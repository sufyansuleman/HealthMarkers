# Adiposity SDS (Sex-Stratified)

## Scope

Sex-stratified SDS (z-scores) for anthropometric measures using separate
male/female references, with NA policies and flexible variable mapping.

## When to use this

- You need sex-specific standardization before modeling or flagging
  outliers.
- Your study has both men and women and you want SDS relative to
  sex-appropriate reference means/SDs.
- You want built-in options for missing data handling and plausibility
  screening.

## What you need (rich guide)

- Required: `sex` (coded M/F or 1/2 after mapping) and at least one
  anthropometric variable present in both `ref$M` and `ref$F`.
- Reference list: `ref$M` and `ref$F` must cover identical variable
  names, each with `mean` and `sd > 0`.
- Units: use the same units in your data as in the reference (e.g., BMI
  kg/m^2, waist cm, height cm).
- Optional: `allow_partial = TRUE` to skip missing variables instead of
  erroring.

## Load packages and data

Use a small slice of the packaged simulated data. Replace `sim_small`
with your data.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)

# Normalize sex labels to M/F and ensure both sexes appear in the demo slice
sim_small <- sim %>%
  mutate(sex = case_when(
    as.character(sex) %in% c("M", "F") ~ as.character(sex),
    sex %in% c("male", "Male", "m") ~ "M",
    sex %in% c("female", "Female", "f") ~ "F",
    sex %in% c(1L, "1") ~ "M",
    sex %in% c(2L, "2") ~ "F",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sex)) %>%
  slice_head(n = 200)
```

## Quick start (self-contained, always runs)

[`adiposity_sds_strat()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)
returns a tibble of **only the `<var>_SDS` columns** — not the original
data. Use
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)
to rejoin.

``` r
# Hard-coded reference values (typical population means/SDs)
ref <- list(
  M = list(BMI = c(mean = 24.5, sd = 3.8), waist = c(mean = 88, sd = 12)),
  F = list(BMI = c(mean = 22.1, sd = 4.2), waist = c(mean = 76, sd = 11))
)

df_demo <- data.frame(
  sex   = c("M", "F", "M", "F"),
  BMI   = c(25.2, 21.8, 27.1, 19.5),
  waist = c(85, 72, 95, 68)
)

col_map <- list(sex = "sex", vars = list(BMI = "BMI", waist = "waist"))

sds_out <- adiposity_sds_strat(df_demo, col_map = col_map, ref = ref)
sds_out
#> # A tibble: 4 × 2
#>   BMI_SDS waist_SDS
#>     <dbl>     <dbl>
#> 1  0.184     -0.25 
#> 2 -0.0714    -0.364
#> 3  0.684      0.583
#> 4 -0.619     -0.727
```

Spot-check row 1 (male): BMI_SDS = (25.2 − 24.5) / 3.8 = 0.1842. Row 2
(female): BMI_SDS = (21.8 − 22.1) / 4.2 = -0.0714.

## Column map

Map sex and each variable to your column names.

``` r
col_map <- list(
  sex = "sex",
  vars = list(
    BMI = "BMI",
    waist = "waist",
    height = "height"
  )
)
```

## Walkthrough with simulated data (build sex-specific refs)

``` r
# Build sex-specific references from the demo slice
ref_sex <- sim_small |>
  group_by(sex) |>
  summarise(
    BMI = list(c(mean = mean(BMI, na.rm = TRUE), sd = sd(BMI, na.rm = TRUE))),
    waist = list(c(mean = mean(waist, na.rm = TRUE), sd = sd(waist, na.rm = TRUE))),
    height = list(c(mean = mean(height, na.rm = TRUE), sd = sd(height, na.rm = TRUE)))
  )

if (!all(c("M", "F") %in% ref_sex$sex)) {
  stop("Demo data slice must contain both M and F; normalize or increase sample.")
}

ref <- list(
  M = list(
    BMI = ref_sex$BMI[[ref_sex$sex == "M"]],
    waist = ref_sex$waist[[ref_sex$sex == "M"]],
    height = ref_sex$height[[ref_sex$sex == "M"]]
  ),
  F = list(
    BMI = ref_sex$BMI[[ref_sex$sex == "F"]],
    waist = ref_sex$waist[[ref_sex$sex == "F"]],
    height = ref_sex$height[[ref_sex$sex == "F"]]
  )
)

asds_strat <- adiposity_sds_strat(
  data = sim_small,
  col_map = col_map,
  ref = ref,
  na_action = "keep",
  allow_partial = FALSE,
  prefix = "",
  verbose = FALSE
)

new_cols <- setdiff(names(asds_strat), names(sim_small))
asds_strat %>%
  slice_head(n = 5) %>%
  select(all_of(new_cols))
```

Interpretation: each `<var>_SDS` is standardized against sex-specific
mean/SD; values near 0 are average for that sex, positive above,
negative below.

## Missing data handling

Compare row handling with NA policy.

``` r
demo <- sim_small[1:8, c("sex", "BMI", "waist", "height")]
demo$BMI[3] <- NA         # missing

demo_keep <- adiposity_sds_strat(
  data = demo,
  col_map = col_map,
  ref = ref,
  na_action = "keep",
  allow_partial = FALSE,
  prefix = ""
)

demo_omit <- adiposity_sds_strat(
  data = demo,
  col_map = col_map,
  ref = ref,
  na_action = "omit",
  allow_partial = FALSE,
  prefix = ""
)

list(
  keep_rows = nrow(demo_keep),
  omit_rows = nrow(demo_omit),
  keep_preview = demo_keep %>% select(ends_with("_SDS")) %>% slice_head(n = 3)
)
```

## Column prefix

Use `prefix` to namespace output columns when combining multiple calls
on the same data frame:

``` r
# Same data as quick-start but prefix with "z_"
sds_prefixed <- adiposity_sds_strat(
  df_demo, col_map = col_map, ref = ref, prefix = "z_"
)
names(sds_prefixed)  # "z_BMI_SDS" "z_waist_SDS"
#> [1] "z_BMI_SDS"   "z_waist_SDS"
```

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each mapped variable
    resolved to. Example:
    `adiposity_sds_strat(): column map: BMI -> 'BMI', waist -> 'waist', sex -> 'sex', ...`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `adiposity_sds_strat(): results: BMI_SDS 28/30, waist_SDS 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level;
`options(healthmarkers.verbose = "inform")` must also be active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")
invisible(adiposity_sds_strat(df_demo, col_map = col_map, ref = ref))
#> adiposity_sds_strat(): reading input 'df_demo' — 4 rows × 3 variables
#> adiposity_sds_strat(): col_map (2 columns — 2 specified)
#>   BMI               ->  'BMI'
#>   waist             ->  'waist'
#> adiposity_sds_strat(): computing markers:
#>   BMI_SDS, waist_SDS
#> adiposity_sds_strat(): results: BMI_SDS 4/4, waist_SDS 4/4
options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## Expectations

- `col_map$sex` must be present; sex must map to M/F (or 1/2) or the
  call errors.
- `ref$M` and `ref$F` must list identical variables with finite `mean`
  and `sd > 0`.
- All mapped columns must exist unless `allow_partial = TRUE`, in which
  case missing variables are skipped with an info message.
- `na_action` controls whether rows with missing inputs are kept,
  dropped, or error; missing values propagate to SDS when kept.
- Numeric coercion happens with warnings if NAs are introduced; align
  your data units with the reference units.

## Common pitfalls

- Sex not normalized to M/F (or 1/2) leads to an error; recode before
  calling.
- References built from a small or biased subset will give unstable SDS;
  use appropriate population means/SDs.
- Unit mismatches (e.g., inches vs cm, lb vs kg) will distort SDS; keep
  units consistent with the reference.
- Forgetting `allow_partial = TRUE` when some variables are absent will
  error; enable it to compute what’s available.

## Validation notes

- Within each sex, SDS should center near 0 with SD near 1 if references
  match the data population.
- Spot-check a variable: `(value - sex_mean)/sex_sd` should match the
  corresponding `<var>_SDS`.

## See also

- [`adiposity_sds()`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md)
  for pooled-sex SDS.
- [`obesity_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/obesity_indices.md)
  and `obesity_metrics()` for related adiposity markers.

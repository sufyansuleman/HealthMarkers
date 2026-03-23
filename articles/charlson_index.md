# Charlson Comorbidity Index

## Scope

Compute the 19-condition Charlson Comorbidity Index (CCI) without age
points. Handles binary indicators, optional bounds screening, and NA
policies.

## When to use

- You have binary comorbidity indicators and need the canonical Charlson
  score (no age points here).
- You want explicit row-retention rules for missing indicators and
  optional bounds checks on 0/1 flags.
- You may need to cap or error on out-of-bounds inputs.

## Requirements checklist

- Packages: HealthMarkers, dplyr (for display).
- Data columns: binary 0/1 indicators for the 19 conditions.
- Column map: provide all 19 keys -\> columns; missing mappings abort.
- Row policy: na_action = keep (default), omit, error; warn/ignore act
  like keep but warn.
- Optional bounds screening: check_extreme + extreme_action
  (warn/cap/error/ignore/NA); defaults use 0-1.

## Load packages and example data

Illustrative-only flags below; replace with your real comorbidity
indicators.

``` r
library(HealthMarkers)
library(dplyr)

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim <- readRDS(sim_path)
sim_small <- dplyr::slice_head(sim, n = 30)

# Illustrative binary flags
set.seed(123)
sim_small <- sim_small %>% mutate(
  mi = rbinom(n(), 1, 0.1),
  chf = rbinom(n(), 1, 0.1),
  pvd = rbinom(n(), 1, 0.1),
  stroke = rbinom(n(), 1, 0.1),
  dementia = rbinom(n(), 1, 0.05),
  copd = rbinom(n(), 1, 0.15),
  rheum = rbinom(n(), 1, 0.05),
  ulcer = rbinom(n(), 1, 0.05),
  mild_liver = rbinom(n(), 1, 0.05),
  diabetes = rbinom(n(), 1, 0.2),
  diab_comp = rbinom(n(), 1, 0.05),
  hemiplegia = rbinom(n(), 1, 0.02),
  renal = rbinom(n(), 1, 0.08),
  cancer = rbinom(n(), 1, 0.08),
  leukemia = rbinom(n(), 1, 0.01),
  lymphoma = rbinom(n(), 1, 0.01),
  sev_liver = rbinom(n(), 1, 0.02),
  metastatic_cancer = rbinom(n(), 1, 0.01),
  hiv = rbinom(n(), 1, 0.01)
)
```

## Map columns

Provide all 19 keys. Missing mappings error.

``` r
col_map <- list(
  mi="mi", chf="chf", pvd="pvd", stroke="stroke", dementia="dementia", copd="copd",
  rheum="rheum", ulcer="ulcer", mild_liver="mild_liver", diabetes="diabetes",
  diab_comp="diab_comp", hemiplegia="hemiplegia", renal="renal", cancer="cancer",
  leukemia="leukemia", lymphoma="lymphoma", sev_liver="sev_liver",
  metastatic_cancer="metastatic_cancer", hiv="hiv"
)
```

## Quick start: compute CCI

Defaults keep rows with missing indicators and return NA scores for
them.

``` r
cci_out <- charlson_index(
  data = sim_small,
  col_map = col_map,
  na_action = "keep",
  check_extreme = FALSE,
  extreme_action = "warn",
  verbose = FALSE
)

head(cci_out)
#> # A tibble: 6 × 1
#>   charlson_index
#>            <int>
#> 1              4
#> 2              1
#> 3              0
#> 4              0
#> 5              1
#> 6              1
```

## Arguments that matter

- col_map: all 19 keys required; missing or empty entries abort.
- na_action: keep (default), omit (drop rows with missing indicators),
  error (abort on missing); warn/ignore behave like keep but warn.
- check_extreme: FALSE by default; TRUE enforces bounds.
- extreme_action: warn (default), cap, error, ignore, NA; default bounds
  0-1 per indicator unless overridden via extreme_rules.
- verbose: emit step messages.

## Handling missing and non-binary inputs

- Non-numeric inputs are coerced; NA introduced are warned. Non-finite
  become NA.
- Missing indicators yield NA scores unless na_action = “omit” or
  “error”.
- Out-of-range (non 0/1) values warn; enable check_extreme to
  cap/error/NA.

### Compare row policies

``` r
demo <- sim_small[1:8, names(col_map)]
demo$renal[3] <- NA

a_keep <- charlson_index(demo, col_map, na_action = "keep", check_extreme = FALSE)
a_omit <- charlson_index(demo, col_map, na_action = "omit", check_extreme = FALSE)

list(
  keep_rows = nrow(a_keep),
  omit_rows = nrow(a_omit),
  preview = head(a_keep$charlson_index)
)
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
#> 
#> $preview
#> [1]  4  1 NA  0  1  1
```

## Bounds screening (optional)

Cap or error on inputs outside chosen ranges.

``` r
demo2 <- demo
demo2$mi[5] <- 2  # out of bounds

a_cap <- charlson_index(
  data = demo2,
  col_map = col_map,
  na_action = "keep",
  check_extreme = TRUE,
  extreme_action = "cap",
  extreme_rules = NULL,
  verbose = FALSE
)

head(a_cap$charlson_index)
#> [1]  4  1 NA  0  1  1
```

## Outputs

- charlson_index integer total
- Rows drop only with na_action = “omit”; na_action = “error” aborts on
  missing.
- Optional bounds handling via check_extreme/extreme_action.

## Pitfalls and tips

- Indicators must be truly binary; non-binary values warn and can be
  capped if screening is on.
- Provide all 19 mappings; missing keys error.
- Age points are not included here; add separately if needed.
- Use extreme_rules to adjust bounds if your inputs differ.

## Validation ideas

- Set all indicators to 0: expected score 0.
- Single metastatic_cancer = 1, all else 0: expected score 6.
- diabetes = 1 and diab_comp = 1: expected diabetes contribution = 2
  (max rule).

## Verbose diagnostics

Set `verbose = TRUE` to emit three structured messages per call:

1.  **Preparing inputs** — start-of-function signal.
2.  **Column map** — confirms which data column each comorbidity key
    resolved to. Example:
    `charlson_index(): column map: mi -> 'mi', chf -> 'chf', ...`
3.  **Results summary** — shows how many rows computed successfully
    (non-NA) per output column. Example:
    `charlson_index(): results: charlson_index 30/30, charlson_index_age 30/30, ...`

`verbose = TRUE` emits at the `"inform"` level; you also need
`options(healthmarkers.verbose = "inform")` active:

``` r
old_opt <- options(healthmarkers.verbose = "inform")

patient <- tibble::tibble(
  mi=0, chf=0, pvd=0, stroke=0, dementia=0, copd=0, rheum=0, ulcer=0,
  mild_liver=0, diabetes=1, diab_comp=1, hemiplegia=0, renal=1,
  cancer=0, leukemia=0, lymphoma=0, sev_liver=0, metastatic_cancer=0, hiv=0
)
cm_v <- as.list(stats::setNames(names(patient), names(patient)))
charlson_index(patient, col_map = cm_v, verbose = TRUE)
#> charlson_index(): preparing inputs
#> charlson_index(): column map: mi -> 'mi', chf -> 'chf', pvd -> 'pvd', stroke -> 'stroke', dementia -> 'dementia', copd -> 'copd', rheum -> 'rheum', ulcer -> 'ulcer', mild_liver -> 'mild_liver', diabetes -> 'diabetes', diab_comp -> 'diab_comp', hemiplegia -> 'hemiplegia', renal -> 'renal', cancer -> 'cancer', leukemia -> 'leukemia', lymphoma -> 'lymphoma', sev_liver -> 'sev_liver', metastatic_cancer -> 'metastatic_cancer', hiv -> 'hiv'
#> charlson_index(): results: charlson_index 1/1
#> # A tibble: 1 × 1
#>   charlson_index
#>            <int>
#> 1              4

options(old_opt)
```

Reset with `options(healthmarkers.verbose = NULL)` or `"none"`.

## See also

- metss and metabolic_risk_features for metabolic burden scoring.
- health_summary() to combine multiple indices.

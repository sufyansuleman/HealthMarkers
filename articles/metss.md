# MetSS

## Scope

Compute the Metabolic Syndrome Severity Score (MetSSS) using sex- and
race-specific coefficients, with configurable NA/extreme handling and
plausibility diagnostics.

## When to use

- You have waist, blood pressure, fasting lipids/glucose, sex, and race
  and want a continuous MetS severity score.
- You need built-in NA policies, high-missingness warnings, and optional
  extreme-value scanning/capping.
- You are okay with a single parameter key (race/sex) applied to all
  rows—matching the original implementation.

## Inputs

- `data`: data.frame/tibble with `waist` (cm), `bp_sys`/`bp_dia` (mmHg),
  `TG`/`HDL_c`/`glucose` (mmol/L), `sex` (1=male, 2=female), `race` in
  {NHW, NHB, HW, HA} or accepted synonyms.
- `params`: named list keyed by `RACE_SEX` (e.g., `NHW_M`), each with
  `intercept` and component vectors (`waist`, `TG`, `HDL`, `glucose`,
  `MAP`) containing `mean`, `sd`, `coef`.
- `na_action`: `keep` (default) propagates NA; `omit` drops rows missing
  required inputs; `error` aborts; `ignore`/`warn` act like keep (`warn`
  also adds high-missingness warnings via `na_warn_prop`).
- `diagnostics`: if TRUE (default), warns on implausible ranges and
  invalid sex/race encodings.
- `verbose`: log progress and summary.

## Quick start

``` r
library(HealthMarkers)
library(tibble)

mets <- tibble::tibble(
  waist = c(95, 108),
  bp_sys = c(120, 138),
  bp_dia = c(80, 86),
  TG = c(1.5, 2.1),
  HDL_c = c(1.2, 0.9),
  glucose = c(5.5, 6.2),
  sex = c(1, 1),
  race = c("NHW", "NHW")
)

metss(
  data = mets,
  params = list(
    NHW_M = list(
      intercept = -2.344,
      waist   = c(mean = 94.0, sd = 12.4, coef = 0.846),
      TG      = c(mean = 1.5,  sd = 0.6,  coef = 0.701),
      HDL     = c(mean = 1.1,  sd = 0.3,  coef = -0.663),
      glucose = c(mean = 5.3,  sd = 0.6,  coef = 0.658),
      MAP     = c(mean = 97,   sd = 11,   coef = 0.466)
    )
  ),
  na_action = "keep"
)
#> # A tibble: 2 × 1
#>   MetSSS
#>    <dbl>
#> 1  -2.43
#> 2   1.01
```

## Extreme values

Extreme inputs will produce extreme MetSSS scores. The
`diagnostics = TRUE` option will warn on implausible ranges; pre-filter
before calling.

``` r
extreme <- mets
extreme$TG[2] <- 25       # extreme; pre-filter
extreme$bp_sys[1] <- 400  # out of range; pre-filter
extreme$TG[extreme$TG > 20] <- NA
extreme$bp_sys[extreme$bp_sys > 300] <- NA

metss(
  data = extreme,
  params = list(
    NHW_M = list(
      intercept = -2.344,
      waist   = c(mean = 94.0, sd = 12.4, coef = 0.846),
      TG      = c(mean = 1.5,  sd = 0.6,  coef = 0.701),
      HDL     = c(mean = 1.1,  sd = 0.3,  coef = -0.663),
      glucose = c(mean = 5.3,  sd = 0.6,  coef = 0.658),
      MAP     = c(mean = 97,   sd = 11,   coef = 0.466)
    )
  ),
  na_action = "warn",
  diagnostics = TRUE,
  verbose = TRUE
)
#> # A tibble: 2 × 1
#>   MetSSS
#>    <dbl>
#> 1     NA
#> 2     NA
```

## Missing data policy

``` r
missing <- mets
missing$HDL_c[1] <- NA

metss(
  data = missing,
  params = list(
    NHW_M = list(
      intercept = -2.344,
      waist   = c(mean = 94.0, sd = 12.4, coef = 0.846),
      TG      = c(mean = 1.5,  sd = 0.6,  coef = 0.701),
      HDL     = c(mean = 1.1,  sd = 0.3,  coef = -0.663),
      glucose = c(mean = 5.3,  sd = 0.6,  coef = 0.658),
      MAP     = c(mean = 97,   sd = 11,   coef = 0.466)
    )
  ),
  na_action = "omit"
)
#> # A tibble: 1 × 1
#>   MetSSS
#>    <dbl>
#> 1   1.01

metss(
  data = missing,
  params = list(
    NHW_M = list(
      intercept = -2.344,
      waist   = c(mean = 94.0, sd = 12.4, coef = 0.846),
      TG      = c(mean = 1.5,  sd = 0.6,  coef = 0.701),
      HDL     = c(mean = 1.1,  sd = 0.3,  coef = -0.663),
      glucose = c(mean = 5.3,  sd = 0.6,  coef = 0.658),
      MAP     = c(mean = 97,   sd = 11,   coef = 0.466)
    )
  ),
  na_action = "warn"
)
#> # A tibble: 2 × 1
#>   MetSSS
#>    <dbl>
#> 1  NA   
#> 2   1.01
```

## Outputs and expectations

- Returns a tibble with `MetSSS` using a single race/sex key derived
  from the first row (warns if multiple keys present).
- `na_action = "omit"` drops rows missing required inputs; otherwise
  missing inputs propagate `NA` into `MetSSS`.
- `diagnostics` warns independently on implausible ranges.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
metss(
  data = mets,
  params = list(
    NHW_M = list(
      intercept = -2.344,
      waist   = c(mean = 94.0, sd = 12.4, coef = 0.846),
      TG      = c(mean = 1.5,  sd = 0.6,  coef = 0.701),
      HDL     = c(mean = 1.1,  sd = 0.3,  coef = -0.663),
      glucose = c(mean = 5.3,  sd = 0.6,  coef = 0.658),
      MAP     = c(mean = 97,   sd = 11,   coef = 0.466)
    )
  ),
  verbose = TRUE
)
#> metss(): reading input 'mets' — 2 rows × 8 variables
#> metss(): col_map (8 columns — 8 inferred from data)
#>   waist             ->  'waist'    (inferred)
#>   bp_sys            ->  'bp_sys'    (inferred)
#>   bp_dia            ->  'bp_dia'    (inferred)
#>   TG                ->  'TG'    (inferred)
#>   HDL_c             ->  'HDL_c'    (inferred)
#>   glucose           ->  'glucose'    (inferred)
#>   sex               ->  'sex'    (inferred)
#>   race              ->  'race'    (inferred)
#> metss(): computing markers:
#>   MetSSS  [factor-loading z-score, sex/race-specific params]
#> metss(): results: MetSSS 2/2
#> # A tibble: 2 × 1
#>   MetSSS
#>    <dbl>
#> 1  -2.43
#> 2   1.01
options(old_opt)
```

## Tips

- Confirm sex encoded as 1/2 and race matches a `params` key; only the
  first-row key is used.
- Keep units consistent (waist cm, BP mmHg, lipids/glucose mmol/L); no
  unit conversion occurs here.
- Leave `diagnostics = TRUE` during QA to catch implausible ranges;
  silence only if needed for tests.

## Supplying params for all race/sex groups

The function requires a `params` list keyed by `RACE_SEX` (e.g.,
`"NHW_M"`). Published coefficients for all eight groups are available
in:

> Gurka MJ, Vishnu A, Santen RJ, DeBoer MD (2016). Adjusting the
> metabolic syndrome components for use during the menopausal
> transition. *J Clin Endocrinol Metab*, 101(8), 3104–3113.

The structure below shows the full eight-group params object. Population
means and standard deviations are derived from NHANES; regression
coefficients weight the contribution of each standardised component:

``` r
# Full 8-group params object (consult Gurka & DeBoer for exact published values)
all_params <- list(
  NHW_M = list(
    intercept = -2.344,
    waist   = c(mean = 94.0, sd = 12.4, coef =  0.846),
    TG      = c(mean =  1.5, sd =  0.6, coef =  0.701),
    HDL     = c(mean =  1.1, sd =  0.3, coef = -0.663),
    glucose = c(mean =  5.3, sd =  0.6, coef =  0.658),
    MAP     = c(mean = 97.0, sd = 11.0, coef =  0.466)
  ),
  NHW_F = list(
    intercept = -2.048,
    waist   = c(mean = 87.5, sd = 15.2, coef =  0.718),
    TG      = c(mean =  1.3, sd =  0.7, coef =  0.610),
    HDL     = c(mean =  1.5, sd =  0.4, coef = -0.720),
    glucose = c(mean =  5.1, sd =  0.6, coef =  0.575),
    MAP     = c(mean = 91.0, sd = 11.5, coef =  0.420)
  ),
  NHB_M = list(
    intercept = -2.116,
    waist   = c(mean = 92.0, sd = 13.8, coef =  0.790),
    TG      = c(mean =  1.2, sd =  0.5, coef =  0.580),
    HDL     = c(mean =  1.2, sd =  0.3, coef = -0.590),
    glucose = c(mean =  5.4, sd =  0.7, coef =  0.640),
    MAP     = c(mean = 99.0, sd = 12.0, coef =  0.510)
  ),
  NHB_F = list(
    intercept = -1.892,
    waist   = c(mean = 92.8, sd = 16.1, coef =  0.695),
    TG      = c(mean =  1.1, sd =  0.5, coef =  0.520),
    HDL     = c(mean =  1.4, sd =  0.4, coef = -0.650),
    glucose = c(mean =  5.2, sd =  0.7, coef =  0.590),
    MAP     = c(mean = 92.0, sd = 12.0, coef =  0.455)
  ),
  HW_M = list(
    intercept = -2.280,
    waist   = c(mean = 98.2, sd = 13.0, coef =  0.855),
    TG      = c(mean =  1.7, sd =  0.7, coef =  0.730),
    HDL     = c(mean =  1.1, sd =  0.3, coef = -0.640),
    glucose = c(mean =  5.5, sd =  0.7, coef =  0.680),
    MAP     = c(mean = 96.0, sd = 11.0, coef =  0.470)
  ),
  HW_F = list(
    intercept = -2.010,
    waist   = c(mean = 92.0, sd = 14.5, coef =  0.705),
    TG      = c(mean =  1.4, sd =  0.7, coef =  0.620),
    HDL     = c(mean =  1.4, sd =  0.4, coef = -0.690),
    glucose = c(mean =  5.3, sd =  0.7, coef =  0.605),
    MAP     = c(mean = 90.0, sd = 11.5, coef =  0.435)
  ),
  HA_M = list(
    intercept = -2.200,
    waist   = c(mean = 97.0, sd = 12.8, coef =  0.840),
    TG      = c(mean =  1.6, sd =  0.7, coef =  0.715),
    HDL     = c(mean =  1.1, sd =  0.3, coef = -0.635),
    glucose = c(mean =  5.4, sd =  0.7, coef =  0.665),
    MAP     = c(mean = 95.5, sd = 11.0, coef =  0.460)
  ),
  HA_F = list(
    intercept = -1.980,
    waist   = c(mean = 91.0, sd = 14.2, coef =  0.700),
    TG      = c(mean =  1.4, sd =  0.7, coef =  0.615),
    HDL     = c(mean =  1.4, sd =  0.4, coef = -0.685),
    glucose = c(mean =  5.2, sd =  0.7, coef =  0.600),
    MAP     = c(mean = 89.5, sd = 11.5, coef =  0.430)
  )
)
```

> **Note:** The `NHW_M` coefficients above match the original Gurka *et
> al.* values shown in the Quick start example. The remaining seven
> groups use approximate values consistent with NHANES population
> distributions; verify against the original paper before publishing
> results.

Pass the full list when your dataset includes multiple race/sex groups:

``` r
metss(data = your_cohort, params = all_params, na_action = "keep")
```

## Using real cohort data

The bundled simulated dataset has `waist`, `TG`, `HDL_c`, `glucose`,
`bp_sys`, `bp_dia`, `sex`, and `race` columns. Compute MAP internally
and run MetSSS directly on `sim_small`:

``` r
library(dplyr)
sim_path  <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim_small <- dplyr::slice_head(readRDS(sim_path), n = 50)

# metss expects MAP; derive it if not already present:
sim_small <- dplyr::mutate(
  sim_small,
  MAP = (bp_sys + 2 * bp_dia) / 3
)

metss(
  data   = sim_small,
  params = all_params,    # defined above
  na_action = "keep"
)
```

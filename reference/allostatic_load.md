# Allostatic Load Index

Computes a composite Allostatic Load (AL) score by flagging biomarkers
that exceed user-specified high-risk thresholds (strict \> when multiple
biomarkers; inclusive \>= when only one biomarker). Aligned with HM-CS
v3: structured validation, diagnostic control, verbose reporting, and
optional summary output.

## Usage

``` r
allostatic_load(
  data,
  thresholds,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  check_extreme = FALSE,
  extreme_action = c("cap", "NA", "error"),
  sds_limit = 6,
  return_summary = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  data.frame or tibble of numeric biomarker columns.

- thresholds:

  named list of scalar numeric cutoffs (names must match columns).

- col_map:

  optional named list mapping keys in `thresholds` to column names in
  `data`.

- na_action:

  one of c("keep","omit","error") ("keep" treats NA as zero
  contribution).

- check_extreme:

  logical; scan columns with name containing "sds" for \|value\| \>
  sds_limit.

- extreme_action:

  one of c("cap","NA","error") for SDS-like extremes.

- sds_limit:

  positive numeric cutoff for SDS-like scan (default 6).

- return_summary:

  logical; TRUE returns list(data, summary, warnings).

- verbose:

  logical; print progress messages via hm_inform() (also gated by
  options(healthmarkers.verbose)).

## Value

tibble with AllostaticLoad or list when return_summary = TRUE.

## References

McEwen BS, Stellar E (1993). “Stress and the Individual: Mechanisms
Leading to Disease.” *Archives of Internal Medicine*, **153**(18),
2093–2101.
[doi:10.1001/archinte.153.18.2093](https://doi.org/10.1001/archinte.153.18.2093)
. ; Seeman TE, Singer BH, Rowe JW, Horwitz RI, McEwen BS (1997). “Price
of Adaptation—Allostatic Load and Its Health Consequences.” *Archives of
Internal Medicine*, **157**(19), 2259–2268.
[doi:10.1001/archinte.1997.00440400111013](https://doi.org/10.1001/archinte.1997.00440400111013)
. ; Juster R, McEwen BS, Lupien SJ (2010). “Allostatic Load Biomarkers
of Chronic Stress and Impact on Health and Cognition.” *Neuroscience and
Biobehavioral Reviews*, **35**(1), 2–16.
[doi:10.1016/j.neubiorev.2009.10.002](https://doi.org/10.1016/j.neubiorev.2009.10.002)
. ; Wiley JF, Gruenewald TL, Karlamangla AS, Seeman TE (2016). “Modeling
Multisystem Physiological Dysregulation.” *Psychosomatic Medicine*,
**78**(3), 290–301.
[doi:10.1097/PSY.0000000000000288](https://doi.org/10.1097/PSY.0000000000000288)
.

## See also

[`adiposity_sds`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds.md),
[`adiposity_sds_strat`](https://sufyansuleman.github.io/HealthMarkers/reference/adiposity_sds_strat.md)

## Examples

``` r
df <- tibble::tibble(
  SBP = c(118, 142, 130),
  DBP = c(76, 92, 85),
  CRP = c(1.2, 4.8, 2.1)
)
thr <- list(SBP = 130, DBP = 85, CRP = 3)
allostatic_load(df, thresholds = thr, na_action = "keep", verbose = FALSE)
#> # A tibble: 3 × 1
#>   AllostaticLoad
#>            <int>
#> 1              0
#> 2              3
#> 3              0

# Single biomarker uses inclusive >= rule
allostatic_load(df, thresholds = list(CRP = 3))
#> # A tibble: 3 × 1
#>   AllostaticLoad
#>            <int>
#> 1              0
#> 2              1
#> 3              0
```

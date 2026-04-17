# Inflammatory age (iAge proxy)

## Scope

Compute a simplified inflammatory age proxy (`iAge`) as a weighted sum
of CRP, IL6, and TNFa. Supports NA policies and high-missingness
warnings. Units assumed: CRP mg/L; IL6, TNFa pg/mL (no internal
conversions).

## Background

`iAge` is a simplified implementation of the inflammatory age proxy
described by Sayed *et al.* (2021, *Nature Aging*). The original model
uses a 50+ cytokine proteomics panel; this function computes a weighted
sum of three routinely available markers—CRP, IL-6, and TNF-α—as a
practical, single-site approximation.

Elevated iAge scores have been associated with:

- Biological age acceleration independent of chronological age
- Increased frailty, functional decline, and all-cause mortality risk in
  longitudinal cohorts
- Cardiometabolic and neurodegenerative disease incidence

| Marker | Biological role                                            | Default weight |
|--------|------------------------------------------------------------|----------------|
| CRP    | Acute-phase protein; reflects systemic IL-6 signalling     | 0.33           |
| IL-6   | Pro-inflammatory interleukin; master acute-phase regulator | 0.33           |
| TNF-α  | Pleiotropic cytokine; adipose source in metabolic disease  | 0.34           |

Approximate reference ranges (healthy adults, standard lab units): - CRP
\< 3 mg/L (hs-CRP \< 1 mg/L = low cardiovascular risk) - IL-6 \< 7
pg/mL - TNF-α 1–6 pg/mL

## Load packages and demo data

Six participants spanning low to markedly elevated inflammatory burden,
with one missing CRP to demonstrate NA handling.

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  CRP  = c(0.8, 1.5, 3.5,  8.0,   NA, 42.0), # mg/L
  IL6  = c(1.2, 2.0, 4.1,  6.8,  3.2, 15.0), # pg/mL
  TNFa = c(0.8, 1.0, 1.8,  2.5,  1.4,  3.2)  # pg/mL
)
```

## Column map (required)

Map each marker to your column names; all three are mandatory.

``` r
col_map <- list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa")
```

## Core calculation

Default behavior (`na_action = "omit"`/`ignore`/`warn`): NAs are ignored
in the weighted sum (treated as 0 contribution), preserving row count.

``` r
ia_default <- iAge(
  data = df,
  col_map = col_map,
  na_action = "omit",
  verbose = FALSE
)

ia_default
#> # A tibble: 6 × 1
#>     iAge
#>    <dbl>
#> 1  0.932
#> 2  1.50 
#> 3  3.12 
#> 4  5.73 
#> 5  1.53 
#> 6 19.9
```

## Interpreting iAge scores

As a weighted sum of (unstandardised) inflammatory markers, the raw
score is influenced by the absolute magnitude of CRP (which dominates
when elevated). As a broad guide:

| Score | Inflammatory profile                                                            |
|-------|---------------------------------------------------------------------------------|
| 0–2   | Low burden; typical of healthy younger adults                                   |
| 2–8   | Sub-clinical elevation; common in overweight, sedentary, or older adults        |
| \> 8  | High chronic inflammation; consistent with metabolic syndrome or active disease |

For cross-cohort comparison, normalise the score with
[`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
(method `"z"` or `"robust"`) before downstream modelling.

## Keep NA vs drop

`na_action = "keep"` returns NA if any required marker is missing in a
row. `na_action = "error"` stops on missing required inputs.

``` r
ia_keep <- iAge(
  data = df,
  col_map = col_map,
  na_action = "keep"
)

ia_keep
#> # A tibble: 6 × 1
#>     iAge
#>    <dbl>
#> 1  0.932
#> 2  1.50 
#> 3  3.12 
#> 4  5.73 
#> 5 NA    
#> 6 19.9
```

## Extreme values

Extreme marker values will produce extreme scores. Pre-filter
implausible values before calling.

``` r
df_ext <- df
df_ext$CRP[2] <- 500  # extreme on purpose
# Pre-filter or cap before passing to iAge
df_ext$CRP[df_ext$CRP > 300] <- 300

ia_filtered <- iAge(
  data = df_ext,
  col_map = col_map,
  verbose = FALSE
)

ia_filtered
#> # A tibble: 6 × 1
#>      iAge
#>     <dbl>
#> 1   0.932
#> 2 100    
#> 3   3.12 
#> 4   5.73 
#> 5  NA    
#> 6  19.9
```

## Weights

- Default weights: CRP 0.33, IL6 0.33, TNFa 0.34 (sum to 1). Named
  weights are reordered to CRP/IL6/TNFa.
- For cohorts where one marker is more informative, adjust weights
  (ensure they sum to 1):

``` r
iAge(df, col_map = col_map, weights = c(CRP = 0.5, IL6 = 0.25, TNFa = 0.25))
#> # A tibble: 6 × 1
#>    iAge
#>   <dbl>
#> 1  0.9 
#> 2  1.5 
#> 3  3.22
#> 4  6.32
#> 5 NA   
#> 6 25.6
```

## Expectations

- All three markers must be present and numeric; coercion to numeric
  will error if NAs are introduced.
- `na_action` behaviors:
  - `omit`/`ignore`/`warn`: missing markers contribute 0 to the sum;
    rows preserved.
  - `keep`: any missing required marker yields `NA` for that row.
  - `error`: abort if any required marker is missing/NA.
- Output: one-column tibble `iAge`; rows match input unless you subset
  beforehand.

## Verbose diagnostics

Set `verbose = TRUE` (and `healthmarkers.verbose = "inform"`) to surface
three structured messages on each call: preparing inputs, the column
map, and a results summary.

``` r
old_opt <- options(healthmarkers.verbose = "inform")
df_v <- tibble::tibble(CRP = 1.2, IL6 = 2.0, TNFa = 1.0)
iAge(df_v, col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"), verbose = TRUE)
#> iAge(): reading input 'df_v' — 1 rows × 3 variables
#> iAge(): col_map (3 columns — 3 specified)
#>   CRP               ->  'CRP'
#>   IL6               ->  'IL6'
#>   TNFa              ->  'TNFa'
#> iAge(): computing markers:
#>   iAge  [weighted sum: CRP*0.33 + IL6*0.33 + TNFa*0.34]
#> iAge(): results: iAge 1/1
#> # A tibble: 1 × 1
#>    iAge
#>   <dbl>
#> 1  1.40
options(old_opt)
```

## Column recognition (multi-biobank)

CRP is widely available in clinical databases (often labelled `CRP`,
`hs_CRP`, `hsCRP`, `c_reactive_protein`). IL-6 and TNF-α require
targeted immunoassay panels (e.g., Meso Scale Discovery, Luminex) and
are absent from most routine clinical extracts.

``` r
# Check which inflammation markers are present in your cohort data
hm_col_report(your_data)
```

For datasets where only CRP is available, pass a single-marker `col_map`
and assign all weight to CRP:

``` r
iAge(
  data    = your_data,
  col_map = list(CRP = "hs_CRP"),  # IL6/TNFa omitted
  weights = c(CRP = 1.0),          # full weight to CRP
  na_action = "keep"
)
```

See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for recognised synonyms across major biobanks.

## Tips

- Keep units consistent (CRP mg/L; IL6/TNFa pg/mL). Convert before
  calling if needed.
- Prefer `na_action = "keep"` when you want to see which rows are
  incomplete; use `omit` when you want continuity of the score despite
  single missing markers.
- Use `verbose = TRUE` during setup to see mapping and warnings.
- Impute upstream (e.g.,
  [`impute_missing()`](https://sufyansuleman.github.io/HealthMarkers/reference/impute_missing.md))
  if you prefer not to down-weight missing markers to zero.
- Reference: Sayed N, *et al.* (2021). An inflammatory aging clock
  (iAge) based on deep learning tracks multimorbidity, immunosenescence,
  epigenetic reprogramming, and mortality. *Nature Aging*, 1, 598–615.
  <https://doi.org/10.1038/s43587-021-00082-y>

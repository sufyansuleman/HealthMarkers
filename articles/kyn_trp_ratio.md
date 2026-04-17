# Kynurenine/Tryptophan ratio

## Scope

Compute the kynurenine/tryptophan ratio (KTR), a marker of IDO activity
and immune activation. Supports NA policies and verbose progress. Inputs
must already be Kyn (nmol/L) and Trp (µmol/L); no unit conversion.

## Background

The kynurenine/tryptophan ratio (KTR) quantifies the activity of
indoleamine 2,3-dioxygenase 1 (IDO1), the rate-limiting enzyme in the
kynurenine pathway of tryptophan catabolism. Elevated KTR reflects:

- **Immune activation:** IDO1 is upregulated by interferon-γ and other
  pro-inflammatory signals; high KTR indicates cell-mediated immune
  response.
- **Immune escape:** tumour cells exploit IDO1 to suppress T-cell
  activity; KTR is elevated in many malignancies.
- **Ageing and inflammation:** KTR rises with biological age and
  correlates with inflammatory burden (CRP, IL-6).

Formula: \$\text{KTR} = \frac{\text{kynurenine
(nmol/L)}}{\text{tryptophan (\mu mol/L)}}\$

Approximate reference values in healthy adults:

| Population                | Typical KTR |
|---------------------------|-------------|
| Healthy adults (\< 50 yr) | 15–30       |
| Older adults (\> 65 yr)   | 25–45       |
| Active infection / sepsis | 80–400+     |
| Cancer (some types)       | 50–200      |

> **Unit note:** Kyn must be in nmol/L and Trp in µmol/L for the ratio
> to yield values in the physiological range. The resulting KTR is
> unitless (nmol/µmol).

## Load packages and demo data

``` r
library(HealthMarkers)
library(tibble)

# Eight participants: healthy → chronically ill → acute infection range
df <- tibble::tibble(
  Kyn_nM = c(1200, 1800, 2500, 3200, 4500, 8000, 15000, 25000),
  Trp_uM = c(62,   50,   42,   38,   35,   28,   18,    12   )
)
```

*Row interpretation:* rows 1–2 ≈ healthy adults; rows 3–4 ≈ sub-clinical
inflammation; rows 5–6 ≈ chronic immune activation; rows 7–8 ≈ acute
infection or high-grade malignancy.

## Column map (required)

``` r
col_map <- list(kynurenine = "Kyn_nM", tryptophan = "Trp_uM")
```

## Core calculation

Default `na_action = "keep"`: ratios are `NA` where inputs are missing;
no extreme scan.

``` r
ktr <- kyn_trp_ratio(
  data = df,
  col_map = col_map,
  na_action = "keep",
  verbose = FALSE
)

ktr
#> # A tibble: 8 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          19.4
#> 2          36  
#> 3          59.5
#> 4          84.2
#> 5         129. 
#> 6         286. 
#> 7         833. 
#> 8        2083.
```

## Extreme values

Extreme Kyn or Trp values will produce extreme KTR. Pre-filter
implausible inputs before calling.

``` r
df_ext <- df
df_ext$Kyn_nM[2] <- 50000  # extreme; pre-filter
df_ext$Kyn_nM[df_ext$Kyn_nM > 20000] <- NA

ktr_filtered <- kyn_trp_ratio(
  data = df_ext,
  col_map = col_map,
  na_action = "warn",
  verbose = FALSE
)

ktr_filtered
#> # A tibble: 8 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          19.4
#> 2          NA  
#> 3          59.5
#> 4          84.2
#> 5         129. 
#> 6         286. 
#> 7         833. 
#> 8          NA
```

## Missing data handling

`keep`/`ignore`/`warn` retain rows with NA ratios; `omit` drops rows
with missing inputs; `error` stops on missingness.

``` r
df_na <- df
df_na$Trp_uM[3] <- NA

keep_out <- kyn_trp_ratio(df_na, col_map, na_action = "keep")
omit_out <- kyn_trp_ratio(df_na, col_map, na_action = "omit")

list(keep_rows = nrow(keep_out), omit_rows = nrow(omit_out))
#> $keep_rows
#> [1] 8
#> 
#> $omit_rows
#> [1] 7
```

## Expectations

- Both markers must be mapped and present; non-numeric inputs are
  coerced with warnings if NAs are introduced.
- Trp must be positive; nonpositive values are set to `NA` with a
  warning.
- Output: one-column tibble `kyn_trp_ratio`; row count follows
  `na_action` if you omit rows.

## Verbose diagnostics

``` r
old_opt <- options(healthmarkers.verbose = "inform")
kyn_trp_ratio(
  data    = df,
  col_map = col_map,
  verbose = TRUE
)
#> kyn_trp_ratio(): reading input 'df' — 8 rows × 2 variables
#> kyn_trp_ratio(): preparing inputs
#> kyn_trp_ratio(): col_map (2 columns — 2 specified)
#>   kynurenine        ->  'Kyn_nM'
#>   tryptophan        ->  'Trp_uM'
#> kyn_trp_ratio(): computing markers:
#>   kyn_trp_ratio  [kynurenine (nmol/L) / tryptophan (mumol/L)]
#> kyn_trp_ratio(): results: kyn_trp_ratio 8/8
#> # A tibble: 8 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          19.4
#> 2          36  
#> 3          59.5
#> 4          84.2
#> 5         129. 
#> 6         286. 
#> 7         833. 
#> 8        2083.
options(old_opt)
```

## Column recognition (multi-biobank)

Kynurenine and tryptophan are measured by targeted HPLC or
mass-spectrometry panels. Column names vary widely across platforms. Use
[`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
to check what is auto-detected in your dataset:

``` r
hm_col_report(your_data)  # shows matched keys and suggests col_map entries
```

See the [Multi-Biobank
Compatibility](https://sufyansuleman.github.io/HealthMarkers/articles/multi_biobank.md)
article for a full list of recognised synonyms (e.g.,
`kynurenine_nmol_L`, `KYN`, `TRP`, `tryptophan_umol`).

## Tips

- Re-check units when ratios are very high (\>100); Kyn should be nmol/L
  and Trp µmol/L.
- Reference: Badawy AA-B (2019). Tryptophan metabolism: a versatile area
  providing multiple targets for pharmacological intervention. *Egypt J
  Basic Clin Pharmacol*. <https://doi.org/10.32527/2019/101415>
- For modeling, consider scaling/transforming KTR separately after
  computation.

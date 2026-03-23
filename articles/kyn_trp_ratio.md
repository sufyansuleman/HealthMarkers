# Kynurenine/Tryptophan ratio

## Scope

Compute the kynurenine/tryptophan ratio (KTR), a marker of IDO activity
and immune activation. Supports NA policies, optional extreme screening
(warn/cap/NA/error), and verbose progress. Inputs must already be Kyn
(nmol/L) and Trp (µmol/L); no unit conversion.

## Load packages and demo data

``` r
library(HealthMarkers)
library(tibble)

df <- tibble::tibble(
  Kyn_nM = c(1800, 2500, 3200),
  Trp_uM = c(50, 42, 12)
)
```

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
  check_extreme = FALSE,
  verbose = FALSE
)

ktr
#> # A tibble: 3 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          36  
#> 2          59.5
#> 3         267.
```

## Extreme screening

Scan and cap into default ranges.

``` r
df_ext <- df
df_ext$Kyn_nM[2] <- 50000  # extreme

ktr_cap <- kyn_trp_ratio(
  data = df_ext,
  col_map = col_map,
  na_action = "warn",
  check_extreme = TRUE,
  extreme_action = "cap",
  verbose = TRUE
)

ktr_cap
#> # A tibble: 3 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1           36 
#> 2          476.
#> 3          267.
```

`extreme_action = "warn"` only warns; `error` aborts; `NA` blanks
flagged values.

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
#> [1] 3
#> 
#> $omit_rows
#> [1] 2
```

## Expectations

- Both markers must be mapped and present; non-numeric inputs are
  coerced with warnings if NAs are introduced.
- Trp must be positive; nonpositive values are set to `NA` with a
  warning.
- Extreme defaults: Kyn 100–20000 nmol/L, Trp 10–150 µmol/L, ratio
  0–200; adjust with `extreme_rules`.
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
#> kyn_trp_ratio(): preparing inputs
#> kyn_trp_ratio(): column map: kynurenine -> 'Kyn_nM', tryptophan -> 'Trp_uM'
#> kyn_trp_ratio(): results: kyn_trp_ratio 3/3
#> # A tibble: 3 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          36  
#> 2          59.5
#> 3         267.
options(old_opt)
```

## Tips

- Re-check units when ratios are very high (\>100); Kyn should be nmol/L
  and Trp µmol/L.
- Use `extreme_action = "cap"` sparingly and adjust ranges to your
  cohort.
- For modeling, consider scaling/transforming KTR separately after
  computation.

# Kynurenine/Tryptophan Ratio (KTR)

Computes the ratio of kynurenine to tryptophan, a marker of IDO activity
and immune activation.

## Usage

``` r
kyn_trp_ratio(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with kynurenine and tryptophan concentrations.

- col_map:

  Named list with:

  - kynurenine: column for kynurenine (nmol/L)

  - tryptophan: column for tryptophan (mumol/L)

- na_action:

  One of c("keep","omit","error","ignore","warn").

- verbose:

  Logical; if `TRUE` (default), prints column mapping and a per-column
  results summary.

## Value

A tibble with one column: kyn_trp_ratio (numeric). If an ID column is
detected, it is prepended.

## Details

KTR is calculated as Kyn (nmol/L) divided by Trp (mumol/L). Elevated KTR
indicates increased tryptophan catabolism via the kynurenine pathway,
often reflecting inflammation and cell-mediated immune activation.

Inputs should already be in Kyn (nmol/L) and Trp (mumol/L).

## References

Fuchs D, Moller AA, Reibnegger G, Werner ER, Werner-Felmayer G, Dierich
MP, Wachter H (1998). “Serum kynurenine-to-tryptophan ratio increases
with disease progression in HIV-1 infection.” *Clinical Chemistry*,
**44**(4), 858–862.
[doi:10.1093/clinchem/44.4.858](https://doi.org/10.1093/clinchem/44.4.858)
, PMID:9555676. ; Damerell V, Midttun O, Ulvik A, et al. (2025).
“Circulating tryptophan-kynurenine pathway metabolites are associated
with all-cause mortality among patients with stage I–III colorectal
cancer.” *International Journal of Cancer*, **156**(3), 552–565.
[doi:10.1002/ijc.35183](https://doi.org/10.1002/ijc.35183) .

## Examples

``` r
df <- data.frame(Kyn_nM = c(2500, 3100, 2700), Trp_uM = c(55, 48, 62))
kyn_trp_ratio(df)
#> kyn_trp_ratio(): reading input 'df' — 3 rows × 2 variables
#> kyn_trp_ratio(): preparing inputs
#> kyn_trp_ratio(): col_map (2 columns — 2 inferred from data)
#>   kynurenine        ->  'Kyn_nM'    (inferred)
#>   tryptophan        ->  'Trp_uM'    (inferred)
#> kyn_trp_ratio(): computing markers:
#>   kyn_trp_ratio  [kynurenine (nmol/L) / tryptophan (mumol/L)]
#> kyn_trp_ratio(): results: kyn_trp_ratio 3/3
#> # A tibble: 3 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          45.5
#> 2          64.6
#> 3          43.5
```

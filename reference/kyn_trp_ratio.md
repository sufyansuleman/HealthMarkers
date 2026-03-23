# Kynurenine/Tryptophan Ratio (KTR)

Computes the ratio of kynurenine to tryptophan, a marker of IDO activity
and immune activation.

## Usage

``` r
kyn_trp_ratio(
  data,
  col_map = list(kynurenine = "Kyn_nM", tryptophan = "Trp_uM"),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  check_extreme = FALSE,
  extreme_action = c("warn", "cap", "error", "ignore", "NA"),
  extreme_rules = NULL,
  verbose = FALSE
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

- check_extreme:

  Logical; if TRUE, scan inputs for plausible ranges.

- extreme_action:

  One of c("warn","cap","error","ignore","NA").

- extreme_rules:

  Optional overrides; defaults: list(kynurenine_nmolL = c(100, 20000),
  tryptophan_umolL = c(10, 150), ratio = c(0, 200)).

- verbose:

  Logical; if TRUE, emits progress via rlang::inform.

## Value

A tibble with one column: kyn_trp_ratio (numeric).

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
#> # A tibble: 3 × 1
#>   kyn_trp_ratio
#>           <dbl>
#> 1          45.5
#> 2          64.6
#> 3          43.5
```

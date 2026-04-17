# Oxidative stress markers

Computes GSH_GSSG_Ratio = reduced glutathione (GSH) / oxidized
glutathione (GSSG).

## Usage

``` r
oxidative_markers(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error"),
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with columns for GSH and GSSG (per col_map).

- col_map:

  Named list with required keys GSH and GSSG. Defaults assume same
  names.

- na_action:

  One of c("keep","omit","error").

- verbose:

  Logical; if `TRUE` (default), prints column mapping and a per-column
  results summary.

## Value

A tibble with column GSH_GSSG_Ratio. If an ID column is detected, it is
prepended.

## References

Forman HJ, Zhang H, Rinna A (2009). “Glutathione: Overview of its
protective roles, measurement, and biosynthesis.” *Molecular Aspects of
Medicine*, **30**(1–2), 1–12.
[doi:10.1016/j.mam.2008.08.006](https://doi.org/10.1016/j.mam.2008.08.006)
.

## Examples

``` r
df <- data.frame(GSH = c(5, 3), GSSG = c(1, 0.5))
oxidative_markers(df, col_map = list(GSH="GSH", GSSG="GSSG"))
#> oxidative_markers(): reading input 'df' — 2 rows × 2 variables
#> oxidative_markers(): col_map (2 columns — 2 specified)
#>   GSH               ->  'GSH'
#>   GSSG              ->  'GSSG'
#> oxidative_markers(): computing markers:
#>   GSH_GSSG_Ratio  [GSH / GSSG]
#> oxidative_markers(): results: GSH_GSSG_Ratio 2/2
#> # A tibble: 2 × 1
#>   GSH_GSSG_Ratio
#>            <dbl>
#> 1              5
#> 2              6
```

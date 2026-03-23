# Oxidative stress markers

Computes GSH_GSSG_Ratio = reduced glutathione (GSH) / oxidized
glutathione (GSSG).

## Usage

``` r
oxidative_markers(
  data,
  col_map = list(GSH = "GSH", GSSG = "GSSG"),
  na_action = c("keep", "omit", "error"),
  verbose = FALSE
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

  Logical; if TRUE, emits progress via hm_inform().

## Value

A tibble with column GSH_GSSG_Ratio.

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
#> # A tibble: 2 × 1
#>   GSH_GSSG_Ratio
#>            <dbl>
#> 1              5
#> 2              6
```

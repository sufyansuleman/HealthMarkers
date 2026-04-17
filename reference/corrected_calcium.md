# Albumin-Corrected Calcium

Calculates the albumin-adjusted (corrected) serum calcium level,
accounting for hypoalbuminemia, using the Payne formula.

## Usage

``` r
corrected_calcium(
  data,
  col_map = NULL,
  units = c("auto", "conventional", "si"),
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble containing serum calcium and albumin.

- col_map:

  Named list with `calcium` and `albumin` indicating column names.

- units:

  One of c("auto","conventional","si"). "auto" attempts unit detection.

- na_action:

  One of c("keep","omit","error","ignore","warn").

- verbose:

  Logical; if TRUE (default), emits progress via hm_inform().

## Value

A tibble with one column: corrected_calcium (numeric, in mg/dL for
conventional input or mmol/L for SI / auto-SI input).

## Details

Payne formula (conventional units): Corrected Ca (mg/dL) = measured Ca
(mg/dL) + 0.8 \* (4.0 - albumin (g/dL)). If inputs appear to be in SI
units (calcium mmol/L, albumin g/L), they are converted to mg/dL and
g/dL (using 1 mmol/L ~= 4 mg/dL; 1 g/L = 0.1 g/dL) for the correction
and converted back to mmol/L for output.

## References

Payne RB, Little AJ, Williams RB, Milner JR (1973). “Interpretation of
serum calcium in patients with abnormal serum proteins.” *British
Medical Journal*, **4**(5893), 643–646.
[doi:10.1136/bmj.4.5893.643](https://doi.org/10.1136/bmj.4.5893.643) .

## Examples

``` r
df <- data.frame(Ca = c(2.3, 2.5, 2.1), Alb = c(38, 42, 30))
corrected_calcium(df)
#> corrected_calcium(): reading input 'df' — 3 rows × 2 variables
#> corrected_calcium(): col_map (2 columns — 2 inferred from data)
#>   calcium           ->  'Ca'    (inferred)
#>   albumin           ->  'Alb'    (inferred)
#> corrected_calcium(): computing markers:
#>   corrected_calcium  [Payne formula: Ca + 0.8 * (4.0 - Alb)]
#> Warning: corrected_calcium(): auto-detected SI units Ca=mmol/L, Alb=g/L; output standardized to mmol/L.
#> corrected_calcium(): results: corrected_calcium 3/3
#> # A tibble: 3 × 1
#>   corrected_calcium
#>               <dbl>
#> 1              2.34
#> 2              2.46
#> 3              2.3 
```

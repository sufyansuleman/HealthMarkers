# Calculate sweat-based ionic & metabolic markers

Computes:

- sweat_chloride (mmol/L)

- Na_K_ratio (sweat Na+/K+)

- sweat_lactate (mmol/L)

- sweat_rate (L/m^2/h) from body mass loss per hour per m^2

## Usage

``` r
sweat_markers(
  data,
  col_map = NULL,
  verbose = TRUE,
  na_action = c("keep", "omit", "error"),
  na_warn_prop = 0.2
)
```

## Arguments

- data:

  A data.frame or tibble containing sweat assay and anthropometrics.

- col_map:

  Named list mapping required inputs (defaults assume same names):

  - sweat_chloride, sweat_Na, sweat_K, sweat_lactate, weight_before,
    weight_after, duration, body_surface_area

- verbose:

  Logical; if `TRUE`, prints progress messages and a completion summary.

- na_action:

  One of `c("keep","omit","error")` for handling missing values in
  required inputs. Default "keep".

- na_warn_prop:

  Proportion \\\[0,1\]\\ to trigger high-missingness diagnostics for
  required inputs (debug level). Default 0.2.

## Value

A tibble with columns: `sweat_chloride`, `Na_K_ratio`, `sweat_lactate`,
`sweat_rate`

## Details

Inputs are validated, missingness handled via `na_action`, safe
divisions are used to avoid Inf/NaN, and an optional extremes scan/cap
is available.

Expected units:

- sweat_chloride, sweat_Na, sweat_K: mmol/L

- sweat_lactate: mmol/L

- weight_before, weight_after: kg

- duration: hours

- body_surface_area: m^2

## References

Gibson LE, Cooke RE. A test for concentration of electrolytes in sweat
in cystic fibrosis of the pancreas utilizing pilocarpine by
iontophoresis. Pediatrics. 1959;23(3):545-549. (Sweat chloride test
origin) Dill DB, Costill DL. Calculation of percentage changes in
volumes of blood, plasma, and red cells in dehydration. J Appl Physiol.
1974;37(2):247-248.
[doi:10.1152/jappl.1974.37.2.247](https://doi.org/10.1152/jappl.1974.37.2.247)
Farrell PM, White TB, Ren CL, et al. Diagnosis of cystic fibrosis:
consensus guidelines from the Cystic Fibrosis Foundation. J Pediatr.
2017;181S:S4-S15.e1.
[doi:10.1016/j.jpeds.2016.09.064](https://doi.org/10.1016/j.jpeds.2016.09.064)
Sawka MN, Cheuvront SN, Kenefick RW. Hypohydration and human
performance: impact of environment and physiological mechanisms. Sports
Med. 2015;45(Suppl 1):S51-S60.
[doi:10.1007/s40279-015-0395-7](https://doi.org/10.1007/s40279-015-0395-7)

## Examples

``` r
df <- tibble::tibble(
  sweat_chloride    = 45,
  sweat_Na          = 55,
  sweat_K           = 5,
  sweat_lactate     = 4.8,
  weight_before     = 70.0,
  weight_after      = 69.5,
  duration          = 1.0,
  body_surface_area = 1.9
)
sweat_markers(df)
#> sweat_markers(): reading input 'df' — 1 rows × 8 variables
#> sweat_markers(): col_map (8 columns — 8 inferred from data)
#>   sweat_chloride    ->  'sweat_chloride'    (inferred)
#>   sweat_Na          ->  'sweat_Na'    (inferred)
#>   sweat_K           ->  'sweat_K'    (inferred)
#>   sweat_lactate     ->  'sweat_lactate'    (inferred)
#>   weight_before     ->  'weight_before'    (inferred)
#>   weight_after      ->  'weight_after'    (inferred)
#>   duration          ->  'duration'    (inferred)
#>   body_surface_area ->  'body_surface_area'    (inferred)
#> sweat_markers(): computing markers:
#>   sweat_chloride [sweat_chloride]
#>   Na_K_ratio [sweat_Na, sweat_K]
#>   sweat_lactate [sweat_lactate]
#>   sweat_rate [weight_before, weight_after, duration, body_surface_area]
#> sweat_markers(): results: sweat_chloride 1/1, Na_K_ratio 1/1, sweat_lactate 1/1, sweat_rate 1/1
#> # A tibble: 1 × 4
#>   sweat_chloride Na_K_ratio sweat_lactate sweat_rate
#>            <dbl>      <dbl>         <dbl>      <dbl>
#> 1             45         11           4.8      0.263
```

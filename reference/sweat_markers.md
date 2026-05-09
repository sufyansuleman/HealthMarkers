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

## Note

`sweat_chloride` and `sweat_lactate` are **pass-through** columns; no
formula is applied. `Na_K_ratio` is a simple Na/K division. `sweat_rate`
uses the mass-loss method: (weight_before - weight_after) / duration /
body_surface_area (units: L/m²/h; assumes 1 kg \\\approx\\ 1 L). Dill &
Costill 1974 describes haematocrit-based blood-volume change, not sweat
rate directly; cited here as background context only.

## References

Gibson LE, Cooke RE (1959). “A test for concentration of electrolytes in
sweat in cystic fibrosis of the pancreas utilizing pilocarpine by
iontophoresis.” *Pediatrics*, **23**(3), 545–549. (pilocarpine sweat
chloride test origin; background) Dill DB, Costill DL (1974).
“Calculation of percentage changes in volumes of blood, plasma, and red
cells in dehydration.” *Journal of Applied Physiology*, **37**(2),
247–248.
[doi:10.1152/jappl.1974.37.2.247](https://doi.org/10.1152/jappl.1974.37.2.247)
. (dehydration and fluid loss context; background) Farrell PM, White TB,
Ren CL, Hempstead SE, Accurso F, Derichs N, Howenstine M, McColley SA,
Rock M, Rosenfeld M, Sermet-Gaudelus I, Southern KW, Marshall BC, Sosnay
PR (2017). “Diagnosis of cystic fibrosis: consensus guidelines from the
Cystic Fibrosis Foundation.” *Journal of Pediatrics*, **181S**,
S4–S15.e1.
[doi:10.1016/j.jpeds.2016.09.064](https://doi.org/10.1016/j.jpeds.2016.09.064)
. (CF diagnostic sweat chloride cutoffs; background) Sawka MN, Cheuvront
SN, Kenefick RW (2015). “Hypohydration and human performance: impact of
environment and physiological mechanisms.” *Sports Medicine*,
**45**(Suppl 1), S51–S60.
[doi:10.1007/s40279-015-0395-7](https://doi.org/10.1007/s40279-015-0395-7)
. (sweat rate and hypohydration context; background)

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

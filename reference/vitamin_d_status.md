# Vitamin D Status Category

Categorizes vitamin D status based on serum 25-hydroxyvitamin D
(25(OH)D) levels.

## Usage

``` r
vitamin_d_status(
  data,
  col_map = NULL,
  na_action = c("keep", "omit", "error", "ignore", "warn"),
  verbose = TRUE
)
```

## Arguments

- data:

  A data.frame or tibble with a 25-hydroxyvitamin D concentration
  column.

- col_map:

  A named list with `vitamin_d` giving the column name in `data` for
  25(OH)D.

- na_action:

  One of c("keep","omit","error","ignore","warn").

  - keep/ignore: compute and propagate NA

  - omit: drop rows with NA in required input

  - error: abort if required input contains NA

  - warn: like keep, but emit missingness warnings

- verbose:

  Logical; if TRUE, emits progress via rlang::inform.

## Value

A tibble with one column: vitamin_d_status (ordered factor with levels
"Deficient","Insufficient","Sufficient").

## Details

Serum 25(OH)D is the standard biomarker for vitamin D status. This
function classifies levels (assumed in ng/mL) into categories:

- Deficient (\< 20 ng/mL)

- Insufficient (20-29 ng/mL)

- Sufficient (\>= 30 ng/mL)

Note: Ensure input units are ng/mL. If values appear extremely high
(e.g., median \> 150), they might be in nmol/L (divide by 2.5 to convert
to ng/mL).

## References

for Vitamin D IoM(CtRDRI, Calcium (2011). *Dietary Reference Intakes for
Calcium and Vitamin D*. National Academies Press.
[doi:10.17226/13050](https://doi.org/10.17226/13050) . ; Holick MF,
Binkley NC, Bischoff-Ferrari HA, et al. (2011). “Evaluation, Treatment,
and Prevention of Vitamin D Deficiency: an Endocrine Society Clinical
Practice Guideline.” *Journal of Clinical Endocrinology & Metabolism*,
**96**(7), 1911–1930.
[doi:10.1210/jc.2011-0385](https://doi.org/10.1210/jc.2011-0385) .

## Examples

``` r
df <- data.frame(VitD = c(18, 45, 72))
vitamin_d_status(df)
#> vitamin_d_status(): reading input 'df' — 3 rows × 1 variables
#> vitamin_d_status(): col_map (2 columns — 2 inferred from data)
#>   vitd              ->  'VitD'    (inferred)
#>   vitamin_d         ->  'VitD'    (inferred)
#> vitamin_d_status(): computing markers:
#>   vitamin_d_status [VitD]
#> vitamin_d_status(): results: vitamin_d_status 3/3
#> # A tibble: 3 × 1
#>   vitamin_d_status
#>   <ord>           
#> 1 Deficient       
#> 2 Sufficient      
#> 3 Sufficient      
```

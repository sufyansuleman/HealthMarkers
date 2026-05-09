# Adult ADHD Self-Report Scale scoring

Adult ADHD Self-Report Scale scoring

## Usage

``` r
asrs_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  impute = c("none", "mean"),
  prefix = "ASRS",
  partA_items = sprintf("asrs_%02d", 1:6),
  partA_thresholds = c(3, 3, 3, 4, 4, 4),
  partA_cutoff = 4,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Named list mapping canonical item IDs to column names; defaults assume
  items are already named.

- na_action:

  How to handle rows with missing items: `keep`, `omit`, or `error`.

- missing_prop_max:

  Maximum allowed proportion of missing items per row before the score
  is set to `NA`.

- impute:

  Imputation strategy for missing items when under the threshold: `none`
  or `mean` (row-wise mean).

- prefix:

  Prefix for output column names.

- partA_items:

  Vector of Part A item IDs.

- partA_thresholds:

  Numeric thresholds applied to Part A items.

- partA_cutoff:

  Count threshold for Part A positivity.

- verbose:

  Logical; if `TRUE`, emits informational messages about column
  resolution and scoring progress via `hm_inform()`.

## Value

A tibble of score columns only: `ASRS_total`, `ASRS_partA_count`,
`ASRS_partA_positive`. Input columns are not included.

## Note

Items are expected on a 0–4 scale (Never=0, Rarely=1, Sometimes=2,
Often=3, Very Often=4). The default `partA_thresholds` follow the
official WHO ASRS v1.1 guide: items 1–3 are positive at \\\geq 3\\
(Often or Very Often) and items 4–6 at \\\geq 4\\ (Very Often only).

## References

Kessler RC, Adler LA, Ames M, Demler O, Faraone SV, Hiripi E, Howes MJ,
Jin R, Secnik K, Spencer T, Üstün TB, Walters EE (2005). “The World
Health Organization Adult ADHD Self-Report Scale (ASRS): A Short
Screening Scale for Use in the General Population.” *Psychological
Medicine*, **35**(2), 245–256.
[doi:10.1017/S0033291704002892](https://doi.org/10.1017/S0033291704002892)
. Kessler RC, Adler L, Ames M, Demler O, Faraone SV, Hiripi E, Howes MJ,
Jin R, Secnik K, Spencer T, Ustün TB, Walters EE (2006). “The Prevalence
and Correlates of Adult ADHD in the United States: Results from the
National Comorbidity Survey Replication.” *American Journal of
Psychiatry*, **163**(4), 716–723.
[doi:10.1176/ajp.2006.163.4.716](https://doi.org/10.1176/ajp.2006.163.4.716)
. (prevalence study; ASRS used as instrument)

## Examples

``` r
df <- data.frame(matrix(2, nrow = 1, ncol = 18))
names(df) <- sprintf("asrs_%02d", 1:18)
asrs_score(df)
#> # A tibble: 1 × 3
#>   ASRS_total ASRS_partA_count ASRS_partA_positive
#>        <dbl>            <dbl> <lgl>              
#> 1         36                0 FALSE              
```

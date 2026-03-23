# Cognitive composite (z-mean or PCA1)

Cognitive composite (z-mean or PCA1)

## Usage

``` r
cognitive_score(
  data,
  col_map = list(),
  na_action = c("keep", "omit", "error"),
  missing_prop_max = 0.2,
  method = c("z_mean", "pca1"),
  prefix = "cog",
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame containing questionnaire item columns.

- col_map:

  Named list with `tasks` mapping task IDs to column names (\>= 2 tasks
  required).

- na_action:

  How to handle rows with missing items: `keep`, `omit`, or `error`.

- missing_prop_max:

  Maximum allowed proportion of missing items per row before the score
  is set to `NA`.

- method:

  Aggregation method: `z_mean` (average of z-scores) or `pca1` (first
  PC).

- prefix:

  Prefix for output column names.

- verbose:

  Logical flag for verbose messaging (reserved).

## Examples

``` r
df <- data.frame(task_a = c(1, 2), task_b = c(2, 3), task_c = c(3, 4))
cm <- list(tasks = list(
  task_a = "task_a",
  task_b = "task_b",
  task_c = "task_c"
))
cognitive_score(df, col_map = cm, method = "z_mean")
#> # A tibble: 2 × 4
#>   task_a task_b task_c cog_z_mean
#>    <dbl>  <dbl>  <dbl>      <dbl>
#> 1      1      2      3     -0.707
#> 2      2      3      4      0.707
```

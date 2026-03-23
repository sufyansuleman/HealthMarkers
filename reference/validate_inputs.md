# Validate required inputs for a calling function

Ensures required keys exist in `col_map` and have non-empty mappings.
Missing keys are reported in a stable order aligned with tests.

## Usage

``` r
validate_inputs(data, col_map, fun_name)
```

## Arguments

- data:

  data.frame or tibble

- col_map:

  named list mapping keys to column names

- fun_name:

  character scalar naming the calling function (e.g., "lipid_markers")

## Value

invisibly TRUE on success; otherwise aborts

## Examples

``` r
df <- data.frame(TG = c(1.5, 2.0), HDL_c = c(1.2, 1.0),
  LDL_c = c(2.0, 2.5), TC = c(4.5, 5.0))
validate_inputs(df,
  list(TG = "TG", HDL_c = "HDL_c", LDL_c = "LDL_c", TC = "TC"),
  fun_name = "lipid_markers")
```

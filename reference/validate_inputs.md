# Validate required inputs for a calling function

Ensures required keys exist in `col_map` and have non-empty mappings.
Missing keys are reported in a stable order aligned with tests.

## Usage

``` r
validate_inputs(data, col_map, fun_name, required_keys = NULL)
```

## Arguments

- data:

  data.frame or tibble

- col_map:

  named list mapping keys to column names

- fun_name:

  character scalar naming the calling function (e.g., "lipid_markers").
  Used to look up built-in required keys when `required_keys` is not
  supplied.

- required_keys:

  optional character vector of required col_map keys. When supplied,
  this takes precedence over the `fun_name` built-in lookup, making the
  function useful for any caller regardless of `fun_name`.

## Value

invisibly TRUE on success; otherwise aborts

## Examples

``` r
df <- data.frame(TG = c(1.5, 2.0), HDL_c = c(1.2, 1.0),
  LDL_c = c(2.0, 2.5), TC = c(4.5, 5.0))
# Using built-in lookup
validate_inputs(df,
  list(TG = "TG", HDL_c = "HDL_c", LDL_c = "LDL_c", TC = "TC"),
  fun_name = "lipid_markers")
# Using explicit required_keys (works for any function)
validate_inputs(df,
  list(TG = "TG", HDL_c = "HDL_c"),
  fun_name = "my_function",
  required_keys = c("TG", "HDL_c"))
```

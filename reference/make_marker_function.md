# Generate a marker function

Creates and assigns a marker function with consistent validation,
NA/extreme handling, and hm_inform logging. Intended for internal
scaffolding of new HealthMarkers functions.

## Usage

``` r
make_marker_function(
  name,
  required_keys,
  compute_body,
  defaults = list(),
  include_extremes = TRUE
)
```

## Arguments

- name:

  Function name to create (string).

- required_keys:

  Character vector of required col_map keys.

- compute_body:

  List containing `expr` (quoted expression computing `out`),
  `empty_out` (returned when zero rows), and `default_rules` (optional
  extreme rules list) used when `include_extremes = TRUE`.

- defaults:

  Named list of default arguments to merge into the generated function
  formals (e.g., col_map, normalize).

- include_extremes:

  Logical; if TRUE, the generated function wires in extreme-value
  scanning/capping using `compute_body$default_rules`.

## Value

Invisibly returns the symbol name after assigning the function into the
parent frame.

## Examples

``` r
# Use local() to prevent assigning into the global environment
local({
  make_marker_function(
    name = "my_marker",
    required_keys = c("var1", "var2"),
    compute_body = list(
      expr = quote(out <- tibble::tibble(
        result = data[[col_map$var1]] + data[[col_map$var2]]
      )),
      empty_out = tibble::tibble(result = numeric(0))
    )
  )
  is.function(my_marker)
})
#> [1] TRUE
```

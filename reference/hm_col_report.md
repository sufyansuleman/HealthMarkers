# Diagnose and report column name mapping for your data

Scans the column names of your data frame against the internal synonym
dictionary used by all HealthMarkers functions and prints a formatted
report showing which internal keys were matched automatically and which
were not found. The function uses five matching layers in order:

1.  **User-supplied** (via `col_map`) — always wins.

2.  **Exact synonym match** — column name is in the synonym list.

3.  **Case-insensitive exact** — same, ignoring upper/lower case.

4.  **Column contains synonym** — data column name contains a synonym as
    a whole word (e.g.\\ `"trig_baseline"` matches `"trig"`).

5.  **Synonym contains column** — a synonym contains the column name as
    a whole word (e.g.\\ `"TG_fasting"` synonym matches column `"TG"`).

6.  **Fuzzy** (opt-in via `fuzzy = TRUE`) — Levenshtein-based
    approximate matching as a last resort.

The function returns the matched mappings invisibly as a named list that
can be passed directly as the `col_map` argument to any HealthMarkers
function.

## Usage

``` r
hm_col_report(
  data,
  col_map = NULL,
  verbose = TRUE,
  fuzzy = FALSE,
  show_unmatched = FALSE
)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- col_map:

  Optional named list of manually specified mappings
  (`list(TG = "trig_my_col")`). These always take priority.

- verbose:

  Logical (default `TRUE`). Print the formatted report.

- fuzzy:

  Logical (default `FALSE`). Enable fuzzy / approximate matching as a
  final layer. May produce false positives; review results with care.

- show_unmatched:

  Logical (default `FALSE`). If `TRUE`, also list every key that could
  not be matched. Useful when you expect a variable to be found but it
  isn't.

## Value

Invisibly returns a named list: internal key \\\to\\ matched data-column
name. Keys that were not found are omitted. You can assign this to
`col_map` and pass it to any HealthMarkers function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic diagnostic
hm_col_report(my_data)

# Fuzzy matching + show all unmatched
hm_col_report(my_data, fuzzy = TRUE, show_unmatched = TRUE)

# Capture the result as a ready-to-use col_map
cm <- hm_col_report(my_data, verbose = FALSE)
all_health_markers(my_data, col_map = cm)
} # }
```

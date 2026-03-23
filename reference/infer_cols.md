# Infer column names from user data based on flexible patterns, with logging

Given a data.frame and a named mapping spec (e.g., list(G0 = NULL, I0 =
NULL)), infer the source column names for each key using a set of regex
patterns. You can supply your own patterns and "preferred" names to
deterministically resolve ambiguous matches. A structured log is kept
and can be written to disk.

Exact-name matching helper used by
[`all_health_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_health_markers.md)
and related wrappers. It picks the first matching candidate for each
key, logs decisions via
[`hm_inform()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_inform.md)
when `verbose = TRUE`, and errors if required keys cannot be resolved.

## Usage

``` r
infer_cols(
  data,
  map,
  verbose = TRUE,
  log_file = NULL,
  patterns = NULL,
  prefer = NULL,
  strategy = c("error", "prefer", "first", "stable"),
  strict = TRUE,
  ignore_case = TRUE,
  fuzzy = FALSE,
  max_distance = 0.1,
  return = c("map", "list")
)

hm_infer_cols(data, patterns, required_keys = names(patterns), verbose = FALSE)
```

## Arguments

- data:

  Data frame whose column names are scanned.

- map:

  Named list where names are target keys (e.g., "G0","I0","TG") and
  values are NULL (to infer) or a user-supplied column name (to keep
  as-is).

- verbose:

  Logical; if TRUE, emits hm_inform() messages for matches and
  unresolved keys.

- log_file:

  Optional file path; if supplied, a human-readable mapping log is
  written there.

- patterns:

  Named list of character vectors, each giving candidate column names
  for a key (first match wins).

- prefer:

  Optional named list of character vectors with preferred column names
  for each key, used to resolve multiple matches deterministically.
  Matching is case-insensitive and exact against the provided names.

- strategy:

  One of c("error","prefer","first","stable") controlling resolution
  when there are multiple candidates. Default "error" (backward
  compatible).

  - "prefer": use `prefer` names first; else fall back to "stable"
    tie-break.

  - "first": take the first match in data's column order.

  - "stable": choose shortest name, then alphabetical.

- strict:

  Logical; if TRUE (default), missing matches error. If FALSE, missing
  matches leave `map[[key]]` as NULL and issue a warning.

- ignore_case:

  Logical; pass to grep(ignore.case = ...). Default TRUE.

- fuzzy:

  Logical; if TRUE and no regex matches are found, attempt a fuzzy match
  with agrep using `max_distance`. Default FALSE.

- max_distance:

  Numeric in \\\[0,1\]\\ passed to agrep when fuzzy = TRUE. Default 0.1.

- return:

  One of c("map","list"). "map" (default) invisibly returns the filled
  mapping list. "list" returns a list(map = ..., log = tibble) for
  auditing.

- required_keys:

  Character vector of keys that must resolve; otherwise an error is
  raised.

## Value

By default, invisibly returns the filled `map`. If return = "list",
returns a list(map = `named list`, log = `tibble`).

Named list mapping keys to column names; unresolved non-required keys
become `NA_character_`.

## Details

This helper produces a col_map you can pass to HealthMarkers functions
(e.g., fasting_is(), lipid-derived indices).

Backward compatibility:

- By default, strict = TRUE and strategy = "error" keep prior behavior:

  - Error if no match found.

  - Error if multiple candidates found.

- You can opt into smarter resolution via strategy = "prefer" or
  "first".

## Examples

``` r
df <- tibble::tibble(
  fasting_glucose = c(5.5, 6.1),
  fasting_insulin = c(60, 88),
  TG = c(120, 150),
  `HDL-c` = c(50, 45),
  age = c(55, 60)
)
spec <- list(G0 = NULL, I0 = NULL, TG = NULL, HDL_c = NULL)
# Backward-compatible: strict and "error" strategy
res1 <- infer_cols(df, spec, verbose = FALSE)
# Prefer/resolve ties deterministically
res2 <- infer_cols(df, spec, strategy = "prefer", verbose = TRUE)
#> HealthMarkers::infer_cols - G0 -> fasting_glucose (unique match)
#> HealthMarkers::infer_cols - I0 -> fasting_insulin (unique match)
#> HealthMarkers::infer_cols - TG -> TG (unique match)
#> HealthMarkers::infer_cols - HDL_c -> HDL-c (unique match)
# Get structured log
res3 <- infer_cols(df, spec, return = "list")
#> HealthMarkers::infer_cols - G0 -> fasting_glucose (unique match)
#> HealthMarkers::infer_cols - I0 -> fasting_insulin (unique match)
#> HealthMarkers::infer_cols - TG -> TG (unique match)
#> HealthMarkers::infer_cols - HDL_c -> HDL-c (unique match)
```

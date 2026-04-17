# Build a complete col_map with dictionary inference and alias materialization

Combines three previously separate steps into one call:

1.  User-supplied entries in `col_map` are treated as authoritative
    Layer 0 seeds – they are never overwritten.

2.  [`hm_col_report()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_col_report.md)
    fills any remaining unmatched keys via the full synonym dictionary
    (exact -\> case-insensitive -\> contains layers).

3.  Literal-name fallback: if a key in `keys` still has no match, but
    the key name exists verbatim as a column in `data`, it is used
    directly.

4.  Materialization: for every mapped key `k` where `col_map[[k]] != k`
    and `k` is *not* already a column in `data`, a copy
    `data[[k]] <- data[[col_map[[k]]]]` is added. This ensures that
    downstream pre-computation helpers (which check `names(data)` for
    dependency names) and their `compute()` closures (which use literal
    key names) both resolve correctly even when the physical column has
    a non-standard name.

## Usage

``` r
.hm_build_col_map(data, col_map, keys, fn = "")
```

## Arguments

- data:

  The data.frame being analysed (modified locally; original is not
  altered outside the function call).

- col_map:

  Named list of user-supplied key -\> column-name mappings, or `NULL`
  for full auto-inference.

- keys:

  Character vector of short internal keys to resolve.

- fn:

  Calling function name string (for messages).

## Value

A list with four elements:

- data:

  The (possibly augmented) data.frame.

- col_map:

  Complete named list of key -\> resolved column name.

- user_keys:

  Keys that were explicitly provided in the original `col_map` argument.

- inferred_keys:

  Keys resolved by dictionary inference or literal fallback (i.e., not
  in `user_keys`).

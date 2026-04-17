# One-level pre-computation of missing keys from existing raw columns

Given a dependency map (`deps`), for each key in `missing_keys` checks
whether its prerequisite columns are present in `data`. If yes, the key
is computed and added as a new column; otherwise an informational
message is emitted (when `verbose = TRUE`) indicating what to provide.

## Usage

``` r
.hm_precompute_from_deps(data, deps, missing_keys, fn = "", verbose = FALSE)
```

## Arguments

- data:

  data.frame to augment.

- deps:

  Named list of dependency definitions, each with:

  needs

  :   character vector of prerequisite column names

  describe

  :   short human-readable formula string

  compute

  :   function(data) returning a numeric vector

- missing_keys:

  character vector of key names to attempt to compute.

- fn:

  calling function name (for messages).

- verbose:

  logical.

## Value

A list with `$data` (augmented data.frame) and `$log` (character vector
of per-key messages).

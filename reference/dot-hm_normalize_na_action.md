# Normalise na_action aliases to canonical values

Converts the backward-compat aliases "ignore" and "warn" to "keep" so
that the rest of each function only needs to handle
c("keep","omit","error"). Returns a list with:

- na_action_eff: the canonical value ("keep", "omit", or "error")

- na_action_raw: the original matched value (for warn-diagnostic checks)

## Usage

``` r
.hm_normalize_na_action(na_action)
```

## Arguments

- na_action:

  character(1) already matched via match.arg()

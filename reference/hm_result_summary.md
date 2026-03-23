# Summarise a result tibble: count non-NA rows per output column

Produces a single string like: "fn(): results: col_a 28/30, col_b 30/30,
col_c 25/30"

## Usage

``` r
hm_result_summary(result, fn = NULL)
```

## Arguments

- result:

  A tibble / data.frame returned by a HealthMarkers function.

- fn:

  Optional function name prefixed to the message.

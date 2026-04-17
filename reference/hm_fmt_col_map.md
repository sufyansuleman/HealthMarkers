# Format a column-map resolution line for verbose output

Produces a single string like: "fn(): column map: G0 -\> 'col_a', I0 -\>
'col_b', ..."

## Usage

``` r
hm_fmt_col_map(col_map, fn = NULL)
```

## Arguments

- col_map:

  Named list of key -\> column-name mappings (only the keys actually
  used by the function need to be supplied).

- fn:

  Optional function name prefixed to the message.

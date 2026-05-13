# Normalise marker columns in a data frame

A convenience wrapper around
[`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
that applies a normalisation method to every selected numeric column in
a data frame and returns the modified data frame. The most common
use-case is to normalise the output of any HealthMarkers function —
especially domain functions such as
[`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md),
[`lipid_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/lipid_markers.md),
or
[`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md)
whose internal `normalize` argument currently has no effect.

## Usage

``` r
hm_normalize(
  data,
  cols = NULL,
  method = c("z", "inverse", "range", "robust"),
  skip_cols = NULL,
  ...
)
```

## Arguments

- data:

  A data frame (or tibble) containing marker columns to normalise.

- cols:

  Character vector of column names to normalise. If `NULL` (default),
  **all numeric columns** in `data` are normalised.

- method:

  One of `c("z","inverse","range","robust")`. Passed directly to
  [`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md):

  `"z"`

  :   z-score (mean 0, sd 1).

  `"inverse"`

  :   Rank-based inverse-normal transform (Rankit; default).

  `"range"`

  :   Min-max scaling to `feature_range` (default `[0, 1]`).

  `"robust"`

  :   Median/MAD scaling.

- skip_cols:

  Character vector of column names to leave untouched even if they are
  numeric (e.g. `c("age", "BMI")`). Ignored when `cols` is explicitly
  supplied.

- ...:

  Additional arguments forwarded to
  [`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
  (e.g. `feature_range`, `invnorm_denominator`, `ties`).

## Value

The input `data` with the selected columns replaced by their normalised
values. Class (tibble, data.frame, etc.) is preserved.

## See also

[`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
for single-vector normalisation.

## Examples

``` r
# Build a tiny data frame with pre-computed marker columns
df <- data.frame(
  age    = c(45, 52, 61),
  HOMA_IR = c(1.2, 3.4, 2.1),
  TyG     = c(8.1, 9.0, 8.6),
  NLR     = c(2.1, 3.5, 1.8)
)

marker_cols <- c("HOMA_IR", "TyG", "NLR")

# z-score normalise marker columns only
hm_normalize(df, cols = marker_cols, method = "z")
#>   age    HOMA_IR         TyG        NLR
#> 1  45 -0.9342606 -1.03490978 -0.4040951
#> 2  52  1.0548104  0.96098765  1.1388135
#> 3  61 -0.1205498  0.07392213 -0.7347184

# Inverse-normal transform, leaving age untouched
hm_normalize(df, method = "inverse", skip_cols = "age")
#>   age    HOMA_IR        TyG        NLR
#> 1  45 -0.9674216 -0.9674216  0.0000000
#> 2  52  0.9674216  0.9674216  0.9674216
#> 3  61  0.0000000  0.0000000 -0.9674216
```

# Normalize a numeric vector

Utility used across HealthMarkers to normalize numeric vectors with
several common schemes while handling edge cases (constant vectors,
all-NA, non-finite values) robustly. NA positions are preserved.

## Usage

``` r
normalize_vec(
  x,
  method = c("none", "z", "inverse", "range", "robust"),
  na_rm = TRUE,
  feature_range = c(0, 1),
  invnorm_denominator = c("n", "n+1", "blom"),
  ties = c("average", "first", "last", "random", "max", "min"),
  warn_constant = TRUE
)
```

## Arguments

- x:

  A numeric (or numeric-coercible) vector.

- method:

  One of c("none","z","inverse","range","robust"). Default "none".

- na_rm:

  Logical; remove NAs when estimating statistics (mean, sd, etc.).
  Default TRUE.

- feature_range:

  Numeric length-2 vector giving the target range for method = "range".
  Default c(0, 1).

- invnorm_denominator:

  One of c("n","n+1","blom") controlling the denominator of the
  inverse-normal transform:

  - "n": p = (r - 0.5) / n (Rankit; default)

  - "n+1": p = (r - 0.5) / (n + 1)

  - "blom": p = (r - 3/8) / (n + 1/4) (Blom, 1958)

- ties:

  Ties method passed to base::rank for method = "inverse". One of
  c("average","first","last","random","max","min"). Default "average".

- warn_constant:

  Logical; if TRUE, warn when input is constant and a zero vector (or
  lower bound for range) is returned. Default TRUE.

## Value

A numeric vector of the same length as x.

## Details

Methods:

- "none": return input as-is (no coercion; fully backward compatible).

- "z": z-score (mean 0, sd 1). Constant vectors return zeros (non-NA
  entries).

- "range": min-max to a target interval (default \\\[0,1\]\\). Constant
  vectors return the lower bound (mapped from zeros).

- "robust": median/MAD scaling. Constant vectors (MAD=0) return zeros.

- "inverse": rank-based inverse normal transform (normal scores).

## References

Beasley TM, Erickson S, Allison DB (2009). Rank-based inverse normal
transformations are increasingly used, but are they merited? Behav
Genet, 39(2):214-227. Leys C, Ley C, Klein O, Bernard P, Licata L
(2013). Detecting outliers: Do not use standard deviation around the
mean, use median absolute deviation around the median. Front Psychol,
4:241.
[doi:10.3389/fpsyg.2013.00241](https://doi.org/10.3389/fpsyg.2013.00241)
Bland JM, Altman DG (1996). Standard deviations and standard errors.
BMJ, 313(7047):41-42.

## Examples

``` r
x <- c(1, 2, 3, NA, 5)
normalize_vec(x, "none")
#> [1]  1  2  3 NA  5
normalize_vec(x, "z")
#> [1] -1.024695 -0.439155  0.146385        NA  1.317465
normalize_vec(x, "range", feature_range = c(-1, 1))
#> [1] -1.0 -0.5  0.0   NA  1.0
normalize_vec(x, "robust")
#> [1] -1.0117361 -0.3372454  0.3372454         NA  1.6862269
normalize_vec(x, "inverse")               # Rankit (default)
#> [1] -1.1503494 -0.3186394  0.3186394         NA  1.1503494
normalize_vec(x, "inverse", invnorm_denominator = "blom")
#> [1] -1.0491314 -0.2993069  0.2993069         NA  1.0491314
```

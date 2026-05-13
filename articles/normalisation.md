# Normalising marker outputs

## Scope

This article explains how to normalise biomarker outputs computed by
HealthMarkers functions using
[`hm_normalize()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md)
and the underlying
[`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md).

Normalisation is useful before:

- Genome-wide association studies (GWAS) — inverse-normal transform is
  standard
- Principal component analysis (PCA) — z-score or min-max
- Polygenic score construction — z-score or robust scaling
- Visualisation — when variables are on very different scales

## When to use `hm_normalize()` vs the built-in `normalize` argument

The `normalize` argument is only implemented in the insulin sensitivity
functions
([`fasting_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/fasting_is.md),
[`ogtt_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/ogtt_is.md),
[`adipo_is()`](https://sufyansuleman.github.io/HealthMarkers/reference/adipo_is.md),
[`all_insulin_indices()`](https://sufyansuleman.github.io/HealthMarkers/reference/all_insulin_indices.md)).
For all other domain functions —
[`glycemic_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/glycemic_markers.md),
[`lipid_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/lipid_markers.md),
[`renal_markers()`](https://sufyansuleman.github.io/HealthMarkers/reference/renal_markers.md),
etc. — the `normalize` argument has no effect.

[`hm_normalize()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md)
works post-computation on **any** data frame and covers all cases.

## Setup

``` r

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all()
} else {
  library(HealthMarkers)
}

sim_path <- system.file("extdata", "simulated_hm_data.rds", package = "HealthMarkers")
sim      <- readRDS(sim_path)[1:50, ]
```

## Step 1 — compute markers

``` r

out <- all_health_markers(
  data    = sim,
  which   = c("glycemic", "lipid", "renal", "inflammatory"),
  verbose = FALSE
)

# New marker columns added by all_health_markers()
new_cols <- setdiff(names(out), names(sim))
cat("New marker columns:", length(new_cols), "\n")
#> New marker columns: 139
```

## Step 2 — normalise with `hm_normalize()`

### Z-score (mean 0, sd 1)

``` r

out_z <- hm_normalize(out, cols = new_cols, method = "z")
round(head(out_z[new_cols[1:5]]), 3)
#>      whr    MAP BUN_Cr_ratio   VLDL non_HDL
#> 1 -0.206 -1.502       -1.410  1.396   0.883
#> 2  0.631 -0.948        0.710 -1.296   0.332
#> 3 -0.762 -1.827        0.189 -0.909   1.435
#> 4  0.137 -1.567       -0.322 -1.075  -1.253
#> 5 -1.063  1.559       -0.102 -0.277  -1.105
#> 6 -0.438 -1.469        1.397 -0.676  -0.239
```

### Rank-based inverse-normal transform (Rankit)

The most common choice for GWAS. Normally distributed regardless of the
original distribution.

``` r

out_int <- hm_normalize(out, cols = new_cols, method = "inverse")
round(head(out_int[new_cols[1:5]]), 3)
#>      whr    MAP BUN_Cr_ratio   VLDL non_HDL
#> 1 -0.025 -1.341       -1.645  1.341   0.739
#> 2  0.739 -0.878        0.739 -1.476   0.279
#> 3 -0.739 -1.881        0.385 -1.126   1.476
#> 4  0.385 -1.476       -0.279 -1.227  -0.954
#> 5 -1.341  1.476        0.025 -0.151  -0.806
#> 6 -0.279 -1.227        1.126 -0.739  -0.126
```

Use the Blom denominator instead of Rankit:

``` r

out_blom <- hm_normalize(out, cols = new_cols, method = "inverse",
                          invnorm_denominator = "blom")
```

### Min-max scaling to \[0, 1\]

``` r

out_range <- hm_normalize(out, cols = new_cols, method = "range")
round(head(out_range[new_cols[1:5]]), 3)
#>     whr   MAP BUN_Cr_ratio  VLDL non_HDL
#> 1 0.366 0.152        0.021 0.631   0.746
#> 2 0.559 0.288        0.553 0.046   0.586
#> 3 0.237 0.072        0.422 0.130   0.906
#> 4 0.445 0.136        0.294 0.094   0.126
#> 5 0.167 0.904        0.349 0.267   0.169
#> 6 0.312 0.160        0.725 0.181   0.420
```

Scale to a custom interval (e.g. \[-1, 1\]):

``` r

out_range2 <- hm_normalize(out, cols = new_cols, method = "range",
                            feature_range = c(-1, 1))
```

### Robust median/MAD scaling

Less sensitive to outliers than z-score.

``` r

out_rob <- hm_normalize(out, cols = new_cols, method = "robust")
round(head(out_rob[new_cols[1:5]]), 3)
#>      whr    MAP BUN_Cr_ratio   VLDL non_HDL
#> 1 -0.055 -1.878       -1.216  1.982   0.753
#> 2  0.878 -1.258        0.786 -1.363   0.290
#> 3 -0.675 -2.242        0.294 -0.881   1.217
#> 4  0.327 -1.951       -0.189 -1.087  -1.043
#> 5 -1.010  1.550        0.020 -0.096  -0.919
#> 6 -0.313 -1.841        1.434 -0.592  -0.190
```

## Protecting covariate columns

Use `skip_cols` to exclude variables you want to keep on their original
scale. When `cols = NULL` (the default),
[`hm_normalize()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md)
targets all numeric columns; `skip_cols` removes covariates from that
set.

``` r

# Normalise all numeric columns except age, BMI, sex-encoded variables
out_protected <- hm_normalize(out, method = "inverse",
                               skip_cols = c("age", "BMI", "sex"))
```

## Using `normalize_vec()` on a single vector

[`hm_normalize()`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md)
calls
[`normalize_vec()`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
internally. You can also use it directly:

``` r

x <- out[[new_cols[1]]]

normalize_vec(x, method = "z")[1:10]
#>  [1] -0.2056805  0.6308316 -0.7621945  0.1368883 -1.0631482 -0.4375478
#>  [7] -0.9907357  2.5049090  0.5142453 -0.6308403
normalize_vec(x, method = "inverse")[1:10]
#>  [1] -0.02506891  0.73884685 -0.73884685  0.38532047 -1.34075503 -0.27931903
#>  [7] -1.12639113  1.88079361  0.67448975 -0.43991317
normalize_vec(x, method = "range")[1:10]
#>  [1] 0.3656107 0.5590158 0.2369424 0.4448141 0.1673606 0.3120021 0.1841027
#>  [8] 0.9923103 0.5320606 0.2673120
normalize_vec(x, method = "robust")[1:10]
#>  [1] -0.05450437  0.87759325 -0.67460932  0.32720869 -1.00995204 -0.31286630
#>  [7] -0.92926521  2.96581519  0.74768494 -0.52824568
```

## Comparison of methods

``` r

compare <- data.frame(
  raw     = x,
  z       = normalize_vec(x, "z"),
  INT     = normalize_vec(x, "inverse"),
  range01 = normalize_vec(x, "range"),
  robust  = normalize_vec(x, "robust")
)
round(head(compare, 10), 3)
#>      raw      z    INT range01 robust
#> 1  0.884 -0.206 -0.025   0.366 -0.055
#> 2  1.002  0.631  0.739   0.559  0.878
#> 3  0.806 -0.762 -0.739   0.237 -0.675
#> 4  0.932  0.137  0.385   0.445  0.327
#> 5  0.763 -1.063 -1.341   0.167 -1.010
#> 6  0.852 -0.438 -0.279   0.312 -0.313
#> 7  0.774 -0.991 -1.126   0.184 -0.929
#> 8  1.266  2.505  1.881   0.992  2.966
#> 9  0.986  0.514  0.674   0.532  0.748
#> 10 0.824 -0.631 -0.440   0.267 -0.528
```

Summary statistics after normalisation:

``` r

round(sapply(compare, function(v) c(mean = mean(v, na.rm = TRUE),
                                     sd   = sd(v, na.rm = TRUE),
                                     min  = min(v, na.rm = TRUE),
                                     max  = max(v, na.rm = TRUE))), 3)
#>        raw      z    INT range01 robust
#> mean 0.913  0.000  0.000   0.413  0.175
#> sd   0.141  1.000  0.997   0.231  1.114
#> min  0.661 -1.787 -2.326   0.000 -1.817
#> max  1.271  2.538  2.326   1.000  3.003
```

## See also

- [`?hm_normalize`](https://sufyansuleman.github.io/HealthMarkers/reference/hm_normalize.md)
  — full argument reference
- [`?normalize_vec`](https://sufyansuleman.github.io/HealthMarkers/reference/normalize_vec.md)
  — single-vector normalisation details
- [Fasting insulin
  sensitivity](https://sufyansuleman.github.io/HealthMarkers/articles/fasting_is.md)
  — `normalize` argument used at compute time

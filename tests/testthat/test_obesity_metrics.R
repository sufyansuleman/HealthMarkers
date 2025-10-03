# tests/testthat/test_obesity_indices.R

library(testthat)
library(tibble)

# Base synthetic data for testing (uses kg, m, and cm-like magnitudes for waist/hip)
base_df <- tibble(
  wt    = c(80, 90),  # kg
  ht    = c(2, 2),    # m
  waist = c(1, 2),    # cm-like numeric used consistently across formulas
  hip   = c(1, 1),    # cm-like numeric
  sex   = c(0, 1)     # 0 = male, 1 = female
)

# 1) Core metrics without options
test_that("core metrics compute correctly without options", {
  out <- obesity_indices(
    data = base_df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    weight_unit = "kg",
    height_unit = "m",
    adjust_WHR = FALSE,
    include_RFM = FALSE
  )

  # weight_kg and height_m
  expect_equal(out$weight_kg, c(80, 90))
  expect_equal(out$height_m, c(2, 2))

  # BMI and category
  expect_equal(out$BMI, c(20, 22.5))
  expect_equal(out$BMI_cat, rep("Normal weight", 2))

  # WHR and waist_to_height_ratio
  expect_equal(out$WHR, c(1, 2))
  expect_equal(out$waist_to_height_ratio, c(0.5, 1))

  # waist_to_BMI_ratio and weight_to_height_ratio
  expect_equal(out$waist_to_BMI_ratio, c(1 / 20, 2 / 22.5))
  expect_equal(out$weight_to_height_ratio, c(40, 45))

  # Advanced indices produce finite numerics
  expect_true(all(is.finite(out$AVI)))
  expect_true(all(is.finite(out$BAI)))
  expect_true(all(is.finite(out$ABSI)))
  expect_true(all(is.finite(out$BRI)))
  expect_true(all(is.finite(out$CI)))
})

# 2) Test adjust_WHR and include_RFM
test_that("adjust_WHR adds WHRadjBMI and include_RFM adds RFM", {
  out2 <- obesity_indices(
    data = base_df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    sex = sex,
    weight_unit = "kg",
    height_unit = "m",
    adjust_WHR = TRUE,
    include_RFM = TRUE
  )
  # WHRadjBMI exists and numeric
  expect_true("WHRadjBMI" %in% names(out2))
  expect_type(out2$WHRadjBMI, "double")
  # Residuals sum to zero (by construction for linear model residuals)
  expect_equal(sum(out2$WHRadjBMI), 0)

  # RFM computation (64 - 20*(height_m/waist) + 12*sex)
  expected_rfm <- c(
    64 - 20 * (2 / 1) + 12 * 0,
    64 - 20 * (2 / 2) + 12 * 1
  )
  expect_equal(out2$RFM, expected_rfm)
})

# 3) Units conversion
test_that("unit conversion works for lb and cm", {
  df_units <- tibble(
    wt_lb = 220,    # lb
    ht_cm = 180,    # cm
    waist = 90,     # cm-like numeric
    hip   = 100     # cm-like numeric
  )
  out <- obesity_indices(
    data = df_units,
    weight = wt_lb,
    height = ht_cm,
    waist = waist,
    hip = hip,
    weight_unit = "lb",
    height_unit = "cm",
    adjust_WHR = FALSE,
    include_RFM = FALSE
  )
  expect_equal(out$weight_kg, 220 * 0.45359237, tolerance = 1e-8)
  expect_equal(out$height_m, 180 / 100, tolerance = 1e-8)
  expect_equal(out$WHR, 90 / 100, tolerance = 1e-8)
})

# 4) Error handling
test_that("errors on missing columns or missing sex when RFM requested", {
  # Missing waist column
  df_missing <- base_df[, setdiff(names(base_df), "waist")]
  expect_error(
    obesity_indices(df_missing, wt, ht, waist, hip),
    "missing required columns"
  )

  # include_RFM = TRUE without sex argument
  expect_error(
    obesity_indices(
      data = base_df,
      weight = wt,
      height = ht,
      waist = waist,
      hip = hip,
      include_RFM = TRUE
    ),
    "must be provided to compute RFM"
  )
})

# 5) na_action policies
test_that("na_action policies behave as expected", {
  df_na <- tibble(
    wt    = c(80, NA_real_),
    ht    = c(2, 2),
    waist = c(1, 2),
    hip   = c(1, 1)
  )
  # error -> abort (suppress high-missingness warning)
  expect_error(
    suppressWarnings(
      obesity_indices(df_na, wt, ht, waist, hip,
                      weight_unit = "kg", height_unit = "m",
                      na_action = "error")
    ),
    "required inputs contain missing values"
  )
  # omit -> drop rows with NA
  out_omit <- suppressWarnings(
    obesity_indices(df_na, wt, ht, waist, hip,
                    weight_unit = "kg", height_unit = "m",
                    na_action = "omit")
  )
  expect_equal(nrow(out_omit), 1L)
  expect_equal(out_omit$BMI, 80 / (2^2))

  # keep -> NA propagates into BMI
  out_keep <- suppressWarnings(
    obesity_indices(df_na, wt, ht, waist, hip,
                    weight_unit = "kg", height_unit = "m",
                    na_action = "keep")
  )
  expect_true(is.na(out_keep$BMI[2]))
})

# 6) Extreme checks and capping
test_that("check_extreme='warn' detects extremes and 'cap' truncates and warns", {
  df_ext <- tibble(
    wt    = 80,
    ht    = 200,   # cm or m depending on height_unit; we pass m below
    waist = 500,   # extreme
    hip   = 100
  )
  # warn: detection message
  expect_warning(
    obesity_indices(df_ext, wt, ht, waist, hip, height_unit = "m", check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values \\(not altered\\)"
  )
  # cap: truncates waist to 200 (default rule) so WHR == 200/100 = 2
  expect_warning(
    out_cap <- obesity_indices(df_ext, wt, ht, waist, hip, height_unit = "m", check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values"
  )
  expect_equal(out_cap$WHR, 2, tolerance = 1e-8)
})

# 7) Denominator-zero summary warning
test_that("denominator zero emits a single summary warning", {
  df_zero <- tibble(
    wt    = 80,
    ht    = 2,
    waist = 1,
    hip   = 0  # zero denominator for WHR
  )
  expect_warning(
    out <- obesity_indices(df_zero, wt, ht, waist, hip),
    "zero denominators detected"
  )
  expect_true(is.na(out$WHR))
})

# 8) include_RFM with invalid sex values warns and sets NA
test_that("include_RFM warns on invalid sex values and sets NA", {
  df_bad_sex <- tibble(
    wt    = c(80, 70),
    ht    = c(2, 1.8),
    waist = c(1, 0.9),
    hip   = c(1, 1.0),
    sex   = c(2, -1)  # invalid codes
  )
  expect_warning(
    out <- obesity_indices(df_bad_sex, wt, ht, waist, hip, sex, include_RFM = TRUE),
    "sex' contains .* values not in \\{0,1\\}"
  )
  expect_true(all(is.na(out$RFM)))
})

# 9) Verbose prints progress and completion summary
test_that("verbose prints progress and completion summary", {
  expect_message(
    obesity_indices(base_df, wt, ht, waist, hip, verbose = TRUE),
    "-> obesity_indices: validating inputs"
  )
  expect_message(
    obesity_indices(base_df, wt, ht, waist, hip, verbose = TRUE),
    "Completed obesity_indices: .* rows; NA/Inf ->"
  )
})

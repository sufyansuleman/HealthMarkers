# tests/testthat/test_adiposity_sds.R

library(testthat)
library(tibble)

# Shared reference for tests
ref <- list(
  BMI   = c(mean = 23, sd = 4),
  waist = c(mean = 80, sd = 12)
)

test_that("errors if data missing required columns", {
  df <- tibble(BMI = 23)
  expect_error(
    adiposity_sds(df, ref = ref),
    "missing required columns: waist"
  )
})

test_that("computes correct SDS for single row", {
  df <- tibble(BMI = 27, waist = 92)
  out <- adiposity_sds(df, ref, diagnostics = FALSE)  # positional ref (back-compat)
  expect_equal(out$BMI_SDS,  (27 - 23) / 4)
  expect_equal(out$waist_SDS, (92 - 80) / 12)
})

test_that("raw extreme scanning (cap) caps values", {
  df <- tibble(BMI = c(120, 22), waist = c(300, 60))
  out <- NULL
  expect_warning(
    out <- adiposity_sds(
      df,
      ref = ref,
      check_raw_extreme = TRUE,                 # legacy alias (warns)
      raw_extreme_rules = list(BMI = c(5, 80))  # legacy alias (warns)
    ),
    "deprecated"
  )
  expect_true(all(abs(out$BMI_SDS) < 30))
})

test_that("computes SDS on identity mapping", {
  df <- tibble(BMI = c(23, 27), waist = c(80, 92))
  out <- adiposity_sds(df, ref = ref)
  expect_equal(out$BMI_SDS, c(0, (27-23)/4))
  expect_equal(out$waist_SDS, c(0, (92-80)/12))
})

test_that("na_action policies: keep, omit, error", {
  df <- tibble(BMI = c(25, NA, 21), waist = c(90, 70, 80))
  expect_equal(nrow(adiposity_sds(df, ref = ref, na_action = "keep", diagnostics = FALSE)), 3)
  expect_equal(nrow(adiposity_sds(df, ref = ref, na_action = "omit", diagnostics = FALSE)), 2)
  expect_error(adiposity_sds(df, ref = ref, na_action = "error", diagnostics = FALSE),
               "rows have missing values")
})

test_that("check_extreme with cap/NA/error adjusts raw values", {
  df <- tibble(BMI = c(120, 22), waist = c(300, 60))
  expect_true(all(abs(adiposity_sds(df, ref = ref, check_extreme = TRUE, extreme_action = "cap", diagnostics = FALSE)$BMI_SDS) < 30))
  expect_true(anyNA(adiposity_sds(df, ref = ref, check_extreme = TRUE, extreme_action = "NA", diagnostics = FALSE)$BMI_SDS))
  expect_error(adiposity_sds(df, ref = ref, check_extreme = TRUE, extreme_action = "error", diagnostics = FALSE),
               "raw extremes")
})

test_that("SDS extreme handling respects sds_cap and extreme_action", {
  df <- tibble(BMI = c(23 + 100*4, 23), waist = c(80, 80))
  out_cap <- adiposity_sds(df, ref = ref, extreme_action = "cap", sds_cap = 6)
  expect_lte(max(abs(out_cap$BMI_SDS)), 6)
  expect_error(adiposity_sds(df, ref = ref, extreme_action = "error", sds_cap = 6), "SDS beyond")
})

test_that("package-level verbosity emits messages", {
  old <- getOption("healthmarkers.verbose", "none")
  on.exit(options(healthmarkers.verbose = old), add = TRUE)
  options(healthmarkers.verbose = "inform")
  df <- tibble(BMI = 23, waist = 80)
  expect_message(adiposity_sds(df, ref = ref), "Completed adiposity_sds:")
})

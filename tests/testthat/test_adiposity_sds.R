# tests/testthat/test_adiposity_sds.R

library(testthat)

# 1) ref must be a named list
test_that("errors if ref is not a named list", {
  df <- tibble::tibble(x = 1)
  expect_error(
    adiposity_sds(df, ref = NULL),
    "`ref` must be a named list"
  )
  # unnamed list
  bad_ref <- list(c(mean = 1, sd = 1))
  expect_error(
    adiposity_sds(df, ref = bad_ref),
    "`ref` must be a named list"
  )
})

# 2) each ref[[nm]] must be numeric length 2 named mean & sd
test_that("errors if any ref element is not numeric length 2 with names mean & sd", {
  df <- tibble::tibble(BMI = 20)
  # length != 2
  ref1 <- list(BMI = c(mean = 18, sd = 4, extra = 1))
  expect_error(
    adiposity_sds(df, ref = ref1),
    "ref\\[\\[\"BMI\"\\]\\] must be a numeric vector with names 'mean' and 'sd'"
  )
  # missing names
  ref2 <- list(BMI = c(18,4))
  expect_error(
    adiposity_sds(df, ref = ref2),
    "ref\\[\\[\"BMI\"\\]\\] must be a numeric vector with names 'mean' and 'sd'"
  )
})

# 3) sd must be > 0
test_that("errors if any sd <= 0", {
  df <- tibble::tibble(BMI = 20)
  ref <- list(BMI = c(mean = 18, sd = 0))
  expect_error(
    adiposity_sds(df, ref = ref),
    "`ref\\[\\[\"BMI\"\\]\\]\\[\"sd\"\\]` must be > 0"
  )
})

# 4) data must contain each ref name
test_that("errors if data is missing required columns", {
  ref <- list(BMI = c(mean = 18, sd = 4), waist = c(mean = 75, sd = 10))
  df <- tibble::tibble(BMI = 20)
  expect_error(
    adiposity_sds(df, ref = ref),
    "missing required columns: waist"
  )
})

# 5) correct SDS for single‐row input
test_that("computes correct SDS for single row", {
  df <- tibble::tibble(
    BMI          = 22,
    waist        = 80,
    body_fat_pct = 25
  )
  ref <- list(
    BMI           = c(mean = 18,   sd = 4),
    waist         = c(mean = 75,   sd = 5),
    body_fat_pct  = c(mean = 20,   sd = 2)
  )
  out <- adiposity_sds(df, ref, verbose = FALSE)
  expect_named(out, c("BMI_SDS", "waist_SDS", "body_fat_pct_SDS"))
  expect_equal(out$BMI_SDS,          (22 - 18) / 4)
  expect_equal(out$waist_SDS,        (80 - 75) / 5)
  expect_equal(out$body_fat_pct_SDS, (25 - 20) / 2)
})

# 6) vectorized input: multiple rows
test_that("vectorized input returns one SDS per row", {
  df <- tibble::tibble(
    BMI   = c(18, 22, 26),
    waist = c(70, 80, 90)
  )
  ref <- list(
    BMI   = c(mean = 20, sd = 5),
    waist = c(mean = 80, sd = 10)
  )
  out <- adiposity_sds(df, ref)
  expect_equal(nrow(out), 3)
  # second row: (22-20)/5, (80-80)/10
  expect_equal(out$BMI_SDS[2],   (22 - 20) / 5)
  expect_equal(out$waist_SDS[2], (80 - 80) / 10)
})

# 7) verbose = TRUE prints progress message
test_that("verbose = TRUE prints a progress message", {
  df <- tibble::tibble(BMI = 22, waist = 80)
  ref <- list(
    BMI   = c(mean = 20, sd = 5),
    waist = c(mean = 75, sd = 5)
  )
  expect_message(
    adiposity_sds(df, ref, verbose = TRUE),
    "-> computing SDS for: BMI, waist"
  )
})

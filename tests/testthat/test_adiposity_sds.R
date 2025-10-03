# tests/testthat/test_adiposity_sds.R
library(testthat)
library(tibble)

ref_basic <- list(
  BMI   = c(mean = 20, sd = 5),
  waist = c(mean = 80, sd = 10)
)

# Helper to capture (and muffle) warnings, returning their messages
capture_warnings <- function(expr) {
  msgs <- character()
  withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  msgs
}

test_that("errors if ref is not a named list", {
  df <- tibble(BMI = 21)
  expect_error(adiposity_sds(df, ref = NULL),
               "must be a named list of mean/sd vectors")
  bad_ref <- list(c(mean = 1, sd = 1))
  expect_error(adiposity_sds(df, ref = bad_ref),
               "must be a named list of mean/sd vectors")
})

test_that("errors if any ref element lacks mean/sd names", {
  df <- tibble(BMI = 21)
  ref_bad_names <- list(BMI = c(18, 4))
  expect_error(adiposity_sds(df, ref = ref_bad_names),
               "ref\\[\\['BMI'\\]\\] must be numeric with names mean, sd")
})

test_that("errors if any sd <= 0", {
  df <- tibble(BMI = 21)
  ref_sd0 <- list(BMI = c(mean = 18, sd = 0))
  expect_error(adiposity_sds(df, ref = ref_sd0),
               "mean/sd must be finite; sd>0")
})

test_that("errors if data missing required columns", {
  df <- tibble(BMI = 22)
  ref <- list(BMI = c(mean = 20, sd = 5), waist = c(mean = 80, sd = 10))
  expect_error(adiposity_sds(df, ref = ref),
               "missing required columns: waist")
})

test_that("computes correct SDS for single row", {
  df <- tibble(BMI = 22, waist = 85, body_fat_pct = 27)
  ref <- list(
    BMI          = c(mean = 18, sd = 4),
    waist        = c(mean = 75, sd = 5),
    body_fat_pct = c(mean = 20, sd = 2)
  )
  out <- adiposity_sds(df, ref, diagnostics = FALSE)
  expect_named(out, c("BMI_SDS", "waist_SDS", "body_fat_pct_SDS"))
  expect_equal(out$BMI_SDS, (22 - 18) / 4)
  expect_equal(out$waist_SDS, (85 - 75) / 5)
  expect_equal(out$body_fat_pct_SDS, (27 - 20) / 2)
})

test_that("vectorized input returns one SDS per row", {
  df <- tibble(
    BMI   = c(18, 22, 26),
    waist = c(70, 80, 90)
  )
  out <- adiposity_sds(df, ref = ref_basic)
  expect_equal(nrow(out), 3)
  expect_equal(out$BMI_SDS[2], (22 - 20) / 5)
  expect_equal(out$waist_SDS[2], 0)
})

test_that("verbose prints starting message", {
  df <- tibble(BMI = 22, waist = 80)
  expect_message(
    adiposity_sds(df, ref = ref_basic, verbose = TRUE),
    "adiposity_sds: starting"
  )
})

test_that("na_action policies", {
  df <- tibble(BMI = c(21, NA, 19), waist = c(82, 90, NA))
  out_keep <- adiposity_sds(df, ref = ref_basic, na_action = "keep", diagnostics = FALSE)
  expect_equal(nrow(out_keep), 3)
  out_omit <- adiposity_sds(df, ref = ref_basic, na_action = "omit", diagnostics = FALSE)
  expect_equal(nrow(out_omit), 1)
  expect_error(
    adiposity_sds(df, ref = ref_basic, na_action = "error", diagnostics = FALSE),
    "rows have missing values"
  )
})

test_that("omit to zero rows returns empty tibble with columns", {
  df <- tibble(BMI = c(NA, NA), waist = c(NA, NA))
  out <- adiposity_sds(df, ref = ref_basic, na_action = "omit", diagnostics = FALSE)
  expect_equal(nrow(out), 0)
  expect_named(out, c("BMI_SDS", "waist_SDS"))
})

test_that("raw extreme scanning (warn) captured", {
  df <- tibble(BMI = c(10, 150), waist = c(60, 400))
  msgs <- capture_warnings(
    adiposity_sds(
      df, ref = ref_basic,
      check_raw_extreme = TRUE,
      extreme_action = "warn",
      diagnostics = TRUE,
      raw_extreme_rules = list(BMI = c(5, 80), waist = c(20, 250))
    )
  )
  expect_true(any(grepl("raw extremes detected", msgs)))
})

test_that("raw extreme scanning (cap) caps values", {
  df <- tibble(BMI = c(10, 150), waist = c(60, 400))
  out <- adiposity_sds(
    df, ref = ref_basic,
    check_raw_extreme = TRUE,
    extreme_action = "cap",
    raw_extreme_rules = list(BMI = c(5, 80), waist = c(20, 250)),
    diagnostics = FALSE
  )
  # Raw extremes are capped (BMI->80, waist->250) then SDS > sds_cap is capped to sds_cap (=6).
  expect_equal(out$BMI_SDS[2], 6)
  expect_equal(out$waist_SDS[2], 6)
})

test_that("SDS extreme capping works", {
  df <- tibble(BMI = c(20, 200))
  ref <- list(BMI = c(mean = 20, sd = 5))
  out <- adiposity_sds(df, ref = ref, extreme_action = "cap", diagnostics = FALSE)
  expect_equal(out$BMI_SDS[2], 6)
})

test_that("SDS extreme warn leaves raw SDS", {
  df <- tibble(BMI = c(20, 200))
  ref <- list(BMI = c(mean = 20, sd = 5))
  msgs <- capture_warnings(adiposity_sds(df, ref = ref, extreme_action = "warn"))
  expect_true(any(grepl("SDS beyond ±6", msgs)))
  expect_equal((200 - 20) / 5, 36)
})

test_that("return_summary structure", {
  df <- tibble(BMI = c(20, 200))
  ref <- list(BMI = c(mean = 20, sd = 5))
  res <- adiposity_sds(df, ref = ref, extreme_action = "cap",
                       return_summary = TRUE, diagnostics = FALSE)
  expect_named(res, c("data", "summary", "warnings"))
  expect_true(is.data.frame(res$data))
  expect_true(is.list(res$summary))
  expect_true("rows_in" %in% names(res$summary))
})

test_that("legacy aliases work (captured warnings)", {
  df <- tibble(BMI = c(20, NA))
  ref <- list(BMI = c(mean = 20, sd = 5))
  msgs1 <- capture_warnings(adiposity_sds(df, ref = ref, na_strategy = "keep"))
  expect_true(any(grepl("na_strategy", msgs1)))
  msgs2 <- capture_warnings(adiposity_sds(df, ref = ref, extreme_strategy = "warn"))
  expect_true(any(grepl("extreme_strategy", msgs2)))
})

test_that("diagnostics = FALSE suppresses warnings", {
  df <- tibble(BMI = c("20", "bad"), waist = c(80, 90))
  expect_silent(adiposity_sds(df, ref = ref_basic, diagnostics = FALSE))
})

test_that("high extreme SDS proportion warning captured", {
  df <- tibble(BMI = c(20, 200, 180))
  ref <- list(BMI = c(mean = 20, sd = 5))
  msgs <- capture_warnings(adiposity_sds(df, ref = ref, extreme_action = "warn"))
  expect_true(any(grepl("high extreme SDS proportion", msgs)))
})

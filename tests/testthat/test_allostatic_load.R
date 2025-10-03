# File: tests/testthat/test_allostatic_load.R

library(testthat)
library(tibble)

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

test_that("basic multi-biomarker scoring uses strict >", {
  df <- tibble(
    A = c(1, 5, 10, NA),
    B = c(0, 2, 3, 4),
    C = c(100, 50, 25, 10)
  )
  thr <- list(A = 4, B = 2, C = 30)
  out <- allostatic_load(df, thr, na_action = "zero", diagnostics = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "AllostaticLoad")
  expect_equal(out$AllostaticLoad, c(1L, 2L, 2L, 1L))
})

test_that("single biomarker uses inclusive >=", {
  df <- tibble(X = c(5, 10, 11))
  thr <- list(X = 10)
  out <- allostatic_load(df, thr, diagnostics = FALSE)
  expect_equal(out$AllostaticLoad, c(0L, 1L, 1L))
})

test_that("input validation errors", {
  df <- tibble(A = 1:3)
  expect_error(allostatic_load("not df", list(A = 1)), "data.frame or tibble")
  # empty list triggers 'named list' error first in current implementation
  expect_error(allostatic_load(df, list()), "named list of numeric cutoffs")
  # Empty name
  bad_thr1 <- list(3); names(bad_thr1) <- ""
  expect_error(allostatic_load(df, bad_thr1), "non-empty")
  # Duplicate names
  bad_thr2 <- list(1, 2); names(bad_thr2) <- c("A", "A")
  expect_error(allostatic_load(df, bad_thr2), "Duplicate names")
  # Non length-1 numeric
  expect_error(allostatic_load(df, list(A = c(1, 2))), "length-1")
  # Missing column
  expect_error(allostatic_load(df, list(B = 2)), "Missing biomarker columns")
})

test_that("biomarker must be numeric", {
  df <- tibble(A = letters[1:3])
  expect_error(allostatic_load(df, list(A = 1)), "must be numeric")
})

test_that("na_action = error stops on NA", {
  df <- tibble(A = c(1, NA, 3))
  expect_error(
    allostatic_load(df, list(A = 2), na_action = "error"),
    "Missing/non-finite values present"
  )
})

test_that("na_action = warn_zero emits expected warning set", {
  df <- tibble(A = c(NA, 3, Inf))
  msgs <- capture_warnings(allostatic_load(df, list(A = 2), na_action = "warn_zero"))
  expect_true(any(grepl("Missing/non-finite values detected", msgs)))
})

test_that("na_action = keep alias behaves like zero (no warning)", {
  df <- tibble(A = c(NA, 3))
  out <- allostatic_load(df, list(A = 2), na_action = "keep", diagnostics = FALSE)
  expect_equal(out$AllostaticLoad, c(0L, 1L))
})

test_that("high missingness warning threshold triggers", {
  df <- tibble(A = c(NA, NA, 5, 6, 7))
  msgs <- capture_warnings(
    allostatic_load(df, list(A = 2), na_action = "warn_zero", na_warn_prop = 0.2)
  )
  expect_true(any(grepl("High missingness", msgs)))
})

test_that("zero-row data returns zero-row tibble", {
  df <- tibble(A = numeric(0))
  out <- allostatic_load(df, list(A = 2), diagnostics = FALSE)
  expect_equal(nrow(out), 0)
  expect_named(out, "AllostaticLoad")
})

test_that("extreme SDS scan warn + error + ignore", {
  df <- tibble(BP_sds = c(-0.5, 7, 6.5), HR_sds = c(0, -7, 5))
  thr <- list(BP_sds = 1.5, HR_sds = 1.5)
  expect_warning(
    allostatic_load(df, thr, check_extreme_sds = TRUE, sds_limit = 6,
                    extreme_sds_action = "warn"),
    "Extreme SDS-like values"
  )
  expect_error(
    allostatic_load(df, thr, check_extreme_sds = TRUE, sds_limit = 6,
                    extreme_sds_action = "error"),
    "Extreme SDS-like values",
    class = "healthmarkers_allo_error_extreme_sds"
  )
  expect_silent(
    allostatic_load(df, thr, check_extreme_sds = TRUE, sds_limit = 6,
                    extreme_sds_action = "ignore", diagnostics = FALSE)
  )
})

test_that("diagnostics = FALSE suppresses non-critical warnings", {
  df <- tibble(A = c(NA, 5, 6))
  thr <- list(A = 4)
  expect_silent(allostatic_load(df, thr, na_action = "warn_zero", diagnostics = FALSE))
})

test_that("verbose messages emitted", {
  df <- tibble(A = c(1, 5), B = c(0, 3))
  thr <- list(A = 2, B = 1)
  expect_message(allostatic_load(df, thr, verbose = TRUE, diagnostics = FALSE),
                 "Computing Allostatic Load")
  expect_message(allostatic_load(df, thr, verbose = TRUE, diagnostics = FALSE),
                 "Completed:")
})

test_that("return_summary returns structured list", {
  df <- tibble(A = c(1, 5, 6))
  thr <- list(A = 2)
  res <- allostatic_load(df, thr, return_summary = TRUE, diagnostics = FALSE)
  expect_named(res, c("data", "summary", "warnings"))
  expect_true(is.data.frame(res$data))
  expect_true(is.list(res$summary))
  expect_true(all(c("rows","biomarkers","total_flags","mean_flags") %in% names(res$summary)))
  expect_equal(res$summary$total_flags, sum(res$data$AllostaticLoad))
})

test_that("correct sums for multi vs single additivity", {
  df <- tibble(A = c(3, 4, 5), B = c(1, 2, 3))
  thr_multi <- list(A = 4, B = 2)
  out_multi <- allostatic_load(df, thr_multi, diagnostics = FALSE)
  expect_equal(out_multi$AllostaticLoad, c(0L, 0L, 2L))
  thr_single <- list(A = 4)
  out_single <- allostatic_load(df, thr_single, diagnostics = FALSE)
  expect_equal(out_single$AllostaticLoad, c(0L, 1L, 1L))
})

test_that("warnings aggregated include summary line", {
  df <- tibble(A = c(NA, NA, 1, 2, 3))
  thr <- list(A = 2)
  msgs <- capture_warnings(allostatic_load(df, thr, na_action = "warn_zero"))
  expect_true(any(grepl("Missing/non-finite values detected", msgs)))
})

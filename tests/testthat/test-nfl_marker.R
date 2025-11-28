library(testthat)

cm <- list(nfl = "NfL")

test_that("mapping validation and missing columns error", {
  df <- data.frame(NfL = c(10, 20))

  expect_error(
    nfl_marker(df, list()),
    class = "healthmarkers_nfl_error_missing_map"
  )

  expect_error(
    nfl_marker(df, list(nfl = "")),
    class = "healthmarkers_nfl_error_bad_map_values"
  )

  expect_error(
    nfl_marker(data.frame(X = 1), list(nfl = "NfL")),
    class = "healthmarkers_nfl_error_missing_columns"
  )
})

test_that("verbose emits progress messages", {
  df <- data.frame(NfL = c(12, 35))

  expect_message(
    nfl_marker(df, cm, verbose = TRUE),
    "-> nfl_marker: preparing inputs"
  )

  expect_message(
    nfl_marker(df, cm, verbose = TRUE),
    "-> nfl_marker: computing"
  )
})

test_that("numeric coercion warning when non-numeric introduces NAs", {
  df <- data.frame(NfL = c("12", "oops", "35"))

  expect_warning(
    nfl_marker(df, cm),
    class = "healthmarkers_nfl_warn_na_coercion"
  )
})

test_that("NA policies: keep, omit, error, warn", {
  df <- data.frame(NfL = c(12, NA, 35))

  out_keep <- nfl_marker(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$nfl_value[2]))

  out_omit <- nfl_marker(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 2L)
  expect_false(any(is.na(out_omit$nfl_value)))

  expect_error(
    nfl_marker(df, cm, na_action = "error"),
    class = "healthmarkers_nfl_error_missing_values"
  )

  expect_warning(
    nfl_marker(df, cm, na_action = "warn"),
    class = "healthmarkers_nfl_warn_missing_inputs"
  )
})

test_that("domain warnings: negative values", {
  df_neg <- data.frame(NfL = c(-5, 15))

  expect_warning(
    nfl_marker(df_neg, cm),
    class = "healthmarkers_nfl_warn_negative_values"
  )
})

test_that("extreme scan behaviors: warn, cap, NA, error", {
  df <- data.frame(NfL = c(-10, 15, 1e9, 35))

  # helper: suppress only the "negative values" domain warning
  suppress_negative <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        if (inherits(w, "healthmarkers_nfl_warn_negative_values")) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  # warn: expect extremes_detected, but hide the negative-values warning
  expect_warning(
    suppress_negative(
      nfl_marker(df, cm, check_extreme = TRUE, extreme_action = "warn")
    ),
    class = "healthmarkers_nfl_warn_extremes_detected"
  )

  # cap: hide all warnings, check result
  out_cap <- suppressWarnings(
    nfl_marker(df, cm, check_extreme = TRUE, extreme_action = "cap")
  )
  expect_true(all(is.finite(out_cap$nfl_value) | is.na(out_cap$nfl_value)))

  # NA: hide warnings, check NA introduction
  out_na <- suppressWarnings(
    nfl_marker(df, cm, check_extreme = TRUE, extreme_action = "NA")
  )
  expect_true(any(is.na(out_na$nfl_value)))

  # error: hide warnings, assert on error class
  expect_error(
    suppressWarnings(
      nfl_marker(df, cm, check_extreme = TRUE, extreme_action = "error")
    ),
    class = "healthmarkers_nfl_error_extremes"
  )
})

test_that("padding preserved for keep/warn", {
  df <- data.frame(NfL = c(12, NA, 35))

  out_keep <- nfl_marker(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(nfl_marker(df, cm, na_action = "warn"))

  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
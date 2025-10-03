# tests/testthat/test_adiposity_sds_strat.R

library(testthat)
library(tibble)

# Shared reference set
ref_full <- list(
  M = list(
    BMI   = c(mean = 23, sd = 3),
    waist = c(mean = 85, sd = 10)
  ),
  F = list(
    BMI   = c(mean = 21, sd = 3),
    waist = c(mean = 75, sd = 9)
  )
)

test_that("ref must have M and F", {
  df <- tibble(sex = "M", BMI = 25)
  bad_refs <- list(
    NULL,
    list(M = list(BMI = c(mean = 23, sd = 3))),
    list(F = list(BMI = c(mean = 21, sd = 3))),
    list(X = list(BMI = c(mean = 23, sd = 3)), F = list(BMI = c(mean = 21, sd = 3)))
  )
  for (r in bad_refs) {
    expect_error(
      adiposity_sds_strat(df, ref = r),
      "`ref` must be a named list with elements 'M' and 'F'"
    )
  }
})

test_that("ref M/F variable sets must match", {
  df <- tibble(sex = "M", BMI = 25)
  ref_mismatch <- list(
    M = list(BMI = c(mean = 23, sd = 3), waist = c(mean = 80, sd = 10)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, ref = ref_mismatch),
    "ref\\$M and ref\\$F must define identical variables"
  )
})

test_that("ref component must have mean & sd names", {
  df <- tibble(sex = "M", BMI = 25)
  bad_ref <- list(
    M = list(BMI = c(23, 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, ref = bad_ref),
    "ref\\[\\['M'\\]\\]\\[\\['BMI'\\]\\] must be numeric with names mean, sd"
  )
})

test_that("sd must be >0", {
  df <- tibble(sex = "F", BMI = 20)
  ref_bad_sd <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 0))
  )
  expect_error(
    adiposity_sds_strat(df, ref = ref_bad_sd),
    "sd must be >0"
  )
})

test_that("missing sex column errors", {
  df <- tibble(gender = "M", BMI = 25)
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, ref = ref, sex_col = "sex"),
    "sex column 'sex' not found"
  )
})

test_that("invalid sex values error", {
  df <- tibble(sex = c("M", "X"), BMI = c(25, 22))
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, ref = ref),
    "rows have unmapped sex values"
  )
})

test_that("computes correct SDS (multi-variable, vectorized)", {
  df <- tibble(
    sex   = c("M", "F", "M"),
    BMI   = c(25, 20, 26),
    waist = c(95, 70, 80)
  )
  out <- adiposity_sds_strat(df, ref = ref_full)
  expect_equal(out$BMI_SDS[1],  (25 - 23) / 3)
  expect_equal(out$waist_SDS[1], (95 - 85) / 10)
  expect_equal(out$BMI_SDS[2],  (20 - 21) / 3)
  expect_equal(out$waist_SDS[2], (70 - 75) / 9)
  expect_equal(nrow(out), 3)
  expect_named(out, c("BMI_SDS", "waist_SDS"))
})

test_that("custom sex_col supported", {
  ref <- list(
    M = list(BMI = c(mean = 24, sd = 3)),
    F = list(BMI = c(mean = 22, sd = 3))
  )
  df <- tibble(gender = c("M","F"), BMI = c(25, 23))
  out <- adiposity_sds_strat(df, ref = ref, sex_col = "gender")
  expect_equal(out$BMI_SDS, c((25-24)/3, (23-22)/3))
})

test_that("na_action omit vs error", {
  df <- tibble(sex = c("M","F","M"), BMI = c(25, NA, 24), waist = c(90, 70, 88))
  # Suppress high-missingness diagnostics; we only test na_action behavior
  out_keep <- adiposity_sds_strat(df, ref = ref_full, na_action = "keep", diagnostics = FALSE)
  expect_equal(nrow(out_keep), 3)
  out_omit <- adiposity_sds_strat(df, ref = ref_full, na_action = "omit", diagnostics = FALSE)
  expect_equal(nrow(out_omit), 2)
  expect_error(
    adiposity_sds_strat(df, ref = ref_full, na_action = "error", diagnostics = FALSE),
    "missing values present"
  )
})

test_that("allow_partial skips missing vars when TRUE", {
  ref_partial <- list(
    M = list(BMI = c(mean=23, sd=3), waist = c(mean=85, sd=10)),
    F = list(BMI = c(mean=21, sd=3), waist = c(mean=75, sd=9))
  )
  df <- tibble(sex = c("M","F"), BMI = c(24,22))  # waist missing
  out <- adiposity_sds_strat(df, ref = ref_partial, allow_partial = TRUE, diagnostics = FALSE)
  expect_named(out, "BMI_SDS")
  expect_equal(nrow(out), 2)
})

test_that("allow_partial=FALSE errors when data var missing", {
  ref_partial <- ref_full
  df <- tibble(sex = c("M","F"), BMI = c(24,22))
  expect_error(
    adiposity_sds_strat(df, ref = ref_partial, allow_partial = FALSE),
    "variables not found in data"
  )
})

test_that("extreme detection warn vs cap", {
  df <- tibble(
    sex = c("M","F"),
    BMI = c(120, 2),
    waist = c(300, 15)
  )
  ref <- list(
    M = list(BMI = c(mean=23, sd=3), waist = c(mean=85, sd=10)),
    F = list(BMI = c(mean=21, sd=3), waist = c(mean=75, sd=9))
  )

  # Capture warning instead of letting testthat register it globally.
  got <- NULL
  withCallingHandlers(
    adiposity_sds_strat(df, ref = ref, check_extreme = TRUE,
                        extreme_action = "warn", diagnostics = FALSE),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("extremes detected", msg)) {
        got <<- msg
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_true(!is.null(got))
  expect_match(got, "extremes detected")

  # Cap path: capture warning similarly
  cap_msg <- NULL
  out_cap <- withCallingHandlers(
    adiposity_sds_strat(df, ref = ref, check_extreme = TRUE,
                        extreme_action = "cap", diagnostics = FALSE),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("capped .* extreme values", msg)) {
        cap_msg <<- msg
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_true(!is.null(cap_msg))
  expect_match(cap_msg, "capped [0-9]+ extreme values")

  expect_true(all(abs(out_cap$BMI_SDS) < 25))
  expect_true(all(abs(out_cap$waist_SDS) < 25))
})

test_that("prefix applied to output columns", {
  df <- tibble(sex = c("M","F"), BMI = c(24,22))
  ref <- list(
    M = list(BMI = c(mean=23, sd=3)),
    F = list(BMI = c(mean=21, sd=3))
  )
  out <- adiposity_sds_strat(df, ref = ref, prefix = "x_")
  expect_named(out, "x_BMI_SDS")
})

test_that("diagnostics=FALSE suppresses coercion warnings", {
  df <- tibble(sex = c("M","F"), BMI = c("24","22"))
  ref <- list(
    M = list(BMI = c(mean=23, sd=3)),
    F = list(BMI = c(mean=21, sd=3))
  )
  expect_silent(adiposity_sds_strat(df, ref = ref, diagnostics = FALSE))
})

test_that("verbose prints processing summary", {
  df <- tibble(sex = "M", BMI = 25, waist = 95)
  expect_message(
    adiposity_sds_strat(df, ref = ref_full, verbose = TRUE),
    "processing 2 variables"
  )
  expect_message(
    adiposity_sds_strat(df, ref = ref_full, verbose = TRUE),
    "Completed adiposity_sds_strat:"
  )
})

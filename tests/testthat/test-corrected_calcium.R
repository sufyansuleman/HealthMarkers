# File: tests/testthat/test-corrected_calcium.R

library(testthat)
library(tibble)

# Basic setup
df <- tibble(Ca = c(9.0, 8.5), Albumin = c(3.5, 3.0))
cm <- list(Ca = "Ca", Albumin = "Albumin")

test_that("numeric coercion warning on non-numeric inputs", {
  df_char <- tibble(Ca = c("9.0", "8.5"), Albumin = c("3.5", "3.0"))
  expect_warning(corrected_calcium(df_char, cm),
                 class = "healthmarkers_calcium_warn_na_coercion")
})

test_that("NA policies keep / warn / omit / error / ignore", {
  df_na <- tibble(Ca = c(9.0, NA_real_), Albumin = c(3.5, 3.0))
  # keep
  out_keep <- corrected_calcium(df_na, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 2L)
  # warn
  expect_warning(corrected_calcium(df_na, cm, na_action = "warn"),
                 class = "healthmarkers_calcium_warn_missing")
  # omit
  out_omit <- corrected_calcium(df_na, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)
  # error
  expect_error(corrected_calcium(df_na, cm, na_action = "error"),
               class = "healthmarkers_calcium_error_missing")
  # ignore (alias for keep)
  out_ignore <- corrected_calcium(df_na, cm, na_action = "ignore")
  expect_equal(nrow(out_ignore), 2L)
})

test_that("basic Payne formula calculation (conventional units)", {
  # Ca = 9.0, Alb = 3.5 -> corrected = 9.0 + 0.8 * (4.0 - 3.5) = 9.4
  # Ca = 8.5, Alb = 3.0 -> corrected = 8.5 + 0.8 * (4.0 - 3.0) = 9.3
  out <- suppressWarnings(corrected_calcium(df, cm))
  expect_equal(out$corrected_calcium[1], 9.4, tolerance = 1e-6)
  expect_equal(out$corrected_calcium[2], 9.3, tolerance = 1e-6)
})

test_that("auto SI detection converts and re-scales back (unit assumption warning)", {
  # SI units: Ca in mmol/L, Alb in g/L
  df_si <- tibble(Ca = c(2.3, 2.1), Albumin = c(35, 30))
  out <- suppressWarnings(corrected_calcium(df_si, cm))
  # Ca 2.3 mmol/L = 9.2 mg/dL, Alb 35 g/L = 3.5 g/dL
  # corrected = 9.2 + 0.8 * (4.0 - 3.5) = 9.6 mg/dL = 9.6/4 = 2.4 mmol/L
  expect_equal(out$corrected_calcium[1], 2.4, tolerance = 1e-6)
})

test_that("domain warnings for albumin and corrected calcium ranges (no extreme scan)", {
  df_high_ca <- tibble(Ca = c(16.0), Albumin = c(4.0))
  expect_warning(corrected_calcium(df_high_ca, cm, units = "conventional"),
                 class = "healthmarkers_calcium_warn_domain_corrected")
  df_low_alb <- tibble(Ca = c(9.0), Albumin = c(1.5))
  expect_warning(corrected_calcium(df_low_alb, cm, units = "conventional"),
                 class = "healthmarkers_calcium_warn_domain_albumin")
})

test_that("domain warnings suppressed when extreme scan active (only extreme warnings)", {
  df_high <- tibble(Ca = c(16.0), Albumin = c(4.0))
  expect_warning(
    corrected_calcium(df_high, cm, units = "conventional",
                     check_extreme = TRUE, extreme_action = "warn"),
    class = "healthmarkers_calcium_warn_extremes_detected"
  )
})

test_that("extreme scan behaviors: warn / cap / NA / error", {
  df_extreme <- tibble(Ca = c(20.0), Albumin = c(4.0))
  # warn
  expect_warning(
    corrected_calcium(df_extreme, cm, units = "conventional",
                     check_extreme = TRUE, extreme_action = "warn"),
    class = "healthmarkers_calcium_warn_extremes_detected"
  )
  # NA
  out_na <- suppressWarnings(corrected_calcium(df_extreme, cm, units = "conventional",
                                               check_extreme = TRUE, extreme_action = "NA"))
  expect_true(any(is.na(out_na$corrected_calcium)))
  expect_error(
    corrected_calcium(df_extreme, cm, units = "conventional",
                     check_extreme = TRUE, extreme_action = "error"),
    class = "healthmarkers_calcium_error_extremes"
  )
})

test_that("custom extreme_rules override defaults (capping within new bounds)", {
  df_custom <- tibble(Ca = c(11.0), Albumin = c(4.0))
  custom_rules <- list(corrected_calcium = c(8, 10))
  expect_warning(
    corrected_calcium(df_custom, cm, units = "conventional",
                     check_extreme = TRUE, extreme_action = "cap",
                     extreme_rules = custom_rules),
    class = "healthmarkers_calcium_warn_extremes_capped"
  )
})

test_that("verbose emits preparing, computing, and completion messages", {
  expect_message(corrected_calcium(df, cm, verbose = TRUE),
                 "preparing inputs")
  expect_message(corrected_calcium(df, cm, verbose = TRUE),
                 "computing result")
  expect_message(corrected_calcium(df, cm, verbose = TRUE),
                 "Completed corrected_calcium")
})

test_that("empty input returns 0-row tibble with column", {
  df_empty <- tibble(Ca = numeric(0), Albumin = numeric(0))
  out <- corrected_calcium(df_empty, cm)
  expect_equal(nrow(out), 0L)
  expect_true("corrected_calcium" %in% names(out))
})

test_that("all NA inputs produce NA outputs (keep/warn/ignore) and omit drops all", {
  df_all_na <- tibble(Ca = c(NA_real_, NA_real_), Albumin = c(NA_real_, NA_real_))
  # keep
  out_keep <- suppressWarnings(corrected_calcium(df_all_na, cm, na_action = "keep"))
  expect_true(all(is.na(out_keep$corrected_calcium)))
  # ignore
  out_ignore <- suppressWarnings(corrected_calcium(df_all_na, cm, na_action = "ignore"))
  expect_true(all(is.na(out_ignore$corrected_calcium)))
  # omit
  out_omit <- suppressWarnings(corrected_calcium(df_all_na, cm, na_action = "omit"))
  expect_equal(nrow(out_omit), 0L)
})

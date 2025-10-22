# tests/testthat/test_saliva_markers.R

library(testthat)
library(tibble)

test_that("saliva_markers errors if missing required columns", {
  df1 <- tibble(
    saliva_cort2    = 15,
    saliva_cort3    = 12,
    saliva_amylase  = 100,
    saliva_glucose  = 80
  )
  expect_error(
    saliva_markers(df1),
    "missing columns: saliva_cort1"
  )

  df2 <- tibble(
    saliva_cort1    = 10,
    saliva_cort3    = 12,
    saliva_amylase  = 100
    # missing saliva_cort2 and saliva_glucose
  )
  expect_error(
    saliva_markers(df2),
    "missing columns: saliva_cort2, saliva_glucose"
  )
})

test_that("saliva_markers computes log transforms, CAR_AUC, and passes through glucose", {
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  out <- saliva_markers(df)

  expect_named(
    out,
    c("log_cortisol_wake", "CAR_AUC", "log_amylase", "saliva_glucose")
  )
  expect_equal(nrow(out), 1)

  expected_log_cort <- log(10)
  # trapezoidal AUC: (10+20)/2*30 + (20+10)/2*30 = 900
  expected_auc <- (10 + 20) / 2 * 30 + (20 + 10) / 2 * 30
  expected_log_amy <- log(100)
  expected_gluc <- 5.0

  expect_equal(out$log_cortisol_wake, expected_log_cort)
  expect_equal(out$CAR_AUC, expected_auc)
  expect_equal(out$log_amylase, expected_log_amy)
  expect_equal(out$saliva_glucose, expected_gluc)
})

test_that("saliva_markers is vectorized over multiple rows", {
  df <- tibble(
    saliva_cort1    = c(10, 5),
    saliva_cort2    = c(20, 10),
    saliva_cort3    = c(10, 5),
    saliva_amylase  = c(100, 50),
    saliva_glucose  = c(5, 3)
  )
  out <- saliva_markers(df)
  expect_equal(nrow(out), 2)
  expect_equal(out$log_cortisol_wake[2], log(5))
  # AUC second row: (5+10)/2*30 + (10+5)/2*30 = 450
  expect_equal(out$CAR_AUC[2], 450)
})

test_that("verbose = TRUE prints progress messages (HM-CS verbosity)", {
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  old <- getOption("healthmarkers.verbose"); on.exit(options(healthmarkers.verbose = old), add = TRUE)
  options(healthmarkers.verbose = "inform")
  expect_message(saliva_markers(df, verbose = TRUE), "-> saliva_markers: validating inputs")
  expect_message(saliva_markers(df, verbose = TRUE), "-> saliva_markers: computing markers")
})

test_that("na_action='omit' drops rows with NA in required inputs and returns empty tibble when all omitted", {
  df <- tibble(
    saliva_cort1    = NA_real_,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  out <- saliva_markers(df, na_action = "omit")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("log_cortisol_wake","CAR_AUC","log_amylase","saliva_glucose"))
})

test_that("invalid `times` argument errors clearly", {
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  expect_error(
    saliva_markers(df, times = c(0, 60)),
    "times"
  )
})

test_that("extreme_action behaviors and rules keyed by input keys", {
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100000,  # extreme vs default cap 50,000
    saliva_glucose  = 5.0
  )
  # ignore -> no warning
  expect_warning(saliva_markers(df, check_extreme = TRUE, extreme_action = "ignore"), NA)
  # warn -> warning
  expect_warning(saliva_markers(df, check_extreme = TRUE, extreme_action = "warn"), "extreme input values")
  # cap -> warning and capped effect on log_amylase
  out_cap <- suppressWarnings(saliva_markers(df, check_extreme = TRUE, extreme_action = "cap"))
  expect_equal(out_cap$log_amylase, log(50000), tolerance = 1e-12)

  # Rules keyed by input key name should be honored (cap amylase at 10)
  out_keycap <- suppressWarnings(saliva_markers(df, check_extreme = TRUE, extreme_action = "cap",
                                                extreme_rules = list(amylase = c(0, 10))))
  expect_equal(out_keycap$log_amylase, log(10), tolerance = 1e-12)
})

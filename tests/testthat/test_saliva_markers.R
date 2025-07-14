# tests/testthat/test_saliva_markers.R

test_that("saliva_markers errors if missing required columns", {
  df1 <- tibble::tibble(
    saliva_cort2    = 15,
    saliva_cort3    = 12,
    saliva_amylase  = 100,
    saliva_glucose  = 80
  )
  expect_error(
    saliva_markers(df1),
    "missing columns: saliva_cort1"
  )
  
  df2 <- tibble::tibble(
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
  df <- tibble::tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  out <- saliva_markers(df)
  
  # Check column names and length
  expect_named(
    out,
    c("log_cortisol_wake","CAR_AUC","log_amylase","saliva_glucose")
  )
  expect_equal(nrow(out), 1)
  
  # Manual calculations
  expected_log_cort <- log(10)
  # trapezoidal AUC: (10+20)/2*30 + (20+10)/2*30 = 15*30 + 15*30 = 900
  expected_auc      <- (10 + 20)/2 * 30 + (20 + 10)/2 * 30
  expected_log_amy  <- log(100)
  expected_gluc     <- 5.0
  
  expect_equal(out$log_cortisol_wake, expected_log_cort)
  expect_equal(out$CAR_AUC,           expected_auc)
  expect_equal(out$log_amylase,       expected_log_amy)
  expect_equal(out$saliva_glucose,    expected_gluc)
})

test_that("saliva_markers is vectorized over multiple rows", {
  df <- tibble::tibble(
    saliva_cort1    = c(10, 5),
    saliva_cort2    = c(20, 10),
    saliva_cort3    = c(10, 5),
    saliva_amylase  = c(100, 50),
    saliva_glucose  = c(5, 3)
  )
  out <- saliva_markers(df)
  expect_equal(nrow(out), 2)
  # Check that second row matches manual
  expect_equal(out$log_cortisol_wake[2], log(5))
  # AUC second row: (5+10)/2*30 + (10+5)/2*30 = 7.5*30 + 7.5*30 = 450
  expect_equal(out$CAR_AUC[2], 450)
})

test_that("verbose = TRUE prints a progress message", {
  df <- tibble::tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  expect_message(
    saliva_markers(df, verbose = TRUE),
    "-> computing saliva markers"
  )
})

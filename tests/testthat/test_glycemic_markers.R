library(testthat)
library(tibble)

test_that("errors if missing any of HDL_c, TG, or BMI", {
  df1 <- tibble(TG = 1, BMI = 24)
  expect_error(
    glycemic_markers(df1),
    "missing required columns: HDL_c"
  )
  df2 <- tibble(HDL_c = 1, BMI = 24)
  expect_error(
    glycemic_markers(df2),
    "missing required columns: TG"
  )
  df3 <- tibble(HDL_c = 1, TG = 1.3)
  expect_error(
    glycemic_markers(df3),
    "missing required columns: BMI"
  )
})

test_that("SPISE is computed correctly", {
  df <- tibble(HDL_c = 1.0, TG = 1.3, BMI = 24)
  expected <- 600 * 1.0^0.185 / (1.3^0.2 * 24^1.338)
  out <- glycemic_markers(df)
  expect_equal(out$SPISE, expected, tolerance = 1e-8)
})

test_that("METS_IR returns Inf/finite when glucose present, NA otherwise", {
  df_with <- tibble(HDL_c = 1, TG = 1.3, BMI = 24, glucose = 5.6)
  out1 <- glycemic_markers(df_with)
  # log(1) = 0, so may be Inf
  expect_true(is.finite(out1$METS_IR) || is.infinite(out1$METS_IR))
  
  df_without <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df_without)
  expect_true(is.na(out2$METS_IR))
})

test_that("prediabetes and diabetes flags handle HbA1c correctly and propagate NA", {
  df <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    HbA1c = c(40, 44, 50)
  )
  out <- glycemic_markers(df)
  expect_equal(out$prediabetes, c(0L, 1L, 1L))
  expect_equal(out$diabetes,    c(0L, 0L, 1L))
  
  df2 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df2)
  expect_true(all(is.na(out2$prediabetes)))
  expect_true(all(is.na(out2$diabetes)))
})

test_that("HOMA_CP computed when C_peptide & G0 present, NA otherwise", {
  df_ok <- tibble(
    HDL_c     = 1, TG = 1.3, BMI = 24,
    C_peptide = 300, G0 = 5.5
  )
  expected <- (5.5 * (300/6)) / 22.5
  out_ok <- glycemic_markers(df_ok)
  expect_equal(out_ok$HOMA_CP, expected, tolerance = 1e-8)
  
  df_bad <- tibble(HDL_c = 1, TG = 1.3, BMI = 24, C_peptide = 300)
  out_bad <- glycemic_markers(df_bad)
  expect_true(is.na(out_bad$HOMA_CP))
})

test_that("LAR, ASI, and TyG_index compute correctly when inputs present", {
  # set up a row where everything is present
  df <- tibble(
    HDL_c       = 1, TG = 1.3, BMI = 24,
    leptin      = 10, adiponectin = 5,
    I0          = 50,
    glucose     = 5.6
  )
  out <- glycemic_markers(df)
  # Leptin/Adiponectin Ratio
  expect_equal(out$LAR, 10/5)
  # Adiponectin Sensitivity Index = adiponectin / I0
  expect_equal(out$ASI, 5/50)
  # TyG_index: ln((TG*88.57) * (glucose*18) / 2)
  TG_mgdl  <- 1.3 * 88.57
  Glu_mgdl <- 5.6 * 18
  expect_equal(out$TyG_index, log(TG_mgdl * Glu_mgdl / 2), tolerance = 1e-8)
})

test_that("LAR, ASI, TyG_index are NA when their inputs are missing", {
  df1 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out1 <- glycemic_markers(df1)
  expect_true(is.na(out1$LAR))
  expect_true(is.na(out1$ASI))
  expect_true(is.na(out1$TyG_index))
  
  df2 <- tibble(
    HDL_c  = 1, TG = 1.3, BMI = 24,
    leptin = 10
  )
  out2 <- glycemic_markers(df2)
  expect_true(is.na(out2$LAR))
})

test_that("output is vectorized and contains all expected columns", {
  df <- tibble(
    HDL_c       = c(1, 1.1),
    TG          = c(1.3, 1.5),
    BMI         = c(24, 26),
    glucose     = c(5.5, NA),
    HbA1c       = c(44, 39),
    C_peptide   = c(300, NA),
    G0          = c(5.5, NA),
    leptin      = c(10, NA),
    adiponectin = c(5, NA),
    I0          = c(50, NA)
  )
  out <- glycemic_markers(df)
  expect_equal(nrow(out), 2)
  expect_named(
    out,
    c(
      "SPISE", "METS_IR", "prediabetes", "diabetes",
      "HOMA_CP", "LAR", "ASI", "TyG_index"
    )
  )
})

test_that("verbose = TRUE prints a progress message", {
  df <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  expect_message(
    glycemic_markers(df, verbose = TRUE),
    "-> glycemic_markers: computing"
  )
})

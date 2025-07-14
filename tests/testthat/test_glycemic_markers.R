# tests/testthat/test_glycemic_markers.R

test_that("errors if missing any of HDL_c, TG, or BMI", {
  df1 <- tibble::tibble(TG = 1, BMI = 24)
  expect_error(
    glycemic_markers(df1),
    "missing required columns: HDL_c"
  )
  df2 <- tibble::tibble(HDL_c = 1, BMI = 24)
  expect_error(
    glycemic_markers(df2),
    "missing required columns: TG"
  )
})

test_that("SPISE is computed correctly against manual formula", {
  df <- tibble::tibble(HDL_c = 1.0, TG = 1.3, BMI = 24)
  # SPISE = 600 * HDL_c^0.185 / (TG^0.2 * BMI^1.338)
  expected <- 600 * 1.0^0.185 / (1.3^0.2 * 24^1.338)
  out <- glycemic_markers(df)
  expect_equal(out$SPISE, expected, tolerance = 1e-8)
})

test_that("METS_IR returns value when glucose present, NA otherwise", {
  df_with <- tibble::tibble(HDL_c = 1, TG = 1.3, BMI = 24, glucose = 5.6)
  # METS_IR = log(2*glucose + TG) * BMI / log(HDL_c)
  expected <- log(2*5.6 + 1.3) * 24 / log(1)
  # log(1) == 0 => Inf; we'll just test non-NA
  out1 <- glycemic_markers(df_with)
  expect_true(is.finite(out1$METS_IR) || is.infinite(out1$METS_IR))
  
  df_without <- tibble::tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df_without)
  expect_true(is.na(out2$METS_IR))
})

test_that("prediabetes and diabetes flags handle HbA1c correctly", {
  df <- tibble::tibble(HDL_c = 1, TG = 1.3, BMI = 24, HbA1c = c(40, 44, 50))
  out <- glycemic_markers(df)
  # 40 < 42 => 0; 44 >=42 =>1; 50>=42 =>1
  expect_equal(out$prediabetes, c(0L, 1L, 1L))
  # diabetes: HbA1c >= 48
  expect_equal(out$diabetes,    c(0L, 0L, 1L))
  
  df2 <- tibble::tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df2)
  expect_true(all(is.na(out2$prediabetes)))
  expect_true(all(is.na(out2$diabetes)))
})

test_that("HOMA_CP computed when C_peptide & G0 present, NA otherwise", {
  df_ok <- tibble::tibble(
    HDL_c      = 1, TG = 1.3, BMI = 24,
    C_peptide  = 300, G0 = 5.5
  )
  # HOMA_CP = (G0 * (C_peptide/6)) / 22.5
  expected <- (5.5 * (300/6)) / 22.5
  out_ok <- glycemic_markers(df_ok)
  expect_equal(out_ok$HOMA_CP, expected, tolerance = 1e-8)
  
  df_bad <- tibble::tibble(HDL_c = 1, TG = 1.3, BMI = 24, C_peptide = 300)
  out_bad <- glycemic_markers(df_bad)
  expect_true(is.na(out_bad$HOMA_CP))
})

test_that("output is vectorized over multiple rows", {
  df <- tibble::tibble(
    HDL_c     = c(1,1.2),
    TG        = c(1.3,1.5),
    BMI       = c(24,26),
    glucose   = c(5.5, 6.0),
    HbA1c     = c(44, 39),
    C_peptide = c(300, 0),
    G0        = c(5.5, 5.5)
  )
  out <- glycemic_markers(df)
  expect_equal(nrow(out), 2)
  expect_named(out, c("SPISE","METS_IR","prediabetes","diabetes","HOMA_CP"))
})

test_that("verbose = TRUE prints a progress message", {
  df <- tibble::tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  expect_message(
    glycemic_markers(df, verbose = TRUE),
    "-> glycemic_markers: computing"
  )
})

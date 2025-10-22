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

test_that("METS_IR returns finite when glucose present and HDL_c != 1, NA when denominator is zero", {
  # Use HDL_c != 1 so log(HDL_c) != 0 and result is finite
  df_with <- tibble(HDL_c = 1.2, TG = 1.3, BMI = 24, glucose = 5.6)
  out1 <- glycemic_markers(df_with)
  expected <- (log(2 * 5.6 + 1.3) * 24) / log(1.2)
  expect_equal(out1$METS_IR, expected, tolerance = 1e-8)

  # Denominator zero case -> safe division returns NA
  df_den0 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24, glucose = 5.6)
  out_den0 <- glycemic_markers(df_den0)
  expect_true(is.na(out_den0$METS_IR))

  # No glucose column -> NA
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
  expect_equal(out$diabetes, c(0L, 0L, 1L))

  df2 <- tibble(HDL_c = 1, TG = 1.3, BMI = 24)
  out2 <- glycemic_markers(df2)
  expect_true(all(is.na(out2$prediabetes)))
  expect_true(all(is.na(out2$diabetes)))
})

test_that("HOMA_CP computed when C_peptide & G0 present, NA otherwise", {
  df_ok <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    C_peptide = 300, G0 = 5.5
  )
  expected <- (5.5 * (300 / 6)) / 22.5
  out_ok <- glycemic_markers(df_ok)
  expect_equal(out_ok$HOMA_CP, expected, tolerance = 1e-8)

  df_bad <- tibble(HDL_c = 1, TG = 1.3, BMI = 24, C_peptide = 300)
  out_bad <- glycemic_markers(df_bad)
  expect_true(is.na(out_bad$HOMA_CP))
})

test_that("LAR, ASI, and TyG_index compute correctly when inputs present", {
  # set up a row where everything is present
  df <- tibble(
    HDL_c = 1, TG = 1.3, BMI = 24,
    leptin = 10, adiponectin = 5,
    I0 = 50,
    glucose = 5.6
  )
  out <- glycemic_markers(df)
  # Leptin/Adiponectin Ratio
  expect_equal(out$LAR, 10 / 5)
  # Adiponectin Sensitivity Index = adiponectin / I0
  expect_equal(out$ASI, 5 / 50)
  # TyG_index: ln((TG*88.57) * (glucose*18) / 2)
  TG_mgdl <- 1.3 * 88.57
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
    HDL_c = 1, TG = 1.3, BMI = 24,
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

test_that("glycemic_markers computes expected columns with minimal inputs", {
  df <- data.frame(HDL_c = c(1.0, 1.3), TG = c(1.5, 2.0), BMI = c(24, 30))
  res <- glycemic_markers(df)
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("SPISE","METS_IR","prediabetes","diabetes","HOMA_CP","LAR","ASI","TyG_index"))
  # METS_IR, TyG_index depend on glucose; NA by default
  expect_true(all(is.na(res$METS_IR)))
  expect_true(all(is.na(res$TyG_index)))
})

test_that("Optional inputs drive only their outputs; NA handling works", {
  df <- data.frame(
    HDL_c = c(1.2, 1.3),   # avoid ln(HDL_c) == 0
    TG = c(1.5, 2.0),
    BMI = c(24, 30),
    glucose = c(5.6, NA),
    HbA1c = c(44, 38),
    C_peptide = c(300, 500),
    G0 = c(5.5, 6.2),
    I0 = c(60, 120),
    leptin = c(10, 20),
    adiponectin = c(8, 5)
  )
  res_keep <- glycemic_markers(df, na_action = "keep")
  expect_false(all(is.na(res_keep$METS_IR))) # at least first row computable
  expect_false(all(is.na(res_keep$TyG_index)))
  expect_equal(res_keep$prediabetes, c(1L, 0L))
  expect_equal(res_keep$diabetes, c(0L, 0L))
  # error on NA/non-finite when na_action = "error"
  expect_error(glycemic_markers(df, na_action = "error"), "missing or non-finite")
  # omit drops the NA row
  res_omit <- glycemic_markers(df, na_action = "omit")
  expect_equal(nrow(res_omit), 1L)
})

test_that("Extreme handling: cap vs NA", {
  df <- data.frame(
    HDL_c = c(1.2, 1.3),   # avoid ln(HDL_c) == 0
    TG = c(25, 2.0),         # TG out-of-range (default limit up to 20)
    BMI = c(24, 30),
    glucose = c(5.6, 7.1)    # makes METS_IR and TyG computable
  )
  # cap: should not introduce NA in SPISE/METS_IR/TyG_index
  res_cap <- suppressWarnings(glycemic_markers(df, check_extreme = TRUE, extreme_action = "cap"))
  expect_false(any(is.na(res_cap$SPISE)))
  expect_false(any(is.na(res_cap$METS_IR)))
  expect_false(any(is.na(res_cap$TyG_index)))
  # NA: extremes propagate NA to affected outputs in that row
  res_na <- glycemic_markers(df, check_extreme = TRUE, extreme_action = "NA")
  expect_true(is.na(res_na$SPISE[1]))
  expect_true(is.na(res_na$METS_IR[1]))
  expect_true(is.na(res_na$TyG_index[1]))
})

test_that("Coercion to numeric warns when NAs introduced", {
  df <- data.frame(
    HDL_c = c("1.0", "bad"),
    TG = c("1.5", "2.0"),
    BMI = c(24, 30)
  )
  expect_warning(glycemic_markers(df), "coerced to numeric; NAs introduced")
})

test_that("Verbose prints completion summary", {
  df <- data.frame(HDL_c = c(1.0, 1.3), TG = c(1.5, 2.0), BMI = c(24, 30))
  expect_message(glycemic_markers(df, verbose = TRUE), "Completed glycemic_markers:")
})

test_that("glycemic_markers runs and returns key columns", {
  df <- tibble::tibble(
    HDL_c  = c(1.0, 1.3),
    TG     = c(1.5, 2.0),
    BMI    = c(24, 30),
    glucose = c(5.6, 7.1)
  )
  out <- glycemic_markers(df, na_action = "keep", verbose = FALSE)
  expect_true(all(c("SPISE", "METS_IR") %in% names(out)))
  expect_equal(nrow(out), nrow(df))
})

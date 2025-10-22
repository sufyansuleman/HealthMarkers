library(testthat)
library(tibble)

# Minimal required mapping and a helper to build small test data frames
cm_req <- list(
  creatinine = "Cr",
  age        = "Age",
  sex        = "Sex",
  race       = "Race",
  BUN        = "BUN"
)

make_df_req <- function(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14) {
  tibble(Cr = Cr, Age = Age, Sex = Sex, Race = Race, BUN = BUN)
}

test_that("verbose prints validating/computing messages (HM-CS verbosity)", {
  df <- make_df_req()
  old <- getOption("healthmarkers.verbose"); on.exit(options(healthmarkers.verbose = old), add = TRUE)
  options(healthmarkers.verbose = "inform")
  expect_message(renal_markers(df, cm_req, verbose = TRUE), "-> renal_markers: validating inputs")
  expect_message(renal_markers(df, cm_req, verbose = TRUE), "-> renal_markers: computing markers")
})

test_that("error when mapped column not found in data", {
  df <- tibble(Age = 40, Sex = 1, Race = "white", BUN = 14) # missing Cr
  expect_error(
    renal_markers(df, cm_req),
    "not found"
  )
})

test_that("na_action='omit' dropping all rows returns empty tibble with expected columns", {
  df <- make_df_req(Cr = NA_real_)
  out <- suppressWarnings(renal_markers(df, cm_req, na_action = "omit"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true(all(c(
    "eGFR_cr","eGFR_cys","eGFR_combined","BUN_Cr_ratio","FE_Urea",
    "NGAL","KIM1","NAG","Beta2Micro","IL18","L_FABP"
  ) %in% names(out)))
})

test_that("sex mapping: numeric vs string agree; male vs female differ", {
  df_code <- make_df_req(Cr = 1.0, Age = 50, Sex = 1, Race = "white", BUN = 14)
  df_strm <- make_df_req(Cr = 1.0, Age = 50, Sex = "male", Race = "white", BUN = 14)
  out_code <- renal_markers(df_code, cm_req)
  out_strm <- renal_markers(df_strm, cm_req)
  expect_equal(out_code$eGFR_cr, out_strm$eGFR_cr, tolerance = 1e-8)

  df_f_code <- make_df_req(Cr = 1.0, Age = 50, Sex = 0, Race = "white", BUN = 14)
  df_f_str  <- make_df_req(Cr = 1.0, Age = 50, Sex = "female", Race = "white", BUN = 14)
  out_f_code <- renal_markers(df_f_code, cm_req)
  out_f_str  <- renal_markers(df_f_str, cm_req)
  expect_equal(out_f_code$eGFR_cr, out_f_str$eGFR_cr, tolerance = 1e-8)

  expect_false(isTRUE(all.equal(out_code$eGFR_cr, out_f_code$eGFR_cr)))
})

test_that("race mapping: black vs white scales eGFR_cr by ~1.159 (all else equal)", {
  df_w <- make_df_req(Cr = 1.0, Age = 50, Sex = 1, Race = "caucasian", BUN = 14)
  df_b <- make_df_req(Cr = 1.0, Age = 50, Sex = 1, Race = "african american", BUN = 14)
  out_w <- renal_markers(df_w, cm_req)
  out_b <- renal_markers(df_b, cm_req)
  expect_gt(out_b$eGFR_cr, 0)
  expect_gt(out_w$eGFR_cr, 0)
  expect_equal(unname(out_b$eGFR_cr / out_w$eGFR_cr), 1.159, tolerance = 1e-6)
})

test_that("extreme_action='ignore' emits no warning; 'warn' emits warning without altering outputs", {
  df_ext <- make_df_req(Cr = 10, Age = 90, Sex = 1, Race = "white", BUN = 300) # extreme by defaults
  out0 <- renal_markers(df_ext, cm_req)
  expect_warning(
    out_ignore <- renal_markers(df_ext, cm_req, check_extreme = TRUE, extreme_action = "ignore"),
    NA
  )
  expect_equal(out_ignore, out0, tolerance = 1e-12)
  expect_warning(
    out_warn <- renal_markers(df_ext, cm_req, check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values"
  )
  expect_equal(out_warn, out0, tolerance = 1e-12)
})

test_that("FE_Urea zero denominators yield consolidated warning and NA", {
  df1 <- tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14,
                U_s = 0, Cr_u = 2, U_u = 80)
  cm1 <- modifyList(cm_req, list(urea_serum = "U_s", creatinine_urine = "Cr_u", urea_urine = "U_u"))
  expect_warning(
    out1 <- renal_markers(df1, cm1),
    "zero denominators detected"
  )
  expect_true(is.na(out1$FE_Urea))

  df2 <- tibble(Cr = 1.0, Age = 40, Sex = 1, Race = "white", BUN = 14,
                U_s = 40, Cr_u = 0, U_u = 80)
  expect_warning(
    out2 <- renal_markers(df2, cm1),
    "zero denominators detected"
  )
  expect_true(is.na(out2$FE_Urea))
})
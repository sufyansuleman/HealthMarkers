test_that("ckd_stage classifies G stages and albuminuria", {
  df <- data.frame(eGFR = c(95, 70, 50, 35, 10), UACR = c(10, 50, 200, 600, 100))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("CKD_stage", "Albuminuria_stage", "KDIGO_risk"))
  expect_equal(res$CKD_stage, c("G1","G2","G3a","G3b","G5"))
  expect_equal(res$Albuminuria_stage, c("A1","A2","A2","A3","A2"))
})

test_that("ckd_stage handles missing by keeping", {
  df <- data.frame(eGFR = c(NA, 80), UACR = c(10, NA))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_warn_prop = 0)
  expect_equal(nrow(res), 2L)
  expect_true(is.na(res$CKD_stage[1]))
})

test_that("ckd_stage keeps rows by default and omits when asked", {
  df <- data.frame(eGFR = c(NA, 80), UACR = c(10, NA))
  res_keep <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_warn_prop = 0)
  expect_equal(nrow(res_keep), 2L)
  res_omit <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_action = "omit", na_warn_prop = 0)
  expect_equal(nrow(res_omit), 0L)
})
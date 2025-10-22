test_that("ckd_stage classifies G and A stages", {
  df <- data.frame(eGFR = c(95, 70, 50, 35, 10), UACR = c(10, 50, 200, 600, 100))
  res <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("CKD_stage", "Albuminuria_stage", "KDIGO_risk"))
  expect_equal(as.character(res$CKD_stage), c("G1","G2","G3a","G3b","G5"))
  expect_equal(as.character(res$Albuminuria_stage), c("A1","A2","A2","A3","A2"))
})

test_that("ckd_stage default keep retains rows; omit drops any NA in mapped inputs", {
  df <- data.frame(eGFR = c(NA, 80), UACR = c(10, NA))
  res_keep <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"))
  expect_equal(nrow(res_keep), 2L)
  expect_true(is.na(res_keep$CKD_stage[1]))
  res_omit <- ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_action = "omit")
  expect_equal(nrow(res_omit), 0L)
})

test_that("ckd_stage na_action = error aborts on missing mapped inputs", {
  df <- data.frame(eGFR = c(NA, 80), UACR = c(10, NA))
  expect_error(
    ckd_stage(df, col_map = list(eGFR = "eGFR", UACR = "UACR"), na_action = "error"),
    "missing/non-finite values"
  )
})
test_that("oxidative_markers computes GSH/GSSG ratio", {
  df <- data.frame(GSH = c(1000, 1500), GSSG = c(10, 15))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"))
  expect_s3_class(res, "tbl_df")
  expect_true("GSH_GSSG_Ratio" %in% names(res))
  expect_equal(res$GSH_GSSG_Ratio, c(100, 100), tolerance = 1e-10)
})

test_that("oxidative_markers safe division returns NA on zero denom", {
  df <- data.frame(GSH = c(10, 10), GSSG = c(0, 10))
  res <- oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG"))
  expect_true(is.na(res$GSH_GSSG_Ratio[1]))
  expect_equal(res$GSH_GSSG_Ratio[2], 1)
})
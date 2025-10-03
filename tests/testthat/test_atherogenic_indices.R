test_that("atherogenic_indices computes AIP, CRI_I, CRI_II", {
  dat <- data.frame(TC = c(200, 180), HDL_c = c(50, 60), TG = c(150, 100), LDL_c = c(120, 90))
  res <- atherogenic_indices(dat, col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c"))
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("AIP","CRI_I","CRI_II"))
  exp_AIP <- log10(dat$TG / dat$HDL_c)
  exp_CRI_I <- dat$TC / dat$HDL_c
  exp_CRI_II <- dat$LDL_c / dat$HDL_c
  expect_equal(res$AIP, as.numeric(exp_AIP), tolerance = 1e-12)
  expect_equal(res$CRI_I, as.numeric(exp_CRI_I), tolerance = 1e-12)
  expect_equal(res$CRI_II, as.numeric(exp_CRI_II), tolerance = 1e-12)
})

test_that("atherogenic_indices NA handling omit", {
  dat <- data.frame(TC = c(200, NA), HDL_c = c(50, 60), TG = c(150, 100), LDL_c = c(120, 90))
  res <- atherogenic_indices(
    dat,
    col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c"),
    na_action = "omit",
    na_warn_prop = 0
  )
  expect_equal(nrow(res), 1L)
})
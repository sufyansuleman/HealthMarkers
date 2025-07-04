library(tibble)
test_that("lipid_markers computes ratios and non_HDL", {
  df <- tibble(TC=5, HDL_c=1, TG=1.3, LDL_c=3, ApoB=1.1, ApoA1=1.5)
  out <- lipid_markers(df)
  expect_named(out, c("non_HDL_c","remnant_c","ratio_TC_HDL","ratio_TG_HDL","ratio_LDL_HDL","ApoB_ApoA1"))
  expect_equal(out$non_HDL_c, 5-1)
  expect_equal(out$ApoB_ApoA1, 1.1/1.5)
})

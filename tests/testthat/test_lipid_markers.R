
test_that("lipid_markers computes core lipid markers", {
  df <- tibble::tibble(
    TC    = 5,
    HDL_c = 1,
    TG    = 1.3,
    LDL_c = 3,
    ApoB  = 1.1,
    ApoA1 = 1.5
  )
  
  out <- lipid_markers(df, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1"
  ))
  
  expect_named(out, c(
    "non_HDL_c", "remnant_c", "ratio_TC_HDL",
    "ratio_TG_HDL", "ratio_LDL_HDL", "ApoB_ApoA1"
  ))
  
  expect_equal(out$non_HDL_c, 5 - 1)
  expect_equal(out$remnant_c, 5 - (1 + 3))
  expect_equal(out$ratio_TC_HDL, 5 / 1)
  expect_equal(out$ratio_TG_HDL, 1.3 / 1)
  expect_equal(out$ratio_LDL_HDL, 3 / 1)
  expect_equal(out$ApoB_ApoA1, 1.1 / 1.5)
})

test_that("lipid_markers estimates LDL via Friedewald when LDL_c missing", {
  df2 <- tibble::tibble(TC = 5, HDL_c = 1, TG = 1.5)
  
  expect_warning(
    out2 <- lipid_markers(df2, col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG")),
    "estimating LDL_c via Friedewald"
  )
  
  # LDL = 5 - 1 - (1.5/5) = 3.7
  expect_equal(out2$ratio_LDL_HDL, 3.7 / 1)
})

test_that("lipid_markers computes VAI_Men and VAI_Women when waist & BMI provided", {
  df3 <- tibble::tibble(
    TC    = 5, HDL_c = 1, TG = 1.3,
    LDL_c = 3, ApoB = 1.1, ApoA1 = 1.5,
    waist = 85, BMI = 26
  )
  
  out3 <- lipid_markers(df3, col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1",
    waist = "waist",
    BMI   = "BMI"
  ))
  
  expect_true(all(c("VAI_Men", "VAI_Women") %in% names(out3)))
  
  exp_men <- (85 / (39.68 + 1.88 * 26)) * (1.3 / 1.03) * (1.31 / 1)
  exp_wom <- (85 / (36.58 + 1.89 * 26)) * (1.3 / 0.81) * (1.52 / 1)
  
  expect_equal(out3$VAI_Men,   exp_men, tolerance = 1e-8)
  expect_equal(out3$VAI_Women, exp_wom, tolerance = 1e-8)
})

test_that("lipid_markers omits VAI when waist or BMI missing", {
  df4a <- tibble::tibble(TC = 5, HDL_c = 1, TG = 1.3, waist = 85)  # BMI missing
  expect_warning(
    out4a <- lipid_markers(df4a, col_map = list(
      TC    = "TC",
      HDL_c = "HDL_c",
      TG    = "TG",
      waist = "waist"
    )),
    "estimating LDL_c via Friedewald"
  )
  expect_false("VAI_Men" %in% names(out4a))
  expect_false("VAI_Women" %in% names(out4a))
  
  df4b <- tibble::tibble(TC = 5, HDL_c = 1, TG = 1.3, BMI = 26)  # waist missing
  expect_warning(
    out4b <- lipid_markers(df4b, col_map = list(
      TC    = "TC",
      HDL_c = "HDL_c",
      TG    = "TG",
      BMI   = "BMI"
    )),
    "estimating LDL_c via Friedewald"
  )
  expect_false("VAI_Men" %in% names(out4b))
  expect_false("VAI_Women" %in% names(out4b))
})

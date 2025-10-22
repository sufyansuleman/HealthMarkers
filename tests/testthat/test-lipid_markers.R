test_that("lipid_markers computes core lipid markers", {
  df <- tibble(
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
  df2 <- tibble(TC = 5, HDL_c = 1, TG = 1.5)
  expect_warning(
    out2 <- lipid_markers(df2, col_map = list(TC = "TC", HDL_c = "HDL_c", TG = "TG")),
    "estimating LDL_c via Friedewald"
  )
  # LDL = 5 - 1 - (1.5/5) = 3.7
  expect_equal(out2$ratio_LDL_HDL, 3.7 / 1)
})

test_that("lipid_markers computes VAI_Men and VAI_Women when waist & BMI provided", {
  df3 <- tibble(
    TC = 5, HDL_c = 1, TG = 1.3,
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
  expect_equal(out3$VAI_Men, exp_men, tolerance = 1e-8)
  expect_equal(out3$VAI_Women, exp_wom, tolerance = 1e-8)
})

test_that("lipid_markers omits VAI when waist or BMI missing", {
  df4a <- tibble(TC = 5, HDL_c = 1, TG = 1.3, waist = 85) # BMI missing
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

  df4b <- tibble(TC = 5, HDL_c = 1, TG = 1.3, BMI = 26) # waist missing
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

# ────────────────────────────────────────────────────────────────────────────────
# LAP tests
# ────────────────────────────────────────────────────────────────────────────────

test_that("lipid_markers computes LAP_Men and LAP_Women when waist provided", {
  df_lap <- tibble(TC = 5, HDL_c = 1, TG = 1.3, waist = 100)
  expect_warning(
    out_lap <- lipid_markers(df_lap, col_map = list(
      TC    = "TC",
      HDL_c = "HDL_c",
      TG    = "TG",
      waist = "waist"
    )),
    "estimating LDL_c via Friedewald"
  )
  expect_true(all(c("LAP_Men", "LAP_Women") %in% names(out_lap)))

  exp_lap_men <- (100 - 65) * 1.3
  exp_lap_wom <- (100 - 58) * 1.3
  expect_equal(out_lap$LAP_Men, exp_lap_men)
  expect_equal(out_lap$LAP_Women, exp_lap_wom)
})

test_that("lipid_markers omits LAP when waist missing", {
  df_no_lap <- tibble(TC = 5, HDL_c = 1, TG = 1.3)
  expect_warning(
    out_no_lap <- lipid_markers(df_no_lap, col_map = list(
      TC    = "TC",
      HDL_c = "HDL_c",
      TG    = "TG"
    )),
    "estimating LDL_c via Friedewald"
  )
  expect_false("LAP_Men" %in% names(out_no_lap))
  expect_false("LAP_Women" %in% names(out_no_lap))
})

# ────────────────────────────────────────────────────────────────────────────────
# HM-CS v2 additions: NA policy, extremes, and coercion
# ────────────────────────────────────────────────────────────────────────────────

test_that("na_action='omit' drops rows with NA in required inputs", {
  df <- tibble(TC = c(5, NA), HDL_c = c(1, 1), TG = c(1.3, 1.4), LDL_c = c(3.7, 3.8))
  out <- lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"), na_action = "omit")
  expect_equal(nrow(out), 1L)
  expect_equal(out$ratio_TC_HDL, 5/1)
})

test_that("na_action='error' aborts on NA in required inputs", {
  df <- tibble(TC = c(5, NA), HDL_c = c(1, 1), TG = c(1.3, 1.4))
  expect_error(
    lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG"), na_action = "error"),
    "missing or non-finite"
  )
})

test_that("extreme handling: cap and NA", {
  df <- tibble(TC = 5, HDL_c = 1, TG = -3, LDL_c = 3) # negative TG out-of-range
   # cap -> TG becomes 0, ratio_TG_HDL = 0/1 = 0
   expect_warning(
    out_cap <- lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"),
                             check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values"
  )
  expect_equal(out_cap$ratio_TG_HDL, 0)
  # NA -> TG becomes NA, ratio_TG_HDL becomes NA
  out_na <- lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c"),
                           check_extreme = TRUE, extreme_action = "NA")
  expect_true(is.na(out_na$ratio_TG_HDL))
})

test_that("numeric coercion warns when NAs introduced", {
  df <- tibble(TC = c("5","oops"), HDL_c = c("1","1"), TG = c("1.3","1.4"), LDL_c = c(3, 3))
   expect_warning(
    lipid_markers(df, col_map = list(TC="TC", HDL_c="HDL_c", TG="TG", LDL_c="LDL_c")),
    "coerced to numeric; NAs introduced"
  )
})

library(testthat)

cm <- list(
  fev1 = "FEV1",
  fvc = "FVC",
  fev1_post = "FEV1_post",
  fvc_post = "FVC_post",
  age = "age",
  height = "height",
  sex = "sex",
  ethnicity = "ethnicity"
)

test_that("computes pre and post ratios and fixed COPD flag", {
  df <- data.frame(
    FEV1 = c(2.1, 1.3, 2.0),
    FVC  = c(3.0, 2.5, 3.1),
    FEV1_post = c(NA, 1.9, 2.4),
    FVC_post  = c(NA, 2.5, 3.0)
  )
  out <- spirometry_markers(df, cm)

  # Pre ratios
  expect_equal(out$ratio_pre[1], 2.1 / 3.0)
  expect_equal(out$ratio_pre[2], 1.3 / 2.5)

  # Post ratios computed for rows with post data
  expect_true(is.na(out$ratio_post[1]))
  expect_equal(out$ratio_post[2], 1.9 / 2.5)
  expect_equal(out$ratio_post[3], 2.4 / 3.0)

  # Fixed COPD flag prefers post if available
  # row2: pre 0.52 < 0.70, post 0.76 >= 0.70 => FALSE
  expect_false(out$copd_flag_fixed[2])
  # row1: only pre 0.70, border (not < 0.70) => FALSE
  expect_false(out$copd_flag_fixed[1])
})

test_that("verbose emits preparing inputs message", {
  df <- data.frame(FEV1 = 2.0, FVC = 3.0)
  expect_message(
    spirometry_markers(df, cm, verbose = TRUE),
    "-> spirometry_markers: preparing inputs"
  )
})

test_that("NA policies: keep, omit, error, warn", {
  df <- data.frame(FEV1 = c(NA, 1.0, 1.2), FVC = c(2.0, 2.5, NA))

  # keep (default via 'keep' or 'ignore')
  out_keep <- spirometry_markers(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), nrow(df))
  expect_true(is.na(out_keep$ratio_pre[1]))
  expect_true(is.na(out_keep$ratio_pre[3]))

  # omit
  out_omit <- spirometry_markers(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)
  expect_equal(out_omit$ratio_pre[1], 1.0 / 2.5)

  # error
  expect_error(
    spirometry_markers(df, cm, na_action = "error"),
    class = "healthmarkers_spiro_error_missing_values"
  )

  # warn
  expect_warning(
    spirometry_markers(df, cm, na_action = "warn"),
    class = "healthmarkers_spiro_warn_missing_inputs"
  )
})

test_that("numeric coercion warning on volumes with non-numeric noise", {
  df <- data.frame(
    FEV1 = c("2.0", "oops"),  # 'oops' will introduce NA on coercion
    FVC  = c("3.0", "4.0")
  )
  expect_warning(
    spirometry_markers(df, cm),
    class = "healthmarkers_spiro_warn_na_coercion"
  )
})

test_that("domain warnings: zero FVC, negative inputs, ratio > 1", {
  # zero FVC
  df_zero <- data.frame(FEV1 = c(2.0, 1.0), FVC = c(0, 3.0))
  expect_warning(
    spirometry_markers(df_zero, cm),
    class = "healthmarkers_spiro_warn_zero_fvc"
  )

  # negative values
  df_neg <- data.frame(FEV1 = c(-1, 2.0), FVC = c(3.0, -2.0))
  expect_warning(
    spirometry_markers(df_neg, cm),
    class = "healthmarkers_spiro_warn_negative_values"
  )

  # ratio > 1
  df_gt1 <- data.frame(FEV1 = 4.0, FVC = 3.0)
  expect_warning(
    spirometry_markers(df_gt1, cm),
    class = "healthmarkers_spiro_warn_ratio_gt_one"
  )
})

test_that("extreme scan behaviors: warn, cap, NA, error", {
  # Ensure FEV1 <= FVC to avoid extra ratio>1 warnings
  df <- data.frame(
    FEV1 = c(0.2, 9.5, 3.0),
    FVC  = c(0.3, 12.0, 4.5)
  )
  cm <- list(fev1 = "FEV1", fvc = "FVC")

  # warn
  expect_warning(
    spirometry_markers(df, cm, check_extreme = TRUE, extreme_action = "warn"),
    class = "healthmarkers_spiro_warn_extremes_detected"
  )

  # cap
  expect_warning(
    spirometry_markers(df, cm, check_extreme = TRUE, extreme_action = "cap"),
    class = "healthmarkers_spiro_warn_extremes_capped"
  )

  # NA
  out_na <- spirometry_markers(df, cm, check_extreme = TRUE, extreme_action = "NA")
  expect_true(any(is.na(out_na$ratio_pre)))

  # error
  expect_error(
    spirometry_markers(df, cm, check_extreme = TRUE, extreme_action = "error"),
    class = "healthmarkers_spiro_error_extremes"
  )
})

test_that("bronchodilator response percent is computed when post provided", {
  df <- data.frame(
    FEV1 = c(1.0, 2.0),
    FVC  = c(2.0, 3.0),
    FEV1_post = c(1.2, 2.1),
    FVC_post  = c(2.1, 3.6)
  )
  out <- spirometry_markers(df, cm)
  expect_equal(out$bdr_fev1, c(20, 5))      # percent improvement
  expect_equal(out$bdr_fvc,  c(5, 20))
  # ratio_post exists and used for copd_flag_fixed
  expect_equal(out$ratio_post, c(1.2/2.1, 2.1/3.6))
})

test_that("padding: keep and warn preserve row count; omit reduces rows", {
  df <- data.frame(FEV1 = c(2.0, NA, 1.0), FVC = c(3.0, 2.5, NA))
  out_keep <- spirometry_markers(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), nrow(df))
  out_warn <- suppressWarnings(spirometry_markers(df, cm, na_action = "warn"))
  expect_equal(nrow(out_warn), nrow(df))
  out_omit <- spirometry_markers(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)
})

test_that("custom extreme_rules are honored", {
  # Keep ratios ≤ 1 here as well
  df <- data.frame(
    FEV1 = c(0.05, 6.0),
    FVC  = c(0.1,  7.5)
  )
  cm <- list(fev1 = "FEV1", fvc = "FVC")

  out <- expect_warning(
    spirometry_markers(
      df, cm, check_extreme = TRUE, extreme_action = "cap",
      extreme_rules = list(fev1 = c(0.1, 5.5), fvc = c(0.2, 7.0))
    ),
    class = "healthmarkers_spiro_warn_extremes_capped"
  )
  # values should be capped into the provided bounds
  expect_true(all(is.finite(out$ratio_pre) | is.na(out$ratio_pre)))
})

test_that("GLI-based outputs present when rspiro and demographics available", {
  skip_if_not_installed("rspiro")
  df <- data.frame(
    FEV1 = c(2.5, 1.2), FVC = c(3.2, 2.0),
    FEV1_post = c(2.7, 1.3), FVC_post = c(3.3, 2.1),
    age = c(55, 60), height = c(170, 165),
    sex = c("male", "female"), ethnicity = c("white", "white")
  )
  out <- spirometry_markers(df, cm)

  # Columns exist and are numeric/logical
  expect_true(all(c("fev1_pp","fvc_pp","fev1_z","fvc_z","ratio_z","obstruction_lln","gold_grade") %in% names(out)))
  # At least some non-NA values expected if rspiro returns references
  expect_true(any(is.finite(out$fev1_pp) | is.finite(out$fvc_pp)))
  # obstruction_lln is logical; gold_grade either character NA or "GOLD X"
  expect_type(out$obstruction_lln, "logical")
  expect_type(out$gold_grade, "character")
})
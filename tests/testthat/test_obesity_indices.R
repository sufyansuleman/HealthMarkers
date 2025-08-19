# tests/testthat/test_obesity_indices.R

# Base synthetic data for testing
base_df <- tibble(
  wt    = c(80, 90), # kg
  ht    = c(2, 2), # m
  waist = c(1, 2), # m
  hip   = c(1, 1), # m
  sex   = c(0, 1) # 0 = male, 1 = female
)

# 1) Core metrics without options
test_that("core metrics compute correctly without options", {
  out <- obesity_indices(
    data = base_df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    weight_unit = "kg",
    height_unit = "m",
    adjust_WHR = FALSE,
    include_RFM = FALSE
  )

  # weight_kg and height_m
  expect_equal(out$weight_kg, c(80, 90))
  expect_equal(out$height_m, c(2, 2))

  # BMI and category
  expect_equal(out$BMI, c(20, 22.5))
  expect_equal(out$BMI_cat, rep("Normal weight", 2))

  # WHR and waist_to_height_ratio
  expect_equal(out$WHR, c(1, 2))
  expect_equal(out$waist_to_height_ratio, c(0.5, 1))

  # waist_to_BMI_ratio and weight_to_height_ratio
  expect_equal(out$waist_to_BMI_ratio, c(1 / 20, 2 / 22.5))
  expect_equal(out$weight_to_height_ratio, c(40, 45))

  # Advanced indices produce finite numerics
  expect_true(all(is.finite(out$AVI)))
  expect_true(all(is.finite(out$BAI)))
  expect_true(all(is.finite(out$ABSI)))
  expect_true(all(is.finite(out$BRI)))
  expect_true(all(is.finite(out$CI)))
})

# 2) Test adjust_WHR and include_RFM
test_that("adjust_WHR adds WHRadjBMI and include_RFM adds RFM", {
  out2 <- obesity_indices(
    data = base_df,
    weight = wt,
    height = ht,
    waist = waist,
    hip = hip,
    sex = sex,
    weight_unit = "kg",
    height_unit = "m",
    adjust_WHR = TRUE,
    include_RFM = TRUE
  )
  # WHRadjBMI exists and numeric
  expect_true("WHRadjBMI" %in% names(out2))
  expect_type(out2$WHRadjBMI, "double")
  # Residuals should sum to zero
  expect_equal(sum(out2$WHRadjBMI), 0)

  # RFM computation
  expected_rfm <- c(
    64 - 20 * (2 / 1) + 12 * 0,
    64 - 20 * (2 / 2) + 12 * 1
  )
  expect_equal(out2$RFM, expected_rfm)
})

# 3) Error handling
test_that("errors on missing columns or missing sex when RFM requested", {
  # Missing waist column
  df_missing <- select(base_df, -waist)
  expect_error(
    obesity_indices(df_missing, wt, ht, waist, hip),
    "missing required columns"
  )

  # include_RFM = TRUE without sex argument
  expect_error(
    obesity_indices(
      data = base_df,
      weight = wt,
      height = ht,
      waist = waist,
      hip = hip,
      include_RFM = TRUE
    ),
    "must be provided to compute RFM"
  )
})

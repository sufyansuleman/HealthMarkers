# tests/testthat/test_inflammatory_age.R

# Fixture data
df <- tibble(
  CRP  = c(1.0, 2.0, NA),
  IL6  = c(5.0, NA, 3.0),
  TNFa = c(1.5, 2.5, 3.5)
)
col_map <- list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa")

# 1) Default weights: should compute equal-weight sum
test_that("iAge computes correct weighted sum with default weights", {
  out <- iAge(df, col_map = col_map)
  # Default weights: c(0.33,0.33,0.34)
  expected <- rowSums(cbind(
    0.33 * df$CRP,
    0.33 * df$IL6,
    0.34 * df$TNFa
  ), na.rm = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "iAge")
  expect_equal(out$iAge, expected)
})

# 2) Custom weights
test_that("iAge respects custom weights and sums to correct values", {
  w <- c(CRP = 0.5, IL6 = 0.3, TNFa = 0.2)
  out2 <- iAge(df, col_map = col_map, weights = w)
  expected2 <- rowSums(cbind(
    0.5 * df$CRP,
    0.3 * df$IL6,
    0.2 * df$TNFa
  ), na.rm = TRUE)
  expect_equal(out2$iAge, expected2)
})

# 3) NA handling: if all markers NA for a row, sum gives 0
test_that("iAge NA handling gives zero for all-NA row", {
  df_na <- tibble(CRP = NA_real_, IL6 = NA_real_, TNFa = NA_real_)
  out3 <- iAge(df_na, col_map = col_map)
  expect_equal(out3$iAge, 0)
})

# 4) Verbose message
test_that("iAge verbose prints message", {
  expect_message(iAge(df, col_map = col_map, verbose = TRUE),
                 "-> iAge: computing weighted sum of inflammatory markers")
})

# 5) Errors on invalid inputs
test_that("iAge errors on non-data.frame input", {
  expect_error(iAge("not_df", col_map), "must be a data.frame or tibble")
})

test_that("iAge errors on missing col_map entries", {
  bad_map <- list(CRP = "CRP", IL6 = "IL6")
  expect_error(iAge(df, col_map = bad_map), "missing col_map entries for: TNFa")
})

test_that("iAge errors on non-numeric weights", {
  expect_error(iAge(df, col_map = col_map, weights = c(CRP = "a", IL6 = 0.5, TNFa = 0.5)),
               "weights must be a named numeric vector")
})

test_that("iAge errors on weights not summing to 1", {
  w2 <- c(CRP = 0.2, IL6 = 0.2, TNFa = 0.2)
  expect_error(iAge(df, col_map = col_map, weights = w2),
               "weights must be a named numeric vector.*summing to 1")
})

# 6) Errors on missing columns in data
test_that("iAge errors if data is missing required columns", {
  df_bad <- tibble(CRP = 1, IL6 = 2)
  expect_error(iAge(df_bad, col_map = col_map), "Column 'TNFa' not found in data")
})

# 7) Errors if mapped column is non-numeric
test_that("iAge errors if mapped column is not numeric", {
  df_char <- tibble(CRP = c("x"), IL6 = 2, TNFa = 3)
  expect_error(iAge(df_char, col_map = col_map), "must be numeric")
})

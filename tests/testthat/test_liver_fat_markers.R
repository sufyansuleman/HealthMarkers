test_that("liver_fat_markers computes HSI and NAFLD_LFS", {
  df <- data.frame(
    ALT = c(30, 40), AST = c(20, 35), BMI = c(25, 32),
    sex = c(1, 2), diabetes = c(0, 1), MetS = c(1, 0), insulin = c(15, 20)
  )
  res <- liver_fat_markers(df, col_map = list(
    ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex",
    diabetes = "diabetes", MetS = "MetS", insulin = "insulin"
  ))
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("HSI", "NAFLD_LFS"))
  exp_hsi <- 8 * (df$ALT / df$AST) + df$BMI + 2 * as.numeric(df$sex == 2) + 2 * as.numeric(df$diabetes == 1)
  exp_lfs <- -2.89 + 1.18 * df$MetS + 0.45 * df$diabetes + 0.15 * df$insulin + 0.04 * df$AST - 0.94 * (df$AST / df$ALT)
  expect_equal(res$HSI, as.numeric(exp_hsi), tolerance = 1e-10)
  expect_equal(res$NAFLD_LFS, as.numeric(exp_lfs), tolerance = 1e-10)
})

test_that("liver_fat_markers missing handling omit", {
  df <- data.frame(
    ALT = c(30, NA), AST = c(20, 35), BMI = c(25, 32),
    sex = c(1, 2), diabetes = c(0, 1), MetS = c(1, 0), insulin = c(15, 20)
  )
  res <- liver_fat_markers(
    df,
    col_map = list(
      ALT = "ALT", AST = "AST", BMI = "BMI", sex = "sex",
      diabetes = "diabetes", MetS = "MetS", insulin = "insulin"
    ),
    na_action = "omit",
    na_warn_prop = 0
  )
  expect_equal(nrow(res), 1L)
})
library(tibble)

test_that("fasting_is computes HOMA correctly and names all columns", {
  df <- tibble(G0 = 5.5, I0 = 60)
  out <- fasting_is(df,
                    col_map  = list(G0="G0", I0="I0"),
                    normalize = "none",
                    verbose   = FALSE)
  
  # Expect exactly 10 indices:
  expect_equal(ncol(out), 10)
  expect_true("HOMA_IR_inv" %in% names(out))
  
  # HOMA_IR_inv = -((G0*18)*(I0/6))/22.5
  expected_homa <- -((5.5 * 18) * (60/6)) / 22.5
  expect_equal(out$HOMA_IR_inv, expected_homa)
})

test_that("fasting_is errors if missing columns", {
  # drop I0
  df <- tibble(G0 = 5.5)
  expect_error(
    fasting_is(df, col_map = list(G0="G0",I0="I0")),
    "missing required columns"
  )
})

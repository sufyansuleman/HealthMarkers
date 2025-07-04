library(tibble)
test_that("urine_markers computes UACR, eGFR, FENa, UPCR", {
  df <- tibble(
    urine_albumin=30, urine_creatinine=1, serum_creatinine=1,
    plasma_Na=140, urine_Na=100, plasma_creatinine=1, sex=1, age=40
  )
  out <- urine_markers(df)
  expect_named(out, c("UACR","microalbuminuria","eGFR_CKD_EPI","FENa","UPCR"))
  expect_equal(out$UACR, 30/1*1000)
})

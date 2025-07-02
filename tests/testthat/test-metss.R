library(tibble)
test_that("metss returns a single numeric column", {
  df <- tibble(
    waist=94, bp_sys=120, bp_dia=80, TG=1.5, HDL_c=1.1,
    glucose=5.3, sex=1, race="NHW"
  )
  out <- metss(df)
  expect_named(out, "MetSSS")
  expect_type(out$MetSSS, "double")
})

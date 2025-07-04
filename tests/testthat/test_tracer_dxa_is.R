library(tibble)
test_that("tracer_dxa_is returns three indices", {
  df <- tibble(
    I0=60, rate_palmitate=1.5, rate_glycerol=2.0,
    fat_mass=20, weight=70, HDL_c=1.0, bmi=24
  )
  out <- tracer_dxa_is(df,
                       col_map = list(
                         I0="I0", rate_palmitate="rate_palmitate",
                         rate_glycerol="rate_glycerol", fat_mass="fat_mass",
                         weight="weight", HDL_c="HDL_c", bmi="bmi"
                       ),
                       normalize="none", verbose=FALSE)
  expect_named(out, c("LIRI_inv","Lipo_inv","ATIRI_inv"))
})

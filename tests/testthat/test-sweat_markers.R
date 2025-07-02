library(tibble)
test_that("sweat_markers computes ionic and rate metrics", {
  df <- tibble(
    sweat_chloride=30, sweat_Na=50, sweat_K=5, sweat_lactate=10,
    weight_before=70, weight_after=69.5, duration=1, body_surface_area=1.8
  )
  out <- sweat_markers(df)
  expect_named(out, c("sweat_chloride","Na_K_ratio","sweat_lactate","sweat_rate"))
  expect_equal(out$Na_K_ratio, 50/5)
})

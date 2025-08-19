# tests/testthat/test_nutrient_markers.R

test_that("nutrient_markers computes all indices correctly", {
  df <- tibble(
    ferritin         = 100,
    transferrin_sat  = 50,
    albumin          = 40,
    total_protein    = 70,
    EPA              = 1,
    DHA              = 2,
    Mg               = 1.0,
    creatinine       = 50,
    glycated_albumin = 4,
    uric_acid        = 400,
    BUN              = 10,
    phosphate        = 1.2,
    calcium          = 2.2,
    Na               = 140,
    K                = 4,
    Cl               = 100,
    HCO3             = 24,
    Tyr              = 60,
    Phe              = 30
  )

  out <- nutrient_markers(df)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c(
    "FerritinTS", "AGR", "Omega3Index", "Mg_Cr_Ratio",
    "GlycatedAlbuminPct", "UA_Cr_Ratio", "BUN_Cr_Ratio",
    "Ca_x_Phosphate", "AnionGap", "Tyr_Phe_Ratio"
  ))

  expect_equal(out$FerritinTS, 100 / 50)
  expect_equal(out$AGR, 40 / (70 - 40), tolerance = 1e-8)
  expect_equal(out$Omega3Index, 1 + 2)
  expect_equal(out$Mg_Cr_Ratio, 1.0 / 50, tolerance = 1e-8)
  expect_equal(out$GlycatedAlbuminPct, (4 / 40) * 100, tolerance = 1e-8)
  expect_equal(out$UA_Cr_Ratio, 400 / 50, tolerance = 1e-8)
  expect_equal(out$BUN_Cr_Ratio, 10 / 50, tolerance = 1e-8)
  expect_equal(out$Ca_x_Phosphate, 2.2 * 1.2, tolerance = 1e-8)
  expect_equal(out$AnionGap, (140 + 4) - (100 + 24))
  expect_equal(out$Tyr_Phe_Ratio, 60 / 30, tolerance = 1e-8)
})

test_that("nutrient_markers is vectorized over rows", {
  df <- tibble(
    ferritin         = c(80, 120),
    transferrin_sat  = c(40, 60),
    albumin          = c(42, 38),
    total_protein    = c(74, 72)
  )
  out <- nutrient_markers(df)
  expect_equal(nrow(out), 2)
  # FerritinTS
  expect_equal(out$FerritinTS, c(80 / 40, 120 / 60))
  # AGR
  expect_equal(out$AGR, c(42 / (74 - 42), 38 / (72 - 38)), tolerance = 1e-8)
  # Others should be NA since inputs missing
  expect_true(all(is.na(out$Omega3Index)))
})

test_that("missing inputs yield NA rather than error", {
  df <- tibble(
    ferritin        = 90,
    transferrin_sat = 45,
    albumin         = 42
  )
  out <- nutrient_markers(df)
  expect_equal(out$FerritinTS, 90 / 45)
  expect_true(is.na(out$AGR)) # total_protein missing
  expect_true(is.na(out$Omega3Index))
  expect_true(is.na(out$Mg_Cr_Ratio))
})

test_that("verbose = TRUE prints a progress message", {
  df <- tibble(
    ferritin        = 50,
    transferrin_sat = 25
  )
  expect_message(
    nutrient_markers(df, verbose = TRUE),
    "-> computing nutrient markers"
  )
})

test_that("errors if `data` is not a data.frame", {
  expect_error(
    nutrient_markers("not a df"),
    "`data` must be a data.frame"
  )
})

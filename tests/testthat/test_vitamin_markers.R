test_that("vitamin_markers computes each index correctly", {
  df <- tibble(
    VitD = 50, VitD_ref_mean = 40, VitD_ref_sd = 5,
    B12 = 300, Folate = 15,
    Ferritin = 100, TSat = 0.25,
    Cortisol = 200, DHEAS = 100,
    Testosterone = 12, Estradiol = 120,
    TSH = 2, free_T4 = 14,
    Retinol = 0.8, Retinol_ref_mean = 0.9, Retinol_ref_sd = 0.2,
    Tocopherol = 30, Total_lipids = 3,
    PIVKA_II = 5,
    VitC = 60,
    Homocysteine = 10,
    MMA = 0.3,
    Magnesium = 0.8, Zinc = 15, Copper = 15
  )
  cm <- as.list(names(df))
  names(cm) <- names(df)
  out <- vitamin_markers(df, col_map = cm)

  expect_equal(out$VitD_Z, (50 - 40) / 5)
  expect_equal(out$B12_Fol_Ratio, 300 / 15)
  expect_equal(out$Ferr_TSat_R, 100 / 0.25)
  expect_equal(out$Cort_DHEA_R, 200 / 100)
  expect_equal(out$T_E2_Ratio, 12 / 120)
  expect_equal(out$TSH_fT4_R, 2 / 14)
  expect_equal(out$Retinol_Z, (0.8 - 0.9) / 0.2)
  expect_equal(out$Toco_Lip_R, 30 / 3)
  expect_equal(out$PIVKA_II, 5)
  expect_equal(out$VitC, 60)
  expect_equal(out$Homocysteine, 10)
  expect_equal(out$MMA, 0.3)
  expect_equal(out$Mg_Zn_R, 0.8 / 15)
  expect_equal(out$Cu_Zn_R, 15 / 15)
})

test_that("errors if any col_map key is missing", {
  df <- tibble(VitD = 10, VitD_ref_mean = 8, VitD_ref_sd = 2)
  cm <- list(VitD = "VitD")
  expect_error(
    vitamin_markers(df, col_map = cm),
    "missing col_map entries for"
  )
})

test_that("verbose = TRUE prints a message", {
  df <- tibble(
    VitD = 10, VitD_ref_mean = 8, VitD_ref_sd = 2,
    B12 = 1, Folate = 1,
    Ferritin = 1, TSat = 1,
    Cortisol = 1, DHEAS = 1,
    Testosterone = 1, Estradiol = 1,
    TSH = 1, free_T4 = 1,
    Retinol = 1, Retinol_ref_mean = 1, Retinol_ref_sd = 1,
    Tocopherol = 1, Total_lipids = 1,
    PIVKA_II = 1,
    VitC = 1,
    Homocysteine = 1,
    MMA = 1,
    Magnesium = 1, Zinc = 1, Copper = 1
  )
  cm <- as.list(names(df))
  names(cm) <- names(df)
  expect_message(
    vitamin_markers(df, col_map = cm, verbose = TRUE),
    "computing vitamin & nutrient indices"
  )
})

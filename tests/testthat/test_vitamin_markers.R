library(testthat)
library(tibble)

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
  cm <- as.list(names(df)); names(cm) <- names(df)

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

test_that("verbose = TRUE prints a progress message", {
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
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_message(
    vitamin_markers(df, col_map = cm, verbose = TRUE),
    "-> vitamin_markers: computing markers"
  )
})

test_that("na_action policies: error and omit behave as expected", {
  df <- tibble(
    VitD = c(50, 60), VitD_ref_mean = c(40, 40), VitD_ref_sd = c(5, 5),
    B12 = c(300, NA_real_), Folate = c(15, 15),
    Ferritin = c(100, 100), TSat = c(0.25, 0.25),
    Cortisol = c(200, 200), DHEAS = c(100, 100),
    Testosterone = c(12, 12), Estradiol = c(120, 120),
    TSH = c(2, 2), free_T4 = c(14, 14),
    Retinol = c(0.8, 0.8), Retinol_ref_mean = c(0.9, 0.9), Retinol_ref_sd = c(0.2, 0.2),
    Tocopherol = c(30, 30), Total_lipids = c(3, 3),
    PIVKA_II = c(5, 5),
    VitC = c(60, 60),
    Homocysteine = c(10, 10),
    MMA = c(0.3, 0.3),
    Magnesium = c(0.8, 0.8), Zinc = c(15, 15), Copper = c(15, 15)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_error(
    suppressWarnings(vitamin_markers(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(vitamin_markers(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out_omit), 1L)
})

test_that("extreme input detection and capping warn as expected", {
  df <- tibble(
    VitD = 1000, VitD_ref_mean = 500, VitD_ref_sd = 0.01,
    B12 = 5000, Folate = 0.1,
    Ferritin = 5000, TSat = 2,
    Cortisol = 5000, DHEAS = 500000,
    Testosterone = 500, Estradiol = 50000,
    TSH = 200, free_T4 = 200,
    Retinol = 50, Retinol_ref_mean = 50, Retinol_ref_sd = 0.001,
    Tocopherol = 500, Total_lipids = 100,
    PIVKA_II = 100000,
    VitC = 1000,
    Homocysteine = 500,
    MMA = 500,
    Magnesium = 10, Zinc = 1000, Copper = 1000
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out_warn <- vitamin_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "warn"),
    "detected .* extreme input values \\(not altered\\)"
  )
  expect_warning(
    out_cap <- vitamin_markers(df, col_map = cm, check_extreme = TRUE, extreme_action = "cap"),
    "capped .* extreme input values into allowed ranges"
  )
  expect_false(isTRUE(all.equal(out_warn$VitD_Z, out_cap$VitD_Z)))
  expect_false(isTRUE(all.equal(out_warn$Toco_Lip_R, out_cap$Toco_Lip_R)))
})

test_that("zero denominators emit a consolidated warning and yield NA in ratios", {
  df <- tibble(
    VitD = 50, VitD_ref_mean = 40, VitD_ref_sd = 0,  # zero sd -> NA VitD_Z
    B12 = 300, Folate = 0,                           # zero -> NA B12/Folate
    Ferritin = 100, TSat = 0,                        # zero -> NA Ferr/TSat
    Cortisol = 200, DHEAS = 0,                       # zero -> NA Cort/DHEAS
    Testosterone = 12, Estradiol = 0,                # zero -> NA T/E2
    TSH = 2, free_T4 = 0,                            # zero -> NA TSH/fT4
    Retinol = 0.8, Retinol_ref_mean = 0.9, Retinol_ref_sd = 0, # zero -> NA Retinol_Z
    Tocopherol = 30, Total_lipids = 0,               # zero -> NA Toco/Lip
    PIVKA_II = 5,
    VitC = 60,
    Homocysteine = 10,
    MMA = 0.3,
    Magnesium = 0.8, Zinc = 0, Copper = 15           # zero Zn -> NA Mg/Zn and Cu/Zn
  )
  cm <- as.list(names(df)); names(cm) <- names(df)

  expect_warning(
    out <- vitamin_markers(df, col_map = cm),
    "zero denominators detected"
  )
  expect_true(is.na(out$VitD_Z))
  expect_true(is.na(out$B12_Fol_Ratio))
  expect_true(is.na(out$Ferr_TSat_R))
  expect_true(is.na(out$Cort_DHEA_R))
  expect_true(is.na(out$T_E2_Ratio))
  expect_true(is.na(out$TSH_fT4_R))
  expect_true(is.na(out$Retinol_Z))
  expect_true(is.na(out$Toco_Lip_R))
  expect_true(is.na(out$Mg_Zn_R))
  expect_true(is.na(out$Cu_Zn_R))
  # passthrough analytes remain
  expect_equal(out$PIVKA_II, 5)
  expect_equal(out$VitC, 60)
  expect_equal(out$Homocysteine, 10)
  expect_equal(out$MMA, 0.3)
})

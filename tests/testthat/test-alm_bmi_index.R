library(testthat)

cm <- list(alm = "ALM_kg", bmi = "BMI", sex = "Sex")

test_that("mapping validation and missing columns error", {
  df <- data.frame(ALM_kg = 18, BMI = 25, Sex = "Male")
  expect_error(alm_bmi_index(df, list()), class = "healthmarkers_alm_bmi_error_missing_map")
  expect_error(alm_bmi_index(df, list(alm = "", bmi = "BMI", sex = "Sex")),
               class = "healthmarkers_alm_bmi_error_bad_map_values")
  expect_error(alm_bmi_index(data.frame(A=1,B=2,C="M"), cm),
               class = "healthmarkers_alm_bmi_error_missing_columns")
})

test_that("verbose emits progress messages", {
  df <- data.frame(ALM_kg = 18, BMI = 25, Sex = "Male")
  expect_message(alm_bmi_index(df, cm, verbose = TRUE), "-> alm_bmi_index: preparing inputs")
  expect_message(alm_bmi_index(df, cm, verbose = TRUE), "-> alm_bmi_index: computing")
})

test_that("numeric coercion warning when strings introduce NAs", {
  df <- data.frame(ALM_kg = c("18", "oops"), BMI = c("25", "30"), Sex = c("Male","Female"))
  expect_warning(alm_bmi_index(df, cm), class = "healthmarkers_alm_bmi_warn_na_coercion")
})

test_that("NA policies: keep, omit, error, warn", {
  df <- data.frame(
    ALM_kg = c(18, NA, 14),
    BMI    = c(25, 22, NA),
    Sex    = c("Male", "Female", "Female")
  )
  out_keep <- alm_bmi_index(df, cm, na_action = "keep")
  expect_equal(nrow(out_keep), 3L)
  expect_true(is.na(out_keep$alm_bmi_ratio[2]) || is.na(out_keep$alm_bmi_ratio[3]))
  out_omit <- alm_bmi_index(df, cm, na_action = "omit")
  expect_equal(nrow(out_omit), 1L)
  expect_error(alm_bmi_index(df, cm, na_action = "error"),
               class = "healthmarkers_alm_bmi_error_missing_values")
  expect_warning(alm_bmi_index(df, cm, na_action = "warn"),
                 class = "healthmarkers_alm_bmi_warn_missing_inputs")
})

test_that("sex normalization and unknown sexes", {
  df <- data.frame(ALM_kg = c(18, 10, 15), BMI = c(25, 22, 20), Sex = c("M","F","X"))
  out <- expect_warning(alm_bmi_index(df, cm), class = "healthmarkers_alm_bmi_warn_sex_unknown")
  # Row1 male: 18/25=0.72 < 0.789 -> TRUE
  expect_true(out$low_muscle_mass[1])
  # Row2 female: 10/22=0.4545 < 0.512 -> TRUE
  expect_true(out$low_muscle_mass[2])
  # Row3 unknown sex -> NA
  expect_true(is.na(out$low_muscle_mass[3]))
})

test_that("domain warnings: BMI and ALM ranges; nonpositive BMI handled", {
  df <- data.frame(ALM_kg = c(3, 45, 18), BMI = c(0, 70, 25), Sex = c("Male","Female","Male"))
  expect_warning(alm_bmi_index(df, cm), class = "healthmarkers_alm_bmi_warn_bmi_range")
  expect_warning(alm_bmi_index(df, cm), class = "healthmarkers_alm_bmi_warn_alm_range")
  out <- expect_warning(alm_bmi_index(df, cm), class = "healthmarkers_alm_bmi_warn_bmi_nonpositive")
  expect_true(is.na(out$alm_bmi_ratio[1]))
})

test_that("extreme scan behaviors: warn, cap, NA, error", {
  df <- data.frame(ALM_kg = c(2, 18, 45), BMI = c(8, 25, 70), Sex = c("Male","Female","Male"))
  # warn
  expect_warning(
    alm_bmi_index(df, cm, check_extreme = TRUE, extreme_action = "warn"),
    class = "healthmarkers_alm_bmi_warn_extremes_detected"
  )
  # cap
  out_cap <- alm_bmi_index(df, cm, check_extreme = TRUE, extreme_action = "cap")
  expect_true(all(out_cap$alm_bmi_ratio >= 5/60 | is.na(out_cap$alm_bmi_ratio))) # weak sanity check
  # NA
  out_na <- alm_bmi_index(df, cm, check_extreme = TRUE, extreme_action = "NA")
  expect_true(any(is.na(out_na$alm_bmi_ratio)))
  # error
  expect_error(
    alm_bmi_index(df, cm, check_extreme = TRUE, extreme_action = "error"),
    class = "healthmarkers_alm_bmi_error_extremes"
  )
})

test_that("ratio calculation and threshold boundaries are correct", {
  # Exact threshold cases: male 0.789, female 0.512
  df <- data.frame(
    ALM_kg = c(19.725, 10.24),
    BMI    = c(25.0,    20.0),
    Sex    = c("Male",  "Female")
  )
  out <- alm_bmi_index(df, cm)
  expect_equal(out$alm_bmi_ratio[1], 19.725/25, tolerance = 1e-12)
  expect_equal(out$alm_bmi_ratio[2], 10.24/20, tolerance = 1e-12)
  # At equality, should NOT be flagged (cut-point is strictly less-than)
  expect_false(out$low_muscle_mass[1])
  expect_false(out$low_muscle_mass[2])

  # Below thresholds -> flagged
  df2 <- data.frame(ALM_kg = c(18, 9.5), BMI = c(25, 20), Sex = c("Male","Female"))
  out2 <- alm_bmi_index(df2, cm)
  expect_true(out2$low_muscle_mass[1])  # 0.72 < 0.789
  expect_true(out2$low_muscle_mass[2])  # 0.475 < 0.512
})

test_that("padding preserved for keep/warn", {
  df <- data.frame(ALM_kg = c(18, NA, 14), BMI = c(25, 22, 22), Sex = c("Male","Female","Female"))
  out_keep <- alm_bmi_index(df, cm, na_action = "keep")
  out_warn <- suppressWarnings(alm_bmi_index(df, cm, na_action = "warn"))
  expect_equal(nrow(out_keep), 3L)
  expect_equal(nrow(out_warn), 3L)
})
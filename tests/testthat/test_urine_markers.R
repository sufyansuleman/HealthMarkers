# tests/testthat/test_urine_markers.R

test_that("errors if any required column is missing", {
  # Missing urine_albumin
  df1 <- tibble::tibble(
    urine_creatinine  = 1,
    serum_creatinine  = 1,
    plasma_Na         = 140,
    urine_Na          = 100,
    age               = 40,
    sex               = 1
  )
  expect_error(
    urine_markers(df1),
    "missing columns: urine_albumin"
  )
  
  # Missing multiple
  df2 <- tibble::tibble(
    urine_albumin     = 30,
    urine_creatinine  = 2,
    plasma_Na         = 140,
    age               = 40
    # missing serum_creatinine, urine_Na, sex
  )
  expect_error(
    urine_markers(df2),
    "missing columns: serum_creatinine, urine_Na, sex"
  )
})

test_that("computes UACR correctly and includes all five outputs", {
  df <- tibble::tibble(
    urine_albumin     = 30,
    urine_creatinine  = 2,
    serum_creatinine  = 1,
    plasma_Na         = 140,
    urine_Na          = 70,
    age               = 40,
    sex               = 1
  )
  out <- urine_markers(df)
  
  expect_named(
    out,
    c("UACR", "microalbuminuria", "eGFR_CKD_EPI", "FENa", "UPCR")
  )
  # UACR = (30/2)*1000 = 15000
  expect_equal(out$UACR, 15000)
})

test_that("microalbuminuria flag is a factor with correct levels & values", {
  df <- tibble::tibble(
    urine_albumin     = c(20, 50, 400),
    urine_creatinine  = c(1, 1000, 1),
    serum_creatinine  = 1,
    plasma_Na         = 140,
    urine_Na          = 70,
    age               = 40,
    sex               = 1
  )
  out <- urine_markers(df)
  expect_true(is.factor(out$microalbuminuria))
  expect_equal(levels(out$microalbuminuria), c("normal","micro"))
  # Values: 20<30 → normal; 50→micro; 400>300→normal
  expect_equal(as.character(out$microalbuminuria), c("normal","micro","normal"))
})

test_that("eGFR_CKD_EPI matches the race‐free CKD‐EPI 2021 formula", {
  df <- tibble::tibble(
    urine_albumin     = 30, urine_creatinine = 1,
    serum_creatinine  = 1, plasma_Na        = 140,
    urine_Na          = 70, age             = c(50,50),
    sex               = c(1,2)
  )
  out <- urine_markers(df)
  
  kappa    <- ifelse(df$sex == 1, 0.9, 0.7)
  alpha    <- ifelse(df$sex == 1, -0.302, -0.241)
  sex_mult <- ifelse(df$sex == 1, 1.0, 1.012)
  Scr_k    <- df$serum_creatinine / kappa
  expected <- 142 *
    pmin(Scr_k, 1)^alpha *
    pmax(Scr_k, 1)^(-1.200) *
    (0.9938^df$age) *
    sex_mult
  
  expect_equal(out$eGFR_CKD_EPI, expected, tolerance = 1e-8)
})

test_that("FENa and UPCR compute correctly, and UPCR is NA without urine_protein", {
  df_with <- tibble::tibble(
    urine_albumin     = 30, urine_creatinine = 2,
    serum_creatinine  = 1, plasma_Na        = 140,
    urine_Na          = 70, age             = 40,
    sex               = 1, urine_protein    = 150
  )
  out_with <- urine_markers(df_with)
  # FENa = (urine_Na * serum_creatinine)/(plasma_Na * urine_creatinine)*100
  expect_equal(out_with$FENa, (70 * 1)/(140 * 2) * 100)
  # UPCR = urine_protein / (urine_creatinine * 0.01) = 150/(2*0.01)=150/0.02=7500
  expect_equal(out_with$UPCR, 150/(2 * 0.01))
  
  df_without <- tibble::tibble(
    urine_albumin     = 30, urine_creatinine = 2,
    serum_creatinine  = 1, plasma_Na        = 140,
    urine_Na          = 70, age             = 40,
    sex               = 1
  )
  out_without <- urine_markers(df_without)
  expect_true(is.na(out_without$UPCR))
})

test_that("is vectorized over multiple rows", {
  df <- tibble::tibble(
    urine_albumin     = c(30, 40),
    urine_creatinine  = c(2, 4),
    serum_creatinine  = c(1, 0.8),
    plasma_Na         = c(140, 138),
    urine_Na          = c(70, 60),
    age               = c(40, 60),
    sex               = c(1,2),
    urine_protein     = c(150, NA_real_)
  )
  out <- urine_markers(df)
  expect_equal(nrow(out), 2)
  # second row UPCR NA
  expect_true(is.na(out$UPCR[2]))
})

test_that("verbose = TRUE prints a progress message", {
  df <- tibble::tibble(
    urine_albumin     = 30,
    urine_creatinine  = 2,
    serum_creatinine  = 1,
    plasma_Na         = 140,
    urine_Na          = 70,
    age               = 40,
    sex               = 1
  )
  expect_message(
    urine_markers(df, verbose = TRUE),
    "-> computing urine markers"
  )
})

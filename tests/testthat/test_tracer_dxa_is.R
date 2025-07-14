# tests/testthat/test_tracer_dxa_is.R

test_that("adipose-only mode returns only LIRI_inv (NA), Lipo_inv, ATIRI_inv", {
  df <- tibble::tibble(
    I0              = 60,
    rate_palmitate  = 1.5,
    rate_glycerol   = 2.0,
    fat_mass        = 20,
    weight          = 70,
    HDL_c           = 1.2,
    bmi             = 24
  )
  col_map_adipose <- list(
    I0             = "I0",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  )
  out <- tracer_dxa_is(df, col_map_adipose, verbose = FALSE)
  expect_named(out, c("LIRI_inv", "Lipo_inv", "ATIRI_inv"))
  expect_true(all(is.na(out$LIRI_inv)))
  # Lipo_inv = - rate_glycerol * (I0/6) = -2 * 10 = -20
  expect_equal(out$Lipo_inv, -1 * (2.0 * (60/6)))
  # ATIRI_inv = - rate_palmitate * (I0/6) = -1.5 * 10 = -15
  expect_equal(out$ATIRI_inv, -1 * (1.5 * (60/6)))
})

test_that("full mode errors when required columns are missing", {
  df <- tibble::tibble(G0 = 5, I0 = 60)
  full_map <- list(
    G0             = "G0",  G30 = "G30",  G120 = "G120",
    I0             = "I0",  I30 = "I30",  I120 = "I120",
    TG             = "TG",  HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    bmi            = "bmi"
  )
  expect_error(
    tracer_dxa_is(df, full_map),
    "missing required columns"
  )
})

test_that("full mode returns all seven indices and correct values", {
  df_full <- tibble::tibble(
    G0             = 5,
    G30            = 7,
    G120           = 7,
    I0             = 60,
    I30            = 65,
    I120           = 70,
    TG             = 1.5,
    HDL_c          = 1.2,
    FFA            = 0.5,
    rate_palmitate = 1.5,
    rate_glycerol  = 2.0,
    fat_mass       = 20,
    weight         = 70,
    bmi            = 24
  )
  full_map <- list(
    G0             = "G0",  G30 = "G30",  G120 = "G120",
    I0             = "I0",  I30 = "I30",  I120 = "I120",
    TG             = "TG",  HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    bmi            = "bmi"
  )
  out <- tracer_dxa_is(df_full, full_map, verbose = FALSE)
  
  expected_names <- c(
    "I_AUC", "FFA_AUC",
    "tracer_palmitate_SI", "tracer_glycerol_SI",
    "LIRI_inv", "Lipo_inv", "ATIRI_inv"
  )
  expect_named(out, expected_names)
  
  # check a few values
  I0_u  <- 60/6
  I30_u <- 65/6
  I120_u<- 70/6
  
  # I_AUC = 0.5 * ((I0_u + I30_u)*30 + (I30_u + I120_u)*90)
  I_AUC_expected <- 0.5 * ((I0_u + I30_u)*30 + (I30_u + I120_u)*90)
  expect_equal(out$I_AUC, I_AUC_expected, tolerance = 1e-8)
  
  # FFA_AUC = 0.5 * (FFA + FFA) * 120
  expect_equal(out$FFA_AUC, 0.5 * (0.5 + 0.5) * 120)
  
  # tracer SI
  expect_equal(out$tracer_palmitate_SI, 1.5/20)
  expect_equal(out$tracer_glycerol_SI,  2.0/20)
  
  # Lipo_inv and ATIRI_inv
  expect_equal(out$Lipo_inv,  -1 * (2.0 * I0_u))
  expect_equal(out$ATIRI_inv, -1 * (1.5 * I0_u))
})

test_that("vectorized over multiple rows", {
    base_df <- tibble::tibble(
        G0             = 5, G30 = 7, G120 = 7,
        I0             = 60, I30 = 65, I120 = 70,
        TG             = 1.5, HDL_c = 1.2, FFA = 0.5,
        rate_palmitate = 1.5, rate_glycerol = 2.0,
        fat_mass       = 20, weight = 70, bmi = 24
      )
    df2 <- dplyr::bind_rows(
        base_df,
        dplyr::mutate(base_df,
                             I0             = 120,
                             rate_glycerol  = 4,
                             rate_palmitate = 3
                           )
      )
  full_map <- list(
    G0             = "G0",  G30 = "G30",  G120 = "G120",
    I0             = "I0",  I30 = "I30",  I120 = "I120",
    TG             = "TG",  HDL_c = "HDL_c", FFA = "FFA",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    bmi            = "bmi"
  )
  out2 <- tracer_dxa_is(df2, full_map, verbose = FALSE)
  expect_equal(nrow(out2), 2)
})

test_that("verbose messages print appropriately", {
  df_full <- tibble::tibble(
    G0             = 5, G30 = 7, G120 = 7,
    I0             = 60, I30 = 65, I120 = 70,
    TG             = 1.5, HDL_c = 1.2, FFA = 0.5,
    rate_palmitate = 1.5, rate_glycerol = 2.0,
    fat_mass       = 20, weight = 70, bmi = 24
  )
  # adipose-only
  col_map_adipose <- list(
    I0             = "I0",
    rate_palmitate = "rate_palmitate",
    rate_glycerol  = "rate_glycerol",
    fat_mass       = "fat_mass",
    weight         = "weight",
    HDL_c          = "HDL_c",
    bmi            = "bmi"
  )
  expect_message(
    tracer_dxa_is(df_full, col_map_adipose),
    "adipose-only indices"
  )
  # full mode
  full_map <- c(col_map_adipose,
                G0 = "G0", G30 = "G30", G120 = "G120",
                I30 = "I30", I120 = "I120",
                TG  = "TG", FFA = "FFA"
  )
  expect_message(
    tracer_dxa_is(df_full, full_map),
    "computing indices"
  )
})

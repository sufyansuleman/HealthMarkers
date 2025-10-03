# tests/testthat/test_adipo_is.R

library(testthat)
library(tibble)
library(dplyr)

run_adipo <- function(df, normalize = "none", verbose = FALSE, ...) {
  adipo_is(
    df,
    col_map = list(
      G0    = "G0",
      I0    = "I0",
      TG    = "TG",
      HDL_c = "HDL_c",
      FFA   = "FFA",
      waist = "waist",
      bmi   = "bmi"
    ),
    normalize = normalize,
    verbose = verbose,
    ...
  )
}

base_df <- tibble(
  G0    = 5.5,
  I0    = 60,
  TG    = 1.2,
  HDL_c = 1.0,
  FFA   = 0.45,
  waist = 80,
  bmi   = 24
)

test_that("errors when col_map is missing required keys", {
  expect_error(
    adipo_is(base_df, col_map = list(G0 = "G0")),
    "missing col_map entries for"
  )
})

test_that("returns all 10 adipose-based indices", {
  out <- run_adipo(base_df)
  expected <- c(
    "Revised_QUICKI","VAI_Men_inv","VAI_Women_inv",
    "TG_HDL_C_inv","TyG_inv","LAP_Men_inv",
    "LAP_Women_inv","McAuley_index","Adipo_inv",
    "Belfiore_inv_FFA"
  )
  expect_named(out, expected)
  expect_equal(ncol(out), length(expected))
  expect_equal(nrow(out), 1L)
})

test_that("vectorized input produces matching row count", {
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out2 <- run_adipo(df2)
  expect_equal(nrow(out2), 2)
})

test_that("Revised_QUICKI matches manual computation", {
  I0_u <- base_df$I0 / 6
  G0_mgdL <- base_df$G0 * 18
  FFA_v <- base_df$FFA
  manual <- 1 / (log10(I0_u) + log10(G0_mgdL) + log10(FFA_v))
  out <- run_adipo(base_df)
  expect_equal(out$Revised_QUICKI, manual, tolerance = 1e-8)
})

test_that("normalize='range' maps variable columns to [0,1]; constants allowed", {
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out_r <- suppressWarnings(run_adipo(df2, normalize = "range"))
  for (col in names(out_r)) {
    vals <- out_r[[col]]
    finite <- vals[is.finite(vals)]
    if (length(unique(finite)) > 1) {
      expect_gte(min(finite), 0)
      expect_lte(max(finite), 1)
    } else {
      expect_true(length(unique(finite)) == 1)
    }
  }
})

test_that("normalize='z' gives mean≈0 / sd≈1 when variable; constants -> zeros", {
  df3 <- bind_rows(
    base_df,
    mutate(base_df, G0 = 6),
    mutate(base_df, G0 = 8)
  )
  out_z <- suppressWarnings(run_adipo(df3, normalize = "z"))
  for (col in names(out_z)) {
    vals <- out_z[[col]]
    finite <- vals[is.finite(vals)]
    if (length(unique(finite)) > 1) {
      expect_equal(mean(finite), 0, tolerance = 1e-6)
      expect_equal(sd(finite), 1, tolerance = 1e-6)
    } else {
      expect_true(all(finite == 0))
    }
  }
})

test_that("normalize='inverse' and 'robust' execute", {
  df2 <- bind_rows(base_df, mutate(base_df, I0 = 30))
  # Silence normalization warnings (e.g., MAD zero)
  expect_error(suppressWarnings(run_adipo(df2, normalize = "inverse")), NA)
  expect_error(suppressWarnings(run_adipo(df2, normalize = "robust")),  NA)
})

test_that("invalid normalize argument errors", {
  expect_error(
    run_adipo(base_df, normalize = "foo"),
    "`normalize` must be one of"
  )
})

test_that("na_action policies", {
  df_na <- bind_rows(base_df, mutate(base_df, TG = NA_real_))
  expect_error(
    suppressWarnings(adipo_is(df_na,
                              col_map = list(G0="G0",I0="I0",TG="TG",HDL_c="HDL_c",FFA="FFA",waist="waist",bmi="bmi"),
                              na_action = "error")),
    "required inputs contain missing values"
  )
  out_omit <- suppressWarnings(adipo_is(df_na,
                                        col_map = list(G0="G0",I0="I0",TG="TG",HDL_c="HDL_c",FFA="FFA",waist="waist",bmi="bmi"),
                                        na_action = "omit"))
  expect_equal(nrow(out_omit), 1L)
})

test_that("extreme detection warn vs cap", {
  df_ext <- mutate(base_df,
                   G0 = 40,
                   I0 = 5000,
                   TG = 30,
                   HDL_c = 0.1,
                   FFA = 5,
                   waist = 400,
                   bmi = 100)
  expect_warning(
    out_w <- run_adipo(df_ext, check_extreme = TRUE, extreme_action = "warn", diagnostics = FALSE),
    "detected .* extreme input values \\(not altered\\)"
  )
  expect_warning(
    out_c <- run_adipo(df_ext, check_extreme = TRUE, extreme_action = "cap", diagnostics = FALSE),
    "capped .* extreme input values into allowed ranges"
  )
  expect_false(isTRUE(all.equal(out_w$Revised_QUICKI, out_c$Revised_QUICKI)))
})

test_that("zero denominators consolidated warning", {
  df_zero <- mutate(base_df, HDL_c = 0, TG = 0)
  expect_warning(
    out_zero <- run_adipo(df_zero, diagnostics = FALSE),
    "zero denominators detected"
  )
  expect_true(any(is.na(out_zero$TG_HDL_C_inv)))
})

test_that("verbose outputs messages", {
  expect_message(run_adipo(base_df, verbose = TRUE), "-> adipo_is: computing indices")
  expect_message(run_adipo(base_df, verbose = TRUE), "Completed adipo_is:")
})

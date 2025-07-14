# tests/testthat/test_adipo_is.R



# Helper to call adipo_is with the standard col_map
run_adipo <- function(df, normalize = "none", verbose = FALSE) {
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
    verbose   = verbose
  )
}

# Base single‐row input
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
    "you must supply col_map entries"
  )
})

test_that("returns all 10 adipose‐based indices", {
  out <- run_adipo(base_df)
  expected <- c(
    "Revised_QUICKI", "VAI_Men_inv",    "VAI_Women_inv",
    "TG_HDL_C_inv",   "TyG_inv",        "LAP_Men_inv",
    "LAP_Women_inv",  "McAuley_index",  "Adipo_inv",
    "Belfiore_inv_FFA"
  )
  expect_setequal(names(out), expected)
  expect_equal(ncol(out), length(expected))
})

test_that("vectorized input: two rows gives two outputs", {
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out2 <- run_adipo(df2)
  expect_equal(nrow(out2), 2)
})

test_that("Revised_QUICKI matches manual computation", {
  # internal conversions: I0_u <- I0/6, G0_mgdL <- G0*18, FFA stays
  I0_u   <- base_df$I0 / 6
  G0_mgdL <- base_df$G0 * 18
  FFA_v  <- base_df$FFA
  manual <- 1 / (log10(I0_u) + log10(G0_mgdL) + log10(FFA_v))
  out    <- run_adipo(base_df)
  expect_equal(out$Revised_QUICKI, manual, tolerance = 1e-8)
})

test_that("normalize = 'range' scales variable indices to [0,1], constants to NA", {
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out_r <- run_adipo(df2, normalize = "range")
  for (col in names(out_r)) {
    vals <- out_r[[col]]
    v2   <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_equal(min(v2), 0)
      expect_equal(max(v2), 1)
    } else {
      expect_true(all(is.na(vals)))
    }
  }
})

test_that("normalize = 'z' yields mean ≈ 0 and sd ≈ 1 on variable indices", {
  df3 <- bind_rows(
    base_df,
    mutate(base_df, G0 = 6),
    mutate(base_df, G0 = 8)
  )
  out_z <- run_adipo(df3, normalize = "z")
  for (col in names(out_z)) {
    vals <- out_z[[col]]
    v2   <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_equal(mean(v2), 0, tolerance = 1e-6)
      expect_equal(sd(v2),   1, tolerance = 1e-6)
    } else {
      expect_true(all(is.na(vals)))
    }
  }
})

test_that("normalize = 'inverse' and 'robust' run without error", {
  df2 <- bind_rows(base_df, mutate(base_df, I0 = 30))
  expect_silent(run_adipo(df2, normalize = "inverse"))
  expect_silent(run_adipo(df2, normalize = "robust"))
})

test_that("invalid normalize argument errors", {
  expect_error(
    run_adipo(base_df, normalize = "foo"),
    "'arg' should be one of"
  )
})

test_that("verbose = TRUE prints a progress message", {
    expect_message(
        run_adipo(base_df, verbose = TRUE),
        "-> adipo_is: computing adipose"
      )
})

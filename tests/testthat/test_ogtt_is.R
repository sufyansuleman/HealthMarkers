

# helper to run ogtt_is more concisely
run_ogtt <- function(df, normalize = "none", verbose = FALSE) {
  ogtt_is(
    df,
    col_map = list(
      G0     = "G0",     I0      = "I0",
      G30    = "G30",    I30     = "I30",
      G120   = "G120",   I120    = "I120",
      weight = "weight", bmi     = "bmi",
      age    = "age",    sex     = "sex"
    ),
    normalize = normalize,
    verbose   = verbose
  )
}

# base single‐row dataset
base_df <- tibble(
  G0     = 5.5,
  I0     = 60,
  G30    = 7.8,
  I30    = 90,
  G120   = 6.2,
  I120   = 50,
  weight = 70,
  bmi    = 24,
  age    = 30,
  sex    = 1
)

test_that("returns all expected indices for single row", {
  out <- run_ogtt(base_df, normalize = "none")
  expected_cols <- c(
    "Isi_120", "Cederholm_index", "Gutt_index",
    "Avignon_Si0", "Avignon_Si120", "Avignon_Sim",
    "Modified_stumvoll", "Stumvoll_Demographics",
    "Matsuda_AUC", "Matsuda_ISI",
    "BigttSi", "Ifc_inv", "HIRI_inv", "Belfiore_isi_gly"
  )
  expect_true(all(expected_cols %in% names(out)))
  expect_true(all(is.finite(as.numeric(out[1, ]))))
})

test_that("vectorized input: two rows gives two outputs", {
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 6))
  out2 <- run_ogtt(df2)
  expect_equal(nrow(out2), 2)
})

test_that("normalize = 'range' scales variable indices to [0,1], constants to NA", {
  df2 <- bind_rows(base_df, mutate(base_df, G0 = 8))
  out_r <- run_ogtt(df2, normalize = "range")
  for (col in names(out_r)) {
    vals <- out_r[[col]]
    # drop NA to check only actual values
    v2 <- vals[!is.na(vals)]
    if (length(unique(v2)) > 1) {
      expect_equal(min(v2), 0)
      expect_equal(max(v2), 1)
    } else {
      # constant column => all NAs after range-normalization
      expect_true(all(is.na(vals)))
    }
  }
})

test_that("normalize = 'z' gives mean ≈ 0 & sd ≈ 1 on variable indices, NA otherwise", {
  df3 <- bind_rows(
    base_df,
    mutate(base_df, G0 = 8),
    mutate(base_df, G0 = 10)
  )
  out_z <- run_ogtt(df3, normalize = "z")
  for (col in names(out_z)) {
    vals <- out_z[[col]]
    v2 <- vals[!is.na(vals)]
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
  expect_silent(run_ogtt(df2, normalize = "inverse"))
  expect_silent(run_ogtt(df2, normalize = "robust"))
})

test_that("invalid normalize argument errors via normalize_vec", {
  expect_error(
    run_ogtt(base_df, normalize = "foo"),
    "'arg' should be one of"
  )
})

test_that("verbose = TRUE prints a progress message", {
  msg <- capture.output(run_ogtt(base_df, verbose = TRUE))
  expect_true(any(grepl("computing OGTT indices", msg)))
})

test_that("error if col_map missing required keys", {
  bad_map <- list(G0="G0", I0="I0")  # too few entries
  expect_error(
    ogtt_is(base_df, col_map = bad_map),
    "you must supply col_map entries"
  )
})

test_that("BigttSi accounts for sex = 2 differently", {
  out_m <- run_ogtt(mutate(base_df, sex = 1))
  out_f <- run_ogtt(mutate(base_df, sex = 2))
  expect_false(isTRUE(all.equal(out_m$BigttSi, out_f$BigttSi)))
})

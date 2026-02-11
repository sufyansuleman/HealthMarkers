# tests/testthat/test-psych_scores.R

library(testthat)
library(tibble)
library(dplyr)

sample_psych_df <- tibble(
  phq9_01 = 1, phq9_02 = 2, phq9_03 = 1, phq9_04 = 0, phq9_05 = 1, phq9_06 = 2, phq9_07 = 1, phq9_08 = 2, phq9_09 = 1,
  gad7_01 = 0, gad7_02 = 1, gad7_03 = 1, gad7_04 = 1, gad7_05 = 0, gad7_06 = 1, gad7_07 = 0,
  k6_01 = 2, k6_02 = 1, k6_03 = 2, k6_04 = 1, k6_05 = 2, k6_06 = 1,
  k10_01 = 1, k10_02 = 1, k10_03 = 1, k10_04 = 1, k10_05 = 1, k10_06 = 1, k10_07 = 1, k10_08 = 1, k10_09 = 1, k10_10 = 1,
  ghq12_01 = 0, ghq12_02 = 1, ghq12_03 = 2, ghq12_04 = 1, ghq12_05 = 0, ghq12_06 = 2,
  ghq12_07 = 1, ghq12_08 = 0, ghq12_09 = 1, ghq12_10 = 0, ghq12_11 = 1, ghq12_12 = 0,
  who5_01 = 3, who5_02 = 3, who5_03 = 4, who5_04 = 3, who5_05 = 4,
  isi_01 = 2, isi_02 = 1, isi_03 = 2, isi_04 = 1, isi_05 = 0, isi_06 = 1, isi_07 = 0,
  mdq_01 = 1, mdq_02 = 1, mdq_03 = 1, mdq_04 = 0, mdq_05 = 1, mdq_06 = 1, mdq_07 = 1, mdq_08 = 0, mdq_09 = 1, mdq_10 = 1, mdq_11 = 0, mdq_12 = 1, mdq_13 = 1,
  mdq_cluster = 1, mdq_impair = 1,
  asrs_01 = 4, asrs_02 = 3, asrs_03 = 3, asrs_04 = 2, asrs_05 = 2, asrs_06 = 3,
  asrs_07 = 1, asrs_08 = 1, asrs_09 = 1, asrs_10 = 1, asrs_11 = 1, asrs_12 = 1,
  asrs_13 = 1, asrs_14 = 1, asrs_15 = 1, asrs_16 = 1, asrs_17 = 1, asrs_18 = 1,
  bis_01 = 2, bis_02 = 3, bis_03 = 2, bis_04 = 3, bis_05 = 1, bis_06 = 2,
  bis_07 = 2, bis_08 = 2, bis_09 = 2, bis_10 = 2, bis_11 = 2, bis_12 = 2,
  task_rt = 300, task_mem = 0.8, task_att = 0.4,
  dx_mdd = 1, dx_anxiety = 0, dx_adhd = 1, dx_bipolar = 0, dx_scz = 0, dx_sud = 0,
  med_ssri = 1, med_snri = 0, med_antipsychotic = 0, med_mood_stabilizer = 0, med_anxiolytic = 0
)

bis_key_small <- list(
  name = "BIS_demo",
  items = sprintf("bis_%02d", 1:12),
  min_val = 1, max_val = 4,
  reverse = c("bis_02","bis_03"),
  subscales = list(
    attention = c("bis_01","bis_05"),
    motor = c("bis_02","bis_06")
  )
)

test_that("phq9_score computes totals and severity", {
  out <- phq9_score(sample_psych_df, col_map = list(items = setNames(names(sample_psych_df)[1:9], sprintf("phq9_%02d", 1:9))))
  expect_equal(out$PHQ9_total, sum(c(1,2,1,0,1,2,1,2,1)))
  expect_s3_class(out$PHQ9_severity, "factor")
})

test_that("gad7_score severity bands work", {
  out <- gad7_score(sample_psych_df, col_map = list(items = setNames(sprintf("gad7_%02d", 1:7), sprintf("gad7_%02d", 1:7))))
  expect_true(out$GAD7_total >= 0)
  expect_true(out$GAD7_severity %in% c("minimal","mild","moderate","severe"))
})

test_that("who5_score percent and low wellbeing flag", {
  out <- who5_score(sample_psych_df, col_map = list(items = setNames(sprintf("who5_%02d", 1:5), sprintf("who5_%02d", 1:5))))
  expect_equal(out$WHO5_raw, sum(c(3,3,4,3,4)))
  expect_equal(out$WHO5_percent, out$WHO5_raw * 4)
  expect_false(out$WHO5_low_wellbeing)
})

test_that("asrs_score part A thresholding", {
  out <- asrs_score(sample_psych_df, col_map = list(items = setNames(sprintf("asrs_%02d", 1:18), sprintf("asrs_%02d", 1:18))))
  expect_equal(out$ASRS_partA_count, 6)
  expect_true(out$ASRS_partA_positive)
})

test_that("bis_score key-driven scoring works", {
  out <- bis_score(sample_psych_df, col_map = list(items = setNames(sprintf("bis_%02d", 1:12), sprintf("bis_%02d", 1:12))), key = bis_key_small)
  expect_equal(out$BIS_total, sum(c(2, (5-3), (5-2),3,1, 2,2,2,2,2,2,2)))
  expect_true(!is.na(out$BIS_attention))
  expect_true(!is.na(out$BIS_motor))
})

test_that("cognitive_score produces z_mean", {
  cm <- list(tasks = list(rt = "task_rt", memory = "task_mem", attention = "task_att"))
  out <- cognitive_score(sample_psych_df, col_map = cm, method = "z_mean")
  expect_true("cog_z_mean" %in% names(out))
})

test_that("diagnosis and medication flags aggregate", {
  dx_map <- list(dx = list(mdd = "dx_mdd", anxiety = "dx_anxiety", adhd = "dx_adhd", bipolar = "dx_bipolar", scz = "dx_scz", sud = "dx_sud"))
  med_map <- list(med = list(ssri = "med_ssri", snri = "med_snri", antipsychotic = "med_antipsychotic", mood_stabilizer = "med_mood_stabilizer", anxiolytic = "med_anxiolytic"))

  dx_out <- psych_dx_flags(sample_psych_df, col_map = dx_map)
  med_out <- psych_med_flags(sample_psych_df, col_map = med_map)

  expect_true(dx_out$dx_any_psych)
  expect_equal(dx_out$dx_count, 2)
  expect_true(med_out$med_any_psych)
  expect_equal(med_out$med_count, 1)
})

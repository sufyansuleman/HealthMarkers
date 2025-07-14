# Thin wrappers for external cardiovascular risk functions
# ---------------------------------------------
# Each function simply re-exports the underlying package function
# with proper roxygen documentation so that `devtools::document()`
# generates correct Rd files.

#' @title   Thin wrapper for PooledCohort::predict_30yr_stroke_risk
#' @description
#' Calls `PooledCohort::predict_30yr_stroke_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_30yr_stroke_risk <- function(...){
  PooledCohort::predict_30yr_stroke_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_30yr_hf_risk
#' @description
#' Calls `PooledCohort::predict_30yr_hf_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_30yr_hf_risk <- function(...){
  PooledCohort::predict_30yr_hf_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_30yr_chd_risk
#' @description
#' Calls `PooledCohort::predict_30yr_chd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_30yr_chd_risk <- function(...){
  PooledCohort::predict_30yr_chd_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_30yr_ascvd_risk
#' @description
#' Calls `PooledCohort::predict_30yr_ascvd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_30yr_ascvd_risk <- function(...){
  PooledCohort::predict_30yr_ascvd_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_30yr_cvd_risk
#' @description
#' Calls `PooledCohort::predict_30yr_cvd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_30yr_cvd_risk <- function(...){
  PooledCohort::predict_30yr_cvd_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_10yr_stroke_risk
#' @description
#' Calls `PooledCohort::predict_10yr_stroke_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_10yr_stroke_risk <- function(...){
  PooledCohort::predict_10yr_stroke_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_10yr_hf_risk
#' @description
#' Calls `PooledCohort::predict_10yr_hf_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_10yr_hf_risk <- function(...){
  PooledCohort::predict_10yr_hf_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_10yr_cvd_risk
#' @description
#' Calls `PooledCohort::predict_10yr_cvd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_10yr_cvd_risk <- function(...){
  PooledCohort::predict_10yr_cvd_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_5yr_ascvd_risk
#' @description
#' Calls `PooledCohort::predict_5yr_ascvd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_5yr_ascvd_risk <- function(...){
  PooledCohort::predict_5yr_ascvd_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_10yr_chd_risk
#' @description
#' Calls `PooledCohort::predict_10yr_chd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_10yr_chd_risk <- function(...){
  PooledCohort::predict_10yr_chd_risk(...)
}

#' @title   Thin wrapper for PooledCohort::predict_10yr_ascvd_risk
#' @description
#' Calls `PooledCohort::predict_10yr_ascvd_risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
pooledcohort_predict_10yr_ascvd_risk <- function(...){
  PooledCohort::predict_10yr_ascvd_risk(...)
}

#' @title   Thin wrapper for QRISK3::QRISK3_2017
#' @description
#' Calls `QRISK3::QRISK3_2017(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
qrisk3_QRISK3_2017 <- function(...){
  QRISK3::QRISK3_2017(...)
}

#' @title   Thin wrapper for CVrisk::chd_10y_mesa
#' @description
#' Calls `CVrisk::chd_10y_mesa(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
cvrisk_chd_10y_mesa <- function(...){
  CVrisk::chd_10y_mesa(...)
}

#' @title   Thin wrapper for CVrisk::ascvd_10y_accaha
#' @description
#' Calls `CVrisk::ascvd_10y_accaha(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
cvrisk_ascvd_10y_accaha <- function(...){
  CVrisk::ascvd_10y_accaha(...)
}

#' @title   Thin wrapper for CVrisk::compute_CVrisk
#' @description
#' Calls `CVrisk::compute_CVrisk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
cvrisk_compute_CVrisk <- function(...){
  CVrisk::compute_CVrisk(...)
}

#' @title   Thin wrapper for CVrisk::ascvd_10y_frs_simple
#' @description
#' Calls `CVrisk::ascvd_10y_frs_simple(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
cvrisk_ascvd_10y_frs_simple <- function(...){
  CVrisk::ascvd_10y_frs_simple(...)
}

#' @title   Thin wrapper for CVrisk::ascvd_10y_frs
#' @description
#' Calls `CVrisk::ascvd_10y_frs(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
cvrisk_ascvd_10y_frs <- function(...){
  CVrisk::ascvd_10y_frs(...)
}

#' @title   Thin wrapper for CVrisk::chd_10y_mesa_cac
#' @description
#' Calls `CVrisk::chd_10y_mesa_cac(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
cvrisk_chd_10y_mesa_cac <- function(...){
  CVrisk::chd_10y_mesa_cac(...)
}

#' @title   Thin wrapper for whoishRisk::WHO_ISH_Risk
#' @description
#' Calls `whoishRisk::WHO_ISH_Risk(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
whoishrisk_WHO_ISH_Risk <- function(...){
  whoishRisk::WHO_ISH_Risk(...)
}

#' @title   Thin wrapper for RiskScorescvd::round_to_nearest_digit
#' @description
#' Calls `RiskScorescvd::round_to_nearest_digit(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_round_to_nearest_digit <- function(...){
  RiskScorescvd::round_to_nearest_digit(...)
}

#' @title   Thin wrapper for RiskScorescvd::HEART_scores
#' @description
#' Calls `RiskScorescvd::HEART_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_HEART_scores <- function(...){
  RiskScorescvd::HEART_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::GRACE
#' @description
#' Calls `RiskScorescvd::GRACE(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_GRACE <- function(...){
  RiskScorescvd::GRACE(...)
}

#' @title   Thin wrapper for RiskScorescvd::GRACE_scores
#' @description
#' Calls `RiskScorescvd::GRACE_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_GRACE_scores <- function(...){
  RiskScorescvd::GRACE_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::SCORE2
#' @description
#' Calls `RiskScorescvd::SCORE2(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_SCORE2 <- function(...){
  RiskScorescvd::SCORE2(...)
}

#' @title   Thin wrapper for RiskScorescvd::ASCVD
#' @description
#' Calls `RiskScorescvd::ASCVD(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_ASCVD <- function(...){
  RiskScorescvd::ASCVD(...)
}

#' @title   Thin wrapper for RiskScorescvd::calc_scores
#' @description
#' Calls `RiskScorescvd::calc_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_calc_scores <- function(...){
  RiskScorescvd::calc_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::SCORE2_Diabetes
#' @description
#' Calls `RiskScorescvd::SCORE2_Diabetes(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_SCORE2_Diabetes <- function(...){
  RiskScorescvd::SCORE2_Diabetes(...)
}

#' @title   Thin wrapper for RiskScorescvd::TIMI_scores
#' @description
#' Calls `RiskScorescvd::TIMI_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_TIMI_scores <- function(...){
  RiskScorescvd::TIMI_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::EDACS
#' @description
#' Calls `RiskScorescvd::EDACS(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_EDACS <- function(...){
  RiskScorescvd::EDACS(...)
}

#' @title   Thin wrapper for RiskScorescvd::SCORE2_CKD
#' @description
#' Calls `RiskScorescvd::SCORE2_CKD(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_SCORE2_CKD <- function(...){
  RiskScorescvd::SCORE2_CKD(...)
}

#' @title   Thin wrapper for RiskScorescvd::HEART
#' @description
#' Calls `RiskScorescvd::HEART(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_HEART <- function(...){
  RiskScorescvd::HEART(...)
}

#' @title   Thin wrapper for RiskScorescvd::ASCVD_scores
#' @description
#' Calls `RiskScorescvd::ASCVD_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_ASCVD_scores <- function(...){
  RiskScorescvd::ASCVD_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::TIMI
#' @description
#' Calls `RiskScorescvd::TIMI(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_TIMI <- function(...){
  RiskScorescvd::TIMI(...)
}

#' @title   Thin wrapper for RiskScorescvd::EDACS_scores
#' @description
#' Calls `RiskScorescvd::EDACS_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_EDACS_scores <- function(...){
  RiskScorescvd::EDACS_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::SCORE2_CKD_scores
#' @description
#' Calls `RiskScorescvd::SCORE2_CKD_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_SCORE2_CKD_scores <- function(...){
  RiskScorescvd::SCORE2_CKD_scores(...)
}

#' @title   Thin wrapper for RiskScorescvd::SCORE2_scores
#' @description
#' Calls `RiskScorescvd::SCORE2_scores(...)` directly so you
#' can forward arguments through HealthMarkers.
#'
#' @param ... Arguments passed to the underlying function.
#' @return A numeric vector or data.frame as returned by the original function.
#' @export
riskscorescvd_SCORE2_scores <- function(...){
  RiskScorescvd::SCORE2_scores(...)
}

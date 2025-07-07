#' ASCVD 10-year risk (ACC/AHA Pooled Cohort Equations)
#'
#' Wrapper around PooledCohort's ASCVD risk calculators.
#'
#' @param data A tibble with columns: \code{age}, \code{sex} (1=male,0=female), \code{race} ("white","black","other"),
#'   \code{smoker} (logical), \code{total_chol}, \code{HDL_c}, \code{sbp}, \code{bp_treated} (logical),
#'   \code{diabetes} (logical), \code{bmi}.
#' @param year Risk horizon: 10 or 30 years.
#' @param ... Additional arguments passed to the underlying PooledCohort function.
#' @return A tibble with columns \code{model}, \code{year}, and \code{risk} (numeric percentage).
#' @importFrom PooledCohort predict_10yr_ascvd_risk predict_30yr_ascvd_risk
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(
#'   age = 55, sex = 1, race = "white", smoker = FALSE,
#'   total_chol = 200, HDL_c = 50, sbp = 140, bp_treated = FALSE,
#'   diabetes = FALSE, bmi = 27
#' )
#' cvd_risk_ascvd(df, year = 10)
#' cvd_risk_ascvd(df, year = 30)
#' }
cvd_risk_ascvd <- function(data, year = 10, ...) {
  if (!requireNamespace("PooledCohort", quietly = TRUE)) {
    stop("Please install 'PooledCohort' to compute ASCVD risk.")
  }
  fn <- if (year == 10) PooledCohort::predict_10yr_ascvd_risk
  else               PooledCohort::predict_30yr_ascvd_risk
  risk <- fn(
    age_years       = data$age,
    race            = data$race,
    sex             = ifelse(data$sex == 1, "male", "female"),
    smoke_current   = ifelse(as.logical(data$smoker), "yes", "no"),
    chol_total_mgdl = data$total_chol,
    chol_hdl_mgdl   = data$HDL_c,
    bp_sys_mmhg     = data$sbp,
    bp_meds         = ifelse(as.logical(data$bp_treated), "yes", "no"),
    statin_meds     = "no",
    diabetes        = ifelse(as.logical(data$diabetes), "yes", "no"),
    bmi             = data$bmi,
    ...
  )
  tibble::tibble(model = "ASCVD", year = year, risk = risk)
}

#' QRISK3 10-year risk (UK QRISK3-2017)
#'
#' Wrapper around QRISK3::QRISK3_2017.
#'
#' @param data A tibble with columns required by QRISK3 (see package documentation).
#' @param ... Additional arguments for QRISK3_2017.
#' @return A tibble with columns \code{model}, \code{year} (always 10), and \code{risk}.
#' @importFrom QRISK3 QRISK3_2017
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(age=55, sex=1, sbp=140, HDL_c=50, total_chol=200, smoker=FALSE, diabetes=FALSE)
#' cvd_risk_qrisk3(df)
#' }
cvd_risk_qrisk3 <- function(data, ...) {
  if (!requireNamespace("QRISK3", quietly = TRUE)) {
    stop("Please install 'QRISK3' to compute QRISK3 risk.")
  }
  risk <- QRISK3::QRISK3_2017(data = data, ...)
  tibble::tibble(model = "QRISK3", year = 10, risk = risk)
}

#' MESA 10-year CHD risk
#'
#' Wrapper around CVrisk::chd_10y_mesa.
#'
#' @param data A tibble with columns: \code{race}, \code{sex}, \code{age}, \code{totchol}, \code{HDL_c}, \code{sbp}, \code{bp_treated}, \code{smoker}, \code{diabetes}.
#' @param ... Additional arguments for chd_10y_mesa.
#' @return A tibble with columns \code{model}, \code{year} (10), and \code{risk}.
#' @importFrom CVrisk chd_10y_mesa
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(race="white", sex=1, age=55, totchol=200, HDL_c=50,
#' sbp=140, bp_treated=FALSE, smoker=FALSE, diabetes=FALSE)
#' cvd_risk_mesa(df)
#' }
cvd_risk_mesa <- function(data, ...) {
  if (!requireNamespace("CVrisk", quietly = TRUE)) {
    stop("Please install 'CVrisk' to compute MESA risk.")
  }
  risk <- CVrisk::chd_10y_mesa(
    race           = data$race,
    gender         = ifelse(data$sex == 1, "male", "female"),
    age            = data$age,
    totChol        = data$total_chol,
    hdlChol        = data$HDL_c,
    sbp            = data$sbp,
    treatBP        = data$bp_treated,
    smoker         = data$smoker,
    diabetes       = data$diabetes,
    ...
  )
  tibble::tibble(model = "MESA", year = 10, risk = risk)
}

#' Stroke 10-year risk
#'
#' Wrapper around PooledCohort::predict_10yr_stroke_risk.
#'
#' @param data A tibble with columns: \code{age}, \code{sex}, \code{race}, \code{smoker}, \code{total_chol}, \code{HDL_c}, \code{sbp}, \code{bp_treated}, \code{diabetes}, \code{bmi}.
#' @param ... Additional arguments for predict_10yr_stroke_risk.
#' @return A tibble with columns \code{model}, \code{year} (10), and \code{risk}.
#' @importFrom PooledCohort predict_10yr_stroke_risk
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(age=55, sex=1, race="white", smoker=FALSE,
#' total_chol=200, HDL_c=50, sbp=140, bp_treated=FALSE,
#' diabetes=FALSE, bmi=27)
#' cvd_risk_stroke(df)
#' }
cvd_risk_stroke <- function(data, ...) {
  if (!requireNamespace("PooledCohort", quietly = TRUE)) {
    stop("Please install 'PooledCohort' to compute stroke risk.")
  }
  risk <- PooledCohort::predict_10yr_stroke_risk(
    age_years       = data$age,
    race            = data$race,
    sex             = ifelse(data$sex == 1, "male", "female"),
    smoke_current   = ifelse(as.logical(data$smoker), "yes", "no"),
    chol_total_mgdl = data$total_chol,
    chol_hdl_mgdl   = data$HDL_c,
    bp_sys_mmhg     = data$sbp,
    bp_meds         = ifelse(as.logical(data$bp_treated), "yes", "no"),
    statin_meds     = "no",
    diabetes        = ifelse(as.logical(data$diabetes), "yes", "no"),
    bmi             = data$bmi,
    egfr_mlminm2    = NULL,
    acr             = NULL,
    hba1c           = NULL,
    ...
  )
  tibble::tibble(model = "Stroke", year = 10, risk = risk)
}

#' RiskScorescvd main wrapper
#'
#' Wrapper around RiskScorescvd::calc_scores to compute selected CV scores
#'
#' @param data A tibble with columns required by RiskScorescvd::calc_scores (see package docs).
#' @param ... Additional arguments passed to calc_scores.
#' @return The raw output of calc_scores, typically a named numeric vector.
#' @importFrom RiskScorescvd calc_scores
#' @export
cvd_risk_scorescvd <- function(data, ...) {
  if (!requireNamespace("RiskScorescvd", quietly = TRUE)) {
    stop("Please install 'RiskScorescvd' to compute RiskScorescvd scores.")
  }
  RiskScorescvd::calc_scores(data = data, ...)
}

#' Compute cardiovascular risk by selected model
#'
#' Dispatch to the appropriate risk function
#'
#' @param data A tbl data.frame with required columns.
#' @param model One of c("ASCVD","QRISK3","MESA","Stroke","WHO","RiskScorescvd").
#' @param year Risk horizon for applicable models (10 or 30).
#' @param ... Additional args passed to underlying wrapper.
#' @export
cvd_risk <- function(data,
                     model = c("ASCVD","QRISK3","MESA","Stroke","WHO","RiskScorescvd"),
                     year  = 10, ...) {
  model <- match.arg(model)
  switch(model,
         "ASCVD"         = cvd_risk_ascvd(data, year = year, ...),
         "QRISK3"        = cvd_risk_qrisk3(data, ...),
         "MESA"          = cvd_risk_mesa(data, ...),
         "Stroke"        = cvd_risk_stroke(data, ...),
         "WHO"           = cvd_risk_who(data, ...),
         "RiskScorescvd" = cvd_risk_scorescvd(data, ...)
  )
}

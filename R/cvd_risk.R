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

# File: R/cvd_risk_qrisk3.R

#' QRISK3 10-year risk (UK QRISK3-2017)
#'
#' Wrapper around QRISK3::QRISK3_2017 that auto-provides a dummy `patid`
#' if you haven’t already given one.
#'
#' @inheritParams cvd_risk
#' @param col_map Not used here.
#' @param patid Optional vector of patient IDs; if missing, we auto-generate 1‒n
#' @return A tibble with columns `model`, `year`, and `risk`.
#' @importFrom QRISK3 QRISK3_2017
#' @importFrom tibble tibble
#' @export
cvd_risk_qrisk3 <- function(data,
                            ...,
                            patid = NULL) {
  if (!requireNamespace("QRISK3", quietly = TRUE)) {
    stop("Please install 'QRISK3' to compute QRISK3 risk.")
  }
  # QRISK3_2017 requires a patid argument
  if (is.null(patid)) {
    patid <- seq_len(nrow(data))
  }
  risk <- QRISK3::QRISK3_2017(
    data   = data,
    patid  = patid,
    ...
  )
  tibble::tibble(
    model = "QRISK3",
    year  = 10,
    risk  = risk
  )
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

#' Atherogenic Index of Plasma (AIP)
#'
#' Compute the log‐ratio of triglycerides to HDL‐C, a surrogate of small‐dense LDL.
#'
#' @param data A tibble or data.frame containing at least:
#'   - `TG`   (triglycerides, mg/dL)
#'   - `HDL_c` (HDL cholesterol, mg/dL)
#' @param col_map Named list mapping:
#'   - `TG`    → your TG column
#'   - `HDL_c` → your HDL‐C column
#' @param verbose Logical; if `TRUE`, prints a progress message.
#' @return A tibble with columns:
#'   - `model` = `"AIP"`
#'   - `value` = `log10(TG / HDL_c)`
#' @references
#' - Dobiásová M (2004). Atherogenic index of plasma [log(TG/HDL-C)]: theoretical and practical implications. *Clin Chem*. **50**(7): 1113–1114.
#' @export
cvd_marker_aip <- function(data,
                           col_map = list(TG = "TG", HDL_c = "HDL_c"),
                           verbose = FALSE) {
  validate_inputs(data, col_map, fun_name = "cvd_marker_aip", required_keys = c("TG", "HDL_c"))
  if (verbose) message("-> computing AIP")
  tg  <- data[[col_map$TG]]
  hdl <- data[[col_map$HDL_c]]
  value <- log10(tg / hdl)
  tibble::tibble(model = "AIP", value = value)
}

#' LDL Particle Number Estimate (via ApoB)
#'
#' Use circulating ApoB concentration as a proxy for LDL particle number.
#'
#' @param data A tibble or data.frame containing at least:
#'   - `ApoB` (apolipoprotein-B, mg/dL)
#' @param col_map Named list mapping:
#'   - `ApoB` → your ApoB column
#' @param verbose Logical; if `TRUE`, prints a progress message.
#' @return A tibble with columns:
#'   - `model` = `"LDL_PN"`
#'   - `value` = `ApoB` (mg/dL)
#' @references
#' - Walldius G, Jungner I (2004). The apoB/apoA-I ratio: a strong, new risk factor for cardiovascular disease and a target for lipid-lowering therapy—a review of the evidence. *J Intern Med*. **255**(2): 188–205.
#' @export
cvd_marker_ldl_particle_number <- function(data,
                                           col_map = list(ApoB = "ApoB"),
                                           verbose = FALSE) {
  validate_inputs(data, col_map, fun_name = "cvd_marker_ldl_particle_number", required_keys = "ApoB")
  if (verbose) message("-> computing LDL particle number estimate")
  value <- data[[col_map$ApoB]]
  tibble::tibble(model = "LDL_PN", value = value)
}





#' Compute cardiovascular risk or marker by selected model
#'
#' Dispatch to the appropriate risk or marker function, or run *all* of them.
#'
#' @param data A tibble or data.frame with the columns required by your chosen `model`
#' @param model One of:
#'   * `"ALL"` — run every implemented model/marker
#'   * Risk calculators: `"ASCVD"`, `"QRISK3"`, `"MESA"`, `"Stroke"`, `"WHO"`, `"RiskScorescvd"`
#'   * Lipid markers: `"AIP"`, `"LDL_PN"`
#' @param year Risk horizon (10 or 30) for applicable models; ignored for lipid markers
#' @param ... Additional arguments passed to the underlying wrapper (e.g. `verbose`, `col_map`)
#' @return A tibble.  Risk models return columns `model`, `year`, `risk`; lipid markers return `model`, `value`.
#'   When `model = "ALL"`, you get one row per sub-model and all of the columns (`model`,`year`,`risk`,`value`).
#' @export
cvd_risk <- function(data,
                     model = c("ALL","ASCVD","QRISK3","MESA","Stroke","WHO","RiskScorescvd","AIP","LDL_PN"),
                     year  = 10,
                     ...) {
  model <- match.arg(model)
  
  # If the user wants *every* model
  if (model == "ALL") {
    all_models <- c("ASCVD","QRISK3","MESA","Stroke","WHO","RiskScorescvd","AIP","LDL_PN")
    results <- lapply(all_models, function(m) {
      # for each one, try to compute or else return a placeholder with NAs
      tryCatch(
        cvd_risk(data, model = m, year = year, ...),
        error = function(e) {
          # build a one-row tibble with the right columns
          tibble::tibble(
            model = m,
            year  = if (m %in% c("ASCVD","Stroke")) year else if (m %in% c("QRISK3","MESA")) 10 else NA_integer_,
            risk  = NA_real_,
            value = NA_real_
          )
        }
      )
    })
    # row-bind them all; missing columns will be filled with NA
    return(dplyr::bind_rows(results))
  }
  
  # Otherwise dispatch normally
  switch(model,
         "ASCVD"         = cvd_risk_ascvd(data, year = year, ...),
         "QRISK3"        = cvd_risk_qrisk3(data, ...),
         "MESA"          = cvd_risk_mesa(data, ...),
         "Stroke"        = cvd_risk_stroke(data, ...),
         "WHO"           = cvd_risk_who(data, ...),
         "RiskScorescvd" = cvd_risk_scorescvd(data, ...),
         "AIP"           = cvd_marker_aip(data, ...),
         "LDL_PN"        = cvd_marker_ldl_particle_number(data, ...),
         stop("Unknown model: ", model)
  )
}

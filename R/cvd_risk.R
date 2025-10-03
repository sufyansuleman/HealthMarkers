# R/cvd_risk.R
# Core cardiovascular risk calculators & markers + dispatcher
# Optional deps live in Suggests: PooledCohort, QRISK3, CVrisk, RiskScorescvd

# ---- internal helpers ------------------------------------------------------

# Small runtime gate for optional packages in Suggests
.need_pkg <- function(pkg) {
  ok <- suppressMessages(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))
  if (!ok) {
    rlang::abort(sprintf("Package '%s' is required for this feature. Install it first.", pkg))
  }
}

# length-stable safe division (avoids Inf/NaN on zero denominators)
.safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

# Require columns exist in data
.require_cols <- function(df, cols, fun = "function") {
  miss <- setdiff(cols, names(df))
  if (length(miss)) stop(sprintf("%s(): missing required column(s): %s", fun, paste(miss, collapse = ", ")))
  invisible(TRUE)
}

# Validate simple named mapping (col_map)
.validate_map <- function(map, required, fun = "function") {
  if (!is.list(map) || is.null(names(map))) {
    stop(sprintf("%s(): `col_map` must be a named list.", fun))
  }
  miss <- setdiff(required, names(map))
  if (length(miss)) {
    stop(sprintf("%s(): missing col_map entries for: %s", fun, paste(miss, collapse = ", ")))
  }
  invisible(TRUE)
}

# Basic data-quality scan of variables; returns counts; can warn if asked
.quality_scan_warn <- function(df, vars, .warn = FALSE, na_warn_prop = 0.2, prefix = "") {
  nonfin <- character(0); high_na <- character(0); all_na <- character(0)
  for (v in vars) {
    x <- df[[v]]
    n_nonfin <- sum(!is.finite(x))
    if (n_nonfin > 0L) nonfin <- c(nonfin, sprintf("%s(%d non-finite)", v, n_nonfin))
    x_na <- sum(is.na(x) | !is.finite(x))
    if (length(x) > 0L && x_na == length(x)) all_na <- c(all_na, v)
    if (length(x) > 0L && x_na > 0L && (x_na / length(x)) >= na_warn_prop) {
      high_na <- c(high_na, sprintf("%s(%.1f%% NA)", v, 100 * x_na / length(x)))
    }
  }
  if (.warn) {
    if (length(nonfin)) warning(sprintf("%sNon-finite values: %s; treated as NA.", prefix, paste(nonfin, collapse = ", ")), call. = FALSE)
    if (length(all_na)) warning(sprintf("%sEntirely missing variables: %s.", prefix, paste(all_na, collapse = ", ")), call. = FALSE)
    if (length(high_na)) warning(sprintf("%sHigh missingness (>= %.0f%%): %s.", prefix, 100 * na_warn_prop, paste(high_na, collapse = ", ")), call. = FALSE)
  }
  list(nonfinite = nonfin, high_na = high_na, all_na = all_na)
}

# build a one-row placeholder with NA columns appropriate to a model
.na_row <- function(model, year = NA_integer_) {
  tibble::tibble(
    model = model,
    year  = year,
    risk  = NA_real_,
    value = NA_real_
  )
}

# ---- calculators (exported) ------------------------------------------------

#' ASCVD risk (ACC/AHA Pooled Cohort Equations)
#'
#' Wrapper around the PooledCohort ASCVD calculators with added input validation,
#' optional data-quality warnings, and quiet failure to NA if the backend errors.
#'
#' @param data A data frame with columns:
#'   \code{age}, \code{sex} (1 = male, 0 = female), \code{race} ("white","black","other"),
#'   \code{smoker} (logical), \code{total_chol}, \code{HDL_c}, \code{sbp},
#'   \code{bp_treated} (logical), \code{diabetes} (logical), \code{bmi}.
#' @param year Risk horizon: 10 or 30.
#' @param na_warn_prop Proportion (0–1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}; underlying backend handles NA as per its API.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to the underlying PooledCohort function.
#' @return A tibble with columns \code{model}, \code{year}, \code{risk} (percentage).
#' @export
#' @examples
#' df <- tibble::tibble(
#'   age = 55, sex = 1, race = "white", smoker = FALSE,
#'   total_chol = 200, HDL_c = 50, sbp = 140, bp_treated = FALSE,
#'   diabetes = FALSE, bmi = 27
#' )
#' if (requireNamespace("PooledCohort", quietly = TRUE)) {
#'   cvd_risk_ascvd(df, year = 10, verbose = TRUE)
#' }
#'
#' @references
#' - Goff DC Jr, Lloyd-Jones DM, Bennett G, et al. 2014 ACC/AHA Guideline on the
#'   Assessment of Cardiovascular Risk: a report of the ACC/AHA Task Force on Practice
#'   Guidelines. Circulation. 2014;129(25 Suppl 2):S49–S73. (Pooled Cohort Equations)
cvd_risk_ascvd <- function(data, year = 10, na_warn_prop = 0.2, verbose = FALSE, ...) {
  .need_pkg("PooledCohort")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.numeric(year) || length(year) != 1L || !(year %in% c(10, 30))) {
    stop("`year` must be 10 or 30.")
  }
  req <- c("age","sex","race","smoker","total_chol","HDL_c","sbp","bp_treated","diabetes","bmi")
  .require_cols(data, req, fun = "cvd_risk_ascvd")
  if (verbose) {
    qs <- .quality_scan_warn(data, req, .warn = FALSE, na_warn_prop = na_warn_prop)
    message(sprintf("-> ASCVD inputs ok; non-finite=%d var(s), high-NA=%d, all-NA=%d",
                    length(qs$nonfinite), length(qs$high_na), length(qs$all_na)))
  }
  res <- try({
    fn <- if (year == 10) PooledCohort::predict_10yr_ascvd_risk else PooledCohort::predict_30yr_ascvd_risk
    fn(
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
  }, silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0L) {
    return(tibble::tibble(model = "ASCVD", year = as.integer(year), risk = NA_real_))
  }
  out <- tibble::tibble(model = "ASCVD", year = as.integer(year), risk = as.double(res))
  if (verbose) message(sprintf("-> ASCVD %dyr: completed (%d row[s])", as.integer(year), nrow(out)))
  return(out)
}

#' QRISK3 10-year risk (UK QRISK3-2017)
#'
#' Wrapper around \code{QRISK3::QRISK3_2017()} that auto-generates a \code{patid}
#' if one is not supplied. Adds input validation and quiet failure to NA on backend error.
#'
#' @param data A data frame with variables required by \code{QRISK3::QRISK3_2017()}.
#' @param patid Optional vector of patient IDs (default: \code{1:nrow(data)}).
#' @param na_warn_prop Proportion (0–1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to \code{QRISK3::QRISK3_2017()}.
#' @return A tibble with columns \code{model}, \code{year}, \code{risk}.
#' @export
#' @examples
#' if (requireNamespace("QRISK3", quietly = TRUE)) {
#'   # cvd_risk_qrisk3(your_data_frame, verbose = TRUE)
#' }
#'
#' @references
#' - Hippisley-Cox J, Coupland C, Brindle P. Development and validation of QRISK3
#'   risk prediction algorithms. BMJ. 2017;357:j2099.
cvd_risk_qrisk3 <- function(data, ..., patid = NULL, na_warn_prop = 0.2, verbose = FALSE) {
  .need_pkg("QRISK3")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (is.null(patid)) patid <- seq_len(nrow(data))
  if (verbose) {
    message("-> QRISK3 inputs received")
    # Cannot reliably pre-validate all QRISK3 inputs without duplicating its spec
  }
  res <- try(QRISK3::QRISK3_2017(data = data, patid = patid, ...), silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0L) {
    return(tibble::tibble(model = "QRISK3", year = 10L, risk = NA_real_))
  }
  out <- tibble::tibble(model = "QRISK3", year = 10L, risk = as.double(res))
  if (verbose) message(sprintf("-> QRISK3: completed (%d row[s])", nrow(out)))
  return(out)
}

#' MESA 10-year CHD risk
#'
#' Wrapper around \code{CVrisk::chd_10y_mesa()} with graceful fallback to NA if the
#' backend errors.
#'
#' @param data Data frame with: \code{race}, \code{sex}, \code{age}, \code{total_chol},
#'   \code{HDL_c}, \code{sbp}, \code{bp_treated}, \code{smoker}, \code{diabetes}.
#' @param na_warn_prop Proportion (0–1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to \code{CVrisk::chd_10y_mesa()}.
#' @return A tibble with \code{model}, \code{year}, \code{risk}.
#' @export
#' @examples
#' if (requireNamespace("CVrisk", quietly = TRUE)) {
#'   # cvd_risk_mesa(df, verbose = TRUE)
#' }
#'
#' @references
#' - McClelland RL, et al. Ten-year coronary heart disease risk prediction using
#'   coronary artery calcium and traditional risk factors: the MESA risk score.
#'   Circulation. 2015;131(5):402–409.
cvd_risk_mesa <- function(data, na_warn_prop = 0.2, verbose = FALSE, ...) {
  .need_pkg("CVrisk")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  # Some CVrisk versions expect an internal dataset; try to load gracefully
  try(utils::data("mesa_coef", package = "CVrisk"), silent = TRUE)
  if (verbose) {
    req <- c("race","sex","age","total_chol","HDL_c","sbp","bp_treated","smoker","diabetes")
    qs <- .quality_scan_warn(data, req, .warn = FALSE, na_warn_prop = na_warn_prop)
    message(sprintf("-> MESA inputs check; non-finite=%d, high-NA=%d, all-NA=%d",
                    length(qs$nonfinite), length(qs$high_na), length(qs$all_na)))
  }
  res <- suppressWarnings(try(
    CVrisk::chd_10y_mesa(
      race     = data$race,
      gender   = ifelse(data$sex == 1, "male", "female"),
      age      = data$age,
      totchol  = data$total_chol,
      hdl      = data$HDL_c,
      sbp      = data$sbp,
      bp_med   = as.integer(isTRUE(data$bp_treated)),
      smoker   = as.integer(isTRUE(data$smoker)),
      diabetes = as.integer(isTRUE(data$diabetes)),
      ...
    ),
    silent = TRUE
  ))
  if (inherits(res, "try-error") || length(res) == 0L || is.null(res)) {
    return(tibble::tibble(model = "MESA", year = 10L, risk = NA_real_))
  }
  out <- tibble::tibble(model = "MESA", year = 10L, risk = as.double(res))
  if (verbose) message(sprintf("-> MESA: completed (%d row[s])", nrow(out)))
  return(out)
}

#' Stroke 10-year risk
#'
#' Wrapper around \code{PooledCohort::predict_10yr_stroke_risk()} with quiet
#' fallback to NA if the backend errors.
#'
#' @param data Data frame with: \code{age}, \code{sex}, \code{race}, \code{smoker},
#'   \code{total_chol}, \code{HDL_c}, \code{sbp}, \code{bp_treated}, \code{diabetes}, \code{bmi}.
#' @param na_warn_prop Proportion (0–1) to flag high missingness warnings (default 0.2).
#'   Only used when \code{verbose = TRUE}.
#' @param verbose Logical; if TRUE, prints progress and a short summary.
#' @param ... Passed to \code{PooledCohort::predict_10yr_stroke_risk()}.
#' @return A tibble with \code{model}, \code{year}, \code{risk}.
#' @export
#'
#' @references
#' - Goff DC Jr, Lloyd-Jones DM, Bennett G, et al. 2014 ACC/AHA Guideline
#'   (Pooled Cohort Equations include stroke prediction component). Circulation. 2014;129:S49–S73.
cvd_risk_stroke <- function(data, na_warn_prop = 0.2, verbose = FALSE, ...) {
  .need_pkg("PooledCohort")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (verbose) {
    req <- c("age","sex","race","smoker","total_chol","HDL_c","sbp","bp_treated","diabetes","bmi")
    qs <- .quality_scan_warn(data, req, .warn = FALSE, na_warn_prop = na_warn_prop)
    message(sprintf("-> Stroke inputs ok; non-finite=%d, high-NA=%d, all-NA=%d",
                    length(qs$nonfinite), length(qs$high_na), length(qs$all_na)))
  }
  res <- try({
    PooledCohort::predict_10yr_stroke_risk(
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
      equation_version = "Goff_2013",  # use PCEs; avoids CKD inputs in PREVENT
      ...
    )
  }, silent = TRUE)
  if (inherits(res, "try-error") || length(res) == 0L) {
    return(tibble::tibble(model = "Stroke", year = 10L, risk = NA_real_))
  }
  out <- tibble::tibble(model = "Stroke", year = 10L, risk = as.double(res))
  if (verbose) message(sprintf("-> Stroke: completed (%d row[s])", nrow(out)))
  return(out)
}

#' RiskScorescvd calculator
#'
#' Passthrough to \code{RiskScorescvd::calc_scores()} with graceful fallback to NA.
#'
#' @param data Data required by \code{RiskScorescvd::calc_scores()}.
#' @param ... Passed to \code{RiskScorescvd::calc_scores()}.
#' @return Object returned by \code{RiskScorescvd::calc_scores()}.
#' @export
cvd_risk_scorescvd <- function(data, ...) {
  .need_pkg("RiskScorescvd")
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  out <- tryCatch(RiskScorescvd::calc_scores(data = data, ...),
                  error = function(e) .na_row("RiskScorescvd", NA_integer_))
  return(out)
}

# ---- lipid markers (exported) ----------------------------------------------

#' Atherogenic Index of Plasma (AIP)
#'
#' Computes \code{log10(TG / HDL_c)} with input validation, optional data-quality
#' warnings, and verbose progress.
#'
#' @param data A data frame with numeric columns \code{TG} (mg/dL) and \code{HDL_c} (mg/dL).
#' @param col_map Named list mapping \code{TG} and \code{HDL_c} to your column names.
#' @param na_action One of "ignore", "warn", "error" controlling behavior when
#'   required inputs are missing or non-finite. Default "ignore".
#' @param na_warn_prop Proportion (0–1) threshold for high-missingness warnings when \code{na_action="warn"}.
#' @param verbose Logical; if \code{TRUE}, prints a progress message and summary.
#' @return A tibble with columns \code{model} = \code{"AIP"} and \code{value}.
#' @export
#' @examples
#' df <- tibble::tibble(TG = c(120, 180), HDL_c = c(50, 40))
#' cvd_marker_aip(df, verbose = TRUE)
#'
#' @references
#' - Dobiášová M, Frohlich J. The atherogenic index of plasma \eqn{\log(TG/HDL)}:
#'   relation to lipoprotein particle size. Clin Biochem. 2001;34(7):583–587.
cvd_marker_aip <- function(data,
                           col_map = list(TG = "TG", HDL_c = "HDL_c"),
                           na_action = c("ignore","warn","error"),
                           na_warn_prop = 0.2,
                           verbose = FALSE) {
  na_action <- match.arg(na_action)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  .validate_map(col_map, c("TG","HDL_c"), fun = "cvd_marker_aip")
  .require_cols(data, unlist(col_map[c("TG","HDL_c")]), fun = "cvd_marker_aip")
  if (verbose) message("-> computing AIP")
  tg_col <- col_map$TG; hdl_col <- col_map$HDL_c
  tg  <- data[[tg_col]]
  hdl <- data[[hdl_col]]
  qs <- .quality_scan_warn(data, c(tg_col, hdl_col), .warn = (na_action == "warn"), na_warn_prop = na_warn_prop, prefix = "AIP: ")
  if (na_action == "error") {
    if (any(!is.finite(tg)) || any(!is.finite(hdl))) stop("AIP: missing or non-finite TG/HDL_c with na_action='error'.")
  }
  value <- log10(.safe_div(tg, hdl))
  out <- tibble::tibble(model = "AIP", value = value)
  if (verbose) {
    message(sprintf("Completed AIP: %d rows; non-finite vars=%d; high-NA=%d; all-NA=%d",
                    nrow(out), length(qs$nonfinite), length(qs$high_na), length(qs$all_na)))
  }
  return(out)
}

#' LDL Particle Number Estimate (via ApoB)
#'
#' Uses circulating ApoB concentration as a proxy for LDL particle number.
#' Input validation, optional data-quality warnings, and verbose summary included.
#'
#' @param data A data frame with numeric column \code{ApoB} (mg/dL).
#' @param col_map Named list mapping \code{ApoB} to your column name.
#' @param na_action One of "ignore","warn","error" for missing/non-finite inputs (default "ignore").
#' @param na_warn_prop Proportion (0–1) for high-missingness warnings when \code{na_action="warn"}.
#' @param verbose Logical; if \code{TRUE}, prints a progress message.
#' @return A tibble with columns \code{model} = \code{"LDL_PN"} and \code{value}.
#' @export
#' @examples
#' df <- tibble::tibble(ApoB = c(80, 95))
#' cvd_marker_ldl_particle_number(df, verbose = TRUE)
#'
#' @references
#' - Sniderman AD, et al. Apolipoprotein B and cardiovascular disease: pathophysiology
#'   and clinical utility as an index of atherogenic particle number. Arterioscler Thromb
#'   Vasc Biol. 2003;23(10):1723–1731.
cvd_marker_ldl_particle_number <- function(data,
                                           col_map = list(ApoB = "ApoB"),
                                           na_action = c("ignore","warn","error"),
                                           na_warn_prop = 0.2,
                                           verbose = FALSE) {
  na_action <- match.arg(na_action)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  .validate_map(col_map, c("ApoB"), fun = "cvd_marker_ldl_particle_number")
  .require_cols(data, unlist(col_map["ApoB"]), fun = "cvd_marker_ldl_particle_number")
  if (verbose) message("-> computing LDL particle number estimate")
  apob_col <- col_map$ApoB
  apob <- data[[apob_col]]
  qs <- .quality_scan_warn(data, apob_col, .warn = (na_action == "warn"), na_warn_prop = na_warn_prop, prefix = "LDL_PN: ")
  if (na_action == "error" && any(!is.finite(apob))) stop("LDL_PN: missing or non-finite ApoB with na_action='error'.")
  out <- tibble::tibble(model = "LDL_PN", value = apob)
  if (verbose) {
    message(sprintf("Completed LDL_PN: %d rows; non-finite vars=%d; high-NA=%d; all-NA=%d",
                    nrow(out), length(qs$nonfinite), length(qs$high_na), length(qs$all_na)))
  }
  return(out)
}

# ---- dispatcher (exported) -------------------------------------------------

#' Compute cardiovascular risk or marker by selected model
#'
#' Dispatch to the appropriate risk or marker function, or run all of them.
#' Includes basic argument validation, optional progress messages, and robust
#' fallback to NA rows if individual calculators fail.
#'
#' @param data A data frame with the columns required by your chosen \code{model}.
#' @param model One of:
#'   \itemize{
#'     \item \code{"ALL"} - run every implemented model/marker
#'     \item Risk calculators: \code{"ASCVD"}, \code{"QRISK3"}, \code{"MESA"},
#'           \code{"Stroke"}, \code{"RiskScorescvd"}
#'     \item Lipid markers: \code{"AIP"}, \code{"LDL_PN"}
#'   }
#' @param year Risk horizon (10 or 30) for applicable models; ignored for lipid markers.
#' @param verbose Logical; if TRUE, prints step-by-step progress and a completion summary.
#' @param ... Additional arguments passed to the underlying wrapper (e.g. \code{na_action}, \code{na_warn_prop}, \code{col_map}).
#' @return A tibble. Risk models return columns \code{model}, \code{year}, \code{risk};
#'   lipid markers return \code{model}, \code{value}. When \code{model = "ALL"},
#'   you get one row per sub-model; missing columns are filled with \code{NA}.
#' @export
cvd_risk <- function(data,
                     model = c("ALL", "ASCVD", "QRISK3", "MESA",
                               "Stroke", "RiskScorescvd", "AIP", "LDL_PN"),
                     year = 10,
                     ...,
                     verbose = FALSE) {
  model <- match.arg(model)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.numeric(year) || length(year) != 1L) stop("`year` must be a single numeric (e.g., 10, 30).")
  if (verbose) message(sprintf("cvd_risk: dispatching model '%s'%s", model, if (model %in% c("ASCVD","Stroke")) paste0(" (year=", as.integer(year), ")") else ""))

  if (model == "ALL") {
    all_models <- c("ASCVD", "QRISK3", "MESA", "Stroke", "RiskScorescvd", "AIP", "LDL_PN")
    results <- lapply(all_models, function(m) {
      out <- try(cvd_risk(data, model = m, year = year, ..., verbose = verbose), silent = TRUE)
      if (inherits(out, "try-error") || !is.data.frame(out) || nrow(out) == 0L) {
        # choose an appropriate year for placeholder
        yr <- if (m %in% c("ASCVD", "Stroke")) as.integer(year) else if (m %in% c("QRISK3", "MESA")) 10L else NA_integer_
        return(.na_row(m, yr))
      }
      out
    })
    out_all <- dplyr::bind_rows(results)
    if (verbose) message(sprintf("cvd_risk: completed ALL (%d row[s])", nrow(out_all)))
    return(out_all)
  }

  out <- switch(model,
    "ASCVD"         = cvd_risk_ascvd(data, year = year, ..., verbose = verbose),
    "QRISK3"        = cvd_risk_qrisk3(data, ..., verbose = verbose),
    "MESA"          = cvd_risk_mesa(data, ..., verbose = verbose),
    "Stroke"        = cvd_risk_stroke(data, ..., verbose = verbose),
    "RiskScorescvd" = cvd_risk_scorescvd(data, ...),
    "AIP"           = cvd_marker_aip(data, ..., verbose = verbose),
    "LDL_PN"        = cvd_marker_ldl_particle_number(data, ..., verbose = verbose),
    stop("Unknown model: ", model)
  )
  if (verbose) message("cvd_risk: done")
  return(out)
}

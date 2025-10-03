# R/fasting_is.R

#' Calculate fasting-based insulin sensitivity indices
#'
#' Given a data.frame with fasting glucose and insulin, compute the following
#' fasting-based indices (directionality preserved as in the original code):
#' - Fasting_inv = -I0_u
#' - Raynaud = 40 / I0_u
#' - HOMA_IR_inv = -((G0_mg * I0_u) / 22.5)
#' - FIRI = (G0_mg * I0_u) / 25
#' - QUICKI = 1 / (log(G0_mg) + log(I0_u))
#' - Belfiore_basal = 2 / ((I0_u * G0_mg) + 1)
#' - Ig_ratio_basal = -(I0_u / G0_mg)
#' - Isi_basal = 10000 / (G0_mg * I0_u)
#' - Bennett = 1 / (log(I0_u) * log(G0_mg))
#' - HOMA_IR_rev_inv = -((I0_u * G0_mg) / 405)
#'
#' Units assumed:
#' - Fasting glucose G0 in mmol/L (converted internally to mg/dL via G0_mg = G0 * 18).
#' - Fasting insulin I0 in pmol/L (converted internally to µU/mL via I0_u = I0 / 6).
#'
#' Quality controls and options:
#' - Input validation ensures required mappings exist and columns are present.
#' - Non-numeric inputs are coerced to numeric with a warning (NAs introduced reported).
#' - Missing or non-finite inputs are handled via `na_action`.
#' - Logs are safely computed (log(x) returns NA for x <= 0).
#' - Optional detection/handling of extreme output values via `check_extreme`.
#' - Verbose mode prints step-by-step progress and a completion summary.
#'
#' @param data A data.frame or tibble containing at least two columns:
#'   - fasting glucose at time 0 (mmol/L), mapped by `col_map$G0`
#'   - fasting insulin at time 0 (pmol/L), mapped by `col_map$I0`
#' @param col_map Named list mapping:
#'   - `G0` -> your fasting glucose column name
#'   - `I0` -> your fasting insulin column name
#' @param normalize One of `c("none","z","inverse","range","robust")` controlling
#'   post-hoc scaling of each index via `HealthMarkers::normalize_vec()`. Default "none".
#' @param na_action One of `c("ignore","warn","error")` controlling behavior when
#'   required inputs are missing or non-finite. Default "ignore".
#' @param na_warn_prop Proportion (0–1) threshold for high-missingness warnings when
#'   `na_action = "warn"`. Default 0.2.
#' @param check_extreme Logical; if `TRUE`, scan computed indices for absolute values
#'   exceeding `extreme_limit`. Default `FALSE`.
#' @param extreme_limit Positive numeric; magnitude limit for `check_extreme`. Default 1e3.
#' @param extreme_action One of `c("warn","cap","error","ignore")` controlling what to do
#'   when extreme values are detected. If "cap", values are truncated to ±`extreme_limit`.
#'   Default "warn".
#' @param verbose Logical; if `TRUE`, prints step-by-step progress and a completion summary.
#'
#' @return A tibble with the 10 fasting-index columns listed above (order preserved).
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across everything
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   G0 = c(5.5, 6.1),   # mmol/L
#'   I0 = c(60, 120)     # pmol/L
#' )
#'
#' # Defaults (quiet, no normalization), tidy tibble returned
#' fasting_is(
#'   df,
#'   col_map   = list(G0 = "G0", I0 = "I0")
#' )
#'
#' # Verbose with warnings for high missingness, and extreme checking
#' fasting_is(
#'   df,
#'   col_map        = list(G0 = "G0", I0 = "I0"),
#'   na_action      = "warn",
#'   na_warn_prop   = 0.1,
#'   check_extreme  = TRUE,
#'   extreme_limit  = 1e3,
#'   extreme_action = "warn",
#'   verbose        = TRUE
#' )
#'
#' @references
#' Matthews DR, Hosker JP, Rudenski AS, Naylor BA, Treacher DF, Turner RC (1985). Homeostasis model assessment (HOMA): insulin resistance and β-cell function. Diabetologia, 28(7):412–419. \doi{10.1007/BF00280883}
#' Katz A, Nambi S, Mather K, Baron AD, Follmann DA, Sullivan G, Quon MJ (2000). Quantitative insulin sensitivity check index (QUICKI). J Clin Endocrinol Metab, 85(7):2402–2410. \doi{10.1210/jcem.85.7.6661}
#' Raynaud E, Pérez-Martin A, Brun JF, Benhaddad AA, Mercier J (1999). Fasting plasma insulin and insulin resistance indices. Diabetes Metab, 25(6):524–532.
#' Avignon A, Boegner C, Sultan A (1999). Simple assessment of insulin sensitivity from fasting insulin and glucose. Int J Obes Relat Metab Disord, 23(5):512–517.
#' Belfiore F, Iannello S, Volpicelli G (1998). Insulin sensitivity indices from basal insulin and glucose. Mol Genet Metab, 63(2):134–141.
#' Sluiter et al. Glucose tolerance and insulin release a mathematical approach. Diabetes. 1976;25:245–249.
#'   (Early use of fasting insulin/glucose ratios and log-products; background for Bennett/FIRI-type indices)
#' Hanson RL et al. Evaluation of simple indices of insulin sensitivity and insulin secretion for use in epidemiologic studies. Am J Epidemiol. 2000;151(2):190–198.
#' Anderson RL et al. Exploration of simple measures of insulin resistance. Am J Epidemiol. 1995;142(7):724–732.
fasting_is <- function(
  data,
  col_map,
  normalize = c("none","z","inverse","range","robust"),
  na_action = c("zero","warn_zero","error","keep"),
  verbose = FALSE,
  check_extreme = FALSE,
  extreme_limit = 1e3,
  extreme_action = c("warn","cap","error","ignore")
) {
  # ---- Argument normalization ----
  if (is.null(na_action) || !length(na_action) || all(is.na(na_action))) na_action <- "zero"
  na_action <- match.arg(na_action)
  if (na_action == "keep") na_action <- "zero"

  extreme_action <- match.arg(extreme_action)

  # Custom normalize validation (replaces match.arg to control message)
  allowed_normalize <- c("none","z","inverse","range","robust")
  if (is.null(normalize) || !length(normalize) || all(is.na(normalize))) normalize <- "none"
  normalize <- normalize[1]
  if (!normalize %in% allowed_normalize)
    stop("`normalize` must be one of: none, z, inverse, range, robust.")

  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.list(col_map) || any(c("G0","I0") %in% names(col_map) == FALSE))
    stop("col_map must be a named list with entries G0 and I0.")

  # Basic validation for new extreme args
  if (!(is.logical(check_extreme) && length(check_extreme) == 1L && !is.na(check_extreme)))
    stop("`check_extreme` must be a single logical.")
  if (!(is.numeric(extreme_limit) && length(extreme_limit) == 1L && is.finite(extreme_limit) && extreme_limit > 0))
    stop("`extreme_limit` must be a single positive numeric.")

  # Column presence
  miss <- setdiff(unlist(col_map[c("G0","I0")]), names(data))
  if (length(miss)) stop("Missing required columns: ", paste(miss, collapse = ", "))

  if (verbose) message("-> fasting_is: validating and preparing inputs")

  # Coerce to numeric
  for (nm in c("G0","I0")) {
    cn <- col_map[[nm]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      if (any(is.na(data[[cn]]) & !is.na(old)))
        warning(sprintf("Column '%s' coerced to numeric; NAs introduced.", cn), call. = FALSE)
    }
  }

  G0 <- data[[col_map$G0]]
  I0 <- data[[col_map$I0]]

  # Missingness scan
  scan_nf <- function(x) list(na = sum(is.na(x) | !is.finite(x)), total = length(x))
  sG <- scan_nf(G0); sI <- scan_nf(I0)

  if (na_action == "error" && (sG$na > 0 || sI$na > 0))
    stop("fasting_is: missing/non-finite inputs with na_action='error'.")

  if (na_action == "warn_zero") {
    if (sG$na / sG$total >= 0.2) warning("High missingness in glucose.", call. = FALSE)
    if (sI$na / sI$total >= 0.2) warning("High missingness in insulin.", call. = FALSE)
  }

  if (verbose) message("-> converting units (glucose mmol/L -> mg/dL; insulin pmol/L -> µU/mL)")
  G0_mg <- G0 * 18
  I0_u  <- I0 / 6

  if (verbose) message("-> computing indices")

  lg <- function(x) {
    y <- x
    y[!(is.finite(y) & y > 0)] <- NA_real_
    out <- suppressWarnings(log(y))
    out[!is.finite(out)] <- NA_real_
    out
  }
  sdiv <- function(a, b) {
    z <- a / b
    z[!is.finite(z)] <- NA_real_
    z
  }

  out <- tibble::tibble(
    Fasting_inv     = -I0_u,
    Raynaud         = sdiv(40, I0_u),
    HOMA_IR_inv     = -sdiv(G0_mg * I0_u, 22.5),
    FIRI            =  sdiv(G0_mg * I0_u, 25),
    QUICKI          = sdiv(1, lg(G0_mg) + lg(I0_u)),
    Belfiore_basal  = sdiv(2, (I0_u * G0_mg) + 1),
    Ig_ratio_basal  = -sdiv(I0_u, G0_mg),
    Isi_basal       = sdiv(10000, G0_mg * I0_u),
    Bennett         = sdiv(1, lg(I0_u) * lg(G0_mg)),
    HOMA_IR_rev_inv = -sdiv(I0_u * G0_mg, 405)
  )

  # NA handling conversion
  if (na_action %in% c("zero","warn_zero")) {
    out <- dplyr::mutate(out, dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), 0, .x)))
  }

  # Extreme value handling (new)
  if (check_extreme) {
    m <- as.matrix(out)
    ext_mask <- is.finite(m) & abs(m) > extreme_limit
    n_ext <- sum(ext_mask)
    if (n_ext > 0) {
      if (extreme_action == "error") {
        stop(sprintf("fasting_is: %d extreme values beyond ±%g.", n_ext, extreme_limit))
      } else if (extreme_action == "cap") {
        m[ext_mask & m > 0] <- extreme_limit
        m[ext_mask & m < 0] <- -extreme_limit
        out[] <- m
      } else if (extreme_action == "warn") {
        warning(sprintf("fasting_is: %d extreme values beyond ±%g (not capped).", n_ext, extreme_limit), call. = FALSE)
      }
    }
  }

  # Normalization
  if (normalize != "none") {
    out <- dplyr::mutate(out, dplyr::across(dplyr::everything(),
                                            ~ HealthMarkers::normalize_vec(.x, method = normalize)))
  }

  if (verbose) {
    message(sprintf("Completed fasting_is: %d rows.", nrow(out)))
  }

  out
}

# ---- internal helpers (not exported) ----------------------------------------

.fi_validate_args <- function(normalize, na_warn_prop, check_extreme, extreme_limit, verbose) {
  ok_norm <- normalize %in% c("none","z","inverse","range","robust")
  if (!ok_norm) stop("`normalize` must be one of: 'none','z','inverse','range','robust'.")
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    stop("`na_warn_prop` must be a single numeric in [0, 1].")
  }
  if (!(is.logical(check_extreme) && length(check_extreme) == 1L && !is.na(check_extreme))) {
    stop("`check_extreme` must be a single logical value.")
  }
  if (!(is.numeric(extreme_limit) && length(extreme_limit) == 1L && is.finite(extreme_limit) && extreme_limit > 0)) {
    stop("`extreme_limit` must be a single positive finite numeric.")
  }
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    stop("`verbose` must be a single logical value.")
  }
  invisible(TRUE)
}

.fi_quality_scan <- function(df, vars, .warn = FALSE, na_warn_prop = 0.2) {
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
    if (length(nonfin)) warning(sprintf("Non-finite values: %s; treated as NA.", paste(nonfin, collapse = ", ")), call. = FALSE)
    if (length(all_na)) warning(sprintf("Entirely missing variables: %s.", paste(all_na, collapse = ", ")), call. = FALSE)
    if (length(high_na)) warning(sprintf("High missingness (>= %.0f%%): %s.", 100 * na_warn_prop, paste(high_na, collapse = ", ")), call. = FALSE)
  }
  list(nonfinite = nonfin, high_na = high_na, all_na = all_na)
}

.fi_safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

.fi_log <- function(x) {
  y <- x
  y[!(is.finite(y) & y > 0)] <- NA_real_
  out <- suppressWarnings(log(y))
  out[!is.finite(out)] <- NA_real_
  out
}

.fi_cap_matrix <- function(m, lim) {
  out <- m
  # cap only finite entries exceeding limit
  over <- is.finite(out) & out > lim
  under <- is.finite(out) & out < -lim
  out[over]  <- lim
  out[under] <- -lim
  out
}

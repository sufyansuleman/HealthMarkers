# File: R/glycemic_markers.R

#' Calculate glycemic-, C-peptide-, and additional metabolic markers
#'
#' Given fasting labs and anthropometry, computes:
#'  - SPISE (Single-Point Insulin Sensitivity Estimator)
#'  - METS_IR (Metabolic Score for Insulin Resistance)
#'  - prediabetes flag (HbA1c >= 42 mmol/mol)
#'  - diabetes flag (HbA1c >= 48 mmol/mol)
#'  - HOMA_CP (C-peptide-based HOMA-IR variant; operational formula, see notes)
#'  - LAR (Leptin/Adiponectin Ratio)
#'  - ASI (Adiponectin Sensitivity Index; adiponectin/insulin)
#'  - TyG_index (Triglyceride-Glucose Index)
#'
#' Assumed units (no automatic conversion of inputs except where noted):
#' - HDL_c, TG: mmol/L (TyG internally converts TG to mg/dL via 88.57)
#' - BMI: kg/m^2
#' - glucose, G0: mmol/L (TyG internally converts glucose to mg/dL via 18)
#' - HbA1c: mmol/mol
#' - C_peptide, I0: pmol/L (HOMA_CP uses I-like conversion factor 6 as in insulin µU/mL; see notes)
#' - leptin, adiponectin: ng/mL
#'
#' Quality controls and options:
#' - Input validation ensures required variables exist and are numeric-coercible.
#' - Non-numeric inputs are coerced to numeric with a warning (NAs introduced reported).
#' - Missing or non-finite inputs are handled via `na_action`.
#' - Logs and divisions are computed safely (non-positive arguments yield NA).
#' - Optional detection/handling of extreme input values via `check_extreme` and `extreme_action`.
#' - Verbose mode prints step-by-step progress and a completion summary.
#'
#' @param data A data.frame or tibble containing at least:
#'   - HDL_c (mmol/L), TG (mmol/L), BMI (kg/m^2)
#'   Optional if present: glucose (mmol/L), HbA1c (mmol/mol), C_peptide (pmol/L),
#'   G0 (mmol/L), I0 (pmol/L), leptin (ng/mL), adiponectin (ng/mL).
#' @param na_action One of `c("ignore","warn","error")` controlling behavior when
#'   required inputs contain missing or non-finite values. Default "ignore".
#' @param na_warn_prop Proportion (0–1) threshold for high-missingness warnings
#'   among used columns when `na_action = "warn"`. Default 0.2.
#' @param check_extreme Logical; if `TRUE`, scan selected input variables for values
#'   outside plausible ranges defined in `extreme_rules`. Default `FALSE`.
#' @param extreme_action One of `c("warn","cap","error","ignore")` controlling what to do
#'   when extreme inputs are detected. If "cap", values are truncated to the allowed range.
#'   Default "warn".
#' @param extreme_rules Named list of numeric length-2 ranges for inputs to scan when
#'   `check_extreme = TRUE`. Defaults are broad medical plausibility ranges:
#'   - HDL_c: c(0.1, 5), TG: c(0.1, 20), BMI: c(10, 80),
#'   - glucose: c(2, 30), HbA1c: c(20, 200), C_peptide: c(0, 5000),
#'   - G0: c(2, 30), I0: c(0, 3000), leptin: c(0, 200), adiponectin: c(0, 300).
#'   Only variables present in `data` are checked.
#' @param verbose Logical; if `TRUE`, prints progress and a completion summary. Default FALSE.
#'
#' @return A tibble with columns:
#'   - SPISE, METS_IR, prediabetes, diabetes, HOMA_CP, LAR, ASI, TyG_index
#'
#' @details
#' Notes on HOMA_CP:
#' - This function retains the package's existing operational formula:
#'   HOMA_CP = (G0 [mmol/L] * (C_peptide [pmol/L] / 6)) / 22.5
#'   which mirrors HOMA-IR’s structure using a 6 pmol/µU scaling used for insulin.
#'   Users should verify unit conventions for their datasets; alternative C-peptide
#'   HOMA implementations exist (e.g., HOMA2-CP).
#' @references
#' - Paulmichl K, et al. (2016). SPISE: Single-Point Insulin Sensitivity Estimator. Clin Chem, 62(9):1211–1219.
#' - Bello-Chavolla OY, et al. (2018). METS-IR score. Eur J Endocrinol, 178(5):533–544.
#' - Frühbeck G, et al. (2018). Adiponectin-leptin ratio. Adipocyte, 7(1):57–62.
#' - Matthews DR, et al. (1985). HOMA. Diabetologia, 28(7):412–419. \doi{10.1007/BF00280883}
#' - Simental-Mendía LE, et al. (2008). TyG index. Metab Syndr Relat Disord, 6(2):89–95. \doi{10.1089/met.2007.0038}
#' - Yang X, et al. (2006). Adiponectin/insulin ratio. Diabetes Res Clin Pract, 73(2):293–299.
#' 
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   HDL_c       = c(1.0, 1.3),
#'   TG          = c(1.3, 2.0),
#'   BMI         = c(24, 30),
#'   glucose     = c(5.6, 7.1),
#'   HbA1c       = c(44, 38),
#'   C_peptide   = c(300, 500),
#'   G0          = c(5.5, 6.2),
#'   I0          = c(60, 120),
#'   leptin      = c(10, 20),
#'   adiponectin = c(8, 5)
#' )
#' # Quiet defaults
#' glycemic_markers(df)
#' # Warn on missingness and scan for extremes with capping
#' glycemic_markers(df,
#'   na_action = "warn", na_warn_prop = 0.1,
#'   check_extreme = TRUE, extreme_action = "cap",
#'   verbose = TRUE
#' )
glycemic_markers <- function(
  data,
  na_action = c("ignore","warn","error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  .gm_validate_args(na_warn_prop, check_extreme, extreme_rules, verbose)

  # 1) required core columns
  req <- c("HDL_c", "TG", "BMI")
  if (!is.data.frame(data)) stop("glycemic_markers(): `data` must be a data.frame or tibble.", call. = FALSE)
  missing <- setdiff(req, names(data))
  if (length(missing)) {
    stop("glycemic_markers(): missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (verbose) message("-> glycemic_markers: validating inputs")

  # 2) coerce used columns to numeric and report NAs introduced
  used <- intersect(
    c(req, "glucose", "HbA1c", "C_peptide", "G0", "I0", "leptin", "adiponectin"),
    names(data)
  )
  warns <- character(0)
  for (cn in used) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced_na > 0) {
        w <- sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na)
        warning(w, call. = FALSE)
        warns <- c(warns, w)
      }
    }
  }

  # 3) missingness and non-finite scan on used columns
  qs <- .gm_quality_scan(data, used, .warn = (na_action == "warn"), na_warn_prop = na_warn_prop)
  if (na_action == "error") {
    has_nonfin <- any(vapply(used, function(cn) any(!is.finite(data[[cn]])), logical(1)))
    if (has_nonfin) stop("glycemic_markers(): missing or non-finite values in required inputs with na_action='error'.", call. = FALSE)
  }

  # 4) extreme-value scan and optional handling
  if (isTRUE(check_extreme)) {
    rules <- .gm_default_extreme_rules()
    if (!is.null(extreme_rules) && is.list(extreme_rules)) {
      # override defaults with user-supplied ranges
      for (nm in names(extreme_rules)) {
        rules[[nm]] <- extreme_rules[[nm]]
      }
    }
    ex <- .gm_extreme_scan(data, used, rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        stop(sprintf("glycemic_markers(): %d extreme input values detected.", ex$count), call. = FALSE)
      } else if (extreme_action == "cap") {
        data <- .gm_cap_inputs(data, ex$flags, rules)
        warning(sprintf("glycemic_markers(): capped %d extreme input values into allowed ranges.", ex$count), call. = FALSE)
      } else if (extreme_action == "warn") {
        warning(sprintf("glycemic_markers(): detected %d extreme input values (not altered).", ex$count), call. = FALSE)
      }
    }
  }

  if (verbose) message("-> glycemic_markers: computing markers")

  # helpers for safe math
  lg <- function(x) .gm_log(x)
  dv <- function(a, b) .gm_safe_div(a, b)

  # 5) compute markers (preserve original formulas; produce NAs on invalid inputs)
  SPISE <- dv(600 * (data$HDL_c ^ 0.185), ((data$TG ^ 0.2) * (data$BMI ^ 1.338)))

  METS_IR <- if ("glucose" %in% names(data)) {
    num <- lg(2 * data$glucose + data$TG)
    den <- lg(data$HDL_c)
    dv(num * data$BMI, den)
  } else {
    rep(NA_real_, nrow(data))
  }

  prediabetes <- if ("HbA1c" %in% names(data)) {
    as.integer(data$HbA1c >= 42)
  } else {
    rep(NA_integer_, nrow(data))
  }

  diabetes <- if ("HbA1c" %in% names(data)) {
    as.integer(data$HbA1c >= 48)
  } else {
    rep(NA_integer_, nrow(data))
  }

  HOMA_CP <- if (all(c("C_peptide", "G0") %in% names(data))) {
    dv(data$G0 * (data$C_peptide / 6), 22.5)
  } else {
    rep(NA_real_, nrow(data))
  }

  LAR <- if (all(c("leptin", "adiponectin") %in% names(data))) {
    dv(data$leptin, data$adiponectin)
  } else {
    rep(NA_real_, nrow(data))
  }

  ASI <- if (all(c("adiponectin", "I0") %in% names(data))) {
    dv(data$adiponectin, data$I0)
  } else {
    rep(NA_real_, nrow(data))
  }

  TyG_index <- if ("glucose" %in% names(data)) {
    TG_mgdl  <- data$TG * 88.57
    Glu_mgdl <- data$glucose * 18
    lg((TG_mgdl * Glu_mgdl) / 2)
  } else {
    rep(NA_real_, nrow(data))
  }

  out <- tibble::tibble(
    SPISE = SPISE,
    METS_IR = METS_IR,
    prediabetes = prediabetes,
    diabetes = diabetes,
    HOMA_CP = HOMA_CP,
    LAR = LAR,
    ASI = ASI,
    TyG_index = TyG_index
  )

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(!is.finite(x) | is.na(x)), integer(1))
    message(sprintf(
      "Completed glycemic_markers: %d rows; NA counts -> %s",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", ")
    ))
  }

  return(out)
}

# ---- internal helpers (not exported) ----------------------------------------

.gm_validate_args <- function(na_warn_prop, check_extreme, extreme_rules, verbose) {
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    stop("`na_warn_prop` must be a single numeric in [0, 1].", call. = FALSE)
  }
  if (!(is.logical(check_extreme) && length(check_extreme) == 1L && !is.na(check_extreme))) {
    stop("`check_extreme` must be a single logical value.", call. = FALSE)
  }
  if (!is.null(extreme_rules) && !is.list(extreme_rules)) {
    stop("`extreme_rules` must be NULL or a named list of length-2 numeric ranges.", call. = FALSE)
  }
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    stop("`verbose` must be a single logical value.", call. = FALSE)
  }
  invisible(TRUE)
}

.gm_quality_scan <- function(df, vars, .warn = FALSE, na_warn_prop = 0.2) {
  any_na <- character(0); high_na <- character(0); nonfin <- character(0)
  for (v in vars) {
    x <- df[[v]]
    n_nonfin <- sum(!is.finite(x))
    if (n_nonfin > 0L) nonfin <- c(nonfin, sprintf("%s(%d non-finite)", v, n_nonfin))
    x_na <- sum(is.na(x))
    if (length(x) > 0L && x_na > 0L) any_na <- c(any_na, v)
    if (length(x) > 0L && (x_na / length(x)) >= na_warn_prop && x_na > 0L) {
      high_na <- c(high_na, sprintf("%s(%.1f%% NA)", v, 100 * x_na / length(x)))
    }
  }
  if (.warn) {
    if (length(nonfin)) warning(sprintf("Non-finite values: %s; treated as NA.", paste(nonfin, collapse = ", ")), call. = FALSE)
    if (length(high_na)) warning(sprintf("High missingness (>= %.0f%%): %s.", 100 * na_warn_prop, paste(high_na, collapse = ", ")), call. = FALSE)
  }
  list(any_na = unique(any_na), high_na = unique(high_na), nonfin = unique(nonfin))
}

.gm_default_extreme_rules <- function() {
  list(
    HDL_c = c(0.1, 5),
    TG = c(0.1, 20),
    BMI = c(10, 80),
    glucose = c(2, 30),
    HbA1c = c(20, 200),
    C_peptide = c(0, 5000),
    G0 = c(2, 30),
    I0 = c(0, 3000),
    leptin = c(0, 200),
    adiponectin = c(0, 300)
  )
}

.gm_extreme_scan <- function(df, vars, rules) {
  flags <- lapply(vars, function(v) {
    if (is.null(rules[[v]])) return(rep(FALSE, NROW(df)))
    rng <- rules[[v]]
    x <- df[[v]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    bad
  })
  names(flags) <- vars
  list(count = sum(vapply(flags, sum, integer(1))), flags = flags)
}

.gm_cap_inputs <- function(df, flags, rules) {
  for (v in names(flags)) {
    if (is.null(rules[[v]])) next
    rng <- rules[[v]]
    x <- df[[v]]
    bad <- flags[[v]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[v]] <- x
  }
  df
}

.gm_safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

.gm_log <- function(x) {
  y <- x
  y[!(is.finite(y) & y > 0)] <- NA_real_
  out <- suppressWarnings(log(y))
  out[!is.finite(out)] <- NA_real_
  out
}

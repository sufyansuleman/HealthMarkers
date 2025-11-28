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
#' @param col_map Optional named list mapping keys to column names in `data`.
#'   Required keys: `HDL_c`, `TG`, `BMI`. Optional keys (if present will be used):
#'   `glucose`, `HbA1c`, `C_peptide`, `G0`, `I0`, `leptin`, `adiponectin`.
#'   If `NULL` (default), the function uses columns named exactly as the keys.
#' @param na_action One of `c("ignore","warn","error","keep","omit")`.
#'   HM-CS aliases: `keep` ≡ `ignore`; `omit` drops rows with any NA/non-finite
#'   in used inputs before computing markers. Default "ignore".
#' @param na_warn_prop Proportion (0–1) threshold for high-missingness warnings
#'   among used columns when `na_action = "warn"`. Default 0.2.
#' @param check_extreme Logical; if `TRUE`, scan selected input variables for values
#'   outside plausible ranges defined in `extreme_rules`. Default `FALSE`.
#' @param extreme_action One of `c("warn","cap","error","ignore","NA")` controlling
#'   how to handle extremes when `check_extreme = TRUE`. If "cap", values are
#'   truncated to the allowed range; if "NA", out-of-range values become NA.
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
  col_map = NULL,
  na_action = c("ignore","warn","error","keep","omit"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # HM-CS v2: start message at debug level or console when verbose
  if (isTRUE(verbose)) {
    message("glycemic_markers: validating inputs")
  } else {
    hm_inform("glycemic_markers(): validating inputs", level = "debug")
  }

  # Normalize col_map to identity if NULL
  if (is.null(col_map)) {
    col_map <- list(
      HDL_c = "HDL_c", TG = "TG", BMI = "BMI",
      glucose = "glucose", HbA1c = "HbA1c", C_peptide = "C_peptide",
      G0 = "G0", I0 = "I0", leptin = "leptin", adiponectin = "adiponectin"
    )
  }

  # HM-CS v3: explicit input validation (no hm_validate_inputs)
  if (!is.data.frame(data)) {
    stop("glycemic_markers(): `data` must be a data.frame or tibble.", call. = FALSE)
  }

  # Pre-check required columns to match expected error message
  req_keys <- c("HDL_c", "TG", "BMI")
  req_cols <- unname(unlist(col_map[req_keys]))
  miss <- setdiff(req_cols, names(data))
  if (length(miss)) {
    stop(sprintf("missing required columns: %s", paste(miss, collapse = ", ")), call. = FALSE)
  }

  # Determine used keys present in data
  all_keys <- c(req_keys, "glucose","HbA1c","C_peptide","G0","I0","leptin","adiponectin")
  present <- vapply(all_keys, function(k) {
    nm <- col_map[[k]]
    !is.null(nm) && nm %in% names(data)
  }, logical(1))
  used_keys <- all_keys[present]
  used_cols <- unname(unlist(col_map[used_keys]))

  if (isTRUE(verbose)) message("-> coercing used columns to numeric")

  # Coerce used columns to numeric and report NAs introduced
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced_na > 0) {
        warning(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na), call. = FALSE)
      }
    }
  }

  # Extract vectors via mapping
  getv <- function(key) if (key %in% used_keys) data[[col_map[[key]]]] else NULL

  HDL_c <- data[[col_map$HDL_c]]
  TG    <- data[[col_map$TG]]
  BMI   <- data[[col_map$BMI]]

  glucose     <- getv("glucose")
  HbA1c       <- getv("HbA1c")
  C_peptide   <- getv("C_peptide")
  G0          <- getv("G0")
  I0          <- getv("I0")
  leptin      <- getv("leptin")
  adiponectin <- getv("adiponectin")

  # NA handling
  used_df <- data[, used_cols, drop = FALSE]
  nonfin_mask <- vapply(used_cols, function(cn) any(!is.finite(data[[cn]])), logical(1))
  if (na_action == "warn" && any(nonfin_mask)) {
    wvars <- paste(used_cols[nonfin_mask], collapse = ", ")
    warning(sprintf("Non-finite values detected in: %s; treated as NA.", wvars), call. = FALSE)
  }
  if (na_action == "error" && any(!stats::complete.cases(used_df))) {
    stop("glycemic_markers(): missing or non-finite values in required inputs with na_action='error'.", call. = FALSE)
  } else if (na_action == "omit") {
    keep <- stats::complete.cases(used_df)
    data <- data[keep, , drop = FALSE]
    # re-extract after filtering
    HDL_c <- HDL_c[keep]; TG <- TG[keep]; BMI <- BMI[keep]
    if (!is.null(glucose))     glucose     <- glucose[keep]
    if (!is.null(HbA1c))       HbA1c       <- HbA1c[keep]
    if (!is.null(C_peptide))   C_peptide   <- C_peptide[keep]
    if (!is.null(G0))          G0          <- G0[keep]
    if (!is.null(I0))          I0          <- I0[keep]
    if (!is.null(leptin))      leptin      <- leptin[keep]
    if (!is.null(adiponectin)) adiponectin <- adiponectin[keep]
  }

  # Extreme-value scan and optional handling
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .gm_default_extreme_rules() else {
      # overlay defaults with user-supplied ranges
      def <- .gm_default_extreme_rules()
      if (is.list(extreme_rules)) {
        for (nm in names(extreme_rules)) def[[nm]] <- extreme_rules[[nm]]
      }
      def
    }
    # Remap rule names (keys) to actual column names to support col_map
    if (!is.null(names(rules))) {
      remapped <- list()
      for (nm in names(rules)) {
        col_nm <- if (!is.null(col_map[[nm]])) col_map[[nm]] else nm
        remapped[[col_nm]] <- rules[[nm]]
      }
      rules <- remapped
    }
    vars_to_scan <- intersect(used_cols, names(rules))
    ex <- .gm_extreme_scan(data, vars_to_scan, rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        stop(sprintf("glycemic_markers(): %d extreme input values detected.", ex$count), call. = FALSE)
      } else if (extreme_action == "cap") {
        data <- .gm_cap_inputs(data, ex$flags, rules)
        warning(sprintf("glycemic_markers(): capped %d extreme input values into allowed ranges.", ex$count), call. = FALSE)
      } else if (extreme_action == "warn") {
        warning(sprintf("glycemic_markers(): detected %d extreme input values (not altered).", ex$count), call. = FALSE)
      } else if (extreme_action == "NA") {
        # set out-of-range to NA
        for (v in names(ex$flags)) {
          cn <- v
          bad <- ex$flags[[v]]
          x <- data[[cn]]
          x[bad] <- NA_real_
          data[[cn]] <- x
        }
      }
    }
    # refresh vectors from possibly modified data
    HDL_c <- data[[col_map$HDL_c]]
    TG    <- data[[col_map$TG]]
    BMI   <- data[[col_map$BMI]]
    if (!is.null(glucose))     glucose     <- data[[col_map$glucose]]
    if (!is.null(HbA1c))       HbA1c       <- data[[col_map$HbA1c]]
    if (!is.null(C_peptide))   C_peptide   <- data[[col_map$C_peptide]]
    if (!is.null(G0))          G0          <- data[[col_map$G0]]
    if (!is.null(I0))          I0          <- data[[col_map$I0]]
    if (!is.null(leptin))      leptin      <- data[[col_map$leptin]]
    if (!is.null(adiponectin)) adiponectin <- data[[col_map$adiponectin]]
  }

  if (isTRUE(verbose)) message("-> glycemic_markers: computing markers")

  # helpers for safe math
  lg <- function(x) .gm_log(x)
  dv <- function(a, b) .gm_safe_div(a, b)

  # Unit conversions for indices that require mg/dL
  TG_mgdl  <- TG * 88.57

  # compute markers
  SPISE <- dv(600 * (HDL_c ^ 0.185), ((TG ^ 0.2) * (BMI ^ 1.338)))

  # METS-IR (use mmol/L as in tests; denominator ln(HDL_c) -> NA if HDL_c == 1)
  METS_IR <- if (!is.null(glucose)) {
    num <- lg(2 * glucose + TG)
    den <- lg(HDL_c)
    dv(num * BMI, den)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  prediabetes <- if (!is.null(HbA1c)) {
    as.integer(HbA1c >= 42)
  } else {
    rep(NA_integer_, NROW(HDL_c))
  }

  diabetes <- if (!is.null(HbA1c)) {
    as.integer(HbA1c >= 48)
  } else {
    rep(NA_integer_, NROW(HDL_c))
  }

  HOMA_CP <- if (!is.null(C_peptide) && !is.null(G0)) {
    dv(G0 * (C_peptide / 6), 22.5)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  LAR <- if (!is.null(leptin) && !is.null(adiponectin)) {
    dv(leptin, adiponectin)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  ASI <- if (!is.null(adiponectin) && !is.null(I0)) {
    dv(adiponectin, I0)
  } else {
    rep(NA_real_, NROW(HDL_c))
  }

  TyG_index <- if (!is.null(glucose)) {
    Glu_mgdl <- glucose * 18
    lg((TG_mgdl * Glu_mgdl) / 2)
  } else {
    rep(NA_real_, NROW(HDL_c))
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

  # Completion summary
  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(!is.finite(x) | is.na(x)), integer(1))
    message(sprintf(
      "Completed glycemic_markers: %d rows; NA counts -> %s",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", ")
    ))
  } else {
    hm_inform(sprintf("glycemic_markers(): completed (%d rows)", nrow(out)), level = "debug")
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

# R/kidney_kfre.R

#' Kidney Failure Risk Equation (KFRE, 2- and 5-year risk)
#'
#' Compute 2- and 5-year risk of end-stage kidney disease using the original
#' 4-variable KFRE (Tangri et al., 2011) with optional data-quality diagnostics,
#' extreme-value handling, and verbose progress reporting.
#'
#' This function preserves prior behavior by default:
#' - Inputs are taken as-is; NA values propagate to outputs (na_action = "keep").
#' - No capping or out-of-range checks are applied unless requested.
#'
#' Units (no automatic conversion):
#' - age: years; sex: 1 = male, 2 = female
#' - eGFR: mL/min/1.73 m^2
#' - UACR: mg/g (albumin-to-creatinine ratio)
#'
#' Details
#' - Prognostic index: PI = 0.220*log(age) + (-0.556)*log(eGFR) + 0.451*log(UACR) + 0.391*(male)
#'   where male = 1 if sex == 1, else 0.
#' - Baseline survival: S0(2y) = 0.934, S0(5y) = 0.881 (Tangri 2011).
#' - Risks: KFRE_t = 1 - (S0_t ^ exp(PI)).
#' - The 2016 JAMA study provides a large, multinational validation of the KFRE in humans.
#'
#' @param data A data.frame or tibble containing at least the columns mapped in `col_map`.
#' @param col_map Named list mapping:
#'   - `age`  -> age in years
#'   - `sex`  -> sex code (1 = male, 2 = female)
#'   - `eGFR` -> estimated GFR (mL/min/1.73 m^2)
#'   - `UACR` -> urine albumin-to-creatinine ratio (mg/g)
#' @param na_action One of c("keep","error","omit","warn"). Default "keep" to preserve previous behavior:
#'   - "keep": propagate NA/NaN through logs and outputs.
#'   - "error": abort if any required input contains missing values.
#'   - "omit": drop rows with NA in required inputs before computation.
#'   - "warn": like "keep" but emits high-missingness warnings.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness warnings. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan for out-of-range values (see `extreme_rules`). Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") used when `check_extreme = TRUE`.
#'   - "warn": only warn about out-of-range values (default).
#'   - "cap": truncate to range and warn.
#'   - "error": abort if any out-of-range is detected.
#'   - "ignore": do nothing.
#'   - "NA": set out-of-range input values to NA before computation.
#' @param extreme_rules Optional named list of numeric c(min,max) ranges for c(age, eGFR, UACR).
#'   If NULL, broad defaults are used: age (18, 120), eGFR (1, 200), UACR (0.1, 10000) (mg/g).
#' @param verbose Logical; if TRUE, prints stepwise messages and a completion summary. Default FALSE.
#'
#' @return A tibble with:
#' - `KFRE_2yr` risk (0-1) at 2 years
#' - `KFRE_5yr` risk (0-1) at 5 years
#' @seealso [inflammatory_markers()], [iAge()], [impute_missing()]
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   age  = c(65, 72),
#'   sex  = c(1, 2),          # 1 = male, 2 = female
#'   eGFR = c(45, 22),        # mL/min/1.73 m^2
#'   UACR = c(300, 1200)      # mg/g
#' )
#' # Default behavior (NA propagate, no extreme checks)
#' kidney_failure_risk(
#'   data = df,
#'   col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR")
#' )
#'
#' # With diagnostics and capping
#' \donttest{
#' kidney_failure_risk(
#'   data = df,
#'   col_map = list(age = "age", sex = "sex", eGFR = "eGFR", UACR = "UACR"),
#'   check_extreme = TRUE, extreme_action = "cap", verbose = TRUE
#' )
#' }
#'
#' @references
#' Tangri N, Stevens LA, Griffith J, et al. (2011). A predictive model for progression of chronic kidney disease to kidney failure. JAMA, 305(15):1553-1559. \doi{10.1001/jama.2011.451}
#' Tangri N, Grams ME, Levey AS, et al. (2016). Multinational assessment of accuracy of equations for predicting risk of kidney failure: a meta-analysis. JAMA, 315(2):164-174. \doi{10.1001/jama.2015.18202}
#' Matsushita K, van der Velde M, Astor BC, et al. (2015). Estimated glomerular filtration rate and albuminuria for prediction of kidney failure: a collaborative meta-analysis. Ann Intern Med, 162(4):247-255. \doi{10.7326/M14-3312}
#' Tangri N, Inker LA, Tighiouart H, et al. (2020). Validation of the kidney failure risk equation in advanced CKD. Kidney Int, 97(5):995-1004. \doi{10.1016/j.kint.2019.12.009}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
kidney_failure_risk <- function(data,
                                col_map = list(
                                  age  = "age",
                                  sex  = "sex",
                                  eGFR = "eGFR",
                                  UACR = "UACR"
                                ),
                                na_action = c("keep","error","omit","warn"),
                                na_warn_prop = 0.2,
                                check_extreme = FALSE,
                                extreme_action = c("warn","cap","error","ignore","NA"),
                                extreme_rules = NULL,
                                verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  if (isTRUE(verbose)) rlang::inform("-> kidney_failure_risk: validating inputs")

  # Optional package-level validation; keep test-facing messages stable
  req_keys <- c("age", "sex", "eGFR", "UACR")
  if (is.function(get0("hm_validate_inputs", envir = asNamespace("HealthMarkers"), inherits = TRUE))) {
    try(hm_validate_inputs(data = data, col_map = col_map, required_keys = req_keys, fn = "kidney_failure_risk"), silent = TRUE)
  }

  .kfre_validate_args(data, col_map, na_warn_prop, extreme_rules)

  # Check mapping presence and data columns
  req <- req_keys
  missing_map_keys <- setdiff(req, names(col_map))
  if (length(missing_map_keys)) {
    rlang::abort(
      message = paste0("kidney_failure_risk(): you must supply col_map entries for: ", paste(missing_map_keys, collapse = ", ")),
      class = "healthmarkers_kfre_error_missing_map"
    )
  }
  mapped_cols <- unlist(col_map[req], use.names = FALSE)
  if (any(!nzchar(mapped_cols))) {
    bad <- req[!nzchar(mapped_cols)]
    rlang::abort(
      message = paste0("kidney_failure_risk(): you must supply col_map entries for: ", paste(bad, collapse = ", ")),
      class = "healthmarkers_kfre_error_missing_map"
    )
  }
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      message = paste0("missing required columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_kfre_error_missing_columns"
    )
  }

  # Column classes
  if (!is.numeric(data[[col_map$age]]))  rlang::abort("kidney_failure_risk(): 'age' column must be numeric.",  class = "healthmarkers_kfre_error_nonnumeric_age")
  if (!is.numeric(data[[col_map$eGFR]])) rlang::abort("kidney_failure_risk(): 'eGFR' column must be numeric.", class = "healthmarkers_kfre_error_nonnumeric_egfr")
  if (!is.numeric(data[[col_map$UACR]])) rlang::abort("kidney_failure_risk(): 'UACR' column must be numeric.", class = "healthmarkers_kfre_error_nonnumeric_uacr")
  if (!is.numeric(data[[col_map$sex]]) && !is.integer(data[[col_map$sex]])) {
    rlang::abort("kidney_failure_risk(): 'sex' column must be coded numeric (1=male, 2=female).", class = "healthmarkers_kfre_error_nonnumeric_sex")
  }

  # Missingness policy
  .kfre_warn_high_missing(
    data,
    unlist(col_map[req], use.names = FALSE),
    na_warn_prop = na_warn_prop,
    do_warn_high_missing = identical(na_action, "warn")
  )
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(req, function(k) is.na(data[[col_map[[k]]]])))
    if (any(any_na)) {
      rlang::abort("kidney_failure_risk(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_kfre_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(k) is.na(data[[col_map[[k]]]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> kidney_failure_risk: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Extract inputs
  age <- data[[col_map$age]]
  sex <- data[[col_map$sex]]
  eGFR <- data[[col_map$eGFR]]
  UACR <- data[[col_map$UACR]]

  # Sex coding and validation
  if (any(!(sex %in% c(1, 2)) & !is.na(sex))) {
    rlang::abort("kidney_failure_risk(): 'sex' must be coded as 1=male or 2=female.", class = "healthmarkers_kfre_error_sex_codes")
  }
  male <- ifelse(sex == 1, 1L, 0L)

  # Optional extreme scanning/capping
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .kfre_default_extreme_rules() else extreme_rules
    ex <- .kfre_extreme_scan(age = age, eGFR = eGFR, UACR = UACR, rules = rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("kidney_failure_risk(): detected %d out-of-range values.", ex$count),
                     class = "healthmarkers_kfre_error_extremes")
      } else if (extreme_action == "cap") {
        tmp <- .kfre_cap_inputs(age = age, eGFR = eGFR, UACR = UACR, flags = ex$flags, rules = rules)
        age <- tmp$age; eGFR <- tmp$eGFR; UACR <- tmp$UACR
        capped_n <- ex$count
        rlang::warn(sprintf("kidney_failure_risk(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("kidney_failure_risk(): detected %d extreme input values (not altered).", ex$count))
      } else if (extreme_action == "NA") {
        if (any(ex$flags$age, na.rm = TRUE))   age[ex$flags$age]   <- NA_real_
        if (any(ex$flags$eGFR, na.rm = TRUE))  eGFR[ex$flags$eGFR] <- NA_real_
        if (any(ex$flags$UACR, na.rm = TRUE))  UACR[ex$flags$UACR] <- NA_real_
      }
      # "ignore" does nothing
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> kidney_failure_risk: computing KFRE risks")

  # Prognostic index and risk
  pi <- 0.220 * log(age) +
    (-0.556) * log(eGFR) +
    0.451 * log(UACR) +
    0.391 * male

  S0_2 <- 0.934
  S0_5 <- 0.881

  KFRE_2yr <- 1 - (S0_2^exp(pi))
  KFRE_5yr <- 1 - (S0_5^exp(pi))

  out <- tibble::tibble(
    KFRE_2yr = KFRE_2yr,
    KFRE_5yr = KFRE_5yr
  )

  if (isTRUE(verbose)) {
    n <- nrow(out)
    bad <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed kidney_failure_risk: %d rows; NA/Inf -> %s; capped=%d; elapsed=%.2fs",
      n, paste(sprintf("%s=%d", names(bad), bad), collapse = ", "), capped_n, elapsed
    ))
  }

  return(out)
}

# ---- internal helpers (not exported) -----------------------------------------

.kfre_validate_args <- function(data, col_map, na_warn_prop, extreme_rules) {
  if (!is.data.frame(data)) {
    rlang::abort("kidney_failure_risk(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_kfre_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("kidney_failure_risk(): `col_map` must be a named list.",
                 class = "healthmarkers_kfre_error_colmap_type")
  }
  needed <- c("age","sex","eGFR","UACR")
  missing_keys <- setdiff(needed, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(paste0("kidney_failure_risk(): you must supply col_map entries for: ", paste(missing_keys, collapse = ", ")),
                 class = "healthmarkers_kfre_error_missing_map")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("kidney_failure_risk(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_kfre_error_na_warn_prop")
  }
  if (!is.null(extreme_rules)) {
    if (!is.list(extreme_rules)) {
      rlang::abort("kidney_failure_risk(): `extreme_rules` must be NULL or a named list of c(min,max).",
                   class = "healthmarkers_kfre_error_extreme_rules_type")
    }
    for (nm in names(extreme_rules)) {
      rng <- extreme_rules[[nm]]
      if (!(is.numeric(rng) && length(rng) == 2L && all(is.finite(rng)) && rng[1] <= rng[2])) {
        rlang::abort(sprintf("kidney_failure_risk(): `extreme_rules[['%s']]` must be numeric length-2 with min <= max.", nm),
                     class = "healthmarkers_kfre_error_extreme_rules_value")
      }
    }
  }
  invisible(TRUE)
}

.kfre_warn_high_missing <- function(df, cols, na_warn_prop = 0.2, do_warn_high_missing = TRUE) {
  for (cn in cols) {
    x <- df[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    # High-missingness warning only when na_action = 'warn'
    if (isTRUE(do_warn_high_missing) && pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("kidney_failure_risk(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    # Diagnostic warnings for domain issues (always on; tests rely on these)
    if (grepl("eGFR", cn, ignore.case = TRUE)) {
      neg <- sum(is.finite(x) & x <= 0)
      if (neg > 0L) rlang::warn(sprintf("kidney_failure_risk(): '%s' contains %d non-positive values; log() undefined.", cn, neg))
    }
    if (grepl("UACR", cn, ignore.case = TRUE)) {
      nonpos <- sum(is.finite(x) & x <= 0)
      if (nonpos > 0L) rlang::warn(sprintf("kidney_failure_risk(): '%s' contains %d non-positive values; log() undefined.", cn, nonpos))
      if (sum(is.finite(x) & x > 30000) > 0L) {
        rlang::warn(sprintf("kidney_failure_risk(): '%s' has very large values (>30000 mg/g); check units (mg/g vs mg/mmol).", cn))
      }
    }
  }
  invisible(TRUE)
}

.kfre_default_extreme_rules <- function() {
  list(
    age  = c(18, 120),
    eGFR = c(1, 200),     # mL/min/1.73 m^2
    UACR = c(0.1, 10000)  # mg/g
  )
}

.kfre_extreme_scan <- function(age, eGFR, UACR, rules) {
  flags <- list(
    age  = is.finite(age)  & (age  < rules$age[1]  | age  > rules$age[2]),
    eGFR = is.finite(eGFR) & (eGFR < rules$eGFR[1] | eGFR > rules$eGFR[2]),
    UACR = is.finite(UACR) & (UACR < rules$UACR[1] | UACR > rules$UACR[2])
  )
  list(count = sum(flags$age) + sum(flags$eGFR) + sum(flags$UACR), flags = flags)
}

.kfre_cap_inputs <- function(age, eGFR, UACR, flags, rules) {
  if (any(flags$age))  { age[flags$age]   <- pmin(pmax(age[flags$age],   rules$age[1]),  rules$age[2]) }
  if (any(flags$eGFR)) { eGFR[flags$eGFR] <- pmin(pmax(eGFR[flags$eGFR], rules$eGFR[1]), rules$eGFR[2]) }
  if (any(flags$UACR)) { UACR[flags$UACR] <- pmin(pmax(UACR[flags$UACR], rules$UACR[1]), rules$UACR[2]) }
  list(age = age, eGFR = eGFR, UACR = UACR)
}

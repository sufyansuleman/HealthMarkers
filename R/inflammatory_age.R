# R/inflammatory_age.R

#' Compute a simplified Inflammatory Age Index (iAge) with QA and verbose summaries
#'
#' Implements a linear proxy for immunosenescence based on key inflammatory biomarkers,
#' following the approach introduced by Sayed et al. for the inflammatory aging clock (iAge).
#' This simplified iAge is computed as a weighted sum of C-reactive protein (CRP),
#' interleukin-6 (IL6), and tumor necrosis factor-alpha (TNFa).
#'
#' By default, missing inputs are omitted in the sum (consistent with prior behavior).
#' Optional diagnostics can warn on high missingness and scan for extreme values, with
#' the ability to cap, warn, or error on extremes. Verbose mode prints step-by-step
#' progress and a final summary.
#'
#' Assumed units (no automatic unit conversion):
#' - CRP: mg/L
#' - IL6: pg/mL
#' - TNFa: pg/mL
#'
#' Note:
#' - The original iAge model in Sayed et al. (Nature Aging, 2021) is a multi-marker
#'   machine learning model. This function provides a simple, linear proxy using
#'   three canonical inflammatory biomarkers. It is not identical to the original
#'   published iAge but is inspired by its rationale.
#'
#' @param data A data.frame or tibble containing the biomarker columns mapped by `col_map`.
#' @param col_map Named list mapping:
#'   - CRP  -> column name for C-reactive protein (mg/L)
#'   - IL6  -> column name for interleukin-6 (pg/mL)
#'   - TNFa -> column name for tumor necrosis factor-alpha (pg/mL)
#' @param weights Named numeric vector of weights for each marker (must sum to 1).
#'   Defaults to c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34).
#' @param verbose Logical; if TRUE, prints stepwise progress and a completion summary. Default FALSE.
#' @param na_action One of c("omit","keep","error") controlling how missing inputs affect iAge:
#'   - "omit": ignore NAs in the sum (default; preserves previous behavior).
#'   - "keep": return NA for rows where any required marker is NA.
#'   - "error": abort if any required marker contains NA.
#' @param na_warn_prop Numeric in [0,1]; per-variable threshold for high missingness warnings. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for values outside plausible ranges. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") controlling what to do when extremes are detected.
#'   - "warn": issue a warning, do not modify values (default if check_extreme = TRUE).
#'   - "cap": truncate values to the allowed range and warn.
#'   - "error": abort on detection.
#'   - "ignore": do nothing.
#' @param extreme_rules Optional named list of numeric ranges c(min, max) for CRP, IL6, TNFa. If NULL, broad defaults are used.
#'
#' @return A tibble with one column:
#'   - iAge (numeric): the computed inflammatory age index.
#'
#' @details
#' Default extreme ranges (if `extreme_rules = NULL`):
#' - CRP: 0 to 300 mg/L
#' - IL6: 0 to 1,000 pg/mL
#' - TNFa: 0 to 1,000 pg/mL
#'
#' These are deliberately broad. Adjust `extreme_rules` to fit your cohort.
#'
#' @seealso [impute_missing()], [glycemic_markers()]
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   CRP  = c(1.2, 3.5, NA),  # mg/L
#'   IL6  = c(2.0, 4.1, 1.5), # pg/mL
#'   TNFa = c(1.0, 1.8, 0.9)  # pg/mL
#' )
#' # Default behavior (omit NAs in row-wise sum)
#' iAge(
#'   df,
#'   col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa")
#' )
#'
#' # Keep NA if any marker missing in a row
#' iAge(
#'   df,
#'   col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"),
#'   na_action = "keep"
#' )
#'
#' # Scan and cap extreme values
#' iAge(
#'   df,
#'   col_map = list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa"),
#'   check_extreme = TRUE, extreme_action = "cap", verbose = TRUE
#' )
#'
#' @references
#' - Sayed N et al, An inflammatory aging clock (iAge) predicts multimorbidity, immunosenescence, frailty and cardiovascular aging, Nat Aging, 2021;1:598–610.
#' - Sayed N et al, Multi-omic immune aging models quantify inflammatory dysregulation across adult lifespan, Sci Transl Med, 2023;15(722):eabo4163.
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
iAge <- function(data,
                 col_map,
                 weights = c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34),
                 verbose = FALSE,
                 na_action = c("omit", "keep", "error"),
                 na_warn_prop = 0.2,
                 check_extreme = FALSE,
                 extreme_action = c("warn","cap","error","ignore"),
                 extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  markers <- names(weights)

  # Validate
  .ia_validate_inputs(
    data = data,
    col_map = col_map,
    weights = weights,
    na_warn_prop = na_warn_prop,
    extreme_rules = extreme_rules,
    markers = markers
  )

  if (isTRUE(verbose)) rlang::inform("-> iAge: validating and preparing inputs")

  # Coerce mapped columns to numeric (do not alter values; error if non-numeric)
  for (m in markers) {
    cn <- col_map[[m]]
    if (!(cn %in% names(data))) {
      rlang::abort(
        message = sprintf("iAge(): column '%s' (for %s) not found in data.", cn, m),
        class = "healthmarkers_iage_error_missing_column"
      )
    }
    if (!is.numeric(data[[cn]])) {
      rlang::abort(
        message = sprintf("iAge(): column '%s' (for %s) must be numeric.", cn, m),
        class = "healthmarkers_iage_error_nonnumeric"
      )
    }
  }

  used_cols <- unname(vapply(markers, function(m) col_map[[m]], character(1)))
  # Missingness diagnostics
  .ia_quality_scan(data, used_cols, na_warn_prop = na_warn_prop)

  # If na_action == "error", fail fast on any NA in required inputs
  if (na_action == "error") {
    any_na <- any(vapply(used_cols, function(cn) anyNA(data[[cn]]), logical(1)))
    if (any_na) {
      rlang::abort(
        message = "iAge(): missing values detected in required inputs with na_action='error'.",
        class = "healthmarkers_iage_error_na_inputs"
      )
    }
  }

  # Extreme-value handling
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- extreme_rules %||% .ia_default_extreme_rules()
    ex <- .ia_extreme_scan(data, used_cols, rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(
          message = sprintf("iAge(): detected %d extreme input values.", ex$count),
          class = "healthmarkers_iage_error_extremes"
        )
      } else if (extreme_action == "cap") {
        data <- .ia_cap_inputs(data, ex$flags, rules)
        capped_n <- ex$count
        rlang::warn(sprintf("iAge(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("iAge(): detected %d extreme input values (not altered).", ex$count))
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> iAge: computing weighted sum")

  # Compute iAge with selected NA strategy (default: omit -> previous behavior)
  res <- .ia_compute_iage(data, markers, col_map, weights, na_action = na_action)

  out <- tibble::tibble(iAge = res)

  if (isTRUE(verbose)) {
    n <- nrow(out)
    n_na_rows <- sum(!is.finite(res) | is.na(res))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed iAge: %d rows; iAge NA rows=%d; capped=%d; elapsed=%.2fs",
      n, n_na_rows, capped_n, elapsed
    ))
  }

  return(out)
}

# ---- internal helpers --------------------------------------------------------

.ia_validate_inputs <- function(data, col_map, weights, na_warn_prop, extreme_rules, markers) {
  if (!is.data.frame(data)) {
    rlang::abort("iAge(): `data` must be a data.frame or tibble.", class = "healthmarkers_iage_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("iAge(): `col_map` must be a named list of column mappings.", class = "healthmarkers_iage_error_colmap_type")
  }
  if (!is.numeric(weights) || any(!is.finite(weights)) || any(weights < 0)) {
    rlang::abort("iAge(): `weights` must be non-negative finite numeric values.", class = "healthmarkers_iage_error_weights_type")
  }
  if (is.null(names(weights)) || length(weights) == 0L) {
    rlang::abort("iAge(): `weights` must be a named numeric vector.", class = "healthmarkers_iage_error_weights_names")
  }
  if (!all(markers %in% names(col_map))) {
    miss <- setdiff(markers, names(col_map))
    rlang::abort(sprintf("iAge(): missing `col_map` entries for: %s", paste(miss, collapse = ", ")),
                 class = "healthmarkers_iage_error_missing_map")
  }
  # weights must align to markers and sum to 1
  if (!all(markers %in% names(weights))) {
    rlang::abort("iAge(): `weights` must be named for CRP, IL6, TNFa.", class = "healthmarkers_iage_error_weights_markers")
  }
  s <- sum(weights)
  if (!is.finite(s) || abs(s - 1) > 1e-6) {
    rlang::abort("iAge(): `weights` must sum to 1.", class = "healthmarkers_iage_error_weights_sum")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("iAge(): `na_warn_prop` must be a single numeric in [0, 1].",
                 class = "healthmarkers_iage_error_na_warn_prop")
  }
  if (!is.null(extreme_rules)) {
    if (!is.list(extreme_rules)) {
      rlang::abort("iAge(): `extreme_rules` must be NULL or a named list of c(min,max) numeric ranges.",
                   class = "healthmarkers_iage_error_extreme_rules_type")
    }
    # Quick sanity: if provided, ranges should be length-2 finite numerics
    for (nm in names(extreme_rules)) {
      rng <- extreme_rules[[nm]]
      if (!(is.numeric(rng) && length(rng) == 2L && all(is.finite(rng)))) {
        rlang::abort(sprintf("iAge(): `extreme_rules[['%s']]` must be numeric length-2 (finite).", nm),
                     class = "healthmarkers_iage_error_extreme_rules_value")
      }
      if (rng[1] > rng[2]) {
        rlang::abort(sprintf("iAge(): `extreme_rules[['%s']]` min > max.", nm),
                     class = "healthmarkers_iage_error_extreme_rules_order")
      }
    }
  }
  invisible(TRUE)
}

.ia_quality_scan <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("iAge(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    # Basic domain check for negatives (biomarkers expected >= 0)
    neg_n <- sum(is.finite(x) & x < 0)
    if (neg_n > 0L) {
      rlang::warn(sprintf("iAge(): column '%s' contains %d negative values; check units.", cn, neg_n))
    }
  }
  invisible(TRUE)
}

.ia_default_extreme_rules <- function() {
  list(
    CRP  = c(0, 300),   # mg/L
    IL6  = c(0, 1000),  # pg/mL
    TNFa = c(0, 1000)   # pg/mL
  )
}

.ia_extreme_scan <- function(df, cols, rules) {
  flags <- list()
  total <- 0L
  for (cn in cols) {
    nm <- names(cols)[which(cols == cn)]
    # when cols is a character vector, derive name by reverse-lookup from rules
    nm <- names(rules)[match(cn, cols)]
    rng <- rules[[nm]]
    x <- df[[cn]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    total <- total + sum(bad)
  }
  list(count = total, flags = flags)
}

.ia_cap_inputs <- function(df, flags, rules) {
  for (cn in names(flags)) {
    bad <- flags[[cn]]
    nm <- names(rules)[1]
    # find rule by matching column name to rule key when possible
    for (k in names(rules)) {
      if (grepl(k, cn, ignore.case = TRUE)) { nm <- k; break }
    }
    rng <- rules[[nm]]
    x <- df[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

.ia_compute_iage <- function(data, markers, col_map, weights, na_action = c("omit","keep","error")) {
  na_action <- match.arg(na_action)
  # Build a proper matrix of weighted values (nrow(data) x length(markers))
  mat <- do.call(
    cbind,
    lapply(markers, function(m) weights[[m]] * data[[col_map[[m]]]])
  )
  mat <- as.matrix(mat)

  # NA strategy
  if (na_action == "omit") {
    # Prior behavior: omit missing contributions in row sum (na.rm = TRUE)
    out <- rowSums(mat, na.rm = TRUE)
  } else if (na_action == "keep") {
    # If any NA in a row, return NA
    any_na <- apply(mat, 1L, function(r) any(is.na(r)))
    out <- rowSums(mat, na.rm = FALSE)
    out[any_na] <- NA_real_
  } else {
    # "error" handled upstream; compute without NA
    out <- rowSums(mat, na.rm = FALSE)
  }
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

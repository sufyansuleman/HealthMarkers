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
#' @param na_action One of c("omit","keep","error","ignore","warn") controlling how missing inputs affect iAge:
#'   - "omit": ignore NAs in the weighted sum (default; preserves previous behavior).
#'   - "keep": return NA for rows where any required marker is NA.
#'   - "error": abort if any required marker contains NA.
#'   - "ignore": alias of "omit".
#'   - "warn": alias of "omit" but emits missingness warnings (per na_warn_prop).
#' @param check_extreme Logical; if TRUE, scan inputs for values outside plausible ranges. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") controlling what to do when extremes are detected.
#'   - "warn": issue a warning, do not modify values (default if check_extreme = TRUE).
#'   - "cap": truncate values to the allowed range and warn.
#'   - "error": abort on detection.
#'   - "ignore": do nothing.
#'   - "NA": set out-of-range values to NA.
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
#' - Sayed N, et al. (2021). An inflammatory aging clock (iAge) predicts multimorbidity, immunosenescence, frailty and cardiovascular aging. Nat Aging, 1:598–610. \doi{10.1038/s43587-021-00136-0}
#' - Sayed N, et al. (2023). Multi-omic immune aging models quantify inflammatory dysregulation across adult lifespan. Sci Transl Med, 15(722):eabo4163. \doi{10.1126/scitranslmed.abo4163}
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
iAge <- function(data,
                 col_map,
                 weights = c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34),
                 verbose = FALSE,
                 na_action = c("omit", "keep", "error", "ignore", "warn"),
                 na_warn_prop = 0.2,
                 check_extreme = FALSE,
                 extreme_action = c("warn","cap","error","ignore","NA"),
                 extreme_rules = NULL) {
  na_action_raw <- match.arg(na_action)
  na_action <- if (na_action_raw %in% c("ignore","warn")) "omit" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  if (!is.data.frame(data)) rlang::abort("iAge(): `data` must be a data.frame or tibble.")
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> iAge: starting")

  markers <- c("CRP","IL6","TNFa")
  if (is.null(col_map) || !is.list(col_map)) rlang::abort("iAge(): `col_map` must be a named list.")
  missing_keys <- setdiff(markers, names(col_map))
  if (length(missing_keys)) rlang::abort(paste0("missing required columns: ", paste(missing_keys, collapse = ", ")))

  used_cols <- vapply(markers, function(m) col_map[[m]], character(1))
  if (any(!nzchar(used_cols))) {
    bad <- markers[!nzchar(used_cols)]
    rlang::abort(paste0("missing required columns: ", paste(bad, collapse = ", ")))
  }
  missing_cols <- setdiff(unname(used_cols), names(data))
  if (length(missing_cols)) {
    rlang::abort(sprintf("iAge(): column '%s' not found in data.", missing_cols[1]))
  }
  used_cols_named <- stats::setNames(unname(used_cols), markers)

  # Validate weights
  if (is.null(weights)) rlang::abort("iAge(): `weights` must be a numeric vector.")
  if (!is.numeric(weights)) rlang::abort("iAge(): `weights` must be numeric.")
  if (length(weights) != length(markers)) rlang::abort("iAge(): `weights` must have length 3.")
  if (!is.null(names(weights))) {
    if (all(markers %in% names(weights))) {
      weights <- weights[markers]
    }
  }
  if (any(!is.finite(weights))) rlang::abort("iAge(): `weights` must be finite numeric values.")
  if (abs(sum(weights) - 1) > 1e-8) rlang::abort("iAge(): `weights` must sum to 1.")

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> iAge: coercing inputs to numeric")
  for (cn in unname(used_cols)) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings({
        new <- as.numeric(old)
      })
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::abort(sprintf("iAge(): mapped column '%s' must be numeric.", cn))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  .ia_quality_scan(data, unname(used_cols), na_warn_prop = na_warn_prop, do_warn = (na_action_raw == "warn"))
  if (na_action == "error") {
    if (!all(stats::complete.cases(data[, unname(used_cols), drop = FALSE]))) {
      rlang::abort("iAge(): missing or non-finite values in required inputs with na_action='error'.")
    }
  }

  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .ia_default_extreme_rules() else {
      def <- .ia_default_extreme_rules()
      for (nm in intersect(names(extreme_rules), names(def))) def[[nm]] <- extreme_rules[[nm]]
      def
    }
    ex <- .ia_extreme_scan(data, used_cols_named, rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("iAge(): detected %d extreme input values.", ex$count))
      } else if (extreme_action == "cap") {
        data <- .ia_cap_inputs(data, ex$flags, used_cols_named, rules)
        rlang::warn(sprintf("iAge(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("iAge(): detected %d extreme input values (not altered).", ex$count))
      } else if (extreme_action == "NA") {
        for (mk in names(ex$flags)) {
          cn <- used_cols_named[[mk]]
          bad <- ex$flags[[mk]]
          xi <- data[[cn]]
          xi[bad] <- NA_real_
          data[[cn]] <- xi
        }
      }
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> iAge: computing weighted sum")

  M <- as.matrix(data[, unname(used_cols), drop = FALSE])
  w <- as.numeric(weights)
  if (na_action == "keep") {
    ok <- stats::complete.cases(M)
    iage <- rep(NA_real_, nrow(M))
    if (any(ok)) {
      M_ok <- M[ok, , drop = FALSE]
      M_ok[is.na(M_ok)] <- 0
      iage[ok] <- as.numeric(M_ok %*% w)
    }
  } else {
    M[is.na(M)] <- 0
    iage <- as.numeric(M %*% w)
  }

  out <- tibble::tibble(iAge = iage)
  if (isTRUE(verbose)) {
    na_n <- sum(is.na(out$iAge))
    hm_inform(level = "inform", msg = sprintf("Completed iAge: %d rows; NA(iAge)=%d", nrow(out), na_n))
  }
  out
}

.ia_quality_scan <- function(df, cols, na_warn_prop = 0.2, do_warn = TRUE) {
  for (cn in cols) {
    x <- df[[cn]]
    if (!length(x)) next
    pna <- mean(is.na(x))
    if (do_warn && is.finite(pna) && pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("iAge(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    neg_n <- sum(is.finite(x) & x < 0)
    if (do_warn && neg_n > 0L) {
      rlang::warn(sprintf("iAge(): column '%s' contains %d negative values; check units.", cn, neg_n))
    }
  }
  invisible(TRUE)
}

.ia_default_extreme_rules <- function() {
  list(
    CRP = c(0, 300),
    IL6 = c(0, 1000),
    TNFa = c(0, 2000)
  )
}

.ia_extreme_scan <- function(df, used_cols_named, rules) {
  flags <- list(); total <- 0L
  for (mk in names(used_cols_named)) {
    cn <- used_cols_named[[mk]]
    if (!mk %in% names(rules)) next
    rng <- rules[[mk]]
    x <- df[[cn]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[mk]] <- bad
    total <- total + sum(bad)
  }
  list(count = total, flags = flags)
}

.ia_cap_inputs <- function(df, flags, used_cols_named, rules) {
  for (mk in names(flags)) {
    cn <- used_cols_named[[mk]]
    if (!mk %in% names(rules)) next
    bad <- flags[[mk]]
    rng <- rules[[mk]]
    x <- df[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

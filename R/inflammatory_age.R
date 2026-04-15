
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
#' @param verbose Logical; if TRUE, prints column mapping and computing messages.
#' @param na_action One of c("keep","omit","error","ignore","warn") controlling how missing inputs affect iAge:
#'   - "keep": return NA for rows where any required marker is NA (default; consistent with other functions).
#'   - "omit": ignore NAs in the weighted sum (treat missing markers as 0).
#'   - "error": abort if any required marker contains NA.
#'   - "ignore": alias of "omit".
#'   - "warn": alias of "omit" but emits missingness warnings (per na_warn_prop).
#' @param na_warn_prop Proportion in \eqn{[0,1]} above which a high-missingness warning
#'   is emitted when `na_action = "warn"`. Default 0.2.
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
#' @references \insertRef{sayed2021iage}{HealthMarkers}
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
iAge <- function(data,
                 col_map = NULL,
                 weights = c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34),
                 verbose = TRUE,
                 na_action = c("keep", "omit", "error", "ignore", "warn"),
                 na_warn_prop = 0.2) {
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "iAge"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col <- .hm_detect_id_col(data)
  na_action_raw <- match.arg(na_action)
  na_action <- if (na_action_raw %in% c("ignore","warn")) "omit" else na_action_raw

  if (!is.data.frame(data)) rlang::abort("iAge(): `data` must be a data.frame or tibble.")

  markers <- c("CRP","IL6","TNFa")
  cm_iage <- .hm_build_col_map(data, col_map, markers, fn = "iAge")
  data    <- cm_iage$data
  col_map <- cm_iage$col_map
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

  .hm_log_cols(cm_iage, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  iAge  [weighted sum: CRP*%.2f + IL6*%.2f + TNFa*%.2f]",
      fn_name, weights[1], weights[2], weights[3]), level = "inform")

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
  if (!is.null(id_col)) {
    out[[id_col]] <- data[[id_col]]
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) { hm_inform(hm_result_summary(out, fn_name), level = "inform") }
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


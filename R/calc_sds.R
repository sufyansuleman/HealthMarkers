#' Calculate Standard Deviation Scores (SDS; z-scores) for health markers
#'
#' Computes per-variable SDS as (x - mean) / sd using supplied reference statistics.
#' Includes input validation, NA/error handling, data quality warnings, and
#' verbose progress with a completion summary.
#'
#' By default, returns a tibble with added <var>_sds columns (tidyverse-friendly).
#' For backward compatibility, you can request the previous list output.
#'
#' @param data A data.frame/tibble containing the variables.
#' @param vars Character vector of variable names in `data` to compute SDS for.
#'   Must be non-empty and unique.
#' @param ref A data.frame with columns: `variable`, `mean`, `sd` supplying reference
#'   statistics for each variable listed in `vars`. Must contain exactly one row per
#'   requested variable, with finite `mean` and `sd > 0`.
#' @param id_col Optional character scalar naming an ID column in `data` (used only
#'   in messages and duplicate-ID notes).
#' @param sds_cap Numeric scalar; absolute cap for SDS when `extreme_strategy = "cap"`.
#'   Must be positive. Default 6.
#' @param na_strategy One of c("omit","error","keep"):
#'   - "omit": drop rows with missing values across any of `vars` (default)
#'   - "error": stop if any missing values among `vars`
#'   - "keep": keep rows; SDS for missing inputs will be NA
#' @param extreme_strategy One of c("cap","warn","error"):
#'   - "cap": cap |SDS| at `sds_cap` and warn (default)
#'   - "warn": keep extreme SDS, but warn
#'   - "error": stop if any |SDS| > `sds_cap`
#' @param warn_thresholds Named list controlling warnings (proportions in [0,1]):
#'   - na_prop: warn if proportion of rows with NA among `vars` exceeds this (default 0.05)
#'   - extreme_prop: warn if proportion of extreme SDS (cells) exceeds this (default 0.01)
#' @param return One of c("data","list"). "data" returns a tibble with added
#'   `<var>_sds` columns (default). "list" returns a list with components
#'   `data`, `summary`, and `warnings` (backward compatible).
#' @param verbose Logical; if TRUE, print progress and a completion summary.
#'   Default FALSE (CRAN-friendly).
#'
#' @return
#' - If `return = "data"` (default): a tibble with added `<var>_sds` columns.
#' - If `return = "list"`: a list with:
#'   - data: tibble with added SDS columns
#'   - summary: list with input/output row counts, omitted rows, total extremes, and per-variable summary
#'   - warnings: character vector of warning messages emitted
#'
#' @examples
#' ref <- data.frame(
#'   variable = c("bmi","sbp"),
#'   mean     = c(25, 120),
#'   sd       = c(4, 15)
#' )
#' df <- data.frame(
#'   id  = 1:6,
#'   bmi = c(24, 30, NA, 29, 10, 26),
#'   sbp = c(118, 200, 119, 121, 500, 120)
#' )
#' # Tidyverse-friendly return (default): tibble with <var>_sds columns
#' out_tbl <- calc_sds(
#'   data = df,
#'   vars = c("bmi","sbp"),
#'   ref = ref,
#'   id_col = "id",
#'   na_strategy = "omit",
#'   extreme_strategy = "cap",
#'   sds_cap = 6,
#'   verbose = TRUE
#' )
#'
#' # Backward-compatible list return
#' out_list <- calc_sds(
#'   data = df,
#'   vars = c("bmi","sbp"),
#'   ref = ref,
#'   id_col = "id",
#'   na_strategy = "keep",
#'   extreme_strategy = "warn",
#'   sds_cap = 6,
#'   return = "list",
#'   verbose = TRUE
#' )
#' str(out_list$summary)
#'
#' @references
#' de Onis, M., et al. (2006). WHO Child Growth Standards: Methods and development. World Health Organization.
#' Cole, T. J., & Green, P. J. (1992). Smoothing reference centile curves: the LMS method and penalized likelihood. Stat Med, 11(10), 1305–1319. \doi{10.1002/sim.4780111005}
#' Kuczmarski, R. J., et al. (2000). CDC growth charts: United States. Adv Data, (314), 1–27. (Primary reference standards using z-scores.)
#'
#' @importFrom tibble as_tibble
#' @export
calc_sds <- function(
  data,
  vars,
  ref,
  id_col = NULL,
  sds_cap = 6,
  na_strategy = c("omit","error","keep"),
  extreme_strategy = c("cap","warn","error","NA"),  # allow HM-CS 'NA'
  warn_thresholds = list(na_prop = 0.05, extreme_prop = 0.01),
  return = c("data","list"),
  verbose = FALSE,
  # HM-CS v1 additions (aliases; preferred)
  na_action = NULL,
  check_extreme = TRUE,
  extreme_action = NULL
) {
  # HM-CS v1: map new args -> legacy slots (single combined deprecation if mixed)
  legacy_msgs <- character()
  if (!is.null(na_action)) {
    na_strategy <- match.arg(na_action, c("keep","omit","error"))
    legacy_msgs <- c(legacy_msgs, "use 'na_action' (alias of 'na_strategy').")
  }
  if (!is.null(extreme_action)) {
    # Accept HM-CS values; map deprecated 'warn' if provided via legacy slot
    extreme_strategy <- match.arg(extreme_action, c("cap","NA","error"))
    legacy_msgs <- c(legacy_msgs, "use 'extreme_action' (alias of 'extreme_strategy').")
  }
  if (length(legacy_msgs)) {
    # was: rlang::inform(...)
    hm_inform(
      paste0("calc_sds(): HM-CS v1: ", paste(unique(legacy_msgs), collapse = " ")),
      level = "debug"  # only emits when options(healthmarkers.verbose = "debug")
    )
  }

  na_strategy <- match.arg(na_strategy)
  extreme_strategy <- match.arg(extreme_strategy)  # now includes "NA" via default choices
  return <- match.arg(return)

  # --- Input validation ---
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  if (!is.character(vars) || length(vars) == 0) stop("`vars` must be a non-empty character vector.")
  if (anyNA(vars) || any(vars == "")) stop("`vars` must not contain NA/empty names.")
  if (anyDuplicated(vars)) stop("`vars` must be unique (no duplicates).")
  .ensure_cols_exist(data, vars, label = "`data`")
  if (!is.null(id_col)) .ensure_cols_exist(data, id_col, label = "`data` (id_col)")

  ref_ok <- .validate_ref(ref, vars)
  if (!ref_ok$ok) stop(ref_ok$msg)

  if (!is.numeric(sds_cap) || length(sds_cap) != 1 || !is.finite(sds_cap) || sds_cap <= 0) {
    stop("`sds_cap` must be a positive finite number.")
  }
  if (!is.list(warn_thresholds)) {
    stop("`warn_thresholds` must be a list with elements `na_prop` and `extreme_prop`.")
  }
  na_prop <- warn_thresholds$na_prop %||% 0.05
  extreme_prop <- warn_thresholds$extreme_prop %||% 0.01
  if (!(is.numeric(na_prop) && length(na_prop) == 1 && is.finite(na_prop) && na_prop >= 0 && na_prop <= 1)) {
    stop("`warn_thresholds$na_prop` must be a single numeric in [0, 1].")
  }
  if (!(is.numeric(extreme_prop) && length(extreme_prop) == 1 && is.finite(extreme_prop) && extreme_prop >= 0 && extreme_prop <= 1)) {
    stop("`warn_thresholds$extreme_prop` must be a single numeric in [0, 1].")
  }
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    stop("`verbose` must be a single logical value.")
  }

  # --- Start messages ---
  if (verbose) {
    total_rows <- nrow(data)
    msg_id <- if (is.null(id_col)) "" else paste0(" (id: ", id_col, ")")
    message(sprintf("calc_sds: starting on %d row(s), %d variable(s)%s", total_rows, length(vars), msg_id))
  }

  warns <- character(0)
  out <- data

  # --- Coerce non-numeric vars with warning ---
  for (v in vars) {
    if (!is.numeric(out[[v]])) {
      old <- out[[v]]
      suppressWarnings(out[[v]] <- as.numeric(out[[v]]))
      introduced_na <- sum(is.na(out[[v]]) & !is.na(old))
      w <- sprintf("Variable `%s` was coerced to numeric; NAs introduced: %d", v, introduced_na)
      warning(w)
      warns <- c(warns, w)
      if (verbose) message(sprintf("- coerced `%s` to numeric", v))
    }
  }

  # --- NA handling strategy ---
  row_has_na <- rowSums(is.na(out[, vars, drop = FALSE])) > 0
  n_rows_in <- nrow(out)
  n_rows_na <- sum(row_has_na)
  prop_na_rows <- if (n_rows_in > 0) n_rows_na / n_rows_in else 0

  if (prop_na_rows > na_prop) {
    w <- sprintf("High NA proportion among `vars`: %.1f%% of rows", 100 * prop_na_rows)
    warning(w)
    warns <- c(warns, w)
  }

  if (na_strategy == "error" && n_rows_na > 0) {
    stop(sprintf("Missing values found in `vars` for %d row(s); na_strategy='error'", n_rows_na))
  } else if (na_strategy == "omit" && n_rows_na > 0) {
    if (verbose) message(sprintf("- omitting %d row(s) with NA across `vars`", n_rows_na))
    out <- out[!row_has_na, , drop = FALSE]
  } else if (verbose && n_rows_na > 0) {
    message(sprintf("- keeping %d row(s) with NA across `vars`", n_rows_na))
  }

  # Recompute after omission
  n_rows_out <- nrow(out)
  omitted_rows <- n_rows_in - n_rows_out

  # --- Prepare reference lookups ---
  ref_map_mean <- setNames(ref$mean, ref$variable)
  ref_map_sd   <- setNames(ref$sd,   ref$variable)

  # Validate SD > 0 for all vars
  zero_sd_vars <- vars[ref_map_sd[vars] <= 0 | !is.finite(ref_map_sd[vars])]
  if (length(zero_sd_vars)) {
    stop(sprintf("Reference SD must be > 0 and finite for: %s", paste(zero_sd_vars, collapse = ", ")))
  }

  # --- Compute SDS per variable ---
  if (verbose) message(sprintf("Computing SDS for %d variable(s)...", length(vars)))
  per_var_summary <- data.frame(
    variable = vars,
    n_missing = integer(length(vars)),
    n_extreme = integer(length(vars)),
    stringsAsFactors = FALSE
  )

  total_extreme <- 0L

  for (i in seq_along(vars)) {
    v <- vars[i]
    mu <- ref_map_mean[[v]]
    sdv <- ref_map_sd[[v]]

    z <- (out[[v]] - mu) / sdv
    # Count missing (after strategy)
    per_var_summary$n_missing[i] <- sum(is.na(out[[v]]))

    # Gate SDS extreme handling behind HM-CS check_extreme
    if (isTRUE(check_extreme)) {
      is_extreme <- is.finite(z) & abs(z) > sds_cap
      n_extreme <- sum(is_extreme, na.rm = TRUE)
      total_extreme <- total_extreme + n_extreme

      if (n_extreme > 0) {
        if (identical(extreme_strategy, "error")) {
          stop(sprintf("Found %d SDS beyond ±%g for `%s`", n_extreme, sds_cap, v))
        } else if (identical(extreme_strategy, "cap")) {
          z[is_extreme] <- sds_cap * sign(z[is_extreme])
          w <- sprintf("Capped %d SDS beyond ±%g for `%s`", n_extreme, sds_cap, v)
          warning(w)
          warns <- c(warns, w)
        } else if (identical(extreme_strategy, "warn")) {
          # legacy behavior; HM-CS prefers cap/NA/error; keep for back-compat
          w <- sprintf("Detected %d SDS beyond ±%g for `%s` (not capped)", n_extreme, sds_cap, v)
          warning(w)
          warns <- c(warns, w)
        } else if (identical(extreme_strategy, "NA")) {  # HM-CS: blank extremes
          z[is_extreme] <- NA_real_
        }
      }

      per_var_summary$n_extreme[i] <- n_extreme
    }

    out[[paste0(v, "_sds")]] <- z
  }

  # --- Data quality warning for extremes proportion ---
  prop_extreme <- if (n_rows_out > 0) total_extreme / (n_rows_out * length(vars)) else 0
  if (prop_extreme > extreme_prop) {
    w <- sprintf("High proportion of extreme SDS: %.2f%% of computed cells", 100 * prop_extreme)
    warning(w)
    warns <- c(warns, w)
  }

  # --- Verbose completion summary ---
  if (verbose) {
    message("calc_sds: completed")
    message(sprintf("- rows in:    %d", n_rows_in))
    message(sprintf("- rows out:   %d", n_rows_out))
    message(sprintf("- omitted:    %d (na_strategy = '%s')", omitted_rows, na_strategy))
    message(sprintf("- extremes:   %d (strategy = '%s', cap = %s)", total_extreme, extreme_strategy, as.character(sds_cap)))
    if (!is.null(id_col) && n_rows_out > 0 && anyDuplicated(out[[id_col]]) > 0) {
      message("- note: duplicate IDs detected")
    }
  }

  # Ensure tibble for outputs
  out_tbl <- tibble::as_tibble(out)

  # --- Explicit return ---
  if (return == "data") {
    return(out_tbl)
  } else {
    return(list(
      data = out_tbl,
      summary = list(
        rows_in = n_rows_in,
        rows_out = n_rows_out,
        omitted_rows = omitted_rows,
        total_extreme = total_extreme,
        per_var = per_var_summary
      ),
      warnings = unique(warns)
    ))
  }
}

# Internal helpers ----------------------------------------------------------

# Safe null-coalescing for lists
`%||%` <- function(x, y) if (is.null(x)) y else x

.ensure_cols_exist <- function(df, cols, label = "`data`") {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols)) {
    stop(sprintf("Missing column(s) in %s: %s", label, paste(missing_cols, collapse = ", ")))
  }
  invisible(TRUE)
}

.validate_ref <- function(ref, vars) {
  if (!is.data.frame(ref)) {
    return(list(ok = FALSE, msg = "`ref` must be a data.frame with columns: variable, mean, sd"))
  }
  req <- c("variable","mean","sd")
  if (!all(req %in% names(ref))) {
    return(list(ok = FALSE, msg = "`ref` must have columns: variable, mean, sd"))
  }
  # Basic type checks
  if (!is.character(ref$variable)) {
    return(list(ok = FALSE, msg = "`ref$variable` must be character"))
  }
  if (!is.numeric(ref$mean) || !is.numeric(ref$sd)) {
    return(list(ok = FALSE, msg = "`ref$mean` and `ref$sd` must be numeric"))
  }
  # Ensure no duplicate reference rows per variable
  if (anyDuplicated(ref$variable)) {
    return(list(ok = FALSE, msg = "`ref` must have at most one row per variable"))
  }
  # No missing in ref rows for required vars
  missing_vars <- setdiff(vars, ref$variable)
  if (length(missing_vars)) {
    return(list(ok = FALSE, msg = sprintf("`ref` is missing stats for: %s", paste(missing_vars, collapse = ", "))))
  }
  # Subset to needed variables and check finiteness
  rsub <- ref[match(vars, ref$variable), , drop = FALSE]
  if (any(!is.finite(rsub$mean)) || any(!is.finite(rsub$sd))) {
    return(list(ok = FALSE, msg = "`ref` contains non-finite mean or sd"))
  }
  list(ok = TRUE, msg = "")
}

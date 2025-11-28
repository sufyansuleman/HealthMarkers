# R/adiposity_sds.R
#' Calculate standardized scores (SDS) for adiposity measures
#'
#' Computes standard deviation (z) scores for anthropometric variables relative to
#' a single (non–sex-stratified) reference set of means and standard deviations.
#' Includes input validation, optional raw-value extreme screening/capping,
#' configurable handling of extreme SDS values, NA row policies, optional concise
#' summary output, and optional verbose progress messages.
#'
#' SDS are computed as: (observed - mean) / sd. Rows are removed only when
#' `na_action = 'omit'`. Raw-value extreme screening (if enabled) is applied
#' before SDS computation. Extreme SDS handling (cap / warn / error / ignore)
#' is controlled by `extreme_action`. Legacy argument aliases (`na_strategy`,
#' `extreme_strategy`) are soft-deprecated but still accepted.
#'
#' @param data data.frame or tibble containing the measurement columns.
#' @param ref Named list where each element is a numeric vector with names
#'   `mean` and `sd`, e.g. `list(BMI = c(mean = 23, sd = 4))`.
#' @param na_action One of `c("omit","error","keep")` determining how rows with
#'   any missing required values are handled.
#' @param extreme_action One of `c("cap","warn","error","ignore")` controlling
#'   how SDS values exceeding `sds_cap` in absolute value are treated.
#' @param sds_cap Positive numeric; absolute cap used when
#'   `extreme_action = "cap"`. Default 6.
#' @param check_raw_extreme Logical; if TRUE apply raw-value extreme screening
#'   using `raw_extreme_rules` (before SDS computation).
#' @param raw_extreme_rules Optional named list of length-2 numeric vectors
#'   giving c(min, max) for raw variables; if NULL, broad defaults are used
#'   for common anthropometric measures (BMI, waist, weight, height, hip, WC, HC).
#' @param diagnostics Logical; if TRUE emit informational/warning messages
#'   (coercions, missingness, extremes). FALSE suppresses non-critical warnings.
#' @param warn_thresholds Named list with optional elements:
#'   `na_prop` (default 0.05) and `extreme_prop` (default 0.01) used for
#'   proportion-based diagnostic warnings.
#' @param id_col Optional column name used only in verbose summaries.
#' @param verbose Logical; if TRUE print progress and completion summaries.
#' @param return_summary Logical; if TRUE return a list with elements
#'   `data`, `summary`, and `warnings` instead of just the SDS tibble.
#' @param na_strategy Soft-deprecated alias for `na_action`.
#' @param extreme_strategy Soft-deprecated alias for `extreme_action`.
#'
#' @return A tibble with one `<var>_SDS` column per reference variable, or a
#'   list when `return_summary = TRUE`.
#'
#' @examples
#' ref <- list(BMI = c(mean = 23, sd = 4), waist = c(mean = 80, sd = 12))
#' df <- data.frame(BMI = c(25, NA, 60, 18), waist = c(85, 70, 300, 55))
#' adiposity_sds(df, ref)
#'
#' @export
adiposity_sds <- function(
  data,
  col_map = NULL,
  ref,
  na_action = c("keep","omit","error"),
  extreme_action = c("cap","NA","error","warn","ignore"),
  sds_cap = 6,
  check_extreme = FALSE,
  extreme_rules = NULL,
  diagnostics = FALSE,  # quiet by default
  warn_thresholds = list(na_prop = 0.05, extreme_prop = 0.01),
  id_col = NULL,
  return_summary = FALSE,
  verbose = FALSE,
  na_strategy = NULL,
  extreme_strategy = NULL,
  check_raw_extreme = NULL,
  raw_extreme_rules = NULL
) {

  # --- Back-compat: allow adiposity_sds(data, ref, ...) positional calls ---
  looks_like_ref <- function(x) {
    is.list(x) && length(x) > 0 &&
      all(vapply(x, function(y) is.numeric(y) && !is.null(names(y)) &&
                    all(c("mean","sd") %in% names(y)), logical(1)))
  }
  if (missing(ref) && !is.null(col_map) && looks_like_ref(col_map)) {
    ref <- col_map
    col_map <- NULL
  }

  # ---- Legacy alias reconciliation (combine into one warning) ----
  legacy_msgs <- character()

  if (!is.null(na_strategy)) {
    if (!missing(na_action)) {
      legacy_msgs <- c(legacy_msgs, "both na_strategy & na_action supplied; using na_action.")
    } else {
      na_action <- na_strategy
      legacy_msgs <- c(legacy_msgs, "argument 'na_strategy' is deprecated; use 'na_action'.")
    }
  }
  if (!is.null(extreme_strategy)) {
    if (!missing(extreme_action)) {
      legacy_msgs <- c(legacy_msgs, "both extreme_strategy & extreme_action supplied; using extreme_action.")
    } else {
      extreme_action <- extreme_strategy
      legacy_msgs <- c(legacy_msgs, "argument 'extreme_strategy' is deprecated; use 'extreme_action'.")
    }
  }
  if (!is.null(check_raw_extreme)) {
    check_extreme <- isTRUE(check_raw_extreme)
    legacy_msgs <- c(legacy_msgs, "'check_raw_extreme' is deprecated; use 'check_extreme'.")
  }
  if (!is.null(raw_extreme_rules)) {
    extreme_rules <- raw_extreme_rules
    legacy_msgs <- c(legacy_msgs, "'raw_extreme_rules' is deprecated; use 'extreme_rules'.")
  }
  if (length(legacy_msgs)) {
    rlang::warn(paste0("adiposity_sds(): ", paste(legacy_msgs, collapse = " ")))
  }

  na_action <- match.arg(na_action, c("keep","omit","error"))
  extreme_action <- match.arg(extreme_action, c("cap","NA","error","warn","ignore"))
  if (extreme_action %in% c("warn","ignore")) {
    rlang::warn("adiposity_sds(): extreme_action values 'warn' and 'ignore' are deprecated; prefer 'cap', 'NA', or 'error'.")
  }

  t0 <- Sys.time()
  collected_warnings <- character()

  .warn <- function(msg) {
    if (isTRUE(diagnostics)) rlang::warn(msg)
    collected_warnings <<- c(collected_warnings, msg)
  }
  .abort <- function(msg, class) rlang::abort(msg, class = class)
  .inform <- function(msg) {
    # honor package-level verbosity; still respect existing verbose=TRUE
    if (isTRUE(verbose)) rlang::inform(msg)
    hm_inform(msg, level = "inform")
  }

  # ---- Validate inputs & build effective mapping ----
  if (!is.data.frame(data)) {
    .abort("adiposity_sds(): `data` must be a data.frame or tibble.", "healthmarkers_adiposds_error_data_type")
  }
  if (!is.list(ref) || is.null(names(ref)) || any(!nzchar(names(ref)))) {
    .abort("adiposity_sds(): `ref` must be a named list of mean/sd vectors.", "healthmarkers_adiposds_error_ref_type")
  }
  vars <- names(ref)
  # Validate ref components
  for (v in vars) {
    stats <- ref[[v]]
    if (!(is.numeric(stats) && all(c("mean","sd") %in% names(stats)) && length(stats) >= 2)) {
      .abort(sprintf("adiposity_sds(): ref[['%s']] must be numeric with names mean, sd.", v),
             "healthmarkers_adiposds_error_ref_component")
    }
    if (any(!is.finite(stats[c("mean","sd")])) || stats["sd"] <= 0) {
      .abort(sprintf("adiposity_sds(): ref[['%s']] mean/sd must be finite; sd>0.", v),
             "healthmarkers_adiposds_error_ref_values")
    }
  }

  # Effective mapping: default identity; override via col_map$vars
  var_map <- setNames(vars, vars)
  if (!is.null(col_map) && !is.null(col_map$vars)) {
    stopifnot(is.list(col_map$vars))
    var_map <- c(col_map$vars, var_map)[vars]
  }

  # Pre-check for clearer message expected by tests
  missing_cols <- setdiff(unname(unlist(var_map, use.names = FALSE)), names(data))
  if (length(missing_cols)) {
    rlang::abort(
      sprintf("adiposity_sds(): missing required columns: %s", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_adiposds_error_missing_columns"
    )
  }

  # Use centralized validation to check columns exist
  #  hm_validate_inputs(
#    data = data,
#    col_map = as.list(var_map),   # keys are ref vars, values are data columns
#    required_keys = vars,
#    fn = "adiposity_sds"
#  )

  if (!is.null(id_col) && !id_col %in% names(data)) {
    .abort(sprintf("adiposity_sds(): id_col '%s' not found in data.", id_col),
           "healthmarkers_adiposds_error_idcol_missing")
  }
  if (!is.numeric(sds_cap) || length(sds_cap) != 1 || !is.finite(sds_cap) || sds_cap <= 0) {
    .abort("adiposity_sds(): `sds_cap` must be a single positive finite number.",
           "healthmarkers_adiposds_error_sds_cap")
  }
  if (!is.list(warn_thresholds)) {
    .abort("adiposity_sds(): `warn_thresholds` must be a list.", "healthmarkers_adiposds_error_warn_thresholds_type")
  }

  total_rows <- nrow(data)
  .inform(sprintf("-> adiposity_sds: starting (%d rows, %d vars)", total_rows, length(vars)))

  # ---- Copy & numeric coercion ----
  df <- data
  for (v in vars) {
    cn <- var_map[[v]]
    if (!is.numeric(df[[cn]])) {
      old <- df[[cn]]
      suppressWarnings(df[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(df[[cn]]) & !is.na(old))
      if (introduced > 0) .warn(sprintf("adiposity_sds(): '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      else .warn(sprintf("adiposity_sds(): '%s' coerced to numeric.", cn))
    }
  }

  # ---- Missingness diagnostics ----
  if (length(vars)) {
    for (v in vars) {
      cn <- var_map[[v]]
      x <- df[[cn]]
      pna <- mean(is.na(x))
      na_thresh <- rlang::`%||%`(warn_thresholds$na_prop, 0.05)
      if (pna >= na_thresh && pna > 0) {
        .warn(sprintf("adiposity_sds(): '%s' missingness %.1f%%", cn, 100 * pna))
      }
    }
  }

  # ---- NA handling (row-wise across mapped columns) ----
  row_na <- if (length(vars)) rowSums(as.data.frame(lapply(var_map, function(cn) is.na(df[[cn]])))) > 0 else rep(FALSE, nrow(df))
  n_rows_in <- nrow(df)
  n_rows_with_na <- sum(row_na)
  if (na_action == "error" && n_rows_with_na > 0) {
    .abort(sprintf("adiposity_sds(): %d rows have missing values (na_action='error').", n_rows_with_na),
           "healthmarkers_adiposds_error_missing_values")
  } else if (na_action == "omit" && n_rows_with_na > 0) {
    df <- df[!row_na, , drop = FALSE]
  }
  n_rows_out <- nrow(df)
  omitted_rows <- n_rows_in - n_rows_out

  if (n_rows_out == 0L) {
    out_empty <- tibble::as_tibble(setNames(replicate(length(vars), numeric(0), simplify = FALSE),
                                            paste0(vars, "_SDS")))
    if (return_summary) {
      return(list(
        data = out_empty,
        summary = list(
          rows_in = n_rows_in,
          rows_out = 0L,
          omitted_rows = omitted_rows,
          total_extreme = 0L,
          per_var = data.frame(variable = vars, n_missing = NA_integer_, n_extreme = NA_integer_, stringsAsFactors = FALSE)
        ),
        warnings = unique(collected_warnings)
      ))
    }
    return(out_empty)
  }

  # ---- Raw extreme screening (consistent API) ----
  capped_raw <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      list(
        BMI = c(5, 80),
        waist = c(20, 250),
        weight = c(2, 400),
        height = c(40, 250),
        hip = c(30, 250),
        WC = c(20, 250),
        HC = c(30, 250)
      )
    } else extreme_rules

    for (v in intersect(names(rules), vars)) {
      cn <- var_map[[v]]
      rng <- rules[[v]]
      x <- df[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      nbad <- sum(bad, na.rm = TRUE)
      if (nbad > 0) {
        if (extreme_action == "error") {
          .abort(sprintf("adiposity_sds(): %d raw extremes in '%s' (check_extreme).", nbad, v),
                 "healthmarkers_adiposds_error_raw_extreme")
        } else if (extreme_action == "cap") {
          x[bad & x < rng[1]] <- rng[1]
          x[bad & x > rng[2]] <- rng[2]
          df[[cn]] <- x
          capped_raw <- capped_raw + nbad
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_
          df[[cn]] <- x
          capped_raw <- capped_raw + nbad
        } else if (extreme_action == "warn") {
          .warn(sprintf("adiposity_sds(): %d raw extremes detected in '%s' (not altered).", nbad, v))
        } # ignore -> do nothing
      }
    }
    if (capped_raw > 0 && extreme_action %in% c("cap","NA")) {
      .warn(sprintf("adiposity_sds(): adjusted %d raw extreme values (%s).", capped_raw, extreme_action))
    }
  }

  # ---- Compute SDS over mapped columns ----
  per_var_summary <- data.frame(variable = vars, n_missing = integer(length(vars)), n_extreme = integer(length(vars)), stringsAsFactors = FALSE)
  total_extreme <- 0L
  out_list <- lapply(vars, function(v) {
    stats <- ref[[v]]
    cn <- var_map[[v]]
    x <- df[[cn]]
    sds <- unname((x - stats["mean"]) / stats["sd"])
    per_var_summary$n_missing[per_var_summary$variable == v] <<- sum(is.na(x))
    finite <- is.finite(sds)
    is_extreme_sds <- finite & abs(sds) > sds_cap
    n_ext <- sum(is_extreme_sds, na.rm = TRUE)
    per_var_summary$n_extreme[per_var_summary$variable == v] <<- n_ext
    total_extreme <<- total_extreme + n_ext
    if (n_ext > 0) {
      if (extreme_action == "error") {
        .abort(sprintf("adiposity_sds(): %d SDS beyond ±%g for '%s'.", n_ext, sds_cap, v),
               "healthmarkers_adiposds_error_sds_extreme")
      } else if (extreme_action == "cap") {
        sds[is_extreme_sds] <- sds_cap * sign(sds[is_extreme_sds])
        .warn(sprintf("adiposity_sds(): capped %d SDS beyond ±%g for '%s'.", n_ext, sds_cap, v))
      } else if (extreme_action == "warn") {
        .warn(sprintf("adiposity_sds(): %d SDS beyond ±%g for '%s' (not capped).", n_ext, sds_cap, v))
      } # NA or ignore -> leave as-is
    }
    sds
  })
  names(out_list) <- paste0(vars, "_SDS")
  out <- tibble::as_tibble(out_list)

  # ---- Proportion diagnostics (extreme SDS) ----
  denom_cells <- n_rows_out * length(vars)
  if (denom_cells > 0) {
    prop_extreme <- sum(per_var_summary$n_extreme) / denom_cells
    extreme_prop_thresh <- rlang::`%||%`(warn_thresholds$extreme_prop, 0.01)
    if (prop_extreme > extreme_prop_thresh && sum(per_var_summary$n_extreme) > 0) {
      .warn(sprintf("adiposity_sds(): high extreme SDS proportion %.2f%% (threshold %.2f%%)",
                    100 * prop_extreme, 100 * extreme_prop_thresh))
    }
  }

  # ---- Verbose/package-level completion summary ----
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  .inform(paste0(
    "Completed adiposity_sds: rows_in=", n_rows_in,
    " rows_out=", n_rows_out,
    " omitted=", omitted_rows,
    " raw_adjusted=", capped_raw,
    " sds_extreme=", total_extreme,
    " vars=", length(vars),
    sprintf(" elapsed=%.2fs", elapsed)
  ))

  # ---- Return ----
  if (isTRUE(return_summary)) {
    return(list(
      data = out,
      summary = list(
        rows_in = n_rows_in,
        rows_out = n_rows_out,
        omitted_rows = omitted_rows,
        total_extreme = total_extreme,
        raw_adjusted = capped_raw,
        per_var = per_var_summary
      ),
      warnings = unique(collected_warnings)
    ))
  }

  out
}

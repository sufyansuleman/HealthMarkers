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
  ref,
  na_action = c("omit","error","keep"),
  extreme_action = c("cap","warn","error","ignore"),
  sds_cap = 6,
  check_raw_extreme = FALSE,
  raw_extreme_rules = NULL,
  diagnostics = TRUE,
  warn_thresholds = list(na_prop = 0.05, extreme_prop = 0.01),
  id_col = NULL,
  verbose = FALSE,
  return_summary = FALSE,
  na_strategy = NULL,
  extreme_strategy = NULL
) {

  # ---- Legacy alias reconciliation ----
  if (!is.null(na_strategy)) {
    if (!missing(na_action)) {
      rlang::warn("adiposity_sds(): both na_strategy & na_action supplied; using na_action.")
    } else {
      rlang::warn("adiposity_sds(): argument 'na_strategy' is deprecated; use 'na_action'.")
      na_action <- na_strategy
    }
  }
  if (!is.null(extreme_strategy)) {
    if (!missing(extreme_action)) {
      rlang::warn("adiposity_sds(): both extreme_strategy & extreme_action supplied; using extreme_action.")
    } else {
      rlang::warn("adiposity_sds(): argument 'extreme_strategy' is deprecated; use 'extreme_action'.")
      extreme_action <- extreme_strategy
    }
  }

  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  collected_warnings <- character()

  .warn <- function(msg) {
    if (isTRUE(diagnostics)) {
      rlang::warn(msg)
    }
    collected_warnings <<- c(collected_warnings, msg)
  }
  .abort <- function(msg, class) rlang::abort(msg, class = class)
  .inform <- function(msg) if (isTRUE(verbose)) rlang::inform(msg)

  # ---- Validate inputs ----
  if (!is.data.frame(data)) {
    .abort("adiposity_sds(): `data` must be a data.frame or tibble.", "healthmarkers_adiposds_error_data_type")
  }
  if (!is.list(ref) || is.null(names(ref)) || any(!nzchar(names(ref)))) {
    .abort("adiposity_sds(): `ref` must be a named list of mean/sd vectors.", "healthmarkers_adiposds_error_ref_type")
  }
  vars <- names(ref)
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
  missing_cols <- setdiff(vars, names(data))
  if (length(missing_cols)) {
    .abort(paste0("adiposity_sds(): missing required columns: ", paste(missing_cols, collapse = ", ")),
           "healthmarkers_adiposds_error_missing_columns")
  }
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
    if (!is.numeric(df[[v]])) {
      old <- df[[v]]
      suppressWarnings(df[[v]] <- as.numeric(old))
      introduced <- sum(is.na(df[[v]]) & !is.na(old))
      if (introduced > 0) {
        .warn(sprintf("adiposity_sds(): '%s' coerced to numeric; NAs introduced: %d", v, introduced))
      } else {
        .warn(sprintf("adiposity_sds(): '%s' coerced to numeric.", v))
      }
    }
  }

  # ---- Missingness diagnostics ----
  if (length(vars)) {
    for (v in vars) {
      x <- df[[v]]
      pna <- mean(is.na(x))
      na_thresh <- warn_thresholds$na_prop %||% 0.05
      if (pna >= na_thresh && pna > 0) {
        .warn(sprintf("adiposity_sds(): '%s' missingness %.1f%%", v, 100 * pna))
      }
    }
  }

  # ---- NA handling (row-wise) ----
  row_na <- if (length(vars)) rowSums(is.na(df[vars])) > 0 else rep(FALSE, nrow(df))
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
    # create zero-row numeric columns (not NULL) to satisfy tibble
    out_empty <- tibble::as_tibble(
      setNames(
        replicate(length(vars), numeric(0), simplify = FALSE),
        paste0(vars, "_SDS")
      )
    )
    if (return_summary) {
      return(list(
        data = out_empty,
        summary = list(
          rows_in = n_rows_in,
          rows_out = 0L,
          omitted_rows = omitted_rows,
          total_extreme = 0L,
          per_var = data.frame(
            variable = vars,
            n_missing = NA_integer_,
            n_extreme = NA_integer_,
            stringsAsFactors = FALSE
          )
        ),
        warnings = unique(collected_warnings)
      ))
    }
    return(out_empty)
  }

  # ---- Raw extreme screening (optional) ----
  capped_raw <- 0L
  if (isTRUE(check_raw_extreme)) {
    rules <- if (is.null(raw_extreme_rules)) {
      list(
        BMI = c(5, 80),
        waist = c(20, 250),
        weight = c(2, 400),
        height = c(40, 250),
        hip = c(30, 250),
        WC = c(20, 250),
        HC = c(30, 250)
      )
    } else raw_extreme_rules
    for (v in intersect(names(rules), vars)) {
      rng <- rules[[v]]
      x <- df[[v]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      nbad <- sum(bad, na.rm = TRUE)
      if (nbad > 0) {
        if (extreme_action == "error") {
          .abort(sprintf("adiposity_sds(): %d raw extremes in '%s' (check_raw_extreme).", nbad, v),
                 "healthmarkers_adiposds_error_raw_extreme")
        } else if (extreme_action == "cap") {
          x[bad & x < rng[1]] <- rng[1]
          x[bad & x > rng[2]] <- rng[2]
          df[[v]] <- x
          capped_raw <- capped_raw + nbad
        } else if (extreme_action == "warn") {
          .warn(sprintf("adiposity_sds(): %d raw extremes detected in '%s' (not altered).", nbad, v))
        } # ignore -> do nothing
      }
    }
    if (capped_raw > 0 && extreme_action == "cap") {
      .warn(sprintf("adiposity_sds(): capped %d raw extreme values.", capped_raw))
    }
  }

  # ---- Compute SDS ----
  per_var_summary <- data.frame(
    variable = vars,
    n_missing = integer(length(vars)),
    n_extreme = integer(length(vars)),
    stringsAsFactors = FALSE
  )
  total_extreme <- 0L
  out_list <- lapply(vars, function(v) {
    stats <- ref[[v]]
    x <- df[[v]]
    # unname to drop carried names from stats["mean"]/["sd"]
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
      }
    }
    sds
  })
  names(out_list) <- paste0(vars, "_SDS")
  out <- tibble::as_tibble(out_list)

  # ---- Proportion diagnostics (extreme SDS) ----
  denom_cells <- n_rows_out * length(vars)
  if (denom_cells > 0) {
    prop_extreme <- sum(per_var_summary$n_extreme) / denom_cells
    extreme_prop_thresh <- warn_thresholds$extreme_prop %||% 0.01
    if (prop_extreme > extreme_prop_thresh && sum(per_var_summary$n_extreme) > 0) {
      .warn(sprintf("adiposity_sds(): high extreme SDS proportion %.2f%% (threshold %.2f%%)",
                    100 * prop_extreme, 100 * extreme_prop_thresh))
    }
  }

  # ---- Verbose summary ----
  if (isTRUE(verbose)) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(paste0(
      "Completed adiposity_sds: rows_in=", n_rows_in,
      " rows_out=", n_rows_out,
      " omitted=", omitted_rows,
      " raw_capped=", capped_raw,
      " sds_extreme=", total_extreme,
      " vars=", length(vars),
      sprintf(" elapsed=%.2fs", elapsed)
    ))
  }

  # ---- Return ----
  if (isTRUE(return_summary)) {
    return(list(
      data = out,
      summary = list(
        rows_in = n_rows_in,
        rows_out = n_rows_out,
        omitted_rows = omitted_rows,
        total_extreme = total_extreme,
        raw_capped = capped_raw,
        per_var = per_var_summary
      ),
      warnings = unique(collected_warnings)
    ))
  }

  out
}

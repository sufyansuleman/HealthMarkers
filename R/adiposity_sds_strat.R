# R/adiposity_sds_strat.R

#' Compute sex-stratified standardized scores (SDS) for adiposity measures
#'
#' @description
#' Computes sex-specific SDS (z-scores) for selected anthropometric variables using
#' reference means and SDs provided separately for males and females.
#'
#' @param data Data frame or tibble with variables and a sex column
#' @param col_map Named list mapping:
#'   - sex: column name with sex values ("M","F","m","f", 1, 2)
#'   - vars: optional named list mapping reference variable names -> data column names.
#'           If omitted, expects the reference variable names to exist in `data`.
#' @param ref Named list with elements "M" and "F". Each is a named list of numeric
#'   vectors c(mean=, sd=) keyed by variable name, e.g.:
#'   list(
#'     M = list(BMI = c(mean=23, sd=3.5), waist = c(mean=85, sd=10)),
#'     F = list(BMI = c(mean=21, sd=3.0), waist = c(mean=75, sd=9))
#'   )
#' @param na_action One of:
#'   - "keep"  - keep rows with NA (propagates to outputs)
#'   - "omit"  - drop rows with NA in any required variable
#'   - "error" - abort if any required variable has NA
#' @param check_extreme Logical; if TRUE, screen raw variables for extremes before SDS
#' @param extreme_action One of:
#'   - "cap"   - winsorize to bounds
#'   - "NA"    - set out-of-range to NA
#'   - "error" - abort on out-of-range
#' @param extreme_rules Optional named list of c(min, max) per variable (raw scale).
#'   If NULL, broad defaults are applied for common measures (BMI, waist, weight, height, hip, WC, HC).
#' @param allow_partial If TRUE, skip variables absent in data (with a warning); if FALSE error
#' @param prefix Optional prefix for output columns (default "")
#' @param verbose Logical; when TRUE emit progress via package logger; by default logging is controlled by options(healthmarkers.verbose)
#'
#' @return A tibble with one SDS column per retained variable: <prefix><var>_SDS
#'
#' @references
#' Cole TJ, Green PJ (1992) Smoothing reference centile curves: the LMS method and penalized likelihood. Stat Med 11(10):1305-1319. \doi{10.1002/sim.4780111005}
#' WHO Expert Committee (1995) Physical Status: The Use and Interpretation of Anthropometry. WHO TRS 854.
#'
#' @examples
#' ref <- list(
#'   M = list(BMI = c(mean=24.5, sd=3.8), waist = c(mean=88, sd=12)),
#'   F = list(BMI = c(mean=22.1, sd=4.2), waist = c(mean=76, sd=11))
#' )
#' df <- data.frame(BMI=c(25.2,21.8,27.1), waist=c(85,72,95), sex=c("M","F","M"))
#' adiposity_sds_strat(
#'   df,
#'   col_map = list(sex = "sex", vars = list(BMI = "BMI", waist = "waist")),
#'   ref = ref
#' )
#' @export
adiposity_sds_strat <- function(data,
                                col_map,
                                ref,
                                na_action = c("keep","omit","error"),
                                check_extreme = FALSE,
                                extreme_action = c("cap","NA","error"),
                                extreme_rules = NULL,
                                allow_partial = FALSE,
                                prefix = "",
                                verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Custom checks so tests see the expected error messages
  if (!is.list(col_map) || is.null(col_map$sex) || !nzchar(col_map$sex)) {
    rlang::abort("adiposity_sds_strat(): 'col_map$sex' is required.",
                 class = "healthmarkers_adiposds_error_colmap_sex")
  }
  sex_col <- col_map$sex
  if (!sex_col %in% names(data)) {
    rlang::abort(sprintf("adiposity_sds_strat(): sex column '%s' not found.", sex_col),
                 class = "healthmarkers_adiposds_error_sexcol_missing")
  }

  # Validate ref structure
  if (!is.list(ref) || !all(c("M","F") %in% names(ref))) {
    rlang::abort("adiposity_sds_strat(): `ref` must be a named list with elements 'M' and 'F'.",
                 class = "healthmarkers_adiposds_error_ref_structure")
  }
  vars_M <- names(ref$M); vars_F <- names(ref$F)
  if (is.null(vars_M) || is.null(vars_F) || any(vars_M == "") || any(vars_F == "")) {
    rlang::abort("adiposity_sds_strat(): reference variable names must be non-empty.",
                 class = "healthmarkers_adiposds_error_ref_names")
  }
  if (!identical(sort(vars_M), sort(vars_F))) {
    rlang::abort("adiposity_sds_strat(): ref$M and ref$F must define identical variables.",
                 class = "healthmarkers_adiposds_error_ref_var_mismatch")
  }
  vars <- vars_M

  # Validate each ref component
  check_ref_comp <- function(x) {
    if (!is.numeric(x) || is.null(names(x)) || !all(c("mean","sd") %in% names(x))) {
      rlang::abort("adiposity_sds_strat(): ref component must be numeric with names mean, sd.",
                   class = "healthmarkers_adiposds_error_ref_component")
    }
    if (!is.finite(x[["sd"]]) || x[["sd"]] <= 0) {
      rlang::abort("adiposity_sds_strat(): sd must be >0.",
                   class = "healthmarkers_adiposds_error_ref_sd")
    }
  }
  for (v in vars) {
    check_ref_comp(ref$M[[v]])
    check_ref_comp(ref$F[[v]])
  }

  # Build variable mapping (user mapping overrides defaults)
  var_map <- if (!is.null(col_map$vars)) {
    stopifnot(is.list(col_map$vars))
    c(col_map$vars, setNames(vars, vars))[vars]
  } else {
    setNames(vars, vars)
  }

  # Sex normalization
  sex_raw <- data[[sex_col]]
  sex_norm <- ifelse(sex_raw %in% c("M","m",1), "M",
                ifelse(sex_raw %in% c("F","f",2), "F", NA_character_))
  bad_sex <- sum(is.na(sex_norm))
  if (bad_sex > 0) {
    rlang::abort(sprintf("adiposity_sds_strat(): %d rows have unmapped sex values.", bad_sex),
                 class = "healthmarkers_adiposds_error_sex_values")
  }

  # Ensure variable columns exist (respect allow_partial)
  missing_in_data <- setdiff(unname(unlist(var_map, use.names = FALSE)), names(data))
  if (length(missing_in_data)) {
    if (isTRUE(allow_partial)) {
      hm_inform(sprintf(
        "adiposity_sds_strat(): skipping %d ref vars absent in data: %s",
        length(missing_in_data), paste(missing_in_data, collapse = ", ")
      ), level = "inform")
      keep_vars <- vars[unname(unlist(var_map)) %in% names(data)]
      vars <- keep_vars
      var_map <- var_map[vars]
    } else {
      rlang::abort(sprintf("adiposity_sds_strat(): variables not found in data: %s",
                           paste(setdiff(names(var_map), names(var_map)[unname(unlist(var_map)) %in% names(data)]), collapse = ", ")),
                   class = "healthmarkers_adiposds_error_missing_vars")
    }
  }
  if (!length(vars)) {
    rlang::abort("adiposity_sds_strat(): no variables remain to process.",
                 class = "healthmarkers_adiposds_error_no_vars")
  }

  hm_inform(sprintf("-> adiposity_sds_strat: processing %d variables for %d rows",
                    length(vars), nrow(data)), level = "inform")

  # Coerce to numeric (warn on NA introduction)
  for (v in vars) {
    cn <- var_map[[v]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      intro <- sum(is.na(data[[cn]]) & !is.na(old))
      if (intro > 0) {
        rlang::warn(
          sprintf("adiposity_sds_strat(): column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
          class = "healthmarkers_adiposds_warn_na_coercion"
        )
      }
    }
  }

  # NA policy (across selected variables)
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(unname(unlist(var_map)), function(cn) is.na(data[[cn]])))
    if (any(any_na)) {
      rlang::abort("adiposity_sds_strat(): missing values present (na_action='error').",
                   class = "healthmarkers_adiposds_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(unname(unlist(var_map)), function(cn) is.na(data[[cn]])))
    data <- data[keep, , drop = FALSE]
    sex_norm <- sex_norm[keep]
  }
  if (nrow(data) == 0L) return(tibble::tibble())

  # Extreme scan (raw scale)
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

    total_adj <- 0L
    for (v in intersect(names(rules), vars)) {
      cn <- var_map[[v]]
      rng <- rules[[v]]
      x <- data[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      nbad <- sum(bad, na.rm = TRUE)
      if (nbad > 0) {
        if (extreme_action == "error") {
          rlang::abort(sprintf("adiposity_sds_strat(): %d extreme values in '%s'.", nbad, v),
                       class = "healthmarkers_adiposds_error_extremes")
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_
          data[[cn]] <- x
          total_adj <- total_adj + nbad
        } else if (extreme_action == "cap") {
          x[bad & x < rng[1]] <- rng[1]
          x[bad & x > rng[2]] <- rng[2]
          data[[cn]] <- x
          total_adj <- total_adj + nbad
        }
      }
    }
    if (total_adj > 0 && extreme_action %in% c("cap","NA")) {
      rlang::warn(sprintf("adiposity_sds_strat(): adjusted %d extreme input values (%s).",
                          total_adj, extreme_action))
    }
  }

  # Compute SDS
  out_list <- lapply(vars, function(v) {
    stats_M <- ref$M[[v]]
    stats_F <- ref$F[[v]]
    mu  <- ifelse(sex_norm == "M", stats_M[["mean"]], stats_F[["mean"]])
    sdv <- ifelse(sex_norm == "M", stats_M[["sd"]],   stats_F[["sd"]])
    (data[[var_map[[v]]]] - mu) / sdv
  })
  names(out_list) <- paste0(prefix, vars, "_SDS")
  out <- tibble::as_tibble(out_list)

  hm_inform("Completed adiposity_sds_strat", level = "inform")
  out
}

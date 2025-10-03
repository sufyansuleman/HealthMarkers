# R/adiposity_sds_strat.R

#' Calculate sex-stratified standardized scores (SDS) for adiposity measures (HM-CRANIZED)
#'
#' Computes sex-specific z-scores (standard deviation scores) for supplied
#' anthropometric variables using reference means/SDs provided separately
#' for males and females. Adds extended validation, NA and extreme handling.
#'
#' @param data data.frame / tibble containing variables + a sex column.
#' @param ref Named list with elements "M" and "F". Each is a named list of
#'   numeric vectors c(mean=, sd=). Example:
#'   list(
#'     M = list(BMI = c(mean=23, sd=3.5), waist = c(mean=85, sd=10)),
#'     F = list(BMI = c(mean=21, sd=3.0), waist = c(mean=75, sd=9))
#'   )
#' @param sex_col Name of sex column (default "sex"). Values accepted:
#'   "M","F","m","f", 1,2.
#' @param na_action One of c("keep","omit","error") for rows with NA in any
#'   required measure (default "keep").
#' @param na_warn_prop Proportion (0–1) above which per-variable missingness
#'   triggers a warning (default 0.2).
#' @param check_extreme Logical; if TRUE perform extreme range screening on
#'   raw variables before SDS computation.
#' @param extreme_action One of c("warn","cap","error","ignore") for handling
#'   detected extremes (default "warn").
#' @param extreme_rules Optional named list of c(min,max) bounds per variable
#'   (raw scale). If NULL, broad defaults are inferred per variable name
#'   (supports BMI, waist, weight, height, hip, WC, HC; others skipped).
#' @param allow_partial If TRUE and some reference variables are missing in
#'   data (or vice versa) they are skipped with a warning; if FALSE (default)
#'   this is an error.
#' @param diagnostics Logical; if TRUE (default) emit diagnostic warnings
#'   (missing ref vars, extreme SDS > 10, etc.).
#' @param verbose Logical; if TRUE print progress & summary.
#' @param prefix Optional string prepended to output column names (default "").
#'
#' @return A tibble with one SDS column per retained variable:
#'   <prefix><var>_SDS
#'
#' @details
#' SDS = (observed - sex_mean) / sex_sd (with sex-specific mean/sd vectorization).
#' Rows dropped only if na_action == "omit" and required var NA present.
#'
#' @references
#' WHO Expert Committee. Physical Status: The Use and Interpretation of Anthropometry. WHO Technical Report Series 854. Geneva: World Health Organization; 1995.
#' Cole TJ, Green PJ (1992). Smoothing reference centile curves: the LMS method and penalized likelihood. Stat Med, 11(10):1305–1319. \doi{10.1002/sim.4780111005}
#'
#' @examples
#' ref <- list(
#'   M = list(BMI = c(mean=24.5, sd=3.8), waist = c(mean=88, sd=12)),
#'   F = list(BMI = c(mean=22.1, sd=4.2), waist = c(mean=76, sd=11))
#' )
#' df <- data.frame(BMI=c(25.2,21.8,27.1), waist=c(85,72,95), sex=c("M","F","M"))
#' adiposity_sds_strat(df, ref, verbose=TRUE)
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang abort warn inform
#' @export
adiposity_sds_strat <- function(data,
                                ref,
                                sex_col = "sex",
                                na_action = c("keep","omit","error"),
                                na_warn_prop = 0.2,
                                check_extreme = FALSE,
                                extreme_action = c("warn","cap","error","ignore"),
                                extreme_rules = NULL,
                                allow_partial = FALSE,
                                diagnostics = TRUE,
                                verbose = FALSE,
                                prefix = "") {
  t0 <- Sys.time()
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # --- validate data
  if (!is.data.frame(data)) {
    rlang::abort("adiposity_sds_strat(): `data` must be a data.frame/tibble.",
                 class = "healthmarkers_adiposds_error_data_type")
  }
  if (!is.character(sex_col) || length(sex_col) != 1) {
    rlang::abort("adiposity_sds_strat(): `sex_col` must be single character name.",
                 class = "healthmarkers_adiposds_error_sexcol_type")
  }
  if (!sex_col %in% names(data)) {
    rlang::abort(sprintf("adiposity_sds_strat(): sex column '%s' not found.", sex_col),
                 class = "healthmarkers_adiposds_error_sexcol_missing")
  }

  # --- validate ref
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
  # component validation
  .chk_ref_components <- function(ref_entry, sex_label) {
    if (!is.list(ref_entry) || is.null(names(ref_entry)))
      rlang::abort(sprintf("adiposity_sds_strat(): ref[['%s']] must be a named list.", sex_label),
                   class = "healthmarkers_adiposds_error_ref_sex_list")
    for (nm in names(ref_entry)) {
      v <- ref_entry[[nm]]
      if (!(is.numeric(v) && all(c("mean","sd") %in% names(v)) && length(v) >= 2)) {
        rlang::abort(sprintf("adiposity_sds_strat(): ref[['%s']][['%s']] must be numeric with names mean, sd.",
                             sex_label, nm),
                     class = "healthmarkers_adiposds_error_ref_component")
      }
      if (v["sd"] <= 0 || any(!is.finite(v[c("mean","sd")]))) {
        rlang::abort(sprintf("adiposity_sds_strat(): ref[['%s']][['%s']]: sd must be >0 & finite.",
                             sex_label, nm),
                     class = "healthmarkers_adiposds_error_ref_component_value")
      }
    }
  }
  .chk_ref_components(ref$M, "M")
  .chk_ref_components(ref$F, "F")

  vars <- vars_M

  # --- reconcile variables with data
  missing_in_data <- setdiff(vars, names(data))
  if (length(missing_in_data)) {
    if (allow_partial) {
      if (isTRUE(diagnostics))
        rlang::warn(sprintf("adiposity_sds_strat(): skipping %d ref vars absent in data: %s",
                            length(missing_in_data), paste(missing_in_data, collapse = ", ")))
      vars <- setdiff(vars, missing_in_data)
    } else {
      rlang::abort(sprintf("adiposity_sds_strat(): variables not found in data: %s",
                           paste(missing_in_data, collapse = ", ")),
                   class = "healthmarkers_adiposds_error_missing_vars")
    }
  }
  if (!length(vars)) {
    rlang::abort("adiposity_sds_strat(): no variables remain to process.",
                 class = "healthmarkers_adiposds_error_no_vars")
  }

  # --- normalize sex values
  sex_raw <- data[[sex_col]]
  sex_norm <- ifelse(sex_raw %in% c("M","m",1), "M",
                ifelse(sex_raw %in% c("F","f",2), "F", NA_character_))
  bad_sex <- sum(is.na(sex_norm))
  if (bad_sex > 0) {
    rlang::abort(sprintf("adiposity_sds_strat(): %d rows have unmapped sex values.", bad_sex),
                 class = "healthmarkers_adiposds_error_sex_values")
  }

  if (isTRUE(verbose))
    rlang::inform(sprintf("-> adiposity_sds_strat: processing %d variables for %d rows",
                          length(vars), nrow(data)))

  # --- numeric coercion & missingness diagnostics
  for (v in vars) {
    if (!is.numeric(data[[v]])) {
      old <- data[[v]]
      suppressWarnings(data[[v]] <- as.numeric(old))
      introduced <- sum(is.na(data[[v]]) & !is.na(old))
      if (introduced > 0 && isTRUE(diagnostics))
        rlang::warn(sprintf("adiposity_sds_strat(): '%s' coerced to numeric; NAs introduced: %d", v, introduced))
    }
  }

  if (isTRUE(diagnostics)) {
    for (v in vars) {
      x <- data[[v]]; pna <- sum(is.na(x))/length(x)
      if (pna >= na_warn_prop && pna > 0)
        rlang::warn(sprintf("adiposity_sds_strat(): '%s' missingness %.1f%%", v, 100*pna))
    }
  }

  # --- NA handling policy
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(vars, function(v) is.na(data[[v]])))
    if (any(any_na))
      rlang::abort("adiposity_sds_strat(): missing values present (na_action='error').",
                   class = "healthmarkers_adiposds_error_missing_values")
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(vars, function(v) is.na(data[[v]]) ))
    dropped <- sum(!keep)
    data <- data[keep, , drop = FALSE]
    sex_norm <- sex_norm[keep]
    if (isTRUE(verbose))
      rlang::inform(sprintf("-> adiposity_sds_strat: omitted %d rows with NA", dropped))
  }

  if (nrow(data) == 0L) {
    return(tibble::tibble())
  }

  # --- extreme value scan (raw scale)
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      # heuristic defaults
      default_ranges <- list(
        BMI = c(5, 80),
        waist = c(20, 250),
        weight = c(2, 400),
        height = c(40, 250),
        hip = c(30, 250),
        WC = c(20, 250),
        HC = c(30, 250)
      )
      # keep only variables present
      default_ranges
    } else extreme_rules
    ex_counts <- integer()
    for (v in intersect(names(rules), vars)) {
      rng <- rules[[v]]
      x <- data[[v]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      nbad <- sum(bad, na.rm = TRUE)
      ex_counts[v] <- nbad
      if (nbad > 0) {
        if (extreme_action == "error") {
          rlang::abort(sprintf("adiposity_sds_strat(): %d extreme values in '%s'.", nbad, v),
                       class = "healthmarkers_adiposds_error_extremes")
        } else if (extreme_action == "cap") {
          x[bad & x < rng[1]] <- rng[1]
          x[bad & x > rng[2]] <- rng[2]
          data[[v]] <- x
          capped_n <- capped_n + nbad
        } else if (extreme_action == "warn") {  # removed diagnostics gating
          rlang::warn(sprintf("adiposity_sds_strat(): %d extremes detected in '%s' (not altered).", nbad, v))
        }
      }
    }
    if (extreme_action == "cap" && capped_n > 0) {  # removed diagnostics gating
      rlang::warn(sprintf("adiposity_sds_strat(): capped %d extreme values.", capped_n))
    }
  }

  # --- compute SDS
  out_list <- lapply(vars, function(v) {
    stats_M <- ref$M[[v]]
    stats_F <- ref$F[[v]]
    mu <- ifelse(sex_norm == "M", stats_M["mean"], stats_F["mean"])
    sdv <- ifelse(sex_norm == "M", stats_M["sd"],  stats_F["sd"])
    sds <- (data[[v]] - mu) / sdv
    if (isTRUE(diagnostics)) {
      if (any(is.infinite(sds), na.rm = TRUE))
        rlang::warn(sprintf("adiposity_sds_strat(): infinite SDS produced for '%s'.", v))
      if (any(abs(sds) > 10, na.rm = TRUE))
        rlang::warn(sprintf("adiposity_sds_strat(): extreme |SDS|>10 for '%s'.", v))
    }
    sds
  })
  names(out_list) <- paste0(prefix, vars, "_SDS")

  out <- tibble::as_tibble(out_list)

  if (isTRUE(verbose)) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    na_counts <- vapply(out, function(x) sum(is.na(x)), integer(1))
    rlang::inform(sprintf(
      "Completed adiposity_sds_strat: rows=%d vars=%d capped=%d elapsed=%.2fs | NA: %s",
      nrow(out), length(vars), capped_n, elapsed,
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", ")
    ))
  }

  out
}

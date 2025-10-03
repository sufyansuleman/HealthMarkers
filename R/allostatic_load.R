# File: allostatic_load.R
#'
#' Allostatic Load Index (HM-CRANIZED)
#'
#' Computes a composite Allostatic Load (AL) score by flagging biomarkers that
#' exceed user-specified high-risk thresholds (strict > when multiple biomarkers;
#' inclusive >= when only one biomarker). Designed to align with HealthMarkers
#' HM-CRANIZED conventions: structured validation, diagnostic control, verbose
#' reporting, and optional summary output.
#'
#' Methodological context:
#' Allostatic Load is a multisystem cumulative (dis)regulation construct. This
#' function operationalizes a threshold-based (count) AL index; other approaches
#' (e.g. z-score summation, percentile cut-points, weighted or system-level
#' aggregation) exist and may be preferable depending on study aims.
#' Thresholds supplied via `thresholds` should be justified (e.g. clinical
#' cut-points, cohort-specific quartiles, or consensus risk criteria). This
#' routine does not derive thresholds—only applies them.
#'
#' @param data data.frame or tibble of numeric biomarker columns.
#' @param thresholds named list of scalar numeric cutoffs (names must match columns).
#' @param verbose logical; print progress messages.
#' @param na_action one of c("zero","warn_zero","error","keep") ("keep" treated as "zero").
#' @param na_warn_prop numeric in [0,1]; high-missingness warning threshold (default 0.2).
#' @param check_extreme_sds logical; scan columns containing "sds" for |value| > sds_limit.
#' @param sds_limit positive numeric cutoff for SDS-like scan (default 6).
#' @param extreme_sds_action one of c("warn","error","ignore") for SDS-like extremes.
#' @param diagnostics logical; FALSE suppresses non-critical warnings.
#' @param return_summary logical; TRUE returns list(data, summary, warnings).
#'
#' @return tibble with AllostaticLoad or list when return_summary = TRUE.
#' @seealso \code{\link{adiposity_sds}}, \code{\link{adiposity_sds_strat}}
#'
#' @references
#' McEwen BS, Stellar E (1993). Stress and the individual: mechanisms leading to disease. Brain Res Rev, 17:209–238.
#' Seeman TE, Singer BH, Rowe JW, Horwitz RI, McEwen BS (1997). Price of adaptation—Allostatic Load and its health consequences. Arch Intern Med, 157:2259–2268.
#' Juster RP et al, Allostatic Load biomarkers: review of measurement issues, Neurosci Biobehav Rev, 2010;35:2–16.
#' Wiley JF et al, Methodological considerations in constructing allostatic load indices, Psychoneuroendocrinology, 2021;129:105248.
#' @export
allostatic_load <- function(
  data,
  thresholds,
  verbose = FALSE,
  na_action = c("zero", "warn_zero", "error", "keep"),
  na_warn_prop = 0.2,
  check_extreme_sds = FALSE,
  sds_limit = 6,
  extreme_sds_action = c("warn", "error", "ignore"),
  diagnostics = TRUE,
  return_summary = FALSE
) {
  na_action <- match.arg(na_action)
  if (na_action == "keep") na_action <- "zero"
  extreme_sds_action <- match.arg(extreme_sds_action)

  collected_warnings <- character()

  .warn <- function(msg) {
    collected_warnings <<- c(collected_warnings, msg)
    if (diagnostics) rlang::warn(msg)
  }
  .abort <- function(msg, class = NULL) {
    if (is.null(class)) rlang::abort(msg) else rlang::abort(msg, class = class)
  }
  .inform <- function(msg) if (verbose) rlang::inform(msg)

  # ---- Validate inputs ----
  if (!is.data.frame(data)) {
    .abort("`data` must be a data.frame or tibble.", "healthmarkers_allo_error_data_type")
  }
  .validate_misc_args(verbose, na_warn_prop, check_extreme_sds, sds_limit)

  if (!is.list(thresholds) || is.null(names(thresholds))) {
    .abort("`thresholds` must be a named list of numeric cutoffs.", "healthmarkers_allo_error_thr_type")
  }
  if (!length(thresholds)) {
    .abort("`thresholds` must contain at least one element.", "healthmarkers_allo_error_thr_empty")
  }
  if (any(!nzchar(names(thresholds)))) {
    .abort("All `thresholds` must have non-empty names.", "healthmarkers_allo_error_thr_names")
  }
  if (any(duplicated(names(thresholds)))) {
    .abort("Duplicate names in `thresholds`.", "healthmarkers_allo_error_thr_dupe")
  }
  if (!all(vapply(thresholds, .is_scalar_finite_numeric, logical(1)))) {
    .abort("All threshold entries must be length-1 finite numerics.", "healthmarkers_allo_error_thr_values")
  }

  vars <- names(thresholds)
  missing <- setdiff(vars, names(data))
  if (length(missing)) {
    .abort(paste("Missing biomarker columns:", paste(missing, collapse = ", ")),
           "healthmarkers_allo_error_missing_cols")
  }
  for (v in vars) {
    if (!is.numeric(data[[v]])) {
      .abort(sprintf("Biomarker '%s' must be numeric.", v),
             "healthmarkers_allo_error_non_numeric")
    }
  }

  # Zero-row shortcut
  if (nrow(data) == 0L) {
    out0 <- tibble::tibble(AllostaticLoad = integer(0))
    if (return_summary) {
      return(list(
        data = out0,
        summary = list(
          rows = 0L,
          biomarkers = length(vars),
            total_flags = 0L,
            mean_flags = NA_real_
        ),
        warnings = character()
      ))
    }
    return(out0)
  }

  # ---- Scan quality / extremes ----
  high_na <- character()
  all_na  <- character()
  nonfinite <- character()
  sds_ext_msgs <- character()
  sds_ext_n <- 0L

  for (v in vars) {
    x <- data[[v]]
    n_nonf <- sum(!is.finite(x))
    if (n_nonf) nonfinite <- c(nonfinite, sprintf("%s(%d)", v, n_nonf))
    bad <- sum(is.na(x) | !is.finite(x))
    if (bad == length(x)) all_na <- c(all_na, v)
    if (bad > 0 && bad / length(x) >= na_warn_prop) {
      high_na <- c(high_na, sprintf("%s(%.1f%%)", v, 100 * bad / length(x)))
    }
    if (check_extreme_sds && grepl("sds", v, ignore.case = TRUE)) {
      x2 <- x
      x2[!is.finite(x2)] <- NA_real_
      hits <- sum(!is.na(x2) & abs(x2) > sds_limit)
      if (hits) {
        sds_ext_n <- sds_ext_n + hits
        sds_ext_msgs <- c(sds_ext_msgs, sprintf("%s(%d>|%g|)", v, hits, sds_limit))
      }
    }
  }

  if (na_action == "error") {
    any_bad <- any(vapply(vars, function(v) any(is.na(data[[v]]) | !is.finite(data[[v]])), logical(1)))
    if (any_bad) {
      .abort("Missing/non-finite values present (na_action='error').",
             "healthmarkers_allo_error_missing_values")
    }
  } else if (na_action == "warn_zero") {
    if (length(nonfinite)) .warn(paste("Non-finite in:", paste(nonfinite, collapse = ", ")))
    if (length(all_na))   .warn(paste("Entirely missing:", paste(all_na, collapse = ", ")))
    if (length(high_na))  .warn(paste("High missingness:", paste(high_na, collapse = ", ")))
    if (length(nonfinite) || length(all_na) || length(high_na)) {
      .warn("Missing/non-finite values detected; treated as zero (na_action='warn_zero').")
    }
  }

  if (sds_ext_n) {
    msg <- paste("Extreme SDS-like values:", paste(sds_ext_msgs, collapse = "; "))
    if (extreme_sds_action == "error") {
      .abort(msg, "healthmarkers_allo_error_extreme_sds")
    } else if (extreme_sds_action == "warn") {
      .warn(msg)
    }
  }

  # ---- Compute ----
  inclusive <- length(vars) == 1L
  .inform(paste("Computing Allostatic Load (rule", if (inclusive) ">=" else ">", ")"))

  flag_mat <- vapply(
    vars,
    function(v) {
      x <- data[[v]]
      x[!is.finite(x)] <- NA_real_
      th <- thresholds[[v]]
      flag <- if (inclusive) x >= th else x > th
      if (na_action %in% c("zero", "warn_zero")) {
        as.integer(ifelse(is.na(x), 0L, flag))
      } else {
        as.integer(flag)
      }
    },
    integer(nrow(data))
  )

  load <- rowSums(flag_mat, na.rm = TRUE)
  out <- tibble::tibble(AllostaticLoad = as.integer(load))

  .inform(sprintf("Completed: rows=%d biomarkers=%d total_flags=%d mean=%.2f",
                  nrow(data), length(vars), sum(out$AllostaticLoad), mean(out$AllostaticLoad)))

  if (return_summary) {
    return(list(
      data = out,
      summary = list(
        rows = nrow(data),
        biomarkers = length(vars),
        total_flags = sum(out$AllostaticLoad),
        mean_flags = mean(out$AllostaticLoad)
      ),
      warnings = unique(collected_warnings)
    ))
  }

  out
}

# ---- Helpers ----
.is_scalar_finite_numeric <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

.validate_misc_args <- function(verbose, na_warn_prop, check_extreme_sds, sds_limit) {
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    rlang::abort("`verbose` must be a single logical.")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop))) {
    rlang::abort("`na_warn_prop` must be a single finite numeric.")
  }
  if (na_warn_prop < 0 || na_warn_prop > 1) {
    rlang::abort("`na_warn_prop` must be in [0,1].")
  }
  if (!(is.logical(check_extreme_sds) && length(check_extreme_sds) == 1L && !is.na(check_extreme_sds))) {
    rlang::abort("`check_extreme_sds` must be a single logical.")
  }
  if (!(is.numeric(sds_limit) && length(sds_limit) == 1L && is.finite(sds_limit) && sds_limit > 0)) {
    rlang::abort("`sds_limit` must be a single positive finite numeric.")
  }
  invisible(TRUE)
}

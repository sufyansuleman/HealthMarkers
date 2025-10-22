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
#' @param data data.frame or tibble of numeric biomarker columns.
#' @param thresholds named list of scalar numeric cutoffs (names must match columns).
#' @param col_map optional named list mapping keys in `thresholds` to column names in `data`.
#' @param na_action one of c("keep","omit","error") ("keep" treated as "zero" contribution).
#' @param check_extreme logical; scan columns with name containing "sds" for |value| > sds_limit.
#' @param extreme_action one of c("cap","NA","error") for SDS-like extremes.
#' @param sds_limit positive numeric cutoff for SDS-like scan (default 6).
#' @param return_summary logical; TRUE returns list(data, summary, warnings).
#' @param verbose logical; print progress messages.
#'
#' @return tibble with AllostaticLoad or list when return_summary = TRUE.
#' @seealso \code{\link{adiposity_sds}}, \code{\link{adiposity_sds_strat}}
#'
#' @references
#' McEwen BS, Stellar E (1993). Stress and the individual: mechanisms leading to disease. Brain Res Rev, 17:209–238.
#' Seeman TE, Singer BH, Rowe JW, Horwitz RI, McEwen BS (1997). Price of adaptation—Allostatic Load and its health consequences. Arch Intern Med, 157:2259–2268.
#' Juster RP et al, Neurosci Biobehav Rev, 2010;35:2–16.
#' Wiley JF et al, Psychoneuroendocrinology, 2021;129:105248.
#'
#' @export
allostatic_load <- function(
  data,
  thresholds,
  col_map = NULL,
  na_action = c("keep","omit","error"),
  check_extreme = FALSE,
  extreme_action = c("cap","NA","error"),
  sds_limit = 6,
  return_summary = FALSE,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  hm_inform("allostatic_load(): preparing inputs", level = "debug")

  # ---- Validate inputs ----
  if (!is.data.frame(data)) {
    rlang::abort("allostatic_load(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_allo_error_data_type")
  }
  if (length(thresholds) == 0L) {
    rlang::abort("allostatic_load(): `thresholds` must contain at least one element.",
                 class = "healthmarkers_allo_error_thr_empty")
  }
  if (!is.list(thresholds) || is.null(names(thresholds)) || any(!nzchar(names(thresholds)))) {
    rlang::abort("allostatic_load(): `thresholds` must be a named list of scalar numeric cutoffs.",
                 class = "healthmarkers_allo_error_thr_type")
  }
  if (any(duplicated(names(thresholds)))) {
    rlang::abort("allostatic_load(): duplicate names in `thresholds`.",
                 class = "healthmarkers_allo_error_thr_dupe")
  }
  if (!all(vapply(thresholds, function(x) is.numeric(x) && length(x) == 1L && is.finite(x), logical(1)))) {
    rlang::abort("allostatic_load(): each threshold must be a length-1 finite numeric.",
                 class = "healthmarkers_allo_error_thr_values")
  }

  vars <- names(thresholds)

  # Build effective var -> column mapping
  var_map <- setNames(vars, vars)
  if (!is.null(col_map)) {
    stopifnot(is.list(col_map))
    # user-provided names in col_map override defaults
    var_map <- c(col_map, var_map)[vars]
  }

  hm_validate_inputs(
    data = data,
    col_map = as.list(var_map),
    required_keys = vars,
    fn = "allostatic_load"
  )
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> allostatic_load: validating inputs")

  # Confirm required columns exist, and capture their names in data
  req_cols <- unname(unlist(var_map, use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("allostatic_load(): missing required columns in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_allo_error_missing_columns"
    )
  }

  # Coerce only required columns to numeric; non-finite -> NA
  for (cn in req_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("allostatic_load(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # ---- NA row policy (row-wise over the required columns) ----
  rows_with_na <- if (length(req_cols)) {
    Reduce(`|`, lapply(req_cols, function(cn) is.na(data[[cn]])))
  } else rep(FALSE, nrow(data))

  if (na_action == "error" && any(rows_with_na)) {
    rlang::abort("allostatic_load(): missing/non-finite values present (na_action='error').",
                 class = "healthmarkers_allo_error_missing_values")
  } else if (na_action == "omit" && any(rows_with_na)) {
    data <- data[!rows_with_na, , drop = FALSE]
  }

  # Early return if no rows remain
  if (nrow(data) == 0L) {
    out0 <- tibble::tibble(AllostaticLoad = integer(0))
    return(if (return_summary) list(
      data = out0,
      summary = list(rows = 0L, biomarkers = length(vars), total_flags = 0L, mean_flags = NA_real_),
      warnings = character()
    ) else out0)
  }

  # ---- Optional SDS-like extreme scan over vars containing "sds" ----
  if (isTRUE(check_extreme) && length(vars)) {
    sds_vars <- vars[grepl("sds", vars, ignore.case = TRUE)]
    for (v in sds_vars) {
      cn <- var_map[[v]]
      x <- data[[cn]]
      bad <- is.finite(x) & abs(x) > sds_limit
      nbad <- sum(bad, na.rm = TRUE)
      if (nbad > 0) {
        if (extreme_action == "error") {
          rlang::abort(
            sprintf("allostatic_load(): extreme SDS-like values in '%s' (%d > |%g|).", v, nbad, sds_limit),
            class = "healthmarkers_allo_error_extreme_sds"
          )
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_
          data[[cn]] <- x
        } else if (extreme_action == "cap") {
          x[bad & x > 0] <-  sds_limit
          x[bad & x < 0] <- -sds_limit
          data[[cn]] <- x
        }
      }
    }
  }

  # ---- Compute flags and sum ----
  inclusive <- length(vars) == 1L
  hm_inform(sprintf("allostatic_load(): computing (rule %s)", if (inclusive) ">=" else ">"), level = "inform")

  flag_cols <- lapply(vars, function(v) {
    cn <- var_map[[v]]
    x <- data[[cn]]
    x[!is.finite(x)] <- NA_real_
    th <- thresholds[[v]]
    as.integer(if (inclusive) x >= th else x > th)
  })

  flag_mat <- if (length(flag_cols)) {
    fm <- do.call(cbind, flag_cols)
    colnames(fm) <- vars
    fm
  } else {
    matrix(integer(0), nrow = nrow(data), ncol = 0)
  }

  out <- tibble::tibble(AllostaticLoad = as.integer(rowSums(flag_mat, na.rm = TRUE)))

  hm_inform("allostatic_load(): computed allostatic load", level = "inform")

  if (isTRUE(return_summary)) {
    return(list(
      data = out,
      summary = list(
        rows = nrow(data),
        biomarkers = length(vars),
        total_flags = sum(out$AllostaticLoad),
        mean_flags = mean(out$AllostaticLoad)
      ),
      warnings = character()
    ))
  }

  out
}

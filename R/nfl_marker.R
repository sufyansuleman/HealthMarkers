#' Neurofilament Light Chain (NfL) Biomarker
#'
#' Incorporates a neurofilament light chain (NfL) measurement into the analysis pipeline.
#' Placeholder for future NfL-based computations; returns provided values with input checks.
#'
#' @details
#' NfL is released during neuroaxonal injury; elevated levels in CSF or blood indicate
#' neuroaxonal damage and typically increase with age and in neurological diseases.
#' Interpretation requires context-specific and age-adjusted references. This function
#' simply returns the input NfL values (assumed in a single matrix/fluid, e.g., plasma pg/mL)
#' without classification.
#'
#' @param data A data.frame or tibble with an NfL concentration column.
#' @param col_map Named list with `nfl` indicating the NfL column name.
#' @param verbose Logical; if TRUE, emits progress messages.
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param check_extreme Logical; if TRUE, scan inputs for plausible ranges.
#' @param extreme_action One of c("warn","cap","error","ignore","NA").
#' @param extreme_rules Optional overrides; default: list(nfl = c(0, 1e6)) in input units.
#'
#' @return A tibble with one column: nfl_value (numeric; same units as input).
#'
#' @references
#' Simren J, Ashton NJ, Blennow K, Zetterberg H, et al. (2022).
#' Reference values for plasma neurofilament light in healthy individuals.
#' Brain Commun. 4(4):fcac174. doi:10.1093/braincomms/fcac174
#'
#' Disanto G, Barro C, Benkert P, et al. (2017).
#' Serum neurofilament light: A biomarker of neuronal damage in multiple sclerosis.
#' Neurology. 88(9):861-867. doi:10.1212/WNL.0000000000003663
#'
#' @export
nfl_marker <- function(
  data,
  col_map = list(nfl = "NfL"),
  verbose = FALSE,
  na_action = c("keep","omit","error","ignore","warn"),
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  # --- validate data and col_map (HM-CS style) -------------------------------
  if (!is.data.frame(data)) {
    rlang::abort(
      "nfl_marker(): `data` must be a data.frame or tibble.",
      class = "healthmarkers_nfl_error_data_type"
    )
  }

  if (!is.list(col_map)) {
    rlang::abort(
      "nfl_marker(): `col_map` must be a named list.",
      class = "healthmarkers_nfl_error_colmap_type"
    )
  }

  if (length(col_map) == 0L || is.null(names(col_map))) {
    rlang::abort(
      "nfl_marker(): missing or empty `col_map`.",
      class = "healthmarkers_nfl_error_missing_map"
    )
  }

  if (!("nfl" %in% names(col_map))) {
    rlang::abort(
      "nfl_marker(): missing col_map entry for: nfl",
      class = "healthmarkers_nfl_error_missing_map"
    )
  }

  mapped <- unname(col_map[["nfl"]])
  if (is.null(mapped) || length(mapped) == 0L || !nzchar(as.character(mapped))) {
    rlang::abort(
      "nfl_marker(): missing col_map entry value for: nfl",
      class = "healthmarkers_nfl_error_bad_map_values"
    )
  }

  if (!(mapped %in% names(data))) {
    rlang::abort(
      paste0("nfl_marker(): missing required column in data: ", mapped),
      class = "healthmarkers_nfl_error_missing_columns"
    )
  }

  # --- verbose / debug messages ---------------------------------------------
  if (isTRUE(verbose)) {
    rlang::inform("-> nfl_marker: preparing inputs")
  } else if (exists("hm_inform", mode = "function")) {
    hm_inform("nfl_marker(): preparing inputs", level = "debug")
  }

  # --- coerce to numeric; warn on NA introduction ---------------------------
  cn <- mapped
  if (!is.numeric(data[[cn]])) {
    old <- data[[cn]]
    suppressWarnings(new <- as.numeric(old))
    intro <- sum(is.na(new) & !is.na(old))
    if (intro > 0L) {
      rlang::warn(
        sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, intro),
        class = "healthmarkers_nfl_warn_na_coercion"
      )
    }
    data[[cn]] <- new
  }
  data[[cn]][!is.finite(data[[cn]])] <- NA_real_

  nfl_vals <- data[[cn]]

  # --- NA policy ------------------------------------------------------------
  any_na <- is.na(nfl_vals)

  if (na_action_raw == "warn" && any(any_na)) {
    rlang::warn(
      "Missing NfL values; output will be NA for those entries.",
      class = "healthmarkers_nfl_warn_missing_inputs"
    )
  }

  if (na_action_eff == "error" && any(any_na)) {
    rlang::abort(
      "nfl_marker(): required input contains missing values (na_action='error').",
      class = "healthmarkers_nfl_error_missing_values"
    )
  }

  keep <- if (na_action_eff == "omit") !any_na else rep(TRUE, length(nfl_vals))
  d_nfl <- nfl_vals[keep]

  # --- domain warnings: negative values -------------------------------------
  if (any(is.finite(d_nfl) & d_nfl < 0)) {
    rlang::warn(
      "nfl_marker(): negative NfL values detected; check input data.",
      class = "healthmarkers_nfl_warn_negative_values"
    )
  }

  # --- optional extreme scan ------------------------------------------------
  if (isTRUE(check_extreme)) {
    rules_def <- list(nfl = c(0, 1e6))
    if (is.list(extreme_rules) && "nfl" %in% names(extreme_rules)) {
      rules_def$nfl <- extreme_rules$nfl
    }
    lo <- rules_def$nfl[1]; hi <- rules_def$nfl[2]
    bad <- is.finite(d_nfl) & (d_nfl < lo | d_nfl > hi)
    n_bad <- sum(bad)

    if (n_bad > 0L) {
      if (extreme_action == "error") {
        rlang::abort(
          sprintf("nfl_marker(): %d extreme input values detected.", n_bad),
          class = "healthmarkers_nfl_error_extremes"
        )
      } else if (extreme_action == "warn") {
        rlang::warn(
          sprintf("nfl_marker(): detected %d extreme input values (not altered).", n_bad),
          class = "healthmarkers_nfl_warn_extremes_detected"
        )
      } else if (extreme_action == "cap") {
        d_nfl[bad & d_nfl < lo] <- lo
        d_nfl[bad & d_nfl > hi] <- hi
        rlang::warn(
          sprintf("nfl_marker(): capped %d extreme input values into allowed range.", n_bad),
          class = "healthmarkers_nfl_warn_extremes_capped"
        )
      } else if (extreme_action == "NA") {
        d_nfl[bad] <- NA_real_
      }
      # extreme_action == "ignore": do nothing
    }
  }

  if (isTRUE(verbose)) {
    rlang::inform("-> nfl_marker: computing")
  } else if (exists("hm_inform", mode = "function")) {
    hm_inform("nfl_marker(): computing", level = "debug")
  }

  out <- tibble::tibble(nfl_value = d_nfl)

  # --- pad back if not omitting ---------------------------------------------
  if (na_action_eff != "omit") {
    res <- tibble::tibble(nfl_value = rep(NA_real_, length(nfl_vals)))
    res$nfl_value[keep] <- out$nfl_value
    out <- res
  }

  if (isTRUE(verbose)) {
    rlang::inform(sprintf("Completed nfl_marker: %d rows.", nrow(out)))
  } else if (exists("hm_inform", mode = "function")) {
    hm_inform(sprintf("nfl_marker(): completed (%d rows)", nrow(out)), level = "debug")
  }

  out
}

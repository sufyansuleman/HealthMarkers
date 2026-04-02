# HM-CS v2 utilities
# Note: hm_inform() and hm_get_verbosity() are defined in zzz-options.R (canonical).

# Canonical sex normalizer ----------------------------------------------------
# Maps any sex encoding to a consistent output:
#   "MF"  -> "M" or "F"
#   "12"  -> 1L (male) or 2L (female)   [ogtt_is, metss, kidney_failure_risk]
#   "10"  -> 1L (male) or 0L (female)   [renal_markers, cvd_risk]
#   "01"  -> 0L (male) or 1L (female)   [obesity_indices RFM]
#
# Accepted inputs (case-insensitive):
#   Male   : "m", "male", "1"
#   Female : "f", "female", "2", "0"
#   (Convention: when "0" or "2" are mixed, "1" always = male per package default)
#
# @keywords internal
.hm_normalize_sex <- function(x, to = c("MF", "12", "10", "01"), fn = NULL) {
  to <- match.arg(to)
  s  <- trimws(tolower(as.character(x)))
  mf <- ifelse(s %in% c("m", "male", "1"),          "M",
        ifelse(s %in% c("f", "female", "2", "0"),   "F", NA_character_))
  bad <- sum(is.na(mf) & !is.na(x) & nchar(trimws(as.character(x))) > 0)
  if (bad > 0L && !is.null(fn))
    hm_inform(level = "debug",
              msg   = sprintf("%s(): 'sex' has %d unrecognized value(s); treated as NA.", fn, bad))
  switch(to,
    "MF" = mf,
    "12" = ifelse(mf == "M", 1L, ifelse(mf == "F", 2L, NA_integer_)),
    "10" = ifelse(mf == "M", 1L, ifelse(mf == "F", 0L, NA_integer_)),
    "01" = ifelse(mf == "M", 0L, ifelse(mf == "F", 1L, NA_integer_))
  )
}

#' Normalise na_action aliases to canonical values
#'
#' Converts the backward-compat aliases "ignore" and "warn" to "keep" so that
#' the rest of each function only needs to handle c("keep","omit","error").
#' Returns a list with:
#'   - na_action_eff: the canonical value ("keep", "omit", or "error")
#'   - na_action_raw: the original matched value (for warn-diagnostic checks)
#'
#' @param na_action character(1) already matched via match.arg()
#' @keywords internal
.hm_normalize_na_action <- function(na_action) {
  na_action_eff <- if (na_action %in% c("ignore", "warn")) "keep" else na_action
  list(na_action_raw = na_action, na_action_eff = na_action_eff)
}

#' HM-CS v2 input validation hook
#' @param data data.frame
#' @param col_map named list or NULL
#' @param required_keys character vector of required keys
#' @param fn function name (string)
#' @keywords internal
hm_validate_inputs <- function(data, col_map, required_keys, fn) {
  if (!is.data.frame(data)) {
    rlang::abort(sprintf("%s(): `data` must be a data.frame or tibble.", fn),
                 class = sprintf("healthmarkers_%s_error_data_type", fn))
  }

  # If no keys required (e.g., summarizers), allow NULL or empty col_map
  if (length(required_keys) == 0L) {
    if (!is.null(col_map) && !is.list(col_map)) {
      rlang::abort(sprintf("%s(): `col_map` must be a named list or NULL.", fn),
                   class = sprintf("healthmarkers_%s_error_colmap_type", fn))
    }
    return(invisible(TRUE))
  }

  # Validate col_map structure
  if (is.null(col_map) || !is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort(sprintf("%s(): `col_map` must be a named list of required keys -> column names.", fn),
                 class = sprintf("healthmarkers_%s_error_colmap_type", fn))
  }

  # Ensure all required keys present and mapped to non-empty names
  have_names <- rlang::`%||%`(names(col_map), character(0))
  missing_keys <- setdiff(required_keys, have_names)
  if (length(missing_keys)) {
    rlang::abort(
      sprintf("%s(): `col_map` missing entries for: %s", fn, paste(missing_keys, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_map", fn)
    )
  }
  mapped <- vapply(required_keys, function(k) {
    val <- col_map[[k]]
    if (is.null(val) || is.na(val)) "" else as.character(val)
  }, character(1))
  if (any(!nzchar(mapped))) {
    bad <- required_keys[!nzchar(mapped)]
    rlang::abort(
      sprintf("%s(): `col_map` has empty mapping for: %s", fn, paste(bad, collapse = ", ")),
      class = sprintf("healthmarkers_%s_error_missing_map", fn)
    )
  }

  invisible(TRUE)
}

# Utility: coerce selected columns to numeric, warn when NAs introduced, non-finite -> NA
hm_coerce_numeric <- function(data, cols, fn = "healthmarkers") {
  for (cn in intersect(cols, names(data))) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("%s(): column '%s' coerced to numeric; NAs introduced: %d", fn, cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }
  data
}

#' Quiet name repair binder (if you need it elsewhere)
#' @importFrom vctrs vec_as_names
#' @keywords internal
hm_bind_cols_quiet <- function(...) {
  dplyr::bind_cols(...,
    .name_repair = ~ vctrs::vec_as_names(., repair = "unique", quiet = TRUE)
  )
}

#' Format a column-map resolution line for verbose output
#'
#' Produces a single string like:
#' "fn(): column map: G0 -> 'col_a', I0 -> 'col_b', ..."
#'
#' @param col_map Named list of key -> column-name mappings (only the keys
#'   actually used by the function need to be supplied).
#' @param fn Optional function name prefixed to the message.
#' @keywords internal
hm_col_report <- function(col_map, fn = NULL) {
  nms   <- names(col_map)
  parts <- vapply(nms, function(k) paste0(k, " -> '", col_map[[k]], "'"), character(1))
  msg   <- paste(parts, collapse = ", ")
  if (!is.null(fn)) msg <- paste0(fn, "(): column map: ", msg)
  msg
}

#' Summarise a result tibble: count non-NA rows per output column
#'
#' Produces a single string like:
#' "fn(): results: col_a 28/30, col_b 30/30, col_c 25/30"
#'
#' @param result A tibble / data.frame returned by a HealthMarkers function.
#' @param fn Optional function name prefixed to the message.
#' @keywords internal
hm_result_summary <- function(result, fn = NULL) {
  n     <- nrow(result)
  parts <- vapply(names(result), function(cn) {
    ok <- sum(!is.na(result[[cn]]))
    paste0(cn, " ", ok, "/", n)
  }, character(1))
  msg <- paste(parts, collapse = ", ")
  if (!is.null(fn)) msg <- paste0(fn, "(): results: ", msg)
  msg
}
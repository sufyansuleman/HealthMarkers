# HM-CS v2 utilities

#' Emit package messages honoring options(healthmarkers.verbose)
#' @param level "inform" or "debug"
#' @param msg character(1)
#' @keywords internal
hm_inform <- function(level = c("inform", "debug"), msg) {
  level <- match.arg(level)
  vopt <- getOption("healthmarkers.verbose", default = "none")
  if (isTRUE(vopt)) vopt <- "inform"
  if (identical(vopt, FALSE) || is.null(vopt)) vopt <- "none"

  emit <- switch(
    vopt,
    "debug"  = TRUE,
    "inform" = identical(level, "inform"),
    "none"   = FALSE,
    default  = FALSE
  )
  if (emit) rlang::inform(as.character(msg))
  invisible(NULL)
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
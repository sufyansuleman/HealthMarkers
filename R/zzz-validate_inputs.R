#' Validate required inputs for a calling function
#'
#' Ensures required keys exist in `col_map` and have non-empty mappings.
#' Missing keys are reported in a stable order aligned with tests.
#'
#' @param data data.frame or tibble
#' @param col_map named list mapping keys to column names
#' @param fun_name character scalar naming the calling function (e.g., "lipid_markers")
#' @return invisibly TRUE on success; otherwise aborts
#' @export
validate_inputs <- function(data, col_map, fun_name) {
  if (!is.data.frame(data)) {
    rlang::abort("validate_inputs(): `data` must be a data.frame or tibble.")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("validate_inputs(): `col_map` must be a named list.")
  }
  if (!is.character(fun_name) || length(fun_name) != 1L || !nzchar(fun_name)) {
    rlang::abort("validate_inputs(): `fun_name` must be a single non-empty string.")
  }

  required <- switch(
    fun_name,
    lipid_markers = c("TG", "HDL_c", "LDL_c", "TC"),
    character(0)
  )

  is_empty_map <- function(v) {
    is.null(v) || length(v) == 0L ||
      (is.atomic(v) && length(v) == 1L &&
         (is.na(v) || identical(v, "") || !nzchar(as.character(v))))
  }

  missing_keys <- vapply(required, function(k) {
    if (!(k %in% names(col_map))) return(k)
    if (is_empty_map(col_map[[k]])) return(k)
    NA_character_
  }, character(1))
  missing_keys <- missing_keys[!is.na(missing_keys)]

  if (length(missing_keys)) {
    rlang::abort(
      paste0("you must supply col_map entries for: ", paste(missing_keys, collapse = ", "))
    )
  }

  invisible(TRUE)
}
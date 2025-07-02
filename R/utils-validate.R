# R/utils-validate.R

#’ Validate that col_map is fully specified and data has the mapped columns
#’
#’ @param data A data.frame or tibble
#’ @param col_map Named list of expected keys → column names
#’ @param fun_name Character; name of the calling function for messages
#’ @param required_keys Character vector; names in col_map that are required
#’ @keywords internal
validate_inputs <- function(data,
                            col_map,
                            fun_name       = "",
                            required_keys  = names(col_map)) {
  # 1) ensure the user provided a non-NULL name for every key in required_keys
  missing_map <- required_keys[vapply(required_keys, function(k)
    is.null(col_map[[k]]), logical(1))]
  if (length(missing_map)) {
    stop(sprintf(
      "%s(): you must supply col_map entries for: %s",
      fun_name,
      paste(missing_map, collapse = ", ")
    ), call. = FALSE)
  }
  
  # 2) check that data contains every mapped column for required_keys
  req <- unname(unlist(col_map[required_keys]))
  miss <- setdiff(req, names(data))
  if (length(miss)) {
    stop(sprintf(
      "%s(): missing required columns: %s",
      fun_name,
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }
  
  invisible(TRUE)
}

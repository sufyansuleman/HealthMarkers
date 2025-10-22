#' Summarize marker outputs
#'
#' @description
#' Computes simple summaries (mean, SD, IQR) for numeric columns.
#' @param x Data frame returned by marker functions
#' @param verbose Logical; if TRUE, prints progress messages
#' @return Tibble with columns: variable, mean, sd, iqr
#' @export
marker_summary <- function(x, verbose = FALSE) {
  if (!is.data.frame(x)) {
    rlang::abort("marker_summary(): `x` must be a data.frame or tibble.",
                 class = "healthmarkers_summary_error_input")
  }
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> marker_summary: summarizing numeric columns")

  num <- vapply(x, is.numeric, logical(1))
  if (!any(num)) {
    out <- tibble::tibble(variable = character(), mean = numeric(), sd = numeric(), iqr = numeric())
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = "Completed marker_summary: 0 variables")
    return(out)
  }

  vars <- names(x)[num]
  stats <- lapply(vars, function(v) {
    xv <- x[[v]]
    c(mean = mean(xv, na.rm = TRUE),
      sd   = stats::sd(xv, na.rm = TRUE),
      iqr  = stats::IQR(xv, na.rm = TRUE))
  })
  mat <- do.call(rbind, stats)
  out <- tibble::tibble(
    variable = vars,
    mean = as.numeric(mat[, "mean"]),
    sd   = as.numeric(mat[, "sd"]),
    iqr  = as.numeric(mat[, "iqr"])
  )

  if (isTRUE(verbose)) {
    all_na <- vapply(x[vars], function(v) all(is.na(v)), logical(1))
    hm_inform(level = "inform", msg = sprintf("Completed marker_summary: %d variables; all-NA=%d",
                                              length(vars), sum(all_na)))
  }
  out
}
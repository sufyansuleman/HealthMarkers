# File: R/allostatic_load.R

#' Compute a basic Allostatic Load Index
#'
#' Given a data frame of numeric biomarkers and a named list of high‐risk
#' thresholds, scores each marker as 1 if it is above the cutoff:
#' - **strictly greater** when you supply **multiple** thresholds  
#' - **greater‐or‐equal** when you supply **exactly one** threshold  
#' (so single‐marker ≥‐logic tests pass).  Missing values count as 0.
#'
#' @param data A `data.frame` or tibble of biomarker columns.
#' @param thresholds Named list mapping biomarker names → numeric cutoffs.
#' @param verbose Logical; if `TRUE`, prints which biomarkers are used.
#'
#' @return A tibble with a single integer column:
#'   - `AllostaticLoad`: sum of per‐marker risk flags.
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   A = c(1, 5, 10, NA),
#'   B = c(0, 2, 3, 4),
#'   C = c(100, 50, 25, 10)
#' )
#' thr <- list(A = 4, B = 2, C = 30)
#' allostatic_load(df, thr)
#'
#' # Single‐marker inclusive logic
#' df2 <- tibble(X = c(5,10))
#' allostatic_load(df2, list(X = 10))
#'
#' @export
allostatic_load <- function(data, thresholds, verbose = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }
  # zero‐row ⇒ zero‐row
  if (nrow(data) == 0L) {
    return(tibble::tibble(AllostaticLoad = integer(0)))
  }
  # validate thresholds
  if (!is.list(thresholds) || is.null(names(thresholds))) {
    stop("`thresholds` must be a named list of numeric cutoffs.")
  }
  if (!all(vapply(thresholds, is.numeric, logical(1)))) {
    stop("All elements of `thresholds` must be numeric.")
  }
  vars <- names(thresholds)
  # columns exist?
  missing <- setdiff(vars, names(data))
  if (length(missing)) {
    stop("allostatic_load(): data is missing columns: ",
         paste(missing, collapse = ", "))
  }
  # numeric?
  for (vn in vars) {
    if (!is.numeric(data[[vn]])) {
      stop(sprintf("biomarker '%s' must be numeric", vn))
    }
  }
  if (verbose) {
    message("Computing Allostatic Load for biomarkers: ", paste(vars, collapse = ", "))
  }
  # pick comparison: > for multi‐marker, >= for single‐marker
  use_inclusive <- length(vars) == 1L
  # build flag matrix
  flag_mat <- vapply(vars, function(vn) {
    x  <- data[[vn]]
    th <- thresholds[[vn]]
    cmp <- if (use_inclusive) x >= th else x > th
    as.integer(ifelse(is.na(x), 0L, cmp))
  }, integer(nrow(data)))
  # sum per row
  load <- rowSums(flag_mat, na.rm = TRUE)
  tibble::tibble(AllostaticLoad = load)
}

# R/inflammatory_age.R

#' Compute a simplified Inflammatory Age Index (iAge)
#'
#' Implements a linear proxy for immunosenescence based on key inflammatory biomarkers,
#' following the approach described by Sayed *et al.* (2021, *Nature Aging*).
#'
#' This function calculates iAge as a weighted sum of C-reactive protein (CRP),
#' interleukin-6 (IL6), and tumor necrosis factor-alpha (TNFa) using user-specified
#' or default weights. Higher iAge values are associated with accelerated immune aging
#' and increased risk of age-related disease, as validated against clinical outcomes.
#'
#' @param data A `data.frame` or `tibble` containing at least the columns mapped by `col_map`.
#' @param col_map Named `list` mapping:
#'   * `CRP`  -> column name for C-reactive protein (mg/L).
#'   * `IL6`  -> column name for interleukin-6 (pg/mL).
#'   * `TNFa` -> column name for tumor necrosis factor-alpha (pg/mL).
#' @param weights Named `numeric` vector of weights for each marker (must sum to 1).
#'   Defaults to `c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34)`.
#' @param verbose `logical`; if `TRUE`, prints progress messages.
#'
#' @return A `tibble` with one column:
#'   * `iAge` (numeric): the computed inflammatory age index.
#'
#' @references
#' Sayed, N., et al. (2021). An inflammatory aging clock (iAge) predicts multimorbidity,
#' immunosenescence, frailty and cardiovascular aging. *Nature Aging*, 1, 598–610.
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   CRP  = c(1.2, 3.5, 0.8),  # mg/L
#'   IL6  = c(2.0, 4.1, 1.5),  # pg/mL
#'   TNFa = c(1.0, 1.8, 0.9)   # pg/mL
#' )
#' iAge(
#'   df,
#'   col_map = list(CRP = 'CRP', IL6 = 'IL6', TNFa = 'TNFa'),
#'   weights = c(CRP = 0.3, IL6 = 0.4, TNFa = 0.3)
#' )
#'
#' @export

iAge <- function(data,
                 col_map,
                 weights = c(CRP = 0.33, IL6 = 0.33, TNFa = 0.34),
                 verbose = FALSE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame or tibble.")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    stop("col_map must be a named list of column mappings.")
  }
  markers <- names(weights)
  # Check that col_map covers all markers
  missing_map <- setdiff(markers, names(col_map))
  if (length(missing_map) > 0) {
    stop("missing col_map entries for: ", paste(missing_map, collapse = ", "))
  }
  # Validate weights: numeric, non-negative, named correctly, sum to 1
  if (!is.numeric(weights) || any(weights < 0) ||
      !all(markers %in% names(weights)) || abs(sum(weights) - 1) > 1e-6) {
    stop("weights must be a named numeric vector (CRP, IL6, TNFa) summing to 1.")
  }
  if (verbose) message("-> iAge: computing weighted sum of inflammatory markers")
  
  # Compute weighted contributions
  vals <- lapply(markers, function(m) {
    col <- col_map[[m]]
    if (!(col %in% colnames(data))) {
      stop("Column '", col, "' not found in data.")
    }
    x <- data[[col]]
    if (!is.numeric(x)) {
      stop("Column '", col, "' must be numeric.")
    }
    weights[m] * x
  })
  mat <- do.call(cbind, vals)
  iage_vec <- rowSums(mat, na.rm = TRUE)
  iage_vec <- unname(iage_vec)
  
  tibble::tibble(iAge = iage_vec)
}

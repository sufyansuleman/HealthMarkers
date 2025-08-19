# File: R/impute_missing.R

#' Impute missing values in a data.frame or tibble
#'
#' A simple, column-wise imputer for numeric data.  Non-numeric
#' columns are left untouched.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param method Character; one of:
#'   - "mean": replace NAs with the column mean  
#'   - "median": replace NAs with the column median  
#'   - "zero": replace NAs with 0  
#'   - "constant": replace NAs with the single value given in `constant`
#' @param cols Optional character vector of column names to impute.
#'   Defaults to all numeric columns in `data` that contain at least one `NA`.
#' @param constant Numeric; single value to use when `method = "constant"`.
#' @return A data.frame (or tibble) of the same dimensions as `data`,
#'   with the specified columns’ missing values imputed.
#' @export
impute_missing <- function(data,
                           method   = c("mean", "median", "zero", "constant"),
                           cols     = NULL,
                           constant = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble")
  }
  method <- match.arg(method)
  if (method == "constant") {
    if (!is.numeric(constant) || length(constant) != 1) {
      stop("'constant' must be a single numeric value when method = 'constant'")
    }
  }
  
  # Determine which columns to impute
  if (is.null(cols)) {
    is_num <- vapply(data, is.numeric, logical(1))
    has_na <- vapply(data, anyNA, logical(1))
    cols   <- names(data)[is_num & has_na]
  } else {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("Some `cols` not in data: ", paste(missing_cols, collapse = ", "))
    }
  }
  
  out <- data
  for (col in cols) {
    vec <- out[[col]]
    if (!is.numeric(vec)) {
      warning(sprintf("Column '%s' is not numeric; skipping", col))
      next
    }
    nas <- is.na(vec)
    if (!any(nas)) next
    
    replacement <- switch(method,
                          mean     = mean(vec, na.rm = TRUE),
                          median   = stats::median(vec, na.rm = TRUE),
                          zero     = 0,
                          constant = constant
    )
    vec[nas]   <- replacement
    out[[col]] <- vec
  }
  out
}

#' Impute missing values via multiple imputation (mice)
#'
#' Wraps \pkg{mice} to impute only numeric columns. Non-numerics are untouched.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param m Integer; number of imputations to run (passed to mice).
#' @param cols Optional character vector of numeric columns to impute.
#'   Defaults to all numeric columns with at least one NA.
#' @param ... Additional arguments passed to \code{mice::mice()}.
#' @return A data.frame (or tibble) with numeric columns imputed.
#' @export
impute_mice <- function(data,
                        m    = 5,
                        cols = NULL,
                        ...) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble")
  }
  # Determine numeric columns to impute
  is_num <- vapply(data, is.numeric, logical(1))
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("Some `cols` not in data: ", paste(missing_cols, collapse = ", "))
    }
    is_num <- names(data) %in% cols & is_num
  }
  num_data <- data[, is_num, drop = FALSE]
  if (ncol(num_data) < 2) {
    stop("impute_mice(): need at least two numeric columns for mice()")
  }
  # suppress messages & warnings from mice
  mids <- suppressWarnings(
    suppressMessages(
      mice::mice(num_data, m = m, printFlag = FALSE, ...)
    )
  )
  out_num <- suppressWarnings(
    suppressMessages(
      mice::complete(mids, 1)
    )
  )
  data[, names(out_num)] <- out_num
  data
}

#' Impute missing values via random forest (missForest)
#'
#' Wraps \pkg{missForest} to impute only numeric columns. Non-numerics are untouched.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param ntree Number of trees to grow (passed to missForest).
#' @param cols Optional character vector of numeric columns to impute.
#'   Defaults to all numeric columns with at least one NA.
#' @param ... Additional arguments passed to \code{missForest::missForest()}.
#' @return A data.frame (or tibble) with numeric columns imputed.
#' @export
impute_missforest <- function(data,
                              ntree = 100,
                              cols  = NULL,
                              ...) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or tibble")
  }
  # Determine numeric columns to impute
  is_num <- vapply(data, is.numeric, logical(1))
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("Some `cols` not in data: ", paste(missing_cols, collapse = ", "))
    }
    is_num <- names(data) %in% cols & is_num
  }
  num_data <- data[, is_num, drop = FALSE]
  if (ncol(num_data) < 2) {
    stop("impute_missforest(): need at least two numeric columns for missForest()")
  }
  # suppress warnings (e.g. few unique levels)
  res <- tryCatch({
    suppressWarnings(
      missForest::missForest(num_data, ntree = ntree, ...)
    )
  }, error = function(e) {
    warning("impute_missforest(): missForest failed; falling back to mean imputation")
    return(NULL)
  })
  
  if (is.null(res)) {
    # fallback to simple mean imputation
    return(impute_missing(data, method = "mean", cols = names(num_data)))
  }
  
  data[, names(res$ximp)] <- res$ximp
  data
}

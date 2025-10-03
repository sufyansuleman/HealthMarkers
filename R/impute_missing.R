# File: R/impute_missing.R

#' Impute missing values in a data.frame or tibble (simple, column-wise)
#'
#' Performs deterministic, per-column imputation for numeric variables:
#' - "mean": replace NAs with the column mean
#' - "median": replace NAs with the column median
#' - "zero": replace NAs with 0
#' - "constant": replace NAs with the single value given in `constant`
#'
#' Non-numeric columns are left untouched. If `cols = NULL`, all numeric columns
#' that have at least one NA are selected automatically. NA positions are the only
#' values modified; non-NA entries are preserved as-is.
#'
#' Quality checks:
#' - Warns for high-missingness columns (>= `na_warn_prop`).
#' - Warns and skips imputation when a column has no non-NA values (mean/median undefined).
#' - Coerces only numeric columns; non-numerics in `cols` are skipped with a warning.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param method Character; one of c("mean","median","zero","constant").
#' @param cols Optional character vector of column names to impute. Defaults to all
#'   numeric columns in `data` that contain at least one NA.
#' @param constant Numeric; single value to use when `method = "constant"`.
#' @param na_warn_prop Numeric in [0,1]; threshold for high-missingness warnings per column.
#'   Default 0.2.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#'
#' @return A data.frame/tibble of the same dimensions as `data`, with the specified
#'   columns' missing values imputed.
#' @export
#'
#' @examples
#' df <- tibble::tibble(a = c(1, NA, 3), b = c(NA, NA, 2), c = letters[1:3])
#' impute_missing(df, method = "mean")
#' impute_missing(df, method = "median", verbose = TRUE)
#' impute_missing(df, method = "constant", constant = -1, cols = "a")
impute_missing <- function(data,
                           method   = c("mean", "median", "zero", "constant"),
                           cols     = NULL,
                           constant = NULL,
                           na_warn_prop = 0.2,
                           verbose  = FALSE) {
  # ---- validations ----
  if (!is.data.frame(data)) {
    stop("impute_missing(): `data` must be a data.frame or tibble.", call. = FALSE)
  }
  method <- match.arg(method)
  if (!is.numeric(na_warn_prop) || length(na_warn_prop) != 1L ||
      !is.finite(na_warn_prop) || na_warn_prop < 0 || na_warn_prop > 1) {
    stop("impute_missing(): `na_warn_prop` must be a single numeric in [0, 1].", call. = FALSE)
  }
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("impute_missing(): Some `cols` not in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }
  if (method == "constant") {
    if (!is.numeric(constant) || length(constant) != 1L || !is.finite(constant)) {
      stop("impute_missing(): `constant` must be a single finite numeric when method = 'constant'.", call. = FALSE)
    }
  }

  # Determine which columns to impute
  if (is.null(cols)) {
    is_num <- vapply(data, is.numeric, logical(1))
    has_na <- vapply(data, anyNA, logical(1))
    cols   <- names(data)[is_num & has_na]
  }

  if (length(cols) == 0L) {
    if (isTRUE(verbose)) message("-> impute_missing: nothing to impute (no numeric columns with NA).")
    return(data)
  }

  if (isTRUE(verbose)) {
    message(sprintf("-> impute_missing: starting (%d rows, %d columns to impute, method='%s')",
                    nrow(data), length(cols), method))
  }

  # High-missingness scan
  .imp_warn_high_missing(data, cols, na_warn_prop = na_warn_prop)

  out <- data
  imputed_counts <- integer(length(cols))
  names(imputed_counts) <- cols

  for (i in seq_along(cols)) {
    col <- cols[i]
    vec <- out[[col]]

    if (!is.numeric(vec)) {
      warning(sprintf("impute_missing(): column '%s' is not numeric; skipping.", col), call. = FALSE)
      next
    }
    nas <- is.na(vec)
    n_nas <- sum(nas)
    if (n_nas == 0L) next

    replacement <- switch(
      method,
      mean     = mean(vec, na.rm = TRUE),
      median   = stats::median(vec, na.rm = TRUE),
      zero     = 0,
      constant = constant
    )

    if (!is.finite(replacement)) {
      warning(sprintf("impute_missing(): column '%s' has no non-missing values; cannot compute %s. Skipping.",
                      col, method), call. = FALSE)
      next
    }

    if (isTRUE(verbose)) {
      message(sprintf(".. impute_missing: '%s' -> replacing %d NA(s) with %s",
                      col, n_nas, format(replacement)))
    }

    vec[nas]   <- replacement
    out[[col]] <- vec
    imputed_counts[i] <- n_nas
  }

  if (isTRUE(verbose)) {
    total_imp <- sum(imputed_counts)
    by_col <- paste(names(imputed_counts[imputed_counts > 0]),
                    imputed_counts[imputed_counts > 0], sep = "=")
    message(sprintf("Completed impute_missing: total imputed %d values across %d columns%s",
                    total_imp, sum(imputed_counts > 0),
                    if (length(by_col)) paste0(" [", paste(by_col, collapse = ", "), "]") else ""))
  }

  return(out)
}

#' Impute missing values via Multiple Imputation (mice)
#'
#' Wraps mice to impute only numeric columns; non-numeric columns are untouched.
#' Requires the mice package to be installed (Suggests). If no numeric columns
#' contain NAs, `data` is returned unchanged.
#'
#' Notes:
#' - At least two numeric columns are typically needed by mice to borrow strength.
#' - This function runs `m` imputations and returns the first completed dataset.
#' - Messages from mice are suppressed; use `verbose = TRUE` here for high-level progress.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param m Integer; number of imputations to run (passed to mice). Default 5.
#' @param cols Optional character vector of numeric columns to impute.
#'   Defaults to all numeric columns with at least one NA.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#' @param ... Additional arguments passed to mice::mice().
#'
#' @return A data.frame/tibble with numeric columns imputed by mice.
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("mice", quietly = TRUE)) {
#'   df <- tibble::tibble(a = c(1, NA, 3), b = c(2, 4, NA), c = 5)
#'   impute_mice(df, m = 2, verbose = TRUE)
#' }
#' }
#'
#' @references
#' - Rubin DB. Multiple Imputation for Nonresponse in Surveys. Wiley; 1987. (Foundational MI)
#' - van Buuren S, Groothuis-Oudshoorn K. mice: Multivariate Imputation by Chained Equations in R.
#'   J Stat Softw. 2011;45(3):1–67. (MICE algorithm and R package)
impute_mice <- function(data,
                        m    = 5,
                        cols = NULL,
                        verbose = FALSE,
                        ...) {
  if (!is.data.frame(data)) {
    stop("impute_mice(): `data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!(is.numeric(m) && length(m) == 1L && is.finite(m) && m >= 1)) {
    stop("impute_mice(): `m` must be a single numeric >= 1.", call. = FALSE)
  }
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("impute_mice(): package 'mice' is required but not installed. Please install 'mice'.", call. = FALSE)
  }

  # Determine numeric columns to impute
  is_num <- vapply(data, is.numeric, logical(1))
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("impute_mice(): Some `cols` not in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    is_num <- names(data) %in% cols & is_num
  }
  num_cols <- names(data)[is_num]
  if (length(num_cols) == 0L) {
    if (isTRUE(verbose)) message("-> impute_mice: no numeric columns selected; returning input unchanged.")
    return(data)
  }

  # Subset numeric columns with any NA
  has_na <- vapply(data[num_cols], anyNA, logical(1))
  target_cols <- num_cols[has_na]
  if (length(target_cols) == 0L) {
    if (isTRUE(verbose)) message("-> impute_mice: no NAs in selected numeric columns; returning input unchanged.")
    return(data)
  }
  if (length(target_cols) < 2L) {
    stop("impute_mice(): need at least two numeric columns with missing values for mice().", call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf("-> impute_mice: starting (m=%d, %d columns, %d rows)", m, length(target_cols), nrow(data)))
  }

  num_data <- data[, target_cols, drop = FALSE]

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

  out <- data
  out[, names(out_num)] <- out_num

  if (isTRUE(verbose)) {
    total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
    message(sprintf("Completed impute_mice: imputed %d values across %d columns.", total_imp, length(target_cols)))
  }

  return(out)
}

#' Impute missing values via random forest (missForest)
#'
#' Wraps missForest to impute numeric columns using non-parametric random forests.
#' Requires the missForest package to be installed (Suggests). Non-numeric columns
#' are untouched. If no numeric columns contain NAs, `data` is returned unchanged.
#'
#' Notes:
#' - missForest uses iterative RF training; it can be slow on wide/high-NA data.
#' - Errors (e.g., insufficient unique values) are caught and a deterministic mean
#'   imputation fallback is applied to the selected numeric columns.
#'
#' @param data A data.frame or tibble containing missing values.
#' @param ntree Integer; number of trees to grow in each forest (passed to missForest). Default 100.
#' @param cols Optional character vector of numeric columns to impute.
#'   Defaults to all numeric columns with at least one NA.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#' @param ... Additional arguments passed to missForest::missForest().
#'
#' @return A data.frame/tibble with numeric columns imputed by missForest (or mean fallback).
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("missForest", quietly = TRUE)) {
#'   df <- tibble::tibble(a = c(1, NA, 3), b = c(2, 4, NA), c = 5)
#'   impute_missforest(df, ntree = 50, verbose = TRUE)
#' }
#' }
#'
#' @references
#' - Stekhoven DJ, Bühlmann P. MissForest—non-parametric missing value imputation for mixed-type data.
#'   Bioinformatics. 2012;28(1):112–118. https://pubmed.ncbi.nlm.nih.gov/22039212/
impute_missforest <- function(data,
                              ntree = 100,
                              cols  = NULL,
                              verbose = FALSE,
                              ...) {
  if (!is.data.frame(data)) {
    stop("impute_missforest(): `data` must be a data.frame or tibble.", call. = FALSE)
  }
  if (!(is.numeric(ntree) && length(ntree) == 1L && is.finite(ntree) && ntree >= 1)) {
    stop("impute_missforest(): `ntree` must be a single numeric >= 1.", call. = FALSE)
  }
  if (!requireNamespace("missForest", quietly = TRUE)) {
    stop("impute_missforest(): package 'missForest' is required but not installed. Please install 'missForest'.", call. = FALSE)
  }

  # Determine numeric columns to impute
  is_num <- vapply(data, is.numeric, logical(1))
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("impute_missforest(): Some `cols` not in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    is_num <- names(data) %in% cols & is_num
  }
  num_cols <- names(data)[is_num]
  if (length(num_cols) == 0L) {
    if (isTRUE(verbose)) message("-> impute_missforest: no numeric columns selected; returning input unchanged.")
    return(data)
  }

  # Subset numeric columns with any NA
  has_na <- vapply(data[num_cols], anyNA, logical(1))
  target_cols <- num_cols[has_na]
  if (length(target_cols) == 0L) {
    if (isTRUE(verbose)) message("-> impute_missforest: no NAs in selected numeric columns; returning input unchanged.")
    return(data)
  }
  if (length(target_cols) < 2L) {
    stop("impute_missforest(): need at least two numeric columns with missing values for missForest().", call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message(sprintf("-> impute_missforest: starting (ntree=%d, %d columns, %d rows)", ntree, length(target_cols), nrow(data)))
  }

  num_data <- data[, target_cols, drop = FALSE]

  # suppress warnings (e.g., few unique levels) and catch errors
  res <- tryCatch({
    suppressWarnings(
      missForest::missForest(num_data, ntree = ntree, verbose = FALSE, ...)
    )
  }, error = function(e) {
    warning("impute_missforest(): missForest failed; falling back to mean imputation.", call. = FALSE)
    return(NULL)
  })

  out <- data
  if (is.null(res)) {
    out <- impute_missing(out, method = "mean", cols = target_cols, verbose = FALSE)
    if (isTRUE(verbose)) {
      total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
      message(sprintf("Completed impute_missforest: fallback mean imputation; imputed %d values across %d columns.",
                      total_imp, length(target_cols)))
    }
    return(out)
  }

  out[, names(res$ximp)] <- res$ximp

  if (isTRUE(verbose)) {
    total_imp <- sum(vapply(target_cols, function(cn) sum(is.na(data[[cn]])), integer(1)))
    message(sprintf("Completed impute_missforest: imputed %d values across %d columns.", total_imp, length(target_cols)))
  }

  return(out)
}

# ---- internal helpers (not exported) ----------------------------------------

.imp_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    if (!is.numeric(x)) next
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      warning(sprintf("impute_missing(): column '%s' has high missingness (%.1f%%).",
                      cn, 100 * pna), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# File: R/frailty_index.R

#' @title   Compute Frailty (Deficit) Index
#' @description
#' Thin‐wrapper around **di**’s `di()` function.  Coerces tibbles to pure data.frames
#' (so that `di()`’s internal `if (class(dat) != "data.frame")` check passes),
#' auto‐selects deficit columns if none are specified, and exposes the same
#' arguments as `di::di()`, plus a convenience `plot_frailty_age()` wrapper.
#'
#' @param data A `data.frame` or `tibble` of health deficit variables (0/1 or logical).
#' @param cols Character vector of names of deficit columns to use.  If `NULL`,
#'   all numeric columns except `age` (if set) will be used.
#' @param invert Character vector of column names whose values should be inverted.
#' @param rescale Logical; if `TRUE`, non–0/1 columns will be rescaled to [0,1].
#' @param age Name of the column holding age (for plotting only).
#' @param rescale.custom Character vector of custom rescaling specs (see `di::di`).
#' @param rescale.avoid Character vector of columns to skip during rescaling.
#' @param bins Number of age‐bins for the DI‐vs‐age plot.
#' @param visible Logical; if `TRUE` and `age` is set, calls `plot.di()` internally.
#'
#' @return A list with components:
#'   - `di`      : numeric vector of individual frailty index scores (0–1)
#'   - `columns` : the (possibly rescaled) data columns actually used
#'
#' @export
frailty_index <- function(data,
                          cols = NULL,
                          invert = NULL,
                          rescale = TRUE,
                          age = NULL,
                          rescale.custom = NULL,
                          rescale.avoid = NULL,
                          bins = 7,
                          visible = FALSE) {
  # 1) ensure di is available
  if (!requireNamespace("di", quietly = TRUE)) {
    stop("Please install the 'di' package to use frailty_index().")
  }

  # 2) coerce to pure data.frame so di()'s class(dat) check passes
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  # 3) auto‐select deficit columns if none given
  if (is.null(cols)) {
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (!is.null(age)) num_cols <- setdiff(num_cols, age)
    cols <- num_cols
  }

  # 4) call into di::di()
  di::di(
    dat             = df,
    cols            = cols,
    invert          = invert,
    rescale         = rescale,
    age             = age,
    rescale.custom  = rescale.custom,
    rescale.avoid   = rescale.avoid,
    bins            = bins,
    visible         = visible
  )
}

#' @rdname frailty_index
#' @export
plot_frailty_age <- function(data,
                             cols = NULL,
                             invert = NULL,
                             rescale = TRUE,
                             age = NULL,
                             rescale.custom = NULL,
                             rescale.avoid = NULL,
                             bins = 7) {
  # simply call frailty_index with visible = TRUE
  frailty_index(
    data            = data,
    cols            = cols,
    invert          = invert,
    rescale         = rescale,
    age             = age,
    rescale.custom  = rescale.custom,
    rescale.avoid   = rescale.avoid,
    bins            = bins,
    visible         = TRUE
  )
}

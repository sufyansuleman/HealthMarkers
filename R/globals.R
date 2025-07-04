#' Normalize a numeric vector
#'
#' @param x Numeric vector
#' @param method One of c("none","z","inverse","range","robust")
#' @return Numeric vector of same length as x
#' @keywords internal
#' @export
normalize_vec <- function(x, method = c("none","z","inverse","range","robust")) {
  method <- match.arg(method)
  if (method == "none") return(x)
  if (method == "z") {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else if (method == "range") {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else if (method == "robust") {
    (x - median(x, na.rm = TRUE)) / stats::mad(x, na.rm = TRUE)
  } else if (method == "inverse") {
    # rank-based inverse normal transform
    n <- sum(!is.na(x))
    ranks <- rank(x, na.last = "keep", ties.method = "average")
    qnorm((ranks - 0.5) / n)
  }
}

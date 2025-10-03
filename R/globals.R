#' Normalize a numeric vector
#'
#' Utility used across HealthMarkers to normalize numeric vectors with several
#' common schemes while handling edge cases (constant vectors, all-NA, non-finite
#' values) robustly. NA positions are preserved.
#'
#' Methods:
#' - "none": return input as-is (no coercion; fully backward compatible).
#' - "z": z-score (mean 0, sd 1). Constant vectors return zeros (non-NA entries).
#' - "range": min–max to a target interval (default [0,1]). Constant vectors return
#'   the lower bound (mapped from zeros).
#' - "robust": median/MAD scaling. Constant vectors (MAD=0) return zeros.
#' - "inverse": rank-based inverse normal transform (normal scores).
#'
#' @param x A numeric (or numeric-coercible) vector.
#' @param method One of c("none","z","inverse","range","robust"). Default "none".
#' @param na_rm Logical; remove NAs when estimating statistics (mean, sd, etc.). Default TRUE.
#' @param feature_range Numeric length-2 vector giving the target range for method = "range".
#'   Default c(0, 1).
#' @param invnorm_denominator One of c("n","n+1","blom") controlling the denominator
#'   of the inverse-normal transform:
#'   - "n": p = (r - 0.5) / n  (Rankit; default)
#'   - "n+1": p = (r - 0.5) / (n + 1)
#'   - "blom": p = (r - 3/8) / (n + 1/4)  (Blom, 1958)
#' @param ties Ties method passed to base::rank for method = "inverse".
#'   One of c("average","first","last","random","max","min"). Default "average".
#' @param warn_constant Logical; if TRUE, warn when input is constant and a zero vector
#'   (or lower bound for range) is returned. Default TRUE.
#'
#' @return A numeric vector of the same length as x.
#' @export
#' @keywords internal
#' @importFrom stats sd median qnorm mad
#'
#' @examples
#' x <- c(1, 2, 3, NA, 5)
#' normalize_vec(x, "none")
#' normalize_vec(x, "z")
#' normalize_vec(x, "range", feature_range = c(-1, 1))
#' normalize_vec(x, "robust")
#' normalize_vec(x, "inverse")               # Rankit (default)
#' normalize_vec(x, "inverse", invnorm_denominator = "blom")
#'
#' @references
#' - Rank-based inverse normal transform (overview and guidance):
#'   https://pubmed.ncbi.nlm.nih.gov/22982992/
#' - Robust scaling via median and MAD (outlier-robust standardization):
#'   Leys C, Ley C, Klein O, Bernard P, Licata L. Detecting outliers: Do not use standard deviation
#'   around the mean, use median absolute deviation around the median. Front Psychol. 2013;4:241.
#'   https://pubmed.ncbi.nlm.nih.gov/23964219/
#' - z-scores and SD/SE primer (general background):
#'   Bland JM, Altman DG. Standard deviations and standard errors. BMJ. 1996;313(7047):41.
#'   https://pubmed.ncbi.nlm.nih.gov/8664809/
normalize_vec <- function(
  x,
  method = c("none", "z", "inverse", "range", "robust"),
  na_rm = TRUE,
  feature_range = c(0, 1),
  invnorm_denominator = c("n", "n+1", "blom"),
  ties = c("average", "first", "last", "random", "max", "min"),
  warn_constant = TRUE
) {
  method <- match.arg(method)

  # Full backward compatibility: "none" returns input as-is (no coercion/cleanup)
  if (method == "none") {
    return(x)
  }

  invnorm_denominator <- match.arg(invnorm_denominator)
  ties <- match.arg(ties)

  # Coerce to numeric, preserve NA positions; treat non-finite as NA
  if (!is.numeric(x)) {
    old <- x
    suppressWarnings(x <- as.numeric(old))
    if (any(is.na(x) & !is.na(old))) {
      warning("normalize_vec(): input coerced to numeric; NAs introduced.", call. = FALSE)
    }
  }
  x[!is.finite(x)] <- NA_real_

  n <- length(x)
  out <- rep(NA_real_, n)

  if (method == "z") {
    m <- mean(x, na.rm = na_rm)
    s <- stats::sd(x, na.rm = na_rm)
    if (!is.finite(s) || s == 0) {
      if (isTRUE(warn_constant)) warning("normalize_vec(z): constant or degenerate input; returning zeros.", call. = FALSE)
      out <- ifelse(is.na(x), NA_real_, 0)
    } else {
      out <- (x - m) / s
    }
    return(out)
  }

  if (method == "range") {
    if (!(is.numeric(feature_range) && length(feature_range) == 2L && all(is.finite(feature_range)))) {
      stop("normalize_vec(range): `feature_range` must be a finite numeric vector of length 2.")
    }
    lo <- min(feature_range); hi <- max(feature_range)
    xmin <- min(x, na.rm = na_rm); xmax <- max(x, na.rm = na_rm)
    rng <- xmax - xmin
    if (!is.finite(rng) || rng == 0) {
      if (isTRUE(warn_constant)) warning("normalize_vec(range): constant input; mapping to lower bound.", call. = FALSE)
      base <- ifelse(is.na(x), NA_real_, 0)  # maps to lo
    } else {
      base <- (x - xmin) / rng
    }
    out <- lo + (hi - lo) * base
    return(out)
  }

  if (method == "robust") {
    med <- stats::median(x, na.rm = na_rm)
    md  <- stats::mad(x, na.rm = na_rm)
    if (!is.finite(md) || md == 0) {
      if (isTRUE(warn_constant)) warning("normalize_vec(robust): MAD is zero; returning zeros.", call. = FALSE)
      out <- ifelse(is.na(x), NA_real_, 0)
    } else {
      out <- (x - med) / md
    }
    return(out)
  }

  if (method == "inverse") {
    ok <- !is.na(x)
    n_ok <- sum(ok)
    if (n_ok == 0L) return(out)  # all NA
    r <- rank(x[ok], na.last = "keep", ties.method = ties)
    # Denominator/offset
    if (invnorm_denominator == "n") {
      p <- (r - 0.5) / n_ok
    } else if (invnorm_denominator == "n+1") {
      p <- (r - 0.5) / (n_ok + 1)
    } else { # "blom"
      p <- (r - 3/8) / (n_ok + 1/4)
    }
    # Clamp probabilities away from 0/1 to avoid +/-Inf
    p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
    out[ok] <- stats::qnorm(p)
    return(out)
  }

  # Fallback (should not reach)
  return(x)
}

# File: R/bone_markers.R

#' Compute Bone Health & Body-Composition Markers
#'
#' Given DXA, anthropometry, and optional bone-turnover markers, computes:
#' - OSTA: (weight - age) x 0.2
#' - ALMI: Appendicular Lean Mass Index = ALM / height^2
#' - FMI: Fat Mass Index = FM / height^2
#' - BMD_Tscore: (BMD - ref_mean) / ref_sd
#' and (if in `col_map` + data) passes through: TBS, HSA, PINP, CTX, BSAP, Osteocalcin.
#' @param data A `data.frame` or tibble with subject-level DXA/anthropometry data.
#' @param col_map Named list mapping keys to column names. Required keys:
#'   - `age`, `weight`, `height`, `ALM`, `FM`, `BMD`, `BMD_ref_mean`, `BMD_ref_sd`
#'   Optional (passed-through if present and found in data): `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin`.
#' @param verbose Logical; if `TRUE`, prints progress and summary.
#' @param na_action One of "zero", "warn_zero", or "error" controlling how
#'   missing/non-finite input values are treated. Default "zero" treats them as
#'   zero contribution in derived metrics (where applicable) or yields `NA` but
#'   suppresses warnings.
#' @param na_warn_prop Proportion (0–1) threshold to warn about high missingness
#'   per variable (only when `na_action = "warn_zero"`). Default 0.2.
#' @param check_extreme_sds Logical; if `TRUE`, scan mapped columns whose key
#'   names contain "sds" (case-insensitive) for absolute values > `sds_limit`.
#'   Default FALSE.
#' @param sds_limit Positive numeric; SDS magnitude limit for `check_extreme_sds`.
#'   Default 6.
#' @param extreme_sds_action One of "warn", "error", or "ignore" for what to do
#'   when extreme SDS-like values are detected. Default "warn".
#'
#' @return A tibble with columns: `OSTA`, `ALMI`, `FMI`, `BMD_Tscore`, and
#'   optionally `TBS`, `HSA`, `PINP`, `CTX`, `BSAP`, `Osteocalcin` (in that order).
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   age = c(60, 72), weight = c(65, 50), height = c(1.65, 1.58),
#'   ALM = c(18.2, 14.7), FM = c(22.0, 20.5),
#'   BMD = c(0.95, 0.80), BMD_ref_mean = c(1.00, 1.00), BMD_ref_sd = c(0.12, 0.12)
#' )
#' col_map <- list(
#'   age = "age", weight = "weight", height = "height",
#'   ALM = "ALM", FM = "FM", BMD = "BMD",
#'   BMD_ref_mean = "BMD_ref_mean", BMD_ref_sd = "BMD_ref_sd"
#' )
#' bone_markers(df, col_map)
#'
#' @references
#' Woo J, Leung J, Lau E, et al. (2002). The Osteoporosis Self-Assessment Tool (OSTA): a simple screening tool for osteoporosis. Maturitas, 41(2):227–232.
#' Kelly TL, Wilson KE, Heymsfield SB (2009). Dual energy X-ray absorptiometry body composition reference values from NHANES. Int J Obes, 33(6):783–789.
#' World Health Organization (1994). Assessment of fracture risk and its application to screening for postmenopausal osteoporosis. WHO Tech Rep Ser, 843:1–129.
#'
#' @importFrom tibble tibble
#' @export
bone_markers <- function(
  data,
  col_map,
  verbose = FALSE,
  na_action = c("zero", "warn_zero", "error"),
  na_warn_prop = 0.2,
  check_extreme_sds = FALSE,
  sds_limit = 6,
  extreme_sds_action = c("warn", "error", "ignore")
) {
  na_action <- match.arg(na_action)
  extreme_sds_action <- match.arg(extreme_sds_action)
  .bm_validate_misc_args(verbose, na_warn_prop, check_extreme_sds, sds_limit)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }
  if (!is.list(col_map) || is.null(names(col_map))) {
    stop("`col_map` must be a named list mapping keys to column names.")
  }
  required <- c("age", "weight", "height", "ALM", "FM", "BMD", "BMD_ref_mean", "BMD_ref_sd")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    stop(
      "bone_markers(): missing col_map entries for: ",
      paste(missing_map, collapse = ", ")
    )
  }
  # Ensure required columns exist
  validate_inputs(data, col_map,
    fun_name      = "bone_markers",
    required_keys = required
  )
  if (verbose) message("-> computing bone markers")
  # Type checks for required columns
  for (k in required) {
    cn <- col_map[[k]]
    if (!is.numeric(data[[cn]])) {
      stop(sprintf("'%s' column ('%s') must be numeric", k, cn))
    }
  }
  # Specific constraints (only enforce positivity on non-missing values;
  # missing/non-finite handled via `na_action`)
  ref_sd_vec <- data[[col_map$BMD_ref_sd]]
  height_vec <- data[[col_map$height]]
  if (any(ref_sd_vec <= 0, na.rm = TRUE)) {
    stop("'BMD_ref_sd' must be positive for non-missing rows.")
  }
  if (any(height_vec <= 0, na.rm = TRUE)) {
    stop("'height' must be positive for non-missing rows.")
  }
  n <- nrow(data)
  # extract required
  age <- data[[col_map$age]]
  weight <- data[[col_map$weight]]
  height <- data[[col_map$height]]
  ALM <- data[[col_map$ALM]]
  FM <- data[[col_map$FM]]
  BMD <- data[[col_map$BMD]]
  ref_mean <- data[[col_map$BMD_ref_mean]]
  ref_sd <- data[[col_map$BMD_ref_sd]]
  # Data quality scans
  req_vars <- c("age","weight","height","ALM","FM","BMD","BMD_ref_mean","BMD_ref_sd")
  nonfinite_vars <- character(0)
  high_na_vars <- character(0)
  all_na_vars <- character(0)
  sds_extreme_msgs <- character(0)
  sds_extreme_count <- 0L
  for (k in req_vars) {
    x <- data[[col_map[[k]]]]
    n_nonfinite <- sum(!is.finite(x))
    if (n_nonfinite > 0L) nonfinite_vars <- c(nonfinite_vars, sprintf("%s(%d non-finite)", k, n_nonfinite))
    x_na <- sum(is.na(x) | !is.finite(x))
    if (x_na == length(x)) all_na_vars <- c(all_na_vars, k)
    if (length(x) > 0L && (x_na / length(x)) >= na_warn_prop && x_na > 0L) {
      high_na_vars <- c(high_na_vars, sprintf("%s(%.1f%% NA)", k, 100 * x_na / length(x)))
    }
  }
  # Optional extreme SDS-like check for mapped keys containing 'sds'
  if (isTRUE(check_extreme_sds)) {
    sds_keys <- names(col_map)[grepl("sds", names(col_map), ignore.case = TRUE)]
    for (k in sds_keys) {
      cn <- col_map[[k]]
      if (cn %in% names(data)) {
        x <- data[[cn]]
        x[!is.finite(x)] <- NA_real_
        idx <- which(!is.na(x) & abs(x) > sds_limit)
        if (length(idx) > 0L) {
          sds_extreme_count <- sds_extreme_count + length(idx)
          sds_extreme_msgs <- c(sds_extreme_msgs, sprintf("%s(%d values > |%g|)", k, length(idx), sds_limit))
        }
      }
    }
  }
  if (na_action == "error") {
    any_na <- any(vapply(req_vars, function(k) any(is.na(data[[col_map[[k]]]]) | !is.finite(data[[col_map[[k]]]])), logical(1)))
    if (any_na) stop("Missing or non-finite values present in required inputs and na_action='error'.")
  }
  if (verbose) {
    sds_msg <- if (isTRUE(check_extreme_sds)) sprintf(", SDS extremes=%d", sds_extreme_count) else ""
    message(sprintf("Quality scan: non-finite in %d var(s), high-NA in %d, all-NA in %d%s.",
                    length(nonfinite_vars), length(high_na_vars), length(all_na_vars), sds_msg))
  }
  # Emit warnings (only when na_action = 'warn_zero')
  if (na_action == "warn_zero") {
    if (length(nonfinite_vars)) warning(sprintf("Non-finite values found in: %s; treated as NA.", paste(nonfinite_vars, collapse = ", ")))
    if (length(all_na_vars)) warning(sprintf("The following variables are entirely missing and contribute 0/NA: %s.", paste(all_na_vars, collapse = ", ")))
    if (length(high_na_vars)) warning(sprintf("High missingness (>= %.0f%%) in: %s.", 100 * na_warn_prop, paste(high_na_vars, collapse = ", ")))
  }
  if (sds_extreme_count > 0L) {
    if (extreme_sds_action == "error") stop(sprintf("Extreme SDS-like values detected: %s.", paste(sds_extreme_msgs, collapse = "; ")))
    if (extreme_sds_action == "warn") warning(sprintf("Extreme SDS-like values detected: %s.", paste(sds_extreme_msgs, collapse = "; ")))
  }
  if (verbose) message("Deriving OSTA, ALMI, FMI, and BMD_Tscore...")
  # compute core indices
  OSTA <- (weight - age) * 0.2
  ALMI <- ALM / (height^2)
  FMI <- FM / (height^2)
  BMD_Tscore <- (BMD - ref_mean) / ref_sd
  # helper for optional pass-through
  get_opt <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      data[[col_map[[key]]]]
    } else {
      rep(NA_real_, n)
    }
  }
  TBS <- get_opt("TBS")
  HSA <- get_opt("HSA")
  PINP <- get_opt("PINP")
  CTX <- get_opt("CTX")
  BSAP <- get_opt("BSAP")
  Osteocalcin <- get_opt("Osteocalcin")
  result <- tibble::tibble(
    OSTA,
    ALMI,
    FMI,
    BMD_Tscore,
    TBS,
    HSA,
    PINP,
    CTX,
    BSAP,
    Osteocalcin
  )
  if (verbose) {
    message(sprintf(
      "Completed: %d rows. Outputs: OSTA, ALMI, FMI, BMD_Tscore%s.",
      n,
      if (any(c("TBS","HSA","PINP","CTX","BSAP","Osteocalcin") %in% names(col_map))) ", plus optional markers" else ""
    ))
  }
  return(result)
}

# ---- internal helpers (not exported) ----
.bm_validate_misc_args <- function(verbose, na_warn_prop, check_extreme_sds, sds_limit) {
  if (!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    stop("`verbose` must be a single logical value.")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop))) {
    stop("`na_warn_prop` must be a single finite numeric between 0 and 1.")
  }
  if (na_warn_prop < 0 || na_warn_prop > 1) stop("`na_warn_prop` must be between 0 and 1.")
  if (!(is.logical(check_extreme_sds) && length(check_extreme_sds) == 1L && !is.na(check_extreme_sds))) {
    stop("`check_extreme_sds` must be a single logical value.")
  }
  if (!(is.numeric(sds_limit) && length(sds_limit) == 1L && is.finite(sds_limit) && sds_limit > 0)) {
    stop("`sds_limit` must be a single positive finite numeric.")
  }
  invisible(TRUE)
}

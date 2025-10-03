#' Compute Oxidative Stress Markers
#'
#' This function calculates simple oxidative stress indices from plasma or
#' cellular glutathione measurements. The primary metric implemented is the
#' GSH/GSSG ratio, defined as the concentration of reduced glutathione
#' (GSH) divided by oxidised glutathione (GSSG). A high ratio reflects a
#' healthy redox state, whereas a decreased ratio indicates oxidative stress.
#' Users may supply additional measurements such as total antioxidant capacity
#' or FORT/FORD assay values, which will be returned unchanged to aid
#' reporting. No universal transformation is applied to FORT/FORD because
#' these assays are kit‑specific.
#'
#' @param data A data frame or tibble with glutathione concentrations and
#'   optional oxidative assay results.
#' @param col_map Named list mapping variables to columns in `data`.
#'   Required name is `GSH` (reduced glutathione, μmol/L) and `GSSG`
#'   (oxidised glutathione, μmol/L). Optional names include `FORT` and
#'   `FORD` for reporting kit results.
#' @param verbose Logical; if `TRUE` prints progress messages.
#' @param na_action How to handle missing values: `"keep"` (default) propagates
#'   `NA` values; `"omit"` drops rows with any missing input; `"error"`
#'   aborts if missing values are present.
#' @param na_warn_prop Proportion threshold above which to warn about high
#'   missingness. Default is 0.2.
#' @param check_extreme Logical; if `TRUE` scans for implausible GSH and
#'   GSSG concentrations and responds according to `extreme_action`.
#' @param extreme_action One of `"warn"`, `"cap"`, `"error"` or `"ignore"`. When
#'   `"cap"`, out‑of‑range values are truncated to plausible ranges given in
#'   `extreme_rules`.
#' @param extreme_rules Optional named list of plausible ranges for `GSH` and
#'   `GSSG` (e.g., GSH 500–10000 μmol/L, GSSG 1–500 μmol/L). If omitted,
#'   default ranges are used when `check_extreme = TRUE`.
#'
#' @return A tibble with one or more columns. Always includes
#'   `GSH_GSSG_Ratio`. If FORT or FORD measurements are supplied via
#'   `col_map`, they are returned as columns `FORT` and `FORD` without
#'   modification.
#'
#' @examples
#' df <- data.frame(GSH = c(1000, 1500), GSSG = c(10, 15), FORT = c(300, 250))
#' oxidative_markers(df, col_map = list(GSH = "GSH", GSSG = "GSSG", FORT = "FORT"))
#'
#' @references
#' Jones DP. Redox potential of GSH/GSSG couples: biochemical basis and
#' measurement of cellular redox states. Biochim Biophys Acta. 2008;1780(11):1273–1290.
#' \doi{10.1016/j.bbagen.2008.03.030}
#' Tedesco I, Russo M, Russo P, Iacomino G, Russo GL, Palumbo R. Antioxidant
#' efficacy of red wine polyphenols in normal and catalase‑deficient human
#' lymphocytes: effect on intrinsic apoptotic pathway. J Nutr Biochem.
#' 2000;11(7–8):371–376. \doi{10.1016/S0955-2863(00)00109-X}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
oxidative_markers <- function(data, col_map, verbose = FALSE,
                              na_action = c("keep", "omit", "error"),
                              na_warn_prop = 0.2,
                              check_extreme = FALSE,
                              extreme_action = c("warn", "cap", "error", "ignore"),
                              extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Required and optional variables
  req_vars <- c("GSH", "GSSG")
  optional <- c("FORT", "FORD")

  # Validate col_map (no external helpers)
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("oxidative_markers(): 'col_map' must be a named list.")
  }
  missing_req <- setdiff(req_vars, names(col_map))
  if (length(missing_req)) {
    rlang::abort(sprintf(
      "oxidative_markers(): 'col_map' missing required names: %s",
      paste(missing_req, collapse = ", ")
    ))
  }
  # Resolve mapped columns
  req_map <- vapply(req_vars, function(nm) as.character(col_map[[nm]]), character(1))
  if (anyNA(req_map) || any(!nzchar(req_map))) {
    rlang::abort("oxidative_markers(): 'col_map' contains empty or NA column names for required inputs.")
  }
  # Optional mapping if provided
  opt_present <- intersect(optional, names(col_map))
  opt_map <- if (length(opt_present)) vapply(opt_present, function(nm) as.character(col_map[[nm]]), character(1)) else character(0)

  # Check duplicates and existence
  all_mapped <- c(req_map, opt_map)
  if (anyDuplicated(all_mapped)) {
    rlang::abort("oxidative_markers(): 'col_map' has duplicated target columns.")
  }
  missing_in_data <- setdiff(all_mapped, names(data))
  if (length(missing_in_data)) {
    rlang::abort(sprintf(
      "oxidative_markers(): columns not found in 'data': %s",
      paste(missing_in_data, collapse = ", ")
    ))
  }

  # Build standardized data.frame with canonical names
  df <- data[, req_map, drop = FALSE]
  names(df) <- req_vars
  if (length(opt_present)) {
    for (nm in opt_present) df[[nm]] <- data[[col_map[[nm]]]]
  }

  # High missingness warning on required inputs
  if (na_warn_prop > 0) {
    for (cn in req_vars) {
      pna <- mean(is.na(df[[cn]]))
      if (pna >= na_warn_prop && pna > 0) {
        rlang::warn(sprintf("oxidative_markers(): column '%s' has high missingness (%.1f%%).",
                            cn, 100 * pna))
      }
    }
  }

  # Handle missingness
  if (na_action == "error") {
    if (any(vapply(df[req_vars], function(x) any(is.na(x)), logical(1)))) {
      rlang::abort("oxidative_markers(): required inputs contain missing values (na_action='error').")
    }
  } else if (na_action == "omit") {
    keep <- Reduce(`&`, lapply(df[req_vars], function(x) !is.na(x)))
    if (isTRUE(verbose)) {
      rlang::inform(sprintf("oxidative_markers(): omitting %d rows with missing required inputs", sum(!keep)))
    }
    df <- df[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(df) == 0L) {
    out <- tibble::tibble(GSH_GSSG_Ratio = numeric())
    if ("FORT" %in% names(df)) out$FORT <- numeric()
    if ("FORD" %in% names(df)) out$FORD <- numeric()
    return(out)
  }

  # Coerce required to numeric
  for (cn in req_vars) {
    if (!is.numeric(df[[cn]])) {
      old <- df[[cn]]
      suppressWarnings(df[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(df[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("oxidative_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
  }

  # Extreme value checks (optional)
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    if (is.null(extreme_rules)) {
      extreme_rules <- list(GSH = c(500, 10000), GSSG = c(1, 500))
    }
    ex_counts <- integer(0)
    for (nm in names(extreme_rules)) {
      if (!nm %in% names(df)) next
      rng <- extreme_rules[[nm]]
      x <- df[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[nm] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & x < rng[1] & is.finite(x)] <- rng[1]
        x[bad & x > rng[2] & is.finite(x)] <- rng[2]
        df[[nm]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("oxidative_markers(): detected %d extreme input values.", total_ex))
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("oxidative_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("oxidative_markers(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  # Safe division
  sdv <- function(a, b) {
    res <- a / b
    res[is.na(a) | is.na(b) | b == 0] <- NA_real_
    res
  }

  ratio <- sdv(df$GSH, df$GSSG)

  out <- tibble::tibble(GSH_GSSG_Ratio = as.numeric(ratio))
  if ("FORT" %in% names(df)) out$FORT <- df$FORT
  if ("FORD" %in% names(df)) out$FORD <- df$FORD

  if (isTRUE(verbose)) {
    bad <- sum(is.na(out$GSH_GSSG_Ratio) | !is.finite(out$GSH_GSSG_Ratio))
    rlang::inform(sprintf(
      "oxidative_markers(): computed for %d rows; NA/Inf in ratio: %d; capped extremes: %d",
      nrow(out), bad, capped_n))
  }
  out
}
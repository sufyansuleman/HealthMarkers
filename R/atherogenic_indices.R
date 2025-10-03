#' Compute Atherogenic and Castelli Indices
#'
#' This helper computes three widely used atherogenic risk indices from a lipid
#' panel: the atherogenic index of plasma (AIP) and the Castelli risk indices I
#' and II. All ratios are calculated on the raw scale, with AIP being the
#' base‑10 logarithm of the triglyceride to HDL cholesterol ratio. These
#' indices are useful markers of dyslipidaemia and have been associated with
#' non‑alcoholic fatty liver disease and cardiovascular risk. The function
#' accepts a data frame and a column mapping for total cholesterol, HDL
#' cholesterol, triglycerides and LDL cholesterol. Optionally, missing values
#' can be kept, omitted or trigger an error; extreme values may be scanned and
#' either warned about, capped into plausible ranges, or raise errors.
#'
#' @param data Data frame or tibble containing the lipid panel.
#' @param col_map Named list mapping required variables to columns in `data`.
#'   Required names are `TC`, `HDL_c`, `TG`, and `LDL_c`.
#' @param verbose Logical; if `TRUE`, prints progress messages.
#' @param na_action How to handle missing values. One of `"keep"` (default),
#'   `"omit"`, or `"error"`.
#' @param na_warn_prop Proportion (0–1) above which a warning about missingness
#'   is issued for any required column. Default `0.2`.
#' @param check_extreme Logical; if `TRUE`, checks inputs against plausible
#'   ranges given in `extreme_rules`.
#' @param extreme_action One of `"warn"`, `"cap"`, `"error"`, or `"ignore"`.
#'   When `"cap"`, out‑of‑range values are truncated to allowed limits.
#' @param extreme_rules Optional named list of plausible ranges for inputs,
#'   e.g., `list(TC = c(50,400), HDL_c = c(10,150), LDL_c = c(10,300), TG = c(20,1000))`.
#'
#' @return A tibble with three numeric columns: `AIP` (the atherogenic index of
#'   plasma), `CRI_I` (Castelli risk index I, TC/HDL) and `CRI_II` (Castelli
#'   risk index II, LDL/HDL). If any denominator is zero or missing, the
#'   corresponding ratio will be `NA`.
#' @examples
#' # Compute AIP and Castelli indices from a small dataset
#' dat <- data.frame(TC = c(200, 180), HDL_c = c(50, 60), TG = c(150, 100),
#'                   LDL_c = c(120, 90))
#' res <- atherogenic_indices(dat, col_map = list(TC = "TC", HDL_c = "HDL_c",
#'                                                TG = "TG", LDL_c = "LDL_c"))
#' res
#' @references
#' Dobiášová M. Atherogenic index of plasma [log(triglycerides/high-density
#' lipoprotein cholesterol)]: theoretical and practical implications. Clin
#' Chem. 2004;50(7):1113–1115. \doi{10.1373/clinchem.2004.033175}
#' Njelekela M, Kadewele S, Liu M, et al. Lipid profile and atherogenic
#' indices among patients with metabolic syndrome in Dar es Salaam. Afr Health
#' Sci. 2016;16(3):687–696. \doi{10.4314/ahs.v16i3.17}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
atherogenic_indices <- function(data, col_map, verbose = FALSE,
                                na_action = c("keep", "omit", "error"),
                                na_warn_prop = 0.2,
                                check_extreme = FALSE,
                                extreme_action = c("warn", "cap", "error", "ignore"),
                                extreme_rules = NULL) {
  # match arguments
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Required inputs
  req_vars <- c("TC", "HDL_c", "TG", "LDL_c")

  # Validate col_map and extract columns (no external helpers)
  if (!is.list(col_map) || is.null(names(col_map))) {
    rlang::abort("atherogenic_indices(): 'col_map' must be a named list.")
  }
  missing_names <- setdiff(req_vars, names(col_map))
  if (length(missing_names)) {
    rlang::abort(sprintf(
      "atherogenic_indices(): 'col_map' missing required names: %s",
      paste(missing_names, collapse = ", ")
    ))
  }
  mapped_cols <- vapply(req_vars, function(nm) as.character(col_map[[nm]]), character(1))
  if (anyNA(mapped_cols) || any(!nzchar(mapped_cols))) {
    rlang::abort("atherogenic_indices(): 'col_map' contains empty or NA column names.")
  }
  if (anyDuplicated(mapped_cols)) {
    rlang::abort("atherogenic_indices(): 'col_map' has duplicated target columns.")
  }
  missing_in_data <- setdiff(mapped_cols, names(data))
  if (length(missing_in_data)) {
    rlang::abort(sprintf(
      "atherogenic_indices(): columns not found in 'data': %s",
      paste(missing_in_data, collapse = ", ")
    ))
  }

  # Subset and standardize names
  df <- data[, mapped_cols, drop = FALSE]
  names(df) <- req_vars

  # Warn about high missingness
  if (na_warn_prop > 0) {
    for (cn in req_vars) {
      prop_na <- mean(is.na(df[[cn]]))
      if (isTRUE(prop_na >= na_warn_prop) && prop_na > 0) {
        rlang::warn(sprintf("atherogenic_indices(): column '%s' has high missingness (%.1f%%).",
                            cn, 100 * prop_na))
      }
    }
  }

  # handle missingness
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(df[req_vars], function(x) any(is.na(x))))
    if (any_na) {
      rlang::abort("atherogenic_indices(): required inputs contain missing values (na_action='error').")
    }
  } else if (na_action == "omit") {
    keep <- Reduce(`&`, lapply(df[req_vars], function(x) !is.na(x)))
    if (isTRUE(verbose)) {
      rlang::inform(sprintf("atherogenic_indices(): omitting %d rows with missing required inputs",
                            sum(!keep)))
    }
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0L) {
    return(tibble::tibble(AIP = numeric(), CRI_I = numeric(), CRI_II = numeric()))
  }

  # Coerce to numeric and warn if conversion introduced NA
  for (cn in req_vars) {
    if (!is.numeric(df[[cn]])) {
      old <- df[[cn]]
      suppressWarnings(df[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(df[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("atherogenic_indices(): column '%s' coerced to numeric; NAs introduced: %d",
                            cn, introduced))
      }
    }
  }

  # Check for extremes if requested
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    if (is.null(extreme_rules)) {
      extreme_rules <- list(TC = c(50, 400), HDL_c = c(10, 150), LDL_c = c(10, 300), TG = c(20, 1000))
    }
    ex_counts <- integer(0)
    for (nm in intersect(names(extreme_rules), req_vars)) {
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
        rlang::abort(sprintf("atherogenic_indices(): detected %d extreme input values.", total_ex))
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("atherogenic_indices(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("atherogenic_indices(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  # Safe division helper
  sdv <- function(num, den) {
    res <- num / den
    res[is.na(num) | is.na(den) | den == 0] <- NA_real_
    res
  }

  # Compute indices
  AIP   <- suppressWarnings(log10(sdv(df$TG,   df$HDL_c)))
  CRI_I <- sdv(df$TC,   df$HDL_c)
  CRI_II<- sdv(df$LDL_c, df$HDL_c)

  out <- tibble::tibble(
    AIP   = as.numeric(AIP),
    CRI_I = as.numeric(CRI_I),
    CRI_II= as.numeric(CRI_II)
  )

  if (isTRUE(verbose)) {
    bad_vals <- sum(!is.finite(out$AIP)   | is.na(out$AIP)) +
                sum(!is.finite(out$CRI_I) | is.na(out$CRI_I)) +
                sum(!is.finite(out$CRI_II)| is.na(out$CRI_II))
    rlang::inform(sprintf(
      "atherogenic_indices(): computed for %d rows; NA/Inf in outputs: %d; capped extremes: %d",
      nrow(out), bad_vals, capped_n))
  }
  out
}
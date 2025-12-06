# File: R/vitamin_markers.R

#' Compute composite vitamin and endocrine marker ratios and z-scores
#'
#' Given serum/plasma vitamins and related analytes, `vitamin_markers()` computes:
#' - VitD_Z: z-score of 25(OH)D using provided reference mean/sd
#' - B12_Fol_Ratio: vitamin B12 / folate
#' - Ferr_TSat_R: ferritin / transferrin saturation (TSat)
#' - Cort_DHEA_R: cortisol / DHEA-S
#' - T_E2_Ratio: testosterone / estradiol
#' - TSH_fT4_R: TSH / free T4
#' - Retinol_Z: z-score of retinol using provided reference mean/sd
#' - Toco_Lip_R: alpha-tocopherol / total lipids
#' - Mg_Zn_R: magnesium / zinc
#' - Cu_Zn_R: copper / zinc
#' Plus pass-through: PIVKA_II, VitC, Homocysteine, MMA
#'
#' HM-CS v2:
#' - Validation via `hm_validate_inputs(data, col_map, required_keys, fn)`
#' - User errors via `rlang::abort(..., class=...)`
#' - Verbosity via `hm_inform(level)` controlled by `options(healthmarkers.verbose)`
#' - High-missingness diagnostics at debug level only
#'
#' @param data A data.frame or tibble with vitamin/analyte columns.
#' @param col_map Named list mapping required keys to column names:
#'   VitD, VitD_ref_mean, VitD_ref_sd, B12, Folate, Ferritin, TSat,
#'   Cortisol, DHEAS, Testosterone, Estradiol, TSH, free_T4,
#'   Retinol, Retinol_ref_mean, Retinol_ref_sd, Tocopherol, Total_lipids,
#'   PIVKA_II, VitC, Homocysteine, MMA, Magnesium, Zinc, Copper.
#' @param na_action One of c("keep","omit","error") for required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness debug notices. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extreme values. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds keyed by input keys or column names.
#' @param verbose Logical; if TRUE, prints progress messages via hm_inform().
#'
#' @return A tibble with columns:
#'   VitD_Z, B12_Fol_Ratio, Ferr_TSat_R, Cort_DHEA_R, T_E2_Ratio, TSH_fT4_R,
#'   Retinol_Z, Toco_Lip_R, PIVKA_II, VitC, Homocysteine, MMA, Mg_Zn_R, Cu_Zn_R
#'
#' @examples
#' df <- tibble::tibble(
#'   VitD = 50, VitD_ref_mean = 40, VitD_ref_sd = 5,
#'   B12 = 300, Folate = 15,
#'   Ferritin = 100, TSat = 0.25,
#'   Cortisol = 200, DHEAS = 100,
#'   Testosterone = 12, Estradiol = 120,
#'   TSH = 2, free_T4 = 14,
#'   Retinol = 0.8, Retinol_ref_mean = 0.9, Retinol_ref_sd = 0.2,
#'   Tocopherol = 30, Total_lipids = 3,
#'   PIVKA_II = 5, VitC = 60, Homocysteine = 10, MMA = 0.3,
#'   Magnesium = 0.8, Zinc = 15, Copper = 15
#' )
#' cm <- as.list(names(df)); names(cm) <- names(df)
#' vitamin_markers(df, cm)
#'
#' @references
#' Holick MF. Vitamin D deficiency. N Engl J Med. 2007;357:266-281. \doi{10.1056/NEJMra070553}
#' O'Leary F, Samman S. Vitamin B12 in health and disease. Nutrients. 2010;2(3):299-316. \doi{10.3390/nu2030299}
#' Ganz T, Nemeth E. Iron homeostasis in host defence and inflammation. Nat Rev Immunol. 2015;15:500-510. \doi{10.1038/nri3863}
#' Huxtable RJ. Physiological actions of taurine. Physiol Rev. 1992;72(1):101-163. (endocrine ratios context)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
vitamin_markers <- function(data,
                            col_map,
                            na_action = c("keep","omit","error"),
                            na_warn_prop = 0.2,
                            check_extreme = FALSE,
                            extreme_action = c("warn","cap","error","ignore"),
                            extreme_rules = NULL,
                            verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  t0 <- Sys.time()

  if (!is.data.frame(data)) {
    rlang::abort("vitamin_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_vitamin_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("vitamin_markers(): `col_map` must be a named list of required keys -> column names.",
                 class = "healthmarkers_vitamin_error_colmap_type")
  }

  required_keys <- c(
    "VitD","VitD_ref_mean","VitD_ref_sd","B12","Folate","Ferritin","TSat",
    "Cortisol","DHEAS","Testosterone","Estradiol","TSH","free_T4",
    "Retinol","Retinol_ref_mean","Retinol_ref_sd","Tocopherol","Total_lipids",
    "PIVKA_II","VitC","Homocysteine","MMA","Magnesium","Zinc","Copper"
  )

  # HM-CS v2: standardized validation
  hm_validate_inputs(data, col_map, required_keys = required_keys, fn = "vitamin_markers")

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> vitamin_markers: validating inputs")

  # Ensure mapped columns exist
  req_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("vitamin_markers(): missing required columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_vitamin_error_missing_columns"
    )
  }

  # Coerce mapped numerics; warn if NAs introduced; non-finite -> NA
  for (key in required_keys) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("vitamin_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug)
  for (key in required_keys) {
    cn <- col_map[[key]]
    x <- data[[cn]]
    n <- length(x); if (!n) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("vitamin_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # NA policy
  used_cols <- req_cols
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("vitamin_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_vitamin_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> vitamin_markers: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      VitD_Z = numeric(),
      B12_Fol_Ratio = numeric(),
      Ferr_TSat_R = numeric(),
      Cort_DHEA_R = numeric(),
      T_E2_Ratio = numeric(),
      TSH_fT4_R = numeric(),
      Retinol_Z = numeric(),
      Toco_Lip_R = numeric(),
      PIVKA_II = numeric(),
      VitC = numeric(),
      Homocysteine = numeric(),
      MMA = numeric(),
      Mg_Zn_R = numeric(),
      Cu_Zn_R = numeric()
    ))
  }

  # Optional extremes scan/cap (allow rules keyed by keys or by column names)
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    default_rules <- list(
      VitD = c(0, 250), VitD_ref_mean = c(-Inf, Inf), VitD_ref_sd = c(0.01, Inf),
      B12 = c(0, 2000), Folate = c(0, 100),
      Ferritin = c(0, 3000), TSat = c(0, 1),
      Cortisol = c(0, 2000), DHEAS = c(0, 2000),
      Testosterone = c(0, 200), Estradiol = c(0, 5000),
      TSH = c(0, 200), free_T4 = c(0, 100),
      Retinol = c(0, 10), Retinol_ref_mean = c(-Inf, Inf), Retinol_ref_sd = c(0.001, Inf),
      Tocopherol = c(0, 200), Total_lipids = c(0.001, 100),
      PIVKA_II = c(0, 10000), VitC = c(0, 1000), Homocysteine = c(0, 200), MMA = c(0, 20),
      Magnesium = c(0, 10), Zinc = c(0, 1000), Copper = c(0, 1000)
    )
    rules <- if (is.null(extreme_rules)) default_rules else extreme_rules

    # Remap if rules keyed by required_keys
    key_to_col <- stats::setNames(req_cols, required_keys)
    if (!is.null(names(rules))) {
      remapped <- list()
      for (nm in names(rules)) {
        col_nm <- if (nm %in% names(key_to_col)) key_to_col[[nm]] else nm
        remapped[[col_nm]] <- rules[[nm]]
      }
      rules <- remapped
    }

    ex_counts <- integer(0)
    for (nm in intersect(names(rules), names(data))) {
      rng <- rules[[nm]]
      x <- data[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[nm] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap" && any(bad, na.rm = TRUE)) {
        x[bad & is.finite(x) & x < rng[1]] <- rng[1]
        x[bad & is.finite(x) & x > rng[2]] <- rng[2]
        data[[nm]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("vitamin_markers(): detected %d extreme input values.", total_ex),
                     class = "healthmarkers_vitamin_error_extremes")
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("vitamin_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("vitamin_markers(): detected %d extreme input values (not altered).", total_ex))
      }
      # ignore: no-op
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> vitamin_markers: computing markers")

  # Safe division with consolidated zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  g <- function(key) data[[col_map[[key]]]]

  VitD_Z        <- (g("VitD")     - g("VitD_ref_mean"))    / g("VitD_ref_sd")
  B12_Fol_Ratio <-  safe_div(g("B12"), g("Folate"), "B12_Folate_den")
  Ferr_TSat_R   <-  safe_div(g("Ferritin"), g("TSat"), "Ferr_TSat_den")
  Cort_DHEA_R   <-  safe_div(g("Cortisol"), g("DHEAS"), "Cort_DHEAS_den")
  T_E2_Ratio    <-  safe_div(g("Testosterone"), g("Estradiol"), "T_E2_den")
  TSH_fT4_R     <-  safe_div(g("TSH"), g("free_T4"), "TSH_fT4_den")
  Retinol_Z     <- (g("Retinol") - g("Retinol_ref_mean")) / g("Retinol_ref_sd")
  Toco_Lip_R    <-  safe_div(g("Tocopherol"), g("Total_lipids"), "Toco_Lip_den")
  Mg_Zn_R       <-  safe_div(g("Magnesium"), g("Zinc"), "Mg_Zn_den")
  Cu_Zn_R       <-  safe_div(g("Copper"), g("Zinc"), "Cu_Zn_den")

  out <- tibble::tibble(
    VitD_Z        = as.numeric(VitD_Z),
    B12_Fol_Ratio = as.numeric(B12_Fol_Ratio),
    Ferr_TSat_R   = as.numeric(Ferr_TSat_R),
    Cort_DHEA_R   = as.numeric(Cort_DHEA_R),
    T_E2_Ratio    = as.numeric(T_E2_Ratio),
    TSH_fT4_R     = as.numeric(TSH_fT4_R),
    Retinol_Z     = as.numeric(Retinol_Z),
    Toco_Lip_R    = as.numeric(Toco_Lip_R),
    PIVKA_II      = as.numeric(g("PIVKA_II")),
    VitC          = as.numeric(g("VitC")),
    Homocysteine  = as.numeric(g("Homocysteine")),
    MMA           = as.numeric(g("MMA")),
    Mg_Zn_R       = as.numeric(Mg_Zn_R),
    Cu_Zn_R       = as.numeric(Cu_Zn_R)
  )

  # Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("vitamin_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hm_inform(level = "inform", msg = sprintf(
      "Completed vitamin_markers: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}

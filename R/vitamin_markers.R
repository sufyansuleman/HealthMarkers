# File: R/vitamin_markers.R

#' Compute Vitamin & Nutrient-Status Indices (HM-CRANIZED)
#'
#' Given a data.frame or tibble with vitamin and nutrient labs, `vitamin_markers()`
#' returns commonly-used z-scores, ratios, and direct measures of vitamin and nutrient status.
#'
#' Required col_map keys -> column names in `data`:
#'   - VitD (nmol/L), VitD_ref_mean (nmol/L), VitD_ref_sd (nmol/L)
#'   - B12 (pmol/L), Folate (nmol/L)
#'   - Ferritin (ng/mL), TSat (fraction 0–1)
#'   - Cortisol (nmol/L), DHEAS (nmol/L)
#'   - Testosterone (nmol/L), Estradiol (pmol/L)
#'   - TSH (mIU/L), free_T4 (pmol/L)
#'   - Retinol (umol/L), Retinol_ref_mean (umol/L), Retinol_ref_sd (umol/L)
#'   - Tocopherol (umol/L), Total_lipids (mmol/L)
#'   - PIVKA_II (ng/mL), VitC (umol/L)
#'   - Homocysteine (umol/L), MMA (umol/L)
#'   - Magnesium (mmol/L), Zinc (umol/L), Copper (umol/L)
#'
#' Adds:
#'  - Robust validation and numeric coercion (warn on NAs introduced)
#'  - NA handling policies via `na_action`
#'  - High-missingness diagnostics via `na_warn_prop`
#'  - Optional extremes scan/cap via `check_extreme` and `extreme_action`
#'  - Safe division with consolidated zero-denominator warnings
#'  - Verbose progress and completion summaries
#'
#' @param data A data.frame or tibble with the mapped columns.
#' @param col_map Named list mapping required keys to column names (see above).
#' @param verbose Logical; if TRUE, prints progress messages and a completion summary. Default FALSE.
#' @param na_action One of c("keep","omit","error") for handling NAs in required inputs. Default "keep".
#' @param na_warn_prop Proportion [0,1] to trigger high-missingness warnings. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extremes. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds keyed by col_map keys. If NULL, broad defaults are used.
#'
#' @return A tibble with:
#'   VitD_Z, B12_Fol_Ratio, Ferr_TSat_R, Cort_DHEA_R, T_E2_Ratio, TSH_fT4_R,
#'   Retinol_Z, Toco_Lip_R, PIVKA_II, VitC, Homocysteine, MMA, Mg_Zn_R, Cu_Zn_R
#'
#' @references
#' Original derivations
#' - Holick MF, Binkley NC, Bischoff-Ferrari HA, et al. Evaluation, treatment, and prevention of vitamin D deficiency: an Endocrine Society clinical practice guideline. 
#'   J Clin Endocrinol Metab. 2011;96(7):1911–1930. \doi{10.1210/jc.2011-0385} (Vitamin D reference ranges, z-scores)
#' - Stabler SP. Vitamin B12 deficiency. 
#'   N Engl J Med. 2013;368(2):149–160. \doi{10.1056/NEJMcp1113996} (B12/Folate ratio, MMA, homocysteine)
#' - Traber MG, Atkinson J. Vitamin E, antioxidant and nothing more. 
#'   Free Radic Biol Med. 2007;43(1):4–15. \doi{10.1016/j.freeradbiomed.2007.03.024} (Tocopherol/lipids ratio)
#' - Levine M, Rumsey SC, Daruwala R, Park JB, Wang Y. Criteria and recommendations for vitamin C intake. 
#'   Proc Natl Acad Sci U S A. 1996;93(8):3704–3709. \doi{10.1073/pnas.93.8.3704} (Vitamin C reference values)
#'
#' Validation and consensus
#' - Allen LH. Causes of vitamin B12 and folate deficiency. 
#'   Food Nutr Bull. 2008;29(2 Suppl):S20–S34. \doi{10.1177/15648265080292S105} (Validation of B12/Folate ratio)
#' - Arnaud J, Faure H, et al. Longitudinal studies of antioxidant vitamins: correlation of serum α-tocopherol with lipids. 
#'   Am J Clin Nutr. 1991;53(4):884–889. \doi{10.1093/ajcn/53.4.884} (Validation of tocopherol/lipids ratio)
#' - Labrie F, Bélanger A, et al. DHEA and aging: importance of DHEA in sex steroid biosynthesis. 
#'   Endocr Rev. 1997;18(5):713–738. \doi{10.1210/edrv.18.5.0319} (Cortisol/DHEAS ratio context)
#' - Lippi G, Montagnana M, Guidi GC. Biochemical markers of alcoholism. 
#'   Clin Chim Acta. 2006;356(1–2):9–26. \doi{10.1016/j.cca.2005.11.007} (PIVKA-II as marker of vitamin K deficiency)
#' - Prasad AS. Zinc in human health: effect of zinc on immune cells. 
#'   Mol Med. 2008;14(5–6):353–357. \doi{10.2119/2008-00033.Prasad} (Zn and Cu/Zn ratio in health and disease)
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
vitamin_markers <- function(data, col_map,
                            verbose = FALSE,
                            na_action = c("keep","omit","error"),
                            na_warn_prop = 0.2,
                            check_extreme = FALSE,
                            extreme_action = c("warn","cap","error","ignore"),
                            extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)
  t0 <- Sys.time()

  # Validate inputs
  if (!is.data.frame(data)) {
    rlang::abort("vitamin_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_vitamin_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("vitamin_markers(): `col_map` must be a named list mapping keys to column names.",
                 class = "healthmarkers_vitamin_error_colmap_type")
  }
  if (isTRUE(verbose)) rlang::inform("-> vitamin_markers: validating inputs")

  required_keys <- c(
    "VitD", "VitD_ref_mean", "VitD_ref_sd",
    "B12", "Folate",
    "Ferritin", "TSat",
    "Cortisol", "DHEAS",
    "Testosterone", "Estradiol",
    "TSH", "free_T4",
    "Retinol", "Retinol_ref_mean", "Retinol_ref_sd",
    "Tocopherol", "Total_lipids",
    "PIVKA_II", "VitC",
    "Homocysteine", "MMA",
    "Magnesium", "Zinc", "Copper"
  )

  missing_keys <- setdiff(required_keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("vitamin_markers(): missing col_map entries for: ", paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_vitamin_error_missing_map"
    )
  }

  mapped_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("vitamin_markers(): missing required columns in `data`: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_vitamin_error_missing_columns"
    )
  }

  # Coerce mapped columns to numeric where appropriate; warn on NAs introduced
  .vm_coerce_numeric(data, col_map, keys = required_keys, warn = TRUE)

  # High-missingness warnings on required inputs
  .vm_warn_high_missing(data, mapped_cols, na_warn_prop = na_warn_prop)

  # NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("vitamin_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_vitamin_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> vitamin_markers: omitting %d rows with NA in required inputs", sum(!keep)))
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

  # Optional extremes scan/cap
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .vm_default_extreme_rules() else extreme_rules
    ex_counts <- integer(0)
    for (key in intersect(names(rules), required_keys)) {
      cn <- col_map[[key]]
      rng <- rules[[key]]
      x <- data[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[key] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & is.finite(x) & x < rng[1]] <- rng[1]
        x[bad & is.finite(x) & x > rng[2]] <- rng[2]
        data[[cn]] <- x
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
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> vitamin_markers: computing markers")

  # Safe division helper with consolidated zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # Pull inputs
  d25  <- data[[col_map$VitD]]
  d25m <- data[[col_map$VitD_ref_mean]]
  d25s <- data[[col_map$VitD_ref_sd]]
  B12  <- data[[col_map$B12]]
  Fol  <- data[[col_map$Folate]]
  Fe   <- data[[col_map$Ferritin]]
  TS   <- data[[col_map$TSat]]
  Cort <- data[[col_map$Cortisol]]
  DHEA <- data[[col_map$DHEAS]]
  Tst  <- data[[col_map$Testosterone]]
  E2   <- data[[col_map$Estradiol]]
  TSHv <- data[[col_map$TSH]]
  fT4  <- data[[col_map$free_T4]]
  Ret  <- data[[col_map$Retinol]]
  Rm   <- data[[col_map$Retinol_ref_mean]]
  Rs   <- data[[col_map$Retinol_ref_sd]]
  Toc  <- data[[col_map$Tocopherol]]
  Lip  <- data[[col_map$Total_lipids]]
  PIV  <- data[[col_map$PIVKA_II]]
  VC   <- data[[col_map$VitC]]
  Hcy  <- data[[col_map$Homocysteine]]
  MMAv <- data[[col_map$MMA]]
  Mg   <- data[[col_map$Magnesium]]
  Zn   <- data[[col_map$Zinc]]
  Cu   <- data[[col_map$Copper]]

  # Compute indices using safe divisions
  VitD_Z        <- safe_div(d25 - d25m, d25s, "VitD_Z_sd")
  B12_Fol_Ratio <- safe_div(B12, Fol, "B12_over_Folate")
  Ferr_TSat_R   <- safe_div(Fe, TS, "Ferritin_over_TSat")
  Cort_DHEA_R   <- safe_div(Cort, DHEA, "Cortisol_over_DHEAS")
  T_E2_Ratio    <- safe_div(Tst, E2, "Testosterone_over_E2")
  TSH_fT4_R     <- safe_div(TSHv, fT4, "TSH_over_freeT4")
  Retinol_Z     <- safe_div(Ret - Rm, Rs, "Retinol_Z_sd")
  Toco_Lip_R    <- safe_div(Toc, Lip, "Tocopherol_over_Lipids")
  Mg_Zn_R       <- safe_div(Mg, Zn, "Magnesium_over_Zinc")
  Cu_Zn_R       <- safe_div(Cu, Zn, "Copper_over_Zinc")

  out <- tibble::tibble(
    VitD_Z        = as.numeric(VitD_Z),
    B12_Fol_Ratio = as.numeric(B12_Fol_Ratio),
    Ferr_TSat_R   = as.numeric(Ferr_TSat_R),
    Cort_DHEA_R   = as.numeric(Cort_DHEA_R),
    T_E2_Ratio    = as.numeric(T_E2_Ratio),
    TSH_fT4_R     = as.numeric(TSH_fT4_R),
    Retinol_Z     = as.numeric(Retinol_Z),
    Toco_Lip_R    = as.numeric(Toco_Lip_R),
    PIVKA_II      = as.numeric(PIV),
    VitC          = as.numeric(VC),
    Homocysteine  = as.numeric(Hcy),
    MMA           = as.numeric(MMAv),
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
    rlang::inform(sprintf(
      "Completed vitamin_markers: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}

# ------------------- internal helpers ------------------------------------------

.vm_coerce_numeric <- function(data, col_map, keys, warn = TRUE) {
  for (key in keys) {
    cn <- col_map[[key]]
    if (!(cn %in% names(data))) next
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      if (isTRUE(warn)) {
        introduced <- sum(is.na(data[[cn]]) & !is.na(old))
        if (introduced > 0) {
          rlang::warn(sprintf("vitamin_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
        }
      }
    }
  }
  invisible(TRUE)
}

.vm_warn_high_missing <- function(data, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("vitamin_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.vm_default_extreme_rules <- function() {
  # Broad plausibility bounds by analyte (units in header)
  list(
    VitD = c(0, 375),
    VitD_ref_mean = c(0, 375),
    VitD_ref_sd = c(0.1, 200),
    B12 = c(50, 2000),
    Folate = c(1, 100),
    Ferritin = c(1, 2000),
    TSat = c(0, 1.2), # fraction
    Cortisol = c(0, 2000),
    DHEAS = c(0, 30000),
    Testosterone = c(0, 100),
    Estradiol = c(0, 20000),
    TSH = c(0, 100),
    free_T4 = c(0, 100),
    Retinol = c(0, 10),
    Retinol_ref_mean = c(0, 10),
    Retinol_ref_sd = c(0.05, 5),
    Tocopherol = c(0, 100),
    Total_lipids = c(0.1, 50),
    PIVKA_II = c(0, 10000),
    VitC = c(0, 300),
    Homocysteine = c(0, 200),
    MMA = c(0, 100),
    Magnesium = c(0, 3),
    Zinc = c(0, 200),
    Copper = c(0, 300)
  )
}

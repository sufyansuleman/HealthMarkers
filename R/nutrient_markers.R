#' Compute a Suite of Nutrient-Based Health Markers
#'
#' Given a data frame or tibble of routine biochemical labs,
#' `nutrient_markers()` returns a set of widely used ratios, products,
#' and simple percentages that summarize iron metabolism, protein status,
#' omega-3 balance, renal excretion, mineral homeostasis, and aromatic
#' amino-acid patterns.
#'
#' Recognized markers (returned as columns):
#' - FerritinTS: Ferritin / Transferrin saturation
#' - AGR: Albumin / Globulin, where Globulin = Total protein - Albumin
#' - Omega3Index: EPA + DHA (percentage points)
#' - Mg_Cr_Ratio: Magnesium / Creatinine
#' - GlycatedAlbuminPct: (Glycated albumin / Albumin) x 100
#' - UA_Cr_Ratio: Uric acid / Creatinine
#' - BUN_Cr_Ratio: BUN / Creatinine
#' - Ca_x_Phosphate: Calcium x Phosphate
#' - AnionGap: (Na + K) - (Cl + HCO3)
#' - Tyr_Phe_Ratio: Tyrosine / Phenylalanine
#'
#' @param data A data frame or tibble containing subject-level data.
#' @param col_map Optional named list mapping variable keys (see Details) to
#'   column names in `data`. You only need to supply the keys you have; any
#'   markers with missing inputs return `NA`. If NULL, defaults to identity
#'   mapping for all known keys.
#' @param na_action One of c("keep","omit","error") controlling missing-data policy
#'   across the columns referenced by `col_map`.
#'   - "keep" (default): keep NA; outputs become NA where inputs are NA.
#'   - "omit": drop rows with NA in any used input column.
#'   - "error": abort if any used input contains NA.
#' @param na_warn_prop Numeric in \eqn{[0,1]}; per-variable threshold for high-missingness
#'   diagnostics on used input columns. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan used input columns for out-of-range
#'   values (see `extreme_rules`). Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") used when extremes are
#'   detected (only when `check_extreme = TRUE`). Default "warn".
#'   - "warn": only warn, "cap": truncate to range and warn, "error": abort,
#'   - "ignore": no-op, "NA": set flagged inputs to NA.
#' @param extreme_rules Optional named list from input keys to c(min,max) ranges. If NULL,
#'   broad defaults are used (see Details).
#' @param verbose Logical; if TRUE, prints stepwise messages and a final summary via hm_inform. Default FALSE.
#'
#' @details
#' Recognized `col_map` keys and expected units (no automatic conversion):
#' - ferritin: Serum ferritin (ng/mL)
#' - transferrin_sat: Transferrin saturation (%)
#' - albumin: Serum albumin (g/L)
#' - total_protein: Total serum protein (g/L)
#' - EPA: Red-cell EPA as % of total fatty acids
#' - DHA: Red-cell DHA as % of total fatty acids
#' - Mg: Serum magnesium (mmol/L)
#' - creatinine: Serum creatinine (umol/L)
#' - glycated_albumin: Glycated albumin (g/L)
#' - uric_acid: Serum uric acid (umol/L)
#' - BUN: Blood urea nitrogen (mg/dL)
#' - phosphate: Serum phosphate (mmol/L)
#' - calcium: Serum calcium (mmol/L)
#' - Na: Serum sodium (mmol/L)
#' - K: Serum potassium (mmol/L)
#' - Cl: Serum chloride (mmol/L)
#' - HCO3: Serum bicarbonate (mmol/L)
#' - Tyr: Serum tyrosine (umol/L)
#' - Phe: Serum phenylalanine (umol/L)
#'
#' Default `extreme_rules` (inputs) are broad and intended for unit/entry checks:
#' ferritin (0, 2000), transferrin_sat (0, 100), albumin (10, 60), total_protein (40, 100),
#' EPA (0, 20), DHA (0, 20), Mg (0.2, 3), creatinine (20, 2000), glycated_albumin (0, 60),
#' uric_acid (50, 1000), BUN (1, 150), phosphate (0.1, 5), calcium (0.5, 4),
#' Na (100, 200), K (2, 8), Cl (70, 130), HCO3 (5, 45), Tyr (10, 300), Phe (20, 300).
#'
#' @return A tibble with one row per input row and these columns:
#' FerritinTS, AGR, Omega3Index, Mg_Cr_Ratio, GlycatedAlbuminPct,
#' UA_Cr_Ratio, BUN_Cr_Ratio, Ca_x_Phosphate, AnionGap, Tyr_Phe_Ratio.
#'
#' @references
#'  Original derivations
#'  Harris WS, von Schacky C. The Omega-3 Index: a new risk factor for death from coronary heart disease? 
#'   Prostaglandins Leukot Essent Fatty Acids. 2004;71(5):263-270. \doi{10.1016/j.plefa.2004.05.011} (Omega-3 Index)
#'  Koga M, Kasayama S. Clinical impact of glycated albumin as another glycemic control marker. 
#'   J Diabetes Investig. 2010;1(1-2):43-48. \doi{10.1111/j.2040-1124.2010.00011.x} (Glycated Albumin %)
#'  Block GA, Hulbert-Shearon TE, Levin NW, Port FK. Association of serum phosphorus and calcium * phosphate product 
#'   with mortality risk in chronic hemodialysis patients: a national study. Kidney Int. 1998;54(2):556-562. 
#'   \doi{10.1046/j.1523-1755.1998.00005.x} (Calcium-phosphate product)
#'  Waikar SS, Bonventre JV. Creatinine kinetics and the definition of acute kidney injury. 
#'   J Am Soc Nephrol. 2009;20(3):672-679. \doi{10.1681/ASN.2008070669} (BUN/Creatinine ratio)
#' @examples
#' df <- tibble::tibble(
#'   ferritin         = c(50, 100),
#'   transferrin_sat  = c(30, 50),
#'   albumin          = c(45, 40),
#'   total_protein    = c(70, 75),
#'   EPA              = c(2.0, 2.5),
#'   DHA              = c(4.0, 4.5),
#'   Mg               = c(0.85, 0.90),
#'   creatinine       = c(80, 90),
#'   glycated_albumin = c(12, 14),
#'   uric_acid        = c(300, 400),
#'   BUN              = c(14, 16),
#'   phosphate        = c(1.0, 1.2),
#'   calcium          = c(2.3, 2.4),
#'   Na               = c(140, 138),
#'   K                = c(4.2, 4.0),
#'   Cl               = c(100, 102),
#'   HCO3             = c(24, 26),
#'   Tyr              = c(60, 70),
#'   Phe              = c(50, 55)
#' )
#' nutrient_markers(df, verbose = TRUE)
#'
#' @importFrom rlang abort warn inform
#' @importFrom tibble tibble
#' @export
nutrient_markers <- function(
  data,
  col_map = NULL,
  na_action = c("keep","omit","error"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> nutrient_markers: validating inputs")

  # HM-CS v2 validation hook; no strictly required keys for this summarizer
  hm_validate_inputs(data, col_map, required_keys = character(0), fn = "nutrient_markers")

  keys <- c(
    "ferritin","transferrin_sat","albumin","total_protein",
    "EPA","DHA","Mg","creatinine","glycated_albumin","uric_acid",
    "BUN","phosphate","calcium","Na","K","Cl","HCO3","Tyr","Phe"
  )

  if (is.null(col_map)) {
    col_map <- as.list(keys); names(col_map) <- keys
  } else {
    extra <- setdiff(names(col_map), keys)
    if (length(extra)) {
      rlang::warn(sprintf("nutrient_markers(): ignoring unrecognized keys in col_map: %s", paste(extra, collapse = ", ")))
      col_map[extra] <- NULL
    }
  }

  mapped <- unlist(col_map, use.names = TRUE)
  used_cols <- intersect(unname(mapped), names(data))

  # Coerce used inputs to numeric; NA on non-finite
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("nutrient_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # High-missingness diagnostics (debug verbosity)
  .nm_high_missing_diag(data, used_cols, na_warn_prop = na_warn_prop)

  # NA policy over used inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("nutrient_markers(): used input columns contain missing values (na_action='error').",
                   class = "healthmarkers_nm_error_missing_values")
    }
  } else if (na_action == "omit" && length(used_cols)) {
    keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> nutrient_markers: omitting %d rows with NA in used inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Optional extreme scan/capping on used inputs
  capped_n <- 0L
  if (isTRUE(check_extreme) && length(used_cols)) {
    rules <- if (is.null(extreme_rules)) .nm_default_extreme_rules() else extreme_rules
    ex <- .nm_extreme_scan(data, col_map, rules, keys)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("nutrient_markers(): detected %d extreme input values.", ex$count),
                     class = "healthmarkers_nm_error_extremes")
      } else if (extreme_action == "cap") {
        data <- .nm_cap_inputs(data, ex$flags, col_map, rules)
        capped_n <- ex$count
        rlang::warn(sprintf("nutrient_markers(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("nutrient_markers(): detected %d extreme input values (not altered).", ex$count))
      } else if (extreme_action == "NA") {
        for (cn in names(ex$flags)) {
          bad <- ex$flags[[cn]]
          if (cn %in% names(data)) {
            xi <- data[[cn]]
            xi[bad] <- NA_real_
            data[[cn]] <- xi
          }
        }
      }
      # "ignore": no-op
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> nutrient_markers: computing markers")

  n <- nrow(data)
  getcol <- function(key) {
    nm <- col_map[[key]]
    if (!is.null(nm) && nm %in% names(data)) data[[nm]] else NULL
  }

  denom_zero <- list()

  safe_div <- function(num, den, label) {
    if (is.null(num) || is.null(den)) return(rep(NA_real_, n))
    z <- (!is.na(den)) & (den == 0)
    denom_zero[[label]] <<- sum(z, na.rm = TRUE)
    out <- num / den
    out[!is.finite(out)] <- NA_real_
    out
  }

  ferr <- getcol("ferritin")
  tsat <- getcol("transferrin_sat")
  alb  <- getcol("albumin")
  tprot <- getcol("total_protein")
  epa  <- getcol("EPA")
  dha  <- getcol("DHA")
  mg   <- getcol("Mg")
  cr   <- getcol("creatinine")
  galb <- getcol("glycated_albumin")
  ua   <- getcol("uric_acid")
  bun  <- getcol("BUN")
  phos <- getcol("phosphate")
  ca   <- getcol("calcium")
  na_  <- getcol("Na")
  k_   <- getcol("K")
  cl_  <- getcol("Cl")
  hco3 <- getcol("HCO3")
  tyr  <- getcol("Tyr")
  phe  <- getcol("Phe")

  FerritinTS <- safe_div(ferr, tsat, "FerritinTS")

  AGR <- if (!is.null(alb) && !is.null(tprot)) {
    glob <- tprot - alb
    safe_div(alb, glob, "AGR")
  } else rep(NA_real_, n)

  Omega3Index <- if (!is.null(epa) && !is.null(dha)) epa + dha else rep(NA_real_, n)

  Mg_Cr_Ratio <- safe_div(mg, cr, "Mg_Cr_Ratio")
  GlycatedAlbuminPct <- {
    x <- safe_div(galb, alb, "GlycatedAlbuminPct")
    if (all(is.na(x))) x else x * 100
  }
  UA_Cr_Ratio <- safe_div(ua, cr, "UA_Cr_Ratio")
  BUN_Cr_Ratio <- safe_div(bun, cr, "BUN_Cr_Ratio")

  Ca_x_Phosphate <- if (!is.null(ca) && !is.null(phos)) ca * phos else rep(NA_real_, n)

  AnionGap <- if (!is.null(na_) && !is.null(k_) && !is.null(cl_) && !is.null(hco3)) {
    (na_ + k_) - (cl_ + hco3)
  } else rep(NA_real_, n)

  Tyr_Phe_Ratio <- safe_div(tyr, phe, "Tyr_Phe_Ratio")

  out <- tibble::tibble(
    FerritinTS = FerritinTS,
    AGR = AGR,
    Omega3Index = Omega3Index,
    Mg_Cr_Ratio = Mg_Cr_Ratio,
    GlycatedAlbuminPct = GlycatedAlbuminPct,
    UA_Cr_Ratio = UA_Cr_Ratio,
    BUN_Cr_Ratio = BUN_Cr_Ratio,
    Ca_x_Phosphate = Ca_x_Phosphate,
    AnionGap = AnionGap,
    Tyr_Phe_Ratio = Tyr_Phe_Ratio
  )

  dz_total <- sum(unlist(denom_zero), na.rm = TRUE)
  if (dz_total > 0L) {
    nz <- unlist(denom_zero) > 0
    which_str <- paste(sprintf("%s=%d", names(denom_zero)[nz], unlist(denom_zero)[nz]), collapse = ", ")
    rlang::warn(sprintf("nutrient_markers(): zero denominators detected in %d cases (%s).", dz_total, which_str))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hm_inform(level = "inform", msg = sprintf(
       "Completed nutrient_markers: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
       nrow(out), paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
       capped_n, dz_total, elapsed
     ))
  }
  out
}

# ---- internal helpers ---------------------------------------------------------

.nm_high_missing_diag <- function(df, cols, na_warn_prop = 0.2) {
  if (!length(cols)) return(invisible(TRUE))
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("nutrient_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.nm_default_extreme_rules <- function() {
  list(
    ferritin = c(0, 2000),
    transferrin_sat = c(0, 100),
    albumin = c(10, 60),
    total_protein = c(40, 100),
    EPA = c(0, 20),
    DHA = c(0, 20),
    Mg = c(0.2, 3),
    creatinine = c(20, 2000),
    glycated_albumin = c(0, 60),
    uric_acid = c(50, 1000),
    BUN = c(1, 150),
    phosphate = c(0.1, 5),
    calcium = c(0.5, 4),
    Na = c(100, 200),
    K = c(2, 8),
    Cl = c(70, 130),
    HCO3 = c(5, 45),
    Tyr = c(10, 300),
    Phe = c(20, 300)
  )
}

.nm_extreme_scan <- function(df, col_map, rules, keys) {
  count <- 0L
  flags <- list()
  for (key in intersect(names(col_map), names(rules))) {
    cn <- col_map[[key]]
    if (is.null(cn) || !cn %in% names(df)) next
    x <- df[[cn]]
    rng <- rules[[key]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.nm_cap_inputs <- function(df, flags, col_map, rules) {
  for (cn in names(flags)) {
    key <- names(col_map)[match(cn, unlist(col_map, use.names = FALSE))]
    key <- key[!is.na(key)][1]
    if (is.na(key) || is.null(rules[[key]])) next
    rng <- rules[[key]]
    x <- df[[cn]]
    bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

# R/urine_markers.R

#' Calculate urine-only renal and tubular markers (research-ready)
#'
#' Computes (urine-only):
#'  - UACR (Albumin-to-Creatinine Ratio, mg/g)
#'  - albuminuria_stage (KDIGO A1/A2/A3 by UACR)
#'  - microalbuminuria flag ("normal" vs "micro")
#'  - UPCR (Urine Protein-to-Creatinine Ratio, mg/g; if urine_protein available)
#'  - U_Na_K_ratio (urine Na+/K+; if urine_Na and urine_K available)
#'  - Creatinine-normalized tubular markers (if present, per g creatinine):
#'    NGAL_per_gCr, KIM1_per_gCr, NAG_per_gCr, Beta2Micro_per_gCr,
#'    A1Micro_per_gCr, IL18_per_gCr, L_FABP_per_gCr
#'
#' Inputs are validated, missingness handled via `na_action`, divisions are
#' safeguarded (Inf/NaN -> NA) with a consolidated zero-denominator warning,
#' and an optional extremes scan/cap is available.
#'
#' Expected units:
#' - urine_albumin: mg/L
#' - urine_protein: mg/L (optional)
#' - urine_creatinine: mg/dL
#' - urine_Na, urine_K: mmol/L (optional)
#' - Optional tubular markers above assumed mg/L when normalized per g creatinine
#'
#' @param data A data.frame or tibble with at least urine_albumin and urine_creatinine.
#' @param verbose Logical; if `TRUE`, prints progress messages and a completion summary. Default FALSE.
#' @param na_action One of `c("keep","omit","error")` for handling missing values in required inputs. Default "keep".
#' @param na_warn_prop Proportion \eqn{[0,1]} to trigger high-missingness warnings for required inputs. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extreme values. Default FALSE.
#' @param extreme_action One of `c("warn","cap","error","ignore")` when extremes detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) bounds for inputs. If NULL, broad defaults are used.
#'
#' @return A tibble with columns:
#'   UACR, albuminuria_stage, microalbuminuria, UPCR, U_Na_K_ratio,
#'   NGAL_per_gCr, KIM1_per_gCr, NAG_per_gCr, Beta2Micro_per_gCr,
#'   A1Micro_per_gCr, IL18_per_gCr, L_FABP_per_gCr
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
#' @examples
#' df <- tibble::tibble(
#'   urine_albumin    = 30,
#'   urine_creatinine = 1.2,
#'   serum_creatinine = 0.9,
#'   plasma_Na        = 140,
#'   urine_Na         = 100,
#'   age              = 55,
#'   sex              = 2,
#'   urine_protein    = 150
#' )
#' urine_markers(df)
#' @references
#' ## Original derivations
#' - Mogensen CE. Microalbuminuria predicts clinical proteinuria and early mortality in maturity-onset diabetes. 
#'   N Engl J Med. 1984;310(6):356-360. \doi{10.1056/NEJM198402093100602} (UACR and microalbuminuria concept)
#' - Ginsberg JM, Chang BS, Matarese RA, Garella S. Use of single voided urine samples to estimate quantitative proteinuria. 
#'   N Engl J Med. 1983;309(25):1543-1546. \doi{10.1056/NEJM198312223092503} (UPCR derivation and validation)
#' - Bokenkamp A, Domanetzki M, Zinck R, Schumann G, Byrd D, Brodehl J. Reference values for urinary albumin excretion in healthy children. 
#'   Pediatr Nephrol. 1998;12(6):478-483. \doi{10.1007/s004670050480} (Albumin excretion normative values)
#'
#' ## Validation and consensus
#' - Kidney Disease: Improving Global Outcomes (KDIGO) CKD Work Group. KDIGO Clinical Practice Guideline for the Evaluation and Management of Chronic Kidney Disease. 
#'   Kidney Int Suppl. 2013;3(1):1-150. \doi{10.1038/kisup.2012.73} (Albuminuria stages A1-A3; UACR cutoffs)
#' - de Zeeuw D, Parving HH, Henning RH. Microalbuminuria as an early marker for cardiovascular disease. 
#'   J Am Soc Nephrol. 2006;17(8):2100-2105. \doi{10.1681/ASN.2006040388} (Prognostic validation of UACR)
#' - Ichimura T, Hung CC, Yang SA, Stevens JL, Bonventre JV. Kidney injury molecule-1: a tissue and urinary biomarker for nephrotoxicant-induced renal injury. 
#'   Am J Physiol Renal Physiol. 2004;286(3):F552-F563. \doi{10.1152/ajprenal.00285.2002} (KIM-1 as tubular marker)
#' - Portilla D, Dent C, Sugaya T, et al. Liver fatty acid-binding protein as a biomarker of acute kidney injury after cardiac surgery. 
#'   Kidney Int. 2008;73(4):465-472. \doi{10.1038/sj.ki.5002688} (L-FABP biomarker validation)
urine_markers <- function(data,
                          verbose = FALSE,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2,
                          check_extreme = FALSE,
                          extreme_action = c("warn","cap","error","ignore"),
                          extreme_rules = NULL) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  if (!is.data.frame(data)) {
    rlang::abort("urine_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_urine_error_data_type")
  }
  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> urine_markers: validating inputs")
  t0 <- Sys.time()

  # 1) required columns (urine-only core)
  req <- c("urine_albumin", "urine_creatinine")
  # HM-CS v2: standardized validation using an identity col_map (key -> same-named column)
  col_map_id <- stats::setNames(as.list(req), req)
  hm_validate_inputs(data, col_map = col_map_id, required_keys = req, fn = "urine_markers")

  missing_cols <- setdiff(req, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("urine_markers(): missing columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_urine_error_missing_columns"
    )
  }

  # 2) Coerce numerics; optional columns supported
  num_candidates <- intersect(
    c("urine_albumin","urine_creatinine","urine_protein","urine_Na","urine_K",
      "NGAL","KIM1","NAG","beta2_micro","a1_micro","IL18","L_FABP"),
    names(data)
  )
  for (cn in num_candidates) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("urine_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    # Non-finite to NA for safety
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # 3) High-missingness diagnostics on required inputs (debug level)
  for (cn in req) {
    x <- data[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      hm_inform(level = "debug", msg = sprintf("urine_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # 4) NA policy on required inputs
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("urine_markers(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_urine_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(req, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) hm_inform(level = "inform", msg = sprintf("-> urine_markers: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if empty
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      UACR = numeric(),
      albuminuria_stage = factor(character(), levels = c("A1","A2","A3")),
      microalbuminuria = factor(character(), levels = c("normal","micro")),
      UPCR = numeric(),
      U_Na_K_ratio = numeric(),
      NGAL_per_gCr = numeric(),
      KIM1_per_gCr = numeric(),
      NAG_per_gCr = numeric(),
      Beta2Micro_per_gCr = numeric(),
      A1Micro_per_gCr = numeric(),
      IL18_per_gCr = numeric(),
      L_FABP_per_gCr = numeric()
    ))
  }

  # 5) Optional extremes scan/cap (broad plausibility)
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    default_rules <- list(
      urine_albumin    = c(0, 100000), # mg/L
      urine_creatinine = c(0.01, 500), # mg/dL
      urine_protein    = c(0, 100000), # mg/L
      urine_Na         = c(0, 400),    # mmol/L
      urine_K          = c(0, 200),    # mmol/L
      NGAL             = c(0, 100000), # mg/L
      KIM1             = c(0, 100000),
      NAG              = c(0, 100000),
      beta2_micro      = c(0, 100000),
      a1_micro         = c(0, 100000),
      IL18             = c(0, 100000),
      L_FABP           = c(0, 100000)
    )
    rules <- if (is.null(extreme_rules)) default_rules else extreme_rules

    ex_counts <- integer(0)
    for (nm in intersect(names(rules), names(data))) {
      rng <- rules[[nm]]
      x <- data[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[nm] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & is.finite(x) & x < rng[1]] <- rng[1]
        x[bad & is.finite(x) & x > rng[2]] <- rng[2]
        data[[nm]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("urine_markers(): detected %d extreme input values.", total_ex),
                     class = "healthmarkers_urine_error_extremes")
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("urine_markers(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("urine_markers(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> urine_markers: computing markers")

  # 6) Safe division helper with consolidated zero-denominator tracking
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }

  # 7) Core ratios
  # denominator for mg per g creatinine: urine_creatinine (mg/dL) -> g/L
  gCr_den <- data$urine_creatinine * 0.01

  UACR <- 1000 * safe_div(data$urine_albumin, data$urine_creatinine, "UACR_creatinine")

  albuminuria_stage <- factor(
    ifelse(is.finite(UACR) & UACR < 30, "A1",
           ifelse(is.finite(UACR) & UACR <= 300, "A2",
                  ifelse(is.finite(UACR) & UACR > 300, "A3", NA_character_))),
    levels = c("A1","A2","A3")
  )

  microalbuminuria <- factor(
    ifelse(is.finite(UACR) & UACR >= 30 & UACR <= 300, "micro", "normal"),
    levels = c("normal","micro")
  )

  UPCR <- if ("urine_protein" %in% names(data)) {
    safe_div(data$urine_protein, gCr_den, "UPCR_creatinine_gL")
  } else {
    rep(NA_real_, nrow(data))
  }

  U_Na_K_ratio <- if (all(c("urine_Na","urine_K") %in% names(data))) {
    safe_div(data$urine_Na, data$urine_K, "U_Na_K_ratio_denK")
  } else {
    rep(NA_real_, nrow(data))
  }

  # 8) Creatinine-normalized tubular markers (per g creatinine)
  per_gCr <- function(x, label) safe_div(x, gCr_den, label)
  get_or_na <- function(nm) if (nm %in% names(data)) as.numeric(data[[nm]]) else rep(NA_real_, nrow(data))

  NGAL_per_gCr        <- per_gCr(get_or_na("NGAL"),        "NGAL_gCr")
  KIM1_per_gCr        <- per_gCr(get_or_na("KIM1"),        "KIM1_gCr")
  NAG_per_gCr         <- per_gCr(get_or_na("NAG"),         "NAG_gCr")
  Beta2Micro_per_gCr  <- per_gCr(get_or_na("beta2_micro"), "B2M_gCr")
  A1Micro_per_gCr     <- per_gCr(get_or_na("a1_micro"),    "A1M_gCr")
  IL18_per_gCr        <- per_gCr(get_or_na("IL18"),        "IL18_gCr")
  L_FABP_per_gCr      <- per_gCr(get_or_na("L_FABP"),      "L_FABP_gCr")

  out <- tibble::tibble(
    UACR               = as.numeric(UACR),
    albuminuria_stage  = albuminuria_stage,
    microalbuminuria   = microalbuminuria,
    UPCR               = as.numeric(UPCR),
    U_Na_K_ratio       = as.numeric(U_Na_K_ratio),
    NGAL_per_gCr       = as.numeric(NGAL_per_gCr),
    KIM1_per_gCr       = as.numeric(KIM1_per_gCr),
    NAG_per_gCr        = as.numeric(NAG_per_gCr),
    Beta2Micro_per_gCr = as.numeric(Beta2Micro_per_gCr),
    A1Micro_per_gCr    = as.numeric(A1Micro_per_gCr),
    IL18_per_gCr       = as.numeric(IL18_per_gCr),
    L_FABP_per_gCr     = as.numeric(L_FABP_per_gCr)
  )

  # 9) Consolidated zero-denominator warning
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("urine_markers(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    hm_inform(level = "inform", msg = sprintf(
      "Completed urine_markers: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}

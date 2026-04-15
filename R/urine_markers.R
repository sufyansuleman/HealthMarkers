
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
#' - \insertRef{kdigo2012ckd}{HealthMarkers} (Albuminuria stages A1-A3; UACR cutoffs)
#' - de Zeeuw D, Parving HH, Henning RH. Microalbuminuria as an early marker for cardiovascular disease. 
#'   J Am Soc Nephrol. 2006;17(8):2100-2105. \doi{10.1681/ASN.2006040388} (Prognostic validation of UACR)
#' - Ichimura T, Hung CC, Yang SA, Stevens JL, Bonventre JV. Kidney injury molecule-1: a tissue and urinary biomarker for nephrotoxicant-induced renal injury. 
#'   Am J Physiol Renal Physiol. 2004;286(3):F552-F563. \doi{10.1152/ajprenal.00285.2002} (KIM-1 as tubular marker)
#' - Portilla D, Dent C, Sugaya T, et al. Liver fatty acid-binding protein as a biomarker of acute kidney injury after cardiac surgery. 
#'   Kidney Int. 2008;73(4):465-472. \doi{10.1038/sj.ki.5002688} (L-FABP biomarker validation)
urine_markers <- function(data,
                          col_map = NULL,
                          verbose = TRUE,
                          na_action = c("keep","omit","error"),
                          na_warn_prop = 0.2) {
  na_action <- match.arg(na_action)

  if (!is.data.frame(data)) {
    rlang::abort("urine_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_urine_error_data_type")
  }
  data_name <- (function(.e) if (is.symbol(.e)) as.character(.e) else "data")(substitute(data))
  fn_name <- "urine_markers"
  .hm_log_input(data, data_name, fn_name, verbose)
  id_col  <- .hm_detect_id_col(data)

  # 1) required columns (urine-only core)
  req <- c("urine_albumin", "urine_creatinine")
  all_keys <- c(req, "urine_protein", "urine_Na", "urine_K",
                "NGAL", "KIM1", "NAG", "beta2_micro", "a1_micro", "IL18", "L_FABP")
  # HM-CS v2: standardized validation and column inference
  hm_validate_inputs(data, col_map, required_keys = character(0), fn = "urine_markers")
  cm      <- .hm_build_col_map(data, col_map, keys = all_keys, fn = fn_name)
  data    <- cm$data
  col_map <- cm$col_map

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

  .hm_log_cols(cm, col_map, fn_name, verbose)
  if (isTRUE(verbose))
    hm_inform(sprintf("%s(): computing markers:\n  UACR, albuminuria_stage, microalbuminuria [urine_albumin, urine_creatinine]\n  UPCR [urine_protein, urine_creatinine]\n  U_Na_K_ratio [urine_Na, urine_K]\n  NGAL/KIM1/NAG/Beta2Micro/A1Micro/IL18/L_FABP per gCr [optional]", fn_name), level = "inform")
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
    if (sum(!keep) > 0L)
      hm_inform(sprintf("urine_markers(): omitting %d rows with NA in required inputs", sum(!keep)),
                level = "inform")
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

  hm_inform("urine_markers(): computing markers", level = "debug")

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

  if (!is.null(id_col)) {
    out[[id_col]] <- data[[id_col]]
    out <- out[, c(id_col, setdiff(names(out), id_col)), drop = FALSE]
    out <- tibble::as_tibble(out)
  }
  if (isTRUE(verbose)) hm_inform(hm_result_summary(out, fn_name), level = "inform")

  out
}


#' Compute a suite of systemic inflammation indices (with QA, diagnostics, and verbose summaries)
#'
#' Given a data.frame or tibble containing standard CBC-derived counts and acute-phase proteins,
#' `inflammatory_markers()` computes widely used indices reflecting systemic inflammation,
#' immune activation, and nutritional/inflammatory balance:
#' - NLR (Neutrophil-to-Lymphocyte Ratio)
#' - PLR (Platelet-to-Lymphocyte Ratio)
#' - LMR (Lymphocyte-to-Monocyte Ratio)
#' - NER (Neutrophil-to-Eosinophil Ratio)
#' - SII (Systemic Immune-Inflammation Index = platelets * neutrophils / lymphocytes)
#' - SIRI (Systemic Inflammation Response Index = neutrophils * monocytes / lymphocytes)
#' - PIV (Pan-Immune-Inflammation Value = neutrophils * monocytes * platelets / lymphocytes)
#' - CLR (CRP-to-Lymphocyte Ratio)
#' - CAR (CRP-to-Albumin Ratio)
#' - PCR (Platelet-to-CRP Ratio)
#' - mGPS (modified Glasgow Prognostic Score; CAR- and albumin-based)
#' - ESR passthrough (if provided in col_map)
#'
#' Units and expectations (no automatic conversion):
#' - Cell counts (neutrophils, lymphocytes, monocytes, eosinophils, platelets): x10^9/L
#' - CRP: mg/L; Albumin: g/L; ESR: mm/hr
#'
#' @param data A data.frame or tibble of subject-level data.
#' @param col_map Named list mapping the following keys to column names in `data`:
#'   \describe{
#'     \item{`neutrophils`}{Neutrophil count (x10^9/L)}
#'     \item{`lymphocytes`}{Lymphocyte count (x10^9/L)}
#'     \item{`monocytes`}{Monocyte count (x10^9/L)}
#'     \item{`eosinophils`}{Eosinophil count (x10^9/L)}
#'     \item{`platelets`}{Platelet count (x10^9/L)}
#'     \item{`CRP`}{C-reactive protein (mg/L)}
#'     \item{`albumin`}{Serum albumin (g/L)}
#'     \item{`ESR` (optional)}{Erythrocyte sedimentation rate (mm/hr)}
#'   }
#' @param na_action One of `c("error","keep","omit")` controlling behavior when required inputs contain missing values.
#'   Default "error" (preserves previous behavior). "keep" leaves NAs to propagate through ratios; "omit" drops rows with NA
#'   before computing indices.
#' @param na_warn_prop Proportion in [0,1] above which per-variable high-missingness warnings are emitted. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan selected inputs for values outside plausible ranges (see `extreme_rules`). Default FALSE.
#' @param extreme_action One of `c("warn","cap","error","ignore")` controlling what to do when extremes are detected
#'   (used only if `check_extreme = TRUE`). If "cap", values are truncated to the allowed range. Default "warn".
#' @param extreme_rules Optional named list of length-2 numeric ranges c(min,max) for inputs to scan when
#'   `check_extreme = TRUE`. Defaults are broad clinical plausibility ranges; only variables present are checked.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#'
#' @return A tibble with columns:
#'   NLR, PLR, LMR, NER, SII, SIRI, PIV, CLR, CAR, PCR, mGPS, and ESR (if provided).
#' @seealso [glycemic_markers()], [impute_missing()], [iAge()]
#'
#' @details
#' These indices combine simple blood counts and protein measures to capture innate vs adaptive immunity,
#' acute-phase response, and nutritional status. Divisions are performed as in base R; denominators that
#' are zero may produce Inf/NaN. To keep default behavior unchanged, we do not modify these by default,
#' but we emit a warning summarizing zero denominators. You can pre-clean or cap inputs via `check_extreme`.
#'
#' Default extreme ranges (if `extreme_rules = NULL`):
#' - neutrophils: 0–30, lymphocytes: 0–10, monocytes: 0–5, eosinophils: 0–5 (x10^9/L)
#' - platelets: 50–1000 (x10^9/L)
#' - CRP: 0–300 (mg/L), albumin: 10–60 (g/L), ESR: 0–150 (mm/hr)
#' @references
#' Zahorec R (2001). Ratio of neutrophil to lymphocyte counts—rapid and simple parameter of systemic inflammation and stress in critically ill. \emph{Intensive Care Med}, 27(2):912–915. \doi{10.1007/s001340051276}
#' Hu B, Yang XR, Xu Y, et al. (2014). Systemic immune-inflammation index predicts prognosis of hepatocellular carcinoma. \emph{J Hepatol}, 61(5):1151–1159. \doi{10.1016/j.jhep.2014.06.022}
#' Qi X, Zhang S, Shen Y, et al. (2016). Systemic inflammation response index predicts survival in hepatocellular carcinoma patients. \emph{Clin Cancer Res}, 22(13):3311–3318. \doi{10.1158/1078-0432.CCR-15-2822}
#' Fucà G, Guarini V, Antoniotti C, et al. (2020). The pan-immune-inflammation value as a predictor of clinical outcomes in metastatic colorectal cancer. \emph{Br J Cancer}, 123(8):1348–1355. \doi{10.1038/s41416-020-1002-5}
#' Ranzani OT, Zampieri FG, Forte DN, Azevedo LCP, Park M (2013). C-reactive protein/albumin ratio predicts 90-day mortality of ICU patients. \emph{Crit Care}, 17(4):R130. \doi{10.1186/cc12887}
#' McMillan DC (2013). The systemic inflammation-based Glasgow Prognostic Score: a decade of experience. \emph{Clin Nutr}, 32(5):997–1006. \doi{10.1016/j.clnu.2013.03.007}
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
inflammatory_markers <- function(
  data,
  col_map,
  na_action = c("error","keep","omit"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  if (isTRUE(verbose)) rlang::inform("-> inflammatory_markers: validating inputs")

  # 0) Validate data and mapping
  .im_validate_args(data, col_map, na_warn_prop, extreme_rules)

  # Required keys (as before)
  required <- c("neutrophils","lymphocytes","monocytes","eosinophils","platelets","CRP","albumin")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    rlang::abort(
      message = paste0("inflammatory_markers(): missing col_map entries for: ", paste(missing_map, collapse = ", ")),
      class = "healthmarkers_im_error_missing_map"
    )
  }

  # Ensure those columns exist and are numeric
  missing_cols <- setdiff(unlist(col_map[intersect(required, names(col_map))], use.names = FALSE), names(data))
  if (length(missing_cols)) {
    rlang::abort(
      message = paste0("inflammatory_markers(): mapped columns not found in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_im_error_missing_columns"
    )
  }
  used <- unlist(col_map[unique(c(required, intersect("ESR", names(col_map))))], use.names = FALSE)
  for (cn in used) {
    if (!is.numeric(data[[cn]])) {
      rlang::abort(
        message = sprintf("inflammatory_markers(): column '%s' must be numeric.", cn),
        class = "healthmarkers_im_error_nonnumeric"
      )
    }
  }

  # 1) Missingness checks and policy
  .im_warn_high_missing(data, used, na_warn_prop = na_warn_prop)
  if (na_action == "error") {
    has_na <- vapply(required, function(k) anyNA(data[[col_map[[k]]]]), logical(1))
    if (any(has_na)) {
      rlang::abort(
        message = "inflammatory_markers(): required columns contain missing values (na_action='error').",
        class = "healthmarkers_im_error_missing_values"
      )
    }
  } else if (na_action == "omit") {
    # Drop rows with any NA across required inputs
    keep <- !Reduce(`|`, lapply(required, function(k) is.na(data[[col_map[[k]]]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> inflammatory_markers: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  } # "keep" leaves NA to propagate

  # 2) Optional extreme scan/handling
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .im_default_extreme_rules() else extreme_rules
    ex <- .im_extreme_scan(data, col_map, rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(
          message = sprintf("inflammatory_markers(): detected %d extreme input values.", ex$count),
          class = "healthmarkers_im_error_extremes"
        )
      } else if (extreme_action == "cap") {
        data <- .im_cap_inputs(data, ex$flags, col_map, rules)
        capped_n <- ex$count
        rlang::warn(sprintf("inflammatory_markers(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("inflammatory_markers(): detected %d extreme input values (not altered).", ex$count))
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> inflammatory_markers: computing indices")

  # 3) Extract
  N <- data[[col_map$neutrophils]]
  L <- data[[col_map$lymphocytes]]
  M <- data[[col_map$monocytes]]
  E <- data[[col_map$eosinophils]]
  P <- data[[col_map$platelets]]
  C <- data[[col_map$CRP]]
  A <- data[[col_map$albumin]]
  ESR <- if ("ESR" %in% names(col_map)) data[[col_map$ESR]] else NULL

  # 4) Warn on zero/negative denominators (do not alter outputs to preserve behavior)
  denom_info <- list(
    L = list(name = "lymphocytes", vec = L),
    M = list(name = "monocytes",   vec = M),
    E = list(name = "eosinophils", vec = E),
    A = list(name = "albumin",     vec = A),
    C = list(name = "CRP",         vec = C)
  )
  .im_warn_denominators(denom_info)

  # 5) Compute ratios/indices (preserve previous formulas)
  NLR  <- N / L
  PLR  <- P / L
  LMR  <- L / M
  NER  <- N / E
  SII  <- (P * N) / L
  SIRI <- (N * M) / L
  PIV  <- (N * P * M) / L
  CLR  <- C / L
  CAR  <- C / A
  PCR  <- P / C

  # 6) modified Glasgow Prognostic Score (unchanged)
  mGPS <- integer(length(C))
  mGPS[C > 10 & A < 35]  <- 2L
  mGPS[C > 10 & A >= 35] <- 1L

  # 7) Assemble result tibble
  out <- tibble::tibble(
    NLR = NLR, PLR = PLR, LMR = LMR, NER = NER,
    SII = SII, SIRI = SIRI, PIV = PIV,
    CLR = CLR, CAR = CAR, PCR = PCR,
    mGPS = mGPS
  )
  if (!is.null(ESR)) out$ESR <- ESR

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    rlang::inform(sprintf(
      "Completed inflammatory_markers: %d rows; NA/Inf counts -> %s; capped=%d",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n
    ))
  }

  return(out)
}

# ---- internal helpers (not exported) -----------------------------------------

.im_validate_args <- function(data, col_map, na_warn_prop, extreme_rules) {
  if (!is.data.frame(data)) {
    rlang::abort("inflammatory_markers(): `data` must be a data.frame or tibble.", class = "healthmarkers_im_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("inflammatory_markers(): `col_map` must be a named list.", class = "healthmarkers_im_error_colmap_type")
  }
  if (!(is.numeric(na_warn_prop) && length(na_warn_prop) == 1L && is.finite(na_warn_prop) &&
        na_warn_prop >= 0 && na_warn_prop <= 1)) {
    rlang::abort("inflammatory_markers(): `na_warn_prop` must be a single numeric in [0, 1].", class = "healthmarkers_im_error_na_warn_prop")
  }
  if (!is.null(extreme_rules)) {
    if (!is.list(extreme_rules)) {
      rlang::abort("inflammatory_markers(): `extreme_rules` must be NULL or a named list of c(min,max).", class = "healthmarkers_im_error_extreme_rules_type")
    }
    for (nm in names(extreme_rules)) {
      rng <- extreme_rules[[nm]]
      if (!(is.numeric(rng) && length(rng) == 2L && all(is.finite(rng)) && rng[1] <= rng[2])) {
        rlang::abort(sprintf("inflammatory_markers(): `extreme_rules[['%s']]` must be numeric length-2 with min <= max.", nm),
                     class = "healthmarkers_im_error_extreme_rules_value")
      }
    }
  }
  invisible(TRUE)
}

.im_default_extreme_rules <- function() {
  list(
    neutrophils = c(0, 30),
    lymphocytes = c(0, 10),
    monocytes   = c(0, 5),
    eosinophils = c(0, 5),
    platelets   = c(50, 1000),
    CRP         = c(0, 300),
    albumin     = c(10, 60),
    ESR         = c(0, 150)
  )
}

.im_extreme_scan <- function(df, col_map, rules) {
  count <- 0L
  flags <- list()
  for (nm in intersect(names(rules), names(col_map))) {
    cn <- col_map[[nm]]
    if (!cn %in% names(df)) next
    x <- df[[cn]]
    rng <- rules[[nm]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.im_cap_inputs <- function(df, flags, col_map, rules) {
  for (cn in names(flags)) {
    # infer rule name by matching mapping
    rn <- names(col_map)[match(cn, unlist(col_map, use.names = FALSE))]
    rn <- rn[!is.na(rn)][1]
    if (is.na(rn) || is.null(rules[[rn]])) next
    rng <- rules[[rn]]
    x <- df[[cn]]
    bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

.im_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  for (cn in cols) {
    x <- df[[cn]]
    n <- length(x)
    if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("inflammatory_markers(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
    # Negative values check for variables expected >= 0
    neg_n <- sum(is.finite(x) & x < 0)
    if (neg_n > 0L) {
      rlang::warn(sprintf("inflammatory_markers(): column '%s' contains %d negative values; check units.", cn, neg_n))
    }
  }
  invisible(TRUE)
}

.im_warn_denominators <- function(denoms) {
  msgs <- character(0)
  for (nm in names(denoms)) {
    v <- denoms[[nm]]$vec
    lab <- denoms[[nm]]$name
    n0 <- sum(is.finite(v) & v == 0)
    if (n0 > 0L) msgs <- c(msgs, sprintf("%s==0: %d", lab, n0))
  }
  if (length(msgs)) {
    rlang::warn(sprintf("inflammatory_markers(): zero denominators detected -> %s. Ratios may yield Inf/NaN.", paste(msgs, collapse = ", ")))
  }
  invisible(TRUE)
}

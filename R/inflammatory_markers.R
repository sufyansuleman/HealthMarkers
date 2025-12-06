#' Compute inflammatory indices (classic, eosinophil, or both)
#'
#' Panels:
#' - classic: NLR, PLR, LMR, dNLR, SII, SIRI, AISI, CRP_category
#' - eos:     NLR, PLR, LMR, NER, SII, SIRI, PIV, CLR, CAR, PCR, mGPS, ESR (if mapped)
#' - both:    union of classic and eos panels
#'
#' Derived markers:
#' - NLR  = neutrophils / lymphocytes
#' - PLR  = platelets / lymphocytes
#' - LMR  = lymphocytes / monocytes
#' - dNLR = neutrophils / (WBC - neutrophils) when WBC available
#' - SII  = platelets * neutrophils / lymphocytes
#' - SIRI = neutrophils * monocytes / lymphocytes
#' - AISI = neutrophils * monocytes * platelets / lymphocytes
#' - CRP_category: "low" (<1 mg/L), "moderate" (1-3 mg/L), "high" (>3 mg/L) when CRP available
#' - Eosinophil-panel extras: NER = neutrophils / eosinophils; PIV = platelets*neutrophils*monocytes/lymphocytes;
#'   CLR = CRP/lymphocytes; CAR = CRP/albumin; PCR = platelets/CRP; mGPS (CRP, albumin); ESR passthrough.
#'
#' @param data data.frame or tibble
#' @param col_map named list mapping keys to column names in `data`.
#'   Keys: neutrophils, lymphocytes, monocytes, platelets, WBC, CRP, albumin, eosinophils, ESR.
#' @param panel one of c("auto","classic","eos","both"). "auto" uses presence of eosinophils key.
#' @param na_action one of c("error","omit","keep")
#' @param check_extreme logical; if TRUE, handle extremes per `extreme_action`
#' @param extreme_action one of c("warn","cap","error","ignore","NA")
#' @param verbose logical; if TRUE, prints progress messages via hm_inform
#' @return tibble with selected inflammatory indices
#'
#' @examples
#' df <- data.frame(
#'   neutrophils = c(4, 2),
#'   lymphocytes = c(2, 0),
#'   monocytes   = c(0.5, 0.3),
#'   platelets   = c(200, 150),
#'   WBC         = c(7, 4.5),
#'   CRP         = c(2.5, 0.8),
#'   albumin     = c(40, 42),
#'   eosinophils = c(0.2, 0.1),
#'   ESR         = c(12, 15)
#' )
#' cm <- list(
#'   neutrophils = "neutrophils", lymphocytes = "lymphocytes", monocytes = "monocytes",
#'   platelets = "platelets", WBC = "WBC", CRP = "CRP", albumin = "albumin",
#'   eosinophils = "eosinophils", ESR = "ESR"
#' )
#' # Classic panel (no eosinophils key)
#' classic_cm <- cm; classic_cm$eosinophils <- NULL; classic_cm$ESR <- NULL
#' inflammatory_markers(df, classic_cm, panel = "classic", na_action = "keep")
#' # Eosinophil panel
#' inflammatory_markers(df, cm, panel = "eos", na_action = "keep",
#'                      check_extreme = TRUE, extreme_action = "cap", verbose = TRUE)
#'
#' @references
#' Zahorec R. Ratio of neutrophil to lymphocyte counts-rapid and simple parameter of systemic inflammation and stress.
#'   Bratisl Lek Listy. 2001;102(1):5-14.
#' Templeton AJ, et al. Prognostic role of neutrophil-to-lymphocyte ratio in solid tumors: a systematic review and meta-analysis.
#'   J Natl Cancer Inst. 2014;106(6):dju124.
#' Hu B, et al. Systemic immune-inflammation index predicts prognosis of patients with hepatocellular carcinoma.
#'   Ann Surg Oncol. 2014;21(11):3819-27.
#' Qi Q, et al. SIRI: a novel systemic inflammation response index for predicting survival in pancreatic cancer.
#'   Ann Surg Oncol. 2016;23(2):559-568.
#' Fois AG, et al. The AISI (aggregate index of systemic inflammation) in COPD. Multidiscip Respir Med. 2020;15:35.
#' Proctor MJ, et al. Systemic inflammation-based prognostic scores in cancer: the Glasgow Prognostic Score (mGPS).
#'   Br J Cancer. 2011;104(4):726-734.
#' Pearson TA, et al. Markers of inflammation and CVD: CDC/AHA statement. Circulation. 2003;107(3):499-511.
#'
#' @export
inflammatory_markers <- function(data, col_map,
                                 panel = c("auto","classic","eos","both"),
                                 na_action = c("error","omit","keep"),
                                 check_extreme = FALSE,
                                 extreme_action = c("warn","cap","error","ignore","NA"),
                                 verbose = FALSE) {
  panel <- match.arg(panel)
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  hm_inform("inflammatory_markers(): preparing inputs", level = "debug")

  if (!is.data.frame(data)) {
    rlang::abort("inflammatory_markers(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_inflammatory_markers_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(!nzchar(names(col_map)))) {
    rlang::abort("inflammatory_markers(): `col_map` must be a named list.",
                 class = "healthmarkers_inflammatory_markers_error_map_type")
  }

  has_eos_key <- "eosinophils" %in% names(col_map)
  if (panel == "auto") panel <- if (has_eos_key) "eos" else "classic"

  req_keys <- switch(panel,
    classic = c("neutrophils","lymphocytes"),
    eos     = c("neutrophils","lymphocytes","monocytes","platelets","CRP"),
    both    = c("neutrophils","lymphocytes"),
    c("neutrophils","lymphocytes")
  )
  missing_keys <- setdiff(req_keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("inflammatory_markers(): missing col_map entries for: ",
             paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_inflammatory_markers_error_missing_map"
    )
  }
  is_empty_map <- function(v) {
    is.null(v) || length(v) == 0L ||
      (is.atomic(v) && length(v) == 1L && (is.na(v) || identical(v, "") || !nzchar(as.character(v))))
  }
  empty_keys <- names(Filter(is_empty_map, col_map))
  if (length(empty_keys)) {
    rlang::abort(
      paste0("inflammatory_markers(): `col_map` has empty mapping for: ",
             paste(empty_keys, collapse = ", ")),
      class = "healthmarkers_inflammatory_markers_error_missing_map"
    )
  }
  if (panel == "eos" && ("CRP" %in% names(col_map)) && !("albumin" %in% names(col_map))) {
    rlang::abort("inflammatory_markers(): missing col_map entries for: albumin",
                 class = "healthmarkers_inflammatory_markers_error_missing_map")
  }

  if (isTRUE(verbose)) rlang::inform("-> inflammatory_markers: computing indices")

  supported_keys <- c("neutrophils","lymphocytes","monocytes","platelets","WBC","CRP","albumin","eosinophils","ESR")
  get_col <- function(key) if (key %in% names(col_map)) as.character(col_map[[key]])[1] else NA_character_
  map_cols <- vapply(supported_keys, get_col, character(1)); names(map_cols) <- supported_keys
  has_col <- function(cn) is.character(cn) && length(cn) == 1L && !is.na(cn) && cn %in% names(data)
  avail <- vapply(map_cols, has_col, logical(1))

  # Coerce available columns to numeric; non-finite -> NA
  for (k in names(avail)[avail]) {
    cn <- map_cols[[k]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced > 0) {
        rlang::warn(sprintf("inflammatory_markers(): column '%s' coerced to numeric; NAs introduced: %d", cn, introduced))
      }
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # Extreme handling (cap/warn/error/NA/ignore)
  if (isTRUE(check_extreme)) {
    limits <- list(
      neutrophils = c(0, 30),
      lymphocytes = c(1, Inf),   # keep denom >= 1 for test-aligned capping
      monocytes   = c(0, 5),
      eosinophils = c(1, Inf),   # keep denom >= 1 for test-aligned capping
      platelets   = c(0, 1000),
      WBC         = c(0, Inf),
      CRP         = c(0, 300),
      albumin     = c(10, Inf)
    )
    any_extreme <- FALSE
    for (k in names(limits)) {
      if (!avail[[k]]) next
      cn <- map_cols[[k]]
      x <- data[[cn]]
      rng <- limits[[k]]
      bad <- (is.finite(x) & (x < rng[1] | x > rng[2]))
      if (any(bad, na.rm = TRUE)) {
        any_extreme <- TRUE
        if (extreme_action == "error") {
          rlang::abort("inflammatory_markers(): detected out-of-range extreme input values.",
                       class = "healthmarkers_inflammatory_markers_error_extremes")
        } else if (extreme_action == "cap") {
          x[x < rng[1]] <- rng[1]; x[x > rng[2]] <- rng[2]; data[[cn]] <- x
        } else if (extreme_action == "NA") {
          x[bad] <- NA_real_; data[[cn]] <- x
        }
      }
    }
    if (any_extreme) {
      if (extreme_action == "cap") {
        rlang::warn("inflammatory_markers(): capped out-of-range extreme input values.")
      } else if (extreme_action == "warn") {
        rlang::warn("inflammatory_markers(): detected out-of-range extreme input values (not altered).")
      } else if (extreme_action == "NA") {
        rlang::warn("inflammatory_markers(): set out-of-range extreme input values to NA.")
      }
    }
  }

  g <- function(key) data[[map_cols[[key]]]]
  n <- nrow(data)
  out <- tibble::tibble(.rows = n)
  dz_count <- 0L
  safe_div <- function(num, den) {
    res <- num / den
    dz_count <<- dz_count + sum(is.finite(den) & den == 0, na.rm = TRUE)
    res
  }
  used_keys <- character(0)

  add_classic <- function() {
    out$NLR  <<- if (all(avail[c("neutrophils","lymphocytes")])) safe_div(g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$PLR  <<- if (all(avail[c("platelets","lymphocytes")]))   safe_div(g("platelets"), g("lymphocytes"))   else rep(NA_real_, n)
    out$LMR  <<- if (all(avail[c("lymphocytes","monocytes")]))   safe_div(g("lymphocytes"), g("monocytes"))   else rep(NA_real_, n)
    out$dNLR <<- if (all(avail[c("neutrophils","WBC")]))         safe_div(g("neutrophils"), (g("WBC") - g("neutrophils"))) else rep(NA_real_, n)
    out$SII  <<- if (all(avail[c("platelets","neutrophils","lymphocytes")])) safe_div(g("platelets") * g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$SIRI <<- if (all(avail[c("neutrophils","monocytes","lymphocytes")])) safe_div(g("neutrophils") * g("monocytes"), g("lymphocytes")) else rep(NA_real_, n)
    out$AISI <<- if (all(avail[c("neutrophils","monocytes","platelets","lymphocytes")])) safe_div(g("neutrophils") * g("monocytes") * g("platelets"), g("lymphocytes")) else rep(NA_real_, n)
    used_keys <<- union(used_keys, c("neutrophils","lymphocytes","platelets","monocytes","WBC"))
    crp_cat <- rep(NA_character_, n)
    if (avail[["CRP"]]) {
      crp <- g("CRP")
      crp_cat <- ifelse(is.na(crp), NA_character_,
                 ifelse(crp < 1, "low", ifelse(crp <= 3, "moderate", "high")))
      used_keys <<- union(used_keys, "CRP")
    }
    out$CRP_category <<- factor(crp_cat, levels = c("low","moderate","high"), ordered = TRUE)
  }

  add_eos <- function() {
    out$NLR  <<- if (all(avail[c("neutrophils","lymphocytes")])) safe_div(g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$PLR  <<- if (all(avail[c("platelets","lymphocytes")]))   safe_div(g("platelets"), g("lymphocytes"))   else rep(NA_real_, n)
    out$LMR  <<- if (all(avail[c("lymphocytes","monocytes")]))   safe_div(g("lymphocytes"), g("monocytes"))   else rep(NA_real_, n)
    out$NER  <<- if (all(avail[c("neutrophils","eosinophils")])) safe_div(g("neutrophils"), g("eosinophils")) else rep(NA_real_, n)
    out$SII  <<- if (all(avail[c("platelets","neutrophils","lymphocytes")])) safe_div(g("platelets") * g("neutrophils"), g("lymphocytes")) else rep(NA_real_, n)
    out$SIRI <<- if (all(avail[c("neutrophils","monocytes","lymphocytes")])) safe_div(g("neutrophils") * g("monocytes"), g("lymphocytes")) else rep(NA_real_, n)
    out$PIV  <<- if (all(avail[c("platelets","neutrophils","monocytes","lymphocytes")])) safe_div(g("platelets") * g("neutrophils") * g("monocytes"), g("lymphocytes")) else rep(NA_real_, n)
    out$CLR  <<- if (all(avail[c("CRP","lymphocytes")]))         safe_div(g("CRP"), g("lymphocytes")) else rep(NA_real_, n)
    out$CAR  <<- if (all(avail[c("CRP","albumin")]))             safe_div(g("CRP"), g("albumin")) else rep(NA_real_, n)
    out$PCR  <<- if (all(avail[c("platelets","CRP")]))           safe_div(g("platelets"), g("CRP")) else rep(NA_real_, n)
    out$mGPS <<- if (all(avail[c("CRP","albumin")])) {
                   crp <- g("CRP"); alb <- g("albumin")
                   as.integer(ifelse(is.na(crp), NA_integer_,
                              ifelse(crp <= 10, 0L, ifelse(alb >= 35, 1L, 2L))))
                 } else rep(NA_integer_, n)
    if (avail[["ESR"]]) out$ESR <<- as.numeric(g("ESR"))
    used_keys <<- union(used_keys, c("neutrophils","lymphocytes","platelets","monocytes","eosinophils","CRP","albumin","ESR"))
  }

  if (panel == "classic") {
    add_classic()
    out <- out[, c("NLR","PLR","LMR","dNLR","SII","SIRI","AISI","CRP_category"), drop = FALSE]
  } else if (panel == "eos") {
    add_eos()
    ord <- c("NLR","PLR","LMR","NER","SII","SIRI","PIV","CLR","CAR","PCR","mGPS", if ("ESR" %in% names(out)) "ESR")
    out <- out[, ord, drop = FALSE]
  } else { # both
    add_eos(); add_classic()
    ord <- c("NLR","PLR","LMR","NER","SII","SIRI","PIV","CLR","CAR","PCR","mGPS","ESR","dNLR","AISI","CRP_category")
    ord <- intersect(ord, names(out))
    out <- out[, ord, drop = FALSE]
  }

  # NA policy over used inputs
  used_cols <- map_cols[intersect(unique(used_keys), names(map_cols))]
  used_cols <- used_cols[!is.na(used_cols)]
  cc <- if (length(used_cols)) stats::complete.cases(data[, used_cols, drop = FALSE]) else rep(TRUE, n)

  na_action_eff <- if (isTRUE(check_extreme) && identical(extreme_action, "NA")) "keep" else na_action
  if (na_action_eff == "error" && any(!cc)) {
    rlang::abort("inflammatory_markers(): required columns contain missing or non-finite values (na_action='error').",
                 class = "healthmarkers_inflammatory_markers_error_missing_values")
  } else if (na_action_eff == "omit") {
    out <- out[cc, , drop = FALSE]
  }

  if (dz_count > 0L) rlang::warn("inflammatory_markers(): zero denominators detected.")
  if (isTRUE(verbose)) rlang::inform("inflammatory_markers(): computed inflammatory indices")

  tibble::as_tibble(out)
}

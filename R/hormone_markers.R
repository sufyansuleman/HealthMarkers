# R/hormone_markers.R

#' Compute a suite of hormone ratio markers with QA and verbose summaries
#'
#' Given a data.frame or tibble with hormone measurements, `hormone_markers()`
#' returns nine commonly used clinical ratios:
#' 1) FAI = (total_testosterone / SHBG) * 100
#' 2) LH_FSH = LH / FSH
#' 3) E2_P = Estradiol / Progesterone
#' 4) T3_T4 = Free T3 / Free T4
#' 5) ARR = Aldosterone / Renin ratio
#' 6) Ins_Glu = Insulin / Glucagon
#' 7) GH_IGF1 = Growth Hormone / IGF1
#' 8) PRL_T = Prolactin / total_testosterone
#' 9) CAR_slope = (cortisol_30 - cortisol_0) / 30
#'
#' Expected units (no automatic unit conversion):
#' - total_testosterone, SHBG: nmol/L (FAI dimensionless)
#' - LH, FSH: IU/L
#' - estradiol: pmol/L; progesterone: nmol/L
#' - free_T3, free_T4: pmol/L
#' - aldosterone: ng/dL; renin: (PRA ng/mL/h or PRC mU/L – ensure consistency)
#' - insulin: µIU/mL; glucagon: pmol/L
#' - GH, IGF1: µg/L
#' - cortisol_0, cortisol_30: nmol/L
#'
#' @param data A data.frame or tibble containing hormone measurements.
#' @param col_map Named list mapping keys to column names in `data`:
#'   total_testosterone, SHBG, LH, FSH, estradiol, progesterone, free_T3, free_T4,
#'   aldosterone, renin, insulin, glucagon, GH, IGF1, prolactin, cortisol_0, cortisol_30.
#' @param na_action One of c("ignore","warn","error") controlling behavior when used inputs
#'   contain missing or non-finite values. Default "ignore".
#' @param na_warn_prop Proportion in [0,1] above which a high-missingness warning is raised
#'   per variable when na_action = "warn". Default 0.2.
#' @param check_extreme Logical; if TRUE, scan selected inputs for values outside plausible
#'   ranges defined in `extreme_rules`. Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") controlling how extreme
#'   inputs are handled. If "cap", values are truncated to the allowed range. Default "warn".
#' @param extreme_rules Optional named list of length-2 numeric ranges (c(min,max)) for inputs
#'   to scan when check_extreme = TRUE. Defaults are broad clinical plausibility ranges; only
#'   variables present in data are checked.
#' @param verbose Logical; if TRUE, prints progress and a completion summary. Default FALSE.
#'
#' @return A tibble with columns:
#'   FAI, LH_FSH, E2_P, T3_T4, ARR, Ins_Glu, GH_IGF1, PRL_T, CAR_slope
#'
#' @examples
#' df <- tibble::tibble(
#'   total_testosterone = c(12, 18),
#'   SHBG               = c(30, 40),
#'   LH                 = c(8, 9),
#'   FSH                = c(5, 4),
#'   estradiol          = c(200, 300),
#'   progesterone       = c(5, 4),
#'   free_T3            = c(4.5, 4.2),
#'   free_T4            = c(17, 16),
#'   aldosterone        = c(10, 20),
#'   renin              = c(1.5, 2.0),
#'   insulin            = c(10, 14),
#'   glucagon           = c(15, 12),
#'   GH                 = c(1.2, 0.9),
#'   IGF1               = c(150, 180),
#'   prolactin          = c(12, 9),
#'   cortisol_0         = c(350, 300),
#'   cortisol_30        = c(450, 380)
#' )
#' map <- list(
#'   total_testosterone = "total_testosterone", SHBG = "SHBG",
#'   LH = "LH", FSH = "FSH", estradiol = "estradiol", progesterone = "progesterone",
#'   free_T3 = "free_T3", free_T4 = "free_T4", aldosterone = "aldosterone", renin = "renin",
#'   insulin = "insulin", glucagon = "glucagon", GH = "GH", IGF1 = "IGF1",
#'   prolactin = "prolactin", cortisol_0 = "cortisol_0", cortisol_30 = "cortisol_30"
#' )
#' hormone_markers(df, map, verbose = TRUE)
#'
#' @references
#' Vermeulen A, Verdonck L, Kaufman JM (1999). A critical evaluation of simple methods for the estimation of free testosterone in serum. J Clin Endocrinol Metab, 84(10):3666–3672. \doi{10.1210/jcem.84.10.6079}
#' Taylor AE, McCourt B, Martin KA, et al. (1997). Determinants of abnormal gonadotropin secretion in polycystic ovary syndrome. J Clin Endocrinol Metab, 82(7):2248–2256. \doi{10.1210/jcem.82.7.4047}
#' Visser TJ, Friesema ECH, Jansen J, et al. (2010). Thyroid hormone transporters and deiodinases. Thyroid, 20(7):751–758. \doi{10.1089/thy.2010.0075}
#' Hiramatsu K, Yamada T, Yukimura Y, et al. (1981). A screening test to identify primary aldosteronism by measuring plasma renin activity and aldosterone concentration. Endocrinol Jpn, 28(5):679–686. \doi{10.1507/endocrj1954.28.679}
#' Müller WA, Faloona GR, Unger RH (1971). The effect of insulin and glucagon on glucose production by the isolated perfused rat liver. J Clin Invest, 50(12):2715–2723. \doi{10.1172/JCI106756}
#' Juul A, Bang P, Hertel NT, et al. (1994). Serum insulin-like growth factor-I in 1030 healthy subjects: relation to age and sex. Clin Chem, 40(10):1766–1774. \doi{10.1093/clinchem/40.10.1766}
#' Pruessner JC, Wolf OT, Hellhammer DH, et al. (1997). Free cortisol levels after awakening: a reliable biological marker for the assessment of adrenocortical activity. Psychoneuroendocrinology, 22(4):295–309. \doi{10.1016/S0306-4530(96)00039-4}
#' @importFrom tibble tibble
#' @export
hormone_markers <- function(data,
                            col_map,
                            na_action = c("ignore","warn","error"),
                            na_warn_prop = 0.2,
                            check_extreme = FALSE,
                            extreme_action = c("warn","cap","error","ignore"),
                            extreme_rules = NULL,
                            verbose = FALSE) {
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # Required keys
  required <- c(
    "total_testosterone","SHBG","LH","FSH","estradiol","progesterone",
    "free_T3","free_T4","aldosterone","renin","insulin","glucagon",
    "GH","IGF1","prolactin","cortisol_0","cortisol_30"
  )

  # Validate inputs and column map
  if (!is.data.frame(data)) stop("hormone_markers(): `data` must be a data.frame or tibble.", call. = FALSE)
  validate_inputs(
    data = data,
    col_map = col_map,
    fun_name = "hormone_markers",
    required_keys = required,
    allow_extra_keys = TRUE,
    duplicate_action = "ignore",
    missing_action = "error",
    missing_cols_action = "error",
    return_map = FALSE,
    verbose = isTRUE(verbose)
  )

  if (isTRUE(verbose)) {
    message(sprintf("-> hormone_markers: starting (%d rows, %d mapped inputs)", nrow(data), length(required)))
    message("-> hormone_markers: coercing inputs to numeric (if needed)")
  }

  # Coerce used columns to numeric; warn if NAs introduced by coercion
  used_cols <- unlist(col_map[required], use.names = FALSE)
  used_cols <- used_cols[!is.na(used_cols) & nzchar(used_cols)]
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      introduced_na <- sum(is.na(data[[cn]]) & !is.na(old))
      if (introduced_na > 0L) warning(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na), call. = FALSE)
    }
    # standardize non-finite to NA for QA and safe math
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # Missingness / non-finite scan
  if (isTRUE(verbose)) message("-> hormone_markers: scanning missingness and non-finite values")
  qa <- .hm_quality_scan(data, used_cols, na_warn_prop = na_warn_prop)
  if (na_action == "warn") {
    if (length(qa$any_na)) warning(sprintf("Missing values in: %s.", paste(qa$any_na, collapse = ", ")), call. = FALSE)
    if (length(qa$high_na)) warning(sprintf("High missingness (>= %.0f%%): %s.", 100 * na_warn_prop, paste(qa$high_na, collapse = ", ")), call. = FALSE)
  }
  if (na_action == "error" && length(qa$any_na)) {
    stop("hormone_markers(): missing or non-finite values in required inputs with na_action='error'.", call. = FALSE)
  }

  # Extreme-value scan and optional handling
  if (isTRUE(check_extreme)) {
    if (isTRUE(verbose)) message("-> hormone_markers: scanning for extreme input values")
    rules <- .hm_default_extreme_rules()
    if (!is.null(extreme_rules) && is.list(extreme_rules)) {
      for (nm in names(extreme_rules)) rules[[nm]] <- extreme_rules[[nm]]
    }
    ex <- .hm_extreme_scan(data, col_map, rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        stop(sprintf("hormone_markers(): %d extreme input values detected.", ex$count), call. = FALSE)
      } else if (extreme_action == "cap") {
        data <- .hm_cap_inputs(data, ex$flags, col_map, rules)
        warning(sprintf("hormone_markers(): capped %d extreme input values into allowed ranges.", ex$count), call. = FALSE)
      } else if (extreme_action == "warn") {
        warning(sprintf("hormone_markers(): detected %d extreme input values (not altered).", ex$count), call. = FALSE)
      }
    }
  }

  if (isTRUE(verbose)) message("-> hormone_markers: computing ratios")

  # Helper to fetch by key
  getv <- function(key) data[[col_map[[key]]]]

  # Safe division: returns NA when denominator <= 0 or non-finite
  sdv <- function(a, b) {
    out <- a / b
    bad <- !is.finite(a) | !is.finite(b) | (is.finite(b) & b == 0)
    out[bad] <- NA_real_
    out
  }

  # Compute ratios (preserve existing formulas; safe math)
  FAI       <- sdv(getv("total_testosterone"), getv("SHBG")) * 100
  LH_FSH    <- sdv(getv("LH"), getv("FSH"))
  E2_P      <- sdv(getv("estradiol"), getv("progesterone"))
  T3_T4     <- sdv(getv("free_T3"), getv("free_T4"))
  ARR       <- sdv(getv("aldosterone"), getv("renin"))
  Ins_Glu   <- sdv(getv("insulin"), getv("glucagon"))
  GH_IGF1   <- sdv(getv("GH"), getv("IGF1"))
  PRL_T     <- sdv(getv("prolactin"), getv("total_testosterone"))
  CAR_slope <- (getv("cortisol_30") - getv("cortisol_0")) / 30
  CAR_slope[!is.finite(CAR_slope)] <- NA_real_

  out <- tibble::tibble(
    FAI = FAI,
    LH_FSH = LH_FSH,
    E2_P = E2_P,
    T3_T4 = T3_T4,
    ARR = ARR,
    Ins_Glu = Ins_Glu,
    GH_IGF1 = GH_IGF1,
    PRL_T = PRL_T,
    CAR_slope = CAR_slope
  )

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x)), integer(1))
    message(sprintf(
      "Completed hormone_markers: %d rows; NA counts -> %s",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", ")
    ))
  }

  return(out)
}

# ---- internal helpers (not exported) ----------------------------------------

.hm_quality_scan <- function(df, cols, na_warn_prop = 0.2) {
  any_na <- character(0); high_na <- character(0)
  for (cn in cols) {
    x <- df[[cn]]
    if (anyNA(x)) any_na <- c(any_na, cn)
    n <- length(x)
    if (n > 0L) {
      pna <- sum(is.na(x)) / n
      if (pna >= na_warn_prop && pna > 0) {
        high_na <- c(high_na, sprintf("%s(%.1f%% NA)", cn, 100 * pna))
      }
    }
  }
  list(any_na = unique(any_na), high_na = unique(high_na))
}

.hm_default_extreme_rules <- function() {
  # Broad plausibility ranges; adjust as needed in your workflows
  list(
    total_testosterone = c(0, 100),   # nmol/L
    SHBG               = c(0, 300),   # nmol/L
    LH                 = c(0, 150),   # IU/L
    FSH                = c(0, 200),   # IU/L
    estradiol          = c(0, 10000), # pmol/L
    progesterone       = c(0, 300),   # nmol/L
    free_T3            = c(0, 30),    # pmol/L
    free_T4            = c(0, 60),    # pmol/L
    aldosterone        = c(0, 200),   # ng/dL
    renin              = c(0, 50),    # PRA or PRC (ensure consistent units)
    insulin            = c(0, 500),   # µIU/mL
    glucagon           = c(0, 500),   # pmol/L
    GH                 = c(0, 100),   # µg/L
    IGF1               = c(0, 1000),  # µg/L
    prolactin          = c(0, 200),   # ng/mL
    cortisol_0         = c(0, 2000),  # nmol/L
    cortisol_30        = c(0, 2000)   # nmol/L
  )
}

.hm_extreme_scan <- function(df, col_map, rules) {
  count <- 0L
  flags <- list()
  for (nm in names(rules)) {
    if (is.null(col_map[[nm]])) next
    cn <- col_map[[nm]]
    if (!cn %in% names(df)) next
    rng <- rules[[nm]]
    x <- df[[cn]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[nm]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.hm_cap_inputs <- function(df, flags, col_map, rules) {
  for (nm in names(flags)) {
    cn <- col_map[[nm]]
    if (is.null(cn) || !cn %in% names(df)) next
    rng <- rules[[nm]]
    x <- df[[cn]]
    bad <- flags[[nm]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

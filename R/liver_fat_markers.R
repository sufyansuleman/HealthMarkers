#' Liver fat surrogates: HSI and NAFLD Liver Fat Score
#'
#' Computes:
#' - HSI = 8 * (ALT/AST) + BMI + 2 (if female) + 2 (if diabetes)
#' - NAFLD-LFS = -2.89 + 1.18*MetS + 0.45*Type2DM + 0.15*Insulin_u + 0.04*AST - 0.94*(AST/ALT)
#'
#' Assumptions/units:
#' - ALT, AST in U/L; BMI in kg/m^2; I0 in pmol/L (converted to µU/mL via /6).
#' - MetS is taken directly if provided; otherwise derived via NCEP-ATP III when sufficient inputs exist.
#' - Type2DM is taken from `diabetes` (logical or 0/1).
#'
#' @param data Data frame with needed columns (see col_map).
#' @param col_map Named list mapping:
#'   - Required for HSI: ALT, AST, BMI
#'   - Optional direct inputs: sex, diabetes, MetS, insulin
#'   - Optional to derive MetS or insulin: I0, waist, TG, HDL_c, sbp, bp_sys, bp_treated, glucose, G0
#' @param na_action One of c("keep","omit","error","ignore","warn").
#' @param na_warn_prop Proportion in [0,1] for high-missingness warnings when na_action = "warn". Default 0.2.
#' @param check_extreme Logical; if TRUE, scan selected inputs for plausible ranges.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") controlling how extremes are handled.
#' @param extreme_rules Optional named list of c(min,max) to override defaults.
#' @param verbose Logical; if TRUE, prints progress.
#' @return A tibble with columns HSI and NAFLD_LFS.
#' @references
#' - Hepatic Steatosis Index (HSI) original description.
#' - NAFLD Liver Fat Score (Kotronen et al., 2009).
#' @examples
#' df <- data.frame(ALT=20, AST=25, BMI=27, sex="female", diabetes=FALSE, I0=60)
#' liver_fat_markers(df, col_map = list(ALT="ALT", AST="AST", BMI="BMI", sex="sex", diabetes="diabetes", I0="I0"))
#' @export
liver_fat_markers <- function(
  data,
  col_map = list(ALT="ALT", AST="AST", BMI="BMI", sex="sex", diabetes="diabetes",
                 MetS="MetS", insulin="insulin", I0="I0", waist="waist", TG="TG", HDL_c="HDL_c",
                 sbp="sbp", bp_sys="bp_sys", bp_treated="bp_treated",
                 glucose="glucose", G0="G0"),
  na_action = c("keep","omit","error","ignore","warn"),
  na_warn_prop = 0.2,
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  # Validate required mapping and data
  if (!is.data.frame(data)) rlang::abort("liver_fat_markers(): `data` must be a data.frame or tibble.")
  req <- c("ALT","AST","BMI")
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) rlang::abort(paste("liver_fat_markers(): missing required columns:", paste(missing_keys, collapse=", ")))
  mapped_req <- unname(unlist(col_map[req]))
  if (any(!nzchar(mapped_req))) {
    bad <- req[!nzchar(mapped_req)]
    rlang::abort(paste("liver_fat_markers(): missing required columns:", paste(bad, collapse=", ")))
  }
  miss <- setdiff(mapped_req, names(data))
  if (length(miss)) rlang::abort(paste("liver_fat_markers(): missing required columns:", paste(miss, collapse=", ")))

  if (isTRUE(verbose)) rlang::inform("-> liver_fat_markers: coercing inputs to numeric (if needed)")

  # Columns potentially used directly in formulas
  direct_keys <- c("ALT","AST","BMI","sex","diabetes","MetS","insulin","I0")
  # Additional optional used for MetS derivation
  deriva_keys <- c("waist","TG","HDL_c","sbp","bp_sys","bp_treated","glucose","G0")
  # Build list of mapped columns present
  mapped_all <- unlist(col_map[intersect(names(col_map), c(direct_keys, deriva_keys))], use.names = TRUE)
  used_cols <- unique(mapped_all[!is.na(mapped_all) & nzchar(mapped_all) & mapped_all %in% names(data)])

  # Coerce only numeric-like columns; warn if NAs introduced
  numeric_like <- intersect(used_cols, unname(unlist(col_map[c("ALT","AST","BMI","insulin","I0","waist","TG","HDL_c","sbp","bp_sys","glucose","G0")])))
  for (cn in numeric_like) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) rlang::warn(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na))
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # NA policy: build used frame for handling
  used_for_policy <- unique(unname(unlist(col_map[intersect(names(col_map), direct_keys)])))
  used_for_policy <- used_for_policy[!is.na(used_for_policy) & nzchar(used_for_policy)]
  if (length(used_for_policy) == 0L) used_for_policy <- mapped_req
  if (na_action_raw == "warn" && length(used_for_policy)) {
    dfp <- data[, used_for_policy, drop = FALSE]
    any_na_cols <- names(which(colSums(is.na(dfp)) > 0))
    high_na_cols <- names(which(colMeans(is.na(dfp)) >= na_warn_prop))
    if (length(any_na_cols)) rlang::warn(sprintf("Missing values in: %s.", paste(any_na_cols, collapse = ", ")))
    if (length(high_na_cols)) rlang::warn(sprintf("High missingness (>= %.0f%%): %s.", 100*na_warn_prop, paste(high_na_cols, collapse = ", ")))
  }
  cc_req <- stats::complete.cases(data[, mapped_req, drop = FALSE])
  if (na_action_eff == "error" && !all(cc_req)) {
    rlang::abort("liver_fat_markers(): missing/non-finite values with na_action='error'.")
  }
  keep <- if (na_action_eff == "omit") cc_req else rep(TRUE, nrow(data))

  d <- data[keep, , drop = FALSE]

  # Optional extreme-value scan/handling (broad defaults)
  if (isTRUE(check_extreme)) {
    rules_def <- list(
      ALT = c(0, 1000), AST = c(0, 1000), BMI = c(10, 80),
      insulin = c(0, 500), I0 = c(0, 3000),
      TG = c(0, 50), HDL_c = c(0, 10), waist = c(30, 250),
      sbp = c(60, 300), bp_sys = c(60, 300), glucose = c(0, 50), G0 = c(0, 50)
    )
    if (is.list(extreme_rules)) {
      for (nm in intersect(names(extreme_rules), names(rules_def))) rules_def[[nm]] <- extreme_rules[[nm]]
    }
    total_flag <- 0L
    flags <- list()
    # name->column map (prefer sbp, else bp_sys)
    name_to_col <- list(
      ALT = col_map$ALT, AST = col_map$AST, BMI = col_map$BMI,
      insulin = col_map$insulin, I0 = col_map$I0, TG = col_map$TG, HDL_c = col_map$HDL_c,
      waist = col_map$waist, sbp = if (!is.null(col_map$sbp)) col_map$sbp else col_map$bp_sys,
      bp_sys = col_map$bp_sys, glucose = col_map$glucose, G0 = col_map$G0
    )
    for (nm in names(rules_def)) {
      cn <- name_to_col[[nm]]
      if (is.null(cn) || !(cn %in% names(d))) next
      x <- d[[cn]]; rng <- rules_def[[nm]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      flags[[nm]] <- bad
      total_flag <- total_flag + sum(bad)
    }
    if (total_flag > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("liver_fat_markers(): %d extreme input values detected.", total_flag))
      } else if (extreme_action == "cap") {
        for (nm in names(flags)) {
          cn <- name_to_col[[nm]]; if (is.null(cn) || !(cn %in% names(d))) next
          lo <- rules_def[[nm]][1]; hi <- rules_def[[nm]][2]
          x <- d[[cn]]; bad <- flags[[nm]]
          x[bad & is.finite(x) & x < lo] <- lo
          x[bad & is.finite(x) & x > hi] <- hi
          d[[cn]] <- x
        }
        rlang::warn(sprintf("liver_fat_markers(): capped %d extreme input values into allowed ranges.", total_flag))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("liver_fat_markers(): detected %d extreme input values (not altered).", total_flag))
      } else if (extreme_action == "NA") {
        for (nm in names(flags)) {
          cn <- name_to_col[[nm]]; if (is.null(cn) || !(cn %in% names(d))) next
          bad <- flags[[nm]]; x <- d[[cn]]; x[bad] <- NA_real_; d[[cn]] <- x
        }
      }
      # ignore: do nothing
    }
  }

  # Extract helpers from subset
  getv <- function(k) {
    nm <- col_map[[k]]
    if (!is.null(nm) && nzchar(nm) && nm %in% names(d)) d[[nm]] else NULL
  }
  ALT <- getv("ALT"); AST <- getv("AST"); BMI <- getv("BMI")
  sex <- getv("sex"); diabetes <- getv("diabetes")
  MetS_direct <- getv("MetS"); insulin <- getv("insulin")
  I0 <- getv("I0"); glucose <- getv("glucose"); G0 <- getv("G0")
  TG <- getv("TG"); HDL <- getv("HDL_c"); waist <- getv("waist")
  sbp <- if (!is.null(getv("sbp"))) getv("sbp") else getv("bp_sys")
  bp_treated <- getv("bp_treated")

  # Helpers
  sdiv <- function(a,b){ z <- a/b; z[!is.finite(z)] <- NA_real_; z }
  as01 <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.logical(x)) return(as.integer(x))
    if (is.numeric(x)) return(as.integer(x > 0))
    xv <- suppressWarnings(as.numeric(x))
    as.integer(ifelse(is.na(xv), NA, xv > 0))
  }
  female_flag <- {
    if (is.null(sex)) rep(0L, length(BMI))
    else {
      if (is.numeric(sex)) as.integer(sex == 2)
      else if (is.logical(sex)) as.integer(sex)
      else {
        xchr <- tolower(as.character(sex))
        as.integer(xchr %in% c("f","female","woman","women"))
      }
    }
  }
  dm2_flag <- as01(diabetes)
  if (is.null(dm2_flag)) dm2_flag <- rep(0L, length(BMI))

  # HSI
  HSI <- 8 * sdiv(ALT, AST) + BMI + ifelse(female_flag == 1L, 2, 0) + ifelse(dm2_flag == 1L, 2, 0)

  # MetS: use direct mapping if available; otherwise derive; else NA
  derive_mets <- function() {
    n <- length(BMI)
    out <- rep(NA_integer_, n)
    have <- list(waist=!is.null(waist), TG=!is.null(TG), HDL=!is.null(HDL), sbp=!is.null(sbp), glucose=!is.null(glucose) || !is.null(G0))
    if (!all(unlist(have))) return(out)
    glu <- if (!is.null(glucose)) glucose else G0
    low_hdl <- ifelse(female_flag == 1L, HDL < 1.29, HDL < 1.03)
    crits <- cbind(
      waist = waist > ifelse(female_flag == 1L, 88, 102),
      tg    = TG >= 1.7,
      hdl   = low_hdl,
      bp    = sbp >= 130 | (if (!is.null(bp_treated)) as01(bp_treated)==1L else FALSE),
      glu   = glu >= 5.6
    )
    as.integer(rowSums(crits, na.rm = TRUE) >= 3)
  }
  MetS <- if (!is.null(MetS_direct)) as01(MetS_direct) else derive_mets()
  if (is.null(MetS)) MetS <- rep(NA_integer_, length(BMI))

  # fasting insulin in µU/mL: prefer direct insulin; else I0/6 if available
  Ins_u <- if (!is.null(insulin)) {
    insulin
  } else if (!is.null(I0)) {
    I0 / 6
  } else {
    rep(NA_real_, length(BMI))
  }

  NAFLD_LFS <- -2.89 + 1.18 * MetS + 0.45 * dm2_flag + 0.15 * Ins_u + 0.04 * AST - 0.94 * sdiv(AST, ALT)

  out <- tibble::tibble(HSI = as.numeric(HSI), NAFLD_LFS = as.numeric(NAFLD_LFS))
  if (na_action_eff == "omit") return(out)

  # if keep/error, preserve original row count by padding
  res <- tibble::tibble(HSI = rep(NA_real_, nrow(data)), NAFLD_LFS = rep(NA_real_, nrow(data)))
  res$HSI[keep] <- out$HSI
  res$NAFLD_LFS[keep] <- out$NAFLD_LFS
  res
}
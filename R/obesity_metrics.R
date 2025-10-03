# R/obesity_metrics.R

#' Compute anthropometric obesity & adiposity indices
#'
#' Calculates a comprehensive set of body shape and adiposity indices:
#' * BMI and WHO BMI categories
#' * Waist-to-hip ratio (WHR) and optional WHR adjusted for BMI (WHRadjBMI)
#' * Waist-to-height ratio (WHtR)
#' * Abdominal Volume Index (AVI)
#' * Body Adiposity Index (BAI)
#' * A Body Shape Index (ABSI)
#' * Body Roundness Index (BRI)
#' * Conicity Index (CI)
#' * (Optional) Relative Fat Mass (RFM)
#'
#' Units assumed (no automatic conversion beyond the specified weight/height options):
#' - weight: kg (or lb if weight_unit = "lb")
#' - height: m (or cm if height_unit = "cm")
#' - waist, hip: cm
#' - sex: 0 = male, 1 = female (only required if include_RFM = TRUE)
#'
#' @param data A data.frame or tibble containing the input columns.
#' @param weight Unquoted column name for weight.
#' @param height Unquoted column name for height.
#' @param waist Unquoted column name for waist circumference.
#' @param hip Unquoted column name for hip circumference.
#' @param sex   (Optional) Unquoted column name for sex, coded 0=male, 1=female; required if include_RFM=TRUE.
#' @param weight_unit   One of c("kg","lb"); if "lb", converts weight to kg by *0.45359237.
#' @param height_unit   One of c("cm","m"); if "cm", converts height to metres by /100.
#' @param adjust_WHR    Logical; if TRUE, adds a column WHRadjBMI as residuals from WHR ~ BMI.
#' @param include_RFM   Logical; if TRUE, computes Relative Fat Mass (requires sex column).
#' @param na_action One of c("keep","omit","error") for handling NA in required inputs. Default "keep".
#' @param na_warn_prop Proportion [0,1] to trigger high-missingness warnings for required inputs. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for out-of-range values (heuristic). Default FALSE.
#' @param extreme_action One of c("warn","cap","error","ignore") when extremes are detected. Default "warn".
#' @param extreme_rules Optional named list of c(min,max) for c("weight_kg","height_m","waist","hip"). NULL uses defaults.
#' @param verbose Logical; if TRUE, prints progress messages and a completion summary. Default FALSE.
#'
#' @return A tibble with original columns plus new indices:
#' * weight_kg, height_m, BMI, BMI_cat,
#' * WHR, WHRadjBMI (optional), waist_to_height_ratio,
#' * AVI, BAI, ABSI, BRI, CI,
#' * waist_to_BMI_ratio, weight_to_height_ratio,
#' * RFM (optional).
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang enquo quo_name quo_is_null abort warn inform
#' @importFrom stats lm resid
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   wt     = c(70, 80),  # kg
#'   ht     = c(175, 165),# cm
#'   waist  = c(80, 90),  # cm
#'   hip    = c(100, 95), # cm
#'   sex    = c(0, 1)
#' )
#' obesity_indices(
#'   df,
#'   weight       = wt,
#'   height       = ht,
#'   waist        = waist,
#'   hip          = hip,
#'   sex          = sex,
#'   weight_unit  = "kg",
#'   height_unit  = "cm",
#'   adjust_WHR   = TRUE,
#'   include_RFM  = TRUE,
#'   verbose      = TRUE
#' )
#' @references
#'  Original derivations
#'  Quetelet A. A Treatise on Man and the Development of his Faculties. Edinburgh: William & Robert Chambers; 1842. (BMI origin)
#'  WHO Expert Committee. Physical Status: The Use and Interpretation of Anthropometry. WHO Technical Report Series 854. Geneva: World Health Organization; 1995. (BMI categories, WHR, WHtR)
#'  Guerrero-Romero F, Rodríguez-Morán M. Abdominal volume index. An anthropometric index of central obesity. Int J Obes Relat Metab Disord. 1999;23(6):615–619. \doi{10.1038/sj.ijo.0800887} (AVI derivation)
#'  Bergman RN, Stefanovski D, Buchanan TA, et al. A better index of body adiposity. Obesity (Silver Spring). 2011;19(5):1083–1089. \doi{10.1038/oby.2011.38} (BAI derivation)
#'  Krakauer NY, Krakauer JC. A new body shape index predicts mortality hazard independently of BMI. PLoS One. 2012;7(7):e39504. \doi{10.1371/journal.pone.0039504} (ABSI derivation)
#'  Thomas DM, Bredlau C, Bosy-Westphal A, et al. Relationships between body roundness with body fat and visceral adipose tissue emerging from a new geometrical model. Obesity (Silver Spring). 2013;21(11):2264–2271. \doi{10.1002/oby.20408} (BRI derivation)
#'  Valdez R. A simple model-based index of abdominal adiposity. J Clin Epidemiol. 1991;44(9):955–956. \doi{10.1016/0895-4356(91)90019-I} (Conicity Index derivation)
#'  Woolcott OO, Bergman RN. Relative fat mass (RFM) as a new estimator of whole-body fat percentage: a cross-sectional study in American adults. Sci Rep. 2018;8(1):10980. \doi{10.1038/s41598-018-29362-1} (RFM derivation)
#'
#'  Validation and consensus
#'  Calle EE, Thun MJ, Petrelli JM, Rodriguez C, Heath CW Jr. Body-mass index and mortality in a prospective cohort of U.S. adults. N Engl J Med. 1999;341(15):1097–1105. \doi{10.1056/NEJM199910073411501} (BMI validation in mortality risk)
#'  Freedman DS, Thornton JC, Pi-Sunyer FX, et al. The body adiposity index is not a more accurate measure of adiposity than BMI, waist circumference, or hip circumference. Obesity (Silver Spring). 2012;20(12):2438–2444. \doi{10.1038/oby.2012.81} (BAI validation vs. BMI)
#'  He S, Chen X. Could the new body shape index predict the new onset of diabetes mellitus in the Chinese population? PLoS One. 2013;8(1):e50573. \doi{10.1371/journal.pone.0050573} (ABSI validation in diabetes)
#'  Maessen MF, Eijsvogels TM, Verheggen RJ, Hopman MT, Verbeek AL, de Vegt F. Entering a new era of body indices: the feasibility of ABSI and BRI to identify cardiovascular health status. PLoS One. 2014;9(9):e107212. \doi{10.1371/journal.pone.0107212} (BRI & ABSI validation in CVD risk)
obesity_indices <- function(data,
                            weight,
                            height,
                            waist,
                            hip,
                            sex = NULL,
                            weight_unit = c("kg", "lb"),
                            height_unit = c("cm", "m"),
                            adjust_WHR = FALSE,
                            include_RFM = FALSE,
                            na_action = c("keep","omit","error"),
                            na_warn_prop = 0.2,
                            check_extreme = FALSE,
                            extreme_action = c("warn","cap","error","ignore"),
                            extreme_rules = NULL,
                            verbose = FALSE) {
  # Match args
  weight_unit <- match.arg(weight_unit)
  height_unit <- match.arg(height_unit)
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  t0 <- Sys.time()
  if (isTRUE(verbose)) rlang::inform("-> obesity_indices: validating inputs")

  # Capture and quote arguments
  wt_q <- rlang::enquo(weight);  wt_name  <- rlang::quo_name(wt_q)
  ht_q <- rlang::enquo(height);  ht_name  <- rlang::quo_name(ht_q)
  wst_q<- rlang::enquo(waist);   wst_name <- rlang::quo_name(wst_q)
  hp_q <- rlang::enquo(hip);     hp_name  <- rlang::quo_name(hp_q)
  sx_q <- rlang::enquo(sex);     sx_name  <- if (!rlang::quo_is_null(sx_q)) rlang::quo_name(sx_q) else NULL

  # Validate data frame and required columns
  .oi_validate_data_and_cols(data, c(wt_name, ht_name, wst_name, hp_name), include_RFM, sx_name)
  .oi_warn_high_missing(data, c(wt_name, ht_name, wst_name, hp_name, if (include_RFM) sx_name else NULL), na_warn_prop)

  # NA policy on required inputs
  used_cols <- c(wt_name, ht_name, wst_name, hp_name, if (include_RFM) sx_name else NULL)
  if (na_action == "error") {
    has_na <- Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
    if (any(has_na)) {
      rlang::abort("obesity_indices(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_obesity_error_missing_values")
    }
  } else if (na_action == "omit") {
    if (length(used_cols)) {
      keep <- !Reduce(`|`, lapply(used_cols, function(cn) is.na(data[[cn]])))
      if (isTRUE(verbose)) rlang::inform(sprintf("-> obesity_indices: omitting %d rows with NA in required inputs", sum(!keep)))
      data <- data[keep, , drop = FALSE]
    }
  }

  # Unit-normalized base fields
  out <- data %>%
    dplyr::mutate(
      weight_kg = dplyr::case_when(
        weight_unit == "kg" ~ !!wt_q,
        weight_unit == "lb" ~ !!wt_q * 0.45359237
      ),
      height_m = dplyr::case_when(
        height_unit == "m" ~ !!ht_q,
        height_unit == "cm" ~ !!ht_q / 100
      )
    )

  # Optional extreme scan/cap on base inputs
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) .oi_default_extreme_rules() else extreme_rules
    ex <- .oi_extreme_scan(out, c("weight_kg","height_m", wst_name, hp_name), rules)
    if (ex$count > 0L) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("obesity_indices(): detected %d extreme input values.", ex$count),
                     class = "healthmarkers_obesity_error_extremes")
      } else if (extreme_action == "cap") {
        out <- .oi_cap_inputs(out, ex$flags, rules)
        capped_n <- ex$count
        rlang::warn(sprintf("obesity_indices(): capped %d extreme input values into allowed ranges.", ex$count))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("obesity_indices(): detected %d extreme input values (not altered).", ex$count))
      }
      # ignore: no-op
    }
  }

  # Compute BMI and category using normalized units
  out <- out %>%
    dplyr::mutate(
      BMI = weight_kg / (height_m^2),
      BMI_cat = dplyr::case_when(
        BMI < 18.5 ~ "Underweight",
        BMI < 25 ~ "Normal weight",
        BMI < 30 ~ "Overweight",
        BMI < 35 ~ "Obesity Class I",
        BMI < 40 ~ "Obesity Class II",
        BMI >= 40 ~ "Obesity Class III",
        TRUE ~ NA_character_
      )
    )

  # Safe division helper with denominator-zero tracking
  denom_zero <- new.env(parent = emptyenv()); denom_zero$counts <- list()
  safe_div <- function(num, den, label) {
    res <- rep(NA_real_, length(num))
    ok <- !is.na(num) & !is.na(den)
    zero_den <- ok & (den == 0)
    denom_zero$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    valid <- ok & !zero_den
    res[valid] <- num[valid] / den[valid]
    res[!is.finite(res)] <- NA_real_
    res
  }
  # Safe power roots
  safe_pow <- function(x, p) {
    z <- rep(NA_real_, length(x))
    ok <- !is.na(x) & x > 0
    z[ok] <- x[ok]^p
    z
  }

  # Pull raw columns for waist/hip/sex
  wst <- out[[wst_name]]
  hip <- out[[hp_name]]
  sex_vec <- if (!is.null(sx_name)) out[[sx_name]] else NULL

  # Derived indices
  WHR <- safe_div(wst, hip, "WHR")
  WHtR <- safe_div(wst, out$height_m, "waist_to_height_ratio")
  waist_BMI <- safe_div(wst, out$BMI, "waist_to_BMI_ratio")
  wt_ht <- safe_div(out$weight_kg, out$height_m, "weight_to_height_ratio")

  AVI <- (2 * (wst^2) + 0.7 * (wst - hip)^2) / 1000
  BAI <- ifelse(out$height_m > 0, hip / (out$height_m^1.5) - 18, NA_real_)

  denom_absi <- (safe_pow(out$BMI, 2/3)) * (safe_pow(out$height_m, 1/2))
  ABSI <- safe_div(wst, denom_absi, "ABSI")

  # BRI: guard height_m > 0
  ratio <- ifelse(out$height_m > 0, wst / (2 * pi * out$height_m), NA_real_)
  BRI <- 364.2 - 365.5 * sqrt(pmax(0, 1 - (ratio^2)))

  # CI: guard divisor > 0
  denom_ci <- 0.109 * sqrt(safe_div(out$weight_kg, out$height_m, "CI_internal"))
  CI <- safe_div(wst, denom_ci, "CI")

  # Attach derived columns
  out$WHR <- WHR
  out$waist_to_height_ratio <- WHtR
  out$waist_to_BMI_ratio <- waist_BMI
  out$weight_to_height_ratio <- wt_ht
  out$AVI <- as.numeric(AVI)
  out$BAI <- as.numeric(BAI)
  out$ABSI <- as.numeric(ABSI)
  out$BRI <- as.numeric(BRI)
  out$CI  <- as.numeric(CI)

  # WHR adjusted for BMI
  if (isTRUE(adjust_WHR)) {
    # Fit only on rows with finite WHR and BMI
    fit_df <- out[is.finite(out$WHR) & is.finite(out$BMI), c("WHR","BMI")]
    if (nrow(fit_df) >= 2L && stats::var(fit_df$BMI, na.rm = TRUE) > 0) {
      res <- stats::resid(stats::lm(WHR ~ BMI, data = fit_df))
      # Map residuals back to full rows (NA where not used in fit)
      WHRadj <- rep(NA_real_, nrow(out))
      WHRadj[as.numeric(rownames(fit_df))] <- as.numeric(res)
      out$WHRadjBMI <- WHRadj
    } else {
      rlang::warn("obesity_indices(): insufficient data to compute WHRadjBMI; coercing to NA.")
      out$WHRadjBMI <- NA_real_
    }
  }

  # Relative Fat Mass
  if (isTRUE(include_RFM)) {
    # Validate sex values
    bad_sex <- sum(is.finite(sex_vec) & !(sex_vec %in% c(0, 1)), na.rm = TRUE)
    if (bad_sex > 0) {
      rlang::warn(sprintf("obesity_indices(): 'sex' contains %d values not in {0,1}; RFM set to NA for those rows.", bad_sex))
    }
    denom_rfm <- wst
    RFM <- 64 - 20 * safe_div(out$height_m, denom_rfm, "RFM") + 12 * ifelse(sex_vec %in% c(0,1), sex_vec, NA_real_)
    out$RFM <- as.numeric(RFM)
  }

  # Consolidated denominator-zero warning
  dz <- denom_zero$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0L) {
    nz <- unlist(dz)
    nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("obesity_indices(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    # Summarize NA/Inf across new outputs
    outputs <- c("weight_kg","height_m","BMI","BMI_cat","WHR","waist_to_height_ratio",
                 "AVI","BAI","ABSI","BRI","CI","waist_to_BMI_ratio","weight_to_height_ratio",
                 if (adjust_WHR) "WHRadjBMI" else NULL,
                 if (include_RFM) "RFM" else NULL)
    na_counts <- vapply(outputs, function(nm) {
      x <- out[[nm]]
      if (is.character(x)) sum(is.na(x)) else sum(is.na(x) | !is.finite(x))
    }, integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed obesity_indices: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out), paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}

# ---- internal helpers ---------------------------------------------------------

.oi_validate_data_and_cols <- function(data, required, include_RFM, sx_name) {
  if (!is.data.frame(data)) {
    rlang::abort("obesity_indices(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_obesity_error_data_type")
  }
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("obesity_indices(): missing required columns: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_obesity_error_missing_columns"
    )
  }
  # Validate numeric types
  for (cn in required) {
    if (!is.numeric(data[[cn]])) {
      rlang::abort(sprintf("obesity_indices(): column '%s' must be numeric.", cn),
                   class = "healthmarkers_obesity_error_type")
    }
  }
  if (isTRUE(include_RFM)) {
    if (is.null(sx_name)) {
      rlang::abort("obesity_indices(): 'sex' must be provided to compute RFM",
                   class = "healthmarkers_obesity_error_missing_sex")
    }
    if (!is.numeric(data[[sx_name]])) {
      rlang::abort("obesity_indices(): 'sex' must be numeric coded 0 (male) or 1 (female).",
                   class = "healthmarkers_obesity_error_sex_type")
    }
  }
  invisible(TRUE)
}

.oi_warn_high_missing <- function(df, cols, na_warn_prop = 0.2) {
  cols <- cols[!is.null(cols)]
  for (cn in cols) {
    x <- df[[cn]]; n <- length(x); if (n == 0L) next
    pna <- sum(is.na(x)) / n
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("obesity_indices(): column '%s' has high missingness (%.1f%%).", cn, 100 * pna))
    }
  }
  invisible(TRUE)
}

.oi_default_extreme_rules <- function() {
  list(
    weight_kg = c(20, 400),
    height_m  = c(1.2, 2.5),
    waist     = c(30, 200),  # cm
    hip       = c(30, 200)   # cm
  )
}

.oi_extreme_scan <- function(df, cols, rules) {
  count <- 0L
  flags <- list()
  for (cn in cols) {
    if (!cn %in% names(df)) next
    key <- if (cn %in% names(rules)) cn else cn
    rng <- rules[[key]]
    if (is.null(rng)) next
    x <- df[[cn]]
    bad <- is.finite(x) & (x < rng[1] | x > rng[2])
    flags[[cn]] <- bad
    count <- count + sum(bad, na.rm = TRUE)
  }
  list(count = count, flags = flags)
}

.oi_cap_inputs <- function(df, flags, rules) {
  for (cn in names(flags)) {
    rng <- rules[[cn]]; if (is.null(rng)) next
    x <- df[[cn]]; bad <- flags[[cn]]
    x[bad & is.finite(x) & x < rng[1]] <- rng[1]
    x[bad & is.finite(x) & x > rng[2]] <- rng[2]
    df[[cn]] <- x
  }
  df
}

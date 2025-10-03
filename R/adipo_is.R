# R/adipo_is.R

#' Calculate adipose-based insulin sensitivity indices (HM-CRANIZED)
#'
#' Computes 10 adipose / lipid–glucose derived insulin sensitivity (or inverted
#' resistance) indices from fasting measurements. Resistance-style indices are
#' multiplied by -1 so that higher values uniformly indicate better insulin
#' sensitivity (suffix _inv where applicable).
#'
#' Required col_map keys (units expected in input):
#'   - G0      : fasting glucose (mmol/L)
#'   - I0      : fasting insulin (pmol/L)
#'   - TG      : triglycerides (mmol/L)
#'   - HDL_c   : HDL cholesterol (mmol/L)
#'   - FFA     : free fatty acids (mmol/L)
#'   - waist   : waist circumference (cm)
#'   - bmi     : body mass index (kg/m^2)
#'
#' Internal unit conversions (do not pre-convert):
#'   - Glucose mmol/L -> mg/dL ( * 18 )
#'   - Insulin pmol/L -> µU/mL ( / 6 )
#'   - TG mmol/L -> mg/dL ( * 88.57 )
#'   - HDL mmol/L -> mg/dL ( * 38.67 )
#'
#' Indices returned:
#'   Revised_QUICKI, VAI_Men_inv, VAI_Women_inv, TG_HDL_C_inv, TyG_inv,
#'   LAP_Men_inv, LAP_Women_inv, McAuley_index, Adipo_inv, Belfiore_inv_FFA
#'
#' @param data data.frame / tibble with required columns
#' @param col_map Named list mapping required keys (see above) to column names
#' @param normalize One of c("none","z","inverse","range","robust"). Applied column-wise post-computation.
#' @param verbose Logical; print step messages
#' @param na_action One of c("keep","omit","error") for required-input NAs. Default "keep".
#' @param na_warn_prop Proportion (0–1) at/above which high-missingness warning is emitted. Default 0.2.
#' @param check_extreme Logical; if TRUE, scan inputs for extremes using `extreme_rules`.
#' @param extreme_action One of c("warn","cap","error","ignore"). Default "warn".
#' @param extreme_rules Optional named list of c(min,max) per required key (original units before conversion).
#'   If NULL, broad defaults are used.
#' @param diagnostics Logical; if TRUE (default) emit domain diagnostic warnings (non-positive values that
#'   will invalidate logs, etc.).
#' @param normalize_quiet Logical; if TRUE suppress normalization warnings (passed
#'   through to normalize_vec when supported, else warnings suppressed locally). Default FALSE.
#'
#' @return tibble with the 10 indices (after optional normalization)
#'
#' @references
#' Katz A, Nambi S, Mather K, et al. (2000). Quantitative insulin sensitivity check index (QUICKI). J Clin Endocrinol Metab, 85(7):2402–2410. \doi{10.1210/jcem.85.7.6661}
#' Amato MC, Giordano C, Galia M, et al. (2010). Visceral Adiposity Index: a reliable indicator of visceral fat function associated with cardiometabolic risk. Diabetes Care, 33(4):920–922. \doi{10.2337/dc09-1825}
#' Kahn HS (2005). The lipid accumulation product performs better than body mass index for identifying cardiovascular risk. Diabetes Care, 28(7):1757–1762. \doi{10.2337/diacare.28.7.1757}
#' Simental-Mendía LE, Rodríguez-Morán M, Guerrero-Romero F (2008). The product of fasting glucose and triglycerides as surrogate for identifying insulin resistance. Metab Syndr Relat Disord, 6(4):299–304. \doi{10.1089/met.2007.0038}
#' McAuley KA, Williams SM, Mann JI, et al. (2001). Derivation of an index of insulin sensitivity from fasting plasma insulin and triglyceride concentrations. Diabetes Care, 24(3):460–464. \doi{10.2337/diacare.24.3.460}
#' Perseghin G, Lattuada G, De Cobelli F, et al. (2003). Insulin resistance, intramyocellular lipid content, and plasma adiponectin in subjects with type 2 diabetes. J Clin Endocrinol Metab, 88(2):681–686.
#' McLaughlin T, Abbasi F, Cheal K, et al. (2005). Use of metabolic markers to identify overweight individuals who are insulin resistant. Am J Cardiol, 96(3):399–404.
#' Belfiore F, Iannello S, Volpicelli G (1998). Insulin sensitivity indices calculated from basal and OGTT values validated by clamp in healthy and diabetic subjects. Mol Genet Metab, 63(2):134–141.
#'
#' @importFrom tibble tibble
#' @importFrom rlang abort warn inform
#' @export
adipo_is <- function(data,
                     col_map,
                     normalize = "none",
                     verbose = FALSE,
                     na_action = c("keep","omit","error"),
                     na_warn_prop = 0.2,
                     check_extreme = FALSE,
                     extreme_action = c("warn","cap","error","ignore"),
                     extreme_rules = NULL,
                     diagnostics = TRUE,
                     normalize_quiet = FALSE) {

  t0 <- Sys.time()
  na_action <- match.arg(na_action)
  extreme_action <- match.arg(extreme_action)

  # ---- Validate data & col_map ----
  if (!is.data.frame(data)) {
    rlang::abort("adipo_is(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_adipo_error_data_type")
  }
  if (!is.list(col_map) || is.null(names(col_map)) || any(names(col_map) == "")) {
    rlang::abort("adipo_is(): `col_map` must be a named list mapping required keys to column names.",
                 class = "healthmarkers_adipo_error_colmap_type")
  }
  required_keys <- c("G0","I0","TG","HDL_c","FFA","waist","bmi")
  missing_keys <- setdiff(required_keys, names(col_map))
  if (length(missing_keys)) {
    rlang::abort(
      paste0("adipo_is(): missing col_map entries for: ", paste(missing_keys, collapse = ", ")),
      class = "healthmarkers_adipo_error_missing_map"
    )
  }

  mapped_cols <- unname(unlist(col_map[required_keys], use.names = FALSE))
  missing_cols <- setdiff(mapped_cols, names(data))
  if (length(missing_cols)) {
    rlang::abort(
      paste0("adipo_is(): missing required columns in data: ", paste(missing_cols, collapse = ", ")),
      class = "healthmarkers_adipo_error_missing_columns"
    )
  }

  if (!normalize %in% c("none","z","inverse","range","robust")) {
    rlang::abort("adipo_is(): `normalize` must be one of: 'none','z','inverse','range','robust'",
                 class = "healthmarkers_adipo_error_normalize_arg")
  }

  if (isTRUE(verbose)) rlang::inform("-> adipo_is: validating & preparing inputs")

  # ---- Numeric coercion with warnings ----
  for (key in required_keys) {
    cn <- col_map[[key]]
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(data[[cn]] <- as.numeric(old))
      intro <- sum(is.na(data[[cn]]) & !is.na(old))
      if (intro > 0)
        rlang::warn(sprintf("adipo_is(): column '%s' coerced to numeric; NAs introduced: %d", cn, intro))
    }
  }

  # ---- High missingness diagnostics ----
  for (cn in mapped_cols) {
    x <- data[[cn]]
    pna <- sum(is.na(x)) / length(x)
    if (pna >= na_warn_prop && pna > 0) {
      rlang::warn(sprintf("adipo_is(): column '%s' high missingness (%.1f%%).", cn, 100 * pna))
    }
  }

  # ---- NA handling policy ----
  if (na_action == "error") {
    any_na <- Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (any(any_na)) {
      rlang::abort("adipo_is(): required inputs contain missing values (na_action='error').",
                   class = "healthmarkers_adipo_error_missing_values")
    }
  } else if (na_action == "omit") {
    keep <- !Reduce(`|`, lapply(mapped_cols, function(cn) is.na(data[[cn]])))
    if (isTRUE(verbose)) rlang::inform(sprintf("-> adipo_is: omitting %d rows with NA in required inputs", sum(!keep)))
    data <- data[keep, , drop = FALSE]
  }

  # Early return if no data
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      Revised_QUICKI = numeric(),
      VAI_Men_inv = numeric(),
      VAI_Women_inv = numeric(),
      TG_HDL_C_inv = numeric(),
      TyG_inv = numeric(),
      LAP_Men_inv = numeric(),
      LAP_Women_inv = numeric(),
      McAuley_index = numeric(),
      Adipo_inv = numeric(),
      Belfiore_inv_FFA = numeric()
    ))
  }

  # ---- Optional extreme scan / cap (original units) ----
  capped_n <- 0L
  if (isTRUE(check_extreme)) {
    rules <- if (is.null(extreme_rules)) {
      list(
        G0 = c(2, 30),       # mmol/L glucose
        I0 = c(5, 3000),     # pmol/L insulin
        TG = c(0.1, 20),     # mmol/L
        HDL_c = c(0.2, 5),   # mmol/L
        FFA = c(0.05, 3),    # mmol/L
        waist = c(30, 250),  # cm
        bmi = c(10, 80)      # kg/m^2
      )
    } else extreme_rules

    ex_counts <- integer(0)
    for (key in intersect(names(rules), required_keys)) {
      cn <- col_map[[key]]
      rng <- rules[[key]]
      x <- data[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      ex_counts[key] <- sum(bad, na.rm = TRUE)
      if (extreme_action == "cap") {
        x[bad & x < rng[1] & is.finite(x)] <- rng[1]
        x[bad & x > rng[2] & is.finite(x)] <- rng[2]
        data[[cn]] <- x
      }
    }
    total_ex <- sum(ex_counts, na.rm = TRUE)
    if (total_ex > 0) {
      if (extreme_action == "error") {
        rlang::abort(sprintf("adipo_is(): detected %d extreme input values.", total_ex),
                     class = "healthmarkers_adipo_error_extremes")
      } else if (extreme_action == "cap") {
        capped_n <- total_ex
        rlang::warn(sprintf("adipo_is(): capped %d extreme input values into allowed ranges.", total_ex))
      } else if (extreme_action == "warn") {
        rlang::warn(sprintf("adipo_is(): detected %d extreme input values (not altered).", total_ex))
      }
    }
  }

  # ---- Diagnostics (log domain, non-positive) ----
  if (isTRUE(diagnostics)) {
    diag_keys <- c("G0","I0","TG","HDL_c","FFA")
    for (k in diag_keys) {
      x <- data[[col_map[[k]]]]
      n_nonpos <- sum(is.finite(x) & x <= 0)
      if (n_nonpos > 0) {
        rlang::warn(sprintf("adipo_is(): key '%s' has %d non-positive values; log-based indices -> NA.", k, n_nonpos))
      }
    }
  }

  if (isTRUE(verbose)) rlang::inform("-> adipo_is: computing indices")

  # ---- Extraction & unit conversion ----
  g_raw   <- data[[col_map$G0]]
  ins_raw <- data[[col_map$I0]]
  tg_raw  <- data[[col_map$TG]]
  hdl_raw <- data[[col_map$HDL_c]]
  ffa_raw <- data[[col_map$FFA]]
  waist   <- data[[col_map$waist]]
  bmi     <- data[[col_map$bmi]]

  G0  <- g_raw * 18
  I0  <- ins_raw / 6
  TG  <- tg_raw * 88.57
  HDL <- hdl_raw * 38.67
  FFA <- ffa_raw

  # ---- Safe helpers ----
  dz_env <- new.env(parent = emptyenv()); dz_env$counts <- list()
  safe_div <- function(num, den, label) {
    out <- num / den
    zero_den <- is.finite(den) & den == 0
    dz_env$counts[[label]] <- sum(zero_den, na.rm = TRUE)
    out[!is.finite(out)] <- NA_real_
    out
  }
  safe_log <- function(x) {
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x) & x > 0
    out[ok] <- log(x[ok])
    out
  }
  safe_log10 <- function(x) {
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x) & x > 0
    out[ok] <- log10(x[ok])
    out
  }

  # ---- Compute indices ----
  revised_quicki_denom <- safe_log10(I0) + safe_log10(G0) + safe_log10(FFA)
  Revised_QUICKI <- safe_div(1, revised_quicki_denom, "Revised_QUICKI_denom")

  VAI_Men_inv <- -(
    safe_div(waist, (39.68 + 1.88 * bmi), "VAI_men_denom1") *
      safe_div(TG,  (1.03 * 100), "VAI_men_TG_norm") *  # scale constant*100? Keep original ratio scaling; 1.03 factor
      safe_div(1.31 * 100, HDL * 100, "VAI_men_hdl")
  )
  # The constant multiplications cancel; simplified robust version:
  VAI_Men_inv <- -(
    safe_div(waist, (39.68 + 1.88 * bmi), "VAI_men_denom1") *
      safe_div(TG, 1.03, "VAI_men_TG") *
      safe_div(1.31, HDL, "VAI_men_HDL")
  )

  VAI_Women_inv <- -(
    safe_div(waist, (36.58 + 1.89 * bmi), "VAI_women_denom1") *
      safe_div(TG, 0.81, "VAI_women_TG") *
      safe_div(1.52, HDL, "VAI_women_HDL")
  )

  TG_HDL_C_inv <- -safe_div(TG, HDL, "TG_over_HDL")

  TyG_inv <- -{
    arg <- TG * G0 / 2
    lg <- safe_log(arg)
    lg
  }

  LAP_Men_inv   <- -((waist - 65) * TG)
  LAP_Women_inv <- -((waist - 58) * TG)

  McAuley_index <- exp(2.63 - 0.28 * safe_log(I0) - 0.31 * safe_log(TG))

  Adipo_inv <- -(FFA * I0)

  Belfiore_inv_FFA <- -safe_div(2, (I0 * FFA + 1), "Belfiore_FFA_denom")

  out <- tibble::tibble(
    Revised_QUICKI = as.numeric(Revised_QUICKI),
    VAI_Men_inv = as.numeric(VAI_Men_inv),
    VAI_Women_inv = as.numeric(VAI_Women_inv),
    TG_HDL_C_inv = as.numeric(TG_HDL_C_inv),
    TyG_inv = as.numeric(TyG_inv),
    LAP_Men_inv = as.numeric(LAP_Men_inv),
    LAP_Women_inv = as.numeric(LAP_Women_inv),
    McAuley_index = as.numeric(McAuley_index),
    Adipo_inv = as.numeric(Adipo_inv),
    Belfiore_inv_FFA = as.numeric(Belfiore_inv_FFA)
  )

  # ---- Normalization (optional) ----
  if (normalize != "none") {
    if (!("normalize_vec" %in% ls(getNamespace("HealthMarkers"), all.names = TRUE))) {
      rlang::warn("adipo_is(): normalize_vec not found in HealthMarkers namespace; skipping normalization.")
    } else {
      norm_fun <- getExportedValue("HealthMarkers", "normalize_vec")
      out[] <- lapply(out, function(col) {
        # Build argument list dynamically, add quiet if supported
        args <- list(x = col, method = normalize)
        fmls <- names(formals(norm_fun))
        if ("quiet" %in% fmls) {
          args$quiet <- normalize_quiet
          do.call(norm_fun, args)
        } else {
          if (isTRUE(normalize_quiet)) {
            suppressWarnings(do.call(norm_fun, args))
          } else {
            do.call(norm_fun, args)
          }
        }
      })
    }
  }

  # ---- Zero denominator consolidated warning ----
  dz <- dz_env$counts
  dz_total <- if (length(dz)) sum(unlist(dz), na.rm = TRUE) else 0L
  if (dz_total > 0) {
    nz <- unlist(dz); nz <- nz[nz > 0]
    lbl <- paste(sprintf("%s=%d", names(nz), nz), collapse = ", ")
    rlang::warn(sprintf("adipo_is(): zero denominators detected in %d cases (%s).", dz_total, lbl))
  }

  if (isTRUE(verbose)) {
    na_counts <- vapply(out, function(x) sum(is.na(x) | !is.finite(x)), integer(1))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    rlang::inform(sprintf(
      "Completed adipo_is: %d rows; NA/Inf -> %s; capped=%d; denom_zero=%d; elapsed=%.2fs",
      nrow(out),
      paste(sprintf("%s=%d", names(na_counts), na_counts), collapse = ", "),
      capped_n, dz_total, elapsed
    ))
  }

  out
}

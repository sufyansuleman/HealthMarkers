# R/lipid_markers.R

#' Calculate lipid-panel markers, Visceral Adiposity Index (VAI),
#' Lipid Accumulation Product (LAP), and TyG-BMI index
#'
#' Given total cholesterol, HDL, TG (and optionally LDL, ApoB/ApoA1,
#' waist, BMI, glucose), computes:
#' - `non_HDL_c`, `remnant_c`
#' - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`
#' - `ApoB_ApoA1`
#' - `VAI_Men`, `VAI_Women`
#' - `LAP_Men`, `LAP_Women`
#' - `TyG_BMI`
#'
#' @param data A `data.frame` or `tibble` containing your lipid
#'   (and optional anthropometry/glucose) data.
#' @param col_map Named list mapping:
#'   - `TC`    -> total cholesterol
#'   - `HDL_c` -> HDL-C
#'   - `TG`    -> triglycerides
#'   - `LDL_c` -> (optional) LDL-C; if missing, estimated via Friedewald
#'   - `ApoB`, `ApoA1` -> (optional) apolipoproteins
#'   - `waist` -> (optional) waist circumference (cm)
#'   - `BMI`   -> (optional) body mass index (kg/m^2)
#' @param na_action One of c("keep","omit","error","ignore","warn").
#'   - keep/ignore: compute and propagate NA in outputs
#'   - omit: drop rows with NA in required inputs (TC, HDL_c, TG)
#'   - error: abort if any required input contains NA
#'   - warn: like keep, but emit missingness warnings
#' @param check_extreme Logical; if TRUE, scan inputs for out-of-range values.
#' @param extreme_action One of c("warn","cap","error","ignore","NA") controlling
#'   how extremes are handled when check_extreme=TRUE. "cap" truncates to range; "NA" sets flagged values to NA.
#' @param extreme_rules Optional named list of c(min,max) per key to override defaults.
#' @param verbose Logical; if `TRUE`, prints messages about computing markers.
#'
#' @return A tibble with:
#'   - `non_HDL_c`, `remnant_c`
#'   - `ratio_TC_HDL`, `ratio_TG_HDL`, `ratio_LDL_HDL`
#'   - `ApoB_ApoA1`
#'   - `VAI_Men`, `VAI_Women`
#'   - `LAP_Men`, `LAP_Women`
#'   - `TyG_BMI`
#'
#' @references
#' Friedewald WT, Levy RI, Fredrickson DS (1972). Estimation of the concentration of LDL cholesterol in plasma, without use of preparative ultracentrifuge. Clin Chem, 18(6):499–502. \doi{10.1093/clinchem/18.6.499}
#' Amato MC, Giordano C, Galia M, et al. (2010). Visceral Adiposity Index: a reliable indicator of visceral fat function associated with cardiometabolic risk. Diabetes Care, 33(4):920–922. \doi{10.2337/dc09-1825}
#' Kahn HS (2005). The lipid accumulation product performs better than BMI as an indicator of cardiovascular risk in women. Diabetes Care, 28(11):2728–2734. \doi{10.2337/diacare.28.11.2728}
#' Lee YH, Lee SH, Jee JH, et al. (2016). Triglyceride-glucose body mass index is a simple and clinically useful surrogate marker for insulin resistance in nondiabetic adults. Int J Obes (Lond), 40(7):1187–1192. \doi{10.1038/ijo.2016.52}
#' Lee YH, Jung DH, Park YW, et al. (2020). Triglyceride-glucose-body mass index (TyG-BMI) predicts nonalcoholic fatty liver disease. Int J Obes (Lond), 44(9):2101–2110. \doi{10.1038/s41366-020-0599-7}
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @export
lipid_markers <- function(
  data,
  col_map = list(
    TC    = "TC",
    HDL_c = "HDL_c",
    TG    = "TG",
    LDL_c = "LDL_c",
    ApoB  = "ApoB",
    ApoA1 = "ApoA1",
    waist = NULL,
    BMI   = NULL
  ),
  na_action = c("keep","omit","error","ignore","warn"),
  check_extreme = FALSE,
  extreme_action = c("warn","cap","error","ignore","NA"),
  extreme_rules = NULL,
  verbose = FALSE
) {
  na_action_raw <- match.arg(na_action)
  na_action_eff <- if (na_action_raw %in% c("ignore","warn")) "keep" else na_action_raw
  extreme_action <- match.arg(extreme_action)

  # Basic validation
  if (!is.data.frame(data)) stop("lipid_markers(): `data` must be a data.frame or tibble.", call. = FALSE)
  if (is.null(col_map) || !is.list(col_map)) stop("lipid_markers(): `col_map` must be a named list.", call. = FALSE)

  req <- c("TC","HDL_c","TG")
  missing_keys <- setdiff(req, names(col_map))
  if (length(missing_keys)) stop("missing required columns: ", paste(missing_keys, collapse = ", "), call. = FALSE)
  mapped_req <- unname(unlist(col_map[req]))
  if (any(!nzchar(mapped_req))) {
    bad <- req[!nzchar(mapped_req)]
    stop("missing required columns: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  missing_cols <- setdiff(mapped_req, names(data))
  if (length(missing_cols)) stop("missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)

  if (isTRUE(verbose)) message("-> computing lipid markers")

  # Collect used columns (required + present optional + glucose if present)
  optional <- c("LDL_c","ApoB","ApoA1","waist","BMI")
  mapped_opt <- unname(unlist(col_map[optional]))
  mapped_opt <- mapped_opt[!is.na(mapped_opt) & nzchar(mapped_opt) & mapped_opt %in% names(data)]
  used_cols <- unique(c(mapped_req, mapped_opt, intersect("glucose", names(data))))

  # Coerce numeric where applicable; warn if NAs introduced; set non-finite to NA
  for (cn in used_cols) {
    if (!is.numeric(data[[cn]])) {
      old <- data[[cn]]
      suppressWarnings(new <- as.numeric(old))
      introduced_na <- sum(is.na(new) & !is.na(old))
      if (introduced_na > 0L) warning(sprintf("Column '%s' coerced to numeric; NAs introduced: %d", cn, introduced_na), call. = FALSE)
      data[[cn]] <- new
    }
    data[[cn]][!is.finite(data[[cn]])] <- NA_real_
  }

  # NA policy on required inputs
  cc_req <- stats::complete.cases(data[, mapped_req, drop = FALSE])
  if (na_action_eff == "error" && !all(cc_req)) {
    stop("lipid_markers(): missing or non-finite values in required inputs with na_action='error'.", call. = FALSE)
  }
  if (na_action_raw == "warn") {
    any_na_cols <- names(which(colSums(is.na(data[, mapped_req, drop = FALSE])) > 0))
    if (length(any_na_cols)) warning(sprintf("Missing values in: %s.", paste(any_na_cols, collapse = ", ")), call. = FALSE)
  }
  keep_rows <- if (na_action_eff == "omit") cc_req else rep(TRUE, nrow(data))
  d <- data[keep_rows, , drop = FALSE]

  # Extreme scanning/handling
  if (isTRUE(check_extreme)) {
    rules_def <- list(
      TC = c(0, 50), HDL_c = c(0, 10), TG = c(0, 50), LDL_c = c(0, 50),
      ApoB = c(0, 10), ApoA1 = c(0, 10),
      waist = c(30, 250), BMI = c(10, 80),
      glucose = c(0, 50)
    )
    if (is.list(extreme_rules)) {
      for (nm in intersect(names(extreme_rules), names(rules_def))) rules_def[[nm]] <- extreme_rules[[nm]]
    }
    flags <- list(); total <- 0L
    used_for_scan <- intersect(names(rules_def), intersect(c(req, optional, "glucose"), names(col_map)))
    # Add 'glucose' explicitly even if not in col_map
    used_for_scan <- unique(c(used_for_scan, "glucose"))
    # Build name -> column mapping
    name_to_col <- c(col_map, list(glucose = "glucose"))
    for (nm in names(rules_def)) {
      cn <- name_to_col[[nm]]
      if (is.null(cn) || !(cn %in% names(d))) next
      rng <- rules_def[[nm]]
      x <- d[[cn]]
      bad <- is.finite(x) & (x < rng[1] | x > rng[2])
      flags[[nm]] <- bad
      total <- total + sum(bad)
    }
    if (total > 0L) {
      if (extreme_action == "error") {
        stop(sprintf("lipid_markers(): %d extreme input values detected.", total), call. = FALSE)
      } else if (extreme_action == "cap") {
        for (nm in names(flags)) {
          cn <- name_to_col[[nm]]
          if (is.null(cn) || !(cn %in% names(d))) next
          bad <- flags[[nm]]; lo <- rules_def[[nm]][1]; hi <- rules_def[[nm]][2]
          x <- d[[cn]]
          x[bad & is.finite(x) & x < lo] <- lo
          x[bad & is.finite(x) & x > hi] <- hi
          d[[cn]] <- x
        }
        warning(sprintf("lipid_markers(): capped %d extreme input values into allowed ranges.", total), call. = FALSE)
      } else if (extreme_action == "warn") {
        warning(sprintf("lipid_markers(): detected %d extreme input values (not altered).", total), call. = FALSE)
      } else if (extreme_action == "NA") {
        for (nm in names(flags)) {
          cn <- name_to_col[[nm]]
          if (is.null(cn) || !(cn %in% names(d))) next
          bad <- flags[[nm]]
          x <- d[[cn]]; x[bad] <- NA_real_; d[[cn]] <- x
        }
      }
      # ignore -> do nothing
    }
  }

  # Helper safe division
  sdiv <- function(a, b) { z <- a / b; z[!is.finite(z)] <- NA_real_; z }

  # Required
  TC <- d[[col_map$TC]]
  HDL <- d[[col_map$HDL_c]]
  TG  <- d[[col_map$TG]]

  # LDL measured or Friedewald
  LDL <- if (!is.null(col_map$LDL_c) && (col_map$LDL_c %in% names(d))) {
    d[[col_map$LDL_c]]
  } else {
    warning("lipid_markers(): estimating LDL_c via Friedewald (TC - HDL - TG/5)", call. = FALSE)
    TC - HDL - TG / 5
  }

  # ApoB/ApoA1 ratio if both present
  ApoB_ApoA1 <- if (!is.null(col_map$ApoB) && !is.null(col_map$ApoA1) &&
                    all(c(col_map$ApoB, col_map$ApoA1) %in% names(d))) {
    sdiv(d[[col_map$ApoB]], d[[col_map$ApoA1]])
  } else {
    rep(NA_real_, nrow(d))
  }

  out_sub <- tibble::tibble(
    non_HDL_c     = TC - HDL,
    remnant_c     = TC - (HDL + LDL),
    ratio_TC_HDL  = sdiv(TC, HDL),
    ratio_TG_HDL  = sdiv(TG, HDL),
    ratio_LDL_HDL = sdiv(LDL, HDL),
    ApoB_ApoA1    = ApoB_ApoA1
  )

  # VAI if waist and BMI present
  if (!is.null(col_map$waist) && !is.null(col_map$BMI) &&
      all(c(col_map$waist, col_map$BMI) %in% names(d))) {
    W  <- d[[col_map$waist]]
    BM <- d[[col_map$BMI]]
    VAI_Men   <- (W / (39.68 + 1.88 * BM)) * sdiv(TG, 1.03) * sdiv(1.31, HDL)
    VAI_Women <- (W / (36.58 + 1.89 * BM)) * sdiv(TG, 0.81) * sdiv(1.52, HDL)
    out_sub <- dplyr::bind_cols(out_sub, tibble::tibble(VAI_Men = VAI_Men, VAI_Women = VAI_Women))
  }

  # LAP if waist present
  if (!is.null(col_map$waist) && (col_map$waist %in% names(d))) {
    W <- d[[col_map$waist]]
    LAP_Men   <- (W - 65) * TG
    LAP_Women <- (W - 58) * TG
    out_sub <- dplyr::bind_cols(out_sub, tibble::tibble(LAP_Men = LAP_Men, LAP_Women = LAP_Women))
  }

  # TyG-BMI if glucose and BMI present
  if ("glucose" %in% names(d) && !is.null(col_map$BMI) && (col_map$BMI %in% names(d))) {
    TG_mgdl  <- TG * 88.57
    Glu_mgdl <- d$glucose * 18
    TyG      <- log(TG_mgdl * Glu_mgdl / 2)
    TyG_BMI  <- TyG * d[[col_map$BMI]]
    out_sub  <- dplyr::bind_cols(out_sub, tibble::tibble(TyG_BMI = TyG_BMI))
  }

  # Pad back if na_action != 'omit'
  if (na_action_eff != "omit") {
    res <- tibble::tibble(
      non_HDL_c = rep(NA_real_, nrow(data)),
      remnant_c = NA_real_,
      ratio_TC_HDL = NA_real_, ratio_TG_HDL = NA_real_, ratio_LDL_HDL = NA_real_,
      ApoB_ApoA1 = NA_real_
    )
    res <- res[rep(1, nrow(data)), , drop = FALSE]
    res$non_HDL_c[keep_rows]     <- out_sub$non_HDL_c
    res$remnant_c[keep_rows]     <- out_sub$remnant_c
    res$ratio_TC_HDL[keep_rows]  <- out_sub$ratio_TC_HDL
    res$ratio_TG_HDL[keep_rows]  <- out_sub$ratio_TG_HDL
    res$ratio_LDL_HDL[keep_rows] <- out_sub$ratio_LDL_HDL
    res$ApoB_ApoA1[keep_rows]    <- out_sub$ApoB_ApoA1

    # Conditionally add optional outputs with proper padding
    if ("VAI_Men" %in% names(out_sub)) {
      res$VAI_Men   <- NA_real_; res$VAI_Women <- NA_real_
      res$VAI_Men[keep_rows]   <- out_sub$VAI_Men
      res$VAI_Women[keep_rows] <- out_sub$VAI_Women
    }
    if ("LAP_Men" %in% names(out_sub)) {
      res$LAP_Men   <- NA_real_; res$LAP_Women <- NA_real_
      res$LAP_Men[keep_rows]   <- out_sub$LAP_Men
      res$LAP_Women[keep_rows] <- out_sub$LAP_Women
    }
    if ("TyG_BMI" %in% names(out_sub)) {
      res$TyG_BMI <- NA_real_
      res$TyG_BMI[keep_rows] <- out_sub$TyG_BMI
    }
    out <- res
  } else {
    out <- out_sub
  }

  out
}

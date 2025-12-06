# Internal registry & utilities
# -----------------------------
# Helper: normalize choices
.hm_normalize_choice <- function(x, choices) {
  if (length(x) == 0) return(choices[1L])
  x <- match.arg(x, choices)
  x
}

# Helper: safely bind new columns
.hm_bind_new_cols <- function(df, addon) {
  if (is.null(addon) || NROW(addon) == 0) return(df)
  if (NROW(addon) != NROW(df)) {
    rlang::warn("Addon has different number of rows; skipping bind.",
                class = "healthmarkers_health_markers_warn_bind_rows")
    return(df)
  }
  keep <- setdiff(names(addon), names(df))
  if (length(keep)) cbind(df, addon[keep]) else df
}

# Fallback for MetS so aggregator never errors
.hm_mets_fallback <- function(data) {
  tibble::tibble(MetS_simple = rep(NA_integer_, nrow(data)))
}

.hm_marker_registry <- function(verbose = FALSE) {
  reg <- list()
  add <- function(name, fun_name, needs_col_map) {
    f <- get0(fun_name, mode = "function")
    if (is.null(f)) {
      if (isTRUE(verbose)) {
        hm_inform(level = "debug",
          msg = sprintf("Registry: function '%s' not found; skipping group '%s'.", fun_name, name))
      }
    } else {
      reg[[name]] <<- list(fun = f, needs_col_map = needs_col_map)
    }
  }

  # Insulin-related (require col_map)
  add("insulin_fasting",    "fasting_is",            TRUE)
  add("insulin_ogtt",       "ogtt_is",               TRUE)
  add("insulin_adipose",    "adipo_is",              TRUE)
  add("insulin_tracer_dxa", "tracer_dxa_is",         TRUE)

  # Body composition / obesity
  add("adiposity_sds",      "adiposity_sds",         FALSE)
  add("obesity_metrics",    "obesity_metrics",       FALSE)

  # Lipid and atherogenic
  add("lipid",                  "lipid_markers",         FALSE)
  add("atherogenic_indices",    "atherogenic_indices",   TRUE)
  add("atherogenic",            "atherogenic_indices",   TRUE)  # alias

  # Liver
  add("liver",              "liver_markers",         FALSE)
  add("liver_fat",          "liver_fat_markers",     TRUE)

  # Glycemic
  add("glycemic",           "glycemic_markers",      FALSE)

  # Metabolic syndrome
  add("mets",               "metss",                 FALSE)
  add("metabolic_risk",     "metabolic_risk_features", TRUE)

  # Pulmonary
  add("pulmo",              "pulmo_markers",         FALSE)

  # Saliva, Sweat, Urine
  add("saliva",             "saliva_markers",        TRUE)
  add("sweat",              "sweat_markers",         TRUE)
  add("urine",              "urine_markers",         TRUE)

  # Renal and CKD
  add("renal",              "renal_markers",         TRUE)
  add("kidney_kfre",        "kidney_kfre",           FALSE)
  add("ckd_stage",          "ckd_stage",             TRUE)

  # Nutrients and vitamins
  add("nutrient",           "nutrient_markers",      TRUE)
  add("vitamin",            "vitamin_markers",       TRUE)

  # Hormone and inflammation
  add("hormone",            "hormone_markers",       TRUE)
  add("inflammatory",       "inflammatory_markers",  TRUE)
  add("inflammatory_age",   "inflammatory_age",      FALSE)

  # Bone and allostatic load
  add("bone",               "bone_markers",          TRUE)
  add("allostatic_load",    "allostatic_load",       FALSE)

  # Oxidative stress
  add("oxidative",          "oxidative_markers",     TRUE)

  # Frailty / comorbidity / functional
  add("frailty_index",      "frailty_index",        FALSE)
  add("charlson",           "charlson_index",       TRUE)
  add("sarc_f",             "sarc_f_score",         TRUE)

  # Neuro / aging markers
  add("nfl",                "nfl_marker",           TRUE)
  add("inflammatory_age",   "inflammatory_age",     FALSE)  # already present
  add("iAge",               "iAge",                 TRUE)

  # Micronutrient / vitamin sub-panels
  add("vitamin_d_status",   "vitamin_d_status",     TRUE)

  # Single biochemical ratios / corrections
  add("calcium_corrected",  "corrected_calcium",    TRUE)
  add("kyn_trp",            "kyn_trp_ratio",        TRUE)

  reg
}

# Prepare data for specific groups (small convenience fixes)
.hm_prepare_for_group <- function(data, grp) {
  out <- data
  if (identical(grp, "liver")) {
    if (!("triglycerides" %in% names(out)) && "TG" %in% names(out)) {
      out$triglycerides <- out$TG
    }
  }
  if (identical(grp, "mets")) {
    if (!("bp_sys" %in% names(out)) && "sbp" %in% names(out)) out$bp_sys <- out$sbp
    if (!("bp_dia" %in% names(out)) && "dbp" %in% names(out)) out$bp_dia <- out$dbp
    if (!("glucose" %in% names(out)) && "G0" %in% names(out)) out$glucose <- out$G0
    if (!("waist" %in% names(out)) && "WC" %in% names(out)) out$waist <- out$WC
    if (!("triglycerides" %in% names(out)) && "TG" %in% names(out)) out$triglycerides <- out$TG
    if (!("bp_treated" %in% names(out))) out$bp_treated <- FALSE
    if (!("smoker" %in% names(out))) out$smoker <- FALSE
    if (!("diabetes" %in% names(out))) out$diabetes <- FALSE

    if (!("sex" %in% names(out))) {
      out$sex <- "M"
    } else {
      s <- out$sex
      if (is.numeric(s)) {
        out$sex <- ifelse(s == 2, "F", "M")
      } else {
        s <- toupper(as.character(s))
        out$sex <- ifelse(startsWith(s, "F"), "F", "M")
      }
    }

    if (!("race" %in% names(out)) && "ethnicity" %in% names(out)) out$race <- out$ethnicity
    if ("race" %in% names(out)) {
      r <- toupper(as.character(out$race))
      out$race <- ifelse(grepl("^NHW|WHITE|CAUC", r), "NHW",
                  ifelse(grepl("^NHB|BLACK|AFRIC", r), "NHB",
                  ifelse(grepl("HISP|LATIN", r), "HISP", "Other")))
    } else {
      out$race <- "NHW"
    }
  }
  out
}

.hm_safe_call <- function(fun, data, col_map, needs_col_map, verbose, tag, extra_args = list()) {
  if (isTRUE(verbose)) hm_inform(level = "info", msg = paste0("-> ", tag))

  args <- list(data)
  fn_formals <- tryCatch(formals(fun), error = function(e) NULL)
  fn_names <- if (is.null(fn_formals)) character(0) else names(fn_formals)

  if (isTRUE(needs_col_map) && "col_map" %in% fn_names) args$col_map <- col_map

  if (!is.null(fn_formals)) {
    has_dots <- any(fn_names == "...")
    if (!has_dots) {
      extra_args <- extra_args[intersect(names(extra_args), fn_names)]
    }
  } else {
    extra_args <- list()
  }

  if (!is.null(fn_formals)) {
    req <- names(fn_formals)[vapply(fn_formals, function(x) identical(x, quote(expr = )), logical(1))]
    first_arg <- if (length(fn_names)) fn_names[1L] else NULL
    req <- setdiff(req, c("...", first_arg))
    planned <- union(names(args), names(extra_args))
    missing_required <- setdiff(req, planned)
    if (length(missing_required)) {
      if (isTRUE(verbose)) hm_inform(level = "debug",
        msg = sprintf("Skipping '%s': missing required args: %s", tag, paste(missing_required, collapse = ", ")))
      return(NULL)
    }
  }

  call_fun <- function() do.call(fun, c(args, extra_args))
  if (isTRUE(verbose)) {
    tryCatch(call_fun(), error = function(e) {
      hm_inform(level = "debug", msg = sprintf("'%s' failed: %s", tag, conditionMessage(e)))
      NULL
    })
  } else {
    tryCatch(suppressWarnings(call_fun()), error = function(e) NULL)
  }
}

# -----------------------------
# 1. All insulin indices
# -----------------------------
#' Compute insulin sensitivity/resistance panels (fasting, OGTT, adipose, tracer/DXA)
#'
#' @note For scholarly references to specific indices (e.g., HOMA-IR, QUICKI,
#' Raynaud, Belfiore, tracer-derived indices, adiposity-related IS metrics),
#' consult the individual function help pages (e.g. ?fasting_is, ?ogtt_is,
#' ?adipo_is, ?tracer_dxa_is). Citations are intentionally not duplicated here.
#'
#' @param data A data.frame or tibble of raw measurements.
#' @param col_map Named list with keys G0,I0,G30,I30,G120,I120,TG,HDL_c,FFA,waist,weight,bmi,age,sex,rate_palmitate,rate_glycerol,fat_mass.
#' @param normalize One of c("none","z","inverse","range","robust").
#' @param mode One of c("IS","IR","both"). "IR" returns only inverted IR, "IS" only the original IS, "both" returns both with IR_ prefix.
#' @param verbose Logical.
#' @param na_action One of c("keep","omit","error"); forwarded to underlying calculators (HM-CS v2).
#' @return A tibble of IS (and/or IR_) columns.
#' @references
#' Aggregator wrapper. See underlying function help pages for full references:
#' fasting_is(), ogtt_is(), adipo_is(), tracer_dxa_is().
#' @export
#' @examples
#' df <- data.frame(
#'   G0 = 5.2, I0 = 60, G30 = 7.5, I30 = 90, G120 = 6.2, I120 = 80,
#'   TG = 1.5, HDL_c = 1.3, FFA = 0.3, waist = 85, weight = 70, bmi = 24,
#'   age = 40, sex = "M", rate_palmitate = 0.1, rate_glycerol = 0.2, fat_mass = 20
#' )
#' all_insulin_indices(df, col_map = list(
#'   G0="G0", I0="I0", G30="G30", I30="I30", G120="G120", I120="I120",
#'   TG="TG", HDL_c="HDL_c", FFA="FFA", waist="waist", weight="weight",
#'   bmi="bmi", age="age", sex="sex", rate_palmitate="rate_palmitate",
#'   rate_glycerol="rate_glycerol", fat_mass="fat_mass"
#' ), normalize = "none", mode = "IS", verbose = FALSE, na_action = "keep")
all_insulin_indices <- function(
  data,
  col_map,
  normalize = c("none","z","inverse","range","robust"),
  mode = c("both","IS","IR"),
  verbose = TRUE,
  na_action = c("keep","omit","error")
) {
  normalize <- .hm_normalize_choice(normalize, c("none","z","inverse","range","robust"))
  mode <- .hm_normalize_choice(mode, c("both","IS","IR"))
  na_action <- match.arg(na_action)

  common_args <- list(normalize = normalize, na_action = na_action)

  pieces <- list(
    fasting_is    = .hm_safe_call(fasting_is,    data, col_map, TRUE, verbose, "fasting",    common_args),
    ogtt_is       = .hm_safe_call(ogtt_is,       data, col_map, TRUE, verbose, "OGTT",       common_args),
    adipo_is      = .hm_safe_call(adipo_is,      data, col_map, TRUE, verbose, "adipose",    common_args),
    tracer_dxa_is = .hm_safe_call(tracer_dxa_is, data, col_map, TRUE, verbose, "tracer/DXA", common_args)
  )
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) return(tibble::tibble())
  is_tbl <- dplyr::bind_cols(pieces)

  if (mode == "IS") return(is_tbl)

  # IR inversion
  ir_tbl <- purrr::map_dfc(names(is_tbl), function(nm) {
    x <- is_tbl[[nm]]
    out <- ifelse(is.na(x) | x == 0, NA_real_, 1 / x)
    out
  })
  names(ir_tbl) <- paste0("IR_", names(is_tbl))

  if (mode == "IR") return(ir_tbl)

  dplyr::bind_cols(is_tbl, ir_tbl)
}

# -----------------------------
# 2. Metabolic markers (compat)
# -----------------------------
#' Aggregate selected metabolic marker groups
#'
#' @note For references supporting liver, lipid, glycemic, MetS, adiposity and
#' other domain-specific indices, see each underlying function's documentation
#' (e.g. ?liver_markers, ?lipid_markers, ?glycemic_markers, ?metss, ?adiposity_sds).
#' This wrapper omits repeated reference listings to avoid redundancy.
#'
#' @param data A data.frame or tibble.
#' @param col_map Named list for column mapping forwarded to underlying functions.
#' @param which Character vector of groups to compute: c("insulin","adiposity_sds","cardio","lipid","liver","glycemic","mets").
#' @param normalize One of c("none","z","inverse","range","robust").
#' @param mode One of c("both","IS","IR").
#' @param verbose Logical.
#' @param na_action One of c("keep","omit","error"); forwarded to underlying calculators (HM-CS v2).
#' @return Data frame with original columns plus derived markers.
#' @references
#' Aggregator wrapper. See underlying function help pages for full references:
#' all_insulin_indices(), lipid_markers(), liver_markers(), glycemic_markers(), metss().
#' @export
#' @examples
#' df <- data.frame(
#'   TC = 200, HDL_c = 50, TG = 150, LDL_c = 120,
#'   ALT = 30, AST = 20, BMI = 25
#' )
#' metabolic_markers(df, col_map = list(), which = c("lipid","liver"),
#'                   normalize = "none", mode = "both", verbose = FALSE, na_action = "keep")
metabolic_markers <- function(
  data,
  col_map,
  which = c("insulin","adiposity_sds","cardio","lipid","liver","glycemic","mets"),
  normalize = c("none","z","inverse","range","robust"),
  mode = c("both","IS","IR"),
  verbose = TRUE,
  na_action = c("keep","omit","error")
) {
  normalize <- .hm_normalize_choice(normalize, c("none","z","inverse","range","robust"))
  mode <- .hm_normalize_choice(mode, c("both","IS","IR"))
  which <- match.arg(which, several.ok = TRUE)
  na_action <- match.arg(na_action)

  out <- data

  if ("insulin" %in% which) {
    add <- .hm_safe_call(
      all_insulin_indices, out, col_map, TRUE, verbose, "insulin_panel",
      list(normalize = normalize, mode = mode, verbose = verbose, na_action = na_action)
    )
    if (!is.null(add)) out <- .hm_bind_new_cols(out, add)
  }

  if ("adiposity_sds" %in% which) {
    add <- .hm_safe_call(get0("adiposity_sds", mode = "function"), out, col_map, FALSE, verbose, "adiposity_sds",
                         list(verbose = verbose, na_action = na_action))
    if (!is.null(add)) out <- .hm_bind_new_cols(out, add)
  }

  if ("cardio" %in% which) {
    cr <- suppressWarnings(try(cvd_risk(out), silent = TRUE))
    if (!inherits(cr, "try-error")) {
      out <- .hm_bind_new_cols(out, cr)
    } else if (isTRUE(verbose)) {
      hm_inform(level = "debug", msg = "-> cardio skipped (cvd_risk unavailable)")
    }
  }

  if ("lipid" %in% which) {
    add <- .hm_safe_call(lipid_markers, out, col_map, FALSE, verbose, "lipid",
                         list(verbose = verbose, na_action = na_action))
    if (!is.null(add)) out <- .hm_bind_new_cols(out, add)
  }

  if ("liver" %in% which) {
    out2 <- .hm_prepare_for_group(out, "liver")
    if ("triglycerides" %in% names(out2) && !("triglycerides" %in% names(out))) {
      out$triglycerides <- out2$triglycerides
    }
    add <- .hm_safe_call(liver_markers, out2, col_map, FALSE, verbose, "liver",
                         list(verbose = verbose, na_action = na_action))
    if (!is.null(add)) out <- .hm_bind_new_cols(out, add)
  }

  if ("glycemic" %in% which) {
    add <- .hm_safe_call(glycemic_markers, out, col_map, FALSE, verbose, "glycemic",
                         list(verbose = verbose, na_action = na_action))
    if (!is.null(add)) out <- .hm_bind_new_cols(out, add)
  }

  if ("mets" %in% which) {
    out_m <- .hm_prepare_for_group(out, "mets")
    mets_add <- .hm_safe_call(metss, out_m, col_map, FALSE, verbose, "mets",
                              list(verbose = verbose, na_warn_prop = 0, na_action = na_action))
    if (is.null(mets_add)) mets_add <- .hm_mets_fallback(out_m)
    if (!is.null(mets_add)) out <- .hm_bind_new_cols(out, mets_add)
  }

  out
}

# -----------------------------
# 3. Comprehensive aggregator
# -----------------------------
#' Compute all available HealthMarkers categories
#'
#' @note For academic / clinical references tied to each derived marker or
#' index, consult the help pages of the source functions (e.g. ?allostatic_load,
#' ?bone_markers, ?vitamin_markers, ?inflammatory_markers, etc.). This
#' aggregator provides integration only and does not restate citations.
#'
#' @details
#' Common group names for `which` include:
#' \itemize{
#'   \item \code{"lipid"}, \code{"liver"}, \code{"glycemic"}, \code{"mets"}, \code{"oxidative"}
#'   \item \code{"bone"}, \code{"allostatic_load"}, \code{"nutrient"}, \code{"vitamin"}, \code{"vitamin_d_status"}
#'   \item \code{"renal"}, \code{"ckd_stage"}, \code{"kidney_kfre"}
#'   \item \code{"frailty_index"}, \code{"charlson"}, \code{"sarc_f"}
#'   \item \code{"nfl"}, \code{"iAge"}, \code{"calcium_corrected"}, \code{"kyn_trp"}
#' }
#'
#' @param data A data.frame or tibble.
#' @param col_map Named list for column mapping forwarded to underlying functions.
#'   If `col_map` is `NULL` or missing, `all_health_markers()` calls
#'   [hm_infer_cols()] once at the top level to guess a column map from common
#'   synonyms (for example `TG` vs `triglycerides`, `BMI` vs `bmi`,
#'   `HDL_c` vs `HDL`). The inferred `col_map` is then reused for all groups
#'   that require it, and an error is thrown if required keys (e.g. `TG`,
#'   `HDL_c`, `LDL_c`, `TC`, `BMI`, `age`, `sex`) cannot be inferred.
#' @param which "all" or a vector of registry keys (see Details).
#' @param include_insulin Logical; include all_insulin_indices() first.
#' @param normalize One of c("none","z","inverse","range","robust").
#' @param mode One of c("both","IS","IR") passed to insulin indices.
#' @param verbose Logical.
#' @param na_action One of c("keep","omit","error"); forwarded to underlying calculators (HM-CS v2).
#' @return Data frame with original columns plus many derived markers.
#' @references
#' Aggregator wrapper. See underlying function help pages for full references
#' across categories included by `which`.
#' @export
#' @examples
#' df <- data.frame(
#'   TC = 200, HDL_c = 50, TG = 150, LDL_c = 120,
#'   ALT = 30, AST = 20, BMI = 25
#' )
#' all_health_markers(df, col_map = list(), which = c("lipid","liver"),
#'                    include_insulin = FALSE, normalize = "none", mode = "both",
#'                    verbose = FALSE, na_action = "keep")
all_health_markers <- function(
  data,
  col_map,
  which = "all",
  include_insulin = TRUE,
  normalize = c("none","z","inverse","range","robust"),
  mode = c("both","IS","IR"),
  verbose = TRUE,
  na_action = c("keep","omit","error")
) {
  orig_col_map <- if (missing(col_map)) NULL else col_map

  normalize <- .hm_normalize_choice(normalize, c("none","z","inverse","range","robust"))
  mode <- .hm_normalize_choice(mode, c("both","IS","IR"))
  na_action <- match.arg(na_action)

  # Auto-infer col_map if not supplied
  if (missing(col_map) || is.null(col_map)) {
    patterns <- .hm_default_col_patterns_exact()

    required <- c("TG","HDL_c","LDL_c","TC","BMI","age","sex")

    col_map <- hm_infer_cols(
      data,
      patterns      = patterns,
      required_keys = intersect(names(patterns), required),
      verbose       = isTRUE(verbose)
    )
  }

  if (isTRUE(verbose)) {
    user_keys <- if (is.null(orig_col_map)) character(0) else
      names(orig_col_map)[!vapply(orig_col_map, is.null, logical(1))]

    kinds <- ifelse(names(col_map) %in% user_keys, "user", "inferred")

    mapping_str <- paste(
      sprintf("%s->%s (%s)", names(col_map), unlist(col_map), kinds),
      collapse = ", "
    )

    hm_inform(
      level = "inform",
      msg   = sprintf("Column mapping summary: %s", mapping_str)
    )
  }

  reg <- .hm_marker_registry(verbose = isTRUE(verbose))
  reg_names <- names(reg)

  if (identical(which, "all")) {
    which_vec <- setdiff(reg_names, c("atherogenic_indices"))
  } else {
    unknown <- setdiff(which, reg_names)
    if (length(unknown)) {
      rlang::abort(
        paste0("Unknown marker group(s): ", paste(unknown, collapse = ", ")),
        class = "healthmarkers_health_markers_error_unknown_group"
      )
    }
    which_vec <- which
  }

  out <- data
  group_status <- list()

  if (isTRUE(include_insulin)) {
    ins <- .hm_safe_call(
      all_insulin_indices, out, col_map, TRUE, verbose, "insulin_panel",
      extra_args = list(
        normalize = normalize,
        mode      = mode,
        verbose   = verbose,
        na_action = na_action
      )
    )
    if (!is.null(ins)) {
      out <- .hm_bind_new_cols(out, ins)
      group_status[["insulin_panel"]] <- "ok"
    } else {
      group_status[["insulin_panel"]] <- "skipped_or_failed"
    }
  }

  for (grp in which_vec) {
    if (isTRUE(include_insulin) && startsWith(grp, "insulin_")) {
      group_status[[grp]] <- "skipped (covered by insulin_panel)"
      next
    }

    entry <- reg[[grp]]
    if (is.null(entry)) {
      group_status[[grp]] <- "skipped (not in registry)"
      next
    }

    data2 <- .hm_prepare_for_group(out, grp)
    if (identical(grp, "liver") &&
        "triglycerides" %in% names(data2) &&
        !("triglycerides" %in% names(out))) {
      out$triglycerides <- data2$triglycerides
    }
    addon <- .hm_safe_call(
      entry$fun, data2, col_map, entry$needs_col_map, verbose, grp,
      extra_args = list(
        verbose   = verbose,
        na_action = na_action,
        normalize = normalize
      )
    )
    if (!is.null(addon)) {
      out <- .hm_bind_new_cols(out, addon)
      group_status[[grp]] <- "ok"
    } else {
      group_status[[grp]] <- "skipped_or_failed"
    }
  }

  if (isTRUE(verbose) && length(group_status)) {
    ok    <- names(group_status)[group_status == "ok"]
    other <- names(group_status)[group_status != "ok"]

    parts <- character()
    if (length(ok)) {
      parts <- c(parts, sprintf("computed: %s", paste(ok, collapse = ", ")))
    }
    if (length(other)) {
      detail <- paste(
        sprintf("%s (%s)", other, unlist(group_status[other])),
        collapse = "; "
      )
      parts <- c(parts, sprintf("skipped/failed: %s", detail))
    }

    if (length(parts)) {
      hm_inform(
        level = "inform",
        msg   = sprintf("all_health_markers(): summary - %s", paste(parts, collapse = " | "))
      )
    }
  }

  out
}

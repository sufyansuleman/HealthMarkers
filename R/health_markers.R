#' @title   Compute all insulin-sensitivity (and optionally resistance) indices
#' @description
#' `all_insulin_indices()` calls the four IS calculators
#' (`fasting_is()`, `ogtt_is()`, `adipo_is()`, `tracer_dxa_is()`) and then
#' optionally inverts them to IR measures.
#'
#' @param data A data.frame or tibble of raw measurements.
#' @param col_map Named list with keys G0,I0,G30,I30,G120,I120,TG,HDL_c,FFA,waist,weight,bmi,age,sex,rate_palmitate,rate_glycerol,fat_mass.
#' @param normalize One of `c("none","z","inverse","range","robust")`.
#' @param mode  One of `c("IS","IR","both")`.  `"IR"` returns only inverted IR, `"IS"` only the original IS, `"both"` returns both with IR_ prefix.
#' @param verbose Logical.
#' @return A tibble of IS (and/or IR_) columns.
#' @export
# -----------------------------
# Internal registry & utilities
# -----------------------------
.hm_marker_registry <- function() {
  reg <- list()
  add <- function(name, fun_name, needs_col_map) {
    f <- get0(fun_name, mode = "function")
    if (is.null(f)) {
      warning(sprintf("Registry: function '%s' not found; skipping group '%s'.", fun_name, name), call. = FALSE)
    } else {
      reg[[name]] <<- list(fun = f, needs_col_map = needs_col_map)
    }
  }
  add("insulin_fasting",    "fasting_is",            TRUE)
  add("insulin_ogtt",       "ogtt_is",               TRUE)
  add("insulin_adipose",    "adipo_is",              TRUE)
  add("insulin_tracer_dxa", "tracer_dxa_is",         TRUE)
  add("adiposity_sds",      "adiposity_sds",         FALSE)
  add("obesity_metrics",    "obesity_indices",       FALSE)
  add("lipid",              "lipid_markers",         FALSE)
  add("liver",              "liver_markers",         FALSE)
  add("glycemic",           "glycemic_markers",      FALSE)
  add("mets",               "metss",                 FALSE)
  add("metabolic_risk",     "metabolic_risk_features", FALSE)
  add("pulmo",              "pulmo_markers",         FALSE)
  add("saliva",             "saliva_markers",        FALSE)
  add("sweat",              "sweat_markers",         FALSE)
  add("urine",              "urine_markers",         FALSE)
  add("renal",              "renal_markers",         FALSE)
  add("kidney_kfre",        "kidney_failure_risk",   FALSE)
  add("nutrient",           "nutrient_markers",      FALSE)
  add("vitamin",            "vitamin_markers",       FALSE)
  add("hormone",            "hormone_markers",       FALSE)
  add("inflammatory",       "inflammatory_markers",  FALSE)
  # inflammatory_age / iAge may be named differently; only add if present:
  add("inflammatory_age",   "inflammatory_age",      FALSE)
  add("bone",               "bone_markers",          FALSE)
  add("allostatic_load",    "allostatic_load",       FALSE)
  reg
}

.hm_normalize_choice <- function(x, choices) {
  if (is.null(x) || !length(x)) return(choices[1])
  x <- x[1]
  if (!x %in% choices) stop("Invalid choice: ", x)
  x
}

.hm_bind_new_cols <- function(base, addon) {
  if (is.null(addon) || !is.data.frame(addon) || !nrow(addon)) return(base)
  dup <- intersect(names(base), names(addon))
  if (length(dup)) {
    for (nm in dup) {
      new_nm <- nm
      i <- 1
      while (new_nm %in% names(base) || new_nm %in% names(addon)) {
        i <- i + 1
        new_nm <- paste0(nm, "_dup", i)
      }
      names(addon)[names(addon) == nm] <- new_nm
    }
  }
  dplyr::bind_cols(base, addon)
}

.hm_safe_call <- function(fun, data, col_map, needs_col_map, verbose, tag, extra_args = list()) {
  if (verbose) message("-> ", tag)
  args <- list(data)
  if (needs_col_map) args$col_map <- col_map
  if (length(extra_args)) {
    for (nm in names(extra_args)) args[[nm]] <- extra_args[[nm]]
  }
  out <- try(do.call(fun, args), silent = TRUE)
  if (inherits(out, "try-error")) {
    warning(sprintf("Marker '%s' failed: %s", tag, conditionMessage(attr(out, "condition"))), call. = FALSE)
    return(NULL)
  }
  out
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
all_insulin_indices <- function(
  data,
  col_map,
  normalize = c("none","z","inverse","range","robust"),
  mode = c("both","IS","IR"),
  verbose = TRUE,
  na_action = NULL
) {
  normalize <- .hm_normalize_choice(normalize, c("none","z","inverse","range","robust"))
  mode <- .hm_normalize_choice(mode, c("both","IS","IR"))

  common_args <- list(normalize = normalize)
  if (!is.null(na_action)) common_args$na_action <- na_action

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
metabolic_markers <- function(
  data,
  col_map,
  which = c("insulin","adiposity_sds","cardio","lipid","liver","glycemic","mets"),
  normalize = c("none","z","inverse","range","robust"),
  mode = c("both","IS","IR"),
  verbose = TRUE
) {
  normalize <- .hm_normalize_choice(normalize, c("none","z","inverse","range","robust"))
  mode <- .hm_normalize_choice(mode, c("both","IS","IR"))
  which <- match.arg(which, several.ok = TRUE)

  out <- data

  if ("insulin" %in% which) {
    out <- .hm_bind_new_cols(out, all_insulin_indices(out, col_map, normalize = normalize, mode = mode, verbose = verbose))
  }
  if ("adiposity_sds" %in% which) {
    out <- .hm_bind_new_cols(out, adiposity_sds(out, verbose = verbose))
  }
  if ("cardio" %in% which) {
    cr <- try(cvd_risk(out), silent = TRUE)
    if (!inherits(cr, "try-error")) out <- .hm_bind_new_cols(out, cr)
    else if (verbose) message("-> cardio skipped (cvd_risk unavailable)")
  }
  if ("lipid" %in% which) out <- .hm_bind_new_cols(out, lipid_markers(out, verbose = verbose))

  # Liver: supply triglycerides if needed
  if ("liver" %in% which) {
    if (!"triglycerides" %in% names(out) && "TG" %in% names(out)) out$triglycerides <- out$TG
    out <- .hm_bind_new_cols(out, liver_markers(out))
  }

  if ("glycemic" %in% which) out <- .hm_bind_new_cols(out, glycemic_markers(out, verbose = verbose))
  if ("mets" %in% which) out <- .hm_bind_new_cols(out, metss(out, verbose = verbose))
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
all_health_markers <- function(
  data,
  col_map,
  which = "all",
  include_insulin = TRUE,
  normalize = c("none","z","inverse","range","robust"),
  mode = c("both","IS","IR"),
  verbose = TRUE,
  na_action = NULL
) {
  normalize <- .hm_normalize_choice(normalize, c("none","z","inverse","range","robust"))
  mode <- .hm_normalize_choice(mode, c("both","IS","IR"))

  reg <- .hm_marker_registry()
  reg_names <- names(reg)

  if (identical(which, "all")) {
    which_vec <- reg_names
  } else {
    unknown <- setdiff(which, reg_names)
    if (length(unknown)) stop("Unknown marker group(s): ", paste(unknown, collapse = ", "))
    which_vec <- which
  }

  out <- data

  # 1. Insulin panel first (optional)
  if (isTRUE(include_insulin)) {
    out <- .hm_bind_new_cols(out, all_insulin_indices(out, col_map, normalize = normalize, mode = mode, verbose = verbose, na_action = na_action))
  }

  # 2. Other groups
  for (grp in which_vec) {
    # skip insulin_x groups already covered
    if (startsWith(grp, "insulin_")) next
    entry <- reg[[grp]]
    addon <- .hm_safe_call(entry$fun, out, col_map, entry$needs_col_map, verbose, grp,
                           extra_args = list(normalize = normalize, na_action = na_action, verbose = verbose))
    if (!is.null(addon)) out <- .hm_bind_new_cols(out, addon)
  }

  out
}

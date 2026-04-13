
#' Infer column names from user data based on flexible patterns, with logging
#'
#' Given a data.frame and a named mapping spec (e.g., list(G0 = NULL, I0 = NULL)),
#' infer the source column names for each key using a set of regex patterns.
#' You can supply your own patterns and "preferred" names to deterministically
#' resolve ambiguous matches. A structured log is kept and can be written to disk.
#'
#' This helper produces a col_map you can pass to HealthMarkers functions
#' (e.g., fasting_is(), lipid-derived indices).
#'
#' Backward compatibility:
#' - By default, strict = TRUE and strategy = "error" keep prior behavior:
#'   - Error if no match found.
#'   - Error if multiple candidates found.
#' - You can opt into smarter resolution via strategy = "prefer" or "first".
#'
#' @param data A data.frame or tibble whose column names are scanned.
#' @param map Named list where names are target keys (e.g., "G0","I0","TG") and
#'   values are NULL (to infer) or a user-supplied column name (to keep as-is).
#' @param verbose Logical; if TRUE, messages are printed for each mapping decision.
#'   Default TRUE.
#' @param log_file Optional file path; if supplied, a human-readable mapping log is written there.
#' @param patterns Optional named character vector of regex patterns keyed by the
#'   same names as `map`. If NULL, a built-in dictionary is used.
#' @param prefer Optional named list of character vectors with preferred column
#'   names for each key, used to resolve multiple matches deterministically.
#'   Matching is case-insensitive and exact against the provided names.
#' @param strategy One of c("error","prefer","first","stable") controlling resolution when
#'   there are multiple candidates. Default "error" (backward compatible).
#'   - "prefer": use `prefer` names first; else fall back to "stable" tie-break.
#'   - "first": take the first match in data's column order.
#'   - "stable": choose shortest name, then alphabetical.
#' @param strict Logical; if TRUE (default), missing matches error. If FALSE, missing
#'   matches leave \code{map[[key]]} as NULL and issue a warning.
#' @param ignore_case Logical; pass to grep(ignore.case = ...). Default TRUE.
#' @param fuzzy Logical; if TRUE and no regex matches are found, attempt a fuzzy match
#'   with agrep using `max_distance`. Default FALSE.
#' @param max_distance Numeric in \eqn{[0,1]} passed to agrep when fuzzy = TRUE. Default 0.1.
#' @param return One of c("map","list"). "map" (default) invisibly returns the
#'   filled mapping list. "list" returns a list(map = ..., log = tibble) for auditing.
#'
#' @return By default, invisibly returns the filled `map`. If return = "list",
#'   returns a list(map = \code{named list}, log = \code{tibble}).
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   fasting_glucose = c(5.5, 6.1),
#'   fasting_insulin = c(60, 88),
#'   TG = c(120, 150),
#'   `HDL-c` = c(50, 45),
#'   age = c(55, 60)
#' )
#' spec <- list(G0 = NULL, I0 = NULL, TG = NULL, HDL_c = NULL)
#' # Backward-compatible: strict and "error" strategy
#' res1 <- infer_cols(df, spec, verbose = FALSE)
#' # Prefer/resolve ties deterministically
#' res2 <- infer_cols(df, spec, strategy = "prefer", verbose = TRUE)
#' # Get structured log
#' res3 <- infer_cols(df, spec, return = "list")
infer_cols <- function(data,
                       map,
                       verbose = TRUE,
                       log_file = NULL,
                       patterns = NULL,
                       prefer = NULL,
                       strategy = c("error", "prefer", "first", "stable"),
                       strict = TRUE,
                       ignore_case = TRUE,
                       fuzzy = FALSE,
                       max_distance = 0.1,
                       return = c("map", "list")) {
  strategy <- match.arg(strategy)
  return <- match.arg(return)

  # ---- validations ----
  if (!is.data.frame(data)) stop("HealthMarkers::infer_cols: `data` must be a data.frame or tibble.")
  if (!is.list(map) || is.null(names(map)) || any(names(map) == "")) {
    stop("HealthMarkers::infer_cols: `map` must be a named list (e.g., list(G0=NULL, I0=NULL)).")
  }
  keys <- names(map)
  cn <- names(data)

  # Built-in regex dictionary (separator-tolerant: space/_/-/.)
  if (is.null(patterns)) {
    sep <- "[-_. ]"             # flexible separators
    opt <- function(x) paste0("(?:", x, ")?")  # optional group (non-capturing)
    # Tokens for common fields
    patterns <- c(
      # ---- Insulin / OGTT / Fasting ----
      G0   = paste0("^(?:G", opt(sep), "0(?:\\b|$)|glu(?:cose)?", opt(sep), "0(?:\\b|$)|",
                    "fast(?:ing)?", opt(sep), opt("plasma"), opt(sep), "glu(?:cose)?(?:\\b|$)|",
                    "FPG(?:\\b|$))"),
      I0   = paste0("^(?:I", opt(sep), "0(?:\\b|$)|ins(?:ulin)?", opt(sep), "0(?:\\b|$)|",
                    "fast(?:ing)?", opt(sep), "ins(?:ulin)?(?:\\b|$))"),
      G30  = paste0("^(?:G", opt(sep), "30(?:\\b|$)|glu(?:cose)?", opt(sep), "30(?:\\b|$))"),
      I30  = paste0("^(?:I", opt(sep), "30(?:\\b|$)|ins(?:ulin)?", opt(sep), "30(?:\\b|$))"),
      G120 = paste0("^(?:G", opt(sep), "120(?:\\b|$)|glu(?:cose)?", opt(sep), "120(?:\\b|$))"),
      I120 = paste0("^(?:I", opt(sep), "120(?:\\b|$)|ins(?:ulin)?", opt(sep), "120(?:\\b|$))"),

      # ---- Anthropometry ----
      height    = paste0("^(?:height|Height|height_m|body_height|stature|ht_cm)(?:\\b|$)"),
      weight    = paste0("^(?:weight|body", sep, "?weight)(?:\\b|$)"),
      bmi       = paste0("^(?:BMI|body", sep, "?mass", sep, "?index)(?:\\b|$)"),
      BMI       = paste0("^(?:BMI|body", sep, "?mass", sep, "?index)(?:\\b|$)"),
      waist     = paste0("^(?:WC|waist|waist", sep, "?circumference)(?:\\b|$)"),
      WC        = paste0("^(?:WC|waist|waist", sep, "?circumference)(?:\\b|$)"),
      age       = "^(?:age|years?)(?:\\b|$)",
      sex       = "^(?:sex|gender)(?:\\b|$)",
      SBP       = paste0("^(?:SBP|sys(?:tolic)?", sep, "?(?:blood", sep, "?)?pres(?:sure)?)(?:\\b|$)"),
      DBP       = paste0("^(?:DBP|dia(?:stolic)?", sep, "?(?:blood", sep, "?)?pres(?:sure)?)(?:\\b|$)"),
      FFA       = paste0("^(?:FFA|NEFA|free", sep, "?fatty", sep, "?acids?)(?:\\b|$)"),
      fat_mass  = paste0("^(?:fat", sep, "?mass|FM)(?:\\b|$)"),
      lean_mass = paste0("^(?:lean", sep, "?mass|LM|lean", sep, "?body", sep, "?mass)(?:\\b|$)"),
      ALM       = paste0("^(?:ALM|alm|ALM_kg|appendicular", sep, "?lean", sep, "?mass)(?:\\b|$)"),

      # ---- Lipids ----
      TG     = "^(?:TG|tri(?:acyl)?glyceri?des?)(?:\\b|$)",
      HDL_c  = paste0("^(?:HDL(?:", sep, "?c)?|HDL", sep, "?chol(?:esterol)?|",
                      "high", sep, "?density", sep, "?lipoprotein)(?:\\b|$)"),
      LDL_c  = paste0("^(?:LDL(?:", sep, "?c)?|LDL", sep, "?chol(?:esterol)?|",
                      "low", sep, "?density", sep, "?lipoprotein)(?:\\b|$)"),
      TC     = paste0("^(?:TC|total", sep, "?chol(?:esterol)?)(?:\\b|$)"),
      ApoB   = paste0("^(?:Apo", sep, "?B|apoB|apolipoprotein", sep, "?B|ApoB100)(?:\\b|$)"),
      ApoA1  = paste0("^(?:Apo", sep, "?A1|apoA1|apolipoprotein", sep, "?A1)(?:\\b|$)"),

      # ---- Cardiometabolic aliases ----
      chol_total    = paste0("^(?:chol", sep, "?total|total", sep, "?chol(?:esterol)?)(?:\\b|$)"),
      chol_ldl      = paste0("^(?:chol", sep, "?ldl|ldl)(?:\\b|$)"),
      chol_hdl      = paste0("^(?:chol", sep, "?hdl|hdl)(?:\\b|$)"),
      triglycerides = "^(?:TG|tri(?:acyl)?glyceri?des?)(?:\\b|$)",

      # ---- Liver ----
      AST        = paste0("^(?:AST|aspartate", sep, "?aminotransferase)(?:\\b|$)"),
      ALT        = paste0("^(?:ALT|alanine", sep, "?aminotransferase)(?:\\b|$)"),
      GGT        = paste0("^(?:GGT|gamma", sep, "?glutamyl", sep, "?transferase)(?:\\b|$)"),
      platelets  = paste0("^(?:platelets?|platelet", sep, "?count)(?:\\b|$)"),
      albumin    = paste0("^(?:albumin|serum", sep, "?albumin)(?:\\b|$)"),
      bilirubin  = "^(?:bilirubin|bili)(?:\\b|$)",
      creatinine = "^(?:creatinine|creat)(?:\\b|$)",

      # ---- Electrolytes & minerals ----
      calcium   = paste0("^(?:calcium|ca|Ca|ca_mgdl)(?:\\b|$)"),
      phosphate = paste0("^(?:phosphate|phos)(?:\\b|$)"),
      magnesium = paste0("^(?:magnesium|Mg|mg)(?:\\b|$)"),

      # ---- Renal / Urine ----
      eGFR             = paste0("^(?:eGFR|estimated", sep, "?GFR|CKD", sep, "?EPI)(?:\\b|$)"),
      UACR             = paste0("^(?:UACR|ACR|urine", sep, "?albumin", sep, "?creatinine", sep, "?ratio)(?:\\b|$)"),
      urine_albumin    = paste0("^(?:urine", sep, "?albumin)(?:\\b|$)"),
      urine_creatinine = paste0("^(?:urine", sep, "?creatinine)(?:\\b|$)"),
      plasma_Na        = paste0("^(?:plasma", sep, "?Na|serum", sep, "?Na)(?:\\b|$)"),
      urine_Na         = paste0("^(?:urine", sep, "?Na)(?:\\b|$)"),
      serum_creatinine = paste0("^(?:serum", sep, "?creatinine)(?:\\b|$)"),

      # ---- Sweat ----
      sweat_chloride   = paste0("^(?:sweat", sep, "?chloride)(?:\\b|$)"),
      sweat_Na         = paste0("^(?:sweat", sep, "?Na)(?:\\b|$)"),
      sweat_K          = paste0("^(?:sweat", sep, "?K)(?:\\b|$)"),
      sweat_lactate    = paste0("^(?:sweat", sep, "?lactate)(?:\\b|$)"),
      weight_before    = paste0("^(?:weight", sep, "?before)(?:\\b|$)"),
      weight_after     = paste0("^(?:weight", sep, "?after)(?:\\b|$)"),
      duration         = paste0("^(?:duration|time", sep, "?h)(?:\\b|$)"),
      body_surface_area = paste0("^(?:body", sep, "?surface", sep, "?area|BSA)(?:\\b|$)"),

      # ---- Tracer / metabolic flux ----
      rate_glycerol  = paste0("^(?:rate", sep, "?glycerol|glycerol", sep, "?fm)(?:\\b|$)"),
      rate_palmitate = paste0("^(?:rate", sep, "?palmitate|palmitate", sep, "?fm)(?:\\b|$)"),

      # ---- Tryptophan-kynurenine pathway ----
      tryptophan  = paste0("^(?:tryptophan|Trp", sep, "?uM|tryptophan", sep, "?umolL)(?:\\b|$)"),
      kynurenine  = paste0("^(?:kynurenine|Kyn", sep, "?nM|kynurenine", sep, "?nmolL)(?:\\b|$)"),

      # ---- Saliva ----
      saliva_cort1   = paste0("^(?:saliva", sep, "?cort1|cortisol", sep, "?wake)(?:\\b|$)"),
      saliva_cort2   = paste0("^(?:saliva", sep, "?cort2|cortisol", sep, "?30)(?:\\b|$)"),
      saliva_cort3   = paste0("^(?:saliva", sep, "?cort3|cortisol", sep, "?60)(?:\\b|$)"),
      saliva_amylase = paste0("^(?:saliva", sep, "?amylase)(?:\\b|$)"),
      saliva_glucose = paste0("^(?:saliva", sep, "?glucose)(?:\\b|$)")
    )
  }

  # default preferences (exact names to pick first if multiple matches)
  if (is.null(prefer)) {
    prefer <- list(
      G0 = c("G0", "glucose_0", "glucose0", "fasting_glucose", "fpg"),
      I0 = c("I0", "insulin_0", "insulin0", "fasting_insulin"),
      TG = c("TG", "triglycerides"),
      HDL_c = c("HDL_c", "HDL-c", "HDLc", "HDL", "hdl_chol"),
      LDL_c = c("LDL_c", "LDL-c", "LDLc", "LDL", "ldl_chol"),
      TC = c("TC", "total_cholesterol"),
      ApoB = c("ApoB", "apoB", "apolipoprotein_B", "ApoB100"),
      ApoA1 = c("ApoA1", "apoA1", "apolipoprotein_A1")
    )
  }

  logs <- list()
  add_log <- function(key, selected, candidates, reason) {
    logs[[length(logs) + 1L]] <<- list(
      key = key,
      selected = if (is.null(selected)) NA_character_ else selected,
      candidates = paste(candidates, collapse = ", "),
      reason = reason
    )
    if (isTRUE(verbose)) {
      msg <- sprintf("HealthMarkers::infer_cols - %s -> %s (%s)",
                     key, ifelse(is.null(selected), "<none>", selected), reason)
      message(msg)
    }
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a

  pick_preferred <- function(cands, prefs) {
    if (length(cands) == 0L || length(prefs) == 0L) return(NULL)
    lc <- tolower(cands); lp <- tolower(prefs)
    for (p in lp) {
      hit <- which(lc == p)[1]
      if (length(hit) == 1L && !is.na(hit)) return(cands[hit])
    }
    NULL
  }

  pick_stable <- function(cands) {
    if (length(cands) <= 1L) return(cands)
    lens <- nchar(cands)
    ord <- order(lens, cands)
    cands[ord][1L]
  }

  # iterate over keys
  for (nm in keys) {
    # keep user-specified mapping
    if (!is.null(map[[nm]])) {
      if (map[[nm]] %in% cn) {
        add_log(nm, map[[nm]], map[[nm]], "user-supplied")
        next
      } else {
        msg <- sprintf("HealthMarkers::infer_cols: user-supplied column '%s' for key '%s' not found in data.", map[[nm]], nm)
        if (isTRUE(strict)) stop(msg) else { warning(msg, call. = FALSE); map[[nm]] <- NULL }
      }
    }

    pat <- if (nm %in% names(patterns)) patterns[[nm]] else NULL
    if (is.null(pat) || is.na(pat)) {
      msg <- sprintf("HealthMarkers::infer_cols: no pattern defined for '%s'.", nm)
      if (isTRUE(strict)) stop(msg) else { warning(msg, call. = FALSE); add_log(nm, NULL, character(0), "no pattern"); next }
    }

    hits <- grep(pat, cn, ignore.case = isTRUE(ignore_case), value = TRUE)
    # Fuzzy fallback if enabled and no hits
    if (length(hits) == 0L && isTRUE(fuzzy)) {
      idx <- tryCatch(agrep(nm, cn, max.distance = max_distance, ignore.case = isTRUE(ignore_case)), error = function(e) integer(0))
      if (length(idx)) {
        hits <- cn[idx]
      }
    }

    if (length(hits) == 0L) {
      msg <- sprintf("HealthMarkers::infer_cols: no match for '%s'.", nm)
      if (isTRUE(strict)) stop(msg) else { warning(msg, call. = FALSE); add_log(nm, NULL, hits, "no match"); next }
    }

    if (length(hits) == 1L) {
      map[[nm]] <- hits
      add_log(nm, hits, hits, "unique match")
      next
    }

    # resolve multiple candidates
    sel <- NULL
    reason <- NULL
    if (strategy == "prefer" && !is.null(prefer[[nm]])) {
      sel <- pick_preferred(hits, prefer[[nm]])
      if (!is.null(sel)) reason <- "preferred match"
    }
    if (is.null(sel)) {
      if (strategy == "first") {
        sel <- hits[1L]
        reason <- "first in data order"
      } else if (strategy == "stable" || (strategy == "prefer" && is.null(reason))) {
        sel <- pick_stable(hits)
        reason <- "stable tie-break (shortest, then alphabetical)"
      } else if (strategy == "error") {
        stop(sprintf("HealthMarkers::infer_cols: multiple candidates for '%s': %s",
                     nm, paste(hits, collapse = ", ")))
      }
    }

    map[[nm]] <- sel
    add_log(nm, sel, hits, reason %||% "resolved")
  }

  # write log if requested
  if (!is.null(log_file)) {
    lines <- vapply(logs, function(x) {
      sprintf("%s -> %s | candidates: [%s] | %s", x$key, x$selected, x$candidates, x$reason)
    }, character(1))
    writeLines(lines, con = log_file)
    if (isTRUE(verbose)) message("HealthMarkers::infer_cols - wrote inference log to ", log_file)
  }

  if (return == "list") {
    log_tbl <- if (length(logs)) {
      tibble::tibble(
        key = vapply(logs, `[[`, character(1), "key"),
        selected = vapply(logs, `[[`, character(1), "selected"),
        candidates = vapply(logs, `[[`, character(1), "candidates"),
        reason = vapply(logs, `[[`, character(1), "reason")
      )
    } else {
      tibble::tibble(key = character(0), selected = character(0), candidates = character(0), reason = character(0))
    }
    return(list(map = map, log = log_tbl))
  }

  invisible(map)
}

#' Simplified column inference for HealthMarkers aggregators
#'
#' Exact-name matching helper used by `all_health_markers()` and related wrappers.
#' It picks the first matching candidate for each key, logs decisions via
#' `hm_inform()` when `verbose = TRUE`, and errors if required keys cannot be
#' resolved.
#'
#' @param data Data frame whose column names are scanned.
#' @param patterns Named list of character vectors, each giving candidate column
#'   names for a key (first match wins).
#' @param required_keys Character vector of keys that must resolve; otherwise an
#'   error is raised.
#' @param verbose Logical; if TRUE, emits hm_inform() messages for matches and
#'   unresolved keys.
#' @return Named list mapping keys to column names; unresolved non-required keys
#'   become `NA_character_`.
#' @rdname infer_cols
#' @keywords internal
hm_infer_cols <- function(data, patterns, required_keys = names(patterns), verbose = FALSE) {
  if (!is.data.frame(data)) {
    rlang::abort("hm_infer_cols(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_infer_error_data_type")
  }
  if (!is.list(patterns) || is.null(names(patterns)) || any(names(patterns) == "")) {
    rlang::abort("hm_infer_cols(): `patterns` must be a named list of candidate names per key.",
                 class = "healthmarkers_infer_error_patterns_type")
  }

  hm_inform(level = if (isTRUE(verbose)) "inform" else "debug", msg = "hm_infer_cols(): inferring column map")

  resolved <- list()
  for (key in names(patterns)) {
    cands <- unique(na.omit(as.character(patterns[[key]])))
    hit <- intersect(cands, names(data))
    if (length(hit) >= 1L) {
      resolved[[key]] <- hit[[1L]]
      if (isTRUE(verbose)) hm_inform(level = "debug", msg = sprintf("hm_infer_cols(): key '%s' -> '%s'", key, hit[[1L]]))
    } else {
      resolved[[key]] <- NA_character_
      hm_inform(level = "debug", msg = sprintf("hm_infer_cols(): key '%s' unresolved", key))
    }
  }

  # Ensure required keys are found
  missing_req <- required_keys[is.na(rlang::`%||%`(unlist(resolved[required_keys]), NA_character_))]
  if (length(missing_req)) {
    rlang::abort(
      sprintf("hm_infer_cols(): could not infer columns for required keys: %s",
              paste(missing_req, collapse = ", ")),
      class = "healthmarkers_infer_error_missing_required"
    )
  }

  resolved
}

#' Internal: default exact-name patterns for hm_infer_cols()
#' Not exported; used by all_health_markers() when col_map is missing.
#' Each vector lists every column-name spelling observed in the package's
#' simulated data, the Inter99/ADDITION real phenotype dataset, and common
#' biobank / cohort study conventions.  The first element is the canonical
#' internal key name.
#' @keywords internal
.hm_default_col_patterns_exact <- function() {
  list(

  ## =========================================================
  ## Demographics & basic
  ## =========================================================
  age = c("age","Age","AGE","age_year","age_years","age_0","baseline_age",
          "participant_age","age_at_visit","age_at_recruitment","age_enrol"),
  sex = c("sex","Sex","SEX","gender","Gender","gender_qrisk",
          "biological_sex","sex_at_birth","male","Male","female","Female",
          "woman","women"),
  ethnicity = c("ethnicity","Ethnicity","ethiniciy","race","Race",
                "ethnicity_qrisk","ASIAN","asian","BLACK","black",
                "HISPANIC","hispanic","WHITE","white","Caucasian","caucasian",
                "NHW","NHW_M","NHB","HISP","latino"),

  ## =========================================================
  ## Anthropometry & obesity
  ## =========================================================
  height = c("height","Height","HEIGHT","height_m","height_5",
             "body_height","stature","standing_height","ht_cm","cm"),
  weight = c("weight","Weight","WEIGHT","weight_kg","weight_0","weight_1",
             "weight_3","weight_5","weight_before","weight_after","wt","wt_kg","kg"),
  BMI    = c("BMI","bmi","bmi_0","bmi_1","bmi_3","bmi_5",
             "BMI_value","bmi_calc","BMIkgm2","bmi_kg_m2","body_mass_index",
             "BMI3025"),
  waist  = c("waist","Waist","waist_cm","waist_0","waist_1","waist_3","waist_5",
             "waist_circ","waist_circumference","waist_measure","WC","wc"),
  hip    = c("hip","Hip","hip_cm","hip_0","hip_1","hip_3","hip_5",
             "hip_circ","hip_circumference"),
  whr    = c("whr","WHR","whratio","whratio_5","waist_hip_ratio",
             "waist_to_BMI_ratio","waist_to_height_ratio","WHRadjBMI"),
  ABSI   = c("ABSI","absi"),
  BAI    = c("BAI","bai"),
  BRI    = c("BRI","bri"),
  RFM    = c("RFM","rfm"),
  WHRadjBMI   = c("WHRadjBMI"),
  obesity_metrics = c("obesity_metrics","metabolic_risk_features"),

  ## =========================================================
  ## Body composition / DXA
  ## =========================================================
  fat_mass = c("fat_mass","fatmass","FM","fm","fm_kg","fm_wt","fat_kg","body_fat_mass",
               "fatpct","fat_percent","fat_percentage","fatfreemass",
               "fatfreemassindex","fatmassindex","body_fat_perc"),
  lean_mass = c("lean_mass","leanmass","LM","lm","lm_kg","fat_free_mass",
                "fatfreemass","FFM","ffm"),
  ALM   = c("ALM","alm","ALM_kg","appendicular_lean_mass"),
  VAT   = c("VAT","vat","visceral_fat","visceral_adipose_tissue"),
  SAT   = c("SAT","SAT_VAT_ratio","subcutaneous_adipose_tissue",
            "subcutaneous_fat","SAT_kg","SAT_cm2"),
  BMD   = c("BMD","bmd","bmd_t","bone_mineral_density"),
  BMD_ref_mean = c("BMD_ref_mean"),
  BMD_ref_sd   = c("BMD_ref_sd"),
  liver_fat    = c("liver_fat","liver_fat_pct"),

  ## =========================================================
  ## Glycemic markers (glucose / HbA1c)
  ## =========================================================
  fasting_glucose = c(
    "G0","pglu0","pglu0_0","glu0","fasting_glucose","glucose_fasting",
    "glucose0","glucose_0","fpg","FPG","fasting_plasma_glucose",
    "fasting_bg","fbg","FBG","fast_glu","bg0","BG0",
    "plasma_glucose","serum_glucose","glucose_f","GLUC","glu","Glu",
    "GLUCOSE","glucose","nglu0","p_glucose0"),
  glucose_30m  = c("G30","pglu30","pglu30_0","glu30","glucose_30m",
                   "glucose_30","bg30","G30_0"),
  glucose_120m = c("G120","pglu120","pglu120_0","glu120","glucose_120",
                   "glucose_2h","glucose_120m","bg120","glu120m","G120_0"),
  glucose_generic = c("glucose"),
  HbA1c = c("hba1c","HbA1c","hba1c_0","hba1c_1","hba1c_3","hba1c_5",
             "A1c","a1c","HBA1C","HbA1C","glycated_hba1c","hba1c_mmol",
             "hemoglobin_a1c","HBAIC","ghb","GHB","hgba1c",
             "glycohemoglobin","HbA1c_ifcc"),
  glycated_albumin = c("glycated_albumin","GlycatedAlbuminPct"),

  ## =========================================================
  ## Insulin / OGTT / C-peptide
  ## =========================================================
  fasting_insulin = c(
    "I0","insu0","insu0_0","ins0","insulin0","insulin_0","fasting_insulin",
    "insulin_fasting","ins_f","basal_insulin","FI","fi","fast_ins",
    "Insulin","INS","ins","plasma_insulin","serum_insulin","Xinsulin",
    "insulin_f","ins_fasting","insulin_bas","homair"),
  insulin_30m  = c("I30","insu30","insu30_0","ins30","insulin_30",
                   "ins_30","insulin_30m"),
  insulin_120m = c("I120","insu120","insu120_0","ins120","insulin_120",
                   "ins_2h","insulin_120m"),
  insulin_ogtt    = c("insulin_ogtt","insulin_panel","insulin_adipose",
                      "insulin_tracer_dxa","insulin"),
  c_peptide_0     = c("cp0","cpep0","C_peptide","c_peptide","cp0_0"),
  c_peptide_30    = c("cp30","cpep30"),
  c_peptide_120   = c("cp120","cpep120"),
  fast_is_index   = c("fasting_is","fasting"),
  glucose_markers = c("glycemic","glycemic_markers"),

  ## Derived IR / IS indices stored as columns
  HOMA_IR   = c("HOMA_IR","homair","z_HOMA","HOMA","homa_ir","homa","IR","IR_"),
  ISI       = c("ISI_matsuda","ISIstum0_120","isi","IS","insulin_sensitivity_index",
                "BIG_SI","BIG_AIR"),
  AISI      = c("AISI"),
  Avignon_Si0   = c("Avignon_Si0"),
  Avignon_Si120 = c("Avignon_Si120"),
  OGTT_ISI      = c("ogtt_is","OGTT","ISI_matsuda","ISIstum0_120"),

  ## =========================================================
  ## Lipids
  ## =========================================================
  total_cholesterol = c(
    "TC","tc","chol","Chol","CHOL","chol_0","chol_1","chol_3","chol_5",
    "total_cholesterol","total_chol","chol_total","chol_t","cholesterol",
    "Cholesterol","TCHOL","tot_chol","totalchol","TotalChol","total.chol",
    "s_chol","serum_cholesterol","plasma_cholesterol","TC_mmol","CHOLE",
    "tcholesterol","cholesterol_total"),
  HDL_c = c(
    "HDL_c","HDL","hdlc","hdlc_0","hdlc_1","hdlc_3","hdlc_5",
    "HDLc","hdl_chol","chol_hdl","hdl_c","HDL_cholesterol",
    "hdl_cholesterol","hdlchol","HDLC","s_HDL","p_hdl","HDL_mmol",
    "hdl_mmol","HDLCHOLEST","chol_hdl_c","high_density_lipoprotein",
    "total.hdl","cholesterol_HDL_ratio"),
  LDL_c = c(
    "LDL_c","LDL","ldl","ldl_5","LDLc","ldl_c","ldl_chol",
    "ldlcalc","LDLcalc","LDL_PN","ldl_cholesterol","LDL_cholesterol",
    "LDLC","ldlchol","ldl_calc","s_LDL","p_ldl","LDL_mmol","ldl_mmol",
    "LDLCHOLEST","ldl_direct","LDL_direct","low_density_lipoprotein"),
  TG    = c(
    "TG","tg","trig","trig_0","trig_1","trig_3","trig_5",
    "triglycerides","Triglycerides","TRIGLYCERIDES","trigs","tgs",
    "TRIG","TryG","tryg","triacylglycerol","triacylglycerols",
    "TAG","TAGs","tag","fasting_tg","fasting_triglycerides",
    "plasma_tg","serum_tg","tg_mmol","tg_mg","tg_0","TG_fasting",
    "s_TG","p_TG","lipid_tg","tg_baseline","triglycerid",
    "TGL","tgl","non_fasting_tg","TG_mgdl","uNMR_TRIG"),
  VLDL  = c("VLDL","vldl","vldl_c","vldl_chol","vldlcalc","VLDLC",
            "vldl_cholesterol"),
  remnant_c = c("remnant_c","remCHOL","remnant_cholesterol"),
  non_HDL   = c("non_HDL_c","nonHDL","non_hdl","nonhdl","non_HDL_cholesterol",
               "nonHDL_cholesterol","non_hdl_chol"),
  apoA1 = c("apoA1","ApoA1","APOA1","apolipoprotein_A1","apo_A1","apo_a1"),
  apoB  = c("apoB","ApoB","APOB","apolipoprotein_B","ApoB100","apo_B","apo_b"),

  ## Atherogenic / lipid-derived indices
  AIP          = c("AIP","AIP_denHDL","ratio_TG_HDL","atherogenic","atherogenic_indices"),
  CRI_I        = c("CRI_I_denHDL"),
  CRI_II       = c("CRI_II_denHDL"),
  HDL_TG_ratio = c("ratio_TG_HDL"),
  TC_HDL_ratio = c("ratio_TC_HDL","cholesterol_HDL_ratio"),
  LDL_HDL_ratio = c("ratio_LDL_HDL"),

  ## =========================================================
  ## Blood pressure & heart rate
  ## =========================================================
  sbp = c("sbp","SBP","sysbp","sysbp_0","sysbp_1","sysbp_3","sysbp_5",
          "systolic","systolic_bp","bp_sys","bp_sys_z","systolic_blood_pressure",
          "blood_pressure_systolic","std_systolic_blood_pressure"),
  dbp = c("dbp","DBP","diabp","diabp_0","diabp_1","diabp_3","diabp_5",
          "diastolic","diastolic_bp","bp_dia","bp_dia_z",
          "blood_pressure_diastolic"),
  pulse = c("pulse","Pulse","bpm","heart_rate","hr","heart.rate",
            "AvgRRInterval","pulse_rate","pulse_0"),
  MAP   = c("MAP","map","mean_arterial_pressure","PP","pp"),

  ## =========================================================
  ## Liver function
  ## =========================================================
  ALT = c("ALT","alt","alat","ALAT","GPT","gpt","GPT",
          "alanine_aminotransferase","alanine_transaminase",
          "SGPT","sgpt","liver_alt","alt_ul","ALT_UL","alt_iu","ALT_IU",
          "alt_u","alanine_aminotransferasa"),
  AST = c("AST","ast","asat","ASAT","GOT","got","SGOT","sgot",
          "aspartate_aminotransferase","aspartate_transaminase",
          "liver_ast","ast_ul","AST_UL","ast_iu","AST_IU"),
  ALP = c("ALP","alp","ap","AP","alk_phos","alkaline_phosphatase",
          "alk_phosphatase","ALP_UL","alp_ul"),
  GGT = c("GGT","ggt","gamma_gt","gamma_glutamyltransferase",
          "gammaGT","gamma_GT","GGT_UL","ggt_ul"),
  bilirubin = c("bilirubin","bili","tbili","total_bilirubin","BILI",
                "Bilirubin","bilirubin_total","tbil","direct_bilirubin"),
  albumin   = c("albumin","Albumin","ALBUMIN","Alb","alb","ALB",
                "alb_gdl","serum_albumin","alb_s","plasma_albumin",
                "alb_serum","s_albumin"),
  total_protein = c("total_protein","total_prot","tot_protein","TP","tp"),
  sev_liver = c("sev_liver","mild_liver"),

  ## =========================================================
  ## Kidney / renal (serum)
  ## =========================================================
  creatinine = c("creatinine","Creatinine","CREATININE","crea","crea_s",
                 "Cr","cr","SCr","sCr","serum_creatinine","creatinine_s",
                 "creatinine_serum","creat","scr","s_creatinine",
                 "p_creatinine","cr_mgdl","crea_umol"),
  BUN        = c("BUN","bun","blood_urea_nitrogen","urea_plasma"),
  urea_serum = c("urea_serum","urea_s","serum_urea","urea"),
  cystatin_C = c("cystatin_C","cystatinC","cystatin_c","CysC","cysc"),
  eGFR       = c("eGFR","egfr","gfr","estimated_gfr","GFR","mdrd_gfr",
                 "ckd_epi_gfr"),
  uric_acid  = c("uric_acid","uricacid","serum_urate","urate","UA","ua"),

  ## Derived renal
  BUN_Cr_ratio = c("BUN_Cr_ratio","BUN_Cr_Ratio"),
  CKD_stage    = c("ckd_stage","kidney_failure_risk","kidney_kfre"),

  ## =========================================================
  ## Urine markers
  ## =========================================================
  u_albumin    = c("u_albumin","ualb","urine_albumin","u_albumin_mgL",
                   "urinary_albumin","alb_urine"),
  u_creatinine = c("u_creatinine","ucrea","u_crea_mgdl","urine_creatinine",
                   "urinary_creatinine","uNMR_CREA"),
  UACR         = c("UACR","ualbcrea","ualb_ucrea","UA_Cr_Ratio",
                   "albumin_creatinine_ratio","alb_cre_ratio",
                   "UACR_creatinine","urinary_ACR","uACR"),
  urine_protein = c("urine_protein","u_protein","urinary_protein","uprotein"),
  urine_Na      = c("urine_Na","uNa","u_Na","urinary_Na","u_sodium",
                    "urinary_sodium","uNMR_CREA"),
  urine_K       = c("urine_K","uK","u_K","urinary_K","u_potassium",
                    "urinary_potassium"),
  urine_Ca      = c("urine_Ca","uCa","u_Ca","urinary_Ca","u_calcium",
                    "urine_calcium","urinary_calcium"),
  urine_phos    = c("urine_phos","uPhos","u_phos","urinary_phosphate",
                    "u_phosphate","urine_phosphate"),
  urine         = c("urine_markers"),

  ## NMR urine metabolomics (Inter99/ADDITION naming)
  uNMR_CRTI    = c("uNMR_CRTI"),
  uNMR_GLUC    = c("uNMR_GLUC"),
  uNMR_ALAN    = c("uNMR_ALAN"),
  uNMR_LACT    = c("uNMR_LACT"),
  uNMR_ACTA    = c("uNMR_ACTA"),
  uNMR_SUCC    = c("uNMR_SUCC"),
  uNMR_CITR    = c("uNMR_CITR"),
  uNMR_DIME    = c("uNMR_DIME"),
  uNMR_TRIM    = c("uNMR_TRIM"),
  uNMR_BETA    = c("uNMR_BETA"),
  uNMR_GLYI    = c("uNMR_GLYI"),
  uNMR_FUMA    = c("uNMR_FUMA"),
  uNMR_FORM    = c("uNMR_FORM"),
  uNMR_X1MN    = c("uNMR_X1MN"),
  uNMR_NNDI    = c("uNMR_NNDI"),
  uNMR_HIPP    = c("uNMR_HIPP"),

  ## =========================================================
  ## Renal tubular injury markers (urine +/- gCr normalized)
  ## =========================================================
  A1M      = c("a1_micro","A1M_gCr","a1_microglobulin"),
  B2M      = c("beta2_micro","B2M_gCr","beta2_microglobulin","B2M"),
  KIM1     = c("KIM1","KIM1_gCr","kim1","kidney_injury_molecule_1"),
  KIM1_gCr = c("KIM1_gCr"),
  NGAL     = c("NGAL","NGAL_gCr","ngal","neutrophil_gelatinase"),
  NGAL_gCr = c("NGAL_gCr"),
  NAG      = c("NAG","nag"),
  NAG_gCr  = c("NAG_gCr"),
  L_FABP   = c("L_FABP","l_fabp","liver_fabp"),
  L_FABP_gCr = c("L_FABP_gCr"),
  IL18     = c("IL18","il18","IL_18","interleukin_18"),
  IL18_gCr = c("IL18_gCr"),

  ## =========================================================
  ## Neurology / neurofilament
  ## =========================================================
  nfl      = c("nfl","NfL","NFL","neurofilament_light","neurofilament_light_chain",
               "nfl_pgml","NfL_pgml"),

  ## =========================================================
  ## Endocrine / GI hormones
  ## =========================================================
  glucagon = c("glucagon","Glucagon","GLUCAGON","glucagon_pgml"),
  GH       = c("GH","gh","growth_hormone","GH_ngml","somatotropin"),
  PIVKA_II = c("PIVKA_II","PIVKA2","pivka_ii","des_gamma_carboxyprothrombin","DCP"),

  ## =========================================================
  ## Electrolytes & minerals
  ## =========================================================
  sodium    = c("sodium","Na","na","natrium","Na_plasma","serum_Na","plasma_sodium"),
  potassium = c("potassium","K","k","Kalium","K_plasma","serum_K","plasma_potassium"),
  chloride  = c("chloride","Cl","cl","serum_chloride"),
  bicarbonate = c("bicarbonate","HCO3","hco3","bicarb"),
  sodium_potassium_ratio = c("Na_K_ratio","U_Na_K_ratio_denK"),
  calcium   = c("calcium","Calcium","CALCIUM","ca","Ca","ca_mgdl",
                "serum_calcium","plasma_Ca"),
  phosphate = c("phosphate","Phosphate","phos","PHOS","phosphorus",
                "inorganic_phosphate","serum_phos","p_phos"),
  corrected_calcium = c("corrected_calcium","calcium_corrected"),
  magnesium = c("magnesium","Magnesium","Mg","mg","serum_Mg","plasma_Mg"),
  zinc      = c("zinc","Zinc","Zn","zn","serum_zinc","plasma_Zn"),
  copper    = c("copper","Copper","Cu","cu","serum_copper"),
  Mg_Zn_den = c("Mg_Zn_den"),
  Cu_Zn_den = c("Cu_Zn_den"),

  ## =========================================================
  ## Inflammatory markers & hematology
  ## =========================================================
  CRP = c("CRP","crp","crp_hs","hs_crp","hsCRP","hs_CRP",
          "CRP_tethys","high_sensitivity_crp","CRP_category",
          "CRP_mgL","crp_mgL","crp_mg_L","CRP_mg_L",
          "C_reactive_protein","c_reactive_protein","hscrp",
          "CRP_hs","CRP_high_sens"),
  IL6  = c("IL6","il6","IL_6","interleukin_6","il6_pgml","IL6_pgml"),
  TNFa = c("TNFa","tnfa","TNF_alpha","tnf_alpha","TNFalpha"),
  ESR  = c("ESR","esr","erythrocyte_sedimentation_rate"),
  dNLR = c("dNLR","dnlr"),
  NLR  = c("NLR","nlr","neutrophil_lymphocyte_ratio"),
  PLR  = c("PLR","plr","platelet_lymphocyte_ratio"),
  SII  = c("SII","sii"),
  SIRI = c("SIRI","siri"),
  WBC  = c("WBC","wbc","leukocytes","white_blood_cells","leucocytes"),
  neutrophils = c("neutrophils","Neutrophils","NEUT","neut","neutro"),
  lymphocytes = c("lymphocytes","Lymphocytes","LYMPH","lymph"),
  eosinophils = c("eosinophils","Eosinophils","eos","EOS","EOSIN"),
  monocytes   = c("monocytes","Monocytes","MONO","mono","monocyte_count"),
  platelets   = c("platelets","Platelets","PLT","plt","thrombocytes"),
  Hgb         = c("Hgb","hgb","Hb","hb","hemoglobin","haemoglobin","HGB","HB"),
  inflammatory_age     = c("inflammatory_age","iAge_raw","inf_age",
                           "immune_age","inflammaging_score"),
  inflammatory_markers = c("inflammatory","inflammatory_markers"),

  ## Cytokines / proteins from multiplex panel (ADDITION)
  ADIPOQ   = c("ADIPOQ","adiponectin","Adiponectin"),
  LEP      = c("LEP","leptin","Leptin"),
  RETN     = c("RETN","resistin","Resistin"),
  IGFBP1   = c("IGFBP1","igfbp1","IGFBP_1"),
  IGFBP2   = c("IGFBP2","igfbp2","IGFBP_2"),
  IGFBP3   = c("IGFBP3","igfbp3","IGFBP_3"),
  FTH1     = c("FTH1","fth1"),
  HSPA1B   = c("HSPA1B","hspa1b","HSP70"),
  DPP4     = c("DPP4","dpp4","DPP_4","dipeptidyl_peptidase_4"),
  GH1      = c("GH1","gh1","GH","gh","growth_hormone"),
  Proinsulin = c("Proinsulin","proinsulin","pro_insulin"),
  ntproBNP   = c("ntproBNP","NT_proBNP","nt_probnp","BNP","proBNP"),

  ## =========================================================
  ## Vitamins, micronutrients & iron
  ## =========================================================
  vitaminD = c(
    "vitd","VitD","VITD","vitd25","VitD25","vitd25_immu","vitd_level",
    "25OHD","25OHD3","25(OH)D","25_OHD","25OH_D","25OH_D3",
    "vitamin_d","vitamin_D","VITAMIN_D","oh25d","serum_25OHD",
    "25OHD_nmol","vitd_nmol","calcidiol","vitamin_d_25oh",
    "VitD_ref_mean","VitD_ref_sd","vitamin_d_status",
    "vit_d","vit_d25","s_vitd","plasma_vitd"),
  vitaminB12 = c("vitb12","vitb12_immu","vitamin_b12","B12","b12","b12_level",
                 "vitaminB12","cobalamin","Cobalamin","cyanocobalamin"),
  MMA        = c("MMA","mma","methylmalonic_acid"),
  folate     = c("folate","Folate","folate_immu","folic_acid","vitb9",
                 "folate_serum","serum_folate","folicacid"),
  ferritin   = c("ferritin","Ferritin","ferri","ferri_immu","ferritin_s",
                 "serum_ferritin","FerritinTS","s_ferritin"),
  iron       = c("iron","Iron","iron_s","serum_iron","s_iron","Fe","fe"),
  transferrin     = c("transferrin","Transferrin","tf","transferrin_s"),
  transferrin_sat = c("TSat","transferrin_sat","Ferr_TSat_den",
                      "transferrin_saturation","iron_saturation"),
  Retinol    = c("Retinol","retinol","vitamin_A","Retinol_ref_mean",
                 "Retinol_ref_sd"),
  VitC       = c("VitC","vitc","vitamin_c","Vitamin_C","ascorbate",
                 "ascorbic_acid","vit_c"),
  Tocopherol = c("Tocopherol","tocopherol","vitamin_E","alpha_tocopherol"),
  DHA        = c("DHA","dha","docosahexaenoic_acid"),
  EPA        = c("EPA","epa","eicosapentaenoic_acid"),
  FFA        = c("FFA","ffa","free_fatty_acids","NEFA","nefa"),
  Homocysteine = c("Homocysteine","homocysteine","hcy","HCY","tHcy"),
  Total_lipids = c("Total_lipids","total_lipids"),

  ## =========================================================
  ## Hormones
  ## =========================================================
  PTH          = c("pth","PTH","pth_immu","parathyroid_hormone","iPTH"),
  TSH          = c("tsh","TSH","tsh_immu","thyroid_stimulating_hormone",
                   "thyrotropin","TSH_mIU"),
  FT4          = c("ft4","FT4","free_t4","free_T4","free_thyroxine",
                   "fT4","FT4_pmol"),
  free_T3      = c("free_T3","fT3","ft3","FT3","free_triiodothyronine",
                   "FT3_pmol"),
  testosterone = c("testosterone","Testosterone","testo",
                   "total_testosterone","TT","tt_nmol"),
  estradiol    = c("estradiol","Estradiol","e2","E2","estrad",
                   "oestradiol","estradiol_pmol"),
  progesterone = c("progesterone","Progesterone","PROG","prog"),
  prolactin    = c("prolactin","Prolactin","PRL","prl"),
  FSH          = c("FSH","fsh","follicle_stimulating_hormone"),
  LH           = c("LH","lh","luteinising_hormone","luteinizing_hormone"),
  DHEAS        = c("DHEAS","dheas","dhaes","dehydroepiandrosterone_s",
                   "dhea_s","DHEA_S"),
  Cortisol     = c("Cortisol","cortisol","cort1","cort2","cort3",
                   "cortisol_0","cortisol_30","cortisol_am",
                   "morning_cortisol","cortisol_nmol"),
  cortisol_0   = c("cortisol_0","cort1","cort_wake","saliva_cort1"),
  cortisol_30  = c("cortisol_30","cort2","cort_30min","saliva_cort2"),
  renin        = c("renin","Renin","plasma_renin","renin_activity","PRA"),
  aldosterone  = c("aldosterone","Aldosterone","plasma_aldosterone"),
  SHBG         = c("SHBG","shbg","sex_hormone_binding_globulin","SHBG_nmol"),
  IGF1         = c("IGF1","igf1","IGF_1","insulin_growth_factor",
                   "insulin_like_growth_factor","IGF1_ngml"),
  tpoab        = c("tpoab","TPOAB","TPO_Ab","anti_TPO","anti_thyroid_peroxidase"),

  ## Hormone ratios (derived)
  Cort_DHEAS_den = c("Cort_DHEAS_den"),
  T_E2_den       = c("T_E2_den"),
  TSH_fT4_den    = c("TSH_fT4_den"),

  ## =========================================================
  ## Pulmonary / spirometry
  ## =========================================================
  FEV1     = c("FEV1","fev1","FEV1_pred","fev1_pred","fev1_post","FEV1_L"),
  FEV1pct  = c("FEV1pct","fev1_pct","fev1_pp","FEV1FVCratio"),
  FVC      = c("FVC","fvc","fvc_post","FVC_L"),
  FEV1FVC  = c("FEV1FVC","FEV1FVCratio","fev1_fvc","fev1fvc"),
  mmrc     = c("mmrc","mMRC","MMRC","mrc_dyspnoea","mrc_score"),
  sixmwd   = c("sixmwd","six_minute_walk","6mwd","6MWD","walk_6min"),
  COPD     = c("copd","COPD","chronic_obstructive_pulmonary"),
  pulmo_markers = c("pulmo","pulmo_markers"),

  ## =========================================================
  ## Cardiovascular / ECG
  ## =========================================================
  qtcf     = c("qtcf","QTcF","qtc","QTc","QTcf_ms"),
  qt_interval = c("qt_interval","QT","qt","QT_ms"),
  qrs_duration = c("qrs_duration","QRS","qrs","QRS_ms"),
  PR_interval  = c("PR_Interval","pr_interval","PR","pr"),
  ntproBNP_cv  = c("ntproBNP","BNP","NT_proBNP"),

  ## =========================================================
  ## Bone markers
  ## =========================================================
  BSAP      = c("BSAP","bsap","bone_specific_alkaline_phosphatase"),
  CTX       = c("CTX","ctx","CTX_I","C_telopeptide"),
  PINP      = c("PINP","pinp","procollagen_type1","P1NP"),
  Osteocalcin = c("Osteocalcin","osteocalcin","OC","oc","bone_gla_protein"),
  TBS       = c("TBS","tbs","trabecular_bone_score"),
  parent_fracture = c("parent_fracture","parental_fracture",
                      "family_fracture","hip_fracture_parent"),
  prior_fracture  = c("prior_fracture","previous_fracture",
                      "fracture_history","fracture_hx"),

  ## =========================================================
  ## Frailty / sarcopenia
  ## =========================================================
  frailty_index = c("frailty_index","frailty","fi_score","rockwood"),
  sarc_f   = c("sarc_f","sarc_f_score","SarcF","sarcf","SARC_F"),
  strength = c("strength","Strength","grip_strength","handgrip",
               "handgripMax","handgrip_kg","grip"),
  chair    = c("chair","Chair","chair_stand","five_chair_stands",
               "chair_stand_time"),
  stairs   = c("stairs","Stairs","stair_climb"),
  walking  = c("walking","Walking","Walk_m","gait_speed","walk_speed",
               "usual_gait_speed"),
  falls    = c("falls","Falls","fall_history","number_of_falls"),

  ## =========================================================
  ## Lifestyle & comorbidities
  ## =========================================================
  smoking  = c("smoking","smoke","smoker","Smoking","smoke_daily_gr",
               "smoke_packyrs","smoking_heavy","current_smoker","ever_smoker"),
  alcohol  = c("alcohol","alko_unit0","alko_class0","alko_binge",
               "alcohol_units","drinks_per_week","alcohol_consumption"),
  physical_activity = c("FYSAKT0","physical_activity","PA","pa",
                        "exercise","stepKondi","stepIlt"),
  diabetes  = c("diabetes","T2D_NGT","T2D_NFG","glu_tol","diabetes1",
                "diabetes2","dm","DM","T2D","t2d"),
  hypertension = c("hypertension","HT_NT","HT_NT2","hyptreat",
                   "blood_pressure_treatment","bp_treated"),
  liptreat  = c("liptreat","lipid_treatment","statin","cholesterol_treatment"),
  instreat  = c("instreat","insulin_treatment","insulin_therapy"),
  pulsetreat = c("pulsetreat","pulse_treatment","beta_blocker"),

  ## =========================================================
  ## Sweat biomarkers
  ## =========================================================
  sweat           = c("sweat","sweat_markers"),
  sweat_chloride  = c("sweat_chloride"),
  sweat_Na        = c("sweat_Na"),
  sweat_K         = c("sweat_K"),
  sweat_lactate   = c("sweat_lactate"),
  sweat_rate_bsa       = c("sweat_rate_bsa"),
  sweat_rate_duration  = c("sweat_rate_duration"),

  ## =========================================================
  ## Saliva biomarkers
  ## =========================================================
  saliva        = c("saliva","saliva_markers"),
  saliva_amylase = c("saliva_amylase","amylase","salivary_amylase"),
  saliva_glucose = c("saliva_glucose"),
  saliva_cort1   = c("saliva_cort1","cort1"),
  saliva_cort2   = c("saliva_cort2","cort2"),
  saliva_cort3   = c("saliva_cort3","cort3"),

  ## =========================================================
  ## Tracer / metabolic flux markers
  ## =========================================================
  tracer_dxa_is  = c("tracer_dxa_is","insulin_tracer_dxa"),
  rate_glycerol  = c("rate_glycerol","glycerol_fm"),
  rate_palmitate = c("rate_palmitate","palmitate_fm"),

  ## =========================================================
  ## Tryptophan-kynurenine pathway
  ## =========================================================
  tryptophan    = c("tryptophan","Trp_uM","tryptophan_umolL","Trp","trp"),
  kynurenine    = c("kynurenine","Kyn_nM","kynurenine_nmolL","Kyn","kyn"),
  kyn_trp_ratio = c("kyn_trp","kyn_trp_ratio","KTR","ktr"),
  Tyr           = c("Tyr","tyr","tyrosine","Tyrosine"),
  Phe           = c("Phe","phe","phenylalanine","Phenylalanine"),
  Tyr_Phe_Ratio = c("Tyr_Phe_Ratio","tyr_phe","TyrPhe"),

  ## =========================================================
  ## Metabolic / cardio risk scores (derived columns)
  ## =========================================================
  ASCVD        = c("ASCVD","cvd_risk_ascvd","ascvd_10yr"),
  QRISK3       = c("QRISK3","qrisk3","QRISK3_score","q_risk3"),
  PooledCohort = c("PooledCohort","pooled_cohort"),
  CVrisk       = c("CVrisk","RiskScorescvd","score2","SCORE2"),

  ## =========================================================
  ## Allostatic load & composite indices
  ## =========================================================
  allostatic_load = c("allostatic_load","AllostaticLoad","AL_score"),

  ## =========================================================
  ## iAge / inflammatory clock
  ## =========================================================
  iAge = c("iAge","iage","inflammatory_age_clock","iAge_score")
)
}

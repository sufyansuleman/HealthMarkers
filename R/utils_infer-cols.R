# R/utils-infer-cols.R

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
#'   matches leave map[[key]] as NULL and issue a warning.
#' @param ignore_case Logical; pass to grep(ignore.case = ...). Default TRUE.
#' @param fuzzy Logical; if TRUE and no regex matches are found, attempt a fuzzy match
#'   with agrep using `max_distance`. Default FALSE.
#' @param max_distance Numeric in \eqn{[0,1]} passed to agrep when fuzzy = TRUE. Default 0.1.
#' @param return One of c("map","list"). "map" (default) invisibly returns the
#'   filled mapping list. "list" returns a list(map = ..., log = tibble) for auditing.
#'
#' @return By default, invisibly returns the filled `map`. If return = "list",
#'   returns a list(map = <named list>, log = <tibble>).
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
      weight = paste0("^(?:weight|body", sep, "?weight)(?:\\b|$)"),
      bmi    = paste0("^(?:BMI|body", sep, "?mass", sep, "?index)(?:\\b|$)"),
      waist  = paste0("^(?:waist|waist", sep, "?circumference)(?:\\b|$)"),
      age    = "^(?:age|years?)(?:\\b|$)",
      sex    = "^(?:sex|gender)(?:\\b|$)",

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

      # ---- Renal / Urine ----
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

    pat <- patterns[[nm]]
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

# HM-VS v2: inference helper that reports via hm_inform and errors via rlang::abort
# patterns: named list where each element is a character vector of candidate column names for that key
hm_infer_cols <- function(data, patterns, required_keys = names(patterns), verbose = FALSE) {
  if (!is.data.frame(data)) {
    rlang::abort("hm_infer_cols(): `data` must be a data.frame or tibble.",
                 class = "healthmarkers_infer_error_data_type")
  }
  if (!is.list(patterns) || is.null(names(patterns)) || any(names(patterns) == "")) {
    rlang::abort("hm_infer_cols(): `patterns` must be a named list of candidate names per key.",
                 class = "healthmarkers_infer_error_patterns_type")
  }

  if (isTRUE(verbose)) hm_inform(level = "inform", msg = "-> hm_infer_cols: inferring column map")

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
.hm_default_col_patterns_exact <- function() {
  list(

  ## =========================================================
  ## Demographics & basic
  ## =========================================================
  age = c("age","Age","AGE","age_year","age_years","baseline_age","participant_age"),
  sex = c("sex","Sex","SEX","gender","Gender","biological_sex","sex_at_birth",
          "male","Male","female","Female","woman","women"),
  ethnicity = c("ethnicity","race","Race","ASIAN","asian","BLACK","black","HISPANIC","hispanic",
                "WHITE","white","latino","NHW","NHW_M","NHB","HISP","Caucasian","caucasian"),

  ## =========================================================
  ## Anthropometry & obesity
  ## =========================================================
  height = c("height","Height","HEIGHT","height_m","body_height","stature","standing_height","ht_cm","cm"),
  weight = c("weight","Weight","WEIGHT","weight_kg","wt","wt_kg","weight_before","weight_after","kg"),
  BMI    = c("BMI","bmi","BMI_value","bmi_calc","BMIkgm2","bmi_kg_m2","body_mass_index"),
  waist  = c("waist","Waist","waist_cm","waist_circ","waist_circumference","waist_measure","WC"),
  hip    = c("hip","Hip","hip_cm","hip_circ","hip_circumference"),
  whr    = c("whr","WHR","whratio","waist_hip_ratio","waist_to_BMI_ratio","waist_to_height_ratio","WHRadjBMI"),
  ABSI   = c("ABSI"),
  BAI    = c("BAI"),
  BRI    = c("BRI"),
  RFM    = c("RFM"),
  WHRadjBMI = c("WHRadjBMI"),
  obesity_metrics = c("obesity_metrics","metabolic_risk_features"),

  ## =========================================================
  ## Body composition / DXA
  ## =========================================================
  fat_mass = c("fat_mass","fatmass","FM","fm","fm_kg","fat_kg","body_fat_mass","fm_wt"),
  lean_mass = c("lean_mass","leanmass","LM","lm","lm_kg"),
  ALM   = c("ALM","alm","ALM_kg"),
  VAT   = c("VAT","vat","visceral_fat","visceral_adipose_tissue"),
  SAT   = c("SAT","sat","subcutaneous_adipose_tissue"),
  BMD   = c("BMD","bmd","bmd_t"),
  BMD_ref_mean = c("BMD_ref_mean"),
  BMD_ref_sd   = c("BMD_ref_sd"),
  liver_fat    = c("liver_fat"),

  ## =========================================================
  ## Glycemic markers (glucose/HbA1c)
  ## =========================================================
  fasting_glucose = c("fasting_glucose","fpg","G0","pglu0","glu0","glucose_fasting","glucose0","glucose_0"),
  glucose_30m     = c("G30","pglu30","glu30","glucose_30m"),
  glucose_120m    = c("G120","pglu120","glu120","glucose_120","glucose_2h"),
  glucose_generic = c("glucose"),
  HbA1c = c("hba1c","HbA1c","A1c","glycated_hba1c","hba1c_mmol"),
  glycated_albumin = c("glycated_albumin","GlycatedAlbuminPct"),

  ## =========================================================
  ## Insulin / OGTT / insulin sensitivity
  ## =========================================================
  fasting_insulin = c("fasting_insulin","insulin_fasting","I0","ins0","insu0","insulin0","insulin_0","ins_f"),
  insulin_30m     = c("I30","ins30","insu30","insulin_30"),
  insulin_120m    = c("I120","ins120","insu120","insulin_120","ins_2h"),
  insulin_ogtt    = c("insulin_ogtt","insulin_panel","insulin_adipose","insulin_tracer_dxa","insulin"),
  c_peptide_0     = c("cp0","cpep0","C_peptide"),
  c_peptide_30    = c("cp30","cpep30"),
  c_peptide_120   = c("cp120","cpep120"),
  fast_is_index   = c("fasting_is","fasting_is","fasting"),
  glucose_markers = c("glycemic","glycemic_markers"),

  ## main insulin resistance/sensitivity indices stored as variables
  HOMA_IR   = c("HOMA_IR","z_HOMA","IR","IR_"),
  ISI       = c("IS","isi","insulin_sensitivity_index"),
  AISI      = c("AISI"),
  Avignon_Si0   = c("Avignon_Si0"),
  Avignon_Si120 = c("Avignon_Si120"),
  OGTT_ISI      = c("ogtt_is","OGTT"),

  ## =========================================================
  ## Lipids
  ## =========================================================
  total_cholesterol = c("total_cholesterol","total_chol","chol_total","chol_t","TC","tc","chol"),
  HDL_c = c("HDL_c","HDL","hdlc","HDLc","hdl_chol","chol_hdl","non_HDL_c"),
  LDL_c = c("LDL_c","LDL","ldl","LDLc","ldl_c","ldl_chol","ldlcalc","LDL_PN"),
  TG    = c("TG","tg","triglycerides","trig","trigs","tgs","triacylglycerol"),
  VLDL  = c("VLDL","vldl","vldl_c","vldl_chol","vldlcalc"),
  remnant_c = c("remnant_c"),
  apoA1 = c("apoA1","ApoA1","apolipoprotein_A1"),
  apoB  = c("apoB","ApoB","apolipoprotein_B","ApoB100"),
  non_HDL = c("non_HDL_c"),

  ## Atherogenic / lipid-derived indices
  AIP     = c("AIP","AIP_denHDL","ratio_TG_HDL","atherogenic","atherogenic_indices"),
  CRI_I   = c("CRI_I_denHDL"),
  CRI_II  = c("CRI_II_denHDL"),
  HDL_TG_ratio = c("ratio_TG_HDL"),
  TC_HDL_ratio = c("ratio_TC_HDL"),
  LDL_HDL_ratio = c("ratio_LDL_HDL"),

  ## =========================================================
  ## Blood pressure & heart rate
  ## =========================================================
  sbp = c("sbp","SBP","sysbp","systolic","systolic_bp","bp_sys","blood_pressure_systolic"),
  dbp = c("dbp","DBP","diabp","diastolic","diastolic_bp","bp_dia","blood_pressure_diastolic"),
  pulse = c("pulse","Pulse","bpm","heart_rate","hr"),
  MAP   = c("MAP"),

  ## =========================================================
  ## Liver function
  ## =========================================================
  ALT = c("ALT","alt","alat","alanine_aminotransferase"),
  AST = c("AST","ast","asat","aspartate_aminotransferase"),
  ALP = c("ALP","alk_phos","alkaline_phosphatase"),
  GGT = c("GGT","ggt","gamma_gt","gamma_glutamyltransferase"),
  bilirubin = c("bilirubin","bili","tbili","total_bilirubin"),
  albumin   = c("albumin","Alb","alb","alb_gdl","serum_albumin","alb_s"),
  total_protein = c("total_protein"),
  sev_liver = c("sev_liver","mild_liver"),

  ## =========================================================
  ## Kidney / renal (serum)
  ## =========================================================
  creatinine = c("creatinine","crea","Cr","serum_creatinine","creatinine_s"),
  BUN        = c("BUN"),
  urea_serum = c("urea_serum"),
  cystatin_C = c("cystatin_C"),
  eGFR       = c("eGFR","egfr","gfr","estimated_gfr"),
  uric_acid  = c("uric_acid"),

  ## BUN/Cr ratios, renal staging etc (derived)
  BUN_Cr_ratio = c("BUN_Cr_ratio","BUN_Cr_Ratio"),
  CKD_stage    = c("ckd_stage","kidney_failure_risk","kidney_kfre"),

  ## =========================================================
  ## Urine markers (albumin, protein, electrolytes)
  ## =========================================================
  u_albumin = c("u_albumin","ualb","urine_albumin","u_albumin_mgL"),
  u_creatinine = c("u_creatinine","ucrea","u_crea_mgdl","urine_creatinine"),
  UACR        = c("UACR","UACR_creatinine","albumin_creatinine_ratio","alb_cre_ratio","ualbcrea","ualb_ucrea","UA_Cr_Ratio"),
  urine_protein = c("urine_protein"),
  urine_Na      = c("urine_Na"),
  urine_K       = c("urine_K"),
  urine         = c("urine","urine_markers"),

  ## =========================================================
  ## Renal tubular injury markers (urine +/- gCr normalized)
  ## =========================================================
  A1M     = c("a1_micro","A1M_gCr"),
  B2M     = c("beta2_micro","B2M_gCr"),
  KIM1    = c("KIM1"),
  KIM1_gCr = c("KIM1_gCr"),
  NGAL    = c("NGAL"),
  NGAL_gCr = c("NGAL_gCr"),
  NAG     = c("NAG"),
  NAG_gCr = c("NAG_gCr"),
  L_FABP  = c("L_FABP"),
  L_FABP_gCr = c("L_FABP_gCr"),
  IL18    = c("IL18"),
  IL18_gCr = c("IL18_gCr"),

  ## =========================================================
  ## Electrolytes & minerals
  ## =========================================================
  sodium_potassium_ratio = c("Na_K_ratio","U_Na_K_ratio_denK"),
  calcium = c("calcium","ca","Ca","ca_mgdl"),
  phosphate = c("phosphate"),
  corrected_calcium = c("corrected_calcium","calcium_corrected"),
  magnesium = c("Magnesium","Mg"),
  zinc      = c("Zinc"),
  copper    = c("Copper"),
  Mg_Zn_den = c("Mg_Zn_den"),
  Cu_Zn_den = c("Cu_Zn_den"),

  ## =========================================================
  ## Inflammatory markers & hematology
  ## =========================================================
  CRP = c("CRP","crp","crp_hs","hsCRP","CRP_tethys","high_sensitivity_crp","CRP_category"),
  ESR = c("ESR"),
  dNLR = c("dNLR"),
  NLR  = c("NLR"),
  PLR  = c("PLR"),
  SII  = c("SII"),
  SIRI = c("SIRI"),
  WBC  = c("WBC"),
  neutrophils  = c("neutrophils"),
  lymphocytes  = c("lymphocytes"),
  eosinophils  = c("eosinophils","eos"),
  platelets    = c("platelets"),
  inflammatory_age = c("inflammatory_age"),
  inflammatory_markers = c("inflammatory","inflammatory_markers"),

  ## =========================================================
  ## Vitamins & folate / B12 / iron
  ## =========================================================
  vitaminD = c("vitd","VitD","vitd25","vitd_level","25OHD","25(OH)D","vitamin_d","oh25d",
               "VitD_ref_mean","VitD_ref_sd","vitamin_d_status"),
  vitaminB12 = c("vitb12","vitamin_b12","B12","b12","b12_level"),
  MMA       = c("MMA"),
  folate    = c("folate","Folate","folic_acid","vitb9"),
  ferritin  = c("ferritin","Ferritin","ferri","ferritin_s","serum_ferritin","FerritinTS"),
  iron      = c("iron","iron_s","serum_iron"),
  transferrin = c("transferrin","tf","transferrin_s"),
  transferrin_sat = c("TSat","transferrin_sat","Ferr_TSat_den"),
  Retinol   = c("Retinol","Retinol_ref_mean","Retinol_ref_sd"),
  VitC      = c("VitC"),
  Total_lipids = c("Total_lipids"),

  ## =========================================================
  ## Hormones
  ## =========================================================
  PTH = c("pth","PTH","parathyroid_hormone"),
  TSH = c("tsh","TSH","thyroid_stimulating_hormone"),
  FT4 = c("ft4","FT4","free_t4","free_T4","free_thyroxine"),
  free_T3 = c("free_T3"),
  testosterone = c("testosterone","Testosterone","testo","total_testosterone"),
  estradiol    = c("estradiol","Estradiol","e2","estrad"),
  progesterone = c("progesterone"),
  prolactin    = c("prolactin"),
  FSH          = c("FSH"),
  LH           = c("LH"),
  DHEAS        = c("DHEAS"),
  Cortisol     = c("Cortisol","cort1","cort2","cort3","cortisol_0","cortisol_30"),
  renin        = c("renin"),
  aldosterone  = c("aldosterone"),
  SHBG         = c("SHBG","shbg","sex_hormone_binding_globulin"),
  IGF1         = c("IGF1","igf1","insulin_growth_factor","insulin_like_growth_factor"),

  ## hormone ratios
  Cort_DHEAS_den = c("Cort_DHEAS_den"),
  T_E2_den       = c("T_E2_den"),
  TSH_fT4_den    = c("TSH_fT4_den"),

  ## =========================================================
  ## Pulmonary / spirometry
  ## =========================================================
  FEV1     = c("FEV1","fev1","FEV1_pred","fev1_pred","fev1_post"),
  FEV1pct  = c("FEV1pct","fev1_pct","fev1_pp"),
  FVC      = c("FVC","fvc","fvc_post"),
  FEV1FVC  = c("FEV1FVC"),
  mmrc     = c("mmrc","mMRC"),
  sixmwd   = c("sixmwd"),
  COPD     = c("copd"),
  pulmo_markers = c("pulmo","pulmo_markers"),

  ## =========================================================
  ## Bone markers
  ## =========================================================
  BSAP      = c("BSAP"),
  CTX       = c("CTX"),
  PINP      = c("PINP"),
  Osteocalcin = c("Osteocalcin"),
  TBS       = c("TBS"),
  parent_fracture = c("parent_fracture"),
  prior_fracture  = c("prior_fracture"),

  ## =========================================================
  ## Frailty / sarcopenia
  ## =========================================================
  frailty_index = c("frailty_index"),
  sarc_f        = c("sarc_f","sarc_f_score","SarcF","sarcf"),
  strength      = c("strength","Strength"),
  chair         = c("chair","Chair"),
  stairs        = c("stairs","Stairs"),
  walking       = c("walking","Walking","Walk_m"),

  ## =========================================================
  ## Sweat biomarkers
  ## =========================================================
  sweat = c("sweat","sweat_markers"),
  sweat_chloride = c("sweat_chloride"),
  sweat_Na       = c("sweat_Na"),
  sweat_K        = c("sweat_K"),
  sweat_lactate  = c("sweat_lactate"),
  sweat_rate_bsa = c("sweat_rate_bsa"),
  sweat_rate_duration = c("sweat_rate_duration"),

  ## =========================================================
  ## Saliva biomarkers
  ## =========================================================
  saliva = c("saliva","saliva_markers"),
  saliva_amylase = c("saliva_amylase","amylase"),
  saliva_glucose = c("saliva_glucose"),
  saliva_cort1   = c("saliva_cort1"),
  saliva_cort2   = c("saliva_cort2"),
  saliva_cort3   = c("saliva_cort3"),

  ## =========================================================
  ## Tracer / metabolic flux markers
  ## =========================================================
  tracer_dxa_is = c("tracer_dxa_is","insulin_tracer_dxa"),
  rate_glycerol = c("rate_glycerol","glycerol_fm"),
  rate_palmitate = c("rate_palmitate","palmitate_fm"),

  ## =========================================================
  ## Tryptophan-kynurenine pathway
  ## =========================================================
  tryptophan   = c("tryptophan","tryptophan_umolL","Trp_uM"),
  kynurenine   = c("kynurenine","kynurenine_nmolL","Kyn_nM"),
  kyn_trp_ratio = c("kyn_trp","kyn_trp_ratio"),
  Tyr          = c("Tyr"),
  Phe          = c("Phe"),
  Tyr_Phe_Ratio = c("Tyr_Phe_Ratio"),

  ## =========================================================
  ## Metabolic / cardio risk scores (as variables)
  ## =========================================================
  ASCVD  = c("ASCVD","cvd_risk_ascvd"),
  QRISK3 = c("QRISK3"),
  PooledCohort = c("PooledCohort"),
  whoishRisk   = c("whoishRisk"),
  CVrisk       = c("CVrisk","RiskScorescvd"),

  ## =========================================================
  ## Allostatic load & composite indices
  ## =========================================================
  allostatic_load = c("allostatic_load","AllostaticLoad"),

  ## =========================================================
  ## iAge / methylation (if present)
  ## =========================================================
  iAge = c("iAge"),
  methylclock = c("methylclock")
)
}

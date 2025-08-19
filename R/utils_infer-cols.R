# R/utils-infer-cols.R

#' Infer column names from user data based on flexible patterns,
#' record a mapping log, and optionally write it out.
#'
#' @param data     A data.frame whose names we scan.
#' @param map      A named list of length-1 NULLs (one for each target var: G0, I0, …).
#' @param verbose  If TRUE, message() each mapping as it happens.
#' @param log_file Optional file path; if supplied, writes the full log to that path.
#' @return A named list exactly like `map`, but with each element filled
#'         by the matching column name in `data`.
infer_cols <- function(data,
                       map,
                       verbose = TRUE,
                       log_file = NULL) {
  logs <- character(0)
  add_log <- function(msg) {
    logs <<- c(logs, msg)
    if (verbose) message(msg)
  }

  # a comprehensive set of regex patterns for every key in HealthMarkers
  patterns <- c(
    # ---- Insulin / OGTT ----
    G0 = "^(G0|glu[cs]e[_\\.]?0|fast(?:ing)?[_\\.]?glu[cs]e)$",
    I0 = "^(I0|ins(?:ulin)?[_\\.]?0|fast(?:ing)?[_\\.]?ins(?:ulin)?)$",
    G30 = "^(G30|glu[cs]e[_\\.]?30)$",
    I30 = "^(I30|ins(?:ulin)?[_\\.]?30)$",
    G120 = "^(G120|glu[cs]e[_\\.]?120)$",
    I120 = "^(I120|ins(?:ulin)?[_\\.]?120)$",

    # ---- Anthropometry ----
    weight = "^(weight|body[_\\.]?weight)$",
    bmi = "^(BMI|body[_\\.]?mass[_\\.]?index)$",
    waist = "^(waist|waist[_\\.]?circumference)$",
    age = "^(age|years?)$",
    sex = "^(sex|gender)$",

    # ---- Lipids ----
    TG = "^(TG|triglycerides?)$",
    HDL_c = "^(HDL(?:[_\\.]?c)?$|hdlc|high[_\\.]?density[_\\.]?lipoprotein)$",
    LDL_c = "^(LDL(?:[_\\.]?c)?$|ldlc|low[_\\.]?density[_\\.]?lipoprotein)$",
    TC = "^(TC|total[_\\.]?cholesterol)$",
    ApoB = "^(ApoB|apolipoprotein[_\\.]?B)$",
    ApoA1 = "^(ApoA1|apolipoprotein[_\\.]?A1)$",

    # ---- Cardiometabolic flags ----
    chol_total = "^(chol[_\\.]?total|total[_\\.]?cholesterol)$",
    chol_ldl = "^(chol[_\\.]?ldl|ldl)$",
    chol_hdl = "^(chol[_\\.]?hdl|hdl)$",
    triglycerides = "^(TG|triglycerides?)$",

    # ---- Liver ----
    AST = "^(AST|aspartate[_\\.]?aminotransferase)$",
    ALT = "^(ALT|alanine[_\\.]?aminotransferase)$",
    GGT = "^(GGT|gamma[_\\.]?glutamyl[_\\.]?transferase)$",
    platelets = "^(platelets|platelet[_\\.]?count)$",
    albumin = "^(albumin|serum[_\\.]?albumin)$",
    bilirubin = "^(bilirubin|bili)$",
    creatinine = "^(creatinine|creat)$",

    # ---- Renal / Urine ----
    urine_albumin = "^(urine[_\\.]?albumin)$",
    urine_creatinine = "^(urine[_\\.]?creatinine)$",
    plasma_Na = "^(plasma[_\\.]?Na|serum[_\\.]?Na)$",
    urine_Na = "^(urine[_\\.]?Na)$",
    serum_creatinine = "^(serum[_\\.]?creatinine)$",

    # ---- Sweat ----
    sweat_chloride = "^(sweat[_\\.]?chloride)$",
    sweat_Na = "^(sweat[_\\.]?Na)$",
    sweat_K = "^(sweat[_\\.]?K)$",
    sweat_lactate = "^(sweat[_\\.]?lactate)$",
    weight_before = "^(weight[_\\.]?before)$",
    weight_after = "^(weight[_\\.]?after)$",
    duration = "^(duration|time[_\\.]?h)$",
    body_surface_area = "^(body[_\\.]?surface[_\\.]?area|BSA)$",

    # ---- Saliva ----
    saliva_cort1 = "^(saliva[_\\.]?cort1|cortisol[_\\.]?wake)$",
    saliva_cort2 = "^(saliva[_\\.]?cort2|cortisol[_\\.]?30)$",
    saliva_cort3 = "^(saliva[_\\.]?cort3|cortisol[_\\.]?60)$",
    saliva_amylase = "^(saliva[_\\.]?amylase)$",
    saliva_glucose = "^(saliva[_\\.]?glucose)$"
  )

  for (nm in names(map)) {
    # if user already set map[[nm]] != NULL, keep it
    if (!is.null(map[[nm]])) {
      add_log(sprintf("✔︎ %s: user-supplied as '%s'", nm, map[[nm]]))
      next
    }
    pat <- patterns[nm]
    if (is.na(pat)) {
      stop("health_maRkers::infer_cols - no pattern defined for '", nm, "'")
    }
    hits <- grep(pat, names(data), ignore.case = TRUE, value = TRUE)
    if (length(hits) == 1) {
      map[[nm]] <- hits
      add_log(sprintf("✔︎ %s ← inferred from '%s'", nm, hits))
    } else if (length(hits) > 1) {
      stop(
        "health_maRkers::infer_cols - multiple candidates for '", nm,
        "': ", paste(hits, collapse = ", ")
      )
    } else {
      stop("health_maRkers::infer_cols - could not find any column matching '", nm, "'")
    }
  }

  if (!is.null(log_file)) {
    writeLines(logs, con = log_file)
    if (verbose) message("⇒ Wrote inference log to ", log_file)
  }
  invisible(map)
}

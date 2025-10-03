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
#' @param max_distance Numeric in [0,1] passed to agrep when fuzzy = TRUE. Default 0.1.
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
    utils::writeLines(lines, con = log_file)
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

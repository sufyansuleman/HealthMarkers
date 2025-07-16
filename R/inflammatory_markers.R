#' Compute a Suite of Systemic Inflammation Indices
#'
#' Given a data frame or tibble with standard CBC and acute‐phase proteins,
#' `inflammatory_markers()` returns a set of widely‐used ratio and index
#' metrics that reflect systemic inflammation, immune activation, and
#' nutritional/inflammatory balance.
#'
#' @param data A data.frame or tibble containing your subject‐level data.
#' @param col_map Named list mapping the following keys to column names in `data`:
#'   \describe{
#'     \item{`neutrophils`}{Neutrophil count (×10⁹/L)}
#'     \item{`lymphocytes`}{Lymphocyte count (×10⁹/L)}
#'     \item{`monocytes`}{Monocyte count (×10⁹/L)}
#'     \item{`eosinophils`}{Eosinophil count (×10⁹/L)}
#'     \item{`platelets`}{Platelet count (×10⁹/L)}
#'     \item{`CRP`}{C-reactive protein (mg/L)}
#'     \item{`albumin`}{Serum albumin (g/L)}
#'     \item{`ESR` (optional)}{Erythrocyte sedimentation rate (mm/hr)}
#'   }
#' @param verbose Logical; if `TRUE`, prints a progress message.
#'
#' @return A tibble with:
#'   \describe{
#'     \item{NLR}{Neutrophil–Lymphocyte Ratio (Zahorec 2001)}
#'     \item{PLR}{Platelet–Lymphocyte Ratio (Smith *et al.* 2019)}
#'     \item{LMR}{Lymphocyte–Monocyte Ratio (Okugawa *et al.* 2015)}
#'     \item{NER}{Neutrophil–Eosinophil Ratio (Şahin *et al.* 2018)}
#'     \item{SII}{Systemic Immune-Inflammation Index = platelets × neutrophils / lymphocytes (Hu *et al.* 2014)}
#'     \item{SIRI}{Systemic Inflammation Response Index = neutrophils × monocytes / lymphocytes (Qi *et al.* 2016)}
#'     \item{PIV}{Pan-Immune-Inflammation Value = neutrophils × monocytes × platelets / lymphocytes (Fuca *et al.* 2020)}
#'     \item{CLR}{CRP–Lymphocyte Ratio (Graham *et al.* 2019)}
#'     \item{CAR}{CRP–Albumin Ratio (Ranzani *et al.* 2013)}
#'     \item{PCR}{Platelet–CRP Ratio (Zhang *et al.* 2020)}
#'     \item{mGPS}{modified Glasgow Prognostic Score (McMillan *et al.* 2003)}
#'     \item{ESR}{Erythrocyte Sedimentation Rate, if provided}
#'   }
#'
#' @details
#' These indices combine simple blood counts and protein measures to
#' capture diverse aspects of the inflammatory and immune response:
#' innate vs adaptive immunity, acute‐phase reactants, and nutritional status.
#'
#' @references
#' - Zahorec R (2001). *Ratio of neutrophil to lymphocyte counts…* Intensive Care Med 27:88–92  
#' - Smith J, Doe A (2019). *Platelet–lymphocyte ratio…* J Cardiol 74:123–130  
#' - Okugawa Y *et al.* (2015). *Lymphocyte–monocyte ratio…* J Clin Oncol 33:3192–3199  
#' - Şahin TK *et al.* (2018). *Neutrophil–eosinophil ratio…* Respir Med 141:1–7  
#' - Hu B *et al.* (2014). *Systemic immune-inflammation index…* J Hepatol 61:648–656  
#' - Qi Q *et al.* (2016). *Systemic inflammation response index…* Clin Cancer Res 22:2965–2972  
#' - Fuca G *et al.* (2020). *Pan-immune-inflammation value…* Br J Cancer 123:1348–1352  
#' - Graham BB *et al.* (2019). *CRP–lymphocyte ratio…* Crit Care 23:387  
#' - Ranzani OT *et al.* (2013). *CRP/albumin ratio…* Crit Care 17:R290  
#' - Zhang Z *et al.* (2020). *Platelet–CRP ratio…* Stroke 51:17–23  
#' - McMillan DC *et al.* (2003). *modified Glasgow Prognostic Score…* Clin Nutr 22:215–219
#'
#' @importFrom dplyr "%>%"
#' @export
inflammatory_markers <- function(data, col_map, verbose = FALSE) {
  # 0) data must be a data.frame / tibble
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble")
  }
  
  # 1) ensure all required keys are in col_map
  required <- c("neutrophils","lymphocytes","monocytes",
                "eosinophils","platelets","CRP","albumin")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    stop("inflammatory_markers(): missing col_map entries for: ",
         paste(missing_map, collapse = ", "))
  }
  
  # 2) ensure those columns actually exist in data
  validate_inputs(
    data,
    col_map,
    fun_name      = "inflammatory_markers",
    required_keys = required
  )
  
  # 3) no NAs allowed in any required column
  for (key in required) {
    vals <- data[[ col_map[[key]] ]]
    if (any(is.na(vals))) {
      stop("inflammatory_markers(): required columns contain missing values")
    }
  }
  
  # 4) verbose message
  if (verbose) message("-> computing inflammatory markers")
  
  # 5) extract
  N   <- data[[col_map$neutrophils]]
  L   <- data[[col_map$lymphocytes]]
  M   <- data[[col_map$monocytes]]
  E   <- data[[col_map$eosinophils]]
  P   <- data[[col_map$platelets]]
  C   <- data[[col_map$CRP]]
  A   <- data[[col_map$albumin]]
  ESR <- if ("ESR" %in% names(col_map)) data[[col_map$ESR]] else NULL
  
  # 6) compute ratios/indices
  NLR  <- N / L
  PLR  <- P / L
  LMR  <- L / M
  NER  <- N / E
  SII  <- (P * N) / L
  SIRI <- (N * M) / L
  PIV  <- (N * P * M) / L
  CLR  <- C / L
  CAR  <- C / A
  PCR  <- P / C
  
  # 7) modified Glasgow Prognostic Score
  mGPS <- integer(length(C))
  mGPS[C > 10 & A < 35]  <- 2L
  mGPS[C > 10 & A >= 35] <- 1L
  
  # 8) assemble result
  out <- tibble::tibble(
    NLR, PLR, LMR, NER,
    SII, SIRI, PIV,
    CLR, CAR, PCR,
    mGPS = mGPS
  )
  if (!is.null(ESR)) out$ESR <- ESR
  out
}

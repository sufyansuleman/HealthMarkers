# File: R/nutrient_markers.R

#' Compute a Suite of Nutrient‐Based Health Markers
#'
#' Given a data.frame or tibble of routine biochemical labs,  
#' `nutrient_markers()` returns a set of widely‐used ratios, products  
#' and z-scores that capture iron metabolism, protein status, fatty-acid  
#' balance, renal excretion markers, mineral homeostasis and amino-acid  
#' patterns.
#'
#' @param data A `data.frame` or `tibble` containing your subject‐level data.
#' @param col_map Named list mapping the following keys to column names in `data`:
#'   \describe{
#'     \item{`ferritin`}          {Serum ferritin (ng/mL)}
#'     \item{`transferrin_sat`}   {Transferrin saturation (\%)} 
#'     \item{`albumin`}           {Serum albumin (g/L)}
#'     \item{`total_protein`}     {Total serum protein (g/L)}
#'     \item{`EPA`}               {Red‐cell EPA \% of total fatty acids}
#'     \item{`DHA`}               {Red‐cell DHA \% of total fatty acids}
#'     \item{`Mg`}                {Serum magnesium (mmol/L)}
#'     \item{`creatinine`}        {Serum creatinine (µmol/L)}
#'     \item{`glycated_albumin`}  {Glycated albumin (g/L)}
#'     \item{`uric_acid`}         {Serum uric acid (µmol/L)}
#'     \item{`BUN`}               {Blood urea nitrogen (mg/dL)}
#'     \item{`phosphate`}         {Serum phosphate (mmol/L)}
#'     \item{`calcium`}           {Serum calcium (mmol/L)}
#'     \item{`Na`}                {Serum sodium (mmol/L)}
#'     \item{`K`}                 {Serum potassium (mmol/L)}
#'     \item{`Cl`}                {Serum chloride (mmol/L)}
#'     \item{`HCO3`}              {Serum bicarbonate (mmol/L)}
#'     \item{`Tyr`}               {Serum tyrosine (µmol/L)}
#'     \item{`Phe`}               {Serum phenylalanine (µmol/L)}
#'   }
#'   You only need to supply the columns you have—any markers missing  
#'   will return `NA`.
#' @param verbose Logical; if `TRUE`, prints a progress message.
#'
#' @return A `tibble` with columns:
#' \describe{
#'   \item{FerritinTS}{Ferritin / Transferrin Saturation Ratio}
#'   \item{AGR}{Albumin / Globulin Ratio}
#'   \item{Omega3Index}{EPA + DHA (\% of total fatty acids)}
#'   \item{Mg_Cr_Ratio}{Magnesium / Creatinine}
#'   \item{GlycatedAlbuminPct}{\% Glycated Albumin}
#'   \item{UA_Cr_Ratio}{Uric Acid / Creatinine}
#'   \item{BUN_Cr_Ratio}{BUN / Creatinine}
#'   \item{Ca_x_Phosphate}{Calcium × Phosphate Product}
#'   \item{AnionGap}{(Na + K) − (Cl + HCO₃)}
#'   \item{Tyr_Phe_Ratio}{Tyrosine / Phenylalanine}
#' }
#'
#' @references
#' - Sullivan JL (2007). *Iron in Health and Disease*.  
#' - Peters T (2014). *All About Albumin*.  
#' - Harris WS & von Schacky C (2004). The Omega-3 Index. *Prostaglandins Leukot Essent Fatty Acids*.  
#' - Vormann J (2003). Magnesium Basics. *Clin Chim Acta*.  
#' - Koga M *et al.* (2014). Glycated albumin in diabetes. *J Diabetes Investig*.  
#' - Kanbay M *et al.* (2013). Uric acid & kidney function. *Clin J Am Soc Nephrol*.  
#' - Waikar SS *et al.* (2006). BUN:Cr as AKI marker. *Clin J Am Soc Nephrol*.  
#' - Block GA *et al.* (1998). Calcium×phosphate in CKD–MBD. *Kidney Int*.  
#' - Emmett M & Neville K (2010). Anion Gap Review. *J Emerg Med*.  
#' - Kaufman LM *et al.* (2005). Aromatic AAs in health. *Clin Chem*.  
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   ferritin         = c(50, 100),
#'   transferrin_sat  = c(30, 50),
#'   albumin          = c(45, 40),
#'   total_protein    = c(70, 75),
#'   EPA              = c(2.0, 2.5),
#'   DHA              = c(4.0, 4.5),
#'   Mg               = c(0.85, 0.90),
#'   creatinine       = c(80, 90),
#'   glycated_albumin = c(12, 14),
#'   uric_acid        = c(300, 400),
#'   BUN              = c(14, 16),
#'   phosphate        = c(1.0, 1.2),
#'   calcium          = c(2.3, 2.4),
#'   Na               = c(140, 138),
#'   K                = c(4.2, 4.0),
#'   Cl               = c(100, 102),
#'   HCO3             = c(24, 26),
#'   Tyr              = c(60, 70),
#'   Phe              = c(50, 55)
#' )
#' nutrient_markers(df, verbose = TRUE)
#'
#' @export
nutrient_markers <- function(data,
                             col_map = list(
                               ferritin         = "ferritin",
                               transferrin_sat  = "transferrin_sat",
                               albumin          = "albumin",
                               total_protein    = "total_protein",
                               EPA              = "EPA",
                               DHA              = "DHA",
                               Mg               = "Mg",
                               creatinine       = "creatinine",
                               glycated_albumin = "glycated_albumin",
                               uric_acid        = "uric_acid",
                               BUN              = "BUN",
                               phosphate        = "phosphate",
                               calcium          = "calcium",
                               Na               = "Na",
                               K                = "K",
                               Cl               = "Cl",
                               HCO3             = "HCO3",
                               Tyr              = "Tyr",
                               Phe              = "Phe"
                             ),
                             verbose = FALSE) {
  if (!is.data.frame(data)) {
    stop("nutrient_markers(): `data` must be a data.frame or tibble.")
  }
  if (verbose) message("-> computing nutrient markers")
  
  # helper to pull or NULL
  getcol <- function(key) {
    nm <- col_map[[key]]
    if (!is.null(nm) && nm %in% names(data)) data[[nm]] else NULL
  }
  
  ferr   <- getcol("ferritin")
  tsat   <- getcol("transferrin_sat")
  alb    <- getcol("albumin")
  tprot  <- getcol("total_protein")
  EPA    <- getcol("EPA")
  DHA    <- getcol("DHA")
  Mg     <- getcol("Mg")
  Cr     <- getcol("creatinine")
  gAlb   <- getcol("glycated_albumin")
  UA     <- getcol("uric_acid")
  BUN    <- getcol("BUN")
  phos   <- getcol("phosphate")
  Ca     <- getcol("calcium")
  Na_    <- getcol("Na")
  K_     <- getcol("K")
  Cl_    <- getcol("Cl")
  HCO3_  <- getcol("HCO3")
  Tyr    <- getcol("Tyr")
  Phe    <- getcol("Phe")
  
  # compute each marker (NA if inputs missing)
  FerritinTS        <- if (!is.null(ferr) && !is.null(tsat)) ferr / tsat else NA_real_
  AGR               <- if (!is.null(alb)  && !is.null(tprot)) alb / (tprot - alb) else NA_real_
  Omega3Index       <- if (!is.null(EPA)  && !is.null(DHA)) EPA + DHA else NA_real_
  Mg_Cr_Ratio       <- if (!is.null(Mg)   && !is.null(Cr))  Mg / Cr else NA_real_
  GlycatedAlbuminPct<- if (!is.null(gAlb) && !is.null(alb)) (gAlb / alb) * 100 else NA_real_
  UA_Cr_Ratio       <- if (!is.null(UA)   && !is.null(Cr))  UA / Cr else NA_real_
  BUN_Cr_Ratio      <- if (!is.null(BUN)  && !is.null(Cr))  BUN / Cr else NA_real_
  Ca_x_Phosphate    <- if (!is.null(Ca)   && !is.null(phos)) Ca * phos else NA_real_
  AnionGap          <- if (!is.null(Na_)  && !is.null(K_) && !is.null(Cl_) && !is.null(HCO3_))
    (Na_ + K_) - (Cl_ + HCO3_) else NA_real_
  Tyr_Phe_Ratio     <- if (!is.null(Tyr)  && !is.null(Phe))  Tyr / Phe else NA_real_
  
  # assemble results
  tibble::tibble(
    FerritinTS,
    AGR,
    Omega3Index,
    Mg_Cr_Ratio,
    GlycatedAlbuminPct,
    UA_Cr_Ratio,
    BUN_Cr_Ratio,
    Ca_x_Phosphate,
    AnionGap,
    Tyr_Phe_Ratio
  )
}

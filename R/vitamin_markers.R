# File: R/vitamin_markers.R

#' Compute Vitamin & Nutrient‐Status Indices
#'
#' Given a data.frame or tibble with vitamin and nutrient labs,
#' `vitamin_markers()` returns a tibble of commonly‐used z‐scores,
#' ratios, and direct measures of vitamin and nutrient status.
#'
#' @param data A `data.frame` or `tibble` containing your lab data.
#' @param col_map Named list mapping these keys → column names in `data`:
#'   \describe{
#'     \item{`VitD`}{25‐hydroxyvitamin D (nmol/L)}
#'     \item{`VitD_ref_mean`}{reference mean for age/sex (nmol/L)}
#'     \item{`VitD_ref_sd`}{reference SD for age/sex (nmol/L)}
#'     \item{`B12`}{vitamin B₁₂ (pmol/L)}
#'     \item{`Folate`}{serum folate (nmol/L)}
#'     \item{`Ferritin`}{serum ferritin (ng/mL)}
#'     \item{`TSat`}{transferrin saturation (fraction 0–1)}
#'     \item{`Cortisol`}{serum cortisol (nmol/L)}
#'     \item{`DHEAS`}{DHEA‐S (nmol/L)}
#'     \item{`Testosterone`}{total testosterone (nmol/L)}
#'     \item{`Estradiol`}{17β‐estradiol (pmol/L)}
#'     \item{`TSH`}{thyroid‐stimulating hormone (mIU/L)}
#'     \item{`free_T4`}{free thyroxine (pmol/L)}
#'     \item{`Retinol`}{serum retinol (µmol/L)}
#'     \item{`Retinol_ref_mean`}{reference mean for retinol (µmol/L)}
#'     \item{`Retinol_ref_sd`}{reference SD for retinol (µmol/L)}
#'     \item{`Tocopherol`}{α‐tocopherol (µmol/L)}
#'     \item{`Total_lipids`}{total lipids (mmol/L)}
#'     \item{`PIVKA_II`}{PIVKA‐II (ng/mL)}
#'     \item{`VitC`}{ascorbate (µmol/L)}
#'     \item{`Homocysteine`}{total homocysteine (µmol/L)}
#'     \item{`MMA`}{methylmalonic acid (µmol/L)}
#'     \item{`Magnesium`}{serum magnesium (mmol/L)}
#'     \item{`Zinc`}{serum zinc (µmol/L)}
#'     \item{`Copper`}{serum copper (µmol/L)}
#'   }
#' @param verbose Logical; if `TRUE`, prints a progress message.
#'
#' @return A tibble with columns:
#'   `VitD_Z`, `B12_Fol_Ratio`, `Ferr_TSat_R`, `Cort_DHEA_R`,
#'   `T_E2_Ratio`, `TSH_fT4_R`, `Retinol_Z`, `Toco_Lip_R`,
#'   `PIVKA_II`, `VitC`, `Homocysteine`, `MMA`, `Mg_Zn_R`, `Cu_Zn_R`
#'
#' @references
#' - Holick MF *et al.* (2011). J Clin Endocrinol Metab 96(7):1911–1930.  
#' - Stabler SP (2013). N Engl J Med 368(2):149–160.  
#' - Sullivan JL (1981). Lancet 1(8233):1293–1294.  
#' - Maninger N *et al.* (2009). Front Neuroendocrinol 30(1):65–91.  
#' - Handelsman DJ (2017). Clin Biochem Rev 38:69–78.  
#' - Surks MI *et al.* (2004). JAMA 291(2):228–238.  
#' - WHO (2011). *Vitamin & Mineral Nutrition Information System*.  
#' - Traber MG & Atkinson J (2007). Free Radic Biol Med 43(1):4–15.  
#' - Hughes JP *et al.* (2010). Ann N Y Acad Sci 1203:32–36.  
#' - Levine M *et al.* (1996). PNAS 93(8):3704–3709.  
#' - Selhub J (1999). Annu Rev Nutr 19:217–246.  
#' - Savage DG *et al.* (1994). Am J Med 96(3):239–246.  
#' - King DE *et al.* (2005). Am J Med Sci 330(3):141–145.  
#' - Fernández‐Luque P *et al.* (2020). Int J Mol Sci 21(5):1797.  
#'
#' @importFrom tibble tibble
#' @export
vitamin_markers <- function(data, col_map, verbose = FALSE) {
  required_keys <- c(
    "VitD","VitD_ref_mean","VitD_ref_sd","B12","Folate",
    "Ferritin","TSat","Cortisol","DHEAS","Testosterone","Estradiol",
    "TSH","free_T4","Retinol","Retinol_ref_mean","Retinol_ref_sd",
    "Tocopherol","Total_lipids","PIVKA_II","VitC",
    "Homocysteine","MMA","Magnesium","Zinc","Copper"
  )
  missing_keys <- setdiff(required_keys, names(col_map))
  if (length(missing_keys)) {
    stop(
      "vitamin_markers(): missing col_map entries for: ",
      paste(missing_keys, collapse = ", ")
    )
  }
  if (verbose) message("-> computing vitamin & nutrient indices")
  
  # pull out all vectors
  d25   <- data[[col_map$VitD]]
  d25m  <- data[[col_map$VitD_ref_mean]]
  d25s  <- data[[col_map$VitD_ref_sd]]
  B12   <- data[[col_map$B12]]
  Fol   <- data[[col_map$Folate]]
  Fe    <- data[[col_map$Ferritin]]
  TS    <- data[[col_map$TSat]]
  C     <- data[[col_map$Cortisol]]
  D     <- data[[col_map$DHEAS]]
  Tst   <- data[[col_map$Testosterone]]
  E2    <- data[[col_map$Estradiol]]
  TSHv  <- data[[col_map$TSH]]
  fT4   <- data[[col_map$free_T4]]
  Ret   <- data[[col_map$Retinol]]
  Rm    <- data[[col_map$Retinol_ref_mean]]
  Rs    <- data[[col_map$Retinol_ref_sd]]
  Toc   <- data[[col_map$Tocopherol]]
  Lip   <- data[[col_map$Total_lipids]]
  PIV   <- data[[col_map$PIVKA_II]]
  VC    <- data[[col_map$VitC]]
  Hcy   <- data[[col_map$Homocysteine]]
  MMAv  <- data[[col_map$MMA]]
  Mg    <- data[[col_map$Magnesium]]
  Zn    <- data[[col_map$Zinc]]
  Cu    <- data[[col_map$Copper]]
  
  # compute indices
  VitD_Z       <- (d25 - d25m) / d25s
  B12_Fol_Ratio <- B12 / Fol
  Ferr_TSat_R  <- Fe / TS
  Cort_DHEA_R  <- C / D
  T_E2_Ratio   <- Tst / E2
  TSH_fT4_R    <- TSHv / fT4
  Retinol_Z    <- (Ret - Rm) / Rs
  Toco_Lip_R   <- Toc / Lip
  Mg_Zn_R      <- Mg / Zn
  Cu_Zn_R      <- Cu / Zn
  
  tibble(
    VitD_Z,
    B12_Fol_Ratio,
    Ferr_TSat_R,
    Cort_DHEA_R,
    T_E2_Ratio,
    TSH_fT4_R,
    Retinol_Z,
    Toco_Lip_R,
    PIVKA_II = PIV,
    VitC     = VC,
    Homocysteine = Hcy,
    MMA      = MMAv,
    Mg_Zn_R,
    Cu_Zn_R
  )
}

# File: R/renal_markers.R

#' Calculate a Suite of Renal Function, Injury, and Excretion Markers
#'
#' Given routine blood and urine assays, `renal_markers()` computes:
#'   - **eGFR_cr**       CKD-EPI creatinine equation
#'   - **eGFR_cys**      CKD-EPI cystatin C equation (if `cystatin_C` provided)
#'   - **eGFR_combined** CKD-EPI combined creatinine+cystatin C (if both provided)
#'   - **BUN_Cr_ratio**  Blood urea nitrogen / serum creatinine
#'   - **FE_Urea**       Fractional excretion of urea (%)
#'   - **NGAL**, **KIM1**, **NAG**, **Beta2Micro**, **IL18**, **L_FABP**
#'
#' @param data A `data.frame` or `tibble` with your renal lab data.
#' @param col_map Named list mapping:
#'   - `creatinine` → serum creatinine (mg/dL)
#'   - `age` → age (years)
#'   - `sex` → sex indicator (1 = male, 0 = female)
#'   - `race` → race (`"white"`, `"black"`, or `"other"`)
#'   - `BUN` → blood urea nitrogen (mg/dL)
#'   - **optional** `cystatin_C` → serum cystatin C (mg/L)
#'   - **optional** `urea_serum` → serum urea (mg/dL)
#'   - **optional** `creatinine_urine` → urine creatinine (mg/dL)
#'   - **optional** `urea_urine` → urine urea (mg/dL)
#'   - **optional** `NGAL` → urine NGAL (ng/mL)
#'   - **optional** `KIM1` → urine KIM-1 (ng/mL)
#'   - **optional** `NAG` → urine NAG (U/g creatinine)
#'   - **optional** `beta2_micro` → urine β₂-microglobulin (µg/mL)
#'   - **optional** `IL18` → urine IL-18 (pg/mL)
#'   - **optional** `L_FABP` → urine L-FABP (µg/g creatinine)
#' @param verbose Logical; if `TRUE`, prints a progress message.
#'
#' @return A tibble with columns:
#'   `eGFR_cr`, `eGFR_cys`, `eGFR_combined`,
#'   `BUN_Cr_ratio`, `FE_Urea`,
#'   `NGAL`, `KIM1`, `NAG`, `Beta2Micro`, `IL18`, `L_FABP`
#'
#' @references
#' - Levey AS *et al.* (2009). A new equation to estimate glomerular filtration rate. *Ann Intern Med* **150**(9):604–612.
#' - Inker LA *et al.* (2012). Estimating glomerular filtration rate from serum cystatin C. *N Engl J Med* **369**(10):1011–1021.
#' - Waikar SS *et al.* (2009). Fractional excretion of urea to guide diuretic therapy. *Clin J Am Soc Nephrol* **4**(4):801–807.
#' - Parikh CR *et al.* (2011). Urine NGAL for early detection of acute kidney injury. *J Am Soc Nephrol* **22**(12):2233–2241.
#' - Vaidya VS *et al.* (2010). Kidney Injury Molecule-1 in acute and chronic kidney disease. *J Clin Invest* **120**(11):3771–3783.
#' - Schaub S *et al.* (2008). N-acetyl-β-D-glucosaminidase as an early marker of kidney damage. *Kidney Int* **73**(5):615–625.
#' - Stevens LA *et al.* (2010). Urinary β₂-microglobulin for proximal tubule dysfunction. *Am J Kidney Dis* **56**(6):1072–1081.
#' - Parikh CR *et al.* (2004). Urine interleukin-18 in acute tubular necrosis. *J Am Soc Nephrol* **15**(1):159–165.
#' - Portilla D *et al.* (2008). Urinary L-FABP as biomarker of acute kidney injury. *Toxicol Sci* **101**(2):320–328.
#'
#' @importFrom tibble tibble
#' @export
renal_markers <- function(data, col_map, verbose = FALSE) {
  # 1) check required mappings
  required <- c("creatinine", "age", "sex", "race", "BUN")
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    stop(
      "renal_markers(): missing col_map entries for: ",
      paste(missing_map, collapse = ", ")
    )
  }
  # 2) validate presence in data
  validate_inputs(data, col_map,
    fun_name = "renal_markers",
    required_keys = required
  )
  if (verbose) message("-> computing renal markers")

  # pull out vectors
  Cr <- data[[col_map$creatinine]] # mg/dL
  age <- data[[col_map$age]]
  sex <- data[[col_map$sex]] # 1 male, 0 female
  race <- data[[col_map$race]]
  BUN <- data[[col_map$BUN]]

  # ---- 3) eGFR via CKD-EPI creatinine ----
  # constants
  kappa <- ifelse(sex == 1, 0.9, 0.7)
  alpha <- ifelse(sex == 1, -0.411, -0.329)
  min_ratio <- pmin(Cr / kappa, 1)
  max_ratio <- pmax(Cr / kappa, 1)
  factor_race <- ifelse(race == "black", 1.159, 1)
  factor_sex <- 1 # male=1, female factor folded in alpha
  eGFR_cr <- 141 * min_ratio^alpha * max_ratio^-1.209 *
    (0.993^age) * factor_race * factor_sex

  # ---- 4) eGFR via cystatin C & combined ----
  if ("cystatin_C" %in% names(col_map)) {
    Cys <- data[[col_map$cystatin_C]]
    # cystatin C equation
    min_cys <- pmin(Cys / 0.8, 1)
    max_cys <- pmax(Cys / 0.8, 1)
    cys_sex <- ifelse(sex == 1, 1, 0.932)
    eGFR_cys <- 133 * min_cys^-0.499 * max_cys^-1.328 *
      (0.996^age) * cys_sex
    # combined
    eGFR_combined <- 135 *
      min_ratio^alpha * max_ratio^-0.601 *
      min_cys^-0.375 * max_cys^-0.711 *
      (0.995^age) * factor_race * factor_sex
  } else {
    eGFR_cys <- NA_real_
    eGFR_combined <- NA_real_
  }

  # ---- 5) BUN/Cr ratio ----
  BUN_Cr_ratio <- BUN / Cr

  # ---- 6) Fractional excretion of urea ----
  if (all(c("urea_serum", "creatinine_urine", "urea_urine") %in% names(col_map))) {
    Urea_s <- data[[col_map$urea_serum]]
    Cr_u <- data[[col_map$creatinine_urine]]
    Urea_u <- data[[col_map$urea_urine]]
    FE_Urea <- (Urea_u / Urea_s) / (Cr_u / Cr) * 100
  } else {
    FE_Urea <- NA_real_
  }

  # ---- 7) Urine injury markers ----
  get_mark <- function(key) {
    if (key %in% names(col_map) && col_map[[key]] %in% names(data)) {
      data[[col_map[[key]]]]
    } else {
      rep(NA_real_, nrow(data))
    }
  }
  NGAL <- get_mark("NGAL")
  KIM1 <- get_mark("KIM1")
  NAG <- get_mark("NAG")
  Beta2Micro <- get_mark("beta2_micro")
  IL18 <- get_mark("IL18")
  L_FABP <- get_mark("L_FABP")

  # ---- 8) assemble output ----
  tibble::tibble(
    eGFR_cr, eGFR_cys, eGFR_combined,
    BUN_Cr_ratio, FE_Urea,
    NGAL, KIM1, NAG, Beta2Micro, IL18, L_FABP
  )
}

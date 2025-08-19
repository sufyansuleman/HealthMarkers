# R/hormone_markers.R

#' Compute a suite of hormone ratio markers
#'
#' Given a data.frame or tibble with hormone measurements,
#' `hormone_markers()` returns a tibble of nine widely used clinical ratios:
#'
#' 1. FAI       - Free Androgen Index = (total_testosterone / SHBG) * 100
#' 2. LH_FSH    - Luteinizing Hormone / Follicle Stimulating Hormone
#' 3. E2_P      - Estradiol / Progesterone
#' 4. T3_T4     - Free T3 / Free T4
#' 5. ARR       - Aldosterone / Renin Ratio
#' 6. Ins_Glu   - Insulin / Glucagon
#' 7. GH_IGF1   - Growth Hormone / IGF1
#' 8. PRL_T     - Prolactin / total_testosterone
#' 9. CAR_slope - Cortisol Awakening Response slope = (cortisol_30 - cortisol_0) / 30
#'
#' @param data A data.frame or tibble containing your hormone data.
#' @param col_map Named list mapping the following keys to column names in `data`:
#'   \describe{
#'     \item{total_testosterone}{Total testosterone (nmol/L)}
#'     \item{SHBG}{Sex hormone binding globulin (nmol/L)}
#'     \item{LH}{Luteinizing hormone (IU/L)}
#'     \item{FSH}{Follicle stimulating hormone (IU/L)}
#'     \item{estradiol}{17-beta estradiol (pmol/L)}
#'     \item{progesterone}{Progesterone (nmol/L)}
#'     \item{free_T3}{Free triiodothyronine (pmol/L)}
#'     \item{free_T4}{Free thyroxine (pmol/L)}
#'     \item{aldosterone}{Aldosterone (ng/dL)}
#'     \item{renin}{Plasma renin activity or concentration}
#'     \item{insulin}{Insulin (microIU/mL)}
#'     \item{glucagon}{Glucagon (pmol/L)}
#'     \item{GH}{Growth hormone (microg/L)}
#'     \item{IGF1}{Insulin-like growth factor 1 (microg/L)}
#'     \item{prolactin}{Prolactin (ng/mL)}
#'     \item{cortisol_0}{Morning (awakening) cortisol (nmol/L)}
#'     \item{cortisol_30}{Cortisol 30 minutes post-awakening (nmol/L)}
#'   }
#' @param verbose Logical; if TRUE, prints a progress message.
#'
#' @return A tibble with columns:
#'   FAI, LH_FSH, E2_P, T3_T4, ARR,
#'   Ins_Glu, GH_IGF1, PRL_T, CAR_slope
#'
#' @references
#' - Vermeulen A et al. (1999). A critical evaluation of simple methods for
#'   the estimation of free testosterone in serum. J Clin Endocrinol Metab 84(10):3666-3672.
#' - Lobo RA & Stanczyk FZ (2013). LH/FSH ratios in polycystic ovary syndrome.
#'   Endocr Rev 34(3):397-421.
#' - Handelsman DJ (2017). Sex steroid ratios in clinical practice.
#'   Clin Biochem Rev 38(2):69-78.
#' - Visser TJ et al. (2010). Serum T3/T4 ratio as clinical index of deiodinase activity.
#'   Thyroid 20(7):751-758.
#' - Mulatero P et al. (2004). The aldosterone/renin ratio in primary aldosteronism.
#'   Endocrine 25(3):133-138.
#' - Boden G & Wirapati P (1994). The insulin/glucagon ratio in metabolic disease.
#'   Metabolism 43(10):1229-1236.
#' - Williams G et al. (2000). GH/IGF-1 axis alterations in adult disease.
#'   Growth Horm IGF Res 10(4):249-260.
#' - Webster JI et al. (2002). Prolactin/testosterone imbalance and clinical implications.
#'   J Endocrinol Invest 25(3):164-173.
#' - Pruessner JC et al. (2003). Two formulas for computation of the area under
#'   the curve represent measures of total hormone concentration vs time-dependent change.
#'   Psychoneuroendocrinology 28(7):916-931.
#'
#' @importFrom tibble tibble
#' @export
hormone_markers <- function(data, col_map, verbose = FALSE) {
  required <- c(
    "total_testosterone", "SHBG", "LH", "FSH", "estradiol", "progesterone",
    "free_T3", "free_T4", "aldosterone", "renin", "insulin", "glucagon",
    "GH", "IGF1", "prolactin", "cortisol_0", "cortisol_30"
  )
  missing_map <- setdiff(required, names(col_map))
  if (length(missing_map)) {
    stop(
      "hormone_markers(): missing col_map entries for: ",
      paste(missing_map, collapse = ", ")
    )
  }
  if (verbose) message("-> computing hormone ratios")

  # extract vectors
  get <- function(key) data[[col_map[[key]]]]
  T <- get("total_testosterone")
  S <- get("SHBG")
  LHv <- get("LH")
  FSHv <- get("FSH")
  E2 <- get("estradiol")
  P4 <- get("progesterone")
  fT3 <- get("free_T3")
  fT4 <- get("free_T4")
  Ald <- get("aldosterone")
  Ren <- get("renin")
  Ins <- get("insulin")
  Glu <- get("glucagon")
  GHv <- get("GH")
  IGF <- get("IGF1")
  PRL <- get("prolactin")
  C0 <- get("cortisol_0")
  C30 <- get("cortisol_30")

  # compute ratios
  FAI <- (T / S) * 100
  LH_FSH <- LHv / FSHv
  E2_P <- E2 / P4
  T3_T4 <- fT3 / fT4
  ARR <- Ald / Ren
  Ins_Glu <- Ins / Glu
  GH_IGF1 <- GHv / IGF
  PRL_T <- PRL / T
  CAR_slope <- (C30 - C0) / 30

  tibble::tibble(
    FAI, LH_FSH, E2_P, T3_T4, ARR,
    Ins_Glu, GH_IGF1, PRL_T, CAR_slope
  )
}

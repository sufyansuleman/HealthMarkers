# R/kidney_kfre.R

#' Kidney Failure Risk Equation (2- and 5-year)
#'
#' Compute 2- and 5-year risk of end-stage kidney disease using the
#' 4-variable KFRE (Tangri *et al.*, 2011).
#'
#' @param data A data.frame or tibble containing at least the columns
#'   mapped below.
#' @param col_map Named list mapping:
#'   - `age`  → age in years
#'   - `sex`  → sex code (1 = male, 2 = female)
#'   - `eGFR` → estimated GFR (mL/min/1.73 m²)
#'   - `UACR` → urine albumin-to-creatinine ratio (mg/g)
#' @return A tibble with two columns:
#'   - `KFRE_2yr` — risk (0–1) at 2 years  
#'   - `KFRE_5yr` — risk (0–1) at 5 years
#' @export
#' @examples
#' library(tibble)
#' df <- tibble(
#'   age  = 65,
#'   sex  = 1,
#'   eGFR = 45,
#'   UACR = 300
#' )
#' kidney_failure_risk(
#'   data = df,
#'   col_map = list(
#'     age  = "age",
#'     sex  = "sex",
#'     eGFR = "eGFR",
#'     UACR = "UACR"
#'   )
#' )
kidney_failure_risk <- function(data,
                                col_map = list(
                                  age  = "age",
                                  sex  = "sex",
                                  eGFR = "eGFR",
                                  UACR = "UACR"
                                )) {
  # 0) validate presence of required inputs
  validate_inputs(
    data, col_map,
    fun_name      = "kidney_failure_risk",
    required_keys = c("age", "sex", "eGFR", "UACR")
  )
  
  # 1) pull inputs
  age  <- data[[col_map$age]]
  sex  <- data[[col_map$sex]]
  eGFR <- data[[col_map$eGFR]]
  UACR <- data[[col_map$UACR]]
  
  # 2) map sex to male indicator (1 for male, 0 otherwise)
  male <- ifelse(sex == 1, 1, 0)
  
  # 3) prognostic index
  pi <- 0.220 * log(age) +
    (-0.556) * log(eGFR) +
    0.451  * log(UACR) +
    0.391  * male
  
  # 4) baseline survival at 2 and 5 years
  S0_2 <- 0.934
  S0_5 <- 0.881
  
  # 5) compute risks
  KFRE_2yr <- 1 - (S0_2 ^ exp(pi))
  KFRE_5yr <- 1 - (S0_5 ^ exp(pi))
  
  # 6) return tibble
  tibble::tibble(
    KFRE_2yr = KFRE_2yr,
    KFRE_5yr = KFRE_5yr
  )
}

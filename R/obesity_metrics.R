# R/obesity_metrics.R

#' Compute anthropometric obesity & adiposity indices
#'
#' Calculates a comprehensive set of body shape and adiposity indices:
#' * BMI and WHO BMI categories
#' * Waist-to-hip ratio (WHR) and optional WHR adjusted for BMI (WHRadjBMI)
#' * Waist-to-height ratio (WHtR)
#' * Abdominal Volume Index (AVI)
#' * Body Adiposity Index (BAI)
#' * A Body Shape Index (ABSI)
#' * Body Roundness Index (BRI)
#' * Conicity Index (CI)
#' * (Optional) Relative Fat Mass (RFM)
#'
#' @param data A data.frame or tibble containing the input columns.
#' @param weight Unquoted column name for weight.
#' @param height Unquoted column name for height.
#' @param waist Unquoted column name for waist circumference.
#' @param hip Unquoted column name for hip circumference.
#' @param sex   (Optional) Unquoted column name for sex, coded 0=male, 1=female; required if include_RFM=TRUE.
#' @param weight_unit   One of c("kg","lb"); if "lb", converts weight to kg by *0.45359237.
#' @param height_unit   One of c("cm","m"); if "cm", converts height to metres by /100.
#' @param adjust_WHR    Logical; if TRUE, adds a column WHRadjBMI as residuals from WHR ~ BMI.
#' @param include_RFM   Logical; if TRUE, computes Relative Fat Mass (requires sex column).
#'
#' @return A tibble with original columns plus new indices:
#' * weight_kg, height_m, BMI, BMI_cat,
#' * WHR, WHRadjBMI (optional), waist_to_height_ratio,
#' * AVI, BAI, ABSI, BRI, CI,
#' * waist_to_BMI_ratio, weight_to_height_ratio,
#' * RFM (optional).
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang enquo quo_name quo_is_null
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   wt     = c(70, 80),    # kg
#'   ht     = c(175, 165),  # cm
#'   waist  = c(80, 90),     # cm
#'   hip    = c(100, 95),    # cm
#'   sex    = c(0, 1)
#' )
#' obesity_indices(
#'   df,
#'   weight       = wt,
#'   height       = ht,
#'   waist        = waist,
#'   hip          = hip,
#'   sex          = sex,
#'   weight_unit  = "kg",
#'   height_unit  = "cm",
#'   adjust_WHR   = TRUE,
#'   include_RFM  = TRUE
#' )
obesity_indices <- function(data,
                            weight,
                            height,
                            waist,
                            hip,
                            sex = NULL,
                            weight_unit   = c("kg","lb"),
                            height_unit   = c("cm","m"),
                            adjust_WHR    = FALSE,
                            include_RFM   = FALSE) {
  # Capture and quote arguments
  wt_name  <- rlang::quo_name(rlang::enquo(weight))
  ht_name  <- rlang::quo_name(rlang::enquo(height))
  wst_name <- rlang::quo_name(rlang::enquo(waist))
  hp_name  <- rlang::quo_name(rlang::enquo(hip))
  sx_q     <- rlang::enquo(sex)
  
  # Validate required columns
  required <- c(wt_name, ht_name, wst_name, hp_name)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    stop("obesity_indices(): missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (include_RFM && rlang::quo_is_null(sx_q)) {
    stop("obesity_indices(): 'sex' must be provided to compute RFM")
  }
  
  # Unit matching
  weight_unit <- match.arg(weight_unit)
  height_unit <- match.arg(height_unit)
  
  wt  <- rlang::enquo(weight)
  ht  <- rlang::enquo(height)
  wst <- rlang::enquo(waist)
  hp  <- rlang::enquo(hip)
  sx  <- sx_q
  
  # Compute all indices
  out <- data %>%
    dplyr::mutate(
      # Unit conversions
      weight_kg = dplyr::case_when(
        weight_unit == "kg" ~ !!wt,
        weight_unit == "lb" ~ !!wt * 0.45359237
      ),
      height_m = dplyr::case_when(
        height_unit == "m"  ~ !!ht,
        height_unit == "cm" ~ !!ht / 100
      ),
      # BMI and category
      BMI = weight_kg / (height_m^2),
      BMI_cat = dplyr::case_when(
        BMI < 18.5 ~ "Underweight",
        BMI < 25   ~ "Normal weight",
        BMI < 30   ~ "Overweight",
        BMI < 35   ~ "Obesity Class I",
        BMI < 40   ~ "Obesity Class II",
        BMI >= 40  ~ "Obesity Class III",
        TRUE       ~ NA_character_
      ),
      # Simple ratios
      WHR = (!!wst) / (!!hp),
      waist_to_height_ratio = (!!wst) / height_m,
      waist_to_BMI_ratio    = (!!wst) / BMI,
      weight_to_height_ratio = weight_kg / height_m,
      # Advanced indices
      AVI  = (2 * (!!wst)^2 + 0.7 * ((!!wst - !!hp))^2) / 1000,
      BAI  = (!!hp) / (height_m^1.5) - 18,
      ABSI = (!!wst) / (BMI^(2/3) * height_m^(1/2)),
      BRI  = 364.2 - 365.5 * sqrt(pmax(0, 1 - ((!!wst)/(2 * pi * height_m))^2)),
      CI   = (!!wst) / (0.109 * sqrt(weight_kg / height_m))
    )
  
  # WHR adjusted for BMI
  if (adjust_WHR) {
    res <- stats::resid(stats::lm(WHR ~ BMI, data = out))
    out <- out %>% dplyr::mutate(WHRadjBMI = as.numeric(res))
  }
  
  # Relative Fat Mass
  if (include_RFM) {
    out <- out %>%
      dplyr::mutate(RFM = as.numeric(64 - 20 * (height_m / (!!wst)) + 12 * (!!sx)))
  }
  
  out
}

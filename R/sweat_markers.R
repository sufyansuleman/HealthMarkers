# R/sweat_markers.R

#' Calculate sweat‐based ionic & metabolic markers
#'
#' Computes:
#'  - **sweat_chloride** (mmol/L)  
#'  - **Na_K_ratio**      (sweat Na⁺/K⁺)  
#'  - **sweat_lactate**   (mmol/L)  
#'  - **sweat_rate**      (L/m^2/h)  
#'
#' @param data A data.frame or tibble containing at least:
#'   - `sweat_chloride`    (mmol/L)  
#'   - `sweat_Na`, `sweat_K` (mmol/L)  
#'   - `sweat_lactate`     (mmol/L)  
#'   - `weight_before`, `weight_after` (kg)  
#'   - `duration`          (h)  
#'   - `body_surface_area` (m^2)  
#' @param verbose Logical; if `TRUE`, prints progress messages.  
#'
#' @return A tibble with columns:
#'   `sweat_chloride`, `Na_K_ratio`, `sweat_lactate`, `sweat_rate`  
#' @export
#' @examples
#' df <- tibble::tibble(
#'   sweat_chloride    = 45,
#'   sweat_Na          = 55,
#'   sweat_K           = 5,
#'   sweat_lactate     = 4.8,
#'   weight_before     = 70.0,
#'   weight_after      = 69.5,
#'   duration          = 1.0,
#'   body_surface_area = 1.9
#' )
#' sweat_markers(df)
sweat_markers <- function(data, verbose = FALSE) {
  # 1) validate required columns
  req <- c(
    "sweat_chloride", "sweat_Na", "sweat_K", "sweat_lactate",
    "weight_before", "weight_after", "duration", "body_surface_area"
  )
  missing_cols <- setdiff(req, names(data))
  if (length(missing_cols)) {
    stop("sweat_markers(): missing columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (verbose) message("-> computing sweat markers")
  
  # 2) compute Na/K ratio
  Na_K_ratio <- data$sweat_Na / data$sweat_K
  
  # 3) compute sweat rate (L/m^2/h) from weight loss
  mass_loss_kg <- data$weight_before - data$weight_after
  sweat_rate <- (mass_loss_kg / data$duration) / data$body_surface_area
  
  # 4) assemble output
  tibble::tibble(
    sweat_chloride = data$sweat_chloride,
    Na_K_ratio,
    sweat_lactate  = data$sweat_lactate,
    sweat_rate
  )
}

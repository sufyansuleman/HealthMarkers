# File: R/pulmo_markers.R

#' Calculate pulmonary function markers (FEV₁/FVC, z-scores, percent predicted, LLN, etc.)
#'
#' Uses the `rspiro` reference equations to compute predicted normals,
#' z-scores, percent predicted and lower limits of normal (LLN) for FEV₁, FVC,
#' and the FEV₁/FVC ratio.
#'
#' @param data A data.frame or tibble with columns:
#'   - `age`       (numeric): years
#'   - `sex`       (character): "male"/"female" (case‐insensitive)
#'   - `height`    (numeric): cm or m (auto‐detected)
#'   - `ethnicity` (character): e.g. "Caucasian", "African-American", …
#'   - `fev1`      (numeric): observed FEV₁ in L
#'   - `fvc`       (numeric): observed FVC in L
#' @param equation One of `c("GLI","GLIgl","NHANES3")` (see `rspiro` for more)
#' @param verbose   Logical; if `TRUE` prints progress messages.
#'
#' @return A tibble with:
#'   - `fev1_pred`, `fev1_z`,   `fev1_pctpred`, `fev1_LLN`
#'   - `fvc_pred`,  `fvc_z`,    `fvc_pctpred`,  `fvc_LLN`
#'   - `fev1_fvc_ratio`, `fev1_fvc_pred`, `fev1_fvc_z`, `fev1_fvc_pctpred`, `fev1_fvc_LLN`
#' @export
pulmo_markers <- function(data,
                          equation = c("GLI", "GLIgl", "NHANES3"),
                          verbose  = FALSE) {
  equation <- match.arg(equation)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  ## 0) Basic coercion & validation
  req <- c("age","sex","height","ethnicity","fev1","fvc")
  miss <- setdiff(req, names(data))
  if (length(miss)) stop(
    "pulmo_markers(): missing required columns: ", paste(miss, collapse=", "),
    call. = FALSE
  )
  data$age    <- as.numeric(data$age)
  data$height <- as.numeric(data$height)
  data$fev1   <- as.numeric(data$fev1)
  data$fvc    <- as.numeric(data$fvc)
  
  if (verbose) {
    message("↪ FEV1 class: ", class(data$fev1), " | any NA: ", anyNA(data$fev1))
    message("↪ FVC class:  ", class(data$fvc),  " | any NA: ", anyNA(data$fvc))
  }
  
  sex_char <- tolower(as.character(data$sex))
  gender   <- ifelse(sex_char %in% c("male","m"), 1L, 2L)
  eth_char <- tolower(as.character(data$ethnicity))
  ethnicity <- ifelse(
    eth_char %in% c("caucasian","white"),            1L,
    ifelse(eth_char %in% c("african-american","black"), 2L,
           ifelse(eth_char %in% c("ne asian","northeast asian"),3L,
                  ifelse(eth_char %in% c("se asian","southeast asian"),4L, 5L)))
  )
  
  if (any(data$height > 3, na.rm=TRUE)) {
    if (verbose) message("-> converting height from cm to m")
    data$height <- data$height / 100
  }
  if (verbose) message("-> pulmo_markers[", equation, "] gender=", unique(gender),
                       " eth=", unique(ethnicity))
  
  ## 1) lookup reference functions
  ns         <- asNamespace("rspiro")
  pred_fun   <- get(paste0("pred_",   equation), ns)
  zscore_fun <- get(paste0("zscore_", equation), ns)
  lln_fun    <- get(paste0("LLN_",    equation), ns)
  
  ## 2) compute predicted, z-scores, LLN by equation
  if (equation == "GLIgl") {
    fev1_pred <- pred_fun(data$age, data$height, gender, param = "FEV1")
    fvc_pred  <- pred_fun(data$age, data$height, gender, param = "FVC")
    fev1_z    <- zscore_fun(data$age, data$height, gender, FEV1 = data$fev1)
    fvc_z     <- zscore_fun(data$age, data$height, gender, FVC  = data$fvc)
    fev1_LLN  <- lln_fun(data$age, data$height, gender, param = "FEV1")
    fvc_LLN   <- lln_fun(data$age, data$height, gender, param = "FVC")
  } else {
    fev1_pred <- pred_fun(data$age, data$height, gender, ethnicity, param = "FEV1")
    fvc_pred  <- pred_fun(data$age, data$height, gender, ethnicity, param = "FVC")
    fev1_z    <- zscore_fun(data$age, data$height, gender, ethnicity, FEV1 = data$fev1)
    fvc_z     <- zscore_fun(data$age, data$height, gender, ethnicity, FVC  = data$fvc)
    fev1_LLN  <- lln_fun(data$age, data$height, gender, ethnicity, param = "FEV1")
    fvc_LLN   <- lln_fun(data$age, data$height, gender, ethnicity, param = "FVC")
  }
  
  ## 3) percent predicted
  fev1_pctpred <- 100 * data$fev1 / fev1_pred
  fvc_pctpred  <- 100 * data$fvc  / fvc_pred
  
  ## 4) ratio outputs
  obs_ratio  <- data$fev1 / data$fvc
  pred_ratio <- fev1_pred / fvc_pred
  ratio_z    <- (obs_ratio - mean(pred_ratio, na.rm=TRUE)) / sd(pred_ratio, na.rm=TRUE)
  ratio_pct  <- 100 * obs_ratio / pred_ratio
  
  ## 5) assemble results
  tibble::tibble(
    fev1_pred        = fev1_pred,
    fev1_z           = fev1_z,
    fev1_pctpred     = fev1_pctpred,
    fev1_LLN         = fev1_LLN,
    fvc_pred         = fvc_pred,
    fvc_z            = fvc_z,
    fvc_pctpred      = fvc_pctpred,
    fvc_LLN          = fvc_LLN,
    fev1_fvc_ratio   = obs_ratio,
    fev1_fvc_pred    = pred_ratio,
    fev1_fvc_z       = ratio_z,
    fev1_fvc_pctpred = ratio_pct,
    fev1_fvc_LLN     = NA_real_
  )
}

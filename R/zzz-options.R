# Package verbosity option + flexible logger

hm_get_verbosity <- function() {
  lv <- getOption("healthmarkers.verbose", default = "none")
  if (isTRUE(lv)) return("inform")
  if (identical(lv, FALSE)) return("none")
  lv <- tolower(as.character(lv))
  if (lv %in% c("info")) lv <- "inform"
  if (lv %in% c("none","inform","debug")) lv else "none"
}

# Accepts hm_inform(level = "inform", msg = "text") OR hm_inform("text", level = "inform")
hm_inform <- function(level = c("inform","info","debug"), msg = NULL) {
  # Support hm_inform("message", level="inform")
  if (is.null(msg) && !missing(level) && is.character(level) && length(level) == 1L &&
      !(tolower(level) %in% c("inform","info","debug"))) {
    msg <- level
    level <- "inform"
  }
  level <- tolower(if (length(level)) level[[1]] else "inform")
  if (level == "info") level <- "inform"

  allowed <- switch(hm_get_verbosity(),
    none = character(0),
    inform = "inform",
    debug = c("inform","debug"),
    character(0)
  )
  if (level %in% allowed && !is.null(msg) && nzchar(msg)) message(msg)
  invisible(NULL)
}
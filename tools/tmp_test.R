suppressMessages(devtools::load_all(".", quiet=TRUE))
df <- readRDS("inst/extdata/simulated_hm_data.rds")
r1 <- tryCatch(liver_markers(df, verbose=FALSE), error=function(e) e)
cat("liver direct:", if(inherits(r1,"error")) paste("ERR", conditionMessage(r1)) else paste("OK ncol", ncol(r1)), "\n")
all_col_map <- tryCatch(hm_col_report(df, verbose=FALSE), error=function(e) list())
cat("col_map size:", length(all_col_map), "\n")
r2 <- tryCatch(liver_markers(df, col_map=all_col_map, verbose=FALSE), error=function(e) e)
cat("liver+col_map:", if(inherits(r2,"error")) paste("ERR", conditionMessage(r2)) else paste("OK ncol", ncol(r2)), "\n")


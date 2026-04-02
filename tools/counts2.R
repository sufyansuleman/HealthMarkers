suppressMessages(pkgload::load_all(".", quiet=TRUE))
res <- testthat::test_dir("tests/testthat", reporter="silent", stop_on_failure=FALSE)
df <- as.data.frame(res)
cat("Tests:", nrow(df), "\n")
cat("PASS :", sum(df[["passed"]]), "\n")
cat("FAIL :", sum(df[["failed"]]), "\n")
cat("ERROR:", sum(df[["error"]] == TRUE), "\n")
fails <- df[df$failed > 0 | df$error == TRUE, ]
if (nrow(fails) > 0) {
  cat("FAILING:\n")
  print(fails[, c("test","failed","error")])
}

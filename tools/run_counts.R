suppressMessages(pkgload::load_all(".", quiet=TRUE))
res <- testthat::test_dir("tests/testthat", reporter="summary", stop_on_failure=FALSE)
df <- as.data.frame(res)
cat(sprintf("PASS: %d  FAIL: %d  ERROR: %d\n",
            sum(df$passed, na.rm=TRUE),
            sum(df$failed, na.rm=TRUE),
            sum(df$error, na.rm=TRUE)))
fails <- df[df$failed > 0 | df$error > 0, ]
if (nrow(fails) > 0) {
  cat("FAILING TESTS:\n")
  print(fails[, c("test", "failed", "error")])
}

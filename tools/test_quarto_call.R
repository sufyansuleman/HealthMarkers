cat("quarto=", Sys.which("quarto"), "\n")
res <- system2("quarto", "-V", stdout = TRUE, stderr = TRUE, env = paste0("TMPDIR=", tempdir()))
print(res)

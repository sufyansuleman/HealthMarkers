sink("tools/_test_results.txt", split=FALSE)
options(warn=0)
devtools::test()
sink()

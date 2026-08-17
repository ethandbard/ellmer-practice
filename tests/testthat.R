# Run from the project root:
#   testthat::test_dir("tests/testthat")
# or
#   source("tests/testthat.R")

library(testthat)

test_dir("tests/testthat", reporter = "summary")

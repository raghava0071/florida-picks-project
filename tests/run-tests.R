if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
testthat::test_dir("tests", reporter = testthat::SummaryReporter$new())

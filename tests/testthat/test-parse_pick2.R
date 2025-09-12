test_that("parse_pick2 returns clean frame", {
  source("R/parse_pick2.R")
  df <- parse_pick2()
  expect_true(all(c("draw_date","time","d1","d2") %in% names(df)))
  expect_gt(nrow(df), 1000)
  expect_true(all(df$d1 %in% 0:9))
  expect_true(all(df$d2 %in% 0:9))
  expect_false(any(is.na(df$draw_date)))
})

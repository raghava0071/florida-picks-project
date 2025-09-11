# tests/test-parse.R
source("R/parse_pick2.R")

test_that("end-to-end parse returns clean table", {
  pdf <- download_pick2_pdf("data_raw/pick2.pdf")
  tbl <- parse_pick2_pdf(pdf)

  expect_s3_class(tbl, "tbl_df")
  expect_true(all(c("draw_date","time","d1","d2") %in% names(tbl)))

  # digits are 0..9 and not NA
  expect_true(all(tbl$d1 %in% 0:9))
  expect_true(all(tbl$d2 %in% 0:9))
  expect_false(any(is.na(tbl$draw_date)))

  # roughly > 1,000 rows (guardrail; adjust if needed)
  expect_gt(nrow(tbl), 1000)

  # no duplicate (date,time)
  dup <- dplyr::count(tbl, draw_date, time) |> dplyr::filter(n > 1)
  expect_equal(nrow(dup), 0)
})

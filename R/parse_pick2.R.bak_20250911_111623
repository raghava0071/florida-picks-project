suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
})

# Download the current Pick 2 PDF
download_pick2_pdf <- function(dest = "data_raw/pick2.pdf") {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  url <- "https://files.floridalottery.com/exptkt/p2.pdf"
  download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  dest
}

# Parse the PDF into a clean tibble (robust text approach)
parse_pick2_pdf <- function(pdf_path) {
  text  <- pdftools::pdf_text(pdf_path)
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  date_lines <- stringr::str_subset(lines, "^\\d{2}/\\d{2}/\\d{2}")
  m <- stringr::str_match(
    date_lines,
    "^(\\d{2}/\\d{2}/\\d{2})\\s+([ME])\\s+(\\d)\\s*-\\s*(\\d)"
  )
  tibble(
    draw_date = as.Date(m[, 2], format = "%m/%d/%y"),
    time      = m[, 3],
    d1        = as.integer(m[, 4]),
    d2        = as.integer(m[, 5])
  ) |>
    tidyr::drop_na() |>
    dplyr::arrange(draw_date, time)
}

# Quick health checks
check_pick2 <- function(x) {
  list(
    n = nrow(x),
    min_date = min(x$draw_date),
    max_date = max(x$draw_date),
    na_counts = colSums(is.na(x)),
    digits_ok = all(x$d1 %in% 0:9) && all(x$d2 %in% 0:9),
    dup_rows  = nrow(dplyr::count(x, draw_date, time) |> dplyr::filter(n > 1))
  )
}

# Write CSV (auto-create folders)
write_pick2_csv <- function(tbl, clean_path = "data/clean/pick2_history.csv") {
  dir.create(dirname(clean_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tbl, clean_path)
  clean_path
}

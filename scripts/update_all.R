message("Running updater...")
source("R/parse_pick2.R")
df <- parse_pick2()
message("Parsed rows: ", nrow(df))

csv <- "data/clean/pick2_history.csv"
dir.create(dirname(csv), recursive = TRUE, showWarnings = FALSE)
if (file.exists(csv)) {
  old <- readr::read_csv(csv, show_col_types = FALSE)
  df  <- dplyr::bind_rows(old, df) |>
        dplyr::arrange(draw_date, time) |>
        dplyr::distinct(draw_date, time, .keep_all = TRUE)
}
readr::write_csv(df, csv)
message("Wrote ", csv)
print(utils::head(df))

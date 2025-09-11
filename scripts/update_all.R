# scripts/update_all.R

# 1) Move to the project root (folder that has .git or R/)
find_root <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/")
  repeat {
    if (file.exists(file.path(cur, ".git")) || file.exists(file.path(cur, "R"))) {
      setwd(cur); return(invisible(cur))
    }
    parent <- dirname(cur)
    if (parent == cur) { setwd(start); return(invisible(start)) }
    cur <- parent
  }
}
find_root()

# 2) Load parser functions (must be in R/parse_pick2.R)
source("R/parse_pick2.R")

message("== Pick2 update ==")

# 3) Download latest PDF
pdf_path <- download_pick2_pdf("data_raw/pick2.pdf")
message("PDF: ", normalizePath(pdf_path))

# 4) Parse to tibble
pick2 <- parse_pick2_pdf(pdf_path)

# 5) Basic de-dup & ordering
pick2 <- pick2 |>
  dplyr::distinct(draw_date, time, .keep_all = TRUE) |>
  dplyr::arrange(draw_date, time)

# 6) Quick health checks
chk <- check_pick2(pick2)
print(chk)

# 7) Save CSVs
out_path <- write_pick2_csv(pick2, "data/clean/pick2_history.csv")
message("Wrote: ", normalizePath(out_path))

# 8) Preview
print(utils::head(pick2, 10))
print(utils::tail(pick2, 10))

message("Done.")

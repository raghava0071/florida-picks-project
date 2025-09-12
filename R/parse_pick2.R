parse_pick2 <- function(
  url  = "https://files.floridalottery.com/exptkt/p2.pdf",
  dest = "data_raw/pick2.pdf"
) {
  # 1) Download the latest PDF
  if (!file.exists(dest)) dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)

  # 2) Read PDF text via pdftools *or* poppler CLI pdftotext
  read_pdf_text <- function(pdf_path) {
    # Try pdftools if it loads, else use pdftotext CLI
    if (requireNamespace("pdftools", quietly = TRUE)) {
      ok <- TRUE
      txt <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) {ok <<- FALSE; e})
      if (ok) return(txt)
    }
    # Fallback: use poppler CLI
    exe <- Sys.which("pdftotext")
    if (nzchar(exe)) {
      out <- system2(exe, c("-layout","-nopgbrk", pdf_path, "-"), stdout = TRUE, stderr = FALSE)
      return(paste(out, collapse = "\n"))
    }
    stop("Neither pdftools nor pdftotext available. On macOS: `brew install poppler`")
  }

  txt <- read_pdf_text(dest)
  if (is.character(txt) && length(txt) > 1L) txt <- paste(txt, collapse = "\n")
  lines <- unlist(strsplit(txt, "\n"))

  # 3) Keep only lines that start with a date (MM/DD/YY)
  keep <- grepl("^\d{2}/\d{2}/\d{2}", lines)
  date_lines <- lines[keep]

  # 4) Extract date, time (M/E), d1, d2 with regex
  # Handles patterns like: "09/05/25  E  8 - 1   FB 6"
  m <- stringr::str_match_all(date_lines, "^(\d{2}/\d{2}/\d{2})\s+([ME])\s+(\d)\s*[-–]\s*(\d)")

  # 5) Build tibble
  rows <- lapply(m, function(mm) {
    if (!is.null(mm) && nrow(mm) > 0) {
      tibble::tibble(
        draw_date = as.Date(mm[,2], format = "%m/%d/%y"),
        time      = mm[,3],
        d1        = as.integer(mm[,4]),
        d2        = as.integer(mm[,5])
      )
    } else NULL
  })
  df <- dplyr::bind_rows(rows)

  # 6) Clean + de-duplicate: one row per (date,time)
  df <- df |>
    dplyr::arrange(draw_date, time) |>
    dplyr::distinct(draw_date, time, .keep_all = TRUE)

  # basic sanity checks
  stopifnot(all(df$d1 %in% 0:9), all(df$d2 %in% 0:9))
  df
}

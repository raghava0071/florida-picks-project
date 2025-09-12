parse_pick2 <- function(
  url  = "https://files.floridalottery.com/exptkt/p2.pdf",
  dest = "data_raw/pick2.pdf"
) {
  # 1) Download the latest PDF
  if (!file.exists(dest)) dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)

  # 2) Read PDF text via pdftools *or* poppler CLI pdftotext
  read_pdf_text <- function(pdf_path) {
    if (requireNamespace("pdftools", quietly = TRUE)) {
      ok <- TRUE
      txt <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) { ok <<- FALSE; e })
      if (ok) return(txt)
    }
    exe <- Sys.which("pdftotext")
    if (nzchar(exe)) {
      out <- system2(exe, c("-layout","-nopgbrk", pdf_path, "-"), stdout = TRUE, stderr = FALSE)
      return(paste(out, collapse = "\n"))
    }
    stop("Need pdftools or poppler-utils (pdftotext). On macOS: brew install poppler")
  }

  txt <- read_pdf_text(dest)
  if (is.character(txt) && length(txt) > 1L) txt <- paste(txt, collapse = "\n")
  lines <- unlist(strsplit(txt, "\n"))

  # Keep only lines that start with a date (MM/DD/YY)
  date_lines <- grep("^\\d{2}/\\d{2}/\\d{2}", lines, value = TRUE)

  # Extract date, time (M/E), and digits
  m <- stringr::str_match_all(
    date_lines,
    "^(\\d{2}/\\d{2}/\\d{2})\\s+([ME])\\s+(\\d)\\s*[-–]\\s*(\\d)"
  )

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

  df <- dplyr::bind_rows(rows) |>
    dplyr::arrange(draw_date, time) |>
    dplyr::distinct(draw_date, time, .keep_all = TRUE)

  stopifnot(all(df$d1 %in% 0:9), all(df$d2 %in% 0:9))
  df
}


boost_probs_with_fdr <- function(in_csv, out_csv, q = 0.20, boost = 1.05) {
  suppressPackageStartupMessages({library(readr); library(dplyr)})
  if (!file.exists(in_csv)) { message("Missing ", in_csv); return(invisible(NULL)) }

  zs <- readr::read_csv("data/analysis/pair_zscan.csv", show_col_types = FALSE) |>
    mutate(p_fdr = p.adjust(p_two_sided, method = "BH")) |>
    filter(p_fdr <= q) |>
    transmute(pair)

  df <- readr::read_csv(in_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!("prob" %in% names(df)) && "p" %in% names(df)) df <- dplyr::rename(df, prob = p)
  stopifnot(all(c("d1","d2","prob") %in% names(df)))
  if (!"time" %in% names(df)) {
    if (endsWith(in_csv, "_M.csv")) df$time <- "M"
    else if (endsWith(in_csv, "_E.csv")) df$time <- "E"
    else df$time <- "ALL"
  }

  df <- df |>
    mutate(pair = paste0(d1, d2),
           prob_boost = ifelse(pair %in% zs$pair, prob * boost, prob)) |>
    group_by(time) |>
    mutate(prob_boost = prob_boost / sum(prob_boost)) |>
    ungroup() |>
    select(time, d1, d2, pair, prob, prob_boost)

  readr::write_csv(df, out_csv)
  message("Wrote ", out_csv)
  invisible(df)
}

run_boosts <- function() {
  cand <- c(
    "outputs/predictions_backoff_blend_M.csv",
    "outputs/predictions_backoff_blend_E.csv",
    "outputs/predictions_weekday_blend_M.csv",
    "outputs/predictions_weekday_blend_E.csv"
  )
  for (p in cand[file.exists(cand)]) {
    out <- sub("\.csv$", "_boosted.csv", p)
    try(boost_probs_with_fdr(p, out, q = 0.20, boost = 1.05), silent = TRUE)
  }
  invisible(TRUE)
}


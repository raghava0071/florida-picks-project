suppressPackageStartupMessages({
  library(readr); library(dplyr); library(glue); library(tidyr)
})

cat("\n=== Florida Pick 2 — Next Draw Picks ===\n")

# prefer blend outputs; fall back to simple outputs
pred_file <- if (file.exists("outputs/predictions_next_draw_blend.csv"))
  "outputs/predictions_next_draw_blend.csv" else "outputs/predictions_next_draw.csv"

pairs_file <- if (file.exists("outputs/top25_pairs_next_draw_blend.csv"))
  "outputs/top25_pairs_next_draw_blend.csv" else "outputs/top25_pairs_next_draw.csv"

pred <- suppressMessages(read_csv(pred_file, show_col_types = FALSE))
pairs <- suppressMessages(read_csv(pairs_file, show_col_types = FALSE))

# print marginal probabilities (top-3 for each time & position)
for (tm in c("M","E")) {
  row <- pred %>% filter(time == tm)
  if (nrow(row) == 0) next
  d1_cols <- grep("^d1_", names(row), value = TRUE)
  d2_cols <- grep("^d2_", names(row), value = TRUE)
  d1 <- tibble(digit = as.integer(sub("d1_", "", d1_cols)), p = as.numeric(row[1, d1_cols])) %>%
    arrange(desc(p)) %>% slice_head(n=3)
  d2 <- tibble(digit = as.integer(sub("d2_", "", d2_cols)), p = as.numeric(row[1, d2_cols])) %>%
    arrange(desc(p)) %>% slice_head(n=3)
  cat(glue("\n[{tm}] Top D1: {paste(sprintf('%d (%.2f%%)', d1$digit, 100*d1$p), collapse=', ')}\n"))
  cat(glue(  "[{tm}] Top D2: {paste(sprintf('%d (%.2f%%)', d2$digit, 100*d2$p), collapse=', ')}\n"))
}

# print top 10 joint pairs per time
for (tm in c("M","E")) {
  cat(glue("\n[{tm}] Top 10 pairs:\n"))
  pairs %>% filter(time==tm) %>% arrange(desc(p)) %>% slice_head(n=10) %>%
    mutate(pct = sprintf('%.2f%%', 100*p)) %>%
    transmute(pair = paste0(d1, d2), pct) %>%
    print(n=Inf)
}
cat("\n=========================================\n")

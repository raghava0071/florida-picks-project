suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# --- helpers ---------------------------------------------------------------
norm_probs <- function(p_tbl) {
  out <- tibble(digit = 0:9) %>%
    left_join(p_tbl %>% dplyr::select(digit, p), by = "digit") %>%
    mutate(p = replace_na(p, 0))
  if (sum(out$p) <= 0) out$p <- rep(0.1, 10)
  out$p <- out$p / sum(out$p)
  out
}

vec_to_row <- function(p_tbl, prefix) {
  p_tbl <- norm_probs(p_tbl) %>% arrange(digit)
  vals  <- as.list(p_tbl$p)
  nm    <- paste0(prefix, p_tbl$digit)
  tibble::as_tibble(setNames(vals, nm))
}

freq_probs <- function(df, time_code, window_days) {
  last_date <- max(df$draw_date, na.rm = TRUE)
  train <- df %>% filter(time == time_code,
                         draw_date > last_date - window_days,
                         draw_date <= last_date)
  p1 <- train %>% count(d1) %>%
    complete(d1 = 0:9, fill = list(n = 0)) %>%
    arrange(d1) %>% mutate(p = (n + 1) / (sum(n) + 10)) %>%
    transmute(digit = d1, p)
  p2 <- train %>% count(d2) %>%
    complete(d2 = 0:9, fill = list(n = 0)) %>%
    arrange(d2) %>% mutate(p = (n + 1) / (sum(n) + 10)) %>%
    transmute(digit = d2, p)
  list(p1 = norm_probs(p1), p2 = norm_probs(p2))
}

blend_probs <- function(p_short, p_long, w_short = 0.6) {
  stopifnot(all(p_short$digit == 0:9), all(p_long$digit == 0:9))
  tibble(digit = 0:9, p = w_short * p_short$p + (1 - w_short) * p_long$p) %>%
    norm_probs()
}

top_pairs <- function(p1, p2, k = 25) {
  p1 <- norm_probs(p1); p2 <- norm_probs(p2)
  tidyr::expand_grid(d1 = 0:9, d2 = 0:9) %>%
    mutate(p = p1$p[match(d1, p1$digit)] * p2$p[match(d2, p2$digit)]) %>%
    arrange(desc(p)) %>% slice(1:k)
}

predict_next_main <- function() {
  stopifnot(file.exists("data/clean/pick2_history.csv"))
  pick2 <- readr::read_csv("data/clean/pick2_history.csv", show_col_types = FALSE)

  win_short <- 180; win_long <- 365; w_short <- 0.6

  pM_s <- freq_probs(pick2, "M", win_short); pM_l <- freq_probs(pick2, "M", win_long)
  pE_s <- freq_probs(pick2, "E", win_short); pE_l <- freq_probs(pick2, "E", win_long)

  pM_d1 <- blend_probs(pM_s$p1, pM_l$p1, w_short)
  pM_d2 <- blend_probs(pM_s$p2, pM_l$p2, w_short)
  pE_d1 <- blend_probs(pE_s$p1, pE_l$p1, w_short)
  pE_d2 <- blend_probs(pE_s$p2, pE_l$p2, w_short)

  pred_M <- dplyr::bind_cols(tibble(time = "M"),
                             vec_to_row(pM_d1, "d1_"),
                             vec_to_row(pM_d2, "d2_"))
  pred_E <- dplyr::bind_cols(tibble(time = "E"),
                             vec_to_row(pE_d1, "d1_"),
                             vec_to_row(pE_d2, "d2_"))
  preds  <- dplyr::bind_rows(pred_M, pred_E)

  dir.create("outputs", showWarnings = FALSE)
  readr::write_csv(preds, "outputs/predictions_next_draw.csv")

  top_M <- top_pairs(pM_d1, pM_d2, 25) %>% mutate(time = "M") %>% select(time, d1, d2, p)
  top_E <- top_pairs(pE_d1, pE_d2, 25) %>% mutate(time = "E") %>% select(time, d1, d2, p)
  top_both <- dplyr::bind_rows(top_M, top_E)
  readr::write_csv(top_both, "outputs/top25_pairs_next_draw.csv")

  last_date <- max(pick2$draw_date, na.rm = TRUE)
  message("\nPredictions ready for next draw after ", as.character(last_date),
          ". Saved outputs/predictions_next_draw.csv and outputs/top25_pairs_next_draw.csv")
  message("\nTop 10 M:\n"); print(head(top_M, 10))
  message("\nTop 10 E:\n"); print(head(top_E, 10))
}

predict_next_main()

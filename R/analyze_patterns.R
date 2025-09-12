# R/analyze_patterns.R
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(readr); library(stringr); library(purrr); library(tibble)
})

dir.create("outputs", showWarnings = FALSE)
dir.create("data/analysis", showWarnings = FALSE)

# ---------- Load data (prefer cached CSV; else parse) ----------
load_pick2 <- function() {
  csv <- "data/clean/pick2_history.csv"
  if (file.exists(csv)) {
    message("Loading cached clean CSV: ", csv)
    df <- readr::read_csv(csv, show_col_types = FALSE)
  } else {
    message("No cached CSV; parsing from PDF...")
    source("R/parse_pick2.R")
    df <- parse_pick2()
    readr::write_csv(df, csv)
  }
  df %>%
    mutate(time = as.character(time)) %>%
    arrange(draw_date, time)
}

pick2 <- load_pick2()

# ---------- Quick sanity ----------
sanity <- pick2 %>%
  summarise(n = n(),
            min_date = min(draw_date), max_date = max(draw_date),
            na_d1 = sum(is.na(d1)), na_d2 = sum(is.na(d2)),
            ok_d1 = all(d1 %in% 0:9), ok_d2 = all(d2 %in% 0:9))
print(sanity)

stopifnot(sanity$ok_d1, sanity$ok_d2, sanity$na_d1 == 0, sanity$na_d2 == 0)

# Add lags per time bucket (Midday vs Evening separately)
pick2 <- pick2 %>%
  group_by(time) %>%
  arrange(draw_date, .by_group = TRUE) %>%
  mutate(d1_lag1 = dplyr::lag(d1), d2_lag1 = dplyr::lag(d2)) %>%
  ungroup()

# ---------- Metrics ----------
logloss10 <- function(p_mat, y) {
  # p_mat: N x 10 (cols correspond to digits 0..9), rows sum to 1
  # y: integer vector in 0..9
  eps <- 1e-15
  idx <- cbind(seq_along(y), y + 1L)
  -mean(log(pmax(p_mat[idx], eps)))
}
topk_acc <- function(p_mat, y, k=1) {
  preds <- apply(p_mat, 1, function(r) order(r, decreasing=TRUE)[1:k] - 1L)
  if (k == 1) mean(preds == y) else {
    mean(map2_lgl(as.list(as.data.frame(t(preds))), as.list(y), ~ .y %in% .x))
  }
}

# Uniform baseline (no-skill) probabilities
uniform_probs <- matrix(0.1, nrow = nrow(pick2), ncol = 10)

# ---------- Rolling-window Frequency Model ----------
# For each row i, look back W days (same time bucket) and use empirical digit frequencies
rolling_freq_predict <- function(df, which_digit = c("d1","d2"), window_days = 180) {
  which_digit <- match.arg(which_digit)
  res <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    cur_date <- df$draw_date[i]; cur_time <- df$time[i]
    start_date <- cur_date - window_days
    hist <- df %>%
      filter(time == cur_time, draw_date < cur_date, draw_date >= start_date)
    # fallback: if little history, widen window once
    if (nrow(hist) < 20) {
      hist <- df %>% filter(time == cur_time, draw_date < cur_date)
    }
    counts <- table(factor(hist[[which_digit]], levels = 0:9))
    p <- as.numeric(counts + 1)  # Laplace smoothing
    p <- p / sum(p)
    res[[i]] <- p
  }
  do.call(rbind, res)
}

# ---------- Markov (lag-1) Model ----------
# P(next digit | previous digit) estimated from rolling window (same time bucket)
rolling_markov_predict <- function(df, which_digit = c("d1","d2"), window_days = 180) {
  which_digit <- match.arg(which_digit)
  # ensure lag exists in df: d?_lag1
  lag_col <- paste0(which_digit, "_lag1")
  res <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    cur_date <- df$draw_date[i]; cur_time <- df$time[i]
    prev_d <- df[[lag_col]][i]
    # If no lag (first row in bucket), fall back to frequency
    if (is.na(prev_d)) {
      res[[i]] <- rep(0.1, 10)
      next
    }
    start_date <- cur_date - window_days
    hist <- df %>%
      filter(time == cur_time, draw_date < cur_date, draw_date >= start_date)
    if (nrow(hist) < 50) {
      hist <- df %>% filter(time == cur_time, draw_date < cur_date)
    }
    # build transition table
    trans <- hist %>%
      mutate(prev = dplyr::lag(.data[[which_digit]])) %>%
      filter(!is.na(prev)) %>%
      count(prev, next = .data[[which_digit]], .drop = FALSE) %>%
      complete(prev = 0:9, next = 0:9, fill = list(n = 0)) %>%
      group_by(prev) %>%
      mutate(p = (n + 1) / (sum(n) + 10)) %>%  # Laplace smoothing
      ungroup()
    # probabilities for the observed previous digit
    p_vec <- trans %>% filter(prev == prev_d) %>% arrange(next) %>% pull(p)
    if (length(p_vec) != 10 || any(is.na(p_vec))) p_vec <- rep(0.1, 10)
    res[[i]] <- p_vec
  }
  do.call(rbind, res)
}

# ---------- Walk-forward Backtest ----------
# Split chronologically into train/test using a sliding window
walk_forward <- function(df, window_days = 365, horizon_days = 30) {
  dates <- sort(unique(df$draw_date))
  starts <- dates[dates >= (min(dates) + window_days) & dates <= (max(dates) - horizon_days)]
  out <- list()
  for (s in starts) {
    train_end <- s
    test_end  <- s + horizon_days
    tr <- df %>% filter(draw_date > (train_end - window_days) & draw_date <= train_end)
    te <- df %>% filter(draw_date > train_end & draw_date <= test_end)
    
    if (nrow(tr) < 200 || nrow(te) < 20) next
    
    # Predict for test rows using models trained implicitly on 'tr' via rolling functions
    # (We combine train+test to let the predictors look back from each test row)
    tmp <- bind_rows(tr, te) %>% arrange(draw_date, time)
    freq_d1 <- rolling_freq_predict(tmp, "d1", window_days)[(nrow(tr)+1):nrow(tmp), , drop=FALSE]
    freq_d2 <- rolling_freq_predict(tmp, "d2", window_days)[(nrow(tr)+1):nrow(tmp), , drop=FALSE]
    mkv_d1  <- rolling_markov_predict(tmp, "d1", window_days)[(nrow(tr)+1):nrow(tmp), , drop=FALSE]
    mkv_d2  <- rolling_markov_predict(tmp, "d2", window_days)[(nrow(tr)+1):nrow(tmp), , drop=FALSE]
    
    out[[as.character(s)]] <- list(
      test = te,
      freq_d1 = freq_d1, freq_d2 = freq_d2,
      mkv_d1  = mkv_d1,  mkv_d2  = mkv_d2
    )
  }
  out
}

message("Running walk-forward backtest (this may take ~10–30s)...")
wf <- walk_forward(pick2, window_days = 365, horizon_days = 30)

# ---------- Evaluate ----------
eval_rows <- function(wf_list, which = c("freq","mkv"), digit = c("d1","d2")) {
  which <- match.arg(which); digit <- match.arg(digit)
  probs <- do.call(rbind, lapply(wf_list, `[[`, paste0(which, "_", digit)))
  truth <- do.call(c, lapply(wf_list, function(x) x$test[[digit]]))
  list(
    n = length(truth),
    logloss = logloss10(probs, truth),
    top1 = topk_acc(probs, truth, 1),
    top3 = topk_acc(probs, truth, 3)
  )
}

res <- tibble(
  model = c("Uniform_d1","Freq_d1","Markov_d1","Uniform_d2","Freq_d2","Markov_d2"),
  n = NA_integer_, logloss = NA_real_, top1 = NA_real_, top3 = NA_real_
)

# Uniform baselines
# (Use same number of rows as each evaluation for fair comparison)
eval_size_d1 <- sum(vapply(wf, function(x) nrow(x$test), integer(1)))
eval_size_d2 <- eval_size_d1
res$n[res$model=="Uniform_d1"] <- eval_size_d1
res$n[res$model=="Uniform_d2"] <- eval_size_d2
res$logloss[res$model=="Uniform_d1"] <- -mean(log(rep(0.1, eval_size_d1)))
res$logloss[res$model=="Uniform_d2"] <- -mean(log(rep(0.1, eval_size_d2)))
res$top1[res$model %in% c("Uniform_d1","Uniform_d2")] <- 0.10
res$top3[res$model %in% c("Uniform_d1","Uniform_d2")] <- 0.30

# Frequency & Markov
e_fd1 <- eval_rows(wf, "freq", "d1"); e_md1 <- eval_rows(wf, "mkv", "d1")
e_fd2 <- eval_rows(wf, "freq", "d2"); e_md2 <- eval_rows(wf, "mkv", "d2")

res[res$model=="Freq_d1",   2:5] <- unlist(e_fd1)
res[res$model=="Markov_d1", 2:5] <- unlist(e_md1)
res[res$model=="Freq_d2",   2:5] <- unlist(e_fd2)
res[res$model=="Markov_d2", 2:5] <- unlist(e_md2)

print(res %>% arrange(model))

readr::write_csv(res, "data/analysis/backtest_summary.csv")

# ---------- Save a heatmap of Markov residuals (overall) ----------
tab12 <- table(pick2$d1, pick2$d2)
expected <- suppressWarnings(chisq.test(tab12)$expected)
std_res  <- (tab12 - expected) / sqrt(expected)
res_df <- as.data.frame(as.table(std_res))
names(res_df) <- c("d1","d2","std_resid")
res_df$d1 <- as.integer(as.character(res_df$d1))
res_df$d2 <- as.integer(as.character(res_df$d2))

p <- ggplot(res_df, aes(x=factor(d2), y=factor(d1), fill=std_resid)) +
  geom_tile() +
  geom_text(aes(label=sprintf("%.1f", std_resid)), size=3) +
  scale_fill_gradient2(low="#2166ac", mid="white", high="#b2182b", midpoint=0) +
  labs(title="Standardized residuals for (d1,d2) independence",
       x="d2", y="d1", fill="Std resid")
ggsave("outputs/std_resid_heatmap.png", p, width=7, height=6, dpi=160)

message("Done. Saved: data/analysis/backtest_summary.csv and outputs/std_resid_heatmap.png")

setwd("~/Desktop/florida-picks-project")
source("R/analyze_patterns.R")

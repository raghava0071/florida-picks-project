tune_backoff_weights <- function(
  hist_csv = "data/clean/pick2_history.csv",
  out_summary = "outputs/backoff_tuning_summary.txt",
  out_yaml = "data/analysis/weights_backoff.yml",
  grid_weekday = seq(0, 0.7, by = 0.05),
  grid_markov  = seq(0, 0.7, by = 0.05)
) {
  suppressPackageStartupMessages({
    library(dplyr); library(readr); library(tidyr)
  })
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  # basic checks / defaults
  if (!all(c("d1","d2") %in% names(df))) stop("Need d1,d2 in history")
  if (!"time" %in% names(df)) df$time <- "All"
  if (!"draw_date" %in% names(df)) {
    df$draw_date <- seq_len(nrow(df))
  } else {
    df$draw_date <- as.Date(df$draw_date)
  }
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1, d2))

  all_pairs <- sprintf("%d%d", rep(0:9, each=10), rep(0:9, times=10))
  K <- length(all_pairs)
  eps <- 1e-12

  # precompute weekday per row
  df$weekday <- if (inherits(df$draw_date, "Date")) weekdays(df$draw_date) else as.character(df$draw_date)

  # function to compute probs for a given t using data up to t-1
  probs_for_t <- function(past, target_wd, last_pair, alpha_w, alpha_m) {
    # base counts
    base <- past |> count(pair, name="n") |> mutate(p_base = n/sum(n)) |> select(pair, p_base)
    # conditional weekday x time
    cond <- past |> count(weekday, time, pair, name="n") |>
      group_by(weekday, time) |> mutate(p_cond = n/sum(n)) |> ungroup() |>
      filter(weekday == target_wd) |> select(time, pair, p_cond)
    # grid
    grid <- tidyr::expand_grid(time = unique(past$time), pair = all_pairs) |>
      left_join(base, by="pair") |>
      left_join(cond, by=c("time","pair")) |>
      mutate(p_base = tidyr::replace_na(p_base, 0),
             p_cond = tidyr::replace_na(p_cond, 0)) |>
      mutate(p_bw = (1 - alpha_w) * p_base + alpha_w * p_cond,
             p_bw = pmax(p_bw, eps)) |>
      group_by(time) |> mutate(p_bw = p_bw / sum(p_bw)) |> ungroup()
    # markov (first-order)
    trans <- past |>
      transmute(last = dplyr::lag(pair), next_pair = pair) |>
      filter(!is.na(last)) |>
      count(last, next_pair, name = "n") |>
      group_by(last) |>
      mutate(p = (n + 1) / (sum(n) + K)) |>  # Laplace smoothing
      ungroup()
    if (!is.na(last_pair) && last_pair %in% trans$last) {
      mrow <- trans |> filter(last == last_pair) |> select(next_pair, p) |>
        rename(pair = next_pair, p_markov = p)
      grid <- grid |> left_join(mrow, by="pair") |>
        mutate(p_markov = tidyr::replace_na(p_markov, 0),
               p_raw = (1 - alpha_m) * p_bw + alpha_m * p_markov,
               p_raw = pmax(p_raw, eps)) |>
        group_by(time) |> mutate(p_raw = p_raw / sum(p_raw)) |> ungroup()
    } else {
      grid <- grid |> mutate(p_raw = p_bw)
    }
    grid
  }

  # walk forward (train-on-past → predict current)
  # returns a data.frame of winning-prob per draw for a given (alpha_w, alpha_m)
  backtest_once <- function(alpha_w, alpha_m) {
    ll <- numeric(nrow(df) - 1)
    ct <- 1L
    for (t in 2:nrow(df)) {
      past <- df[1:(t-1), , drop = FALSE]
      target_wd <- df$weekday[t]
      last_pair <- df$pair[t-1]
      grid <- probs_for_t(past, target_wd, last_pair, alpha_w, alpha_m)
      # choose the row matching the actual winning pair & time at t
      time_t <- df$time[t]
      pair_t <- df$pair[t]
      p_t <- grid |> filter(time == time_t, pair == pair_t) |> pull(p_raw)
      if (length(p_t) == 0) p_t <- 1/K
      ll[ct] <- -log(pmax(p_t, eps))
      ct <- ct + 1L
    }
    mean(ll)
  }

  # grid search
  results <- expand.grid(alpha_weekday = grid_weekday, alpha_markov = grid_markov)
  results$logloss <- NA_real_
  for (i in seq_len(nrow(results))) {
    aw <- results$alpha_weekday[i]
    am <- results$alpha_markov[i]
    results$logloss[i] <- backtest_once(aw, am)
  }
  results <- results |> arrange(logloss)
  best <- results[1, ]

  # write outputs
  dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
  writeLines(
    c(
      sprintf("Best weights (lower log-loss is better):"),
      sprintf("alpha_weekday = %.2f", best$alpha_weekday),
      sprintf("alpha_markov  = %.2f", best$alpha_markov),
      sprintf("cv log-loss   = %.6f", best$logloss)
    ),
    out_summary
  )

  # save yaml (install yaml if needed)
  if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml", quiet = TRUE)
  yaml::write_yaml(
    list(alpha_weekday = as.numeric(best$alpha_weekday),
         alpha_markov  = as.numeric(best$alpha_markov)),
    out_yaml
  )
  message("Wrote: ", out_summary, " and ", out_yaml)
  invisible(best)
}

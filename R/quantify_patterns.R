
# R/quantify_patterns.R
# Quantify how much pattern signal we capture.

quantify_patterns <- function(
  hist_csv = "data/clean/pick2_history.csv",
  out_csv  = "data/analysis/patterns_quant_summary.csv",
  out_png  = "outputs/topk_curves.png",
  weights_yaml = "data/analysis/weights_backoff.yml"
) {
  suppressPackageStartupMessages({
    library(dplyr); library(readr); library(tidyr); library(ggplot2)
  })
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!all(c("d1","d2") %in% names(df))) stop("Need d1,d2 in history")
  if (!"time" %in% names(df)) df$time <- "All"
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2),
                                           weekday = if (inherits(draw_date,"Date")) weekdays(draw_date) else as.character(draw_date))

  all_pairs <- sprintf("%d%d", rep(0:9, each=10), rep(0:9, times=10))
  K <- length(all_pairs); eps <- 1e-12

  make_base_probs <- function(past) {
    base <- past |> count(pair, name="n") |> mutate(p = n/sum(n))
    tibble(pair = all_pairs) |> left_join(base, by="pair") |> mutate(p = tidyr::replace_na(p, 0))
  }
  make_weekday_probs <- function(past, target_wd) {
    cond <- past |>
      count(weekday, time, pair, name="n") |>
      group_by(weekday, time) |> mutate(p = n/sum(n)) |> ungroup() |>
      filter(weekday == target_wd) |>
      select(time, pair, p)
    tidyr::expand_grid(time = unique(past$time), pair = all_pairs) |>
      left_join(cond, by=c("time","pair")) |>
      mutate(p = tidyr::replace_na(p, 0))
  }
  make_markov <- function(past) {
    past |>
      transmute(last = dplyr::lag(pair), next_pair = pair) |>
      filter(!is.na(last)) |>
      count(last, next_pair, name="n") |>
      group_by(last) |>
      mutate(p = (n + 1) / (sum(n) + K)) |>  # Laplace
      ungroup()
  }

  # tuned weights (fallback to defaults)
  alpha_w <- 0.40; alpha_m <- 0.35
  if (file.exists(weights_yaml)) {
    if (!requireNamespace("yaml", quietly = TRUE)) install.packages("yaml", quiet = TRUE)
    w <- yaml::read_yaml(weights_yaml)
    if (!is.null(w$alpha_weekday)) alpha_w <- as.numeric(w$alpha_weekday)
    if (!is.null(w$alpha_markov))  alpha_m <- as.numeric(w$alpha_markov)
  }

  recs <- list()
  for (t in 2:nrow(df)) {
    past <- df[1:(t-1), , drop = FALSE]
    wd_t <- df$weekday[t]
    time_t <- df$time[t]
    pair_t <- df$pair[t]
    last_pair <- df$pair[t-1]

    # Uniform (FIX: use expand_grid so column lengths match)
    pu <- tidyr::expand_grid(time = unique(past$time), pair = all_pairs) |> mutate(p = 1/K)

    # Base
    pb <- make_base_probs(past) |>
      mutate(time = list(unique(past$time))) |>
      tidyr::unnest(time)

    # Weekday only
    pw <- make_weekday_probs(past, wd_t)

    # Backoff = blend(base, weekday) then Markov pull
    pbw <- pb |> rename(p_base = p) |>
      left_join(pw |> rename(p_cond = p), by = c("time","pair")) |>
      mutate(p_cond = tidyr::replace_na(p_cond, 0),
             p = (1 - alpha_w) * p_base + alpha_w * p_cond,
             p = pmax(p, eps)) |>
      group_by(time) |> mutate(p = p / sum(p)) |> ungroup()

    mk <- make_markov(past)
    if (nrow(mk) > 0 && last_pair %in% mk$last) {
      pull <- mk |> filter(last == last_pair) |> select(next_pair, p) |> rename(pair = next_pair, p_markov = p)
      pbo <- pbw |> left_join(pull, by="pair") |>
        mutate(p_markov = tidyr::replace_na(p_markov, 0),
               p = (1 - alpha_m) * p + alpha_m * p_markov,
               p = pmax(p, eps)) |>
        group_by(time) |> mutate(p = p / sum(p)) |> ungroup()
    } else {
      pbo <- pbw
    }

    pick <- function(tbl) {
      val <- tbl |> filter(time == time_t, pair == pair_t) |> pull(p)
      if (length(val)==0) 1/K else val[1]
    }
    recs[[length(recs)+1]] <- tibble(
      t = t,
      model = c("Uniform","Base","Weekday","Backoff"),
      prob = c(pick(pu), pick(pb), pick(pw), pick(pbo))
    )
  }

  bt <- bind_rows(recs)
  metrics <- bt |>
    mutate(ll = -log(pmax(prob, eps)), brier = (1 - prob)^2) |>
    group_by(model) |>
    summarize(
      draws = dplyr::n(),
      logloss = mean(ll),
      brier   = mean(brier),
      .groups = "drop"
    ) |>
    arrange(logloss)

  # gain vs Uniform
  ll_uniform <- metrics |> filter(model=="Uniform") |> pull(logloss)
  if (length(ll_uniform)==1) {
    metrics <- metrics |>
      mutate(gain_vs_uniform = (ll_uniform - logloss)/ll_uniform)
  }

  dir.create("data/analysis", showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(metrics, out_csv)

  # Top-K coverage for Backoff only (K=1,10,25)
  topk_eval <- function(Kk) {
    hits <- 0L; total <- 0L
    for (t in 2:nrow(df)) {
      past <- df[1:(t-1), , drop = FALSE]
      wd_t <- df$weekday[t]; time_t <- df$time[t]; pair_t <- df$pair[t]; last_pair <- df$pair[t-1]
      pb <- make_base_probs(past) |> mutate(time = list(unique(past$time))) |> tidyr::unnest(time)
      pw <- make_weekday_probs(past, wd_t)
      pbw <- pb |> rename(p_base = p) |>
        left_join(pw |> rename(p_cond = p), by = c("time","pair")) |>
        mutate(p_cond = tidyr::replace_na(p_cond, 0),
               p = (1 - alpha_w) * p_base + alpha_w * p_cond,
               p = pmax(p, eps)) |>
        group_by(time) |> mutate(p = p / sum(p)) |> ungroup()
      mk <- make_markov(past)
      if (nrow(mk) > 0 && last_pair %in% mk$last) {
        pull <- mk |> filter(last == last_pair) |> select(next_pair, p) |> rename(pair = next_pair, p_markov = p)
        pbo <- pbw |> left_join(pull, by="pair") |>
          mutate(p_markov = tidyr::replace_na(p_markov, 0),
                 p = (1 - alpha_m) * p + alpha_m * p_markov,
                 p = pmax(p, eps)) |>
          group_by(time) |> mutate(p = p / sum(p)) |> ungroup()
      } else { pbo <- pbw }
      top <- pbo |> filter(time == time_t) |> arrange(desc(p)) |> slice_head(n = Kk)
      hits <- hits + as.integer(any(top$pair == pair_t)); total <- total + 1L
    }
    tibble(K = Kk, hit_rate = hits/total)
  }
  topks <- bind_rows(topk_eval(1), topk_eval(10), topk_eval(25))

  ggplot(topks, aes(K, hit_rate)) +
    geom_line() + geom_point() +
    scale_x_continuous(breaks = c(1,10,25)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Backoff: Top-K hit rate (walk-forward)",
         x = "K", y = "Hit rate") +
    theme_minimal()
  ggsave(out_png, width = 6.5, height = 4.2, dpi = 150)

  message("Wrote: ", out_csv, " and ", out_png)
  invisible(list(summary = metrics, topk = topks))
}


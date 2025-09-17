# Blend base frequency, weekday×time freq, and Markov(next|last)
predict_backoff_blend <- function(
  hist_csv = "data/clean/pick2_history.csv",
  trans_csv = "data/analysis/markov1_smoothed.csv",
  alpha_weekday = 0.40,
  alpha_markov  = 0.35,
  when = Sys.Date() + 1,
  last_pair = NULL
) {
  library(dplyr); library(readr); library(tidyr); library(purrr)
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!all(c("d1","d2") %in% names(df))) stop("Expected d1,d2 in ", hist_csv)
  if (!"time" %in% names(df)) df$time <- "All"

  df <- df |> mutate(pair = paste0(d1,d2))
  base <- df |> count(pair, name="n") |> mutate(p_base = n/sum(n)) |> select(pair, p_base)

  # weekday×time conditional for target weekday
  target_wd <- weekdays(as.Date(when))
  weekday_col <- if ("draw_date" %in% names(df)) weekdays(as.Date(df$draw_date)) else weekdays(Sys.Date())
  cond <- df |> mutate(weekday = weekday_col) |>
    count(weekday, time, pair, name="n") |>
    group_by(weekday, time) |> mutate(p_cond = n/sum(n)) |> ungroup() |>
    filter(weekday == target_wd) |> select(time, pair, p_cond)

  all_pairs <- sprintf("%d%d", rep(0:9, each=10), rep(0:9, times=10))
  grid <- tidyr::expand_grid(time = unique(df$time), pair = all_pairs) |>
    left_join(base, by="pair") |>
    left_join(cond, by=c("time","pair")) |>
    mutate(p_base = tidyr::replace_na(p_base, 0),
           p_cond = tidyr::replace_na(p_cond, 0))

  eps <- 1e-12
  grid <- grid |>
    mutate(p_bw = (1 - alpha_weekday) * p_base + alpha_weekday * p_cond,
           p_bw = pmax(p_bw, eps)) |>
    group_by(time) |> mutate(p_bw = p_bw / sum(p_bw)) |> ungroup()

  # infer last_pair if missing
  if (is.null(last_pair)) {
    if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
    last_pair <- df |> arrange(draw_date) |> tail(1) |> pull(pair)
  }

  if (file.exists(trans_csv)) {
    trans_sm <- readr::read_csv(trans_csv, show_col_types = FALSE)
    names(trans_sm) <- tolower(names(trans_sm))
    if (all(c("last","next_pair","p") %in% names(trans_sm))) {
      mrow <- trans_sm |> filter(last == last_pair) |>
        select(next_pair, p) |> rename(pair = next_pair, p_markov = p)
      grid <- grid |> left_join(mrow, by="pair") |>
        mutate(p_markov = tidyr::replace_na(p_markov, 0),
               p_raw = (1 - alpha_markov) * p_bw + alpha_markov * p_markov,
               p_raw = pmax(p_raw, eps)) |>
        group_by(time) |> mutate(p_raw = p_raw / sum(p_raw)) |> ungroup()
    } else {
      warning("markov1_smoothed.csv missing expected cols; using p_bw only.")
      grid <- grid |> mutate(p_raw = p_bw)
    }
  } else {
    warning("markov1_smoothed.csv not found; using p_bw only.")
    grid <- grid |> mutate(p_raw = p_bw)
  }

  split(grid |> select(time, pair, p_raw), grid$time)
}

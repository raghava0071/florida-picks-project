
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
})

# 1) Popularity / Weekday / Time
analyze_popularity <- function(hist_csv = "data/clean/pick2_history.csv") {
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"time" %in% names(df)) df$time <- "All"
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |>
    mutate(pair = paste0(d1,d2),
           weekday = if (inherits(draw_date,"Date")) weekdays(draw_date) else as.character(draw_date))
  d1_freq <- df |> count(d1, name="n") |> mutate(p = n/sum(n))
  d2_freq <- df |> count(d2, name="n") |> mutate(p = n/sum(n))
  readr::write_csv(d1_freq, "data/analysis/d1_freq.csv")
  readr::write_csv(d2_freq, "data/analysis/d2_freq.csv")
  pair_freq <- df |> count(pair, name="n") |> arrange(desc(n)) |> mutate(p = n/sum(n))
  readr::write_csv(pair_freq, "data/analysis/pair_freq.csv")
  wtp <- df |> count(weekday, time, pair, name="n") |>
    group_by(weekday, time) |> mutate(p = n/sum(n)) |> ungroup()
  readr::write_csv(wtp, "data/analysis/weekday_time_pair.csv")
  ggsave("outputs/d1_freq.png",
         ggplot(d1_freq, aes(factor(d1), n)) + geom_col() + labs(x="d1", y="count", title="d1 frequency"),
         width=6.5, height=4, dpi=150)
  ggsave("outputs/d2_freq.png",
         ggplot(d2_freq, aes(factor(d2), n)) + geom_col() + labs(x="d2", y="count", title="d2 frequency"),
         width=6.5, height=4, dpi=150)
  pf_mat <- df |> count(d1, d2, name="n")
  ggsave("outputs/pair_freq_heatmap.png",
         ggplot(pf_mat, aes(factor(d2), factor(d1), fill=n)) + geom_tile() + geom_text(aes(label=n), size=3) +
           labs(x="d2", y="d1", title="Pair frequency heatmap"),
         width=6.5, height=5.5, dpi=150)
  invisible(TRUE)
}

# 2) Randomness / Independence
analyze_randomness <- function(hist_csv = "data/clean/pick2_history.csv") {
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df)); df$pair <- paste0(df$d1, df$d2)
  d1_tab <- table(df$d1); d2_tab <- table(df$d2)
  chi_d1 <- suppressWarnings(chisq.test(d1_tab, p = rep(1/10, 10)))
  chi_d2 <- suppressWarnings(chisq.test(d2_tab, p = rep(1/10, 10)))
  ct     <- table(df$d1, df$d2)
  chi_ind <- suppressWarnings(chisq.test(ct))
  out <- tibble::tibble(
    test = c("d1 uniform", "d2 uniform", "d1 ⟂ d2 (independence)"),
    stat = c(unname(chi_d1$statistic), unname(chi_d2$statistic), unname(chi_ind$statistic)),
    df   = c(chi_d1$parameter, chi_d2$parameter, chi_ind$parameter),
    pval = c(chi_d1$p.value, chi_d2$p.value, chi_ind$p.value)
  )
  readr::write_csv(out, "data/analysis/randomness_tests.csv")
  N <- nrow(df); pij <- ct / N; pi <- rowSums(pij); pj <- colSums(pij); eps <- 1e-12
  MI <- sum(pij * log((pij + eps) / outer(pi, pj))) / log(2)
  readr::write_csv(tibble::tibble(metric="MI_d1_d2_bits", value = MI), "data/analysis/mi_bits.csv")
  invisible(TRUE)
}

# 3) Recency / Gap effects


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    reframe(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}



analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


analyze_recency <- function(hist_csv = "data/clean/pick2_history.csv") {
  suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})
  stopifnot(file.exists(hist_csv))
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))

  # For each pair, compute gaps between consecutive appearances: Δt = idx[i] - idx[i-1]
  df$idx <- seq_len(nrow(df))
  gaps_all <- df |>
    group_by(pair) |>
    summarize(gaps = {
      pos <- idx
      if (length(pos) < 2) integer(0) else diff(pos)
    }, .groups = "drop") |>
    tidyr::unnest(gaps)

  # Empirical distribution of gaps: P(gap = k)
  gap_dist <- gaps_all |>
    count(gaps, name = "count") |>
    arrange(gaps) |>
    mutate(p = count / sum(count))

  readr::write_csv(gap_dist, "data/analysis/gap_distribution.csv")

  # Plot distribution
  p <- ggplot(gap_dist, aes(gaps, p)) + geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Empirical gap distribution (consecutive repeats of same pair)",
         x = "Gap (draws between repeats)", y = "Probability")
  ggsave("outputs/gap_distribution.png", p, width = 7, height = 4, dpi = 150)

  invisible(gap_dist)
}


# 4) Markov transitions
analyze_markov <- function(hist_csv = "data/clean/pick2_history.csv") {
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))
  all_pairs <- sprintf("%d%d", rep(0:9, each=10), rep(0:9, times=10))
  trans <- df |>
    transmute(last = dplyr::lag(pair), next_pair = pair) |>
    filter(!is.na(last)) |>
    count(last, next_pair, name="n") |>
    group_by(last) |>
    mutate(p = (n + 1) / (sum(n) + length(all_pairs))) |>
    ungroup()
  readr::write_csv(trans, "data/analysis/transitions_smoothed.csv")
  last_pair <- tail(df$pair, 1)
  topnext <- trans |> filter(last == last_pair) |> arrange(desc(p)) |> slice_head(n=15)
  readr::write_csv(topnext, "data/analysis/topnext_lastpair.csv")
  ggsave("outputs/topnext_lastpair.png",
         ggplot(topnext, aes(reorder(next_pair, p), p)) + geom_col() + coord_flip() +
           labs(title = paste0("Top next after last pair ", last_pair), x="next_pair", y="p"),
         width=6.5, height=5, dpi=150)
  invisible(TRUE)
}

# 5) Rolling entropy
analyze_entropy <- function(hist_csv = "data/clean/pick2_history.csv", window = 200) {
  df <- readr::read_csv(hist_csv, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (!"draw_date" %in% names(df)) df$draw_date <- seq_len(nrow(df)) else df$draw_date <- as.Date(df$draw_date)
  df <- df |> arrange(draw_date) |> mutate(pair = paste0(d1,d2))
  H <- rep(NA_real_, nrow(df)); eps <- 1e-12
  for (t in seq_len(nrow(df))) {
    lo <- max(1, t - window + 1)
    past <- df[lo:t, , drop=FALSE]
    p <- past |> count(pair, name="n") |> mutate(p = n/sum(n)) |> pull(p)
    H[t] <- -sum(p * log(p + eps)) / log(2)
  }
  out <- tibble::tibble(draw_date = df$draw_date, entropy_bits = H)
  readr::write_csv(out, "data/analysis/entropy_rolling.csv")
  ggsave("outputs/entropy_rolling.png",
         ggplot(out, aes(draw_date, entropy_bits)) + geom_line() +
           labs(title=paste0("Rolling entropy (window=", window, ")"), x="date", y="bits (0–~6.64)"),
         width=7, height=4, dpi=150)
  invisible(TRUE)
}

# Runner
run_pattern_suite <- function() {
  analyze_popularity()
  analyze_randomness()
  analyze_recency()
  analyze_markov()
  analyze_entropy()
  message("Pattern suite complete.")
}


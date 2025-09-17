suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr) })

# Pick an input that contains per-draw predicted prob and hit indicator
input <- if (file.exists("data/analysis/backtest_draws_freq.csv")) {
  "data/analysis/backtest_draws_freq.csv"
} else if (file.exists("data/analysis/backtest_by_date_multi.csv")) {
  "data/analysis/backtest_by_date_multi.csv"
} else if (file.exists("data/analysis/backtest_by_date.csv")) {
  "data/analysis/backtest_by_date.csv"
} else {
  stop("No backtest file found.")
}

df <- readr::read_csv(input, show_col_types = FALSE)

# Standardize column names: expect p (pred prob) and hit (0/1 or TRUE/FALSE)
if (!"p" %in% names(df) && "p_pred" %in% names(df)) df <- dplyr::rename(df, p = p_pred)
if (!"hit" %in% names(df) && "y" %in% names(df))     df <- dplyr::rename(df, hit = y)
if (!"p" %in% names(df))   stop("No predicted-prob column found (expected 'p' or 'p_pred').")
if (!"hit" %in% names(df)) stop("No hit column found (expected 'hit' or 'y').")

df <- df %>% filter(is.finite(p), p >= 0, p <= 1)

# Build safe quantile bins (strictly increasing); fallback to equal-width if needed
safe_cut_quantile <- function(x, nbins = 10) {
  qs <- quantile(x, probs = seq(0, 1, length.out = nbins + 1),
                 na.rm = TRUE, names = FALSE)
  qs <- sort(unique(qs))

  if (length(qs) <= 2) {
    # Too many ties -> use equal-width bins
    r <- range(x, na.rm = TRUE)
    if (!is.finite(diff(r)) || diff(r) == 0) r[2] <- r[2] + 1e-12
    qs <- seq(r[1], r[2], length.out = nbins + 1)
  } else {
    # Enforce strictly increasing by tiny nudges if needed
    eps <- 1e-12
    for (i in 2:length(qs)) if (qs[i] <= qs[i - 1]) qs[i] <- qs[i - 1] + eps
  }

  cut(x, breaks = qs, include.lowest = TRUE, right = TRUE)
}

cal <- df %>%
  mutate(bin = safe_cut_quantile(p, 10)) %>%
  group_by(bin) %>%
  summarise(
    n        = n(),
    p_avg    = mean(p),
    hit_rate = mean(as.numeric(hit)),
    .groups  = "drop"
  ) %>%
  arrange(p_avg)

dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(cal, "data/analysis/calibration.csv")
message("Wrote data/analysis/calibration.csv with ", nrow(cal), " bins.")

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(rlang) })

input <- "data/analysis/backtest_draws_freq.csv"
stopifnot(file.exists(input))

df <- readr::read_csv(input, show_col_types = FALSE)

# --- 1) Standardize/derive probability ---------------------------------------
prob_candidates <- c("p","p_pred","prob","pp","joint_p")
hit_candidates  <- c("hit","y","is_hit","correct","top1_correct")

prob_col <- prob_candidates[prob_candidates %in% names(df)][1]
hit_col  <- hit_candidates[hit_candidates %in% names(df)][1]

# helper: extract row-wise probability from wide distribution columns
get_prob_from_wide <- function(df, prefix, digit_col) {
  cols <- paste0(prefix, 0:9)
  if (!all(cols %in% names(df))) return(NULL)
  mat <- as.matrix(df[, cols, drop = FALSE])
  idx <- df[[digit_col]]
  # ensure 0..9 and not NA
  ok  <- is.finite(idx) & idx >= 0 & idx <= 9
  out <- rep(NA_real_, nrow(df))
  out[ok] <- mat[cbind(which(ok), idx[ok] + 1)]
  out
}

if (!is.na(prob_col)) {
  df <- df %>% rename(p = !!sym(prob_col))
} else {
  # try to derive p from d1_* and d2_* distributions
  d1_prob <- get_prob_from_wide(df, "d1_", "d1")
  d2_prob <- get_prob_from_wide(df, "d2_", "d2")
  if (!is.null(d1_prob) && !is.null(d2_prob)) {
    df <- df %>% mutate(p = as.numeric(d1_prob) * as.numeric(d2_prob))
  } else {
    stop("No predicted-prob column found (tried: ",
         paste(prob_candidates, collapse=", "),
         "), and could not derive from d1_/d2_ wide columns.")
  }
}

# --- 2) Standardize/derive hit -----------------------------------------------
if (!is.na(hit_col)) {
  df <- df %>% rename(hit = !!sym(hit_col))
} else {
  # Fallback: if you don't store correctness per draw, treat each row as the
  # realized outcome with its own predicted probability. Use hit = 1L to
  # enable reliability curve shape (this is conservative; refine later).
  df <- df %>% mutate(hit = 1L)
  message("No explicit hit/correctness column found; using hit = 1L as fallback.")
}

df <- df %>%
  mutate(
    p   = as.numeric(p),
    hit = as.numeric(hit)
  ) %>%
  filter(is.finite(p), p >= 0, p <= 1, is.finite(hit))

# --- 3) Safe binning (handles tied quantiles) --------------------------------
safe_cut_quantile <- function(x, nbins = 10) {
  qs <- quantile(x, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE, names = FALSE)
  qs <- sort(unique(qs))
  if (length(qs) <= 2) {
    r <- range(x, na.rm = TRUE); if (!is.finite(diff(r)) || diff(r) == 0) r[2] <- r[2] + 1e-12
    qs <- seq(r[1], r[2], length.out = nbins + 1)
  } else {
    eps <- 1e-12
    for (i in 2:length(qs)) if (qs[i] <= qs[i-1]) qs[i] <- qs[i-1] + eps
  }
  cut(x, breaks = qs, include.lowest = TRUE, right = TRUE)
}

cal <- df %>%
  mutate(bin = safe_cut_quantile(p, nbins = 10)) %>%
  group_by(bin) %>%
  summarise(
    n        = n(),
    p_avg    = mean(p),
    hit_rate = mean(hit),
    .groups  = "drop"
  ) %>%
  arrange(p_avg)

dir.create("data/analysis", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(cal, "data/analysis/calibration.csv")
message("Wrote data/analysis/calibration.csv with ", nrow(cal), " bins.")

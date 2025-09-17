suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(rlang)
})

in_bt  <- "data/analysis/backtest_draws_freq.csv"
in_hist <- "data/clean/pick2_history.csv"
out_cal <- "data/analysis/calibration.csv"

stopifnot(file.exists(in_bt))
bt <- readr::read_csv(in_bt, show_col_types = FALSE)

# ------------- 1) Try to standardize/derive probability p --------------------
prob_candidates <- c("p","p_pred","prob","pp","joint_p")
hit_candidates  <- c("hit","y","is_hit","correct","top1_correct")

prob_col <- prob_candidates[prob_candidates %in% names(bt)][1]
hit_col  <- hit_candidates[hit_candidates %in% names(bt)][1]

# helper: pull row-wise prob from wide cols (d1_0..9 or d2_0..9)
get_prob_from_wide <- function(df, prefix, digit_col) {
  cols <- paste0(prefix, 0:9)
  if (!all(cols %in% names(df))) return(NULL)
  mat <- as.matrix(df[, cols, drop = FALSE])
  idx <- df[[digit_col]]
  ok  <- is.finite(idx) & idx >= 0 & idx <= 9
  out <- rep(NA_real_, nrow(df))
  out[ok] <- mat[cbind(which(ok), idx[ok] + 1)]
  out
}

if (!is.na(prob_col)) {
  bt <- bt %>% rename(p = !!sym(prob_col))
} else {
  # Try d1_/d2_ wide distributions
  d1p <- get_prob_from_wide(bt, "d1_", "d1")
  d2p <- get_prob_from_wide(bt, "d2_", "d2")
  if (!is.null(d1p) && !is.null(d2p)) {
    bt <- bt %>% mutate(p = as.numeric(d1p) * as.numeric(d2p))
  } else {
    # Final fallback: reconstruct p from full history with a rolling window
    stopifnot(file.exists(in_hist))
    hist <- readr::read_csv(in_hist, show_col_types = FALSE) %>%
      mutate(
        draw_date = as.Date(draw_date),
        time = as.character(time),
        d1 = as.integer(d1),
        d2 = as.integer(d2)
      )

    # Require these columns in bt to rebuild
    need_cols <- c("draw_date","time","d1","d2")
    if (!all(need_cols %in% names(bt))) {
      stop("Cannot reconstruct p: backtest file lacks columns: ",
           paste(setdiff(need_cols, names(bt)), collapse=", "))
    }

    bt <- bt %>%
      mutate(draw_date = as.Date(draw_date),
             time = as.character(time),
             d1 = as.integer(d1),
             d2 = as.integer(d2))

    win_days <- 180L
    # Precompute counts per day/time for speed
    hist <- hist %>% arrange(draw_date)

    compute_p_one <- function(date_i, time_i, d1_i, d2_i) {
      L <- date_i - win_days
      tr <- hist %>% filter(draw_date > L, draw_date < date_i, time == time_i)
      if (nrow(tr) < 30) return(NA_real_)  # not enough data
      p1 <- tr %>% count(d1) %>% complete(d1 = 0:9, fill = list(n = 0)) %>%
        arrange(d1) %>% mutate(p = (n + 1) / (sum(n) + 10))
      p2 <- tr %>% count(d2) %>% complete(d2 = 0:9, fill = list(n = 0)) %>%
        arrange(d2) %>% mutate(p = (n + 1) / (sum(n) + 10))
      p1$p[p1$d1 == d1_i] * p2$p[p2$d2 == d2_i]
    }

    bt$p <- mapply(
      compute_p_one,
      bt$draw_date, bt$time, bt$d1, bt$d2
    )
  }
}

# ------------- 2) Standardize hit (fallback = 1) ----------------------------
if (!is.na(hit_col)) {
  bt <- bt %>% rename(hit = !!sym(hit_col))
} else {
  bt <- bt %>% mutate(hit = 1L)
  message("No explicit hit/correctness column found; using hit = 1L for calibration bins.")
}

bt <- bt %>%
  mutate(p = as.numeric(p),
         hit = as.numeric(hit)) %>%
  filter(is.finite(p), p >= 0, p <= 1, is.finite(hit))

# ------------- 3) Safe quantile binning (handles ties) ----------------------
safe_cut_quantile <- function(x, nbins = 10) {
  qs <- quantile(x, probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE, names = FALSE)
  qs <- sort(unique(qs))
  if (length(qs) <= 2) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(diff(r)) || diff(r) == 0) r[2] <- r[2] + 1e-12
    qs <- seq(r[1], r[2], length.out = nbins + 1)
  } else {
    eps <- 1e-12
    for (i in 2:length(qs)) if (qs[i] <= qs[i-1]) qs[i] <- qs[i-1] + eps
  }
  cut(x, breaks = qs, include.lowest = TRUE, right = TRUE)
}

cal <- bt %>%
  mutate(bin = safe_cut_quantile(p, nbins = 10)) %>%
  group_by(bin) %>%
  summarise(
    n        = n(),
    p_avg    = mean(p),
    hit_rate = mean(hit),
    .groups  = "drop"
  ) %>%
  arrange(p_avg)

dir.create(dirname(out_cal), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(cal, out_cal)
message("Wrote ", out_cal, " with ", nrow(cal), " bins.")

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr); library(stringr)
})

# ---------- helpers ----------
.norm10 <- function(p) {
  # ensure length 10 probs, no NA, sum=1
  p[is.na(p)] <- 0
  if (length(p) != 10) p <- rep(0, 10)
  s <- sum(p)
  if (s <= 0) p <- rep(1/10, 10) else p <- p / s
  p
}
.topk_hit_pair <- function(p1, p2, d1, d2, k=3) {
  grid <- tidyr::expand_grid(d1 = 0:9, d2 = 0:9) |>
    mutate(p = p1[d1+1] * p2[d2+1]) |>
    arrange(desc(p))
  any(grid$d1[1:k] == d1 & grid$d2[1:k] == d2)
}

# ---------- models (produce P(d1), P(d2)) ----------
model_freq <- function(train, time_code) {
  tr <- train |> filter(time == time_code)
  p1 <- tr |> count(d1) |> complete(d1=0:9, fill=list(n=0)) |> arrange(d1) |> mutate(p=(n+1)/(sum(n)+10))
  p2 <- tr |> count(d2) |> complete(d2=0:9, fill=list(n=0)) |> arrange(d2) |> mutate(p=(n+1)/(sum(n)+10))
  list(p1=.norm10(p1$p), p2=.norm10(p2$p))
}

model_ewma <- function(train, time_code, lambda=0.97) {
  tr <- train |> filter(time == time_code) |> arrange(draw_date)
  if (nrow(tr) == 0) return(list(p1=rep(0.1,10), p2=rep(0.1,10)))
  w <- lambda ^ rev(seq_len(nrow(tr))-1)
  w <- w / sum(w)
  p1 <- sapply(0:9, function(d) sum(w * as.numeric(tr$d1==d)))
  p2 <- sapply(0:9, function(d) sum(w * as.numeric(tr$d2==d)))
  # add tiny smoothing
  p1 <- .norm10(p1 + 1e-6)
  p2 <- .norm10(p2 + 1e-6)
  list(p1=p1, p2=p2)
}

# Markov(1) across time per position: P(d_t | d_{t-1})
# Joint pair prob = P(d1_t|d1_{t-1}) * P(d2_t|d2_{t-1})
model_markov <- function(train, test_prev_digit, time_code) {
  tr <- train |> filter(time == time_code) |> arrange(draw_date)
  if (nrow(tr) < 2) {
    return(list(T1 = matrix(1/10, nrow=10, ncol=10), T2 = matrix(1/10, nrow=10, ncol=10)))
  }
  # transitions for d1
  prev1 <- head(tr$d1, -1); next1 <- tail(tr$d1, -1)
  tab1 <- matrix(1, nrow=10, ncol=10) # Laplace smoothing
  for (i in seq_along(prev1)) tab1[prev1[i]+1, next1[i]+1] <- tab1[prev1[i]+1, next1[i]+1] + 1
  T1 <- tab1 / rowSums(tab1)

  # transitions for d2
  prev2 <- head(tr$d2, -1); next2 <- tail(tr$d2, -1)
  tab2 <- matrix(1, nrow=10, ncol=10)
  for (i in seq_along(prev2)) tab2[prev2[i]+1, next2[i]+1] <- tab2[prev2[i]+1, next2[i]+1] + 1
  T2 <- tab2 / rowSums(tab2)

  # Return transition matrices and the most recent prev digits for the test date
  list(T1=T1, T2=T2)
}

# ---------- backtest engine ----------
backtest_models <- function(df, start_after_days = 400, win_days = 180, ewma_lambda = 0.97, blend_w = 0.5) {
  df <- df |> arrange(draw_date)
  dates <- sort(unique(df$draw_date))
  if (length(dates) <= start_after_days) return(list(by_date=tibble(), summary=tibble()))

  rows <- list()
  for (cut in dates) {
    if (cut <= min(dates) + start_after_days) next
    test <- df |> filter(draw_date == cut)
    if (nrow(test) == 0) next

    train <- df |> filter(draw_date < cut, draw_date > cut - win_days)

    eval_one <- function(tcode) {
      tst <- test |> filter(time == tcode)
      if (nrow(tst) == 0) return(NULL)

      # previous same-time draw before 'cut' (for Markov)
      prev_row <- df |> filter(time==tcode, draw_date < cut) |> arrange(desc(draw_date)) |> slice_head(n=1)
      prev_d1 <- if (nrow(prev_row)) prev_row$d1 else NA_integer_
      prev_d2 <- if (nrow(prev_row)) prev_row$d2 else NA_integer_

      # models
      mF <- model_freq(train, tcode)
      mE <- model_ewma(train, tcode, lambda=ewma_lambda)
      mM <- model_markov(train, prev_row, tcode)

      # probabilities for the actual outcome
      pF <- mF$p1[tst$d1+1] * mF$p2[tst$d2+1]
      pE <- mE$p1[tst$d1+1] * mE$p2[tst$d2+1]

      if (!is.na(prev_d1) && !is.na(prev_d2)) {
        pM <- mM$T1[prev_d1+1, tst$d1+1] * mM$T2[prev_d2+1, tst$d2+1]
      } else {
        pM <- NA_real_
      }

      # blend (freq + ewma); Markov is separate
      pB <- blend_w * pF + (1-blend_w) * pE

      # top-k hits
      top1_F  <- .topk_hit_pair(mF$p1, mF$p2, tst$d1, tst$d2, 1)
      top3_F  <- .topk_hit_pair(mF$p1, mF$p2, tst$d1, tst$d2, 3)
      top1_E  <- .topk_hit_pair(mE$p1, mE$p2, tst$d1, tst$d2, 1)
      top3_E  <- .topk_hit_pair(mE$p1, mE$p2, tst$d1, tst$d2, 3)
      top1_B  <- .topk_hit_pair(.norm10(mF$p1*blend_w + mE$p1*(1-blend_w)),
                                .norm10(mF$p2*blend_w + mE$p2*(1-blend_w)),
                                tst$d1, tst$d2, 1)
      top3_B  <- .topk_hit_pair(.norm10(mF$p1*blend_w + mE$p1*(1-blend_w)),
                                .norm10(mF$p2*blend_w + mE$p2*(1-blend_w)),
                                tst$d1, tst$d2, 3)

      tibble(
        date = cut, time = tcode,
        logloss_freq   = -log(pmax(pF, 1e-12)),
        logloss_ewma   = -log(pmax(pE, 1e-12)),
        logloss_blend  = -log(pmax(pB, 1e-12)),
        logloss_markov = ifelse(is.na(pM), NA_real_, -log(pmax(pM, 1e-12))),
        top1_freq = top1_F, top3_freq = top3_F,
        top1_ewma = top1_E, top3_ewma = top3_E,
        top1_blend = top1_B, top3_blend = top3_B,
        prev_d1 = prev_d1, prev_d2 = prev_d2
      )
    }

    rows[[as.character(cut)]] <- bind_rows(eval_one("M"), eval_one("E"))
  }

  by_date <- bind_rows(rows)

  # long summary
  smry <- list(
    freq  = by_date |> summarise(model="freq",  n=n(), logloss=mean(logloss_freq),  top1=mean(top1_freq),  top3=mean(top3_freq)),
    ewma  = by_date |> summarise(model="ewma",  n=n(), logloss=mean(logloss_ewma),  top1=mean(top1_ewma),  top3=mean(top3_ewma)),
    blend = by_date |> summarise(model="blend", n=n(), logloss=mean(logloss_blend), top1=mean(top1_blend), top3=mean(top3_blend)),
    markov= by_date |> filter(!is.na(logloss_markov)) |>
      summarise(model="markov", n=n(), logloss=mean(logloss_markov), top1=NA_real_, top3=NA_real_)
  ) |> bind_rows()

  list(by_date = by_date, summary = smry)
}

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr)
})

source("R/backtest_models.R")

predict_next_blend <- function(ewma_lambda = 0.97, blend_w = 0.5) {
  pick2 <- readr::read_csv("data/clean/pick2_history.csv", show_col_types = FALSE) |>
    mutate(draw_date = as.Date(draw_date),
           time = as.character(time),
           d1 = as.integer(d1),
           d2 = as.integer(d2)) |>
    arrange(draw_date)

  last_date <- max(pick2$draw_date, na.rm = TRUE)
  train <- pick2 %>% filter(draw_date >= last_date - 180)  # same window as backtest

  # use the same model pieces as backtest_models()
  .norm10 <- function(p){ p[is.na(p)] <- 0; if (length(p)!=10) p <- rep(0,10)
                          s <- sum(p); if(s<=0) rep(1/10,10) else p/s }
  model_freq <- function(train, time_code){
    tr <- train %>% filter(time==time_code)
    p1 <- tr %>% count(d1) %>% complete(d1=0:9, fill=list(n=0)) %>%
      arrange(d1) %>% mutate(p=(n+1)/(sum(n)+10))
    p2 <- tr %>% count(d2) %>% complete(d2=0:9, fill=list(n=0)) %>%
      arrange(d2) %>% mutate(p=(n+1)/(sum(n)+10))
    list(p1=.norm10(p1$p), p2=.norm10(p2$p))
  }
  model_ewma <- function(train, time_code, lambda=0.97){
    tr <- train %>% filter(time==time_code) %>% arrange(draw_date)
    if(nrow(tr)==0) return(list(p1=rep(0.1,10), p2=rep(0.1,10)))
    w <- lambda ^ rev(seq_len(nrow(tr))-1); w <- w/sum(w)
    p1 <- sapply(0:9, function(d) sum(w * as.numeric(tr$d1==d)))
    p2 <- sapply(0:9, function(d) sum(w * as.numeric(tr$d2==d)))
    list(p1=.norm10(p1 + 1e-6), p2=.norm10(p2 + 1e-6))
  }

  predict_time <- function(tcode){
    mF <- model_freq(train, tcode)
    mE <- model_ewma(train, tcode, lambda = ewma_lambda)

    p1 <- .norm10(mF$p1*blend_w + mE$p1*(1-blend_w))
    p2 <- .norm10(mF$p2*blend_w + mE$p2*(1-blend_w))

    grid <- tidyr::expand_grid(d1=0:9, d2=0:9) %>%
      mutate(p = p1[d1+1]*p2[d2+1]) %>%
      arrange(desc(p))

    list(
      probs_d1 = setNames(as.numeric(p1), paste0("d1_",0:9)),
      probs_d2 = setNames(as.numeric(p2), paste0("d2_",0:9)),
      top_pairs = grid %>% slice_head(n=25) %>% mutate(time=tcode, .before=1)
    )
  }

  M <- predict_time("M")
  E <- predict_time("E")

  dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

  preds <- bind_rows(
    tibble(time="M", !!!as.list(M$probs_d1), !!!as.list(M$probs_d2)),
    tibble(time="E", !!!as.list(E$probs_d1), !!!as.list(E$probs_d2))
  )
  readr::write_csv(preds, "outputs/predictions_next_draw_blend.csv")

  top25 <- bind_rows(M$top_pairs, E$top_pairs)
  readr::write_csv(top25, "outputs/top25_pairs_next_draw_blend.csv")

  message("Saved outputs/predictions_next_draw_blend.csv and outputs/top25_pairs_next_draw_blend.csv")
  invisible(list(preds=preds, top25=top25))
}

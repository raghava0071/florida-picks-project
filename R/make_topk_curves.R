suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(ggplot2)})

pick2 <- read_csv("data/clean/pick2_history.csv", show_col_types = FALSE) |>
  mutate(draw_date = as.Date(draw_date),
         time = as.character(time),
         d1 = as.integer(d1),
         d2 = as.integer(d2)) |>
  arrange(draw_date)

topk_backtest <- function(df, window_days = 180, K_seq = c(1,3,5,10,15,20,25)) {
  dates <- sort(unique(df$draw_date))
  res <- list()
  for (i in (window_days+1):length(dates)) {
    cut <- dates[i]
    train <- df |> filter(draw_date > cut - window_days, draw_date < cut)
    test  <- df |> filter(draw_date == cut)
    for (tm in c("M","E")) {
      tr <- train |> filter(time==tm); tst <- test |> filter(time==tm)
      if (nrow(tr)==0 || nrow(tst)==0) next

      p1 <- tr |> count(d1) |> complete(d1=0:9, fill=list(n=0)) |>
        arrange(d1) |> mutate(p=(n+1)/(sum(n)+10))
      p2 <- tr |> count(d2) |> complete(d2=0:9, fill=list(n=0)) |>
        arrange(d2) |> mutate(p=(n+1)/(sum(n)+10))
      grid <- tidyr::expand_grid(d1=0:9, d2=0:9) |>
        mutate(p = p1$p[match(d1,p1$d1)] * p2$p[match(d2,p2$d2)]) |>
        arrange(desc(p))
      truth <- paste0(tst$d1, "-", tst$d2)

      for (K in K_seq) {
        hit <- truth %in% paste0(grid$d1[1:K], "-", grid$d2[1:K])
        res[[length(res)+1]] <- tibble(draw_date=cut, time=tm, K=K, hit=as.integer(hit))
      }
    }
  }
  bind_rows(res)
}

dfk <- topk_backtest(pick2)
curves <- dfk |> group_by(K) |> summarise(hit_rate = mean(hit), n=n(), .groups="drop")

dir.create("data/analysis", recursive=TRUE, showWarnings=FALSE)
dir.create("outputs", showWarnings=FALSE)
write_csv(curves, "data/analysis/topk_curves.csv")

g <- ggplot(curves, aes(K, hit_rate)) + geom_line() + geom_point() +
  labs(title="Hit@K vs K (independence model)", y="hit rate", x="K") +
  theme_minimal()
ggsave("outputs/topk_curves.png", g, width=7, height=5, dpi=120)

message("Saved data/analysis/topk_curves.csv and outputs/topk_curves.png")
